// gt3x_stream.cpp - reads the log.bin of an extracted ActiGraph .gt3x file
// Part of canhrActi: CANHR Accelerometer Analysis Package
//
// Written from ActiGraph's published format description
// (https://github.com/actigraph/GT3X-File-Format, MIT licence) and checked against
// read.gt3x output with identical(). No read.gt3x code was used.
// One pass indexes the records; a block then reads only its own records.

#include <Rcpp.h>
#include <cstdio>
#include <cstdint>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

#ifdef _WIN32
#define GT3X_FSEEK _fseeki64
#else
#define GT3X_FSEEK fseeko
#endif

static const unsigned char GT3X_SEP = 0x1E;
static const int GT3X_ACTIVITY = 0x00;
static const int GT3X_PARAMETERS = 0x15;

static double gt3x_u32(const unsigned char* p) {
    return (double) ((uint32_t) p[0] | ((uint32_t) p[1] << 8) |
                     ((uint32_t) p[2] << 16) | ((uint32_t) p[3] << 24));
}

// One sequential pass: record type counts, bytes outside records, where each
// data-bearing ACTIVITY record starts, which of them fail the checksum, how many other
// records fail it, the short ACTIVITY records (USB events) with their offset and
// checksum byte, the size of every PARAMETERS record, the key/value pairs of
// the first PARAMETERS record and how many ACTIVITY records come before it.
// [[Rcpp::export]]
Rcpp::List gt3x_log_index_cpp(std::string path) {
    FILE* f = std::fopen(path.c_str(), "rb");
    if (f == NULL) {
        return List::create(Named("ok") = false, Named("reason") = "cannot open log.bin");
    }
    std::setvbuf(f, NULL, _IOFBF, 1 << 20);
    std::vector<double> off, ts, bad_data, short_ts, short_size, short_after, short_off, short_cs;
    std::vector<double> par_size;
    std::vector<double> par_addr, par_id, par_val;
    IntegerVector types(256);
    std::vector<unsigned char> payload(65536 + 1);
    unsigned char hdr[8];
    double pos = 0, junk = 0, bad_other = 0, act_before_param = 0;
    int n_param = 0, size_min = -1, size_max = -1;
    std::string reason = "";

    while (true) {
        int c = std::fgetc(f);
        if (c == EOF) break;
        if (c != GT3X_SEP) {
            junk += 1;
            pos += 1;
            continue;
        }
        hdr[0] = GT3X_SEP;
        if (std::fread(hdr + 1, 1, 7, f) != 7) {
            reason = "truncated record header";
            break;
        }
        size_t sz = (size_t) hdr[6] | ((size_t) hdr[7] << 8);
        if (std::fread(payload.data(), 1, sz + 1, f) != sz + 1) {
            reason = "truncated record";
            break;
        }
        unsigned char cs = 0;
        for (int k = 0; k < 8; k++) cs ^= hdr[k];
        for (size_t k = 0; k < sz; k++) cs ^= payload[k];
        cs = (unsigned char) ~cs;
        bool bad = cs != payload[sz];
        int type = hdr[1];
        double t = gt3x_u32(hdr + 2);
        types[type] += 1;
        if (bad && !(type == GT3X_ACTIVITY && sz > 1)) bad_other += 1;
        if (type == GT3X_ACTIVITY) {
            if (n_param == 0) act_before_param += 1;
            if (sz > 1) {
                off.push_back(pos);
                ts.push_back(t);
                if (bad) bad_data.push_back((double) off.size());
                if (size_min < 0 || (int) sz < size_min) size_min = (int) sz;
                if ((int) sz > size_max) size_max = (int) sz;
            } else {
                short_ts.push_back(t);
                short_size.push_back((double) sz);
                short_after.push_back((double) off.size());
                short_off.push_back(pos);
                short_cs.push_back((double) payload[sz]);
            }
        } else if (type == GT3X_PARAMETERS) {
            par_size.push_back((double) sz);
            if (n_param == 0) {
                for (size_t k = 0; k + 8 <= sz; k += 8) {
                    const unsigned char* q = payload.data() + k;
                    par_addr.push_back((double) (q[0] | (q[1] << 8)));
                    par_id.push_back((double) (q[2] | (q[3] << 8)));
                    par_val.push_back(gt3x_u32(q + 4));
                }
            }
            n_param += 1;
        }
        pos += 9 + (double) sz;
    }
    std::fclose(f);
    List out = List::create(
        Named("ok") = reason.empty(),
        Named("reason") = reason,
        Named("bytes") = pos,
        Named("junk") = junk,
        Named("bad_data") = wrap(bad_data),
        Named("bad_other") = bad_other,
        Named("types") = types,
        Named("off") = wrap(off),
        Named("ts") = wrap(ts),
        Named("size_min") = size_min,
        Named("size_max") = size_max,
        Named("short_ts") = wrap(short_ts),
        Named("short_size") = wrap(short_size),
        Named("short_after") = wrap(short_after),
        Named("n_param") = n_param,
        Named("act_before_param") = act_before_param,
        Named("par_addr") = wrap(par_addr),
        Named("par_id") = wrap(par_id),
        Named("par_val") = wrap(par_val));
    // List::create takes at most 20 elements
    out.push_back(wrap(short_off), "short_off");
    out.push_back(wrap(short_cs), "short_cs");
    out.push_back(wrap(par_size), "par_size");
    return out;
}

// Decodes consecutive data-bearing ACTIVITY records (12-bit samples in Y, X, Z order,
// scaled to g and rounded half away from zero to three decimals). Every record is
// checked again (separator, type, size, timestamp; not the checksum, which read.gt3x
// does not check); any mismatch returns ok = FALSE and no data. The time of sample j
// of a record with timestamp s is start + ((s - start) + j * (1 / sf)) * 100 / 100.
// [[Rcpp::export]]
Rcpp::List gt3x_log_block_cpp(std::string path, NumericVector off, NumericVector ts,
                              int sf, double scale, double start) {
    List fail = List::create(Named("ok") = false);
    R_xlen_t nrec = off.size();
    if (nrec == 0 || ts.size() != nrec || sf <= 0 || scale <= 0) return fail;
    size_t psize = ((size_t) sf * 36 + 7) / 8;
    size_t rlen = 9 + psize;
    double first = off[0];
    double span = off[nrec - 1] - first + (double) rlen;
    if (span < (double) rlen || span > 2e9) return fail;
    for (R_xlen_t i = 1; i < nrec; i++) {
        if (off[i] - off[i - 1] < (double) rlen) return fail;
    }
    std::vector<unsigned char> buf((size_t) span);
    FILE* f = std::fopen(path.c_str(), "rb");
    if (f == NULL) return fail;
    bool read_ok = GT3X_FSEEK(f, (int64_t) first, SEEK_SET) == 0 &&
        std::fread(buf.data(), 1, buf.size(), f) == buf.size();
    std::fclose(f);
    if (!read_ok) return fail;

    // every 12-bit value maps to one rounded g value
    std::vector<double> lut(4096);
    for (int v = -2048; v < 2048; v++) {
        double g = v / scale;
        volatile double m = std::fabs(g) * 1000.0;
        double a = std::floor(m + 0.5);
        lut[v + 2048] = (g < 0 ? -a : a) / 1000.0;
    }
    std::vector<double> frac(sf);
    const double inv = 1.0 / sf;
    for (int j = 0; j < sf; j++) {
        volatile double fj = j * inv;
        frac[j] = fj;
    }

    R_xlen_t n = nrec * (R_xlen_t) sf;
    NumericVector time = no_init(n), X = no_init(n), Y = no_init(n), Z = no_init(n);
    R_xlen_t row = 0;
    for (R_xlen_t i = 0; i < nrec; i++) {
        const unsigned char* p = buf.data() + (size_t) (off[i] - first);
        if (p[0] != GT3X_SEP || p[1] != GT3X_ACTIVITY) return fail;
        if (((size_t) p[6] | ((size_t) p[7] << 8)) != psize) return fail;
        if (gt3x_u32(p + 2) != ts[i]) return fail;
        const unsigned char* q = p + 8;
        double secs = ts[i] - start;
        for (int j = 0; j < sf; j++) {
            int v[3];
            for (int a = 0; a < 3; a++) {
                size_t bit = ((size_t) j * 3 + a) * 12;
                size_t b = bit >> 3;
                int u;
                if ((bit & 7) == 0) {
                    u = (q[b] << 4) | (q[b + 1] >> 4);
                } else {
                    u = ((q[b] & 0x0F) << 8) | q[b + 1];
                }
                if (u > 2047) u -= 4096;
                v[a] = u;
            }
            double ti = (secs + frac[j]) * 100.0;
            time[row] = start + ti / 100.0;
            Y[row] = lut[v[0] + 2048];
            X[row] = lut[v[1] + 2048];
            Z[row] = lut[v[2] + 2048];
            row++;
        }
    }
    time.attr("class") = CharacterVector::create("POSIXct", "POSIXt");
    time.attr("tzone") = "GMT";
    return List::create(Named("ok") = true, Named("time") = time,
                        Named("X") = X, Named("Y") = Y, Named("Z") = Z);
}

// Copies len bytes at offset of src into a new file dst and returns their CRC-32, or -1
// when a read or write fails. Used for the stored (uncompressed) members of a .gt3x.
// [[Rcpp::export]]
double gt3x_copy_stored_cpp(std::string src, std::string dst, double offset, double len) {
    if (!(offset >= 0) || !(len >= 0) || offset + len > 9e15) return -1;
    uint32_t tab[8][256];
    for (uint32_t i = 0; i < 256; i++) {
        uint32_t c = i;
        for (int k = 0; k < 8; k++) c = (c & 1) ? 0xEDB88320u ^ (c >> 1) : c >> 1;
        tab[0][i] = c;
    }
    for (uint32_t i = 0; i < 256; i++) {
        for (int t = 1; t < 8; t++) tab[t][i] = tab[0][tab[t - 1][i] & 0xFF] ^ (tab[t - 1][i] >> 8);
    }
    FILE* in = std::fopen(src.c_str(), "rb");
    if (in == NULL) return -1;
    if (GT3X_FSEEK(in, (int64_t) offset, SEEK_SET) != 0) {
        std::fclose(in);
        return -1;
    }
    FILE* out = std::fopen(dst.c_str(), "wb");
    if (out == NULL) {
        std::fclose(in);
        return -1;
    }
    const size_t chunk = (size_t) 8 << 20;
    std::vector<unsigned char> buf(chunk);
    uint32_t crc = 0xFFFFFFFFu;
    double left = len;
    bool ok = true;
    while (left > 0) {
        size_t want = left > (double) chunk ? chunk : (size_t) left;
        if (std::fread(buf.data(), 1, want, in) != want) {
            ok = false;
            break;
        }
        const unsigned char* p = buf.data();
        size_t n = want;
        while (n >= 8) {
            uint32_t a = crc ^ ((uint32_t) p[0] | ((uint32_t) p[1] << 8) |
                                ((uint32_t) p[2] << 16) | ((uint32_t) p[3] << 24));
            crc = tab[7][a & 0xFF] ^ tab[6][(a >> 8) & 0xFF] ^ tab[5][(a >> 16) & 0xFF] ^
                tab[4][a >> 24] ^ tab[3][p[4]] ^ tab[2][p[5]] ^ tab[1][p[6]] ^ tab[0][p[7]];
            p += 8;
            n -= 8;
        }
        while (n > 0) {
            crc = tab[0][(crc ^ *p++) & 0xFF] ^ (crc >> 8);
            n--;
        }
        if (std::fwrite(buf.data(), 1, want, out) != want) {
            ok = false;
            break;
        }
        left -= (double) want;
    }
    std::fclose(in);
    if (std::fclose(out) != 0) ok = false;
    return ok ? (double) (crc ^ 0xFFFFFFFFu) : -1;
}
