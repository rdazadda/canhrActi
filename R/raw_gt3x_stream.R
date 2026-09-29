# canhrActi's reader for the log.bin of an extracted .gt3x (stream_gt3x), written from
# ActiGraph's format description (https://github.com/actigraph/GT3X-File-Format, MIT
# licence) without read.gt3x code. What it was not checked on goes to read.gt3x.

# Record indexes for the session, keyed by log.bin and info.txt size and mtime.
.raw.gt3x.index.env <- new.env(parent = emptyenv())

.RAW_GT3X_STREAM_TYPES <- c(0L, 2L, 3L, 5L, 6L, 13L, 19L, 21L)

#' Record Index of an Extracted gt3x, or NULL When the Reader Declines the File
#' @keywords internal
#' @noRd
.raw.gt3x.stream.index <- function(dir) {
  # identical() was verified against read.gt3x 1.2.0 only, so another version declines
  if (!isTRUE(utils::packageVersion("read.gt3x") == "1.2.0")) return(NULL)
  dir <- gsub("\\\\", "/", dir)
  lb <- file.path(dir, "log.bin")
  fi <- suppressWarnings(file.info(lb, extra_cols = FALSE))
  if (is.na(fi$size) || isTRUE(fi$isdir)) return(NULL)
  ii <- suppressWarnings(file.info(file.path(dir, "info.txt"), extra_cols = FALSE))
  key <- paste(dir, fi$size, format(as.numeric(fi$mtime), digits = 17),
               ii$size, format(as.numeric(ii$mtime), digits = 17), sep = "|")
  hit <- .raw.gt3x.index.env[[key]]
  if (!is.null(hit)) return(if (isTRUE(hit$ok)) hit else NULL)
  w <- tryCatch(.raw.gt3x.stream.build(dir, lb), error = function(e) NULL)
  .raw.gt3x.index.env[[key]] <- if (is.null(w)) list(ok = FALSE) else w
  w
}

#' Build the Record Index, NULL Unless the File Looks Like Those the Reader Was Checked On
#' @keywords internal
#' @noRd
.raw.gt3x.stream.build <- function(dir, lb) {
  if (file.exists(file.path(dir, "activity.bin")) || !file.exists(file.path(dir, "info.txt"))) return(NULL)
  hdr <- read.gt3x::parse_gt3x_info(dir)
  need <- c("Serial Prefix", "Firmware", "Sample Rate", "Start Date", "Stop Date",
            "Last Sample Time", "TimeZone", "Subject Name", "Acceleration Scale",
            "Acceleration Min", "Acceleration Max")
  if (!all(need %in% names(hdr)) || any(lengths(hdr[need]) != 1)) return(NULL)
  if (!identical(hdr[["Serial Prefix"]], "MOS") || identical(hdr[["Firmware"]], "1.6.0")) return(NULL)
  sf <- hdr[["Sample Rate"]]
  if (!is.numeric(sf) || !(sf %in% seq(30, 100, by = 10))) return(NULL)
  sf <- as.integer(sf)
  if (!identical(as.numeric(hdr[["Acceleration Scale"]]), 256)) return(NULL)
  start <- as.numeric(hdr[["Start Date"]])
  last <- as.numeric(hdr[["Last Sample Time"]])
  if (!is.finite(start) || !is.finite(last) || start %% 1 != 0 || last %% 1 != 0) return(NULL)
  max_samples <- (last - start) * sf
  if (max_samples <= 0 || max_samples >= .Machine$integer.max) return(NULL)

  x <- gt3x_log_index_cpp(lb)
  if (!isTRUE(x$ok) || x$junk != 0 || x$bad_other != 0 || x$bytes != file.size(lb)) return(NULL)
  seen <- which(x$types > 0) - 1L
  if (!all(seen %in% .RAW_GT3X_STREAM_TYPES)) return(NULL)
  n <- length(x$ts)
  if (n == 0 || x$size_min != x$size_max || x$size_min != (36L * sf + 7L) %/% 8L) return(NULL)
  # one PARAMETERS record before any ACTIVITY record (read.gt3x resets its start there),
  # with scale 256 and info.txt's sample rate and start
  if (x$n_param != 1 || x$act_before_param != 0) return(NULL)
  # read.gt3x reads floor(size / 8) pairs and would misframe the bytes left over
  if (any(x$par_size %% 8 != 0)) return(NULL)
  par <- function(a, i) x$par_val[x$par_addr == a & x$par_id == i]
  if (!identical(.raw.gt3x.ssp(par(0, 55)), 256) || !identical(par(1, 10), as.numeric(sf)) ||
      !identical(par(1, 12), start)) return(NULL)
  # FEATURE_ENABLE: sleep mode alone or none of the documented bits. read.gt3x takes
  # the value as an integer, so from 2^31 it reads NA and reports no features
  fe <- par(1, 2)
  if (length(fe) != 1 || fe >= 2^31 || !(fe %% 64 %in% c(0, 4))) return(NULL)
  # USB events are 1-byte ACTIVITY records, at most one between two data records
  sa <- x$short_after
  if (any(x$short_size != 1) || anyDuplicated(sa) || any(sa == 0)) return(NULL)
  # read.gt3x takes the payload byte of a USB event for its checksum, so a checksum
  # byte of 0x1E starts a false record and misframes whatever follows it
  if (any(x$short_cs == 0x1E & x$short_off + 10 < x$bytes)) return(NULL)
  o <- order(c(seq_len(n), sa + 0.5))
  rt <- c(x$ts, x$short_ts)[o]
  usb <- rep(c(FALSE, TRUE), c(n, length(sa)))[o]
  pos <- c(seq_len(n), sa + 1)[o]
  prev <- c(start - 1, rt[-length(rt)])
  nxt <- c(rt[-1], last)
  if (any(rt - prev < 1) || any(usb & (rt - prev < 2 | nxt - rt < 2)) ||
      rt[length(rt)] + 1 >= last) return(NULL)
  # missingness rows in file order: the gap before a record, one second per USB event
  g <- rt - prev > 1
  eo <- order(c(which(g) * 2 - 1, which(usb) * 2))
  list(ok = TRUE, logbin = lb, header = hdr, sf = sf, scale = 256, start = start,
       n = n, off = x$off, ts = x$ts, bad = x$bad_data,
       ent_t = c(prev[g] + 1, rt[usb])[eo],
       ent_n = c(as.integer((rt[g] - prev[g] - 1) * sf), rep(sf, sum(usb)))[eo],
       ent_pos = c(pos[g], pos[usb])[eo],
       short_ts = x$short_ts, short_pos = sa + 1, max_samples = as.integer(max_samples),
       features = if (fe %% 64 == 4) "sleep mode" else "none")
}

#' Decode a PARAMETERS Float (Three-Byte Fraction, One-Byte Exponent)
#' @keywords internal
#' @noRd
.raw.gt3x.ssp <- function(v) {
  if (length(v) != 1) return(NA_real_)
  e <- v %/% 16777216
  if (e >= 128) e <- e - 256
  s <- v %% 16777216
  if (s >= 8388608) s <- s - 16777216
  (s / 8388608) * 2^e
}

#' One Batch as read.gt3x Returns It, "eof" Past the Last Record, or NULL When Declined
#' @keywords internal
#' @noRd
.raw.gt3x.stream.batch <- function(dir, startpage, endpage) {
  w <- .raw.gt3x.stream.index(dir)
  if (is.null(w)) return(NULL)
  ok_page <- function(p) is.numeric(p) && length(p) == 1 && is.finite(p) && p %% 1 == 0
  if (!ok_page(startpage) || !ok_page(endpage) || startpage < 1 || endpage < startpage) return(NULL)
  if (startpage > w$n) return("eof")
  e <- min(endpage, w$n)
  # a data record that fails its checksum is served, since read.gt3x never checks it
  i <- startpage:e
  b <- gt3x_log_block_cpp(w$logbin, w$off[i], w$ts[i], w$sf, w$scale, w$start)
  if (!isTRUE(b$ok)) return(NULL)
  nrow <- as.integer(length(i) * w$sf)
  if (length(b$time) != nrow) return(NULL)

  # read.gt3x reads on to the next data record, so a USB event just past record e counts
  inc <- w$ent_pos <= endpage
  mt <- w$ent_t[inc]
  mn <- w$ent_n[inc]
  last <- max(w$ts[e], w$short_ts[w$short_pos <= endpage])
  rest <- w$max_samples - nrow - sum(as.numeric(mn))
  if (rest <= 0) return(NULL)
  mt <- c(mt, last + 1)
  mn <- c(mn, as.integer(rest))
  miss <- data.frame(time = .POSIXct(mt, tz = "GMT"), n_missing = mn,
                     row.names = sprintf("%.0f", mt))

  h <- w$header
  x <- b[c("time", "X", "Y", "Z")]
  attributes(x) <- list(
    names = c("time", "X", "Y", "Z"), row.names = c(NA_integer_, nrow),
    class = c("activity_df", "data.frame"),
    subject_name = h[["Subject Name"]], time_zone = h[["TimeZone"]], missingness = miss,
    old_version = FALSE, firmware = h[["Firmware"]], last_sample_time = h[["Last Sample Time"]],
    serial_prefix = h[["Serial Prefix"]], sample_rate = w$sf,
    acceleration_min = h[["Acceleration Min"]], acceleration_max = h[["Acceleration Max"]],
    header = h, start_time = h[["Start Date"]], stop_time = h[["Stop Date"]],
    total_records = nrow, bad_samples = FALSE, features = w$features)
  x
}

#' read.gt3x's End-of-File Error, Which .raw.read.block Takes as the End of the Loop
#' @keywords internal
#' @noRd
.raw.gt3x.eof <- function() {
  stop(structure(class = c("std::range_error", "C++Error", "error", "condition"),
                 list(message = "upper value must be greater than lower value", call = NULL)))
}
