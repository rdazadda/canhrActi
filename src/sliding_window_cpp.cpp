// sliding_window_cpp.cpp - High-performance sliding window calculations
// Implements L5/M10, IS, IV, and other circadian rhythm metrics
// Part of canhrActi: CANHR Accelerometer Analysis Package
//
// References:
// - van Someren EJW, et al. (1999). Bright light therapy: Improved sensitivity to
//   its effects on rest-activity rhythms in Alzheimer patients by application of
//   nonparametric methods. Chronobiology International, 16(4), 505-518.
// - Blume C, et al. (2016). nparACT: Non-parametric measures of actigraphy data.
//   R package.

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>

using namespace Rcpp;

// L5/M10 Calculations (Least Active 5h / Most Active 10h)

// Calculate L5 and M10 from minute-level data using van Someren (1999) method
// CORRECT METHOD: First create average 24-hour profile, then apply circular sliding window
// window_L5: number of minutes for L5 (typically 300 = 5 hours)
// window_M10: number of minutes for M10 (typically 600 = 10 hours)
// start_minute: minute of day when data starts (0-1439). If data starts at 14:30, this is 870.
//               This ensures correct alignment of the 24-hour profile.
//
// References:
// van Someren EJ, et al. (1999). Bright light therapy: improved sensitivity to
// its effects on rest-activity rhythms in Alzheimer patients by application of
// nonparametric methods. Chronobiol Int, 16(4):505-518.
// [[Rcpp::export]]
Rcpp::List calculate_L5_M10_cpp(NumericVector minute_data,
                                 int window_L5 = 300,
                                 int window_M10 = 600,
                                 int start_minute = 0) {
    int n = minute_data.size();
    const int MINUTES_PER_DAY = 1440;

    bool any_valid = false;
    for (int i = 0; i < n && !any_valid; ++i) {
        any_valid = !ISNA(minute_data[i]) && R_finite(minute_data[i]);
    }

    if (n < MINUTES_PER_DAY || !any_valid) {
        // Not enough data for circadian analysis: under a day, or no valid minute
        return Rcpp::List::create(
            Named("L5_value") = NA_REAL,
            Named("L5_onset") = NA_INTEGER,
            Named("L5_onset_hours") = NA_REAL,
            Named("M10_value") = NA_REAL,
            Named("M10_onset") = NA_INTEGER,
            Named("M10_onset_hours") = NA_REAL,
            Named("RA") = NA_REAL
        );
    }

    // Validate start_minute
    start_minute = ((start_minute % MINUTES_PER_DAY) + MINUTES_PER_DAY) % MINUTES_PER_DAY;

    // STEP 1: Create average 24-hour profile
    // This is the KEY step that matches the standard van Someren method
    // Average activity at each minute-of-day across all days
    // Use start_minute offset to correctly map index to minute-of-day

    std::vector<double> avg_profile(MINUTES_PER_DAY, 0.0);
    std::vector<int> count_profile(MINUTES_PER_DAY, 0);

    for (int i = 0; i < n; ++i) {
        // Calculate actual minute of day, accounting for start offset
        int minute_of_day = (start_minute + i) % MINUTES_PER_DAY;
        if (!ISNA(minute_data[i]) && R_finite(minute_data[i])) {
            avg_profile[minute_of_day] += minute_data[i];
            count_profile[minute_of_day]++;
        }
    }

    // Compute averages and identify missing bins
    std::vector<bool> valid_bin(MINUTES_PER_DAY, false);
    for (int m = 0; m < MINUTES_PER_DAY; ++m) {
        if (count_profile[m] > 0) {
            avg_profile[m] /= count_profile[m];
            valid_bin[m] = true;
        }
    }

    // Interpolate missing bins from neighbors (circular)
    // This prevents bias from zero-filling during non-wear periods
    for (int m = 0; m < MINUTES_PER_DAY; ++m) {
        if (!valid_bin[m]) {
            // Find nearest valid neighbors (circular search)
            int prev = -1, next = -1;
            for (int d = 1; d < MINUTES_PER_DAY / 2; ++d) {
                int p = (m - d + MINUTES_PER_DAY) % MINUTES_PER_DAY;
                int n = (m + d) % MINUTES_PER_DAY;
                if (prev < 0 && valid_bin[p]) prev = p;
                if (next < 0 && valid_bin[n]) next = n;
                if (prev >= 0 && next >= 0) break;
            }
            if (prev >= 0 && next >= 0) {
                // Linear interpolation
                avg_profile[m] = (avg_profile[prev] + avg_profile[next]) / 2.0;
            } else if (prev >= 0) {
                avg_profile[m] = avg_profile[prev];
            } else if (next >= 0) {
                avg_profile[m] = avg_profile[next];
            }
            // If no valid neighbors at all, remains 0.0 (very rare edge case)
        }
    }

    // STEP 2: Circular sliding window on average profile
    // Window wraps around midnight (24:00 -> 00:00)

    // Helper lambda for circular sliding window mean
    auto circular_sliding_mean = [&](int window_size, bool find_min) -> std::pair<double, int> {
        if (window_size > MINUTES_PER_DAY) {
            window_size = MINUTES_PER_DAY;
        }

        // Calculate rolling means with circular wraparound
        std::vector<double> rolling_means(MINUTES_PER_DAY);

        for (int i = 0; i < MINUTES_PER_DAY; ++i) {
            double sum = 0.0;
            for (int j = 0; j < window_size; ++j) {
                int idx = (i + j) % MINUTES_PER_DAY;  // Circular wraparound
                sum += avg_profile[idx];
            }
            rolling_means[i] = sum / window_size;
        }

        // Find optimal window (min or max)
        int best_idx = 0;
        double best_value = rolling_means[0];

        for (int i = 1; i < MINUTES_PER_DAY; ++i) {
            if (find_min) {
                if (rolling_means[i] < best_value) {
                    best_value = rolling_means[i];
                    best_idx = i;
                }
            } else {
                if (rolling_means[i] > best_value) {
                    best_value = rolling_means[i];
                    best_idx = i;
                }
            }
        }

        return std::make_pair(best_value, best_idx);
    };

    // Calculate L5 (least active 5 hours = 300 minutes)
    auto L5_result = circular_sliding_mean(window_L5, true);
    double L5_value = L5_result.first;
    int L5_onset = L5_result.second;  // Minute of day (0-1439)

    // Calculate M10 (most active 10 hours = 600 minutes)
    auto M10_result = circular_sliding_mean(window_M10, false);
    double M10_value = M10_result.first;
    int M10_onset = M10_result.second;  // Minute of day (0-1439)

    // Calculate Relative Amplitude
    double RA = (M10_value + L5_value > 0) ?
                (M10_value - L5_value) / (M10_value + L5_value) : NA_REAL;
    if (!R_finite(RA)) RA = NA_REAL;

    // Convert onset to hours (time of day)
    double L5_onset_hours = L5_onset / 60.0;
    double M10_onset_hours = M10_onset / 60.0;

    return Rcpp::List::create(
        Named("L5_value") = L5_value,
        Named("L5_onset") = L5_onset,
        Named("L5_onset_hours") = L5_onset_hours,
        Named("M10_value") = M10_value,
        Named("M10_onset") = M10_onset,
        Named("M10_onset_hours") = M10_onset_hours,
        Named("RA") = RA
    );
}


// Interdaily Stability (IS)

// TRUE when the first n values are all equal: no variance, so IS and IV are undefined
static bool is_constant(const NumericVector& x, int n) {
    for (int i = 1; i < n; ++i) {
        if (!(x[i] == x[0])) return false;
    }
    return true;
}

// IS measures day-to-day consistency (0-1, higher = more stable)
// [[Rcpp::export]]
double calculate_IS_cpp(NumericVector hourly_data, int hours_per_day = 24) {
    if (hours_per_day <= 0) Rcpp::stop("hours_per_day must be positive");
    int n = hourly_data.size();
    int n_days = n / hours_per_day;

    if (n_days < 2 || n < hours_per_day) {
        return NA_REAL;
    }

    // Truncate to complete days
    int total_hours = n_days * hours_per_day;

    // Calculate overall mean
    double grand_mean = 0.0;
    for (int i = 0; i < total_hours; ++i) {
        grand_mean += hourly_data[i];
    }
    grand_mean /= total_hours;

    // Calculate mean for each hour of day (across all days)
    std::vector<double> hourly_means(hours_per_day, 0.0);
    for (int h = 0; h < hours_per_day; ++h) {
        for (int d = 0; d < n_days; ++d) {
            hourly_means[h] += hourly_data[d * hours_per_day + h];
        }
        hourly_means[h] /= n_days;
    }

    // Calculate numerator: variance of hourly means
    double var_hourly = 0.0;
    for (int h = 0; h < hours_per_day; ++h) {
        double diff = hourly_means[h] - grand_mean;
        var_hourly += diff * diff;
    }
    var_hourly *= n_days;  // n * variance

    // Calculate denominator: total variance
    double var_total = 0.0;
    for (int i = 0; i < total_hours; ++i) {
        double diff = hourly_data[i] - grand_mean;
        var_total += diff * diff;
    }

    // IS = (n * Var(hourly_means)) / Var(total)
    double IS = var_hourly / var_total;

    // NA when undefined: a constant series, or an NA hour
    if (is_constant(hourly_data, total_hours) || !R_finite(IS)) {
        return NA_REAL;
    }

    return std::max(0.0, std::min(1.0, IS));
}


// Intradaily Variability (IV)

// IV measures fragmentation within days (typically 0-2+, lower = less fragmented)
// NOTE: This function assumes ALL hourly data points are consecutive (no gaps).
// For data with missing hours, use the gap-aware R implementation in .calculate.IS.IV()
// which filters to only use differences between actually consecutive hours.
// [[Rcpp::export]]
double calculate_IV_cpp(NumericVector hourly_data) {
    int n = hourly_data.size();

    if (n < 2) {
        return NA_REAL;
    }

    // Calculate mean
    double mean = 0.0;
    for (int i = 0; i < n; ++i) {
        mean += hourly_data[i];
    }
    mean /= n;

    // Calculate numerator: sum of squared differences between consecutive hours
    double sum_sq_diff = 0.0;
    for (int i = 1; i < n; ++i) {
        double diff = hourly_data[i] - hourly_data[i - 1];
        sum_sq_diff += diff * diff;
    }

    // Calculate denominator: total variance
    double var_total = 0.0;
    for (int i = 0; i < n; ++i) {
        double diff = hourly_data[i] - mean;
        var_total += diff * diff;
    }

    // IV = (n * sum_sq_diff) / ((n-1) * var_total)
    double IV = (n * sum_sq_diff) / ((n - 1) * var_total);

    // NA when undefined: a constant series, or an NA hour
    return R_finite(IV) && !is_constant(hourly_data, n) ? IV : NA_REAL;
}


// Phi (Autocorrelation at lag 1)

double calculate_phi_cpp(NumericVector x, int lag = 1) {
    int n = x.size();

    if (n <= lag) {
        return NA_REAL;
    }

    // Calculate mean
    double mean = 0.0;
    for (int i = 0; i < n; ++i) {
        mean += x[i];
    }
    mean /= n;

    // Calculate variance
    double var = 0.0;
    for (int i = 0; i < n; ++i) {
        double diff = x[i] - mean;
        var += diff * diff;
    }

    // Calculate covariance at lag
    double cov = 0.0;
    for (int i = lag; i < n; ++i) {
        cov += (x[i] - mean) * (x[i - lag] - mean);
    }

    // Autocorrelation
    double phi = cov / var;

    return R_finite(phi) ? phi : NA_REAL;
}


// Rolling Statistics (General Purpose)

static double window_sum(const NumericVector& x, int start, int window) {
    double s = 0.0;
    for (int j = start; j < start + window; ++j) {
        s += x[j];
    }
    return s;
}

// The running sum keeps only finite values; a window holding NA, NaN or Inf is
// summed directly, so a missing value reaches only the windows that contain it
static NumericVector rolling_sum_impl(const NumericVector& x, int window) {
    if (window < 1) {
        Rcpp::stop("window must be at least 1");
    }
    int n = x.size();
    int n_out = n - window + 1;

    if (n_out <= 0) {
        return NumericVector(0);
    }

    NumericVector result(n_out);
    double sum = 0.0;
    int n_bad = 0;

    // Initialize first window
    for (int i = 0; i < window; ++i) {
        if (R_finite(x[i])) sum += x[i]; else ++n_bad;
    }
    result[0] = n_bad ? window_sum(x, 0, window) : sum;

    // Slide window
    for (int i = 1; i < n_out; ++i) {
        double out = x[i - 1], in = x[i + window - 1];
        if (R_finite(out)) sum -= out; else --n_bad;
        if (R_finite(in)) sum += in; else ++n_bad;
        result[i] = n_bad ? window_sum(x, i, window) : sum;
    }

    return result;
}

// Rolling mean with O(n) complexity
// [[Rcpp::export]]
NumericVector rolling_mean_cpp(NumericVector x, int window) {
    NumericVector result = rolling_sum_impl(x, window);
    for (int i = 0; i < result.size(); ++i) {
        result[i] /= window;
    }
    return result;
}


// Rolling standard deviation with O(n) complexity using Welford's algorithm
// [[Rcpp::export]]
NumericVector rolling_sd_cpp(NumericVector x, int window) {
    if (window < 1) {
        Rcpp::stop("window must be at least 1");
    }
    int n = x.size();
    int n_out = n - window + 1;

    if (n_out <= 0) {
        return NumericVector(0);
    }

    // one value has no spread, as with stats::sd
    if (window == 1) {
        return NumericVector(n_out, NA_REAL);
    }

    NumericVector result(n_out);

    // For each window position
    for (int i = 0; i < n_out; ++i) {
        double mean = 0.0;
        double M2 = 0.0;

        for (int j = 0; j < window; ++j) {
            double val = x[i + j];
            double delta = val - mean;
            mean += delta / (j + 1);
            double delta2 = val - mean;
            M2 += delta * delta2;
        }

        result[i] = std::sqrt(M2 / (window - 1));
    }

    return result;
}


// Rolling sum
// [[Rcpp::export]]
NumericVector rolling_sum_cpp(NumericVector x, int window) {
    return rolling_sum_impl(x, window);
}


// Comprehensive Circadian Analysis

// All-in-one circadian metrics calculation
// start_minute: minute of day when data starts (0-1439) for correct 24h profile alignment
// [[Rcpp::export]]
Rcpp::List calculate_all_circadian_cpp(NumericVector minute_data,
                                        int hours_per_day = 24,
                                        int start_minute = 0) {
    if (hours_per_day <= 0) Rcpp::stop("hours_per_day must be positive");
    int n = minute_data.size();

    // Convert to hourly for IS/IV
    int n_hours = n / 60;
    NumericVector hourly_data(n_hours);
    for (int h = 0; h < n_hours; ++h) {
        double sum = 0.0;
        for (int m = 0; m < 60; ++m) {
            int idx = h * 60 + m;
            if (idx < n) sum += minute_data[idx];
        }
        hourly_data[h] = sum;
    }

    // Calculate L5/M10 with correct time alignment
    Rcpp::List L5M10 = calculate_L5_M10_cpp(minute_data, 300, 600, start_minute);

    // Calculate L1/M1 with correct time alignment
    Rcpp::List L1M1 = calculate_L5_M10_cpp(minute_data, 60, 60, start_minute);

    // Calculate IS
    double IS = calculate_IS_cpp(hourly_data, hours_per_day);

    // Calculate IV
    double IV = calculate_IV_cpp(hourly_data);

    // Calculate Phi (autocorrelation at 1-hour lag)
    double phi = calculate_phi_cpp(hourly_data, 1);

    return Rcpp::List::create(
        Named("L5_value") = L5M10["L5_value"],
        Named("L5_onset_hours") = L5M10["L5_onset_hours"],
        Named("M10_value") = L5M10["M10_value"],
        Named("M10_onset_hours") = L5M10["M10_onset_hours"],
        Named("RA") = L5M10["RA"],
        Named("L1_value") = L1M1["L5_value"],
        Named("L1_onset_hours") = L1M1["L5_onset_hours"],
        Named("M1_value") = L1M1["M10_value"],
        Named("M1_onset_hours") = L1M1["M10_onset_hours"],
        Named("IS") = IS,
        Named("IV") = IV,
        Named("phi") = phi,
        Named("n_minutes") = n,
        Named("n_hours") = n_hours,
        Named("n_days") = n_hours / hours_per_day
    );
}
