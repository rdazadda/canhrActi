// intensity_metrics_cpp.cpp - High-performance intensity gradient and MX metrics
// Implements Rowlands et al. (2018, 2019) algorithms
// Part of canhrActi: CANHR Accelerometer Analysis Package
//
// References:
// - Rowlands AV, et al. (2018). Beyond Cut Points: Accelerometer Metrics that
//   Capture the Physical Activity Profile. MSSE, 50(6):1323-1332.
// - Rowlands AV, et al. (2019). Activity Intensity, Volume, and Norms.
//   MSSE, 51(11):2410-2422.

#include <RcppArmadillo.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

//' Calculate Intensity Gradient (C++ Implementation)
//'
//' High-performance calculation of the intensity gradient using log-log
//' linear regression on acceleration bin data.
//'
//' @param data NumericVector of acceleration values in mg
//' @param epoch_length Epoch length in seconds (default: 5)
//' @param bin_size Bin width in mg (default: 25)
//' @param max_value Maximum value for binning (default: 4000)
//'
//' @return List with gradient, intercept, r_squared, average_acceleration
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List intensity_gradient_cpp(NumericVector data,
                                   double epoch_length = 5.0,
                                   double bin_size = 25.0,
                                   double max_value = 4000.0) {

    int n = data.size();

    if (n == 0) {
        return Rcpp::List::create(
            Named("gradient") = NA_REAL,
            Named("intercept") = NA_REAL,
            Named("r_squared") = NA_REAL,
            Named("average_acceleration") = NA_REAL,
            Named("total_time_minutes") = 0.0,
            Named("n_valid") = 0
        );
    }

    // Count valid (non-NA) values and calculate average
    int n_valid = 0;
    double sum = 0.0;
    for (int i = 0; i < n; ++i) {
        if (!ISNA(data[i]) && R_finite(data[i])) {
            sum += data[i];
            n_valid++;
        }
    }

    if (n_valid == 0) {
        return Rcpp::List::create(
            Named("gradient") = NA_REAL,
            Named("intercept") = NA_REAL,
            Named("r_squared") = NA_REAL,
            Named("average_acceleration") = NA_REAL,
            Named("total_time_minutes") = 0.0,
            Named("n_valid") = 0
        );
    }

    double average_acceleration = sum / n_valid;
    double total_time_minutes = n_valid * epoch_length / 60.0;

    // Create bins: 0-25, 25-50, ..., max_value-inf
    int n_bins = static_cast<int>(max_value / bin_size) + 1;
    std::vector<int> bin_counts(n_bins, 0);
    std::vector<double> bin_midpoints(n_bins);

    // Set midpoints
    for (int i = 0; i < n_bins - 1; ++i) {
        bin_midpoints[i] = (i + 0.5) * bin_size;  // Center of each bin
    }
    bin_midpoints[n_bins - 1] = max_value + bin_size;  // Top bin

    // Assign data to bins
    for (int i = 0; i < n; ++i) {
        if (ISNA(data[i]) || !R_finite(data[i])) continue;

        double val = data[i];
        if (val < 0) val = 0;

        int bin_idx;
        if (val >= max_value) {
            bin_idx = n_bins - 1;
        } else {
            bin_idx = static_cast<int>(val / bin_size);
            if (bin_idx >= n_bins - 1) bin_idx = n_bins - 2;
        }
        bin_counts[bin_idx]++;
    }

    // Convert to minutes
    double minutes_per_epoch = epoch_length / 60.0;
    std::vector<double> bin_minutes(n_bins);
    for (int i = 0; i < n_bins; ++i) {
        bin_minutes[i] = bin_counts[i] * minutes_per_epoch;
    }

    // Filter bins with non-zero time for regression
    std::vector<double> log_midpoints;
    std::vector<double> log_minutes;

    for (int i = 0; i < n_bins; ++i) {
        if (bin_minutes[i] > 0 && bin_midpoints[i] > 0) {
            log_midpoints.push_back(std::log10(bin_midpoints[i]));
            log_minutes.push_back(std::log10(bin_minutes[i]));
        }
    }

    int n_valid_bins = log_midpoints.size();
    if (n_valid_bins < 3) {
        return Rcpp::List::create(
            Named("gradient") = NA_REAL,
            Named("intercept") = NA_REAL,
            Named("r_squared") = NA_REAL,
            Named("average_acceleration") = average_acceleration,
            Named("total_time_minutes") = total_time_minutes,
            Named("n_valid") = n_valid,
            Named("n_bins_used") = n_valid_bins
        );
    }

    // Simple linear regression: log_minutes = intercept + gradient * log_midpoints
    double sum_x = 0.0, sum_y = 0.0, sum_xy = 0.0, sum_xx = 0.0;
    for (int i = 0; i < n_valid_bins; ++i) {
        sum_x += log_midpoints[i];
        sum_y += log_minutes[i];
        sum_xy += log_midpoints[i] * log_minutes[i];
        sum_xx += log_midpoints[i] * log_midpoints[i];
    }

    double mean_x = sum_x / n_valid_bins;
    double mean_y = sum_y / n_valid_bins;

    double ss_xx = sum_xx - n_valid_bins * mean_x * mean_x;
    double ss_xy = sum_xy - n_valid_bins * mean_x * mean_y;

    double gradient = ss_xy / ss_xx;
    double intercept = mean_y - gradient * mean_x;

    // Calculate R-squared
    double ss_tot = 0.0, ss_res = 0.0;
    for (int i = 0; i < n_valid_bins; ++i) {
        double y_pred = intercept + gradient * log_midpoints[i];
        double diff_tot = log_minutes[i] - mean_y;
        double diff_res = log_minutes[i] - y_pred;
        ss_tot += diff_tot * diff_tot;
        ss_res += diff_res * diff_res;
    }

    double r_squared = 1.0 - (ss_res / ss_tot);

    return Rcpp::List::create(
        Named("gradient") = gradient,
        Named("intercept") = intercept,
        Named("r_squared") = r_squared,
        Named("average_acceleration") = average_acceleration,
        Named("total_time_minutes") = total_time_minutes,
        Named("n_valid") = n_valid,
        Named("n_bins_used") = n_valid_bins
    );
}

//' Calculate MX Metrics (C++ Implementation)
//'
//' High-performance calculation of MX metrics (acceleration exceeded during
//' most active X minutes).
//'
//' @param data NumericVector of acceleration values in mg
//' @param X_values IntegerVector of time values in minutes (e.g., c(2, 5, 10, 30, 60))
//' @param epoch_length Epoch length in seconds (default: 5)
//'
//' @return Named NumericVector of MX values
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List calculate_MX_cpp(NumericVector data,
                             IntegerVector X_values,
                             double epoch_length = 5.0) {

    int n = data.size();
    int n_X = X_values.size();

    // Initialize result with NA
    NumericVector MX(n_X, NA_REAL);
    CharacterVector names(n_X);
    for (int i = 0; i < n_X; ++i) {
        names[i] = "M" + std::to_string(X_values[i]);
    }
    MX.names() = names;

    if (n == 0) {
        return Rcpp::List::create(
            Named("MX") = MX,
            Named("average_acceleration") = NA_REAL,
            Named("total_time_minutes") = 0.0
        );
    }

    // Collect valid values
    std::vector<double> valid_data;
    valid_data.reserve(n);
    double sum = 0.0;

    for (int i = 0; i < n; ++i) {
        if (!ISNA(data[i]) && R_finite(data[i])) {
            valid_data.push_back(data[i]);
            sum += data[i];
        }
    }

    int n_valid = valid_data.size();
    if (n_valid == 0) {
        return Rcpp::List::create(
            Named("MX") = MX,
            Named("average_acceleration") = NA_REAL,
            Named("total_time_minutes") = 0.0
        );
    }

    double average_acceleration = sum / n_valid;
    double total_time_minutes = n_valid * epoch_length / 60.0;

    // Sort in descending order
    std::sort(valid_data.begin(), valid_data.end(), std::greater<double>());

    // Calculate MX for each X value
    for (int i = 0; i < n_X; ++i) {
        int X = X_values[i];
        double X_minutes = static_cast<double>(X);

        if (X_minutes > total_time_minutes) {
            MX[i] = NA_REAL;  // Not enough data
            continue;
        }

        // Number of epochs in X minutes
        int n_epochs = static_cast<int>(std::ceil(X_minutes * 60.0 / epoch_length));
        if (n_epochs > n_valid) {
            n_epochs = n_valid;
        }
        if (n_epochs <= 0) {
            MX[i] = NA_REAL;
            continue;
        }

        // MX is the value at the n_epochs position (X-th highest value)
        // Index n_epochs - 1 because 0-indexed
        MX[i] = valid_data[n_epochs - 1];
    }

    return Rcpp::List::create(
        Named("MX") = MX,
        Named("average_acceleration") = average_acceleration,
        Named("total_time_minutes") = total_time_minutes,
        Named("n_valid") = n_valid
    );
}


//' Calculate Complete Activity Profile (C++ Implementation)
//'
//' Combines intensity gradient and MX metrics in a single optimized function.
//'
//' @param data NumericVector of acceleration values in mg
//' @param epoch_length Epoch length in seconds
//' @param bin_size Bin size for intensity gradient
//' @param X_values X values for MX metrics
//'
//' @return List with all activity profile metrics
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List activity_profile_cpp(NumericVector data,
                                 double epoch_length = 5.0,
                                 double bin_size = 25.0,
                                 IntegerVector X_values = IntegerVector::create(2, 5, 10, 15, 30, 60, 120)) {

    // Calculate intensity gradient
    Rcpp::List ig = intensity_gradient_cpp(data, epoch_length, bin_size, 4000.0);

    // Calculate MX metrics
    Rcpp::List mx = calculate_MX_cpp(data, X_values, epoch_length);

    return Rcpp::List::create(
        Named("average_acceleration") = ig["average_acceleration"],
        Named("gradient") = ig["gradient"],
        Named("intercept") = ig["intercept"],
        Named("ig_r_squared") = ig["r_squared"],
        Named("MX") = mx["MX"],
        Named("total_time_minutes") = ig["total_time_minutes"],
        Named("n_valid") = ig["n_valid"],
        Named("n_bins_used") = ig["n_bins_used"]
    );
}

//' Create Acceleration Histogram for Intensity Gradient
//'
//' Creates binned distribution of acceleration values for intensity gradient
//' analysis or visualization.
//'
//' @param data NumericVector of acceleration values in mg
//' @param bin_size Bin width in mg (default: 25)
//' @param max_value Maximum value for regular bins (default: 4000)
//'
//' @return DataFrame with bin_lower, bin_upper, midpoint, count
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::DataFrame acceleration_histogram_cpp(NumericVector data,
                                            double bin_size = 25.0,
                                            double max_value = 4000.0) {

    int n = data.size();
    int n_bins = static_cast<int>(max_value / bin_size) + 1;

    std::vector<double> bin_lower(n_bins);
    std::vector<double> bin_upper(n_bins);
    std::vector<double> midpoint(n_bins);
    std::vector<int> count(n_bins, 0);

    // Set bin boundaries
    for (int i = 0; i < n_bins - 1; ++i) {
        bin_lower[i] = i * bin_size;
        bin_upper[i] = (i + 1) * bin_size;
        midpoint[i] = bin_lower[i] + bin_size / 2.0;
    }
    // Last bin: max_value to infinity
    bin_lower[n_bins - 1] = max_value;
    bin_upper[n_bins - 1] = R_PosInf;
    midpoint[n_bins - 1] = max_value + bin_size;

    // Count values in each bin
    for (int i = 0; i < n; ++i) {
        if (ISNA(data[i]) || !R_finite(data[i])) continue;

        double val = data[i];
        if (val < 0) val = 0;

        int bin_idx;
        if (val >= max_value) {
            bin_idx = n_bins - 1;
        } else {
            bin_idx = static_cast<int>(val / bin_size);
            if (bin_idx >= n_bins - 1) bin_idx = n_bins - 2;
        }
        count[bin_idx]++;
    }

    return Rcpp::DataFrame::create(
        Named("bin_lower") = bin_lower,
        Named("bin_upper") = bin_upper,
        Named("midpoint") = midpoint,
        Named("count") = count
    );
}

//' Calculate Acceleration Percentiles
//'
//' Fast percentile calculation for acceleration distribution.
//'
//' @param data NumericVector of acceleration values
//' @param probs NumericVector of probabilities (0-1)
//'
//' @return NumericVector of percentile values
//'
//' @keywords internal
// [[Rcpp::export]]
NumericVector acceleration_percentiles_cpp(NumericVector data,
                                            NumericVector probs) {

    int n = data.size();
    int n_probs = probs.size();

    NumericVector result(n_probs, NA_REAL);

    // Collect valid values
    std::vector<double> valid_data;
    valid_data.reserve(n);

    for (int i = 0; i < n; ++i) {
        if (!ISNA(data[i]) && R_finite(data[i])) {
            valid_data.push_back(data[i]);
        }
    }

    int n_valid = valid_data.size();
    if (n_valid == 0) {
        return result;
    }

    // Sort ascending
    std::sort(valid_data.begin(), valid_data.end());

    // Calculate percentiles
    for (int i = 0; i < n_probs; ++i) {
        double p = probs[i];
        if (p < 0 || p > 1) {
            result[i] = NA_REAL;
            continue;
        }

        // Linear interpolation (type 7 in R's quantile function)
        double idx = (n_valid - 1) * p;
        int idx_lo = static_cast<int>(std::floor(idx));
        int idx_hi = static_cast<int>(std::ceil(idx));

        if (idx_lo == idx_hi || idx_hi >= n_valid) {
            result[i] = valid_data[std::min(idx_lo, n_valid - 1)];
        } else {
            double frac = idx - idx_lo;
            result[i] = valid_data[idx_lo] * (1 - frac) + valid_data[idx_hi] * frac;
        }
    }

    return result;
}


