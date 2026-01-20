// canhrActi_types.h - Common type definitions and constants
// Part of canhrActi: CANHR Accelerometer Analysis Package
// High-performance C++ core for computationally intensive operations

#ifndef CANHRACTI_TYPES_H
#define CANHRACTI_TYPES_H

#include <RcppArmadillo.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]

namespace canhrActi {

// Constants

// ActiGraph filter coefficients (Neishabouri et al., 2022)
// 8th order Butterworth bandpass filter (0.25-2.5 Hz at 30 Hz)
constexpr double ACTIGRAPH_A_COEFFS[] = {
    1.0,
    -3.6381193842463251,
    5.0152876319073795,
    -3.0806171539140930,
    0.70368824805498257
};

constexpr double ACTIGRAPH_B_COEFFS[] = {
    0.049109909160068027,
    0.0,
    -0.098219818320136054,
    0.0,
    0.049109909160068027
};

// Number of filter coefficients
constexpr int N_FILTER_COEFFS = 5;

// Default parameters
constexpr double GRAVITY = 9.80665;  // m/s^2
constexpr double DEFAULT_EPOCH_SEC = 60.0;
constexpr int DEFAULT_SAMPLE_FREQ = 30;

// Intensity cut-points (Freedson 1998, counts per minute)
constexpr int CUTPOINT_SEDENTARY = 100;
constexpr int CUTPOINT_LIGHT = 1952;
constexpr int CUTPOINT_MODERATE = 5725;
constexpr int CUTPOINT_VIGOROUS = 9498;

// Sleep scoring thresholds
constexpr double COLE_KRIPKE_THRESHOLD = 1.0;
constexpr double SADEH_THRESHOLD = -4.0;

// Wear time parameters
constexpr int TROIANO_WINDOW_MIN = 60;
constexpr int CHOI_WINDOW_MIN = 90;
constexpr int CANHR_WINDOW_MIN = 120;

// Utility Functions

// Fast median calculation for small vectors
inline double fast_median(std::vector<double>& v) {
    size_t n = v.size();
    if (n == 0) return NA_REAL;

    size_t mid = n / 2;
    std::nth_element(v.begin(), v.begin() + mid, v.end());

    if (n % 2 == 0) {
        double mid_val = v[mid];
        std::nth_element(v.begin(), v.begin() + mid - 1, v.end());
        return (v[mid - 1] + mid_val) / 2.0;
    }
    return v[mid];
}

// Fast mean calculation with division by zero guard
inline double fast_mean(const double* data, int n) {
    if (n <= 0) return NA_REAL;
    double sum = 0.0;
    for (int i = 0; i < n; ++i) {
        sum += data[i];
    }
    return sum / n;
}

// Fast standard deviation with division by zero guard
inline double fast_sd(const double* data, int n, double mean) {
    if (n <= 1) return NA_REAL;
    double sum_sq = 0.0;
    for (int i = 0; i < n; ++i) {
        double diff = data[i] - mean;
        sum_sq += diff * diff;
    }
    return std::sqrt(sum_sq / (n - 1));
}

// Clamp value to range
template<typename T>
inline T clamp(T value, T min_val, T max_val) {
    return std::max(min_val, std::min(value, max_val));
}

// Result Structures

struct BoutInfo {
    int start_index;
    int end_index;
    int duration_epochs;
    double mean_value;
    std::string type;
};

struct WearPeriod {
    int start_index;
    int end_index;
    int duration_minutes;
    bool is_wear;
};

struct SleepMetrics {
    double total_sleep_time;
    double sleep_efficiency;
    double sleep_onset_latency;
    double wake_after_sleep_onset;
    int number_of_awakenings;
    double fragmentation_index;
};

struct CircadianMetrics {
    double L5_value;
    int L5_onset;
    double M10_value;
    int M10_onset;
    double relative_amplitude;
    double interdaily_stability;
    double intradaily_variability;
};

} // namespace canhrActi

#endif // CANHRACTI_TYPES_H
