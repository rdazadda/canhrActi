// wear_time_cpp.cpp - High-performance wear time detection algorithms
// Implements Troiano (2007), Choi (2011), and CANHR (2025) algorithms
// Part of canhrActi: CANHR Accelerometer Analysis Package
//
// References:
// - Troiano RP, et al. (2008). Physical activity in the United States measured
//   by accelerometer. Medicine & Science in Sports & Exercise, 40(1), 181-188.
// - Choi L, et al. (2011). Validation of accelerometer wear and nonwear time
//   classification algorithm. Medicine & Science in Sports & Exercise, 43(2), 357-364.

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Core Wear Time Detection Engine

// Core wear detection algorithm (configurable for all three methods)
// [[Rcpp::export]]
IntegerVector detect_wear_core_cpp(NumericVector counts,
                                    int window_minutes,
                                    int spike_tolerance_minutes,
                                    int spike_max_count,
                                    bool use_upstream_downstream,
                                    int upstream_minutes,
                                    int downstream_minutes) {
    int n = counts.size();
    IntegerVector wear(n, 1);  // Initialize all as wear (1)

    if (n == 0) return wear;

    // Sliding window to detect consecutive zeros
    int i = 0;
    while (i < n) {
        // Count consecutive zeros/low counts
        int zero_start = i;
        int zero_count = 0;
        int spike_count = 0;
        int in_spike = 0;

        while (i < n) {
            if (counts[i] <= spike_max_count) {
                if (counts[i] > 0) {
                    // This is a spike
                    in_spike++;
                    if (in_spike > spike_tolerance_minutes) {
                        // Spike too long, break the zero run
                        break;
                    }
                } else {
                    // True zero
                    in_spike = 0;
                }
                zero_count++;
                i++;
            } else {
                // Non-zero count above spike threshold
                if (in_spike > 0 && in_spike <= spike_tolerance_minutes) {
                    // Was in a valid spike, continue
                    spike_count++;
                }
                break;
            }
        }

        // Check if this qualifies as non-wear
        // CRITICAL FIX: Should be >= (window_minutes - spike_tolerance_minutes)
        // This allows for spike tolerance within the non-wear window
        // Matches R implementation and GGIR/ActiLife standards
        int actual_zeros = zero_count - spike_count;
        int min_zeros_required = window_minutes - spike_tolerance_minutes;

        if (actual_zeros >= min_zeros_required) {
            // Check upstream/downstream if required (Choi algorithm)
            bool valid_nonwear = true;

            if (use_upstream_downstream && spike_count > 0) {
                // Choi 2011: BOTH upstream AND downstream windows MUST be
                // exactly upstream_minutes/downstream_minutes long and all zeros

                // Check upstream - must have full window length
                int upstream_start = zero_start - upstream_minutes;
                bool has_full_upstream = upstream_start >= 0;
                int upstream_activity = 0;
                if (has_full_upstream) {
                    for (int j = upstream_start; j < zero_start; ++j) {
                        upstream_activity += static_cast<int>(counts[j]);
                    }
                }

                // Check downstream - must have full window length
                int downstream_end = i + downstream_minutes;
                bool has_full_downstream = downstream_end <= n;
                int downstream_activity = 0;
                if (has_full_downstream) {
                    for (int j = i; j < downstream_end; ++j) {
                        downstream_activity += static_cast<int>(counts[j]);
                    }
                }

                // Both windows must exist with full length and have zero activity
                if (!has_full_upstream || !has_full_downstream ||
                    upstream_activity > 0 || downstream_activity > 0) {
                    valid_nonwear = false;
                }
            }

            if (valid_nonwear) {
                // Mark as non-wear
                for (int j = zero_start; j < i; ++j) {
                    wear[j] = 0;
                }
            }
        }

        // Move to next position if stuck
        if (i == zero_start) {
            i++;
        }
    }

    return wear;
}


// Troiano Algorithm (2007)

// Troiano: 60-min window, 2-min spikes allowed, max 100 counts during spike
// [[Rcpp::export]]
IntegerVector wear_troiano_cpp(NumericVector counts,
                                int window_minutes = 60,
                                int spike_tolerance = 2,
                                int spike_max_count = 100) {
    return detect_wear_core_cpp(
        counts,
        window_minutes,
        spike_tolerance,
        spike_max_count,
        false,  // No upstream/downstream check
        0,
        0
    );
}


// Choi Algorithm (2011)

// Choi: 90-min window, 2-min spikes, 30-min upstream/downstream validation
// [[Rcpp::export]]
IntegerVector wear_choi_cpp(NumericVector counts,
                             int window_minutes = 90,
                             int spike_tolerance = 2,
                             int spike_max_count = 100,
                             int upstream_minutes = 30,
                             int downstream_minutes = 30) {
    return detect_wear_core_cpp(
        counts,
        window_minutes,
        spike_tolerance,
        spike_max_count,
        true,   // Use upstream/downstream check
        upstream_minutes,
        downstream_minutes
    );
}


// CANHR Algorithm (2025)

// CANHR2025: 120-min window, 3-min spikes, 45-min validation
// More conservative for Arctic populations with long sedentary periods
// CRITICAL: spike_max_count should be 150 (higher than standard 100) per CANHR design
// [[Rcpp::export]]
IntegerVector wear_canhr2025_cpp(NumericVector counts,
                                  int window_minutes = 120,
                                  int spike_tolerance = 3,
                                  int spike_max_count = 150,
                                  int upstream_minutes = 45,
                                  int downstream_minutes = 45) {
    return detect_wear_core_cpp(
        counts,
        window_minutes,
        spike_tolerance,
        spike_max_count,
        true,
        upstream_minutes,
        downstream_minutes
    );
}


// Advanced Wear Detection Using Multiple Axes

// Detect wear using all three axes (more robust)
// [[Rcpp::export]]
IntegerVector wear_triaxial_cpp(NumericVector axis1,
                                 NumericVector axis2,
                                 NumericVector axis3,
                                 int window_minutes = 90,
                                 int spike_tolerance = 2,
                                 double sd_threshold = 3.0) {
    int n = axis1.size();
    IntegerVector wear(n, 1);

    if (n == 0) return wear;

    // Calculate VM for each epoch
    NumericVector vm(n);
    for (int i = 0; i < n; ++i) {
        vm[i] = std::sqrt(axis1[i] * axis1[i] +
                         axis2[i] * axis2[i] +
                         axis3[i] * axis3[i]);
    }

    // Sliding window detection
    for (int i = 0; i <= n - window_minutes; ++i) {
        // Calculate SD of each axis in window
        double mean1 = 0, mean2 = 0, mean3 = 0;
        for (int j = 0; j < window_minutes; ++j) {
            mean1 += axis1[i + j];
            mean2 += axis2[i + j];
            mean3 += axis3[i + j];
        }
        mean1 /= window_minutes;
        mean2 /= window_minutes;
        mean3 /= window_minutes;

        double sd1 = 0, sd2 = 0, sd3 = 0;
        for (int j = 0; j < window_minutes; ++j) {
            sd1 += std::pow(axis1[i + j] - mean1, 2);
            sd2 += std::pow(axis2[i + j] - mean2, 2);
            sd3 += std::pow(axis3[i + j] - mean3, 2);
        }
        sd1 = std::sqrt(sd1 / (window_minutes - 1));
        sd2 = std::sqrt(sd2 / (window_minutes - 1));
        sd3 = std::sqrt(sd3 / (window_minutes - 1));

        // If all axes have very low SD, likely non-wear
        if (sd1 < sd_threshold && sd2 < sd_threshold && sd3 < sd_threshold) {
            // Check for consecutive zeros
            int zero_count = 0;
            for (int j = 0; j < window_minutes; ++j) {
                if (vm[i + j] < 1.0) {  // Very low VM
                    zero_count++;
                }
            }

            // If most of window is near-zero, mark as non-wear
            if (zero_count >= window_minutes - spike_tolerance) {
                for (int j = 0; j < window_minutes; ++j) {
                    wear[i + j] = 0;
                }
            }
        }
    }

    return wear;
}


// Wear Time Summary Statistics

// Calculate wear time statistics
// [[Rcpp::export]]
Rcpp::List wear_summary_cpp(IntegerVector wear,
                             int epochs_per_hour = 60,
                             int min_wear_hours = 10) {
    int n = wear.size();

    // Guard against empty input
    if (n == 0) {
        return Rcpp::List::create(
            Named("total_epochs") = 0,
            Named("wear_epochs") = 0,
            Named("nonwear_epochs") = 0,
            Named("wear_hours") = 0.0,
            Named("nonwear_hours") = 0.0,
            Named("wear_percent") = 0.0,
            Named("valid_day") = false,
            Named("n_wear_periods") = 0,
            Named("n_nonwear_periods") = 0,
            Named("max_wear_period_epochs") = 0,
            Named("max_nonwear_period_epochs") = 0
        );
    }

    // Count total wear/non-wear
    int total_wear = 0;
    int total_nonwear = 0;
    for (int i = 0; i < n; ++i) {
        if (wear[i] == 1) {
            total_wear++;
        } else {
            total_nonwear++;
        }
    }

    double wear_hours = static_cast<double>(total_wear) / epochs_per_hour;
    double nonwear_hours = static_cast<double>(total_nonwear) / epochs_per_hour;
    double wear_percent = (n > 0) ? (100.0 * total_wear / n) : 0.0;

    bool valid_day = wear_hours >= min_wear_hours;

    // Count wear periods
    int n_wear_periods = 0;
    int n_nonwear_periods = 0;
    int max_wear_period = 0;
    int max_nonwear_period = 0;
    int current_period = 0;
    int current_state = wear[0];

    for (int i = 0; i < n; ++i) {
        if (wear[i] == current_state) {
            current_period++;
        } else {
            // End of period
            if (current_state == 1) {
                n_wear_periods++;
                if (current_period > max_wear_period) {
                    max_wear_period = current_period;
                }
            } else {
                n_nonwear_periods++;
                if (current_period > max_nonwear_period) {
                    max_nonwear_period = current_period;
                }
            }
            current_state = wear[i];
            current_period = 1;
        }
    }
    // Don't forget last period
    if (current_state == 1) {
        n_wear_periods++;
        if (current_period > max_wear_period) {
            max_wear_period = current_period;
        }
    } else {
        n_nonwear_periods++;
        if (current_period > max_nonwear_period) {
            max_nonwear_period = current_period;
        }
    }

    return Rcpp::List::create(
        Named("total_epochs") = n,
        Named("wear_epochs") = total_wear,
        Named("nonwear_epochs") = total_nonwear,
        Named("wear_hours") = wear_hours,
        Named("nonwear_hours") = nonwear_hours,
        Named("wear_percent") = wear_percent,
        Named("valid_day") = valid_day,
        Named("n_wear_periods") = n_wear_periods,
        Named("n_nonwear_periods") = n_nonwear_periods,
        Named("max_wear_period_epochs") = max_wear_period,
        Named("max_nonwear_period_epochs") = max_nonwear_period
    );
}


// Extract Continuous Wear Periods

// Get start/end indices of wear periods
// [[Rcpp::export]]
Rcpp::DataFrame get_wear_periods_cpp(IntegerVector wear,
                                      int min_duration_epochs = 1) {
    int n = wear.size();

    std::vector<int> starts, ends, durations;
    std::vector<bool> is_wear;

    // Guard against empty input
    if (n == 0) {
        return Rcpp::DataFrame::create(
            Named("start") = starts,
            Named("end") = ends,
            Named("duration") = durations,
            Named("is_wear") = is_wear
        );
    }

    int period_start = 0;
    int current_state = wear[0];

    for (int i = 1; i <= n; ++i) {
        if (i == n || wear[i] != current_state) {
            int duration = i - period_start;

            if (duration >= min_duration_epochs) {
                starts.push_back(period_start + 1);  // 1-indexed for R
                ends.push_back(i);
                durations.push_back(duration);
                is_wear.push_back(current_state == 1);
            }

            if (i < n) {
                current_state = wear[i];
                period_start = i;
            }
        }
    }

    return Rcpp::DataFrame::create(
        Named("start") = starts,
        Named("end") = ends,
        Named("duration") = durations,
        Named("is_wear") = is_wear
    );
}


// Day-by-Day Wear Time Calculation

// Calculate wear time per day
// [[Rcpp::export]]
Rcpp::DataFrame wear_by_day_cpp(IntegerVector wear,
                                 IntegerVector day_indices,
                                 int epochs_per_hour = 60,
                                 int min_wear_hours = 10) {
    // Get unique days
    std::vector<int> unique_days;
    for (int i = 0; i < day_indices.size(); ++i) {
        if (unique_days.empty() || day_indices[i] != unique_days.back()) {
            unique_days.push_back(day_indices[i]);
        }
    }

    int n_days = unique_days.size();
    std::vector<int> days(n_days);
    std::vector<double> wear_hours(n_days);
    std::vector<double> nonwear_hours(n_days);
    std::vector<bool> valid(n_days);

    for (int d = 0; d < n_days; ++d) {
        int day = unique_days[d];
        days[d] = day;

        int wear_count = 0;
        int total_count = 0;

        for (int i = 0; i < wear.size(); ++i) {
            if (day_indices[i] == day) {
                total_count++;
                if (wear[i] == 1) {
                    wear_count++;
                }
            }
        }

        wear_hours[d] = static_cast<double>(wear_count) / epochs_per_hour;
        nonwear_hours[d] = static_cast<double>(total_count - wear_count) / epochs_per_hour;
        valid[d] = wear_hours[d] >= min_wear_hours;
    }

    return Rcpp::DataFrame::create(
        Named("day") = days,
        Named("wear_hours") = wear_hours,
        Named("nonwear_hours") = nonwear_hours,
        Named("valid") = valid
    );
}
