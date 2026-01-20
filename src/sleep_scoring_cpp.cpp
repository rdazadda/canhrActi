// sleep_scoring_cpp.cpp - High-performance sleep scoring algorithms
// Implements Cole-Kripke (1992), Sadeh (1994), and Tudor-Locke algorithms
// Part of canhrActi: CANHR Accelerometer Analysis Package
//
// References:
// - Cole RJ, et al. (1992). Automatic sleep/wake identification from wrist activity.
//   Sleep, 15(5), 461-469.
// - Sadeh A, et al. (1994). Activity-based sleep-wake identification: An empirical
//   test of methodological issues. Sleep, 17(3), 201-207.
// - Tudor-Locke C, et al. (2014). Fully automated sleep scoring algorithms.
//   Sleep Medicine, 15(12), 1505-1513.

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Forward declarations
IntegerVector webster_rescoring_cpp(IntegerVector sleep);

// Cole-Kripke Algorithm (1992)

// Cole-Kripke scoring with 7-epoch window (±3 epochs + current)
// D = 0.001 × (106×P4 + 54×P3 + 58×P2 + 76×P1 + 230×C + 74×N1 + 67×N2)
// Sleep if D < 1.0
// CRITICAL: Counts must be divided by 100 and capped at 300 per Cole et al. (1992)
// [[Rcpp::export]]
IntegerVector cole_kripke_cpp(NumericVector counts,
                               double threshold = 1.0,
                               bool apply_rescoring = true) {
    int n = counts.size();
    IntegerVector sleep(n, 0);  // 0 = wake, 1 = sleep

    if (n < 7) {
        // Not enough data for Cole-Kripke
        return sleep;
    }

    // CRITICAL FIX: Scale counts by dividing by 100 and cap at 300
    // This matches the R implementation and ActiLife/GGIR standards
    // Reference: Cole et al., Sleep 15(5):461-469, 1992
    NumericVector scaled_counts(n);
    for (int i = 0; i < n; ++i) {
        double scaled = counts[i] / 100.0;
        scaled_counts[i] = (scaled > 300.0) ? 300.0 : scaled;
    }

    // Cole-Kripke coefficients: D = 0.001 * (106*A4 + 54*A3 + 58*A2 + 76*A1 + 230*A0 + 74*N1 + 67*N2)
    // Reference: Cole et al., Sleep 15(5):461-469, 1992, Table 2
    const double P4_coef = 0.106;  // 0.001 * 106 = 0.106
    const double P3_coef = 0.054;
    const double P2_coef = 0.058;
    const double P1_coef = 0.076;
    const double C_coef  = 0.230;
    const double N1_coef = 0.074;
    const double N2_coef = 0.067;

    // Score each epoch using SCALED counts
    for (int i = 4; i < n - 2; ++i) {
        double D = P4_coef * scaled_counts[i - 4] +
                   P3_coef * scaled_counts[i - 3] +
                   P2_coef * scaled_counts[i - 2] +
                   P1_coef * scaled_counts[i - 1] +
                   C_coef  * scaled_counts[i] +
                   N1_coef * scaled_counts[i + 1] +
                   N2_coef * scaled_counts[i + 2];

        sleep[i] = (D < threshold) ? 1 : 0;
    }

    // Handle edges (use simpler rule: current epoch only, using SCALED counts)
    for (int i = 0; i < 4; ++i) {
        sleep[i] = (scaled_counts[i] < threshold / C_coef) ? 1 : 0;
    }
    for (int i = n - 2; i < n; ++i) {
        sleep[i] = (scaled_counts[i] < threshold / C_coef) ? 1 : 0;
    }

    // Apply Webster's rescoring rules if requested
    if (apply_rescoring) {
        sleep = webster_rescoring_cpp(sleep);
    }

    return sleep;
}


// Webster's rescoring rules (1982) - Original paper specification
// Rules reduce false positive sleep detection (convert sleep → wake)
// [[Rcpp::export]]
IntegerVector webster_rescoring_cpp(IntegerVector sleep) {
    int n = sleep.size();
    IntegerVector rescored = clone(sleep);

    // Rule 1: After >= 4 consecutive minutes of WAKE,
    // the first 1 minute of SLEEP is rescored as WAKE
    for (int i = 4; i < n; ++i) {
        bool four_wake = true;
        for (int j = i - 4; j < i; ++j) {
            if (sleep[j] != 0) {  // Check for WAKE (0)
                four_wake = false;
                break;
            }
        }
        if (four_wake && sleep[i] == 1) {  // Current is SLEEP
            rescored[i] = 0;  // Rescore as WAKE
        }
    }

    // Rule 2: After >= 10 consecutive minutes of WAKE,
    // the first 3 minutes of SLEEP are rescored as WAKE
    for (int i = 10; i < n; ++i) {
        bool ten_wake = true;
        for (int j = i - 10; j < i; ++j) {
            if (sleep[j] != 0) {  // Check for WAKE (0)
                ten_wake = false;
                break;
            }
        }
        if (ten_wake) {
            for (int j = i; j < std::min(i + 3, n); ++j) {
                if (sleep[j] == 1) {  // If SLEEP
                    rescored[j] = 0;  // Rescore as WAKE
                }
            }
        }
    }

    // Rule 3: After >= 15 consecutive minutes of WAKE,
    // the first 4 minutes of SLEEP are rescored as WAKE
    for (int i = 15; i < n; ++i) {
        bool fifteen_wake = true;
        for (int j = i - 15; j < i; ++j) {
            if (sleep[j] != 0) {  // Check for WAKE (0)
                fifteen_wake = false;
                break;
            }
        }
        if (fifteen_wake) {
            for (int j = i; j < std::min(i + 4, n); ++j) {
                if (sleep[j] == 1) {  // If SLEEP
                    rescored[j] = 0;  // Rescore as WAKE
                }
            }
        }
    }

    // Rules 4-5: Sleep bouts surrounded by long wake periods
    // Use run-length encoding approach to find sleep/wake bouts
    // Reference: Webster JB et al. (1982). Psychophysiology. 19(6):682-687

    int sleep_start = -1;
    for (int i = 0; i < n; ++i) {
        if (rescored[i] == 1 && sleep_start < 0) {
            sleep_start = i;
        } else if (rescored[i] == 0 && sleep_start >= 0) {
            int sleep_duration = i - sleep_start;

            // Count wake minutes before the sleep bout
            int wake_before = 0;
            for (int j = sleep_start - 1; j >= 0 && rescored[j] == 0; --j) {
                wake_before++;
            }

            // Count wake minutes after the sleep bout
            int wake_after = 0;
            for (int j = i; j < n && rescored[j] == 0; ++j) {
                wake_after++;
            }

            // Rule 4: <= 6 min sleep surrounded by >= 15 min wake on BOTH sides
            if (sleep_duration <= 6 && wake_before >= 15 && wake_after >= 15) {
                for (int j = sleep_start; j < i; ++j) {
                    rescored[j] = 0;
                }
            }

            // Rule 5: <= 10 min sleep surrounded by >= 20 min wake on BOTH sides
            // This is checked separately (not else-if) as it uses different thresholds
            if (sleep_duration <= 10 && wake_before >= 20 && wake_after >= 20) {
                for (int j = sleep_start; j < i; ++j) {
                    rescored[j] = 0;
                }
            }

            sleep_start = -1;
        }
    }

    return rescored;
}


// Sadeh Algorithm (1994)

// Sadeh scoring using activity statistics in 11-minute window
// PS = 7.601 - 0.065×AVG - 1.08×NATS - 0.056×SD - 0.703×LG
// Sleep if PS > 0 (some versions use PS > -4)
// CRITICAL: Activity counts MUST be capped at 300 per Sadeh et al. (1994)
// [[Rcpp::export]]
IntegerVector sadeh_cpp(NumericVector counts,
                         double threshold = -4.0) {
    int n = counts.size();
    IntegerVector sleep(n, 0);

    if (n < 11) {
        return sleep;
    }

    // CRITICAL FIX: Cap counts at 300 per Sadeh algorithm specification
    // This matches the R implementation and ActiLife/GGIR standards
    NumericVector capped_counts(n);
    for (int i = 0; i < n; ++i) {
        capped_counts[i] = (counts[i] > 300.0) ? 300.0 : counts[i];
    }

    // For each epoch, calculate metrics in 11-minute window (5 before, current, 5 after)
    for (int i = 5; i < n - 5; ++i) {
        // AVG: Mean activity in window (using CAPPED counts)
        double sum = 0.0;
        for (int j = i - 5; j <= i + 5; ++j) {
            sum += capped_counts[j];
        }
        double AVG = sum / 11.0;

        // SD: Standard deviation in 6-epoch BACKWARD window (current + 5 previous)
        // Per Sadeh (1994), SD6 uses last 6 minutes, not the full 11-epoch window
        // Using CAPPED counts
        double sd_sum = 0.0;
        for (int j = i - 5; j <= i; ++j) {
            sd_sum += capped_counts[j];
        }
        double sd_mean = sd_sum / 6.0;
        double sq_sum = 0.0;
        for (int j = i - 5; j <= i; ++j) {
            sq_sum += (capped_counts[j] - sd_mean) * (capped_counts[j] - sd_mean);
        }
        double SD = std::sqrt(sq_sum / 5.0);  // Sample SD (n-1)

        // NATS: Number of epochs with activity >= 50 and < 100 (using CAPPED counts)
        int NATS = 0;
        for (int j = i - 5; j <= i + 5; ++j) {
            if (capped_counts[j] >= 50 && capped_counts[j] < 100) {
                NATS++;
            }
        }

        // LG: Natural log of activity + 1 (current epoch, using CAPPED count)
        double LG = std::log(capped_counts[i] + 1.0);

        // Probability score
        double PS = 7.601 - 0.065 * AVG - 1.08 * NATS - 0.056 * SD - 0.703 * LG;

        sleep[i] = (PS > threshold) ? 1 : 0;
    }

    // Handle edges with simpler rule (using CAPPED counts)
    for (int i = 0; i < 5; ++i) {
        sleep[i] = (capped_counts[i] < 50) ? 1 : 0;
    }
    for (int i = n - 5; i < n; ++i) {
        sleep[i] = (capped_counts[i] < 50) ? 1 : 0;
    }

    return sleep;
}


// Tudor-Locke Algorithm (2014)

// Tudor-Locke with configurable parameters
// [[Rcpp::export]]
IntegerVector tudor_locke_cpp(NumericVector counts,
                               int sleep_threshold = 20,
                               int min_sleep_block = 160,
                               int max_wake_block = 60) {
    int n = counts.size();
    IntegerVector sleep(n, 0);

    // First pass: mark potential sleep (counts below threshold)
    IntegerVector potential_sleep(n);
    for (int i = 0; i < n; ++i) {
        potential_sleep[i] = (counts[i] <= sleep_threshold) ? 1 : 0;
    }

    // Find sleep blocks (consecutive potential sleep epochs)
    int block_start = -1;
    for (int i = 0; i <= n; ++i) {
        if (i < n && potential_sleep[i] == 1) {
            if (block_start < 0) {
                block_start = i;
            }
        } else {
            if (block_start >= 0) {
                int block_length = i - block_start;

                // Check if this is a valid sleep block
                if (block_length >= min_sleep_block) {
                    // Mark as sleep
                    for (int j = block_start; j < i; ++j) {
                        sleep[j] = 1;
                    }
                }

                block_start = -1;
            }
        }
    }

    // Fill in short wake gaps within sleep
    // Find first and last sleep
    int first_sleep = -1, last_sleep = -1;
    for (int i = 0; i < n; ++i) {
        if (sleep[i] == 1) {
            if (first_sleep < 0) first_sleep = i;
            last_sleep = i;
        }
    }

    if (first_sleep >= 0 && last_sleep > first_sleep) {
        // Fill gaps shorter than max_wake_block
        int wake_start = -1;
        for (int i = first_sleep; i <= last_sleep; ++i) {
            if (sleep[i] == 0) {
                if (wake_start < 0) wake_start = i;
            } else {
                if (wake_start >= 0) {
                    int wake_length = i - wake_start;
                    if (wake_length <= max_wake_block) {
                        // Fill this gap
                        for (int j = wake_start; j < i; ++j) {
                            sleep[j] = 1;
                        }
                    }
                    wake_start = -1;
                }
            }
        }
    }

    return sleep;
}


// Sleep Period Detection

// Detect sleep onset and wake time
// [[Rcpp::export]]
Rcpp::List detect_sleep_period_cpp(IntegerVector sleep,
                                    int min_sleep_duration = 30,
                                    int sleep_onset_buffer = 5,
                                    int wake_buffer = 5) {
    int n = sleep.size();

    // Find longest continuous sleep block
    int best_start = -1, best_end = -1, best_length = 0;
    int current_start = -1;

    for (int i = 0; i <= n; ++i) {
        if (i < n && sleep[i] == 1) {
            if (current_start < 0) {
                current_start = i;
            }
        } else {
            if (current_start >= 0) {
                int length = i - current_start;
                if (length > best_length) {
                    best_length = length;
                    best_start = current_start;
                    best_end = i;
                }
                current_start = -1;
            }
        }
    }

    if (best_length < min_sleep_duration) {
        return Rcpp::List::create(
            Named("sleep_onset") = NA_INTEGER,
            Named("wake_time") = NA_INTEGER,
            Named("time_in_bed") = NA_INTEGER,
            Named("total_sleep_time") = NA_INTEGER,
            Named("sleep_efficiency") = NA_REAL,
            Named("valid") = false
        );
    }

    // Extend to find actual sleep onset (first sleep epoch before main block)
    int sleep_onset = best_start;
    for (int i = best_start - 1; i >= std::max(0, best_start - sleep_onset_buffer * 60); --i) {
        if (sleep[i] == 1) {
            sleep_onset = i;
        } else {
            break;
        }
    }

    // Find wake time (last sleep epoch after main block)
    int wake_time = best_end;
    for (int i = best_end; i < std::min(n, best_end + wake_buffer * 60); ++i) {
        if (sleep[i] == 1) {
            wake_time = i + 1;
        } else {
            break;
        }
    }

    // Calculate sleep metrics
    int time_in_bed = wake_time - sleep_onset;
    int total_sleep_time = 0;
    for (int i = sleep_onset; i < wake_time; ++i) {
        if (sleep[i] == 1) {
            total_sleep_time++;
        }
    }

    double sleep_efficiency = (time_in_bed > 0) ?
        (100.0 * total_sleep_time / time_in_bed) : NA_REAL;

    return Rcpp::List::create(
        Named("sleep_onset") = sleep_onset + 1,  // 1-indexed for R
        Named("wake_time") = wake_time,
        Named("time_in_bed") = time_in_bed,
        Named("total_sleep_time") = total_sleep_time,
        Named("sleep_efficiency") = sleep_efficiency,
        Named("valid") = true
    );
}


// Sleep Quality Metrics

// Calculate comprehensive sleep metrics
// [[Rcpp::export]]
Rcpp::List calculate_sleep_metrics_cpp(IntegerVector sleep,
                                        int sleep_onset,
                                        int wake_time) {
    int n = sleep.size();

    // Adjust to 0-indexed
    sleep_onset--;
    wake_time--;

    // Bounds checking to prevent out-of-bounds access
    if (sleep_onset < 0) sleep_onset = 0;
    if (wake_time > n) wake_time = n;
    if (sleep_onset >= n || wake_time < 0) {
        return Rcpp::List::create(
            Named("total_sleep_time") = NA_INTEGER,
            Named("sleep_efficiency") = NA_REAL,
            Named("sleep_onset_latency") = NA_INTEGER,
            Named("waso") = NA_INTEGER,
            Named("number_of_awakenings") = NA_INTEGER,
            Named("avg_awakening_length") = NA_REAL,
            Named("fragmentation_index") = NA_REAL
        );
    }

    int time_in_bed = wake_time - sleep_onset;
    if (time_in_bed <= 0) {
        return Rcpp::List::create(
            Named("total_sleep_time") = NA_INTEGER,
            Named("sleep_efficiency") = NA_REAL,
            Named("sleep_onset_latency") = NA_INTEGER,
            Named("waso") = NA_INTEGER,
            Named("number_of_awakenings") = NA_INTEGER,
            Named("avg_awakening_length") = NA_REAL,
            Named("fragmentation_index") = NA_REAL
        );
    }

    // Total sleep time
    int total_sleep = 0;
    for (int i = sleep_onset; i < wake_time; ++i) {
        if (sleep[i] == 1) {
            total_sleep++;
        }
    }

    // Sleep onset latency (time from start to first sleep)
    int sol = 0;
    for (int i = sleep_onset; i < wake_time; ++i) {
        if (sleep[i] == 1) break;
        sol++;
    }

    // Find last sleep epoch
    int last_sleep = wake_time - 1;
    for (int i = wake_time - 1; i >= sleep_onset; --i) {
        if (sleep[i] == 1) {
            last_sleep = i;
            break;
        }
    }

    // WASO: Wake after sleep onset (excluding final wake)
    int waso = 0;
    int first_sleep = sleep_onset + sol;
    for (int i = first_sleep; i <= last_sleep; ++i) {
        if (sleep[i] == 0) {
            waso++;
        }
    }

    // Number of awakenings
    int n_awakenings = 0;
    bool in_sleep = false;
    for (int i = first_sleep; i <= last_sleep; ++i) {
        if (sleep[i] == 1) {
            in_sleep = true;
        } else {
            if (in_sleep) {
                n_awakenings++;
                in_sleep = false;
            }
        }
    }

    // Average awakening length
    double avg_awakening = (n_awakenings > 0) ?
        (static_cast<double>(waso) / n_awakenings) : 0.0;

    // Sleep efficiency
    double efficiency = (time_in_bed > 0) ?
        (100.0 * total_sleep / time_in_bed) : NA_REAL;

    // Fragmentation index (movement + fragmentation)
    // Simple version: percentage of 1-minute mobile windows
    int mobile_count = 0;
    for (int i = first_sleep; i <= last_sleep; ++i) {
        if (sleep[i] == 0) {
            mobile_count++;
        }
    }
    double frag_index = (last_sleep > first_sleep) ?
        (100.0 * mobile_count / (last_sleep - first_sleep + 1)) : NA_REAL;

    return Rcpp::List::create(
        Named("total_sleep_time") = total_sleep,
        Named("sleep_efficiency") = efficiency,
        Named("sleep_onset_latency") = sol,
        Named("waso") = waso,
        Named("number_of_awakenings") = n_awakenings,
        Named("avg_awakening_length") = avg_awakening,
        Named("fragmentation_index") = frag_index
    );
}


// Sleep Regularity Index (SRI)

// Calculate SRI (Phillips et al., 2017)
// Measures day-to-day sleep-wake pattern regularity
// [[Rcpp::export]]
double calculate_sri_cpp(IntegerMatrix sleep_matrix) {
    // sleep_matrix: rows = days, columns = epochs (e.g., 1440 for minute data)
    int n_days = sleep_matrix.nrow();
    int n_epochs = sleep_matrix.ncol();

    if (n_days < 2) {
        return NA_REAL;
    }

    // Calculate pairwise agreement between consecutive days
    double total_agreement = 0.0;
    int n_comparisons = 0;

    for (int d = 0; d < n_days - 1; ++d) {
        int agreements = 0;
        for (int e = 0; e < n_epochs; ++e) {
            if (sleep_matrix(d, e) == sleep_matrix(d + 1, e)) {
                agreements++;
            }
        }
        total_agreement += static_cast<double>(agreements) / n_epochs;
        n_comparisons++;
    }

    // SRI = -100 + 200 * (mean agreement)
    // Ranges from -100 (perfectly irregular) to 100 (perfectly regular)
    double mean_agreement = total_agreement / n_comparisons;
    double sri = -100.0 + 200.0 * mean_agreement;

    return sri;
}


// Vectorized version for single vector with day boundaries
// [[Rcpp::export]]
double calculate_sri_vector_cpp(IntegerVector sleep,
                                 int epochs_per_day) {
    int n = sleep.size();
    int n_days = n / epochs_per_day;

    if (n_days < 2) {
        return NA_REAL;
    }

    // Calculate agreement between consecutive days
    double total_agreement = 0.0;
    int n_comparisons = n_days - 1;

    for (int d = 0; d < n_days - 1; ++d) {
        int agreements = 0;
        for (int e = 0; e < epochs_per_day; ++e) {
            int idx1 = d * epochs_per_day + e;
            int idx2 = (d + 1) * epochs_per_day + e;

            if (idx2 < n && sleep[idx1] == sleep[idx2]) {
                agreements++;
            }
        }
        total_agreement += static_cast<double>(agreements) / epochs_per_day;
    }

    double mean_agreement = total_agreement / n_comparisons;
    double sri = -100.0 + 200.0 * mean_agreement;

    return sri;
}


// Night-by-Night Sleep Summary

// Calculate sleep metrics for multiple nights
// [[Rcpp::export]]
Rcpp::DataFrame sleep_summary_by_night_cpp(IntegerVector sleep,
                                            IntegerVector night_indices,
                                            NumericVector counts) {
    // Get unique nights
    std::vector<int> unique_nights;
    for (int i = 0; i < night_indices.size(); ++i) {
        if (unique_nights.empty() || night_indices[i] != unique_nights.back()) {
            unique_nights.push_back(night_indices[i]);
        }
    }

    int n_nights = unique_nights.size();
    std::vector<int> nights(n_nights);
    std::vector<int> tst(n_nights);
    std::vector<double> efficiency(n_nights);
    std::vector<int> sol(n_nights);
    std::vector<int> waso(n_nights);
    std::vector<int> awakenings(n_nights);

    for (int night = 0; night < n_nights; ++night) {
        int night_id = unique_nights[night];
        nights[night] = night_id;

        // Extract sleep for this night
        std::vector<int> night_sleep;
        for (int i = 0; i < sleep.size(); ++i) {
            if (night_indices[i] == night_id) {
                night_sleep.push_back(sleep[i]);
            }
        }

        if (night_sleep.empty()) {
            tst[night] = NA_INTEGER;
            efficiency[night] = NA_REAL;
            sol[night] = NA_INTEGER;
            waso[night] = NA_INTEGER;
            awakenings[night] = NA_INTEGER;
            continue;
        }

        // Find sleep period
        int first_sleep = -1, last_sleep = -1;
        for (int i = 0; i < static_cast<int>(night_sleep.size()); ++i) {
            if (night_sleep[i] == 1) {
                if (first_sleep < 0) first_sleep = i;
                last_sleep = i;
            }
        }

        if (first_sleep < 0) {
            tst[night] = 0;
            efficiency[night] = 0.0;
            sol[night] = NA_INTEGER;
            waso[night] = NA_INTEGER;
            awakenings[night] = 0;
            continue;
        }

        // Calculate metrics
        int total_sleep = 0;
        int wake_count = 0;
        int awakening_count = 0;
        bool was_asleep = false;

        for (int i = first_sleep; i <= last_sleep; ++i) {
            if (night_sleep[i] == 1) {
                total_sleep++;
                was_asleep = true;
            } else {
                wake_count++;
                if (was_asleep) {
                    awakening_count++;
                    was_asleep = false;
                }
            }
        }

        tst[night] = total_sleep;
        int time_in_bed = last_sleep - first_sleep + 1;
        efficiency[night] = (time_in_bed > 0) ?
            (100.0 * total_sleep / time_in_bed) : 0.0;
        sol[night] = first_sleep;
        waso[night] = wake_count;
        awakenings[night] = awakening_count;
    }

    return Rcpp::DataFrame::create(
        Named("night") = nights,
        Named("total_sleep_time") = tst,
        Named("efficiency") = efficiency,
        Named("sleep_onset_latency") = sol,
        Named("waso") = waso,
        Named("awakenings") = awakenings
    );
}
