// bout_detection_cpp.cpp - High-performance bout detection algorithms
// MVPA and sedentary bout detection with transition probability analysis
// Part of canhrActi: CANHR Accelerometer Analysis Package
//
// References:
// - Troiano RP, et al. (2008). Physical activity in the United States measured
//   by accelerometer. MSSE, 40(1), 181-188.
// - Chastin SF, Granat MH (2010). Methods for objective measure, quantification
//   and analysis of sedentary behaviour. Gait & Posture, 31(1), 82-86.
// - Wanigatunga AA, et al. (2019). Association of total daily physical activity
//   and fragmented physical activity with mortality. JAMA Network Open.

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>

// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Activity Classification

// Classify epochs into intensity levels
// [[Rcpp::export]]
IntegerVector classify_intensity_cpp(NumericVector counts,
                                      int sedentary_threshold = 100,
                                      int light_threshold = 1952,
                                      int moderate_threshold = 5725,
                                      int vigorous_threshold = 9498) {
    int n = counts.size();
    IntegerVector intensity(n);

    // 0 = sedentary, 1 = light, 2 = moderate, 3 = vigorous, 4 = very vigorous
    // NA_INTEGER = -2147483648 for missing values
    for (int i = 0; i < n; ++i) {
        double c = counts[i];
        // Handle NA values - treat as sedentary (most conservative)
        if (ISNA(c) || !R_finite(c)) {
            intensity[i] = NA_INTEGER;
        } else if (c <= sedentary_threshold) {
            intensity[i] = 0;
        } else if (c <= light_threshold) {
            intensity[i] = 1;
        } else if (c <= moderate_threshold) {
            intensity[i] = 2;
        } else if (c <= vigorous_threshold) {
            intensity[i] = 3;
        } else {
            intensity[i] = 4;
        }
    }

    return intensity;
}


// MVPA Bout Detection

// Detect MVPA bouts (moderate-to-vigorous physical activity)
// [[Rcpp::export]]
Rcpp::List detect_mvpa_bouts_cpp(NumericVector counts,
                                  int moderate_threshold = 1952,
                                  int min_bout_length = 10,
                                  int drop_time = 2,
                                  bool use_80_percent_rule = false) {
    int n = counts.size();

    // Track whether each epoch is MVPA (NA values treated as non-MVPA)
    std::vector<bool> is_mvpa(n);
    for (int i = 0; i < n; ++i) {
        double c = counts[i];
        is_mvpa[i] = (!ISNA(c) && R_finite(c)) && (c >= moderate_threshold);
    }

    // Detect bouts
    std::vector<int> bout_starts;
    std::vector<int> bout_ends;
    std::vector<int> bout_lengths;
    std::vector<double> bout_means;
    std::vector<int> bout_drops;

    int i = 0;
    while (i < n) {
        // Look for start of potential bout
        if (is_mvpa[i]) {
            int bout_start = i;
            int mvpa_count = 0;
            int total_count = 0;
            int drop_count = 0;
            int consecutive_drops = 0;
            double sum_counts = 0.0;

            // Extend bout
            while (i < n) {
                if (is_mvpa[i]) {
                    mvpa_count++;
                    total_count++;
                    sum_counts += counts[i];
                    consecutive_drops = 0;
                    i++;
                } else {
                    // This is a drop (non-MVPA epoch)
                    consecutive_drops++;
                    if (consecutive_drops <= drop_time) {
                        // Allow this drop within the bout
                        drop_count++;
                        total_count++;
                        sum_counts += counts[i];
                        i++;
                    } else {
                        // Too many consecutive drops, end bout
                        // Backtrack to remove only the trailing consecutive drops
                        // Note: consecutive_drops includes current epoch which wasn't added yet
                        int drops_to_remove = consecutive_drops - 1;  // Epochs actually added
                        total_count -= drops_to_remove;
                        drop_count -= drops_to_remove;
                        // Remove sum_counts for the trailing drops (positions i-1, i-2, ..., i-drops_to_remove)
                        for (int j = 1; j <= drops_to_remove; ++j) {
                            if (i - j >= 0) {
                                sum_counts -= counts[i - j];
                            }
                        }
                        break;
                    }
                }
            }

            // Check if bout is valid
            bool valid_bout = false;
            if (use_80_percent_rule) {
                // 80% of bout must be MVPA
                valid_bout = (mvpa_count >= min_bout_length) &&
                            (static_cast<double>(mvpa_count) / total_count >= 0.8);
            } else {
                // Simple rule: enough MVPA epochs
                valid_bout = mvpa_count >= min_bout_length;
            }

            if (valid_bout) {
                bout_starts.push_back(bout_start + 1);  // 1-indexed for R
                bout_ends.push_back(bout_start + total_count);
                bout_lengths.push_back(total_count);
                bout_means.push_back(sum_counts / total_count);
                bout_drops.push_back(drop_count);
            }
        } else {
            i++;
        }
    }

    // Calculate summary statistics
    int total_mvpa = 0;
    for (int j = 0; j < n; ++j) {
        if (is_mvpa[j]) total_mvpa++;
    }

    int total_bout_time = 0;
    for (int len : bout_lengths) {
        total_bout_time += len;
    }

    return Rcpp::List::create(
        Named("bout_start") = bout_starts,
        Named("bout_end") = bout_ends,
        Named("bout_length") = bout_lengths,
        Named("bout_mean") = bout_means,
        Named("bout_drops") = bout_drops,
        Named("n_bouts") = bout_starts.size(),
        Named("total_mvpa_epochs") = total_mvpa,
        Named("total_bout_time") = total_bout_time,
        Named("percent_mvpa_in_bouts") = (total_mvpa > 0) ?
            (100.0 * total_bout_time / total_mvpa) : 0.0
    );
}


// Sedentary Bout Detection

// Detect sedentary bouts
// [[Rcpp::export]]
Rcpp::List detect_sedentary_bouts_cpp(NumericVector counts,
                                       int sedentary_threshold = 100,
                                       int min_bout_length = 1,
                                       IntegerVector wear = IntegerVector()) {
    int n = counts.size();
    bool use_wear = wear.size() == n;

    // Track whether each epoch is sedentary
    // IMPORTANT: Check for NA values to prevent undefined behavior
    std::vector<bool> is_sedentary(n);
    for (int i = 0; i < n; ++i) {
        double c = counts[i];
        bool valid = !use_wear || wear[i] == 1;
        // Handle NA values - treat as non-sedentary (most conservative)
        is_sedentary[i] = valid && !ISNA(c) && R_finite(c) && (c <= sedentary_threshold);
    }

    // Detect bouts
    std::vector<int> bout_starts;
    std::vector<int> bout_ends;
    std::vector<int> bout_lengths;
    std::vector<double> bout_means;

    int bout_start = -1;
    for (int i = 0; i <= n; ++i) {
        if (i < n && is_sedentary[i]) {
            if (bout_start < 0) {
                bout_start = i;
            }
        } else {
            if (bout_start >= 0) {
                int bout_length = i - bout_start;
                if (bout_length >= min_bout_length) {
                    // Calculate mean
                    double sum = 0.0;
                    for (int j = bout_start; j < i; ++j) {
                        sum += counts[j];
                    }

                    bout_starts.push_back(bout_start + 1);  // 1-indexed
                    bout_ends.push_back(i);
                    bout_lengths.push_back(bout_length);
                    bout_means.push_back(sum / bout_length);
                }
                bout_start = -1;
            }
        }
    }

    // Calculate summary statistics
    int total_sedentary = 0;
    for (int i = 0; i < n; ++i) {
        if (is_sedentary[i]) total_sedentary++;
    }

    int total_bout_time = 0;
    for (int len : bout_lengths) {
        total_bout_time += len;
    }

    // Calculate bout duration statistics
    double mean_bout = 0.0;
    double median_bout = 0.0;
    if (!bout_lengths.empty()) {
        for (int len : bout_lengths) {
            mean_bout += len;
        }
        mean_bout /= bout_lengths.size();

        std::vector<int> sorted_lengths = bout_lengths;
        std::sort(sorted_lengths.begin(), sorted_lengths.end(), std::greater<int>());  // Descending (longest first) per Chastin 2010
        int mid = sorted_lengths.size() / 2;
        median_bout = (sorted_lengths.size() % 2 == 0) ?
            (sorted_lengths[mid - 1] + sorted_lengths[mid]) / 2.0 :
            sorted_lengths[mid];
    }

    return Rcpp::List::create(
        Named("bout_start") = bout_starts,
        Named("bout_end") = bout_ends,
        Named("bout_length") = bout_lengths,
        Named("bout_mean") = bout_means,
        Named("n_bouts") = bout_starts.size(),
        Named("total_sedentary_epochs") = total_sedentary,
        Named("total_bout_time") = total_bout_time,
        Named("mean_bout_duration") = mean_bout,
        Named("median_bout_duration") = median_bout
    );
}


// Sedentary Fragmentation Metrics

// Calculate ASTP and SATP (transition probabilities)
// CRITICAL FIX: Added Wanigatunga method (1/mean_bout_duration) to match R implementation
// The original Markov transition method is kept as secondary metrics
// [[Rcpp::export]]
Rcpp::List calculate_transition_probabilities_cpp(NumericVector counts,
                                                   int sedentary_threshold = 100,
                                                   IntegerVector wear = IntegerVector()) {
    int n = counts.size();
    bool use_wear = wear.size() == n;

    // Count transitions (Markov method)
    int sedentary_to_active = 0;
    int sedentary_to_sedentary = 0;
    int active_to_sedentary = 0;
    int active_to_active = 0;

    // Track bout durations (Wanigatunga method - matches R implementation)
    std::vector<int> sedentary_bout_durations;
    std::vector<int> active_bout_durations;
    int current_bout_length = 0;
    int prev_state = -1;  // -1 = unknown, 0 = active, 1 = sedentary

    for (int i = 0; i < n; ++i) {
        // Skip non-wear epochs
        if (use_wear && wear[i] == 0) {
            // End current bout if in one
            if (prev_state == 1 && current_bout_length > 0) {
                sedentary_bout_durations.push_back(current_bout_length);
            } else if (prev_state == 0 && current_bout_length > 0) {
                active_bout_durations.push_back(current_bout_length);
            }
            current_bout_length = 0;
            prev_state = -1;
            continue;
        }

        // Skip if epoch has NA value
        double c_curr = counts[i];
        if (ISNA(c_curr) || !R_finite(c_curr)) {
            continue;
        }

        bool current_sed = c_curr <= sedentary_threshold;
        int current_state = current_sed ? 1 : 0;

        // Track bout durations
        if (prev_state == -1) {
            // Starting new bout
            prev_state = current_state;
            current_bout_length = 1;
        } else if (current_state == prev_state) {
            // Continue current bout
            current_bout_length++;
        } else {
            // Bout ended, save duration
            if (prev_state == 1) {
                sedentary_bout_durations.push_back(current_bout_length);
            } else {
                active_bout_durations.push_back(current_bout_length);
            }
            // Start new bout
            prev_state = current_state;
            current_bout_length = 1;
        }

        // Count transitions (for Markov method)
        if (i < n - 1) {
            if (use_wear && wear[i + 1] == 0) continue;
            double c_next = counts[i + 1];
            if (ISNA(c_next) || !R_finite(c_next)) continue;

            bool next_sed = c_next <= sedentary_threshold;
            if (current_sed) {
                if (next_sed) {
                    sedentary_to_sedentary++;
                } else {
                    sedentary_to_active++;
                }
            } else {
                if (next_sed) {
                    active_to_sedentary++;
                } else {
                    active_to_active++;
                }
            }
        }
    }

    // Save last bout
    if (prev_state == 1 && current_bout_length > 0) {
        sedentary_bout_durations.push_back(current_bout_length);
    } else if (prev_state == 0 && current_bout_length > 0) {
        active_bout_durations.push_back(current_bout_length);
    }

    // Calculate Wanigatunga ASTP/SATP (1 / mean_bout_duration)
    // This matches the R implementation in sedentary_fragmentation.R
    double mean_sedentary_duration = NA_REAL;
    double mean_active_duration = NA_REAL;

    if (sedentary_bout_durations.size() > 0) {
        double sum = 0;
        for (size_t i = 0; i < sedentary_bout_durations.size(); ++i) {
            sum += sedentary_bout_durations[i];
        }
        mean_sedentary_duration = sum / sedentary_bout_durations.size();
    }

    if (active_bout_durations.size() > 0) {
        double sum = 0;
        for (size_t i = 0; i < active_bout_durations.size(); ++i) {
            sum += active_bout_durations[i];
        }
        mean_active_duration = sum / active_bout_durations.size();
    }

    // Wanigatunga method: SATP = 1/mean_sedentary_duration, ASTP = 1/mean_active_duration
    double SATP_wanigatunga = (!ISNA(mean_sedentary_duration) && mean_sedentary_duration > 0) ?
        1.0 / mean_sedentary_duration : NA_REAL;
    double ASTP_wanigatunga = (!ISNA(mean_active_duration) && mean_active_duration > 0) ?
        1.0 / mean_active_duration : NA_REAL;

    // Calculate Markov transition probabilities (secondary)
    int total_from_sedentary = sedentary_to_sedentary + sedentary_to_active;
    int total_from_active = active_to_active + active_to_sedentary;

    double SATP_markov = (total_from_sedentary > 0) ?
        static_cast<double>(sedentary_to_active) / total_from_sedentary : NA_REAL;
    double ASTP_markov = (total_from_active > 0) ?
        static_cast<double>(active_to_sedentary) / total_from_active : NA_REAL;

    // Calculate fragmentation index using Wanigatunga method (matches R)
    double fragmentation = NA_REAL;
    if (!ISNA(SATP_wanigatunga) && !ISNA(ASTP_wanigatunga)) {
        fragmentation = (SATP_wanigatunga + ASTP_wanigatunga) / 2.0;
    }

    return Rcpp::List::create(
        // Primary outputs (Wanigatunga method - matches R)
        Named("SATP") = SATP_wanigatunga,
        Named("ASTP") = ASTP_wanigatunga,
        Named("fragmentation_index") = fragmentation,
        Named("mean_sedentary_bout") = mean_sedentary_duration,
        Named("mean_active_bout") = mean_active_duration,
        Named("n_sedentary_bouts") = static_cast<int>(sedentary_bout_durations.size()),
        Named("n_active_bouts") = static_cast<int>(active_bout_durations.size()),
        // Secondary outputs (Markov method)
        Named("SATP_markov") = SATP_markov,
        Named("ASTP_markov") = ASTP_markov,
        Named("sedentary_to_active") = sedentary_to_active,
        Named("sedentary_to_sedentary") = sedentary_to_sedentary,
        Named("active_to_sedentary") = active_to_sedentary,
        Named("active_to_active") = active_to_active
    );
}


// Calculate W50 (weighted median bout duration, Chastin method)
// W50 = the bout duration at which 50% of TOTAL TIME is accumulated
// Reference: Chastin SFM, Granat MH (2010). Gait & Posture, 31(1):82-86.
// [[Rcpp::export]]
double calculate_w50_cpp(IntegerVector bout_lengths) {
    int n = bout_lengths.size();
    if (n == 0) return NA_REAL;

    // Total time in sedentary
    double total_time = 0.0;
    for (int i = 0; i < n; ++i) {
        total_time += bout_lengths[i];
    }

    if (total_time == 0) return NA_REAL;

    // Sort bouts by duration DESCENDING (longest first) per Chastin 2010
    std::vector<int> sorted_lengths(bout_lengths.begin(), bout_lengths.end());
    std::sort(sorted_lengths.begin(), sorted_lengths.end(), std::greater<int>());

    // Find duration at which 50% of sedentary time is accumulated
    // Use double for target to avoid integer division issues
    double cumsum = 0.0;
    double target = total_time / 2.0;  // Use double division for precision

    for (int len : sorted_lengths) {
        cumsum += len;
        if (cumsum >= target) {
            return static_cast<double>(len);
        }
    }

    return static_cast<double>(sorted_lengths.back());
}


// Calculate Gini coefficient for bout lengths
// With finite-sample bias correction per GGIR/ineq package methodology
// Reference: Chastin & Granat (2010), ineq::Gini(corr = TRUE)
// [[Rcpp::export]]
double calculate_gini_cpp(IntegerVector bout_lengths) {
    int n = bout_lengths.size();
    if (n <= 1) return 0.0;

    // Sort in ASCENDING order (required for standard Gini formula)
    std::vector<double> sorted_lengths(bout_lengths.begin(), bout_lengths.end());
    std::sort(sorted_lengths.begin(), sorted_lengths.end());

    // Calculate Gini using formula: G = [2*sum(i*x_i) - (n+1)*sum(x)] / [n*sum(x)]
    // Where i is 1-indexed (1, 2, 3, ..., n)
    double sum_ix = 0.0;
    double sum_x = 0.0;
    for (int i = 0; i < n; ++i) {
        sum_ix += (i + 1) * sorted_lengths[i];  // i+1 for 1-indexed
        sum_x += sorted_lengths[i];
    }

    if (sum_x == 0) return 0.0;

    double gini = (2.0 * sum_ix - (n + 1.0) * sum_x) / (n * sum_x);

    // Apply finite-sample bias correction: G_corrected = G * n / (n - 1)
    // This matches GGIR's use of ineq::Gini(corr = TRUE)
    if (n > 1) {
        gini = gini * n / (n - 1.0);
    }

    return std::max(0.0, std::min(1.0, gini));
}


// Calculate power-law alpha (robust estimation)
// [[Rcpp::export]]
Rcpp::List calculate_alpha_robust_cpp(IntegerVector bout_lengths,
                                       int xmin = 1) {
    int n = bout_lengths.size();
    if (n < 10) {
        return Rcpp::List::create(
            Named("alpha") = NA_REAL,
            Named("xmin") = NA_INTEGER,
            Named("n_tail") = NA_INTEGER
        );
    }

    // Filter bouts >= xmin
    std::vector<double> lengths;
    for (int i = 0; i < n; ++i) {
        if (bout_lengths[i] >= xmin) {
            lengths.push_back(static_cast<double>(bout_lengths[i]));
        }
    }

    int n_tail = lengths.size();
    if (n_tail < 5) {
        return Rcpp::List::create(
            Named("alpha") = NA_REAL,
            Named("xmin") = xmin,
            Named("n_tail") = n_tail
        );
    }

    // MLE estimate: alpha = 1 + n / sum(ln(x / xmin))
    double sum_log = 0.0;
    for (double x : lengths) {
        sum_log += std::log(x / (xmin - 0.5));  // Continuous correction
    }

    double alpha = 1.0 + n_tail / sum_log;

    return Rcpp::List::create(
        Named("alpha") = alpha,
        Named("xmin") = xmin,
        Named("n_tail") = n_tail
    );
}


// Estimate optimal xmin using Clauset method
// [[Rcpp::export]]
Rcpp::List estimate_xmin_cpp(IntegerVector bout_lengths) {
    int n = bout_lengths.size();
    if (n < 20) {
        return Rcpp::List::create(
            Named("xmin") = 1,
            Named("alpha") = NA_REAL,
            Named("ks_stat") = NA_REAL
        );
    }

    // Sort ascending (smallest first) - xmin candidates should be smallest values
    // per Clauset et al. (2009) "Power-law distributions in empirical data"
    std::vector<int> sorted_lengths(bout_lengths.begin(), bout_lengths.end());
    std::sort(sorted_lengths.begin(), sorted_lengths.end());  // Ascending order

    // Test different xmin values - use smallest unique values as candidates
    // (xmin is the minimum value above which power-law behavior holds)
    std::vector<int> xmin_candidates;
    for (int i = 0; i < std::min(n / 4, 50); ++i) {
        int val = sorted_lengths[i];
        if (xmin_candidates.empty() || val != xmin_candidates.back()) {
            xmin_candidates.push_back(val);
        }
    }

    double best_ks = 1.0;
    int best_xmin = 1;
    double best_alpha = 2.0;

    for (int xmin : xmin_candidates) {
        // Calculate alpha for this xmin
        std::vector<double> tail;
        for (int x : sorted_lengths) {
            if (x >= xmin) {
                tail.push_back(static_cast<double>(x));
            }
        }

        int n_tail = tail.size();
        if (n_tail < 10) continue;

        // MLE alpha
        double sum_log = 0.0;
        for (double x : tail) {
            sum_log += std::log(x / (xmin - 0.5));
        }
        double alpha = 1.0 + n_tail / sum_log;

        // Calculate KS statistic
        // Empirical CDF
        std::vector<double> ecdf(n_tail);
        for (int i = 0; i < n_tail; ++i) {
            ecdf[i] = static_cast<double>(i + 1) / n_tail;
        }

        // Theoretical CDF (power-law)
        double max_diff = 0.0;
        for (int i = 0; i < n_tail; ++i) {
            double x = tail[i];
            double tcdf = 1.0 - std::pow(xmin / x, alpha - 1);
            double diff = std::abs(ecdf[i] - tcdf);
            if (diff > max_diff) {
                max_diff = diff;
            }
        }

        if (max_diff < best_ks) {
            best_ks = max_diff;
            best_xmin = xmin;
            best_alpha = alpha;
        }
    }

    return Rcpp::List::create(
        Named("xmin") = best_xmin,
        Named("alpha") = best_alpha,
        Named("ks_stat") = best_ks
    );
}


// Comprehensive Sedentary Fragmentation Analysis

// All-in-one fragmentation metrics
// [[Rcpp::export]]
Rcpp::List sedentary_fragmentation_all_cpp(NumericVector counts,
                                            int sedentary_threshold = 100,
                                            IntegerVector wear = IntegerVector()) {
    // Detect bouts
    Rcpp::List bouts = detect_sedentary_bouts_cpp(counts, sedentary_threshold, 1, wear);
    IntegerVector bout_lengths = bouts["bout_length"];

    // Transition probabilities
    Rcpp::List transitions = calculate_transition_probabilities_cpp(
        counts, sedentary_threshold, wear);

    // W50
    double w50 = calculate_w50_cpp(bout_lengths);

    // Gini
    double gini = calculate_gini_cpp(bout_lengths);

    // Alpha with optimal xmin
    Rcpp::List alpha_result = estimate_xmin_cpp(bout_lengths);

    // Breaks per sedentary hour
    int n_bouts = bouts["n_bouts"];
    int total_sed = bouts["total_sedentary_epochs"];
    double breaks_per_sed_hour = (total_sed > 0) ?
        (static_cast<double>(n_bouts) / total_sed * 60.0) : 0.0;

    // Prolonged sedentary (30+ and 60+ minutes)
    int prolonged_30 = 0;
    int prolonged_60 = 0;
    for (int i = 0; i < bout_lengths.size(); ++i) {
        if (bout_lengths[i] >= 30) prolonged_30 += bout_lengths[i];
        if (bout_lengths[i] >= 60) prolonged_60 += bout_lengths[i];
    }
    double percent_prolonged_30 = (total_sed > 0) ?
        (100.0 * prolonged_30 / total_sed) : 0.0;
    double percent_prolonged_60 = (total_sed > 0) ?
        (100.0 * prolonged_60 / total_sed) : 0.0;

    return Rcpp::List::create(
        // Bout statistics
        Named("n_bouts") = n_bouts,
        Named("total_sedentary_min") = total_sed,
        Named("mean_bout_duration") = bouts["mean_bout_duration"],
        Named("median_bout_duration") = bouts["median_bout_duration"],

        // Transition probabilities
        Named("SATP") = transitions["SATP"],
        Named("ASTP") = transitions["ASTP"],

        // Advanced metrics
        Named("W50") = w50,
        Named("gini") = gini,
        Named("alpha") = alpha_result["alpha"],
        Named("alpha_xmin") = alpha_result["xmin"],

        // Derived metrics
        Named("breaks_per_sed_hour") = breaks_per_sed_hour,
        Named("percent_prolonged_30min") = percent_prolonged_30,
        Named("percent_prolonged_60min") = percent_prolonged_60
    );
}


// Activity Bout Summary by Day

// Calculate bout metrics for each day
// [[Rcpp::export]]
Rcpp::DataFrame bout_summary_by_day_cpp(NumericVector counts,
                                         IntegerVector day_indices,
                                         int sedentary_threshold = 100,
                                         int moderate_threshold = 1952) {
    // Get unique days
    std::vector<int> unique_days;
    for (int i = 0; i < day_indices.size(); ++i) {
        if (unique_days.empty() || day_indices[i] != unique_days.back()) {
            unique_days.push_back(day_indices[i]);
        }
    }

    int n_days = unique_days.size();

    std::vector<int> days(n_days);
    std::vector<int> n_sed_bouts(n_days);
    std::vector<int> n_mvpa_bouts(n_days);
    std::vector<double> mean_sed_bout(n_days);
    std::vector<double> total_mvpa(n_days);
    std::vector<double> total_sed(n_days);

    for (int d = 0; d < n_days; ++d) {
        int day = unique_days[d];
        days[d] = day;

        // Extract counts for this day
        NumericVector day_counts;
        for (int i = 0; i < counts.size(); ++i) {
            if (day_indices[i] == day) {
                day_counts.push_back(counts[i]);
            }
        }

        // Detect sedentary bouts
        Rcpp::List sed_bouts = detect_sedentary_bouts_cpp(
            day_counts, sedentary_threshold, 1, IntegerVector());
        n_sed_bouts[d] = sed_bouts["n_bouts"];
        mean_sed_bout[d] = sed_bouts["mean_bout_duration"];
        total_sed[d] = sed_bouts["total_sedentary_epochs"];

        // Detect MVPA bouts
        Rcpp::List mvpa_bouts = detect_mvpa_bouts_cpp(
            day_counts, moderate_threshold, 10, 2, false);
        n_mvpa_bouts[d] = mvpa_bouts["n_bouts"];
        total_mvpa[d] = mvpa_bouts["total_mvpa_epochs"];
    }

    return Rcpp::DataFrame::create(
        Named("day") = days,
        Named("n_sedentary_bouts") = n_sed_bouts,
        Named("mean_sedentary_bout_duration") = mean_sed_bout,
        Named("total_sedentary_min") = total_sed,
        Named("n_mvpa_bouts") = n_mvpa_bouts,
        Named("total_mvpa_min") = total_mvpa
    );
}
