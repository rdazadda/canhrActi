#' C++ Computational Backend
#'
#' High-performance C++ functions for accelerometer data processing.
#' Provides 10-50x speedup for large datasets.
#'
#' @name cpp-interface
#' @keywords internal
NULL

#' @useDynLib canhrActi, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# Circadian Rhythm

#' Calculate L5/M10 Circadian Metrics
#'
#' Finds least active 5 hours (L5) and most active 10 hours (M10)
#' using van Someren (1999) average-profile method.
#'
#' @param minute_data Numeric vector of minute-level activity
#' @param window_L5 L5 window in minutes (default: 300)
#' @param window_M10 M10 window in minutes (default: 600)
#' @param start_minute Start minute of day (0-1439)
#' @return List with L5_value, L5_onset, M10_value, M10_onset, RA
#' @export
L5M10_cpp <- function(minute_data, window_L5 = 300L, window_M10 = 600L, start_minute = 0L) {
  calculate_L5_M10_cpp(as.numeric(minute_data), as.integer(window_L5),
                       as.integer(window_M10), as.integer(start_minute))
}

#' Interdaily Stability (IS)
#' @param hourly_data Numeric vector of hourly activity
#' @param hours_per_day Hours per day (default: 24)
#' @return IS value (0-1, higher = more stable)
#' @export
IS_cpp <- function(hourly_data, hours_per_day = 24L) {
  calculate_IS_cpp(as.numeric(hourly_data), as.integer(hours_per_day))
}

#' Intradaily Variability (IV)
#' @param hourly_data Numeric vector of hourly activity
#' @return IV value (lower = less fragmented)
#' @export
IV_cpp <- function(hourly_data) {
  calculate_IV_cpp(as.numeric(hourly_data))
}

#' Complete Circadian Analysis
#' @param minute_data Numeric vector of minute-level activity
#' @param hours_per_day Hours per day (default: 24)
#' @param start_minute Start minute of day (0-1439)
#' @return List with L5, M10, RA, IS, IV, phi
#' @export
circadian_cpp <- function(minute_data, hours_per_day = 24L, start_minute = 0L) {
  calculate_all_circadian_cpp(as.numeric(minute_data), as.integer(hours_per_day),
                              as.integer(start_minute))
}

# Wear Time Detection

#' Troiano Wear Time Algorithm
#' @param counts Activity counts (1-min epochs)
#' @param window_minutes Non-wear window (default: 60)
#' @param spike_tolerance Allowed spike minutes (default: 2)
#' @param spike_max_count Max spike count (default: 100)
#' @return Integer vector: 1=wear, 0=non-wear
#' @references Troiano RP, et al. (2008). MSSE, 40(1), 181-188.
#' @export
wear_troiano_cpp_wrapper <- function(counts, window_minutes = 60L,
                                     spike_tolerance = 2L, spike_max_count = 100L) {
  wear_troiano_cpp(as.numeric(counts), as.integer(window_minutes),
                   as.integer(spike_tolerance), as.integer(spike_max_count))
}

#' Choi Wear Time Algorithm
#' @param counts Activity counts
#' @param window_minutes Non-wear window (default: 90)
#' @param spike_tolerance Spike tolerance (default: 2)
#' @param spike_max_count Max spike count (default: 100)
#' @param upstream_minutes Upstream validation (default: 30)
#' @param downstream_minutes Downstream validation (default: 30)
#' @return Integer vector: 1=wear, 0=non-wear
#' @references Choi L, et al. (2011). MSSE, 43(2), 357-364.
#' @export
wear_choi_cpp_wrapper <- function(counts, window_minutes = 90L, spike_tolerance = 2L,
                                  spike_max_count = 100L, upstream_minutes = 30L,
                                  downstream_minutes = 30L) {
  wear_choi_cpp(as.numeric(counts), as.integer(window_minutes),
                as.integer(spike_tolerance), as.integer(spike_max_count),
                as.integer(upstream_minutes), as.integer(downstream_minutes))
}

#' CANHR2025 Wear Time Algorithm
#' @param counts Activity counts
#' @param window_minutes Non-wear window (default: 120)
#' @param spike_tolerance Spike tolerance (default: 3)
#' @param spike_max_count Max spike count (default: 150)
#' @param upstream_minutes Upstream validation (default: 45)
#' @param downstream_minutes Downstream validation (default: 45)
#' @return Integer vector: 1=wear, 0=non-wear
#' @export
wear_canhr2025_cpp_wrapper <- function(counts, window_minutes = 120L, spike_tolerance = 3L,
                                       spike_max_count = 150L, upstream_minutes = 45L,
                                       downstream_minutes = 45L) {
  wear_canhr2025_cpp(as.numeric(counts), as.integer(window_minutes),
                     as.integer(spike_tolerance), as.integer(spike_max_count),
                     as.integer(upstream_minutes), as.integer(downstream_minutes))
}

# Sleep Scoring

#' Cole-Kripke Sleep Algorithm
#' @param counts Activity counts (1-min epochs)
#' @param threshold Discrimination threshold (default: 1.0)
#' @param apply_rescoring Apply Webster's rescoring? (default: TRUE)
#' @return Integer vector: 1=sleep, 0=wake
#' @references Cole RJ, et al. (1992). Sleep, 15(5), 461-469.
#' @export
sleep_cole_kripke_cpp_wrapper <- function(counts, threshold = 1.0, apply_rescoring = TRUE) {
  cole_kripke_cpp(as.numeric(counts), as.numeric(threshold), as.logical(apply_rescoring))
}

#' Sadeh Sleep Algorithm
#' @param counts Activity counts
#' @param threshold Probability threshold (default: -4.0)
#' @return Integer vector: 1=sleep, 0=wake
#' @references Sadeh A, et al. (1994). Sleep, 17(3), 201-207.
#' @export
sleep_sadeh_cpp_wrapper <- function(counts, threshold = -4.0) {
  sadeh_cpp(as.numeric(counts), as.numeric(threshold))
}

#' Tudor-Locke Sleep Algorithm
#' @param counts Activity counts
#' @param sleep_threshold Count threshold (default: 20)
#' @param min_sleep_block Minimum sleep block epochs (default: 160)
#' @param max_wake_block Max wake gap to fill (default: 60)
#' @return Integer vector: 1=sleep, 0=wake
#' @export
sleep_tudor_locke_cpp_wrapper <- function(counts, sleep_threshold = 20L,
                                          min_sleep_block = 160L, max_wake_block = 60L) {
  tudor_locke_cpp(as.numeric(counts), as.integer(sleep_threshold),
                  as.integer(min_sleep_block), as.integer(max_wake_block))
}

#' Sleep Quality Metrics
#' @param sleep Sleep scores (1=sleep)
#' @param sleep_onset Sleep onset index (1-indexed)
#' @param wake_time Wake time index (1-indexed)
#' @return List with TST, efficiency, SOL, WASO, awakenings, fragmentation
#' @export
sleep_metrics_cpp <- function(sleep, sleep_onset, wake_time) {
  calculate_sleep_metrics_cpp(as.integer(sleep), as.integer(sleep_onset),
                              as.integer(wake_time))
}

#' Sleep Regularity Index (SRI)
#' @param sleep Sleep scores
#' @param epochs_per_day Epochs per day (default: 1440)
#' @return SRI value (-100 to 100)
#' @references Phillips AJK, et al. (2017). Scientific Reports.
#' @export
sri_cpp <- function(sleep, epochs_per_day = 1440L) {
  calculate_sri_vector_cpp(as.integer(sleep), as.integer(epochs_per_day))
}

# Bout Detection

#' Detect MVPA Bouts
#' @param counts Activity counts
#' @param moderate_threshold MVPA threshold (default: 1952)
#' @param min_bout_length Min bout epochs (default: 10)
#' @param drop_time Allowed drop epochs (default: 2)
#' @param use_80_percent_rule Use 80% rule? (default: FALSE)
#' @return List with bout details and statistics
#' @export
mvpa_bouts_cpp <- function(counts, moderate_threshold = 1952L, min_bout_length = 10L,
                           drop_time = 2L, use_80_percent_rule = FALSE) {
  detect_mvpa_bouts_cpp(as.numeric(counts), as.integer(moderate_threshold),
                        as.integer(min_bout_length), as.integer(drop_time),
                        as.logical(use_80_percent_rule))
}

#' Detect Sedentary Bouts
#' @param counts Activity counts
#' @param sedentary_threshold Sedentary threshold (default: 100)
#' @param min_bout_length Min bout length (default: 1)
#' @param wear Wear time vector (optional)
#' @return List with bout details and statistics
#' @export
sedentary_bouts_cpp <- function(counts, sedentary_threshold = 100L,
                                min_bout_length = 1L, wear = integer()) {
  detect_sedentary_bouts_cpp(as.numeric(counts), as.integer(sedentary_threshold),
                             as.integer(min_bout_length), as.integer(wear))
}

#' Sedentary Fragmentation Metrics
#' @param counts Activity counts
#' @param sedentary_threshold Sedentary threshold (default: 100)
#' @param wear Wear time vector (optional)
#' @return List with ASTP, SATP, W50, Gini, alpha
#' @references Chastin SF, Granat MH (2010). Gait & Posture.
#' @export
fragmentation_cpp <- function(counts, sedentary_threshold = 100L, wear = integer()) {
  sedentary_fragmentation_all_cpp(as.numeric(counts), as.integer(sedentary_threshold),
                                  as.integer(wear))
}

# Rolling Statistics

#' Rolling Mean
#' @param x Numeric vector
#' @param window Window size
#' @return Rolling means
#' @export
rolling_mean <- function(x, window) {
  rolling_mean_cpp(as.numeric(x), as.integer(window))
}

#' Rolling Standard Deviation
#' @param x Numeric vector
#' @param window Window size
#' @return Rolling SDs
#' @export
rolling_sd <- function(x, window) {
  rolling_sd_cpp(as.numeric(x), as.integer(window))
}

#' Rolling Sum
#' @param x Numeric vector
#' @param window Window size
#' @return Rolling sums
#' @export
rolling_sum <- function(x, window) {
  rolling_sum_cpp(as.numeric(x), as.integer(window))
}

# Backend Utilities

#' Check C++ Availability
#' @return Logical
#' @export
cpp_available <- function() {

  tryCatch({
    length(rolling_mean_cpp(c(1, 2, 3, 4, 5), 2L)) > 0
  }, error = function(e) FALSE)
}

#' Backend Information
#' @return Invisible list with backend info
#' @export
backend_info <- function() {
  cpp_ok <- cpp_available()
  message("canhrActi C++ backend: ", if (cpp_ok) "Available (10-50x speedup)" else "Not available")
  if (!cpp_ok) {
    message("Install Rcpp and RcppArmadillo, then reinstall canhrActi to enable.")
  }
  invisible(list(cpp_available = cpp_ok))
}
