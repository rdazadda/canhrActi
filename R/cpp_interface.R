#' C++ Computational Backend
#'
#' High-performance C++ helpers for the circadian-rhythm calculations
#' (L5/M10, IS, IV) used by \code{\link{circadian.rhythm}}, plus rolling-window
#' statistics. The wear-time, sleep-scoring, and sedentary-fragmentation
#' algorithms are implemented in pure R; only these circadian/rolling helpers
#' are dispatched to C++.
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
  message("canhrActi C++ backend: ", if (cpp_ok) "available" else "not available")
  if (!cpp_ok) {
    message("Install Rcpp, then reinstall canhrActi to enable the C++ helpers.")
  }
  invisible(list(cpp_available = cpp_ok))
}
