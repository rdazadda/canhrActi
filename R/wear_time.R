#' Base Wear Time Detection Function
#'
#' Internal function implementing the core wear time detection logic.
#' Used by Troiano, Choi, and CANHR2025 algorithms.
#'
#' @param counts_per_minute Numeric vector of counts per minute
#' @param non_wear_window Window length in minutes
#' @param spike_tolerance Maximum consecutive minutes with activity allowed
#' @param spike_stoplevel Maximum count value for spike
#' @param validate_spikes Logical. If TRUE, use upstream/downstream validation (Choi/CANHR2025)
#' @param min_window_len Upstream/downstream window length (for Choi/CANHR2025)
#' @return Logical vector where TRUE indicates wear time
#' @keywords internal
.detect_wear_time_base <- function(counts_per_minute,
                                   non_wear_window,
                                   spike_tolerance,
                                   spike_stoplevel,
                                   validate_spikes = FALSE,
                                   min_window_len = 30) {

  n.minutes <- length(counts_per_minute)
  wear.time <- rep(TRUE, n.minutes)

  i <- 1L
  while (i <= n.minutes) {
    if (i + non_wear_window - 1L > n.minutes) break

    window.start <- i
    window.end <- i + non_wear_window - 1L
    window <- counts_per_minute[window.start:window.end]

    is.zero <- window == 0
    is.spike <- window > 0 & window <= spike_stoplevel
    n.zeros <- sum(is.zero)

    # For Troiano (simple algorithm)
    if (!validate_spikes) {
      n.spikes <- sum(is.spike)
      if (n.zeros >= (non_wear_window - spike_tolerance) && n.spikes <= spike_tolerance) {
        wear.time[window.start:window.end] <- FALSE
        i <- window.end + 1L
      } else {
        i <- i + 1L
      }
      next
    }

    # For Choi and CANHR2025 (with spike validation)
    rle.spike <- rle(is.spike)
    spike.lengths <- rle.spike$lengths[rle.spike$values]

    if (n.zeros < (non_wear_window - spike_tolerance)) {
      i <- i + 1L
      next
    }

    if (length(spike.lengths) > 0 && any(spike.lengths > spike_tolerance)) {
      i <- i + 1L
      next
    }

    valid.nonwear <- TRUE

    if (length(spike.lengths) > 0) {
      spike.positions <- which(is.spike)

      for (spike.idx in spike.positions) {
        actual.pos <- window.start + spike.idx - 1L

        # Check upstream
        upstream.start <- max(1L, actual.pos - min_window_len)
        upstream.end <- actual.pos - 1L
        has.upstream <- FALSE

        if (upstream.end >= upstream.start && upstream.end >= 1L) {
          upstream.window <- counts_per_minute[upstream.start:upstream.end]
          if (length(upstream.window) >= min_window_len && all(upstream.window == 0)) {
            has.upstream <- TRUE
          }
        }

        # Check downstream
        downstream.start <- actual.pos + 1L
        downstream.end <- min(n.minutes, actual.pos + min_window_len)
        has.downstream <- FALSE

        if (downstream.start <= downstream.end && downstream.start <= n.minutes) {
          downstream.window <- counts_per_minute[downstream.start:downstream.end]
          if (length(downstream.window) >= min_window_len && all(downstream.window == 0)) {
            has.downstream <- TRUE
          }
        }

        if (!has.upstream && !has.downstream) {
          valid.nonwear <- FALSE
          break
        }
      }
    }

    if (valid.nonwear) {
      wear.time[window.start:window.end] <- FALSE
      i <- window.end + 1L
    } else {
      i <- i + 1L
    }
  }

  return(wear.time)
}

#' Wear Time (Troiano)
#'
#' @param counts_per_minute Counts per minute
#' @param non_wear_window Consecutive zero minutes for non-wear (default: 60)
#' @param spike_tolerance Allowed spike minutes in non-wear (default: 2)
#' @param spike_stoplevel Maximum CPM for spikes (default: 100)
#' @return Logical vector (TRUE = wear time)
#' @export
wear.troiano <- function(counts_per_minute,
                         non_wear_window = 60,
                         spike_tolerance = 2,
                         spike_stoplevel = 100) {

  .detect_wear_time_base(counts_per_minute,
                         non_wear_window,
                         spike_tolerance,
                         spike_stoplevel,
                         validate_spikes = FALSE)
}

#' Detect Wear Time Using Choi Algorithm
#'
#' Choi et al. (2011) algorithm with upstream/downstream window validation.
#' The Choi algorithm extends Troiano by requiring that any spike of activity
#' within a non-wear period must be surrounded by consecutive zero-count windows.
#'
#' @param counts_per_minute Numeric vector of counts per minute
#' @param non_wear_window Integer. Window length in minutes (default: 90)
#' @param spike_tolerance Integer. Maximum consecutive minutes with activity allowed (default: 2)
#' @param min_spike_length Integer. Minimum length of spike (default: 1, not used in current implementation)
#' @param spike_stoplevel Integer. Maximum count value for spike (default: 100)
#' @param min_window_len Integer. Upstream/downstream window length in minutes (default: 30)
#'
#' @return Logical vector where TRUE indicates wear time
#' @export
wear.choi <- function(counts_per_minute,
                      non_wear_window = 90,
                      spike_tolerance = 2,
                      min_spike_length = 1,
                      spike_stoplevel = 100,
                      min_window_len = 30) {

  .detect_wear_time_base(counts_per_minute,
                         non_wear_window,
                         spike_tolerance,
                         spike_stoplevel,
                         validate_spikes = TRUE,
                         min_window_len = min_window_len)
}

#' Detect Wear Time Using CANHR 2025 Algorithm
#'
#' CANHR 2025 algorithm with upstream/downstream window validation.
#' The CANHR 2025 algorithm extends Choi by using custom parameters
#' optimized for specific study populations.
#'
#' @param counts_per_minute Numeric vector of counts per minute
#' @param non_wear_window Integer. Window length in minutes (default: 120)
#' @param spike_tolerance Integer. Maximum consecutive minutes with activity allowed (default: 3)
#' @param min_spike_length Integer. Minimum length of spike (default: 1, not used in current implementation)
#' @param spike_stoplevel Integer. Maximum count value for spike (default: 150)
#' @param min_window_len Integer. Upstream/downstream window length in minutes (default: 45)
#'
#' @return Logical vector where TRUE indicates wear time
#' @export
wear.CANHR2025 <- function(counts_per_minute,
                           non_wear_window = 120,
                           spike_tolerance = 3,
                           min_spike_length = 1,
                           spike_stoplevel = 150,
                           min_window_len = 45) {

  .detect_wear_time_base(counts_per_minute,
                         non_wear_window,
                         spike_tolerance,
                         spike_stoplevel,
                         validate_spikes = TRUE,
                         min_window_len = min_window_len)
}
