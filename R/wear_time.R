#' Base Wear Time Detection Function
#'
#' Internal function implementing the core wear time detection logic.
#' Used by Troiano, Choi, and CANHR2025 algorithms.
#'
#' @param counts_per_minute Numeric vector of counts per minute
#' @param non_wear_window Window length in minutes (epochs)
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
  if (n.minutes == 0) return(logical(0))
  if (n.minutes < non_wear_window) return(rep(TRUE, n.minutes))

  wear.time <- rep(TRUE, n.minutes)

  # Handle NA values - treat as zeros

  counts_per_minute[is.na(counts_per_minute)] <- 0

  # OPTIMIZED: Pre-compute key indicators using vectorized operations
  is_zero <- counts_per_minute == 0
  is_spike <- counts_per_minute > 0 & counts_per_minute <= spike_stoplevel
  is_above_stop <- counts_per_minute > spike_stoplevel

  # Pre-compute cumulative sums for fast window calculations
  zero_cumsum <- c(0, cumsum(is_zero))
  above_cumsum <- c(0, cumsum(is_above_stop))

  # Find windows with enough zeros and no values above stoplevel
  # Using vectorized rolling sums
  n_zeros_in_window <- zero_cumsum[(non_wear_window + 1):(n.minutes + 1)] - zero_cumsum[1:(n.minutes - non_wear_window + 1)]
  n_above_in_window <- above_cumsum[(non_wear_window + 1):(n.minutes + 1)] - above_cumsum[1:(n.minutes - non_wear_window + 1)]

  # Candidate windows: enough zeros, no above-stoplevel values
  min_zeros_required <- non_wear_window - spike_tolerance
  candidate_starts <- which(n_zeros_in_window >= min_zeros_required & n_above_in_window == 0)

  if (length(candidate_starts) == 0) return(wear.time)

  # Process candidates (still need some iteration but much fewer)
  i <- 1L
  while (i <= length(candidate_starts)) {
    window.start <- candidate_starts[i]
    window.end <- window.start + non_wear_window - 1L

    # Check consecutive spike constraint using RLE on window
    window_spikes <- is_spike[window.start:window.end]
    rle_spikes <- rle(window_spikes)
    max_consec_spikes <- if (any(rle_spikes$values)) max(rle_spikes$lengths[rle_spikes$values]) else 0L

    if (max_consec_spikes > spike_tolerance) {
      i <- i + 1L
      next
    }

    # For Choi/CANHR2025: validate spikes with upstream/downstream
    if (validate_spikes && any(window_spikes)) {
      spike_positions <- which(window_spikes)
      valid_nonwear <- TRUE

      for (sp_idx in spike_positions) {
        actual_pos <- window.start + sp_idx - 1L

        # Check upstream zeros
        up_start <- max(1L, actual_pos - min_window_len)
        up_end <- actual_pos - 1L
        has_upstream <- up_end >= up_start && all(counts_per_minute[up_start:up_end] == 0)

        # Check downstream zeros
        down_start <- actual_pos + 1L
        down_end <- min(n.minutes, actual_pos + min_window_len)
        has_downstream <- down_start <= down_end && all(counts_per_minute[down_start:down_end] == 0)

        if (!has_upstream || !has_downstream) {
          valid_nonwear <- FALSE
          break
        }
      }

      if (!valid_nonwear) {
        i <- i + 1L
        next
      }
    }

    # Valid non-wear window found - mark and extend
    wear.time[window.start:window.end] <- FALSE

    # Extend non-wear period using vectorized approach
    extend_pos <- window.end + 1L
    while (extend_pos <= n.minutes) {
      if (is_zero[extend_pos]) {
        wear.time[extend_pos] <- FALSE
        extend_pos <- extend_pos + 1L
      } else if (is_spike[extend_pos]) {
        # Find consecutive spike length
        spike_start <- extend_pos
        while (extend_pos <= n.minutes && is_spike[extend_pos]) {
          extend_pos <- extend_pos + 1L
        }
        spike_len <- extend_pos - spike_start

        if (spike_len <= spike_tolerance) {
          wear.time[spike_start:(extend_pos - 1L)] <- FALSE
        } else {
          break
        }
      } else {
        break
      }
    }

    # Skip to candidates after this non-wear period
    i <- which(candidate_starts > extend_pos)[1]
    if (is.na(i)) break
  }

  return(wear.time)
}

#' Wear Time Detection Using Troiano Algorithm
#'
#' Troiano et al. (2008) algorithm for detecting non-wear time in accelerometer data.
#' Non-wear is defined as 60+ consecutive minutes of zero counts, allowing for up to
#' 2 consecutive minutes of counts between 1-100 (spikes).
#'
#' @param counts_per_minute Numeric vector of activity counts (one value per epoch)
#' @param non_wear_window Integer. Consecutive zero minutes for non-wear detection (default: 60)
#' @param spike_tolerance Integer. Maximum consecutive minutes with activity allowed within non-wear (default: 2)
#' @param spike_stoplevel Integer. Maximum count value considered as spike (default: 100)
#' @return Logical vector where TRUE indicates wear time, FALSE indicates non-wear
#' @export
#' @references Troiano RP, et al. (2008). Physical activity in the United States measured
#'   by accelerometer. Medicine and Science in Sports and Exercise, 40(1), 181-188.
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

#' Wear Time Detection Using Choi Algorithm
#'
#' Choi et al. (2011) algorithm with upstream/downstream window validation.
#' The Choi algorithm extends Troiano by requiring that any spike of activity
#' within a non-wear period must be surrounded by consecutive zero-count windows
#' of at least 30 minutes on BOTH sides (upstream AND downstream).
#'
#' @param counts_per_minute Numeric vector of activity counts (one value per epoch)
#' @param non_wear_window Integer. Window length in minutes for initial non-wear detection (default: 90)
#' @param spike_tolerance Integer. Maximum consecutive minutes with activity allowed (default: 2)
#' @param min_spike_length Integer. Minimum length of spike (default: 1, reserved for future use)
#' @param spike_stoplevel Integer. Maximum count value for spike (default: 100)
#' @param min_window_len Integer. Required consecutive zeros before/after spike (default: 30)
#'
#' @return Logical vector where TRUE indicates wear time, FALSE indicates non-wear
#' @export
#' @references Choi L, et al. (2011). Validation of accelerometer wear and nonwear time
#'   classification algorithm. Medicine and Science in Sports and Exercise, 43(2), 357-364.
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

#' Wear Time Detection Using CANHR 2025 Algorithm
#'
#' CANHR 2025 algorithm with upstream/downstream window validation.
#' Based on Choi algorithm with parameters optimized for Alaska Native populations
#' and cold climate activity patterns.
#'
#' @param counts_per_minute Numeric vector of activity counts (one value per epoch)
#' @param non_wear_window Integer. Window length in minutes (default: 120)
#' @param spike_tolerance Integer. Maximum consecutive minutes with activity allowed (default: 3)
#' @param min_spike_length Integer. Minimum length of spike (default: 1, reserved for future use)
#' @param spike_stoplevel Integer. Maximum count value for spike (default: 150)
#' @param min_window_len Integer. Upstream/downstream window length in minutes (default: 45)
#'
#' @return Logical vector where TRUE indicates wear time, FALSE indicates non-wear
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


#' Extract Wear Time Periods from Boolean Vector
#'
#' Converts a boolean wear time vector into a data frame of continuous
#' wear periods with start and end timestamps.
#'
#' @param wear_time Logical vector where TRUE indicates wear time
#' @param timestamps POSIXct vector of timestamps corresponding to each epoch
#' @param epoch_length Numeric. Length of each epoch in seconds (default: 60)
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{period} - Period number (1, 2, 3, ...)
#'     \item \code{start_time} - POSIXct start timestamp of wear period
#'     \item \code{end_time} - POSIXct end timestamp of wear period
#'     \item \code{duration_minutes} - Duration of the period in minutes
#'     \item \code{start_idx} - Starting epoch index
#'     \item \code{end_idx} - Ending epoch index
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get wear periods from analysis results
#' wear_vector <- wear.choi(counts.data$axis1)
#' periods <- get.wear.periods(wear_vector, counts.data$timestamp)
#' print(periods)
#' }
get.wear.periods <- function(wear_time, timestamps, epoch_length = 60) {

  if (length(wear_time) != length(timestamps)) {
    stop("wear_time and timestamps must have the same length")
  }

  # Handle NA values in wear_time - treat as non-wear (FALSE)
  if (any(is.na(wear_time))) {
    wear_time <- ifelse(is.na(wear_time), FALSE, wear_time)
  }

  if (length(wear_time) == 0) {
    return(data.frame(
      period = integer(0),
      start_time = as.POSIXct(character(0)),
      end_time = as.POSIXct(character(0)),
      duration_minutes = numeric(0),
      start_idx = integer(0),
      end_idx = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Use run-length encoding to find consecutive TRUE values
  rle_result <- rle(wear_time)

  # Calculate cumulative positions
  cum_lengths <- cumsum(rle_result$lengths)
  start_positions <- c(1L, cum_lengths[-length(cum_lengths)] + 1L)

  # Filter to only wear periods (TRUE values)
  wear_mask <- rle_result$values == TRUE
  wear_starts <- start_positions[wear_mask]
  wear_ends <- cum_lengths[wear_mask]

  if (length(wear_starts) == 0) {
    return(data.frame(
      period = integer(0),
      start_time = as.POSIXct(character(0)),
      end_time = as.POSIXct(character(0)),
      duration_minutes = numeric(0),
      start_idx = integer(0),
      end_idx = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Build result data frame
  periods <- data.frame(
    period = seq_along(wear_starts),
    start_time = timestamps[wear_starts],
    end_time = timestamps[wear_ends],
    duration_minutes = (wear_ends - wear_starts + 1) * (epoch_length / 60),
    start_idx = wear_starts,
    end_idx = wear_ends,
    stringsAsFactors = FALSE
  )

  return(periods)
}
