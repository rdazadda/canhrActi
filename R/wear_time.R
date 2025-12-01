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
  wear.time <- rep(TRUE, n.minutes)


  # Handle NA values - treat as zeros (no activity detected)
  # This prevents errors and is consistent with how missing data is typically handled
  if (any(is.na(counts_per_minute))) {
    counts_per_minute <- ifelse(is.na(counts_per_minute), 0, counts_per_minute)
  }

  i <- 1L
  while (i <= n.minutes) {
    if (i + non_wear_window - 1L > n.minutes) break

    window.start <- i
    window.end <- i + non_wear_window - 1L
    window <- counts_per_minute[window.start:window.end]

    is.zero <- window == 0
    is.spike <- window > 0 & window <= spike_stoplevel
    has.above.stoplevel <- any(window > spike_stoplevel)
    n.zeros <- sum(is.zero)

    # For Troiano (simple algorithm)
    if (!validate_spikes) {
      # Count CONSECUTIVE spikes using run-length encoding
      rle.result <- rle(is.spike)
      max.consecutive.spikes <- 0
      if (any(rle.result$values)) {
        max.consecutive.spikes <- max(rle.result$lengths[rle.result$values])
      }

      # Check: enough zeros AND no consecutive spike run exceeds tolerance
      if (!has.above.stoplevel && n.zeros >= (non_wear_window - spike_tolerance) && max.consecutive.spikes <= spike_tolerance) {
        # Mark initial window as non-wear
        wear.time[window.start:window.end] <- FALSE

        # EXTEND non-wear period beyond the window
        extend.pos <- window.end + 1L
        while (extend.pos <= n.minutes) {
          val <- counts_per_minute[extend.pos]
          if (val == 0) {
            # Zero count - extend non-wear
            wear.time[extend.pos] <- FALSE
            extend.pos <- extend.pos + 1L
          } else if (val <= spike_stoplevel) {
            # Potential spike - check if it's within tolerance
            spike.start <- extend.pos
            spike.len <- 0L
            while (extend.pos <= n.minutes &&
                   counts_per_minute[extend.pos] > 0 &&
                   counts_per_minute[extend.pos] <= spike_stoplevel) {
              spike.len <- spike.len + 1L
              extend.pos <- extend.pos + 1L
            }
            if (spike.len <= spike_tolerance) {
              # Valid spike within tolerance - mark as non-wear
              wear.time[spike.start:(spike.start + spike.len - 1L)] <- FALSE
            } else {
              # Spike too long - end non-wear period
              # Revert the spike minutes back to wear
              wear.time[spike.start:(spike.start + spike.len - 1L)] <- TRUE
              break
            }
          } else {
            # Activity above spike level - end non-wear period
            break
          }
        }
        i <- extend.pos
      } else {
        i <- i + 1L
      }
      next
    }

    # For Choi and CANHR2025 (with spike validation)
    rle.spike <- rle(is.spike)
    spike.lengths <- rle.spike$lengths[rle.spike$values]

    # Check minimum zeros requirement
    if (n.zeros < (non_wear_window - spike_tolerance)) {
      i <- i + 1L
      next
    }

    # Check no consecutive spike exceeds tolerance
    if (length(spike.lengths) > 0 && any(spike.lengths > spike_tolerance)) {
      i <- i + 1L
      next
    }

    valid.nonwear <- TRUE

    # Validate spikes with upstream/downstream zero windows
    if (length(spike.lengths) > 0) {
      spike.positions <- which(is.spike)

      for (spike.idx in spike.positions) {
        actual.pos <- window.start + spike.idx - 1L

        # Check upstream (allow shorter window at data boundaries)
        upstream.start <- max(1L, actual.pos - min_window_len)
        upstream.end <- actual.pos - 1L
        has.upstream <- FALSE

        if (upstream.end >= upstream.start && upstream.end >= 1L) {
          upstream.window <- counts_per_minute[upstream.start:upstream.end]
          # Accept if we have at least some zeros (relaxed for boundaries)
          min.required <- min(min_window_len, length(upstream.window))
          if (length(upstream.window) >= min.required && all(upstream.window == 0)) {
            has.upstream <- TRUE
          }
        }

        # Check downstream (allow shorter window at data boundaries)
        downstream.start <- actual.pos + 1L
        downstream.end <- min(n.minutes, actual.pos + min_window_len)
        has.downstream <- FALSE

        if (downstream.start <= downstream.end && downstream.start <= n.minutes) {
          downstream.window <- counts_per_minute[downstream.start:downstream.end]
          min.required <- min(min_window_len, length(downstream.window))
          if (length(downstream.window) >= min.required && all(downstream.window == 0)) {
            has.downstream <- TRUE
          }
        }

        # Spike is valid if it has zeros on at least one side
        if (!has.upstream && !has.downstream) {
          valid.nonwear <- FALSE
          break
        }
      }
    }

    if (valid.nonwear) {
      # Mark initial window as non-wear
      wear.time[window.start:window.end] <- FALSE

      # EXTEND non-wear period beyond the window (Choi extension)
      extend.pos <- window.end + 1L
      while (extend.pos <= n.minutes) {
        val <- counts_per_minute[extend.pos]
        if (val == 0) {
          # Zero count - extend non-wear
          wear.time[extend.pos] <- FALSE
          extend.pos <- extend.pos + 1L
        } else if (val <= spike_stoplevel) {
          # Potential spike - check consecutive length and upstream/downstream
          spike.start <- extend.pos
          spike.len <- 0L
          while (extend.pos <= n.minutes &&
                 counts_per_minute[extend.pos] > 0 &&
                 counts_per_minute[extend.pos] <= spike_stoplevel) {
            spike.len <- spike.len + 1L
            extend.pos <- extend.pos + 1L
          }

          if (spike.len <= spike_tolerance) {
            # Valid spike within tolerance - mark as non-wear and continue
            # (Same as Troiano - upstream/downstream validation only applies to initial detection)
            wear.time[spike.start:(spike.start + spike.len - 1L)] <- FALSE
          } else {
            # Spike too long - end non-wear period
            # Revert the spike minutes back to wear
            wear.time[spike.start:(spike.start + spike.len - 1L)] <- TRUE
            break
          }
        } else {
          # Activity above spike level - end non-wear period
          break
        }
      }
      i <- extend.pos
    } else {
      i <- i + 1L
    }
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
#' of at least 30 minutes on at least one side.
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
