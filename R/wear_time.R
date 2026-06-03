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
#' @param max_spike_minutes Integer. Total number of spike (1-stoplevel) minutes permitted
#'   within a candidate non-wear window. This is the TOTAL spike-minute budget and is kept
#'   distinct from \code{spike_tolerance}, which caps the longest CONSECUTIVE spike run.
#'   Defaults to \code{spike_tolerance} to preserve historical behavior.
#' @return Logical vector where TRUE indicates wear time
#' @keywords internal
#'
#' @section NA handling:
#' Missing counts (\code{NA}) are imputed to 0 (treated as measured zero counts) before
#' detection. This is a deliberate imputation choice, not part of Troiano (2008) or Choi
#' (2011). Because zeros contribute to zero-windows, this biases detection TOWARD non-wear:
#' missing epochs can create or extend non-wear bouts. Callers that need missingness to be
#' methodologically distinct from a measured zero should pre-handle NA before calling.
.detect_wear_time_base <- function(counts_per_minute,
                                   non_wear_window,
                                   spike_tolerance,
                                   spike_stoplevel,
                                   validate_spikes = FALSE,
                                   min_window_len = 30,
                                   max_spike_minutes = spike_tolerance) {

  n.minutes <- length(counts_per_minute)
  if (n.minutes == 0) return(logical(0))
  if (n.minutes < non_wear_window) return(rep(TRUE, n.minutes))

  wear.time <- rep(TRUE, n.minutes)

  # NA handling (deliberate imputation choice, documented in the function's
  # @section NA handling): missing counts are imputed to 0. Neither Troiano (2008)
  # nor Choi (2011) specifies this; treating NA as a measured zero biases detection
  # TOWARD non-wear, because imputed zeros contribute to zero-windows and can create
  # or extend non-wear bouts. Callers needing missingness handled distinctly from a
  # measured zero should pre-process NA before calling this function.
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

  # Candidate windows: enough zeros, no above-stoplevel values.
  # Two DISTINCT spike notions are enforced separately:
  #   - max_spike_minutes: TOTAL spike-minute budget in the window (used here to set the
  #     minimum required zeros). Because n_above_in_window == 0, every non-zero minute in a
  #     candidate window is a spike, so allowing up to max_spike_minutes spikes is equivalent
  #     to requiring (non_wear_window - max_spike_minutes) zeros.
  #   - spike_tolerance: longest CONSECUTIVE spike run permitted (enforced below via RLE).
  min_zeros_required <- non_wear_window - max_spike_minutes
  candidate_starts <- which(n_zeros_in_window >= min_zeros_required & n_above_in_window == 0)

  if (length(candidate_starts) == 0) return(wear.time)

  # Choi/CANHR2025 spike rule: a spike at position `pos` is a valid part of a non-wear
  # period only if flanked by min_window_len consecutive zeros on BOTH sides. Shared by the
  # initial-window validation and the extension loop so both branches use the identical rule.
  .spike_has_flanking_zeros <- function(pos) {
    up_start <- max(1L, pos - min_window_len)
    up_end <- pos - 1L
    has_upstream <- up_end >= up_start && all(counts_per_minute[up_start:up_end] == 0)

    down_start <- pos + 1L
    down_end <- min(n.minutes, pos + min_window_len)
    has_downstream <- down_start <= down_end && all(counts_per_minute[down_start:down_end] == 0)

    has_upstream && has_downstream
  }

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

        if (!.spike_has_flanking_zeros(actual_pos)) {
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

        # For Choi/CANHR2025: the extended portion must obey the SAME upstream/downstream
        # zero-window rule as the initial window, not just the consecutive-run cap. Every
        # spike epoch in this run must be flanked by min_window_len zeros on both sides;
        # otherwise the extension stops here (the spike marks real wear).
        spikes_valid <- spike_len <= spike_tolerance
        if (spikes_valid && validate_spikes) {
          for (sp_pos in spike_start:(extend_pos - 1L)) {
            if (!.spike_has_flanking_zeros(sp_pos)) {
              spikes_valid <- FALSE
              break
            }
          }
        }

        if (spikes_valid) {
          wear.time[spike_start:(extend_pos - 1L)] <- FALSE
        } else {
          # Roll back to the start of the failing spike run so it is treated as wear.
          extend_pos <- spike_start
          break
        }
      } else {
        break
      }
    }

    # Advance to the next candidate that begins OUTSIDE the region just marked as non-wear.
    # The marked region is [window.start, extend_pos - 1L]; extend_pos is the first epoch NOT
    # marked (the epoch that broke extension, or n.minutes + 1L if extension reached the end).
    # Using >= extend_pos (rather than > extend_pos) ensures a legitimate later bout whose
    # start coincides with extend_pos is not skipped.
    i <- which(candidate_starts >= extend_pos)[1]
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
#' @param epoch_length Integer. Epoch length in seconds (default: 60). Window parameters
#'   are automatically scaled to epochs based on this value.
#' @return Logical vector where TRUE indicates wear time, FALSE indicates non-wear
#' @export
#' @references Troiano RP, et al. (2008). Physical activity in the United States measured
#'   by accelerometer. Medicine and Science in Sports and Exercise, 40(1), 181-188.
wear.troiano <- function(counts_per_minute,
                         non_wear_window = 60,
                         spike_tolerance = 2,
                         spike_stoplevel = 100,
                         epoch_length = 60) {

  epochs_per_minute <- 60 / epoch_length
  window_epochs <- ceiling(non_wear_window * epochs_per_minute)
  spike_epochs <- ceiling(spike_tolerance * epochs_per_minute)

  .detect_wear_time_base(counts_per_minute,
                         window_epochs,
                         spike_epochs,
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
#' @param min_window_len Integer. Required consecutive zeros before/after spike in minutes (default: 30)
#' @param epoch_length Integer. Epoch length in seconds (default: 60). Window parameters
#'   are automatically scaled to epochs based on this value.
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
                      min_window_len = 30,
                      epoch_length = 60) {

  epochs_per_minute <- 60 / epoch_length
  window_epochs <- ceiling(non_wear_window * epochs_per_minute)
  spike_epochs <- ceiling(spike_tolerance * epochs_per_minute)
  validation_epochs <- ceiling(min_window_len * epochs_per_minute)

  .detect_wear_time_base(counts_per_minute,
                         window_epochs,
                         spike_epochs,
                         spike_stoplevel,
                         validate_spikes = TRUE,
                         min_window_len = validation_epochs)
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
#' @param epoch_length Integer. Epoch length in seconds (default: 60). Window parameters
#'   are automatically scaled to epochs based on this value.
#'
#' @return Logical vector where TRUE indicates wear time, FALSE indicates non-wear
#' @export
wear.CANHR2025 <- function(counts_per_minute,
                           non_wear_window = 120,
                           spike_tolerance = 3,
                           min_spike_length = 1,
                           spike_stoplevel = 150,
                           min_window_len = 45,
                           epoch_length = 60) {

  epochs_per_minute <- 60 / epoch_length
  window_epochs <- ceiling(non_wear_window * epochs_per_minute)
  spike_epochs <- ceiling(spike_tolerance * epochs_per_minute)
  validation_epochs <- ceiling(min_window_len * epochs_per_minute)

  .detect_wear_time_base(counts_per_minute,
                         window_epochs,
                         spike_epochs,
                         spike_stoplevel,
                         validate_spikes = TRUE,
                         min_window_len = validation_epochs)
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
