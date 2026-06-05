#' Cole-Kripke Sleep Scoring Algorithm
#'
#' Implements the Cole-Kripke sleep/wake scoring algorithm for adults (ages 35-65).
#'
#' @param counts Numeric vector of activity counts (from axis1)
#' @param apply_rescoring Logical. Apply Webster's rescoring rules? (default: TRUE)
#' @param epoch_length Epoch length in seconds (default: 60). The Cole-Kripke algorithm
#'   was validated on 1-minute epochs. Using other epoch lengths will generate a warning
#'   as the scoring may not be accurate without revalidation.
#'
#' @return Character vector of sleep states: "S" (sleep) or "W" (wake)
#'
#' @details
#' Uses a 7-epoch window: 4 epochs before, current, 2 epochs after, with weighted
#' coefficients:
#' D = 0.001 * (106*P4 + 54*P3 + 58*P2 + 76*P1 + 230*C + 74*N1 + 67*N2)
#'
#' Counts are divided by 100 and capped at 300.
#' Classification: D < 1 = Sleep, D >= 1 = Wake
#'
#' Webster's Rescoring (if enabled):
#' 1. After >= 4 min wake, next 1 min sleep -> wake
#' 2. After >= 10 min wake, next 3 min sleep -> wake
#' 3. After >= 15 min wake, next 4 min sleep -> wake
#' 4. <= 6 min sleep surrounded by >= 15 min wake -> wake
#' 5. <= 10 min sleep surrounded by >= 20 min wake -> wake
#'
#' @references
#' Cole RJ, Kripke DF, Gruen W, Mullaney DJ, Gillin JC (1992).
#' Automatic sleep/wake identification from wrist activity.
#' Sleep, 15(5):461-469.
#'
#' ActiGraph documentation:
#' https://actigraphcorp.my.site.com/support/s/article/Where-can-I-find-documentation-for-the-Sadeh-and-Cole-Kripke-algorithms
#'
#' @examples
#' \dontrun{
#' agd.data <- read.agd("participant.agd")
#' counts.data <- agd.counts(agd.data)
#' sleep.wake <- sleep.cole.kripke(counts.data$axis1)
#' table(sleep.wake)
#' }
#'
#' @export
sleep.cole.kripke <- function(counts, apply_rescoring = TRUE, epoch_length = 60) {
  counts <- validate_counts(counts)

  if (!is.null(epoch_length) && !is.na(epoch_length) && epoch_length != 60) {
    warning("Cole-Kripke was validated for 60-second epochs. epoch_length = ", epoch_length, "s.")
  }

  # Scale counts (divide by 100, cap at 300)
  scaled.counts <- counts / 100
  scaled.counts[scaled.counts > 300] <- 300

  n <- length(scaled.counts)

  # Calculate sleep index D for each epoch using vectorized convolution
  # D = 0.001 * (106*P4 + 54*P3 + 58*P2 + 76*P1 + 230*C + 74*N1 + 67*N2)
  # Window: P4(-4), P3(-3), P2(-2), P1(-1), C(0), N1(+1), N2(+2)
  # This is a 7-epoch window: 4 epochs before, current, 2 epochs after
  #
  # For stats::filter with sides=2 (centered), coefficients are applied symmetrically
  # But Cole-Kripke is asymmetric (2 before, 4 after), so we use manual convolution

  # Coefficients in temporal order: P4, P3, P2, P1, C, N1, N2 (for convolution)
  # Original Cole-Kripke (1992): D = 0.001 * (106*A4 + 54*A3 + 58*A2 + 76*A1 + 230*A0 + 74*A1' + 67*A2')
  # Reference: Cole et al., Sleep 15(5):461-469, 1992, Table 2
  coef <- c(106, 54, 58, 76, 230, 74, 67) / 1000  # P4=0.106, P3=0.054, ..., N2=0.067

  # Pad: need 4 before (for P4...P1) and 2 after (for N1, N2)
  padded.counts <- c(rep(0, 4), scaled.counts, rep(0, 2))

  # Manual convolution for proper Cole-Kripke alignment
  # For each epoch i, calculate: sum(coef * counts[(i-4):(i+2)])
  # i.e. 4 epochs before, current, 2 epochs after
  sleep.index <- numeric(n)
  for (i in seq_len(n)) {
    idx <- (i + 4 - 4):(i + 4 + 2)  # 7-epoch window in padded array
    sleep.index[i] <- sum(coef * padded.counts[idx])
  }

  # Classify sleep/wake based on threshold
  # D < 1 = Sleep (S), D >= 1 = Wake (W)
  sleep.state <- ifelse(sleep.index < 1, "S", "W")

  # Apply Webster's rescoring rules (if requested)
  if (apply_rescoring) {
    sleep.state <- .apply.webster.rescoring(sleep.state)
  }

  return(sleep.state)
}


.apply.webster.rescoring <- function(sleep.state) {
  # Webster's Rescoring Rules for Cole-Kripke algorithm
  # Reference: Webster JB et al. (1982). Psychophysiology. 19(6):682-687
  #
  # Optimized using run-length encoding for ~10x speedup

  n <- length(sleep.state)
  if (n == 0) return(sleep.state)

  rescored <- sleep.state

  # Use run-length encoding for efficient bout detection
  rle.result <- rle(sleep.state)
  bout.lengths <- rle.result$lengths
  bout.values <- rle.result$values
  n.bouts <- length(bout.lengths)

  # Calculate cumulative positions for each bout
  bout.ends <- cumsum(bout.lengths)
  bout.starts <- c(1, bout.ends[-n.bouts] + 1)

  # Rules 1-3: After wake period of certain length, rescore following sleep
  for (b in seq_len(n.bouts)) {
    if (bout.values[b] == "W" && b < n.bouts && bout.values[b + 1] == "S") {
      wake.len <- bout.lengths[b]
      sleep.len <- bout.lengths[b + 1]
      sleep.start <- bout.starts[b + 1]

      # Rule 1: After >= 4 min wake, next 1 min sleep -> wake
      if (wake.len >= 4) {
        rescored[sleep.start] <- "W"
      }

      # Rule 2: After >= 10 min wake, next 3 min sleep -> wake
      if (wake.len >= 10 && sleep.len >= 3) {
        rescored[sleep.start:min(sleep.start + 2, n)] <- "W"
      }

      # Rule 3: After >= 15 min wake, next 4 min sleep -> wake
      if (wake.len >= 15 && sleep.len >= 4) {
        rescored[sleep.start:min(sleep.start + 3, n)] <- "W"
      }
    }
  }

  # Rules 4-5: Sleep bouts surrounded by long wake periods
  for (b in seq_len(n.bouts)) {
    if (bout.values[b] == "S") {
      sleep.len <- bout.lengths[b]
      sleep.start <- bout.starts[b]
      sleep.end <- bout.ends[b]

      # Get wake length before (previous bout if it's wake)
      wake.before <- if (b > 1 && bout.values[b - 1] == "W") bout.lengths[b - 1] else 0

      # Get wake length after (next bout if it's wake)
      wake.after <- if (b < n.bouts && bout.values[b + 1] == "W") bout.lengths[b + 1] else 0

      # Rule 4: <= 6 min sleep surrounded by >= 15 min wake -> wake
      if (sleep.len <= 6 && wake.before >= 15 && wake.after >= 15) {
        rescored[sleep.start:sleep.end] <- "W"
      }

      # Rule 5: <= 10 min sleep surrounded by >= 20 min wake -> wake
      if (sleep.len <= 10 && wake.before >= 20 && wake.after >= 20) {
        rescored[sleep.start:sleep.end] <- "W"
      }
    }
  }

  return(rescored)
}


#' Sadeh Sleep Scoring Algorithm
#'
#' Implements the Sadeh sleep/wake scoring algorithm for children/adolescents (ages 10-25).
#'
#' @param counts Numeric vector of activity counts (from axis1)
#' @param epoch_length Epoch length in seconds (default: 60). The Sadeh algorithm
#'   was validated on 60-second epochs. Using other epoch lengths will generate a warning.
#'
#' @return Character vector of sleep states: "S" (sleep) or "W" (wake)
#'
#' @details
#' Uses 11-epoch window (5 previous + current + 5 next):
#' PS = 7.601 - (0.065*AVG) - (1.08*NATS) - (0.056*SD) - (0.703*LG)
#'
#' AVG: Mean of 11-epoch window
#' NATS: Count of epochs with 50 <= counts < 100
#' SD: Standard deviation of current + 5 previous epochs
#' LG: ln(count + 1)
#'
#' Counts capped at 300. Classification: SI > -4 = Sleep, SI <= -4 = Wake
#'
#' Note: This threshold matches the validated actigraph.sleepr implementation.
#' The original Sadeh (1994) paper used >= 0, but ActiGraph implementations
#' use > -4 which has been validated against polysomnography.
#'
#' @references
#' Sadeh A, Sharkey KM, Carskadon MA (1994).
#' Activity-based sleep-wake identification: an empirical test of methodological issues.
#' Sleep, 17(3):201-207.
#'
#' @examples
#' \dontrun{
#' agd.data <- read.agd("participant.agd")
#' counts.data <- agd.counts(agd.data)
#' sleep.wake <- sleep.sadeh(counts.data$axis1)
#' table(sleep.wake)
#' }
#'
#' @export
sleep.sadeh <- function(counts, epoch_length = 60) {
  counts <- validate_counts(counts)

  if (!is.null(epoch_length) && !is.na(epoch_length) && epoch_length != 60) {
    warning("Sadeh was validated for 60-second epochs. epoch_length = ", epoch_length, "s.")
  }

  # Cap counts at 300
  capped.counts <- counts
  capped.counts[capped.counts > 300] <- 300

  n <- length(capped.counts)

  # Pad the vector with zeros for boundary epochs
  padded.counts <- c(rep(0, 5), capped.counts, rep(0, 5))

  # Vectorized calculation of sleep index components

  # AVG: Rolling mean of 11-epoch window using fast convolution
  avg.filter <- rep(1/11, 11)
  AVG.padded <- stats::filter(padded.counts, avg.filter, sides = 2)
  AVG <- as.numeric(AVG.padded[6:(5 + n)])

  # NATS: Count of epochs with 50 <= counts < 100 in 11-epoch window
  nat.indicator <- as.numeric(padded.counts >= 50 & padded.counts < 100)
  nat.filter <- rep(1, 11)
  NATS.padded <- stats::filter(nat.indicator, nat.filter, sides = 2)
  NATS <- as.numeric(NATS.padded[6:(5 + n)])

  # SD: Rolling standard deviation of 6 epochs (current + 5 previous)
  # Vectorized using rolling sum of squares formula
  # SD = sqrt((sum(x^2) - (sum(x))^2/n) / (n-1))
  sum.filter.6 <- rep(1, 6)
  sumsq.padded <- stats::filter(padded.counts^2, sum.filter.6, sides = 1)
  sum.padded <- stats::filter(padded.counts, sum.filter.6, sides = 1)

  # Extract the causal 6-epoch filter aligned at the current epoch, so the
  # window covers the current epoch + 5 PRECEDING epochs (Sadeh 1994).
  # Original epoch i sits at padded index 5 + i; the sides = 1 filter there
  # sums padded indices i:(5 + i), i.e. original epochs (i-5):i.
  sumsq <- as.numeric(sumsq.padded[6:(5 + n)])
  sums <- as.numeric(sum.padded[6:(5 + n)])

  # Calculate variance: (sum_sq - sum^2/n) / (n-1)
  variance <- (sumsq - (sums^2) / 6) / 5
  variance[variance < 0] <- 0  # Handle numerical precision issues
  SD <- sqrt(variance)
  SD[is.na(SD)] <- 0

  # LG: Natural logarithm of (current epoch + 1)
  LG <- log(capped.counts + 1)

  # Calculate sleep index (vectorized)
  # SI = 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)
  sleep.index <- 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)

  # Classify sleep/wake based on threshold
  # Threshold: SI > -4 = Sleep (S), SI <= -4 = Wake (W)
  # Note: This matches the validated actigraph.sleepr implementation
  sleep.state <- ifelse(sleep.index > -4, "S", "W")

  return(sleep.state)
}


#' Tudor-Locke Sleep Period Detection
#'
#' Detects sleep periods and calculates sleep quality metrics.
#'
#' @param sleep.state Character vector of sleep states ("S" or "W")
#' @param timestamps POSIXct vector of epoch timestamps
#' @param counts Optional numeric vector of activity counts for total counts
#' @param bedtime_start Consecutive sleep minutes to define bedtime (default: 5)
#' @param wake_time_end Consecutive wake minutes to define wake time (default: 10)
#' @param min_sleep_period Minimum sleep period in minutes (default: 160)
#' @param max_sleep_period Maximum sleep period in minutes (default: 1440)
#' @param min_nonzero_epochs Minimum non-zero activity epochs (default: 15)
#' @param epoch_length Epoch length in seconds (default: 60). Parameters are
#'   automatically scaled to epochs based on this value.
#' @param filter_suspicious Logical. Apply an optional post-detection heuristic
#'   filter that drops periods likely reflecting device removal or sustained
#'   sedentary behavior rather than sleep? (default: FALSE). These heuristics are
#'   NOT part of the Tudor-Locke (2014) algorithm and can remove physiologically
#'   plausible sleep (e.g. consolidated high-efficiency nights, naps), so they are
#'   opt-in only. When enabled, the count of removed periods is surfaced via a
#'   warning.
#'
#' @return Data frame with sleep periods and metrics (TST, SE, WASO, awakenings, etc.)
#'
#' @details
#' The core detection follows Tudor-Locke (2014). The \code{filter_suspicious}
#' option is an additional, non-Tudor-Locke heuristic pass (disabled by default).
#' When enabled, a period is removed if it matches ANY of the following criteria:
#' \enumerate{
#'   \item sleep_time > 720 min, sleep_efficiency > 99\%, and 0 awakenings;
#'   \item sleep_time > 120 min and mean activity < 5 counts/min;
#'   \item sleep_time > 180 min, sleep_efficiency >= 99.5\%, and 0 awakenings;
#'   \item 30 < sleep_time < 180 min, sleep_efficiency > 98\%, and 0 awakenings;
#'   \item total_counts < 50 and sleep_time > 60 min;
#'   \item activity CV < 0.5, sleep_efficiency > 95\%, < 2 awakenings, and
#'         60 < sleep_time < 300 min;
#'   \item in-bed start between 09:00-17:00, sleep_time < 180 min, and
#'         sleep_efficiency > 90\% (daytime short consolidated period).
#' }
#' Because several of these can discard real sleep, prefer leaving
#' \code{filter_suspicious = FALSE} and inspecting periods directly.
#'
#' @references
#' Tudor-Locke C, Barreira TV, Schuna JM, Mire EF, Katzmarzyk PT (2014).
#' Fully automated waist-worn accelerometer algorithm for detecting children's
#' sleep-period time separate from 24-h physical activity or sedentary behaviors.
#' Applied Physiology, Nutrition, and Metabolism, 39(1):53-57.
#'
#' @examples
#' \dontrun{
#' agd.data <- read.agd("participant.agd")
#' counts.data <- agd.counts(agd.data)
#' sleep.state <- sleep.cole.kripke(counts.data$axis1)
#' sleep.periods <- sleep.tudor.locke(sleep.state, counts.data$timestamp)
#' }
#'
#' @export
sleep.tudor.locke <- function(sleep.state,
                              timestamps,
                              counts = NULL,
                              bedtime_start = 5,
                              wake_time_end = 10,
                              min_sleep_period = 160,
                              max_sleep_period = 1440,
                              min_nonzero_epochs = 15,
                              epoch_length = 60,
                              filter_suspicious = FALSE) {

  if (length(sleep.state) != length(timestamps)) {
    stop("Length of sleep.state (", length(sleep.state),
         ") must equal length of timestamps (", length(timestamps), ")")
  }

  if (length(sleep.state) == 0) {
    warning("Empty sleep.state vector. Returning empty results.")
    return(.empty.sleep.periods.df())
  }

  if (!is.null(counts) && length(counts) != length(sleep.state)) {
    stop("Length of counts (", length(counts),
         ") must equal length of sleep.state (", length(sleep.state), ")")
  }

  # Scale time-based parameters from minutes to epochs
  epochs_per_minute <- 60 / epoch_length
  bedtime_epochs <- ceiling(bedtime_start * epochs_per_minute)
  wake_epochs <- ceiling(wake_time_end * epochs_per_minute)
  min_period_epochs <- ceiling(min_sleep_period * epochs_per_minute)
  max_period_epochs <- ceiling(max_sleep_period * epochs_per_minute)

  sleep.periods <- .detect.sleep.periods(
    sleep.state = sleep.state,
    timestamps = timestamps,
    counts = counts,
    bedtime_start = bedtime_epochs,
    wake_time_end = wake_epochs,
    min_sleep_period = min_period_epochs,
    max_sleep_period = max_period_epochs,
    min_nonzero_epochs = min_nonzero_epochs,
    epoch_length = epoch_length,
    filter_suspicious = filter_suspicious
  )

  return(sleep.periods)
}


# Helper function to return empty sleep periods data frame
.empty.sleep.periods.df <- function() {
  data.frame(
    period_number = integer(0),
    in_bed_time = character(0),
    out_bed_time = character(0),
    onset = character(0),
    sleep_time = numeric(0),
    wake_time = numeric(0),
    number_of_awakenings = integer(0),
    average_awakening = numeric(0),
    movement_index = numeric(0),
    fragmentation_index = numeric(0),
    sleep_efficiency = numeric(0),
    total_counts = numeric(0),
    activity_sd = numeric(0),
    activity_cv = numeric(0),
    stringsAsFactors = FALSE
  )
}


.detect.sleep.periods <- function(sleep.state, timestamps, counts,
                                  bedtime_start, wake_time_end,
                                  min_sleep_period, max_sleep_period,
                                  min_nonzero_epochs,
                                  epoch_length = 60,
                                  filter_suspicious = FALSE) {

  n <- length(sleep.state)
  periods <- list()
  period.count <- 0

  if (n < bedtime_start + wake_time_end) {
    return(.empty.sleep.periods.df())
  }

  # OPTIMIZED: Use run-length encoding for fast consecutive detection
  # Find all positions where bedtime_start consecutive S epochs start
  is_sleep <- sleep.state == "S"
  is_wake <- sleep.state == "W"

  # Rolling sum to find consecutive sleep epochs >= bedtime_start
  # Using cumsum for O(n) complexity instead of O(n*bedtime_start)
  sleep_cumsum <- c(0, cumsum(is_sleep))
  sleep_run <- sleep_cumsum[(bedtime_start + 1):(n + 1)] - sleep_cumsum[1:(n - bedtime_start + 1)]
  potential_bedtimes <- which(sleep_run == bedtime_start)

  # Rolling sum to find consecutive wake epochs >= wake_time_end
  wake_cumsum <- c(0, cumsum(is_wake))
  wake_run <- wake_cumsum[(wake_time_end + 1):(n + 1)] - wake_cumsum[1:(n - wake_time_end + 1)]
  potential_wakes <- which(wake_run == wake_time_end)

  if (length(potential_bedtimes) == 0 || length(potential_wakes) == 0) {
    return(.empty.sleep.periods.df())
  }

  # Process potential sleep periods
  i <- 1
  while (i <= length(potential_bedtimes)) {
    bedtime.idx <- potential_bedtimes[i]

    # Find first valid wake after this bedtime (must be after bedtime + bedtime_start)
    min_wake_idx <- bedtime.idx + bedtime_start
    valid_wakes <- potential_wakes[potential_wakes >= min_wake_idx]

    if (length(valid_wakes) == 0) break

    wake.idx <- valid_wakes[1]
    period.duration <- wake.idx - bedtime.idx

    # Check duration criteria
    if (period.duration >= min_sleep_period && period.duration < max_sleep_period) {
      period.sleep.state <- sleep.state[bedtime.idx:(wake.idx - 1)]

      # Count epochs with non-zero activity (validates device was worn)
      if (!is.null(counts)) {
        period.counts <- counts[bedtime.idx:(wake.idx - 1)]
        nonzero.count <- sum(period.counts > 0, na.rm = TRUE)
      } else {
        nonzero.count <- min_nonzero_epochs
      }

      if (nonzero.count >= min_nonzero_epochs) {
        period.count <- period.count + 1

        metrics <- .calculate.sleep.metrics(
          sleep.state = period.sleep.state,
          start.idx = bedtime.idx,
          end.idx = wake.idx - 1,
          timestamps = timestamps,
          counts = counts
        )

        periods[[period.count]] <- metrics

        # Skip to bedtimes after this wake period ends
        i <- which(potential_bedtimes > wake.idx)[1]
        if (is.na(i)) break
        next
      }
    }

    i <- i + 1
  }

  # Combine all periods into a data frame
  if (period.count == 0) {
    return(data.frame(
      period_number = integer(0),
      in_bed_time = character(0),
      out_bed_time = character(0),
      onset = character(0),
      sleep_time = numeric(0),
      wake_time = numeric(0),
      number_of_awakenings = integer(0),
      average_awakening = numeric(0),
      movement_index = numeric(0),
      fragmentation_index = numeric(0),
      sleep_efficiency = numeric(0),
      total_counts = numeric(0),
      activity_sd = numeric(0),
      activity_cv = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, lapply(1:period.count, function(i) {
    df <- periods[[i]]
    df$period_number <- i
    df
  }))

  # Reorder columns
  result <- result[, c("period_number", "in_bed_time", "out_bed_time", "onset",
                       "sleep_time", "wake_time", "number_of_awakenings",
                       "average_awakening", "movement_index", "fragmentation_index",
                       "sleep_efficiency", "total_counts", "activity_sd", "activity_cv")]

  if (filter_suspicious && nrow(result) > 0) {
    avg_counts_per_min <- result$total_counts / pmax(result$sleep_time, 1)
    avg_counts_per_min[is.na(avg_counts_per_min) | is.infinite(avg_counts_per_min)] <- 0

    suspicious1 <- result$sleep_time > 720 &
                   result$sleep_efficiency > 99 &
                   result$number_of_awakenings == 0

    suspicious2 <- result$sleep_time > 120 & avg_counts_per_min < 5

    suspicious3 <- result$sleep_time > 180 &
                   result$sleep_efficiency >= 99.5 &
                   result$number_of_awakenings == 0

    suspicious4 <- result$sleep_time < 180 &
                   result$sleep_time > 30 &
                   result$sleep_efficiency > 98 &
                   result$number_of_awakenings == 0

    suspicious5 <- result$total_counts < 50 & result$sleep_time > 60

    suspicious6 <- result$activity_cv < 0.5 &
                   result$sleep_efficiency > 95 &
                   result$number_of_awakenings < 2 &
                   result$sleep_time > 60 &
                   result$sleep_time < 300

    suspicious7 <- tryCatch({
      in_bed_hours <- as.numeric(format(as.POSIXct(result$in_bed_time), "%H"))
      daytime_start <- in_bed_hours >= 9 & in_bed_hours <= 17
      short_duration <- result$sleep_time < 180
      high_efficiency <- result$sleep_efficiency > 90
      daytime_start & short_duration & high_efficiency
    }, error = function(e) rep(FALSE, nrow(result)))

    suspicious <- suspicious1 | suspicious2 | suspicious3 | suspicious4 | suspicious5 | suspicious6 | suspicious7

    if (any(suspicious)) {
      warning("Removed ", sum(suspicious), " suspicious sleep period(s) ",
              "(likely device removal or sedentary behavior, not actual sleep)")
      result <- result[!suspicious, , drop = FALSE]
      if (nrow(result) > 0) {
        result$period_number <- 1:nrow(result)
      }
    }
  }

  return(result)
}


.calculate.sleep.metrics <- function(sleep.state, start.idx, end.idx,
                                     timestamps, counts) {
  period.timestamps <- timestamps[start.idx:end.idx]
  in.bed.time <- period.timestamps[1]
  out.bed.time <- period.timestamps[length(period.timestamps)]

  first.sleep <- which(sleep.state == "S")[1]
  if (is.na(first.sleep)) first.sleep <- 1
  onset <- period.timestamps[first.sleep]

  sleep.minutes <- sum(sleep.state == "S")
  wake.minutes <- sum(sleep.state == "W")
  total.minutes <- length(sleep.state)

  # Vectorized awakening calculation using run-length encoding
  wake_runs <- rle(sleep.state == "W")
  awakenings <- sum(wake_runs$values)  # Count of wake bouts
  awakening.lengths <- wake_runs$lengths[wake_runs$values]  # Lengths of wake bouts
  average.awakening <- if (length(awakening.lengths) > 0) mean(awakening.lengths) else 0
  sleep.efficiency <- 100 * sleep.minutes / total.minutes

  # Activity metrics for validation
  total.counts <- 0
  activity.sd <- 0
  activity.cv <- 0  # Coefficient of variation
  period.counts <- NULL

  if (!is.null(counts)) {
    period.counts <- counts[start.idx:end.idx]
    total.counts <- sum(period.counts, na.rm = TRUE)
    activity.sd <- sd(period.counts, na.rm = TRUE)
    if (is.na(activity.sd)) activity.sd <- 0
    mean.counts <- mean(period.counts, na.rm = TRUE)
    if (!is.na(mean.counts) && mean.counts > 0) {
      activity.cv <- activity.sd / mean.counts
    }
  }

  # Tudor-Locke / ActiLife Fragmentation Index (FI):
  #   FI = 100 * (number of 1-epoch immobile/sleep bouts) / (number of sleep bouts)
  sleep_runs <- rle(sleep.state == "S")
  sleep.bout.lengths <- sleep_runs$lengths[sleep_runs$values]
  fragmentation.index <- if (length(sleep.bout.lengths) > 0) {
    100 * sum(sleep.bout.lengths == 1) / length(sleep.bout.lengths)
  } else {
    0
  }

  # Movement Index (MI): % of period epochs with non-zero activity counts.
  # When counts are unavailable, fall back to the wake fraction of the period.
  if (!is.null(period.counts)) {
    n.scored <- sum(!is.na(period.counts))
    movement.index <- if (n.scored > 0) {
      100 * sum(period.counts > 0, na.rm = TRUE) / n.scored
    } else {
      0
    }
  } else {
    movement.index <- 100 * wake.minutes / total.minutes
  }

  data.frame(
    in_bed_time = format(in.bed.time, "%Y-%m-%d %H:%M:%S"),
    out_bed_time = format(out.bed.time, "%Y-%m-%d %H:%M:%S"),
    onset = format(onset, "%Y-%m-%d %H:%M:%S"),
    sleep_time = sleep.minutes,
    wake_time = wake.minutes,
    number_of_awakenings = awakenings,
    average_awakening = round(average.awakening, 2),
    movement_index = round(movement.index, 2),
    fragmentation_index = round(fragmentation.index, 2),
    sleep_efficiency = round(sleep.efficiency, 2),
    total_counts = total.counts,
    activity_sd = round(activity.sd, 2),
    activity_cv = round(activity.cv, 3),
    stringsAsFactors = FALSE
  )
}


#' Enhanced Sleep Fragmentation Analysis
#'
#' Provides detailed sleep fragmentation metrics beyond basic awakening counts,
#' including temporal patterns and bout duration analysis.
#'
#' @param sleep_state Character vector of sleep states ("S" or "W")
#' @param timestamps POSIXt vector of timestamps corresponding to sleep_state
#' @param epoch_length Epoch length in seconds (default: 60). Used to convert the
#'   epoch count to hours when computing the per-hour transition rate.
#'
#' @return List containing:
#'   - basic_metrics: Standard fragmentation metrics
#'   - bout_analysis: Sleep and wake bout duration statistics
#'   - temporal_pattern: Hourly fragmentation profile
#'   - sleep_fragmentation_index: Composite fragmentation score
#'
#' @details
#' Enhanced fragmentation analysis provides:
#' 1. Bout duration analysis (mean, median, SD for sleep/wake bouts)
#' 2. Transition frequency per hour
#' 3. Temporal pattern showing which hours have most fragmentation
#' 4. Sleep Fragmentation Index (SFI) combining multiple metrics
#'
#' Note: the Sleep Fragmentation Index returned here is a custom composite metric
#' (a weighted blend of transition rate, short-sleep-bout proportion, and wake
#' proportion) and is not the Tudor-Locke/ActiLife Sleep Fragmentation Index.
#'
#' @references
#' Lim J, Dinges DF (2008). Sleep deprivation and vigilant attention.
#' Ann N Y Acad Sci, 1129:305-322.
#'
#' @export
sleep.fragmentation.enhanced <- function(sleep_state, timestamps, epoch_length = 60) {

  if (length(sleep_state) == 0) {
    return(.empty.fragmentation.result())
  }

  if (length(sleep_state) != length(timestamps)) {
    warning("sleep_state and timestamps length mismatch")
    timestamps <- seq_along(sleep_state)
  }

  n <- length(sleep_state)

  # Run-length encoding for bout detection
  rle_result <- rle(sleep_state)
  bout_lengths <- rle_result$lengths
  bout_values <- rle_result$values
  n_bouts <- length(bout_lengths)

  # Separate sleep and wake bouts
  sleep_bouts <- bout_lengths[bout_values == "S"]
  wake_bouts <- bout_lengths[bout_values == "W"]

  # Basic metrics
  n_awakenings <- sum(bout_values == "W" & c(FALSE, bout_values[-n_bouts] == "S"))
  total_sleep <- sum(sleep_state == "S")
  total_wake <- sum(sleep_state == "W")

  # Bout duration statistics
  sleep_bout_stats <- list(
    count = length(sleep_bouts),
    mean = if (length(sleep_bouts) > 0) mean(sleep_bouts) else NA_real_,
    median = if (length(sleep_bouts) > 0) median(sleep_bouts) else NA_real_,
    sd = if (length(sleep_bouts) > 1) sd(sleep_bouts) else NA_real_,
    min = if (length(sleep_bouts) > 0) min(sleep_bouts) else NA_real_,
    max = if (length(sleep_bouts) > 0) max(sleep_bouts) else NA_real_
  )

  wake_bout_stats <- list(
    count = length(wake_bouts),
    mean = if (length(wake_bouts) > 0) mean(wake_bouts) else NA_real_,
    median = if (length(wake_bouts) > 0) median(wake_bouts) else NA_real_,
    sd = if (length(wake_bouts) > 1) sd(wake_bouts) else NA_real_,
    min = if (length(wake_bouts) > 0) min(wake_bouts) else NA_real_,
    max = if (length(wake_bouts) > 0) max(wake_bouts) else NA_real_
  )

  # Temporal pattern (transitions per hour)
  temporal_pattern <- tryCatch({
    hours <- as.numeric(format(as.POSIXct(timestamps), "%H"))
    transitions <- c(FALSE, sleep_state[-1] != sleep_state[-n])
    hourly_transitions <- tapply(transitions, hours, sum, na.rm = TRUE)

    data.frame(
      hour = as.numeric(names(hourly_transitions)),
      transitions = as.numeric(hourly_transitions),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    NULL
  })

  # Sleep Fragmentation Index (SFI)
  # Combines: transition rate + short sleep bout proportion + wake proportion
  total_transitions <- sum(bout_values[-1] != bout_values[-n_bouts])
  # Transitions per hour, scaled by epoch length (n epochs * epoch_length / 3600 s)
  period_hours <- n * epoch_length / 3600
  transition_rate <- if (period_hours > 0) total_transitions / period_hours else NA_real_

  short_sleep_threshold <- 10  # Minutes
  short_sleep_proportion <- if (length(sleep_bouts) > 0) {
    sum(sleep_bouts < short_sleep_threshold) / length(sleep_bouts)
  } else {
    NA_real_
  }

  wake_proportion <- total_wake / n

  # Composite index (higher = more fragmented)
  sfi <- NA_real_
  if (!is.na(transition_rate) && !is.na(short_sleep_proportion) && !is.na(wake_proportion)) {
    # Normalized to 0-100 scale
    sfi <- min(100, (transition_rate * 10 + short_sleep_proportion * 50 + wake_proportion * 40))
  }

  list(
    basic_metrics = list(
      n_awakenings = n_awakenings,
      total_sleep_epochs = total_sleep,
      total_wake_epochs = total_wake,
      total_transitions = total_transitions,
      sleep_efficiency = round(100 * total_sleep / n, 2)
    ),
    bout_analysis = list(
      sleep_bouts = sleep_bout_stats,
      wake_bouts = wake_bout_stats
    ),
    temporal_pattern = temporal_pattern,
    sleep_fragmentation_index = round(sfi, 2),
    transition_rate_per_hour = round(transition_rate, 2),
    short_sleep_proportion = round(short_sleep_proportion * 100, 2)
  )
}


#' @keywords internal
.empty.fragmentation.result <- function() {
  list(
    basic_metrics = list(
      n_awakenings = NA_integer_,
      total_sleep_epochs = NA_integer_,
      total_wake_epochs = NA_integer_,
      total_transitions = NA_integer_,
      sleep_efficiency = NA_real_
    ),
    bout_analysis = list(
      sleep_bouts = list(count = 0, mean = NA_real_, median = NA_real_,
                         sd = NA_real_, min = NA_real_, max = NA_real_),
      wake_bouts = list(count = 0, mean = NA_real_, median = NA_real_,
                        sd = NA_real_, min = NA_real_, max = NA_real_)
    ),
    temporal_pattern = NULL,
    sleep_fragmentation_index = NA_real_,
    transition_rate_per_hour = NA_real_,
    short_sleep_proportion = NA_real_
  )
}
