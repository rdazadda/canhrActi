#' Cole-Kripke Sleep Scoring Algorithm
#'
#' Implements the Cole-Kripke sleep/wake scoring algorithm for adults (ages 35-65).
#'
#' @param counts Numeric vector of activity counts (from axis1)
#' @param apply_rescoring Logical. Apply Webster's rescoring rules? (default: TRUE)
#'
#' @return Character vector of sleep states: "S" (sleep) or "W" (wake)
#'
#' @details
#' Uses 7-epoch window (4 previous + current + 2 next) with weighted coefficients:
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
sleep.cole.kripke <- function(counts, apply_rescoring = TRUE) {

  if (length(counts) == 0) {
    stop("Input counts vector is empty")
  }

  if (any(is.na(counts))) {
    warning("NA values detected in counts (", sum(is.na(counts)),
            " values). Replacing with 0.")
    counts[is.na(counts)] <- 0
  }

  # Step 1: Scale counts (divide by 100, cap at 300)
  scaled.counts <- counts / 100
  scaled.counts[scaled.counts > 300] <- 300

  n <- length(scaled.counts)

  # Step 2: Calculate sleep index D for each epoch
  # D = 0.001 * (106*P4 + 54*P3 + 58*P2 + 76*P1 + 230*C + 74*N1 + 67*N2)
  # Coefficients for the 7-epoch window
  coef <- c(106, 54, 58, 76, 230, 74, 67)

  # Pad the vector with zeros for boundary epochs
  padded.counts <- c(rep(0, 4), scaled.counts, rep(0, 2))

  # Calculate sleep index using rolling window
  sleep.index <- numeric(n)

  for (i in 1:n) {
    # Extract 7-epoch window centered at position i (in padded vector: i+4)
    window.idx <- (i):(i + 6)
    window.counts <- padded.counts[window.idx]

    # Calculate weighted sum
    sleep.index[i] <- 0.001 * sum(coef * window.counts)
  }

  # Step 3: Classify sleep/wake based on threshold
  # D < 1 = Sleep (S), D >= 1 = Wake (W)
  sleep.state <- ifelse(sleep.index < 1, "S", "W")

  # Step 4: Apply Webster's rescoring rules (if requested)
  if (apply_rescoring) {
    sleep.state <- .apply.webster.rescoring(sleep.state)
  }

  return(sleep.state)
}


.apply.webster.rescoring <- function(sleep.state) {
  # Webster's Rescoring Rules for Cole-Kripke algorithm
  # Reference: Webster JB et al. (1982). Psychophysiology. 19(6):682-687
  #
  # Rules (applied sequentially):
  # 1. After >= 4 min wake, next 1 min sleep -> wake
  # 2. After >= 10 min wake, next 3 min sleep -> wake
  # 3. After >= 15 min wake, next 4 min sleep -> wake
  # 4. <= 6 min sleep surrounded by >= 15 min wake on each side -> wake
 # 5. <= 10 min sleep surrounded by >= 20 min wake on each side -> wake

  n <- length(sleep.state)
  rescored <- sleep.state

  # Helper function: count consecutive wake epochs ending at position idx
  count.wake.before <- function(idx) {
    if (idx < 2) return(0)
    count <- 0
    for (j in (idx - 1):1) {
      if (sleep.state[j] == "W") {
        count <- count + 1
      } else {
        break
      }
    }
    return(count)
  }

  # Helper function: count consecutive wake epochs starting at position idx
  count.wake.after <- function(idx) {
    if (idx > n) return(0)
    count <- 0
    for (j in idx:n) {
      if (sleep.state[j] == "W") {
        count <- count + 1
      } else {
        break
      }
    }
    return(count)
  }

  # Rule 1: After >= 4 min wake, next 1 min sleep -> wake
  for (i in 2:n) {
    if (sleep.state[i] == "S" && count.wake.before(i) >= 4) {
      rescored[i] <- "W"
    }
  }

  # Rule 2: After >= 10 min wake, next 3 consecutive min sleep -> wake
  if (n >= 4) {
    for (i in 2:(n - 2)) {
      if (all(sleep.state[i:(i + 2)] == "S") && count.wake.before(i) >= 10) {
        rescored[i:(i + 2)] <- "W"
      }
    }
  }

  # Rule 3: After >= 15 min wake, next 4 consecutive min sleep -> wake
  if (n >= 5) {
    for (i in 2:(n - 3)) {
      if (all(sleep.state[i:(i + 3)] == "S") && count.wake.before(i) >= 15) {
        rescored[i:(i + 3)] <- "W"
      }
    }
  }

  # Rule 4: <= 6 min sleep surrounded by >= 15 min wake on each side -> wake
  # Find sleep bouts and check surroundings
  i <- 1
  while (i <= n) {
    if (sleep.state[i] == "S") {
      # Found start of sleep bout
      bout.start <- i
      bout.end <- i
      while (bout.end < n && sleep.state[bout.end + 1] == "S") {
        bout.end <- bout.end + 1
      }
      bout.len <- bout.end - bout.start + 1

      if (bout.len <= 6) {
        wake.before <- count.wake.before(bout.start)
        wake.after <- count.wake.after(bout.end + 1)

        if (wake.before >= 15 && wake.after >= 15) {
          rescored[bout.start:bout.end] <- "W"
        }
      }
      i <- bout.end + 1
    } else {
      i <- i + 1
    }
  }

  # Rule 5: <= 10 min sleep surrounded by >= 20 min wake on each side -> wake
  i <- 1
  while (i <= n) {
    if (sleep.state[i] == "S") {
      bout.start <- i
      bout.end <- i
      while (bout.end < n && sleep.state[bout.end + 1] == "S") {
        bout.end <- bout.end + 1
      }
      bout.len <- bout.end - bout.start + 1

      if (bout.len <= 10) {
        wake.before <- count.wake.before(bout.start)
        wake.after <- count.wake.after(bout.end + 1)

        if (wake.before >= 20 && wake.after >= 20) {
          rescored[bout.start:bout.end] <- "W"
        }
      }
      i <- bout.end + 1
    } else {
      i <- i + 1
    }
  }

  return(rescored)
}


#' Sadeh Sleep Scoring Algorithm
#'
#' Implements the Sadeh sleep/wake scoring algorithm for children/adolescents (ages 10-25).
#'
#' @param counts Numeric vector of activity counts (from axis1)
#'
#' @return Character vector of sleep states: "S" (sleep) or "W" (wake)
#'
#' @details
#' Uses 11-epoch window (5 previous + current + 5 next):
#' SI = 7.601 - (0.065*AVG) - (1.08*NATS) - (0.056*SD) - (0.703*LG)
#'
#' AVG: Mean of 11-epoch window
#' NATS: Count of epochs with 50 <= counts < 100
#' SD: Standard deviation of current + 5 previous epochs
#' LG: ln(count + 1)
#'
#' Counts capped at 300. Classification: SI > -4 = Sleep, SI <= -4 = Wake
#'
#' @references
#' Sadeh A, Sharkey KM, Carskadon MA (1994).
#' Activity-based sleep-wake identification: an empirical test of methodological issues.
#' Sleep, 17(3):201-207.
#'
#' ActiGraph documentation:
#' https://actigraphcorp.my.site.com/support/s/article/Where-can-I-find-documentation-for-the-Sadeh-and-Cole-Kripke-algorithms
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
sleep.sadeh <- function(counts) {

  if (length(counts) == 0) {
    stop("Input counts vector is empty")
  }

  if (any(is.na(counts))) {
    warning("NA values detected in counts (", sum(is.na(counts)),
            " values). Replacing with 0.")
    counts[is.na(counts)] <- 0
  }

  # Step 1: Cap counts at 300
  capped.counts <- counts
  capped.counts[capped.counts > 300] <- 300

  n <- length(capped.counts)

  # Pad the vector with zeros for boundary epochs
  # Need 5 before and 5 after for 11-epoch window
  padded.counts <- c(rep(0, 5), capped.counts, rep(0, 5))

  # Step 2: Calculate sleep index for each epoch
  sleep.index <- numeric(n)

  for (i in 1:n) {
    # Indices in padded vector
    padded.idx <- i + 5

    # AVG: Mean of 11-epoch window (current ± 5 epochs)
    window.11 <- padded.counts[(padded.idx - 5):(padded.idx + 5)]
    AVG <- mean(window.11)

    # NATS: Count of epochs with 50 <= counts < 100 in 11-epoch window
    NATS <- sum(window.11 >= 50 & window.11 < 100)

    # SD: Standard deviation of 6 epochs (current + 5 previous)
    window.6 <- padded.counts[(padded.idx - 5):padded.idx]
    SD <- sd(window.6)
    if (is.na(SD)) SD <- 0  # Handle case where all values are identical

    # LG: Natural logarithm of (current epoch + 1)
    current.count <- padded.counts[padded.idx]
    LG <- log(current.count + 1)

    # Calculate sleep index
    # SI = 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)
    sleep.index[i] <- 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)
  }

  # Step 3: Classify sleep/wake based on threshold
  # SI > -4 = Sleep (S), SI <= -4 = Wake (W)
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
#' @param bedtime_start Consecutive sleep epochs to define bedtime (default: 5)
#' @param wake_time_end Consecutive wake epochs to define wake time (default: 10)
#' @param min_sleep_period Minimum sleep period in minutes (default: 160)
#' @param max_sleep_period Maximum sleep period in minutes (default: 1440)
#' @param min_nonzero_epochs Minimum non-zero epochs (default: 15)
#'
#' @return Data frame with sleep periods and metrics (TST, SE, WASO, awakenings, etc.)
#'
#' @details
#' Suspicious periods (>12 hours, >99% efficiency, 0 awakenings) are automatically
#' filtered as they likely indicate device removal rather than actual sleep.
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
                              min_nonzero_epochs = 15) {

  if (length(sleep.state) != length(timestamps)) {
    stop("Length of sleep.state (", length(sleep.state),
         ") must equal length of timestamps (", length(timestamps), ")")
  }

  if (length(sleep.state) == 0) {
    warning("Empty sleep.state vector. Returning empty results.")
    return(data.frame())
  }

  if (!is.null(counts) && length(counts) != length(sleep.state)) {
    stop("Length of counts (", length(counts),
         ") must equal length of sleep.state (", length(sleep.state), ")")
  }

  sleep.periods <- .detect.sleep.periods(
    sleep.state = sleep.state,
    timestamps = timestamps,
    counts = counts,
    bedtime_start = bedtime_start,
    wake_time_end = wake_time_end,
    min_sleep_period = min_sleep_period,
    max_sleep_period = max_sleep_period,
    min_nonzero_epochs = min_nonzero_epochs
  )

  return(sleep.periods)
}


.detect.sleep.periods <- function(sleep.state, timestamps, counts,
                                  bedtime_start, wake_time_end,
                                  min_sleep_period, max_sleep_period,
                                  min_nonzero_epochs) {

  n <- length(sleep.state)
  periods <- list()
  period.count <- 0
  i <- 1

  while (i <= n) {
    # Look for bedtime: bedtime_start consecutive sleep epochs
    if (i <= n - bedtime_start + 1) {
      if (all(sleep.state[i:(i + bedtime_start - 1)] == "S")) {
        # Found potential bedtime
        bedtime.idx <- i

        # Now look for wake time: wake_time_end consecutive wake epochs
        j <- bedtime.idx + bedtime_start
        wake.idx <- NA

        while (j <= n - wake_time_end + 1) {
          if (all(sleep.state[j:(j + wake_time_end - 1)] == "W")) {
            wake.idx <- j
            break
          }
          j <- j + 1
        }

        # If we found wake time, validate the sleep period
        if (!is.na(wake.idx)) {
          period.duration <- wake.idx - bedtime.idx

          # Check if period meets criteria
          if (period.duration >= min_sleep_period &&
              period.duration < max_sleep_period) {

            period.sleep.state <- sleep.state[bedtime.idx:(wake.idx - 1)]

            # Check minimum non-zero epochs (count sleep epochs)
            nonzero.count <- sum(period.sleep.state == "S")

            if (nonzero.count >= min_nonzero_epochs) {
              # Valid sleep period found
              period.count <- period.count + 1

              metrics <- .calculate.sleep.metrics(
                sleep.state = period.sleep.state,
                start.idx = bedtime.idx,
                end.idx = wake.idx - 1,
                timestamps = timestamps,
                counts = counts
              )

              periods[[period.count]] <- metrics

              # Move to the epoch after the detected sleep period ends
              # This allows detection of adjacent sleep periods
              i <- wake.idx
              next
            }
          }
        }
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
                       "sleep_efficiency", "total_counts")]

  # Filter out suspicious periods (likely device removal, not real sleep)
  # Criteria: very long (>12 hours) + very high efficiency (>99%) + no awakenings
  suspicious <- result$sleep_time > 720 &
                result$sleep_efficiency > 99 &
                result$number_of_awakenings == 0
  if (any(suspicious)) {
    warning("Removed ", sum(suspicious), " suspicious sleep period(s) ",
            "(>12h, >99% efficiency, 0 awakenings - likely device removal)")
    result <- result[!suspicious, , drop = FALSE]
    if (nrow(result) > 0) {
      result$period_number <- 1:nrow(result)
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
  movement.index <- 100 * wake.minutes / total.minutes
  fragmentation.index <- (awakenings + movement.index) / total.minutes
  sleep.efficiency <- 100 * sleep.minutes / total.minutes

  total.counts <- if (!is.null(counts)) {
    sum(counts[start.idx:end.idx], na.rm = TRUE)
  } else {
    0
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
    stringsAsFactors = FALSE
  )
}
