#' @title Circadian Rhythm Analysis
#'
#' @description
#' Circadian rhythm analysis implementing non-parametric methods from peer-reviewed
#' literature for characterizing 24-hour activity patterns.
#'
#' @name circadian-analysis
#'
#' @references
#' \strong{Non-Parametric Methods:}
#' \itemize{
#'   \item van Someren EJ, et al. (1999). Bright light therapy: improved sensitivity to
#'     its effects on rest-activity rhythms in Alzheimer patients by application of
#'     nonparametric methods. Chronobiol Int, 16(4):505-518.
#'   \item Witting W, et al. (1990). Alterations in the circadian rest-activity rhythm
#'     in aging and Alzheimer's disease. Biol Psychiatry, 27(6):563-572.
#'   \item Blume C, et al. (2016). nparACT package for R: A free software tool for the
#'     non-parametric analysis of actigraphy data. MethodsX, 3:430-435.
#' }
#'
#' \strong{Sleep Regularity:}
#' \itemize{
#'   \item Phillips AJK, et al. (2017). Irregular sleep/wake patterns are associated
#'     with poorer academic performance and delayed circadian and sleep/wake timing.
#'     Scientific Reports, 7(1):3216.
#'   \item Lunsford-Avery JR, et al. (2018). Validation of the Sleep Regularity Index
#'     in Older Adults and Associations with Cardiometabolic Risk.
#'     Scientific Reports, 8(1):14158.
#' }
#'
#' \strong{Additional Metrics:}
#' \itemize{
#'   \item Wittmann M, et al. (2006). Social jetlag: misalignment of biological and
#'     social time. Chronobiology International, 23(1-2):497-509.
#'   \item Di J, et al. (2019). Joint and Individual Representation of Domains of
#'     Physical Activity, Sleep, and Circadian Rhythmicity. Statistics in Biosciences.
#' }
NULL


#' Circadian Rhythm Analysis
#'
#' Circadian analysis using non-parametric methods from GGIR, ActCR, nparACT,
#' and peer-reviewed literature. Implements minute-level sliding window for
#' L5/M10, Sleep Regularity Index, and multiple phase metrics.
#'
#' @param counts Numeric vector of activity counts (minute-level recommended for accuracy)
#' @param timestamps POSIXct vector of epoch timestamps
#' @param sleep_state Optional character vector of sleep states ("S" or "W") for SRI calculation
#' @param wear_time Optional logical vector indicating wear time (TRUE = worn)
#' @param epoch_length Numeric. Epoch length in seconds (default: 60)
#' @param calculate_sri Logical. Calculate Sleep Regularity Index? (default: TRUE if sleep_state provided)
#' @param use_cpp Logical. Use C++ backend for faster computation? (default: TRUE)
#'
#' @return List with class 'canhrActi_circadian' containing:
#'   \describe{
#'     \item{L5, L5_start, L5_start_hour}{Least active 5-hour average and timing (sliding window)}
#'     \item{M10, M10_start, M10_start_hour}{Most active 10-hour average and timing (sliding window)}
#'     \item{L1, M1}{Least/most active 1-hour for additional granularity}
#'     \item{RA}{Relative amplitude: (M10-L5)/(M10+L5), range 0-1}
#'     \item{IS}{Interdaily stability: day-to-day consistency (0-1, higher=more stable)}
#'     \item{IV}{Intradaily variability: within-day fragmentation (0=sine, ~2=noise)}
#'     \item{phi}{First-order autocorrelation at 1-hour lag}
#'     \item{SRI}{Sleep Regularity Index (-100 to 100, higher=more regular)}
#'     \item{onset_timing_variability}{Mean circular SD of daily L5/M10 onset times
#'       (day-to-day phase variability). NOT the published Fischer/Roenneberg CPD.}
#'     \item{hourly_profile}{Mean activity by hour of day}
#'     \item{daily_metrics}{Per-day L5, M10, RA values}
#'   }
#'
#' @details
#' \strong{Non-Parametric Metrics (van Someren et al., 1999):}
#'
#' This implementation uses the \strong{minute-level sliding window}
#' method for L5/M10 calculation, NOT hourly aggregation. This provides more accurate
#' timing estimates (to the minute rather than hour).
#'
#' \itemize{
#'   \item \strong{L5} = Average activity during the least active 5 consecutive hours.
#'     Uses sliding window across minute-level data for precise timing.
#'   \item \strong{M10} = Average activity during the most active 10 consecutive hours.
#'   \item \strong{RA} = Relative amplitude = (M10 - L5) / (M10 + L5).
#'     Range 0-1, higher values indicate stronger day-night amplitude.
#'   \item \strong{IS} = Interdaily stability. Quantifies coupling of rhythm to
#'     stable zeitgebers. Range 0 (Gaussian noise) to 1 (perfect stability).
#'   \item \strong{IV} = Intradaily variability. Measures rhythm fragmentation.
#'     ~0 for perfect sine wave, ~2 for Gaussian noise, can exceed 2 with ultradian rhythms.
#' }
#'
#' \strong{Sleep Regularity Index (Phillips et al., 2017):}
#'
#' SRI = probability of being in same sleep/wake state at any two time points
#' 24 hours apart. Range -100 to +100, with 100 indicating perfect regularity.
#'
#' @examples
#' \dontrun{
#' # Basic analysis
#' results <- canhrActi("participant.agd")
#' circadian <- circadian.rhythm(
#'   results$epoch_data$axis1,
#'   results$epoch_data$timestamp,
#'   results$epoch_data$wear_time
#' )
#' print(circadian)
#' plot(circadian)
#'
#' # With sleep state for SRI calculation
#' sleep_state <- sleep.cole.kripke(results$epoch_data$axis1)
#' circadian <- circadian.rhythm(
#'   results$epoch_data$axis1,
#'   results$epoch_data$timestamp,
#'   sleep_state = sleep_state
#' )
#' }
#'
#' @seealso
#' \code{\link{sleep.regularity.index}} for standalone SRI calculation,
#' \code{\link{social.jet.lag}} for social jet lag calculation
#'
#' @export
circadian.rhythm <- function(counts,
                             timestamps,
                             sleep_state = NULL,
                             wear_time = NULL,
                             epoch_length = 60,
                             calculate_sri = TRUE,
                             use_cpp = TRUE) {

  # Input validation

  if (length(counts) != length(timestamps)) {
    stop("counts and timestamps must have same length")
  }
  if (length(counts) == 0) {
    stop("No data provided")
  }
  if (!inherits(timestamps, "POSIXct")) {
    stop("timestamps must be POSIXct class")
  }

  # Apply wear time filter
  if (!is.null(wear_time)) {
    if (length(wear_time) != length(counts)) {
      stop("wear_time must have same length as counts")
    }
    counts[!wear_time] <- NA
  }

  # Check if C++ is available
  cpp_available <- use_cpp && tryCatch({
    exists("calculate_L5_M10_cpp") && is.function(get("calculate_L5_M10_cpp"))
  }, error = function(e) FALSE)

  # 

  if (cpp_available) {
    # Use fast C++ implementations (10-50x faster)
    # Convert to minute-level if needed
    epochs_per_min <- 60 / epoch_length

    # Validate epoch length - must divide 60 evenly for proper aggregation
    # Non-standard epochs (45s, 90s) cause fractional indexing issues
    if (60 %% epoch_length != 0 && epoch_length %% 60 != 0) {
      warning("Epoch length ", epoch_length, "s does not divide evenly into 60s. ",
              "Minute-level aggregation may be imprecise. ",
              "Recommended epoch lengths: 5, 10, 15, 30, 60, 120, 180, 300s")
    }

    if (epochs_per_min != 1 && epochs_per_min > 0) {
      # Aggregate to minute level for C++ functions
      # Use proper indexing that handles non-integer epochs_per_min
      n_minutes <- floor(length(counts) / max(1, epochs_per_min))
      if (epochs_per_min >= 1) {
        # Sub-minute epochs (5s, 10s, 15s, 30s): aggregate multiple epochs per minute
        epm_int <- as.integer(round(epochs_per_min))
        minute_counts <- sapply(1:n_minutes, function(i) {
          start_idx <- (i - 1) * epm_int + 1
          end_idx <- min(i * epm_int, length(counts))
          mean(counts[start_idx:end_idx], na.rm = TRUE)
        })
      } else {
        # Super-minute epochs (90s, 120s): use weighted resampling
        # Each epoch contributes to multiple minutes proportionally
        minute_counts <- sapply(1:n_minutes, function(i) {
          # Find epochs that overlap with minute i
          min_start <- (i - 1) * 60  # minute start in seconds
          min_end <- i * 60          # minute end in seconds
          epoch_indices <- which(
            ((seq_along(counts) - 1) * epoch_length < min_end) &
            (seq_along(counts) * epoch_length > min_start)
          )
          if (length(epoch_indices) > 0) {
            mean(counts[epoch_indices], na.rm = TRUE)
          } else {
            NA_real_
          }
        })
      }
      minute_counts[is.nan(minute_counts)] <- NA
    } else if (epoch_length == 60) {
      minute_counts <- counts
    } else {
      # Fallback: use counts as-is with warning
      warning("Unexpected epoch_length: ", epoch_length, ". Using counts directly.")
      minute_counts <- counts
    }

    # NOTE: Do NOT replace NA with 0 - the C++ code at lines 128-131 of
    # sliding_window_cpp.cpp already handles NA properly with ISNA() checks.
    # Replacing NA with 0 would bias L5 downward by treating non-wear as inactivity.

    # Track coverage for quality metrics
    n_valid <- sum(!is.na(minute_counts))
    n_total <- length(minute_counts)
    coverage_pct <- if (n_total > 0) 100 * n_valid / n_total else 0

    # Calculate start minute of day from first timestamp
    # This ensures correct alignment of the 24-hour profile
    first_ts <- timestamps[1]
    start_minute <- as.integer(as.numeric(format(first_ts, "%H")) * 60 +
                               as.numeric(format(first_ts, "%M")))

    # L5/M10 using C++ (much faster) with correct time alignment
    # Pass minute_counts directly - C++ handles NA values properly
    l5m10_result <- tryCatch({
      calculate_L5_M10_cpp(minute_counts, 300L, 600L, start_minute)  # 5h=300min, 10h=600min
    }, error = function(e) NULL)

    if (!is.null(l5m10_result)) {
      l5 <- list(
        value = l5m10_result$L5_value,
        start_time = sprintf("%02d:%02d", l5m10_result$L5_onset %/% 60, l5m10_result$L5_onset %% 60),
        start_hour = l5m10_result$L5_onset / 60
      )
      m10 <- list(
        value = l5m10_result$M10_value,
        start_time = sprintf("%02d:%02d", l5m10_result$M10_onset %/% 60, l5m10_result$M10_onset %% 60),
        start_hour = l5m10_result$M10_onset / 60
      )
      ra <- l5m10_result$RA
    } else {
      # Fall back to R implementation
      l5 <- .calculate.LX.sliding(counts, timestamps, X = 5, find_minimum = TRUE, epoch_length = epoch_length)
      m10 <- .calculate.LX.sliding(counts, timestamps, X = 10, find_minimum = FALSE, epoch_length = epoch_length)
      ra <- .calculate.RA(m10$value, l5$value)
    }

    # L1/M1 using R (not critical for speed)
    l1 <- .calculate.LX.sliding(counts, timestamps, X = 1, find_minimum = TRUE, epoch_length = epoch_length)
    m1 <- .calculate.LX.sliding(counts, timestamps, X = 1, find_minimum = FALSE, epoch_length = epoch_length)

    # IS/IV using C++ (aggregate to hourly first)
    hours_factor <- cut(timestamps, breaks = "hour")
    hourly_means <- tapply(counts, hours_factor, mean, na.rm = TRUE)
    hourly_means <- as.numeric(hourly_means[!is.na(hourly_means)])

    # IS: always use the alignment-correct R implementation.
    # calculate_IS_cpp buckets hours by array position mod 24, which only matches
    # real clock hour-of-day when the recording starts at 00:00 AND no hour is
    # dropped. Because any all-NA (non-wear) hour is removed above (shifting every
    # later element) and recordings rarely start at midnight, the C++ buckets do
    # not correspond to real clock hours and misalign the Witting (1990) profile.
    # .calculate.IS.IV keys hourly means by POSIXlt$hour, so it stays aligned.
    is_val <- tryCatch({
      .calculate.IS.IV(counts, timestamps, epoch_length)$IS
    }, error = function(e) NA_real_)

    # IV requires consecutive hours - check for gaps before using C++.
    # calculate_IV_cpp differences consecutive ARRAY positions of the NA-dropped
    # vector, so any missing hour makes a difference bridge a >1h gap and inflates
    # the Witting (1990) successive-difference numerator. Only use C++ when EVERY
    # expected hour is present; otherwise route through the gap-aware R fallback.
    # floor() the span so a complete recording is not misflagged: N distinct
    # clock hours span (N-1) full hours plus a fractional remainder, so
    # floor(span) + 1 == N exactly when every expected hour is present.
    expected_hours <- floor(as.numeric(difftime(max(timestamps), min(timestamps), units = "hours"))) + 1
    has_gaps <- length(hourly_means) < expected_hours  # require all expected hours present

    iv_val <- tryCatch({
      if (has_gaps) {
        # Use gap-aware R implementation for IV
        .calculate.IS.IV(counts, timestamps, epoch_length)$IV
      } else {
        # Safe to use C++ when data is continuous
        calculate_IV_cpp(hourly_means)
      }
    }, error = function(e) NA_real_)

    is_iv <- list(IS = is_val, IV = iv_val)

  } else {
    # Use pure R implementations (slower but always available)
    l5 <- .calculate.LX.sliding(counts, timestamps, X = 5, find_minimum = TRUE, epoch_length = epoch_length)
    m10 <- .calculate.LX.sliding(counts, timestamps, X = 10, find_minimum = FALSE, epoch_length = epoch_length)
    l1 <- .calculate.LX.sliding(counts, timestamps, X = 1, find_minimum = TRUE, epoch_length = epoch_length)
    m1 <- .calculate.LX.sliding(counts, timestamps, X = 1, find_minimum = FALSE, epoch_length = epoch_length)
    ra <- .calculate.RA(m10$value, l5$value)
    is_iv <- .calculate.IS.IV(counts, timestamps, epoch_length)

    # Track coverage for quality metrics (same as C++ path)
    n_valid <- sum(!is.na(counts))
    n_total <- length(counts)
    coverage_pct <- if (n_total > 0) 100 * n_valid / n_total else 0
  }

  # Phi - first-order autocorrelation (GGIR method) - fast enough in R
  phi <- .calculate.phi(counts, timestamps)

  # 

  # 

  sri <- NA_real_
  sri_n_pairs <- NA_integer_
  if (!is.null(sleep_state) && calculate_sri) {
    if (length(sleep_state) != length(counts)) {
      warning("sleep_state length doesn't match counts, skipping SRI calculation")
    } else {
      # Proper Phillips (2017) epoch-of-day x day concordance matrix - robust to
      # gaps and non-midnight starts (supersedes the single-24h-lag form).
      sri_res <- sri.matrix(sleep_state, timestamps, epoch_length)
      sri <- sri_res$SRI
      sri_n_pairs <- sri_res$n_valid_pairs
    }
  }

  # Endogenous period via the Lomb-Scargle periodogram (gap-robust).
  period_res <- tryCatch(circadian.period(counts, timestamps),
                         error = function(e) list(tau = NA_real_, peak_power = NA_real_, p_value = NA_real_))

  #

  hourly_profile <- .calculate.hourly.profile(counts, timestamps)
  daily_metrics <- .calculate.daily.circadian(counts, timestamps, epoch_length)

  # Onset timing variability (circular SD of daily L5/M10 onsets).
  otv <- .onset.timing.variability(daily_metrics)

  # Published Fischer-Roenneberg Composite Phase Deviation + onset CIs, on the
  # daily L5 onset times.
  l5_onsets <- daily_metrics$L5_start_hour
  if (is.null(l5_onsets)) {
    l5_onsets <- vapply(daily_metrics$L5_start, function(s) {
      if (is.na(s) || !nzchar(as.character(s))) return(NA_real_)
      p <- strsplit(as.character(s), ":")[[1]]
      as.numeric(p[1]) + as.numeric(p[2]) / 60
    }, numeric(1))
  }
  cpd_res <- composite.phase.deviation(l5_onsets)
  l5_onset_ci <- circadian.onset.ci(l5_onsets)

  # 

  n_days <- length(unique(as.Date(timestamps)))
  n_valid_days <- sum(!is.na(daily_metrics$L5))

  result <- list(
    # Non-parametric metrics (van Someren et al., 1999)
    L5 = round(l5$value, 2),
    L5_start = l5$start_time,
    L5_start_hour = l5$start_hour,
    M10 = round(m10$value, 2),
    M10_start = m10$start_time,
    M10_start_hour = m10$start_hour,
    L1 = round(l1$value, 2),
    L1_start = l1$start_time,
    M1 = round(m1$value, 2),
    M1_start = m1$start_time,
    RA = round(ra, 4),
    IS = round(is_iv$IS, 4),
    IV = round(is_iv$IV, 4),
    phi = phi,

    # Endogenous period (Lomb-Scargle)
    tau = period_res$tau,
    period_power = period_res$peak_power,
    period_p_value = period_res$p_value,

    # Sleep-based metrics (Phillips et al., 2017; matrix form)
    SRI = sri,
    SRI_n_valid_pairs = sri_n_pairs,

    # Phase variability: circular SD of daily onsets (onset_timing_variability)
    # AND the published Fischer-Roenneberg Composite Phase Deviation (CPD).
    onset_timing_variability = otv$onset_timing_variability,
    L5_variability_hours = otv$L5_variability,
    M10_variability_hours = otv$M10_variability,
    CPD = cpd_res$CPD,
    CPD_precision = cpd_res$precision,
    CPD_accuracy = cpd_res$accuracy,
    L5_onset_mean = l5_onset_ci$mean_onset,
    L5_onset_ci_lower = l5_onset_ci$ci_lower,
    L5_onset_ci_upper = l5_onset_ci$ci_upper,

    # Profiles
    hourly_profile = hourly_profile,
    daily_metrics = daily_metrics,

    # Quality metrics (for assessing data reliability)
    coverage_percent = round(coverage_pct, 1),
    n_valid_epochs = n_valid,
    n_total_epochs = n_total,

    # Metadata
    n_days_analyzed = n_days,
    n_valid_circadian_days = n_valid_days,
    epoch_length = epoch_length,
    analysis_method = "canhrActi_v2_circadian"
  )

  class(result) <- c("canhrActi_circadian", "list")
  return(result)
}


#' Calculate LX/MX using Standard Average-Profile Method (van Someren 1999)
#'
#' Implements the van Someren (1999) method correctly:
#' 1. First creates an average 24-hour activity profile across all days
#' 2. Then applies a circular sliding window to find L5/M10 on the averaged profile
#'
#' This is the CORRECT implementation matching GGIR, nparACT, and the original paper.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#' @param X Window size in hours (5 for L5, 10 for M10)
#' @param find_minimum TRUE for LX (least active), FALSE for MX (most active)
#' @param epoch_length Epoch length in seconds
#'
#' @return List with value, start_time, start_hour (decimal)
#'
#' @references
#' van Someren EJ, et al. (1999). Bright light therapy: improved sensitivity to
#' its effects on rest-activity rhythms in Alzheimer patients by application of
#' nonparametric methods. Chronobiol Int, 16(4):505-518.
#'
#' @keywords internal
.calculate.LX.sliding <- function(counts, timestamps, X, find_minimum = TRUE, epoch_length = 60) {

  n <- length(counts)
  epochs_per_hour <- 3600 / epoch_length
  epochs_per_day <- 24 * epochs_per_hour
  window_size <- as.integer(X * epochs_per_hour)

  if (n < epochs_per_day) {
    return(list(
      value = NA_real_,
      start_time = NA_character_,
      start_hour = NA_real_,
      window_start_idx = NA_integer_,
      window_end_idx = NA_integer_
    ))
  }

  # Create average 24-hour profile
  # This is the KEY step that matches the standard method

  # Calculate minute/epoch of day (0 to epochs_per_day-1)
  epoch_of_day <- (as.numeric(format(timestamps, "%H")) * epochs_per_hour +
                   as.numeric(format(timestamps, "%M")) * (epochs_per_hour / 60) +
                   as.numeric(format(timestamps, "%S")) / epoch_length) %% epochs_per_day
  epoch_of_day <- as.integer(epoch_of_day)

  # Average activity at each epoch of day across all days
  avg_profile <- tapply(counts, epoch_of_day, mean, na.rm = TRUE)

  # Ensure we have a complete 24-hour profile
  full_profile <- rep(NA_real_, epochs_per_day)
  profile_indices <- as.integer(names(avg_profile)) + 1  # Convert to 1-based
  full_profile[profile_indices] <- as.numeric(avg_profile)

  # Interpolate any missing epochs (linear interpolation)
  if (any(is.na(full_profile))) {
    non_na <- which(!is.na(full_profile))
    if (length(non_na) > 2) {
      full_profile <- approx(non_na, full_profile[non_na], xout = 1:epochs_per_day,
                             rule = 2, method = "linear")$y
    }
  }

  # Circular sliding window on average profile
  # Window wraps around midnight (24:00 -> 00:00)

  # Extend profile for circular wraparound
  extended_profile <- c(full_profile, full_profile)

  # Calculate rolling means with circular wraparound
  rolling_means <- sapply(1:epochs_per_day, function(i) {
    window_vals <- extended_profile[i:(i + window_size - 1)]
    mean(window_vals, na.rm = TRUE)
  })

  # Find optimal window
  if (all(is.na(rolling_means))) {
    return(list(
      value = NA_real_,
      start_time = NA_character_,
      start_hour = NA_real_,
      window_start_idx = NA_integer_,
      window_end_idx = NA_integer_
    ))
  }

  if (find_minimum) {
    best_idx <- which.min(rolling_means)
  } else {
    best_idx <- which.max(rolling_means)
  }

  if (length(best_idx) == 0) {
    return(list(
      value = NA_real_,
      start_time = NA_character_,
      start_hour = NA_real_,
      window_start_idx = NA_integer_,
      window_end_idx = NA_integer_
    ))
  }

  best_value <- rolling_means[best_idx]

  # Convert epoch index to time
  # best_idx is 1-based, representing the epoch of day (1 = first epoch)
  start_hour <- (best_idx - 1) / epochs_per_hour
  start_h <- floor(start_hour)
  start_m <- round((start_hour - start_h) * 60)
  if (start_m == 60) {
    start_h <- (start_h + 1) %% 24
    start_m <- 0
  }

  list(
    value = best_value,
    start_time = sprintf("%02d:%02d", start_h, start_m),
    start_hour = round(start_hour, 2),
    window_start_idx = best_idx,
    window_end_idx = (best_idx + window_size - 1) %% epochs_per_day + 1
  )
}


#' Calculate Relative Amplitude
#'
#' RA = (M10 - L5) / (M10 + L5)
#'
#' @param M10 Most active 10-hour average
#' @param L5 Least active 5-hour average
#'
#' @return Numeric. Relative amplitude (0-1)
#'
#' @references
#' van Someren EJ, et al. (1999). Chronobiol Int, 16(4):505-518.
#'
#' @keywords internal
.calculate.RA <- function(M10, L5) {
  if (is.na(M10) || is.na(L5) || (M10 + L5) == 0) {
    return(NA_real_)
  }
  (M10 - L5) / (M10 + L5)
}


#' Calculate Interdaily Stability (IS) and Intradaily Variability (IV)
#'
#' IS measures consistency of activity patterns across days.
#' IV measures fragmentation of the activity rhythm within days.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#' @param epoch_length Epoch length in seconds
#'
#' @return List with IS and IV values
#'
#' @details
#' \strong{IS Formula (Witting et al., 1990):}
#' IS = (n * sum((Xh - Xmean)^2)) / (p * sum((Xi - Xmean)^2))
#'
#' Where:
#' - n = total number of hourly data points
#' - p = number of hours per day (24)
#' - Xh = mean activity for hour h across all days
#' - Xmean = overall mean activity
#'
#' \strong{IV Formula (Witting et al., 1990):}
#' IV = (n * sum((Xi - Xi-1)^2)) / ((n-1) * sum((Xi - Xmean)^2))
#'
#' @references
#' Witting W, et al. (1990). Alterations in the circadian rest-activity rhythm
#' in aging and Alzheimer's disease. Biol Psychiatry, 27(6):563-572.
#'
#' van Someren EJ, et al. (1999). Chronobiol Int, 16(4):505-518.
#'
#' @keywords internal
.calculate.IS.IV <- function(counts, timestamps, epoch_length = 60) {

  # Remove NA values
  valid_idx <- !is.na(counts)
  counts <- counts[valid_idx]
  timestamps <- timestamps[valid_idx]

  # Calculate minimum required epochs: need at least 48 hours (2 days) of data
  # This varies by epoch length: 48 hours * 60 min/hour / (epoch_length / 60) epochs
  min_epochs <- 48 * 60 * 60 / epoch_length  # 48 hours in epochs
  if (length(counts) < min_epochs) {
    return(list(IS = NA_real_, IV = NA_real_))
  }

  # Aggregate to hourly data for IS/IV calculation
  hours <- as.POSIXlt(timestamps)$hour
  dates <- as.Date(timestamps)

  # Calculate hourly means for each unique date-hour combination
  hourly_data <- aggregate(counts, by = list(date = dates, hour = hours), FUN = mean, na.rm = TRUE)
  names(hourly_data)[3] <- "activity"

  # Overall mean
  X_mean <- mean(hourly_data$activity, na.rm = TRUE)

  # Number of data points
  n <- nrow(hourly_data)
  p <- 24  # Hours per day

  # Calculate mean activity for each hour across all days (Xh)
  hourly_means <- aggregate(activity ~ hour, data = hourly_data, FUN = mean, na.rm = TRUE)

  # Interdaily Stability (IS)
  # IS = n * var(hourly_means) / (p * var(all_points))
  between_hour_var <- sum((hourly_means$activity - X_mean)^2, na.rm = TRUE)
  total_var <- sum((hourly_data$activity - X_mean)^2, na.rm = TRUE)

  IS <- if (total_var > 0) {
    (n * between_hour_var) / (p * total_var)
  } else NA_real_

  # Bound IS to 0-1 (can slightly exceed due to numerical precision)
  if (!is.na(IS)) {
    IS <- max(0, min(1, IS))
  }

  # Intradaily Variability (IV)
  # Standard Witting (1990): IV = n * sum((Xi - Xi-1)^2) / ((n-1) * sum((Xi - X_mean)^2)).
  # IMPORTANT: Only include differences between CONSECUTIVE hours to avoid
  # inflated IV values when there are data gaps (e.g., non-wear periods).
  # A gap of >1 hour would incorrectly inflate the squared differences. We use a
  # gap-aware variant (see IV computation below) with a single consistent count
  # for the numerator (consecutive pairs) and denominator (all hourly points).
  hourly_data <- hourly_data[order(hourly_data$date, hourly_data$hour), ]

  # Calculate time differences in hours between consecutive data points
  # Consecutive hours should have diff = 1 (same day) or diff = -23 (midnight)
  if (n > 1) {
    hour_diffs <- diff(as.numeric(hourly_data$hour))
    date_diffs <- diff(as.numeric(hourly_data$date))

    # Consecutive hours are: same day with hour diff of 1, OR
    # consecutive days with hour diff of -23 (23:00 to 00:00)
    is_consecutive <- (date_diffs == 0 & hour_diffs == 1) |
                      (date_diffs == 1 & hour_diffs == -23)

    # Only include differences between actually consecutive hours
    activity_diffs <- diff(hourly_data$activity)
    consecutive_diffs <- activity_diffs[is_consecutive]
    successive_diff_sq <- sum(consecutive_diffs^2, na.rm = TRUE)
    # Number of consecutive diff pairs actually contributing to the numerator
    n_pairs <- sum(is_consecutive, na.rm = TRUE)
  } else {
    successive_diff_sq <- 0
    n_pairs <- 0
  }

  # Intradaily Variability (gap-aware variant of Witting et al., 1990).
  # The published IV = [n * sum((Xi - Xi-1)^2)] / [(n-1) * sum((Xi - Xmean)^2)]
  # uses a single n throughout, i.e. IV = mean(successive squared diff) / variance.
  # To keep a consistent count in numerator and denominator while excluding
  # gap-bridging differences, we form:
  #   mean successive squared diff = successive_diff_sq / n_pairs   (over consecutive pairs only)
  #   variance of the full series   = total_var / n                 (over all n hourly points)
  # IV = (mean successive squared diff) / (variance). For a fully consecutive
  # series this reduces to the standard n/(n-1) Witting normalization.
  IV <- if (total_var > 0 && n_pairs > 0) {
    (successive_diff_sq / n_pairs) / (total_var / n)
  } else NA_real_

  list(IS = IS, IV = IV)
}


#' Calculate Phi (First-Order Autocorrelation)
#'
#' Phi indicates how correlated the activity time series is with itself
#' at a 1-hour lag. Used in GGIR as a measure of activity predictability.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#'
#' @return Numeric phi value (-1 to 1, higher = more autocorrelated)
#'
#' @details
#' Higher phi indicates more consistent, predictable activity patterns.
#' Low or negative phi indicates fragmented, unpredictable activity.
#'
#' This is the lag-1 autocorrelation of the hourly activity series. The series
#' is placed on a regular hourly grid (NA for missing/non-wear hours) so the
#' "1-hour lag" only correlates genuinely adjacent clock hours rather than
#' bridging multi-hour gaps. It equals the AR(1) coefficient only for a true
#' AR(1) process.
#'
#' @references
#' van Hees VT, et al. (2015). A Novel, Open Access Method to Assess Sleep
#' Duration Using a Wrist-Worn Accelerometer. PLoS ONE, 10(11):e0142533.
#'
#' Winnebeck EC, et al. (2018). Dynamics and Ultradian Structure of Human
#' Sleep in Real Life. Current Biology, 28(1):49-59.
#'
#' @keywords internal
.calculate.phi <- function(counts, timestamps) {

  # Aggregate to hourly means on a REGULAR hourly grid.
  # cut(..., breaks = "hour") produces a factor whose levels span every clock
  # hour from the first to the last timestamp, so empty (non-wear) hours appear
  # as NA in the tapply result. We KEEP those NAs in place (ordered by the factor
  # levels) instead of dropping them, so adjacent array positions correspond to
  # adjacent clock hours. Dropping NAs would let the lag-1 ACF silently bridge a
  # multi-hour gap and correlate points that are not actually 1 hour apart.
  hours_factor <- cut(timestamps, breaks = "hour")
  hourly_means <- tapply(counts, hours_factor, mean, na.rm = TRUE)
  # Order by the (time-ordered) factor levels so the series is on a regular grid
  hourly_means <- as.numeric(hourly_means[levels(hours_factor)])

  # Need at least 2 days of hourly slots (including NA gaps)
  n <- length(hourly_means)
  if (n < 48 || sum(!is.na(hourly_means)) < 48) {
    return(NA_real_)
  }

  # Lag-1 autocorrelation. na.pass keeps the regular grid; acf computes the
  # covariance only over genuinely adjacent (non-NA) hourly pairs.
  acf_result <- tryCatch({
    acf(hourly_means, lag.max = 1, plot = FALSE, na.action = na.pass)
  }, error = function(e) NULL)

  if (is.null(acf_result)) {
    return(NA_real_)
  }

  phi <- acf_result$acf[2]  # lag 1 autocorrelation

  return(round(phi, 4))
}


#' Calculate Sleep Regularity Index (SRI) - Fast Vectorized Version
#'
#' The probability of being in the same sleep/wake state at any two time points
#' 24 hours apart, averaged across the recording period.
#'
#' @param sleep_state Character vector of sleep states ("S" or "W")
#' @param timestamps POSIXct timestamps
#' @param epoch_length Epoch length in seconds
#'
#' @return Numeric SRI value (-100 to 100, higher = more regular)
#'
#' @details
#' SRI = 200 * P(same state at t and t-24h) - 100
#'
#' A perfectly regular sleeper (same schedule every day) scores 100.
#' Random sleep/wake patterns score ~0.
#' Perfectly anti-regular patterns (opposite states) score -100.
#'
#' @references
#' Phillips AJK, et al. (2017). Irregular sleep/wake patterns are associated
#' with poorer academic performance and delayed circadian and sleep/wake timing.
#' Scientific Reports, 7(1):3216.
#'
#' @keywords internal
.calculate.sri.fast <- function(sleep_state, timestamps, epoch_length = 60) {

  # Convert to binary (1 = sleep, 0 = wake)
  state_binary <- as.integer(sleep_state == "S")

  # Determine epochs per day
  epochs_per_day <- round(24 * 3600 / epoch_length)

  n <- length(state_binary)

  if (n <= epochs_per_day) {
    return(NA_real_)
  }

  # Compare each epoch with 24h prior
  current <- state_binary[(epochs_per_day + 1):n]
  lagged <- state_binary[1:(n - epochs_per_day)]

  # Handle NAs
  valid <- !is.na(current) & !is.na(lagged)
  if (sum(valid) == 0) return(NA_real_)

  current <- current[valid]
  lagged <- lagged[valid]

  # Count matches (both sleep OR both wake)
  matches <- sum(current == lagged)
  total <- length(current)

  # Guard against division by zero
  if (total == 0) {
    return(NA_real_)
  }

  # SRI formula: -100 to +100 scale
  sri <- 200 * (matches / total) - 100

  return(round(sri, 2))
}


#' Calculate Sleep Regularity Index (SRI) - Exported Version
#'
#' @param sleep_state Character vector of sleep states ("S" or "W")
#' @param timestamps POSIXct timestamps (must be regular epochs)
#' @param epoch_length Epoch length in seconds (default 60)
#'
#' @return Numeric SRI value (-100 to 100)
#'
#' @references
#' Phillips AJK, et al. (2017). Scientific Reports, 7(1):3216.
#'
#' @export
sleep.regularity.index <- function(sleep_state, timestamps, epoch_length = 60) {

  if (length(sleep_state) != length(timestamps)) {
    stop("sleep_state and timestamps must have same length")
  }

  n <- length(sleep_state)
  epochs_per_day <- round(24 * 3600 / epoch_length)

  if (n < epochs_per_day * 2) {
    warning("Less than 2 days of data. SRI may be unreliable.")
  }

  .calculate.sri.fast(sleep_state, timestamps, epoch_length)
}


#' Calculate Onset Timing Variability
#'
#' Measures day-to-day variability in the timing of the L5 and M10 activity
#' windows using circular statistics.
#'
#' @param daily_metrics Data frame with daily L5_start, M10_start
#'
#' @return List with onset_timing_variability and component (L5/M10) variabilities
#'
#' @details
#' Uses circular standard deviation to properly handle midnight wraparound.
#' For example, if L5 starts at 23:00 one day and 01:00 the next, the
#' actual variability is 2 hours, not 22 hours.
#'
#' NOTE: This is the mean of the circular SD of daily L5 and M10 onset times. It
#' is NOT the published Composite Phase Deviation (CPD) of Fischer & Roenneberg
#' (2016), which combines each day's precision (deviation from the individual's
#' own mean phase) and accuracy (deviation from a reference phase) as
#' mean(sqrt(precision^2 + accuracy^2)). It is named here to reflect exactly what
#' it computes so it is not mistaken for the established CPD metric.
#'
#' @keywords internal
.onset.timing.variability <- function(daily_metrics) {

  # Convert time strings to decimal hours
  to_decimal <- function(time_str) {
    if (is.na(time_str) || time_str == "" || is.null(time_str)) return(NA_real_)
    parts <- strsplit(as.character(time_str), ":")[[1]]
    if (length(parts) < 2) return(NA_real_)
    as.numeric(parts[1]) + as.numeric(parts[2]) / 60
  }

  l5_hours <- sapply(daily_metrics$L5_start, to_decimal)
  m10_hours <- sapply(daily_metrics$M10_start, to_decimal)

  # Calculate circular SD (accounts for midnight wraparound)
  circular_sd <- function(hours) {
    hours <- hours[!is.na(hours)]
    if (length(hours) < 2) return(NA_real_)

    # Convert to radians (24h -> 2*pi)
    radians <- hours * 2 * pi / 24

    # Mean resultant length
    mean_cos <- mean(cos(radians))
    mean_sin <- mean(sin(radians))
    r <- sqrt(mean_cos^2 + mean_sin^2)

    # Circular SD in hours
    if (r >= 1) return(0)
    if (r <= 0) return(NA_real_)

    # Circular dispersion: sqrt(-2 * log(r))
    circular_sd_rad <- sqrt(-2 * log(r))
    circular_sd_hours <- circular_sd_rad * 24 / (2 * pi)

    return(circular_sd_hours)
  }

  sd_l5 <- circular_sd(l5_hours)
  sd_m10 <- circular_sd(m10_hours)

  # Onset timing variability = mean of the L5/M10 onset-time circular SDs.
  # (Not the published Fischer/Roenneberg CPD; see @details.)
  otv <- mean(c(sd_l5, sd_m10), na.rm = TRUE)

  list(
    onset_timing_variability = round(otv, 2),
    L5_variability = round(sd_l5, 2),
    M10_variability = round(sd_m10, 2)
  )
}


#' Calculate Social Jet Lag
#'
#' Difference between sleep midpoint on work days vs. free days.
#'
#' @param sleep_periods Data frame with in_bed_time, out_bed_time columns
#' @param work_days Optional. Logical vector, date vector, or NULL (uses Mon-Fri default)
#'
#' @return List with social jet lag metrics
#'
#' @details
#' Social jet lag quantifies the discrepancy between social and biological time.
#' It's calculated as MSF - MSW (midpoint on free days minus work days).
#'
#' Positive values (most common) indicate later sleep timing on free days.
#' Values > 1 hour are associated with increased health risks.
#'
#' @references
#' Wittmann M, et al. (2006). Social jetlag: misalignment of biological and
#' social time. Chronobiology International, 23(1-2):497-509.
#'
#' @export
social.jet.lag <- function(sleep_periods, work_days = NULL) {

  if (nrow(sleep_periods) == 0) {
    return(list(
      MSW = NA_real_, MSW_time = NA_character_,
      MSF = NA_real_, MSF_time = NA_character_,
      social_jet_lag_hours = NA_real_,
      social_jet_lag_min = NA_integer_
    ))
  }

  # Parse timestamps
  in_bed <- as.POSIXct(sleep_periods$in_bed_time)
  out_bed <- as.POSIXct(sleep_periods$out_bed_time)

  # Calculate midpoint
  midpoint_posix <- in_bed + difftime(out_bed, in_bed, units = "secs") / 2

  # Convert midpoint to decimal hours
  midpoint_hour <- (as.numeric(format(midpoint_posix, "%H")) +
                   as.numeric(format(midpoint_posix, "%M")) / 60) %% 24

  # Determine work vs free days
  sleep_date <- as.Date(in_bed)

  if (is.null(work_days)) {
    # Default: Mon-Fri are work days, weekend is free
    is_work_day <- weekdays(sleep_date) %in%
      c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  } else if (is.logical(work_days)) {
    is_work_day <- work_days
  } else {
    is_work_day <- sleep_date %in% as.Date(work_days)
  }

  # Calculate MSW and MSF using circular mean (handles midnight wraparound)
  circular_mean <- function(hours) {
    hours <- hours[!is.na(hours)]
    if (length(hours) == 0) return(NA_real_)

    radians <- hours * 2 * pi / 24
    mean_cos <- mean(cos(radians))
    mean_sin <- mean(sin(radians))
    mean_rad <- atan2(mean_sin, mean_cos)

    (mean_rad * 24 / (2 * pi)) %% 24
  }

  msw <- circular_mean(midpoint_hour[is_work_day])
  msf <- circular_mean(midpoint_hour[!is_work_day])

  # Social jet lag = MSF - MSW
  if (is.na(msw) || is.na(msf)) {
    sjl <- NA_real_
  } else {
    sjl <- msf - msw
    # Handle wraparound
    if (abs(sjl) > 12) {
      sjl <- sjl - sign(sjl) * 24
    }
  }

  # Format times
  format_time <- function(h) {
    if (is.na(h)) return(NA_character_)
    sprintf("%02d:%02d", floor(h), round((h %% 1) * 60))
  }

  list(
    MSW = round(msw, 2),
    MSW_time = format_time(msw),
    MSF = round(msf, 2),
    MSF_time = format_time(msf),
    social_jet_lag_hours = round(sjl, 2),
    social_jet_lag_min = if (is.na(sjl)) NA_integer_ else as.integer(round(sjl * 60)),
    n_work_nights = sum(is_work_day, na.rm = TRUE),
    n_free_nights = sum(!is_work_day, na.rm = TRUE)
  )
}


#' Cosinor Analysis for Circadian Rhythm
#'
#' Fits a single-component cosinor model to activity data to estimate
#' parametric circadian rhythm characteristics.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct vector of timestamps
#' @param period Period in hours (default: 24 for circadian rhythm)
#' @param wear_time Optional logical vector indicating wear time
#'
#' @return List with class 'canhrActi_cosinor' containing:
#'   \describe{
#'     \item{mesor}{Rhythm-adjusted mean (MESOR)}
#'     \item{amplitude}{Half the peak-to-trough difference}
#'     \item{acrophase}{Time of peak activity (hours, 0-24)}
#'     \item{acrophase_time}{Acrophase as HH:MM string}
#'     \item{r_squared}{Goodness of fit (coefficient of determination)}
#'     \item{f_statistic}{F-statistic for model significance}
#'     \item{p_value}{P-value for model significance}
#'   }
#'
#' @details
#' The single-component cosinor model fits:
#' \deqn{Y(t) = M + A \cdot cos(2\pi t / T + \phi)}
#'
#' Where:
#' \itemize{
#'   \item M = mesor (rhythm-adjusted mean)
#'   \item A = amplitude (half peak-to-trough difference)
#'   \item T = period (typically 24 hours)
#'   \item φ = acrophase (phase angle, converted to time)
#' }
#'
#' The model is fit using linear least squares on the linearized form:
#' Y(t) = M + β₁·cos(2πt/T) + β₂·sin(2πt/T)
#'
#' @references
#' Nelson W, et al. (1979). Methods for cosinor-rhythmometry.
#' Chronobiologia, 6(4):305-323.
#'
#' Cornelissen G. (2014). Cosinor-based rhythmometry.
#' Theoretical Biology and Medical Modelling, 11:16.
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' cosinor <- cosinor.analysis(
#'   results$epoch_data$axis1,
#'   results$epoch_data$timestamp
#' )
#' print(cosinor)
#' }
#'
#' @export
cosinor.analysis <- function(counts, timestamps, period = 24, wear_time = NULL) {
  # Cosinor analysis using GGIR/ActCR method

# This function fits a cosinor model to the AVERAGED 24-hour activity profile,
  # which is the standard approach used by GGIR, ActCR, and most published studies.
  #
  # Fitting to the averaged profile (rather than raw epochs):
  # 1. Aligns acrophase with what users see in the Activity Profile plot
  # 2. Is more robust to day-to-day variability and noise
  # 3. Matches the van Someren (1999) methodology used for L5/M10
  #
  # References:
  # - Nelson W et al. (1979). Chronobiologia, 6(4):305-323
  # - Cornelissen G. (2014). Theor Biol Med Model, 11:16
  # - van Hees VT et al. (2015). PLoS ONE - GGIR methodology

  if (length(counts) != length(timestamps)) {
    stop("counts and timestamps must have same length")
  }

  # Apply wear time filter
  if (!is.null(wear_time)) {
    valid <- wear_time & !is.na(counts)
  } else {
    valid <- !is.na(counts)
  }

  if (sum(valid) < 48) {  # Need at least 2 full periods
    return(list(
      mesor = NA_real_,
      amplitude = NA_real_,
      acrophase = NA_real_,
      acrophase_time = NA_character_,
      r_squared = NA_real_,
      f_statistic = NA_real_,
      p_value = NA_real_,
      class = "canhrActi_cosinor"
    ))
  }

  y <- counts[valid]
  ts <- timestamps[valid]
  n_raw <- length(y)

  # Create averaged 24-hour profile (GGIR method)
  # This averages activity at each hour of day across all days
  hour_of_day <- as.numeric(format(ts, "%H")) +
                 as.numeric(format(ts, "%M")) / 60

  # Create 24 hourly bins (0-1, 1-2, ..., 23-24)
  hour_bin <- floor(hour_of_day)

  # Calculate mean activity for each hour
  hourly_means <- tapply(y, hour_bin, mean, na.rm = TRUE)
  hourly_counts <- tapply(y, hour_bin, function(x) sum(!is.na(x)))

  # Create profile data frame
  hours_present <- as.integer(names(hourly_means))
  profile_y <- as.numeric(hourly_means)
  profile_n <- as.numeric(hourly_counts)

  # Require at least 12 hours with data for reliable fit
  if (length(hours_present) < 12) {
    return(list(
      mesor = NA_real_,
      amplitude = NA_real_,
      acrophase = NA_real_,
      acrophase_time = NA_character_,
      r_squared = NA_real_,
      f_statistic = NA_real_,
      p_value = NA_real_,
      class = "canhrActi_cosinor"
    ))
  }

  # Fit cosinor to the averaged profile
  # Using hour of day (0-24) directly means acrophase IS clock time
  t_hours <- hours_present + 0.5  # Center of each hour bin

  # Angular frequency for 24-hour period
  omega <- 2 * pi / period

  # Design matrix: [1, cos(ωt), sin(ωt)]
  cos_term <- cos(omega * t_hours)
  sin_term <- sin(omega * t_hours)

  # Weighted least squares (weight by number of observations per hour)
  # This gives more weight to hours with more data
  weights <- sqrt(profile_n)  # sqrt for variance stabilization

  X <- cbind(1, cos_term, sin_term)
  Xw <- X * weights
  yw <- profile_y * weights

  fit <- tryCatch({
    lm.fit(Xw, yw)
  }, error = function(e) NULL)

  if (is.null(fit)) {
    return(list(
      mesor = NA_real_,
      amplitude = NA_real_,
      acrophase = NA_real_,
      acrophase_time = NA_character_,
      r_squared = NA_real_,
      f_statistic = NA_real_,
      p_value = NA_real_,
      class = "canhrActi_cosinor"
    ))
  }

  # Extract coefficients
  beta <- fit$coefficients
  mesor <- beta[1]
  beta1 <- beta[2]  # cos coefficient
  beta2 <- beta[3]  # sin coefficient

  # Calculate amplitude and acrophase
  amplitude <- sqrt(beta1^2 + beta2^2)

  # Acrophase calculation - CORRECTED FORMULA
  #
  # The linearized model is: Y(t) = M + β₁·cos(ωt) + β₂·sin(ωt)
  # This equals: Y(t) = M + A·cos(ωt - φ)
  #
  # Where: A = sqrt(β₁² + β₂²)
  #        cos(φ) = β₁/A
  #        sin(φ) = β₂/A
  #        φ = atan2(β₂, β₁)  [NO negative sign!]
  #
  # Peak occurs when cos(ωt - φ) = 1, i.e., ωt - φ = 0
  # So t_peak = φ/ω = φ × T/(2π)
  #
  # References: Cornelissen (2014), Refinetti et al. (2007)
  acrophase_rad <- atan2(beta2, beta1)

  # Convert to hours (0-24 scale)
  # Since t is already in hours (0-24), acrophase IS clock time
  acrophase <- (acrophase_rad * period / (2 * pi)) %% period
  if (acrophase < 0) acrophase <- acrophase + period

  # Calculate fit statistics on the averaged profile.
  # The model is fit by weighted least squares (weights = sqrt(profile_n)), so the
  # total sum of squares MUST be taken about the WEIGHTED mean for the
  # decomposition SS_total = SS_model + SS_resid to hold (and R^2 in [0,1] and the
  # F-test to be valid). Using the unweighted mean(profile_y) here would break the
  # decomposition and mis-scale r_squared/percent_rhythm/f_statistic/p_value.
  y_pred <- mesor + amplitude * cos(omega * t_hours - acrophase_rad)
  ybar_w <- sum(weights^2 * profile_y) / sum(weights^2)  # weighted mean baseline
  ss_total <- sum(weights^2 * (profile_y - ybar_w)^2)
  ss_resid <- sum(weights^2 * (profile_y - y_pred)^2)
  r_squared <- if (ss_total > 0) 1 - ss_resid / ss_total else NA_real_

  # F-statistic and p-value (zero-amplitude test)
  n <- length(profile_y)
  df_model <- 2
  df_resid <- n - 3
  if (df_resid > 0) {
    ms_model <- (ss_total - ss_resid) / df_model
    ms_resid <- ss_resid / df_resid
    f_stat <- if (ms_resid > 0) ms_model / ms_resid else NA_real_
    p_value <- 1 - pf(f_stat, df_model, df_resid)
  } else {
    f_stat <- NA_real_
    p_value <- NA_real_
  }

  # Calculate confidence intervals (delta method, Cornelissen 2014)
  # Variance-covariance matrix of beta coefficients
  mse <- if (df_resid > 0) ss_resid / df_resid else NA_real_
  if (!is.na(mse) && mse > 0) {
    XtX_inv <- tryCatch(solve(t(Xw) %*% Xw), error = function(e) NULL)
    if (!is.null(XtX_inv)) {
      vcov_beta <- mse * XtX_inv

      # SE for MESOR
      se_mesor <- sqrt(vcov_beta[1, 1])

      # SE for amplitude (delta method)
      var_amp <- (beta1^2 * vcov_beta[2, 2] + beta2^2 * vcov_beta[3, 3] +
                  2 * beta1 * beta2 * vcov_beta[2, 3]) / (amplitude^2)
      se_amplitude <- sqrt(max(0, var_amp))

      # SE for acrophase in hours (delta method)
      var_phi <- (beta2^2 * vcov_beta[2, 2] + beta1^2 * vcov_beta[3, 3] -
                  2 * beta1 * beta2 * vcov_beta[2, 3]) / (amplitude^4)
      se_acrophase_rad <- sqrt(max(0, var_phi))
      se_acrophase <- se_acrophase_rad * period / (2 * pi)

      # 95% CIs
      t_crit <- if (df_resid > 0) qt(0.975, df_resid) else 1.96
      ci_mesor <- c(mesor - t_crit * se_mesor, mesor + t_crit * se_mesor)
      ci_amplitude <- c(max(0, amplitude - t_crit * se_amplitude),
                        amplitude + t_crit * se_amplitude)
      ci_acrophase <- c((acrophase - t_crit * se_acrophase) %% 24,
                        (acrophase + t_crit * se_acrophase) %% 24)
    } else {
      se_mesor <- se_amplitude <- se_acrophase <- NA_real_
      ci_mesor <- ci_amplitude <- ci_acrophase <- c(NA_real_, NA_real_)
    }
  } else {
    se_mesor <- se_amplitude <- se_acrophase <- NA_real_
    ci_mesor <- ci_amplitude <- ci_acrophase <- c(NA_real_, NA_real_)
  }

  # Format acrophase time
  acro_hours <- floor(acrophase)
  acro_mins <- round((acrophase - acro_hours) * 60)
  if (acro_mins == 60) {
    acro_hours <- (acro_hours + 1) %% 24
    acro_mins <- 0
  }
  acrophase_time <- sprintf("%02d:%02d", acro_hours, acro_mins)

  # Calculate percent rhythm (effect size)
  percent_rhythm <- r_squared * 100

  # Number of days analyzed
  n_days <- length(unique(as.Date(ts)))

  result <- list(
    # Core cosinor parameters
    mesor = round(mesor, 2),
    amplitude = round(amplitude, 2),
    acrophase = round(acrophase, 2),
    acrophase_time = acrophase_time,

    # Standard errors
    se_mesor = round(se_mesor, 2),
    se_amplitude = round(se_amplitude, 2),
    se_acrophase = round(se_acrophase, 2),

    # 95% Confidence intervals
    ci_mesor = round(ci_mesor, 2),
    ci_amplitude = round(ci_amplitude, 2),
    ci_acrophase = round(ci_acrophase, 2),

    # Model fit statistics
    r_squared = round(r_squared, 4),
    percent_rhythm = round(percent_rhythm, 1),
    f_statistic = round(f_stat, 2),
    p_value = signif(p_value, 3),
    rhythm_significant = !is.na(p_value) && p_value < 0.05,

    # Metadata
    period = period,
    n_observations = n_raw,
    n_profile_hours = length(profile_y),
    n_days = n_days,
    method = "averaged_profile",
    class = "canhrActi_cosinor"
  )

  class(result) <- "canhrActi_cosinor"
  result
}


#' Multi-Component Cosinor Analysis with Harmonics
#'
#' Fits a multi-component cosinor model including fundamental (24h) and harmonics
#' (12h, 8h, etc.) to better capture complex activity patterns like bimodal rhythms.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct vector of timestamps
#' @param harmonics Vector of periods to include (default: c(24, 12) for 24h + 12h)
#' @param wear_time Optional logical vector indicating valid wear time
#'
#' @return A list with class "canhrActi_cosinor_extended" containing:
#'   \describe{
#'     \item{mesor}{Rhythm-adjusted mean}
#'     \item{components}{Data frame with amplitude, acrophase, acrophase_time for each harmonic}
#'     \item{dominant_period}{Period with largest amplitude}
#'     \item{dominant_acrophase}{Acrophase of dominant component}
#'     \item{r_squared}{Model R-squared (should be higher than single-component)}
#'     \item{r_squared_improvement}{Improvement over single 24h cosinor}
#'   }
#'
#' @details
#' The multi-component model is:
#'
#' \code{Y(t) = M + sum_k[A_k * cos(2*pi*t/T_k - phi_k)]}
#'
#' Where T_k are the periods (24h, 12h, 8h, etc.)
#'
#' This is useful for:
#' \itemize{
#'   \item Bimodal patterns (morning + evening peaks) - captured by 12h harmonic
#'   \item Complex daily routines with multiple activity bouts
#'   \item Shift workers or irregular schedules
#' }
#'
#' @references
#' Cornelissen G. (2014). Cosinor-based rhythmometry. Theor Biol Med Model, 11:16.
#' Refinetti R, et al. (2007). Procedures for numerical analysis of circadian rhythms.
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' extended <- cosinor.extended(
#'   results$epoch_data$axis1,
#'   results$epoch_data$timestamp,
#'   harmonics = c(24, 12, 8)
#' )
#' print(extended)
#' }
#'
#' @export
cosinor.extended <- function(counts, timestamps, harmonics = c(24, 12),
                              wear_time = NULL) {

  if (length(counts) != length(timestamps)) {
    stop("counts and timestamps must have same length")
  }

  # Validate harmonics
  harmonics <- sort(unique(harmonics), decreasing = TRUE)
  if (length(harmonics) < 1 || any(harmonics <= 0)) {
    stop("harmonics must be positive periods (e.g., c(24, 12, 8))")
  }

  # Apply wear time filter
  if (!is.null(wear_time)) {
    valid <- wear_time & !is.na(counts)
  } else {
    valid <- !is.na(counts)
  }

  if (sum(valid) < 48) {
    return(.empty.extended.cosinor.result(harmonics))
  }

  y <- counts[valid]
  ts <- timestamps[valid]
  n_raw <- length(y)

  # Create averaged 24-hour profile (same as single-component cosinor)
  hour_of_day <- as.numeric(format(ts, "%H")) +
                 as.numeric(format(ts, "%M")) / 60
  hour_bin <- floor(hour_of_day)

  hourly_means <- tapply(y, hour_bin, mean, na.rm = TRUE)
  hourly_counts <- tapply(y, hour_bin, function(x) sum(!is.na(x)))

  hours_present <- as.integer(names(hourly_means))
  profile_y <- as.numeric(hourly_means)
  profile_n <- as.numeric(hourly_counts)

  if (length(hours_present) < 12) {
    return(.empty.extended.cosinor.result(harmonics))
  }

  t_hours <- hours_present + 0.5  # Center of each hour bin
  weights <- sqrt(profile_n)

  # Build design matrix with multiple harmonics
  # Y = M + sum_k[beta_{2k-1}*cos(omega_k*t) + beta_{2k}*sin(omega_k*t)]
  X <- matrix(1, nrow = length(t_hours), ncol = 1)  # Intercept (MESOR)

  for (period in harmonics) {
    omega <- 2 * pi / period
    X <- cbind(X, cos(omega * t_hours), sin(omega * t_hours))
  }

  # Weighted least squares
  Xw <- X * weights
  yw <- profile_y * weights

  fit <- tryCatch({
    lm.fit(Xw, yw)
  }, error = function(e) NULL)

  if (is.null(fit)) {
    return(.empty.extended.cosinor.result(harmonics))
  }

  # Extract coefficients
  beta <- fit$coefficients
  mesor <- beta[1]

  # Extract amplitude and acrophase for each harmonic
  components <- data.frame(
    period = harmonics,
    amplitude = NA_real_,
    acrophase = NA_real_,
    acrophase_time = NA_character_,
    relative_power = NA_real_,
    stringsAsFactors = FALSE
  )

  total_power <- 0
  for (i in seq_along(harmonics)) {
    idx_cos <- 2 * i
    idx_sin <- 2 * i + 1

    beta1 <- beta[idx_cos]
    beta2 <- beta[idx_sin]

    amp <- sqrt(beta1^2 + beta2^2)
    acro_rad <- atan2(beta2, beta1)
    acro_hours <- (acro_rad * harmonics[i] / (2 * pi)) %% harmonics[i]
    if (acro_hours < 0) acro_hours <- acro_hours + harmonics[i]

    # For non-24h harmonics, convert to clock time of first peak in 24h
    if (harmonics[i] < 24) {
      # Find all peaks within 24 hours
      n_peaks <- 24 / harmonics[i]
      peaks <- (acro_hours + (0:(n_peaks - 1)) * harmonics[i]) %% 24
      # Report the earliest peak
      acro_hours_24 <- min(peaks)
    } else {
      acro_hours_24 <- acro_hours %% 24
    }

    # Format time
    acro_h <- floor(acro_hours_24)
    acro_m <- round((acro_hours_24 - acro_h) * 60)
    if (acro_m == 60) { acro_h <- (acro_h + 1) %% 24; acro_m <- 0 }

    components$amplitude[i] <- round(amp, 2)
    components$acrophase[i] <- round(acro_hours_24, 2)
    components$acrophase_time[i] <- sprintf("%02d:%02d", acro_h, acro_m)

    total_power <- total_power + amp^2
  }

  # Calculate relative power (% contribution of each harmonic)
  if (total_power > 0) {
    components$relative_power <- round(100 * components$amplitude^2 / total_power, 1)
  }

  # Find dominant component
  dominant_idx <- which.max(components$amplitude)
  dominant_period <- components$period[dominant_idx]
  dominant_amplitude <- components$amplitude[dominant_idx]
  dominant_acrophase <- components$acrophase[dominant_idx]
  dominant_acrophase_time <- components$acrophase_time[dominant_idx]

  # Calculate R-squared for multi-component model
  y_pred <- mesor
  for (i in seq_along(harmonics)) {
    omega <- 2 * pi / harmonics[i]
    acro_rad <- atan2(beta[2*i + 1], beta[2*i])
    y_pred <- y_pred + components$amplitude[i] * cos(omega * t_hours - acro_rad)
  }

  # Use the WEIGHTED mean as the SS-total baseline to match the weighted least
  # squares fit (see cosinor.analysis); the unweighted mean would break the
  # SS decomposition and mis-scale r_squared / percent_rhythm / F-test.
  ybar_w <- sum(weights^2 * profile_y) / sum(weights^2)
  ss_total <- sum(weights^2 * (profile_y - ybar_w)^2)
  ss_resid <- sum(weights^2 * (profile_y - y_pred)^2)
  r_squared <- if (ss_total > 0) 1 - ss_resid / ss_total else NA_real_

  # Compare to single-component 24h cosinor
  single_cosinor <- cosinor.analysis(counts, timestamps, period = 24, wear_time = wear_time)
  r_squared_single <- if (!is.null(single_cosinor) && !is.na(single_cosinor$r_squared)) {
    single_cosinor$r_squared
  } else {
    NA_real_
  }
  r_squared_improvement <- if (!is.na(r_squared_single)) {
    round((r_squared - r_squared_single) * 100, 1)
  } else {
    NA_real_
  }

  # F-test for overall model
  n <- length(profile_y)
  df_model <- 2 * length(harmonics)
  df_resid <- n - df_model - 1
  if (df_resid > 0 && ss_resid > 0) {
    ms_model <- (ss_total - ss_resid) / df_model
    ms_resid <- ss_resid / df_resid
    f_stat <- if (ms_resid > 0) ms_model / ms_resid else NA_real_
    p_value <- 1 - pf(f_stat, df_model, df_resid)
  } else {
    f_stat <- NA_real_
    p_value <- NA_real_
  }

  # Pattern classification based on harmonic contributions
  pattern_type <- .classify.rhythm.pattern(components)

  n_days <- length(unique(as.Date(ts)))

  result <- list(
    # Core results
    mesor = round(mesor, 2),
    components = components,

    # Dominant harmonic
    dominant_period = dominant_period,
    dominant_amplitude = dominant_amplitude,
    dominant_acrophase = dominant_acrophase,
    dominant_acrophase_time = dominant_acrophase_time,

    # Combined metrics (for compatibility with single-component)
    amplitude = dominant_amplitude,  # Dominant amplitude
    acrophase = dominant_acrophase,
    acrophase_time = dominant_acrophase_time,

    # Model fit
    r_squared = round(r_squared, 4),
    r_squared_single = round(r_squared_single, 4),
    r_squared_improvement = r_squared_improvement,
    percent_rhythm = round(r_squared * 100, 1),
    f_statistic = round(f_stat, 2),
    p_value = signif(p_value, 3),

    # Pattern interpretation
    pattern_type = pattern_type,
    is_bimodal = any(harmonics == 12) && components$relative_power[components$period == 12] > 15,

    # Metadata
    harmonics = harmonics,
    n_observations = n_raw,
    n_days = n_days,
    method = "multi_component_cosinor"
  )

  class(result) <- c("canhrActi_cosinor_extended", "list")
  result
}


#' Helper: Empty extended cosinor result
#' @keywords internal
.empty.extended.cosinor.result <- function(harmonics) {
  components <- data.frame(
    period = harmonics,
    amplitude = NA_real_,
    acrophase = NA_real_,
    acrophase_time = NA_character_,
    relative_power = NA_real_,
    stringsAsFactors = FALSE
  )

  result <- list(
    mesor = NA_real_,
    components = components,
    dominant_period = NA_real_,
    dominant_amplitude = NA_real_,
    dominant_acrophase = NA_real_,
    dominant_acrophase_time = NA_character_,
    amplitude = NA_real_,
    acrophase = NA_real_,
    acrophase_time = NA_character_,
    r_squared = NA_real_,
    r_squared_single = NA_real_,
    r_squared_improvement = NA_real_,
    percent_rhythm = NA_real_,
    f_statistic = NA_real_,
    p_value = NA_real_,
    pattern_type = NA_character_,
    is_bimodal = NA,
    harmonics = harmonics,
    n_observations = 0L,
    n_days = 0L,
    method = "multi_component_cosinor"
  )

  class(result) <- c("canhrActi_cosinor_extended", "list")
  result
}


#' Helper: Classify rhythm pattern based on harmonic contributions
#' @keywords internal
.classify.rhythm.pattern <- function(components) {
  if (all(is.na(components$relative_power))) {
    return(NA_character_)
  }

  # Get relative powers
  p24 <- components$relative_power[components$period == 24]
  p12 <- components$relative_power[components$period == 12]
  p8  <- if (8 %in% components$period) components$relative_power[components$period == 8] else 0

  if (is.na(p24)) p24 <- 0
  if (is.na(p12)) p12 <- 0

  # Classification rules (short labels for UI display)
  if (p24 >= 70) {
    "Strong 24h"
  } else if (p24 >= 50 && p12 < 25) {
    "Moderate 24h"
  } else if (p12 >= 30) {
    "Bimodal"
  } else if (p24 >= 40 && p12 >= 15) {
    "Mixed"
  } else if (p8 >= 20) {
    "Complex"
  } else {
    "Irregular"
  }
}


#' Print method for extended cosinor analysis
#' @param x canhrActi_cosinor_extended object
#' @param ... Additional arguments (ignored)
#' @export
print.canhrActi_cosinor_extended <- function(x, ...) {
  cat("Multi-Component Cosinor Analysis\n\n")

  if (is.na(x$mesor)) {
    cat("Insufficient data for cosinor analysis\n")
    return(invisible(x))
  }

  cat(sprintf("MESOR (mean):     %.2f\n", x$mesor))
  cat(sprintf("Pattern type:     %s\n", x$pattern_type))
  cat(sprintf("R-squared:        %.4f (%.1f%% of variance explained)\n",
              x$r_squared, x$percent_rhythm))

  if (!is.na(x$r_squared_improvement)) {
    cat(sprintf("Improvement:      +%.1f%% over single 24h cosinor\n", x$r_squared_improvement))
  }

  cat("\nHarmonic Components:\n")
  for (i in seq_len(nrow(x$components))) {
    comp <- x$components[i, ]
    dominant_marker <- if (comp$period == x$dominant_period) " *DOMINANT*" else ""
    cat(sprintf("  %dh: Amplitude=%.1f, Peak=%s (%.1fh), Power=%.1f%%%s\n",
                comp$period, comp$amplitude, comp$acrophase_time,
                comp$acrophase, comp$relative_power, dominant_marker))
  }

  if (x$is_bimodal) {
    cat("\n[!] BIMODAL PATTERN DETECTED - This person has two activity peaks per day\n")
    # Find 12h component peaks
    if (12 %in% x$components$period) {
      acro_12 <- x$components$acrophase[x$components$period == 12]
      peak1 <- acro_12
      peak2 <- (acro_12 + 12) %% 24
      cat(sprintf("    Estimated peaks at: ~%02d:00 and ~%02d:00\n",
                  round(min(peak1, peak2)), round(max(peak1, peak2))))
    }
  }

  cat(sprintf("\nF-statistic: %.2f, p-value: %s\n", x$f_statistic,
              format(x$p_value, scientific = TRUE, digits = 3)))

  invisible(x)
}


#' Print method for cosinor analysis
#' @param x canhrActi_cosinor object
#' @param ... Additional arguments (ignored)
#' @export
print.canhrActi_cosinor <- function(x, ...) {
  cat("Cosinor Analysis Results\n\n")

  if (is.na(x$mesor)) {
    cat("Insufficient data for cosinor analysis\n")
    return(invisible(x))
  }

  cat(sprintf("Period:     %.0f hours\n", x$period))
  cat(sprintf("N obs:      %d (%.1f days)\n", x$n_observations, x$n_days))
  cat("\n")
  cat("Parameters:\n")
  cat(sprintf("  MESOR:      %.2f (rhythm-adjusted mean)\n", x$mesor))
  cat(sprintf("  Amplitude:  %.2f (half peak-to-trough)\n", x$amplitude))
  cat(sprintf("  Acrophase:  %s (%.2f h, time of peak)\n", x$acrophase_time, x$acrophase))
  cat("\n")
  cat("Model Fit:\n")
  cat(sprintf("  R-squared:  %.4f\n", x$r_squared))
  cat(sprintf("  F-statistic: %.2f\n", x$f_statistic))
  cat(sprintf("  P-value:    %s\n", format(x$p_value, scientific = TRUE, digits = 3)))

  invisible(x)
}


#' Calculate Hourly Activity Profile
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#'
#' @return Data frame with hour, mean_counts, sd_counts, se_counts, n
#'
#' @keywords internal
.calculate.hourly.profile <- function(counts, timestamps) {

  hours <- as.POSIXlt(timestamps)$hour

  profile <- data.frame(
    hour = 0:23,
    mean_counts = NA_real_,
    sd_counts = NA_real_,
    se_counts = NA_real_,
    n = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (h in 0:23) {
    idx <- hours == h
    hour_counts <- counts[idx]
    n_valid <- sum(!is.na(hour_counts))

    if (n_valid > 0) {
      profile$mean_counts[h + 1] <- mean(hour_counts, na.rm = TRUE)
      profile$sd_counts[h + 1] <- if (n_valid > 1) sd(hour_counts, na.rm = TRUE) else NA_real_
      profile$se_counts[h + 1] <- if (n_valid > 1) sd(hour_counts, na.rm = TRUE) / sqrt(n_valid) else NA_real_
      profile$n[h + 1] <- n_valid
    }
  }

  # Clean up NaN values
  profile$mean_counts <- ifelse(is.nan(profile$mean_counts), NA_real_, round(profile$mean_counts, 1))
  profile$sd_counts <- ifelse(is.nan(profile$sd_counts), NA_real_, round(profile$sd_counts, 1))
  profile$se_counts <- ifelse(is.nan(profile$se_counts), NA_real_, round(profile$se_counts, 2))

  return(profile)
}


#' Calculate Daily Circadian Metrics
#'
#' @param counts Numeric vector
#' @param timestamps POSIXct
#' @param epoch_length Epoch length in seconds
#'
#' @return Data frame with per-day metrics
#'
#' @keywords internal
.calculate.daily.circadian <- function(counts, timestamps, epoch_length) {

  dates <- as.Date(timestamps)
  unique_dates <- unique(dates)

  # Pre-allocate results
  n_dates <- length(unique_dates)
  daily_stats <- data.frame(
    date = as.character(unique_dates),
    L5 = rep(NA_real_, n_dates),
    L5_start = rep(NA_character_, n_dates),
    M10 = rep(NA_real_, n_dates),
    M10_start = rep(NA_character_, n_dates),
    RA = rep(NA_real_, n_dates),
    stringsAsFactors = FALSE
  )

  # Minimum epochs for 12 hours of data
  epochs_per_hour <- 3600 / epoch_length
  min_epochs_12h <- 12 * epochs_per_hour

  for (i in seq_along(unique_dates)) {
    d <- unique_dates[i]
    day_idx <- dates == d
    day_counts <- counts[day_idx]
    day_timestamps <- timestamps[day_idx]

    # Need at least 12 hours of valid data
    if (sum(!is.na(day_counts)) < min_epochs_12h) {
      next
    }

    l5 <- .calculate.LX.sliding(day_counts, day_timestamps, X = 5, find_minimum = TRUE, epoch_length = epoch_length)
    m10 <- .calculate.LX.sliding(day_counts, day_timestamps, X = 10, find_minimum = FALSE, epoch_length = epoch_length)
    ra <- .calculate.RA(m10$value, l5$value)

    daily_stats$L5[i] <- round(l5$value, 2)
    daily_stats$L5_start[i] <- l5$start_time
    daily_stats$M10[i] <- round(m10$value, 2)
    daily_stats$M10_start[i] <- m10$start_time
    daily_stats$RA[i] <- round(ra, 4)
  }

  return(daily_stats)
}


#' Print Method for Circadian Rhythm Results
#'
#' @param x Object of class 'canhrActi_circadian'
#' @param ... Additional arguments (unused)
#'
#' @export
print.canhrActi_circadian <- function(x, ...) {
  cat("\nCircadian Rhythm Analysis\n\n")

  cat("Data Summary\n")
  cat(sprintf("  Days analyzed:            %d\n", x$n_days_analyzed))
  cat(sprintf("  Valid circadian days:     %d\n", x$n_valid_circadian_days))
  cat(sprintf("  Epoch length:             %d seconds\n", x$epoch_length))

  cat("\nNon-Parametric Metrics (van Someren et al., 1999)\n")
  cat(sprintf("  L5 (least active 5h):     %.2f counts/min, onset %s\n", x$L5, x$L5_start))
  cat(sprintf("  M10 (most active 10h):    %.2f counts/min, onset %s\n", x$M10, x$M10_start))
  cat(sprintf("  L1 (least active 1h):     %.2f counts/min, onset %s\n", x$L1, x$L1_start))
  cat(sprintf("  M1 (most active 1h):      %.2f counts/min, onset %s\n", x$M1, x$M1_start))
  cat(sprintf("  Relative Amplitude (RA):  %.4f (range 0-1, higher=stronger rhythm)\n", x$RA))
  cat(sprintf("  Interdaily Stability (IS): %.4f (range 0-1, higher=more consistent)\n", x$IS))
  cat(sprintf("  Intradaily Variability (IV): %.4f (~0=sine, ~2=noise)\n", x$IV))
  cat(sprintf("  Phi (autocorrelation):    %.4f (higher=more predictable)\n", x$phi))

  cat("\nSleep-Based & Variability Metrics\n")
  if (!is.na(x$SRI)) {
    cat(sprintf("  Sleep Regularity Index:   %.2f (range -100 to 100, higher=more regular)\n", x$SRI))
  } else {
    cat("  Sleep Regularity Index:   Not calculated (requires sleep_state input)\n")
  }
  cat(sprintf("  Onset timing variability: %.2f hours\n", x$onset_timing_variability))
  cat(sprintf("  L5 timing variability:    %.2f hours (circular SD)\n", x$L5_variability_hours))
  cat(sprintf("  M10 timing variability:   %.2f hours (circular SD)\n", x$M10_variability_hours))

  cat("\nReferences: van Someren (1999), Phillips (2017)\n\n")

  invisible(x)
}


# Interpretation helpers for circadian metrics
.interpret.IS <- function(is_val) {
  if (is.na(is_val)) return("")
  if (is_val >= 0.6) return("Strong")
  if (is_val >= 0.4) return("Moderate")
  return("Weak")
}

.interpret.IV <- function(iv_val) {
  if (is.na(iv_val)) return("")
  if (iv_val <= 0.8) return("Stable")
  if (iv_val <= 1.2) return("Moderate")
  return("Fragmented")
}

.interpret.RA <- function(ra_val) {
  if (is.na(ra_val)) return("")
  if (ra_val >= 0.85) return("Robust")
  if (ra_val >= 0.65) return("Moderate")
  return("Dampened")
}


#' Plot Circadian Rhythm Profile
#'
#' Creates publication-quality visualizations of circadian rhythm analysis.
#'
#' @param x canhrActi_circadian object from circadian.rhythm()
#' @param type Type of plot: "profile" (default), "daily", or "all"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return ggplot object (or list of ggplot objects if type="all")
#'
#' @export
plot.canhrActi_circadian <- function(x, type = "profile", ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }

  if (type == "profile" || type == "all") {
    profile <- x$hourly_profile

    p_profile <- ggplot2::ggplot(profile, ggplot2::aes(x = hour, y = mean_counts)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pmax(0, mean_counts - sd_counts),
                     ymax = mean_counts + sd_counts),
        alpha = 0.2, fill = "#0072B2"
      ) +
      ggplot2::geom_line(color = "#0072B2", linewidth = 1.2) +
      ggplot2::geom_point(color = "#0072B2", size = 2) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 23, 3),
        labels = sprintf("%02d:00", seq(0, 23, 3))
      ) +
      ggplot2::labs(
        title = "24-Hour Activity Profile",
        subtitle = sprintf("RA=%.2f (%s) | IS=%.2f (%s) | IV=%.2f (%s)",
                          x$RA, .interpret.RA(x$RA),
                          x$IS, .interpret.IS(x$IS),
                          x$IV, .interpret.IV(x$IV)),
        x = "Time of Day",
        y = "Mean Activity (counts/min)"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    # Add L5/M10 windows
    if (!is.na(x$L5_start_hour) && !is.na(x$M10_start_hour)) {
      l5_end <- (x$L5_start_hour + 5) %% 24
      m10_end <- (x$M10_start_hour + 10) %% 24

      p_profile <- p_profile +
        ggplot2::annotate("rect",
          xmin = x$L5_start_hour, xmax = ifelse(l5_end > x$L5_start_hour, l5_end, 24),
          ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue"
        ) +
        ggplot2::annotate("rect",
          xmin = x$M10_start_hour, xmax = ifelse(m10_end > x$M10_start_hour, m10_end, 24),
          ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "orange"
        )
    }
  }

  if (type == "profile") {
    return(p_profile)
  }

  if (type == "daily" || type == "all") {
    daily <- x$daily_metrics
    daily$date <- as.Date(daily$date)

    p_daily <- ggplot2::ggplot(daily, ggplot2::aes(x = date, y = RA)) +
      ggplot2::geom_line(color = "#0072B2", linewidth = 1) +
      ggplot2::geom_point(color = "#0072B2", size = 2) +
      ggplot2::geom_hline(yintercept = x$RA, linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = "Daily Relative Amplitude",
        subtitle = sprintf("Mean RA = %.3f (dashed line)", x$RA),
        x = "Date",
        y = "Relative Amplitude"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  if (type == "daily") {
    return(p_daily)
  }

  if (type == "all") {
    return(list(profile = p_profile, daily = p_daily))
  }

  stop("Unknown plot type. Use 'profile', 'daily', or 'all'")
}
