#' Circadian Rhythm Analysis
#'
#' Comprehensive circadian rhythm analysis including non-parametric (L5, M10, IS, IV, RA)
#' and parametric (cosinor) methods for characterizing 24-hour activity patterns.
#'
#' @param counts Numeric vector of activity counts (typically axis1 or VM)
#' @param timestamps POSIXct vector of epoch timestamps
#' @param wear_time Logical vector indicating wear time (optional)
#' @param epoch_length Numeric. Epoch length in seconds (default: 60)
#'
#' @return List with class 'canhrActi_circadian' containing:
#'   \itemize{
#'     \item \code{L5} - Least active 5-hour average
#'     \item \code{L5_start} - Start time of L5 window
#'     \item \code{M10} - Most active 10-hour average
#'     \item \code{M10_start} - Start time of M10 window
#'     \item \code{RA} - Relative amplitude (M10-L5)/(M10+L5)
#'     \item \code{IS} - Interdaily stability (0-1)
#'     \item \code{IV} - Intradaily variability (0-2+)
#'     \item \code{cosinor} - Cosinor analysis results (MESOR, amplitude, acrophase)
#'     \item \code{hourly_profile} - Average activity by hour of day
#'     \item \code{daily_metrics} - Per-day circadian metrics
#'   }
#'
#' @details
#' \strong{Non-Parametric Metrics (van Someren et al., 1999):}
#' \itemize{
#'   \item \strong{L5} = Average activity during the least active 5 consecutive hours.
#'     Typically corresponds to nighttime/sleep.
#'   \item \strong{M10} = Average activity during the most active 10 consecutive hours.
#'     Typically corresponds to daytime activity.
#'   \item \strong{RA} = Relative amplitude = (M10 - L5) / (M10 + L5).
#'     Higher values indicate stronger day-night differences.
#'   \item \strong{IS} = Interdaily stability. Measures consistency of activity patterns
#'     across days. Ranges 0-1, higher = more stable rhythm.
#'   \item \strong{IV} = Intradaily variability. Measures fragmentation of activity.
#'     Approaches 0 for perfect sine wave, 2 for Gaussian noise.
#' }
#'
#' \strong{Cosinor Analysis:}
#' Fits: Y(t) = MESOR + Amplitude * cos(2*pi*t/24 + Acrophase)
#' \itemize{
#'   \item \strong{MESOR} = Midline Estimating Statistic Of Rhythm (rhythm-adjusted mean)
#'   \item \strong{Amplitude} = Half the peak-to-trough difference
#'   \item \strong{Acrophase} = Time of peak activity (hours from midnight)
#' }
#'
#' @references
#' Witting W, et al. (1990). Alterations in the circadian rest-activity rhythm
#' in aging and Alzheimer's disease. Biol Psychiatry, 27(6):563-572.
#'
#' Van Someren EJ, et al. (1999). Bright light therapy: improved sensitivity to
#' its effects on rest-activity rhythms in Alzheimer patients by application
#' of nonparametric methods. Chronobiol Int, 16(4):505-518.
#'
#' Nelson W, et al. (1979). Methods for cosinor-rhythmometry.
#' Chronobiologia, 6(4):305-323.
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' circadian <- circadian.rhythm(results$epoch_data$axis1,
#'                               results$epoch_data$timestamp,
#'                               results$epoch_data$wear_time)
#' print(circadian)
#'
#' # Plot 24-hour activity profile
#' plot(circadian$hourly_profile$hour, circadian$hourly_profile$mean_counts,
#'      type = "l", xlab = "Hour", ylab = "Mean Counts")
#' }
#'
#' @export
circadian.rhythm <- function(counts,
                             timestamps,
                             wear_time = NULL,
                             epoch_length = 60) {

  if (length(counts) != length(timestamps)) {
    stop("counts and timestamps must have same length")
  }

  if (length(counts) == 0) {
    stop("No data provided")
  }

  # Apply wear time filter if provided
  if (!is.null(wear_time)) {
    if (length(wear_time) != length(counts)) {
      stop("wear_time must have same length as counts")
    }
    counts[!wear_time] <- NA
  }

  # Calculate hourly averages
  hourly_profile <- .calculate.hourly.profile(counts, timestamps)

  # Calculate L5 and M10
  l5_result <- .calculate.LX(counts, timestamps, X = 5, find_minimum = TRUE)
  m10_result <- .calculate.LX(counts, timestamps, X = 10, find_minimum = FALSE)

  # Relative Amplitude
  ra <- .calculate.RA(m10_result$value, l5_result$value)

  # Interdaily Stability and Intradaily Variability
  is_iv <- .calculate.IS.IV(counts, timestamps, epoch_length)

  # Cosinor analysis
  cosinor <- .cosinor.analysis(counts, timestamps)

  # Daily metrics
  daily_metrics <- .calculate.daily.circadian(counts, timestamps, epoch_length)

  # Count days with valid circadian data (non-NA L5/M10)
  n_days_analyzed <- length(unique(as.Date(timestamps)))
  n_valid_circadian_days <- sum(!is.na(daily_metrics$L5))

  result <- list(
    L5 = round(l5_result$value, 2),
    L5_start = l5_result$start_time,
    M10 = round(m10_result$value, 2),
    M10_start = m10_result$start_time,
    RA = round(ra, 3),
    IS = round(is_iv$IS, 3),
    IV = round(is_iv$IV, 3),
    cosinor = cosinor,
    hourly_profile = hourly_profile,
    daily_metrics = daily_metrics,
    n_days_analyzed = n_days_analyzed,
    n_valid_days = n_valid_circadian_days
  )

  class(result) <- c("canhrActi_circadian", "list")
  return(result)
}


#' Calculate L5 or M10 (Least/Most Active X Hours)
#'
#' Finds the consecutive X-hour window with lowest (L5) or highest (M10) activity.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#' @param X Number of hours for the window (default: 5 for L5, 10 for M10)
#' @param find_minimum Logical. TRUE for L5 (least active), FALSE for M10 (most active)
#' @return List with value (average) and start_time
#' @keywords internal
.calculate.LX <- function(counts, timestamps, X, find_minimum = TRUE) {

  # Aggregate to hourly data
  hours <- as.POSIXlt(timestamps)$hour
  dates <- as.Date(timestamps)

  # Create hourly means across all days
  # Handle case where no data exists for a particular hour
  hourly_means <- numeric(24)
  for (h in 0:23) {
    idx <- hours == h
    if (sum(idx) > 0 && sum(!is.na(counts[idx])) > 0) {
      hourly_means[h + 1] <- mean(counts[idx], na.rm = TRUE)
    } else {
      hourly_means[h + 1] <- NA_real_
    }
  }

  # Find X consecutive hours with min/max average
  # Use circular search (wraps around midnight)
  best_value <- if (find_minimum) Inf else -Inf
  best_start <- 0

  for (start in 0:23) {
    # Get X consecutive hours (circular)
    hour_indices <- ((start:(start + X - 1)) %% 24) + 1
    window_mean <- mean(hourly_means[hour_indices], na.rm = TRUE)

    # Skip if window_mean is NA or NaN
    if (is.na(window_mean) || is.nan(window_mean)) next

    if (find_minimum && window_mean < best_value) {
      best_value <- window_mean
      best_start <- start
    } else if (!find_minimum && window_mean > best_value) {
      best_value <- window_mean
      best_start <- start
    }
  }

  # Handle case where no valid window was found (best_value still Inf/-Inf)
  if (is.infinite(best_value)) {
    return(list(value = NA_real_, start_time = NA_character_))
  }

  # Format start time
  start_time <- sprintf("%02d:00", best_start)

  return(list(value = best_value, start_time = start_time))
}


#' Calculate Relative Amplitude
#'
#' RA = (M10 - L5) / (M10 + L5)
#'
#' @param M10 Most active 10-hour average
#' @param L5 Least active 5-hour average
#' @return Numeric. Relative amplitude (0-1)
#' @keywords internal
.calculate.RA <- function(M10, L5) {
  if (is.na(M10) || is.na(L5) || (M10 + L5) == 0) {
    return(NA)
  }
  (M10 - L5) / (M10 + L5)
}


#' Calculate Interdaily Stability (IS) and Intradaily Variability (IV)
#'
#' IS measures consistency of activity patterns across days.
#' IV measures fragmentation of the activity rhythm.
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#' @param epoch_length Epoch length in seconds
#' @return List with IS and IV values
#'
#' @details
#' IS Formula: IS = n * sum((Xh - X_mean)^2) / (p * sum((Xi - X_mean)^2))
#' IV Formula: IV = n * sum((Xi - Xi-1)^2) / ((n-1) * sum((Xi - X_mean)^2))
#'
#' Where:
#' - n = total number of data points
#' - p = number of data points per day (typically 24 for hourly data)
#' - Xh = mean activity for hour h across all days
#' - Xi = activity at data point i
#' - X_mean = overall mean activity
#'
#' @references
#' Witting W, et al. (1990). Alterations in the circadian rest-activity rhythm
#' in aging and Alzheimer's disease. Biol Psychiatry, 27(6):563-572.
#'
#' @keywords internal
.calculate.IS.IV <- function(counts, timestamps, epoch_length = 60) {

  # Remove NA values
  valid_idx <- !is.na(counts)
  counts <- counts[valid_idx]
  timestamps <- timestamps[valid_idx]

  if (length(counts) < 48) {  # Need at least 2 days of hourly data
    return(list(IS = NA, IV = NA))
  }

  # Aggregate to hourly data for IS/IV calculation
  hours <- as.POSIXlt(timestamps)$hour
  dates <- as.Date(timestamps)

  # Create hourly sums (60 1-min epochs = 1 hour)
  epochs_per_hour <- 60 / (epoch_length / 60)

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

  # Interdaily Stability (IS): n * sum((Xh - X_mean)^2) / (p * sum((Xi - X_mean)^2))

  # Numerator: variance of hourly means
  between_hour_var <- sum((hourly_means$activity - X_mean)^2, na.rm = TRUE)

  # Denominator: total variance
  total_var <- sum((hourly_data$activity - X_mean)^2, na.rm = TRUE)

  IS <- if (total_var > 0) {
    (n * between_hour_var) / (p * total_var)
  } else NA

  # Bound IS to 0-1
  IS <- max(0, min(1, IS))

  # Intradaily Variability (IV): n * sum((Xi - Xi-1)^2) / ((n-1) * sum((Xi - X_mean)^2))

  # Sort by date and hour
  hourly_data <- hourly_data[order(hourly_data$date, hourly_data$hour), ]

  # Calculate successive differences
  successive_diff_sq <- sum(diff(hourly_data$activity)^2, na.rm = TRUE)

  IV <- if (total_var > 0 && n > 1) {
    (n * successive_diff_sq) / ((n - 1) * total_var)
  } else NA

  return(list(IS = IS, IV = IV))
}


#' Cosinor Analysis
#'
#' Fits a single-component cosinor model to activity data.
#' Y(t) = MESOR + Amplitude * cos(2*pi*t/24 + Acrophase)
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#' @return List with MESOR, amplitude, acrophase, and model fit statistics
#' @keywords internal
.cosinor.analysis <- function(counts, timestamps) {

  # Remove NA values
  valid_idx <- !is.na(counts)
  counts <- counts[valid_idx]
  timestamps <- timestamps[valid_idx]

  if (length(counts) < 24) {
    return(list(
      MESOR = NA,
      amplitude = NA,
      acrophase = NA,
      acrophase_time = NA,
      r_squared = NA,
      p_value = NA
    ))
  }

  # Convert timestamps to hours from midnight (decimal)
  time_hours <- as.numeric(format(timestamps, "%H")) +
                as.numeric(format(timestamps, "%M")) / 60 +
                as.numeric(format(timestamps, "%S")) / 3600

  # Period = 24 hours
  period <- 24

  # Create cosine and sine terms for linearized model
  # Y = M + A*cos(2*pi*t/T + phi)
  # Y = M + beta*cos(2*pi*t/T) + gamma*sin(2*pi*t/T)
  # where beta = A*cos(phi), gamma = -A*sin(phi)

  cos_term <- cos(2 * pi * time_hours / period)
  sin_term <- sin(2 * pi * time_hours / period)

  # Fit linear model
  model <- tryCatch({
    lm(counts ~ cos_term + sin_term)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(model)) {
    return(list(
      MESOR = NA,
      amplitude = NA,
      acrophase = NA,
      acrophase_time = NA,
      r_squared = NA,
      p_value = NA
    ))
  }

  coefs <- coef(model)
  MESOR <- coefs[1]
  beta <- coefs[2]
  gamma <- coefs[3]

  # Calculate amplitude and acrophase
  amplitude <- sqrt(beta^2 + gamma^2)
  acrophase_rad <- atan2(-gamma, beta)

  # Convert acrophase to hours (0-24)
  acrophase_hours <- (acrophase_rad * period / (2 * pi)) %% 24

  # Format as time string
  acrophase_h <- floor(acrophase_hours)
  acrophase_m <- round((acrophase_hours - acrophase_h) * 60)
  acrophase_time <- sprintf("%02d:%02d", acrophase_h, acrophase_m)

  # Model statistics
  model_summary <- summary(model)
  r_squared <- model_summary$r.squared

  # F-test p-value
  f_stat <- model_summary$fstatistic
  if (!is.null(f_stat) && length(f_stat) == 3) {
    p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  } else {
    p_value <- NA
  }

  return(list(
    MESOR = round(MESOR, 2),
    amplitude = round(amplitude, 2),
    acrophase = round(acrophase_hours, 2),
    acrophase_time = acrophase_time,
    r_squared = round(r_squared, 3),
    p_value = signif(p_value, 3)
  ))
}


#' Calculate Hourly Activity Profile
#'
#' @param counts Numeric vector of activity counts
#' @param timestamps POSIXct timestamps
#' @return Data frame with hour, mean_counts, sd_counts, n
#' @keywords internal
.calculate.hourly.profile <- function(counts, timestamps) {

  hours <- as.POSIXlt(timestamps)$hour

  profile <- data.frame(
    hour = 0:23,
    mean_counts = NA_real_,
    sd_counts = NA_real_,
    n = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (h in 0:23) {
    idx <- hours == h
    n_valid <- sum(!is.na(counts[idx]))
    if (sum(idx) > 0 && n_valid > 0) {
      profile$mean_counts[h + 1] <- mean(counts[idx], na.rm = TRUE)
      profile$sd_counts[h + 1] <- if (n_valid > 1) sd(counts[idx], na.rm = TRUE) else NA_real_
      profile$n[h + 1] <- n_valid
    }
  }

  # Replace any NaN with NA for cleaner output
  profile$mean_counts <- ifelse(is.nan(profile$mean_counts), NA_real_, round(profile$mean_counts, 1))
  profile$sd_counts <- ifelse(is.nan(profile$sd_counts), NA_real_, round(profile$sd_counts, 1))

  return(profile)
}


#' Calculate Daily Circadian Metrics
#'
#' @param counts Numeric vector
#' @param timestamps POSIXct
#' @param epoch_length Epoch length in seconds
#' @return Data frame with per-day metrics
#' @keywords internal
.calculate.daily.circadian <- function(counts, timestamps, epoch_length) {

  dates <- as.Date(timestamps)
  unique_dates <- unique(dates)

  daily_stats <- data.frame(
    date = character(),
    L5 = numeric(),
    L5_start = character(),
    M10 = numeric(),
    M10_start = character(),
    RA = numeric(),
    stringsAsFactors = FALSE
  )

  # Calculate minimum epochs needed for 12 hours based on actual epoch length
  epochs_per_hour <- 3600 / epoch_length
  min_epochs_12h <- 12 * epochs_per_hour

  for (d in unique_dates) {
    day_idx <- dates == d
    day_counts <- counts[day_idx]
    day_timestamps <- timestamps[day_idx]

    if (sum(!is.na(day_counts)) < min_epochs_12h) {  # Need at least 12 hours
      daily_stats <- rbind(daily_stats, data.frame(
        date = as.character(d),
        L5 = NA, L5_start = NA,
        M10 = NA, M10_start = NA,
        RA = NA,
        stringsAsFactors = FALSE
      ))
      next
    }

    l5 <- .calculate.LX(day_counts, day_timestamps, X = 5, find_minimum = TRUE)
    m10 <- .calculate.LX(day_counts, day_timestamps, X = 10, find_minimum = FALSE)
    ra <- .calculate.RA(m10$value, l5$value)

    daily_stats <- rbind(daily_stats, data.frame(
      date = as.character(d),
      L5 = round(l5$value, 2),
      L5_start = l5$start_time,
      M10 = round(m10$value, 2),
      M10_start = m10$start_time,
      RA = round(ra, 3),
      stringsAsFactors = FALSE
    ))
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
  cat("\nCircadian Rhythm Analysis\n")
  cat(paste(rep("=", 45), collapse = ""), "\n")

  cat("\nData Summary:\n")
  cat(paste(rep("-", 25), collapse = ""), "\n")
  cat("Days analyzed:", x$n_days_analyzed, "\n")
  cat("Valid circadian days:", x$n_valid_days, "\n")

  cat("\nNon-Parametric Metrics:\n")
  cat(paste(rep("-", 25), collapse = ""), "\n")
  cat("L5 (least active 5h):", x$L5, "counts/min, start:", x$L5_start, "\n")
  cat("M10 (most active 10h):", x$M10, "counts/min, start:", x$M10_start, "\n")
  cat("Relative Amplitude (RA):", x$RA, "\n")
  cat("Interdaily Stability (IS):", x$IS, "(0-1, higher=more stable)\n")
  cat("Intradaily Variability (IV):", x$IV, "(0=sine, 2=noise)\n")

  cat("\nCosinor Analysis:\n")
  cat(paste(rep("-", 25), collapse = ""), "\n")
  cat("MESOR:", x$cosinor$MESOR, "counts/min\n")
  cat("Amplitude:", x$cosinor$amplitude, "counts/min\n")
  cat("Acrophase:", x$cosinor$acrophase_time, "(", x$cosinor$acrophase, "h)\n")
  cat("R-squared:", x$cosinor$r_squared, "\n")
  if (!is.na(x$cosinor$p_value)) {
    cat("P-value:", x$cosinor$p_value, "\n")
  }

  cat("\n")
  invisible(x)
}


#' Plot Circadian Rhythm Profile
#'
#' Creates a 24-hour activity profile plot with L5/M10 windows highlighted.
#'
#' @param x canhrActi_circadian object from circadian.rhythm()
#' @param show_cosinor Logical. Overlay cosinor fit? (default: TRUE)
#' @param ... Additional arguments passed to plot functions
#'
#' @return ggplot object
#'
#' @export
plot.canhrActi_circadian <- function(x, show_cosinor = TRUE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }

  profile <- x$hourly_profile

  # Create base plot
  p <- ggplot2::ggplot(profile, ggplot2::aes(x = hour, y = mean_counts)) +
    ggplot2::geom_line(color = "#2E86AB", linewidth = 1.2) +
    ggplot2::geom_point(color = "#2E86AB", size = 2) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mean_counts - sd_counts,
                   ymax = mean_counts + sd_counts),
      alpha = 0.2, fill = "#2E86AB"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3),
                                 labels = sprintf("%02d:00", seq(0, 23, 3))) +
    ggplot2::labs(
      title = "24-Hour Activity Profile",
      subtitle = sprintf("RA=%.2f, IS=%.2f, IV=%.2f",
                         x$RA, x$IS, x$IV),
      x = "Hour of Day",
      y = "Mean Activity Counts"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # Add cosinor fit if requested
  if (show_cosinor && !is.na(x$cosinor$MESOR)) {
    hours_seq <- seq(0, 23, 0.1)
    cosinor_fit <- x$cosinor$MESOR +
      x$cosinor$amplitude * cos(2 * pi * (hours_seq - x$cosinor$acrophase) / 24)

    cosinor_df <- data.frame(hour = hours_seq, fit = cosinor_fit)
    p <- p + ggplot2::geom_line(
      data = cosinor_df,
      ggplot2::aes(x = hour, y = fit),
      color = "#E94F37", linetype = "dashed", linewidth = 1
    )
  }

  return(p)
}
