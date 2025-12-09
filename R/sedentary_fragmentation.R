#' Sedentary Fragmentation Analysis
#'
#' Functions for analyzing patterns of sedentary behavior accumulation,
#' including bout detection, fragmentation indices, and break analysis.
#'
#' @name sedentary-fragmentation
NULL

#' Detect Sedentary Bouts
#'
#' Identifies continuous periods of sedentary behavior from intensity classifications.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps (same length as intensity)
#' @param wear_time Optional logical vector indicating wear time (TRUE = worn)
#' @param min_bout_length Minimum bout length in minutes to include (default 1)
#' @param epoch_length Epoch length in seconds (default 60)
#'
#' @return A data frame with columns:
#'   \item{bout_id}{Unique identifier for each bout}
#'   \item{start_time}{Start timestamp of the bout}
#'   \item{end_time}{End timestamp of the bout}
#'   \item{duration_min}{Duration in minutes}
#'   \item{start_index}{Starting row index}
#'   \item{end_index}{Ending row index}
#'
#' @export
#'
#' @examples
#' intensity <- factor(c(rep("sedentary", 30), rep("light", 10), rep("sedentary", 20)),
#'                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
#' timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 60)
#' bouts <- detect.sedentary.bouts(intensity, timestamps)
detect.sedentary.bouts <- function(intensity, timestamps, wear_time = NULL,
                                    min_bout_length = 1, epoch_length = 60) {
  # Input validation

if (!is.factor(intensity) && !is.character(intensity)) {
    stop("intensity must be a factor or character vector")
  }
  if (!inherits(timestamps, "POSIXct")) {
    stop("timestamps must be POSIXct")
  }
  if (length(intensity) != length(timestamps)) {
    stop("intensity and timestamps must have the same length")
  }

  n <- length(intensity)
  if (n == 0) {
    return(data.frame(
      bout_id = integer(0),
      start_time = as.POSIXct(character(0)),
      end_time = as.POSIXct(character(0)),
      duration_min = numeric(0),
      start_index = integer(0),
      end_index = integer(0)
    ))
  }

  # Apply wear time filter if provided
  is_sedentary <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    if (length(wear_time) != n) {
      stop("wear_time must have the same length as intensity")
    }
    is_sedentary <- is_sedentary & wear_time
  }

  # Find run-length encoding of sedentary periods
  rle_sed <- rle(is_sedentary)

  # Calculate bout boundaries
  end_indices <- cumsum(rle_sed$lengths)
  start_indices <- c(1, end_indices[-length(end_indices)] + 1)

  # Filter to sedentary bouts only
  sed_mask <- rle_sed$values
  bout_starts <- start_indices[sed_mask]
  bout_ends <- end_indices[sed_mask]

  if (length(bout_starts) == 0) {
    return(data.frame(
      bout_id = integer(0),
      start_time = as.POSIXct(character(0)),
      end_time = as.POSIXct(character(0)),
      duration_min = numeric(0),
      start_index = integer(0),
      end_index = integer(0)
    ))
  }

  # Calculate durations
  bout_lengths <- bout_ends - bout_starts + 1
  duration_min <- bout_lengths * (epoch_length / 60)

  # Filter by minimum bout length
  valid_bouts <- duration_min >= min_bout_length

  if (sum(valid_bouts) == 0) {
    return(data.frame(
      bout_id = integer(0),
      start_time = as.POSIXct(character(0)),
      end_time = as.POSIXct(character(0)),
      duration_min = numeric(0),
      start_index = integer(0),
      end_index = integer(0)
    ))
  }

  data.frame(
    bout_id = seq_len(sum(valid_bouts)),
    start_time = timestamps[bout_starts[valid_bouts]],
    end_time = timestamps[bout_ends[valid_bouts]],
    duration_min = duration_min[valid_bouts],
    start_index = bout_starts[valid_bouts],
    end_index = bout_ends[valid_bouts]
  )
}

#' Calculate Sedentary Fragmentation Metrics
#'
#' Computes comprehensive fragmentation metrics for sedentary behavior patterns.
#' Based on methods from Chastin & Granat (2010) and subsequent literature.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector indicating wear time
#' @param epoch_length Epoch length in seconds (default 60)
#'
#' @return A list with class "canhrActi_fragmentation" containing:
#'   \item{total_sedentary_min}{Total sedentary time in minutes}
#'   \item{total_bouts}{Number of sedentary bouts}
#'   \item{mean_bout_duration}{Mean bout duration in minutes}
#'   \item{median_bout_duration}{Median bout duration in minutes}
#'   \item{max_bout_duration}{Maximum bout duration in minutes}
#'   \item{breaks_total}{Total number of breaks in sedentary time}
#'   \item{breaks_per_sed_hour}{Breaks per hour of sedentary time}
#'   \item{alpha}{Power-law exponent (fragmentation index)}
#'   \item{gini}{Gini coefficient for bout duration inequality}
#'   \item{bout_distribution}{Summary of bout duration distribution}
#'   \item{bouts}{Data frame of individual bouts}
#'
#' @details
#' The alpha (power-law exponent) indicates how sedentary time is accumulated:
#' \itemize{
#'   \item Higher alpha (>2): More fragmented, many short bouts
#'   \item Lower alpha (<1.5): Less fragmented, dominated by long bouts
#' }
#'
#' @references
#' Chastin, S. F., & Granat, M. H. (2010). Methods for objective measure,
#' quantification and analysis of sedentary behaviour and inactivity.
#' Gait & Posture, 31(1), 82-86.
#'
#' @export
#'
#' @examples
#' intensity <- factor(c(rep("sedentary", 30), rep("light", 5), rep("sedentary", 20),
#'                       rep("moderate", 10), rep("sedentary", 15)),
#'                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
#' timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 80)
#' frag <- sedentary.fragmentation(intensity, timestamps)
#' print(frag)
sedentary.fragmentation <- function(intensity, timestamps, wear_time = NULL,
                                     epoch_length = 60) {
  # Get bouts
  bouts <- detect.sedentary.bouts(intensity, timestamps, wear_time,
                                   min_bout_length = 1, epoch_length = epoch_length)

  n <- length(intensity)
  epoch_min <- epoch_length / 60

  # Apply wear time filter for total calculations
  is_sedentary <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    is_sedentary <- is_sedentary & wear_time
    total_wear_min <- sum(wear_time) * epoch_min
  } else {
    total_wear_min <- n * epoch_min
  }

  total_sedentary_min <- sum(is_sedentary) * epoch_min

  # Handle case with no sedentary bouts
  if (nrow(bouts) == 0) {
    result <- list(
      total_sedentary_min = total_sedentary_min,
      total_bouts = 0,
      mean_bout_duration = NA_real_,
      median_bout_duration = NA_real_,
      max_bout_duration = NA_real_,
      breaks_total = 0,
      breaks_per_sed_hour = NA_real_,
      alpha = NA_real_,
      gini = NA_real_,
      sedentary_percent = if (total_wear_min > 0) (total_sedentary_min / total_wear_min) * 100 else NA_real_,
      bout_distribution = data.frame(
        category = c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "30-60 min", ">60 min"),
        count = rep(0, 6),
        percent = rep(0, 6)
      ),
      bouts = bouts
    )
    class(result) <- c("canhrActi_fragmentation", "list")
    return(result)
  }

  durations <- bouts$duration_min
  n_bouts <- length(durations)

  # Basic bout statistics
  mean_bout <- mean(durations)
  median_bout <- median(durations)
  max_bout <- max(durations)

  # Breaks = transitions from sedentary to non-sedentary
  # Number of breaks equals number of bouts (each bout ends with a break, except possibly the last)
  breaks_total <- max(0, n_bouts - 1)
  if (n_bouts > 0) {
    # Check if last epoch is sedentary - if not, count that as a break too
    last_is_sed <- as.character(intensity[n]) == "sedentary"
    if (!is.null(wear_time)) last_is_sed <- last_is_sed && wear_time[n]
    if (!last_is_sed || n_bouts > 1) {
      breaks_total <- n_bouts
    }
  }

  # Breaks per sedentary hour
  sed_hours <- total_sedentary_min / 60
  breaks_per_sed_hour <- if (sed_hours > 0) breaks_total / sed_hours else NA_real_

  # Power-law alpha (Chastin & Granat method)
  alpha <- calculate_alpha(durations)

  # Gini coefficient
  gini <- calculate_gini(durations)

  # Bout duration distribution
  breaks_cat <- c(0, 5, 10, 20, 30, 60, Inf)
  labels_cat <- c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "30-60 min", ">60 min")
  bout_cats <- cut(durations, breaks = breaks_cat, labels = labels_cat, right = TRUE, include.lowest = TRUE)
  bout_table <- table(factor(bout_cats, levels = labels_cat))

  bout_distribution <- data.frame(
    category = labels_cat,
    count = as.integer(bout_table),
    percent = round(as.numeric(bout_table) / n_bouts * 100, 1)
  )

  # Calculate number of unique days in the data
  n_days_analyzed <- length(unique(as.Date(timestamps)))

  result <- list(
    total_sedentary_min = total_sedentary_min,
    total_sedentary_hours = round(total_sedentary_min / 60, 2),
    total_wear_min = total_wear_min,
    total_wear_hours = round(total_wear_min / 60, 2),
    n_days_analyzed = n_days_analyzed,
    total_bouts = n_bouts,
    mean_bout_duration = round(mean_bout, 1),
    median_bout_duration = round(median_bout, 1),
    max_bout_duration = round(max_bout, 1),
    breaks_total = breaks_total,
    breaks_per_sed_hour = round(breaks_per_sed_hour, 2),
    alpha = round(alpha, 3),
    gini = round(gini, 3),
    sedentary_percent = round((total_sedentary_min / total_wear_min) * 100, 1),
    bout_distribution = bout_distribution,
    bouts = bouts
  )

  class(result) <- c("canhrActi_fragmentation", "list")
  result
}

#' Calculate Power-Law Alpha (Fragmentation Index)
#'
#' Estimates the power-law exponent for bout duration distribution using
#' maximum likelihood estimation.
#'
#' @param durations Numeric vector of bout durations
#' @param xmin Minimum value for power-law fitting (default 1)
#'
#' @return Estimated alpha value
#'
#' @details
#' Uses the Hill estimator (MLE) for power-law exponent:
#' alpha = 1 + n / sum(log(x / xmin))
#'
#' @keywords internal
calculate_alpha <- function(durations, xmin = 1) {
  # Filter durations >= xmin
  x <- durations[durations >= xmin]
  n <- length(x)

  if (n < 2) return(NA_real_)

  # Hill estimator (MLE for power-law)
  alpha <- 1 + n / sum(log(x / xmin))

  alpha
}

#' Calculate Gini Coefficient
#'
#' Computes the Gini coefficient for bout duration inequality.
#'
#' @param x Numeric vector of values
#'
#' @return Gini coefficient (0 = perfect equality, 1 = perfect inequality)
#'
#' @keywords internal
calculate_gini <- function(x) {
  x <- sort(x)
  n <- length(x)

  if (n < 2 || sum(x) == 0) return(NA_real_)

  # Gini formula
  numerator <- 2 * sum(seq_len(n) * x) - (n + 1) * sum(x)
  denominator <- n * sum(x)

  numerator / denominator
}

#' Print Method for Fragmentation Results
#'
#' @param x A canhrActi_fragmentation object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.canhrActi_fragmentation <- function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("   SEDENTARY FRAGMENTATION ANALYSIS\n")
  cat("========================================\n\n")

  cat(sprintf("Days Analyzed:           %d\n", x$n_days_analyzed))
  cat(sprintf("Total Wear Time:         %.1f hours\n", x$total_wear_hours))
  cat(sprintf("Total Sedentary Time:    %.1f hours (%.1f%%)\n",
              x$total_sedentary_hours, x$sedentary_percent))
  cat("\n")
  cat(sprintf("Number of Bouts:         %d\n", x$total_bouts))
  cat(sprintf("Mean Bout Duration:      %.1f min\n", x$mean_bout_duration))
  cat(sprintf("Median Bout Duration:    %.1f min\n", x$median_bout_duration))
  cat(sprintf("Max Bout Duration:       %.1f min\n", x$max_bout_duration))
  cat("\n")
  cat(sprintf("Total Breaks:            %d\n", x$breaks_total))
  cat(sprintf("Breaks per Sed Hour:     %.2f\n", x$breaks_per_sed_hour))
  cat("\n")
  cat(sprintf("Alpha (Power-Law):       %.3f\n", x$alpha))
  cat(sprintf("Gini Coefficient:        %.3f\n", x$gini))
  cat("\n")
  cat("Bout Duration Distribution:\n")
  print(x$bout_distribution, row.names = FALSE)
  cat("\n")

  invisible(x)
}

#' Plot Sedentary Fragmentation
#'
#' Creates visualizations of sedentary bout patterns.
#'
#' @param x A canhrActi_fragmentation object
#' @param type Type of plot: "distribution" (default), "accumulation", or "timeline"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object
#'
#' @export
plot.canhrActi_fragmentation <- function(x, type = "distribution", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  if (type == "distribution") {
    # Bout duration distribution bar chart
    df <- x$bout_distribution
    df$category <- factor(df$category, levels = df$category)

    ggplot2::ggplot(df, ggplot2::aes(x = category, y = count)) +
      ggplot2::geom_col(fill = "#3c8dbc", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 3.5) +
      ggplot2::labs(
        title = "Sedentary Bout Duration Distribution",
        subtitle = sprintf("Total: %d bouts | Mean: %.1f min | Median: %.1f min",
                          x$total_bouts, x$mean_bout_duration, x$median_bout_duration),
        x = "Bout Duration Category",
        y = "Number of Bouts"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

  } else if (type == "accumulation") {
    # Cumulative sedentary time accumulation curve
    if (nrow(x$bouts) == 0) {
      stop("No bouts available for accumulation plot")
    }

    bouts_sorted <- x$bouts[order(x$bouts$duration_min, decreasing = TRUE), ]
    bouts_sorted$cumulative_time <- cumsum(bouts_sorted$duration_min)
    bouts_sorted$cumulative_percent <- bouts_sorted$cumulative_time / x$total_sedentary_min * 100
    bouts_sorted$bout_rank <- seq_len(nrow(bouts_sorted))
    bouts_sorted$bout_percent <- bouts_sorted$bout_rank / nrow(bouts_sorted) * 100

    ggplot2::ggplot(bouts_sorted, ggplot2::aes(x = bout_percent, y = cumulative_percent)) +
      ggplot2::geom_line(color = "#3c8dbc", linewidth = 1.2) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = "Sedentary Time Accumulation Curve",
        subtitle = sprintf("Gini = %.3f (0 = equal bouts, 1 = one dominant bout)", x$gini),
        x = "% of Bouts (longest to shortest)",
        y = "% of Total Sedentary Time"
      ) +
      ggplot2::scale_x_continuous(limits = c(0, 100)) +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  } else if (type == "histogram") {
    # Histogram of bout durations
    if (nrow(x$bouts) == 0) {
      stop("No bouts available for histogram")
    }

    ggplot2::ggplot(x$bouts, ggplot2::aes(x = duration_min)) +
      ggplot2::geom_histogram(binwidth = 5, fill = "#3c8dbc", alpha = 0.8, color = "white") +
      ggplot2::labs(
        title = "Sedentary Bout Duration Histogram",
        subtitle = sprintf("Alpha = %.3f (higher = more fragmented)", x$alpha),
        x = "Bout Duration (minutes)",
        y = "Frequency"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  } else {
    stop("Unknown plot type. Use 'distribution', 'accumulation', or 'histogram'")
  }
}

#' Summarize Sedentary Breaks by Hour
#'
#' Calculates hourly patterns of sedentary breaks throughout the day.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector indicating wear time
#'
#' @return A data frame with hourly break statistics
#'
#' @export
sedentary.breaks.hourly <- function(intensity, timestamps, wear_time = NULL) {
  if (length(intensity) != length(timestamps)) {
    stop("intensity and timestamps must have same length")
  }

  # Create data frame
  df <- data.frame(
    hour = as.integer(format(timestamps, "%H")),
    is_sedentary = as.character(intensity) == "sedentary"
  )

  if (!is.null(wear_time)) {
    df$is_sedentary <- df$is_sedentary & wear_time
    df$wear <- wear_time
  } else {
    df$wear <- TRUE
  }

  # Detect transitions (breaks)
  df$break_start <- c(FALSE, diff(df$is_sedentary) == -1)  # Transition from sedentary to non-sedentary

  # Aggregate by hour
  hourly <- aggregate(
    cbind(sedentary_min = is_sedentary, breaks = break_start, wear_min = wear) ~ hour,
    data = df,
    FUN = sum
  )

  hourly$breaks_per_hour <- ifelse(hourly$sedentary_min > 0,
                                    hourly$breaks / (hourly$sedentary_min / 60),
                                    0)

  hourly
}
