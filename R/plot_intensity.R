#' Plot Activity Intensity Heatmap
#'
#' Creates a beautiful heatmap visualization of activity intensity patterns over time.
#'
#' @param x A \code{canhrActi_analysis} object from \code{canhrActi()}
#' @param wear_only Logical. Show only wear time periods? (default: TRUE)
#' @param interactive Logical. Create interactive plotly version? (default: FALSE)
#'
#' @return A ggplot2 object (or plotly if interactive = TRUE)
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' plot_intensity(results)
#' }
#'
#' @export
plot_intensity <- function(x, wear_only = TRUE, interactive = FALSE) {

  if (!inherits(x, "canhrActi_analysis")) {
    stop("Input must be a canhrActi_analysis object from canhrActi()")
  }

  epoch_data <- x$epoch_data

  # Filter to wear time if requested
  if (wear_only) {
    epoch_data <- epoch_data[epoch_data$wear_time, ]
  }

  if (nrow(epoch_data) == 0) {
    stop("No data to plot (check wear_only setting)")
  }

  # Add time components
  epoch_data$hour <- as.numeric(format(epoch_data$timestamp, "%H")) +
                     as.numeric(format(epoch_data$timestamp, "%M")) / 60
  epoch_data$date_label <- format(epoch_data$date, "%b %d")
  epoch_data$day_num <- as.numeric(factor(epoch_data$date))

  # Ensure intensity is factor with correct levels
  intensity_levels <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")
  epoch_data$intensity <- factor(epoch_data$intensity, levels = intensity_levels)

  # Get colors
  colors <- canhrActi_palette("intensity")

  # Create heatmap
  p <- ggplot2::ggplot(epoch_data, ggplot2::aes(x = hour, y = factor(day_num),
                                                  fill = intensity)) +
    ggplot2::geom_tile(color = NA) +
    ggplot2::scale_fill_manual(
      values = colors,
      labels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
      name = "Intensity",
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 6),
      labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(
      labels = setNames(unique(epoch_data$date_label)[order(unique(epoch_data$day_num))],
                       sort(unique(epoch_data$day_num)))
    ) +
    ggplot2::labs(
      title = "Activity Intensity Heatmap",
      subtitle = sprintf("MVPA: %d min/day (%.1f%%) | %s algorithm",
                        round(x$overall_summary$mvpa_minutes / x$overall_summary$total_days),
                        x$overall_summary$mvpa_percent,
                        x$parameters$intensity_algorithm),
      x = "Time of Day",
      y = "Date",
      caption = if (wear_only) "Showing wear time only" else "Showing all epochs"
    ) +
    theme_canhrActi(grid = FALSE) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 11),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.8, "cm")
    )

  # Convert to plotly if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' not installed. Returning static ggplot2 version.")
      return(p)
    }
    p <- plotly::ggplotly(p)
  }

  return(p)
}


#' Plot Intensity Distribution
#'
#' Creates a visual summary of time spent in each intensity level.
#'
#' @param x A \code{canhrActi_analysis} object from \code{canhrActi()}
#' @param style Character. Either "bar" (default) or "pie"
#'
#' @return A ggplot2 object
#'
#' @export
plot_intensity_distribution <- function(x, style = c("bar", "pie")) {

  style <- match.arg(style)

  if (!inherits(x, "canhrActi_analysis")) {
    stop("Input must be a canhrActi_analysis object from canhrActi()")
  }

  # Get intensity summary
  intensity_summary <- x$intensity_summary

  # Ensure proper factor levels
  intensity_levels <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")
  intensity_summary$intensity <- factor(intensity_summary$intensity, levels = intensity_levels)

  # Get colors
  colors <- canhrActi_palette("intensity")

  if (style == "bar") {
    # Horizontal bar chart
    p <- ggplot2::ggplot(intensity_summary,
                        ggplot2::aes(x = percentage, y = intensity, fill = intensity)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%% (%d min)",
                                                       percentage, minutes)),
                        hjust = -0.1, size = 3.5) +
      ggplot2::scale_fill_manual(
        values = colors,
        guide = "none"
      ) +
      ggplot2::scale_y_discrete(
        labels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, max(intensity_summary$percentage) * 1.3),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = "Time in Each Intensity Level",
        subtitle = sprintf("Total wear time: %.1f hours | MVPA: %d minutes",
                          x$overall_summary$total_wear_hours,
                          x$overall_summary$mvpa_minutes),
        x = "Percentage of Wear Time (%)",
        y = NULL,
        caption = sprintf("Intensity algorithm: %s", x$parameters$intensity_algorithm)
      ) +
      theme_canhrActi()

  } else {
    # Pie chart
    p <- ggplot2::ggplot(intensity_summary,
                        ggplot2::aes(x = "", y = percentage, fill = intensity)) +
      ggplot2::geom_col(width = 1, color = "white", linewidth = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)),
                        position = ggplot2::position_stack(vjust = 0.5),
                        size = 3.5, fontface = "bold") +
      ggplot2::scale_fill_manual(
        values = colors,
        labels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
        name = "Intensity"
      ) +
      ggplot2::labs(
        title = "Activity Intensity Distribution",
        subtitle = sprintf("Total wear time: %.1f hours", x$overall_summary$total_wear_hours),
        caption = sprintf("Intensity algorithm: %s", x$parameters$intensity_algorithm)
      ) +
      theme_canhrActi(grid = FALSE) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = "right",
        legend.title = ggplot2::element_text(face = "bold", size = 11),
        legend.text = ggplot2::element_text(size = 10),
        legend.key.size = ggplot2::unit(0.8, "cm")
      )
  }

  return(p)
}


#' Plot Wear Time Quality Check
#'
#' Visualizes minute-by-minute counts with wear/non-wear classification overlay.
#' Essential for validating wear time detection algorithm performance.
#'
#' @param x A \code{canhrActi_analysis} object from \code{canhrActi()}
#' @param date Character or Date. Which day to plot (default: first day)
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' plot_wear_time(results)
#' plot_wear_time(results, date = "2024-01-15")
#' }
#'
#' @export
plot_wear_time <- function(x, date = NULL) {

  if (!inherits(x, "canhrActi_analysis")) {
    stop("Input must be a canhrActi_analysis object from canhrActi()")
  }

  epoch_data <- x$epoch_data

  if (is.null(date)) {
    date <- epoch_data$date[1]
  } else {
    date <- as.Date(date)
  }

  day_data <- epoch_data[epoch_data$date == date, ]

  if (nrow(day_data) == 0) {
    stop("No data found for date: ", date)
  }

  day_data$wear_status <- ifelse(day_data$wear_time, "Wear", "Non-wear")
  day_data$hour <- as.numeric(format(day_data$timestamp, "%H")) +
                   as.numeric(format(day_data$timestamp, "%M")) / 60

  p <- ggplot2::ggplot(day_data, ggplot2::aes(x = hour, y = axis1)) +
    ggplot2::geom_rect(
      data = day_data[!day_data$wear_time, ],
      ggplot2::aes(xmin = hour - 1/60, xmax = hour + 1/60,
                   ymin = 0, ymax = max(day_data$axis1) * 1.1),
      fill = "#FFE0E0", alpha = 0.5, inherit.aes = FALSE
    ) +
    ggplot2::geom_line(color = canhrActi_color("blue"), linewidth = 0.4) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = sprintf("%02d:00", seq(0, 24, 4)),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(day_data$axis1) * 1.1),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Wear Time Quality Check",
      subtitle = sprintf("%s | Wear: %.1f hours | Non-wear: %.1f hours",
                        format(date, "%B %d, %Y"),
                        sum(day_data$wear_time) / 60,
                        sum(!day_data$wear_time) / 60),
      x = "Time of Day",
      y = "Activity Counts (axis1)",
      caption = sprintf("Wear time algorithm: %s | Red shading indicates detected non-wear periods",
                       x$parameters$wear_time_algorithm)
    ) +
    theme_canhrActi()

  return(p)
}


#' Plot Activity Profile Timeline
#'
#' Creates a detailed timeline of activity counts with intensity zones.
#' Similar to ActiLife's primary visualization.
#'
#' @param x A \code{canhrActi_analysis} object from \code{canhrActi()}
#' @param date_range Character or Date vector of length 2. Date range to plot
#'   (default: all dates)
#' @param wear_only Logical. Show only wear time? (default: TRUE)
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' plot_activity_profile(results)
#' plot_activity_profile(results, date_range = c("2024-01-15", "2024-01-17"))
#' }
#'
#' @export
plot_activity_profile <- function(x, date_range = NULL, wear_only = TRUE) {

  if (!inherits(x, "canhrActi_analysis")) {
    stop("Input must be a canhrActi_analysis object from canhrActi()")
  }

  epoch_data <- x$epoch_data

  if (!is.null(date_range)) {
    date_range <- as.Date(date_range)
    if (length(date_range) != 2) {
      stop("date_range must be a vector of length 2")
    }
    epoch_data <- epoch_data[epoch_data$date >= date_range[1] &
                            epoch_data$date <= date_range[2], ]
  }

  if (wear_only) {
    epoch_data <- epoch_data[epoch_data$wear_time, ]
  }

  if (nrow(epoch_data) == 0) {
    stop("No data to plot")
  }

  intensity_levels <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")
  epoch_data$intensity <- factor(epoch_data$intensity, levels = intensity_levels)
  colors <- canhrActi_palette("intensity")

  p <- ggplot2::ggplot(epoch_data, ggplot2::aes(x = timestamp, y = axis1)) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = timestamp - 30, xmax = timestamp + 30,
                   ymin = 0, ymax = axis1, fill = intensity),
      alpha = 0.3
    ) +
    ggplot2::geom_line(color = canhrActi_color("blue"), linewidth = 0.3, alpha = 0.8) +
    ggplot2::scale_fill_manual(
      values = colors,
      labels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
      name = "Intensity",
      drop = FALSE
    ) +
    ggplot2::scale_x_datetime(
      date_breaks = "1 day",
      date_labels = "%b %d"
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Activity Profile",
      subtitle = sprintf("MVPA: %d min | Sedentary: %d min | Wear time: %.1f hours",
                        x$overall_summary$mvpa_minutes,
                        x$intensity_summary$minutes[x$intensity_summary$intensity == "sedentary"],
                        x$overall_summary$total_wear_hours),
      x = "Date",
      y = "Activity Counts (axis1)",
      caption = if (wear_only) "Showing wear time only" else "Showing all epochs"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      legend.position = "right"
    )

  return(p)
}


#' Plot MVPA Bouts
#'
#' Visualizes detected MVPA bouts as timeline segments showing when sustained
#' moderate-to-vigorous activity occurred.
#'
#' @param x A \code{canhrActi_analysis} object from \code{canhrActi()}
#' @param min_bout_length Integer. Minimum bout duration in minutes (default: 10)
#' @param drop_time_allowance Integer. Drop time allowance in minutes (default: 2)
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' plot_mvpa_bouts(results)
#' }
#'
#' @export
plot_mvpa_bouts <- function(x, min_bout_length = 10, drop_time_allowance = 2) {

  if (!inherits(x, "canhrActi_analysis")) {
    stop("Input must be a canhrActi_analysis object from canhrActi()")
  }

  bouts <- detect.mvpa.bouts(x$epoch_data$intensity,
                             min_bout_length = min_bout_length,
                             drop_time_allowance = drop_time_allowance)

  if (nrow(bouts) == 0) {
    stop("No MVPA bouts detected with current parameters")
  }

  bout_data <- data.frame(
    bout_number = bouts$bout_number,
    start_time = x$epoch_data$timestamp[bouts$start_index],
    end_time = x$epoch_data$timestamp[bouts$end_index],
    bout_length = bouts$bout_length,
    mvpa_percent = bouts$mvpa_percent
  )

  bout_data$date <- as.Date(bout_data$start_time)
  bout_data$start_hour <- as.numeric(format(bout_data$start_time, "%H")) +
                         as.numeric(format(bout_data$start_time, "%M")) / 60
  bout_data$end_hour <- as.numeric(format(bout_data$end_time, "%H")) +
                       as.numeric(format(bout_data$end_time, "%M")) / 60

  bout_data$date_factor <- factor(bout_data$date)

  p <- ggplot2::ggplot(bout_data, ggplot2::aes(y = date_factor)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = start_hour, xend = end_hour,
                   yend = date_factor, color = bout_length),
      linewidth = 4
    ) +
    ggplot2::scale_color_gradient(
      low = canhrActi_color("moderate"),
      high = canhrActi_color("vigorous"),
      name = "Duration\n(minutes)"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = sprintf("%02d:00", seq(0, 24, 4)),
      limits = c(0, 24)
    ) +
    ggplot2::labs(
      title = "MVPA Bout Timeline",
      subtitle = sprintf("%d bouts detected | Total bouted MVPA: %d minutes | Mean duration: %.1f min",
                        nrow(bouts),
                        sum(bouts$mvpa_minutes),
                        mean(bouts$bout_length)),
      x = "Time of Day",
      y = "Date",
      caption = sprintf("Minimum bout: %d min | Drop time allowance: %d min",
                       min_bout_length, drop_time_allowance)
    ) +
    theme_canhrActi()

  return(p)
}


#' Plot Sleep Analysis
#'
#' Creates an enhanced actogram visualization showing activity counts with
#' sleep/wake scoring overlay. Based on research best practices for actigraphy
#' sleep visualization.
#'
#' @param x A \code{canhrActi_sleep} or \code{canhrActi_sleep_batch} object
#' @param night Integer. Which sleep period to plot (default: 1)
#' @param show_activity Logical. Show activity counts as background (default: TRUE)
#' @param show_awakenings Logical. Highlight wake bouts in red (default: TRUE)
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' sleep <- canhrActi.sleep("participant.agd")
#' plot_sleep(sleep)
#' plot_sleep(sleep, night = 2, show_awakenings = TRUE)
#' }
#'
#' @export
plot_sleep <- function(x, night = 1, show_activity = TRUE, show_awakenings = TRUE) {

  if (!inherits(x, "canhrActi_sleep") && !inherits(x, "canhrActi_sleep_batch")) {
    stop("Input must be a canhrActi_sleep object from canhrActi.sleep()")
  }

  if (inherits(x, "canhrActi_sleep_batch")) {
    if (length(x$results) == 0) {
      stop("No sleep results found")
    }
    x <- x$results[[1]]
  }

  if (is.null(x$sleep_periods) || nrow(x$sleep_periods) == 0) {
    stop("No sleep periods detected")
  }

  if (night > nrow(x$sleep_periods)) {
    stop("Night ", night, " not found. Only ", nrow(x$sleep_periods), " periods available")
  }

  sleep_period <- x$sleep_periods[night, ]

  in_bed <- as.POSIXct(sleep_period$in_bed_time)
  out_bed <- as.POSIXct(sleep_period$out_bed_time)

  if (is.null(x$epoch_data)) {
    stop("No epoch data available for plotting")
  }

  epoch_data <- x$epoch_data

  period_data <- epoch_data[epoch_data$timestamp >= in_bed &
                           epoch_data$timestamp <= out_bed, ]

  if (nrow(period_data) == 0) {
    stop("No data for night ", night)
  }

  period_data$sleep_numeric <- ifelse(period_data$sleep_wake == "S", 0, 1)
  period_data$minutes <- as.numeric(difftime(period_data$timestamp,
                                             in_bed,
                                             units = "mins"))

  max_counts <- max(period_data$axis1, na.rm = TRUE)
  if (max_counts > 0) {
    period_data$activity_scaled <- period_data$axis1 / max_counts
  } else {
    period_data$activity_scaled <- 0
  }

  p <- ggplot2::ggplot(period_data, ggplot2::aes(x = minutes))

  if (show_activity) {
    p <- p +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = minutes - 0.5, xmax = minutes + 0.5,
                     ymin = 0, ymax = activity_scaled * 0.3, fill = "Activity"),
        alpha = 0.5
      )
  }

  p <- p +
    ggplot2::geom_rect(
      data = period_data[period_data$sleep_wake == "S", ],
      ggplot2::aes(xmin = minutes - 0.5, xmax = minutes + 0.5,
                   ymin = 0, ymax = 0.05, fill = "Sleep Period"),
      alpha = 0.8
    )

  if (show_awakenings) {
    wake_data <- period_data[period_data$sleep_wake == "W" &
                            period_data$minutes > 0 &
                            period_data$minutes < max(period_data$minutes), ]
    if (nrow(wake_data) > 0) {
      rle_wake <- rle(period_data$sleep_wake == "W")
      wake_bouts <- data.frame()
      pos <- 1
      for (i in seq_along(rle_wake$lengths)) {
        if (rle_wake$values[i] && rle_wake$lengths[i] >= 2) {
          start_idx <- pos
          end_idx <- pos + rle_wake$lengths[i] - 1
          if (start_idx > 1 && end_idx < nrow(period_data)) {
            wake_bouts <- rbind(wake_bouts, data.frame(
              start = period_data$minutes[start_idx],
              end = period_data$minutes[end_idx]
            ))
          }
        }
        pos <- pos + rle_wake$lengths[i]
      }

      if (nrow(wake_bouts) > 0) {
        p <- p +
          ggplot2::geom_rect(
            data = wake_bouts,
            ggplot2::aes(xmin = start, xmax = end, ymin = 0.85, ymax = 0.95, fill = "Wake Bout"),
            alpha = 0.7, inherit.aes = FALSE
          )
      }
    }
  }

  p <- p +
    ggplot2::geom_line(ggplot2::aes(y = sleep_numeric, color = "Sleep/Wake State"),
                      linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                       color = "#999999", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted",
                       color = "#2E7D32", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c("Sleep Period" = "#4575B4",
                "Wake Bout" = "#D73027",
                "Activity" = "#B0BEC5"),
      breaks = c("Sleep Period", "Wake Bout", "Activity"),
      labels = c("Sleep Period (blue bar)",
                "Wake Bout (red box)",
                "Activity Intensity (gray)")
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Sleep/Wake State" = "#1565C0"),
      labels = c("Sleep/Wake Line (blue)")
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Sleep", "Wake"),
      limits = c(0, 1.05),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0)
    )

  time_in_bed <- as.numeric(difftime(out_bed, in_bed, units = "mins"))
  total_sleep_min <- round(time_in_bed * sleep_period$sleep_efficiency / 100)
  waso <- time_in_bed - total_sleep_min

  p <- p +
    ggplot2::labs(
      title = "Sleep Actogram",
      subtitle = sprintf("Period %d: %s | Efficiency: %.1f%% | TST: %.1f hr | WASO: %d min | Awakenings: %d",
                        night,
                        format(in_bed, "%b %d, %Y"),
                        sleep_period$sleep_efficiency,
                        total_sleep_min / 60,
                        waso,
                        sleep_period$number_of_awakenings),
      x = "Minutes Since Bedtime",
      y = NULL,
      caption = sprintf("Algorithm: %s | In bed: %s | Out bed: %s%s",
                       x$parameters$algorithm,
                       format(in_bed, "%H:%M"),
                       format(out_bed, "%H:%M"),
                       if (show_activity) " | Gray bars = activity intensity" else "")
    ) +
    theme_canhrActi(grid = FALSE) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#E0E0E0", linewidth = 0.2),
      axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "right",
      legend.justification = "top",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 9),
      legend.key.size = ggplot2::unit(0.8, "cm"),
      legend.spacing.y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2)
    )

  return(p)
}


#' Plot Daily Summary
#'
#' Creates a multi-panel visualization showing daily patterns of activity metrics.
#'
#' @param x A \code{canhrActi_analysis} object from \code{canhrActi()}
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' plot_daily_summary(results)
#' }
#'
#' @export
plot_daily_summary <- function(x) {

  if (!inherits(x, "canhrActi_analysis")) {
    stop("Input must be a canhrActi_analysis object from canhrActi()")
  }

  epoch_data <- x$epoch_data

  daily_data <- aggregate(
    cbind(wear_time = as.numeric(wear_time),
          mvpa = as.numeric(intensity %in% c("moderate", "vigorous", "very_vigorous")),
          sedentary = as.numeric(intensity == "sedentary"),
          steps = steps) ~ date,
    data = epoch_data,
    FUN = sum
  )

  daily_data$wear_hours <- daily_data$wear_time / 60

  daily_data$date_label <- format(daily_data$date, "%b %d")

  p1 <- ggplot2::ggplot(daily_data, ggplot2::aes(x = date_label, y = wear_hours)) +
    ggplot2::geom_col(fill = canhrActi_color("blue"), width = 0.7) +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed",
                       color = "#999999", linewidth = 0.5) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 24)) +
    ggplot2::labs(
      title = "Daily Wear Time",
      x = NULL,
      y = "Hours"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11)
    )

  p2 <- ggplot2::ggplot(daily_data, ggplot2::aes(x = date_label, y = mvpa)) +
    ggplot2::geom_col(fill = canhrActi_color("moderate"), width = 0.7) +
    ggplot2::geom_hline(yintercept = 30, linetype = "dashed",
                       color = "#999999", linewidth = 0.5) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = "Daily MVPA",
      x = NULL,
      y = "Minutes"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11)
    )

  p3 <- ggplot2::ggplot(daily_data, ggplot2::aes(x = date_label, y = sedentary)) +
    ggplot2::geom_col(fill = canhrActi_color("sedentary"), width = 0.7) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = "Daily Sedentary Time",
      x = NULL,
      y = "Minutes"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11)
    )

  p4 <- ggplot2::ggplot(daily_data, ggplot2::aes(x = date_label, y = steps)) +
    ggplot2::geom_col(fill = canhrActi_color("green"), width = 0.7) +
    ggplot2::geom_hline(yintercept = 10000, linetype = "dashed",
                       color = "#999999", linewidth = 0.5) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = "Daily Steps",
      x = "Date",
      y = "Steps"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)
    )

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("Package 'patchwork' not installed. Install it to combine panels.\n",
           "Returning first panel only. Install with: install.packages('patchwork')")
    return(p1)
  }

  p_combined <- p1 / p2 / p3 / p4 +
    patchwork::plot_annotation(
      title = "Daily Activity Summary",
      subtitle = sprintf("%d valid days | Mean MVPA: %.0f min/day | Mean steps: %.0f/day",
                        x$overall_summary$valid_days,
                        mean(daily_data$mvpa),
                        mean(daily_data$steps)),
      theme = theme_canhrActi()
    )

  return(p_combined)
}
