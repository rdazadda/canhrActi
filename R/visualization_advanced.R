#' @title Advanced Visualization Module
#'
#' @description
#' Advanced visualization tools for accelerometer data, replicating and
#' exceeding ActiLife's graphing capabilities. Includes multi-day activity
#' timelines, inclinometer/posture analysis, and interactive visualizations.
#'
#' @name visualization-advanced
NULL


#' Multi-Day Activity Timeline Plot (ActiLife Style)
#'
#' Creates a stacked multi-day activity visualization similar to ActiLife's
#' graphing interface, with each day displayed as a separate row.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column (default: "timestamp")
#' @param show_axes Character vector of axes to display. Options: "axis1", "axis2",
#'   "axis3", "vm", "steps", "hr" (default: c("axis1"))
#' @param show_cutpoints Logical. Show activity intensity cut-point lines? (default: TRUE)
#' @param cutpoints Named numeric vector of cut-points (default: Freedson adult)
#' @param show_inclinometer Logical. Show inclinometer posture as background? (default: FALSE)
#' @param inclinometer_col Name of inclinometer/posture column
#' @param equal_scales Logical. Use same y-axis scale for all days? (default: TRUE)
#' @param max_counts Maximum y-axis value (default: auto)
#' @param epoch_length Epoch length in seconds (optional)
#' @param title Plot title (default: "Daily Activity Timeline")
#' @param color_scheme Named list of colors for each metric
#'
#' @return A ggplot2 object
#'
#' @details
#' This function replicates ActiLife's multi-day graphing view where each day
#' is displayed as a horizontal timeline stacked vertically. Features include:
#'
#' \itemize{
#'   \item Multiple metrics on same plot (Axis1, Axis2, Axis3, VM, Steps, HR)
#'   \item Cut-point threshold lines for activity classification
#'   \item Inclinometer posture as background shading
#'   \item Equal or independent y-axis scales per day
#'   \item Professional color scheme matching ActiLife
#' }
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#'
#' # Basic plot with Axis1 only
#' plot_daily_timeline(results$epoch_data)
#'
#' # Multi-axis plot
#' plot_daily_timeline(results$epoch_data,
#'                     show_axes = c("axis1", "axis2", "vm"),
#'                     show_cutpoints = TRUE)
#'
#' # With inclinometer overlay
#' plot_daily_timeline(results$epoch_data,
#'                     show_axes = c("axis1"),
#'                     show_inclinometer = TRUE,
#'                     inclinometer_col = "inclinometer")
#' }
#'
#' @export
plot_daily_timeline <- function(data,
                                 timestamp_col = "timestamp",
                                 show_axes = c("axis1"),
                                 show_cutpoints = TRUE,
                                 cutpoints = c(sedentary = 100, light = 1952,
                                             moderate = 5725, vigorous = 9498),
                                 show_inclinometer = FALSE,
                                 inclinometer_col = "inclinometer",
                                 equal_scales = TRUE,
                                 max_counts = NULL,
                                 epoch_length = NULL,
                                 title = "Daily Activity Timeline",
                                 color_scheme = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Default ActiLife-style color scheme
  if (is.null(color_scheme)) {
    color_scheme <- list(
      axis1 = "#1E90FF",      # Blue (DodgerBlue)
      axis2 = "#DC143C",      # Red (Crimson)
      axis3 = "#FFD700",      # Yellow (Gold)
      vm = "#9370DB",         # Purple (MediumPurple)
      steps = "#32CD32",      # Green (LimeGreen)
      hr = "#FF4500",         # OrangeRed (Heart Rate)
      lux = "#FFA500",        # Orange
      # Inclinometer colors
      standing = "#32CD32",   # Green
      sitting = "#FFD700",    # Yellow
      lying = "#1E90FF",      # Blue
      off = "#2F2F2F"         # Dark gray
    )
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Add date and time columns
  data$date <- as.Date(data[[timestamp_col]])
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60

  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    if (nrow(data) > 1) {
      time_diffs <- diff(as.numeric(data[[timestamp_col]]))
      epoch_length <- round(stats::median(time_diffs, na.rm = TRUE))
    }
  }
  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    epoch_length <- 60
  }

  # Get unique dates
  unique_dates <- sort(unique(data$date))
  n_days <- length(unique_dates)

  # Format date labels
  data$date_label <- format(data$date, "%A\n%m/%d/%Y")
  data$date_label <- factor(data$date_label,
                            levels = format(unique_dates, "%A\n%m/%d/%Y"))

  # Prepare data for plotting - reshape to long format for multiple axes
  plot_data_list <- list()

  for (axis in show_axes) {
    if (axis %in% names(data)) {
      temp_df <- data.frame(
        date_label = data$date_label,
        time_of_day = data$time_of_day,
        value = data[[axis]],
        metric = axis,
        stringsAsFactors = FALSE
      )
      plot_data_list[[axis]] <- temp_df
    } else if (axis == "vm" && all(c("axis1", "axis2", "axis3") %in% names(data))) {
      # Calculate VM if not present
      temp_df <- data.frame(
        date_label = data$date_label,
        time_of_day = data$time_of_day,
        value = sqrt(data$axis1^2 + data$axis2^2 + data$axis3^2),
        metric = "vm",
        stringsAsFactors = FALSE
      )
      plot_data_list[["vm"]] <- temp_df
    }
  }

  if (length(plot_data_list) == 0) {
    stop("No valid axes found in data. Available columns: ",
         paste(names(data), collapse = ", "))
  }

  plot_data <- do.call(rbind, plot_data_list)
  plot_data$metric <- factor(plot_data$metric, levels = show_axes)

  # Calculate y-axis maximum
  if (is.null(max_counts)) {
    max_counts <- max(plot_data$value, na.rm = TRUE) * 1.1
  }

  # Create base plot
  p <- ggplot2::ggplot()

  # Add inclinometer background if requested
  if (show_inclinometer && inclinometer_col %in% names(data)) {
    incl_data <- data.frame(
      date_label = data$date_label,
      time_of_day = data$time_of_day,
      posture = data[[inclinometer_col]],
      stringsAsFactors = FALSE
    )

    # Create rectangles for posture periods
    incl_data$posture <- tolower(as.character(incl_data$posture))

    p <- p +
      ggplot2::geom_tile(
        data = incl_data,
        ggplot2::aes(x = time_of_day, y = max_counts / 2,
                     fill = posture, height = max_counts),
        alpha = 0.3, width = 1/60
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "standing" = color_scheme$standing,
          "sitting" = color_scheme$sitting,
          "lying" = color_scheme$lying,
          "off" = color_scheme$off
        ),
        name = "Posture",
        na.value = "transparent"
      )
  }

  # Add activity lines
  metric_colors <- sapply(show_axes, function(x) color_scheme[[x]])
  names(metric_colors) <- show_axes

  p <- p +
    ggplot2::geom_line(
      data = plot_data,
      ggplot2::aes(x = time_of_day, y = value, color = metric),
      linewidth = 0.4, na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = metric_colors,
      labels = c(
        axis1 = "Axis 1", axis2 = "Axis 2", axis3 = "Axis 3",
        vm = "VM", steps = "Steps", hr = "Heart Rate"
      )[show_axes],
      name = "Metric"
    )

  # Add cut-point lines if requested
  if (show_cutpoints && length(cutpoints) > 0) {
    cutpoints_scaled <- cutpoints * (epoch_length / 60)
    cutpoint_df <- data.frame(
      yintercept = cutpoints_scaled,
      label = names(cutpoints_scaled),
      stringsAsFactors = FALSE
    )
    cutpoint_df <- cutpoint_df[cutpoint_df$yintercept <= max_counts, ]

    if (nrow(cutpoint_df) > 0) {
      p <- p +
        ggplot2::geom_hline(
          data = cutpoint_df,
          ggplot2::aes(yintercept = yintercept),
          linetype = "dashed", color = "#FF8C00", alpha = 0.7, linewidth = 0.3
        )
    }
  }

  # Facet by date
  p <- p +
    ggplot2::facet_wrap(~ date_label, ncol = 1, scales = ifelse(equal_scales, "fixed", "free_y")) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12:00 AM", "4:00 AM", "8:00 AM", "12:00 PM", "4:00 PM", "8:00 PM", "12:00 AM"),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max_counts),
      labels = scales::comma_format(),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title,
      x = "Time of Day",
      y = "Counts"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      strip.text = ggplot2::element_text(face = "bold", size = 9, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#E8E8E8", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#E0E0E0", linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(color = "#E0E0E0", linewidth = 0.3),
      panel.background = ggplot2::element_rect(fill = "white", color = "#CCCCCC"),
      legend.position = "right",
      axis.text.x = ggplot2::element_text(size = 7),
      panel.spacing = ggplot2::unit(0.3, "lines")
    )

  return(p)
}


#' Inclinometer/Posture Visualization (ActiLife Style)
#'
#' Creates comprehensive posture visualizations including pie chart and
#' hourly stacked bar charts, similar to ActiLife's inclinometer view.
#'
#' @param data Data frame with timestamp and inclinometer columns
#' @param timestamp_col Name of timestamp column
#' @param inclinometer_col Name of inclinometer/posture column
#' @param date_filter Optional. Specific date to filter (default: NULL for all dates)
#' @param show_pie Logical. Show pie chart? (default: TRUE)
#' @param show_hourly Logical. Show hourly breakdown? (default: TRUE)
#' @param color_scheme Named list of posture colors
#'
#' @return A ggplot2 object or list of ggplot2 objects
#'
#' @details
#' Creates ActiLife-style inclinometer visualizations:
#' \itemize{
#'   \item Pie chart showing overall posture distribution with time labels
#'   \item Hourly stacked bar charts showing posture by hour
#'   \item Color scheme: Green=Standing, Yellow=Sitting, Blue=Lying, Black=Off
#' }
#'
#' @export
plot_inclinometer <- function(data,
                               timestamp_col = "timestamp",
                               inclinometer_col = "inclinometer",
                               date_filter = NULL,
                               show_pie = TRUE,
                               show_hourly = TRUE,
                               color_scheme = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Default ActiLife-style colors
  if (is.null(color_scheme)) {
    color_scheme <- c(
      "standing" = "#32CD32",   # Green
      "sitting" = "#FFD700",    # Yellow
      "lying" = "#1E90FF",      # Blue
      "off" = "#2F2F2F"         # Dark gray/black
    )
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Filter by date if specified
  if (!is.null(date_filter)) {
    data <- data[as.Date(data[[timestamp_col]]) == as.Date(date_filter), ]
  }

  if (nrow(data) == 0) {
    stop("No data available for the specified date")
  }

  # Standardize posture names
  data$posture <- tolower(as.character(data[[inclinometer_col]]))
  data$posture[is.na(data$posture)] <- "off"

  # Calculate epoch length (assuming regular intervals)
  time_diffs <- diff(as.numeric(data[[timestamp_col]]))
  epoch_seconds <- median(time_diffs, na.rm = TRUE)
  epoch_minutes <- epoch_seconds / 60

  plots <- list()

  # 
  if (show_pie) {
    # Calculate total time in each posture
    posture_summary <- as.data.frame(table(data$posture))
    names(posture_summary) <- c("posture", "epochs")
    posture_summary$minutes <- posture_summary$epochs * epoch_minutes
    posture_summary$hours <- posture_summary$minutes / 60
    posture_summary$percent <- posture_summary$epochs / sum(posture_summary$epochs) * 100

    # Format time strings (HH:MM:SS)
    posture_summary$time_str <- sapply(posture_summary$minutes, function(m) {
      h <- floor(m / 60)
      mins <- floor(m %% 60)
      secs <- round((m %% 1) * 60)
      sprintf("%d:%02d:%02d", h, mins, secs)
    })

    # Create labels
    posture_summary$label <- sprintf("%s\n%.0f%% (%s)",
                                      tools::toTitleCase(posture_summary$posture),
                                      posture_summary$percent,
                                      posture_summary$time_str)

    # Order factors
    posture_order <- c("standing", "sitting", "lying", "off")
    posture_summary$posture <- factor(posture_summary$posture, levels = posture_order)
    posture_summary <- posture_summary[order(posture_summary$posture), ]

    # Pie chart
    pie_plot <- ggplot2::ggplot(posture_summary,
                                 ggplot2::aes(x = "", y = percent, fill = posture)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.5) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::scale_fill_manual(
        values = color_scheme,
        labels = posture_summary$label,
        name = "Posture"
      ) +
      ggplot2::labs(title = "Overall Posture Distribution") +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
        legend.position = "right",
        legend.text = ggplot2::element_text(size = 10)
      )

    plots$pie <- pie_plot
  }

  # 
  if (show_hourly) {
    # Add hour column
    data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
    data$date <- as.Date(data[[timestamp_col]])

    # Calculate proportion per hour per date
    hourly_summary <- aggregate(
      list(count = rep(1, nrow(data))),
      by = list(date = data$date, hour = data$hour, posture = data$posture),
      FUN = sum
    )

    # Calculate total per hour for percentages
    hour_totals <- aggregate(count ~ date + hour, data = hourly_summary, FUN = sum)
    names(hour_totals)[3] <- "total"

    hourly_summary <- merge(hourly_summary, hour_totals, by = c("date", "hour"))
    hourly_summary$percent <- hourly_summary$count / hourly_summary$total * 100
    hourly_summary$minutes <- hourly_summary$count * epoch_minutes

    # Format for display
    hourly_summary$hour_label <- sprintf("%02d:00", hourly_summary$hour)
    hourly_summary$date_label <- format(hourly_summary$date, "%m/%d/%Y\n%I:%M %p")

    # Order postures
    hourly_summary$posture <- factor(hourly_summary$posture,
                                      levels = c("off", "lying", "sitting", "standing"))

    # Create hourly plot for each date
    unique_dates <- unique(hourly_summary$date)

    for (d in unique_dates) {
      day_data <- hourly_summary[hourly_summary$date == d, ]
      date_str <- format(d, "%m/%d/%Y")

      hourly_plot <- ggplot2::ggplot(day_data,
                                      ggplot2::aes(x = factor(hour), y = minutes, fill = posture)) +
        ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.8) +
        ggplot2::scale_fill_manual(
          values = color_scheme,
          labels = c("Off", "Lying", "Sitting", "Standing"),
          name = "Posture"
        ) +
        ggplot2::scale_x_discrete(
          breaks = as.character(seq(0, 23, 2)),
          labels = sprintf("%02d:00", seq(0, 23, 2))
        ) +
        ggplot2::labs(
          title = sprintf("Hourly Inclinometer - %s", date_str),
          x = "Hour of Day",
          y = "Minutes"
        ) +
        theme_canhrActi() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          legend.position = "right",
          panel.grid.minor = ggplot2::element_blank()
        )

      plots[[paste0("hourly_", date_str)]] <- hourly_plot
    }
  }

  # Return single plot or list
  if (length(plots) == 1) {
    return(plots[[1]])
  }
  return(plots)
}


#' Combined Inclinometer Dashboard
#'
#' Creates a combined visualization with pie chart and hourly bars in a single view.
#'
#' @param data Data frame with timestamp and inclinometer columns
#' @param timestamp_col Name of timestamp column
#' @param inclinometer_col Name of inclinometer/posture column
#' @param date_filter Optional date to filter
#'
#' @return A combined ggplot2 object
#'
#' @export
plot_inclinometer_dashboard <- function(data,
                                         timestamp_col = "timestamp",
                                         inclinometer_col = "inclinometer",
                                         date_filter = NULL) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for combined plots")
  }

  plots <- plot_inclinometer(
    data = data,
    timestamp_col = timestamp_col,
    inclinometer_col = inclinometer_col,
    date_filter = date_filter,
    show_pie = TRUE,
    show_hourly = TRUE
  )

  if (is.list(plots) && length(plots) > 1) {
    # Combine pie with first hourly plot
    combined <- plots$pie + plots[[2]] +
      patchwork::plot_layout(widths = c(1, 2))
    return(combined)
  }

  return(plots)
}


#' Enhanced Activity Heatmap Visualization
#'
#' Creates a publication-quality heatmap showing activity intensity across days
#' and hours with colorblind-safe palettes, weekend highlighting, sleep overlay,
#' and summary statistics panel.
#'
#' @param data Data frame with timestamp and counts columns
#' @param timestamp_col Name of timestamp column
#' @param counts_col Name of counts column (default: "axis1")
#' @param metric Alternative name for counts_col parameter (default: NULL)
#' @param aggregate_func Function to aggregate counts (default: mean)
#' @param color_palette Color palette: "viridis" (default), "default", "plasma",
#'   "magma", "inferno", "cividis", or custom vector
#' @param show_values Logical. Show values in cells? (default: FALSE)
#' @param normalize Logical. Normalize values to 0-1 range? (default: FALSE)
#' @param show_weekends Logical. Add weekend row markers? (default: TRUE)
#' @param show_sleep_window Logical. Mark typical sleep window (22:00-06:00)? (default: TRUE)
#' @param sleep_periods Optional data frame with sleep periods for overlay
#' @param show_summary Logical. Show daily summary on right margin? (default: FALSE)
#' @param title Character. Plot title (default: "Activity Heatmap")
#'
#' @return A ggplot2 heatmap object
#'
#' @details
#' The heatmap provides a comprehensive view of activity patterns:
#' \itemize{
#'   \item Rows = days, Columns = hours
#'   \item Color intensity = activity level
#'   \item Weekend rows optionally highlighted
#'   \item Sleep window optionally shaded
#'   \item Colorblind-safe palette options via viridis
#' }
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#'
#' # Basic heatmap
#' plot_activity_heatmap(results$epoch_data)
#'
#' # Colorblind-safe with weekend markers
#' plot_activity_heatmap(results$epoch_data,
#'                       color_palette = "viridis",
#'                       show_weekends = TRUE)
#'
#' # With sleep window overlay
#' plot_activity_heatmap(results$epoch_data,
#'                       show_sleep_window = TRUE)
#' }
#'
#' @export
plot_activity_heatmap <- function(data,
                                   timestamp_col = "timestamp",
                                   counts_col = "axis1",
                                   metric = NULL,
                                   aggregate_func = mean,
                                   color_palette = "viridis",
                                   show_values = FALSE,
                                   normalize = FALSE,
                                   show_weekends = TRUE,
                                   show_sleep_window = TRUE,
                                   sleep_periods = NULL,
                                   show_summary = FALSE,
                                   title = "Activity Heatmap") {

  # Handle 'metric' parameter (alias for counts_col from dashboard)
  if (!is.null(metric)) {
    counts_col <- metric
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Set up color palette
  if (is.character(color_palette) && length(color_palette) == 1) {
    palette_colors <- switch(color_palette,
      "default" = c("#FFFFFF", "#FFF5F0", "#FEE0D2", "#FCBBA1",
                    "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D"),
      "viridis" = c("#440154", "#482878", "#3E4A89", "#31688E",
                    "#26828E", "#1F9E89", "#35B779", "#6DCD59",
                    "#B4DE2C", "#FDE725"),
      "plasma" = c("#0D0887", "#46039F", "#7201A8", "#9C179E",
                   "#BD3786", "#D8576B", "#ED7953", "#FB9F3A",
                   "#FDC328", "#F0F921"),
      "magma" = c("#000004", "#180F3D", "#440F76", "#721F81",
                  "#9E2F7F", "#CD4071", "#F1605D", "#FD9668",
                  "#FECA8D", "#FCFDBF"),
      "inferno" = c("#000004", "#1B0C41", "#4A0C6B", "#781C6D",
                    "#A52C60", "#CF4446", "#ED6925", "#FB9A06",
                    "#F7D03C", "#FCFFA4"),
      "cividis" = c("#00224E", "#123570", "#274683", "#395694",
                    "#4B67A4", "#5D79B3", "#6F8BC2", "#839ED0",
                    "#97B1DD", "#ACC5E9"),  # Colorblind optimized
      # Default fallback
      c("#FFFFFF", "#FFF5F0", "#FEE0D2", "#FCBBA1",
        "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D")
    )
  } else {
    palette_colors <- color_palette
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Create date and hour columns
  data$date <- as.Date(data[[timestamp_col]])
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$weekday <- weekdays(data$date, abbreviate = TRUE)
  data$weekday <- factor(data$weekday,
                         levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

  # Aggregate by date and hour
  heatmap_data <- aggregate(
    data[[counts_col]],
    by = list(date = data$date, hour = data$hour, weekday = data$weekday),
    FUN = aggregate_func,
    na.rm = TRUE
  )
  names(heatmap_data)[4] <- "activity"

  # Identify weekend dates
  unique_dates <- sort(unique(heatmap_data$date))
  weekend_dates <- unique_dates[weekdays(unique_dates) %in% c("Saturday", "Sunday")]

  # Normalize if requested
  if (normalize) {
    max_val <- max(heatmap_data$activity, na.rm = TRUE)
    if (max_val > 0) {
      heatmap_data$activity <- heatmap_data$activity / max_val
    }
  }

  # Calculate daily summaries for annotation
  if (show_summary) {
    daily_summary <- aggregate(
      heatmap_data$activity,
      by = list(date = heatmap_data$date),
      FUN = function(x) c(mean = mean(x, na.rm = TRUE), total = sum(x, na.rm = TRUE))
    )
    daily_summary <- cbind(daily_summary[, 1, drop = FALSE], as.data.frame(daily_summary$x))
  }

  # Create base plot
  p <- ggplot2::ggplot()

  # Add weekend row highlighting
  if (show_weekends && length(weekend_dates) > 0) {
    for (wd in weekend_dates) {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = -0.5, xmax = 23.5,
        ymin = as.Date(wd) - 0.5, ymax = as.Date(wd) + 0.5,
        fill = "#FFF3E0", alpha = 0.7
      )
    }
  }

  # Add sleep window shading (22:00-06:00)
  if (show_sleep_window) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = 22 - 0.5, xmax = 23.5,
        ymin = min(heatmap_data$date) - 0.5,
        ymax = max(heatmap_data$date) + 0.5,
        fill = "#E3F2FD", alpha = 0.3
      ) +
      ggplot2::annotate(
        "rect",
        xmin = -0.5, xmax = 6 - 0.5,
        ymin = min(heatmap_data$date) - 0.5,
        ymax = max(heatmap_data$date) + 0.5,
        fill = "#E3F2FD", alpha = 0.3
      )
  }

  # Add sleep period overlays if provided
  if (!is.null(sleep_periods) && nrow(sleep_periods) > 0) {
    for (i in seq_len(nrow(sleep_periods))) {
      sleep_start <- as.POSIXct(sleep_periods$start[i])
      sleep_end <- as.POSIXct(sleep_periods$end[i])
      sleep_date <- as.Date(sleep_start)

      start_hour <- as.numeric(format(sleep_start, "%H")) +
                    as.numeric(format(sleep_start, "%M")) / 60
      end_hour <- as.numeric(format(sleep_end, "%H")) +
                  as.numeric(format(sleep_end, "%M")) / 60

      if (end_hour < start_hour) {
        # Sleep spans midnight
        next_date <- sleep_date + 1
        # Evening portion
        p <- p + ggplot2::annotate(
          "segment",
          x = start_hour, xend = 24,
          y = as.Date(sleep_date), yend = as.Date(sleep_date),
          color = "#9C27B0", linewidth = 3, alpha = 0.6
        )
        # Morning portion (next day)
        if (next_date <= max(heatmap_data$date)) {
          p <- p + ggplot2::annotate(
            "segment",
            x = 0, xend = end_hour,
            y = as.Date(next_date), yend = as.Date(next_date),
            color = "#9C27B0", linewidth = 3, alpha = 0.6
          )
        }
      } else {
        p <- p + ggplot2::annotate(
          "segment",
          x = start_hour, xend = end_hour,
          y = as.Date(sleep_date), yend = as.Date(sleep_date),
          color = "#9C27B0", linewidth = 3, alpha = 0.6
        )
      }
    }
  }

  # Add heatmap tiles
  p <- p +
    ggplot2::geom_tile(
      data = heatmap_data,
      ggplot2::aes(x = hour, y = date, fill = activity),
      color = "white", linewidth = 0.1
    ) +
    ggplot2::scale_fill_gradientn(
      colors = palette_colors,
      name = if (normalize) "Activity\n(normalized)" else "Activity\n(counts/min)",
      na.value = "gray90"
    )

  # Add values in cells if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      data = heatmap_data,
      ggplot2::aes(x = hour, y = date, label = round(activity, 0)),
      size = 1.8, color = "black"
    )
  }

  # Calculate summary stats for subtitle
  n_days <- length(unique_dates)
  n_weekend <- length(weekend_dates)
  mean_activity <- round(mean(heatmap_data$activity, na.rm = TRUE), 0)

  subtitle_text <- sprintf("%d days (%d weekends) | Mean activity: %s counts/min",
                           n_days, n_weekend, format(mean_activity, big.mark = ","))
  if (show_sleep_window) {
    subtitle_text <- paste0(subtitle_text, " | Sleep window: 22:00-06:00 (blue)")
  }

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = sprintf("%02d:00", seq(0, 23, 3)),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_date(
      date_labels = "%a %m/%d",
      expand = c(0.01, 0.01)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle_text,
      x = "Hour of Day",
      y = NULL,
      caption = if (show_weekends) "Weekend rows highlighted" else NULL
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray40", size = 9),
      plot.caption = ggplot2::element_text(color = "gray50", size = 8, hjust = 0),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 9, face = "bold")
    )

  return(p)
}


#' Multi-Metric Overlay Plot
#'
#' Creates a single-day plot with multiple metrics overlaid, similar to
#' ActiLife's metric selection functionality.
#'
#' @param data Data frame with timestamp and metric columns
#' @param timestamp_col Name of timestamp column
#' @param metrics Named list mapping metric names to column names
#' @param date_filter Specific date to plot
#' @param show_cutpoints Logical. Show cut-point lines?
#' @param cutpoints Named vector of cut-point values
#' @param epoch_length Epoch length in seconds (optional)
#' @param normalize Logical. Normalize metrics to same scale? (default: FALSE)
#' @param facet Logical. Use faceted display instead of overlay? (default: FALSE)
#' @param title Character. Plot title (default: "Multi-Metric Comparison")
#'
#' @return A ggplot2 object
#'
#' @export
plot_multi_metric <- function(data,
                               timestamp_col = "timestamp",
                               metrics = list(
                                 "Axis 1" = "axis1",
                                 "VM" = "vm"
                               ),
                               date_filter = NULL,
                               show_cutpoints = TRUE,
                               cutpoints = c(sedentary = 100, light = 1952,
                                           moderate = 5725, vigorous = 9498),
                               epoch_length = NULL,
                               normalize = FALSE,
                               facet = FALSE,
                               title = "Multi-Metric Comparison") {

  # Handle vector input for metrics (from dashboard)
  if (is.character(metrics) && is.null(names(metrics))) {
    metrics <- setNames(as.list(metrics), metrics)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Filter by date if specified
  if (!is.null(date_filter)) {
    data <- data[as.Date(data[[timestamp_col]]) == as.Date(date_filter), ]
  }

  if (nrow(data) == 0) {
    stop("No data available for plotting")
  }

  # Add time of day
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60

  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    if (nrow(data) > 1) {
      time_diffs <- diff(as.numeric(data[[timestamp_col]]))
      epoch_length <- round(stats::median(time_diffs, na.rm = TRUE))
    }
  }
  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    epoch_length <- 60
  }

  # Prepare long-format data
  plot_data_list <- list()

  for (metric_name in names(metrics)) {
    col_name <- metrics[[metric_name]]
    if (col_name %in% names(data)) {
      values <- data[[col_name]]
      if (normalize) {
        values <- (values - min(values, na.rm = TRUE)) /
                  (max(values, na.rm = TRUE) - min(values, na.rm = TRUE)) * 100
      }
      plot_data_list[[metric_name]] <- data.frame(
        time = data$time_of_day,
        value = values,
        metric = metric_name,
        stringsAsFactors = FALSE
      )
    }
  }

  plot_data <- do.call(rbind, plot_data_list)

  # Color palette
  colors <- c("#1E90FF", "#DC143C", "#FFD700", "#32CD32",
              "#9370DB", "#FF4500", "#00CED1", "#FF69B4")
  names(colors) <- names(metrics)[1:min(length(metrics), length(colors))]

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = value, color = metric)) +
    ggplot2::geom_line(linewidth = 0.5, alpha = 0.8) +
    ggplot2::scale_color_manual(values = colors, name = "Metric") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"),
      limits = c(0, 24)
    )

  # Add cut-points if not normalized
  if (show_cutpoints && !normalize) {
    max_val <- max(plot_data$value, na.rm = TRUE)
    cutpoints_scaled <- cutpoints * (epoch_length / 60)
    cutpoint_df <- data.frame(
      yintercept = cutpoints_scaled[cutpoints_scaled <= max_val * 1.1],
      label = names(cutpoints_scaled[cutpoints_scaled <= max_val * 1.1])
    )

    if (nrow(cutpoint_df) > 0) {
      cutpoint_df$x <- 0.5  # Add x position for geom_text
      p <- p +
        ggplot2::geom_hline(
          data = cutpoint_df,
          ggplot2::aes(yintercept = yintercept),
          linetype = "dashed", color = "orange", alpha = 0.6
        ) +
        ggplot2::geom_text(
          data = cutpoint_df,
          ggplot2::aes(x = x, y = yintercept, label = label),
          hjust = 0, vjust = -0.3, size = 2.5, color = "orange"
        )
    }
  }

  # Date label for title
  date_str <- if (!is.null(date_filter)) {
    format(as.Date(date_filter), "%A, %B %d, %Y")
  } else {
    format(min(as.Date(data[[timestamp_col]])), "%A, %B %d, %Y")
  }

  # Add faceting if requested
  if (facet) {
    p <- p + ggplot2::facet_wrap(~metric, ncol = 1, scales = "free_y")
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = date_str,
      x = "Time of Day",
      y = if (normalize) "Normalized Value (%)" else "Counts"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      legend.position = if (facet) "none" else "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}


#' Daily Summary Bar Chart
#'
#' Creates a bar chart showing daily totals for key metrics.
#'
#' @param data Data frame with daily summary data
#' @param metrics Character vector of metrics to plot
#' @param date_col Name of date column
#' @param timestamp_col Name of timestamp column (default: "timestamp")
#' @param axis1_col Name of axis1 counts column (default: "axis1")
#' @param epoch_length Epoch length in seconds (optional)
#' @param cutpoints Cut-point algorithm to use (default: "freedson")
#' @param daily_summary Optional. Pre-computed daily summary data frame (default: NULL)
#' @param title Character. Plot title (default: "Daily Activity Summary")
#' @param subtitle Character. Optional plot subtitle (default: NULL)
#'
#' @return A ggplot2 object
#'
#' @export
plot_daily_summary_bars <- function(data,
                                     metrics = c("steps", "mvpa_min", "sedentary_min"),
                                     date_col = "date",
                                     timestamp_col = "timestamp",
                                     axis1_col = "axis1",
                                     epoch_length = NULL,
                                     cutpoints = "freedson",
                                     daily_summary = NULL,
                                     title = "Daily Activity Summary",
                                     subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    if (timestamp_col %in% names(data) && nrow(data) > 1) {
      time_diffs <- diff(as.numeric(data[[timestamp_col]]))
      epoch_length <- round(stats::median(time_diffs, na.rm = TRUE))
    }
  }
  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    epoch_length <- 60
  }

  # Use pre-calculated daily summary if provided
  if (!is.null(daily_summary)) {
    data <- daily_summary
    date_col <- "date"

    # Handle Activity Analysis format: use epoch counts directly
    # Activity Analysis stores: sedentary, light, moderate, vigorous (epoch counts)
    # For 1-minute epochs, epoch count = minutes, so use directly
    if ("sedentary" %in% names(data) && !"sedentary_min" %in% names(data)) {
      # Add MVPA column
      data$mvpa <- data$moderate + data$vigorous
      # Use epoch counts directly as metrics
      metrics <- c("sedentary", "light", "moderate", "vigorous")
    }
  } else if (!date_col %in% names(data) && timestamp_col %in% names(data)) {
    # Auto-detect and summarize raw data if needed
    # Raw data with timestamps - need to summarize by date
    data$date <- as.Date(data[[timestamp_col]])

    # Get cut-point thresholds
    cp <- tryCatch({
      get_cutpoint_thresholds(cutpoints)
    }, error = function(e) {
      list(sedentary = 100, light = 1952, moderate = 5725, vigorous = 9498)
    })

    # Summarize by date
    daily_summary <- lapply(split(data, data$date), function(day_data) {
      axis1_vals <- if (axis1_col %in% names(day_data)) day_data[[axis1_col]] else rep(0, nrow(day_data))
      if (!is.null(epoch_length) && !is.na(epoch_length) && epoch_length > 0 && epoch_length != 60) {
        axis1_vals <- to_cpm(axis1_vals, epoch_length)
      }

      # Calculate metrics
      total_steps <- if ("steps" %in% names(day_data)) sum(day_data$steps, na.rm = TRUE) else NA
      sedentary_epochs <- sum(axis1_vals < cp$sedentary, na.rm = TRUE)
      light_epochs <- sum(axis1_vals >= cp$sedentary & axis1_vals < cp$light, na.rm = TRUE)
      moderate_epochs <- sum(axis1_vals >= cp$light & axis1_vals < cp$moderate, na.rm = TRUE)
      vigorous_epochs <- sum(axis1_vals >= cp$moderate, na.rm = TRUE)
      mvpa_epochs <- moderate_epochs + vigorous_epochs

      data.frame(
        date = unique(day_data$date),
        steps = total_steps,
        sedentary_min = sedentary_epochs * epoch_length / 60,
        light_min = light_epochs * epoch_length / 60,
        moderate_min = moderate_epochs * epoch_length / 60,
        vigorous_min = vigorous_epochs * epoch_length / 60,
        mvpa_min = mvpa_epochs * epoch_length / 60,
        wear_min = nrow(day_data) * epoch_length / 60,
        stringsAsFactors = FALSE
      )
    })
    data <- do.call(rbind, daily_summary)
    date_col <- "date"
  }

  # Reshape to long format
  plot_data <- data.frame()

  for (metric in metrics) {
    if (metric %in% names(data)) {
      temp <- data.frame(
        date = data[[date_col]],
        value = data[[metric]],
        metric = metric,
        stringsAsFactors = FALSE
      )
      plot_data <- rbind(plot_data, temp)
    }
  }

  if (nrow(plot_data) == 0) {
    stop("No valid metrics found in data. Available columns: ", paste(names(data), collapse = ", "))
  }

  plot_data$date <- as.Date(plot_data$date)
  plot_data$metric <- factor(plot_data$metric, levels = metrics)

  # Metric labels (support both epoch counts and minutes formats)
  metric_labels <- c(
    steps = "Steps",
    mvpa_min = "MVPA (min)", sedentary_min = "Sedentary (min)",
    wear_min = "Wear Time (min)", light_min = "Light Activity (min)",
    moderate_min = "Moderate (min)", vigorous_min = "Vigorous (min)",
    mvpa = "MVPA", sedentary = "Sedentary", light = "Light",
    moderate = "Moderate", vigorous = "Vigorous"
  )

  # Colors (support both formats)
  metric_colors <- c(
    steps = "#32CD32", mvpa_min = "#FF4500", sedentary_min = "#1E90FF",
    wear_min = "#9370DB", light_min = "#FFD700", moderate_min = "#FFA500",
    vigorous_min = "#DC143C", mvpa = "#FF4500", sedentary = "#1E90FF",
    light = "#FFD700", moderate = "#FFA500", vigorous = "#DC143C"
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value, fill = metric)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_manual(
      values = metric_colors[metrics],
      labels = metric_labels[metrics],
      name = "Metric"
    ) +
    ggplot2::scale_x_date(date_labels = "%a\n%m/%d") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Date",
      y = "Value"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray50"),
      legend.position = "right"
    )

  return(p)
}


#' Intensity Distribution Stacked Area Plot
#'
#' Shows activity intensity distribution over time as a stacked area chart.
#'
#' @param data Data frame with timestamp and intensity columns
#' @param timestamp_col Name of timestamp column
#' @param intensity_col Name of intensity column
#' @param epoch_length Epoch length in seconds (optional)
#' @param cutpoints Cut-points to use for intensity classification (default: "freedson")
#' @param stacked Whether to stack the areas (default: TRUE)
#' @param intensity Optional. Pre-computed intensity vector (default: NULL)
#' @param title Character. Plot title (default: "Activity Intensity Distribution by Hour")
#' @param subtitle Character. Optional plot subtitle (default: NULL)
#'
#' @return A ggplot2 object
#'
#' @export
plot_intensity_area <- function(data,
                                 timestamp_col = "timestamp",
                                 intensity_col = "intensity",
                                 epoch_length = NULL,
                                 cutpoints = "freedson",
                                 stacked = TRUE,
                                 intensity = NULL,
                                 title = "Activity Intensity Distribution by Hour",
                                 subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    if (nrow(data) > 1) {
      time_diffs <- diff(as.numeric(data[[timestamp_col]]))
      epoch_length <- round(stats::median(time_diffs, na.rm = TRUE))
    }
  }
  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0) {
    epoch_length <- 60
  }

  # Use pre-calculated intensity if provided
  if (!is.null(intensity)) {
    # Use provided intensity vector (from Activity Analysis)
    data$intensity <- intensity
    intensity_col <- "intensity"
  } else if (!intensity_col %in% names(data)) {
    # If intensity column doesn't exist, compute it from axis1
    if ("axis1" %in% names(data)) {
      # Use selected cutpoints
      cp <- switch(cutpoints,
        "freedson" = c(0, 100, 1952, 5725, 9498),
        "evenson" = c(0, 101, 574, 1003, Inf),
        "canhr" = c(0, 100, 1500, 5000, 9000),
        c(0, 100, 1952, 5725, 9498)  # default to freedson
      )
      axis1_vals <- data$axis1
      if (!is.null(epoch_length) && !is.na(epoch_length) && epoch_length > 0 && epoch_length != 60) {
        axis1_vals <- to_cpm(axis1_vals, epoch_length)
      }
      data$intensity <- cut(axis1_vals,
        breaks = c(cp, Inf),
        labels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
        include.lowest = TRUE, right = FALSE
      )
      intensity_col <- "intensity"
    } else {
      stop("No intensity column found and no axis1 column to compute intensity")
    }
  }

  # Add hour column
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$date <- as.Date(data[[timestamp_col]])

  # Aggregate by hour and intensity
  hourly <- aggregate(
    list(count = rep(1, nrow(data))),
    by = list(
      date = data$date,
      hour = data$hour,
      intensity = data[[intensity_col]]
    ),
    FUN = sum
  )

  hourly$minutes <- hourly$count * (epoch_length / 60)

  # Order intensity levels
  intensity_order <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")
  hourly$intensity <- factor(hourly$intensity, levels = intensity_order)

  # Intensity colors - matching Intensity Pie chart
  intensity_colors <- c(
    sedentary = "#3498DB",      # Blue (matches pie)
    light = "#F1C40F",          # Yellow (matches pie)
    moderate = "#E67E22",       # Orange (matches pie)
    vigorous = "#E74C3C",       # Red (matches pie)
    very_vigorous = "#9B59B6"   # Purple (matches pie)
  )

  position_type <- if (stacked) "stack" else "identity"

  # Get cutpoint info for subtitle (use provided subtitle if given)
  if (!is.null(subtitle)) {
    cutpoint_info <- subtitle
  } else {
    cutpoint_info <- switch(cutpoints,
      "freedson" = "Freedson (1998): Sed<100, Light<1952, Mod<5725, Vig<9498 CPM",
      "evenson" = "Evenson (2008): Sed<101, Light<574, Mod<1003 CPM",
      "canhr" = "CANHR (2025): Sed<100, Light<1500, Mod<5000, Vig<9000 CPM",
      "Freedson cut-points"
    )
  }

  # Intensity labels with descriptions
  intensity_labels <- c(
    "sedentary" = "Sedentary (<100 CPM)",
    "light" = "Light Activity",
    "moderate" = "Moderate (MVPA)",
    "vigorous" = "Vigorous (MVPA)",
    "very_vigorous" = "Very Vigorous"
  )

  p <- ggplot2::ggplot(hourly, ggplot2::aes(x = hour, y = minutes, fill = intensity)) +
    ggplot2::geom_area(position = position_type, alpha = if (stacked) 0.8 else 0.6) +
    ggplot2::facet_wrap(~ date, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = intensity_colors,
      labels = intensity_labels,
      name = "Activity Intensity",
      guide = ggplot2::guide_legend(
        title.position = "top",
        title.hjust = 0,
        ncol = 1,
        keywidth = ggplot2::unit(1.2, "cm"),
        keyheight = ggplot2::unit(0.6, "cm")
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = c("12AM", "3AM", "6AM", "9AM", "12PM", "3PM", "6PM", "9PM"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      subtitle = cutpoint_info,
      x = "Time of Day",
      y = "Minutes per Hour",
      caption = "MVPA = Moderate-to-Vigorous Physical Activity"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9, color = "gray50"),
      plot.caption = ggplot2::element_text(size = 8, color = "gray50", hjust = 0),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.5, "lines")
    )

  return(p)
}


#' Quick Visualization Function
#'
#' Generates multiple standard visualizations from a canhrActi analysis result.
#'
#' @param result A canhrActi analysis result object
#' @param plots Character vector of plot types to generate
#'
#' @return A list of ggplot2 objects
#'
#' @export
quick_plots <- function(result,
                        plots = c("timeline", "heatmap", "intensity")) {

  output <- list()

  if ("timeline" %in% plots && "epoch_data" %in% names(result)) {
    output$timeline <- plot_daily_timeline(
      result$epoch_data,
      show_axes = c("axis1"),
      show_cutpoints = TRUE
    )
  }

  if ("heatmap" %in% plots && "epoch_data" %in% names(result)) {
    output$heatmap <- plot_activity_heatmap(result$epoch_data)
  }

  if ("intensity" %in% plots && "epoch_data" %in% names(result)) {
    if ("intensity" %in% names(result$epoch_data)) {
      output$intensity <- plot_intensity_area(result$epoch_data)
    }
  }

  if ("inclinometer" %in% plots && "epoch_data" %in% names(result)) {
    if ("inclinometer" %in% names(result$epoch_data)) {
      output$inclinometer <- plot_inclinometer(result$epoch_data)
    }
  }

  return(output)
}


# LIGHT EXPOSURE VISUALIZATION

#' Light Exposure Visualization
#'
#' Creates comprehensive light exposure visualizations showing lux values
#' over time, with circadian-relevant thresholds highlighted.
#'
#' @param data Data frame with timestamp and lux columns
#' @param timestamp_col Name of timestamp column
#' @param lux_col Name of lux column (default: "lux")
#' @param log_scale Logical. Use logarithmic scale for lux? (default: TRUE)
#' @param show_thresholds Logical. Show circadian light thresholds? (default: TRUE)
#' @param show_daylight Logical. Shade daylight hours? (default: TRUE)
#' @param date_filter Optional date to filter
#' @param title Character. Plot title (default: "Light Exposure Throughout Day")
#'
#' @return A ggplot2 object
#'
#' @details
#' Light thresholds shown (when enabled):
#' \itemize{
#'   \item 10 lux: Minimum for melatonin suppression detection
#'   \item 100 lux: Indoor bright light
#'   \item 1000 lux: Threshold for significant circadian effects
#'   \item 10000 lux: Bright outdoor light
#' }
#'
#' @references
#' Duffy JF, Czeisler CA. Effect of Light on Human Circadian Physiology.
#' Sleep Med Clin. 2009;4(2):165-177.
#'
#' @export
plot_light_exposure <- function(data,
                                 timestamp_col = "timestamp",
                                 lux_col = "lux",
                                 log_scale = TRUE,
                                 show_thresholds = TRUE,
                                 show_daylight = TRUE,
                                 date_filter = NULL,
                                 title = "Light Exposure Throughout Day") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Check lux column exists
  if (!lux_col %in% names(data)) {
    stop("Lux column '", lux_col, "' not found in data")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Filter by date if specified
  if (!is.null(date_filter)) {
    data <- data[as.Date(data[[timestamp_col]]) == as.Date(date_filter), ]
  }

  if (nrow(data) == 0) {
    stop("No data available for the specified parameters")
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60
  data$lux_value <- data[[lux_col]]

  # Replace 0 or negative with small value for log scale
  if (log_scale) {
    data$lux_value[data$lux_value <= 0] <- 0.1
  }

  # Format date labels for faceting
  unique_dates <- sort(unique(data$date))
  data$date_label <- format(data$date, "%A\n%m/%d/%Y")
  data$date_label <- factor(data$date_label,
                            levels = format(unique_dates, "%A\n%m/%d/%Y"))

  # Light color gradient (dark purple to bright yellow)
  light_colors <- c(
    "#1a1a2e",  # Dark (very low light)
    "#16213e",  # Deep blue
    "#0f3460",  # Dark blue
    "#533483",  # Purple
    "#e94560",  # Pink/red
    "#f39422",  # Orange
    "#f1c40f",  # Yellow
    "#f7dc6f"   # Bright yellow
  )

  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = time_of_day, y = lux_value))

  # Add daylight shading (approximate 6 AM - 8 PM)
  if (show_daylight) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = 6, xmax = 20,
        ymin = -Inf, ymax = Inf,
        fill = "#FFF8DC", alpha = 0.3
      )
  }

  # Add area and line
  p <- p +
    ggplot2::geom_area(
      fill = "#F39C12",
      alpha = 0.4
    ) +
    ggplot2::geom_line(
      color = "#E67E22",
      linewidth = 0.5
    )

  # Add threshold lines if requested
  if (show_thresholds) {
    thresholds <- data.frame(
      level = c(10, 100, 1000, 10000),
      label = c("Dim light (10 lux)",
                "Indoor bright (100 lux)",
                "Circadian threshold (1000 lux)",
                "Outdoor bright (10000 lux)"),
      color = c("#9B59B6", "#3498DB", "#27AE60", "#F39C12")
    )

    for (i in 1:nrow(thresholds)) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = thresholds$level[i],
          linetype = "dashed",
          color = thresholds$color[i],
          linewidth = 0.5,
          alpha = 0.7
        )
    }

    # Add threshold legend
    thresholds$x <- 24
    p <- p +
      ggplot2::geom_text(
        data = thresholds,
        ggplot2::aes(x = x, y = level, label = label, color = color),
        hjust = 1, vjust = -0.3, size = 2.5
      ) +
      ggplot2::scale_color_identity()
  }

  # Apply log scale if requested
  if (log_scale) {
    p <- p +
      ggplot2::scale_y_log10(
        breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000),
        labels = c("0", "1", "10", "100", "1K", "10K", "100K"),
        limits = c(0.1, max(data$lux_value, na.rm = TRUE) * 2)
      )
  }

  # Facet if multiple days
  if (length(unique_dates) > 1) {
    p <- p +
      ggplot2::facet_wrap(~ date_label, ncol = 1, scales = "free_y")
  }

  # Format plot
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = if (log_scale) "Lux (log scale)" else "Lux (linear scale)",
      x = "Time of Day",
      y = "Light Intensity (lux)"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#E8E8E8", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = "#CCCCCC")
    )

  return(p)
}


#' Light Exposure Summary Plot
#'
#' Creates a summary visualization of daily light exposure patterns including
#' total exposure at different thresholds and timing metrics.
#'
#' @param data Data frame with timestamp and lux columns
#' @param timestamp_col Name of timestamp column
#' @param lux_col Name of lux column
#'
#' @return A ggplot2 object
#'
#' @export
plot_light_summary <- function(data,
                                timestamp_col = "timestamp",
                                lux_col = "lux") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Calculate epoch length
  time_diffs <- diff(as.numeric(data[[timestamp_col]]))
  epoch_minutes <- median(time_diffs, na.rm = TRUE) / 60

  # Add date column
  data$date <- as.Date(data[[timestamp_col]])
  data$lux_value <- data[[lux_col]]

  # Calculate daily light metrics
  daily_light <- aggregate(
    lux_value ~ date,
    data = data,
    FUN = function(x) {
      list(
        mean_lux = mean(x, na.rm = TRUE),
        max_lux = max(x, na.rm = TRUE),
        min_above_10 = sum(x >= 10, na.rm = TRUE) * epoch_minutes,
        min_above_100 = sum(x >= 100, na.rm = TRUE) * epoch_minutes,
        min_above_1000 = sum(x >= 1000, na.rm = TRUE) * epoch_minutes,
        min_above_10000 = sum(x >= 10000, na.rm = TRUE) * epoch_minutes
      )
    }
  )

  # Extract list columns
  daily_light <- cbind(
    date = daily_light$date,
    as.data.frame(do.call(rbind, daily_light$lux_value))
  )

  # Reshape for plotting
  threshold_data <- data.frame(
    date = rep(daily_light$date, 4),
    threshold = rep(c(">10 lux", ">100 lux", ">1000 lux", ">10000 lux"), each = nrow(daily_light)),
    minutes = c(
      unlist(daily_light$min_above_10),
      unlist(daily_light$min_above_100),
      unlist(daily_light$min_above_1000),
      unlist(daily_light$min_above_10000)
    )
  )

  threshold_data$threshold <- factor(
    threshold_data$threshold,
    levels = c(">10000 lux", ">1000 lux", ">100 lux", ">10 lux")
  )

  # Colors for thresholds
  threshold_colors <- c(
    ">10 lux" = "#9B59B6",
    ">100 lux" = "#3498DB",
    ">1000 lux" = "#27AE60",
    ">10000 lux" = "#F39C12"
  )

  p <- ggplot2::ggplot(threshold_data,
                        ggplot2::aes(x = date, y = minutes, fill = threshold)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_manual(
      values = threshold_colors,
      name = "Light\nThreshold"
    ) +
    ggplot2::scale_x_date(date_labels = "%a\n%m/%d") +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%d min", as.integer(x))
    ) +
    ggplot2::labs(
      title = "Daily Light Exposure Duration",
      subtitle = "Minutes spent at different light intensity thresholds",
      x = "Date",
      y = "Duration"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right"
    )

  return(p)
}


#' Sleep Quality Visualization
#'
#' Creates comprehensive sleep quality plots showing sleep efficiency,
#' WASO, sleep duration, and awakenings.
#'
#' @param sleep_data Data frame with sleep metrics
#' @param date_col Name of date column
#' @param title Plot title
#'
#' @return A ggplot2 object (combined multi-panel plot)
#'
#' @export
plot_sleep_quality <- function(sleep_data,
                                date_col = "date",
                                title = "Sleep Quality Metrics") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Extract date from in_bed_time (character timestamp from Tudor-Locke)
  if ("in_bed_time" %in% names(sleep_data)) {
    d <- sleep_data$in_bed_time
    if (is.character(d)) {
      parsed <- tryCatch(as.POSIXct(d), error = function(e) NULL)
      if (!is.null(parsed)) {
        sleep_data$date <- as.Date(parsed)
      }
    } else if (inherits(d, "POSIXct")) {
      sleep_data$date <- as.Date(d)
    }
  }

  # Fallback date extraction
 if (!"date" %in% names(sleep_data) || all(is.na(sleep_data$date))) {
    sleep_data$date <- as.Date("2024-01-01") + seq_len(nrow(sleep_data)) - 1
  }

  # Map column names from Tudor-Locke output
  if ("sleep_efficiency" %in% names(sleep_data)) {
    sleep_data$efficiency <- as.numeric(sleep_data$sleep_efficiency)
  }
  # wake_time is WASO in minutes
  if ("wake_time" %in% names(sleep_data) && is.numeric(sleep_data$wake_time)) {
    sleep_data$waso <- as.numeric(sleep_data$wake_time)
  }
  # sleep_time is TST in minutes (convert to hours)
  if ("sleep_time" %in% names(sleep_data)) {
    sleep_data$duration_hrs <- as.numeric(sleep_data$sleep_time) / 60
  }
  # number_of_awakenings
  if ("number_of_awakenings" %in% names(sleep_data)) {
    sleep_data$awakenings <- as.numeric(sleep_data$number_of_awakenings)
  }

  # Build long-format data for faceted plot
  metrics_list <- list()

  if ("efficiency" %in% names(sleep_data)) {
    metrics_list$efficiency <- data.frame(
      date = sleep_data$date,
      value = sleep_data$efficiency,
      metric = "Sleep Efficiency (%)",
      threshold = 85,
      stringsAsFactors = FALSE
    )
  }

  if ("waso" %in% names(sleep_data)) {
    metrics_list$waso <- data.frame(
      date = sleep_data$date,
      value = sleep_data$waso,
      metric = "WASO (min)",
      threshold = 30,
      stringsAsFactors = FALSE
    )
  }

  if ("duration_hrs" %in% names(sleep_data)) {
    metrics_list$duration <- data.frame(
      date = sleep_data$date,
      value = sleep_data$duration_hrs,
      metric = "Sleep Duration (hrs)",
      threshold = 7,
      stringsAsFactors = FALSE
    )
  }

  if ("awakenings" %in% names(sleep_data)) {
    metrics_list$awakenings <- data.frame(
      date = sleep_data$date,
      value = sleep_data$awakenings,
      metric = "Awakenings",
      threshold = NA,
      stringsAsFactors = FALSE
    )
  }

  if (length(metrics_list) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No sleep quality metrics available\nin the data",
                          size = 5, color = "gray50") +
        ggplot2::theme_void()
    )
  }

  # Combine into single data frame
  plot_data <- do.call(rbind, metrics_list)
  plot_data$date <- as.Date(plot_data$date, origin = "1970-01-01")

  # Define colors for each metric
  metric_colors <- c(
    "Sleep Efficiency (%)" = "#3498DB",
    "WASO (min)" = "#E74C3C",
    "Sleep Duration (hrs)" = "#9B59B6",
    "Awakenings" = "#F39C12"
  )

  # Create faceted plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value, fill = metric)) +
    ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
    ggplot2::geom_hline(
      data = plot_data[!is.na(plot_data$threshold), ],
      ggplot2::aes(yintercept = threshold),
      linetype = "dashed", color = "#27AE60", linewidth = 0.8
    ) +
    ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 2) +
    ggplot2::scale_fill_manual(values = metric_colors) +
    ggplot2::scale_x_date(date_labels = "%m/%d") +
    ggplot2::labs(
      title = title,
      subtitle = "Green dashed line = recommended threshold",
      x = "Date",
      y = NULL
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50", size = 10),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}


# WEAR TIME VISUALIZATION

#' Wear Time Visualization
#'
#' Creates visualizations showing wear time vs non-wear time patterns.
#'
#' @param data Data frame with timestamp and wear status columns
#' @param timestamp_col Name of timestamp column
#' @param wear_col Name of wear status column (logical or 0/1)
#' @param wear_vector Optional. Alternative wear time vector (default: NULL)
#' @param show_summary Logical. Include wear time summary? (default: TRUE)
#' @param title Character. Plot title (default: "Wear Time Analysis")
#'
#' @return A ggplot2 object or list of ggplot2 objects
#'
#' @export
plot_wear_time <- function(data,
                            timestamp_col = "timestamp",
                            wear_col = "wear",
                            wear_vector = NULL,
                            show_summary = TRUE,
                            title = "Wear Time Analysis") {

  # Handle wear_vector parameter (from dashboard)
  if (!is.null(wear_vector)) {
    data$wear <- wear_vector
    wear_col <- "wear"
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60

  # Convert wear to numeric
  data$wear_status <- as.numeric(data[[wear_col]])

  # Format for faceting
  unique_dates <- sort(unique(data$date))
  data$date_label <- format(data$date, "%A\n%m/%d/%Y")
  data$date_label <- factor(data$date_label,
                            levels = format(unique_dates, "%A\n%m/%d/%Y"))

  plots <- list()

  # 1. Wear time heatmap by day
  wear_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = time_of_day, y = 1, fill = factor(wear_status))
  ) +
    ggplot2::geom_tile(height = 1) +
    ggplot2::facet_wrap(~ date_label, ncol = 1, strip.position = "left") +
    ggplot2::scale_fill_manual(
      values = c("0" = "#E74C3C", "1" = "#27AE60"),
      labels = c("0" = "Non-wear", "1" = "Wear"),
      name = "Status"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Wear Time Pattern",
      subtitle = "Green = Wear, Red = Non-wear",
      x = "Time of Day",
      y = ""
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )

  plots$wear_pattern <- wear_plot

  # 2. Daily wear time summary
  if (show_summary) {
    # Calculate epoch length
    time_diffs <- diff(as.numeric(data[[timestamp_col]]))
    epoch_minutes <- median(time_diffs, na.rm = TRUE) / 60

    daily_wear <- aggregate(
      wear_status ~ date,
      data = data,
      FUN = function(x) sum(x) * epoch_minutes / 60  # Convert to hours
    )
    names(daily_wear)[2] <- "wear_hours"

    daily_wear$valid <- daily_wear$wear_hours >= 10  # 10 hour threshold

    summary_plot <- ggplot2::ggplot(
      daily_wear,
      ggplot2::aes(x = date, y = wear_hours, fill = valid)
    ) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "#3498DB") +
      ggplot2::annotate("text", x = min(daily_wear$date), y = 10,
                        label = "10 hr threshold", hjust = 0, vjust = -0.5,
                        color = "#3498DB", size = 3) +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60"),
        labels = c("FALSE" = "Invalid (<10 hr)", "TRUE" = "Valid (\u226510 hr)"),
        name = "Validity"
      ) +
      ggplot2::scale_x_date(date_labels = "%a\n%m/%d") +
      ggplot2::scale_y_continuous(limits = c(0, 24)) +
      ggplot2::labs(
        title = "Daily Wear Time",
        subtitle = "Hours of valid wear time per day",
        x = "Date",
        y = "Wear Time (hours)"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "right"
      )

    plots$wear_summary <- summary_plot
  }

  if (length(plots) == 1) {
    return(plots[[1]])
  }

  # Combine plots - wear pattern on top, summary on bottom
  n_days <- length(unique_dates)
  height_ratio <- c(max(3, n_days), 1.5)  # Scale pattern height with days

  if (requireNamespace("patchwork", quietly = TRUE)) {
    # Remove duplicate titles for cleaner look
    plots$wear_pattern <- plots$wear_pattern + ggplot2::labs(title = NULL, subtitle = NULL)
    combined <- patchwork::wrap_plots(plots, ncol = 1, heights = height_ratio) +
      patchwork::plot_annotation(
        title = title,
        subtitle = "Green = Wear, Red = Non-wear",
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50")
        )
      )
    return(combined)
  } else if (requireNamespace("cowplot", quietly = TRUE)) {
    combined <- cowplot::plot_grid(plotlist = plots, ncol = 1, rel_heights = height_ratio)
    return(combined)
  } else {
    return(plots$wear_pattern)
  }

}


# STEPS VISUALIZATION

#' Steps Visualization
#'
#' Creates comprehensive step count visualizations with daily goals and trends.
#'
#' @param data Data frame with timestamp and steps columns
#' @param timestamp_col Name of timestamp column
#' @param steps_col Name of steps column
#' @param daily_goal Daily step goal (default: 10000)
#' @param show_cumulative Logical. Show cumulative steps? (default: TRUE)
#' @param title Character. Plot title (default: "Steps Analysis")
#'
#' @return A ggplot2 object or list of ggplot2 objects
#'
#' @export
plot_steps <- function(data,
                        timestamp_col = "timestamp",
                        steps_col = "steps",
                        daily_goal = 10000,
                        show_cumulative = TRUE,
                        title = "Steps Analysis") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60
  data$steps_value <- data[[steps_col]]

  plots <- list()

  # 1. Daily total steps bar chart
  daily_steps <- aggregate(steps_value ~ date, data = data, FUN = sum, na.rm = TRUE)
  daily_steps$met_goal <- daily_steps$steps_value >= daily_goal
  daily_steps$percent_goal <- pmin(daily_steps$steps_value / daily_goal * 100, 150)

  daily_plot <- ggplot2::ggplot(
    daily_steps,
    ggplot2::aes(x = date, y = steps_value, fill = met_goal)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = daily_goal, linetype = "dashed", color = "#9B59B6", linewidth = 1) +
    ggplot2::annotate("text", x = min(daily_steps$date), y = daily_goal,
                      label = paste0(format(daily_goal, big.mark = ","), " step goal"),
                      hjust = 0, vjust = -0.5, color = "#9B59B6", size = 3) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "#E74C3C", "TRUE" = "#32CD32"),
      labels = c("FALSE" = "Below goal", "TRUE" = "Goal met"),
      name = ""
    ) +
    ggplot2::scale_x_date(date_labels = "%a\n%m/%d") +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(
      title = "Daily Step Count",
      subtitle = sprintf("Goal: %s steps/day", format(daily_goal, big.mark = ",")),
      x = "Date",
      y = "Steps"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  plots$daily <- daily_plot

  # 2. Cumulative steps throughout each day
  if (show_cumulative) {
    # Calculate cumulative steps per day
    data <- data[order(data[[timestamp_col]]), ]
    data$cumulative_steps <- ave(data$steps_value, data$date,
                                  FUN = function(x) cumsum(replace(x, is.na(x), 0)))

    # Format for faceting
    unique_dates <- sort(unique(data$date))
    data$date_label <- format(data$date, "%A %m/%d")
    data$date_label <- factor(data$date_label,
                              levels = format(unique_dates, "%A %m/%d"))

    cumulative_plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = time_of_day, y = cumulative_steps)
    ) +
      ggplot2::geom_area(fill = "#32CD32", alpha = 0.4) +
      ggplot2::geom_line(color = "#228B22", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = daily_goal, linetype = "dashed", color = "#9B59B6") +
      ggplot2::facet_wrap(~ date_label, ncol = 2) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 24, 6),
        labels = c("12 AM", "6 AM", "12 PM", "6 PM", "12 AM"),
        limits = c(0, 24)
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::labs(
        title = "Cumulative Steps Throughout Day",
        subtitle = "Purple line indicates daily goal",
        x = "Time of Day",
        y = "Cumulative Steps"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        strip.text = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )

    plots$cumulative <- cumulative_plot
  }

  # 3. Step rate (steps per minute) throughout day
  # Format for faceting
  unique_dates <- sort(unique(data$date))
  data$date_label <- format(data$date, "%A %m/%d")
  data$date_label <- factor(data$date_label,
                            levels = format(unique_dates, "%A %m/%d"))

  rate_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = time_of_day, y = steps_value)
  ) +
    ggplot2::geom_line(color = "#32CD32", linewidth = 0.4, alpha = 0.8) +
    ggplot2::facet_wrap(~ date_label, ncol = 1) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"),
      limits = c(0, 24)
    ) +
    ggplot2::labs(
      title = "Steps Throughout Day",
      x = "Time of Day",
      y = "Steps per epoch"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      panel.grid.minor = ggplot2::element_blank()
    )

  plots$rate <- rate_plot

  return(plots)
}


# HEART RATE VISUALIZATION

#' Heart Rate Visualization
#'
#' Creates heart rate visualizations with zones and activity correlation.
#'
#' @param data Data frame with timestamp and heart rate columns
#' @param timestamp_col Name of timestamp column
#' @param hr_col Name of heart rate column
#' @param age Participant age (for zone calculation)
#' @param show_zones Logical. Show heart rate zones? (default: TRUE)
#' @param counts_col Optional. Activity counts column for correlation plot.
#'
#' @return A ggplot2 object or list of ggplot2 objects
#'
#' @export
plot_heart_rate <- function(data,
                             timestamp_col = "timestamp",
                             hr_col = "hr",
                             age = NULL,
                             show_zones = TRUE,
                             counts_col = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60
  data$hr_value <- data[[hr_col]]

  # Calculate HR zones if age provided
  if (!is.null(age) && show_zones) {
    max_hr <- 220 - age
    zones <- data.frame(
      zone = c("Rest", "Fat Burn", "Cardio", "Peak"),
      lower = c(0, 0.5, 0.7, 0.85) * max_hr,
      upper = c(0.5, 0.7, 0.85, 1.0) * max_hr,
      color = c("#3498DB", "#27AE60", "#F39C12", "#E74C3C")
    )
  }

  plots <- list()

  # Format for faceting
  unique_dates <- sort(unique(data$date))
  data$date_label <- format(data$date, "%A\n%m/%d/%Y")
  data$date_label <- factor(data$date_label,
                            levels = format(unique_dates, "%A\n%m/%d/%Y"))

  # 1. HR timeline with zones
  hr_plot <- ggplot2::ggplot(data, ggplot2::aes(x = time_of_day, y = hr_value))

  # Add zone backgrounds if available
  if (!is.null(age) && show_zones) {
    for (i in 1:nrow(zones)) {
      hr_plot <- hr_plot +
        ggplot2::annotate(
          "rect",
          xmin = 0, xmax = 24,
          ymin = zones$lower[i], ymax = zones$upper[i],
          fill = zones$color[i], alpha = 0.1
        )
    }
  }

  hr_plot <- hr_plot +
    ggplot2::geom_line(color = "#DC143C", linewidth = 0.4) +
    ggplot2::facet_wrap(~ date_label, ncol = 1) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"),
      limits = c(0, 24)
    ) +
    ggplot2::labs(
      title = "Heart Rate Throughout Day",
      subtitle = if (!is.null(age)) sprintf("Age: %d, Max HR: %d bpm", age, 220 - age) else NULL,
      x = "Time of Day",
      y = "Heart Rate (bpm)"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      panel.grid.minor = ggplot2::element_blank()
    )

  plots$timeline <- hr_plot

  # 2. HR zone distribution
  if (!is.null(age) && show_zones) {
    # Classify each epoch into zones
    data$zone <- cut(
      data$hr_value,
      breaks = c(0, zones$upper),
      labels = zones$zone,
      include.lowest = TRUE
    )

    zone_summary <- as.data.frame(table(data$zone))
    names(zone_summary) <- c("zone", "count")
    zone_summary$percent <- zone_summary$count / sum(zone_summary$count) * 100
    zone_summary$zone <- factor(zone_summary$zone, levels = zones$zone)

    zone_plot <- ggplot2::ggplot(
      zone_summary,
      ggplot2::aes(x = zone, y = percent, fill = zone)
    ) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::scale_fill_manual(values = setNames(zones$color, zones$zone), guide = "none") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percent)),
                         vjust = -0.5, size = 3) +
      ggplot2::scale_y_continuous(limits = c(0, max(zone_summary$percent) * 1.15)) +
      ggplot2::labs(
        title = "Time in Heart Rate Zones",
        x = "Zone",
        y = "Percentage of Time"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )

    plots$zones <- zone_plot
  }

  # 3. HR vs Activity correlation
  if (!is.null(counts_col) && counts_col %in% names(data)) {
    correlation_plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data[[counts_col]], y = hr_value)
    ) +
      ggplot2::geom_point(alpha = 0.1, size = 0.5, color = "#DC143C") +
      ggplot2::geom_smooth(method = "loess", color = "#3498DB", se = TRUE) +
      ggplot2::labs(
        title = "Heart Rate vs Activity",
        x = "Activity Counts",
        y = "Heart Rate (bpm)"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )

    plots$correlation <- correlation_plot
  }

  return(plots)
}


# INTERACTIVE SHINY DASHBOARD

#' Launch Interactive Visualization Dashboard
#'
#' Creates and launches an interactive Shiny dashboard for exploring
#' accelerometer data, similar to ActiLife's graphing interface but
#' with enhanced features.
#'
#' @param data Data frame with accelerometer data (optional - can load in app)
#' @param launch Logical. Automatically launch the app? (default: TRUE)
#'
#' @return A Shiny app object
#'
#' @details
#' The dashboard includes:
#' \itemize{
#'   \item Multi-day activity timeline with metric selection
#'   \item Interactive date range and metric toggles
#'   \item Cut-point overlay options
#'   \item Inclinometer/posture visualization
#'   \item Light exposure plots
#'   \item Sleep period overlay
#'   \item Wear time visualization
#'   \item Export options for all plots
#' }
#'
#' @export
launch_visualization_dashboard <- function(data = NULL, launch = TRUE) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the interactive dashboard")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("canhrActi Advanced Visualization Dashboard"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        # File upload if no data provided
        shiny::conditionalPanel(
          condition = "typeof input.uploaded_data == 'undefined' || input.uploaded_data == null",
          shiny::fileInput("data_file", "Load Data (RDS/CSV)",
                           accept = c(".rds", ".csv"))
        ),

        shiny::hr(),

        # Date selection
        shiny::dateRangeInput("date_range", "Date Range",
                               start = Sys.Date() - 7,
                               end = Sys.Date()),

        shiny::hr(),

        # Metric selection
        shiny::h4("Display Metrics"),
        shiny::checkboxGroupInput(
          "metrics",
          NULL,
          choices = c(
            "Axis 1" = "axis1",
            "Axis 2" = "axis2",
            "Axis 3" = "axis3",
            "Vector Magnitude" = "vm",
            "Steps" = "steps",
            "Heart Rate" = "hr",
            "Light (lux)" = "lux"
          ),
          selected = "axis1"
        ),

        shiny::hr(),

        # Cut-points
        shiny::checkboxInput("show_cutpoints", "Show Cut-point Lines", TRUE),
        shiny::selectInput(
          "cutpoint_set",
          "Cut-point Set",
          choices = c(
            "Freedson Adult (1998)" = "freedson_adult",
            "Freedson Children (2005)" = "freedson_child",
            "Troiano (2008)" = "troiano",
            "Matthews (2005)" = "matthews",
            "Custom" = "custom"
          )
        ),

        shiny::hr(),

        # Overlays
        shiny::h4("Overlays"),
        shiny::checkboxInput("show_sleep", "Show Sleep Periods", FALSE),
        shiny::checkboxInput("show_inclinometer", "Show Posture", FALSE),
        shiny::checkboxInput("show_nonwear", "Highlight Non-wear", FALSE),

        shiny::hr(),

        # Export
        shiny::downloadButton("download_plot", "Download Current Plot"),
        shiny::downloadButton("download_all", "Download All Plots")
      ),

      shiny::mainPanel(
        width = 9,

        shiny::tabsetPanel(
          id = "main_tabs",

          # Timeline tab
          shiny::tabPanel(
            "Activity Timeline",
            shiny::plotOutput("timeline_plot", height = "600px"),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(6,
                shiny::sliderInput("y_max", "Y-axis Maximum",
                                   min = 100, max = 20000, value = 5000, step = 100)
              ),
              shiny::column(6,
                shiny::checkboxInput("equal_scales", "Equal Y-scales Across Days", TRUE)
              )
            )
          ),

          # Inclinometer tab
          shiny::tabPanel(
            "Inclinometer/Posture",
            shiny::fluidRow(
              shiny::column(4, shiny::plotOutput("incl_pie", height = "400px")),
              shiny::column(8, shiny::plotOutput("incl_hourly", height = "400px"))
            )
          ),

          # Light exposure tab
          shiny::tabPanel(
            "Light Exposure",
            shiny::plotOutput("light_plot", height = "500px"),
            shiny::hr(),
            shiny::plotOutput("light_summary", height = "300px")
          ),

          # Steps tab
          shiny::tabPanel(
            "Steps",
            shiny::fluidRow(
              shiny::column(6, shiny::plotOutput("steps_daily", height = "350px")),
              shiny::column(6, shiny::plotOutput("steps_cumulative", height = "350px"))
            ),
            shiny::hr(),
            shiny::numericInput("step_goal", "Daily Step Goal", value = 10000, min = 1000, max = 30000)
          ),

          # Heart rate tab
          shiny::tabPanel(
            "Heart Rate",
            shiny::plotOutput("hr_plot", height = "400px"),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(4, shiny::plotOutput("hr_zones", height = "300px")),
              shiny::column(8, shiny::plotOutput("hr_correlation", height = "300px"))
            ),
            shiny::numericInput("age", "Participant Age (for zones)", value = 30, min = 1, max = 120)
          ),

          # Heatmap tab
          shiny::tabPanel(
            "Activity Heatmap",
            shiny::plotOutput("heatmap_plot", height = "600px"),
            shiny::selectInput("heatmap_metric", "Metric",
                               choices = c("Axis 1" = "axis1", "Steps" = "steps", "VM" = "vm"))
          ),

          # Sleep tab
          shiny::tabPanel(
            "Sleep Analysis",
            shiny::plotOutput("sleep_overlay", height = "500px"),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(6, shiny::plotOutput("sleep_efficiency", height = "300px")),
              shiny::column(6, shiny::plotOutput("sleep_timing", height = "300px"))
            )
          ),

          # Summary tab
          shiny::tabPanel(
            "Daily Summary",
            shiny::plotOutput("summary_bars", height = "400px"),
            shiny::hr(),
            shiny::tableOutput("summary_table")
          )
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Reactive data
    app_data <- shiny::reactiveVal(data)

    # Load uploaded data
    shiny::observeEvent(input$data_file, {
      req(input$data_file)
      file_path <- input$data_file$datapath
      ext <- tools::file_ext(input$data_file$name)

      new_data <- if (ext == "rds") {
        readRDS(file_path)
      } else if (ext == "csv") {
        read.csv(file_path, stringsAsFactors = FALSE)
      }

      app_data(new_data)

      # Update date range
      if ("timestamp" %in% names(new_data)) {
        dates <- as.Date(new_data$timestamp)
        shiny::updateDateRangeInput(session, "date_range",
                                     start = min(dates), end = max(dates))
      }
    })

    # Filtered data
    filtered_data <- shiny::reactive({
      req(app_data())
      d <- app_data()

      if ("timestamp" %in% names(d)) {
        if (!inherits(d$timestamp, "POSIXct")) {
          d$timestamp <- as.POSIXct(d$timestamp)
        }
        d <- d[as.Date(d$timestamp) >= input$date_range[1] &
               as.Date(d$timestamp) <= input$date_range[2], ]
      }

      d
    })

    # Get cut-points based on selection
    get_cutpoints <- shiny::reactive({
      switch(input$cutpoint_set,
        freedson_adult = c(sedentary = 100, light = 1952, moderate = 5725, vigorous = 9498),
        freedson_child = c(sedentary = 100, light = 500, moderate = 4000, vigorous = 7600),
        troiano = c(sedentary = 100, light = 2020, moderate = 5999, vigorous = 9498),
        matthews = c(sedentary = 100, light = 760, moderate = 5999, vigorous = 9498),
        custom = c(sedentary = 100, light = 2000, moderate = 5000, vigorous = 9000)
      )
    })

    # Timeline plot
    output$timeline_plot <- shiny::renderPlot({
      req(filtered_data())

      plot_daily_timeline(
        filtered_data(),
        show_axes = input$metrics,
        show_cutpoints = input$show_cutpoints,
        cutpoints = get_cutpoints(),
        show_inclinometer = input$show_inclinometer,
        equal_scales = input$equal_scales,
        max_counts = input$y_max
      )
    })

    # Inclinometer plots
    output$incl_pie <- shiny::renderPlot({
      req(filtered_data(), "inclinometer" %in% names(filtered_data()))
      plots <- plot_inclinometer(filtered_data(), show_pie = TRUE, show_hourly = FALSE)
      if (is.list(plots)) plots$pie else plots
    })

    output$incl_hourly <- shiny::renderPlot({
      req(filtered_data(), "inclinometer" %in% names(filtered_data()))
      plots <- plot_inclinometer(filtered_data(), show_pie = FALSE, show_hourly = TRUE)
      if (is.list(plots)) plots[[1]] else plots
    })

    # Light exposure plots
    output$light_plot <- shiny::renderPlot({
      req(filtered_data(), "lux" %in% names(filtered_data()))
      plot_light_exposure(filtered_data())
    })

    output$light_summary <- shiny::renderPlot({
      req(filtered_data(), "lux" %in% names(filtered_data()))
      plot_light_summary(filtered_data())
    })

    # Steps plots
    output$steps_daily <- shiny::renderPlot({
      req(filtered_data(), "steps" %in% names(filtered_data()))
      plots <- plot_steps(filtered_data(), daily_goal = input$step_goal)
      plots$daily
    })

    output$steps_cumulative <- shiny::renderPlot({
      req(filtered_data(), "steps" %in% names(filtered_data()))
      plots <- plot_steps(filtered_data(), daily_goal = input$step_goal)
      plots$cumulative
    })

    # Heart rate plots
    output$hr_plot <- shiny::renderPlot({
      req(filtered_data(), "hr" %in% names(filtered_data()))
      plots <- plot_heart_rate(filtered_data(), age = input$age)
      plots$timeline
    })

    output$hr_zones <- shiny::renderPlot({
      req(filtered_data(), "hr" %in% names(filtered_data()))
      plots <- plot_heart_rate(filtered_data(), age = input$age)
      if ("zones" %in% names(plots)) plots$zones else NULL
    })

    output$hr_correlation <- shiny::renderPlot({
      req(filtered_data(), "hr" %in% names(filtered_data()))
      plots <- plot_heart_rate(filtered_data(), age = input$age, counts_col = "axis1")
      if ("correlation" %in% names(plots)) plots$correlation else NULL
    })

    # Heatmap
    output$heatmap_plot <- shiny::renderPlot({
      req(filtered_data())
      plot_activity_heatmap(filtered_data(), counts_col = input$heatmap_metric)
    })

    # Sleep plots (placeholder - would need sleep data)
    output$sleep_overlay <- shiny::renderPlot({
      req(filtered_data())
      # Placeholder - would need actual sleep period data
      plot_daily_timeline(filtered_data(), show_axes = "axis1")
    })

    # Download handlers
    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        paste0("canhrActi_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        # Get current tab and save appropriate plot
        current_tab <- input$main_tabs
        p <- switch(current_tab,
          "Activity Timeline" = plot_daily_timeline(filtered_data(), show_axes = input$metrics),
          "Activity Heatmap" = plot_activity_heatmap(filtered_data()),
          plot_daily_timeline(filtered_data())  # default
        )
        ggplot2::ggsave(file, p, width = 12, height = 8, dpi = 300)
      }
    )
  }

  # Create and optionally launch app
  app <- shiny::shinyApp(ui = ui, server = server)

  if (launch) {
    shiny::runApp(app)
  }

  invisible(app)
}


# COMPARISON PLOTS

#' Day-to-Day Comparison Plot
#'
#' Creates side-by-side or overlay comparison of activity patterns
#' across multiple days.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param counts_col Name of counts column
#' @param dates Character vector of dates to compare (max 7)
#' @param comparison_type Type of comparison: "overlay" or "facet"
#' @param title Character. Plot title (default: "Day Comparison")
#'
#' @return A ggplot2 object
#'
#' @export
plot_day_comparison <- function(data,
                                 timestamp_col = "timestamp",
                                 counts_col = "axis1",
                                 dates = NULL,
                                 comparison_type = c("overlay", "facet"),
                                 title = "Day Comparison") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  comparison_type <- match.arg(comparison_type)

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$time_of_day <- as.numeric(format(data[[timestamp_col]], "%H")) +
                      as.numeric(format(data[[timestamp_col]], "%M")) / 60
  data$activity <- data[[counts_col]]

  # Select dates if not specified
  if (is.null(dates)) {
    dates <- sort(unique(data$date))[1:min(7, length(unique(data$date)))]
  }

  # Filter to selected dates
  data <- data[data$date %in% as.Date(dates), ]
  data$date_label <- format(data$date, "%a %m/%d")
  data$date_label <- factor(data$date_label)

  # Color palette for days
  day_colors <- c("#E74C3C", "#3498DB", "#27AE60", "#F39C12", "#9B59B6", "#1ABC9C", "#E67E22")

  if (comparison_type == "overlay") {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = time_of_day, y = activity, color = date_label)
    ) +
      ggplot2::geom_line(linewidth = 0.5, alpha = 0.7) +
      ggplot2::scale_color_manual(values = day_colors[1:length(unique(data$date_label))],
                                   name = "Day") +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 24, 4),
        labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"),
        limits = c(0, 24)
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Overlay view",
        x = "Time of Day",
        y = "Activity (counts)"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        legend.position = "right"
      )
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = time_of_day, y = activity)
    ) +
      ggplot2::geom_line(color = "#1E90FF", linewidth = 0.4) +
      ggplot2::facet_wrap(~ date_label, ncol = 2) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 24, 6),
        labels = c("12 AM", "6 AM", "12 PM", "6 PM", "12 AM"),
        limits = c(0, 24)
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Faceted view",
        x = "Time of Day",
        y = "Activity (counts)"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        strip.text = ggplot2::element_text(face = "bold")
      )
  }

  return(p)
}


#' Weekend vs Weekday Comparison
#'
#' Creates comparison visualizations of activity patterns between
#' weekdays and weekends.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param counts_col Name of counts column
#' @param aggregate_func Function to aggregate activity (default: mean)
#' @param title Character. Plot title (default: "Weekend vs Weekday Activity")
#'
#' @return A ggplot2 object
#'
#' @export
plot_weekend_weekday <- function(data,
                                  timestamp_col = "timestamp",
                                  counts_col = "axis1",
                                  aggregate_func = mean,
                                  title = "Weekend vs Weekday Activity") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$weekday <- weekdays(data$date)
  data$day_type <- ifelse(data$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  data$activity <- data[[counts_col]]

  # Aggregate by hour and day type
  hourly <- aggregate(
    activity ~ hour + day_type,
    data = data,
    FUN = aggregate_func,
    na.rm = TRUE
  )

  # Calculate confidence intervals
  hourly_ci <- aggregate(
    activity ~ hour + day_type,
    data = data,
    FUN = function(x) {
      c(mean = mean(x, na.rm = TRUE),
        se = sd(x, na.rm = TRUE) / sqrt(length(x)))
    }
  )
  hourly_ci <- cbind(hourly_ci[, 1:2], as.data.frame(hourly_ci$activity))
  hourly_ci$lower <- hourly_ci$mean - 1.96 * hourly_ci$se
  hourly_ci$upper <- hourly_ci$mean + 1.96 * hourly_ci$se

  p <- ggplot2::ggplot(hourly_ci, ggplot2::aes(x = hour, y = mean, color = day_type, fill = day_type)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c("Weekday" = "#3498DB", "Weekend" = "#E74C3C"), name = "") +
    ggplot2::scale_fill_manual(values = c("Weekday" = "#3498DB", "Weekend" = "#E74C3C"), name = "") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 4),
      labels = sprintf("%02d:00", seq(0, 23, 4))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Mean activity with 95% confidence interval",
      x = "Hour of Day",
      y = sprintf("Mean Activity (%s)", counts_col)
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  return(p)
}


# EXPORT UTILITIES

#' Export All Visualizations
#'
#' Generates and saves all standard visualizations to a specified directory.
#'
#' @param data Data frame with accelerometer data
#' @param output_dir Directory to save plots
#' @param format Output format: "png", "pdf", or "svg"
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution for raster formats
#'
#' @return Invisible list of generated file paths
#'
#' @export
export_all_plots <- function(data,
                              output_dir = "canhrActi_plots",
                              format = c("png", "pdf", "svg"),
                              width = 12,
                              height = 8,
                              dpi = 300) {

  format <- match.arg(format)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  saved_files <- list()

  # Helper to save plot
  save_plot <- function(p, name) {
    file_path <- file.path(output_dir, paste0(name, ".", format))
    ggplot2::ggsave(file_path, p, width = width, height = height, dpi = dpi)
    saved_files[[name]] <<- file_path
    message("Saved: ", file_path)
  }

  # Generate and save each plot type
  tryCatch({
    save_plot(plot_daily_timeline(data), "01_daily_timeline")
  }, error = function(e) message("Skipped daily_timeline: ", e$message))

  tryCatch({
    save_plot(plot_activity_heatmap(data), "02_activity_heatmap")
  }, error = function(e) message("Skipped activity_heatmap: ", e$message))

  if ("inclinometer" %in% names(data)) {
    tryCatch({
      plots <- plot_inclinometer(data)
      if (is.list(plots) && "pie" %in% names(plots)) {
        save_plot(plots$pie, "03_inclinometer_pie")
      }
    }, error = function(e) message("Skipped inclinometer: ", e$message))
  }

  if ("lux" %in% names(data)) {
    tryCatch({
      save_plot(plot_light_exposure(data), "04_light_exposure")
      save_plot(plot_light_summary(data), "05_light_summary")
    }, error = function(e) message("Skipped light plots: ", e$message))
  }

  if ("steps" %in% names(data)) {
    tryCatch({
      plots <- plot_steps(data)
      save_plot(plots$daily, "06_steps_daily")
      save_plot(plots$cumulative, "07_steps_cumulative")
    }, error = function(e) message("Skipped steps plots: ", e$message))
  }

  if ("hr" %in% names(data)) {
    tryCatch({
      plots <- plot_heart_rate(data, age = 30)
      save_plot(plots$timeline, "08_heart_rate_timeline")
    }, error = function(e) message("Skipped heart rate plots: ", e$message))
  }

  if ("intensity" %in% names(data)) {
    tryCatch({
      save_plot(plot_intensity_area(data), "09_intensity_distribution")
    }, error = function(e) message("Skipped intensity plot: ", e$message))
  }

  tryCatch({
    save_plot(plot_day_comparison(data, comparison_type = "overlay"), "10_day_comparison_overlay")
    save_plot(plot_day_comparison(data, comparison_type = "facet"), "11_day_comparison_facet")
  }, error = function(e) message("Skipped day comparison: ", e$message))

  tryCatch({
    save_plot(plot_weekend_weekday(data), "12_weekend_weekday")
  }, error = function(e) message("Skipped weekend_weekday: ", e$message))

  message("\nExported ", length(saved_files), " plots to: ", output_dir)
  invisible(saved_files)
}


#' Create Visualization Report
#'
#' Generates an HTML or PDF report containing all visualizations
#' with annotations and summaries.
#'
#' @param data Data frame with accelerometer data
#' @param output_file Output file path
#' @param title Report title
#' @param author Report author
#'
#' @return Invisible path to generated report
#'
#' @export
create_visualization_report <- function(data,
                                         output_file = "canhrActi_report.html",
                                         title = "Accelerometer Data Visualization Report",
                                         author = "canhrActi") {

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for report generation")
  }

  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")

  rmd_content <- sprintf('---
title: "%s"
author: "%s"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: flatly
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 6)
library(ggplot2)
```

# Overview

This report presents visualizations of accelerometer data processed with the canhrActi package.

# Daily Activity Timeline

```{r timeline}
plot_daily_timeline(data, show_axes = c("axis1"))
```

# Activity Heatmap

```{r heatmap}
plot_activity_heatmap(data)
```

# Day-to-Day Comparison

```{r comparison}
plot_day_comparison(data, comparison_type = "overlay")
```

# Weekend vs Weekday Patterns

```{r weekend}
plot_weekend_weekday(data)
```

---

', title, author)

  writeLines(rmd_content, temp_rmd)

  # Create environment with data
  report_env <- new.env()
  report_env$data <- data

  # Render report
  output_format <- if (grepl("\\.pdf$", output_file)) "pdf_document" else "html_document"

  rmarkdown::render(
    temp_rmd,
    output_file = output_file,
    output_dir = dirname(output_file),
    envir = report_env,
    quiet = TRUE
  )

  message("Report generated: ", output_file)
  invisible(output_file)
}


# INTENSITY PIE CHART (Professor's Suggestion)

#' Activity Intensity Pie Chart
#'
#' Creates a pie chart showing the distribution of time spent in each
#' activity intensity category (sedentary, light, moderate, vigorous, very vigorous).
#'
#' @param data Data frame with timestamp and counts columns
#' @param timestamp_col Name of timestamp column
#' @param counts_col Name of counts column (default: "axis1")
#' @param cutpoints Named vector of cut-points or preset name
#' @param epoch_length Epoch length in seconds (default: auto-detect)
#' @param show_labels Logical. Show percentage and time labels? (default: TRUE)
#' @param intensity Optional. Pre-computed intensity classification vector (default: NULL)
#' @param donut_style Logical. Display as donut chart instead of pie? (default: TRUE)
#' @param show_mvpa_goal Logical. Show MVPA goal indicator? (default: TRUE)
#' @param mvpa_goal_minutes Numeric. Daily MVPA goal in minutes (default: 30)
#' @param show_thresholds Logical. Show intensity threshold labels? (default: TRUE)
#' @param colorblind_safe Logical. Use colorblind-friendly colors? (default: FALSE)
#' @param title Character. Plot title (default: "Activity Intensity Distribution")
#' @param subtitle Character. Optional plot subtitle (default: NULL)
#'
#' @return A ggplot2 object
#'
#' @details
#' Classifies each epoch into intensity categories based on activity counts
#' and displays the proportion of time in each category. The default cut-points
#' are Freedson Adult (1998):
#' \itemize{
#'   \item Sedentary: 0-99 counts/min
#'   \item Light: 100-1951 counts/min
#'   \item Moderate: 1952-5724 counts/min
#'   \item Vigorous: 5725-9498 counts/min
#'   \item Very Vigorous: >9498 counts/min
#' }
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' plot_intensity_pie(results$epoch_data)
#'
#' # With custom cut-points
#' plot_intensity_pie(results$epoch_data,
#'                    cutpoints = c(sedentary = 100, light = 2000,
#'                                  moderate = 5000, vigorous = 9000))
#' }
#'
#' @export
plot_intensity_pie <- function(data,
                                timestamp_col = "timestamp",
                                counts_col = "axis1",
                                cutpoints = "freedson",
                                epoch_length = NULL,
                                show_labels = TRUE,
                                intensity = NULL,
                                donut_style = TRUE,
                                show_mvpa_goal = TRUE,
                                mvpa_goal_minutes = 30,
                                show_thresholds = TRUE,
                                colorblind_safe = FALSE,
                                title = "Activity Intensity Distribution",
                                subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Auto-detect epoch length
  if (is.null(epoch_length)) {
    time_diffs <- diff(as.numeric(data[[timestamp_col]]))
    epoch_length <- median(time_diffs, na.rm = TRUE)
  }
  epoch_minutes <- epoch_length / 60

  # Get cut-point values for thresholds display
  if (is.character(cutpoints)) {
    cp <- switch(cutpoints,
      "freedson" = c(0, 100, 1952, 5725, 9498, Inf),
      "evenson" = c(0, 101, 574, 1003, Inf, Inf),
      "troiano" = c(0, 100, 2020, 5999, 9498, Inf),
      "matthews" = c(0, 100, 760, 5999, 9498, Inf),
      "canhr" = c(0, 100, 1500, 5000, 9000, Inf),
      c(0, 100, 1952, 5725, 9498, Inf)  # default to freedson
    )
    cp_name <- switch(cutpoints,
      "freedson" = "Freedson Adult (1998)",
      "evenson" = "Evenson Children (2008)",
      "troiano" = "Troiano (2008)",
      "matthews" = "Matthews (2005)",
      "canhr" = "CANHR (2025)",
      cutpoints
    )
  } else if (is.numeric(cutpoints)) {
    cp <- c(0, cutpoints, Inf)
    if (length(cp) < 6) cp <- c(cp, rep(Inf, 6 - length(cp)))
    cp_name <- "Custom"
  } else {
    cp <- c(0, 100, 1952, 5725, 9498, Inf)
    cp_name <- "Freedson Adult (1998)"
  }

  # Use pre-calculated intensity if provided, otherwise calculate from counts
  if (!is.null(intensity)) {
    # Use provided intensity vector (from Activity Analysis)
    # Standardize labels
    intensity_clean <- as.character(intensity)
    intensity_clean <- gsub("^sed.*", "Sedentary", intensity_clean, ignore.case = TRUE)
    intensity_clean <- gsub("^light$", "Light", intensity_clean, ignore.case = TRUE)
    intensity_clean <- gsub("^mod.*", "Moderate", intensity_clean, ignore.case = TRUE)
    intensity_clean <- gsub("^vig.*", "Vigorous", intensity_clean, ignore.case = TRUE)
    intensity_clean <- gsub("^very.*", "Very Vigorous", intensity_clean, ignore.case = TRUE)
    data$intensity <- factor(intensity_clean,
                             levels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"))
  } else {
    # Calculate from counts using cut-points
    # Classify intensity
    counts_vals <- data[[counts_col]]
    if (!is.null(epoch_length) && !is.na(epoch_length) && epoch_length > 0 && epoch_length != 60) {
      counts_vals <- to_cpm(counts_vals, epoch_length)
    }
    data$intensity <- cut(counts_vals,
      breaks = cp,
      labels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
      include.lowest = TRUE, right = FALSE
    )
  }

  # Handle NA values
  data$intensity[is.na(data$intensity)] <- "Sedentary"

  # Calculate summary
  intensity_summary <- as.data.frame(table(data$intensity))
  names(intensity_summary) <- c("intensity", "epochs")
  intensity_summary$minutes <- intensity_summary$epochs * epoch_minutes
  intensity_summary$hours <- intensity_summary$minutes / 60
  intensity_summary$percent <- intensity_summary$epochs / sum(intensity_summary$epochs) * 100

  # Calculate MVPA (Moderate + Vigorous + Very Vigorous)
  mvpa_cats <- c("Moderate", "Vigorous", "Very Vigorous")
  mvpa_minutes <- sum(intensity_summary$minutes[intensity_summary$intensity %in% mvpa_cats])
  mvpa_hours <- mvpa_minutes / 60
  mvpa_goal_met <- mvpa_minutes >= mvpa_goal_minutes

  # Total wear time
  total_minutes <- sum(intensity_summary$minutes)
  total_hours <- total_minutes / 60

  # Format time strings (HH:MM)
  intensity_summary$time_str <- sapply(intensity_summary$minutes, function(m) {
    h <- floor(m / 60)
    mins <- round(m %% 60)
    if (h > 0) {
      sprintf("%dh %dm", h, mins)
    } else {
      sprintf("%dm", mins)
    }
  })

  # Create labels
  intensity_summary$label <- sprintf("%.1f%%\n(%s)",
                                      intensity_summary$percent,
                                      intensity_summary$time_str)

  # Order factors
  intensity_order <- c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")
  intensity_summary$intensity <- factor(intensity_summary$intensity, levels = intensity_order)
  intensity_summary <- intensity_summary[order(intensity_summary$intensity), ]

  # Intensity colors
  if (colorblind_safe) {
    intensity_colors <- c(
      "Sedentary" = "#0072B2",      # Blue
      "Light" = "#F0E442",          # Yellow
      "Moderate" = "#E69F00",       # Orange
      "Vigorous" = "#D55E00",       # Vermillion
      "Very Vigorous" = "#CC79A7"   # Pink
    )
  } else {
    intensity_colors <- c(
      "Sedentary" = "#3498DB",
      "Light" = "#F1C40F",
      "Moderate" = "#E67E22",
      "Vigorous" = "#E74C3C",
      "Very Vigorous" = "#9B59B6"
    )
  }

  # Remove categories with 0%
  intensity_summary <- intensity_summary[intensity_summary$epochs > 0, ]

  # Calculate label positions properly for pie chart
  intensity_summary <- intensity_summary[order(intensity_summary$intensity), ]

  # Calculate cumulative percentages for positioning
  intensity_summary$ymax <- cumsum(intensity_summary$percent)
  intensity_summary$ymin <- c(0, head(intensity_summary$ymax, -1))
  intensity_summary$pos <- (intensity_summary$ymax + intensity_summary$ymin) / 2

  # Calculate angle for label positioning (for placing labels outside)
  intensity_summary$angle <- (intensity_summary$pos / 100) * 360
  # Convert to radians and calculate x,y for outside labels
  intensity_summary$angle_rad <- (90 - (intensity_summary$pos / 100) * 360) * pi / 180
  intensity_summary$label_x <- 1.4 * cos(intensity_summary$angle_rad)
  intensity_summary$label_y <- 1.4 * sin(intensity_summary$angle_rad)

  # Determine if label should be inside (large slice) or outside (small slice)
  intensity_summary$inside <- intensity_summary$percent >= 10

  # Create donut or pie chart
  if (donut_style) {
    # Donut chart with center hole
    p <- ggplot2::ggplot(intensity_summary,
                          ggplot2::aes(x = 2, y = percent, fill = intensity)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.5) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::xlim(0.5, 2.5)  # Creates center hole
  } else {
    # Traditional pie chart
    p <- ggplot2::ggplot(intensity_summary,
                          ggplot2::aes(x = "", y = percent, fill = intensity)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.5) +
      ggplot2::coord_polar("y", start = 0)
  }

  p <- p +
    ggplot2::scale_fill_manual(
      values = intensity_colors,
      name = "Activity\nIntensity",
      guide = ggplot2::guide_legend(
        title.position = "top",
        keywidth = ggplot2::unit(1, "cm"),
        keyheight = ggplot2::unit(0.8, "cm")
      )
    )

  # Add center annotation for donut style
  if (donut_style) {
    # Format MVPA time
    mvpa_h <- floor(mvpa_minutes / 60)
    mvpa_m <- round(mvpa_minutes %% 60)
    if (mvpa_h > 0) {
      mvpa_str <- sprintf("%dh %dm", mvpa_h, mvpa_m)
    } else {
      mvpa_str <- sprintf("%dm", mvpa_m)
    }

    # Center text with total time and MVPA summary
    total_h <- floor(total_minutes / 60)
    total_m <- round(total_minutes %% 60)
    total_str <- sprintf("%dh %dm", total_h, total_m)

    # MVPA goal indicator
    goal_icon <- if (mvpa_goal_met) "\u2713" else ""  # checkmark
    goal_color <- if (mvpa_goal_met) "#27AE60" else "#E74C3C"

    # Add center annotations
    p <- p +
      ggplot2::annotate("text", x = 0.5, y = 0, label = total_str,
                        size = 5, fontface = "bold", color = "gray20") +
      ggplot2::annotate("text", x = 0.5, y = -8, label = "Total Wear",
                        size = 3, color = "gray50")

    if (show_mvpa_goal) {
      mvpa_label <- sprintf("MVPA: %s", mvpa_str)
      goal_label <- sprintf("Goal: %dm %s", mvpa_goal_minutes, goal_icon)

      p <- p +
        ggplot2::annotate("text", x = 0.5, y = 8, label = mvpa_label,
                          size = 3.5, fontface = "bold", color = "#E67E22") +
        ggplot2::annotate("text", x = 0.5, y = 16, label = goal_label,
                          size = 3, color = goal_color)
    }
  }

  if (show_labels) {
    label_x_pos <- if (donut_style) 2.0 else 0.7

    inside_labels <- intensity_summary[intensity_summary$inside & intensity_summary$percent > 0, ]
    if (nrow(inside_labels) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = inside_labels,
          ggplot2::aes(y = pos, label = label),
          x = label_x_pos,
          color = "white", fontface = "bold", size = 3.5,
          lineheight = 0.9
        )
    }

    outside_labels <- intensity_summary[!intensity_summary$inside & intensity_summary$percent > 2, ]
    if (nrow(outside_labels) > 0) {
      outside_labels$short_label <- sprintf("%.1f%%", outside_labels$percent)
      outside_x <- if (donut_style) 2.6 else 1.7

      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p +
          ggrepel::geom_text_repel(
            data = outside_labels,
            ggplot2::aes(y = pos, label = short_label),
            x = outside_x,
            size = 3.5, fontface = "bold", color = "gray30",
            min.segment.length = 0,
            segment.color = "gray50",
            segment.size = 0.3,
            box.padding = 0.3,
            point.padding = 0.2,
            force = 1.5,
            max.overlaps = 15,
            direction = "y",
            seed = 42
          )
      } else {
        p <- p +
          ggplot2::geom_text(
            data = outside_labels,
            ggplot2::aes(y = pos, label = short_label),
            x = outside_x,
            color = "gray30", fontface = "bold", size = 3.5
          )
      }
    }
  }

  # Build subtitle with threshold info if requested
  if (is.null(subtitle)) {
    if (show_thresholds) {
      # Show cut-point thresholds in subtitle
      subtitle <- sprintf("Cut-points: %s | Sed: <%d | Light: %d-%d | Mod: %d-%d | Vig: >%d CPM",
                          cp_name, cp[2], cp[2], cp[3]-1, cp[3], cp[4]-1, cp[4])
    } else {
      subtitle <- sprintf("Total: %.1f hours | Cut-points: %s",
                          sum(intensity_summary$hours), cp_name)
    }
  }

  # Caption with MVPA achievement
  if (show_mvpa_goal) {
    mvpa_pct <- (mvpa_minutes / mvpa_goal_minutes) * 100
    caption_text <- sprintf("MVPA: %.0f min (%.0f%% of %d min goal) | %s",
                            mvpa_minutes, min(mvpa_pct, 100), mvpa_goal_minutes,
                            if (mvpa_goal_met) "Goal Met!" else "Below Goal")
  } else {
    caption_text <- "Percentages shown for each intensity category"
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption_text
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50", size = 9),
      plot.caption = ggplot2::element_text(hjust = 0.5,
                                            color = if (show_mvpa_goal && mvpa_goal_met) "#27AE60" else "gray60",
                                            face = if (show_mvpa_goal && mvpa_goal_met) "bold" else "plain",
                                            size = 9),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  return(p)
}


#' Plot Intensity Pie Chart from Summary Data
#'
#' Creates an intensity distribution pie chart from pre-calculated summary
#' totals (e.g., from Activity Analysis with wear-time filtering applied).
#' This is the recommended approach per GGIR/ActiLife best practices.
#'
#' @param intensity_summary Data frame with 'intensity' and 'minutes' columns
#' @param cutpoints Name of cut-points used (for display)
#' @param show_labels Whether to show percentage labels
#' @param title Plot title
#' @param subtitle Plot subtitle
#'
#' @return A ggplot2 object
#'
#' @export
plot_intensity_pie_from_summary <- function(intensity_summary,
                                             cutpoints = "freedson",
                                             show_labels = TRUE,
                                             title = "Activity Intensity Distribution",
                                             subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  if (!all(c("intensity", "minutes") %in% names(intensity_summary))) {
    stop("intensity_summary must have 'intensity' and 'minutes' columns")
  }

  intensity_summary$hours <- intensity_summary$minutes / 60
  intensity_summary$percent <- intensity_summary$minutes / sum(intensity_summary$minutes, na.rm = TRUE) * 100

  intensity_summary$time_str <- sapply(intensity_summary$minutes, function(m) {
    h <- floor(m / 60)
    mins <- round(m %% 60)
    if (h > 0) sprintf("%dh %dm", h, mins) else sprintf("%dm", mins)
  })

  intensity_summary$label <- sprintf("%.1f%%\n(%s)",
                                      intensity_summary$percent,
                                      intensity_summary$time_str)

  intensity_order <- c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")
  intensity_summary$intensity <- factor(intensity_summary$intensity, levels = intensity_order)
  intensity_summary <- intensity_summary[order(intensity_summary$intensity), ]

  intensity_colors <- c(
    "Sedentary" = "#3498DB",
    "Light" = "#F1C40F",
    "Moderate" = "#E67E22",
    "Vigorous" = "#E74C3C",
    "Very Vigorous" = "#9B59B6"
  )

  intensity_summary <- intensity_summary[intensity_summary$minutes > 0, ]

  if (nrow(intensity_summary) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No activity data available",
                        size = 5, hjust = 0.5) +
      ggplot2::theme_void())
  }

  intensity_summary$ymax <- cumsum(intensity_summary$percent)
  intensity_summary$ymin <- c(0, head(intensity_summary$ymax, -1))
  intensity_summary$pos <- (intensity_summary$ymax + intensity_summary$ymin) / 2

  angle_rad <- (90 - intensity_summary$pos * 3.6) * pi / 180
  intensity_summary$label_x <- 0.6 * sin(angle_rad)
  intensity_summary$label_y <- 0.6 * cos(angle_rad)
  intensity_summary$outside_x <- 1.4 * sin(angle_rad)
  intensity_summary$outside_y <- 1.4 * cos(angle_rad)

  intensity_summary$hjust <- ifelse(intensity_summary$outside_x > 0.1, 0,
                                    ifelse(intensity_summary$outside_x < -0.1, 1, 0.5))

  pie_data <- intensity_summary
  pie_data$id <- seq_len(nrow(pie_data))

  pie_segments <- do.call(rbind, lapply(seq_len(nrow(pie_data)), function(i) {
    row <- pie_data[i, ]
    start_angle <- (90 - row$ymin * 3.6) * pi / 180
    end_angle <- (90 - row$ymax * 3.6) * pi / 180
    n_points <- max(2, round(abs(row$percent) / 2))
    angles <- seq(start_angle, end_angle, length.out = n_points)
    data.frame(
      id = row$id,
      x = c(0, sin(angles), 0),
      y = c(0, cos(angles), 0),
      intensity = row$intensity,
      stringsAsFactors = FALSE
    )
  }))

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = pie_segments,
      ggplot2::aes(x = x, y = y, fill = intensity, group = id),
      color = "white", linewidth = 1.5
    ) +
    ggplot2::scale_fill_manual(
      values = intensity_colors,
      name = "Activity\nIntensity",
      guide = ggplot2::guide_legend(
        title.position = "top",
        keywidth = ggplot2::unit(1, "cm"),
        keyheight = ggplot2::unit(0.6, "cm")
      )
    ) +
    ggplot2::coord_equal(xlim = c(-2, 2), ylim = c(-1.5, 1.5))

  if (show_labels) {
    large_slices <- intensity_summary[intensity_summary$percent >= 8, ]
    if (nrow(large_slices) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = large_slices,
          ggplot2::aes(x = label_x, y = label_y, label = label),
          color = "white", fontface = "bold", size = 4, lineheight = 0.9
        )
    }

    small_slices <- intensity_summary[intensity_summary$percent < 8 & intensity_summary$percent >= 2, ]
    if (nrow(small_slices) > 0) {
      small_slices$short_label <- sprintf("%.1f%%\n(%s)", small_slices$percent, small_slices$time_str)

      # Calculate positions pushed further out for small slices
      small_slices$anchor_x <- small_slices$outside_x * 0.75  # Point on pie edge
      small_slices$anchor_y <- small_slices$outside_y * 0.75
      small_slices$label_pos_x <- small_slices$outside_x * 1.35  # Label position further out
      small_slices$label_pos_y <- small_slices$outside_y * 1.35

      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p +
          ggrepel::geom_text_repel(
            data = small_slices,
            ggplot2::aes(x = anchor_x, y = anchor_y, label = short_label),
            size = 3.5, fontface = "bold", color = "gray20",
            lineheight = 0.85,
            min.segment.length = 0,
            segment.color = "gray50",
            segment.size = 0.5,
            box.padding = 0.5,
            point.padding = 0.2,
            force = 3,
            force_pull = 0.3,
            max.overlaps = 20,
            direction = "both",
            nudge_x = small_slices$label_pos_x - small_slices$anchor_x,
            nudge_y = small_slices$label_pos_y - small_slices$anchor_y,
            seed = 42
          )
      } else {
        p <- p +
          ggplot2::geom_text(
            data = small_slices,
            ggplot2::aes(x = label_pos_x, y = label_pos_y, label = short_label, hjust = hjust),
            size = 3.5, fontface = "bold", color = "gray20", lineheight = 0.85
          ) +
          ggplot2::geom_segment(
            data = small_slices,
            ggplot2::aes(x = anchor_x, y = anchor_y,
                         xend = label_pos_x * 0.85, yend = label_pos_y * 0.85),
            color = "gray50", linewidth = 0.5
          )
      }
    }
  }

  if (is.null(subtitle)) {
    cutpoint_name <- if (is.character(cutpoints)) {
      switch(cutpoints,
        "freedson" = "Freedson Adult (1998)",
        "evenson" = "Evenson Children (2008)",
        "troiano" = "Troiano (2008)",
        "canhr" = "CANHR (2025)",
        cutpoints
      )
    } else {
      "Custom"
    }
    subtitle <- sprintf("Total: %.1f hours | Cut-points: %s",
                        sum(intensity_summary$hours), cutpoint_name)
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = "Wear-time filtered data"
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50", size = 10),
      plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray60", size = 9),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  return(p)
}


#' Plot Intensity Area Chart from Hourly Data
#'
#' Creates an intensity distribution area chart from pre-calculated hourly
#' summaries (e.g., from Activity Analysis with wear-time filtering applied).
#'
#' @param hourly_data Data frame with hourly intensity breakdown
#' @param cutpoints Name of cut-points used (for display)
#' @param stacked Whether to stack the areas
#' @param title Plot title
#' @param subtitle Plot subtitle
#'
#' @return A ggplot2 object
#'
#' @export
plot_intensity_area_from_hourly <- function(hourly_data,
                                             cutpoints = "freedson",
                                             stacked = TRUE,
                                             title = "Activity Intensity by Hour",
                                             subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Check if hourly_data has expected structure
  if (is.null(hourly_data) || nrow(hourly_data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No hourly data available",
                        size = 5, hjust = 0.5) +
      ggplot2::theme_void())
  }

  # Expected columns: hour, sedentary, light, moderate, vigorous (or similar)
  # Reshape to long format if needed
  intensity_cols <- c("sedentary", "light", "moderate", "vigorous", "mvpa",
                      "sedentary_min", "light_min", "moderate_min", "vigorous_min")
  available_cols <- intersect(tolower(names(hourly_data)), intensity_cols)

  if (length(available_cols) == 0) {
    # Try to work with what we have
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "Hourly intensity data not in expected format",
                        size = 4, hjust = 0.5) +
      ggplot2::theme_void())
  }

  # Standardize column names
  names(hourly_data) <- tolower(names(hourly_data))

  # Ensure hour column exists
  if (!"hour" %in% names(hourly_data)) {
    if ("time" %in% names(hourly_data)) {
      hourly_data$hour <- as.numeric(substr(hourly_data$time, 1, 2))
    } else {
      hourly_data$hour <- 0:(nrow(hourly_data) - 1) %% 24
    }
  }

  # Reshape to long format
  intensity_vars <- intersect(names(hourly_data),
                              c("sedentary", "light", "moderate", "vigorous",
                                "sedentary_min", "light_min", "moderate_min", "vigorous_min"))

  if (length(intensity_vars) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = "No intensity columns found in hourly data",
                        size = 4, hjust = 0.5) +
      ggplot2::theme_void())
  }

  plot_data <- tidyr::pivot_longer(
    hourly_data,
    cols = all_of(intensity_vars),
    names_to = "intensity",
    values_to = "minutes"
  )

  # Clean up intensity names
  plot_data$intensity <- gsub("_min$", "", plot_data$intensity)
  plot_data$intensity <- tools::toTitleCase(plot_data$intensity)

  # Set factor order
  intensity_order <- c("Sedentary", "Light", "Moderate", "Vigorous")
  plot_data$intensity <- factor(plot_data$intensity, levels = intensity_order)

  # Intensity colors
  intensity_colors <- c(
    "Sedentary" = "#3498DB",
    "Light" = "#2ECC71",
    "Moderate" = "#F39C12",
    "Vigorous" = "#E74C3C"
  )

  position_type <- if (stacked) "stack" else "identity"

  # Generate subtitle if not provided
  if (is.null(subtitle)) {
    cutpoint_name <- switch(cutpoints,
      "freedson" = "Freedson (1998)",
      "evenson" = "Evenson (2008)",
      "canhr" = "CANHR (2025)",
      cutpoints
    )
    subtitle <- paste("Hourly averages |", cutpoint_name, "cut-points")
  }

  p <- ggplot2::ggplot(plot_data,
                        ggplot2::aes(x = hour, y = minutes, fill = intensity)) +
    ggplot2::geom_area(position = position_type, alpha = 0.8) +
    ggplot2::scale_fill_manual(
      values = intensity_colors,
      name = "Activity Intensity"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = c("12AM", "3AM", "6AM", "9AM", "12PM", "3PM", "6PM", "9PM"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Time of Day",
      y = "Minutes per Hour",
      caption = "Wear-time filtered data"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9, color = "gray50"),
      plot.caption = ggplot2::element_text(size = 8, color = "gray50", hjust = 0),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}


# SLEEP HYPNOGRAM

#' Enhanced Sleep Hypnogram Visualization
#'
#' Creates a publication-quality hypnogram showing sleep/wake states with
#' comprehensive sleep metrics (TST, WASO, SOL, efficiency), activity overlay,
#' and awakening markers.
#'
#' @param data Data frame with timestamp and sleep state columns
#' @param timestamp_col Name of timestamp column
#' @param sleep_col Name of sleep state column (values: "S" or "W", or 0/1)
#' @param counts_col Optional. Name of activity counts column for overlay
#' @param sleep_periods Optional data frame with sleep period boundaries
#' @param show_metrics Logical. Show TST, WASO, SOL, efficiency? (default: TRUE)
#' @param show_activity Logical. Show activity counts as background? (default: TRUE)
#' @param show_awakenings Logical. Mark awakening episodes? (default: TRUE)
#' @param epoch_seconds Epoch length in seconds for calculations (default: 60)
#' @param title Plot title
#' @param compact_mode Display mode: "auto" (default), TRUE, or FALSE. Controls
#'   whether to use a compact layout for shorter sleep periods.
#'
#' @return A ggplot2 object
#'
#' @details
#' This enhanced hypnogram includes:
#' \itemize{
#'   \item TST (Total Sleep Time) annotation
#'   \item WASO (Wake After Sleep Onset) annotation
#'   \item SOL (Sleep Onset Latency) annotation
#'   \item Sleep Efficiency percentage
#'   \item Activity counts as gray background
#'   \item Awakening episode markers
#'   \item Sleep onset/offset time markers
#' }
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#'
#' # Basic hypnogram
#' plot_hypnogram(results$epoch_data, sleep_col = "sleep")
#'
#' # With activity overlay and all metrics
#' plot_hypnogram(results$epoch_data,
#'                sleep_col = "sleep",
#'                counts_col = "axis1",
#'                show_activity = TRUE,
#'                show_metrics = TRUE)
#' }
#'
#' @export
plot_hypnogram <- function(data,
                            timestamp_col = "timestamp",
                            sleep_col = "sleep_state",
                            counts_col = NULL,
                            sleep_periods = NULL,
                            show_metrics = TRUE,
                            show_activity = TRUE,
                            show_awakenings = TRUE,
                            epoch_seconds = 60,
                            title = "Sleep Hypnogram",
                            compact_mode = "auto") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Validate timestamp column exists
  if (!timestamp_col %in% names(data)) {
    stop("Timestamp column '", timestamp_col, "' not found in data")
  }

  # Robust timestamp conversion - handle various input types
  ts <- data[[timestamp_col]]
  if (inherits(ts, "POSIXct")) {
    # Already POSIXct, good
    data$ts_posix <- ts
  } else if (inherits(ts, "POSIXlt")) {
    data$ts_posix <- as.POSIXct(ts)
  } else if (inherits(ts, "Date")) {
    # Date only - convert to POSIXct at midnight
    data$ts_posix <- as.POSIXct(as.character(ts))
  } else if (is.numeric(ts)) {
    # Numeric - assume Unix timestamp or .NET ticks
    if (max(ts, na.rm = TRUE) > 1e12) {
      # Likely .NET ticks (100-nanosecond intervals since 0001-01-01)
      data$ts_posix <- as.POSIXct((ts / 10000000 - 62135596800), origin = "1970-01-01", tz = "UTC")
    } else {
      # Assume Unix timestamp
      data$ts_posix <- as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
    }
  } else if (is.character(ts)) {
    # Character - try to parse
    data$ts_posix <- as.POSIXct(ts)
  } else {
    stop("Cannot convert timestamp column to POSIXct. Class: ", class(ts)[1])
  }

  # Validate conversion worked
  if (all(is.na(data$ts_posix))) {
    stop("Failed to convert timestamps to POSIXct - all values are NA")
  }

  # Convert sleep state to numeric (1 = Wake, 0 = Sleep)
  if (sleep_col %in% names(data)) {
    sleep_state <- data[[sleep_col]]
    if (is.character(sleep_state)) {
      sleep_state <- toupper(as.character(sleep_state))
      valid_vals <- all(sleep_state %in% c("S", "W", NA))
      if (!valid_vals) {
        sleep_state <- ifelse(substr(sleep_state, 1, 1) == "W", "W", "S")
      }
    } else if (is.numeric(sleep_state)) {
      sleep_state <- ifelse(sleep_state == 1, "W", "S")
    } else {
      sleep_state <- as.character(sleep_state)
      sleep_state <- ifelse(toupper(substr(sleep_state, 1, 1)) == "W", "W", "S")
    }
    data$sleep_numeric <- ifelse(is.na(sleep_state), NA_real_, ifelse(sleep_state == "W", 1, 0))
  } else {
    stop("Sleep state column '", sleep_col, "' not found in data")
  }

  if (all(is.na(data$sleep_numeric))) {
    stop("Sleep state column has no valid values")
  }

  # Get activity data if available
  has_activity <- !is.null(counts_col) && counts_col %in% names(data)
  if (has_activity) {
    data$activity <- as.numeric(data[[counts_col]])
    # Normalize activity for overlay
    max_activity <- max(data$activity, na.rm = TRUE)
    if (!is.na(max_activity) && max_activity > 0) {
      data$activity_scaled <- data$activity / max_activity * 0.8
    } else {
      data$activity_scaled <- 0
    }
  }

  data <- data[!is.na(data$sleep_numeric), ]

  # Add time components using validated POSIXct
  data$date <- as.Date(data$ts_posix)
  data$time_of_day <- as.numeric(format(data$ts_posix, "%H")) +
                      as.numeric(format(data$ts_posix, "%M")) / 60

  # Get unique nights (defined as 6 PM to 12 PM next day)
  data$night_date <- as.Date(data$ts_posix - 6 * 3600)
  unique_nights <- sort(unique(data$night_date))

  # Ensure we have valid dates before formatting
  if (length(unique_nights) == 0 || all(is.na(unique_nights))) {
    stop("No valid dates found in data after processing")
  }

  # Determine compact mode based on number of nights
  n_nights <- length(unique_nights)
  is_compact <- if (compact_mode == "auto") {
    n_nights > 5  # Use compact mode for more than 5 nights
  } else {
    isTRUE(compact_mode)
  }

  # Adjust parameters for compact mode
  if (is_compact) {
    y_labels <- c("S", "W")  # Short labels
    base_font_size <- 9
    strip_font_size <- 8
    axis_font_size <- 8
    metrics_font_size <- 2.0
    panel_spacing <- 0.1
    awakening_size <- 1.2
  } else {
    y_labels <- c("Sleep", "Wake")
    base_font_size <- 11
    strip_font_size <- 10
    axis_font_size <- 10
    metrics_font_size <- 2.5
    panel_spacing <- 0.3
    awakening_size <- 2
  }

  # Format for faceting
  data$night_label <- format(data$night_date, "%a %m/%d")
  data$night_label <- factor(data$night_label,
                             levels = format(unique_nights, "%a %m/%d"))

  data <- data[order(data$night_date, data$ts_posix), ]
  data$segment_id <- ave(data$time_of_day, data$night_label, FUN = function(x) {
    cumsum(c(FALSE, diff(x) < -12))
  })
  data$wake_band <- ifelse(data$sleep_numeric == 1, 1, 0)
  data$sleep_band <- ifelse(data$sleep_numeric == 0, 1, 0)

  # Calculate comprehensive sleep metrics for each night from raw epoch data
  # This provides consistent metrics display regardless of Tudor-Locke matching issues
  metrics_data <- data.frame()

  {
    # Fallback: Calculate metrics from raw epoch data
    # Use index-based loop to preserve Date class (for loops coerce Date to numeric)
    for (i in seq_along(unique_nights)) {
      night <- unique_nights[i]  # This preserves Date class
      night_data <- data[data$night_date == night, ]
      if (nrow(night_data) == 0) next

      # Find sleep onset (first sleep epoch)
      sleep_indices <- which(night_data$sleep_numeric == 0)
      wake_indices <- which(night_data$sleep_numeric == 1)

      if (length(sleep_indices) == 0) next

      # Sleep onset index
      sleep_onset_idx <- min(sleep_indices)
      # Final wake (last wake before final sleep or end of night)
      sleep_offset_idx <- max(sleep_indices)

      # Sleep Onset Latency (SOL) - time from recording start to first sleep
      sol_epochs <- sleep_onset_idx - 1
      sol_min <- sol_epochs * (epoch_seconds / 60)

      # Total recording time within sleep period
      sleep_period_epochs <- sleep_offset_idx - sleep_onset_idx + 1

      # Total Sleep Time (TST) - actual sleep epochs during sleep period
      tst_epochs <- sum(night_data$sleep_numeric[sleep_onset_idx:sleep_offset_idx] == 0)
      tst_min <- tst_epochs * (epoch_seconds / 60)
      tst_hours <- floor(tst_min / 60)
      tst_remainder <- round(tst_min %% 60)

      # Wake After Sleep Onset (WASO) - wake epochs during sleep period
      waso_epochs <- sum(night_data$sleep_numeric[sleep_onset_idx:sleep_offset_idx] == 1)
      waso_min <- waso_epochs * (epoch_seconds / 60)

      # Sleep Efficiency
      if (sleep_period_epochs > 0) {
        efficiency <- (tst_epochs / sleep_period_epochs) * 100
      } else {
        efficiency <- 0
      }

      # Count awakenings (transitions from sleep to wake during sleep period)
      sleep_period_states <- night_data$sleep_numeric[sleep_onset_idx:sleep_offset_idx]
      awakening_count <- sum(diff(sleep_period_states) == 1, na.rm = TRUE)

      # Sleep onset and offset times
      sleep_onset_time <- night_data$time_of_day[sleep_onset_idx]
      sleep_offset_time <- night_data$time_of_day[sleep_offset_idx]

      # Build metrics label
      metrics_label <- sprintf(
        "TST: %dh %dmin | WASO: %.0fmin | SOL: %.0fmin | Eff: %.0f%% | Awakenings: %d",
        tst_hours, tst_remainder, waso_min, sol_min, efficiency, awakening_count
      )

      metrics_data <- rbind(metrics_data, data.frame(
        night_date = night,
        night_label = format(night, "%a %m/%d"),
        tst_min = tst_min,
        waso_min = waso_min,
        sol_min = sol_min,
        efficiency = efficiency,
        awakening_count = awakening_count,
        sleep_onset_time = sleep_onset_time,
        sleep_offset_time = sleep_offset_time,
        metrics_label = metrics_label,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Create hypnogram plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = time_of_day))

  # Add activity background if available
  if (show_activity && has_activity) {
    p <- p +
      ggplot2::geom_area(
        ggplot2::aes(y = activity_scaled, group = segment_id),
        fill = "#cbd5e0", alpha = 0.25
      )
  }

  # Add sleep/wake ribbons
  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = wake_band, fill = "Wake", group = segment_id),
                          alpha = 0.45) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = sleep_band, fill = "Sleep", group = segment_id),
                          alpha = 0.45) +
    ggplot2::geom_step(ggplot2::aes(y = sleep_numeric, group = segment_id),
                       color = "#1f2a44", linewidth = 0.8)

  # Add awakening markers
  if (show_awakenings) {
    # Detect awakening starts (transitions from 0 to 1)
    data$awakening_start <- c(FALSE, diff(data$sleep_numeric) == 1)
    awakenings <- data[data$awakening_start, ]

    if (nrow(awakenings) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = awakenings,
          ggplot2::aes(x = time_of_day, y = 1),
          color = "#E74C3C", size = awakening_size, shape = 17
        )
    }
  }

  # Add sleep onset/offset markers
  if (nrow(metrics_data) > 0) {
    onset_data <- data.frame(
      time_of_day = metrics_data$sleep_onset_time,
      night_label = factor(metrics_data$night_label, levels = format(unique_nights, "%a %m/%d")),
      label = "Onset"
    )
    offset_data <- data.frame(
      time_of_day = metrics_data$sleep_offset_time,
      night_label = factor(metrics_data$night_label, levels = format(unique_nights, "%a %m/%d")),
      label = "Offset"
    )

    p <- p +
      ggplot2::geom_vline(
        data = onset_data,
        ggplot2::aes(xintercept = time_of_day),
        color = "#27AE60", linetype = "dashed", linewidth = 0.5
      ) +
      ggplot2::geom_vline(
        data = offset_data,
        ggplot2::aes(xintercept = time_of_day),
        color = "#8E44AD", linetype = "dashed", linewidth = 0.5
      )
  }

  p <- p +
    ggplot2::scale_fill_manual(
      values = c("Sleep" = "#1a365d", "Wake" = "#f56565"),
      name = "State",
      guide = ggplot2::guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        keywidth = ggplot2::unit(1.5, "cm"),
        keyheight = ggplot2::unit(0.5, "cm")
      )
    ) +
    ggplot2::facet_wrap(~ night_label, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = y_labels,
      limits = c(-0.1, 1.15)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 3),
      labels = sprintf("%02d:00", seq(0, 24, 3) %% 24),
      limits = c(0, 24)
    )

  # Build subtitle
  subtitle_parts <- c("Blue = Sleep", "Red = Wake")
  if (show_activity && has_activity) {
    subtitle_parts <- c(subtitle_parts, "Gray = Activity")
  }
  if (show_awakenings) {
    subtitle_parts <- c(subtitle_parts, "Triangle = Awakening")
  }
  subtitle_text <- paste(subtitle_parts, collapse = " | ")

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle_text,
      x = "Time of Day",
      y = "",
      caption = "Green line = Sleep Onset | Purple line = Sleep Offset"
    ) +
    theme_canhrActi(base_size = base_font_size) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = strip_font_size),
      strip.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(panel_spacing, "lines"),
      axis.text.y = ggplot2::element_text(face = "bold", size = axis_font_size),
      axis.text.x = ggplot2::element_text(size = axis_font_size),
      legend.position = "bottom",
      legend.box = "horizontal"
    )

  # Add metrics labels
  if (show_metrics && nrow(metrics_data) > 0) {
    metrics_data$night_label <- factor(metrics_data$night_label,
                                        levels = format(unique_nights, "%a %m/%d"))
    p <- p +
      ggplot2::geom_label(
        data = metrics_data,
        ggplot2::aes(x = 12, y = 1.1, label = metrics_label),
        size = metrics_font_size, hjust = 0.5, vjust = 0,
        fill = "white", alpha = 0.9,
        linewidth = 0.2, label.padding = ggplot2::unit(0.1, "lines")
      )
  }

  return(p)
}


# POLAR/RADAR CHART FOR CIRCADIAN PATTERNS

#' Enhanced Polar/Radar Chart for Circadian Activity
#'
#' Creates a publication-quality polar coordinate chart showing hourly activity
#' patterns with L5/M10 window overlays, relative amplitude annotation, and
#' reference ranges for circadian metrics.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param counts_col Name of counts column
#' @param aggregate_func Function to aggregate activity (default: mean)
#' @param show_ribbon Logical. Show confidence ribbon? (default: TRUE)
#' @param by_day_type Logical. Separate by weekday/weekend? (default: FALSE)
#' @param show_L5M10 Logical. Show L5/M10 windows as arcs? (default: TRUE)
#' @param L5_onset Optional. Pre-computed L5 onset hour (0-23)
#' @param M10_onset Optional. Pre-computed M10 onset hour (0-23)
#' @param show_metrics Logical. Show RA, IS, IV annotations? (default: TRUE)
#' @param IS_value Optional. Pre-computed interdaily stability value
#' @param IV_value Optional. Pre-computed intradaily variability value
#' @param show_reference Logical. Show reference ranges? (default: TRUE)
#' @param title Plot title
#'
#' @return A ggplot2 object
#'
#' @details
#' This enhanced polar chart includes:
#' \itemize{
#'   \item L5 window (5-hour arc) highlighted in blue
#'   \item M10 window (10-hour arc) highlighted in orange
#'   \item Relative Amplitude (RA) annotation
#'   \item Optional IS/IV values with interpretation
#'   \item Reference ranges for normal circadian function
#' }
#'
#' @references
#' Van Someren EJ, et al. Bright light therapy: improved sensitivity to its
#' effects on rest-activity rhythms in Alzheimer patients by application of
#' nonparametric methods. Chronobiol Int. 1999;16(4):505-518.
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#'
#' # Basic polar plot
#' plot_circadian_polar(results$epoch_data)
#'
#' # With L5/M10 overlays and metrics
#' plot_circadian_polar(results$epoch_data,
#'                      show_L5M10 = TRUE,
#'                      show_metrics = TRUE)
#'
#' # Weekday vs weekend comparison
#' plot_circadian_polar(results$epoch_data, by_day_type = TRUE)
#' }
#'
#' @export
plot_circadian_polar <- function(data,
                                  timestamp_col = "timestamp",
                                  counts_col = "axis1",
                                  aggregate_func = mean,
                                  show_ribbon = TRUE,
                                  by_day_type = FALSE,
                                  show_L5M10 = TRUE,
                                  L5_onset = NULL,
                                  M10_onset = NULL,
                                  show_metrics = TRUE,
                                  IS_value = NULL,
                                  IV_value = NULL,
                                  show_reference = TRUE,
                                  title = "Circadian Activity Pattern") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Validate required columns exist
  if (!timestamp_col %in% names(data)) {
    stop("Timestamp column '", timestamp_col, "' not found in data")
  }
  if (!counts_col %in% names(data)) {
    stop("Counts column '", counts_col, "' not found in data")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Ensure counts column is numeric
  if (!is.numeric(data[[counts_col]])) {
    data[[counts_col]] <- as.numeric(data[[counts_col]])
  }

  # Check for empty data
  if (nrow(data) == 0) {
    stop("Data is empty - cannot create circadian polar plot")
  }

  # Check if data spans at least 12 hours
  time_range <- diff(range(data[[timestamp_col]], na.rm = TRUE))
  if (as.numeric(time_range, units = "hours") < 12) {
    warning("Data spans less than 12 hours. Circadian pattern may be incomplete.")
  }

  # Validate and coerce L5_onset and M10_onset to numeric hours (0-23)
  if (!is.null(L5_onset)) {
    if (inherits(L5_onset, "POSIXct") || inherits(L5_onset, "POSIXlt")) {
      L5_onset <- as.integer(format(L5_onset, "%H"))
    } else if (is.character(L5_onset)) {
      # Try to parse as time string (e.g., "02:30" or "2")
      if (grepl(":", L5_onset)) {
        L5_onset <- as.integer(sub(":.*", "", L5_onset))
      } else {
        L5_onset <- as.integer(L5_onset)
      }
    } else {
      L5_onset <- as.integer(L5_onset)
    }
    # Ensure it's in valid range
    if (is.na(L5_onset) || L5_onset < 0 || L5_onset > 23) {
      L5_onset <- NULL  # Reset to NULL so it gets recalculated
    }
  }

  if (!is.null(M10_onset)) {
    if (inherits(M10_onset, "POSIXct") || inherits(M10_onset, "POSIXlt")) {
      M10_onset <- as.integer(format(M10_onset, "%H"))
    } else if (is.character(M10_onset)) {
      if (grepl(":", M10_onset)) {
        M10_onset <- as.integer(sub(":.*", "", M10_onset))
      } else {
        M10_onset <- as.integer(M10_onset)
      }
    } else {
      M10_onset <- as.integer(M10_onset)
    }
    if (is.na(M10_onset) || M10_onset < 0 || M10_onset > 23) {
      M10_onset <- NULL
    }
  }

  # Add hour
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$activity <- as.numeric(data[[counts_col]])
  data$activity[is.na(data$activity)] <- 0

  # Aggregate by hour for L5/M10 calculation
  hourly_means <- aggregate(
    data$activity,
    by = list(hour = data$hour),
    FUN = mean, na.rm = TRUE
  )
  names(hourly_means)[2] <- "mean_activity"

  # Ensure all 24 hours are represented in hourly_means (required for L5/M10 calculation)
  all_hours_df <- data.frame(hour = 0:23)
  hourly_means <- merge(all_hours_df, hourly_means, by = "hour", all.x = TRUE)
  hourly_means$mean_activity[is.na(hourly_means$mean_activity)] <- 0
  hourly_means <- hourly_means[order(hourly_means$hour), ]

  # Calculate L5/M10 if not provided
  if (show_L5M10 || show_metrics) {
    if (is.null(L5_onset)) {
      min_sum <- Inf
      L5_value <- 0
      for (start in 0:23) {
        hours <- (start:(start + 4)) %% 24
        # Use match to get values in correct order, ensuring we get exactly 5 values
        window_vals <- hourly_means$mean_activity[match(hours, hourly_means$hour)]
        window_sum <- sum(window_vals, na.rm = TRUE)
        if (window_sum < min_sum) {
          min_sum <- window_sum
          L5_onset <- start
          L5_value <- mean(window_vals, na.rm = TRUE)
        }
      }
    } else {
      hours <- (L5_onset:(L5_onset + 4)) %% 24
      window_vals <- hourly_means$mean_activity[match(hours, hourly_means$hour)]
      L5_value <- mean(window_vals, na.rm = TRUE)
    }

    if (is.null(M10_onset)) {
      max_sum <- -Inf
      M10_value <- 0
      for (start in 0:23) {
        hours <- (start:(start + 9)) %% 24
        # Use match to get values in correct order, ensuring we get exactly 10 values
        window_vals <- hourly_means$mean_activity[match(hours, hourly_means$hour)]
        window_sum <- sum(window_vals, na.rm = TRUE)
        if (window_sum > max_sum) {
          max_sum <- window_sum
          M10_onset <- start
          M10_value <- mean(window_vals, na.rm = TRUE)
        }
      }
    } else {
      hours <- (M10_onset:(M10_onset + 9)) %% 24
      window_vals <- hourly_means$mean_activity[match(hours, hourly_means$hour)]
      M10_value <- mean(window_vals, na.rm = TRUE)
    }

    # Handle NA values from mean calculations
    if (is.na(L5_value)) L5_value <- 0
    if (is.na(M10_value)) M10_value <- 0

    # Calculate Relative Amplitude (protect against division by zero)
    if ((M10_value + L5_value) == 0) {
      RA <- 0
    } else {
      RA <- (M10_value - L5_value) / (M10_value + L5_value)
    }
    RA <- round(RA, 3)
  }

  # Create hourly stats for plotting
  if (by_day_type) {
    data$weekday <- weekdays(data[[timestamp_col]])
    data$day_type <- ifelse(data$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

    hourly_stats <- aggregate(
      activity ~ hour + day_type,
      data = data,
      FUN = function(x) c(mean = mean(x, na.rm = TRUE), se = sd(x, na.rm = TRUE) / sqrt(length(x)))
    )
    hourly_stats <- cbind(hourly_stats[, 1:2], as.data.frame(hourly_stats$activity))
    hourly_stats$lower <- hourly_stats$mean - 1.96 * hourly_stats$se
    hourly_stats$upper <- hourly_stats$mean + 1.96 * hourly_stats$se

    # Ensure all 24 hours are represented for each day type (required for polar ribbon)
    all_combos <- expand.grid(hour = 0:23, day_type = unique(hourly_stats$day_type))
    hourly_stats <- merge(all_combos, hourly_stats, by = c("hour", "day_type"), all.x = TRUE)
    # Fill missing values with 0 (or could interpolate)
    hourly_stats$mean[is.na(hourly_stats$mean)] <- 0
    hourly_stats$se[is.na(hourly_stats$se)] <- 0
    hourly_stats$lower[is.na(hourly_stats$lower)] <- 0
    hourly_stats$upper[is.na(hourly_stats$upper)] <- 0
    hourly_stats <- hourly_stats[order(hourly_stats$day_type, hourly_stats$hour), ]

  } else {
    hourly_stats <- aggregate(
      activity ~ hour,
      data = data,
      FUN = function(x) c(mean = mean(x, na.rm = TRUE), se = sd(x, na.rm = TRUE) / sqrt(length(x)))
    )
    hourly_stats <- cbind(hour = hourly_stats[, 1], as.data.frame(hourly_stats$activity))
    hourly_stats$lower <- hourly_stats$mean - 1.96 * hourly_stats$se
    hourly_stats$upper <- hourly_stats$mean + 1.96 * hourly_stats$se

    # Ensure all 24 hours are represented (required for polar ribbon)
    all_hours <- data.frame(hour = 0:23)
    hourly_stats <- merge(all_hours, hourly_stats, by = "hour", all.x = TRUE)
    # Fill missing values with 0
    hourly_stats$mean[is.na(hourly_stats$mean)] <- 0
    hourly_stats$se[is.na(hourly_stats$se)] <- 0
    hourly_stats$lower[is.na(hourly_stats$lower)] <- 0
    hourly_stats$upper[is.na(hourly_stats$upper)] <- 0
    hourly_stats <- hourly_stats[order(hourly_stats$hour), ]
  }

  # Ensure lower bounds are not negative (required for polar coordinates)
  hourly_stats$lower <- pmax(hourly_stats$lower, 0)

  # Add a closing point (hour 24 = hour 0) to properly close the polar line/ribbon
  if (by_day_type) {
    for (dt in unique(hourly_stats$day_type)) {
      hour0_row <- hourly_stats[hourly_stats$day_type == dt & hourly_stats$hour == 0, ]
      if (nrow(hour0_row) > 0) {
        closing_row <- hour0_row
        closing_row$hour <- 24
        hourly_stats <- rbind(hourly_stats, closing_row)
      }
    }
    hourly_stats <- hourly_stats[order(hourly_stats$day_type, hourly_stats$hour), ]
  } else {
    hour0_row <- hourly_stats[hourly_stats$hour == 0, ]
    if (nrow(hour0_row) > 0) {
      closing_row <- hour0_row
      closing_row$hour <- 24
      hourly_stats <- rbind(hourly_stats, closing_row)
    }
    hourly_stats <- hourly_stats[order(hourly_stats$hour), ]
  }

  # Get max for scaling L5/M10 arcs (protect against -Inf/NA)
  max_upper <- max(hourly_stats$upper, na.rm = TRUE)
  if (!is.finite(max_upper) || max_upper <= 0) {
    max_upper <- 100  # Default fallback value
  }
  max_y <- max_upper * 1.15

  # Start building plot
  p <- ggplot2::ggplot()

  # Add L5/M10 arcs BEFORE the main data
  if (show_L5M10) {
    # Create arc data for L5 (5 hours)
    L5_hours <- (L5_onset:(L5_onset + 4)) %% 24
    L5_arc <- data.frame(
      hour = L5_hours,
      ymin = 0,
      ymax = max_y * 0.95
    )

    # Create arc data for M10 (10 hours)
    M10_hours <- (M10_onset:(M10_onset + 9)) %% 24
    M10_arc <- data.frame(
      hour = M10_hours,
      ymin = 0,
      ymax = max_y * 0.95
    )

    # Add L5 arc (blue)
    p <- p +
      ggplot2::geom_rect(
        data = L5_arc,
        ggplot2::aes(xmin = hour - 0.5, xmax = hour + 0.5, ymin = ymin, ymax = ymax),
        fill = "#2196F3", alpha = 0.15
      )

    # Add M10 arc (orange)
    p <- p +
      ggplot2::geom_rect(
        data = M10_arc,
        ggplot2::aes(xmin = hour - 0.5, xmax = hour + 0.5, ymin = ymin, ymax = ymax),
        fill = "#FF9800", alpha = 0.15
      )
  }

  # Add activity data
  if (by_day_type) {
    if (show_ribbon) {
      p <- p + ggplot2::geom_ribbon(
        data = hourly_stats,
        ggplot2::aes(x = hour, ymin = lower, ymax = upper,
                     fill = day_type, group = day_type),
        alpha = 0.2
      )
    }

    p <- p +
      ggplot2::geom_line(
        data = hourly_stats,
        ggplot2::aes(x = hour, y = mean, color = day_type, group = day_type),
        linewidth = 1.2
      ) +
      ggplot2::geom_point(
        data = hourly_stats[hourly_stats$hour < 24, ],  # Exclude closing point
        ggplot2::aes(x = hour, y = mean, color = day_type),
        size = 2.5
      ) +
      ggplot2::scale_color_manual(
        values = c("Weekday" = "#3498DB", "Weekend" = "#E74C3C"),
        name = ""
      ) +
      ggplot2::scale_fill_manual(
        values = c("Weekday" = "#3498DB", "Weekend" = "#E74C3C"),
        name = ""
      )

  } else {
    if (show_ribbon) {
      p <- p + ggplot2::geom_ribbon(
        data = hourly_stats,
        ggplot2::aes(x = hour, ymin = lower, ymax = upper, group = 1),
        fill = "#3498DB", alpha = 0.25
      )
    }

    p <- p +
      ggplot2::geom_line(
        data = hourly_stats,
        ggplot2::aes(x = hour, y = mean, group = 1),
        color = "#3498DB", linewidth = 1.2
      ) +
      ggplot2::geom_point(
        data = hourly_stats[hourly_stats$hour < 24, ],  # Exclude closing point
        ggplot2::aes(x = hour, y = mean),
        color = "#3498DB", size = 2.5
      )
  }

  # Add time markers
  p <- p +
    ggplot2::geom_vline(xintercept = c(6, 12, 18), linetype = "dashed",
                        color = "gray60", linewidth = 0.3)

  # Build subtitle with metrics
  subtitle_parts <- c()

  if (show_L5M10) {
    subtitle_parts <- c(subtitle_parts,
                        sprintf("L5: %02d:00 (blue)", L5_onset),
                        sprintf("M10: %02d:00 (orange)", M10_onset))
  }

  if (show_metrics && exists("RA") && !is.na(RA)) {
    ra_interp <- if (RA >= 0.85) "Robust" else if (RA >= 0.65) "Moderate" else "Dampened"
    subtitle_parts <- c(subtitle_parts, sprintf("RA: %.2f (%s)", RA, ra_interp))

    if (!is.null(IS_value)) {
      is_interp <- if (IS_value >= 0.6) "Strong" else if (IS_value >= 0.4) "Moderate" else "Weak"
      subtitle_parts <- c(subtitle_parts, sprintf("IS: %.2f (%s)", IS_value, is_interp))
    }

    if (!is.null(IV_value)) {
      iv_interp <- if (IV_value <= 0.8) "Stable" else if (IV_value <= 1.2) "Moderate" else "Fragmented"
      subtitle_parts <- c(subtitle_parts, sprintf("IV: %.2f (%s)", IV_value, iv_interp))
    }
  }

  subtitle_text <- paste(subtitle_parts, collapse = " | ")

  if (show_reference && show_metrics) {
    caption_text <- "RA > 0.85 = robust | IS > 0.6 = strong stability | IV < 0.8 = stable pattern"
  } else {
    caption_text <- NULL
  }

  p <- p +
    ggplot2::coord_polar(start = -pi/2) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = sprintf("%02d:00", seq(0, 23, 3)),
      limits = c(0, 24)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max_y),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = if (length(subtitle_parts) > 0) subtitle_text else NULL,
      x = "",
      y = "Activity (counts/min)",
      caption = caption_text
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "gray30"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0.5, color = "gray50",
                                            face = "italic"),
      axis.text.x = ggplot2::element_text(size = 9, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray85", linewidth = 0.3)
    )

  return(p)
}


# IS/IV VISUALIZATION

#' Interdaily Stability / Intradaily Variability Visualization
#'
#' Creates visualizations for IS and IV circadian metrics.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param counts_col Name of counts column
#' @param is_value Optional. Pre-computed IS value (default: NULL, will be calculated)
#' @param iv_value Optional. Pre-computed IV value (default: NULL, will be calculated)
#' @param title Character. Plot title (default: "Interdaily Stability / Intradaily Variability")
#' @param subtitle Character. Optional plot subtitle (default: NULL)
#'
#' @return A ggplot2 object showing hourly profiles and IS/IV values
#'
#' @export
plot_is_iv <- function(data,
                        timestamp_col = "timestamp",
                        counts_col = "axis1",
                        is_value = NULL,
                        iv_value = NULL,
                        title = "Interdaily Stability / Intradaily Variability",
                        subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Validate required columns exist
  if (!timestamp_col %in% names(data)) {
    stop("Timestamp column '", timestamp_col, "' not found in data")
  }
  if (!counts_col %in% names(data)) {
    stop("Counts column '", counts_col, "' not found in data")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Ensure counts column is numeric
  if (!is.numeric(data[[counts_col]])) {
    data[[counts_col]] <- as.numeric(data[[counts_col]])
  }

  # Add time components
  data$date <- as.Date(data[[timestamp_col]])
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$activity <- as.numeric(data[[counts_col]])
  data$activity[is.na(data$activity)] <- 0

  # Calculate hourly means per day
  daily_hourly <- aggregate(activity ~ date + hour, data = data, FUN = mean, na.rm = TRUE)

  # Calculate overall hourly means
  hourly_profile <- aggregate(activity ~ hour, data = daily_hourly, FUN = mean, na.rm = TRUE)
  names(hourly_profile)[2] <- "mean_activity"

  # Calculate grand mean (always needed for plot)
  grand_mean <- mean(data$activity, na.rm = TRUE)

  # Use pre-calculated IS/IV if provided, otherwise calculate
  if (!is.null(is_value) && !is.null(iv_value)) {
    IS <- is_value
    IV <- iv_value
  } else {
    # Calculate IS and IV
    n <- nrow(data)

    # Hourly means across all data
    hourly_means <- aggregate(activity ~ hour, data = data, FUN = mean, na.rm = TRUE)
    n_h <- 24
    p_val <- n / n_h  # epochs per hour on average

    # IS = variance of hourly means / variance of all data
    var_hourly <- var(hourly_means$activity)
    var_total <- var(data$activity, na.rm = TRUE)
    IS <- if (var_total > 0) n_h * p_val * var_hourly / (n * var_total) else NA

    # IV = mean squared difference of consecutive epochs / variance
    diff_sq <- diff(data$activity)^2
    IV <- if (var_total > 0) n * mean(diff_sq, na.rm = TRUE) / ((n - 1) * var_total) else NA
  }

  # Create hourly profile plot with individual days
  p <- ggplot2::ggplot()

  # Add individual day lines (light gray)
  for (d in unique(daily_hourly$date)) {
    day_data <- daily_hourly[daily_hourly$date == d, ]
    p <- p +
      ggplot2::geom_line(
        data = day_data,
        ggplot2::aes(x = hour, y = activity, group = date),
        color = "gray80", linewidth = 0.3, alpha = 0.5
      )
  }

  # Add mean profile
  p <- p +
    ggplot2::geom_line(
      data = hourly_profile,
      ggplot2::aes(x = hour, y = mean_activity),
      color = "#E74C3C", linewidth = 1.5
    ) +
    ggplot2::geom_point(
      data = hourly_profile,
      ggplot2::aes(x = hour, y = mean_activity),
      color = "#E74C3C", size = 2.5
    )

  # Add grand mean line
  p <- p +
    ggplot2::geom_hline(yintercept = grand_mean, linetype = "dashed",
                        color = "#27AE60", linewidth = 0.8)

  # Add IS/IV annotation with interpretations
  is_interp <- if (is.na(IS)) "" else if (IS >= 0.6) "Strong" else if (IS >= 0.4) "Moderate" else "Weak"
  iv_interp <- if (is.na(IV)) "" else if (IV <= 0.8) "Stable" else if (IV <= 1.2) "Moderate" else "Fragmented"

  annotation_text <- sprintf(
    "IS = %.2f (%s)\nIV = %.2f (%s)\nMean = %.0f",
    IS, is_interp, IV, iv_interp, grand_mean
  )

  p <- p +
    ggplot2::annotate(
      "label",
      x = 22, y = max(hourly_profile$mean_activity) * 0.9,
      label = annotation_text,
      hjust = 1, vjust = 1,
      size = 3.5, fill = "white", alpha = 0.9,
      fontface = "bold"
    )

  # Use provided subtitle or generate default
  if (is.null(subtitle)) {
    subtitle <- "Gray lines = individual days, Red line = mean profile, Green dashed = grand mean"
  }

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 4),
      labels = sprintf("%02d:00", seq(0, 23, 4))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Hour of Day",
      y = "Activity (counts/min)"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50", size = 9),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}


#' Interactive Kaplan-Meier Survival Curves for Sedentary Bouts
#'
#' Creates Kaplan-Meier survival curves showing probability of sedentary bouts
#' continuing beyond given durations, with confidence intervals.
#'
#' @param bout_durations Numeric vector of bout durations in minutes
#' @param groups Optional factor vector for group comparison
#' @param show_ci Logical. Show 95% confidence intervals? (default: TRUE)
#' @param show_median Logical. Show median survival time? (default: TRUE)
#' @param max_time Maximum time to display (minutes). NULL for auto.
#' @param title Plot title
#' @param color_palette Named vector of colors for groups
#'
#' @return A ggplot2 object
#'
#' @export
plot_survival_curves <- function(bout_durations,
                                  groups = NULL,
                                  show_ci = TRUE,
                                  show_median = TRUE,
                                  max_time = NULL,
                                  title = "Sedentary Bout Survival Analysis",
                                  color_palette = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  bout_durations <- bout_durations[!is.na(bout_durations) & bout_durations > 0]

  if (length(bout_durations) < 10) {
    stop("At least 10 valid bouts required for survival analysis")
  }

  if (is.null(max_time)) {
    max_time <- min(quantile(bout_durations, 0.95), 120)
  }

  # Calculate KM survival
  .calc_km <- function(durations, max_t) {
    durations <- sort(durations)
    n <- length(durations)
    times <- sort(unique(c(0, durations[durations <= max_t], max_t)))
    survival <- sapply(times, function(t) sum(durations > t) / n)
    n_at_risk <- sapply(times, function(t) sum(durations >= t))
    n_events <- sapply(times, function(t) sum(durations == t))

    var_log_s <- cumsum(n_events / (n_at_risk * (n_at_risk - n_events + 0.001)))
    se <- sqrt(var_log_s)
    ci_lower <- pmax(0, survival * exp(-1.96 * se / (survival + 0.001)))
    ci_upper <- pmin(1, survival * exp(1.96 * se / (survival + 0.001)))

    data.frame(time = times, survival = survival, ci_lower = ci_lower, ci_upper = ci_upper)
  }

  if (is.null(groups)) {
    surv_data <- .calc_km(bout_durations, max_time)
    surv_data$group <- "All Bouts"
    groups_to_plot <- "All Bouts"
    if (is.null(color_palette)) color_palette <- c("All Bouts" = "#1565C0")
  } else {
    groups <- as.factor(groups)
    groups_to_plot <- levels(groups)
    surv_data <- do.call(rbind, lapply(groups_to_plot, function(g) {
      d <- .calc_km(bout_durations[groups == g], max_time)
      d$group <- g
      d
    }))
    if (is.null(color_palette)) {
      color_palette <- setNames(c("#1565C0", "#2E7D32", "#F57C00", "#C62828")[1:length(groups_to_plot)], groups_to_plot)
    }
  }

  p <- ggplot2::ggplot(surv_data, ggplot2::aes(x = time, y = survival, color = group))

  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = group),
                                   alpha = 0.15, color = NA)
  }

  p <- p +
    ggplot2::geom_step(linewidth = 1.2) +
    ggplot2::scale_color_manual(values = color_palette, name = NULL) +
    ggplot2::scale_fill_manual(values = color_palette, name = NULL)

  if (show_median) {
    p <- p + ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", color = "#9E9E9E")
  }

  p <- p +
    ggplot2::scale_x_continuous(limits = c(0, max_time), expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0%", "25%", "50%", "75%", "100%"),
                                 limits = c(0, 1), expand = c(0.01, 0.01)) +
    ggplot2::labs(title = title,
                  subtitle = sprintf("n = %d bouts | Median: %.1f min", length(bout_durations), median(bout_durations)),
                  x = "Bout Duration (minutes)", y = "Survival Probability") +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = if (length(groups_to_plot) > 1) "right" else "none",
      panel.background = ggplot2::element_rect(fill = "white", color = "#BDBDBD")
    )

  return(p)
}


#' Vector Magnitude Heatmap
#'
#' Creates a heatmap of Vector Magnitude over time with each row representing
#' a day and color intensity representing activity level.
#'
#' @param data Data frame with timestamp and tri-axial data
#' @param timestamp_col Name of timestamp column
#' @param axis1_col Name of axis1 column
#' @param axis2_col Name of axis2 column
#' @param axis3_col Name of axis3 column
#' @param vm_col Optional pre-computed VM column
#' @param aggregation Aggregation period: "minute", "5min", "15min", "hour"
#' @param title Plot title
#'
#' @return A ggplot2 object
#'
#' @export
plot_vm_heatmap <- function(data,
                             timestamp_col = "timestamp",
                             axis1_col = "axis1",
                             axis2_col = "axis2",
                             axis3_col = "axis3",
                             vm_col = NULL,
                             aggregation = "15min",
                             title = "Vector Magnitude Activity Heatmap") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Calculate or use VM
  if (!is.null(vm_col) && vm_col %in% names(data)) {
    data$vm <- data[[vm_col]]
  } else if (all(c(axis1_col, axis2_col, axis3_col) %in% names(data))) {
    data$vm <- sqrt(data[[axis1_col]]^2 + data[[axis2_col]]^2 + data[[axis3_col]]^2)
  } else {
    stop("Either vm_col or all three axis columns must be provided")
  }

  data$date <- as.Date(data[[timestamp_col]])
  data$hour <- as.numeric(format(data[[timestamp_col]], "%H"))
  data$minute <- as.numeric(format(data[[timestamp_col]], "%M"))

  agg_minutes <- switch(aggregation, "minute" = 1, "5min" = 5, "15min" = 15, "hour" = 60, 15)
  data$time_bin <- floor((data$hour * 60 + data$minute) / agg_minutes) * agg_minutes / 60

  agg_data <- aggregate(vm ~ date + time_bin, data = data, FUN = mean, na.rm = TRUE)
  unique_dates <- sort(unique(agg_data$date))
  agg_data$date_factor <- factor(agg_data$date, levels = rev(unique_dates))

  vm_95 <- quantile(agg_data$vm, 0.95, na.rm = TRUE)

  p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = time_bin, y = date_factor, fill = vm)) +
    ggplot2::geom_tile(color = NA) +
    ggplot2::scale_fill_gradientn(
      colors = c("#ECEFF1", "#81D4FA", "#4CAF50", "#FFC107", "#FF5722", "#B71C1C"),
      limits = c(0, vm_95), name = "VM\n(counts)",
      guide = ggplot2::guide_colorbar(barwidth = 1, barheight = 10)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "00:00"),
      limits = c(0, 24), expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(labels = function(x) format(as.Date(x), "%a %m/%d")) +
    ggplot2::labs(title = title,
                  subtitle = sprintf("Aggregation: %s | %s to %s", aggregation,
                                     format(min(unique_dates), "%b %d"),
                                     format(max(unique_dates), "%b %d, %Y")),
                  x = "Time of Day", y = NULL) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.y = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = "#BDBDBD")
    )

  return(p)
}
