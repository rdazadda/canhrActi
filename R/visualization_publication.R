# Publication-Ready Visualization Functions
# canhrActi - Center for Alaska Native Health Research
# University of Alaska Fairbanks
#
# Enhanced visualizations for publications and presentations:
# - Acceleration distribution plots
# - 24-hour radar/clock plots
# - Multi-day heatmaps with wear time overlay
# - Publication-ready figure export

#' Acceleration Distribution Plot
#'
#' Creates a histogram or density plot of acceleration values, useful for
#' understanding the intensity distribution profile of activity data.
#'
#' @param data Data frame with acceleration data, or numeric vector
#' @param acc_col Name of acceleration column (default: "axis1")
#' @param type Plot type: "histogram", "density", or "both" (default: "both")
#' @param log_scale Logical. Use log scale for x-axis? (default: FALSE)
#' @param bins Number of bins for histogram (default: 50)
#' @param show_percentiles Logical. Show percentile markers? (default: TRUE)
#' @param percentiles Percentiles to mark (default: c(25, 50, 75, 95))
#' @param color Fill color (default: "#236192")
#' @param wear_time Optional logical vector for wear time filtering
#' @param title Plot title
#' @param subtitle Plot subtitle
#'
#' @return A ggplot2 object
#'
#' @details
#' This plot shows the distribution of acceleration values across the
#' recording period. The shape reveals:
#' - Peak at low values: Mostly sedentary
#' - Right skew: Typical activity pattern
#' - Multiple modes: Distinct activity periods
#'
#' @references
#' Rowlands AV, et al. (2018). Beyond cut points: Accelerometer metrics
#' that capture the physical activity profile. Med Sci Sports Exerc.
#'
#' @examples
#' \dontrun{
#' data <- read.agd("participant.agd")
#' plot_acceleration_distribution(data$counts, acc_col = "axis1")
#' }
#'
#' @export
plot_acceleration_distribution <- function(data,
                                           acc_col = "axis1",
                                           type = "both",
                                           log_scale = FALSE,
                                           bins = 50,
                                           show_percentiles = TRUE,
                                           percentiles = c(25, 50, 75, 95),
                                           color = "#236192",
                                           wear_time = NULL,
                                           title = "Acceleration Distribution",
                                           subtitle = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Extract acceleration values
  if (is.data.frame(data)) {
    if (!acc_col %in% names(data)) {
      stop("Column '", acc_col, "' not found in data")
    }
    acc <- data[[acc_col]]
  } else if (is.numeric(data)) {
    acc <- data
  } else {
    stop("data must be a data.frame or numeric vector")
  }

  # Apply wear time filter
  if (!is.null(wear_time)) {
    if (length(wear_time) != length(acc)) {
      stop("wear_time must have same length as data")
    }
    acc <- acc[wear_time]
  }

  # Remove NA and negative values
  acc <- acc[!is.na(acc) & acc >= 0]

  if (length(acc) == 0) {
    stop("No valid acceleration values after filtering")
  }

  # Create data frame
  df <- data.frame(acceleration = acc)

  # Calculate percentiles
  pct_values <- quantile(acc, probs = percentiles / 100, na.rm = TRUE)
  pct_df <- data.frame(
    percentile = paste0("P", percentiles),
    value = as.numeric(pct_values),
    label = paste0("P", percentiles, ": ", round(pct_values, 0))
  )

  # Generate subtitle if not provided
  if (is.null(subtitle)) {
    subtitle <- sprintf("N = %s epochs | Mean = %.1f | Median = %.1f",
                        format(length(acc), big.mark = ","),
                        mean(acc, na.rm = TRUE),
                        median(acc, na.rm = TRUE))
  }

  # Build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = acceleration))

  if (type %in% c("histogram", "both")) {
    p <- p + ggplot2::geom_histogram(
      bins = bins,
      fill = color,
      color = "white",
      alpha = if (type == "both") 0.6 else 0.8
    )
  }

  if (type %in% c("density", "both")) {
    p <- p + ggplot2::geom_density(
      color = color,
      fill = color,
      alpha = 0.2,
      linewidth = 1
    )
  }

  # Add percentile lines
  if (show_percentiles) {
    p <- p + ggplot2::geom_vline(
      data = pct_df,
      ggplot2::aes(xintercept = value),
      linetype = "dashed",
      color = "#e74c3c",
      linewidth = 0.5
    )

    # Add labels at top - calculate y_max from histogram data
    # (layer_scales is deprecated in newer ggplot2 versions)
    y_max <- tryCatch({
      built <- ggplot2::ggplot_build(p)
      built$layout$panel_scales_y[[1]]$range$range[2]
    }, error = function(e) NULL)
    if (is.null(y_max)) y_max <- max(hist(acc, breaks = bins, plot = FALSE)$counts)

    p <- p + ggplot2::geom_text(
      data = pct_df,
      ggplot2::aes(x = value, y = y_max * 0.95, label = percentile),
      angle = 90, hjust = 1, vjust = -0.3,
      size = 3, color = "#e74c3c"
    )
  }

  # Apply log scale if requested
  if (log_scale) {
    p <- p + ggplot2::scale_x_log10(
      labels = scales::comma_format()
    )
  }

  # Styling
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Acceleration (counts/epoch)",
      y = if (type == "density") "Density" else "Frequency"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50", size = 10)
    )

  return(p)
}


#' Intensity Bin Distribution Plot
#'
#' Creates a bar chart showing time spent in acceleration intensity bins,
#' following the methodology of Rowlands et al. for intensity gradient analysis.
#'
#' @param data Data frame or numeric vector of acceleration values
#' @param acc_col Column name for acceleration data
#' @param bin_size Bin width in mg or counts (default: 25)
#' @param max_bins Maximum number of bins to show (default: 20)
#' @param epoch_length Epoch length in seconds for time conversion (default: 60)
#' @param wear_time Optional wear time filter
#' @param show_gradient Logical. Show intensity gradient line? (default: TRUE)
#' @param color_gradient Logical. Use gradient colors? (default: TRUE)
#' @param title Plot title
#'
#' @return A ggplot2 object
#'
#' @references
#' Rowlands AV, et al. (2018). Beyond cut points: Accelerometer metrics
#' that capture the physical activity profile.
#'
#' @export
plot_intensity_bins <- function(data,
                                acc_col = "axis1",
                                bin_size = 25,
                                max_bins = 20,
                                epoch_length = 60,
                                wear_time = NULL,
                                show_gradient = TRUE,
                                color_gradient = TRUE,
                                title = "Intensity Distribution by Bins") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Extract acceleration
  if (is.data.frame(data)) {
    acc <- data[[acc_col]]
  } else {
    acc <- data
  }

  # Apply wear time
  if (!is.null(wear_time)) {
    acc <- acc[wear_time]
  }

  acc <- acc[!is.na(acc) & acc >= 0]

  # Create bins
  breaks <- seq(0, max(acc, na.rm = TRUE) + bin_size, by = bin_size)
  breaks <- breaks[1:min(length(breaks), max_bins + 1)]

  bin_labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  bin_mids <- (breaks[-length(breaks)] + breaks[-1]) / 2

  # Count epochs per bin
  bin_counts <- as.numeric(table(cut(acc, breaks = breaks, include.lowest = TRUE, right = FALSE)))
  bin_minutes <- bin_counts * (epoch_length / 60)

  # Create data frame
  df <- data.frame(
    bin = factor(bin_labels, levels = bin_labels),
    bin_mid = bin_mids[1:length(bin_counts)],
    count = bin_counts,
    minutes = bin_minutes
  )

  # Remove zero bins from end
  last_nonzero <- max(which(df$count > 0))
  df <- df[1:last_nonzero, ]

  # Build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = bin, y = minutes))

  if (color_gradient) {
    p <- p + ggplot2::geom_col(
      ggplot2::aes(fill = bin_mid),
      color = "white",
      linewidth = 0.2
    ) +
      ggplot2::scale_fill_gradient(
        low = "#3498db",
        high = "#e74c3c",
        name = "Intensity\n(counts)",
        guide = "none"
      )
  } else {
    p <- p + ggplot2::geom_col(fill = "#236192", color = "white", linewidth = 0.2)
  }

  # Add gradient line if requested
  if (show_gradient && nrow(df) > 2) {
    # Calculate intensity gradient (log-log regression)
    df_nonzero <- df[df$minutes > 0, ]
    if (nrow(df_nonzero) >= 2) {
      log_mid <- log10(df_nonzero$bin_mid)
      log_time <- log10(df_nonzero$minutes)

      fit <- lm(log_time ~ log_mid)
      gradient <- coef(fit)[2]

      # Add annotation
      p <- p + ggplot2::annotate(
        "text",
        x = nrow(df) * 0.75,
        y = max(df$minutes) * 0.9,
        label = sprintf("Intensity Gradient: %.2f", gradient),
        fontface = "bold",
        size = 4,
        color = "#2c3e50"
      )
    }
  }

  p <- p +
    ggplot2::labs(
      title = title,
      x = "Acceleration Bin (counts)",
      y = "Time (minutes)"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
    )

  return(p)
}


#' 24-Hour Activity Clock Plot
#'
#' Creates a clock-style circular plot showing activity patterns over 24 hours,
#' with customizable metrics and styling for publication.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param activity_col Name of activity column
#' @param aggregate_func Aggregation function (default: mean)
#' @param fill_area Logical. Fill area under curve? (default: TRUE)
#' @param show_hours Logical. Show hour markers? (default: TRUE)
#' @param show_sleep_window Logical. Shade typical sleep window? (default: TRUE)
#' @param sleep_start Sleep window start hour (default: 22)
#' @param sleep_end Sleep window end hour (default: 6)
#' @param color_scheme Color scheme: "canhr", "viridis", or custom colors
#' @param title Plot title
#'
#' @return A ggplot2 object
#'
#' @details
#' The clock plot displays activity in a 24-hour circular format:
#' - 12 o'clock position = midnight
#' - 3 o'clock = 6 AM
#' - 6 o'clock = noon
#' - 9 o'clock = 6 PM
#'
#' This format intuitively shows the circadian pattern of activity.
#'
#' @export
plot_activity_clock <- function(data,
                                timestamp_col = "timestamp",
                                activity_col = "axis1",
                                aggregate_func = mean,
                                fill_area = TRUE,
                                show_hours = TRUE,
                                show_sleep_window = TRUE,
                                sleep_start = 22,
                                sleep_end = 6,
                                color_scheme = "canhr",
                                title = "24-Hour Activity Clock") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp is POSIXct
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  # Extract hour
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$activity <- data[[activity_col]]

  # Aggregate by hour
  hourly <- aggregate(activity ~ hour, data = data, FUN = aggregate_func, na.rm = TRUE)

  # Note: coord_polar() wraps the 0-24 scale automatically, so we do NOT
  # append a duplicate hour = 24 row (doing so double-plots a point/segment
  # at midnight because 0 and 24 map to the same polar angle).

  # Define color scheme
  if (color_scheme == "canhr") {
    main_color <- "#236192"
    fill_color <- "#236192"
    sleep_color <- "gray90"
  } else if (color_scheme == "viridis") {
    main_color <- "#21918c"
    fill_color <- "#21918c"
    sleep_color <- "#440154"
  } else {
    main_color <- color_scheme[1]
    fill_color <- color_scheme[1]
    sleep_color <- if (length(color_scheme) > 1) color_scheme[2] else "gray90"
  }

  # Build plot
  p <- ggplot2::ggplot(hourly, ggplot2::aes(x = hour, y = activity))

  # Add sleep window shading
  if (show_sleep_window) {
    if (sleep_start > sleep_end) {
      # Sleep spans midnight
      p <- p + ggplot2::annotate(
        "rect",
        xmin = sleep_start, xmax = 24,
        ymin = 0, ymax = Inf,
        fill = sleep_color, alpha = 0.3
      ) + ggplot2::annotate(
        "rect",
        xmin = 0, xmax = sleep_end,
        ymin = 0, ymax = Inf,
        fill = sleep_color, alpha = 0.3
      )
    } else {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = sleep_start, xmax = sleep_end,
        ymin = 0, ymax = Inf,
        fill = sleep_color, alpha = 0.3
      )
    }
  }

  # Add area fill
  if (fill_area) {
    p <- p + ggplot2::geom_area(
      fill = fill_color,
      alpha = 0.3
    )
  }

  # Add line and points
  p <- p +
    ggplot2::geom_line(color = main_color, linewidth = 1.2) +
    ggplot2::geom_point(color = main_color, size = 2)

  # Convert to polar coordinates
  p <- p + ggplot2::coord_polar(start = -pi/2)  # Start at 12 o'clock (midnight)

  # X-axis (hours)
  if (show_hours) {
    p <- p + ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00"),
      limits = c(0, 24),
      expand = c(0, 0)
    )
  }

  # Styling
  p <- p +
    ggplot2::labs(
      title = title,
      x = "",
      y = "Activity (counts)"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 9, face = "bold")
    )

  return(p)
}


#' Multi-Component Clock Plot
#'
#' Creates a 24-hour clock plot with multiple behavior components stacked,
#' showing time-use composition across the day.
#'
#' @param data Data frame with timestamp and intensity columns
#' @param timestamp_col Name of timestamp column
#' @param intensity_col Name of intensity classification column
#' @param epoch_length Epoch length in seconds (default: 60)
#' @param title Plot title
#'
#' @return A ggplot2 object
#'
#' @export
plot_composition_clock <- function(data,
                                   timestamp_col = "timestamp",
                                   intensity_col = "intensity",
                                   epoch_length = 60,
                                   title = "24-Hour Time-Use Composition") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Ensure timestamp
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$intensity <- data[[intensity_col]]

  # Count epochs per hour and intensity
  hourly <- as.data.frame(table(data$hour, data$intensity))
  names(hourly) <- c("hour", "intensity", "count")
  hourly$hour <- as.numeric(as.character(hourly$hour))
  hourly$minutes <- hourly$count * (epoch_length / 60)

  # Calculate percentage per hour
  totals <- aggregate(minutes ~ hour, data = hourly, sum)
  names(totals)[2] <- "total"
  hourly <- merge(hourly, totals, by = "hour")
  hourly$percentage <- hourly$minutes / hourly$total * 100

  # Define colors - Colorblind-safe Okabe-Ito palette
  intensity_colors <- c(
    "sedentary" = "#64748B",     # Neutral gray
    "light" = "#56B4E9",         # Sky blue
    "moderate" = "#009E73",      # Bluish green
    "vigorous" = "#E69F00",      # Orange
    "very_vigorous" = "#D55E00", # Vermillion
    "sleep" = "#0072B2"          # Dark blue
  )

  # Order intensity levels
  hourly$intensity <- factor(hourly$intensity,
                             levels = c("sleep", "sedentary", "light", "moderate", "vigorous", "very_vigorous"))

  # Build plot
  p <- ggplot2::ggplot(hourly, ggplot2::aes(x = hour, y = percentage, fill = intensity)) +
    ggplot2::geom_area(position = "stack", alpha = 0.8) +
    ggplot2::coord_polar(start = -pi/2) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00"),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(
      values = intensity_colors,
      name = "Intensity",
      na.value = "gray50"
    ) +
    ggplot2::labs(
      title = title,
      x = "",
      y = "% of Hour"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "right"
    )

  return(p)
}


#' Activity Heatmap with Wear Time Overlay
#'
#' Creates a multi-day heatmap with non-wear periods clearly marked,
#' essential for publication-quality figures.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param activity_col Name of activity column
#' @param wear_time Optional logical vector or column name for wear status
#' @param aggregate_func Function to aggregate epochs (default: mean)
#' @param color_palette Color palette for activity (default: heat colors)
#' @param nonwear_color Color for non-wear periods (default: "gray30")
#' @param show_legend Logical. Show color legend? (default: TRUE)
#' @param annotate_nonwear Logical. Mark non-wear with pattern? (default: TRUE)
#' @param title Plot title
#'
#' @return A ggplot2 object
#'
#' @details
#' This enhanced heatmap clearly distinguishes between:
#' - Activity levels (color gradient)
#' - Non-wear periods (gray with optional pattern)
#' - Missing data (white/blank)
#'
#' @export
plot_activity_heatmap_wear <- function(data,
                                       timestamp_col = "timestamp",
                                       activity_col = "axis1",
                                       wear_time = NULL,
                                       aggregate_func = mean,
                                       color_palette = NULL,
                                       nonwear_color = "gray30",
                                       show_legend = TRUE,
                                       annotate_nonwear = TRUE,
                                       title = "Activity Heatmap with Wear Time") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Default color palette
  if (is.null(color_palette)) {
    color_palette <- c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494")
  }

  # Ensure timestamp
  if (!inherits(data[[timestamp_col]], "POSIXct")) {
    data[[timestamp_col]] <- as.POSIXct(data[[timestamp_col]])
  }

  data$date <- as.Date(data[[timestamp_col]])
  data$hour <- as.integer(format(data[[timestamp_col]], "%H"))
  data$activity <- data[[activity_col]]

  # Handle wear time
  if (!is.null(wear_time)) {
    if (is.character(wear_time) && wear_time %in% names(data)) {
      data$wear <- data[[wear_time]]
    } else if (is.logical(wear_time) && length(wear_time) == nrow(data)) {
      data$wear <- wear_time
    } else {
      warning("wear_time format not recognized, ignoring")
      data$wear <- TRUE
    }
  } else {
    data$wear <- TRUE
  }

  # Keep the raw activity (pre-mask) for the non-wear fraction, and a masked
  # copy (non-wear -> NA) for the displayed activity values.
  data$activity_raw <- data$activity
  data$activity[!data$wear] <- NA
  data$is_nonwear <- !data$wear

  # Aggregate activity (masked) and the non-wear fraction with separate,
  # explicit FUNs so the two quantities are not read from the wrong column.
  # Guard all-NA cells: aggregate_func over an empty vector would warn/NaN.
  activity_agg <- aggregate(
    activity ~ date + hour,
    data = data,
    FUN = function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_real_ else aggregate_func(x)
    },
    na.action = na.pass
  )

  # Non-wear fraction: share of epochs in the cell whose raw (pre-mask)
  # activity is missing or zero. Computed from activity_raw so the fraction
  # is NOT read from the mean of the 0/1 is_nonwear flag (the original bug).
  nonwear_agg <- aggregate(
    activity_raw ~ date + hour,
    data = data,
    FUN = function(x) {
      if (length(x) == 0) NA_real_ else mean(is.na(x) | x == 0)
    },
    na.action = na.pass
  )
  names(nonwear_agg)[names(nonwear_agg) == "activity_raw"] <- "nonwear_pct"

  # Combine the two aggregations on date + hour
  heatmap_data <- merge(
    activity_agg,
    nonwear_agg[, c("date", "hour", "nonwear_pct")],
    by = c("date", "hour")
  )

  # Mark predominantly non-wear cells
  heatmap_data$display_nonwear <- heatmap_data$nonwear_pct > 0.5

  # Build plot
  p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = hour, y = date))

  # Add activity tiles
  p <- p + ggplot2::geom_tile(
    ggplot2::aes(fill = activity),
    color = "white",
    linewidth = 0.1
  )

  # Add non-wear overlay
  if (annotate_nonwear) {
    nonwear_data <- heatmap_data[heatmap_data$display_nonwear, ]
    if (nrow(nonwear_data) > 0) {
      p <- p + ggplot2::geom_tile(
        data = nonwear_data,
        fill = nonwear_color,
        color = "white",
        linewidth = 0.1,
        alpha = 0.9
      )
    }
  }

  # Color scale
  p <- p + ggplot2::scale_fill_gradientn(
    colors = color_palette,
    name = "Activity\n(counts)",
    na.value = nonwear_color,
    guide = if (show_legend) "colorbar" else "none"
  )

  # Axes
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = sprintf("%02d:00", seq(0, 23, 3)),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_date(
      date_labels = "%a %m/%d",
      expand = c(0, 0)
    )

  # Labels and theme
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = "Gray cells indicate non-wear periods",
      x = "Hour of Day",
      y = "Date"
    ) +
    theme_canhrActi(grid = FALSE) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50", size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  return(p)
}


#' Export Publication-Ready Figure
#'
#' Exports a ggplot2 figure with journal-specific dimensions and resolution,
#' ready for submission.
#'
#' @param plot A ggplot2 object
#' @param filename Output filename (extension determines format)
#' @param width Figure width in inches (default: 7)
#' @param height Figure height in inches (default: 5)
#' @param dpi Resolution in dots per inch (default: 300)
#' @param format Output format: "png", "pdf", "eps", "tiff", "svg"
#' @param preset Journal preset: "default", "nature", "science", "plosone",
#'        "jama", "nejm", "lancet", "cell", "ajph"
#' @param transparent Logical. Transparent background? (default: FALSE)
#' @param compression TIFF compression: "none", "lzw", "rle" (default: "lzw")
#'
#' @return Invisible path to saved file
#'
#' @details
#' Journal presets apply recommended dimensions:
#' - Nature: 89mm (single) or 183mm (double) column
#' - Science: 9cm (single) or 18.2cm (double) column
#' - PLOS ONE: 13.2cm (single) or 17.4cm (double) column
#'
#' @examples
#' \dontrun{
#' p <- plot_acceleration_distribution(data)
#' export_publication_figure(p, "figure1.tiff", preset = "nature")
#' }
#'
#' @export
export_publication_figure <- function(plot,
                                      filename,
                                      width = 7,
                                      height = 5,
                                      dpi = 300,
                                      format = NULL,
                                      preset = "default",
                                      transparent = FALSE,
                                      compression = "lzw") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Determine format from extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(filename))
    format <- if (ext %in% c("png", "pdf", "eps", "tiff", "tif", "svg")) ext else "png"
    if (ext == "tif") format <- "tiff"
  }

  # Apply journal presets (dimensions in inches)
  presets <- list(
    default = list(width = 7, height = 5, dpi = 300),
    nature = list(width = 3.5, height = 2.5, dpi = 300),       # 89mm single column
    nature_double = list(width = 7.2, height = 5, dpi = 300),  # 183mm double column
    science = list(width = 3.54, height = 2.5, dpi = 300),     # 9cm single column
    plosone = list(width = 5.2, height = 4, dpi = 300),        # 13.2cm single column
    jama = list(width = 3.25, height = 3, dpi = 350),          # Single column
    nejm = list(width = 3.5, height = 3, dpi = 300),           # Single column
    lancet = list(width = 3.35, height = 3, dpi = 300),        # 85mm single column
    cell = list(width = 3.35, height = 2.75, dpi = 300),       # 85mm single column
    ajph = list(width = 3.5, height = 3, dpi = 300)            # AJPH single column
  )

  if (preset %in% names(presets)) {
    settings <- presets[[preset]]
    width <- settings$width
    height <- settings$height
    dpi <- settings$dpi
  }

  # Background
  bg <- if (transparent) "transparent" else "white"

  # Save based on format
  if (format == "png") {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = bg,
      device = "png"
    )
  } else if (format == "pdf") {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      device = grDevices::cairo_pdf
    )
  } else if (format == "eps") {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      device = grDevices::cairo_ps
    )
  } else if (format == "tiff") {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      compression = compression,
      bg = bg,
      device = "tiff"
    )
  } else if (format == "svg") {
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      device = "svg"
    )
  }

  message("Figure saved: ", filename, " (", width, "x", height, " in, ", dpi, " dpi)")
  invisible(filename)
}


#' Create Multi-Panel Publication Figure
#'
#' Combines multiple plots into a single multi-panel figure with
#' consistent styling and panel labels.
#'
#' @param plots List of ggplot2 objects
#' @param ncol Number of columns (default: 2)
#' @param nrow Number of rows (calculated if NULL)
#' @param labels Panel labels: "A", "a", "1", or custom vector
#' @param label_size Label font size (default: 14)
#' @param label_face Label font face (default: "bold")
#' @param common_legend Logical. Use shared legend? (default: TRUE)
#' @param legend_position Position: "right", "bottom", "none"
#'
#' @return A combined ggplot object (patchwork)
#'
#' @export
create_multipanel_figure <- function(plots,
                                     ncol = 2,
                                     nrow = NULL,
                                     labels = "A",
                                     label_size = 14,
                                     label_face = "bold",
                                     common_legend = TRUE,
                                     legend_position = "right") {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for multi-panel figures. Install with: install.packages('patchwork')")
  }

  n_plots <- length(plots)

  # Calculate rows if not specified
  if (is.null(nrow)) {
    nrow <- ceiling(n_plots / ncol)
  }

  # Generate labels
  if (length(labels) == 1) {
    if (labels == "A") {
      panel_labels <- LETTERS[1:n_plots]
    } else if (labels == "a") {
      panel_labels <- letters[1:n_plots]
    } else if (labels == "1") {
      panel_labels <- as.character(1:n_plots)
    } else {
      panel_labels <- rep(labels, n_plots)
    }
  } else {
    panel_labels <- labels
  }

  # Combine plots using patchwork
  combined <- patchwork::wrap_plots(plots, ncol = ncol, nrow = nrow)

  # Add panel labels
  combined <- combined + patchwork::plot_annotation(
    tag_levels = list(panel_labels),
    theme = ggplot2::theme(
      plot.tag = ggplot2::element_text(size = label_size, face = label_face)
    )
  )

  # Handle common legend
  if (common_legend) {
    combined <- combined + patchwork::plot_layout(
      guides = "collect"
    ) & ggplot2::theme(legend.position = legend_position)
  }

  return(combined)
}


#' Create Summary Figure Panel
#'
#' Creates a standardized multi-panel summary figure for accelerometer
#' analysis, suitable for publications.
#'
#' @param data Data frame with timestamp and activity columns
#' @param timestamp_col Name of timestamp column
#' @param activity_col Name of activity column
#' @param wear_time Optional wear time vector
#' @param include Panels to include: "heatmap", "clock", "distribution", "daily"
#' @param title Overall figure title
#'
#' @return A patchwork object
#'
#' @export
create_summary_figure <- function(data,
                                  timestamp_col = "timestamp",
                                  activity_col = "axis1",
                                  wear_time = NULL,
                                  include = c("heatmap", "clock", "distribution"),
                                  title = "Accelerometer Data Summary") {

  plots <- list()

  if ("heatmap" %in% include) {
    plots$heatmap <- plot_activity_heatmap_wear(
      data,
      timestamp_col = timestamp_col,
      activity_col = activity_col,
      wear_time = wear_time,
      title = "Activity Heatmap"
    )
  }

  if ("clock" %in% include) {
    plots$clock <- plot_activity_clock(
      data,
      timestamp_col = timestamp_col,
      activity_col = activity_col,
      title = "24-Hour Pattern"
    )
  }

  if ("distribution" %in% include) {
    plots$distribution <- plot_acceleration_distribution(
      data,
      acc_col = activity_col,
      title = "Intensity Distribution"
    )
  }

  if (length(plots) == 0) {
    stop("No valid panels specified in 'include'")
  }

  # Combine
  combined <- create_multipanel_figure(plots, ncol = min(length(plots), 2))

  return(combined)
}
