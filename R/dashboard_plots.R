# Charts moved out of the dashboard modules (mod_activity, mod_circadian, mod_sedentary)
# so the Visualization tab can export them. Same geoms, colours and labels as the tabs;
# the hardcoded white plot.background is dropped because theme_canhrActi() sets it.

#' Mean Activity by Hour of Day
#'
#' Draws the hourly pattern the Activity page shows: mean activity counts
#' against clock hour, as a filled area with the line and its hourly points on
#' top. Every recording handed in contributes its own hourly means and they are
#' averaged per clock hour, so the same function draws one participant or a
#' whole cohort.
#'
#' @param hourly Hourly activity means: one data frame with the columns \code{hour}
#'   (0-23) and \code{counts}, or a list of such data frames (one per recording) which
#'   are pooled and averaged per hour. \code{NULL}, empty and wrongly-shaped entries are
#'   skipped.
#' @param title Optional plot title. \code{NULL} (the default) draws no title.
#'
#' @return A \code{ggplot} object: mean activity counts (y) against hour of day
#'   (x). On empty or unusable input an annotated empty \code{ggplot} is returned
#'   instead; the function never errors.
#'
#' @details
#' The pooled mean is unweighted across recordings. An hour no recording covered is
#' absent from the line rather than drawn as zero.
#'
#' @seealso \code{\link{plot_intensity_hours}}
#'
#' @examples
#' \dontrun{
#' hourly <- data.frame(hour = 0:23,
#'                      counts = 200 + 400 * sin(pi * (0:23) / 24))
#' plot_hourly_pattern(hourly)
#' }
#'
#' @export
plot_hourly_pattern <- function(hourly, title = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_hourly_pattern().")
  }

  if (missing(hourly) || is.null(hourly) || length(hourly) == 0L) {
    return(.circ_empty_plot("No hourly data", title = title))
  }

  frames <- if (is.data.frame(hourly)) list(hourly) else as.list(hourly)
  keep <- vapply(
    frames,
    function(h) is.data.frame(h) && nrow(h) > 0L &&
      all(c("hour", "counts") %in% names(h)),
    logical(1)
  )
  frames <- frames[keep]
  if (length(frames) == 0L) {
    return(.circ_empty_plot("Could not compute hourly patterns", title = title))
  }

  all_hourly <- do.call(rbind, lapply(frames, function(h) {
    data.frame(
      hour = suppressWarnings(as.numeric(h$hour)),
      counts = suppressWarnings(as.numeric(h$counts))
    )
  }))
  all_hourly <- all_hourly[is.finite(all_hourly$hour) &
                             is.finite(all_hourly$counts), , drop = FALSE]
  if (nrow(all_hourly) == 0L) {
    return(.circ_empty_plot("Could not compute hourly patterns", title = title))
  }

  # mean per clock hour across recordings
  means <- tapply(all_hourly$counts, all_hourly$hour, mean, na.rm = TRUE)
  avg_hourly <- data.frame(
    hour = as.numeric(names(means)),
    counts = as.numeric(means)
  )
  avg_hourly <- avg_hourly[order(avg_hourly$hour), , drop = FALSE]

  ggplot2::ggplot(avg_hourly, ggplot2::aes(x = .data$hour, y = .data$counts)) +
    ggplot2::geom_area(fill = "#3b82f6", alpha = 0.2) +
    ggplot2::geom_line(color = "#3b82f6", linewidth = 1.5) +
    ggplot2::geom_point(color = "#3b82f6", size = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 2),
                                labels = sprintf("%02d:00", seq(0, 23, 2))) +
    ggplot2::labs(title = title, x = "Hour of Day", y = "Mean Activity Counts") +
    .circ_theme() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 13, color = "#64748b"),
      axis.title = ggplot2::element_text(size = 14, color = "#64748b")
    )
}


#' Total Hours in Each Activity Intensity Band
#'
#' One bar per intensity band (sedentary, light, moderate, vigorous, very
#' vigorous) holding the total hours recorded in that band, labelled with the
#' hours and the share of all recorded time. Days are pooled across every
#' recording handed in.
#'
#' @param daily Per-day activity summary: one data frame with one row per day and
#'   the columns \code{sedentary_hrs}, \code{light_hrs}, \code{moderate_hrs},
#'   \code{vigorous_hrs} and \code{very_vigorous_hrs}, or a list of such data frames
#'   whose rows are pooled. An absent column contributes zero.
#' @param title Optional plot title. \code{NULL} (the default) draws no title.
#'
#' @return A \code{ggplot} object: total hours (y) by intensity band (x). On empty
#'   or unusable input an annotated empty \code{ggplot} is returned instead; the
#'   function never errors.
#'
#' @details
#' The share printed under each bar is that band's hours over the sum of all five
#' bands, not over 24 h a day.
#'
#' @seealso \code{\link{plot_hourly_pattern}}
#'
#' @examples
#' \dontrun{
#' daily <- data.frame(
#'   sedentary_hrs = c(9.1, 8.4), light_hrs = c(4.2, 4.8),
#'   moderate_hrs = c(0.8, 1.1), vigorous_hrs = c(0.1, 0.2),
#'   very_vigorous_hrs = c(0, 0)
#' )
#' plot_intensity_hours(daily)
#' }
#'
#' @export
plot_intensity_hours <- function(daily, title = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_intensity_hours().")
  }

  if (missing(daily) || is.null(daily) || length(daily) == 0L) {
    return(.circ_empty_plot("No data yet", title = title))
  }

  frames <- if (is.data.frame(daily)) list(daily) else as.list(daily)
  keep <- vapply(
    frames,
    function(d) is.data.frame(d) && nrow(d) > 0L,
    logical(1)
  )
  frames <- frames[keep]
  if (length(frames) == 0L) {
    return(.circ_empty_plot("No valid data found", title = title))
  }

  col_sum <- function(d, nm) {
    if (!(nm %in% names(d))) return(0)
    v <- suppressWarnings(as.numeric(d[[nm]]))
    s <- sum(v, na.rm = TRUE)
    if (is.finite(s)) s else 0
  }

  total_days <- 0
  total_sedentary <- 0
  total_light <- 0
  total_moderate <- 0
  total_vigorous <- 0
  total_very_vigorous <- 0

  for (d in frames) {
    total_days <- total_days + nrow(d)
    total_sedentary <- total_sedentary + col_sum(d, "sedentary_hrs")
    total_light <- total_light + col_sum(d, "light_hrs")
    total_moderate <- total_moderate + col_sum(d, "moderate_hrs")
    total_vigorous <- total_vigorous + col_sum(d, "vigorous_hrs")
    total_very_vigorous <- total_very_vigorous + col_sum(d, "very_vigorous_hrs")
  }

  if (total_days <= 0) {
    return(.circ_empty_plot("No valid data found", title = title))
  }

  total_hours <- total_sedentary + total_light + total_moderate +
    total_vigorous + total_very_vigorous
  if (!is.finite(total_hours) || total_hours <= 0) {
    return(.circ_empty_plot("No valid data found", title = title))
  }

  hours <- c(total_sedentary, total_light, total_moderate,
             total_vigorous, total_very_vigorous)

  df <- data.frame(
    intensity = factor(
      c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
      levels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")
    ),
    hours = hours,
    pct = hours / total_hours * 100
  )

  colors <- c("Sedentary" = "#94a3b8", "Light" = "#3b82f6", "Moderate" = "#f59e0b",
              "Vigorous" = "#f97316", "Very Vigorous" = "#ef4444")

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$intensity, y = .data$hours, fill = .data$intensity)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1fh\n(%.1f%%)", .data$hours, .data$pct)),
      vjust = -0.3, size = 4, fontface = "bold", color = "#1e293b"
    ) +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = title, x = NULL, y = "Total Hours") +
    .circ_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14, face = "bold", color = "#334155"),
      axis.text.y = ggplot2::element_text(size = 13, color = "#64748b"),
      axis.title.y = ggplot2::element_text(size = 14, color = "#64748b",
                                           margin = ggplot2::margin(r = 10))
    )
}


#' Plot the 24-Hour Activity Profile with the L5 and M10 Windows
#'
#' Draws the averaged 24-hour activity profile produced by
#' \code{\link{circadian.rhythm}} (its \code{hourly_profile} component): mean
#' activity per clock hour as a line with points, a \eqn{\pm 1} SD ribbon, and
#' the least-active 5-hour (L5) and most-active 10-hour (M10) windows shaded
#' where they fall on the clock. A window that runs past midnight is drawn in
#' two pieces.
#'
#' @param hourly A data frame of the averaged 24-hour profile, as returned in
#'   the \code{hourly_profile} element of \code{\link{circadian.rhythm}}, with
#'   \code{hour} (0-23) and \code{mean_counts} columns; the SD ribbon is drawn
#'   when a \code{sd_counts} column is present.
#' @param L5_start Onset of the least-active 5-hour window: a decimal hour, an
#'   \code{"HH:MM"} clock string, or a \code{POSIXct}. \code{NULL} or an
#'   unparseable value omits the shading.
#' @param M10_start Onset of the most-active 10-hour window, in the same forms
#'   as \code{L5_start}.
#' @param L5_hours,M10_hours Lengths of the two windows in hours.
#' @param title Optional plot title. \code{NULL} (default) draws no title.
#' @param value_label Y axis label.
#'
#' @return A \code{ggplot} object: hour of day (x) against mean activity (y),
#'   with the L5 and M10 windows shaded. On a missing, empty or malformed profile
#'   an annotated empty \code{ggplot} is returned; the function never errors.
#'
#' @details
#' The profile is not recomputed here, so the tab, the workbook and the exported
#' figure report the same numbers. The ribbon spans \code{mean_counts - sd_counts}
#' (floored at zero) to \code{mean_counts + sd_counts}. L5 and M10 are the
#' non-parametric markers of van Someren et al. (1999).
#'
#' @references
#' Van Someren EJW, Swaab DF, Colenda CC, Cohen W, McCall WV, Rosenquist PB
#' (1999). Bright light therapy: improved sensitivity to its effects on
#' rest-activity rhythms in Alzheimer patients by application of nonparametric
#' methods. \emph{Chronobiology International}, 16(4):505-518.
#'
#' @seealso \code{\link{circadian.rhythm}}, \code{\link{plot_actogram}},
#'   \code{\link{plot_extended_cosinor}}
#'
#' @examples
#' hourly <- data.frame(
#'   hour = 0:23,
#'   mean_counts = 100 + 200 * pmax(0, cos((0:23 - 14) * pi / 12)),
#'   sd_counts = 30
#' )
#' plot_circadian_profile(hourly, L5_start = 2.5, M10_start = 9)
#'
#' @export
plot_circadian_profile <- function(hourly, L5_start = NULL, M10_start = NULL,
                                   L5_hours = 5, M10_hours = 10, title = NULL,
                                   value_label = "Activity (counts/min)") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_circadian_profile().")
  }

  insufficient <- function() {
    .circ_empty_plot(
      "Insufficient data for the 24-hour profile",
      title = if (is.null(title)) "24-Hour Activity Profile" else title
    )
  }

  if (missing(hourly) || is.null(hourly) || !is.data.frame(hourly) ||
      nrow(hourly) == 0L ||
      !all(c("hour", "mean_counts") %in% names(hourly))) {
    return(insufficient())
  }

  hourly <- as.data.frame(hourly)
  hourly$hour <- suppressWarnings(as.numeric(hourly$hour))
  hourly$mean_counts <- suppressWarnings(as.numeric(hourly$mean_counts))
  if (!any(is.finite(hourly$hour) & is.finite(hourly$mean_counts))) {
    return(insufficient())
  }

  has_sd <- "sd_counts" %in% names(hourly)
  if (has_sd) {
    hourly$sd_counts <- suppressWarnings(as.numeric(hourly$sd_counts))
    has_sd <- any(is.finite(hourly$sd_counts))
  }

  # a window that crosses midnight is drawn in two pieces
  win_band <- function(start_h, len_h, fill) {
    # accepts a decimal hour, an "HH:MM" clock string, or a POSIXct
    if (inherits(start_h, "POSIXct") || inherits(start_h, "POSIXlt")) {
      start_h <- as.numeric(format(start_h, "%H")) +
        as.numeric(format(start_h, "%M")) / 60
    }
    if (is.character(start_h)) {
      p <- strsplit(start_h, ":")[[1]]
      start_h <- suppressWarnings(as.numeric(p[1]) + as.numeric(p[2]) / 60)
    }
    start_h <- suppressWarnings(as.numeric(start_h))
    len_h <- suppressWarnings(as.numeric(len_h))
    if (length(start_h) != 1 || is.na(start_h)) return(NULL)
    if (length(len_h) != 1 || is.na(len_h) || len_h <= 0) return(NULL)
    s0 <- start_h %% 24
    e0 <- s0 + len_h
    segs <- if (e0 <= 24) list(c(s0, e0)) else list(c(s0, 24), c(0, e0 - 24))
    lapply(segs, function(se) ggplot2::annotate("rect", xmin = se[1], xmax = se[2],
                                                ymin = -Inf, ymax = Inf,
                                                fill = fill, alpha = 0.13))
  }

  p <- ggplot2::ggplot(
    hourly, ggplot2::aes(x = .data$hour, y = .data$mean_counts)
  ) +
    win_band(L5_start, L5_hours, "#123f60") +
    win_band(M10_start, M10_hours, "#8fb4d1")

  if (has_sd) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pmax(0, .data$mean_counts - .data$sd_counts),
                     ymax = .data$mean_counts + .data$sd_counts),
        fill = "#236192", alpha = 0.15
      )
  }

  p +
    ggplot2::geom_line(color = "#236192", linewidth = 1.2) +
    ggplot2::geom_point(color = "#236192", size = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3),
                                labels = sprintf("%02d:00", seq(0, 23, 3)),
                                expand = c(0.02, 0)) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
    ggplot2::labs(title = title, x = NULL, y = value_label) +
    .circ_theme() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#e2e8f0", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#64748b"),
      axis.title = ggplot2::element_text(color = "#1a202c")
    )
}


#' Plot the Fitted Cosinor Curve on the 24-Hour Activity Profile
#'
#' Overlays a fitted single-component cosinor curve on the averaged 24-hour
#' activity profile of one recording, with the MESOR as a dashed reference line
#' and the observed hourly means as points. The fit parameters are taken as
#' plain numbers rather than refitted, so the tab and the figure workbench draw
#' the same curve from the same run.
#'
#' @param hourly_profile A data frame with the columns \code{hour} (0-23) and
#'   \code{mean_counts}, such as the \code{hourly_profile} element of
#'   \code{\link{circadian.rhythm}}. Hours with \code{NA} means are not drawn.
#' @param mesor Numeric MESOR of the cosinor fit, e.g. the \code{mesor} element
#'   of \code{\link{cosinor.extended}} or \code{\link{cosinor.analysis}}.
#' @param amplitude Numeric amplitude of the cosinor fit, in the units of
#'   \code{mean_counts}.
#' @param acrophase Numeric acrophase (time of the fitted peak) in decimal hours.
#' @param r_squared Numeric model fit reported in the subtitle.
#' @param acrophase_time Optional clock-time label for the acrophase, e.g.
#'   \code{"14:30"}. When \code{NULL}, \code{NA} or empty, the decimal hour is
#'   printed instead.
#' @param title Optional plot title. \code{NULL} (the default) draws no title.
#' @param value_label Y axis label.
#'
#' @return A \code{ggplot} object. On missing profile data or non-finite fit
#'   parameters an annotated empty \code{ggplot} is returned instead; the
#'   function never errors.
#'
#' @details
#' The curve is evaluated on a 0-24 h grid in steps of 0.1 h as
#' \eqn{M + A \cos(2\pi t / 24 - \phi)}, where \eqn{\phi} is the acrophase
#' converted from hours to radians.
#'
#' @references
#' Cornelissen G (2014). Cosinor-based rhythmometry. \emph{Theoretical Biology
#' and Medical Modelling}, 11:16.
#'
#' Nelson W, Tong YL, Lee JK, Halberg F (1979). Methods for cosinor-rhythmometry.
#' \emph{Chronobiologia}, 6(4):305-323.
#'
#' @seealso \code{\link{cosinor.extended}}, \code{\link{cosinor.analysis}},
#'   \code{\link{plot_extended_cosinor}}, \code{\link{plot_cosinor_ellipse}}
#'
#' @examples
#' prof <- data.frame(
#'   hour = 0:23,
#'   mean_counts = 200 + 150 * cos(2 * pi * (0:23 - 14) / 24)
#' )
#' plot_cosinor_fit(prof, mesor = 200, amplitude = 150, acrophase = 14,
#'                  r_squared = 0.82, acrophase_time = "14:00")
#'
#' @export
plot_cosinor_fit <- function(hourly_profile, mesor, amplitude, acrophase,
                             r_squared = NA_real_, acrophase_time = NULL,
                             title = NULL,
                             value_label = "Activity (counts/min)") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_cosinor_fit().")
  }

  insufficient <- function(message) .circ_empty_plot(message, title = title)

  one_num <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) < 1L) NA_real_ else v[1]
  }

  if (missing(hourly_profile) || is.null(hourly_profile) ||
      !is.data.frame(hourly_profile) || nrow(hourly_profile) == 0L ||
      !all(c("hour", "mean_counts") %in% names(hourly_profile))) {
    return(insufficient("Hourly profile data not available"))
  }

  if (missing(mesor) || missing(amplitude) || missing(acrophase)) {
    return(insufficient("Cosinor fit parameters not available"))
  }
  mesor <- one_num(mesor)
  amplitude <- one_num(amplitude)
  acrophase <- one_num(acrophase)
  r_squared <- one_num(r_squared)

  if (!is.finite(mesor)) {
    return(insufficient("Cosinor analysis failed - MESOR could not be calculated"))
  }
  if (!is.finite(amplitude)) {
    return(insufficient("Cosinor analysis failed - amplitude could not be calculated"))
  }
  if (!is.finite(acrophase)) {
    return(insufficient("Cosinor analysis failed - acrophase could not be calculated"))
  }

  # fitted curve on a 0.1 h grid
  hours_fine <- seq(0, 24, by = 0.1)
  acro_rad <- (acrophase / 24) * 2 * pi
  fit_df <- data.frame(
    hour = hours_fine,
    fitted = mesor + amplitude * cos(2 * pi * hours_fine / 24 - acro_rad)
  )

  # clock time when supplied, else decimal hours
  acro_label <- if (!is.null(acrophase_time) && length(acrophase_time) >= 1L &&
                    !is.na(acrophase_time[1]) &&
                    nzchar(as.character(acrophase_time[1]))) {
    as.character(acrophase_time[1])
  } else {
    sprintf("%.1fh", acrophase)
  }

  ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = mesor, linetype = "dashed",
      color = "#FFCD00", linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data = fit_df, ggplot2::aes(x = .data$hour, y = .data$fitted),
      color = "#236192", linewidth = 1.5
    ) +
    ggplot2::geom_point(
      data = hourly_profile,
      ggplot2::aes(x = .data$hour, y = .data$mean_counts),
      color = "#1a202c", fill = "#236192", shape = 21, size = 3, stroke = 0.8
    ) +
    ggplot2::annotate(
      "text", x = 23, y = mesor, label = "MESOR",
      hjust = 1, vjust = -0.5, color = "#FFCD00", fontface = "bold", size = 3.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 23, 3),
      labels = sprintf("%02d:00", seq(0, 23, 3)),
      expand = c(0.02, 0)
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = c(0.05, 0)) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = value_label,
      subtitle = sprintf("R-squared = %.3f | Acrophase = %s",
                         r_squared, acro_label)
    ) +
    .circ_theme() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#e2e8f0", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#64748b"),
      axis.title = ggplot2::element_text(color = "#1a202c"),
      plot.subtitle = ggplot2::element_text(color = "#64748b", size = 11)
    )
}


#' Sedentary Time-Accumulation Curve (Longest-First Lorenz Curve)
#'
#' Cumulative share of total sedentary time (y) accumulated by the longest
#' x percent of bouts, with the band between the curve and the equality diagonal
#' shaded and the bias-corrected Gini coefficient annotated. This is a different
#' figure from \code{\link{plot_bout_lorenz}}, which orders bouts shortest-first
#' and reports the Gini carried on the fragmentation object; both are kept.
#'
#' @param bouts Bout durations: a data frame with a \code{duration_min} column
#'   (several may be row-bound to pool subjects), a
#'   \code{\link{sedentary.fragmentation}} result list, or a numeric vector of
#'   durations in minutes.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#'
#' @return A \code{ggplot} object on fixed 0-100 square coordinates. With no
#'   usable bout durations an annotated empty \code{ggplot} is returned instead;
#'   the function never errors.
#'
#' @details
#' The Gini coefficient is computed on the ascending-sorted durations as
#' \code{(2 * sum(i * x_i) - (n + 1) * sum(x)) / (n * sum(x))} and multiplied by
#' \code{n / (n - 1)} when \code{n > 1}, the same estimator the analytic engine
#' reports.
#'
#' @references
#' Chastin SFM, Granat MH (2010). Methods for objective measure, quantification
#' and analysis of sedentary behaviour and inactivity. \emph{Gait & Posture},
#' 31(1):82-86.
#'
#' @seealso \code{\link{plot_bout_lorenz}}, \code{\link{plot_bout_histogram}},
#'   \code{\link{sedentary.fragmentation}}
#'
#' @examples
#' bouts <- data.frame(duration_min = c(1, 2, 2, 3, 5, 8, 12, 20, 45, 90))
#' plot_bout_accumulation(bouts)
#'
#' @export
plot_bout_accumulation <- function(bouts,
                                   title = "Sedentary time accumulation (Lorenz curve)",
                                   subtitle = "Shaded area represents inequality in bout durations") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_bout_accumulation().")
  }

  # a bouts frame, a fragmentation result, or plain durations
  if (missing(bouts) || is.null(bouts)) {
    dur <- numeric(0)
  } else if (is.data.frame(bouts)) {
    dur <- if (!is.null(bouts$duration_min)) bouts$duration_min else numeric(0)
  } else if (is.numeric(bouts)) {
    dur <- bouts
  } else if (is.list(bouts)) {
    b <- bouts$bouts
    dur <- if (is.data.frame(b) && !is.null(b$duration_min)) {
      b$duration_min
    } else {
      numeric(0)
    }
  } else {
    dur <- numeric(0)
  }

  dur <- suppressWarnings(as.numeric(dur))
  dur <- dur[is.finite(dur)]
  n <- length(dur)
  if (n == 0L || sum(dur) <= 0) {
    return(.circ_empty_plot("No bout data", title = title))
  }

  # accumulation, longest bout first
  dur_desc <- sort(dur, decreasing = TRUE)
  total_sed <- sum(dur_desc)
  df <- data.frame(
    bout_pct = seq_len(n) / n * 100,
    cum_pct = cumsum(dur_desc) / total_sed * 100
  )

  # Gini with the finite-sample bias correction
  x <- sort(dur)
  gini <- (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))
  if (n > 1) gini <- gini * n / (n - 1)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$bout_pct, y = .data$cum_pct)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$bout_pct, ymax = .data$cum_pct),
                         fill = "#236192", alpha = 0.2) +
    ggplot2::geom_line(color = "#236192", linewidth = 1.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#94a3b8") +
    ggplot2::annotate("text", x = 70, y = 30, label = paste("Gini =", round(gini, 3)),
                      size = 4, fontface = "bold", color = "#236192") +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "% of Bouts (longest first)", y = "% of Total Sedentary Time"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::coord_fixed() +
    .circ_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
      plot.subtitle = ggplot2::element_text(color = "#64748b")
    )
}


#' Sedentary Bouts by Duration Category
#'
#' Column chart of how many sedentary bouts fall in each duration category
#' (1-5, 5-10, 10-20, 20-30, 30-60 and >60 minutes), with the count and its
#' share of all bouts printed above each column. Counts are summed across every
#' distribution handed in.
#'
#' @param bout_distribution The \code{bout_distribution} data frame from
#'   \code{\link{sedentary.fragmentation}} (columns \code{category} and
#'   \code{count}), several of those row-bound together, a
#'   \code{sedentary.fragmentation} result list, or a list of such results.
#' @param title Plot title.
#'
#' @return A \code{ggplot} object. Never errors; returns an annotated empty
#'   plot when there is no distribution to draw.
#'
#' @seealso \code{\link{sedentary.fragmentation}},
#'   \code{\link{plot_bout_histogram}}
#'
#' @export
plot_bout_categories <- function(bout_distribution,
                                 title = "Bouts by duration category") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_bout_categories().")
  }

  empty <- function() .circ_empty_plot("No distribution data", title = title)

  if (missing(bout_distribution) || is.null(bout_distribution)) return(empty())

  # the data frame, a fragmentation result, or a list of either
  x <- bout_distribution
  if (!is.data.frame(x) && is.list(x)) {
    if (!is.null(x[["bout_distribution"]])) {
      x <- x[["bout_distribution"]]
    } else {
      x <- do.call(rbind, lapply(x, function(e) {
        if (is.data.frame(e)) {
          e
        } else if (is.list(e) && !is.null(e[["bout_distribution"]])) {
          e[["bout_distribution"]]
        } else {
          NULL
        }
      }))
    }
  }

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0 ||
      is.null(x$category) || is.null(x$count)) {
    return(empty())
  }

  agg_dist <- stats::aggregate(count ~ category, x, sum)
  if (nrow(agg_dist) == 0) return(empty())

  agg_dist$category <- factor(agg_dist$category,
                               levels = c("1-5 min", "5-10 min", "10-20 min",
                                         "20-30 min", "30-60 min", ">60 min"))
  total <- sum(agg_dist$count)
  if (!is.finite(total) || total <= 0) return(empty())
  agg_dist$pct <- round(agg_dist$count / total * 100, 1)

  colors <- c("1-5 min" = "#17a589", "5-10 min" = "#3a7ab0",
             "10-20 min" = "#236192", "20-30 min" = "#FFCD00",
             "30-60 min" = "#f4b942", ">60 min" = "#e6a000")

  ggplot2::ggplot(agg_dist, ggplot2::aes(x = .data$category, y = .data$count,
                                         fill = .data$category)) +
    ggplot2::geom_col(alpha = 0.9, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(.data$count, "\n(", .data$pct, "%)")),
                      vjust = -0.3, size = 3.5) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = title,
      subtitle = paste("Total:", total, "bouts"),
      x = NULL, y = "Number of Bouts"
    ) +
    .circ_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
      plot.subtitle = ggplot2::element_text(color = "#64748b"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


#' Pooled Sedentary Bout-Duration Histogram (dashboard figure)
#'
#' The bout-duration histogram as the Sedentary tab draws it: one histogram
#' over a pooled bout table, the median as a dashed line, the prolonged-bout
#' threshold as a dotted line with a labelled callout, and the pooled power-law
#' \code{alpha} in the subtitle. It is a separate function from
#' \code{\link{plot_bout_histogram}}, which takes a single
#' \code{\link{sedentary.fragmentation}} result and uses the package palette;
#' the two figures differ in colours, callout, subtitle and labels.
#'
#' @param bouts A data frame of sedentary bouts with a numeric
#'   \code{duration_min} column, typically the \code{$bouts} element of a
#'   \code{\link{sedentary.fragmentation}} result, or several such frames
#'   stacked with \code{rbind} to pool a cohort.
#' @param alpha Numeric power-law exponent printed in the subtitle; pass the
#'   pooled alpha when \code{bouts} pools several recordings.
#' @param prolonged_threshold Prolonged-bout threshold in minutes, drawn as a
#'   dotted vertical line with a label box.
#' @param title Plot title.
#' @param empty_message Text of the annotation used when there is no bout data.
#' @param empty_color Colour of that annotation.
#'
#' @return A \code{ggplot} object. Never errors on empty or missing input; an
#'   annotated blank plot is returned instead.
#'
#' @seealso \code{\link{plot_bout_histogram}} for the single-recording,
#'   package-palette variant.
#'
#' @export
plot_bout_histogram_pooled <- function(bouts,
                                       alpha = NA_real_,
                                       prolonged_threshold = 30,
                                       title = "Bout duration distribution",
                                       empty_message = "No bout data",
                                       empty_color = "black") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_bout_histogram_pooled().")
  }

  # the tab's no-data state is a theme_void() annotation, not .circ_empty_plot()
  .empty <- function(msg, col) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 5, hjust = 0.5, color = col) +
      ggplot2::theme_void()
  }

  if (missing(bouts) || is.null(bouts) || !is.data.frame(bouts) || nrow(bouts) == 0 ||
      is.null(bouts[["duration_min"]])) {
    return(.empty(empty_message, empty_color))
  }

  dur <- as.numeric(bouts[["duration_min"]])
  if (length(dur) == 0) return(.empty(empty_message, empty_color))

  med <- stats::median(dur)

  a <- if (is.null(alpha) || length(alpha) == 0) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(alpha)[1])
  }

  thr <- if (is.null(prolonged_threshold) || length(prolonged_threshold) == 0) {
    30
  } else {
    suppressWarnings(as.numeric(prolonged_threshold)[1])
  }
  if (is.na(thr)) thr <- 30

  ggplot2::ggplot(data.frame(duration_min = dur),
                  ggplot2::aes(x = .data$duration_min)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "#236192", alpha = 0.8,
                            color = "white") +
    ggplot2::geom_vline(xintercept = med, linetype = "dashed",
                        color = "#FFCD00", linewidth = 1) +
    ggplot2::geom_vline(xintercept = thr, linetype = "dotted",
                        color = "#f4b942", linewidth = 1) +
    ggplot2::annotate("label", x = thr, y = Inf,
                      label = paste0(thr, " min threshold"),
                      vjust = 1.5, size = 3, fill = "#fff8e1") +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Alpha = %.2f | Median = %.1f min | N = %d bouts",
                         a, med, length(dur)),
      x = "Duration (minutes)", y = "Count"
    ) +
    .circ_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
      plot.subtitle = ggplot2::element_text(color = "#64748b")
    )
}


#' Hourly Sedentary Bout Duration Distribution
#'
#' Boxplots of sedentary bout duration by the clock hour in which the bout
#' started, with a dashed reference line at the prolonged-bout threshold. All
#' 24 hours are kept on the x axis even when no bout started in them.
#'
#' @param bouts The \code{bouts} data frame from \code{\link{detect.sedentary.bouts}}
#'   or \code{\link{sedentary.fragmentation}} (columns \code{start_time} and
#'   \code{duration_min}), a \code{sedentary.fragmentation} result list, or a list
#'   of either, one element per participant, pooled.
#' @param threshold Prolonged-bout reference line in minutes.
#' @param title Plot title.
#' @param subtitle Plot subtitle. Defaults to a line naming \code{threshold}.
#'
#' @return A \code{ggplot} object. With no usable bouts an annotated empty
#'   \code{ggplot} is returned instead; the function never errors.
#'
#' @details
#' Each bout is placed in the clock hour of its \code{start_time}, so long bouts
#' are not split across the hours they span. The hour is taken per recording,
#' before pooling, so files in different time zones bin in their own local hour.
#'
#' @export
plot_hourly_bout_duration <- function(bouts,
                                      threshold = 30,
                                      title = "Hourly bout duration distribution",
                                      subtitle = sprintf(
                                        "Boxplot of bout durations by hour (dashed = %g min threshold)",
                                        threshold)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_hourly_bout_duration().")
  }
  if (missing(bouts) || is.null(bouts)) {
    return(.circ_empty_plot("No bout data", title = title))
  }

  frames <- if (is.data.frame(bouts)) {
    list(bouts)
  } else if (is.list(bouts) && is.data.frame(bouts$bouts)) {
    list(bouts$bouts)
  } else if (is.list(bouts)) {
    lapply(bouts, function(b) {
      if (is.data.frame(b)) {
        b
      } else if (is.list(b) && is.data.frame(b$bouts)) {
        b$bouts
      } else {
        NULL
      }
    })
  } else {
    list()
  }
  need <- c("start_time", "duration_min")
  frames <- Filter(
    function(b) is.data.frame(b) && nrow(b) > 0 && all(need %in% names(b)),
    frames
  )
  if (length(frames) == 0) return(.circ_empty_plot("No bout data", title = title))

  # hour per recording before pooling; rbind would coerce start_time to one zone
  all_bouts <- do.call(rbind, lapply(frames, function(b) {
    data.frame(
      hour = as.integer(format(b$start_time, "%H")),
      duration_min = as.numeric(b$duration_min)
    )
  }))
  all_bouts <- all_bouts[!is.na(all_bouts$hour) & !is.na(all_bouts$duration_min), ,
                         drop = FALSE]
  if (nrow(all_bouts) == 0) return(.circ_empty_plot("No bout data", title = title))

  ggplot2::ggplot(all_bouts, ggplot2::aes(x = factor(.data$hour, levels = 0:23),
                                          y = .data$duration_min)) +
    ggplot2::geom_boxplot(fill = "#236192", alpha = 0.6, outlier.alpha = 0.3) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", color = "#f4b942") +
    ggplot2::scale_x_discrete(breaks = as.character(seq(0, 23, 3)), drop = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Hour of Day", y = "Duration (minutes)"
    ) +
    .circ_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
      plot.subtitle = ggplot2::element_text(color = "#64748b")
    )
}


#' Hourly Sedentary Bout Frequency
#'
#' Bar chart of how many sedentary bouts start in each clock hour of the day,
#' with a loess trend line over the 24 bars. Hours in which no bout started are
#' filled in as zero.
#'
#' @param bouts The \code{bouts} data frame from \code{\link{detect.sedentary.bouts}}
#'   or \code{\link{sedentary.fragmentation}} (needs a \code{start_time} column), a
#'   \code{sedentary.fragmentation} result list, or a list of either, one element
#'   per participant, pooled.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param span Loess span for the trend line.
#'
#' @return A \code{ggplot} object. With no usable bouts an annotated empty
#'   \code{ggplot} is returned instead; the function never errors.
#'
#' @details
#' Each bout counts once, in the clock hour of its \code{start_time}. This is the
#' count analogue of \code{\link{hourly.fragmentation.pattern}}: when in the day
#' the person sits down, not how much of each hour is sedentary. The hour is taken
#' per recording, before pooling, so files in different time zones bin in their
#' own local hour.
#'
#' @export
plot_hourly_bout_frequency <- function(bouts,
                                       title = "Hourly bout frequency",
                                       subtitle = "Number of sedentary bouts starting each hour",
                                       span = 0.4) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_hourly_bout_frequency().")
  }
  if (missing(bouts) || is.null(bouts)) {
    return(.circ_empty_plot("No bout data", title = title))
  }

  frames <- if (is.data.frame(bouts)) {
    list(bouts)
  } else if (is.list(bouts) && is.data.frame(bouts$bouts)) {
    list(bouts$bouts)
  } else if (is.list(bouts)) {
    lapply(bouts, function(b) {
      if (is.data.frame(b)) {
        b
      } else if (is.list(b) && is.data.frame(b$bouts)) {
        b$bouts
      } else {
        NULL
      }
    })
  } else {
    list()
  }
  frames <- Filter(
    function(b) is.data.frame(b) && nrow(b) > 0 && "start_time" %in% names(b),
    frames
  )
  if (length(frames) == 0) return(.circ_empty_plot("No bout data", title = title))

  # hour per recording before pooling; rbind would coerce start_time to one zone
  all_bouts <- data.frame(
    hour = unlist(lapply(frames, function(b) as.integer(format(b$start_time, "%H"))))
  )
  all_bouts <- all_bouts[!is.na(all_bouts$hour), , drop = FALSE]
  if (nrow(all_bouts) == 0) return(.circ_empty_plot("No bout data", title = title))

  hourly_counts <- as.data.frame(table(all_bouts$hour))
  names(hourly_counts) <- c("hour", "count")
  hourly_counts$hour <- as.integer(as.character(hourly_counts$hour))

  # Fill missing hours
  all_hours <- data.frame(hour = 0:23)
  hourly_counts <- merge(all_hours, hourly_counts, by = "hour", all.x = TRUE)
  hourly_counts$count[is.na(hourly_counts$count)] <- 0

  ggplot2::ggplot(hourly_counts, ggplot2::aes(x = .data$hour, y = .data$count)) +
    ggplot2::geom_col(fill = "#3a7ab0", alpha = 0.8) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#FFCD00",
                         linewidth = 1.5, span = span) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Hour of Day", y = "Number of Bouts"
    ) +
    .circ_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
      plot.subtitle = ggplot2::element_text(color = "#64748b")
    )
}


#' Sedentary Day-by-Hour Heatmap
#'
#' Sedentary minutes per clock hour and calendar day as a tile heatmap, with the
#' fill scale centred on the median cell.
#'
#' @param bouts A sedentary-bout data frame with \code{start_time} (POSIXct) and
#'   \code{duration_min}, as in the \code{bouts} element of a
#'   \code{\link{sedentary.fragmentation}} result, or the whole result list.
#'   Bouts from several recordings may be row-bound together.
#' @param title Plot title (default \code{NULL}, no title).
#'
#' @return A \code{ggplot} object. Never errors; returns an annotated empty plot
#'   when there is no usable bout data.
#' @seealso \code{\link{plot_sedentary_timeline}},
#'   \code{\link{plot_sedentary_occurrence}}
#' @export
plot_sedentary_heatmap <- function(bouts, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_sedentary_heatmap().")
  }

  empty <- function() {
    .circ_empty_plot("No sedentary bout data available", title = title)
  }

  if (missing(bouts) || is.null(bouts)) return(empty())
  if (!is.data.frame(bouts) && is.list(bouts) && !is.null(bouts$bouts)) {
    bouts <- bouts$bouts
  }
  if (!is.data.frame(bouts) || nrow(bouts) == 0 ||
      is.null(bouts$start_time) || is.null(bouts$duration_min)) {
    return(empty())
  }

  all_bouts <- bouts
  all_bouts$hour <- as.integer(format(all_bouts$start_time, "%H"))
  all_bouts$date <- .clock_date(all_bouts$start_time)
  keep <- is.finite(all_bouts$hour) & !is.na(all_bouts$date) &
    is.finite(all_bouts$duration_min)
  all_bouts <- all_bouts[keep, , drop = FALSE]
  if (nrow(all_bouts) == 0) return(empty())

  heatmap_data <- stats::aggregate(duration_min ~ hour + date, all_bouts, sum)
  if (nrow(heatmap_data) == 0) return(empty())

  # dates as an ordered factor for the y axis
  heatmap_data$date_label <- .format_english(heatmap_data$date, "%b %d")
  heatmap_data$date_label <- factor(
    heatmap_data$date_label,
    levels = unique(heatmap_data$date_label[order(heatmap_data$date)])
  )

  ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(x = .data$hour, y = .data$date_label, fill = .data$duration_min)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5, width = 1, height = 1) +
    ggplot2::scale_fill_gradient2(
      low = "#f8fafc", mid = "#3a7ab0", high = "#0f2d42",
      midpoint = stats::median(heatmap_data$duration_min, na.rm = TRUE),
      name = "Minutes"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3), expand = c(0, 0)) +
    ggplot2::labs(title = title, x = "Hour of Day", y = "Date") +
    .circ_theme() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10)
    )
}


#' Sedentary Bout Occurrence Scatter
#'
#' One point per sedentary bout at the clock time it started, one row per
#' calendar day, with the point sized and coloured by bout duration.
#'
#' @param bouts A sedentary-bout data frame with \code{start_time} (POSIXct) and
#'   \code{duration_min}, as in the \code{bouts} element of a
#'   \code{\link{sedentary.fragmentation}} result, or the whole result list.
#'   Bouts from several recordings may be row-bound together.
#' @param title Plot title (default \code{NULL}, no title).
#'
#' @return A \code{ggplot} object. Never errors; returns an annotated empty plot
#'   when there is no usable bout data.
#' @seealso \code{\link{plot_sedentary_timeline}},
#'   \code{\link{plot_sedentary_heatmap}}
#' @export
plot_sedentary_occurrence <- function(bouts, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_sedentary_occurrence().")
  }

  empty <- function() {
    .circ_empty_plot("No sedentary bout data available", title = title)
  }

  if (missing(bouts) || is.null(bouts)) return(empty())
  if (!is.data.frame(bouts) && is.list(bouts) && !is.null(bouts$bouts)) {
    bouts <- bouts$bouts
  }
  if (!is.data.frame(bouts) || nrow(bouts) == 0 ||
      is.null(bouts$start_time) || is.null(bouts$duration_min)) {
    return(empty())
  }

  all_bouts <- bouts
  all_bouts$date <- .clock_date(all_bouts$start_time)

  all_bouts$time_of_day <- as.numeric(format(all_bouts$start_time, "%H")) +
                           as.numeric(format(all_bouts$start_time, "%M")) / 60

  keep <- is.finite(all_bouts$time_of_day) & !is.na(all_bouts$date) &
    is.finite(all_bouts$duration_min)
  all_bouts <- all_bouts[keep, , drop = FALSE]
  if (nrow(all_bouts) == 0) return(empty())

  # dates as an ordered factor for the y axis
  all_bouts$date_label <- .format_english(all_bouts$date, "%b %d")
  all_bouts$date_label <- factor(
    all_bouts$date_label,
    levels = unique(all_bouts$date_label[order(all_bouts$date)])
  )

  ggplot2::ggplot(
    all_bouts,
    ggplot2::aes(x = .data$time_of_day, y = .data$date_label,
                 size = .data$duration_min, color = .data$duration_min)
  ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::scale_color_gradient2(low = "#17a589", mid = "#FFCD00", high = "#236192",
                                   midpoint = 30, name = "Duration\n(min)") +
    ggplot2::scale_size_continuous(range = c(1, 8), guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq(0, 24, 4),
                                labels = paste0(seq(0, 24, 4), ":00"),
                                limits = c(0, 24)) +
    ggplot2::labs(title = title, x = "Time of Day", y = "Date") +
    .circ_theme() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
}


#' Hourly Sedentary Timeline
#'
#' Total sedentary minutes accumulated in each clock hour, drawn as a filled
#' area with one point per hour sized by how many bouts started in that hour and
#' coloured by their average bout duration. The morning (06:00-09:00) and
#' evening (17:00-21:00) bands are shaded.
#'
#' @param bouts A sedentary-bout data frame with \code{start_time} (POSIXct) and
#'   \code{duration_min}, as in the \code{bouts} element of a
#'   \code{\link{sedentary.fragmentation}} result, or the whole result list.
#'   Bouts from several recordings may be row-bound together.
#' @param title Plot title (default \code{NULL}, no title).
#'
#' @return A \code{ggplot} object. Never errors; returns an annotated empty plot
#'   when there is no usable bout data.
#' @seealso \code{\link{plot_sedentary_heatmap}},
#'   \code{\link{plot_sedentary_occurrence}}
#' @export
plot_sedentary_timeline <- function(bouts, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_sedentary_timeline().")
  }

  empty <- function() {
    .circ_empty_plot("No sedentary bout data available", title = title)
  }

  if (missing(bouts) || is.null(bouts)) return(empty())
  if (!is.data.frame(bouts) && is.list(bouts) && !is.null(bouts$bouts)) {
    bouts <- bouts$bouts
  }
  if (!is.data.frame(bouts) || nrow(bouts) == 0 ||
      is.null(bouts$start_time) || is.null(bouts$duration_min)) {
    return(empty())
  }

  all_bouts <- bouts
  all_bouts$hour <- as.integer(format(all_bouts$start_time, "%H"))
  keep <- is.finite(all_bouts$hour) & is.finite(all_bouts$duration_min)
  all_bouts <- all_bouts[keep, , drop = FALSE]
  if (nrow(all_bouts) == 0) return(empty())

  # hourly sedentary minutes
  hourly_data <- stats::aggregate(duration_min ~ hour, all_bouts, sum)
  hourly_counts <- table(all_bouts$hour)
  hourly_data$n_bouts <- as.numeric(hourly_counts[as.character(hourly_data$hour)])
  hourly_data$avg_duration <- hourly_data$duration_min / hourly_data$n_bouts

  # Add missing hours
  all_hours <- data.frame(hour = 0:23)
  hourly_data <- merge(all_hours, hourly_data, by = "hour", all.x = TRUE)
  hourly_data$duration_min[is.na(hourly_data$duration_min)] <- 0
  hourly_data$n_bouts[is.na(hourly_data$n_bouts)] <- 0
  hourly_data$avg_duration[is.na(hourly_data$avg_duration)] <- 0

  hourly_data$bout_category <- cut(
    hourly_data$avg_duration,
    breaks = c(-Inf, 10, 20, 30, Inf),
    labels = c("Short (<10)", "Moderate (10-20)", "Long (20-30)", "Prolonged (>30)")
  )

  ggplot2::ggplot(hourly_data, ggplot2::aes(x = .data$hour, y = .data$duration_min)) +
    ggplot2::geom_area(fill = "#236192", alpha = 0.3) +
    ggplot2::geom_line(color = "#236192", linewidth = 1.2) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$n_bouts, color = .data$avg_duration), alpha = 0.8
    ) +
    ggplot2::scale_color_gradient2(low = "#17a589", mid = "#FFCD00", high = "#236192",
                                   midpoint = 20, name = "Avg Bout\n(min)") +
    ggplot2::scale_size_continuous(name = "Bouts", range = c(2, 8)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 2),
                                labels = paste0(seq(0, 23, 2), ":00")) +
    ggplot2::annotate("rect", xmin = 6, xmax = 9, ymin = -Inf, ymax = Inf,
                      fill = "#FFCD00", alpha = 0.08) +
    ggplot2::annotate("rect", xmin = 17, xmax = 21, ymin = -Inf, ymax = Inf,
                      fill = "#3a7ab0", alpha = 0.08) +
    ggplot2::labs(
      title = title,
      x = "Hour of Day",
      y = "Total Sedentary Minutes"
    ) +
    .circ_theme() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "right",
      axis.title = ggplot2::element_text(color = "#64748b")
    )
}


#' Sedentary State-Transition Matrix
#'
#' The 2x2 per-epoch transition probabilities drawn as a labelled heat map:
#' ASTP (active to sedentary) and SATP (sedentary to active, the break rate),
#' with the two stay probabilities as their complements.
#'
#' @param fragmentation A \code{\link{sedentary.fragmentation}} result list, or
#'   any list carrying numeric \code{ASTP} and \code{SATP} elements.
#' @param title Plot title.
#'
#' @return A \code{ggplot} object. When either transition probability is missing
#'   or non-finite an annotated empty \code{ggplot} is returned instead; the
#'   function never errors.
#'
#' @export
plot_transition_matrix <- function(fragmentation,
                                   title = "State transition probabilities") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_transition_matrix().")
  }
  if (missing(fragmentation) || is.null(fragmentation) || !is.list(fragmentation)) {
    return(.circ_empty_plot("No transition data", title = title))
  }

  avg_astp <- suppressWarnings(as.numeric(fragmentation$ASTP)[1])
  avg_satp <- suppressWarnings(as.numeric(fragmentation$SATP)[1])
  if (!is.finite(avg_astp) || !is.finite(avg_satp)) {
    return(.circ_empty_plot("No transition data", title = title))
  }

  trans_data <- data.frame(
    from = c("Active", "Active", "Sedentary", "Sedentary"),
    to = c("Stay Active", "Go Sedentary", "Break (Get Up)", "Stay Sedentary"),
    prob = c(1 - avg_astp, avg_astp, avg_satp, 1 - avg_satp),
    type = c("stay", "change", "change", "stay"),
    stringsAsFactors = FALSE
  )
  trans_data$label <- sprintf("%.1f%%", trans_data$prob * 100)
  trans_data$from <- factor(trans_data$from, levels = c("Active", "Sedentary"))
  trans_data$to <- factor(
    trans_data$to,
    levels = c("Stay Active", "Go Sedentary", "Break (Get Up)", "Stay Sedentary")
  )

  ggplot2::ggplot(trans_data,
                  ggplot2::aes(x = .data$to, y = .data$from, fill = .data$prob)) +
    ggplot2::geom_tile(color = "white", linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 6, fontface = "bold",
                       color = ifelse(trans_data$prob > 0.5, "white", "#1a202c")) +
    ggplot2::scale_fill_gradient2(low = "#e8f5e9", mid = "#42a5f5", high = "#0d47a1",
                                  midpoint = 0.5, limits = c(0, 1),
                                  name = "Probability") +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("SATP = %.3f (breaks) | ASTP = %.3f (sitting down)",
                         avg_satp, avg_astp),
      x = "Transition To", y = "Current State"
    ) +
    .circ_theme() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#236192", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "#64748b", hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
}

