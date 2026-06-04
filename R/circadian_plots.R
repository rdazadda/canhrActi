#' @title Circadian Rhythm Visualizations
#'
#' @description
#' ggplot2 visualizations for the circadian / chronobiology metrics produced by
#' canhrActi. These functions are thin plotting wrappers around the existing
#' analytic engines and add no new fitting logic of their own:
#' \itemize{
#'   \item \code{\link{plot_periodogram}} draws the Lomb-Scargle periodogram and
#'     highlights the endogenous period estimated by
#'     \code{\link{circadian.period}}.
#'   \item \code{\link{plot_extended_cosinor}} overlays the Marler extended
#'     (anti-logistic) cosinor fit from \code{\link{cosinor.antilogistic}} and
#'     the ordinary cosinor from \code{\link{cosinor.analysis}} on the averaged
#'     24-hour activity profile.
#'   \item \code{\link{plot_dfa}} draws the detrended-fluctuation log-log scaling
#'     plot from \code{\link{fractal.dfa}}.
#' }
#'
#' Every function returns a \code{ggplot} object, never errors on degenerate or
#' insufficient input (it returns an annotated empty plot instead), and falls
#' back to \code{ggplot2::theme_minimal()} when the package theme helpers are
#' unavailable.
#'
#' @name circadian-plots
#'
#' @references
#' Lomb NR (1976). Least-squares frequency analysis of unequally spaced data.
#' \emph{Astrophysics and Space Science}, 39(2):447-462.
#'
#' Scargle JD (1982). Studies in astronomical time series analysis. II.
#' Statistical aspects of spectral analysis of unevenly spaced data.
#' \emph{The Astrophysical Journal}, 263:835-853.
#'
#' Marler MR, Gehrman P, Martin JL, Ancoli-Israel S (2006). The sigmoidally
#' transformed cosine curve: a mathematical model for circadian rhythms with
#' symmetric non-sinusoidal shapes. \emph{Statistics in Medicine},
#' 25(22):3893-3904.
#'
#' Peng CK, Buldyrev SV, Havlin S, Simons M, Stanley HE, Goldberger AL (1994).
#' Mosaic organization of DNA nucleotides. \emph{Physical Review E},
#' 49(2):1685-1689.
NULL


# ---------------------------------------------------------------------------
# Internal helper: resolve the package theme, falling back to theme_minimal().
# ---------------------------------------------------------------------------
.circ_theme <- function() {
  if (exists("theme_canhrActi", mode = "function")) {
    tryCatch(theme_canhrActi(), error = function(e) ggplot2::theme_minimal())
  } else {
    ggplot2::theme_minimal()
  }
}


# ---------------------------------------------------------------------------
# Internal helper: resolve the canhrActi accent colour, falling back to blue.
# ---------------------------------------------------------------------------
.circ_color <- function(name = "blue") {
  fallback <- "#236192"
  if (exists("canhrActi_palette", mode = "function")) {
    out <- tryCatch(
      {
        pal <- canhrActi_palette("primary")
        if (!is.null(pal[[name]])) pal[[name]] else fallback
      },
      error = function(e) fallback
    )
    return(out)
  }
  fallback
}


# ---------------------------------------------------------------------------
# Internal helper: a centred-annotation empty ggplot for the insufficient-data
# / non-convergence edge cases. Never errors.
# ---------------------------------------------------------------------------
.circ_empty_plot <- function(message, title = NULL) {
  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5, label = message,
      hjust = 0.5, vjust = 0.5, size = 5
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    .circ_theme() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  p
}


# ---------------------------------------------------------------------------
# Internal helper: averaged 24h activity profile on a 0-24 hour-of-day grid.
# Mirrors the hourly averaging used by cosinor.analysis(): mean activity per
# clock-hour bin across all days, bin centres at hour + 0.5. Returns a data
# frame with columns `hour` and `activity` (hours with no data are dropped).
# ---------------------------------------------------------------------------
.circ_hourly_profile <- function(counts, timestamps) {
  valid <- !is.na(counts) & !is.na(timestamps)
  if (!any(valid)) {
    return(data.frame(hour = numeric(0), activity = numeric(0)))
  }
  y <- as.numeric(counts[valid])
  ts <- timestamps[valid]
  hour_of_day <- as.numeric(format(ts, "%H")) +
    as.numeric(format(ts, "%M")) / 60
  hour_bin <- floor(hour_of_day)
  hourly_means <- tapply(y, hour_bin, mean, na.rm = TRUE)
  hours_present <- as.integer(names(hourly_means))
  data.frame(
    hour = hours_present + 0.5,
    activity = as.numeric(hourly_means)
  )
}


#' Plot the Lomb-Scargle Periodogram with the Endogenous Period
#'
#' Computes the full Lomb-Scargle periodogram of an activity series over a
#' period search window and plots spectral power against period (in hours). The
#' dominant endogenous period (\code{tau}) estimated by
#' \code{\link{circadian.period}} is marked with a labelled vertical line, and a
#' dashed reference line is drawn at 24 hours so the deviation of the biological
#' clock from exactly one solar day is visible at a glance.
#'
#' @param counts Numeric vector of activity counts (minute-level recommended).
#'   \code{NA} values (e.g. non-wear epochs) are dropped together with their
#'   timestamps before estimation.
#' @param timestamps A \code{POSIXct} vector (or anything coercible by
#'   \code{as.numeric}) of epoch timestamps, the same length as \code{counts}.
#' @param from Numeric. Lower bound of the period search window, in hours
#'   (default \code{18}).
#' @param to Numeric. Upper bound of the period search window, in hours
#'   (default \code{30}).
#' @param ofac Integer oversampling factor passed to \code{lomb::lsp}. Higher
#'   values give a finer period grid (default \code{4}).
#'
#' @return A \code{ggplot} object: Lomb-Scargle power (y) versus period in hours
#'   (x), with the peak period and the 24 h reference annotated. On insufficient
#'   data (all-\code{NA}, constant, or fewer than ~2 days of span) a
#'   \code{ggplot} carrying a centred "Insufficient data for periodogram"
#'   annotation is returned instead; the function never errors.
#'
#' @details
#' The full spectrum is obtained from
#' \code{lomb::lsp(x, times, from, to, type = "period", ofac, plot = FALSE)},
#' whose \code{$scanned} component holds the trial periods (hours) and
#' \code{$power} the corresponding normalized Lomb-Scargle power. The peak period
#' \code{tau} and its \code{p_value} come from \code{\link{circadian.period}} so
#' that the highlighted peak is exactly the value reported by the analytic
#' function. The Lomb-Scargle periodogram is the least-squares spectral estimator
#' for unevenly sampled series and is therefore appropriate for gappy actigraphy
#' data, which an FFT cannot accommodate.
#'
#' @references
#' Lomb NR (1976). Least-squares frequency analysis of unequally spaced data.
#' \emph{Astrophysics and Space Science}, 39(2):447-462.
#'
#' Scargle JD (1982). Studies in astronomical time series analysis. II.
#' Statistical aspects of spectral analysis of unevenly spaced data.
#' \emph{The Astrophysical Journal}, 263:835-853.
#'
#' Ruf T (1999). The Lomb-Scargle periodogram in biological rhythm research.
#' \emph{Biological Rhythm Research}, 30(2):178-201.
#'
#' @seealso \code{\link{circadian.period}}, \code{\link{plot_extended_cosinor}}
#'
#' @examples
#' \dontrun{
#' t_hours <- seq(0, 7 * 24 - 1 / 60, by = 1 / 60)
#' ts <- as.POSIXct("2024-01-01 00:00:00") + t_hours * 3600
#' counts <- 100 + 80 * cos(2 * pi * (t_hours - 8) / 24) +
#'   rnorm(length(t_hours), 0, 5)
#' plot_periodogram(counts, ts)
#' }
#'
#' @export
plot_periodogram <- function(counts, timestamps, from = 18, to = 30,
                             ofac = 4) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_periodogram().")
  }

  insufficient <- function() {
    .circ_empty_plot("Insufficient data for periodogram",
                     title = "Lomb-Scargle Periodogram")
  }

  # --- Basic structural validation -------------------------------------------
  if (missing(counts) || missing(timestamps) ||
      length(counts) == 0L || length(timestamps) == 0L ||
      length(counts) != length(timestamps)) {
    return(insufficient())
  }

  t_sec <- suppressWarnings(as.numeric(timestamps))
  cnt <- suppressWarnings(as.numeric(counts))

  keep <- is.finite(cnt) & is.finite(t_sec)
  cnt <- cnt[keep]
  t_sec <- t_sec[keep]

  if (length(cnt) < 10L) {
    return(insufficient())
  }

  t_hours <- (t_sec - min(t_sec)) / 3600
  span_days <- (max(t_hours) - min(t_hours)) / 24

  # Same guards as circadian.period(): need >= 2 days and a non-constant series.
  if (!is.finite(span_days) || span_days < 2) {
    return(insufficient())
  }
  if (stats::sd(cnt) == 0 || !is.finite(stats::sd(cnt))) {
    return(insufficient())
  }

  # --- Full Lomb-Scargle spectrum --------------------------------------------
  lsp <- tryCatch(
    .lomb_scargle(x = cnt, times = t_hours, from = from, to = to, ofac = ofac),
    error = function(e) NULL
  )
  if (is.null(lsp) || is.null(lsp$scanned) || is.null(lsp$power) ||
      length(lsp$scanned) < 2L) {
    return(insufficient())
  }

  spectrum <- data.frame(
    period = as.numeric(lsp$scanned),
    power = as.numeric(lsp$power)
  )

  # --- Endogenous period (tau) + p-value from the analytic engine ------------
  cp <- tryCatch(
    circadian.period(counts, timestamps, from = from, to = to, ofac = ofac),
    error = function(e) NULL
  )
  tau <- if (!is.null(cp) && is.finite(cp$tau)) cp$tau else NA_real_
  p_value <- if (!is.null(cp) && is.finite(cp$p_value)) cp$p_value else NA_real_

  accent <- .circ_color("blue")
  peak_col <- .circ_color("orange")

  ttl <- if (is.finite(tau)) {
    sprintf("Endogenous period: %.2f h (p = %.3g)", tau, p_value)
  } else {
    "Lomb-Scargle Periodogram"
  }

  p <- ggplot2::ggplot(
    spectrum, ggplot2::aes(x = .data$period, y = .data$power)
  ) +
    ggplot2::geom_line(color = accent, linewidth = 0.7)

  # 24 h reference line (dashed) when inside the search window.
  if (from <= 24 && to >= 24) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = 24, linetype = "dashed",
        color = "grey40", linewidth = 0.5
      )
  }

  # Peak period marker + label.
  if (is.finite(tau)) {
    y_top <- max(spectrum$power, na.rm = TRUE)
    p <- p +
      ggplot2::geom_vline(
        xintercept = tau, color = peak_col, linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text", x = tau, y = y_top,
        label = sprintf("tau = %.2f h", tau),
        hjust = -0.05, vjust = 1, color = peak_col, fontface = "bold"
      )
  }

  p +
    ggplot2::labs(
      title = ttl,
      x = "Period (hours)",
      y = "Lomb-Scargle power"
    ) +
    .circ_theme()
}


#' Double-Plotted Actogram
#'
#' Draws a classic double-plotted actogram: one row per calendar day, each row
#' showing 48 hours (the day itself on the left, the following day on the right)
#' so circadian phase can be traced down the diagonal. Activity is rendered as a
#' per-minute raster (darker = more active). Non-wear (via \code{wear_time}) and
#' missing time are left blank, and any skipped calendar days appear as empty
#' rows.
#'
#' @param counts Numeric vector of activity counts on a regular epoch grid.
#' @param timestamps A \code{POSIXct} vector the same length as \code{counts}.
#' @param epoch_length Epoch length in seconds. If \code{NULL} (default) it is
#'   inferred from the median spacing of \code{timestamps}.
#' @param wear_time Optional logical vector (\code{TRUE} = worn) the same length
#'   as \code{counts}; non-wear epochs are blanked.
#' @param double_plot Logical; draw the 48-hour double plot (default
#'   \code{TRUE}) or a single 24-hour plot.
#'
#' @return A \code{ggplot} object. Never errors; returns an annotated empty plot
#'   on insufficient data.
#' @export
plot_actogram <- function(counts, timestamps, epoch_length = NULL,
                          wear_time = NULL, double_plot = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_actogram().")
  }

  insufficient <- function() {
    .circ_empty_plot("Insufficient data for actogram", title = "Actogram")
  }

  if (missing(counts) || missing(timestamps) ||
      length(counts) == 0L || length(counts) != length(timestamps) ||
      !inherits(timestamps, "POSIXct")) {
    return(insufficient())
  }

  x <- suppressWarnings(as.numeric(counts))
  if (!is.null(wear_time)) {
    wt <- as.logical(wear_time)
    if (length(wt) == length(x)) x[!(wt %in% TRUE)] <- NA
  }
  if (sum(is.finite(x)) < 10L) return(insufficient())

  t_sec <- as.numeric(timestamps)
  if (is.null(epoch_length)) {
    d <- diff(sort(t_sec[is.finite(t_sec)]))
    d <- d[d > 0]
    epoch_length <- if (length(d)) stats::median(d) else 60
  }
  if (!is.finite(epoch_length) || epoch_length <= 0) epoch_length <- 60
  res_min <- max(1L, as.integer(round(epoch_length / 60)))
  bins_per_day <- 1440L %/% res_min
  if (bins_per_day < 2L) return(insufficient())

  # Calendar-day offset (full range keeps skipped days blank).
  day_date <- as.Date(format(timestamps, "%Y-%m-%d"))
  day0 <- min(day_date, na.rm = TRUE)
  day_idx <- as.integer(day_date - day0) + 1L
  n_days <- as.integer(max(day_idx, na.rm = TRUE))

  minute_of_day <- as.integer(format(timestamps, "%H")) * 60L +
    as.integer(format(timestamps, "%M"))
  bin_idx <- pmin((minute_of_day %/% res_min) + 1L, bins_per_day)

  ok <- is.finite(day_idx) & is.finite(bin_idx) & is.finite(x)
  if (!any(ok)) return(insufficient())
  act <- tapply(
    x[ok],
    list(factor(day_idx[ok], levels = seq_len(n_days)),
         factor(bin_idx[ok], levels = seq_len(bins_per_day))),
    mean, na.rm = TRUE
  )
  act <- matrix(as.numeric(act), nrow = n_days, ncol = bins_per_day)
  act[is.nan(act)] <- NA_real_

  # Right half of each row = the next day (double plot).
  total_bins <- if (isTRUE(double_plot)) 2L * bins_per_day else bins_per_day
  grid <- expand.grid(b = seq_len(total_bins), day = seq_len(n_days))
  grid$value <- NA_real_
  left <- grid$b <= bins_per_day
  grid$value[left] <- act[cbind(grid$day[left], grid$b[left])]
  if (isTRUE(double_plot)) {
    right <- !left
    nd <- grid$day[right] + 1L
    bb <- grid$b[right] - bins_per_day
    good <- nd <= n_days
    v <- rep(NA_real_, length(nd))
    v[good] <- act[cbind(nd[good], bb[good])]
    grid$value[right] <- v
  }
  grid$x_h <- (grid$b - 1L) * res_min / 60

  # Fill ceiling so one spike doesn't dominate.
  fin <- grid$value[is.finite(grid$value)]
  cap <- if (length(fin)) as.numeric(stats::quantile(fin, 0.98, na.rm = TRUE)) else 1
  if (!is.finite(cap) || cap <= 0) cap <- max(c(fin, 1), na.rm = TRUE)

  x_max <- if (isTRUE(double_plot)) 48 else 24
  brks <- seq(0, x_max, 6)
  day_labels <- format(day0 + seq_len(n_days) - 1L, "%a %m-%d")

  p <- ggplot2::ggplot(
    grid, ggplot2::aes(x = .data$x_h, y = .data$day, fill = .data$value)
  ) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_reverse(
      breaks = seq_len(n_days), labels = day_labels, expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      breaks = brks, labels = sprintf("%02d:00", brks %% 24)
    ) +
    ggplot2::scale_fill_gradient(
      low = "white", high = "grey10", na.value = "white",
      limits = c(0, cap), oob = scales::squish, name = "Activity"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), expand = FALSE) +
    ggplot2::labs(
      title = "Actogram",
      subtitle = if (isTRUE(double_plot)) "Double-plotted (48 h)" else "Single (24 h)",
      x = NULL, y = NULL
    ) +
    .circ_theme()

  if (isTRUE(double_plot)) {
    p <- p + ggplot2::geom_vline(
      xintercept = 24, color = "grey50", linewidth = 0.4
    )
  }
  p
}


#' Plot the Extended (Marler) Cosinor Fit on the 24-Hour Activity Profile
#'
#' Builds the averaged 24-hour activity profile and overlays two model fits for
#' comparison: the Marler (2006) anti-logistic extended cosinor from
#' \code{\link{cosinor.antilogistic}} (drawn as a bold solid curve) and the
#' ordinary single-component cosinor from \code{\link{cosinor.analysis}} (drawn
#' as a dashed curve). The acrophase (time of peak) is marked with a vertical
#' line and the extended-fit parameters are summarised in the subtitle.
#'
#' @param counts Numeric vector of activity counts (one value per epoch).
#' @param timestamps A \code{POSIXct} vector of timestamps, the same length as
#'   \code{counts}.
#' @param period Numeric period of the rhythm in hours passed to
#'   \code{cosinor.antilogistic()} (default \code{24}).
#'
#' @return A \code{ggplot} object: the averaged hourly profile (points and a
#'   light connecting line) over hour-of-day 0-24, with the extended and
#'   ordinary cosinor curves overlaid. If the extended fit does not converge the
#'   profile and ordinary cosinor are still drawn with a "did not converge" note;
#'   on insufficient data an annotated empty \code{ggplot} is returned. The
#'   function never errors.
#'
#' @details
#' The profile is the mean activity in each clock-hour bin averaged across all
#' recorded days (bin centres at \code{hour + 0.5}), matching the profile that
#' \code{cosinor.analysis()} and \code{cosinor.antilogistic()} fit internally.
#' The fitted curves are evaluated on a dense 0-24 h grid:
#' \itemize{
#'   \item Extended (Marler): \eqn{f(t) = minimum + amplitude \cdot
#'     expit(\beta (cos(2\pi (t - acrotime)/T) - \alpha))}, using the
#'     \code{minimum}, \code{amplitude}, \code{alpha}, \code{beta} and
#'     \code{acrotime} returned by \code{cosinor.antilogistic()}.
#'   \item Ordinary: \eqn{M + A cos(2\pi (t - acrophase)/T)}, using the
#'     \code{mesor}, \code{amplitude} and \code{acrophase} from
#'     \code{cosinor.analysis()}.
#' }
#' The subtitle reports the extended-fit MESOR, amplitude, the \code{alpha}
#' width-asymmetry and the \code{beta} steepness when the fit converged.
#'
#' @references
#' Marler MR, Gehrman P, Martin JL, Ancoli-Israel S (2006). The sigmoidally
#' transformed cosine curve: a mathematical model for circadian rhythms with
#' symmetric non-sinusoidal shapes. \emph{Statistics in Medicine},
#' 25(22):3893-3904.
#'
#' Cornelissen G (2014). Cosinor-based rhythmometry. \emph{Theoretical Biology
#' and Medical Modelling}, 11:16.
#'
#' @seealso \code{\link{cosinor.antilogistic}}, \code{\link{cosinor.analysis}}
#'
#' @examples
#' \dontrun{
#' ts <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440 * 7)
#' hour <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60
#' counts <- 50 + 300 * plogis(4 * (cos((hour - 14) * 2 * pi / 24) - 0.2)) +
#'   rnorm(length(ts), 0, 10)
#' plot_extended_cosinor(counts, ts)
#' }
#'
#' @export
plot_extended_cosinor <- function(counts, timestamps, period = 24) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_extended_cosinor().")
  }

  insufficient <- function() {
    .circ_empty_plot("Insufficient data for cosinor",
                     title = "Extended Cosinor")
  }

  if (missing(counts) || missing(timestamps) ||
      length(counts) == 0L || length(timestamps) == 0L ||
      length(counts) != length(timestamps)) {
    return(insufficient())
  }

  prof <- .circ_hourly_profile(counts, timestamps)
  # Need a reasonable number of distinct hours for a meaningful profile / fit.
  if (nrow(prof) < 12L || length(unique(prof$activity)) < 2L) {
    return(insufficient())
  }

  omega <- 2 * pi / period
  grid <- data.frame(hour = seq(0, 24, length.out = 481))

  # --- Ordinary cosinor (always overlaid for comparison) ---------------------
  oc <- tryCatch(
    cosinor.analysis(counts, timestamps, period = period),
    error = function(e) NULL
  )
  oc_ok <- !is.null(oc) && is.finite(oc$mesor) &&
    is.finite(oc$amplitude) && is.finite(oc$acrophase)
  if (oc_ok) {
    grid$ordinary <- oc$mesor +
      oc$amplitude * cos(omega * (grid$hour - oc$acrophase))
  }

  # --- Extended (Marler anti-logistic) cosinor -------------------------------
  ext <- tryCatch(
    cosinor.antilogistic(counts, timestamps, period = period),
    error = function(e) NULL
  )
  ext_ok <- !is.null(ext) && isTRUE(ext$converged) &&
    is.finite(ext$minimum) && is.finite(ext$amplitude) &&
    is.finite(ext$alpha) && is.finite(ext$beta) && is.finite(ext$acrotime)
  if (ext_ok) {
    ct <- cos((grid$hour - ext$acrotime) * omega)
    grid$extended <- ext$minimum +
      ext$amplitude * stats::plogis(ext$beta * (ct - ext$alpha))
  }

  accent <- .circ_color("blue")
  ext_col <- .circ_color("orange")
  ord_col <- .circ_color("green")

  # --- Assemble the plot -----------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = prof,
      ggplot2::aes(x = .data$hour, y = .data$activity),
      color = "grey60", linewidth = 0.4
    ) +
    ggplot2::geom_point(
      data = prof,
      ggplot2::aes(x = .data$hour, y = .data$activity),
      color = "grey45", size = 1.6
    )

  if (oc_ok) {
    p <- p +
      ggplot2::geom_line(
        data = grid,
        ggplot2::aes(x = .data$hour, y = .data$ordinary),
        color = ord_col, linetype = "dashed", linewidth = 0.8
      )
  }
  if (ext_ok) {
    p <- p +
      ggplot2::geom_line(
        data = grid,
        ggplot2::aes(x = .data$hour, y = .data$extended),
        color = ext_col, linewidth = 1.2
      )
  }

  # Acrophase marker: prefer the extended fit's acrotime, else ordinary.
  acro <- if (ext_ok) ext$acrotime else if (oc_ok) oc$acrophase else NA_real_
  if (is.finite(acro)) {
    acro_clock <- acro %% 24
    p <- p +
      ggplot2::geom_vline(
        xintercept = acro_clock, color = accent,
        linetype = "dotted", linewidth = 0.6
      )
  }

  subtitle <- if (ext_ok) {
    sprintf(
      "Marler fit: MESOR = %.1f, amplitude = %.1f, alpha = %.2f, beta = %.2f",
      ext$MESOR, ext$amplitude, ext$alpha, ext$beta
    )
  } else {
    "Extended-cosinor fit did not converge"
  }

  p +
    ggplot2::scale_x_continuous(breaks = seq(0, 24, 6), limits = c(0, 24)) +
    ggplot2::labs(
      title = "Extended (Marler) Cosinor",
      subtitle = subtitle,
      x = "Hour of day",
      y = "Activity"
    ) +
    .circ_theme()
}


#' Plot the Detrended Fluctuation Analysis (DFA) Scaling Relationship
#'
#' Runs \code{\link{fractal.dfa}} on an activity series and draws the
#' detrended-fluctuation log-log scaling plot: \code{log10(F(n))} against
#' \code{log10(n)} (window size). A regression line whose slope is the overall
#' scaling exponent \code{alpha} is overlaid, and when the analysis splits the
#' scales at a breakpoint the short- (\code{alpha1}) and long-timescale
#' (\code{alpha2}) segments are drawn separately. The exponents are annotated on
#' the plot and an interpretive guide appears in the subtitle.
#'
#' @param counts Numeric vector of activity counts (minute-level recommended).
#'   The longest continuous non-\code{NA} segment is analyzed internally by
#'   \code{fractal.dfa()}.
#'
#' @return A \code{ggplot} object: \code{log10(F(n))} (y) versus
#'   \code{log10(window size)} (x) as points with fitted scaling line(s) and
#'   \code{alpha}/\code{alpha1}/\code{alpha2} annotations. On an unusable series
#'   (too short, all-\code{NA}, or constant) an annotated empty \code{ggplot} is
#'   returned. The function never errors.
#'
#' @details
#' DFA quantifies long-range temporal correlations. The scaling exponent is the
#' slope of \code{log10(F(n))} regressed on \code{log10(n)}: \code{alpha ~ 0.5}
#' indicates uncorrelated (white) noise, \code{alpha ~ 1.0} indicates 1/f (pink)
#' noise, and \code{alpha ~ 1.5} indicates Brownian (random-walk) noise. The
#' \code{scales}, \code{fluctuations}, \code{alpha}, \code{alpha1},
#' \code{alpha2} and \code{breakpoint_min} fields returned by
#' \code{fractal.dfa()} drive the plot directly.
#'
#' @references
#' Peng CK, Buldyrev SV, Havlin S, Simons M, Stanley HE, Goldberger AL (1994).
#' Mosaic organization of DNA nucleotides. \emph{Physical Review E},
#' 49(2):1685-1689.
#'
#' Hu K, Van Someren EJW, Shea SA, Scheer FAJL (2009). Reduction of scale
#' invariance of activity fluctuations with aging and Alzheimer's disease.
#' \emph{PNAS}, 106(8):2490-2494.
#'
#' @seealso \code{\link{fractal.dfa}}, \code{\link{multiscale.entropy}}
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' plot_dfa(cumsum(rnorm(5000)))   # Brownian-like, alpha ~ 1.5
#' }
#'
#' @export
plot_dfa <- function(counts) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_dfa().")
  }

  insufficient <- function() {
    .circ_empty_plot("Insufficient data for DFA",
                     title = "Detrended Fluctuation Analysis")
  }

  if (missing(counts) || length(counts) == 0L) {
    return(insufficient())
  }

  dfa <- tryCatch(fractal.dfa(counts), error = function(e) NULL)
  if (is.null(dfa) || length(dfa$scales) < 2L ||
      length(dfa$fluctuations) < 2L ||
      !any(is.finite(dfa$fluctuations) & dfa$fluctuations > 0)) {
    return(insufficient())
  }

  ok <- is.finite(dfa$scales) & is.finite(dfa$fluctuations) &
    dfa$scales > 0 & dfa$fluctuations > 0
  if (sum(ok) < 2L) {
    return(insufficient())
  }

  pts <- data.frame(
    logn = log10(dfa$scales[ok]),
    logF = log10(dfa$fluctuations[ok]),
    scale = dfa$scales[ok]
  )

  bp <- dfa$breakpoint_min
  pts$segment <- ifelse(pts$scale < bp, "short", "long")

  accent <- .circ_color("blue")
  a1_col <- .circ_color("orange")
  a2_col <- .circ_color("green")

  p <- ggplot2::ggplot(
    pts, ggplot2::aes(x = .data$logn, y = .data$logF)
  ) +
    ggplot2::geom_point(color = accent, size = 2)

  # Overall fitted scaling line (slope = alpha).
  if (is.finite(dfa$alpha)) {
    fit <- stats::lm(logF ~ logn, data = pts)
    pts$fit_overall <- stats::predict(fit)
    p <- p +
      ggplot2::geom_line(
        data = pts,
        ggplot2::aes(x = .data$logn, y = .data$fit_overall),
        color = accent, linewidth = 0.8
      )
  }

  # Per-segment lines when the breakpoint produces two estimable slopes.
  has_split <- is.finite(dfa$alpha1) && is.finite(dfa$alpha2) &&
    sum(pts$segment == "short") >= 2L && sum(pts$segment == "long") >= 2L
  if (has_split) {
    seg_short <- pts[pts$segment == "short", ]
    seg_long <- pts[pts$segment == "long", ]
    fit_s <- stats::lm(logF ~ logn, data = seg_short)
    fit_l <- stats::lm(logF ~ logn, data = seg_long)
    seg_short$fit_seg <- stats::predict(fit_s)
    seg_long$fit_seg <- stats::predict(fit_l)
    p <- p +
      ggplot2::geom_line(
        data = seg_short,
        ggplot2::aes(x = .data$logn, y = .data$fit_seg),
        color = a1_col, linewidth = 1, linetype = "dashed"
      ) +
      ggplot2::geom_line(
        data = seg_long,
        ggplot2::aes(x = .data$logn, y = .data$fit_seg),
        color = a2_col, linewidth = 1, linetype = "dashed"
      )
  }

  # Alpha annotation (top-left).
  label <- sprintf("alpha == %.3f", dfa$alpha)
  if (has_split) {
    label <- sprintf(
      "alpha = %.3f\nalpha1 = %.3f (n < %g)\nalpha2 = %.3f (n >= %g)",
      dfa$alpha, dfa$alpha1, bp, dfa$alpha2, bp
    )
  } else {
    label <- sprintf("alpha = %.3f", dfa$alpha)
  }
  x_left <- min(pts$logn, na.rm = TRUE)
  y_top <- max(pts$logF, na.rm = TRUE)
  p <- p +
    ggplot2::annotate(
      "text", x = x_left, y = y_top, label = label,
      hjust = 0, vjust = 1, fontface = "bold", size = 4
    )

  p +
    ggplot2::labs(
      title = "Detrended Fluctuation Analysis",
      subtitle = "~0.5 uncorrelated, ~1.0 1/f, ~1.5 Brownian",
      x = "log10(window size)",
      y = "log10(F(n))"
    ) +
    .circ_theme()
}
