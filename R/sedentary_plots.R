#' @title Sedentary Fragmentation Visualizations
#'
#' @description
#' ggplot2 visualizations of a \code{\link{sedentary.fragmentation}} result: the
#' bout-duration histogram, the time-accumulation (Lorenz) curve, and the
#' state-transition matrix. Each takes the fragmentation result list, returns a
#' \code{ggplot}, and never errors on degenerate input (returns an annotated
#' empty plot instead).
#'
#' @name sedentary-plots
#' @references
#' Chastin SFM, Granat MH (2010). Methods for objective measure, quantification
#' and analysis of sedentary behaviour and inactivity. \emph{Gait & Posture},
#' 31(1):82-86.
#'
#' Wanigatunga AA, et al. (2019). Active-to-Sedentary Behavior Transitions,
#' Fatigability, and Physical Functioning in Older Adults. \emph{J Gerontol A}.
NULL


#' Sedentary Bout-Duration Histogram
#'
#' @param fragmentation A \code{\link{sedentary.fragmentation}} result list.
#' @param prolonged_threshold Numeric. Prolonged-bout reference line in minutes
#'   (default 30).
#' @param title Plot title.
#' @return A \code{ggplot} object.
#' @export
plot_bout_histogram <- function(fragmentation, prolonged_threshold = 30,
                                title = "Bout Duration Distribution") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_bout_histogram().")
  }
  dur <- if (!is.null(fragmentation$bouts)) fragmentation$bouts$duration_min else numeric(0)
  dur <- dur[is.finite(dur) & dur > 0]
  if (length(dur) == 0) return(.circ_empty_plot("No bout data", title = title))

  med <- stats::median(dur)
  alpha <- fragmentation$alpha
  alpha_txt <- if (is.null(alpha) || is.na(alpha)) "--" else sprintf("%.2f", alpha)

  ggplot2::ggplot(data.frame(duration_min = dur), ggplot2::aes(x = .data$duration_min)) +
    ggplot2::geom_histogram(binwidth = 5, fill = .circ_color("blue"), alpha = 0.85,
                            color = "white") +
    ggplot2::geom_vline(xintercept = med, linetype = "dashed",
                        color = .circ_color("orange"), linewidth = 1) +
    ggplot2::geom_vline(xintercept = prolonged_threshold, linetype = "dotted",
                        color = "grey50", linewidth = 0.8) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Alpha = %s | Median = %.1f min | %d bouts (dotted = %g min)",
                         alpha_txt, med, length(dur), prolonged_threshold),
      x = "Bout duration (minutes)", y = "Count"
    ) +
    .circ_theme()
}


#' Sedentary Time-Accumulation (Lorenz) Curve
#'
#' Cumulative share of total sedentary time (y) accumulated by the shortest
#' x percent of bouts. The dashed diagonal is perfect equality; the further the
#' curve bows below it, the more sedentary time is concentrated in a few long
#' bouts (higher Gini).
#'
#' @param fragmentation A \code{\link{sedentary.fragmentation}} result list.
#' @param title Plot title.
#' @return A \code{ggplot} object.
#' @export
plot_bout_lorenz <- function(fragmentation, title = "Sedentary Time Accumulation") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_bout_lorenz().")
  }
  dur <- if (!is.null(fragmentation$bouts)) fragmentation$bouts$duration_min else numeric(0)
  dur <- sort(dur[is.finite(dur) & dur > 0])
  n <- length(dur)
  if (n == 0 || sum(dur) == 0) return(.circ_empty_plot("No bout data", title = title))

  df <- data.frame(
    pct_bouts = c(0, seq_len(n) / n * 100),
    pct_time = c(0, cumsum(dur) / sum(dur) * 100)
  )
  gini <- fragmentation$gini
  gini_txt <- if (is.null(gini) || is.na(gini)) "--" else sprintf("%.3f", gini)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$pct_bouts, y = .data$pct_time)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
    ggplot2::geom_area(fill = .circ_color("blue"), alpha = 0.25) +
    ggplot2::geom_line(color = .circ_color("blue"), linewidth = 1) +
    ggplot2::coord_equal(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Gini = %s (inequality of bout durations)", gini_txt),
      x = "% of bouts (shortest first)", y = "% of sedentary time"
    ) +
    .circ_theme()
}


#' Sedentary State-Transition Matrix
#'
#' The 2x2 per-epoch transition probabilities: ASTP (active to sedentary) and
#' SATP (sedentary to active = the break rate), with stay probabilities on the
#' diagonal.
#'
#' @param fragmentation A \code{\link{sedentary.fragmentation}} result list.
#' @param title Plot title.
#' @return A \code{ggplot} object.
#' @export
plot_transition_matrix <- function(fragmentation, title = "State Transition Probabilities") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_transition_matrix().")
  }
  astp <- fragmentation$ASTP
  satp <- fragmentation$SATP
  if (is.null(astp) || is.na(astp) || is.null(satp) || is.na(satp)) {
    return(.circ_empty_plot("No transition data", title = title))
  }

  trans <- data.frame(
    from = factor(c("Active", "Active", "Sedentary", "Sedentary"),
                  levels = c("Active", "Sedentary")),
    to = factor(c("Stay Active", "Go Sedentary", "Break (Get Up)", "Stay Sedentary"),
                levels = c("Stay Active", "Go Sedentary", "Break (Get Up)", "Stay Sedentary")),
    prob = c(1 - astp, astp, satp, 1 - satp),
    stringsAsFactors = FALSE
  )
  trans$label <- sprintf("%.1f%%", trans$prob * 100)

  ggplot2::ggplot(trans, ggplot2::aes(x = .data$to, y = .data$from, fill = .data$prob)) +
    ggplot2::geom_tile(color = "white", linewidth = 2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 6, fontface = "bold",
                       color = ifelse(trans$prob > 0.5, "white", "#1a202c")) +
    ggplot2::scale_fill_gradient2(low = "#e8f5e9", mid = "#42a5f5", high = "#0d47a1",
                                  midpoint = 0.5, limits = c(0, 1), name = "Probability") +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("SATP = %.3f (breaks) | ASTP = %.3f (sitting down)", satp, astp),
      x = "Transition to", y = "Current state"
    ) +
    .circ_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 18, hjust = 0.7),
      legend.position = "right"
    )
}
