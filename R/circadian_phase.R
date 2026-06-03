# Phase-timing circadian metrics: the published Composite Phase Deviation,
# onset-timing confidence intervals, and multi-resolution interdaily stability.
# These complement the non-parametric and parametric metrics in circadian.R.

#' Composite Phase Deviation (Fischer & Roenneberg, 2016)
#'
#' Combines, for each day, the deviation of the phase marker from the
#' individual's own mean phase (precision) and from a reference phase
#' (accuracy) as \code{CPD = mean(sqrt(precision^2 + accuracy^2))}. When no
#' external reference phase is supplied, accuracy is taken relative to the
#' individual's own mean phase (accuracy = 0), so CPD reduces to the mean
#' absolute phase deviation - still a valid, published measure of phase
#' instability, and distinct from the circular SD reported by
#' \code{onset_timing_variability}.
#'
#' @param onset_hours Numeric vector of daily phase-marker onset times in
#'   decimal hours (e.g. daily L5 onsets).
#' @param reference_phase Optional reference phase in decimal hours (e.g. a
#'   scheduled/expected time, or a group mean). Default \code{NULL} uses the
#'   individual's own mean phase (accuracy term = 0).
#'
#' @return List with \code{CPD}, \code{precision} (mean absolute deviation from
#'   own mean phase, hours), \code{accuracy} (mean absolute deviation from the
#'   reference, hours), \code{reference_phase}, and \code{n_days}.
#'
#' @references Fischer D, Vetter C, Roenneberg T (2016). A novel method to
#'   visualise and quantify circadian misalignment. Scientific Reports 6:38601.
#'
#' @export
composite.phase.deviation <- function(onset_hours, reference_phase = NULL) {
  onset_hours <- onset_hours[!is.na(onset_hours)]
  out <- list(CPD = NA_real_, precision = NA_real_, accuracy = NA_real_,
              reference_phase = reference_phase, n_days = length(onset_hours))
  if (length(onset_hours) < 2) return(out)

  # Signed circular deviation between two clock times, wrapped to [-12, 12] h.
  circ_diff <- function(a, b) ((a - b + 12) %% 24) - 12

  # Individual mean phase via circular mean (handles midnight wraparound).
  rad <- onset_hours * 2 * pi / 24
  mean_phase <- (atan2(mean(sin(rad)), mean(cos(rad))) %% (2 * pi)) * 24 / (2 * pi)

  precision_i <- circ_diff(onset_hours, mean_phase)
  ref <- if (is.null(reference_phase)) mean_phase else reference_phase
  accuracy_i <- circ_diff(onset_hours, ref)

  out$CPD <- round(mean(sqrt(precision_i^2 + accuracy_i^2)), 3)
  out$precision <- round(mean(abs(precision_i)), 3)
  out$accuracy <- round(mean(abs(accuracy_i)), 3)
  out$reference_phase <- round(ref, 3)
  out
}

#' Confidence Intervals for L5/M10 Onset Timing
#'
#' Percentile bootstrap confidence interval for the mean daily onset time of a
#' circadian phase marker (e.g. L5 or M10), using circular resampling of the
#' per-day onsets.
#'
#' @param onset_hours Numeric vector of daily onset times in decimal hours.
#' @param level Confidence level (default 0.95).
#' @param n_boot Bootstrap replicates (default 2000).
#'
#' @return List with \code{mean_onset}, \code{ci_lower}, \code{ci_upper}
#'   (decimal hours) and \code{n_days}.
#' @export
circadian.onset.ci <- function(onset_hours, level = 0.95, n_boot = 2000) {
  onset_hours <- onset_hours[!is.na(onset_hours)]
  n <- length(onset_hours)
  out <- list(mean_onset = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, n_days = n)
  if (n < 3) return(out)

  circ_mean <- function(h) {
    rad <- h * 2 * pi / 24
    (atan2(mean(sin(rad)), mean(cos(rad))) %% (2 * pi)) * 24 / (2 * pi)
  }
  out$mean_onset <- round(circ_mean(onset_hours), 3)

  boot <- vapply(seq_len(n_boot), function(i) {
    circ_mean(sample(onset_hours, n, replace = TRUE))
  }, numeric(1))
  # Centre on the point estimate before taking quantiles so wraparound near
  # midnight does not split the bootstrap distribution.
  centred <- ((boot - out$mean_onset + 12) %% 24) - 12
  a <- (1 - level) / 2
  q <- stats::quantile(centred, c(a, 1 - a), names = FALSE, na.rm = TRUE)
  out$ci_lower <- round((out$mean_onset + q[1]) %% 24, 3)
  out$ci_upper <- round((out$mean_onset + q[2]) %% 24, 3)
  out
}

#' Multi-resolution Interdaily Stability (IS)
#'
#' Computes interdaily stability at several within-day bin resolutions. IS is
#' classically reported at 60-min bins, but finer bins (30/15 min) capture
#' higher-frequency day-to-day regularity. For each bin width,
#' \code{IS = (N * sum_h (xbar_h - xbar)^2) / (p * sum_i (x_i - xbar)^2)} where
#' \code{p} is bins-per-day, \code{xbar_h} the mean for bin-of-day \code{h},
#' and \code{N} the total number of bins.
#'
#' @param counts Numeric vector of epoch-level activity.
#' @param timestamps POSIXct timestamps (one per epoch).
#' @param bin_minutes Integer vector of bin widths in minutes (default
#'   \code{c(60, 30, 15)}).
#'
#' @return Data frame with columns \code{bin_minutes} and \code{IS}.
#' @references Witting W, et al. (1990). Biol Psychiatry 27(6):563-572.
#' @export
circadian.is.multiscale <- function(counts, timestamps, bin_minutes = c(60, 30, 15)) {
  if (length(counts) != length(timestamps)) {
    stop("counts and timestamps must have the same length")
  }
  lt <- as.POSIXlt(timestamps)
  sec_of_day <- lt$hour * 3600 + lt$min * 60 + lt$sec
  day_index <- as.integer(as.Date(timestamps) - min(as.Date(timestamps)))

  is_at <- function(bin_min) {
    p <- 1440L %/% bin_min                 # bins per day
    bin_of_day <- (sec_of_day %/% (bin_min * 60)) + 1L
    cell <- interaction(day_index, bin_of_day, drop = TRUE)
    x <- tapply(counts, cell, mean, na.rm = TRUE)  # per (day,bin) mean
    x <- x[!is.na(x)]
    if (length(x) < p + 1) return(NA_real_)
    # bin-of-day for each retained cell
    bod <- as.integer(sub(".*\\.", "", names(x)))
    grand <- mean(x)
    xbar_h <- tapply(x, bod, mean)
    n_total <- length(x)
    num <- n_total * sum((xbar_h - grand)^2)
    den <- p * sum((x - grand)^2)
    if (den == 0) return(NA_real_)
    round(min(num / den, 1), 4)
  }

  data.frame(
    bin_minutes = bin_minutes,
    IS = vapply(bin_minutes, is_at, numeric(1))
  )
}
