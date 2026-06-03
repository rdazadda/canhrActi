#' @title Endogenous Circadian Period Estimation (Lomb-Scargle)
#'
#' @description
#' Estimates the dominant (endogenous) circadian PERIOD (tau) of an activity
#' time series using the Lomb-Scargle periodogram. Unlike the classical Fast
#' Fourier Transform (FFT), the Lomb-Scargle method does not require evenly
#' sampled data, so it correctly handles the irregular and gappy sampling that
#' results from non-wear periods, dropped epochs, or mixed epoch lengths in
#' accelerometer recordings.
#'
#' @name circadian-period
NULL


#' Estimate Endogenous Circadian Period via the Lomb-Scargle Periodogram
#'
#' Computes the Lomb-Scargle periodogram of an activity \code{counts} series
#' sampled at the supplied \code{timestamps} and returns the period (tau, in
#' hours) of the strongest spectral peak within the search window
#' \code{[from, to]}. The Lomb-Scargle method (Lomb 1976; Scargle 1982) is the
#' least-squares-equivalent spectral estimator for unevenly sampled time series
#' and is therefore appropriate for actigraphy data containing gaps, which the
#' FFT cannot accommodate.
#'
#' @param counts Numeric vector of activity counts (minute-level recommended).
#'   \code{NA} values (e.g. non-wear epochs) are dropped together with their
#'   timestamps before estimation.
#' @param timestamps A \code{POSIXct} vector (or anything coercible by
#'   \code{as.numeric}) of epoch timestamps, the same length as \code{counts}.
#'   Internally converted to hours elapsed since the first timestamp.
#' @param from Numeric. Lower bound of the period search window, in hours
#'   (default \code{18}).
#' @param to Numeric. Upper bound of the period search window, in hours
#'   (default \code{30}).
#' @param ofac Integer oversampling factor passed to \code{lomb::lsp}. Higher
#'   values give a finer period grid and a more precise peak location at the
#'   cost of computation (default \code{4}).
#'
#' @return A named \code{list} with elements:
#'   \describe{
#'     \item{tau}{Numeric. Period (hours) of the strongest Lomb-Scargle peak in
#'       \code{[from, to]}, i.e. the estimated endogenous circadian period.
#'       \code{NA_real_} when the data are insufficient.}
#'     \item{peak_power}{Numeric. Normalized power of that peak (the
#'       Lomb-Scargle peak statistic). \code{NA_real_} when insufficient.}
#'     \item{p_value}{Numeric. P-value of the peak under the null hypothesis of
#'       Gaussian noise, as returned by \code{lomb::lsp} (\code{$p.value}).
#'       \code{NA_real_} when insufficient.}
#'     \item{oversampling}{The \code{ofac} oversampling factor used.}
#'     \item{n_used}{Integer. Number of non-\code{NA} observations actually
#'       passed to the periodogram (\code{NA_integer_} when not run).}
#'     \item{span_days}{Numeric. Total recording span in days (max minus min
#'       timestamp), used for the >= 2-day guard.}
#'   }
#'   On any edge case (too few points, too short a span, degenerate input, or an
#'   internal \code{lomb::lsp} failure) the function returns this same structure
#'   with \code{tau}, \code{peak_power} and \code{p_value} set to \code{NA};
#'   it never throws.
#'
#' @details
#' Processing steps:
#' \enumerate{
#'   \item Timestamps are converted to hours since the first sample
#'     (\code{t_hours = (as.numeric(timestamps) - min) / 3600}).
#'   \item Pairs with a missing \code{count} or a missing/non-finite time are
#'     dropped.
#'   \item Two guards are applied so the estimate is never based on
#'     insufficient data: the recording must span at least \strong{2 days}
#'     (otherwise a 18-30 h period cannot be resolved) and at least
#'     \strong{10 non-\code{NA}} observations must remain.
#'   \item \code{lomb::lsp(x = counts, times = t_hours, from = from, to = to,
#'     type = "period", ofac = ofac, plot = FALSE)} is evaluated. With
#'     \code{type = "period"} the returned \code{$peak.at[1]} is the peak period
#'     in hours (its second element is the corresponding frequency and is
#'     ignored here).
#' }
#'
#' The Lomb-Scargle periodogram is chosen specifically because actigraphy series
#' are rarely gap-free: the FFT assumes uniform sampling, whereas Lomb-Scargle
#' fits sinusoids by least squares at each trial frequency and is unbiased for
#' irregular sampling.
#'
#' @references
#' Lomb NR (1976). Least-squares frequency analysis of unequally spaced data.
#' \emph{Astrophysics and Space Science}, 39(2):447-462.
#'
#' Scargle JD (1982). Studies in astronomical time series analysis. II.
#' Statistical aspects of spectral analysis of unevenly spaced data.
#' \emph{The Astrophysical Journal}, 263:835-853.
#'
#' Ruf T (1999). The Lomb-Scargle periodogram in biological rhythm research:
#' analysis of incomplete and unequally spaced time-series.
#' \emph{Biological Rhythm Research}, 30(2):178-201.
#'
#' Refinetti R, Cornelissen G, Halberg F (2007). Procedures for numerical
#' analysis of circadian rhythms. \emph{Biological Rhythm Research},
#' 38(4):275-325.
#'
#' @seealso \code{\link{cosinor.analysis}} for parametric (fixed-period) rhythm
#'   estimation, \code{\link{circadian.rhythm}} for non-parametric L5/M10/IS/IV
#'   metrics.
#'
#' @examples
#' \dontrun{
#' # Seven days of minute-level data with a ~24 h rhythm
#' t_hours <- seq(0, 7 * 24 - 1/60, by = 1/60)
#' ts <- as.POSIXct("2024-01-01 00:00:00") + t_hours * 3600
#' counts <- 100 + 80 * cos(2 * pi * (t_hours - 8) / 24) + rnorm(length(t_hours), 0, 5)
#' circadian.period(counts, ts)$tau   # ~= 24.0
#' }
#'
#' @export
circadian.period <- function(counts, timestamps, from = 18, to = 30, ofac = 4) {

  # Structured "insufficient data" return so callers get a stable shape and the
  # function never errors on an edge case.
  na_result <- function(n_used = NA_integer_, span_days = NA_real_) {
    list(
      tau          = NA_real_,
      peak_power   = NA_real_,
      p_value      = NA_real_,
      oversampling = ofac,
      n_used       = n_used,
      span_days    = span_days
    )
  }

  # --- Basic structural validation (never error; return NA structure) ---------
  if (missing(counts) || missing(timestamps) ||
      length(counts) == 0L || length(timestamps) == 0L ||
      length(counts) != length(timestamps)) {
    return(na_result())
  }

  # Numeric time axis in hours since the first sample. as.numeric() works for
  # POSIXct (seconds since epoch) and for plain numeric seconds.
  t_sec <- suppressWarnings(as.numeric(timestamps))
  counts <- suppressWarnings(as.numeric(counts))

  # --- Drop NA / non-finite pairs in either counts or times -------------------
  keep <- is.finite(counts) & is.finite(t_sec)
  counts <- counts[keep]
  t_sec  <- t_sec[keep]

  n_used <- length(counts)
  if (n_used == 0L) {
    return(na_result(n_used = 0L))
  }

  # Hours elapsed since the first retained sample.
  t_hours <- (t_sec - min(t_sec)) / 3600

  span_days <- (max(t_hours) - min(t_hours)) / 24

  # --- Guards: require >= 2 days of span and >= 10 non-NA points --------------
  # A period in the 18-30 h band cannot be resolved from < 2 days of data, and
  # too few samples make the periodogram meaningless.
  if (!is.finite(span_days) || span_days < 2) {
    return(na_result(n_used = n_used, span_days = span_days))
  }
  if (n_used < 10L) {
    return(na_result(n_used = n_used, span_days = span_days))
  }

  # A degenerate (constant) series has no spectral peak; guard so lsp does not
  # divide by a zero variance.
  if (stats::sd(counts) == 0 || !is.finite(stats::sd(counts))) {
    return(na_result(n_used = n_used, span_days = span_days))
  }

  # --- Lomb-Scargle periodogram over the requested period window --------------
  # 'lomb' is an allowed runtime Import, so call it directly. Wrap in tryCatch so
  # any internal failure degrades gracefully to the NA structure.
  lsp <- tryCatch(
    lomb::lsp(
      x      = counts,
      times  = t_hours,
      from   = from,
      to     = to,
      type   = "period",
      ofac   = ofac,
      plot   = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(lsp)) {
    return(na_result(n_used = n_used, span_days = span_days))
  }

  # With type = "period", lsp$peak.at[1] is the peak PERIOD (hours); its second
  # element is the matching frequency, which we ignore.
  tau        <- suppressWarnings(as.numeric(lsp$peak.at[1]))
  peak_power <- suppressWarnings(as.numeric(lsp$peak))
  p_value    <- suppressWarnings(as.numeric(lsp$p.value))

  if (length(tau) == 0L || !is.finite(tau)) {
    return(na_result(n_used = n_used, span_days = span_days))
  }

  list(
    tau          = tau,
    peak_power   = if (length(peak_power) == 0L) NA_real_ else peak_power,
    p_value      = if (length(p_value) == 0L) NA_real_ else p_value,
    oversampling = ofac,
    n_used       = n_used,
    span_days    = span_days
  )
}
