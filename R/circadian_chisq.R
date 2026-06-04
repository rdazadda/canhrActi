#' Chi-square (Sokolove-Bushell) Periodogram
#'
#' Estimates the dominant period of an activity time series with the classic
#' Sokolove-Bushell (1978) chi-square periodogram, the periodogram most widely
#' used in chronobiology / actigraphy. It complements the Lomb-Scargle estimator
#' in \code{\link{circadian.period}}: where Lomb-Scargle is the least-squares
#' spectral method for unevenly sampled data, the chi-square periodogram is the
#' analysis-of-variance method for regularly sampled data and reports a formal
#' significance threshold.
#'
#' For each trial period \eqn{P} (an integer number of epochs) the series is
#' folded into \eqn{P} phase bins over \eqn{K = \lfloor N/P \rfloor} complete
#' cycles and the statistic
#' \deqn{Q_P = \frac{N \sum_{h=1}^{P}(\bar{A}_h - \bar{A})^2}{\sum_{i=1}^{N}(A_i - \bar{A})^2}}
#' is computed, where \eqn{\bar{A}_h} is the mean at phase \eqn{h} and
#' \eqn{\bar{A}} the grand mean over the \eqn{N = KP} retained points. Under the
#' null hypothesis of no rhythm at \eqn{P}, \eqn{Q_P} follows a chi-square
#' distribution with \eqn{P-1} degrees of freedom, giving the significance line
#' \eqn{\chi^2_{P-1,\,1-\alpha}}. The estimated period is the \eqn{P} maximising
#' \eqn{Q_P} within the search window.
#'
#' @param counts Numeric vector of activity counts on a regular epoch grid.
#'   \code{NA} values (e.g. non-wear) are handled by removal from the phase and
#'   grand means.
#' @param timestamps A \code{POSIXct} vector (or numeric seconds) the same length
#'   as \code{counts}; used to infer the epoch length.
#' @param from,to Period search window in hours (default \code{18} to \code{30}).
#' @param alpha Significance level for the chi-square threshold (default
#'   \code{0.05}).
#' @param epoch_length Epoch length in seconds. If \code{NULL} (default) it is
#'   inferred from the median spacing of \code{timestamps}.
#'
#' @return A named list with \code{period} (hours, the \eqn{Q_P} peak),
#'   \code{Qp_peak}, \code{p_value} (family-wise Sidak p-value of the peak across
#'   the scanned periods), \code{significant} (logical, \code{p_value < alpha}),
#'   \code{scanned} (trial periods in hours), \code{Qp} (the periodogram aligned
#'   to \code{scanned}), \code{critical} (the per-period chi-square threshold),
#'   \code{epoch_length} and \code{alpha}. On insufficient/degenerate data the
#'   same shape is returned with \code{period}/\code{Qp_peak} \code{NA} and
#'   empty vectors; the function never throws.
#'
#' @references
#' Sokolove PG, Bushell WN (1978). The chi square periodogram: its utility for
#' analysis of circadian rhythms. \emph{Journal of Theoretical Biology},
#' 72(1):131-160.
#'
#' Refinetti R, Cornelissen G, Halberg F (2007). Procedures for numerical
#' analysis of circadian rhythms. \emph{Biological Rhythm Research}, 38(4):275-325.
#'
#' @seealso \code{\link{circadian.period}} for the Lomb-Scargle estimator.
#' @export
chi.sq.periodogram <- function(counts, timestamps, from = 18, to = 30,
                               alpha = 0.05, epoch_length = NULL) {

  na_result <- function() {
    list(period = NA_real_, Qp_peak = NA_real_, p_value = NA_real_,
         significant = NA, scanned = numeric(0), Qp = numeric(0),
         critical = numeric(0), epoch_length = epoch_length, alpha = alpha)
  }

  if (missing(counts) || missing(timestamps) ||
      length(counts) == 0L || length(counts) != length(timestamps)) {
    return(na_result())
  }

  x <- suppressWarnings(as.numeric(counts))
  t_sec <- suppressWarnings(as.numeric(timestamps))

  if (is.null(epoch_length)) {
    d <- diff(sort(t_sec[is.finite(t_sec)]))
    d <- d[d > 0]
    epoch_length <- if (length(d)) stats::median(d) else NA_real_
  }
  if (!is.finite(epoch_length) || epoch_length <= 0) return(na_result())

  N <- length(x)
  sdx <- stats::sd(x, na.rm = TRUE)
  if (sum(is.finite(x)) < 10L || !is.finite(sdx) || sdx == 0) return(na_result())

  # Trial periods as integer epoch counts within the [from, to] hour window.
  P_lo <- ceiling(from * 3600 / epoch_length)
  P_hi <- floor(to * 3600 / epoch_length)
  Ps <- seq.int(max(2L, P_lo), P_hi)
  Ps <- Ps[Ps >= 2 & Ps <= N %/% 2]
  if (length(Ps) < 2L) return(na_result())

  qp <- vapply(Ps, function(P) {
    K <- N %/% P
    if (K < 2L) return(NA_real_)
    M <- K * P
    xm <- x[seq_len(M)]
    mat <- matrix(xm, nrow = K, ncol = P, byrow = TRUE)
    phase_means <- colMeans(mat, na.rm = TRUE)
    g <- mean(xm, na.rm = TRUE)
    n_used <- sum(is.finite(xm))
    den <- sum((xm - g)^2, na.rm = TRUE)
    if (!is.finite(den) || den == 0) return(NA_real_)
    # Q_P = SS_between / variance, with the between-phase SS = K*sum((Ah-A)^2)
    # (each phase mean averages K cycles), variance = total SS / N.
    K * n_used * sum((phase_means - g)^2, na.rm = TRUE) / den
  }, numeric(1))

  ok <- is.finite(qp)
  if (!any(ok)) return(na_result())
  Ps <- Ps[ok]
  qp <- qp[ok]

  imax <- which.max(qp)
  P_peak <- Ps[imax]
  Qp_peak <- qp[imax]
  df_peak <- P_peak - 1L

  # Family-wise (Sidak) p-value of the peak over the scanned periods, so a single
  # large periodogram value is not called significant purely from multiple testing.
  p_single <- stats::pchisq(Qp_peak, df_peak, lower.tail = FALSE)
  p_value <- 1 - (1 - p_single)^length(Ps)

  list(
    period      = P_peak * epoch_length / 3600,
    Qp_peak     = Qp_peak,
    p_value     = p_value,
    significant = p_value < alpha,
    scanned     = Ps * epoch_length / 3600,
    Qp          = qp,
    critical    = stats::qchisq(1 - alpha, Ps - 1L),
    epoch_length = epoch_length,
    alpha       = alpha
  )
}
