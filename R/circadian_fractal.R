#' @title Fractal and Complexity Metrics for Activity Time Series
#'
#' @description
#' Hand-coded, dependency-light implementations of two nonlinear complexity /
#' scaling metrics for accelerometer activity time series:
#' Detrended Fluctuation Analysis (DFA) and Multiscale Sample Entropy (MSE).
#' Both are implemented in base R (only \pkg{stats} is used) so they add no new
#' runtime dependency. They quantify the temporal structure (long-range
#' correlations and information-theoretic complexity) of minute-level activity
#' counts, which complement the amplitude- and timing-based circadian metrics in
#' \code{\link{circadian.rhythm}}.
#'
#' @name circadian-fractal
#'
#' @references
#' \strong{Detrended Fluctuation Analysis:}
#' \itemize{
#'   \item Peng CK, Buldyrev SV, Havlin S, Simons M, Stanley HE, Goldberger AL
#'     (1994). Mosaic organization of DNA nucleotides. Physical Review E,
#'     49(2):1685-1689.
#'   \item Hu K, Ivanov PC, Chen Z, Carpena P, Stanley HE (2001). Effect of trends
#'     on detrended fluctuation analysis. Physical Review E, 64(1):011114.
#'   \item Hu K, Van Someren EJW, Shea SA, Scheer FAJL (2009). Reduction of scale
#'     invariance of activity fluctuations with aging and Alzheimer's disease.
#'     PNAS, 106(8):2490-2494.
#' }
#'
#' \strong{Multiscale Sample Entropy:}
#' \itemize{
#'   \item Richman JS, Moorman JR (2000). Physiological time-series analysis using
#'     approximate entropy and sample entropy. American Journal of Physiology -
#'     Heart and Circulatory Physiology, 278(6):H2039-H2049.
#'   \item Costa M, Goldberger AL, Peng CK (2002). Multiscale entropy analysis of
#'     complex physiologic time series. Physical Review Letters, 89(6):068102.
#'   \item Costa M, Goldberger AL, Peng CK (2005). Multiscale entropy analysis of
#'     biological signals. Physical Review E, 71(2):021906.
#' }
NULL


# ---------------------------------------------------------------------------
# Internal helper: longest continuous non-NA run of a numeric vector
# ---------------------------------------------------------------------------
# Returns the longest contiguous stretch of x that contains no NA values.
# DFA and (optionally) MSE require a gap-free series; rather than imputing or
# erroring on gaps, we operate on the longest clean segment.
.longest_non_na_run <- function(x) {
  x <- as.numeric(x)
  ok <- !is.na(x)
  if (!any(ok)) {
    return(numeric(0))
  }
  # Encode runs of the logical 'ok' vector.
  r <- rle(ok)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  good <- which(r$values)
  if (length(good) == 0L) {
    return(numeric(0))
  }
  best <- good[which.max(r$lengths[good])]
  x[starts[best]:ends[best]]
}


#' Detrended Fluctuation Analysis (DFA)
#'
#' Computes the scaling exponent alpha of an activity time series using
#' Detrended Fluctuation Analysis (Peng et al., 1994). DFA quantifies long-range
#' temporal correlations: alpha approximately 0.5 indicates uncorrelated (white)
#' noise, alpha approximately 1.0 indicates 1/f (pink) noise, and alpha
#' approximately 1.5 indicates Brownian (random-walk / brown) noise. Healthy
#' human activity fluctuations typically show alpha in the 0.9-1.0 range, with
#' reductions reported in aging and Alzheimer's disease (Hu et al., 2009).
#'
#' @param x Numeric vector of activity counts (minute-level recommended).
#'   Internally analyzed on the longest continuous non-NA segment.
#' @param scale_min Integer. Smallest window size (box length) in samples.
#'   Must be >= 4 so that a line can be detrended with residual degrees of
#'   freedom. Default 4.
#' @param scale_max Integer or NULL. Largest window size in samples. If NULL
#'   (default) it is set to floor(N / 4) where N is the length of the analyzed
#'   segment, ensuring at least four windows at the largest scale.
#' @param breakpoint_min Numeric. Window-size boundary (in samples / minutes for
#'   minute-level data) separating the short-timescale exponent \code{alpha1}
#'   (scales < breakpoint_min) from the long-timescale exponent \code{alpha2}
#'   (scales >= breakpoint_min). Default 90.
#'
#' @return A list with class \code{"canhrActi_dfa"} containing:
#'   \describe{
#'     \item{alpha}{Overall scaling exponent: slope of
#'       \code{lm(log10(F) ~ log10(n))} across all scales.}
#'     \item{alpha1}{Short-timescale exponent (scales < \code{breakpoint_min}).
#'       NA if fewer than two qualifying scales.}
#'     \item{alpha2}{Long-timescale exponent (scales >= \code{breakpoint_min}).
#'       NA if fewer than two qualifying scales.}
#'     \item{scales}{Integer vector of window sizes n that were used.}
#'     \item{fluctuations}{Numeric vector of fluctuation magnitudes F(n)
#'       corresponding to \code{scales}.}
#'     \item{n_used}{Length of the analyzed (longest non-NA) segment.}
#'     \item{breakpoint_min}{The breakpoint value used.}
#'   }
#'   On an unusable series (too short, all NA, or constant) the numeric scaling
#'   outputs are returned as NA with the same structure (never an error).
#'
#' @details
#' Algorithm (integrated, non-overlapping, linear DFA):
#' \enumerate{
#'   \item Extract the longest continuous non-NA segment of \code{x}.
#'   \item Integrate the mean-centred signal: \code{y = cumsum(x - mean(x))}.
#'   \item For each window size n (log-spaced from \code{scale_min} to
#'     \code{scale_max}), split y into floor(N / n) non-overlapping windows of
#'     length n, fit and remove a least-squares line within each window, and
#'     pool the residuals. The fluctuation is
#'     \code{F(n) = sqrt(mean(residuals^2))} over all pooled residuals.
#'   \item The scaling exponent is the slope of
#'     \code{log10(F(n))} regressed on \code{log10(n)}.
#' }
#' Window sizes are unique integers chosen on a base-10 log grid, which gives
#' approximately even spacing in the log-log fit and matches the convention used
#' by reference implementations such as \pkg{nonlinearTseries}.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' # White noise -> alpha ~ 0.5
#' fractal.dfa(rnorm(10000))$alpha
#' # Brown noise -> alpha ~ 1.5
#' fractal.dfa(cumsum(rnorm(10000)))$alpha
#' }
#'
#' @seealso \code{\link{multiscale.entropy}}, \code{\link{circadian.rhythm}}
#'
#' @export
fractal.dfa <- function(x, scale_min = 4, scale_max = NULL,
                        breakpoint_min = 90) {

  # --- Graceful empty/edge result builder -----------------------------------
  empty_result <- function(n_used = 0L) {
    structure(
      list(
        alpha = NA_real_,
        alpha1 = NA_real_,
        alpha2 = NA_real_,
        scales = integer(0),
        fluctuations = numeric(0),
        n_used = as.integer(n_used),
        breakpoint_min = breakpoint_min
      ),
      class = "canhrActi_dfa"
    )
  }

  scale_min <- max(4L, as.integer(round(scale_min)))

  # Work on the longest gap-free segment.
  seg <- .longest_non_na_run(x)
  N <- length(seg)
  if (N < scale_min * 4L) {
    # Need at least four windows at the smallest scale to be meaningful.
    return(empty_result(N))
  }

  # Constant series: no fluctuation structure, DFA undefined.
  if (stats::sd(seg) == 0 || !is.finite(stats::sd(seg))) {
    return(empty_result(N))
  }

  # Default largest scale: a quarter of the series (>= 4 windows).
  if (is.null(scale_max)) {
    scale_max <- as.integer(floor(N / 4L))
  } else {
    scale_max <- as.integer(round(scale_max))
  }
  scale_max <- min(scale_max, as.integer(floor(N / 4L)))
  if (scale_max < scale_min + 1L) {
    return(empty_result(N))
  }

  # --- Step 1: integrate the mean-centred profile ---------------------------
  y <- cumsum(seg - mean(seg))

  # --- Step 2: log-spaced unique integer window sizes -----------------------
  n_points <- max(8L, ceiling(log10(scale_max / scale_min) * 10) + 1L)
  raw <- 10^(seq(log10(scale_min), log10(scale_max), length.out = n_points))
  scales <- unique(as.integer(round(raw)))
  scales <- scales[scales >= scale_min & scales <= scale_max]
  if (length(scales) < 2L) {
    return(empty_result(N))
  }

  # --- Step 3: fluctuation function F(n) ------------------------------------
  # For each window we detrend with a least-squares line. We solve the local
  # regression directly (predictor 1..n is fixed across windows of a given n),
  # which is faster and avoids per-window lm() overhead.
  fluct <- vapply(scales, function(n) {
    n_win <- N %/% n
    if (n_win < 1L) {
      return(NA_real_)
    }
    used <- n_win * n
    # Reshape the first used points into an n x n_win matrix (column = window).
    ymat <- matrix(y[seq_len(used)], nrow = n, ncol = n_win)
    tt <- seq_len(n)
    # Least-squares line fit per column via centred normal equations.
    tbar <- mean(tt)
    tc <- tt - tbar                                  # centred predictor, sum(tc)=0
    denom <- sum(tc * tc)
    cmean <- colMeans(ymat)                          # per-window mean (intercept)
    # Because sum(tc) = 0, crossprod(tc, ymat)[, j] = sum(tc_i * y_ij), which is
    # exactly the numerator of the least-squares slope for window j.
    slope <- as.numeric(crossprod(tc, ymat)) / denom
    # Fitted values: cmean_j + slope_j * tc_i
    fitted <- outer(tc, slope) + matrix(cmean, nrow = n, ncol = n_win,
                                        byrow = TRUE)
    resid <- ymat - fitted
    sqrt(mean(resid^2))
  }, numeric(1))

  ok <- is.finite(fluct) & fluct > 0
  scales_ok <- scales[ok]
  fluct_ok <- fluct[ok]
  if (length(scales_ok) < 2L) {
    res <- empty_result(N)
    res$scales <- scales
    res$fluctuations <- fluct
    return(res)
  }

  logn <- log10(scales_ok)
  logF <- log10(fluct_ok)

  fit_slope <- function(lx, ly) {
    if (length(lx) < 2L) {
      return(NA_real_)
    }
    unname(stats::coef(stats::lm(ly ~ lx))[2L])
  }

  alpha <- fit_slope(logn, logF)

  lo <- scales_ok < breakpoint_min
  hi <- scales_ok >= breakpoint_min
  alpha1 <- fit_slope(logn[lo], logF[lo])
  alpha2 <- fit_slope(logn[hi], logF[hi])

  structure(
    list(
      alpha = alpha,
      alpha1 = alpha1,
      alpha2 = alpha2,
      scales = scales_ok,
      fluctuations = fluct_ok,
      n_used = N,
      breakpoint_min = breakpoint_min
    ),
    class = "canhrActi_dfa"
  )
}


# ---------------------------------------------------------------------------
# Internal helper: Richman-Moorman Sample Entropy
# ---------------------------------------------------------------------------
# SampEn(m, r) = -log(A / B) where
#   B = number of template pairs of length m that match within Chebyshev r,
#   A = number of template pairs of length m+1 that match within r,
# self-matches excluded, no distinction between A/B normalisation constants
# (they cancel in the ratio). Returns NA if no length-m matches (B == 0) or no
# length-(m+1) matches (A == 0), as the estimator is then undefined.
.sample_entropy <- function(x, m = 2L, r = 0.15) {
  x <- as.numeric(x)
  N <- length(x)
  m <- as.integer(m)
  if (N < (m + 2L) || !is.finite(r) || r <= 0) {
    return(NA_real_)
  }

  # Count matched template pairs of a given embedding dimension mm.
  count_matches <- function(mm) {
    n_templates <- N - mm + 1L
    if (n_templates < 2L) {
      return(0)
    }
    # Build embedding matrix: row i = x[i:(i+mm-1)].
    emb <- matrix(NA_real_, nrow = n_templates, ncol = mm)
    for (k in seq_len(mm)) {
      emb[, k] <- x[k:(k + n_templates - 1L)]
    }
    total <- 0
    # For each template i, count j > i within Chebyshev distance r.
    for (i in seq_len(n_templates - 1L)) {
      idx <- (i + 1L):n_templates
      # max absolute coordinate difference across the mm columns
      d <- abs(emb[idx, , drop = FALSE] -
                 matrix(emb[i, ], nrow = length(idx), ncol = mm, byrow = TRUE))
      cheb <- do.call(pmax, as.data.frame(d))
      total <- total + sum(cheb <= r)
    }
    total
  }

  B <- count_matches(m)
  if (B == 0) {
    return(NA_real_)
  }
  A <- count_matches(m + 1L)
  if (A == 0) {
    return(NA_real_)
  }
  -log(A / B)
}


# ---------------------------------------------------------------------------
# Internal helper: coarse-grain a series into non-overlapping means
# ---------------------------------------------------------------------------
.coarse_grain <- function(x, tau) {
  tau <- as.integer(tau)
  if (tau <= 1L) {
    return(as.numeric(x))
  }
  N <- length(x)
  n_out <- N %/% tau
  if (n_out < 1L) {
    return(numeric(0))
  }
  used <- n_out * tau
  m <- matrix(x[seq_len(used)], nrow = tau, ncol = n_out)
  colMeans(m)
}


#' Multiscale Sample Entropy (MSE)
#'
#' Computes Multiscale Sample Entropy (Costa et al., 2002, 2005), which applies
#' Sample Entropy (Richman & Moorman, 2000) to coarse-grained versions of an
#' activity time series across a range of temporal scales. MSE distinguishes
#' genuinely complex signals (whose entropy is sustained or increases across
#' scales, e.g. 1/f-like physiological signals) from uncorrelated random signals
#' (whose entropy falls monotonically with scale).
#'
#' @param x Numeric vector of activity counts (minute-level recommended).
#'   Internally analyzed on the longest continuous non-NA segment.
#' @param scales Integer vector of coarse-graining scale factors tau.
#'   Default \code{1:20}.
#' @param m Integer embedding dimension (template length) for Sample Entropy.
#'   Default 2.
#' @param r Numeric tolerance as a fraction of the standard deviation of the
#'   ORIGINAL (scale-1) series. The Chebyshev matching radius used at every scale
#'   is \code{r * sd(x_original)}, following the standard MSE convention of
#'   holding r fixed in absolute units across scales. Default 0.15.
#'
#' @return A list with class \code{"canhrActi_mse"} containing:
#'   \describe{
#'     \item{mse}{Numeric vector of SampEn values, one per requested scale
#'       (NA where the coarse-grained series is too short or yields no matches).}
#'     \item{scales}{Integer vector of the scale factors used.}
#'     \item{area}{Sum of \code{mse} over scales (complexity index),
#'       \code{sum(mse, na.rm = TRUE)}.}
#'     \item{slope}{Slope of \code{lm(mse ~ scales)} over the non-NA points
#'       (negative => entropy declines with scale, typical of noise).}
#'     \item{r_absolute}{The absolute tolerance \code{r * sd(x_original)} used.}
#'     \item{n_used}{Length of the analyzed (longest non-NA) segment.}
#'   }
#'   On an unusable series the entropy vector is all NA with the same structure
#'   (never an error).
#'
#' @details
#' For each scale tau the series is coarse-grained into non-overlapping means of
#' length tau, then Sample Entropy is computed on the coarse-grained series with
#' embedding dimension \code{m} and the FIXED absolute tolerance
#' \code{r * sd(x_original)}. Sample Entropy is the negative natural log of the
#' conditional probability that two sub-sequences matching for \code{m} points
#' (within the tolerance, Chebyshev distance, self-matches excluded) also match
#' for \code{m + 1} points. The implementation is fully self-contained (base R /
#' \pkg{stats} only).
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' # White noise: entropy decreases with scale
#' multiscale.entropy(rnorm(2000))$mse
#' # 1/f-like (random walk): flatter / sustained entropy
#' multiscale.entropy(cumsum(rnorm(2000)))$mse
#' }
#'
#' @seealso \code{\link{fractal.dfa}}, \code{\link{circadian.rhythm}}
#'
#' @export
multiscale.entropy <- function(x, scales = 1:20, m = 2, r = 0.15) {

  scales <- sort(unique(as.integer(round(scales))))
  scales <- scales[scales >= 1L]

  empty_result <- function(n_used = 0L) {
    structure(
      list(
        mse = rep(NA_real_, length(scales)),
        scales = scales,
        area = NA_real_,
        slope = NA_real_,
        r_absolute = NA_real_,
        n_used = as.integer(n_used)
      ),
      class = "canhrActi_mse"
    )
  }

  if (length(scales) == 0L) {
    return(empty_result(0L))
  }

  seg <- .longest_non_na_run(x)
  N <- length(seg)
  if (N < (as.integer(m) + 2L)) {
    return(empty_result(N))
  }

  sd_orig <- stats::sd(seg)
  if (!is.finite(sd_orig) || sd_orig == 0) {
    return(empty_result(N))
  }
  r_abs <- r * sd_orig

  mse <- vapply(scales, function(tau) {
    cg <- .coarse_grain(seg, tau)
    .sample_entropy(cg, m = m, r = r_abs)
  }, numeric(1))

  area <- sum(mse, na.rm = TRUE)

  ok <- is.finite(mse)
  slope <- NA_real_
  if (sum(ok) >= 2L) {
    slope <- unname(stats::coef(stats::lm(mse[ok] ~ scales[ok]))[2L])
  }

  structure(
    list(
      mse = mse,
      scales = scales,
      area = area,
      slope = slope,
      r_absolute = r_abs,
      n_used = N
    ),
    class = "canhrActi_mse"
  )
}


#' Print Method for DFA Results
#'
#' @param x Object of class \code{"canhrActi_dfa"}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.canhrActi_dfa <- function(x, ...) {
  cat("\nDetrended Fluctuation Analysis (Peng et al., 1994)\n\n")
  cat(sprintf("  Samples analyzed:   %d\n", x$n_used))
  cat(sprintf("  Window sizes:       %d (range %s)\n",
              length(x$scales),
              if (length(x$scales)) paste(range(x$scales), collapse = "-") else "NA"))
  cat(sprintf("  alpha  (overall):   %s\n", .fmt_num(x$alpha)))
  cat(sprintf("  alpha1 (n < %g):     %s\n", x$breakpoint_min, .fmt_num(x$alpha1)))
  cat(sprintf("  alpha2 (n >= %g):    %s\n", x$breakpoint_min, .fmt_num(x$alpha2)))
  cat("\n  Guide: ~0.5 uncorrelated, ~1.0 1/f, ~1.5 Brownian\n\n")
  invisible(x)
}


#' Print Method for Multiscale Entropy Results
#'
#' @param x Object of class \code{"canhrActi_mse"}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.canhrActi_mse <- function(x, ...) {
  cat("\nMultiscale Sample Entropy (Costa et al., 2002)\n\n")
  cat(sprintf("  Samples analyzed:   %d\n", x$n_used))
  cat(sprintf("  Scales:             %d (1-%d)\n",
              length(x$scales),
              if (length(x$scales)) max(x$scales) else 0L))
  cat(sprintf("  SampEn @ scale 1:   %s\n",
              .fmt_num(if (length(x$mse)) x$mse[1L] else NA_real_)))
  cat(sprintf("  Complexity (area):  %s\n", .fmt_num(x$area)))
  cat(sprintf("  Slope (mse ~ scale):%s\n", .fmt_num(x$slope)))
  cat("\n  Negative slope => noise-like; flat/positive => complex\n\n")
  invisible(x)
}


# Small numeric formatter used by the print methods.
.fmt_num <- function(v) {
  if (length(v) == 0L || is.na(v) || !is.finite(v)) {
    return("NA")
  }
  sprintf("%.4f", v)
}
