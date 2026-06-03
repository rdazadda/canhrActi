# =============================================================================
# Extended (anti-logistic) cosinor and cosinor utilities
#
# This file implements:
#   (1) cosinor.antilogistic()      - Marler et al. (2006) sigmoidally
#                                      transformed cosine fit on the averaged
#                                      24h activity profile (matches
#                                      ActCR::ActExtendCosinor numerically).
#   (2) cosinor.confidence.ellipse() - Bingham et al. (1982) joint
#                                      amplitude-acrophase confidence region.
#   (3) circadian.quotient()        - amplitude / mesor and relative amplitude.
#
# The math is hand-coded in base R (stats only) so the package gains no new
# runtime dependency.  The numeric conventions (averaged hourly profile,
# weighted least-squares cosinor, atan2 acrophase) mirror cosinor.analysis()
# in R/circadian.R so the two functions are mutually consistent.
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helper: build the averaged 24h activity profile.
#
# Mirrors the profile that cosinor.analysis() / .calculate.hourly.profile()
# build: for each clock hour 0-23 take the mean of all (non-NA) counts that
# fall in that hour across all days, together with the number of observations
# contributing (used as the weight).  Returned on a 0-23 hour grid; hours with
# no data are dropped (returned via the `present` index).
# -----------------------------------------------------------------------------
.ext.hourly.profile <- function(counts, timestamps) {
  valid <- !is.na(counts) & !is.na(timestamps)
  if (!any(valid)) {
    return(list(t = numeric(0), y = numeric(0), n = numeric(0)))
  }
  y <- counts[valid]
  ts <- timestamps[valid]

  hour_of_day <- as.numeric(format(ts, "%H")) +
    as.numeric(format(ts, "%M")) / 60
  hour_bin <- floor(hour_of_day)

  hourly_means <- tapply(y, hour_bin, mean, na.rm = TRUE)
  hourly_counts <- tapply(y, hour_bin, function(x) sum(!is.na(x)))

  hours_present <- as.integer(names(hourly_means))
  list(
    t = hours_present + 0.5,           # center of each hourly bin (clock hours)
    y = as.numeric(hourly_means),
    n = as.numeric(hourly_counts)
  )
}


# -----------------------------------------------------------------------------
# Internal helper: ordinary (single-component) cosinor on a profile.
# Weighted least squares with weights = sqrt(n), exactly as cosinor.analysis().
# Returns mesor, amplitude, acrophase (radians & hours) and the cos/sin betas.
# -----------------------------------------------------------------------------
.ext.ordinary.cosinor <- function(t, y, n, period = 24) {
  omega <- 2 * pi / period
  w <- sqrt(n)
  X <- cbind(1, cos(omega * t), sin(omega * t))
  Xw <- X * w
  yw <- y * w
  fit <- tryCatch(stats::lm.fit(Xw, yw), error = function(e) NULL)
  if (is.null(fit) || any(!is.finite(fit$coefficients))) {
    return(NULL)
  }
  b <- fit$coefficients
  mesor <- b[1]
  beta1 <- b[2]
  beta2 <- b[3]
  amplitude <- sqrt(beta1^2 + beta2^2)
  phi <- atan2(beta2, beta1)              # acrophase angle (Cornelissen 2014)
  acrophase <- (phi / omega) %% period    # clock hours
  list(
    mesor = as.numeric(mesor),
    beta1 = as.numeric(beta1),
    beta2 = as.numeric(beta2),
    amplitude = as.numeric(amplitude),
    phi = as.numeric(phi),
    acrophase = as.numeric(acrophase)
  )
}


#' Anti-Logistic (Extended) Cosinor Analysis (Marler et al. 2006)
#'
#' Fits the sigmoidally transformed cosine ("anti-logistic" extended cosinor)
#' model of Marler et al. (2006) to the averaged 24-hour activity profile. The
#' extended model relaxes the symmetric shape of the ordinary cosinor by adding
#' two shape parameters: \code{alpha} (governs the relative width of the active
#' versus rest phase) and \code{beta} (governs the steepness of the
#' rest-to-active transitions). This is the same model implemented by
#' \code{ActCR::ActExtendCosinor}, and this function reproduces its parameter
#' estimates numerically.
#'
#' @param counts Numeric vector of activity counts (one value per epoch).
#' @param timestamps POSIXct vector of timestamps, the same length as
#'   \code{counts}.
#' @param period Numeric period of the rhythm in hours (default \code{24}).
#'
#' @return A list with class \code{"canhrActi_cosinor_ext"} containing:
#'   \describe{
#'     \item{minimum}{Lower asymptote of the fitted curve (rest-phase level).}
#'     \item{amplitude}{Vertical span of the sigmoidal transition
#'       (\code{amp}); peak level is \code{minimum + amplitude}.}
#'     \item{alpha}{Width-asymmetry parameter in \eqn{[-1, 1]}.}
#'     \item{beta}{Steepness parameter (\eqn{\ge 0}).}
#'     \item{acrophase}{Time of peak activity in clock hours (\code{acrotime}).}
#'     \item{acrotime}{Alias of \code{acrophase} (ActCR naming).}
#'     \item{UpMesor}{Clock time of the rest-to-active (rising) transition,
#'       \eqn{-\arccos(\alpha) / (2\pi/T) + acrotime}.}
#'     \item{DownMesor}{Clock time of the active-to-rest (falling) transition,
#'       \eqn{\arccos(\alpha) / (2\pi/T) + acrotime}.}
#'     \item{MESOR}{Midline statistic \code{minimum + amplitude / 2}.}
#'     \item{F_pseudo}{Pseudo-F improvement of the extended fit over the
#'       ordinary cosinor: \eqn{((RSS_{cos} - RSS_{ext})/2)/(RSS_{ext}/(n-5))}.}
#'     \item{rss_cosinor}{Residual sum of squares of the ordinary cosinor.}
#'     \item{rss_extended}{Residual sum of squares of the extended cosinor.}
#'     \item{converged}{Logical; whether the nonlinear fit converged.}
#'     \item{period}{Period used (hours).}
#'     \item{n_profile_hours}{Number of profile hours used in the fit.}
#'   }
#'   On non-convergence or insufficient data all numeric parameters are
#'   returned as \code{NA} with \code{converged = FALSE}.
#'
#' @details
#' The model fitted to the averaged profile is
#' \deqn{f(t) = minimum + amplitude \cdot expit(\beta (cos(2\pi (t - acrotime)/T) - \alpha))}
#' where \eqn{expit(z) = 1/(1+e^{-z})}. This is the ActCR/Marler
#' parameterization, in which \code{amplitude} is the raw multiplier of the
#' logistic transform (it is \emph{not} renormalized by
#' \eqn{expit(\beta(1-\alpha))}). The closely related "normalized" Marler form,
#' \eqn{f(t) = min + amp \cdot expit(\beta(cos(\cdot) - \alpha)) / expit(\beta(1-\alpha))},
#' rescales \code{amplitude} so that the peak equals exactly
#' \code{minimum + amplitude}; the two forms describe the same curve shape and
#' the (\code{alpha}, \code{beta}, \code{acrotime}) parameters are identical.
#' The normalized peak level is reported as \code{peak}.
#'
#' Starting values are taken from an ordinary cosinor fit
#' (\code{minimum = max(mesor - amp, 0)}, \code{amplitude = 2 * amp},
#' \code{alpha = 0}, \code{beta = 2}, \code{acrotime = ordinary acrophase}). The
#' nonlinear least-squares problem is solved with \code{stats::optim()} using
#' the box-constrained L-BFGS-B method (bounds
#' \code{lower = c(0, 0, -1, 0, -3)}, \code{upper = c(Inf, Inf, 1, Inf, 27)},
#' matching ActCR), so no extra package dependency is required.
#'
#' @references
#' Marler MR, Gehrman P, Martin JL, Ancoli-Israel S (2006). The sigmoidally
#' transformed cosine curve: a mathematical model for circadian rhythms with
#' symmetric non-sinusoidal shapes. \emph{Statistics in Medicine},
#' 25(22):3893-3904.
#'
#' Wang J, et al. (2021). ActCR: Extract Circadian Rhythms Metrics from
#' Actigraphy Data. R package.
#'
#' @examples
#' \dontrun{
#' ts <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440 * 3)
#' hour <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60
#' counts <- 50 + 300 * plogis(4 * (cos((hour - 14) * 2 * pi / 24) - 0.2))
#' fit <- cosinor.antilogistic(counts, ts)
#' print(fit)
#' }
#'
#' @export
cosinor.antilogistic <- function(counts, timestamps, period = 24) {

  na_result <- function() {
    res <- list(
      minimum = NA_real_, amplitude = NA_real_, alpha = NA_real_,
      beta = NA_real_, acrophase = NA_real_, acrotime = NA_real_,
      peak = NA_real_, UpMesor = NA_real_, DownMesor = NA_real_,
      MESOR = NA_real_, F_pseudo = NA_real_,
      rss_cosinor = NA_real_, rss_extended = NA_real_,
      converged = FALSE, period = period, n_profile_hours = 0L
    )
    class(res) <- "canhrActi_cosinor_ext"
    res
  }

  if (length(counts) != length(timestamps)) {
    stop("counts and timestamps must have same length")
  }
  if (!is.numeric(period) || length(period) != 1L || !is.finite(period) ||
      period <= 0) {
    stop("period must be a single positive number")
  }

  prof <- .ext.hourly.profile(counts, timestamps)
  t <- prof$t
  y <- prof$y
  n <- prof$n

  # Need a reasonable number of distinct hours for a stable nonlinear fit.
  if (length(t) < 12 || all(y == y[1])) {
    return(na_result())
  }

  omega <- 2 * pi / period

  # Ordinary cosinor for starting values and for the RSS baseline.
  oc <- .ext.ordinary.cosinor(t, y, n, period = period)
  if (is.null(oc)) {
    return(na_result())
  }

  # Ordinary-cosinor residual sum of squares (unweighted, matches ActCR's
  # RSS_cos which is computed on the raw profile residuals).
  ypred_cos <- oc$mesor + oc$amplitude * cos(omega * t - oc$phi)
  rss_cos <- sum((y - ypred_cos)^2)

  # Starting parameters (ActCR convention).
  start <- c(
    max(oc$mesor - oc$amplitude, 0),  # minimum
    2 * oc$amplitude,                 # amplitude
    0,                                # alpha
    2,                                # beta
    oc$acrophase                      # acrotime
  )
  lower <- c(0, 0, -1, 0, -3)
  upper <- c(Inf, Inf, 1, Inf, 27)

  # Keep the acrotime start inside the box (acrophase can wrap above 24).
  if (start[5] > upper[5]) start[5] <- start[5] - period
  if (start[5] < lower[5]) start[5] <- start[5] + period
  start[5] <- min(max(start[5], lower[5]), upper[5])

  # Residual-sum-of-squares objective for the sigmoidally transformed cosine.
  rss_fun <- function(p) {
    ct <- cos((t - p[5]) * omega)
    z <- p[4] * (ct - p[3])
    lct <- stats::plogis(z)            # expit, numerically stable
    rt <- p[1] + p[2] * lct
    val <- sum((y - rt)^2)
    if (!is.finite(val)) return(.Machine$double.xmax)
    val
  }

  opt <- tryCatch(
    stats::optim(
      par = start, fn = rss_fun, method = "L-BFGS-B",
      lower = lower, upper = upper,
      control = list(maxit = 1000, factr = 1e7)
    ),
    error = function(e) NULL
  )

  if (is.null(opt) || opt$convergence != 0 || any(!is.finite(opt$par))) {
    res <- na_result()
    res$rss_cosinor <- rss_cos
    return(res)
  }

  p <- opt$par
  e_min <- p[1]
  e_amp <- p[2]
  e_alpha <- p[3]
  e_beta <- p[4]
  e_acrotime <- p[5]
  rss_ext <- opt$value

  # Pseudo-F improvement over the ordinary cosinor (Marler 2006 / ActCR).
  df2 <- length(y) - 5
  if (df2 > 0 && rss_ext > 0) {
    F_pseudo <- ((rss_cos - rss_ext) / 2) / (rss_ext / df2)
  } else {
    F_pseudo <- NA_real_
  }

  # Up/Down mesor crossing times (ActCR formulas). acos() is defined on [-1, 1];
  # clamp alpha defensively.
  a_clamped <- min(max(e_alpha, -1), 1)
  UpMesor <- -acos(a_clamped) / omega + e_acrotime
  DownMesor <- acos(a_clamped) / omega + e_acrotime
  MESOR <- e_min + e_amp / 2

  # Normalized-Marler peak level (minimum + amplitude in the renormalized form).
  peak <- e_min + e_amp * stats::plogis(e_beta * (1 - e_alpha))

  res <- list(
    minimum = as.numeric(e_min),
    amplitude = as.numeric(e_amp),
    alpha = as.numeric(e_alpha),
    beta = as.numeric(e_beta),
    acrophase = as.numeric(e_acrotime),
    acrotime = as.numeric(e_acrotime),
    peak = as.numeric(peak),
    UpMesor = as.numeric(UpMesor),
    DownMesor = as.numeric(DownMesor),
    MESOR = as.numeric(MESOR),
    F_pseudo = as.numeric(F_pseudo),
    rss_cosinor = as.numeric(rss_cos),
    rss_extended = as.numeric(rss_ext),
    converged = TRUE,
    period = period,
    n_profile_hours = length(y)
  )
  class(res) <- "canhrActi_cosinor_ext"
  res
}


#' Joint Amplitude-Acrophase Confidence Ellipse (Bingham et al. 1982)
#'
#' Computes the joint confidence region for the cosinor amplitude and acrophase,
#' following Bingham et al. (1982). The region is an ellipse in the
#' \eqn{(\beta_1, \beta_2)} (cosine/sine coefficient) plane. If the ellipse
#' excludes the origin, the population amplitude is significantly different from
#' zero and a rhythm is detected.
#'
#' @param cosinor_result A list returned by \code{cosinor.analysis()} (class
#'   \code{"canhrActi_cosinor"}). It must contain \code{amplitude},
#'   \code{acrophase} (clock hours), \code{se_amplitude}, \code{period} and
#'   \code{n_profile_hours}. If the object additionally carries an explicit
#'   \code{vcov_beta} (2x2 covariance of \eqn{(\beta_1, \beta_2)}) and/or
#'   \code{beta1}/\code{beta2}, those are used directly.
#' @param level Confidence level for the region (default \code{0.95}).
#' @param n_points Number of points used to trace the ellipse boundary
#'   (default \code{200}).
#'
#' @return A list with class \code{"canhrActi_cosinor_ellipse"} containing:
#'   \describe{
#'     \item{ellipse}{Data frame with columns \code{x} (\eqn{\beta_1}) and
#'       \code{y} (\eqn{\beta_2}) giving the ellipse boundary vertices.}
#'     \item{center}{Numeric vector \code{c(beta1, beta2)} of the point estimate.}
#'     \item{excludes_origin}{Logical; \code{TRUE} when the ellipse excludes
#'       \code{(0, 0)} (i.e. a rhythm is detected at the requested level).}
#'     \item{rhythm_detected}{Alias of \code{excludes_origin}.}
#'     \item{distance_stat}{Mahalanobis-type statistic of the origin relative to
#'       the fitted ellipse.}
#'     \item{critical_value}{Threshold \eqn{2 F_{2, df, level}} the statistic is
#'       compared against.}
#'     \item{level}{Confidence level used.}
#'   }
#'   When the input is missing required fields or is degenerate, the boundary is
#'   returned as \code{NA} and \code{excludes_origin = NA}.
#'
#' @details
#' Under the cosinor model \eqn{Y(t) = M + \beta_1 cos(\omega t) +
#' \beta_2 sin(\omega t)}, the \eqn{100(1-\alpha)\%} joint confidence region for
#' \eqn{(\beta_1, \beta_2)} is the set of points \eqn{b} satisfying
#' \deqn{(b - \hat b)^\top \Sigma^{-1} (b - \hat b) \le 2 F_{2, df, 1-\alpha}}
#' where \eqn{\Sigma} is the covariance of the estimated coefficients and
#' \eqn{df} the residual degrees of freedom. The averaged-profile cosinor design
#' used by \code{cosinor.analysis()} has orthogonal cosine and sine columns, so
#' \eqn{\Sigma} is (very nearly) diagonal with equal variances
#' \eqn{\sigma^2}; in that case \code{se_amplitude} (delta method) equals
#' \eqn{\sigma}, which is how \eqn{\Sigma} is reconstructed when it is not
#' supplied explicitly. The point estimate is recovered from amplitude and
#' acrophase as \eqn{\beta_1 = A cos(\phi)}, \eqn{\beta_2 = A sin(\phi)} with
#' \eqn{\phi = acrophase \cdot 2\pi / T}.
#'
#' @references
#' Bingham C, Arbogast B, Cornelissen Guillaume G, Lee JK, Halberg F (1982).
#' Inferential statistical methods for estimating and comparing cosinor
#' parameters. \emph{Chronobiologia}, 9(4):397-439.
#'
#' @examples
#' \dontrun{
#' cos <- cosinor.analysis(counts, timestamps)
#' ell <- cosinor.confidence.ellipse(cos)
#' ell$rhythm_detected
#' plot(ell$ellipse$x, ell$ellipse$y, type = "l"); points(0, 0)
#' }
#'
#' @export
cosinor.confidence.ellipse <- function(cosinor_result, level = 0.95,
                                       n_points = 200) {

  na_result <- function() {
    res <- list(
      ellipse = data.frame(x = NA_real_, y = NA_real_),
      center = c(NA_real_, NA_real_),
      excludes_origin = NA,
      rhythm_detected = NA,
      distance_stat = NA_real_,
      critical_value = NA_real_,
      level = level
    )
    class(res) <- "canhrActi_cosinor_ellipse"
    res
  }

  if (!is.list(cosinor_result)) {
    return(na_result())
  }
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("level must be a single number in (0, 1)")
  }

  period <- if (!is.null(cosinor_result$period)) cosinor_result$period else 24
  omega <- 2 * pi / period

  # ---- Point estimate of (beta1, beta2) -------------------------------------
  if (!is.null(cosinor_result$beta1) && !is.null(cosinor_result$beta2) &&
      is.finite(cosinor_result$beta1) && is.finite(cosinor_result$beta2)) {
    beta1 <- cosinor_result$beta1
    beta2 <- cosinor_result$beta2
  } else {
    amp <- cosinor_result$amplitude
    acr <- cosinor_result$acrophase
    if (is.null(amp) || is.null(acr) || !is.finite(amp) || !is.finite(acr)) {
      return(na_result())
    }
    phi <- acr * omega
    beta1 <- amp * cos(phi)
    beta2 <- amp * sin(phi)
  }

  # ---- Covariance of (beta1, beta2) -----------------------------------------
  vc <- NULL
  if (!is.null(cosinor_result$vcov_beta)) {
    vcb <- cosinor_result$vcov_beta
    if (is.matrix(vcb) && all(dim(vcb) == c(2, 2)) && all(is.finite(vcb))) {
      vc <- vcb
    }
  }
  if (is.null(vc)) {
    se_amp <- cosinor_result$se_amplitude
    if (is.null(se_amp) || !is.finite(se_amp) || se_amp <= 0) {
      return(na_result())
    }
    # Orthogonal averaged-profile design: equal, uncorrelated coefficient
    # variances, each equal to se_amplitude^2 (delta method identity).
    v <- se_amp^2
    vc <- matrix(c(v, 0, 0, v), nrow = 2)
  }

  # ---- Residual degrees of freedom ------------------------------------------
  df <- NULL
  if (!is.null(cosinor_result$n_profile_hours) &&
      is.finite(cosinor_result$n_profile_hours)) {
    df <- cosinor_result$n_profile_hours - 3
  } else if (!is.null(cosinor_result$df_resid) &&
             is.finite(cosinor_result$df_resid)) {
    df <- cosinor_result$df_resid
  }
  if (is.null(df) || df < 1) {
    return(na_result())
  }

  vc_inv <- tryCatch(solve(vc), error = function(e) NULL)
  if (is.null(vc_inv)) {
    return(na_result())
  }

  # Bingham joint region radius^2 = 2 * F(2, df, level).
  Fcrit <- stats::qf(level, 2, df)
  rad2 <- 2 * Fcrit

  # ---- Trace the ellipse boundary -------------------------------------------
  eig <- eigen(vc, symmetric = TRUE)
  evals <- pmax(eig$values, 0)            # guard tiny negatives
  axes <- sqrt(rad2 * evals)
  th <- seq(0, 2 * pi, length.out = n_points)
  circle <- rbind(axes[1] * cos(th), axes[2] * sin(th))
  pts <- eig$vectors %*% circle
  x <- beta1 + pts[1, ]
  y <- beta2 + pts[2, ]

  # ---- Does the region exclude the origin? ----------------------------------
  d0 <- c(0 - beta1, 0 - beta2)
  stat <- as.numeric(t(d0) %*% vc_inv %*% d0)
  excludes <- stat > rad2

  res <- list(
    ellipse = data.frame(x = x, y = y),
    center = c(beta1 = beta1, beta2 = beta2),
    excludes_origin = excludes,
    rhythm_detected = excludes,
    distance_stat = stat,
    critical_value = rad2,
    level = level
  )
  class(res) <- "canhrActi_cosinor_ellipse"
  res
}


#' Circadian Quotient and Relative Amplitude from a Cosinor Fit
#'
#' Computes the circadian quotient (amplitude divided by MESOR) and a cosinor
#' relative amplitude (amplitude divided by the overall mean) from a cosinor
#' result. The circadian quotient is a dimensionless measure of rhythm strength
#' relative to the rhythm-adjusted mean.
#'
#' @param cosinor_result A list returned by \code{cosinor.analysis()} (class
#'   \code{"canhrActi_cosinor"}) or any list providing \code{amplitude} and
#'   \code{mesor}. An optional \code{overall_mean} (raw arithmetic mean of the
#'   counts) is used for the relative-amplitude denominator when present;
#'   otherwise \code{mesor} is used.
#'
#' @return A list with class \code{"canhrActi_circadian_quotient"} containing:
#'   \describe{
#'     \item{circadian_quotient}{\code{amplitude / mesor}.}
#'     \item{relative_amplitude}{\code{amplitude / overall_mean} (falls back to
#'       \code{amplitude / mesor} when \code{overall_mean} is absent).}
#'   }
#'   Returns \code{NA} entries when the inputs are missing or the denominator is
#'   non-positive.
#'
#' @references
#' Nelson W, Tong YL, Lee JK, Halberg F (1979). Methods for cosinor-rhythmometry.
#' \emph{Chronobiologia}, 6(4):305-323.
#'
#' @examples
#' \dontrun{
#' cos <- cosinor.analysis(counts, timestamps)
#' circadian.quotient(cos)
#' }
#'
#' @export
circadian.quotient <- function(cosinor_result) {

  na_result <- function() {
    res <- list(
      circadian_quotient = NA_real_,
      relative_amplitude = NA_real_
    )
    class(res) <- "canhrActi_circadian_quotient"
    res
  }

  if (!is.list(cosinor_result) ||
      is.null(cosinor_result$amplitude) || is.null(cosinor_result$mesor)) {
    return(na_result())
  }

  amp <- cosinor_result$amplitude
  mesor <- cosinor_result$mesor
  if (!is.finite(amp) || !is.finite(mesor)) {
    return(na_result())
  }

  cq <- if (mesor > 0) amp / mesor else NA_real_

  denom <- if (!is.null(cosinor_result$overall_mean) &&
               is.finite(cosinor_result$overall_mean) &&
               cosinor_result$overall_mean > 0) {
    cosinor_result$overall_mean
  } else if (mesor > 0) {
    mesor
  } else {
    NA_real_
  }
  ra <- if (is.finite(denom)) amp / denom else NA_real_

  res <- list(
    circadian_quotient = as.numeric(cq),
    relative_amplitude = as.numeric(ra)
  )
  class(res) <- "canhrActi_circadian_quotient"
  res
}
