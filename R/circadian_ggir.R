# GGIR's own circadian block, copied rather than reproduced: the blocks marked verbatim are
# GGIR 3.3-6 R/cosinor_IS_IV_Analyses.R and R/g.IVIS.R, so that the numbers reproduce
# GGIR's. Offered as named methods beside the package's own engine because the two are not
# comparable: GGIR fits the cosinor to log(mg + 1), binarises the series against a
# threshold and aggregates to one hour before IS, IV and phi, and its phi is the AR(1)
# coefficient of the hourly series. Two edits: a requireNamespace guard for ActCR, which is
# in Suggests, and g.IVIS called through this file's own copy. GGIR's stale trailing
# comment on the g.IVIS call is left as it is.
# GGIR is Apache-2.0, copyright Vincent van Hees and the GGIR contributors,
# https://github.com/wadpac/GGIR. The copied blocks carry that licence; a copy is at
# inst/LICENSE.GGIR.

# GGIR 3.3-6 R/g.IVIS.R, whole file, verbatim.
.circ.ggir.ivis <- function(Xi, epochSize = 60, threshold = NULL) {
  # GGIR verbatim from here
  if (!is.null(threshold)) {
    Xi = ifelse(Xi > threshold, 0, 1)
  }
  if (epochSize < 3600) {
    df = data.frame(Xi = Xi, time = numeric(length(Xi)))
    time = seq(0, length(Xi) * epochSize, by = epochSize)
    df$time = time[1:nrow(df)]
    df = stats::aggregate(x = df, by = list(floor(df$time/3600) *
        3600), FUN = mean, na.action = stats::na.pass, na.rm = TRUE)
    Xi = df$Xi
  }
  hour = (1:ceiling(length(Xi))) - 1
  ts = data.frame(Xi = Xi, hour = hour, stringsAsFactors = TRUE)
  IS = IV = phi = NA
  if (nrow(ts) > 1) {
    ts$day = floor(ts$hour/24) + 1
    ts$hour = ts$hour - (floor(ts$hour/24) * 24)
    if (nrow(ts) > 1) {
      aveDay = stats::aggregate(. ~ hour, data = ts, mean, na.action = stats::na.omit)
      Xh = aveDay$Xi
      Xm = suppressWarnings(mean(Xh, na.rm = TRUE))
      deltaXi = diff(Xi)^2
      N = length(Xi[!is.na(Xi)])
      model = stats::arima(Xi[!is.na(Xi)], order = c(1, 0, 0))
      phi = model$coef[[1]]
      ISnum = sum((Xh - Xm)^2, na.rm = TRUE) * N
      ISdenom = 24 * sum((Xi - Xm)^2, na.rm = TRUE)
      IS = ISnum/ISdenom
      IVnum = sum(deltaXi, na.rm = TRUE) * N
      IVdenom = (N - 1) * sum((Xi - Xm)^2, na.rm = TRUE)
      IV = IVnum/IVdenom
    }
  }
  invisible(list(InterdailyStability = IS, IntradailyVariability = IV,
      phi = phi))
  # end of GGIR verbatim
}

#' GGIR's Circadian Block, on Any Activity Series
#'
#' Runs GGIR's own part-2 circadian analysis: the cosinor and the extended cosinor of Marler
#' et al. through \pkg{ActCR}, and interdaily stability, intradaily variability and phi
#' through GGIR's \code{g.IVIS}. The statements are copied from GGIR, so the result is the
#' 21 circadian columns GGIR writes to \code{part2_summary.csv}. It runs on ActiGraph counts
#' and on ENMO alike, as GGIR does, but the values are not comparable between the two,
#' because GGIR fits the cosinor to \code{log(mg + 1)}.
#'
#' @param x Numeric activity series at a fixed epoch, no gaps. Values below 13 with a mean
#'   below 1 are taken to be g and multiplied by 1000, which is GGIR's own test.
#' @param epoch_length Epoch in seconds.
#' @param time_offset_hours Hours between midnight and the first sample, used to put the
#'   phase estimates back on the clock.
#' @param threshold Activity threshold for the binarisation that precedes IS, IV and phi, in
#'   mg. GGIR's default is \code{threshold.lig}, 40.
#'
#' @return A one-row data frame of GGIR's 21 columns, or NULL when \pkg{ActCR} is not
#'   installed.
#' @references
#' van Hees VT and others. GGIR: Raw Accelerometer Data Analysis.
#' \doi{10.5281/zenodo.1051064}
#'
#' Marler MR, Gehrman P, Martin JL, Ancoli-Israel S (2006). The sigmoidally
#' transformed cosine curve: a mathematical model for circadian rhythms with
#' symmetric non-sinusoidal shapes. \emph{Statistics in Medicine}, 25(22).
#' @export
circadian.ggir <- function(x, epoch_length = 60, time_offset_hours = 0,
                           threshold = 40) {
  if (!requireNamespace("ActCR", quietly = TRUE)) {
    warning("circadian.ggir() needs the ActCR package, which GGIR itself uses ",
            "for the cosinor and the extended cosinor. Install it to reproduce ",
            "GGIR's numbers.", call. = FALSE)
    return(NULL)
  }
  x <- as.numeric(x)
  if (length(x) < 2 || all(is.na(x))) return(NULL)
  out <- tryCatch(
    .circ.ggir.cosinor(Xi = x, epochsize = epoch_length,
                       timeOffsetHours = time_offset_hours,
                       threshold = threshold),
    error = function(e) NULL)
  if (is.null(out)) return(NULL)
  cp <- out$coef$params
  ce <- out$coefext$params
  data.frame(
    cosinor_timeOffsetHours = time_offset_hours,
    cosinor_mes = cp$mes, cosinor_amp = cp$amp,
    cosinor_acrophase = cp$acr, cosinor_acrotime = cp$acrotime,
    cosinor_ndays = cp$ndays, cosinor_R2 = cp$R2,
    cosinorExt_minimum = ce$minimum, cosinorExt_amp = ce$amp,
    cosinorExt_alpha = ce$alpha, cosinorExt_beta = ce$beta,
    cosinorExt_acrotime = ce$acrotime, cosinorExt_UpMesor = ce$UpMesor,
    cosinorExt_DownMesor = ce$DownMesor, cosinorExt_MESOR = ce$MESOR,
    cosinorExt_ndays = ce$ndays, cosinorExt_F_pseudo = ce$F_pseudo,
    cosinorExt_R2 = ce$R2,
    IS = out$IVIS$InterdailyStability,
    IV = out$IVIS$IntradailyVariability,
    phi = out$IVIS$phi,
    stringsAsFactors = FALSE)
}

# GGIR 3.3-6 R/cosinor_IS_IV_Analyses.R, whole file, verbatim apart from the
# two edits named in this file's header.
.circ.ggir.cosinor <- function(Xi, epochsize = 60, timeOffsetHours = 0, threshold = NULL) {
  # GGIR verbatim from here
  if (length(threshold) > 1) {
    threshold = threshold[1]
    warning("Multiple threshold values supplied to cosinor analysis, only first value used.")
  }
  # Apply Cosinor function from ActRC
  N = 1440 * (60 / epochsize) # Number of epochs per day
  Xi = Xi[1:(N * floor(length(Xi) / N))] # ActCR expects integer number of days

  # omit all days at the end with no data
  end2 = length(Xi)
  while (end2 >= N) {
    if (all(is.na(Xi[(end2 - N + 1):end2]))) {
      end2 = end2 - N
      Xi = Xi[1:end2]
    } else {
      end2 = -1
    }
  }
  # transform data to millig if data is stored in g-units
  notna = !is.na(Xi)
  if (max(Xi, na.rm = TRUE) < 13 && mean(Xi, na.rm = TRUE) < 1) {
    # 13 because a typical 8g accelerometer could in theory measure 7.5 in each axis without
    # being considered clipping, which results in a vector of 13
    # as soon as the time series has values above 13 then it is most likely that
    # it is either expressed in counts or in mg.
    Xi[notna] = Xi[notna] * 1000
  }
  # log transform data for ActCosinor, IV IS further down will use the non-transformed signal
  Xi_log = Xi
  Xi_log[notna] = log(Xi[notna] + 1)
  coef = ActCR::ActCosinor(x = Xi_log, window = 1440 / N)

  # Apply Extended Cosinor function from ActRC (now temporarily turned of to apply my own version)
  coefext = ActCR::ActExtendCosinor(x = Xi_log, window = 1440 / N, export_ts = TRUE)
  # Correct time estimates by offset in start of recording
  add24ifneg = function(x) {
    if (x < 0) x = x + 24
    return(x)
  }
  coef$params$acrotime = add24ifneg(coef$params$acrotime - timeOffsetHours)
  coefext$params$UpMesor = add24ifneg(coefext$params$UpMesor - timeOffsetHours)
  coefext$params$DownMesor = add24ifneg(coefext$params$DownMesor - timeOffsetHours)
  coefext$params$acrotime = add24ifneg(coefext$params$acrotime - timeOffsetHours)
  # do same for acrophase in radians (24 hours: 2 * pi)
  # take absolute value of acrophase, because it seems ActCR provides negative value in radians,
  # which is inverse correlated with acrotime
  coef$params$acr = abs(coef$params$acr) - ((timeOffsetHours / 24) * 2 * pi)
  k = ceiling(abs(coef$params$acr) / (pi * 2))
  if (coef$params$acr < 0) coef$params$acr = coef$params$acr + (k * 2 * pi)
  # Perform IVIS on the same input signal to allow for direct comparison
  IVIS = .circ.ggir.ivis(Xi = Xi,
                epochSize = epochsize,
                threshold = threshold) # take log, because Xi is logtransformed with offset of 1

  coefext$params$R2 = stats::cor(coefext$cosinor_ts$original, coefext$cosinor_ts$fittedYext)^2
  coef$params$R2 = stats::cor(coefext$cosinor_ts$original, coefext$cosinor_ts$fittedY)^2

 # this should equal: https://en.wikipedia.org/wiki/Coefficient_of_determination

  invisible(list(coef = coef, coefext = coefext, IVIS = IVIS))
  # end of GGIR verbatim
}
