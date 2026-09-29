# Ported from GGIR 3.3-9 R/g.applymetrics.R and the metric rounding of R/g.getmeta.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the closures nested inside
# g.applymetrics are standalone internal functions with explicit arguments, the
# metrics2do data.frame became do.* arguments, signal, zoo and actilifecounts are loaded
# through requireNamespace with an install hint, and ggir_exact gates two GGIR quirks
# (the HFENplus low-pass cutoff and the rollmed length fallback). The arithmetic, operand
# order, library calls and the order in which metrics are appended are unchanged.

#' Names of the Metrics in the Order g.applymetrics Appends Them
#'
#' Also the column order of GGIR's metashort table after the timestamp. Only the
#' metrics whose flag is on appear in a result. GGIR's metashort renames angle_x/y/z to
#' anglex/y/z at the epoch assembly stage, not here.
#'
#' @return Character vector of length 34.
#' @keywords internal
#' @noRd
.raw.metric.names <- function() {
  c("BFX", "BFY", "BFZ", "BFEN",
    "ZCX", "ZCY", "ZCZ",
    "LFX", "LFY", "LFZ", "LFEN", "LFENMO",
    "HFX", "HFY", "HFZ", "HFEN", "HFENplus",
    "roll_med_acc_x", "roll_med_acc_y", "roll_med_acc_z",
    "dev_roll_med_acc_x", "dev_roll_med_acc_y", "dev_roll_med_acc_z",
    "angle_x", "angle_y", "angle_z",
    "ENMO", "MAD", "EN", "ENMOa",
    "NeishabouriCount_x", "NeishabouriCount_y", "NeishabouriCount_z",
    "NeishabouriCount_vm")
}

#' Build the Full Set of Metric Flags With GGIR's Defaults
#'
#' The 32 \code{do.*} switches of g.applymetrics with GGIR's defaults (only
#' \code{do.enmo} and \code{do.anglez} on), overridden by whatever is supplied.
#'
#' @param ... Named logical overrides (\code{do.en = TRUE}), or a single unnamed list or
#'   data.frame whose \code{do.*} members are taken (a \code{raw.params()} object can be
#'   passed as is). Unknown names are an error.
#' @param all Optional logical; when given, every flag except the deprecated
#'   \code{do.brondcounts} is first set to that value, then \code{...} is applied.
#' @return Named list of 32 logicals in GGIR's order.
#' @keywords internal
#' @noRd
.raw.metric.flags <- function(..., all = NULL) {
  flags <- list(
    do.bfen = FALSE, do.enmo = TRUE, do.lfenmo = FALSE, do.en = FALSE,
    do.hfen = FALSE, do.hfenplus = FALSE, do.mad = FALSE,
    do.anglex = FALSE, do.angley = FALSE, do.anglez = TRUE,
    do.roll_med_acc_x = FALSE, do.roll_med_acc_y = FALSE, do.roll_med_acc_z = FALSE,
    do.dev_roll_med_acc_x = FALSE, do.dev_roll_med_acc_y = FALSE, do.dev_roll_med_acc_z = FALSE,
    do.enmoa = FALSE, do.lfen = FALSE,
    do.hfx = FALSE, do.hfy = FALSE, do.hfz = FALSE,
    do.lfx = FALSE, do.lfy = FALSE, do.lfz = FALSE,
    do.bfx = FALSE, do.bfy = FALSE, do.bfz = FALSE,
    do.zcx = FALSE, do.zcy = FALSE, do.zcz = FALSE,
    do.brondcounts = FALSE, do.neishabouricounts = FALSE)
  if (!is.null(all)) {
    for (nm in setdiff(names(flags), "do.brondcounts")) flags[[nm]] <- isTRUE(all)
  }
  dots <- list(...)
  if (length(dots) == 1L && is.null(names(dots)) &&
      (is.list(dots[[1L]]) || is.data.frame(dots[[1L]]))) {
    src <- as.list(dots[[1L]])
    dots <- src[intersect(names(src), names(flags))]
  }
  if (length(dots) > 0) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
      stop("metric flags must be named (do.enmo = TRUE, ...)", call. = FALSE)
    }
    unknown <- setdiff(names(dots), names(flags))
    if (length(unknown) > 0) {
      stop("unknown metric flag(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    for (nm in names(dots)) flags[[nm]] <- isTRUE(as.logical(dots[[nm]])[1L])
  }
  flags
}

#' Mean of a Per-Sample Series Over Consecutive Epochs
#'
#' GGIR's averagePerEpoch: the mean per \code{sf * epoch} samples from a cumulative sum.
#' A trailing partial epoch is dropped. The cumsum/diff form is not bit-identical to
#' rowMeans of a reshaped matrix, so it stays.
#'
#' @param x Numeric vector, one value per sample.
#' @param sf Sample frequency in Hz.
#' @param epoch Epoch length in seconds.
#' @return Numeric vector with one value per whole epoch.
#' @keywords internal
#' @noRd
.raw.average.per.epoch <- function(x, sf, epoch) {
  x2 <- cumsum(c(0, x))
  select <- seq(1, length(x2), by = sf * epoch)
  x3 <- diff(x2[round(select)]) / abs(diff(round(select)))
  x3
}

#' Sum of a Per-Sample Series Over Consecutive Epochs
#'
#' GGIR's sumPerEpoch, used for the zero-crossing counts.
#'
#' @inheritParams .raw.average.per.epoch
#' @return Numeric vector with one value per whole epoch.
#' @keywords internal
#' @noRd
.raw.sum.per.epoch <- function(x, sf, epoch) {
  x2 <- cumsum(c(0, x))
  select <- seq(1, length(x2), by = sf * epoch)
  x3 <- diff(x2[round(select)])
  x3
}

#' Coerce the Upper Filter Bound Below the Nyquist Frequency
#'
#' GGIR lowers \code{hb} to \code{round(sf / 2) - 1} whenever \code{sf <= 2 * hb}: 15 Hz
#' becomes 14 Hz at 30 Hz and stays 15 Hz at 100 Hz. Every low-pass and band-pass metric
#' uses the coerced value.
#'
#' @param sf Sample frequency in Hz.
#' @param hb Requested upper bound in Hz (GGIR default 15).
#' @return The bound actually used, in Hz.
#' @keywords internal
#' @noRd
.raw.coerce.hb <- function(sf, hb) {
  if (sf <= (hb * 2)) {
    hb <- round(sf / 2) - 1
  }
  hb
}

#' zoo::rollmedian With a Zero Fill, Without the zoo Object
#'
#' zoo computes it as runmed(x, k) with the k %/% 2 values at each end set to the fill, so
#' the same runmed call gives the same numbers; zoo itself for NA, an even k or a short x.
#' @keywords internal
#' @noRd
.raw.rollmedian0 <- function(x, k) {
  n <- length(x)
  if (n < 1L || k < 1 || k > n || k %% 2 != 1 || !is.double(x) || !is.null(attributes(x)) ||
      anyNA(x)) {
    return(zoo::rollmedian(x, k = k, fill = c(0, 0, 0), na.pad = FALSE))
  }
  xm <- stats::runmed(x, k)
  attr(xm, "k") <- NULL
  m <- k %/% 2
  if (m >= 1) xm[c(1:m, (n - m + 1):n)] <- 0
  xm
}

#' Rolling Median of One Axis at a 10 Hz Working Rate
#'
#' GGIR's rollmed: every \code{stepsize}-th sample (\code{max(floor(sf / 10), 1)}), a
#' rolling median of \code{round(newsf * 5)} samples made odd with zero fill at both
#' ends, the zeros at the head and tail patched with the nearest non-zero value, and
#' each value repeated \code{stepsize} times to return to the input rate. Used for the
#' angle and roll_med metrics. \code{zoo::rollmedian} is the arithmetic, taken through
#' \code{.raw.rollmedian0}.
#'
#' @param x Numeric vector, one axis in g.
#' @param sf Sample frequency in Hz.
#' @param ggir_exact TRUE reproduces GGIR's fallback: when the repeated series is longer
#'   than \code{x} (a length that is not a multiple of \code{stepsize}) GGIR returns the
#'   raw axis \code{x[1:LX]}, not the median. FALSE returns the median truncated to the
#'   input length. The fallback is never reached from the epoch assembly, whose chunk
#'   lengths are multiples of \code{ws2 * sf}.
#' @return Numeric vector of the same length as \code{x}.
#' @keywords internal
#' @noRd
.raw.rollmed <- function(x, sf, ggir_exact = TRUE) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("the rolling-median metrics (angles, roll_med_acc_*) need the 'zoo' package: ",
         "install.packages('zoo')", call. = FALSE)
  }
  stepsize <- max(c(floor(sf / 10), 1))
  newsf <- ifelse(test = stepsize > 1, yes = 10, no = sf)
  winsi <- round(newsf * 5)
  if (round(winsi / 2) == (winsi / 2)) winsi <- winsi + 1
  if ((winsi %% 2) == 0) winsi <- winsi + 1
  xm <- .raw.rollmedian0(x[seq(1, length(x), by = stepsize)], winsi)
  # patch the zero fill
  S2check <- max(c(sf * 60, 1000))
  # head
  xm[which(xm[1:S2check] == 0)] <- xm[which(xm[1:S2check] != 0)[1]]
  # tail
  LN <- length(xm)
  S2check <- min(LN - 1, S2check)
  xm_tail <- xm[(LN - S2check):LN]
  lastvalue <- xm_tail[which(xm_tail != 0)][1]
  if (length(lastvalue) == 0) lastvalue <- xm[which(xm != 0)][1]
  xm_tail[which(xm_tail == 0)] <- lastvalue
  xm[(LN - S2check):LN] <- xm_tail
  # back to the input rate
  xm <- rep(xm, each = stepsize)
  LN <- length(xm)
  LX <- length(x)
  if (LN > LX) {
    if (ggir_exact) {
      xm <- x[1:LX] # GGIR's raw-axis fallback
    } else {
      xm <- xm[1:LX]
    }
  } else if (LN < LX) {
    xm <- c(xm, rep(xm[LN], LX - LN))
  }
  return(xm)
}

#' Filter the Three Axes of a Chunk
#'
#' GGIR's process_axes: one causal pass of a Butterworth filter (\code{signal::butter}
#' then \code{signal::filter}, zero initial state, no transient trimming) over each
#' axis, or the rolling median of \code{.raw.rollmed} per axis with a last-value patch
#' of any NA. GGIR runs this once per chunk, so the filter state restarts at every chunk
#' edge; callers pass whole chunks, never concatenated output. In GGIR the cut_point
#' argument is only read for "pass"; this port takes \code{lb} and \code{hb} directly
#' and the caller decides what to pass (see the HFENplus note in
#' \code{.raw.apply.metrics}).
#'
#' @param data Numeric matrix or data.frame with the three axes in columns 1 to 3.
#' @param type One of "pass" (band-pass \code{lb} to \code{hb}), "low" (low-pass at
#'   \code{hb}), "high" (high-pass at \code{lb}) or "rollmedian".
#' @param lb Lower cutoff in Hz.
#' @param hb Upper cutoff in Hz, already coerced with \code{.raw.coerce.hb}.
#' @param n Filter order (GGIR default 4).
#' @param sf Sample frequency in Hz.
#' @param ggir_exact Passed to \code{.raw.rollmed}.
#' @return Object of the same shape as \code{data} with the filtered axes.
#' @keywords internal
#' @noRd
.raw.filter <- function(data, type = c("pass", "low", "high", "rollmedian"),
                        lb = 0.2, hb = 15, n = 4, sf = c(), ggir_exact = TRUE) {
  type <- match.arg(type)
  if (length(sf) == 0) warning("sf not found")
  if (type == "pass" | type == "high" | type == "low") {
    if (!requireNamespace("signal", quietly = TRUE)) {
      stop("the filter-based metrics (BF*, LF*, HF*, HFENplus, ZC*) need the 'signal' package: ",
           "install.packages('signal')", call. = FALSE)
    }
    hf_lf_filter <- function(bound, n, sf, filtertype) {
      return(signal::butter(n, c(bound / (sf / 2)), type = filtertype))
    }
    bf_filter <- function(lb, hb, n, sf) {
      Wc <- matrix(0, 2, 1)
      Wc[1, 1] <- lb / (sf / 2)
      Wc[2, 1] <- hb / (sf / 2)
      if (sf / 2 < hb | sf / 2 < hb) {
        warning("\nSample frequency ", sf, " too low for calculating this metric.")
      }
      return(signal::butter(n, Wc, type = c("pass")))
    }
    if (type == "pass") {
      coef <- bf_filter(lb, hb, n, sf)
    } else {
      if (type == "low") {
        bound <- hb
      } else {
        bound <- lb
      }
      coef <- hf_lf_filter(bound, n, sf, type)
    }
    data_processed <- data
    for (i in 1:3) {
      data_processed[, i] <- signal::filter(coef, data[, i])
    }
  } else if (type == "rollmedian") {
    data_processed <- data
    for (i in 1:3) {
      data_processed[, i] <- .raw.rollmed(data[, i], sf, ggir_exact = ggir_exact)
    }
    if (length(which(is.na(data_processed[, 1]) == TRUE |
                     is.na(data_processed[, 2]) == TRUE |
                     is.na(data_processed[, 3]) == TRUE)) > 0) {
      for (j in 1:3) {
        p1 <- which(is.na(data_processed[, j]) == FALSE)
        data_processed[which(is.na(data_processed[, j]) == TRUE), j] <- data_processed[p1[length(p1)], j]
      }
    }
  }
  return(data_processed)
}

#' Acceleration Metrics per Short Epoch for One Calibrated Chunk
#'
#' GGIR's g.applymetrics: every requested metric from a calibrated three-axis chunk, one
#' vector per metric with one value per \code{ws3} epoch, in the order of
#' \code{.raw.metric.names()}. The result is unrounded; the epoch assembly rounds it
#' with \code{.raw.round.metrics}. ENMO, EN, ENMOa and MAD need no extra package; the
#' angle and roll_med metrics need \pkg{zoo}, the filtered and zero-crossing metrics
#' \pkg{signal}, and NeishabouriCount_* \pkg{actilifecounts}.
#'
#' @param data Numeric matrix (or data.frame) with columns named x, y, z in g, already
#'   calibrated; unnamed columns are taken as x, y, z in order. A trailing partial epoch
#'   is dropped.
#' @param sf Sample frequency in Hz.
#' @param ws3 Short epoch length in seconds (GGIR default 5).
#' @param do.bfen,do.enmo,do.lfenmo,do.en,do.hfen,do.hfenplus,do.mad,do.anglex,do.angley,do.anglez,do.roll_med_acc_x,do.roll_med_acc_y,do.roll_med_acc_z,do.dev_roll_med_acc_x,do.dev_roll_med_acc_y,do.dev_roll_med_acc_z,do.enmoa,do.lfen,do.hfx,do.hfy,do.hfz,do.lfx,do.lfy,do.lfz,do.bfx,do.bfy,do.bfz,do.zcx,do.zcy,do.zcz,do.brondcounts,do.neishabouricounts
#'   Logical metric switches with GGIR's defaults (only \code{do.enmo} and
#'   \code{do.anglez} on). \code{do.brondcounts = TRUE} stops with GGIR's deprecation
#'   message. See \code{.raw.metric.flags} for a builder.
#' @param n Butterworth filter order (GGIR default 4).
#' @param lb Lower filter bound in Hz (GGIR default 0.2).
#' @param hb Upper filter bound in Hz (GGIR default 15), coerced with
#'   \code{.raw.coerce.hb}.
#' @param zc.lb,zc.hb Band edges in Hz of the zero-crossing band-pass (GGIR 0.25 and 3).
#' @param zc.sb Stop band in g below which a filtered value counts as zero (GGIR 0.01).
#' @param zc.order Order of the zero-crossing band-pass (GGIR default 2).
#' @param actilife_LFE Passed to \code{actilifecounts::get_counts} as \code{lfe_select}.
#' @param ggir_exact TRUE reproduces GGIR. FALSE changes two quirks: the HFENplus gravity
#'   component is low-passed at \code{lb} as GGIR's comments describe, not at \code{hb}
#'   as GGIR's process_axes does; and the \code{.raw.rollmed} fallback.
#' @return A named list with one numeric vector per metric switched on, in the order of
#'   \code{.raw.metric.names()}; NULL when no metric is on. Angles are in degrees, ZC*
#'   and NeishabouriCount_* are counts per epoch, everything else is in g.
#'
#' @details Kept as in GGIR: HFEN is computed whenever any of do.hfen, do.hfx, do.hfy or
#'   do.hfz is on, any roll_med or dev_roll_med flag computes all three axes, and the
#'   epoch assembly drops the columns not asked for. NeishabouriCount_* comes straight
#'   from \code{actilifecounts::get_counts} on the whole chunk and is not the same as
#'   canhrActi's \code{gt3x.counts()} (different idle-sleep treatment).
#' @keywords internal
#' @noRd
.raw.apply.metrics <- function(data, sf, ws3,
                               do.bfen = FALSE, do.enmo = TRUE, do.lfenmo = FALSE,
                               do.en = FALSE, do.hfen = FALSE, do.hfenplus = FALSE,
                               do.mad = FALSE, do.anglex = FALSE, do.angley = FALSE,
                               do.anglez = TRUE,
                               do.roll_med_acc_x = FALSE, do.roll_med_acc_y = FALSE,
                               do.roll_med_acc_z = FALSE,
                               do.dev_roll_med_acc_x = FALSE, do.dev_roll_med_acc_y = FALSE,
                               do.dev_roll_med_acc_z = FALSE,
                               do.enmoa = FALSE, do.lfen = FALSE,
                               do.hfx = FALSE, do.hfy = FALSE, do.hfz = FALSE,
                               do.lfx = FALSE, do.lfy = FALSE, do.lfz = FALSE,
                               do.bfx = FALSE, do.bfy = FALSE, do.bfz = FALSE,
                               do.zcx = FALSE, do.zcy = FALSE, do.zcz = FALSE,
                               do.brondcounts = FALSE, do.neishabouricounts = FALSE,
                               n = 4, lb = 0.2, hb = 15,
                               zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2,
                               actilife_LFE = FALSE, ggir_exact = TRUE) {
  if (is.null(colnames(data))) {
    if (NCOL(data) < 3) stop("data needs three columns x, y, z", call. = FALSE)
    data <- data[, 1:3]
    colnames(data) <- c("x", "y", "z")
  } else if (!all(c("x", "y", "z") %in% colnames(data))) {
    stop("data needs columns named x, y and z", call. = FALSE)
  }
  data <- data[, c("x", "y", "z")]

  epochsize <- ws3

  # GGIR imports signal and calls actilifecounts without a check
  needs_signal <- do.bfen | do.bfx | do.bfy | do.bfz | do.zcx | do.zcy | do.zcz |
    do.lfenmo | do.lfx | do.lfy | do.lfz | do.lfen |
    do.hfen | do.hfx | do.hfy | do.hfz | do.hfenplus
  if (needs_signal && !suppressMessages(requireNamespace("signal", quietly = TRUE))) {
    stop("the filter-based metrics (BF*, LF*, HF*, HFENplus, ZC*) need the 'signal' package: ",
         "install.packages('signal')", call. = FALSE)
  }
  if (do.brondcounts == TRUE) {
    stop(paste0("\nThe brondcounts option has been deprecated following issues with the ",
                "following issues with the activityCounts package. We will reinsert brondcounts ",
                "once the issues are resolved."), call. = FALSE)
  }
  if (do.neishabouricounts == TRUE &&
      !suppressMessages(requireNamespace("actilifecounts", quietly = TRUE))) {
    stop("the NeishabouriCount_* metrics need the 'actilifecounts' package: ",
         "install.packages('actilifecounts')", call. = FALSE)
  }

  allmetrics <- c()

  hb <- .raw.coerce.hb(sf, hb)
  gravity <- 1
  # GGIR's process_axes low-passes HFENplus at hb, not lb
  hfenplus_low_hb <- if (ggir_exact) hb else lb

  anglex <- function(xyz) {
    return(atan(xyz[, 1] / (sqrt(xyz[, 2]^2 + xyz[, 3]^2))) / (pi / 180))
  }
  angley <- function(xyz) {
    return(atan(xyz[, 2] / (sqrt(xyz[, 1]^2 + xyz[, 3]^2))) / (pi / 180))
  }
  anglez <- function(xyz) {
    return(atan(xyz[, 3] / (sqrt(xyz[, 1]^2 + xyz[, 2]^2))) / (pi / 180))
  }
  EuclideanNorm <- function(xyz) {
    return(sqrt((xyz[, 1]^2) + (xyz[, 2]^2) + (xyz[, 3]^2)))
  }
  # band-pass metrics
  if (do.bfen == TRUE | do.bfx == TRUE | do.bfy == TRUE | do.bfz == TRUE) {
    data_processed <- abs(.raw.filter(data, type = "pass", lb = lb, hb = hb, n = n, sf = sf))
    if (do.bfx == TRUE) {
      allmetrics$BFX <- .raw.average.per.epoch(x = data_processed[, 1], sf, epochsize)
    }
    if (do.bfy == TRUE) {
      allmetrics$BFY <- .raw.average.per.epoch(x = data_processed[, 2], sf, epochsize)
    }
    if (do.bfz == TRUE) {
      allmetrics$BFZ <- .raw.average.per.epoch(x = data_processed[, 3], sf, epochsize)
    }
    if (do.bfen == TRUE) {
      allmetrics$BFEN <- .raw.average.per.epoch(x = EuclideanNorm(data_processed), sf, epochsize)
    }
  }
  if (do.zcx == TRUE | do.zcy == TRUE | do.zcz == TRUE) { # zero crossings
    # band-pass to mimic the old sensor
    data_processed <- .raw.filter(data, type = "pass", lb = zc.lb, hb = zc.hb, n = zc.order, sf = sf)
    zil <- c()
    # the axis is the user's choice (Sadeh did not specify orientation)
    if (do.zcx == TRUE) zil <- 1
    if (do.zcy == TRUE) zil <- c(zil, 2)
    if (do.zcz == TRUE) zil <- c(zil, 3)
    Ndat <- nrow(data_processed)
    for (zi in zil) {
      # stop band
      smallvalues <- which(abs(data_processed[, zi]) < zc.sb)
      if (length(smallvalues) > 0) {
        data_processed[smallvalues, zi] <- 0
      }
      rm(smallvalues)
      # sign series; a stop-banded 0 counts as positive
      data_processed[, zi] <- ifelse(test = data_processed[, zi] >= 0, yes = 1, no = -1)
      zerocross <- function(x, Ndat) {
        tmp <- abs(sign(x[2:Ndat]) - sign(x[1:(Ndat - 1)])) * 0.5
        tmp <- c(tmp[1], tmp) # keep the length
        return(tmp)
      }
      if (zi == 1) {
        allmetrics$ZCX <- .raw.sum.per.epoch(zerocross(data_processed[, zi], Ndat), sf, epochsize)
      } else if (zi == 2) {
        allmetrics$ZCY <- .raw.sum.per.epoch(zerocross(data_processed[, zi], Ndat), sf, epochsize)
      } else if (zi == 3) {
        allmetrics$ZCZ <- .raw.sum.per.epoch(zerocross(data_processed[, zi], Ndat), sf, epochsize)
      }
    }
  }
  # low-pass metrics
  if (do.lfenmo == TRUE | do.lfx == TRUE | do.lfy == TRUE | do.lfz == TRUE | do.lfen == TRUE) {
    data_processed <- abs(.raw.filter(data, type = "low", lb = lb, hb = hb, n = n, sf = sf))
    if (do.lfx == TRUE) {
      allmetrics$LFX <- .raw.average.per.epoch(x = data_processed[, 1], sf, epochsize)
    }
    if (do.lfy == TRUE) {
      allmetrics$LFY <- .raw.average.per.epoch(x = data_processed[, 2], sf, epochsize)
    }
    if (do.lfz == TRUE) {
      allmetrics$LFZ <- .raw.average.per.epoch(x = data_processed[, 3], sf, epochsize)
    }
    if (do.lfen == TRUE) {
      allmetrics$LFEN <- .raw.average.per.epoch(x = EuclideanNorm(data_processed), sf, epochsize)
    }
    if (do.lfenmo == TRUE) {
      LFENMO <- EuclideanNorm(data_processed) - gravity
      LFENMO[which(LFENMO < 0)] <- 0
      allmetrics$LFENMO <- .raw.average.per.epoch(x = LFENMO, sf, epochsize)
    }
  }
  # high-pass metrics
  if (do.hfen == TRUE | do.hfx == TRUE | do.hfy == TRUE | do.hfz == TRUE) {
    data_processed <- abs(.raw.filter(data, type = "high", lb = lb, hb = hb, n = n, sf = sf))
    if (do.hfx == TRUE) {
      allmetrics$HFX <- .raw.average.per.epoch(x = data_processed[, 1], sf, epochsize)
    }
    if (do.hfy == TRUE) {
      allmetrics$HFY <- .raw.average.per.epoch(x = data_processed[, 2], sf, epochsize)
    }
    if (do.hfz == TRUE) {
      allmetrics$HFZ <- .raw.average.per.epoch(x = data_processed[, 3], sf, epochsize)
    }
    allmetrics$HFEN <- .raw.average.per.epoch(x = EuclideanNorm(data_processed), sf, epochsize)
  }
  # HFENplus
  if (do.hfenplus == TRUE) {
    data_processed <- .raw.filter(data, type = "low", lb = lb, hb = hfenplus_low_hb, n = n, sf = sf)
    GCP <- EuclideanNorm(data_processed) - gravity
    data_processed <- .raw.filter(data, type = "high", lb = lb, hb = hb, n = n, sf = sf)
    HFENplus <- EuclideanNorm(data_processed) + GCP
    HFENplus[which(HFENplus < 0)] <- 0
    allmetrics$HFENplus <- .raw.average.per.epoch(x = HFENplus, sf, epochsize)
  }
  # rolling median metrics
  roll_median_done <- FALSE
  if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE |
      do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) {
    data_processed <- .raw.filter(data, type = "rollmedian", sf = sf, ggir_exact = ggir_exact)
    roll_median_done <- TRUE
    if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE) {
      allmetrics$roll_med_acc_x <- .raw.average.per.epoch(x = data_processed[, 1], sf, epochsize)
      allmetrics$roll_med_acc_y <- .raw.average.per.epoch(x = data_processed[, 2], sf, epochsize)
      allmetrics$roll_med_acc_z <- .raw.average.per.epoch(x = data_processed[, 3], sf, epochsize)
    }
    if (do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) {
      allmetrics$dev_roll_med_acc_x <- .raw.average.per.epoch(x = abs(data[, 1] - data_processed[, 1]), sf, epochsize)
      allmetrics$dev_roll_med_acc_y <- .raw.average.per.epoch(x = abs(data[, 2] - data_processed[, 2]), sf, epochsize)
      allmetrics$dev_roll_med_acc_z <- .raw.average.per.epoch(x = abs(data[, 3] - data_processed[, 3]), sf, epochsize)
    }
  }
  if (do.anglex == TRUE | do.angley == TRUE | do.anglez == TRUE) {
    if (roll_median_done == FALSE) {
      data_processed <- .raw.filter(data, type = "rollmedian", sf = sf, ggir_exact = ggir_exact)
    }
    if (do.anglex == TRUE) {
      allmetrics$angle_x <- .raw.average.per.epoch(x = anglex(data_processed), sf, epochsize)
    }
    if (do.angley == TRUE) {
      allmetrics$angle_y <- .raw.average.per.epoch(x = angley(data_processed), sf, epochsize)
    }
    if (do.anglez == TRUE) {
      allmetrics$angle_z <- .raw.average.per.epoch(x = anglez(data_processed), sf, epochsize)
    }
  }
  # filter-free metrics
  EN <- EuclideanNorm(data)
  if (do.enmo == TRUE) {
    ENMO <- EN - 1
    ENMO[which(ENMO < 0)] <- 0
    allmetrics$ENMO <- .raw.average.per.epoch(x = ENMO, sf, epochsize)
  }
  if (do.mad == TRUE) { # mean amplitude deviation
    MEANS <- rep(.raw.average.per.epoch(x = EN, sf, epochsize), each = sf * epochsize)
    MAD <- abs(EN - MEANS)
    allmetrics$MAD <- .raw.average.per.epoch(x = MAD, sf, epochsize)
  }
  if (do.en == TRUE) {
    allmetrics$EN <- .raw.average.per.epoch(x = EN, sf, epochsize)
  }
  if (do.enmoa == TRUE) {
    ENMOa <- abs(EN - gravity)
    allmetrics$ENMOa <- .raw.average.per.epoch(x = ENMOa, sf, epochsize)
  }
  # Neishabouri counts
  if (do.neishabouricounts == TRUE) {
    if (ncol(data) > 3) data <- data[, 2:4]
    mycounts <- actilifecounts::get_counts(raw = data, sf = sf,
                                           epoch = epochsize, lfe_select = actilife_LFE,
                                           verbose = FALSE)
    if (sf < 30) {
      warning("\nNote: activityCounts not designed for handling sample frequencies below 30 Hertz")
    }
    allmetrics$NeishabouriCount_x <- mycounts[, 1]
    allmetrics$NeishabouriCount_y <- mycounts[, 2]
    allmetrics$NeishabouriCount_z <- mycounts[, 3]
    allmetrics$NeishabouriCount_vm <- mycounts[, 4]
  }
  return(allmetrics)
}

#' Round Every Metric Vector as g.getmeta Does Before Storing It
#'
#' GGIR rounds each metric to 4 decimal places after the epoch averaging and before
#' writing it into the metashort character matrix.
#'
#' @param allmetrics The list returned by \code{.raw.apply.metrics}.
#' @param digits Decimal places (GGIR's n_decimal_places, 4).
#' @return The same list with every element rounded; NULL in gives NULL out.
#' @keywords internal
#' @noRd
.raw.round.metrics <- function(allmetrics, digits = 4) {
  if (is.null(allmetrics)) return(NULL)
  lapply(allmetrics, round, digits)
}
