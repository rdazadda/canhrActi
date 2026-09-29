# Ported from GGIR 3.3-9 R/HASPT.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees et al.; Medical Research Council UK; Accelting; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Each branch and the shared tail is an
# internal function taking explicit arguments, the result is returned visibly, an unknown
# HASPT.algo and MotionWare at an unsupported epoch raise named errors, and the
# mis-scaled marker-button hour guard is kept under ggir_exact = TRUE.

# PARAMETERS AND HELPERS

#' Guider Parameters This Module Reads, With GGIR's Defaults
#'
#' The nine members of GGIR's params_sleep that HASPT reads, so \code{raw.guider()} can be
#' called without a full parameter object.
#'
#' @return Named list of nine members with GGIR's load_params defaults.
#' @keywords internal
#' @noRd
.raw.guider.defaults <- function() {
  list(HASPT.ignore.invalid = FALSE,
       HDCZA_threshold = c(),
       HDCZA_roll_windowsize = 5,
       HorAngle_threshold = 60,
       LowAcc_threshold = 0.014,
       spt_min_block_dur = 30,
       spt_max_gap_dur = 60,
       spt_max_gap_ratio = 1,
       consider_marker_button = FALSE)
}

#' Fill the Guider Parameters a Caller Did Not Supply
#'
#' A present-but-NULL member keeps its NULL (GGIR distinguishes NULL from the default for
#' HDCZA_threshold and spt_max_gap_ratio); absent members take \code{.raw.guider.defaults()}.
#'
#' @param params_sleep A list (GGIR's params_sleep or a subset), or NULL.
#' @return The list with every member of \code{.raw.guider.defaults()} present.
#' @keywords internal
#' @noRd
.raw.guider.params <- function(params_sleep = NULL) {
  if (is.null(params_sleep)) params_sleep <- list()
  if (!is.list(params_sleep)) {
    stop("params_sleep must be a list of GGIR params_sleep members, or NULL", call. = FALSE)
  }
  defaults <- .raw.guider.defaults()
  absent <- setdiff(names(defaults), names(params_sleep))
  if (length(absent) > 0) params_sleep[absent] <- defaults[absent]
  params_sleep
}

#' Clip or Zero-Pad the Invalid Mask to the Length of the Guider Series
#'
#' GGIR's inner \code{adjustlength}. The pad value is 0, meaning valid.
#'
#' @param x The guider series (the rolling median, the absolute angle or the smoothed
#'   activity, depending on the algorithm).
#' @param invalid The per-epoch 0/1 non-wear mask for the same window.
#' @return \code{invalid}, clipped or zero-padded to \code{length(x)}.
#' @keywords internal
#' @noRd
.raw.guider.adjustlength <- function(x, invalid) {
  if (length(invalid) > length(x)) {
    invalid <- invalid[1:length(x)]
  } else if (length(invalid) < length(x)) {
    invalid <- c(invalid, rep(0, length(x) - length(invalid)))
  }
  return(invalid)
}

#' Rebuild a Run Length Encoding After Its Values Were Edited
#'
#' GGIR's inner \code{rebuild_rle}. The series fed in is one epoch longer than N because of
#' the left pad, so the \code{[1:N]} truncation drops \code{nomov[N]} and the final epoch of
#' the window is unreachable.
#'
#' @param rlex An rle object whose \code{values} have been changed.
#' @param N Length to truncate the expanded series to.
#' @return A new rle object.
#' @keywords internal
#' @noRd
.raw.guider.rebuild.rle <- function(rlex, N) {
  y <- rep(rlex$values, rlex$lengths)
  return(rle(y[1:N]))
}

# MARKER BUTTON

#' Sleep Period From a Pair of Marker Button Presses
#'
#' Runs before the algorithm dispatch, so a winning marker pair replaces whatever
#' \code{HASPT.algo} asked for. Only Actiwatch and Philips Health band recordings carry a
#' marker column. The pair with the lowest mean activity and a duration nearest eight hours
#' wins, ties going to the first row.
#'
#' @details GGIR's hour guard \code{diff(range(button_pressed)) * (60/ws3) > 60} multiplies
#'   epochs by epochs per minute instead of dividing, so at ws3 = 5 it accepts a 25-second
#'   span; \code{ggir_exact = FALSE} divides instead. The top-10 ranking indexes out of
#'   bounds with fewer than 10 presses, producing NAs that \code{which(Var1 < Var2)} drops.
#'
#' @param marker The per-epoch marker column for this window.
#' @param invalid The per-epoch 0/1 non-wear mask for this window.
#' @param activity The per-epoch acceleration metric, or NULL.
#' @param sibs The per-epoch 0/1 sustained inactivity bout classification, or NULL.
#' @param ws3 Short epoch length in seconds.
#' @param consider_marker_button GGIR's params_sleep member; the block is skipped unless TRUE.
#' @param ggir_exact TRUE keeps GGIR's mis-scaled hour guard.
#' @return A four-member list (SPTE_start, SPTE_end, tib.threshold, part3_guider) when a pair
#'   wins, otherwise NULL, meaning the caller carries on to the algorithm dispatch.
#' @keywords internal
#' @noRd
.raw.guider.markerbutton <- function(marker, invalid, activity = NULL, sibs = NULL, ws3 = 5,
                                     consider_marker_button = FALSE, ggir_exact = TRUE) {
  if (length(marker) > 0 && consider_marker_button == TRUE) {
    button_pressed <- which(marker != 0)
    N_markers <- length(button_pressed)
    # more than one press, a span over an hour, under half the epochs invalid; GGIR's
    # span test multiplies where it should divide
    fraction_invalid <- length(which(invalid == 1)) / length(invalid)
    if (N_markers > 1 &&
        (if (isTRUE(ggir_exact)) diff(range(button_pressed)) * (60 / ws3) > 60
         else diff(range(button_pressed)) / (60 / ws3) > 60) &&
        fraction_invalid < 0.5) {
      # the 10 presses closest to the day, or all of that day's presses if more
      ranking <- sort(marker[button_pressed], decreasing = TRUE, index.return = TRUE)$ix
      button_pressed <- button_pressed[ranking[1:pmax(length(which(marker == 1)), 10)]]
      pairs <- expand.grid(button_pressed, button_pressed)
      pairs <- pairs[which(pairs$Var1 < pairs$Var2), ]
      pairs$duration_hours <- (pairs$Var2 - pairs$Var1) / (3600 / ws3)

      # drop short pairs that involve an imputed press
      pairs$mark1 <- marker[pairs$Var1]
      pairs$mark2 <- marker[pairs$Var2]
      pairs_to_ignore <- which((pairs$duration_hours < 3 & (pairs$mark1 < 1 | pairs$mark2 < 1)) |
                                 (pairs$duration_hours < 1))
      if (length(pairs_to_ignore) > 0 & length(pairs_to_ignore) < nrow(pairs)) {
        pairs <- pairs[-pairs_to_ignore, ]
      }
      pairs$activity <- 0
      if (is.null(activity) && !is.null(sibs)) {
        activity_tmp <- 1 - sibs
      } else {
        activity_tmp <- activity
      }
      invalid_index <- which(invalid == 1)
      if (length(invalid_index) > 0) {
        activity_tmp[invalid_index] <- NA
      }
      pairs$fraction_sibs_outside <- NA
      pairs$fraction_sibs_inside <- NA
      for (i in 1:nrow(pairs)) {
        i0 <- pairs$Var1[i]
        i1 <- pairs$Var2[i]
        pairs$activity[i] <- mean(activity_tmp[i0:i1], na.rm = TRUE) + 1
        if (i0 > 1 && i1 < length(sibs)) {
          outside <- c(1:(i0 - 1), (i1 + 1):length(sibs))
          sum_outside <- sum(sibs[outside])
          fraction_outside <- sum_outside / length(outside)
          fraction_inside <- sum(sibs[i0:i1]) / (i1 - i0 + 1)
          pairs$fraction_sibs_inside[i] <- fraction_inside
          pairs$fraction_sibs_outside[i] <- fraction_outside
        }
      }
      # only pairs with fewer sibs outside than inside
      pairs <- pairs[which(pairs$fraction_sibs_outside < pairs$fraction_sibs_inside), ]
      if (nrow(pairs) > 0) {
        pairs$dur_score <- pmax(abs(pairs$duration_hours - 8), 1) - 1
        pairs$score <- pairs$activity * pairs$dur_score
        winner <- which.min(pairs$score)[1]
        if (!is.na(winner)) {
          start <- pairs$Var1[winner]
          end <- pairs$Var2[winner]
          HASPT.algo <- "markerbutton"
          return(list(SPTE_start = start, SPTE_end = end,
                      tib.threshold = 0,
                      part3_guider = HASPT.algo))
        }
      }
    }
  }
  NULL
}

# BRANCHES THAT SHARE THE TAIL

#' HDCZA: the 5-Minute Rolling Median of the Absolute Angle Difference
#'
#' The default guider. \code{x} is a rolling median of \code{abs(diff(angle))} and the
#' threshold is a percentile of that series times a multiplier, clamped between 0.13 and 0.50
#' for the two-element form of HDCZA_threshold only. \code{zoo::rollapply} centres the even
#' width and its \code{fill = 0} pad is below any threshold, so the padded epochs count as
#' no movement. When the clamp does not fire the threshold carries the "10%" name from
#' \code{quantile()}.
#'
#' @param angle The per-epoch angle for this window (anglez, or the longitudinal axis on a
#'   hip recording).
#' @param ws3 Short epoch length in seconds.
#' @param HDCZA_roll_windowsize Rolling window in minutes (GGIR default 5; hard-coded 5 in
#'   GGIR 3.3.6).
#' @param HDCZA_threshold NULL or \code{c(percentile, multiplier)} (the clamped form), or a
#'   single number used as the threshold with no clamping.
#' @return list(x, threshold).
#' @keywords internal
#' @noRd
.raw.guider.hdcza <- function(angle, ws3 = 5, HDCZA_roll_windowsize = 5,
                              HDCZA_threshold = NULL) {
  medabsdi <- function(angle) {
    # median, not mean, which is outlier sensitive
    angvar <- stats::median(abs(diff(angle)))
    return(angvar)
  }
  k1 <- HDCZA_roll_windowsize * (60 / ws3)
  x <- .raw.roll.medabsdi(angle, k1, medabsdi)
  if (is.null(HDCZA_threshold)) {
    HDCZA_threshold <- c(10, 15)
  }
  if (length(HDCZA_threshold) == 2) {
    threshold <- stats::quantile(x, probs = HDCZA_threshold[1] / 100) * HDCZA_threshold[2]
    if (threshold < 0.13) {
      threshold <- 0.13
    } else if (threshold > 0.50) {
      threshold <- 0.50
    }
  } else {
    threshold <- HDCZA_threshold
  }
  list(x = x, threshold = threshold)
}

#' zoo::rollapply(angle, k1, medabsdi, fill = 0) as One runmed Call
#'
#' For an even k1 each centred window holds k1 - 1 differences, an odd count, so its median
#' is one of them and runmed over abs(diff(angle)) picks the same value; zoo otherwise.
#' @keywords internal
#' @noRd
.raw.roll.medabsdi <- function(angle, k1, medabsdi) {
  n <- length(angle)
  if (!is.double(angle) || !is.null(attributes(angle)) || !all(is.finite(angle)) || k1 %% 2 != 0 ||
      k1 < 4 || n < k1 + 2) {
    return(zoo::rollapply(angle, width = k1, FUN = medabsdi, fill = 0))
  }
  d <- abs(diff(angle))
  k <- k1 - 1
  r <- stats::runmed(d, k)
  m <- k %/% 2
  x <- numeric(n)
  # zoo centres an even width on its (k1 / 2)th value
  x[(k1 / 2):(n - k1 / 2)] <- r[(m + 1):(length(d) - m)]
  x
}

#' HorAngle: the Absolute Angle of the Longitudinal Axis
#'
#' For hip recordings, where the longitudinal axis lies horizontal while the wearer is in
#' bed. The default threshold is 60 degrees.
#'
#' @param angle The per-epoch angle of the longitudinal axis for this window.
#' @param HorAngle_threshold GGIR's params_sleep member, in degrees (default 60).
#' @return list(x, threshold).
#' @keywords internal
#' @noRd
.raw.guider.horangle <- function(angle, HorAngle_threshold = 60) {
  x <- abs(angle)
  threshold <- HorAngle_threshold
  list(x = x, threshold = threshold)
}

#' LowAcc: a Smoothed Acceleration Metric Against a Fixed Low Threshold
#'
#' New in GGIR 3.3-9. The smoother is \code{stats::filter} with \code{circular = TRUE}, so
#' the first and last 2.5 minutes of the window mix in data from the other end, as in GGIR.
#'
#' @param activity The per-epoch acceleration metric for this window.
#' @param ws3 Short epoch length in seconds.
#' @param LowAcc_threshold GGIR's params_sleep member (default 0.014).
#' @return list(x, threshold).
#' @keywords internal
#' @noRd
.raw.guider.lowacc <- function(activity, ws3 = 5, LowAcc_threshold = 0.014) {
  x <- activity
  # 5 minute rolling average, to reduce sensitivity to sudden peaks
  ma <- function(x, n = 300 / ws3) {
    stats::filter(x, rep(1 / n, n), sides = 2, circular = TRUE)
  }
  x <- ma(x)
  threshold <- LowAcc_threshold
  list(x = x, threshold = threshold)
}

#' NotWorn: the Longest Period of Near-Zero Activity, for Protocols That Remove the Sensor
#'
#' The threshold is 5 percent of the standard deviation of the non-zero smoothed values,
#' falling back to the 10th percentile when that is below \code{min(activity)}, plus 0.001
#' so the shared tail's strict \code{<} behaves like \code{<=}. The caller forces
#' HASPT.ignore.invalid to NA for this branch.
#'
#' @param activity The per-epoch acceleration metric for this window.
#' @param ws3 Short epoch length in seconds.
#' @return list(x, threshold).
#' @keywords internal
#' @noRd
.raw.guider.notworn <- function(activity, ws3 = 5) {
  # no angle in count data, so look for the longest period of zero or very low intensity
  x <- activity
  ma <- function(x, n = 300 / ws3) {
    stats::filter(x, rep(1 / n, n), sides = 2, circular = TRUE)
  }
  x <- ma(x)
  nonzero <- which(x != 0)
  if (length(nonzero) > 0) {
    activityThreshold <- stats::sd(x[nonzero], na.rm = TRUE) * 0.05
    # sensewear data has values of 1 and up, so fall back to a percentile
    if (activityThreshold < min(activity)) {
      activityThreshold <- stats::quantile(x, probs = 0.1)
    }
  } else {
    activityThreshold <- 0
  }
  # + 0.001 so the tail's x < threshold reads as x <= threshold
  threshold <- activityThreshold + 0.001
  list(x = x, threshold = threshold)
}

# BRANCHES WITH THEIR OWN SEARCH

#' MotionWare: the Cambridge Neurotechnology Auto Sleep Detection
#'
#' Steps A to F follow Information bulletin no.3, sleep algorithms, by Cambridge
#' Neurotechnologies, section 1.6, with GGIR's hard-coded MarkerButtonLimit 3,
#' minimum_sleep_fraction 0.6 and Prescan_Sleep_Fraction 0.8. GGIR defines
#' \code{score_threshold} only for ws3 of 15, 30 or 60 seconds; any other epoch length is a
#' named error here.
#'
#' @param activity The per-epoch counts for this window.
#' @param invalid The per-epoch 0/1 non-wear mask for this window.
#' @param marker The per-epoch marker column, or NULL.
#' @param ws3 Short epoch length in seconds; must be 15, 30 or 60.
#' @return A four-member list (SPTE_start, SPTE_end, tib.threshold, part3_guider).
#' @keywords internal
#' @noRd
.raw.guider.motionware <- function(activity, invalid, marker = NULL, ws3 = 60) {
  # hardcoded in GGIR while the branch is experimental
  MarkerButtonLimit <- 3
  minimum_sleep_fraction <- 0.6
  Prescan_Sleep_Fraction <- 0.8
  # (A - classify sleep/wake)
  if (ws3 == 15) {
    score_threshold <- 1.5
  } else if (ws3 == 30) {
    score_threshold <- 3
  } else if (ws3 == 60) {
    score_threshold <- 6
  } else {
    # GGIR leaves score_threshold undefined here
    stop("HASPT.algo \"MotionWare\" needs a short epoch (ws3) of 15, 30 or 60 seconds; got ",
         ws3, call. = FALSE)
  }
  sleep_wake_score <- rep(0, length(activity))
  sleep_wake_score[which(activity <= score_threshold)] <- 1

  # (B - ignore nonwear)
  sleep_wake_score[which(invalid == 1)] <- 0

  # at least an hour each of sleep and wake, and under 33% invalid
  Npoints <- length(sleep_wake_score)
  start_log <- end_log <- NULL
  start <- 0
  end <- 0
  tib.threshold <- 0
  if (length(which(invalid == 1)) / Npoints <= 0.33 &&
      length(which(sleep_wake_score == 1)) > 60 * (60 / ws3) &&
      length(which(sleep_wake_score == 0)) > 60 * (60 / ws3)) {
    # (F while loop to repeat steps C, D, E to find all sleep periods)
    start_point <- 1
    while (start_point < Npoints - 180 * (60 / ws3)) {
      # (C - find midpoint)
      find_midpoint <- function(cnt, sleep_wake_score, ws3) {
        midpoint <- NULL
        avscore <- 0
        while (avscore < Prescan_Sleep_Fraction) { #0.8
          j0 <- cnt
          j1 <- cnt + ((60 / ws3) * 180)
          if (j1 > Npoints) break
          avscore <- mean(sleep_wake_score[j0:j1])
          cnt <- cnt + 1
        }
        if (avscore >= Prescan_Sleep_Fraction) {
          while (avscore > Prescan_Sleep_Fraction - 0.02) {
            j1 <- j1 + 1
            if (j1 > Npoints) break
            avscore <- mean(sleep_wake_score[j0:j1])
          }
          midpoint <- round((j1 - j0) / 2) + j0
        } else {
          j0 <- j1 <- midpoint <- NULL
        }
        invisible(list(j0 = j0, j1 = j1, midpoint = midpoint))
      }
      fmout <- find_midpoint(cnt = start_point, sleep_wake_score, ws3)

      start <- fmout$j0
      end <- fmout$j1
      midpoint <- fmout$midpoint
      if (is.null(midpoint)) break
      # (D - extend)
      find_edge <- function(x, where, midpoint, minimum_sleep_fraction) {
        if (where == "before") {
          xrle <- rle(rev(x[1:midpoint]))
        } else if (where == "after") {
          xrle <- rle(x[midpoint:length(x)])
        }
        log_len <- 0

        if (xrle$values[1] == 0) {
          # If midpoint equals wake, then always include that segment
          log_len <- log_len + xrle$lengths[1]
          xrle$values <- xrle$values[-1]
          xrle$lengths <- xrle$lengths[-1]
        }
        if (length(xrle$values) > 0) {
          if (xrle$values[1] == 1) {
            # Always include first sleep segment
            log_len <- log_len + xrle$lengths[1]
            xrle$values <- xrle$values[-1]
            xrle$lengths <- xrle$lengths[-1]
          }
          Nsegments <- length(xrle$lengths)
          Nout <- floor(Nsegments / 2)
          if (Nsegments > 1) {
            wakesegs <- cumsum(xrle$lengths[seq(1, by = 2, length.out = Nout)])
            sleepsegs <- cumsum(xrle$lengths[seq(2, by = 2, length.out = Nout)])
            extension_duration <- sleepsegs + wakesegs
            fractions <- sleepsegs / (sleepsegs + wakesegs)
            # ignore (keep) extensions that do not meet sleep fraction
            too_little_sleep <- which(fractions < minimum_sleep_fraction)
            if (length(too_little_sleep) > 0) {
              last_segment <- (too_little_sleep[1] - 1) * 2
            } else {
              last_segment <- length(xrle$lengths)
            }
            if (last_segment > 0 && !is.na(last_segment)) {
              log_len <- log_len + sum(xrle$lengths[1:last_segment])
            }
          }
        }
        if (where == "before") {
          edge <- midpoint - log_len
        } else if (where == "after") {
          edge <- midpoint + log_len
        }
        return(edge)
      }
      start <- find_edge(sleep_wake_score, where = "before", midpoint, minimum_sleep_fraction)
      end <- find_edge(sleep_wake_score, where = "after", midpoint, minimum_sleep_fraction)
      # (E - snap to a nearby marker button, unless the loop is stuck)
      do_no_use_marker <- FALSE
      if (length(start_log) > 2 &&
          start_log[length(start_log)] == start_log[length(start_log) - 1]) {
        do_no_use_marker <- TRUE
      }
      if (length(marker) > 0 && do_no_use_marker == FALSE) {
        marker_indices <- which(marker == 1)
        if (length(marker_indices) > 1) {
          delta_start <- abs(start - marker_indices)
          ds2 <- which(delta_start < (60 / ws3) * 60 * MarkerButtonLimit)
          if (length(ds2) > 0) {
            start <- rev(marker_indices[ds2])[1] # use last
          }
          delta_end <- abs(end - marker_indices)
          de2 <- which(delta_end < (60 / ws3) * 60 * MarkerButtonLimit)
          if (length(de2) > 0) {
            end <- rev(marker_indices[de2])[1] # use last
          }
        }
      }
      start_log <- c(start_log, start)
      end_log <- c(end_log, end)
      start_point <- end + (60 / ws3) * 60
    }
    dur_log <- end_log - start_log + 1
    # longest period only; GGIR cannot handle multiple sleep periods
    start <- start_log[which.max(dur_log)[1]]
    end <- end_log[which.max(dur_log)[1]]
  }
  list(SPTE_start = start, SPTE_end = end, tib.threshold = 0,
       part3_guider = "MotionWare")
}

#' HLRB: the Longest Rest Block in the Sustained Inactivity Classification
#'
#' Works from \code{sibs} alone. The rolling mean window is one hour, wake gaps shorter
#' than one hour are filled, the longest remaining block wins and ties go to the last one.
#' \code{MaxSleepGap} is assigned and never used, as in GGIR.
#'
#' @param sibs The per-epoch 0/1 sustained inactivity bout classification for this window.
#' @param ws3 Short epoch length in seconds.
#' @return A four-member list (SPTE_start, SPTE_end, tib.threshold, part3_guider).
#' @keywords internal
#' @noRd
.raw.guider.hlrb <- function(sibs, ws3 = 5) {
  start <- end <- NULL
  sibs <- round(zoo::rollmean(x = sibs, k = (60 / ws3) * 60, fill = 0))
  MaxSleepGap <- 1
  # fill wake gaps shorter than an hour
  srle <- rle(c(0, as.numeric(sibs), 0))
  wakegaps <- which(srle$values == 0 & srle$lengths < (60 / ws3) * 60 * 1)
  wakegaps <- wakegaps[which(wakegaps %in% c(1, length(srle$values)) == FALSE)]
  if (length(wakegaps) > 0) {
    srle$values[wakegaps] <- 1
  }
  sibs <- rep(srle$values, srle$lengths)
  sibs <- sibs[-c(1, length(sibs))]
  # keep longest sleep period
  srle <- rle(c(0, as.numeric(sibs), 0))
  if (length(srle$lengths) > 2) {
    size_longest_sleep <- max(srle$lengths[which(srle$values == 1)])
    sleepgaps <- which(srle$values == 1 & srle$lengths < size_longest_sleep)
    if (length(sleepgaps) > 0) {
      srle$values[sleepgaps] <- 0
    }
  }
  sibs <- c(0, rep(srle$values, srle$lengths), 0)
  start <- which(diff(sibs) == 1)
  end <- which(diff(sibs) == -1)
  # If there are multiple windows with equal length take last one
  if (length(start) > 1 & length(end) > 1) {
    start <- start[length(end)]
    end <- end[length(end)]
  }
  sibs <- sibs[-c(1, length(sibs))]
  # no block found: the whole window
  if (length(start) == 0 & length(end) == 0) {
    start <- 1
    end <- length(sibs)
  }
  list(SPTE_start = start, SPTE_end = end, tib.threshold = 0,
       part3_guider = "HLRB")
}

# SHARED TAIL

#' The Three Steps That Turn a Guider Series Into One SPT Window
#'
#' Shared by HDCZA, HorAngle, LowAcc and NotWorn. Classifies no-movement, drops blocks
#' shorter than \code{spt_min_block_dur}, fills gaps shorter than \code{spt_max_gap_dur},
#' keeps the longest surviving block and reports its edges.
#'
#' @details The comparisons are \code{<} for no-movement, \code{<=} for the short block
#'   removal and \code{<} for the gap fill; the first and last rle segment are never removed
#'   or filled; the ratio branch needs \code{spt_max_gap_ratio < 1}, so the default of 1
#'   takes the simple path. SPTE_start and SPTE_end are epoch indices into this window, not
#'   hours, and the interval is half open: SPTE_end is the first epoch after the block. A
#'   block running to the end of the window reports \code{SPTE_end = N}, because the padded
#'   series is truncated one epoch early. \code{spt_crude_estimate} is snapshotted before the
#'   longest-block selection and then has \code{[SPTE_start:SPTE_end]} overwritten with 2,
#'   inclusive and unshifted. A window that is 100 percent invalid skips the whole block, so
#'   SPTE stays empty and the guider stays "none". The "+invalid" tag, which part 4 reads, is
#'   appended only when HASPT.ignore.invalid is NA and an invalid epoch falls inside the
#'   selected window.
#'
#' @param x The guider series from the branch.
#' @param threshold The threshold from the branch.
#' @param invalid The per-epoch 0/1 non-wear mask for this window.
#' @param HASPT.algo The algorithm name, written into \code{part3_guider}.
#' @param angle,activity,sibs The three input series, used only for the final truncation
#'   length \code{max(c(length(angle), length(activity), length(sibs)))}.
#' @param ws3 Short epoch length in seconds.
#' @param HASPT.ignore.invalid FALSE (ignore the mask), TRUE (invalid is movement) or NA
#'   (invalid is no movement, and tag the guider).
#' @param spt_min_block_dur,spt_max_gap_dur Minutes.
#' @param spt_max_gap_ratio NULL or a number; the ratio branch needs a value below 1.
#' @return The five-member list.
#' @keywords internal
#' @noRd
.raw.guider.finalsteps <- function(x, threshold, invalid, HASPT.algo, angle = NULL,
                                   activity = NULL, sibs = NULL, ws3 = 5,
                                   HASPT.ignore.invalid = FALSE, spt_min_block_dur = 30,
                                   spt_max_gap_dur = 60, spt_max_gap_ratio = 1) {
  # no-movement epochs, with the selected treatment of invalid time
  nomov <- rep(0, length(x))
  invalid <- .raw.guider.adjustlength(x, invalid)
  if (is.na(HASPT.ignore.invalid)) { # invalid = no movement
    nomov[which(x < threshold | invalid == 1)] <- 1
  } else if (HASPT.ignore.invalid == FALSE) { # over the imputed angle
    nomov[which(x < threshold)] <- 1
  } else if (HASPT.ignore.invalid == TRUE) {  # invalid = movement
    nomov[which(x < threshold & invalid == 0)] <- 1
  }

  SPTE_end <- c()
  SPTE_start <- c()
  tib.threshold <- c()
  part3_guider <- "none"
  spt_crude_estimate <- NULL
  N <- length(x)
  spt_estimate <- rep(NA, N)
  nomov <- c(0, nomov, 0)
  rle_nomov <- rle(nomov)
  fraction_night_invalid <- sum(invalid) / length(invalid)
  if (fraction_night_invalid < 1) {
    # Step -3: ignore blocks that are too short
    blocks_to_remove <- which(rle_nomov$values == 1 &
                                rle_nomov$lengths <= (60 / ws3) * spt_min_block_dur)
    blocks_to_remove <- blocks_to_remove[which(blocks_to_remove %in%
                                                 c(1, length(rle_nomov$values)) == FALSE)]
    if (length(blocks_to_remove) > 0) {
      rle_nomov$values[blocks_to_remove] <- 0
      rle_nomov <- .raw.guider.rebuild.rle(rle_nomov, N)
    }
    # Step -2: fill gaps that are short
    Nsegments <- length(rle_nomov$lengths)
    if (!is.null(spt_max_gap_ratio) && spt_max_gap_ratio < 1 && Nsegments > 3) {
      gap_ratios <- data.frame(values = rle_nomov$values, lengths = rle_nomov$lengths)
      gap_ratios$ratio <- gap_ratios$length_after <- gap_ratios$length_before <- 0
      gap_ratios$length_after[1:(Nsegments - 1)] <- gap_ratios$lengths[2:Nsegments]
      gap_ratios$length_before[2:Nsegments] <- gap_ratios$lengths[1:(Nsegments - 1)]
      gaps_to_fill <- which(gap_ratios$values == 0 &
                              gap_ratios$lengths < (60 / ws3) * spt_max_gap_dur &
                              gap_ratios$lengths / gap_ratios$length_after < spt_max_gap_ratio &
                              gap_ratios$lengths / gap_ratios$length_before < spt_max_gap_ratio)
    } else {
      gaps_to_fill <- which(rle_nomov$values == 0 &
                              rle_nomov$lengths < (60 / ws3) * spt_max_gap_dur)
    }
    if (length(gaps_to_fill) > 0) {
      gaps_to_fill <- gaps_to_fill[which(gaps_to_fill %in%
                                           c(1, length(rle_nomov$values)) == FALSE)]
    }
    if (length(gaps_to_fill) > 0) {
      rle_nomov$values[gaps_to_fill] <- 1
      rle_nomov <- .raw.guider.rebuild.rle(rle_nomov, N)
    }
    # estimate before the longest block is selected
    spt_crude_estimate <- rep(rle_nomov$values, rle_nomov$lengths)
    # Step -1: keep indices for longest spt block
    if (1 %in% rle_nomov$values) {
      max_length <- max(rle_nomov$lengths[which(rle_nomov$values == 1)])
      rle_nomov$values[which(rle_nomov$values == 1 & rle_nomov$lengths == max_length)[1]] <- 2
      rle_nomov$values[which(rle_nomov$values != 2)] <- 0
      rle_nomov$values[which(rle_nomov$values == 2)] <- 1
    }
    spt_estimate <- rep(rle_nomov$values, rle_nomov$lengths)
    spt_estimate <- spt_estimate[1:length(x)]
    # edges of the longest block; the - 1 undoes the left pad
    SPTE_start <- which(diff(c(0, spt_estimate, 0)) == 1) - 1
    SPTE_end <- which(diff(c(0, spt_estimate, 0)) == -1) - 1
    if (length(SPTE_start) == 1 && length(SPTE_end) == 1 && SPTE_start == 0) SPTE_start <- 1
    if (length(SPTE_start) > 0 & length(SPTE_end) > 0) {
      spt_crude_estimate[SPTE_start:SPTE_end] <- 2 # the final estimate, in the crude one
    }
    spt_crude_estimate <- spt_crude_estimate[1:max(c(length(angle), length(activity),
                                                     length(sibs)))]
    part3_guider <- HASPT.algo
    if (is.na(HASPT.ignore.invalid)) {
      # tag the guider when invalid time fell inside the SPT, so part 4 trusts the diary
      spt_long <- rep(0, length(invalid))
      if (length(SPTE_start) > 0 & length(SPTE_end) > 0) {
        spt_long[SPTE_start:SPTE_end] <- 1
      }
      invalid_in_spt <- which(invalid == 1 & spt_long == 1)
      if (length(invalid_in_spt)) {
        part3_guider <- paste0(HASPT.algo, "+invalid")
      }
    }
  }
  tib.threshold <- threshold
  list(SPTE_start = SPTE_start, SPTE_end = SPTE_end, tib.threshold = tib.threshold,
       part3_guider = part3_guider, spt_crude_estimate = spt_crude_estimate)
}

# THE GUIDER

#' Estimate One Night's Sleep Period Time Window (GGIR HASPT)
#'
#' The guider: given one noon-to-noon window of angle, activity, non-wear mask, marker button
#' and sustained inactivity bouts, it returns the window GGIR calls the sleep period time,
#' which part 4 then uses to label each sustained inactivity bout as sleep or not. Eight
#' strategies are available through \code{HASPT.algo}; the default HDCZA needs only the
#' z-angle.
#'
#' @details The marker button runs before the dispatch and pre-empts every algorithm when
#'   a pair of presses wins. \code{notused} (set by GGIR when \code{def.noc.sleep} has
#'   length 2) skips the body and returns five NULLs. HDCZA, HorAngle, LowAcc and NotWorn
#'   share the tail in \code{.raw.guider.finalsteps}; NotWorn forces HASPT.ignore.invalid to
#'   NA for this call only. MotionWare (15, 30 or 60 s epochs only) and HLRB return early.
#'
#'   SPTE_start and SPTE_end are 1-based epoch indices into this window, not hours; the
#'   detector converts them. The interval is half open, so the duration in epochs is
#'   \code{SPTE_end - SPTE_start}, and a block that runs to the end of the window reports
#'   \code{SPTE_end = N}. The early-return branches return four members and no
#'   \code{spt_crude_estimate}, which the guider corrector relies on.
#'
#'   Deviations: the result is returned visibly, an unrecognised \code{HASPT.algo} and
#'   MotionWare at an unsupported epoch raise named errors, and \code{ggir_exact = FALSE}
#'   corrects the mis-scaled marker-button hour guard.
#'
#' @param angle The per-epoch angle for one night window, NAs already replaced by 0
#'   (\code{anglez} from the part-2 imputed short-epoch table, or \code{anglex}/\code{angley}
#'   on a hip recording). Used by HDCZA and HorAngle.
#' @param params_sleep GGIR's params_sleep list, or NULL for its defaults. The members read
#'   are HASPT.ignore.invalid, HDCZA_threshold, HDCZA_roll_windowsize, HorAngle_threshold,
#'   LowAcc_threshold, spt_min_block_dur, spt_max_gap_dur, spt_max_gap_ratio and
#'   consider_marker_button.
#' @param ws3 Short epoch length in seconds (GGIR's \code{M$windowsizes[1]}).
#' @param HASPT.algo One of "HDCZA", "HorAngle", "LowAcc", "NotWorn", "MotionWare", "HLRB",
#'   "notused". GGIR's caller passes \code{params_sleep$HASPT.algo[guider_to_use]}, one
#'   element.
#' @param invalid The per-epoch 0/1 non-wear mask for the same window.
#' @param activity The per-epoch acceleration metric for the same window (ENMO by default).
#'   Used by LowAcc, NotWorn, MotionWare and the marker button.
#' @param marker The per-epoch marker column for the same window, or NULL.
#' @param sibs The per-epoch 0/1 sustained inactivity bout classification for the same
#'   window, the FIRST definition only. Used by HLRB and the marker button.
#' @param ggir_exact TRUE (default) reproduces GGIR exactly, including the mis-scaled
#'   marker-button hour guard. FALSE divides by the epochs per minute there instead of
#'   multiplying.
#'
#' @return A list with
#' \describe{
#'   \item{SPTE_start}{Index of the first epoch of the window, or NULL.}
#'   \item{SPTE_end}{Index of the first epoch AFTER the window (half open), or NULL.}
#'   \item{tib.threshold}{The threshold the branch used; 0 for the early-return branches. It
#'     keeps the "10%" name from \code{quantile()} when the HDCZA clamp does not fire, which
#'     GGIR strips one level up by assigning into a pre-allocated vector.}
#'   \item{part3_guider}{The algorithm that produced the window, "none" when the night was
#'     100 percent invalid, or the algorithm with "+invalid" appended when invalid time was
#'     taken into the window.}
#'   \item{spt_crude_estimate}{Per-epoch 0/1/2 codes before the longest-block selection, with
#'     the winning window overwritten with 2. Absent from the three early-return branches.}
#' }
#'
#' @seealso \code{\link{raw.wear.decision}} for the non-wear mask this consumes.
#'
#' @examples
#' # One synthetic 24 h window at 5 s epochs: an alternating angle (large epoch-to-epoch
#' # differences) with a five-hour still block in the middle of the night.
#' n <- 17280
#' angle <- rep(c(0, 30), length.out = n)
#' angle[8001:14000] <- 10
#' g <- raw.guider(angle = angle, ws3 = 5, invalid = rep(0, n))
#' c(start = g$SPTE_start, end = g$SPTE_end, hours = (g$SPTE_end - g$SPTE_start) / 720)
#' g$tib.threshold
#'
#' @export
raw.guider <- function(angle, params_sleep = NULL, ws3 = 5,
                       HASPT.algo = "HDCZA", invalid,
                       activity = NULL, marker = NULL,
                       sibs = NULL, ggir_exact = TRUE) {
  valid_algos <- c("HDCZA", "HorAngle", "LowAcc", "NotWorn", "MotionWare", "HLRB", "notused")
  if (length(HASPT.algo) != 1 || !is.character(HASPT.algo) || is.na(HASPT.algo)) {
    stop("HASPT.algo must be one of: ", paste(valid_algos, collapse = ", "), call. = FALSE)
  }
  params_sleep <- .raw.guider.params(params_sleep)
  tib.threshold <- SPTE_start <- SPTE_end <- part3_guider <- spt_crude_estimate <- NULL

  # marker button first, when present and asked for
  if (length(marker) > 0) {
    markerout <- .raw.guider.markerbutton(
      marker = marker, invalid = invalid, activity = activity, sibs = sibs, ws3 = ws3,
      consider_marker_button = params_sleep[["consider_marker_button"]],
      ggir_exact = ggir_exact)
    if (!is.null(markerout)) return(markerout)
  }

  if (HASPT.algo != "notused") {
    # a local copy, so NotWorn's override does not leak back to the caller
    HASPT.ignore.invalid <- params_sleep[["HASPT.ignore.invalid"]]
    if (HASPT.algo == "HDCZA") { # default
      branch <- .raw.guider.hdcza(
        angle = angle, ws3 = ws3,
        HDCZA_roll_windowsize = params_sleep[["HDCZA_roll_windowsize"]],
        HDCZA_threshold = params_sleep[["HDCZA_threshold"]])
    } else if (HASPT.algo == "HorAngle") { # hip
      branch <- .raw.guider.horangle(
        angle = angle, HorAngle_threshold = params_sleep[["HorAngle_threshold"]])
    } else if (HASPT.algo == "LowAcc") {
      branch <- .raw.guider.lowacc(
        activity = activity, ws3 = ws3,
        LowAcc_threshold = params_sleep[["LowAcc_threshold"]])
    } else if (HASPT.algo == "NotWorn") {
      branch <- .raw.guider.notworn(activity = activity, ws3 = ws3)
      # NotWorn is interested in the invalid periods, not the imputed series
      HASPT.ignore.invalid <- NA
    } else if (HASPT.algo == "MotionWare") {
      return(.raw.guider.motionware(activity = activity, invalid = invalid, marker = marker,
                                    ws3 = ws3))
    } else if (HASPT.algo == "HLRB") {
      return(.raw.guider.hlrb(sibs = sibs, ws3 = ws3))
    } else {
      # GGIR has no branch for this and fails with "object 'x' not found"
      stop("unknown HASPT.algo \"", HASPT.algo, "\"; valid values are: ",
           paste(valid_algos, collapse = ", "), call. = FALSE)
    }
    return(.raw.guider.finalsteps(
      x = branch$x, threshold = branch$threshold, invalid = invalid, HASPT.algo = HASPT.algo,
      angle = angle, activity = activity, sibs = sibs, ws3 = ws3,
      HASPT.ignore.invalid = HASPT.ignore.invalid,
      spt_min_block_dur = params_sleep[["spt_min_block_dur"]],
      spt_max_gap_dur = params_sleep[["spt_max_gap_dur"]],
      spt_max_gap_ratio = params_sleep[["spt_max_gap_ratio"]]))
  }
  list(SPTE_start = SPTE_start, SPTE_end = SPTE_end, tib.threshold = tib.threshold,
       part3_guider = part3_guider, spt_crude_estimate = spt_crude_estimate)
}
