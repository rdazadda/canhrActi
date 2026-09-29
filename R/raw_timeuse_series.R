# Ported from GGIR 3.3-9 R/g.part5_initialise_ts.R, R/g.part5.addsib.R,
# R/g.part5.handle_lux_extremes.R and R/is.ISO8601.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The params_247 and params_general
# lists became explicit arguments, the IMP and M lists are accepted as canhrActi objects
# too, and the one GGIR defect here that can change numbers (the addsib NA-angle repair,
# which indexes the whole series with window-relative positions) sits behind ggir_exact.
# No file I/O and no printing.

#' Build the Part-5 Time Series From the Imputed Epoch Table (GGIR g.part5_initialise_ts)
#'
#' Assembles the data.frame the whole of part 5 is computed from: one row per short epoch,
#' carrying the timestamp, acceleration in milli-g, a guider label, the elevation angle, the
#' non-wear flag expanded from the long-epoch wear decision, and whatever optional channels
#' part 1 produced. The series is built from the part-2 imputed short epochs, never from the
#' part-1 table.
#'
#' @details \code{ts$ACC} is the metric times 1000 for a g-unit metric such as ENMO and times
#'   1 for a count metric (Brond, Neishabouri, ZC, ExtAct, ExtHeartRate), where the thresholds
#'   are then counts per epoch. \code{ts$time} stays a character ISO 8601 vector, because the
#'   sib and sleep-window functions match on strings so that an autumn daylight-saving
#'   repeated hour cannot collide. Kept as GGIR has it: a \code{sensor_location} other than
#'   "hip" forces \code{longitudinal_axis} to NULL; the angle selection arms for axes 2 and 3
#'   test for "anglex" while picking angley and anglez; \code{nonwear} is the long-epoch
#'   decision repeated \code{windowsizes[2]/windowsizes[1]} times and truncated or zero-padded
#'   to the short epochs; light is recalibrated and cleaned at long-epoch resolution before
#'   the repeat; and the marker block tests the part-1 table but copies from the imputed one.
#'
#' @param imputed A canhrActi_raw_imputed from \code{raw.impute()}, a canhrActi_raw carrying one
#'   in \code{$imputed}, or a GGIR IMP list. Needs \code{metashort}, \code{rout} and
#'   \code{windowsizes}.
#' @param meta A canhrActi_raw_meta from \code{raw.getmeta()}, a canhrActi_raw, or a GGIR M
#'   list; only \code{metalong} (light and temperature) and \code{metashort} (the marker test)
#'   are read. Defaults to the \code{$meta} of \code{imputed} when that is a canhrActi_raw.
#' @param acc_metric Name of the acceleration column in the imputed table, GGIR's
#'   params_general acc.metric. Default "ENMO".
#' @param sensor_location GGIR's params_general sensor.location. Only the literal "hip" lets
#'   \code{longitudinal_axis} have any effect. Default "wrist".
#' @param lux_cal_constant GGIR's params_247 LUX_cal_constant. Both this and the exponent must
#'   be non-empty for the light recalibration to happen.
#' @param lux_cal_exponent GGIR's params_247 LUX_cal_exponent.
#' @param longitudinal_axis 1, 2, 3 or NULL, as estimated by part 3 and stored in the ms3
#'   milestone. Ignored unless \code{sensor_location} is "hip".
#' @return A data.frame with one row per short epoch and, on a default wrist recording, the
#'   columns time (character ISO 8601), ACC (milli-g), guider ("unknown"), angle and nonwear,
#'   plus anglex, angley, anglez, step_count, lightpeak, lightpeak_imputationcode, temperature
#'   and marker when those channels exist.
#' @keywords internal
#' @noRd
.raw.timeuse.init.ts <- function(imputed, meta = NULL, acc_metric = "ENMO",
                                 sensor_location = "wrist", lux_cal_constant = c(),
                                 lux_cal_exponent = c(), longitudinal_axis = NULL) {
  if (inherits(imputed, "canhrActi_raw")) {
    if (is.null(meta)) meta <- imputed$meta
    imputed <- imputed$imputed
  }
  if (inherits(meta, "canhrActi_raw")) meta <- meta$meta
  IMP <- imputed
  M <- meta
  if (!is.list(IMP) || !all(c("metashort", "rout", "windowsizes") %in% names(IMP))) {
    stop("imputed must be a canhrActi_raw, a canhrActi_raw_imputed from raw.impute(), or a ",
         "GGIR IMP list", call. = FALSE)
  }
  if (!is.list(M) || !"metalong" %in% names(M)) {
    stop("meta must be a canhrActi_raw, a canhrActi_raw_meta from raw.getmeta(), or a GGIR M ",
         "list", call. = FALSE)
  }
  if (is.null(IMP$metashort) || nrow(IMP$metashort) == 0) {
    stop("the imputed metashort has no rows", call. = FALSE)
  }
  if (!acc_metric %in% names(IMP$metashort)) {
    stop("the imputed metashort has no column named ", acc_metric, call. = FALSE)
  }
  # imputed acceleration, because it describes behaviour
  scale <- ifelse(test = grepl("^Brond|^Neishabouri|^ZC|^ExtAct|^ExtHeartRate", acc_metric),
                  yes = 1, no = 1000)

  # Use anglez by default or longitudinal axis if specified when sensor is worn on hip

  if (sensor_location != "hip") {
    longitudinal_axis <- NULL
  }
  if (is.null(longitudinal_axis) && "anglez" %in% names(IMP$metashort)) {
    angleName <- "anglez"
  } else if (longitudinal_axis == 1 && "anglex" %in% names(IMP$metashort)) {
    angleName <- "anglex"
  } else if (longitudinal_axis == 2 && "anglex" %in% names(IMP$metashort)) {
    # angley on an anglex test, as in GGIR
    angleName <- "angley"
  } else if (longitudinal_axis == 3 && "anglex" %in% names(IMP$metashort)) {
    # anglez on an anglex test, as in GGIR
    angleName <- "anglez"
  } else {
    angleName <- NULL
  }
  if (!is.null(angleName) && angleName %in% names(IMP$metashort)) {
    ts <- data.frame(time = IMP$metashort[, 1], ACC = IMP$metashort[, acc_metric] * scale,
                     guider = rep("unknown", nrow(IMP$metashort)),
                     angle = as.numeric(as.matrix(IMP$metashort[,
                                                                which(names(IMP$metashort) ==
                                                                        angleName)])))
    # also store other angles if available:
    for (otherAngle in c("anglex", "angley", "anglez")) {
      if (otherAngle %in% names(IMP$metashort) == TRUE && angleName != otherAngle) {
        if (otherAngle == "anglex") {
          ts$anglex <- as.numeric(as.matrix(IMP$metashort[, otherAngle]))
        } else if (otherAngle == "angley") {
          ts$angley <- as.numeric(as.matrix(IMP$metashort[, otherAngle]))
        } else if (otherAngle == "anglez") {
          ts$anglez <- as.numeric(as.matrix(IMP$metashort[, otherAngle]))
        }
      }
    }
  } else {
    ts <- data.frame(time = IMP$metashort[, 1], ACC = IMP$metashort[, acc_metric] * scale,
                     guider = rep("unknown", nrow(IMP$metashort)))
  }
  if ("step_count" %in% colnames(IMP$metashort)) {
    ts$step_count <- 0
    ts$step_count <- IMP$metashort$step_count
  }

  Nts <- nrow(ts)
  # add non-wear column
  nonwear <- IMP$rout[, 5]
  nonwear <- rep(nonwear, each = (IMP$windowsizes[2]/IMP$windowsizes[1]))
  if (length(nonwear) > Nts) {
    nonwear <- nonwear[1:Nts]
  } else if (length(nonwear) < Nts) {
    nonwear <- c(nonwear, rep(0, (Nts - length(nonwear))))
  }
  ts$nonwear <- 0 # initialise column
  ts$nonwear <- nonwear

  # Add temperature and light, if present
  lightpeak_available <- "lightpeak" %in% colnames(M$metalong)
  temperature_available <- "temperaturemean" %in% colnames(M$metalong)
  repeatvalues <- function(x, windowsizes, Nts) {
    x <- rep(x, each = (windowsizes[2]/windowsizes[1]))
    if (length(x) > Nts) {
      x <- x[1:Nts]
    } else if (length(x) < Nts) {
      x <- c(x, rep(0, (Nts - length(x))))
    }
    return(x)
  }
  if (lightpeak_available == TRUE) {
    luz <- M$metalong$lightpeak
    if (length(lux_cal_constant) > 0 &
        length(lux_cal_exponent) > 0) { # re-calibrate light
      luz <- lux_cal_constant * exp(lux_cal_exponent * luz)
    }
    handle_luz_extremes <- .raw.timeuse.lux.extremes(luz)
    luz <- handle_luz_extremes$lux
    correction_log <- handle_luz_extremes$correction_log
    # repeat to the short-epoch resolution
    luz <- repeatvalues(x = luz, windowsizes = IMP$windowsizes, Nts)
    correction_log <- repeatvalues(x = correction_log, windowsizes = IMP$windowsizes, Nts)
    ts$lightpeak_imputationcode <- ts$lightpeak <- 0 # initialise column
    ts$lightpeak <- luz
    ts$lightpeak_imputationcode <- correction_log
  }
  if (temperature_available == TRUE) {
    temperature <- M$metalong$temperaturemean
    ts$temperature <- repeatvalues(x = temperature, windowsizes = IMP$windowsizes, Nts)
  }
  # tests the part-1 table and copies from the imputed one, as in GGIR
  if ("marker" %in% colnames(M$metashort)) {
    ts$marker <- NA
    ts$marker <- IMP$metashort$marker
  }
  return(ts)
}

#' Blank and Impute Extreme Light Values (GGIR g.part5.handle_lux_extremes)
#'
#' Runs on the long-epoch light series. Every value above 120000 lux is flagged. A run of
#' three or more consecutive flagged long epochs is set to NA and coded 2; an isolated
#' extreme is blanked and then replaced by the mean of its surviving neighbours and coded 1.
#' Both constants are hard coded in GGIR. The runs pass comes first, so a run leaves NA in
#' the series and an isolated extreme next to a blanked run takes the mean of whatever
#' neighbour is not NA.
#'
#' @param lux Numeric vector of long-epoch light peaks.
#' @return Invisibly, list(lux, correction_log): the corrected series and a vector of 0 (no
#'   correction), 1 (isolated extreme, imputed) and 2 (part of a run, set to NA).
#' @keywords internal
#' @noRd
.raw.timeuse.lux.extremes <- function(lux) {
  detect.pattern <- function(patrn, x) {
    # credits to Berend Hasselman for following three lines of code
    # https://r.789695.n4.nabble.com/matching-a-sequence-in-a-vector-tp4389523p4389909.html
    patrn.rev <- rev(patrn)
    w <- stats::embed(x, length(patrn))
    w.pos <- which(apply(w, 1, function(r) all(r == patrn.rev)))
    if (length(w.pos) > 0) {
      result <- c()
      for (w.pos_i in 1:length(w.pos)) {
        result <- c(result, w.pos[w.pos_i]:(w.pos[w.pos_i] + length(patrn) - 1))
      }
    } else {
      result <- c()
    }
    return(result)
  }
  extremes_lux_values <- which(lux > 120000)
  correction_log <- rep(0, length(lux))
  correction_log[extremes_lux_values] <- 1
  # remove sequences lasting 3 long epochs or longer (default 45 minutes)
  extreme_sequence <- detect.pattern(patrn = rep(1, 3), x = correction_log)
  if (length(extreme_sequence) > 0) {
    lux[extreme_sequence] <- NA
    correction_log[extreme_sequence] <- 2
  }
  # for remaining extremes impute by average of neighbours
  extremes <- which(correction_log == 1)
  if (length(extremes) > 0) {
    lux[extremes] <- NA
    for (eb in 1:length(extremes)) {
      if (extremes[eb] == 1) {
        impluxi <- 1:2
      } else if (extremes[eb] > 1 & extremes[eb] < length(lux)) {
        impluxi <- (extremes[eb] - 1):(extremes[eb] + 1)
      } else if (extremes[eb] == length(lux)) {
        impluxi <- (length(lux) - 1):length(lux)
      }
      implux <- mean(lux[impluxi], na.rm = TRUE)
      if (length(implux) > 0) lux[extremes[eb]] <- implux
    }
  }
  invisible(list(lux = lux, correction_log = correction_log))
}

#' Is This an ISO 8601 Timestamp (GGIR is.ISO8601)
#'
#' Counts the pieces a split on "-" and on "+" produces: "2025-10-07T20:30:00-0800" gives
#' four and is TRUE, "2025-10-07 20:30:00" gives three and is FALSE.
#'
#' @param x A length-1 character value; anything that is not character returns FALSE.
#' @return TRUE or FALSE.
#' @keywords internal
#' @noRd
.raw.is.iso8601 <- function(x) {
  is.ISO <- FALSE
  if (is.character(x)) {
    NNeg <- length(unlist(strsplit(x, "[-]")))
    NPos <- length(unlist(strsplit(x, "[+]")))
    if (NPos == 2 | NNeg == 4 | NNeg == 2) {
      is.ISO <- TRUE
    }
  }
  return(is.ISO)
}

#' Put Part 3's Sustained Inactivity Bouts Back Into the Series (GGIR g.part5.addsib)
#'
#' Part 3 stores each sustained inactivity bout as a start and an end timestamp in
#' \code{sib.cla.sum}; this converts them to epoch indices and writes a 0/1
#' \code{sibdetection} column onto the series. Part A walks the bout table with a monotone
#' six-day search window and matches the onset and end strings inside it, marking the bout
#' inclusive of both ends; the comparison is on ISO 8601 strings so that an autumn
#' daylight-saving repeated hour matches the right occurrence. Part B re-derives the bouts
#' from the angle over the stretch from one hour before the first midnight to fourteen hours
#' after it, for every vanHees2015 definition (a name holding both "T" and "A"), because
#' part 3 with \code{excludefirstlast = TRUE} never assessed that stretch; it overwrites what
#' part A put there.
#'
#' @details Kept as GGIR has it: a bout that does not match moves the search window six days
#'   on; the thresholds are parsed back out of the definition string ("T5A5" is 5 minutes and
#'   5 degrees); when no gap qualifies the whole window becomes 1 if there were fewer than 5
#'   posture changes and 0 otherwise. One GGIR defect changes numbers and is reproduced under
#'   \code{ggir_exact = TRUE}: the NA-angle repair computes positions relative to the window
#'   and uses them to index the whole angle vector. With \code{ggir_exact = FALSE} the
#'   positions are offset into the window.
#'
#' @param ts The series from \code{.raw.timeuse.init.ts}, with \code{time} still character
#'   ISO 8601.
#' @param epochSize Short epoch length in seconds.
#' @param part3_output The \code{sib.cla.sum} rows for one sib definition, already cut of the
#'   invalid and empty nights by the caller.
#' @param desiredtz Timezone, used only when \code{ts$time} is not already ISO 8601.
#' @param sibDefinition The definition name, for example "T5A5".
#' @param nightsi Indices of the midnights. Part B is skipped when it is empty.
#' @param ggir_exact Reproduce GGIR's NA-angle repair exactly (default TRUE).
#' @return \code{ts} with a \code{sibdetection} column of 0 and 1.
#' @keywords internal
#' @noRd
.raw.timeuse.addsib <- function(ts, epochSize, part3_output, desiredtz, sibDefinition, nightsi,
                                ggir_exact = TRUE) {
  # part 3 stores the bouts as start and end times; convert them to indices
  Nts <- nrow(ts)
  ts$sibdetection <- 0
  s0s1 <- c()
  # pr0:pr1 is a six-day search window that follows the last matched bout; s0 and s1 are
  # the positions of the next bout inside it
  pr0 <- 1
  pr1 <- pr0 + ((60/epochSize) * 1440 * 6)
  pr2 <- Nts
  if (nrow(part3_output) > 0) {
    gik.ons <- format(part3_output$sib.onset.time)
    gik.end <- format(part3_output$sib.end.time)
    timeChar <- format(ts$time)
    if (.raw.is.iso8601(timeChar[1]) == FALSE) { # only do this for POSIX format
      timeChar <- .raw.posix.to.iso8601(timeChar, tz = desiredtz)
    }
    if (timeChar[1] != as.character(timeChar[1])) {
      timeChar <- as.character(timeChar)
    }
    for (g in 1:nrow(part3_output)) {
      lastpr0 <- pr0
      pr1 <- pr0 + ((60/epochSize) * 1440 * 6)
      if (pr1 > pr2) pr1 <- pr2
      if (pr0 > pr1) pr0 <- pr1
      # string match, so a daylight-saving repeated hour cannot collide
      timeChar_tmp <- timeChar[pr0:pr1]
      s0 <- which(timeChar_tmp == gik.ons[g])[1]
      s1 <- which(timeChar_tmp == gik.end[g])[1]
      if (is.na(s0) == TRUE) s0 <- which(timeChar_tmp == paste0(gik.ons[g], " 00:00:00"))[1]
      if (is.na(s1) == TRUE) s1 <- which(timeChar_tmp == paste0(gik.end[g], " 00:00:00"))[1]
      # add pr0 to make s0 and s1 be relative to the start of the recording
      s0 <- s0 + pr0 - 1
      s1 <- s1 + pr0 - 1
      pr0 <- s1
      if (length(s1) != 0 & length(s0) != 0 & is.na(s0) == FALSE & is.na(s1) == FALSE) {
        s0s1 <- c(s0s1, s0:s1)
      } else {
        pr0 <- lastpr0 + ((60/epochSize) * 1440 * 6)
      }
    }
  }
  ts$sibdetection[s0s1] <- 1
  if (length(grep(pattern = "A", sibDefinition)) > 0 &&
      length(grep(pattern = "T", sibDefinition)) > 0 &&
      length(nightsi) > 0) {
    # part 3 with excludefirstlast never assessed the first night; redo it here for vanHees2015
    redo1 <- nightsi[1] - ((60/epochSize) * 60) # 1 hour before first midnight
    if (redo1 < 1) redo1 <- 1
    redo2 <- nightsi[1] + (14 * (60/epochSize) * 60) # 14 hours after first midnight
    if (redo2 > Nts) redo2 <- Nts
    anglethreshold <- as.numeric(unlist(strsplit(sibDefinition, "A"))[2])
    tempi <- unlist(strsplit(unlist(strsplit(sibDefinition, "A"))[1], "T"))
    timethreshold <- as.numeric(tempi[length(tempi)])
    if (any(is.na(ts$angle[redo1:redo2]))) {
      if (ggir_exact == TRUE) {
        # GGIR's line: window-relative positions, absolute assignment
        ts$angle[which(is.na(ts$angle[redo1:redo2]) == T)] <- 0
      } else {
        ts$angle[(redo1 - 1) + which(is.na(ts$angle[redo1:redo2]) == T)] <- 0
      }
    }
    sdl1 <- rep(0, length(ts$time[redo1:redo2]))
    # posture change of at least j degrees
    postch <- which(abs(diff(ts$angle[redo1:redo2])) > anglethreshold)
    q1 <- c()
    if (length(postch) > 1) {
      # less than once per i minutes
      q1 <- which(diff(postch) > (timethreshold * (60/epochSize)))
    }
    if (length(q1) > 0) {
      for (gi in 1:length(q1)) {
        sdl1[postch[q1[gi]]:postch[q1[gi] + 1]] <- 1 #periods with no posture change
      }
    } else {
      if (length(postch) < 5) {  #possibly a day without wearing
        sdl1[1:length(sdl1)] <- 1
      } else {  #possibly a day with constant posture changes
        sdl1[1:length(sdl1)] <- 0
      }
    }
    if (redo2 > Nts) {
      delta <- redo2 - Nts
      redo2 <- Nts
      sdl1 <- sdl1[1:(length(sdl1) - delta)]
    }
    if (redo1 > Nts) {
      redo1 <- Nts
      sdl1 <- sdl1[1]
    }
    ts$sibdetection[redo1:redo2] <- sdl1
  }
  return(ts)
}
