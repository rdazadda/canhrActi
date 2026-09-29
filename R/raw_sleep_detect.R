# Ported from GGIR 3.3-9 R/g.sib.det.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The nested helpers and four inline
# blocks are file-level internals, the M / IMP / I triple became canhrActi objects, the
# per-epoch time column stays character (as.ggir.SLE re-emits GGIR's factor), the
# external-function branch raises a named error, and LC_TIME is forced to "C". Two GGIR
# defects are kept under ggir_exact = TRUE: the partial-first-day unlist() that makes
# both SPTE values NA, and the pre-allocation that leaves the four SPTE vectors one
# element short from midnight 0.

# NESTED HELPERS

#' Re-adjust a Pair of Decimal Hours Across a Daylight Saving Transition
#'
#' GGIR's \code{dstime_handling_check}. The UTC offset of the window's first epoch is
#' compared with the offsets at the two epochs the guider selected; when they differ, the
#' signed difference in hours is added to each decimal-hour value.
#'
#' @param tmpTIME Character ISO 8601 timestamps of the night window actually sent to the guider
#'   (the shifted window when the day-sleeper re-run was accepted).
#' @param spt_estimate The guider result; only \code{SPTE_start} and \code{SPTE_end}, the epoch
#'   indices into \code{tmpTIME}, are read.
#' @param tz Olson timezone name, GGIR's desiredtz. The empty string means the system zone.
#' @param calc_SPTE_end,calc_SPTE_start The two decimal-hour values before the correction.
#' @return A numeric vector of length two, start then end.
#' @keywords internal
#' @noRd
.raw.sleep.dst.check <- function(tmpTIME = c(), spt_estimate = c(), tz = c(),
                                 calc_SPTE_end = c(), calc_SPTE_start = c()) {
  t <- tmpTIME[c(1, spt_estimate$SPTE_start, spt_estimate$SPTE_end)]
  timezone <- format(.raw.iso8601.to.posix(t, tz = tz), "%z")
  if (length(unique(timezone)) == 1) {
    return(c(calc_SPTE_start, calc_SPTE_end))
  } else {
    sign <- ifelse(substr(timezone, 1, 1) == "-", -1, 1)
    hours <- as.numeric(substr(timezone, 2, 3))
    minutes <- as.numeric(substr(timezone, 4, 5))
    offset <- sign * (hours + minutes / 60)
    offset <- offset[2:3] - offset[1]
    return(c(calc_SPTE_start + offset[1], calc_SPTE_end + offset[2]))
  }
}

#' Choose Which of Two Guider Algorithms This Night Gets
#'
#' GGIR's \code{decide_guider}. Returns 2, the fallback, only when the first algorithm is
#' "NotWorn", the night is less than 25 percent non-wear and exactly two algorithms were
#' given; otherwise 1.
#'
#' @param HASPT.algo GGIR's params_sleep member, one or two algorithm names.
#' @param nonwear_percentage Percentage of the night window flagged invalid, 0 to 100.
#' @return 1 or 2, the index into \code{HASPT.algo}.
#' @keywords internal
#' @noRd
.raw.sleep.decide.guider <- function(HASPT.algo, nonwear_percentage) {
  if (HASPT.algo[1] == "NotWorn" && nonwear_percentage < 25 && length(HASPT.algo) == 2) {
    return(2)
  } else {
    return(1)
  }
}

# INLINE BLOCKS OF g.sib.det

#' Expand the Long-Epoch Wear Decision to the Short Epoch
#'
#' Repeats each long-epoch decision \code{ws2/ws3} times through GGIR's matrix / replace /
#' transpose / dim idiom, then clips the result to the length of the short-epoch series or
#' pads it with zero, which means valid. The source is column 5 of the part-2 wear decision
#' matrix.
#'
#' @param rout The part-2 wear decision, a matrix or data frame with at least five columns, one
#'   row per long epoch.
#' @param nD Number of short epochs in the imputed series.
#' @param ws2 Long epoch length in seconds.
#' @param ws3 Short epoch length in seconds.
#' @return A numeric vector of length \code{nD}, 1 where the epoch is invalid.
#' @keywords internal
#' @noRd
.raw.sleep.invalid <- function(rout, nD, ws2, ws3) {
  if (is.null(rout) || NCOL(rout) < 5) {
    stop("the wear decision matrix rout needs five columns; g.sib.det reads column 5",
         call. = FALSE)
  }
  r5 <- as.numeric(as.matrix(rout[, 5]))
  r5long <- matrix(0, length(r5), (ws2 / ws3))
  r5long <- replace(r5long, 1:length(r5long), r5)
  r5long <- t(r5long)
  dim(r5long) <- c((length(r5) * (ws2 / ws3)), 1)
  if (nD < length(r5long)) {
    invalid <- r5long[1:nD]
  } else {
    invalid <- c(r5long, rep(0, (nD - length(r5long))))
  }
  invalid
}

#' Replace NA With Zero
#'
#' GGIR's \code{fix_NA_invector}. Applied to the angle and count series, not to the
#' acceleration metric, which reaches the detectors with its NAs intact.
#'
#' @param x A numeric vector.
#' @return \code{x} with every NA replaced by 0.
#' @keywords internal
#' @noRd
.raw.sleep.fix.na <- function(x) {
  if (length(which(is.na(x) == TRUE)) > 0) {
    x[which(is.na(x) == TRUE)] <- 0
  }
  return(x)
}

#' Fill in Marker Button Presses on the Days That Have None
#'
#' When \code{impute_marker_button} is TRUE, every real press is copied to the same clock time
#' on every other day of the recording, with a weight of \code{0.9 / (abs(j) + 1)} that falls
#' off with the distance in days, so a real press always outranks an imputed one. Presses
#' that would land on an existing press, before the start or after the end are dropped.
#'
#' @param MARKER The marker column of the imputed series, or NULL.
#' @param countmidn Number of midnights in the recording.
#' @param ws3 Short epoch length in seconds.
#' @param impute_marker_button GGIR's params_sleep member; FALSE leaves MARKER untouched.
#' @return MARKER, with the imputed presses written in.
#' @keywords internal
#' @noRd
.raw.sleep.marker.impute <- function(MARKER, countmidn, ws3, impute_marker_button) {
  if (length(MARKER) > 0) {
    # typical press times, to fall back on when a day has none
    button_pressed <- which(MARKER != 0)
    if (length(button_pressed) > 0) {
      if (impute_marker_button == TRUE) {
        newmarkers <- button_pressed
        for (j in -countmidn:countmidn) {
          newmarkers <- button_pressed + (j * 24 * (3600 / ws3))
          newmarkers <- newmarkers[which(newmarkers %in% button_pressed == FALSE &
                                           newmarkers > 0 &
                                           newmarkers <= length(MARKER))]
          if (length(newmarkers) > 0) {
            MARKER[newmarkers] <- 0.9 / (abs(j) + 1)
          }
        }
      }
    }
  }
  MARKER
}

#' Centre of the Least Active Five Hours of a Night Window, the Guider Back-Up
#'
#' GGIR's fall-back when no sleep period could be estimated: the centre of the five-hour
#' window with the lowest mean acceleration, in hours on the night's own axis, where 12 is
#' noon of the calendar date that opens the night. The \code{+ 12} is unconditional, unlike
#' the conversion of the guider's own estimate on a partial first day; that is GGIR's. A
#' window no longer than the kernel, or a flat one, returns 0 rather than a time.
#'
#' @param tmpACC The acceleration metric for one night window.
#' @param ws3 Short epoch length in seconds.
#' @return A single number: hours on the night axis, or 0 when the window is too short or flat.
#' @keywords internal
#' @noRd
.raw.sleep.l5 <- function(tmpACC, ws3) {
  windowRL <- round((3600 / ws3) * 5)
  if ((windowRL / 2) == round(windowRL / 2)) windowRL <- windowRL + 1
  if (length(tmpACC) <= windowRL) {
    L5 <- 0
  } else {
    ZRM <- zoo::rollmean(x = tmpACC, k = windowRL, fill = "extend", align = "center")
    L5 <- which(ZRM == min(ZRM))[1]
    if (stats::sd(ZRM) == 0) {
      L5 <- 0
    } else {
      L5 <- (L5 / (3600 / ws3)) + 12
    }
    if (length(L5) == 0) L5 <- 0 # no L5 because the full day is zero
  }
  L5
}

#' Turn the Guider's Epoch Indices Into Decimal Hours on the Night Axis
#'
#' Hours are measured from the midnight that opens the night's calendar date: 12 is noon of
#' that date, 24 the following midnight, 36 the next noon, and an accepted day-sleeper re-run
#' can push the wake time past 36. The offset is 12 hours, plus 6 when the day-sleeper re-run
#' was accepted. On a partial first day GGIR instead uses the recording's own start hour
#' through \code{unlist()} on a POSIXct, which is unnamed, so both SPTE values come back NA
#' while \code{tib.threshold} and \code{part3_guider} are still written. That is kept under
#' \code{ggir_exact = TRUE}; under FALSE the same expression runs on a POSIXlt and resolves.
#'
#' @param spt_estimate The guider result; \code{SPTE_start} and \code{SPTE_end} are read.
#' @param ws3 Short epoch length in seconds.
#' @param qqq1 First short-epoch index of the night window, after clamping.
#' @param partialFirstDay Whether the window was clipped to the start of the recording.
#' @param daysleep_offset 0, or 6 when the day-sleeper re-run was accepted.
#' @param rec_starttime The first timestamp of the IMPUTED series (GGIR reads
#'   \code{IMP$metashort$timestamp[1]}, not the part-1 series).
#' @param tmpTIME Character timestamps of the window sent to the guider, for the DST check.
#' @param desiredtz Olson timezone name; the empty string means the system zone.
#' @param ggir_exact TRUE keeps the partial-first-day NA defect; FALSE computes the start hour.
#' @return \code{list(SPTE_start, SPTE_end)} in decimal hours.
#' @keywords internal
#' @noRd
.raw.sleep.spte.hours <- function(spt_estimate, ws3, qqq1, partialFirstDay, daysleep_offset,
                                  rec_starttime, tmpTIME, desiredtz = "", ggir_exact = TRUE) {
  if (qqq1 == 1 && partialFirstDay == TRUE) {
    # only use startTimeRecord if the start of the block sent into SPTE was after noon
    startTimeRecord <- .raw.iso8601.to.posix(rec_starttime, tz = desiredtz)
    # unlist() on a POSIXct is unnamed, so the three names select NA (GGIR's arithmetic)
    startTimeRecord <- if (isTRUE(ggir_exact)) {
      unlist(startTimeRecord)
    } else {
      unlist(as.POSIXlt(startTimeRecord))
    }
    startTimeRecord <- sum(as.numeric(startTimeRecord[c("hour", "min", "sec")]) / c(1, 60, 3600))
    daysleep_offset <- daysleep_offset + startTimeRecord
  } else {
    daysleep_offset <- daysleep_offset + 12
  }
  SPTE_end <- (spt_estimate$SPTE_end / (3600 / ws3)) + daysleep_offset
  SPTE_start <- (spt_estimate$SPTE_start / (3600 / ws3)) + daysleep_offset
  SPTE_dst <- .raw.sleep.dst.check(tmpTIME = tmpTIME, spt_estimate = spt_estimate,
                                   tz = desiredtz,
                                   calc_SPTE_end = SPTE_end,
                                   calc_SPTE_start = SPTE_start)
  list(SPTE_start = SPTE_dst[1], SPTE_end = SPTE_dst[2])
}

# INPUT RESOLUTION

#' Resolve the Epoch Tables g.sib.det Reads From M
#'
#' @param meta A canhrActi_raw_meta, or GGIR's M list.
#' @return list(windowsizes, file, settings).
#' @keywords internal
#' @noRd
.raw.sib.detect.meta <- function(meta) {
  if (!is.list(meta) || is.null(meta$windowsizes)) {
    stop("meta must be a canhrActi_raw, a canhrActi_raw_meta from raw.getmeta(), or a GGIR M ",
         "list carrying windowsizes", call. = FALSE)
  }
  if (length(meta$windowsizes) < 2) {
    stop("meta$windowsizes must hold at least the short and the long epoch length",
         call. = FALSE)
  }
  meta
}

#' Resolve the Imputed Short-Epoch Series and the Wear Decision Matrix
#'
#' GGIR reads both from the same object, IMP, so the wear argument is only a fallback for a
#' caller that has the two apart. Part 3 must see the imputed series, not the part-1 one.
#'
#' @param imputed A canhrActi_raw_imputed from \code{\link{raw.impute}}, GGIR's IMP list, or
#'   NULL.
#' @param wear A canhrActi_raw_wear or GGIR IMP list, used only for \code{rout} when
#'   \code{imputed} has none.
#' @return list(metashort, rout).
#' @keywords internal
#' @noRd
.raw.sib.detect.imp <- function(imputed, wear = NULL) {
  if (is.null(imputed) || !is.list(imputed) || is.null(imputed$metashort)) {
    stop("imputed must be a canhrActi_raw_imputed from raw.impute() or a GGIR IMP list ",
         "carrying metashort; part 3 reads the IMPUTED series, never the part-1 one",
         call. = FALSE)
  }
  metashort <- imputed$metashort
  if (!is.data.frame(metashort)) {
    # as a data.frame, a missing count column errors instead of scoring the recording as one bout
    metashort <- as.data.frame(metashort, stringsAsFactors = FALSE)
  }
  if (!("timestamp" %in% colnames(metashort))) {
    stop("the imputed short-epoch table needs a column named \"timestamp\"", call. = FALSE)
  }
  rout <- imputed$rout
  if (is.null(rout) && !is.null(wear)) rout <- wear$rout
  if (is.null(rout)) {
    stop("no wear decision matrix: pass an imputed object carrying rout (raw.impute() does), ",
         "or a wear decision from raw.wear.decision()", call. = FALSE)
  }
  list(metashort = metashort, rout = rout)
}

#' The Sleep Parameters g.sib.det and HASPT Read, Filled From the Parameter Object
#'
#' Every member of GGIR's params_sleep, taken from \code{params} where present and from
#' \code{raw.params()}'s defaults otherwise.
#'
#' @param params A \code{\link{raw.params}} object or any named list.
#' @return A named list with every member of the sleep group.
#' @keywords internal
#' @noRd
.raw.sib.detect.sleep.params <- function(params) {
  params_sleep <- .raw.sleep.params.defaults()
  for (nm in names(params_sleep)) {
    if (nm %in% names(params)) params_sleep[nm] <- list(params[[nm]])
  }
  params_sleep
}

#' The Timezone Part 3 Works In
#'
#' The timezone recorded by part 1 wins over anything the caller passes, as GGIR's
#' \code{desiredtz_part1} does; the empty string (system zone) is honoured as stored. A GGIR
#' M list carries no such field, so for one of those the parameter object is used.
#'
#' @param meta The epoch tables.
#' @param params The parameter object.
#' @return A single character string.
#' @keywords internal
#' @noRd
.raw.sib.detect.tz <- function(meta, params) {
  if (is.list(meta$settings) && "desiredtz" %in% names(meta$settings)) {
    tz <- meta$settings$desiredtz
    if (is.character(tz) && length(tz) == 1) return(tz)
  }
  tz <- .raw.param(params, "desiredtz", "")
  if (!is.character(tz) || length(tz) != 1) tz <- ""
  tz
}

#' The Named Error That Replaces GGIR's External-Function Branch
#'
#' @param HASIB.algo The value that asked for it.
#' @keywords internal
#' @noRd
.raw.sib.detect.stop.extfun <- function(HASIB.algo) {
  stop(structure(class = c("canhrActi_raw_sleep_error", "error", "condition"),
                 list(message = paste0(
                   "HASIB.algo \"", HASIB.algo[1], "\" asks for a sustained inactivity bout ",
                   "classification computed by an external function in part 1. canhrActi does ",
                   "not carry GGIR's applyExtFunction plumbing, so g.sib.det.R:139-181 is not ",
                   "ported. Use one of the six built-in algorithms instead."),
                   call = NULL, HASIB.algo = HASIB.algo)))
}

# THE DETECTOR

#' Sustained Inactivity Bouts and the Nightly Sleep Period Guider (GGIR Part 3 Detector)
#'
#' GGIR's \code{g.sib.det} over one recording: it expands the part-2 wear decision to the
#' short epoch, classifies every epoch as a sustained inactivity bout or not
#' (\code{\link{raw.sib}}, once for the whole series), cuts the recording into noon-to-noon
#' nights anchored on each midnight, and for every night estimates the sleep period time
#' window with a guider (\code{\link{raw.guider}}), with the centre of the least active five
#' hours as a back-up.
#'
#' Every hour value it returns is hours since the midnight that opens the night's calendar
#' date: 12 is noon of that date, 24 the following midnight, 36 the next noon, and an
#' accepted day-sleeper re-run can push a wake time past 36.
#'
#' @details The too-short gate is \code{nD/n_ws3_perday > 0.2}, independent of part 1's
#'   \code{filetooshort}. The loop starts at midnight 0 when the recording began between
#'   midnight and 04:00, at \code{countmidn} when there is no 04:00 in the first day, and at
#'   1 otherwise. \code{dayborder} is 0 here whatever \code{raw.params()} says; only the
#'   sleep summary is dayborder specific. The night index is a plain loop counter that
#'   increments before the short-night skip, so a skipped night still consumes an index; the
#'   skip test uses the unclamped window start. The guider sees the first sib definition
#'   only. \code{tib.threshold} and \code{part3_guider} are written for a night whose SPTE
#'   values are NA.
#'
#'   Two GGIR defects are kept under \code{ggir_exact = TRUE}: on a partial first day the
#'   conversion to hours takes the recording's start hour through \code{unlist()} on a
#'   POSIXct, so both SPTE values come back NA; and the four per-night vectors are
#'   pre-allocated to the number of midnights while the loop can run one iteration more from
#'   midnight 0, leaving them one element short of \code{max(night)}. Part 4 indexes these
#'   vectors by night number.
#'
#'   The \code{time} column of \code{$output} is character where GGIR's is a factor;
#'   \code{\link{as.ggir.SLE}} re-emits GGIR's shape. The external-function branch
#'   (\code{HASIB.algo = "data"}) raises a named error, and GGIR's "No midnights found in
#'   file" print becomes a recorded message.
#'
#' @param meta A \code{canhrActi_raw} (its \code{$meta}, \code{$imputed} and \code{$wear} are
#'   used), a \code{canhrActi_raw_meta} from \code{\link{raw.getmeta}}, or a GGIR M list. Only
#'   \code{windowsizes} and, for a canhrActi object, the part-1 timezone are read.
#' @param imputed A \code{canhrActi_raw_imputed} from \code{\link{raw.impute}} or a GGIR IMP
#'   list. NULL runs \code{\link{raw.impute}} here. This is the series the detector works on.
#' @param wear A \code{canhrActi_raw_wear} from \code{\link{raw.wear.decision}}, used only for
#'   \code{rout} when \code{imputed} carries none, and to build the imputed series when that is
#'   NULL.
#' @param params A \code{\link{raw.params}} object, or NULL for GGIR's defaults. The members
#'   read are the whole sleep group plus \code{acc.metric}, \code{sensor.location},
#'   \code{zc.scale}, \code{desiredtz} and \code{ggir_exact}.
#' @param twd Hours before and after each midnight that make a night. GGIR hardcodes
#'   \code{c(-12, 12)} at its call site and exposes no parameter for it.
#' @param progress NULL or a \code{function(stage, i, n, message)} called once per night.
#' @param ... Individual parameter overrides, routed through \code{raw.params()}.
#'
#' @return An object of class "canhrActi_raw_sib": a list whose first eight members are GGIR's
#' \describe{
#'   \item{output}{The per-epoch frame \code{time}, \code{invalid}, \code{night}, one 0/1 column
#'     per sib definition, then \code{spt_crude_estimate}. \code{night} is 0 outside every
#'     window. The caller must drop \code{spt_crude_estimate} before
#'     \code{\link{raw.sib.summary}}, as g.part3 does.}
#'   \item{detection.failed}{TRUE when the recording was too short or had no midnight.}
#'   \item{L5list}{Centre of the least active five hours per night, decimal hours.}
#'   \item{SPTE_start, SPTE_end}{The guider window per night, decimal hours.}
#'   \item{tib.threshold}{The threshold the guider used per night.}
#'   \item{longitudinal_axis}{Echoed from the parameters; NULL for a wrist recording.}
#'   \item{part3_guider}{The guider that produced each night, with "+invalid" appended when
#'     invalid time was taken into the window.}
#' }
#' and then the canhrActi additions: \code{windows} (one row per loop iteration, with the
#' window bounds before and after clamping, the timestamps, the epoch and invalid counts, the
#' non-wear percentage, the guider index, the partial-first-day flag and the day-sleeper
#' offset), \code{rec_starttime} (the first timestamp of the imputed series, which part 4
#' aligns a diary against), \code{midnights}, \code{midn_start}, \code{definitions},
#' \code{n_short}, \code{n_invalid_short}, \code{desiredtz}, \code{status}, \code{file},
#' \code{settings}, \code{messages} and \code{elapsed}.
#'
#' @seealso \code{\link{raw.sib}} for the epoch classifier, \code{\link{raw.guider}} for the
#'   per-night guider, \code{\link{raw.sib.summary}} for the sib period table and
#'   \code{\link{raw.impute}} for the series this reads.
#'
#' @examples
#' \dontrun{
#' x <- read.raw.accelerometer(path)
#' imp <- raw.impute(x)
#' sib <- raw.sib.detect(x$meta, imp, x$wear)
#' sib$SPTE_start
#' epochs <- sib$output[, -which(names(sib$output) == "spt_crude_estimate")]
#' raw.sib.summary(list(output = epochs), x$meta, desiredtz = sib$desiredtz)
#' }
#' @export
raw.sib.detect <- function(meta, imputed = NULL, wear = NULL, params = NULL,
                           twd = c(-12, 12), progress = NULL, ...) {
  t0 <- Sys.time()
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be NULL or a function(stage, i, n, message)", call. = FALSE)
  }
  if (!is.numeric(twd) || length(twd) != 2) {
    stop("twd must be two numbers, the hours before and after midnight; GGIR hardcodes ",
         "c(-12, 12)", call. = FALSE)
  }
  if (inherits(meta, "canhrActi_raw")) {
    if (is.null(imputed)) imputed <- meta$imputed
    if (is.null(wear)) wear <- meta$wear
    meta <- meta$meta
  }
  meta <- .raw.sib.detect.meta(meta)
  params <- .raw.calibrate.params(list(params = NULL), params, list(...))
  messages <- character()
  ggir_exact <- isTRUE(.raw.param(params, "ggir_exact", TRUE))
  params_sleep <- .raw.sib.detect.sleep.params(params)
  acc.metric <- .raw.param(params, "acc.metric", "ENMO")
  sensor.location <- .raw.param(params, "sensor.location", "wrist")
  zc.scale <- .raw.param(params, "zc.scale", 1)
  desiredtz <- .raw.sib.detect.tz(meta, params)
  if (is.null(imputed)) imputed <- raw.impute(meta, wear, params)
  imp <- .raw.sib.detect.imp(imputed, wear)
  metashort <- imp$metashort
  nD <- nrow(metashort)
  ws3 <- meta$windowsizes[1] # short epoch
  ws2 <- meta$windowsizes[2] # long epoch, used for non-wear detection
  n_ws2_perday <- (1440 * 60) / ws2 # never read, as in GGIR
  n_ws3_perday <- (1440 * 60) / ws3
  invalid <- .raw.sleep.invalid(imp$rout, nD, ws2, ws3)
  ND <- nD / n_ws3_perday # number of days
  windows <- NULL
  definitions <- NULL
  midnightsi <- NULL
  midn_start <- NA_integer_
  status <- "ok"
  rec_starttime <- if (nD > 0) metashort$timestamp[1] else NA_character_
  if (ND > 0.2) {
    time <- format(metashort[, 1])
    anglez <- as.numeric(as.matrix(metashort[, which(colnames(metashort) == "anglez")]))
    anglez <- .raw.sleep.fix.na(anglez)
    anglex <- angley <- c()
    do.HASPT.hip <- FALSE
    if (sensor.location == "hip" &
        "anglex" %in% colnames(metashort) &
        "angley" %in% colnames(metashort) &
        "anglez" %in% colnames(metashort)) {
      do.HASPT.hip <- TRUE
      if (length(colnames(metashort) == "anglex") > 0) {
        anglex <- metashort[, which(colnames(metashort) == "anglex")]
        anglex <- .raw.sleep.fix.na(anglex)
      }
      if (length(colnames(metashort) == "angley") > 0) {
        angley <- metashort[, which(colnames(metashort) == "angley")]
        angley <- .raw.sleep.fix.na(angley)
      }
    }
    if (acc.metric %in% colnames(metashort) == FALSE) {
      if ("ExtAct" %in% colnames(metashort) == TRUE) {
        acc.metric <- "ExtAct"
      } else {
        stop("Argument acc.metric is set to ", acc.metric,
             " but not found in GGIR part 1 output data", call. = FALSE)
      }
    }
    ACC <- as.numeric(as.matrix(metashort[, which(colnames(metashort) == acc.metric)]))
    night <- spt_crude_estimate <- rep(0, length(ACC))

    if ("marker" %in% colnames(metashort)) {
      MARKER <- as.numeric(as.matrix(metashort[, which(colnames(metashort) == "marker")]))
    } else {
      MARKER <- NULL
    }
    sib_90s_algo_names <- c("Sadeh1994", "Galland2012", "ColeKripke1992", "Oakley1997")
    if (any(params_sleep[["HASIB.algo"]] %in% sib_90s_algo_names)) { # count-based algorithms
      if (params_sleep[["Sadeh_axis"]] %in% c("X", "Y", "Z") == FALSE) {
        params_sleep[["Sadeh_axis"]] <- "Z"
      }
      count_column_index <- which(colnames(metashort) %in%
                                    c(paste0("ZC", params_sleep[["Sadeh_axis"]]),
                                      "ExtAct"))[1]
      zeroCrossingCount <- metashort[, count_column_index]
      zeroCrossingCount <- .raw.sleep.fix.na(zeroCrossingCount)
      zeroCrossingCount <- zeroCrossingCount * zc.scale
      NeishabouriCount_colname <- paste0("NeishabouriCount_",
                                         tolower(params_sleep[["Sadeh_axis"]]))
      if (NeishabouriCount_colname %in% colnames(metashort)) {
        NeishabouriCount <- metashort[, NeishabouriCount_colname]
        NeishabouriCount <- .raw.sleep.fix.na(NeishabouriCount)
      } else {
        NeishabouriCount <- c()
      }
    } else {
      zeroCrossingCount <- c()
      NeishabouriCount <- c()
    }
    # sustained inactivity bouts, once for the whole series; the external-function branch is not ported
    if (identical(params_sleep[["HASIB.algo"]][1], "data")) {
      .raw.sib.detect.stop.extfun(params_sleep[["HASIB.algo"]])
    }
    sleep <- raw.sib(HASIB.algo = params_sleep[["HASIB.algo"]],
                     timethreshold = params_sleep[["timethreshold"]],
                     anglethreshold = params_sleep[["anglethreshold"]],
                     time = time, anglez = anglez, ws3 = ws3,
                     zeroCrossingCount = zeroCrossingCount,
                     NeishabouriCount = NeishabouriCount, activity = ACC,
                     oakley_threshold = params_sleep[["oakley_threshold"]])
    # midnights; dayborder is always 0 for sleep, only the summary is dayborder specific
    detemout <- .raw.detect.midnight(time, desiredtz, dayborder = 0)
    midnights <- detemout$midnights
    midnightsi <- detemout$midnightsi
    countmidn <- length(midnightsi)
    # logical NA, which stays logical when no night produces an estimate, as in GGIR's milestone
    tib.threshold <- SPTE_end <- SPTE_start <- L5list <- part3_guider <- rep(NA, countmidn)
    if (countmidn != 0) {
      # only firstmidnighti is read again; the rest are GGIR's own assignments
      if (countmidn == 1) {
        tooshort <- 1
        lastmidnight <- midnights[length(midnights)]
        lastmidnighti <- midnightsi[length(midnights)]
        firstmidnight <- time[1]
        firstmidnighti <- midnightsi[1]
      } else {
        cut <- which(as.numeric(midnightsi) == 0)
        if (length(cut) > 0) {
          midnights <- midnights[-cut]
          midnightsi <- midnightsi[-cut]
        }
        lastmidnight <- midnights[length(midnights)]
        lastmidnighti <- midnightsi[length(midnights)]
        firstmidnight <- midnights[1]
        firstmidnighti <- midnightsi[1]
      }
      # a recording that started before 4am also gets the first awakening
      first4am <- grep("04:00:00", time[1:pmin(nD, (n_ws3_perday + 1))])[1]
      if (!is.na(first4am)) {
        if (first4am < firstmidnighti) { # started after midnight and before 4am
          midn_start <- 0
        } else {
          midn_start <- 1
        }
      } else {
        # no 4am in the first day: skip this midnight
        midn_start <- countmidn
      }
      nwin <- countmidn - midn_start + 1
      if (!ggir_exact && nwin > countmidn) {
        # GGIR pre-allocates countmidn slots but the loop runs nwin times from midnight 0
        tib.threshold <- SPTE_end <- SPTE_start <- L5list <- part3_guider <- rep(NA, nwin)
      }
      win <- list(night = rep(NA_real_, nwin), midnight = rep(NA_real_, nwin),
                  qqq1_unclamped = rep(NA_real_, nwin), qqq2_unclamped = rep(NA_real_, nwin),
                  qqq1 = rep(NA_real_, nwin), qqq2 = rep(NA_real_, nwin),
                  skipped = rep(NA, nwin), start.time = rep(NA_character_, nwin),
                  end.time = rep(NA_character_, nwin), n_epochs = rep(NA_real_, nwin),
                  n_invalid = rep(NA_real_, nwin), nonwear_percentage = rep(NA_real_, nwin),
                  guider_to_use = rep(NA_real_, nwin),
                  partial_first_day = rep(NA, nwin),
                  daysleep_offset = rep(NA_real_, nwin))
      sptei <- 0
      MARKER <- .raw.sleep.marker.impute(MARKER, countmidn, ws3,
                                         params_sleep[["impute_marker_button"]])
      for (j in midn_start:(countmidn)) {
        if (j == 0) {
          qqq1 <- 1 # preceding noon, not in the recording
          qqq2 <- midnightsi[1] + (twd[1] * (3600 / ws3)) # first noon in the recording
        } else {
          qqq1 <- midnightsi[j] + (twd[1] * (3600 / ws3)) + 1 # preceding noon
          qqq2 <- midnightsi[j] + (twd[2] * (3600 / ws3)) # next noon
        }
        # twd assumes a 24 hour window, which a DST day is not
        if (qqq2 < length(time) & qqq2 > 0) {
          qqq2_hour <- as.numeric(format(.raw.iso8601.to.posix(time[qqq2], tz = desiredtz),
                                         "%H"))
          if (qqq2_hour == 11) {
            qqq2 <- qqq2 + (3600 / ws3)
          } else if (qqq2_hour == 13) {
            qqq2 <- qqq2 - (3600 / ws3)
          }
        }
        sptei <- sptei + 1
        if (!is.null(progress)) {
          progress("sib_detect", as.integer(sptei), as.integer(nwin),
                   paste0("Night ", sptei, " of ", nwin))
        }
        win$night[sptei] <- sptei
        win$midnight[sptei] <- j
        win$qqq1_unclamped[sptei] <- qqq1
        win$qqq2_unclamped[sptei] <- qqq2
        if (qqq2 - qqq1 < 60) { # skip a night of fewer than 60 epochs
          win$skipped[sptei] <- TRUE
          next
        }
        win$skipped[sptei] <- FALSE
        if (qqq2 > length(time))  qqq2 <- length(time)
        if (qqq1 < 1)             qqq1 <- 1
        if (qqq1 == 1 && qqq2 != 24 * 3600 / ws3) {
          partialFirstDay <- TRUE
        } else {
          partialFirstDay <- FALSE
        }
        tSegment <- qqq1:qqq2
        night[tSegment] <- sptei
        win$qqq1[sptei] <- qqq1
        win$qqq2[sptei] <- qqq2
        win$start.time[sptei] <- time[qqq1]
        win$end.time[sptei] <- time[qqq2]
        win$n_epochs[sptei] <- qqq2 - qqq1 + 1
        win$n_invalid[sptei] <- length(which(invalid[tSegment] == 1))
        detection.failed <- FALSE
        nonwear_percentage <- (length(which(invalid[tSegment] == 1)) / (qqq2 - qqq1 + 1)) * 100
        guider_to_use <- .raw.sleep.decide.guider(params_sleep[["HASPT.algo"]],
                                                  nonwear_percentage)
        # L5 as the back-up
        tmpACC <- ACC[tSegment]
        L5 <- .raw.sleep.l5(tmpACC, ws3)
        L5list[sptei] <- L5
        # sleep period time window, which part 4 uses when there is no diary
        tmpANGLE <- anglez[tSegment]
        tmpTIME <- time[tSegment]
        daysleep_offset <- 0
        win$daysleep_offset[sptei] <- daysleep_offset
        if (do.HASPT.hip == TRUE & params_sleep[["HASPT.algo"]][guider_to_use] != "NotWorn") {
          if (params_sleep[["longitudinal_axis"]] == 1) {
            tmpANGLE <- anglex[tSegment]
          } else if (params_sleep[["longitudinal_axis"]] == 2) {
            tmpANGLE <- angley[tSegment]
          }
        }
        if (length(params_sleep[["def.noc.sleep"]]) == 1) {
          spt_estimate <- raw.guider(angle = tmpANGLE, ws3 = ws3,
                                     params_sleep = params_sleep,
                                     HASPT.algo = params_sleep[["HASPT.algo"]][guider_to_use],
                                     invalid = invalid[tSegment],
                                     activity = tmpACC,
                                     marker = MARKER[tSegment],
                                     sibs = sleep[tSegment, 1],
                                     ggir_exact = ggir_exact)
        } else {
          spt_estimate <- list(SPTE_end = NULL, SPTE_start = NULL, tib.threshold = NULL,
                               part3_guider = NULL)
        }
        tSegment_backup <- tSegment
        if (length(spt_estimate$SPTE_end) != 0 & length(spt_estimate$SPTE_start) != 0) {
          if (spt_estimate$SPTE_end + qqq1 >= qqq2 - (1 * (3600 / ws3))) {
            # SPT ends within an hour of noon: re-run on a window moved 6 h forward, for daysleepers
            daysleep_offset <- 6
            newqqq1 <- qqq1 + (daysleep_offset * (3600 / ws3))
            newqqq2 <- qqq2 + (daysleep_offset * (3600 / ws3))
            if (qqq1 == 1 && newqqq2 - newqqq1 < (24 * 3600) / ws3 &&
                newqqq2 > (24 * 3600) / ws3) {
              newqqq1 <- newqqq2 - (24 * 3600) / ws3
              partialFirstDay <- FALSE
            }
            if (newqqq2 > length(anglez)) newqqq2 <- length(anglez)
            # only when the new window is longer than 23 h
            if (newqqq2 < length(anglez) & (newqqq2 - newqqq1) > (23 * (3600 / ws3))) {
              tSegment <- newqqq1:newqqq2
              nonwear_percentage <- (length(which(invalid[tSegment] == 1)) /
                                       (newqqq2 - newqqq1 + 1)) * 100
              guider_to_use <- .raw.sleep.decide.guider(params_sleep[["HASPT.algo"]],
                                                        nonwear_percentage)
              tmpTIME <- time[tSegment]
              if (params_sleep[["HASPT.algo"]][guider_to_use] != "NotWorn") {
                tmpANGLE <- anglez[tSegment]
                if (do.HASPT.hip == TRUE) {
                  if (params_sleep[["longitudinal_axis"]] == 1) {
                    tmpANGLE <- anglex[tSegment]
                  } else if (params_sleep[["longitudinal_axis"]] == 2) {
                    tmpANGLE <- angley[tSegment]
                  }
                }
              }
              spt_estimate_tmp <- raw.guider(angle = tmpANGLE, ws3 = ws3,
                                             params_sleep = params_sleep,
                                             HASPT.algo = params_sleep[["HASPT.algo"]][guider_to_use],
                                             invalid = invalid[tSegment],
                                             activity = ACC[tSegment],
                                             sibs = sleep[tSegment, 1],
                                             ggir_exact = ggir_exact)

              if (length(spt_estimate_tmp$SPTE_start) > 0) {
                # keep the re-run only when its SPTE_end is past noon
                if (spt_estimate_tmp$SPTE_end + newqqq1 >= qqq2) {
                  spt_estimate <- spt_estimate_tmp
                } else {
                  daysleep_offset <- 0
                }
              } else {
                daysleep_offset <- 0
              }
            } else {
              daysleep_offset <- 0
            }
          }
          win$daysleep_offset[sptei] <- daysleep_offset
          spte <- .raw.sleep.spte.hours(spt_estimate = spt_estimate, ws3 = ws3, qqq1 = qqq1,
                                        partialFirstDay = partialFirstDay,
                                        daysleep_offset = daysleep_offset,
                                        rec_starttime = metashort$timestamp[1],
                                        tmpTIME = tmpTIME, desiredtz = desiredtz,
                                        ggir_exact = ggir_exact)
          # assigning into the pre-allocated vectors strips the "10%" name a quantile() threshold carries
          SPTE_start[sptei] <- spte$SPTE_start
          SPTE_end[sptei] <- spte$SPTE_end
          tib.threshold[sptei] <- spt_estimate$tib.threshold
          part3_guider[sptei] <- spt_estimate$part3_guider
        }
        # crude estimate written back into the series
        if (length(spt_estimate$spt_crude_estimate) == length(tSegment)) {
          spt_crude_estimate[tSegment] <- spt_estimate$spt_crude_estimate
        } else if (length(spt_estimate$spt_crude_estimate) == length(tSegment_backup)) {
          spt_crude_estimate[tSegment_backup] <- spt_estimate$spt_crude_estimate
        } else {
          if (!is.null(spt_estimate$spt_crude_estimate)) {
            warning("Crude estimate of sleep has unexpected length, please contact GGIR maintainer.")
          }
        }
        win$nonwear_percentage[sptei] <- nonwear_percentage
        win$guider_to_use[sptei] <- guider_to_use
        win$partial_first_day[sptei] <- partialFirstDay
      }
      detection.failed <- FALSE
      windows <- data.frame(win, stringsAsFactors = FALSE)
    } else {
      messages <- c(messages, "No midnights found in file")
      detection.failed <- TRUE
      status <- "no_midnights"
    }
    metatmp <- data.frame(time, invalid, night = night, sleep = sleep,
                          stringsAsFactors = FALSE)
    if (!is.null(spt_crude_estimate)) {
      metatmp$spt_crude_estimate <- spt_crude_estimate
    }
    definitions <- setdiff(colnames(metatmp), c("time", "invalid", "night",
                                                "spt_crude_estimate"))
  } else {
    metatmp <- L5list <- SPTE_end <- SPTE_start <- tib.threshold <- part3_guider <- NULL
    detection.failed <- TRUE
    status <- "too_short"
    messages <- c(messages,
                  paste0("The imputed series holds ", nD, " short epochs, ", signif(ND, 4),
                         " days; g.sib.det needs more than 0.2 days. Nothing detected."))
  }
  structure(list(output = metatmp, detection.failed = detection.failed, L5list = L5list,
                 SPTE_start = SPTE_start, SPTE_end = SPTE_end,
                 tib.threshold = tib.threshold,
                 longitudinal_axis = params_sleep[["longitudinal_axis"]],
                 part3_guider = part3_guider,
                 windows = windows, rec_starttime = rec_starttime,
                 midnights = midnightsi, midn_start = midn_start,
                 definitions = definitions,
                 n_short = nD, n_invalid_short = length(which(invalid == 1)),
                 desiredtz = desiredtz, status = status, file = meta$file,
                 settings = list(windowsizes = meta$windowsizes, twd = twd,
                                 acc.metric = acc.metric, sensor.location = sensor.location,
                                 zc.scale = zc.scale, desiredtz = desiredtz,
                                 ggir_exact = ggir_exact, params_sleep = params_sleep),
                 messages = messages,
                 elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs"))),
            class = "canhrActi_raw_sib")
}

# GGIR SHAPES AND PRINTING

#' GGIR's SLE List From a canhrActi Sustained Inactivity Bout Object
#'
#' The eight members g.sib.det returns, in GGIR's order (SPTE_end before SPTE_start) and
#' with GGIR's types, so that \code{identical()} against a live \code{g.sib.det} call works.
#' The only conversion is the \code{time} column of \code{output}, a factor in GGIR.
#'
#' @param x A canhrActi_raw_sib from \code{\link{raw.sib.detect}}, or a canhrActi_raw carrying
#'   one in \code{$sleep$sib}.
#' @return A named list in GGIR's member order.
#' @examples
#' \dontrun{
#' SLE <- as.ggir.SLE(raw.sib.detect(x$meta, imp, x$wear))
#' identical(SLE, GGIR:::g.sib.det(M, IMP, I))
#' }
#' @export
as.ggir.SLE <- function(x) {
  if (inherits(x, "canhrActi_raw")) x <- x$sleep$sib
  if (!inherits(x, "canhrActi_raw_sib")) {
    stop("x must be a canhrActi_raw_sib object from raw.sib.detect()", call. = FALSE)
  }
  output <- x$output
  if (!is.null(output) && is.character(output$time)) {
    # GGIR builds the frame with stringsAsFactors = TRUE
    output$time <- factor(output$time)
  }
  list(output = output, detection.failed = x$detection.failed, L5list = x$L5list,
       SPTE_end = x$SPTE_end, SPTE_start = x$SPTE_start, tib.threshold = x$tib.threshold,
       longitudinal_axis = x$longitudinal_axis, part3_guider = x$part3_guider)
}

#' Print Method for a Sustained Inactivity Bout Object
#'
#' @param x A canhrActi_raw_sib.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_sib <- function(x, ...) {
  cat("\ncanhrActi sustained inactivity bouts and nightly guider (GGIR part 3: g.sib.det)\n")
  if (!is.null(x$file$filename) && !is.na(x$file$filename)) {
    cat("  file:        ", x$file$filename, "\n", sep = "")
  }
  if (!identical(x$status, "ok")) {
    cat("  status:      ", x$status, "\n", sep = "")
  }
  if (isTRUE(x$detection.failed) || is.null(x$output)) {
    cat("  detection:   failed, no nights\n")
  } else {
    ws3 <- x$settings$windowsizes[1]
    cat("  epochs:      ", x$n_short, " short (", ws3, " s), ", x$n_invalid_short,
        " invalid\n", sep = "")
    cat("  definitions: ", paste(x$definitions, collapse = ", "), "\n", sep = "")
    nnights <- if (is.null(x$windows)) 0 else nrow(x$windows)
    nskip <- if (is.null(x$windows)) 0 else length(which(x$windows$skipped))
    cat("  nights:      ", nnights, " noon-to-noon windows from ", length(x$midnights),
        " midnights", if (nskip > 0) paste0(", ", nskip, " too short and skipped") else "",
        "\n", sep = "")
    fmt <- function(v) {
      if (is.null(v)) return("none")
      paste(ifelse(is.na(v), "NA", format(round(as.numeric(v), 3), trim = TRUE)),
            collapse = " ")
    }
    cat("  SPTE start:  ", fmt(x$SPTE_start), "\n", sep = "")
    cat("  SPTE end:    ", fmt(x$SPTE_end), "\n", sep = "")
    cat("  L5:          ", fmt(x$L5list), "\n", sep = "")
    cat("  guider:      ", paste(ifelse(is.na(x$part3_guider), "NA", x$part3_guider),
                                 collapse = " "), "\n", sep = "")
  }
  if (length(x$messages) > 0) {
    cat("  messages:\n")
    for (m in x$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(x$elapsed) && !is.na(x$elapsed)) {
    cat("  elapsed:     ", round(x$elapsed, 2), " s\n", sep = "")
  }
  invisible(x)
}
