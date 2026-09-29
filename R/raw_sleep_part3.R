# Ported from GGIR 3.3-9 R/g.part3.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Only the per-file body of main_part3
# is ported. Not reproduced: the milestone folder machinery, the parallel branch and the
# pdf block. The loaded milestones are members of the canhrActi_raw, the save() is the
# returned object, the silent no-output paths become a state on that object, and LC_TIME
# is forced to "C" for the whole call, which GGIR does only in its parallel workers.

# PRE-STEPS

#' Longitudinal Axis for a Hip Recording
#'
#' Re-uses the longitudinal axis part 2 estimated, unless the user named one, and falls
#' back to 2 (the y axis) when part 2 could not estimate one. Both the empty string and
#' NA mean no estimate. Untested against a reference: no hip recording is available.
#'
#' @param if_hip_long_axis_id Part 2's estimate: 1, 2, 3, \code{""} or \code{NA}.
#' @return 1, 2 or 3.
#' @keywords internal
#' @noRd
.raw.sleep.part3.hip.axis <- function(if_hip_long_axis_id) {
  if (length(if_hip_long_axis_id) != 1) return(2)
  v <- if_hip_long_axis_id
  if (is.character(v)) {
    if (!nzchar(v)) return(2) # g.analyse's "no estimate"
    v <- suppressWarnings(as.numeric(v))
  }
  if (is.na(v)) return(2)
  v
}

#' Sleep Regularity Index Gate of Part 3
#'
#' The index is computed only when the series is longer than two days of short epochs;
#' otherwise the result is the scalar \code{NA}, which part 4 and the reports tolerate.
#' The gate runs after \code{spt_crude_estimate} has been dropped, so the first sustained
#' inactivity definition is the first column \code{\link{raw.sleep.regularity}} does not
#' recognise by name.
#'
#' @param output The per-epoch frame after the \code{spt_crude_estimate} drop, or NULL.
#' @param epochsize Short epoch length in seconds.
#' @param desiredtz Olson timezone name; the empty string means the system zone.
#' @param SRI1_smoothing_wsize_hrs,SRI1_smoothing_frac GGIR's smoothing parameters, both
#'   NULL by default and both needed for smoothing to run.
#' @return A data frame of one row per consecutive day pair, or the scalar \code{NA}.
#' @keywords internal
#' @noRd
.raw.sleep.part3.sri <- function(output, epochsize, desiredtz,
                                 SRI1_smoothing_wsize_hrs = NULL,
                                 SRI1_smoothing_frac = NULL) {
  if (!is.null(output)) {
    # only calculate SRI if there are at least two days of data
    if (nrow(output) > 2 * 24 * (3600 / epochsize)) {
      SleepRegularityIndex <- raw.sleep.regularity(
        data = output, epochsize = epochsize, desiredtz = desiredtz,
        SRI1_smoothing_wsize_hrs = SRI1_smoothing_wsize_hrs,
        SRI1_smoothing_frac = SRI1_smoothing_frac)
    } else {
      SleepRegularityIndex <- NA
    }
  } else {
    SleepRegularityIndex <- NA
  }
  SleepRegularityIndex
}

#' Per-Night Table Including the Nights sib.cla.sum Omits
#'
#' One row per night the detector looked at, whether or not it produced a sustained
#' inactivity bout. GGIR's own per-night record is \code{sib.cla.sum}, which drops every
#' night with no sib period; this table has no GGIR counterpart.
#'
#' @details \code{fraction_night_invalid} uses g.sib.sum's formula,
#'   \code{(invalid + max(0, NepochsInDay - n_epochs))/NepochsInDay} clamped at 1, so a
#'   night with no \code{sib.cla.sum} row still carries one. The five per-night vectors can
#'   be shorter than the number of nights (GGIR pre-allocates them to the number of
#'   midnights while the loop can run one iteration more), so a night past their end
#'   reports NA.
#'
#' @param windows The detector's per-iteration table, \code{$windows} of a canhrActi_raw_sib.
#' @param sib.cla.sum The sib period table, or NULL.
#' @param L5list,SPTE_start,SPTE_end,tib.threshold,part3_guider The five per-night vectors.
#' @param definition The sustained inactivity definition the counts are taken from, normally
#'   the first, which is the only one the guider sees.
#' @param ws3 Short epoch length in seconds.
#' @return A data frame with one row per night, or NULL when there are no nights.
#' @keywords internal
#' @noRd
.raw.sleep.part3.nights <- function(windows, sib.cla.sum, L5list, SPTE_start, SPTE_end,
                                    tib.threshold, part3_guider, definition, ws3) {
  has_sib <- is.data.frame(sib.cla.sum) && nrow(sib.cla.sum) > 0
  nights_sib <- if (has_sib) sort(unique(as.numeric(sib.cla.sum$night))) else numeric()
  has_win <- is.data.frame(windows) && nrow(windows) > 0
  nights_win <- if (has_win) as.numeric(windows$night) else numeric()
  nn <- sort(unique(c(nights_win, nights_sib)))
  if (length(nn) == 0) return(NULL)
  at <- function(v, i, empty = NA_real_) {
    if (is.na(i) || i < 1 || length(v) < i) return(empty)
    v[i]
  }
  winrow <- function(k) {
    if (!has_win) return(NA_integer_)
    i <- which(windows$night == k)
    if (length(i) == 0) NA_integer_ else i[1]
  }
  NepochsInDay <- (60 / ws3) * 1440
  out <- vector("list", length(nn))
  for (q in seq_along(nn)) {
    k <- nn[q]
    wi <- winrow(k)
    n_epochs <- if (is.na(wi)) NA_real_ else windows$n_epochs[wi]
    n_invalid <- if (is.na(wi)) NA_real_ else windows$n_invalid[wi]
    if (!is.na(n_epochs) && !is.na(n_invalid)) {
      Nmissingvalues <- max(c(0, (NepochsInDay - n_epochs)))
      frac <- (n_invalid + Nmissingvalues) / NepochsInDay
      if (frac > 1) frac <- 1
    } else if (has_sib && k %in% nights_sib) {
      frac <- sib.cla.sum$fraction.night.invalid[which(sib.cla.sum$night == k)[1]]
    } else {
      frac <- NA_real_
    }
    sub <- if (has_sib) {
      sib.cla.sum[which(sib.cla.sum$night == k &
                          as.character(sib.cla.sum$definition) == definition), , drop = FALSE]
    } else {
      NULL
    }
    empty_sub <- is.null(sub) || nrow(sub) == 0
    out[[q]] <- data.frame(
      night = as.numeric(k),
      midnight = if (is.na(wi)) NA_real_ else windows$midnight[wi],
      date = if (is.na(wi) || is.na(windows$start.time[wi])) {
        NA_character_
      } else {
        substr(windows$start.time[wi], 1, 10)
      },
      start.time = if (is.na(wi)) NA_character_ else windows$start.time[wi],
      end.time = if (is.na(wi)) NA_character_ else windows$end.time[wi],
      n_epochs = n_epochs,
      n_invalid = n_invalid,
      fraction_night_invalid = frac,
      nonwear_percentage = if (is.na(wi)) NA_real_ else windows$nonwear_percentage[wi],
      skipped = if (is.na(wi)) NA else windows$skipped[wi],
      partial_first_day = if (is.na(wi)) NA else windows$partial_first_day[wi],
      daysleep_offset = if (is.na(wi)) NA_real_ else windows$daysleep_offset[wi],
      guider_to_use = if (is.na(wi)) NA_real_ else windows$guider_to_use[wi],
      guider = as.character(at(part3_guider, k, NA_character_)),
      SPTE_start = as.numeric(at(SPTE_start, k)),
      SPTE_end = as.numeric(at(SPTE_end, k)),
      tib.threshold = as.numeric(at(tib.threshold, k)),
      L5 = as.numeric(at(L5list, k)),
      definition = definition,
      n_sib_periods = if (empty_sub) 0 else as.numeric(sub$nsib.periods[1]),
      sib_dur_hrs = if (empty_sub) 0 else sum(sub$tot.sib.dur.hrs),
      in_sib_cla_sum = k %in% nights_sib,
      stringsAsFactors = FALSE)
  }
  nights <- do.call(rbind, out)
  row.names(nights) <- NULL
  nights
}

#' Resolve the Imputed Series Part 3 Works On
#'
#' Part 3 reads the part-2 imputed short-epoch series, never part 1's. Returns the
#' recording's own \code{$imputed} when it has one and runs \code{\link{raw.impute}} when
#' it does not.
#'
#' @param imputed The recording's \code{$imputed}: a canhrActi_raw_imputed, a GGIR IMP list,
#'   or NULL.
#' @param meta The recording's \code{$meta}.
#' @param wear The recording's \code{$wear}.
#' @param params The resolved parameter object.
#' @return list(imputed, computed); computed says whether raw.impute() had to run.
#' @keywords internal
#' @noRd
.raw.sleep.part3.imputed <- function(imputed, meta, wear, params) {
  if (!is.null(imputed) && (inherits(imputed, "canhrActi_raw_imputed") ||
                            (is.list(imputed) && "metashort" %in% names(imputed)))) {
    return(list(imputed = imputed, computed = FALSE))
  }
  list(imputed = raw.impute(meta, wear, params), computed = TRUE)
}

# PART 3

#' GGIR Part 3: Sustained Inactivity Bouts and the Nightly Sleep Period Guider
#'
#' Runs GGIR's part 3 over one recording and returns the objects GGIR would have saved into
#' \code{meta/ms3.out}, plus the per-epoch series GGIR discards at that save and a complete
#' per-night table. Attach the result to the recording the way \code{$wear} and
#' \code{$imputed} are attached: \code{x$sleep <- raw.sleep.part3(x)}.
#'
#' @details The stages, in GGIR's order: the timezone override (part 1's timezone wins over
#'   the caller's, and the empty string means the system zone), the corrupt and too-short
#'   gate, the hip longitudinal axis pre-step, the detector (\code{\link{raw.sib.detect}},
#'   with the noon-to-noon window \code{twd = c(-12, 12)} hardcoded as in GGIR), the
#'   optional guider correction (\code{\link{raw.guider.correct}}, off by default), the
#'   \code{spt_crude_estimate} drop, the Sleep Regularity Index
#'   (\code{\link{raw.sleep.regularity}}) and the save gate, inside which the sib period
#'   table (\code{\link{raw.sib.summary}}) and \code{rec_starttime} (the first timestamp of
#'   the imputed series) are produced.
#'
#'   Where GGIR writes no ms3 file and logs nothing (a corrupt or too-short recording, a
#'   recording under 0.2 days, a failed detection) the result carries a \code{$status$state}
#'   of "skipped", "too_short_for_sleep", "no_midnights" or "detection_failed"; it is "ok"
#'   only when GGIR would have written the file, and \code{sib.cla.sum} and
#'   \code{rec_starttime} are NULL otherwise. No PDF is drawn. \code{GGIRversion} is not
#'   reproduced; \code{\link{as.ggir.ms3}} writes the installed GGIR's version, so parity
#'   tests must not assert on it. LC_TIME is forced to "C" for the duration of the call so
#'   that \code{SleepRegularityIndex$weekday} does not depend on the machine.
#'
#' @param x A \code{canhrActi_raw} from \code{\link{read.raw.accelerometer}} or
#'   \code{\link{read.ggir.milestone}}. Its \code{$imputed} is the series part 3 works on;
#'   when it carries none, \code{\link{raw.impute}} is run here and a message says so. Its
#'   \code{$tz$desiredtz} is part 1's timezone and overrides the caller's.
#' @param params A \code{\link{raw.params}} object, or NULL to use the recording's own
#'   (\code{x$params}). The members read are the sleep group plus \code{acc.metric},
#'   \code{sensor.location}, \code{zc.scale}, \code{desiredtz}, \code{do.part3.pdf} and
#'   \code{ggir_exact}.
#' @param progress NULL or a \code{function(stage, i, n, message)}. Called once per stage with
#'   the stage name "part3", and once per night by the detector with "sib_detect".
#' @param ... Individual parameter overrides, routed through \code{raw.params()}.
#'
#' @return An object of class "canhrActi_raw_sleep": a list whose first members are the ones
#'   GGIR saves into \code{meta/ms3.out},
#' \describe{
#'   \item{sib.cla.sum}{The sib period table, one row per (night, definition, period), or NULL
#'     when GGIR would have written no file. Nights with no sib period are absent from it.}
#'   \item{L5list}{Centre of the least active five hours per night, decimal hours.}
#'   \item{SPTE_start, SPTE_end}{The guider window per night, decimal hours on the axis that
#'     starts at the midnight opening the night's calendar date.}
#'   \item{tib.threshold}{The threshold the guider used per night.}
#'   \item{part3_guider}{The guider that produced each night.}
#'   \item{SPTE_corrected}{NULL unless the guider correction ran.}
#'   \item{SleepRegularityIndex}{One row per consecutive day pair, or the scalar NA.}
#'   \item{longitudinal_axis}{NULL for a wrist recording.}
#'   \item{id}{GGIR's \code{ID}, the recording identifier part 1 extracted.}
#'   \item{rec_starttime}{The first timestamp of the imputed series.}
#'   \item{desiredtz_part1}{Part 1's timezone, the empty string preserved.}
#'   \item{tail_expansion_log}{Carried through from part 1.}
#' }
#'   and then the canhrActi additions: \code{epochs} (the per-epoch time, invalid, night and
#'   one column per sib definition), \code{nights} (the per-night table, including the nights
#'   \code{sib.cla.sum} omits), \code{sib} (the whole detector object), \code{settings},
#'   \code{versions}, \code{file}, \code{status} (\code{state}, \code{detection_failed},
#'   \code{messages} and \code{stage_times}) and \code{elapsed}.
#'
#' @seealso \code{\link{as.ggir.ms3}} for the GGIR-shaped list, \code{\link{raw.sib.detect}}
#'   for the detector, \code{\link{raw.sib.summary}} for the sib period table,
#'   \code{\link{raw.sleep.regularity}} for the index and \code{\link{raw.impute}} for the
#'   series part 3 reads.
#'
#' @examples
#' \dontrun{
#' x <- read.raw.accelerometer("MOS2E39230594.gt3x", desiredtz = "America/Anchorage")
#' x$imputed <- raw.impute(x)
#' x$sleep <- raw.sleep.part3(x)
#' x$sleep$SPTE_start
#' identical(as.ggir.ms3(x)$sib.cla.sum, stored_ms3$sib.cla.sum)
#' }
#' @export
raw.sleep.part3 <- function(x, params = NULL, progress = NULL, ...) {
  t_total <- Sys.time()
  # C time locale so that SleepRegularityIndex$weekday does not depend on the machine
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be NULL or a function(stage, i, n, message)", call. = FALSE)
  }
  if (!inherits(x, "canhrActi_raw")) {
    stop("x must be a canhrActi_raw object from read.raw.accelerometer() or ",
         "read.ggir.milestone()", call. = FALSE)
  }
  params <- .raw.calibrate.params(list(params = x$params), params, list(...))
  messages <- character()
  stage_times <- c(impute = NA_real_, detect = NA_real_, guider_correct = NA_real_,
                   sri = NA_real_, summary = NA_real_, total = NA_real_)
  prog <- function(i, n, message) {
    if (!is.null(progress)) progress("part3", as.integer(i), as.integer(n), message)
    invisible(NULL)
  }
  meta <- x$meta
  ws3 <- meta$windowsizes[1] # short epoch size
  tail_expansion_log <- meta$tail_expansion_log
  desiredtz_part1 <- x$tz$desiredtz
  # Part 1's timezone wins over the caller's; the empty string wins too and means the
  # system zone
  if (!is.null(desiredtz_part1)) {
    params[["desiredtz"]] <- desiredtz_part1
  } else {
    desiredtz_part1 <- .raw.param(params, "desiredtz", "")
  }
  desiredtz <- .raw.param(params, "desiredtz", "")
  # raw.sib.detect() uses the epoch tables' own timezone; report a disagreement
  meta_tz <- if (is.list(meta$settings)) meta$settings$desiredtz else NULL
  if (!is.null(meta_tz) && is.character(meta_tz) && length(meta_tz) == 1 &&
      !identical(meta_tz, desiredtz)) {
    messages <- c(messages,
                  paste0("The epoch tables were built in timezone \"", meta_tz,
                         "\" but the recording records part 1's timezone as \"", desiredtz,
                         "\"; the epoch tables' timezone is the one the detector uses."))
  }
  versions <- list(canhrActi = if (is.null(x$versions$canhrActi)) NA_character_ else
    x$versions$canhrActi)
  settings <- list(windowsizes = meta$windowsizes, twd = c(-12, 12),
                   acc.metric = .raw.param(params, "acc.metric", "ENMO"),
                   sensor.location = .raw.param(params, "sensor.location", "wrist"),
                   zc.scale = .raw.param(params, "zc.scale", 1),
                   desiredtz = desiredtz,
                   ignorenonwear = .raw.param(params, "ignorenonwear", TRUE),
                   guider_cor_do = isTRUE(.raw.param(params, "guider_cor_do", FALSE)),
                   do.part3.pdf = isTRUE(.raw.param(params, "do.part3.pdf", FALSE)),
                   ggir_exact = isTRUE(.raw.param(params, "ggir_exact", TRUE)),
                   params = params)
  finish <- function(obj, state, detection_failed) {
    stage_times[["total"]] <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
    obj$settings <- settings
    obj$versions <- versions
    obj$file <- x$file
    obj$status <- list(state = state, detection_failed = detection_failed,
                       messages = messages, stage_times = stage_times)
    obj$elapsed <- stage_times[["total"]]
    structure(obj, class = "canhrActi_raw_sleep")
  }
  empty <- function() {
    list(sib.cla.sum = NULL, L5list = NULL, SPTE_start = NULL, SPTE_end = NULL,
         tib.threshold = NULL, part3_guider = NULL, SPTE_corrected = NULL,
         SleepRegularityIndex = NA, longitudinal_axis = NULL,
         id = if (is.null(x$inspection$id)) NA_character_ else x$inspection$id,
         rec_starttime = NULL, desiredtz_part1 = desiredtz_part1,
         tail_expansion_log = tail_expansion_log,
         epochs = NULL, nights = NULL, sib = NULL)
  }
  # GGIR writes no file for a corrupt or too-short recording; the inspection's own flags
  # count too
  corrupt <- isTRUE(meta$filecorrupt) || isTRUE(x$status$corrupt) || isTRUE(x$status$skipped)
  too_short <- isTRUE(meta$filetooshort) || isTRUE(x$status$too_short)
  if (corrupt || too_short) {
    messages <- c(messages,
                  paste0("The recording is ",
                         if (corrupt) "corrupt or was skipped" else "too short for part 1",
                         "; GGIR writes no part-3 milestone for it."))
    return(finish(empty(), "skipped", TRUE))
  }
  t0 <- Sys.time()
  imp <- .raw.sleep.part3.imputed(x$imputed, meta, x$wear, params)
  if (imp$computed) {
    messages <- c(messages, paste0("The recording carried no imputed series; raw.impute() ",
                                   "was run here. Part 3 reads the imputed series, never ",
                                   "the part-1 one."))
  }
  IMP <- imp$imputed
  stage_times[["impute"]] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # Hip longitudinal axis: an explicit user value wins, otherwise part 2's estimate. The
  # parameter object stores "unset" as c() where GGIR stores NULL.
  if (.raw.param(params, "sensor.location", "wrist") == "hip") {
    if (length(params[["longitudinal_axis"]]) == 0) {
      params[["longitudinal_axis"]] <- .raw.sleep.part3.hip.axis(
        if (is.null(IMP$if_hip_long_axis_id)) x$wear$if_hip_long_axis_id else
          IMP$if_hip_long_axis_id)
      settings$params <- params
    }
  }
  # twd is hardcoded at the call site, as in GGIR
  prog(1L, 5L, "Sustained inactivity bouts and the nightly guider")
  t0 <- Sys.time()
  SLE <- raw.sib.detect(meta = meta, imputed = IMP, wear = x$wear, params = params,
                        twd = c(-12, 12), progress = progress)
  stage_times[["detect"]] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  messages <- c(messages, SLE$messages)
  # Optional guider correction
  if (isTRUE(.raw.param(params, "guider_cor_do", FALSE)) &&
      length(SLE$SPTE_start) > 0 && any(!is.na(SLE$SPTE_start)) &&
      length(SLE$SPTE_end) > 0 && any(!is.na(SLE$SPTE_end))) {
    prog(2L, 5L, "Guider correction")
    t0 <- Sys.time()
    params_sleep <- .raw.sib.detect.sleep.params(params)
    params_sleep[["ggir_exact"]] <- isTRUE(.raw.param(params, "ggir_exact", TRUE))
    SLE <- raw.guider.correct(SLE, desiredtz = desiredtz, epochSize = meta$windowsizes[1],
                              params_sleep = params_sleep)
    stage_times[["guider_correct"]] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  }
  # Drop the crude estimate before anything reads the frame. The detector object keeps its
  # own output so that as.ggir.SLE() still reproduces g.sib.det's return.
  epochs <- SLE$output
  if ("spt_crude_estimate" %in% names(epochs)) {
    epochs <- epochs[, -which(names(epochs) == "spt_crude_estimate")]
  }
  prog(3L, 5L, "Sleep Regularity Index")
  t0 <- Sys.time()
  SleepRegularityIndex <- .raw.sleep.part3.sri(
    output = epochs, epochsize = meta$windowsizes[1], desiredtz = desiredtz,
    SRI1_smoothing_wsize_hrs = .raw.param(params, "SRI1_smoothing_wsize_hrs", NULL),
    SRI1_smoothing_frac = .raw.param(params, "SRI1_smoothing_frac", NULL))
  stage_times[["sri"]] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  L5list <- SLE$L5list
  SPTE_end <- SLE$SPTE_end
  SPTE_start <- SLE$SPTE_start
  SPTE_corrected <- SLE$SPTE_corrected
  tib.threshold <- SLE$tib.threshold
  longitudinal_axis <- SLE$longitudinal_axis
  part3_guider <- SLE$part3_guider
  out <- empty()
  out$L5list <- L5list
  out$SPTE_end <- SPTE_end
  out$SPTE_start <- SPTE_start
  out$SPTE_corrected <- SPTE_corrected
  out$tib.threshold <- tib.threshold
  out$longitudinal_axis <- longitudinal_axis
  out$part3_guider <- part3_guider
  out$SleepRegularityIndex <- SleepRegularityIndex
  out$epochs <- epochs
  out$sib <- SLE
  state <- if (isTRUE(SLE$detection.failed)) {
    if (identical(SLE$status, "too_short")) {
      "too_short_for_sleep"
    } else if (identical(SLE$status, "ok")) {
      "detection_failed"
    } else {
      SLE$status
    }
  } else {
    "ok"
  }
  # The save gate; length() of NULL is 0, so an empty series fails it as GGIR's does
  if (length(epochs) > 0 & SLE$detection.failed == FALSE) {
    prog(4L, 5L, "Sustained inactivity bout summary")
    ID <- x$inspection$id
    if (is.list(ID)) ID <- unlist(ID)
    if (isTRUE(.raw.param(params, "do.part3.pdf", FALSE))) {
      messages <- c(messages, paste0("do.part3.pdf is TRUE: GGIR would write ",
                                     "meta/sleep.qc/graphperday_id_<ID>_<file>.pdf here. ",
                                     "canhrActi draws its own figures and writes no PDF."))
    }
    t0 <- Sys.time()
    sib.cla.sum <- raw.sib.summary(list(output = epochs), meta,
                                   ignorenonwear = .raw.param(params, "ignorenonwear", TRUE),
                                   desiredtz = desiredtz)
    stage_times[["summary"]] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    # used by the sleep log loader to align the log with the recording
    rec_starttime <- IMP$metashort[1, 1]
    out$id <- ID
    out$sib.cla.sum <- sib.cla.sum
    out$rec_starttime <- rec_starttime
    state <- "ok"
  } else {
    messages <- c(messages,
                  paste0("GGIR writes no part-3 milestone for this recording (state \"",
                         state, "\"): the per-epoch series is ",
                         if (length(epochs) == 0) "empty" else "present",
                         " and detection.failed is ", isTRUE(SLE$detection.failed), "."))
  }
  prog(5L, 5L, "Per-night table")
  out$nights <- .raw.sleep.part3.nights(
    windows = SLE$windows, sib.cla.sum = out$sib.cla.sum, L5list = L5list,
    SPTE_start = SPTE_start, SPTE_end = SPTE_end, tib.threshold = tib.threshold,
    part3_guider = part3_guider,
    definition = if (length(SLE$definitions) > 0) SLE$definitions[1] else NA_character_,
    ws3 = ws3)
  finish(out, state, isTRUE(SLE$detection.failed))
}

# GGIR SHAPES AND PRINTING

#' GGIR's ms3 Milestone Objects From a Part-3 Object
#'
#' Returns the fourteen objects GGIR saves into \code{meta/ms3.out/<file>.RData}, under
#' GGIR's own names and in the order of its \code{save()} call, so that \code{identical()}
#' against a stored milestone works object by object.
#'
#' @details \code{GGIRversion} is written only when GGIR is installed (NA otherwise) and
#'   \code{tail_expansion_log} is a pass-through from part 1, so neither is comparable in a
#'   parity test. \code{SLE$output} is not among the fourteen: GGIR discards the per-epoch
#'   series at the save; canhrActi keeps it as \code{$epochs}. When the part-3 state is not
#'   "ok", \code{sib.cla.sum} and \code{rec_starttime} are NULL.
#'
#' @param x A canhrActi_raw_sleep from \code{\link{raw.sleep.part3}}, or a canhrActi_raw
#'   carrying one in \code{$sleep}.
#' @return A named list of fourteen members in GGIR's order.
#' @examples
#' \dontrun{
#' ms3 <- as.ggir.ms3(x$sleep)
#' e <- new.env(); load("meta/ms3.out/MOS2E39230594.gt3x.RData", envir = e)
#' identical(ms3$sib.cla.sum, e$sib.cla.sum)
#' }
#' @export
as.ggir.ms3 <- function(x) {
  if (inherits(x, "canhrActi_raw")) x <- x$sleep
  if (!inherits(x, "canhrActi_raw_sleep")) {
    stop("x must be a canhrActi_raw_sleep object from raw.sleep.part3()", call. = FALSE)
  }
  GGIRversion <- if (requireNamespace("GGIR", quietly = TRUE)) {
    utils::packageVersion("GGIR")
  } else {
    NA
  }
  # single-bracket assignment keeps a NULL member
  out <- list()
  out["sib.cla.sum"] <- list(x$sib.cla.sum)
  out["L5list"] <- list(x$L5list)
  out["SPTE_end"] <- list(x$SPTE_end)
  out["SPTE_start"] <- list(x$SPTE_start)
  out["tib.threshold"] <- list(x$tib.threshold)
  out["rec_starttime"] <- list(x$rec_starttime)
  out["ID"] <- list(x$id)
  out["longitudinal_axis"] <- list(x$longitudinal_axis)
  out["SleepRegularityIndex"] <- list(x$SleepRegularityIndex)
  out["tail_expansion_log"] <- list(x$tail_expansion_log)
  out["GGIRversion"] <- list(GGIRversion)
  out["part3_guider"] <- list(x$part3_guider)
  out["desiredtz_part1"] <- list(x$desiredtz_part1)
  out["SPTE_corrected"] <- list(x$SPTE_corrected)
  out
}

#' Print Method for a Part-3 Sleep Object
#'
#' @param x A canhrActi_raw_sleep.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_sleep <- function(x, ...) {
  cat("\ncanhrActi sleep period estimation (GGIR part 3: g.part3)\n")
  if (!is.null(x$file$filename) && !is.na(x$file$filename)) {
    cat("  file:        ", x$file$filename, "\n", sep = "")
  }
  if (!is.null(x$id) && !is.na(x$id)) cat("  id:          ", x$id, "\n", sep = "")
  cat("  timezone:    ",
      if (is.null(x$desiredtz_part1) || !nzchar(x$desiredtz_part1)) {
        paste0("part 1 recorded none, so the system zone (", Sys.timezone(), ")")
      } else {
        x$desiredtz_part1
      }, "\n", sep = "")
  if (!identical(x$status$state, "ok")) {
    cat("  state:       ", x$status$state,
        " (GGIR would write no part-3 milestone)\n", sep = "")
  }
  if (!is.null(x$nights)) {
    cat("  nights:      ", nrow(x$nights), " noon-to-noon windows, ",
        length(which(x$nights$in_sib_cla_sum)), " with sustained inactivity bouts\n", sep = "")
  }
  if (!is.null(x$sib.cla.sum)) {
    cat("  sib periods: ", nrow(x$sib.cla.sum), " rows over ",
        length(unique(x$sib.cla.sum$night)), " nights, definitions ",
        paste(unique(as.character(x$sib.cla.sum$definition)), collapse = ", "), "\n", sep = "")
  }
  fmt <- function(v) {
    if (is.null(v)) return("none")
    paste(ifelse(is.na(v), "NA", format(round(as.numeric(v), 3), trim = TRUE)), collapse = " ")
  }
  cat("  SPTE start:  ", fmt(x$SPTE_start), "\n", sep = "")
  cat("  SPTE end:    ", fmt(x$SPTE_end), "\n", sep = "")
  cat("  L5:          ", fmt(x$L5list), "\n", sep = "")
  cat("  guider:      ",
      if (is.null(x$part3_guider)) "none" else
        paste(ifelse(is.na(x$part3_guider), "NA", x$part3_guider), collapse = " "),
      "\n", sep = "")
  if (is.data.frame(x$SleepRegularityIndex)) {
    cat("  SRI:         ", fmt(x$SleepRegularityIndex$SleepRegularityIndex), "\n", sep = "")
  } else {
    cat("  SRI:         not computed (fewer than two days of short epochs)\n")
  }
  if (length(x$status$messages) > 0) {
    cat("  messages:\n")
    for (m in x$status$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(x$elapsed) && !is.na(x$elapsed)) {
    cat("  elapsed:     ", round(x$elapsed, 2), " s\n", sep = "")
  }
  invisible(x)
}
