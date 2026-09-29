# Ported from GGIR 3.3-9 R/g.part5_analyseSegment.R and R/g.part5.lux_persegment.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The row is written into a named list
# instead of GGIR's shared character matrix and converted once by .raw.timeuse.row.chr;
# the params objects are explicit arguments; the unread lightpeak_available argument, the
# commented-out steps block, the NaN blanking (dead on a character matrix, not on a
# numeric row) and options(encoding = "UTF-8") are not transcribed; the quantile writes
# keep 3.3.6's ungated form; two shape checks raise where GGIR would emit a nonsense
# column name.

# HELPERS

#' Force a Bout Indicator to a Matrix With One Row Per Bout Duration
#'
#' \code{.raw.identify.levels} builds each \code{bc.*} by \code{rbind}, so a length-1
#' \code{boutdur.*} gives a plain vector. This turns it back into a 1 by n matrix. The
#' \code{nrow > ncol} test rather than a plain transpose is GGIR's: on a one-epoch recording
#' \code{as.matrix()} already gives 1 by 1.
#'
#' @param boutcount A bout indicator matrix, or a bare vector from a single bout setting.
#' @return A matrix with one row per bout duration.
#' @keywords internal
#' @noRd
.raw.timeuse.checkshape <- function(boutcount) {
  if (is.matrix(boutcount) == FALSE) { # if there is only one bout setting
    boutcount <- as.matrix(boutcount)
    if (nrow(boutcount) > ncol(boutcount)) boutcount <- t(boutcount)
  }
  return(boutcount)
}

#' Resolve One Segment-Analysis Setting
#'
#' An explicit argument wins; the flat parameter object is consulted only when the argument
#' is NULL.
#'
#' @param value The explicit argument, or NULL.
#' @param params A flat named parameter list from \code{raw.params()}, or NULL.
#' @param name Member name to look up.
#' @param default Value to use when neither supplies one.
#' @return The resolved value.
#' @keywords internal
#' @noRd
.raw.timeuse.segment.setting <- function(value, params, name, default = NULL) {
  if (!is.null(value)) return(value)
  if (!is.null(params) && name %in% names(params)) return(params[[name]])
  return(default)
}

#' Convert One Day-Summary Row to the Character Vector GGIR Stores
#'
#' The single place where the named-list row becomes GGIR's character encoding.
#' \code{as.character()} on a double gives 15 significant digits, as GGIR's assignment into a
#' character matrix does; a NaN (\code{mean()} of no values) becomes the string "NaN"; a real
#' NA becomes "", through GGIR's NA sweep. NaN and NA are two different empty markers in the
#' same table.
#'
#' @param row A named list, one element per column, as \code{.raw.timeuse.segment} returns.
#' @return A named character vector of the same length, in the same order, with the same names.
#' @keywords internal
#' @noRd
.raw.timeuse.row.chr <- function(row) {
  out <- vapply(row, function(x) {
    if (length(x) == 0) return("")
    x <- x[[1]]
    if (is.na(x) && !is.nan(x)) return("")
    as.character(x)
  }, character(1), USE.NAMES = FALSE)
  names(out) <- names(row)
  return(out)
}

# LUX PER SEGMENT

#' Pad a Per-Segment Lux Aggregate to the Full Segment List
#'
#' An outer join onto the full \code{LUX_day_segments} list, so a day which does not reach
#' every clock segment still emits the same columns in the same order.
#'
#' @details GGIR's nested \code{standardise_luxperseg}. The 24 is dropped from the segment
#'   list before the join and put back to look up each segment's end hour, so the last column
#'   is named after the interval that ends at 24. \code{base::merge} sorts by \code{seg}, so
#'   the returned order follows the clock.
#'
#' @param x A two-column aggregate, group then value.
#' @param LUX_day_segments Numeric vector of clock hours.
#' @param LUXmetricname Middle part of the emitted column names.
#' @return list(values, names), one entry per segment.
#' @keywords internal
#' @noRd
.raw.lux.standardise.persegment <- function(x, LUX_day_segments, LUXmetricname = "") {
  colnames(x) <- c("seg", "light")
  if (24 %in% LUX_day_segments) {
    LUX_day_segments <- LUX_day_segments[which(LUX_day_segments != 24)] # remove end of day
  }
  Nsegs <- length(LUX_day_segments)
  x <- base::merge(x, data.frame(seg = LUX_day_segments,
                                 light = rep(NA, Nsegs)),
                   by = c("seg"), all.y = TRUE)
  x <- x[, c("seg", "light.x")]
  colnames(x) <- c("seg", "light")
  LUX_day_segments <- c(LUX_day_segments, 24) # end of day back in
  end_of_segment <- LUX_day_segments[which(x$seg %in% LUX_day_segments) + 1]
  invisible(list(values = x$light,
                 names = paste0("LUX_", LUXmetricname, "_", x$seg, "_",
                                end_of_segment, "hr_day")))
}

#' Five Lux Summaries Per Clock Segment of One Day Window
#'
#' Splits the waking epochs of one day window into the clock segments named by
#' \code{LUX_day_segments} and returns, for each segment, the minutes above 1000 lux, the
#' minutes awake, the mean lux, the minutes of imputed lux and the minutes of ignored lux.
#'
#' @details The window is truncated to 24 hours of epochs; the first segment is back-filled
#'   to its full clock length with epochs from before the window start, whose lux is forced
#'   to zero; the last segment is dropped whenever the window is shorter than 24 hours. The
#'   3.3-9 body is used because 3.3.6 lacks the two guards and fails when the back-fill runs
#'   off the front of the series. Every count is divided by \code{60/epochSize}, so the
#'   values are minutes. The caller gates the whole lux block on a \code{lightpeak} column,
#'   which a .gt3x never produces.
#'
#' @param ts The part-5 time series, with \code{lightpeak}, \code{lightpeak_imputationcode},
#'   \code{diur} and \code{time}.
#' @param sse Epoch indices of the whole window, waking and sleep period time together.
#' @param LUX_day_segments Numeric vector of clock hours, ascending, normally starting at 0 and
#'   ending at 24.
#' @param epochSize Epoch length in seconds.
#' @param desiredtz Timezone, used only when \code{ts$time} is still a character column.
#' @return list(values, names), five times the number of segments of each.
#' @keywords internal
#' @noRd
.raw.timeuse.lux.persegment <- function(ts, sse, LUX_day_segments, epochSize, desiredtz = "") {
  # only look at waking hours of this day
  sse <- sse[which(ts$diur[sse] == 0)]
  # first hour of the segment each epoch falls in
  if ("POSIXt" %in% class(ts$time[sse[1]])) {
    posixTime <- ts$time[sse]
  } else {
    posixTime <- .raw.iso8601.to.posix(ts$time[sse], tz = desiredtz)
  }
  first_hour_seg <- as.numeric(format(posixTime, "%H"))
  for (ldi in 1:(length(LUX_day_segments) - 1)) {
    tmpl <- which(first_hour_seg >= LUX_day_segments[ldi] &
                    first_hour_seg < LUX_day_segments[ldi + 1])
    if (length(tmpl) > 0) {
      first_hour_seg[tmpl] <- LUX_day_segments[ldi]
    }
  }
  Nepochperday <- 24 * (3600 / epochSize)
  if (length(first_hour_seg) > Nepochperday) {
    first_hour_seg <- first_hour_seg[1:Nepochperday]
  }
  if (length(sse) > Nepochperday) {
    sse <- sse[1:Nepochperday]
  }
  # expanding first time segment
  isFirstHour <- which(LUX_day_segments == first_hour_seg[1])
  expected_N_seg1 <- (rep(LUX_day_segments, 2)[isFirstHour + 1] -
                        LUX_day_segments[isFirstHour]) * 60 * (60 / epochSize)
  actual_N_seg1 <- length(which(first_hour_seg[1:round(Nepochperday * 0.66)] == first_hour_seg[1]))
  missingN_seg1 <- expected_N_seg1 - actual_N_seg1
  if (length(sse) > 0) {
    if (sse[1] - missingN_seg1 < 0) missingN_seg1 <- missingN_seg1 + (sse[1] - missingN_seg1 - 1)
    if (missingN_seg1 > 0) {
      extension <- (sse[1] - missingN_seg1):(sse[1] - 1)
      ts$lightpeak[extension] <- 0 # ensure that light during SPT is treated as zeros
      sse <- c(extension, sse)
      first_hour_seg <- c(rep(first_hour_seg[1], missingN_seg1), first_hour_seg)
    }
  }
  # truncate to 24 hours again
  if (length(first_hour_seg) > Nepochperday) {
    first_hour_seg <- first_hour_seg[1:Nepochperday]
  }
  if (length(sse) > Nepochperday) {
    sse <- sse[1:Nepochperday]
  }
  # drop the incomplete last segment
  if (length(sse) < Nepochperday) {
    last_seg <- first_hour_seg[length(first_hour_seg)]
    N_last_seg <- length(which(first_hour_seg[round(length(first_hour_seg) * 0.5):length(first_hour_seg)] == last_seg))
    sse <- sse[1:(length(sse) - N_last_seg)]
    first_hour_seg <- first_hour_seg[1:(length(first_hour_seg) - N_last_seg)]
  }
  # Aggregation per segment
  fraction_above_thousand <- function(x, epochSize) {
    timeabove1000 <- length(which(x > 1000)) / (60 / epochSize)
    return(timeabove1000)
  }
  LUXabove1000 <- stats::aggregate(ts$lightpeak[sse], by = list(first_hour_seg),
                                   fraction_above_thousand, epochSize)
  countvalue <- function(x, epochSize, value) {
    return(length(which(x == value)) / (60 / epochSize))
  }
  LUXwaketime <- stats::aggregate(ts$diur[sse], by = list(first_hour_seg), FUN = countvalue,
                                  value = 0, epochSize = epochSize)
  mymean <- function(x) {
    return(round(mean(x, na.rm = TRUE), digits = 1))
  }
  LUXmean <- stats::aggregate(ts$lightpeak[sse], by = list(first_hour_seg), mymean)
  LUXlightimputed <- stats::aggregate(ts$lightpeak_imputationcode[sse], by = list(first_hour_seg),
                                      FUN = countvalue, value = 1, epochSize = epochSize)
  LUXlightignored <- stats::aggregate(ts$lightpeak_imputationcode[sse], by = list(first_hour_seg),
                                      FUN = countvalue, value = 2, epochSize = epochSize)

  standardLPS1 <- .raw.lux.standardise.persegment(LUXabove1000, LUX_day_segments, "above1000")
  standardLPS2 <- .raw.lux.standardise.persegment(LUXwaketime, LUX_day_segments, "timeawake")
  standardLPS3 <- .raw.lux.standardise.persegment(LUXmean, LUX_day_segments, "mean")
  standardLPS4 <- .raw.lux.standardise.persegment(LUXlightimputed, LUX_day_segments, "imputed")
  standardLPS5 <- .raw.lux.standardise.persegment(LUXlightignored, LUX_day_segments, "ignored")

  values <- c(standardLPS1$values, standardLPS2$values,
              standardLPS3$values, standardLPS4$values,
              standardLPS5$values)
  names <- c(standardLPS1$names, standardLPS2$names,
             standardLPS3$names, standardLPS4$names,
             standardLPS5$names)
  invisible(list(values = values, names = names))
}

# THE SEGMENT ROW

#' One Row of the Part 5 Day Summary, From One Segment of One Day Window
#'
#' One (sib definition, threshold triple, timewindow, window, segment) in, one row of the
#' day table out, plus the writes the call leaves on the time series. At the default
#' configuration it produces 116 of the 119 columns of GGIR's \code{output}; \code{ID},
#' \code{filename} and \code{GGIRversion} are the caller's. Everything after \code{TRVi} is
#' written only when the segment has at least one epoch; otherwise \code{doNext} is TRUE and
#' the row has 16 columns. Units and the two partitions of waking time are described on
#' \code{\link{raw.timeuse}}.
#'
#' @details Transcribes GGIR 3.3-9 R/g.part5_analyseSegment.R, with 3.3-9's multi-range
#'   \code{sse} accumulation and guider write, but without its gate on
#'   \code{segments_names[si]} around the two \code{quantile_mostactive*} writes, which
#'   indexes by the row index and blanks both columns after the first row.
#'
#'   GGIR's conventions, reproduced: \code{calendar_date} is the date of the first epoch of
#'   the window plus a one-day carry set by the previous window when a WW wake fell before
#'   midnight or an OO onset after it; the six part-4 pass-through columns are looked up by
#'   matching \code{as.Date()} of \code{sumSleep$calendar_date} (day, month, year) and are NA
#'   when no night carries that date; \code{N_atleast5minwakenight} subtracts 2 before
#'   clamping at 0 (behind \code{ggir_exact}); an MM window with neither an onset nor a wake
#'   has \code{ts$diur} zeroed over the window while \code{LEVELS} was computed from the
#'   unzeroed series (behind \code{ggir_exact}); \code{sleep_efficiency_after_onset} is sib
#'   epochs over all sleep-period epochs, so non-wear counts as wake; \code{WLH} is the window
#'   length in hours plus one epoch, floored at 1.001 for a single segment only;
#'   \code{Nbouts_*} counts runs of 1 in the bout indicator and \code{Nblocks_*} runs of
#'   \code{LEVELS}, which are not the same quantity; \code{start_end_window} is the qwindow
#'   label on an MM window and the real clock edges on a WW or OO window; \code{window} is
#'   the segment name.
#'
#'   Not transcribed: the NaN blanking (dead on a character matrix, not on a numeric row),
#'   the commented-out steps block, \code{options(encoding = "UTF-8")} and the unread
#'   \code{lightpeak_available} argument. The six part-4 columns keep their own types until
#'   \code{.raw.timeuse.row.chr} converts them, where GGIR's single \code{c()} coerces them
#'   at once; the two differ only for a factor \code{guider}. Two checks raise where GGIR
#'   would emit a nonsense column name: a \code{bc.*} whose row count disagrees with
#'   \code{boutdur.*}, and a \code{segStartEnd} that is neither 2 nor 4 long.
#'
#' @param indexlog list(fileIndex, winType, winIndex, winStartEnd, segIndex1, segIndex2,
#'   segStartEnd, columnIndex), as GGIR's caller builds it: the timewindow, the window
#'   number, the window's \code{qqq}, the output row index, the segment index within the
#'   window, the segment's two or four epoch indices and the caller's column cursor.
#' @param timeList list(ts, sec, min, hour, time_POSIX, epochSize). \code{ts$time} must already
#'   be POSIXct; the four vectors come from \code{.raw.timeuse.midnights}.
#' @param levelList list(threshold, LEVELS, Lnames, OLEVELS, bc.mvpa, bc.in, bc.lig) from
#'   \code{.raw.identify.levels}.
#' @param segments Named list of segments for this window, from
#'   \code{.raw.timeuse.define.days}; only \code{names(segments)[segIndex2]} is read.
#' @param segments_names Segment names for this window, from \code{.raw.timeuse.define.days}.
#' @param dsummary The row accumulator: a list whose \code{[[si]]} element is the named row for
#'   output row \code{si}. Pass \code{list()} on the first call.
#' @param sumSleep The part-4 night summary rows for this sib definition.
#' @param sibDef The sib definition label, written to \code{sleepparam}.
#' @param add_one_day_to_next_date The carry flag from the previous window.
#' @param desiredtz Timezone of the timestamps; NULL takes it from \code{params}, and ""
#'   means the system zone.
#' @param boutdur.mvpa,boutdur.in,boutdur.lig Bout durations in minutes, descending, the same
#'   vectors that were passed to \code{.raw.identify.levels}; not sorted here.
#' @param boutcriter.mvpa,boutcriter.in,boutcriter.lig Bout criteria, written to the row as
#'   provenance columns.
#' @param frag.metrics Fragmentation metrics, or NULL for no fragmentation block.
#' @param iglevels Intensity gradient bin edges, or NULL for no intensity gradient.
#' @param LUXthresholds Lux range edges, defaulting to GGIR's c(0, 100, 500, 1000, 3000,
#'   5000, 10000). The last one opens the "_inf_day" column.
#' @param LUX_day_segments Clock hours for the per-segment lux block, or NULL. This member
#'   gates all lux output, not only the per-segment part.
#' @param do.sibreport TRUE (the default) to allow the rest and nap blocks.
#' @param storefolderstructure TRUE to add \code{filename_dir} and \code{foldername}.
#' @param possible_nap_window,possible_nap_dur,possible_nap_gap,possible_nap_edge_acc Nap
#'   detection parameters. Both of the first two must be non-NULL for the rest analysis to run.
#' @param nap_markerbutton_method,nap_markerbutton_max_distance,method_research_vars Further
#'   rest-analysis parameters, passed through unchanged.
#' @param nap_model Read only by the two nap columns, which are unreachable because nothing
#'   in part 5 creates the \code{nap1_nonwear2} column they test for.
#' @param fullFilename Full path of the source file, for \code{filename_dir}.
#' @param foldernamei Lowest folder name, for \code{foldername}.
#' @param tail_expansion_log The part-1 tail expansion log, or NULL.
#' @param sibreport The whole-recording sib report, for the rest analysis.
#' @param warn_xmin Passed to \code{.raw.fragmentation}: TRUE warns once that the power-law
#'   metrics use \code{xmin} in epochs rather than minutes.
#' @param params A flat parameter object from \code{raw.params()}, consulted only for settings
#'   left NULL above.
#' @param ggir_exact TRUE reproduces GGIR's unconditional \code{- 2} in
#'   \code{N_atleast5minwakenight} and the zeroing of \code{ts$diur} over an MM window with
#'   neither an onset nor a wake. FALSE drops both.
#'
#' @return list(indexlog, ds_names, dsummary, timeList, doNext, add_one_day_to_next_date).
#'   \code{ds_names} is \code{names()} of the row just written. \code{timeList} carries the
#'   possibly-mutated \code{ts} and the possibly-grown \code{LEVELS} and \code{Lnames}.
#'   \code{doNext} is TRUE when the segment held no epochs.
#' @keywords internal
#' @noRd
.raw.timeuse.segment <- function(indexlog, timeList, levelList,
                                 segments,
                                 segments_names,
                                 dsummary,
                                 sumSleep, sibDef,
                                 add_one_day_to_next_date = FALSE,
                                 desiredtz = NULL,
                                 boutdur.mvpa = NULL, boutdur.in = NULL, boutdur.lig = NULL,
                                 boutcriter.mvpa = NULL, boutcriter.in = NULL,
                                 boutcriter.lig = NULL,
                                 frag.metrics = NULL,
                                 iglevels = NULL,
                                 LUXthresholds = NULL,
                                 LUX_day_segments = NULL,
                                 do.sibreport = NULL,
                                 storefolderstructure = NULL,
                                 possible_nap_window = NULL, possible_nap_dur = NULL,
                                 possible_nap_gap = NULL, possible_nap_edge_acc = NULL,
                                 nap_markerbutton_method = NULL,
                                 nap_markerbutton_max_distance = NULL,
                                 method_research_vars = NULL,
                                 nap_model = NULL,
                                 fullFilename = NULL, foldernamei = NULL,
                                 tail_expansion_log = NULL,
                                 sibreport = NULL,
                                 warn_xmin = TRUE,
                                 params = NULL,
                                 ggir_exact = TRUE) {
  # an explicit argument wins over the parameter object
  boutdur.mvpa <- .raw.timeuse.segment.setting(boutdur.mvpa, params, "boutdur.mvpa")
  boutdur.in <- .raw.timeuse.segment.setting(boutdur.in, params, "boutdur.in")
  boutdur.lig <- .raw.timeuse.segment.setting(boutdur.lig, params, "boutdur.lig")
  boutcriter.mvpa <- .raw.timeuse.segment.setting(boutcriter.mvpa, params, "boutcriter.mvpa")
  boutcriter.in <- .raw.timeuse.segment.setting(boutcriter.in, params, "boutcriter.in")
  boutcriter.lig <- .raw.timeuse.segment.setting(boutcriter.lig, params, "boutcriter.lig")
  frag.metrics <- .raw.timeuse.segment.setting(frag.metrics, params, "frag.metrics")
  iglevels <- .raw.timeuse.segment.setting(iglevels, params, "iglevels")
  LUXthresholds <- .raw.timeuse.segment.setting(LUXthresholds, params, "LUXthresholds",
                                                c(0, 100, 500, 1000, 3000, 5000, 10000))
  LUX_day_segments <- .raw.timeuse.segment.setting(LUX_day_segments, params, "LUX_day_segments")
  do.sibreport <- .raw.timeuse.segment.setting(do.sibreport, params, "do.sibreport", TRUE)
  storefolderstructure <- .raw.timeuse.segment.setting(storefolderstructure, params,
                                                       "storefolderstructure", FALSE)
  possible_nap_window <- .raw.timeuse.segment.setting(possible_nap_window, params,
                                                      "possible_nap_window")
  possible_nap_dur <- .raw.timeuse.segment.setting(possible_nap_dur, params, "possible_nap_dur")
  possible_nap_gap <- .raw.timeuse.segment.setting(possible_nap_gap, params, "possible_nap_gap")
  possible_nap_edge_acc <- .raw.timeuse.segment.setting(possible_nap_edge_acc, params,
                                                        "possible_nap_edge_acc")
  nap_markerbutton_method <- .raw.timeuse.segment.setting(nap_markerbutton_method, params,
                                                          "nap_markerbutton_method")
  nap_markerbutton_max_distance <- .raw.timeuse.segment.setting(nap_markerbutton_max_distance,
                                                                params,
                                                                "nap_markerbutton_max_distance")
  method_research_vars <- .raw.timeuse.segment.setting(method_research_vars, params,
                                                       "method_research_vars")
  nap_model <- .raw.timeuse.segment.setting(nap_model, params, "nap_model")
  desiredtz <- .raw.timeuse.segment.setting(desiredtz, params, "desiredtz", "")

  # unpack indexlog
  fileIndex <- indexlog$fileIndex
  timewindowi <- indexlog$winType
  wi <- indexlog$winIndex
  qqq <- indexlog$winStartEnd
  si <- indexlog$segIndex1
  current_segment_i <- indexlog$segIndex2
  Nsegments <- length(indexlog$segStartEnd) / 2
  if (!length(indexlog$segStartEnd) %in% c(2, 4)) {
    # GGIR reads only segStartEnd[1:2] and [3:4]
    stop("segStartEnd must hold one or two index ranges, not ",
         length(indexlog$segStartEnd) / 2, call. = FALSE)
  }
  if (Nsegments == 1) {
    segStart <- indexlog$segStartEnd[1]
    segEnd <- indexlog$segStartEnd[2]
  } else {
    segStart <- indexlog$segStartEnd[1:2]
    segEnd <- indexlog$segStartEnd[3:4]
  }
  fi <- indexlog$columnIndex
  if (is.null(fi)) fi <- 1

  ts <- timeList$ts
  sec <- timeList$sec
  min <- timeList$min
  hour <- timeList$hour
  time_POSIX <- timeList$time_POSIX
  ws3new <- timeList$epochSize

  TRLi <- levelList$threshold[1]
  TRMi <- levelList$threshold[2]
  TRVi <- levelList$threshold[3]
  LEVELS <- levelList$LEVELS
  Lnames <- levelList$Lnames
  OLEVELS <- levelList$OLEVELS
  bc.mvpa <- levelList$bc.mvpa
  bc.in <- levelList$bc.in
  bc.lig <- levelList$bc.lig

  skiponset <- skipwake <- TRUE
  row <- list()

  # calendar_date is the date of the first epoch of the window
  date <- as.Date(ts$time[qqq[1]], tz = desiredtz)
  if (add_one_day_to_next_date == TRUE & timewindowi %in% c("WW", "OO")) {
    date <- date + 1
    add_one_day_to_next_date <- FALSE
  }
  weekday <- .in_c_time(weekdays(date, abbreviate = FALSE))
  row$weekday <- weekday
  row$calendar_date <- as.character(date)
  segmentName <- segments_names[current_segment_i]
  segmentTiming <- names(segments)[current_segment_i]
  row$window_number <- wi
  row$window <- segmentName
  row$start_end_window <- segmentTiming
  # Get onset and waking timing, both as timestamp and as index
  onsetwaketiming <- .raw.timeuse.onsetwake(qqq, ts, min, sec, hour, timewindowi)
  onset <- onsetwaketiming$onset; wake <- onsetwaketiming$wake
  onseti <- onsetwaketiming$onseti; wakei <- onsetwaketiming$wakei
  skiponset <- onsetwaketiming$skiponset; skipwake <- onsetwaketiming$skipwake
  if (wake < 24 & timewindowi == "WW") {
    # a wake before midnight means the next WW window starts a day before its SPT date
    add_one_day_to_next_date <- TRUE
  }
  if (onset > 24 & timewindowi == "OO") {
    # an onset after midnight means the next OO window starts a day after its SPT date
    add_one_day_to_next_date <- TRUE
  }
  if (skiponset == FALSE) {
    row$sleeponset <- onset
    row$sleeponset_ts <- as.character(strftime(format(time_POSIX[onseti]), tz = desiredtz,
                                               format = "%H:%M:%S"))
  } else {
    row$sleeponset <- NA
    row$sleeponset_ts <- NA
  }
  if (skipwake == FALSE) {
    row$wakeup <- wake
    row$wakeup_ts <- as.character(strftime(format(time_POSIX[wakei]), tz = desiredtz,
                                           format = "%H:%M:%S"))
  } else {
    row$wakeup <- NA
    row$wakeup_ts <- NA
  }
  # if skiponset and skipwake and "MM", then set full window as awake
  zeroed_diur <- skiponset == TRUE & skipwake == TRUE & timewindowi == "MM"
  if (isTRUE(ggir_exact) && zeroed_diur) {
    ts$diur_bu <- ts$diur
    ts$diur[qqq[1]:qqq[2]] <- 0
  }
  # look up the matching part-4 night by date
  recDates <- as.Date(sumSleep$calendar_date, format = "%d/%m/%Y", origin = "1970-01-01")
  row$sleepparam <- sibDef
  dayofinterest <- which(recDates == date)
  if (length(dayofinterest) > 0) {
    dayofinterest <- dayofinterest[1]
    row$night_number <- sumSleep$night[dayofinterest]
    row$daysleeper <- sumSleep$daysleeper[dayofinterest]
    row$cleaningcode <- sumSleep$cleaningcode[dayofinterest]
    row$guider <- sumSleep$guider[dayofinterest]
    row$sleeplog_used <- sumSleep$sleeplog_used[dayofinterest]
    row$acc_available <- sumSleep$acc_available[dayofinterest]
    for (gi in 1:Nsegments) {
      if (!is.na(segStart[gi]) & !is.na(segEnd[gi])) {
        # add guider also to timeseries
        ts$guider[segStart[gi]:segEnd[gi]] <- sumSleep$guider[dayofinterest]
      }
    }
  } else {
    row$night_number <- NA
    row$daysleeper <- NA
    row$cleaningcode <- NA
    row$guider <- NA
    row$sleeplog_used <- NA
    row$acc_available <- NA
  }
  # qqq1 is the start of the day/segment and qqq2 the end
  qqq1 <- segStart
  qqq2 <- segEnd
  row$TRLi <- TRLi
  row$TRMi <- TRMi
  row$TRVi <- TRVi
  sse <- NULL
  for (gi in 1:Nsegments) {
    if (!is.na(qqq1[gi])) {
      if (qqq1[gi] > length(LEVELS)) qqq1[gi] <- length(LEVELS)
      if (gi == 1) {
        sse <- qqq1[gi]:qqq2[gi]
      } else {
        sse <- c(sse, qqq1[gi]:qqq2[gi])
      }
    } else {
      sse <- NULL
    }
  }
  doNext <- FALSE
  if (length(sse) >= 1) {
    # percentage of available data
    zt_hrs_nonwear <- (length(which(ts$diur[sse] == 0 & ts$nonwear[sse] == 1)) * ws3new) / 3600 # day
    zt_hrs_total <- (length(which(ts$diur[sse] == 0)) * ws3new) / 3600 # day
    row$nonwear_perc_day <- (zt_hrs_nonwear / zt_hrs_total) * 10000 / 100
    zt_hrs_nonwear <- (length(which(ts$diur[sse] == 1 & ts$nonwear[sse] == 1)) * ws3new) / 3600 # night
    zt_hrs_total <- (length(which(ts$diur[sse] == 1)) * ws3new) / 3600 # night
    row$nonwear_perc_spt <- (zt_hrs_nonwear / zt_hrs_total) * 10000 / 100
    zt_hrs_nonwear <- (length(which(ts$nonwear[sse] == 1)) * ws3new) / 3600
    zt_hrs_total <- (length(ts$diur[sse]) * ws3new) / 3600 # night and day
    row$nonwear_perc_day_spt <- (zt_hrs_nonwear / zt_hrs_total) * 10000 / 100
    # nap/sib/nonwear overlap analysis
    if (do.sibreport == TRUE &&
        !is.null(possible_nap_window) &&
        !is.null(possible_nap_dur)) {
      restAnalyses <- .raw.timeuse.rest(
        sibreport = sibreport,
        row = row,
        ts = ts[sse[ts$diur[sse] == 0], ],
        tz = desiredtz,
        params_sleep = list(possible_nap_window = possible_nap_window,
                            possible_nap_dur = possible_nap_dur,
                            possible_nap_gap = possible_nap_gap,
                            possible_nap_edge_acc = possible_nap_edge_acc,
                            nap_markerbutton_method = nap_markerbutton_method,
                            nap_markerbutton_max_distance = nap_markerbutton_max_distance,
                            method_research_vars = method_research_vars))
      ts[sse[ts$diur[sse] == 0], ] <- restAnalyses$ts
      row <- restAnalyses$row

      # If naps detected add these to LEVELS
      detectedNaps <- which(ts$sibdetection[sse] == 2)
      if (length(detectedNaps) > 0) {
        if ("day_nap" %in% Lnames) {
          LEVELS[sse[detectedNaps]] <- length(Lnames) - 1
        } else {
          LEVELS[sse[detectedNaps]] <- length(Lnames)
        }
      }
      if ("day_nap" %in% Lnames == FALSE) {
        Lnames <- c(Lnames, "day_nap")
      }
    }
    # time spent in each class
    for (levelsc in 0:(length(Lnames) - 1)) {
      row[[paste0("dur_", Lnames[levelsc + 1], "_min")]] <-
        (length(which(LEVELS[sse] == levelsc)) * ws3new) / 60
    }
    onames <- c("dur_day_total_IN_min", "dur_day_total_LIG_min",
                "dur_day_total_MOD_min", "dur_day_total_VIG_min")
    for (g in 1:4) {
      row[[onames[g]]] <- (length(which(OLEVELS[sse] == g)) * ws3new) / 60
    }
    row$dur_day_min <- (length(which(ts$diur[sse] == 0)) * ws3new) / 60
    row$dur_spt_min <- (length(which(ts$diur[sse] == 1)) * ws3new) / 60
    row$dur_day_spt_min <- (length(c(sse)) * ws3new) / 60
    # number of wake periods longer than 5 minutes during the night
    Nawake <- length(which(abs(diff(which(LEVELS[sse] == 0))) > (300 / ws3new)))
    if (isTRUE(ggir_exact)) Nawake <- Nawake - 2
    if (Nawake < 0) Nawake <- 0
    row$N_atleast5minwakenight <- Nawake
    # sleep efficiency
    row$sleep_efficiency_after_onset <-
      length(which(ts$sibdetection[sse] == 1 &
                     ts$diur[sse] == 1)) / length(which(ts$diur[sse] == 1))
    # naps (estimation)
    if (do.sibreport == TRUE & "nap1_nonwear2" %in% colnames(ts) &
        length(nap_model) > 0) {
      row$nap_count <-
        length(which(diff(c(-1, which(ts$nap1_nonwear2[sse] == 1 & ts$diur[sse] == 0))) > 1))
      row$nap_totalduration <-
        round((sum(ts$nap1_nonwear2[sse[which(ts$nap1_nonwear2[sse] == 1 &
                                                ts$diur[sse] == 0)]]) * ws3new) / 60, digits = 2)
    }
    if (length(tail_expansion_log) != 0) {
      # do not store sleep variables if data was expanded in GGIR part 1
      row$tail_expansion_minutes <- (tail_expansion_log[["short"]] * ws3new) / 60
    } else {
      row$tail_expansion_minutes <- 0
    }
    # average ACC per window
    for (levelsc in 0:(length(Lnames) - 1)) {
      row[[paste("ACC_", Lnames[levelsc + 1], "_mg", sep = "")]] <-
        mean(ts$ACC[sse[LEVELS[sse] == levelsc]], na.rm = TRUE)
    }
    onames <- c("ACC_day_total_IN_mg", "ACC_day_total_LIG_mg",
                "ACC_day_total_MOD_mg", "ACC_day_total_VIG_mg")
    for (g in 1:4) {
      row[[onames[g]]] <- mean(ts$ACC[sse[OLEVELS[sse] == g]], na.rm = TRUE)
    }
    row$ACC_day_mg <- mean(ts$ACC[sse[ts$diur[sse] == 0]], na.rm = TRUE)
    row$ACC_spt_mg <- mean(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
    row$ACC_spt_mg_median <- stats::median(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
    row$ACC_spt_mg_stdev <- stats::sd(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
    row$ACC_day_spt_mg <- mean(ts$ACC[sse], na.rm = TRUE)
    # the steps block is commented out in GGIR and not transcribed
    # quantiles
    if (Nsegments == 1) {
      WLH <- ((qqq2 - qqq1) + 1) / ((60 / ws3new) * 60)
      if (WLH <= 1) WLH <- 1.001
    } else {
      WLH <- ((sum(qqq2 - qqq1)) + 1) / ((60 / ws3new) * 60)
    }
    # 3.3.6's ungated form; 3.3-9 gates on segments_names[si] and blanks both columns
    row$quantile_mostactive60min_mg <-
      as.numeric(stats::quantile(ts$ACC[sse], probs = ((WLH - 1) / WLH), na.rm = TRUE))
    row$quantile_mostactive30min_mg <-
      as.numeric(stats::quantile(ts$ACC[sse], probs = ((WLH - 0.5) / WLH), na.rm = TRUE))
    # GGIR's NaN blanking is dead on a character matrix and is not transcribed
    # number of bouts
    bc.mvpa <- .raw.timeuse.checkshape(bc.mvpa)
    bc.in <- .raw.timeuse.checkshape(bc.in)
    bc.lig <- .raw.timeuse.checkshape(bc.lig)
    if (nrow(bc.mvpa) != length(boutdur.mvpa) ||
        nrow(bc.in) != length(boutdur.in) ||
        nrow(bc.lig) != length(boutdur.lig)) {
      # GGIR would name the column Nbouts_day_IN_bts_NA
      stop("the bout indicator matrices in levelList must have one row per boutdur entry: ",
           "bc.mvpa ", nrow(bc.mvpa), "/", length(boutdur.mvpa),
           ", bc.in ", nrow(bc.in), "/", length(boutdur.in),
           ", bc.lig ", nrow(bc.lig), "/", length(boutdur.lig), call. = FALSE)
    }
    for (bci in 1:nrow(bc.mvpa)) {
      RLE <- rle(bc.mvpa[bci, sse])
      if (bci == 1) {
        bname <- paste0("Nbouts_day_MVPA_bts_", boutdur.mvpa[bci])
      } else {
        bname <- paste0("Nbouts_day_MVPA_bts_", boutdur.mvpa[bci], "_", boutdur.mvpa[bci - 1])
      }
      row[[bname]] <- length(which(RLE$values == 1))
    }
    for (bci in 1:nrow(bc.in)) {
      RLE <- rle(bc.in[bci, sse])
      if (bci == 1) {
        bname <- paste0("Nbouts_day_IN_bts_", boutdur.in[bci])
      } else {
        bname <- paste0("Nbouts_day_IN_bts_", boutdur.in[bci], "_", boutdur.in[bci - 1])
      }
      row[[bname]] <- length(which(RLE$values == 1))
    }
    for (bci in 1:nrow(bc.lig)) {
      RLE <- rle(bc.lig[bci, sse])
      if (bci == 1) {
        bname <- paste0("Nbouts_day_LIG_bts_", boutdur.lig[bci])
      } else {
        bname <- paste0("Nbouts_day_LIG_bts_", boutdur.lig[bci], "_", boutdur.lig[bci - 1])
      }
      row[[bname]] <- length(which(RLE$values == 1))
    }
    # number of blocks
    RLE_LEVELS <- rle(LEVELS[sse])
    RLE_OLEVELS <- rle(OLEVELS[sse])
    for (levelsc in 0:(length(Lnames) - 1)) {
      row[[paste("Nblocks_", Lnames[levelsc + 1], sep = "")]] <-
        length(which(RLE_LEVELS$values == levelsc))
    }
    onames <- c("Nblocks_day_total_IN", "Nblocks_day_total_LIG",
                "Nblocks_day_total_MOD", "Nblocks_day_total_VIG")
    for (g in 1:4) {
      row[[onames[g]]] <- length(which(RLE_OLEVELS$values == g))
    }
    row$boutcriter.in <- boutcriter.in
    row$boutcriter.lig <- boutcriter.lig
    row$boutcriter.mvpa <- boutcriter.mvpa
    row$boutdur.in <- paste(boutdur.in, collapse = "_")
    row$boutdur.lig <- paste(boutdur.lig, collapse = "_")
    row$boutdur.mvpa <- paste(boutdur.mvpa, collapse = "_")
    # intensity gradient over waking hours
    if (length(iglevels) > 0) {
      q55 <- cut(ts$ACC[sse[ts$diur[sse] == 0]], breaks = iglevels, right = FALSE)
      x_ig <- zoo::rollmean(iglevels, k = 2)
      y_ig <- (as.numeric(table(q55)) * ws3new) / 60 # converting to minutes
      ig <- as.numeric(.raw.intensity.gradient(x_ig, y_ig))
      row$ig_day_gradient <- ig[1]
      row$ig_day_intercept <- ig[2]
      row$ig_day_rsquared <- ig[3]
    }
    # intensity gradient over the full window
    if (length(iglevels) > 0) {
      q55 <- cut(ts$ACC[sse], breaks = iglevels, right = FALSE)
      x_ig <- zoo::rollmean(iglevels, k = 2)
      y_ig <- (as.numeric(table(q55)) * ws3new) / 60 # converting to minutes
      ig <- as.numeric(.raw.intensity.gradient(x_ig, y_ig))
      row$ig_day_spt_gradient <- ig[1]
      row$ig_day_spt_intercept <- ig[2]
      row$ig_day_spt_rsquared <- ig[3]
    }
    # fragmentation, daytime only; the spt window need not hold valid data in part 5
    if (length(frag.metrics) > 0) {
      fragmode <- "day"
      frag.out <- .raw.fragmentation(frag.metrics = frag.metrics,
                                     LEVELS = LEVELS[sse[ts$diur[sse] ==
                                                           ifelse(fragmode == "day", 0, 1)]],
                                     Lnames = Lnames, xmin = 60 / ws3new, mode = fragmode,
                                     ggir_exact = ggir_exact, warn_xmin = warn_xmin)
      frag.values <- round(as.numeric(frag.out), digits = 6)
      frag.names <- paste0("FRAG_", names(frag.out), "_", fragmode)
      for (fgi in seq_along(frag.values)) {
        row[[frag.names[fgi]]] <- frag.values[fgi]
      }
    }
    # light, if available
    if ("lightpeak" %in% colnames(ts) & length(LUX_day_segments) > 0) {
      # mean LUX
      if (length(which(ts$diur[sse] == 0)) > 0 & length(which(ts$diur[sse] == 1)) > 0) {
        row$LUX_max_day <- round(max(ts$lightpeak[sse[ts$diur[sse] == 0]], na.rm = TRUE),
                                 digits = 1)
        row$LUX_mean_day <- round(mean(ts$lightpeak[sse[ts$diur[sse] == 0]], na.rm = TRUE),
                                  digits = 1)
        row$LUX_mean_spt <- round(mean(ts$lightpeak[sse[ts$diur[sse] == 1]], na.rm = TRUE),
                                  digits = 1)
        row$LUX_mean_day_mvpa <- round(mean(ts$lightpeak[sse[ts$diur[sse] == 0 &
                                                               ts$ACC[sse] > TRMi]],
                                            na.rm = TRUE), digits = 1)
      } else {
        row$LUX_max_day <- NA
        row$LUX_mean_day <- NA
        row$LUX_mean_spt <- NA
        row$LUX_mean_day_mvpa <- NA
      }
      # time in LUX ranges
      Nluxt <- length(LUXthresholds)
      for (lti in 1:Nluxt) {
        if (lti < Nluxt) {
          row[[paste0("LUX_min_", LUXthresholds[lti], "_", LUXthresholds[lti + 1], "_day")]] <-
            length(which(ts$lightpeak[sse[ts$diur[sse] == 0]] >= LUXthresholds[lti] &
                           ts$lightpeak[sse[ts$diur[sse] == 0]] <
                             LUXthresholds[lti + 1])) / (60 / ws3new)
        } else {
          row[[paste0("LUX_min_", LUXthresholds[lti], "_inf_day")]] <-
            length(which(ts$lightpeak[sse[ts$diur[sse] == 0]] >=
                           LUXthresholds[lti])) / (60 / ws3new)
        }
      }
      # LUX per segment of the day
      if (timewindowi %in% c("WW", "OO", "MM")) {
        luxperseg <- .raw.timeuse.lux.persegment(ts, sse,
                                                 LUX_day_segments = LUX_day_segments,
                                                 epochSize = ws3new,
                                                 desiredtz = desiredtz)
        if (!is.null(unlist(luxperseg$values))) {
          for (lpi in seq_along(luxperseg$values)) {
            row[[luxperseg$names[lpi]]] <- luxperseg$values[lpi]
          }
        }
      }
    }
    # folder structure
    if (storefolderstructure == TRUE) {
      row$filename_dir <- fullFilename
      row$foldername <- foldernamei
    }
  } else {
    doNext <- TRUE
  }
  # restore ts$diur
  if (isTRUE(ggir_exact) && zeroed_diur) {
    ts$diur <- ts$diur_bu
    ts <- ts[, -which(colnames(ts) == "diur_bu")]
  }
  dsummary[[si]] <- row
  indexlog <- list(fileIndex = fileIndex,
                   winStartEnd = qqq,
                   segIndex1 = si,
                   segIndex2 = current_segment_i,
                   segStartEnd = c(segStart, segEnd),
                   columnIndex = fi + length(row))
  timeList <- list(ts = ts,
                   epochSize = ws3new,
                   LEVELS = LEVELS,
                   Lnames = Lnames)
  invisible(list(
    indexlog = indexlog,
    ds_names = names(row),
    dsummary = dsummary,
    timeList = timeList,
    doNext = doNext,
    add_one_day_to_next_date = add_one_day_to_next_date
  ))
}
