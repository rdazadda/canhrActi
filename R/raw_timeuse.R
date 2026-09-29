# Ported from GGIR 3.3-9 R/g.part5.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The folder, parallel and file layer is
# gone: the milestone loads are members of the objects the caller passes in, the per-file
# body main_part5 is raw.timeuse(), the 500-column character matrix is a named-list row
# accumulator converted once at the end, every file write is a member of the returned
# object, the silent no-output paths are states on it, the LC_TIME locale is forced to
# "C" for the call, and the unreachable else arm of the fixmissingnight gate is not
# transcribed.

# AGGREGATION TO ONE MINUTE EPOCHS

#' Aggregate the Part-5 Time Series to One Minute Epochs
#'
#' Runs after \code{ts$time} has become POSIXct and before the sib report. Every duration in
#' the day table then comes out in whole minutes and the fragmentation \code{xmin} is one
#' epoch rather than twelve.
#'
#' @details The grouping key is \code{floor(as.numeric(ts$time) / 60) * 60}. \code{ACC},
#'   \code{sibdetection}, \code{diur}, \code{nonwear}, the angle, light and temperature
#'   columns are aggregated by mean, \code{step_count} by sum and \code{guider} by its first
#'   value; the three 0/1 columns are then rounded back, with R's banker's rounding. Columns
#'   not named here (\code{window}, \code{segment}, \code{selfreported}) are dropped, which
#'   is why GGIR runs this before the window loop. The caller restores the 5 s series at the
#'   top of every sib-definition iteration so it is never aggregated twice.
#'
#' @param ts The part-5 time series with \code{time} already POSIXct.
#' @param desiredtz Olson timezone name; "" means the system zone.
#' @param dayborder GGIR's dayborder, in decimal hours.
#' @param lightpeak_available TRUE when part 1 produced a light channel.
#' @param tail_expansion_log The part-1 tail expansion log, or NULL; a non-empty log appends
#'   the last epoch to the midnight grid.
#' @return list(ts, ws3new, nightsi, sec, min, hour, time_POSIX, Nts).
#' @keywords internal
#' @noRd
.raw.timeuse.aggregate60 <- function(ts, desiredtz = "", dayborder = 0,
                                     lightpeak_available = FALSE,
                                     tail_expansion_log = NULL) {
  ts$time_num <- floor(as.numeric(ts$time) / 60) * 60
  # only include angle if angle is present
  angleColName <- grep(pattern = "angle", x = names(ts), value = TRUE)
  if (lightpeak_available == TRUE) {
    light_columns <- c("lightpeak", "lightpeak_imputationcode")
  } else {
    light_columns <- NULL
  }
  temperature_col <- grep(pattern = "temperature", x = names(ts), value = TRUE)
  stepcount_available <- ifelse("step_count" %in% names(ts), yes = TRUE, no = FALSE)
  if (stepcount_available) {
    step_count_tmp <- stats::aggregate(ts$step_count, by = list(ts$time_num),
                                       FUN = function(x) sum(x))
    colnames(step_count_tmp)[2] <- "step_count"
  }
  # guider takes the first value in the minute
  agg_guider <- stats::aggregate(ts$guider, by = list(ts$time_num),
                                 FUN = function(x) x[1])
  colnames(agg_guider)[2] <- "guider"
  ts <- stats::aggregate(ts[, c("ACC", "sibdetection", "diur", "nonwear", angleColName,
                                light_columns, temperature_col)],
                         by = list(ts$time_num), FUN = function(x) mean(x))
  ts <- base::merge(x = ts, y = agg_guider, by = "Group.1")
  if (stepcount_available) {
    ts <- base::merge(x = ts, y = step_count_tmp, by = "Group.1")
  }
  ts$sibdetection <- round(ts$sibdetection)
  ts$diur <- round(ts$diur)
  ts$nonwear <- round(ts$nonwear)
  names(ts)[1] <- "time"
  ts$time <- as.POSIXct(ts$time, origin = "1970-1-1", tz = desiredtz)
  ws3new <- 60
  # extract nightsi again
  mid <- .raw.timeuse.midnights(ts$time, dayborder = dayborder)
  nightsi <- mid$nightsi
  if (length(tail_expansion_log) != 0 & nrow(ts) > max(nightsi)) {
    nightsi[length(nightsi) + 1] <- nrow(ts) # include last window
  }
  invisible(list(ts = ts, ws3new = ws3new, nightsi = nightsi,
                 sec = mid$sec, min = mid$min, hour = mid$hour,
                 time_POSIX = mid$time_POSIX, Nts = nrow(ts)))
}

# DAY TABLE SHAPE

#' The Behaviour Class Names for a Bout Configuration
#'
#' Ids 0 to 8 are fixed and 9 upward are one per element of the three bout duration vectors.
#' Returns the vector \code{.raw.identify.levels()} would return, without needing a series,
#' so a recording with no analysable window still gets a day table with the right columns.
#'
#' @details Band 1 is \code{<class>_bts_<d1>} and band k above 1 is
#'   \code{<class>_bts_<dk>_<d(k-1)>}. The bout durations must already be sorted descending,
#'   as \code{raw.timeuse()} sorts them.
#'
#' @param boutdur.mvpa,boutdur.in,boutdur.lig Bout duration bands in minutes, descending.
#' @return A character vector of class names, in class order.
#' @keywords internal
#' @noRd
.raw.timeuse.level.names <- function(boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
                                     boutdur.lig = c(10, 5, 1)) {
  Lnames <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
  Lnames <- c(Lnames, "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt")
  band <- function(stem, dur) {
    out <- character(0)
    if (length(dur) == 0) return(out)
    for (BL in seq_along(dur)) {
      if (BL == 1) {
        out <- c(out, paste0(stem, dur[BL]))
      } else {
        out <- c(out, paste0(stem, dur[BL], "_", dur[BL - 1]))
      }
    }
    out
  }
  Lnames <- c(Lnames, band("day_MVPA_bts_", boutdur.mvpa))
  Lnames <- c(Lnames, band("day_IN_bts_", boutdur.in))
  Lnames <- c(Lnames, band("day_LIG_bts_", boutdur.lig))
  Lnames
}

#' The Column Names of a Default Day Table
#'
#' The shape of the day table with no optional block switched on, used only for the empty
#' frame \code{raw.timeuse()} returns when no window was analysable. A frame with rows takes
#' its names from the rows.
#'
#' @details The block order is the order \code{g.part5_analyseSegment} writes in. The
#'   \code{dur_}, \code{ACC_} and \code{Nbouts_} columns are named from \code{Lnames}, so a
#'   non-default bout configuration gives a different column set.
#'
#' @param Lnames Class names from \code{.raw.timeuse.level.names()}.
#' @return A character vector of column names.
#' @keywords internal
#' @noRd
.raw.timeuse.default.columns <- function(Lnames) {
  bouted <- Lnames[grep("_bts_", Lnames)]
  c("ID", "filename",
    "weekday", "calendar_date",
    "window_number", "window", "start_end_window",
    "sleeponset", "sleeponset_ts", "wakeup", "wakeup_ts",
    "sleepparam",
    "night_number", "daysleeper", "cleaningcode", "guider", "sleeplog_used",
    "acc_available",
    "TRLi", "TRMi", "TRVi",
    "nonwear_perc_day", "nonwear_perc_spt", "nonwear_perc_day_spt",
    paste0("dur_", Lnames, "_min"),
    paste0("dur_day_total_", c("IN", "LIG", "MOD", "VIG"), "_min"),
    "dur_day_min", "dur_spt_min", "dur_day_spt_min",
    "N_atleast5minwakenight", "sleep_efficiency_after_onset", "tail_expansion_minutes",
    paste0("ACC_", Lnames, "_mg"),
    paste0("ACC_day_total_", c("IN", "LIG", "MOD", "VIG"), "_mg"),
    "ACC_day_mg", "ACC_spt_mg", "ACC_spt_mg_median", "ACC_spt_mg_stdev", "ACC_day_spt_mg",
    "quantile_mostactive60min_mg", "quantile_mostactive30min_mg",
    paste0("Nbouts_", bouted),
    paste0("Nblocks_", Lnames),
    paste0("Nblocks_day_total_", c("IN", "LIG", "MOD", "VIG")),
    "boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
    "boutdur.in", "boutdur.lig", "boutdur.mvpa",
    "GGIRversion")
}

#' Merge One Row's Column Names Into the Running Column Order
#'
#' The named-list accumulator replaces GGIR's single \code{ds_names} vector shared by every
#' row, so the column order has to be assembled. A run of names this row has and the running
#' order does not goes immediately before the next name they share; a run at the end of the
#' row goes after the last shared name; a row that shares nothing is appended whole. With the
#' default configuration every row carries the same names and the merge is a no-op after the
#' first row.
#'
#' @param master The running column order; \code{character(0)} on the first row.
#' @param nm The names of the row just written.
#' @return The merged column order.
#' @keywords internal
#' @noRd
.raw.timeuse.merge.names <- function(master, nm) {
  if (length(master) == 0) return(nm)
  if (length(nm) == 0) return(master)
  out <- master
  pending <- character(0)
  prev <- 0L # position in out of the last shared name
  for (k in seq_along(nm)) {
    j <- match(nm[k], out)
    if (is.na(j)) {
      pending <- c(pending, nm[k])
      next
    }
    if (length(pending) > 0) {
      out <- append(out, pending, after = j - 1L)
      j <- j + length(pending)
      pending <- character(0)
    }
    prev <- j
  }
  if (length(pending) > 0) {
    out <- append(out, pending, after = if (prev > 0L) prev else length(out))
  }
  out
}

#' Turn the Row Accumulator Into GGIR's Character Day Table
#'
#' @details The NA sweep is done cell by cell in \code{.raw.timeuse.row.chr}, which keeps a
#'   NaN as the string "NaN"; both empty markers are in GGIR's own output. The frame is built
#'   from a character matrix so the \code{row.names} attribute matches a stored milestone
#'   under \code{identical()}. GGIR's empty-row drop is applied although a written row always
#'   carries ID and filename.
#'
#' @param dsummary The list of named rows, one per output row.
#' @param ds_names The merged column order.
#' @return A character data.frame.
#' @keywords internal
#' @noRd
.raw.timeuse.bind <- function(dsummary, ds_names) {
  n <- length(dsummary)
  mat <- matrix("", nrow = n, ncol = length(ds_names))
  if (n > 0) {
    for (i in seq_len(n)) {
      r <- .raw.timeuse.row.chr(dsummary[[i]])
      if (length(r) > 0) mat[i, match(names(r), ds_names)] <- r
    }
  }
  output <- data.frame(mat, stringsAsFactors = FALSE)
  names(output) <- ds_names
  # drop rows with neither ID nor filename
  if (ncol(output) >= 2 && nrow(output) > 0) {
    emptyrows <- which(output[, 1] == "" & output[, 2] == "")
    if (length(emptyrows) > 0) output <- output[-emptyrows, ]
  }
  output
}

# ENTRY POINT

#' GGIR Part 5: Time-Use Analysis Over the Day and the Sleep Period
#'
#' Combines the part-1 epoch tables, the part-2 imputed series, the part-3 sustained
#' inactivity bouts and the part-4 night summary into one row per analysis window, with a
#' closed 24 hour accounting: every epoch falls into exactly one of eighteen behaviour
#' classes, five inside the sleep period and thirteen outside it, and the class durations sum
#' to the window length. GGIR loops over sib definition, the three thresholds and the window
#' definition and stacks every combination into the same table, with \code{sleepparam},
#' \code{TRLi}, \code{TRMi}, \code{TRVi} and \code{window} as the discriminating columns.
#'
#' Units: \code{ts$ACC} is milli-g for a g-unit metric such as ENMO and counts per epoch
#' (not per minute) for a count metric, so \code{threshold.lig = 40} means 0.040 g. The
#' \code{dur_} columns are minutes, the \code{ACC_} columns milli-g, the \code{nonwear_perc_}
#' columns percentages and \code{sleep_efficiency_after_onset} a fraction. The fragmentation
#' block reports durations in epochs.
#'
#' The four \code{dur_day_total_*_min} columns are the cut-point partition of waking time and
#' the thirteen \code{dur_day_*} class columns are the bout partition. Each sums to
#' \code{dur_day_min}, but a bout absorbs gaps of up to one minute of a different intensity,
#' so an \code{_unbt_} column plus the bout classes of the same intensity is not the
#' \code{_total_} column.
#'
#' The default thresholds 40, 100 and 400 mg are GGIR's rounded Hildebrand adult
#' non-dominant wrist ENMO values. For older adults Migueles (2021) gives 18 and 60 mg for
#' the same sensor position: \code{threshold.lig = 18, threshold.mod = 60}.
#'
#' @details Transcribes \code{main_part5} of GGIR 3.3-9. Per sib definition: midnights,
#'   addsib, fixmissingnight, wakesleep, addfirstwake, the optional 60 s aggregation and the
#'   sib report; then per threshold triple \code{identify_levels} once, so bouts are found
#'   over the whole recording and a bout crossing midnight is counted in \code{Nbouts_} for
#'   both days; then per timewindow the trim, \code{define.days} and \code{analyseSegment}
#'   over each window; then \code{savetimeseries} once per triple, after the window loop, so
#'   the series GGIR writes carries the last timewindow's \code{window} column beside a
#'   \code{guider} column accumulated across all of them. \code{ts} is shared state across
#'   the loop nest: only \code{ts$window} is reset per timewindow pass. The bout durations
#'   are sorted descending, as GGIR does, because each band only sees what the longer band
#'   did not claim.
#'
#'   The port follows 3.3-9 except in four places: \code{definedays} keeps 3.3.6's WW and OO
#'   branch, so \code{start_end_window} on a WW row is the real clock edge (the one column
#'   the version choice changes; \code{segment_all_windows = TRUE} selects 3.3-9's form);
#'   \code{analyseSegment} keeps 3.3.6's ungated \code{quantile_mostactive*} writes, which
#'   3.3-9 blanks after the first row; \code{lux_persegment} uses the 3.3-9 body, because
#'   3.3.6 fails on a recording that does not start on a segment boundary; and
#'   \code{savetimeseries} uses 3.3-9's guider table, a strict superset. Not transcribed:
#'   the folder and parallel layer (a missing milestone is a named error here and the class
#'   legend is returned as an object), the unreachable else arm of the fixmissingnight gate,
#'   \code{options(encoding = "UTF-8")} and the \code{angle} column drop after the export.
#'
#'   GGIR writes no ms5 file, and says nothing, for a recording with no part-4 night, no sib
#'   definition or no window that survives the 900 second gate. Each is a
#'   \code{$status$state}: "skipped", "no_nights", "no_valid_night", "no_sib_definitions",
#'   "no_windows", and "ok" only when GGIR would have written the file. The \code{windows}
#'   table has no GGIR equivalent: it records every window attempted, the gate it passed or
#'   failed, every midnight the trim discarded and the span no window covered. The LC_TIME
#'   locale is forced to "C" for the duration of the call and restored on exit.
#'
#' @param x A \code{canhrActi_raw} from \code{\link{read.raw.accelerometer}} or
#'   \code{\link{read.ggir.milestone}}, carrying \code{$imputed} (the part-2 series part 5
#'   reads, never \code{$meta$metashort}) and \code{$sleep} (the part-3 result). Its
#'   \code{$tz$desiredtz} overrides the parameter, as in GGIR.
#' @param nights The part-4 night summary: a \code{canhrActi_raw_nights} from
#'   \code{\link{raw.sleep.nights}}, or the plain data frame GGIR stores as
#'   \code{nightsummary}.
#' @param params A \code{\link{raw.params}} object, or NULL to reuse the parameters the
#'   recording was read with. The checker is re-run on entry, as g.part5 does.
#' @param progress NULL or a \code{function(stage, i, n, message)}, called once per
#'   (sib definition, threshold triple, timewindow) pass with the stage name "timeuse".
#' @param sleeplog NULL, the list \code{\link{raw.sleeplog}} returns, or a data frame already
#'   in the long per-night diary shape. With NULL and a \code{loglocation} parameter set, the
#'   diary is read here.
#' @param segment_all_windows FALSE, the default, is GGIR 3.3.6's day segmentation: qwindow
#'   segments for MM only and real clock edges in \code{start_end_window} on a WW or OO row.
#'   TRUE is GGIR 3.3-9's: segments for every timewindow, the timewindow prefixed onto the
#'   segment name and the qwindow label in \code{start_end_window} everywhere.
#' @param ... Individual parameter overrides, for example \code{threshold.lig = 18}.
#'
#' @return An object of class "canhrActi_raw_timeuse":
#' \describe{
#'   \item{daysummary}{GGIR's \code{output} frame: one row per (sib definition, threshold
#'     triple, timewindow, window, segment), every cell a character string. 119 columns at
#'     the default configuration.}
#'   \item{series}{The exported time series, keyed by threshold triple ("40_100_400"), then
#'     sib definition, then timewindow, each the 14-column \code{mdat} frame. NULL when
#'     \code{save_ms5rawlevels} is FALSE.}
#'   \item{levels}{\code{Lnames}, the class names: a character vector when every threshold
#'     triple gave the same ladder, a named list keyed by triple otherwise.}
#'   \item{legend}{The class dictionary GGIR writes to \code{behavioralcodes<date>.csv}.}
#'   \item{sibreport}{A named list keyed by sib definition of \code{\link{raw.sib.report}}
#'     results, or NULL when \code{do.sibreport} is FALSE.}
#'   \item{last_timestamp}{The last epoch of the series, GGIR's own ms5 object.}
#'   \item{windows}{One row per window attempt, see the details.}
#'   \item{settings}{The effective parameters, the threshold grid, the sib definition
#'     labels, the timezone and \code{ggir_version_label}.}
#'   \item{status}{\code{state}, \code{messages} and \code{stage_times}.}
#' }
#'
#' @section Comparison with canhrActi's count-based functions:
#' Nothing here goes through \code{\link{apply_cutpoints}}. The count-based cut points take
#' counts per minute and return a five-level factor; part 5 takes milli-g (or counts per
#' epoch) and returns an integer class per epoch that encodes intensity, waking against
#' sleep period and bout membership together. The bout detectors and the fragmentation
#' metrics differ in the same way, and neither is a parameterisation of the other.
#'
#' @seealso \code{\link{raw.sleep.nights}} for the input, \code{\link{as.ggir.ms5}} for the
#'   GGIR-shaped milestone, \code{\link{raw.sib.report}} for the sib report,
#'   \code{\link{raw.params}} for the parameters and \code{\link{apply_cutpoints}} for the
#'   count-based pipeline this one does not replace.
#'
#' @examples
#' \dontrun{
#' x <- read.raw.accelerometer("MOS2E39230594.gt3x", desiredtz = "America/Anchorage")
#' x$imputed <- raw.impute(x)
#' x$sleep <- raw.sleep.part3(x)
#' nights <- raw.sleep.nights(x)
#' tu <- raw.timeuse(x, nights)
#' tu$daysummary[, c("window", "calendar_date", "dur_day_min", "dur_spt_min")]
#' rep <- raw.sleep.report(nights, part5 = tu$daysummary)
#' }
#' @export
raw.timeuse <- function(x, nights, params = raw.params(), progress = NULL, sleeplog = NULL,
                        segment_all_windows = FALSE, ...) {
  t_total <- Sys.time()
  # C time locale, so the weekday column is the same on every machine
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
  if (!is.logical(segment_all_windows) || length(segment_all_windows) != 1 ||
      is.na(segment_all_windows)) {
    stop("segment_all_windows must be TRUE or FALSE", call. = FALSE)
  }
  # Parameters; the bout durations are sorted descending, which check_params does not do
  params <- .raw.calibrate.params(list(params = x$params), params, list(...))
  params <- .raw.params.check(params)
  gp <- function(name, default = NULL) .raw.param(params, name, default)
  boutdur.mvpa <- sort(gp("boutdur.mvpa", c(1, 5, 10)), decreasing = TRUE)
  boutdur.lig <- sort(gp("boutdur.lig", c(1, 5, 10)), decreasing = TRUE)
  boutdur.in <- sort(gp("boutdur.in", c(10, 20, 30)), decreasing = TRUE)
  boutcriter.mvpa <- gp("boutcriter.mvpa", 0.8)
  boutcriter.lig <- gp("boutcriter.lig", 0.8)
  boutcriter.in <- gp("boutcriter.in", 0.9)
  threshold.lig <- gp("threshold.lig", 40)
  threshold.mod <- gp("threshold.mod", 100)
  threshold.vig <- gp("threshold.vig", 400)
  timewindow <- gp("timewindow", c("MM", "WW"))
  dayborder <- gp("dayborder", 0)
  agg2_60 <- isTRUE(gp("part5_agg2_60seconds", FALSE))
  do.sibreport <- isTRUE(gp("do.sibreport", TRUE))
  save_ms5rawlevels <- isTRUE(gp("save_ms5rawlevels", TRUE))
  storefolderstructure <- isTRUE(gp("storefolderstructure", FALSE))
  method_research_vars <- gp("method_research_vars", NULL)
  ggir_exact <- isTRUE(gp("ggir_exact", TRUE))
  ggir_version_label <- gp("ggir_version_label", .RAW_GGIR_VERSION_LABEL)
  loglocation <- gp("loglocation", NULL)
  sleepwindowType <- gp("sleepwindowType", "SPT")
  HASPT.algo <- gp("HASPT.algo", "HDCZA")
  qwindow <- gp("qwindow", c(0, 24))
  frag.metrics <- gp("frag.metrics", NULL)
  DaCleanFile <- .raw.timeuse.cleaning.file(gp("data_cleaning_file", NULL))
  messages <- character()
  stage_times <- c(series = NA_real_, sleep = NA_real_, sibreport = NA_real_,
                   levels = NA_real_, windows = NA_real_, timeseries = NA_real_,
                   total = NA_real_)
  # Milestone inputs
  meta <- x$meta
  if (!is.list(meta) || !"metalong" %in% names(meta)) {
    stop("x carries no part-1 epoch tables (x$meta); part 5 reads M$metalong and ",
         "M$metashort", call. = FALSE)
  }
  # un-factor the part-1 tables of an older milestone; a no-op on anything read here
  meta$metashort <- .raw.correct.older.milestone(meta$metashort)
  meta$metalong <- .raw.correct.older.milestone(meta$metalong)
  IMP <- x$imputed
  if (inherits(IMP, "canhrActi_raw_imputed")) IMP <- unclass(IMP)
  if (!is.list(IMP) || !all(c("metashort", "rout") %in% names(IMP))) {
    stop("x carries no part-2 imputed series (x$imputed); run raw.impute() first. Part 5 ",
         "reads the IMPUTED short epochs, never x$meta$metashort", call. = FALSE)
  }
  p3 <- x$sleep
  if (!is.list(p3) || !"sib.cla.sum" %in% names(p3)) {
    stop("x carries no part-3 result (x$sleep); run raw.sleep.part3() first", call. = FALSE)
  }
  # part 1's timezone wins over the parameter
  desiredtz_part1 <- x$tz$desiredtz
  if (!is.null(desiredtz_part1)) {
    params[["desiredtz"]] <- desiredtz_part1
  } else {
    desiredtz_part1 <- gp("desiredtz", "")
  }
  desiredtz <- .raw.param(params, "desiredtz", "")
  # filename_dir is stored beside the exported series and read nowhere
  filename_dir <- if (is.list(x$file)) x$file$filename else NULL
  if (is.null(filename_dir) || length(filename_dir) != 1 || is.na(filename_dir)) {
    filename_dir <- NA_character_
  } else {
    filename_dir <- as.character(filename_dir)
  }
  # storefolderstructure: GGIR scans datadir for the original file; the path is known here
  fullFilename <- foldernamei <- NA_character_
  if (storefolderstructure == TRUE) {
    fp <- if (is.list(x$file)) x$file$path else NULL
    if (!is.null(fp) && length(fp) == 1 && !is.na(fp)) {
      fullFilename <- as.character(fp)
      foldernamei <- basename(dirname(fullFilename))
    }
  }
  # the tail_expansion_log that survives into ms5 is ms3's
  tail_expansion_log <- p3$tail_expansion_log
  SPTE_end <- p3$SPTE_end
  if (is.null(SPTE_end)) SPTE_end <- c()
  longitudinal_axis <- p3$longitudinal_axis
  sib.cla.sum <- p3$sib.cla.sum
  ws3 <- meta$windowsizes[1]
  # Part-4 night summary
  if (inherits(nights, "canhrActi_raw_nights")) {
    summarysleep <- as.ggir.nightsummary(nights)
  } else if (is.data.frame(nights)) {
    summarysleep <- as.data.frame(nights)
  } else {
    stop("nights must be a canhrActi_raw_nights from raw.sleep.nights(), or GGIR's ",
         "nightsummary data frame", call. = FALSE)
  }
  # the part-3 milestone name is column 2 of every row and the key into the night summary
  fname_ms3 <- NA_character_
  if ("filename" %in% names(summarysleep) && nrow(summarysleep) > 0) {
    fname_ms3 <- as.character(summarysleep$filename)[1]
  }
  if (is.na(fname_ms3) && !is.na(filename_dir)) {
    fname_ms3 <- paste0(basename(filename_dir), ".RData")
  }
  idindex <- if ("filename" %in% names(summarysleep)) {
    which(as.character(summarysleep$filename) == fname_ms3)
  } else {
    seq_len(nrow(summarysleep))
  }
  ID <- if (length(idindex) > 0 && "ID" %in% names(summarysleep)) {
    as.character(summarysleep$ID[idindex[1]])
  } else {
    as.character(x$inspection$id)
  }
  if (length(ID) != 1 || is.na(ID)) ID <- NA_character_
  # Empty shapes for the early returns
  Lnames_default <- .raw.timeuse.level.names(boutdur.mvpa, boutdur.in, boutdur.lig)
  empty_day <- function() {
    nm <- .raw.timeuse.default.columns(Lnames_default)
    out <- data.frame(matrix("", nrow = 0, ncol = length(nm)), stringsAsFactors = FALSE)
    names(out) <- nm
    out
  }
  settings <- list(
    windowsizes = meta$windowsizes, desiredtz = desiredtz, dayborder = dayborder,
    timewindow = timewindow, part5_agg2_60seconds = agg2_60,
    threshold.lig = threshold.lig, threshold.mod = threshold.mod,
    threshold.vig = threshold.vig,
    boutdur.mvpa = boutdur.mvpa, boutdur.lig = boutdur.lig, boutdur.in = boutdur.in,
    boutcriter.mvpa = boutcriter.mvpa, boutcriter.lig = boutcriter.lig,
    boutcriter.in = boutcriter.in,
    qwindow = qwindow, segment_all_windows = segment_all_windows,
    sleepparam = character(0), ID = ID, filename = fname_ms3,
    filename_dir = filename_dir, ggir_exact = ggir_exact,
    ggir_version_label = ggir_version_label, params = params)
  finish <- function(obj, state) {
    stage_times[["total"]] <- as.numeric(difftime(Sys.time(), t_total, units = "secs"))
    obj$settings <- settings
    obj$status <- list(state = state, messages = messages, stage_times = stage_times)
    obj$elapsed <- stage_times[["total"]]
    structure(obj, class = "canhrActi_raw_timeuse")
  }
  empty <- function() {
    list(daysummary = empty_day(), series = NULL, levels = Lnames_default,
         legend = .raw.timeuse.class.dictionary(Lnames_default), sibreport = NULL,
         last_timestamp = NULL, tail_expansion_log = tail_expansion_log,
         GGIRversion = ggir_version_label, windows = .raw.timeuse.empty.windows())
  }
  # States; GGIR writes no file and logs nothing for any of these
  corrupt <- isTRUE(meta$filecorrupt) || isTRUE(x$status$corrupt) || isTRUE(x$status$skipped)
  too_short <- isTRUE(meta$filetooshort) || isTRUE(x$status$too_short)
  if (corrupt || too_short) {
    messages <- c(messages,
                  paste0("The recording is ",
                         if (corrupt) "corrupt or was skipped" else "too short for part 1",
                         "; GGIR writes no part-5 milestone for it."))
    return(finish(empty(), "skipped"))
  }
  if (length(idindex) == 0 || nrow(summarysleep) == 0) {
    messages <- c(messages,
                  paste0("The part-4 night summary holds no row whose filename is \"",
                         fname_ms3, "\"",
                         if (nrow(summarysleep) > 0) {
                           paste0(" (it holds ",
                                  paste(unique(as.character(summarysleep$filename)),
                                        collapse = ", "), ")")
                         } else {
                           " and has no rows at all"
                         },
                         "; GGIR writes no part-5 milestone for it."))
    return(finish(empty(), "no_nights"))
  }
  if (is.null(sib.cla.sum) || nrow(sib.cla.sum) == 0) {
    messages <- c(messages,
                  paste0("Part 3 found no sustained inactivity bout, so there is no sleep ",
                         "definition to analyse."))
    return(finish(empty(), "no_sib_definitions"))
  }
  # Sleep diary: the object part 4 was given, or read from loglocation
  logs_diaries <- c()
  if (is.data.frame(sleeplog)) {
    logs_diaries <- if (sleepwindowType == "TimeInBed") {
      list(sleeplog = NULL, bedlog = sleeplog)
    } else {
      list(sleeplog = sleeplog, bedlog = NULL)
    }
  } else if (is.list(sleeplog)) {
    logs_diaries <- sleeplog
  } else if (length(loglocation) > 0) {
    logs_diaries <- raw.sleeplog(loglocation, colid = gp("colid", 1), coln1 = gp("coln1", 2),
                                 sleepwindowType = sleepwindowType, desiredtz = desiredtz,
                                 rec_starttime = p3$rec_starttime, id = ID)
  }
  sleeplog_frame <- c()
  if (length(logs_diaries) > 0) {
    if (is.list(logs_diaries)) {
      if (sleepwindowType == "TimeInBed" && length(logs_diaries$bedlog) > 0) {
        sleeplog_frame <- logs_diaries$bedlog
      } else {
        sleeplog_frame <- logs_diaries$sleeplog
      }
    } else {
      sleeplog_frame <- logs_diaries
    }
  }
  if (is.null(sleeplog_frame)) sleeplog_frame <- c()
  # Tail-expansion trimming: part 5 analyses the unexpanded series
  if (length(tail_expansion_log) != 0) {
    expanded_short <- which(IMP$r5long == -1)
    expanded_long <- which(IMP$rout$r5 == -1)
    if (length(expanded_short) > 0) {
      IMP$metashort <- IMP$metashort[-expanded_short, ]
      meta$metashort <- meta$metashort[-expanded_short, ]
    }
    if (length(expanded_long) > 0) {
      IMP$rout <- IMP$rout[-expanded_long, ]
      meta$metalong <- meta$metalong[-expanded_long, ]
    }
  }
  # The time series
  t0 <- Sys.time()
  ts0 <- .raw.timeuse.init.ts(IMP, meta,
                              acc_metric = gp("acc.metric", "ENMO"),
                              sensor_location = gp("sensor.location", "wrist"),
                              lux_cal_constant = gp("LUX_cal_constant", c()),
                              lux_cal_exponent = gp("LUX_cal_exponent", c()),
                              longitudinal_axis = longitudinal_axis)
  Nts <- nrow(ts0)
  lightpeak_available <- "lightpeak" %in% names(ts0)
  Nepochsinhour <- (60 / ws3) * 60
  stage_times[["series"]] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # xmin for the power-law fragmentation metrics is 60/epoch epochs; warn once per call
  frag_epoch <- if (agg2_60) 60 else ws3
  if (length(frag.metrics) > 0 && frag_epoch != 60) {
    msg <- paste0("The fragmentation metrics are being computed at a ", frag_epoch,
                  " s epoch, where GGIR's xmin is ", 60 / frag_epoch,
                  " epochs rather than one minute. alpha_dur_*, x0.5_dur_* and W0.5_dur_* ",
                  "are not interpretable there; part5_agg2_60seconds = TRUE makes xmin 1.")
    messages <- c(messages, msg)
    warning(msg, call. = FALSE)
  }
  # Sib summary; def is taken before the cut, so a fully cut definition still iterates
  S <- sib.cla.sum
  def <- unique(S$definition)
  cut <- which(S$fraction.night.invalid > 0.9 | S$nsib.periods == 0)
  if (length(cut) > 0) S <- S[-cut, ]
  # Remove impossible entries
  summarysleep_tmp <- summarysleep
  pko <- which(summarysleep_tmp$sleeponset == 0 & summarysleep_tmp$wakeup == 0 &
                 summarysleep_tmp$SptDuration == 0)
  if (length(pko) > 0) summarysleep_tmp <- summarysleep_tmp[-pko, ]
  # if expanded time with expand_tail_max_hours, then latest wakeup might not be in data
  if (length(tail_expansion_log) != 0 && nrow(S) > 0) {
    last_night <- S[which(S$night == max(S$night)), ]
    last_wakeup <- last_night$sib.end.time[which(last_night$sib.period ==
                                                   max(last_night$sib.period))]
    if (!(last_wakeup %in% ts0$time)) {
      replaceLastWakeup <- which(S$sib.end.time == last_wakeup)
      S$sib.end.time[replaceLastWakeup] <- ts0$time[nrow(ts0)]
    }
  }
  settings$sleepparam <- as.character(def)
  # The loop nest
  dsummary <- list()
  ds_names <- character()
  di <- 1
  nwritten <- 0L
  series <- list()
  levels_by_triple <- list()
  sibreports <- list()
  legend <- NULL
  time_POSIX <- NULL
  winrows <- list()
  # the unaggregated series is restored at the top of every sib definition when aggregating
  ts_backup <- ts0
  ts <- ts0
  npass <- length(def) * length(threshold.lig) * length(threshold.mod) *
    length(threshold.vig) * length(timewindow)
  ipass <- 0L
  t_sleep <- t_sib <- t_lev <- t_win <- t_ser <- 0
  for (sibDef in def) {
    ws3new <- ws3 # the aggregation may have changed it
    if (agg2_60 == TRUE) ts <- ts_backup
    # Midnights
    t0 <- Sys.time()
    mid <- .raw.timeuse.midnights(.raw.iso8601.to.posix(ts$time, tz = desiredtz),
                                  dayborder = dayborder, time_char = ts$time,
                                  desiredtz = desiredtz)
    time_POSIX <- mid$time_POSIX
    sec <- mid$sec; min <- mid$min; hour <- mid$hour
    nightsi <- mid$nightsi
    nightsi2 <- mid$nightsi2
    if (length(tail_expansion_log) != 0 && nrow(ts) > max(nightsi)) {
      nightsi[length(nightsi) + 1] <- nrow(ts)
    }
    summarysleep_tmp2 <- summarysleep_tmp[which(summarysleep_tmp$sleepparam == sibDef), ]
    # Add sustained inactivity bouts (sib) to the time series
    ts <- .raw.timeuse.addsib(ts, epochSize = ws3new,
                              part3_output = S[S$definition == sibDef, ],
                              desiredtz = desiredtz, sibDefinition = sibDef,
                              nightsi = nightsi, ggir_exact = ggir_exact)
    if (nrow(summarysleep_tmp2) > 0) {
      if (!all(is.na(summarysleep_tmp$sleepparam))) {
        # Fix missing nights in part 4 data
        summarysleep_tmp2 <- .raw.timeuse.fixmissingnight(summarysleep_tmp2,
                                                          sleeplog = sleeplog_frame, ID)
      }
    }
    # 0 if wake/daytime, 1 if sleep/nighttime
    ts$diur <- 0
    t_sleep <- t_sleep + as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (nrow(summarysleep_tmp2) == 0) next
    t0 <- Sys.time()
    ts <- .raw.timeuse.wakesleep(ts, part4_output = summarysleep_tmp2,
                                 desiredtz = desiredtz, nightsi = nightsi2,
                                 sleeplog = sleeplog_frame, epochSize = ws3, ID = ID,
                                 Nepochsinhour = Nepochsinhour)
    ts <- .raw.timeuse.addfirstwake(ts, summarysleep = summarysleep_tmp2, nightsi = nightsi,
                                    sleeplog = sleeplog_frame, ID = ID,
                                    Nepochsinhour = Nepochsinhour, SPTE_end = SPTE_end)
    # everything above matched on time strings; POSIXct from here on
    ts$time <- .raw.iso8601.to.posix(ts$time, tz = desiredtz)
    if (agg2_60 == TRUE) {
      ag <- .raw.timeuse.aggregate60(ts, desiredtz = desiredtz, dayborder = dayborder,
                                     lightpeak_available = lightpeak_available,
                                     tail_expansion_log = tail_expansion_log)
      ts <- ag$ts; ws3new <- ag$ws3new; nightsi <- ag$nightsi
      sec <- ag$sec; min <- ag$min; hour <- ag$hour
      time_POSIX <- ag$time_POSIX
      Nts <- ag$Nts
    }
    t_sleep <- t_sleep + as.numeric(difftime(Sys.time(), t0, units = "secs"))
    # Sib report and self-reported classes
    t0 <- Sys.time()
    if (do.sibreport == TRUE) {
      IDtmp <- as.character(ID)
      sibreport <- raw.sib.report(ts, ID = IDtmp, epochlength = ws3new,
                                  logs_diaries = if (length(logs_diaries) > 0) logs_diaries
                                  else NULL,
                                  desiredtz = desiredtz, ggir_exact = ggir_exact)
      ts$selfreported <- NA
      if ("imputecode" %in% colnames(sibreport)) {
        if ("logImputationCode" %in% colnames(ts) == FALSE) {
          ts$diaryImputationCode <- NA
        }
        addImputationCode <- TRUE
      } else {
        addImputationCode <- FALSE
      }
      for (srType in c("sleeplog", "nap", "nonwear", "bedlog")) {
        sr_index <- which(sibreport$type == srType)
        if (length(sr_index) > 0) {
          for (sii in sr_index) {
            ts_index <- which(ts$time >= sibreport$start[sii] & ts$time < sibreport$end[sii])
            if (addImputationCode == TRUE && srType %in% c("sleeplog", "bedlog")) {
              ts$diaryImputationCode[ts_index] <- as.numeric(sibreport$imputecode[sii])
            }
            ts_index1 <- ts_index[which(is.na(ts$selfreported[ts_index]))]
            ts_index2 <- ts_index[which(!is.na(ts$selfreported[ts_index]))]
            if (length(ts_index1) > 0) {
              ts$selfreported[ts_index1] <- srType
            }
            if (length(ts_index2) > 0) {
              ts$selfreported[ts_index2] <- paste0(ts$selfreported[ts_index2], "+", srType)
            }
          }
        }
      }
      ts$selfreported <- as.factor(ts$selfreported)
      sibreports[[as.character(sibDef)]] <- sibreport
    } else {
      sibreport <- NULL
    }
    t_sib <- t_sib + as.numeric(difftime(Sys.time(), t0, units = "secs"))
    # nightsi is restored at the top of every timewindow pass
    nightsi_bu <- nightsi
    for (TRLi in threshold.lig) {
      for (TRMi in threshold.mod) {
        for (TRVi in threshold.vig) {
          t0 <- Sys.time()
          levelList <- .raw.identify.levels(ts = ts, TRLi = TRLi, TRMi = TRMi, TRVi = TRVi,
                                            ws3 = ws3new,
                                            boutdur.mvpa = boutdur.mvpa,
                                            boutdur.in = boutdur.in,
                                            boutdur.lig = boutdur.lig,
                                            boutcriter.mvpa = boutcriter.mvpa,
                                            boutcriter.in = boutcriter.in,
                                            boutcriter.lig = boutcriter.lig,
                                            ggir_exact = ggir_exact)
          LEVELS <- levelList$LEVELS
          Lnames <- levelList$Lnames
          ts <- levelList$ts
          t_lev <- t_lev + as.numeric(difftime(Sys.time(), t0, units = "secs"))
          triple <- paste0(TRLi, "_", TRMi, "_", TRVi)
          # ignore all nights before the first waking up and after the last waking up
          FM <- which(diff(ts$diur) == -1)
          SO <- which(diff(ts$diur) == 1)
          ts_by_timewindow <- list()
          t0 <- Sys.time()
          for (timewindowi in timewindow) {
            ipass <- ipass + 1L
            if (!is.null(progress)) {
              progress("timeuse", ipass, npass,
                       paste0("Sleep definition ", sibDef, ", thresholds ", triple,
                              ", window ", timewindowi))
            }
            ts$window <- 0
            nightsi <- nightsi_bu
            # the part-3 estimate planted by addfirstwake is not a real guider name
            part3_estimates_firstnight <- which(ts$guider == "part3_estimate")
            if (length(part3_estimates_firstnight) > 0) {
              ts$guider[part3_estimates_firstnight] <- rev(HASPT.algo)[1]
            }
            trimmed <- .raw.timeuse.trim.nights(nightsi, ts, timewindowi, ws3new,
                                                FM = FM, SO = SO)
            dropped <- setdiff(nightsi, trimmed$nightsi)
            nightsi <- trimmed$nightsi
            Nwindows <- trimmed$Nwindows
            for (dm in dropped) {
              winrows[[length(winrows) + 1]] <- .raw.timeuse.window.row(
                sibDef, TRLi, TRMi, TRVi, timewindowi, NA_integer_, dm, NA_integer_,
                ts$time, NA, NA_integer_, NA_integer_, "midnight_trimmed",
                if (timewindowi == "MM") {
                  paste0("the MM grid drops every midnight more than twelve hours ",
                         "outside the first and last sleep boundary")
                } else {
                  paste0("the ", timewindowi,
                         " grid drops every midnight outside the first and last sleep transition")
                }, tz = desiredtz)
            }
            add_one_day_to_next_date <- FALSE
            lastDay <- ifelse(Nwindows > 0 && length(nightsi) > 0, yes = FALSE, no = TRUE)
            wi <- 1
            covered <- integer(0)
            while (lastDay == FALSE) {
              defdays <- .raw.timeuse.define.days(nightsi, wi, epochSize = ws3new, ts = ts,
                                                  timewindowi = timewindowi,
                                                  Nwindows = Nwindows, qwindow = qwindow,
                                                  ID = ID,
                                                  segment_all_windows = segment_all_windows)
              qqq <- defdays$qqq
              segments <- defdays$segments
              segments_names <- defdays$segments_names
              lastDay <- defdays$lastDay
              wstate <- "no_window"; wreason <- ""
              nseg <- 0L; si_first <- NA_integer_; si_last <- NA_integer_
              # if it is a meaningful day then none of the values in qqq should be NA
              if (length(which(is.na(qqq) == TRUE)) == 0) {
                if ((qqq[2] - qqq[1]) * ws3new > 900) {
                  ts$window[qqq[1]:qqq[2]] <- wi
                  covered <- c(covered, qqq[1], qqq[2])
                  if (di == 1) next_si <- 1 else next_si <- nwritten + 1
                  si_first <- next_si
                  nseg <- length(segments)
                  for (si in next_si:(next_si + length(segments) - 1)) {
                    fi <- 1
                    current_segment_i <- si - next_si + 1
                    Nindices <- length(segments[[current_segment_i]])
                    segStart <- segments[[current_segment_i]][seq(1, Nindices, by = 2)]
                    segEnd <- segments[[current_segment_i]][seq(2, Nindices, by = 2)]
                    Nsegments <- pmin(length(segStart), length(segEnd))
                    if (timewindowi %in% c("MM", "WW") & si > 1) {
                      # because the first segment is always the full window
                      if (("segment" %in% colnames(ts)) == FALSE) ts$segment <- NA
                      for (gi in 1:Nsegments) {
                        if (!is.na(segStart[gi]) && !is.na(segEnd[gi])) {
                          ts$segment[segStart[gi]:segEnd[gi]] <- si
                        }
                      }
                    }
                    head <- list(ID = ID, filename = fname_ms3)
                    fi <- fi + 2
                    gas <- .raw.timeuse.segment(
                      indexlog = list(fileIndex = 1L, winType = timewindowi, winIndex = wi,
                                      winStartEnd = qqq, segIndex1 = si,
                                      segIndex2 = current_segment_i,
                                      segStartEnd = c(segStart, segEnd), columnIndex = fi),
                      timeList = list(ts = ts, sec = sec, min = min, hour = hour,
                                      time_POSIX = time_POSIX, epochSize = ws3new),
                      levelList = levelList,
                      segments = segments, segments_names = segments_names,
                      dsummary = dsummary,
                      sumSleep = summarysleep_tmp2, sibDef = sibDef,
                      add_one_day_to_next_date = add_one_day_to_next_date,
                      desiredtz = desiredtz,
                      boutdur.mvpa = boutdur.mvpa, boutdur.in = boutdur.in,
                      boutdur.lig = boutdur.lig,
                      boutcriter.mvpa = boutcriter.mvpa, boutcriter.in = boutcriter.in,
                      boutcriter.lig = boutcriter.lig,
                      frag.metrics = frag.metrics,
                      iglevels = gp("iglevels", NULL),
                      LUXthresholds = gp("LUXthresholds",
                                         c(0, 100, 500, 1000, 3000, 5000, 10000)),
                      LUX_day_segments = gp("LUX_day_segments", NULL),
                      do.sibreport = do.sibreport,
                      storefolderstructure = storefolderstructure,
                      possible_nap_window = gp("possible_nap_window", NULL),
                      possible_nap_dur = gp("possible_nap_dur", NULL),
                      possible_nap_gap = gp("possible_nap_gap", 0),
                      possible_nap_edge_acc = gp("possible_nap_edge_acc", Inf),
                      nap_markerbutton_method = gp("nap_markerbutton_method", 0),
                      nap_markerbutton_max_distance =
                        gp("nap_markerbutton_max_distance", 30),
                      method_research_vars = method_research_vars,
                      nap_model = gp("nap_model", NULL),
                      fullFilename = fullFilename,
                      foldernamei = foldernamei,
                      tail_expansion_log = tail_expansion_log,
                      sibreport = sibreport,
                      warn_xmin = FALSE, # warned once above
                      ggir_exact = ggir_exact)
                    dsummary <- gas$dsummary
                    # ID and filename come first
                    dsummary[[si]] <- c(head, dsummary[[si]])
                    ds_names <- .raw.timeuse.merge.names(ds_names, names(dsummary[[si]]))
                    nwritten <- max(nwritten, si)
                    si_last <- si
                    ts <- gas$timeList$ts
                    ws3new <- gas$timeList$epochSize
                    Lnames <- levelList$Lnames <- gas$timeList$Lnames
                    LEVELS <- levelList$LEVELS <- gas$timeList$LEVELS
                    add_one_day_to_next_date <- gas$add_one_day_to_next_date
                    fi <- gas$indexlog$columnIndex
                    doNext <- gas$doNext
                    if (doNext == TRUE) next
                  }
                  # GGIR writes the folder columns a second time into row di; analyseSegment
                  # already wrote them into row si, so this cannot change the table
                  if (storefolderstructure == TRUE && di <= length(dsummary)) {
                    if (!is.null(dsummary[[di]])) {
                      dsummary[[di]]$filename_dir <- fullFilename
                      dsummary[[di]]$foldername <- foldernamei
                      ds_names <- .raw.timeuse.merge.names(ds_names, names(dsummary[[di]]))
                    }
                  }
                  di <- di + 1
                  wstate <- "analysed"
                } else {
                  wstate <- "too_short"
                  wreason <- paste0("(qqq[2] - qqq[1]) * ", ws3new, " = ",
                                    (qqq[2] - qqq[1]) * ws3new,
                                    " s is not above the 900 s minimum")
                }
              } else {
                wreason <- paste0("g.part5.definedays returned c(NA, NA): ", timewindowi,
                                  " gives one window fewer than it has transitions and the ",
                                  "tail after the last one is dropped")
              }
              winrows[[length(winrows) + 1]] <- .raw.timeuse.window.row(
                sibDef, TRLi, TRMi, TRVi, timewindowi, wi, qqq[1], qqq[2], ts$time, lastDay,
                nseg, if (is.na(si_first)) NA_integer_ else si_last - si_first + 1L,
                wstate, wreason, tz = desiredtz)
              di <- di + 1
              wi <- wi + 1
            }
            # the part of the recording no window of this timewindow reached
            if (length(covered) > 0) {
              if (min(covered) > 1) {
                winrows[[length(winrows) + 1]] <- .raw.timeuse.window.row(
                  sibDef, TRLi, TRMi, TRVi, timewindowi, NA_integer_, 1L,
                  min(covered) - 1L, ts$time, NA, NA_integer_, 0L, "not_analysed",
                  "before the first window of this window definition", tz = desiredtz)
              }
              if (max(covered) < nrow(ts)) {
                winrows[[length(winrows) + 1]] <- .raw.timeuse.window.row(
                  sibDef, TRLi, TRMi, TRVi, timewindowi, NA_integer_,
                  max(covered) + 1L, nrow(ts), ts$time, NA, NA_integer_, 0L, "not_analysed",
                  "after the last window of this window definition", tz = desiredtz)
              }
            } else {
              winrows[[length(winrows) + 1]] <- .raw.timeuse.window.row(
                sibDef, TRLi, TRMi, TRVi, timewindowi, NA_integer_, 1L, nrow(ts), ts$time,
                NA, NA_integer_, 0L, "not_analysed",
                "this window definition produced no analysable window", tz = desiredtz)
            }
            ts_by_timewindow[[timewindowi]] <- ts
          }
          t_win <- t_win + as.numeric(difftime(Sys.time(), t0, units = "secs"))
          levels_by_triple[[triple]] <- Lnames
          if (is.null(legend)) legend <- .raw.timeuse.class.dictionary(Lnames)
          if (save_ms5rawlevels == TRUE && length(ts_by_timewindow) > 0) {
            t0 <- Sys.time()
            frames <- list()
            for (tw in names(ts_by_timewindow)) {
              tsw <- ts_by_timewindow[[tw]]
              frames[[tw]] <- .raw.timeuse.timeseries(
                ts = tsw[, .raw.timeuse.timeseries.columns(tsw)],
                LEVELS = LEVELS, desiredtz = desiredtz,
                DaCleanFile = DaCleanFile,
                includedaycrit.part5 = gp("includedaycrit.part5", 2 / 3),
                includenightcrit.part5 = gp("includenightcrit.part5", 0),
                ID = ID, params = params, Lnames = Lnames, timewindow = tw,
                filename = filename_dir,
                save_ms5raw_without_invalid = gp("save_ms5raw_without_invalid", FALSE),
                require_complete_lastnight_part5 =
                  gp("require_complete_lastnight_part5", FALSE))
            }
            if (is.null(series[[triple]])) series[[triple]] <- list()
            series[[triple]][[as.character(sibDef)]] <- frames
            t_ser <- t_ser + as.numeric(difftime(Sys.time(), t0, units = "secs"))
          }
        }
      }
    }
  }
  # last epoch of the last sib definition's series; the aggregated one when aggregating
  last_timestamp <- time_POSIX[length(time_POSIX)]
  if (length(last_timestamp) == 0) last_timestamp <- NULL
  stage_times[["sleep"]] <- t_sleep
  stage_times[["sibreport"]] <- t_sib
  stage_times[["levels"]] <- t_lev
  stage_times[["windows"]] <- t_win
  stage_times[["timeseries"]] <- t_ser
  # Tidy-up
  if (length(dsummary) == 0) {
    messages <- c(messages,
                  paste0("No window survived part 5's own gates, so GGIR would write ",
                         "nothing at all. The windows table records every attempt and its ",
                         "reason."))
    obj <- empty()
    obj$windows <- .raw.timeuse.window.table(winrows)
    obj$sibreport <- if (length(sibreports) > 0) sibreports else NULL
    obj$last_timestamp <- last_timestamp
    state <- if (all(vapply(def, function(d) {
      nrow(summarysleep_tmp[which(summarysleep_tmp$sleepparam == d), ]) == 0
    }, logical(1)))) "no_valid_night" else "no_windows"
    return(finish(obj, state))
  }
  output <- .raw.timeuse.bind(dsummary, ds_names)
  # a WW window also relies on the previous night's sleep log
  whoareWW <- which(output$window == "WW")
  if (length(loglocation) > 0) {
    if (length(whoareWW) > 0) {
      whoareNOSL <- which(output$sleeplog_used[whoareWW] == "0")
      if (length(whoareNOSL) > 0) {
        for (k23 in 1:length(whoareNOSL)) {
          k24 <- whoareWW[(whoareNOSL[k23] - 1)]
          if (length(k24) > 0) {
            if (k24 > 0) {
              output$sleeplog_used[k24] <- "0"
            }
          }
        }
      }
    }
  }
  # drop empty columns right of boutdur.mvpa
  lastcolumn <- which(colnames(output) == "boutdur.mvpa")
  if (length(lastcolumn) == 0) {
    messages <- c(messages,
                  paste0("No row carried a boutdur.mvpa column, which is the condition ",
                         "GGIR's save is gated on; it would write nothing."))
    obj <- empty()
    obj$windows <- .raw.timeuse.window.table(winrows)
    obj$sibreport <- if (length(sibreports) > 0) sibreports else NULL
    obj$last_timestamp <- last_timestamp
    return(finish(obj, "no_windows"))
  }
  if (ncol(output) > lastcolumn) {
    emptycols <- sapply(output, function(x) all(x == ""))
    emptycols <- which(emptycols == TRUE &
                         colnames(output) %in%
                         grep(pattern = "LUX_|FRAG_|dur_|ACC_|Nbouts_|Nblocks_",
                              x = colnames(output), value = TRUE) == FALSE)
    if (length(emptycols) > 0) emptycols <- emptycols[which(emptycols > lastcolumn)]
    # while the fragmentation variables are being explored, keep all of them
    FRAG_variables_indices <- grep(pattern = "FRAG_", x = names(output))
    emptycols <- emptycols[which(emptycols %in% FRAG_variables_indices == FALSE)]
    if (length(emptycols) > 0) output <- output[, -emptycols]
  }
  if (length(output) > 0 && nrow(output) > 0) {
    output$GGIRversion <- as.character(ggir_version_label)
    # nap columns go to the end, or are dropped
    if ("nap" %in% method_research_vars) {
      output <- output[, c(grep(pattern = "denap|srnap|srnonw|sibreport_n_items",
                                x = names(output), invert = TRUE, value = FALSE),
                           grep(pattern = "denap|srnap|srnonw|sibreport_n_items",
                                x = names(output), invert = FALSE, value = FALSE))]
    } else {
      output <- output[, grep(pattern = "denap|srnap|srnonw|sibreport_n_items",
                              x = names(output), invert = TRUE, value = FALSE)]
    }
  }
  levels_out <- if (length(unique(levels_by_triple)) == 1) {
    levels_by_triple[[1]]
  } else {
    levels_by_triple
  }
  obj <- list(daysummary = output,
              series = if (length(series) > 0) series else NULL,
              levels = levels_out,
              legend = legend,
              sibreport = if (length(sibreports) > 0) sibreports else NULL,
              last_timestamp = last_timestamp,
              tail_expansion_log = tail_expansion_log,
              GGIRversion = as.character(ggir_version_label),
              windows = .raw.timeuse.window.table(winrows))
  finish(obj, "ok")
}

# WINDOWS TABLE

#' One Row of the Windows Table
#'
#' @param sibDef,TRLi,TRMi,TRVi,timewindowi The configuration this window belongs to.
#' @param wi Window number within the timewindow, or NA for a row that is not a window.
#' @param i1,i2 First and last epoch index of the span, either of which may be NA.
#' @param tvec The time column, for the two timestamps and the calendar date.
#' @param lastDay The flag \code{.raw.timeuse.define.days} returned.
#' @param nseg Number of segments, or NA.
#' @param nrows Number of output rows the window emitted, or NA.
#' @param state One of "analysed", "too_short", "no_window", "midnight_trimmed",
#'   "not_analysed".
#' @param reason Free text.
#' @param tz The timezone \code{calendar_date} is read in; \code{as.Date()} on a POSIXct
#'   defaults to UTC.
#' @return A one-row data.frame.
#' @keywords internal
#' @noRd
.raw.timeuse.window.row <- function(sibDef, TRLi, TRMi, TRVi, timewindowi, wi, i1, i2, tvec,
                                    lastDay, nseg, nrows, state, reason, tz = "") {
  at <- function(i) {
    if (is.na(i) || i < 1 || i > length(tvec)) return(as.POSIXct(NA)) else return(tvec[i])
  }
  t1 <- at(i1); t2 <- at(i2)
  data.frame(sleepparam = as.character(sibDef), TRLi = TRLi, TRMi = TRMi, TRVi = TRVi,
             window = timewindowi, window_number = as.integer(wi),
             start_index = as.integer(i1), end_index = as.integer(i2),
             start_time = t1, end_time = t2,
             calendar_date = if (is.na(i1)) as.Date(NA) else as.Date(t1, tz = tz),
             n_segments = as.integer(nseg), n_rows = as.integer(nrows),
             lastDay = as.logical(lastDay), state = state, reason = reason,
             stringsAsFactors = FALSE)
}

#' The Empty Windows Table
#'
#' @return A zero-row data.frame with the windows table's columns.
#' @keywords internal
#' @noRd
.raw.timeuse.empty.windows <- function() {
  .raw.timeuse.window.row(NA_character_, NA_real_, NA_real_, NA_real_, NA_character_,
                          NA_integer_, NA_integer_, NA_integer_, as.POSIXct(NA), NA,
                          NA_integer_, NA_integer_, NA_character_, NA_character_)[0, ]
}

#' Stack the Window Rows
#'
#' @param winrows A list of one-row frames.
#' @return A data.frame.
#' @keywords internal
#' @noRd
.raw.timeuse.window.table <- function(winrows) {
  if (length(winrows) == 0) return(.raw.timeuse.empty.windows())
  out <- do.call(rbind, winrows)
  row.names(out) <- NULL
  out
}

#' Read the Data Cleaning File
#'
#' @details GGIR passes the file only to \code{g.part5.savetimeseries}; a path that does not
#'   exist is ignored. An already-parsed frame is accepted as well.
#'
#' @param data_cleaning_file A path, an already-parsed data frame, or NULL.
#' @return A data.frame or NULL.
#' @keywords internal
#' @noRd
.raw.timeuse.cleaning.file <- function(data_cleaning_file) {
  if (is.data.frame(data_cleaning_file)) return(data_cleaning_file)
  if (length(data_cleaning_file) == 0) return(NULL)
  if (!file.exists(data_cleaning_file)) return(NULL)
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::fread(data_cleaning_file, data.table = FALSE))
  }
  utils::read.csv(data_cleaning_file, stringsAsFactors = FALSE)
}

# GGIR SHAPES AND PRINTING

#' GGIR's ms5 Milestone Objects From a canhrActi Time-Use Object
#'
#' Returns the four objects GGIR saves into \code{meta/ms5.out/<file>.RData}, under GGIR's own
#' names and in the order of its \code{save()} call, so that an \code{identical()} comparison
#' against a stored milestone works object by object.
#'
#' @details \code{GGIRversion} is a compatibility label and also the last column of
#'   \code{output}, so set \code{ggir_version_label} to the version of the reference run
#'   before comparing. The ms5.outraw objects are not returned here: they are
#'   \code{x$series[[triple]][[sibDef]]} (the last timewindow's frame), \code{filename}
#'   (\code{x$settings$filename_dir}), \code{Lnames} (\code{x$levels}) and \code{desiredtz}
#'   (\code{x$settings$desiredtz}). When part 5 produced no row, \code{output} has zero rows.
#'
#' @param x A canhrActi_raw_timeuse from \code{\link{raw.timeuse}}.
#' @return A named list of four members in GGIR's order.
#' @examples
#' \dontrun{
#' ms5 <- as.ggir.ms5(tu)
#' e <- new.env(); load("meta/ms5.out/MOS2E39230594.gt3x.RData", envir = e)
#' identical(ms5$output, e$output)
#' }
#' @export
as.ggir.ms5 <- function(x) {
  if (!inherits(x, "canhrActi_raw_timeuse")) {
    stop("x must be a canhrActi_raw_timeuse object from raw.timeuse()", call. = FALSE)
  }
  out <- list()
  out["output"] <- list(x$daysummary)
  out["tail_expansion_log"] <- list(x$tail_expansion_log)
  out["GGIRversion"] <- list(x$GGIRversion)
  out["last_timestamp"] <- list(x$last_timestamp)
  out
}

#' Print Method for a Time-Use Object
#'
#' @param x A canhrActi_raw_timeuse.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_timeuse <- function(x, ...) {
  s <- x$settings
  cat("\ncanhrActi time-use analysis (GGIR part 5: g.part5)\n")
  if (!is.null(s$filename) && !is.na(s$filename)) {
    cat("  file:        ", s$filename, "\n", sep = "")
  }
  if (!is.null(s$ID) && !is.na(s$ID)) cat("  id:          ", s$ID, "\n", sep = "")
  cat("  timezone:    ",
      if (is.null(s$desiredtz) || !nzchar(s$desiredtz)) {
        paste0("part 1 recorded none, so the system zone (", Sys.timezone(), ")")
      } else {
        s$desiredtz
      }, "\n", sep = "")
  cat("  epoch:       ", if (isTRUE(s$part5_agg2_60seconds)) 60 else s$windowsizes[1],
      " s", if (isTRUE(s$part5_agg2_60seconds)) " (aggregated from the part-1 epoch)" else "",
      "\n", sep = "")
  cat("  thresholds:  ", paste(s$threshold.lig, collapse = "/"), " light, ",
      paste(s$threshold.mod, collapse = "/"), " moderate, ",
      paste(s$threshold.vig, collapse = "/"), " vigorous, in milli-g\n", sep = "")
  cat("  windows:     ", paste(s$timewindow, collapse = ", "), "\n", sep = "")
  if (length(s$sleepparam) > 0) {
    cat("  sleep defs:  ", paste(s$sleepparam, collapse = ", "), "\n", sep = "")
  }
  if (!identical(x$status$state, "ok")) {
    cat("  state:       ", x$status$state,
        " (GGIR would write no part-5 milestone)\n", sep = "")
  }
  cat("  day table:   ", nrow(x$daysummary), " rows x ", ncol(x$daysummary),
      " columns, every cell a character string\n", sep = "")
  if (nrow(x$daysummary) > 0) {
    keep <- c("window", "window_number", "calendar_date", "dur_day_min", "dur_spt_min",
              "dur_day_spt_min", "ACC_day_mg")
    keep <- keep[keep %in% names(x$daysummary)]
    y <- x$daysummary[, keep, drop = FALSE]
    for (nm in names(y)) {
      v <- suppressWarnings(as.numeric(y[[nm]]))
      if (!all(is.na(v))) y[[nm]] <- round(v, 3)
    }
    print(y, row.names = FALSE)
  }
  if (!is.null(x$series)) {
    n <- sum(vapply(x$series, function(a) sum(vapply(a, length, integer(1))), integer(1)))
    cat("  series:      ", n, " exported time series over ", length(x$series),
        " threshold set", if (length(x$series) == 1) "" else "s", "\n", sep = "")
  }
  if (!is.null(x$sibreport)) {
    cat("  sib report:  ",
        paste(vapply(x$sibreport, function(r) as.character(nrow(r)), character(1)),
              collapse = ", "), " rows\n", sep = "")
  }
  if (is.data.frame(x$windows) && nrow(x$windows) > 0) {
    tb <- table(x$windows$state)
    cat("  attempts:    ", paste(paste0(names(tb), " ", as.integer(tb)), collapse = ", "),
        "\n", sep = "")
  }
  if (!is.null(x$last_timestamp)) {
    cat("  last epoch:  ", format(x$last_timestamp), "\n", sep = "")
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
