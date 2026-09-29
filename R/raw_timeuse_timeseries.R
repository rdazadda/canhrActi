# Ported from GGIR 3.3-9 R/g.part5.savetimeseries.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The function returns the mdat frame
# instead of writing a csv and an RData file, so the returned frame always carries the
# timestamp column that GGIR appends only on the RData branch; params_output and
# params_247 become explicit arguments; the column subset of GGIR's call site is folded
# in as .raw.timeuse.timeseries.columns; three input checks raise where GGIR would build
# a wrong-shaped frame. .raw.timeuse.timeseries.bywindow is a canhrActi addition.

#' Resolve One Time-Series-Export Setting
#'
#' An explicit argument wins; a flat canhrActi parameter object is consulted only when the
#' argument is NULL.
#'
#' @param value The explicit argument, or NULL.
#' @param params A flat named parameter list from \code{raw.params()}, or NULL.
#' @param name Member name to look up.
#' @param default Value to use when neither supplies one.
#' @return The resolved value.
#' @keywords internal
#' @noRd
.raw.timeuse.timeseries.setting <- function(value, params, name, default = NULL) {
  if (!is.null(value)) return(value)
  if (!is.null(params) && name %in% names(params)) return(params[[name]])
  return(default)
}

#' Pick and Order the Time-Series Columns GGIR Exports
#'
#' GGIR's call site subsets \code{ts} to seven fixed columns plus up to eight optional ones,
#' in a fixed order, and that order is the column order of the stored series. GGIR gates the
#' optional columns on feature flags; this tests presence in \code{ts}, which selects the
#' same columns. The greps are kept as greps because they decide the column count on a hip
#' recording: \code{grep("angle", ...)} takes anglex, angley and anglez as well.
#'
#' @param ts The part-5 time series.
#' @return A character vector of column names, in GGIR's export order.
#' @keywords internal
#' @noRd
.raw.timeuse.timeseries.columns <- function(ts) {
  fixed <- c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection")
  missing_fixed <- fixed[!fixed %in% names(ts)]
  if (length(missing_fixed) > 0) {
    stop("the part-5 time series has no column named ",
         paste(missing_fixed, collapse = ", "), call. = FALSE)
  }
  present <- function(x) if (x %in% names(ts)) x else NULL
  greptake <- function(pattern) {
    hit <- grep(pattern = pattern, x = names(ts), value = TRUE)
    if (length(hit) == 0) NULL else hit
  }
  c(fixed,
    present("nap1_nonwear2"),
    present("lightpeak"),
    present("selfreported"),
    greptake("angle"),
    greptake("temperature"),
    greptake("step_count"),
    greptake("diaryImputationCode"),
    greptake("marker"))
}

#' Turn the Guider Labels Into GGIR's Integer Codes
#'
#' The exported series stores the guider as a number. The mapping is lossy: \code{HDCZA} and
#' \code{HDCZA+invalid} both become 2, and every label the table does not list, the empty
#' string included, becomes 0 with no warning. The table is 3.3-9's, which adds
#' \code{LowAcc} to 3.3.6's nine names. The return is a double, as the stored column is.
#'
#' \preformatted{
#'   0  anything not listed, including ""    5  HorAngle, HorAngle+invalid
#'   1  sleeplog                             6  NotWorn, NotWorn+invalid
#'   2  HDCZA, HDCZA+invalid                 7  markerbutton
#'   3  setwindow                            8  HLRB
#'   4  L512                                 9  MotionWare
#'                                          10  LowAcc
#' }
#'
#' @param guider Character vector of guider labels, or a factor of them.
#' @return A numeric vector of codes, the same length as \code{guider}.
#' @keywords internal
#' @noRd
.raw.timeuse.guider.code <- function(guider) {
  new_guider_number <- ifelse(guider == "sleeplog", yes = 1, # digitize guider to save storage
                       no = ifelse(guider == "HDCZA" | guider == "HDCZA+invalid", yes = 2,
                            no = ifelse(guider == "HorAngle" | guider == "HorAngle+invalid",
                                        yes = 5,
                                 no = ifelse(guider == "NotWorn" | guider == "NotWorn+invalid",
                                             yes = 6, no = 0))))
  # use loop for other guiders:
  guidernames <- c("setwindow", "L512", "markerbutton", "HLRB", "MotionWare", "LowAcc")
  guidernumbers <- c(3, 4, 7, 8, 9, 10)
  for (gi in 1:length(guidernames)) {
    guidername_instance <- which(guider == guidernames[gi])
    if (length(guidername_instance) > 0) {
      new_guider_number[guidername_instance] <- guidernumbers[gi]
    }
  }
  return(new_guider_number)
}

#' Build the 14-Column Part-5 Time Series (GGIR g.part5.savetimeseries)
#'
#' Merges the behaviour classification onto the part-5 time series and returns the frame GGIR
#' stores in meta/ms5.outraw/<L>_<M>_<V>/<file>_<sibDef>.RData: one row per short epoch and,
#' on a default wrist recording with a sib report,
#'
#' \preformatted{
#'   timenum              epoch start, seconds since 1970-01-01 UTC. The merge key
#'   ACC                  acceleration in the part-1 metric, milli-g, ROUNDED TO 3 DIGITS
#'   SleepPeriodTime      ts$diur renamed: 1 inside the sleep period, 0 in waking hours
#'   invalidepoch         ts$nonwear renamed: 1 when the epoch is non-wear or invalid
#'   guider               the guider, digitised to an integer code
#'   window               window number, 0 outside any window
#'   sibdetection         0 none, 1 sustained inactivity bout, 2 detected nap
#'   selfreported         diary labels, present only when a sib report was produced
#'   angle                z-angle in degrees
#'   class_id             behaviour class, a 0-based index into Lnames
#'   invalid_fullwindow   percent invalid over the whole window, 100 outside any window
#'   invalid_sleepperiod  percent invalid over the sleep period of the window
#'   invalid_wakinghours  percent invalid over the waking hours of the window
#'   timestamp            POSIXct rebuilt from timenum in desiredtz
#' }
#'
#' Six more columns sit between \code{angle} and \code{class_id} when the recording carries
#' them: \code{nap1_nonwear2}, \code{lightpeak}, \code{temperature}, \code{step_count},
#' \code{diaryImputationCode} and \code{marker}.
#'
#' GGIR calls this once per threshold triple after the timewindow loop has finished, so the
#' exported \code{window} column is the last timewindow's numbering while \code{guider}
#' accumulated across all of them. That is reproduced, because \code{identical()} against a
#' stored milestone requires it; \code{.raw.timeuse.timeseries.bywindow()} gives a series
#' whose columns describe one pass.
#'
#' @details Kept as GGIR has it: the merge is on \code{timenum}, so a series handed over out
#'   of order comes back in time order; \code{epochSize} is the second epoch minus the first;
#'   the three invalid columns are created by one chained assignment, so their order is
#'   fullwindow, sleepperiod, wakinghours; they start at 100, and the leading and trailing
#'   runs of \code{window == 0} keep that sentinel; a window with no waking epoch gets NaN in
#'   \code{invalid_wakinghours}; \code{ACC} is rounded to 3 digits, \code{lightpeak} to an
#'   integer and \code{temperature} to 2; \code{includedaycrit.part5} and
#'   \code{includenightcrit.part5} are read as percentages with a rule that differs from the
#'   report's; and everything from the invalid columns onward sits inside
#'   \code{if (length(window_starts) > 0)}, so a recording with no windowed epoch gives NULL
#'   where GGIR writes no file.
#'
#'   Three checks raise where GGIR would build a wrong-shaped frame: a repeated
#'   \code{timenum} (a cartesian merge), a \code{LEVELS} whose length is not \code{nrow(ts)}
#'   (recycled by \code{data.frame()}), and a \code{timewindow} that is not one single "MM",
#'   "WW" or "OO" when \code{require_complete_lastnight_part5} is TRUE.
#'
#' @param ts The part-5 time series at the end of the timewindow loop, carrying at least
#'   time, ACC, diur, nonwear, guider, window and sibdetection. \code{ts$time} may be POSIXct
#'   or a character ISO 8601 vector.
#' @param LEVELS Integer behaviour class per epoch, from \code{.raw.identify.levels}, of
#'   length \code{nrow(ts)}.
#' @param desiredtz Olson time zone name used to parse and to rebuild the timestamps. Default
#'   "", the session zone.
#' @param DaCleanFile Parsed data-cleaning file, a frame with ID and day_part5 columns, or
#'   NULL. Read only when \code{save_ms5raw_without_invalid} is TRUE.
#' @param includedaycrit.part5 GGIR's params_cleaning member. Default 2/3.
#' @param includenightcrit.part5 GGIR's params_cleaning member. Default 0.
#' @param ID Recording identifier, matched against \code{DaCleanFile$ID}.
#' @param params A flat named parameter list from \code{raw.params()}, consulted only for
#'   arguments left NULL.
#' @param Lnames The behaviour class names, used here only to check that \code{LEVELS} indexes
#'   them.
#' @param timewindow The timewindow this call follows, a single "MM", "WW" or "OO". Read only
#'   by the last-window suppression.
#' @param filename Accepted for call-site compatibility with GGIR; nothing here reads it.
#' @param save_ms5raw_without_invalid When TRUE, drop the rows of the days that will not be
#'   analysed. Default FALSE.
#' @param require_complete_lastnight_part5 When TRUE, relabel the last window as window 0 if
#'   the recording ended too early in the day for its sleep to be trusted. Default FALSE.
#' @return The mdat data.frame, 14 columns on a default wrist recording with a sib report, or
#'   NULL when no epoch belongs to any window, which is the case in which GGIR writes no file.
#' @keywords internal
#' @noRd
.raw.timeuse.timeseries <- function(ts, LEVELS, desiredtz = NULL, DaCleanFile = NULL,
                                    includedaycrit.part5 = NULL,
                                    includenightcrit.part5 = NULL, ID = NULL, params = NULL,
                                    Lnames = NULL, timewindow = NULL, filename = "",
                                    save_ms5raw_without_invalid = NULL,
                                    require_complete_lastnight_part5 = NULL) {
  gs <- function(value, name, default) {
    .raw.timeuse.timeseries.setting(value, params, name, default)
  }
  desiredtz <- gs(desiredtz, "desiredtz", "")
  includedaycrit.part5 <- gs(includedaycrit.part5, "includedaycrit.part5", 2 / 3)
  includenightcrit.part5 <- gs(includenightcrit.part5, "includenightcrit.part5", 0)
  save_ms5raw_without_invalid <- gs(save_ms5raw_without_invalid,
                                    "save_ms5raw_without_invalid", FALSE)
  require_complete_lastnight_part5 <- gs(require_complete_lastnight_part5,
                                         "require_complete_lastnight_part5", FALSE)
  if (!is.data.frame(ts)) stop("ts must be a data.frame", call. = FALSE)
  if (nrow(ts) == 0) stop("ts has no rows", call. = FALSE)
  if (length(LEVELS) != nrow(ts)) { # not in GGIR
    stop("LEVELS has ", length(LEVELS), " values and the time series has ", nrow(ts),
         " epochs; data.frame() would recycle one into the other", call. = FALSE)
  }
  if (length(Lnames) > 0) {
    bad <- LEVELS[!is.na(LEVELS) & (LEVELS < 0 | LEVELS > (length(Lnames) - 1))]
    if (length(bad) > 0) {
      stop("LEVELS holds ", bad[1], ", which is not a 0-based index into the ",
           length(Lnames), " behaviour class names", call. = FALSE)
    }
  }
  ts <- ts[, .raw.timeuse.timeseries.columns(ts), drop = FALSE]
  ms5rawlevels <- data.frame(date_time = ts$time, class_id = LEVELS,
                             stringsAsFactors = FALSE)
  if (length(grep(pattern = " ", x = ts$time[1])) == 0) {
    ts$timestamp <- as.POSIXct(ts$time, tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
    ms5rawlevels$date_time <- as.POSIXct(ms5rawlevels$date_time,
                                         tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
  } else {
    ts$timestamp <- ts$time
  }
  # numeric time for the merge
  ts$timenum <- as.numeric(ts$timestamp)
  epochSize <- ts$timenum[2] - ts$timenum[1]
  ms5rawlevels$timenum <- as.numeric(ms5rawlevels$date_time)
  if (anyDuplicated(ts$timenum) > 0) { # not in GGIR
    stop("the time series repeats the timestamp of epoch ", anyDuplicated(ts$timenum),
         "; merging the behaviour classes onto it would produce a cartesian product. Part 1 ",
         "is expected to deliver a strictly increasing grid, so check the daylight-saving ",
         "handling of this recording", call. = FALSE)
  }
  mdat <- merge(ts, ms5rawlevels, by = "timenum")
  rm(ts, ms5rawlevels)
  names(mdat)[which(names(mdat) == "nonwear")] <- "invalidepoch"
  names(mdat)[which(names(mdat) == "diur")] <- "SleepPeriodTime"
  mdat <- mdat[, -which(names(mdat) == "date_time")]
  if (isTRUE(require_complete_lastnight_part5)) {
    if (length(timewindow) != 1 || !timewindow %in% c("MM", "WW", "OO")) { # not in GGIR
      stop("require_complete_lastnight_part5 needs one timewindow, \"MM\", \"WW\" or \"OO\", ",
           "to know which rule to apply", call. = FALSE)
    }
    N_window0_at_end <- which(rev(mdat$window) != 0)[1]
    N_hours_window0_at_end <- ((N_window0_at_end * epochSize) / 3600)
    lastHour <- as.numeric(format(mdat$timestamp[length(mdat$timestamp)], "%H"))
    # ended between midnight and 9am, so sleep onset may be unreliable for the last night
    if ((timewindow == "MM" || timewindow == "OO") && lastHour < 9 &&
        N_hours_window0_at_end < 9 + 6) {
      mdat$window[which(mdat$window == max(mdat$window))] <- 0
    }
    # ended between midnight and 3pm, so wake-up may be unreliable for the last night
    if (timewindow == "WW" && lastHour < 15 &&
        N_hours_window0_at_end < 15 + 6) {
      mdat$window[which(mdat$window == max(mdat$window))] <- 0
    }
  }

  # Add invalid day indicator
  mdat$invalid_wakinghours <- mdat$invalid_sleepperiod <- mdat$invalid_fullwindow <- 100
  window_starts <- which(abs(diff(c(0, mdat$window, 0))) > 0) # first epoch of each run
  if (length(window_starts) > 0) {
    if (length(window_starts) > 1) {
      for (di in 1:(length(window_starts) - 1)) {
        window_indices <- window_starts[di]:pmin((window_starts[di + 1] - 1), nrow(mdat))
        wake <- which(mdat$SleepPeriodTime[window_indices] == 0)
        sleep <- which(mdat$SleepPeriodTime[window_indices] == 1)
        mdat$invalid_wakinghours[window_indices] <-
          round(mean(mdat$invalidepoch[window_indices[wake]]) * 100, digits = 2)
        mdat$invalid_sleepperiod[window_indices] <-
          round(mean(mdat$invalidepoch[window_indices[sleep]]) * 100, digits = 2)
        mdat$invalid_fullwindow[window_indices] <-
          round(mean(mdat$invalidepoch[window_indices]) * 100, digits = 2)
      }
    } else {
      # unreachable in GGIR too: the zero padding forces at least two transitions
      wake <- which(mdat$SleepPeriodTime == 0)
      sleep <- which(mdat$SleepPeriodTime == 1)
      mdat$invalid_wakinghours <- round(mean(mdat$invalidepoch[wake]) * 100, digits = 2)
      mdat$invalid_sleepperiod <- round(mean(mdat$invalidepoch[sleep]) * 100, digits = 2)
      mdat$invalid_fullwindow <- round(mean(mdat$invalidepoch) * 100, digits = 2)
    }
    # rounded to reduce storage space
    mdat$ACC <- round(mdat$ACC, digits = 3)
    if ("lightpeak" %in% names(mdat)) mdat$lightpeak <- round(mdat$lightpeak)
    if ("temperature" %in% names(mdat)) mdat$temperature <- round(mdat$temperature, digits = 2)
    if (save_ms5raw_without_invalid == TRUE) {
      # Remove days based on data_cleaning_file
      if (length(DaCleanFile) > 0) {
        if (ID %in% DaCleanFile$ID) {
          days2exclude <- DaCleanFile$day_part5[which(DaCleanFile$ID == ID)]
          if (length(days2exclude) > 0) {
            cut <- which(mdat$window %in% days2exclude == TRUE)
            if (length(cut) > 0) mdat <- mdat[-cut, ]
          }
        }
      }
      # the two criteria become maximum percentages of non-wear
      if (includedaycrit.part5 >= 0 & includedaycrit.part5 <= 1) { # used as a ratio
        includedaycrit.part5 <- includedaycrit.part5 * 100
      } else if (includedaycrit.part5 > 1 & includedaycrit.part5 <= 25) { # used as hours
        includedaycrit.part5 <- (includedaycrit.part5 / 24) * 100
      }
      if (includenightcrit.part5 >= 0 & includenightcrit.part5 <= 1) { # used as a ratio
        includenightcrit.part5 <- includenightcrit.part5 * 100
      } else if (includenightcrit.part5 > 1 & includenightcrit.part5 <= 25) { # used as hours
        includenightcrit.part5 <- (includenightcrit.part5 / 24) * 100
      }
      maxpernwday <- 100 - includedaycrit.part5
      maxpernwnight <- 100 - includenightcrit.part5
      # Exclude days that have 100 percent nonwear over the full window or over wakinghours
      cut <- which(mdat$invalid_fullwindow == 100 |
                     mdat$invalid_wakinghours > maxpernwday |
                     mdat$invalid_sleepperiod > maxpernwnight |
                     mdat$window == 0)
      if (length(cut) > 0) mdat <- mdat[-cut, ]
    }
    mdat$guider <- .raw.timeuse.guider.code(mdat$guider)
    mdat <- mdat[, -which(names(mdat) %in% c("timestamp", "time"))]
    # GGIR appends timestamp only on the RData branch; the csv writer drops it again
    mdat$timestamp <- as.POSIXct(mdat$timenum, origin = "1970-01-01", tz = desiredtz)
    return(mdat)
  }
  # no epoch belongs to any window: GGIR writes no file at all
  return(NULL)
}

#' Build One Exported Time Series per Timewindow
#'
#' The GGIR-faithful series describes two passes at once: its \code{window} column comes from
#' the last timewindow and its \code{guider} column from all of them. This keys the series by
#' timewindow as well. Hand it the series as it stood at the end of each timewindow pass, in
#' the order the passes ran; each snapshot goes through \code{.raw.timeuse.timeseries()}
#' unchanged. The last timewindow's frame is identical to the faithful one; an earlier one
#' differs only in \code{window}, the three \code{invalid_*} columns computed from it, and
#' \code{guider} on any epoch a later pass wrote.
#'
#' @param ts_by_timewindow A named list of time series, one per timewindow ("MM", "WW",
#'   "OO"), in the order the passes ran.
#' @param LEVELS Integer behaviour class per epoch, shared by every timewindow.
#' @param ... Passed to \code{.raw.timeuse.timeseries()}. \code{timewindow} is set per element
#'   from the list names and must not be passed here.
#' @return A named list of mdat frames, one per timewindow, in the order of the input.
#' @keywords internal
#' @noRd
.raw.timeuse.timeseries.bywindow <- function(ts_by_timewindow, LEVELS, ...) {
  if (!is.list(ts_by_timewindow) || is.data.frame(ts_by_timewindow)) {
    stop("ts_by_timewindow must be a named list of time series, one per timewindow",
         call. = FALSE)
  }
  tw <- names(ts_by_timewindow)
  if (length(tw) == 0 || any(is.na(tw)) || any(tw == "")) {
    stop("ts_by_timewindow must be named by timewindow, one of \"MM\", \"WW\" or \"OO\"",
         call. = FALSE)
  }
  if (!all(tw %in% c("MM", "WW", "OO"))) {
    stop("ts_by_timewindow is named ", paste(tw, collapse = ", "),
         "; the timewindows are \"MM\", \"WW\" and \"OO\"", call. = FALSE)
  }
  if (anyDuplicated(tw) > 0) {
    stop("ts_by_timewindow names one timewindow twice", call. = FALSE)
  }
  if (!is.null(list(...)$timewindow)) {
    stop("timewindow comes from the names of ts_by_timewindow and must not be passed",
         call. = FALSE)
  }
  out <- lapply(tw, function(w) {
    .raw.timeuse.timeseries(ts = ts_by_timewindow[[w]], LEVELS = LEVELS, timewindow = w, ...)
  })
  names(out) <- tw
  return(out)
}
