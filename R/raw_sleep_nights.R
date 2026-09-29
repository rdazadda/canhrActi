# Ported from GGIR 3.3-9 R/g.part4.R, R/g.create.sp.mat.R and R/is_this_a_dst_night.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Only the per-file body of g.part4 is
# ported: the folder machinery, the milestone load and save, the diary RData cache and
# the pdf drawing are absent, the per-definition day-2 objects are a named list instead
# of eval(parse()), and LC_TIME is forced to "C".

# TIME CONVERSION HELPERS

#' Decimal Hours Since the Previous Midnight to an HH:MM:SS Clock String
#'
#' GGIR's \code{convertHRsinceprevMN2Clocktime}. The guider edges make a round trip through
#' this function and \code{.raw.nights.clock2hours}, which quantises them to whole seconds;
#' the stored guider columns depend on that. Only one 24 is subtracted, so 48.5 h comes
#' back as "24:30:00".
#'
#' @param x One number, decimal hours since the midnight that opens the night's date.
#' @return An "HH:MM:SS" character string.
#' @keywords internal
#' @noRd
.raw.nights.hours2clock <- function(x) {
  if (x > 24) x <- x - 24
  HR <- floor(x)
  MI <- floor((x - HR) * 60)
  SE <- round(((x - HR) - (MI / 60)) * 3600)
  if (SE == 60) {
    MI <- MI + 1
    SE <- 0
  }
  if (MI == 60) {
    HR <- HR + 1
    MI <- 0
  }
  if (HR == 24) HR <- 0
  if (HR < 10) HR <- paste0("0", HR)
  if (MI < 10) MI <- paste0("0", MI)
  if (SE < 10) SE <- paste0("0", SE)
  return(paste0(HR, ":", MI, ":", SE))
}

#' An HH:MM:SS Clock String Back to Decimal Hours
#'
#' GGIR's read-back \code{sum(as.numeric(unlist(strsplit(format(x), ":"))) / c(1, 60, 3600))}.
#' A string with fewer than three fields recycles the divisors, and a non-numeric one gives
#' NA, which is how an absent diary entry reaches the \code{is.na(GuiderOnset)} test.
#'
#' @param x One clock string, or whatever \code{format()} produced.
#' @return One number, or NA.
#' @keywords internal
#' @noRd
.raw.nights.clock2hours <- function(x) {
  tmp <- unlist(strsplit(x, ":"))
  sum(as.numeric(tmp) / c(1, 60, 3600))
}

#' Re-pad a Clock String to Double Digits
#'
#' GGIR's \code{doubleDigitClocktime}. A diary time such as "22:45:0", which
#' \code{raw.sleeplog} produces because the basic parser strips leading zeros, becomes
#' "22:45:00".
#'
#' @param x One clock string with three colon-separated fields.
#' @return The padded string.
#' @keywords internal
#' @noRd
.raw.nights.doubledigit <- function(x) {
  x <- unlist(strsplit(x, ":"))
  xHR <- as.numeric(x[1])
  xMI <- as.numeric(x[2])
  xSE <- as.numeric(x[3])
  if (xHR < 10) xHR <- paste0("0", xHR)
  if (xMI < 10) xMI <- paste0("0", xMI)
  if (xSE < 10) xSE <- paste0("0", xSE)
  x <- paste0(xHR, ":", xMI, ":", xSE)
  return(x)
}

# SLEEP PERIOD MATRIX

#' Sustained Inactivity Bouts of One Night on the Continuous Noon-to-Noon Axis
#'
#' GGIR's \code{g.create.sp.mat}. Converts every sustained inactivity bout of one night and
#' one definition into decimal hours after the midnight that opens the night's calendar
#' date, and reports the weekday and the calendar date of the night.
#'
#' @details A bout whose time is exactly midnight formats without a clock part and scores 0
#'   hours, not NA. \code{th2} is 12 for a night sleeper and 18 for a day sleeper and affects
#'   only the reported calendar date, which is unpadded "d/m/YYYY". The weekday vector is
#'   hardcoded English. \code{tmp7w} is filled from the onset column, as in GGIR, so the
#'   \code{tmp10w > th2} test reads the onset hour of the last bout. \code{calendar_date} and
#'   \code{wdayname} are assigned only in the \code{sp == nsp} pass.
#'
#' @param nsp Number of sustained inactivity bouts in this night and definition.
#' @param spo A data frame of \code{nsp} rows with columns nb, start, end, overlapGuider, def.
#' @param sleepdet.t The sib.cla.sum rows of this night and definition, with
#'   \code{sib.onset.time} and \code{sib.end.time} already POSIXct.
#' @param daysleep TRUE when the guider window for this night crossed noon.
#' @return list(spo, calendar_date, wdayname).
#' @keywords internal
#' @noRd
.raw.nights.spmat <- function(nsp, spo, sleepdet.t, daysleep = FALSE) {
  if (daysleep == FALSE) {
    th2 <- 12
  } else {
    th2 <- 18
  }
  weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  for (sp in 1:nsp) {
    spo[sp, 1] <- sp
    tmp7 <- format(sleepdet.t$sib.onset.time[which(sleepdet.t$sib.period == sp)])[1]
    tmp7w <- format(sleepdet.t$sib.onset.time[which(sleepdet.t$sib.period == sp)])[1]
    if (length(tmp7) > 0) {
      if (tmp7 != "") {
        tmp8 <- unlist(strsplit(tmp7, " "))
        tmp8w <- unlist(strsplit(tmp7w, " "))
        tmp9 <- unlist(strsplit(tmp8[2], ":"))
        tmp9w <- unlist(strsplit(tmp8w[2], ":"))
        if (length(tmp8) == 1) {
          tmp10 <- 0
        } else {
          tmp10 <- as.numeric(tmp9[1]) + (as.numeric(tmp9[2]) / 60) + (as.numeric(tmp9[3]) / 3600)
        }
        if (length(tmp8w) == 1) {
          tmp10w <- 0
        } else {
          tmp10w <- as.numeric(tmp9w[1]) + (as.numeric(tmp9w[2]) / 60) + (as.numeric(tmp9w[3]) / 3600)
        }
        if (sp == 1) { # first sleep period
          soso <- unclass(as.POSIXlt(tmp7))
          wday <- soso$wday # 0-6, 0 is Sunday
          wday_safe <- wday + 1
          calendar_date_safe <- paste(soso$mday, "/", (soso$mon + 1), "/", (soso$year + 1900), sep = "")
        }
        if (sp == nsp) { # last sleep period
          soso <- unclass(as.POSIXlt(tmp7w))
          wday <- soso$wday
          wday <- wday + 1
          if (wday == (wday_safe + 1)) { # night starts on one day and ends on the next
            wdayname <- weekdays[wday_safe]
            calendar_date <- calendar_date_safe
          } else { # start and end on the same day
            if (tmp10w > th2) {
              wdayname <- weekdays[wday_safe]
              calendar_date <- paste(soso$mday, "/", (soso$mon + 1), "/", (soso$year + 1900), sep = "")
            } else {
              tmp7 <- as.POSIXlt(tmp7) - (24 * 3600)
              soso <- unclass(as.POSIXlt(tmp7))
              wday <- soso$wday
              wday <- wday + 1
              wdayname <- weekdays[wday]
              calendar_date <- paste(soso$mday, "/", (soso$mon + 1), "/", (soso$year + 1900), sep = "")
            }
          }
        }
        tmp11 <- format(sleepdet.t$sib.end.time[which(sleepdet.t$sib.period == sp)])
        tmp12 <- unlist(strsplit(tmp11, " "))
        tmp13 <- unlist(strsplit(tmp12[2], ":"))
        if (length(tmp12) == 1) {
          tmp14 <- 0
        } else {
          tmp14 <- as.numeric(tmp13[1]) + (as.numeric(tmp13[2]) / 60) + (as.numeric(tmp13[3]) / 3600)
        }
        if (tmp10 != tmp14) {
          if (tmp10 < 12) tmp10 <- tmp10 + 24
          if (tmp14 <= 12) tmp14 <- tmp14 + 24
        }
        spo[sp, 2] <- tmp10
        spo[sp, 3] <- tmp14
      }
    }
  }
  invisible(list(spo = spo, calendar_date = calendar_date, wdayname = wdayname))
}

# DAYLIGHT SAVING TIME

#' Is the Night That Starts on This Date a Clock-Change Night
#'
#' GGIR's \code{is_this_a_dst_night}. Builds 21:00 on the date and the instant nine physical
#' hours later and compares the clock difference with nine: smaller means the clock went
#' back (-1, \code{dsthour} the doubled hour), larger means it went forward (+1, the missing
#' hour), equal means no transition (0, \code{dsthour} empty). A transition outside 21:00 to
#' 06:00 is invisible to it.
#'
#' @param calendar_date One date as "d/m/YYYY" or "dd/mm/YYYY".
#' @param tz Olson timezone name, or "" for the system zone.
#' @return list(dst_night_or_not, dsthour).
#' @keywords internal
#' @noRd
.raw.nights.dst <- function(calendar_date = c(), tz = "Europe/London") {
  splitdate <- unlist(strsplit(calendar_date, "/"))
  t0 <- as.POSIXlt(paste0(splitdate[3], "-", splitdate[2], "-", splitdate[1], " 21:00:00"), tz = tz)
  t1 <- as.POSIXlt(as.numeric(t0) + 3600 * 9, origin = "1970-01-01", tz = tz)
  hoursinbetween <- as.numeric(format(seq(t0, t1, by = 3600), "%H"))
  t1 <- as.numeric(format(t1, "%H")) + 24
  t0 <- as.numeric(format(t0, "%H"))
  dsthour <- c()
  if (t1 - t0 < 9) {
    # clock went back: 8 clock hours for 9 physical
    dst_night_or_not <- -1
    dsthour <- hoursinbetween[duplicated(hoursinbetween)] # the double hour
  } else if (t1 - t0 > 9) {
    # clock went forward: 10 clock hours for 9 physical
    dst_night_or_not <- 1
    expectedhours <- c(21:23, 0:7)
    dsthour <- expectedhours[which(expectedhours %in% hoursinbetween == FALSE)[1]] # the missing hour
  } else if (t1 - t0 == 9) {
    dst_night_or_not <- 0
  }
  return(invisible(list(
    dst_night_or_not = dst_night_or_not,
    dsthour = dsthour)))
}

#' Flip a Lone 0 Between Two 1s in the Sleep Flag
#'
#' GGIR's \code{correct01010pattern}. On an autumn clock-change night a doubled hour can
#' leave one sustained inactivity bout unflagged inside an otherwise continuous sleep
#' period. At least two rising edges are needed before anything is repaired, and it runs on
#' every night.
#'
#' @param x The overlapGuider column.
#' @return The repaired column, as numeric.
#' @keywords internal
#' @noRd
.raw.nights.correct01010 <- function(x) {
  x <- as.numeric(x)
  if (length(which(diff(x) == 1)) > 1) {
    minone <- which(diff(x) == -1) + 1
    plusone <- which(diff(x) == 1)
    matchingvalue <- which(minone %in% plusone == TRUE)
    if (length(matchingvalue) > 0) x[minone[matchingvalue]] <- 1
  }
  return(x)
}

#' Add the Doubled Hour Back to a Sleep Period That Ends or Starts Inside It
#'
#' GGIR's \code{correctSptEdgingInDoubleHour}. Both branches of the \code{timeWentBackward}
#' test add the same hour; the branch is kept as GGIR has it. \code{nightsummary} is one row
#' and the three column arguments are positions, (3, 4, 5) for the accelerometer edges and
#' (7, 8, 9) for the guider's.
#'
#' @param nightsummary One row of the night summary.
#' @param onsetcol,wakecol,durcol Column positions of the onset, the wake and the duration.
#' @param dsthour The doubled hour.
#' @param delta_t1 \code{diff()} of the bout end times, taken before the duplicate drop.
#' @return The row, with the duration possibly one hour longer.
#' @keywords internal
#' @noRd
.raw.nights.correct.double.hour <- function(nightsummary, onsetcol, wakecol, durcol,
                                            dsthour, delta_t1) {
  wakeInDoubleHour <- nightsummary[, wakecol] >= (dsthour + 24) & nightsummary[, wakecol] <= (dsthour + 25)
  onsetInDoubleHour <- nightsummary[, onsetcol] >= (dsthour + 24) & nightsummary[, onsetcol] <= (dsthour + 25)
  onsetBeforeDoubleHour <- nightsummary[, onsetcol] <= (dsthour + 24)
  wakeAfterDoubleHour <- nightsummary[, wakecol] >= (dsthour + 25)
  timeWentBackward <- length(which(delta_t1 < 0)) > 0
  if (onsetBeforeDoubleHour == TRUE & wakeInDoubleHour == TRUE) {
    if (timeWentBackward == TRUE) {
      # ended in the second hour of the double hour
      nightsummary[, durcol] <- nightsummary[, durcol] + 1
    } else if (timeWentBackward == FALSE) {
      # could be either hour; GGIR assumes the second, to keep sleep efficiency at or below 1
      nightsummary[, durcol] <- nightsummary[, durcol] + 1
    }
  }
  if (wakeAfterDoubleHour == TRUE & onsetInDoubleHour == TRUE) {
    if (timeWentBackward == TRUE) {
      nightsummary[, durcol] <- nightsummary[, durcol] + 1
    } else if (timeWentBackward == FALSE) {
      nightsummary[, durcol] <- nightsummary[, durcol] + 1
    }
  }
  return(nightsummary)
}

# INPUT RESOLUTION

#' The Column Names of the Night Summary
#'
#' GGIR's 40 names, plus the two folder columns. The TimeInBed renames are \code{gsub}s on
#' the whole vector, so \code{guider_onset_ts} and \code{guider_wakeup_ts} become
#' \code{guider_inbedStart_ts} and \code{guider_inbedEnd_ts} too.
#'
#' @param sleepwindowType "SPT" or "TimeInBed".
#' @param storefolderstructure TRUE adds filename_dir and foldername.
#' @return A character vector of 40 or 42 names.
#' @keywords internal
#' @noRd
.raw.nights.colnames <- function(sleepwindowType = "SPT", storefolderstructure = FALSE) {
  colnamesnightsummary <- c("ID", "night", "sleeponset", "wakeup", "SptDuration", "sleepparam", "guider_onset",
                            "guider_wakeup", "guider_SptDuration", "error_onset", "error_wake", "error_dur",
                            "fraction_night_invalid",
                            "SleepDurationInSpt", "WASO", "duration_sib_wakinghours",
                            "number_sib_sleepperiod", "number_of_awakenings",
                            "number_sib_wakinghours", "duration_sib_wakinghours_atleast15min",
                            "sleeponset_ts", "wakeup_ts", "guider_onset_ts", "guider_wakeup_ts",
                            "sleeplatency", "sleepefficiency", "page", "daysleeper", "weekday", "calendar_date",
                            "filename", "cleaningcode", "sleeplog_used", "sleeplog_ID", "acc_available",
                            "guider", "SleepRegularityIndex1", "SriFractionValid",
                            "longitudinal_axis", "guider_corrected")
  if (storefolderstructure == TRUE) {
    colnamesnightsummary <- c(colnamesnightsummary, "filename_dir", "foldername")
  }
  if (sleepwindowType == "TimeInBed") {
    colnamesnightsummary <- gsub(replacement = "guider_inbedStart", pattern = "guider_onset",
                                 x = colnamesnightsummary)
    colnamesnightsummary <- gsub(replacement = "guider_inbedEnd", pattern = "guider_wakeup",
                                 x = colnamesnightsummary)
    colnamesnightsummary <- gsub(replacement = "guider_inbedDuration", pattern = "guider_SptDuration",
                                 x = colnamesnightsummary)
  }
  colnamesnightsummary
}

#' Resolve the Part-3 Objects This Function Reads
#'
#' Part 4 needs only \code{sib.cla.sum} plus eight small objects; this accepts every shape
#' canhrActi and GGIR produce them in.
#'
#' @param part3 A canhrActi_raw (its \code{$sleep} is taken), a canhrActi_raw_sleep from
#'   \code{\link{raw.sleep.part3}}, or any list carrying GGIR's own ms3 names.
#' @return A named list with GGIR's names: sib.cla.sum, SPTE_start, SPTE_end, SPTE_corrected,
#'   part3_guider, L5list, SleepRegularityIndex, ID, longitudinal_axis, desiredtz_part1,
#'   tail_expansion_log, rec_starttime, filename.
#' @keywords internal
#' @noRd
.raw.nights.part3 <- function(part3) {
  if (is.null(part3)) {
    stop("part3 is NULL: raw.sleep.nights() needs the part-3 result of a recording ",
         "(raw.sleep.part3(x), or the objects of a meta/ms3.out milestone)", call. = FALSE)
  }
  if (inherits(part3, "canhrActi_raw")) part3 <- part3$sleep
  if (is.null(part3)) {
    stop("The recording carries no $sleep: run x$sleep <- raw.sleep.part3(x) first",
         call. = FALSE)
  }
  if (!is.list(part3)) {
    stop("part3 must be a canhrActi_raw_sleep from raw.sleep.part3(), a canhrActi_raw ",
         "carrying one, or a list of GGIR ms3 objects", call. = FALSE)
  }
  pick <- function(...) {
    for (nm in c(...)) if (nm %in% names(part3)) return(part3[[nm]])
    NULL
  }
  if (!("sib.cla.sum" %in% names(part3))) {
    stop("part3 has no sib.cla.sum: it is not a part-3 result", call. = FALSE)
  }
  filename <- NULL
  if (is.list(part3$file) && !is.null(part3$file$filename) && !is.na(part3$file$filename)) {
    filename <- as.character(part3$file$filename)
  }
  list(sib.cla.sum = part3[["sib.cla.sum"]],
       SPTE_start = pick("SPTE_start"),
       SPTE_end = pick("SPTE_end"),
       SPTE_corrected = pick("SPTE_corrected"),
       part3_guider = pick("part3_guider"),
       L5list = pick("L5list"),
       SleepRegularityIndex = pick("SleepRegularityIndex"),
       ID = pick("ID", "id"),
       longitudinal_axis = pick("longitudinal_axis"),
       desiredtz_part1 = pick("desiredtz_part1"),
       tail_expansion_log = pick("tail_expansion_log"),
       rec_starttime = pick("rec_starttime"),
       filename = filename)
}

#' Resolve the Sleep Diary
#'
#' GGIR's diary loading without the RData cache. The diary is an argument here;
#' \code{\link{raw.sleeplog}} is called only when the caller passed a \code{loglocation} and
#' no diary object. The two consistency checks and the bedlog choice are GGIR's, with their
#' messages.
#'
#' @param sleeplog NULL, the list \code{\link{raw.sleeplog}} returns, or a data frame already
#'   in the long per-night shape.
#' @param loglocation Path to a diary csv, or NULL.
#' @param colid,coln1 GGIR's two column positions for the basic format.
#' @param sleepwindowType "SPT" or "TimeInBed".
#' @param desiredtz Olson timezone name.
#' @param rec_starttime The recording start, for the advanced parser.
#' @param id The recording identifier, for the advanced parser.
#' @return list(dolog, sleeplog).
#' @keywords internal
#' @noRd
.raw.nights.diary <- function(sleeplog = NULL, loglocation = NULL, colid = 1, coln1 = 2,
                              sleepwindowType = "SPT", desiredtz = "",
                              rec_starttime = NULL, id = NULL) {
  logs_diaries <- NULL
  if (is.data.frame(sleeplog)) {
    # already the long per-night frame
    logs_diaries <- if (sleepwindowType == "TimeInBed") {
      list(sleeplog = NULL, bedlog = sleeplog)
    } else {
      list(sleeplog = sleeplog, bedlog = NULL)
    }
  } else if (is.list(sleeplog)) {
    logs_diaries <- sleeplog
  } else if (length(loglocation) > 0) {
    logs_diaries <- raw.sleeplog(loglocation, colid = colid, coln1 = coln1,
                                 sleepwindowType = sleepwindowType, desiredtz = desiredtz,
                                 rec_starttime = rec_starttime, id = id)
  }
  if (is.null(logs_diaries)) return(list(dolog = FALSE, sleeplog = NULL))
  dolog <- TRUE
  if (sleepwindowType == "SPT" && length(logs_diaries$bedlog) > 0 &&
      length(logs_diaries$sleeplog) == 0) {
    stop(paste0("The sleep diary as provided only appears to have time indicators",
                " for time in bed and not for the Sleep Period Time window, while",
                " parameter sleepwindowType is set to SPT (default). Either change sleepwindowType to",
                " \"TimeInBed\" or change your sleep diary column names for sleep timing",
                " to \"wakeup\" and \"sleeponset\"."), call. = FALSE)
  } else if (sleepwindowType == "TimeInBed" && length(logs_diaries$bedlog) == 0 &&
             length(logs_diaries$sleeplog) > 0) {
    stop(paste0("The sleep diary as provided only appears to have time indicators",
                " for SPT and not for the Time in Bed, while",
                " parameter sleepwindowType is set to TimeInBed. Either change sleepwindowType to",
                " \"SPT\" or change your sleep diary column names for sleep timing",
                " to \"outbed\" and \"inbed\"."), call. = FALSE)
  }
  if (sleepwindowType == "TimeInBed" && length(logs_diaries$bedlog) > 0) {
    sleeplog <- logs_diaries$bedlog
  } else {
    sleeplog <- logs_diaries$sleeplog
  }
  if (is.null(logs_diaries$sleeplog) && is.null(logs_diaries$bedlog)) {
    return(list(dolog = FALSE, sleeplog = NULL))
  }
  sleeplog$night <- as.numeric(sleeplog$night)
  sleeplog$duration <- as.numeric(sleeplog$duration)
  list(dolog = dolog, sleeplog = sleeplog)
}

# PART 4

#' GGIR Part 4: Per-Night Sleep Labelling
#'
#' Turns the sustained inactivity bouts of GGIR part 3 into one row per night and per sib
#' definition, with sleep onset, wake-up, sleep period time, WASO and the rest of GGIR's
#' night summary. A sib becomes sleep when it overlaps the guider window under the active
#' overlap rule; sleep onset is the start of the first such bout and wake-up the end of the
#' last, every bout in between counts as sleep, and the gaps between them are WASO. Part 4
#' needs no raw data and no epoch series, only \code{sib.cla.sum} and eight small objects of
#' the part-3 result, so a night table can be recomputed with other parameters or another
#' diary in milliseconds.
#'
#' The result has 39 columns by default, 41 when \code{sleepwindowType} is "TimeInBed" or
#' \code{consider_marker_button} is TRUE (\code{sleeplatency} and \code{sleepefficiency}),
#' and two more with \code{storefolderstructure}. Hour columns are decimal hours on a
#' continuous axis whose origin is the midnight that opens the night's calendar date: 12 is
#' noon of that date, 24 the following midnight, 36 the next noon, and a day sleeper's
#' wake-up can exceed 36. The four \code{_ts} columns are "HH:MM:SS" strings with no date.
#' \code{calendar_date} is "d/m/YYYY" without zero padding.
#'
#' @section Comparison with canhrActi's count-based sleep functions:
#' This is not the same estimator as the count-based \code{sleep.analysis} and its
#' Tudor-Locke period detection, which discover sleep periods from the count series and
#' report one row per period. This indexes nights noon to noon from the recording start and
#' labels sustained inactivity bouts against a guider window, one row per night.
#' \code{sleep_time} is not \code{SleepDurationInSpt}, and the count pipeline's
#' \code{sleep_efficiency} is not GGIR's \code{sleepefficiency}, which exists only in
#' TimeInBed mode and can exceed 1. Do not read one as the other.
#'
#' @details
#' The per-file body of GGIR 3.3-9 \code{g.part4}, in GGIR's order: the timezone recorded by
#' part 1 wins over the caller's; the diary and its consistency checks; the night-0 drop;
#' the identifier and diary row match; the night range and the exclusion rules; per night
#' the guider from \code{def.noc.sleep} and the clock-string round trip that quantises it
#' to whole seconds; per definition the bouts to hours, the artificial guider-edge bouts
#' and the overlap rule; then the night summary columns with the daylight-saving block.
#'
#' GGIR's behaviour is reproduced, not repaired: a night with no rows in \code{sib.cla.sum}
#' produces no row at all; \code{number_of_awakenings} is
#' \code{number_sib_sleepperiod - 1}, so -1 is reachable; \code{sleepefficiency} metric 1
#' can exceed 1; the SRI join needs \code{frac_valid} strictly above
#' \code{includenightcrit/24}; \code{spo} and \code{guider} survive from one night to the
#' next. None of this is gated behind \code{ggir_exact}.
#'
#' Deviations, all behaviour preserving: the per-definition day-2 objects are a named list
#' instead of \code{eval(parse())}, so a sib definition label that is not a syntactic R
#' name works; the diary arrives as an object or a path, and a diary object counts as a
#' configured diary wherever GGIR tests \code{length(loglocation) > 0}, so
#' \code{sleepwindowType = "TimeInBed"} with a diary object is honoured rather than
#' reverted by \code{\link{raw.params}}; the pdf is not drawn but the page counters that
#' feed the \code{page} column are; \code{GGIRversion} is \code{ggir_version_label} from
#' \code{\link{raw.params}}, the GGIR version the port matches, not the installed GGIR's.
#'
#' @param part3 The part-3 result: a \code{canhrActi_raw} carrying \code{$sleep}, a
#'   \code{canhrActi_raw_sleep} from \code{\link{raw.sleep.part3}}, or any list holding
#'   GGIR's own ms3 object names.
#' @param params A parameter object from \code{\link{raw.params}}, or NULL to reuse the
#'   parameters part 3 ran with.
#' @param sleeplog NULL, the list \code{\link{raw.sleeplog}} returns, or a data frame already
#'   in the long per-night diary shape. With NULL and a \code{loglocation} parameter set, the
#'   diary is read here.
#' @param filename The name the \code{filename} column should carry. NULL derives GGIR's own
#'   value, the ms3 milestone name, which is the recording file name plus ".RData".
#' @param progress NULL, or a function(stage, i, n, message) called once per night.
#' @param ... Individual parameter overrides, for example \code{includenightcrit = 0}.
#' @return A data frame of class \code{canhrActi_raw_nights}, one row per night and sib
#'   definition. The \code{"canhrActi"} attribute carries the settings, the per-night guider
#'   table, the labelled bouts and a status.
#' @seealso \code{\link{raw.sleep.part3}} for the input, \code{\link{raw.sleeplog}} for the
#'   diary, \code{\link{raw.sib.summary}} for \code{sib.cla.sum}.
#' @examples
#' \dontrun{
#' x$sleep <- raw.sleep.part3(x)
#' nights <- raw.sleep.nights(x)
#' nights[, c("night", "sleeponset", "wakeup", "SleepDurationInSpt", "WASO")]
#' }
#' @export
raw.sleep.nights <- function(part3, params = NULL, sleeplog = NULL, filename = NULL,
                             progress = NULL, ...) {
  t_total <- Sys.time()
  # C time locale for the call, as GGIR's parallel workers do
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be NULL or a function(stage, i, n, message)", call. = FALSE)
  }
  p3obj <- if (inherits(part3, "canhrActi_raw")) part3$sleep else part3
  P3 <- .raw.nights.part3(part3)
  overrides <- list(...)
  stored <- if (is.list(p3obj) && is.list(p3obj$settings)) p3obj$settings$params else NULL
  params <- .raw.calibrate.params(list(params = stored), params, overrides)
  messages <- character()
  sleeplog_supplied <- !is.null(sleeplog)
  # Part-4 parameters
  loglocation <- .raw.param(params, "loglocation", NULL)
  colid <- .raw.param(params, "colid", 1)
  coln1 <- .raw.param(params, "coln1", 2)
  sleepwindowType <- .raw.param(params, "sleepwindowType", "SPT")
  def.noc.sleep <- .raw.param(params, "def.noc.sleep", 1)
  HASPT.algo <- .raw.param(params, "HASPT.algo", "HDCZA")
  relyonguider <- .raw.param(params, "relyonguider", FALSE)
  sib_must_fully_overlap_with_TimeInBed <-
    .raw.param(params, "sib_must_fully_overlap_with_TimeInBed", c(TRUE, TRUE))
  sleepefficiency.metric <- .raw.param(params, "sleepefficiency.metric", 1)
  consider_marker_button <- .raw.param(params, "consider_marker_button", FALSE)
  excludefirstlast <- .raw.param(params, "excludefirstlast", FALSE)
  excludefirst.part4 <- .raw.param(params, "excludefirst.part4", FALSE)
  excludelast.part4 <- .raw.param(params, "excludelast.part4", FALSE)
  includenightcrit <- .raw.param(params, "includenightcrit", 16)
  data_cleaning_file <- .raw.param(params, "data_cleaning_file", NULL)
  do.visual <- .raw.param(params, "do.visual", TRUE)
  outliers.only <- .raw.param(params, "outliers.only", FALSE)
  criterror <- .raw.param(params, "criterror", 3)
  storefolderstructure <- .raw.param(params, "storefolderstructure", FALSE)
  desiredtz <- .raw.param(params, "desiredtz", "")
  idloc <- .raw.param(params, "idloc", 1)
  nnpp <- 40 # nights per page of GGIR's report, which feeds the page column
  # The timezone part 3 ran in wins, the empty string (system zone) included
  desiredtz_part1 <- P3$desiredtz_part1
  if (!is.null(desiredtz_part1)) {
    desiredtz <- desiredtz_part1
  }
  # raw.params() reverts sleepwindowType to "SPT" when loglocation is empty; a diary passed
  # as an object is still a diary, so TimeInBed is kept
  if (sleeplog_supplied && identical(overrides[["sleepwindowType"]], "TimeInBed") &&
      !identical(sleepwindowType, "TimeInBed")) {
    sleepwindowType <- "TimeInBed"
    messages <- c(messages, paste0("raw.params() reverted sleepwindowType to \"SPT\" ",
                                   "because loglocation is empty; the diary was passed as ",
                                   "an object, so \"TimeInBed\" is kept."))
  }
  # where GGIR tests length(loglocation) > 0 for "a diary is configured", an object counts too
  diary_configured <- length(loglocation) > 0 || sleeplog_supplied
  # Sleep diary, without GGIR's RData cache
  diary <- .raw.nights.diary(sleeplog = sleeplog, loglocation = loglocation, colid = colid,
                             coln1 = coln1, sleepwindowType = sleepwindowType,
                             desiredtz = desiredtz, rec_starttime = P3$rec_starttime,
                             id = P3$ID)
  dolog <- diary$dolog
  sleeplog <- diary$sleeplog
  colnamesnightsummary <- .raw.nights.colnames(sleepwindowType = sleepwindowType,
                                               storefolderstructure = storefolderstructure)
  # logdur is written and never read in GGIR; kept so the two versions line up
  logdur <- 0
  # Name of the ms3 milestone this row set came from
  if (is.null(filename)) {
    base <- P3$filename
    if (is.null(base) || is.na(base)) base <- P3$ID
    if (is.null(base) || length(base) == 0 || is.na(base[1])) {
      fname <- NA_character_
    } else {
      fname <- paste0(as.character(base)[1], ".RData")
    }
  } else {
    fname <- as.character(filename)[1]
  }
  # storefolderstructure columns from the recording's path, NA when there is none
  ffd <- ffp <- NA_character_
  if (storefolderstructure == TRUE) {
    p <- if (is.list(p3obj)) p3obj$file$path else NULL
    if (!is.null(p) && length(p) == 1 && !is.na(p)) {
      ffd <- as.character(p)
      ffp <- basename(dirname(ffd))
    }
  }
  nightsummary <- as.data.frame(matrix(0, 0, length(colnamesnightsummary)))
  colnames(nightsummary) <- colnamesnightsummary
  sumi <- 1 # output row counter
  cnt <- 1 # nights plotted on the current report page
  pagei <- 1
  episodes <- list() # the labelled bouts, which GGIR only draws
  guiders <- list() # resolved guider window per night
  # spo and guider persist across nights in GGIR; exists("spo") becomes spo_exists
  spo <- NULL
  spo_exists <- FALSE
  spo_day <- list()
  spo_day2 <- list()
  guider <- NULL
  guider_set <- FALSE
  tail_expansion_log <- P3$tail_expansion_log
  ID <- P3$ID
  SPTE_start <- P3$SPTE_start
  SPTE_end <- P3$SPTE_end
  SPTE_corrected <- P3$SPTE_corrected
  part3_guider <- P3$part3_guider
  L5list <- P3$L5list
  SleepRegularityIndex <- P3$SleepRegularityIndex
  longitudinal_axis <- P3$longitudinal_axis
  sib.cla.sum <- P3$sib.cla.sum
  if (length(data_cleaning_file) > 0) {
    # allow for forced relying on guider based on external data_cleaning_file
    DaCleanFile <- data.table::fread(data_cleaning_file, data.table = FALSE)
  }
  accid <- c()
  logid <- NA # diary id matched to accid
  if (length(ID) > 0) {
    if (!is.na(ID)) {
      accid <- ID
    }
    # with no ID, .raw.sleeplog.extractid() derives one from the file name
  }
  finish <- function(ns, state) {
    # row names stay as GGIR leaves them; identical() against a stored nightsummary compares them
    ep <- if (length(episodes) > 0) do.call(rbind, episodes) else NULL
    if (!is.null(ep)) row.names(ep) <- NULL
    gd <- if (length(guiders) > 0) do.call(rbind, guiders) else NULL
    if (!is.null(gd)) row.names(gd) <- NULL
    structure(ns,
              class = c("canhrActi_raw_nights", "data.frame"),
              canhrActi = list(
                id = if (length(accid) == 0) NA_character_ else as.character(accid)[1],
                filename = fname,
                desiredtz = desiredtz,
                dolog = dolog,
                episodes = ep,
                guiders = gd,
                settings = list(sleepwindowType = sleepwindowType,
                                def.noc.sleep = def.noc.sleep,
                                relyonguider = relyonguider,
                                sib_must_fully_overlap_with_TimeInBed =
                                  sib_must_fully_overlap_with_TimeInBed,
                                sleepefficiency.metric = sleepefficiency.metric,
                                consider_marker_button = consider_marker_button,
                                includenightcrit = includenightcrit,
                                excludefirstlast = excludefirstlast,
                                excludefirst.part4 = excludefirst.part4,
                                excludelast.part4 = excludelast.part4,
                                storefolderstructure = storefolderstructure,
                                ggir_exact = isTRUE(.raw.param(params, "ggir_exact", TRUE)),
                                params = params),
                versions = list(canhrActi = if (is.list(p3obj)) p3obj$versions$canhrActi else NA),
                status = list(state = state, messages = messages),
                elapsed = as.numeric(difftime(Sys.time(), t_total, units = "secs"))))
  }
  # Ignore night zero, the night before the recording
  if (is.null(sib.cla.sum) || nrow(sib.cla.sum) == 0) {
    # GGIR writes no ms4 file here; return the empty table with a state instead
    messages <- c(messages, paste0("Part 3 produced no sustained inactivity bout periods ",
                                   "for this recording; GGIR writes no part-4 milestone ",
                                   "for it and no night row can be built."))
    ns <- nightsummary
    if (sleepwindowType != "TimeInBed" && consider_marker_button == FALSE) {
      ns <- ns[, which(colnames(ns) %in% c("sleeplatency", "sleepefficiency") == FALSE)]
    }
    return(finish(ns, if (is.null(sib.cla.sum)) "no_part3" else "no_sib_periods"))
  }
  sib.cla.sum <- sib.cla.sum[which(sib.cla.sum$night != 0), ]
  if (nrow(sib.cla.sum) == 0) {
    messages <- c(messages, paste0("Every sustained inactivity bout period belongs to ",
                                   "night 0, which part 4 drops; GGIR writes no part-4 ",
                                   "milestone for this recording."))
    ns <- nightsummary
    if (sleepwindowType != "TimeInBed" && consider_marker_button == FALSE) {
      ns <- ns[, which(colnames(ns) %in% c("sleeplatency", "sleepefficiency") == FALSE)]
    }
    return(finish(ns, "no_sib_periods"))
  }
  if (is.character(sib.cla.sum$sib.onset.time)) {
    sib.cla.sum$sib.onset.time <- .raw.iso8601.to.posix(sib.cla.sum$sib.onset.time, tz = desiredtz)
  }
  if (is.character(sib.cla.sum$sib.end.time)) {
    sib.cla.sum$sib.end.time <- .raw.iso8601.to.posix(sib.cla.sum$sib.end.time, tz = desiredtz)
  }
  # identifier and matching diary rows
  idwi <- .raw.sleeplog.extractid(idloc, fname = fname, dolog, sleeplog, accid = accid)
  accid <- idwi$accid
  wi <- idwi$matching_indices_sleeplog
  # night numbers in the file
  if (dolog == TRUE) {
    logid <- sleeplog$ID[wi][1]
    sleeplog_matching_ID <- which(sleeplog$ID == accid)
    if (length(sleeplog_matching_ID) == 0) {
      # try without letters
      sleeplog_matching_ID <- which(sleeplog$ID == gsub("[^0-9.-]", "", accid))
      if (length(sleeplog_matching_ID) == 0) {
        # try without leading zeros
        sleeplog_matching_ID <- which(sleeplog$ID == gsub("^0+", "", accid))
        if (length(sleeplog_matching_ID) == 0) {
          # try interpret as integer that was accidentally stored as decimal number, e.g. 123.00
          split_by_dot <- unlist(strsplit(accid, "[.]"))
          if (length(split_by_dot) == 2) {
            sleeplog_matching_ID <- which(sleeplog$ID == split_by_dot[1])
          }
          if (length(sleeplog_matching_ID) == 0) {
            sleeplog_matching_ID <- 1:length(sleeplog$night)
            warning(paste0("No matching sleeplog entry found for acceleromete recording ", accid))
          }
        }
      }
    }
    first_night <- min(min(sib.cla.sum$night),
                       min(as.numeric(sleeplog$night[sleeplog_matching_ID])))
    last_night <- max(max(sib.cla.sum$night),
                      max(as.numeric(sleeplog$night[sleeplog_matching_ID])))
  } else {
    first_night <- min(sib.cla.sum$night)
    last_night <- max(sib.cla.sum$night)
  }
  nnightlist <- first_night:last_night
  if (length(nnightlist) < length(wi)) {
    nnightlist <- nnightlist[1:length(wi)]
  }
  nnights.list <- nnightlist
  nnights.list <- nnights.list[which(is.na(nnights.list) == FALSE)]
  if (excludefirstlast == TRUE & excludelast.part4 == FALSE & excludefirst.part4 == FALSE) {
    # exclude first and last night
    if (length(nnights.list) >= 3) {
      nnights.list <- nnights.list[2:(length(nnights.list) - 1)]
    } else {
      nnights.list <- c()
    }
  } else if (excludelast.part4 == FALSE & excludefirst.part4 == TRUE) {
    if (length(nnights.list) >= 2) {
      nnights.list <- nnights.list[2:length(nnights.list)]
    } else {
      nnights.list <- c()
    }
  } else if (excludelast.part4 == TRUE & excludefirst.part4 == FALSE) {
    if (length(nnights.list) >= 2) {
      nnights.list <- nnights.list[1:(length(nnights.list) - 1)]
    } else {
      nnights.list <- c()
    }
  }
  calendar_date <- wdayname <- rep("", length(nnights.list))
  daysleeper <- rep(FALSE, length(nnights.list)) # woke up after noon
  nightj <- 1 # never read
  guider.df <- data.frame(matrix(NA, length(nnights.list), 5), stringsAsFactors = FALSE)
  names(guider.df) <- c("ID", "night", "duration", "sleeponset", "sleepwake")
  guider.df$night <- nnights.list
  if (dolog == TRUE) {
    if (length(wi) > 0) {
      wi2 <- wi[which(sleeplog$night[wi] %in% guider.df$night)]
      guider.df[which(guider.df$night %in% sleeplog$night[wi2]), ] <- sleeplog[wi2, ]
    }
  }
  nj <- 0
  for (j in nnights.list) {
    nj <- nj + 1
    if (!is.null(progress)) progress("part4", nj, length(nnights.list), paste0("night ", j))
    # default onset and wake for the night; def.noc.sleep chooses how the guider window is set
    if ((length(def.noc.sleep) == 0 ||
         length(SPTE_start) == 0 ||
         length(SPTE_start[which(is.na(SPTE_start) == FALSE)]) == 0) &&
        length(def.noc.sleep) != 2) {
      # use L5+/-6hr algorithm if SPTE fails OR if the user explicitly asks for it
      guider <- "notavailable"
      guider_set <- TRUE
      if (length(L5list) > 0 & length(L5list) >= j) {
        defaultGuiderOnset <- L5list[j] - 6
        defaultGuiderWake <- L5list[j] + 6
        guider <- "L512"
      } else {
        # final backup
        defaultGuiderOnset <- 21
        defaultGuiderWake <- 31
      }
      defaultGuider <- guider
      defaultGuiderCorrected <- NULL
    } else if ((length(def.noc.sleep) == 1 ||
                diary_configured) &&
               length(SPTE_start) != 0) {
      # use the part-3 guider as backup for the sleeplog OR if the user explicitly asks
      defaultGuiderOnset <- SPTE_start[j]
      defaultGuiderWake <- SPTE_end[j]
      defaultGuiderCorrected <- SPTE_corrected[j]
      defaultGuider <- part3_guider[j] # HDCZA, NotWorn, HorAngle (or plus invalid)
      if (is.null(defaultGuider)) {
        # compatibility with versions in which part3_guider was not stored
        guider <- HASPT.algo[1]
        guider_set <- TRUE
        defaultGuider <- guider
      } else {
        if (is.na(defaultGuider)) {
          # no guider for this night; guider keeps the previous night's value, as in GGIR
        } else {
          guider <- defaultGuider
          guider_set <- TRUE
        }
      }
      if (is.na(defaultGuiderOnset) == TRUE) {
        # If SPTE was not derived for this night, use the average estimate of the others
        availableestimate <- which(is.na(SPTE_start) == FALSE)
        cleaningcode <- 6
        if (length(availableestimate) > 0) {
          defaultGuiderOnset <- mean(SPTE_start[availableestimate])
          guider <- defaultGuider <- names(sort(table(part3_guider[availableestimate]),
                                                decreasing = TRUE)[1])
          guider_set <- TRUE
        } else {
          defaultGuiderOnset <- L5list[j] - 6
          guider <- "L512"
          guider_set <- TRUE
        }
      }
      if (is.na(defaultGuiderWake) == TRUE) {
        availableestimate <- which(is.na(SPTE_end) == FALSE)
        cleaningcode <- 6
        if (length(availableestimate) > 0) {
          defaultGuiderWake <- mean(SPTE_end[availableestimate])
          guider <- defaultGuider <- names(sort(table(part3_guider[availableestimate]),
                                                decreasing = TRUE)[1])
          guider_set <- TRUE
        } else {
          defaultGuiderWake <- L5list[j] + 6
          guider <- "L512"
          guider_set <- TRUE
        }
      }
    } else if (length(def.noc.sleep) == 2) {
      # use constant onset and waking time as specified with def.noc.sleep
      defaultGuiderOnset <- def.noc.sleep[1]
      defaultGuiderWake <- def.noc.sleep[2]
      defaultGuider <- guider <- "setwindow"
      guider_set <- TRUE
      defaultGuiderCorrected <- NULL
    }
    if (!guider_set) {
      # GGIR would look up an unassigned local and stop with "object 'guider' not found"
      stop(paste0("No guider could be chosen for night ", j, ": part3_guider is NA while ",
                  "SPTE_start is not, and no earlier night set one. Check the part-3 ",
                  "result."), call. = FALSE)
    }
    if (defaultGuiderOnset >= 24) {
      defaultGuiderOnset <- defaultGuiderOnset - 24
    }
    if (defaultGuiderWake >= 24) {
      defaultGuiderWake <- defaultGuiderWake - 24
    }
    defaultdur <- defaultGuiderWake - defaultGuiderOnset
    sleeplog_used <- FALSE
    if (dolog == TRUE) {
      if (all(!is.na(guider.df[which(guider.df$night == j), 4:5]))) {
        sleeplog_used <- TRUE
      }
    }
    if (sleeplog_used == FALSE) {
      # no diary entry: use the defaults
      guider.df[which(guider.df$night == j), 1:5] <-
        c(accid, j, defaultdur, .raw.nights.hours2clock(defaultGuiderOnset),
          .raw.nights.hours2clock(defaultGuiderWake))
      cleaningcode <- 1
    }
    nightj <- nightj + 1
    acc_available <- TRUE
    spocum <- data.frame(nb = numeric(0), start = numeric(0), end = numeric(0),
                         overlapGuider = numeric(0), def = character(0))
    spocumi <- 1
    guider.df2 <- guider.df[which(guider.df$night == j), ]
    # guider onset and wake as clock strings, and whether this is a daysleeper
    tmp1 <- format(guider.df2$sleeponset[1])
    GuiderOnset <- .raw.nights.clock2hours(tmp1)
    tmp4 <- format(guider.df2$sleepwake[1])
    GuiderWake <- .raw.nights.clock2hours(tmp4)
    daysleeper[j] <- FALSE
    if (is.na(GuiderOnset) == FALSE & is.na(GuiderWake) == FALSE & tmp1 != "" & tmp4 != "") {
      tmp1 <- .raw.nights.doubledigit(tmp1)
      tmp4 <- .raw.nights.doubledigit(tmp4)
      # a sleep period that overlaps noon makes a daysleeper
      if (GuiderWake > 12 & GuiderOnset < 12) daysleeper[j] <- TRUE
      if (GuiderWake > 12 & GuiderOnset > GuiderWake) daysleeper[j] <- TRUE
      # to the continuous 12..36 axis
      if (GuiderOnset < 12) GuiderOnset <- GuiderOnset + 24
      if (GuiderWake <= 12) GuiderWake <- GuiderWake + 24
      if (GuiderWake > 12 & GuiderWake < 18 & daysleeper[j] == TRUE) GuiderWake <- GuiderWake + 24
      if (daysleeper[j] == TRUE) {
        logdur <- GuiderOnset - GuiderWake
      } else {
        logdur <- GuiderWake - GuiderOnset
      }
      if (sleeplog_used == TRUE) {
        cleaningcode <- 0
        guider <- "sleeplog"
        guider_set <- TRUE
      }
    } else {
      GuiderOnset <- defaultGuiderOnset
      GuiderWake <- defaultGuiderWake + 24
      logdur <- GuiderWake - GuiderOnset
      cleaningcode <- 1 # no diary for this night
      sleeplog_used <- FALSE
    }
    # a daysleeper needs the next day loaded too
    if (excludefirstlast == FALSE) {
      if (daysleeper[j] == TRUE & j != max(nnights.list)) {
        loaddays <- 2
      } else {
        loaddays <- 1
      }
      if (daysleeper[j] == TRUE & j == max(nnights.list)) {
        # a daysleeper on the last night is treated as a normal day, wake capped at noon
        daysleeper[j] <- FALSE
        loaddays <- 1
        if (GuiderWake > 36) GuiderWake <- 36
        logdur <- GuiderWake - GuiderOnset
      }
    } else {
      # last day excluded, so the last-night case does not arise
      if (daysleeper[j] == TRUE) {
        loaddays <- 2
      } else {
        loaddays <- 1
      }
    }
    dummyspo <- data.frame(nb = numeric(1), start = numeric(1), end = numeric(1),
                           overlapGuider = numeric(1), def = character(1), duration = numeric(1))
    dummyspo$nb[1] <- 1
    # spo_day and spo_day2 persist across nights, as GGIR's eval(parse()) objects do;
    # only spo_day_exists is per night
    spo_day_exists <- FALSE
    defs <- unique(sib.cla.sum$definition) # see van Hees 2015 PLoSONE
    for (k in defs) {
      # a daysleeper needs the afternoon of the next day too
      for (loaddaysi in 1:loaddays) {
        qq <- sib.cla.sum
        sleepdet <- qq[which(qq$night == (j + (loaddaysi - 1))), ]
        if (nrow(sleepdet) == 0) {
          if (spocumi == 1) {
            spocum <- dummyspo
          } else {
            spocum <- rbind(spocum, dummyspo)
          }
          spocumi <- spocumi + 1
          cleaningcode <- 3
          acc_available <- FALSE
        } else if (length(tail_expansion_log) != 0 & sumi == max(nnightlist)) {
          # last night of an expanded recording
          cleaningcode <- 3
          acc_available <- FALSE
        } else {
          acc_available <- TRUE
        }
        if (nrow(sleepdet) == 0) next
        ki <- which(sleepdet$definition == k)
        if (length(ki) == 0) next
        sleepdet.t <- sleepdet[ki, ]
        if (loaddaysi == 1) remember_fraction_invalid_day1 <- sleepdet.t$fraction.night.invalid[1]
        nsp <- length(unique(sleepdet.t$sib.period))
        spo <- data.frame(nb = numeric(nsp), start = numeric(nsp), end = numeric(nsp),
                          overlapGuider = numeric(nsp), def = character(nsp))
        spo_exists <- TRUE
        if (nsp <= 1 & unique(sleepdet.t$sib.period)[1] == 0) {
          # no sleep periods
          spo$nb[1] <- 1
          spo[1, c("start", "end", "overlapGuider")] <- 0
          spo$def[1] <- k
          if (daysleeper[j] == TRUE) {
            spo_day[k] <- list(NULL)
            spo_day_exists <- TRUE
          }
        } else {
          DD <- .raw.nights.spmat(nsp, spo, sleepdet.t, daysleep = daysleeper[j])
          if (loaddaysi == 1) {
            wdayname[j] <- DD$wdayname
            calendar_date[j] <- DD$calendar_date
          }
          spo <- DD$spo
          if (daysleeper[j] == TRUE) {
            if (loaddaysi == 1) {
              w1 <- which(spo$end >= 18) # only periods ending after 6pm
              if (length(w1) > 0) {
                spo <- spo[w1, ]
                if (nrow(spo) == 1) {
                  if (spo$start[1] <= 18) spo$start[1] <- 18
                } else {
                  spo$start[which(spo$start <= 18)] <- 18
                }
                spo_day[[k]] <- spo
                spo_day_exists <- TRUE
              } else {
                spo_day[k] <- list(NULL)
                spo_day_exists <- TRUE
              }
            } else if (loaddaysi == 2 & spo_day_exists == TRUE) {
              w2 <- which(spo$start < 18) # only periods starting before 6pm
              if (length(w2) > 0) {
                spo <- spo[w2, ]
                if (ncol(spo) == 1) spo <- t(spo)
                if (nrow(spo) == 1) {
                  if (spo$end[1] > 18) spo$end[1] <- 18
                } else {
                  spo$end[which(spo$end > 18)] <- 18
                }
                # day 2 on the continuous axis
                spo[, c("start", "end")] <- spo[, c("start", "end")] + 24
                spo_day2[[k]] <- spo
              } else {
                spo_day2[k] <- list(NULL)
              }
              # stitch the two days
              spo <- rbind(spo_day[[k]], spo_day2[[k]])
              spo_exists <- TRUE
            }
          }
          if (daysleeper[j] == TRUE) {
            if (GuiderWake < 21 & GuiderWake > 12 & GuiderOnset > GuiderWake) {
              # waking up in the afternoon should have a value above 36
              GuiderWake <- GuiderWake + 24
            }
          }
          # When no sib overlaps the SPT, two tiny artificial sibs are added at the guider
          # edges so part 5 still gets them, and cleaningcode is 5
          relyonguider_thisnight <- FALSE
          if (length(data_cleaning_file) > 0) {
            if (length(which(DaCleanFile$relyonguider_part4 == j &
                             DaCleanFile$ID == accid)) > 0) {
              relyonguider_thisnight <- TRUE
              cleaningcode <- 5 # user specified to rely on guider
            }
          }
          if (length(spo) == 0) {
            # spo may have been emptied above; the code below needs a row
            spo <- data.frame(nb = numeric(1), start = numeric(1), end = numeric(1),
                              overlapGuider = numeric(1), def = character(1))
            spo$nb[1] <- 1
            spo[1, 2:4] <- 0
            spo$def[1] <- k
            spo_exists <- TRUE
          }
          if (length(which(spo$start < GuiderWake &
                           spo$end > GuiderOnset)) == 0) {
            relyonguider_thisnight <- TRUE
            cleaningcode <- 5
          }
          # invalid time used in part 3 (HASPT.ignore.invalid): rely on the guider, no code 5
          if (grepl("+invalid", guider) | grepl("+invalid", defaultGuider)) {
            relyonguider_thisnight <- TRUE
          }
          if (relyonguider_thisnight == TRUE) {
            if (guider != "NotWorn") {
              newlines <- rbind(spo[1, ], spo[1, ])
              newlines[1, 1:4] <- c(nrow(spo) + 1, GuiderOnset, GuiderOnset + 1/60, 1)
              newlines[2, 1:4] <- c(nrow(spo) + 1, GuiderWake - 1/60, GuiderWake, 1)
              spo <- rbind(spo, newlines)
            } else {
              # with NotWorn, trust the guider and ignore the sibs
              newlines <- spo[1, ]
              newlines[1, 1:4] <- c(nrow(spo) + 1, GuiderOnset, GuiderWake, 1)
              spo <- newlines
            }
            spo <- spo[order(spo$start), ]
            spo$nb <- 1:nrow(spo)
            relyonguider_thisnight <- TRUE
          }
          # classify each bout as inside the guider window or not
          for (evi in 1:nrow(spo)) {
            if (spo$start[evi] < GuiderWake && spo$end[evi] > GuiderOnset) {
              if ((sleepwindowType == "TimeInBed" || guider == "markerbutton") &&
                  sum(sib_must_fully_overlap_with_TimeInBed) != 0) {
                if (all(sib_must_fully_overlap_with_TimeInBed) &&
                    spo$start[evi] > GuiderOnset && spo$end[evi] < GuiderWake) {
                  spo$overlapGuider[evi] <- 1
                }
                if (all(sib_must_fully_overlap_with_TimeInBed == c(FALSE, TRUE)) &&
                    spo$end[evi] < GuiderWake) {
                  spo$overlapGuider[evi] <- 1
                }
                if (all(sib_must_fully_overlap_with_TimeInBed == c(TRUE, FALSE)) &&
                    spo$start[evi] > GuiderOnset) {
                  spo$overlapGuider[evi] <- 1
                }
              } else {
                spo$overlapGuider[evi] <- 1 # partial overlap
              }
              if (relyonguider == TRUE | relyonguider_thisnight == TRUE) {
                # snap the bout edges to the guider
                if ((spo$start[evi] < GuiderWake && spo$end[evi] > GuiderWake) |
                    (spo$start[evi] < GuiderWake && spo$end[evi] < spo$start[evi])) {
                  spo$end[evi] <- GuiderWake
                }
                if ((spo$start[evi] < GuiderOnset && spo$end[evi] > GuiderOnset) |
                    (spo$end[evi] > GuiderOnset && spo$end[evi] < spo$start[evi])) {
                  spo$start[evi] <- GuiderOnset
                }
              }
            }
          }
          spo$duration <- spo$end - spo$start
          if (daysleeper[j] == TRUE) {
            # back to a 24 h scale for the second day
            reversetime2 <- which(spo$start >= 36)
            reversetime3 <- which(spo$end >= 36)
            if (length(reversetime2) > 0) spo$start[reversetime2] <- spo$start[reversetime2] - 24
            if (length(reversetime3) > 0) spo$end[reversetime3] <- spo$end[reversetime3] - 24
          }
        }
      }
      # spo holds the sibs of one definition, spocum of all definitions
      if (spo_exists) {
        spo$def <- k
        if (spocumi == 1) {
          spocum <- spo
        } else {
          spocum <- rbind(spocum, spo)
        }
        spocumi <- spocumi + 1
      }
    }
    # page counters that feed the page column
    if (do.visual == TRUE) {
      if (cnt == (nnpp + 1)) {
        pagei <- pagei + 1
        cnt <- 1
      }
    }
    if (length(spocum) > 0) {
      NAvalues <- which(is.na(spocum$def) == TRUE)
      if (length(NAvalues) > 0) {
        spocum <- spocum[-NAvalues, ]
      }
    }
    # night summary
    if (length(spocum) > 0 & class(spocum)[1] == "data.frame" & length(calendar_date) >= j) {
      if (nrow(spocum) > 0 & ncol(spocum) >= 5 & calendar_date[j] != "") {
        undef <- unique(spocum$def)
        undef <- undef[undef != ""]
        for (defi in undef) {
          rowswithdefi <- which(spocum$def == defi)
          if (length(rowswithdefi) > 0) {
            spocum.t <- spocum[rowswithdefi, ]
            evin <- 2
            if (guider == "NotWorn") {
              while (evin <= nrow(spocum.t)) {
                if (spocum.t$start[evin] - spocum.t$start[evin - 1] < -2) {
                  spocum.t$start[evin] <- spocum.t$start[evin] + 24
                }
                if (spocum.t$end[evin] - spocum.t$end[evin - 1] < -2) {
                  spocum.t$end[evin] <- spocum.t$end[evin] + 24
                }
                evin <- evin + 1
              }
            }
            # in DST a double hour may not be recognised as part of the SPT
            delta_t1 <- diff(as.numeric(spocum.t$end))
            spocum.t$overlapGuider <- .raw.nights.correct01010(spocum.t$overlapGuider)
            nightsummary[sumi, 1] <- accid
            nightsummary[sumi, 2] <- j
            spocum.t <- spocum.t[!duplicated(spocum.t), ]
            # accelerometer onset and wake
            if (length(which(as.numeric(spocum.t$overlapGuider) == 1)) > 0) {
              rtl <- which(spocum.t$overlapGuider == 1)
              nightsummary[sumi, 3] <- spocum.t$start[rtl[1]]
              nightsummary[sumi, 4] <- spocum.t$end[rtl[length(rtl)]]
            } else {
              cleaningcode <- 5
              nightsummary[sumi, 3] <- GuiderOnset
              nightsummary[sumi, 4] <- GuiderWake
            }
            nightsummary[, 3] <- as.numeric(nightsummary[, 3])
            nightsummary[, 4] <- as.numeric(nightsummary[, 4])
            # onset after wake is impossible; a daysleeper's wake before noon moves a day on
            if (nightsummary[sumi, 3] > nightsummary[sumi, 4] &
                nightsummary[sumi, 4] < 36 & daysleeper[j] == TRUE) {
              nightsummary[sumi, 4] <- nightsummary[sumi, 4] + 24
            }
            if (nightsummary[sumi, 3] == nightsummary[sumi, 4] & nightsummary[sumi, 4] == 18) {
              # sleeping from 6pm to 6pm (probably non-wear)
              nightsummary[sumi, 4] <- nightsummary[sumi, 4] + 24
            }
            nightsummary[sumi, 5] <- nightsummary[sumi, 4] - nightsummary[sumi, 3] # SptDuration
            nightsummary[, 5] <- as.numeric(nightsummary[, 5])
            nightsummary[sumi, 6] <- defi
            # guider edges back into [12, 36], except a daysleeper's wake
            if (GuiderOnset > 36) {
              nightsummary[sumi, 7] <- GuiderOnset - 24
            } else {
              nightsummary[sumi, 7] <- GuiderOnset
            }
            if (GuiderWake > 36 & daysleeper[j] == FALSE) {
              nightsummary[sumi, 8] <- GuiderWake - 24
            } else {
              nightsummary[sumi, 8] <- GuiderWake
            }
            if (nightsummary[sumi, 7] > nightsummary[sumi, 8]) {
              nightsummary[sumi, 9] <- abs((36 - nightsummary[sumi, 7]) +
                                             (nightsummary[sumi, 8] - 12))
            } else {
              nightsummary[sumi, 9] <- abs(nightsummary[sumi, 8] - nightsummary[sumi, 7])
            }
            # error: estimate minus guider, wrapped into (-12, 12]
            nightsummary[sumi, 10] <- nightsummary[sumi, 3] - nightsummary[sumi, 7]
            nightsummary[sumi, 11] <- nightsummary[sumi, 4] - nightsummary[sumi, 8]
            if (nightsummary[sumi, 10] > 12) nightsummary[sumi, 10] <- -(24 - nightsummary[sumi, 10])
            if (nightsummary[sumi, 10] < -12) nightsummary[sumi, 10] <- -(nightsummary[sumi, 10] + 24)
            if (nightsummary[sumi, 11] > 12) nightsummary[sumi, 11] <- -(24 - nightsummary[sumi, 11])
            if (nightsummary[sumi, 11] < -12) nightsummary[sumi, 11] <- -(nightsummary[sumi, 11] + 24)
            nightsummary[sumi, 12] <- nightsummary[sumi, 5] - nightsummary[sumi, 9]
            if (acc_available == TRUE) {
              nightsummary[sumi, 13] <- remember_fraction_invalid_day1
              if (remember_fraction_invalid_day1 > ((24 - includenightcrit) / 24)) {
                cleaningcode <- 2
              }
            } else {
              nightsummary[sumi, 13] <- 1
            }
            # accumulated sib time inside the SPT and during the day
            overlap <- which(spocum.t$overlapGuider == 1)
            nocs <- spocum.t$duration[overlap]
            no_overlap <- which(spocum.t$overlapGuider == 0)
            sibds <- spocum.t$duration[no_overlap]
            # nocs is negative for an episode that spans the autumn clock change
            negval <- which(nocs < 0)
            if (length(negval) > 0) {
              kk0 <- as.numeric(spocum.t$start[which(spocum.t$overlapGuider == 1)])
              kk1 <- as.numeric(spocum.t$end[which(spocum.t$overlapGuider == 1)])
              kk1[negval] <- kk1[negval] + 1
              nocs <- kk1 - kk0
            }
            if (length(nocs) > 0) {
              spocum.t.dur.noc <- sum(nocs)
            } else {
              spocum.t.dur.noc <- 0
            }
            # daylight saving: an episode spanning the skipped spring hour is an hour shorter
            is_this_a_dst_night_output <- .raw.nights.dst(calendar_date = calendar_date[j],
                                                          tz = desiredtz)
            dst_night_or_not <- is_this_a_dst_night_output$dst_night_or_not
            dsthour <- is_this_a_dst_night_output$dsthour
            if (dst_night_or_not == 1) {
              checkoverlap <- spocum.t[which(spocum.t$overlapGuider == 1), c("start", "end")]
              if (nrow(checkoverlap) > 0) {
                overlaps <- which(checkoverlap[, 1] <= (dsthour + 24) &
                                    checkoverlap[, 2] >= (dsthour + 25))
              } else {
                overlaps <- c()
              }
              if (length(overlaps) > 0) {
                spocum.t.dur.noc <- spocum.t.dur.noc - 1
                nightsummary[sumi, 5] <- nightsummary[sumi, 5] - 1
                nightsummary[sumi, 9] <- nightsummary[sumi, 9] - 1
              }
            } else if (dst_night_or_not == -1) {
              # autumn: elapsed time from onset to wake grows by the doubled hour
              if (nightsummary[sumi, 3] <= (dsthour + 24) &
                  nightsummary[sumi, 4] >= (dsthour + 25)) {
                nightsummary[sumi, 5] <- nightsummary[sumi, 5] + 1
              }
              if (nightsummary[sumi, 7] <= (dsthour + 24) &
                  nightsummary[sumi, 8] >= (dsthour + 25)) {
                nightsummary[sumi, 9] <- nightsummary[sumi, 9] + 1
              }
              # does the SPT end within the double hour?
              nightsummary[sumi, ] <- .raw.nights.correct.double.hour(
                nightsummary[sumi, ], onsetcol = 3, wakecol = 4, durcol = 5,
                dsthour = dsthour, delta_t1 = delta_t1)
              nightsummary[sumi, ] <- .raw.nights.correct.double.hour(
                nightsummary[sumi, ], onsetcol = 7, wakecol = 8, durcol = 9,
                dsthour = dsthour, delta_t1 = delta_t1)
            }
            sibds_atleast15min <- 0
            if (length(sibds) > 0) {
              spocum.t.dur_sibd <- sum(sibds)
              atleast15min <- which(sibds >= 1/4)
              if (length(atleast15min) > 0) {
                sibds_atleast15min <- sibds[atleast15min]
                spocum.t.dur_sibd_atleast15min <- sum(sibds_atleast15min)
              } else {
                spocum.t.dur_sibd_atleast15min <- 0
              }
            } else {
              spocum.t.dur_sibd <- 0
              spocum.t.dur_sibd_atleast15min <- 0
            }
            nightsummary[sumi, 14] <- spocum.t.dur.noc # SleepDurationInSpt
            nightsummary[sumi, 15] <- nightsummary[sumi, 5] - spocum.t.dur.noc # WASO
            nightsummary[sumi, 16] <- spocum.t.dur_sibd # sib duration during waking hours
            nightsummary[sumi, 17] <- length(which(spocum.t$overlapGuider == 1))
            nightsummary[sumi, 18] <- nightsummary[sumi, 17] - 1 # number of awakenings
            nightsummary[sumi, 19] <- length(which(spocum.t$overlapGuider == 0))
            nightsummary[sumi, 20] <- as.numeric(spocum.t.dur_sibd_atleast15min)
            # clock strings
            acc_onset <- nightsummary[sumi, 3]
            acc_wake <- nightsummary[sumi, 4]
            if (acc_onset > 24) acc_onset <- acc_onset - 24
            if (acc_wake > 24) acc_wake <- acc_wake - 24
            acc_onsetTS <- .raw.nights.hours2clock(acc_onset)
            acc_wakeTS <- .raw.nights.hours2clock(acc_wake)
            nightsummary[sumi, 21] <- acc_onsetTS
            nightsummary[sumi, 22] <- acc_wakeTS
            nightsummary[sumi, 23] <- tmp1 # guider_onset_ts
            nightsummary[sumi, 24] <- tmp4 # guider_wake_ts
            if (sleepwindowType == "TimeInBed" || guider == "markerbutton") {
              # sleep latency and efficiency, only with time in bed or a marker button
              nightsummary[sumi, 25] <- round(nightsummary[sumi, 3] - nightsummary[sumi, 7],
                                              digits = 7)
              if (sleepefficiency.metric == 1) {
                nightsummary[sumi, 26] <- round(nightsummary[sumi, 14] / nightsummary[sumi, 9],
                                                digits = 5)
              } else if (sleepefficiency.metric == 2) {
                nightsummary[sumi, 26] <- round(nightsummary[sumi, 14] /
                                                  (nightsummary[sumi, 5] + nightsummary[sumi, 25]),
                                                digits = 5)
              }
            }
            nightsummary[sumi, 27] <- pagei
            nightsummary[sumi, 28] <- daysleeper[j]
            nightsummary[sumi, 29] <- wdayname[j]
            nightsummary[sumi, 30] <- calendar_date[j]
            nightsummary[sumi, 31] <- fname
            # page bookkeeping only; the pdf is not drawn
            if (do.visual == TRUE) {
              if (defi == undef[1]) {
                if (outliers.only == TRUE) {
                  if (abs(nightsummary$error_onset[sumi]) > criterror |
                      abs(nightsummary$error_wake[sumi]) > criterror |
                      abs(nightsummary$error_dur[sumi]) > (criterror * 2)) {
                    doplot <- TRUE
                  } else {
                    doplot <- FALSE
                  }
                } else {
                  doplot <- TRUE
                }
              }
              if (diary_configured) {
                cleaningcriterion <- 1
              } else {
                cleaningcriterion <- 2
              }
            }
            nightsummary[sumi, 32] <- cleaningcode
            nightsummary[sumi, 33] <- sleeplog_used
            nightsummary[sumi, 34] <- logid
            nightsummary[sumi, 35] <- acc_available
            nightsummary[sumi, 36] <- guider
            # SRI for this night; frac_valid must be strictly above includenightcrit/24
            nightsummary[sumi, 37:38] <- NA
            if (is.null(SleepRegularityIndex)) {
              SleepRegularityIndex <- NA
            }
            SRI <- SleepRegularityIndex
            if (is.data.frame(SRI) == TRUE) {
              calendar_date_asDate <- format(as.Date(calendar_date[j], format = "%d/%m/%Y"),
                                             format = ("%d/%m/%Y"))
              calendar_date_reformat <- format(x = calendar_date_asDate, format = "%d/%m/%Y")
              SRIindex <- which(SRI$date == calendar_date_reformat &
                                  SRI$frac_valid > (includenightcrit / 24))
              if (length(SRIindex) > 0) {
                nightsummary[sumi, 37] <- SRI$SleepRegularityIndex[SRIindex[1]]
                nightsummary[sumi, 38] <- SRI$frac_valid[SRIindex[1]]
              }
            }
            if (length(longitudinal_axis) == 0) {
              nightsummary[sumi, 39] <- NA
            } else {
              nightsummary[sumi, 39] <- longitudinal_axis
            }
            if (length(defaultGuiderCorrected) == 0 | guider == "sleeplog") {
              nightsummary[sumi, 40] <- NA
            } else {
              nightsummary[sumi, 40] <- defaultGuiderCorrected
            }
            if (storefolderstructure == TRUE) {
              nightsummary[sumi, 41] <- ffd
              nightsummary[sumi, 42] <- ffp
            }
            # keep the labelled bouts GGIR only draws
            ep <- spocum.t
            if (!("duration" %in% names(ep))) ep$duration <- ep$end - ep$start
            episodes[[length(episodes) + 1]] <- data.frame(
              night = j, def = as.character(ep$def), nb = as.numeric(ep$nb),
              start = as.numeric(ep$start), end = as.numeric(ep$end),
              overlapGuider = as.numeric(ep$overlapGuider),
              duration = as.numeric(ep$duration), stringsAsFactors = FALSE)
            sumi <- sumi + 1
          }
          if (do.visual == TRUE) {
            if (cleaningcode < cleaningcriterion & doplot == TRUE) {
              # count the night once a bar would have been plotted, on the last definition
              if (defi == undef[length(undef)]) {
                cnt <- cnt + 1
              }
            }
          }
        }
      }
    }
    guiders[[length(guiders) + 1]] <- data.frame(
      night = j, guider = as.character(guider),
      guider_onset = as.numeric(GuiderOnset), guider_wakeup = as.numeric(GuiderWake),
      daysleeper = as.logical(daysleeper[j]), loaddays = as.numeric(loaddays),
      sleeplog_used = as.logical(sleeplog_used), cleaningcode = as.numeric(cleaningcode),
      calendar_date = as.character(if (length(calendar_date) >= j) calendar_date[j] else ""),
      weekday = as.character(if (length(wdayname) >= j) wdayname[j] else ""),
      stringsAsFactors = FALSE)
  }
  if (length(nnights.list) == 0) {
    # no nights to analyse
    nightsummary[sumi, 1:2] <- c(accid, 0)
    nightsummary[sumi, c(3:30, 34, 36:39)] <- NA
    nightsummary[sumi, 31] <- fname
    nightsummary[sumi, 32] <- 4 # cleaningcode 4, no nights
    nightsummary[sumi, c(33, 35)] <- c(FALSE, TRUE) # sleeplog_used, acc_available
    if (storefolderstructure == TRUE) {
      nightsummary[sumi, 41:42] <- c(ffd, ffp)
    }
    sumi <- sumi + 1
  }
  if (sleepwindowType != "TimeInBed" && consider_marker_button == FALSE) {
    nightsummary <- nightsummary[, which(colnames(nightsummary) %in%
                                           c("sleeplatency", "sleepefficiency") == FALSE)]
  }
  # the GGIR version the port matches, the same label parts 2 and 5 write
  GGIRversion <- params$ggir_version_label %||% .RAW_GGIR_VERSION_LABEL
  if (nrow(nightsummary) > 0) {
    nightsummary$GGIRversion <- GGIRversion
  }
  finish(nightsummary, "ok")
}

# GGIR SHAPES AND PRINTING

#' GGIR's ms4 Nightsummary From a canhrActi Night Table
#'
#' Strips canhrActi's class and its \code{"canhrActi"} attribute, leaving the plain data
#' frame GGIR saves into \code{meta/ms4.out/<file>.RData} as \code{nightsummary}, so that
#' \code{identical()} against a stored milestone works.
#'
#' @param x A canhrActi_raw_nights from \code{\link{raw.sleep.nights}}.
#' @return A plain data.frame.
#' @examples
#' \dontrun{
#' e <- new.env(); load("meta/ms4.out/MOS2E39230594.gt3x.RData", envir = e)
#' identical(as.ggir.nightsummary(nights), e$nightsummary)
#' }
#' @export
as.ggir.nightsummary <- function(x) {
  if (!inherits(x, "canhrActi_raw_nights")) {
    stop("x must be a canhrActi_raw_nights object from raw.sleep.nights()", call. = FALSE)
  }
  attr(x, "canhrActi") <- NULL
  class(x) <- "data.frame"
  x
}

#' Print Method for a Night Table
#'
#' @param x A canhrActi_raw_nights.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_nights <- function(x, ...) {
  meta <- attr(x, "canhrActi")
  cat("\ncanhrActi per-night sleep summary (GGIR part 4: g.part4)\n")
  if (!is.null(meta$filename) && !is.na(meta$filename)) {
    cat("  file:        ", meta$filename, "\n", sep = "")
  }
  if (!is.null(meta$id) && !is.na(meta$id)) cat("  id:          ", meta$id, "\n", sep = "")
  cat("  timezone:    ",
      if (is.null(meta$desiredtz) || !nzchar(meta$desiredtz)) {
        paste0("part 1 recorded none, so the system zone (", Sys.timezone(), ")")
      } else {
        meta$desiredtz
      }, "\n", sep = "")
  cat("  window:      ", meta$settings$sleepwindowType,
      if (isTRUE(meta$dolog)) ", sleep diary used" else ", no sleep diary", "\n", sep = "")
  if (!identical(meta$status$state, "ok")) {
    cat("  state:       ", meta$status$state,
        " (GGIR would write no part-4 milestone)\n", sep = "")
  }
  cat("  nights:      ", nrow(x), " rows x ", ncol(x), " columns\n", sep = "")
  if (nrow(x) > 0) {
    keep <- c("night", "sleeponset", "wakeup", "SptDuration", "SleepDurationInSpt", "WASO",
              "number_sib_sleepperiod", "cleaningcode", "guider")
    keep <- keep[keep %in% names(x)]
    y <- as.data.frame(x)[, keep, drop = FALSE]
    for (nm in names(y)) if (is.numeric(y[[nm]])) y[[nm]] <- round(y[[nm]], 4)
    print(y, row.names = FALSE)
  }
  if (length(meta$status$messages) > 0) {
    cat("  messages:\n")
    for (m in meta$status$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(meta$elapsed) && !is.na(meta$elapsed)) {
    cat("  elapsed:     ", round(meta$elapsed, 3), " s\n", sep = "")
  }
  invisible(x)
}
