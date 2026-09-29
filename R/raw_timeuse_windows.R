# Ported from GGIR 3.3-9 R/g.part5.definedays.R, R/g.part5.wakesleepwindows.R,
# R/g.part5.addfirstwake.R, R/g.part5.fixmissingnight.R, R/g.part5.onsetwaketiming.R and
# the midnight and night-trimming blocks of R/g.part5.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The two inline blocks become functions
# of their arguments; definedays loses its three unread arguments and the unread
# qqq_backup return, has its two label helpers hoisted to file level, keeps the GGIR
# 3.3.6 WW/OO branch by default with 3.3-9's behind segment_all_windows, never reproduces
# the 3.3-9 NA crash on a one-wake recording, and raises on an unrecognised timewindow
# where GGIR loops forever; wakesleepwindows nests is.ISO8601 as a local and calls
# .raw.iso8601.to.posix in place of iso8601chartime2POSIX.

# MIDNIGHTS

#' Midnight Indices, and the Clock Vectors Part 5 Reads Them From
#'
#' \code{nightsi} is the MM window grid, shifted by \code{dayborder}; \code{nightsi2} is
#' always the true midnights and is what \code{.raw.timeuse.wakesleep} must be given, because
#' its sleep-log fallback is relative to real midnight. \code{sec}, \code{min} and \code{hour}
#' are computed once here and carried into the segment analysis. \code{.raw.detect.midnight}
#' is a different GGIR function (g.detecmidnight) and is not reused.
#'
#' @details The minute test \code{(dayborder - floor(dayborder)) * 60} is an exact floating
#'   point comparison, so a dayborder whose minute part is not representable (4.1 gives
#'   6.000000000000005) finds no midnight at all.
#'
#' @param time_POSIX The whole ts time column as POSIXct.
#' @param dayborder GGIR's dayborder, in decimal hours.
#' @param time_char The ISO8601 character timestamps, used only for GGIR's re-parse fallback.
#'   NULL skips the fallback.
#' @param desiredtz Timezone used by that fallback.
#' @return list(nightsi, nightsi2, sec, min, hour, time_POSIX). \code{time_POSIX} is returned
#'   because GGIR's fallback replaces it.
#' @keywords internal
#' @noRd
.raw.timeuse.midnights <- function(time_POSIX, dayborder = 0, time_char = NULL,
                                   desiredtz = "") {
  tempp <- as.POSIXlt(time_POSIX)
  if (is.na(tempp$sec[1]) == TRUE) {
    if (!is.null(time_char)) {
      time_POSIX <- as.POSIXct(time_char, tz = desiredtz)
      tempp <- as.POSIXlt(time_POSIX)
    }
  }
  sec <- tempp$sec
  min <- tempp$min
  hour <- tempp$hour
  if (dayborder == 0) {
    nightsi <- which(sec == 0 & min == 0 & hour == 0)
    nightsi2 <- nightsi # nightsi2 will be used in .raw.timeuse.wakesleep
  } else {
    # shift the definition of midnight if required
    nightsi <- which(sec == 0 &
                       min == (dayborder - floor(dayborder)) * 60 &
                       hour == floor(dayborder))
    nightsi2 <- which(sec == 0 & min == 0 & hour == 0)
  }
  return(invisible(list(nightsi = nightsi, nightsi2 = nightsi2,
                        sec = sec, min = min, hour = hour,
                        time_POSIX = time_POSIX)))
}

# NIGHT TRIMMING

#' Drop the Midnights That Cannot Open a Window of This Timewindow Type
#'
#' Run once per timewindow, before the window loop. For MM every midnight more than twelve
#' hours outside the first and last sleep boundary is discarded, so a recording whose last
#' days carry no part-4 night produces no MM window for them. For WW and OO the surviving
#' midnights decide only whether the window loop runs at all, since the window edges come
#' from \code{diff(ts$diur)}. The caller does the other two things GGIR's block does:
#' \code{ts$window <- 0} and the rename of the "part3_estimate" guider.
#'
#' @details \code{FM} and \code{SO} are arguments so the caller can compute them once. An
#'   unrecognised timewindow raises; GGIR leaves Nwindows undefined and hangs in the window
#'   loop.
#'
#' @param nightsi The untrimmed midnight grid from \code{.raw.timeuse.midnights}.
#' @param ts The part-5 time series; only \code{ts$diur} is read.
#' @param timewindowi One of "MM", "WW", "OO".
#' @param ws3new Epoch length in seconds, 60 when part5_agg2_60seconds aggregated the series.
#' @param FM,SO Wake and onset transition indices; NULL computes them from \code{ts$diur}.
#' @return list(nightsi, Nwindows).
#' @keywords internal
#' @noRd
.raw.timeuse.trim.nights <- function(nightsi, ts, timewindowi, ws3new, FM = NULL, SO = NULL) {
  if (!timewindowi %in% c("MM", "WW", "OO")) {
    stop("timewindow must be one of MM, WW or OO, not ", timewindowi, call. = FALSE)
  }
  # ignore all nights in 'inights' before the first waking up and after the last waking up
  if (is.null(FM)) FM <- which(diff(ts$diur) == -1)
  if (is.null(SO)) SO <- which(diff(ts$diur) == 1)
  if (timewindowi == "WW") {
    if (length(FM) > 0) {
      # ignore the first and last midnight; no sleep detection was done on them
      nightsi <- nightsi[nightsi > FM[1] & nightsi < FM[length(FM)]]
    }
  } else if (timewindowi == "OO") {
    if (length(SO) > 0) {
      # ignore data before the first sleep onset and after the last sleep onset
      nightsi <- nightsi[nightsi > SO[1] & nightsi < SO[length(SO)]]
    }
  } else {
    # if first night is missing then nights needs to align with diur
    startend_sleep <- which(abs(diff(ts$diur)) == 1)
    Nepochsin12Hours <- (60 / ws3new) * 60 * 12
    nightsi <- nightsi[nightsi >= (startend_sleep[1] - Nepochsin12Hours) &
                         nightsi <= (startend_sleep[length(startend_sleep)] + Nepochsin12Hours)]
  }
  if (timewindowi == "MM") {
    Nwindows <- length(nightsi) + 1 # +1 to include the data after last awakening
  } else if (timewindowi == "WW") {
    Nwindows <- length(which(diff(ts$diur) == -1))
  } else if (timewindowi == "OO") {
    Nwindows <- length(which(diff(ts$diur) == 1))
  }
  return(invisible(list(nightsi = nightsi, Nwindows = Nwindows)))
}

# DAY WINDOWS

#' qwindow Hours to "HH:MM:SS" Segment Breaks
#'
#' @details Each break's seconds are snapped to the nearest whole epoch, with a carry when
#'   that lands on 60, and "00:00:00" is prepended when the first break is not midnight.
#'
#' @param qwindow Numeric vector of clock hours, sorted, starting at 0 and ending at 24.
#' @param epochSize Epoch length in seconds.
#' @return Character vector of "HH:MM:SS" breaks.
#' @keywords internal
#' @noRd
.raw.timeuse.qwindow2timestamp <- function(qwindow, epochSize) {
  hour <- floor(qwindow)
  minute <- floor((qwindow - hour) * 60)
  second <- floor((qwindow - hour - minute / 60) * 60 * 60)
  expected_seconds <- seq(0, 60, by = epochSize)
  seconds_tobe_revised <- which(!second %in% expected_seconds)
  if (length(seconds_tobe_revised) > 0) {
    for (si in seconds_tobe_revised) {
      second[si] <- expected_seconds[which.min(abs(expected_seconds - second[si]))]
      if (second[si] == 60) { # shift minute if second == 60
        minute[si] <- minute[si] + 1
        second[si] <- 0
      }
    }
  }
  hour <- as.character(hour); minute <- as.character(minute); second <- as.character(second)
  hour <- ifelse(nchar(hour) == 1, paste0("0", hour), hour)
  minute <- ifelse(nchar(minute) == 1, paste0("0", minute), minute)
  second <- ifelse(nchar(second) == 1, paste0("0", second), second)
  HMS <- paste(hour, minute, second, sep = ":")
  if (HMS[1] != "00:00:00") HMS <- c("00:00:00", HMS)
  return(HMS)
}

#' One Epoch Before an "HH:MM:SS" Segment Break
#'
#' @details \code{strptime} with no date: the arithmetic happens on today's date, which is
#'   harmless for the clock string it returns.
#'
#' @param x Character vector of "HH:MM:SS" breaks.
#' @param epochSize Epoch length in seconds.
#' @return Character vector of "HH:MM:SS", one epoch earlier.
#' @keywords internal
#' @noRd
.raw.timeuse.subtract.epoch <- function(x, epochSize) {
  hms <- strptime(x, format = "%H:%M:%S")
  hms <- hms - epochSize
  hms <- format(hms, format = "%H:%M:%S")
  return(hms)
}

#' Start and End Epoch of One Day Window, and Its qwindow Segments
#'
#' Everything is in epoch index space, 1..nrow(ts), and within one timewindow type the
#' windows tile the analysed part of the recording without gap or overlap. MM is midnight
#' to midnight, with \code{nightsi} re-extended on every call so that a first or last partial
#' day shorter than 25 hours becomes its own window. WW is waking to waking and OO is onset to
#' onset, both built from \code{diff(ts$diur)}: N wakes give N-1 windows and the tail after
#' the last wake is dropped as \code{c(NA, NA)}. The caller skips a window whose \code{qqq}
#' contains NA or whose length is 900 s or less.
#'
#' @details \code{indjump}, \code{qqq_backup} and \code{dayborder} are dropped: GGIR reads
#'   none of them. \code{segment_all_windows = FALSE} is GGIR 3.3.6 and the default: the WW/OO
#'   branch builds a single segment from the real clock edges of the window, names it after
#'   the timewindow, ends the loop with \code{wi >= Nwindows}, and the qwindow block runs for
#'   MM only with unprefixed segment names. TRUE is GGIR 3.3-9: the qwindow block runs for
#'   every timewindow, \code{start_end_window} on a WW row becomes the qwindow label, segment
#'   names gain the timewindow prefix, and a clock segment that wraps across the window start
#'   is split into two index ranges. 3.3-9's crash on a recording with exactly one wake (an NA
#'   test after \code{qqq <- c(NA, NA)}) is not reproduced; that case returns \code{c(NA, NA)}
#'   with \code{lastDay = TRUE}, as 3.3.6 does. An unrecognised timewindow raises; in GGIR the
#'   caller's while loop spins forever.
#'
#' @param nightsi Trimmed midnight grid for this timewindow.
#' @param wi Window number, 1-based, within this timewindow type.
#' @param epochSize Epoch length in seconds (GGIR's ws3new).
#' @param ts The part-5 time series. \code{ts$diur} and \code{ts$time} are read, and
#'   \code{ts$time} must already be POSIXct.
#' @param timewindowi One of "MM", "WW", "OO".
#' @param Nwindows Window count for this timewindow, from \code{.raw.timeuse.trim.nights}.
#' @param qwindow Numeric vector of clock hours, or an activity-log data frame with columns ID,
#'   date, qwindow_values and qwindow_names.
#' @param ID Recording ID, used only to match the activity log.
#' @param segment_all_windows FALSE for GGIR 3.3.6 semantics (the default), TRUE for GGIR
#'   3.3-9's.
#' @return list(qqq, lastDay, segments, segments_names).
#' @keywords internal
#' @noRd
.raw.timeuse.define.days <- function(nightsi, wi, epochSize, ts, timewindowi, Nwindows,
                                     qwindow = c(0, 24), ID = NULL,
                                     segment_all_windows = FALSE) {
  Nts <- nrow(ts)
  lastDay <- FALSE
  qqq <- rep(0, 2); segments <- segments_names <- c()
  if (timewindowi == "MM") {
    # include first and last partial days in MM
    if (nightsi[1] > 1 && nightsi[1] < 25 * 3600 / epochSize) {
      nightsi <- c(1, nightsi)
    }
    if (nightsi[length(nightsi)] < nrow(ts) &&
        nrow(ts) - nightsi[length(nightsi)] < 25 * 3600 / epochSize) {
      nightsi <- c(nightsi, nrow(ts))
    }
    qqq[1] <- nightsi[wi]
    if (length(nightsi) >= wi + 1) {
      qqq[2] <- nightsi[wi + 1] - 1
    } else {
      qqq[2] <- Nts
      lastDay <- TRUE
    }
    if (wi == length(nightsi) - 1) {
      lastDay <- TRUE
    }
    if (qqq[2] >= Nts - 1) {
      qqq[2] <- Nts
      lastDay <- TRUE
    }
  } else if (timewindowi == "WW" || timewindowi == "OO") {
    windowEdge <- ifelse(timewindowi == "WW", yes = -1, no = 1)
    if (wi <= (Nwindows - 1)) { # all full windows
      qqq[1] <- which(diff(ts$diur) == windowEdge)[wi] + 1
      qqq[2] <- which(diff(ts$diur) == windowEdge)[wi + 1]
    } else {
      # the tail after the last wake or onset is dropped; that night was not analysed
      qqq <- c(NA, NA)
    }
    if (segment_all_windows == FALSE) {
      # GGIR 3.3.6: the WW/OO window is one segment labelled with its real clock edges
      if (!is.na(qqq[1]) & !is.na(qqq[2])) {
        segments <- list(qqq)
        start <- format(ts$time[qqq[1]], "%H:%M:%S")
        end <- format(ts$time[qqq[2]], "%H:%M:%S")
        names(segments) <- paste(start, end, sep = "-")
        segments_names <- timewindowi
      }
      if (wi >= Nwindows) {
        lastDay <- TRUE
      }
    } else {
      # GGIR 3.3-9: the MM branch's two lastDay tests
      if (wi == length(which(diff(ts$diur) == windowEdge)) - 1) {
        lastDay <- TRUE
      }
      if (is.na(qqq[2])) {
        # 3.3-9 tests qqq[2] here with no NA guard and fails on a one-wake recording
        lastDay <- TRUE
      } else if (qqq[2] >= Nts - 1) {
        qqq[2] <- Nts
        lastDay <- TRUE
      }
    }
  } else {
    stop("timewindow must be one of MM, WW or OO, not ", timewindowi, call. = FALSE)
  }
  # qwindow segments; 3.3.6 runs this for MM only, 3.3-9 for every timewindow
  if (segment_all_windows == TRUE || timewindowi == "MM") {
    if (!is.na(qqq[1]) & !is.na(qqq[2])) {
      segments_timing <- NULL
      if (qqq[2] > Nts) qqq[2] <- Nts
      fullQqq <- qqq[1]:qqq[2]
      firstepoch <- format(ts$time[qqq[1]], "%H:%M:%S")
      lastepoch <- format(ts$time[qqq[2]], "%H:%M:%S")
      qnames <- NULL
      if (is.data.frame(qwindow)) {
        date_of_interest <- substr(ts$time[qqq[1]], 1, 10)
        qdate <- which(qwindow$ID == ID & qwindow$date == date_of_interest)
        if (length(qdate) == 1) { # if ID/date matched with activity log
          qnames <- unlist(qwindow$qwindow_names[qdate])
          qwindow <- unlist(qwindow$qwindow_values[qdate])
          qwindow_order <- order(qwindow)
          qwindow <- qwindow[qwindow_order]
          qnames <- qnames[qwindow_order]
        } else { # if ID/date not correctly matched with activity log
          qwindow <- c(0, 24)
        }
      } else {
        qwindow <- sort(qwindow)
        if (qwindow[1] != 0) qwindow <- c(0, qwindow)
        if (qwindow[length(qwindow)] != 24) qwindow <- c(qwindow, 24)
      }
      breaks <- .raw.timeuse.qwindow2timestamp(qwindow, epochSize)
      startOfSegments <- breaks[-length(breaks)]
      endOfSegments <- .raw.timeuse.subtract.epoch(breaks[-1], epochSize)
      if (length(startOfSegments) > 1) {
        # when qwindow segments are defined, add fullwindow at the beginning
        startOfSegments <- c(firstepoch, startOfSegments)
        endOfSegments <- c(lastepoch, endOfSegments)
      }
      segments_timing <- paste(startOfSegments, endOfSegments, sep = "-")
      # 3.3.6 writes bare names ("segment1") and hardcodes "MM"
      if (is.null(qnames)) {
        if (segment_all_windows == TRUE) {
          segments_names <- paste0(paste0(timewindowi, "segment"),
                                   0:(length(segments_timing) - 1))
          segments_names <- gsub(paste0(timewindowi, "segment0"), timewindowi, segments_names)
        } else {
          segments_names <- paste0("segment", 0:(length(segments_timing) - 1))
          segments_names <- gsub("segment0", timewindowi, segments_names)
        }
      } else {
        if (segment_all_windows == TRUE) {
          segments_names <- c(timewindowi, paste(paste0(timewindowi, "segment"),
                                                 qnames[-length(qnames)], qnames[-1], sep = "-"))
        } else {
          segments_names <- c(timewindowi, paste(qnames[-length(qnames)], qnames[-1], sep = "-"))
        }
      }
      # Get indices in ts for segments start and end limits
      hms <- format(ts$time[fullQqq], format = "%H:%M:%S")
      segments <- vector("list", length = length(segments_timing))
      names(segments) <- segments_timing
      for (si in 1:length(segments_timing)) {
        s0s1 <- unlist(strsplit(segments_timing[si], split = "[-]"))
        s0s1 <- format(s0s1, format = "%H:%M:%S")
        # tryCatch: a segment absent from ts gives a no non-missing values warning
        if (segment_all_windows == TRUE) {
          if (si == 1) {
            test_seg_condition <- NULL
            segments[[si]] <- range(fullQqq)
          } else {
            test_seg_condition <- which(hms >= s0s1[1] & hms <= s0s1[2])
            if (any(diff(test_seg_condition) > 1) &
                segments_names[si] %in% c("WW", "OO") == FALSE) {
              # the window is over 24 hours and the segment occurs twice; keep both ranges
              jump <- which(diff(test_seg_condition) > 1)
              test_seg_condition1 <- test_seg_condition[1:jump]
              test_seg_condition2 <- test_seg_condition[(jump + 1):length(test_seg_condition)]
              segments[[si]] <- c(tryCatch(range(fullQqq[test_seg_condition1]),
                                           warning = function(w) rep(NA, 2)),
                                  tryCatch(range(fullQqq[test_seg_condition2]),
                                           warning = function(w) rep(NA, 2)))
            } else {
              segments[[si]] <- tryCatch(range(fullQqq[test_seg_condition]),
                                         warning = function(w) rep(NA, 2))
            }
          }
        } else {
          segments[[si]] <- tryCatch(range(fullQqq[which(hms >= s0s1[1] & hms <= s0s1[2])]),
                                     warning = function(w) rep(NA, 2))
        }
      }
    }
  }
  return(invisible(list(qqq = qqq, lastDay = lastDay,
                        segments = segments, segments_names = segments_names)))
}

# SLEEP PERIOD TIME

#' Diurnal 0/1 Classification of the Time Series From the Part-4 Night Summary
#'
#' Rebuilds the sleep period time epoch by epoch: the night's \code{calendar_date},
#' \code{sleeponset_ts}, \code{wakeup_ts}, \code{sleeponset}, \code{wakeup} and
#' \code{daysleeper} are turned back into two timestamps, matched against the time series by
#' string equality, then \code{ts$diur[s0:(s1 - 1)] <- 1}. The wake epoch itself is not sleep
#' period time, which is why part 5's \code{dur_spt_sleep_min} differs from part 4's
#' \code{SleepDurationInSpt * 60} by one epoch.
#'
#' @details GGIR's is.ISO8601 is nested as a local. Reproduced: the defaults when part 4 left
#'   NA (onset 22.0, wake 31.0); the two NA rescues, which turn s0 into 1 only when the part-4
#'   onset is before the first timestamp and s1 into nrow(ts) only when the wake is after the
#'   last; the operator precedence in the sleep-log guard, where \code{&} binds tighter than
#'   \code{|}, so the second arm fires with no sleep log at all and the zero-length lookups
#'   leave s0 and s1 untouched; and the sleep-log hour folding. \code{nightsi} must be the
#'   true midnights (GGIR's nightsi2), because the sleep-log fallback is relative to real
#'   midnight.
#'
#' @param ts The part-5 time series. \code{ts$time} must still be the ISO8601 character
#'   column, so that an autumn DST repeated hour does not collide. \code{ts$diur} must
#'   already exist, zeroed, and \code{ts$nonwear} is read by the sleep-log rescue.
#' @param part4_output The part-4 night summary rows for one sib definition.
#' @param desiredtz Timezone of the timestamps.
#' @param nightsi The TRUE midnight indices.
#' @param sleeplog The sleep diary frame, or \code{c()} when there is none.
#' @param epochSize Epoch length in seconds.
#' @param ID Recording ID, used to select the diary rows.
#' @param Nepochsinhour Number of epochs in one hour.
#' @return \code{ts} with \code{ts$diur} filled.
#' @keywords internal
#' @noRd
.raw.timeuse.wakesleep <- function(ts, part4_output, desiredtz, nightsi,
                                   sleeplog, epochSize, ID,
                                   Nepochsinhour) {
  # 1 from onset to wake, 0 from wake to onset
  Nts <- nrow(ts)
  findIndex <- function(timeChar = NULL, wc = NULL) {
    wc_withzerotime <- paste0(wc, " 00:00:00")
    if (length(grep(pattern = " ", x = wc)) == 0) {
      wc <- wc_withzerotime
    }
    index <- which(timeChar == wc)[1]
    if (is.na(index) == TRUE) {
      index <- which(timeChar == wc_withzerotime)[1]
    }
    return(index)
  }
  clock2numtime <- function(x) {
    x2 <- as.numeric(unlist(strsplit(x, ":"))) / c(1, 60, 3600)
    return(sum(x2))
  }
  is.ISO8601 <- function(x) { # GGIR's is.ISO8601
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
  w0 <- w1 <- rep(0, length(part4_output$calendar_date))
  # Round seconds to integer number of epoch lengths (needed if cleaningcode = 5).
  round_seconds_to_epochSize <- function(x, epochSize) {
    if (length(as.numeric(unlist(strsplit(x, ":")))) == 3) {
      xPOSIX <- as.POSIXct(x, format = "%H:%M:%S", origin = "1970-01-01")
      xPOSIX_rounded <- as.POSIXct(round(as.numeric(xPOSIX) / epochSize) * epochSize,
                                   origin = "1970-01-01")
      x <- format(xPOSIX_rounded, format = "%H:%M:%S")
    } else {
      x <- ""
    }
    return(x)
  }
  # time series as plain character, not ISO8601
  timeChar <- format(ts$time)
  if (is.ISO8601(timeChar[1]) == TRUE) {
    timeChar <- format(.raw.iso8601.to.posix(timeChar, tz = desiredtz))
  }

  for (k in 1:length(part4_output$calendar_date)) {
    part4_output$wakeup_ts[k] <- round_seconds_to_epochSize(part4_output$wakeup_ts[k], epochSize)
    part4_output$sleeponset_ts[k] <- round_seconds_to_epochSize(part4_output$sleeponset_ts[k],
                                                                epochSize)

    part4_output$sleeponset[k] <- (round((part4_output$sleeponset[k] * 3600) / epochSize) *
                                     epochSize) / 3600
    part4_output$wakeup[k] <- (round((part4_output$wakeup[k] * 3600) / epochSize) *
                                 epochSize) / 3600

    # Load sleep onset and waking time from part 4 and convert them into timestamps
    tt <- unlist(strsplit(as.character(part4_output$calendar_date[k]), "/"))
    # defaults when part 4 has no onset or wake; these days are discarded later anyway
    if (is.na(part4_output$sleeponset[k]) == TRUE) {
      defSO <- 22
      defSO_ts <- "22:00:00"
    } else {
      defSO <- part4_output$sleeponset[k]
      defSO_ts <- part4_output$sleeponset_ts[k]
    }
    if (is.na(part4_output$wakeup[k]) == TRUE) {
      defWA <- 31
      defWA_ts <- "07:00:00"
    } else {
      defWA <- part4_output$wakeup[k]
      defWA_ts <- part4_output$wakeup_ts[k]
    }
    w0[k] <- paste(tt[3], "-", tt[2], "-", tt[1], " ", as.character(defSO_ts), sep = "")
    w1[k] <- paste(tt[3], "-", tt[2], "-", tt[1], " ", as.character(defWA_ts), sep = "")
    # if time is beyond 24 then change the date
    if (defSO >= 24) {
      tmp_w0 <- as.POSIXlt(w0[k], tz = desiredtz, origin = "1970-01-01")
      w0[k] <- as.character(tmp_w0 + (24 * 3600))
    }
    if (defWA >= 24 |
        (part4_output$daysleeper[k] == 1 & defWA < 18)) {
      w1[k] <- format(as.POSIXlt(w1[k], tz = desiredtz) + (24 * 3600))
    }
    s0 <- findIndex(timeChar, wc = format(as.POSIXlt(w0[k], tz = desiredtz)))
    s1 <- findIndex(timeChar, wc = format(as.POSIXlt(w1[k], tz = desiredtz)))

    if (is.na(s0) == TRUE) {
      if (format(as.POSIXlt(w0[k], tz = desiredtz)) < timeChar[1]) {
        # only when the part-4 onset is before the first timestamp, so a cleaningcode 5
        # night cannot flood the series
        s0 <- 1
      }
    }
    if (is.na(s1) == TRUE) {
      if (format(as.POSIXlt(w1[k], tz = desiredtz)) > timeChar[length(timeChar)]) {
        # only when the part-4 wake is after the last timestamp
        s1 <- nrow(ts)
      }
    }

    if (length(s1) != 0 & length(s0) != 0 & is.na(s0) == FALSE & is.na(s1) == FALSE &&
        length(nightsi) > 0) {
      distance2midnight <- abs(nightsi - s1) + abs(nightsi - s0)
      closestmidnighti <- which.min(distance2midnight)
      closestmidnight <- nightsi[closestmidnighti]
      noon0 <- closestmidnight - (12 * (60 / epochSize) * 60)
      noon1 <- closestmidnight + (12 * (60 / epochSize) * 60)
      if (noon0 < 1) noon0 <- 1
      if (noon1 > Nts) noon1 <- Nts
      nonwearpercentage <- mean(ts$nonwear[noon0:noon1])
      if ((length(sleeplog) > 0 & (nonwearpercentage > 0.33) |
           part4_output$sleeponset_ts[k] == "")) {
        # If non-wear is high for this day and if sleeplog is available
        sleeplogonset <- sleeplog$sleeponset[which(sleeplog$ID == ID &
                                                     sleeplog$night == part4_output$night[k])]
        sleeplogwake <- sleeplog$sleepwake[which(sleeplog$ID == ID &
                                                   sleeplog$night == part4_output$night[k])]
        if (length(sleeplogonset) != 0 & length(sleeplogwake) != 0) {
          if (!is.na(sleeplogonset) & !is.na(sleeplogwake)) {
            # rely on the sleeplog for the start and end of the night
            sleeplogonset_hr <- clock2numtime(sleeplogonset)
            sleeplogwake_hr <- clock2numtime(sleeplogwake)
            # express hour relative to midnight within the noon-noon:
            if (sleeplogonset_hr > 12) {
              sleeplogonset_hr <- sleeplogonset_hr - 24
            }
            if (sleeplogwake_hr > 18 & part4_output$daysleeper[k] == 1) {
              sleeplogwake_hr <- sleeplogwake_hr - 24 # 18 because daysleepers can wake after 12
            } else if (sleeplogwake_hr > 12 & part4_output$daysleeper[k] == 0) {
              sleeplogwake_hr <- sleeplogwake_hr - 24
            }
            if (sleeplogwake_hr > 36 & sleeplogonset_hr > 36) {
              sleeplogwake_hr <- sleeplogwake_hr - 24
              sleeplogonset_hr <- sleeplogonset_hr - 24
            }
            s0 <- closestmidnight + round(sleeplogonset_hr * Nepochsinhour)
            if (s0 < 1) {
              warning("Impossible index for first night, consider setting excludefirst.part4=TRUE")
              s0 <- 1
            }
            s1 <- closestmidnight + round(sleeplogwake_hr * Nepochsinhour)
            # a sleeplog wake after the recording end sets SPT to the end of the recording
            if (s1 > nrow(ts) + 1) s1 <- nrow(ts) + 1
          }
        }
      }
      # s1 < s0 when both diary times lie beyond the recording
      if (s0 < s1) {
        ts$diur[s0:(s1 - 1)] <- 1
      }
    }
  }
  return(ts)
}

# FIRST WAKE

#' Impute the Waking Up That Closes a Missing First Night
#'
#' When part 4 dropped night 1, typically through \code{excludefirst.part4}, the diurnal
#' vector starts awake and the first wake-to-wake window would be wrong. The imputed wake
#' snaps back to the end of the last sustained inactivity bout before the estimate. If the
#' estimate is unusable, five minutes of waking time are fabricated before the first onset
#' and the lead-in is marked non-wear, so day numbering stays aligned.
#'
#' @details Reproduced: the operator precedence of the gate, where \code{&&} binds tighter
#'   than \code{||}; the NA strip, which promotes night 2's SPTE estimate to night 1 and
#'   errors on an empty \code{SPTE_end} (\code{if (logical(0))}); and the dead first
#'   computation of \code{wake_night1_index}. The guider planted for the SPTE route is the
#'   literal "part3_estimate", which the caller renames to \code{rev(HASPT.algo)[1]} at the
#'   top of every timewindow iteration.
#'
#' @param ts The part-5 time series, with \code{diur} from \code{.raw.timeuse.wakesleep},
#'   \code{sibdetection} from addsib, and \code{guider} and \code{nonwear} present.
#' @param summarysleep The part-4 night summary rows for one sib definition.
#' @param nightsi The midnight grid, dayborder-shifted (GGIR's nightsi, not nightsi2).
#' @param sleeplog The sleep diary frame, or \code{c()} when there is none.
#' @param ID Recording ID, used to select the diary rows.
#' @param Nepochsinhour Number of epochs in one hour.
#' @param SPTE_end Part 3's sleep period time end estimates, in decimal hours.
#' @return \code{ts}, possibly with \code{diur}, \code{guider} and \code{nonwear} changed.
#' @keywords internal
#' @noRd
.raw.timeuse.addfirstwake <- function(ts, summarysleep, nightsi, sleeplog, ID,
                                      Nepochsinhour, SPTE_end) {
  # diur lacks the first night here; nightsi has every midnight, so a missing first wake
  # can be detected and imputed
  clock2numtime <- function(x) {
    x2 <- as.numeric(unlist(strsplit(x, ":"))) / c(1, 60, 3600)
    return(sum(x2))
  }
  firstwake <- which(diff(ts$diur) == -1)[1]
  firstonset <- which(diff(ts$diur) == 1)[1]
  Nts <- nrow(ts)
  epochSize <- round(3600 / Nepochsinhour)
  if (is.na(SPTE_end[1]) == TRUE) {
    SPTE_end <- SPTE_end[which(is.na(SPTE_end) == FALSE)]
  }
  # is the wake for the second day missing?
  if (length(nightsi) < 2) {
    return(ts)
  }
  guider <- "unknown"
  if (!is.na(firstwake) && firstwake > nightsi[2] ||
      (summarysleep$sleeponset[1] < 18 &&
       summarysleep$wakeup[1] < 18 &&
       firstwake < nightsi[2])) {
    wake_night1_index <- c()
    if (length(sleeplog) > 0) {
      # use sleeplog for waking up after first night
      wake_night1 <- sleeplog[which(sleeplog$ID == ID & sleeplog$night == 1),
                              grep(pattern = "bedend|wake", x = colnames(sleeplog))]
      onset_night1 <- sleeplog[which(sleeplog$ID == ID & sleeplog$night == 1),
                               grep(pattern = "bedstart|sleeponset", x = colnames(sleeplog))]
      if (length(wake_night1) != 0 & length(onset_night1) != 0) {
        if (wake_night1 != "" & onset_night1 != "") {
          # express hour relative to midnight within the noon-noon:
          wake_night1_hour <- clock2numtime(wake_night1)
          onset_night1_hour <- clock2numtime(onset_night1)
          # a wake in the afternoon with an onset around noon is not a day sleeper
          if (wake_night1_hour > 12 & (onset_night1_hour >= 12 & onset_night1_hour < 18)) {
            wake_night1_hour <- wake_night1_hour - 24
          }
          wake_night1_index <- nightsi[1] + round(wake_night1_hour * Nepochsinhour)
          if (wake_night1_index > Nts) wake_night1_index <- Nts
          if (wake_night1_hour > 18) wake_night1_hour <- wake_night1_hour - 24
          if (wake_night1_index < 1) wake_night1_index <- 1
          wake_night1_index <- nightsi[1] + round(wake_night1_hour * Nepochsinhour)
          if (wake_night1_index > Nts) wake_night1_index <- Nts
          if (wake_night1_index < 1) wake_night1_index <- 1
          guider <- "sleeplog"
        } else { # use SPTE algorithm as plan B
          wake_night1_index <- nightsi[1] + round((SPTE_end[1] - 24) * Nepochsinhour)
          guider <- "part3_estimate"
        }
      } else { # use SPTE algorithm as plan B
        wake_night1_index <- nightsi[1] + round((SPTE_end[1] - 24) * Nepochsinhour)
        guider <- "part3_estimate"
      }
    } else if (length(SPTE_end) > 0 & length(sleeplog) == 0) {
      # use the SPTE estimate when there is no sleep log
      if (is.na(SPTE_end[1]) == FALSE) {
        if (SPTE_end[1] != 0) {
          wake_night1_index <- nightsi[1] + round((SPTE_end[1] - 24) * Nepochsinhour)
          guider <- "part3_estimate"
        }
      }
    }
    if (length(wake_night1_index) == 0) {
      # last option: the next day's wake minus 24 hours
      wake_night1_index <- (firstwake - (24 * ((60 / epochSize) * 60))) + 1
    }
    if (is.na(wake_night1_index)) wake_night1_index <- 0
    if (wake_night1_index < firstwake & wake_night1_index > 1 &
        (wake_night1_index - 1) > nightsi[1]) {
      newWakeIndex <- c()
      firstSIBs <- which(ts$sibdetection[1:(wake_night1_index - 1)] == 1)
      if (length(firstSIBs) > 0) newWakeIndex <- max(firstSIBs)
      if (length(newWakeIndex) == 0) {
        newWakeIndex <- wake_night1_index - 1
      }
      ts$diur[1:newWakeIndex] <- 1
      ts$guider[1:newWakeIndex] <- guider
    } else {
      # no sleep data for the first night: add 5 minutes of dummy waking time before the
      # first onset, labelled non-wear, so day numbering stays consistent
      if (!is.na(firstonset)) {
        dummywake <- max(firstonset - round(Nepochsinhour / 12),
                         nightsi[1] + round(Nepochsinhour * 6))
        ts$diur[1:dummywake] <- 1
        ts$nonwear[1:firstonset] <- 1
        ts$guider[1:dummywake] <- guider
      }
    }
  }
  return(ts)
}

# MISSING NIGHTS

#' Splice a Placeholder Row Into the Night Summary for Every Night Part 4 Skipped
#'
#' Part 4 stores nothing for a day when the accelerometer was not worn at all and
#' \code{ignorenonwear} is TRUE. Every integer in \code{min(night):max(night)} that is absent
#' gets a row cloned from row 1 with everything blanked except ID, night, sleepparam,
#' filename, filename_dir and foldername.
#'
#' @details Three GGIR defects are reproduced: \code{summarysleep$calendar_date[mi - 1]}
#'   indexes by the missing-night position, not the row position; \code{is.na(sleeplogonset)}
#'   is tested twice where the second should be \code{sleeplogwake}; and \code{sleeplog_used}
#'   and \code{guider} are set only inside \code{if (sleeplogwake_hr > 36)}. The
#'   \code{+ 36 * 3600} advances the previous night's date by one day clear of a DST boundary,
#'   and the split-and-repaste on "/" strips the leading zeros.
#'
#' @param summarysleep The part-4 night summary rows for one sib definition.
#' @param sleeplog The sleep diary frame, or \code{c()} when there is none.
#' @param ID Recording ID, used to select the diary rows.
#' @return \code{summarysleep} with one row spliced in per missing night.
#' @keywords internal
#' @noRd
.raw.timeuse.fixmissingnight <- function(summarysleep, sleeplog = c(), ID) {
  clock2numtime <- function(x) {
    x2 <- as.numeric(unlist(strsplit(x, ":"))) / c(1, 60, 3600)
    return(sum(x2))
  }
  hr_to_clocktime <- function(x) {
    hrsNEW <- floor(x)
    minsUnrounded <- (x - hrsNEW) * 60
    minsNEW <- floor(minsUnrounded)
    secsNEW <- floor((minsUnrounded - minsNEW) * 60)
    if (minsNEW < 10) minsNEW <- paste0(0, minsNEW)
    if (secsNEW < 10) secsNEW <- paste0(0, secsNEW)
    if (hrsNEW < 10) hrsNEW <- paste0(0, hrsNEW)
    time <- paste0(hrsNEW, ":", minsNEW, ":", secsNEW)
    return(time)
  }
  potentialnight <- min(summarysleep$night):max(summarysleep$night)
  missingnight <- which(as.numeric(potentialnight) %in% as.numeric(summarysleep$night) == FALSE)

  if ("guider_wakeup" %in% colnames(summarysleep) == TRUE) {
    guider_onset <- "guider_onset"
    guider_wakeup <- "guider_wakeup"
  } else {
    guider_onset <- "guider_inbedStart"
    guider_wakeup <- "guider_inbedEnd"
  }
  if (length(missingnight) > 0) {
    for (mi in missingnight) {
      missingNight <- potentialnight[mi]
      newnight <- summarysleep[1, ]
      newnight[which(names(newnight) %in% c("ID", "night", "sleepparam", "filename",
                                            "filename_dir", "foldername") == FALSE)] <- NA
      newnight$wakeup <- newnight[, guider_wakeup] <- newnight$sleeponset <-
        newnight[, guider_onset] <- NA
      newnight$night <- missingNight
      newnight$calendar_date <- format(as.Date(as.POSIXlt(summarysleep$calendar_date[mi - 1],
                                                          format = "%d/%m/%Y") + (36 * 3600)),
                                       "%d/%m/%Y")
      # remove leading zeros
      timesplit <- as.numeric(unlist(strsplit(as.character(newnight$calendar_date), "/")))
      newnight$calendar_date <- paste0(timesplit[1], "/", timesplit[2], "/", timesplit[3])
      newnight$daysleeper <- 0
      newnight$acc_available <- 0
      if (length(sleeplog) > 0) {
        # impute from the sleeplog
        sleeplogonset <- sleeplog$sleeponset[which(sleeplog$ID == ID &
                                                     sleeplog$night == missingNight)]
        sleeplogwake <- sleeplog$sleepwake[which(sleeplog$ID == ID &
                                                   sleeplog$night == missingNight)]
        newnight$sleeplog_used <- 0
        newnight$guider <- "nosleeplog_accnotworn"
        if (length(sleeplogonset) != 0 & length(sleeplogwake) != 0) {
          if (is.na(sleeplogonset) == FALSE & is.na(sleeplogonset) == FALSE) {
            sleeplogonset_hr <- clock2numtime(sleeplogonset)
            sleeplogwake_hr <- clock2numtime(sleeplogwake)
            newnight$sleeponset <- newnight[, guider_onset] <- sleeplogonset_hr
            newnight$wakeup <- newnight[, guider_wakeup] <- sleeplogwake_hr
            if (sleeplogwake_hr > 36) {
              newnight$daysleeper <- 1
              newnight$sleeponset_ts <- hr_to_clocktime(sleeplogonset_hr)
              newnight$wakeup_ts <- hr_to_clocktime(sleeplogwake_hr)
              newnight$sleeplog_used <- 1
              newnight$guider <- "sleeplog"
            }
          }
        }
      } else {
        newnight$sleeplog_used <- 0
        newnight$guider <- "nosleeplog_accnotworn"
      }
      newnight$cleaningcode <- 5
      summarysleep <- rbind(summarysleep[1:(mi - 1), ],
                            newnight,
                            summarysleep[mi:nrow(summarysleep), ])
    }
  }
  return(summarysleep)
}

# ONSET AND WAKE TIMING

#' Sleep Onset and Waking Time of One Window, in Decimal Hours
#'
#' Decimal hours since the midnight that opens the window's calendar date, the same axis
#' parts 3 and 4 use: 12 is noon, 24 the following midnight, and a day sleeper's wake can
#' exceed 36. The paired \code{_ts} columns the caller writes are the raw local clock time of
#' the same epoch. For WW the wake is the window boundary by construction and for OO the
#' onset is; for MM both are searched inside the window and either can be missing, in which
#' case \code{skipwake} or \code{skiponset} stays TRUE.
#'
#' @details Reproduced: the asymmetric \code{+ 1}, outside the subscript for the onset index
#'   and inside it for the wake index; MM takes the last onset in the window and the first
#'   wake; for OO \code{onseti} is \code{qqq[1] + 1}, two epochs after part 4's onset index;
#'   and the order of the 24 hour corrections, in which \code{wake} is tested first and is 0
#'   when skipped, so the \code{wake <= 12} arm fires and an onset at or below noon is pushed
#'   by 24. The 3.3-9 clamp on \code{wakei} is kept.
#'
#' @param qqq Window start and end epoch indices.
#' @param ts The part-5 time series; only \code{ts$diur} is read.
#' @param min,sec,hour The whole-series clock vectors from \code{.raw.timeuse.midnights}.
#' @param timewindowi One of "MM", "WW", "OO".
#' @return list(wake, onset, wakei, onseti, skiponset, skipwake). \code{wake} and \code{onset}
#'   are decimal hours with the 24 hour corrections applied, and are 0 when skipped.
#' @keywords internal
#' @noRd
.raw.timeuse.onsetwake <- function(qqq, ts, min, sec, hour, timewindowi) {
  onset <- wake <- 0
  skiponset <- TRUE; skipwake <- TRUE
  # Onset index
  if (timewindowi == "OO") {
    onseti <- qqq[1] + 1
  } else {
    onseti <- c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2] - 1)]) == 1)] + 1
    if (length(onseti) > 1) {
      onseti <- onseti[length(onseti)] # in the case if MM use last onset
    }
  }
  # Wake index
  if (timewindowi == "WW") {
    wakei <- qqq[2] + 1
    if (wakei > length(hour)) wakei <- length(hour)
  } else {
    wakei <- c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2] - 1)]) == -1) + 1]
    if (length(wakei) > 1) wakei <- wakei[1] # in the case if MM use first wake-up time
  }
  # Onset time
  if (length(onseti) == 1) { # in MM window it is possible to not have an onset
    if (is.na(onseti) == FALSE) {
      onset <- hour[onseti] + (min[onseti] / 60) + (sec[onseti] / 3600)
      skiponset <- FALSE
    }
  }
  # Wake time
  if (length(wakei) == 1) { # in MM window it is possible to not have a wake
    if (is.na(wakei) == FALSE) {
      wake <- hour[wakei] + (min[wakei] / 60) + (sec[wakei] / 3600)
      skipwake <- FALSE
    }
  }
  if (wake >= 12 & wake <= 18) { # daysleeper and onset in the morning or afternoon
    if (onset <= 18 & skiponset == FALSE) onset <- onset + 24
    if (wake <= 18 & skipwake == FALSE) {
      wake <- wake + 24
    }
  } else if (wake <= 12) { # no daysleeper, but onset before noon
    if (wake <= 12 & skipwake == FALSE) wake <- wake + 24
    if (onset <= 12 & skiponset == FALSE) onset <- onset + 24
  }
  if (wake >= 12 & onset <= 12 & skiponset == FALSE) onset <- onset + 24
  if (wake > 36 & onset > 36) {
    # both on the next afternoon is not possible, so reverse the overcorrection
    onset <- onset - 24
    wake <- wake - 24
  }
  return(invisible(
    list(
      wake = wake,
      onset = onset,
      wakei = wakei,
      onseti = onseti,
      skiponset = skiponset,
      skipwake = skipwake
    )
  ))
}
