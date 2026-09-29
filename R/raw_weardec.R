# Ported from GGIR 3.3-9 R/g.weardec.R, R/filterNonwearNight.R, R/g.detecmidnight.R, the
# non-wear decision and protocol mask of R/g.impute.R, the day slicing of
# R/g.analyse.perday.R and the recording summary of R/g.analyse.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the cleaning parameters are
# explicit arguments; the study_dates_file and TimeSegments2Zero paths of g.impute are
# not ported, nor its average-day imputation or the activity summaries of g.analyse;
# raw.wear.decision assembles the pieces into one classed object. Thresholds, index
# construction, operand order and comparisons are GGIR's.

# NIGHT FILTER

#' Ignore Short Non-Wear Episodes Inside a Night Window
#'
#' GGIR's filterNonwearNight: a non-wear episode shorter than
#' \code{nonwearFiltermaxHours} whose first long epoch lies in the night window is
#' relabelled wear before the wear decision. The window comes from
#' \code{nonwearFilterWindow} (two clock hours, wrapping past midnight when the first is
#' larger) or from a qwindow diary; with both absent GGIR's stop text is raised. The
#' diary branch is ported but canhrActi carries no diary.
#'
#' @param r1 The non-wear vector (0/1, one per long epoch).
#' @param metalong GGIR's metalong table (a timestamp column is required).
#' @param qwindowImp A qwindow diary data.frame, a numeric window, or NULL.
#' @param desiredtz Timezone of the timestamps.
#' @param nonwearFiltermaxHours,nonwearFilterWindow GGIR's params_cleaning members.
#' @param ws2 Long epoch length in seconds.
#' @return list(r1, nonwearHoursFiltered, nonwearEventsFiltered).
#' @keywords internal
#' @noRd
.raw.filter.nonwear.night <- function(r1, metalong, qwindowImp = NULL, desiredtz = "",
                                      nonwearFiltermaxHours = NULL, nonwearFilterWindow = NULL,
                                      ws2 = 900) {
  nonwearEventsFiltered <- nonwearHoursFiltered <- 0
  if (!is.null(nonwearFilterWindow)) {
    filter_method <- 1 # fixed window
  } else {
    if (is.null(qwindowImp)) {
      stop(paste0("Please specify parameter nonwearFilterWindow or ",
                  "qwindow as diary with columns to define the window for ",
                  "filtering short nonwear. See documentation for ",
                  "parameter nonwearFiltermaxHours"), call. = FALSE)
    }
    if (inherits(qwindowImp, "data.frame")) {
      filter_method <- 2 # diary
    } else {
      filter_method <- 1
      nonwearFilterWindow <- qwindowImp
    }
  }
  metalong$time_POSIX <- .raw.iso8601.to.posix(metalong$timestamp, tz = desiredtz)
  metalong$hour <- as.numeric(format(metalong$time_POSIX, "%H")) + as.numeric(format(metalong$time_POSIX, "%M")) / 60
  x <- rle(as.numeric(r1))
  if (length(x$values) != 1) {
    r1B <- as.data.frame(lapply(x, rep, times =  x$lengths))
    r1B$hr <- metalong$hour
    r1B$filter_method <- filter_method
    r1B$lengths <- (r1B$lengths * ws2) / 3600 # epochs to hours
    r1B$filterWindow <- 0
    # night window
    if (filter_method == 1) {
      if (nonwearFilterWindow[1] > nonwearFilterWindow[2]) {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] |
                                 r1B$hr < nonwearFilterWindow[2])] <- 1
      } else {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] &
                                 r1B$hr < nonwearFilterWindow[2])] <- 1
      }
    } else if (filter_method == 2) {
      # window per date from the diary
      r1B$date <- as.Date(metalong$time_POSIX)
      for (qi in 1:nrow(qwindowImp)) {
        qwindow_temp <- qwindowImp$qwindow_values[[qi]]
        # both a start and an end must be reported
        available <- c(length(grep(pattern = "inbed|sleeponset|lightsout", x = qwindowImp$qwindow_names[[qi]])) > 0,
                       length(grep(pattern = "outbed|wakeup|lightsoff", x = qwindowImp$qwindow_names[[qi]])) > 0)
        qwindow_temp <- qwindow_temp[grep(pattern = "bed|wakeup|sleeponset|lights", x = qwindowImp$qwindow_names[[qi]])]
        qwindow_temp <- sort(qwindow_temp)
        isDefaultWindow <- length(qwindow_temp) == 2 && qwindow_temp[1] == 0 && qwindow_temp[2] == 24
        if (length(qwindow_temp) > 1 && isDefaultWindow == FALSE && all(available)) {
          # hours before 18:00 belong to the next day
          below18 <- which(qwindow_temp < 18)
          if (length(below18) > 0) {
            qwindow_temp[below18] <- qwindow_temp[below18] + 24
          }
          start <- min(qwindow_temp)
          end <- max(qwindow_temp)
          if (length(below18) > 0) {
            start <- ifelse(start >= 24, yes = start - 24, no = start)
            end <- ifelse(end >= 24, yes = end - 24, no = end)
          }
          if (start > end) {
            r1B$filterWindow[which(r1B$date == qwindowImp$date[qi] &
                                     r1B$hr >= start |
                                     r1B$hr < end)] <- 1
          } else {
            r1B$filterWindow[which(r1B$date == qwindowImp$date[qi] &
                                     r1B$hr >= start &
                                     r1B$hr < end)] <- 1
          }
        } else {
          # no usable report: midnight to 06:00
          r1B$filter_method[which(r1B$hr >= 0 &
                                    r1B$hr < 6)] <- 4
          r1B$filterWindow[which(r1B$hr >= 0 &
                                   r1B$hr < 6)] <- 1
        }
      }
    }
    short_nonwear_night <- which(r1B$values == 1 &
                                   r1B$lengths < nonwearFiltermaxHours &
                                   r1B$filterWindow == 1)

    if (length(short_nonwear_night) > 0) {
      nonwearHoursFiltered <- (length(which(diff(short_nonwear_night) == 1)) * ws2) / 3600
      nonwearEventsFiltered <- length(which(diff(short_nonwear_night) != 1)) + 1
      r1[short_nonwear_night] <- 0
    }
  }
  invisible(list(r1 = r1, nonwearHoursFiltered = nonwearHoursFiltered,
                 nonwearEventsFiltered = nonwearEventsFiltered))
}

# WEAR DECISION

#' Non-Wear, Clipping and Additional Non-Wear per Long Epoch
#'
#' GGIR's g.weardec: r1 flags the long epochs whose nonwearscore reaches
#' \code{wearthreshold}, r2 those whose clipping fraction exceeds 0.3, and r3 relabels
#' wear islands GGIR judges implausible: shorter than 6 h and under 0.3 of the
#' surrounding non-wear, shorter than 3 h and under 0.8 of it, or (inside the final 24 h)
#' shorter than 3 h after more than 1 h of non-wear. With \code{nonWearEdgeCorrection} a
#' non-wear episode starting within the first 3 h is extended to the start and one
#' ending within the last 3 h to the end. The island rules run twice more on the union
#' of r1 and r3.
#'
#' @details One GGIR quirk is kept under \code{ggir_exact = TRUE}: the nested function
#'   tests the enclosing \code{nonWearEdgeCorrection} instead of its own
#'   \code{EdgeCorrection} argument, so the two extra passes still apply the edge
#'   correction whenever the parameter is TRUE. Under \code{ggir_exact = FALSE} the
#'   argument is honoured. No recording was found where the two differ.
#'
#' @param metalong GGIR's metalong data.frame (columns timestamp, nonwearscore,
#'   clippingscore, ...).
#' @param wearthreshold Number of still axes that make an epoch non-wear (GGIR: 2).
#' @param ws2 Long epoch length in seconds.
#' @param nonWearEdgeCorrection GGIR's params_cleaning member (default TRUE).
#' @param nonwearFiltermaxHours,nonwearFilterWindow GGIR's night filter (default off).
#' @param desiredtz Timezone of the timestamps (night filter only).
#' @param qwindowImp GGIR's diary object for the night filter; NULL in canhrActi.
#' @param ggir_exact TRUE keeps the edge-correction quirk described in Details.
#' @return list(r1, r2, r3, LC, LC2, nonwearHoursFiltered, nonwearEventsFiltered),
#'   GGIR's return list; r1 to r3 have one value per long epoch.
#' @keywords internal
#' @noRd
.raw.weardec <- function(metalong, wearthreshold = 2, ws2 = 900,
                         nonWearEdgeCorrection = TRUE,
                         nonwearFiltermaxHours = NULL, nonwearFilterWindow = NULL,
                         desiredtz = "", qwindowImp = c(), ggir_exact = TRUE) {
  nsi <- which(colnames(metalong) == "nonwearscore")
  csi <- which(colnames(metalong) == "clippingscore")
  NLongEpochs <- nrow(metalong)
  clipsig <- as.numeric(as.matrix(metalong[,csi]))
  # clipsig: fraction of clipped samples per long epoch
  LC <- length(which(clipsig > 0.05))
  turnoffclip <- which(clipsig > 0.3)
  LC2 <- length(turnoffclip)
  turnoffnonw <- which(as.numeric(as.matrix(metalong[,nsi])) >= wearthreshold)
  r1 <- r2 <- r3 <- matrix(0, nrow(metalong), 1)
  r1[turnoffnonw] <- 1 # non-wear
  r2[turnoffclip] <- 1 # clipping

  # optional night filter
  if (!is.null(nonwearFiltermaxHours)) {
    fnn <- .raw.filter.nonwear.night(r1, metalong, qwindowImp, desiredtz,
                                     nonwearFiltermaxHours = nonwearFiltermaxHours,
                                     nonwearFilterWindow = nonwearFilterWindow, ws2 = ws2)
    r1[, 1] <- fnn$r1
    nonwearHoursFiltered <- fnn$nonwearHoursFiltered
    nonwearEventsFiltered <- fnn$nonwearEventsFiltered
  } else {
    nonwearEventsFiltered <- nonwearHoursFiltered <- 0
  }
  r1 <- c(0, r1, 0)
  r2 <- c(0, r2, 0)
  r3 <- c(0, r3, 0)
  additionalNonWearDetection <- function(s1, s3, ws2, EdgeCorrection = FALSE) {
    ch1 <- which(diff(s1) == 1) + 1 # starts of wear
    ch2 <- which(diff(s1) == -1) + 1 # starts of non-wear
    if (length(ch1) > 1) { # at least two non-wear periods
      wear <- matrix(0, (length(ch1) - 1), 3) # wear islands between non-wear periods
      for (weari in 1:(length(ch1) - 1)) {
        wear[weari,1] <- abs(ch1[weari + 1] - ch2[weari]) # length of wear
        wear[weari,2] <- abs(ch2[weari + 1] - ch1[weari + 1]) # non-wear after it
        wear[weari,3] <- abs(ch2[weari] - ch1[weari]) # non-wear before it
        if (wear[weari, 1] < (6 / (ws2 / 3600)) &
            (wear[weari, 1] / (wear[weari, 2] + wear[weari, 3])) < 0.3) {
          # under 6 h and under 0.3 of the surrounding non-wear
          s3[ch2[weari]:(ch1[weari + 1] - 1)] <- 1
        }
        if (wear[weari, 1] < (3 / (ws2 / 3600)) &
            (wear[weari, 1] / (wear[weari, 2] + wear[weari, 3])) < 0.8) {
          # under 3 h and under 0.8 of it
          s3[ch2[weari]:(ch1[weari + 1] - 1)] <- 1
        }
        if (ch1[weari] > (NLongEpochs - (24 / (ws2 / 3600)))) {
          # final 24 h: under 3 h after more than 1 h of non-wear
          if (wear[weari, 1] < (3 / (ws2 / 3600)) &
              wear[weari, 3] > (1 / (ws2 / 3600))) {
            s3[ch2[weari]:(ch1[weari + 1] - 1)] <- 1
          }
        }
      }
    }
    # GGIR tests the enclosing nonWearEdgeCorrection, not the argument
    applyEdgeCorrection <- if (ggir_exact) nonWearEdgeCorrection else EdgeCorrection
    if (applyEdgeCorrection == TRUE) {
      if (length(ch1) > 0) {
        if (ch1[1] < (3 / (ws2 / 3600)) &
            ch1[1] > 1) {
          # first non-wear episode starts within 3 h: non-wear from the start
          s3[1:(ch1[1] - 1)] <- 1
        }
        if (ch2[length(ch2)] > (length(s3) - (3 / (ws2 / 3600))) &
            ch2[length(ch2)] != length(s3)) {
          # a non-wear episode ends within the last 3 h: non-wear to the end
          s3[ch2[length(ch2)]:length(s3)] <- 1
        }
      }
    }
    invisible(list(r1 = s1, r3 = s3))
  }
  NEWlabels <- additionalNonWearDetection(s1 = r1, s3 = r3, ws2, EdgeCorrection = nonWearEdgeCorrection)
  r1 <- NEWlabels$r1
  r3 <- NEWlabels$r3
  r1 <- r1[-c(1, length(r1))]
  r2 <- r2[-c(1, length(r2))]
  r3 <- r3[-c(1, length(r3))]
  # two more passes on the union of r1 and r3
  for (i in 1:2) {
    r1b <- r3 + r1
    r1b[which(r1b > 1)] <- 1
    r1b <- c(0, r1b, 0)
    r3b <- c(0, r3, 0)
    NEWlabels <- additionalNonWearDetection(s1 = r1b, s3 = r3b, ws2, EdgeCorrection = FALSE)
    r1b <- NEWlabels$r1
    r3b <- NEWlabels$r3
    r3 <- r3b[-c(1, length(r3b))]
  }
  invisible(list(
    r1 = r1,
    r2 = r2,
    r3 = r3,
    LC = LC,
    LC2 = LC2,
    nonwearHoursFiltered = nonwearHoursFiltered,
    nonwearEventsFiltered = nonwearEventsFiltered
  ))
}

# MIDNIGHTS

#' Find the Long Epochs That Start a Day
#'
#' GGIR's g.detecmidnight: a day starts at the epoch whose clock time, summed as
#' fractional hours from the ISO timestamp read in \code{desiredtz} (or the second token
#' of a "date time" string), equals \code{dayborder} exactly. With no such epoch the last
#' timestamp is used as a dummy midnight so the day slicing still works.
#'
#' @param time Character vector of long-epoch timestamps.
#' @param desiredtz Timezone in which the ISO strings are read.
#' @param dayborder Hour at which a day starts (GGIR default 0).
#' @return list(firstmidnight, firstmidnighti, lastmidnight, lastmidnighti, midnights,
#'   midnightsi), GGIR's return list.
#' @keywords internal
#' @noRd
.raw.detect.midnight <- function(time, desiredtz = "", dayborder = 0) {
  convert2clock <- function(x) { # ISO
    return(format(as.POSIXlt(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = desiredtz), "%H:%M:%S"))
  }
  convert2clock_space <- function(x) { # "date time"
    return(unlist(strsplit(x, " "))[2])
  }
  checkmidnight <- function(x){
    temp1 <- as.numeric(unlist(strsplit(x, ":")))
    return(sum(temp1 / c(1, 60, 3600)))
  }
  space <- ifelse(length(unlist(strsplit(time[1], " "))) > 1,TRUE,FALSE)
  if (space == TRUE) {
    time_clock <- sapply(time, FUN = convert2clock_space)
    checkmidnight_out <- lapply(time_clock, FUN = checkmidnight)
  } else {
    time_clock <- convert2clock(time)
    checkmidnight_out <- unlist(lapply(time_clock, FUN = checkmidnight))
  }
  midn <- which(checkmidnight_out == dayborder)
  if (length(midn) == 0) { # no midnight: the last timestamp is a dummy
    midnights <- time[length(time)]
    midnightsi <- length(time)
  } else {
    midnights <- format(time[midn])
    midnightsi <- midn
  }
  if (length(midn) == 0) {
    lastmidnight <- time[length(time)]
    lastmidnighti <- length(time)
    firstmidnight <- time[1]
    firstmidnighti <- 1
  } else {
    lastmidnight <- midnights[length(midnights)]
    lastmidnighti <- midnightsi[length(midnights)]
    firstmidnight <- midnights[1]
    firstmidnighti <- midnightsi[1]
  }
  invisible(list(firstmidnight = firstmidnight, firstmidnighti = firstmidnighti,
                 lastmidnight = lastmidnight, lastmidnighti = lastmidnighti,
                 midnights = midnights, midnightsi = midnightsi))
}

# PROTOCOL MASK

#' The Study-Protocol Mask r4 of GGIR's g.impute
#'
#' Marks the long epochs outside the analysis protocol: hours deleted at the start and
#' end (strategy 1), everything outside the first and last midnight (2), the most active
#' \code{ndayswindow} days by rolling mean (3) or by calendar day (5), everything before
#' the first midnight (4), then \code{maxdur} days and \code{max_calendar_days}. With
#' GGIR's defaults r4 is all zero.
#'
#' @details The study_dates_file and TimeSegments2Zero paths of g.impute are not ported.
#'   Kept as in GGIR: the \code{LD < 1440} truncation that turns r4 into a plain vector,
#'   strategy 3's \code{zoo::rollmean} that errors when the recording is shorter than
#'   \code{ndayswindow} days, the \code{acc.metric} fallback to the first metashort
#'   column that is neither timestamp nor angle, and the max_calendar_days dates taken
#'   with \code{as.Date()} on POSIXct values, which are UTC dates.
#'
#' @param r1,r2 The non-wear and clipping vectors from \code{.raw.weardec}.
#' @param metalong,metashort GGIR-shaped tables (no canhrActi time column).
#' @param dmidn The list returned by \code{.raw.detect.midnight}.
#' @param windowsizes c(ws3, ws2, ws).
#' @param desiredtz Timezone (max_calendar_days only).
#' @param data_masking_strategy,hrs.del.start,hrs.del.end,maxdur GGIR's params_cleaning
#'   members.
#' @param max_calendar_days,ndayswindow,acc.metric GGIR members that raw.params() does not
#'   carry (GGIR defaults 0, 7, "ENMO").
#' @return list(r4, starttimei, endtimei, hrs.del.start, hrs.del.end, maxdur, LD, ND);
#'   strategy 3 rewrites hrs.del.start and maxdur.
#' @keywords internal
#' @noRd
.raw.wear.mask <- function(r1, r2, metalong, metashort, dmidn, windowsizes = c(5, 900, 3600),
                           desiredtz = "", data_masking_strategy = 1,
                           hrs.del.start = 0, hrs.del.end = 0, maxdur = 0,
                           max_calendar_days = 0, ndayswindow = 7, acc.metric = "ENMO") {
  shortEpoch <- windowsizes[1]
  mediumEpoch <- windowsizes[2]
  longEpoch <- windowsizes[3]
  n_short_in_mediumEpoch <- mediumEpoch/shortEpoch
  n_longEpoch_perday <- 86400 / longEpoch
  n_mediumEpoch_perday <- 86400 / mediumEpoch
  n_shortEpoch_perday <- 86400 / shortEpoch
  n_shortEpoch_permin <- 60 / shortEpoch
  n_longEpoch_perhour <- 3600 / longEpoch
  n_medium_perhour <- 3600 / mediumEpoch
  n_shortEpoch_perhour <- 3600 / shortEpoch
  LD <- nrow(metalong) * (mediumEpoch / 60) # minutes
  ND <- nrow(metalong) / n_mediumEpoch_perday # days
  timeline <- seq(0, ceiling(nrow(metalong) / n_mediumEpoch_perday),
                  by = 1/n_mediumEpoch_perday)
  timeline <- timeline[1:nrow(metalong)]
  r4 <- matrix(0,length(r1),1)
  firstmidnight <- dmidn$firstmidnight;  firstmidnighti <- dmidn$firstmidnighti
  lastmidnight <- dmidn$lastmidnight;    lastmidnighti <- dmidn$lastmidnighti
  midnights <- dmidn$midnights;          midnightsi <- dmidn$midnightsi
  # study_dates_file is not ported; its defaults are fixed
  study_dates_log_used <- c(FALSE, FALSE)
  study_date_indices <- NULL
  if (data_masking_strategy == 1) { # hours from start and end
    if (hrs.del.start > 0) {
      r4[1:(hrs.del.start * n_medium_perhour)] <- 1
    }
    if (hrs.del.end > 0) {
      if (length(r4) > hrs.del.end * n_medium_perhour) {
        r4[(length(r4) + 1 - (hrs.del.end * n_medium_perhour)):length(r4)] <- 1
      } else {
        r4[1:length(r4)] <- 1
      }
    }
    if (LD < 1440) {
      r4 <- r4[1:floor(LD / (mediumEpoch / 60))]
    }
    starttimei <- 1
    endtimei <- length(r4)
  } else if (data_masking_strategy == 2) { # first to last midnight
    starttime <- firstmidnight
    endtime <- lastmidnight
    if (!any(study_dates_log_used)) {
      starttimei <- firstmidnighti
      endtimei <- lastmidnighti
      if (firstmidnighti != 1) {
        r4[1:(firstmidnighti - 1)] <- 1 # the midnight epoch itself belongs to the first full day
      }
      r4[(lastmidnighti):length(r4)] <- 1
    } else {
      starttimei <- 1
      endtimei <- length(r4)
    }
  } else if (data_masking_strategy %in% c(3, 5)) { # most active days
    if (acc.metric %in% colnames(metashort)) {
      acc <- as.numeric(as.matrix(metashort[, acc.metric]))
    } else {
      acc.metric <- grep("timestamp|angle", colnames(metashort),
                         value = TRUE, invert = TRUE)[1]
      acc <- as.numeric(as.matrix(metashort[, acc.metric]))
    }
    acc[which(rep(r2, each = n_short_in_mediumEpoch) == 1 |
                rep(r1, each = n_short_in_mediumEpoch) == 1)] <- 0
    if (!is.null(study_date_indices)) {
      if (study_dates_log_used[1] == TRUE) {
        tt1 <- ((study_date_indices[1] - 1) * n_short_in_mediumEpoch) + 1
      } else {
        tt1 <- 1
      }
      if (study_dates_log_used[2] == TRUE) {
        tt2 <- max(study_date_indices) * n_short_in_mediumEpoch
      } else {
        tt2 <- length(acc)
      }
      acc <- acc[tt1:tt2]
    }
    if (data_masking_strategy == 3) {
      if (!requireNamespace("zoo", quietly = TRUE)) {
        stop("data_masking_strategy 3 needs the 'zoo' package: install.packages('zoo')", call. = FALSE)
      }
      NDAYS <- length(acc) / n_shortEpoch_perday
      acc_roll_mean <- zoo::rollmean(x = acc, k = ndayswindow * n_shortEpoch_perday, align = "left")
      start_ndayswindow_hour <- floor(which(acc_roll_mean == max(acc_roll_mean))[1] / n_shortEpoch_perhour)
      hrs.del.start <- start_ndayswindow_hour + hrs.del.start
      maxdur <- ((start_ndayswindow_hour / 24) + ndayswindow) - (hrs.del.end/24)
      if (maxdur > NDAYS) maxdur <- NDAYS
      if (hrs.del.start > 0) {
        start_epoch <- max((hrs.del.start * n_medium_perhour) - 1, 1)
        r4[1:start_epoch] <- 1
      }
      ignore_from <- round((maxdur * n_mediumEpoch_perday))
      if (maxdur > 0 && length(r4) > (ignore_from + 1)) {
        r4[ignore_from:length(r4)] <- 1
      }
      if (LD < 1440) r4 <- r4[1:floor(LD / (mediumEpoch / 60))]
    } else if (data_masking_strategy == 5) {
      acc_roll_mean <- c()
      if (study_dates_log_used[1] == TRUE) {
        midnightsi <- midnightsi[midnightsi >= firstmidnighti & midnightsi <= lastmidnighti]
        firstmidnighti <- firstmidnighti - (midnightsi[1] - 1)
        lastmidnighti <- lastmidnighti - midnightsi[1]
        midnightsi <- midnightsi - midnightsi[1] + 1
      }
      for (i in 1:length(midnightsi)) {
        p0 <- ((midnightsi[i] - 1) * n_short_in_mediumEpoch) + 1
        if (i == length(midnightsi) && i + ndayswindow > length(midnightsi)) {
          p1 <- length(acc)
        } else {
          p1 <- (midnightsi[i + ndayswindow] - 1) * n_short_in_mediumEpoch
        }
        if (is.na(p1) || p1 > length(acc)) {
          break
        }
        acc_roll_mean[i] <- mean(acc[p0:p1], na.rm = TRUE)
      }
      start_index_most_active_window <- ifelse(length(acc_roll_mean) > 0, which.max(acc_roll_mean), 1)
      offset <- ifelse(study_dates_log_used[1] == TRUE, firstmidnighti - 1, 0)
      ignore_until <- midnightsi[start_index_most_active_window] + (hrs.del.start * n_medium_perhour) - 1 - offset
      if (ignore_until > 0) r4[1:pmin(ignore_until, length(r4))] <- 1
      target_idx <- start_index_most_active_window + ndayswindow
      if (target_idx > length(midnightsi)) target_idx <- length(midnightsi)
      ignore_from <- midnightsi[target_idx] - (hrs.del.end * n_medium_perhour) - offset
      if (!is.na(ignore_from) && ignore_from < length(r4)) {
        r4[pmax(1, ignore_from):length(r4)] <- 1
      }
    }
    starttimei <- 1
    endtimei <- length(r4)
  } else if (data_masking_strategy == 4) { # first midnight to end
    starttime <- firstmidnight
    endtime <- lastmidnight
    if (!any(study_dates_log_used)) {
      starttimei <- firstmidnighti
      endtimei <- lastmidnighti
      if (firstmidnighti != 1) {
        r4[1:(firstmidnighti - 1)] <- 1
      }
    } else {
      starttimei <- 1
      endtimei <- length(r4)
    }
  } else {
    stop("data_masking_strategy must be 1, 2, 3, 4 or 5", call. = FALSE)
  }
  # maxdur
  if (maxdur > 0 &&
      (length(r4) > (maxdur * n_mediumEpoch_perday) + 1)) {
    r4[((maxdur * n_mediumEpoch_perday) + 1):length(r4)] <- 1
  }
  # max_calendar_days
  if (max_calendar_days > 0) {
    if (any(study_dates_log_used)) {
      datetime <- metalong$timestamp[firstmidnighti:(lastmidnighti - 1)]
    } else {
      datetime <- metalong$timestamp
    }
    dates <- as.Date(.raw.iso8601.to.posix(datetime, tz = desiredtz))
    if (max_calendar_days < length(unique(dates))) {
      lastDateToInclude <- sort(unique(dates))[max_calendar_days]
      r4[which(dates > lastDateToInclude)] <- 1
    }
  }
  # unreachable without a study dates log
  if (any(study_dates_log_used)) {
    r4_bu[which(r4_bu == 0),] <- r4
    r4 <- r4_bu
  }
  list(r4 = r4, starttimei = starttimei, endtimei = endtimei,
       hrs.del.start = hrs.del.start, hrs.del.end = hrs.del.end, maxdur = maxdur,
       LD = LD, ND = ND)
}

#' Expand a Long-Epoch Vector to Short Epochs
#'
#' GGIR's r5long: the matrix fill by column, transpose and reshape that repeat each
#' value \code{n_short_in_mediumEpoch} times.
#'
#' @param r5 Numeric vector (one value per long epoch).
#' @param n_short_in_mediumEpoch ws2 / ws3.
#' @return A one-column numeric matrix with \code{length(r5) * n_short_in_mediumEpoch}
#'   rows.
#' @keywords internal
#' @noRd
.raw.r5long <- function(r5, n_short_in_mediumEpoch) {
  r5long <- matrix(0,length(r5), n_short_in_mediumEpoch)
  r5long <- replace(r5long, 1:length(r5long), r5)
  r5long <- t(r5long)
  dim(r5long) <- c(length(r5) * n_short_in_mediumEpoch,1)
  r5long
}

# DAY SLICING

#' Short-Epoch Index Range of Every Day
#'
#' GGIR's part-2 day (g.analyse.perday): the run of short epochs from one dayborder
#' crossing to the epoch before the next; the first day runs from the first epoch and
#' the last day to the end of the table. Kept as in GGIR: a recording with no crossing
#' (the dummy midnight) yields one day whose last index is \code{nrow(metashort) - 1},
#' and \code{floor(ndays)} in the case tests.
#'
#' @param time Character long-epoch timestamps (metalong$timestamp).
#' @param nshort Number of short epochs (nrow(metashort)).
#' @param dmidn The list from \code{.raw.detect.midnight}.
#' @param ws3,ws2 Short and long epoch lengths in seconds.
#' @return data.frame(day, first_epoch, last_epoch) with attributes ndays, nfulldays,
#'   startatmidnight and endatmidnight.
#' @keywords internal
#' @noRd
.raw.day.slices <- function(time, nshort, dmidn, ws3 = 5, ws2 = 900) {
  firstmidnighti <- dmidn$firstmidnighti
  lastmidnight <- dmidn$lastmidnight; lastmidnighti <- dmidn$lastmidnighti
  midnights <- dmidn$midnights; midnightsi <- dmidn$midnightsi
  nfulldays <- (lastmidnighti - firstmidnighti) / ((3600/ws2)*24)
  ndays <- length(midnights) + 1
  startatmidnight <- endatmidnight <- 0
  if (nfulldays >= 1) {
    if (firstmidnighti == 1) {  # starts at midnight
      ndays <- ndays - 1
      startatmidnight <-  1
    }
    if (lastmidnight == time[length(time)] & nshort < ((60/ws3) * 1440)) { # ends at midnight
      ndays <- ndays - 1
      endatmidnight <- 1
    }
  } else {
    # a short recording that starts and ends at midnight
    if (firstmidnighti == 1 &
        lastmidnight == time[length(time)] &
        nshort < ((60/ws3) * 1440)) {
      ndays <- ndays - 1
      endatmidnight <- 1
    }
  }
  slices <- vector("list", max(0, ndays))
  if (ndays >= 1) {
    for (di in 1:ndays) {
      if (startatmidnight == 1 & endatmidnight == 1) {
        qqq1 <- midnightsi[di] * (ws2/ws3)
        qqq2 <- (midnightsi[(di + 1)]*(ws2/ws3)) - 1
      } else if (startatmidnight == 1 & endatmidnight == 0) {
        if (di < floor(ndays)) { # floor: DST makes ndays non-integer
          qqq1 <- (midnightsi[di] - 1) * (ws2 / ws3) + 1
          qqq2 <- ((midnightsi[(di + 1)] - 1) * (ws2 / ws3))
        } else if (di == floor(ndays)) {
          qqq1 <- (midnightsi[di] - 1) * (ws2 / ws3) + 1
          qqq2 <- nshort
        }
      } else if (startatmidnight == 0 & endatmidnight == 0) {
        if (di == 1) {
          qqq1 <- 1
          qqq2 <- ((midnightsi[di] - 1) * (ws2 / ws3))
        } else if (di > 1 & di < floor(ndays)) {
          qqq1 <- (midnightsi[(di - 1)] - 1) * (ws2 / ws3) + 1
          qqq2 <- ((midnightsi[di] - 1) * (ws2 / ws3))
        } else if (di == floor(ndays)) {
          qqq1 <- (midnightsi[(di - 1)] - 1)*(ws2 / ws3) + 1
          qqq2 <- nshort
        }
      } else if (startatmidnight == 0 & endatmidnight == 1) {
        if (di == 1) {
          qqq1 <- 1
          qqq2 <- (midnightsi[di] * (ws2 / ws3)) - 1
        } else if (di > 1 & di <= floor(ndays)) {
          qqq1 <- midnightsi[(di - 1)] * (ws2 / ws3)
          qqq2 <- (midnightsi[di] * (ws2 / ws3)) - 1
        }
      }
      if (qqq2 > nshort) qqq2 <- nshort
      slices[[di]] <- data.frame(day = di, first_epoch = as.numeric(qqq1), last_epoch = as.numeric(qqq2))
    }
  }
  out <- if (length(slices) > 0) do.call(rbind, slices) else
    data.frame(day = integer(0), first_epoch = numeric(0), last_epoch = numeric(0))
  rownames(out) <- NULL
  attr(out, "ndays") <- ndays
  attr(out, "nfulldays") <- nfulldays
  attr(out, "startatmidnight") <- startatmidnight
  attr(out, "endatmidnight") <- endatmidnight
  out
}

#' Hours and Valid Hours per Day
#'
#' Per GGIR day: the slice of qcheck (r5long), valid hours as the epochs equal to 0 over
#' 3600/ws3, the calendar date from the first timestamp of the slice (formatted in
#' desiredtz as part2_daysummary does), and the weekday counted on from the recording's
#' first weekday. The valid flag is GGIR's person-level rule: n_hours not NA and
#' \code{n_valid_hours >= includedaycrit[1]}.
#'
#' @param slices The data.frame from \code{.raw.day.slices}.
#' @param timestamp Character short-epoch timestamps (metashort$timestamp).
#' @param r5long GGIR's r5long (one value per short epoch).
#' @param ws3 Short epoch length in seconds.
#' @param wday,wdayname The recording's first weekday (M$wday, 1 = Sunday) and its name.
#' @param includedaycrit Minimum valid hours for a valid day (GGIR default 16).
#' @param desiredtz Timezone used to format the calendar date.
#' @return data.frame(day, date, weekday, weekend, start_time, n_hours, n_valid_hours, valid,
#'   first_epoch, last_epoch).
#' @keywords internal
#' @noRd
.raw.daily.hours <- function(slices, timestamp, r5long, ws3 = 5, wday = 1, wdayname = "Sunday",
                             includedaycrit = 16, desiredtz = "") {
  weekdays <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  weekdays <- rep(weekdays, 104) # 104 weeks, as in GGIR
  n <- nrow(slices)
  out <- data.frame(day = integer(n), date = as.Date(rep(NA_character_, n)), weekday = character(n),
                    weekend = logical(n), start_time = character(n),
                    n_hours = numeric(n), n_valid_hours = numeric(n), valid = logical(n),
                    first_epoch = numeric(n), last_epoch = numeric(n), stringsAsFactors = FALSE)
  if (n == 0) return(out)
  for (di in 1:n) {
    qqq1 <- slices$first_epoch[di]; qqq2 <- slices$last_epoch[di]
    vari_ts <- timestamp[qqq1:qqq2]
    val <- r5long[qqq1:qqq2]
    val <- as.numeric(val)
    nvalidhours <- length(which(val == 0)) / (3600 / ws3)
    nhours <- length(val) / (3600 / ws3)
    calendardate <- unlist(strsplit(as.character(vari_ts[1])," "))[1]
    if (di == 1) {
      wd <- wdayname
    } else {
      wd <- weekdays[wday + (di - 1)]
    }
    dd <- .raw.iso8601.to.posix(calendardate, tz = desiredtz)
    out$day[di] <- as.integer(di)
    out$date[di] <- as.Date(format(dd, format = "%Y-%m-%d"))
    out$weekday[di] <- wd
    out$weekend[di] <- wd %in% c("Saturday", "Sunday")
    out$start_time[di] <- calendardate
    out$n_hours[di] <- nhours
    out$n_valid_hours[di] <- nvalidhours
    out$first_epoch[di] <- qqq1
    out$last_epoch[di] <- qqq2
  }
  out$valid <- is.na(as.numeric(out$n_hours)) == FALSE & as.numeric(out$n_valid_hours) >= includedaycrit[1]
  out
}

# EXPORTED FUNCTIONS

#' Wear Decision, Protocol Mask and Valid Hours per Day
#'
#' GGIR's part-2 decision on the long-epoch tables of a recording: which long epochs are
#' non-wear (r1), clipped (r2), implausible wear islands or edges (r3), outside the study
#' protocol (r4), and their union r5, expanded to r5long at the short epoch. The
#' recording is then sliced into days at \code{dayborder}, hours and valid hours are
#' counted per day (valid = r5long 0), valid days are flagged with \code{includedaycrit},
#' and the recording-level wear and clipping summaries of part2_summary.csv are
#' reported. With GGIR's defaults rout and r5long are identical() to g.impute's and the
#' day table equals part2_daysummary.csv. The average-day imputation of g.impute and the
#' activity summaries of g.analyse are not part of this function.
#'
#' @details
#' Order of operations, as in g.part2: \code{.raw.weardec} on metalong (wearthreshold 2,
#' hard-coded in GGIR), the protocol mask \code{.raw.wear.mask}, \code{r5 = r1 + r2 + r3 +
#' r4} capped at 1 with -1 on tail-expansion epochs, r5long. As g.part2 does before
#' g.analyse, the tail-expansion epochs are removed from the tables used for the day
#' slicing and the recording summary; rout and r5long keep them.
#'
#' Timezone: GGIR part 2 uses the timezone part 1 ran with. A canhrActi_raw_meta records
#' it in \code{settings$desiredtz} and that value is used here; a params desiredtz that
#' differs is noted in \code{messages}. For a GGIR M list the params desiredtz is used.
#'
#' Parameters read: dayborder, includedaycrit, nonWearEdgeCorrection,
#' nonwearFiltermaxHours, nonwearFilterWindow, data_masking_strategy, hrs.del.start,
#' hrs.del.end, maxdur, ggir_exact. Three GGIR members that \code{raw.params()} does not
#' carry are explicit arguments with GGIR's defaults.
#'
#' @param meta A \code{canhrActi_raw_meta} from \code{\link{raw.getmeta}}, or GGIR's M
#'   list from a meta_*.RData milestone.
#' @param params A \code{\link{raw.params}} object, or NULL for GGIR's defaults.
#' @param ... Individual parameter overrides (e.g. \code{includedaycrit = 10}), routed
#'   through \code{raw.params()}.
#' @param max_calendar_days GGIR's params_cleaning member (0 = off).
#' @param ndayswindow GGIR's params_cleaning member for strategies 3 and 5 (default 7).
#' @param acc.metric GGIR's params_general member for strategies 3 and 5 (default "ENMO").
#'
#' @return An object of class "canhrActi_raw_wear": a list with
#' \describe{
#'   \item{rout}{data.frame r1 (non-wear), r2 (clipping), r3 (additional non-wear), r4
#'     (protocol mask), r5 (any; -1 on tail-expansion epochs), one row per long epoch.}
#'   \item{r5long}{One-column numeric matrix, r5 repeated per short epoch.}
#'   \item{daily}{One row per GGIR day: day, date, weekday, weekend, start_time, n_hours,
#'     n_valid_hours, valid, first_epoch and last_epoch (row range in metashort).}
#'   \item{wear_days, wear_dur_def_proto_day}{Wear time inside the protocol in days (the
#'     same number twice).}
#'   \item{meas_dur_dys, meas_dur_def_proto_day}{Recording length and protocol length in
#'     days.}
#'   \item{clipping_score}{Fraction of 15-minute blocks with more than 30 percent clipped
#'     samples.}
#'   \item{n_valid_days, n_valid_weekdays, n_valid_weekend_days}{Counts of valid days.}
#'   \item{LC, LC2, nonwearHoursFiltered, nonwearEventsFiltered}{GGIR's g.weardec extras.}
#'   \item{midnights}{The list from \code{.raw.detect.midnight}.}
#'   \item{n_long, n_short, n_expanded_long, n_expanded_short}{Table sizes and the
#'     tail-expansion epochs excluded from the day slicing.}
#'   \item{starttimei, endtimei, hrs.del.start, hrs.del.end, maxdur}{GGIR's g.impute
#'     bookkeeping.}
#'   \item{settings, messages, elapsed}{The values used, collected notes and seconds
#'     elapsed.}
#' }
#' For a corrupt or too-short recording the object carries NULL tables, \code{status}
#' "no_data" and a message, without an error.
#'
#' @examples
#' \dontrun{
#' meta <- raw.getmeta(info, cal)
#' w <- raw.wear.decision(meta)
#' colSums(w$rout); w$daily; w$wear_days
#' }
#' @export
raw.wear.decision <- function(meta, params = NULL, ..., max_calendar_days = 0, ndayswindow = 7,
                              acc.metric = "ENMO") {
  t0 <- Sys.time()
  if (!is.list(meta) || !all(c("metalong", "metashort") %in% names(meta))) {
    stop("meta must be a canhrActi_raw_meta from raw.getmeta() or a GGIR M list with metalong and metashort",
         call. = FALSE)
  }
  is_canhr <- inherits(meta, "canhrActi_raw_meta")
  params <- .raw.calibrate.params(list(params = NULL), params, list(...))
  messages <- character()
  ggir_exact <- isTRUE(.raw.param(params, "ggir_exact", TRUE))
  desiredtz <- .raw.param(params, "desiredtz", "")
  if (is_canhr && !is.null(meta$settings$desiredtz)) {
    if (!identical(meta$settings$desiredtz, desiredtz)) {
      messages <- c(messages, paste0("desiredtz taken from the epoch tables (\"", meta$settings$desiredtz,
                                     "\"), as GGIR part 2 does, not from params (\"", desiredtz, "\")"))
    }
    desiredtz <- meta$settings$desiredtz
  }
  if (length(desiredtz) == 0) desiredtz <- ""
  dayborder <- .raw.param(params, "dayborder", 0)
  includedaycrit <- .raw.param(params, "includedaycrit", 16)
  nonWearEdgeCorrection <- .raw.param(params, "nonWearEdgeCorrection", TRUE)
  nonwearFiltermaxHours <- .raw.param(params, "nonwearFiltermaxHours", NULL)
  nonwearFilterWindow <- .raw.param(params, "nonwearFilterWindow", NULL)
  data_masking_strategy <- .raw.param(params, "data_masking_strategy", 1)
  hrs.del.start <- .raw.param(params, "hrs.del.start", 0)
  hrs.del.end <- .raw.param(params, "hrs.del.end", 0)
  maxdur <- .raw.param(params, "maxdur", 0)
  wearthreshold <- 2 # hard-coded in GGIR

  settings <- list(wearthreshold = wearthreshold, windowsizes = meta$windowsizes,
                   desiredtz = desiredtz, dayborder = dayborder, includedaycrit = includedaycrit,
                   nonWearEdgeCorrection = nonWearEdgeCorrection,
                   nonwearFiltermaxHours = nonwearFiltermaxHours,
                   nonwearFilterWindow = nonwearFilterWindow,
                   data_masking_strategy = data_masking_strategy,
                   hrs.del.start = hrs.del.start, hrs.del.end = hrs.del.end, maxdur = maxdur,
                   max_calendar_days = max_calendar_days, ndayswindow = ndayswindow,
                   acc.metric = acc.metric, ggir_exact = ggir_exact)
  finish <- function(obj) {
    obj$settings <- settings
    obj$messages <- messages
    obj$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    structure(obj, class = "canhrActi_raw_wear")
  }
  empty <- function() {
    list(rout = NULL, r5long = NULL, daily = NULL, wear_days = NA_real_,
         wear_dur_def_proto_day = NA_real_, meas_dur_dys = NA_real_,
         meas_dur_def_proto_day = NA_real_, clipping_score = NA_real_,
         n_valid_days = 0L, n_valid_weekdays = 0L, n_valid_weekend_days = 0L,
         LC = NA_integer_, LC2 = NA_integer_, nonwearHoursFiltered = 0, nonwearEventsFiltered = 0,
         midnights = NULL, n_long = 0L, n_short = 0L, n_expanded_long = 0L, n_expanded_short = 0L,
         starttimei = NA_real_, endtimei = NA_real_,
         hrs.del.start = hrs.del.start, hrs.del.end = hrs.del.end, maxdur = maxdur,
         status = "no_data", file = meta$file)
  }
  if (isTRUE(meta$filecorrupt) || isTRUE(meta$filetooshort) ||
      is.null(meta$metalong) || is.null(meta$metashort) ||
      nrow(meta$metalong) == 0 || nrow(meta$metashort) == 0) {
    messages <- c(messages, "No epoch tables (corrupt, too short or skipped recording); no wear decision made")
    return(finish(empty()))
  }

  # GGIR-shaped tables, or the acc.metric fallback of strategies 3 and 5 would pick up
  # the canhrActi time column
  M <- if (is_canhr) as.ggir.M(meta) else meta
  windowsizes <- M$windowsizes # default c(5, 900, 3600)
  if (is.null(windowsizes)) stop("meta has no windowsizes", call. = FALSE)
  metashort <- M$metashort
  metalong <- M$metalong
  shortEpoch <- windowsizes[1]
  mediumEpoch <- windowsizes[2]
  n_short_in_mediumEpoch <- mediumEpoch/shortEpoch
  n_mediumEpoch_perday <- 86400 / mediumEpoch
  n_shortEpoch_perday <- 86400 / shortEpoch
  # the two tables must cover the same span
  if ((nrow(metalong) / n_mediumEpoch_perday) - (nrow(metashort) / n_shortEpoch_perday) > 0.1) {
    messages <- c(messages, "Matrices 'metalong' and 'metashort' are not compatible")
  }
  tmi <- which(colnames(metalong) == "timestamp")
  time <- as.character(as.matrix(metalong[,tmi]))
  out <- .raw.weardec(metalong, wearthreshold, mediumEpoch,
                      nonWearEdgeCorrection = nonWearEdgeCorrection,
                      nonwearFiltermaxHours = nonwearFiltermaxHours,
                      nonwearFilterWindow = nonwearFilterWindow,
                      desiredtz = desiredtz, qwindowImp = NULL, ggir_exact = ggir_exact)
  r1 <- out$r1
  r2 <- out$r2
  r3 <- out$r3
  LC <- out$LC
  LC2 <- out$LC2
  nonwearHoursFiltered <- out$nonwearHoursFiltered
  nonwearEventsFiltered <- out$nonwearEventsFiltered
  dmidn <- .raw.detect.midnight(time, desiredtz, dayborder)
  # protocol mask
  mask <- .raw.wear.mask(r1, r2, metalong, metashort, dmidn, windowsizes = windowsizes,
                         desiredtz = desiredtz, data_masking_strategy = data_masking_strategy,
                         hrs.del.start = hrs.del.start, hrs.del.end = hrs.del.end, maxdur = maxdur,
                         max_calendar_days = max_calendar_days, ndayswindow = ndayswindow,
                         acc.metric = acc.metric)
  r4 <- mask$r4
  # r5: any reason, -1 on tail-expansion epochs
  r5 <- r1 + r2 + r3 + r4
  r5[which(r5 > 1) ] <- 1
  r5[which(metalong$nonwearscore == -1) ] <- -1
  r5long <- .raw.r5long(r5, n_short_in_mediumEpoch)
  rout <- data.frame(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5, stringsAsFactors = TRUE)

  # the tail-expansion epochs are left out of the day slicing and the summaries
  expanded_time_short <- which(r5long == -1)
  expanded_time_long <- which(rout$r5 == -1)
  metashort_a <- metashort; metalong_a <- metalong; rout_a <- rout
  if (length(expanded_time_long) > 0) {
    metashort_a <- metashort[-expanded_time_short,]
    metalong_a <- metalong[-expanded_time_long,]
    rout_a <- rout[-expanded_time_long,]
  }
  r4a <- as.numeric(as.matrix(rout_a[,4]))
  r5a <- as.numeric(as.matrix(rout_a[,5]))
  ws3 <- windowsizes[1]
  ws2 <- windowsizes[2]
  time_a <- as.character(metalong_a[, which(colnames(metalong_a) == "timestamp")])
  LD <- nrow(metalong_a) * (ws2/60) # minutes
  dmidn_a <- .raw.detect.midnight(time_a, desiredtz = desiredtz, dayborder = dayborder)
  qcheck <- .raw.r5long(r5a, (ws2/ws3))
  slices <- .raw.day.slices(time_a, nrow(metashort_a), dmidn_a, ws3 = ws3, ws2 = ws2)
  daily <- .raw.daily.hours(slices, timestamp = as.character(metashort_a$timestamp), r5long = qcheck,
                            ws3 = ws3, wday = M$wday, wdayname = M$wdayname,
                            includedaycrit = includedaycrit, desiredtz = desiredtz)
  # recording summary of part2_summary.csv
  LWp <- length(which(r5a[which(r4a == 0)] < 1)) * (ws2/60) # wear minutes inside the protocol
  LMp <- length(which(r4a == 0)) * (ws2/60) # protocol minutes
  clipping_score <- LC2  / ((LD/1440)*96)
  meas_dur_dys <- LD/1440
  meas_dur_def_proto_day <- LMp / 1440
  wear_dur_def_proto_day <- LWp / 1440
  # valid weekday and weekend-day counts
  wkend <- which(daily$weekday == "Saturday" | daily$weekday == "Sunday")
  v1 <- which(is.na(as.numeric(daily$n_hours[wkend])) == F &
                as.numeric(daily$n_valid_hours[wkend]) >= includedaycrit[1])
  wkend <- wkend[v1]
  wkday <- which(daily$weekday != "Saturday" & daily$weekday != "Sunday")
  v2 <- which(is.na(as.numeric(daily$n_hours[wkday])) == F &
                as.numeric(daily$n_valid_hours[wkday]) >= includedaycrit[1])
  wkday <- wkday[v2]
  validdays <- which(is.na(as.numeric(daily$n_hours)) == F &
                       as.numeric(daily$n_valid_hours) >= includedaycrit[1])

  obj <- list(rout = rout, r5long = r5long, daily = daily,
              wear_days = wear_dur_def_proto_day,
              wear_dur_def_proto_day = wear_dur_def_proto_day,
              meas_dur_dys = meas_dur_dys, meas_dur_def_proto_day = meas_dur_def_proto_day,
              clipping_score = clipping_score,
              n_valid_days = length(validdays), n_valid_weekdays = length(wkday),
              n_valid_weekend_days = length(wkend),
              LC = LC, LC2 = LC2,
              nonwearHoursFiltered = nonwearHoursFiltered, nonwearEventsFiltered = nonwearEventsFiltered,
              midnights = dmidn,
              n_long = nrow(metalong), n_short = nrow(metashort),
              n_expanded_long = length(expanded_time_long), n_expanded_short = length(expanded_time_short),
              starttimei = mask$starttimei, endtimei = mask$endtimei,
              hrs.del.start = mask$hrs.del.start, hrs.del.end = mask$hrs.del.end, maxdur = mask$maxdur,
              status = "ok", file = meta$file)
  finish(obj)
}

#' Print method for a wear decision object
#'
#' @param x A canhrActi_raw_wear.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_wear <- function(x, ...) {
  cat("\ncanhrActi wear decision (GGIR part 2: g.weardec, g.impute mask, valid hours)\n")
  if (!is.null(x$file$filename) && !is.na(x$file$filename)) cat("  file:        ", x$file$filename, "\n", sep = "")
  if (!identical(x$status, "ok")) {
    cat("  status:      ", x$status, "\n", sep = "")
  } else {
    ws2 <- x$settings$windowsizes[2]
    cs <- colSums(x$rout[, c("r1", "r2", "r3", "r4")])
    cat("  long epochs: ", x$n_long, " (", ws2, " s); non-wear r1 ", cs[["r1"]], ", clipping r2 ", cs[["r2"]],
        ", additional r3 ", cs[["r3"]], ", protocol r4 ", cs[["r4"]], ", any r5 ", sum(x$rout$r5 == 1),
        if (x$n_expanded_long > 0) paste0(", expanded ", x$n_expanded_long) else "", "\n", sep = "")
    cat("  recording:   ", round(x$meas_dur_dys, 3), " days, protocol ", round(x$meas_dur_def_proto_day, 3),
        " days, wear ", round(x$wear_dur_def_proto_day, 4), " days, clipping score ",
        signif(x$clipping_score, 3), "\n", sep = "")
    cat("  valid days:  ", x$n_valid_days, " of ", nrow(x$daily), " (", x$n_valid_weekdays, " weekdays, ",
        x$n_valid_weekend_days, " weekend days; criterion ", x$settings$includedaycrit[1], " h)\n", sep = "")
    if (is.data.frame(x$daily) && nrow(x$daily) > 0) {
      print(x$daily[, c("day", "date", "weekday", "n_hours", "n_valid_hours", "valid")], row.names = FALSE)
    }
  }
  if (length(x$messages) > 0) {
    cat("  messages:\n")
    for (m in x$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(x$elapsed) && !is.na(x$elapsed)) cat("  elapsed:     ", round(x$elapsed, 2), " s\n", sep = "")
  invisible(x)
}
