# Ported from GGIR 3.3-9 R/g.sibreport.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. g.sibreport became raw.sib.report and
# its nested closures became internals taking explicit arguments; g.part4_extractid and
# is.ISO8601 are reused from their own ports; an empty report returns a zero-row
# data.frame where GGIR returns a two-member list of empty POSIXct vectors; the two wrong
# guards on the one-minute acceleration windows and the stale logreport_tmp append are
# reproduced under ggir_exact = TRUE and corrected under ggir_exact = FALSE.

#' Give a Bare Date String a Midnight Clock Time (GGIR convert2POSIX)
#'
#' format() on a POSIXct that falls exactly on midnight drops the time of day.
#'
#' @param x A length-one character timestamp.
#' @return The same string, with " 00:00:00" appended when it carried no clock time.
#' @keywords internal
#' @noRd
.raw.sib.report.convert2posix <- function(x) {
  if (length(unlist(strsplit(x, " "))) == 1) {
    x <- paste0(x, " 00:00:00")
  }
  return(x)
}

#' Attach the Night's Calendar Date to a Diary Report Block (GGIR addDate)
#'
#' The date is the calendar date of the start of the event, moved back one day when the
#' start is before noon, so an onset at 23:00 on the 7th and a wake at 06:00 on the 8th both
#' carry the 7th and match the same imputecodelog row. Applied to the sleeplog and bedlog
#' blocks only.
#'
#' @param x A data.frame with a character start column.
#' @param tz Timezone used for the two conversions.
#' @return x with a date column of class Date.
#' @keywords internal
#' @noRd
.raw.sib.report.add.date <- function(x, tz) {
  start_time <- as.POSIXct(x$start, tz = tz)
  x$date <- as.Date(start_time, tz = tz)
  start_hour <- as.numeric(format(start_time, "%H"))
  start_am <- which(start_hour < 12)
  if (length(start_am) > 0) {
    x$date[start_am] <- x$date[start_am] - 1
  }
  return(x)
}

#' Sustained Inactivity Bouts During Waking Hours, as a Report (GGIR g.sibreport, first half)
#'
#' Every index here is into dayind, the compressed vector of waking epochs, not into ts, so
#' two daytime bouts separated only by a sleep period merge into one row, duration is the
#' number of waking epochs spanned, and the minute before a bout can be hours earlier in real
#' time. Two guards are wrong in GGIR and are reproduced under ggir_exact = TRUE:
#' min(minute_before) > 1 should be >= 1, and max(minute_after) < nrow(ts) compares an index
#' into dayind against the length of the full series. Under ggir_exact = FALSE both are
#' corrected.
#'
#' @param ts The part-5 time series, with time, ACC, diur (0 waking, 1 sleep period time) and
#'   sibdetection (0 or 1).
#' @param ID Character identifier, repeated down the ID column.
#' @param epochlength Short epoch length in seconds.
#' @param desiredtz Timezone used when ts$time is still ISO 8601 character.
#' @param ggir_exact TRUE reproduces the two wrong guards.
#' @return A data.frame of seven columns, or c() when there is no waking sib at all.
#' @keywords internal
#' @noRd
.raw.sib.report.acc <- function(ts, ID, epochlength, desiredtz = "", ggir_exact = TRUE) {
  dayind <- which(ts$diur == 0)
  sib_starts <- which(diff(c(0, ts$sibdetection[dayind], 0)) == 1)
  sib_ends <- which(diff(c(ts$sibdetection[dayind], 0)) == -1)
  Nsibs <- length(sib_starts)
  sibreport <- c()
  if (Nsibs > 0) {
    sibreport <- data.frame(ID = rep(ID, Nsibs),
                            type = rep("sib", Nsibs),
                            start = character(Nsibs),
                            end = character(Nsibs),
                            duration = numeric(Nsibs),
                            mean_acc_1min_before = numeric(Nsibs),
                            mean_acc_1min_after = numeric(Nsibs), stringsAsFactors = FALSE)
    for (sibi in 1:Nsibs) {
      sibreport$start[sibi] <- format(ts$time[dayind[sib_starts[sibi]]])
      sibreport$end[sibi] <- format(ts$time[dayind[sib_ends[sibi]]])
      if (.raw.is.iso8601(sibreport$start[sibi])) {
        sibreport$start[sibi] <- format(.raw.iso8601.to.posix(sibreport$start[sibi], tz = desiredtz))
        sibreport$end[sibi] <- format(.raw.iso8601.to.posix(sibreport$end[sibi], tz = desiredtz))
      }
      sibreport$duration[sibi] <- ((sib_ends[sibi] - sib_starts[sibi]) + 1) / (60 / epochlength)
      minute_before <- (sib_starts[sibi] - (60 / epochlength)):(sib_starts[sibi] - 1)
      minute_after <- (sib_ends[sibi] + 1):(sib_ends[sibi] + (60 / epochlength))
      if (ggir_exact) {
        before_in_range <- min(minute_before) > 1
        after_in_range <- max(minute_after) < nrow(ts)
      } else {
        before_in_range <- min(minute_before) >= 1
        after_in_range <- max(minute_after) <= length(dayind)
      }
      if (before_in_range) {
        sibreport$mean_acc_1min_before[sibi] <- round(mean(ts$ACC[dayind[minute_before]]), digits = 3)
      }
      if (after_in_range) {
        sibreport$mean_acc_1min_after[sibi] <- round(mean(ts$ACC[dayind[minute_after]]), digits = 3)
      }
    }
  }
  return(sibreport)
}

#' Self-Reported Naps, Non-Wear, Sleep and Bed Times as Report Rows (GGIR extract_logs)
#'
#' A basic diary carries a night number and no date, so for the sleeplog and bedlog blocks
#' the date is rebuilt as firstDate + night - 1 and the timestamps are pushed forward a day
#' by three rules in this order: every time at or before 12:00 belongs to the next calendar
#' day; a wake after noon and at or before 18:00 is a day sleeper and also belongs to the
#' next day; and an onset later in the clock than the wake has the onset taken back out. The
#' nap and non-wear blocks get no firstDate and take the else branch, which re-assigns the
#' end to the same date, a no-op, so a period crossing midnight gets an end earlier than its
#' start and the complement of its real duration, as in GGIR.
#'
#' @details Kept as GGIR has it: 1:floor(length(times)/2) counts down to 1:0 with fewer than
#'   two timestamps, harmlessly. logreport_tmp is appended outside the branch that builds it,
#'   so a row whose timestamps all fall away either fails or appends the previous row's block
#'   again; under ggir_exact = FALSE the append is skipped when nothing was built.
#'
#' @param log One of the diary tables from raw.sleeplog(): naplog, nonwearlog, sleeplog or
#'   bedlog.
#' @param ID Character identifier of the recording, used to select the diary rows.
#' @param logname The value written into the type column: "nap", "nonwear", "sleeplog" or
#'   "bedlog".
#' @param dateformat The date format raw.sleeplog() recognised, its dateformat member.
#' @param desiredtz Timezone for every date and timestamp conversion.
#' @param firstDate Date of the first night, supplied for the sleeplog and bedlog and NULL for
#'   naps and non-wear.
#' @param ggir_exact TRUE reproduces the stale logreport_tmp append.
#' @return A data.frame of ID, type, start, end and duration, or c().
#' @keywords internal
#' @noRd
.raw.sib.report.extract.logs <- function(log, ID, logname, dateformat, desiredtz = "",
                                         firstDate = NULL, ggir_exact = TRUE) {
  logreport <- c()
  if (length(log) > 0) {
    idwi <- .raw.sleeplog.extractid(idloc = 1, # idloc is irrelevant here
                                    fname = log$ID,
                                    dolog = TRUE,
                                    sleeplog = log, accid = ID)
    relevant_rows <- idwi$matching_indices_sleeplog
    if (length(relevant_rows) > 0) {
      log <- log[relevant_rows, ]
      if (!is.null(firstDate)) {
        # Add date if missing and remove unneeded columns
        log$date <- firstDate + as.numeric(log$night) - 1
        log <- log[, c("ID", "date", grep(pattern = "onset|wake|bed", x = names(log), value = TRUE))]
      }
      for (i in 1:nrow(log)) {

        tmp <- log[i, ]
        # only attempt if there are at least 2 timestamps to process
        if (ncol(tmp) <= 2) next
        built_this_row <- FALSE
        nonempty <- which(tmp[3:ncol(tmp)] != "" & tmp[3:ncol(tmp)] != "NA")
        if (length(nonempty) > 1) {
          date <- as.Date(tmp[1, 2], format = dateformat, tz = desiredtz)
          times <- as.character(unlist(tmp[1, 3:ncol(tmp)]))
          times <- grep(pattern = "NA", value = TRUE, invert = TRUE, x = times)
          times <- gsub(pattern = " ", replacement = "", x = times)
          times <- times[which(times %in% c("", "NA") == FALSE)]
          # ignore entries without start and/or end time
          t_to_remove <- c()
          for (ji in 1:floor(length(times) / 2)) {
            check <- ((ji * 2) - 1):(ji * 2)
            if (length(which(times[check] == "")) > 0) {
              t_to_remove <- c(t_to_remove, check)
            }
          }
          if (length(t_to_remove) > 0) {
            times <- times[-t_to_remove]
          }
          if (length(times) > 1) {
            Nevents <- floor(length(times) / 2)
            timestamps <- as.POSIXlt(paste0(date, " ", times), tz = desiredtz)
            hour <- as.numeric(format(timestamps, "%H"))
            if (!is.null(firstDate)) {
              # times before noon belong to the next day
              AM <- which(hour <= 12)

              # a wake between noon and 6pm is a day sleeper, also on the next day
              if (hour[2] > 12 & hour[2] <= 18) {
                AM <- c(AM, 2)
              }
              # "11:00 to 09:00" means 11:00 to 09:00 the next morning, not 09:00 to 11:00
              if (hour[1] > hour[2] && 1 %in% AM) {
                AM <- AM[which(AM != 1)]
              }
              if (length(AM) > 0) {
                timestamps[AM] <- as.POSIXlt(paste0(date + 1, " ", times[AM]), tz = desiredtz)
              }
            } else {
              # nap or non-wear crossing midnight (a no-op, as in GGIR)
              if (hour[1] >= 12 & hour[2] < 12) {
                timestamps[2] <- as.POSIXlt(paste0(date, " ", times[2]), tz = desiredtz)
              }
            }
            timestamps <- sort(timestamps)
            logreport_tmp <- data.frame(ID = rep(ID, Nevents),
                                        type = rep(logname, Nevents),
                                        start = rep("", Nevents),
                                        end = rep("", Nevents),
                                        duration = rep(0, Nevents), stringsAsFactors = FALSE)
            for (bi in 1:Nevents) {
              tt1 <- as.POSIXct(timestamps[(bi * 2) - 1], tz = desiredtz)
              tt2 <- as.POSIXct(timestamps[(bi * 2)], tz = desiredtz)
              logreport_tmp$start[bi] <- format(tt1)
              logreport_tmp$end[bi] <- format(tt2)
              if (length(unlist(strsplit(logreport_tmp$start[bi], " "))) == 1) {
                logreport_tmp$start[bi] <- paste0(logreport_tmp$start[bi], " 00:00:00")
              }
              if (length(unlist(strsplit(logreport_tmp$end[bi], " "))) == 1) {
                logreport_tmp$end[bi] <- paste0(logreport_tmp$end[bi], " 00:00:00")
              }
              logreport_tmp$duration[bi] <- abs(as.numeric(difftime(time1 = tt1, time2 = tt2,
                                                                    units = "mins")))
            }
            built_this_row <- TRUE
          }
          if (ggir_exact || built_this_row) {
            if (length(logreport) == 0) {
              logreport <- logreport_tmp
            } else {
              logreport <- rbind(logreport, logreport_tmp)
            }
          }
        }
      }
    }
  }
  return(logreport)
}

#' Report of Sustained Inactivity Bouts and Self-Reported Events
#'
#' Finds every sustained inactivity bout (sib) that part 3 detected during waking hours and
#' reports it with the mean acceleration of the minute before and the minute after. When a
#' sleep diary is supplied, its naps, non-wear windows, sleep times and bed times are appended
#' as rows of the same shape. This is the object GGIR writes to
#' meta/ms5.outraw/sib.reports/sib_report_<file>_<sibDef>.csv, and the input to the part-5
#' rest and nap analysis. GGIR calls it once per sib definition, outside the threshold and
#' timewindow loops.
#'
#' @details duration is minutes, the number of waking epochs spanned divided by
#'   60/epochlength, with no minimum length; mean_acc_1min_before and mean_acc_1min_after are
#'   in the units of ts$ACC, rounded to three decimals. Every index is into the compressed
#'   vector of waking epochs, so a row can straddle a night and a one-minute window can span
#'   hours. firstDate is the calendar date of the first epoch, taken back one day when the
#'   recording starts before 04:00; the sleeplog and bedlog blocks are dated from it and the
#'   nap and non-wear blocks carry their own dates. With imputation codes the sleeplog and
#'   bedlog rows gain an imputecode column. The diary blocks are attached with
#'   base::merge(..., all = TRUE), which sorts the result by ID, type, start, end and
#'   duration, so the sib rows are no longer in time order once a diary is present.
#' @param ts The part-5 time series, a data.frame with time (POSIXct, or ISO 8601 character),
#'   ACC, diur (0 waking, 1 sleep period time) and sibdetection (0 or 1).
#' @param ID Character identifier of the recording. GGIR passes the ms3 file name.
#' @param epochlength Short epoch length in seconds, after the optional aggregation to 60 s.
#' @param logs_diaries The list returned by \code{\link{raw.sleeplog}}, or NULL for no diary.
#' @param desiredtz Timezone for every conversion. "" means the system timezone.
#' @param ggir_exact TRUE, the default, reproduces GGIR's two wrong one-minute-window guards and
#'   its stale logreport_tmp append. FALSE corrects both.
#' @return A data.frame with columns ID, type ("sib", "nap", "nonwear", "sleeplog", "bedlog"),
#'   start and end (POSIXct), duration (minutes), mean_acc_1min_before and mean_acc_1min_after,
#'   plus imputecode when the diary carried imputation codes. The two acceleration columns are
#'   NA on diary rows, and absent when there is no waking sib but the diary supplied rows. With
#'   nothing to report, a zero-row frame with the seven columns; GGIR returns a two-member list
#'   of empty POSIXct vectors there.
#' @seealso \code{\link{raw.sleeplog}} for the diary list, \code{\link{raw.sib}} and
#'   \code{\link{raw.sib.detect}} for the part-3 detection this reports on.
#' @examples
#' ts <- data.frame(time = as.POSIXct("2025-10-07 12:00:00", tz = "UTC") + seq(0, 3595, by = 5),
#'                  ACC = rep(c(50, 2), each = 360),
#'                  diur = rep(0, 720),
#'                  sibdetection = rep(c(0, 1), each = 360))
#' raw.sib.report(ts, ID = "demo", epochlength = 5, desiredtz = "UTC")
#' @export
raw.sib.report <- function(ts, ID, epochlength, logs_diaries = NULL, desiredtz = "",
                           ggir_exact = TRUE) {
  # GGIR indexes a missing column as NULL and reports zero bouts; name the problem instead
  if (!is.data.frame(ts)) {
    stop("raw.sib.report: ts must be a data.frame, the part-5 time series.", call. = FALSE)
  }
  missing_cols <- setdiff(c("time", "ACC", "diur", "sibdetection"), names(ts))
  if (length(missing_cols) > 0) {
    stop(paste0("raw.sib.report: ts is missing the column(s) ",
                paste(missing_cols, collapse = ", "),
                ". The part-5 time series carries time, ACC, diur and sibdetection."),
         call. = FALSE)
  }
  if (length(ID) != 1 || is.na(ID)) {
    stop("raw.sib.report: ID must be a single, non-missing identifier.", call. = FALSE)
  }
  if (length(epochlength) != 1 || !is.numeric(epochlength) || is.na(epochlength) ||
      epochlength <= 0) {
    stop("raw.sib.report: epochlength must be one positive number of seconds.", call. = FALSE)
  }
  ID <- as.character(ID)

  sibreport <- .raw.sib.report.acc(ts = ts, ID = ID, epochlength = epochlength,
                                   desiredtz = desiredtz, ggir_exact = ggir_exact)

  if (length(logs_diaries) > 0) {
    nonwearlog <- logs_diaries$nonwearlog
    naplog <- logs_diaries$naplog
    sleeplog <- logs_diaries$sleeplog
    bedlog <- logs_diaries$bedlog
    imputecodelog <- logs_diaries$imputecodelog
    dateformat <- logs_diaries$dateformat

    firstDate <- as.Date(ts$time[1], tz = desiredtz)
    # when recording starts after midnight and before 4am we count the previous date as the first night
    if (as.numeric(format(ts$time[1], "%H")) < 4) firstDate <- firstDate - 1

    naplogreport <- .raw.sib.report.extract.logs(naplog, ID, logname = "nap",
                                                 dateformat = dateformat, desiredtz = desiredtz,
                                                 ggir_exact = ggir_exact)
    nonwearlogreport <- .raw.sib.report.extract.logs(nonwearlog, ID, logname = "nonwear",
                                                     dateformat = dateformat,
                                                     desiredtz = desiredtz,
                                                     ggir_exact = ggir_exact)
    sleeplogreport <- .raw.sib.report.extract.logs(sleeplog, ID, logname = "sleeplog",
                                                   dateformat = dateformat, desiredtz = desiredtz,
                                                   firstDate = firstDate, ggir_exact = ggir_exact)
    bedlogreport <- .raw.sib.report.extract.logs(bedlog, ID, logname = "bedlog",
                                                 dateformat = dateformat, desiredtz = desiredtz,
                                                 firstDate = firstDate, ggir_exact = ggir_exact)
    # add imputecodelog
    if (length(imputecodelog) > 0) {
      if (length(sleeplogreport) > 0) {
        sleeplogreport <- .raw.sib.report.add.date(sleeplogreport, tz = desiredtz)
      }
      if (length(bedlogreport) > 0) {
        bedlogreport <- .raw.sib.report.add.date(bedlogreport, tz = desiredtz)
      }

      imputecodelog_tmp <- imputecodelog[which(imputecodelog$ID == ID), ]
      if (length(sleeplogreport) > 0) {
        sleeplogreport <- merge(sleeplogreport, imputecodelog_tmp, by = c("ID", "date"))
        sleeplogreport <- sleeplogreport[, which(colnames(sleeplogreport) != "date")]
      }
      if (length(bedlogreport) > 0) {
        bedlogreport <- merge(bedlogreport, imputecodelog_tmp, by = c("ID", "date"))
        bedlogreport <- bedlogreport[, which(colnames(bedlogreport) != "date")]
      }
    }
    logreport <- sibreport
    # append all together in one output data.frame
    if (length(logreport) > 0 & length(naplogreport) > 0) {
      logreport <- merge(logreport, naplogreport,
                         by = c("ID", "type", "start", "end", "duration"), all = TRUE)
    } else if (length(logreport) == 0 & length(naplogreport) > 0) {
      logreport <- naplogreport
    }
    if (length(logreport) > 0 & length(nonwearlogreport) > 0) {
      logreport <- merge(logreport, nonwearlogreport,
                         by = c("ID", "type", "start", "end", "duration"), all = TRUE)
    } else if (length(logreport) == 0 & length(nonwearlogreport) > 0) {
      logreport <- nonwearlogreport
    }
    if (length(logreport) > 0 & length(sleeplogreport) > 0) {
      logreport <- merge(logreport, sleeplogreport,
                         by = c("ID", "type", "start", "end", "duration"), all = TRUE)
    } else if (length(logreport) == 0 & length(sleeplogreport) > 0) {
      logreport <- sleeplogreport
    }
    if (length(logreport) > 0 & length(bedlogreport) > 0) {
      if ("imputecode" %in% colnames(logreport) && "imputecode" %in% colnames(bedlogreport)) {
        include_imputecode <- "imputecode"
      } else {
        include_imputecode <- NULL
      }
      logreport <- merge(logreport, bedlogreport,
                         by = c("ID", "type", "start", "end", "duration", include_imputecode),
                         all = TRUE)
    } else if (length(logreport) == 0 & length(bedlogreport) > 0) {
      logreport <- bedlogreport
    }
  } else {
    logreport <- sibreport
  }
  # add midnight timetimes which got lost
  if (length(logreport) == 0) {
    # GGIR turns a NULL logreport into a two-member list here; return the empty frame instead
    return(data.frame(ID = character(0), type = character(0),
                      start = as.POSIXct(character(0), tz = desiredtz),
                      end = as.POSIXct(character(0), tz = desiredtz),
                      duration = numeric(0),
                      mean_acc_1min_before = numeric(0),
                      mean_acc_1min_after = numeric(0), stringsAsFactors = FALSE))
  }
  logreport$start <- as.POSIXct(unlist(lapply(X = logreport$start,
                                              FUN = .raw.sib.report.convert2posix)),
                                tz = desiredtz)
  logreport$end <- as.POSIXct(unlist(lapply(X = logreport$end,
                                            FUN = .raw.sib.report.convert2posix)),
                              tz = desiredtz)
  return(logreport)
}
