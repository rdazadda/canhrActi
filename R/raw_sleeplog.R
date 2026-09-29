# Ported from GGIR 3.3-9 R/g.loadlog.R and R/g.part4_extractid.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The recording start dates and
# identifiers arrive as arguments instead of a load() of every RData file in
# meta.sleep.folder, the nested closures are file-level internals, the exists("S") and
# exists("B") tests became local flags, read.csv stands in when data.table is not
# installed, and LC_TIME is forced to "C" for the duration of the call.

# HELPERS

#' Read a Sleep Diary csv the Way GGIR Reads It
#'
#' GGIR's \code{data.table::fread} call; \code{utils::read.csv} with the same settings when
#' data.table is not installed (it names unnamed columns differently).
#' @param path Path to the diary csv.
#' @return A data.frame of character columns.
#' @keywords internal
#' @noRd
.raw.sleeplog.read <- function(path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::fread(file = path, stringsAsFactors = FALSE, data.table = FALSE,
                             check.names = TRUE, colClasses = "character"))
  }
  utils::read.csv(file = path, stringsAsFactors = FALSE, check.names = TRUE,
                  colClasses = "character")
}

#' Drop All-Empty Rows and Columns From a Nap or Non-Wear Log Matrix
#'
#' GGIR's \code{remove_empty_rows_cols}. The name vector is built from the trimmed column
#' count and then truncated to \code{ncol}.
#' @param logmatrix Character matrix with ID in column 1 and date in column 2.
#' @param name Column name stem, "nap", "nonwear" or "imputecode".
#' @return A data.frame.
#' @keywords internal
#' @noRd
.raw.sleeplog.trim.rows.cols <- function(logmatrix, name) {
  logmatrix <- as.data.frame(logmatrix[which((rowSums(logmatrix != "") != 0) == TRUE),
                                       which((colSums(logmatrix != "") != 0) == TRUE)])
  logmatrix <- as.data.frame(logmatrix)
  if (length(name) > 0 & nrow(logmatrix) > 0) {
    newnames <- c("ID", "date", rep(paste0(name, 1:ncol(logmatrix)), each = 2))
    colnames(logmatrix) <- newnames[1:ncol(logmatrix)]
  }
  return(logmatrix)
}

#' Drop Empty Rows and Trailing Empty Column Pairs From the Rebuilt Diary Matrix
#'
#' GGIR's \code{removeEmptyCells}. Returns NULL when only the ID column survives.
#' @param x Character matrix, ID in column 1 and timestamp pairs after it.
#' @return A character matrix, or NULL when nothing but the ID column is left.
#' @keywords internal
#' @noRd
.raw.sleeplog.trim.cells <- function(x) {
  if (nrow(x) == 1) {
    if (sum(x[, 2:ncol(x)] == "") == ncol(x) - 1) {
      emptyrows <- 1
    } else {
      emptyrows <- NULL
    }
  } else {
    emptyrows <- which(rowSums(x[, 2:ncol(x)] == "") == ncol(x) - 1)
  }
  if (length(emptyrows)) {
    x <- as.matrix(x[-emptyrows, ])
  }
  if (length(x) != 0) {
    if (ncol(x) == 1 & nrow(x) > 1) {
      x <- t(x)
    }
  }
  emptycols <- which(colSums(x == "") == nrow(x))
  colp <- ncol(x)
  twocols <- c(colp - 1, colp)
  while (min(twocols) > 0) {
    if (all(twocols %in% emptycols)) {
      x <- as.matrix(x[, -twocols])
      if (ncol(x) == 1) x <- t(x)
      twocols <- twocols - 2
    } else {
      break
    }
  }
  if (ncol(x) == 1) x <- NULL
  return(x)
}

# BASIC FORMAT

#' Turn a Wide Diary Into One Row per ID and Night (GGIR adjustLogFormat)
#'
#' Walks the diary columns in pairs starting at \code{coln1}, parses each pair as
#' \code{HH:MM} or \code{HH:MM:SS}, and returns a long data.frame with one row per
#' participant and night.
#'
#' @details The times are re-serialised with \code{paste0(h, ":", m, ":", s)}, which strips
#'   leading zeros ("07:00:00" comes back as "7:0:0"); part 4 re-pads them. A wake time
#'   equal to the onset time fires neither branch, so \code{dur} keeps the previous
#'   participant's value, as in GGIR. A row whose ID is literally "0" is deleted with the
#'   unfilled rows. Rows with a missing onset or wake are dropped entirely, so a blank night
#'   leaves no placeholder row.
#'
#' @param S Diary as a data.frame of character columns.
#' @param nnights Number of night column pairs to allocate for.
#' @param mode "sleeplog" or "bedlog"; selects the names of the two time columns and the
#'   wording of the invalid-timestamp error.
#' @param colid Index of the identifier column.
#' @param coln1 Index of the first timestamp column.
#' @return A data.frame with columns ID, night, duration and either sleeponset/sleepwake or
#'   bedstart/bedend, all character.
#' @keywords internal
#' @noRd
.raw.sleeplog.basic <- function(S, nnights, mode = "sleeplog", colid, coln1) {
  cnt_time_notrecognise <- 0 # never read, as in GGIR
  log <- matrix(0, (nrow(S) * nnights), 3)
  log_times <- matrix(" ", (nrow(S) * nnights), 2)
  cnt <- 1
  sli <- coln1
  wki <- sli + 1
  night <- 1
  while (wki <= ncol(S)) { # nights
    SL <- as.character(S[, sli])
    WK <- as.character(S[, wki])
    for (j in 1:length(SL)) { # participants
      if (is.na(WK[j]) == FALSE & is.na(SL[j]) == FALSE & WK[j] != "" & SL[j] != "") {
        SLN <- as.numeric(unlist(strsplit(SL[j], ":")))
        WKN <- as.numeric(unlist(strsplit(WK[j], ":")))
        if (length(SLN) == 2) SLN <- c(SLN, 0) # add seconds when not stored
        if (length(WKN) == 2) WKN <- c(WKN, 0)
        SL[j] <- paste0(SLN[1], ":", SLN[2], ":", SLN[3])
        WK[j] <- paste0(WKN[1], ":", WKN[2], ":", WKN[3])
        SLN2 <- SLN[1] * 3600 + SLN[2] * 60 + SLN[3]
        WKN2 <- WKN[1] * 3600 + WKN[2] * 60 + WKN[3]
        if (is.na(WKN2)) {
          stop(paste0(WKN[1], " as found in the ", mode, " is not a valid timestamp"), call. = FALSE)
        }
        if (is.na(SLN2)) {
          stop(paste0(SLN[1], " as found in the ", mode, " is not a valid timestamp"), call. = FALSE)
        }
        if (WKN2 > SLN2) { # e.g. 01:00 - 07:00
          dur <- WKN2 - SLN2
        } else if (WKN2 < SLN2) { # e.g. 22:00 - 07:00
          dur <- ((24 * 3600) - SLN2) + WKN2
        }
        dur <- dur / 3600
      } else {
        cnt_time_notrecognise <- cnt_time_notrecognise + 1
        dur <- 0
        is.na(dur) <- TRUE
      }
      if (nrow(log) < cnt) {
        log <- rbind(log, matrix(0, 1, 3))
        log_times <- rbind(log_times, matrix(0, 1, 2))
      }
      log[cnt, 1] <- as.character(S[j, colid])
      log[cnt, 2] <- night
      log[cnt, 3] <- dur
      log_times[cnt, 1] <- SL[j]
      log_times[cnt, 2] <- WK[j]
      cnt <- cnt + 1
    }
    sli <- sli + 2
    wki <- wki + 2
    night <- night + 1
  }
  # unfilled rows, and any row whose ID is literally "0"
  empty_rows <- which(as.character(log[, 1]) == "0")
  if (length(empty_rows) > 0) {
    log <- log[-empty_rows, , drop = FALSE]
    log_times <- log_times[-empty_rows, , drop = FALSE]
  }
  log <- as.data.frame(log, stringsAsFactors = FALSE)
  names(log) <- c("ID", "night", "duration")
  if (mode == "sleeplog") {
    log$sleeponset <- log_times[, 1]
    log$sleepwake <- log_times[, 2]
  } else if (mode == "bedlog") {
    log$bedstart <- log_times[, 1]
    log$bedend <- log_times[, 2]
  }
  # a blank night leaves no row
  log <- log[which(is.na(log$duration) == FALSE), ]
  return(log)
}

# ADVANCED FORMAT

#' Rebuild a Date-Based Diary as a Wide One Aligned to the Recording (GGIR Advanced Sleeplog)
#'
#' Any diary with a column name containing "date" is an advanced diary. The columns are
#' matched by keyword, the date format is sniffed from a list of sixteen, the diary dates are
#' aligned to the recording start date, and the result is a wide matrix in the shape the
#' basic parser expects, plus separate nap, non-wear and imputation-code tables.
#'
#' @details The keyword sets are GGIR's: \code{lightsout|inbed|tobed|bedstart} starts a time
#'   in bed window and \code{lightson|outbed|bedend} ends one, \code{onset} starts a sleep
#'   period and \code{wakeup} ends one, \code{nap}, \code{nonwear} and \code{impute} go to
#'   their own tables. The wake and bed-end columns are looked for after the next date
#'   column, which attaches a wake time to the night that opened on the previous date. With
#'   only one date column nothing here runs. \code{wakeupi} and \code{bedendi} are not reset
#'   on the last day, and the mixed-reporting fix-up mutates the column sets inside the date
#'   loop; both as in GGIR. \code{startAtMidnight} adds one day to \code{deltadate} for a
#'   recording that starts at or before 04:00, because part 3 does not count that first half
#'   night.
#'
#' @param S Diary as read, a data.frame of character columns.
#' @param colid Index of the identifier column.
#' @param startdates data.frame with columns ID, startdate (a Date) and startAtMidnight, or
#'   NULL when the caller supplied no recording start information.
#' @param desiredtz Timezone used for every date conversion.
#' @param dateformat_correct The date format to try first, "%Y-%m-%d" on the first call.
#' @param deltadate Day offset between diary and recording, 0 on entry.
#' @return A list with S, B, has_B, naplog, nonwearlog, imputecodelog, nnights, deltadate,
#'   dateformat_correct, coln1 and colid.
#' @keywords internal
#' @noRd
.raw.sleeplog.advanced <- function(S, colid, startdates, desiredtz,
                                   dateformat_correct = "%Y-%m-%d", deltadate = 0) {
  count <- 1 # row in the new sleeplog matrix
  naplog <- nonwearlog <- newsleeplog <- imputecodelog <- c()
  B <- NULL
  has_B <- FALSE
  coln1 <- NULL
  datecols <- grep(pattern = "date", x = colnames(S), value = FALSE, ignore.case = TRUE)
  nnights <- length(datecols)
  # two or more date columns make an advanced diary
  if (length(datecols) > 1) {
    bedstartcols <- grep(pattern = "lightsout|inbed|tobed|bedstart", x = colnames(S), value = FALSE, ignore.case = TRUE)
    bedendcols <- grep(pattern = "lightson|outbed|bedend", x = colnames(S), value = FALSE, ignore.case = TRUE)
    wakecols <- grep(pattern = "wakeup", x = colnames(S), value = FALSE, ignore.case = TRUE)
    onsetcols <- grep(pattern = "onset", x = colnames(S), value = FALSE, ignore.case = TRUE)
    napcols <- grep(pattern = "nap", x = colnames(S), value = FALSE, ignore.case = TRUE)
    nonwearcols <- grep(pattern = "nonwear", x = colnames(S), value = FALSE, ignore.case = TRUE)
    imputecols <- grep(pattern = "impute", x = colnames(S), value = FALSE, ignore.case = TRUE)
    # the new sleeplog: ID, then onset and wake pairs aligned to the recording
    newsleeplog <- matrix("", nrow(S), max(c(nnights * 2, 100)) + 1)
    newbedlog <- matrix("", nrow(S), max(c(nnights * 2, 100)) + 1)
    naplog <- matrix("", nrow(S) * nnights * 5, 50) # ID, date, start, end
    nonwearlog <- matrix("", nrow(S) * nnights * 5, 50)
    if (length(imputecols) > 0) {
      imputecodelog <- matrix("", nrow(S) * nnights * 2, 50)
    } else {
      imputecodelog <- NULL
    }
    napcnt <- nwcnt <- iccnt <- 1
    IDcouldNotBeMatched <- TRUE
    dateformat_found <- FALSE
    dateformats_to_consider <- c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y", "%Y-%d-%m",
                                 "%y-%m-%d", "%d-%m-%y", "%m-%d-%y", "%y-%d-%m",
                                 "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%d/%m",
                                 "%y/%m/%d", "%d/%m/%y", "%m/%d/%y", "%y/%d/%m")
    for (i in 1:nrow(S)) {
      ID <- S[i, colid]
      if (ID %in% startdates$ID == TRUE) { # an ID with no recording is ignored
        IDcouldNotBeMatched <- FALSE
        matchingID <- which(startdates$ID == ID)
        if (length(matchingID) > 1) {
          warning(paste0("There is more than 1 accelerometer recording for ID ",
                         ID, ", assuming that the first corresponds to the ",
                         "sleeplog entry for this ID"), call. = FALSE)
          matchingID <- matchingID[1]
        }
        startdate_acc <- as.Date(startdates$startdate[matchingID], tz = desiredtz)
        startdate_sleeplog <- as.character(S[i, datecols[1:pmin(length(datecols), 5)]])
        Sdates_correct <- c()
        if (dateformat_found == TRUE && dateformats_to_consider[1] != dateformat_correct) {
          # the format that worked last is tried first
          dateformats_to_consider <- unique(c(dateformat_correct, dateformats_to_consider))
        }
        for (dateformat in dateformats_to_consider) {
          startdate_sleeplog_tmp <- as.Date(startdate_sleeplog[which(startdate_sleeplog != "")],
                                            format = dateformat, tz = desiredtz)
          if (is.null(startdate_sleeplog_tmp)) next
          Sdates <- as.Date(as.character(S[i, datecols]), format = dateformat, tz = desiredtz)
          if (length(which(diff(which(is.na(Sdates))) > 1)) > 0) {
            stop(paste0("\nSleeplog for ID: ", ID, " has missing date(s)"), call. = FALSE)
          }
          if (all(is.na(startdate_sleeplog_tmp) == FALSE)) {
            deltadate <- as.numeric(startdate_sleeplog_tmp - startdate_acc)
            if (all(is.na(deltadate) == FALSE)) {
              if (all(abs(deltadate) < 30)) {
                startdate_sleeplog <- startdate_sleeplog_tmp[1]
                Sdates_correct <- Sdates
                dateformat_correct <- dateformat
                deltadate <- deltadate[1]
                dateformat_found <- TRUE
                break
              }
            }
          }
        }
        if (is.null(Sdates_correct)) {
          warning(paste0("\nSkipping sleeplog row for ID ", ID,
                         " because it has no date(s) or",
                         " date range sleeplog does not overlap with",
                         " date range accelerometer. Please check",
                         " diary dates are correct."), call. = FALSE)
          next
        }
        if (deltadate > 300) {
          warning(paste0("For ID ", ID, " the sleeplog start date is more than 300 days separated ",
                         "from the dates in the accelerometer recording, this may indicate a ",
                         "problem with date formats or their recognition, please check."), call. = FALSE)
        }
        if (startdates$startAtMidnight[matchingID] == TRUE) {
          # a recording that starts at midnight counts that first night, so the diary is one
          # further day behind it
          deltadate <- deltadate + 1
        }

        if (length(Sdates_correct) == 0 | is.na(startdate_sleeplog) == TRUE) {
          warning(paste0("\nSleeplog for ID: ", ID, " not used because first date",
                         " not within 30 days of first date in accerometer recording"), call. = FALSE)
        } else {
          # missing dates widen the matrices
          ndates <- as.numeric(diff(range(Sdates_correct[!is.na(Sdates_correct)]))) + 1
          if (ndates > 300) {
            warning(paste0("For ID ", ID, " the sleeplog has has ",
                           "more than 300 missing dates, this may ",
                           "indicate a problem with date format ",
                           "recognition. Please check."), call. = FALSE)
          }
          if (ndates > nnights) {
            extraColumns <- matrix("", nrow(newsleeplog), max(c((ndates - nnights) * 2, 100)) + 1)
            newsleeplog <- cbind(newsleeplog, extraColumns)
            newbedlog <- cbind(newbedlog, extraColumns)
            extraColumns <- matrix("", nrow(naplog), max(c((ndates - nnights) * 2, 100)) + 1)
            naplog <- cbind(naplog, extraColumns)
            nonwearlog <- cbind(nonwearlog, extraColumns)
            nnights <- ndates
          }
          if (startdate_sleeplog - deltadate > startdate_sleeplog + nnights) {
            warning(paste0("Accelerometer recording for ID ",
                           ID, " does not overlap with sleeplog date",
                           " range"), call. = FALSE)
            next
          }
          if (count > nrow(newsleeplog)) {
            newsleeplog <- rbind(newsleeplog, matrix(NA, 1, ncol(newsleeplog)))
          }
          if (count > nrow(newbedlog)) {
            newbedlog <- rbind(newbedlog, matrix(NA, 1, ncol(newbedlog)))
          }
          newsleeplog[count, 1] <- ID
          newbedlog[count, 1] <- ID
          newsleeplog_times <- newbedlog_times <- rep("time", 100)
          newCounter <- 1
          expected_dates <- seq(startdate_sleeplog - deltadate, startdate_sleeplog + nnights, by = 1)
          for (ni in 1:(length(expected_dates) - 1)) {
            ind <- which(Sdates_correct == as.Date(expected_dates[ni], tz = desiredtz))
            if (length(ind) > 0) {
              if (length(ind) > 1) {
                duplicatedDate <- unique(as.character(S[i, datecols[ind]]))
                stop(paste0("\n", ID, " has duplicate dates in the diary, please fix ", duplicatedDate),
                     call. = FALSE)
              }
              curdatecol <- datecols[ind]
              nextdatecol <- datecols[which(datecols > curdatecol)[1]]
              doublenextdatecol <- datecols[which(datecols > nextdatecol)[1]]
              lastday <- FALSE
              if (is.na(nextdatecol)) {
                nextdatecol <- ncol(S) + 1
                lastday <- TRUE
              }
              if (is.na(doublenextdatecol)) {
                doublenextdatecol <- ncol(S) + 1
              }
              # mixed reporting: bedstart with wakeup is time in bed, bedend with onset is SPT
              if (length(bedendcols) == 0 & length(bedstartcols) != 0 &
                  length(onsetcols) == 0 & length(wakecols) != 0) {
                bedendcols <- wakecols
                wakecols <- NULL
              }
              if (length(bedendcols) != 0 & length(bedstartcols) == 0 &
                  length(onsetcols) != 0 & length(wakecols) == 0) {
                wakecols <- bedendcols
                bedendcols <- NULL
              }
              # sleep log
              onseti <- onsetcols[which(onsetcols > curdatecol & onsetcols < nextdatecol)]
              if (lastday == FALSE) {
                wakeupi <- wakecols[which(wakecols > nextdatecol & wakecols < doublenextdatecol)[1]]
                wakeuptime <- S[i, wakeupi]
              } else {
                wakeuptime <- ""
              }
              if (length(onseti) == 1 & length(wakeupi) == 1) {
                newsleeplog_times[newCounter:(newCounter + 1)] <- c(S[i, onseti], wakeuptime)
              } else {
                newsleeplog_times[newCounter:(newCounter + 1)] <- c("", "")
              }
              # time in bed
              bedstarti <- bedstartcols[which(bedstartcols > curdatecol & bedstartcols < nextdatecol)]
              if (lastday == FALSE) {
                bedendi <- bedendcols[which(bedendcols > nextdatecol & bedendcols < doublenextdatecol)[1]]
                bedendtime <- S[i, bedendi]
              } else {
                bedendtime <- ""
              }
              if (length(bedstarti) == 1 & length(bedendi) == 1) {
                newbedlog_times[newCounter:(newCounter + 1)] <- c(S[i, bedstarti], bedendtime)
              } else {
                newbedlog_times[newCounter:(newCounter + 1)] <- c("", "")
              }
              # nap, non-wear and imputation code columns go to their own matrices
              naps <- napcols[which(napcols > curdatecol & napcols < nextdatecol)]
              nonwears <- nonwearcols[which(nonwearcols > curdatecol & nonwearcols < nextdatecol)]
              imputecodes <- imputecols[which(imputecols > nextdatecol & imputecols < doublenextdatecol)[1]]
              if (is.na(imputecodes)) imputecodes <- NULL
              if (length(naps) > 0) {
                naplog[napcnt, 1] <- ID
                naplog[napcnt, 2] <- S[i, curdatecol]
                naplog[napcnt, 3:(2 + length(naps))] <- as.character(S[i, naps])
                napcnt <- napcnt + 1
              }
              if (length(nonwears) > 0) {
                nonwearlog[nwcnt, 1] <- ID
                nonwearlog[nwcnt, 2] <- S[i, curdatecol]
                nonwearlog[nwcnt, 3:(2 + length(nonwears))] <- as.character(S[i, nonwears])
                nwcnt <- nwcnt + 1
              }
              if (length(imputecodes) > 0 && imputecodes <= ncol(S)) {
                imputecodelog[iccnt, 1] <- ID
                imputecodelog[iccnt, 2] <- S[i, curdatecol]
                imputecodelog[iccnt, 3:(2 + length(imputecodes))] <-
                  gsub("[^0-9.-]", "", as.character(S[i, imputecodes]))
                iccnt <- iccnt + 1
              }
            } else {
              newsleeplog_times[newCounter:(newCounter + 1)] <- c("", "")
              newbedlog_times[newCounter:(newCounter + 1)] <- c("", "")
            }
            newCounter <- newCounter + 2
            if (newCounter > length(newbedlog_times) - 5) {
              newbedlog_times <- c(newbedlog_times, rep("time", 100))
              newsleeplog_times <- c(newsleeplog_times, rep("time", 100))
            }
          }
          newsleeplog_times <- newsleeplog_times[which(newsleeplog_times != "time")]
          newbedlog_times <- newbedlog_times[which(newbedlog_times != "time")]
          extracols <- (length(newsleeplog_times) + 2) - ncol(newsleeplog)
          if (extracols > 0) {
            newsleeplog <- cbind(newsleeplog, matrix(NA, nrow(newsleeplog), extracols))
          }
          newsleeplog[count, 2:(length(newsleeplog_times) + 1)] <- newsleeplog_times

          extracols <- (length(newbedlog_times) + 2) - ncol(newbedlog)
          if (extracols > 0) {
            newbedlog <- cbind(newbedlog, matrix(NA, nrow(newbedlog), extracols))
          }
          newbedlog[count, 2:(length(newbedlog_times) + 1)] <- newbedlog_times

          count <- count + 1
        }
      }
    }
    if (IDcouldNotBeMatched == TRUE) {
      warning(paste0("\nNone of the IDs in the accelerometer data could be matched with",
                     " the ID numbers in the sleeplog. You may want to check that the ID",
                     " format in your sleeplog is consistent with the ID column in the GGIR part2 csv-report,",
                     " and that argument coldid is correctly set."), call. = FALSE)
    }
    # remove empty rows and columns
    if (length(naplog) > 0) {
      naplog <- .raw.sleeplog.trim.rows.cols(naplog, name = "nap")
    }
    if (length(nonwearlog) > 0) {
      nonwearlog <- .raw.sleeplog.trim.rows.cols(nonwearlog, name = "nonwear")
    }
    if (length(imputecodelog) > 0) {
      imputecodelog <- .raw.sleeplog.trim.rows.cols(imputecodelog, name = "imputecode")
      colnames(imputecodelog)[3] <- "imputecode"
      imputecodelog$date <- as.Date(imputecodelog$date, dateformat_correct)
    }

    if (length(newsleeplog) > 0) {
      newsleeplog <- .raw.sleeplog.trim.cells(newsleeplog)
      if (!is.null(newsleeplog)) {
        S <- as.data.frame(newsleeplog)
      }
      coln1 <- 2
      colid <- 1
    }
    if (length(newbedlog) > 0) {
      newbedlog <- .raw.sleeplog.trim.cells(newbedlog)
      if (!is.null(newbedlog)) {
        B <- as.data.frame(newbedlog)
        has_B <- TRUE
      }
      coln1 <- 2
      colid <- 1
    }
  }
  return(list(S = S, B = B, has_B = has_B, naplog = naplog, nonwearlog = nonwearlog,
              imputecodelog = imputecodelog, nnights = nnights, deltadate = deltadate,
              dateformat_correct = dateformat_correct, coln1 = coln1, colid = colid))
}

# THE ENTRY POINT

#' Read a Sleep Diary (GGIR g.loadlog)
#'
#' Reads a sleep diary csv in either of the two formats GGIR accepts and returns the six
#' member list that the night summary consumes. A diary with no column name containing
#' "date" is the basic wide format: an identifier column followed by alternating
#' onset and wake times, one pair per night. A diary with a "date" column is the advanced
#' format, where each day carries its own date and the columns are matched by keyword, and
#' which can also carry nap, non-wear and imputation-code columns.
#'
#' @details The parsing, the keyword sets, the sixteen date formats and every warning and
#'   error string are GGIR's. The recording identifiers and start times are the \code{id}
#'   and \code{rec_starttime} arguments rather than a \code{load()} of every RData file in
#'   \code{meta.sleep.folder}; without them an advanced diary warns and matches nothing.
#'   GGIR's RData cache of the parsed diary is dropped. The times come back with leading
#'   zeros stripped ("07:00:00" as "7:0:0") and the night summary re-pads them. Rows with a
#'   missing onset or wake time are dropped entirely. \code{LC_TIME} is set to "C" for the
#'   duration of the call, as \code{GGIR()} sets it for the session.
#'
#' @param path Path to the diary csv.
#' @param colid Column index of the participant identifier. Default 1.
#' @param coln1 Column index of the first timestamp. Default 2. Ignored for an advanced
#'   diary, which sets it to 2 itself.
#' @param sleepwindowType Either "SPT" (the diary reports sleep onset and wake) or
#'   "TimeInBed" (it reports bed times). For a basic diary this decides whether the table
#'   comes back as \code{sleeplog} or as \code{bedlog}. Default "SPT".
#' @param desiredtz Timezone used for every date conversion. Default "", meaning the system
#'   timezone, which is what GGIR passes through as well.
#' @param rec_starttime Character vector of recording start times, one per recording, in
#'   ISO 8601 "%Y-%m-%dT%H:%M:%S%z" form, as stored by part 1. Only used by the
#'   advanced format.
#' @param id Character vector of recording identifiers, parallel to \code{rec_starttime}.
#'   Only used by the advanced format.
#' @return A list with six members.
#'   \describe{
#'     \item{sleeplog}{data.frame of ID, night, duration, sleeponset, sleepwake, or NULL.}
#'     \item{nonwearlog}{data.frame of ID, date and non-wear time pairs, or an empty vector.}
#'     \item{naplog}{data.frame of ID, date and nap time pairs, or an empty vector.}
#'     \item{bedlog}{data.frame of ID, night, duration, bedstart, bedend, or NULL.}
#'     \item{imputecodelog}{data.frame of ID, date and imputecode, or NULL.}
#'     \item{dateformat}{The date format that was recognised, "%Y-%m-%d" by default.}
#'   }
#' @seealso \code{\link{read.raw.accelerometer}} for the recording object that supplies
#'   \code{id} and \code{rec_starttime}.
#' @export
raw.sleeplog <- function(path, colid = 1, coln1 = 2, sleepwindowType = "SPT",
                         desiredtz = "", rec_starttime = NULL, id = NULL) {
  # C time locale for the call; GGIR() sets it for the whole session
  LC_TIME_backup <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", LC_TIME_backup), add = TRUE)

  dateformat_correct <- "%Y-%m-%d"
  deltadate <- 0
  S <- .raw.sleeplog.read(path)
  has_S <- TRUE
  B <- NULL
  has_B <- FALSE
  if (any(duplicated(S[, colid]))) {
    duplicatedIDs <- paste0(S[duplicated(S[, colid]), colid], collapse = " ")
    duplicatedIDs <- gsub(pattern = " ", replacement = "", x = duplicatedIDs)
    if (duplicatedIDs != "") {
      stop(paste0("Sleeplog has duplicated entries (rows) for ID(s) ",
                  duplicatedIDs,
                  ", please fix. GGIR expects one sleeplog row per unique ID. "), call. = FALSE)
    } else {
      # duplicated rows without an ID
      S <- S[!duplicated(S[, colid]), ]
    }
  }

  if (colnames(S)[1] == "V1" && any(S[1, ] == "")) {
    stop(paste0("Sleeplog column found with empty header, please fix. This can also happen if ",
                "there are empty columns at the end, delete those columns if applicable."), call. = FALSE)
  }
  advanced_sleeplog <- length(grep(pattern = "date", x = colnames(S), ignore.case = TRUE)) > 0
  startdates <- NULL
  if (advanced_sleeplog == TRUE) {
    if (length(rec_starttime) > 0 && length(id) > 0) {
      # GGIR builds this frame from the RData files in meta.sleep.folder
      startdates <- data.frame(ID = as.character(id),
                               rec_starttime = as.character(rec_starttime),
                               stringsAsFactors = FALSE)

      startdates$startAtMidnight <- FALSE
      # a recording that starts at or before 4am has its first half night counted in part 3,
      # so its start date is not the start date of the nights
      starthour <- as.numeric(format(as.POSIXct(x = startdates$rec_starttime,
                                                format = "%Y-%m-%dT%H:%M:%S%z", tz = desiredtz),
                                     format = "%H"))
      startdates$startAtMidnight[which(starthour <= 4)] <- TRUE
      colnames(startdates)[1:2] <- c("ID", "startdate")
      startdates$startdate <- as.Date(.raw.iso8601.to.posix(startdates$startdate, tz = desiredtz),
                                      tz = desiredtz)
    } else {
      warning("\nArguments rec_starttime and id have not been specified")
    }
  }
  if (length(S) == 0) {
    warning(paste0("Could not read sleeplog file, check that file path is correct.",
                   "Tip: Try to aply function g.loadlog to your sleeplog file first",
                   " to verify that sleeplog is correctly processed."), call. = FALSE)
  } else {
    if (nrow(S) == 0 | ncol(S) <= 2) {
      warning(paste0("Could not read sleeplog file. Does it have at least 3 columns",
                     " and comma seperated values?",
                     " Tip: Try to aply function g.loadlog to your sleeplog file ",
                     "first to verify that sleeplog is correctly processed."), call. = FALSE)
    }
  }
  naplog <- nonwearlog <- imputecodelog <- c()
  if (advanced_sleeplog == TRUE) {
    adv <- .raw.sleeplog.advanced(S = S, colid = colid, startdates = startdates,
                                  desiredtz = desiredtz,
                                  dateformat_correct = dateformat_correct,
                                  deltadate = deltadate)
    S <- adv$S
    B <- adv$B
    has_B <- adv$has_B
    naplog <- adv$naplog
    nonwearlog <- adv$nonwearlog
    imputecodelog <- adv$imputecodelog
    nnights <- adv$nnights
    deltadate <- adv$deltadate
    dateformat_correct <- adv$dateformat_correct
    if (!is.null(adv$coln1)) coln1 <- adv$coln1
    if (!is.null(adv$coln1)) colid <- adv$colid
  } else {
    nnights <- (ncol(S) - coln1 + 1) / 2
  }
  # an odd number of timestamp columns gives nnights %% 2 == 0.5
  if (nnights %% 2 == 0.5) {
    warning(paste0("\nWe see an odd number of timestamp columns",
                   " in the sleeplog. The last column will be ignored. If this is incorrect,",
                   " please check that argument coln1 is correctly specified if you use a basic sleeplog format and",
                   " that all days have a date column if you use an advanced sleeplog format."), call. = FALSE)
    nnights <- floor(nnights)
  }
  nnights <- nnights + deltadate + 1 # a possible extra night at the start of the recording
  if (!is.null(sleepwindowType) && advanced_sleeplog == FALSE) {
    if (sleepwindowType == "TimeInBed") {
      B <- S
      has_B <- TRUE
      S <- NULL
      has_S <- FALSE
    }
  }
  if (has_S && ncol(S) > 0 && nnights > 0) {
    sleeplog <- .raw.sleeplog.basic(S, nnights, mode = "sleeplog", colid = colid, coln1 = coln1)
  } else {
    sleeplog <- NULL
  }
  if (has_B && ncol(B) > 0 && nnights > 0) {
    bedlog <- .raw.sleeplog.basic(B, nnights, mode = "bedlog", colid = colid, coln1 = coln1)
  } else {
    bedlog <- NULL
  }
  invisible(list(sleeplog = sleeplog, nonwearlog = nonwearlog, naplog = naplog, bedlog = bedlog,
                 imputecodelog = imputecodelog,
                 dateformat = dateformat_correct))
}

# IDENTIFIER EXTRACTION

#' Identifier From a File Name and Its Matching Diary Rows (GGIR g.part4_extractid)
#'
#' When the part-3 object carried no identifier, derive one from the file name according to
#' \code{idloc}: 2 takes the first token before "_", 5 before " ", 6 before ".", 7 before
#' "-", and any other value strips before "_", " ", ".RDa" and ".cs" in that order. Then, if
#' a diary is in use, find its rows for that identifier: exact after removing spaces, then
#' case insensitive, then with letters removed, then with leading zeros removed, each
#' attempt only when the previous found nothing. Attempts 3 and 4 rewrite the strings in
#' place, as in GGIR. More than one distinct match warns and keeps the first.
#'
#' @param idloc GGIR's identifier location code.
#' @param fname File name to derive the identifier from.
#' @param dolog TRUE when a diary is in use.
#' @param sleeplog The diary data.frame, needs an ID column. Ignored when \code{dolog} is
#'   FALSE.
#' @param accid Identifier already known from the recording, or \code{c()} to derive one.
#' @return A list with \code{accid} and \code{matching_indices_sleeplog}.
#' @keywords internal
#' @noRd
.raw.sleeplog.extractid <- function(idloc, fname, dolog, sleeplog, accid = c()) {
  if (length(accid) == 0) {
    # identifier from the file name
    if (idloc %in% c(2, 5, 6, 7) == TRUE) {
      if (idloc == 2) {
        getIDfromChar <- function(x) {
          return(as.character(unlist(strsplit(x, "_")))[1])
        }
      } else if (idloc == 5) {
        getIDfromChar <- function(x) {
          return(as.character(unlist(strsplit(x, " ")))[1])
        }
      } else if (idloc == 6) {
        getIDfromChar <- function(x) {
          return(as.character(unlist(strsplit(x, "[.]")))[1])
        }
      } else if (idloc == 7) {
        getIDfromChar <- function(x) {
          return(as.character(unlist(strsplit(x, "-")))[1])
        }
      }
      accid <- apply(as.matrix(as.character(fname)), MARGIN = c(1), FUN = getIDfromChar)
    } else {
      newaccid <- fname
      if (length(unlist(strsplit(newaccid, "_"))) > 1) newaccid <- unlist(strsplit(newaccid, "_"))[1]
      if (length(unlist(strsplit(newaccid, " "))) > 1) newaccid <- unlist(strsplit(newaccid, " "))[1]
      if (length(unlist(strsplit(newaccid, "[.]RDa"))) > 1) newaccid <- unlist(strsplit(newaccid, "[.]RDa"))[1]
      if (length(unlist(strsplit(newaccid, "[.]cs"))) > 1) newaccid <- unlist(strsplit(newaccid, "[.]cs"))[1]
      accid <- newaccid[1]
    }
  }
  # matching diary rows
  if (dolog == TRUE) {

    logid <- as.character(sleeplog$ID)
    accid2 <- as.character(accid)

    # some brands pad the ID with spaces
    logid <- gsub(pattern = " ", replacement = "", x = as.character(logid))
    accid2 <- gsub(pattern = " ", replacement = "", x = as.character(accid2))

    # attempt 1: identical
    matching_indices_sleeplog <- which(logid == accid2)
    matched <- length(matching_indices_sleeplog)
    matched_unique <- unique(sleeplog$ID[matching_indices_sleeplog])

    # attempt 2: ignore case
    if (matched == 0) {
      matching_indices_sleeplog <- which(tolower(logid) == tolower(accid2))
      matched <- length(matching_indices_sleeplog)
      matched_unique <- unique(sleeplog$ID[matching_indices_sleeplog])
    }

    # attempt 3: letters removed
    if (matched == 0) {
      accid2 <- gsub("[^0-9.-]", "", accid2)
      logid <- gsub("[^0-9.-]", "", sleeplog$ID)
      matching_indices_sleeplog <- which(logid == accid2)
      matched <- length(matching_indices_sleeplog)
      matched_unique <- unique(sleeplog$ID[matching_indices_sleeplog])
    }

    # attempt 4: leading zeros removed
    if (matched == 0) {
      accid2 <- gsub("^0+", "", accid2)
      logid <- gsub("^0+", "", logid)
      matching_indices_sleeplog <- which(logid == accid2)
      matched <- length(matching_indices_sleeplog)
      matched_unique <- unique(sleeplog$ID[matching_indices_sleeplog])
    }

    # more than one diary entry matched
    if (length(matched_unique) > 1) {
      warning(paste0("\n", as.character(accid), " matched to more than one entrance ",
                     "in the sleeplog (i.e., ", paste(as.character(matched_unique), collapse = ", "),
                     ").\nPlease revise the IDs in your sleeplog. ", matched_unique[1], " used."))
      matching_indices_sleeplog <- which(sleeplog$ID == matched_unique[1])
    }
  } else if (dolog == FALSE) {
    matching_indices_sleeplog <- 1
  }
  invisible(list(accid = accid, matching_indices_sleeplog = matching_indices_sleeplog))
}
