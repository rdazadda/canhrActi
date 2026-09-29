# Ported from GGIR 3.3-9 R/g.sib.sum.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. g.sib.sum became raw.sib.summary, SLE
# and M became canhrActi objects, three input guards were added in front of the
# transcription, and LC_TIME is forced to "C" for the call. Every number-producing line
# is transcribed as it stands, including the 1000-row pre-allocation and its row names.

#' The Per-Epoch Part-3 Series Behind a Sustained Inactivity Bout Object
#'
#' @param sib A canhrActi_raw_sib from \code{raw.sib.detect()}, any list with an \code{output}
#'   member (GGIR's SLE), or the per-epoch data frame itself.
#' @return The per-epoch data frame, validated.
#' @keywords internal
#' @noRd
.raw.sib.summary.frame <- function(sib) {
  out <- sib
  if (is.list(sib) && !is.data.frame(sib)) {
    if (!"output" %in% names(sib)) {
      stop("sib must be a canhrActi_raw_sib from raw.sib.detect(), a list with an output member, ",
           "or the per-epoch data frame itself", call. = FALSE)
    }
    out <- sib$output
  }
  if (!is.data.frame(out)) {
    out <- as.data.frame(out, stringsAsFactors = FALSE)
  }
  nms <- colnames(out)
  absent <- setdiff(c("time", "invalid", "night"), nms)
  if (length(absent) > 0) {
    stop("the sustained inactivity bout series has no column named ",
         paste0('"', absent, '"', collapse = " or "),
         "; raw.sib.summary needs the g.sib.det layout time, invalid, night, <definitions>",
         call. = FALSE)
  }
  # every column right of night is a sib definition, so a leftover crude estimate would add
  # a whole extra definition
  if ("spt_crude_estimate" %in% nms) {
    stop("the sustained inactivity bout series still carries the column \"spt_crude_estimate\"; ",
         "drop it before summarising, as g.part3.R:102-104 does. g.sib.sum counts it as a sib ",
         "definition and returns extra rows.", call. = FALSE)
  }
  if (which(nms == "night") >= ncol(out)) {
    stop("the sustained inactivity bout series has no sib definition column to the right of ",
         "\"night\"", call. = FALSE)
  }
  out
}

#' The Two Things g.sib.sum Reads From M
#'
#' @param meta A canhrActi_raw, a canhrActi_raw_meta from \code{raw.getmeta()}, or a GGIR M list.
#' @return List with \code{timestamp} (the first metashort timestamp, character) and
#'   \code{windowsizes}.
#' @keywords internal
#' @noRd
.raw.sib.summary.meta <- function(meta) {
  if (inherits(meta, "canhrActi_raw")) meta <- meta$meta
  if (!is.list(meta) || !all(c("metashort", "windowsizes") %in% names(meta))) {
    stop("meta must be a canhrActi_raw, a canhrActi_raw_meta from raw.getmeta(), or a GGIR M list",
         call. = FALSE)
  }
  ms <- meta$metashort
  if (is.null(ms) || nrow(ms) == 0 || !"timestamp" %in% colnames(ms)) {
    stop("meta has no metashort timestamp column", call. = FALSE)
  }
  # GGIR reads the part-1 metashort here, not the imputed one; the timestamps are the same
  list(timestamp = as.character(ms$timestamp[1]), windowsizes = meta$windowsizes)
}

#' Summarise Sustained Inactivity Bouts per Night (GGIR g.sib.sum)
#'
#' Turns the per-epoch part-3 series produced by \code{\link{raw.sib.detect}} into GGIR's
#' \code{sib.cla.sum}: one row per (night, sib definition, sib period), nine columns. This is
#' stage 7 of GGIR part 3, and the table is the only input part 4 has.
#'
#' @details Kept as GGIR has it. \code{invalid} is copied out before the \code{ignorenonwear}
#'   blanking, which zeroes every column except \code{time} and \code{night} on invalid epochs
#'   and so splits sib runs. Every column to the right of \code{night} is a sib definition, so
#'   \code{spt_crude_estimate} must already be dropped. Epochs after the last night are
#'   relabelled -1 and trimmed; a leading night 0 survives unless the recording started after
#'   04:00, a test made with \code{as.Date()} on a POSIXct, which converts in UTC.
#'   \code{fraction.night.invalid} is (invalid epochs + epochs short of a calendar day) over a
#'   full calendar day, capped at 1. \code{tot.sib.dur.hrs} is minutes rounded to two decimals,
#'   divided by 60. A night with no sib period gets no row at all, so the table can be missing
#'   nights; \code{\link{raw.sleep.part3}} carries a complete per-night table alongside it.
#'   Row names are the pre-allocation indices, which \code{identical()} against a stored
#'   milestone compares.
#'
#'   \code{start.time.day} is the first epoch of the night window, repeated on every row of
#'   the night; \code{nsib.periods} is the night total, repeated; \code{tot.sib.dur.hrs} is the
#'   duration of this period; \code{sib.end.time} is the last epoch of the period, so the
#'   interval is closed at both ends.
#'
#' @param sib The per-epoch part-3 series: a \code{canhrActi_raw_sib} from
#'   \code{\link{raw.sib.detect}}, any list with an \code{output} member (GGIR's \code{SLE}), or
#'   the data frame itself. The columns must be \code{time}, \code{invalid}, \code{night} and
#'   then one 0/1 column per sib definition, with \code{spt_crude_estimate} already dropped.
#' @param meta The epoch tables: a \code{canhrActi_raw}, a \code{canhrActi_raw_meta} from
#'   \code{\link{raw.getmeta}}, or a GGIR \code{M} list. Only the first \code{metashort}
#'   timestamp and \code{windowsizes} are read.
#' @param ignorenonwear Logical, GGIR's \code{ignorenonwear} (default TRUE): blank every column
#'   except \code{time} and \code{night} on invalid epochs.
#' @param desiredtz Olson timezone name; the empty string means the system timezone.
#' @return A data frame with nine columns and one row per (night, definition, sib period):
#'   \code{night} (numeric), \code{definition} (character), \code{start.time.day} (character),
#'   \code{nsib.periods} (numeric), \code{tot.sib.dur.hrs} (numeric),
#'   \code{fraction.night.invalid} (numeric), \code{sib.period} (numeric),
#'   \code{sib.onset.time} (character) and \code{sib.end.time} (character). Row names are the
#'   pre-allocation indices. A recording with no sib period at all returns a zero-row frame with
#'   those same nine columns and classes.
#' @seealso \code{\link{raw.sib.detect}} for the input and \code{\link{raw.sib}} for the
#'   detection itself.
#' @examples
#' # Two whole nights at a 60 s epoch with one bout of 90 minutes in each
#' tt <- seq(as.POSIXct("2024-01-01 12:00:00", tz = "UTC"), by = 60, length.out = 2880)
#' iso <- strftime(tt, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
#' hm <- format(tt, "%H:%M", tz = "UTC")
#' epochs <- data.frame(time = iso, invalid = 0, night = rep(1:2, each = 1440),
#'                      T5A5 = as.numeric(hm >= "23:00" | hm < "00:30"))
#' meta <- list(metashort = data.frame(timestamp = iso), windowsizes = c(60, 900, 3600))
#' raw.sib.summary(list(output = epochs), meta, ignorenonwear = TRUE, desiredtz = "UTC")
#' @export
raw.sib.summary <- function(sib, meta, ignorenonwear = TRUE, desiredtz = "") {
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  output <- .raw.sib.summary.frame(sib)
  M <- .raw.sib.summary.meta(meta)

  A <- as.data.frame(output, stringsAsFactors = TRUE)
  invalid <- A$invalid # captured before the blanking, which zeroes A$invalid too
  if (ignorenonwear == TRUE) {
    if (length(which(A$invalid == 1)) > 0) {
      A[which(A$invalid == 1), which(colnames(A) %in% c("time", "night") == FALSE)] <- 0
    }
  }
  space <- ifelse(length(unlist(strsplit(format(A$time[1]), " "))) > 1, TRUE, FALSE)
  time <- format(unlist(A$time))
  #time stored as iso8601 what makes it sensitive to daylight saving days.
  if (space == TRUE) {
    time <- .raw.posix.to.iso8601(as.POSIXlt(time, tz = desiredtz), tz = desiredtz)
  }
  night <- A$night
  sleep <- as.data.frame(as.matrix(A[, (which(colnames(A) == "night") + 1):ncol(A)]),
                         stringsAsFactors = TRUE)
  colnames(sleep) <- colnames(A)[(which(colnames(A) == "night") + 1):ncol(A)]
  ws3 <- M$windowsizes[1]
  ws2 <- M$windowsizes[2] # unused, as in GGIR
  sib.cla.sum <- as.data.frame(matrix(-1, 1000, 9))
  # label night after last night as -1 to differentiate it from night 0
  lastNightEnd <- max(which(night == max(night)))
  if (length(night) > lastNightEnd) {
    night[(lastNightEnd + 1):length(night)] <- -1
  }
  un <- unique(night) #unique nights
  missingnights <- which(un == -1 | is.na(un) == TRUE)
  if (length(missingnights) > 0) {
    un <- un[-missingnights]
  }
  # if recording starts after 4am, then also remove first night
  firstTS_posix <- .raw.iso8601.to.posix(M$timestamp, tz = desiredtz)
  # as.Date() on a POSIXct converts in UTC, not in desiredtz, as in GGIR
  firstday4am_posix <- as.POSIXct(paste0(as.Date(firstTS_posix), " 04:00:00"), tz = desiredtz)
  if (firstday4am_posix < firstTS_posix) { # then first timestamp is later than 4am
    missingnights <- which(un == 0)
    if (length(missingnights) > 0) {
      un <- un[-missingnights]
    }
  }
  if (length(un) != 0) {
    if (is.numeric(max(un)) == TRUE) {
      cnt <- 1
      for (i in un) {
        qqq1 <- which(night == i)[1]
        qqq2 <- which(night == i)[length(which(night == i))]
        if (length(qqq1) == 1 & length(qqq2) == 1) {
          time.t <- time[qqq1:qqq2]
          sleep.t <- as.data.frame(sleep[qqq1:qqq2, ], stringsAsFactors = TRUE)
          colnames(sleep.t) <- colnames(sleep)
          invalid.t <- invalid[qqq1:qqq2]
          for (j in 1:ncol(sleep.t)) { #sleep definitions
            nsleepperiods <- length(which(diff(c(0, sleep.t[, j])) == 1))
            if (nsleepperiods > 0) {
              start_sp <- which(diff(c(0, sleep.t[, j])) == 1)
              end_sp <- which(diff(c(sleep.t[, j], 0)) == -1)
              if (length(end_sp) == 0) end_sp <- nrow(sleep.t) #if sleep period ends the next 'nightday'
              if (start_sp[1] > end_sp[1]) { #if period starts with sleep
                start_sp <- c(1, start_sp)
              }
              if (start_sp[length(start_sp)] > end_sp[length(end_sp)]) { #if period ends with sleep
                end_sp <- c(end_sp, nrow(sleep.t))
              }
              nsleepperiods <- length(start_sp)
            }
            colnames(sib.cla.sum)[1:9] <- c("night", "definition", "start.time.day",
                                            "nsib.periods", "tot.sib.dur.hrs",
                                            "fraction.night.invalid",
                                            "sib.period", "sib.onset.time",
                                            "sib.end.time")
            NepochsInDay <- (60 / ws3) * 1440
            Nmissingvalues <- max(c(0, (NepochsInDay - length(invalid.t))))
            fraction.night.invalid <- (length(which(invalid.t != 0)) + Nmissingvalues) / NepochsInDay
            if (fraction.night.invalid > 1) fraction.night.invalid <- 1 # day with 25 hours
            if (nsleepperiods == 0) {
              sib.cla.sum[cnt, 1] <- i #night
              sib.cla.sum[cnt, 2] <- colnames(sleep.t)[j] #definition
              sib.cla.sum[cnt, 3] <- format(time.t[1])
              sib.cla.sum[cnt, 4] <- nsleepperiods #number of sleep periods
              sib.cla.sum[cnt, 5] <- 0 #total sleep duration
              sib.cla.sum[cnt, 6] <- fraction.night.invalid
              sib.cla.sum[cnt, 7] <- 0
              sib.cla.sum[cnt, 8:9] <- ""
              cnt <- cnt + 1
            } else {
              for (spi in 1:nsleepperiods) {
                sib.cla.sum[cnt, 1] <- i #night
                sib.cla.sum[cnt, 2] <- colnames(sleep.t)[j] #definition
                sib.cla.sum[cnt, 3] <- format(time.t[1])
                sib.cla.sum[cnt, 4] <- nsleepperiods #number of sleep periods
                sleep_sp <- sleep.t[start_sp[spi]:end_sp[spi], j]
                time_sp <- time.t[start_sp[spi]:end_sp[spi]]
                sleep_dur <- (round((length(which(sleep_sp == 1)) / (60 / ws3)) * 100)) / 100
                sib.cla.sum[cnt, 5] <- sleep_dur / 60 #total sleep duration
                sib.cla.sum[cnt, 6] <- fraction.night.invalid
                sib.cla.sum[cnt, 7] <- spi
                sib.cla.sum[cnt, 8] <- format(time_sp[which(sleep_sp == 1)[1]])
                sib.cla.sum[cnt, 9] <- format(time_sp[length(sleep_sp)])
                cnt <- cnt + 1
              }
            }
            if (cnt > 800) {
              emptydf <- as.data.frame(matrix(0, 1000, ncol(sib.cla.sum)))
              colnames(emptydf) <- colnames(sib.cla.sum)
              sib.cla.sum <- rbind(sib.cla.sum, emptydf)
            }
          }
        }
      }
    }
  }
  sib.cla.sum <- sib.cla.sum[-which(sib.cla.sum$night == -1 | sib.cla.sum$sib.period == 0), ]
  return(sib.cla.sum)
}
