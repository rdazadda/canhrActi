# Ported from GGIR 3.3-9 R/get_starttime_weekday_truncdata.R and R/g.getstarttime.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: names follow canhrActi, the
# monitor and format codes are explicit arguments, the session-wide options(digits.secs)
# and options(warn) are scoped to the call, the LC_TIME "C" locale that GGIR() sets for
# the session is reproduced locally behind ggir_exact, and unisensR is loaded on demand.
# The arithmetic of the start alignment is GGIR's.

# GGIR's monitor and format codes (I$monc, I$dformc), kept here so this file has no
# dependency on the inspection module
.raw.monitor.codes <- c(AD_HOC = 0L, GENEA = 1L, GENEACTIV = 2L, ACTIGRAPH = 3L,
                        AXIVITY = 4L, MOVISENS = 5L, VERISENSE = 6L, PARMAY_MTX = 7L)
.raw.format.codes <- c(BIN = 1L, CSV = 2L, WAV = 3L, CWA = 4L, AD_HOC_CSV = 5L,
                       GT3X = 6L)

#' Resolve a Monitor or Format Code
#'
#' Accepts either GGIR's numeric code (monc 0..7, dformc 1..6) or the GGIR name
#' ("ACTIGRAPH", "GT3X", case-insensitive) and returns the numeric code.
#'
#' @param x numeric code or character name.
#' @param codes one of \code{.raw.monitor.codes} or \code{.raw.format.codes}.
#' @param what label used in the error message.
#' @return a single numeric code.
#' @keywords internal
#' @noRd
.raw.resolve.code <- function(x, codes, what) {
  if (is.null(x) || length(x) != 1) {
    stop("Argument ", what, " must be a single monitor/format code or name")
  }
  if (is.character(x)) {
    m <- match(toupper(x), names(codes))
    if (is.na(m)) stop("Unknown ", what, " name: ", x)
    return(as.numeric(codes[[m]]))
  }
  as.numeric(x)
}

#' Recording Start Time of a Raw Block
#'
#' GGIR's g.getstarttime. Formats whose reader emits a \code{time} column (gt3x, cwa,
#' GENEActiv, Parmay, ad-hoc and Axivity csv) take it from the data; Movisens takes it
#' from the unisens.xml next to the data; the ActiGraph and Verisense raw csv exports
#' take it from the "Start Time" and "Start Date" header lines, with the date layout
#' sniffed from the first line of the file.
#'
#' @details GGIR() sets LC_TIME to "C" for the whole session, which the csv branch
#'   depends on whenever the sniffed format contains a month name. With
#'   \code{ggir_exact = TRUE} the locale is "C" for the duration of the call; with FALSE
#'   the user's locale is used and a month-name date may parse to NA. In the csv branch
#'   the "." separator test matches any string, so the final warning branch is
#'   unreachable, as in GGIR.
#'
#' @param data data.frame or matrix of the first block. Only the presence of a
#'   \code{time} column and its first value are used.
#' @param mon monitor code as in GGIR's \code{I$monc} (0 ad hoc, 1 GENEA, 2 GENEActiv,
#'   3 ActiGraph, 4 Axivity, 5 Movisens, 6 Verisense, 7 Parmay) or its name.
#' @param dformat file format code as in GGIR's \code{I$dformc} (1 bin, 2 csv, 3 wav,
#'   4 cwa, 5 ad-hoc csv, 6 gt3x) or its name.
#' @param desiredtz Olson time zone the start time is expressed in ("" = the
#'   machine's zone).
#' @param configtz time zone the device was configured in; NULL means desiredtz.
#' @param datafile path to the data file; needed only by the Movisens and ActiGraph
#'   csv branches.
#' @param ggir_exact logical; reproduce GGIR's session LC_TIME "C" locale (default TRUE).
#' @return a \code{POSIXlt} of length one.
#' @keywords internal
#' @noRd
.raw.getstarttime <- function(data, mon, dformat, desiredtz, configtz = NULL,
                              datafile = NULL, ggir_exact = TRUE) {
  mon <- .raw.resolve.code(mon, .raw.monitor.codes, "mon")
  dformat <- .raw.resolve.code(dformat, .raw.format.codes, "dformat")
  if (isTRUE(ggir_exact)) {
    # GGIR sets this once per session
    LC_TIME_backup <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", LC_TIME_backup), add = TRUE)
  }
  starttime <- NULL
  if (is.null(configtz)) {
    configtz <- desiredtz
  }
  if ("time" %in% colnames(data)) {
    starttime <- as.POSIXlt(data[1, "time"], tz = desiredtz, origin = "1970-01-01")
  } else if (mon == .raw.monitor.codes[["MOVISENS"]]) {
    if (is.null(datafile)) {
      stop("datafile is required to read the Movisens start time")
    }
    if (!requireNamespace("unisensR", quietly = TRUE)) {
      stop("Package 'unisensR' is required for Movisens data. ",
           "Install it with install.packages(\"unisensR\").")
    }
    starttime <- unisensR::readUnisensStartTime(dirname(datafile))
    # unisens.xml carries no zone; relabel into configtz
    if (configtz != "") {
      starttime <- lubridate::force_tz(starttime, configtz)
    }
    starttime <- as.POSIXlt(starttime, tz = desiredtz)
  } else if (dformat == .raw.format.codes[["CSV"]] &&
             (mon == .raw.monitor.codes[["ACTIGRAPH"]] ||
              mon == .raw.monitor.codes[["VERISENSE"]])) {
    if (is.null(datafile)) {
      stop("datafile is required to read the ActiGraph csv header")
    }
    starttime <- startdate <- NULL
    header_rows <- 8
    tmph <- utils::read.csv(datafile, nrow = header_rows, skip = 1)

    tmphi <- 1
    while (tmphi < header_rows) {
      tmp <- unlist(strsplit(format(tmph[tmphi, 1]), "Start Time"))
      if (length(tmp) > 1) {
        starttime <- tmp[2]
        break
      }
      tmphi <- tmphi + 1
    }
    if (is.null(starttime)) {
      stop(paste0("Start Time not found in the header of ", datafile))
    }
    tmphi <- 1
    while (tmphi < header_rows) {
      tmp <- unlist(strsplit(format(tmph[tmphi, 1]), "Start Date"))
      if (length(tmp) > 1) {
        startdate <- tmp[2]
        break
      }
      tmphi <- tmphi + 1
    }
    if (is.null(startdate)) {
      stop(paste0("Start Date not found in the header of ", datafile))
    }

    startdate <- gsub(" ", "", startdate, fixed = TRUE)
    starttime <- gsub(" ", "", starttime, fixed = TRUE)

    starttime <- paste0(startdate, " ", starttime)
    # session-wide in GGIR; scoped to this call
    oldopts <- options(digits.secs = 3)
    on.exit(options(oldopts), add = TRUE)

    topline <- suppressWarnings(
      as.matrix(colnames(as.matrix(utils::read.csv(datafile, nrow = 1, skip = 0)))))
    topline <- topline[1]
    # sniff the date format of the first line: Y and y are the year with and without
    # century, b a month name, m a month number; a year in the middle is not considered
    allformats <- c("mdY", "mdy", "bdY", "bdy",
                    "dmY", "dmy", "dbY", "dby",
                    "Ymd", "ymd", "Ybd", "ybd",
                    "Ydm", "ydm", "Ydb", "ydb")
    fc <- data.frame(matrix(0, 1, length(allformats)))
    names(fc) <- allformats
    # yyyy beats yy, and MMM beats M(M)
    fc$mdY <- length(grep("MM[.]dd[.]yyyy|M[.]d[.]yyyy|M[.]dd[.]yyyy|MM[.]d[.]yyyy", topline))
    fc$mdy <- length(grep("MM[.]dd[.]yy|M[.]d[.]yy|M[.]dd[.]yy|MM[.]d[.]yy", topline))
    fc$bdY <- length(grep("MMM[.]dd[.]yyyy|MMM[.]d[.]yyyy", topline))
    fc$bdy <- length(grep("MMM[.]dd[.]yy|MMM[.]d[.]yy", topline))
    if (fc$mdY == 1 & fc$mdy == 1) fc$mdy <- 0
    if (fc$bdY == 1 & fc$bdy == 1) fc$bdy <- 0
    if (fc$mdY == 1 & fc$bdY == 1) fc$mdY <- 0
    if (fc$mdy == 1 & fc$bdy == 1) fc$mdy <- 0
    fc$dmY <- length(grep("d[.]M[.]yyyy|d[.]MM[.]yyyy", topline))
    fc$dmy <- length(grep("d[.]M[.]yy|d[.]MM[.]yy", topline))
    fc$dbY <- length(grep("d[.]MMM[.]yyyy", topline))
    fc$dby <- length(grep("d[.]MMM[.]yy", topline))
    if (fc$dmY == 1 & fc$dmy == 1) fc$dmy <- 0
    if (fc$dbY == 1 & fc$dby == 1) fc$dby <- 0
    if (fc$dmY == 1 & fc$dbY == 1) fc$dmY <- 0
    if (fc$dmy == 1 & fc$dby == 1) fc$dmy <- 0
    fc$Ymd <- length(grep("yyyy[.]M[.]d|yyyy[.]MM[.]d", topline))
    fc$ymd <- length(grep("yy[.]M[.]d|yy[.]MM[.]d", topline))
    fc$Ybd <- length(grep("yyyy[.]MMM[.]d", topline))
    fc$ybd <- length(grep("yy[.]MMM[.]d", topline))
    if (fc$Ymd == 1 & fc$ymd == 1) fc$ymd <- 0
    if (fc$Ybd == 1 & fc$ybd == 1) fc$ybd <- 0
    if (fc$Ymd == 1 & fc$Ybd == 1) fc$Ymd <- 0
    if (fc$ymd == 1 & fc$ybd == 1) fc$ymd <- 0
    fc$Ydm <- length(grep("yyyy[.]dd[.]MM|yyyy[.]d[.]M|yyyy[.]d[.]MM|yyyy[.]dd[.]M", topline))
    fc$ydm <- length(grep("yy[.]dd[.]MM|yy[.]d[.]M|yy[.]d[.]MM|yy[.]dd[.]M", topline))
    fc$Ydb <- length(grep("yyyy[.]dd[.]MMM|yyyy[.]d[.]MMM", topline))
    fc$ydb <- length(grep("yy[.]dd[.]MMM|yy[.]d[.]MMM", topline))
    if (fc$Ydm == 1 & fc$ydm == 1) fc$ydm <- 0
    if (fc$Ydb == 1 & fc$ydb == 1) fc$ydb <- 0
    if (fc$Ydm == 1 & fc$Ydb == 1) fc$Ydm <- 0
    if (fc$ydm == 1 & fc$ydb == 1) fc$ydm <- 0
    theformat <- names(fc)[which(fc == 1)[1]]
    if (is.na(theformat)) warning("date format not recognised")
    splitformat <- unlist(strsplit(theformat, ""))
    # separator
    if (length(grep("/", starttime)) > 0) {
      sepa <- "/"
    } else {
      if (length(grep("-", starttime)) > 0) {
        sepa <- "-"
      } else {
        if (length(grep(".", starttime)) > 0) {
          sepa <- "."
        } else {
          warning("separator character for dates not identified")
        }
      }
    }
    expectedformat <- paste0("%", splitformat[1], sepa, "%", splitformat[2], sepa,
                             "%", splitformat[3], " %H:%M:%S")
    starttime <- as.POSIXct(starttime, format = expectedformat, tz = configtz)
    starttime <- as.POSIXlt(starttime, tz = desiredtz)
  } else {
    stop(paste0("Timestamps not found for monitor type ", mon, " and file format type ", dformat,
                "\nThis should not happen."))
  }

  return(starttime)
}

#' Align the First Block to the Long-Epoch Grid
#'
#' GGIR's get_starttime_weekday_truncdata: finds the start time of the recording,
#' derives the weekday, and drops the samples before the next multiple of
#' \code{ws2/60} minutes so the first epoch starts on a whole quarter hour. Called
#' once, on the first block, after time-gap imputation and before the whole-window cut.
#'
#' @details The weekday is taken from the first sample's time, before the shift, so a
#'   shift across midnight keeps the earlier day's name. A \code{sampleshift} of exactly
#'   1 drops nothing (GGIR's \code{> 1} test), and a fractional-second start is floored
#'   to whole samples. Setting \code{starttime$min} past 59 is left to POSIXlt
#'   normalisation, as in GGIR, so the returned object carries unnormalised fields;
#'   convert with \code{as.POSIXct} before formatting. When \code{info} or
#'   \code{params} is supplied, missing arguments are filled from them.
#'
#' @param data data.frame or matrix of the first block with columns x, y, z and, for
#'   most formats, a numeric \code{time} column (Unix seconds).
#' @param mon monitor code or name (see \code{.raw.getstarttime}).
#' @param dformat file format code or name (see \code{.raw.getstarttime}).
#' @param desiredtz Olson time zone for the start time ("" = machine zone).
#' @param configtz time zone the device was configured in; NULL means desiredtz.
#' @param ws2 long epoch length in seconds (GGIR \code{windowsizes[2]}, default 900).
#' @param sf sample frequency in Hz.
#' @param datafile path to the data file; Movisens and ActiGraph csv branches only.
#' @param header the GGIR header object; passed by GGIR and never used, as here.
#' @param info optional canhrActi raw inspection object supplying mon, dformat, sf and
#'   datafile.
#' @param params optional \code{raw.params()} object supplying desiredtz, configtz, ws2
#'   and ggir_exact.
#' @param ggir_exact logical; reproduce GGIR's LC_TIME "C" for the csv date parse
#'   (default TRUE).
#' @return list with \code{starttime} (POSIXlt, aligned), \code{wday} (1..7, 1 =
#'   Sunday), \code{wdayname} and \code{data} (the block with the leading samples
#'   removed).
#' @keywords internal
#' @noRd
.raw.starttime.truncate <- function(data, mon = NULL, dformat = NULL, desiredtz = NULL,
                                    configtz = NULL, ws2 = NULL, sf = NULL,
                                    datafile = NULL, header = NULL, info = NULL,
                                    params = NULL, ggir_exact = NULL) {
  if (!is.null(info)) {
    if (is.null(mon)) mon <- info$monc
    if (is.null(dformat)) dformat <- info$dformc
    if (is.null(sf)) sf <- info$sf
    if (is.null(datafile)) datafile <- info$path
    if (is.null(header)) header <- info$header
  }
  if (!is.null(params)) {
    if (is.null(desiredtz)) desiredtz <- params$desiredtz
    if (is.null(configtz)) configtz <- params$configtz
    if (is.null(ws2) && !is.null(params$windowsizes)) ws2 <- params$windowsizes[2]
    if (is.null(ggir_exact)) ggir_exact <- params$ggir_exact
  }
  if (is.null(desiredtz)) desiredtz <- ""
  if (is.null(ws2)) ws2 <- 900
  if (is.null(ggir_exact)) ggir_exact <- TRUE
  if (is.null(mon) || is.null(dformat)) {
    stop("mon and dformat must be supplied, directly or through info")
  }
  if (is.null(sf) || !is.numeric(sf) || length(sf) != 1 || is.na(sf) || sf <= 0) {
    stop("sf must be a single positive number")
  }
  if (length(configtz) == 0) configtz <- NULL # GGIR passes c() / NULL for "not set"

  # the first window starts on a whole multiple of ws2 (15, 30, 45 or 60 min)
  start_meas <- ws2 / 60

  starttime <- .raw.getstarttime(
    data = data,
    mon = mon,
    dformat = dformat,
    desiredtz = desiredtz,
    configtz = configtz,
    datafile = datafile,
    ggir_exact = ggir_exact
  )

  wday <- starttime$wday # 0 is Sunday
  wday <- wday + 1
  weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  wdayname <- weekdays[wday]

  # samples to drop before the next whole period
  secshift <- 60 - starttime$sec
  if (secshift == 60) {
    secshift <- 0 # on a whole minute already
  } else {
    starttime$min <- starttime$min + 1 # the seconds shift moves into the next minute
  }

  minshift <- start_meas - (starttime$min %% start_meas)
  if (minshift == start_meas) {
    minshift <- 0
  }

  sampleshift <- (minshift * 60 * sf) + (secshift * sf)
  sampleshift <- floor(sampleshift)
  if (sampleshift > 1) {
    data <- data[-c(1:sampleshift), ]
  }

  # the aligned start; POSIXlt normalises a minute past 59
  starttime$min <- starttime$min + minshift
  starttime$sec <- 0

  invisible(
    list(
      starttime = starttime,
      wday = wday,
      wdayname = wdayname,
      data = data
    )
  )
}
