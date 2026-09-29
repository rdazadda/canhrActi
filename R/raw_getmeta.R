# Ported from GGIR 3.3-9 R/g.getmeta.R, with the tail expansion of R/g.part1.R and the
# timestamp helpers R/iso8601chartime2POSIX.R and R/POSIXtime2iso8601.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: g.getmeta became raw.getmeta
# with a canhrActi params list and explicit arguments, and its helpers became the .raw.*
# functions of this package; the myfun path and console printing are removed; warnings
# are collected into a messages vector; a per-chunk trace, a progress callback and a
# numeric time column are added; the return is a classed list with as.ggir.M() giving
# GGIR's M shape. The between-chunk gap test keeps GGIR's unit mix under ggir_exact =
# TRUE.

# SMALL HELPERS

#' ISO 8601 Character Timestamps to POSIXct
#'
#' GGIR's iso8601chartime2POSIX.
#' @param x Character vector in the format "%Y-%m-%dT%H:%M:%S%z".
#' @param tz Timezone of the result.
#' @return POSIXct.
#' @keywords internal
#' @noRd
.raw.iso8601.to.posix <- function(x, tz) {
  return(as.POSIXct(x = x, format = "%Y-%m-%dT%H:%M:%S%z", tz = tz))
}

#' POSIX Times to ISO 8601 Character Timestamps
#'
#' GGIR's POSIXtime2iso8601, including the format() and re-parse round trip that drops
#' sub-second digits.
#' @param x POSIXct or POSIXlt vector.
#' @param tz Timezone in which the strings are written.
#' @return Character vector in the format "%Y-%m-%dT%H:%M:%S%z".
#' @keywords internal
#' @noRd
.raw.posix.to.iso8601 <- function(x, tz) {
  chartime2iso8601 <- function(x, tz) {
    POStime <- as.POSIXlt(as.numeric(as.POSIXlt(x, tz)), origin = "1970-1-1", tz)
    POStimeISO <- strftime(POStime, format = "%Y-%m-%dT%H:%M:%S%z")
    return(POStimeISO)
  }
  POStime <- as.POSIXlt(x, tz) #turn to right timezone
  POStime_z <- chartime2iso8601(format(POStime), tz) #change format
  return(POStime_z)
}

#' Epoch Timestamps Regenerated From the Aligned Start Time
#'
#' GGIR never reads epoch times from the data: it takes the aligned start time, rounds it
#' to whole seconds, builds an arithmetic sequence and formats it in the desired timezone.
#' Epoch edges are therefore absolute Unix seconds; a daylight saving change shifts the
#' wall-clock labels, never the edges. g.getmeta's metashort and metalong constructions of
#' the sequence end give the same values and both are kept behind \code{long}. GGIR's
#' warning for a zero-length timezone is reproduced.
#'
#' @param starttime The aligned start time as returned by \code{.raw.starttime.truncate}
#'   (a POSIXlt; any object \code{as.numeric()} accepts).
#' @param n Number of epochs.
#' @param step Epoch length in seconds (ws3 for metashort, ws2 for metalong).
#' @param tz GGIR's desiredtz ("" is the machine's zone).
#' @param long TRUE selects the metalong construction of the sequence end.
#' @return A list with \code{time} (numeric Unix seconds, one per epoch) and
#'   \code{timestamp} (character, GGIR's format). Both empty when \code{n < 1}.
#' @keywords internal
#' @noRd
.raw.epoch.timestamps <- function(starttime, n, step, tz = "", long = FALSE) {
  if (length(n) != 1 || is.na(n) || n < 1) {
    return(list(time = numeric(0), timestamp = character(0)))
  }
  if (length(tz) == 0) {
    warning("desiredtz not specified, system timezone used as default")
    tz <- ""
  }
  starttime3 <- round(as.numeric(starttime)) #numeric time but relative to the desiredtz
  if (long) {
    time5 <- seq(starttime3, (starttime3 + (n * step) - 1), by = step)
  } else {
    time5 <- seq(starttime3, (starttime3 + ((n - 1) * step)), by = step)
  }
  time6 <- as.POSIXlt(time5, origin = "1970-01-01", tz = tz)
  time6 <- strftime(time6, format = "%Y-%m-%dT%H:%M:%S%z")
  list(time = time5, timestamp = as.character(time6))
}

#' Replicate the Epochs That Stand for a Long Gap
#'
#' GGIR's impute_at_epoch_level. Gaps longer than \code{max(6 * ws2 / 60, 90)} minutes are
#' only filled at raw level up to the next long-epoch cut and from the cut before the
#' resume time; the whole epochs in between arrive here as a marker on the epoch that
#' contains the end of the raw fill, and that epoch is replicated \code{gapsize} times. In
#' metalong the marker epoch gets nonwearscore 3; in metashort every column except the
#' timestamp and the angles is set to 0 and EN to 1. Because the marker sits at the end of
#' the raw fill, the replicas follow both raw fills back to back, as in GGIR. Three or more
#' markers in one epoch would empty the index vector on the second pass; unreachable with
#' 90-minute gaps and 5-second epochs, kept as is.
#'
#' @param gapsize Numeric vector, number of epochs each marker stands for (metashort: the
#'   remaining_epochs value; metalong: \code{floor(remaining_epochs * ws3 / ws2) + 1}).
#' @param timeseries The character matrix (metashort or metalong) being filled.
#' @param gap_index Row of each marker epoch in \code{timeseries}.
#' @param metnames Column names of \code{timeseries} (metashort: c("timestamp", metnames);
#'   metalong: metricnames_long).
#' @return The matrix with the marker rows replicated.
#' @keywords internal
#' @noRd
.raw.impute.at.epoch.level <- function(gapsize, timeseries, gap_index, metnames) {
  # gap_index: where do gaps occur (epoch indexing)
  # gap_size: how long is gap (epoch numbers)
  if (any(duplicated(gap_index))) {
    # When 2 gap_index are within the same epoch (either short or long)
    # we would have a duplicated gap_index here, then combine information
    dup_index_tmp <- which(duplicated(gap_index))
    dup_index <- gap_index[dup_index_tmp]
    for (dup_index_i in dup_index) {
      to_combine <- which(gap_index == dup_index_i)
      length_to_combine <- length(to_combine) # In the unlikely event that a gap_index appears more than 2, this should be able to deal with it.
      delete <- to_combine[-1] # leave only the first index and remove duplicates
      gap_index <- gap_index[-delete] # remove from gap index
      gapsize[to_combine[1]] <- sum(gapsize[to_combine]) - (length_to_combine - 1) # minus 1 because it was summed 1 to each gapsize (which is +2 when it is duplicated) in the function call
      gapsize <- gapsize[-delete]
    }
  }
  if ("nonwearscore" %in% metnames) {
    timeseries[gap_index, which(metnames == "nonwearscore")]  <- 3
  } else {
    # set all features to zero except time and angle feature
    timeseries[gap_index, grep(pattern = "time|angle",
                               x = metnames, invert = TRUE, value = FALSE)] <- 0
    # set EN to 1 if it is available
    if ("EN" %in% metnames) timeseries[gap_index, which(metnames == "EN")] <- 1
  }
  N_time <- nrow(timeseries)
  newindi <- rep(1, N_time)
  newindi[gap_index] <- as.numeric(gapsize)
  newindi <- rep(1:N_time, newindi)
  timeseries <- timeseries[newindi,]
  return(timeseries)
}

#' Apply the Calibration Coefficients to One Chunk
#'
#' The one place where the auto-calibration changes the data: after gap imputation, start
#' alignment and the whole-window cut, before the Euclidean norm and every metric.
#' \code{base::scale()} is the arithmetic, as in g.getmeta; the algebraic form
#' \code{(x + offset) * scale} differs at 4.4e-16. Temperature enters only when the chunk
#' has a usable temperature column (mean of the first ten values at most 50 C) and the
#' calibration carries a still-window mean temperature; a calibration retrieved from a
#' data_quality_report has no meantempcal, so its temperature term is dropped.
#'
#' @param data Numeric matrix with columns named x, y, z (further columns pass through).
#' @param offset,scale,tempoffset The coefficients (length 3 each).
#' @param meantempcal The calibration's mean still-window temperature, or NULL / c().
#' @param use.temp Logical, g.getmeta's own temperature decision for this file.
#' @param temperature The chunk's temperature column (one value per row) when use.temp.
#' @return The matrix with x, y, z calibrated.
#' @keywords internal
#' @noRd
.raw.apply.calibration <- function(data, offset = c(0, 0, 0), scale = c(1, 1, 1),
                                   tempoffset = c(0, 0, 0), meantempcal = c(),
                                   use.temp = FALSE, temperature = c()) {
  # rescale data
  data[, c("x", "y", "z")] <- scale(data[, c("x", "y", "z")], center = -offset, scale = 1/scale)
  if (use.temp && length(meantempcal) > 0) {
    yy <- cbind(temperature,
                temperature,
                temperature)
    data[, c("x", "y", "z")] <- data[, c("x", "y", "z")] + scale(yy, center = rep(meantempcal,3), scale = 1/tempoffset)
  }
  data
}

# CALIBRATION COEFFICIENTS

#' Coefficients to Apply, From Whatever Calibration Object the Caller Holds
#'
#' g.part1 hands g.getmeta the members scale, offset, tempoffset and meantempcal of its C
#' object after the identity-reset rule. This accepts the shapes canhrActi can hold: NULL
#' (GGIR's default C: identity, no temperature), a canhrActi_raw_calibration (its applied
#' coefficients), a GGIR C list from a milestone (used as stored), or a
#' data_quality_report-shaped data.frame or csv path (GGIR's retrieve semantics:
#' meantempcal absent).
#'
#' @param calibration One of the shapes above.
#' @param filename File name used to match a table's filename column.
#' @return list(offset, scale, tempoffset, meantempcal, source, applied).
#' @keywords internal
#' @noRd
.raw.getmeta.coefficients <- function(calibration, filename = NA_character_) {
  if (is.null(calibration)) {
    # GGIR's default C; meantempcal is not a member there
    return(list(offset = c(0, 0, 0), scale = c(1, 1, 1), tempoffset = c(0, 0, 0),
                meantempcal = c(), source = "none", applied = FALSE))
  }
  if (inherits(calibration, "canhrActi_raw_calibration")) {
    return(list(offset = calibration$offset, scale = calibration$scale,
                tempoffset = calibration$tempoffset, meantempcal = calibration$meantempcal,
                source = "canhrActi_raw_calibration", applied = isTRUE(calibration$applied)))
  }
  if (is.list(calibration) && !is.data.frame(calibration) &&
      all(c("scale", "offset") %in% names(calibration))) {
    # a GGIR C list (milestone), used exactly as g.part1 passes it (tempoffset may be NULL)
    return(list(offset = calibration$offset, scale = calibration$scale,
                tempoffset = calibration$tempoffset, meantempcal = calibration$meantempcal,
                source = "ggir_C",
                applied = !(identical(as.numeric(calibration$scale), c(1, 1, 1)) &&
                              identical(as.numeric(calibration$offset), c(0, 0, 0)))))
  }
  if (is.data.frame(calibration) || is.character(calibration)) {
    obj <- .raw.calibration.supplied(calibration, filename)
    if (is.null(obj)) {
      stop("The supplied calibration table has no row for ", filename, call. = FALSE)
    }
    return(list(offset = obj$offset, scale = obj$scale, tempoffset = obj$tempoffset,
                meantempcal = obj$meantempcal, source = obj$supplied_from, applied = TRUE))
  }
  stop("calibration must be NULL, a canhrActi_raw_calibration, a GGIR C list, ",
       "a data_quality_report-shaped data.frame or a csv path", call. = FALSE)
}

# THE BLOCK LOOP

#' The Block Loop of GGIR's g.getmeta
#'
#' Reads the file in blocks (24 hours of recorded samples for gt3x), imputes time gaps for
#' the formats whose readers do not (csv, ad-hoc csv, gt3x), carries the leftover samples
#' between blocks, aligns the first block to the long-epoch grid, keeps whole long epochs,
#' applies the calibration, computes the Euclidean norm, the short-epoch metrics, the
#' long-epoch non-wear and clipping scores, light and temperature means, the epoch-level
#' replication of long gaps, and assembles the two character matrices that become
#' metashort and metalong.
#'
#' @details
#' The loop of g.getmeta, kept verbatim: the counters, the pre-allocation of metashort and
#' metalong as character matrices of " " and their extension rule, the state taken from
#' the reader when it returns it, the first-block decisions on light and temperature with
#' the 50 C test, gap imputation with k = 0.25 and epochsize c(ws3, ws2) for csv, ad-hoc
#' csv and gt3x only, the remaining_epochs column harmonisation between S and the block,
#' the between-chunk gap test (GGIR's unit mix of seconds against 3/sf, 60*sf and 3600*sf
#' under \code{ggir_exact = TRUE}, the intended seconds under FALSE; dead for gt3x because
#' the gap imputation already closed the gap), the start alignment on block 1, the whole
#' long-epoch cut and the leftover rule, the calibration, EN, the metrics rounded to 4
#' decimals, the metashort columns in allmetrics order, the non-wear and clipping call, the
#' metalong columns with the lightpeak window of ws2*sf + 1 samples starting one sample
#' early, the epoch-level imputation with \code{exists("remaining_epochs")} semantics, the
#' end rules and the assembly with the character round trip.
#'
#' Changes: no myfun support; GGIR's warnings and the readers' warnings are recorded in
#' \code{messages}; a progress callback, a per-block trace and the numeric epoch times are
#' added; the reader's header is never fed back (GGIR's loop passes NULL on every block).
#' If no block was ever processed while the file was neither corrupt nor too short, GGIR
#' fails with "object 'metnames' not found"; here the file is reported as too short.
#'
#' @param info A canhrActi_raw_info from raw.inspect().
#' @param sf Sample frequency in Hz.
#' @param mon,dformat GGIR's monitor and format codes (info$monc, info$dformc; Verisense is
#'   not mapped to ActiGraph here, as in g.getmeta).
#' @param blocksize The getmeta block size from \code{.raw.blocksize(info, "getmeta")}.
#' @param clipthres,sdcriter,racriter Clipping and non-wear thresholds from
#'   \code{.raw.clip.block.params}.
#' @param windowsizes c(ws3, ws2, ws) after GGIR's coercions.
#' @param offset,scale,tempoffset,meantempcal The calibration coefficients to apply.
#' @param metrics Named list of the 32 do.* flags (see \code{.raw.metric.flags}).
#' @param n,lb,hb,zc.lb,zc.hb,zc.sb,zc.order,actilife_LFE GGIR's params_metrics members.
#' @param imputeTimegaps,nonwear_approach GGIR's params_rawdata / params_cleaning members.
#' @param desiredtz,configtz Timezones.
#' @param read_params The params list forwarded to \code{.raw.read.block}.
#' @param ggir_exact TRUE reproduces the between-chunk unit mix.
#' @param progress NULL or function(stage, i, n, message), called before every block read
#'   with stage "getmeta".
#' @param daylimit FALSE, or the number of blocks to read (GGIR's testing switch).
#' @param filename The path the reader opens; default info$read_path, then info$path.
#' @return A list with GGIR's members (filecorrupt, filetooshort, NFilePagesSkipped,
#'   metalong, metashort, wday, wdayname, windowsizes, bsc_qc, QClog) plus starttime (the
#'   aligned POSIXlt), time_short and time_long (numeric epoch times), metricnames_short,
#'   metricnames_long, light.available, use.temp, filedoesnotholdday, chunks, messages,
#'   blocks (number of loop passes) and rows_leftover (samples left in S at the end).
#' @keywords internal
#' @noRd
.raw.getmeta.loop <- function(info, sf, mon, dformat, blocksize, clipthres, sdcriter, racriter,
                              windowsizes = c(5, 900, 3600),
                              offset = c(0, 0, 0), scale = c(1, 1, 1), tempoffset = c(0, 0, 0),
                              meantempcal = c(),
                              metrics = .raw.metric.flags(),
                              n = 4, lb = 0.2, hb = 15,
                              zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2,
                              actilife_LFE = FALSE,
                              imputeTimegaps = TRUE, nonwear_approach = "2023",
                              desiredtz = "", configtz = NULL,
                              read_params = NULL, ggir_exact = TRUE, progress = NULL,
                              daylimit = FALSE, filename = NULL) {
  messages <- character()
  note <- function(...) messages <<- c(messages, paste0(...))
  trace <- list()
  if (is.null(filename)) {
    filename <- if (!is.null(info$read_path)) info$read_path else info$path
  }
  datafile <- filename
  metrics2do <- metrics

  nmetrics <- sum(c(metrics2do[["do.bfen"]], metrics2do[["do.enmo"]],
                    metrics2do[["do.lfenmo"]], metrics2do[["do.en"]],
                    metrics2do[["do.hfen"]], metrics2do[["do.hfenplus"]],
                    metrics2do[["do.mad"]], metrics2do[["do.anglex"]],
                    metrics2do[["do.angley"]], metrics2do[["do.anglez"]],
                    metrics2do[["do.roll_med_acc_x"]], metrics2do[["do.roll_med_acc_y"]],
                    metrics2do[["do.roll_med_acc_z"]],
                    metrics2do[["do.dev_roll_med_acc_x"]], metrics2do[["do.dev_roll_med_acc_y"]],
                    metrics2do[["do.dev_roll_med_acc_z"]],
                    metrics2do[["do.enmoa"]], metrics2do[["do.lfen"]],
                    metrics2do[["do.lfx"]], metrics2do[["do.lfy"]],
                    metrics2do[["do.lfz"]],  metrics2do[["do.hfx"]],
                    metrics2do[["do.hfy"]], metrics2do[["do.hfz"]],
                    metrics2do[["do.bfx"]], metrics2do[["do.bfy"]],
                    metrics2do[["do.bfz"]],
                    metrics2do[["do.zcx"]], metrics2do[["do.zcy"]],
                    metrics2do[["do.zcz"]], metrics2do[["do.brondcounts"]] * 3,
                    metrics2do[["do.neishabouricounts"]] * 4))
  if (nmetrics == 0) {
    # GGIR only warns here and then fails inside the loop on accmetrics[[1]]
    stop("No metrics selected: switch on at least one do.* metric (GGIR's defaults are do.enmo and do.anglez).",
         call. = FALSE)
  }

  ws3 <- windowsizes[1]; ws2 <- windowsizes[2]; ws <- windowsizes[3]

  PreviousEndPage <- c()

  filequality <- data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                            filedoesnotholdday = FALSE, NFilePagesSkipped = 0)
  filetooshort <- FALSE
  filecorrupt <- FALSE
  filedoesnotholdday <- FALSE
  NFilePagesSkipped <- 0

  i <- 1 #counter to keep track of which binary block is being read
  count <- 1 #counter to keep track of the number of seconds that have been read
  count2 <- 1 #count number of blocks read with length "ws2" (long epoch, 15 minutes by default)
  LD <- 2 #dummy variable used to identify end of file and to make the process stop
  bsc_qc <- data.frame(time = c(), size = c(), stringsAsFactors = FALSE)

  n_decimal_places <- 4 # number of decimal places to which features should be rounded
  # creating matrices for storing output
  S <- matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  nev <- 80*10^7 # number expected values
  NR <- ceiling(nev / (sf*ws3)) + 1000 #NR = number of 'ws3' second rows (this is for 10 days at 80 Hz)
  metashort <- matrix(" ",NR,(1 + nmetrics)) #generating output matrix for acceleration signal
  QClog <- NULL

  # Read file
  isLastBlock <- FALSE # dummy variable part "end of loop mechanism"

  PreviousLastValue <- c(0, 0, 1)
  PreviousLastTime <- NULL
  header <- NULL
  # members that GGIR leaves undefined until the first processed block
  starttime <- NULL; wday <- NULL; wdayname <- NULL
  metnames <- NULL; metricnames_long <- NULL
  light.available <- FALSE; use.temp <- FALSE

  while (LD > 1) {
    if (!is.null(progress)) {
      progress("getmeta", as.integer(i), NA_integer_, paste0("Loading chunk: ", i))
    }
    t_block <- Sys.time()
    tr <- list(block = as.integer(i), rows_read = 0, rows_imputed = 0, rows_carried_in = as.numeric(nrow(S)),
               rows_dropped_start = 0, rows_used = 0, rows_carried_out = 0,
               epochs_short = 0, epochs_long = 0, epochs_short_added = 0, epochs_long_added = 0,
               gaps = 0L, minutes_imputed = 0, span_seconds = NA_real_, is_last_block = FALSE)

    # GGIR does not muffle warnings here; the readers' warnings are recorded instead of raised
    accread <- withCallingHandlers(
      .raw.read.block(info, blocksize = blocksize, blocknumber = i,
                      previous_end_page = PreviousEndPage, ws = ws, params = read_params,
                      previous_last_value = PreviousLastValue, previous_last_time = PreviousLastTime,
                      filequality = filequality, header = header, filename = filename),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if (length(accread$messages) > 0) messages <- c(messages, accread$messages)
    # g.readaccfile returns no header, so GGIR's loop passes NULL on every block
    header <- NULL

    # the state is the reader's when it returned it (ad-hoc csv), else unchanged
    PreviousLastValue <- accread$previous_last_value
    PreviousLastTime <- accread$previous_last_time

    filequality <- accread$filequality
    filetooshort <- filequality$filetooshort
    filecorrupt <- filequality$filecorrupt
    filedoesnotholdday <- filequality$filedoesnotholdday
    NFilePagesSkipped <- filequality$NFilePagesSkipped
    isLastBlock <- accread$is_last_block
    PreviousEndPage <- accread$endpage
    tr$is_last_block <- isLastBlock

    #process data as read from binary file
    if (length(accread$P) > 0) { # would have been set to zero if file was corrupt or empty
      data <- accread$P$data
      QClog <- rbind(QClog, accread$P$QClog)
      rm(accread)
      tr$rows_read <- as.numeric(nrow(data))
      if ("time" %in% colnames(data) && nrow(data) > 0) {
        # wall clock the block covers, first sample to the end of the last one, as a whole
        # number of sample periods; differs from QClog$blockLengthSeconds for partly filled gaps
        tr$span_seconds <- (round((as.numeric(data$time[nrow(data)]) -
                                     as.numeric(data$time[1])) * sf) + 1) / sf
      }

      if (i == 1) {
        light.available <- ("light" %in% colnames(data))

        use.temp <- ("temperature" %in% colnames(data))
        if (use.temp) {
          if (mean(data$temperature[1:10], na.rm = TRUE) > 50) {
            note("temperature value is unreaslistically high (> 50 Celcius) and will not be used.")
            use.temp <- FALSE
          }
        }

        # output matrix for 15 minutes summaries
        if (!use.temp && !light.available) {
          metalong <- matrix(" ", ((nev/(sf*ws2)) + 100), 4)
          metricnames_long <- c("timestamp","nonwearscore","clippingscore","EN")
        } else if (use.temp && !light.available) {
          metalong <- matrix(" ", ((nev/(sf*ws2)) + 100), 5)
          metricnames_long <- c("timestamp","nonwearscore","clippingscore","temperaturemean","EN")
        } else if (!use.temp && light.available) {
          metalong <- matrix(" ", ((nev/(sf*ws2)) + 100), 6)
          metricnames_long <- c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","EN")
        } else if (use.temp && light.available) {
          metalong <- matrix(" ", ((nev/(sf*ws2)) + 100), 7)
          metricnames_long <- c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean","EN")
        }
      }

      if (imputeTimegaps && (dformat == .RAW_FORMAT[["CSV"]] ||
                             dformat == .RAW_FORMAT[["AD_HOC_CSV"]] ||
                             dformat == .RAW_FORMAT[["GT3X"]])) {
        P <- .raw.impute.timegaps(data, sf = sf, k = 0.25,
                                  previous_last_value = PreviousLastValue,
                                  previous_last_time = PreviousLastTime,
                                  epochsize = c(ws3, ws2))
        data <- P$x
        PreviousLastValue <- data[nrow(data), c("x", "y", "z")]
        if ("time" %in% colnames(data)) {
          PreviousLastTime <- as.POSIXct(data$time[nrow(data)], origin = "1970-1-1")
        } else {
          PreviousLastTime <- NULL
        }
        QClog <- rbind(QClog, P$qclog)
        tr$gaps <- P$qclog$timegaps_n
        tr$minutes_imputed <- P$qclog$timegaps_min
        rm(P)
      }
      tr$rows_imputed <- as.numeric(nrow(data))

      gc()

      data <- as.matrix(data, rownames.force = FALSE)
      #add leftover data from last time
      if (nrow(S) > 0) {
        if (imputeTimegaps) {
          if ("remaining_epochs" %in% colnames(data)) {
            if (ncol(S) == (ncol(data) - 1)) {
              # this block has time gaps while the previous block did not
              S <- cbind(S, 1)
              colnames(S)[ncol(S)] <- "remaining_epochs"
            }
          } else if ("remaining_epochs" %in% colnames(S)) {
            if ((ncol(S) - 1) == ncol(data)) {
              # this block does not have time gaps while the previous block did
              data <- cbind(data, 1)
              colnames(data)[ncol(S)] <- "remaining_epochs"
            }
          }
        }
        # gaps between chunks are not handled by the gap imputation and are not logged
        if ("time" %in% colnames(data) && "time" %in% colnames(S)) {
          timegap_between_chunks <- as.numeric(data[1, "time"] - S[nrow(S), "time"])
          if (ggir_exact) {
            # GGIR compares seconds with 3600 * sf and 60 * sf
            gap_stop <- 3600 * sf
            gap_fill_max <- 60 * sf
          } else {
            # GGIR's intent: more than 1 hour stops, more than 3 samples and at most 1 hour is filled
            gap_stop <- 3600
            gap_fill_max <- 3600
          }
          if (timegap_between_chunks > gap_stop) {
            stop(paste0("Time gap observed of more than 1 hour between data ",
                        "chunks for ", basename(datafile), " . Please contact ",
                        "package maintainer."), call. = FALSE)
          } else if (timegap_between_chunks > 3 / sf && timegap_between_chunks <= gap_fill_max) {
            # impute time gap of more than 3 samples and equal to or less than 1 hour
            # normalise last value
            S[nrow(S), c("x", "y", "z")] <- S[nrow(S), c("x", "y", "z")] / sqrt(sum(S[nrow(S), c("x", "y", "z")]^2))
            # replicate last row
            newRows <- do.call(rbind, replicate(round(timegap_between_chunks  * sf), S[nrow(S), ], simplify = FALSE))
            # append to end
            S <- rbind(S, newRows)
          }
        }
        data <- rbind(S, data)
      }
      if (i == 1) {
        rows_before_shift <- nrow(data)
        SWMT <- .raw.starttime.truncate(data = data, mon = mon, dformat = dformat,
                                        desiredtz = desiredtz, configtz = configtz,
                                        ws2 = ws2, sf = sf, datafile = datafile, header = header,
                                        ggir_exact = ggir_exact)
        starttime <- SWMT$starttime
        wday <- SWMT$wday; wdayname <- SWMT$wdayname
        data <- SWMT$data
        tr$rows_dropped_start <- as.numeric(rows_before_shift - nrow(data))

        rm(SWMT)
      }

      LD <- nrow(data)
      if (LD < (ws*sf) && i == 1) {
        note('\nWarning data too short for doing non-wear detection 3\n')
        isLastBlock <- TRUE
        LD <- 0 #ignore rest of the data and store what has been loaded so far.
      }

      #store data that could not be used for this block, but will be added to next block
      if (LD >= (ws*sf)) {

        use <- (floor(LD / (ws2*sf))) * (ws2*sf) #number of datapoint to use # changes from ws to ws2 Vvh 23/4/2017
        if ((LD - use) > 1) {
          S <- data[(use + 1):LD,] #store leftover data
          if (ncol(S) == 1) {
            S <- t(S)
          }
        } else { #use all data
          S <- matrix(0, 0, ncol(data))
        }
        data <- data[1:use,]
        LD <- nrow(data) #redefine LD because there is less data
        tr$rows_used <- as.numeric(use)
        tr$rows_carried_out <- as.numeric(nrow(S))
        if ("remaining_epochs" %in% colnames(data)) { #
          # remove remaining_epochs from data object and keep it seperately
          remaining_epochs <- data[,"remaining_epochs"]
          data <- data[, -which(colnames(data) == "remaining_epochs")]
        }
        # Feature calculation
        temperature <- light <- c()
        if (light.available) {
          light <- data[, "light"]
        }
        if (use.temp) {
          temperature <- data[, "temperature"]
        }

        # rescale data (base::scale kept)
        data <- .raw.apply.calibration(data, offset = offset, scale = scale, tempoffset = tempoffset,
                                       meantempcal = meantempcal, use.temp = use.temp,
                                       temperature = temperature)

        EN <- sqrt(data[, "x"]^2 + data[, "y"]^2 + data[, "z"]^2) # Do not delete Used for long epoch calculation
        accmetrics <- do.call(.raw.apply.metrics,
                              c(list(data = data, sf = sf, ws3 = ws3), metrics2do,
                                list(n = n, lb = lb, hb = hb,
                                     zc.lb = zc.lb, zc.hb = zc.hb, zc.sb = zc.sb, zc.order = zc.order,
                                     actilife_LFE = actilife_LFE, ggir_exact = ggir_exact)))
        # round decimal places, because due to averaging we get a lot of information
        # that only slows down computation and increases storage size
        accmetrics <- .raw.round.metrics(accmetrics, n_decimal_places)
        accmetrics <- data.frame(sapply(accmetrics,c)) # collapse to data.frame
        # update LD in case data has been imputed at epoch level
        if (floor(LD / (ws3 * sf)) < nrow(accmetrics)) { # then, data has been imputed
          LD <- nrow(accmetrics) * ws3 * sf
        }
      }
      if (LD >= (ws*sf)) { #LD != 0
        #extend metashort and metalong if it is expected to be too short
        if (exists("remaining_epochs", inherits = FALSE)) {
          totalgap <- sum(remaining_epochs[which(remaining_epochs != 1)])
        } else {
          totalgap <- 0
        }
        if (count > (nrow(metashort) - ((2.5*(3600/ws3) * 24)) + totalgap)) {
          extension <- matrix(" ", ((3600/ws3) * 24) + totalgap, ncol(metashort)) #add another day to metashort once you reach the end of it
          metashort <- rbind(metashort,extension)
          extension2 <- matrix(" ", ((3600/ws2) * 24)  + (totalgap * (ws2/ws3)), ncol(metalong)) #add another day to metashort once you reach the end of it
          metalong <- rbind(metalong, extension2)
        }
        col_msi <- 2
        # Add metric time series to metashort object
        metnames <- grep(pattern = "BrondCount|NeishabouriCount", x = names(accmetrics), invert = TRUE, value = TRUE)
        for (metnam in metnames) {
          dovalue <- paste0("do.",tolower(metnam))
          dovalue <- gsub(pattern = "angle_", replacement = "angle", x = dovalue)
          if (metrics2do[[dovalue]] == TRUE) {
            metashort[count:(count - 1 + length(accmetrics[[metnam]])), col_msi] <- accmetrics[[metnam]]
            col_msi <- col_msi + 1
          }
        }
        if (metrics2do[["do.brondcounts"]] == TRUE) {
          metashort[count:(count - 1 + length(accmetrics$BrondCount_x)), col_msi] <- accmetrics$BrondCount_x
          metashort[count:(count - 1 + length(accmetrics$BrondCount_y)), col_msi + 1] <- accmetrics$BrondCount_y
          metashort[count:(count - 1 + length(accmetrics$BrondCount_z)), col_msi + 2] <- accmetrics$BrondCount_z
          col_msi <- col_msi + 3
          metnames <- c(metnames, "BrondCount_x", "BrondCount_y", "BrondCount_z")
        }
        if (metrics2do[["do.neishabouricounts"]] == TRUE) {
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_x)), col_msi] <- accmetrics$NeishabouriCount_x
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_y)), col_msi + 1] <- accmetrics$NeishabouriCount_y
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_z)), col_msi + 2] <- accmetrics$NeishabouriCount_z
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_vm)), col_msi + 3] <- accmetrics$NeishabouriCount_vm
          col_msi <- col_msi + 3 # GGIR advances by 3 after four columns; no effect without myfun
          metnames <- c(metnames, "NeishabouriCount_x", "NeishabouriCount_y", "NeishabouriCount_z", "NeishabouriCount_vm")
        }
        metnames <- gsub(pattern = "angle_", replacement = "angle", x = metnames)

        length_acc_metrics <-  length(accmetrics[[1]]) # changing indicator to whatever metric is calculated, EN produces incompatibility when deriving both ENMO and ENMOa
        rm(accmetrics)
        # update blocksize depending on available memory
        BlocksizeNew <- .raw.update.blocksize(blocksize = blocksize, bsc_qc = bsc_qc)
        bsc_qc <- BlocksizeNew$bsc_qc
        blocksize <- BlocksizeNew$blocksize
        # MODULE 2 - non-wear time & clipping
        NWCW <- .raw.nonwear.clipping(data = data, windowsizes = c(ws3, ws2, ws), sf = sf,
                                      clipthres = clipthres, sdcriter = sdcriter, racriter = racriter,
                                      approach = nonwear_approach)
        NWav <- NWCW$nonwear; CWav <- NWCW$clipping; nmin <- NWCW$nmin
        # metalong
        col_mli <- 2
        metalong[count2:((count2 - 1) + length(NWav)),col_mli] <- NWav; col_mli <- col_mli + 1
        metalong[count2:((count2 - 1) + length(NWav)),col_mli] <- CWav; col_mli <- col_mli + 1

        if(light.available) {
          #light (running mean)
          lightc <- cumsum(c(0,light))
          select <- seq(1, length(lightc), by = (ws2 * sf))
          lightmean <- diff(lightc[round(select)]) / abs(diff(round(select)))
          rm(lightc)
          #light (running max)
          lightmax <- matrix(0, length(lightmean), 1)
          for (li in 1:(length(light)/(ws2*sf))) {
            tempm <- max(light[((li - 1) * (ws2 * sf)):(li * (ws2 * sf))])
            if (length(tempm) > 0) {
              lightmax[li] <- tempm[1]
            } else {
              lightmax[li] <- max(light[((li - 1) * (ws2 * sf)):(li * (ws2 * sf))])
            }
          }

          metalong[(count2):((count2 - 1) + length(NWav)), col_mli] <- round(lightmean, digits = n_decimal_places)
          col_mli <- col_mli + 1
          metalong[(count2):((count2 - 1) + length(NWav)), col_mli] <- round(lightmax, digits = n_decimal_places)
          col_mli <- col_mli + 1
        }

        if(use.temp) {
          #temperature (running mean)
          temperaturec <- cumsum(c(0, temperature))
          select <- seq(1, length(temperaturec), by = (ws2 * sf))
          temperatureb <- diff(temperaturec[round(select)]) / abs(diff(round(select)))
          rm(temperaturec)

          metalong[(count2):((count2 - 1) + length(NWav)), col_mli] <- round(temperatureb, digits = n_decimal_places)
          col_mli <- col_mli + 1
        }

        #EN going from sample to ws2
        ENc <- cumsum(c(0, EN))
        select <- seq(1, length(ENc), by = (ws2 * sf)) #<= EN is derived from data, so it needs the new sf
        ENb <- diff(ENc[round(select)]) / abs(diff(round(select)))
        rm(ENc, EN)
        metalong[(count2):((count2 - 1) + length(NWav)), col_mli] <- round(ENb, digits = n_decimal_places)

        if (exists("remaining_epochs", inherits = FALSE)) {
          # Impute long gaps at epoch levels, because imputing them at raw level would
          # be too memory hungry
          gaps_to_fill <- which(remaining_epochs != 1)
          if (length(gaps_to_fill) > 0) {
            nr_before <- c(nrow(metalong), nrow(metashort))
            # metalong
            metalong <- .raw.impute.at.epoch.level(gapsize = floor(remaining_epochs[gaps_to_fill] * (ws3/ws2)) + 1, # plus 1 needed to count for current epoch
                                                   timeseries = metalong,
                                                   gap_index = floor(gaps_to_fill / (ws2 * sf)) + count2, # Using floor so that the gap is filled in the epoch in which it is occurring
                                                   metnames = metricnames_long)
            # metashort
            # added epoch-level nonwear to metashort to get it imputed, then remove it
            metashort <- .raw.impute.at.epoch.level(gapsize = remaining_epochs[gaps_to_fill], # gapsize in epochs
                                                    timeseries = metashort,
                                                    gap_index = floor(gaps_to_fill / (ws3 * sf)) + count, # Using floor so that the gap is filled in the epoch in which it is occurring
                                                    metnames = c("timestamp", metnames)) # epoch level index of gap
            nr_after <- c(nrow(metalong), nrow(metashort))
            count2 <- count2 + (nr_after[1] - nr_before[1])
            count <- count + (nr_after[2] - nr_before[2])
            tr$epochs_long_added <- as.numeric(nr_after[1] - nr_before[1])
            tr$epochs_short_added <- as.numeric(nr_after[2] - nr_before[2])
          }
        }
        col_mli <- col_mli + 1
        count2 <- count2 + nmin
        count <- count + length_acc_metrics
        tr$epochs_short <- as.numeric(length_acc_metrics)
        tr$epochs_long <- as.numeric(nmin)
        rm(data, light, temperature)
        gc()
      } #end of section which is skipped when switchoff == 1
    } else {
      LD <- 0 #once LD < 1 the analysis stops, so this is a trick to stop it
      # stop reading because there is not enough data in this block
    }
    if (isLastBlock) LD <- 0
    if (ceiling(daylimit) != FALSE) {
      if (i == ceiling(daylimit)) { #to speed up testing only read first 'i' blocks of data
        LD <- 0 #once LD < 1 the analysis stops, so this is a trick to stop it
        note("stopped reading data because this analysis is limited to ", ceiling(daylimit), " days")
      }
    }
    tr$elapsed_seconds <- as.numeric(difftime(Sys.time(), t_block, units = "secs"))
    trace[[i]] <- as.data.frame(tr, stringsAsFactors = FALSE)
    i <- i + 1 #go to next block
  }
  # deriving timestamps
  time_short <- NULL; time_long <- NULL
  metricnames_short <- NULL
  windowsizes_out <- windowsizes
  if (!filecorrupt && !filetooshort && !filedoesnotholdday && is.null(metnames)) {
    # GGIR fails here with "object 'metnames' not found"; report the file as too short instead
    note("No epoch could be stored (the first block did not reach one whole non-wear window after ",
         "zero removal and start alignment); treated as too short.")
    filetooshort <- TRUE
  }
  if (!filecorrupt && !filetooshort && !filedoesnotholdday) {
    cut <- count:nrow(metashort)
    if (length(cut) > 1) {
      metashort <- metashort[-cut,]
      # a single row would be coerced to a vector; keep it a 1-row matrix
      if(is.vector(metashort)) {
        metashort <- as.matrix(t(metashort))
      }
    }
    if (nrow(metashort) > 1) {
      ts_short <- .raw.epoch.timestamps(starttime, nrow(metashort), ws3, tz = desiredtz, long = FALSE)
      time_short <- ts_short$time
      metashort[,1] <- ts_short$timestamp
    } else {
      time_short <- rep(NA_real_, nrow(metashort))
    }
    cut2 <- count2:nrow(metalong)
    if (length(cut2) > 1) {
      metalong <- metalong[-cut2,]
      # a single row would be coerced to a vector; keep it a 1-row matrix
      if(is.vector(metalong)) {
        metalong <- as.matrix(t(metalong))
      }
    }
    if (nrow(metalong) > 2) {
      if (length(desiredtz) == 0) {
        note("desiredtz not specified, system timezone used as default")
        desiredtz <- ""
      }
      ts_long <- .raw.epoch.timestamps(starttime, nrow(metalong), ws2, tz = desiredtz, long = TRUE)
      time_long <- ts_long$time
      metalong[, 1] <- ts_long$timestamp
    } else {
      time_long <- rep(NA_real_, nrow(metalong))
    }
    metricnames_short <- c("timestamp", metnames)

    # Following code is needed to make sure that algorithms that produce character value
    # output are not assumed to be numeric
    NbasicMetrics <- length(metricnames_short)
    metashort <- data.frame(A = metashort, stringsAsFactors = FALSE)
    names(metashort) <- metricnames_short
    for (ncolms in 2:NbasicMetrics) {
      metashort[,ncolms] <- as.numeric(metashort[,ncolms])
    }

    metalong <- data.frame(A = metalong, stringsAsFactors = FALSE)
    names(metalong) <- metricnames_long
    for (ncolml in 2:ncol(metalong)) {
      metalong[,ncolml] <- as.numeric(metalong[,ncolml])
    }
  } else {
    metalong <- metashort <- wday <- wdayname <- windowsizes_out <- c()
  }
  if (length(metashort) == 0 | filedoesnotholdday == TRUE) filetooshort <- TRUE
  chunks <- if (length(trace) > 0) do.call(rbind, trace) else NULL
  rows_discarded_end <- NA_real_
  if (!is.null(chunks)) {
    rownames(chunks) <- NULL
    # samples that entered the loop (after imputation) but were neither dropped at the start
    # nor used: the final leftover under one hour, or every partial long epoch of the last block
    rows_discarded_end <- sum(chunks$rows_imputed) - sum(chunks$rows_dropped_start) - sum(chunks$rows_used)
  }
  list(filecorrupt = filecorrupt, filetooshort = filetooshort, NFilePagesSkipped = NFilePagesSkipped,
       metalong = metalong, metashort = metashort, wday = wday, wdayname = wdayname,
       windowsizes = windowsizes_out, bsc_qc = bsc_qc, QClog = QClog,
       starttime = starttime, time_short = time_short, time_long = time_long,
       metricnames_short = metricnames_short, metricnames_long = metricnames_long,
       light.available = light.available, use.temp = use.temp,
       filedoesnotholdday = filedoesnotholdday,
       chunks = chunks, messages = messages, blocks = i - 1,
       rows_discarded_end = rows_discarded_end)
}

# TAIL EXPANSION

#' Expand the Tables Past the End of a Recording That Stops in the Evening
#'
#' GGIR's recordingEndSleepHour option, verbatim from g.part1: when the last long epoch
#' ends within \code{(24 + dayborder - (recordingEndSleepHour - dayborder))} hours of the
#' next midnight, both tables are extended to 8 hours past midnight with synthetic epochs
#' (metashort metrics 0, EN 1, angles \code{round(sin(k / (ws2/ws3))) * 15}; metalong every
#' score 0 and nonwearscore -1) so that a last night can be scored. Off by default. GGIR's
#' \code{M$nonwear[expand_indices] = 0} side effect is kept, and its
#' \code{metalong_expand$en} line is a no-op because the column is EN. The numeric time
#' column is extended with the same instants.
#'
#' @param x A canhrActi_raw_meta with non-empty tables.
#' @param recordingEndSleepHour GGIR's parameter (hour, 19 or later); NULL returns x
#'   unchanged with tail_expansion_log NULL.
#' @param dayborder GGIR's dayborder (default 0).
#' @param desiredtz GGIR's desiredtz.
#' @return x with the tables expanded when the rule fires, \code{tail_expansion_log}
#'   (list(short, long) or NULL) and \code{nonwear} (GGIR's side-effect vector or NULL).
#' @keywords internal
#' @noRd
.raw.tail.expand <- function(x, recordingEndSleepHour = NULL, dayborder = 0, desiredtz = "") {
  if (!inherits(x, "canhrActi_raw_meta")) stop("x must be a canhrActi_raw_meta", call. = FALSE)
  x$tail_expansion_log <- NULL
  if (is.null(recordingEndSleepHour) || is.null(x$metashort) || is.null(x$metalong) ||
      nrow(x$metashort) == 0 || nrow(x$metalong) == 0) {
    return(x)
  }
  M <- as.ggir.M(x)
  params_general <- list(recordingEndSleepHour = recordingEndSleepHour, dayborder = dayborder,
                         desiredtz = desiredtz)
  tail_expansion_log <- NULL
  expand_short <- NULL; expand_long <- NULL
  if (!is.null(params_general[["recordingEndSleepHour"]])) {
    # Identify gap between last timestamp and following midnight
    ws3 <- M$windowsizes[1]
    ws2 <- M$windowsizes[2]
    # Check whether gap is less then criteria
    last_ts <- c(Sys.time(), Sys.time())
    secs_to_midnight <- c(0, 0)
    lastTimeLong <- as.character(tail(M$metalong$timestamp, n = 1))
    lastTimeShort <- as.character(tail(M$metashort$timestamp, n = 1))
    last_ts[1] <- .raw.iso8601.to.posix(x = lastTimeLong, tz = params_general[["desiredtz"]])
    last_ts[2] <- .raw.iso8601.to.posix(x = lastTimeShort, tz = params_general[["desiredtz"]])
    refhour <- 24 + params_general[["dayborder"]]
    for (wsi in 1:2) {
      secs_to_midnight[wsi] <- (refhour * 3600) -
        (as.numeric(format(last_ts[wsi], format = "%H", tz = params_general[["desiredtz"]])) * 3600 +
           as.numeric(format(last_ts[wsi], format = "%M", tz = params_general[["desiredtz"]])) * 60  +
           as.numeric(format(last_ts[wsi], format = "%S", tz = params_general[["desiredtz"]])))
    }
    # only expand if recording ends at 19PM or later
    max_expand_time <- (refhour - (params_general[["recordingEndSleepHour"]] - params_general[["dayborder"]])) * 3600
    if (secs_to_midnight[1] <= max_expand_time) {
      # If yes, expand data
      secs_to_midnight <- secs_to_midnight + (8 * 3600) # also add 8 hour till the morning
      N_long_epochs_expand <- ceiling(secs_to_midnight[1] / ws2) + 1
      N_short_epochs_expand <- ceiling(secs_to_midnight[2] / ws3) + 1
      if (N_short_epochs_expand / (ws2 / ws3) < N_long_epochs_expand) {
        N_short_epochs_expand <- N_long_epochs_expand * (ws2 / ws3)
      }
      # Expand metashort
      NR <- nrow(M$metashort)
      metashort_expand <- M$metashort[NR,]
      metashort_expand[, grep(pattern = "timestamp|angle", x = names(metashort_expand), invert = TRUE, value = FALSE)] <- 0
      if ("EN" %in% names(metashort_expand)) {
        metashort_expand$EN <- 1
      }
      expand_indices <- (NR + 1):(NR + N_short_epochs_expand)
      expand_tsPOSIX <- seq(last_ts[2] + ws3, last_ts[2] + (N_short_epochs_expand * ws3), by = ws3)
      M$metashort[expand_indices,] <- metashort_expand
      M$nonwear[expand_indices] <- 0
      M$metashort$timestamp[expand_indices] <- .raw.posix.to.iso8601(expand_tsPOSIX, tz = params_general[["desiredtz"]])
      anglecol <- grep(pattern = "angle", x = names(metashort_expand), value = FALSE)
      if (length(anglecol) > 0) {
        M$metashort[expand_indices,anglecol] <- round(sin((1:length(expand_indices)) / (ws2/ws3))) * 15
      }
      tail_expansion_log <- list(short = length(expand_indices))
      expand_short <- as.numeric(expand_tsPOSIX)
      # Expand metalong
      NR <- nrow(M$metalong)
      metalong_expand <- M$metalong[NR,]
      metalong_expand[, grep(pattern = "timestamp", x = names(metalong_expand), invert = TRUE, value = FALSE)] <- 0
      metalong_expand[, "nonwearscore"] <- -1
      metalong_expand$en <- tail(M$metalong$en, n = 1)
      expand_indices <- (NR + 1):(NR + N_long_epochs_expand)
      expand_tsPOSIX <- seq(last_ts[1] + ws2, last_ts[1] + (N_long_epochs_expand * ws2), by = ws2)
      M$metalong[expand_indices,] <- metalong_expand
      M$metalong$timestamp[expand_indices] <- .raw.posix.to.iso8601(expand_tsPOSIX, tz = params_general[["desiredtz"]])
      # Keep log of data expansion
      tail_expansion_log[["long"]] <- length(expand_indices)
      expand_long <- as.numeric(expand_tsPOSIX)
    } else {
      tail_expansion_log <- NULL
    }
  }
  if (!is.null(tail_expansion_log)) {
    x$metashort <- .raw.meta.add.time(M$metashort, c(x$metashort$time, expand_short))
    x$metalong <- .raw.meta.add.time(M$metalong, c(x$metalong$time, expand_long))
    x$nonwear <- M$nonwear
  }
  x$tail_expansion_log <- tail_expansion_log
  x
}

#' Insert the Numeric Time Column After the Timestamp
#' @keywords internal
#' @noRd
.raw.meta.add.time <- function(tbl, time) {
  tbl$time <- time
  tbl[c("timestamp", "time", setdiff(names(tbl), c("timestamp", "time")))]
}

# ENTRY POINT

#' Epoch-Level Tables of a Raw Accelerometer File as GGIR Part 1 Builds Them
#'
#' Runs GGIR's g.getmeta on an inspected file: reads it in blocks, imputes time gaps, applies
#' the calibration, computes the short-epoch acceleration metrics (ENMO and the z angle by
#' default) and the long-epoch non-wear and clipping scores, and assembles the two tables
#' GGIR stores as metashort and metalong. Every stage is transcribed from GGIR 3.3-9, so
#' with the same calibration and parameters the tables are identical() to GGIR's.
#'
#' @details
#' Stages, in GGIR's order per block: read (24 h of recorded samples for gt3x, so a block
#' spans however much wall clock idle sleep stretched it over); gap imputation (k = 0.25 s,
#' gaps above 90 min only filled to the long-epoch cuts) for csv, ad-hoc csv and gt3x;
#' leftover samples of the previous block prepended; on block 1 the start alignment to the
#' next whole ws2/60 minutes and the weekday; whole long epochs kept, the rest carried;
#' calibration with \code{base::scale}; metrics rounded to 4 decimals; non-wear and
#' clipping per block, so the one-hour windows are truncated at every block edge; the
#' replication of the epochs that stand for gaps above 90 min (nonwearscore 3, metrics 0,
#' EN 1, angle copied). The timestamps are regenerated from the aligned start, never read
#' from the data, and the character matrices become data.frames through \code{as.numeric},
#' the 15-significant-digit round trip GGIR's stored values carry.
#'
#' Floors that come from GGIR: the first block needs 2 h and one sample of recorded data or
#' the file is too short (no tables); a final leftover shorter than one hour is dropped even
#' when it holds whole 15-minute epochs; up to 15 minutes are dropped at the start. The
#' tail expansion of g.part1 (\code{recordingEndSleepHour}) is applied when that parameter
#' is set. GGIR's warnings and the readers' warnings are collected in \code{messages}
#' rather than raised.
#'
#' Parameters read: windowsizes, desiredtz, configtz, chunksize, dynrange,
#' nonwear_range_threshold, nonwear_approach, imputeTimegaps, every do.* metric flag, n, lb,
#' hb, zc.lb, zc.hb, zc.sb, zc.order, actilife_LFE, recordingEndSleepHour, dayborder,
#' ggir_exact, progress, and the reader's parameters (interpolationType, frequency_tol,
#' rmc.*).
#'
#' @param info A \code{canhrActi_raw_info} from \code{\link{raw.inspect}}.
#' @param calibration The calibration to apply: a \code{canhrActi_raw_calibration} from
#'   \code{\link{raw.calibrate}} (its coefficients after GGIR's decision rule), a GGIR C list
#'   from a meta_*.RData milestone, a data_quality_report-shaped data.frame or csv path
#'   (GGIR's retrieve semantics), or NULL for no calibration (identity, GGIR's default C).
#' @param params A \code{\link{raw.params}} object, or NULL to use the parameters the file
#'   was inspected with (\code{info$params}; GGIR's defaults when absent).
#' @param progress NULL or a \code{function(stage, i, n, message)} called before every block
#'   read with stage "getmeta"; overrides \code{params$progress}. With stream_gt3x on, a
#'   .gt3x gets one more call after the last block, with i NA and a message such as
#'   "gt3x reader: 2 of 2 blocks read directly".
#' @param daylimit FALSE (whole file), or the number of blocks to process (GGIR's daylimit;
#'   a testing convenience: 1 gives the first 24 h of recorded samples).
#' @param ... Individual parameter overrides, e.g. \code{do.en = TRUE}; routed through
#'   \code{raw.params()} so they get GGIR's checks.
#'
#' @return An object of class "canhrActi_raw_meta": a list with
#' \describe{
#'   \item{metashort}{data.frame, one row per short epoch (ws3): \code{timestamp} (GGIR's
#'     character "%Y-%m-%dT%H:%M:%S%z" in desiredtz, verbatim), \code{time} (numeric Unix
#'     seconds, canhrActi addition), then one column per metric switched on, in GGIR's
#'     order, with angle_ renamed to angle (anglez, ENMO by default).}
#'   \item{metalong}{data.frame, one row per long epoch (ws2): timestamp, time,
#'     nonwearscore (0 to 3; 3 for epochs standing for a gap above 90 min; -1 for tail
#'     expansion rows), clippingscore, then lightmean and lightpeak when the file has light,
#'     temperaturemean when it has a usable temperature, and EN.}
#'   \item{qclog}{GGIR's QClog: one row per block from the gap imputation (imputed, start,
#'     end, blockLengthSeconds, timegaps_n, timegaps_min; end is start + samples, not a
#'     time) preceded by the reader's rows for cwa and Parmay; NULL when nothing was logged.}
#'   \item{wday, wdayname}{Weekday of the first sample (1 = Sunday) and its name.}
#'   \item{windowsizes}{c(ws3, ws2, ws) after GGIR's coercions.}
#'   \item{starttime}{POSIXct of the first epoch in desiredtz.}
#'   \item{filecorrupt, filetooshort, nfilepagesskipped}{GGIR's file flags.}
#'   \item{chunks}{data.frame with one row per block: block, rows_read, rows_imputed,
#'     rows_carried_in, rows_dropped_start, rows_used, rows_carried_out, epochs_short,
#'     epochs_long, epochs_short_added, epochs_long_added (epoch-level replicas), gaps,
#'     minutes_imputed, span_seconds (wall clock the raw block covers, first sample
#'     to the end of the last, including unfilled long gaps), is_last_block,
#'     elapsed_seconds.}
#'   \item{rows_discarded_end}{Samples that reached the loop but were never used: the final
#'     leftover under one hour, or the partial long epoch of the last block (GGIR keeps no
#'     record of these).}
#'   \item{bsc_qc}{GGIR's block size log (time, memory in MB), one row per block.}
#'   \item{calibration}{The coefficients applied (offset, scale, tempoffset, meantempcal),
#'     their source and whether they differ from the identity.}
#'   \item{tail_expansion_log, nonwear}{NULL unless recordingEndSleepHour fired.}
#'   \item{light_available, use_temp, metric_names, messages, skipped, file, settings,
#'     elapsed}{Bookkeeping: which optional columns exist; the metric column names; collected
#'     warning texts; whether the file was skipped at inspection; path and filename; the
#'     settings used (sf, monc, dformc, blocksize, clipthres, sdcriter, racriter,
#'     windowsizes, nonwear_approach, imputeTimegaps, desiredtz, configtz, ggir_exact,
#'     daylimit); seconds elapsed.}
#' }
#' Use \code{\link{as.ggir.M}} to obtain GGIR's M list for comparison with a milestone.
#'
#' @examples
#' \dontrun{
#' info <- raw.inspect("recording.gt3x", desiredtz = "America/Anchorage")
#' cal <- raw.calibrate(info)
#' meta <- raw.getmeta(info, cal)
#' head(meta$metashort); table(meta$metalong$nonwearscore)
#' M <- as.ggir.M(meta)
#' }
#' @export
raw.getmeta <- function(info, calibration = NULL, params = NULL, progress = NULL,
                        daylimit = FALSE, ...) {
  t0 <- Sys.time()
  if (!is.list(info) || !all(c("monc", "dformc") %in% names(info))) {
    stop("info must be a canhrActi_raw_info object from raw.inspect()", call. = FALSE)
  }
  params <- .raw.calibrate.params(info, params, list(...))
  if (is.null(progress)) progress <- .raw.param(params, "progress", NULL)
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be NULL or a function(stage, i, n, message)", call. = FALSE)
  }
  if (!(isFALSE(daylimit) || (is.numeric(daylimit) && length(daylimit) == 1 && !is.na(daylimit) && daylimit > 0))) {
    stop("daylimit must be FALSE or a positive number of blocks", call. = FALSE)
  }
  ggir_exact <- isTRUE(.raw.param(params, "ggir_exact", TRUE))
  filename <- if (!is.null(info$filename)) info$filename else basename(as.character(info$path))
  read_path <- if (!is.null(info$read_path)) info$read_path else info$path
  desiredtz <- .raw.param(params, "desiredtz", "")
  configtz <- .raw.param(params, "configtz", NULL)
  windowsizes <- .raw.param(params, "windowsizes", c(5, 900, 3600))
  sf <- info$sf
  mon <- info$monc
  dformat <- info$dformc
  cal <- .raw.getmeta.coefficients(calibration, filename)
  bsc_qc0 <- data.frame(time = c(), size = c(), stringsAsFactors = FALSE)

  finish <- function(obj, loop = NULL, blocksize = NULL, ncb = NULL) {
    obj$calibration <- cal
    obj$file <- list(path = if (is.null(info$path)) NA_character_ else info$path, filename = filename)
    obj$settings <- list(sf = sf, monc = mon, dformc = dformat, blocksize = blocksize,
                         clipthres = ncb$clipthres, sdcriter = ncb$sdcriter, racriter = ncb$racriter,
                         windowsizes = windowsizes,
                         nonwear_approach = .raw.param(params, "nonwear_approach", "2023"),
                         imputeTimegaps = .raw.param(params, "imputeTimegaps", TRUE),
                         desiredtz = desiredtz, configtz = configtz, ggir_exact = ggir_exact,
                         daylimit = daylimit)
    obj$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    structure(obj, class = "canhrActi_raw_meta")
  }
  empty <- function(filecorrupt, filetooshort, messages, skipped = FALSE) {
    # the corrupt and too-short shapes of g.getmeta
    list(metashort = NULL, metalong = NULL, qclog = NULL, wday = NULL, wdayname = NULL,
         windowsizes = NULL, starttime = NULL,
         filecorrupt = filecorrupt, filetooshort = filetooshort, nfilepagesskipped = 0,
         chunks = NULL, rows_discarded_end = NA_real_, bsc_qc = bsc_qc0,
         light_available = FALSE, use_temp = FALSE,
         metric_names = NULL, messages = messages, skipped = skipped,
         tail_expansion_log = NULL, nonwear = NULL)
  }

  if (isTRUE(info$skipped) || is.null(mon) || is.null(dformat)) {
    return(finish(empty(FALSE, TRUE, "The file was skipped at inspection (too small); no epochs derived",
                        skipped = TRUE)))
  }
  if (is.null(sf)) { # sf is NULL for corrupt files
    return(finish(empty(TRUE, FALSE, paste0("No sample frequency could be read from ", filename,
                                            " (corrupt file); no epochs derived"))))
  }

  # non-wear, clipping and block size thresholds (the Parmay dynamic range is info$dynrange_file)
  ncb <- .raw.clip.block.params(info, params = params, pass = "getmeta")
  blocksize <- ncb$blocksize
  metrics <- .raw.metric.flags(params)

  # called on its own, the stage removes the .gt3x extraction it makes
  own <- .raw.gt3x.stage.owner(read_path, params)
  if (!is.null(own)) on.exit(try(.raw.gt3x.extract.cleanup(own$path, exdir = own$dir), silent = TRUE), add = TRUE)
  # how many blocks the stream reader served, reported once after the pass
  report <- !is.null(progress) && isTRUE(.raw.param(params, "stream_gt3x", FALSE)) &&
    is.character(read_path) && grepl("\\.gt3x$", read_path, ignore.case = TRUE)
  if (report) .raw.gt3x.tally()
  loop <- .raw.getmeta.loop(info, sf = sf, mon = mon, dformat = dformat, blocksize = blocksize,
                            clipthres = ncb$clipthres, sdcriter = ncb$sdcriter, racriter = ncb$racriter,
                            windowsizes = windowsizes,
                            offset = cal$offset, scale = cal$scale, tempoffset = cal$tempoffset,
                            meantempcal = cal$meantempcal,
                            metrics = metrics,
                            n = .raw.param(params, "n", 4), lb = .raw.param(params, "lb", 0.2),
                            hb = .raw.param(params, "hb", 15),
                            zc.lb = .raw.param(params, "zc.lb", 0.25), zc.hb = .raw.param(params, "zc.hb", 3),
                            zc.sb = .raw.param(params, "zc.sb", 0.01), zc.order = .raw.param(params, "zc.order", 2),
                            actilife_LFE = .raw.param(params, "actilife_LFE", FALSE),
                            imputeTimegaps = isTRUE(.raw.param(params, "imputeTimegaps", TRUE)),
                            nonwear_approach = .raw.param(params, "nonwear_approach", "2023"),
                            desiredtz = desiredtz, configtz = configtz,
                            read_params = params, ggir_exact = ggir_exact, progress = progress,
                            daylimit = daylimit, filename = read_path)
  if (report) {
    progress("getmeta", NA_integer_, NA_integer_,
             paste0("gt3x reader: ", .raw.gt3x.tally.env$served, " of ", .raw.gt3x.tally.env$n,
                    " blocks read directly"))
  }

  metashort <- loop$metashort
  metalong <- loop$metalong
  if (!is.null(metashort)) {
    metashort <- .raw.meta.add.time(metashort, loop$time_short)
    metalong <- .raw.meta.add.time(metalong, loop$time_long)
  }
  starttime <- NULL
  if (!is.null(loop$starttime) && !is.null(metashort)) {
    tz_start <- if (length(desiredtz) == 0) "" else desiredtz
    starttime <- as.POSIXct(round(as.numeric(loop$starttime)), origin = "1970-01-01", tz = tz_start)
  }
  obj <- list(metashort = metashort, metalong = metalong, qclog = loop$QClog,
              wday = loop$wday, wdayname = loop$wdayname, windowsizes = loop$windowsizes,
              starttime = starttime,
              filecorrupt = loop$filecorrupt, filetooshort = loop$filetooshort,
              nfilepagesskipped = loop$NFilePagesSkipped,
              chunks = loop$chunks, rows_discarded_end = loop$rows_discarded_end, bsc_qc = loop$bsc_qc,
              light_available = loop$light.available, use_temp = loop$use.temp,
              metric_names = if (is.null(loop$metricnames_short)) NULL else loop$metricnames_short[-1],
              messages = loop$messages, skipped = FALSE,
              tail_expansion_log = NULL, nonwear = NULL)
  obj <- finish(obj, loop = loop, blocksize = blocksize, ncb = ncb)
  # tail expansion
  obj <- .raw.tail.expand(obj, recordingEndSleepHour = .raw.param(params, "recordingEndSleepHour", NULL),
                          dayborder = .raw.param(params, "dayborder", 0), desiredtz = desiredtz)
  obj$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  obj
}

#' GGIR's M List From a canhrActi Epoch Table Object
#'
#' Returns the tables in the shape GGIR's g.getmeta returns and g.part1 saves in its part-1
#' milestone (the M object of meta_*.RData), so that identical() comparisons and milestone
#' export work: the canhrActi numeric \code{time} column is dropped from both tables. For a
#' corrupt file the tables, wday, wdayname and windowsizes are NULL, as in GGIR. When the
#' tail expansion fired, the \code{nonwear} vector it leaves on M follows as the eleventh
#' member.
#'
#' @param x A canhrActi_raw_meta.
#' @return A named list in GGIR's member order.
#' @examples
#' \dontrun{
#' M <- as.ggir.M(raw.getmeta(info, cal))
#' identical(M$metashort, stored_M$metashort)
#' }
#' @export
as.ggir.M <- function(x) {
  if (!inherits(x, "canhrActi_raw_meta")) {
    stop("x must be a canhrActi_raw_meta object", call. = FALSE)
  }
  drop_time <- function(tbl) {
    if (is.null(tbl)) return(NULL)
    tbl$time <- NULL
    tbl
  }
  M <- list(filecorrupt = x$filecorrupt, filetooshort = x$filetooshort,
            NFilePagesSkipped = x$nfilepagesskipped,
            metalong = drop_time(x$metalong), metashort = drop_time(x$metashort),
            wday = x$wday, wdayname = x$wdayname,
            windowsizes = x$windowsizes, bsc_qc = x$bsc_qc, QClog = x$qclog)
  if (!is.null(x$nonwear)) M$nonwear <- x$nonwear
  M
}

#' Print Method for a Raw Epoch Table Object
#'
#' @param x A canhrActi_raw_meta.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_meta <- function(x, ...) {
  cat("\ncanhrActi raw epoch tables (GGIR g.getmeta)\n")
  if (!is.null(x$file$filename) && !is.na(x$file$filename)) cat("  file:       ", x$file$filename, "\n", sep = "")
  if (isTRUE(x$skipped)) {
    cat("  status:      skipped at inspection (too small)\n")
  } else if (isTRUE(x$filecorrupt)) {
    cat("  status:      file corrupt; no epochs\n")
  } else if (isTRUE(x$filetooshort)) {
    cat("  status:      file too short; no epochs\n")
  } else {
    ws <- x$windowsizes
    cat("  epochs:      ", nrow(x$metashort), " short (", ws[1], " s), ", nrow(x$metalong),
        " long (", ws[2], " s)\n", sep = "")
    cat("  from:        ", x$metashort$timestamp[1], " (", x$wdayname, ")  to  ",
        x$metashort$timestamp[nrow(x$metashort)], "\n", sep = "")
    cat("  metrics:     ", paste(x$metric_names, collapse = ", "), "\n", sep = "")
    nw <- table(factor(x$metalong$nonwearscore, levels = c(-1, 0, 1, 2, 3)))
    cat("  nonwear:     ", paste0(names(nw), ":", as.integer(nw), collapse = "  "), "\n", sep = "")
    cat("  clipping:    ", sum(x$metalong$clippingscore > 0), " of ", nrow(x$metalong),
        " long epochs with any clipped sample\n", sep = "")
    if (!is.null(x$qclog)) {
      cat("  gaps:        ", sum(x$qclog$timegaps_n, na.rm = TRUE), " gaps, ",
          round(sum(x$qclog$timegaps_min, na.rm = TRUE), 2), " min imputed over ",
          nrow(x$qclog), " log rows\n", sep = "")
    }
    cat("  calibration: ", x$calibration$source,
        if (isTRUE(x$calibration$applied)) " (applied)" else " (identity)", "\n", sep = "")
    if (!is.null(x$tail_expansion_log)) {
      cat("  tail expansion: ", x$tail_expansion_log$short, " short and ",
          x$tail_expansion_log$long, " long epochs appended\n", sep = "")
    }
    if (is.data.frame(x$chunks) && nrow(x$chunks) > 0) {
      cat("  blocks:\n")
      print(x$chunks[, c("block", "rows_read", "rows_imputed", "rows_used", "epochs_short",
                         "epochs_long", "gaps", "minutes_imputed")], row.names = FALSE)
    }
  }
  if (length(x$messages) > 0) {
    cat("  messages:\n")
    for (m in x$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(x$elapsed) && !is.na(x$elapsed)) cat("  elapsed:     ", round(x$elapsed, 1), " s\n", sep = "")
  invisible(x)
}
