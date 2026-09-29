# Ported from GGIR 3.3-9 R/g.calibrate.R and the calibration steps of R/g.part1.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles, Zhou Fang and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: GGIR's parameter objects
# became a canhrActi params list, the block reader is .raw.read.block, the nested
# rollMean and rollSD are .raw.roll.mean and .raw.roll.sd, the identity-reset rule of
# g.part1 is .raw.calibration.decision, console printing was removed and GGIR's warnings
# are collected in a messages vector. The arithmetic of the still-window features, the
# selection, the sphere check, the fit and the acceptance test is GGIR's, operand order
# and function choices included.

# WINDOW FEATURES

#' Mean per Non-Overlapping Window
#'
#' GGIR's rollMean: the mean of \code{x} over consecutive windows of \code{sf * epoch}
#' samples, taken from a cumulative sum. The cumulative sum is not the same
#' floating-point operation as \code{mean()} per window, so it stays.
#'
#' @param x Numeric vector; a partial last window is dropped.
#' @param sf Sample frequency in Hz.
#' @param epoch Window length in seconds.
#' @return Numeric vector, one mean per window.
#' @keywords internal
#' @noRd
.raw.roll.mean <- function(x, sf, epoch = 10) {
  x <- cumsum(c(0, x))
  select <- seq(1, length(x), by = sf * epoch)
  y <- diff(x[round(select)]) / abs(diff(round(select)))
  return(y)
}

#' Sample SD per Non-Overlapping Window
#'
#' GGIR's rollSD: \code{x} reshaped to \code{sf * epoch} rows, \code{stats::sd} per
#' column. A length that is not a multiple of the window is an error, as in GGIR.
#'
#' @inheritParams .raw.roll.mean
#' @return Numeric vector, one sample SD per window.
#' @keywords internal
#' @noRd
.raw.roll.sd <- function(x, sf, epoch = 10) {
  dim(x) <- c(sf * epoch, ceiling(length(x) / (sf * epoch)))
  y <- apply(x, 2, stats::sd)
  return(y)
}

# CALIBRATION OBJECT

#' Empty per-chunk trace
#' @keywords internal
#' @noRd
.raw.calibration.chunks0 <- function() {
  data.frame(block = integer(0), rows = numeric(0), hours = numeric(0),
             still_windows = integer(0), iterations = integer(0),
             error_start = numeric(0), error_end = numeric(0), accepted = logical(0),
             rows_read = numeric(0), zeros_removed = numeric(0), rows_carried = numeric(0),
             qcmessage = character(0), stringsAsFactors = FALSE)
}

#' Build a canhrActi_raw_calibration Object
#'
#' Members whose GGIR value is \code{c()} are kept as NULL list members so their
#' length-0 state survives (g.part1 tests \code{length(cal.error.end) == 0}). Edit one
#' with \code{x["member"] <- list(NULL)}, not \code{x$member <- NULL}.
#'
#' @param scale,offset,tempoffset The coefficients as they stand.
#' @param cal_error_start,cal_error_end GGIR's rounded errors, or NULL.
#' @param spheredata,npoints,nhoursused,qcmessage,use_temp,meantempcal,bsc_qc GGIR's members.
#' @param chunks The per-block trace.
#' @param source "fit", "supplied" or "not_attempted".
#' @param supplied_from NA, or the shape a supplied calibration came in.
#' @param attempted Whether the block loop ran.
#' @param temp_available Whether a temperature column existed.
#' @param filequality The block-1 file quality record, or NULL.
#' @param messages Character vector of collected warning texts.
#' @param rows_unused Samples read but never used in a feature window.
#' @return A list of class "canhrActi_raw_calibration".
#' @keywords internal
#' @noRd
.raw.calibration.object <- function(scale = c(1, 1, 1), offset = c(0, 0, 0), tempoffset = c(0, 0, 0),
                                    cal_error_start = NULL, cal_error_end = NULL,
                                    spheredata = NULL, npoints = NULL, nhoursused = 0,
                                    qcmessage = "Autocalibration not done", use_temp = TRUE,
                                    meantempcal = NULL, bsc_qc = NULL,
                                    chunks = .raw.calibration.chunks0(),
                                    source = c("fit", "supplied", "not_attempted"),
                                    supplied_from = NA_character_,
                                    attempted = FALSE, temp_available = NA,
                                    filequality = NULL, messages = character(),
                                    rows_unused = NA_real_) {
  source <- match.arg(source)
  structure(list(
    scale = scale, offset = offset, tempoffset = tempoffset,
    cal_error_start = cal_error_start, cal_error_end = cal_error_end,
    spheredata = spheredata, npoints = npoints, nhoursused = nhoursused,
    qcmessage = qcmessage, use_temp = use_temp, meantempcal = meantempcal, bsc_qc = bsc_qc,
    applied = FALSE, reset = FALSE, check_backup = FALSE, decision_rule = FALSE,
    chunks = chunks,
    fit = if (source == "fit") list(scale = scale, offset = offset, tempoffset = tempoffset) else NULL,
    source = source, supplied_from = supplied_from,
    attempted = attempted, temp_available = temp_available,
    filequality = filequality, messages = messages, rows_unused = rows_unused,
    file = list(path = NA_character_, filename = NA_character_),
    settings = list(), elapsed = NA_real_
  ), class = "canhrActi_raw_calibration")
}

# DECISION RULE

#' Apply GGIR's Identity-Reset Rule to a Fitted Calibration
#'
#' g.calibrate returns the coefficients of the last fit whatever its QC message. g.part1
#' then resets them to the identity when no starting error exists, when no fit was made,
#' or when the 5-decimal starting error is strictly smaller than the end error.
#' Everything else is applied, including a fit whose message says "possibly not good
#' enough" and a tie of the two rounded errors. The strict comparison is GGIR's.
#'
#' @param cal A canhrActi_raw_calibration holding the fit.
#' @return The object with the coefficients reset when the rule says so, and the members
#'   \code{reset}, \code{check_backup} (GGIR's check.backup.cal.coef),
#'   \code{decision_rule} and \code{applied} set.
#' @keywords internal
#' @noRd
.raw.calibration.decision <- function(cal) {
  if (!inherits(cal, "canhrActi_raw_calibration")) {
    stop("cal must be a canhrActi_raw_calibration object", call. = FALSE)
  }
  cal.error.end <- cal$cal_error_end
  cal.error.start <- cal$cal_error_start
  if (length(cal.error.start) == 0) {
    # file too short or corrupt for a starting error
    cal.error.start <- NA
  }
  check.backup.cal.coef <- FALSE
  if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
    cal$scale <- c(1,1,1); cal$offset <- c(0,0,0); cal$tempoffset <- c(0,0,0)
    check.backup.cal.coef <- TRUE
  } else {
    if (cal.error.start < cal.error.end) {
      cal$scale <- c(1,1,1); cal$offset <- c(0,0,0); cal$tempoffset <-  c(0,0,0)
      check.backup.cal.coef <- TRUE
    }
  }
  cal$reset <- check.backup.cal.coef
  cal$check_backup <- check.backup.cal.coef
  cal$decision_rule <- TRUE
  cal$applied <- !check.backup.cal.coef && cal$source != "not_attempted" && length(cal.error.end) > 0
  cal
}

# SUPPLIED CALIBRATION

#' Read one cell of a coefficient table, NULL when the column is absent
#' @keywords internal
#' @noRd
.raw.calibration.cell <- function(row, name, numeric = FALSE) {
  j <- which(colnames(row) == name)
  if (length(j) == 0) return(NULL)
  v <- row[1, j]
  if (numeric) v <- as.numeric(v)
  v
}

#' Calibration Coefficients From a data_quality_report-Shaped Table
#'
#' The filename column is matched after stripping a leading "meta_" and a trailing
#' ".RData", the first matching row wins, and the members are read by column name. A
#' member whose column is absent is NULL (GGIR holds a zero-column data.frame; both have
#' length 0). data_quality_report.csv has no meantempcal column, so a retrieved
#' calibration always drops the temperature term. A table without a filename column is
#' accepted when it has one row. The six scale and offset columns must be present.
#'
#' @param tbl A data.frame (one row of data_quality_report.csv, or the whole file).
#' @param filename The file name to match.
#' @return A canhrActi_raw_calibration, or NULL when the table has no row for the file.
#' @keywords internal
#' @noRd
.raw.calibration.from.table <- function(tbl, filename, supplied_from = "table") {
  bcc.data <- as.data.frame(tbl, stringsAsFactors = FALSE)
  if ("filename" %in% colnames(bcc.data)) {
    bcc.data$filename <- as.character(bcc.data$filename)
    for (nri in 1:nrow(bcc.data)) {
      tmp <- unlist(strsplit(as.character(bcc.data$filename[nri]),"meta_"))
      if (length(tmp) > 1) {
        bcc.data$filename[nri] <- tmp[2]
        bcc.data$filename[nri] <- unlist(strsplit(bcc.data$filename[nri],".RData"))[1]
      }
    }
    if (length(which(as.character(bcc.data$filename) == filename)) > 0) {
      bcc.i <- which(bcc.data$filename == filename)
    } else {
      return(NULL)
    }
  } else if (nrow(bcc.data) == 1) {
    bcc.i <- 1
  } else {
    stop("A calibration table without a filename column must have exactly one row", call. = FALSE)
  }
  bcc.cal.error.start <- which(colnames(bcc.data) == "cal.error.start")
  bcc.cal.error.end <- which(colnames(bcc.data) == "cal.error.end")
  bcc.scalei <- which(colnames(bcc.data) == "scale.x" | colnames(bcc.data) == "scale.y" | colnames(bcc.data) == "scale.z")
  bcc.offseti <- which(colnames(bcc.data) == "offset.x" | colnames(bcc.data) == "offset.y" | colnames(bcc.data) == "offset.z")
  bcc.temp.offseti <- which(colnames(bcc.data) == "temperature.offset.x" | colnames(bcc.data) == "temperature.offset.y" | colnames(bcc.data) == "temperature.offset.z")
  if (length(bcc.scalei) != 3 || length(bcc.offseti) != 3) {
    stop("The calibration table needs the columns scale.x, scale.y, scale.z, offset.x, offset.y and offset.z",
         call. = FALSE)
  }
  row <- bcc.data[bcc.i[1], , drop = FALSE]
  .raw.calibration.object(
    scale = as.numeric(bcc.data[bcc.i[1],bcc.scalei]),
    offset = as.numeric(bcc.data[bcc.i[1],bcc.offseti]),
    tempoffset = as.numeric(bcc.data[bcc.i[1],bcc.temp.offseti]),
    meantempcal = .raw.calibration.cell(row, "meantempcal"),
    cal_error_start = .raw.calibration.cell(row, "cal.error.start", numeric = TRUE),
    cal_error_end = .raw.calibration.cell(row, "cal.error.end", numeric = TRUE),
    qcmessage = .raw.calibration.cell(row, "QCmessage"),
    npoints = .raw.calibration.cell(row, "n.10sec.windows"),
    nhoursused = .raw.calibration.cell(row, "n.hours.considered"),
    use_temp = .raw.calibration.cell(row, "use.temperature"),
    source = "supplied", supplied_from = supplied_from)
}

#' Turn a Supplied Calibration Into a canhrActi_raw_calibration
#'
#' GGIR's backup.cal.coef retrieve path: the coefficients are used as given, with no
#' refit and no identity-reset rule. Accepted shapes: a canhrActi_raw_calibration, a GGIR
#' C list, a data.frame shaped like data_quality_report.csv, or the path to such a csv
#' (read with data.table::fread as GGIR does, utils::read.csv without data.table). A
#' table with no row for the file gives NULL, and raw.calibrate then fits.
#'
#' @param calibration One of the shapes above.
#' @param filename File name used to match a table's filename column.
#' @return A canhrActi_raw_calibration or NULL.
#' @keywords internal
#' @noRd
.raw.calibration.supplied <- function(calibration, filename) {
  if (inherits(calibration, "canhrActi_raw_calibration")) {
    calibration$source <- "supplied"
    calibration$supplied_from <- "canhrActi_raw_calibration"
    calibration$chunks <- .raw.calibration.chunks0()
    calibration$attempted <- FALSE
    return(calibration)
  }
  # retrieved coefficients are applied as given
  retrieved <- function(obj) {
    obj$applied <- TRUE
    obj$reset <- FALSE
    obj$check_backup <- TRUE
    obj$decision_rule <- FALSE
    obj
  }
  if (is.character(calibration)) {
    if (length(calibration) != 1 || !file.exists(calibration)) {
      stop("calibration must be an existing csv path, a data.frame, a GGIR C list or a canhrActi_raw_calibration",
           call. = FALSE)
    }
    if (requireNamespace("data.table", quietly = TRUE)) {
      bcc.data <- data.table::fread(calibration, data.table = FALSE)
    } else {
      bcc.data <- utils::read.csv(calibration, stringsAsFactors = FALSE, check.names = FALSE)
    }
    obj <- .raw.calibration.from.table(bcc.data, filename, supplied_from = "csv")
    return(if (is.null(obj)) NULL else retrieved(obj))
  }
  if (is.data.frame(calibration)) {
    obj <- .raw.calibration.from.table(calibration, filename, supplied_from = "data.frame")
    return(if (is.null(obj)) NULL else retrieved(obj))
  }
  if (is.list(calibration) && any(c("QCmessage", "cal.error.start", "scale") %in% names(calibration))) {
    C <- calibration
    get <- function(nm, default = NULL) if (nm %in% names(C)) C[[nm]] else default
    obj <- .raw.calibration.object(
      scale = get("scale", c(1, 1, 1)), offset = get("offset", c(0, 0, 0)),
      tempoffset = get("tempoffset", c(0, 0, 0)),
      cal_error_start = get("cal.error.start"), cal_error_end = get("cal.error.end"),
      spheredata = get("spheredata"), npoints = get("npoints"), nhoursused = get("nhoursused", 0),
      qcmessage = get("QCmessage", "Autocalibration not done"), use_temp = get("use.temp", TRUE),
      meantempcal = get("meantempcal"), bsc_qc = get("bsc_qc"),
      source = "supplied", supplied_from = "ggir_C")
    return(retrieved(obj))
  }
  stop("calibration must be a csv path, a data.frame, a GGIR C list or a canhrActi_raw_calibration",
       call. = FALSE)
}

#' Resolve the Parameter List for raw.calibrate
#'
#' NULL takes the list stored on the inspection object, else raw.params(). Overrides are
#' routed through raw.params() so they get its checks.
#'
#' @keywords internal
#' @noRd
.raw.calibrate.params <- function(info, params = NULL, overrides = list()) {
  if (is.null(params)) {
    if (!is.null(info$params)) {
      params <- info$params
    } else {
      params <- raw.params()
    }
  }
  if (!is.list(params)) stop("params must be a list (see raw.params())", call. = FALSE)
  if (length(overrides) > 0) {
    if (is.null(names(overrides)) || any(names(overrides) == "")) {
      stop("Overrides passed through ... must be named, for example minloadcrit = 1", call. = FALSE)
    }
    known <- names(.raw.params.defaults())
    base <- params[names(params) %in% known]
    for (nm in names(overrides)) base[nm] <- list(overrides[[nm]])
    rebuilt <- do.call(raw.params, base)
    for (nm in names(params)) if (!(nm %in% known)) rebuilt[nm] <- list(params[[nm]])
    params <- rebuilt
  }
  params
}

# THE FIT

#' The Auto-Calibration Loop of GGIR's g.calibrate
#'
#' Reads the file in blocks, removes exact-zero samples, keeps whole hours (the remainder
#' is carried to the next block and a final partial hour is lost) and computes per
#' 10-second window the mean Euclidean norm, the mean and sample SD of each axis and,
#' when a usable temperature column exists, the mean temperature. After every block the
#' still windows are selected from all features read so far, the sphere coverage is
#' checked, the iteratively reweighted sphere fit is run and the acceptance rule is
#' tested; the loop stops early only when the fit is accepted.
#'
#' @details Differences from g.calibrate: the reader is \code{.raw.read.block}, called
#'   with no previous last value or time (so the ad-hoc csv gap imputation restarts every
#'   block, as in GGIR) and a NULL header on every block; \code{options(warn = -1)}
#'   around the read became a handler that records warnings in \code{messages}, where
#'   the temperature and rmc.noise warnings go too; a progress callback and a per-block
#'   trace were added. Kept as in GGIR: a leftover of exactly one row is not prepended to
#'   the next block, a block shorter than one hour after the leftover is neither used nor
#'   carried, and the final empty read of a gt3x, cwa or csv file refits on the same
#'   features (it is where GGIR's last message is set).
#'
#' @param info A canhrActi_raw_info from raw.inspect().
#' @param sf Sample frequency in Hz.
#' @param mon Monitor code after the Verisense to ActiGraph mapping.
#' @param dformat Format code.
#' @param blocksize The calibration block size from \code{.raw.blocksize(info, "calibrate")}.
#' @param spherecrit,minloadcrit,rmc.noise GGIR's params_rawdata members.
#' @param read_params The params list forwarded to \code{.raw.read.block}.
#' @param progress NULL or a function(stage, i, n, message) called before every block
#'   read with stage "calibrate"; n is NA because the number of blocks is not known.
#' @return A list with GGIR's twelve C members under GGIR's names, plus temp.available,
#'   chunks, messages, filequality (block 1) and rows_unused.
#' @keywords internal
#' @noRd
.raw.calibrate.fit <- function(info, sf, mon, dformat, blocksize,
                               spherecrit = 0.3, minloadcrit = 168, rmc.noise = 13,
                               read_params = NULL, progress = NULL) {
  messages <- character()
  note <- function(...) messages <<- c(messages, paste0(...))
  trace <- list()
  filequality_first <- NULL
  rows_read_total <- zeros_total <- rows_used_total <- 0
  use.temp <- temp.available <- TRUE

  filequality <- data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                            filedoesnotholdday = FALSE, stringsAsFactors = FALSE)
  calibEpochSize <- 10 # window of the 2014 paper, seconds
  blockResolution <- 3600 # whole hours, seconds
  cal.error.start <- cal.error.end <- c()
  spheredata <- c()
  tempoffset <- c()
  npoints <- c()
  PreviousEndPage <- c()
  scale <- c(1,1,1)
  offset <- c(0,0,0)
  bsc_qc <- data.frame(time = c(), size = c(), stringsAsFactors = FALSE)
  S <- matrix(0,0,4) # leftover rows carried to the next block
  NR <- ceiling((90*10^6) / (sf*calibEpochSize)) + 1000 # initial rows of the features matrix
  i <- 1 # block number
  count <- 1 # next row of features
  LD <- 2 # rows in the current block; the loop stops below 2
  isLastBlock <- FALSE
  header <- NULL
  QC <- "Autocalibration not done"
  while (LD > 1) {
    if (!is.null(progress)) {
      progress("calibrate", as.integer(i), NA_integer_, paste0("Loading chunk: ", i))
    }
    # per-pass trace values
    tr_rows_read <- 0; tr_zeros <- 0; tr_use <- 0; tr_carried <- 0
    tr_iter <- NA_integer_; tr_start <- NA_real_; tr_end <- NA_real_; tr_accepted <- FALSE
    # GGIR silences warnings around the read; they are recorded instead
    accread <- withCallingHandlers(
      .raw.read.block(info, blocksize = blocksize, blocknumber = i,
                      previous_end_page = PreviousEndPage, ws = blockResolution,
                      params = read_params, filequality = filequality, header = header),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if (length(accread$messages) > 0) messages <- c(messages, accread$messages)
    # GGIR passes a NULL header on every block
    header <- NULL
    isLastBlock <- accread$is_last_block
    PreviousEndPage <- accread$endpage

    if (i == 1) {
      filequality_first <- accread$filequality
      use.temp <- temp.available <- ("temperature" %in% colnames(accread$P$data))
      if (use.temp) {
        features <- matrix(99999, NR, 8)
      } else {
        features <- matrix(99999, NR, 7)
      }
    }

    if (length(accread$P) > 0) { # empty when the file is corrupt or finished
      # without row names, as in getmeta: no value uses them, and every c() in
      # .raw.roll.mean would copy millions of them
      data <- as.matrix(accread$P$data[,which(colnames(accread$P$data) %in% c("x", "y", "z", "time", "temperature"))],
                        rownames.force = FALSE)
      tr_rows_read <- nrow(data)
      if (exists("accread")) {
        rm(accread)
      }
      # prepend the leftover of the previous block
      if (min(dim(S)) > 1) {
        data <- rbind(S,data)
      }
      # drop all-zero samples (idle sleep mode); ActiGraph csv always takes this path
      zeros <- c()
      if (!(mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["CSV"]])) {
        zeros <- which(data[, "x"] == 0 & data[, "y"] == 0 & data[, "z"] == 0)
      }
      if ((mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["CSV"]]) || length(zeros) > 0) {
        n_before <- nrow(data)
        data <- .raw.impute.timegaps(x = as.data.frame(data), sf = sf, impute = FALSE)
        data <- as.matrix(data$x, rownames.force = FALSE)
        tr_zeros <- n_before - nrow(data)
      }
      LD <- nrow(data)
      use <- (floor(LD / (blockResolution*sf))) * (blockResolution*sf) # whole hours only
      if (length(use) > 0) {
        if (use > 0) {
          tr_use <- use
          if (use != LD) {
            S <- data[(use + 1):LD,]
            # a one-row leftover drops to a vector
            if (is.vector(S)) {
              S <- t(S)
            }
            tr_carried <- LD - use
          }
          data <- data[1:use,]
          LD <- nrow(data)

          Gx <- data[, "x"]
          Gy <- data[, "y"]
          Gz <- data[, "z"]

          # temperature sanity: first 10 samples above 120 C, or no variance
          if (use.temp) {
            if (mean(data[1:10, "temperature"], na.rm = TRUE) > 120) {
              note("\ntemperature ignored for auto-calibration because values are too high\n")
              use.temp <- FALSE
            } else if (stats::sd(data[, "temperature"], na.rm = TRUE) < 0.01) {
              note("\ntemperature ignored for auto-calibration because no variance in values\n")
              use.temp <- FALSE
            }
          }
          # grow features when this block would overrun it, with a day to spare
          expected_EN2_length <- use / (sf * calibEpochSize)
          expected_endCount <- count + expected_EN2_length - 1
          if (expected_endCount > nrow(features)) {
            rows_needed <- expected_endCount - nrow(features)
            rows_to_add <- rows_needed + (3600/calibEpochSize) * 24
            extension <- matrix(99999, rows_to_add, ncol(features))
            features <- rbind(features, extension)
          }
          # mean acceleration for EN, x, y, and z
          EN <- sqrt(Gx^2 + Gy^2 + Gz^2)
          EN2 <- .raw.roll.mean(EN, sf, calibEpochSize)
          endCount <- count - 1 + length(EN2)
          features[count:endCount, 1] <- EN2
          features[count:endCount, 2] <- .raw.roll.mean(Gx, sf, calibEpochSize)
          features[count:endCount, 3] <- .raw.roll.mean(Gy, sf, calibEpochSize)
          features[count:endCount, 4] <- .raw.roll.mean(Gz, sf, calibEpochSize)
          # sd acceleration
          features[count:endCount, 5] <- .raw.roll.sd(Gx, sf, calibEpochSize)
          features[count:endCount, 6] <- .raw.roll.sd(Gy, sf, calibEpochSize)
          features[count:endCount, 7] <- .raw.roll.sd(Gz, sf, calibEpochSize)
          if (use.temp == TRUE) {
            features[count:endCount, 8] <- .raw.roll.mean(data[, "temperature"], sf, calibEpochSize)
          }
          count <- endCount + 1
          rm(Gx); rm(Gy); rm(Gz)
          # blocksize follows available memory
          BlocksizeNew <- .raw.update.blocksize(blocksize = blocksize, bsc_qc = bsc_qc)
          bsc_qc <- BlocksizeNew$bsc_qc
          blocksize <- BlocksizeNew$blocksize
        }
      }
    } else {
      LD <- 0 # stops the loop
    }
    rows_read_total <- rows_read_total + tr_rows_read
    zeros_total <- zeros_total + tr_zeros
    rows_used_total <- rows_used_total + tr_use
    spherepopulated <- 0
    if (isLastBlock) {
      LD <- 0
    }
    features_temp <- data.frame(V = features, stringsAsFactors = FALSE)
    # drop unfilled rows and NA
    cut <- which(features_temp[,1] == 99999 |
                   is.na(features_temp[,4]) == TRUE | is.na(features_temp[,1]) == TRUE)
    if (length(cut) > 0) {
      features_temp <- features_temp[-cut,]
    }
    # GGIR's duplicate test: a row goes when exactly 3 of its 6 mean/SD values equal the
    # next row's (non-wear repeats)
    if (nrow(features_temp) > 0) {
      cut <- which(rowSums(features_temp[1:(nrow(features_temp) - 1), 2:7] == features_temp[2:nrow(features_temp), 2:7]) == 3)
      if (length(cut) > 0) {
        features_temp <- features_temp[-cut,]
      }
    }
    nhoursused <- 0
    if (nrow(features_temp) > (minloadcrit - 21)) {  # enough data for the sphere?
      nhoursused <- (nrow(features_temp) * 10) / 3600
      features_temp <- features_temp[-1,] # GGIR always drops the first window
      # still windows
      if (mon == .RAW_MONITOR[["AD_HOC"]]) {
        if (length(rmc.noise) == 0) {
          note("Argument rmc.noise not specified, please specify expected noise level in g-units")
        }
        sdcriter <- rmc.noise * 1.2
        if (length(rmc.noise) == 0) {
          stop(paste0("Please provide noise level for the acceleration sensors",
                      " in g-units with argument rmc.noise to aid non-wear detection"),
               call. = FALSE)
        }
      } else {
        sdcriter <- 0.013
      }
      nomovement <- which(features_temp[,5] < sdcriter & features_temp[,6] < sdcriter & features_temp[,7] < sdcriter &
                            abs(as.numeric(features_temp[,2])) < 2 & abs(as.numeric(features_temp[,3])) < 2 &
                            abs(as.numeric(features_temp[,4])) < 2) # the 2 g bound excludes clipping
      if (length(nomovement) < 10) {
        # one row makes the next test skip the fit
        features_temp <- features_temp[1, ]
      } else {
        features_temp <- features_temp[nomovement,]
      }
      if (min(dim(features_temp)) > 1) {
        npoints <- nrow(features_temp)
        cal.error.start <- sqrt(as.numeric(features_temp[,2])^2 + as.numeric(features_temp[,3])^2 + as.numeric(features_temp[,4])^2)
        cal.error.start <- round(mean(abs(cal.error.start - 1)), digits = 5)
        tr_start <- cal.error.start
        # check whether the sphere is well populated
        tel <- 0
        for (axis in 2:4) {
          if ( min(features_temp[,axis]) < -spherecrit & max(features_temp[,axis]) > spherecrit) {
            tel <- tel + 1
          }
        }
        if (tel == 3) {
          spherepopulated <- 1
        } else {
          spherepopulated <- 0
          QC <- "recalibration not done because not enough points on all sides of the sphere"
        }
      } else {
        QC <- "recalibration not done because no non-movement data available"
        features_temp <- c()
      }
    } else {
      QC <- "recalibration not done because not enough data in the file or because file is corrupt"
    }
    if (spherepopulated == 1) {
      # Zhou Fang's iteratively reweighted sphere fit
      input <- features_temp[,2:4]
      if (use.temp == TRUE) {
        inputtemp <- cbind(as.numeric(features_temp[,8]),as.numeric(features_temp[,8]),as.numeric(features_temp[,8]))
      } else {
        inputtemp <- matrix(0, nrow(input), ncol(input)) # no temperature: a zero column keeps the fit shape
      }
      meantemp <- mean(as.numeric(inputtemp[, 1]), na.rm = TRUE)
      inputtemp <- inputtemp - meantemp
      offset <- rep(0, ncol(input))
      scale <- rep(1, ncol(input))
      tempoffset <- rep(0, ncol(input))
      weights <- rep(1, nrow(input))
      res <- Inf
      maxiter <- 1000
      tol <- 1e-10
      for (iter in 1:maxiter) {
        curr <- c()
        try(expr = {curr <- scale(input, center = -offset, scale = 1/scale) +
          scale(inputtemp, center = F, scale = 1/tempoffset)}, silent = TRUE)
        if (length(curr) == 0) {
          # scale() failed
          break
        }
        closestpoint <- curr / sqrt(rowSums(curr^2))
        k <- 1
        offsetch <- rep(0, ncol(input))
        scalech <- rep(1, ncol(input))
        toffch <- rep(0, ncol(inputtemp))
        for (k in 1:ncol(input)) {
          # drop windows whose projection is NaN (ActiGraph and Movisens)
          if ((mon == .RAW_MONITOR[["ACTIGRAPH"]] || mon == .RAW_MONITOR[["MOVISENS"]]) && length(which(is.na(closestpoint[,k, drop = F]) == TRUE)) > 0 &
              length(which(is.na(closestpoint[,k, drop = F]) == FALSE)) > 10) {
            invi <- which(is.na(closestpoint[,k, drop = F]) == TRUE)
            closestpoint <- closestpoint[-invi,]
            curr <- curr[-invi,]
            inputtemp <- inputtemp[-invi,]
            input <- input[-invi,]
            weights <- weights[-invi]
          }
          fobj <- stats::lm.wfit(cbind(1, curr[,k],inputtemp[,k]) , closestpoint[,k, drop = F], w = weights)
          offsetch[k] <- fobj$coef[1]
          scalech[k] <- fobj$coef[2]
          if (use.temp == TRUE) {
            toffch[k] <- fobj$coeff[3]
          }
          curr[,k] <- fobj$fitted.values
        }
        offset <- offset + offsetch / (scale  * scalech)
        if (use.temp == TRUE) {
          tempoffset <- tempoffset * scalech + toffch
        }
        scale <- scale * scalech
        res <- c(res,  3 * mean(weights*(curr - closestpoint)^2 / sum(weights)))
        weights <- pmin(1 / sqrt(rowSums((curr - closestpoint)^2)), 1 / 0.01)
        if (abs(res[iter + 1] - res[iter]) < tol)  break
      }
      tr_iter <- as.integer(iter)
      # scale(center = -offset, scale = 1/scale) is (D + offset) * scale
      if (use.temp == FALSE) {
        features_temp2 <- scale(as.matrix(features_temp[,2:4]),center = -offset, scale = 1/scale)
      } else {
        yy <- as.matrix(cbind(as.numeric(features_temp[,8]),as.numeric(features_temp[,8]),as.numeric(features_temp[,8])))
        features_temp2 <- scale(as.matrix(features_temp[,2:4]),center = -offset, scale = 1/scale) +
          scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)
      }
      cal.error.end <- sqrt(features_temp2[,1]^2 + features_temp2[,2]^2 + features_temp2[,3]^2)
      rm(features_temp2)
      cal.error.end <- round(mean(abs(cal.error.end - 1)), digits = 5)
      tr_end <- cal.error.end
      # acceptance test
      if (cal.error.end < cal.error.start & cal.error.end < 0.01 & nhoursused > minloadcrit) {
        if (use.temp == temp.available) {
          QC <- "recalibration done, no problems detected"
        } else {
          QC <- "recalibration done, but temperature values not used"
        }
        tr_accepted <- TRUE
        LD <- 0 # stop loading
      } else {
        QC <- "recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this"
      }
    }
    trace[[i]] <- data.frame(block = as.integer(i), rows = tr_use, hours = nhoursused,
                             still_windows = if (!is.na(tr_start)) as.integer(npoints) else NA_integer_,
                             iterations = tr_iter, error_start = tr_start, error_end = tr_end,
                             accepted = tr_accepted,
                             rows_read = tr_rows_read, zeros_removed = tr_zeros, rows_carried = tr_carried,
                             qcmessage = QC, stringsAsFactors = FALSE)
    i <- i + 1
  }
  if (length(cal.error.end) > 0) {
    if (cal.error.end > cal.error.start) {
      QC <- "recalibration not done because recalibration does not decrease error"
    }
  }
  if (!is.null(features_temp) && all(dim(features_temp)) != 0) {
    spheredata <- features_temp
    if (use.temp == TRUE) {
      names(spheredata) <- c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz","temperature")
    } else {
      names(spheredata) <- c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz")
    }
  } else {
    spheredata <- c()
  }
  rm(features_temp)
  QCmessage <- QC
  if (use.temp == TRUE && length(spheredata) > 0) {
    meantempcal <- mean(spheredata[,8], na.rm = TRUE)
  } else {
    meantempcal <- c()
  }
  chunks <- if (length(trace) > 0) do.call(rbind, trace) else .raw.calibration.chunks0()
  rownames(chunks) <- NULL
  list(scale = scale, offset = offset, tempoffset = tempoffset,
       cal.error.start = cal.error.start, cal.error.end = cal.error.end,
       spheredata = spheredata, npoints = npoints, nhoursused = nhoursused,
       QCmessage = QCmessage, use.temp = use.temp, meantempcal = meantempcal, bsc_qc = bsc_qc,
       temp.available = temp.available, chunks = chunks, messages = messages,
       filequality = filequality_first,
       rows_unused = rows_read_total - zeros_total - rows_used_total)
}

# EXPORTED FUNCTIONS

#' Auto-Calibrate a Raw Accelerometer File
#'
#' Derives the per-axis scale, offset and (when a usable temperature channel exists)
#' temperature-offset coefficients of the van Hees et al. (2014) auto-calibration from
#' the still periods of a recording, then applies GGIR's rule for whether they are used.
#' The procedure is that of GGIR 3.3-9's g.calibrate and g.part1, so the result matches a
#' stored GGIR milestone with identical().
#'
#' @details
#' As in g.part1: no calibration when \code{do.cal} is FALSE, when the inspection found
#' no sample frequency or when the file was skipped as too small; a supplied calibration
#' is used as given (GGIR's backup.cal.coef retrieve path: no refit, no identity-reset
#' rule, and the temperature term dropped whenever meantempcal is absent); a supplied
#' table with no row for this file falls back to a fit, which under
#' \code{ggir_exact = TRUE} bypasses the identity-reset rule as GGIR does. The message
#' "recalibration attempted with all available data, but possibly not good enough" does
#' not mean uncalibrated, and the hour gate \code{nhoursused > minloadcrit} is strict;
#' both are GGIR's.
#'
#' Fixed as in GGIR: 10 s windows, 0.013 g SD and 2 g mean still criteria, 10 still
#' windows minimum, 3600 s block resolution, 1e-10 tolerance, 1000 iterations, weight cap
#' 100, 0.01 g acceptance, 5-decimal rounding of the errors, 120 C and SD 0.01
#' temperature sanity. Parameters read: chunksize, spherecrit, minloadcrit, do.cal,
#' rmc.noise (ad-hoc csv only), backup.cal.coef (a csv path is treated like
#' \code{calibration}), ggir_exact, progress, and the reader's own.
#'
#' GGIR's warnings are collected in \code{messages} rather than raised.
#'
#' @param info A \code{canhrActi_raw_info} from \code{\link{raw.inspect}}.
#' @param params A \code{\link{raw.params}} object, or NULL for the parameters the file
#'   was inspected with.
#' @param progress NULL or a \code{function(stage, i, n, message)} called before every
#'   block read with stage "calibrate"; overrides \code{params$progress}.
#' @param calibration NULL to fit, or a previously derived calibration to use as given: a
#'   \code{canhrActi_raw_calibration}, a GGIR C list, a data.frame shaped like GGIR's
#'   data_quality_report.csv, or the path to such a csv.
#' @param ... Individual parameter overrides, e.g. \code{minloadcrit = 1}, routed through
#'   \code{raw.params()}.
#'
#' @return An object of class "canhrActi_raw_calibration": a list with
#' \describe{
#'   \item{scale, offset, tempoffset}{The coefficients to apply, after the decision rule.}
#'   \item{cal_error_start, cal_error_end}{Mean absolute deviation of the still-window
#'     norms from 1 g before and after the fit, rounded to 5 decimals; NULL when not
#'     computed.}
#'   \item{spheredata}{The still windows used (Euclidean Norm, meanx, meany, meanz, sdx,
#'     sdy, sdz and, when present, temperature), or NULL.}
#'   \item{npoints, nhoursused}{Number of still windows; hours of data used.}
#'   \item{qcmessage}{GGIR's QCmessage.}
#'   \item{use_temp, meantempcal}{Whether temperature entered the fit; its mean over the
#'     still windows (NULL without temperature).}
#'   \item{bsc_qc}{GGIR's block size log, one row per block.}
#'   \item{applied}{TRUE when the coefficients are a real calibration that survived the
#'     rule or were supplied; FALSE for the identity.}
#'   \item{reset, check_backup, decision_rule}{The rule's outcome, GGIR's
#'     check.backup.cal.coef flag, and whether the rule was applied at all.}
#'   \item{chunks}{One row per pass of the block loop: block, rows, hours, still_windows,
#'     iterations, error_start, error_end, accepted, rows_read, zeros_removed,
#'     rows_carried, qcmessage.}
#'   \item{fit}{The last fit's coefficients before the rule, or NULL.}
#'   \item{source, supplied_from, attempted}{"fit", "supplied" or "not_attempted"; the
#'     shape a supplied calibration came in; whether the block loop ran.}
#'   \item{temp_available, filequality, messages, rows_unused, file, settings, elapsed}{Whether
#'     a temperature column existed; the block-1 file quality record; collected warning
#'     texts; samples read but never used; path and filename; the settings used; seconds
#'     elapsed.}
#' }
#' Use \code{\link{as.ggir.C}} to obtain GGIR's C list.
#'
#' @references van Hees VT et al. (2014). Autocalibration of accelerometer data for
#'   free-living physical activity assessment using local gravity and temperature: an
#'   evaluation on four continents. J Appl Physiol 117(7):738-744.
#'
#' @examples
#' \dontrun{
#' info <- raw.inspect("recording.gt3x", desiredtz = "America/Anchorage")
#' cal <- raw.calibrate(info)
#' cal$scale; cal$offset; cal$qcmessage; cal$applied
#' cal$chunks
#' as.ggir.C(cal)
#' }
#' @export
raw.calibrate <- function(info, params = NULL, progress = NULL, calibration = NULL, ...) {
  t0 <- Sys.time()
  if (!is.list(info) || !all(c("monc", "dformc") %in% names(info))) {
    stop("info must be a canhrActi_raw_info object from raw.inspect()", call. = FALSE)
  }
  params <- .raw.calibrate.params(info, params, list(...))
  if (is.null(progress)) progress <- .raw.param(params, "progress", NULL)
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be NULL or a function(stage, i, n, message)", call. = FALSE)
  }
  ggir_exact <- isTRUE(.raw.param(params, "ggir_exact", TRUE))
  do.cal <- isTRUE(.raw.param(params, "do.cal", TRUE))
  filename <- if (!is.null(info$filename)) info$filename else basename(as.character(info$path))
  sf <- info$sf
  mon <- info$monc # NULL for a file skipped at inspection
  # GGIR calibrates Verisense as ActiGraph
  if (length(mon) == 1 && mon == .RAW_MONITOR[["VERISENSE"]]) mon <- .RAW_MONITOR[["ACTIGRAPH"]]
  dformat <- info$dformc
  chunksize <- .raw.param(params, "chunksize", 1)
  spherecrit <- .raw.param(params, "spherecrit", 0.3)
  minloadcrit <- .raw.param(params, "minloadcrit", 168)
  rmc.noise <- .raw.param(params, "rmc.noise", 13)
  adhoc <- length(mon) == 1 && mon == .RAW_MONITOR[["AD_HOC"]]

  finish <- function(obj, blocksize = NULL) {
    obj$file <- list(path = if (is.null(info$path)) NA_character_ else info$path, filename = filename)
    obj$settings <- list(sf = sf, monc = info$monc, dformc = dformat, blocksize = blocksize,
                         chunksize = chunksize, spherecrit = spherecrit, minloadcrit = minloadcrit,
                         sdcriter = if (adhoc) rmc.noise * 1.2 else 0.013,
                         ggir_exact = ggir_exact, do.cal = do.cal)
    obj$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    obj
  }

  # a supplied calibration is used as given; a table with no row for this file means a fit
  if (is.null(calibration)) {
    bcc <- .raw.param(params, "backup.cal.coef", NULL)
    if (length(bcc) > 0 && is.character(bcc) && !(bcc[1] %in% c("retrieve", "redo"))) {
      calibration <- bcc[1]
    }
  }
  table_given <- FALSE
  if (!is.null(calibration)) {
    supplied <- .raw.calibration.supplied(calibration, filename)
    if (!is.null(supplied)) {
      supplied$messages <- c(supplied$messages, "Calibration coefficients supplied; no fit was made.")
      return(finish(supplied))
    }
    table_given <- TRUE
  }

  # GGIR calibrates only with do.cal, no backup in use and a known sample rate; the
  # default C carries errors 0/0, so the rule leaves it alone
  not_attempted <- function(why) {
    obj <- .raw.calibration.object(cal_error_start = 0, cal_error_end = 0, npoints = 0,
                                   nhoursused = 0, qcmessage = "Autocalibration not done",
                                   use_temp = TRUE, source = "not_attempted", attempted = FALSE,
                                   messages = why, filequality = NULL)
    obj <- .raw.calibration.decision(obj)
    finish(obj)
  }
  if (!do.cal) {
    return(not_attempted("do.cal is FALSE: autocalibration not done"))
  }
  if (isTRUE(info$skipped) || is.null(mon) || is.null(dformat)) {
    return(not_attempted("The file was skipped at inspection (too small): autocalibration not done"))
  }
  if (is.null(sf)) {
    return(not_attempted(paste0("No sample frequency could be read from ", filename,
                                " (corrupt file): autocalibration not done")))
  }
  blocksize <- .raw.blocksize(info, "calibrate", chunksize = chunksize)
  # called on its own, the stage removes the .gt3x extraction it makes
  own <- .raw.gt3x.stage.owner(if (!is.null(info$read_path)) info$read_path else info$path, params)
  if (!is.null(own)) on.exit(try(.raw.gt3x.extract.cleanup(own$path, exdir = own$dir), silent = TRUE), add = TRUE)
  fit <- .raw.calibrate.fit(info, sf = sf, mon = mon, dformat = dformat, blocksize = blocksize,
                            spherecrit = spherecrit, minloadcrit = minloadcrit, rmc.noise = rmc.noise,
                            read_params = params, progress = progress)
  obj <- .raw.calibration.object(
    scale = fit$scale, offset = fit$offset, tempoffset = fit$tempoffset,
    cal_error_start = fit$cal.error.start, cal_error_end = fit$cal.error.end,
    spheredata = fit$spheredata, npoints = fit$npoints, nhoursused = fit$nhoursused,
    qcmessage = fit$QCmessage, use_temp = fit$use.temp, meantempcal = fit$meantempcal,
    bsc_qc = fit$bsc_qc, chunks = fit$chunks, source = "fit", attempted = TRUE,
    temp_available = fit$temp.available, filequality = fit$filequality,
    messages = fit$messages, rows_unused = fit$rows_unused)
  if (table_given && ggir_exact) {
    # in GGIR the fit made after an unmatched backup table bypasses the rule
    obj$decision_rule <- FALSE
    obj$reset <- FALSE
    obj$check_backup <- TRUE
    obj$applied <- length(obj$cal_error_end) > 0
    obj$messages <- c(obj$messages,
                      "No row for this file in the supplied calibration table; fitted without GGIR's identity-reset rule (ggir_exact = TRUE)")
  } else {
    obj <- .raw.calibration.decision(obj)
    if (table_given) {
      obj$messages <- c(obj$messages, "No row for this file in the supplied calibration table; fitted instead")
    }
  }
  finish(obj, blocksize = blocksize)
}

#' GGIR's C List From a canhrActi Calibration
#'
#' Returns the calibration in the shape GGIR saves in its part-1 milestone (the C object
#' of meta_*.RData), so identical() comparisons and milestone export work. The
#' coefficients are those after g.part1's identity-reset rule, which is what the
#' milestone stores; \code{decided = FALSE} gives the last fit's instead. A calibration
#' that was not attempted is GGIR's default C; one supplied as a table or csv is that
#' default overwritten member by member in GGIR's order.
#'
#' @param x A canhrActi_raw_calibration.
#' @param decided TRUE (default) for the coefficients after the decision rule, FALSE for the
#'   last fit's.
#' @return A named list in GGIR's member order.
#' @examples
#' \dontrun{
#' C <- as.ggir.C(raw.calibrate(info))
#' identical(C[names(C) != "bsc_qc"], stored_C[names(stored_C) != "bsc_qc"])
#' }
#' @export
as.ggir.C <- function(x, decided = TRUE) {
  if (!inherits(x, "canhrActi_raw_calibration")) {
    stop("x must be a canhrActi_raw_calibration object", call. = FALSE)
  }
  Cdefault <- list(cal.error.end = 0, cal.error.start = 0)
  Cdefault$scale <- c(1,1,1)
  Cdefault$offset <- c(0,0,0)
  Cdefault$tempoffset <-  c(0,0,0)
  Cdefault$QCmessage <- "Autocalibration not done"
  Cdefault$npoints <- 0
  Cdefault$nhoursused <- 0
  Cdefault$use.temp <- TRUE
  if (x$source == "not_attempted") {
    return(Cdefault)
  }
  if (x$source == "supplied" && x$supplied_from %in% c("data.frame", "csv", "table")) {
    C <- Cdefault
    C$scale <- x$scale
    C$offset <- x$offset
    C$tempoffset <- x$tempoffset
    C["meantempcal"] <- list(x$meantempcal)
    C["cal.error.start"] <- list(x$cal_error_start)
    C["cal.error.end"] <- list(x$cal_error_end)
    C["QCmessage"] <- list(x$qcmessage)
    C["npoints"] <- list(x$npoints)
    C["nhoursused"] <- list(x$nhoursused)
    C["use.temp"] <- list(x$use_temp)
    return(C)
  }
  coef <- if (decided || is.null(x$fit)) x[c("scale", "offset", "tempoffset")] else x$fit
  list(scale = coef$scale, offset = coef$offset, tempoffset = coef$tempoffset,
       cal.error.start = x$cal_error_start, cal.error.end = x$cal_error_end,
       spheredata = x$spheredata, npoints = x$npoints, nhoursused = x$nhoursused,
       QCmessage = x$qcmessage, use.temp = x$use_temp, meantempcal = x$meantempcal,
       bsc_qc = x$bsc_qc)
}

#' Print method for a raw calibration
#'
#' @param x A canhrActi_raw_calibration.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_calibration <- function(x, ...) {
  fmt <- function(v, digits = 6) {
    if (is.null(v) || length(v) == 0) return("NULL")
    paste(format(v, digits = digits), collapse = " ")
  }
  cat("\ncanhrActi raw calibration (GGIR g.calibrate)\n")
  if (!is.null(x$file$filename) && !is.na(x$file$filename)) cat("  file:      ", x$file$filename, "\n", sep = "")
  cat("  source:    ", x$source,
      if (x$source == "supplied") paste0(" (", x$supplied_from, ")") else "", "\n", sep = "")
  cat("  status:    ", x$qcmessage, "\n", sep = "")
  cat("  applied:   ", x$applied, if (isTRUE(x$reset)) " (reset to identity by GGIR's rule)" else "", "\n", sep = "")
  cat("  scale:     ", fmt(x$scale, 10), "\n", sep = "")
  cat("  offset:    ", fmt(x$offset, 10), "\n", sep = "")
  cat("  tempoffset:", fmt(x$tempoffset, 10), "\n", sep = "")
  cat("  error (g): ", fmt(x$cal_error_start), " -> ", fmt(x$cal_error_end), "\n", sep = "")
  cat("  hours used:", fmt(x$nhoursused), "   still windows: ", fmt(x$npoints), "\n", sep = "")
  cat("  temperature used: ", x$use_temp,
      if (length(x$meantempcal) > 0) paste0(" (mean over still windows ", fmt(x$meantempcal), " C)") else "",
      "\n", sep = "")
  if (is.data.frame(x$chunks) && nrow(x$chunks) > 0) {
    cat("  blocks:\n")
    print(x$chunks[, c("block", "rows", "hours", "still_windows", "iterations", "error_start", "error_end", "accepted")],
          row.names = FALSE)
  }
  if (length(x$messages) > 0) {
    cat("  messages:\n")
    for (m in x$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.na(x$elapsed)) cat("  elapsed:   ", round(x$elapsed, 1), " s\n", sep = "")
  invisible(x)
}
