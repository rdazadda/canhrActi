# Ported from GGIR 3.3-9 R/g.part1.R, R/g.part2.R, R/g.part3.R, R/g.part4.R, R/g.part5.R,
# R/g.part5.savetimeseries.R, R/g.report.part5.R, R/g.report.part5_dictionary.R,
# R/checkMilestoneFolders.R and R/correctOlderMilestoneData.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the save() calls of parts 1
# to 5 became write.ggir.milestone, which writes the objects of as.ggir.milestone under
# GGIR's names and in its save order into the folders checkMilestoneFolders creates, plus
# the ms5.outraw tree, the report csv files and the two dictionaries that GGIR writes
# from inside part 5; the load() of g.part2 became read.ggir.milestone, which rebuilds a
# canhrActi_raw object from I, C and M; correctOlderMilestoneData is
# .raw.correct.older.milestone. No arithmetic lives in this file.

# WRITE

#' Write GGIR Milestone Files From a canhrActi_raw Object
#'
#' Saves the objects of \code{\link{as.ggir.milestone}} under the names GGIR's own
#' \code{save()} calls use, so that GGIR can be pointed at canhrActi's output and run the
#' parts that follow. With the default \code{parts = 1} this writes the part-1 file
#' meta_<file>.RData alone; with any other part it writes meta/basic, meta/ms2.out,
#' meta/ms3.out, meta/ms4.out and, for part 5, meta/ms5.out, meta/ms5.outraw and
#' \code{results} under \code{path}, and returns one path per file.
#'
#' @details Reproduces five GGIR save() calls, each with its own object list and order.
#'   Part 1: M, I, C, filename_dir, filefoldername, tail_expansion_log, GGIRversion,
#'   desiredtz_part1, in a file named with the recording's file name plus ".RData" unless
#'   it already contains ".RD", prefixed with "meta_"; GGIRversion is NA because no GGIR
#'   code produced the objects. Part 2: SUM, IMP, tail_expansion_log, GGIRversion, with
#'   the SUM of \code{\link{as.ggir.milestone}}. Part 3: the fourteen objects of
#'   \code{\link{as.ggir.ms3}}. Part 4: nightsummary, tail_expansion_log, GGIRversion.
#'   Part 5: output, tail_expansion_log, GGIRversion, last_timestamp, written only when
#'   the day table still has a boutdur.mvpa column and at least one row, as in GGIR.
#'   Parts 2 to 5 take the recording's file name with ".RData" appended and no "meta_"
#'   prefix, in the folders checkMilestoneFolders creates. Part 5 also writes, in GGIR's
#'   order, the sib reports, the behavioural-class legend, the exported time series (one
#'   folder per threshold triple) and, when \code{reports} is given, the report csv files
#'   and the two variable dictionaries. GGIR's two basename strip rules differ and both
#'   are reproduced: the time-series name drops gt3x and the sib-report name does not.
#'   \code{results} and \code{results/QC} are created whenever part 5 is written because
#'   GGIR's report layer cannot write into a missing folder.
#'
#' @param x A canhrActi_raw object, or (for \code{parts = 1} only) the list returned by
#'   \code{as.ggir.milestone}, or (for \code{parts = 5} only) the
#'   \code{canhrActi_raw_timeuse} itself.
#' @param path With \code{parts = 1}, either a directory (the file meta_<file>.RData is
#'   written into it; for a GGIR run this is <metadatadir>/meta/basic) or the full path of
#'   the RData file to write. With any other \code{parts}, the metadatadir root: the five
#'   meta/... folders are created under it. Parent directories are created either way.
#' @param parts Which milestone parts to write: any of 1, 2, 3, 4 and 5.
#' @param nights For part 4, the \code{canhrActi_raw_nights} of
#'   \code{\link{raw.sleep.nights}}; a recording can have several night tables, so part 4
#'   is passed separately.
#' @param timeuse For part 5, the \code{canhrActi_raw_timeuse} of \code{\link{raw.timeuse}};
#'   one call with two light thresholds produces two complete part-5 tables, so part 5 is
#'   passed separately too.
#' @param reports For part 5, NULL or the nested list of report frames the report layer
#'   produced, one element per configuration, each holding \code{daysummary_full},
#'   \code{daysummary_cleaned} and \code{personsummary}, any of which may be NULL. A
#'   configuration is identified either by five members \code{window}, \code{TRLi},
#'   \code{TRMi}, \code{TRVi} and \code{sleepparam}, or by its name in the list, which is
#'   then used as the file-name suffix. With NULL nothing is written under \code{results}
#'   except the two folders, and no dictionary is written.
#' @param dictionary_per_report For part 5, TRUE writes one variable dictionary per report
#'   written, named with that report's own configuration, instead of GGIR's single unnamed
#'   pair built from whichever report sorted first. FALSE, the default, keeps GGIR's file
#'   names, which is what a byte comparison against a GGIR run needs.
#' @return With \code{parts = 1}, the path of the written file, invisibly. Otherwise a named
#'   character vector of the files written, one per part, invisibly, except that part 5
#'   contributes one entry per file under the names part5, part5_series, part5_sibreport,
#'   part5_legend, part5_report and part5_dictionary.
#' @examples
#' \dontrun{
#' write.ggir.milestone(x, "output_study/meta/basic")
#' GGIR::GGIR(mode = 2:5, datadir = "raw", outputdir = ".", studyname = "study")
#'
#' # hand canhrActi's part 3 to GGIR's part 4
#' write.ggir.milestone(x, "roundtrip", parts = 1:3)
#' GGIR:::g.part4(metadatadir = "roundtrip", f0 = 1, f1 = 1)
#'
#' # hand canhrActi's part 5 to GGIR's own report layer
#' tu <- raw.timeuse(x, nights)
#' write.ggir.milestone(x, "roundtrip", parts = 1:5, nights = nights, timeuse = tu)
#' GGIR:::g.report.part5(metadatadir = "roundtrip", f0 = 1, f1 = 1)
#' }
#' @seealso \code{\link{as.ggir.milestone}}, \code{\link{as.ggir.ms3}},
#'   \code{\link{as.ggir.ms4}}, \code{\link{as.ggir.ms5}},
#'   \code{\link{raw.timeuse.dictionary}}, \code{\link{read.ggir.milestone}}
#' @export
write.ggir.milestone <- function(x, path, parts = 1, nights = NULL, timeuse = NULL,
                                 reports = NULL, dictionary_per_report = FALSE) {
  if (!is.numeric(parts) || length(parts) == 0 || anyNA(parts) ||
      !all(parts %in% 1:5)) {
    stop("parts must be one or more of 1, 2, 3, 4 and 5", call. = FALSE)
  }
  parts <- as.integer(parts)
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("path must be a directory or a file path", call. = FALSE)
  }
  if (!is.logical(dictionary_per_report) || length(dictionary_per_report) != 1 ||
      is.na(dictionary_per_report)) {
    stop("dictionary_per_report must be TRUE or FALSE", call. = FALSE)
  }
  # writing part 5 alone needs no recording, so the time-use object may be passed as x
  if (identical(sort(unique(parts)), 5L) && inherits(x, "canhrActi_raw_timeuse")) {
    if (is.null(timeuse)) timeuse <- x
    x <- NULL
  }
  if (5L %in% parts && !inherits(timeuse, "canhrActi_raw_timeuse")) {
    stop("part 5 needs timeuse = raw.timeuse(x, nights): canhrActi does not attach the ",
         "time-use analysis to the recording, because one call with two light thresholds ",
         "produces two complete part-5 tables for one recording", call. = FALSE)
  }
  if (!(5L %in% parts) && (!is.null(reports) || isTRUE(dictionary_per_report))) {
    stop("reports and dictionary_per_report belong to part 5; add 5 to parts", call. = FALSE)
  }
  if (!identical(sort(unique(parts)), 1L)) {
    if (!inherits(x, "canhrActi_raw") && !identical(sort(unique(parts)), 5L)) {
      stop("x must be a canhrActi_raw object to write more than the part-1 milestone",
           call. = FALSE)
    }
    return(invisible(.raw.milestone.write.parts(
      x, path, parts, nights = nights, timeuse = timeuse, reports = reports,
      dictionary_per_report = dictionary_per_report)))
  }
  if (inherits(x, "canhrActi_raw")) {
    ms <- as.ggir.milestone(x)
  } else if (is.list(x) && all(c("M", "I", "C") %in% names(x))) {
    ms <- x
  } else {
    stop("x must be a canhrActi_raw object or the list returned by as.ggir.milestone()", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("path must be a directory or a file path", call. = FALSE)
  }
  path <- gsub("\\\\", "/", path)
  fname <- ms$I$filename
  if (is.null(fname) && inherits(x, "canhrActi_raw")) fname <- x$file$filename
  if (is.null(fname) || is.na(fname)) stop("The milestone carries no file name (I$filename)", call. = FALSE)
  filename <- unlist(strsplit(fname, "/"))
  if (length(filename) > 0) {
    filename <- filename[length(filename)]
  } else {
    filename <- fname
  }
  filefoldername <- filename_dir <- fname
  if (length(unlist(strsplit(fname, "[.]RD"))) == 1) { # to avoid getting .RData.RData
    filename <- paste0(filename, ".RData")
  }
  if (dir.exists(path)) {
    file <- file.path(path, paste0("meta_", filename))
  } else {
    file <- path
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  }
  env <- new.env(parent = emptyenv())
  assign("M", ms$M, envir = env)
  assign("I", ms$I, envir = env)
  assign("C", ms$C, envir = env)
  assign("filename_dir", filename_dir, envir = env)
  assign("filefoldername", filefoldername, envir = env)
  assign("tail_expansion_log", if ("tail_expansion_log" %in% names(ms)) ms$tail_expansion_log else NULL, envir = env)
  assign("GGIRversion", if ("GGIRversion" %in% names(ms)) ms$GGIRversion else NA, envir = env)
  assign("desiredtz_part1", if ("desiredtz_part1" %in% names(ms)) ms$desiredtz_part1 else "", envir = env)
  save(list = c("M", "I", "C", "filename_dir", "filefoldername", "tail_expansion_log",
                "GGIRversion", "desiredtz_part1"),
       envir = env, file = file)
  invisible(file)
}

# READ

#' Convert the Columns of an Older Milestone Table
#'
#' GGIR's correctOlderMilestoneData: a factor timestamp becomes character and any factor
#' metric, temperature or light column becomes numeric. A current milestone passes through
#' unchanged.
#' @param x metashort or metalong.
#' @return The table.
#' @keywords internal
#' @noRd
.raw.correct.older.milestone <- function(x) {
  # timestamp is assumed to be always present and in character format
  if (is.factor(x$timestamp[1])) {
    x$timestamp <- format(x$timestamp)
  }
  # temperature and light may not always be present
  # if present they are expected to be in numeric format
  metrics <- c("ENMO","LFENMO", "BFEN", "EN", "HFEN", "HFENplus", "MAD", "ENMOa",
               "ZCX", "ZCY", "ZCZ", "BrondCount_x", "BrondCount_y",
               "BrondCount_z", "NeishabouriCount_x", "NeishabouriCount_y",
               "NeishabouriCount_z", "NeishabouriCount_vm",
               "anglex", "angley", "anglez", "temperature", "light")
  pattern <- paste(metrics, collapse = "|")
  columns2check <- grep(pattern = pattern, x = names(x), value = FALSE)
  if (length(columns2check) > 0) {
    for (column in columns2check) {
      if (is.factor(x[1, column])) {
        x[, column] <- as.numeric(as.character(x[, column]))
      }
    }
  }
  return(x)
}

#' Rebuild a canhrActi_raw_info From GGIR's I Object
#'
#' A milestone carries no file path, size or parsed header list, so those members are NA
#' or NULL; everything raw.inspect derives from the header (header variables, ID, serial,
#' firmware, timezone, dynamic range, clipping threshold, block sizes) is derived here from
#' I with the same helpers, and \code{.raw.ggir.I} on the result gives I back unchanged.
#' The header list is rebuilt from the formatted header strings for gt3x, with dates
#' parsed as GMT-labelled device wall clock (how read.gt3x 1.2.0 formatted them; a later
#' read.gt3x shifts the strings by the desiredtz offset).
#'
#' @param I GGIR's inspection list.
#' @param params A canhrActi_raw_params.
#' @param milestone Path of the RData the object came from.
#' @return A canhrActi_raw_info.
#' @keywords internal
#' @noRd
.raw.info.from.ggir <- function(I, params, milestone = NA_character_) {
  if (!is.list(I) || !all(c("header", "monc", "monn", "dformc", "dformn", "sf", "decn", "filename") %in% names(I))) {
    stop("I must be GGIR's inspection list (header, monc, monn, dformc, dformn, sf, decn, filename)", call. = FALSE)
  }
  messages <- character()
  collect <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }
  corrupt <- is.null(I$sf)
  info <- c(list(path = NULL),
            list(filename = I$filename, size_bytes = NA_real_, read_path = NA_character_,
                 monc = I$monc, monn = I$monn, dformc = I$dformc, dformn = I$dformn),
            list(sf = I$sf),
            list(decn = I$decn, header = I$header))
  # header as a list (gt3x: the formatted strings, dates as GMT-labelled device wall clock)
  header_list <- NULL
  if (identical(I$dformn, "gt3x") && is.data.frame(I$header)) {
    header_list <- as.list(as.character(I$header[, 1]))
    names(header_list) <- rownames(I$header)
    for (nm in c("Start Date", "Stop Date", "Last Sample Time", "Download Date")) {
      if (!is.null(header_list[[nm]])) {
        t <- suppressWarnings(as.POSIXct(header_list[[nm]], format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
        if (length(t) == 1 && !is.na(t)) header_list[[nm]] <- t
      }
    }
    for (nm in c("Sample Rate", "Acceleration Scale", "Acceleration Min", "Acceleration Max",
                 "Battery Voltage", "Board Revision", "Unexpected Resets")) {
      if (!is.null(header_list[[nm]])) {
        v <- suppressWarnings(as.numeric(header_list[[nm]]))
        if (length(v) == 1 && !is.na(v)) header_list[[nm]] <- v
      }
    }
  }
  if (corrupt || is.null(I$monn)) {
    hvars <- list(ID = I$filename, iID = "not extracted", HN = "not extracted",
                  sensor.location = "not extracted", SX = "not available",
                  deviceSerialNumber = "not extracted")
  } else {
    hvars <- tryCatch(.raw.header.vars(info), error = function(e) {
      messages <<- c(messages, paste0("Header variables could not be extracted from I: ", conditionMessage(e)))
      list(ID = I$filename, iID = "not extracted", HN = "not extracted",
           sensor.location = "not extracted", SX = "not available", deviceSerialNumber = "not extracted")
    })
  }
  id <- collect(.raw.extract.id(hvars, idloc = .raw.param(params, "idloc", 1), fname = I$filename))
  device_serial <- hvars$deviceSerialNumber
  firmware <- NA_character_
  serial_prefix <- NA_character_
  header_timezone <- NA_character_
  monc <- I$monc
  if (!is.null(monc) && monc %in% c(.RAW_MONITOR[["ACTIGRAPH"]], .RAW_MONITOR[["VERISENSE"]])) {
    if (grepl("_firmware_", device_serial, fixed = TRUE)) firmware <- sub("^.*_firmware_", "", device_serial)
    serial_only <- sub("_firmware_.*$", "", device_serial)
    if (!identical(serial_only, "not extracted")) serial_prefix <- substr(trimws(serial_only), 1, 3)
    if (!is.null(header_list) && !is.null(header_list[["TimeZone"]])) {
      header_timezone <- as.character(header_list[["TimeZone"]])
    }
  } else if (!is.null(monc) && monc == .RAW_MONITOR[["GENEACTIV"]] && is.data.frame(I$header)) {
    if ("firmware" %in% rownames(I$header)) firmware <- as.character(I$header["firmware", 1])
    if ("tzone" %in% rownames(I$header)) header_timezone <- as.character(I$header["tzone", 1])
  } else if (!is.null(monc) && monc == .RAW_MONITOR[["AXIVITY"]] && is.data.frame(I$header)) {
    if ("firmwareVersion" %in% rownames(I$header)) firmware <- as.character(I$header["firmwareVersion", 1])
  }
  info$header_list <- header_list
  info$header_vars <- hvars
  info$id <- id
  info$device_serial <- device_serial
  info$serial_prefix <- serial_prefix
  info$firmware <- firmware
  info$header_timezone <- header_timezone
  info$dynrange_file <- NULL
  if (!is.null(monc) && !is.null(I$dformc)) {
    ncb <- .raw.clip.block.params(info, params = params, pass = "getmeta")
    ncb_cal <- .raw.clip.block.params(info, params = params, pass = "calibrate")
    info$dynrange <- ncb$dynrange
    info$dynrange_source <- ncb$dynrange_source
    info$clipthres <- ncb$clipthres
    info$sdcriter <- ncb$sdcriter
    info$racriter <- ncb$racriter
    info$blocksize_calibrate <- ncb_cal$blocksize
    info$blocksize_getmeta <- ncb$blocksize
  } else {
    info$dynrange <- NULL; info$dynrange_source <- NA_character_; info$clipthres <- NULL
    info$sdcriter <- NULL; info$racriter <- NULL
    info$blocksize_calibrate <- NULL; info$blocksize_getmeta <- NULL
  }
  info$corrupt <- corrupt
  info$too_small <- NA
  info$uppercase_extension <- NA
  info$ggir_accepts_extension <- NA
  info$renamed <- FALSE
  info$stand_in_method <- NA_character_
  info$skipped <- FALSE
  info$messages <- c(paste0("Inspection rebuilt from GGIR's I object in ", milestone,
                            "; file path, size and the parsed header list are not available."), messages)
  info$tz <- list(desiredtz = .raw.param(params, "desiredtz", ""), configtz = .raw.param(params, "configtz", NULL))
  info$params <- params
  info$milestone <- milestone
  structure(info, class = "canhrActi_raw_info")
}

#' Rebuild a canhrActi_raw_calibration From GGIR's C Object
#'
#' The milestone stores the coefficients after g.part1's identity-reset rule together with
#' the fit's errors and still windows. The object is built with source "fit" (the fit was
#' GGIR's), the rule is re-applied (it is idempotent on post-rule coefficients, so
#' \code{reset} and \code{applied} come out as g.part1 decided them) and
#' \code{supplied_from} records the provenance. A C whose message is "Autocalibration not
#' done" (a corrupt file) becomes a not-attempted calibration. The pre-rule coefficients
#' are not recoverable from a milestone, so \code{fit} holds the stored values.
#'
#' @param C GGIR's calibration list.
#' @param filename The recording's file name.
#' @param milestone Path of the RData the object came from.
#' @return A canhrActi_raw_calibration.
#' @keywords internal
#' @noRd
.raw.calibration.from.ggir <- function(C, filename = NA_character_, milestone = NA_character_) {
  if (!is.list(C) || !all(c("scale", "offset", "QCmessage") %in% names(C))) {
    stop("C must be GGIR's calibration list (scale, offset, tempoffset, QCmessage, ...)", call. = FALSE)
  }
  get <- function(nm, default = NULL) if (nm %in% names(C)) C[[nm]] else default
  if (identical(C$QCmessage, "Autocalibration not done")) {
    obj <- .raw.calibration.object(cal_error_start = get("cal.error.start", 0), cal_error_end = get("cal.error.end", 0),
                                   npoints = get("npoints", 0), nhoursused = get("nhoursused", 0),
                                   qcmessage = "Autocalibration not done", use_temp = get("use.temp", TRUE),
                                   source = "not_attempted", attempted = FALSE,
                                   messages = "No calibration in the GGIR milestone (Autocalibration not done)")
  } else {
    obj <- .raw.calibration.object(
      scale = C$scale, offset = C$offset, tempoffset = get("tempoffset"),
      cal_error_start = get("cal.error.start"), cal_error_end = get("cal.error.end"),
      spheredata = get("spheredata"), npoints = get("npoints"), nhoursused = get("nhoursused", 0),
      qcmessage = C$QCmessage, use_temp = get("use.temp", TRUE),
      meantempcal = get("meantempcal"), bsc_qc = get("bsc_qc"),
      source = "fit", attempted = TRUE,
      temp_available = if (!is.null(get("spheredata")) && is.data.frame(C$spheredata)) "temperature" %in% names(C$spheredata) else NA,
      messages = "Calibration coefficients taken from a GGIR milestone; no fit was made here")
  }
  obj <- .raw.calibration.decision(obj)
  obj$supplied_from <- "ggir_milestone"
  obj$file <- list(path = NA_character_, filename = filename)
  obj$settings <- list(milestone = milestone)
  obj$elapsed <- 0
  obj
}

#' Rebuild a canhrActi_raw_meta From GGIR's M Object
#'
#' Adds the numeric time column canhrActi keeps next to GGIR's character timestamp (parsed
#' from the ISO strings in the run's desiredtz), the start time, and the bookkeeping members
#' of \code{\link{raw.getmeta}} that a milestone cannot supply (block trace NULL,
#' rows_discarded_end NA). \code{\link{as.ggir.M}} on the result gives M back unchanged.
#'
#' @param M GGIR's epoch-table list.
#' @param desiredtz The run's desiredtz (desiredtz_part1).
#' @param filename The recording's file name.
#' @param I GGIR's inspection list (sf, monc, dformc for the settings).
#' @param tail_expansion_log The milestone's tail_expansion_log.
#' @param params A canhrActi_raw_params.
#' @param milestone Path of the RData the object came from.
#' @return A canhrActi_raw_meta.
#' @keywords internal
#' @noRd
.raw.meta.from.ggir <- function(M, desiredtz = "", filename = NA_character_, I = NULL,
                                tail_expansion_log = NULL, params = NULL, milestone = NA_character_) {
  if (!is.list(M) || !all(c("filecorrupt", "filetooshort") %in% names(M))) {
    stop("M must be GGIR's epoch-table list (filecorrupt, filetooshort, metalong, metashort, ...)", call. = FALSE)
  }
  tz <- if (length(desiredtz) == 0) "" else desiredtz
  has <- !isTRUE(M$filecorrupt) && !isTRUE(M$filetooshort) &&
    !is.null(M$metashort) && !is.null(M$metalong) && NROW(M$metashort) > 0 && NROW(M$metalong) > 0
  metashort <- NULL; metalong <- NULL; starttime <- NULL
  if (has) {
    ms <- .raw.correct.older.milestone(M$metashort)
    ml <- .raw.correct.older.milestone(M$metalong)
    metashort <- .raw.meta.add.time(ms, as.numeric(.raw.iso8601.to.posix(as.character(ms$timestamp), tz = tz)))
    metalong <- .raw.meta.add.time(ml, as.numeric(.raw.iso8601.to.posix(as.character(ml$timestamp), tz = tz)))
    starttime <- as.POSIXct(metashort$time[1], origin = "1970-01-01", tz = tz)
  }
  obj <- list(metashort = metashort, metalong = metalong, qclog = M$QClog,
              wday = M$wday, wdayname = M$wdayname, windowsizes = M$windowsizes,
              starttime = starttime,
              filecorrupt = isTRUE(M$filecorrupt), filetooshort = isTRUE(M$filetooshort),
              nfilepagesskipped = if (is.null(M$NFilePagesSkipped)) 0 else M$NFilePagesSkipped,
              chunks = NULL, rows_discarded_end = NA_real_, bsc_qc = M$bsc_qc,
              light_available = has && "lightmean" %in% names(M$metalong),
              use_temp = has && "temperaturemean" %in% names(M$metalong),
              metric_names = if (has) setdiff(names(M$metashort), "timestamp") else NULL,
              messages = paste0("Epoch tables taken from GGIR's M object in ", milestone,
                                "; the block trace is not available."),
              skipped = FALSE, tail_expansion_log = tail_expansion_log, nonwear = M$nonwear)
  obj$calibration <- list(source = "ggir_milestone", applied = NA)
  obj$file <- list(path = NA_character_, filename = filename)
  obj$settings <- list(sf = I$sf, monc = I$monc, dformc = I$dformc, blocksize = NULL,
                       windowsizes = M$windowsizes,
                       nonwear_approach = .raw.param(params, "nonwear_approach", "2023"),
                       imputeTimegaps = .raw.param(params, "imputeTimegaps", TRUE),
                       desiredtz = tz, configtz = .raw.param(params, "configtz", NULL),
                       ggir_exact = .raw.param(params, "ggir_exact", TRUE), daylimit = FALSE,
                       milestone = milestone)
  obj$elapsed <- 0
  structure(obj, class = "canhrActi_raw_meta")
}

#' Load a GGIR Part-1 Milestone Into a canhrActi_raw Object
#'
#' Reads a meta_<file>.RData written by GGIR's g.part1 (or by
#' \code{\link{write.ggir.milestone}}) and returns the same kind of object that
#' \code{\link{read.raw.accelerometer}} returns: the inspection, calibration and epoch
#' tables come from the stored I, C and M; the part-2 wear decision and the quality report
#' are computed from them. Use it to compare a GGIR run with a canhrActi run, or to show a
#' GGIR milestone in the dashboard without re-reading the raw file.
#'
#' @details Reproduces the load of g.part2: desiredtz_part1, when present, replaces the
#'   parameter's desiredtz; the older-milestone column conversion; the corrupt rule (a
#'   NULL sf marks the file corrupt) applied through the inspection object.
#'   \code{as.ggir.milestone} on the result gives M, I and C back unchanged. Members that
#'   need the raw file or the block traces (file size and md5, the start and end trims,
#'   zero triplets, the per-block trace) are NA or NULL.
#'
#' @param path Path of the meta_*.RData file.
#' @param params A \code{\link{raw.params}} object for the wear decision and the quality
#'   report, or NULL for GGIR's defaults with desiredtz set to the milestone's
#'   desiredtz_part1.
#' @param ... Individual parameter overrides, routed through \code{raw.params()}.
#' @return An object of class "canhrActi_raw" (see \code{\link{read.raw.accelerometer}}),
#'   with \code{file$milestone} holding the RData path.
#' @examples
#' \dontrun{
#' x <- read.ggir.milestone("output_study/meta/basic/meta_MOS2E39230594.gt3x.RData")
#' colSums(x$wear$rout)
#' }
#' @export
read.ggir.milestone <- function(path, params = NULL, ...) {
  t_total <- Sys.time()
  if (!is.character(path) || length(path) != 1 || is.na(path) || !file.exists(path)) {
    stop(paste0("Milestone file not found: ", if (is.character(path) && length(path) == 1) path else "<path>"),
         call. = FALSE)
  }
  path <- gsub("\\\\", "/", path)
  env <- new.env(parent = emptyenv())
  nms <- load(path, envir = env)
  if (!all(c("M", "I", "C") %in% nms)) {
    stop(paste0(path, " does not hold the objects M, I and C of a GGIR part-1 milestone (found: ",
                paste(nms, collapse = ", "), ")"), call. = FALSE)
  }
  M <- get("M", envir = env); I <- get("I", envir = env); C <- get("C", envir = env)
  desiredtz_part1 <- if ("desiredtz_part1" %in% nms) get("desiredtz_part1", envir = env) else NULL
  tail_expansion_log <- if ("tail_expansion_log" %in% nms) get("tail_expansion_log", envir = env) else NULL
  overrides <- list(...)
  if (is.null(params)) {
    if (!is.null(desiredtz_part1) && !("desiredtz" %in% names(overrides))) {
      overrides$desiredtz <- desiredtz_part1
    }
    params <- .raw.pipeline.params(NULL, overrides)
  } else {
    params <- .raw.pipeline.params(params, overrides)
    # the milestone's desiredtz wins
    if (!is.null(desiredtz_part1) && !identical(params$desiredtz, desiredtz_part1)) {
      params <- .raw.calibrate.params(list(params = NULL), params, list(desiredtz = desiredtz_part1))
    }
  }
  params["progress"] <- list(NULL)
  stage_times <- c(inspect = 0, calibrate = 0, getmeta = 0, wear = NA_real_, quality = NA_real_, total = NA_real_)

  t0 <- Sys.time()
  info <- .raw.info.from.ggir(I, params, milestone = path)
  stage_times[["inspect"]] <- .raw.pipeline.secs(t0)
  t0 <- Sys.time()
  cal <- .raw.calibration.from.ggir(C, filename = I$filename, milestone = path)
  stage_times[["calibrate"]] <- .raw.pipeline.secs(t0)
  t0 <- Sys.time()
  meta <- .raw.meta.from.ggir(M, desiredtz = if (is.null(desiredtz_part1)) params$desiredtz else desiredtz_part1,
                              filename = I$filename, I = I, tail_expansion_log = tail_expansion_log,
                              params = params, milestone = path)
  stage_times[["getmeta"]] <- .raw.pipeline.secs(t0)
  t0 <- Sys.time()
  wear <- raw.wear.decision(meta, params = params)
  stage_times[["wear"]] <- .raw.pipeline.secs(t0)
  t0 <- Sys.time()
  quality <- raw.quality.report(info, cal, meta, wear, params = params)
  checks <- raw.quality.checks(info, cal, meta, wear, params = params)
  stage_times[["quality"]] <- .raw.pipeline.secs(t0)
  stage_times[["total"]] <- .raw.pipeline.secs(t_total)

  x <- .raw.pipeline.build(info, cal, meta, wear, quality, checks, params, stage_times, md5 = FALSE)
  x$file$milestone <- path
  x$status$messages <- c(x$status$messages,
                         stats::setNames(paste0("Loaded from GGIR milestone ", path,
                                                if ("GGIRversion" %in% nms) paste0(" (GGIRversion ", as.character(get("GGIRversion", envir = env)), ")") else ""),
                                         "milestone"))
  x
}

# PARTS 2, 3 AND 4

#' The GGIR Version to Stamp on a Part-2, Part-3 or Part-4 Milestone
#'
#' GGIR writes \code{utils::packageVersion("GGIR")} into every milestone from part 2
#' onwards. canhrActi writes the installed GGIR version when there is one and NA when
#' there is not, matching \code{\link{as.ggir.ms3}}; part 1's \code{\link{as.ggir.milestone}}
#' writes NA unconditionally.
#'
#' @return A package_version, or NA.
#' @keywords internal
#' @noRd
.raw.milestone.ggirversion <- function() {
  if (requireNamespace("GGIR", quietly = TRUE)) utils::packageVersion("GGIR") else NA
}

#' The File Name GGIR Gives a Milestone of Part 2 and Later
#'
#' The recording's file name with ".RData" appended unless it already holds ".RD", and with
#' no "meta_" prefix (only part 1 prefixes). g.part3 and g.part4 reuse the name of the file
#' they loaded, so all three folders hold the same name.
#'
#' @param fname The recording's file name.
#' @return The milestone file name.
#' @keywords internal
#' @noRd
.raw.milestone.name <- function(fname) {
  filename <- unlist(strsplit(fname, "/"))
  if (length(filename) > 0) {
    filename <- filename[length(filename)]
  } else {
    filename <- fname
  }
  if (length(unlist(strsplit(filename, "[.]RD"))) == 1) { # to avoid getting .RData.RData
    filename <- paste0(filename, ".RData")
  }
  filename
}

#' GGIR's Part-2 Summary Object
#'
#' g.part2's SUM is the whole per-file g.analyse summary plus daysummary and cosinor_ts,
#' built by \code{.raw.part2.sum}. For a configuration that function does not build, the
#' SUM is reduced to the two fields part 3 reads, \code{SUM$summary$ID} and, for a hip
#' sensor, \code{SUM$summary$if_hip_long_axis_id}, and marked partial.
#'
#' @param x A canhrActi_raw.
#' @return list(summary, daysummary, cosinor_ts = NULL).
#' @keywords internal
#' @noRd
.raw.milestone.sum <- function(x) {
  full <- .raw.part2.sum(x)
  if (!is.null(full)) return(full)
  id <- x$inspection$id
  if (is.list(id)) id <- unlist(id)
  if (length(id) != 1 || is.null(id)) id <- NA_character_
  axis <- if (!is.null(x$imputed) && !is.null(x$imputed$if_hip_long_axis_id)) {
    x$imputed$if_hip_long_axis_id
  } else if (!is.null(x$wear) && !is.null(x$wear$if_hip_long_axis_id)) {
    x$wear$if_hip_long_axis_id
  } else {
    NA
  }
  if (length(axis) != 1) axis <- NA
  if (identical(axis, "")) axis <- NA
  out <- list(summary = data.frame(ID = as.character(id), if_hip_long_axis_id = axis,
                                   stringsAsFactors = FALSE),
              daysummary = NULL, cosinor_ts = NULL)
  attr(out, "canhrActi_partial") <- TRUE
  out
}

#' The Objects GGIR Part 4 Saves, From a canhrActi Night Table
#'
#' Returns the three objects g.part4 writes into meta/ms4.out/<file>.RData, in its save
#' order, so that they can be compared with a stored milestone with \code{identical()} and
#' written with \code{\link{write.ggir.milestone}} for GGIR parts 5 and 6.
#'
#' @details Reproduces the \code{save()} of g.part4:
#'   \code{nightsummary, tail_expansion_log, GGIRversion}. The night summary is
#'   \code{\link{as.ggir.nightsummary}}'s, which is the plain data frame with canhrActi's
#'   class and attribute stripped. \code{tail_expansion_log} is a part-1 pass-through: it is
#'   taken from the night table's own attribute when \code{\link{raw.sleep.nights}} carried
#'   it, and from the \code{log} argument otherwise. \code{GGIRversion} is the installed GGIR
#'   version, or NA when GGIR is not installed.
#'
#' @param nights A canhrActi_raw_nights from \code{\link{raw.sleep.nights}}.
#' @param log NULL, or the part-1 tail_expansion_log to carry through.
#' @return list(nightsummary, tail_expansion_log, GGIRversion).
#' @examples
#' \dontrun{
#' ms4 <- as.ggir.ms4(nights)
#' e <- new.env(); load("meta/ms4.out/MOS2E39230594.gt3x.RData", envir = e)
#' identical(ms4$nightsummary, e$nightsummary)
#' }
#' @seealso \code{\link{as.ggir.ms3}}, \code{\link{write.ggir.milestone}}
#' @export
as.ggir.ms4 <- function(nights, log = NULL) {
  if (!inherits(nights, "canhrActi_raw_nights")) {
    stop("nights must be a canhrActi_raw_nights object from raw.sleep.nights()", call. = FALSE)
  }
  a <- attr(nights, "canhrActi")
  if (is.null(log) && is.list(a)) log <- a$tail_expansion_log
  out <- list()
  out["nightsummary"] <- list(as.ggir.nightsummary(nights))
  out["tail_expansion_log"] <- list(log)
  out["GGIRversion"] <- list(.raw.milestone.ggirversion())
  out
}

#' The Objects of One Milestone Part, With the Names and Order GGIR Saves Them Under
#'
#' The body of \code{\link{as.ggir.milestone}} for parts 2, 3, 4 and 5. Part 1 stays in
#' R/raw_pipeline.R.
#'
#' @param x A canhrActi_raw.
#' @param part 2, 3, 4 or 5.
#' @param nights For part 4, the canhrActi_raw_nights to write.
#' @param timeuse For part 5, the canhrActi_raw_timeuse to write.
#' @return A named list in GGIR's save order.
#' @keywords internal
#' @noRd
.raw.milestone.part <- function(x, part, nights = NULL, timeuse = NULL) {
  if (part == 5) {
    if (is.null(timeuse)) {
      stop(paste0("Part 5 is a separate call: pass timeuse = raw.timeuse(x, nights). ",
                  "canhrActi does not attach the time-use analysis to the recording, ",
                  "because one call with two light thresholds produces two complete ",
                  "part-5 tables for one recording."), call. = FALSE)
    }
    return(as.ggir.ms5(timeuse))
  }
  if (!inherits(x, "canhrActi_raw")) stop("x must be a canhrActi_raw object", call. = FALSE)
  log <- if (is.null(x$meta)) NULL else x$meta$tail_expansion_log
  if (part == 2) {
    # part 2 saves SUM, IMP, tail_expansion_log, GGIRversion
    imp <- x$imputed
    if (is.null(imp)) {
      stop(paste0("The recording carries no imputed series. Run x$imputed <- raw.impute(x) ",
                  "before asking for the part-2 milestone."), call. = FALSE)
    }
    out <- list()
    out["SUM"] <- list(.raw.milestone.sum(x))
    out["IMP"] <- list(as.ggir.IMP(imp))
    out["tail_expansion_log"] <- list(log)
    out["GGIRversion"] <- list(.raw.milestone.ggirversion())
    return(out)
  }
  if (part == 3) {
    if (is.null(x$sleep)) {
      stop(paste0("The recording carries no part-3 result. Run x$sleep <- raw.sleep.part3(x) ",
                  "before asking for the part-3 milestone."), call. = FALSE)
    }
    ms3 <- as.ggir.ms3(x$sleep)
    # the part-1 pass-through wins over the copy part 3 carried, as GGIR's own does
    ms3["tail_expansion_log"] <- list(log)
    return(ms3)
  }
  if (part == 4) {
    if (is.null(nights)) {
      stop(paste0("Part 4 is a separate call: pass nights = raw.sleep.nights(x$sleep). ",
                  "canhrActi does not attach the night table to the recording, because the ",
                  "same recording legitimately has several."), call. = FALSE)
    }
    return(as.ggir.ms4(nights, log = log))
  }
  stop("part must be 1, 2, 3, 4 or 5", call. = FALSE)
}

#' Write One Milestone File, in the Folder and Under the Name GGIR Uses
#'
#' @param objs The named list of objects, in GGIR's save order.
#' @param dir The folder to write into; created when missing.
#' @param file The file name.
#' @return The path written.
#' @keywords internal
#' @noRd
.raw.milestone.write <- function(objs, dir, file) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  env <- new.env(parent = emptyenv())
  for (nm in names(objs)) assign(nm, objs[[nm]], envir = env)
  path <- file.path(dir, file)
  save(list = names(objs), envir = env, file = path)
  path
}

#' Write the GGIR Milestone Folders for Parts 2, 3 and 4
#'
#' The multi-part half of \code{\link{write.ggir.milestone}}: creates meta/basic, meta/ms2.out,
#' meta/ms3.out, meta/ms4.out and meta/ms5.out under \code{root} the way GGIR's
#' \code{checkMilestoneFolders} does, and writes each requested part into its own folder.
#' Part 5 is not one file, so it is handed to \code{.raw.milestone.write.part5};
#' \code{results} and \code{results/QC} are created alongside because a real GGIR run
#' creates them and GGIR's report layer errors on a missing folder.
#'
#' @param x A canhrActi_raw, or NULL when only part 5 is being written.
#' @param root The metadatadir root.
#' @param parts Which parts to write.
#' @param nights For part 4, the canhrActi_raw_nights.
#' @param timeuse For part 5, the canhrActi_raw_timeuse.
#' @param reports For part 5, NULL or the report layer's nested list.
#' @param dictionary_per_report For part 5, see \code{\link{write.ggir.milestone}}.
#' @return A named character vector of the files written, one per part except part 5.
#' @keywords internal
#' @noRd
.raw.milestone.write.parts <- function(x, root, parts, nights = NULL, timeuse = NULL,
                                       reports = NULL, dictionary_per_report = FALSE) {
  root <- gsub("\\\\", "/", root)
  # the five folders checkMilestoneFolders creates, plus meta/sleep.qc from part 3 onwards
  folders <- c("1" = "meta/basic", "2" = "meta/ms2.out", "3" = "meta/ms3.out",
               "4" = "meta/ms4.out", "5" = "meta/ms5.out")
  for (p in as.character(sort(unique(parts)))) {
    dir.create(file.path(root, folders[[p]]), recursive = TRUE, showWarnings = FALSE)
  }
  if (any(parts >= 3)) {
    dir.create(file.path(root, "meta/sleep.qc"), recursive = TRUE, showWarnings = FALSE)
  }
  if (5L %in% parts) {
    dir.create(file.path(root, "results/QC"), recursive = TRUE, showWarnings = FALSE)
  }
  short <- NULL
  if (!is.null(x)) {
    fname <- x$file$filename
    if (is.null(fname) || length(fname) != 1 || is.na(fname)) {
      stop("The recording carries no file name (x$file$filename)", call. = FALSE)
    }
    short <- .raw.milestone.name(fname)
  }
  out <- character(0)
  for (p in sort(unique(parts))) {
    if (p == 5) {
      out <- c(out, .raw.milestone.write.part5(
        timeuse, root, reports = reports,
        ggir_exact = isTRUE(.raw.ms5.setting(timeuse, "ggir_exact", TRUE)),
        dictionary_per_report = dictionary_per_report))
      next
    }
    if (p == 1) {
      f <- write.ggir.milestone(x, file.path(root, "meta/basic"))
    } else {
      f <- .raw.milestone.write(.raw.milestone.part(x, p, nights = nights),
                                file.path(root, folders[[as.character(p)]]), short)
    }
    out <- c(out, stats::setNames(gsub("\\\\", "/", f), paste0("part", p)))
  }
  out
}

# PART 5

#' One Effective Part-5 Setting of a Time-Use Object
#'
#' The parameters part 5 ran with are kept in the object, so the writer reads them from there
#' rather than taking a second parameter object that could disagree with the numbers.
#'
#' @param timeuse A canhrActi_raw_timeuse.
#' @param name The parameter name.
#' @param default The value to use when the object carries none.
#' @return The setting.
#' @keywords internal
#' @noRd
.raw.ms5.setting <- function(timeuse, name, default) {
  p <- timeuse$settings$params
  if (is.null(p)) return(default)
  .raw.param(p, name, default)
}

#' The Name GGIR Gives the Part-5 Milestone File
#'
#' GGIR writes into meta/ms5.out under the name of the part-3 milestone it loaded, which is
#' also column 2 of every output row.
#'
#' @param timeuse A canhrActi_raw_timeuse.
#' @return The file name.
#' @keywords internal
#' @noRd
.raw.ms5.name <- function(timeuse) {
  fn <- timeuse$settings$filename
  if (is.null(fn) || length(fn) != 1 || is.na(fn) || !nzchar(fn)) {
    fn <- timeuse$settings$filename_dir
    if (is.null(fn) || length(fn) != 1 || is.na(fn) || !nzchar(fn)) {
      stop("The time-use object carries no file name (settings$filename); part 5's files ",
           "are all named after the part-3 milestone", call. = FALSE)
    }
    fn <- .raw.milestone.name(fn)
  }
  as.character(fn)
}

#' GGIR's Two Basename Strip Rules for Part-5 File Names
#'
#' The time-series file name strips gt3x and the sib report does not, so one recording gives
#' MOS2E39230594 and MOS2E39230594gt3x from the same milestone name. Both are verbatim,
#' including the unanchored dot class that also eats any dot inside the name:
#' EE_left_29.5.2017-05-30.gt3x.RData gives EE_left_2952017-05-30.
#'
#' @param fname The part-3 milestone file name.
#' @param drop_gt3x TRUE for the time-series rule, FALSE for the sib-report rule.
#' @return The stripped name.
#' @keywords internal
#' @noRd
.raw.ms5.strip <- function(fname, drop_gt3x = TRUE) {
  if (drop_gt3x) {
    gsub(pattern = "[.]|rdata|csv|cwa|gt3x|bin", replacement = "", x = fname,
         ignore.case = TRUE)
  } else {
    gsub(pattern = "[.]|RData|csv|cwa|bin", replacement = "", x = fname,
         ignore.case = TRUE)
  }
}

#' Write the Behavioural-Class Legend
#'
#' GGIR writes it once per run into meta/ms5.outraw, under a name carrying
#' \code{as.Date(Sys.time())} (the UTC date, not the recording's), and only when the file
#' does not already exist, so a second run on a later day leaves two of them. Both
#' behaviours are reproduced, so this file cannot be byte-compared by name against a
#' reference run made on another day.
#' @param timeuse A canhrActi_raw_timeuse.
#' @param outraw The meta/ms5.outraw folder.
#' @param sep,dec The csv separators.
#' @return The path written, or character(0).
#' @keywords internal
#' @noRd
.raw.ms5.write.legend <- function(timeuse, outraw, sep = ",", dec = ".") {
  legendtable <- timeuse$legend
  if (is.null(legendtable) || nrow(legendtable) == 0) return(character(0))
  legendfile <- file.path(outraw, paste0("behavioralcodes", as.Date(Sys.time()), ".csv"))
  if (!file.exists(legendfile)) {
    data.table::fwrite(legendtable, file = legendfile, row.names = FALSE, sep = sep,
                       dec = dec)
  }
  legendfile
}

#' Write the Exported Time Series
#'
#' One folder per threshold triple, named \code{<TRLi>_<TRMi>_<TRVi>}, and inside it one
#' file per sib definition named \code{<stripped>_<sibDef>} with the extension of each
#' requested format. The frame written is the last timewindow's, because GGIR calls
#' savetimeseries after the timewindow loop and the name carries no timewindow tag, so on
#' a \code{c("MM", "WW")} run the MM pass is overwritten by the WW pass. The csv has 13
#' columns and the RData 14: GGIR re-appends \code{timestamp} after writing the csv. The
#' RData holds mdat, filename, Lnames and desiredtz, in that order; GGIR's
#' \code{save(..., desiredtz_part1 = desiredtz)} does not rename the object, so the stored
#' name is \code{desiredtz}.
#'
#' @param timeuse A canhrActi_raw_timeuse.
#' @param outraw The meta/ms5.outraw folder.
#' @param stripped The time-series basename rule applied to the milestone name.
#' @param format Which of "csv" and "RData" to write.
#' @param sep,dec The csv separators.
#' @return The paths written.
#' @keywords internal
#' @noRd
.raw.ms5.write.series <- function(timeuse, outraw, stripped, format = "RData",
                                  sep = ",", dec = ".") {
  if (is.null(timeuse$series) || length(timeuse$series) == 0) return(character(0))
  out <- character(0)
  filename <- timeuse$settings$filename_dir
  desiredtz <- timeuse$settings$desiredtz
  if (is.null(desiredtz)) desiredtz <- ""
  for (triple in names(timeuse$series)) {
    dir.create(file.path(outraw, triple), recursive = TRUE, showWarnings = FALSE)
    # the class ladder is per threshold triple when the nap branch grew it
    Lnames <- if (is.list(timeuse$levels)) timeuse$levels[[triple]] else timeuse$levels
    for (sibDef in names(timeuse$series[[triple]])) {
      frames <- timeuse$series[[triple]][[sibDef]]
      if (length(frames) == 0) next
      mdat <- frames[[length(frames)]]
      stem <- file.path(outraw, triple, paste0(stripped, "_", sibDef))
      if ("csv" %in% format) {
        csvdat <- mdat
        if ("timestamp" %in% names(csvdat)) {
          csvdat <- csvdat[, -which(names(csvdat) == "timestamp")]
        }
        data.table::fwrite(csvdat, paste0(stem, ".csv"), row.names = FALSE, sep = sep,
                           dec = dec)
        out <- c(out, paste0(stem, ".csv"))
      }
      if ("RData" %in% format) {
        save(mdat, filename, Lnames, desiredtz_part1 = desiredtz,
             file = paste0(stem, ".RData"))
        out <- c(out, paste0(stem, ".RData"))
      }
    }
  }
  out
}

#' Write the sib Reports
#'
#' One csv per sib definition under meta/ms5.outraw/sib.reports, named
#' \code{sib_report_<stripped2>_<sibDef>.csv} with the strip rule that leaves gt3x in the
#' name. \code{dateTimeAs = "write.csv"} makes the two timestamp columns come out as local
#' wall clock rather than as seconds. An empty report is not written.
#' @param timeuse A canhrActi_raw_timeuse.
#' @param outraw The meta/ms5.outraw folder.
#' @param stripped2 The sib-report basename rule applied to the milestone name.
#' @param sep,dec The csv separators.
#' @return The paths written.
#' @keywords internal
#' @noRd
.raw.ms5.write.sibreports <- function(timeuse, outraw, stripped2, sep = ",", dec = ".") {
  if (is.null(timeuse$sibreport) || length(timeuse$sibreport) == 0) return(character(0))
  dir.create(file.path(outraw, "sib.reports"), recursive = TRUE, showWarnings = FALSE)
  out <- character(0)
  for (sibDef in names(timeuse$sibreport)) {
    sibreport <- timeuse$sibreport[[sibDef]]
    if (length(sibreport) == 0) next
    f <- file.path(outraw, "sib.reports",
                   paste0("sib_report_", stripped2, "_", sibDef, ".csv"))
    data.table::fwrite(x = sibreport, file = f, row.names = FALSE, sep = sep, dec = dec,
                       dateTimeAs = "write.csv")
    out <- c(out, f)
  }
  out
}

#' The GGIR File-Name Suffix of One Report Configuration
#'
#' GGIR builds every report name as \code{<window>_L<TRLi>M<TRMi>V<TRVi>_<sleepparam>}.
#'
#' @param entry One configuration of the \code{reports} list.
#' @param nm The name that configuration had in the list.
#' @return The suffix.
#' @keywords internal
#' @noRd
.raw.ms5.report.suffix <- function(entry, nm) {
  has <- function(k) !is.null(entry[[k]]) && length(entry[[k]]) == 1 && !is.na(entry[[k]])
  if (all(vapply(c("window", "TRLi", "TRMi", "TRVi", "sleepparam"), has, logical(1)))) {
    return(paste0(entry$window, "_L", entry$TRLi, "M", entry$TRMi, "V", entry$TRVi,
                  "_", entry$sleepparam))
  }
  if (is.null(nm) || is.na(nm) || !nzchar(nm)) {
    stop("every configuration in reports must either carry window, TRLi, TRMi, TRVi and ",
         "sleepparam, or be named with the report suffix GGIR would use, for example ",
         "\"MM_L40M100V400_T5A5\"", call. = FALSE)
  }
  nm
}

#' Write the Report csv Files of Every Configuration
#'
#' The frames are written as they come, with \code{na = ""}: the rounding, the validity
#' filter and the person aggregation all happen in the report layer. A NULL member is not
#' written, which is how GGIR behaves when no valid day exists: the QC csv is still
#' written and the cleaned day csv and the person csv are not.
#' @param reports The nested list of configurations.
#' @param root The metadatadir root.
#' @param sep,dec The csv separators.
#' @return The paths written.
#' @keywords internal
#' @noRd
.raw.ms5.write.reports <- function(reports, root, sep = ",", dec = ".") {
  if (is.null(reports) || length(reports) == 0) return(character(0))
  if (!is.list(reports)) stop("reports must be a list of configurations", call. = FALSE)
  dir.create(file.path(root, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  nms <- names(reports)
  out <- character(0)
  for (i in seq_along(reports)) {
    entry <- reports[[i]]
    if (!is.list(entry)) {
      stop("every element of reports must be a list of daysummary_full, ",
           "daysummary_cleaned and personsummary", call. = FALSE)
    }
    sfx <- .raw.ms5.report.suffix(entry, if (is.null(nms)) NA_character_ else nms[i])
    write1 <- function(df, path) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      data.table::fwrite(df, path, row.names = FALSE, na = "", sep = sep, dec = dec)
      path
    }
    out <- c(out,
             write1(entry$daysummary_full,
                    file.path(root, "results", "QC",
                              paste0("part5_daysummary_full_", sfx, ".csv"))),
             write1(entry$daysummary_cleaned,
                    file.path(root, "results", paste0("part5_daysummary_", sfx, ".csv"))),
             write1(entry$personsummary,
                    file.path(root, "results", paste0("part5_personsummary_", sfx, ".csv"))))
  }
  out
}

#' Write the Variable Dictionaries
#'
#' GGIR's own file choice and file names: it lists \code{results/part5*.csv}, takes the
#' first day summary and the first person summary in \code{dir()}'s order, and writes
#' \code{results/variableDictionary/part5_dictionary_daysummary.csv} and
#' \code{..._personsummary.csv}, whose names carry no configuration, so on a
#' \code{c("MM", "WW")} run both describe MM. When no cleaned report exists it falls back
#' to \code{results/QC} and describes every file there, and because the name rule maps
#' them all to \code{part5_dictionary_daysummary_full.csv}, the last one written wins.
#' \code{per_report = TRUE} writes one dictionary per report, named with that report's own
#' suffix; off by default so the export stays byte-compatible with GGIR.
#'
#' @param root The metadatadir root.
#' @param sep,dec The csv separators.
#' @param ggir_exact Passed to \code{\link{raw.timeuse.dictionary}}.
#' @param per_report One dictionary per report written.
#' @return The paths written.
#' @keywords internal
#' @noRd
.raw.ms5.write.dictionary <- function(root, sep = ",", dec = ".", ggir_exact = TRUE,
                                      per_report = FALSE) {
  resdir <- file.path(root, "results")
  # dir() and not sort(dir()): GGIR's pick is dir()'s own order and re-sorting could differ
  # under another collation
  reports <- dir(resdir, full.names = TRUE, pattern = "^part5.*\\.csv$")
  if (per_report == FALSE) {
    # GGIR's pick: one day summary and one person summary (both MM on a c("MM", "WW") run)
    ds <- grep("^part5_daysummary", basename(reports))[1]
    ps <- grep("^part5_personsummary", basename(reports))[1]
    reports <- reports[c(ds, ps)]
  }
  if (length(reports) == 0 || all(is.na(reports))) {
    # No cleaned part 5 report probably because no valid windows, so try full report instead
    reports <- dir(file.path(resdir, "QC"), full.names = TRUE, pattern = "^part5.*\\.csv$")
    if (length(reports) == 0 || all(is.na(reports))) return(character(0))
  }
  reports <- reports[!is.na(reports)]
  directory <- file.path(resdir, "variableDictionary")
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  out <- character(0)
  # GGIR runs both reports in one call, so under ggir_exact the leaked tokens carry from
  # the last column of the first report into the first column of the second
  elements <- NULL
  for (ri in seq_along(reports)) {
    cnames <- colnames(data.table::fread(reports[ri], verbose = FALSE, nrows = 2))
    person <- grepl("personsummary", reports[ri])
    r <- .raw.dictionary.table(cnames, person = person, elements = elements,
                               ggir_exact = ggir_exact)
    elements <- r$elements
    fn <- gsub("part5_", "part5_dictionary_", basename(reports[ri]))
    if (per_report == FALSE) {
      fn <- unlist(strsplit(fn, "_MM|_WW|_OO"))[1]
      fn <- paste0(fn, ".csv")
    }
    data.table::fwrite(r$dictionary, file = file.path(directory, fn), row.names = FALSE,
                       na = "", sep = sep, dec = dec)
    out <- c(out, file.path(directory, fn))
  }
  unique(out)
}

#' Write Everything Part 5 Writes
#'
#' The part-5 half of \code{\link{write.ggir.milestone}}. The order is GGIR's: the sib
#' reports, the legend and the time series are written inside the loop nest, and the ms5
#' milestone last. The ms5 file is written only when the day table still has a
#' \code{boutdur.mvpa} column and at least one row, which is GGIR's own gate: a recording
#' with no analysable window leaves no part-5 file.
#'
#' @param timeuse A canhrActi_raw_timeuse.
#' @param root The metadatadir root.
#' @param reports NULL, or the report layer's nested list.
#' @param ggir_exact Passed to the dictionary.
#' @param dictionary_per_report One dictionary per report written.
#' @return A named character vector of the files written.
#' @keywords internal
#' @noRd
.raw.milestone.write.part5 <- function(timeuse, root, reports = NULL, ggir_exact = TRUE,
                                       dictionary_per_report = FALSE) {
  fname <- .raw.ms5.name(timeuse)
  sep <- .raw.ms5.setting(timeuse, "sep_reports", ",")
  dec <- .raw.ms5.setting(timeuse, "dec_reports", ".")
  format <- .raw.ms5.setting(timeuse, "save_ms5raw_format", "RData")
  save_ms5rawlevels <- isTRUE(.raw.ms5.setting(timeuse, "save_ms5rawlevels", TRUE))
  do.sibreport <- isTRUE(.raw.ms5.setting(timeuse, "do.sibreport", TRUE))
  out <- character(0)
  # GGIR gates the legend and the series on save_ms5rawlevels || part6HCA || part6CR; part 6
  # is out of scope, so the gate is save_ms5rawlevels alone. meta/ms5.outraw exists when
  # either the series or the sib reports are wanted.
  outraw <- file.path(root, "meta", "ms5.outraw")
  if (save_ms5rawlevels == TRUE || do.sibreport == TRUE) {
    dir.create(outraw, recursive = TRUE, showWarnings = FALSE)
  }
  if (save_ms5rawlevels == TRUE) {
    # one folder per configuration of the threshold grid, in GGIR's own nesting order
    configurations <- c()
    for (TRLi in timeuse$settings$threshold.lig) {
      for (TRMi in timeuse$settings$threshold.mod) {
        for (TRVi in timeuse$settings$threshold.vig) {
          configurations <- c(configurations, paste0(TRLi, "_", TRMi, "_", TRVi))
        }
      }
    }
    for (hi in unique(c(configurations, names(timeuse$series)))) {
      dir.create(file.path(outraw, hi), recursive = TRUE, showWarnings = FALSE)
    }
  }
  if (do.sibreport == TRUE) {
    p <- .raw.ms5.write.sibreports(timeuse, outraw, .raw.ms5.strip(fname, drop_gt3x = FALSE),
                                   sep = sep, dec = dec)
    out <- c(out, stats::setNames(p, rep("part5_sibreport", length(p))))
  }
  if (save_ms5rawlevels == TRUE) {
    p <- .raw.ms5.write.legend(timeuse, outraw, sep = sep, dec = dec)
    out <- c(out, stats::setNames(p, rep("part5_legend", length(p))))
    p <- .raw.ms5.write.series(timeuse, outraw, .raw.ms5.strip(fname, drop_gt3x = TRUE),
                               format = format, sep = sep, dec = dec)
    out <- c(out, stats::setNames(p, rep("part5_series", length(p))))
  }
  # the milestone itself, under the same gate GGIR applies
  day <- timeuse$daysummary
  if (!is.null(day) && nrow(day) > 0 && "boutdur.mvpa" %in% colnames(day)) {
    p <- .raw.milestone.write(as.ggir.ms5(timeuse), file.path(root, "meta", "ms5.out"),
                              fname)
    out <- c(out, stats::setNames(p, "part5"))
  }
  # the reports and the two dictionaries, when the report layer supplied them
  if (!is.null(reports)) {
    p <- .raw.ms5.write.reports(reports, root, sep = sep, dec = dec)
    out <- c(out, stats::setNames(p, rep("part5_report", length(p))))
    p <- .raw.ms5.write.dictionary(root, sep = sep, dec = dec, ggir_exact = ggir_exact,
                                   per_report = dictionary_per_report)
    out <- c(out, stats::setNames(p, rep("part5_dictionary", length(p))))
  }
  gsub("\\\\", "/", out)
}
