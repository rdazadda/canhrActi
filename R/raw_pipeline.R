# Ported from GGIR 3.3-9 R/g.part1.R, the per-file orchestration
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the per-file body of g.part1
# (inspect, calibrate, decide, epoch tables, tail expansion, save) became
# read.raw.accelerometer, which calls the canhrActi ports of those stages and adds the
# part-2 wear decision and quality report; the file discovery, output folder,
# already-processed check, parallel loop and console output are removed; corrupt and
# too-short recordings are states of the returned object, as in GGIR; the per-stage wall
# clock, a progress callback, print and summary methods and as.ggir.milestone are added.
# No arithmetic lives in this file.

# PARAMETER AND PROGRESS PLUMBING

#' Resolve the Parameter Object of the Pipeline
#'
#' NULL gives GGIR's defaults through raw.params(); a canhrActi_raw_params object is used as
#' is; a plain named list is passed through raw.params() so it gets GGIR's checks and
#' coercions. Named overrides (from ...) are applied through raw.params() as well.
#'
#' @param params NULL, a canhrActi_raw_params or a named list.
#' @param overrides Named list of individual overrides.
#' @return A canhrActi_raw_params object.
#' @keywords internal
#' @noRd
.raw.pipeline.params <- function(params = NULL, overrides = list()) {
  if (is.null(params)) {
    params <- raw.params()
  } else if (!inherits(params, "canhrActi_raw_params")) {
    if (!is.list(params)) stop("params must be NULL, a raw.params() object or a named list", call. = FALSE)
    known <- names(.raw.params.defaults())
    bad <- setdiff(names(params), known)
    if (length(bad) > 0) stop("Unknown parameter(s): ", paste(bad, collapse = ", "), call. = FALSE)
    params <- do.call(raw.params, params)
  }
  .raw.calibrate.params(list(params = NULL), params, overrides)
}

#' Wrap the User's Progress Callback
#'
#' The block loops of \code{raw.calibrate} and \code{raw.getmeta} report the block number
#' with \code{n = NA} because the number of blocks is not known in advance (a gt3x block
#' counts recorded seconds, and idle sleep stretches it over an unknown wall clock). The
#' wrapper substitutes an upper bound derived from the header span when one is available,
#' so a progress bar has a denominator; the true count is at most that bound.
#'
#' @param progress The user's \code{function(stage, i, n, message)} or NULL.
#' @param n_est Named list of estimated block counts per stage (NA when unknown).
#' @return A function with the same signature, or NULL.
#' @keywords internal
#' @noRd
.raw.pipeline.progress <- function(progress = NULL, n_est = list()) {
  if (is.null(progress)) return(NULL)
  force(progress)
  function(stage, i, n, message) {
    if ((length(n) == 0 || is.na(n)) && !is.null(n_est[[stage]]) && !is.na(n_est[[stage]])) {
      n <- as.integer(n_est[[stage]])
    }
    progress(stage, i, n, message)
    invisible(NULL)
  }
}

#' Re-Signal an Error of One Stage With the File Path in the Message
#'
#' GGIR's stops (unrecognised format, deprecated csv, a decimal probe that cannot read the
#' file, a non-numeric column) keep their text; the path is appended when the text does not
#' already carry it, and the condition gets the class canhrActi_raw_pipeline_error with the
#' stage, the path and the original condition attached.
#'
#' @param e The caught condition.
#' @param stage Stage name.
#' @param path The file.
#' @keywords internal
#' @noRd
.raw.pipeline.stop <- function(e, stage, path) {
  msg <- conditionMessage(e)
  if (!grepl(path, msg, fixed = TRUE) && !grepl(basename(path), msg, fixed = TRUE)) {
    msg <- paste0(msg, " (stage ", stage, ", file ", path, ")")
  } else {
    msg <- paste0(msg, " (stage ", stage, ")")
  }
  stop(structure(class = c("canhrActi_raw_pipeline_error", "error", "condition"),
                 list(message = msg, call = NULL, stage = stage, path = path, parent = e)))
}

#' Seconds Elapsed Since a Time Stamp
#' @keywords internal
#' @noRd
.raw.pipeline.secs <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))

# THE BLOCKS OF THE OBJECT

#' A Header Timing Field as a GMT-Labelled Device Wall-Clock Time
#'
#' gt3x headers carry device wall-clock times without a zone; read.gt3x gives them as ticks
#' that raw.inspect keeps in header_list as GMT-labelled POSIXct (independent of the
#' read.gt3x version). Falls back to the formatted header string (read.gt3x 1.2.0 formats
#' it in GMT) for a GGIR I header.
#'
#' @param info A canhrActi_raw_info (or the reconstruction from a GGIR I list).
#' @param name Header field ("Start Date", "Last Sample Time", ...).
#' @return A POSIXct of length one labelled GMT, NA when the field is absent.
#' @keywords internal
#' @noRd
.raw.pipeline.header.time <- function(info, name) {
  na <- .POSIXct(NA_real_, tz = "GMT")
  hl <- info$header_list
  if (!is.null(hl) && !is.null(hl[[name]])) {
    v <- hl[[name]]
    if (inherits(v, "POSIXt")) return(.POSIXct(as.numeric(v), tz = "GMT"))
  }
  h <- info$header
  if (is.data.frame(h) && name %in% rownames(h)) {
    t <- suppressWarnings(as.POSIXct(as.character(h[name, 1]), format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
    if (length(t) == 1 && !is.na(t)) return(t)
  }
  na
}

#' One Header Field as a Character Scalar, NA When Absent
#' @keywords internal
#' @noRd
.raw.pipeline.header.chr <- function(info, name) {
  hl <- info$header_list
  if (!is.null(hl) && !is.null(hl[[name]])) return(as.character(hl[[name]])[1])
  h <- info$header
  if (is.data.frame(h) && name %in% rownames(h)) return(as.character(h[name, 1]))
  NA_character_
}

#' The File Block of a canhrActi_raw Object
#'
#' @param info A canhrActi_raw_info.
#' @param md5 Whether to compute the md5 checksum of the file (tools::md5sum).
#' @return list(path, filename, size_bytes, too_small, uppercase_extension, md5).
#' @keywords internal
#' @noRd
.raw.pipeline.file <- function(info, md5 = TRUE) {
  path <- info$path
  if (is.null(path)) path <- NA_character_
  sum <- NA_character_
  if (md5 && !is.na(path) && file.exists(path)) sum <- unname(tools::md5sum(path))
  list(path = path,
       filename = info$filename,
       size_bytes = if (is.null(info$size_bytes)) NA_real_ else as.numeric(info$size_bytes),
       too_small = if (is.null(info$too_small)) NA else info$too_small,
       uppercase_extension = if (is.null(info$uppercase_extension)) NA else info$uppercase_extension,
       md5 = sum)
}

#' The Device Block of a canhrActi_raw Object
#'
#' The brand and format names GGIR stores in I, the serial without GGIR's "_firmware_"
#' suffix, the firmware, the header fields that describe the device and its clock, the
#' dynamic range and clipping threshold GGIR derives at inspection, and GGIR's formatted
#' header data.frame.
#'
#' @param info A canhrActi_raw_info.
#' @return A list of 14 members.
#' @keywords internal
#' @noRd
.raw.pipeline.device <- function(info) {
  serial_ggir <- if (is.null(info$device_serial)) "not extracted" else as.character(info$device_serial)
  serial <- sub("_firmware_.*$", "", serial_ggir)
  scale <- suppressWarnings(as.numeric(.raw.pipeline.header.chr(info, "Acceleration Scale")))
  list(brand = if (is.null(info$monn)) NA_character_ else info$monn,
       format = if (is.null(info$dformn)) NA_character_ else info$dformn,
       serial = serial,
       serial_prefix = if (is.null(info$serial_prefix)) NA_character_ else info$serial_prefix,
       firmware = if (is.null(info$firmware)) NA_character_ else info$firmware,
       device_type = .raw.pipeline.header.chr(info, "Device Type"),
       sf = if (is.null(info$sf)) NA_real_ else as.numeric(info$sf),
       dynrange = if (is.null(info$dynrange)) NA_real_ else as.numeric(info$dynrange),
       clipthres = if (is.null(info$clipthres)) NA_real_ else as.numeric(info$clipthres),
       header_timezone = if (is.null(info$header_timezone)) NA_character_ else info$header_timezone,
       header_start = .raw.pipeline.header.time(info, "Start Date"),
       header_last_sample = .raw.pipeline.header.time(info, "Last Sample Time"),
       acceleration_scale = if (length(scale) == 1) scale else NA_real_,
       header = info$header)
}

#' The Versions Block of a canhrActi_raw Object
#' @keywords internal
#' @noRd
.raw.pipeline.versions <- function() {
  list(canhrActi = .raw.quality.version("canhrActi"),
       read.gt3x = .raw.quality.version("read.gt3x"),
       GGIRread = .raw.quality.version("GGIRread"),
       R = R.version.string,
       run_time = Sys.time())
}

#' Assemble a canhrActi_raw Object From the Stage Results
#'
#' Shared by \code{read.raw.accelerometer} (live run) and \code{read.ggir.milestone}
#' (objects loaded from a GGIR meta_*.RData).
#'
#' @param info,cal,meta,wear The four stage objects.
#' @param quality,checks The quality report row and the check table.
#' @param params The parameter object used.
#' @param stage_times Named numeric seconds per stage.
#' @param md5 Whether to checksum the file.
#' @return An object of class canhrActi_raw.
#' @keywords internal
#' @noRd
.raw.pipeline.build <- function(info, cal, meta, wear, quality, checks, params, stage_times,
                                md5 = TRUE, imputed = NULL, sleep = NULL,
                                extra_messages = character()) {
  desiredtz <- .raw.param(params, "desiredtz", "")
  configtz <- .raw.param(params, "configtz", NULL)
  machine_tz <- Sys.timezone()
  effective_tz <- if (length(desiredtz) == 1 && nzchar(desiredtz)) desiredtz else machine_tz
  tz <- list(desiredtz = desiredtz)
  tz["configtz"] <- list(configtz)
  tz$machine_tz <- machine_tz
  tz$effective_tz <- effective_tz

  corrupt <- isTRUE(info$corrupt) || isTRUE(meta$filecorrupt)
  too_short <- isTRUE(meta$filetooshort)
  skipped <- isTRUE(info$skipped)
  messages <- c(stats::setNames(as.character(info$messages), rep("inspect", length(info$messages))),
                stats::setNames(as.character(cal$messages), rep("calibrate", length(cal$messages))),
                stats::setNames(as.character(meta$messages), rep("getmeta", length(meta$messages))),
                stats::setNames(as.character(wear$messages), rep("wear", length(wear$messages))),
                extra_messages)
  status <- list(corrupt = corrupt, too_short = too_short, skipped = skipped,
                 messages = messages, stage_times = stage_times)

  structure(list(
    file = .raw.pipeline.file(info, md5 = md5),
    device = .raw.pipeline.device(info),
    tz = tz,
    params = params,
    status = status,
    calibration = cal,
    meta = meta,
    wear = wear,
    imputed = imputed,
    sleep = sleep,
    quality = quality,
    checks = checks,
    versions = .raw.pipeline.versions(),
    inspection = info
  ), class = "canhrActi_raw")
}

# THE PIPELINE

#' Read a Raw Accelerometer File as GGIR Part 1 Does
#'
#' Runs the canhrActi raw pipeline on one recording: inspection, auto-calibration, the
#' epoch tables (metashort and metalong), the part-2 wear decision and the data quality
#' report, in that order, and returns one object holding every stage. The numbers are those
#' of GGIR part 1 (and of the part-2 non-wear decision and QC report) for the same file and
#' parameters: every stage is a transcription of GGIR 3.3-9, so \code{as.ggir.milestone}
#' on the result reproduces the M, I and C objects of GGIR's meta_*.RData milestone with
#' identical().
#'
#' @details
#' Order of operations, as in GGIR's g.part1: \code{\link{raw.inspect}} (g.inspectfile),
#' \code{\link{raw.calibrate}} (g.calibrate and g.part1's identity-reset rule, or a
#' supplied calibration used as given), \code{\link{raw.getmeta}} (g.getmeta and g.part1's
#' tail expansion); then what GGIR does in part 2 for the QC report:
#' \code{\link{raw.wear.decision}} (g.weardec,
#' the g.impute mask, the valid hours per day) and \code{\link{raw.quality.report}} with
#' \code{\link{raw.quality.checks}}. GGIR's file discovery, output folder, skip of already
#' processed files and parallel loop are not part of this function; \code{raw.discover}
#' lists files and the caller loops.
#'
#' Corrupt and too-short recordings are states of the result, as they are in GGIR
#' (\code{status$corrupt}, \code{status$too_short}): a gt3x whose info.txt cannot be parsed
#' has \code{device$sf} NA, no calibration attempt, NULL tables and a quality row with
#' file.corrupt TRUE; a recording whose first block holds fewer than two hours and one
#' sample has \code{status$too_short} TRUE, a calibration message "not enough data" and NULL
#' tables. GGIR's stops (a path that does not exist, an unrecognised extension, the
#' deprecated GENEActiv csv, a file the decimal-separator probe cannot read, a non-numeric
#' column) remain errors, re-signalled with the path in the message as class
#' canhrActi_raw_pipeline_error. A file at or below minimumFileSizeMB is analysed and
#' flagged (\code{file$too_small}) unless \code{skip_small_files} is TRUE, in which case
#' \code{status$skipped} is TRUE and nothing is read.
#'
#' Timezone: GGIR ignores the device's TimeZone header; timestamps are the device's
#' wall-clock digits labelled with configtz (default desiredtz, default the machine's zone).
#' The object records desiredtz, configtz, the machine zone and the zone the timestamps carry
#' in \code{tz}, and check 22 of \code{checks} compares the header's offset with it.
#'
#' The progress callback is called before every block read of the calibration pass (stage
#' "calibrate", 12-hour blocks) and of the epoch pass (stage "getmeta", 24-hour blocks) with
#' the block number, an upper bound of the number of blocks derived from the header span
#' (NA when no span is known; idle sleep can make the true count smaller) and GGIR's
#' "Loading chunk" text, and once at the start of the other stages ("inspect", "wear",
#' "quality") and at the end ("done") with the elapsed time. With stream_gt3x on, a .gt3x
#' gets one more "getmeta" call after the epoch pass, with i NA and a message such as
#' "gt3x reader: 2 of 2 blocks read directly" (the blocks the stream reader served, of all
#' the blocks read).
#'
#' @param path Path to one raw accelerometer file (.gt3x, .bin, .cwa, .csv, optionally
#'   .gz-wrapped). Backslashes are accepted.
#' @param params A \code{\link{raw.params}} object (GGIR's defaults), or NULL for the same,
#'   or a named list passed through \code{raw.params()}.
#' @param calibration NULL to auto-calibrate, or a previously derived calibration to use as
#'   given (GGIR's backup.cal.coef "retrieve" semantics): a \code{canhrActi_raw_calibration},
#'   a GGIR C list from a milestone, a data_quality_report-shaped data.frame or a csv path.
#' @param progress NULL or a \code{function(stage, i, n, message)}; overrides
#'   \code{params$progress}.
#' @param sleep Run GGIR part 3, the sustained inactivity bouts and the sleep window, and
#'   attach it as \code{$sleep} (default TRUE); about 12 percent of the run. Set FALSE when
#'   only the data quality result is wanted. A failure inside part 3 leaves \code{$sleep}
#'   NULL and adds a message named "sleep" to \code{$status$messages} rather than failing
#'   the whole read. The part-2 imputed series \code{$imputed} is built either way, because
#'   GGIR parts 3 to 6 all read it rather than \code{$meta$metashort}.
#' @param ... Individual parameter overrides, e.g. \code{desiredtz = "America/Anchorage"},
#'   routed through \code{raw.params()}.
#'
#' @return An object of class "canhrActi_raw": a list with
#' \describe{
#'   \item{file}{path, filename, size_bytes, too_small, uppercase_extension, md5.}
#'   \item{device}{brand, format, serial (without GGIR's firmware suffix), serial_prefix,
#'     firmware, device_type, sf, dynrange, clipthres, header_timezone, header_start and
#'     header_last_sample (GMT-labelled device wall-clock times from the header),
#'     acceleration_scale, header (GGIR's formatted header data.frame).}
#'   \item{tz}{desiredtz, configtz, machine_tz, effective_tz (the zone the timestamps
#'     carry).}
#'   \item{params}{The parameter object as used, after GGIR's coercions, with the progress
#'     callback removed so the object serialises without closures.}
#'   \item{status}{corrupt, too_short, skipped, messages (a character vector of the GGIR
#'     warning texts and canhrActi notes of every stage, named by stage), stage_times (named
#'     numeric seconds: inspect, calibrate, getmeta, wear, quality, impute, sleep, total).}
#'   \item{calibration}{The \code{canhrActi_raw_calibration} (see \code{\link{raw.calibrate}}).}
#'   \item{meta}{The \code{canhrActi_raw_meta} (see \code{\link{raw.getmeta}}): metashort and
#'     metalong with GGIR's character timestamp verbatim plus a numeric time column,
#'     windowsizes, starttime, wday, wdayname, qclog, chunks and the rest.}
#'   \item{wear}{The \code{canhrActi_raw_wear} (see \code{\link{raw.wear.decision}}): rout,
#'     r5long, daily, wear_days and the recording summaries; status "no_data" without tables.}
#'   \item{imputed}{The \code{canhrActi_raw_imputed} of \code{\link{raw.impute}}: GGIR's part-2
#'     imputed short-epoch series, the average day and the completeness score. Every GGIR part
#'     from 3 onwards reads this, not \code{$meta$metashort}.}
#'   \item{sleep}{The \code{canhrActi_raw_sleep} of \code{\link{raw.sleep.part3}}, or NULL when
#'     \code{sleep = FALSE} or part 3 failed: the sustained inactivity bouts, the sleep window
#'     per night, the sleep regularity index and GGIR's whole part-3 milestone.}
#'   \item{quality, checks}{The one-row report of \code{\link{raw.quality.report}} and the
#'     25-row table of \code{\link{raw.quality.checks}}.}
#'   \item{versions}{canhrActi, read.gt3x, GGIRread, R, run_time.}
#'   \item{inspection}{The \code{canhrActi_raw_info} the stages ran on (what
#'     \code{as.ggir.milestone} needs for I and what a re-run of the quality report needs).}
#' }
#'
#' @examples
#' \dontrun{
#' x <- read.raw.accelerometer("MOS2E39230594.gt3x", desiredtz = "America/Anchorage")
#' x
#' summary(x)
#' ms <- as.ggir.milestone(x)
#' identical(ms$M$metashort, stored_M$metashort)
#' }
#' @seealso \code{\link{as.ggir.milestone}}, \code{\link{write.ggir.milestone}},
#'   \code{\link{read.ggir.milestone}}
#' @export
read.raw.accelerometer <- function(path, params = raw.params(), calibration = NULL,
                                   progress = NULL, sleep = TRUE, ...) {
  t_total <- Sys.time()
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("path must be a single file path", call. = FALSE)
  }
  path_in <- path
  path <- gsub("\\\\", "/", path)
  if (!file.exists(path)) {
    stop(structure(class = c("canhrActi_raw_pipeline_error", "error", "condition"),
                   list(message = paste0("File not found: ", path_in), call = NULL,
                        stage = "inspect", path = path_in, parent = NULL)))
  }
  if (dir.exists(path)) {
    stop(structure(class = c("canhrActi_raw_pipeline_error", "error", "condition"),
                   list(message = paste0(path_in, " is a directory; use raw.discover() to list the files in it"),
                        call = NULL, stage = "inspect", path = path_in, parent = NULL)))
  }
  params <- .raw.pipeline.params(params, list(...))
  # the .gt3x extraction is as large as the recording, so it goes when the run does,
  # however the run ends; read after ... is merged, so an override there counts
  if ((isTRUE(params$unzip_once) || isTRUE(params$stream_gt3x)) &&
      identical(tolower(tools::file_ext(path)), "gt3x")) {
    on.exit(try(.raw.gt3x.extract.cleanup(path), silent = TRUE), add = TRUE)
  }
  # the stages keep the one extraction until then instead of each removing its own
  .raw.gt3x.hold.env$depth <- .raw.gt3x.hold.env$depth + 1L
  on.exit(.raw.gt3x.hold.env$depth <- .raw.gt3x.hold.env$depth - 1L, add = TRUE)
  if (is.null(progress)) progress <- .raw.param(params, "progress", NULL)
  if (!is.null(progress) && !is.function(progress)) {
    stop("progress must be NULL or a function(stage, i, n, message)", call. = FALSE)
  }
  # the callback is passed explicitly; the stored parameter object carries no closure
  params["progress"] <- list(NULL)
  stage_times <- c(inspect = NA_real_, calibrate = NA_real_, getmeta = NA_real_,
                   wear = NA_real_, quality = NA_real_, impute = NA_real_,
                   sleep = NA_real_, total = NA_real_)
  prog <- .raw.pipeline.progress(progress)

  # inspect
  if (!is.null(prog)) prog("inspect", 0L, NA_integer_, paste0("Inspecting ", basename(path)))
  t0 <- Sys.time()
  info <- tryCatch(raw.inspect(path, params = params),
                   error = function(e) .raw.pipeline.stop(e, "inspect", path_in))
  stage_times[["inspect"]] <- .raw.pipeline.secs(t0)

  # block-count upper bounds for the progress bar (gt3x: one record per recorded second)
  n_est <- list(calibrate = NA_integer_, getmeta = NA_integer_)
  if (!is.null(info$sf) && identical(info$dformn, "gt3x")) {
    hs <- .raw.pipeline.header.time(info, "Start Date")
    hl <- .raw.pipeline.header.time(info, "Last Sample Time")
    span <- as.numeric(hl) - as.numeric(hs)
    if (!is.na(span) && span > 0) {
      if (!is.null(info$blocksize_calibrate)) n_est$calibrate <- ceiling(span / info$blocksize_calibrate) + 1L
      if (!is.null(info$blocksize_getmeta)) n_est$getmeta <- ceiling(span / info$blocksize_getmeta) + 1L
    }
  }
  prog <- .raw.pipeline.progress(progress, n_est)

  # calibrate and decide
  t0 <- Sys.time()
  cal <- tryCatch(raw.calibrate(info, params = params, progress = prog, calibration = calibration),
                  error = function(e) .raw.pipeline.stop(e, "calibrate", path_in))
  stage_times[["calibrate"]] <- .raw.pipeline.secs(t0)

  # the epoch tables and the tail expansion
  t0 <- Sys.time()
  meta <- tryCatch(raw.getmeta(info, calibration = cal, params = params, progress = prog),
                   error = function(e) .raw.pipeline.stop(e, "getmeta", path_in))
  stage_times[["getmeta"]] <- .raw.pipeline.secs(t0)

  # the part-2 wear decision
  if (!is.null(prog)) prog("wear", 0L, NA_integer_, "Wear decision (GGIR part 2)")
  t0 <- Sys.time()
  wear <- tryCatch(raw.wear.decision(meta, params = params),
                   error = function(e) .raw.pipeline.stop(e, "wear", path_in))
  stage_times[["wear"]] <- .raw.pipeline.secs(t0)

  # the QC report and the checks
  if (!is.null(prog)) prog("quality", 0L, NA_integer_, "Data quality report")
  t0 <- Sys.time()
  quality <- tryCatch(raw.quality.report(info, cal, meta, wear, params = params),
                      error = function(e) .raw.pipeline.stop(e, "quality", path_in))
  checks <- tryCatch(raw.quality.checks(info, cal, meta, wear, params = params),
                     error = function(e) .raw.pipeline.stop(e, "quality", path_in))
  stage_times[["quality"]] <- .raw.pipeline.secs(t0)

  # the part-2 imputed series; GGIR parts 3 to 6 all read it, so it is always built
  t0 <- Sys.time()
  imputed <- tryCatch(raw.impute(meta, wear, params),
                      error = function(e) .raw.pipeline.stop(e, "impute", path_in))
  stage_times[["impute"]] <- .raw.pipeline.secs(t0)

  # GGIR part 3, switchable; a failure here is a state on the object, not an error, since
  # the part 1 and 2 results are still valid
  sleep_out <- NULL
  sleep_msg <- character()
  if (isTRUE(sleep)) {
    if (!is.null(prog)) prog("sleep", 0L, NA_integer_, "Sleep detection (GGIR part 3)")
    t0 <- Sys.time()
    x0 <- .raw.pipeline.build(info, cal, meta, wear, quality, checks, params, stage_times,
                              md5 = FALSE, imputed = imputed)
    sleep_out <- tryCatch(raw.sleep.part3(x0, params = params, progress = progress),
                          error = function(e) {
                            sleep_msg <<- stats::setNames(conditionMessage(e), "sleep")
                            NULL
                          })
    stage_times[["sleep"]] <- .raw.pipeline.secs(t0)
  }
  stage_times[["total"]] <- .raw.pipeline.secs(t_total)

  x <- .raw.pipeline.build(info, cal, meta, wear, quality, checks, params, stage_times,
                           imputed = imputed, sleep = sleep_out,
                           extra_messages = sleep_msg)
  if (!is.null(prog)) {
    prog("done", NA_integer_, NA_integer_,
         paste0("Finished ", basename(path), " in ", round(stage_times[["total"]], 1), " s"))
  }
  x
}

# MILESTONE OBJECTS

#' The Objects GGIR Saves in a Milestone File, From a canhrActi_raw Object
#'
#' Returns the list of objects that one of GGIR's parts writes into its milestone RData
#' file, each in GGIR's shape and in GGIR's save order, so that they can be compared with a
#' stored milestone with identical() and written with \code{\link{write.ggir.milestone}} for
#' GGIR parts 2 to 6. The default, \code{part = 1}, is meta/basic/meta_<file>.RData.
#'
#' @details Part 1: M through \code{\link{as.ggir.M}} (g.getmeta's return list, the
#'   canhrActi time column dropped), I as g.inspectfile's eight-member list, C through
#'   \code{\link{as.ggir.C}} (the coefficients after g.part1's identity-reset rule, as the
#'   milestone stores them), desiredtz_part1 and tail_expansion_log (NULL unless
#'   recordingEndSleepHour fired). GGIRversion is NA: no GGIR code produced these objects.
#'   The two file-name copies g.part1 also saves (filename_dir and filefoldername) are
#'   added by \code{write.ggir.milestone}.
#'
#'   Parts 2 to 5 are built in R/raw_export.R and each reproduces its own GGIR save()
#'   call: part 2 is SUM, IMP, tail_expansion_log and GGIRversion, with the summary and
#'   daysummary g.part2 saves, so that GGIR's part-2 report can be run on the folder (for
#'   a configuration the port does not analyse, a partial SUM carrying only
#'   \code{summary$ID} and \code{summary$if_hip_long_axis_id}, the two fields part 3
#'   reads); part 3 is the fourteen objects of \code{\link{as.ggir.ms3}}; part 4 is
#'   nightsummary, tail_expansion_log and GGIRversion, built from \code{nights}; part 5 is
#'   output, tail_expansion_log, GGIRversion and last_timestamp, built from \code{timeuse}
#'   and stamping the time-use object's own \code{ggir_version_label}. Parts 2, 3 and 4
#'   stamp the installed GGIR version where part 1 stamps NA.
#'
#' @param x A canhrActi_raw object.
#' @param part 1 (the default), 2, 3, 4 or 5: which milestone's objects to return.
#' @param nights For \code{part = 4}, the \code{canhrActi_raw_nights} of
#'   \code{\link{raw.sleep.nights}}.
#' @param timeuse For \code{part = 5}, the \code{canhrActi_raw_timeuse} of
#'   \code{\link{raw.timeuse}}. Part 5 does not live on the recording object.
#' @param ... Not used.
#' @return For part 1, list(M, I, C, desiredtz_part1, GGIRversion = NA,
#'   tail_expansion_log); for part 2, list(SUM, IMP, tail_expansion_log, GGIRversion); for
#'   part 3, the fourteen ms3 objects; for part 4, list(nightsummary, tail_expansion_log,
#'   GGIRversion); for part 5, list(output, tail_expansion_log, GGIRversion,
#'   last_timestamp).
#' @examples
#' \dontrun{
#' ms <- as.ggir.milestone(x)
#' e <- new.env(); load("meta_MOS2E39230594.gt3x.RData", envir = e)
#' identical(ms$M[names(ms$M) != "bsc_qc"], e$M[names(e$M) != "bsc_qc"])
#' }
#' @seealso \code{\link{write.ggir.milestone}}, \code{\link{as.ggir.ms3}},
#'   \code{\link{as.ggir.ms4}}, \code{\link{as.ggir.ms5}}
#' @export
as.ggir.milestone <- function(x, part = 1, nights = NULL, timeuse = NULL, ...) {
  if (!is.numeric(part) || length(part) != 1 || is.na(part) || !(part %in% 1:5)) {
    stop("part must be 1, 2, 3, 4 or 5", call. = FALSE)
  }
  # part 5 is built from the time-use object alone, so it needs no recording
  if (part == 5) return(.raw.milestone.part(NULL, 5L, timeuse = timeuse))
  if (!inherits(x, "canhrActi_raw")) stop("x must be a canhrActi_raw object", call. = FALSE)
  # parts 2, 3, 4 and 5 live in R/raw_export.R with the rest of the milestone machinery
  if (part != 1) return(.raw.milestone.part(x, as.integer(part), nights = nights))
  M <- as.ggir.M(x$meta)
  I <- .raw.ggir.I(x$inspection)
  C <- as.ggir.C(x$calibration)
  out <- list(M = M, I = I, C = C, desiredtz_part1 = x$tz$desiredtz, GGIRversion = NA)
  out["tail_expansion_log"] <- list(x$meta$tail_expansion_log)
  out
}

# PRINT AND SUMMARY

#' Format Seconds for the Print Methods
#' @keywords internal
#' @noRd
.raw.pipeline.fmt.secs <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s)) return("NA")
  if (s < 60) paste0(round(s, 1), " s") else paste0(round(s / 60, 1), " min")
}

#' Print Method for a Raw Accelerometer Recording
#'
#' @param x A canhrActi_raw object.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw <- function(x, ...) {
  cat("\ncanhrActi raw accelerometer recording (GGIR part 1 semantics)\n")
  cat("  file:        ", x$file$filename,
      if (!is.na(x$file$size_bytes)) paste0(" (", round(x$file$size_bytes / 1e6, 1), " MB)") else "", "\n", sep = "")
  cat("  device:      ", x$device$brand, " ", x$device$format,
      if (!is.na(x$device$serial) && x$device$serial != "not extracted") paste0(", serial ", x$device$serial) else "",
      if (!is.na(x$device$firmware)) paste0(", firmware ", x$device$firmware) else "",
      ", ", if (is.na(x$device$sf)) "sample rate unknown" else paste0(x$device$sf, " Hz"), "\n", sep = "")
  cat("  timezone:    timestamps in ", x$tz$effective_tz,
      if (!is.na(x$device$header_timezone)) paste0(" (device header ", x$device$header_timezone, ")") else "", "\n", sep = "")
  if (isTRUE(x$status$skipped)) {
    cat("  status:      skipped (below minimumFileSizeMB)\n")
  } else if (isTRUE(x$status$corrupt)) {
    cat("  status:      corrupt; no epochs derived\n")
  } else if (isTRUE(x$status$too_short)) {
    cat("  status:      too short (under two hours of recorded data); no epochs derived\n")
  } else {
    m <- x$meta
    ws <- m$windowsizes
    cat("  epochs:      ", nrow(m$metashort), " short (", ws[1], " s) and ", nrow(m$metalong),
        " long (", ws[2], " s), from ", m$metashort$timestamp[1], " to ",
        m$metashort$timestamp[nrow(m$metashort)], "\n", sep = "")
    cal <- x$calibration
    cat("  calibration: ", if (isTRUE(cal$applied)) "applied" else "not applied", "; ", cal$qcmessage, "\n", sep = "")
    w <- x$wear
    if (identical(w$status, "ok")) {
      cat("  wear:        ", round(w$wear_dur_def_proto_day, 2), " of ", round(w$meas_dur_dys, 2),
          " days worn; ", w$n_valid_days, " valid day", if (w$n_valid_days == 1) "" else "s",
          " (", w$n_valid_weekdays, " weekdays, ", w$n_valid_weekend_days, " weekend days)\n", sep = "")
    }
  }
  if (is.data.frame(x$checks)) {
    st <- table(factor(x$checks$status, levels = c("ok", "warn", "fail", "info")))
    cat("  checks:      ", st[["ok"]], " ok, ", st[["warn"]], " warn, ", st[["fail"]], " fail, ",
        st[["info"]], " info\n", sep = "")
  }
  cat("  elapsed:     ", .raw.pipeline.fmt.secs(x$status$stage_times[["total"]]), "\n", sep = "")
  invisible(x)
}

#' Summary of a Raw Accelerometer Recording
#'
#' Collects the facts a reader wants to see before trusting the numbers: what was read,
#' how the calibration went, what the tables hold, how much of the recording was worn, and
#' which quality checks did not pass. \code{print} on the result writes them as plain prose.
#'
#' @param object A canhrActi_raw object.
#' @param ... Not used.
#' @return An object of class "summary.canhrActi_raw": a list with file, device, tz, status,
#'   stage_times, calibration, epochs, wear, days, flagged (the rows of the check table whose
#'   status is warn or fail) and messages.
#' @export
summary.canhrActi_raw <- function(object, ...) {
  x <- object
  m <- x$meta
  has <- !isTRUE(x$status$corrupt) && !isTRUE(x$status$too_short) && !isTRUE(x$status$skipped) &&
    is.data.frame(m$metashort) && nrow(m$metashort) > 0
  epochs <- NULL
  if (has) {
    ws <- m$windowsizes
    epochs <- list(n_short = nrow(m$metashort), n_long = nrow(m$metalong), ws3 = ws[1], ws2 = ws[2],
                   first = m$metashort$timestamp[1], last = m$metashort$timestamp[nrow(m$metashort)],
                   wdayname = m$wdayname, metrics = m$metric_names,
                   nonwear = table(factor(m$metalong$nonwearscore, levels = c(-1, 0, 1, 2, 3))),
                   gaps_n = if (!is.null(m$qclog) && "timegaps_n" %in% names(m$qclog)) sum(m$qclog$timegaps_n) else NA,
                   gaps_min = if (!is.null(m$qclog) && "timegaps_min" %in% names(m$qclog)) sum(m$qclog$timegaps_min) else NA,
                   blocks = if (is.data.frame(m$chunks)) nrow(m$chunks) else NA)
  }
  cal <- x$calibration
  calibration <- list(source = cal$source, qcmessage = cal$qcmessage, applied = isTRUE(cal$applied),
                      scale = cal$scale, offset = cal$offset, tempoffset = cal$tempoffset,
                      error_start = cal$cal_error_start, error_end = cal$cal_error_end,
                      hours = cal$nhoursused, still_windows = cal$npoints, use_temp = isTRUE(cal$use_temp))
  w <- x$wear
  wear <- if (identical(w$status, "ok")) {
    list(worn_days = w$wear_dur_def_proto_day, recorded_days = w$meas_dur_dys,
         n_valid_days = w$n_valid_days, n_valid_weekdays = w$n_valid_weekdays,
         n_valid_weekend_days = w$n_valid_weekend_days, r = colSums(w$rout[, c("r1", "r2", "r3", "r4", "r5")]))
  } else NULL
  days <- if (identical(w$status, "ok")) w$daily[, c("day", "date", "weekday", "n_hours", "n_valid_hours", "valid")] else NULL
  flagged <- if (is.data.frame(x$checks)) x$checks[x$checks$status %in% c("warn", "fail"), c("id", "check", "status", "message")] else NULL
  structure(list(file = x$file, device = x$device, tz = x$tz, status = x$status[c("corrupt", "too_short", "skipped")],
                 stage_times = x$status$stage_times, calibration = calibration, epochs = epochs,
                 wear = wear, days = days, flagged = flagged, messages = x$status$messages),
            class = "summary.canhrActi_raw")
}

#' Print Method for the Summary of a Raw Accelerometer Recording
#'
#' @param x A summary.canhrActi_raw object.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.summary.canhrActi_raw <- function(x, ...) {
  d <- x$device
  brand <- if (is.na(d$brand)) "device of unknown brand" else paste0(.raw.quality.brand(d$brand), " device")
  article <- if (grepl("^[AEIOUaeiou]", brand)) "an " else "a "
  cat("\nRecording ", x$file$filename, "\n", sep = "")
  cat("This file was read from ", article, brand,
      if (!is.na(d$device_type)) paste0(" (", d$device_type, ")") else "",
      if (!is.na(d$serial) && d$serial != "not extracted") paste0(" with serial number ", d$serial) else "",
      if (!is.na(d$firmware)) paste0(" and firmware ", d$firmware) else "",
      if (!is.na(d$sf)) paste0(", sampling at ", d$sf, " Hz") else "", ".", sep = "")
  if (!is.na(d$header_start) && !is.na(d$header_last_sample)) {
    cat(" The header says the device ran from ", format(d$header_start, "%Y-%m-%d %H:%M:%S"), " to ",
        format(d$header_last_sample, "%Y-%m-%d %H:%M:%S"), " on its own clock",
        if (!is.na(d$header_timezone)) paste0(", configured at UTC offset ", d$header_timezone) else "", ".", sep = "")
  }
  cat(" Timestamps are labelled ", x$tz$effective_tz, ", which is what GGIR does with desiredtz \"",
      x$tz$desiredtz, "\" on this machine.\n", sep = "")
  if (isTRUE(x$status$skipped)) {
    cat("\nThe file is at or below the minimum file size and was skipped, as GGIR would skip it.\n")
  } else if (isTRUE(x$status$corrupt)) {
    cat("\nThe file is corrupt: its header or its data could not be read, so no calibration was attempted and no epoch tables were built. GGIR reports such a file with file.corrupt TRUE and analyses nothing.\n")
  } else if (isTRUE(x$status$too_short)) {
    cat("\nThe file holds fewer than two hours and one sample of recorded data in its first block, which is GGIR's floor, so no epoch tables were built and the calibration could not be fitted.\n")
  } else {
    e <- x$epochs
    cal <- x$calibration
    cat("\nCalibration. ")
    if (identical(cal$source, "supplied")) {
      cat("The coefficients were supplied and used as given, without a new fit.")
    } else if (identical(cal$source, "not_attempted")) {
      cat("No auto-calibration was attempted.")
    } else {
      cat("The auto-calibration used ", round(cal$hours, 1), " hours of data and ", cal$still_windows,
          " still windows; the mean error of the still windows went from ",
          round(cal$error_start * 1000, 1), " mg to ", round(cal$error_end * 1000, 1), " mg. GGIR's message: ",
          cal$qcmessage, ". The coefficients ", if (cal$applied) "were applied" else "were not applied (identity used)",
          if (cal$use_temp) ", with the temperature term" else "", ".", sep = "")
    }
    cat("\n\nEpochs. The tables hold ", e$n_short, " short epochs of ", e$ws3, " s and ", e$n_long,
        " long epochs of ", e$ws2, " s, from ", e$first, " (", e$wdayname, ") to ", e$last,
        ", read in ", e$blocks, " block", if (!is.na(e$blocks) && e$blocks == 1) "" else "s",
        ". Metrics: ", paste(e$metrics, collapse = ", "), ".", sep = "")
    if (!is.na(e$gaps_n)) {
      cat(" The reader logged ", e$gaps_n, " time gaps totalling ", round(e$gaps_min, 1),
          " minutes, filled by last value carried forward as GGIR does.", sep = "")
    }
    nw <- e$nonwear
    cat(" Non-wear scores of the long epochs: ", nw[["0"]], " at 0, ", nw[["1"]], " at 1, ", nw[["2"]], " at 2 and ",
        nw[["3"]], " at 3", if (nw[["-1"]] > 0) paste0(", plus ", nw[["-1"]], " expanded epochs at -1") else "", ".\n", sep = "")
    if (!is.null(x$wear)) {
      w <- x$wear
      cat("\nWear. GGIR's part-2 decision marks ", w$r[["r1"]], " long epochs as non-wear, ", w$r[["r3"]],
          " more as implausible wear and ", w$r[["r2"]], " as clipped; ", round(w$worn_days, 2), " of ",
          round(w$recorded_days, 2), " recorded days count as worn. ", w$n_valid_days, " day",
          if (w$n_valid_days == 1) " reaches" else "s reach", " the valid-day criterion (",
          w$n_valid_weekdays, " on weekdays, ", w$n_valid_weekend_days, " at the weekend).\n", sep = "")
      if (is.data.frame(x$days) && nrow(x$days) > 0) {
        print(x$days, row.names = FALSE)
      }
    }
  }
  if (is.data.frame(x$flagged) && nrow(x$flagged) > 0) {
    cat("\nChecks that need attention:\n")
    for (i in seq_len(nrow(x$flagged))) {
      cat("  [", x$flagged$status[i], "] ", x$flagged$check[i], ": ", x$flagged$message[i], "\n", sep = "")
    }
  } else if (!is.null(x$flagged)) {
    cat("\nEvery quality check passed or is informational.\n")
  }
  if (length(x$messages) > 0) {
    cat("\nMessages recorded during the run:\n")
    for (i in seq_along(x$messages)) {
      cat("  ", names(x$messages)[i], ": ", trimws(x$messages[i]), "\n", sep = "")
    }
  }
  st <- x$stage_times
  cat("\nWall clock: inspect ", .raw.pipeline.fmt.secs(st[["inspect"]]), ", calibrate ",
      .raw.pipeline.fmt.secs(st[["calibrate"]]), ", epochs ", .raw.pipeline.fmt.secs(st[["getmeta"]]),
      ", wear ", .raw.pipeline.fmt.secs(st[["wear"]]), ", quality ", .raw.pipeline.fmt.secs(st[["quality"]]),
      "; total ", .raw.pipeline.fmt.secs(st[["total"]]), ".\n", sep = "")
  invisible(x)
}
