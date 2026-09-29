# Parity and failure-mode tests for R/raw_pipeline.R (read.raw.accelerometer,
# as.ggir.milestone, print and summary) and R/raw_export.R against the stored GGIR
# part-1 milestone of the MOS2 recording. Reference data live in the folder named by
# CANHRACTI_GGIR_REF; tests skip when a file is missing. The MOS2 recording is read
# end to end once and cached (about 70 s). GGIR wrote the reference outputs in an
# America/Anchorage session, so the file runs in that zone.

withr::local_timezone("America/Anchorage")

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}
skip_if_no_zip <- function() {
  if (!requireNamespace("zip", quietly = TRUE) && !nzchar(Sys.which("zip"))) {
    testthat::skip("neither the zip package nor a zip executable is available to fabricate gt3x archives")
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
TRUNC <- function() ref_file("failmodes", "din", "truncated.gt3x")
SHORT <- function() ref_file("failmodes", "din", "tooshort.gt3x")
MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to

MSG_POSSIBLY <- "recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this"
MSG_NOT_ENOUGH <- "recalibration not done because not enough data in the file or because file is corrupt"
# $imputed is always built, $sleep only when read.raw.accelerometer(sleep = TRUE)
SPEC_NAMES <- c("file", "device", "tz", "params", "status", "calibration", "meta", "wear",
                "imputed", "sleep", "quality", "checks", "versions")

load_stored <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  as.list(e)
}
no_bsc <- function(l) l[setdiff(names(l), "bsc_qc")]

# results cached across the tests of this file
.cache <- new.env(parent = emptyenv())
mos2_run <- function() {
  if (is.null(.cache$x)) {
    events <- list()
    cb <- function(stage, i, n, message) {
      events[[length(events) + 1]] <<- data.frame(stage = stage, i = i, n = n, message = message,
                                                  stringsAsFactors = FALSE)
    }
    t0 <- Sys.time()
    .cache$x <- read.raw.accelerometer(MOS2(), desiredtz = MOS2_TZ, progress = cb)
    .cache$wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    .cache$events <- do.call(rbind, events)
    cat(sprintf("\n[timing] read.raw.accelerometer(MOS2): %.1f s wall clock; stages: %s\n",
                .cache$wall,
                paste(names(.cache$x$status$stage_times),
                      round(.cache$x$status$stage_times, 1), sep = " ", collapse = ", ")),
        file = stderr())
  }
  .cache$x
}

test_that("end to end on MOS2: structure, stage times and progress events", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  x <- mos2_run()
  expect_s3_class(x, "canhrActi_raw")
  expect_identical(names(x), c(SPEC_NAMES, "inspection"))
  expect_false(x$status$corrupt); expect_false(x$status$too_short); expect_false(x$status$skipped)
  st <- x$status$stage_times
  expect_identical(names(st), c("inspect", "calibrate", "getmeta", "wear", "quality",
                                "impute", "sleep", "total"))
  expect_true(all(is.finite(st)) && all(st >= 0))
  expect_true(st[["total"]] >= sum(st[c("inspect", "calibrate", "getmeta", "wear", "quality",
                                        "impute", "sleep")]))
  expect_true(st[["total"]] < 600)
  expect_length(x$status$messages, 0)
  ev <- .cache$events
  expect_identical(ev$stage,
                   c("inspect", rep("calibrate", 5), rep("getmeta", 4), "wear", "quality",
                     "sleep", "part3", rep("sib_detect", 7), rep("part3", 3), "done"))
  expect_identical(ev$i[ev$stage == "sib_detect"], 1:7)
  expect_true(all(ev$n[ev$stage == "sib_detect"] == 7L))
  expect_identical(ev$message[ev$stage == "sib_detect"], paste0("Night ", 1:7, " of 7"))
  expect_identical(ev$i[ev$stage == "calibrate"], 1:5)
  # the fourth getmeta event is the stream reader's count (it serves nothing off read.gt3x 1.2.0)
  expect_identical(ev$i[ev$stage == "getmeta"], c(1:3, NA))
  n_direct <- if (utils::packageVersion("read.gt3x") == "1.2.0") 2 else 0
  expect_identical(ev$message[ev$stage == "getmeta"][4],
                   paste0("gt3x reader: ", n_direct, " of 2 blocks read directly"))
  # denominators are the header-span upper bounds: 595196 s / 43200 -> 14 + 1, / 86400 -> 7 + 1
  expect_true(all(ev$n[ev$stage == "calibrate"] == 15L))
  expect_true(all(ev$n[ev$stage == "getmeta"] == 8L))
  expect_identical(ev$message[ev$stage == "calibrate"], paste0("Loading chunk: ", 1:5))
  expect_match(ev$message[ev$stage == "done"], "^Finished MOS2E39230594.gt3x in [0-9.]+ s$")
  expect_s3_class(x$params, "canhrActi_raw_params")
  expect_null(x$params$progress)
  expect_identical(x$params$desiredtz, MOS2_TZ)
  expect_s3_class(x$inspection, "canhrActi_raw_info")
})

test_that("P12: as.ggir.milestone reproduces the stored M, I and C field by field", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  x <- mos2_run()
  st <- load_stored(MOS2_RDATA())
  ms <- as.ggir.milestone(x)
  expect_identical(names(ms), c("M", "I", "C", "desiredtz_part1", "GGIRversion", "tail_expansion_log"))
  expect_identical(names(ms$M), names(st$M))
  for (nm in setdiff(names(st$M), "bsc_qc")) {
    expect_identical(ms$M[[nm]], st$M[[nm]], label = paste0("M$", nm))
  }
  expect_identical(no_bsc(ms$M), no_bsc(st$M))
  expect_identical(ms$I, st$I)
  expect_identical(names(ms$C), names(st$C))
  for (nm in setdiff(names(st$C), "bsc_qc")) {
    expect_identical(ms$C[[nm]], st$C[[nm]], label = paste0("C$", nm))
  }
  expect_identical(no_bsc(ms$C), no_bsc(st$C))
  # bsc_qc is a wall-clock log: same shape, different times
  expect_identical(dim(ms$M$bsc_qc), dim(st$M$bsc_qc))
  expect_identical(dim(ms$C$bsc_qc), dim(st$C$bsc_qc))
  expect_identical(ms$desiredtz_part1, MOS2_TZ)
  expect_true(is.na(ms$GGIRversion))
  expect_null(ms$tail_expansion_log)
  expect_identical(dim(ms$M$metashort), c(118800L, 3L))
  expect_identical(dim(ms$M$metalong), c(660L, 4L))
  expect_identical(names(ms$M$metashort), c("timestamp", "anglez", "ENMO"))
  expect_identical(ms$M$metashort$timestamp[1], "2025-10-07T20:30:00-0800")
  expect_identical(ms$M$metashort$timestamp[118800], "2025-10-14T17:29:55-0800")
  expect_identical(ms$M$metashort$ENMO[1:6], c(0.0175, 0.0604, 0.0581, 0.0659, 0.0396, 0.0896))
  expect_identical(ms$M$metashort$anglez[1:6], c(48.0663, 2.2517, 7.3921, 29.8733, 10.2377, 29.3146))
  expect_identical(ms$M$wday, 3); expect_identical(ms$M$wdayname, "Tuesday")
  expect_identical(ms$M$windowsizes, c(5, 900, 3600))
  expect_identical(ms$M$QClog$timegaps_n, c(915L, 525L))
  expect_identical(ms$M$QClog$blockLengthSeconds, c(167882, 137906))
  expect_identical(as.integer(table(ms$M$metalong$nonwearscore)), c(308L, 9L, 343L))
  expect_identical(ms$C$cal.error.start, 0.01614); expect_identical(ms$C$cal.error.end, 0.0058)
  expect_identical(ms$C$npoints, 1176L); expect_identical(ms$C$nhoursused, 41)
  expect_identical(ms$C$QCmessage, MSG_POSSIBLY)
  expect_identical(ms$C$scale, c(1.0009963740829368, 0.99502644175360377, 0.99849677326354713))
  expect_identical(ms$C$offset, c(-0.020387751890171227, -0.017829364795671376, 0.0077769254660801793))
  expect_identical(ms$I$monc, 3L); expect_identical(ms$I$dformc, 6L); expect_identical(ms$I$sf, 30)
})

test_that("end to end on MOS2: the file, device, tz, wear and quality blocks", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  x <- mos2_run()
  st <- load_stored(MOS2_RDATA())
  expect_identical(names(x$file), c("path", "filename", "size_bytes", "too_small", "uppercase_extension", "md5"))
  expect_identical(x$file$filename, "MOS2E39230594.gt3x")
  expect_identical(x$file$size_bytes, 22102275)
  expect_false(x$file$too_small); expect_false(x$file$uppercase_extension)
  expect_identical(x$file$md5, unname(tools::md5sum(MOS2())))
  expect_identical(names(x$device), c("brand", "format", "serial", "serial_prefix", "firmware", "device_type",
                                      "sf", "dynrange", "clipthres", "header_timezone", "header_start",
                                      "header_last_sample", "acceleration_scale", "header"))
  expect_identical(x$device$brand, "actigraph"); expect_identical(x$device$format, "gt3x")
  expect_identical(x$device$serial, "MOS2E39230594"); expect_identical(x$device$serial_prefix, "MOS")
  expect_identical(x$device$firmware, "1.9.2"); expect_identical(x$device$device_type, "wGT3XBT")
  expect_identical(x$device$sf, 30); expect_identical(x$device$dynrange, 8); expect_identical(x$device$clipthres, 7.5)
  expect_identical(x$device$header_timezone, "-04:00:00")
  expect_identical(as.numeric(x$device$header_start), 1759868880)
  expect_identical(attr(x$device$header_start, "tzone"), "GMT")
  expect_identical(format(x$device$header_last_sample, "%Y-%m-%d %H:%M:%S"), "2025-10-14 17:47:56")
  expect_identical(x$device$acceleration_scale, 256)
  expect_identical(x$device$header, st$I$header)
  expect_identical(names(x$tz), c("desiredtz", "configtz", "machine_tz", "effective_tz"))
  expect_identical(x$tz$desiredtz, MOS2_TZ); expect_null(x$tz$configtz)
  expect_identical(x$tz$machine_tz, Sys.timezone()); expect_identical(x$tz$effective_tz, MOS2_TZ)
  # meta keeps GGIR's timestamp and adds the numeric time
  expect_s3_class(x$meta, "canhrActi_raw_meta")
  expect_identical(names(x$meta$metashort), c("timestamp", "time", "anglez", "ENMO"))
  expect_identical(x$meta$metashort$time[1], as.numeric(as.POSIXct("2025-10-07 20:30:00", tz = MOS2_TZ)))
  expect_identical(nrow(x$meta$chunks), 3L)
  expect_identical(x$meta$chunks$rows_read, c(2592000, 1837110, 0))
  expect_identical(x$meta$rows_discarded_end, 17160)
  expect_s3_class(x$calibration, "canhrActi_raw_calibration")
  expect_true(x$calibration$applied); expect_identical(x$calibration$source, "fit")
  expect_identical(x$calibration$chunks$still_windows, c(346L, 757L, 1100L, 1176L, 1176L))
  expect_s3_class(x$wear, "canhrActi_raw_wear")
  expect_identical(x$wear$status, "ok")
  expect_identical(unname(colSums(x$wear$rout)), c(343, 0, 23, 0, 366))
  expect_identical(x$wear$wear_dur_def_proto_day, 3.0625)
  expect_identical(x$wear$meas_dur_dys, 6.875)
  expect_identical(x$wear$n_valid_days, 3L); expect_identical(x$wear$n_valid_weekend_days, 0L)
  expect_identical(x$wear$daily$n_hours, c(3.5, 24, 24, 24, 24, 24, 24, 17.5))
  expect_identical(x$wear$daily$n_valid_hours, c(3.5, 22.75, 24, 23.25, 0, 0, 0, 0))
  expect_identical(nrow(x$quality), 1L)
  expect_identical(x$quality$filename, "MOS2E39230594.gt3x")
  expect_false(x$quality$file.corrupt); expect_false(x$quality$file.too.short)
  expect_identical(x$quality$filehealth_totimp_min, 7451.65)
  expect_identical(x$quality$filehealth_totimp_N, 1440)
  expect_identical(x$quality$cal.error.start, 0.01614); expect_identical(x$quality$cal.error.end, 0.0058)
  expect_identical(x$quality$n.10sec.windows, 1176L)
  expect_identical(x$quality$wear_dur_def_proto_day, 3.0625)
  expect_identical(x$quality$seconds_trimmed_start, 116)
  expect_identical(x$quality$samples_discarded_end, 17160)
  expect_identical(x$quality$zero_triplets_removed, 0)
  expect_identical(x$quality$gap_count_over_90min, 15L)
  expect_identical(x$quality$minutes_epoch_level_imputed, 4815)
  expect_identical(x$quality$desiredtz_used, MOS2_TZ)
  expect_identical(nrow(x$checks), 25L)
  expect_identical(x$checks$id, 1:25)
  expect_true(all(x$checks$status %in% c("ok", "warn", "fail", "info")))
  expect_identical(x$checks$status[x$checks$check == "calibration_status"], "warn")
  expect_identical(names(x$versions), c("canhrActi", "read.gt3x", "GGIRread", "R", "run_time"))
  expect_identical(x$versions$read.gt3x, as.character(utils::packageVersion("read.gt3x")))
  expect_s3_class(x$versions$run_time, "POSIXct")
})

test_that("print and summary write plain prose for a complete recording", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  x <- mos2_run()
  # the S3 methods are called directly; under load_all print() may fall through to print.default
  out <- capture.output(r <- print.canhrActi_raw(x))
  expect_identical(r, x)
  expect_true(any(grepl("MOS2E39230594.gt3x (22.1 MB)", out, fixed = TRUE)))
  expect_true(any(grepl("actigraph gt3x, serial MOS2E39230594, firmware 1.9.2, 30 Hz", out, fixed = TRUE)))
  expect_true(any(grepl("118800 short (5 s) and 660 long (900 s)", out, fixed = TRUE)))
  expect_true(any(grepl("calibration: applied;", out, fixed = TRUE)))
  expect_true(any(grepl("3.06 of 6.88 days worn; 3 valid days (3 weekdays, 0 weekend days)", out, fixed = TRUE)))
  expect_false(any(grepl("\u2014", out)))
  s <- summary.canhrActi_raw(x)
  expect_s3_class(s, "summary.canhrActi_raw")
  expect_identical(s$epochs$n_short, 118800L)
  expect_identical(s$wear$n_valid_days, 3L)
  expect_identical(nrow(s$days), 8L)
  out2 <- capture.output(print.summary.canhrActi_raw(s))
  expect_true(any(grepl("The auto-calibration used 41 hours of data and 1176 still windows", out2, fixed = TRUE)))
  expect_true(any(grepl("16.1 mg to 5.8 mg", out2, fixed = TRUE)))
  expect_true(any(grepl("The tables hold 118800 short epochs of 5 s and 660 long epochs of 900 s", out2, fixed = TRUE)))
  gap_min <- sum(x$meta$qclog$timegaps_min)
  expect_equal(gap_min, 7451.65)
  expect_true(any(grepl(paste0("1440 time gaps totalling ", round(gap_min, 1), " minutes"), out2, fixed = TRUE)))
  expect_true(any(grepl("343 long epochs as non-wear, 23 more as implausible wear and 0 as clipped", out2, fixed = TRUE)))
  expect_true(any(grepl("Checks that need attention:", out2, fixed = TRUE)))
  expect_false(any(grepl("\u2014", out2)))
})

test_that("write.ggir.milestone then load() gives the stored objects under GGIR's names", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  x <- mos2_run()
  st <- load_stored(MOS2_RDATA())
  out <- file.path(tempdir(), "canhrActi_ms", "meta", "basic")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  f <- write.ggir.milestone(x, out)
  expect_identical(basename(f), "meta_MOS2E39230594.gt3x.RData")
  expect_true(file.exists(f))
  e <- new.env(parent = emptyenv())
  nms <- load(f, envir = e)
  expect_identical(nms, c("M", "I", "C", "filename_dir", "filefoldername", "tail_expansion_log",
                          "GGIRversion", "desiredtz_part1"))
  expect_identical(no_bsc(e$M), no_bsc(st$M))
  expect_identical(e$I, st$I)
  expect_identical(no_bsc(e$C), no_bsc(st$C))
  expect_identical(e$filename_dir, "MOS2E39230594.gt3x")
  expect_identical(e$filefoldername, "MOS2E39230594.gt3x")
  expect_null(e$tail_expansion_log)
  expect_true(is.na(e$GGIRversion))
  expect_identical(e$desiredtz_part1, MOS2_TZ)
  # a full file path and a milestone list are accepted too
  f2 <- write.ggir.milestone(as.ggir.milestone(x), file.path(tempdir(), "canhrActi_ms2", "custom_name.RData"))
  expect_true(file.exists(f2))
  e2 <- new.env(parent = emptyenv()); load(f2, envir = e2)
  expect_identical(e2$M, e$M); expect_identical(e2$C, e$C)
  y <- read.ggir.milestone(f)
  my <- as.ggir.milestone(y)
  expect_identical(my$M, e$M); expect_identical(my$I, e$I); expect_identical(my$C, e$C)
  expect_identical(my$desiredtz_part1, MOS2_TZ)
  expect_identical(y$file$milestone, gsub("\\\\", "/", f))
  expect_error(write.ggir.milestone(list(a = 1), out), "canhrActi_raw object")
  expect_error(write.ggir.milestone(x, ""), "directory or a file path")
})

test_that("read.ggir.milestone on the stored milestone recomputes the wear decision and quality of the live run", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  x <- mos2_run()
  st <- load_stored(MOS2_RDATA())
  y <- read.ggir.milestone(MOS2_RDATA())
  expect_s3_class(y, "canhrActi_raw")
  expect_identical(names(y), c(SPEC_NAMES, "inspection"))
  expect_false(y$status$corrupt); expect_false(y$status$too_short)
  # the stored objects come back unchanged, bsc_qc included
  my <- as.ggir.milestone(y)
  expect_identical(my$M, st$M); expect_identical(my$I, st$I); expect_identical(my$C, st$C)
  expect_identical(my$desiredtz_part1, "")  # the stored run used "" on this machine
  expect_identical(y$tz$desiredtz, "")
  expect_identical(y$tz$effective_tz, Sys.timezone())
  expect_identical(names(y$meta$metashort), c("timestamp", "time", "anglez", "ENMO"))
  expect_identical(y$meta$metashort$time, x$meta$metashort$time)
  expect_identical(y$meta$metalong$time, x$meta$metalong$time)
  expect_identical(as.numeric(y$meta$starttime), as.numeric(x$meta$starttime))
  expect_null(y$meta$chunks)
  expect_true(y$calibration$applied); expect_false(y$calibration$reset)
  expect_identical(y$calibration$source, "fit"); expect_identical(y$calibration$supplied_from, "ggir_milestone")
  expect_identical(y$calibration$scale, st$C$scale)
  expect_identical(y$wear$rout, x$wear$rout)
  expect_identical(y$wear$r5long, x$wear$r5long)
  expect_identical(y$wear$daily, x$wear$daily)
  expect_identical(y$wear$wear_dur_def_proto_day, x$wear$wear_dur_def_proto_day)
  expect_identical(y$wear$n_valid_days, x$wear$n_valid_days)
  # every quality column except the three that need the block traces
  trace_only <- c("seconds_trimmed_start", "samples_discarded_end", "zero_triplets_removed")
  expect_identical(names(y$quality), names(x$quality))
  for (nm in setdiff(names(x$quality), trace_only)) {
    expect_identical(y$quality[[nm]], x$quality[[nm]], label = paste0("quality$", nm))
  }
  expect_true(all(is.na(unlist(y$quality[trace_only]))))
  expect_identical(y$quality$seconds_trimmed_end, x$quality$seconds_trimmed_end)
  expect_identical(y$quality$gap_count_over_90min, 15L)
  # checks: all but the five that read the path, the size or the traces
  path_or_trace <- c(1L, 2L, 8L, 9L, 10L)
  keep <- !(x$checks$id %in% path_or_trace)
  expect_identical(y$checks$status[keep], x$checks$status[keep])
  expect_identical(y$checks$message[keep], x$checks$message[keep])
  expect_identical(y$checks$message[2], "No path recorded (GGIR milestone input).")
  expect_true(is.na(y$file$path)); expect_true(is.na(y$file$md5))
  expect_identical(y$file$milestone, MOS2_RDATA())
  expect_identical(y$device$header, st$I$header)
  expect_identical(as.numeric(y$device$header_start), 1759868880)
  expect_true(any(grepl("Loaded from GGIR milestone", y$status$messages, fixed = TRUE)))
  expect_true(any(grepl("GGIRversion 3.3.6", y$status$messages, fixed = TRUE)))
  # a supplied desiredtz is overridden by desiredtz_part1, as g.part2 does
  y2 <- read.ggir.milestone(MOS2_RDATA(), params = raw.params(desiredtz = "UTC"))
  expect_identical(y2$tz$desiredtz, "")
  expect_identical(y2$wear$rout, x$wear$rout)
  expect_error(read.ggir.milestone(file.path(tempdir(), "nope.RData")), "Milestone file not found")
})

test_that("F1: a truncated gt3x is a corrupt state with GGIR's two warning texts", {
  skip_if_no_ggir_ref(); skip_if_no_file(TRUNC())
  z <- read.raw.accelerometer(TRUNC(), desiredtz = MOS2_TZ)
  expect_s3_class(z, "canhrActi_raw")
  expect_true(z$status$corrupt); expect_false(z$status$too_short); expect_false(z$status$skipped)
  expect_true(is.na(z$device$sf))
  expect_null(z$inspection$sf)
  expect_true(any(grepl("File info could not be extracted", z$status$messages, fixed = TRUE)))
  expect_true(any(grepl("Sample frequency not recognised", z$status$messages, fixed = TRUE)))
  expect_identical(unname(names(z$status$messages)[1]), "inspect")
  expect_identical(z$calibration$source, "not_attempted")
  expect_false(z$calibration$attempted)
  expect_null(z$meta$metashort); expect_null(z$meta$metalong)
  expect_true(z$meta$filecorrupt)
  expect_identical(z$wear$status, "no_data")
  expect_true(z$quality$file.corrupt); expect_false(z$quality$file.too.short)
  expect_identical(z$quality$cal.error.start, 0)
  expect_identical(z$quality$n.10sec.windows, 0)
  expect_identical(z$quality$QCmessage, "Autocalibration not done")
  expect_identical(nrow(z$checks), 25L)
  expect_identical(z$checks$status[z$checks$check == "header"], "fail")
  expect_true(z$file$too_small)
  expect_identical(z$file$size_bytes, 5000)
  ms <- as.ggir.milestone(z)
  expect_true(ms$M$filecorrupt); expect_false(ms$M$filetooshort)
  expect_null(ms$M$metashort); expect_null(ms$M$wday); expect_null(ms$M$QClog)
  expect_identical(names(ms$C), c("cal.error.end", "cal.error.start", "scale", "offset", "tempoffset",
                                  "QCmessage", "npoints", "nhoursused", "use.temp"))
  expect_identical(ms$C$scale, c(1, 1, 1))
  expect_identical(ms$I$monc, 3L); expect_null(ms$I$sf)
  out <- capture.output(print.canhrActi_raw(z))
  expect_true(any(grepl("status:      corrupt; no epochs derived", out, fixed = TRUE)))
  out2 <- capture.output(print.summary.canhrActi_raw(summary.canhrActi_raw(z)))
  expect_true(any(grepl("The file is corrupt", out2, fixed = TRUE)))
  # a corrupt milestone round trip keeps the corrupt shape
  f <- write.ggir.milestone(z, file.path(tempdir(), "canhrActi_ms_trunc"))
  z2 <- read.ggir.milestone(f)
  expect_true(z2$status$corrupt)
  expect_identical(as.ggir.milestone(z2)$M, ms$M)
  expect_identical(as.ggir.milestone(z2)$C, ms$C)
})

test_that("F2: a 330 s recording is a too-short state, and is skipped on request", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  s <- read.raw.accelerometer(SHORT(), desiredtz = MOS2_TZ)
  expect_false(s$status$corrupt); expect_true(s$status$too_short); expect_false(s$status$skipped)
  expect_identical(s$device$sf, 100)
  expect_identical(s$device$brand, "actigraph"); expect_identical(s$device$serial_prefix, "TAS")
  expect_identical(s$calibration$qcmessage, MSG_NOT_ENOUGH)
  expect_true(s$calibration$attempted); expect_false(s$calibration$applied)
  # GGIR's reader drops a first block under the 2 h floor before any trace sees it
  expect_identical(s$calibration$chunks$rows_read[1], 0)
  expect_identical(nrow(s$calibration$chunks), 1L)
  expect_true(s$calibration$filequality$filetooshort)
  expect_false(s$calibration$filequality$filecorrupt)
  expect_null(s$meta$metashort); expect_true(s$meta$filetooshort); expect_false(s$meta$filecorrupt)
  expect_identical(s$wear$status, "no_data")
  expect_true(s$quality$file.too.short); expect_false(s$quality$file.corrupt)
  expect_identical(s$quality$samplefreq, 100)
  expect_identical(s$quality$cal.error.end, " ")
  expect_true(s$file$too_small)
  expect_identical(s$checks$status[s$checks$check == "duration_floor"], "fail")
  ms <- as.ggir.milestone(s)
  expect_false(ms$M$filecorrupt); expect_true(ms$M$filetooshort)
  expect_identical(ms$C$QCmessage, MSG_NOT_ENOUGH)
  out <- capture.output(print.canhrActi_raw(s))
  expect_true(any(grepl("too short", out, fixed = TRUE)))
  # skip_small_files: nothing is read
  s2 <- read.raw.accelerometer(SHORT(), desiredtz = MOS2_TZ, skip_small_files = TRUE)
  expect_true(s2$status$skipped); expect_true(s2$status$too_short); expect_false(s2$status$corrupt)
  expect_true(any(grepl("Skipping files that are too small for analysis: tooshort.gt3x", s2$status$messages, fixed = TRUE)))
  expect_identical(s2$calibration$source, "not_attempted")
  expect_true(s2$meta$skipped)
  expect_true(s2$status$stage_times[["calibrate"]] < 5)
  expect_identical(s2$checks$status[s2$checks$check == "file_size"], "fail")
})

test_that("F3: an uppercase .GT3X copy gives identical tables and leaves the source untouched", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  x <- mos2_run()
  dir_up <- file.path(tempdir(), "canhrActi_upper")
  dir.create(dir_up, showWarnings = FALSE)
  up <- file.path(dir_up, "MOS2_UPPER.GT3X")
  expect_true(file.copy(MOS2(), up, overwrite = TRUE))
  md5_before <- unname(tools::md5sum(up))
  mtime_before <- file.info(up)$mtime
  listing_before <- list.files(dir_up)
  t0 <- Sys.time()
  u <- read.raw.accelerometer(up, desiredtz = MOS2_TZ)
  cat(sprintf("\n[timing] read.raw.accelerometer(MOS2_UPPER.GT3X): %.1f s\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))), file = stderr())
  expect_true(file.exists(up))
  expect_identical(list.files(dir_up), listing_before)
  expect_identical(list.files(dir_up), "MOS2_UPPER.GT3X")
  expect_identical(unname(tools::md5sum(up)), md5_before)
  expect_identical(file.info(up)$mtime, mtime_before)
  expect_true(u$file$uppercase_extension)
  expect_identical(u$file$filename, "MOS2_UPPER.GT3X")
  expect_identical(u$file$md5, x$file$md5)
  expect_false(u$inspection$renamed)
  expect_true(grepl("\\.gt3x$", u$inspection$read_path))
  expect_false(identical(u$inspection$read_path, gsub("\\\\", "/", up)))
  mu <- as.ggir.milestone(u); mx <- as.ggir.milestone(x)
  expect_identical(mu$M$metashort, mx$M$metashort)
  expect_identical(mu$M$metalong, mx$M$metalong)
  expect_identical(mu$M$QClog, mx$M$QClog)
  expect_identical(mu$M$wday, mx$M$wday)
  expect_identical(no_bsc(mu$C), no_bsc(mx$C))
  expect_identical(mu$I$header, mx$I$header)
  expect_identical(mu$I$filename, "MOS2_UPPER.GT3X")
  expect_identical(u$wear$rout, x$wear$rout)
  expect_identical(u$wear$daily, x$wear$daily)
  trace_or_name <- c("filename", "too_small", "uppercase_extension")
  for (nm in setdiff(names(x$quality), trace_or_name)) {
    expect_identical(u$quality[[nm]], x$quality[[nm]], label = paste0("quality$", nm))
  }
  expect_true(u$quality$uppercase_extension)
  expect_identical(u$checks$status[u$checks$check == "format"], "info")
  expect_true(any(grepl("temporary lowercase stand-in", u$status$messages, fixed = TRUE)))
})

test_that("F4: a missing path, a directory and an unrecognised extension are errors that name the file", {
  missing <- "C:/no/such/folder/recording.gt3x"
  err <- tryCatch(read.raw.accelerometer(missing), error = function(e) e)
  expect_s3_class(err, "canhrActi_raw_pipeline_error")
  expect_true(grepl(missing, conditionMessage(err), fixed = TRUE))
  expect_identical(err$stage, "inspect")
  d <- file.path(tempdir(), "canhrActi_dir.csv_exports")
  dir.create(d, showWarnings = FALSE)
  err2 <- tryCatch(read.raw.accelerometer(d), error = function(e) e)
  expect_s3_class(err2, "canhrActi_raw_pipeline_error")
  expect_true(grepl("is a directory", conditionMessage(err2), fixed = TRUE))
  expect_true(grepl(d, conditionMessage(err2), fixed = TRUE))
  # a file GGIR's extension switch rejects
  txt <- file.path(tempdir(), "notes.txt")
  writeLines("not an accelerometer file", txt)
  err3 <- tryCatch(read.raw.accelerometer(txt), error = function(e) e)
  expect_s3_class(err3, "canhrActi_raw_pipeline_error")
  expect_true(grepl("unrecognised file format", conditionMessage(err3), fixed = TRUE))
  expect_true(grepl("notes.txt", conditionMessage(err3), fixed = TRUE))
  expect_identical(err3$stage, "inspect")
  expect_s3_class(err3$parent, "canhrActi_raw_inspect_error")
  # raw.discover on an empty directory and on one with other files
  e1 <- file.path(tempdir(), "canhrActi_empty"); dir.create(e1, showWarnings = FALSE)
  d1 <- raw.discover(e1)
  expect_identical(nrow(d1), 0L)
  expect_true(grepl("no accelerometer files found", attr(d1, "message"), fixed = TRUE))
  e2 <- file.path(tempdir(), "canhrActi_other"); dir.create(e2, showWarnings = FALSE)
  writeLines("x", file.path(e2, "a.agd")); writeLines("x", file.path(e2, "b.txt"))
  d2 <- raw.discover(e2)
  expect_identical(nrow(d2), 2L)
  expect_false(any(d2$recognised))
  expect_true(grepl("no accelerometer files found", attr(d2, "message"), fixed = TRUE))
  expect_error(read.raw.accelerometer(c("a", "b")), "single file path")
  expect_error(read.raw.accelerometer(1), "single file path")
})

test_that("parameter handling: NULL, a list, overrides, unknown names, the progress argument", {
  skip_if_no_ggir_ref(); skip_if_no_file(TRUNC())
  z1 <- read.raw.accelerometer(TRUNC(), params = NULL)
  expect_identical(z1$params$desiredtz, "")
  z2 <- read.raw.accelerometer(TRUNC(), params = list(desiredtz = "UTC", minloadcrit = 1))
  expect_identical(z2$params$desiredtz, "UTC"); expect_identical(z2$params$minloadcrit, 1)
  z3 <- read.raw.accelerometer(TRUNC(), params = raw.params(desiredtz = "UTC"), minloadcrit = 2)
  expect_identical(z3$params$desiredtz, "UTC"); expect_identical(z3$params$minloadcrit, 2)
  expect_identical(z3$tz$effective_tz, "UTC")
  expect_error(read.raw.accelerometer(TRUNC(), nosuchparam = 1), "unknown to raw.params")
  expect_error(read.raw.accelerometer(TRUNC(), params = list(nosuchparam = 1)), "Unknown parameter")
  expect_error(read.raw.accelerometer(TRUNC(), progress = "yes"), "progress must be NULL or a function")
  # a callback stored in params is used and then removed from the stored object
  seen <- character()
  p <- raw.params(progress = function(stage, i, n, message) seen <<- c(seen, stage))
  z4 <- read.raw.accelerometer(TRUNC(), params = p)
  # a corrupt file still reaches the sleep stage, which returns its own empty state
  expect_identical(seen, c("inspect", "wear", "quality", "sleep", "done"))
  expect_null(z4$params$progress)
})

test_that("F8: a corrupt log.bin behind a valid info.txt", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_zip()
  work <- file.path(tempdir(), "canhrActi_f8")
  unlink(work, recursive = TRUE); dir.create(work)
  ex <- file.path(work, "ex")
  utils::unzip(MOS2(), exdir = ex)
  expect_true(all(file.exists(file.path(ex, c("info.txt", "log.bin")))))
  full <- readBin(file.path(ex, "log.bin"), "raw", file.size(file.path(ex, "log.bin")))
  make_gt3x <- function(name, log_bytes) {
    d <- file.path(work, name); dir.create(d, showWarnings = FALSE)
    file.copy(file.path(ex, "info.txt"), d, overwrite = TRUE)
    if (!is.null(log_bytes)) writeBin(log_bytes, file.path(d, "log.bin"))
    out <- file.path(work, paste0(name, ".gt3x"))
    files <- if (is.null(log_bytes)) "info.txt" else c("info.txt", "log.bin")
    old <- setwd(d); on.exit(setwd(old), add = TRUE)
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::zip(out, files = files, mode = "cherry-pick")
    } else {
      utils::zip(out, files = files, flags = "-q")
    }
    out
  }
  # (a) log.bin truncated to 5000 bytes: 840 records survive and the 2 h floor makes it too short
  f_trunc <- make_gt3x("trunc5000", full[1:5000])
  t1 <- read.raw.accelerometer(f_trunc, desiredtz = MOS2_TZ)
  expect_false(t1$status$corrupt); expect_true(t1$status$too_short)
  expect_identical(t1$device$sf, 30)
  expect_identical(t1$calibration$qcmessage, MSG_NOT_ENOUGH)
  expect_identical(t1$calibration$chunks$rows_read[1], 0)   # discarded by the 2 h floor, not traced
  expect_true(t1$calibration$filequality$filetooshort); expect_false(t1$calibration$filequality$filecorrupt)
  expect_true(t1$quality$file.too.short); expect_false(t1$quality$file.corrupt)
  expect_identical(nrow(read.gt3x::read.gt3x(f_trunc, batch_begin = 1, batch_end = 43200, asDataFrame = TRUE)), 840L)
  # (b) a garbage log.bin: GGIR's decimal-separator probe meets the read.gt3x failure and stops
  set.seed(1)
  f_garbage <- make_gt3x("garbage", as.raw(sample(0:255, 200000, TRUE)))
  err <- tryCatch(read.raw.accelerometer(f_garbage, desiredtz = MOS2_TZ), error = function(e) e)
  expect_s3_class(err, "canhrActi_raw_pipeline_error")
  expect_true(grepl("Problem with reading .gt3x file in GGIR function dotorcomma", conditionMessage(err), fixed = TRUE))
  expect_true(grepl("garbage.gt3x", conditionMessage(err), fixed = TRUE))
  expect_identical(err$stage, "inspect")
  # (c) no log.bin at all: the same GGIR stop
  f_nolog <- make_gt3x("nolog", NULL)
  err2 <- tryCatch(read.raw.accelerometer(f_nolog, desiredtz = MOS2_TZ), error = function(e) e)
  expect_s3_class(err2, "canhrActi_raw_pipeline_error")
  expect_true(grepl("dotorcomma", conditionMessage(err2), fixed = TRUE))
  # (d) a non-EOF read.gt3x error in the block reader ends in GGIR's corrupt state
  info <- raw.inspect(MOS2(), desiredtz = MOS2_TZ)
  info$read_path <- f_nolog
  cal <- raw.calibrate(info)
  expect_false(cal$attempted && cal$applied)
  expect_true(any(grepl("read.gt3x::read.gt3x failed on block 1", cal$messages, fixed = TRUE)))
  expect_true(any(grepl("File empty, possibly corrupt", cal$messages, fixed = TRUE)))
  expect_true(cal$filequality$filecorrupt)
  meta <- raw.getmeta(info, cal)
  expect_true(meta$filecorrupt); expect_true(meta$filetooshort)
  expect_true(any(grepl("read.gt3x::read.gt3x failed on block 1", meta$messages, fixed = TRUE)))
  unlink(work, recursive = TRUE)
})

test_that("read.raw.accelerometer attaches the imputed series and part 3", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  x <- mos2_run()

  expect_s3_class(x$imputed, "canhrActi_raw_imputed")
  expect_identical(x$imputed$metashort, raw.impute(x$meta, x$wear, x$params)$metashort)
  ms2_path <- ref_file("out", "output_din", "meta", "ms2.out", "MOS2E39230594.gt3x.RData")
  skip_if_no_file(ms2_path)
  expect_identical(x$imputed$metashort, load_stored(ms2_path)$IMP$metashort)

  expect_s3_class(x$sleep, "canhrActi_raw_sleep")
  ms3_path <- ref_file("out", "output_din", "meta", "ms3.out", "MOS2E39230594.gt3x.RData")
  skip_if_no_file(ms3_path)
  ms3 <- load_stored(ms3_path)
  got <- as.ggir.milestone(x, part = 3)
  for (nm in c("sib.cla.sum", "L5list", "SPTE_start", "SPTE_end", "tib.threshold",
               "part3_guider", "SleepRegularityIndex", "ID", "rec_starttime",
               "longitudinal_axis", "SPTE_corrected")) {
    expect_identical(got[[nm]], ms3[[nm]], info = nm)
  }
  # desiredtz_part1 records the string that was passed; the stored run passed ""
  expect_identical(ms3$desiredtz_part1, "")
  expect_identical(got$desiredtz_part1, MOS2_TZ)
  expect_identical(dim(x$sleep$sib.cla.sum), c(111L, 9L))
  expect_true(is.na(x$sleep$SPTE_start[1]))        # the partial first day
  expect_identical(x$sleep$tib.threshold[1], 0.13) # while tib.threshold is not NA
})

test_that("sleep = FALSE skips part 3 but still builds the imputed series", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  x <- read.raw.accelerometer(MOS2(), desiredtz = MOS2_TZ, sleep = FALSE)
  expect_s3_class(x$imputed, "canhrActi_raw_imputed")
  expect_null(x$sleep)
  expect_true(is.na(x$status$stage_times[["sleep"]]))
  expect_true(is.finite(x$status$stage_times[["impute"]]))
  # the member set is the same either way
  expect_identical(names(x), c(SPEC_NAMES, "inspection"))
  x$sleep <- raw.sleep.part3(x)
  expect_identical(as.ggir.milestone(x, part = 3),
                   as.ggir.milestone(mos2_run(), part = 3))
})

test_that("a failure inside part 3 is a message, not a failed read", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  skip_if_not(utils::packageVersion("testthat") >= "3.2.0", "needs with_mocked_bindings")
  testthat::with_mocked_bindings(
    raw.sleep.part3 = function(...) stop("synthetic part 3 failure"),
    .package = "canhrActi",
    code = {
      x <- read.raw.accelerometer(MOS2(), desiredtz = MOS2_TZ)
      expect_null(x$sleep)
      expect_s3_class(x$imputed, "canhrActi_raw_imputed")   # the earlier stages survive
      expect_true("sleep" %in% names(x$status$messages))
      expect_match(x$status$messages[["sleep"]], "synthetic part 3 failure")
      expect_s3_class(x$quality, "data.frame")              # and the QC row is still there
    }
  )
})
