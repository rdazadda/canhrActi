# raw.getmeta, as.ggir.M and the helpers of R/raw_getmeta.R against GGIR's g.getmeta and the
# tail expansion of g.part1. Reference data come from CANHRACTI_GGIR_REF (the MOS2 and EE
# recordings with their stored milestones, the failmodes files, the study's synth_temp.csv and
# the GGIRread test files); tests skip when a file is missing and live comparisons when GGIR
# is not installed. Every parity assertion is identical(). The whole-file MOS2 runs take about
# 45 s each; the EE file (100 Hz, 7 days) runs only when CANHRACTI_LONG_TESTS is set, and the
# live GGIR run on MOS2 only when CANHRACTI_GGIR_SLOW is set.

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}
skip_unless_long <- function() {
  if (!canhr_flag("CANHRACTI_LONG_TESTS")) {
    testthat::skip("long-running test; set CANHRACTI_LONG_TESTS=1 to run it")
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
study_file <- function(...) file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study", ...)
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
MOS2_QC_CSV <- function() ref_file("out", "output_din", "results", "QC", "data_quality_report.csv")
EE <- function() ref_file("EE_left_29.5.2017-05-30.gt3x")
EE_RDATA <- function() ref_file("timing_out", "output_timing", "meta", "basic", "meta_EE_left_29.5.2017-05-30.gt3x.RData")
TRUNC <- function() ref_file("failmodes", "din", "truncated.gt3x")
SHORT <- function() ref_file("failmodes", "din", "tooshort.gt3x")
ggirread_tf <- function() {
  clone <- file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIRread", "inst", "testfiles")
  if (dir.exists(clone)) return(clone)
  system.file("testfiles", package = "GGIRread")
}

MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to
EE_TZ <- "Europe/Helsinki"
OTHER_TZ <- "Europe/London"      # what the study used for the GGIRread test files

memo <- new.env()

stored_mos2 <- function() {
  if (is.null(memo$stored)) {
    skip_if_no_file(MOS2_RDATA())
    e <- new.env(); load(MOS2_RDATA(), envir = e)
    memo$stored <- list(C = e$C, M = e$M, I = e$I)
  }
  memo$stored
}
mos2_info <- function() {
  if (is.null(memo$info)) {
    skip_if_no_file(MOS2())
    memo$info <- raw.inspect(MOS2(), desiredtz = MOS2_TZ)
  }
  memo$info
}
# whole file with the stored calibration
mos2_meta_stored <- function() {
  if (is.null(memo$meta_stored)) {
    memo$meta_stored <- raw.getmeta(mos2_info(), calibration = stored_mos2()$C)
  }
  memo$meta_stored
}
# whole file with raw.calibrate's calibration
mos2_meta_fit <- function() {
  if (is.null(memo$meta_fit)) {
    memo$cal <- raw.calibrate(mos2_info())
    memo$meta_fit <- raw.getmeta(mos2_info(), calibration = memo$cal)
  }
  memo$meta_fit
}
# chunk 1 only (daylimit = 1)
mos2_meta_day1 <- function() {
  if (is.null(memo$meta_day1)) {
    memo$meta_day1 <- raw.getmeta(mos2_info(), calibration = stored_mos2()$C, daylimit = 1)
  }
  memo$meta_day1
}
# the chunk-1 intermediate before calibration: block read, gap imputation, start alignment,
# whole-window cut
mos2_block1 <- function() {
  if (is.null(memo$block1)) {
    info <- mos2_info()
    b1 <- .raw.read.block(info, blocksize = info$blocksize_getmeta, blocknumber = 1,
                          previous_end_page = c(), ws = 3600, params = info$params)
    P <- .raw.impute.timegaps(b1$data, sf = info$sf, k = 0.25, previous_last_value = c(0, 0, 1),
                              previous_last_time = NULL, epochsize = c(5, 900))
    rows_imputed <- nrow(P$x)
    d <- as.matrix(P$x, rownames.force = FALSE)
    SW <- .raw.starttime.truncate(d, mon = info$monc, dformat = info$dformc, desiredtz = MOS2_TZ,
                                  configtz = NULL, ws2 = 900, sf = info$sf, datafile = info$path)
    d <- SW$data
    LD <- nrow(d)
    use <- (floor(LD / (900 * info$sf))) * (900 * info$sf)
    d <- d[1:use, ]
    d <- d[, colnames(d) != "remaining_epochs"]
    memo$block1 <- list(data = d, rows_read = nrow(b1$data), rows_imputed = rows_imputed,
                        dropped = rows_imputed - LD, LD = LD, use = use, starttime = SW$starttime,
                        wday = SW$wday, wdayname = SW$wdayname)
  }
  memo$block1
}

# GGIR parameter objects as the study built them: load_params defaults with desiredtz set
ggir_params <- function(desiredtz, rawdata = list(), metrics = list(), cleaning = list()) {
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- desiredtz
  for (nm in names(rawdata)) P$params_rawdata[nm] <- list(rawdata[[nm]])
  for (nm in names(metrics)) P$params_metrics[nm] <- list(metrics[[nm]])
  for (nm in names(cleaning)) P$params_cleaning[nm] <- list(cleaning[[nm]])
  P
}
ggir_inspect <- function(file, P) {
  suppressWarnings(GGIR::g.inspectfile(file, desiredtz = P$params_general[["desiredtz"]],
                                       params_rawdata = P$params_rawdata,
                                       configtz = P$params_general[["configtz"]]))
}
# live g.getmeta with a C list, as g.part1 calls it
ggir_getmeta <- function(file, P, I, C = NULL, daylimit = FALSE) {
  if (is.null(C)) C <- list(offset = c(0, 0, 0), scale = c(1, 1, 1), tempoffset = c(0, 0, 0), meantempcal = c())
  suppressWarnings(GGIR::g.getmeta(file, params_metrics = P$params_metrics, params_rawdata = P$params_rawdata,
                                   params_general = P$params_general, params_cleaning = P$params_cleaning,
                                   daylimit = daylimit, tempoffset = C$tempoffset, scale = C$scale,
                                   offset = C$offset, meantempcal = C$meantempcal, myfun = c(),
                                   inspectfileobject = I, verbose = FALSE))
}
# GGIR's identity-reset rule from g.part1, applied to a live g.calibrate result
ggir_rule <- function(C) {
  cal.error.end <- C$cal.error.end; cal.error.start <- C$cal.error.start
  if (length(cal.error.start) == 0) cal.error.start <- NA
  if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
    C$scale <- c(1,1,1); C$offset <- c(0,0,0); C$tempoffset <- c(0,0,0)
  } else if (cal.error.start < cal.error.end) {
    C$scale <- c(1,1,1); C$offset <- c(0,0,0); C$tempoffset <- c(0,0,0)
  }
  C
}
mos2_meta_live <- function() {
  if (is.null(memo$meta_live)) {
    skip_if_no_ggir()
    P <- ggir_params(MOS2_TZ)
    I <- ggir_inspect(MOS2(), P)
    memo$meta_live <- ggir_getmeta(MOS2(), P, I, stored_mos2()$C)
  }
  memo$meta_live
}

drop_bsc <- function(M) M[names(M) != "bsc_qc"]

# first differing position of two vectors or data.frames, for failure messages
first_diff <- function(a, b) {
  if (is.null(a) || is.null(b)) return(paste0("one side is NULL: ours ", is.null(a), " ref ", is.null(b)))
  if (is.data.frame(a) && is.data.frame(b)) {
    if (!identical(dim(a), dim(b))) return(paste0("dim ours ", paste(dim(a), collapse = "x"), " ref ", paste(dim(b), collapse = "x")))
    if (!identical(names(a), names(b))) return(paste0("names ours ", paste(names(a), collapse = ","), " ref ", paste(names(b), collapse = ",")))
    for (nm in names(a)) {
      if (!identical(a[[nm]], b[[nm]])) {
        i <- which(a[[nm]] != b[[nm]] | is.na(a[[nm]]) != is.na(b[[nm]]))
        if (length(i) == 0) return(paste0("column ", nm, ": same values, different attributes/class (ours ",
                                          paste(class(a[[nm]]), collapse = "/"), " ref ", paste(class(b[[nm]]), collapse = "/"), ")"))
        return(sprintf("column %s first differs at row %d: ours %s ref %s", nm, i[1],
                       format(a[[nm]][i[1]], digits = 17), format(b[[nm]][i[1]], digits = 17)))
      }
    }
    return(paste0("columns identical; attributes differ: ours ", paste(names(attributes(a)), collapse = ","),
                  " ref ", paste(names(attributes(b)), collapse = ","), "; row names info ",
                  .row_names_info(a), " vs ", .row_names_info(b)))
  }
  if (length(a) != length(b)) return(paste0("length ours ", length(a), " ref ", length(b)))
  i <- which(a != b | is.na(a) != is.na(b))
  if (length(i)) sprintf("first differs at %d: ours %s ref %s", i[1], format(a[i[1]], digits = 17), format(b[i[1]], digits = 17)) else "no elementwise difference (attributes?)"
}
# every member of GGIR's M except bsc_qc (wall clock and memory) must be identical()
expect_M_identical <- function(ours, ref, label = "") {
  ours <- drop_bsc(ours); ref <- drop_bsc(ref)
  testthat::expect_identical(names(ours), names(ref), label = paste(label, "member names"))
  for (nm in names(ref)) {
    testthat::expect(identical(ours[[nm]], ref[[nm]]),
                     paste0(label, " member ", nm, " not identical: ", first_diff(ours[[nm]], ref[[nm]])))
  }
}

# GGIR's nested impute_at_epoch_level, pulled out of g.getmeta's body by walking the AST
ggir_find_assign <- function(expr, name) {
  if (is.call(expr)) {
    if ((identical(expr[[1]], as.name("=")) || identical(expr[[1]], as.name("<-"))) &&
        identical(expr[[2]], as.name(name))) {
      return(expr[[3]])
    }
    for (i in seq_along(expr)) {
      if (is.call(expr[[i]])) {
        r <- ggir_find_assign(expr[[i]], name)
        if (!is.null(r)) return(r)
      }
    }
  }
  NULL
}
ggir_impute_at_epoch_level <- function() {
  skip_if_no_ggir()
  eval(ggir_find_assign(body(GGIR::g.getmeta), "impute_at_epoch_level"))
}
# the recordingEndSleepHour block of g.part1
ggir_find_if <- function(expr, pattern) {
  if (is.call(expr)) {
    if (identical(expr[[1]], as.name("if")) &&
        grepl(pattern, paste(deparse(expr[[2]]), collapse = ""), fixed = TRUE)) return(expr)
    for (i in seq_along(expr)) {
      if (is.call(expr[[i]])) {
        r <- ggir_find_if(expr[[i]], pattern)
        if (!is.null(r)) return(r)
      }
    }
  }
  NULL
}
ggir_tail_expand <- function(M, recordingEndSleepHour, dayborder, desiredtz) {
  skip_if_no_ggir()
  blk <- ggir_find_if(body(GGIR::g.part1), "recordingEndSleepHour")
  env <- new.env()
  env$M <- M
  env$params_general <- list(recordingEndSleepHour = recordingEndSleepHour, dayborder = dayborder, desiredtz = desiredtz)
  env$iso8601chartime2POSIX <- GGIR:::iso8601chartime2POSIX
  env$POSIXtime2iso8601 <- GGIR:::POSIXtime2iso8601
  eval(blk, env)
  list(M = env$M, tail_expansion_log = env$tail_expansion_log)
}
# a canhrActi_raw_meta built from GGIR-shaped tables (for the tail expansion tests)
meta_from_M <- function(M, tz) {
  num <- function(ts) as.numeric(as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S%z", tz = tz))
  structure(list(metashort = .raw.meta.add.time(M$metashort, num(M$metashort$timestamp)),
                 metalong = .raw.meta.add.time(M$metalong, num(M$metalong$timestamp)),
                 qclog = M$QClog, wday = M$wday, wdayname = M$wdayname, windowsizes = M$windowsizes,
                 filecorrupt = M$filecorrupt, filetooshort = M$filetooshort,
                 nfilepagesskipped = M$NFilePagesSkipped, bsc_qc = M$bsc_qc, nonwear = NULL,
                 tail_expansion_log = NULL),
            class = "canhrActi_raw_meta")
}

# the study's synthetic ad-hoc csv layout (3 h at 30 Hz, timestamp strings, temperature)
synth_params <- function(tempcol = TRUE, ...) {
  ov <- list(desiredtz = "UTC", rmc.firstrow.acc = 2, rmc.col.acc = 2:4, rmc.col.time = 1,
             rmc.unit.time = "character", rmc.format.time = "%Y-%m-%d %H:%M:%OS", rmc.sf = 30,
             rmc.dec = ".", rmc.noise = 0.013, minloadcrit = 1)
  if (tempcol) { ov$rmc.col.temp <- 5; ov$rmc.unit.temp <- "C" }
  extra <- list(...)
  for (nm in names(extra)) ov[[nm]] <- extra[[nm]]
  ov
}
synth_setup <- function() {
  if (is.null(memo$synth)) {
    f <- study_file("synth_temp.csv")
    skip_if_no_file(f)
    testthat::skip_if_not_installed("data.table")
    ov <- synth_params(TRUE)
    params <- do.call(raw.params, ov)
    info <- raw.inspect(f, params = params)
    cal <- raw.calibrate(info, params = params)
    memo$synth <- list(file = f, ov = ov, params = params, info = info, cal = cal)
  }
  memo$synth
}
synth_live <- function() {
  if (is.null(memo$synth_live)) {
    skip_if_no_ggir()
    s <- synth_setup()
    P <- ggir_params("UTC", rawdata = s$ov[setdiff(names(s$ov), "desiredtz")])
    I <- ggir_inspect(s$file, P)
    C <- suppressWarnings(GGIR::g.calibrate(s$file, params_rawdata = P$params_rawdata,
                                            params_general = P$params_general,
                                            params_cleaning = P$params_cleaning,
                                            inspectfileobject = I, verbose = FALSE))
    memo$synth_live <- list(P = P, I = I, C = ggir_rule(C), C_raw = C)
  }
  memo$synth_live
}

test_that(".raw.epoch.timestamps: GGIR's two sequence constructions, format and timezone", {
  start <- as.POSIXlt("2025-10-07 20:30:00", tz = MOS2_TZ)
  s <- .raw.epoch.timestamps(start, 118800, 5, tz = MOS2_TZ)
  l <- .raw.epoch.timestamps(start, 660, 900, tz = MOS2_TZ, long = TRUE)
  expect_identical(s$timestamp[1], "2025-10-07T20:30:00-0800")
  expect_identical(s$timestamp[118800], "2025-10-14T17:29:55-0800")
  expect_identical(l$timestamp[660], "2025-10-14T17:15:00-0800")
  expect_identical(s$timestamp[181], l$timestamp[2])
  expect_identical(unique(diff(s$time)), 5)
  expect_identical(unique(diff(l$time)), 900)
  expect_identical(s$time[1], 1759897800)
  expect_type(s$timestamp, "character"); expect_type(s$time, "double")
  # seq(a, a + n*ws2 - 1) gives the same n values as seq(a, a + (n-1)*ws2)
  expect_identical(l$time, .raw.epoch.timestamps(start, 660, 900, tz = MOS2_TZ, long = FALSE)$time)
  # GGIR's own expressions
  starttime3 <- round(as.numeric(start))
  time5 <- seq(starttime3, (starttime3 + ((118800 - 1) * 5)), by = 5)
  time6 <- strftime(as.POSIXlt(time5, origin = "1970-01-01", tz = MOS2_TZ), format = "%Y-%m-%dT%H:%M:%S%z")
  expect_identical(s$timestamp, as.character(time6))
  time1 <- seq(starttime3, (starttime3 + (660 * 900) - 1), by = 900)
  time2 <- strftime(as.POSIXlt(time1, origin = "1970-01-01", tz = MOS2_TZ), format = "%Y-%m-%dT%H:%M:%S%z")
  expect_identical(l$timestamp, as.character(time2))
  # a DST change moves the label, not the edge (Anchorage falls back 2025-11-02)
  d <- .raw.epoch.timestamps(as.POSIXlt("2025-11-02 00:00:00", tz = MOS2_TZ), 4, 3600, tz = MOS2_TZ)
  expect_identical(unique(diff(d$time)), 3600)
  expect_identical(d$timestamp, c("2025-11-02T00:00:00-0800", "2025-11-02T01:00:00-0800",
                                  "2025-11-02T01:00:00-0900", "2025-11-02T02:00:00-0900"))
  expect_identical(.raw.epoch.timestamps(start, 0, 5, tz = MOS2_TZ), list(time = numeric(0), timestamp = character(0)))
  expect_warning(z <- .raw.epoch.timestamps(start, 3, 5, tz = character(0)), "desiredtz not specified")
  expect_length(z$timestamp, 3L)
})

test_that(".raw.iso8601.to.posix and .raw.posix.to.iso8601 reproduce GGIR's helpers", {
  x <- c("2025-10-14T17:15:00-0800", "2025-11-02T01:00:00-0900")
  p <- .raw.iso8601.to.posix(x, MOS2_TZ)
  expect_s3_class(p, "POSIXct")
  # %z is honoured: 17:15 at -0800 is 01:15 UTC; 01:00 at -0900 is 10:00 UTC.
  expect_identical(as.numeric(p), c(1760490900, 1762077600))
  # the second instant is the fall-back hour and comes back as -0800: GGIR's formatter
  # re-parses the wall-clock digits without an offset, so R picks the first occurrence
  expect_identical(.raw.posix.to.iso8601(p, MOS2_TZ)[1], x[1])
  expect_identical(.raw.posix.to.iso8601(p + 0.4, MOS2_TZ)[1], x[1]) # sub-second digits dropped by format()
  skip_if_no_ggir()
  expect_identical(p, GGIR:::iso8601chartime2POSIX(x, MOS2_TZ))
  expect_identical(.raw.posix.to.iso8601(p, MOS2_TZ), GGIR:::POSIXtime2iso8601(p, MOS2_TZ))
  expect_identical(.raw.posix.to.iso8601(p + 0.4, MOS2_TZ), GGIR:::POSIXtime2iso8601(p + 0.4, MOS2_TZ))
})

test_that(".raw.impute.at.epoch.level: metalong and metashort replication rules, duplicate merge, identical() to GGIR's closure", {
  ml <- matrix(as.character(1:20), 5, 4)
  ml[, 1] <- " "
  r <- .raw.impute.at.epoch.level(gapsize = 3, timeseries = ml, gap_index = 2,
                                  metnames = c("timestamp", "nonwearscore", "clippingscore", "EN"))
  expect_identical(dim(r), c(7L, 4L))
  expect_identical(r[2:4, 2], rep("3", 3))           # nonwearscore 3 on the replicas
  expect_identical(r[2:4, 4], rep(ml[2, 4], 3))      # EN copied
  expect_identical(r[c(1, 5:7), ], ml[c(1, 3:5), ])
  # metashort: ENMO 0, EN 1, angle copied
  ms <- matrix(as.character(seq(0.5, by = 0.5, length.out = 24)), 6, 4)
  ms[, 1] <- " "
  r2 <- .raw.impute.at.epoch.level(gapsize = 4, timeseries = ms, gap_index = 5,
                                   metnames = c("timestamp", "anglez", "ENMO", "EN"))
  expect_identical(dim(r2), c(9L, 4L))
  expect_identical(r2[5:8, 2], rep(ms[5, 2], 4))
  expect_identical(r2[5:8, 3], rep("0", 4))
  expect_identical(r2[5:8, 4], rep("1", 4))
  expect_identical(r2[9, ], ms[6, ])
  # two markers in one epoch: sizes combined minus one
  r3 <- .raw.impute.at.epoch.level(gapsize = c(3, 4), timeseries = ms, gap_index = c(2, 2),
                                   metnames = c("timestamp", "anglez", "ENMO", "EN"))
  expect_identical(dim(r3), c(11L, 4L))   # 6 - 1 + (3 + 4 - 1)
  r4 <- .raw.impute.at.epoch.level(gapsize = numeric(0), timeseries = ms, gap_index = integer(0),
                                   metnames = c("timestamp", "anglez", "ENMO", "EN"))
  expect_identical(r4, ms)
  skip_if_no_ggir()
  g <- ggir_impute_at_epoch_level()
  expect_identical(r, g(gapsize = 3, timeseries = ml, gap_index = 2, metnames = c("timestamp", "nonwearscore", "clippingscore", "EN")))
  expect_identical(r2, g(gapsize = 4, timeseries = ms, gap_index = 5, metnames = c("timestamp", "anglez", "ENMO", "EN")))
  expect_identical(r3, g(gapsize = c(3, 4), timeseries = ms, gap_index = c(2, 2), metnames = c("timestamp", "anglez", "ENMO", "EN")))
  expect_identical(r4, g(gapsize = numeric(0), timeseries = ms, gap_index = integer(0), metnames = c("timestamp", "anglez", "ENMO", "EN")))
})

test_that(".raw.apply.calibration: base::scale form, with and without the temperature term (synthetic)", {
  set.seed(3)
  d <- cbind(time = 1:100, x = rnorm(100, 0, 0.5), y = rnorm(100, 0, 0.5), z = rnorm(100, 1, 0.1), temperature = 20 + sin(1:100 / 10))
  offset <- c(-0.02, -0.018, 0.008); scale <- c(1.001, 0.995, 0.998); tempoffset <- c(0.003, -0.002, 0.0025)
  # GGIR's own expression
  ref <- d
  ref[, c("x", "y", "z")] <- scale(ref[, c("x", "y", "z")], center = -offset, scale = 1/scale)
  ours <- .raw.apply.calibration(d, offset, scale, tempoffset, meantempcal = c(), use.temp = TRUE, temperature = d[, "temperature"])
  expect_identical(ours, ref)                      # no meantempcal, so no temperature term
  expect_identical(.raw.apply.calibration(d, offset, scale, tempoffset, meantempcal = 20.5, use.temp = FALSE), ref)
  ref2 <- ref
  yy <- cbind(d[, "temperature"], d[, "temperature"], d[, "temperature"])
  ref2[, c("x", "y", "z")] <- ref2[, c("x", "y", "z")] + scale(yy, center = rep(20.5, 3), scale = 1/tempoffset)
  ours2 <- .raw.apply.calibration(d, offset, scale, tempoffset, meantempcal = 20.5, use.temp = TRUE, temperature = d[, "temperature"])
  expect_identical(ours2, ref2)
  expect_false(identical(ours2, ref))
  expect_identical(ours2[, c("time", "temperature")], d[, c("time", "temperature")])
  expect_identical(.raw.apply.calibration(d, c(0, 0, 0), c(1, 1, 1)), d)
})

test_that(".raw.getmeta.coefficients: the four accepted shapes and GGIR's default C", {
  z <- .raw.getmeta.coefficients(NULL)
  expect_identical(z[c("offset", "scale", "tempoffset")], list(offset = c(0, 0, 0), scale = c(1, 1, 1), tempoffset = c(0, 0, 0)))
  expect_length(z$meantempcal, 0L); expect_false(z$applied)
  C <- list(scale = c(1.1, 1, 1), offset = c(0.01, 0, 0), tempoffset = c(0, 0, 0), meantempcal = NULL, QCmessage = "x")
  g <- .raw.getmeta.coefficients(C)
  expect_identical(g$scale, C$scale); expect_identical(g$offset, C$offset); expect_identical(g$source, "ggir_C"); expect_true(g$applied)
  expect_false(.raw.getmeta.coefficients(list(scale = c(1, 1, 1), offset = c(0, 0, 0), tempoffset = c(0, 0, 0)))$applied)
  expect_error(.raw.getmeta.coefficients(42), "calibration must be")
  expect_error(.raw.getmeta.coefficients(list(a = 1)), "calibration must be")
  skip_if_no_ggir_ref()
  s <- stored_mos2()
  st <- .raw.getmeta.coefficients(s$C)
  expect_identical(st$scale, s$C$scale); expect_identical(st$offset, s$C$offset)
  expect_identical(st$tempoffset, c(0, 0, 0)); expect_null(st$meantempcal); expect_true(st$applied)
  # a data_quality_report row carries 15 significant digits: within 1e-13, not identical
  skip_if_no_file(MOS2_QC_CSV())
  q <- .raw.getmeta.coefficients(MOS2_QC_CSV(), "MOS2E39230594.gt3x")
  expect_equal(q$scale, s$C$scale, tolerance = 1e-13); expect_equal(q$offset, s$C$offset, tolerance = 1e-13)
  expect_length(q$meantempcal, 0L); expect_identical(q$source, "csv"); expect_true(q$applied)
  expect_error(.raw.getmeta.coefficients(MOS2_QC_CSV(), "nosuchfile.gt3x"), "no row for")
})

test_that("P6 MOS2 chunk 1: scale(center = -offset, scale = 1/scale) identical() to GGIR's expression; (x + o) * s is rejected", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  b <- mos2_block1(); C <- stored_mos2()$C
  expect_identical(b$rows_read, 2592000L)
  expect_identical(b$rows_imputed, 5036460L)
  expect_identical(b$dropped, 3480L)          # 116 s at 30 Hz to the 20:30:00 boundary
  expect_identical(b$use, 5022000)            # 186 whole 15-min epochs
  expect_identical(b$LD - b$use, 10980)       # the leftover carried to chunk 2
  d <- b$data
  ggir_way <- d
  ggir_way[, c("x", "y", "z")] <- scale(ggir_way[, c("x", "y", "z")], center = -C$offset, scale = 1/C$scale)
  ours <- .raw.apply.calibration(d, offset = C$offset, scale = C$scale, tempoffset = C$tempoffset,
                                 meantempcal = C$meantempcal, use.temp = FALSE)
  expect_identical(ours, ggir_way)
  alg <- d
  for (k in 1:3) alg[, c("x", "y", "z")[k]] <- (d[, c("x", "y", "z")[k]] + C$offset[k]) * C$scale[k]
  expect_false(identical(alg, ggir_way))
  dif <- abs(alg[, c("x", "y", "z")] - ggir_way[, c("x", "y", "z")])
  expect_true(max(dif) > 0 && max(dif) < 1e-15)   # rounding-level differences
  expect_true(sum(dif > 0) > 1e6)
  swept <- d
  swept[, c("x", "y", "z")] <- sweep(sweep(d[, c("x", "y", "z")], 2, C$offset, "+"), 2, C$scale, "*")
  expect_false(identical(swept, ggir_way))
  # deparse wraps long calls, so join and collapse the whitespace
  src <- gsub("\\s+", " ", paste(deparse(.raw.apply.calibration, width.cutoff = 500L), collapse = " "))
  expect_true(grepl('scale(data[, c("x", "y", "z")], center = -offset, scale = 1/scale)', src, fixed = TRUE))
  expect_false(grepl("+ offset) * scale", src, fixed = TRUE))
  # non-wear on the same intermediate: "2023" matches the stored scores, "2013" differs in 4 of 186
  info <- mos2_info(); M0 <- stored_mos2()$M
  nw23 <- .raw.nonwear.clipping(ours, windowsizes = c(5, 900, 3600), sf = info$sf, clipthres = info$clipthres,
                                sdcriter = info$sdcriter, racriter = info$racriter, approach = "2023")
  nw13 <- .raw.nonwear.clipping(ours, windowsizes = c(5, 900, 3600), sf = info$sf, clipthres = info$clipthres,
                                sdcriter = info$sdcriter, racriter = info$racriter, approach = "2013")
  expect_identical(nw23$nmin, 186)
  expect_identical(nw23$nonwear, M0$metalong$nonwearscore[1:186])
  expect_identical(sum(nw13$nonwear != nw23$nonwear), 4L)
  expect_identical(as.vector(table(nw13$nonwear)), c(184L, 1L, 1L))
  expect_identical(as.vector(table(nw23$nonwear)), c(181L, 5L))
})

test_that("P7a MOS2 with the stored calibration: as.ggir.M identical() to the stored M on every field M has", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  m <- mos2_meta_stored(); M0 <- stored_mos2()$M
  expect_s3_class(m, "canhrActi_raw_meta")
  Mm <- as.ggir.M(m)
  expect_identical(names(Mm), c("filecorrupt", "filetooshort", "NFilePagesSkipped", "metalong", "metashort",
                                "wday", "wdayname", "windowsizes", "bsc_qc", "QClog"))
  expect_M_identical(Mm, M0, "MOS2 stored C")
  expect_identical(dim(Mm$metashort), c(118800L, 3L))
  expect_identical(dim(Mm$metalong), c(660L, 4L))
  expect_identical(names(Mm$metashort), c("timestamp", "anglez", "ENMO"))
  expect_identical(names(Mm$metalong), c("timestamp", "nonwearscore", "clippingscore", "EN"))
  expect_identical(Mm$QClog, M0$QClog)
  expect_identical(nrow(m$qclog), 2L)
  expect_identical(m$qclog$timegaps_n, c(915L, 525L))
  expect_equal(m$qclog$timegaps_min, c(1358.54166667, 6093.10833333), tolerance = 1e-9)
  expect_identical(m$qclog$blockLengthSeconds, c(167882, 137906))
  expect_identical(m$wday, 3); expect_identical(m$wdayname, "Tuesday")
  expect_identical(m$windowsizes, c(5, 900, 3600))
  expect_false(m$filecorrupt); expect_false(m$filetooshort); expect_identical(m$nfilepagesskipped, 0)
  # the canhrActi additions
  expect_identical(names(m$metashort), c("timestamp", "time", "anglez", "ENMO"))
  expect_identical(names(m$metalong), c("timestamp", "time", "nonwearscore", "clippingscore", "EN"))
  expect_identical(m$metric_names, c("anglez", "ENMO"))
  expect_identical(m$calibration$source, "ggir_C"); expect_true(m$calibration$applied)
  expect_identical(m$calibration$scale, stored_mos2()$C$scale)
  expect_null(m$tail_expansion_log); expect_null(m$nonwear)
  expect_identical(nrow(m$bsc_qc), 2L)
  expect_length(m$messages, 0L)
  expect_false(m$light_available); expect_false(m$use_temp)
  # bsc_qc has GGIR's shape; wall clock and memory differ
  expect_identical(names(m$bsc_qc), names(M0$bsc_qc))
})

test_that("P8a/P8b MOS2: start alignment, timestamps regenerated from the start, 5 s / 900 s spacing, [181] == [2]", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  m <- mos2_meta_stored()
  expect_identical(m$metashort$timestamp[1], "2025-10-07T20:30:00-0800")
  expect_identical(m$metalong$timestamp[1], "2025-10-07T20:30:00-0800")
  expect_identical(m$metashort$timestamp[118800], "2025-10-14T17:29:55-0800")
  expect_identical(m$metalong$timestamp[660], "2025-10-14T17:15:00-0800")
  expect_identical(unique(diff(m$metashort$time)), 5)
  expect_identical(unique(diff(m$metalong$time)), 900)
  expect_identical(m$metashort$timestamp[181], m$metalong$timestamp[2])
  expect_identical(m$metashort$time[181], m$metalong$time[2])
  expect_identical(as.numeric(as.POSIXct(m$metashort$timestamp, format = "%Y-%m-%dT%H:%M:%S%z", tz = MOS2_TZ)), m$metashort$time)
  expect_identical(m$metashort$time[1], 1759897800)
  expect_s3_class(m$starttime, "POSIXct")
  expect_identical(as.numeric(m$starttime), 1759897800)
  expect_identical(format(m$starttime, "%Y-%m-%d %H:%M:%S", tz = MOS2_TZ), "2025-10-07 20:30:00")
  expect_identical(m$metashort$ENMO[1:6], c(0.0175, 0.0604, 0.0581, 0.0659, 0.0396, 0.0896))
  expect_identical(m$metashort$anglez[1:6], c(48.0663, 2.2517, 7.3921, 29.8733, 10.2377, 29.3146))
  # the chunk trace; 57780 short and 321 long epochs are replicated in block 2
  ch <- m$chunks
  expect_identical(ch$block, 1:3)
  expect_identical(ch$rows_read, c(2592000, 1837110, 0))
  expect_identical(ch$rows_imputed, c(5036460, 4137180, 0))
  expect_identical(ch$rows_dropped_start, c(3480, 0, 0))
  expect_identical(ch$rows_used, c(5022000, 4131000, 0))
  expect_identical(ch$rows_carried_out, c(10980, 17160, 0))
  expect_identical(ch$rows_carried_in, c(0, 10980, 17160))
  expect_identical(ch$epochs_short, c(33480, 27540, 0))
  expect_identical(ch$epochs_long, c(186, 153, 0))
  expect_identical(ch$epochs_short_added, c(0, 57780, 0))
  expect_identical(ch$epochs_long_added, c(0, 321, 0))
  expect_identical(sum(ch$epochs_short) + sum(ch$epochs_short_added), 118800)
  expect_identical(sum(ch$epochs_long) + sum(ch$epochs_long_added), 660)
  expect_identical(ch$gaps, c(915L, 525L, 0L))
  expect_identical(ch$is_last_block, c(FALSE, FALSE, TRUE))
  expect_identical(ch$span_seconds[1:2], c(167882, 426806))
  expect_identical(m$rows_discarded_end, 17160)   # 9.5 min after 17:30, under one hour
})

test_that("P8c MOS2: the raw fills sit back to back before the replicated marker epochs of the first > 90 min gap", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  m <- mos2_meta_stored()
  ms <- m$metashort; ml <- m$metalong
  i <- which(ms$timestamp == "2025-10-10T23:07:25-0800")
  expect_identical(i, 53730L)
  expect_identical(ms$timestamp[i + 192], "2025-10-10T23:23:25-0800")
  expect_identical(unique(ms$anglez[i:(i + 192)]), 1.3695)      # raw fills, 193 rows
  expect_identical(unique(ms$ENMO[i:(i + 192)]), 0)
  expect_identical(ms$timestamp[i + 193], "2025-10-10T23:23:30-0800")
  expect_identical(unique(ms$anglez[(i + 193):(i + 193 + 1260)]), 10.998)  # 1261 marker replicas
  expect_identical(unique(ms$ENMO[(i + 193):(i + 193 + 1260)]), 0)
  expect_identical(ms$timestamp[i + 193 + 1260], "2025-10-11T01:08:30-0800")
  expect_identical(ms$timestamp[i + 193 + 1261], "2025-10-11T01:08:35-0800")   # real data resumes
  expect_identical(ms$anglez[i + 193 + 1261], 73.2985)
  expect_identical(ms$ENMO[i + 193 + 1261], 0.0079)
  expect_identical(ml$timestamp[300], "2025-10-10T23:15:00-0800")
  expect_identical(ml$nonwearscore[300:307], rep(3, 8))
  expect_identical(unique(ml$EN[300:307]), 0.9896)
  expect_identical(ml$nonwearscore[299], 1); expect_identical(ml$EN[299], 0.9826)
  expect_identical(ml$nonwearscore[308], 0); expect_identical(ml$EN[308], 0.9991)
  expect_identical(ml$timestamp[308], "2025-10-11T01:15:00-0800")
})

test_that("P9 MOS2 whole file: non-wear distribution 0:308 1:9 3:343, clipping non-zero in 10 of 660 epochs as k/27000 after the character round trip", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  m <- mos2_meta_stored()
  nw <- table(m$metalong$nonwearscore)
  expect_identical(names(nw), c("0", "1", "3"))
  expect_identical(as.vector(nw), c(308L, 9L, 343L))
  cs <- m$metalong$clippingscore
  expect_identical(sum(cs != 0), 10L)
  k <- round(cs[cs != 0] * 27000)
  expect_identical(sort(unique(k)), c(1, 2, 3, 5))
  # GGIR stores as.numeric(as.character(k/27000)), not k/27000
  expect_identical(cs[cs != 0], as.numeric(as.character(k / 27000)))
  expect_false(identical(cs[cs != 0], k / 27000))
  expect_true(all(cs >= 0 & cs <= 1))
})

test_that("P7a MOS2 with raw.calibrate's calibration: identical() to the stored M as well", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  m <- mos2_meta_fit()
  expect_identical(m$calibration$source, "canhrActi_raw_calibration")
  expect_true(m$calibration$applied)
  expect_identical(m$calibration$scale, stored_mos2()$C$scale)
  expect_identical(m$calibration$offset, stored_mos2()$C$offset)
  expect_M_identical(as.ggir.M(m), stored_mos2()$M, "MOS2 raw.calibrate C")
  expect_identical(m$metashort, mos2_meta_stored()$metashort)
  expect_identical(m$metalong, mos2_meta_stored()$metalong)
})

test_that("T2 MOS2: live GGIR::g.getmeta with the stored C is identical() to ours and to the stored M", {
  # P7a holds ours to the stored M; this adds only that the installed GGIR still gives it
  if (!canhr_flag("CANHRACTI_GGIR_SLOW")) {
    skip("set CANHRACTI_GGIR_SLOW=1 to compare with a live GGIR run on MOS2 (about 35 s)")
  }
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_ggir()
  live <- mos2_meta_live()
  expect_M_identical(as.ggir.M(mos2_meta_stored()), live, "ours vs live GGIR")
  expect_M_identical(live, stored_mos2()$M, "live GGIR vs stored")
  expect_identical(nrow(live$bsc_qc), nrow(mos2_meta_stored()$bsc_qc))
})

test_that("daylimit = 1: chunk 1 alone gives the first 33480 short and 186 long rows of the stored tables", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  m <- mos2_meta_day1(); M0 <- stored_mos2()$M
  expect_identical(dim(m$metashort), c(33480L, 4L)); expect_identical(dim(m$metalong), c(186L, 5L))
  for (nm in names(M0$metashort)) expect_identical(m$metashort[[nm]], M0$metashort[[nm]][1:33480], label = nm)
  for (nm in names(M0$metalong)) expect_identical(m$metalong[[nm]], M0$metalong[[nm]][1:186], label = nm)
  expect_identical(nrow(m$chunks), 1L)
  expect_identical(m$chunks$rows_dropped_start, 3480)
  expect_identical(nrow(m$qclog), 1L)
  expect_identical(m$qclog, M0$QClog[1, ])
  expect_true(any(grepl("limited to 1 days", m$messages, fixed = TRUE)))
  expect_identical(m$settings$daylimit, 1)
  expect_false(m$filetooshort)
})

test_that(".raw.tail.expand: identical() to the g.part1 block on a recording ending at 21:59:55, including M$nonwear", {
  skip_if_no_ggir_ref()
  M0 <- stored_mos2()$M
  cutS <- which(M0$metashort$timestamp == "2025-10-08T21:59:55-0800")
  cutL <- which(M0$metalong$timestamp == "2025-10-08T21:45:00-0800")
  Ms <- M0; Ms$metashort <- M0$metashort[1:cutS, ]; Ms$metalong <- M0$metalong[1:cutL, ]
  x <- meta_from_M(Ms, MOS2_TZ)
  y <- .raw.tail.expand(x, recordingEndSleepHour = 19, dayborder = 0, desiredtz = MOS2_TZ)
  expect_identical(y$tail_expansion_log, list(short = 7560L, long = 42L))
  expect_identical(nrow(y$metashort), cutS + 7560L); expect_identical(nrow(y$metalong), cutL + 42L)
  expect_identical(tail(y$metalong$nonwearscore, 42), rep(-1, 42))
  expect_identical(tail(y$metalong$clippingscore, 42), rep(0, 42))
  expect_identical(tail(y$metalong$EN, 42), rep(0, 42))
  expect_identical(tail(y$metashort$ENMO, 7560), rep(0, 7560))
  expect_identical(tail(y$metashort$anglez, 7560), round(sin((1:7560) / 180)) * 15)
  expect_identical(tail(y$metashort$timestamp, 1), "2025-10-09T08:29:55-0800")
  expect_identical(tail(y$metalong$timestamp, 1), "2025-10-09T08:15:00-0800")
  expect_identical(unique(diff(y$metashort$time)), 5); expect_identical(unique(diff(y$metalong$time)), 900)
  expect_identical(as.numeric(as.POSIXct(y$metashort$timestamp, format = "%Y-%m-%dT%H:%M:%S%z", tz = MOS2_TZ)), y$metashort$time)
  expect_length(y$nonwear, cutS + 7560L)
  expect_identical(sum(is.na(y$nonwear)), cutS); expect_identical(sum(y$nonwear == 0, na.rm = TRUE), 7560L)
  Mg <- as.ggir.M(y)
  expect_identical(names(Mg)[11], "nonwear")
  # unchanged when the parameter is NULL or the recording ends too early (MOS2 ends 17:15)
  z <- .raw.tail.expand(x, recordingEndSleepHour = NULL)
  expect_null(z$tail_expansion_log); expect_identical(z$metashort, x$metashort); expect_null(z$nonwear)
  z2 <- .raw.tail.expand(meta_from_M(M0, MOS2_TZ), recordingEndSleepHour = 19, desiredtz = MOS2_TZ)
  expect_null(z2$tail_expansion_log); expect_identical(nrow(z2$metashort), 118800L)
  expect_error(.raw.tail.expand(list(), 19), "canhrActi_raw_meta")
  skip_if_no_ggir()
  ref <- ggir_tail_expand(Ms, 19, 0, MOS2_TZ)
  expect_identical(ref$tail_expansion_log, y$tail_expansion_log)
  expect_identical(names(Mg), names(ref$M))
  for (nm in names(ref$M)) {
    expect(identical(Mg[[nm]], ref$M[[nm]]), paste0("tail expansion member ", nm, ": ", first_diff(Mg[[nm]], ref$M[[nm]])))
  }
  ref0 <- ggir_tail_expand(M0, 19, 0, MOS2_TZ)
  expect_null(ref0$tail_expansion_log)
})

test_that("TRUNC (sf NULL): the corrupt object, identical() to GGIR's early return", {
  skip_if_no_ggir_ref(); skip_if_no_file(TRUNC())
  info <- suppressWarnings(raw.inspect(TRUNC(), desiredtz = MOS2_TZ))
  expect_null(info$sf)
  m <- raw.getmeta(info)
  expect_true(m$filecorrupt); expect_false(m$filetooshort); expect_false(m$skipped)
  expect_null(m$metashort); expect_null(m$metalong); expect_null(m$qclog); expect_null(m$chunks)
  expect_null(m$wday); expect_null(m$windowsizes); expect_null(m$starttime)
  expect_true(any(grepl("corrupt", m$messages)))
  M <- as.ggir.M(m)
  expect_identical(M, list(filecorrupt = TRUE, filetooshort = FALSE, NFilePagesSkipped = 0,
                           metalong = NULL, metashort = NULL, wday = NULL, wdayname = NULL,
                           windowsizes = NULL, bsc_qc = data.frame(time = c(), size = c(), stringsAsFactors = FALSE),
                           QClog = NULL))
  skip_if_no_ggir()
  P <- ggir_params(MOS2_TZ)
  live <- ggir_getmeta(TRUNC(), P, ggir_inspect(TRUNC(), P))
  expect_identical(drop_bsc(M), drop_bsc(live))
  expect_identical(M$bsc_qc, live$bsc_qc)
})

test_that("SHORT (330 s at 100 Hz): too short after the block-1 floor, identical() to GGIR; skip_small_files gives a skipped object", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  info <- raw.inspect(SHORT(), desiredtz = MOS2_TZ)
  expect_identical(info$sf, 100)
  m <- raw.getmeta(info)
  expect_false(m$filecorrupt); expect_true(m$filetooshort); expect_false(m$skipped)
  expect_null(m$metashort); expect_null(m$metalong); expect_null(m$windowsizes)
  expect_identical(nrow(m$chunks), 1L); expect_identical(m$chunks$rows_read, 0); expect_true(m$chunks$is_last_block)
  M <- as.ggir.M(m)
  expect_identical(M$filetooshort, TRUE); expect_null(M$metashort); expect_null(M$QClog)
  sk <- raw.getmeta(raw.inspect(SHORT(), desiredtz = MOS2_TZ, skip_small_files = TRUE))
  expect_true(sk$skipped); expect_true(sk$filetooshort); expect_false(sk$filecorrupt); expect_null(sk$metashort)
  expect_true(any(grepl("skipped at inspection", sk$messages)))
  skip_if_no_ggir()
  P <- ggir_params(MOS2_TZ)
  live <- ggir_getmeta(SHORT(), P, ggir_inspect(SHORT(), P))
  expect_identical(drop_bsc(M), drop_bsc(live))
})

test_that("GGIRread test files (GENEActiv .bin, AX3 .cwa, Parmay .BIN, under 2 h): too short, identical() to GGIR", {
  skip_if_no_ggir_ref()
  tf <- ggirread_tf()
  for (fn in c("GENEActiv_testfile.bin", "ax3_testfile.cwa", "mtx_100Hz_acc_HR_temp.BIN")) {
    f <- file.path(tf, fn)
    skip_if_no_file(f)
    testthat::skip_if_not_installed("GGIRread")
    info <- suppressWarnings(raw.inspect(f, desiredtz = OTHER_TZ))
    m <- suppressWarnings(raw.getmeta(info))
    expect_true(m$filetooshort, label = fn); expect_false(m$filecorrupt, label = fn)
    expect_null(m$metashort)
    if (requireNamespace("GGIR", quietly = TRUE)) {
      P <- ggir_params(OTHER_TZ)
      live <- ggir_getmeta(f, P, ggir_inspect(f, P))
      expect_identical(drop_bsc(as.ggir.M(m)), drop_bsc(live), label = fn)
    }
  }
})

test_that("synth_temp.csv: temperature calibration applied (meantempcal term), temperaturemean column, identical() to live GGIR", {
  skip_if_no_ggir_ref()
  s <- synth_setup()
  expect_true(s$cal$use_temp); expect_true(s$cal$applied)
  expect_equal(s$cal$meantempcal, 23.06698, tolerance = 1e-6)
  m <- raw.getmeta(s$info, calibration = s$cal, params = s$params)
  expect_true(m$use_temp); expect_false(m$light_available)
  expect_identical(names(m$metalong), c("timestamp", "time", "nonwearscore", "clippingscore", "temperaturemean", "EN"))
  expect_identical(dim(m$metashort), c(1980L, 4L)); expect_identical(dim(m$metalong), c(11L, 6L))
  expect_identical(m$metashort$timestamp[1], "2024-01-01T00:15:00+0000")
  expect_identical(m$chunks$rows_read[1], 323999); expect_identical(m$chunks$rows_dropped_start[1], 26999)
  expect_identical(m$chunks$rows_used[1], 297000); expect_identical(m$chunks$rows_carried_out[1], 0)
  expect_true(all(m$metalong$temperaturemean > 16 & m$metalong$temperaturemean < 28))
  expect_identical(m$calibration$meantempcal, s$cal$meantempcal)
  expect_identical(m$calibration$tempoffset, s$cal$tempoffset)
  # the identity calibration gives a different ENMO, so the coefficients took effect
  m0 <- raw.getmeta(s$info, params = s$params)
  expect_false(identical(m0$metashort$ENMO, m$metashort$ENMO))
  expect_identical(m0$calibration$source, "none")
  # ggir_exact = FALSE changes nothing here (the between-chunk fill is not reached)
  m_nx <- raw.getmeta(s$info, calibration = s$cal, params = s$params, ggir_exact = FALSE)
  expect_identical(m_nx$metashort, m$metashort); expect_identical(m_nx$metalong, m$metalong)
  expect_false(m_nx$settings$ggir_exact)
  memo$synth_meta <- m; memo$synth_meta0 <- m0
  skip_if_no_ggir()
  lv <- synth_live()
  expect_identical(lv$C$scale, s$cal$scale); expect_identical(lv$C$offset, s$cal$offset)
  expect_identical(lv$C$tempoffset, s$cal$tempoffset); expect_identical(lv$C$meantempcal, s$cal$meantempcal)
  live <- ggir_getmeta(s$file, lv$P, lv$I, lv$C)
  expect_M_identical(as.ggir.M(m), live, "synth_temp calibrated")
  live0 <- ggir_getmeta(s$file, lv$P, lv$I, NULL)
  expect_M_identical(as.ggir.M(m0), live0, "synth_temp identity")
})

test_that("synth_temp.csv with every metric on: 34 metric columns in allmetrics order, angle_ renamed, identical() to live GGIR", {
  skip_if_no_ggir_ref()
  s <- synth_setup()
  flags <- .raw.metric.flags(all = TRUE)
  flags <- flags[names(flags) != "do.brondcounts"]
  testthat::skip_if_not_installed("signal"); testthat::skip_if_not_installed("actilifecounts")
  m <- do.call(raw.getmeta, c(list(s$info, calibration = s$cal, params = s$params), flags))
  expected <- gsub("angle_", "angle", .raw.metric.names())
  expect_identical(names(m$metashort), c("timestamp", "time", expected))
  expect_identical(m$metric_names, expected)
  expect_identical(dim(m$metashort), c(1980L, 36L))
  expect_true(all(sapply(m$metashort[-1], is.numeric)))
  expect_true(all(m$metashort$NeishabouriCount_x == round(m$metashort$NeishabouriCount_x)))
  expect_identical(m$metashort$anglez, memo$synth_meta$metashort$anglez)
  expect_identical(m$metashort$ENMO, memo$synth_meta$metashort$ENMO)
  skip_if_no_ggir()
  lv <- synth_live()
  Pm <- lv$P
  for (nm in names(flags)) if (nm %in% names(Pm$params_metrics)) Pm$params_metrics[[nm]] <- flags[[nm]]
  live <- ggir_getmeta(s$file, Pm, lv$I, lv$C)
  expect_M_identical(as.ggir.M(m), live, "synth_temp all metrics")
})

test_that("raw.getmeta: argument validation, parameter overrides, progress callback, print", {
  expect_error(raw.getmeta(list(a = 1)), "canhrActi_raw_info")
  expect_error(raw.getmeta(structure(list(monc = 3, dformc = 6, sf = 30), class = "canhrActi_raw_info"), progress = 42), "progress must be")
  expect_error(raw.getmeta(structure(list(monc = 3, dformc = 6, sf = 30), class = "canhrActi_raw_info"), daylimit = -1), "daylimit")
  skip_if_no_ggir_ref()
  s <- synth_setup()
  expect_error(raw.getmeta(s$info, params = s$params, nosuchparameter = 1), "unknown")
  expect_error(raw.getmeta(s$info, params = s$params, do.enmo = FALSE, do.anglez = FALSE), "No metrics selected")
  calls <- list()
  m <- raw.getmeta(s$info, calibration = s$cal, params = s$params,
                   progress = function(stage, i, n, message) calls[[length(calls) + 1]] <<- list(stage, i, n, message))
  expect_identical(length(calls), nrow(m$chunks))
  expect_identical(calls[[1]][[1]], "getmeta"); expect_identical(calls[[1]][[2]], 1L)
  expect_true(is.na(calls[[1]][[3]])); expect_identical(calls[[1]][[4]], "Loading chunk: 1")
  # params$progress is honoured when the argument is NULL
  calls2 <- 0L
  p2 <- do.call(raw.params, c(s$ov, list(progress = function(stage, i, n, message) calls2 <<- calls2 + 1L)))
  m2 <- raw.getmeta(s$info, calibration = s$cal, params = p2)
  expect_identical(calls2, nrow(m2$chunks))
  m3 <- raw.getmeta(s$info, calibration = s$cal, params = s$params, do.anglez = FALSE, do.en = TRUE)
  expect_identical(names(m3$metashort), c("timestamp", "time", "ENMO", "EN"))
  expect_identical(m3$metashort$ENMO, m$metashort$ENMO)
  out <- capture.output(print.canhrActi_raw_meta(m))
  expect_true(any(grepl("canhrActi raw epoch tables", out, fixed = TRUE)))
  expect_true(any(grepl("1980 short (5 s), 11 long (900 s)", out, fixed = TRUE)))
  expect_true(any(grepl("anglez, ENMO", out, fixed = TRUE)))
  expect_true(any(grepl("canhrActi_raw_calibration (applied)", out, fixed = TRUE)))
  skip_if_no_file(TRUNC())
  out2 <- capture.output(print.canhrActi_raw_meta(raw.getmeta(suppressWarnings(raw.inspect(TRUNC(), desiredtz = MOS2_TZ)))))
  expect_true(any(grepl("file corrupt; no epochs", out2, fixed = TRUE)))
})

test_that("EE (long): tables identical() to the stored milestone, 8 qclog rows, the final 35.5 min dropped", {
  skip_if_no_ggir_ref(); skip_unless_long(); skip_if_no_file(EE()); skip_if_no_file(EE_RDATA())
  e <- new.env(); load(EE_RDATA(), envir = e)
  info <- raw.inspect(EE(), desiredtz = EE_TZ)
  expect_identical(info$sf, 100)
  m <- raw.getmeta(info, calibration = e$C)
  expect_M_identical(as.ggir.M(m), e$M, "EE stored C")
  expect_identical(nrow(m$qclog), 8L)
  expect_identical(m$qclog$blockLengthSeconds, c(86401, 86400, 86400, 86400, 86400, 86401, 86400, 2042))
  expect_identical(m$qclog$timegaps_n[c(1, 6)], c(1L, 1L))
  expect_equal(m$qclog$timegaps_min[c(1, 6)], c(0.016833, 0.016833), tolerance = 1e-4)
  expect_identical(m$wday, e$M$wday); expect_identical(m$wdayname, e$M$wdayname)
  expect_identical(m$chunks$rows_dropped_start[1], 0)     # EE starts at 09:15:00
  expect_identical(format(m$starttime, "%H:%M:%S", tz = EE_TZ), "09:15:00")
  expect_identical(substr(tail(m$metashort$timestamp, 1), 12, 19), "09:14:55")
  expect_identical(substr(tail(m$metalong$timestamp, 1), 12, 19), "09:00:00")
  last_sample <- info$header_list[["Last Sample Time"]]
  expect_identical(format(last_sample, "%H:%M:%S", tz = "GMT"), "09:50:29")
  # the last block (204200 samples plus the 200 carried in) is under an hour and is discarded whole
  end_wall <- as.numeric(as.POSIXct(paste(format(last_sample, "%Y-%m-%d", tz = "GMT"), "09:15:00"), tz = "GMT"))
  expect_identical(as.numeric(as.POSIXct(format(last_sample, "%Y-%m-%d %H:%M:%S", tz = "GMT"), tz = "GMT")) - end_wall, 2129)
  expect_identical(nrow(m$chunks), 8L)
  expect_identical(m$chunks$rows_read, c(rep(8640000, 7), 204200))
  expect_identical(m$chunks$rows_used, c(rep(8640000, 7), 0))
  expect_identical(m$chunks$rows_carried_in, c(0, rep(100, 5), 200, 200))
  expect_identical(m$rows_discarded_end, 204400)
  expect_identical(dim(m$metashort), c(120960L, 4L)); expect_identical(dim(m$metalong), c(672L, 5L))
})
