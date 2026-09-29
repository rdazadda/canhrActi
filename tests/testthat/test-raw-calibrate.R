# Parity tests for R/raw_calibrate.R against GGIR's g.calibrate and the decision rule of
# g.part1. Reference data live in the folder named by CANHRACTI_GGIR_REF, the synthetic
# csvs and the GGIRread test files one level up; tests skip when a file or GGIR is
# missing. The EE file (10 min) runs only when CANHRACTI_LONG_TESTS is set, and the live
# GGIR run on MOS2 only when CANHRACTI_GGIR_SLOW is set.

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
OTHER_TZ <- "Europe/London"      # the zone used for the GGIRread test files

# GGIR's message strings, verbatim
MSG_NOT_ENOUGH <- "recalibration not done because not enough data in the file or because file is corrupt"
MSG_NO_STILL <- "recalibration not done because no non-movement data available"
MSG_SPHERE <- "recalibration not done because not enough points on all sides of the sphere"
MSG_DONE <- "recalibration done, no problems detected"
MSG_DONE_NOTEMP <- "recalibration done, but temperature values not used"
MSG_MAYBE <- "recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this"
MSG_NOT_DONE <- "Autocalibration not done"

# GGIR's default C, built the way g.part1 builds it
ggir_Cdefault <- function() {
  C <- list(cal.error.end = 0, cal.error.start = 0)
  C$scale <- c(1,1,1)
  C$offset <- c(0,0,0)
  C$tempoffset <-  c(0,0,0)
  C$QCmessage <- "Autocalibration not done"
  C$npoints <- 0
  C$nhoursused <- 0
  C$use.temp <- TRUE
  C
}

# GGIR's identity-reset rule, as g.part1 applies it to a C list
ggir_rule <- function(C) {
  cal.error.end <- C$cal.error.end
  cal.error.start <- C$cal.error.start
  if (length(cal.error.start) == 0) cal.error.start <- NA
  check.backup.cal.coef <- FALSE
  if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
    C$scale <- c(1,1,1); C$offset <- c(0,0,0); C$tempoffset <- c(0,0,0)
    check.backup.cal.coef <- TRUE
  } else {
    if (cal.error.start < cal.error.end) {
      C$scale <- c(1,1,1); C$offset <- c(0,0,0); C$tempoffset <-  c(0,0,0)
      check.backup.cal.coef <- TRUE
    }
  }
  list(C = C, check = check.backup.cal.coef)
}

# live g.calibrate with load_params defaults, desiredtz and params_rawdata overrides
ggir_calibrate <- function(file, desiredtz, ...) {
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- desiredtz
  ov <- list(...)
  for (nm in names(ov)) P$params_rawdata[nm] <- list(ov[[nm]])
  I <- suppressWarnings(GGIR::g.inspectfile(file, desiredtz = desiredtz,
                                            params_rawdata = P$params_rawdata,
                                            configtz = P$params_general[["configtz"]]))
  suppressWarnings(GGIR::g.calibrate(file, params_rawdata = P$params_rawdata,
                                     params_general = P$params_general,
                                     params_cleaning = P$params_cleaning,
                                     inspectfileobject = I, verbose = FALSE))
}

drop_bsc <- function(C) C[names(C) != "bsc_qc"]

# first differing member of two GGIR C lists, for failure messages
first_diff_C <- function(a, b) {
  for (nm in union(names(a), names(b))) {
    if (!identical(a[[nm]], b[[nm]])) {
      return(paste0("member ", nm, ": ours ", paste(format(unlist(a[[nm]]), digits = 17), collapse = " "),
                    " ref ", paste(format(unlist(b[[nm]]), digits = 17), collapse = " ")))
    }
  }
  "no differing member (attributes or order differ)"
}
expect_C_identical <- function(ours, ref, label = "") {
  testthat::expect(identical(drop_bsc(ours), drop_bsc(ref)),
                   paste0(label, " C not identical: ", first_diff_C(drop_bsc(ours), drop_bsc(ref))))
}

# Pull a named closure (rollMean, rollSD) out of the body of GGIR's g.calibrate.
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
ggir_closure <- function(name) {
  skip_if_no_ggir()
  f <- eval(ggir_find_assign(body(GGIR::g.calibrate), name), envir = asNamespace("GGIR"))
  if (!is.function(f)) testthat::skip(paste0("closure ", name, " not found in GGIR::g.calibrate"))
  f
}

memo <- new.env()

mos2_info <- function() {
  if (is.null(memo$info)) memo$info <- raw.inspect(MOS2(), desiredtz = MOS2_TZ)
  memo$info
}
mos2_cal <- function() {
  if (is.null(memo$cal)) {
    memo$progress <- list()
    rec <- function(stage, i, n, message) memo$progress[[length(memo$progress) + 1]] <- list(stage = stage, i = i, n = n, message = message)
    memo$cal <- raw.calibrate(mos2_info(), progress = rec)
  }
  memo$cal
}
mos2_stored <- function() {
  if (is.null(memo$stored)) {
    e <- new.env()
    load(MOS2_RDATA(), envir = e)
    memo$stored <- e$C
  }
  memo$stored
}
mos2_live <- function() {
  skip_if_no_ggir()
  if (is.null(memo$live)) memo$live <- ggir_calibrate(MOS2(), MOS2_TZ)
  memo$live
}

# the synthetic csv layout: 3 h at 30 Hz, timestamp strings, minloadcrit 1 so a 2 h fit is accepted
synth_params <- function(tempcol, ...) {
  ov <- list(desiredtz = "UTC", rmc.firstrow.acc = 2, rmc.col.acc = 2:4, rmc.col.time = 1,
             rmc.unit.time = "character", rmc.format.time = "%Y-%m-%d %H:%M:%OS", rmc.sf = 30,
             rmc.dec = ".", rmc.noise = 0.013, minloadcrit = 1)
  if (tempcol) { ov$rmc.col.temp <- 5; ov$rmc.unit.temp <- "C" }
  extra <- list(...)
  for (nm in names(extra)) ov[[nm]] <- extra[[nm]]
  ov
}
synth_run <- function(file, tempcol, ...) {
  skip_if_no_file(file)
  testthat::skip_if_not_installed("data.table")
  ov <- synth_params(tempcol, ...)
  params <- do.call(raw.params, ov)
  info <- raw.inspect(file, params = params)
  list(cal = raw.calibrate(info, params = params), info = info, params = params, ov = ov)
}
synth_live <- function(file, ov) {
  skip_if_no_ggir()
  pr <- ov[setdiff(names(ov), "desiredtz")]
  do.call(ggir_calibrate, c(list(file = file, desiredtz = "UTC"), pr))
}
# the two synthetic runs later tests compare with, computed on first use
synth_temp_run <- function() {
  if (is.null(memo$synth_temp)) {
    memo$synth_temp <- synth_run(study_file("synth_temp.csv"), tempcol = TRUE)
  }
  memo$synth_temp
}
synth_consttemp_run <- function() {
  if (is.null(memo$synth_consttemp)) {
    memo$synth_consttemp <- synth_run(study_file("synth_consttemp.csv"), tempcol = TRUE)
  }
  memo$synth_consttemp
}

test_that(".raw.roll.mean and .raw.roll.sd: window statistics at 30 and 100 Hz, identical() to GGIR's nested rollMean/rollSD", {
  set.seed(11)
  x30 <- rnorm(3000); x100 <- rnorm(9000)
  m30 <- .raw.roll.mean(x30, 30); s30 <- .raw.roll.sd(x30, 30)
  m100 <- .raw.roll.mean(x100, 100, epoch = 10); s100 <- .raw.roll.sd(x100, 100, epoch = 10)
  expect_length(m30, 10L); expect_length(s30, 10L); expect_length(m100, 9L); expect_length(s100, 9L)
  # the cumsum trick is not the same floating-point operation as mean()
  expect_equal(m30, unname(sapply(split(x30, rep(1:10, each = 300)), mean)), tolerance = 1e-12)
  expect_identical(s30, unname(sapply(split(x30, rep(1:10, each = 300)), stats::sd)))
  expect_equal(m100[1], mean(x100[1:1000]), tolerance = 1e-12)
  # a partial trailing window is dropped by the mean and errors in the sd
  expect_length(.raw.roll.mean(c(x30, 1:5), 30), 10L)
  expect_error(.raw.roll.sd(c(x30, 1:5), 30), "dims")
  rollMean <- ggir_closure("rollMean"); rollSD <- ggir_closure("rollSD")
  expect_identical(m30, rollMean(x30, 30, 10)); expect_identical(s30, rollSD(x30, 30, 10))
  expect_identical(m100, rollMean(x100, 100, 10)); expect_identical(s100, rollSD(x100, 100, 10))
})

test_that("P5a MOS2: every field of as.ggir.C identical() to the stored C, with the quoted values", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  cal <- mos2_cal(); Cs <- mos2_stored(); C <- as.ggir.C(cal)
  expect_s3_class(cal, "canhrActi_raw_calibration")
  expect_identical(names(C), c("scale", "offset", "tempoffset", "cal.error.start", "cal.error.end",
                               "spheredata", "npoints", "nhoursused", "QCmessage", "use.temp",
                               "meantempcal", "bsc_qc"))
  expect_identical(names(C), names(Cs))
  for (nm in setdiff(names(Cs), "bsc_qc")) {
    expect(identical(C[[nm]], Cs[[nm]]), paste0("member ", nm, " differs: ", first_diff_C(C[nm], Cs[nm])))
  }
  expect_C_identical(C, Cs, "MOS2 vs stored")
  expect_C_identical(as.ggir.C(cal, decided = FALSE), Cs, "MOS2 last fit vs stored")
  # 17 significant digits round-trip exactly
  expect_identical(cal$scale, c(1.0009963740829368, 0.99502644175360377, 0.99849677326354713))
  expect_identical(cal$offset, c(-0.020387751890171227, -0.017829364795671376, 0.0077769254660801793))
  expect_identical(cal$tempoffset, c(0, 0, 0))
  expect_identical(cal$cal_error_start, 0.01614)
  expect_identical(cal$cal_error_end, 0.0058)
  expect_identical(cal$npoints, 1176L)
  expect_identical(cal$nhoursused, 41)
  expect_identical(cal$use_temp, FALSE)
  expect_false(cal$temp_available)
  expect_null(cal$meantempcal)
  expect_true("meantempcal" %in% names(cal))  # NULL member kept, not dropped
  expect_identical(cal$qcmessage, MSG_MAYBE)
  expect_identical(dim(cal$spheredata), c(1176L, 7L))
  expect_identical(names(cal$spheredata), c("Euclidean Norm", "meanx", "meany", "meanz", "sdx", "sdy", "sdz"))
  expect_identical(rownames(cal$spheredata), rownames(Cs$spheredata))
  expect_identical(rownames(cal$spheredata)[1:3], c("6", "193", "198"))
  expect_identical(attr(cal$spheredata, "row.names"), attr(Cs$spheredata, "row.names"))
  # bsc_qc: one row per data block; the wall-clock times differ
  expect_identical(names(cal$bsc_qc), c("time", "size")); expect_identical(nrow(cal$bsc_qc), 4L)
  expect_true(is.numeric(cal$bsc_qc$size))
  expect_true(cal$applied); expect_false(cal$reset); expect_true(cal$decision_rule); expect_false(cal$check_backup)
  expect_identical(cal$source, "fit"); expect_true(cal$attempted)
  expect_identical(cal$fit, list(scale = cal$scale, offset = cal$offset, tempoffset = cal$tempoffset))
  expect_identical(cal$rows_unused, 1110)   # the final partial hour: 541110 - 540000
  expect_identical(unlist(cal$filequality[, c("filetooshort", "filecorrupt")]),
                   c(filetooshort = FALSE, filecorrupt = FALSE))
  expect_length(cal$messages, 0L)
  expect_identical(cal$settings$blocksize, 43200); expect_identical(cal$settings$sf, 30)
  expect_identical(cal$settings$sdcriter, 0.013); expect_identical(cal$settings$minloadcrit, 168)
  expect_identical(cal$file$filename, "MOS2E39230594.gt3x")
  expect_true(is.numeric(cal$elapsed) && cal$elapsed > 0)
})

test_that("P5b MOS2 chunk trace: still windows 346/757/1100/1176/1176, iterations 50/43/33/27/27, errors, rows; progress called once per pass", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  ch <- mos2_cal()$chunks
  expect_identical(names(ch), c("block", "rows", "hours", "still_windows", "iterations", "error_start",
                                "error_end", "accepted", "rows_read", "zeros_removed", "rows_carried", "qcmessage"))
  expect_identical(nrow(ch), 5L)   # 4 data blocks plus the empty read that ends a gt3x file
  expect_identical(ch$block, 1:5)
  expect_identical(ch$rows, c(1296000, 1296000, 1296000, 540000, 0))
  expect_identical(ch$rows_read, c(1296000, 1296000, 1296000, 541110, 0))
  expect_identical(ch$zeros_removed, c(0, 0, 0, 0, 0))
  expect_identical(ch$rows_carried, c(0, 0, 0, 1110, 0))
  expect_identical(ch$hours, c(12, 24, 36, 41, 41))
  expect_identical(ch$still_windows, c(346L, 757L, 1100L, 1176L, 1176L))
  expect_identical(ch$iterations, c(50L, 43L, 33L, 27L, 27L))
  expect_identical(ch$error_start, c(0.01437, 0.01497, 0.016, 0.01614, 0.01614))
  expect_identical(ch$error_end, c(0.00301, 0.00324, 0.00547, 0.0058, 0.0058))
  expect_identical(ch$accepted, rep(FALSE, 5))
  expect_identical(ch$qcmessage, rep(MSG_MAYBE, 5))
  # the 5th (empty) pass refits on the same features and reproduces the 4th
  expect_identical(ch[5, c("still_windows", "iterations", "error_start", "error_end")],
                   `rownames<-`(ch[4, c("still_windows", "iterations", "error_start", "error_end")], 5L))
  p <- memo$progress
  expect_length(p, 5L)
  expect_identical(sapply(p, `[[`, "stage"), rep("calibrate", 5))
  expect_identical(sapply(p, `[[`, "i"), 1:5)
  expect_true(all(is.na(sapply(p, `[[`, "n"))))
  expect_identical(sapply(p, `[[`, "message"), paste0("Loading chunk: ", 1:5))
})

test_that("T2 MOS2: live GGIR g.calibrate identical() to ours and to the stored C", {
  # P5a holds ours to the stored C; this adds only that the installed GGIR still gives it
  if (!canhr_flag("CANHRACTI_GGIR_SLOW")) {
    skip("set CANHRACTI_GGIR_SLOW=1 to compare with a live GGIR run on MOS2 (about 30 s)")
  }
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA()); skip_if_no_ggir()
  Cl <- mos2_live()
  expect_C_identical(as.ggir.C(mos2_cal()), Cl, "ours vs live GGIR")
  expect_C_identical(Cl, mos2_stored(), "live GGIR vs stored")
  expect_identical(nrow(Cl$bsc_qc), nrow(mos2_cal()$bsc_qc))
})

test_that("P5c decision rule: applied for MOS2; end > start, start NULL and end NULL reset to identity; a tie is applied (strict <)", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  cal <- mos2_cal()
  expect_true(cal$applied)
  check <- function(x, label) {
    d <- .raw.calibration.decision(x)
    g <- ggir_rule(as.ggir.C(x, decided = FALSE))
    expect_identical(d$scale, g$C$scale, label = paste(label, "scale"))
    expect_identical(d$offset, g$C$offset, label = paste(label, "offset"))
    expect_identical(d$tempoffset, g$C$tempoffset, label = paste(label, "tempoffset"))
    expect_identical(d$check_backup, g$check, label = paste(label, "check.backup.cal.coef"))
    expect_identical(d$reset, g$check, label = paste(label, "reset"))
    expect_true(d$decision_rule)
    d
  }
  x <- cal; x$cal_error_end <- 0.02
  d <- check(x, "end > start")
  expect_identical(d$scale, c(1, 1, 1)); expect_identical(d$offset, c(0, 0, 0)); expect_identical(d$tempoffset, c(0, 0, 0))
  expect_false(d$applied); expect_true(d$reset)
  expect_identical(d$fit, cal$fit)   # the fit itself is kept
  x <- cal; x["cal_error_start"] <- list(NULL)
  d <- check(x, "start NULL"); expect_identical(d$scale, c(1, 1, 1)); expect_false(d$applied)
  # end NULL and tempoffset NULL, as g.calibrate leaves them when nothing was fitted
  x <- cal; x["cal_error_end"] <- list(NULL); x["tempoffset"] <- list(NULL)
  d <- check(x, "end NULL"); expect_identical(d$tempoffset, c(0, 0, 0)); expect_false(d$applied)
  x <- cal; x$cal_error_end <- x$cal_error_start
  d <- check(x, "tie"); expect_identical(d$scale, cal$scale); expect_true(d$applied); expect_false(d$reset)
  d <- check(cal, "MOS2"); expect_identical(d$scale, cal$scale); expect_true(d$applied)
  # GGIR's default C: errors 0 and 0, no reset, nothing applied
  n <- .raw.calibration.object(cal_error_start = 0, cal_error_end = 0, npoints = 0, source = "not_attempted")
  d <- .raw.calibration.decision(n)
  expect_false(d$reset); expect_false(d$applied); expect_false(d$check_backup)
  expect_error(.raw.calibration.decision(list(a = 1)), "canhrActi_raw_calibration")
})

test_that("as.ggir.C: the twelve-member shape, decided = FALSE gives the last fit, the default C for a calibration that was not attempted", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  cal <- mos2_cal()
  expect_identical(as.ggir.C(cal), as.ggir.C(cal, decided = TRUE))
  x <- cal; x$cal_error_end <- 0.02; x <- .raw.calibration.decision(x)
  expect_identical(as.ggir.C(x)$scale, c(1, 1, 1))
  expect_identical(as.ggir.C(x, decided = FALSE)$scale, cal$scale)
  n <- .raw.calibration.object(cal_error_start = 0, cal_error_end = 0, npoints = 0, source = "not_attempted")
  expect_identical(as.ggir.C(n), ggir_Cdefault())
  expect_error(as.ggir.C(list()), "canhrActi_raw_calibration")
  # the print method is called directly so the test does not depend on NAMESPACE registration
  expect_output(print.canhrActi_raw_calibration(cal), "canhrActi raw calibration")
  expect_output(print.canhrActi_raw_calibration(cal), "recalibration attempted")
  expect_output(print.canhrActi_raw_calibration(cal), "still windows: 1176")
})

test_that("P5d synth_temp.csv: recalibration done, planted coefficients recovered, temperature used, meantempcal 23.07", {
  skip_if_no_ggir_ref()
  r <- synth_temp_run()
  cal <- r$cal
  expect_identical(cal$qcmessage, MSG_DONE)
  expect_true(cal$use_temp); expect_true(cal$temp_available); expect_true(cal$applied)
  expect_identical(round(cal$meantempcal, 2), 23.07)
  # the planted coefficients are recovered to a few thousandths
  expect_true(all(abs(cal$scale - c(1.03, 0.97, 1.01)) < 5e-4))
  expect_true(all(abs(cal$offset - c(0.04, -0.03, 0.02)) < 5e-3))
  expect_true(all(abs(cal$tempoffset - c(0.003, -0.002, 0.0025) * c(1.03, 0.97, 1.01)) < 1e-4))
  expect_identical(cal$cal_error_start, 0.03143); expect_identical(cal$cal_error_end, 0.00018)
  expect_identical(cal$npoints, 363L); expect_identical(cal$nhoursused, 2)
  expect_identical(dim(cal$spheredata), c(363L, 8L))
  expect_identical(names(cal$spheredata)[8], "temperature")
  expect_identical(nrow(cal$chunks), 1L)   # accepted on the first block, loading stopped
  expect_true(cal$chunks$accepted); expect_identical(cal$chunks$iterations, 16L)
  expect_identical(cal$chunks$rows, 216000); expect_identical(cal$chunks$rows_carried, 107999)
  expect_identical(cal$settings$sdcriter, 0.013 * 1.2)
  expect_C_identical(as.ggir.C(cal, decided = FALSE), synth_live(study_file("synth_temp.csv"), r$ov), "synth_temp")
})

test_that("P5d synth_consttemp.csv: temperature available but not used (sd < 0.01), NA eighth spheredata name as in GGIR", {
  skip_if_no_ggir_ref()
  r <- synth_consttemp_run()
  cal <- r$cal
  expect_identical(cal$qcmessage, MSG_DONE_NOTEMP)
  expect_false(cal$use_temp); expect_true(cal$temp_available); expect_true(cal$applied)
  expect_identical(cal$tempoffset, c(0, 0, 0)); expect_null(cal$meantempcal)
  expect_true(any(grepl("no variance in values", cal$messages, fixed = TRUE)))
  expect_identical(dim(cal$spheredata), c(363L, 8L))
  expect_identical(names(cal$spheredata)[8], NA_character_)   # GGIR names 7 columns of an 8-column frame
  expect_identical(cal$cal_error_start, 0.03143); expect_identical(cal$cal_error_end, 0.00776)
  expect_identical(cal$chunks$iterations, 29L)
  expect_C_identical(as.ggir.C(cal, decided = FALSE), synth_live(study_file("synth_consttemp.csv"), r$ov), "synth_consttemp")
})

test_that("P5d synth_notemp.csv: no temperature column, recalibration done, same coefficients as the constant-temperature file", {
  skip_if_no_ggir_ref()
  r <- synth_run(study_file("synth_notemp.csv"), tempcol = FALSE)
  cal <- r$cal
  expect_identical(cal$qcmessage, MSG_DONE)
  expect_false(cal$use_temp); expect_false(cal$temp_available); expect_true(cal$applied)
  expect_identical(dim(cal$spheredata), c(363L, 7L)); expect_null(cal$meantempcal)
  const <- synth_consttemp_run()$cal
  expect_identical(cal$scale, const$scale)
  expect_identical(cal$offset, const$offset)
  expect_C_identical(as.ggir.C(cal, decided = FALSE), synth_live(study_file("synth_notemp.csv"), r$ov), "synth_notemp")
})

test_that("P5d spherecrit 1.1: not enough points on all sides of the sphere; no fit, identity applied", {
  skip_if_no_ggir_ref()
  r <- synth_run(study_file("synth_temp.csv"), tempcol = TRUE, spherecrit = 1.1)
  cal <- r$cal
  expect_identical(cal$qcmessage, MSG_SPHERE)
  expect_identical(cal$cal_error_start, 0.03143); expect_null(cal$cal_error_end)
  expect_true("cal_error_end" %in% names(cal))
  expect_identical(cal$npoints, 363L)
  expect_identical(cal$scale, c(1, 1, 1)); expect_identical(cal$tempoffset, c(0, 0, 0))
  expect_null(cal$fit$tempoffset)   # g.calibrate leaves tempoffset empty when nothing was fitted
  expect_false(cal$applied); expect_true(cal$reset); expect_true(cal$check_backup)
  expect_identical(round(cal$meantempcal, 2), 23.07)   # still computed from the still windows
  expect_identical(nrow(cal$chunks), 2L); expect_true(all(is.na(cal$chunks$iterations)))
  live <- synth_live(study_file("synth_temp.csv"), r$ov)
  expect_null(live$tempoffset); expect_null(live$cal.error.end)
  expect_C_identical(as.ggir.C(cal, decided = FALSE), live, "spherecrit 1.1")
})

test_that("P5d rmc.noise 1e-4: no non-movement data available", {
  skip_if_no_ggir_ref()
  r <- synth_run(study_file("synth_temp.csv"), tempcol = TRUE, rmc.noise = 1e-4)
  cal <- r$cal
  expect_identical(cal$qcmessage, MSG_NO_STILL)
  expect_null(cal$cal_error_start); expect_null(cal$cal_error_end); expect_null(cal$npoints)
  expect_null(cal$spheredata); expect_null(cal$meantempcal)
  expect_identical(cal$nhoursused, 2)
  expect_false(cal$applied); expect_identical(cal$scale, c(1, 1, 1))
  expect_identical(cal$settings$sdcriter, 1e-4 * 1.2)
  expect_C_identical(as.ggir.C(cal, decided = FALSE), synth_live(study_file("synth_temp.csv"), r$ov), "rmc.noise 1e-4")
})

test_that("P5d default minloadcrit with 2 h of data: possibly not good enough, coefficients returned and applied", {
  skip_if_no_ggir_ref()
  r <- synth_run(study_file("synth_temp.csv"), tempcol = TRUE, minloadcrit = 168)
  cal <- r$cal
  expect_identical(cal$qcmessage, MSG_MAYBE)
  expect_true(cal$applied); expect_false(cal$reset)
  expect_identical(cal$nhoursused, 2)
  temp <- synth_temp_run()$cal
  expect_identical(cal$scale, temp$scale)
  expect_identical(cal$offset, temp$offset)
  expect_identical(cal$tempoffset, temp$tempoffset)
  expect_identical(nrow(cal$chunks), 2L); expect_identical(cal$chunks$accepted, c(FALSE, FALSE))
  expect_C_identical(as.ggir.C(cal, decided = FALSE), synth_live(study_file("synth_temp.csv"), r$ov), "minloadcrit 168")
})

test_that("TRUNC (corrupt gt3x, sf NULL): calibration not attempted, GGIR's default C", {
  skip_if_no_ggir_ref(); skip_if_no_file(TRUNC())
  info <- raw.inspect(TRUNC(), desiredtz = MOS2_TZ)
  expect_true(info$corrupt); expect_null(info$sf)
  cal <- raw.calibrate(info)
  expect_identical(cal$source, "not_attempted"); expect_false(cal$attempted); expect_false(cal$applied)
  expect_identical(cal$qcmessage, MSG_NOT_DONE)
  expect_identical(cal$scale, c(1, 1, 1)); expect_identical(cal$cal_error_start, 0); expect_identical(cal$npoints, 0)
  expect_identical(as.ggir.C(cal), ggir_Cdefault())
  expect_identical(nrow(cal$chunks), 0L)
  expect_true(any(grepl("corrupt", cal$messages)))
  expect_output(print.canhrActi_raw_calibration(cal), "source:    not_attempted")
})

test_that("do.cal = FALSE and a skipped inspection: not attempted without reading", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(SHORT())
  info <- mos2_info()
  cal <- raw.calibrate(info, do.cal = FALSE)
  expect_identical(cal$source, "not_attempted"); expect_identical(cal$qcmessage, MSG_NOT_DONE)
  expect_false(cal$settings$do.cal); expect_true(cal$elapsed < 5)
  expect_identical(as.ggir.C(cal), ggir_Cdefault())
  sk <- suppressWarnings(raw.inspect(SHORT(), desiredtz = MOS2_TZ, skip_small_files = TRUE))
  expect_true(sk$skipped)
  cal <- raw.calibrate(sk)
  expect_identical(cal$source, "not_attempted"); expect_true(any(grepl("skipped", cal$messages)))
})

test_that("SHORT and the GGIRread test files (all under 2 h): not enough data, identical() to live GGIR", {
  skip_if_no_ggir_ref()
  files <- list(list(SHORT(), MOS2_TZ, NULL),
                list(file.path(ggirread_tf(), "GENEActiv_testfile.bin"), OTHER_TZ, "GGIRread"),
                list(file.path(ggirread_tf(), "ax3_testfile.cwa"), OTHER_TZ, "GGIRread"),
                list(file.path(ggirread_tf(), "ax6_testfile.cwa"), OTHER_TZ, "GGIRread"),
                list(file.path(ggirread_tf(), "mtx_100Hz_acc_HR_temp.BIN"), OTHER_TZ, "GGIRread"))
  for (f in files) {
    if (!file.exists(f[[1]])) next
    if (!is.null(f[[3]]) && !requireNamespace(f[[3]], quietly = TRUE)) next
    label <- basename(f[[1]])
    info <- suppressWarnings(raw.inspect(f[[1]], desiredtz = f[[2]]))
    cal <- suppressWarnings(raw.calibrate(info))
    expect_identical(cal$qcmessage, MSG_NOT_ENOUGH, label = label)
    ok <- c(attempted = isTRUE(cal$attempted), not_applied = isFALSE(cal$applied),
            cal_error_start_null = is.null(cal$cal_error_start),
            cal_error_end_null = is.null(cal$cal_error_end), npoints_null = is.null(cal$npoints),
            spheredata_null = is.null(cal$spheredata),
            filetooshort = isTRUE(cal$filequality$filetooshort),
            not_filecorrupt = isFALSE(cal$filequality$filecorrupt))
    expect_identical(names(ok)[!ok], character(0), info = paste0(label, ": ", toString(names(ok)[!ok])))
    expect_identical(cal$nhoursused, 0, label = label)
    expect_identical(nrow(cal$chunks), 1L, label = label); expect_identical(cal$chunks$rows, 0, label = label)
    expect_identical(as.ggir.C(cal, decided = FALSE)$tempoffset, NULL)
    if (requireNamespace("GGIR", quietly = TRUE)) {
      expect_C_identical(as.ggir.C(cal, decided = FALSE), ggir_calibrate(f[[1]], f[[2]]), label)
    }
  }
})

test_that("supplied canhrActi object and GGIR C list: used as given, no refit, as.ggir.C round trip", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA())
  info <- mos2_info(); cal <- mos2_cal(); Cs <- mos2_stored()
  r <- raw.calibrate(info, calibration = cal)
  expect_identical(r$source, "supplied"); expect_identical(r$supplied_from, "canhrActi_raw_calibration")
  expect_false(r$attempted); expect_identical(nrow(r$chunks), 0L); expect_true(r$elapsed < 5)
  expect_identical(r$scale, cal$scale); expect_true(r$applied)
  expect_true(any(grepl("supplied", r$messages)))
  r <- raw.calibrate(info, calibration = Cs)
  expect_identical(r$supplied_from, "ggir_C"); expect_false(r$attempted)
  expect_identical(as.ggir.C(r), Cs)   # including bsc_qc, carried as is
  expect_true(r$applied); expect_false(r$decision_rule); expect_true(r$check_backup)
  expect_identical(r$npoints, 1176L); expect_identical(r$qcmessage, MSG_MAYBE)
})

test_that("supplied data_quality_report row and csv path: 15-digit coefficients, meantempcal dropped, filename matched with meta_/.RData stripping", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_file(MOS2_RDATA()); skip_if_no_file(MOS2_QC_CSV())
  info <- mos2_info(); Cs <- mos2_stored()
  qc <- utils::read.csv(MOS2_QC_CSV(), stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(all(c("filename", "scale.x", "offset.z", "n.10sec.windows", "n.hours.considered", "use.temperature") %in% names(qc)))
  expect_false("meantempcal" %in% names(qc))
  r <- raw.calibrate(info, calibration = qc)
  expect_identical(r$source, "supplied"); expect_identical(r$supplied_from, "data.frame"); expect_false(r$attempted)
  # the csv holds 15 significant digits, so not the same bits as the stored double
  expect_true(all(abs(r$scale - Cs$scale) < 1e-13)); expect_false(identical(r$scale, Cs$scale))
  expect_true(all(abs(r$offset - Cs$offset) < 1e-13))
  expect_identical(r$tempoffset, c(0, 0, 0))
  expect_null(r$meantempcal); expect_true("meantempcal" %in% names(r))
  expect_identical(r$cal_error_start, 0.01614); expect_identical(r$cal_error_end, 0.0058)
  expect_identical(as.numeric(r$npoints), 1176); expect_identical(as.numeric(r$nhoursused), 41)
  expect_identical(r$qcmessage, MSG_MAYBE); expect_identical(r$use_temp, FALSE)
  expect_true(r$applied); expect_false(r$decision_rule)
  C <- as.ggir.C(r)
  expect_identical(names(C), c("cal.error.end", "cal.error.start", "scale", "offset", "tempoffset",
                               "QCmessage", "npoints", "nhoursused", "use.temp", "meantempcal"))
  expect_null(C$meantempcal); expect_true("meantempcal" %in% names(C))
  # the same through a csv path and through backup.cal.coef
  r2 <- raw.calibrate(info, calibration = MOS2_QC_CSV())
  expect_identical(r2$supplied_from, "csv")
  expect_equal(r2$scale, r$scale, tolerance = 1e-12); expect_equal(r2$offset, r$offset, tolerance = 1e-12)
  expect_identical(r2$qcmessage, r$qcmessage)
  r3 <- raw.calibrate(info, backup.cal.coef = MOS2_QC_CSV())
  expect_identical(r3$supplied_from, "csv"); expect_equal(r3$scale, r$scale, tolerance = 1e-12)
  # GGIR's filename normalisation
  q2 <- qc; q2$filename <- "meta_MOS2E39230594.gt3x.RData"
  expect_identical(.raw.calibration.from.table(q2, "MOS2E39230594.gt3x")$scale, r$scale)
  expect_null(.raw.calibration.from.table(qc, "other.gt3x"))
  one <- qc[1, setdiff(names(qc), "filename")]
  expect_identical(.raw.calibration.from.table(one, "anything")$scale, r$scale)
  expect_error(raw.calibrate(info, calibration = 42), "calibration must be")
  expect_error(raw.calibrate(info, calibration = file.path(tempdir(), "nope.csv")), "calibration must be")
  expect_error(raw.calibrate(info, calibration = qc[, c("filename", "scale.x")]), "needs the columns")
})

test_that("a table without a row for the file: fit made, GGIR's rule bypassed under ggir_exact and applied under ggir_exact = FALSE; do.cal FALSE gives the default", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2_QC_CSV())
  f <- study_file("synth_temp.csv"); skip_if_no_file(f); testthat::skip_if_not_installed("data.table")
  qc <- utils::read.csv(MOS2_QC_CSV(), stringsAsFactors = FALSE, check.names = FALSE)
  ov <- synth_params(TRUE); params <- do.call(raw.params, ov); info <- raw.inspect(f, params = params)
  exact <- raw.calibrate(info, params = params, calibration = qc)
  expect_identical(exact$source, "fit"); expect_true(exact$attempted)
  expect_false(exact$decision_rule); expect_true(exact$check_backup); expect_true(exact$applied)
  expect_true(any(grepl("No row for this file", exact$messages)))
  expect_identical(exact$qcmessage, MSG_DONE)
  fixed <- raw.calibrate(info, params = params, calibration = qc, ggir_exact = FALSE)
  expect_true(fixed$decision_rule); expect_true(fixed$applied); expect_false(fixed$reset)
  expect_identical(fixed$scale, exact$scale)
  off <- raw.calibrate(info, params = params, calibration = qc, do.cal = FALSE)
  expect_identical(off$source, "not_attempted")
})

test_that("parameter resolution: info$params by default, overrides routed through raw.params, unknown names rejected", {
  skip_if_no_ggir_ref()
  f <- study_file("synth_temp.csv"); skip_if_no_file(f); testthat::skip_if_not_installed("data.table")
  ov <- synth_params(TRUE); params <- do.call(raw.params, ov); info <- raw.inspect(f, params = params)
  a <- raw.calibrate(info)                       # info$params carries minloadcrit 1 and the csv layout
  expect_identical(a$settings$minloadcrit, 1); expect_identical(a$qcmessage, MSG_DONE)
  b <- raw.calibrate(info, minloadcrit = 168)   # an override on top of info$params
  expect_identical(b$settings$minloadcrit, 168); expect_identical(b$qcmessage, MSG_MAYBE)
  expect_identical(b$scale, a$scale)
  expect_error(raw.calibrate(info, bogus = 1), "unknown")
  expect_error(raw.calibrate(info, params = "x"), "params must be a list")
  expect_error(raw.calibrate(list(sf = 30)), "canhrActi_raw_info")
  expect_error(raw.calibrate(info, progress = "no"), "progress must be")
})

test_that("EE (long): every field identical() to the stored C; exactly 168 h fails the strict hour gate but is applied", {
  skip_unless_long(); skip_if_no_ggir_ref(); skip_if_no_file(EE()); skip_if_no_file(EE_RDATA())
  e <- new.env(); load(EE_RDATA(), envir = e); Cs <- e$C
  info <- raw.inspect(EE(), desiredtz = "Europe/Helsinki")
  cal <- raw.calibrate(info)
  expect_C_identical(as.ggir.C(cal), Cs, "EE vs stored")
  expect_identical(cal$scale, c(0.98856923645089312, 1.00368990557098225, 0.99531980902069050))
  expect_identical(cal$offset, c(-0.016537344594794678, 0.015828129597196549, 0.018515787607574781))
  expect_identical(cal$cal_error_start, 0.01428); expect_identical(cal$cal_error_end, 0.00629)
  expect_identical(cal$npoints, 29490L); expect_identical(cal$nhoursused, 168)
  expect_identical(cal$qcmessage, MSG_MAYBE); expect_true(cal$applied)
  expect_identical(nrow(cal$chunks), 15L); expect_identical(nrow(cal$bsc_qc), 14L)
})
