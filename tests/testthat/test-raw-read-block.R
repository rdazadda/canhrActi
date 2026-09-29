# .raw.read.block and the block-size helpers against GGIR's g.readaccfile, on the MOS2 and
# SHORT gt3x files, the GGIRread test files and synthetic csv layouts. Reference data come
# from CANHRACTI_GGIR_REF; tests skip when a file is missing and live comparisons when GGIR
# is not installed. Each MOS2 read unzips the 22 MB archive (3 to 4 s); the EE test runs only
# when CANHRACTI_LONG_TESTS is set.

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
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
EE <- function() ref_file("EE_left_29.5.2017-05-30.gt3x")
SHORT <- function() ref_file("failmodes", "din", "tooshort.gt3x")
ggirread_tf <- function() {
  clone <- file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIRread", "inst", "testfiles")
  if (dir.exists(clone)) return(clone)
  system.file("testfiles", package = "GGIRread")
}

MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to
OTHER_TZ <- "Europe/London"      # what the study used for the GGIRread test files

# GGIR parameter objects as the study built them, plus rawdata overrides
ggir_params <- function(desiredtz, ...) {
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- desiredtz
  P$params_general[["windowsizes"]] <- c(5, 900, 3600)
  ov <- list(...)
  for (nm in names(ov)) P$params_rawdata[nm] <- list(ov[[nm]])
  P
}
ggir_fq <- function() data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                                 filedoesnotholdday = FALSE, NFilePagesSkipped = 0)
ggir_read <- function(file, I, blocksize, blocknumber, fq, ws, PreviousEndPage, P,
                      PreviousLastValue = c(0, 0, 1), PreviousLastTime = NULL, header = NULL) {
  GGIR::g.readaccfile(filename = file, blocksize = blocksize, blocknumber = blocknumber,
                      filequality = fq, ws = ws, PreviousEndPage = PreviousEndPage,
                      inspectfileobject = I,
                      PreviousLastValue = PreviousLastValue, PreviousLastTime = PreviousLastTime,
                      params_rawdata = P$params_rawdata, params_general = P$params_general,
                      header = header)
}

# First differing position between two data.frames (or vectors), for failure messages
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
        return(sprintf("column %s first differs at row %d: ours %.17g ref %.17g", nm, i[1],
                       as.numeric(a[[nm]][i[1]]), as.numeric(b[[nm]][i[1]])))
      }
    }
    return(paste0("columns identical; attributes differ: ours ", paste(names(attributes(a)), collapse = ","),
                  " ref ", paste(names(attributes(b)), collapse = ",")))
  }
  i <- which(a != b)
  if (length(i)) sprintf("first differs at %d: ours %.17g ref %.17g", i[1], as.numeric(a[i[1]]), as.numeric(b[i[1]])) else "no elementwise difference"
}

expect_block_identical <- function(ours, ref, label = "") {
  testthat::expect(identical(ours$data, ref$P$data),
                   paste0(label, " data not identical: ", first_diff(ours$data, ref$P$data)))
  testthat::expect_identical(ours$filequality, ref$filequality, label = paste(label, "filequality"))
  testthat::expect_identical(ours$is_last_block, ref$isLastBlock, label = paste(label, "isLastBlock"))
  testthat::expect_identical(ours$startpage, ref$startpage, label = paste(label, "startpage"))
  testthat::expect_identical(ours$endpage, ref$endpage, label = paste(label, "endpage"))
  testthat::expect_identical(.raw.ggir.accread(ours), ref, label = paste(label, "whole GGIR-shaped result"))
}

memo <- new.env()

mos2_info <- function() {
  if (is.null(memo$info)) {
    memo$info <- raw.inspect(MOS2(), desiredtz = MOS2_TZ)
    if (requireNamespace("GGIR", quietly = TRUE)) {
      memo$I <- GGIR::g.inspectfile(MOS2(), desiredtz = MOS2_TZ)
    }
  }
  memo$info
}

# The whole block loop of one pass, memoised: ours, and GGIR's when installed, with the state
# carried as g.getmeta and g.calibrate carry it.
mos2_pass <- function(pass = c("getmeta", "calibrate"), n_blocks) {
  pass <- match.arg(pass)
  key <- paste0("pass_", pass)
  if (!is.null(memo[[key]])) return(memo[[key]])
  info <- mos2_info()
  blocksize <- .raw.blocksize(info, pass)
  ws <- 3600
  ours <- vector("list", n_blocks)
  fq <- .raw.filequality(); pep <- c(); plv <- c(0, 0, 1); plt <- NULL
  for (i in seq_len(n_blocks)) {
    ours[[i]] <- .raw.read.block(info, blocksize = blocksize, blocknumber = i, previous_end_page = pep,
                                 ws = ws, params = info$params, previous_last_value = plv,
                                 previous_last_time = plt, filequality = fq, header = NULL)
    fq <- ours[[i]]$filequality; pep <- ours[[i]]$endpage
    plv <- ours[[i]]$previous_last_value; plt <- ours[[i]]$previous_last_time
  }
  ref <- NULL
  if (requireNamespace("GGIR", quietly = TRUE)) {
    P <- ggir_params(MOS2_TZ)
    ref <- vector("list", n_blocks)
    fq <- ggir_fq(); pep <- c(); plv <- c(0, 0, 1); plt <- NULL; header <- NULL
    for (i in seq_len(n_blocks)) {
      ref[[i]] <- ggir_read(MOS2(), memo$I, blocksize, i, fq, ws, pep, P, plv, plt, header)
      header <- ref[[i]]$header
      if ("PreviousLastValue" %in% names(ref[[i]]$P)) {
        plv <- ref[[i]]$P$PreviousLastValue; plt <- ref[[i]]$P$PreviousLastTime
      }
      fq <- ref[[i]]$filequality; pep <- ref[[i]]$endpage
    }
  }
  memo[[key]] <- list(ours = ours, ref = ref, blocksize = blocksize)
  memo[[key]]
}

# numeric Unix seconds of a wall-clock time in a timezone
utime <- function(txt, tz) as.numeric(as.POSIXct(txt, tz = tz))
# absolute tolerance; expect_equal's is relative, useless on Unix seconds
expect_close <- function(a, b, tol = 1e-6, label = "") {
  testthat::expect(abs(a - b) < tol, sprintf("%s |%.17g - %.17g| = %.3g not < %g", label, a, b, abs(a - b), tol))
}

test_that("blocksize: MOS2 86400 (getmeta) and 43200 (calibrate), equal to raw.inspect and GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  info <- mos2_info()
  expect_identical(.raw.blocksize(info, "getmeta"), 86400)
  expect_identical(.raw.blocksize(info, "calibrate"), 43200)
  expect_identical(.raw.blocksize(info, "getmeta"), info$blocksize_getmeta)
  expect_identical(.raw.blocksize(info, "calibrate"), info$blocksize_calibrate)
  expect_identical(.raw.blocksize(info, "getmeta", chunksize = 0.5), 43200)
  expect_identical(.raw.blocksize(info, "calibrate", chunksize = 0.5), 21600)
  skip_if_no_ggir()
  hv <- GGIR:::g.extractheadervars(memo$I)
  pr <- ggir_params(MOS2_TZ)$params_rawdata
  expect_identical(.raw.blocksize(info, "getmeta"),
                   GGIR:::get_nw_clip_block_params(memo$I$monc, memo$I$dformc, hv$deviceSerialNumber, memo$I$sf, pr)$blocksize)
  expect_identical(.raw.blocksize(info, "calibrate"), (12 * 3600) * pr[["chunksize"]])
})

test_that("blocksize: GENEActiv, cwa and Parmay test files match GGIR's two formulas", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  tf <- ggirread_tf()
  files <- c(GENEActiv_testfile.bin = "GENEActiv", ax3_testfile.cwa = "cwa", mtx_100Hz_acc_HR_temp.BIN = "Parmay")
  expected <- list(GENEActiv_testfile.bin = c(24961, 12480), ax3_testfile.cwa = c(108000, 54000),
                   mtx_100Hz_acc_HR_temp.BIN = c(720, 360))
  pr <- ggir_params(OTHER_TZ)$params_rawdata
  for (fn in names(files)) {
    f <- file.path(tf, fn)
    skip_if_no_file(f)
    info <- suppressWarnings(raw.inspect(f, desiredtz = OTHER_TZ))
    I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = OTHER_TZ))
    hv <- GGIR:::g.extractheadervars(I)
    expect_identical(.raw.blocksize(info, "getmeta"),
                     GGIR:::get_nw_clip_block_params(I$monc, I$dformc, hv$deviceSerialNumber, I$sf, pr)$blocksize,
                     label = paste(fn, "getmeta"))
    expect_equal(.raw.blocksize(info, "getmeta"), expected[[fn]][1], label = paste(fn, "getmeta value"))
    expect_equal(.raw.blocksize(info, "calibrate"), expected[[fn]][2], label = paste(fn, "calibrate value"))
  }
})

test_that("update.blocksize reproduces updateBlocksize's log and arithmetic", {
  empty <- data.frame(time = c(), size = c())
  u1 <- .raw.update.blocksize(86400, empty)
  expect_named(u1, c("blocksize", "bsc_qc"))
  expect_identical(names(u1$bsc_qc), c("time", "size"))
  expect_identical(nrow(u1$bsc_qc), 1L)
  expect_type(u1$bsc_qc$time, "character")
  expect_true(is.numeric(u1$bsc_qc$size))
  # shrink by 0.8 only above 4000 MB
  expect_identical(u1$blocksize, if (u1$bsc_qc$size > 4000) round(86400 * 0.8) else 86400)
  u2 <- .raw.update.blocksize(u1$blocksize, u1$bsc_qc)
  expect_identical(nrow(u2$bsc_qc), 2L)
  expect_identical(u2$bsc_qc[1, ], u1$bsc_qc[1, ])
  expect_identical(.raw.update.blocksize(100.4, empty)$blocksize, if (u1$bsc_qc$size > 4000) 80 else 100)
  # GGIR warns and then fails in round(NULL); both are reproduced
  expect_warning(try(.raw.update.blocksize(c(), empty), silent = TRUE), "Blocksize is zero, please contact maintainers")
  expect_error(suppressWarnings(.raw.update.blocksize(c(), empty)), "non-numeric argument")
  skip_if_no_ggir()
  g <- GGIR:::updateBlocksize(86400, empty)
  expect_identical(names(g), names(u1))
  expect_identical(names(g$bsc_qc), names(u1$bsc_qc))
  expect_identical(sapply(g$bsc_qc, class), sapply(u1$bsc_qc, class))
  expect_identical(class(g$blocksize), class(u1$blocksize))
})

test_that("P2a getmeta pass: blocks 1, 2, 3 of MOS2 are identical() to GGIR:::g.readaccfile", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_ggir()
  r <- mos2_pass("getmeta", 3)
  expect_identical(r$blocksize, 86400)
  for (i in 1:3) expect_block_identical(r$ours[[i]], r$ref[[i]], label = paste("getmeta block", i))
  expect_identical(dim(r$ours[[1]]$data), c(2592000L, 4L))
  expect_identical(dim(r$ours[[2]]$data), c(1837110L, 4L))
  expect_null(r$ours[[3]]$data)
  expect_identical(names(r$ours[[1]]$data), c("time", "x", "y", "z"))
  expect_identical(c(r$ours[[1]]$startpage, r$ours[[1]]$endpage), c(1, 86400))
  expect_identical(c(r$ours[[2]]$startpage, r$ours[[2]]$endpage), c(86401, 172800))
  expect_identical(c(r$ours[[3]]$startpage, r$ours[[3]]$endpage), c(172801, 259200))
  expect_identical(sapply(r$ours, `[[`, "is_last_block"), c(FALSE, FALSE, TRUE))
  # time is Unix seconds relabelled to America/Anchorage; block 2 continues at the next sample
  d1 <- r$ours[[1]]$data; d2 <- r$ours[[2]]$data
  expect_true(is.numeric(d1$time))
  expect_identical(d1$time[1], 1759897684)
  expect_identical(d1$time[1], utime("2025-10-07 20:28:04", MOS2_TZ))
  expect_close(d1$time[nrow(d1)], utime("2025-10-09 19:06:05", MOS2_TZ) + 29 / 30, 1e-6, "block 1 last time")
  expect_close(d2$time[1], utime("2025-10-09 19:06:06", MOS2_TZ), 1e-6, "block 2 first time")
  expect_identical(unname(unlist(d1[1, c("x", "y", "z")])), c(0.746, 0.008, -0.707))
  expect_identical(unname(unlist(d1[nrow(d1), c("x", "y", "z")])), c(-0.113, 1.004, 0.137))
  expect_identical(sum(diff(d1$time) > 1.5 / 30), 915L)
  expect_identical(sum(diff(d2$time) > 1.5 / 30), 525L)
  expect_identical(sum(d1$x == 0 & d1$y == 0 & d1$z == 0), 0L)
  expect_identical(class(d1), c("activity_df", "data.frame"))
  # no reader state and no header for gt3x
  for (i in 1:3) {
    expect_identical(r$ours[[i]]$filequality, .raw.filequality())
    expect_null(r$ours[[i]]$header)
    expect_null(r$ours[[i]]$qclog)
    expect_identical(r$ours[[i]]$previous_last_value, c(0, 0, 1))
    expect_null(r$ours[[i]]$previous_last_time)
  }
  # the read past the end is the silent end-of-file error
  expect_identical(r$ours[[3]]$messages, character(0))
  # GGIR's emptied P is NULL
  expect_null(r$ours[[3]]$P)
  # free GGIR's copies
  memo$pass_getmeta$ref <- NULL
})

test_that("P2a calibrate pass: three blocks of 1,296,000 rows, then 541,110, then empty, identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2()); skip_if_no_ggir()
  r <- mos2_pass("calibrate", 5)
  expect_identical(r$blocksize, 43200)
  for (i in 1:5) expect_block_identical(r$ours[[i]], r$ref[[i]], label = paste("calibrate block", i))
  rows <- sapply(r$ours, function(b) if (is.null(b$data)) 0L else nrow(b$data))
  expect_identical(rows, c(1296000L, 1296000L, 1296000L, 541110L, 0L))
  expect_identical(sum(rows), 4429110L)
  expect_identical(sapply(r$ours, `[[`, "startpage"), c(1, 43201, 86401, 129601, 172801))
  expect_identical(sapply(r$ours, `[[`, "endpage"), c(43200, 86400, 129600, 172800, 216000))
  expect_identical(sapply(r$ours, `[[`, "is_last_block"), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # the 12 h blocks tile the 24 h blocks exactly
  g <- mos2_pass("getmeta", 3)$ours
  expect_identical(rbind(r$ours[[1]]$data, r$ours[[2]]$data)$time, g[[1]]$data$time)
  expect_identical(r$ours[[3]]$data$x, g[[2]]$data$x[1:1296000])
  expect_identical(r$ours[[4]]$data$z, g[[2]]$data$z[1296001:1837110])
  expect_identical(r$ours[[4]]$data$time, g[[2]]$data$time[1296001:1837110])
  memo$pass_calibrate$ref <- NULL
})

test_that("P2b batch vs full: the getmeta blocks tile read.gt3x::read.gt3x(MOS2) rows 1..4,429,110", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  g <- mos2_pass("getmeta", 3)$ours
  full <- read.gt3x::read.gt3x(MOS2(), asDataFrame = TRUE)
  expect_identical(nrow(full), 4429110L)
  both <- rbind(g[[1]]$data, g[[2]]$data)
  expect_identical(nrow(both), 4429110L)
  expect_identical(both$x, full$X)
  expect_identical(both$y, full$Y)
  expect_identical(both$z, full$Z)
  # the block reader relabels the GMT digits to configtz and makes the time numeric
  expect_identical(both$time, as.numeric(lubridate::force_tz(full$time, MOS2_TZ)))
  rm(full, both)
})

test_that("P2d unzip once: read.gt3x on the extracted directory is identical() to the archive read", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  loc <- file.path(gsub("\\\\", "/", tempfile("rawblock_unzip")))
  ex <- .raw.gt3x.extract(SHORT(), location = loc)
  expect_true(dir.exists(ex))
  expect_true(all(c("info.txt", "log.bin") %in% list.files(ex)))
  # the directory is reused, not re-extracted
  mt <- file.info(file.path(ex, "log.bin"))$mtime
  expect_identical(.raw.gt3x.extract(SHORT(), location = loc), ex)
  expect_identical(file.info(file.path(ex, "log.bin"))$mtime, mt)
  a <- read.gt3x::read.gt3x(SHORT(), batch_begin = 1, batch_end = 100, asDataFrame = TRUE)
  b <- read.gt3x::read.gt3x(ex, batch_begin = 1, batch_end = 100, asDataFrame = TRUE)
  expect_identical(b, a)
  expect_identical(.raw.gt3x.extract(ex), ex)
  .raw.gt3x.extract.cleanup(SHORT(), location = loc)
  expect_false(dir.exists(ex))
  unlink(loc, recursive = TRUE)
})

test_that("P2d unzip once: MOS2 blocks 1 to 3 through unzip_once = TRUE are identical() to GGIR's archive read", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  info <- mos2_info()
  # GGIR's own call on the archive: both gt3x options off, whatever raw.params() defaults to
  g <- list(); pep <- c(); fq <- .raw.filequality()
  for (i in 1:3) {
    g[[i]] <- .raw.read.block(info, 86400, i, pep, 3600, params = info$params, filequality = fq,
                              unzip_once = FALSE, stream_gt3x = FALSE)
    pep <- g[[i]]$endpage; fq <- g[[i]]$filequality
  }
  .raw.gt3x.extract.cleanup(MOS2())
  b1 <- .raw.read.block(info, 86400, 1, c(), 3600, params = info$params, unzip_once = TRUE,
                        stream_gt3x = FALSE)
  ex <- .raw.gt3x.extract(MOS2())
  expect_true(file.exists(file.path(ex, "log.bin")))
  b2 <- .raw.read.block(info, 86400, 2, b1$endpage, 3600, params = info$params, filequality = b1$filequality,
                        unzip_once = TRUE, stream_gt3x = FALSE)
  b3 <- .raw.read.block(info, 86400, 3, b2$endpage, 3600, params = info$params, filequality = b2$filequality,
                        unzip_once = TRUE, stream_gt3x = FALSE)
  for (i in 1:3) {
    o <- list(b1, b2, b3)[[i]]
    expect(identical(o$data, g[[i]]$data), paste0("unzip_once block ", i, ": ", first_diff(o$data, g[[i]]$data)))
    expect_identical(o$P, g[[i]]$P)
    expect_identical(o[c("filequality", "is_last_block", "endpage", "startpage", "messages")],
                     g[[i]][c("filequality", "is_last_block", "endpage", "startpage", "messages")])
  }
  # read.gt3x deletes only its own tempdir extraction
  expect_true(file.exists(file.path(ex, "log.bin")))
  .raw.gt3x.extract.cleanup(MOS2())
  expect_false(dir.exists(ex))
})

test_that("P2e asDataFrame = FALSE: start_time + time_index/100 is identical() to the data.frame time, before and after force_tz", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  m <- read.gt3x::read.gt3x(SHORT(), batch_begin = 1, batch_end = 200, asDataFrame = FALSE)
  d <- read.gt3x::read.gt3x(SHORT(), batch_begin = 1, batch_end = 200, asDataFrame = TRUE)
  tn <- as.numeric(attr(m, "start_time")) + attr(m, "time_index") / 100L
  expect_identical(tn, as.numeric(d$time))
  expect_identical(attr(d$time, "tzone"), "GMT")
  for (tz in c(MOS2_TZ, "Europe/Helsinki", "")) {
    expect_identical(as.numeric(lubridate::force_tz(as.POSIXct(tn, origin = "1970-01-01", tz = "GMT"), tz)),
                     as.numeric(lubridate::force_tz(d$time, tz)), label = paste("after force_tz to", tz))
  }
  # across the autumn DST change in both study timezones
  for (tz in c(MOS2_TZ, "Europe/Helsinki")) {
    base <- as.numeric(as.POSIXct(if (tz == MOS2_TZ) "2025-11-02 00:30:00" else "2025-10-26 02:30:00", tz = "GMT"))
    syn <- base + seq(0, 3 * 3600, by = 0.01)
    expect_identical(as.numeric(lubridate::force_tz(as.POSIXct(syn, origin = "1970-01-01", tz = "GMT"), tz)),
                     as.numeric(lubridate::force_tz(.POSIXct(syn, tz = "GMT"), tz)), label = paste("DST", tz))
  }
  # the batch helper itself
  h <- .raw.gt3x.read.batch(SHORT(), 1, 200, as_data_frame_false = TRUE)
  expect_identical(h$time, d$time)
  expect_identical(h$X, d$X); expect_identical(h$Y, d$Y); expect_identical(h$Z, d$Z)
  expect_identical(class(h), class(d))
})

test_that(".raw.force.tz is identical() to lubridate::force_tz across spring-forward and fall-back", {
  # sample times as the gt3x readers build them: device clock time labelled GMT
  block <- function(clock, minutes, sf) {
    s0 <- as.numeric(as.POSIXct(clock, tz = "GMT"))
    sec <- rep(0:(minutes * 60 - 1), each = sf)
    j <- rep(0:(sf - 1), minutes * 60)
    .POSIXct(s0 + ((sec + j * (1 / sf)) * 100) / 100, tz = "GMT")
  }
  # the fast path makes no force_tz call on the whole block
  real_force_tz <- lubridate::force_tz
  longest <- 0L
  local_mocked_bindings(force_tz = function(time, ...) {
    longest <<- max(longest, length(time))
    real_force_tz(time, ...)
  }, .package = "lubridate")
  same <- function(tt, tz) {
    longest <<- 0L
    ours <- .raw.force.tz(tt, tz)
    fast <- longest < length(tt)
    ref <- real_force_tz(tt, tz)
    list(identical = identical(ours, ref), fast = fast)
  }
  # the clock time of each change and how long its skipped or repeated interval is
  zones <- list(
    list(tz = "America/Anchorage", spring = "2025-03-09 02:00:00", fall = "2025-11-02 01:00:00", len = 60),
    list(tz = "Europe/Amsterdam", spring = "2025-03-30 02:00:00", fall = "2025-10-26 02:00:00", len = 60),
    list(tz = "Australia/Sydney", spring = "2025-10-05 02:00:00", fall = "2025-04-06 02:00:00", len = 60),
    list(tz = "Australia/Lord_Howe", spring = "2025-10-05 02:00:00", fall = "2025-04-06 01:30:00", len = 30),
    list(tz = "UTC", spring = "2025-03-09 02:00:00", fall = "2025-11-02 01:00:00", len = 0))
  at <- function(clock, minutes) format(as.POSIXct(clock, tz = "GMT") + minutes * 60, "%Y-%m-%d %H:%M:%S", tz = "GMT")
  paths <- character()
  for (z in zones) {
    for (change in c("spring", "fall")) {
      t0 <- z[[change]]
      w <- list(before = c(-150, 60), across = c(-30, z$len + 60), after = c(z$len + 30, 60))
      if (z$len > 0) w$inside <- c(z$len / 4, z$len / 2)
      for (nm in names(w)) {
        for (sf in c(30, 80)) {
          r <- same(block(at(t0, w[[nm]][1]), w[[nm]][2], sf), z$tz)
          expect_true(r$identical, label = paste(z$tz, change, nm, sf, "Hz"))
          paths[paste(z$tz, change, nm, sf)] <- if (r$fast) "fast" else "force_tz"
        }
      }
    }
  }
  # a block with a skipped or split interval goes through force_tz; so do the skipped times
  dst <- names(paths)[!startsWith(names(paths), "UTC")]
  expect_true(all(paths[dst[grepl(" across ", dst)]] == "force_tz"))
  expect_true(all(paths[grepl("spring inside", names(paths))] == "force_tz"))
  # the repeated interval maps to one offset (roll_dst "post"), so it takes the fast path
  expect_true(all(paths[grepl("fall inside", names(paths))] == "fast"))
  expect_true(all(paths[grepl(" before | after ", names(paths))] == "fast"))
  expect_true(all(paths[startsWith(names(paths), "UTC")] == "fast"))
  expect_length(paths, 76L)
  expect_identical(sum(paths == "force_tz"), 24L)
  # random second fractions, input labels and output zones, including the machine zone ("")
  set.seed(11)
  for (i in 1:40) {
    s0 <- stats::runif(1, -2e8, 2.2e9)
    v <- floor(s0) + sort(c(stats::runif(300, 0, 4), 1 - 2^-40, 2, 3 - 2^-30))
    from <- sample(c("GMT", "UTC", "America/Anchorage", "Asia/Kolkata"), 1)
    to <- sample(c("America/Anchorage", "Europe/Amsterdam", "Australia/Sydney", "UTC",
                   "America/St_Johns", "Pacific/Chatham", ""), 1)
    expect_true(same(.POSIXct(v, tz = from), to)$identical, label = paste("random", i, from, to))
  }
  # NA, a single sample and a zero-length block go through force_tz
  tt <- block("2025-06-01 00:00:00", 1, 30)
  tt[5] <- NA
  expect_identical(.raw.force.tz(tt, "Europe/Amsterdam"), real_force_tz(tt, "Europe/Amsterdam"))
  expect_identical(.raw.force.tz(tt[1], "Europe/Amsterdam"), real_force_tz(tt[1], "Europe/Amsterdam"))
  expect_identical(.raw.force.tz(tt[0], "Europe/Amsterdam"), real_force_tz(tt[0], "Europe/Amsterdam"))
})

test_that("P2e asDataFrame = FALSE: MOS2 blocks through as_data_frame_false = TRUE are identical() to the default path", {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  info <- mos2_info()
  g <- mos2_pass("getmeta", 3)$ours
  b1 <- .raw.read.block(info, 86400, 1, c(), 3600, params = info$params, as_data_frame_false = TRUE)
  expect(identical(b1$data, g[[1]]$data), paste0("as_data_frame_false block 1: ", first_diff(b1$data, g[[1]]$data)))
  expect_identical(.raw.ggir.accread(b1), .raw.ggir.accread(g[[1]]))
  # both options together, block 2
  b2 <- .raw.read.block(info, 86400, 2, b1$endpage, 3600, params = info$params, filequality = b1$filequality,
                        as_data_frame_false = TRUE, unzip_once = TRUE)
  expect(identical(b2$data, g[[2]]$data), paste0("both options block 2: ", first_diff(b2$data, g[[2]]$data)))
  expect_identical(.raw.ggir.accread(b2), .raw.ggir.accread(g[[2]]))
  .raw.gt3x.extract.cleanup(MOS2())
})

test_that("P2f SHORT: 33,000 rows read, too short, not corrupt, data discarded, identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  info <- raw.inspect(SHORT(), desiredtz = MOS2_TZ)
  expect_identical(info$sf, 100)
  expect_identical(info$blocksize_getmeta, 86400)
  expect_identical(nrow(read.gt3x::read.gt3x(SHORT(), batch_begin = 1, batch_end = 86400, asDataFrame = TRUE)), 33000L)
  expect_lt(33000, 100 * 3600 * 2 + 1)
  b <- .raw.read.block(info, info$blocksize_getmeta, 1, c(), 3600, params = info$params)
  expect_null(b$data)
  expect_true(b$is_last_block)
  expect_true(b$filequality$filetooshort)
  expect_false(b$filequality$filecorrupt)
  expect_identical(b$filequality$NFilePagesSkipped, 0)
  expect_identical(b$messages, character(0))
  expect_identical(c(b$startpage, b$endpage), c(1, 86400))
  expect_null(b$P)
  skip_if_no_ggir()
  I <- GGIR::g.inspectfile(SHORT(), desiredtz = MOS2_TZ)
  ref <- ggir_read(SHORT(), I, 86400, 1, ggir_fq(), 3600, c(), ggir_params(MOS2_TZ))
  expect_block_identical(b, ref, "SHORT block 1")
  expect_identical(ref$filequality$filetooshort, TRUE)
})

# a gt3x whose info.txt has no Sample Rate: read.gt3x fails for a reason other than the end of
# the file, which GGIR swallows and canhrActi records
test_that("gt3x read errors other than end-of-file are recorded in messages and flag the file corrupt", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  loc <- gsub("\\\\", "/", tempfile("rawblock_corrupt"))
  ex <- .raw.gt3x.extract(SHORT(), location = loc)
  bad <- file.path(loc, "no_sample_rate"); dir.create(bad)
  file.copy(file.path(ex, "log.bin"), bad)
  inf <- readLines(file.path(ex, "info.txt"))
  writeLines(inf[!grepl("^Sample Rate", inf)], file.path(bad, "info.txt"))
  info <- list(monc = .RAW_MONITOR[["ACTIGRAPH"]], dformc = .RAW_FORMAT[["GT3X"]], sf = 100, decn = ".",
               read_path = bad, path = bad)
  b <- suppressWarnings(.raw.read.block(info, 86400, 1, c(), 3600, desiredtz = MOS2_TZ))
  expect_null(b$data)
  expect_true(b$is_last_block)
  expect_true(b$filequality$filecorrupt)
  expect_true(b$filequality$filetooshort)
  expect_length(b$messages, 2L)
  expect_match(b$messages[1], "read.gt3x::read.gt3x failed on block 1 (records 1 to 86400): ", fixed = TRUE)
  expect_match(b$messages[1], "Sample Rate", fixed = TRUE)
  expect_identical(b$messages[2], "\nFile empty, possibly corrupt.\n")
  # a batch past the end is the silent end-of-file error
  ok <- .raw.read.block(list(monc = 3L, dformc = 6L, sf = 100, decn = ".", read_path = ex), 86400, 2, 86400, 3600,
                        desiredtz = MOS2_TZ)
  expect_null(ok$data)
  expect_true(ok$is_last_block)
  expect_identical(ok$messages, character(0))
  expect_false(ok$filequality$filecorrupt)
  unlink(loc, recursive = TRUE)
})

test_that("info without a sample frequency (corrupt inspection) is refused with a clear error", {
  info <- list(monc = 3L, dformc = 6L, sf = NULL, decn = ".", read_path = "nowhere.gt3x")
  expect_error(.raw.read.block(info, 86400, 1, c(), 3600), "info\\$sf is NULL", class = "canhrActi_raw_read_error")
})

# a 3-minute ActiGraph RAW csv: 10 header lines, a column-name row, a run of zeros
write_actigraph_csv <- function(path) {
  sf <- 30; N <- 30 * 60 * 3
  set.seed(2)
  hdr <- c("------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.4 Firmware v3.2.1 date format M/d/yyyy at 30 Hz  Filter Normal -----------",
           "Serial Number: NEO1F16120034", "Start Time 10:00:00", "Start Date 3/1/2024", "Epoch Period (hh:mm:ss) 00:00:00",
           "Download Time 12:00:00", "Download Date 3/1/2024",
           "Current Memory Address: 0", "Current Battery Voltage: 4.12     Mode = 12", "--------------------------------------------------")
  ag <- data.frame(x = round(rnorm(N, 0, 0.02), 3), y = round(rnorm(N, 0, 0.02), 3), z = round(1 + rnorm(N, 0, 0.02), 3))
  ag[100:400, ] <- 0
  con <- file(path, "w"); writeLines(hdr, con); close(con)
  # write.table warns about appending a column-name row, which is what the file needs
  suppressWarnings(
    write.table(data.frame(`Accelerometer X` = ag$x, `Accelerometer Y` = ag$y, `Accelerometer Z` = ag$z, check.names = FALSE),
                path, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE))
  invisible(path)
}

# an ad-hoc csv: 3 header rows, a column-name row, mg units, a 3 s gap, a wear column
write_adhoc_csv <- function(path) {
  sf <- 20; N <- 20 * 60 * 5
  t0 <- as.POSIXct("2024-03-01 10:00:00", tz = OTHER_TZ)
  tt <- t0 + (0:(N - 1)) / sf
  tt[(N/2 + 1):N] <- tt[(N/2 + 1):N] + 3
  set.seed(1)
  acc <- data.frame(timestamp = format(tt, "%Y-%m-%d %H:%M:%OS3"),
                    ax = round(1000 * rnorm(N, 0, 0.02)), ay = round(1000 * rnorm(N, 0, 0.02)),
                    az = round(1000 * (1 + rnorm(N, 0, 0.02))), temp = 25 + round(rnorm(N), 2), worn = TRUE)
  con <- file(path, "w"); writeLines(c("sample_rate,20", "serial,ABC123", "id,P01"), con); close(con)
  suppressWarnings(
    write.table(acc, path, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE))
  invisible(path)
}
adhoc_overrides <- list(rmc.firstrow.acc = 5, rmc.firstrow.header = 1, rmc.header.length = 3,
                        rmc.headername.sf = "sample_rate", rmc.headername.sn = "serial",
                        rmc.col.time = 1, rmc.col.acc = 2:4, rmc.col.temp = 5, rmc.col.wear = 6,
                        rmc.unit.acc = "mg", rmc.unit.time = "POSIX", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                        rmc.check4timegaps = TRUE, rmc.noise = 13)

test_that("ActiGraph RAW csv: header row skipped, x300 block, zeros kept, three blocks identical() to GGIR; too short at the real block size", {
  skip_if_not_installed("data.table")
  d <- gsub("\\\\", "/", tempfile("rawblock_agcsv")); dir.create(d)
  f <- write_actigraph_csv(file.path(d, "actigraph_raw.csv"))
  info <- raw.inspect(f, desiredtz = OTHER_TZ)
  expect_identical(info$monc, .RAW_MONITOR[["ACTIGRAPH"]]); expect_identical(info$dformc, .RAW_FORMAT[["CSV"]])
  expect_identical(info$sf, 30); expect_identical(info$decn, ".")
  expect_identical(info$blocksize_getmeta, 8707)
  ours <- list(); fq <- .raw.filequality(); pep <- c()
  for (i in 1:3) {
    ours[[i]] <- .raw.read.block(info, 10, i, pep, ws = 10, params = info$params, filequality = fq)
    fq <- ours[[i]]$filequality; pep <- ours[[i]]$endpage
  }
  expect_identical(dim(ours[[1]]$data), c(3000L, 3L)); expect_identical(names(ours[[1]]$data), c("x", "y", "z"))
  expect_identical(c(ours[[1]]$startpage, ours[[1]]$endpage), c(11, 3011))
  expect_identical(dim(ours[[2]]$data), c(2400L, 3L))
  expect_identical(c(ours[[2]]$startpage, ours[[2]]$endpage), c(3011, 6011))
  expect_null(ours[[3]]$data)
  expect_identical(c(ours[[3]]$startpage, ours[[3]]$endpage), c(6011, 9011))
  expect_identical(sapply(ours, `[[`, "is_last_block"), c(FALSE, FALSE, TRUE))
  expect_identical(sum(ours[[1]]$data$x == 0 & ours[[1]]$data$y == 0 & ours[[1]]$data$z == 0), 301L)
  # the real getmeta block size with ws 3600: 5400 rows < 216001, too short
  short <- .raw.read.block(info, info$blocksize_getmeta, 1, c(), 3600, params = info$params)
  expect_null(short$data); expect_true(short$filequality$filetooshort); expect_false(short$filequality$filecorrupt)
  expect_true(short$is_last_block)
  # a non-numeric acceleration value
  lines <- readLines(f); lines[30] <- sub("^[^,]*", "abc", lines[30]); fb <- file.path(d, "bad.csv"); writeLines(lines, fb)
  infob <- raw.inspect(fb, desiredtz = OTHER_TZ)
  expect_error(.raw.read.block(infob, 10, 1, c(), ws = 10, params = infob$params),
               "Corrupt file. x column contains non-numeric data.", fixed = TRUE, class = "canhrActi_raw_read_error")
  skip_if_no_ggir()
  P <- ggir_params(OTHER_TZ)
  I <- GGIR::g.inspectfile(f, desiredtz = OTHER_TZ)
  fq <- ggir_fq(); pep <- c(); header <- NULL
  for (i in 1:3) {
    ref <- ggir_read(f, I, 10, i, fq, 10, pep, P, header = header)
    expect_block_identical(ours[[i]], ref, paste("ActiGraph csv block", i))
    fq <- ref$filequality; pep <- ref$endpage; header <- ref$header
  }
  refs <- ggir_read(f, I, 8707, 1, ggir_fq(), 3600, c(), P)
  expect_block_identical(short, refs, "ActiGraph csv too short")
  Ib <- GGIR::g.inspectfile(fb, desiredtz = OTHER_TZ)
  expect_error(ggir_read(fb, Ib, 10, 1, ggir_fq(), 10, c(), P), "Corrupt file. x column contains non-numeric data.", fixed = TRUE)
  unlink(d, recursive = TRUE)
})

test_that("ad-hoc csv through .raw.read.myacc.csv: 3060 then 2999 rows, wear 1, state returned, identical() to GGIR", {
  skip_if_not_installed("data.table")
  # The hand-worked times below hold on a machine set to America/Anchorage; in UTC, GGIR's
  # reader and the port both put this synthetic start 1 ms lower
  withr::local_timezone("America/Anchorage")
  d <- gsub("\\\\", "/", tempfile("rawblock_adhoc")); dir.create(d)
  f <- write_adhoc_csv(file.path(d, "adhoc.csv"))
  params <- do.call(raw.params, c(list(desiredtz = OTHER_TZ), adhoc_overrides))
  info <- raw.inspect(f, params = params)
  expect_identical(info$monc, .RAW_MONITOR[["AD_HOC"]]); expect_identical(info$dformc, .RAW_FORMAT[["AD_HOC_CSV"]])
  expect_identical(info$sf, 20)
  expect_identical(info$blocksize_getmeta, 5805)
  ours <- list(); fq <- .raw.filequality(); pep <- c(); plv <- c(0, 0, 1); plt <- NULL
  for (i in 1:3) {
    ours[[i]] <- .raw.read.block(info, 10, i, pep, ws = 10, params = params, filequality = fq,
                                 previous_last_value = plv, previous_last_time = plt)
    fq <- ours[[i]]$filequality; pep <- ours[[i]]$endpage
    plv <- ours[[i]]$previous_last_value; plt <- ours[[i]]$previous_last_time
  }
  b1 <- ours[[1]]$data
  expect_identical(dim(b1), c(3060L, 6L))
  expect_identical(names(b1), c("time", "x", "y", "z", "temperature", "wear"))
  expect_identical(c(ours[[1]]$startpage, ours[[1]]$endpage), c(1, 3001))   # +1 for the column-name row
  expect_true(is.numeric(b1$time)); expect_true(is.numeric(b1$wear)); expect_true(all(b1$wear == 1))
  # the first data row is skipped because column 1 is a timestamp string, so the block starts
  # at 10:00:00.050; 3000 rows read plus 60 imputed for the 3 s gap
  expect_close(b1$time[1], utime("2024-03-01 10:00:00", OTHER_TZ) + 0.05, 1e-6, "ad-hoc first time")
  expect_close(b1$time[nrow(b1)] - b1$time[1], 152.95, 1e-6, "ad-hoc span")
  expect_identical(dim(ours[[2]]$data), c(2999L, 6L))
  expect_identical(c(ours[[2]]$startpage, ours[[2]]$endpage), c(3001, 6001))
  expect_null(ours[[3]]$data); expect_true(ours[[3]]$is_last_block)
  expect_identical(sapply(ours, `[[`, "is_last_block"), c(FALSE, FALSE, TRUE))
  # mg converted to g; the reader returns the carried state (last row of the imputed block)
  expect_true(all(abs(b1$z) < 2))
  expect_identical(unname(unlist(ours[[1]]$previous_last_value)), unname(unlist(b1[nrow(b1), c("x", "y", "z")])))
  expect_s3_class(ours[[1]]$previous_last_time, "POSIXct")
  expect_identical(as.numeric(ours[[1]]$previous_last_time), b1$time[nrow(b1)])
  expect_identical(rownames(ours[[1]]$header), c("sample_rate", "device_serial_number", "id"))
  expect_true("PreviousLastValue" %in% names(ours[[1]]$P))
  skip_if_no_ggir()
  P <- do.call(ggir_params, c(list(desiredtz = OTHER_TZ), adhoc_overrides))
  I <- GGIR::g.inspectfile(f, desiredtz = OTHER_TZ, params_rawdata = P$params_rawdata, configtz = c())
  fq <- ggir_fq(); pep <- c(); plv <- c(0, 0, 1); plt <- NULL; header <- NULL
  for (i in 1:3) {
    ref <- ggir_read(f, I, 10, i, fq, 10, pep, P, plv, plt, header)
    expect_block_identical(ours[[i]], ref, paste("ad-hoc csv block", i))
    expect_identical(ours[[i]]$header, ref$P$header)
    fq <- ref$filequality; pep <- ref$endpage; header <- ref$header
    if ("PreviousLastValue" %in% names(ref$P)) { plv <- ref$P$PreviousLastValue; plt <- ref$P$PreviousLastTime }
  }
  # the reader alone
  a <- do.call(.raw.read.myacc.csv, c(list(rmc.file = f, rmc.nrow = 50, rmc.skip = 1, desiredtz = OTHER_TZ),
                                      adhoc_overrides[setdiff(names(adhoc_overrides), "rmc.noise")]))
  g <- do.call(GGIR::read.myacc.csv, c(list(rmc.file = f, rmc.nrow = 50, rmc.skip = 1, desiredtz = OTHER_TZ),
                                       adhoc_overrides[setdiff(names(adhoc_overrides), "rmc.noise")]))
  expect_identical(a, g)
  unlink(d, recursive = TRUE)
})

test_that("ad-hoc csv: ggir_exact = FALSE passes rmc.headername.recordingid instead of rmc.headername.sn", {
  skip_if_not_installed("data.table")
  d <- gsub("\\\\", "/", tempfile("rawblock_adhoc3")); dir.create(d)
  f <- write_adhoc_csv(file.path(d, "adhoc.csv"))
  ov <- c(adhoc_overrides, list(rmc.headername.recordingid = "id"))
  exact <- do.call(raw.params, c(list(desiredtz = OTHER_TZ), ov))
  fixed <- do.call(raw.params, c(list(desiredtz = OTHER_TZ, ggir_exact = FALSE), ov))
  info <- raw.inspect(f, params = exact)
  b_exact <- .raw.read.block(info, 10, 1, c(), ws = 10, params = exact)
  b_fixed <- .raw.read.block(info, 10, 1, c(), ws = 10, params = fixed)
  # GGIR never renames the recording id row; the corrected path does
  expect_identical(rownames(b_exact$header), c("sample_rate", "device_serial_number", "id"))
  expect_identical(rownames(b_fixed$header), c("sample_rate", "device_serial_number", "recordingID"))
  expect_identical(b_exact$data, b_fixed$data)
  unlink(d, recursive = TRUE)
})

test_that("a reader failure inside GGIR's try() is recorded in messages and flags block 1 corrupt", {
  skip_if_not_installed("GGIRread")
  d <- gsub("\\\\", "/", tempfile("rawblock_missing")); dir.create(d)
  info <- list(monc = .RAW_MONITOR[["GENEACTIV"]], dformc = .RAW_FORMAT[["BIN"]], sf = 86, decn = ".",
               read_path = file.path(d, "missing.bin"))
  b <- suppressWarnings(.raw.read.block(info, 5, 1, c(), ws = 1, desiredtz = OTHER_TZ))
  expect_null(b$data); expect_null(b$P)
  expect_true(b$is_last_block)
  expect_true(b$filequality$filecorrupt); expect_true(b$filequality$filetooshort)
  expect_length(b$messages, 2L)
  expect_match(b$messages[1], "GGIRread::readGENEActiv failed on block 1 (pages 1 to 5): ", fixed = TRUE)
  expect_identical(b$messages[2], "\nFile empty, possibly corrupt.\n")
  unlink(d, recursive = TRUE)
})

test_that("ad-hoc csv errors keep GGIR's texts", {
  skip_if_not_installed("data.table")
  d <- gsub("\\\\", "/", tempfile("rawblock_adhoc2")); dir.create(d)
  f <- write_adhoc_csv(file.path(d, "adhoc.csv"))
  expect_error(.raw.read.myacc.csv(rmc.file = f, rmc.nrow = 5, desiredtz = OTHER_TZ),
               "rmc.firstrow.acc always need to be specified", fixed = TRUE)
  expect_error(.raw.read.myacc.csv(rmc.file = f, rmc.nrow = 5, rmc.firstrow.acc = 5),
               "Timezone not specified", fixed = TRUE)
  expect_error(.raw.read.myacc.csv(rmc.file = f, rmc.nrow = 5, rmc.firstrow.acc = 5, rmc.col.time = 1,
                                   rmc.unit.time = "bogus", desiredtz = OTHER_TZ),
               "Unrecognized rmc.col.time value", fixed = TRUE)
  expect_warning(.raw.read.myacc.csv(rmc.file = f, rmc.nrow = 5, rmc.firstrow.acc = 5, rmc.col.time = 1,
                                     rmc.desiredtz = OTHER_TZ, desiredtz = OTHER_TZ),
                 "scheduled to be deprecated", fixed = TRUE)
  unlink(d, recursive = TRUE)
})

test_that("P4 GENEActiv 85.7 Hz: 5 pages -> 1503 rows at 1/86 s, identical() to GGIR; 20 pages -> 5046 rows, last block", {
  skip_if_no_ggir_ref(); skip_if_not_installed("GGIRread")
  f <- file.path(ggirread_tf(), "GENEActiv_testfile.bin"); skip_if_no_file(f)
  info <- suppressWarnings(raw.inspect(f, desiredtz = OTHER_TZ))
  expect_identical(info$sf, 86)
  b <- .raw.read.block(info, 5, 1, c(), ws = 1, params = info$params)
  expect_identical(nrow(b$data), 1503L)
  expect_identical(names(b$data), c("time", "x", "y", "z", "light", "temperature"))
  # seq(first, last, 1/86) on Unix seconds near 1.4e9, where doubles are 2.4e-7 apart
  expect_true(all(abs(diff(b$data$time) - 1/86) < 1e-6))
  expect_identical(b$data$time, seq(b$data$time[1], b$data$time[1] + 1503/86, 1/86)[1:1503])
  expect_identical(b$header$SampleRate, 86)
  expect_identical(c(b$startpage, b$endpage), c(1, 5))
  expect_false(b$is_last_block)  # 1503 is not below 5 * 300 and not below 86 * 1 * 2 + 1
  expect_close(b$data$time[1], utime("2013-05-30 10:12:54", OTHER_TZ) + 0.5, 1e-6, "GENEActiv first time")
  b20 <- .raw.read.block(info, 20, 1, c(), ws = 1, params = info$params)
  expect_identical(nrow(b20$data), 5046L)
  expect_true(b20$is_last_block)
  expect_identical(c(b20$startpage, b20$endpage), c(1, 20))
  skip_if_no_ggir()
  P <- ggir_params(OTHER_TZ)
  I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = OTHER_TZ))
  expect_block_identical(b, ggir_read(f, I, 5, 1, ggir_fq(), 1, c(), P), "GENEActiv 5 pages")
  expect_block_identical(b20, ggir_read(f, I, 20, 1, ggir_fq(), 1, c(), P), "GENEActiv 20 pages")
})

test_that("P4 AX3 cwa: blocks 0..50 and 50..100 give 6069 and 6068 rows, identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_not_installed("GGIRread")
  f <- file.path(ggirread_tf(), "ax3_testfile.cwa"); skip_if_no_file(f)
  info <- raw.inspect(f, desiredtz = OTHER_TZ)
  expect_identical(info$sf, 100)
  b1 <- .raw.read.block(info, 50, 1, c(), ws = 10, params = info$params)
  b2 <- .raw.read.block(info, 50, 2, b1$endpage, ws = 10, params = info$params, filequality = b1$filequality)
  expect_identical(nrow(b1$data), 6069L); expect_identical(nrow(b2$data), 6068L)
  expect_identical(names(b1$data), c("time", "x", "y", "z", "temperature", "light"))
  expect_identical(c(b1$startpage, b1$endpage), c(0, 50))
  expect_identical(c(b2$startpage, b2$endpage), c(50, 100))
  expect_false(b1$is_last_block); expect_false(b2$is_last_block)
  expect_null(b1$qclog); expect_null(b2$qclog)
  expect_true(all(abs(diff(b1$data$time) - 0.01) < 1e-6))  # doubles near 1.55e9 are 2.4e-7 apart
  expect_identical(names(b1$header)[1:2], c("uniqueSerialCode", "frequency"))
  skip_if_no_ggir()
  P <- ggir_params(OTHER_TZ)
  I <- GGIR::g.inspectfile(f, desiredtz = OTHER_TZ)
  r1 <- ggir_read(f, I, 50, 1, ggir_fq(), 10, c(), P)
  r2 <- ggir_read(f, I, 50, 2, r1$filequality, 10, r1$endpage, P, header = r1$header)
  expect_block_identical(b1, r1, "cwa block 1"); expect_block_identical(b2, r2, "cwa block 2")
})

test_that("P4 corrupt cwa: QClog rows (FALSE 13->14), (FALSE 14->15), (TRUE 12->15 imputed 3.64 s, 33.4 Hz), identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_not_installed("GGIRread")
  f <- file.path(ggirread_tf(), "ax3_testfile_corrupt_blocks_0_13_14_142_143_144.cwa"); skip_if_no_file(f)
  info <- suppressWarnings(raw.inspect(f, desiredtz = OTHER_TZ))
  # block 2 after an end page of 1 with block size 19 is pages 1..20
  b <- suppressWarnings(.raw.read.block(info, 19, 2, 1, ws = 1, params = info$params))
  expect_identical(c(b$startpage, b$endpage), c(1, 20))
  q <- b$qclog
  expect_identical(nrow(q), 3L)
  expect_identical(q$checksum_pass, c(FALSE, FALSE, TRUE))
  expect_equal(q$blockID_current, c(13, 14, 12))
  expect_equal(q$blockID_next, c(14, 15, 15))
  expect_identical(q$imputed, c(FALSE, FALSE, TRUE))
  expect_equal(round(q$blockLengthSeconds[3], 2), 3.64)
  expect_equal(round(q$frequency_observed[3], 1), 33.4)
  expect_equal(q$frequency_blockheader[3], 100)
  # block 1 (pages 0..20) adds the corrupt block 0 row in front
  b0 <- suppressWarnings(.raw.read.block(info, 20, 1, c(), ws = 1, params = info$params))
  expect_identical(nrow(b0$qclog), 4L)
  expect_equal(b0$qclog$blockID_current, c(0, 13, 14, 12))
  expect_identical(b0$filequality$NFilePagesSkipped, 0)
  skip_if_no_ggir()
  P <- ggir_params(OTHER_TZ)
  I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = OTHER_TZ))
  ref <- suppressWarnings(ggir_read(f, I, 19, 2, ggir_fq(), 1, 1, P))
  expect_block_identical(b, ref, "corrupt cwa pages 1..20")
  expect_identical(b$qclog, ref$P$QClog)
  ref0 <- suppressWarnings(ggir_read(f, I, 20, 1, ggir_fq(), 1, c(), P))
  expect_block_identical(b0, ref0, "corrupt cwa pages 0..20")
})

# pages 1..4 are block 2 after an end page of 1; block 1 covers pages 0..3, also 364 rows
test_that("P4 interpolationType 2 changes 263 of 364 rows of the clean cwa file (pages 1..4) and is identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_not_installed("GGIRread")
  f <- file.path(ggirread_tf(), "ax3_testfile.cwa"); skip_if_no_file(f)
  info <- raw.inspect(f, desiredtz = OTHER_TZ)
  lin <- .raw.read.block(info, 3, 2, 1, ws = 1, params = info$params)
  nn <- .raw.read.block(info, 3, 2, 1, ws = 1, params = info$params, interpolationType = 2)
  expect_identical(c(lin$startpage, lin$endpage), c(1, 4))
  expect_identical(nrow(lin$data), 364L); expect_identical(nrow(nn$data), 364L)
  expect_identical(sum(lin$data$x != nn$data$x), 263L)
  expect_identical(lin$data$time, nn$data$time)
  lin0 <- .raw.read.block(info, 3, 1, c(), ws = 1, params = info$params)
  nn0 <- .raw.read.block(info, 3, 1, c(), ws = 1, params = info$params, interpolationType = 2)
  expect_identical(c(lin0$startpage, lin0$endpage), c(0, 3))
  expect_identical(nrow(lin0$data), 364L)
  expect_identical(sum(lin0$data$x != nn0$data$x), 231L)
  skip_if_no_ggir()
  I <- GGIR::g.inspectfile(f, desiredtz = OTHER_TZ)
  expect_block_identical(lin, ggir_read(f, I, 3, 2, ggir_fq(), 1, 1, ggir_params(OTHER_TZ)), "cwa linear 1..4")
  expect_block_identical(nn, ggir_read(f, I, 3, 2, ggir_fq(), 1, 1, ggir_params(OTHER_TZ, interpolationType = 2)), "cwa nearest 1..4")
  expect_block_identical(lin0, ggir_read(f, I, 3, 1, ggir_fq(), 1, c(), ggir_params(OTHER_TZ)), "cwa linear 0..3")
  expect_block_identical(nn0, ggir_read(f, I, 3, 1, ggir_fq(), 1, c(), ggir_params(OTHER_TZ, interpolationType = 2)), "cwa nearest 0..3")
})

ggir_tf <- function() {
  clone <- file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIR", "inst", "testfiles")
  if (dir.exists(clone)) return(clone)
  system.file("testfiles", package = "GGIR")
}

test_that("Axivity csv (GGIR's unix-timestamp export): sf floored to 95, UTC digits relabelled, resampled per block, identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_not_installed("GGIRread"); skip_if_not_installed("data.table")
  f <- file.path(ggir_tf(), "ax3_testfile_unix_timestamps.csv"); skip_if_no_file(f)
  info <- raw.inspect(f, desiredtz = OTHER_TZ)
  expect_identical(info$monc, .RAW_MONITOR[["AXIVITY"]]); expect_identical(info$dformc, .RAW_FORMAT[["CSV"]])
  expect_identical(info$sf, 95)   # floor(observed / 5) * 5
  b1 <- .raw.read.block(info, 2, 1, c(), ws = 1, params = info$params)   # 2 x 300 rows requested, 209 in the file
  expect_identical(nrow(b1$data), 200L)
  expect_identical(names(b1$data), c("time", "x", "y", "z"))
  expect_identical(c(b1$startpage, b1$endpage), c(0, 600))
  expect_true(b1$is_last_block)
  expect_true(all(abs(diff(b1$data$time) * 95 - 1) < 1e-4))
  # the csv timestamps are taken as UTC digits and relabelled to configtz (OmGui quirk)
  expect_identical(b1$data$time[1], as.numeric(lubridate::force_tz(as.POSIXct(1551178506, origin = "1970-01-01", tz = "UTC"), OTHER_TZ)))
  # GGIR never reads past the flagged last block; the port returns an empty block there
  b2 <- .raw.read.block(info, 2, 2, b1$endpage, ws = 1, params = info$params, filequality = b1$filequality)
  expect_null(b2$data); expect_true(b2$is_last_block); expect_false(b2$filequality$filecorrupt)
  expect_match(b2$messages, "data.table::fread failed on block 2 (pages 600 to 1200)", fixed = TRUE)
  skip_if_no_ggir()
  I <- GGIR::g.inspectfile(f, desiredtz = OTHER_TZ)
  expect_block_identical(b1, ggir_read(f, I, 2, 1, ggir_fq(), 1, c(), ggir_params(OTHER_TZ)), "Axivity csv block 1")
})

test_that("P4 Parmay Matrix: the reader ignores endpage, 39,400 rows on the first call, last block, identical() to GGIR", {
  skip_if_no_ggir_ref(); skip_if_not_installed("GGIRread")
  f <- file.path(ggirread_tf(), "mtx_100Hz_acc_HR_temp.BIN"); skip_if_no_file(f)
  info <- suppressWarnings(raw.inspect(f, desiredtz = OTHER_TZ))
  expect_identical(info$sf, 100)
  b <- suppressWarnings(.raw.read.block(info, 2, 1, c(), ws = 10, params = info$params))
  expect_identical(dim(b$data), c(39400L, 5L))
  expect_identical(names(b$data), c("time", "x", "y", "z", "temperature"))
  expect_identical(c(b$startpage, b$endpage), c(1, 2))
  expect_true(b$is_last_block)
  expect_true(isTRUE(b$P$lastchunk))
  expect_identical(nrow(b$qclog), 4L)
  skip_if_no_ggir()
  I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = OTHER_TZ))
  ref <- suppressWarnings(ggir_read(f, I, 2, 1, ggir_fq(), 10, c(), ggir_params(OTHER_TZ)))
  expect_block_identical(b, ref, "Parmay")
})

test_that("EE getmeta block 1: 8,640,000 rows, identical() to GGIR (long)", {
  skip_unless_long(); skip_if_no_ggir_ref(); skip_if_no_file(EE()); skip_if_no_ggir()
  info <- raw.inspect(EE(), desiredtz = "Europe/Helsinki")
  expect_identical(info$sf, 100)
  b <- .raw.read.block(info, info$blocksize_getmeta, 1, c(), 3600, params = info$params)
  expect_identical(nrow(b$data), 8640000L)
  expect_false(b$is_last_block)
  I <- GGIR::g.inspectfile(EE(), desiredtz = "Europe/Helsinki")
  ref <- ggir_read(EE(), I, 86400, 1, ggir_fq(), 3600, c(), ggir_params("Europe/Helsinki"))
  expect_block_identical(b, ref, "EE block 1")
})
