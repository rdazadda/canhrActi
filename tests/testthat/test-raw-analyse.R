# Parity tests for R/raw_analyse_perday.R and R/raw_analyse_perfile.R: the activity columns
# of part2_daysummary.csv and part2_summary.csv (L5/M5, the means, MVPA, the five
# aggregates and the full-recording means) for every metric. The ENMO-only reference is the
# stored MOS2 run in CANHRACTI_GGIR_REF; the three-metric reference (ENMO, MAD, ENMOa, the
# dashboard's) is a GGIR part 2 run inside the test on canhrActi's part-1 milestone of the
# sample. Tests skip when a file or GGIR is missing.

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

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
MOS2_DAYSUM <- function() ref_file("out", "output_din", "results", "part2_daysummary.csv")
MOS2_SUM <- function() ref_file("out", "output_din", "results", "part2_summary.csv")
MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to

sample_gt3x <- function() {
  p <- system.file("shiny", "canhrActi_dashboard", "data", "MOS2E39230594.gt3x", package = "canhrActi")
  if (!nzchar(p) && nzchar(.ggir_ref)) p <- ref_file("din", "MOS2E39230594.gt3x")
  skip_if_no_file(p)
  gsub("\\\\", "/", p)
}

memo <- new.env()
stored_M <- function() {
  if (is.null(memo$M)) {
    skip_if_no_file(MOS2_RDATA())
    e <- new.env(); load(MOS2_RDATA(), envir = e)
    memo$M <- list(M = e$M, I = e$I, C = e$C)
  }
  memo$M
}
read_csv_chr <- function(path) {
  skip_if_no_file(path)
  utils::read.csv(path, colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE)
}

# the pieces of a canhrActi_raw the analysis reads, from a GGIR M list
x_from_M <- function(M, tz, ...) {
  w <- raw.wear.decision(M, desiredtz = tz, ...)
  list(imputed = raw.impute(M, w), wear = w, meta = list(windowsizes = M$windowsizes),
       params = raw.params(desiredtz = tz, ...))
}
# g.impute and g.analyse with GGIR's defaults, mvpathreshold and boutcriter filled from
# threshold.mod and boutcriter.mvpa as check_params fills them
ggir_part2 <- function(M, I, C, tz, ...) {
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- tz
  P$params_phyact[["mvpathreshold"]] <- P$params_phyact[["threshold.mod"]]
  P$params_phyact[["boutcriter"]] <- P$params_phyact[["boutcriter.mvpa"]]
  ov <- list(...)
  for (nm in names(ov)) P$params_cleaning[nm] <- list(ov[[nm]])
  IMP <- GGIR::g.impute(M, I, params_cleaning = P$params_cleaning, desiredtz = tz, dayborder = 0, ID = "x")
  GGIR::g.analyse(I, C, M, IMP, params_247 = P$params_247, params_phyact = P$params_phyact,
                  params_general = P$params_general, params_cleaning = P$params_cleaning, ID = "x")
}
# a GGIR character table as g.part2 stores it
as_num <- function(df) lapply(df, function(v) suppressWarnings(as.numeric(as.character(v))))
AGG <- "^(AD|WD|WE|WWD|WWE)_"

# the metric columns of a GGIR run against the port: names and order, and identical() values
expect_part2_matches <- function(x, S, label) {
  crit <- as.numeric(x$params$includedaycrit %||% 16)
  day <- .raw.analyse.perday(x)
  rep <- .raw.perday.report(day)
  gd <- S$daysummary
  met <- names(gd)[-(1:8)]
  expect_identical(names(rep), met, label = paste(label, "day columns"))
  expect_identical(as.list(rep), as_num(gd[met]), label = paste(label, "day values"))
  gs <- S$summary
  ga <- grep(AGG, names(gs), value = TRUE)
  if (ncol(day)) {
    agg <- .raw.analyse.perfile(day, x$wear$daily, crit)
    expect_identical(names(agg), ga, label = paste(label, "aggregate columns"))
    expect_identical(as.list(agg), as_num(gs[ga]), label = paste(label, "aggregate values"))
  } else {
    expect_length(ga, 0)
  }
  fm <- .raw.full.recording.metrics(x$imputed$metashort)
  frm <- lapply(fm, function(m) .raw.full.recording.mean(x, m))
  gf <- paste0(fm, "_fullRecordingMean")
  expect_identical(grep("_fullRecordingMean$", names(gs), value = TRUE), gf, label = paste(label, "full-recording means"))
  expect_identical(stats::setNames(frm, gf), as_num(gs[gf]), label = paste(label, "full-recording mean values"))
  invisible(day)
}

test_that("the ENMO columns equal the stored part2 csvs at 3 decimals (MOS2)", {
  skip_if_no_ggir_ref()
  s <- stored_M()
  x <- x_from_M(s$M, MOS2_TZ)
  expect_identical(.raw.perday.metrics(x$imputed$metashort), "ENMO")
  day <- .raw.analyse.perday(x)
  ds <- read_csv_chr(MOS2_DAYSUM())
  expect_identical(names(.raw.perday.report(day)), names(ds)[-(1:9)])
  expect_identical(ncol(day), 12L)
  for (k in names(day)) {
    expect_identical(round(day[[k]], 3), suppressWarnings(as.numeric(ds[[k]])), label = k)
  }
  sm <- read_csv_chr(MOS2_SUM())
  agg <- .raw.analyse.perfile(day, x$wear$daily, 16)
  expect_identical(names(agg), grep(AGG, names(sm), value = TRUE))
  expect_identical(ncol(agg), 60L)
  for (k in names(agg)) {
    expect_identical(round(agg[[k]], 3), suppressWarnings(as.numeric(sm[[k]])), label = k)
  }
  expect_identical(round(.raw.full.recording.mean(x, "ENMO"), 3), as.numeric(sm$ENMO_fullRecordingMean))
})

test_that("live GGIR: the ENMO columns are identical() to g.analyse's (MOS2)", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M()
  expect_part2_matches(x_from_M(s$M, MOS2_TZ), ggir_part2(s$M, s$I, s$C, MOS2_TZ), "MOS2 ENMO")
})

test_that("ENMO, MAD and ENMOa: GGIR's own part 2 on canhrActi's part 1 of the sample", {
  skip_if_no_ggir()
  f <- sample_gt3x()
  x <- read.raw.accelerometer(f, params = raw.params(desiredtz = MOS2_TZ, do.enmo = TRUE, do.enmoa = TRUE,
                                                     do.mad = TRUE), sleep = FALSE)
  expect_identical(names(x$imputed$metashort), c("timestamp", "anglez", "ENMO", "MAD", "ENMOa"))
  expect_identical(.raw.perday.metrics(x$imputed$metashort), c("ENMO", "MAD", "ENMOa"))
  # GGIR part 2 and its report, run on the part-1 milestone canhrActi writes
  out <- tempfile("p2three")
  od <- file.path(out, "output_din")
  b <- file.path(od, "meta", "basic")
  for (d in c(b, file.path(od, "meta", c("ms2.out", "sleep.qc")), file.path(od, "results", c("QC", "file summary reports")))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  write.ggir.milestone(x, b)
  suppressWarnings(utils::capture.output(GGIR::GGIR(
    mode = 2, datadir = f, outputdir = out, studyname = "din", do.report = 2, desiredtz = MOS2_TZ,
    do.enmo = TRUE, do.enmoa = TRUE, do.mad = TRUE, overwrite = TRUE, do.parallel = FALSE, verbose = FALSE)))
  e <- new.env(); load(file.path(od, "meta", "ms2.out", "MOS2E39230594.gt3x.RData"), envir = e)
  day <- expect_part2_matches(x, e$SUM, "MOS2 three metrics")
  rep <- .raw.perday.report(day)
  # twelve columns for ENMO, eleven for MAD and ENMOa, whose 1-6am column the day table drops
  expect_identical(ncol(day), 36L)
  expect_identical(ncol(rep), 34L)
  expect_identical(grep("1-6am", names(rep), value = TRUE), "mean_ENMO_mg_1-6am")
  # the csvs at 3 decimals, header included
  ds <- read_csv_chr(file.path(od, "results", "part2_daysummary.csv"))
  expect_identical(ncol(ds), 43L)
  expect_identical(names(ds)[-(1:9)], names(rep))
  for (k in names(rep)) expect_identical(round(rep[[k]], 3), suppressWarnings(as.numeric(ds[[k]])), label = k)
  sm <- read_csv_chr(file.path(od, "results", "part2_summary.csv"))
  expect_identical(ncol(sm), 207L)
  agg <- .raw.analyse.perfile(day, x$wear$daily, 16)
  expect_identical(ncol(agg), 180L)
  for (k in names(agg)) expect_identical(round(agg[[k]], 3), suppressWarnings(as.numeric(sm[[k]])), label = k)
  for (m in c("ENMO", "MAD", "ENMOa")) {
    k <- paste0(m, "_fullRecordingMean")
    expect_identical(round(.raw.full.recording.mean(x, m), 3), as.numeric(sm[[k]]), label = k)
  }
  # the summary column order, where MAD_fullRecordingMean takes part in GGIR's reordering
  gs <- names(e$SUM$summary)
  pre <- c(gs[1:15], paste0(c("ENMO", "MAD", "ENMOa"), "_fullRecordingMean"),
           grep("^filehealth_", gs, value = TRUE), "N valid weekend days (WE)", "N valid weekdays (WD)",
           names(agg), utils::tail(gs, 7))
  expect_identical(.raw.perfile.order(pre), gs)
  expect_identical(match(c("ENMO_fullRecordingMean", "AD_L5hr_ENMO_mg_0-24hr", "MAD_fullRecordingMean"), names(sm)),
                   c(16L, 17L, 197L))
  # the ms2 SUM canhrActi writes is the one GGIR's part 2 saves
  expect_identical(as.ggir.milestone(x, part = 2)$SUM, e$SUM)
  # and GGIR's own part-2 report on a folder canhrActi wrote gives GGIR's three files
  ca <- file.path(out, "output_ca")
  write.ggir.milestone(x, ca, parts = 1:2)
  dir.create(file.path(ca, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(utils::capture.output(GGIR::GGIR(
    mode = c(), datadir = f, outputdir = out, studyname = "ca", do.report = 2, desiredtz = MOS2_TZ,
    do.enmo = TRUE, do.enmoa = TRUE, do.mad = TRUE, overwrite = TRUE, do.parallel = FALSE, verbose = FALSE)))
  bytes <- function(p) readBin(p, "raw", file.size(p))
  for (k in c("part2_summary.csv", "part2_daysummary.csv", file.path("QC", "data_quality_report.csv"))) {
    expect_identical(bytes(file.path(ca, "results", k)), bytes(file.path(od, "results", k)), label = k)
  }
})

test_that("a 25 hour day keeps all 25 hours and a 23 hour day is padded, as g.analyse does", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M()
  # the stored recording's clock moved so that it spans a change of daylight saving time
  relabel <- function(M, start) {
    sh <- as.numeric(as.POSIXct(start, tz = MOS2_TZ)) -
      as.numeric(.raw.iso8601.to.posix(M$metashort$timestamp[1], tz = MOS2_TZ))
    f <- function(ts) format(.raw.iso8601.to.posix(ts, tz = MOS2_TZ) + sh, "%Y-%m-%dT%H:%M:%S%z", tz = MOS2_TZ)
    M$metashort$timestamp <- f(M$metashort$timestamp)
    M$metalong$timestamp <- f(M$metalong$timestamp)
    M
  }
  for (case in list(list(start = "2025-10-30 20:30:00", day = 4, hours = 25),
                    list(start = "2026-03-05 20:30:00", day = 4, hours = 23))) {
    M <- relabel(s$M, case$start)
    x <- x_from_M(M, MOS2_TZ)
    expect_identical(x$wear$daily$n_hours[case$day], case$hours)
    expect_true(x$wear$daily$n_valid_hours[case$day] >= 16)
    expect_part2_matches(x, ggir_part2(M, s$I, s$C, MOS2_TZ), paste(case$hours, "hour day"))
  }
})

test_that("a first valid day of 6 hours or less puts the 1-6am column last, as in GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M()
  # day 1 holds 3.5 hours, valid once includedaycrit is 3 or less
  for (crit in c(3, 0)) {
    x <- x_from_M(s$M, MOS2_TZ, includedaycrit = crit)
    expect_true(x$wear$daily$n_valid_hours[1] >= crit)
    day <- expect_part2_matches(x, ggir_part2(s$M, s$I, s$C, MOS2_TZ, includedaycrit = crit),
                                paste("includedaycrit", crit))
    expect_identical(utils::tail(names(day), 1), "mean_ENMO_mg_1-6am")
    expect_true(is.na(day[["mean_ENMO_mg_1-6am"]][1]))
  }
})

test_that("with no valid day there are no day columns and no aggregates, as in GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M()
  x <- x_from_M(s$M, MOS2_TZ, includedaycrit = 25)
  day <- expect_part2_matches(x, ggir_part2(s$M, s$I, s$C, MOS2_TZ, includedaycrit = 25), "includedaycrit 25")
  expect_identical(dim(day), c(8L, 0L))
  expect_identical(dim(.raw.perday.report(day)), c(8L, 0L))
})

test_that("the full-recording mean is NA without a single worn long epoch, as in GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M()
  x <- x_from_M(s$M, MOS2_TZ)
  expect_false(is.na(.raw.full.recording.mean(x, "ENMO")))
  x$imputed$rout[, 5] <- 1
  expect_identical(.raw.full.recording.mean(x, "ENMO"), NA_real_)
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- MOS2_TZ
  IMP <- GGIR::g.impute(s$M, s$I, params_cleaning = P$params_cleaning, desiredtz = MOS2_TZ, dayborder = 0, ID = "x")
  IMP$rout[, 5] <- 1
  S <- GGIR::g.analyse(s$I, s$C, s$M, IMP, params_247 = P$params_247, params_phyact = P$params_phyact,
                       params_general = P$params_general, params_cleaning = P$params_cleaning, ID = "x")
  expect_identical(as_num(S$summary["ENMO_fullRecordingMean"])[[1]], NA_real_)
})

test_that("optional: every metric's columns on a large recording (CANHRACTI_BIG_GT3X) equal GGIR's", {
  skip_if_no_ggir()
  big <- Sys.getenv("CANHRACTI_BIG_GT3X", unset = "")
  if (!nzchar(big) || !file.exists(big)) testthat::skip("CANHRACTI_BIG_GT3X is unset or not a file")
  x <- read.raw.accelerometer(gsub("\\\\", "/", big),
                              params = raw.params(desiredtz = MOS2_TZ, do.enmo = TRUE, do.enmoa = TRUE,
                                                  do.mad = TRUE), sleep = FALSE)
  ms <- as.ggir.milestone(x)
  expect_part2_matches(x, ggir_part2(ms$M, ms$I, ms$C, MOS2_TZ), "large recording")
})
