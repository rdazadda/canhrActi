# Parity tests for R/raw_impute.R against the average-day block of GGIR's g.impute and
# the orientation block of g.analyse. Part 3 reads the imputed IMP$metashort, not
# M$metashort; on MOS2 65880 anglez values differ between the two. Reference data live
# in the folder named by CANHRACTI_GGIR_REF; tests skip when a file or GGIR is missing.

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
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
MOS2_MS2 <- function() ref_file("out", "output_din", "meta", "ms2.out", "MOS2E39230594.gt3x.RData")
EE_RDATA <- function() ref_file("timing_out", "output_timing", "meta", "basic",
                                "meta_EE_left_29.5.2017-05-30.gt3x.RData")
EE_MS2 <- function() ref_file("timing_out", "output_timing", "meta", "ms2.out",
                              "EE_left_29.5.2017-05-30.gt3x.RData")

MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to
EE_TZ <- "Europe/Helsinki"

memo <- new.env()

stored <- function(which = c("mos2", "ee")) {
  which <- match.arg(which)
  key <- paste0("s_", which)
  if (is.null(memo[[key]])) {
    p1 <- if (which == "mos2") MOS2_RDATA() else EE_RDATA()
    p2 <- if (which == "mos2") MOS2_MS2() else EE_MS2()
    skip_if_no_file(p1); skip_if_no_file(p2)
    e1 <- new.env(); load(p1, envir = e1)
    e2 <- new.env(); load(p2, envir = e2)
    memo[[key]] <- list(M = e1$M, I = e1$I, C = e1$C, IMP = e2$IMP, SUM = e2$SUM,
                        tz = if (which == "mos2") MOS2_TZ else EE_TZ)
  }
  memo[[key]]
}
# live g.impute with load_params defaults and optional params_cleaning overrides
ggir_impute <- function(M, I, desiredtz, dayborder = 0, ...) {
  pc <- GGIR::load_params()$params_cleaning
  ov <- list(...)
  for (nm in names(ov)) pc[nm] <- list(ov[[nm]])
  GGIR::g.impute(M, I, params_cleaning = pc, desiredtz = desiredtz, dayborder = dayborder, ID = "x")
}
# a subset of a stored M (long epoch rows), with the matching short epochs
subset_M <- function(M, rows) {
  n <- M$windowsizes[2] / M$windowsizes[1]
  M$metalong <- M$metalong[rows, ]
  rownames(M$metalong) <- NULL
  short <- ((rows[1] - 1) * n + 1):(rows[length(rows)] * n)
  M$metashort <- M$metashort[short, ]
  rownames(M$metashort) <- NULL
  M
}
# a canhrActi_raw_meta built from a GGIR M list
meta_from_M <- function(M, desiredtz) {
  add_time <- function(tbl) {
    tbl$time <- as.numeric(.raw.iso8601.to.posix(tbl$timestamp, tz = desiredtz))
    tbl[c("timestamp", "time", setdiff(names(tbl), c("timestamp", "time")))]
  }
  structure(list(metashort = add_time(M$metashort), metalong = add_time(M$metalong),
                 qclog = M$QClog, wday = M$wday, wdayname = M$wdayname, windowsizes = M$windowsizes,
                 filecorrupt = M$filecorrupt, filetooshort = M$filetooshort,
                 nfilepagesskipped = M$NFilePagesSkipped, tail_expansion_log = NULL, nonwear = NULL,
                 settings = list(desiredtz = desiredtz),
                 file = list(path = NA_character_, filename = "from_M")),
            class = "canhrActi_raw_meta")
}
first_diff <- function(a, b) {
  i <- which(a != b)[1]
  if (is.na(i)) "no element differs" else paste0("first difference at ", i, ": ", a[i], " vs ", b[i])
}
# every column of two tables under identical(), reporting the first differing value
expect_table_identical <- function(got, ref, label) {
  expect_identical(names(got), names(ref), label = paste(label, "names"))
  expect_identical(nrow(got), nrow(ref), label = paste(label, "nrow"))
  for (nm in names(ref)) {
    expect_identical(got[[nm]], ref[[nm]], label = paste0(label, " column ", nm),
                     info = first_diff(got[[nm]], ref[[nm]]))
  }
  expect_identical(got, ref, label = paste(label, "whole table"))
  invisible(NULL)
}
# the plain vanHees2015 sustained-inactivity rule, kept independent of the sib module
sib_count_vanhees <- function(anglez, timethreshold = 5, anglethreshold = 5, epochsize = 5) {
  sdl1 <- rep(0, length(anglez))
  postch <- which(abs(diff(anglez)) > anglethreshold)
  q1 <- which(diff(postch) > timethreshold * (60/epochsize))
  if (length(q1) > 0) {
    for (gi in 1:length(q1)) sdl1[postch[q1[gi]]:postch[q1[gi] + 1]] <- 1
  }
  sum(sdl1)
}

test_that("S0a MOS2: .raw.impute.averageday reproduces IMP$metashort, averageday and dcomplscore", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  expect_identical(nrow(s$M$metashort), 118800L)
  expect_identical(colnames(s$M$metashort), c("timestamp", "anglez", "ENMO"))
  r5 <- as.numeric(as.matrix(s$IMP$rout[, 5]))
  got <- .raw.impute.averageday(s$M$metashort, r5, s$IMP$r5long, s$M$windowsizes[1])
  expect_table_identical(got$metashort, s$IMP$metashort, "MOS2 IMP$metashort")
  expect_identical(got$averageday, s$IMP$averageday)
  expect_identical(dim(got$averageday), c(17280L, 2L))   # wpd = 1440 * (60/5), two metrics
  expect_identical(got$dcomplscore, s$IMP$dcomplscore)
  expect_identical(got$dcomplscore, 1)
  # every invalid epoch was replaced and every valid epoch was left alone
  r5long <- as.numeric(s$IMP$r5long)
  expect_identical(sum(r5long != 0), 65880L)
  expect_identical(got$metashort$anglez[r5long == 0], s$M$metashort$anglez[r5long == 0])
  expect_identical(got$metashort$ENMO[r5long == 0], s$M$metashort$ENMO[r5long == 0])
  # the average day is the mean over the day grid anchored on the recording start
  metr <- s$M$metashort$anglez
  metr[r5long != 0] <- NA
  imp <- matrix(NA, 17280, 7)
  for (j in 1:6) imp[, j] <- metr[((j - 1) * 17280 + 1):(j * 17280)]
  last <- metr[(6 * 17280 + 1):length(metr)]
  imp[1:length(last), 7] <- last
  av <- rowMeans(imp, na.rm = TRUE)
  av[is.nan(av) | is.na(av)] <- 0
  expect_identical(av, s$IMP$averageday[, 1])
})

test_that("S0a EE: .raw.impute.averageday reproduces IMP$metashort, averageday and dcomplscore", {
  skip_if_no_ggir_ref()
  s <- stored("ee")
  expect_identical(nrow(s$M$metashort), 120960L)
  r5 <- as.numeric(as.matrix(s$IMP$rout[, 5]))
  got <- .raw.impute.averageday(s$M$metashort, r5, s$IMP$r5long, s$M$windowsizes[1])
  expect_table_identical(got$metashort, s$IMP$metashort, "EE IMP$metashort")
  expect_identical(got$averageday, s$IMP$averageday)
  expect_identical(dim(got$averageday), c(17280L, 2L))
  expect_identical(got$dcomplscore, s$IMP$dcomplscore)
  expect_identical(got$dcomplscore, 1)
})

test_that("S0b MOS2 and EE: canhrActi's raw.wear.decision drives the same imputation", {
  skip_if_no_ggir_ref()
  for (which in c("mos2", "ee")) {
    s <- stored(which)
    w <- raw.wear.decision(s$M, desiredtz = s$tz)
    expect_identical(w$rout, s$IMP$rout, label = paste(which, "rout"))
    expect_identical(w$r5long, s$IMP$r5long, label = paste(which, "r5long"))
    expect_identical(unname(colSums(w$rout)),
                     if (which == "mos2") c(343, 0, 23, 0, 366) else c(106, 0, 5, 0, 111),
                     label = paste(which, "colSums(rout)"))
    imp <- raw.impute(s$M, w, desiredtz = s$tz)
    expect_s3_class(imp, "canhrActi_raw_imputed")
    expect_identical(imp$status, "ok")
    expect_table_identical(imp$metashort, s$IMP$metashort, paste(which, "raw.impute metashort"))
    expect_identical(imp$averageday, s$IMP$averageday, label = paste(which, "averageday"))
    expect_identical(imp$dcomplscore, s$IMP$dcomplscore, label = paste(which, "dcomplscore"))
    expect_identical(imp$rout, s$IMP$rout, label = paste(which, "rout carried through"))
    expect_identical(imp$r5long, s$IMP$r5long, label = paste(which, "r5long carried through"))
    expect_identical(imp$windowsizes, c(5, 900, 3600), label = paste(which, "windowsizes"))
    expect_identical(imp$n_invalid_short, if (which == "mos2") 65880L else 19980L,
                     label = paste(which, "n_invalid_short"))
    expect_identical(imp$n_expanded_short, 0L, label = paste(which, "n_expanded_short"))
    # both reference recordings are wrist, so there is no axis estimate
    expect_identical(imp$if_hip_long_axis_id, "", label = paste(which, "if_hip_long_axis_id"))
    expect_true(is.na(s$SUM$summary$if_hip_long_axis_id))
  }
})

test_that("S0c MOS2 end to end: read.raw.accelerometer then raw.impute gives IMP$metashort", {
  skip_if_no_ggir_ref()
  skip_if_no_file(MOS2())
  s <- stored("mos2")
  # the stored calibration is supplied so that the 60 s read is not also a calibration run
  x <- read.raw.accelerometer(MOS2(), desiredtz = MOS2_TZ, calibration = s$C)
  expect_s3_class(x, "canhrActi_raw")
  expect_false(x$status$corrupt)
  expect_false(x$status$too_short)
  expect_identical(as.ggir.M(x$meta)$metashort, s$M$metashort)
  imp <- raw.impute(x)      # meta and wear both taken from the canhrActi_raw object
  expect_identical(imp$status, "ok")
  expect_table_identical(imp$metashort, s$IMP$metashort, "MOS2 end to end metashort")
  expect_identical(imp$averageday, s$IMP$averageday)
  expect_identical(imp$dcomplscore, s$IMP$dcomplscore)
  expect_identical(imp$file$filename, "MOS2E39230594.gt3x")
  expect_identical(as.ggir.IMP(imp), s$IMP)
  imp2 <- raw.impute(x$meta, x$wear)
  expect_identical(imp2$metashort, imp$metashort)
})

test_that("S0e trap 12: the imputed series differs from the part-1 series and changes the sib count", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  M <- s$M; IMP <- s$IMP
  expect_identical(M$metashort$timestamp, IMP$metashort$timestamp)
  expect_identical(sum(M$metashort$anglez != IMP$metashort$anglez), 65880L)
  expect_identical(sum(M$metashort$ENMO != IMP$metashort$ENMO), 61227L)
  expect_identical(nrow(M$metashort), 118800L)
  # exactly the invalid epochs changed
  r5long <- as.numeric(IMP$r5long)
  expect_identical(sum(r5long != 0), 65880L)
  expect_identical(which(M$metashort$anglez != IMP$metashort$anglez), which(r5long != 0))
  # the consequence for part 3
  expect_identical(sib_count_vanhees(M$metashort$anglez), 76793)
  expect_identical(sib_count_vanhees(IMP$metashort$anglez), 43664)
  imp <- raw.impute(M, raw.wear.decision(M, desiredtz = MOS2_TZ), desiredtz = MOS2_TZ)
  expect_identical(sib_count_vanhees(imp$metashort$anglez), 43664)
  # rec_starttime comes from the imputed frame
  expect_identical(imp$metashort[1, 1], IMP$metashort[1, 1])
  expect_identical(imp$metashort[1, 1], "2025-10-07T20:30:00-0800")
  # EE: 19980 invalid epochs but only 19979 differing values, one imputed back to itself
  e <- stored("ee")
  expect_identical(e$M$metashort$timestamp, e$IMP$metashort$timestamp)
  expect_identical(sum(as.numeric(e$IMP$r5long) != 0), 19980L)
  expect_identical(sum(e$M$metashort$anglez != e$IMP$metashort$anglez), 19979L)
  expect_identical(sum(e$M$metashort$ENMO != e$IMP$metashort$ENMO), 19894L)
  expect_identical(sib_count_vanhees(e$M$metashort$anglez), 43655)
  expect_identical(sib_count_vanhees(e$IMP$metashort$anglez), 41209)
})

test_that("T2 MOS2 and EE: raw.impute is identical to a live GGIR g.impute, whole IMP list", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  for (which in c("mos2", "ee")) {
    s <- stored(which)
    live <- ggir_impute(s$M, s$I, s$tz)
    expect_identical(live, s$IMP, label = paste(which, "live g.impute against the stored milestone"))
    imp <- raw.impute(s$M, raw.wear.decision(s$M, desiredtz = s$tz), desiredtz = s$tz)
    expect_table_identical(imp$metashort, live$metashort, paste(which, "live metashort"))
    expect_identical(imp$averageday, live$averageday, label = paste(which, "live averageday"))
    expect_identical(imp$dcomplscore, live$dcomplscore, label = paste(which, "live dcomplscore"))
    expect_identical(as.ggir.IMP(imp), live, label = paste(which, "live IMP list"))
  }
})

test_that("as.ggir.IMP emits g.impute's 14 members in GGIR's order", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  imp <- raw.impute(s$M, raw.wear.decision(s$M, desiredtz = MOS2_TZ), desiredtz = MOS2_TZ)
  G <- as.ggir.IMP(imp)
  expect_identical(names(G), c("metashort", "rout", "r5long", "dcomplscore", "averageday",
                               "windowsizes", "data_masking_strategy", "LC", "LC2",
                               "hrs.del.start", "hrs.del.end", "maxdur", "nonwearHoursFiltered",
                               "nonwearEventsFiltered"))
  expect_identical(names(G), names(s$IMP))
  expect_identical(G, s$IMP)
  # the pass-throughs come from the wear decision, not from defaults
  expect_identical(G$data_masking_strategy, 1)
  expect_identical(G$LC, 0L)
  expect_identical(G$LC2, 0L)
  expect_identical(G$nonwearHoursFiltered, 0)
  expect_error(as.ggir.IMP(s$M), "canhrActi_raw_imputed")
})

test_that("trap 19: en, step_count and marker are dispatched by column NAME", {
  # five days at ws3 = 60, every metric constant within a day, so mean, median and minimum differ
  ws3 <- 60; wpd <- 1440; nday <- 5
  v <- c(1, 2, 4, 8, 16)
  val <- rep(v, each = wpd)
  ms <- data.frame(timestamp = paste0("t", seq_len(nday * wpd)),
                   ENMO = val, step_count = val, marker = val, en = val,
                   stringsAsFactors = FALSE)
  r5long <- matrix(0, nday * wpd, 1)
  r5long[wpd + (1:10)] <- 1                      # day 2 missing at epoch-of-day 1 to 10
  r5long[seq(20, nday * wpd, by = wpd)] <- 1     # epoch-of-day 20 missing on every day
  a <- .raw.impute.averageday(ms, rep(0, nday), r5long, ws3)
  expect_identical(dim(a$averageday), c(1440L, 4L))
  # a fully observed epoch-of-day
  expect_identical(a$averageday[30, 1], mean(v))          # ENMO
  expect_identical(a$averageday[30, 2], stats::median(v)) # step_count
  expect_identical(a$averageday[30, 3], min(v))           # marker
  expect_identical(a$averageday[30, 4], mean(v))          # en
  expect_identical(a$averageday[30, ], c(6.2, 4, 1, 6.2))
  # one day missing: marker sees the NA as 0
  expect_identical(a$averageday[5, ], c(mean(c(1, 4, 8, 16)), stats::median(c(1, 4, 8, 16)), 0,
                                        mean(c(1, 4, 8, 16))))
  expect_identical(a$averageday[5, 1], 7.25)
  expect_identical(unlist(a$metashort[wpd + 5, c("ENMO", "step_count", "marker", "en")]),
                   c(ENMO = 7.25, step_count = 6, marker = 0, en = 7.25))
  # every day missing: 1 for the column named en, 0 for every other
  expect_identical(a$averageday[20, ], c(0, 0, 0, 1))
  expect_identical(unique(a$metashort$ENMO[seq(20, nday * wpd, by = wpd)]), 0)
  expect_identical(unique(a$metashort$en[seq(20, nday * wpd, by = wpd)]), 1)
  # dcomplscore reports the last metric column (en), whose average day has one NaN epoch
  expect_identical(a$dcomplscore, 1439 / 1440)
  # with the columns reversed dcomplscore reports ENMO
  b <- .raw.impute.averageday(ms[, c("timestamp", "en", "marker", "step_count", "ENMO")],
                              rep(0, nday), r5long, ws3)
  expect_identical(b$dcomplscore, 1439 / 1440)
  expect_identical(b$averageday[20, ], c(1, 0, 0, 0))
  expect_identical(a$metashort$ENMO[100], 1)
  expect_identical(a$metashort$ENMO[wpd + 100], 2)
  expect_identical(a$metashort$timestamp, ms$timestamp)
})

test_that("trap 19 against live GGIR: en, step_count and marker columns on the MOS2 tables", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored("mos2")
  set.seed(1)
  n <- nrow(s$M$metashort)
  Mx <- s$M
  Mx$metashort$en <- round(1 + stats::rnorm(n, 0, 0.02), 4)
  Mx$metashort$step_count <- as.numeric(stats::rbinom(n, 3, 0.05))
  Mx$metashort$marker <- as.numeric(stats::rbinom(n, 1, 0.01))
  live <- ggir_impute(Mx, s$I, MOS2_TZ)
  got <- raw.impute(Mx, raw.wear.decision(Mx, desiredtz = MOS2_TZ), desiredtz = MOS2_TZ)
  expect_table_identical(got$metashort, live$metashort, "en/step_count/marker metashort")
  expect_identical(got$averageday, live$averageday)
  expect_identical(got$dcomplscore, live$dcomplscore)
  expect_identical(dim(got$averageday), c(17280L, 5L))
  expect_identical(as.ggir.IMP(got), live)
})

test_that("dcomplscore reports the last metric column, as GGIR does", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  w <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  r5 <- as.numeric(as.matrix(w$rout[, 5]))
  # force one epoch-of-day to be missing on every day, so that the average day has a NaN
  r5long <- w$r5long
  r5long[seq(5000, length(r5long), by = 17280)] <- 1
  set.seed(2)
  ms <- s$M$metashort
  ms$marker <- 0
  a <- .raw.impute.averageday(ms, r5, r5long, 5)                       # last column marker
  b <- .raw.impute.averageday(ms[, c("timestamp", "marker", "ENMO", "anglez")], r5, r5long, 5)
  expect_identical(a$dcomplscore, 1)              # marker never produces NaN (NA becomes 0)
  expect_identical(b$dcomplscore, 17279 / 17280)  # anglez does, at the forced epoch-of-day
  expect_false(identical(a$dcomplscore, b$dcomplscore))
})

test_that("a recording under one day is not imputed and dcomplscore falls back to r5long", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored("mos2")
  Ms <- subset_M(s$M, 1:96)                       # 96 long epochs = 17280 short = exactly wpd
  expect_identical(nrow(Ms$metashort), 17280L)
  live <- ggir_impute(Ms, s$I, MOS2_TZ)
  w <- raw.wear.decision(Ms, desiredtz = MOS2_TZ)
  got <- raw.impute(Ms, w, desiredtz = MOS2_TZ)
  expect_table_identical(got$metashort, live$metashort, "under one day metashort")
  expect_identical(got$averageday, live$averageday)
  expect_true(all(got$averageday == 0))           # nothing is written into it
  expect_identical(got$dcomplscore, live$dcomplscore)
  expect_identical(got$dcomplscore, length(which(w$r5long == 0)) / 17280)
  expect_identical(got$dcomplscore, 16380 / 17280)
  expect_equal(got$dcomplscore, 0.947916666666667, tolerance = 1e-14)
  # nothing was imputed: the values are the part-1 values rounded to 4 decimals
  expect_identical(got$metashort$anglez, round(Ms$metashort$anglez, 4))
  expect_identical(got$metashort$ENMO, round(Ms$metashort$ENMO, 4))
})

test_that("trap 18: tail-expansion epochs (r5long -1) are never imputed", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored("mos2")
  cut_long <- which(s$M$metalong$timestamp == "2025-10-09T21:45:00-0800")
  expect_length(cut_long, 1)
  meta_x <- .raw.tail.expand(meta_from_M(subset_M(s$M, 1:cut_long), MOS2_TZ),
                             recordingEndSleepHour = 19, dayborder = 0, desiredtz = MOS2_TZ)
  expect_identical(meta_x$tail_expansion_log, list(short = 7560L, long = 42L))
  Mx <- as.ggir.M(meta_x)
  w <- raw.wear.decision(meta_x)
  live <- ggir_impute(Mx, s$I, MOS2_TZ)
  got <- raw.impute(meta_x, w)
  expect_identical(got$n_expanded_short, 7560L)
  expect_identical(got$n_invalid_short, 900L)
  expect_table_identical(got$metashort, live$metashort, "tail expansion metashort")
  expect_identical(got$averageday, live$averageday)
  expect_identical(got$dcomplscore, live$dcomplscore)
  # the expanded epochs kept their part-1 values; only the rounding touched them
  expanded <- which(w$r5long == -1)
  expect_length(expanded, 7560L)
  expect_identical(got$metashort$anglez[expanded], round(Mx$metashort$anglez[expanded], 4))
  expect_identical(got$metashort$ENMO[expanded], round(Mx$metashort$ENMO[expanded], 4))
  # and they are left out of the average day as well
  metr <- Mx$metashort$anglez
  metr[as.numeric(w$r5long) != 0] <- NA
  imp <- matrix(NA, 17280, ceiling(length(metr) / 17280))
  for (j in 1:(ncol(imp) - 1)) imp[, j] <- metr[((j - 1) * 17280 + 1):(j * 17280)]
  last <- metr[((ncol(imp) - 1) * 17280 + 1):length(metr)]
  imp[1:length(last), ncol(imp)] <- last
  av <- rowMeans(imp, na.rm = TRUE)
  av[is.nan(av) | is.na(av)] <- 0
  expect_identical(av, got$averageday[, 1])
})

test_that("the part-3 fixtures reproduce their stored IMP, including one with no valid epoch", {
  skip_if_no_ggir_ref()
  fx <- file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures")
  if (!dir.exists(fx)) testthat::skip(paste0("no fixture folder at ", fx))
  dirs <- list.dirs(fx, recursive = FALSE)
  if (!length(dirs)) testthat::skip("the fixture folder is empty")
  seen <- character()
  for (d in dirs) {
    bp <- list.files(file.path(d, "output_din", "meta", "basic"), full.names = TRUE)
    mp <- list.files(file.path(d, "output_din", "meta", "ms2.out"), full.names = TRUE)
    if (!length(bp) || !length(mp)) next
    e1 <- new.env(); load(bp[1], envir = e1)
    e2 <- new.env(); load(mp[1], envir = e2)
    lab <- basename(d)
    seen <- c(seen, lab)
    tz <- if (is.null(e1$desiredtz_part1)) "" else e1$desiredtz_part1
    w <- raw.wear.decision(e1$M, desiredtz = tz)
    expect_identical(w$rout, e2$IMP$rout, label = paste(lab, "rout"))
    expect_identical(w$r5long, e2$IMP$r5long, label = paste(lab, "r5long"))
    got <- raw.impute(e1$M, w, desiredtz = tz)
    expect_table_identical(got$metashort, e2$IMP$metashort, paste(lab, "metashort"))
    expect_identical(got$averageday, e2$IMP$averageday, label = paste(lab, "averageday"))
    expect_identical(got$dcomplscore, e2$IMP$dcomplscore, label = paste(lab, "dcomplscore"))
    expect_identical(as.ggir.IMP(got), e2$IMP, label = paste(lab, "whole IMP list"))
    if (lab %in% c("novalid", "novalid_keepsibs")) {
      # every long epoch non-wear: the average day is all NaN, so every metric goes to 0
      expect_identical(unname(colSums(w$rout)), c(660, 0, 0, 0, 660), label = lab)
      expect_identical(got$n_invalid_short, 118800L, label = lab)
      expect_identical(got$dcomplscore, 0, label = lab)
      expect_true(all(got$averageday == 0), label = lab)
      expect_true(all(got$metashort$anglez == 0), label = lab)
      expect_true(all(got$metashort$ENMO == 0), label = lab)
    }
  }
  if (!length(seen)) testthat::skip("no fixture carries a meta/ms2.out milestone")
  expect_true(length(seen) >= 1)
})

test_that("S0d .raw.longitudinal.axis returns '' for both wrist references and honours the gate", {
  skip_if_no_ggir_ref()
  for (which in c("mos2", "ee")) {
    s <- stored(which)
    expect_identical(.raw.longitudinal.axis(s$IMP$metashort, s$M$windowsizes[1]), "")
    expect_true(is.na(s$SUM$summary$if_hip_long_axis_id))
  }
  epochday <- 17280
  mk <- function(nrows, ax, ay, az) {
    data.frame(timestamp = rep("t", nrows), anglex = ax, angley = ay, anglez = az,
               stringsAsFactors = FALSE)
  }
  set.seed(7)
  # fewer than two whole days: the gate closes
  one <- mk(epochday, stats::rnorm(epochday), stats::rnorm(epochday), stats::rnorm(epochday))
  expect_identical(.raw.longitudinal.axis(one, 5), "")
  # every axis flat: sd 0 gives NA for all three and the result is ""
  flat <- mk(2 * epochday, rep(0, 2 * epochday), rep(0, 2 * epochday), rep(0, 2 * epochday))
  expect_identical(.raw.longitudinal.axis(flat, 5), "")
  # a 24 hour periodic angley wins
  tt <- 1:(2 * epochday)
  per <- mk(2 * epochday, stats::rnorm(2 * epochday), 30 * sin(2 * pi * tt / epochday),
            stats::rnorm(2 * epochday))
  expect_identical(.raw.longitudinal.axis(per, 5), 2L)
  per2 <- mk(2 * epochday, 30 * sin(2 * pi * tt / epochday), stats::rnorm(2 * epochday),
             stats::rnorm(2 * epochday))
  expect_identical(.raw.longitudinal.axis(per2, 5), 1L)
})

test_that("S0d the axis estimate equals GGIR's g.analyse value on a hip-shaped recording", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored("mos2")
  set.seed(4)
  n <- nrow(s$M$metashort)
  Mh <- s$M
  Mh$metashort$anglex <- round(30 * sin(2 * pi * (1:n) / 17280) + stats::rnorm(n, 0, 1), 4)
  Mh$metashort$angley <- round(10 * cos(2 * pi * (1:n) / 17280) + stats::rnorm(n, 0, 5), 4)
  Mh$metashort <- Mh$metashort[, c("timestamp", "anglez", "ENMO", "anglex", "angley")]
  IMPh <- ggir_impute(Mh, s$I, MOS2_TZ)
  got <- raw.impute(Mh, raw.wear.decision(Mh, desiredtz = MOS2_TZ), desiredtz = MOS2_TZ)
  expect_identical(got$metashort, IMPh$metashort)
  expect_identical(got$if_hip_long_axis_id, 1L)
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- MOS2_TZ
  SUM <- GGIR::g.analyse(s$I, s$C, Mh, IMPh, params_247 = P$params_247,
                         params_phyact = P$params_phyact, params_general = P$params_general,
                         params_cleaning = P$params_cleaning, ID = "x")
  expect_identical(as.numeric(SUM$summary$if_hip_long_axis_id),
                   as.numeric(got$if_hip_long_axis_id))
  expect_identical(as.numeric(SUM$summary$if_hip_long_axis_id), 1)
})

test_that("raw.impute checks its input and returns a no-data object without erroring", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  expect_error(raw.impute(1), "canhrActi_raw")
  expect_error(raw.impute(list(a = 1)), "canhrActi_raw")
  expect_error(raw.impute(s$M, list(rout = NULL)), "canhrActi_raw_wear")
  # a corrupt recording: no tables, no error
  meta <- meta_from_M(subset_M(s$M, 1:8), MOS2_TZ)
  meta$filecorrupt <- TRUE
  imp <- raw.impute(meta, NULL)
  expect_identical(imp$status, "no_data")
  expect_null(imp$metashort)
  expect_identical(imp$if_hip_long_axis_id, "")
  expect_true(any(grepl("No epoch tables", imp$messages)))
  # wear NULL runs the wear decision here
  a <- raw.impute(s$M, NULL, desiredtz = MOS2_TZ)
  b <- raw.impute(s$M, raw.wear.decision(s$M, desiredtz = MOS2_TZ), desiredtz = MOS2_TZ)
  expect_identical(a$metashort, b$metashort)
  expect_identical(a$rout, b$rout)
  # a GGIR IMP list is accepted in place of a canhrActi wear decision
  c2 <- raw.impute(s$M, s$IMP, desiredtz = MOS2_TZ)
  expect_identical(c2$metashort, s$IMP$metashort)
  expect_identical(c2$data_masking_strategy, 1)
})

test_that("print method reports the imputation", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  imp <- raw.impute(s$M, raw.wear.decision(s$M, desiredtz = MOS2_TZ), desiredtz = MOS2_TZ)
  out <- capture.output(print.canhrActi_raw_imputed(imp))
  expect_true(any(grepl("118800 short (5 s), of which 65880 imputed (55.5 percent)", out,
                        fixed = TRUE)))
  expect_true(any(grepl("anglez, ENMO", out, fixed = TRUE)))
  expect_true(any(grepl("17280 x 2", out, fixed = TRUE)))
  none <- raw.impute(structure(list(metalong = NULL, metashort = NULL, filecorrupt = TRUE,
                                    windowsizes = c(5, 900, 3600), file = NULL),
                               class = "canhrActi_raw_meta"), NULL)
  out2 <- capture.output(print.canhrActi_raw_imputed(none))
  expect_true(any(grepl("status:      no_data", out2, fixed = TRUE)))
})
