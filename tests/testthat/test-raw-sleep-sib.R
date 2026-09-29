# Parity tests for R/raw_sleep_sib.R (raw.sib and its helpers) against GGIR's HASIB.
# GGIR's own 781-epoch fixture and the synthetic cases need no data file; the live
# comparisons skip when GGIR is not installed, and the MOS2 and EE tests skip when
# CANHRACTI_GGIR_REF is unset. The series HASIB is given must come from the part-2
# imputed metashort, so the reference tests drive raw.sib from the stored IMP.

ggir_ref_dir <- function() {
  ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
  if (!nzchar(ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is not set; GGIR reference data unavailable")
  }
  if (!dir.exists(ref)) {
    testthat::skip(paste0("CANHRACTI_GGIR_REF does not exist: ", ref))
  }
  ref
}

ggir_ref_file <- function(...) {
  f <- file.path(ggir_ref_dir(), ...)
  if (!file.exists(f)) testthat::skip(paste0("Reference file missing: ", f))
  f
}

skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}

# GGIR's fix_NA_invector, applied by g.sib.det before HASIB sees a series
fix_NA_invector <- function(x) {
  if (length(which(is.na(x) == TRUE)) > 0) {
    x[which(is.na(x) == TRUE)] <- 0
  }
  x
}

sib_memo <- new.env(parent = emptyenv())

# The stored part-1 and part-2 milestones of one reference recording, loaded once
stored_milestones <- function(which = c("mos2", "ee")) {
  which <- match.arg(which)
  key <- paste0("ms_", which)
  if (is.null(sib_memo[[key]])) {
    if (which == "mos2") {
      basic <- ggir_ref_file("out", "output_din", "meta", "basic",
                             "meta_MOS2E39230594.gt3x.RData")
      ms2 <- ggir_ref_file("out", "output_din", "meta", "ms2.out",
                           "MOS2E39230594.gt3x.RData")
    } else {
      basic <- ggir_ref_file("timing_out", "output_timing", "meta", "basic",
                             "meta_EE_left_29.5.2017-05-30.gt3x.RData")
      ms2 <- ggir_ref_file("timing_out", "output_timing", "meta", "ms2.out",
                           "EE_left_29.5.2017-05-30.gt3x.RData")
    }
    e1 <- new.env(); load(basic, envir = e1)
    e2 <- new.env(); load(ms2, envir = e2)
    sib_memo[[key]] <- list(M = e1$M, I = e1$I, desiredtz_part1 = e1$desiredtz_part1,
                            IMP = e2$IMP)
  }
  sib_memo[[key]]
}

# The four series g.sib.det hands to HASIB, from the imputed table
sib_inputs <- function(which = c("mos2", "ee")) {
  ms <- stored_milestones(which)
  list(ws3 = ms$M$windowsizes[1],
       time = format(ms$IMP$metashort[, 1]),
       anglez = fix_NA_invector(as.numeric(as.matrix(
         ms$IMP$metashort[, which(colnames(ms$IMP$metashort) == "anglez")]))),
       ACC = ms$IMP$metashort[["ENMO"]])
}

# GGIR's own HASIB fixture (781 epochs of 5 s)
hasib_fixture <- function() {
  time <- seq(as.POSIXlt(x = "2021-3-3 15:00:00", tz = "Europe/Amsterdam"),
              as.POSIXlt(x = "2021-3-3 16:05:00", tz = "Europe/Amsterdam"), by = 5)
  tmp <- c(rep(0, 10), 10, 20, 30, rep(30, 310), 20, 10, -5, -20, 0, -15, -30, rep(-40, 400))
  anglez <- c(tmp, rep(0, length(time) - length(tmp)))
  set.seed(1245)
  zeroCrossingCount <- round(abs(stats::rnorm(n = length(anglez), mean = 0, sd = 50)))
  zeroCrossingCount[40:500] <- 0
  list(time = time, anglez = anglez, zeroCrossingCount = zeroCrossingCount)
}

# A 3-hour count series at ws3 = 5 with two planted still periods and one loud stretch
planted_counts <- function(nmin = 180, ws3 = 5, seed = 20260915) {
  set.seed(seed)
  nep <- nmin * (60 / ws3)
  z <- round(abs(stats::rnorm(nep, mean = 0, sd = 60)))
  z[(30 * (60 / ws3) + 1):(75 * (60 / ws3))] <- 0    # still period, minutes 31 to 75
  z[(110 * (60 / ws3) + 1):(140 * (60 / ws3))] <- 0  # still period, minutes 111 to 140
  z[(150 * (60 / ws3) + 1):(160 * (60 / ws3))] <-
    round(abs(stats::rnorm(10 * (60 / ws3), 0, 400)))  # loud stretch, minutes 151 to 160
  z
}

# Minutes scored awake, for the impulse-response table
wake_minutes <- function(sib, ws3 = 5) {
  as.integer(sort(unique(ceiling(which(sib[, 1] == 0) / (60 / ws3)))))
}

test_that("S1a raw.sib reproduces GGIR's own HASIB fixture, all five algorithms", {
  fx <- hasib_fixture()
  vanHees2015 <- raw.sib(HASIB.algo = "vanHees2015", timethreshold = 5, anglethreshold = 5,
                         time = fx$time, anglez = fx$anglez, ws3 = 5, zeroCrossingCount = c())
  Sadeh1994 <- raw.sib(HASIB.algo = "Sadeh1994", timethreshold = c(), anglethreshold = c(),
                       time = fx$time, anglez = c(), ws3 = 5,
                       zeroCrossingCount = fx$zeroCrossingCount)
  ColeKripke1992 <- raw.sib(HASIB.algo = "ColeKripke1992", timethreshold = c(),
                            anglethreshold = c(), time = fx$time, anglez = c(), ws3 = 5,
                            zeroCrossingCount = fx$zeroCrossingCount)
  Galland2012 <- raw.sib(HASIB.algo = "Galland2012", timethreshold = c(), anglethreshold = c(),
                         time = fx$time, anglez = c(), ws3 = 5,
                         zeroCrossingCount = fx$zeroCrossingCount)
  Oakley1997 <- raw.sib(HASIB.algo = "Oakley1997", timethreshold = c(), anglethreshold = c(),
                        time = fx$time, anglez = c(), ws3 = 5,
                        zeroCrossingCount = fx$zeroCrossingCount, oakley_threshold = 20)

  expect_equal(nrow(vanHees2015), 781)
  expect_equal(nrow(Sadeh1994), 781)
  expect_equal(nrow(Galland2012), 781)
  expect_equal(nrow(ColeKripke1992), 781)
  expect_equal(nrow(Oakley1997), 781)
  expect_equal(length(which(vanHees2015[, 1] == 1)), 713)
  expect_equal(length(which(Sadeh1994[, 1] == 1)), 372)
  expect_equal(length(which(ColeKripke1992[, 1] == 1)), 372)
  expect_equal(length(which(Galland2012[, 1] == 1)), 396)
  expect_equal(length(which(Oakley1997[, 1] == 1)), 435)

  expect_s3_class(vanHees2015, "data.frame")
  expect_true(is.numeric(vanHees2015[, 1]))
  expect_identical(colnames(vanHees2015), "T5A5")
  expect_identical(colnames(Sadeh1994), "Sadeh1994_ZC")
  expect_identical(colnames(ColeKripke1992), "ColeKripke1992_ZC")
  expect_identical(colnames(Galland2012), "Galland2012_ZC")
  expect_identical(colnames(Oakley1997), "Oakley1997_ZC")
})

test_that("S1a raw.sib is identical() to GGIR:::HASIB on the fixture, all six algorithms", {
  skip_if_no_ggir()
  H <- GGIR:::HASIB
  fx <- hasib_fixture()
  for (algo in c("vanHees2015", "Sadeh1994", "ColeKripke1992", "Galland2012", "Oakley1997")) {
    tt <- if (algo == "vanHees2015") 5 else c()
    az <- if (algo == "vanHees2015") fx$anglez else c()
    zc <- if (algo == "vanHees2015") c() else fx$zeroCrossingCount
    expect_identical(
      raw.sib(algo, timethreshold = tt, anglethreshold = tt, time = fx$time, anglez = az,
              ws3 = 5, zeroCrossingCount = zc, oakley_threshold = 20),
      H(algo, timethreshold = tt, anglethreshold = tt, time = fx$time, anglez = az,
        ws3 = 5, zeroCrossingCount = zc, oakley_threshold = 20),
      info = algo)
  }
  activity <- abs(fx$anglez) / 100
  expect_identical(raw.sib("NotWorn", time = fx$time, ws3 = 5, activity = activity),
                   H("NotWorn", time = fx$time, ws3 = 5, activity = activity))
})

test_that("S1b .raw.sib.rollfun.mat is centred, runs future to past and clamps the edges", {
  x <- 1:10
  for (Ncol in c(7, 11, 13)) {
    m <- .raw.sib.rollfun.mat(x, Ncol)
    expect_identical(dim(m), c(10L, as.integer(Ncol)), info = paste("Ncol", Ncol))
    centre <- ceiling(Ncol / 2)
    want <- outer(1:10, 1:Ncol, function(r, jj) {
      k <- r + centre - jj
      as.numeric(x[pmin(pmax(k, 1), length(x))])   # head clamps to x[1], tail to tail(x, 1)
    })
    expect_identical(m, want, info = paste("Ncol", Ncol))
  }
  # column 1 is three epochs ahead, column 7 three behind
  expect_identical(.raw.sib.rollfun.mat(x, 7)[1, ], c(4, 3, 2, 1, 1, 1, 1))
  expect_identical(.raw.sib.rollfun.mat(x, 7)[4, ], c(7, 6, 5, 4, 3, 2, 1))
  expect_identical(.raw.sib.rollfun.mat(x, 7)[10, ], c(10, 10, 10, 10, 9, 8, 7))
  expect_identical(.raw.sib.rollfun.mat(x, 11)[1, ], c(6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1))
  expect_identical(.raw.sib.rollfun.mat(x, 13)[10, ],
                   c(10, 10, 10, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4))
})

test_that("S1c .raw.sib.sum.per.window drops the trailing partial window", {
  x <- 1:125
  out <- .raw.sib.sum.per.window(x, epochsize = 5, summingwindow = 60)
  expect_identical(length(out), 10L)            # not 11: the 5 trailing epochs are dropped
  expect_identical(out[1], 78)                  # sum(1:12)
  expect_identical(out[10], 1374)               # sum(109:120)
  expect_identical(length(.raw.sib.sum.per.window(1:120, 5, 60)), 10L)
  # a 15 s window at 5 s epochs is a stride of 3
  expect_identical(.raw.sib.sum.per.window(1:9, 5, 15), c(6, 15, 24))
})

test_that(".raw.sib.reformat.output upsamples then pads with zeros or truncates", {
  # pad: two minutes of score, thirty 5 s epochs of time
  expect_identical(as.vector(.raw.sib.reformat.output(c(1, 0), rep("t", 30), 5, 60)),
                   c(rep(1, 12), rep(0, 18)))
  # truncate: two minutes of score, twenty epochs of time
  expect_identical(as.vector(.raw.sib.reformat.output(c(1, 0), rep("t", 20), 5, 60)),
                   c(rep(1, 12), rep(0, 8)))
  expect_identical(dim(.raw.sib.reformat.output(c(1, 0), rep("t", 30), 5, 60)), c(30L, 1L))
})

test_that("S1d the shortest vanHees2015 sib at the defaults is 62 epochs, not 60", {
  anglez <- rep(0, 300)
  anglez[100] <- 100; anglez[101] <- 0; anglez[162] <- 100
  postch <- which(abs(diff(anglez)) > 5)
  expect_identical(postch, c(99L, 100L, 161L, 162L))
  expect_identical(diff(postch), c(1L, 61L, 1L))   # the only qualifying gap is exactly 61
  out <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                 time = rep("t", 300), anglez = anglez, ws3 = 5)
  expect_identical(sum(out[, 1]), 62)             # gap + 1, so 310 s and not 300 s
  expect_identical(range(which(out[, 1] == 1)), c(100L, 161L))
})

test_that("S1d the two whole-recording fallbacks fire on the 10 posture change boundary", {
  stair <- function(nstep, n = 300) {
    a <- rep(0, n)
    for (k in 1:nstep) a[(10 * k):n] <- 100 * k
    a
  }
  nine <- stair(9)
  expect_identical(length(which(abs(diff(nine)) > 5)), 9L)
  expect_identical(max(diff(which(abs(diff(nine)) > 5))), 10L)   # no gap qualifies
  out9 <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                  time = rep("t", 300), anglez = nine, ws3 = 5)
  expect_identical(sum(out9[, 1]), 300)   # fewer than 10 posture changes: the whole recording

  ten <- stair(10)
  expect_identical(length(which(abs(diff(ten)) > 5)), 10L)
  out10 <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                   time = rep("t", 300), anglez = ten, ws3 = 5)
  expect_identical(sum(out10[, 1]), 0)    # ten or more: none of it

  # one spike is two posture changes, so this also takes the all-sib branch
  one <- rep(0, 300); one[100] <- 100
  expect_identical(sum(raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                               time = rep("t", 300), anglez = one, ws3 = 5)[, 1]), 300)
  expect_identical(sum(raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                               time = rep("t", 300), anglez = rep(0, 300), ws3 = 5)[, 1]), 300)
  # 299 posture changes and no long gap is all wake
  alt <- rep(c(0, 100), 150)
  expect_identical(length(which(abs(diff(alt)) > 5)), 299L)
  expect_identical(sum(raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                               time = rep("t", 300), anglez = alt, ws3 = 5)[, 1]), 0)
})

test_that("S1d consecutive qualifying gaps merge into one sib period", {
  anglez <- rep(0, 400)
  anglez[c(50, 120, 200, 280)] <- 100
  postch <- which(abs(diff(anglez)) > 5)
  expect_identical(diff(postch), c(1L, 69L, 1L, 79L, 1L, 79L, 1L))
  q1 <- which(diff(postch) > 60)
  expect_identical(length(q1), 3L)                 # three qualifying gaps
  out <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                 time = rep("t", 400), anglez = anglez, ws3 = 5)
  runs <- rle(out[, 1])
  expect_identical(sum(runs$values == 1), 1L)      # but only one run, because they share ends
  expect_identical(sum(out[, 1]), 230)
})

test_that("S1d the vanHees2015 column order is timethreshold outer, anglethreshold inner", {
  anglez <- rep(0, 300); anglez[100] <- 100; anglez[101] <- 0; anglez[162] <- 100
  out <- raw.sib("vanHees2015", timethreshold = c(5, 10), anglethreshold = c(5, 6),
                 time = rep("t", 300), anglez = anglez, ws3 = 5)
  expect_identical(colnames(out), c("T5A5", "T5A6", "T10A5", "T10A6"))
})

test_that("S1e the count algorithms keep their built-in time misalignment", {
  z <- rep(0, 40 * 12)
  z[(20 * 12 + 1):(21 * 12)] <- 1000   # one loud minute, minute 21 of 40
  tm <- rep("t", length(z))
  sadeh <- raw.sib("Sadeh1994", time = tm, ws3 = 5, zeroCrossingCount = z)
  cole <- raw.sib("ColeKripke1992", time = tm, ws3 = 5, zeroCrossingCount = z)
  galland <- raw.sib("Galland2012", time = tm, ws3 = 5, zeroCrossingCount = z)
  oakley <- raw.sib("Oakley1997", time = tm, ws3 = 5, zeroCrossingCount = z,
                    oakley_threshold = 20)
  # the raw 11 minute window around minute 21 is 16..26, plus the 5 minute prefix
  expect_identical(wake_minutes(sadeh), c(1:5, 21:31))
  # the raw 7 minute window is 18..24, plus the 4 minute prefix
  expect_identical(wake_minutes(cole), c(1:4, 22:28))
  # the raw 7 minute window is 18..24, minus 2, plus the trailing zero pad
  expect_identical(wake_minutes(galland), c(16:22, 39:40))
  # the 13 x 15 s kernel is symmetric, so there is no shift at all
  expect_identical(wake_minutes(oakley), 19:23)
})

test_that("S1e the impulse response is identical() to GGIR", {
  skip_if_no_ggir()
  H <- GGIR:::HASIB
  z <- rep(0, 40 * 12)
  z[(20 * 12 + 1):(21 * 12)] <- 1000
  tm <- rep("t", length(z))
  for (algo in c("Sadeh1994", "ColeKripke1992", "Galland2012", "Oakley1997")) {
    expect_identical(raw.sib(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                             oakley_threshold = 20),
                     H(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                       oakley_threshold = 20),
                     info = algo)
  }
})

test_that("the count algorithms score a planted-still-period series exactly", {
  z <- planted_counts()
  tm <- rep("t", length(z))
  expect_identical(length(z), 2160L)
  expect_identical(sum(z), 91535)
  expect_identical(sum(z == 0), 905L)
  expect_identical(sum(raw.sib("Sadeh1994", time = tm, ws3 = 5,
                               zeroCrossingCount = z)[, 1]), 696)
  expect_identical(sum(raw.sib("ColeKripke1992", time = tm, ws3 = 5,
                               zeroCrossingCount = z)[, 1]), 756)
  expect_identical(sum(raw.sib("Galland2012", time = tm, ws3 = 5,
                               zeroCrossingCount = z)[, 1]), 756)
  expect_identical(sum(raw.sib("Oakley1997", time = tm, ws3 = 5, zeroCrossingCount = z,
                               oakley_threshold = 20)[, 1]), 837)
  # the 300 cap applies to Neishabouri counts only, so only Sadeh's two columns differ
  both <- raw.sib("Sadeh1994", time = tm, ws3 = 5, zeroCrossingCount = z, NeishabouriCount = z)
  expect_identical(colnames(both), c("Sadeh1994_ZC", "Sadeh1994_Neishabouri"))
  expect_identical(unname(colSums(both)), c(696, 756))
  for (algo in c("ColeKripke1992", "Galland2012", "Oakley1997")) {
    b <- raw.sib(algo, time = tm, ws3 = 5, zeroCrossingCount = z, NeishabouriCount = z,
                 oakley_threshold = 20)
    expect_identical(colnames(b), paste0(algo, c("_ZC", "_Neishabouri")))
    expect_identical(b[, 1], b[, 2], info = algo)   # no cap in these three
  }
})

test_that("the count algorithms are identical() to GGIR on the planted series", {
  skip_if_no_ggir()
  H <- GGIR:::HASIB
  z <- planted_counts()
  tm <- rep("t", length(z))
  for (algo in c("Sadeh1994", "ColeKripke1992", "Galland2012", "Oakley1997")) {
    expect_identical(raw.sib(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                             oakley_threshold = 20),
                     H(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                       oakley_threshold = 20),
                     info = paste(algo, "ZC"))
    expect_identical(raw.sib(algo, time = tm, ws3 = 5, NeishabouriCount = z,
                             oakley_threshold = 20),
                     H(algo, time = tm, ws3 = 5, NeishabouriCount = z,
                       oakley_threshold = 20),
                     info = paste(algo, "Neishabouri"))
    expect_identical(raw.sib(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                             NeishabouriCount = z, oakley_threshold = 20),
                     H(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                       NeishabouriCount = z, oakley_threshold = 20),
                     info = paste(algo, "both"))
  }
})

test_that("Oakley1997 at a 15, 30 or 60 s epoch duplicates the zero-crossing column", {
  zc <- planted_counts(nmin = 180, ws3 = 30)
  neish <- rev(zc)
  tm <- rep("t", length(zc))
  out <- raw.sib("Oakley1997", time = tm, ws3 = 30, zeroCrossingCount = zc,
                 NeishabouriCount = neish, oakley_threshold = 20)
  expect_identical(colnames(out), c("Oakley1997_ZC", "Oakley1997_Neishabouri"))
  # GGIR assigns counts = zeroCrossingCount regardless of count_type
  expect_identical(out[, 1], out[, 2])
  expect_identical(unname(colSums(out)), c(147, 147))
})

test_that("Galland2012 scores an all-zero count series awake, with no warning", {
  z <- rep(0, 40 * 12)
  out <- expect_silent(raw.sib("Galland2012", time = rep("t", length(z)), ws3 = 5,
                               zeroCrossingCount = z))
  expect_identical(sum(out[, 1]), 0)   # mean(numeric(0)) is NaN, every comparison is NA
})

test_that("an all-zero count series is scored, not refused, which is why the caller must not supply one", {
  z <- rep(0, 600)
  tm <- rep("t", 600)
  expect_identical(sum(raw.sib("Sadeh1994", time = tm, ws3 = 5,
                               zeroCrossingCount = z)[, 1]), 540)
  expect_identical(sum(raw.sib("ColeKripke1992", time = tm, ws3 = 5,
                               zeroCrossingCount = z)[, 1]), 552)
  expect_identical(sum(raw.sib("Oakley1997", time = tm, ws3 = 5, zeroCrossingCount = z,
                               oakley_threshold = 20)[, 1]), 600)
  # with no count series at all the result has the right number of rows and no columns
  for (algo in c("Sadeh1994", "ColeKripke1992", "Galland2012", "Oakley1997")) {
    out <- raw.sib(algo, time = tm, ws3 = 5, anglez = rep(0, 600), oakley_threshold = 20)
    expect_identical(dim(out), c(600L, 0L), info = algo)
  }
})

test_that("a trailing partial minute is scored awake by the zero pad", {
  z <- rep(c(0, 0, 0, 500), length.out = 125)   # 125 epochs = 10 whole minutes plus 5 epochs
  tm <- rep("t", 125)
  for (algo in c("Sadeh1994", "ColeKripke1992", "Galland2012", "Oakley1997")) {
    out <- raw.sib(algo, time = tm, ws3 = 5, zeroCrossingCount = z, oakley_threshold = 20)
    expect_identical(nrow(out), 125L, info = algo)
    expect_identical(sum(out[121:125, 1]), 0, info = algo)   # the unscored tail is awake
  }
})

test_that("series too short for one window fail the same way GGIR does", {
  # under one full minute the three per-minute algorithms error inside their own arithmetic
  z11 <- rep(c(0, 0, 0, 500), length.out = 11)
  expect_error(raw.sib("Sadeh1994", time = rep("t", 11), ws3 = 5, zeroCrossingCount = z11))
  expect_error(raw.sib("ColeKripke1992", time = rep("t", 11), ws3 = 5, zeroCrossingCount = z11))
  expect_error(raw.sib("Galland2012", time = rep("t", 11), ws3 = 5, zeroCrossingCount = z11))
  # Oakley works on 15 s windows, so eleven 5 s epochs still give it three windows
  out <- raw.sib("Oakley1997", time = rep("t", 11), ws3 = 5, zeroCrossingCount = z11,
                 oakley_threshold = 20)
  expect_identical(nrow(out), 11L)
  # exactly one minute: Sadeh's single-row window matrix collapses to a vector and apply fails
  z12 <- rep(c(0, 0, 0, 500), length.out = 12)
  expect_error(raw.sib("Sadeh1994", time = rep("t", 12), ws3 = 5, zeroCrossingCount = z12))
  expect_identical(nrow(raw.sib("ColeKripke1992", time = rep("t", 12), ws3 = 5,
                                zeroCrossingCount = z12)), 12L)
})

test_that("the short-series failures are identical() to GGIR's", {
  skip_if_no_ggir()
  H <- GGIR:::HASIB
  tc <- function(e) tryCatch(e, error = function(err) paste("ERR:", conditionMessage(err)))
  for (nep in c(11, 12, 125, 719)) {
    z <- rep(c(0, 0, 0, 500), length.out = nep)
    tm <- rep("t", nep)
    for (algo in c("Sadeh1994", "ColeKripke1992", "Galland2012", "Oakley1997")) {
      expect_identical(tc(raw.sib(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                                  oakley_threshold = 20)),
                       tc(H(algo, time = tm, ws3 = 5, zeroCrossingCount = z,
                            oakley_threshold = 20)),
                       info = paste(algo, nep))
    }
  }
})

test_that("Cole-Kripke and Oakley refuse epochs longer than a minute, in GGIR's words", {
  expect_error(raw.sib("ColeKripke1992", time = rep("t", 10), ws3 = 120,
                       zeroCrossingCount = 1:10),
               "not designed for epochs larger than 1 minute")
  expect_error(raw.sib("Oakley1997", time = rep("t", 10), ws3 = 120,
                       zeroCrossingCount = 1:10),
               "epoch sizes up to 1 minute")
})

test_that("NotWorn's rollmax fill of 1 forces the edges awake and its fallback is reachable", {
  # all-zero activity: the fill = 1 edges are the only non-zero values, so the threshold is 0
  out <- raw.sib("NotWorn", time = rep("t", 200), ws3 = 5, activity = rep(0, 200))
  expect_identical(sum(out[, 1]), 141)
  expect_identical(range(which(out[, 1] == 1)), c(30L, 170L))  # 29 head, 30 tail forced awake
  expect_identical(sum(out[1:29, 1]), 0)
  expect_identical(colnames(out), "NotWorn")
  # a series whose minimum is above sd * 0.05 takes the quantile fallback
  act <- c(rep(1, 100), rep(1.0001, 100))
  out2 <- raw.sib("NotWorn", time = rep("t", 200), ws3 = 5, activity = act)
  expect_identical(sum(out2[, 1]), 100)
})

test_that("NotWorn is identical() to GGIR on both threshold branches", {
  skip_if_no_ggir()
  H <- GGIR:::HASIB
  for (act in list(rep(0, 200), c(rep(1, 100), rep(1.0001, 100)),
                   abs(sin(seq_len(600) / 7)) / 50)) {
    expect_identical(raw.sib("NotWorn", time = rep("t", length(act)), ws3 = 5, activity = act),
                     H("NotWorn", time = rep("t", length(act)), ws3 = 5, activity = act))
  }
})

test_that("S1g an unrecognised HASIB.algo raises a named error listing the valid values", {
  e <- tryCatch(raw.sib("Sadeh", time = rep("t", 10), ws3 = 5, anglez = rep(0, 10)),
                error = function(e) e)
  expect_s3_class(e, "canhrActi_raw_sleep_error")
  expect_match(conditionMessage(e), "unknown HASIB.algo", fixed = TRUE)
  expect_match(conditionMessage(e), '"Sadeh"', fixed = TRUE)
  for (valid in c("vanHees2015", "Sadeh1994", "ColeKripke1992", "Galland2012",
                  "Oakley1997", "NotWorn")) {
    expect_match(conditionMessage(e), valid, fixed = TRUE)
  }
  expect_identical(e$HASIB.algo, "Sadeh")
  # the empty and multi-element cases error too
  expect_error(raw.sib(character(0), time = rep("t", 10), ws3 = 5, anglez = rep(0, 10)),
               class = "canhrActi_raw_sleep_error")
  expect_error(raw.sib(c("data", "vanHees2015"), time = rep("t", 10), ws3 = 5,
                       anglez = rep(0, 10)),
               class = "canhrActi_raw_sleep_error")
  expect_error(raw.sib(NA_character_, time = rep("t", 10), ws3 = 5, anglez = rep(0, 10)),
               class = "canhrActi_raw_sleep_error")
})

test_that("S1g GGIR errors on the same input, so the port changes the message and nothing else", {
  skip_if_no_ggir()
  expect_error(GGIR:::HASIB("Sadeh", time = rep("t", 10), ws3 = 5, anglez = rep(0, 10)))
})

test_that("raw.sib on MOS2's imputed anglez gives GGIR's 43664 sib epochs", {
  inp <- sib_inputs("mos2")
  out <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5, time = inp$time,
                 anglez = inp$anglez, ws3 = inp$ws3, zeroCrossingCount = c(),
                 NeishabouriCount = c(), activity = inp$ACC, oakley_threshold = 20)
  expect_identical(nrow(out), 118800L)
  expect_identical(colnames(out), "T5A5")
  expect_identical(sum(out[, 1]), 43664)
  runs <- rle(out[, 1])
  expect_identical(length(which(abs(diff(inp$anglez)) > 5)), 27054L)          # posture changes
  expect_identical(length(which(diff(which(abs(diff(inp$anglez)) > 5)) > 60)), 279L)  # gaps
  expect_identical(sum(runs$values == 1), 223L)                               # merged runs
  expect_identical(min(runs$lengths[runs$values == 1]), 63L)                  # 315 s, over 310
})

test_that("the part-1 series gives a different answer, which is why part 3 reads IMP", {
  ms <- stored_milestones("mos2")
  raw_anglez <- fix_NA_invector(as.numeric(as.matrix(ms$M$metashort$anglez)))
  expect_identical(sum(ms$M$metashort$anglez != ms$IMP$metashort$anglez), 65880L)
  out <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                 time = format(ms$M$metashort[, 1]), anglez = raw_anglez,
                 ws3 = ms$M$windowsizes[1], activity = ms$M$metashort$ENMO)
  expect_identical(sum(out[, 1]), 76793)
})

test_that("the threshold sweeps on MOS2 give GGIR's counts and GGIR's column names", {
  inp <- sib_inputs("mos2")
  sib <- function(tt, at) {
    raw.sib("vanHees2015", timethreshold = tt, anglethreshold = at, time = inp$time,
            anglez = inp$anglez, ws3 = inp$ws3, activity = inp$ACC)
  }
  four <- sib(c(5, 10), c(5, 6))
  expect_identical(colnames(four), c("T5A5", "T5A6", "T10A5", "T10A6"))
  expect_identical(unname(colSums(four)), c(43664, 46592, 31671, 32607))
  expect_identical(unname(colSums(sib(10, 5))), 31671)
  expect_identical(unname(colSums(sib(5, 10))), 54252)
  three_t <- sib(c(5, 7.5, 15), 5)
  expect_identical(colnames(three_t), c("T5A5", "T7.5A5", "T15A5"))
  expect_identical(unname(colSums(three_t)), c(43664, 37516, 20383))
  three_a <- sib(5, c(3, 5, 8))
  expect_identical(colnames(three_a), c("T5A3", "T5A5", "T5A8"))
  expect_identical(unname(colSums(three_a)), c(39566, 43664, 50681))
})

test_that("NotWorn on MOS2 gives GGIR's threshold and its 22155 sib epochs", {
  inp <- sib_inputs("mos2")
  out <- raw.sib("NotWorn", time = inp$time, anglez = inp$anglez, ws3 = inp$ws3,
                 activity = inp$ACC)
  expect_identical(colnames(out), "NotWorn")
  expect_identical(sum(out[, 1]), 22155)
  # the threshold itself, recomputed as HASIB does
  activity2 <- zoo::rollmax(x = inp$ACC, k = 300 / inp$ws3, fill = 1)
  nonzero <- which(activity2 != 0)
  threshold <- stats::sd(activity2[nonzero], na.rm = TRUE) * 0.05
  expect_equal(threshold, 0.0074975401882514461, tolerance = 1e-12)
  expect_identical(min(inp$ACC), 0)            # so the quantile fallback does not fire
  expect_false(threshold < min(inp$ACC))
})

test_that("raw.sib is identical() to GGIR:::HASIB on the stored MOS2 and EE series", {
  skip_if_no_ggir()
  H <- GGIR:::HASIB
  for (rec in c("mos2", "ee")) {
    inp <- sib_inputs(rec)
    expect_identical(
      raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5, time = inp$time,
              anglez = inp$anglez, ws3 = inp$ws3, activity = inp$ACC),
      H("vanHees2015", timethreshold = 5, anglethreshold = 5, time = inp$time,
        anglez = inp$anglez, ws3 = inp$ws3, activity = inp$ACC),
      info = paste(rec, "vanHees2015"))
    expect_identical(
      raw.sib("vanHees2015", timethreshold = c(5, 10), anglethreshold = c(5, 6),
              time = inp$time, anglez = inp$anglez, ws3 = inp$ws3, activity = inp$ACC),
      H("vanHees2015", timethreshold = c(5, 10), anglethreshold = c(5, 6),
        time = inp$time, anglez = inp$anglez, ws3 = inp$ws3, activity = inp$ACC),
      info = paste(rec, "vanHees2015 sweep"))
    expect_identical(
      raw.sib("NotWorn", time = inp$time, anglez = inp$anglez, ws3 = inp$ws3,
              activity = inp$ACC),
      H("NotWorn", time = inp$time, anglez = inp$anglez, ws3 = inp$ws3,
        activity = inp$ACC),
      info = paste(rec, "NotWorn"))
  }
})

test_that("raw.sib on EE's imputed anglez gives 41209 sib epochs and 26138 for NotWorn", {
  inp <- sib_inputs("ee")
  ms <- stored_milestones("ee")
  expect_identical(sum(ms$M$metashort$anglez != ms$IMP$metashort$anglez), 19979L)
  out <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5, time = inp$time,
                 anglez = inp$anglez, ws3 = inp$ws3, activity = inp$ACC)
  expect_identical(nrow(out), 120960L)
  expect_identical(sum(out[, 1]), 41209)
  expect_identical(sum(raw.sib("NotWorn", time = inp$time, ws3 = inp$ws3,
                               activity = inp$ACC)[, 1]), 26138)
})

test_that("the imputed series raw.sib is driven from is the one GGIR's g.impute produces", {
  skip_if_no_ggir()
  ms <- stored_milestones("mos2")
  IMP2 <- GGIR:::g.impute(M = ms$M, I = ms$I,
                          params_cleaning = GGIR::load_params()$params_cleaning,
                          desiredtz = ms$desiredtz_part1, dayborder = 0,
                          acc.metric = "ENMO", ID = "MOS2")
  expect_identical(IMP2$metashort, ms$IMP$metashort)
  anglez <- fix_NA_invector(as.numeric(as.matrix(
    IMP2$metashort[, which(colnames(IMP2$metashort) == "anglez")])))
  out <- raw.sib("vanHees2015", timethreshold = 5, anglethreshold = 5,
                 time = format(IMP2$metashort[, 1]), anglez = anglez,
                 ws3 = ms$M$windowsizes[1], activity = IMP2$metashort[["ENMO"]])
  expect_identical(sum(out[, 1]), 43664)
})
