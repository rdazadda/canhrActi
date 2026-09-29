# Parity tests for R/raw_sleep_sri.R (raw.sleep.regularity) against GGIR's
# CalcSleepRegularityIndex. The part-3 epoch series is not stored in the milestone, so
# it is rebuilt once per file with GGIR:::g.sib.det; those tests skip when GGIR is not
# installed. Reference data live in the folder named by CANHRACTI_GGIR_REF. The MOS2
# milestones were stored in the system zone of a machine set to America/Anchorage, so the
# file runs in that zone.

.saved_lc_time <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")   # restored by the last line of this file
withr::local_timezone("America/Anchorage")

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
# The daylight saving fixtures, one level up from the reference folder.
p34_fixture <- function(case) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", case, "output_din")
}

SRI_CASES <- list(
  MOS2 = list(out = c("out", "output_din"),            fn = "MOS2E39230594.gt3x"),
  EE   = list(out = c("timing_out", "output_timing"),  fn = "EE_left_29.5.2017-05-30.gt3x")
)

sri_path <- function(case, what) {
  cc <- SRI_CASES[[case]]
  switch(what,
         basic = do.call(ref_file, c(as.list(cc$out), list("meta", "basic", paste0("meta_", cc$fn, ".RData")))),
         ms2   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms2.out", paste0(cc$fn, ".RData")))),
         ms3   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms3.out", paste0(cc$fn, ".RData")))))
}

# The stored ms3 milestone, as an environment
sri_ms3 <- function(case) {
  p <- sri_path(case, "ms3")
  skip_if_no_file(p)
  e <- new.env(parent = emptyenv())
  load(p, envir = e)
  e
}

# The part-3 epoch series as g.part3 hands it to CalcSleepRegularityIndex, built once.
sri_series <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref()
    skip_if_no_ggir()
    pb <- sri_path(case, "basic"); skip_if_no_file(pb)
    p2 <- sri_path(case, "ms2");   skip_if_no_file(p2)
    e3 <- sri_ms3(case)
    e1 <- new.env(parent = emptyenv()); load(pb, envir = e1)
    e2 <- new.env(parent = emptyenv()); load(p2, envir = e2)
    # desiredtz comes from the stored run
    tz <- e3$desiredtz_part1
    P <- GGIR::load_params(topic = c("sleep", "metrics", "general"))
    SLE <- GGIR:::g.sib.det(M = e1$M, IMP = e2$IMP, I = e1$I, twd = c(-12, 12),
                            acc.metric = P$params_general$acc.metric, desiredtz = tz,
                            myfun = c(), sensor.location = P$params_general$sensor.location,
                            params_sleep = P$params_sleep, zc.scale = P$params_metrics$zc.scale)
    A <- SLE$output
    A <- A[, -which(names(A) == "spt_crude_estimate")]
    out <- list(data = A, epochsize = e1$M$windowsizes[1], desiredtz = tz,
                stored = e3$SleepRegularityIndex)
    assign(case, out, envir = cache)
    out
  }
})

# A synthetic part-3 series: n_days whole calendar days from an absolute 30 s boundary.
sri_synth <- function(n_days, epochsize, state_fun, tz = "UTC",
                      start = "2024-01-01 00:00:00") {
  n <- floor((86400 * n_days) / epochsize)
  tt <- seq(as.POSIXct(start, tz = tz), by = epochsize, length.out = n)
  data.frame(time = tt,
             invalid = 0,
             night = ((as.numeric(tt) - as.numeric(tt[1])) %/% 86400) + 1,
             T5A5 = state_fun(tt, seq_len(n)))
}

test_that("S5a MOS2 SleepRegularityIndex is identical to the stored 7 x 5 table", {
  s <- sri_series("MOS2")
  expect_identical(s$desiredtz, "")
  expect_identical(s$epochsize, 5)
  expect_identical(colnames(s$data), c("time", "invalid", "night", "T5A5"))

  got <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz,
                              SRI1_smoothing_wsize_hrs = NULL, SRI1_smoothing_frac = NULL)

  expect_s3_class(got, "data.frame")
  expect_identical(dim(got), c(7L, 5L))
  expect_identical(colnames(got),
                   c("day", "SleepRegularityIndex", "weekday", "frac_valid", "date"))
  expect_identical(got$day, 1:7)
  expect_identical(got$SleepRegularityIndex, c(17.619, 30.256, 46.738, 0, 0, 0, 0))
  expect_identical(got$frac_valid, c(0.1458, 0.9479, 0.9688, 0, 0, 0, 0))
  expect_identical(got$weekday, c("Tuesday", "Wednesday", "Thursday", "Friday",
                                  "Saturday", "Sunday", "Monday"))
  expect_identical(got$date, c("07/10/2025", "08/10/2025", "09/10/2025", "10/10/2025",
                               "11/10/2025", "12/10/2025", "13/10/2025"))
  # rows 4 to 7 are "no valid pair", not a real index of zero
  expect_true(all(got$frac_valid[got$SleepRegularityIndex == 0] == 0))
  # day 1 covers 20:30 to midnight only, 420 of 2880 thirty second slots
  expect_identical(got$frac_valid[1], round(420 / 2880, 4))

  expect_identical(got, s$stored)
})

test_that("S5a MOS2 is identical to a live GGIR:::CalcSleepRegularityIndex call", {
  s <- sri_series("MOS2")
  ref <- GGIR:::CalcSleepRegularityIndex(data = s$data, epochsize = s$epochsize,
                                         desiredtz = s$desiredtz,
                                         SRI1_smoothing_wsize_hrs = NULL,
                                         SRI1_smoothing_frac = NULL)
  got <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz)
  expect_identical(got, ref)
})

test_that("S5b EE SleepRegularityIndex is identical to the stored table and to live GGIR", {
  s <- sri_series("EE")
  expect_identical(s$desiredtz, "Europe/Helsinki")
  expect_identical(s$epochsize, 5)

  got <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz)

  expect_identical(dim(got), c(7L, 5L))
  expect_identical(got$SleepRegularityIndex,
                   c(63.277, 56.667, 63.686, 64.177, 61.489, 42.286, 0))
  expect_identical(got$frac_valid, c(0.6146, 1, 0.8854, 0.8646, 0.9792, 0.3646, 0))
  expect_identical(got$date, c("23/05/2017", "24/05/2017", "25/05/2017", "26/05/2017",
                               "27/05/2017", "28/05/2017", "29/05/2017"))
  expect_identical(got$weekday, c("Tuesday", "Wednesday", "Thursday", "Friday",
                                  "Saturday", "Sunday", "Monday"))
  expect_identical(got, s$stored)

  ref <- GGIR:::CalcSleepRegularityIndex(data = s$data, epochsize = s$epochsize,
                                         desiredtz = s$desiredtz,
                                         SRI1_smoothing_wsize_hrs = NULL,
                                         SRI1_smoothing_frac = NULL)
  expect_identical(got, ref)
})

test_that("the SRI1 smoothing parameters are identical to live GGIR over a sweep", {
  s <- sri_series("MOS2")
  grid <- list(c(1, 0.5), c(0.5, 0.25), c(2, 0.9), c(1, 0.1), c(4, 0.75))
  for (g in grid) {
    ref <- GGIR:::CalcSleepRegularityIndex(data = s$data, epochsize = s$epochsize,
                                           desiredtz = s$desiredtz,
                                           SRI1_smoothing_wsize_hrs = g[1],
                                           SRI1_smoothing_frac = g[2])
    got <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz,
                                SRI1_smoothing_wsize_hrs = g[1], SRI1_smoothing_frac = g[2])
    expect_identical(got, ref,
                     info = paste0("wsize_hrs = ", g[1], ", frac = ", g[2]))
  }
  # either argument alone leaves the series unsmoothed
  plain <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz)
  expect_identical(raw.sleep.regularity(s$data, epochsize = s$epochsize,
                                        desiredtz = s$desiredtz,
                                        SRI1_smoothing_wsize_hrs = 1), plain)
  expect_identical(raw.sleep.regularity(s$data, epochsize = s$epochsize,
                                        desiredtz = s$desiredtz,
                                        SRI1_smoothing_frac = 0.5), plain)
  smoothed <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz,
                                   SRI1_smoothing_wsize_hrs = 1, SRI1_smoothing_frac = 0.5)
  expect_false(identical(smoothed$SleepRegularityIndex, plain$SleepRegularityIndex))
})

test_that("the metric columns are dropped and the FIRST remaining column is the sleep column", {
  s <- sri_series("MOS2")
  shifted <- c(s$data$T5A5[-1], 0)                       # a second, genuinely different definition
  decoy <- data.frame(time = s$data$time,
                      invalid = s$data$invalid,
                      night = s$data$night,
                      anglez = 1, temperature = 20, guider = "HDCZA",
                      ACC = 5, step_count = 3, invalid_fullwindow = 1,
                      selfreported = "x",
                      T5A5 = s$data$T5A5,
                      T10A5 = shifted,
                      stringsAsFactors = FALSE)
  expect_identical(raw.sleep.regularity(decoy, epochsize = s$epochsize,
                                        desiredtz = s$desiredtz), s$stored)
  # invalid_fullwindow is dropped but the plain invalid column survives
  novalid <- decoy[, colnames(decoy) != "invalid"]
  expect_false(identical(raw.sleep.regularity(novalid, epochsize = s$epochsize,
                                              desiredtz = s$desiredtz)$frac_valid,
                         s$stored$frac_valid))
  # with the second definition placed first it is the one that is used
  swapped <- decoy[, c("time", "invalid", "night", "anglez", "T10A5", "T5A5")]
  got <- raw.sleep.regularity(swapped, epochsize = s$epochsize, desiredtz = s$desiredtz)
  expect_false(identical(got$SleepRegularityIndex, s$stored$SleepRegularityIndex))
  only10 <- decoy[, c("time", "invalid", "night", "T10A5")]
  expect_identical(got, raw.sleep.regularity(only10, epochsize = s$epochsize,
                                             desiredtz = s$desiredtz))
})

test_that("S5c a 30 s bin that is exactly half sib rounds DOWN, not up", {
  # base R rounds half to even, which is the behaviour the aggregation inherits
  expect_identical(round(0.5), 0)
  expect_identical(round(1.5), 2)
  expect_identical(round(2.5), 2)

  # day 1: every 30 s bin is half sib, mean exactly 0.5; days 2 and 3 all wake
  half <- function(tt, i) {
    day <- ((as.numeric(tt) - as.numeric(tt[1])) %/% 86400) + 1
    within30 <- (as.numeric(tt) %/% 5) %% 6
    ifelse(day == 1 & within30 < 3, 1, 0)
  }
  d <- sri_synth(3, 5, half)
  got <- raw.sleep.regularity(d, epochsize = 5, desiredtz = "UTC")
  expect_identical(dim(got), c(2L, 5L))
  expect_identical(got$frac_valid, c(1, 1))
  expect_identical(got$SleepRegularityIndex, c(100, 100))

  # four of six is a mean of 2/3 and rounds up to sleep; day 2 is all sleep
  fourofsix <- function(tt, i) {
    day <- ((as.numeric(tt) - as.numeric(tt[1])) %/% 86400) + 1
    within30 <- (as.numeric(tt) %/% 5) %% 6
    ifelse(day == 1, as.numeric(within30 < 4), 1)
  }
  d2 <- sri_synth(3, 5, fourofsix)
  got2 <- raw.sleep.regularity(d2, epochsize = 5, desiredtz = "UTC")
  expect_identical(got2$SleepRegularityIndex, c(100, 100))

  # two of six is a mean of 1/3 and rounds down to wake
  twoofsix <- function(tt, i) {
    day <- ((as.numeric(tt) - as.numeric(tt[1])) %/% 86400) + 1
    within30 <- (as.numeric(tt) %/% 5) %% 6
    ifelse(day == 1, as.numeric(within30 < 2), 1)
  }
  got3 <- raw.sleep.regularity(sri_synth(3, 5, twoofsix), epochsize = 5, desiredtz = "UTC")
  expect_identical(got3$SleepRegularityIndex, c(-100, 100))
})

test_that("S5c the aggregation matches live GGIR across epoch sizes, and M follows the branch", {
  skip_if_no_ggir()
  blocks <- function(tt, i) as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6))
  # 1, 5, 10 and 15 s take the aggregation branch; 30 and 60 s do not
  for (es in c(1, 5, 10, 15, 30, 60)) {
    d <- sri_synth(3, es, blocks)
    ref <- GGIR:::CalcSleepRegularityIndex(data = d, epochsize = es, desiredtz = "UTC",
                                           SRI1_smoothing_wsize_hrs = NULL,
                                           SRI1_smoothing_frac = NULL)
    got <- raw.sleep.regularity(d, epochsize = es, desiredtz = "UTC")
    expect_identical(got, ref, info = paste0("epochsize = ", es))
  }
  # at 7 s there is no aggregation and M is 86400/7, so the day pairs differ in length and
  # the comparison recycles with a warning; the port reproduces the warning as GGIR does
  d7 <- sri_synth(3, 7, blocks)
  expect_warning(ref7 <- GGIR:::CalcSleepRegularityIndex(data = d7, epochsize = 7,
                                                         desiredtz = "UTC", NULL, NULL),
                 "longer object length")
  expect_warning(got7 <- raw.sleep.regularity(d7, epochsize = 7, desiredtz = "UTC"),
                 "longer object length")
  expect_identical(got7, ref7)
})

test_that("M is 2880 on the aggregation path and 86400/epochsize otherwise", {
  # twelve invalid hours on day 1 leave half of pair 1 usable
  blocks <- function(tt, i) as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6))
  d <- sri_synth(3, 60, blocks)
  d$invalid[1:720] <- 1                     # 12 h of day 1
  got <- raw.sleep.regularity(d, epochsize = 60, desiredtz = "UTC")
  expect_identical(got$frac_valid, c(round(720 / 1440, 4), 1))

  # one single invalid epoch is 1/1440 of the day at 60 s and 1/2880 at 5 s
  d1 <- sri_synth(3, 60, blocks); d1$invalid[10] <- 1
  expect_identical(raw.sleep.regularity(d1, epochsize = 60, desiredtz = "UTC")$frac_valid[1],
                   round(1439 / 1440, 4))
  d2 <- sri_synth(3, 5, blocks); d2$invalid[55:60] <- 1     # one whole 30 s bin
  expect_identical(raw.sleep.regularity(d2, epochsize = 5, desiredtz = "UTC")$frac_valid[1],
                   round(2879 / 2880, 4))
})

test_that("the ne25 and ne23 guards fire on a daylight saving date and match live GGIR", {
  skip_if_no_ggir()
  dst_series <- function(start, n_days, es = 60, tz = "Europe/Helsinki") {
    t0 <- as.POSIXct(start, tz = tz)
    tt <- seq(t0, by = es, length.out = (86400 * n_days) / es)
    data.frame(time = tt, invalid = 0,
               night = ((as.numeric(tt) - as.numeric(tt[1])) %/% 86400) + 1,
               T5A5 = as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6)))
  }

  # autumn 2017-10-29: hour 03 happens twice, so that date carries 1500 rows (ne25)
  fall <- dst_series("2017-10-27 00:00:00", 5)
  counts <- as.integer(table(as.Date(fall$time, tz = "Europe/Helsinki")))
  expect_identical(counts, c(1440L, 1440L, 1500L, 1440L, 1380L))
  got <- raw.sleep.regularity(fall, epochsize = 60, desiredtz = "Europe/Helsinki")
  ref <- GGIR:::CalcSleepRegularityIndex(data = fall, epochsize = 60,
                                         desiredtz = "Europe/Helsinki", NULL, NULL)
  expect_identical(got, ref)
  expect_identical(got$SleepRegularityIndex, c(100, 83.333, 83.333, 100))
  expect_identical(got$frac_valid, c(1, 1, 1, 0.9583))
  expect_identical(got$date, c("27/10/2017", "28/10/2017", "29/10/2017", "30/10/2017"))

  # spring 2017-03-26: hour 03 is skipped, so that date carries 1380 rows (ne23)
  spring <- dst_series("2017-03-24 00:00:00", 5)
  counts2 <- as.integer(table(as.Date(spring$time, tz = "Europe/Helsinki")))
  expect_identical(counts2, c(1440L, 1440L, 1380L, 1440L, 1440L, 60L))
  got2 <- raw.sleep.regularity(spring, epochsize = 60, desiredtz = "Europe/Helsinki")
  ref2 <- GGIR:::CalcSleepRegularityIndex(data = spring, epochsize = 60,
                                          desiredtz = "Europe/Helsinki", NULL, NULL)
  expect_identical(got2, ref2)
  expect_identical(got2$SleepRegularityIndex, rep(100, 5))
  expect_identical(got2$frac_valid, c(1, 0.9583, 0.9583, 1, 0.0417))
})

test_that("the daylight saving fixture recordings reproduce their stored SRI tables", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  expected <- list(
    spring = list(
      fn = "EEspring.gt3x",
      sri = c(63.277, 56.667, 63.686, 64.177, 58.074, 42.857, 0),
      frac = c(0.6146, 1, 0.8854, 0.8646, 0.9375, 0.3646, 0),
      date = c("21/03/2017", "22/03/2017", "23/03/2017", "24/03/2017",
               "25/03/2017", "26/03/2017", "27/03/2017")),
    autumn = list(
      fn = "EEautumn.gt3x",
      sri = c(63.277, 56.667, 63.686, 64.177, 61.489, 35.054, 0),
      frac = c(0.6146, 1, 0.8854, 0.8646, 0.9792, 0.3229, 0),
      date = c("24/10/2017", "25/10/2017", "26/10/2017", "27/10/2017",
               "28/10/2017", "29/10/2017", "30/10/2017")),
    autumn_edge = list(
      fn = "EEautumnE.gt3x",
      sri = c(61.033, 53.403, 71.245, 66.024, 63.472, 7.719, 0),
      frac = c(0.7396, 1, 0.8646, 0.8646, 1, 0.1979, 0),
      date = c("25/10/2017", "26/10/2017", "27/10/2017", "28/10/2017",
               "29/10/2017", "30/10/2017", "31/10/2017"))
  )
  ran <- 0L
  for (case in names(expected)) {
    base <- p34_fixture(case)
    ex <- expected[[case]]
    pb <- file.path(base, "meta", "basic", paste0("meta_", ex$fn, ".RData"))
    p2 <- file.path(base, "meta", "ms2.out", paste0(ex$fn, ".RData"))
    p3 <- file.path(base, "meta", "ms3.out", paste0(ex$fn, ".RData"))
    if (!all(file.exists(pb, p2, p3))) next
    ran <- ran + 1L
    e1 <- new.env(parent = emptyenv()); load(pb, envir = e1)
    e2 <- new.env(parent = emptyenv()); load(p2, envir = e2)
    e3 <- new.env(parent = emptyenv()); load(p3, envir = e3)
    tz <- e3$desiredtz_part1
    P <- GGIR::load_params(topic = c("sleep", "metrics", "general"))
    SLE <- GGIR:::g.sib.det(M = e1$M, IMP = e2$IMP, I = e1$I, twd = c(-12, 12),
                            acc.metric = P$params_general$acc.metric, desiredtz = tz,
                            myfun = c(), sensor.location = P$params_general$sensor.location,
                            params_sleep = P$params_sleep, zc.scale = P$params_metrics$zc.scale)
    A <- SLE$output
    A <- A[, -which(names(A) == "spt_crude_estimate")]
    got <- raw.sleep.regularity(A, epochsize = e1$M$windowsizes[1], desiredtz = tz)
    expect_identical(got, e3$SleepRegularityIndex, info = case)
    expect_identical(got$SleepRegularityIndex, ex$sri, info = case)
    expect_identical(got$frac_valid, ex$frac, info = case)
    expect_identical(got$date, ex$date, info = case)
  }
  if (ran == 0L) skip("no daylight saving fixtures under ggir-study-p34/fixtures")
  expect_identical(ran, 3L)
})

test_that("trap 100 a missing calendar day is not inserted and its neighbours are paired", {
  skip_if_no_ggir()
  blocks <- function(tt, i) as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6))
  d <- sri_synth(4, 60, blocks)
  keep <- as.Date(d$time, tz = "UTC") != as.Date("2024-01-03")
  gap <- d[keep, ]
  got <- raw.sleep.regularity(gap, epochsize = 60, desiredtz = "UTC")
  # three dates remain, so two pairs, and the second pair spans 48 h as if it were 24
  expect_identical(nrow(got), 2L)
  expect_identical(got$date, c("01/01/2024", "02/01/2024"))
  expect_identical(got$frac_valid, c(1, 1))
  ref <- GGIR:::CalcSleepRegularityIndex(data = gap, epochsize = 60, desiredtz = "UTC",
                                         NULL, NULL)
  expect_identical(got, ref)
})

test_that("S5d two distinct calendar dates return the scalar NA, not a one row frame", {
  blocks <- function(tt, i) as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6))
  two <- sri_synth(2, 60, blocks)
  got <- raw.sleep.regularity(two, epochsize = 60, desiredtz = "UTC")
  expect_identical(got, NA)
  expect_false(is.data.frame(got))

  one <- sri_synth(1, 60, blocks)
  expect_identical(raw.sleep.regularity(one, epochsize = 60, desiredtz = "UTC"), NA)

  three <- sri_synth(3, 60, blocks)
  expect_true(is.data.frame(raw.sleep.regularity(three, epochsize = 60, desiredtz = "UTC")))
  expect_identical(nrow(raw.sleep.regularity(three, epochsize = 60, desiredtz = "UTC")), 2L)
})

test_that("S5e a fully invalid day pair gives 0.000 with frac_valid 0.0000, not NA", {
  blocks <- function(tt, i) as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6))
  d <- sri_synth(3, 60, blocks)
  d$invalid[1:1440] <- 1                    # the whole of day 1
  got <- raw.sleep.regularity(d, epochsize = 60, desiredtz = "UTC")
  # pair 1 has no usable slot at all, pair 2 is two identical days
  expect_identical(got$SleepRegularityIndex, c(0, 100))
  expect_identical(got$frac_valid, c(0, 1))
  expect_false(anyNA(got$SleepRegularityIndex))
  expect_identical(typeof(got$SleepRegularityIndex), "double")

  # a genuine index of zero has frac_valid 1; only frac_valid separates it from "no valid pair"
  halfagree <- function(tt, i) {
    day <- ((as.numeric(tt) - as.numeric(tt[1])) %/% 86400) + 1
    ifelse(day == 2 & as.POSIXlt(tt)$hour < 12, 1, 0)
  }
  real0 <- raw.sleep.regularity(sri_synth(3, 60, halfagree), epochsize = 60, desiredtz = "UTC")
  expect_identical(real0$SleepRegularityIndex, c(0, 0))
  expect_identical(real0$frac_valid, c(1, 1))
})

test_that("the weekday column is English and the caller's LC_TIME is restored", {
  # a German caller, so a missing switch to C or a missing restore shows
  local_german_time()
  blocks <- function(tt, i) as.numeric(as.POSIXlt(tt)$hour %in% c(23, 0:6))
  before <- Sys.getlocale("LC_TIME")
  got <- raw.sleep.regularity(sri_synth(4, 60, blocks, start = "2024-01-01 00:00:00"),
                              epochsize = 60, desiredtz = "UTC")
  expect_identical(Sys.getlocale("LC_TIME"), before)
  # 2024-01-01 was a Monday
  expect_identical(got$weekday, c("Monday", "Tuesday", "Wednesday"))
  expect_identical(got$date, c("01/01/2024", "02/01/2024", "03/01/2024"))
  expect_identical(typeof(got$day), "integer")
  expect_identical(typeof(got$weekday), "character")
  expect_identical(typeof(got$date), "character")
  expect_identical(typeof(got$frac_valid), "double")
})

test_that("an ISO 8601 character time column is coerced exactly as GGIR does", {
  s <- sri_series("MOS2")
  fac <- s$data
  expect_true(is.factor(fac$time))        # g.sib.det hands over an ISO 8601 factor column
  chr <- fac
  chr$time <- as.character(fac$time)
  expect_identical(raw.sleep.regularity(chr, epochsize = s$epochsize,
                                        desiredtz = s$desiredtz), s$stored)
  posix <- fac
  posix$time <- .raw.iso8601.to.posix(as.character(fac$time), tz = s$desiredtz)
  expect_identical(raw.sleep.regularity(posix, epochsize = s$epochsize,
                                        desiredtz = s$desiredtz), s$stored)
})

test_that("S5h raw.sleep.regularity and sri.matrix agree once aggregated and pooled", {
  s <- sri_series("MOS2")
  tt <- .raw.iso8601.to.posix(s$data$time, tz = s$desiredtz)
  state <- s$data$T5A5
  state[s$data$invalid == 1] <- NA

  # canhrActi's estimator at the native 5 s resolution
  native <- sri.matrix(state, tt, epoch_length = 5)
  expect_identical(round(native$SRI, 2), 37.01)
  expect_identical(native$n_days, 8L)
  expect_identical(native$n_valid_pairs, 35640L)

  # the same estimator on the series aggregated the GGIR way (mean, then round, on 30 s bins)
  tn <- floor(as.numeric(tt) / 30) * 30
  ag <- aggregate(state, by = list(tn), FUN = mean)
  ag$x <- round(ag$x)
  agg <- sri.matrix(ag$x, as.POSIXct(ag$Group.1, tz = s$desiredtz, origin = "1970-01-01"),
                    epoch_length = 30)
  expect_identical(round(agg$SRI, 2), 37.10)
  expect_identical(agg$n_valid_pairs, 5940L)

  # the GGIR day pairs pooled by their valid pair count give the same number
  got <- raw.sleep.regularity(s$data, epochsize = s$epochsize, desiredtz = s$desiredtz)
  w <- got$frac_valid * 2880
  pooled <- sum(((got$SleepRegularityIndex + 100) / 200) * w) / sum(w)
  expect_identical(round(-100 + 200 * pooled, 2), 37.10)
  # 5940 is frac_valid * 2880 summed over the three non-empty day pairs
  expect_identical(sum(w), 5940)

  # different return type, and a fully invalid pair is 0 here and NA_real_ there
  expect_true(is.data.frame(got))
  expect_type(native, "list")
  expect_identical(sri.matrix(rep(NA_real_, 1440 * 3),
                              seq(as.POSIXct("2024-01-01", tz = "UTC"), by = 60,
                                  length.out = 1440 * 3), epoch_length = 60)$SRI,
                   NA_real_)
})

# The locale forced at the top of this file, put back
Sys.setlocale("LC_TIME", .saved_lc_time)
