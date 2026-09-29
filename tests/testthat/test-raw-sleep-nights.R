# Parity tests for raw.sleep.nights() against GGIR's g.part4. Reference data are found
# through CANHRACTI_GGIR_REF and the fixtures under <ref>/../ggir-study-p34/fixtures; every
# block skips without them, and the live comparisons skip when GGIR is not installed. The
# MOS2 reference was produced in the system zone of a machine set to America/Anchorage, so
# the file runs in that zone.

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
p34_fixture <- function(...) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", ...)
}

nights_loadenv <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  as.list(e)
}

# The two reference recordings, and the ms3 / ms4 paths of any output folder
NIGHTS_CASES <- list(
  MOS2 = list(out = c("out", "output_din"), fn = "MOS2E39230594.gt3x.RData"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x.RData"))

nights_path <- function(case, part) {
  cs <- NIGHTS_CASES[[case]]
  do.call(ref_file, as.list(c(cs$out, "meta", paste0("ms", part, ".out"), cs$fn)))
}

# One folder's first ms3 file and its stored ms4 nightsummary
nights_fixture_pair <- function(outdir) {
  d3 <- file.path(outdir, "meta", "ms3.out")
  if (!dir.exists(d3) || length(dir(d3)) == 0) return(NULL)
  fn <- dir(d3)[1]
  ms4 <- file.path(outdir, "meta", "ms4.out", fn)
  list(fn = fn, ms3 = nights_loadenv(file.path(d3, fn)),
       ms4 = if (file.exists(ms4)) nights_loadenv(ms4)$nightsummary else NULL)
}

# GGIR's own g.part4 over a private copy of an ms3 milestone
run_ggir_part4 <- function(ms3, fname, args = list(), visual = FALSE) {
  d <- tempfile("canhr_p4_")
  dir.create(file.path(d, "meta", "ms3.out"), recursive = TRUE)
  dir.create(file.path(d, "results"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  env <- list2env(ms3, envir = new.env(parent = emptyenv()))
  save(list = names(ms3), envir = env, file = file.path(d, "meta", "ms3.out", fname))
  a <- c(list(datadir = c(), metadatadir = d, f0 = 1, f1 = 1, verbose = FALSE,
              do.visual = visual, overwrite = TRUE), args)
  suppressWarnings(do.call(GGIR:::g.part4, a))
  f4 <- file.path(d, "meta", "ms4.out", fname)
  if (!file.exists(f4)) return(NULL)
  nights_loadenv(f4)$nightsummary
}

ours <- function(ms3, fname, args = list()) {
  as.ggir.nightsummary(suppressWarnings(
    do.call(raw.sleep.nights, c(list(part3 = ms3, filename = fname, do.visual = FALSE), args))))
}

# Report the first differing column and value pair, so a failure says why
expect_same_nightsummary <- function(mine, ref, label = "") {
  if (!identical(mine, ref)) {
    for (cc in union(colnames(mine), colnames(ref))) {
      if (!identical(mine[[cc]], ref[[cc]])) {
        cat("\n", label, " first differing column: ", cc, "\n  ours: ",
            paste(format(mine[[cc]]), collapse = " | "), "\n  GGIR: ",
            paste(format(ref[[cc]]), collapse = " | "), "\n", sep = "")
        break
      }
    }
  }
  testthat::expect_identical(mine, ref)
}

test_that("S7a the stored MOS2 nightsummary is reproduced exactly, whole and column by column", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  p4 <- nights_path("MOS2", 4); skip_if_no_file(p4)
  ms3 <- nights_loadenv(p3)
  ref <- nights_loadenv(p4)$nightsummary
  ns <- ours(ms3, NIGHTS_CASES$MOS2$fn)
  expect_identical(dim(ns), c(4L, 39L))
  expect_identical(colnames(ns), colnames(ref))
  for (cc in colnames(ref)) {
    expect_identical(ns[[cc]], ref[[cc]], info = cc)
  }
  expect_same_nightsummary(ns, ref, "MOS2")
  expect_identical(attr(ns, "row.names"), attr(ref, "row.names"))
})

test_that("S7a the headline MOS2 numbers are the ones the specification quotes", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ns <- ours(nights_loadenv(p3), NIGHTS_CASES$MOS2$fn)
  expect_identical(ns$ID, rep("MOS2E39230594.gt3x", 4))
  expect_identical(ns$night, c(1, 2, 3, 4))
  expect_identical(round(ns$sleeponset, 7),
                   c(22.9861111, 23.5125000, 23.7680556, 22.0347222))
  expect_identical(round(ns$wakeup, 7),
                   c(31.1791667, 31.7666667, 30.9722222, 23.2486111))
  expect_identical(round(ns$SptDuration, 7),
                   c(8.1930556, 8.2541667, 7.2041667, 1.2138889))
  expect_identical(round(ns$guider_onset, 7),
                   c(23.0744444, 23.4597222, 23.7236111, 22.0402778))
  expect_identical(round(ns$guider_wakeup, 7),
                   c(31.3338889, 31.8597222, 31.0708333, 31.0708333))
  expect_identical(round(ns$guider_SptDuration, 7),
                   c(8.2594444, 8.4000000, 7.3472222, 9.0305556))
  expect_identical(round(ns$error_onset, 7),
                   c(-0.0883333, 0.0527778, 0.0444444, -0.0055556))
  expect_identical(round(ns$error_wake, 7),
                   c(-0.1547222, -0.0930556, -0.0986111, -7.8222222))
  expect_identical(round(ns$error_dur, 7),
                   c(-0.0663889, -0.1458333, -0.1430556, -7.8166667))
  expect_identical(round(ns$fraction_night_invalid, 7), c(0.4061921, 0, 0, 0.5313079))
  expect_identical(round(ns$SleepDurationInSpt, 7),
                   c(6.3000000, 7.8958333, 6.7208333, 1.2138889))
  expect_identical(round(ns$WASO, 7), c(1.8930556, 0.3583333, 0.4833333, 0))
  expect_identical(round(ns$duration_sib_wakinghours, 7),
                   c(2.7236111, 4.4791667, 3.4166667, 0.1583333))
  expect_identical(ns$number_sib_sleepperiod, c(18, 13, 15, 1))
  expect_identical(ns$number_of_awakenings, c(17, 12, 14, 0))
  expect_identical(ns$number_sib_wakinghours, c(16, 25, 21, 2))
  expect_identical(round(ns$duration_sib_wakinghours_atleast15min, 7),
                   c(0.5847222, 1.3861111, 1.0722222, 0))
  expect_identical(ns$cleaningcode, c(2, 1, 1, 2))
  expect_identical(ns$calendar_date, c("7/10/2025", "8/10/2025", "9/10/2025", "10/10/2025"))
  expect_identical(ns$weekday, c("Tuesday", "Wednesday", "Thursday", "Friday"))
  expect_identical(ns$guider, rep("HDCZA", 4))
  expect_identical(ns$page, c(1, 1, 1, 1))
  expect_identical(ns$daysleeper, c(0, 0, 0, 0))
  expect_identical(ns$sleeplog_used, c(0, 0, 0, 0))
  expect_identical(ns$acc_available, c(1, 1, 1, 1))
  expect_identical(ns$sleeplog_ID, as.numeric(c(NA, NA, NA, NA)))
  expect_identical(ns$SleepRegularityIndex1, c(NA, 30.256, 46.738, NA))
  expect_identical(ns$SriFractionValid, c(NA, 0.9479, 0.9688, NA))
  expect_identical(ns$longitudinal_axis, as.numeric(c(NA, NA, NA, NA)))
  expect_identical(ns$guider_corrected, as.numeric(c(NA, NA, NA, NA)))
  expect_identical(ns$sleepparam, rep("T5A5", 4))
  expect_identical(ns$filename, rep("MOS2E39230594.gt3x.RData", 4))
})

test_that("units: the hour columns are decimal hours on the 12..36 axis and four are clock strings", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ns <- ours(nights_loadenv(p3), NIGHTS_CASES$MOS2$fn)
  hourcols <- c("sleeponset", "wakeup", "SptDuration", "guider_onset", "guider_wakeup",
                "guider_SptDuration", "error_onset", "error_wake", "error_dur",
                "SleepDurationInSpt", "WASO", "duration_sib_wakinghours",
                "duration_sib_wakinghours_atleast15min")
  for (cc in hourcols) expect_true(is.numeric(ns[[cc]]), info = cc)
  clockcols <- c("sleeponset_ts", "wakeup_ts", "guider_onset_ts", "guider_wakeup_ts")
  for (cc in clockcols) {
    expect_true(is.character(ns[[cc]]), info = cc)
    expect_true(all(grepl("^[0-9]{2}:[0-9]{2}:[0-9]{2}$", ns[[cc]])), info = cc)
  }
  expect_identical(ns$sleeponset_ts, c("22:59:10", "23:30:45", "23:46:05", "22:02:05"))
  expect_identical(ns$wakeup_ts, c("07:10:45", "07:46:00", "06:58:20", "23:14:55"))
  expect_identical(ns$guider_onset_ts, c("23:04:28", "23:27:35", "23:43:25", "22:02:25"))
  expect_identical(ns$guider_wakeup_ts, c("07:20:02", "07:51:35", "07:04:15", "07:04:15"))
  # 12 is noon of calendar_date and 24 the following midnight, so 31.179 is 07:10 next morning
  expect_identical(round((ns$wakeup[1] - 24) * 3600), 25845)
  expect_identical(.raw.nights.hours2clock(ns$wakeup[1] - 24), "07:10:45")
  chr <- names(ns)[vapply(ns, is.character, logical(1))]
  expect_identical(chr, c("ID", "sleepparam", "sleeponset_ts", "wakeup_ts", "guider_onset_ts",
                          "guider_wakeup_ts", "weekday", "calendar_date", "filename",
                          "guider", "GGIRversion"))
  expect_identical(length(names(ns)) - length(chr), 28L)
})

test_that("trap 6 and 8: the guider is quantised to whole seconds by the clock round trip", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3)
  ns <- ours(ms3, NIGHTS_CASES$MOS2$fn)
  # night 1 has no SPTE, so the guider is the mean of the other three nights
  raw_mean <- mean(ms3$SPTE_start[which(!is.na(ms3$SPTE_start))])
  expect_identical(round(raw_mean, 9), 23.074537037)
  expect_false(identical(ns$guider_onset[1], raw_mean))
  expect_identical(ns$guider_onset[1], .raw.nights.clock2hours("23:04:28"))
  expect_identical(round(ns$guider_onset[1], 10), 23.0744444444)
  expect_identical(ns$guider_onset_ts[1], "23:04:28")
  expect_equal(abs(raw_mean - ns$guider_onset[1]) * 3600, 1 / 3, tolerance = 1e-9)
  expect_identical(ns$error_onset[1], ns$sleeponset[1] - ns$guider_onset[1])
  # nights 2 to 4 are already multiples of 5 s, so the clock string loses nothing
  expect_equal(ns$guider_onset[2:4], ms3$SPTE_start[2:4], tolerance = 1e-12)
  expect_equal(ns$guider_wakeup[2:3], ms3$SPTE_end[2:3], tolerance = 1e-12)
  for (i in 1:4) {
    # every onset here is before midnight, so no 24 is added on the way back
    expect_identical(ns$guider_onset[i], .raw.nights.clock2hours(ns$guider_onset_ts[i]))
    expect_identical(ns$guider_wakeup[i], .raw.nights.clock2hours(ns$guider_wakeup_ts[i]) + 24)
  }
})

test_that("S7c the worked example of MOS2 night 1", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  x <- suppressWarnings(raw.sleep.nights(nights_loadenv(p3),
                                         filename = NIGHTS_CASES$MOS2$fn, do.visual = FALSE))
  ep <- attr(x, "canhrActi")$episodes
  e1 <- ep[ep$night == 1, ]
  expect_identical(nrow(e1), 34L)
  expect_identical(which(e1$overlapGuider == 1), 5:22)
  # sib 5 starts before the guider onset and is counted in full: partial overlap is enough
  expect_identical(round(e1$start[5], 7), 22.9861111)
  expect_true(e1$start[5] < x$guider_onset[1])
  expect_identical(round(e1$end[22], 7), 31.1791667)
  expect_identical(round(max(e1$start[1:4]), 7), 22.0430556)
  expect_identical(round(min(e1$start[23:34]), 7), 31.8819444)
  expect_identical(x$sleeponset[1], e1$start[5])
  expect_identical(x$wakeup[1], e1$end[22])
  expect_identical(round(x$SptDuration[1], 7), 8.1930556)
  expect_identical(round(sum(e1$duration[e1$overlapGuider == 1]), 7), 6.3)
  expect_identical(x$SleepDurationInSpt[1], sum(e1$duration[e1$overlapGuider == 1]))
  expect_identical(round(x$WASO[1], 7), 1.8930556)
  expect_identical(round(x$duration_sib_wakinghours[1], 7), 2.7236111)
  expect_identical(x$number_sib_sleepperiod[1], 18)
  expect_identical(x$number_of_awakenings[1], 17)
  expect_identical(x$number_sib_wakinghours[1], 16)
  # the cut is 1/4 h, not the 5 minutes GGIR's own comment claims
  day <- e1$duration[e1$overlapGuider == 0]
  expect_identical(sum(day >= 1 / 4), 2L)
  expect_identical(round(x$duration_sib_wakinghours_atleast15min[1], 7), 0.5847222)
})

test_that("S7b the stored EE nightsummary is reproduced exactly", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("EE", 3); skip_if_no_file(p3)
  p4 <- nights_path("EE", 4); skip_if_no_file(p4)
  ms3 <- nights_loadenv(p3)
  expect_identical(ms3$desiredtz_part1, "Europe/Helsinki")
  ref <- nights_loadenv(p4)$nightsummary
  ns <- ours(ms3, NIGHTS_CASES$EE$fn)
  expect_identical(dim(ns), c(6L, 39L))
  for (cc in colnames(ref)) expect_identical(ns[[cc]], ref[[cc]], info = cc)
  expect_same_nightsummary(ns, ref, "EE")
  expect_identical(round(ns$sleeponset, 7),
                   c(22.2930556, 23.6458333, 25.0694444, 24.5277778, 24.2888889, 22.7569444))
  expect_identical(ns$cleaningcode, rep(1, 6))
  # the day and the month are unpadded
  expect_identical(ns$calendar_date, c("23/5/2017", "24/5/2017", "25/5/2017",
                                       "26/5/2017", "27/5/2017", "28/5/2017"))
  expect_identical(ns$weekday, c("Tuesday", "Wednesday", "Thursday", "Friday",
                                 "Saturday", "Sunday"))
  expect_identical(round(ns$SleepDurationInSpt, 7),
                   c(7.5222222, 8.6611111, 6.9208333, 5.9250000, 8.1375000, 7.1944444))
  expect_identical(ns$number_sib_sleepperiod, c(16, 14, 16, 9, 15, 13))
  # the SRI join needs frac_valid strictly above includenightcrit/24, so nights 1 and 6 get NA
  expect_identical(ns$SleepRegularityIndex1, c(NA, 56.667, 63.686, 64.177, 61.489, NA))
  expect_identical(ns$SriFractionValid, c(NA, 1.0000, 0.8854, 0.8646, 0.9792, NA))
  expect_true(all(ns$SriFractionValid[2:5] > 16 / 24))
  expect_true(all(ms3$SleepRegularityIndex$frac_valid[c(1, 6)] < 16 / 24))
})

test_that("S7b the timezone recorded by part 1 beats the one the caller passes", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("EE", 3); skip_if_no_file(p3)
  p4 <- nights_path("EE", 4); skip_if_no_file(p4)
  ms3 <- nights_loadenv(p3)
  ref <- nights_loadenv(p4)$nightsummary
  ns <- ours(ms3, NIGHTS_CASES$EE$fn, list(desiredtz = "America/Anchorage"))
  expect_identical(ns, ref)
  expect_identical(attr(suppressWarnings(raw.sleep.nights(
    ms3, filename = NIGHTS_CASES$EE$fn, desiredtz = "America/Anchorage")),
    "canhrActi")$desiredtz, "Europe/Helsinki")
})

test_that("end to end on MOS2: part 1 milestone, impute, part 3, part 4 (long running)", {
  skip_if_no_ggir_ref()
  pb <- ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
  skip_if_no_file(pb)
  p4 <- nights_path("MOS2", 4); skip_if_no_file(p4)
  x <- read.ggir.milestone(pb)
  x$imputed <- raw.impute(x)
  x$sleep <- raw.sleep.part3(x)
  ns <- suppressWarnings(raw.sleep.nights(x))
  expect_identical(unique(ns$filename), "MOS2E39230594.gt3x.RData")
  expect_same_nightsummary(as.ggir.nightsummary(ns),
                           nights_loadenv(p4)$nightsummary, "MOS2 end to end")
  expect_identical(as.ggir.nightsummary(suppressWarnings(raw.sleep.nights(x$sleep))),
                   as.ggir.nightsummary(ns))
})

test_that("end to end on EE (long running)", {
  skip_if_no_ggir_ref()
  pb <- ref_file("timing_out", "output_timing", "meta", "basic",
                 "meta_EE_left_29.5.2017-05-30.gt3x.RData")
  skip_if_no_file(pb)
  p4 <- nights_path("EE", 4); skip_if_no_file(p4)
  y <- read.ggir.milestone(pb)
  y$imputed <- raw.impute(y)
  y$sleep <- raw.sleep.part3(y)
  ns <- suppressWarnings(raw.sleep.nights(y))
  expect_same_nightsummary(as.ggir.nightsummary(ns),
                           nights_loadenv(p4)$nightsummary, "EE end to end")
})

test_that("S7d includenightcrit moves cleaningcode 2 and nothing else", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  for (crit in c(0, 24)) {
    mine <- ours(ms3, fn, list(includenightcrit = crit))
    expect_same_nightsummary(mine, run_ggir_part4(ms3, fn, list(includenightcrit = crit)),
                             paste0("includenightcrit ", crit))
  }
  # 24 leaves nights 2 and 3 at 1 because their fraction is exactly 0 and the test is a strict >
  expect_identical(ours(ms3, fn, list(includenightcrit = 0))$cleaningcode, c(1, 1, 1, 1))
  expect_identical(ours(ms3, fn, list(includenightcrit = 24))$cleaningcode, c(2, 1, 1, 2))
  # cleaningcode 6 is assigned on night 1 and then replaced by 1, so it never reaches output
  expect_false(any(ours(ms3, fn, list(includenightcrit = 0))$cleaningcode == 6))
})

test_that("S7e the three night exclusion rules, and both TRUE matching no branch", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  cases <- list(
    list(args = list(excludefirstlast = TRUE), nights = c(2, 3)),
    list(args = list(excludefirst.part4 = TRUE), nights = c(2, 3, 4)),
    list(args = list(excludelast.part4 = TRUE), nights = c(1, 2, 3)),
    list(args = list(excludefirst.part4 = TRUE, excludelast.part4 = TRUE),
         nights = c(1, 2, 3, 4)))
  for (cs in cases) {
    mine <- ours(ms3, fn, cs$args)
    expect_identical(mine$night, cs$nights, info = paste(names(cs$args), collapse = "+"))
    expect_same_nightsummary(mine, run_ggir_part4(ms3, fn, cs$args),
                             paste(names(cs$args), collapse = "+"))
  }
})

test_that("S7f def.noc.sleep of length 2 gives a fixed window, and c(3, 17) a day sleeper", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  a <- ours(ms3, fn, list(def.noc.sleep = c(21, 9)))
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, list(def.noc.sleep = c(21, 9))),
                           "def.noc.sleep c(21,9)")
  expect_identical(a$guider, rep("setwindow", 4))
  expect_identical(a$guider_onset, rep(21, 4))
  expect_identical(a$guider_wakeup, rep(33, 4))
  expect_identical(a$guider_SptDuration, rep(12, 4))
  b <- ours(ms3, fn, list(def.noc.sleep = c(3, 17)))
  expect_same_nightsummary(b, run_ggir_part4(ms3, fn, list(def.noc.sleep = c(3, 17))),
                           "def.noc.sleep c(3,17)")
  # the last night is demoted with the guider wake capped at 36
  expect_identical(b$daysleeper, c(1, 1, 1, 0))
  expect_identical(b$guider_onset, rep(27, 4))
  expect_identical(b$guider_wakeup, c(41, 41, 41, 36))
  expect_identical(b$guider_SptDuration, c(14, 14, 14, 9))
})

test_that("S7g a guider window in the middle of the day builds the two artificial sibs", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  a <- ours(ms3, fn, list(def.noc.sleep = c(13, 14)))
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, list(def.noc.sleep = c(13, 14))),
                           "def.noc.sleep c(13,14)")
  expect_identical(a$sleeponset[c(1, 2, 4)], c(13, 13, 13))
  expect_identical(a$wakeup[c(1, 2, 4)], c(14, 14, 14))
  expect_identical(round(a$SleepDurationInSpt[c(1, 2, 4)], 5), rep(0.03333, 3))
  expect_identical(a$number_sib_sleepperiod[c(1, 2, 4)], c(2, 2, 2))
  # cleaningcode 2 beats 5 on nights 1 and 4, and night 3 keeps 1 because a real sib crosses
  expect_identical(a$cleaningcode, c(2, 5, 1, 2))
})

test_that("S7h a +invalid guider adds the artificial sibs even when real ones do overlap", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  base <- ours(ms3, fn)
  inv <- ms3
  inv$part3_guider <- ifelse(is.na(inv$part3_guider), NA, "HDCZA+invalid")
  a <- ours(inv, fn)
  expect_same_nightsummary(a, run_ggir_part4(inv, fn), "HDCZA+invalid")
  expect_identical(a$sleeponset, a$guider_onset)
  expect_identical(a$number_sib_sleepperiod, base$number_sib_sleepperiod + 2)
  expect_identical(a$number_sib_sleepperiod, c(20, 15, 17, 3))
  expect_identical(a$cleaningcode, c(2, 1, 1, 2))
  expect_identical(a$guider, rep("HDCZA+invalid", 4))
})

test_that("S7i relyonguider trims the leading sib back to the guider onset", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  base <- ours(ms3, fn)
  a <- ours(ms3, fn, list(relyonguider = TRUE))
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, list(relyonguider = TRUE)),
                           "relyonguider")
  # nights 1 and 4 have a sib straddling the guider onset; nights 2 and 3 start inside the window
  expect_identical(a$sleeponset[c(1, 4)], a$guider_onset[c(1, 4)])
  expect_identical(a$error_onset[c(1, 4)], c(0, 0))
  expect_identical(a$sleeponset[2:3], base$sleeponset[2:3])
  expect_identical(round(a$sleeponset[1], 7), 23.0744444)
  expect_identical(round(a$SleepDurationInSpt[1], 5), 6.21167)
  expect_false(identical(base$SleepDurationInSpt[1], a$SleepDurationInSpt[1]))
  expect_identical(a$wakeup, base$wakeup)
})

test_that("S7j a night with no rows in sib.cla.sum produces no row at all", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  cut2 <- ms3
  cut2$sib.cla.sum <- cut2$sib.cla.sum[cut2$sib.cla.sum$night != 2, ]
  a <- ours(cut2, fn)
  expect_same_nightsummary(a, run_ggir_part4(cut2, fn), "night 2 deleted")
  expect_identical(nrow(a), 3L)
  expect_identical(a$night, c(1, 3, 4))
  expect_false(any(a$cleaningcode == 3))
})

test_that("S7l number_of_awakenings reaches -1 under a narrow TimeInBed diary", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  d <- tempfile("diary_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  log <- file.path(d, "narrow.csv")
  hdr <- c("ID", as.vector(rbind(rep("inbed", 7), rep("outbed", 7))))
  row <- c("MOS2E39230594.gt3x", as.vector(rbind(rep("04:20:00", 7), rep("04:25:00", 7))))
  write.table(rbind(hdr, row), log, sep = ",", row.names = FALSE, col.names = FALSE,
              quote = TRUE)
  args <- list(loglocation = log, sleepwindowType = "TimeInBed")
  a <- ours(ms3, fn, args)
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, args), "narrow TimeInBed diary")
  expect_identical(a$number_sib_sleepperiod, c(0, 0, 0, 2))
  expect_identical(a$number_of_awakenings, c(-1, -1, -1, 1))
  expect_equal(a$SleepDurationInSpt, c(0, 0, 0, 1 / 30), tolerance = 1e-12)
  expect_identical(a$sleepefficiency, c(0, 0, 0, 0.4))
  expect_identical(a$cleaningcode, c(2, 5, 5, 2))
  # with no flagged sib the edges fall back to the guider
  expect_identical(a$sleeponset[1:3], a$guider_inbedStart[1:3])
  expect_identical(a$wakeup[1:3], a$guider_inbedEnd[1:3])
})

test_that("S7m the two sleep efficiency metrics, and an unknown one leaving the cell empty", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  d <- tempfile("diary_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  log <- file.path(d, "wide.csv")
  hdr <- c("ID", as.vector(rbind(rep("inbed", 7), rep("outbed", 7))))
  row <- c("MOS2E39230594.gt3x", as.vector(rbind(rep("22:45:00", 7), rep("07:25:00", 7))))
  write.table(rbind(hdr, row), log, sep = ",", row.names = FALSE, col.names = FALSE,
              quote = TRUE)
  for (m in c(1, 2, 3)) {
    args <- list(loglocation = log, sleepwindowType = "TimeInBed", sleepefficiency.metric = m)
    a <- ours(ms3, fn, args)
    expect_same_nightsummary(a, run_ggir_part4(ms3, fn, args),
                             paste0("sleepefficiency.metric ", m))
    expect_identical(ncol(a), 41L)
    if (m == 1) {
      expect_identical(round(a$SleepDurationInSpt[1], 7), 6.3)
      expect_identical(round(a$guider_inbedDuration[1], 7), 8.6666667)
      expect_identical(round(a$SptDuration[1], 7), 8.1930556)
      expect_identical(round(a$sleeplatency[1], 7), 0.2361111)
      expect_identical(a$sleepefficiency[1], 0.72692)
    }
    if (m == 2) expect_identical(a$sleepefficiency[1], 0.74740)
    # check_params only checks the class of sleepefficiency.metric, never its value
    if (m == 3) expect_true(all(is.na(a$sleepefficiency)))
    if (m == 3) expect_false(any(is.na(a$sleeplatency)))
  }
})

test_that("consider_marker_button keeps the two columns and narrows the overlap rule in SPT", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  a <- ours(ms3, fn, list(consider_marker_button = TRUE))
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, list(consider_marker_button = TRUE)),
                           "consider_marker_button")
  expect_identical(ncol(a), 41L)
  expect_true(all(c("sleeplatency", "sleepefficiency") %in% names(a)))
  # the guider is not the marker button here, so the two cells stay empty
  expect_true(all(is.na(a$sleeplatency)))
  mb <- ms3
  mb$part3_guider <- ifelse(is.na(mb$part3_guider), NA, "markerbutton")
  b <- ours(mb, fn, list(consider_marker_button = TRUE))
  expect_same_nightsummary(b, run_ggir_part4(mb, fn, list(consider_marker_button = TRUE)),
                           "markerbutton guider")
  expect_false(any(is.na(b$sleeplatency)))
  expect_identical(b$guider, rep("markerbutton", 4))
})

test_that("storefolderstructure adds the two columns and NA when the path is unknown", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  a <- ours(ms3, fn, list(storefolderstructure = TRUE))
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, list(storefolderstructure = TRUE)),
                           "storefolderstructure")
  expect_identical(ncol(a), 41L)
  expect_identical(names(a)[39:41], c("filename_dir", "foldername", "GGIRversion"))
})

test_that("trap 85: acc_available is FALSE when sumi equals the highest night number", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  te <- ms3
  te$tail_expansion_log <- list(expanded = TRUE)
  a <- ours(te, fn)
  expect_same_nightsummary(a, run_ggir_part4(te, fn), "tail expansion")
  expect_identical(a$acc_available, c(1, 1, 1, 0))
  expect_identical(a$cleaningcode, c(2, 1, 1, 3))
  expect_identical(a$fraction_night_invalid[4], 1)
})

test_that("two sib definitions give one row per night and definition, in GGIR's order", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  two <- ms3
  s2 <- two$sib.cla.sum; s2$definition <- "T10A6"
  two$sib.cla.sum <- rbind(two$sib.cla.sum, s2)
  a <- ours(two, fn)
  expect_same_nightsummary(a, run_ggir_part4(two, fn), "two definitions")
  expect_identical(nrow(a), 8L)
  expect_identical(a$night, c(1, 1, 2, 2, 3, 3, 4, 4))
  expect_identical(a$sleepparam, rep(c("T5A5", "T10A6"), 4))
})

test_that("the page counter reproduces GGIR's, 40 nights to a page", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  base <- ms3$sib.cla.sum[ms3$sib.cla.sum$night == 2, ]
  out <- NULL
  for (n in 1:45) {
    b <- base
    b$night <- n
    sh <- (n - 2) * 86400
    for (cc in c("start.time.day", "sib.onset.time", "sib.end.time")) {
      b[[cc]] <- .raw.posix.to.iso8601(.raw.iso8601.to.posix(b[[cc]], "") + sh, "")
    }
    out <- rbind(out, b)
  }
  rownames(out) <- as.character(seq_len(nrow(out)))
  many <- ms3
  many$sib.cla.sum <- out
  many$SPTE_start <- rep(ms3$SPTE_start[2], 45)
  many$SPTE_end <- rep(ms3$SPTE_end[2], 45)
  many$part3_guider <- rep("HDCZA", 45)
  many$L5list <- rep(ms3$L5list[2], 45)
  many$tib.threshold <- rep(0.13, 45)
  off <- ours(many, fn)
  expect_same_nightsummary(off, run_ggir_part4(many, fn), "45 nights, do.visual FALSE")
  expect_identical(unique(off$page), 1)
  on <- as.ggir.nightsummary(suppressWarnings(
    raw.sleep.nights(many, filename = fn, do.visual = TRUE)))
  expect_same_nightsummary(on, run_ggir_part4(many, fn, list(), visual = TRUE),
                           "45 nights, do.visual TRUE")
  expect_identical(on$page, c(rep(1, 40), rep(2, 5)))
})

test_that("a data cleaning file forces one night onto the guider with cleaningcode 5", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  d <- tempfile("dcf_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dcf <- file.path(d, "dcf.csv")
  utils::write.csv(data.frame(ID = "MOS2E39230594.gt3x", relyonguider_part4 = 3), dcf,
                   row.names = FALSE)
  a <- ours(ms3, fn, list(data_cleaning_file = dcf))
  expect_same_nightsummary(a, run_ggir_part4(ms3, fn, list(data_cleaning_file = dcf)),
                           "data_cleaning_file")
  expect_identical(a$cleaningcode, c(2, 1, 5, 2))
  expect_identical(a$sleeponset[3], a$guider_onset[3])
  expect_identical(a$number_sib_sleepperiod, c(18, 13, 17, 1))
})

test_that("the L5 fall-back chain: L512 when every SPTE is NA, then 21 and 31", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  a <- ms3; a$SPTE_start[] <- NA; a$SPTE_end[] <- NA
  A <- ours(a, fn)
  expect_same_nightsummary(A, run_ggir_part4(a, fn), "SPTE all NA")
  expect_identical(A$guider, rep("L512", 4))
  # L5 minus 6 h, reduced modulo 24 and then quantised by the clock round trip
  expect_equal(A$guider_onset, (ms3$L5list[1:4] - 6) %% 24, tolerance = 1e-12)
  expect_equal(A$guider_wakeup - A$guider_onset, rep(12, 4), tolerance = 1e-12)
  b <- a; b$L5list <- numeric(0)
  B <- ours(b, fn)
  expect_same_nightsummary(B, run_ggir_part4(b, fn), "SPTE all NA, no L5")
  expect_identical(B$guider, rep("notavailable", 4))
  expect_identical(B$guider_onset, rep(21, 4))
  expect_identical(B$guider_wakeup, rep(31, 4))
})

test_that("a night in the list with no sib rows leaves spo undefined and produces no row", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  slog <- p34_fixture("diary", "slog.csv")
  skip_if_no_file(slog)
  drop1 <- ms3
  drop1$sib.cla.sum <- drop1$sib.cla.sum[drop1$sib.cla.sum$night != 1, ]
  args <- list(loglocation = slog)
  a <- ours(drop1, fn, args)
  expect_same_nightsummary(a, run_ggir_part4(drop1, fn, args), "night 1 absent, diary present")
  # the diary carries nights 1 to 7, so night 1 is in the night list
  expect_identical(a$night, c(2, 3, 4))
})

test_that("S8c to S8f the nine stored diary runs are reproduced exactly", {
  skip_if_no_ggir_ref()
  slog <- p34_fixture("diary", "slog.csv")
  slog2 <- p34_fixture("diary", "slog2.csv")
  alog <- p34_fixture("diary", "alog.csv")
  skip_if_no_file(slog)
  runs <- list(
    list(dir = c("diary", "spt"), args = list(loglocation = slog, sleepwindowType = "SPT")),
    list(dir = c("diary", "tib"), args = list(loglocation = slog, sleepwindowType = "TimeInBed")),
    list(dir = c("diary", "nodiary"), args = list()),
    list(dir = c("diary", "adv"), args = list(loglocation = alog, sleepwindowType = "SPT")),
    list(dir = c("diary", "spt_rely"),
         args = list(loglocation = slog, sleepwindowType = "SPT", relyonguider = TRUE)),
    list(dir = c("diary", "tib_TT"),
         args = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                     sib_must_fully_overlap_with_TimeInBed = c(TRUE, TRUE))),
    list(dir = c("diary", "tib_FF"),
         args = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                     sib_must_fully_overlap_with_TimeInBed = c(FALSE, FALSE))),
    list(dir = c("diary", "tib_FT"),
         args = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                     sib_must_fully_overlap_with_TimeInBed = c(FALSE, TRUE))),
    list(dir = c("diary", "tib_TF"),
         args = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                     sib_must_fully_overlap_with_TimeInBed = c(TRUE, FALSE))))
  seen <- 0L
  for (r in runs) {
    fx <- nights_fixture_pair(do.call(p34_fixture, as.list(c(r$dir, "output_din"))))
    if (is.null(fx) || is.null(fx$ms4)) next
    seen <- seen + 1L
    expect_same_nightsummary(ours(fx$ms3, fx$fn, r$args), fx$ms4,
                             paste(r$dir, collapse = "/"))
  }
  expect_gt(seen, 0L)
})

test_that("S8c the diary table, S8e the TimeInBed renames, S8d relyonguider", {
  skip_if_no_ggir_ref()
  slog <- p34_fixture("diary", "slog.csv"); skip_if_no_file(slog)
  fx <- nights_fixture_pair(p34_fixture("diary", "spt", "output_din"))
  if (is.null(fx)) skip("diary fixtures absent")
  a <- ours(fx$ms3, fx$fn, list(loglocation = slog, sleepwindowType = "SPT"))
  expect_identical(round(a$sleeponset, 6),
                   c(22.986111, 26.875000, 23.277778, 22.034722))
  expect_identical(round(a$wakeup, 6), c(31.000000, 40.363889, 30.972222, 23.248611))
  expect_identical(round(a$SptDuration, 7),
                   c(8.0138889, 13.4888889, 7.6944444, 1.2138889))
  expect_identical(a$guider_onset, c(23, 27, 23, 23))
  expect_identical(a$guider_wakeup, c(31, 41, 31, 31))
  expect_identical(a$cleaningcode, c(2, 0, 0, 2))
  expect_identical(a$guider, rep("sleeplog", 4))
  expect_identical(a$sleeplog_used, c(1, 1, 1, 1))
  expect_identical(a$daysleeper, c(0, 1, 0, 0))
  expect_identical(a$number_sib_sleepperiod, c(17, 24, 16, 1))
  # guider_corrected is NA whenever the guider is the sleep log
  expect_true(all(is.na(a$guider_corrected)))
  b <- ours(fx$ms3, fx$fn, list(loglocation = slog, sleepwindowType = "TimeInBed"))
  expect_identical(ncol(b), 41L)
  expect_true(all(c("guider_inbedStart", "guider_inbedEnd", "guider_inbedDuration",
                    "guider_inbedStart_ts", "guider_inbedEnd_ts", "sleeplatency",
                    "sleepefficiency") %in% names(b)))
  expect_false(any(c("guider_onset", "guider_wakeup", "guider_SptDuration") %in% names(b)))
  expect_identical(round(b$sleeplatency, 7), c(0.2263889, 1.4125000, 0.2777778, 0))
  expect_identical(round(b$sleepefficiency, 5), c(0.72986, 0.45456, 0.85469, 0))
  fr <- nights_fixture_pair(p34_fixture("diary", "spt_rely", "output_din"))
  if (!is.null(fr)) {
    c4 <- ours(fr$ms3, fr$fn,
               list(loglocation = slog, sleepwindowType = "SPT", relyonguider = TRUE))
    expect_identical(c4$sleeponset[c(1, 4)], c(23, 23))
    expect_identical(c4$error_onset[c(1, 4)], c(0, 0))
    expect_identical(round(c4$SptDuration[1], 7), 8)
  }
})

test_that("S8f the four sib_must_fully_overlap_with_TimeInBed combinations on night 1", {
  skip_if_no_ggir_ref()
  slog2 <- p34_fixture("diary", "slog2.csv"); skip_if_no_file(slog2)
  want <- list(
    tib_TT = list(v = c(TRUE, TRUE), onset = 23.720833, wake = 29.302778,
                  dur = 4.0680556, lat = 0.2208333),
    tib_FF = list(v = c(FALSE, FALSE), onset = 23.395833, wake = 30.051389,
                  dur = 4.9763889, lat = -0.1041667),
    tib_FT = list(v = c(FALSE, TRUE), onset = 23.395833, wake = 29.302778,
                  dur = 4.3208333, lat = -0.1041667),
    tib_TF = list(v = c(TRUE, FALSE), onset = 23.720833, wake = 30.051389,
                  dur = 4.7236111, lat = 0.2208333))
  seen <- 0L
  for (nm in names(want)) {
    fx <- nights_fixture_pair(p34_fixture("diary", nm, "output_din"))
    if (is.null(fx)) next
    seen <- seen + 1L
    a <- ours(fx$ms3, fx$fn, list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                  sib_must_fully_overlap_with_TimeInBed = want[[nm]]$v))
    expect_identical(round(a$sleeponset[1], 6), want[[nm]]$onset, info = nm)
    expect_identical(round(a$wakeup[1], 6), want[[nm]]$wake, info = nm)
    expect_identical(round(a$SleepDurationInSpt[1], 7), want[[nm]]$dur, info = nm)
    expect_identical(round(a$sleeplatency[1], 7), want[[nm]]$lat, info = nm)
    expect_identical(a$guider_inbedStart[1], 23.5, info = nm)
    expect_identical(a$guider_inbedEnd[1], 30, info = nm)
  }
  if (seen == 0L) skip("the four TimeInBed fixtures are absent")
  # sum(c(FALSE, FALSE)) == 0 disables the narrowing, so FF behaves like SPT
  ff <- nights_fixture_pair(p34_fixture("diary", "tib_FF", "output_din"))
  if (!is.null(ff)) {
    a <- ours(ff$ms3, ff$fn, list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                  sib_must_fully_overlap_with_TimeInBed = c(FALSE, FALSE)))
    b <- ours(ff$ms3, ff$fn, list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                  sib_must_fully_overlap_with_TimeInBed = c(TRUE, TRUE),
                                  consider_marker_button = FALSE))
    expect_false(identical(a$sleeponset, b$sleeponset))
    expect_identical(sum(a$number_sib_sleepperiod >= b$number_sib_sleepperiod), 4L)
  }
})

test_that("TimeInBed with storefolderstructure gives the 43-column shape", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  slog <- p34_fixture("diary", "slog.csv"); skip_if_no_file(slog)
  fx <- nights_fixture_pair(p34_fixture("diary", "spt", "output_din"))
  if (is.null(fx)) skip("diary fixtures absent")
  args <- list(loglocation = slog, sleepwindowType = "TimeInBed", storefolderstructure = TRUE)
  a <- ours(fx$ms3, fx$fn, args)
  expect_identical(ncol(a), 43L)
  expect_same_nightsummary(a, run_ggir_part4(fx$ms3, fx$fn, args), "TimeInBed + storefolder")
  expect_identical(names(a)[c(7:9, 23:26, 41:43)],
                   c("guider_inbedStart", "guider_inbedEnd", "guider_inbedDuration",
                     "guider_inbedStart_ts", "guider_inbedEnd_ts", "sleeplatency",
                     "sleepefficiency", "filename_dir", "foldername", "GGIRversion"))
})

test_that("the diary reaches raw.sleep.nights as an object as well as a path", {
  skip_if_no_ggir_ref()
  slog <- p34_fixture("diary", "slog.csv"); skip_if_no_file(slog)
  fx <- nights_fixture_pair(p34_fixture("diary", "spt", "output_din"))
  if (is.null(fx) || is.null(fx$ms4)) skip("diary fixture absent")
  logs <- raw.sleeplog(slog, sleepwindowType = "SPT")
  byobj <- as.ggir.nightsummary(suppressWarnings(
    raw.sleep.nights(fx$ms3, sleeplog = logs, filename = fx$fn, do.visual = FALSE)))
  byframe <- as.ggir.nightsummary(suppressWarnings(
    raw.sleep.nights(fx$ms3, sleeplog = logs$sleeplog, filename = fx$fn, do.visual = FALSE)))
  expect_identical(byobj, fx$ms4)
  expect_identical(byframe, fx$ms4)
  expect_error(suppressWarnings(raw.sleep.nights(
    fx$ms3, sleeplog = list(sleeplog = NULL, bedlog = logs$sleeplog),
    filename = fx$fn, sleepwindowType = "SPT")), "time in bed")
  expect_error(suppressWarnings(raw.sleep.nights(
    fx$ms3, sleeplog = list(sleeplog = logs$sleeplog, bedlog = NULL),
    filename = fx$fn, loglocation = slog, sleepwindowType = "TimeInBed")), "Time in Bed")
  # a TimeInBed diary passed as an object is not downgraded to SPT by raw.params()
  tib <- nights_fixture_pair(p34_fixture("diary", "tib", "output_din"))
  if (!is.null(tib) && !is.null(tib$ms4)) {
    bl <- raw.sleeplog(slog, sleepwindowType = "TimeInBed")
    b <- suppressWarnings(raw.sleep.nights(tib$ms3, sleeplog = bl, filename = tib$fn,
                                           sleepwindowType = "TimeInBed", do.visual = FALSE))
    expect_identical(as.ggir.nightsummary(b), tib$ms4)
    expect_true(any(grepl("loglocation is empty", attr(b, "canhrActi")$status$messages)))
  }
})

test_that("D1 is_this_a_dst_night returns +1 in spring, -1 in autumn and 0 otherwise", {
  cases <- list(
    # the missing hour of a spring night is an integer (indexed out of c(21:23, 0:7)); the
    # doubled hour of an autumn night is a double (from as.numeric(format))
    list("25/3/2017", "Europe/Helsinki", 1, 3L),
    list("28/10/2017", "Europe/Helsinki", -1, 3),
    list("26/3/2017", "Europe/Helsinki", 0, NULL),
    list("29/10/2017", "Europe/Helsinki", 0, NULL),
    list("1/6/2017", "Europe/Helsinki", 0, NULL),
    list("7/10/2025", "America/Anchorage", 0, NULL),
    list("1/11/2025", "America/Anchorage", -1, 1),
    list("30/3/2019", "Europe/London", 1, 1L),
    list("26/10/2019", "Europe/London", -1, 1))
  for (cs in cases) {
    a <- .raw.nights.dst(cs[[1]], cs[[2]])
    expect_identical(a$dst_night_or_not, cs[[3]], info = paste(cs[[1]], cs[[2]]))
    if (is.null(cs[[4]])) {
      expect_null(a$dsthour, info = paste(cs[[1]], cs[[2]]))
    } else {
      expect_identical(a$dsthour, cs[[4]], info = paste(cs[[1]], cs[[2]]))
    }
  }
  # Lord Howe Island moves its clock by half an hour, which the whole-hour test does not see
  expect_identical(.raw.nights.dst("6/10/2018", "Australia/Lord_Howe")$dst_night_or_not, 0)
})

test_that("D1 is_this_a_dst_night is identical() to GGIR's", {
  skip_if_no_ggir()
  for (tz in c("Europe/Helsinki", "America/Anchorage", "Europe/London", "Australia/Lord_Howe")) {
    for (dt in c("25/3/2017", "28/10/2017", "1/6/2017", "31/3/2018", "6/10/2018",
                 "1/11/2025", "7/10/2025")) {
      expect_identical(.raw.nights.dst(dt, tz), GGIR:::is_this_a_dst_night(dt, tz),
                       info = paste(dt, tz))
    }
  }
})

test_that("D2 and D3 the three daylight saving fixtures are reproduced exactly", {
  skip_if_no_ggir_ref()
  seen <- 0L
  for (nm in c("spring", "autumn", "autumn_edge")) {
    fx <- nights_fixture_pair(p34_fixture(nm, "output_din"))
    if (is.null(fx) || is.null(fx$ms4)) next
    seen <- seen + 1L
    expect_same_nightsummary(ours(fx$ms3, fx$fn), fx$ms4, nm)
  }
  if (seen == 0L) skip("the daylight saving fixtures are absent")
  # spring: the duration is one hour shorter than the span on the night of 25/3/2017
  sp <- nights_fixture_pair(p34_fixture("spring", "output_din"))
  if (!is.null(sp)) {
    a <- ours(sp$ms3, sp$fn)
    i <- which(a$calendar_date == "25/3/2017")
    expect_identical(length(i), 1L)
    expect_identical(round(a$sleeponset[i], 6), 24.288889)
    expect_identical(round(a$wakeup[i], 6), 33.716667)
    expect_identical(round(a$SptDuration[i], 7), 8.4277778)
    expect_identical(round(a$wakeup[i] - a$sleeponset[i] - 1, 7), 8.4277778)
    expect_identical(round(a$guider_SptDuration[i], 7), 8.5402778)
    expect_identical(round(a$guider_wakeup[i] - a$guider_onset[i] - 1, 7), 8.5402778)
    expect_identical(round(a$SleepDurationInSpt[i], 7), 8.1375)
    expect_identical(a$number_sib_sleepperiod[i], 15)
    expect_identical(.raw.nights.dst("25/3/2017", "Europe/Helsinki")$dst_night_or_not, 1)
  }
  # autumn: one hour longer than the span
  au <- nights_fixture_pair(p34_fixture("autumn", "output_din"))
  if (!is.null(au)) {
    a <- ours(au$ms3, au$fn)
    i <- which(a$calendar_date == "28/10/2017")
    expect_identical(length(i), 1L)
    expect_identical(round(a$sleeponset[i], 6), 24.288889)
    expect_identical(round(a$wakeup[i], 6), 31.716667)
    expect_identical(round(a$SptDuration[i], 7), 8.4277778)
    expect_identical(round(a$wakeup[i] - a$sleeponset[i] + 1, 7), 8.4277778)
    expect_identical(round(a$guider_SptDuration[i], 7), 8.5430556)
  }
  # autumn_edge: the SPT ends inside the double hour
  ae <- nights_fixture_pair(p34_fixture("autumn_edge", "output_din"))
  if (!is.null(ae)) {
    a <- ours(ae$ms3, ae$fn)
    i <- which(a$calendar_date == "28/10/2017")
    expect_identical(length(i), 1L)
    expect_identical(round(a$sleeponset[i], 6), 21.527778)
    expect_identical(round(a$wakeup[i], 6), 27.265278)
    expect_identical(round(a$SptDuration[i], 4), 6.7375)
    expect_identical(round(a$wakeup[i] - a$sleeponset[i] + 1, 4), 6.7375)
    expect_identical(round(a$guider_SptDuration[i], 7), 7.3263889)
    expect_identical(round(a$SleepDurationInSpt[i], 7), 6.0861111)
    expect_identical(round(a$WASO[i], 7), 0.6513889)
    expect_identical(a$number_sib_sleepperiod[i], 10)
    j <- which(a$night == 5)
    expect_identical(a$SptDuration[j], a$wakeup[j] - a$sleeponset[j])
  }
})

test_that("correctSptEdgingInDoubleHour adds an hour on both of its branches", {
  ns <- data.frame(a = 1, b = 2, onset = 25.5, wake = 27.5, dur = 2)
  # onset before the double hour, wake inside it
  out <- .raw.nights.correct.double.hour(ns, onsetcol = 3, wakecol = 4, durcol = 5,
                                         dsthour = 3, delta_t1 = c(1, 1))
  expect_identical(out$dur, 3)
  # wake after the double hour, onset inside it
  ns2 <- data.frame(a = 1, b = 2, onset = 27.5, wake = 29.5, dur = 2)
  out2 <- .raw.nights.correct.double.hour(ns2, onsetcol = 3, wakecol = 4, durcol = 5,
                                          dsthour = 3, delta_t1 = c(-1, 1))
  expect_identical(out2$dur, 3)
  # neither: the window is wholly before the double hour
  ns3 <- data.frame(a = 1, b = 2, onset = 22, wake = 26, dur = 4)
  out3 <- .raw.nights.correct.double.hour(ns3, onsetcol = 3, wakecol = 4, durcol = 5,
                                          dsthour = 3, delta_t1 = c(1, 1))
  expect_identical(out3$dur, 4)
  # a window spanning the whole double hour is handled by the caller's +1, not here
  ns4 <- data.frame(a = 1, b = 2, onset = 24, wake = 31, dur = 7)
  out4 <- .raw.nights.correct.double.hour(ns4, onsetcol = 3, wakecol = 4, durcol = 5,
                                          dsthour = 3, delta_t1 = c(1, 1))
  expect_identical(out4$dur, 7)
})

test_that("the day sleeper fixture, its two-day stitch and the demotion of the last night", {
  skip_if_no_ggir_ref()
  fx <- nights_fixture_pair(p34_fixture("daysleeper", "output_din"))
  if (is.null(fx) || is.null(fx$ms4)) skip("the daysleeper fixture is absent")
  a <- ours(fx$ms3, fx$fn)
  expect_same_nightsummary(a, fx$ms4, "daysleeper")
  expect_identical(nrow(a), 4L)
  expect_identical(a$daysleeper, c(1, 1, 1, 0))
  expect_identical(round(a$sleeponset, 6),
                   c(28.986111, 29.512500, 29.768056, 28.034722))
  expect_identical(round(a$wakeup, 6), c(37.179167, 37.766667, 36.972222, 29.248611))
  expect_identical(round(a$SptDuration, 7), c(8.1930556, 8.2541667, 7.2041667, 1.2138889))
  expect_identical(round(a$guider_onset, 6),
                   c(29.074444, 29.459722, 29.723611, 28.040278))
  expect_identical(a$guider_wakeup[4], 36)
  expect_identical(a$cleaningcode, c(2, 1, 1, 1))
  expect_identical(a$number_sib_sleepperiod, c(19, 14, 16, 1))
  # a day sleeper's window runs 18:00 to 18:00, so night 1's invalid fraction is not the
  # night sleeper's
  expect_identical(round(a$fraction_night_invalid, 6), c(0.656192, 0, 0, 0.281308))
  expect_true(all(a$guider_wakeup[1:3] > 36))
})

test_that("F4 an empty sib.cla.sum gives a zero-row table with a state, not a night-0 row", {
  skip_if_no_ggir_ref()
  fx <- nights_fixture_pair(p34_fixture("novalid", "output_din"))
  if (is.null(fx)) skip("the novalid fixture is absent")
  # GGIR writes no ms4 file at all here
  expect_null(fx$ms4)
  x <- suppressWarnings(raw.sleep.nights(fx$ms3, filename = fx$fn))
  expect_identical(nrow(x), 0L)
  expect_identical(ncol(x), 38L)
  expect_identical(attr(x, "canhrActi")$status$state, "no_sib_periods")
  expect_false(any(c("sleeplatency", "sleepefficiency", "GGIRversion") %in% names(x)))
  expect_identical(names(x)[1:3], c("ID", "night", "sleeponset"))
  # as.data.frame(matrix(0, 0, n)) makes every column numeric
  expect_true(all(vapply(x, is.numeric, logical(1))))
  empty3 <- fx$ms3
  empty3["sib.cla.sum"] <- list(NULL)
  y <- suppressWarnings(raw.sleep.nights(empty3, filename = fx$fn))
  expect_identical(nrow(y), 0L)
  expect_identical(attr(y, "canhrActi")$status$state, "no_part3")
})

test_that("F4 the other three failure-mode fixtures are reproduced exactly", {
  skip_if_no_ggir_ref()
  for (nm in c("novalid_keepsibs", "keepsibs")) {
    fx <- nights_fixture_pair(p34_fixture(nm, "output_din"))
    if (is.null(fx) || is.null(fx$ms4)) next
    expect_same_nightsummary(ours(fx$ms3, fx$fn), fx$ms4, nm)
  }
  # with L5 = 0 the guider onset is -6, which GGIR renders as "0-6:00:00"; reading that back
  # gives NA and sends the night down the defaults branch
  expect_identical(.raw.nights.hours2clock(-6), "0-6:00:00")
  expect_true(is.na(suppressWarnings(.raw.nights.clock2hours("0-6:00:00"))))
  fx <- nights_fixture_pair(p34_fixture("novalid_keepsibs", "output_din"))
  if (!is.null(fx) && !is.null(fx$ms4)) {
    a <- ours(fx$ms3, fx$fn)
    expect_identical(nrow(a), 7L)
    expect_identical(a$guider, rep("L512", 7))
    expect_identical(a$guider_onset, rep(-6, 7))
    expect_identical(a$guider_wakeup, rep(30, 7))
    expect_identical(a$guider_SptDuration, rep(36, 7))
    expect_identical(a$cleaningcode, rep(2, 7))
    expect_identical(a$fraction_night_invalid, rep(1, 7))
  }
})

test_that("S7k the no-nights row: night 0, cleaningcode 4 and NA everywhere else", {
  skip_if_no_ggir_ref()
  fx <- nights_fixture_pair(p34_fixture("nonights", "output_din"))
  if (is.null(fx) || is.null(fx$ms4)) skip("the nonights fixture is absent")
  a <- ours(fx$ms3, fx$fn, list(excludefirstlast = TRUE))
  expect_same_nightsummary(a, fx$ms4, "nonights")
  expect_identical(nrow(a), 1L)
  expect_identical(a$night, "0")
  expect_identical(a$cleaningcode, 4)
  expect_identical(a$sleeplog_used, 0)
  expect_identical(a$acc_available, 1)
  expect_identical(a$filename, "MOSfew.gt3x.RData")
  expect_identical(a$ID, "MOSfew.gt3x")
  expect_identical(sum(is.na(a[1, ])), 32L)
  # guider_corrected is outside GGIR's NA assignment and is still NA
  expect_true(is.na(a$guider_corrected))
  expect_identical(a$GGIRversion, fx$ms4$GGIRversion)
})

test_that("F6 a NULL or unusable part-3 object errors with a message that names the problem", {
  expect_error(raw.sleep.nights(NULL), "part3 is NULL")
  expect_error(raw.sleep.nights(list(a = 1)), "no sib.cla.sum")
  expect_error(raw.sleep.nights(42), "must be a canhrActi_raw_sleep")
  expect_error(raw.sleep.nights(list(sib.cla.sum = data.frame()), progress = 3),
               "progress must be NULL or a function")
  expect_error(as.ggir.nightsummary(data.frame(a = 1)), "canhrActi_raw_nights")
})

test_that("F7 a sib definition label that is not a syntactic R name breaks GGIR and not the port", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  odd <- ms3
  odd$sib.cla.sum$definition <- "T5 A5-x"
  # with a day sleeper GGIR reaches eval(parse(text = paste0("spo_day", k, "= spo"))) and dies
  expect_error(suppressWarnings(run_ggir_part4(odd, fn, list(def.noc.sleep = c(3, 17)))),
               "unexpected symbol")
  a <- ours(odd, fn, list(def.noc.sleep = c(3, 17)))
  expect_identical(a$sleepparam, rep("T5 A5-x", 4))
  b <- ours(ms3, fn, list(def.noc.sleep = c(3, 17)))
  for (cc in setdiff(names(a), "sleepparam")) expect_identical(a[[cc]], b[[cc]], info = cc)
  # without a day sleeper GGIR never reaches the eval(parse()) and both agree
  expect_same_nightsummary(ours(odd, fn), run_ggir_part4(odd, fn), "odd label, night sleeper")
})

test_that("the day sleeper two-day stitch is reproduced with two sib definitions", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  ms3 <- nights_loadenv(p3); fn <- NIGHTS_CASES$MOS2$fn
  two <- ms3
  s2 <- two$sib.cla.sum; s2$definition <- "T10A6"
  two$sib.cla.sum <- rbind(two$sib.cla.sum, s2)
  a <- ours(two, fn, list(def.noc.sleep = c(3, 17)))
  expect_same_nightsummary(a, run_ggir_part4(two, fn, list(def.noc.sleep = c(3, 17))),
                           "daysleeper, two definitions")
  expect_identical(nrow(a), 8L)
  expect_identical(a$daysleeper, c(1, 1, 1, 1, 1, 1, 0, 0))
  expect_identical(round(a$sleeponset[1:2], 7), c(26.9361111, 26.9361111))
  expect_identical(round(a$wakeup[1:2], 7), c(40.8375, 40.8375))
  expect_identical(a$number_sib_sleepperiod, c(25, 25, 24, 24, 19, 19, 2, 2))
  # a definition missing on day 1 of the stitch makes GGIR's eval(parse()) read a variable
  # it never created; the port's list keying gives NULL and rbind() carries on
  partial <- ms3
  s3 <- ms3$sib.cla.sum[ms3$sib.cla.sum$night %in% c(2, 3, 4), ]
  s3$definition <- "T10A6"
  partial$sib.cla.sum <- rbind(ms3$sib.cla.sum, s3)
  expect_error(suppressWarnings(run_ggir_part4(partial, fn, list(def.noc.sleep = c(3, 17)))),
               "spo_dayT10A6")
  b <- ours(partial, fn, list(def.noc.sleep = c(3, 17)))
  expect_identical(nrow(b), 8L)
  expect_identical(b$sleepparam, rep(c("T5A5", "T10A6"), 4))
  expect_identical(b[b$night %in% 2:4, ], a[a$night %in% 2:4, ])
})

test_that("convertHRsinceprevMN2Clocktime carries 60 into the next unit and wraps hour 24", {
  expect_identical(.raw.nights.hours2clock(0), "00:00:00")
  expect_identical(.raw.nights.hours2clock(0.5), "00:30:00")
  expect_identical(.raw.nights.hours2clock(12), "12:00:00")
  expect_identical(.raw.nights.hours2clock(23.074537037), "23:04:28")
  expect_identical(.raw.nights.hours2clock(31.3338888889), "07:20:02")
  # only one 24 is subtracted; the hour-24 wrap catches what is left
  expect_identical(.raw.nights.hours2clock(37.5), "13:30:00")
  expect_identical(.raw.nights.hours2clock(48.5), "00:30:00")
  # 50.25 loses one 24 and lands on hour 26, which the == 24 wrap does not catch
  expect_identical(.raw.nights.hours2clock(50.25), "26:15:00")
  expect_identical(.raw.nights.hours2clock(23.999999), "00:00:00")
  expect_identical(.raw.nights.hours2clock(1 + 59 / 60 + 59.6 / 3600), "02:00:00")
  for (v in c(0.5, 12, 23.074537037, 31.3338888889)) {
    expect_equal(.raw.nights.clock2hours(.raw.nights.hours2clock(v)) %% 24, v %% 24,
                 tolerance = 1 / 7200)
  }
})

test_that("doubleDigitClocktime re-pads the strings the basic diary parser strips", {
  expect_identical(.raw.nights.doubledigit("22:45:0"), "22:45:00")
  expect_identical(.raw.nights.doubledigit("7:0:0"), "07:00:00")
  expect_identical(.raw.nights.doubledigit("23:00:00"), "23:00:00")
  expect_identical(.raw.nights.doubledigit("3:5:9"), "03:05:09")
})

test_that("correct01010pattern flips a lone 0 between two 1s and needs two rising edges", {
  expect_identical(.raw.nights.correct01010(c(1, 0, 1, 0, 1)), c(1, 1, 1, 1, 1))
  expect_identical(.raw.nights.correct01010(c(0, 1, 0, 1, 0)), c(0, 1, 1, 1, 0))
  expect_identical(.raw.nights.correct01010(c(1, 0, 1)), c(1, 0, 1))
  expect_identical(.raw.nights.correct01010(c(1, 1, 0, 1)), c(1, 1, 0, 1))
  expect_identical(.raw.nights.correct01010(c(1, 1, 1)), c(1, 1, 1))
  expect_identical(.raw.nights.correct01010(character(0)), numeric(0))
})

test_that("g.create.sp.mat is reproduced exactly, both th2 settings", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  s <- nights_loadenv(p3)$sib.cla.sum
  s$sib.onset.time <- .raw.iso8601.to.posix(s$sib.onset.time, "")
  s$sib.end.time <- .raw.iso8601.to.posix(s$sib.end.time, "")
  for (n in 1:4) {
    st <- s[s$night == n, ]
    nsp <- length(unique(st$sib.period))
    spo <- data.frame(nb = numeric(nsp), start = numeric(nsp), end = numeric(nsp),
                      overlapGuider = numeric(nsp), def = character(nsp))
    for (ds in c(FALSE, TRUE)) {
      expect_identical(.raw.nights.spmat(nsp, spo, st, daysleep = ds),
                       GGIR:::g.create.sp.mat(nsp, spo, st, daysleep = ds),
                       info = paste("night", n, "daysleep", ds))
    }
  }
  st <- s[s$night == 1, ]
  nsp <- length(unique(st$sib.period))
  spo <- data.frame(nb = numeric(nsp), start = numeric(nsp), end = numeric(nsp),
                    overlapGuider = numeric(nsp), def = character(nsp))
  DD <- .raw.nights.spmat(nsp, spo, st, daysleep = FALSE)
  expect_identical(nsp, 34L)
  expect_identical(DD$calendar_date, "7/10/2025")
  expect_identical(DD$wdayname, "Tuesday")
  expect_identical(round(DD$spo$start[1], 7), 21.1833333)
  expect_true(all(DD$spo$end >= DD$spo$start))
  expect_identical(DD$spo$nb, as.numeric(1:34))
})

test_that("the returned object is a data frame with a canhrActi attribute and prints", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  x <- suppressWarnings(raw.sleep.nights(nights_loadenv(p3),
                                         filename = NIGHTS_CASES$MOS2$fn, do.visual = FALSE))
  expect_s3_class(x, "canhrActi_raw_nights")
  expect_s3_class(x, "data.frame")
  md <- attr(x, "canhrActi")
  expect_identical(md$id, "MOS2E39230594.gt3x")
  expect_identical(md$filename, "MOS2E39230594.gt3x.RData")
  expect_false(md$dolog)
  expect_identical(md$status$state, "ok")
  expect_true(md$settings$ggir_exact)
  # the labelled bouts GGIR draws and discards, and the per-night guider table
  expect_identical(nrow(md$episodes), 111L)
  expect_identical(names(md$episodes),
                   c("night", "def", "nb", "start", "end", "overlapGuider", "duration"))
  expect_identical(sum(md$episodes$overlapGuider), 47)
  expect_identical(nrow(md$guiders), 4L)
  expect_identical(md$guiders$night, 1:4)
  expect_identical(md$guiders$guider, rep("HDCZA", 4))
  expect_identical(md$guiders$loaddays, rep(1, 4))
  y <- as.ggir.nightsummary(x)
  expect_null(attr(y, "canhrActi"))
  expect_identical(class(y), "data.frame")
  z <- x
  attr(z, "canhrActi") <- NULL
  class(z) <- "data.frame"
  expect_identical(y, z)
  expect_identical(names(y), names(x))
  expect_identical(attr(y, "row.names"), attr(x, "row.names"))
  out <- utils::capture.output(print.canhrActi_raw_nights(x))
  expect_true(any(grepl("canhrActi per-night sleep summary", out)))
  expect_true(any(grepl("4 rows x 39 columns", out)))
  expect_true(any(grepl("no sleep diary", out)))
})

test_that("a progress callback is called once per night and LC_TIME is restored", {
  skip_if_no_ggir_ref()
  p3 <- nights_path("MOS2", 3); skip_if_no_file(p3)
  seen <- list()
  before <- Sys.getlocale("LC_TIME")
  suppressWarnings(raw.sleep.nights(
    nights_loadenv(p3), filename = NIGHTS_CASES$MOS2$fn, do.visual = FALSE,
    progress = function(stage, i, n, message) {
      seen[[length(seen) + 1]] <<- c(stage, i, n, message)
    }))
  expect_identical(length(seen), 4L)
  expect_identical(seen[[1]][1], "part4")
  expect_identical(seen[[4]][4], "night 4")
  expect_identical(Sys.getlocale("LC_TIME"), before)
  # restored after an error too
  expect_error(suppressWarnings(raw.sleep.nights(NULL)), "part3 is NULL")
  expect_identical(Sys.getlocale("LC_TIME"), before)
})
