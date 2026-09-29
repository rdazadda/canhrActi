# Parity tests for raw.sib.summary() against GGIR's g.sib.sum, against the stored milestone
# and a live call on the same input. The per-epoch series g.sib.sum summarises is stored in
# no milestone, so it is rebuilt with GGIR:::g.sib.det from the part-1 and part-2 milestones;
# those tests skip when GGIR is not installed. Reference data are found through
# CANHRACTI_GGIR_REF and the fixtures under <ref>/../ggir-study-p34/fixtures. The MOS2
# milestones were stored in the system zone of a machine set to America/Anchorage, so the
# file runs in that zone.

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
p34_fixture <- function(case) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", case, "output_din")
}

# The two reference recordings
SUM_CASES <- list(
  MOS2 = list(out = c("out", "output_din"),           fn = "MOS2E39230594.gt3x"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x"))

sum_path <- function(case, what) {
  cc <- SUM_CASES[[case]]
  switch(what,
         basic = do.call(ref_file, c(as.list(cc$out), list("meta", "basic", paste0("meta_", cc$fn, ".RData")))),
         ms2   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms2.out", paste0(cc$fn, ".RData")))),
         ms3   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms3.out", paste0(cc$fn, ".RData")))))
}

# SLE$output from the stored part-1 and part-2 milestones, as g.part3 builds it, and the
# pieces g.sib.sum reads.
sum_build <- function(basic, ms2, ms3, params_sleep = NULL) {
  e1 <- new.env(parent = emptyenv()); load(basic, envir = e1)
  e2 <- new.env(parent = emptyenv()); load(ms2, envir = e2)
  e3 <- new.env(parent = emptyenv()); load(ms3, envir = e3)
  tz <- e3$desiredtz_part1   # the stored run's timezone always wins
  P <- GGIR::load_params(topic = c("sleep", "metrics", "general"))
  ps <- if (is.null(params_sleep)) P$params_sleep else params_sleep
  SLE <- GGIR:::g.sib.det(M = e1$M, IMP = e2$IMP, I = e1$I, twd = c(-12, 12),
                          acc.metric = P$params_general$acc.metric, desiredtz = tz, myfun = c(),
                          sensor.location = P$params_general$sensor.location,
                          params_sleep = ps, zc.scale = P$params_metrics$zc.scale)
  epochs <- SLE$output[, -which(names(SLE$output) == "spt_crude_estimate")]
  list(SLE = SLE, epochs = epochs, M = e1$M, tz = tz, ms3 = e3)
}

sum_series <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref(); skip_if_no_ggir()
    pb <- sum_path(case, "basic"); skip_if_no_file(pb)
    p2 <- sum_path(case, "ms2");   skip_if_no_file(p2)
    p3 <- sum_path(case, "ms3");   skip_if_no_file(p3)
    out <- sum_build(pb, p2, p3)
    assign(case, out, envir = cache)
    out
  }
})

# A synthetic part-3 series and the minimal M that goes with it; timestamps are written in tz.
sum_synth <- function(start, tz, ws3, night, sleep, invalid = 0, posix_style = FALSE) {
  tt <- seq(as.POSIXct(start, tz = tz), by = ws3, length.out = length(night))
  iso <- strftime(tt, format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  time <- if (posix_style) format(tt, tz = tz) else iso
  list(epochs = data.frame(time = time, invalid = invalid, night = night, T5A5 = sleep,
                           stringsAsFactors = FALSE),
       meta = list(metashort = data.frame(timestamp = iso, stringsAsFactors = FALSE),
                   windowsizes = c(ws3, 900, 3600)))
}

# Every synthetic assertion is also checked against live GGIR where it is installed.
expect_same_as_ggir <- function(got, epochs, meta, ignorenonwear, desiredtz) {
  if (!requireNamespace("GGIR", quietly = TRUE)) return(invisible(NULL))
  ggir <- GGIR:::g.sib.sum(list(output = epochs), meta, ignorenonwear = ignorenonwear,
                           desiredtz = desiredtz)
  expect_identical(got, ggir)
}

test_that("S4a MOS2 sib.cla.sum is identical to the stored 111 x 9 table", {
  s <- sum_series("MOS2")
  expect_identical(s$tz, "")
  expect_identical(s$M$windowsizes[1], 5)
  expect_identical(colnames(s$epochs), c("time", "invalid", "night", "T5A5"))

  got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE, desiredtz = s$tz)

  expect_identical(dim(got), c(111L, 9L))
  expect_identical(colnames(got),
                   c("night", "definition", "start.time.day", "nsib.periods",
                     "tot.sib.dur.hrs", "fraction.night.invalid", "sib.period",
                     "sib.onset.time", "sib.end.time"))
  expect_identical(unname(sapply(got, class)),
                   c("numeric", "character", "character", "numeric", "numeric", "numeric",
                     "numeric", "character", "character"))
  expect_identical(rownames(got), as.character(1:111))
  expect_identical(sort(unique(got$night)), c(1, 2, 3, 4))
  expect_identical(as.integer(table(got$night)), c(34L, 38L, 36L, 3L))
  expect_identical(unique(got$definition), "T5A5")
  expect_identical(got, s$ms3$sib.cla.sum)
  expect_same_as_ggir(got, s$epochs, s$M, TRUE, s$tz)
})

test_that("S4b EE sib.cla.sum is identical to the stored 132 x 9 table", {
  s <- sum_series("EE")
  expect_identical(s$tz, "Europe/Helsinki")
  got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE, desiredtz = s$tz)
  expect_identical(dim(got), c(132L, 9L))
  expect_identical(sort(unique(got$night)), c(1, 2, 3, 4, 5, 6))
  expect_identical(as.integer(table(got$night)), c(25L, 22L, 23L, 18L, 20L, 24L))
  expect_identical(got, s$ms3$sib.cla.sum)
  expect_same_as_ggir(got, s$epochs, s$M, TRUE, s$tz)
})

test_that("S4c the first MOS2 row counts both endpoints and rounds twice", {
  s <- sum_series("MOS2")
  got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE, desiredtz = s$tz)
  r1 <- got[1, ]
  expect_identical(r1$sib.onset.time, "2025-10-07T21:11:00-0800")
  expect_identical(r1$sib.end.time,   "2025-10-07T21:18:50-0800")
  # closed-closed: (end - onset)/ws3 + 1 epochs
  secs <- as.numeric(difftime(.raw.iso8601.to.posix(r1$sib.end.time, tz = "UTC"),
                              .raw.iso8601.to.posix(r1$sib.onset.time, tz = "UTC"),
                              units = "secs"))
  expect_identical(secs, 470)
  expect_identical(secs / 5 + 1, 95)
  # 95/12 = 7.9166667 min, round to 7.92, /60 = 0.132 exactly
  expect_identical(r1$tot.sib.dur.hrs, 7.92 / 60)
  expect_identical(round(r1$tot.sib.dur.hrs, 3), 0.132)
  expect_false(identical(r1$tot.sib.dur.hrs, 95 / 12 / 60))   # the double rounding is present
  # start.time.day and nsib.periods are the night's window start and total, repeated per row
  expect_identical(r1$start.time.day, "2025-10-07T20:30:00-0800")
  expect_identical(r1$nsib.periods, 34)
  expect_identical(unique(got$start.time.day[got$night == 1]), "2025-10-07T20:30:00-0800")
  expect_identical(unique(got$nsib.periods[got$night == 1]), 34)
  expect_identical(as.numeric(tapply(got$nsib.periods, got$night, unique)), c(34, 38, 36, 3))
})

test_that("trap 14 the double rounding on a synthetic 95 epoch bout", {
  night <- rep(1, 17280)
  sleep <- rep(0, 17280); sleep[1001:1095] <- 1                    # 95 epochs of 5 s
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 5, night, sleep)
  got <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(nrow(got), 1L)
  expect_identical(got$tot.sib.dur.hrs, 7.92 / 60)
  expect_false(identical(got$tot.sib.dur.hrs, 95 / 12 / 60))
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("S4d fraction.night.invalid counts missing epochs and divides by a calendar day", {
  s <- sum_series("MOS2")
  got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE, desiredtz = s$tz)
  NepochsInDay <- (60 / s$M$windowsizes[1]) * 1440
  expect_identical(NepochsInDay, 17280)
  # the night windows themselves: length and true (pre-blanking) invalid count
  epochs_n <- as.integer(table(factor(s$epochs$night, levels = 1:4)))
  invalid_n <- as.integer(tapply(s$epochs$invalid, factor(s$epochs$night, levels = 1:4),
                                 function(z) sum(z != 0)))
  expect_identical(epochs_n, c(11161L, 17280L, 17280L, 17280L))
  expect_identical(invalid_n, c(900L, 0L, 0L, 9181L))
  expected <- (invalid_n + pmax(0, NepochsInDay - epochs_n)) / NepochsInDay
  expect_identical(round(expected, 7), c(0.4061921, 0, 0, 0.5313079))
  expect_identical(as.numeric(tapply(got$fraction.night.invalid, got$night, unique)), expected)
  # night 1 is short of a calendar day by 6119 epochs and those count as invalid
  expect_identical(17280L - 11161L, 6119L)
  expect_identical(unique(got$fraction.night.invalid[got$night == 1]), (900 + 6119) / 17280)
  expect_identical(unique(got$fraction.night.invalid[got$night == 4]), 9181 / 17280)
})

test_that("trap 13 a 25 hour night is clamped to 1", {
  night <- c(rep(1, 18000), rep(2, 100))         # 25 h at 5 s, then a stub so night 1 is not last
  sleep <- rep(0, 18100); sleep[500:1000] <- 1
  invalid <- c(rep(1, 18000), rep(0, 100))
  s <- sum_synth("2024-10-26 12:00:00", "Europe/Helsinki", 5, night, sleep, invalid)
  got <- raw.sib.summary(s$epochs, s$meta, ignorenonwear = FALSE, desiredtz = "Europe/Helsinki")
  expect_identical(nrow(got), 1L)
  expect_identical(unique(got$fraction.night.invalid), 1)      # not 18000/17280
  expect_gt(18000 / 17280, 1)
  expect_same_as_ggir(got, s$epochs, s$meta, FALSE, "Europe/Helsinki")
})

test_that("S4e ignorenonwear TRUE and FALSE on MOS2", {
  s <- sum_series("MOS2")
  on <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE,  desiredtz = s$tz)
  off <- raw.sib.summary(s$epochs, s$M, ignorenonwear = FALSE, desiredtz = s$tz)
  expect_identical(dim(on), c(111L, 9L))
  expect_identical(round(sum(on$tot.sib.dur.hrs), 4), 33.0625)
  expect_identical(sort(unique(off$night)), c(1, 2, 3, 4, 5, 6, 7))
  expect_identical(dim(off), c(225L, 9L))
  expect_identical(as.integer(table(off$night)), c(38L, 38L, 36L, 29L, 28L, 28L, 28L))
  expect_identical(round(sum(off$tot.sib.dur.hrs), 3), 60.644)
  expect_same_as_ggir(off, s$epochs, s$M, FALSE, s$tz)
  # the fraction is the same either way, because invalid is captured before the blanking
  expect_identical(as.numeric(tapply(on$fraction.night.invalid, on$night, unique)),
                   as.numeric(tapply(off$fraction.night.invalid, off$night, unique))[1:4])
  # nights 5 to 7 are all non-wear and report exactly 1
  expect_identical(unique(off$fraction.night.invalid[off$night %in% c(5, 6, 7)]), 1)
})

test_that("traps 11 and 12 invalid is captured before the blanking, which splits sib runs", {
  night <- rep(1, 1440)
  sleep <- rep(0, 1440); sleep[100:200] <- 1     # one bout of 101 epochs at 60 s
  invalid <- rep(0, 1440); invalid[150] <- 1     # one invalid epoch inside it
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep, invalid)

  on <- raw.sib.summary(s$epochs, s$meta, ignorenonwear = TRUE,  desiredtz = "UTC")
  off <- raw.sib.summary(s$epochs, s$meta, ignorenonwear = FALSE, desiredtz = "UTC")
  # the blanking splits the bout in two, and drops the invalid epoch from both halves
  expect_identical(nrow(on), 2L)
  expect_identical(on$sib.period, c(1, 2))
  expect_identical(on$nsib.periods, c(2, 2))
  expect_identical(on$tot.sib.dur.hrs, c(50 / 60, 50 / 60))
  expect_identical(nrow(off), 1L)
  expect_identical(off$tot.sib.dur.hrs, 101 / 60)
  # the fraction still reports the invalid epoch, because invalid was copied out before the blanking
  expect_identical(unique(on$fraction.night.invalid), 1 / 1440)
  expect_identical(unique(off$fraction.night.invalid), 1 / 1440)
  expect_same_as_ggir(on, s$epochs, s$meta, TRUE, "UTC")
  expect_same_as_ggir(off, s$epochs, s$meta, FALSE, "UTC")
})

test_that("trap 12 a wholly invalid night contributes no row at all under ignorenonwear", {
  night <- c(rep(1, 1440), rep(2, 1440))
  sleep <- rep(0, 2880); sleep[100:200] <- 1; sleep[1600:1700] <- 1
  invalid <- c(rep(1, 1440), rep(0, 1440))
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep, invalid)
  got <- raw.sib.summary(s$epochs, s$meta, ignorenonwear = TRUE, desiredtz = "UTC")
  expect_identical(nrow(got), 1L)
  expect_identical(got$night, 2)
  expect_identical(unique(got$fraction.night.invalid), 0)
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("S4f an undropped spt_crude_estimate column is refused, not counted", {
  s <- sum_series("MOS2")
  # what GGIR does with it: a whole extra definition and 112 rows instead of 111
  ggir <- GGIR:::g.sib.sum(s$SLE, s$M, ignorenonwear = TRUE, desiredtz = s$tz)
  expect_identical(nrow(ggir), 112L)
  expect_identical(sort(unique(as.character(ggir$definition))),
                   c("spt_crude_estimate", "T5A5"))
  # the port refuses the frame instead
  expect_error(raw.sib.summary(s$SLE, s$M, ignorenonwear = TRUE, desiredtz = s$tz),
               "spt_crude_estimate")
  expect_error(raw.sib.summary(s$SLE$output, s$M, ignorenonwear = TRUE, desiredtz = s$tz),
               "spt_crude_estimate")
  # once dropped, as g.part3 drops it, the stored 111 come back
  expect_identical(nrow(raw.sib.summary(s$epochs, s$M, TRUE, s$tz)), 111L)
})

test_that("S4g four definitions give 336 rows with non-contiguous row names", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  pb <- sum_path("MOS2", "basic"); skip_if_no_file(pb)
  p2 <- sum_path("MOS2", "ms2");   skip_if_no_file(p2)
  p3 <- sum_path("MOS2", "ms3");   skip_if_no_file(p3)
  ps <- GGIR::load_params(topic = "sleep")$params_sleep
  ps$timethreshold <- c(5, 10)
  ps$anglethreshold <- c(5, 6)
  s <- sum_build(pb, p2, p3, params_sleep = ps)
  expect_identical(colnames(s$epochs),
                   c("time", "invalid", "night", "sleep.T5A5", "sleep.T5A6",
                     "sleep.T10A5", "sleep.T10A6"))

  got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE, desiredtz = s$tz)
  expect_identical(dim(got), c(336L, 9L))
  expect_identical(unique(got$definition),
                   c("sleep.T5A5", "sleep.T5A6", "sleep.T10A5", "sleep.T10A6"))
  counts <- table(factor(got$definition, levels = unique(got$definition)), got$night)
  expect_identical(as.integer(counts["sleep.T5A5", ]),  c(34L, 38L, 36L, 3L))
  expect_identical(as.integer(counts["sleep.T5A6", ]),  c(37L, 36L, 38L, 3L))
  expect_identical(as.integer(counts["sleep.T10A5", ]), c(19L, 21L, 15L, 1L))
  expect_identical(as.integer(counts["sleep.T10A6", ]), c(19L, 20L, 15L, 1L))
  # the row names are the pre-allocation counter, so each (night, definition) block starts
  # where the previous one left off: 1, 35, 72, 91 within night 1 and 110 for night 2
  first_rn <- as.integer(rownames(got))[!duplicated(paste(got$night, got$definition))]
  expect_identical(first_rn[1:5], c(1L, 35L, 72L, 91L, 110L))
  expect_identical(first_rn[1:4], cumsum(c(1L, 34L, 37L, 19L)))
  expect_same_as_ggir(got, s$epochs, s$M, TRUE, s$tz)
})

test_that("S4h the 04:00 rule fires on a 20:30 Helsinki start and not on a 20:30 Anchorage one", {
  night <- c(rep(0, 720), rep(1, 1440), rep(2, 1440), rep(0, 200))
  sleep <- rep(0, length(night))
  sleep[c(100:160, 1000:1100, 2500:2600, 3700:3750)] <- 1

  hel <- sum_synth("2024-05-07 20:30:00", "Europe/Helsinki", 60, night, sleep)
  ank <- sum_synth("2024-05-07 20:30:00", "America/Anchorage", 60, night, sleep)

  # Helsinki: 20:30 +0300 is 17:30 UTC, as.Date() gives 2024-05-07, and 04:00 on that date is
  # before the start, so night 0 is dropped
  ts_h <- .raw.iso8601.to.posix(hel$meta$metashort$timestamp[1], tz = "Europe/Helsinki")
  expect_identical(format(as.Date(ts_h)), "2024-05-07")
  expect_true(as.POSIXct(paste0(as.Date(ts_h), " 04:00:00"), tz = "Europe/Helsinki") < ts_h)
  gh <- raw.sib.summary(hel$epochs, hel$meta, TRUE, "Europe/Helsinki")
  expect_false(0 %in% gh$night)
  expect_identical(sort(unique(gh$night)), c(1, 2))

  # Anchorage: 20:30 -0800 is 04:30 UTC the next day, as.Date() gives 2024-05-08, and 04:00 on
  # that date is after the start, so night 0 survives
  ts_a <- .raw.iso8601.to.posix(ank$meta$metashort$timestamp[1], tz = "America/Anchorage")
  expect_identical(format(as.Date(ts_a)), "2024-05-08")
  expect_false(as.POSIXct(paste0(as.Date(ts_a), " 04:00:00"), tz = "America/Anchorage") < ts_a)
  ga <- raw.sib.summary(ank$epochs, ank$meta, TRUE, "America/Anchorage")
  expect_true(0 %in% ga$night)
  expect_identical(sort(unique(ga$night)), c(0, 1, 2))
  expect_identical(ga$sib.onset.time[ga$night == 0], "2024-05-07T22:09:00-0800")

  expect_same_as_ggir(gh, hel$epochs, hel$meta, TRUE, "Europe/Helsinki")
  expect_same_as_ggir(ga, ank$epochs, ank$meta, TRUE, "America/Anchorage")
})

test_that("trap 9 the rule reads the part-1 metashort timestamp, not the series", {
  # g.sib.sum reads M$metashort$timestamp[1] and nothing else from M besides windowsizes
  night <- c(rep(0, 720), rep(1, 1440), rep(2, 720))
  sleep <- rep(0, length(night)); sleep[c(100:160, 1000:1100, 2200:2300)] <- 1
  s <- sum_synth("2024-05-07 20:30:00", "America/Anchorage", 60, night, sleep)
  keeps <- raw.sib.summary(s$epochs, s$meta, TRUE, "America/Anchorage")
  expect_true(0 %in% keeps$night)
  s2 <- s
  s2$meta$metashort$timestamp[1] <- "2024-05-07T09:30:00-0800"   # a morning start, after 04:00
  drops <- raw.sib.summary(s2$epochs, s2$meta, TRUE, "America/Anchorage")
  expect_false(0 %in% drops$night)
  expect_identical(data.frame(drops, row.names = NULL),
                   data.frame(keeps[keeps$night != 0, ], row.names = NULL))
  # dropping night 0 moves every later row up one slot of the pre-allocation counter
  expect_identical(rownames(drops), c("1", "2"))
  expect_identical(rownames(keeps[keeps$night != 0, ]), c("2", "3"))
  expect_same_as_ggir(drops, s2$epochs, s2$meta, TRUE, "America/Anchorage")
})

test_that("S4i MOS2 nights 5, 6 and 7 leave no row at all", {
  s <- sum_series("MOS2")
  got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = TRUE, desiredtz = s$tz)
  expect_identical(sort(unique(s$epochs$night)), c(0, 1, 2, 3, 4, 5, 6, 7))
  expect_false(any(got$night %in% c(5, 6, 7)))
  expect_identical(max(got$night), 4)
  # nights 5 to 7 exist in the series and are wholly invalid, which is why they vanish
  expect_identical(as.integer(table(factor(s$epochs$night, levels = 5:7))),
                   c(17280L, 17280L, 17280L))
  expect_identical(as.numeric(tapply(s$epochs$invalid,
                                     factor(s$epochs$night, levels = 5:7), mean)), c(1, 1, 1))
  # the placeholder rows they produce carry sib.period 0 and are deleted by the final trim
  expect_false(any(got$sib.period == 0))
  expect_false(any(got$night == -1))
})

test_that("epochs after the last night are relabelled -1 and never produce rows", {
  night <- c(rep(1, 1440), rep(2, 1440), rep(0, 500))   # 500 trailing epochs labelled 0
  sleep <- rep(0, length(night))
  sleep[c(100:200, 1600:1700, 2950:3050)] <- 1          # the last bout is in the tail
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep)
  got <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(sort(unique(got$night)), c(1, 2))
  expect_identical(nrow(got), 2L)   # the tail bout is dropped with the -1 label
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("trap 78 a bout that straddles the night boundary is split and counted twice", {
  night <- c(rep(1, 720), rep(2, 720), rep(0, 10))
  sleep <- rep(0, length(night)); sleep[700:740] <- 1   # 21 epochs in night 1, 20 in night 2
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep)
  got <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(nrow(got), 2L)
  expect_identical(got$night, c(1, 2))
  expect_identical(got$sib.period, c(1, 1))
  expect_identical(got$nsib.periods, c(1, 1))
  expect_identical(got$tot.sib.dur.hrs, c(21 / 60, 20 / 60))
  expect_identical(got$sib.end.time[1], s$epochs$time[720])    # clipped at the boundary
  expect_identical(got$sib.onset.time[2], s$epochs$time[721])
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("trap 76 the 1000 row pre-allocation grows past 800 and keeps its indices", {
  # three nights of 360 periods each: 1080 rows, past both the growth at 800 and the initial 1000
  night <- rep(1:3, each = 1440)
  sleep <- rep(rep(c(1, 1, 0, 0), 360), 3)
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep)
  got <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(nrow(got), 1080L)
  expect_identical(unique(got$nsib.periods), 360)
  expect_identical(max(as.integer(rownames(got))), 1080L)
  expect_identical(unique(got$tot.sib.dur.hrs), 2 / 60)
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("trap 75 a middle night with no sib period leaves a gap in the row names", {
  night <- rep(1:3, each = 1440)
  sleep <- rep(0, 4320)
  sleep[100:200] <- 1; sleep[300:400] <- 1     # night 1: two bouts
  sleep[3000:3100] <- 1                        # night 3: one bout; night 2 has none
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep)
  got <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(got$night, c(1, 1, 3))
  # night 2's placeholder occupied slot 3 and was deleted, so the last row is named 4
  expect_identical(rownames(got), c("1", "2", "4"))
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("a recording with no sib period at all returns a zero-row frame, not an error", {
  night <- rep(1:2, each = 1440)
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, rep(0, 2880))
  got <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(dim(got), c(0L, 9L))
  expect_identical(unname(sapply(got, class)),
                   c("numeric", "character", "character", "numeric", "numeric", "numeric",
                     "numeric", "character", "character"))
  expect_same_as_ggir(got, s$epochs, s$meta, TRUE, "UTC")
})

test_that("POSIX style timestamps are re-rendered as ISO 8601, ISO ones are passed through", {
  night <- rep(1, 1440)
  sleep <- rep(0, 1440); sleep[100:200] <- 1
  iso <- sum_synth("2024-05-07 12:00:00", "Europe/Helsinki", 60, night, sleep)
  pos <- sum_synth("2024-05-07 12:00:00", "Europe/Helsinki", 60, night, sleep, posix_style = TRUE)
  expect_true(grepl(" ", pos$epochs$time[1], fixed = TRUE))
  expect_false(grepl(" ", iso$epochs$time[1], fixed = TRUE))
  a <- raw.sib.summary(iso$epochs, iso$meta, TRUE, "Europe/Helsinki")
  b <- raw.sib.summary(pos$epochs, pos$meta, TRUE, "Europe/Helsinki")
  expect_identical(a, b)                       # the space branch reproduces the ISO strings
  expect_identical(a$sib.onset.time, "2024-05-07T13:39:00+0300")
  expect_same_as_ggir(b, pos$epochs, pos$meta, TRUE, "Europe/Helsinki")
})

test_that("a factor time column, which is what g.sib.det emits, gives the same table", {
  night <- rep(1, 1440)
  sleep <- rep(0, 1440); sleep[100:200] <- 1
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep)
  chr <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  s$epochs$time <- factor(s$epochs$time)
  fct <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(fct, chr)
})

test_that("the input guards name what is wrong", {
  night <- rep(1, 600)
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, rep(c(0, 1), 300))
  expect_s3_class(raw.sib.summary(s$epochs, s$meta, TRUE, "UTC"), "data.frame")
  expect_error(raw.sib.summary(s$epochs[, c("time", "night", "T5A5")], s$meta, TRUE, "UTC"),
               "invalid")
  expect_error(raw.sib.summary(s$epochs[, c("time", "invalid", "night")], s$meta, TRUE, "UTC"),
               "no sib definition column")
  expect_error(raw.sib.summary(list(nope = s$epochs), s$meta, TRUE, "UTC"), "output member")
  expect_error(raw.sib.summary(s$epochs, list(windowsizes = c(60, 900, 3600)), TRUE, "UTC"),
               "canhrActi_raw")
  expect_error(raw.sib.summary(s$epochs, list(metashort = data.frame(x = 1),
                                              windowsizes = c(60, 900, 3600)), TRUE, "UTC"),
               "timestamp")
})

test_that("sib and meta accept the canhrActi shapes as well as GGIR's", {
  night <- rep(1, 1440)
  sleep <- rep(0, 1440); sleep[100:200] <- 1
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, sleep)
  base <- raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  # a canhrActi_raw_meta carries an extra time column in metashort; only timestamp is read
  cm <- structure(list(metashort = data.frame(time = seq_len(1440),
                                              timestamp = s$meta$metashort$timestamp,
                                              stringsAsFactors = FALSE),
                       windowsizes = s$meta$windowsizes), class = "canhrActi_raw_meta")
  expect_identical(raw.sib.summary(s$epochs, cm, TRUE, "UTC"), base)
  # a whole canhrActi_raw, from which $meta is taken
  cr <- structure(list(meta = cm), class = "canhrActi_raw")
  expect_identical(raw.sib.summary(s$epochs, cr, TRUE, "UTC"), base)
  # GGIR's SLE list, and a canhrActi_raw_sib shaped list
  expect_identical(raw.sib.summary(list(output = s$epochs), s$meta, TRUE, "UTC"), base)
  sib <- structure(list(output = s$epochs, detection.failed = FALSE),
                   class = "canhrActi_raw_sib")
  expect_identical(raw.sib.summary(sib, s$meta, TRUE, "UTC"), base)
})

test_that("the LC_TIME locale is restored on exit", {
  night <- rep(1, 600)
  s <- sum_synth("2024-05-07 12:00:00", "UTC", 60, night, rep(c(0, 1), 300))
  before <- Sys.getlocale("LC_TIME")
  raw.sib.summary(s$epochs, s$meta, TRUE, "UTC")
  expect_identical(Sys.getlocale("LC_TIME"), before)
  expect_error(raw.sib.summary(s$epochs[, 1:3], s$meta, TRUE, "UTC"), "no sib definition column")
  expect_identical(Sys.getlocale("LC_TIME"), before)
})

# The daylight saving and failure-mode fixtures, with their stored shapes.
SUM_FIXTURES <- list(
  spring           = list(fn = "EEspring.gt3x",  inw = TRUE,  dim = c(132L, 9L),
                          per_night = c(25L, 22L, 23L, 18L, 20L, 24L)),
  autumn           = list(fn = "EEautumn.gt3x",  inw = TRUE,  dim = c(132L, 9L),
                          per_night = c(25L, 22L, 23L, 18L, 20L, 24L)),
  autumn_edge      = list(fn = "EEautumnE.gt3x", inw = TRUE,  dim = c(132L, 9L),
                          per_night = c(25L, 22L, 25L, 16L, 22L, 22L)),
  daysleeper       = list(fn = "MOSday.gt3x",    inw = TRUE,  dim = c(112L, 9L),
                          per_night = c(18L, 41L, 43L, 10L)),
  novalid          = list(fn = "MOSnw.gt3x",     inw = TRUE,  dim = c(0L, 9L),
                          per_night = integer(0)),
  novalid_keepsibs = list(fn = "MOSnw.gt3x",     inw = FALSE, dim = c(7L, 9L),
                          per_night = rep(1L, 7)),
  keepsibs         = list(fn = "MOSkeep.gt3x",   inw = FALSE, dim = c(225L, 9L),
                          per_night = c(38L, 38L, 36L, 29L, 28L, 28L, 28L)),
  nonights         = list(fn = "MOSfew.gt3x",    inw = TRUE,  dim = c(55L, 9L),
                          per_night = c(34L, 21L)))

for (.case in names(SUM_FIXTURES)) {
  local({
    case <- .case
    cfg <- SUM_FIXTURES[[case]]
    test_that(paste0("fixture ", case, " sib.cla.sum is identical to the stored one"), {
      skip_if_no_ggir_ref(); skip_if_no_ggir()
      dir <- p34_fixture(case)
      if (!dir.exists(dir)) skip(paste0("fixture not present: ", dir))
      pb <- file.path(dir, "meta", "basic", paste0("meta_", cfg$fn, ".RData"))
      p2 <- file.path(dir, "meta", "ms2.out", paste0(cfg$fn, ".RData"))
      p3 <- file.path(dir, "meta", "ms3.out", paste0(cfg$fn, ".RData"))
      skip_if_no_file(pb); skip_if_no_file(p2); skip_if_no_file(p3)
      s <- sum_build(pb, p2, p3)
      got <- raw.sib.summary(s$epochs, s$M, ignorenonwear = cfg$inw, desiredtz = s$tz)
      expect_identical(dim(got), cfg$dim)
      expect_identical(as.integer(table(got$night)), cfg$per_night)
      expect_identical(got, s$ms3$sib.cla.sum)
      expect_same_as_ggir(got, s$epochs, s$M, cfg$inw, s$tz)
      if (case == "novalid") {
        # a zero-row frame keeps the nine columns and their classes
        expect_identical(unname(sapply(got, class)),
                         c("numeric", "character", "character", "numeric", "numeric", "numeric",
                           "numeric", "character", "character"))
      }
      if (case == "novalid_keepsibs") {
        # every night is one 24 h sib, except the truncated first one
        expect_identical(round(got$tot.sib.dur.hrs, 5),
                         c(15.50133, 24, 24, 24, 24, 24, 24))
        expect_identical(unique(got$fraction.night.invalid), 1)
      }
    })
  })
}
