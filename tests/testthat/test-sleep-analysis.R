# sleep.fragmentation.enhanced() (R/sleep_analysis.R), on a night whose bouts, transitions
# and composite index are counted by hand, and the clock sleep.tudor.locke()'s daytime
# filter reads

# from 22:00: W10 S200 W5 S5 W3 S250 W7, 480 epochs
.night <- function(epoch = 60) {
  st <- rep(c("W", "S", "W", "S", "W", "S", "W"), c(10, 200, 5, 5, 3, 250, 7))
  list(st = st,
       ts = seq(as.POSIXct("2024-03-04 22:00:00", tz = "UTC"), by = epoch, length.out = length(st)))
}

test_that("bouts, awakenings and transitions of a hand-counted night", {
  n <- .night()
  r <- sleep.fragmentation.enhanced(n$st, n$ts)
  expect_identical(names(r), c("basic_metrics", "bout_analysis", "temporal_pattern",
                               "sleep_fragmentation_index", "transition_rate_per_hour",
                               "short_sleep_proportion"))
  bm <- r$basic_metrics
  # a wake bout after sleep is an awakening, the one before sleep onset is not
  expect_identical(bm$n_awakenings, 3L)
  expect_identical(c(bm$total_sleep_epochs, bm$total_wake_epochs), c(455L, 25L))
  expect_identical(bm$total_transitions, 6L)
  expect_equal(bm$sleep_efficiency, round(100 * 455 / 480, 2))
  sb <- r$bout_analysis$sleep_bouts
  expect_identical(sb$count, 3L)
  expect_equal(c(sb$mean, sb$median, sb$min, sb$max), c(455 / 3, 200, 5, 250))
  expect_equal(sb$sd, stats::sd(c(200, 5, 250)))
  wb <- r$bout_analysis$wake_bouts
  expect_identical(wb$count, 4L)
  expect_equal(c(wb$mean, wb$median, wb$min, wb$max), c(6.25, 6, 3, 10))
})

test_that("the transitions per clock hour and the composite index", {
  n <- .night()
  r <- sleep.fragmentation.enhanced(n$st, n$ts)
  # the state changes at 22:10, 01:30, 01:35, 01:40, 01:43 and 05:53
  expect_identical(r$temporal_pattern$hour, c(0, 1, 2, 3, 4, 5, 22, 23))
  expect_identical(r$temporal_pattern$transitions, c(0, 4, 0, 0, 0, 1, 1, 0))
  # 6 changes in 8 hours, 1 of the 3 sleep bouts under 10 minutes, 25 of 480 epochs awake
  expect_identical(r$transition_rate_per_hour, 0.75)
  expect_equal(r$short_sleep_proportion, round(100 / 3, 2))
  expect_equal(r$sleep_fragmentation_index, round(0.75 * 10 + 50 / 3 + 40 * 25 / 480, 2))
})

test_that("epoch_length turns the epoch count into hours for the transition rate", {
  n <- .night(epoch = 30)
  r <- sleep.fragmentation.enhanced(n$st, n$ts, epoch_length = 30)
  # the same 480 epochs now span 4 hours
  expect_identical(r$transition_rate_per_hour, 1.5)
  expect_equal(r$sleep_fragmentation_index, round(1.5 * 10 + 50 / 3 + 40 * 25 / 480, 2))
})

test_that("short sleep bouts are those under 10 minutes whatever the epoch", {
  # at 30 s a 12 epoch bout is 6 minutes, so two of the three sleep bouts are short
  st <- rep(c("S", "W", "S", "W", "S"), c(12, 4, 12, 4, 200))
  ts <- seq(as.POSIXct("2024-03-04 22:00:00", tz = "UTC"), by = 30, length.out = length(st))
  r <- sleep.fragmentation.enhanced(st, ts, epoch_length = 30)
  expect_equal(r$short_sleep_proportion, round(200 / 3, 2))
})

test_that("an epoch not scored (NA) is neither sleep nor wake and ends the bout it falls in", {
  # a night with the hour from midnight not worn: W10 S110 NA60 S5 W3 S250 W7
  st <- rep(c("W", "S", NA, "S", "W", "S", "W"), c(10, 110, 60, 5, 3, 250, 7))
  ts <- seq(as.POSIXct("2024-03-04 22:00:00", tz = "UTC"), by = 60, length.out = length(st))
  r <- sleep.fragmentation.enhanced(st, ts)
  bm <- r$basic_metrics
  expect_identical(c(bm$total_sleep_epochs, bm$total_wake_epochs), c(365L, 20L))
  # the gap splits the sleep into bouts of 110 and 5, with no transition between them
  sb <- r$bout_analysis$sleep_bouts
  expect_identical(sb$count, 3L)
  expect_equal(c(sb$median, sb$min, sb$max), c(110, 5, 250))
  expect_identical(r$bout_analysis$wake_bouts$count, 3L)
  expect_identical(bm$n_awakenings, 2L)
  expect_identical(bm$total_transitions, 4L)
  # rates and proportions are over the 385 scored minutes
  expect_equal(bm$sleep_efficiency, round(100 * 365 / 385, 2))
  expect_equal(r$transition_rate_per_hour, round(4 / (385 / 60), 2))
  expect_equal(r$short_sleep_proportion, round(100 / 3, 2))
  expect_equal(r$sleep_fragmentation_index,
               round(4 / (385 / 60) * 10 + 50 / 3 + 40 * 20 / 385, 2))
  # the hour with no scored epoch is left out of the hourly pattern
  expect_identical(r$temporal_pattern$hour, c(1, 2, 3, 4, 5, 22, 23))
  expect_identical(r$temporal_pattern$transitions, c(2, 0, 0, 0, 1, 1, 0))
})

test_that("a change of state across a gap is not an awakening, and a night never scored is empty", {
  ts <- seq(as.POSIXct("2024-03-04 22:00:00", tz = "UTC"), by = 60, length.out = 60)
  r <- sleep.fragmentation.enhanced(rep(c("S", NA, "W"), c(30, 10, 20)), ts)
  expect_identical(r$basic_metrics$n_awakenings, 0L)
  expect_identical(r$basic_metrics$total_transitions, 0L)
  expect_identical(c(r$bout_analysis$sleep_bouts$count, r$bout_analysis$wake_bouts$count),
                   c(1L, 1L))
  expect_identical(r$transition_rate_per_hour, 0)
  expect_identical(sleep.fragmentation.enhanced(rep(NA_character_, 60), ts),
                   sleep.fragmentation.enhanced(character(0), ts[0]))
})

test_that("a recording with its non-wear set to NA, as the dashboard passes it, gives numbers", {
  d <- agd.counts(read.agd(example_agd(1), verbose = FALSE))
  st <- sleep.cole.kripke(d$axis1)
  st[!wear.choi(d$axis1)] <- NA
  r <- sleep.fragmentation.enhanced(st, d$timestamp)
  expect_false(anyNA(unlist(r$basic_metrics)))
  expect_false(anyNA(c(r$sleep_fragmentation_index, r$transition_rate_per_hour,
                       r$short_sleep_proportion)))
  expect_identical(r$basic_metrics$total_sleep_epochs + r$basic_metrics$total_wake_epochs,
                   sum(!is.na(st)))
})

test_that("an unbroken night scores 0 and a change every epoch is capped at 100", {
  ts <- seq(as.POSIXct("2024-03-04 22:00:00", tz = "UTC"), by = 60, length.out = 120)
  r <- sleep.fragmentation.enhanced(rep("S", 120), ts)
  expect_identical(r$sleep_fragmentation_index, 0)
  expect_identical(r$basic_metrics$n_awakenings, 0L)
  expect_identical(r$basic_metrics$total_transitions, 0L)
  expect_identical(r$basic_metrics$sleep_efficiency, 100)
  expect_identical(r$bout_analysis$wake_bouts$count, 0L)
  expect_identical(r$bout_analysis$wake_bouts$mean, NA_real_)
  # one sleep bout has no standard deviation
  expect_identical(r$bout_analysis$sleep_bouts$sd, NA_real_)
  r2 <- sleep.fragmentation.enhanced(rep(c("S", "W"), 60), ts)
  expect_identical(r2$transition_rate_per_hour, 59.5)
  expect_identical(r2$sleep_fragmentation_index, 100)
})

test_that("empty input gives the empty result and a length mismatch warns", {
  e <- sleep.fragmentation.enhanced(character(0), as.POSIXct(character(0), tz = "UTC"))
  expect_identical(e$sleep_fragmentation_index, NA_real_)
  expect_identical(e$basic_metrics$n_awakenings, NA_integer_)
  expect_identical(e$bout_analysis$sleep_bouts$count, 0)
  expect_null(e$temporal_pattern)
  # the epoch counts need no clock, so they survive timestamps of the wrong length
  n <- .night()
  expect_warning(r <- sleep.fragmentation.enhanced(n$st, n$ts[1:10]),
                 "sleep_state and timestamps length mismatch")
  expect_identical(r$basic_metrics$total_transitions, 6L)
  expect_identical(r$transition_rate_per_hour, 0.75)
})

test_that("the daytime filter reads each period's hour as written, even one in a DST gap", {
  # 02:30 on 9 March 2025 does not exist in Alaska, the zone this test runs in; the
  # 10:00 nap of 169 minutes at 93% efficiency is a daytime period all the same
  withr::local_timezone("America/Anchorage")
  ts <- seq(as.POSIXct("2025-03-08 20:00:00", tz = "UTC"), by = 60, length.out = 2880)
  st <- rep("W", 2880)
  st[391:690] <- rep(c("S", "W", "S", "W", "S"), c(100, 4, 100, 4, 92))
  st[2281:2461] <- rep(c("S", "W", "S", "W", "S", "W", "S"), c(40, 5, 40, 4, 40, 3, 49))
  counts <- rep(20, 2880)
  expect_identical(sleep.tudor.locke(st, ts, counts = counts)$in_bed_time,
                   c("2025-03-09 02:30:00", "2025-03-10 10:00:00"))
  expect_warning(p <- sleep.tudor.locke(st, ts, counts = counts, filter_suspicious = TRUE),
                 "Removed 1 suspicious sleep period")
  expect_identical(p$in_bed_time, "2025-03-09 02:30:00")
})
