# Tests for the valid_days argument of circadian.rhythm() (GGIR's includedaycrit).

test_that("valid_days blanks the per-day rows and leaves the rhythm metrics alone", {
  set.seed(1)
  n_days <- 4
  ts <- seq(as.POSIXct("2024-03-04 00:00:00", tz = "UTC"), by = 60, length.out = n_days * 1440)
  hr <- as.POSIXlt(ts)$hour
  counts <- ifelse(hr >= 8 & hr < 22, rpois(length(ts), 300), rpois(length(ts), 5))
  wear <- rep(TRUE, length(ts))

  full <- circadian.rhythm(counts, ts, wear_time = wear, min_valid_hours = 0)
  keep <- as.Date(ts[1]) + c(0, 2, 3)          # the second day is not a valid day
  gated <- circadian.rhythm(counts, ts, wear_time = wear, min_valid_hours = 0,
                            valid_days = keep)

  dropped <- !(as.Date(gated$daily_metrics$date) %in% keep)
  expect_equal(sum(dropped), 1L)
  expect_true(all(is.na(gated$daily_metrics$L5[dropped])))
  expect_true(all(is.na(gated$daily_metrics$M10_start[dropped])))
  expect_equal(gated$daily_metrics$L5[!dropped], full$daily_metrics$L5[!dropped])
  expect_equal(gated$n_valid_circadian_days, full$n_valid_circadian_days - 1L)

  for (m in c("L5", "M10", "RA", "IS", "IV", "L5_start", "M10_start")) {
    expect_equal(gated[[m]], full[[m]], info = m)
  }
})

test_that("valid_days that names every day changes nothing", {
  set.seed(2)
  ts <- seq(as.POSIXct("2024-03-04 00:00:00", tz = "UTC"), by = 60, length.out = 3 * 1440)
  hr <- as.POSIXlt(ts)$hour
  counts <- ifelse(hr >= 7 & hr < 23, rpois(length(ts), 250), rpois(length(ts), 3))
  a <- circadian.rhythm(counts, ts, min_valid_hours = 0)
  b <- circadian.rhythm(counts, ts, min_valid_hours = 0, valid_days = unique(as.Date(ts)))
  expect_equal(b$daily_metrics, a$daily_metrics)
  expect_equal(b$n_valid_circadian_days, a$n_valid_circadian_days)
})
