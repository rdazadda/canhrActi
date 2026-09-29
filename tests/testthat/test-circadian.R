test_that("circadian.rhythm calculates basic metrics", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_s3_class(result, "canhrActi_circadian")
  expect_true("L5" %in% names(result))
  expect_true("M10" %in% names(result))
  expect_true("RA" %in% names(result))
})

test_that("circadian.rhythm calculates L5 and M10 correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(10, 300), rep(500, 840), rep(10, 300))

  result <- circadian.rhythm(counts, timestamps)

  expect_equal(result$L5, 10)
  expect_equal(result$M10, 500)
})

test_that("circadian.rhythm calculates relative amplitude", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(10, 300), rep(500, 840), rep(10, 300))

  result <- circadian.rhythm(counts, timestamps)

  expect_true(result$RA >= 0 && result$RA <= 1)
  expected_ra <- (result$M10 - result$L5) / (result$M10 + result$L5)
  expect_equal(result$RA, expected_ra, tolerance = 0.01)
})

test_that("circadian.rhythm calculates interdaily stability", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 2880)
  counts <- rep(c(rep(50, 360), rep(500, 600), rep(50, 480)), 2)

  result <- circadian.rhythm(counts, timestamps)

  expect_true("IS" %in% names(result))
  expect_true(result$IS >= 0 && result$IS <= 1)
})

test_that("circadian.rhythm calculates intradaily variability", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_true("IV" %in% names(result))
  expect_true(result$IV >= 0)
})

test_that("circadian.rhythm respects wear time", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))
  wear_time <- rep(TRUE, 1440)
  wear_time[1:100] <- FALSE

  result <- circadian.rhythm(counts, timestamps, wear_time = wear_time)

  expect_s3_class(result, "canhrActi_circadian")
  expect_equal(result$n_valid_epochs, 1340)
})

test_that("circadian.rhythm validates input lengths", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 100)
  counts <- rep(500, 50)

  expect_error(circadian.rhythm(counts, timestamps), "same length")
})

test_that("circadian.rhythm handles empty input", {
  timestamps <- as.POSIXct(character(0))
  counts <- numeric(0)

  expect_error(circadian.rhythm(counts, timestamps), "No data")
})

test_that("circadian.rhythm creates hourly profile", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_true("hourly_profile" %in% names(result))
  expect_equal(nrow(result$hourly_profile), 24)
})

test_that("print method works for circadian results", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_output(print(result), "CIRCADIAN", ignore.case = TRUE)
})

test_that("plot method works for circadian results", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_silent(plot(result))
})

test_that("circadian.rhythm reads the hour and the date on the timestamps' own clock", {
  withr::local_timezone("America/Anchorage")
  # three days from midnight: in the session zone, and labelled UTC on the same clock
  local_ts <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 4320)
  utc_ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 4320)
  counts <- ifelse(rep(0:23, each = 60, times = 3) %in% 8:19, 500, 50) + seq_len(4320) %% 7

  utc <- circadian.rhythm(counts, utc_ts)
  for (cpp in c(TRUE, FALSE)) {
    loc <- circadian.rhythm(counts, local_ts, use_cpp = cpp)
    expect_equal(loc$IV, utc$IV)
    expect_identical(loc$daily_metrics, utc$daily_metrics)
    expect_identical(loc$n_days_analyzed, 3L)
  }
  expect_identical(cosinor.analysis(counts, local_ts)$n_days, 3L)
})

test_that("the valid-day rule splits days at the timestamps' own midnight", {
  withr::local_timezone("America/Anchorage")
  ts <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 4320)
  counts <- ifelse(rep(0:23, each = 60, times = 3) %in% 8:19, 500, 50)

  # three whole days worn, so no day is under the 10 hours
  res <- circadian.rhythm(counts, ts, wear_time = rep(TRUE, 4320))
  expect_identical(res$n_valid_epochs, 4320L)
})

test_that("IS and IV of a constant series are NA in the C++ and R versions alike", {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 2880)
  # 0.1 has no exact binary form, so a plain running sum of it drifts
  for (level in c(100, 0.1)) {
    expect_identical(.calculate.IS.IV(rep(level, 2880), ts), list(IS = NA_real_, IV = NA_real_))
    expect_identical(IS_cpp(rep(level, 48)), NA_real_)
    expect_identical(IV_cpp(rep(level, 48)), NA_real_)
  }
  expect_identical(circadian_cpp(rep(0, 3 * 1440))$IS, NA_real_)
  expect_identical(IS_cpp(c(NA, 1:47)), NA_real_)
})

test_that("circadian.rhythm gives no levels with or without C++ when no epoch is worn", {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 2880)
  counts <- rep(c(rep(50, 360), rep(500, 600), rep(50, 480)), 2)

  for (cpp in c(TRUE, FALSE)) {
    res <- circadian.rhythm(counts, ts, wear_time = rep(FALSE, 2880), use_cpp = cpp)
    expect_identical(c(res$L5, res$M10, res$RA), rep(NA_real_, 3))
  }
  expect_identical(L5M10_cpp(rep(NA_real_, 2880))$L5_value, NA_real_)
})

test_that("the daily plot names its dates in English under a German LC_TIME", {
  ts <- seq(as.POSIXct("2024-10-06 00:00:00", tz = "UTC"), by = 60, length.out = 7 * 1440)
  counts <- rep(c(rep(50, 360), rep(500, 600), rep(50, 480)), 7)
  p <- plot(circadian.rhythm(counts, ts), type = "daily")
  labels_of <- function(p) {
    labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
    unname(labs[!is.na(labs)])
  }
  en <- withr::with_locale(c(LC_TIME = "C"), labels_of(p))
  local_german_time()
  skip_if(format(as.Date("2024-10-06"), "%b") == "Oct", "no German month names under the German LC_TIME")
  expect_true(length(en) > 0 && all(substr(en, 1, 3) %in% month.abb))
  expect_identical(labels_of(p), en)
})
