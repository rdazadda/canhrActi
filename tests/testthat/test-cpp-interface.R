# Tests for cpp_interface.R - C++ accelerated functions

test_that("cpp_available returns logical", {
  result <- cpp_available()

  expect_true(is.logical(result))
  expect_length(result, 1)
})

test_that("backend_info runs without error", {
  expect_error(backend_info(), NA)
})

test_that("IS_cpp calculates interdaily stability", {
  set.seed(42)
  counts <- rep(c(rep(50, 8), rep(500, 10), rep(200, 6)), 3)

  result <- IS_cpp(counts)

  # three identical days are perfectly stable
  expect_equal(result, 1)
  # two days in antiphase have flat hour-of-day means
  expect_equal(IS_cpp(c(rep(c(0, 10), 12), rep(c(10, 0), 12))), 0)
})

test_that("IV_cpp calculates intradaily variability", {
  set.seed(42)
  counts <- rep(c(rep(50, 8), rep(500, 10), rep(200, 6)), 3)

  result <- IV_cpp(counts)

  # by hand: squared successive differences sum to 922500, squared deviations to 2835000
  expect_equal(result, 72 * 922500 / (71 * 2835000))
})

test_that("L5M10_cpp returns correct structure", {
  counts <- rep(100, 1440)
  counts[121:420] <- 20     # 02:00 to 07:00
  counts[601:1200] <- 900   # 10:00 to 20:00

  result <- L5M10_cpp(counts)

  expect_named(result, c("L5_value", "L5_onset", "L5_onset_hours", "M10_value",
                         "M10_onset", "M10_onset_hours", "RA"))
  expect_equal(c(result$L5_value, result$L5_onset_hours), c(20, 2))
  expect_equal(c(result$M10_value, result$M10_onset_hours), c(900, 10))
  expect_equal(result$RA, (900 - 20) / (900 + 20))
})

test_that("sleep scoring with Cole-Kripke works", {
  counts <- create.sleep.pattern(n = 480)

  # Use the R function which may use C++ backend
  result <- sleep.cole.kripke(counts)

  expect_true(is.character(result))
  expect_true(all(result %in% c("S", "W")))
})

test_that("wear time detection with Choi works", {
  counts <- create.nonwear.pattern(n = 1440, nonwear.length = 90)

  result <- wear.choi(counts)

  expect_true(is.logical(result))
  expect_equal(length(result), length(counts))
})

test_that("rolling_mean works correctly", {
  x <- 1:100
  window <- 5

  result <- rolling_mean(x, window)

  # one value per full window: the means of 1:5, 2:6, ..., 96:100
  expect_equal(result, as.numeric(3:98))
})

test_that("rolling_sum works correctly", {
  x <- 1:100
  window <- 5

  result <- rolling_sum(x, window)

  expect_equal(result, 5 * as.numeric(3:98))
})

test_that("rolling_sd works correctly", {
  x <- 1:100
  window <- 5

  result <- rolling_sd(x, window)

  expect_equal(result, rep(sd(1:5), 96))
})

test_that("a missing value reaches only the rolling windows that contain it", {
  x <- c(1, 2, NA, 4, 5, 6, 7)
  expect_equal(rolling_mean(x, 2), c(1.5, NA, NA, 4.5, 5.5, 6.5))
  expect_equal(rolling_sum(x, 3), c(NA, NA, NA, 15, 18))
  expect_equal(rolling_sum(c(1, Inf, 2, 3), 2), c(Inf, Inf, 5))
  expect_equal(rolling_sd(1:5, 1), rep(NA_real_, 5))
  expect_error(rolling_mean(1:5, 0), "window must be at least 1")
  expect_error(rolling_sum(1:5, -1), "window must be at least 1")
  expect_error(rolling_sd(1:5, 0), "window must be at least 1")
})

test_that("C++ functions handle small inputs", {
  # Small but valid input
  small_counts <- rep(100, 24)

  # IS needs two days and L5/M10 a full day of minutes
  expect_true(is.na(IS_cpp(small_counts)))
  expect_true(all(is.na(unlist(L5M10_cpp(small_counts)))))
})

test_that("circadian.rhythm gives the same L5, M10 and IV with and without C++", {
  ts <- as.POSIXct("2024-01-06 14:30:00", tz = "UTC") + (0:(4 * 1440 - 1)) * 60
  hour <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60
  set.seed(1)
  counts <- pmax(0, 100 + 80 * cos(2 * pi * (hour - 14) / 24) + rnorm(length(ts), 0, 20))

  with_cpp <- circadian.rhythm(counts, ts, epoch_length = 60, use_cpp = TRUE)
  without_cpp <- circadian.rhythm(counts, ts, epoch_length = 60, use_cpp = FALSE)

  expect_s3_class(with_cpp, "canhrActi_circadian")
  keep <- c("L5", "L5_start", "M10", "M10_start", "RA", "IV")
  expect_equal(with_cpp[keep], without_cpp[keep])
})

test_that("sedentary.fragmentation uses C++ when available", {
  test_data <- create.test.counts.data(n = 1440)
  intensity_levels <- freedson(test_data$axis1)
  wear <- rep(TRUE, 1440)

  result <- sedentary.fragmentation(
    intensity_levels,
    wear,
    timestamps = test_data$timestamp
  )

  expect_s3_class(result, "canhrActi_fragmentation")
})
