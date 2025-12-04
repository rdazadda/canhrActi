# Tests for calibration functions (enmo and mad)
# Note: auto.calibrate tests are in test-calibration.R

test_that("enmo calculates correctly", {
  # When acceleration = 1g (at rest), ENMO should be 0
  x <- rep(0, 100)
  y <- rep(0, 100)
  z <- rep(1, 100)

  result <- enmo(x, y, z)
  expect_equal(result, rep(0, 100))
})

test_that("enmo handles movement", {
  # When VM > 1g, ENMO should be positive
  x <- rep(0.5, 100)
  y <- rep(0.5, 100)
  z <- rep(1, 100)

  result <- enmo(x, y, z)
  expected <- sqrt(0.5^2 + 0.5^2 + 1^2) - 1
  expect_equal(result[1], expected)
})

test_that("enmo truncates negative values by default", {
  # When VM < 1g
  x <- rep(0, 100)
  y <- rep(0, 100)
  z <- rep(0.5, 100)  # VM = 0.5 < 1

  result <- enmo(x, y, z)
  expect_true(all(result >= 0))
})

test_that("enmo can return negative values when truncate_negative = FALSE", {
  x <- rep(0, 100)
  y <- rep(0, 100)
  z <- rep(0.5, 100)

  result <- enmo(x, y, z, truncate_negative = FALSE)
  expect_true(all(result < 0))
})

test_that("enmo validates input lengths", {
  x <- rep(0, 100)
  y <- rep(0, 50)
  z <- rep(0, 100)

  expect_error(enmo(x, y, z), "same length")
})

test_that("mad calculates mean absolute deviation", {
  set.seed(123)
  x <- rnorm(600, mean = 0, sd = 0.5)
  y <- rnorm(600, mean = 0, sd = 0.5)
  z <- rnorm(600, mean = 1, sd = 0.5)

  result <- mad(x, y, z, epoch_samples = 60)

  expect_equal(length(result), 10)  # 600/60 = 10 epochs
  expect_true(all(result >= 0))
})

test_that("mad handles insufficient samples", {
  x <- rnorm(30)
  y <- rnorm(30)
  z <- rnorm(30)

  expect_warning(
    result <- mad(x, y, z, epoch_samples = 60),
    "Insufficient"
  )
  expect_equal(length(result), 0)
})

test_that("mad validates input lengths", {
  x <- rnorm(100)
  y <- rnorm(50)
  z <- rnorm(100)

  expect_error(mad(x, y, z), "same length")
})

test_that("auto.calibrate validates input", {
  # Missing required columns
  bad_data <- data.frame(a = rnorm(100), b = rnorm(100))
  expect_error(auto.calibrate(bad_data, sampling_freq = 30), "must contain columns")

  # Empty input
  empty_data <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
  expect_error(auto.calibrate(empty_data, sampling_freq = 30), "empty")
})

test_that("auto.calibrate handles insufficient non-movement", {
  # High noise data - no non-movement windows will be detected
  set.seed(123)
  n <- 1000
  accel_data <- data.frame(
    x = rnorm(n, mean = 0, sd = 0.5),
    y = rnorm(n, mean = 0, sd = 0.5),
    z = rnorm(n, mean = 1, sd = 0.5)
  )

  expect_warning(
    result <- auto.calibrate(accel_data, sampling_freq = 30, verbose = FALSE),
    "Insufficient"
  )

  # Should return uncalibrated data
  expect_false(result$calibration_success)
})
