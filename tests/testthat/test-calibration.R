test_that("auto.calibrate validates required columns", {
  accel_data <- data.frame(x = rnorm(100), y = rnorm(100))
  expect_error(auto.calibrate(accel_data, sampling_freq = 30, verbose = FALSE), "z")
})

test_that("auto.calibrate handles empty data", {
  accel_data <- data.frame(x = numeric(0), y = numeric(0), z = numeric(0))
  expect_error(auto.calibrate(accel_data, sampling_freq = 30, verbose = FALSE), "empty")
})

test_that("auto.calibrate handles data with non-movement periods", {
  set.seed(123)
  # Create data with clear non-movement periods (low SD)
  n_samples <- 3000
  accel_data <- data.frame(
    x = c(rep(0, 500), rnorm(500, 0, 0.5), rep(0.01, 500),
          rnorm(500, 0, 0.5), rep(-0.01, 500), rnorm(500, 0, 0.5)),
    y = c(rep(1, 500), rnorm(500, 1, 0.5), rep(0.99, 500),
          rnorm(500, 1, 0.5), rep(1.01, 500), rnorm(500, 1, 0.5)),
    z = c(rep(0, 500), rnorm(500, 0, 0.5), rep(0.02, 500),
          rnorm(500, 0, 0.5), rep(-0.02, 500), rnorm(500, 0, 0.5))
  )

  result <- suppressWarnings(suppressMessages(
    auto.calibrate(accel_data, sampling_freq = 30, min_samples = 10, verbose = FALSE)
  ))

  expect_true("calibrated_data" %in% names(result))
  expect_true("offset" %in% names(result))
  expect_true("scale" %in% names(result))
})

test_that("auto.calibrate returns uncalibrated data when insufficient non-movement", {
  set.seed(123)
  # High noise data with no non-movement periods
  accel_data <- data.frame(
    x = rnorm(1000, 0, 0.5),
    y = rnorm(1000, 1, 0.5),
    z = rnorm(1000, 0, 0.5)
  )

  expect_warning(
    result <- suppressMessages(auto.calibrate(accel_data, sampling_freq = 30, verbose = FALSE)),
    "Insufficient"
  )

  expect_equal(nrow(result$calibrated_data), nrow(accel_data))
})

test_that("auto.calibrate preserves data dimensions", {
  set.seed(123)
  n <- 1000
  accel_data <- data.frame(
    x = rnorm(n, 0, 0.1),
    y = rnorm(n, 1, 0.1),
    z = rnorm(n, 0, 0.1)
  )

  result <- suppressWarnings(suppressMessages(
    auto.calibrate(accel_data, sampling_freq = 30, verbose = FALSE)
  ))

  expect_equal(nrow(result$calibrated_data), n)
  expect_true(all(c("x", "y", "z") %in% names(result$calibrated_data)))
})

test_that("auto.calibrate returns correct structure elements", {
  set.seed(123)
  accel_data <- data.frame(
    x = c(rep(0, 400), rnorm(600, 0, 0.3)),
    y = c(rep(1, 400), rnorm(600, 1, 0.3)),
    z = c(rep(0, 400), rnorm(600, 0, 0.3))
  )

  result <- suppressWarnings(suppressMessages(
    auto.calibrate(accel_data, sampling_freq = 30, min_samples = 5, verbose = FALSE)
  ))

  expect_true("calibrated_data" %in% names(result))
  expect_true("offset" %in% names(result))
  expect_true("scale" %in% names(result))
  expect_true("n_nonmovement" %in% names(result))
  expect_true("error_before" %in% names(result))
  expect_true("error_after" %in% names(result))
  expect_true("calibration_success" %in% names(result))
})
