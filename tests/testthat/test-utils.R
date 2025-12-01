test_that("valid.days identifies valid days correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 2880)
  wear.time <- c(rep(TRUE, 1440), rep(FALSE, 1440))

  result <- valid.days(timestamps, wear.time, min.wear.hours = 10)

  expect_equal(result$n_valid_days, 1)
  expect_equal(length(result$valid_days), 1)
})

test_that("valid.days calculates wear hours correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  wear.time <- rep(FALSE, 1440)
  wear.time[1:600] <- TRUE

  result <- valid.days(timestamps, wear.time, min.wear.hours = 10)

  expect_true(nrow(result$daily_summary) > 0)
  expect_true("wear.hours" %in% names(result$daily_summary))
})

test_that("valid.days rejects days below minimum", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  wear.time <- rep(TRUE, 1440)
  wear.time[1:900] <- FALSE

  result <- valid.days(timestamps, wear.time, min.wear.hours = 10)

  expect_equal(result$n_valid_days, 0)
  expect_true(all(!result$daily_summary$is.valid))
})

test_that("valid.days handles multiple days", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 4320)
  wear.time <- c(rep(TRUE, 1440), rep(TRUE, 1440), rep(FALSE, 1440))

  result <- valid.days(timestamps, wear.time, min.wear.hours = 10)

  expect_equal(result$n_valid_days, 2)
  expect_true(nrow(result$daily_summary) >= 3)
})

test_that("valid.days creates valid day index correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 2880)
  wear.time <- c(rep(TRUE, 1440), rep(FALSE, 1440))

  result <- valid.days(timestamps, wear.time, min.wear.hours = 10)

  expect_equal(length(result$valid_day_index), 2880)
  expect_true(sum(result$valid_day_index) >= 600)
})

test_that("valid.days validates input length", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 100)
  wear.time <- rep(TRUE, 50)

  expect_error(valid.days(timestamps, wear.time), "same length")
})

test_that("valid.days validates timestamp class", {
  timestamps <- 1:100
  wear.time <- rep(TRUE, 100)

  expect_error(valid.days(timestamps, wear.time), "POSIXct")
})

test_that("valid.days handles empty input", {
  timestamps <- as.POSIXct(character(0))
  wear.time <- logical(0)

  result <- valid.days(timestamps, wear.time)
  expect_equal(result$n_valid_days, 0)
})

test_that("valid.days returns correct class", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  wear.time <- rep(TRUE, 1440)

  result <- valid.days(timestamps, wear.time)

  expect_s3_class(result, "canhrActi_valid_days")
  expect_s3_class(result, "list")
})

test_that("sample.rate calculates frequency correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 1/60, length.out = 100)
  result <- sample.rate(timestamps)
  expect_equal(result, 60)
})

test_that("sample.rate handles different frequencies", {
  timestamps30 <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 1/30, length.out = 100)
  timestamps80 <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 1/80, length.out = 100)

  result30 <- sample.rate(timestamps30)
  result80 <- sample.rate(timestamps80)

  expect_equal(result30, 30)
  expect_equal(result80, 80)
})

test_that("sample.rate validates input length", {
  timestamps <- as.POSIXct("2024-01-01 00:00:00")
  expect_error(sample.rate(timestamps), "at least 2")
})


test_that("quality calculates metrics correctly", {
  accel.data <- data.frame(
    x = c(rep(0.5, 90), rep(NA, 10)),
    y = c(rep(1.0, 90), rep(NA, 10)),
    z = c(rep(0.2, 90), rep(NA, 10))
  )

  result <- quality(accel.data)

  expect_equal(result$n.samples, 100)
  expect_equal(result$missing.x, 10)
  expect_equal(result$missing.y, 10)
  expect_equal(result$missing.z, 10)
  expect_equal(result$percent.missing, 10)
})

test_that("quality detects outliers", {
  accel.data <- data.frame(
    x = c(rep(0.5, 90), rep(15, 10)),
    y = rep(1.0, 100),
    z = rep(0.2, 100)
  )

  result <- quality(accel.data)

  expect_equal(result$outliers.x, 10)
  expect_equal(result$outliers.y, 0)
  expect_equal(result$outliers.z, 0)
})

test_that("quality calculates mean magnitude", {
  accel.data <- data.frame(
    x = rep(0.5, 100),
    y = rep(1.0, 100),
    z = rep(0.2, 100)
  )

  result <- quality(accel.data)

  expected.magnitude <- sqrt(0.5^2 + 1.0^2 + 0.2^2)
  expect_equal(result$mean.magnitude, expected.magnitude, tolerance = 0.01)
})

test_that("quality validates required columns", {
  accel.data <- data.frame(x = rnorm(100), y = rnorm(100))

  expect_error(quality(accel.data), "x, y, z")
})

test_that("synthetic generates correct structure", {
  result <- synthetic(duration.hours = 1, sampling.freq = 60, add.activity = FALSE)

  expect_true("timestamp" %in% names(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("z" %in% names(result))
  expect_equal(nrow(result), 1 * 3600 * 60)
})

test_that("synthetic adds activity bouts when requested", {
  result.no.activity <- synthetic(duration.hours = 1, sampling.freq = 60, add.activity = FALSE)
  result.with.activity <- synthetic(duration.hours = 1, sampling.freq = 60, add.activity = TRUE, n.bouts = 5)

  sd.no.activity <- sd(result.no.activity$y)
  sd.with.activity <- sd(result.with.activity$y)

  expect_true(sd.with.activity > sd.no.activity)
})

test_that("print.canhrActi_valid_days displays summary", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  wear.time <- rep(TRUE, 1440)
  result <- valid.days(timestamps, wear.time, min.wear.hours = 10)

  expect_output(print(result), "Valid Day Detection")
  expect_output(print(result), "10 hours")
})
