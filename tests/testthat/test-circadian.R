test_that("circadian.rhythm calculates basic metrics", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_s3_class(result, "canhrActi_circadian")
  expect_true("L5" %in% names(result))
  expect_true("M10" %in% names(result))
  expect_true("RA" %in% names(result))
})

test_that("circadian.rhythm calculates L5 correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(10, 300), rep(500, 840), rep(10, 300))

  result <- circadian.rhythm(counts, timestamps)

  expect_true(result$L5 < result$M10)
  expect_true(result$L5 >= 0)
})

test_that("circadian.rhythm calculates M10 correctly", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(10, 300), rep(500, 840), rep(10, 300))

  result <- circadian.rhythm(counts, timestamps)

  expect_true(result$M10 > result$L5)
  expect_true(result$M10 >= 0)
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
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))
  wear_time <- rep(TRUE, 1440)
  wear_time[1:100] <- FALSE

  result <- circadian.rhythm(counts, timestamps, wear_time)

  expect_s3_class(result, "canhrActi_circadian")
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

test_that("circadian.rhythm performs cosinor analysis", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_true("cosinor" %in% names(result))
  expect_true("MESOR" %in% names(result$cosinor))
  expect_true("amplitude" %in% names(result$cosinor))
  expect_true("acrophase" %in% names(result$cosinor))
})

test_that("print method works for circadian results", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_output(print(result), "Circadian")
})

test_that("plot method works for circadian results", {
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  counts <- c(rep(50, 360), rep(500, 600), rep(50, 480))

  result <- circadian.rhythm(counts, timestamps)

  expect_silent(plot(result))
})
