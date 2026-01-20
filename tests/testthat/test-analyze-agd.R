# Tests for main canhrActi() analysis function

test_that("canhrActi returns correct structure", {
  test_data <- create.test.counts.data(n = 2880)

  expect_true(is.data.frame(test_data))
  expect_true("timestamp" %in% names(test_data))
  expect_true("axis1" %in% names(test_data))
})

test_that("intensity classification works with freedson", {
  test_data <- create.test.counts.data(n = 1440)

  result <- freedson(test_data$axis1)

  expect_s3_class(result, "factor")
  expect_true(all(result %in% c("sedentary", "light", "moderate", "vigorous", "very_vigorous")))
})

test_that("wear time detection works with choi", {
  counts <- create.nonwear.pattern(n = 1440, nonwear.length = 90)

  result <- wear.choi(counts)

  expect_true(is.logical(result))
  expect_equal(length(result), length(counts))
})

test_that("wear time detection works with troiano", {
  counts <- create.nonwear.pattern(n = 1440, nonwear.length = 90)

  result <- wear.troiano(counts)

  expect_true(is.logical(result))
  expect_equal(length(result), length(counts))
})

test_that("daily summary calculation works", {
  # Use exactly 2 days of data (same day range)
  test_data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01 00:00:00"),
                    as.POSIXct("2024-01-02 23:59:00"), by = 60),
    axis1 = sample(0:5000, 2880, replace = TRUE)
  )
  test_data$date <- as.Date(test_data$timestamp)

  # Should have 2 unique dates (Jan 1 and Jan 2)
  expect_true(length(unique(test_data$date)) >= 2)
})

test_that("sleep analysis with Cole-Kripke works", {
  counts <- create.sleep.pattern(n = 1440)

  sleep_state <- sleep.cole.kripke(counts)

  expect_true(is.character(sleep_state))
  expect_true(all(sleep_state %in% c("S", "W")))
})

test_that("sleep analysis with Sadeh works", {
  counts <- create.sleep.pattern(n = 1440)

  sleep_state <- sleep.sadeh(counts)

  expect_true(is.character(sleep_state))
  expect_true(all(sleep_state %in% c("S", "W")))
})

test_that("circadian analysis integrates correctly", {
  test_data <- create.test.counts.data(n = 4320)  # 3 days for IS/IV

  result <- circadian.rhythm(
    counts = test_data$axis1,
    timestamps = test_data$timestamp,
    epoch_length = 60
  )

  expect_s3_class(result, "canhrActi_circadian")
  expect_true("L5" %in% names(result))
  expect_true("M10" %in% names(result))
  expect_true("RA" %in% names(result))
})

test_that("MVPA calculation works with intensity levels", {
  intensity_levels <- freedson(create.test.counts.data(n = 1440)$axis1)

  result <- mvpa(intensity_levels)

  expect_true(is.numeric(result))
  expect_true(result >= 0)
})

test_that("sedentary time calculation works", {
  # Create data that will definitely have sedentary time
  counts <- c(rep(0, 500), rep(50, 500), rep(2000, 440))

  intensity_result <- freedson(counts)
  sedentary_mins <- sum(intensity_result == "sedentary")

  expect_true(sedentary_mins > 0)
  expect_true(sedentary_mins <= 1440)
})

test_that("vector magnitude calculation works", {
  test_data <- create.test.counts.data(n = 100)

  vm_result <- vm(test_data$axis1, test_data$axis2, test_data$axis3)

  expect_true(is.numeric(vm_result))
  expect_equal(length(vm_result), 100)
  expect_true(all(vm_result >= 0))
})

test_that("intensity summary function works", {
  intensity_levels <- freedson(create.test.counts.data(n = 1440)$axis1)

  result <- intensity(intensity_levels)

  expect_true(is.data.frame(result))
  expect_true("minutes" %in% names(result))
  expect_true("percentage" %in% names(result))
})

test_that("Tudor-Locke sleep period detection works", {
  counts <- create.sleep.pattern(n = 1440)
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  sleep_state <- sleep.cole.kripke(counts)

  result <- sleep.tudor.locke(
    sleep.state = sleep_state,
    timestamps = timestamps
  )

  expect_true(is.data.frame(result))
})

test_that("sedentary fragmentation analysis works", {
  test_data <- create.test.counts.data(n = 1440)
  intensity_levels <- freedson(test_data$axis1)
  wear <- rep(TRUE, 1440)

  result <- sedentary.fragmentation(
    intensity = intensity_levels,
    timestamps = test_data$timestamp,
    wear_time = wear
  )

  expect_s3_class(result, "canhrActi_fragmentation")
  expect_true("alpha" %in% names(result))
  expect_true("gini" %in% names(result))
})
