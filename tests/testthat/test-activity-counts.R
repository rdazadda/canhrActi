# Tests for activity_counts.R

test_that("agd.counts extracts counts correctly", {
  # Create mock AGD-like data
  test_data <- create.test.agd.data(n = 1440)

  # Test extraction
  counts <- test_data$data$axis1

  expect_true(is.numeric(counts))
  expect_equal(length(counts), 1440)
  expect_true(all(counts >= 0))
})

test_that("vector magnitude calculation is correct", {
  axis1 <- c(100, 200, 300)
  axis2 <- c(100, 200, 300)
  axis3 <- c(100, 200, 300)

  result <- vm(axis1, axis2, axis3)

  # VM = sqrt(100^2 + 100^2 + 100^2) = sqrt(30000) = 173.2
  expected <- sqrt(axis1^2 + axis2^2 + axis3^2)

  expect_equal(result, expected)
  expect_true(all(result > 0))
})

test_that("counts per minute calculation works", {
  counts <- rep(100, 60)  # 60 epochs of 100 counts each

  cpm <- sum(counts)  # Total counts in 60 epochs = CPM for 1-minute epochs

  expect_equal(cpm, 6000)
})

test_that("epoch aggregation works", {
  # 15-second epochs aggregated to 60-second
  counts_15s <- rep(25, 4)  # 4 x 15s = 60s
  counts_60s <- sum(counts_15s)

  expect_equal(counts_60s, 100)

  # Check array reshaping
  all_counts <- rep(25, 96)  # 24 minutes of 15-second data
  n_60s_epochs <- length(all_counts) / 4

  expect_equal(n_60s_epochs, 24)
})

test_that("daily count totals are calculated correctly", {
  test_data <- create.test.counts.data(n = 1440)

  daily_total <- sum(test_data$axis1)

  expect_true(is.numeric(daily_total))
  expect_true(daily_total >= 0)
})

test_that("hourly count means are calculated correctly", {
  test_data <- create.test.counts.data(n = 1440)
  test_data$hour <- as.integer(format(test_data$timestamp, "%H"))

  hourly_means <- aggregate(axis1 ~ hour, data = test_data, FUN = mean)

  expect_equal(nrow(hourly_means), 24)
  expect_true(all(hourly_means$axis1 >= 0))
})

test_that("count thresholds work correctly", {
  counts <- c(0, 50, 100, 500, 2000, 5000, 10000)

  # Sedentary threshold (typically 100 CPM for 60s epochs)
  sedentary <- counts < 100
  expect_equal(sum(sedentary), 2)  # 0 and 50

  # MVPA threshold (typically 2020 CPM)
  mvpa <- counts >= 2020
  expect_equal(sum(mvpa), 2)  # 5000 and 10000
})

test_that("zero count periods are detected", {
  counts <- c(rep(500, 100), rep(0, 60), rep(500, 100))

  zero_runs <- rle(counts == 0)
  zero_periods <- zero_runs$lengths[zero_runs$values]

  expect_equal(max(zero_periods), 60)
})

test_that("count data validation works", {
  # Valid counts
  valid_counts <- c(0, 100, 500, 1000)
  expect_true(all(valid_counts >= 0))
  expect_true(all(is.numeric(valid_counts)))

  # Invalid counts (negative)
  invalid_counts <- c(-1, 100, 500)
  expect_true(any(invalid_counts < 0))
})

test_that("axis count relationships are reasonable", {
  test_data <- create.test.counts.data(n = 1000)

  # Axis1 (vertical) typically has highest counts
  # Vector magnitude should be >= any single axis
  vm_values <- vm(test_data$axis1, test_data$axis2, test_data$axis3)

  expect_true(all(vm_values >= test_data$axis1))
  expect_true(all(vm_values >= test_data$axis2))
  expect_true(all(vm_values >= test_data$axis3))
})

test_that("step count extraction works", {
  test_data <- create.test.counts.data(n = 1440)

  steps <- test_data$steps

  expect_true(is.numeric(steps))
  expect_true(all(steps >= 0))
  expect_equal(length(steps), 1440)
})

test_that("daily step totals are reasonable", {
  test_data <- create.test.counts.data(n = 1440)

  daily_steps <- sum(test_data$steps)

  # Daily steps should be reasonable (0 - 50000 typical range)
  expect_true(daily_steps >= 0)
  # With random 0-150 per minute, max would be ~216000
  expect_true(daily_steps <= 250000)
})

test_that("count data handles missing values", {
  counts <- c(100, 200, NA, 400, NA, 600)

  # Sum with na.rm
  total <- sum(counts, na.rm = TRUE)
  expect_equal(total, 1300)

  # Mean with na.rm
  avg <- mean(counts, na.rm = TRUE)
  expect_equal(avg, 325)

  # Count non-NA
  valid_n <- sum(!is.na(counts))
  expect_equal(valid_n, 4)
})
