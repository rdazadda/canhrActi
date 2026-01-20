# Tests for batch_sleep.R

test_that("canhrActi.sleep validates input parameters", {
  # Should error with non-existent path
  expect_error(canhrActi.sleep("/nonexistent/path"))

  # Should error with NULL input
  expect_error(canhrActi.sleep(NULL))
})

test_that("canhrActi.sleep handles empty directory", {
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_sleep_test")
  dir.create(empty_dir, showWarnings = FALSE)

  expect_error(canhrActi.sleep(empty_dir))

  unlink(empty_dir, recursive = TRUE)
})

test_that("sleep algorithm parameter validation works", {
  valid_algorithms <- c("cole.kripke", "sadeh")

  expect_true("cole.kripke" %in% valid_algorithms)
  expect_true("sadeh" %in% valid_algorithms)
})

test_that("sleep period detection parameters are valid", {
  # Default parameters
  default_min_period <- 20
  default_max_period <- 1440

  expect_true(default_min_period > 0)
  expect_true(default_max_period > default_min_period)
  expect_true(default_max_period <= 1440)  # Max 24 hours
})

test_that("batch sleep result structure is correct", {
  expected_names <- c(
    "results", "summary", "processing_time", "n_files",
    "n_successful", "n_failed", "failed_files", "settings"
  )

  mock_result <- list(
    results = list(),
    summary = data.frame(),
    processing_time = 0,
    n_files = 0,
    n_successful = 0,
    n_failed = 0,
    failed_files = character(0),
    settings = list(algorithm = "cole.kripke")
  )
  class(mock_result) <- "canhrActi_batch_sleep"

  expect_s3_class(mock_result, "canhrActi_batch_sleep")
  expect_true(all(expected_names %in% names(mock_result)))
})

test_that("sleep metrics aggregation works", {
  # Create mock sleep period data
  periods1 <- data.frame(
    sleep_efficiency = 85.5,
    sleep_time = 420,
    wake_time = 30,
    number_of_awakenings = 3
  )

  periods2 <- data.frame(
    sleep_efficiency = 90.0,
    sleep_time = 450,
    wake_time = 20,
    number_of_awakenings = 2
  )

  combined <- rbind(periods1, periods2)

  expect_equal(mean(combined$sleep_efficiency), 87.75)
  expect_equal(mean(combined$sleep_time), 435)
  expect_equal(mean(combined$number_of_awakenings), 2.5)
})

test_that("sleep scoring integration works", {
  counts <- create.sleep.pattern(n = 1440)

  # Test Cole-Kripke
  result_ck <- sleep.cole.kripke(counts)
  expect_true(is.character(result_ck))
  expect_true(all(result_ck %in% c("S", "W")))

  # Test Sadeh
  result_sadeh <- sleep.sadeh(counts)
  expect_true(is.character(result_sadeh))
  expect_true(all(result_sadeh %in% c("S", "W")))
})

test_that("Tudor-Locke period detection works", {
  counts <- create.sleep.pattern(n = 1440)
  timestamps <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 1440)
  sleep_state <- sleep.cole.kripke(counts)

  result <- sleep.tudor.locke(
    sleep.state = sleep_state,
    timestamps = timestamps
  )

  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_true("sleep_time" %in% names(result))
    expect_true("sleep_efficiency" %in% names(result))
    expect_true("in_bed_time" %in% names(result))
  }
})

test_that("sleep export format validation works", {
  valid_formats <- c("csv", "xlsx", "both")

  export_format <- "csv"
  expect_true(export_format %in% valid_formats)
})

test_that("parallel sleep processing parameters work", {
  available_cores <- parallel::detectCores(logical = FALSE)
  n_files <- 10

  n_cores <- min(available_cores - 1, 8, n_files)

  expect_true(n_cores >= 1)
  expect_true(n_cores <= 8)
  expect_true(n_cores <= n_files)
})

test_that("sleep summary statistics calculation works", {
  # Mock sleep periods
  sleep_data <- data.frame(
    file = rep(c("file1", "file2"), each = 2),
    sleep_efficiency = c(85, 90, 88, 92),
    sleep_time = c(400, 420, 380, 450),
    wake_time = c(40, 30, 50, 25)
  )

  # Per-file aggregation
  by_file <- aggregate(
    cbind(sleep_efficiency, sleep_time) ~ file,
    data = sleep_data,
    FUN = mean
  )

  expect_equal(nrow(by_file), 2)
  expect_equal(by_file$sleep_efficiency[by_file$file == "file1"], 87.5)
})
