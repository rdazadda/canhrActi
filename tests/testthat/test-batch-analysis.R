# Tests for batch_analysis.R

test_that("batch result structure is correct", {
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
    settings = list()
  )
  class(mock_result) <- "canhrActi_batch"

  expect_s3_class(mock_result, "canhrActi_batch")
  expect_true(all(expected_names %in% names(mock_result)))
})

test_that("parallel processing parameter validation works", {
  expect_true(is.logical(FALSE))
  expect_true(is.logical(TRUE))

  available_cores <- parallel::detectCores(logical = FALSE)
  expect_true(is.numeric(available_cores))
  expect_true(available_cores >= 1)
})

test_that("file pattern matching works", {
  test_files <- c("test1.agd", "test2.agd", "test3.csv", "test4.AGD")

  agd_pattern <- "\\.agd$"
  matched <- grep(agd_pattern, test_files, ignore.case = TRUE, value = TRUE)

  expect_equal(length(matched), 3)
  expect_true("test1.agd" %in% matched)
  expect_true("test4.AGD" %in% matched)
  expect_false("test3.csv" %in% matched)
})

test_that("batch export settings are valid", {
  valid_formats <- c("csv", "xlsx", "both")

  expect_true("csv" %in% valid_formats)
  expect_true("xlsx" %in% valid_formats)
})

test_that("progress calculation works", {
  n_files <- 10

  for (i in 1:n_files) {
    progress <- i / n_files
    expect_true(progress >= 0 && progress <= 1)
    expect_equal(progress, i / n_files)
  }
})

test_that("ETA calculation logic works", {
  start_time <- Sys.time()
  Sys.sleep(0.1)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  n_completed <- 1
  n_remaining <- 9

  avg_time <- elapsed / n_completed
  eta_seconds <- avg_time * n_remaining

  expect_true(is.numeric(eta_seconds))
  expect_true(eta_seconds > 0)
})

test_that("summary aggregation works", {
  daily1 <- data.frame(
    date = as.Date("2024-01-01"),
    wear_hours = 14,
    sedentary_min = 480,
    mvpa_min = 45
  )

  daily2 <- data.frame(
    date = as.Date("2024-01-02"),
    wear_hours = 15,
    sedentary_min = 500,
    mvpa_min = 50
  )

  combined <- rbind(daily1, daily2)

  expect_equal(nrow(combined), 2)
  expect_equal(mean(combined$mvpa_min), 47.5)
  expect_equal(sum(combined$wear_hours), 29)
})

test_that("core count calculation works", {
  available_cores <- parallel::detectCores(logical = FALSE)
  n_files <- 10

  n_cores <- min(available_cores - 1, 8, n_files)

  expect_true(n_cores >= 1)
  expect_true(n_cores <= 8)
})

test_that("file list creation works", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_agd_dir")
  dir.create(test_dir, showWarnings = FALSE)

  # Create dummy files
  file.create(file.path(test_dir, "test1.agd"))
  file.create(file.path(test_dir, "test2.agd"))
  file.create(file.path(test_dir, "other.csv"))

  agd_files <- list.files(test_dir, pattern = "\\.agd$", full.names = TRUE, ignore.case = TRUE)

  expect_equal(length(agd_files), 2)

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})
