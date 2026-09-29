# Tests for batch_sleep.R

test_that("canhrActi.sleep validates input parameters", {
  # Should error with non-existent path
  expect_error(canhrActi.sleep("/nonexistent/path"), "File or folder not found: /nonexistent/path")

  # Should error with NULL input
  expect_error(canhrActi.sleep(NULL), "agd_file_path must be an .agd file")
})

test_that("canhrActi.sleep handles empty directory", {
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_sleep_test")
  dir.create(empty_dir, showWarnings = FALSE)

  expect_error(canhrActi.sleep(empty_dir), "No .agd files found in")

  unlink(empty_dir, recursive = TRUE)
})

test_that("a file that fails stays in the batch next to one that succeeds", {
  bad <- withr::local_tempfile(fileext = ".agd")
  file.create(bad)
  good <- example_agd(1)
  invisible(capture.output(res <- canhrActi.sleep(c(bad, good), export = FALSE, verbose = FALSE)))

  expect_s3_class(res, "canhrActi_sleep_batch")
  expect_equal(c(res$n_files, res$n_success, res$n_failed), c(2, 1, 1))
  expect_identical(res$failed_files, basename(bad))
  expect_identical(names(res$results), basename(good))

  expect_identical(res$summary$file_name, basename(c(bad, good)))
  failed <- res$summary[res$summary$file_name == basename(bad), ]
  expect_true(all(is.na(failed[, c("total_epochs", "sleep_periods_detected", "total_sleep_time_min")])))
  ok <- res$summary[res$summary$file_name == basename(good), ]
  expect_equal(c(ok$total_epochs, ok$sleep_periods_detected, ok$total_sleep_time_min), c(9919, 4, 1470))

  expect_identical(names(res$errors), basename(bad))
  expect_match(res$errors[[1]], "Could not find 'data' or 'epochs' table")
})

test_that("a missing file in a list is recorded as failed and the rest still run", {
  missing <- file.path(tempdir(), "no_such_recording.agd")
  good <- example_agd(1)
  invisible(capture.output(res <- canhrActi.sleep(c(good, missing), export = FALSE, verbose = FALSE)))

  expect_equal(c(res$n_files, res$n_success, res$n_failed), c(2, 1, 1))
  expect_identical(res$failed_files, basename(missing))
  expect_match(res$errors[[basename(missing)]], "File not found")
  expect_false(file.exists(missing))
})

test_that("a batch where every file fails returns n_failed", {
  bad <- withr::local_tempfile(fileext = ".agd")
  file.create(bad)

  expect_output(res <- canhrActi.sleep(bad, export = FALSE), "Failed: 1 files")
  expect_equal(c(res$n_success, res$n_failed), c(0, 1))
  expect_identical(res$failed_files, basename(bad))
  expect_equal(nrow(res$summary), 1)
  expect_output(print(res), "Files processed: 0/1")
})

test_that("canhrActi.sleep prints nothing with verbose = FALSE", {
  f <- example_agd(1)
  expect_silent(quiet <- canhrActi.sleep(f, export = FALSE, verbose = FALSE))
  invisible(capture.output(loud <- canhrActi.sleep(f, export = FALSE)))
  same <- setdiff(names(quiet), "processing_time")
  expect_identical(quiet[same], loud[same])
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
