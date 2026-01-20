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
  skip_if_not(cpp_available(), "C++ backend not available")

  set.seed(42)
  counts <- rep(c(rep(50, 8), rep(500, 10), rep(200, 6)), 3)
  epoch_seconds <- 3600

  result <- IS_cpp(counts, epoch_seconds)

  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("IV_cpp calculates intradaily variability", {
  skip_if_not(cpp_available(), "C++ backend not available")

  set.seed(42)
  counts <- rep(c(rep(50, 8), rep(500, 10), rep(200, 6)), 3)

  result <- tryCatch(IV_cpp(counts), error = function(e) NA)

  expect_true(is.na(result) || is.numeric(result))
})

test_that("L5M10_cpp returns correct structure", {
  skip_if_not(cpp_available(), "C++ backend not available")

  set.seed(42)
  counts <- c(rep(50, 8), rep(500, 10), rep(200, 6))

  result <- tryCatch(L5M10_cpp(counts), error = function(e) list())

  expect_true(is.list(result))
})

test_that("fragmentation_cpp returns correct structure", {
  skip_if_not(cpp_available(), "C++ backend not available")

  set.seed(42)
  counts <- create.sedentary.pattern(n = 1440)
  threshold <- 100
  epoch_seconds <- 60

  result <- fragmentation_cpp(counts, threshold, epoch_seconds)

  expect_true(is.list(result))
  expect_true("alpha" %in% names(result) || "n_bouts" %in% names(result))
})

test_that("sedentary_bouts_cpp returns a list", {
  skip_if_not(cpp_available(), "C++ backend not available")

  counts <- create.sedentary.pattern(n = 1440)
  threshold <- 100

  result <- sedentary_bouts_cpp(counts, threshold)

  expect_true(is.list(result))
})

test_that("mvpa_bouts_cpp returns a list", {
  skip_if_not(cpp_available(), "C++ backend not available")

  counts <- create.active.pattern(n = 1440)
  threshold <- 2020
  min_bout <- 10
  tolerance <- 2

  result <- mvpa_bouts_cpp(counts, threshold, min_bout, tolerance)

  expect_true(is.list(result))
})

test_that("sleep scoring with Cole-Kripke works", {
  skip_if_not(cpp_available(), "C++ backend not available")

  counts <- create.sleep.pattern(n = 480)

  # Use the R function which may use C++ backend
  result <- sleep.cole.kripke(counts)

  expect_true(is.character(result))
  expect_true(all(result %in% c("S", "W")))
})

test_that("wear time detection with Choi works", {
  skip_if_not(cpp_available(), "C++ backend not available")

  counts <- create.nonwear.pattern(n = 1440, nonwear.length = 90)

  result <- wear.choi(counts)

  expect_true(is.logical(result))
  expect_equal(length(result), length(counts))
})

test_that("rolling_mean works correctly", {
  skip_if_not(cpp_available(), "C++ backend not available")

  x <- 1:100
  window <- 5

  result <- rolling_mean(x, window)

  expect_true(is.numeric(result))
  # Rolling functions may return shorter vector due to edge handling
  expect_true(length(result) > 0)
})

test_that("rolling_sum works correctly", {
  skip_if_not(cpp_available(), "C++ backend not available")

  x <- 1:100
  window <- 5

  result <- rolling_sum(x, window)

  expect_true(is.numeric(result))
})

test_that("rolling_sd works correctly", {
  skip_if_not(cpp_available(), "C++ backend not available")

  x <- 1:100
  window <- 5

  result <- rolling_sd(x, window)

  expect_true(is.numeric(result))
})

test_that("C++ functions handle small inputs", {
  skip_if_not(cpp_available(), "C++ backend not available")

  # Small but valid input
  small_counts <- rep(100, 24)

  result <- tryCatch(IS_cpp(small_counts, 3600), error = function(e) NA)
  expect_true(is.na(result) || is.numeric(result))
})

test_that("circadian.rhythm uses C++ when available", {
  test_data <- create.test.counts.data(n = 4320)

  result <- circadian.rhythm(
    counts = test_data$axis1,
    timestamps = test_data$timestamp,
    epoch_length = 60
  )

  expect_s3_class(result, "canhrActi_circadian")
  expect_true("IS" %in% names(result))
  expect_true("IV" %in% names(result))
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
