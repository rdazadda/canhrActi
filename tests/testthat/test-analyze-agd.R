# Tests for main canhrActi() analysis function

test_that("canhrActi analyses one .agd file", {
  invisible(capture.output(res <- canhrActi(example_agd(1), output_summary = FALSE)))

  expect_s3_class(res, "canhrActi_analysis")
  expect_identical(names(res), c("epoch_data", "daily_summary", "overall_summary",
                                 "intensity_summary", "valid_days", "wear_time_periods",
                                 "subject_info", "mets_summary", "energy_expenditure_summary",
                                 "fragmentation", "circadian", "sleep", "parameters"))
  expect_equal(nrow(res$epoch_data), 9919)
  expect_identical(res$valid_days, c("2025-10-08", "2025-10-09", "2025-10-10"))

  s <- res$overall_summary
  # Choi with no stop level, as ActiLife runs it: 10/10 03:00-04:22 is non-wear
  expect_equal(c(s$total_days, s$valid_days, s$total_wear_minutes), c(8, 3, 4248))
  expect_equal(c(s$sedentary_minutes, s$light_minutes, s$moderate_minutes,
                 s$vigorous_minutes, s$very_vigorous_minutes, s$mvpa_minutes),
               c(2221, 1379, 609, 32, 7, 648))
  expect_identical(unlist(res$parameters[c("wear_time_algorithm", "intensity_algorithm", "axis_analyzed")]),
                   c(wear_time_algorithm = "choi", intensity_algorithm = "freedson1998",
                     axis_analyzed = "axis1"))
})

test_that("canhrActi on a folder or several files runs the batch", {
  f <- example_agd(1)
  invisible(capture.output(single <- canhrActi(f, output_summary = FALSE)))
  invisible(capture.output(res <- canhrActi(dirname(f))))

  expect_s3_class(res, "canhrActi_batch")
  expect_equal(res$n_participants, 2)
  expect_equal(res$n_failed, 0)
  expect_setequal(res$summary$Filename, example_agd("list"))
  same_file <- Filter(function(a) basename(a$parameters$file_path) == basename(f), res$participants)
  expect_length(same_file, 1)
  expect_identical(same_file[[1]]$overall_summary, single$overall_summary)

  invisible(capture.output(res2 <- canhrActi(c(f, example_agd(2)))))
  expect_identical(res2$summary, res$summary)
})

test_that("canhrActi prints nothing when output_summary = FALSE", {
  f <- example_agd(1)
  # the same seed for both runs: the fragmentation confidence interval is bootstrapped
  expect_silent(quiet <- withr::with_seed(1, canhrActi(f, output_summary = FALSE)))
  invisible(capture.output(loud <- withr::with_seed(1, canhrActi(f))))
  expect_identical(quiet, loud)
})

test_that("canhrActi on several files with output_summary = FALSE prints nothing and writes no file", {
  withr::local_dir(withr::local_tempdir())
  expect_silent(res <- canhrActi(c(example_agd(1), example_agd(2)), output_summary = FALSE,
                                 calculate_fragmentation = FALSE, calculate_circadian = FALSE))
  expect_s3_class(res, "canhrActi_batch")
  expect_identical(res$n_participants, 2L)
  expect_identical(list.files(all.files = TRUE, no.. = TRUE), character(0))
})

test_that("a recording never worn has no valid day, alone or in a batch", {
  f <- file.path(withr::local_tempdir(), "never.agd")
  ts <- seq(as.POSIXct("2024-03-04 00:00:00", tz = "UTC"), by = 60, length.out = 1440)
  write.agd(data.frame(timestamp = ts, axis1 = 0, axis2 = 0, axis3 = 0), f)

  res <- canhrActi(f, output_summary = FALSE)
  expect_identical(res$valid_days, character(0))
  expect_equal(c(res$overall_summary$valid_days, res$overall_summary$total_wear_minutes), c(0, 0))
  expect_identical(res$mets_summary$average_mets, NA_real_)
  expect_identical(nrow(res$energy_expenditure_summary), 0L)

  batch <- canhrActi.batch(f, verbose = FALSE, export = FALSE)
  expect_identical(batch$n_failed, 0L)
  expect_equal(batch$summary$`Calendar Days`, 0)
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

test_that("intensity summary function works", {
  intensity_levels <- freedson(create.test.counts.data(n = 1440)$axis1)

  result <- intensity(intensity_levels)

  expect_true(is.data.frame(result))
  expect_true("minutes" %in% names(result))
  expect_true("percentage" %in% names(result))
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
