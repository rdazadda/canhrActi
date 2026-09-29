# Tests for batch_analysis.R

test_that("canhrActi.batch analyses the two example files", {
  files <- c(example_agd(1), example_agd(2))
  invisible(capture.output(res <- canhrActi.batch(files, verbose = FALSE, export = FALSE)))

  expect_s3_class(res, "canhrActi_batch")
  expect_equal(res$n_participants, 2)
  expect_equal(res$n_failed, 0)
  expect_identical(res$failed_files, character(0))
  expect_identical(names(res$participants), c("A002", "A001"))
  expect_equal(dim(res$summary), c(2, 44))
  expect_identical(res$summary$Filename, basename(files))
  expect_equal(res$summary$`Number of Epochs`, c(4026, 10482))
  expect_equal(res$summary$Sedentary, c(2105, 5717))
  expect_equal(res$summary$`Total MVPA`, c(611, 1085))
  expect_equal(res$summary$`Calendar Days`, c(3, 8))
  expect_equal(res$group_stats$mean_mvpa_minutes, 848)
})

test_that("canhrActi.batch labels its group statistics as means of each file's totals", {
  files <- c(example_agd(1), example_agd(2))
  shown <- capture.output(res <- canhrActi.batch(files, export = FALSE,
                                                 calculate_fragmentation = FALSE,
                                                 calculate_circadian = FALSE))
  shown <- c(shown, capture.output(print(res)))

  expect_false(any(grepl("/day", shown, fixed = TRUE)))
  expect_identical(sum(shown == "  Mean Total MVPA: 848 min"), 2L)
})

test_that("the batch summary scales steps and counts to a minute for 30 s epochs", {
  ts <- seq(as.POSIXct("2024-03-04 00:00:00", tz = "UTC"), by = 30, length.out = 2880)
  worn <- format(ts, "%H") >= "08" & format(ts, "%H") < "22"
  counts <- ifelse(worn, rep(c(20, 300), 1440), 0)
  f <- file.path(withr::local_tempdir(), "thirty.agd")
  write.agd(data.frame(timestamp = ts, axis1 = counts, axis2 = counts, axis3 = counts,
                       steps = ifelse(worn, 3, 0)), f, epoch_length = 30)

  res <- canhrActi.batch(f, verbose = FALSE, export = FALSE, calculate_mets = FALSE,
                         calculate_fragmentation = FALSE, calculate_circadian = FALSE)
  s <- res$summary
  # 14 h worn, 3 steps and 160 counts in each worn 30 s epoch on average
  expect_equal(c(s$Epoch, s$`Number of Epochs`, s$`Steps Average Counts`, s$`Axis 1 Average Counts`),
               c(30, 1680, 3, 160))
  expect_equal(c(s$`Steps Per Minute`, s$`Axis 1 CPM`), c(6, 320))
  # the daily summary gives counts per minute as well, not per epoch
  expect_equal(res$participants[[1]]$daily_summary$average_cpm, 320)
})

test_that("two files with the same subject name both stay in the batch", {
  ts <- seq(as.POSIXct("2024-03-04 00:00:00", tz = "UTC"), by = 60, length.out = 1440)
  f <- file.path(withr::local_tempdir(), c("first.agd", "second.agd"))
  for (i in 1:2) {
    write.agd(data.frame(timestamp = ts, axis1 = 100 * i, axis2 = 0, axis3 = 0), f[i], subject_name = "P01")
  }
  res <- canhrActi.batch(f, verbose = FALSE, export = FALSE, calculate_mets = FALSE,
                         calculate_fragmentation = FALSE, calculate_circadian = FALSE)
  expect_identical(names(res$participants), c("P01", "P01.1"))
  expect_equal(res$n_participants, 2)
  expect_equal(c(res$participants$P01$epoch_data$counts_used[1], res$participants$P01.1$epoch_data$counts_used[1]),
               c(100, 200))
})

test_that("canhrActi.batch prints nothing with verbose = FALSE", {
  files <- c(example_agd(1), example_agd(2))
  # the same seed for both runs: the fragmentation confidence interval is bootstrapped
  expect_silent(quiet <- withr::with_seed(1, canhrActi.batch(files, verbose = FALSE, export = FALSE)))
  invisible(capture.output(loud <- withr::with_seed(1, canhrActi.batch(files, export = FALSE))))
  same <- setdiff(names(quiet), "processing_time")
  expect_identical(quiet[same], loud[same])
})

test_that("canhrActi.batch keeps the error of a file that fails", {
  bad <- withr::local_tempfile(fileext = ".agd")
  file.create(bad)
  invisible(capture.output(res <- canhrActi.batch(c(bad, example_agd(1)),
                                                  verbose = FALSE, export = FALSE)))

  expect_equal(res$n_participants, 1)
  expect_identical(res$failed_files, basename(bad))
  expect_identical(res$summary$Filename, basename(example_agd(1)))
  expect_identical(names(res$errors), basename(bad))
  expect_match(res$errors[[1]], "Could not find 'data' or 'epochs' table")
})

test_that("canhrActi.batch takes every intensity choice of canhrActi() and matches the single-file run", {
  files <- c(example_agd(1), example_agd(2))
  choices <- eval(formals(canhrActi)$intensity_algorithm)
  expect_identical(eval(formals(canhrActi.batch)$intensity_algorithm), choices)
  cols <- c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous",
            "% in Sedentary", "% in Light", "% in Moderate", "% in Vigorous",
            "% in Very Vigorous", "Total MVPA", "% in MVPA", "Average MVPA Per day")

  # METs, fragmentation and circadian do not touch intensity, so both runs skip them
  for (x in choices) {
    invisible(capture.output(res <- canhrActi(files, intensity_algorithm = x,
                                              calculate_mets = FALSE,
                                              calculate_fragmentation = FALSE,
                                              calculate_circadian = FALSE)))
    expect_equal(res$n_failed, 0)
    expect_identical(res$settings$intensity_algorithm, x)

    for (f in files) {
      invisible(capture.output(single <- canhrActi(f, intensity_algorithm = x,
                                                   output_summary = FALSE,
                                                   calculate_mets = FALSE,
                                                   calculate_fragmentation = FALSE,
                                                   calculate_circadian = FALSE)))
      row <- res$summary[res$summary$Filename == basename(f), ]
      expected <- .build_summary_row(f, row$Subject, single, x, "axis1")
      expect_identical(as.list(row[cols]), as.list(expected[cols]))

      in_batch <- res$participants[[row$Subject]]
      expect_identical(in_batch$parameters$intensity_algorithm,
                       single$parameters$intensity_algorithm)
      expect_identical(in_batch$epoch_data$intensity, single$epoch_data$intensity)
      expect_identical(in_batch$intensity_summary, single$intensity_summary)
      expect_identical(in_batch$overall_summary, single$overall_summary)
    }
  }
})

test_that("a missing file fails as not found and nothing is created at its path", {
  missing <- withr::local_tempfile(fileext = ".agd")
  files <- c(example_agd(1), missing)

  invisible(capture.output(res <- canhrActi.batch(files, verbose = FALSE, export = FALSE)))
  expect_equal(res$n_participants, 1)
  expect_identical(res$failed_files, basename(missing))
  expect_identical(res$summary$Filename, basename(example_agd(1)))
  expect_match(res$errors[[basename(missing)]], "File not found")
  expect_false(file.exists(missing))

  invisible(capture.output(res <- canhrActi(files)))
  expect_identical(res$failed_files, basename(missing))
  expect_match(res$errors[[basename(missing)]], "File not found")
  expect_false(file.exists(missing))
})
