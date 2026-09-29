# Tests for analyze.agd.file(), the alias of canhrActi() in R/analyze_agd.R, on the
# package's sample .agd files.

quietly <- function(expr) {
  utils::capture.output(value <- expr)
  value
}

# one default run of the first sample, shared by the tests that only read it
.memo <- new.env()
sample_run <- function() {
  if (is.null(.memo$run)) {
    .memo$run <- quietly(analyze.agd.file(example_agd(1), output_summary = FALSE,
                                          calculate_fragmentation = FALSE,
                                          calculate_circadian = FALSE))
  }
  .memo$run
}

test_that("analyze.agd.file() returns what canhrActi() returns for the same call", {
  args <- list(example_agd(2), output_summary = FALSE, calculate_mets = FALSE,
               calculate_fragmentation = FALSE, calculate_circadian = FALSE)
  a <- quietly(do.call(analyze.agd.file, args))
  expect_identical(a, quietly(do.call(canhrActi, args)))
  expect_identical(class(a), c("canhrActi_analysis", "list"))
})

test_that("analyze.agd.file() passes its arguments on", {
  r <- quietly(analyze.agd.file(example_agd(1), wear_time_algorithm = "troiano",
                                intensity_algorithm = "CANHR", min_wear_hours = 8,
                                output_summary = FALSE, calculate_mets = FALSE,
                                calculate_fragmentation = FALSE, calculate_circadian = FALSE))
  expect_identical(r$parameters[c("wear_time_algorithm", "intensity_algorithm", "min_wear_hours")],
                   list(wear_time_algorithm = "troiano", intensity_algorithm = "CANHR",
                        min_wear_hours = 8))
  cnt <- agd.counts(read.agd(example_agd(1), verbose = FALSE))
  wear <- wear.troiano(cnt$axis1)
  expect_identical(r$epoch_data$wear_time, wear)
  expect_identical(r$epoch_data$intensity, as.character(CANHR.Cutpoints(cnt$axis1)))
  expect_identical(r$valid_days, valid.days(cnt$timestamp, wear, min.wear.hours = 8)$valid_days)
})

test_that("analyze.agd.file() takes counts, wear, classes and totals from the file", {
  r <- sample_run()
  cnt <- agd.counts(read.agd(example_agd(1), verbose = FALSE))
  e <- r$epoch_data
  expect_identical(nrow(e), 9919L)
  expect_identical(e$timestamp, cnt$timestamp)
  expect_identical(e$axis1, cnt$axis1)
  expect_identical(e$wear_time, wear.choi(cnt$axis1))
  expect_identical(e$intensity, as.character(freedson(cnt$axis1)))
  # three days reach 10 h of wear; the first and last are part days
  expect_identical(r$valid_days, c("2025-10-08", "2025-10-09", "2025-10-10"))
  s <- r$overall_summary
  worn <- e$intensity[e$wear_time]
  expect_equal(s$total_wear_minutes, sum(e$wear_time))
  expect_equal(s$sedentary_minutes, sum(worn == "sedentary"))
  expect_equal(s$mvpa_minutes, sum(worn %in% c("moderate", "vigorous", "very_vigorous")))
  expect_identical(r$wear_time_periods, get.wear.periods(e$wear_time, e$timestamp))
  expect_identical(r$subject_info[c("subject_id", "sex", "age")],
                   list(subject_id = "A002", sex = "F", age = 37))
  # energy over the worn epochs, as the daily totals count it
  expect_equal(r$mets_summary$total_kcal, sum(e$kcal[e$wear_time]))
})

test_that("analyze.agd.file() averages METs and counts over each day's wear epochs", {
  r <- sample_run()
  e <- r$epoch_data[r$epoch_data$wear_time, ]
  d <- r$daily_summary
  d <- d[d$date %in% as.character(e$date), ]
  day <- as.character(e$date)
  expect_equal(d$average_mets, as.vector(tapply(e$mets, day, mean)[d$date]))
  expect_equal(d$average_cpm, as.vector(tapply(e$counts_used, day, mean)[d$date]))
})

test_that("analyze.agd.file() reports each day's intensity in minutes", {
  r <- sample_run()
  e <- r$epoch_data[r$epoch_data$wear_time, ]
  d <- r$daily_summary
  d <- d[d$date %in% as.character(e$date), ]
  per_day <- function(x) as.vector(tapply(x, as.character(e$date), sum)[d$date])
  expect_equal(d$sedentary_min, per_day(e$intensity == "sedentary"))
  expect_equal(d$light_min, per_day(e$intensity == "light"))
  expect_equal(d$moderate_min, per_day(e$intensity == "moderate"))
  expect_equal(d$mvpa_min, per_day(e$intensity %in% c("moderate", "vigorous", "very_vigorous")))
})

test_that("analyze.agd.file() reports each day's total energy expenditure", {
  r <- sample_run()
  e <- r$epoch_data[r$epoch_data$wear_time, ]
  d <- r$daily_summary
  d <- d[d$date %in% as.character(e$date), ]
  expect_equal(d$total_kcal, as.vector(tapply(e$kcal, as.character(e$date), sum)[d$date]))
})

test_that("analyze.agd.file() lists the days in date order", {
  d <- sample_run()$daily_summary
  expect_identical(d$date, sort(d$date, method = "radix"))
})

test_that("analyze.agd.file() stops on a missing file, a file that is not .agd, and a wear rule over 24 h", {
  missing <- file.path(withr::local_tempdir(), "no-such-file.agd")
  expect_error(analyze.agd.file(missing), "File not found")
  expect_identical(file.exists(missing), FALSE)
  txt <- withr::local_tempfile(fileext = ".csv", lines = "a,b")
  expect_error(analyze.agd.file(txt), "Unsupported file format")
  expect_error(quietly(analyze.agd.file(example_agd(1), min_wear_hours = 25, output_summary = FALSE)),
               "between 0 and 24")
})
