test_that("read.agd validates file existence", {
  expect_error(read.agd("nonexistent_file.agd"), "File not found")
})

test_that("the .agd readers create nothing at a missing path", {
  missing <- withr::local_tempfile(fileext = ".agd")

  expect_error(read.agd(missing, verbose = FALSE), "File not found")
  expect_false(file.exists(missing))

  meta <- .read.agd.participant.metadata(missing)
  expect_identical(meta$subject_name, tools::file_path_sans_ext(basename(missing)))
  expect_false(file.exists(missing))
})

test_that("read.agd prints its table list only when verbose", {
  expect_silent(quiet <- read.agd(example_agd(1), verbose = FALSE))
  expect_output(loud <- read.agd(example_agd(1)), "AGD file tables:")
  expect_identical(quiet, loud)
})

test_that("the example file reads at its recorded times, epoch and counts", {
  agd <- read.agd(example_agd(1), verbose = FALSE)
  setting <- function(name) agd$settings$settingValue[tolower(agd$settings$settingName) == name]
  expect_identical(setting("epochlength"), "60")

  cc <- agd.counts(agd)
  expect_equal(nrow(cc), 9919)
  expect_identical(format(cc$timestamp[c(1, 9919)], "%Y-%m-%d %H:%M:%S"),
                   c("2025-10-07 20:28:00", "2025-10-14 17:46:00"))
  expect_identical(unique(as.numeric(diff(cc$timestamp), units = "secs")), 60)
  expect_equal(unlist(cc[3, c("axis1", "axis2", "axis3", "steps")]),
               c(axis1 = 4126, axis2 = 3389, axis3 = 6198, steps = 30))
  expect_equal(colSums(cc[c("axis1", "axis2", "axis3", "steps")]),
               c(axis1 = 3207242, axis2 = 2836612, axis3 = 4004316, steps = 29625))
})

test_that("a write.agd file reads back at the absolute times it was given", {
  p <- withr::local_tempfile(fileext = ".agd")
  ts <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + (0:4) * 60
  write.agd(data.frame(timestamp = ts, axis1 = c(0, 15, 2400, 7, 0), axis2 = 1:5, axis3 = 9:5,
                       steps = c(0, 1, 30, 0, 0)), p, epoch_length = 60)

  agd <- read.agd(p, verbose = FALSE)
  # .NET ticks: 100 ns steps since 0001-01-01
  expect_identical(agd$data$dataTimestamp[c(1, 5)], c(638397072000000000, 638397074400000000))
  setting <- function(name) agd$settings$settingValue[tolower(agd$settings$settingName) == name]
  expect_identical(c(setting("epochlength"), setting("startdatetime")), c("60", "638397072000000000"))

  cc <- agd.counts(agd)
  expect_identical(format(cc$timestamp, "%Y-%m-%d %H:%M:%S"),
                   c("2024-01-01 12:00:00", "2024-01-01 12:01:00", "2024-01-01 12:02:00",
                     "2024-01-01 12:03:00", "2024-01-01 12:04:00"))
  expect_equal(cc$axis1, c(0, 15, 2400, 7, 0))
  expect_equal(cc$steps, c(0, 1, 30, 0, 0))
})

test_that("agd.counts extracts axis data correctly", {
  agd.data <- list(
    data = data.frame(
      dataTimestamp = as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")) * 10000000 + 62135596800 * 10000000,
      axis1 = 100,
      axis2 = 80,
      axis3 = 60,
      steps = 5
    ),
    settings = NULL
  )

  result <- agd.counts(agd.data, convert.timestamps = TRUE)

  expect_equal(result$axis1, 100)
  expect_equal(result$axis2, 80)
  expect_equal(result$axis3, 60)
  expect_equal(result$steps, 5)
})

test_that("agd.counts converts timestamps correctly", {
  timestamp.raw <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz = "UTC")) * 10000000 + 62135596800 * 10000000

  agd.data <- list(
    data = data.frame(
      dataTimestamp = timestamp.raw,
      axis1 = 100,
      axis2 = 80,
      axis3 = 60
    ),
    settings = NULL
  )

  result <- agd.counts(agd.data, convert.timestamps = TRUE)

  expect_s3_class(result$timestamp, "POSIXct")
  expect_identical(attr(result$timestamp, "tzone"), "UTC")
  expect_identical(format(result$timestamp, "%Y-%m-%d %H:%M:%S"), "2024-01-01 12:00:00")
})

test_that("agd.counts handles missing axis columns with NA", {
  agd.data <- list(
    data = data.frame(
      dataTimestamp = as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")) * 10000000 + 62135596800 * 10000000,
      axis1 = 100
    ),
    settings = NULL
  )

  result <- agd.counts(agd.data)

  expect_equal(result$axis1, 100)
  expect_true(is.na(result$axis2))
  expect_true(is.na(result$axis3))
})

test_that("agd.counts errors on missing timestamps", {
  # Must fail fast, not fabricate 1:n integer timestamps that corrupt analyses.
  agd.data <- list(
    data = data.frame(
      axis1 = c(100, 200, 300),
      axis2 = c(80, 160, 240),
      axis3 = c(60, 120, 180)
    ),
    settings = NULL
  )

  expect_error(agd.counts(agd.data), "No valid timestamp column found")
})

test_that("agd.counts can skip timestamp conversion", {
  timestamp.raw <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz = "UTC")) * 10000000 + 62135596800 * 10000000

  agd.data <- list(
    data = data.frame(
      dataTimestamp = timestamp.raw,
      axis1 = 100,
      axis2 = 80,
      axis3 = 60
    ),
    settings = NULL
  )

  result <- agd.counts(agd.data, convert.timestamps = FALSE)

  expect_equal(result$timestamp, timestamp.raw)
})

test_that("extract.subject.info extracts all fields correctly", {
  agd.data <- list(
    data = data.frame(axis1 = 100),
    settings = data.frame(
      settingName = c("subjectname", "sex", "age", "height", "mass"),
      settingValue = c("John Doe", "Male", "35", "175", "70")
    )
  )

  result <- extract.subject.info(agd.data)

  expect_equal(result$subject_id, "John Doe")
  expect_equal(result$sex, "M")
  expect_equal(result$age, 35)
  expect_equal(result$height, 175)
  expect_equal(result$mass, 70)
})

test_that("extract.subject.info handles missing settings", {
  agd.data <- list(
    data = data.frame(axis1 = 100),
    settings = NULL
  )

  result <- extract.subject.info(agd.data)

  expect_true(is.na(result$subject_id))
  expect_true(is.na(result$sex) || result$sex == "")
  expect_true(is.na(result$age) || result$age == 0)
  expect_true(is.na(result$mass))
})

test_that("extract.subject.info normalizes sex values", {
  test.cases <- list(
    list(input = "male", expected = "M"),
    list(input = "M", expected = "M"),
    list(input = "1", expected = "M"),
    list(input = "female", expected = "F"),
    list(input = "F", expected = "F"),
    list(input = "2", expected = "F"),
    list(input = "undefined", expected = ""),
    list(input = "", expected = "")
  )

  for (test in test.cases) {
    agd.data <- list(
      data = data.frame(axis1 = 100),
      settings = data.frame(
        settingName = "sex",
        settingValue = test$input
      )
    )

    result <- extract.subject.info(agd.data)
    expect_equal(result$sex, test$expected)
  }
})

test_that("extract.subject.info converts mass to pounds", {
  agd.data <- list(
    data = data.frame(axis1 = 100),
    settings = data.frame(
      settingName = "mass",
      settingValue = "70"
    )
  )

  result <- extract.subject.info(agd.data)

  expected.lbs <- round(70 * 2.20462, 1)
  expect_equal(result$weight_lbs, expected.lbs)
})

test_that("extract.subject.info handles zero or empty values", {
  agd.data <- list(
    data = data.frame(axis1 = 100),
    settings = data.frame(
      settingName = c("age", "mass", "height"),
      settingValue = c("0", "", "0")
    )
  )

  result <- extract.subject.info(agd.data)

  expect_true(is.na(result$age) || result$age == 0)
  expect_true(is.na(result$mass))
  expect_true(is.na(result$height))
})

test_that("extract.subject.info handles partial settings", {
  agd.data <- list(
    data = data.frame(axis1 = 100),
    settings = data.frame(
      settingName = c("age", "mass"),
      settingValue = c("35", "70")
    )
  )

  result <- extract.subject.info(agd.data)

  expect_equal(result$age, 35)
  expect_equal(result$mass, 70)
  expect_true(is.na(result$height))
  expect_true(is.na(result$subject_id))
})

test_that("agd functions preserve data integrity", {
  agd.data <- list(
    data = data.frame(
      dataTimestamp = rep(as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")) * 10000000 + 62135596800 * 10000000, 100),
      axis1 = 1:100,
      axis2 = 101:200,
      axis3 = 201:300,
      steps = 1:100
    ),
    settings = NULL
  )

  result <- agd.counts(agd.data)

  expect_equal(nrow(result), 100)
  expect_equal(result$axis1, 1:100)
  expect_equal(result$axis2, 101:200)
  expect_equal(result$axis3, 201:300)
  expect_equal(result$steps, 1:100)
})
