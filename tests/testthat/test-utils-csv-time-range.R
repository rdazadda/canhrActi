# Tests for load.actigraph.csv() and filter.time.range() in R/utils.R.

# the ten lines ActiLife writes above the columns of a raw .csv export
ACTILIFE_HEADER <- c(
  paste0("------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.3 Firmware v1.8.0 ",
         "date format M/d/yyyy at 30 Hz  Filter Normal -----------"),
  "Serial Number: NEO1F00000000", "Start Time 10:00:00", "Start Date 1/1/2024",
  "Epoch Period (hh:mm:ss) 00:00:00", "Download Time 12:00:00", "Download Date 1/2/2024",
  "Current Memory Address: 0", "Current Battery Voltage: 4.20     Mode = 12",
  "--------------------------------------------------")

ten_minutes <- function() {
  data.frame(timestamp = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (0:9) * 60, v = 1:10)
}

test_that("load.actigraph.csv() reads the axes under ActiLife's header", {
  f <- withr::local_tempfile(fileext = ".csv", lines = c(
    ACTILIFE_HEADER, "Accelerometer X,Accelerometer Y,Accelerometer Z",
    "0.012,-0.998,0.051", "0.020,-1.004,0.047", "0.015,-0.990,0.049"))
  expect_warning(got <- load.actigraph.csv(f), "No timestamp column found")
  expect_identical(names(got), c("timestamp", "x", "y", "z"))
  expect_identical(got$x, c(0.012, 0.020, 0.015))
  expect_identical(got$y, c(-0.998, -1.004, -0.990))
  expect_identical(got$z, c(0.051, 0.047, 0.049))
  # without times it counts 60 samples a second from midnight on 1 January 2024
  expect_identical(format(got$timestamp[1], "%Y-%m-%d %H:%M:%S"), "2024-01-01 00:00:00")
  expect_equal(as.numeric(diff(got$timestamp), units = "secs"), c(1, 1) / 60, tolerance = 1e-5)
})

test_that("load.actigraph.csv() keeps a timestamp column and maps X, Y and Z", {
  f <- withr::local_tempfile(fileext = ".csv", lines = c(
    "timestamp,X,Y,Z", "2024-01-01 10:00:00,1,2,3", "2024-01-01 10:00:01,4,5,6"))
  expect_silent(got <- load.actigraph.csv(f, skip.lines = 0))
  expect_identical(names(got), c("timestamp", "x", "y", "z"))
  expect_identical(as.character(got$timestamp), c("2024-01-01 10:00:00", "2024-01-01 10:00:01"))
  expect_equal(c(got$x, got$y, got$z), c(1, 4, 2, 5, 3, 6))
})

test_that("load.actigraph.csv() keeps the Timestamp column ActiLife writes", {
  f <- withr::local_tempfile(fileext = ".csv", lines = c(
    ACTILIFE_HEADER, "Timestamp,Accelerometer X,Accelerometer Y,Accelerometer Z",
    "1/1/2024 10:00:00.000,0.012,-0.998,0.051", "1/1/2024 10:00:00.033,0.020,-1.004,0.047"))
  expect_silent(got <- load.actigraph.csv(f))
  expect_identical(names(got), c("timestamp", "x", "y", "z"))
  expect_match(as.character(got$timestamp[1]), "10:00:00", fixed = TRUE)
})

test_that("load.actigraph.csv() stops on a missing file or a missing axis", {
  expect_error(load.actigraph.csv(file.path(withr::local_tempdir(), "none.csv")), "File not found")
  f <- withr::local_tempfile(fileext = ".csv", lines = c("timestamp,X,Y", "2024-01-01 10:00:00,1,2"))
  expect_error(load.actigraph.csv(f, skip.lines = 0), "Missing required columns: z")
})

test_that("filter.time.range() keeps the rows between both ends, inclusive", {
  d <- ten_minutes()
  expect_identical(filter.time.range(d, d$timestamp[3], d$timestamp[6])$v, 3:6)
  expect_identical(nrow(filter.time.range(d, d$timestamp[10] + 1, d$timestamp[10] + 60)), 0L)
  expect_error(filter.time.range(data.frame(time = 1), 0, 1), "must have a 'timestamp' column")
})

test_that("filter.time.range() reads character times when the session zone is the data's", {
  withr::local_timezone("UTC")
  got <- filter.time.range(ten_minutes(), "2024-01-01 00:02:00", "2024-01-01 00:05:00")
  expect_identical(got$v, 3:6)
})

test_that("filter.time.range() reads character times in the data's time zone", {
  withr::local_timezone("America/Anchorage")
  got <- filter.time.range(ten_minutes(), "2024-01-01 00:02:00", "2024-01-01 00:05:00")
  expect_identical(got$v, 3:6)
})
