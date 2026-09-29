# Tests for R/read_devices.R: read.accelerometer() on the package's sample .agd files and
# check.device.support().

test_that("read.accelerometer() returns the .agd counts and settings in the standard shape", {
  p <- example_agd(1)
  utils::capture.output(got <- read.accelerometer(p))
  agd <- read.agd(p, verbose = FALSE)
  expect_identical(names(got), c("data", "settings", "device_type", "file_type"))
  expect_identical(got$data, agd.counts(agd))
  expect_identical(got$settings, agd$settings)
  expect_identical(c(got$device_type, got$file_type), c("ActiGraph", "agd"))
})

test_that("read.accelerometer() says what it reads when verbose", {
  out <- utils::capture.output(got <- read.accelerometer(example_agd(1), verbose = TRUE))
  expect_identical(out[1], "Reading ActiGraph AGD file")
})

test_that("read.accelerometer() prints nothing when verbose = FALSE", {
  expect_silent(read.accelerometer(example_agd(2), verbose = FALSE))
})

test_that("read.accelerometer() takes .agd in any case and refuses anything else", {
  expect_error(read.accelerometer(file.path(withr::local_tempdir(), "no-such-file.agd")),
               "File not found")
  csv <- withr::local_tempfile(fileext = ".csv", lines = "x")
  expect_error(read.accelerometer(csv), "Unsupported file format: .csv", fixed = TRUE)
  upper <- withr::local_tempfile(fileext = ".AGD")
  expect_identical(file.copy(example_agd(2), upper), TRUE)
  utils::capture.output(got <- read.accelerometer(upper, verbose = FALSE))
  expect_identical(nrow(got$data), 10482L)
})

test_that("check.device.support() lists the .agd reader and whether RSQLite is there", {
  out <- utils::capture.output(res <- withVisible(check.device.support()))
  expect_identical(res$visible, FALSE)
  d <- res$value
  expect_identical(d$device, "ActiGraph AGD")
  expect_identical(d$required_package, "RSQLite")
  expect_identical(d$installed, requireNamespace("RSQLite", quietly = TRUE))
  expect_identical(d$status, if (d$installed) "Available" else "Not Available")
  expect_identical(sum(grepl("[OK] ActiGraph AGD", out, fixed = TRUE)), as.integer(d$installed))
})
