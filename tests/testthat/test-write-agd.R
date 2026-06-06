# write.agd() must round-trip exactly through read.agd() + agd.counts().

test_that("write.agd round-trips counts + metadata through read.agd", {
  set.seed(1)
  n  <- 200
  ts <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC") + (0:(n - 1)) * 60
  counts <- data.frame(
    timestamp = ts,
    axis1 = sample(0:9000, n, TRUE), axis2 = sample(0:9000, n, TRUE),
    axis3 = sample(0:9000, n, TRUE), steps = sample(0:60, n, TRUE), lux = sample(0:120, n, TRUE)
  )
  p <- tempfile(fileext = ".agd")
  on.exit(unlink(p), add = TRUE)
  write.agd(counts, p, epoch_length = 60, device_serial = "TEST123", subject_name = "S1")

  agd <- read.agd(p, verbose = FALSE)
  cc  <- agd.counts(agd)

  expect_equal(cc$axis1, counts$axis1)
  expect_equal(cc$axis2, counts$axis2)
  expect_equal(cc$axis3, counts$axis3)
  expect_equal(cc$steps, counts$steps)
  expect_equal(as.numeric(cc$timestamp), as.numeric(counts$timestamp), tolerance = 1)

  gs <- function(name) agd$settings$settingValue[tolower(agd$settings$settingName) == name]
  expect_equal(gs("epochlength"), "60")
  expect_equal(gs("deviceserial"), "TEST123")
  expect_equal(gs("subjectname"), "S1")
  expect_equal(gs("epochcount"), "200")
})

test_that("write.agd handles 5s epochs + missing optional columns", {
  ts <- as.POSIXct("2024-06-01 08:00:00", tz = "UTC") + (0:49) * 5
  counts <- data.frame(timestamp = ts, axis1 = 1:50, axis2 = 51:100, axis3 = 101:150)
  p <- tempfile(fileext = ".agd")
  on.exit(unlink(p), add = TRUE)
  write.agd(counts, p, epoch_length = 5)
  cc <- agd.counts(read.agd(p, verbose = FALSE))
  expect_equal(cc$axis1, 1:50)
  expect_equal(cc$steps, rep(0L, 50))   # default-filled
})
