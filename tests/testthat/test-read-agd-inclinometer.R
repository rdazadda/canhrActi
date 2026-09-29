# Tests for has.inclinometer() in R/read_agd.R.

INCLINE <- c("inclineOff", "inclineStanding", "inclineSitting", "inclineLying")

test_that("has.inclinometer() finds the posture columns of an ActiLife .agd", {
  agd <- read.agd(example_agd(1), verbose = FALSE)
  expect_identical(has.inclinometer(agd), TRUE)
  # the same columns agd.counts() hands on to sleep scoring
  expect_identical(intersect(INCLINE, names(agd.counts(agd))), INCLINE)
})

test_that("has.inclinometer() needs one posture column and is FALSE without any", {
  counts <- data.frame(dataTimestamp = 638954656800000000, axis1 = 1)
  expect_identical(has.inclinometer(list(data = counts)), FALSE)
  expect_identical(intersect(INCLINE, names(agd.counts(list(data = counts)))), character(0))
  for (col in INCLINE) {
    one <- counts
    one[[col]] <- 0L
    expect_identical(has.inclinometer(list(data = one)), TRUE, info = col)
  }
  expect_identical(has.inclinometer(list(data = NULL)), FALSE)
})
