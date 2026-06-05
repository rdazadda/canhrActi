# Tests for reintegrate.epochs (sub-minute -> 60s for sleep scoring).

test_that("reintegrate.epochs sums consecutive epochs up to 60s", {
  ts <- as.POSIXct("2024-01-01", tz = "UTC") + (0:239) * 15   # 240 x 15s = 60 min
  r <- reintegrate.epochs(rep(10, 240), ts, rep(TRUE, 240), from_epoch = 15, to_epoch = 60)
  expect_equal(length(r$counts), 60)
  expect_true(all(r$counts == 40))          # 4 x 10 per minute
  expect_equal(r$epoch_length, 60)
  expect_equal(length(r$timestamps), 60)
  expect_equal(length(r$wear), 60)
  expect_equal(r$timestamps[2], ts[5])       # first ts of the 2nd minute
})

test_that("reintegrate.epochs is a no-op for 60s and non-divisor epochs", {
  ts <- as.POSIXct("2024-01-01", tz = "UTC") + (0:99) * 60
  expect_equal(length(reintegrate.epochs(rep(5, 100), ts, from_epoch = 60)$counts), 100)
  # 45s does not divide 60s -> unchanged
  expect_equal(length(reintegrate.epochs(rep(5, 100), ts, from_epoch = 45, to_epoch = 60)$counts), 100)
  # NULL/NA epoch -> unchanged
  expect_equal(length(reintegrate.epochs(rep(5, 100), ts, from_epoch = NA)$counts), 100)
})

test_that("reintegrate.epochs wear flag is a majority vote", {
  ts <- as.POSIXct("2024-01-01", tz = "UTC") + (0:7) * 15      # 2 minutes at 15s
  w <- c(TRUE, TRUE, TRUE, FALSE,  FALSE, FALSE, FALSE, TRUE)  # min1 3/4 worn, min2 1/4
  r <- reintegrate.epochs(rep(1, 8), ts, w, from_epoch = 15, to_epoch = 60)
  expect_equal(r$wear, c(TRUE, FALSE))
})

test_that("scoring at 60s matches reintegrate-then-score for a 15s series", {
  set.seed(3)
  base60 <- pmax(0, round(rnorm(120, 200, 150)))              # 120 one-min epochs
  # explode each minute into 4 x 15s epochs that sum back to base60
  # 4x120 matrix (each column = one minute's 4 sub-epochs); column-major flatten
  # keeps each minute's 4 sub-epochs consecutive.
  cnt15 <- as.numeric(sapply(base60, function(m) {
    as.numeric(stats::rmultinom(1, m, rep(0.25, 4)))
  }))
  ts15 <- as.POSIXct("2024-01-01", tz = "UTC") + (seq_along(cnt15) - 1) * 15
  r <- reintegrate.epochs(cnt15, ts15, from_epoch = 15, to_epoch = 60)
  expect_equal(r$counts, base60)                               # reintegration recovers the minutes
  # Cole-Kripke on the reintegrated series == Cole-Kripke on the native 60s series
  expect_identical(sleep.cole.kripke(r$counts), sleep.cole.kripke(base60))
})
