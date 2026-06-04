# Chi-square (Sokolove-Bushell) periodogram: period recovery, significance, and
# graceful degradation.

.cs_signal <- function(per, days = 10, epl = 60, seed = 1) {
  n <- days * 86400 / epl
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, ts[1], units = "hours"))
  set.seed(seed)
  list(ts = ts, act = pmax(0, 100 + 80 * cos(2 * pi * (th - 14) / per) + rnorm(n, 0, 5)),
       epl = epl)
}

test_that("recovers a known 24 h period and is significant", {
  d <- .cs_signal(24)
  cs <- chi.sq.periodogram(d$act, d$ts, epoch_length = d$epl)
  expect_equal(cs$period, 24, tolerance = 0.05)
  expect_true(cs$significant)
  expect_lt(cs$p_value, 0.001)
  expect_equal(length(cs$scanned), length(cs$Qp))
  expect_gt(length(cs$scanned), 1)
})

test_that("recovers a non-24 h period", {
  d <- .cs_signal(25, seed = 7)
  cs <- chi.sq.periodogram(d$act, d$ts, epoch_length = d$epl)
  expect_equal(cs$period, 25, tolerance = 0.1)
  expect_true(cs$significant)
})

test_that("agrees with the Lomb-Scargle period estimate", {
  d <- .cs_signal(24, seed = 3)
  cs <- chi.sq.periodogram(d$act, d$ts, epoch_length = d$epl)
  ls <- circadian.period(d$act, d$ts)
  expect_lt(abs(cs$period - ls$tau), 0.3)
})

test_that("does not flag pure noise (family-wise correction)", {
  n <- 10 * 1440
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * 60
  set.seed(9)
  cs <- chi.sq.periodogram(rnorm(n, 100, 20), ts, epoch_length = 60)
  expect_false(isTRUE(cs$significant))
})

test_that("recovers the period through a non-wear gap", {
  d <- .cs_signal(24, seed = 5)
  a <- d$act
  a[3000:4200] <- NA
  cs <- chi.sq.periodogram(a, d$ts, epoch_length = d$epl)
  expect_equal(cs$period, 24, tolerance = 0.1)
})

test_that("degrades gracefully on insufficient data", {
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (0:4) * 60
  cs <- chi.sq.periodogram(c(1, 2, 3, 4, 5), ts, epoch_length = 60)
  expect_true(is.na(cs$period))
  expect_equal(length(cs$scanned), 0)
})
