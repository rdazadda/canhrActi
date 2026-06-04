# GGIR-style valid-day filter inside circadian.rhythm(): low-wear days are
# excluded by default when a wear_time mask is supplied, with an opt-out.

.vd_three_days <- function(day2_wear_epochs = 480, epl = 60) {
  n <- 3 * 1440
  ts <- as.POSIXct("2024-01-06 00:00:00", tz = "UTC") + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, ts[1], units = "hours"))
  set.seed(1)
  counts <- pmax(0, 100 + 80 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 15))
  wear <- rep(TRUE, n)
  # Day 2 = epochs 1441..2880; keep only the first `day2_wear_epochs` worn.
  d2 <- 1441:2880
  if (day2_wear_epochs < length(d2)) wear[d2[(day2_wear_epochs + 1):length(d2)]] <- FALSE
  list(ts = ts, counts = counts, wear = wear, epl = epl)
}

test_that("default gate drops a low-wear day from recording-level metrics", {
  d <- .vd_three_days(day2_wear_epochs = 480)  # day 2 = 8 h wear (< 10 h)
  on  <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, epoch_length = d$epl)
  off <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, min_valid_hours = 0,
                          epoch_length = d$epl)
  expect_lt(on$n_valid_epochs, off$n_valid_epochs)
  expect_equal(off$n_valid_epochs - on$n_valid_epochs, 480)  # day 2's 8 h removed
  expect_equal(on$valid_day_min_hours, 10)
  expect_equal(off$valid_day_min_hours, 0)
})

test_that("a day at exactly the threshold is retained", {
  d <- .vd_three_days(day2_wear_epochs = 600)  # exactly 10 h
  on  <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, epoch_length = d$epl)
  off <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, min_valid_hours = 0,
                          epoch_length = d$epl)
  expect_equal(on$n_valid_epochs, off$n_valid_epochs)
})

test_that("just below the threshold is dropped", {
  d <- .vd_three_days(day2_wear_epochs = 599)  # 9.98 h < 10 h
  on  <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, epoch_length = d$epl)
  off <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, min_valid_hours = 0,
                          epoch_length = d$epl)
  expect_equal(off$n_valid_epochs - on$n_valid_epochs, 599)
})

test_that("no gating without a wear_time mask (backward compatible)", {
  d <- .vd_three_days(day2_wear_epochs = 480)
  a <- circadian.rhythm(d$counts, d$ts, epoch_length = d$epl)
  b <- circadian.rhythm(d$counts, d$ts, min_valid_hours = 10, epoch_length = d$epl)
  expect_equal(a$n_valid_epochs, b$n_valid_epochs)   # gate needs wear_time
  expect_equal(a$valid_day_min_hours, 0)
  expect_equal(a$IS, b$IS)
})

test_that("opt-out keeps every worn epoch (pre-gate behaviour)", {
  d <- .vd_three_days(day2_wear_epochs = 480)
  off <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, min_valid_hours = 0,
                          epoch_length = d$epl)
  expect_equal(off$n_valid_epochs, 1440 + 480 + 1440)
})

test_that("dropping a contaminated day changes the recording-level IV", {
  d <- .vd_three_days(day2_wear_epochs = 480)
  # Make day 2's worn window noisy so its inclusion fragments the rhythm.
  set.seed(7)
  d2_worn <- 1441:1920
  d$counts[d2_worn] <- pmax(0, d$counts[d2_worn] + rnorm(length(d2_worn), 0, 200))
  on  <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, epoch_length = d$epl)
  off <- circadian.rhythm(d$counts, d$ts, wear_time = d$wear, min_valid_hours = 0,
                          epoch_length = d$epl)
  expect_false(isTRUE(all.equal(on$IV, off$IV)))
})
