# sleep.regularity.index() (R/circadian.R): the Phillips et al. (2017) Sleep Regularity
# Index over the epoch-of-day by day matrix, on sleep patterns worked out by hand

.sri_grid <- function(days, start = "2024-01-01 00:00:00", epoch = 60) {
  seq(as.POSIXct(start, tz = "UTC"), by = epoch, length.out = days * 86400 / epoch)
}
.sri_hours <- function(ts) {
  lt <- as.POSIXlt(ts)
  lt$hour + lt$min / 60
}
# calendar day from the first one, 0 based
.sri_day <- function(ts) as.integer(as.Date(ts, tz = "UTC") - as.Date(ts[1], tz = "UTC"))

test_that("the same sleep window every day scores 100 and a window that flips each day -100", {
  ts <- .sri_grid(4)
  h <- .sri_hours(ts)
  expect_identical(sleep.regularity.index(ifelse(h < 8, "S", "W"), ts), 100)
  flip <- (h < 8) == (.sri_day(ts) %% 2 == 0)
  expect_identical(sleep.regularity.index(ifelse(flip, "S", "W"), ts), -100)
})

test_that("a one-hour shift on the middle day of three gives the hand-worked value", {
  # each of the two day pairs disagrees on 120 of 1440 minutes, so
  # SRI = -100 + 200 * 2640 / 2880
  ts <- .sri_grid(3)
  h <- .sri_hours(ts)
  asleep <- ifelse(.sri_day(ts) == 1, h >= 1 & h < 9, h < 8)
  st <- ifelse(asleep, "S", "W")
  expect_equal(sleep.regularity.index(st, ts), 83.33)
  expect_identical(sleep.regularity.index(st, ts), sri.matrix(st, ts)$SRI)
})

test_that("non-wear epochs are left out of the pairs instead of counting as wake", {
  ts <- .sri_grid(3)
  h <- .sri_hours(ts)
  st <- ifelse(h < 8, "S", "W")
  st[.sri_day(ts) == 1 & h < 8] <- NA
  expect_identical(sleep.regularity.index(st, ts), 100)
  # read as wake, that night disagrees on 480 minutes in each of the two pairs
  st[is.na(st)] <- "W"
  expect_equal(sleep.regularity.index(st, ts), 33.33)
})

test_that("dropped epochs do not shift the later days out of step", {
  ts <- .sri_grid(3)
  h <- .sri_hours(ts)
  keep <- !(.sri_day(ts) == 1 & h >= 12 & h < 18)
  st <- ifelse(h < 8, "S", "W")
  expect_identical(sleep.regularity.index(st[keep], ts[keep]), 100)
  # a start at noon and a 30 s epoch are matched by clock time too
  noon <- .sri_grid(3, start = "2024-01-01 12:00:00")
  expect_identical(sleep.regularity.index(ifelse(.sri_hours(noon) < 8, "S", "W"), noon), 100)
  ts30 <- .sri_grid(3, epoch = 30)
  expect_identical(sleep.regularity.index(ifelse(.sri_hours(ts30) < 8, "S", "W"), ts30,
                                          epoch_length = 30), 100)
})

test_that("lengths must match, and under two days is warned about", {
  ts <- .sri_grid(3)
  st <- ifelse(.sri_hours(ts) < 8, "S", "W")
  expect_error(sleep.regularity.index(st[1:10], ts),
               "sleep_state and timestamps must have same length")
  # one calendar day has no pair to compare
  one <- .sri_grid(1)
  expect_warning(r <- sleep.regularity.index(ifelse(.sri_hours(one) < 8, "S", "W"), one),
                 "Less than 2 days of data")
  expect_identical(r, NA_real_)
  # a day and a half still has one half-day pair
  half <- .sri_grid(1.5)
  expect_warning(r <- sleep.regularity.index(ifelse(.sri_hours(half) < 8, "S", "W"), half),
                 "Less than 2 days of data")
  expect_identical(r, 100)
})
