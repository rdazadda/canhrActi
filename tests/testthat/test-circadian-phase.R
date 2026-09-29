# circadian.is.multiscale() (R/circadian_phase.R): interdaily stability at several bin
# widths, N * sum_h (xbar_h - xbar)^2 / (p * sum_i (x_i - xbar)^2) over the per-day bin
# means (Witting et al. 1990), on series whose answer is known

.is_grid <- function(days, start = "2024-01-01 00:00:00") {
  seq(as.POSIXct(start, tz = "UTC"), by = 60, length.out = days * 1440)
}

test_that("two days that differ by a constant give the same IS at every width", {
  # 0 before noon and 100 after, 50 higher on day 2: each bin mean is 25 from both of its
  # days and 50 from the grand mean, so IS = 2500 / (2500 + 625) = 0.8
  ts <- .is_grid(2)
  lt <- as.POSIXlt(ts)
  x <- ifelse(lt$hour < 12, 0, 100) + ifelse(lt$mday == 2, 50, 0)
  r <- circadian.is.multiscale(x, ts)
  expect_s3_class(r, "data.frame")
  expect_identical(names(r), c("bin_minutes", "IS"))
  expect_identical(r$bin_minutes, c(60, 30, 15))
  expect_equal(r$IS, c(0.8, 0.8, 0.8))
})

test_that("identical days give 1 at every width, in the order the widths are asked for", {
  ts <- .is_grid(2)
  lt <- as.POSIXlt(ts)
  x <- ifelse(lt$min < 30, 10, 90) + lt$hour
  r <- circadian.is.multiscale(x, ts, bin_minutes = c(15, 60, 5, 30))
  expect_identical(r$bin_minutes, c(15, 60, 5, 30))
  expect_identical(r$IS, c(1, 1, 1, 1))
})

test_that("a pattern inside the hour that flips between days is seen only by the finer bins", {
  # every hourly mean is 50, so at 60 minutes there is no variance and IS is NA
  ts <- .is_grid(2)
  lt <- as.POSIXlt(ts)
  x <- ifelse((lt$min < 30) == (lt$mday == 1), 0, 100)
  expect_identical(circadian.is.multiscale(x, ts)$IS, c(NA, 0, 0))
})

test_that("bins follow the clock and calendar day, and NA epochs are left out", {
  ts <- .is_grid(3)
  x <- ifelse(as.POSIXlt(ts)$hour >= 8 & as.POSIXlt(ts)$hour < 20, 200, 20)
  x[seq(5, length(x), by = 7)] <- NA
  expect_identical(circadian.is.multiscale(x, ts, bin_minutes = 60)$IS, 1)
  # three days from 13:00 still hold every clock hour three times
  ts13 <- .is_grid(3, start = "2024-01-01 13:00:00")
  h13 <- as.POSIXlt(ts13)$hour
  expect_identical(circadian.is.multiscale(ifelse(h13 >= 8 & h13 < 20, 200, 20), ts13,
                                           bin_minutes = c(60, 15))$IS, c(1, 1))
})

test_that("one day is too short, and the lengths must match", {
  ts <- .is_grid(1)
  x <- as.POSIXlt(ts)$hour
  expect_identical(circadian.is.multiscale(x, ts)$IS, rep(NA_real_, 3))
  expect_error(circadian.is.multiscale(x[-1], ts),
               "counts and timestamps must have the same length")
})
