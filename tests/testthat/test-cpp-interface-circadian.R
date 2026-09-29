# circadian_cpp() (R/cpp_interface.R): the all-in-one C++ circadian call, on a cosine day
# and on alternating hours whose answers are known, and against the single-metric exports

# minute counts 100 + 50 cos(2 pi (t - 15) / 24): peak at 15:00, trough at 03:00
.cos_minutes <- function(days = 3, start_minute = 0) {
  tod <- ((start_minute + 0:(days * 1440 - 1)) %% 1440) / 60
  100 + 50 * cos(2 * pi * (tod - 15) / 24)
}
# mean of that curve over a w hour window centred on its peak (+1) or trough (-1)
.window_mean <- function(w, side) 100 + side * 50 * sin(pi * w / 24) / (pi * w / 24)
.alternating <- function(hours) rep(rep(c(0, 1), each = 60), hours / 2)

test_that("L5, M10, L1 and M1 of a cosine day are the windows round the trough and the peak", {
  r <- circadian_cpp(.cos_minutes())
  expect_equal(r$L5_value, .window_mean(5, -1), tolerance = 1e-5)
  expect_equal(r$M10_value, .window_mean(10, 1), tolerance = 1e-5)
  expect_equal(r$L1_value, .window_mean(1, -1), tolerance = 1e-5)
  expect_equal(r$M1_value, .window_mean(1, 1), tolerance = 1e-5)
  # a window centred on 03:00 or 15:00 starts half its width earlier, to the minute
  onsets <- c(r$L5_onset_hours, r$M10_onset_hours, r$L1_onset_hours, r$M1_onset_hours)
  expect_lte(max(abs(onsets - c(0.5, 10, 2.5, 14.5))), 1 / 60 + 1e-9)
  expect_equal(r$RA, (r$M10_value - r$L5_value) / (r$M10_value + r$L5_value))
})

test_that("start_minute puts the onsets on the clock", {
  x <- .cos_minutes(start_minute = 360)
  r <- circadian_cpp(x, start_minute = 360L)
  expect_lte(max(abs(c(r$L5_onset_hours, r$M10_onset_hours) - c(0.5, 10))), 1 / 60)
  # read as if it started at midnight, every onset is six hours early
  r0 <- circadian_cpp(x)
  expect_lte(max(abs(c(r0$L5_onset_hours, r0$M10_onset_hours) - c(18.5, 4))), 1 / 60)
  expect_equal(r0$L5_value, r$L5_value)
})

test_that("IS, IV and phi are computed on hourly sums", {
  expect_equal(circadian_cpp(.cos_minutes())$IS, 1)
  # hourly sums 0, 60, 0, 60, ...: IV = n sum(diff^2) / ((n - 1) sum((x - mean)^2)) = 4, and
  # the lag-1 autocorrelation is -(n - 1) / n, as stats::acf() computes it
  x <- .alternating(48)
  r <- circadian_cpp(x)
  expect_equal(r$IS, 1)
  expect_equal(r$IV, 4)
  expect_equal(r$phi, -47 / 48)
  hourly <- colSums(matrix(x, nrow = 60))
  expect_equal(r$phi, stats::acf(hourly, lag.max = 1, plot = FALSE)$acf[2])
  expect_identical(c(r$n_minutes, r$n_hours, r$n_days), c(2880L, 48L, 2L))
})

test_that("the day metrics need a whole day and IS needs two", {
  r <- circadian_cpp(.alternating(20))
  day_metrics <- c("L5_value", "L5_onset_hours", "M10_value", "M10_onset_hours", "RA",
                   "L1_value", "L1_onset_hours", "M1_value", "M1_onset_hours", "IS")
  expect_identical(unname(vapply(r[day_metrics], is.na, logical(1))), rep(TRUE, 10))
  expect_equal(r$IV, 4)
  expect_equal(r$phi, -19 / 20)
  expect_identical(c(r$n_minutes, r$n_hours, r$n_days), c(1200L, 20L, 0L))
  r1 <- circadian_cpp(.cos_minutes(days = 1))
  expect_false(is.na(r1$L5_value))
  expect_identical(r1$IS, NA_real_)
})

test_that("the combined call agrees with L5M10_cpp, IS_cpp and IV_cpp", {
  # a different level each day, so IS is below 1
  x <- .cos_minutes() + rep(c(0, 20, 40), each = 1440)
  r <- circadian_cpp(x)
  # colSums() adds in a different order from the C++ loop, so the last bits can differ
  hourly <- colSums(matrix(x, nrow = 60))
  expect_equal(r$IS, IS_cpp(hourly))
  expect_lt(r$IS, 1)
  expect_equal(r$IV, IV_cpp(hourly))
  l5 <- L5M10_cpp(x)
  expect_identical(c(r$L5_value, r$L5_onset_hours, r$M10_value, r$M10_onset_hours, r$RA),
                   c(l5$L5_value, l5$L5_onset_hours, l5$M10_value, l5$M10_onset_hours, l5$RA))
  l1 <- L5M10_cpp(x, window_L5 = 60L, window_M10 = 60L)
  expect_identical(c(r$L1_value, r$M1_value), c(l1$L5_value, l1$M10_value))
})

test_that("IS and the combined circadian call refuse a day of no hours", {
  expect_error(IS_cpp(rep(1, 48), 0), "hours_per_day must be positive")
  expect_error(circadian_cpp(rep(1, 2880), 0), "hours_per_day must be positive")
})
