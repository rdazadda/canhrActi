# circadian.ggir() (R/circadian_ggir.R): GGIR's part-2 circadian block. A cosine in
# log(mg + 1) has known answers, and with GGIR installed every column must equal GGIR's own
# cosinor_IS_IV_Analyses() on the same series.

GGIR_CIRCADIAN_COLUMNS <- c(
  "cosinor_timeOffsetHours", "cosinor_mes", "cosinor_amp", "cosinor_acrophase",
  "cosinor_acrotime", "cosinor_ndays", "cosinor_R2", "cosinorExt_minimum", "cosinorExt_amp",
  "cosinorExt_alpha", "cosinorExt_beta", "cosinorExt_acrotime", "cosinorExt_UpMesor",
  "cosinorExt_DownMesor", "cosinorExt_MESOR", "cosinorExt_ndays", "cosinorExt_F_pseudo",
  "cosinorExt_R2", "IS", "IV", "phi")

# a series whose log(x + 1) is mesor + amp * cos(2 pi (t - peak) / 24), t in clock hours
.log_cosine <- function(days = 4, mesor = 3, amp = 1.5, peak = 14, per_hour = 60) {
  t <- (0:(days * 24 * per_hour - 1) %% (24 * per_hour)) / per_hour
  exp(mesor + amp * cos(2 * pi * (t - peak) / 24)) - 1
}

# GGIR's own result in circadian.ggir()'s column order
.ggir_row <- function(x, epoch, offset, threshold) {
  g <- GGIR::cosinor_IS_IV_Analyses(Xi = x, epochsize = epoch, timeOffsetHours = offset,
                                    threshold = threshold)
  unname(unlist(c(offset,
    g$coef$params[c("mes", "amp", "acr", "acrotime", "ndays", "R2")],
    g$coefext$params[c("minimum", "amp", "alpha", "beta", "acrotime", "UpMesor", "DownMesor",
                       "MESOR", "ndays", "F_pseudo", "R2")],
    g$IVIS[c("InterdailyStability", "IntradailyVariability", "phi")])))
}

test_that("the result is one row of GGIR's 21 part-2 circadian columns", {
  skip_if_not_installed("ActCR")
  r <- circadian.ggir(.log_cosine())
  expect_s3_class(r, "data.frame")
  expect_identical(dim(r), c(1L, 21L))
  expect_identical(names(r), GGIR_CIRCADIAN_COLUMNS)
})

test_that("a cosine in log(mg + 1) gives back its mesor, amplitude and peak", {
  skip_if_not_installed("ActCR")
  r <- circadian.ggir(.log_cosine())
  expect_equal(r$cosinor_mes, 3, tolerance = 1e-6)
  expect_equal(r$cosinor_amp, 1.5, tolerance = 1e-6)
  expect_equal(r$cosinor_R2, 1, tolerance = 1e-8)
  # ActCR puts the first sample at minute 1, so the 14:00 peak reads one minute late
  expect_equal(r$cosinor_acrotime, 14 + 1 / 60, tolerance = 1e-6)
  expect_equal(r$cosinor_acrophase, r$cosinor_acrotime * 2 * pi / 24, tolerance = 1e-6)
  expect_equal(r$cosinorExt_acrotime, r$cosinor_acrotime, tolerance = 1e-4)
  expect_identical(c(r$cosinor_ndays, r$cosinorExt_ndays), c(4, 4))
  # every day is the same, so the interdaily stability of the binarised hours is 1
  expect_equal(r$IS, 1, tolerance = 1e-12)
})

test_that("time_offset_hours moves the clock estimates back by the offset", {
  skip_if_not_installed("ActCR")
  x <- .log_cosine()
  r0 <- circadian.ggir(x)
  r6 <- circadian.ggir(x, time_offset_hours = 6)
  expect_identical(r6$cosinor_timeOffsetHours, 6)
  expect_equal(r6$cosinor_acrotime, r0$cosinor_acrotime - 6)
  expect_equal(r6$cosinorExt_acrotime, r0$cosinorExt_acrotime - 6)
  expect_equal(r6$cosinorExt_UpMesor, r0$cosinorExt_UpMesor - 6)
  expect_equal(r6$cosinorExt_DownMesor, r0$cosinorExt_DownMesor - 6)
  expect_equal(r6$cosinor_acrophase, r0$cosinor_acrophase - 6 / 24 * 2 * pi)
  # an estimate that falls before midnight wraps to the day before
  r16 <- circadian.ggir(x, time_offset_hours = 16)
  expect_equal(r16$cosinor_acrotime, r0$cosinor_acrotime - 16 + 24)
  expect_equal(r16$cosinor_acrophase, r0$cosinor_acrophase - 16 / 24 * 2 * pi + 2 * pi)
  # the fit itself does not depend on the offset
  keep <- c("cosinor_mes", "cosinor_amp", "cosinorExt_amp", "IS", "IV", "phi")
  expect_identical(r6[, keep], r0[, keep])
})

test_that("a series in g is scaled to mg, which is GGIR's own test", {
  skip_if_not_installed("ActCR")
  x <- .log_cosine()
  r_mg <- circadian.ggir(x)
  r_g <- circadian.ggir(x / 1000)
  expect_equal(r_g$cosinor_mes, r_mg$cosinor_mes, tolerance = 1e-12)
  # the extended cosinor is an iterative fit, so it agrees to the fit's tolerance
  expect_equal(r_g, r_mg, tolerance = 1e-4)
  # values under 13 with a mean of 1 or more are not taken to be g
  x10 <- x / 10
  expect_lt(max(x10), 13)
  expect_gte(mean(x10), 1)
  r10 <- circadian.ggir(x10, threshold = 5)
  expect_lt(r10$cosinor_mes, r_mg$cosinor_mes)
})

test_that("series GGIR's code cannot fit give NULL, not an error", {
  skip_if_not_installed("ActCR")
  expect_null(circadian.ggir(1))
  expect_null(circadian.ggir(rep(NA_real_, 2880)))
  # under a whole day there is nothing to fit
  expect_null(circadian.ggir(.log_cosine()[1:720]))
  # all under the 40 mg threshold binarises to a constant, which GGIR's AR(1) fit rejects
  expect_null(circadian.ggir(.log_cosine() / 10))
})

test_that("every column equals GGIR's own cosinor_IS_IV_Analyses on the same series", {
  skip_if_not_installed("ActCR")
  skip_if_not_installed("GGIR")
  noise <- withr::with_seed(1, abs(stats::rnorm(3 * 2880, 0, 3)))
  cases <- list(
    list(x = .log_cosine(), epoch = 60, offset = 0, threshold = 40),
    list(x = .log_cosine(days = 3, peak = 20), epoch = 60, offset = 2.5, threshold = 40),
    list(x = .log_cosine(days = 3) / 1000, epoch = 60, offset = 0, threshold = 20),
    list(x = .log_cosine(days = 3, peak = 20, per_hour = 120) + noise, epoch = 30, offset = 0,
         threshold = 40))
  for (k in seq_along(cases)) {
    cs <- cases[[k]]
    r <- circadian.ggir(cs$x, epoch_length = cs$epoch, time_offset_hours = cs$offset,
                        threshold = cs$threshold)
    expect_identical(unname(unlist(r)), .ggir_row(cs$x, cs$epoch, cs$offset, cs$threshold),
                     info = paste("case", k))
  }
})
