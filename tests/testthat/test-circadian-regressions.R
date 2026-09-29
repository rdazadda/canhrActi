# Regressions for three bugs found by diffing this package against actiRhythm.

# CPD: with no external reference the own mean phase was used, making accuracy equal to
# precision and CPD sqrt(2) * precision.

test_that("CPD and accuracy are NA without an external reference", {
  onsets <- c(2.5, 3.1, 2.8, 23.9, 3.4, 2.2, 3.0)

  res <- composite.phase.deviation(onsets)

  expect_true(is.na(res$CPD))
  expect_true(is.na(res$accuracy))
  expect_true(is.na(res$reference_phase))
  # precision is well defined without a reference: spread about the own mean
  expect_false(is.na(res$precision))
  expect_gt(res$precision, 0)
  expect_equal(res$n_days, length(onsets))
})

test_that("CPD is reported once a reference is given, and is not sqrt(2)*precision", {
  onsets <- c(2.5, 3.1, 2.8, 23.9, 3.4, 2.2, 3.0)

  res <- composite.phase.deviation(onsets, reference_phase = 1.0)

  expect_false(is.na(res$CPD))
  expect_false(is.na(res$accuracy))
  expect_equal(res$reference_phase, 1.0)
  expect_false(isTRUE(all.equal(res$accuracy, res$precision)))
  expect_false(isTRUE(all.equal(res$CPD, sqrt(2) * res$precision)))
})

test_that("CPD handles the midnight wrap in both terms", {
  # onsets either side of midnight: a linear mean would land at midday
  onsets <- c(23.5, 23.9, 0.2, 0.6, 23.7)
  res <- composite.phase.deviation(onsets, reference_phase = 0)
  expect_lt(res$precision, 1)   # they are tightly clustered, not 12h apart
  expect_lt(res$accuracy, 1)
})

# Minute aggregation: floor(n / max(1, epochs_per_min)) pinned the divisor at 1 above a
# 60 s epoch, so a 120 s recording reported half its duration.

test_that("circadian.rhythm runs on a 120s epoch and reports a whole day", {
  set.seed(42)
  n <- 720 * 7                                    # 7 days at 120s
  ts <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + (seq_len(n) - 1) * 120
  hr <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60
  counts <- pmax(0, 200 * (1 + sin(2 * pi * (hr - 10) / 24)) + rnorm(n, 0, 20))

  res <- circadian.rhythm(counts, ts, epoch_length = 120)

  expect_false(is.na(res$IS))
  expect_false(is.na(res$IV))
  expect_gte(res$M10, res$L5)
  # both are means of the same series, so both sit inside its range
  expect_lte(res$M10, max(counts))
  expect_gte(res$L5, min(counts))
  # the C++ path's minute series covers all seven days, so L5 and M10 match use_cpp = FALSE
  expect_identical(res$n_total_epochs, 7L * 1440L)
  ref <- circadian.rhythm(counts, ts, epoch_length = 120, use_cpp = FALSE)
  expect_equal(c(res$L5, res$M10), c(ref$L5, ref$M10))
})

test_that("Lomb-Scargle power is the least-squares variance explained", {
  set.seed(3)
  t <- sort(runif(500, 0, 14 * 24))
  y <- 2 * sin(2 * pi * t / 24.2) + rnorm(500, 0, 0.5)
  ts <- as.POSIXct("2025-01-01", tz = "UTC") + t * 3600

  res <- circadian.period(y, ts, ofac = 2)

  yc <- y - mean(y); tss <- sum(yc^2)
  oracle <- vapply(res$scanned, function(ph) {
    w <- 2 * pi / ph
    1 - sum(stats::.lm.fit(cbind(cos(w * t), sin(w * t)), yc)$residuals^2) / tss
  }, numeric(1))

  expect_equal(res$power, oracle, tolerance = 1e-9)
})
