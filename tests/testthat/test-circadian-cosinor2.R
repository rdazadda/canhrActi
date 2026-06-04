# Cross-validation of the single-component cosinor against cosinor::cosinor.lm
# (the fitting engine behind the cosinor2 package). Both estimate
# Y = MESOR + amplitude * cos(2*pi*t/period - acrophase).

test_that("single cosinor MESOR / amplitude / acrophase match cosinor.lm and ground truth", {
  skip_if_not_installed("cosinor")

  epl <- 60
  n <- 7 * 1440
  t0 <- as.POSIXct("2024-01-06 00:00:00", tz = "UTC")
  ts <- t0 + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, t0, units = "hours"))
  set.seed(1)
  # Ground truth: MESOR 100, amplitude 80, acrophase (peak) at 14:00.
  act <- 100 + 80 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 3)

  ca <- cosinor.analysis(act, ts)

  clm <- cosinor::cosinor.lm(Y ~ time(time), period = 24,
                             data = data.frame(time = th %% 24, Y = act))
  tt <- summary(clm)$transformed.table

  # Agreement with cosinor.lm (canhrActi fits the averaged 24 h profile, so the
  # amplitude differs by a fraction of a percent from the raw-series fit).
  expect_equal(as.numeric(ca$mesor),     as.numeric(tt["(Intercept)", "estimate"]), tolerance = 0.02)
  expect_equal(as.numeric(ca$amplitude), as.numeric(tt["amp", "estimate"]),         tolerance = 0.02)

  # Recovery of the known parameters.
  expect_equal(as.numeric(ca$mesor),     100, tolerance = 0.01)
  expect_equal(as.numeric(ca$amplitude), 80,  tolerance = 0.01)
  expect_lt(abs(as.numeric(ca$acrophase) - 14), 0.2)
})

test_that("cosinor recovers a shifted acrophase", {
  skip_if_not_installed("cosinor")

  epl <- 60
  n <- 7 * 1440
  t0 <- as.POSIXct("2024-01-06 00:00:00", tz = "UTC")
  ts <- t0 + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, t0, units = "hours"))
  set.seed(7)
  act <- 50 + 40 * cos(2 * pi * (th - 6) / 24) + rnorm(n, 0, 3)

  ca <- cosinor.analysis(act, ts)
  expect_equal(as.numeric(ca$mesor),     50, tolerance = 0.02)
  expect_equal(as.numeric(ca$amplitude), 40, tolerance = 0.02)
  expect_lt(abs(as.numeric(ca$acrophase) - 6), 0.2)
})
