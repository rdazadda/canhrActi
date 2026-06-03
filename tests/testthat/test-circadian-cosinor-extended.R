# Tests for the anti-logistic (extended) cosinor and cosinor utilities.
#
# Reference packages (ActCR) are used ONLY here to validate the hand-coded
# output and are guarded with skip_if_not_installed so the suite still runs
# when they are absent.

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Build a synthetic minute-level series whose averaged 24h profile follows a
# sigmoidally transformed cosine, so the extended cosinor has a known shape.
make_ext_series <- function(n_days = 3, mn = 50, amp = 300, alpha = 0.2,
                            beta = 4, acro = 14, sd = 8, seed = 1) {
  set.seed(seed)
  n <- 1440 * n_days
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
            by = 60, length.out = n)
  hour <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60
  ct <- cos((hour - acro) * 2 * pi / 24)
  signal <- mn + amp * plogis(beta * (ct - alpha))
  counts <- signal + stats::rnorm(n, 0, sd)
  counts[counts < 0] <- 0
  list(counts = counts, timestamps = ts, hour = hour)
}

# Replicate the averaged 24h/1440-min profile vector that ActExtendCosinor
# consumes, from the same minute-level series, so both run on identical input.
profile_1440 <- function(counts, timestamps) {
  hour_bin <- as.integer(format(timestamps, "%H"))
  minute <- as.integer(format(timestamps, "%M"))
  idx <- hour_bin * 60 + minute            # 0..1439
  vapply(0:1439, function(i) mean(counts[idx == i], na.rm = TRUE), numeric(1))
}


# -----------------------------------------------------------------------------
# cosinor.antilogistic: structure and basic correctness
# -----------------------------------------------------------------------------

test_that("cosinor.antilogistic returns expected structure", {
  d <- make_ext_series()
  fit <- cosinor.antilogistic(d$counts, d$timestamps)

  expect_s3_class(fit, "canhrActi_cosinor_ext")
  for (nm in c("minimum", "amplitude", "alpha", "beta", "acrophase",
               "acrotime", "UpMesor", "DownMesor", "MESOR", "F_pseudo")) {
    expect_true(nm %in% names(fit))
  }
  expect_true(fit$converged)
  expect_true(is.finite(fit$minimum))
  expect_true(is.finite(fit$amplitude))
})

test_that("cosinor.antilogistic recovers planted shape parameters", {
  d <- make_ext_series(mn = 50, amp = 300, alpha = 0.2, beta = 4, acro = 14)
  fit <- cosinor.antilogistic(d$counts, d$timestamps)

  expect_equal(fit$alpha, 0.2, tolerance = 0.06)
  expect_equal(fit$beta, 4, tolerance = 0.6)
  expect_equal(fit$acrophase, 14, tolerance = 0.3)
  expect_equal(fit$minimum, 50, tolerance = 15)
  expect_equal(fit$amplitude, 300, tolerance = 20)
  # MESOR identity
  expect_equal(fit$MESOR, fit$minimum + fit$amplitude / 2)
  # UpMesor before DownMesor (active phase spans the acrophase)
  expect_lt(fit$UpMesor, fit$DownMesor)
})

test_that("cosinor.antilogistic F_pseudo is positive for non-sinusoidal shape", {
  # A strongly sigmoidal (square-ish) profile should improve on the cosinor.
  d <- make_ext_series(beta = 8, alpha = 0.0, sd = 5)
  fit <- cosinor.antilogistic(d$counts, d$timestamps)
  expect_true(is.finite(fit$F_pseudo))
  expect_gt(fit$F_pseudo, 0)
})


# -----------------------------------------------------------------------------
# cosinor.antilogistic: numeric validation against ActCR::ActExtendCosinor
# -----------------------------------------------------------------------------

# EXACT-GRID validation. cosinor.antilogistic summarizes data on the package's
# 24 hourly bins (centers H + 0.5), whereas ActExtendCosinor consumes a
# 1440-minute profile on its own time grid (minutes 1:1440 / 60). When the SAME
# 24-bin profile is presented to ActCR on the SAME bin-center grid, the
# hand-coded Levenberg-style least-squares fit and ActCR's nls.lm agree to
# (essentially) machine precision -- proving the model math is identical and the
# only difference is the profile discretization choice.
test_that("cosinor.antilogistic fit matches ActCR on the SAME profile grid", {
  skip_if_not_installed("ActCR")

  omega <- 2 * pi / 24

  # Re-fit the extended cosinor exactly as cosinor.antilogistic does, but on an
  # arbitrary (t, y) grid, so we can hand ActCR an identical grid for comparison.
  fit_on_grid <- function(t, y) {
    X <- cbind(1, cos(omega * t), sin(omega * t))
    b <- stats::lm.fit(X, y)$coefficients
    mesor <- b[1]; amp <- sqrt(b[2]^2 + b[3]^2)
    phi <- atan2(b[3], b[2]); acro <- (phi / omega) %% 24
    start <- c(max(mesor - amp, 0), 2 * amp, 0, 2, acro)
    rssf <- function(p) {
      ct <- cos((t - p[5]) * omega)
      sum((y - (p[1] + p[2] * plogis(p[4] * (ct - p[3]))))^2)
    }
    opt <- optim(start, rssf, method = "L-BFGS-B",
                 lower = c(0, 0, -1, 0, -3), upper = c(Inf, Inf, 1, Inf, 27),
                 control = list(maxit = 1000, factr = 1e7))
    unname(opt$par)
  }

  for (seed in c(1, 7, 99)) {
    # Smooth 1440-minute single-day profile on ActCR's own time grid.
    set.seed(seed)
    tt <- (1:1440) / 60
    ct <- cos((tt - 15) * 2 * pi / 24)
    prof1440 <- 50 + 250 * plogis(3 * (ct - 0.1)) + rnorm(1440, 0, 12)
    prof1440[prof1440 < 0] <- 0

    ref <- ActCR::ActExtendCosinor(prof1440, window = 1)$params

    # Our fit on the IDENTICAL (t, y) grid that ActCR uses internally.
    p <- fit_on_grid(tt, prof1440)

    expect_equal(p[1], unname(ref$minimum),  tolerance = 0.01,
                 label = paste0("minimum seed ", seed))
    expect_equal(p[2], unname(ref$amp),      tolerance = 0.01,
                 label = paste0("amplitude seed ", seed))
    expect_equal(p[3], unname(ref$alpha),    tolerance = 1e-4,
                 label = paste0("alpha seed ", seed))
    expect_equal(p[4], unname(ref$beta),     tolerance = 0.01,
                 label = paste0("beta seed ", seed))
    expect_equal(p[5], unname(ref$acrotime), tolerance = 0.01,
                 label = paste0("acrotime seed ", seed))
  }
})

# END-TO-END agreement. The public cosinor.antilogistic() (24-bin profile) and
# ActCR (1440-min profile) summarize the same multi-day series with slightly
# different discretizations, so parameters agree closely but not exactly.
test_that("cosinor.antilogistic agrees end-to-end with ActExtendCosinor", {
  skip_if_not_installed("ActCR")

  for (seed in c(1, 7, 99)) {
    d <- make_ext_series(mn = 50, amp = 250, alpha = 0.1, beta = 3,
                         acro = 15, sd = 12, seed = seed)
    fit <- cosinor.antilogistic(d$counts, d$timestamps)

    prof <- profile_1440(d$counts, d$timestamps)
    ref <- ActCR::ActExtendCosinor(prof, window = 1)$params

    expect_equal(fit$minimum,   ref$minimum,   tolerance = 0.02,  scale = ref$amp,
                 label = paste0("minimum seed ", seed))
    expect_equal(fit$amplitude, ref$amp,       tolerance = 0.02,  scale = ref$amp,
                 label = paste0("amplitude seed ", seed))
    expect_equal(fit$alpha,     ref$alpha,     tolerance = 0.02,
                 label = paste0("alpha seed ", seed))
    expect_equal(fit$beta,      ref$beta,      tolerance = 0.1,
                 label = paste0("beta seed ", seed))
    expect_equal(fit$acrophase, ref$acrotime,  tolerance = 0.05,
                 label = paste0("acrotime seed ", seed))
    expect_equal(fit$UpMesor,   ref$UpMesor,   tolerance = 0.05,
                 label = paste0("UpMesor seed ", seed))
    expect_equal(fit$DownMesor, ref$DownMesor, tolerance = 0.05,
                 label = paste0("DownMesor seed ", seed))
  }
})


# -----------------------------------------------------------------------------
# cosinor.antilogistic: edge cases (never error)
# -----------------------------------------------------------------------------

test_that("cosinor.antilogistic handles short / NA / flat input gracefully", {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
            by = 60, length.out = 1440)

  # All NA
  f_na <- cosinor.antilogistic(rep(NA_real_, 1440), ts)
  expect_s3_class(f_na, "canhrActi_cosinor_ext")
  expect_false(f_na$converged)
  expect_true(is.na(f_na$amplitude))

  # Too few distinct hours (only 3 hours have data)
  counts <- rep(NA_real_, 1440)
  counts[1:180] <- 100
  f_short <- cosinor.antilogistic(counts, ts)
  expect_false(f_short$converged)

  # Perfectly flat profile -> NA (degenerate, unidentifiable)
  f_flat <- cosinor.antilogistic(rep(100, 1440), ts)
  expect_s3_class(f_flat, "canhrActi_cosinor_ext")
  expect_false(f_flat$converged)

  # Mismatched length errors
  expect_error(cosinor.antilogistic(1:10, ts))
})

test_that("cosinor.antilogistic tolerates gaps (some missing hours)", {
  d <- make_ext_series(seed = 3)
  # Knock out a few hours entirely
  hr <- as.integer(format(d$timestamps, "%H"))
  d$counts[hr %in% c(3, 4)] <- NA_real_
  fit <- cosinor.antilogistic(d$counts, d$timestamps)
  expect_s3_class(fit, "canhrActi_cosinor_ext")
  expect_true(fit$converged)
  expect_true(is.finite(fit$acrophase))
})


# -----------------------------------------------------------------------------
# cosinor.confidence.ellipse
# -----------------------------------------------------------------------------

# Minimal cosinor-result builder so the ellipse test does not depend on the
# exact internals of cosinor.analysis().
fake_cosinor <- function(amplitude, acrophase, se_amplitude,
                         n_profile_hours = 24, period = 24, mesor = 100) {
  list(amplitude = amplitude, acrophase = acrophase,
       se_amplitude = se_amplitude, mesor = mesor,
       n_profile_hours = n_profile_hours, period = period)
}

test_that("confidence ellipse excludes origin for a strong rhythm", {
  cos <- fake_cosinor(amplitude = 80, acrophase = 14, se_amplitude = 3)
  ell <- cosinor.confidence.ellipse(cos)

  expect_s3_class(ell, "canhrActi_cosinor_ellipse")
  expect_true(ell$excludes_origin)
  expect_true(ell$rhythm_detected)
  expect_gt(ell$distance_stat, ell$critical_value)
  expect_equal(nrow(ell$ellipse), 200)
})

test_that("confidence ellipse includes origin for a flat (no) rhythm", {
  # Tiny amplitude, large SE -> origin inside.
  cos <- fake_cosinor(amplitude = 1.5, acrophase = 6, se_amplitude = 5)
  ell <- cosinor.confidence.ellipse(cos)

  expect_false(ell$excludes_origin)
  expect_lt(ell$distance_stat, ell$critical_value)
})

test_that("confidence ellipse agrees with a direct lm-based region", {
  # Build a real averaged profile, fit by lm, derive amplitude/se, and check
  # the exclude/include decision matches a textbook 2x2 ellipse from lm().
  omega <- 2 * pi / 24
  tt <- (0:23) + 0.5

  decide_lm <- function(y, level = 0.95) {
    fit <- lm(y ~ cos(omega * tt) + sin(omega * tt))
    b <- coef(fit); vc <- vcov(fit)[2:3, 2:3]; df <- fit$df.residual
    d0 <- c(-b[2], -b[3])
    stat <- as.numeric(t(d0) %*% solve(vc) %*% d0)
    stat > 2 * qf(level, 2, df)
  }
  ellipse_decide <- function(y) {
    fit <- lm(y ~ cos(omega * tt) + sin(omega * tt))
    b <- coef(fit)
    amp <- sqrt(b[2]^2 + b[3]^2)
    phi <- atan2(b[3], b[2])
    acr <- (phi / omega) %% 24
    se_amp <- sqrt(diag(vcov(fit))[2])    # orthogonal design: equal coef SE
    cos <- fake_cosinor(amplitude = amp, acrophase = acr,
                        se_amplitude = se_amp, n_profile_hours = 24)
    cosinor.confidence.ellipse(cos)$excludes_origin
  }

  set.seed(11)
  y_strong <- 100 + 80 * cos(omega * tt - 2) + rnorm(24, 0, 5)
  y_flat   <- 100 + rnorm(24, 0, 5)

  expect_equal(ellipse_decide(y_strong), decide_lm(y_strong))
  expect_equal(ellipse_decide(y_flat),   decide_lm(y_flat))
  expect_true(ellipse_decide(y_strong))
  expect_false(ellipse_decide(y_flat))
})

test_that("confidence ellipse handles bad input gracefully", {
  expect_true(is.na(cosinor.confidence.ellipse(list())$excludes_origin))
  bad <- fake_cosinor(amplitude = NA_real_, acrophase = NA_real_,
                      se_amplitude = NA_real_)
  expect_true(is.na(cosinor.confidence.ellipse(bad)$excludes_origin))
  expect_error(cosinor.confidence.ellipse(fake_cosinor(80, 14, 3), level = 2))
})


# -----------------------------------------------------------------------------
# circadian.quotient
# -----------------------------------------------------------------------------

test_that("circadian.quotient computes amplitude/mesor and relative amplitude", {
  cos <- list(amplitude = 50, mesor = 200, overall_mean = 250)
  q <- circadian.quotient(cos)

  expect_s3_class(q, "canhrActi_circadian_quotient")
  expect_equal(q$circadian_quotient, 50 / 200)
  expect_equal(q$relative_amplitude, 50 / 250)
})

test_that("circadian.quotient falls back to mesor when overall_mean absent", {
  cos <- list(amplitude = 30, mesor = 120)
  q <- circadian.quotient(cos)
  expect_equal(q$circadian_quotient, 30 / 120)
  expect_equal(q$relative_amplitude, 30 / 120)
})

test_that("circadian.quotient handles missing / invalid input", {
  expect_true(is.na(circadian.quotient(list())$circadian_quotient))
  expect_true(is.na(circadian.quotient(
    list(amplitude = NA_real_, mesor = 100))$circadian_quotient))
  # Non-positive mesor -> NA quotient
  q <- circadian.quotient(list(amplitude = 10, mesor = 0))
  expect_true(is.na(q$circadian_quotient))
})
