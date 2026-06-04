# Tests for circadian.period() — endogenous circadian PERIOD (tau) estimation
# via the Lomb-Scargle periodogram (handles irregular / gappy sampling).

# --- helpers ----------------------------------------------------------------

# Synthesize n_days of minute-level data with a cosine rhythm of the given
# period (hours). Returns a list(counts, timestamps, t_hours).
.make_rhythm <- function(period_h = 24, n_days = 7, amp = 80, mesor = 100,
                         noise_sd = 5, phase_h = 8, seed = 1) {
  set.seed(seed)
  t_hours <- seq(0, n_days * 24 - 1 / 60, by = 1 / 60)  # minute spacing
  n <- length(t_hours)
  counts <- mesor + amp * cos(2 * pi * (t_hours - phase_h) / period_h) +
    stats::rnorm(n, 0, noise_sd)
  ts <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + t_hours * 3600
  list(counts = counts, timestamps = ts, t_hours = t_hours)
}


# --- structure / contract ---------------------------------------------------

test_that("circadian.period returns the documented list structure", {
  d <- .make_rhythm(period_h = 24)
  res <- circadian.period(d$counts, d$timestamps)

  expect_type(res, "list")
  expect_setequal(
    names(res),
    c("tau", "peak_power", "p_value", "oversampling", "n_used", "span_days",
      "scanned", "power")
  )
  expect_equal(res$oversampling, 4)
  expect_true(is.numeric(res$tau))
})


# --- 24h recovery -----------------------------------------------------------

test_that("circadian.period recovers tau ~= 24 from a 24h rhythm", {
  d <- .make_rhythm(period_h = 24)
  res <- circadian.period(d$counts, d$timestamps)

  expect_true(is.finite(res$tau))
  expect_equal(res$tau, 24, tolerance = 0.5)
  expect_true(res$peak_power > 0)
  expect_true(res$p_value < 0.05)  # strong rhythm -> highly significant
})


# --- 25.5h forced period recovery -------------------------------------------

test_that("circadian.period recovers tau ~= 25.5 from a forced 25.5h rhythm", {
  d <- .make_rhythm(period_h = 25.5, seed = 2)
  res <- circadian.period(d$counts, d$timestamps)

  expect_true(is.finite(res$tau))
  # ofac = 4 gives a coarse period grid; allow ~0.5 h of grid resolution slack.
  expect_equal(res$tau, 25.5, tolerance = 0.5)
})


# --- matches a direct lomb::lsp call ----------------------------------------

test_that("circadian.period matches a direct lomb::lsp call (24h and 25.5h)", {
  testthat::skip_if_not_installed("lomb")

  for (per in c(24, 25.5)) {
    d <- .make_rhythm(period_h = per, seed = if (per == 24) 1 else 2)
    res <- circadian.period(d$counts, d$timestamps, ofac = 4)

    direct <- lomb::lsp(
      x = d$counts, times = d$t_hours,
      from = 18, to = 30, type = "period", ofac = 4, plot = FALSE
    )

    expect_equal(res$tau, as.numeric(direct$peak.at[1]), tolerance = 1e-8)
    expect_equal(res$peak_power, as.numeric(direct$peak), tolerance = 1e-8)
    expect_equal(res$p_value, as.numeric(direct$p.value), tolerance = 1e-8)
  }
})


# --- robust to gaps / NA (the whole point of Lomb-Scargle) ------------------

test_that("circadian.period still recovers tau ~= 24 with NA gaps", {
  d <- .make_rhythm(period_h = 24, seed = 3)
  cn <- d$counts
  # Punch out a multi-hour 'non-wear' gap each day to make sampling irregular.
  set.seed(99)
  gap_idx <- as.logical(rbinom(length(cn), 1, 0.2))
  cn[gap_idx] <- NA

  res <- circadian.period(cn, d$timestamps)
  expect_true(is.finite(res$tau))
  expect_equal(res$tau, 24, tolerance = 0.6)
  expect_true(res$n_used < length(cn))  # NAs were dropped
})


# --- edge cases never error -------------------------------------------------

test_that("circadian.period returns NA structure on short series (< 2 days)", {
  t_hours <- seq(0, 12, by = 1 / 60)  # only 12 hours
  ts <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + t_hours * 3600
  counts <- 100 + 80 * cos(2 * pi * t_hours / 24)

  expect_silent(res <- circadian.period(counts, ts))
  expect_true(is.na(res$tau))
  expect_true(is.na(res$peak_power))
  expect_true(is.na(res$p_value))
  expect_equal(res$oversampling, 4)
})

test_that("circadian.period returns NA structure on too few points", {
  ts <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (0:4) * 3 * 24 * 3600
  counts <- c(1, 2, 3, 4, 5)  # spans many days but only 5 points (< 10)

  expect_silent(res <- circadian.period(counts, ts))
  expect_true(is.na(res$tau))
  expect_equal(res$n_used, 5L)
})

test_that("circadian.period handles all-NA, empty, and mismatched input", {
  d <- .make_rhythm(period_h = 24)

  # all NA counts
  expect_silent(r1 <- circadian.period(rep(NA_real_, length(d$counts)), d$timestamps))
  expect_true(is.na(r1$tau))

  # empty
  expect_silent(r2 <- circadian.period(numeric(0), d$timestamps[0]))
  expect_true(is.na(r2$tau))

  # length mismatch
  expect_silent(r3 <- circadian.period(d$counts[1:10], d$timestamps[1:5]))
  expect_true(is.na(r3$tau))
})

test_that("circadian.period handles a constant (degenerate) series", {
  d <- .make_rhythm(period_h = 24)
  expect_silent(res <- circadian.period(rep(42, length(d$counts)), d$timestamps))
  expect_true(is.na(res$tau))
})


# --- oversampling passthrough -----------------------------------------------

test_that("circadian.period passes ofac through to oversampling and lsp", {
  d <- .make_rhythm(period_h = 25.5, seed = 2)
  res8 <- circadian.period(d$counts, d$timestamps, ofac = 8)
  expect_equal(res8$oversampling, 8)
  # higher oversampling -> finer grid, should land at least as close to 25.5
  expect_equal(res8$tau, 25.5, tolerance = 0.5)
})


# --- cross-check vs pracma / nonlinearTseries where available ---------------

test_that("Lomb-Scargle peak agrees with an independent spectral check", {
  # pracma::findpeaks on a manually-built Lomb-Scargle spectrum is overkill;
  # instead cross-validate the dominant period against a coarse FFT on the
  # (here regularly-sampled) clean signal as an independent sanity check.
  testthat::skip_if_not_installed("lomb")

  d <- .make_rhythm(period_h = 24, noise_sd = 0, seed = 7)
  res <- circadian.period(d$counts, d$timestamps)

  # FFT on the regular minute grid: dominant non-DC frequency -> period.
  x <- d$counts - mean(d$counts)
  spec <- Mod(stats::fft(x))^2
  nfreq <- length(x)
  # frequencies in cycles per hour: index k -> k/(N * dt_hours)
  dt_h <- 1 / 60
  k <- 1:(floor(nfreq / 2))
  freq_cph <- k / (nfreq * dt_h)
  period_h <- 1 / freq_cph
  in_band <- which(period_h >= 18 & period_h <= 30)
  fft_tau <- period_h[in_band][which.max(spec[k][in_band])]

  expect_equal(res$tau, fft_tau, tolerance = 1.0)
})

test_that("circadian.period exposes the full Lomb-Scargle spectrum", {
  th <- seq(0, 7 * 24 - 1 / 60, by = 1 / 60)
  ts <- as.POSIXct("2024-01-01") + th * 3600
  set.seed(1)
  counts <- 100 + 80 * cos(2 * pi * (th - 8) / 24) + rnorm(length(th), 0, 5)
  r <- circadian.period(counts, ts)

  expect_true(is.numeric(r$scanned) && is.numeric(r$power))
  expect_equal(length(r$scanned), length(r$power))
  expect_gt(length(r$scanned), 1)
  # tau is the period at the spectral peak (spectrum is consistent with scalar)
  expect_equal(r$tau, r$scanned[which.max(r$power)])

  # NA-branch (too short) returns an empty spectrum but a stable shape
  r2 <- circadian.period(counts[1:1440], ts[1:1440])
  expect_equal(length(r2$scanned), 0)
  expect_equal(length(r2$power), 0)
})
