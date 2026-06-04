# Social jet lag (sleep.tudor.locke -> social.jet.lag pipeline, as wired in the
# dashboard) and per-day intradaily variability in daily_metrics.

# ---- Per-day IV --------------------------------------------------------------

.di_signal <- function(days = 7, epl = 60, seed = 1) {
  n <- days * 86400 / epl
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, ts[1], units = "hours"))
  set.seed(seed)
  list(ts = ts, act = pmax(0, 100 + 80 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 20)),
       epl = epl)
}

test_that("daily_metrics carries a per-day IV column", {
  d <- .di_signal()
  res <- circadian.rhythm(d$act, d$ts, epoch_length = d$epl)
  expect_true("IV" %in% names(res$daily_metrics))
  iv <- res$daily_metrics$IV
  full <- iv[is.finite(iv)]
  expect_gt(length(full), 0)
  expect_true(all(full >= 0))
})

test_that("per-day IV tracks the recording-level IV for a stationary rhythm", {
  d <- .di_signal(seed = 4)
  res <- circadian.rhythm(d$act, d$ts, epoch_length = d$epl)
  iv <- res$daily_metrics$IV
  iv <- iv[is.finite(iv)]
  # Each day's IV should sit near the whole-recording IV (no day-to-day drift).
  expect_lt(abs(mean(iv) - res$IV), 0.05)
})

test_that("a more fragmented rhythm yields a larger per-day IV", {
  smooth <- .di_signal(seed = 2)
  rs <- circadian.rhythm(smooth$act, smooth$ts, epoch_length = smooth$epl)
  # High-frequency noise on top of the rhythm fragments it.
  set.seed(3)
  frag_act <- smooth$act + rnorm(length(smooth$act), 0, 120) * (seq_along(smooth$act) %% 2)
  frag_act <- pmax(0, frag_act)
  rf <- circadian.rhythm(frag_act, smooth$ts, epoch_length = smooth$epl)
  expect_gt(mean(rf$daily_metrics$IV, na.rm = TRUE),
            mean(rs$daily_metrics$IV, na.rm = TRUE))
})

# ---- Social jet lag ----------------------------------------------------------

test_that("sleep.tudor.locke -> social.jet.lag runs on a scored sleep series", {
  epl <- 60; n <- 14 * 1440
  ts <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (seq_len(n) - 1) * epl
  h <- as.numeric(format(ts, "%H"))
  sleep_state <- ifelse(h >= 23 | h < 7, "S", "W")
  sp <- sleep.tudor.locke(sleep.state = sleep_state, timestamps = ts, epoch_length = epl)
  expect_true(is.data.frame(sp))
  expect_true(all(c("in_bed_time", "out_bed_time") %in% names(sp)))
  expect_gt(nrow(sp), 5)

  sjl <- social.jet.lag(sp)
  expect_true(is.finite(sjl$social_jet_lag_hours))
  expect_true(is.finite(sjl$MSW) && is.finite(sjl$MSF))
  expect_gt(sjl$n_work_nights, 0)
  expect_gt(sjl$n_free_nights, 0)
  # Identical weekday/weekend schedule -> essentially no social jet lag.
  expect_lt(abs(sjl$social_jet_lag_hours), 0.5)
})

test_that("recovers a known weekend sleep delay", {
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-01-14"), by = "day")
  wknd <- weekdays(dates) %in% c("Saturday", "Sunday")
  in_bed  <- ifelse(wknd, paste(dates, "01:00:00"), paste(dates, "23:00:00"))
  out_bed <- ifelse(wknd, paste(dates, "09:00:00"), paste(dates + 1, "07:00:00"))
  sp <- data.frame(in_bed_time = in_bed, out_bed_time = out_bed, stringsAsFactors = FALSE)

  sjl <- social.jet.lag(sp)
  expect_equal(sjl$MSW, 3, tolerance = 0.1)   # weekday mid-sleep 03:00
  expect_equal(sjl$MSF, 5, tolerance = 0.1)   # weekend mid-sleep 05:00
  expect_equal(sjl$social_jet_lag_hours, 2, tolerance = 0.1)
  expect_equal(sjl$social_jet_lag_min, 120, tolerance = 6)
})

test_that("degrades gracefully with no sleep periods", {
  sp <- data.frame(in_bed_time = character(0), out_bed_time = character(0),
                   stringsAsFactors = FALSE)
  sjl <- social.jet.lag(sp)
  expect_true(is.na(sjl$social_jet_lag_hours))
})
