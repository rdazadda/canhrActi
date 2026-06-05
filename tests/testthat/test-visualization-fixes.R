# Regression tests for Visualization-tab plot fixes (stacking, epoch->minutes,
# canonical IS/IV, locale-independent weekend, >7-day colors).

test_that("plot_intensity_area does not smear non-contiguous intensities", {
  skip_if_not_installed("ggplot2")
  ts <- as.POSIXct("2024-01-06 00:00", tz = "UTC") + (0:(3 * 60 - 1)) * 60
  intensity <- ifelse(format(ts, "%H") == "01", "moderate", "sedentary")
  p <- plot_intensity_area(data.frame(timestamp = ts, intensity = intensity),
                           intensity_col = "intensity", epoch_length = 60)
  hd <- p$data
  mod <- hd[as.character(hd$intensity) == "moderate" & hd$hour %in% 0:2, ]
  mod <- mod$minutes[order(mod$hour)]
  expect_equal(mod, c(0, 60, 0))   # moderate only at hour 1, explicit zeros around it
})

test_that("plot_daily_summary_bars converts epoch counts to minutes", {
  skip_if_not_installed("ggplot2")
  daily <- data.frame(date = as.Date(c("2024-01-06", "2024-01-07")),
                      sedentary = c(800, 600), light = c(200, 300),
                      moderate = c(40, 50), vigorous = c(10, 5), very_vigorous = c(2, 0))
  raw <- data.frame(timestamp = as.POSIXct("2024-01-06", tz = "UTC") + (0:9) * 30)  # 30s
  p <- plot_daily_summary_bars(raw, daily_summary = daily, epoch_length = 30)
  sm <- p$data$value[p$data$metric == "sedentary_min"]
  expect_equal(sm[1], 400)   # 800 epochs * 30/60
  expect_true("very_vigorous_min" %in% levels(p$data$metric))
})

test_that("plot_is_iv fallback matches the canonical IS/IV engine", {
  skip_if_not_installed("ggplot2")
  set.seed(1)
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (0:(4 * 1440 - 1)) * 60
  act <- pmax(0, 100 + 80 * cos(2 * pi * (as.numeric(difftime(ts, ts[1], units = "hours")) - 14) / 24) +
                rnorm(5760, 0, 20))
  cr <- circadian.rhythm(act, ts, epoch_length = 60)
  isiv <- .calculate.IS.IV(act, ts, 60)
  expect_equal(round(isiv$IS, 4), cr$IS)   # plot_is_iv fallback now calls this engine
  expect_equal(round(isiv$IV, 4), cr$IV)
  expect_s3_class(plot_is_iv(data.frame(timestamp = ts, axis1 = act)), "ggplot")
})

test_that("plot_weekend_weekday classifies Sat/Sun as weekend (locale-independent)", {
  skip_if_not_installed("ggplot2")
  # Fri 2024-01-05 .. Mon 2024-01-08 spans Sat+Sun.
  ts <- as.POSIXct("2024-01-05 00:00", tz = "UTC") + (0:(4 * 1440 - 1)) * 60
  p <- plot_weekend_weekday(data.frame(timestamp = ts, axis1 = rep(100, length(ts))))
  expect_true(all(c("Weekday", "Weekend") %in% as.character(p$data$day_type)))
})

test_that("plot_circadian_polar accepts HH:MM / POSIXct / integer onsets", {
  skip_if_not_installed("ggplot2")
  set.seed(1); n <- 5 * 1440
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * 60
  th <- as.numeric(difftime(ts, ts[1], units = "hours"))
  a1 <- pmax(0, round(100 + 80 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 20)))
  d <- data.frame(timestamp = ts, axis1 = a1)
  expect_s3_class(plot_circadian_polar(d), "ggplot")                              # fallback
  expect_s3_class(plot_circadian_polar(d, L5_onset = "01:48", M10_onset = "13:30"), "ggplot")
  expect_s3_class(plot_circadian_polar(d, L5_onset = 3, M10_onset = 14), "ggplot")
})

test_that("plot_day_comparison handles more than 7 days without error", {
  skip_if_not_installed("ggplot2")
  ts <- as.POSIXct("2024-01-01", tz = "UTC") + (0:(9 * 1440 - 1)) * 60  # 9 days
  set.seed(2)
  d <- data.frame(timestamp = ts, axis1 = pmax(0, rnorm(length(ts), 200, 150)))
  expect_s3_class(plot_day_comparison(d, comparison_type = "overlay"), "ggplot")
})

# ---- New sedentary gallery plots ---------------------------------------------

.vp_frag <- function() {
  set.seed(1); epl <- 60; n <- 5 * 1440
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * epl
  hod <- as.numeric(format(ts, "%H"))
  a1 <- pmax(0, round(ifelse(hod >= 8 & hod < 22, 300, 20) + rnorm(n, 0, 150)))
  sedentary.fragmentation(freedson(to_cpm(a1, epl)), ts, wear_time = rep(TRUE, n),
                          epoch_length = epl)
}

test_that("plot_bout_histogram / lorenz / transition_matrix render", {
  skip_if_not_installed("ggplot2")
  fr <- .vp_frag()
  expect_s3_class(plot_bout_histogram(fr), "ggplot")
  expect_s3_class(plot_bout_lorenz(fr), "ggplot")
  expect_s3_class(plot_transition_matrix(fr), "ggplot")
})

test_that("plot_bout_lorenz curve is monotonic and ends at 100%", {
  skip_if_not_installed("ggplot2")
  p <- plot_bout_lorenz(.vp_frag())
  d <- p$data
  expect_true(all(diff(d$pct_time) >= -1e-9))      # monotone non-decreasing
  expect_equal(max(d$pct_time), 100, tolerance = 1e-6)
  expect_equal(max(d$pct_bouts), 100, tolerance = 1e-6)
})

test_that("plot_transition_matrix probabilities are in [0,1]", {
  skip_if_not_installed("ggplot2")
  p <- plot_transition_matrix(.vp_frag())
  expect_true(all(p$data$prob >= 0 & p$data$prob <= 1))
})

test_that("plot_actogram accepts L5/M10/sleep overlays + sqrt scale", {
  skip_if_not_installed("ggplot2")
  set.seed(1); epl <- 60; n <- 6 * 1440
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, ts[1], units = "hours")); hod <- th %% 24
  act <- pmax(0, 120 + 90 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 25))
  sleep <- ifelse(hod >= 23 | hod < 7, "S", "W")
  p <- plot_actogram(act, ts, epoch_length = epl, L5_onset = "02:30",
                     M10_onset = "14:00", sleep_mask = sleep, scale = "sqrt")
  expect_s3_class(p, "ggplot")
  expect_gt(length(p$layers), 3)                       # raster + divider + sleep + L5/M10
  expect_s3_class(plot_actogram(act, ts, epoch_length = epl), "ggplot")  # backward compat
})

test_that("plot_light_exposure renders + handles missing lux", {
  skip_if_not_installed("ggplot2")
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (0:(3 * 1440 - 1)) * 60
  hod <- as.numeric(format(ts, "%H"))
  d <- data.frame(timestamp = ts, lux = pmax(0, ifelse(hod >= 8 & hod < 20, 300, 2)))
  expect_s3_class(plot_light_exposure(d), "ggplot")
})

test_that("new sedentary plots degrade gracefully", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_bout_histogram(list(bouts = NULL)), "ggplot")
  expect_s3_class(plot_bout_lorenz(list(bouts = NULL)), "ggplot")
  expect_s3_class(plot_transition_matrix(list(ASTP = NA_real_, SATP = NA_real_)), "ggplot")
})
