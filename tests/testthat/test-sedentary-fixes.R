# Sedentary-tab correctness fixes: Weibull/SRI wiring, pooled bout.distribution
# metrics, transition.probabilities alignment, and daily-breakdown bridging.

.sf_data <- function(days = 5, epl = 60, seed = 1) {
  n <- days * 86400 / epl
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (seq_len(n) - 1) * epl
  hod <- as.numeric(format(ts, "%H"))
  set.seed(seed)
  base <- ifelse(hod >= 8 & hod < 22, 300, 20)        # active days, quiet nights
  counts <- pmax(0, base + rnorm(n, 0, 150))
  list(ts = ts, intensity = freedson(counts), epl = epl, wear = rep(TRUE, n))
}

test_that("sedentary.fragmentation surfaces Weibull shape + sedentary SRI", {
  d <- .sf_data()
  fr <- sedentary.fragmentation(d$intensity, d$ts, wear_time = d$wear, epoch_length = d$epl)
  expect_true(all(c("weibull_shape", "weibull_scale", "weibull_hazard",
                    "sedentary_regularity_index") %in% names(fr)))
  expect_true(is.finite(fr$weibull_shape))
  expect_true(is.character(fr$weibull_hazard))
  expect_true(is.finite(fr$sedentary_regularity_index))
  expect_true(fr$sedentary_regularity_index >= -100 && fr$sedentary_regularity_index <= 100)
})

test_that("bout.distribution.metrics reproduces the headline distribution metrics", {
  d <- .sf_data()
  fr <- sedentary.fragmentation(d$intensity, d$ts, wear_time = d$wear, epoch_length = d$epl)
  bdm <- bout.distribution.metrics(fr$bouts$duration_min, epoch_length = d$epl)
  expect_equal(bdm$alpha, fr$alpha, tolerance = 1e-6)
  expect_equal(bdm$gini, fr$gini, tolerance = 1e-6)
  expect_equal(bdm$W50, fr$W50, tolerance = 1e-6)
  expect_equal(bdm$SATP, fr$SATP, tolerance = 1e-6)
  expect_equal(bdm$n_bouts, fr$total_bouts)
})

test_that("bout.distribution.metrics degrades gracefully on empty input", {
  out <- bout.distribution.metrics(numeric(0))
  expect_equal(out$n_bouts, 0)
  expect_true(is.na(out$alpha))
  expect_true(is.na(out$W50))
})

test_that("transition.probabilities (bridged) matches headline ASTP/SATP", {
  d <- .sf_data()
  fr <- sedentary.fragmentation(d$intensity, d$ts, wear_time = d$wear,
                                epoch_length = d$epl, min_break_length = 5)
  tp <- transition.probabilities(d$intensity, wear_time = d$wear,
                                 min_break_length = 5, epoch_length = d$epl)
  expect_equal(tp$ASTP, fr$ASTP, tolerance = 1e-6)
  expect_equal(tp$SATP, fr$SATP, tolerance = 1e-6)
})

test_that("transition.probabilities matches the headline at sub-minute epochs", {
  d <- .sf_data(epl = 30)
  fr <- sedentary.fragmentation(d$intensity, d$ts, wear_time = d$wear,
                                epoch_length = 30, min_break_length = 5)
  tp <- transition.probabilities(d$intensity, wear_time = d$wear,
                                 min_break_length = 5, epoch_length = 30)
  expect_equal(tp$SATP, fr$SATP, tolerance = 1e-6)
  expect_equal(tp$ASTP, fr$ASTP, tolerance = 1e-6)
})

test_that("a no-bout result still carries the Weibull/SRI keys", {
  ts <- as.POSIXct("2024-01-06", tz = "UTC") + (0:1439) * 60
  intensity <- rep("light", 1440)          # no sedentary -> empty-result path
  fr <- sedentary.fragmentation(intensity, ts, epoch_length = 60)
  expect_true(all(c("weibull_shape", "weibull_scale", "weibull_hazard",
                    "sedentary_regularity_index") %in% names(fr)))
})

test_that("daily fragmentation honours the bridging parameter", {
  d <- .sf_data()
  fr5 <- sedentary.fragmentation(d$intensity, d$ts, wear_time = d$wear,
                                 epoch_length = d$epl, min_break_length = 5)
  fr1 <- sedentary.fragmentation(d$intensity, d$ts, wear_time = d$wear,
                                 epoch_length = d$epl, min_break_length = 1)
  # Bridging short active gaps yields fewer (longer) per-day bouts.
  expect_lte(sum(fr5$daily_fragmentation$n_bouts, na.rm = TRUE),
             sum(fr1$daily_fragmentation$n_bouts, na.rm = TRUE))
  # And the per-day breakdown is no longer fixed at the unbridged default.
  expect_false(isTRUE(all.equal(fr5$daily_fragmentation$n_bouts,
                                fr1$daily_fragmentation$n_bouts)))
})
