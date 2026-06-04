# Double-plotted actogram plot + valid-day (includedaycrit) gate now shared by
# circadian.rhythm / cosinor.analysis / cosinor.extended.

.ag_signal <- function(days = 6, epl = 60, seed = 1) {
  n <- days * 86400 / epl
  ts <- as.POSIXct("2024-01-03 00:00:00", tz = "UTC") + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, ts[1], units = "hours"))
  set.seed(seed)
  list(ts = ts, act = pmax(0, 120 + 90 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 20)),
       epl = epl)
}

# ---- Actogram ----------------------------------------------------------------

test_that("plot_actogram returns a ggplot raster for valid data", {
  skip_if_not_installed("ggplot2")
  d <- .ag_signal()
  p <- plot_actogram(d$act, d$ts, epoch_length = d$epl)
  expect_s3_class(p, "ggplot")
  expect_gt(length(p$layers), 0)
  # Double plot spans ~48 h.
  expect_gt(max(p$data$x_h, na.rm = TRUE), 24)
})

test_that("single plot spans 24 h, double spans 48 h", {
  skip_if_not_installed("ggplot2")
  d <- .ag_signal()
  ps <- plot_actogram(d$act, d$ts, epoch_length = d$epl, double_plot = FALSE)
  pd <- plot_actogram(d$act, d$ts, epoch_length = d$epl, double_plot = TRUE)
  expect_lt(max(ps$data$x_h, na.rm = TRUE), 24)
  expect_gt(max(pd$data$x_h, na.rm = TRUE), 24)
})

test_that("wear_time blanks non-wear epochs", {
  skip_if_not_installed("ggplot2")
  d <- .ag_signal()
  wear <- rep(TRUE, length(d$act))
  wear[(3 * 1440):(3 * 1440 + 600)] <- FALSE
  p_on  <- plot_actogram(d$act, d$ts, epoch_length = d$epl, wear_time = wear)
  p_off <- plot_actogram(d$act, d$ts, epoch_length = d$epl)
  expect_gt(sum(is.na(p_on$data$value)), sum(is.na(p_off$data$value)))
})

test_that("plot_actogram never errors on degenerate input", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_actogram(numeric(0), as.POSIXct(character(0))), "ggplot")
  d <- .ag_signal()
  expect_s3_class(plot_actogram(d$act[1:5], d$ts[1:5], epoch_length = d$epl), "ggplot")
})

# ---- Cosinor valid-day gate --------------------------------------------------

.gate_data <- function(epl = 60) {
  n <- 3 * 1440
  ts <- as.POSIXct("2024-01-06 00:00:00", tz = "UTC") + (seq_len(n) - 1) * epl
  th <- as.numeric(difftime(ts, ts[1], units = "hours"))
  set.seed(1)
  counts <- pmax(0, 100 + 60 * cos(2 * pi * (th - 14) / 24) + rnorm(n, 0, 10))
  d2 <- 1441:2880
  counts[d2] <- counts[d2] + 400          # contaminate day 2
  wear <- rep(TRUE, n)
  wear[d2[361:length(d2)]] <- FALSE        # day 2 = 6 h wear (< 10 h)
  list(ts = ts, counts = counts, wear = wear, d2 = d2)
}

test_that("cosinor.analysis honours the valid-day gate by default", {
  g <- .gate_data()
  on  <- cosinor.analysis(g$counts, g$ts, wear_time = g$wear)                    # default 10 h
  off <- cosinor.analysis(g$counts, g$ts, wear_time = g$wear, min_valid_hours = 0)
  # Reference: days 1 & 3 only (the contaminated low-wear day removed entirely).
  keep <- as.Date(g$ts) != as.Date(g$ts[g$d2[1]])
  ref <- cosinor.analysis(g$counts[keep], g$ts[keep])
  expect_false(isTRUE(all.equal(as.numeric(on$mesor), as.numeric(off$mesor))))
  expect_lt(abs(as.numeric(on$mesor) - as.numeric(ref$mesor)), 2)   # gate ~ clean ref
  expect_gt(as.numeric(off$mesor), as.numeric(on$mesor))           # opt-out pulled up
})

test_that("cosinor.extended honours the valid-day gate by default", {
  g <- .gate_data()
  on  <- cosinor.extended(g$counts, g$ts, wear_time = g$wear)
  off <- cosinor.extended(g$counts, g$ts, wear_time = g$wear, min_valid_hours = 0)
  expect_false(isTRUE(all.equal(as.numeric(on$mesor), as.numeric(off$mesor))))
})

test_that("gate is a no-op without a wear mask (backward compatible)", {
  g <- .gate_data()
  a <- cosinor.analysis(g$counts, g$ts)
  b <- cosinor.analysis(g$counts, g$ts, min_valid_hours = 10)
  expect_equal(as.numeric(a$mesor), as.numeric(b$mesor))
  expect_equal(as.numeric(a$amplitude), as.numeric(b$amplitude))
})

# ---- Chi-square periodogram plot ---------------------------------------------

test_that("plot_chisq returns a ggplot with Qp + significance lines", {
  skip_if_not_installed("ggplot2")
  d <- .ag_signal()
  p <- plot_chisq(d$act, d$ts, epoch_length = d$epl)
  expect_s3_class(p, "ggplot")
  expect_gt(length(p$layers), 1)   # Qp curve + critical threshold at minimum
})

test_that("plot_chisq flags a strong rhythm significant", {
  skip_if_not_installed("ggplot2")
  d <- .ag_signal()
  p <- plot_chisq(d$act, d$ts, epoch_length = d$epl)
  expect_match(p$labels$title, "\\(significant,")
})

test_that("plot_chisq never errors on degenerate input", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_chisq(numeric(0), as.POSIXct(character(0))), "ggplot")
  d <- .ag_signal()
  expect_s3_class(plot_chisq(d$act[1:5], d$ts[1:5], epoch_length = d$epl), "ggplot")
})
