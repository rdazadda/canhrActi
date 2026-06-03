# Tests for circadian visualization functions (R/circadian_plots.R).
#
# Each function must return a ggplot object on valid data, return an annotated
# (still-ggplot) empty plot on degenerate / insufficient input rather than
# erroring, and produce plots that build cleanly. The periodogram test is
# guarded with skip_if_not_installed("lomb") because it depends on lomb::lsp.

skip_if_not_installed("ggplot2")


# --- Shared synthetic fixture: 7 days of minute data, clear 24h rhythm -------
make_circadian_data <- function(days = 7, seed = 42, noise = 15) {
  set.seed(seed)
  n <- 1440L * days
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
            by = 60, length.out = n)
  hour <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60
  counts <- pmax(0, 100 + 80 * cos(2 * pi * (hour - 8) / 24) +
                   rnorm(n, 0, noise))
  list(counts = counts, timestamps = ts, hour = hour, n = n)
}


# =============================================================================
# plot_periodogram()
# =============================================================================
test_that("plot_periodogram returns a ggplot on valid data", {
  skip_if_not_installed("lomb")
  d <- make_circadian_data()

  expect_no_error(p <- plot_periodogram(d$counts, d$timestamps))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_periodogram degrades gracefully on insufficient data", {
  # All-NA: must NOT error, must still be a ggplot.
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
            by = 60, length.out = 120)
  expect_s3_class(plot_periodogram(rep(NA_real_, 120), ts), "ggplot")

  # < 2 days of span (constant short series).
  expect_s3_class(plot_periodogram(rep(50, 120), ts), "ggplot")

  # Length mismatch.
  expect_s3_class(plot_periodogram(1:10, ts), "ggplot")
})


# =============================================================================
# plot_extended_cosinor()
# =============================================================================
test_that("plot_extended_cosinor returns a ggplot on valid data", {
  d <- make_circadian_data()

  expect_no_error(p <- plot_extended_cosinor(d$counts, d$timestamps))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_extended_cosinor degrades gracefully on insufficient data", {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
            by = 60, length.out = 120)
  expect_s3_class(plot_extended_cosinor(rep(NA_real_, 120), ts), "ggplot")
  expect_s3_class(plot_extended_cosinor(rep(50, 120), ts), "ggplot")
})

test_that("plot_extended_cosinor handles a degenerate extended fit", {
  # Constant counts across days -> extended fit cannot converge meaningfully,
  # but the function must still return a (buildable) ggplot.
  n <- 1440L * 3L
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
            by = 60, length.out = n)
  p <- plot_extended_cosinor(rep(10, n), ts)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})


# =============================================================================
# plot_dfa()
# =============================================================================
test_that("plot_dfa returns a ggplot on valid data", {
  d <- make_circadian_data()

  expect_no_error(p <- plot_dfa(d$counts))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_dfa returns a ggplot on a long random-walk series", {
  set.seed(1)
  p <- plot_dfa(cumsum(rnorm(5000)))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_dfa degrades gracefully on insufficient data", {
  expect_s3_class(plot_dfa(rep(NA_real_, 120)), "ggplot")  # all-NA
  expect_s3_class(plot_dfa(c(1, 2, 3)), "ggplot")          # too short
  expect_s3_class(plot_dfa(rep(5, 5000)), "ggplot")        # constant
})
