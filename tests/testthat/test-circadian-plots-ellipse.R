# plot_cosinor_ellipse (R/circadian_plots.R): the drawn ellipse, vector and verdict
# must be the ones cosinor.confidence.ellipse() computes for the same data

# three days of minute counts; amplitude 0 gives pure noise around 100
.cosinor_days <- function(amplitude = 80, seed = 1) {
  ts <- seq(as.POSIXct("2024-01-01", tz = "UTC"), by = 60, length.out = 3 * 1440)
  h <- as.numeric(format(ts, "%H"))
  set.seed(seed)
  list(ts = ts, counts = pmax(0, 100 + amplitude * cos(2 * pi * (h - 14) / 24) + rnorm(length(h), 0, 25)))
}

.geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))

test_that("the ellipse, the estimate vector and the subtitle come from the cosinor fit", {
  d <- .cosinor_days()
  p <- plot_cosinor_ellipse(d$counts, d$ts)
  expect_identical(.geoms(p), c("GeomPath", "GeomSegment", "GeomText", "GeomPolygon", "GeomSegment", "GeomPoint"))
  cos <- cosinor.analysis(d$counts, d$ts)
  ell <- cosinor.confidence.ellipse(cos)
  expect_identical(ell$rhythm_detected, TRUE)

  b <- ggplot2::ggplot_build(p)
  poly <- b$data[[4]]
  expect_equal(poly$x, ell$ellipse$x)
  expect_equal(poly$y, ell$ellipse$y)
  # a detected rhythm is drawn in the package blue
  expect_identical(unique(poly$fill), "#236192")

  vec <- b$data[[5]]
  expect_equal(c(vec$x, vec$y), c(0, 0))
  expect_equal(c(vec$xend, vec$yend), unname(ell$center))
  expect_identical(unique(vec$colour), "#236192")
  pole <- b$data[[6]]
  expect_equal(c(pole$x, pole$y), c(0, 0))

  expect_identical(p$labels$title, "Cosinor confidence ellipse")
  expect_identical(p$labels$subtitle, sprintf("amplitude %.1f, acrophase %.1f h, rhythm detected",
                                              cos$amplitude, cos$acrophase))
})

test_that("eight clock spokes are labelled every three hours for a 24 h period", {
  d <- .cosinor_days()
  b <- ggplot2::ggplot_build(plot_cosinor_ellipse(d$counts, d$ts))
  labs <- b$data[[3]]
  expect_identical(labs$label, sprintf("%02d:00", seq(0, 21, 3)))
  spokes <- b$data[[2]]
  expect_identical(nrow(spokes), 8L)
  # spoke k points at angle k * 45 degrees, all of the same length
  r <- sqrt(spokes$xend^2 + spokes$yend^2)
  expect_equal(r, rep(r[1], 8))
  expect_equal(atan2(spokes$yend, spokes$xend) %% (2 * pi), (0:7) * pi / 4)
})

test_that("an ellipse that covers the pole is drawn in orange and reported as not detected", {
  d <- .cosinor_days(amplitude = 0, seed = 2)
  p <- plot_cosinor_ellipse(d$counts, d$ts)
  ell <- cosinor.confidence.ellipse(cosinor.analysis(d$counts, d$ts))
  expect_identical(ell$rhythm_detected, FALSE)
  b <- ggplot2::ggplot_build(p)
  expect_identical(unique(b$data[[4]]$fill), "#DF6A2E")
  expect_match(p$labels$subtitle, ", not detected$")
})

test_that("a higher confidence level draws a larger ellipse around the same centre", {
  d <- .cosinor_days()
  cen <- cosinor.confidence.ellipse(cosinor.analysis(d$counts, d$ts))$center
  radius <- function(level) {
    poly <- ggplot2::ggplot_build(plot_cosinor_ellipse(d$counts, d$ts, level = level))$data[[4]]
    max(sqrt((poly$x - cen[1])^2 + (poly$y - cen[2])^2))
  }
  r <- vapply(c(0.5, 0.95, 0.99), radius, numeric(1))
  expect_true(all(diff(r) > 0))
  # the boundary sits at sqrt(2 F(2, df, level)) standard errors from the centre
  cos <- cosinor.analysis(d$counts, d$ts)
  expect_equal(r[2], sqrt(2 * stats::qf(0.95, 2, cos$n_profile_hours - 3)) * cos$se_amplitude,
               tolerance = 1e-3)
})

test_that("too little data or mismatched vectors give the titled placeholder, not an error", {
  d <- .cosinor_days()
  for (p in list(plot_cosinor_ellipse(d$counts[1:40], d$ts[1:40]),
                 plot_cosinor_ellipse(d$counts[1:40], d$ts[1:50]),
                 plot_cosinor_ellipse(rep(NA_real_, 200), d$ts[1:200]))) {
    expect_identical(.geoms(p), "GeomText")
    expect_identical(ggplot2::ggplot_build(p)$data[[1]]$label, "Confidence ellipse unavailable")
    expect_identical(p$labels$title, "Cosinor confidence ellipse")
  }
})
