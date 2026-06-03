# Tests for fractal / complexity metrics: fractal.dfa() and multiscale.entropy()
#
# Reference packages (nonlinearTseries, pracma) are used ONLY here to validate
# the hand-coded base-R implementations, and are always guarded with
# skip_if_not_installed so the suite passes without them.

# ---------------------------------------------------------------------------
# fractal.dfa()
# ---------------------------------------------------------------------------

test_that("fractal.dfa returns a well-formed structure", {
  set.seed(101)
  res <- fractal.dfa(rnorm(2000))

  expect_s3_class(res, "canhrActi_dfa")
  expect_true(all(c("alpha", "alpha1", "alpha2", "scales",
                    "fluctuations", "n_used", "breakpoint_min") %in% names(res)))
  expect_length(res$alpha, 1L)
  expect_equal(length(res$scales), length(res$fluctuations))
  expect_true(all(res$fluctuations > 0))
  expect_true(all(diff(res$scales) > 0))  # strictly increasing window sizes
})

test_that("fractal.dfa: white noise alpha ~ 0.5, brown noise alpha ~ 1.5", {
  set.seed(202)
  white <- rnorm(10000)
  brown <- cumsum(white)

  a_white <- fractal.dfa(white)$alpha
  a_brown <- fractal.dfa(brown)$alpha

  expect_equal(a_white, 0.5, tolerance = 0.1)
  expect_equal(a_brown, 1.5, tolerance = 0.1)
  expect_true(a_brown > a_white)
})

test_that("fractal.dfa matches nonlinearTseries::dfa numerically", {
  testthat::skip_if_not_installed("nonlinearTseries")
  set.seed(303)
  white <- rnorm(10000)
  brown <- cumsum(white)

  ours_white <- fractal.dfa(white)$alpha
  ours_brown <- fractal.dfa(brown)$alpha

  ref_alpha <- function(series) {
    out <- nonlinearTseries::dfa(
      time.series = series,
      window.size.range = c(4, floor(length(series) / 4)),
      npoints = 30,
      do.plot = FALSE
    )
    est <- nonlinearTseries::estimate(out, do.plot = FALSE)
    as.numeric(est)
  }

  ref_white <- tryCatch(ref_alpha(white), error = function(e) NA_real_)
  ref_brown <- tryCatch(ref_alpha(brown), error = function(e) NA_real_)

  if (!is.na(ref_white)) {
    expect_equal(ours_white, ref_white, tolerance = 0.1)
  }
  if (!is.na(ref_brown)) {
    expect_equal(ours_brown, ref_brown, tolerance = 0.1)
  }
})

test_that("fractal.dfa: alpha1/alpha2 split obeys the breakpoint", {
  set.seed(404)
  res <- fractal.dfa(cumsum(rnorm(6000)), breakpoint_min = 90)

  # alpha1 from scales below breakpoint, alpha2 from scales at/above it.
  lo <- res$scales < 90
  hi <- res$scales >= 90
  if (sum(lo) >= 2) expect_false(is.na(res$alpha1))
  if (sum(hi) >= 2) expect_false(is.na(res$alpha2))
})

test_that("fractal.dfa handles NA gaps via longest continuous segment", {
  set.seed(505)
  x <- rnorm(4000)
  x[1:50] <- NA          # leading gap
  x[2000:2010] <- NA     # interior gap (segment after is longest: ~1990)
  res <- fractal.dfa(x)

  expect_s3_class(res, "canhrActi_dfa")
  # Longest clean run is samples 2011..4000 => 1990 points.
  expect_equal(res$n_used, 1990)
  expect_false(is.na(res$alpha))
})

test_that("fractal.dfa returns NA structure on degenerate input (no error)", {
  expect_s3_class(fractal.dfa(numeric(0)), "canhrActi_dfa")
  expect_true(is.na(fractal.dfa(numeric(0))$alpha))

  expect_true(is.na(fractal.dfa(rep(5, 1000))$alpha))   # constant series
  expect_true(is.na(fractal.dfa(rnorm(10))$alpha))      # too short
  expect_true(is.na(fractal.dfa(c(NA, NA, NA))$alpha))  # all NA
})

# ---------------------------------------------------------------------------
# multiscale.entropy()
# ---------------------------------------------------------------------------

test_that("multiscale.entropy returns a well-formed structure", {
  set.seed(606)
  res <- multiscale.entropy(rnorm(3000), scales = 1:10)

  expect_s3_class(res, "canhrActi_mse")
  expect_true(all(c("mse", "scales", "area", "slope",
                    "r_absolute", "n_used") %in% names(res)))
  expect_equal(length(res$mse), length(res$scales))
  expect_equal(res$scales, 1:10)
  expect_true(is.finite(res$mse[1]))
})

test_that("multiscale.entropy: white noise entropy decreases with scale", {
  set.seed(707)
  res <- multiscale.entropy(rnorm(5000), scales = 1:20)

  # Scale-10 should be noticeably lower than scale-1 for white noise,
  # and the overall slope should be negative.
  expect_true(res$mse[10] < res$mse[1])
  expect_true(res$slope < 0)
})

test_that("multiscale.entropy: 1/f-like (cumsum) is flatter / more sustained", {
  set.seed(808)
  white <- multiscale.entropy(rnorm(5000), scales = 1:20)
  pink  <- multiscale.entropy(cumsum(rnorm(5000)), scales = 1:20)

  # The random-walk signal sustains entropy better: its decline (white slope is
  # strongly negative) is shallower => pink slope greater than white slope.
  expect_true(pink$slope > white$slope)
})

test_that("multiscale.entropy scale-1 equals inline SampEn hand value", {
  # Deterministic hand check of the Richman-Moorman estimator at scale 1.
  set.seed(909)
  x <- rnorm(500)
  res <- multiscale.entropy(x, scales = 1, m = 2, r = 0.15)

  # Recompute SampEn(m=2, r=0.15*sd) independently with a naive O(N^2) loop.
  sampen_naive <- function(x, m, r) {
    N <- length(x)
    rr <- r * stats::sd(x)
    cnt <- function(mm) {
      tpl <- N - mm + 1
      total <- 0
      for (i in 1:(tpl - 1)) {
        for (j in (i + 1):tpl) {
          d <- max(abs(x[i + 0:(mm - 1)] - x[j + 0:(mm - 1)]))
          if (d <= rr) total <- total + 1
        }
      }
      total
    }
    B <- cnt(m); A <- cnt(m + 1)
    -log(A / B)
  }

  hand <- sampen_naive(x, 2, 0.15)
  expect_equal(res$mse[1], hand, tolerance = 1e-9)
})

test_that("multiscale.entropy scale-1 matches pracma::sample_entropy", {
  testthat::skip_if_not_installed("pracma")
  set.seed(1010)
  x <- rnorm(800)
  res <- multiscale.entropy(x, scales = 1, m = 2, r = 0.15)

  # pracma::sample_entropy takes an absolute tolerance r (= tau argument).
  ref <- tryCatch(
    pracma::sample_entropy(x, edim = 2, r = 0.15 * stats::sd(x), tau = 1),
    error = function(e) NA_real_
  )
  if (is.finite(ref)) {
    expect_equal(res$mse[1], as.numeric(ref), tolerance = 0.05)
  }
})

test_that("multiscale.entropy uses fixed r based on original SD across scales", {
  set.seed(1111)
  x <- rnorm(2000)
  res <- multiscale.entropy(x, scales = 1:5, r = 0.2)
  expect_equal(res$r_absolute, 0.2 * stats::sd(x), tolerance = 1e-9)
})

test_that("multiscale.entropy returns NA structure on degenerate input (no error)", {
  res_empty <- multiscale.entropy(numeric(0))
  expect_s3_class(res_empty, "canhrActi_mse")
  expect_true(all(is.na(res_empty$mse)))

  res_const <- multiscale.entropy(rep(3, 500))
  expect_true(all(is.na(res_const$mse)))

  res_short <- multiscale.entropy(rnorm(3))
  expect_true(all(is.na(res_short$mse)))
})

test_that("multiscale.entropy handles NA via longest continuous segment", {
  set.seed(1212)
  x <- rnorm(2000)
  x[500:520] <- NA
  res <- multiscale.entropy(x, scales = 1:5)
  expect_s3_class(res, "canhrActi_mse")
  expect_true(is.finite(res$mse[1]))
  # longest run is 521..2000 => 1480 points
  expect_equal(res$n_used, 1480)
})

# ---------------------------------------------------------------------------
# print methods
# ---------------------------------------------------------------------------

test_that("print methods run without error", {
  set.seed(1313)
  expect_output(print(fractal.dfa(rnorm(2000))), "Detrended Fluctuation")
  expect_output(print(multiscale.entropy(rnorm(1000), scales = 1:5)),
                "Multiscale Sample Entropy")
})
