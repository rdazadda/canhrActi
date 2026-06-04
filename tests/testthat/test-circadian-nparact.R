# Cross-validation of the non-parametric circadian metrics (IS, IV, RA, L5, M10)
# against nparACT (Blume, Santhi & Schabus 2016), the reference R implementation.

.make_np_signal <- function(days = 7, epoch_length = 60, acrophase = 14, seed = 1) {
  n <- days * (86400 / epoch_length)
  t0 <- as.POSIXct("2024-01-06 00:00:00", tz = "UTC")
  ts <- t0 + (seq_len(n) - 1) * epoch_length
  th <- as.numeric(difftime(ts, t0, units = "hours"))
  set.seed(seed)
  act <- pmax(0, 100 + 80 * cos(2 * pi * (th - acrophase) / 24) + rnorm(n, 0, 3))
  list(ts = ts, act = act, epoch_length = epoch_length)
}

test_that("IS / IV / RA / L5 / M10 match nparACT", {
  skip_if_not_installed("nparACT")

  d <- .make_np_signal()
  cr <- circadian.rhythm(d$act, d$ts, epoch_length = d$epoch_length)

  # nparACT_base() does get(name): it wants the *name* of a data frame in the
  # global environment, not the data frame itself.
  assign("np_xvalid_df", data.frame(time = d$ts, activity = d$act), envir = globalenv())
  on.exit(suppressWarnings(rm("np_xvalid_df", envir = globalenv())), add = TRUE)
  np <- nparACT::nparACT_base("np_xvalid_df", SR = 1 / d$epoch_length, plot = FALSE)

  # IS, L5, M10 agree to high precision; IV and RA agree within nparACT's
  # 2-decimal reporting (its outputs are rounded).
  expect_lt(abs(cr$IS  - np$IS),  0.01)
  expect_lt(abs(cr$IV  - np$IV),  0.02)
  expect_lt(abs(cr$RA  - np$RA),  0.01)
  expect_lt(abs(cr$L5  - np$L5),  0.5)
  expect_lt(abs(cr$M10 - np$M10), 0.5)
})

test_that("nparACT agreement holds at a shifted acrophase", {
  skip_if_not_installed("nparACT")

  d <- .make_np_signal(acrophase = 8, seed = 7)
  cr <- circadian.rhythm(d$act, d$ts, epoch_length = d$epoch_length)

  assign("np_xvalid_df2", data.frame(time = d$ts, activity = d$act), envir = globalenv())
  on.exit(suppressWarnings(rm("np_xvalid_df2", envir = globalenv())), add = TRUE)
  np <- nparACT::nparACT_base("np_xvalid_df2", SR = 1 / d$epoch_length, plot = FALSE)

  expect_lt(abs(cr$IS  - np$IS),  0.01)
  expect_lt(abs(cr$IV  - np$IV),  0.02)
  expect_lt(abs(cr$RA  - np$RA),  0.01)
  expect_lt(abs(cr$M10 - np$M10), 0.5)
})
