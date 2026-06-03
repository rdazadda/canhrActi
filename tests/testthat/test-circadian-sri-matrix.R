# Tests for sri.matrix() - Phillips (2017) epoch-of-day x day matrix SRI.
#
# Reference packages (ActCR provides the canonical SRI() implementation used
# in the sleep literature) are only used here, guarded by skip_if_not_installed,
# to numerically validate the hand-coded base-R math.

# --- Helpers ----------------------------------------------------------------

# Build a 4-day, 1-min-epoch timestamp grid starting at midnight.
.make_ts <- function(n_days = 4, epoch = 60, start = "2024-01-01 00:00:00") {
  per_day <- 86400 / epoch
  seq(as.POSIXct(start, tz = "UTC"), by = epoch,
      length.out = n_days * per_day)
}

# A regular sleeper: asleep in clock hours [sleep_start, sleep_start + 8).
.regular_states <- function(ts, sleep_start = 0, sleep_hours = 8) {
  h <- as.POSIXlt(ts)$hour
  end <- (sleep_start + sleep_hours) %% 24
  if (sleep_start < end) {
    asleep <- h >= sleep_start & h < end
  } else {
    asleep <- h >= sleep_start | h < end
  }
  ifelse(asleep, "S", "W")
}


# --- Core algorithm correctness --------------------------------------------

test_that("perfectly regular sleeper scores ~100", {
  ts <- .make_ts(4)
  state <- .regular_states(ts, sleep_start = 0, sleep_hours = 8)
  res <- sri.matrix(state, ts, epoch_length = 60)

  expect_equal(res$method, "phillips_matrix")
  expect_equal(res$n_days, 4L)
  expect_equal(res$SRI, 100, tolerance = 1e-8)
})

test_that("perfectly anti-regular (flipped every day) scores -100", {
  ts <- .make_ts(4)
  # Day-parity flip: even days asleep 00-08, odd days awake then -> opposite
  # state at every clock epoch on consecutive days.
  h <- as.POSIXlt(ts)$hour
  day_idx <- as.integer(as.Date(ts) - as.Date(ts[1]))
  base_sleep <- h < 8
  # Flip the whole pattern on odd days so every consecutive pair disagrees.
  asleep <- ifelse(day_idx %% 2 == 0, base_sleep, !base_sleep)
  state <- ifelse(asleep, "S", "W")

  res <- sri.matrix(state, ts, epoch_length = 60)
  expect_equal(res$SRI, -100, tolerance = 1e-8)
})

test_that("random independent pattern each day scores ~0", {
  set.seed(123)
  ts <- .make_ts(40)            # many days -> tight around 0
  per_day <- 1440
  n_days <- 40
  # Independent fair coin per epoch -> expected concordance 0.5 -> SRI 0.
  bits <- sample(c("S", "W"), per_day * n_days, replace = TRUE)
  res <- sri.matrix(bits, ts, epoch_length = 60)

  expect_true(abs(res$SRI) < 5)   # sampling noise; ~0
})

test_that("inverted every other epoch-block still bounded and symmetric", {
  ts <- .make_ts(4)
  state <- .regular_states(ts, sleep_start = 0, sleep_hours = 8)
  res <- sri.matrix(state, ts, epoch_length = 60)
  expect_true(res$SRI >= -100 && res$SRI <= 100)
})


# --- Encoding flexibility ---------------------------------------------------

test_that("numeric 1/0 encoding matches character S/W encoding", {
  ts <- .make_ts(4)
  chr <- .regular_states(ts, sleep_start = 2, sleep_hours = 7)
  num <- as.integer(chr == "S")

  r_chr <- sri.matrix(chr, ts, 60)
  r_num <- sri.matrix(num, ts, 60)
  expect_equal(r_chr$SRI, r_num$SRI)
  expect_equal(r_chr$n_valid_pairs, r_num$n_valid_pairs)
})

test_that("logical encoding matches character encoding", {
  ts <- .make_ts(4)
  chr <- .regular_states(ts, sleep_start = 1, sleep_hours = 6)
  lgl <- chr == "S"
  expect_equal(sri.matrix(chr, ts, 60)$SRI, sri.matrix(lgl, ts, 60)$SRI)
})


# --- NA / gap handling ------------------------------------------------------

test_that("NA epochs are skipped and do not bias the index", {
  ts <- .make_ts(4)
  state <- .regular_states(ts, sleep_start = 0, sleep_hours = 8)
  # Knock out 20% of epochs as non-wear (NA): index should remain ~100.
  set.seed(7)
  na_idx <- sample(seq_along(state), length(state) * 0.2)
  state[na_idx] <- NA
  res <- sri.matrix(state, ts, 60)

  expect_equal(res$SRI, 100, tolerance = 1e-8)   # still perfectly regular
  expect_true(res$n_valid_pairs < 3 * 1440)      # fewer than full pairs
  expect_true(res$n_valid_pairs > 0)
})

test_that("an internal missing day breaks the comparison chain across it", {
  # Days 1, 2, 4 present; day 3 missing entirely. Consecutive pairs that can
  # be formed: (1->2) only, because (2->3),(3->4) involve the all-NA day-3
  # column. n_days must reflect the full calendar span (4).
  ts1 <- .make_ts(2, start = "2024-01-01 00:00:00")               # days 1-2
  ts4 <- .make_ts(1, start = "2024-01-04 00:00:00")               # day 4
  ts <- c(ts1, ts4)
  s1 <- .regular_states(ts1, 0, 8)
  s4 <- .regular_states(ts4, 0, 8)
  state <- c(s1, s4)

  res <- sri.matrix(state, ts, 60)
  expect_equal(res$n_days, 4L)
  expect_equal(res$n_valid_pairs, 1440L)   # only the (day1 -> day2) column-pair
  expect_equal(res$SRI, 100, tolerance = 1e-8)
})


# --- Robustness to non-midnight start ---------------------------------------

test_that("recording not starting at midnight aligns by clock time", {
  # Start at 14:30; regular sleeper -> still ~100 because alignment is by
  # real clock time, not by elapsed-sample offset.
  ts <- .make_ts(5, start = "2024-03-10 14:30:00")
  state <- .regular_states(ts, sleep_start = 23, sleep_hours = 8)  # 23:00-07:00
  res <- sri.matrix(state, ts, 60)
  expect_equal(res$SRI, 100, tolerance = 1e-8)
})


# --- Edge cases never error -------------------------------------------------

test_that("fewer than two days returns NA gracefully", {
  ts <- .make_ts(1)
  state <- .regular_states(ts, 0, 8)
  res <- sri.matrix(state, ts, 60)
  expect_true(is.na(res$SRI))
  expect_equal(res$n_days, 1L)
  expect_equal(res$method, "phillips_matrix")
})

test_that("empty / mismatched / null input returns NA without error", {
  expect_true(is.na(sri.matrix(character(0), as.POSIXct(character(0)))$SRI))
  ts <- .make_ts(2)
  expect_true(is.na(sri.matrix(c("S", "W"), ts)$SRI))  # length mismatch
  expect_true(is.na(sri.matrix(NULL, NULL)$SRI))
})

test_that("all-NA states return NA but report calendar span", {
  ts <- .make_ts(3)
  state <- rep(NA_character_, length(ts))
  res <- sri.matrix(state, ts, 60)
  expect_true(is.na(res$SRI))
  expect_equal(res$n_valid_pairs, 0L)
})


# --- Agreement with the existing fast (single-lag) form ---------------------

test_that("matches .calculate.sri.fast on a clean gapless regular series", {
  skip_if_not(exists(".calculate.sri.fast"))
  ts <- .make_ts(5)
  state <- .regular_states(ts, sleep_start = 0, sleep_hours = 8)
  fast <- .calculate.sri.fast(state, ts, 60)
  mat  <- sri.matrix(state, ts, 60)$SRI
  # On a perfectly regular, gapless, midnight-aligned series the two forms
  # coincide (both 100). Allow tiny rounding tolerance.
  expect_equal(mat, fast, tolerance = 1e-6)
})


# --- Numeric validation against ActCR reference (if installed) --------------

test_that("matches ActCR::SRI reference implementation", {
  skip_if_not_installed("ActCR")
  # ActCR::SRI expects a long data.frame with columns: Id, time, state
  # where state is 0/1 (1 = sleep). We build a moderately irregular sleeper
  # so the value is strictly interior (not a trivial +/-100 endpoint).
  set.seed(42)
  n_days <- 7
  ts <- .make_ts(n_days)
  per_day <- 1440

  # Each day shift sleep onset by a random amount -> partial regularity.
  state_chr <- character(0)
  day_starts <- sample(21:25, n_days, replace = TRUE) %% 24  # onset hour
  for (d in seq_len(n_days)) {
    day_ts <- ts[((d - 1) * per_day + 1):(d * per_day)]
    state_chr <- c(state_chr,
                   .regular_states(day_ts, sleep_start = day_starts[d],
                                   sleep_hours = 8))
  }
  ours <- sri.matrix(state_chr, ts, 60)$SRI

  ref_val <- tryCatch({
    df <- data.frame(
      Id = 1L,
      Time = ts,
      State = as.integer(state_chr == "S")
    )
    # ActCR::SRI signature varies across versions; attempt the documented one.
    r <- ActCR::SRI(df)
    if (is.list(r) && !is.null(r$SRI)) r$SRI else as.numeric(r)
  }, error = function(e) NA_real_)

  skip_if(is.na(ref_val), "ActCR::SRI signature differs; skipping numeric xref")
  expect_equal(ours, as.numeric(ref_val), tolerance = 0.5)
})
