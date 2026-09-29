# Tests for get.wear.periods() in R/wear_time.R.

TS10 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (0:9) * 60

test_that("get.wear.periods() turns each run of wear into one period", {
  wear <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  got <- get.wear.periods(wear, TS10)
  expect_identical(names(got), c("period", "start_time", "end_time", "duration_minutes",
                                 "start_idx", "end_idx"))
  expect_identical(got$period, 1:3)
  expect_identical(got$start_idx, c(2L, 7L, 10L))
  expect_identical(got$end_idx, c(4L, 8L, 10L))
  expect_identical(got$start_time, TS10[c(2, 7, 10)])
  expect_identical(got$end_time, TS10[c(4, 8, 10)])
  expect_identical(got$duration_minutes, c(3, 2, 1))
})

test_that("get.wear.periods() scales durations by the epoch length", {
  wear <- c(TRUE, TRUE, TRUE, FALSE, TRUE)
  expect_identical(get.wear.periods(wear, TS10[1:5], epoch_length = 30)$duration_minutes, c(1.5, 0.5))
  expect_equal(get.wear.periods(wear, TS10[1:5], epoch_length = 10)$duration_minutes, c(0.5, 1 / 6))
})

test_that("get.wear.periods() counts NA as non-wear and copes with no wear at all", {
  got <- get.wear.periods(c(TRUE, NA, TRUE), TS10[1:3])
  expect_identical(c(got$start_idx, got$end_idx), c(1L, 3L, 1L, 3L))
  none <- get.wear.periods(c(FALSE, FALSE), TS10[1:2])
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), names(got))
  expect_identical(nrow(get.wear.periods(logical(0), TS10[0])), 0L)
  all_day <- get.wear.periods(rep(TRUE, 10), TS10)
  expect_identical(c(all_day$start_idx, all_day$end_idx, all_day$duration_minutes), c(1, 10, 10))
  expect_error(get.wear.periods(c(TRUE, FALSE), TS10[1:3]), "same length")
})

test_that("get.wear.periods() gives back the wear vector of a sample recording", {
  cnt <- agd.counts(read.agd(example_agd(1), verbose = FALSE))
  wear <- wear.choi(cnt$axis1)
  got <- get.wear.periods(wear, cnt$timestamp)
  rebuilt <- rep(FALSE, length(wear))
  for (i in seq_len(nrow(got))) rebuilt[got$start_idx[i]:got$end_idx[i]] <- TRUE
  expect_identical(rebuilt, wear)
  # no two periods touch, so each run of wear is one period
  expect_identical(any(got$start_idx[-1] == got$end_idx[-nrow(got)] + 1L), FALSE)
  expect_equal(sum(got$duration_minutes), sum(wear))
  expect_identical(got$start_time, cnt$timestamp[got$start_idx])
  expect_identical(got$end_time, cnt$timestamp[got$end_idx])
})
