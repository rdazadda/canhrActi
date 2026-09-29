test_that("troiano detects continuous non-wear correctly", {
  counts <- rep(0, 120)
  result <- wear.troiano(counts)
  expect_equal(sum(!result), 120)
  expect_true(all(!result))
})

test_that("troiano detects continuous wear correctly", {
  counts <- rep(500, 120)
  result <- wear.troiano(counts)
  expect_equal(sum(result), 120)
  expect_true(all(result))
})

test_that("troiano handles 60-minute non-wear window", {
  counts <- c(rep(500, 30), rep(0, 60), rep(500, 30))
  result <- wear.troiano(counts)
  expect_true(sum(!result) >= 50)
  expect_true(sum(result) >= 50)
})

test_that("troiano allows 2-minute spike tolerance", {
  counts <- c(rep(0, 30), c(50, 50), rep(0, 30))
  result <- wear.troiano(counts, non_wear_window = 60, spike_tolerance = 2)
  expect_true(sum(!result) >= 50)
})

test_that("troiano respects spike stop level", {
  counts <- c(rep(0, 30), rep(150, 2), rep(0, 30))
  result <- wear.troiano(counts, spike_stoplevel = 100)
  expect_true(any(result))
})

test_that("choi detects non-wear with 90-minute window", {
  counts <- rep(0, 150)
  result <- wear.choi(counts)
  expect_true(sum(!result) >= 90)
})

test_that("choi validates spikes with upstream/downstream windows", {
  counts <- c(rep(500, 30), rep(0, 45), c(50), rep(0, 45), rep(500, 30))
  result <- wear.choi(counts, non_wear_window = 90, min_window_len = 30)
  expect_identical(which(!result), 31:121)
})

test_that("choi rejects spike without valid upstream/downstream", {
  counts <- c(rep(500, 30), rep(0, 95), c(50), rep(0, 20), rep(500, 30))
  result <- wear.choi(counts, non_wear_window = 90, min_window_len = 30)
  expect_identical(which(!result), 31:125)
})

test_that("choi handles edge cases at start", {
  counts <- c(rep(0, 90), rep(500, 30))
  result <- wear.choi(counts)
  expect_true(sum(!result) >= 80)
})

test_that("choi handles edge cases at end", {
  counts <- c(rep(500, 30), rep(0, 90))
  result <- wear.choi(counts)
  expect_true(sum(!result) >= 80)
})

test_that("CANHR2025 uses 120-minute window", {
  counts <- c(rep(500, 30), rep(0, 120), rep(500, 30))
  result <- wear.CANHR2025(counts)
  expect_true(sum(!result) >= 110)
})

test_that("CANHR2025 allows 3-minute spike tolerance", {
  counts <- c(rep(0, 60), rep(50, 3), rep(0, 60))
  result <- wear.CANHR2025(counts, spike_tolerance = 3)
  expect_identical(result, rep(FALSE, 123))
})

test_that("CANHR2025 uses 45-minute validation windows", {
  counts <- function(after) c(rep(0, 120), 50, rep(0, after), rep(500, 10))
  expect_identical(which(!wear.CANHR2025(counts(45), min_window_len = 45)), 1:166)
  expect_identical(which(!wear.CANHR2025(counts(44), min_window_len = 45)), 1:120)
})

test_that("wear time functions handle empty input", {
  counts <- numeric(0)
  expect_true(length(wear.troiano(counts)) == 0)
  expect_true(length(wear.choi(counts)) == 0)
  expect_true(length(wear.CANHR2025(counts)) == 0)
})

test_that("wear time functions handle single epoch", {
  counts <- 100
  result1 <- wear.troiano(counts)
  result2 <- wear.choi(counts)
  result3 <- wear.CANHR2025(counts)
  expect_true(result1)
  expect_true(result2)
  expect_true(result3)
})

test_that("wear time preserves vector length", {
  counts <- rep(500, 100)
  result1 <- wear.troiano(counts)
  result2 <- wear.choi(counts)
  result3 <- wear.CANHR2025(counts)
  expect_equal(length(result1), length(counts))
  expect_equal(length(result2), length(counts))
  expect_equal(length(result3), length(counts))
})

test_that("wear time functions return logical vectors", {
  counts <- rep(500, 100)
  result1 <- wear.troiano(counts)
  result2 <- wear.choi(counts)
  result3 <- wear.CANHR2025(counts)
  expect_type(result1, "logical")
  expect_type(result2, "logical")
  expect_type(result3, "logical")
})

test_that("troiano scales window for 30-second epochs", {
  counts_60s <- c(rep(500, 30), rep(0, 60), rep(500, 30))
  counts_30s <- c(rep(500, 60), rep(0, 120), rep(500, 60))

  result_60s <- wear.troiano(counts_60s, epoch_length = 60)
  result_30s <- wear.troiano(counts_30s, epoch_length = 30)

  nonwear_60s <- sum(!result_60s)
  nonwear_30s <- sum(!result_30s)

  expect_true(nonwear_30s >= nonwear_60s * 1.5)
})

test_that("choi scales window for 30-second epochs", {
  counts_60s <- rep(0, 150)
  counts_30s <- rep(0, 300)

  result_60s <- wear.choi(counts_60s, epoch_length = 60)
  result_30s <- wear.choi(counts_30s, epoch_length = 30)

  expect_true(sum(!result_60s) >= 90)
  expect_true(sum(!result_30s) >= 180)
})

test_that("wear time accepts epoch_length parameter", {
  counts <- rep(0, 120)
  expect_no_error(wear.troiano(counts, epoch_length = 30))
  expect_no_error(wear.choi(counts, epoch_length = 30))
  expect_no_error(wear.CANHR2025(counts, epoch_length = 30))
})

test_that("choi keeps a spike run when the whole run has 30 zero minutes on each side", {
  # Choi 2011: up to 2 minutes of counts, with no counts 30 minutes up- and downstream of them
  counts <- c(rep(500, 30), rep(0, 60), 50, 50, rep(0, 60), rep(500, 30))
  expect_identical(which(!wear.choi(counts)), 31:152)
  # downstream is counted from the end of the run
  counts <- c(rep(500, 30), rep(0, 90), 50, 50, rep(0, 29), rep(500, 30))
  expect_identical(which(!wear.choi(counts)), 31:120)
  # a run over the tolerance is wear
  counts <- c(rep(500, 30), rep(0, 90), 50, 50, 50, rep(0, 90), rep(500, 30))
  expect_identical(which(!wear.choi(counts)), c(31:120, 124:213))
})

test_that("CANHR2025 keeps a 3-minute spike run with 45 zero minutes on each side", {
  counts <- c(rep(500, 30), rep(0, 80), 50, 50, 50, rep(0, 80), rep(500, 30))
  expect_identical(which(!wear.CANHR2025(counts)), 31:193)
})

test_that("choi keeps a one-minute spike at 30-second epochs", {
  counts <- c(rep(500, 60), rep(0, 120), 50, 50, rep(0, 120), rep(500, 60))
  expect_identical(which(!wear.choi(counts, epoch_length = 30)), 61:302)
})

test_that("choi keeps the 2-minute spike runs that ActiLife's own Choi kept in the sample", {
  # ActiLife 6.15 wrote its Choi wear time into the file (wtvBouts): non-wear from
  # 2025-10-10 01:26 to 04:23, through two 2-minute spike runs on axis 1, at 03:00
  # and 03:44. ActiLife applies no spike stop level.
  cnt <- agd.counts(read.agd(example_agd(1), verbose = FALSE))
  ts <- cnt$timestamp
  day <- ts >= as.POSIXct("2025-10-10 00:00", tz = "UTC") & ts < as.POSIXct("2025-10-10 22:03", tz = "UTC")
  bout <- ts >= as.POSIXct("2025-10-10 01:26", tz = "UTC") & ts < as.POSIXct("2025-10-10 04:23", tz = "UTC")
  wear <- wear.choi(cnt$axis1, spike_stoplevel = Inf)
  expect_identical(wear[day], !bout[day])
})
