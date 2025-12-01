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
  counts <- c(rep(500, 30), rep(0, 40), c(50), rep(0, 40), rep(500, 30))
  result <- wear.choi(counts, non_wear_window = 90, min_window_len = 30)
  expect_equal(length(result), length(counts))
  expect_type(result, "logical")
})

test_that("choi rejects spike without valid upstream/downstream", {
  counts <- c(rep(500, 30), rep(0, 20), c(50), rep(0, 20), rep(500, 30))
  result <- wear.choi(counts, non_wear_window = 90, min_window_len = 30)
  expect_equal(length(result), length(counts))
  expect_type(result, "logical")
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
  expect_equal(length(result), 123)
  expect_type(result, "logical")
})

test_that("CANHR2025 uses 45-minute validation windows", {
  counts <- c(rep(0, 50), c(50), rep(0, 50))
  result <- wear.CANHR2025(counts, min_window_len = 45)
  expect_equal(length(result), 101)
  expect_type(result, "logical")
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
