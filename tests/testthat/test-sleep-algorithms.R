test_that("cole-kripke classifies high counts as wake", {
  counts <- rep(3000, 100)
  result <- sleep.cole.kripke(counts, apply_rescoring = FALSE)
  expect_true(all(result == "W"))
})

test_that("cole-kripke classifies low counts as sleep", {
  counts <- rep(0, 100)
  result <- sleep.cole.kripke(counts, apply_rescoring = FALSE)
  expect_true(sum(result == "S") > 50)
})

test_that("cole-kripke handles empty input", {
  expect_error(sleep.cole.kripke(numeric(0)), "empty")
})

test_that("cole-kripke handles NA values with warning", {
  counts <- c(0, NA, 100, NA, 200)
  expect_warning(result <- sleep.cole.kripke(counts), "NA values")
  expect_equal(length(result), 5)
})

test_that("cole-kripke caps counts at 300", {
  counts <- c(rep(0, 50), rep(50000, 10), rep(0, 50))
  result <- sleep.cole.kripke(counts, apply_rescoring = FALSE)
  expect_true(any(result == "W"))
})

test_that("cole-kripke uses 7-epoch window correctly", {
  counts <- c(rep(0, 10), rep(1000, 5), rep(0, 10))
  result <- sleep.cole.kripke(counts, apply_rescoring = FALSE)
  middle.idx <- 13
  expect_equal(result[middle.idx], "W")
})

test_that("webster rescoring rule 1: after 4 min wake, 1 min sleep becomes wake", {
  counts <- c(rep(3000, 4), 0, rep(3000, 5))
  result <- sleep.cole.kripke(counts, apply_rescoring = TRUE)
  expect_equal(result[5], "W")
})

test_that("sadeh classifies high counts as wake", {
  counts <- rep(3000, 100)
  result <- sleep.sadeh(counts)
  expect_true(all(result == "W"))
})

test_that("sadeh classifies low counts as sleep", {
  counts <- rep(0, 100)
  result <- sleep.sadeh(counts)
  expect_true(sum(result == "S") > 50)
})

test_that("sadeh handles empty input", {
  expect_error(sleep.sadeh(numeric(0)), "empty")
})

test_that("sadeh handles NA values with warning", {
  counts <- c(0, NA, 100, NA, 200)
  expect_warning(result <- sleep.sadeh(counts), "NA values")
  expect_equal(length(result), 5)
})

test_that("sadeh caps counts at 300", {
  counts <- c(rep(0, 50), rep(50000, 10), rep(0, 50))
  result <- sleep.sadeh(counts)
  expect_true(any(result == "W"))
})

test_that("sadeh uses 11-epoch window correctly", {
  counts <- c(rep(0, 20), rep(1000, 5), rep(0, 20))
  result <- sleep.sadeh(counts)
  middle.idx <- 23
  expect_equal(result[middle.idx], "W")
})

test_that("sadeh calculates AVG correctly", {
  counts <- rep(100, 50)
  result <- sleep.sadeh(counts)
  expect_equal(length(result), 50)
})

test_that("sadeh calculates NATS correctly", {
  counts <- c(rep(0, 10), rep(75, 11), rep(0, 10))
  result <- sleep.sadeh(counts)
  expect_true(any(result == "S"))
})

test_that("tudor-locke detects sleep periods", {
  sleep.state <- c(rep("W", 10), rep("S", 200), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 225)
  result <- sleep.tudor.locke(sleep.state, timestamps)
  expect_equal(nrow(result), 1)
})

test_that("tudor-locke requires minimum sleep period", {
  sleep.state <- c(rep("W", 10), rep("S", 50), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 75)
  result <- sleep.tudor.locke(sleep.state, timestamps, min_sleep_period = 160)
  expect_equal(nrow(result), 0)
})

test_that("tudor-locke detects multiple sleep periods", {
  sleep.state <- c(rep("W", 10), rep("S", 200), rep("W", 20),
                   rep("S", 200), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 445)
  result <- sleep.tudor.locke(sleep.state, timestamps)
  expect_true(nrow(result) >= 1)
})

test_that("tudor-locke validates length mismatch", {
  sleep.state <- rep("S", 100)
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 50)
  expect_error(sleep.tudor.locke(sleep.state, timestamps), "must equal")
})

test_that("tudor-locke handles empty input", {
  expect_warning(result <- sleep.tudor.locke(character(0), as.POSIXct(character(0))), "Empty")
  expect_equal(nrow(result), 0)
})

test_that("tudor-locke calculates sleep efficiency", {
  sleep.state <- c(rep("W", 10), rep("S", 180), rep("W", 20), rep("S", 180), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 405)
  result <- sleep.tudor.locke(sleep.state, timestamps)
  expect_true("sleep_efficiency" %in% names(result))
  expect_true(all(result$sleep_efficiency >= 0 & result$sleep_efficiency <= 100))
})

test_that("tudor-locke calculates awakenings", {
  sleep.state <- c(rep("W", 10), rep("S", 180), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 205)
  result <- sleep.tudor.locke(sleep.state, timestamps)
  expect_true("number_of_awakenings" %in% names(result))
})

test_that("tudor-locke handles counts parameter", {
  sleep.state <- c(rep("W", 10), rep("S", 200), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 225)
  counts <- c(rep(500, 10), rep(50, 200), rep(500, 15))
  result <- sleep.tudor.locke(sleep.state, timestamps, counts = counts)
  expect_true("total_counts" %in% names(result))
  expect_true(result$total_counts > 0)
})

test_that("sleep algorithms preserve vector length", {
  counts <- rep(100, 100)
  result1 <- sleep.cole.kripke(counts)
  result2 <- sleep.sadeh(counts)
  expect_equal(length(result1), length(counts))
  expect_equal(length(result2), length(counts))
})

test_that("sleep algorithms return character vectors", {
  counts <- rep(100, 100)
  result1 <- sleep.cole.kripke(counts)
  result2 <- sleep.sadeh(counts)
  expect_type(result1, "character")
  expect_type(result2, "character")
  expect_true(all(result1 %in% c("S", "W")))
  expect_true(all(result2 %in% c("S", "W")))
})

test_that("tudor-locke filters suspicious periods (>12h, >99% efficiency, 0 awakenings)", {
  # Create a suspicious period: 800 minutes of pure sleep (>12h, 100% efficiency, 0 awakenings)
  sleep.state <- c(rep("W", 10), rep("S", 800), rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 825)

  expect_warning(
    result <- sleep.tudor.locke(sleep.state, timestamps),
    "suspicious"
  )

  # Suspicious period should be filtered out
  expect_equal(nrow(result), 0)
})

test_that("tudor-locke keeps valid long periods with awakenings", {
  # Create a long but valid period: 800 minutes with some wake epochs
  sleep.state <- c(rep("W", 10),
                   rep("S", 200), rep("W", 5), rep("S", 200),
                   rep("W", 5), rep("S", 385),
                   rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 820)

  result <- sleep.tudor.locke(sleep.state, timestamps)

  # Should keep this period because it has awakenings
  expect_true(nrow(result) >= 1)
})

test_that("tudor-locke keeps normal sleep periods", {
  # Normal 8-hour sleep with some awakenings
  sleep.state <- c(rep("W", 10),
                   rep("S", 100), rep("W", 3), rep("S", 200), rep("W", 2), rep("S", 175),
                   rep("W", 15))
  timestamps <- seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = 505)

  result <- sleep.tudor.locke(sleep.state, timestamps)

  expect_equal(nrow(result), 1)
  expect_true(result$sleep_efficiency < 100)
  expect_true(result$number_of_awakenings > 0)
})
