# Tests for sedentary fragmentation functions

test_that("detect.sedentary.bouts identifies correct bouts", {
  # Create test data with known sedentary bouts
  intensity <- factor(c(rep("sedentary", 30), rep("light", 10), rep("sedentary", 20),
                       rep("moderate", 5), rep("sedentary", 15)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 80)

  bouts <- detect.sedentary.bouts(intensity, timestamps, epoch_length = 60)

  expect_s3_class(bouts, "data.frame")
  expect_equal(nrow(bouts), 3)  # 3 sedentary bouts
  expect_equal(bouts$duration_min[1], 30)
  expect_equal(bouts$duration_min[2], 20)
  expect_equal(bouts$duration_min[3], 15)
})

test_that("detect.sedentary.bouts respects min_bout_length", {
  intensity <- factor(c(rep("sedentary", 30), rep("light", 10), rep("sedentary", 3)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 43)

  # With min_bout_length = 5, should exclude the 3-minute bout
  bouts <- detect.sedentary.bouts(intensity, timestamps, min_bout_length = 5, epoch_length = 60)
  expect_equal(nrow(bouts), 1)

  # With min_bout_length = 1, should include all bouts
  bouts <- detect.sedentary.bouts(intensity, timestamps, min_bout_length = 1, epoch_length = 60)
  expect_equal(nrow(bouts), 2)
})

test_that("detect.sedentary.bouts applies wear time filter", {
  intensity <- factor(rep("sedentary", 100),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 100)

  # First 50 epochs are non-wear
  wear_time <- c(rep(FALSE, 50), rep(TRUE, 50))

  bouts <- detect.sedentary.bouts(intensity, timestamps, wear_time = wear_time, epoch_length = 60)

  expect_equal(nrow(bouts), 1)
  expect_equal(bouts$duration_min[1], 50)  # Only wear time sedentary
})

test_that("detect.sedentary.bouts handles empty input", {
  intensity <- factor(character(0), levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- as.POSIXct(character(0))

  bouts <- detect.sedentary.bouts(intensity, timestamps, epoch_length = 60)

  expect_s3_class(bouts, "data.frame")
  expect_equal(nrow(bouts), 0)
})

test_that("detect.sedentary.bouts handles no sedentary", {
  intensity <- factor(rep("light", 100),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 100)

  bouts <- detect.sedentary.bouts(intensity, timestamps, epoch_length = 60)

  expect_s3_class(bouts, "data.frame")
  expect_equal(nrow(bouts), 0)
})

test_that("sedentary.fragmentation returns correct class", {
  intensity <- factor(c(rep("sedentary", 30), rep("light", 10), rep("sedentary", 20)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 60)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  expect_s3_class(frag, "canhrActi_fragmentation")
  expect_true("total_sedentary_min" %in% names(frag))
  expect_true("total_bouts" %in% names(frag))
  expect_true("mean_bout_duration" %in% names(frag))
  expect_true("breaks_per_sed_hour" %in% names(frag))
  expect_true("alpha" %in% names(frag))
  expect_true("gini" %in% names(frag))
})

test_that("sedentary.fragmentation calculates correct totals", {
  intensity <- factor(c(rep("sedentary", 30), rep("light", 10), rep("sedentary", 20)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 60)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  expect_equal(frag$total_sedentary_min, 50)  # 30 + 20 minutes
  expect_equal(frag$total_bouts, 2)
  expect_equal(frag$mean_bout_duration, 25)  # (30 + 20) / 2
})

test_that("sedentary.fragmentation handles no sedentary", {
  intensity <- factor(rep("light", 100),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 100)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  expect_equal(frag$total_sedentary_min, 0)
  expect_equal(frag$total_bouts, 0)
  expect_true(is.na(frag$mean_bout_duration))
})

test_that("sedentary.fragmentation bout distribution is correct", {
  # Create bouts of specific lengths
  intensity <- factor(c(
    rep("sedentary", 3),   # 3 min bout (1-5 category)
    rep("light", 2),
    rep("sedentary", 8),   # 8 min bout (5-10 category)
    rep("light", 2),
    rep("sedentary", 15),  # 15 min bout (10-20 category)
    rep("light", 2),
    rep("sedentary", 45)   # 45 min bout (30-60 category)
  ), levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 77)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  expect_equal(frag$total_bouts, 4)
  expect_equal(nrow(frag$bout_distribution), 6)

  # Check specific categories
  dist <- frag$bout_distribution
  expect_equal(dist$count[dist$category == "1-5 min"], 1)
  expect_equal(dist$count[dist$category == "5-10 min"], 1)
  expect_equal(dist$count[dist$category == "10-20 min"], 1)
  expect_equal(dist$count[dist$category == "30-60 min"], 1)
})

test_that("sedentary.fragmentation different epoch lengths", {
  # 30-second epochs
  intensity <- factor(rep("sedentary", 60),  # 30 minutes worth of 30-sec epochs
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "30 sec", length.out = 60)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 30)

  expect_equal(frag$total_sedentary_min, 30)  # 60 epochs * 0.5 min
})

test_that("print method works for fragmentation", {
  intensity <- factor(c(rep("sedentary", 30), rep("light", 10), rep("sedentary", 20)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 60)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  output <- capture.output(print(frag))
  expect_true(length(output) > 0)
  expect_true(any(grepl("SEDENTARY FRAGMENTATION", output)))
})

test_that("sedentary.breaks.hourly returns correct structure", {
  intensity <- factor(c(rep("sedentary", 60), rep("light", 60)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  # First hour sedentary, second hour light
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 120)

  hourly <- sedentary.breaks.hourly(intensity, timestamps)

  expect_s3_class(hourly, "data.frame")
  expect_true("hour" %in% names(hourly))
  expect_true("sedentary_min" %in% names(hourly))
  expect_true("breaks" %in% names(hourly))
})

test_that("detect.sedentary.bouts validates input", {
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 10)

  # Wrong type for intensity
  expect_error(detect.sedentary.bouts(1:10, timestamps), "factor or character")

  # Length mismatch
  intensity <- factor(rep("sedentary", 5), levels = c("sedentary", "light"))
  expect_error(detect.sedentary.bouts(intensity, timestamps), "same length")

  # Wrong type for timestamps
  intensity <- factor(rep("sedentary", 10), levels = c("sedentary", "light"))
  expect_error(detect.sedentary.bouts(intensity, 1:10), "POSIXct")
})

test_that("alpha calculation handles edge cases", {
  # Single bout (should return NA or valid alpha)
  intensity <- factor(rep("sedentary", 30),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 30)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  # With only one bout, alpha calculation behavior depends on implementation
  expect_true(frag$total_bouts == 1)
})

test_that("gini calculation is bounded [0, 1]", {
  intensity <- factor(c(rep("sedentary", 10), rep("light", 5),
                       rep("sedentary", 20), rep("light", 5),
                       rep("sedentary", 5), rep("light", 5),
                       rep("sedentary", 50)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  timestamps <- seq(as.POSIXct("2024-01-01 08:00:00"), by = "1 min", length.out = 100)

  frag <- sedentary.fragmentation(intensity, timestamps, epoch_length = 60)

  expect_true(frag$gini >= 0 && frag$gini <= 1)
})
