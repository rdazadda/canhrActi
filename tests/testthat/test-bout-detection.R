test_that("detect.mvpa.bouts identifies standard 10-minute bout", {
  intensity <- c(rep("sedentary", 30), rep("moderate", 12), rep("sedentary", 30))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 12)
  expect_equal(result$mvpa_minutes, 12)
  expect_equal(result$mvpa_percent, 100)
})

test_that("detect.mvpa.bouts allows 2-minute drop time", {
  intensity <- c(rep("sedentary", 10),
                 rep("moderate", 4), "light", "light", rep("moderate", 6),
                 rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 12)
  expect_equal(result$mvpa_minutes, 10)
  expect_true(result$mvpa_percent > 80)
})

test_that("detect.mvpa.bouts breaks bout after exceeding drop time", {
  intensity <- c(rep("moderate", 5), rep("light", 3), rep("moderate", 5))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 0)
})

test_that("detect.mvpa.bouts detects multiple bouts", {
  intensity <- c(rep("moderate", 12), rep("sedentary", 20), rep("vigorous", 15))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 2)
  expect_equal(result$bout_number, c(1, 2))
})

test_that("detect.mvpa.bouts rejects bouts below minimum length", {
  intensity <- c(rep("moderate", 8), rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 0)
})

test_that("detect.mvpa.bouts handles continuous MVPA", {
  intensity <- rep("moderate", 30)
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 30)
  expect_equal(result$mvpa_minutes, 30)
  expect_equal(result$mvpa_percent, 100)
})

test_that("detect.mvpa.bouts includes vigorous activity", {
  intensity <- c(rep("sedentary", 10), rep("vigorous", 12), rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 1)
  expect_equal(result$mvpa_minutes, 12)
})

test_that("detect.mvpa.bouts includes very vigorous activity", {
  intensity <- c(rep("sedentary", 10), rep("very_vigorous", 12), rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 1)
  expect_equal(result$mvpa_minutes, 12)
})

test_that("detect.mvpa.bouts excludes light activity", {
  intensity <- c(rep("light", 12), rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10)

  expect_equal(nrow(result), 0)
})

test_that("detect.mvpa.bouts with 0 drop time allowance", {
  intensity <- c(rep("moderate", 5), "light", rep("moderate", 5))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 0)

  expect_equal(nrow(result), 0)
})

test_that("detect.mvpa.bouts with 1 minute drop time allowance", {
  intensity <- c(rep("moderate", 5), "light", rep("moderate", 5))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 1)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 11)
})

test_that("detect.mvpa.bouts handles drop time at end of bout", {
  intensity <- c(rep("moderate", 10), "light", "light", rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 10)
  expect_equal(result$mvpa_minutes, 10)
})

test_that("detect.mvpa.bouts handles drop time at end of data", {
  intensity <- c(rep("moderate", 10), "light", "light")
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 12)
  expect_equal(result$mvpa_minutes, 10)
})

test_that("detect.mvpa.bouts 80 percent rule identifies valid bout", {
  intensity <- c(rep("moderate", 8), "light", "light", rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, use_80_percent_rule = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 10)
  expect_equal(result$mvpa_minutes, 8)
  expect_equal(result$mvpa_percent, 80)
})

test_that("detect.mvpa.bouts 80 percent rule rejects invalid bout", {
  intensity <- c(rep("moderate", 7), rep("light", 3), rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, use_80_percent_rule = TRUE)

  expect_equal(nrow(result), 0)
})

test_that("detect.mvpa.bouts 80 percent rule extends valid bouts", {
  intensity <- c(rep("moderate", 10), "light", rep("moderate", 4))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, use_80_percent_rule = TRUE)

  expect_equal(nrow(result), 1)
  expect_true(result$bout_length >= 10)
  expect_true(result$mvpa_percent >= 80)
})

test_that("detect.mvpa.bouts 80 percent rule stops extending when threshold not met", {
  intensity <- c(rep("moderate", 10), rep("light", 5))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, use_80_percent_rule = TRUE)

  expect_equal(nrow(result), 1)
  expect_true(result$bout_length >= 10)
  expect_true(result$mvpa_percent >= 80)
})

test_that("detect.mvpa.bouts handles custom mvpa levels", {
  intensity <- c(rep("sedentary", 10), rep("light", 12), rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity, mvpa_levels = c("light", "moderate", "vigorous"))

  expect_equal(nrow(result), 1)
  expect_equal(result$mvpa_minutes, 12)
})

test_that("detect.mvpa.bouts accepts factor input", {
  intensity <- factor(c(rep("sedentary", 10), rep("moderate", 12)),
                     levels = c("sedentary", "light", "moderate", "vigorous"))
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 1)
})

test_that("detect.mvpa.bouts handles empty input", {
  intensity <- character(0)
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 0)
  expect_true("bout_number" %in% names(result))
})

test_that("detect.mvpa.bouts handles single epoch", {
  intensity <- "moderate"
  result <- detect.mvpa.bouts(intensity, min_bout_length = 1, drop_time_allowance = 0)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 1)
})

test_that("detect.mvpa.bouts handles no MVPA", {
  intensity <- rep("sedentary", 100)
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 0)
})

test_that("detect.mvpa.bouts validates min_bout_length", {
  intensity <- rep("moderate", 10)
  expect_error(detect.mvpa.bouts(intensity, min_bout_length = 0), "at least 1 minute")
})

test_that("detect.mvpa.bouts validates drop_time_allowance", {
  intensity <- rep("moderate", 10)
  expect_error(detect.mvpa.bouts(intensity, drop_time_allowance = -1), "non-negative")
})

test_that("detect.mvpa.bouts warns when drop time exceeds min bout length", {
  intensity <- rep("moderate", 10)
  expect_warning(detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 10),
                "unexpected results")
})

test_that("detect.mvpa.bouts calculates correct start and end indices", {
  intensity <- c(rep("sedentary", 5), rep("moderate", 12), rep("sedentary", 5))
  result <- detect.mvpa.bouts(intensity)

  expect_equal(result$start_index, 6)
  expect_equal(result$end_index, 17)
  expect_equal(result$end_index - result$start_index + 1, result$bout_length)
})

test_that("detect.mvpa.bouts handles mixed MVPA intensities", {
  intensity <- c(rep("sedentary", 10),
                 rep("moderate", 5), rep("vigorous", 3), rep("very_vigorous", 2),
                 rep("sedentary", 10))
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 1)
  expect_equal(result$mvpa_minutes, 10)
})

test_that("detect.mvpa.bouts handles real-world pattern", {
  intensity <- c(rep("sedentary", 30),
                 rep("light", 5),
                 rep("moderate", 8), "light", rep("moderate", 3),
                 rep("light", 10),
                 rep("vigorous", 12),
                 rep("sedentary", 30))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_true(nrow(result) >= 1)
  expect_true(all(result$bout_length >= 10))
})

test_that("detect.mvpa.bouts preserves bout order", {
  intensity <- c(rep("moderate", 12), rep("sedentary", 10), rep("vigorous", 15),
                 rep("sedentary", 10), rep("moderate", 11))
  result <- detect.mvpa.bouts(intensity)

  expect_equal(nrow(result), 3)
  expect_equal(result$bout_number, 1:3)
  expect_true(all(diff(result$start_index) > 0))
})

test_that("detect.mvpa.bouts handles long bout", {
  intensity <- rep("moderate", 120)
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 120)
  expect_equal(result$mvpa_percent, 100)
})

test_that("summarize.mvpa.bouts calculates correct statistics", {
  bouts <- data.frame(
    bout_number = 1:3,
    start_index = c(1, 50, 100),
    end_index = c(10, 65, 119),
    bout_length = c(10, 16, 20),
    mvpa_minutes = c(10, 14, 18),
    mvpa_percent = c(100, 87.5, 90)
  )

  result <- summarize.mvpa.bouts(bouts)

  expect_equal(result$total_bouts, 3)
  expect_equal(result$total_bouted_mvpa, 42)
  expect_equal(result$mean_bout_length, round(mean(c(10, 16, 20)), 1))
  expect_equal(result$median_bout_length, 16)
  expect_equal(result$min_bout_length, 10)
  expect_equal(result$max_bout_length, 20)
})

test_that("summarize.mvpa.bouts handles empty bouts", {
  bouts <- data.frame(
    bout_number = integer(0),
    start_index = integer(0),
    end_index = integer(0),
    bout_length = integer(0),
    mvpa_minutes = integer(0),
    mvpa_percent = numeric(0)
  )

  result <- summarize.mvpa.bouts(bouts)

  expect_equal(result$total_bouts, 0)
  expect_equal(result$total_bouted_mvpa, 0)
  expect_equal(result$mean_bout_length, 0)
  expect_equal(result$median_bout_length, 0)
  expect_true(is.na(result$min_bout_length))
  expect_true(is.na(result$max_bout_length))
})

test_that("summarize.mvpa.bouts handles single bout", {
  bouts <- data.frame(
    bout_number = 1,
    start_index = 1,
    end_index = 12,
    bout_length = 12,
    mvpa_minutes = 11,
    mvpa_percent = 91.7
  )

  result <- summarize.mvpa.bouts(bouts)

  expect_equal(result$total_bouts, 1)
  expect_equal(result$total_bouted_mvpa, 11)
  expect_equal(result$min_bout_length, 12)
  expect_equal(result$max_bout_length, 12)
})

test_that("detect.mvpa.bouts drop time method matches expected behavior", {
  intensity <- c(rep("moderate", 3), "light", rep("moderate", 3), "light", "light", rep("moderate", 4))
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$bout_length, 13)
})

test_that("detect.mvpa.bouts 80 percent vs drop time comparison", {
  intensity <- c(rep("moderate", 8), "light", "light", rep("sedentary", 10))

  result_drop <- detect.mvpa.bouts(intensity, min_bout_length = 10,
                                   drop_time_allowance = 2, use_80_percent_rule = FALSE)
  result_80 <- detect.mvpa.bouts(intensity, min_bout_length = 10, use_80_percent_rule = TRUE)

  expect_equal(nrow(result_drop), 0)
  expect_equal(nrow(result_80), 1)
})

test_that("detect.mvpa.bouts handles minimum bout length of 1", {
  intensity <- c("moderate", "sedentary", "vigorous")
  result <- detect.mvpa.bouts(intensity, min_bout_length = 1, drop_time_allowance = 0)

  expect_equal(nrow(result), 2)
  expect_true(all(result$bout_length == 1))
})

test_that("detect.mvpa.bouts calculates mvpa_percent correctly", {
  intensity <- c(rep("moderate", 8), "light", "light", "moderate", "moderate")
  result <- detect.mvpa.bouts(intensity, min_bout_length = 10, drop_time_allowance = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$mvpa_minutes, 10)
  expect_equal(result$bout_length, 12)
  expect_equal(result$mvpa_percent, round(100 * 10 / 12, 1))
})
