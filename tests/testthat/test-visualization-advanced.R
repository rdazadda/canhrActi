# Tests for visualization_advanced.R plot functions

test_that("plot_daily_timeline returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)

  result <- plot_daily_timeline(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_actogram_simple returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 2880)

  result <- plot_actogram_simple(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_activity_heatmap returns ggplot object", {
  skip_if_not_installed("ggplot2")


  test_data <- create.test.counts.data(n = 4320)  # 3 days

  result <- plot_activity_heatmap(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_hourly_boxplot returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 2880)

  result <- plot_hourly_boxplot(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_intensity_pie returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)
  test_data$intensity <- freedson(test_data$axis1)

  result <- plot_intensity_pie(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_intensity_area returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)

  result <- plot_intensity_area(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_daily_summary_bars returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 2880)
  test_data$intensity <- freedson(test_data$axis1)

  result <- plot_daily_summary_bars(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_sleep_overlay returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)

  # Create mock sleep periods
  sleep_periods <- data.frame(
    in_bed_time = "2024-01-01 22:00:00",
    out_bed_time = "2024-01-02 06:00:00",
    onset = "2024-01-01 22:30:00",
    sleep_time = 420,
    wake_time = 30,
    sleep_efficiency = 93.3,
    stringsAsFactors = FALSE
  )

  result <- plot_sleep_overlay(test_data, sleep_periods = sleep_periods)

  expect_s3_class(result, "ggplot")
})

test_that("plot_hypnogram returns ggplot object", {
  skip_if_not_installed("ggplot2")

  n <- 480  # 8 hours of data
  test_data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = n),
    sleep_state = sample(c("S", "W"), n, replace = TRUE, prob = c(0.85, 0.15))
  )

  result <- plot_hypnogram(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_sleep_quality returns ggplot object", {
  skip_if_not_installed("ggplot2")

  sleep_data <- data.frame(
    in_bed_time = c("2024-01-01 22:00:00", "2024-01-02 22:30:00"),
    out_bed_time = c("2024-01-02 06:00:00", "2024-01-03 06:30:00"),
    sleep_time = c(420, 400),
    wake_time = c(30, 40),
    sleep_efficiency = c(93.3, 90.0),
    number_of_awakenings = c(3, 5),
    stringsAsFactors = FALSE
  )

  result <- plot_sleep_quality(sleep_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_circadian_polar returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 2880)

  result <- plot_circadian_polar(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_circadian_polar handles missing hours", {
  skip_if_not_installed("ggplot2")

  # Create data with only partial day coverage
  test_data <- create.test.counts.data(n = 720)  # Only 12 hours

  # Should not error even with missing hours
  result <- plot_circadian_polar(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_compliance_calendar returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 10080)  # 7 days
  test_data$wear <- TRUE
  test_data$wear[1:120] <- FALSE  # 2 hours non-wear

  result <- plot_compliance_calendar(test_data, wear_col = "wear")

  expect_s3_class(result, "ggplot")
})

test_that("plot_weekend_weekday returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 10080)  # 7 days

  result <- plot_weekend_weekday(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_acceleration_distribution returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)

  result <- plot_acceleration_distribution(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_activity_histogram returns ggplot object", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)

  result <- plot_activity_histogram(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot functions require non-empty data", {
  skip_if_not_installed("ggplot2")

  empty_data <- data.frame(
    timestamp = as.POSIXct(character(0)),
    axis1 = numeric(0)
  )

  # Empty data should error or return informative message
  expect_error(plot_daily_timeline(empty_data))
})

test_that("plot functions handle NA values", {
  skip_if_not_installed("ggplot2")

  test_data <- create.test.counts.data(n = 1440)
  test_data$axis1[100:200] <- NA

  # Should handle NA values without error
  result <- plot_daily_timeline(test_data)

  expect_s3_class(result, "ggplot")
})
