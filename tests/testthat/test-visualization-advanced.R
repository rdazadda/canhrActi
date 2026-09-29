# Tests for visualization_advanced.R plot functions

test_that("plot_daily_timeline returns ggplot object", {
  test_data <- create.test.counts.data(n = 1440)

  result <- plot_daily_timeline(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_activity_heatmap returns ggplot object", {
  test_data <- create.test.counts.data(n = 4320)  # 3 days

  result <- plot_activity_heatmap(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_intensity_pie returns ggplot object", {
  test_data <- create.test.counts.data(n = 1440)
  test_data$intensity <- freedson(test_data$axis1)

  result <- plot_intensity_pie(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_intensity_area returns ggplot object", {
  test_data <- create.test.counts.data(n = 1440)

  result <- plot_intensity_area(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_daily_summary_bars returns ggplot object", {
  test_data <- create.test.counts.data(n = 2880)
  test_data$intensity <- freedson(test_data$axis1)

  result <- plot_daily_summary_bars(test_data)

  expect_s3_class(result, "ggplot")
})

# two UTC days of minute epochs, each epoch with its date: 08:00 to 10:00 at 1980 counts
# and 20 steps a minute, 50 counts and no steps otherwise
.epochs_with_dates <- function() {
  ts <- seq(as.POSIXct("2024-03-04 00:00:00", tz = "UTC"), by = 60, length.out = 2880)
  h <- as.numeric(format(ts, "%H"))
  active <- h >= 8 & h < 10
  data.frame(timestamp = ts, date = as.Date(ts), axis1 = ifelse(active, 1980, 50), steps = ifelse(active, 20, 0))
}

test_that("plot_daily_summary_bars sums epochs by day when every epoch carries its date", {
  d <- plot_daily_summary_bars(.epochs_with_dates())$data
  expect_identical(nrow(d), 6L)
  expect_identical(unique(d$date), as.Date(c("2024-03-04", "2024-03-05")))
  expect_equal(d$value[d$metric == "steps"], c(2400, 2400))
  expect_equal(d$value[d$metric == "mvpa_min"], c(120, 120))
  expect_equal(d$value[d$metric == "sedentary_min"], c(1320, 1320))
})

test_that("plot_daily_summary_bars takes the cut points by name or as numbers", {
  e <- .epochs_with_dates()
  mvpa <- function(cp) {
    d <- plot_daily_summary_bars(e, cutpoints = cp)$data
    d$value[d$metric == "mvpa_min"]
  }
  # 1980 counts a minute is moderate for Freedson, light below a 2000 or Evenson's 2296
  expect_equal(mvpa("freedson"), c(120, 120))
  expect_equal(mvpa(c(sedentary = 100, light = 2000, moderate = 5000, vigorous = 9000)), c(0, 0))
  expect_equal(mvpa("evenson"), c(0, 0))
})

test_that("plot_daily_summary_bars counts only worn epochs when the data marks them", {
  e <- .epochs_with_dates()
  # not worn from midnight to 06:00
  e$wear_time <- as.numeric(format(e$timestamp, "%H")) >= 6
  d <- plot_daily_summary_bars(e, metrics = c("mvpa_min", "sedentary_min", "wear_min"))$data
  expect_equal(d$value[d$metric == "mvpa_min"], c(120, 120))
  expect_equal(d$value[d$metric == "sedentary_min"], c(960, 960))
  expect_equal(d$value[d$metric == "wear_min"], c(1080, 1080))
})

test_that("plot_hypnogram returns ggplot object", {
  n <- 480  # 8 hours of data
  test_data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01 22:00:00"), by = 60, length.out = n),
    sleep_state = sample(c("S", "W"), n, replace = TRUE, prob = c(0.85, 0.15))
  )

  result <- plot_hypnogram(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_sleep_quality returns ggplot object", {
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
  test_data <- create.test.counts.data(n = 2880)

  result <- plot_circadian_polar(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_circadian_polar handles missing hours", {
  # Create data with only partial day coverage
  test_data <- create.test.counts.data(n = 720)  # Only 12 hours

  # Should not error even with missing hours
  expect_warning(result <- plot_circadian_polar(test_data), "less than 12 hours")

  expect_s3_class(result, "ggplot")
})

test_that("plot_weekend_weekday returns ggplot object", {
  test_data <- create.test.counts.data(n = 10080)  # 7 days

  result <- plot_weekend_weekday(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot_acceleration_distribution returns ggplot object", {
  test_data <- create.test.counts.data(n = 1440)

  result <- plot_acceleration_distribution(test_data)

  expect_s3_class(result, "ggplot")
})

test_that("plot functions require non-empty data", {
  empty_data <- data.frame(
    timestamp = as.POSIXct(character(0)),
    axis1 = numeric(0)
  )

  # Empty data should error or return informative message
  expect_error(plot_daily_timeline(empty_data), "No data available for plotting")
  expect_error(plot_intensity_pie(empty_data), "No data available for plotting")
  empty_data$sleep_state <- character(0)
  expect_error(plot_hypnogram(empty_data), "No data available for plotting")
})

test_that("plot functions handle NA values", {
  test_data <- create.test.counts.data(n = 1440)
  test_data$axis1[100:200] <- NA

  # Should handle NA values without error
  result <- plot_daily_timeline(test_data)

  expect_s3_class(result, "ggplot")
})
