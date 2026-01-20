# Tests for Publication-Ready Visualization Functions
# canhrActi - CANHR UAF

# Acceleration Distribution Tests

test_that("plot_acceleration_distribution creates plot with valid data", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  data <- data.frame(axis1 = abs(rnorm(1000, mean = 200, sd = 100)))

  p <- plot_acceleration_distribution(data, acc_col = "axis1")

  expect_s3_class(p, "gg")
})

test_that("plot_acceleration_distribution accepts numeric vector", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  acc <- abs(rnorm(500, mean = 150, sd = 80))

  p <- plot_acceleration_distribution(acc)

  expect_s3_class(p, "gg")
})

test_that("plot_acceleration_distribution handles wear time filter", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  data <- data.frame(axis1 = abs(rnorm(1000, mean = 200, sd = 100)))
  wear <- rep(TRUE, 1000)
  wear[1:200] <- FALSE

  p <- plot_acceleration_distribution(data, acc_col = "axis1", wear_time = wear)

  expect_s3_class(p, "gg")
})

test_that("plot_acceleration_distribution supports different types", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  acc <- abs(rnorm(500, mean = 150, sd = 80))

  p_hist <- plot_acceleration_distribution(acc, type = "histogram")
  p_dens <- plot_acceleration_distribution(acc, type = "density")
  p_both <- plot_acceleration_distribution(acc, type = "both")

  expect_s3_class(p_hist, "gg")
  expect_s3_class(p_dens, "gg")
  expect_s3_class(p_both, "gg")
})

test_that("plot_acceleration_distribution validates input", {
  expect_error(plot_acceleration_distribution("not numeric"))
  expect_error(plot_acceleration_distribution(data.frame(x = 1:10), acc_col = "missing"))
})

# Intensity Bins Tests

test_that("plot_intensity_bins creates plot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  acc <- abs(rnorm(500, mean = 100, sd = 60))

  p <- plot_intensity_bins(acc, bin_size = 25)

  expect_s3_class(p, "gg")
})

test_that("plot_intensity_bins handles data frame input", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  data <- data.frame(axis1 = abs(rnorm(500, mean = 100, sd = 60)))

  p <- plot_intensity_bins(data, acc_col = "axis1")

  expect_s3_class(p, "gg")
})

# Activity Clock Tests

test_that("plot_activity_clock creates circular plot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  n <- 1440
  data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "1 min", length.out = n),
    axis1 = abs(rnorm(n, mean = 200, sd = 100))
  )

  p <- plot_activity_clock(data)

  expect_s3_class(p, "gg")
})

test_that("plot_activity_clock shows sleep window", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  n <- 1440
  data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "1 min", length.out = n),
    axis1 = abs(rnorm(n, mean = 200, sd = 100))
  )

  p <- plot_activity_clock(data, show_sleep_window = TRUE, sleep_start = 22, sleep_end = 6)

  expect_s3_class(p, "gg")
})

# Activity Heatmap with Wear Time Tests

test_that("plot_activity_heatmap_wear creates heatmap", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  n <- 1440 * 3  # 3 days
  data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "1 min", length.out = n),
    axis1 = abs(rnorm(n, mean = 200, sd = 100))
  )

  p <- plot_activity_heatmap_wear(data)

  expect_s3_class(p, "gg")
})

test_that("plot_activity_heatmap_wear shows non-wear overlay", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  n <- 1440 * 2
  data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "1 min", length.out = n),
    axis1 = abs(rnorm(n, mean = 200, sd = 100))
  )
  wear <- rep(TRUE, n)
  wear[500:800] <- FALSE  # Non-wear period

  p <- plot_activity_heatmap_wear(data, wear_time = wear, annotate_nonwear = TRUE)

  expect_s3_class(p, "gg")
})

# Publication Export Tests

test_that("export_publication_figure saves file", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = rnorm(10)),
                       ggplot2::aes(x, y)) + ggplot2::geom_point()

  temp_file <- tempfile(fileext = ".png")

  result <- export_publication_figure(p, temp_file, width = 5, height = 4)

  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("export_publication_figure applies journal presets", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = rnorm(10)),
                       ggplot2::aes(x, y)) + ggplot2::geom_point()

  temp_file <- tempfile(fileext = ".png")

  # Test nature preset
  result <- export_publication_figure(p, temp_file, preset = "nature")

  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

# Multi-Panel Figure Tests

test_that("create_multipanel_figure combines plots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  p1 <- ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 5)
  p2 <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  combined <- create_multipanel_figure(list(p1, p2), ncol = 2)

  expect_s3_class(combined, "patchwork")
})
