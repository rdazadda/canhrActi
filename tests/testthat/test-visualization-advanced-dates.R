# Calendar days in R/visualization_advanced.R come from each timestamp's own clock, so a
# day in a zone other than UTC is not cut at UTC midnight

# Friday 2024-03-01 in Alaska (UTC-9) in 15 min epochs, active 08:00 to 20:00; in UTC the
# day runs from 09:00 on the 1st to 08:45 on the 2nd
.ak_day <- function() {
  ts <- seq(as.POSIXct("2024-03-01 00:00:00", tz = "America/Anchorage"), by = 900, length.out = 96)
  on <- as.POSIXlt(ts)$hour >= 8 & as.POSIXlt(ts)$hour < 20
  data.frame(timestamp = ts, axis1 = ifelse(on, 45000, 0), axis2 = 100, axis3 = 100,
             steps = ifelse(on, 100, 0), hr = 70, lux = ifelse(on, 500, 0), wear = TRUE,
             inclinometer = ifelse(on, "standing", "lying"), sleep_state = ifelse(on, "W", "S"),
             intensity = ifelse(on, "moderate", "sedentary"))
}
.fri <- as.Date("2024-03-01")
.panels <- function(p) nrow(ggplot2::ggplot_build(p)$layout$layout)
.layer <- function(p, geom) Filter(function(l) inherits(l$geom, geom), p$layers)

test_that("each plot keeps a local day whole for timestamps in another zone", {
  d <- .ak_day()

  expect_identical(.panels(plot_daily_timeline(d)), 1L)
  tiles <- .layer(plot_activity_heatmap(d), "GeomTile")[[1]]$data
  expect_identical(unique(tiles$date), .fri)
  expect_identical(sort(unique(tiles$hour)), 0:23)

  hourly <- plot_inclinometer(d, show_pie = FALSE)
  expect_s3_class(hourly, "ggplot")
  expect_identical(unique(hourly$data$date), .fri)
  expect_identical(nrow(plot_multi_metric(d, date_filter = "2024-03-01")$data), 96L)

  expect_identical(unique(plot_intensity_area(d)$data$date), .fri)
  expect_identical(unique(plot_light_exposure(d)$data$date), .fri)
  expect_identical(unique(plot_light_summary(d)$data$date), .fri)
  expect_identical(unique(plot_wear_time(d, show_summary = FALSE)$data$date), .fri)
  steps <- plot_steps(d)$daily$data
  expect_identical(steps$date, .fri)
  expect_equal(steps$steps_value, 48 * 100)
  expect_identical(unique(plot_heart_rate(d)$timeline$data$date), .fri)
  expect_identical(unique(plot_day_comparison(d)$data$date), .fri)
  expect_identical(unique(plot_vm_heatmap(d)$data$date), .fri)
  expect_identical(unique(plot_daily_summary_bars(d)$data$date), .fri)

  # a Friday has no weekend hours
  expect_identical(unique(plot_weekend_weekday(d)$data$day_type), "Weekday")
  # nights run from 06:00, so the small hours belong to Thursday's night
  expect_identical(sort(unique(plot_hypnogram(d)$data$night_date)), c(.fri - 1, .fri))
  # one grey line per day
  days <- vapply(plot_is_iv(d)$layers, function(l) is.data.frame(l$data) && "date" %in% names(l$data), logical(1))
  expect_identical(sum(days), 1L)
})

test_that("a date-time given as a date filter selects the day on its own clock", {
  d <- .ak_day()
  evening <- as.POSIXct("2024-03-01 20:00:00", tz = "America/Anchorage")

  mm <- plot_multi_metric(d, date_filter = evening)
  expect_identical(nrow(mm$data), 96L)
  expect_identical(mm$labels$subtitle, "Friday, March 01, 2024")
  expect_identical(unique(plot_inclinometer(d, date_filter = evening, show_pie = FALSE)$data$date), .fri)
  expect_identical(unique(plot_light_exposure(d, date_filter = evening)$data$date), .fri)
  expect_identical(nrow(plot_day_comparison(d, dates = evening)$data), 96L)
})

test_that("dates of a daily table given as date-times are the days on their own clock", {
  # midnight in Berlin is 23:00 UTC the day before
  berlin <- as.POSIXct(c("2024-03-01", "2024-03-02"), tz = "Europe/Berlin")
  daily <- data.frame(date = berlin, mvpa_min = c(30, 40), sedentary_min = c(600, 650))
  bars <- plot_daily_summary_bars(NULL, daily_summary = daily, metrics = c("mvpa_min", "sedentary_min"))
  expect_identical(sort(unique(bars$data$date)), c(.fri, .fri + 1))

  nights <- data.frame(date = berlin, sleep_efficiency = c(90, 85))
  expect_identical(sort(unique(plot_sleep_quality(nights)$data$date)), c(.fri, .fri + 1))
})

test_that("clock times read in the session zone keep their own day", {
  withr::local_timezone("America/Anchorage")
  nights <- data.frame(in_bed_time = c("2024-03-01 22:00:00", "2024-03-02 22:30:00"),
                       out_bed_time = c("2024-03-02 06:00:00", "2024-03-03 06:30:00"),
                       sleep_time = c(420, 400), sleep_efficiency = c(93, 90), stringsAsFactors = FALSE)
  expect_identical(sort(unique(plot_sleep_quality(nights)$data$date)), c(.fri, .fri + 1))

  # the evening part of a night drawn on the heatmap sits on the day it starts
  ts <- seq(as.POSIXct("2024-03-01 00:00:00"), by = 3600, length.out = 48)
  sp <- data.frame(start = "2024-03-01 22:00:00", end = "2024-03-02 06:00:00")
  segs <- .layer(plot_activity_heatmap(data.frame(timestamp = ts, axis1 = 10), sleep_periods = sp), "GeomSegment")
  expect_identical(as.Date(segs[[1]]$data$y), .fri)
  expect_equal(c(segs[[1]]$data$x, segs[[1]]$data$xend), c(22, 24))
  expect_identical(as.Date(segs[[2]]$data$y), .fri + 1)
})
