# Weekend days come from the date, not from the day names of the locale, and the day and
# month names the plots show are English whatever the locale

# One week from Monday 2024-01-01 (UTC); Saturday and Sunday carry 500 counts, the other days 100
.week_counts <- function() {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 7 * 1440)
  weekend <- as.Date(ts) %in% as.Date(c("2024-01-06", "2024-01-07"))
  data.frame(timestamp = ts, axis1 = ifelse(weekend, 500, 100))
}

# Friday 2024-03-01 to Sunday 2024-03-03 (UTC) in 1-minute epochs, asleep from 23:00 to 07:00
.three_days <- function() {
  ts <- seq(as.POSIXct("2024-03-01 00:00:00", tz = "UTC"), by = 60, length.out = 3 * 1440)
  hour <- as.POSIXlt(ts)$hour
  data.frame(timestamp = ts, axis1 = rep(c(0, 150, 2500), length.out = length(ts)),
             axis2 = 50, axis3 = 20, steps = rep(c(0L, 20L), length.out = length(ts)),
             hr = 70, lux = 100, wear = 1L,
             sleep_state = ifelse(hour >= 7 & hour < 23, "W", "S"))
}

# English short day name and month/day of each date
.en_day_label <- function(dates, sep) {
  days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  paste0(days[as.POSIXlt(dates)$wday + 1L], sep, format(dates, "%m/%d"))
}

# Facet panel values of a plot
.facets <- function(p, var) {
  as.character(suppressWarnings(ggplot2::ggplot_build(p))$layout$layout[[var]])
}

# Every label on a date axis is the English day and date of its break
.expect_day_axis <- function(p, axis, sep) {
  s <- suppressWarnings(ggplot2::ggplot_build(p))$layout$panel_params[[1]][[axis]]
  breaks <- s$get_breaks()
  keep <- !is.na(breaks)
  dates <- as.Date(unname(breaks[keep]), origin = "1970-01-01")
  expect_gt(length(dates), 0)
  expect_equal(unname(s$get_labels()[keep]), .en_day_label(dates, sep))
}

test_that("plot_activity_heatmap highlights Saturday and Sunday under a German LC_TIME", {
  local_german_time()

  p <- plot_activity_heatmap(.week_counts())
  expect_match(p$labels$subtitle, "7 days (2 weekends)", fixed = TRUE)
  rows <- Filter(function(l) inherits(l$geom, "GeomRect") && identical(l$aes_params$fill, "#FFF3E0"), p$layers)
  highlighted <- as.Date(unname(vapply(rows, function(l) as.numeric(l$data$ymin) + 0.5, numeric(1))))
  expect_equal(highlighted, as.Date(c("2024-01-06", "2024-01-07")))
})

test_that("plot_circadian_polar splits weekday and weekend under a German LC_TIME", {
  local_german_time()

  p <- plot_circadian_polar(.week_counts(), by_day_type = TRUE)
  lines <- Filter(function(l) inherits(l$geom, "GeomLine") && "day_type" %in% names(l$data), p$layers)
  expect_length(lines, 1)
  hourly <- lines[[1]]$data
  expect_setequal(hourly$day_type, c("Weekday", "Weekend"))
  expect_equal(unique(hourly$mean[hourly$day_type == "Weekend"]), 500)
  expect_equal(unique(hourly$mean[hourly$day_type == "Weekday"]), 100)
})

test_that("facet strips and legends name the days in English under a German LC_TIME", {
  local_german_time()
  d <- .three_days()

  full <- c("Friday\n03/01/2024", "Saturday\n03/02/2024", "Sunday\n03/03/2024")
  expect_equal(.facets(plot_daily_timeline(d), "date_label"), full)
  expect_equal(.facets(plot_light_exposure(d), "date_label"), full)
  expect_equal(.facets(plot_wear_time(d, show_summary = FALSE), "date_label"), full)
  expect_equal(.facets(plot_heart_rate(d)$timeline, "date_label"), full)

  steps <- plot_steps(d)
  expect_equal(.facets(steps$cumulative, "date_label"), c("Friday 03/01", "Saturday 03/02", "Sunday 03/03"))
  expect_equal(.facets(steps$rate, "date_label"), c("Friday 03/01", "Saturday 03/02", "Sunday 03/03"))

  expect_equal(.facets(plot_hypnogram(d, sleep_col = "sleep_state"), "night_label"),
               c("Thu 02/29", "Fri 03/01", "Sat 03/02", "Sun 03/03"))
  expect_equal(.facets(plot_day_comparison(d, comparison_type = "facet"), "date_label"),
               c("Fri 03/01", "Sat 03/02", "Sun 03/03"))
  legend <- ggplot2::ggplot_build(plot_day_comparison(d))$plot$scales$get_scales("colour")
  expect_equal(legend$get_labels(), c("Fri 03/01", "Sat 03/02", "Sun 03/03"))
})

test_that("date axes name the days in English under a German LC_TIME", {
  local_german_time()
  d <- .three_days()

  .expect_day_axis(plot_activity_heatmap(d), "y", " ")
  .expect_day_axis(plot_daily_summary_bars(d), "x", "\n")
  .expect_day_axis(plot_steps(d)$daily, "x", "\n")
  .expect_day_axis(plot_vm_heatmap(d), "y", " ")
})

test_that("the daily wear time axis names the days in English under a German LC_TIME", {
  skip_if_not_installed("patchwork")
  local_german_time()

  p <- plot_wear_time(.three_days())
  .expect_day_axis(p[[2]], "x", "\n")
})

test_that("subtitles name the days and months in English under a German LC_TIME", {
  local_german_time()
  d <- .three_days()

  expect_equal(plot_multi_metric(d)$labels$subtitle, "Friday, March 01, 2024")
  expect_equal(plot_multi_metric(d, date_filter = "2024-03-02")$labels$subtitle, "Saturday, March 02, 2024")
  expect_equal(plot_vm_heatmap(d)$labels$subtitle, "Aggregation: 15min | Mar 01 to Mar 03, 2024")
})

test_that(".format_english gives English names and leaves LC_TIME as it was", {
  loc <- local_german_time()

  expect_equal(.format_english(as.Date(c("2024-03-02", "2024-03-03", NA)), "%A"), c("Saturday", "Sunday", NA))
  expect_equal(.format_english(as.POSIXct("2024-03-02 13:05", tz = "UTC"), "%a %b %d %I:%M %p"),
               "Sat Mar 02 01:05 PM")
  expect_equal(Sys.getlocale("LC_TIME"), loc)
  # an error inside the C locale still puts the caller's back
  expect_error(.in_c_time(stop("inside the C locale")), "inside the C locale")
  expect_equal(Sys.getlocale("LC_TIME"), loc)
})
