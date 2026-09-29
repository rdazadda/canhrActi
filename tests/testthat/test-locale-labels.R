# Day, month and AM/PM names that reach the user are English whatever the LC_TIME;
# local_german_time() is in helper-env.R

# English names built from the day and month numbers, not from any locale
en_day <- function(d) {
  c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(d)$wday + 1]
}
en_mon_day <- function(d) paste(month.abb[as.POSIXlt(d)$mon + 1], format(d, "%d"))

example_result <- local({
  res <- NULL
  function() {
    if (is.null(res)) res <<- withr::with_seed(1, canhrActi(example_agd(1), output_summary = FALSE))
    res
  }
})

axis_labels <- function(p, axis) {
  labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]][[axis]]$get_labels()
  unname(labs[!is.na(labs)])
}

test_that("the English formatter names days, months and AM/PM in English and restores the locale", {
  before <- Sys.getlocale("LC_TIME")
  t <- as.POSIXct("2024-01-06 15:04:05", tz = "UTC")
  expect_identical(.format_english(t, "%A %a %B %b %I %p"), "Saturday Sat January Jan 03 PM")
  expect_identical(.format_english(as.Date("2024-03-01") + 0:1, "%a %e %b"), c("Fri  1 Mar", "Sat  2 Mar"))
  expect_identical(.format_english(as.Date(NA), "%A"), NA_character_)
  expect_identical(.in_c_time(format(as.Date("2024-10-01"), "%B")), "October")
  expect_identical(Sys.getlocale("LC_TIME"), before)
})

test_that("the ActiLife exports keep English day names and AM/PM under a German LC_TIME", {
  local_german_time()
  res <- example_result()
  out <- withr::local_tempdir()
  suppressMessages(export_canhrActi(res, out, prefix = "one"))
  daily <- read.csv(file.path(out, "one_DailyDetailed.csv"))
  expect_identical(daily$Day.of.Week, en_day(as.Date(daily$Date, "%m/%d/%Y")))
  hourly <- read.csv(file.path(out, "one_HourlyDetailed.csv"))
  expect_identical(hourly$Day.of.Week, en_day(as.Date(hourly$Date, "%m/%d/%Y")))
  expect_true(all(grepl("^(0[1-9]|1[0-2]):00 (AM|PM)$", hourly$Hour)))

  bat <- withr::with_seed(1, canhrActi.batch(c(example_agd(1), example_agd(2)), verbose = FALSE, export = FALSE))
  suppressMessages(export_canhrActi(bat, out, prefix = "all"))
  daily <- read.csv(file.path(out, "all_DailyDetailed.csv"))
  expect_identical(daily$Day.of.Week, en_day(as.Date(daily$Date, "%m/%d/%Y")))
  hourly <- read.csv(file.path(out, "all_HourlyDetailed.csv"))
  expect_true(all(grepl("^(0[1-9]|1[0-2]):00 (AM|PM)$", hourly$Hour)))

  bouts <- suppressMessages(export_sedentary_bouts(res))
  times <- unlist(bouts[grep("Bout.(Start|End)$", names(bouts))])
  expect_length(times, 2 * nrow(bouts))
  expect_true(all(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} (AM|PM)$", times)))
  expect_identical(weekdays(as.Date("2024-01-06")), "Samstag")
})

test_that("the sleep export keeps AM/PM under a German LC_TIME", {
  local_german_time()
  expect_identical(.format.actilife.datetime(as.POSIXct("2024-01-06 15:04:05", tz = "UTC")),
                   "1/6/2024 3:04:05 PM")
  out <- withr::local_tempdir()
  invisible(capture.output(canhrActi.sleep(example_agd(1), output_dir = out, verbose = FALSE)))
  details <- read.csv(file.path(out, "BatchSleepExportDetails.csv"))
  times <- c(details$In.Bed.Time, details$Out.Bed.Time, details$Onset)
  expect_true(length(times) > 0 && all(grepl(" (AM|PM)$", times)))
})

test_that("plot titles and axes keep English month and day names under a German LC_TIME", {
  local_german_time()
  res <- example_result()
  mon <- paste0("^(", paste(month.abb, collapse = "|"), ") [0-9]{2}$")

  p <- plot_intensity(res)
  expect_identical(p$data$date_label, en_mon_day(p$data$date))
  expect_match(plot_wear_time_check(res)$labels$subtitle, "^October 07, 2025 [|]")
  labs <- axis_labels(plot_activity_profile(res), "x")
  expect_true(length(labs) > 0 && all(grepl(mon, labs)))
  p <- plot_daily_summary(res)
  d <- if (inherits(p, "patchwork")) p[[1]]$data else p$data
  expect_identical(d$date_label, en_mon_day(d$date))

  for (p in list(plot_sedentary_heatmap(res$fragmentation$bouts), plot_sedentary_occurrence(res$fragmentation$bouts))) {
    expect_true(all(grepl(mon, levels(p$data$date_label))))
  }
  labs <- axis_labels(plot_hourly_heatmap(res$fragmentation), "y")
  expect_true(length(labs) > 0 && all(grepl(mon, labs)))

  days <- as.Date("2025-10-07") + 0:7
  labs <- axis_labels(plot_actogram(res$epoch_data$axis1, res$epoch_data$timestamp), "y")
  expect_setequal(labs, paste(substr(en_day(days), 1, 3), format(days, "%m-%d")))
  labs <- axis_labels(plot_activity_heatmap_wear(res$epoch_data[, c("timestamp", "axis1")]), "y")
  expect_true(length(labs) > 0 && all(grepl("^(Sun|Mon|Tue|Wed|Thu|Fri|Sat) [0-9]{2}/[0-9]{2}$", labs)))

  slp <- canhrActi.sleep(example_agd(1), export = FALSE, verbose = FALSE)
  expect_match(plot_sleep(slp)$labels$subtitle, "^Period 1: Oct 07, 2025 [|]")
  expect_identical(weekdays(as.Date("2024-01-06")), "Samstag")
})
