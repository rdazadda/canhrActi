# Plots of a canhrActi() analysis and of a canhrActi.sleep() result (R/plot_intensity.R)

.built <- function(p) ggplot2::ggplot_build(p)
.geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
# the number that follows a label in a subtitle, e.g. "MVPA: 26.5 min"
.number_after <- function(text, label) as.numeric(sub(paste0(".*", label, " ?([0-9.]+).*"), "\\1", text))

.cache <- new.env()
# canhrActi() on the first example recording: 60 s epochs, eight calendar days, three valid
.a60 <- function() {
  if (is.null(.cache$a60)) {
    utils::capture.output(.cache$a60 <- canhrActi(example_agd(1), output_summary = FALSE,
      calculate_mets = FALSE, calculate_fragmentation = FALSE, calculate_circadian = FALSE))
  }
  .cache$a60
}
# one day of 30 s epochs written with write.agd(): worn 08:00 to 22:00 with sedentary and
# light epochs alternating, a 20 min moderate block at 10:00 and a 6.5 min one at 14:00,
# so the minutes in each band are not whole numbers
.a30 <- function() {
  if (is.null(.cache$a30)) {
    at <- function(clock) as.POSIXct(paste("2024-03-04", clock), tz = "UTC")
    ts <- seq(at("00:00:00"), by = 30, length.out = 2880)
    awake <- ts >= at("08:00:00") & ts < at("22:00:00")
    a1 <- ifelse(awake, ifelse(cumsum(awake) %% 2 == 0, 300, 20), 0)
    a1[(ts >= at("10:00:00") & ts < at("10:20:00")) | (ts >= at("14:00:00") & ts < at("14:06:30"))] <- 1500
    f <- file.path(withr::local_tempdir(), "thirty.agd")
    write.agd(data.frame(timestamp = ts, axis1 = a1, axis2 = a1, axis3 = a1, steps = round(a1 / 100)),
              f, epoch_length = 30)
    utils::capture.output(.cache$a30 <- canhrActi(f, output_summary = FALSE, calculate_mets = FALSE,
      calculate_fragmentation = FALSE, calculate_circadian = FALSE))
  }
  .cache$a30
}

# one night in UTC, in bed 23:00 to 06:30: asleep except a 5 min wake bout at 01:00,
# a single wake epoch at 03:00 and a 3 min wake bout at 04:00
.sleep_night <- function() {
  ts <- seq(as.POSIXct("2024-03-04 22:00:00", tz = "UTC"), by = 60, length.out = 9 * 60 + 1)
  sw <- ifelse(ts >= as.POSIXct("2024-03-04 23:00:00", tz = "UTC") &
                 ts < as.POSIXct("2024-03-05 06:30:00", tz = "UTC"), "S", "W")
  sw[format(ts, "%H:%M") %in% c(sprintf("01:%02d", 0:4), "03:00", sprintf("04:%02d", 0:2))] <- "W"
  x <- list(
    epoch_data = data.frame(epoch = seq_along(ts), timestamp = ts, date = as.Date(ts),
                            axis1 = ifelse(sw == "W", 400, 0), sleep_wake = sw),
    sleep_periods = data.frame(period_number = 1L, in_bed_time = "2024-03-04 23:00:00",
                               out_bed_time = "2024-03-05 06:30:00", sleep_efficiency = 96,
                               number_of_awakenings = 3L),
    parameters = list(sleep_algorithm = "cole_kripke")
  )
  class(x) <- c("canhrActi_sleep", "list")
  x
}

test_that("each plot refuses an object that is not the analysis it expects", {
  for (f in list(plot_intensity, plot_intensity_distribution, plot_wear_time_check,
                 plot_activity_profile, plot_mvpa_bouts, plot_daily_summary)) {
    expect_error(f(list(epoch_data = data.frame())), "Input must be a canhrActi_analysis object")
  }
  expect_error(plot_sleep(.a60()), "Input must be a canhrActi_sleep object")
})

test_that("plot_intensity draws one tile per worn epoch in the package intensity colours", {
  x <- .a60()
  p <- plot_intensity(x)
  expect_identical(.geoms(p), "GeomTile")
  worn <- x$epoch_data[x$epoch_data$wear_time, ]
  tiles <- .built(p)$data[[1]]
  expect_identical(nrow(tiles), nrow(worn))
  expect_identical(tiles$fill, unname(canhrActi_palette("intensity")[worn$intensity]))
  expect_equal(tiles$x, as.numeric(format(worn$timestamp, "%H")) + as.numeric(format(worn$timestamp, "%M")) / 60)
  expect_equal(as.numeric(tiles$y), as.numeric(factor(worn$date)))
  # all five levels are kept for the legend
  expect_identical(levels(p$data$intensity), c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  # 648 MVPA minutes over 8 days
  expect_equal(c(x$overall_summary$mvpa_minutes, x$overall_summary$total_days), c(648, 8))
  expect_identical(p$labels$subtitle, sprintf("MVPA: 81 min/day (%.1f%%) | freedson1998 algorithm",
                                              x$overall_summary$mvpa_percent))
  expect_identical(p$labels$caption, "Showing wear time only")

  q <- plot_intensity(x, wear_only = FALSE)
  expect_identical(nrow(q$data), nrow(x$epoch_data))
  expect_identical(q$labels$caption, "Showing all epochs")
})

test_that("plot_intensity returns a plotly widget when asked for an interactive plot", {
  skip_if_not_installed("plotly")
  x <- .a60()
  x$epoch_data <- x$epoch_data[1:120, ]
  # ggplotly() leaves a small PNG in tempdir(); remove what this call adds
  before <- list.files(tempdir(), full.names = TRUE)
  withr::defer(unlink(setdiff(list.files(tempdir(), pattern = "^file", full.names = TRUE), before)))
  w <- plot_intensity(x, interactive = TRUE)
  expect_s3_class(w, "plotly")
  expect_s3_class(w, "htmlwidget")
  traces <- unique(vapply(w$x$data, function(t) as.character(t$name), character(1)))
  expect_setequal(traces, unique(x$epoch_data$intensity[x$epoch_data$wear_time]))
})

test_that("plot_intensity_distribution bars give each level's share of wear time and its minutes", {
  x <- .a60()
  s <- x$intensity_summary
  p <- plot_intensity_distribution(x)
  expect_identical(.geoms(p), c("GeomCol", "GeomText"))
  b <- .built(p)
  expect_equal(b$data[[1]]$xmax, s$percentage)
  expect_identical(b$data[[1]]$fill, unname(canhrActi_palette("intensity")[s$intensity]))
  expect_identical(b$data[[2]]$label, sprintf("%.1f%% (%d min)", s$percentage, as.integer(s$minutes)))
  expect_equal(b$layout$panel_params[[1]]$x.range, c(0, max(s$percentage) * 1.3))
  expect_identical(p$labels$subtitle, sprintf("Total wear time: %.1f hours | MVPA: 648 minutes",
                                              x$overall_summary$total_wear_hours))
  expect_identical(p$labels$caption, "Intensity algorithm: freedson1998")
})

test_that("the pie style stacks the five shares to 100 percent on polar coordinates", {
  x <- .a60()
  p <- plot_intensity_distribution(x, style = "pie")
  expect_s3_class(p$coordinates, "CoordPolar")
  b <- .built(p)
  wedges <- b$data[[1]]
  expect_equal(sum(wedges$ymax - wedges$ymin), 100)
  expect_equal(wedges$ymax - wedges$ymin, x$intensity_summary$percentage)
  expect_identical(b$data[[2]]$label, sprintf("%.1f%%", x$intensity_summary$percentage))
})

test_that("plot_intensity_distribution works for 30 s epochs, where minutes are not whole", {
  x <- .a30()
  s <- x$intensity_summary
  expect_identical(s$minutes[s$intensity == "sedentary"], 406.5)
  p <- plot_intensity_distribution(x)
  labels <- .built(p)$data[[2]]$label
  shown <- .number_after(labels, "\\(")
  expect_equal(shown, s$minutes, tolerance = 0.5, scale = 1)
  expect_equal(.number_after(p$labels$subtitle, "MVPA:"), 26.5, tolerance = 0.5, scale = 1)
})

test_that("plot_wear_time_check shades every non-wear epoch of the chosen day under the counts line", {
  x <- .a60()
  p <- plot_wear_time_check(x, date = "2025-10-10")
  expect_identical(.geoms(p), c("GeomRect", "GeomLine"))
  day <- x$epoch_data[x$epoch_data$date == as.Date("2025-10-10"), ]
  b <- .built(p)
  expect_identical(nrow(b$data[[1]]), sum(!day$wear_time))
  expect_identical(nrow(b$data[[2]]), nrow(day))
  expect_equal(unique(b$data[[1]]$ymax), max(day$axis1) * 1.1)
  expect_equal(b$layout$panel_params[[1]]$y.range, c(0, max(day$axis1) * 1.1))
  # 1146 worn and 294 unworn minutes
  expect_match(p$labels$subtitle, " | Wear: 19.1 hours | Non-wear: 4.9 hours", fixed = TRUE)
  expect_identical(p$labels$caption, "Wear time algorithm: choi | Red shading indicates detected non-wear periods")
})

test_that("plot_wear_time_check defaults to the first day and stops on a day with no data", {
  x <- .a60()
  p <- plot_wear_time_check(x)
  expect_identical(nrow(p$data), sum(x$epoch_data$date == x$epoch_data$date[1]))
  expect_match(p$labels$subtitle, " | Wear: 3.5 hours | Non-wear: 0.0 hours", fixed = TRUE)
  expect_error(plot_wear_time_check(x, date = "2030-01-01"), "No data found for date: 2030-01-01")
})

test_that("plot_wear_time_check reports hours from the epoch length", {
  x <- .a30()
  p <- plot_wear_time_check(x)
  expect_equal(.number_after(p$labels$subtitle, "\\| Wear:"), 14)
  expect_equal(.number_after(p$labels$subtitle, "Non-wear:"), 10)
})

test_that("plot_activity_profile keeps the worn epochs of the date range as intensity-coloured bars", {
  x <- .a60()
  e <- x$epoch_data
  in_range <- e$date >= as.Date("2025-10-08") & e$date <= as.Date("2025-10-09")
  p <- plot_activity_profile(x, date_range = c("2025-10-08", "2025-10-09"))
  expect_identical(.geoms(p), c("GeomRect", "GeomLine"))
  keep <- in_range & e$wear_time
  expect_identical(nrow(p$data), sum(keep))
  bars <- .built(p)$data[[1]]
  expect_equal(bars$ymax, e$axis1[keep])
  expect_identical(bars$fill, unname(canhrActi_palette("intensity")[e$intensity[keep]]))
  expect_equal(bars$xmax - bars$xmin, rep(60, sum(keep)))
  # the subtitle reports the whole recording, not the range
  expect_identical(p$labels$subtitle, sprintf("MVPA: 648 min | Sedentary: 2221 min | Wear time: %.1f hours",
                                              x$overall_summary$total_wear_hours))
  q <- plot_activity_profile(x, date_range = c("2025-10-08", "2025-10-09"), wear_only = FALSE)
  expect_identical(nrow(q$data), sum(in_range))
  expect_identical(q$labels$caption, "Showing all epochs")
})

test_that("plot_activity_profile stops on a malformed range or an empty selection", {
  x <- .a60()
  expect_error(plot_activity_profile(x, date_range = "2025-10-08"), "date_range must be a vector of length 2")
  # 2025-10-12 has no worn minute
  expect_error(plot_activity_profile(x, date_range = c("2025-10-12", "2025-10-12")), "No data to plot")
})

test_that("plot_activity_profile works for 30 s epochs, where minutes are not whole", {
  x <- .a30()
  p <- plot_activity_profile(x)
  expect_equal(.number_after(p$labels$subtitle, "MVPA:"), 26.5, tolerance = 0.5, scale = 1)
  expect_equal(.number_after(p$labels$subtitle, "Sedentary:"), 406.5, tolerance = 0.5, scale = 1)
})

test_that("plot_mvpa_bouts draws one segment per bout and agrees with the analysis' bouted MVPA", {
  x <- .a60()
  p <- plot_mvpa_bouts(x)
  expect_identical(.geoms(p), "GeomSegment")
  seg <- .built(p)$data[[1]]
  expect_equal(nrow(seg), x$overall_summary$mvpa_bout_count)
  bouts <- detect.mvpa.bouts(x$epoch_data$intensity, min_bout_length = 10, drop_time_allowance = 2)
  st <- x$epoch_data$timestamp[bouts$start_index]
  expect_equal(seg$x, as.numeric(format(st, "%H")) + as.numeric(format(st, "%M")) / 60)
  expect_identical(p$labels$subtitle, sprintf("%d bouts detected | Total bouted MVPA: %d minutes | Mean duration: %.1f min",
                                              26L, 451L, mean(bouts$bout_length)))
  expect_equal(x$overall_summary$mvpa_bouted_minutes, 451)
  expect_identical(p$labels$caption, "Minimum bout: 10 min | Drop time allowance: 2 min")
  expect_error(plot_mvpa_bouts(x, min_bout_length = 5000), "No MVPA bouts detected")
})

test_that("plot_mvpa_bouts reads the minimum bout in minutes for 30 s epochs", {
  x <- .a30()
  expect_equal(c(x$overall_summary$mvpa_bout_count, x$overall_summary$mvpa_bouted_minutes), c(1, 20))
  p <- plot_mvpa_bouts(x)
  expect_identical(nrow(.built(p)$data[[1]]), 1L)
  expect_equal(.number_after(p$labels$subtitle, "Total bouted MVPA:"), 20)
})

test_that("plot_mvpa_bouts takes a part-minute drop time allowance", {
  p <- plot_mvpa_bouts(.a30(), drop_time_allowance = 0.5)
  expect_identical(nrow(.built(p)$data[[1]]), 1L)
  expect_identical(p$labels$caption, "Minimum bout: 10 min | Drop time allowance: 0.5 min")
})

test_that("plot_mvpa_bouts leaves out MVPA the device was not worn for, as the analysis does", {
  x <- .a60()
  first <- detect.mvpa.bouts(x$epoch_data$intensity, min_bout_length = 10, drop_time_allowance = 2)[1, ]
  x$epoch_data$wear_time[first$start_index:first$end_index] <- FALSE
  p <- plot_mvpa_bouts(x)
  expect_identical(nrow(.built(p)$data[[1]]), 25L)
  expect_equal(.number_after(p$labels$subtitle, "Total bouted MVPA:"), 451 - first$mvpa_minutes)
})

test_that("plot_mvpa_bouts puts a bout on the calendar day of its own clock", {
  # 20:00 in Anchorage is already the next day in UTC
  at <- function(clock) as.POSIXct(paste("2024-03-04", clock), tz = "America/Anchorage")
  ts <- seq(at("19:00:00"), by = 60, length.out = 120)
  moving <- ts >= at("20:00:00") & ts < at("20:15:00")
  x <- structure(list(
    epoch_data = data.frame(timestamp = ts, wear_time = TRUE,
                            intensity = ifelse(moving, "moderate", "sedentary")),
    parameters = list(epoch_length = 60)
  ), class = "canhrActi_analysis")
  p <- plot_mvpa_bouts(x)
  expect_equal(p$data$start_hour, 20)
  expect_equal(p$data$date, as.Date("2024-03-04"))
})

test_that("plot_sleep draws the in-bed window with sleep bars, wake bouts and the sleep/wake line", {
  withr::local_timezone("UTC")
  x <- .sleep_night()
  p <- plot_sleep(x)
  expect_identical(.geoms(p), c("GeomRect", "GeomRect", "GeomRect", "GeomLine", "GeomHline", "GeomVline"))
  b <- .built(p)
  # 23:00 to 06:30 inclusive is 451 minutes
  expect_identical(nrow(b$data[[1]]), 451L)
  expect_equal(range(b$data[[1]]$x), c(0, 450))
  # asleep 450 minutes less the 9 wake epochs
  expect_identical(nrow(b$data[[2]]), 441L)
  # the 5 and 3 minute wake bouts; the single wake epoch at 03:00 is not a bout
  expect_equal(b$data[[3]]$xmin, c(120, 300))
  expect_equal(b$data[[3]]$xmax, c(124, 302))
  line <- b$data[[4]]
  expect_equal(line$y, as.numeric(x$epoch_data$sleep_wake[x$epoch_data$timestamp >= as.POSIXct("2024-03-04 23:00:00", tz = "UTC") &
                                                            x$epoch_data$timestamp <= as.POSIXct("2024-03-05 06:30:00", tz = "UTC")] == "W"))
  expect_match(p$labels$subtitle, "^Period 1: .* \\| Efficiency: 96.0% \\| TST: 7.2 hr \\| WASO: 18 min \\| Awakenings: 3$")
  expect_identical(p$labels$caption, "Algorithm: cole_kripke | In bed: 23:00 | Out bed: 06:30 | Gray bars = activity intensity")
  expect_identical(p$labels$x, "Minutes Since Bedtime")
})

test_that("plot_sleep can leave out the activity bars and the wake bouts", {
  withr::local_timezone("UTC")
  x <- .sleep_night()
  p <- plot_sleep(x, show_activity = FALSE, show_awakenings = FALSE)
  expect_identical(.geoms(p), c("GeomRect", "GeomLine", "GeomHline", "GeomVline"))
  expect_identical(nrow(.built(p)$data[[1]]), 441L)
  expect_identical(p$labels$caption, "Algorithm: cole_kripke | In bed: 23:00 | Out bed: 06:30")
})

test_that("plot_sleep uses the first result of a batch and stops on a missing night or missing data", {
  withr::local_timezone("UTC")
  x <- .sleep_night()
  batch <- structure(list(results = list(a = x)), class = c("canhrActi_sleep_batch", "list"))
  expect_identical(.built(plot_sleep(batch))$data, .built(plot_sleep(x))$data)
  expect_error(plot_sleep(x, night = 2), "Night 2 not found. Only 1 periods available")
  expect_error(plot_sleep(structure(list(results = list()), class = "canhrActi_sleep_batch")),
               "No sleep results found")
  none <- x
  none$sleep_periods <- data.frame()
  expect_error(plot_sleep(none), "No sleep periods detected")
  noepochs <- x
  noepochs["epoch_data"] <- list(NULL)
  expect_error(plot_sleep(noepochs), "No epoch data available for plotting")
})

test_that("plot_sleep on a canhrActi.sleep() result shows the first night the scorer found", {
  withr::local_timezone("UTC")
  utils::capture.output(s <- canhrActi.sleep(example_agd(1), export = FALSE, verbose = FALSE, parallel = FALSE))
  x <- s$results[[1]]
  sp <- x$sleep_periods[1, ]
  win <- x$epoch_data[x$epoch_data$timestamp >= as.POSIXct(sp$in_bed_time, tz = "UTC") &
                        x$epoch_data$timestamp <= as.POSIXct(sp$out_bed_time, tz = "UTC"), ]
  p <- plot_sleep(s)
  b <- .built(p)
  expect_identical(nrow(b$data[[1]]), nrow(win))
  expect_identical(nrow(b$data[[2]]), sum(win$sleep_wake == "S"))
  expect_equal(b$data[[4]]$y, as.numeric(win$sleep_wake == "W"))
  expect_match(p$labels$subtitle, sprintf("Efficiency: %.1f%%", sp$sleep_efficiency), fixed = TRUE)
  expect_match(p$labels$subtitle, sprintf("Awakenings: %d", sp$number_of_awakenings), fixed = TRUE)
})

test_that("plot_sleep finds the night on a computer outside UTC", {
  withr::local_timezone("America/Anchorage")
  p <- plot_sleep(.sleep_night())
  b <- .built(p)
  expect_identical(nrow(b$data[[1]]), 451L)
  expect_identical(nrow(b$data[[2]]), 441L)
  expect_match(p$labels$caption, "In bed: 23:00 | Out bed: 06:30", fixed = TRUE)
})

test_that("plot_daily_summary stacks four daily panels with epoch counts turned into minutes", {
  skip_if_not_installed("patchwork")
  x <- .a60()
  p <- plot_daily_summary(x)
  expect_s3_class(p, "patchwork")
  panels <- lapply(1:4, function(i) p[[i]])
  expect_identical(vapply(panels, function(q) q$labels$title, character(1)),
                   c("Daily Wear Time", "Daily MVPA", "Daily Sedentary Time", "Daily Steps"))
  d <- panels[[1]]$data
  e <- x$epoch_data
  expect_equal(d$date, sort(unique(e$date)))
  expect_equal(d$wear_hours, as.numeric(tapply(e$wear_time, e$date, sum)) / 60)
  ds <- x$daily_summary
  expect_equal(round(d$wear_hours, 2), ds$wear.hours[match(d$date, ds$date)])
  mvpa <- e$wear_time & e$intensity %in% c("moderate", "vigorous", "very_vigorous")
  expect_equal(d$mvpa, as.numeric(tapply(mvpa, e$date, sum)))
  expect_equal(d$steps, as.numeric(tapply(e$steps, e$date, sum)))
  # reference lines at 10 h of wear, 30 min of MVPA and 10,000 steps
  expect_equal(.built(panels[[1]])$data[[2]]$yintercept, 10)
  expect_equal(.built(panels[[2]])$data[[2]]$yintercept, 30)
  expect_equal(.built(panels[[4]])$data[[2]]$yintercept, 10000)
  # the means are over the 3 valid days, not all 8 calendar days
  v <- as.character(d$date) %in% x$valid_days
  expect_identical(sum(v), 3L)
  expect_identical(p$patches$annotation$subtitle,
                   sprintf("3 valid days | Mean MVPA: %.0f min/day | Mean steps: %.0f/day", mean(d$mvpa[v]), mean(d$steps[v])))

  d30 <- plot_daily_summary(.a30())[[1]]$data
  expect_equal(c(d30$wear_time, d30$wear_hours, d30$mvpa), c(840, 14, 26.5))
})

test_that("plot_daily_summary counts sedentary minutes on wear time only", {
  skip_if_not_installed("patchwork")
  x <- .a60()
  e <- x$epoch_data
  d <- plot_daily_summary(x)[[3]]$data
  expect_equal(d$sedentary, as.numeric(tapply(e$wear_time & e$intensity == "sedentary", e$date, sum)))
  expect_equal(d$sedentary[d$date == as.Date("2025-10-12")], 0)
})

test_that("plot_daily_summary counts MVPA minutes on wear time only", {
  skip_if_not_installed("patchwork")
  x <- .a60()
  e <- x$epoch_data
  mvpa <- e$intensity %in% c("moderate", "vigorous", "very_vigorous")
  x$epoch_data$wear_time[which(mvpa & e$date == as.Date("2025-10-08"))[1:5]] <- FALSE
  d <- plot_daily_summary(x)[[2]]$data
  expect_equal(d$mvpa, as.numeric(tapply(x$epoch_data$wear_time & mvpa, e$date, sum)))
})

test_that("plot_daily_summary keeps the days in date order when their labels sort otherwise", {
  skip_if_not_installed("patchwork")
  x <- .a60()
  # eight days from 2025-03-28, so the April labels sort before the March ones
  shift <- as.numeric(as.Date("2025-10-07") - as.Date("2025-03-28"))
  x$epoch_data$date <- x$epoch_data$date - shift
  x$epoch_data$timestamp <- x$epoch_data$timestamp - shift * 86400
  p <- plot_daily_summary(x)
  d <- p[[1]]$data
  expect_equal(d$date, seq(as.Date("2025-03-28"), by = 1, length.out = 8))
  cols <- c("wear_hours", "mvpa", "sedentary", "steps")
  for (i in 1:4) {
    bars <- .built(p[[i]])$data[[1]]
    expect_equal(bars$y[order(bars$x)], d[[cols[i]]], info = cols[i])
  }
})
