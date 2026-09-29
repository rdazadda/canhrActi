# Charts shared by the dashboard tabs and the Visualization tab (R/dashboard_plots.R),
# checked through the numbers ggplot_build() computes

.built <- function(p) ggplot2::ggplot_build(p)
.geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
.layer <- function(p, geom) .built(p)$data[[which(.geoms(p) == geom)[1]]]
# the sentence an empty-state plot shows, NA for a real chart
.empty_message <- function(p) {
  if (length(p$layers) != 1L || !inherits(p$layers[[1]]$geom, "GeomText")) return(NA_character_)
  .built(p)$data[[1]]$label
}

# six sedentary bouts over two days in UTC: two in the 08:00 hour of the first day,
# so that hour's cell sums them, and a 30 min bout that is the median cell
.bouts <- function() {
  data.frame(
    start_time = as.POSIXct(c("2024-03-04 08:10:00", "2024-03-04 08:40:00", "2024-03-04 09:40:00",
                              "2024-03-04 13:05:00", "2024-03-05 08:20:00", "2024-03-05 22:30:00"),
                            tz = "UTC"),
    duration_min = c(5, 10, 30, 45, 20, 60)
  )
}

.cache <- new.env()
# the sedentary fragmentation canhrActi() reports for the first example recording
.example_fragmentation <- function() {
  if (is.null(.cache$frag)) {
    utils::capture.output(r <- canhrActi(example_agd(1), output_summary = FALSE,
                                         calculate_mets = FALSE, calculate_circadian = FALSE))
    .cache$frag <- r$fragmentation
  }
  .cache$frag
}

test_that("plot_hourly_pattern draws the hourly means as area, line and points on a clock axis", {
  p <- plot_hourly_pattern(data.frame(hour = 23:0, counts = 100 + 23:0), title = "One file")
  expect_identical(.geoms(p), c("GeomArea", "GeomLine", "GeomPoint"))
  line <- .layer(p, "GeomLine")
  expect_equal(line$x, 0:23)
  expect_equal(line$y, 100 + 0:23)
  expect_identical(p$labels$title, "One file")
  expect_identical(p$labels$x, "Hour of Day")
  expect_identical(p$labels$y, "Mean Activity Counts")
  xs <- .built(p)$layout$panel_params[[1]]$x
  expect_equal(xs$breaks, seq(0, 22, 2))
  expect_identical(xs$get_labels(), sprintf("%02d:00", seq(0, 22, 2)))
})

test_that("plot_hourly_pattern averages recordings per clock hour and leaves uncovered hours out", {
  a <- data.frame(hour = c(0:6, 8:11), counts = 10)
  b <- data.frame(hour = c(0:4, 7, 8:23), counts = c(rep(30, 5), NA, rep(30, 16)))
  p <- plot_hourly_pattern(list(a, NULL, data.frame(x = 1), b))
  line <- .layer(p, "GeomLine")
  # hour 7 has only an NA, so no recording covers it
  expect_equal(line$x, setdiff(0:23, 7))
  expected <- ifelse(line$x %in% c(0:4, 8:11), 20, ifelse(line$x %in% 5:6, 10, 30))
  expect_equal(line$y, expected)
})

test_that("plot_hourly_pattern returns the annotated empty plot for no data or unusable frames", {
  expect_identical(.empty_message(plot_hourly_pattern()), "No hourly data")
  expect_identical(.empty_message(plot_hourly_pattern(NULL)), "No hourly data")
  expect_identical(.empty_message(plot_hourly_pattern(list())), "No hourly data")
  expect_identical(.empty_message(plot_hourly_pattern(list(data.frame(x = 1)))),
                   "Could not compute hourly patterns")
  expect_identical(.empty_message(plot_hourly_pattern(data.frame(hour = "a", counts = "b"))),
                   "Could not compute hourly patterns")
  expect_identical(plot_hourly_pattern(NULL, title = "T")$labels$title, "T")
})

test_that("plot_intensity_hours totals each band over every day and labels hours with the share", {
  d1 <- data.frame(sedentary_hrs = c(9, 8), light_hrs = c(4, 5), moderate_hrs = c(1, 1),
                   vigorous_hrs = c(0.5, 0.25))
  # a non-numeric column adds nothing and the missing very_vigorous_hrs counts as zero
  d2 <- data.frame(sedentary_hrs = 7, light_hrs = "x")
  p <- plot_intensity_hours(list(d1, d2))
  expect_identical(.geoms(p), c("GeomCol", "GeomText"))

  hours <- c(24, 9, 2, 0.75, 0)
  bars <- .layer(p, "GeomCol")
  expect_equal(bars$y[order(bars$x)], hours)
  expect_identical(bars$fill[order(bars$x)], c("#94a3b8", "#3b82f6", "#f59e0b", "#f97316", "#ef4444"))
  txt <- .layer(p, "GeomText")
  expect_identical(txt$label[order(txt$x)], sprintf("%.1fh\n(%.1f%%)", hours, hours / sum(hours) * 100))
  expect_identical(levels(p$data$intensity), c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"))
  expect_identical(p$labels$y, "Total Hours")
})

test_that("plot_intensity_hours returns the annotated empty plot when nothing was recorded", {
  expect_identical(.empty_message(plot_intensity_hours(NULL)), "No data yet")
  expect_identical(.empty_message(plot_intensity_hours(list(data.frame()))), "No valid data found")
  expect_identical(.empty_message(plot_intensity_hours(data.frame(sedentary_hrs = 0, light_hrs = 0))),
                   "No valid data found")
})

test_that("plot_circadian_profile shades L5 and M10 where they fall, splitting a window at midnight", {
  hourly <- data.frame(hour = 0:23, mean_counts = 100 + 10 * (0:23), sd_counts = 150)
  p <- plot_circadian_profile(hourly, L5_start = 22, M10_start = "09:30", title = "Profile",
                              value_label = "Counts")
  expect_identical(.geoms(p), c("GeomRect", "GeomRect", "GeomRect", "GeomRibbon", "GeomLine", "GeomPoint"))
  b <- .built(p)
  rects <- do.call(rbind, lapply(b$data[1:3], function(d) d[, c("xmin", "xmax", "fill")]))
  expect_equal(rects$xmin, c(22, 0, 9.5))
  expect_equal(rects$xmax, c(24, 3, 19.5))
  expect_identical(rects$fill, c("#123f60", "#123f60", "#8fb4d1"))
  expect_identical(p$labels$title, "Profile")
  expect_identical(p$labels$y, "Counts")
})

test_that("a POSIXct onset is read as clock time and an unreadable or empty window is left out", {
  hourly <- data.frame(hour = 0:23, mean_counts = 100)
  onset <- as.POSIXct("2024-01-01 02:45:00", tz = "America/Anchorage")
  p <- plot_circadian_profile(hourly, L5_start = onset, M10_start = "junk")
  expect_identical(.geoms(p), c("GeomRect", "GeomLine", "GeomPoint"))
  r <- .built(p)$data[[1]]
  expect_equal(c(r$xmin, r$xmax), c(2.75, 7.75))
  expect_identical(.geoms(plot_circadian_profile(hourly, L5_start = 3, L5_hours = 0, M10_start = NULL)),
                   c("GeomLine", "GeomPoint"))
  expect_identical(plot_circadian_profile(hourly)$labels$y, "Activity (counts/min)")
})

test_that("the SD ribbon runs from the mean minus SD, floored at zero, to the mean plus SD", {
  hourly <- data.frame(hour = 0:23, mean_counts = 100 + 10 * (0:23), sd_counts = 150)
  rib <- .layer(plot_circadian_profile(hourly), "GeomRibbon")
  expect_equal(rib$ymin, pmax(0, hourly$mean_counts - 150))
  expect_equal(rib$ymax, hourly$mean_counts + 150)
  expect_false("GeomRibbon" %in% .geoms(plot_circadian_profile(hourly[, 1:2])))
})

test_that("an unusable profile gives the empty plot titled 24-Hour Activity Profile", {
  for (h in list(NULL, data.frame(hour = 1), data.frame(), data.frame(hour = "a", mean_counts = "b"))) {
    p <- plot_circadian_profile(h)
    expect_identical(.empty_message(p), "Insufficient data for the 24-hour profile")
    expect_identical(p$labels$title, "24-Hour Activity Profile")
  }
  expect_identical(plot_circadian_profile(NULL, title = "Mine")$labels$title, "Mine")
})

test_that("plot_cosinor_fit draws M + A cos(2 pi t / 24 - phi) on a 0.1 h grid, peaking at the acrophase", {
  prof <- data.frame(hour = 0:23, mean_counts = 200 + 150 * cos(2 * pi * (0:23 - 14) / 24))
  p <- plot_cosinor_fit(prof, mesor = 200, amplitude = 150, acrophase = 14, r_squared = 0.82,
                        acrophase_time = "14:00", value_label = "Counts")
  expect_identical(.geoms(p), c("GeomHline", "GeomLine", "GeomPoint", "GeomText"))
  b <- .built(p)
  fit <- b$data[[2]]
  expect_equal(fit$x, seq(0, 24, by = 0.1))
  expect_equal(fit$y, 200 + 150 * cos(2 * pi * fit$x / 24 - 14 / 24 * 2 * pi))
  expect_equal(fit$x[which.max(fit$y)], 14)
  expect_equal(max(fit$y), 350)
  expect_equal(fit$x[which.min(fit$y)], 2)
  expect_equal(b$data[[1]]$yintercept, 200)
  expect_equal(b$data[[3]]$y, prof$mean_counts)
  expect_identical(b$data[[4]]$label, "MESOR")
  expect_identical(p$labels$subtitle, "R-squared = 0.820 | Acrophase = 14:00")
  expect_identical(p$labels$y, "Counts")
})

test_that("plot_cosinor_fit prints the decimal acrophase when no clock label is given", {
  prof <- data.frame(hour = 0:23, mean_counts = 1)
  expect_identical(plot_cosinor_fit(prof, 200, 150, 14.3)$labels$subtitle,
                   "R-squared = NA | Acrophase = 14.3h")
  for (lab in list(NA, "", character(0))) {
    expect_identical(plot_cosinor_fit(prof, 200, 150, 3, r_squared = 0.5, acrophase_time = lab)$labels$subtitle,
                     "R-squared = 0.500 | Acrophase = 3.0h")
  }
})

test_that("plot_cosinor_fit names the missing piece instead of failing", {
  prof <- data.frame(hour = 0:23, mean_counts = 1)
  expect_identical(.empty_message(plot_cosinor_fit(NULL, 1, 1, 1)), "Hourly profile data not available")
  expect_identical(.empty_message(plot_cosinor_fit(data.frame(hour = 1), 1, 1, 1)),
                   "Hourly profile data not available")
  expect_identical(.empty_message(plot_cosinor_fit(prof, mesor = 1, amplitude = 1)),
                   "Cosinor fit parameters not available")
  expect_identical(.empty_message(plot_cosinor_fit(prof, NA, 1, 1)),
                   "Cosinor analysis failed - MESOR could not be calculated")
  expect_identical(.empty_message(plot_cosinor_fit(prof, 1, Inf, 1)),
                   "Cosinor analysis failed - amplitude could not be calculated")
  expect_identical(.empty_message(plot_cosinor_fit(prof, 1, 1, "x")),
                   "Cosinor analysis failed - acrophase could not be calculated")
})

test_that("plot_bout_accumulation accumulates the longest bouts first up to 100 percent", {
  dur <- c(1, 2, 2, 3, 5, 8, 12, 20, 45, 90)
  p <- plot_bout_accumulation(data.frame(duration_min = dur))
  expect_identical(.geoms(p), c("GeomRibbon", "GeomLine", "GeomAbline", "GeomText"))
  line <- .layer(p, "GeomLine")
  expect_equal(line$x, (1:10) * 10)
  expect_equal(line$y, cumsum(sort(dur, decreasing = TRUE)) / sum(dur) * 100)
  rib <- .layer(p, "GeomRibbon")
  expect_equal(rib$ymin, rib$x)
  expect_equal(rib$ymax, line$y)
  x <- sort(dur); n <- 10
  gini <- (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x)) * n / (n - 1)
  expect_identical(.layer(p, "GeomText")$label, paste("Gini =", round(gini, 3)))
  # square axes
  expect_equal(p$coordinates$ratio, 1)
  expect_identical(p$labels$x, "% of Bouts (longest first)")
})

test_that("the accumulation Gini is 0 for equal bouts and 1 when one bout holds all the time", {
  expect_identical(.layer(plot_bout_accumulation(rep(5, 10)), "GeomText")$label, "Gini = 0")
  expect_identical(.layer(plot_bout_accumulation(c(0, 0, 0, 10)), "GeomText")$label, "Gini = 1")
})

test_that("a fragmentation result, its bout table and the bare durations give the same curve and the engine's Gini", {
  fr <- .example_fragmentation()
  line <- .layer(plot_bout_accumulation(fr), "GeomLine")
  expect_identical(.layer(plot_bout_accumulation(fr$bouts), "GeomLine"), line)
  expect_identical(.layer(plot_bout_accumulation(fr$bouts$duration_min), "GeomLine"), line)
  expect_identical(nrow(line), nrow(fr$bouts))
  # the label has three decimals, the engine four
  g <- as.numeric(sub("Gini = ", "", .layer(plot_bout_accumulation(fr), "GeomText")$label, fixed = TRUE))
  expect_lt(abs(g - fr$gini), 5e-4)
})

test_that("plot_bout_accumulation returns the empty plot when no duration is usable", {
  for (x in list(NULL, numeric(0), c(NA, Inf), c(0, 0), data.frame(minutes = 1:3), list(bouts = NULL))) {
    p <- plot_bout_accumulation(x, title = "Acc")
    expect_identical(.empty_message(p), "No bout data")
    expect_identical(p$labels$title, "Acc")
  }
})

test_that("plot_bout_categories sums counts per category in the fixed order and labels count and share", {
  d1 <- data.frame(category = c(">60 min", "1-5 min", "10-20 min"), count = c(2, 5, 3))
  d2 <- data.frame(category = c("1-5 min", "5-10 min"), count = c(1, 4))
  counts <- c(6, 4, 3, 2)
  pct <- round(counts / 15 * 100, 1)
  for (input in list(rbind(d1, d2), list(d1, d2),
                     list(list(bout_distribution = d1), list(bout_distribution = d2)))) {
    p <- plot_bout_categories(input)
    expect_identical(.geoms(p), c("GeomCol", "GeomText"))
    b <- .built(p)
    # the fixed duration order, not the alphabetical one that puts >60 first
    expect_identical(b$layout$panel_params[[1]]$x$get_labels(),
                     c("1-5 min", "5-10 min", "10-20 min", ">60 min"))
    bars <- b$data[[1]]
    bars <- bars[order(bars$x), ]
    expect_equal(bars$y, counts)
    expect_identical(bars$fill, c("#17a589", "#3a7ab0", "#236192", "#e6a000"))
    txt <- .layer(p, "GeomText")
    expect_identical(txt$label[order(txt$x)], paste0(counts, "\n(", pct, "%)"))
    expect_identical(p$labels$subtitle, "Total: 15 bouts")
  }
})

test_that("plot_bout_categories agrees with the engine's own bout distribution", {
  fr <- .example_fragmentation()
  p <- plot_bout_categories(fr)
  bars <- .layer(p, "GeomCol")
  bd <- fr$bout_distribution
  pos <- match(bd$category, c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "30-60 min", ">60 min"))
  expect_equal(bars$y[match(pos, bars$x)], bd$count)
  expect_identical(p$labels$subtitle, paste("Total:", sum(bd$count), "bouts"))
  txt <- .layer(p, "GeomText")
  expect_identical(txt$label[match(pos, txt$x)], paste0(bd$count, "\n(", bd$percent, "%)"))
  # a list of two results doubles every count
  expect_equal(sort(.layer(plot_bout_categories(list(fr, fr)), "GeomCol")$y), sort(2 * bd$count))
})

test_that("plot_bout_categories returns the empty plot when there is nothing to count", {
  for (x in list(NULL, data.frame(), data.frame(category = "1-5 min", count = 0),
                 data.frame(category = "1-5 min"), list(list(a = 1)))) {
    expect_identical(.empty_message(plot_bout_categories(x)), "No distribution data")
  }
})

test_that("plot_bout_histogram_pooled bins by 5 minutes and marks the median and the threshold", {
  fr <- .example_fragmentation()
  p <- plot_bout_histogram_pooled(fr$bouts, alpha = fr$alpha, prolonged_threshold = 45)
  expect_identical(.geoms(p), c("GeomBar", "GeomVline", "GeomVline", "GeomLabel"))
  b <- .built(p)
  expect_equal(sum(b$data[[1]]$count), nrow(fr$bouts))
  expect_equal(unique(round(b$data[[1]]$xmax - b$data[[1]]$xmin, 9)), 5)
  med <- stats::median(fr$bouts$duration_min)
  expect_equal(b$data[[2]]$xintercept, med)
  expect_identical(b$data[[2]]$linetype, "dashed")
  expect_equal(b$data[[3]]$xintercept, 45)
  expect_identical(b$data[[3]]$linetype, "dotted")
  expect_identical(b$data[[4]]$label, "45 min threshold")
  expect_identical(p$labels$subtitle, sprintf("Alpha = %.2f | Median = %.1f min | N = %d bouts",
                                              fr$alpha, med, nrow(fr$bouts)))
})

test_that("an absent or unreadable threshold falls back to 30 minutes and a missing alpha prints NA", {
  b <- .bouts()
  for (thr in list(NULL, NA, "abc")) {
    expect_equal(.built(plot_bout_histogram_pooled(b, prolonged_threshold = thr))$data[[3]]$xintercept, 30)
  }
  expect_identical(plot_bout_histogram_pooled(b, alpha = NULL)$labels$subtitle,
                   "Alpha = NA | Median = 25.0 min | N = 6 bouts")
})

test_that("plot_bout_histogram_pooled shows the empty message in the colour asked for", {
  for (x in list(NULL, data.frame(), data.frame(minutes = 1))) {
    p <- plot_bout_histogram_pooled(x, empty_message = "Nothing here", empty_color = "red")
    d <- .built(p)$data[[1]]
    expect_identical(d$label, "Nothing here")
    expect_identical(d$colour, "red")
  }
})

test_that("plot_bout_histogram_pooled returns the empty plot when called without bouts", {
  p <- plot_bout_histogram_pooled()
  expect_identical(.built(p)$data[[1]]$label, "No bout data")
})

test_that("plot_hourly_bout_duration keeps all 24 hours on the axis and draws the threshold", {
  p <- plot_hourly_bout_duration(.bouts(), threshold = 40)
  expect_identical(.geoms(p), c("GeomBoxplot", "GeomHline"))
  b <- .built(p)
  xs <- b$layout$panel_params[[1]]$x
  expect_identical(xs$limits, as.character(0:23))
  expect_identical(as.vector(xs$breaks), as.character(seq(0, 21, 3)))
  box <- b$data[[1]]
  # the x position of hour h is h + 1
  expect_equal(as.numeric(box$x), c(9, 10, 14, 23))
  expect_equal(box$middle, c(10, 30, 45, 60))
  expect_equal(b$data[[2]]$yintercept, 40)
  expect_identical(p$labels$subtitle, "Boxplot of bout durations by hour (dashed = 40 min threshold)")
})

test_that("each recording is binned in its own local hour before pooling", {
  instant <- as.POSIXct("2024-03-04 15:00:00", tz = "UTC")
  utc <- data.frame(start_time = instant, duration_min = 12)
  ny <- data.frame(start_time = instant, duration_min = 30)
  attr(ny$start_time, "tzone") <- "America/New_York"
  box <- .built(plot_hourly_bout_duration(list(utc, list(bouts = ny))))$data[[1]]
  # 15:00 UTC is 10:00 in New York
  expect_equal(as.numeric(box$x), c(11, 16))
  expect_equal(box$middle, c(30, 12))
})

test_that("plot_hourly_bout_duration accepts a fragmentation result and returns the empty plot for no bouts", {
  expect_identical(.layer(plot_hourly_bout_duration(list(bouts = .bouts())), "GeomBoxplot"),
                   .layer(plot_hourly_bout_duration(.bouts()), "GeomBoxplot"))
  for (x in list(NULL, list(), .bouts()[0, ], data.frame(start_time = .bouts()$start_time))) {
    expect_identical(.empty_message(plot_hourly_bout_duration(x)), "No bout data")
  }
  expect_identical(.empty_message(plot_hourly_bout_duration()), "No bout data")
})

test_that("plot_hourly_bout_frequency counts each bout once in its start hour and fills empty hours with zero", {
  b <- .bouts()
  p <- plot_hourly_bout_frequency(list(b, list(bouts = b)))
  expect_identical(.geoms(p), c("GeomCol", "GeomSmooth"))
  built <- suppressMessages(.built(p))
  bars <- built$data[[1]]
  expect_equal(bars$x, 0:23)
  expect_equal(bars$y, ifelse(0:23 == 8, 6, ifelse(0:23 %in% c(9, 13, 22), 2, 0)))
  expect_equal(built$layout$panel_params[[1]]$x$breaks, seq(0, 21, 3))
  expect_identical(p$labels$y, "Number of Bouts")
})

test_that("plot_hourly_bout_frequency bins each recording in its own local hour before pooling", {
  instant <- as.POSIXct("2024-03-04 15:00:00", tz = "UTC")
  utc <- data.frame(start_time = instant, duration_min = 12)
  ny <- data.frame(start_time = instant, duration_min = 30)
  attr(ny$start_time, "tzone") <- "America/New_York"
  bars <- suppressMessages(.built(plot_hourly_bout_frequency(list(utc, list(bouts = ny)))))$data[[1]]
  # 15:00 UTC is 10:00 in New York
  expect_equal(bars$y[bars$x %in% c(10, 15)], c(1, 1))
  expect_equal(sum(bars$y), 2)
})

test_that("plot_hourly_bout_frequency returns the empty plot when no bout has a start time", {
  for (x in list(NULL, list(), .bouts()[0, ], data.frame(duration_min = 1:3))) {
    expect_identical(.empty_message(plot_hourly_bout_frequency(x)), "No bout data")
  }
})

test_that("plot_hourly_bout_frequency returns the empty plot when called without bouts", {
  expect_identical(.empty_message(plot_hourly_bout_frequency()), "No bout data")
})

test_that("plot_sedentary_heatmap sums minutes per clock hour and day and centres the fill on the median cell", {
  p <- plot_sedentary_heatmap(.bouts(), title = "Heat")
  expect_identical(.geoms(p), "GeomTile")
  cells <- p$data[order(p$data$date, p$data$hour), ]
  expect_equal(cells$hour, c(8, 9, 13, 8, 22))
  expect_equal(cells$date, as.Date(c("2024-03-04", "2024-03-04", "2024-03-04", "2024-03-05", "2024-03-05")))
  expect_equal(cells$duration_min, c(15, 30, 45, 20, 60))
  # one row per calendar day, in date order
  expect_identical(as.integer(p$data$date_label), as.integer(factor(p$data$date)))
  tiles <- .layer(p, "GeomTile")
  # the 30 min cell is the median of the five, so it takes the gradient's middle colour
  expect_identical(tiles$fill[tiles$x == 9 & tiles$y == 1], "#3A7AB0")
  expect_identical(p$labels$title, "Heat")
  expect_identical(p$labels$y, "Date")
})

test_that("plot_sedentary_heatmap takes a fragmentation result and returns the empty plot for no bouts", {
  expect_identical(.layer(plot_sedentary_heatmap(list(bouts = .bouts())), "GeomTile"),
                   .layer(plot_sedentary_heatmap(.bouts()), "GeomTile"))
  for (x in list(NULL, .bouts()[0, ], data.frame(start_time = .bouts()$start_time))) {
    expect_identical(.empty_message(plot_sedentary_heatmap(x)), "No sedentary bout data available")
  }
  expect_identical(.empty_message(plot_sedentary_heatmap()), "No sedentary bout data available")
})

test_that("plot_sedentary_occurrence puts one point per bout at its start clock time, sized and coloured by length", {
  p <- plot_sedentary_occurrence(.bouts())
  expect_identical(.geoms(p), "GeomPoint")
  pts <- .layer(p, "GeomPoint")
  expect_equal(pts$x, c(8 + 10 / 60, 8 + 40 / 60, 9 + 40 / 60, 13 + 5 / 60, 8 + 20 / 60, 22.5))
  expect_equal(as.numeric(pts$y), c(1, 1, 1, 1, 2, 2))
  # sizes run from 1 for the shortest to 8 for the longest bout
  expect_equal(pts$size[c(1, 6)], c(1, 8))
  # the colour scale is centred on 30 minutes
  expect_identical(pts$colour[3], "#FFCD00")
  expect_equal(.built(p)$layout$panel_scales_x[[1]]$get_limits(), c(0, 24))
  expect_identical(.empty_message(plot_sedentary_occurrence(NULL)), "No sedentary bout data available")
  expect_identical(.layer(plot_sedentary_occurrence(list(bouts = .bouts())), "GeomPoint"), pts)
})

test_that("an evening bout of a recording in a local time zone stays on its own day", {
  b <- data.frame(start_time = as.POSIXct("2024-03-04 20:00:00", tz = "America/Anchorage"), duration_min = 30)
  h <- plot_sedentary_heatmap(b)$data
  expect_equal(h$hour, 20)
  expect_equal(h$date, as.Date("2024-03-04"))
  o <- plot_sedentary_occurrence(b)$data
  expect_equal(o$time_of_day, 20)
  expect_equal(o$date, as.Date("2024-03-04"))
})

test_that("plot_sedentary_timeline sums minutes per start hour with the bout count and mean length", {
  p <- plot_sedentary_timeline(.bouts())
  expect_identical(.geoms(p), c("GeomArea", "GeomLine", "GeomPoint", "GeomRect", "GeomRect"))
  hd <- p$data
  expect_equal(hd$hour, 0:23)
  minutes <- rep(0, 24)
  minutes[c(8, 9, 13, 22) + 1] <- c(35, 30, 45, 60)
  expect_equal(hd$duration_min, minutes)
  expect_equal(hd$n_bouts[hd$hour %in% c(8, 9, 13, 22)], c(3, 1, 1, 1))
  expect_equal(hd$avg_duration[hd$hour %in% c(8, 9, 13, 22)], c(35 / 3, 30, 45, 60))
  b <- .built(p)
  expect_equal(b$data[[1]]$y[b$data[[1]]$x %in% 0:23], hd$duration_min)
  # morning and evening bands
  expect_equal(unlist(b$data[[4]][, c("xmin", "xmax")]), c(xmin = 6, xmax = 9))
  expect_equal(unlist(b$data[[5]][, c("xmin", "xmax")]), c(xmin = 17, xmax = 21))
  expect_identical(p$labels$y, "Total Sedentary Minutes")
})

test_that("plot_sedentary_timeline takes a fragmentation result and returns the empty plot for no bouts", {
  expect_identical(plot_sedentary_timeline(list(bouts = .bouts()))$data, plot_sedentary_timeline(.bouts())$data)
  for (x in list(NULL, .bouts()[0, ], data.frame(duration_min = 5))) {
    expect_identical(.empty_message(plot_sedentary_timeline(x)), "No sedentary bout data available")
  }
})

test_that("plot_transition_matrix returns the empty plot when called without a result", {
  expect_identical(.empty_message(plot_transition_matrix()), "No transition data")
  expect_identical(.empty_message(plot_transition_matrix(NULL)), "No transition data")
})
