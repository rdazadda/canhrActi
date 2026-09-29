# R/visualization_advanced.R functions that test-visualization-advanced.R does not call.
# Day and month names are locale text, so no test reads them.

.built <- function(p) ggplot2::ggplot_build(p)
.geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
.utc <- function(x) as.POSIXct(x, tz = "UTC")

# epochs from Monday 2024-03-04 00:00 UTC, active 08:00 to 22:00 on a five-step cycle
.days <- function(days = 2, by = 60) {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = by, length.out = days * 86400 / by)
  h <- as.numeric(format(ts, "%H"))
  a1 <- ifelse(h >= 8 & h < 22, rep(c(0, 50, 500, 2500, 6000), length.out = length(ts)), 0)
  data.frame(timestamp = ts, axis1 = a1, axis2 = round(a1 * 0.8), axis3 = round(a1 * 0.6))
}
# share of pixels that are not white and number of distinct colours in a PNG
.png_ink <- function(f) {
  m <- matrix(png::readPNG(f)[, , 1:3], ncol = 3)
  c(ink = mean(rowSums(m < 0.98) > 0), colours = nrow(unique(m)))
}

.cache <- new.env()
# canhrActi() on the first example recording
.a60 <- function() {
  if (is.null(.cache$a60)) {
    utils::capture.output(.cache$a60 <- canhrActi(example_agd(1), output_summary = FALSE,
      calculate_mets = FALSE, calculate_fragmentation = FALSE, calculate_circadian = FALSE))
  }
  .cache$a60
}

# two days of minute data: axis1 3000 and VM 4000 from 08:00 to 20:00, zero otherwise
.plateau <- function(by = 60) {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = by, length.out = 2 * 86400 / by)
  h <- as.numeric(format(ts, "%H"))
  on <- h >= 8 & h < 20
  data.frame(timestamp = ts, axis1 = ifelse(on, 3000, 0), axis2 = ifelse(on, 1000, 0), vm = ifelse(on, 4000, 0))
}

test_that("plot_multi_metric overlays the metrics by clock time with the Freedson lines that fit", {
  p <- plot_multi_metric(.plateau())
  expect_identical(.geoms(p), c("GeomLine", "GeomHline", "GeomText"))
  expect_identical(as.vector(table(p$data$metric)[c("Axis 1", "VM")]), c(2880L, 2880L))
  expect_equal(p$data$time[1:3], c(0, 1, 2) / 60)
  b <- .built(p)
  expect_identical(unique(b$data[[1]]$colour), c("#1E90FF", "#DC143C"))
  # 100 and 1952 are under 1.1 times the 4000 maximum; 5725 and 9499 are not
  expect_equal(b$data[[2]]$yintercept, c(100, 1952))
  expect_identical(b$data[[3]]$label, c("sedentary", "light"))
  expect_identical(p$labels$title, "Multi-Metric Comparison")
  expect_identical(p$labels$y, "Counts")
})

test_that("plot_multi_metric scales the cut points to the epoch and takes named or custom thresholds", {
  # 30 s epochs halve the per-minute thresholds
  expect_equal(.built(plot_multi_metric(.plateau(by = 30)))$data[[2]]$yintercept, c(50, 976, 2862.5))
  custom <- c(sedentary = 0, light = 500, moderate = 2500, vigorous = 4200)
  expect_equal(.built(plot_multi_metric(.plateau(), cutpoints = custom))$data[[2]]$yintercept, unname(custom))
  ev <- get_cutpoint_thresholds("evenson")
  expect_equal(.built(plot_multi_metric(.plateau(), cutpoints = "evenson"))$data[[2]]$yintercept,
               c(ev$sedentary, ev$light, ev$moderate))
})

test_that("plot_multi_metric can normalise, facet, take bare column names and keep one day", {
  n <- plot_multi_metric(.plateau(), normalize = TRUE)
  expect_identical(.geoms(n), "GeomLine")
  expect_equal(range(n$data$value), c(0, 100))
  expect_identical(n$labels$y, "Normalized Value (%)")

  f <- plot_multi_metric(.plateau(), facet = TRUE)
  expect_s3_class(f$facet, "FacetWrap")
  expect_identical(f$theme$legend.position, "none")

  v <- plot_multi_metric(.plateau(), metrics = c("axis1", "axis2"))
  expect_identical(unique(v$data$metric), c("axis1", "axis2"))

  one <- plot_multi_metric(.plateau(), date_filter = "2024-03-05")
  expect_identical(nrow(one$data), 2L * 1440L)
  expect_error(plot_multi_metric(.plateau(), date_filter = "2025-01-01"), "No data available for plotting")
})

test_that("plot_multi_metric colours every metric when there are more than eight", {
  d <- .plateau()
  for (k in 2:9) d[[paste0("m", k)]] <- d$axis1 * k
  m <- setNames(as.list(c("axis1", paste0("m", 2:9))), paste("Metric", 1:9))
  lines <- .built(plot_multi_metric(d, metrics = m, show_cutpoints = FALSE))$data[[1]]
  cols <- tapply(lines$colour, lines$group, unique)
  expect_identical(length(cols), 9L)
  expect_identical(as.vector(cols), canhrActi_colors(9))
})

test_that("plot_light_summary gives the minutes per day at or above 10, 100, 1000 and 10000 lux", {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = 60, length.out = 2880)
  # a five minute cycle of 5, 10, 150, 2000 and 20000 lux
  p <- plot_light_summary(data.frame(timestamp = ts, lux = rep(c(5, 10, 150, 2000, 20000), length.out = 2880)))
  expect_identical(.geoms(p), "GeomCol")
  d <- p$data
  minutes <- function(thr) d$minutes[d$threshold == thr]
  expect_equal(minutes(">10 lux"), c(1152, 1152))
  expect_equal(minutes(">100 lux"), c(864, 864))
  expect_equal(minutes(">1000 lux"), c(576, 576))
  expect_equal(minutes(">10000 lux"), c(288, 288))
})

test_that("plot_light_exposure draws its daylight band and threshold lines on the log scale without warnings", {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = 300, length.out = 2 * 288)
  h <- as.numeric(format(ts, "%H"))
  indoor <- data.frame(timestamp = ts, lux = ifelse(h >= 8 & h < 20, 300, 0))
  p <- plot_light_exposure(indoor)
  expect_no_warning(b <- .built(p))
  # the band covers 06:00 to 20:00 from the axis floor, 0.1 lux, to the top
  expect_identical(.geoms(p)[1], "GeomRect")
  expect_equal(unlist(unique(b$data[[1]][, c("xmin", "xmax", "ymin", "ymax")])), c(xmin = 6, xmax = 20, ymin = -1, ymax = Inf))
  # the axis ends at 600 lux, so only the 10 and 100 lux lines are drawn
  lines <- Filter(function(l) inherits(l$geom, "GeomHline"), p$layers)
  expect_equal(unname(vapply(lines, function(l) l$data$yintercept, numeric(1))), c(10, 100))
  expect_identical(Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]$data$label,
                   c("Dim light (10 lux)", "Indoor bright (100 lux)"))
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_warning({
    grDevices::png(f)
    print(p)
    grDevices::dev.off()
  })

  # bright days keep all four lines; the linear scale keeps its full-height band
  bright <- transform(indoor, lux = lux * 100)
  expect_length(Filter(function(l) inherits(l$geom, "GeomHline"), plot_light_exposure(bright)$layers), 4)
  expect_no_warning(lin <- .built(plot_light_exposure(indoor, log_scale = FALSE)))
  expect_equal(unlist(unique(lin$data[[1]][, c("ymin", "ymax")])), c(ymin = -Inf, ymax = Inf))
})

test_that("plot_steps totals each day against the goal and accumulates steps through the day", {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = 60, length.out = 2880)
  h <- as.numeric(format(ts, "%H"))
  steps <- ifelse(h >= 8 & h < 18, ifelse(as.Date(ts) == as.Date("2024-03-04"), 20, 5), 0)
  steps[ts == .utc("2024-03-04 12:00:00")] <- NA
  ps <- plot_steps(data.frame(timestamp = ts, steps = steps))
  expect_identical(names(ps), c("daily", "cumulative", "rate"))

  daily <- ps$daily$data
  expect_equal(daily$steps_value, c(600 * 20 - 20, 600 * 5))
  expect_identical(daily$met_goal, c(TRUE, FALSE))
  expect_equal(daily$percent_goal, c(119.8, 30))
  b <- .built(ps$daily)
  expect_identical(b$data[[1]]$fill, c("#32CD32", "#E74C3C"))
  expect_equal(b$data[[2]]$yintercept, 10000)
  expect_identical(b$data[[3]]$label, "10,000 step goal")
  expect_identical(ps$daily$labels$subtitle, "Goal: 10,000 steps/day")

  cum <- ps$cumulative$data
  expect_equal(as.numeric(tapply(cum$cumulative_steps, cum$date, max)), daily$steps_value)
  expect_identical(all(tapply(cum$cumulative_steps, cum$date, function(x) all(diff(x) >= 0))), TRUE)
  expect_identical(.geoms(ps$rate), "GeomLine")
  expect_identical(ps$rate$data$steps_value, steps)

  ps2 <- plot_steps(data.frame(timestamp = ts, steps = steps), daily_goal = 5000, show_cumulative = FALSE)
  expect_identical(names(ps2), c("daily", "rate"))
  expect_identical(.built(ps2$daily)$data[[3]]$label, "5,000 step goal")
})

test_that("plot_heart_rate shades four zones from 220 minus age and gives the time spent in each", {
  ts <- seq(.utc("2024-03-04 08:00:00"), by = 60, length.out = 100)
  hr <- data.frame(timestamp = ts, hr = rep(c(60, 100, 140, 170), c(50, 30, 15, 5)))
  ph <- plot_heart_rate(hr, age = 30)
  expect_identical(names(ph), c("timeline", "zones"))
  expect_identical(.geoms(ph$timeline), c("GeomRect", "GeomRect", "GeomRect", "GeomRect", "GeomLine"))
  b <- .built(ph$timeline)
  # zones end at 50, 70, 85 and 100 percent of the 190 bpm maximum
  bounds <- do.call(rbind, lapply(b$data[1:4], function(d) d[, c("ymin", "ymax", "fill")]))
  expect_equal(bounds$ymin, c(0, 95, 133, 161.5))
  expect_equal(bounds$ymax, c(95, 133, 161.5, 190))
  expect_identical(bounds$fill, c("#3498DB", "#27AE60", "#F39C12", "#E74C3C"))
  expect_identical(ph$timeline$labels$subtitle, "Age: 30, Max HR: 190 bpm")

  z <- ph$zones$data
  expect_identical(as.character(z$zone), c("Rest", "Fat Burn", "Cardio", "Peak"))
  expect_equal(z$percent, c(50, 30, 15, 5))
  expect_identical(.built(ph$zones)$data[[2]]$label, c("50.0%", "30.0%", "15.0%", "5.0%"))

  # just inside each limit
  edge <- data.frame(timestamp = ts[1:8], hr = c(94.9, 95.1, 132.9, 133.1, 161.4, 161.6, 189.9, 60))
  expect_equal(plot_heart_rate(edge, age = 30)$zones$data$count, c(2, 2, 2, 2))
})

test_that("plot_heart_rate draws only the timeline without an age and adds the activity scatter on request", {
  ts <- seq(.utc("2024-03-04 08:00:00"), by = 60, length.out = 100)
  hr <- data.frame(timestamp = ts, hr = 60 + (1:100) %% 40, counts = (1:100) * 10)
  plain <- plot_heart_rate(hr)
  expect_identical(names(plain), "timeline")
  expect_identical(.geoms(plain$timeline), "GeomLine")
  expect_null(plain$timeline$labels$subtitle)
  expect_identical(names(plot_heart_rate(hr, age = 30, show_zones = FALSE)), "timeline")
  withc <- plot_heart_rate(hr, age = 30, counts_col = "counts")
  expect_identical(names(withc), c("timeline", "zones", "correlation"))
  expect_identical(.geoms(withc$correlation), c("GeomPoint", "GeomSmooth"))
  expect_equal(suppressMessages(.built(withc$correlation))$data[[1]]$x, hr$counts)
})

test_that("plot_intensity_area_from_hourly stacks each band's minutes per hour", {
  hd <- data.frame(Hour = 0:23, Sedentary_min = 40, Light_min = 15, Moderate_min = 4, Vigorous_min = 1)
  p <- plot_intensity_area_from_hourly(hd)
  expect_identical(.geoms(p), "GeomArea")
  expect_identical(levels(p$data$intensity), c("Sedentary", "Light", "Moderate", "Vigorous"))
  expect_identical(as.vector(table(p$data$intensity)), rep(24L, 4))
  area <- .built(p)$data[[1]]
  area <- area[area$x %in% 0:23, ]
  expect_equal(as.vector(tapply(area$ymax, area$x, max)), rep(60, 24))
  expect_equal(as.vector(tapply(area$ymax - area$ymin, area$group, unique)), c(40, 15, 4, 1))
  expect_identical(as.vector(tapply(area$fill, area$group, unique)), c("#3498DB", "#2ECC71", "#F39C12", "#E74C3C"))
  expect_identical(p$labels$subtitle, "Hourly averages | Freedson (1998) cut-points")
  expect_identical(p$labels$caption, "Wear-time filtered data")
  expect_identical(p$labels$y, "Minutes per Hour")

  flat <- .built(plot_intensity_area_from_hourly(hd, stacked = FALSE))$data[[1]]
  flat <- flat[flat$x %in% 0:23, ]
  expect_equal(as.vector(tapply(flat$y, flat$group, unique)), c(40, 15, 4, 1))
  expect_equal(unique(flat$ymin), 0)
})

test_that("plot_intensity_area_from_hourly names the cut points and finds the hour where it can", {
  hd <- data.frame(sedentary = 50, light = rep(10, 30))
  expect_identical(plot_intensity_area_from_hourly(hd, cutpoints = "evenson")$labels$subtitle,
                   "Hourly averages | Evenson (2008) cut-points")
  expect_identical(plot_intensity_area_from_hourly(hd, cutpoints = "canhr")$labels$subtitle,
                   "Hourly averages | CANHR (2025) cut-points")
  expect_identical(plot_intensity_area_from_hourly(hd, cutpoints = "troiano")$labels$subtitle,
                   "Hourly averages | troiano cut-points")
  expect_identical(plot_intensity_area_from_hourly(hd, subtitle = "Mine")$labels$subtitle, "Mine")
  # without an hour column the row number wraps at 24
  long <- plot_intensity_area_from_hourly(hd)$data
  expect_equal(long$hour[long$intensity == "Light"], (0:29) %% 24)
  timed <- data.frame(time = sprintf("%02d:00", 0:23), sedentary = 50, light = 10)
  expect_equal(unique(plot_intensity_area_from_hourly(timed)$data$hour), 0:23)
})

test_that("plot_intensity_area_from_hourly says what is wrong with unusable input", {
  msg <- function(x) .built(plot_intensity_area_from_hourly(x))$data[[1]]$label
  expect_identical(msg(NULL), "No hourly data available")
  expect_identical(msg(data.frame()), "No hourly data available")
  expect_identical(msg(data.frame(x = 1:3)), "Hourly intensity data not in expected format")
  expect_identical(msg(data.frame(mvpa = 1:3)), "No intensity columns found in hourly data")
})

# the Visualization tab passes the five bands as a factor, out of order here on purpose
.pie_summary <- function(minutes = c(300, 600, 60, 30, 10)) {
  lv <- c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")
  data.frame(intensity = factor(c("Light", "Sedentary", "Moderate", "Vigorous", "Very Vigorous"), levels = lv),
             minutes = minutes)
}

test_that("plot_intensity_pie_from_summary draws one wedge per band in order, sized by its share", {
  p <- plot_intensity_pie_from_summary(.pie_summary())
  expect_identical(.geoms(p), c("GeomPolygon", "GeomText", "GeomText"))
  b <- .built(p)
  poly <- b$data[[1]]
  expect_identical(as.vector(tapply(poly$fill, poly$group, unique)),
                   c("#3498DB", "#F1C40F", "#E67E22", "#E74C3C", "#9B59B6"))
  # each wedge runs from the centre along the unit circle and back
  sed <- poly[poly$group == 1, ]
  expect_equal(unlist(sed[c(1, nrow(sed)), c("x", "y")], use.names = FALSE), c(0, 0, 0, 0))
  arc <- sed[-c(1, nrow(sed)), ]
  expect_equal(arc$x^2 + arc$y^2, rep(1, nrow(arc)))
  # sedentary is 60 percent of the time, so its wedge spans 216 degrees
  a <- atan2(arc$x, arc$y)
  expect_equal(a[1] - a[length(a)], 216 * pi / 180)
  # wedges of 8 percent or more carry the big label, 2 to 8 percent the small one, under 2 none
  expect_identical(b$data[[2]]$label, c("60.0%\n(10h 0m)", "30.0%\n(5h 0m)"))
  expect_identical(b$data[[3]]$label, c("6.0%\n(1h 0m)", "3.0%\n(30m)"))
  expect_identical(p$labels$subtitle, "Total: 16.7 hours | Cut-points: Freedson Adult (1998)")
  expect_identical(p$labels$caption, "Wear-time filtered data")
})

test_that("plot_intensity_pie_from_summary drops empty bands, names the cut points and can go unlabelled", {
  s <- .pie_summary(c(0, 600, 60, 30, 10))
  poly <- .built(plot_intensity_pie_from_summary(s))$data[[1]]
  expect_identical(as.vector(tapply(poly$fill, poly$group, unique)), c("#3498DB", "#E67E22", "#E74C3C", "#9B59B6"))
  expect_identical(.geoms(plot_intensity_pie_from_summary(.pie_summary(), show_labels = FALSE)), "GeomPolygon")
  sub <- function(cp) plot_intensity_pie_from_summary(.pie_summary(), cutpoints = cp)$labels$subtitle
  expect_identical(sub("evenson"), "Total: 16.7 hours | Cut-points: Evenson Children (2008)")
  expect_identical(sub("troiano"), "Total: 16.7 hours | Cut-points: Troiano (2008)")
  expect_identical(sub("canhr"), "Total: 16.7 hours | Cut-points: CANHR (2025)")
  expect_identical(sub(c(light = 100)), "Total: 16.7 hours | Cut-points: Custom")
  expect_identical(.built(plot_intensity_pie_from_summary(.pie_summary(rep(0, 5))))$data[[1]]$label,
                   "No activity data available")
  expect_error(plot_intensity_pie_from_summary(data.frame(intensity = "Light")),
               "intensity_summary must have 'intensity' and 'minutes' columns")
})

.durations <- c(1, 1, 2, 2, 2, 3, 5, 5, 8, 10, 12, 15, 20, 30, 45, 60, 90)

test_that("plot_survival_curves steps down to the share of bouts longer than each duration", {
  p <- plot_survival_curves(.durations)
  expect_identical(.geoms(p), c("GeomRibbon", "GeomStep", "GeomHline"))
  sd <- p$data
  # the curve stops at the 95th percentile, 66 min here
  max_t <- min(stats::quantile(.durations, 0.95), 120)
  expect_equal(unname(max_t), 66)
  expect_equal(sd$time, sort(unique(c(0, .durations[.durations <= max_t], max_t))))
  expect_equal(sd$survival, vapply(sd$time, function(t) mean(.durations > t), numeric(1)))
  expect_identical(all(sd$ci_lower <= sd$survival & sd$survival <= sd$ci_upper), TRUE)
  expect_identical(all(sd$ci_lower >= 0 & sd$ci_upper <= 1), TRUE)
  b <- .built(p)
  expect_equal(b$data[[3]]$yintercept, 0.5)
  expect_equal(b$layout$panel_scales_x[[1]]$get_limits(), c(0, 66))
  expect_identical(b$layout$panel_params[[1]]$y$get_labels(), c("0%", "25%", "50%", "75%", "100%"))
  expect_identical(p$labels$subtitle, "n = 17 bouts | Median: 8.0 min")
  expect_identical(p$theme$legend.position, "none")
})

test_that("plot_survival_curves drops missing and non-positive bouts before the ten-bout minimum", {
  expect_error(plot_survival_curves(c(1:9, NA, 0, -1)), "At least 10 valid bouts required")
  p <- plot_survival_curves(c(.durations, NA, 0, -3))
  expect_identical(p$data, plot_survival_curves(.durations)$data)
  expect_identical(p$labels$subtitle, "n = 17 bouts | Median: 8.0 min")
})

test_that("max_time cuts the curve short and show_ci and show_median remove their layers", {
  capped <- plot_survival_curves(.durations, max_time = 20)
  expect_equal(capped$data$time, c(0, 1, 2, 3, 5, 8, 10, 12, 15, 20))
  expect_equal(.built(capped)$layout$panel_scales_x[[1]]$get_limits(), c(0, 20))
  expect_identical(.geoms(plot_survival_curves(.durations, show_ci = FALSE, show_median = FALSE)), "GeomStep")
})

test_that("plot_survival_curves gives each group its own curve, colour and legend", {
  g <- rep(c("A", "B"), length.out = length(.durations))
  p <- plot_survival_curves(.durations, groups = g)
  sd <- p$data
  for (k in c("A", "B")) {
    s <- sd[sd$group == k, ]
    expect_equal(s$survival, vapply(s$time, function(t) mean(.durations[g == k] > t), numeric(1)), info = k)
  }
  steps <- .built(p)$data[[2]]
  expect_identical(as.vector(tapply(steps$colour, steps$group, unique)), c("#1565C0", "#2E7D32"))
  expect_identical(p$theme$legend.position, "right")
})

test_that("plot_survival_curves keeps each group with its own bouts when some bouts are dropped", {
  g <- rep(c("A", "B"), length.out = length(.durations))
  p <- plot_survival_curves(c(NA, .durations), groups = c("A", g))
  s <- p$data[p$data$group == "B", ]
  expect_equal(s$survival, vapply(s$time, function(t) mean(.durations[g == "B"] > t), numeric(1)))
})

test_that("plot_survival_curves colours every group when there are more than four", {
  g <- rep(paste0("S", 1:6), length.out = 60)
  dur <- rep(.durations, length.out = 60)
  steps <- .built(plot_survival_curves(dur, groups = g))$data[[2]]
  cols <- tapply(steps$colour, steps$group, unique)
  expect_identical(length(cols), 6L)
  expect_identical(anyNA(cols), FALSE)
  expect_identical(length(unique(cols)), 6L)
  # past four groups the colours come from the package palette
  expect_identical(as.vector(cols), canhrActi_colors(6))
  four <- g %in% paste0("S", 1:4)
  steps4 <- .built(plot_survival_curves(dur[four], groups = g[four]))$data[[2]]
  expect_identical(as.vector(tapply(steps4$colour, steps4$group, unique)), c("#1565C0", "#2E7D32", "#F57C00", "#C62828"))
})

test_that("plot_vm_heatmap averages the vector magnitude of the three axes in 15 min bins per day", {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = 60, length.out = 2880)
  h <- as.numeric(format(ts, "%H"))
  first <- as.Date(ts) == as.Date("2024-03-04")
  # axes 3k, 4k and 0 give a vector magnitude of 5k
  k <- ifelse(first, h + 1, 2 * (h + 1))
  p <- plot_vm_heatmap(data.frame(timestamp = ts, axis1 = 3 * k, axis2 = 4 * k, axis3 = 0))
  expect_identical(.geoms(p), "GeomTile")
  agg <- p$data
  expect_identical(nrow(agg), 192L)
  expect_equal(sort(unique(agg$time_bin)), seq(0, 23.75, by = 0.25))
  expect_equal(agg$vm, ifelse(agg$date == as.Date("2024-03-04"), 5, 10) * (floor(agg$time_bin) + 1))
  # the first day is the top row
  expect_identical(levels(agg$date_factor), c("2024-03-05", "2024-03-04"))
  fill <- .built(p)$plot$scales$get_scales("fill")
  expect_equal(unname(fill$get_limits()), c(0, unname(stats::quantile(agg$vm, 0.95))))
  expect_identical(p$labels$title, "Vector Magnitude Activity Heatmap")
})

test_that("plot_vm_heatmap bins by the aggregation asked for and uses a VM column as given", {
  d <- .days()
  expect_identical(nrow(plot_vm_heatmap(d, aggregation = "hour")$data), 48L)
  expect_identical(nrow(plot_vm_heatmap(d, aggregation = "minute")$data), 2880L)
  expect_identical(nrow(plot_vm_heatmap(d, aggregation = "5min")$data), 576L)
  # an unknown name falls back to 15 min
  expect_identical(nrow(plot_vm_heatmap(d, aggregation = "fortnight")$data), 192L)
  d$vmc <- 7
  expect_equal(unique(plot_vm_heatmap(d, vm_col = "vmc")$data$vm), 7)
  expect_error(plot_vm_heatmap(d[, c("timestamp", "axis1")]), "Either vm_col or all three axis columns must be provided")
})

# day one worn 06:00 to 18:00 (12 h), day two 10:00 to 18:00 (8 h)
.wear <- function() {
  ts <- seq(.utc("2024-03-04 00:00:00"), by = 60, length.out = 2880)
  h <- as.numeric(format(ts, "%H"))
  first <- as.Date(ts) == as.Date("2024-03-04")
  data.frame(timestamp = ts, wear = (first & h >= 6 & h < 18) | (!first & h >= 10 & h < 18))
}

test_that("plot_wear_time shows the wear pattern over the daily wear hours measured against 10 h", {
  skip_if_not_installed("patchwork")
  w <- .wear()
  p <- plot_wear_time(w)
  expect_s3_class(p, "patchwork")
  expect_identical(p$patches$annotation$title, "Wear Time Analysis")
  tiles <- .built(p[[1]])$data[[1]]
  expect_identical(nrow(tiles), nrow(w))
  expect_identical(tiles$fill, ifelse(w$wear, "#27AE60", "#E74C3C"))
  # the pattern loses its own title inside the combined figure
  expect_null(p[[1]]$labels$title)

  daily <- p[[2]]$data
  expect_equal(daily$wear_hours, c(12, 8))
  expect_identical(daily$valid, c(TRUE, FALSE))
  b <- .built(p[[2]])
  expect_identical(b$data[[1]]$fill, c("#27AE60", "#E74C3C"))
  expect_equal(b$data[[2]]$yintercept, 10)
})

test_that("plot_wear_time returns the pattern alone without the summary and takes a separate wear vector", {
  skip_if_not_installed("patchwork")
  w <- .wear()
  single <- plot_wear_time(w, show_summary = FALSE)
  expect_false(inherits(single, "patchwork"))
  expect_identical(single$labels$title, "Wear Time Pattern")
  v <- plot_wear_time(w[, "timestamp", drop = FALSE], wear_vector = w$wear, title = "Custom")
  expect_identical(v$patches$annotation$title, "Custom")
  expect_equal(v[[2]]$data$wear_hours, c(12, 8))
})

# two hours from 08:00: an hour standing, then 30 min sitting, 20 lying and 10 off
.posture <- function() {
  data.frame(timestamp = seq(.utc("2024-03-04 08:00:00"), by = 60, length.out = 120),
             inclinometer = rep(c("Standing", "sitting", "lying", NA), c(60, 30, 20, 10)))
}

test_that("plot_inclinometer gives each posture's share of the time with the time itself", {
  pie <- plot_inclinometer(.posture(), show_hourly = FALSE)
  d <- pie$data
  expect_identical(as.character(d$posture), c("standing", "sitting", "lying", "off"))
  expect_equal(d$percent, c(50, 25, 50 / 3, 25 / 3))
  expect_identical(d$time_str, c("1:00:00", "0:30:00", "0:20:00", "0:10:00"))
  expect_s3_class(pie$coordinates, "CoordPolar")
})

test_that("plot_inclinometer gives the minutes in each posture for every hour of each day", {
  hourly <- plot_inclinometer(.posture(), show_pie = FALSE)
  d <- hourly$data
  minutes <- function(hr, posture) sum(d$minutes[d$hour == hr & d$posture == posture])
  expect_equal(c(minutes(8, "standing"), minutes(9, "sitting"), minutes(9, "lying"), minutes(9, "off")),
               c(60, 30, 20, 10))
  expect_identical(.geoms(hourly), "GeomBar")
})

test_that("plot_inclinometer names each posture in the legend of a day without all four", {
  two <- .posture()
  two <- rbind(two, data.frame(timestamp = two$timestamp + 86400, inclinometer = rep(c("sitting", "standing"), 60)))
  hourly <- plot_inclinometer(two, show_pie = FALSE)
  expect_identical(names(hourly), c("hourly_03/04/2024", "hourly_03/05/2024"))
  legend <- function(p) {
    s <- .built(p)$plot$scales$get_scales("fill")
    setNames(as.character(s$get_labels()), as.character(s$get_breaks()))
  }
  expect_identical(legend(hourly[[1]]), c(off = "Off", lying = "Lying", sitting = "Sitting", standing = "Standing"))
  expect_identical(legend(hourly[[2]]), c(sitting = "Sitting", standing = "Standing"))
})

test_that("plot_inclinometer_dashboard puts the pie beside the first day's hourly bars", {
  skip_if_not_installed("patchwork")
  p <- plot_inclinometer_dashboard(.posture())
  expect_s3_class(p, "patchwork")
  expect_identical(p[[1]]$labels$title, "Overall Posture Distribution")
})

test_that("quick_plots builds the timeline, heatmap and intensity figures of an analysis", {
  x <- .a60()
  q <- quick_plots(x)
  expect_identical(names(q), c("timeline", "heatmap", "intensity"))
  direct <- list(timeline = plot_daily_timeline(x$epoch_data, show_axes = "axis1", show_cutpoints = TRUE),
                 heatmap = plot_activity_heatmap(x$epoch_data),
                 intensity = plot_intensity_area(x$epoch_data))
  for (nm in names(direct)) {
    expect_identical(q[[nm]]$data, direct[[nm]]$data, info = nm)
    expect_identical(.geoms(q[[nm]]), .geoms(direct[[nm]]), info = nm)
    expect_identical(q[[nm]]$labels, direct[[nm]]$labels, info = nm)
  }
  expect_identical(names(quick_plots(x, plots = "heatmap")), "heatmap")
  # the intensity and posture figures need their columns
  bare <- list(epoch_data = x$epoch_data[, c("timestamp", "axis1")])
  expect_identical(quick_plots(bare, plots = c("intensity", "inclinometer")), list())
  expect_identical(quick_plots(list(daily_summary = data.frame())), list())
})

test_that("quick_plots adds the posture figures when an inclinometer column is present", {
  x <- list(epoch_data = .posture())
  q <- quick_plots(x, plots = "inclinometer")
  expect_identical(names(q), "inclinometer")
  expect_identical(names(q$inclinometer)[1], "pie")
})

test_that("export_all_plots writes a drawn PNG for every figure the columns allow", {
  skip_if_not_installed("png")
  d <- .days(2, by = 300)
  d$hr <- 60 + round(d$axis1 / 100)
  d$intensity <- as.character(freedson(d$axis1))
  out <- file.path(withr::local_tempdir(), "figures")
  msgs <- character()
  res <- withCallingHandlers(export_all_plots(d, output_dir = out, width = 3, height = 2, dpi = 40),
                             message = function(m) {
                               msgs <<- c(msgs, conditionMessage(m))
                               invokeRestart("muffleMessage")
                             })
  # no steps column, so no 06 and 07
  stems <- c("01_daily_timeline", "02_activity_heatmap", "08_heart_rate_timeline", "09_intensity_distribution",
             "10_day_comparison_overlay", "11_day_comparison_facet", "12_weekend_weekday")
  expect_identical(names(res), stems)
  files <- unlist(res, use.names = FALSE)
  expect_identical(basename(files), paste0(stems, ".png"))
  expect_setequal(list.files(out), paste0(stems, ".png"))
  expect_identical(unique(lapply(files, function(f) dim(png::readPNG(f))[1:2])), list(c(80L, 120L)))
  ink <- vapply(files, .png_ink, numeric(2))
  # a blank or near-blank file is named in the failure
  expect_identical(basename(files[ink["ink", ] <= 0.02 | ink["colours", ] <= 5]), character(0))
  expect_identical(sum(startsWith(msgs, "Saved: ")), length(stems))
  expect_match(msgs[length(msgs)], "Exported 7 plots to: ", fixed = TRUE)
})

test_that("export_all_plots writes PDF files, with the step figures when there is a steps column", {
  td <- withr::local_tempdir()
  d <- .days(2, by = 900)
  d$steps <- round(d$axis1 / 40)
  res <- suppressMessages(export_all_plots(d, output_dir = file.path(td, "pdf"), format = "pdf",
                                           width = 3, height = 2))
  expect_identical(names(res), c("01_daily_timeline", "02_activity_heatmap", "06_steps_daily",
                                 "07_steps_cumulative", "10_day_comparison_overlay",
                                 "11_day_comparison_facet", "12_weekend_weekday"))
  files <- unlist(res, use.names = FALSE)
  expect_identical(unique(tools::file_ext(files)), "pdf")
  expect_identical(unique(vapply(files, function(f) rawToChar(readBin(f, "raw", 4)), "", USE.NAMES = FALSE)), "%PDF")
  # an unknown format stops before the folder is made
  never <- file.path(td, "never")
  expect_error(export_all_plots(.days(), output_dir = never, format = "tiff"), "svg")
  expect_false(dir.exists(never))
})

test_that("export_all_plots adds the posture pie and the light summary when those columns exist", {
  d <- .days(2, by = 300)
  d$inclinometer <- rep(c("standing", "sitting", "lying", "off"), length.out = nrow(d))
  d$lux <- ifelse(d$axis1 > 0, 20000, 5)
  out <- file.path(withr::local_tempdir(), "figures")
  res <- suppressMessages(export_all_plots(d, output_dir = out, width = 3, height = 2, dpi = 40))
  expect_identical(all(c("03_inclinometer_pie", "04_light_exposure", "05_light_summary") %in% names(res)), TRUE)
})

test_that("create_visualization_report renders a self-contained HTML report with the four figures", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")
  out <- file.path(withr::local_tempdir(), "report.html")
  # render() leaves its knitting files in tempdir(); remove the ones this call adds
  before <- list.files(tempdir(), full.names = TRUE)
  withr::defer(unlink(setdiff(list.files(tempdir(), pattern = "^file", full.names = TRUE), before), recursive = TRUE))
  # the report template attaches ggplot2; detach it again so later files see the usual search path
  attached <- "package:ggplot2" %in% search()
  withr::defer(if (!attached && "package:ggplot2" %in% search()) detach("package:ggplot2"))

  v <- withVisible(suppressMessages(create_visualization_report(.days(2, by = 900), output_file = out,
                                                                 title = "Pilot week", author = "Field team")))
  expect_identical(v$visible, FALSE)
  expect_identical(v$value, out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "<h1 class=\"title toc-ignore\">Pilot week</h1>", fixed = TRUE)
  expect_match(html, "Field team", fixed = TRUE)
  for (h in c("Daily Activity Timeline", "Activity Heatmap", "Day-to-Day Comparison", "Weekend vs Weekday Patterns")) {
    expect_match(html, paste0("<h1>", h, "</h1>"), fixed = TRUE)
  }
  expect_identical(lengths(gregexpr("data:image/png;base64,", html, fixed = TRUE)), 4L)
})

test_that("plot_wear_time reads a wear_time column when there is no wear column", {
  skip_if_not_installed("patchwork")
  w <- .wear()
  names(w)[names(w) == "wear"] <- "wear_time"
  expect_equal(plot_wear_time(w)[[2]]$data$wear_hours, c(12, 8))
})
