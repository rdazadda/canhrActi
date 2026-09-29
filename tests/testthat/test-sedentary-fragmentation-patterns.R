# The fragmentation exports that test-sedentary-fragmentation.R does not reach
# (R/sedentary_fragmentation.R): activity.balance.index, hourly.fragmentation.pattern,
# compare.survival.curves, plot_distribution_comparison, plot_hourly_heatmap, the power-law
# line of plot() and the days sedentary.fragmentation() counts

# 08:00 to 10:59 at one minute: sedentary runs of 20, 15, 22 and 30 minutes. The 3 minute
# gap between 15 and 22 is bridged at a 5 minute break length, so the bouts are 20, 40 and
# 30 minutes, and the last one runs over 10:00.
.frag_day <- function(epoch = 60) {
  lab <- rep(c("sedentary", "light", "sedentary", "light", "sedentary", "light", "sedentary",
               "light"), c(20, 10, 15, 3, 22, 30, 30, 50))
  list(lab = lab,
       ts = seq(as.POSIXct("2024-03-04 08:00:00", tz = "UTC"), by = epoch, length.out = 180))
}

# fragmentation of the two sample recordings, built once
.frag_cache <- new.env(parent = emptyenv())
.frag_sample <- function(i) {
  key <- paste0("sample", i)
  if (is.null(.frag_cache[[key]])) {
    d <- agd.counts(read.agd(example_agd(i), verbose = FALSE))
    .frag_cache[[key]] <- sedentary.fragmentation(freedson(d$axis1), d$timestamp,
                                                  wear.choi(d$axis1))
  }
  .frag_cache[[key]]
}

.geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))

test_that("activity.balance.index splits bouts at 10 and 30 minutes", {
  # 10 is not short and 29.9 is neither; 30 is long
  r <- activity.balance.index(c(5, 9.9, 10, 29.9, 30, 45))
  expect_identical(names(r), c("ABI", "short_time_min", "long_time_min", "short_time_pct",
                               "long_time_pct", "interpretation"))
  expect_equal(r$short_time_min, 14.9)
  expect_equal(r$long_time_min, 75)
  expect_equal(r$ABI, round(14.9 / 89.9, 3))
  expect_equal(r$short_time_pct, round(100 * 14.9 / 129.7, 1))
  expect_equal(r$long_time_pct, round(100 * 75 / 129.7, 1))
  expect_identical(r$interpretation, "Poor balance (concentrated in prolonged bouts)")
})

test_that("an ABI of exactly 0.6 or 0.4 is in the moderate band", {
  # nine 5 minute bouts against one of 30 give 45 / 75
  expect_identical(activity.balance.index(c(rep(5, 9), 30))$ABI, 0.6)
  expect_identical(activity.balance.index(c(rep(5, 9), 30))$interpretation, "Moderate balance")
  expect_identical(activity.balance.index(c(rep(5, 10), 30))$interpretation,
                   "Good balance (well-distributed sedentary time)")
  expect_identical(activity.balance.index(c(rep(5, 4), 30))$ABI, 0.4)
  expect_identical(activity.balance.index(c(rep(5, 4), 30))$interpretation, "Moderate balance")
  expect_identical(activity.balance.index(c(rep(5, 3), 30))$interpretation,
                   "Poor balance (concentrated in prolonged bouts)")
  # no bout under 10 or from 30 up is neutral
  r <- activity.balance.index(c(12, 20, 29))
  expect_identical(r$ABI, 0.5)
  expect_identical(c(r$short_time_min, r$long_time_min), c(0, 0))
})

test_that("the thresholds are arguments and missing durations are dropped", {
  r <- activity.balance.index(c(3, 8, 45, 90, NA), short_threshold = 5, long_threshold = 60)
  expect_equal(c(r$short_time_min, r$long_time_min), c(3, 90))
  expect_equal(r$ABI, round(3 / 93, 3))
  expect_equal(r$short_time_pct, round(100 * 3 / 146, 1))
})

test_that("no durations, or no sedentary time, are reported rather than computed", {
  for (x in list(numeric(0), c(NA_real_, NA_real_))) {
    r <- activity.balance.index(x)
    expect_identical(r$ABI, NA_real_)
    expect_identical(r$interpretation, "Insufficient data")
  }
  r <- activity.balance.index(c(0, 0))
  expect_identical(r$ABI, NA_real_)
  expect_identical(r$interpretation, "No sedentary time")
  expect_identical(c(r$short_time_pct, r$long_time_pct), c(0, 0))
})

test_that("hourly.fragmentation.pattern counts each bout once, in the hour it starts", {
  d <- .frag_day()
  h <- hourly.fragmentation.pattern(d$lab, d$ts)
  expect_identical(names(h), c("hour", "sedentary_min", "n_bouts", "mean_bout_duration",
                               "fragmentation_index"))
  expect_identical(h$hour, 0:23)
  # minutes are sedentary epochs, not bridged: 20 + 15 + 12 at 08, 10 + 20 at 09, 10 at 10
  expect_equal(h$sedentary_min[9:11], c(47, 30, 10))
  expect_identical(h$n_bouts[9:11], c(2L, 1L, 0L))
  expect_equal(h$mean_bout_duration[9:11], c(30, 30, NA))
  expect_equal(h$fragmentation_index[9:11], c(2 / 47, 1 / 30, 0))
  expect_identical(sum(h$sedentary_min[-(9:11)]), 0)
  expect_identical(sum(!is.na(h$fragmentation_index[-(9:11)])), 0L)
  # the bouts are detect.sedentary.bouts() with the same bridging
  b <- detect.sedentary.bouts(d$lab, d$ts, min_break_length = 5)
  expect_equal(b$duration_min, c(20, 40, 30))
  expect_identical(sum(h$n_bouts), nrow(b))
})

test_that("the wear mask, the break length and the epoch change what is counted", {
  d <- .frag_day()
  wear <- rep(TRUE, 180)
  wear[1:20] <- FALSE
  h <- hourly.fragmentation.pattern(d$lab, d$ts, wear_time = wear)
  expect_equal(h$sedentary_min[9], 27)
  expect_identical(h$n_bouts[9], 1L)
  # a 2 minute break length does not bridge the 3 minute gap
  h2 <- hourly.fragmentation.pattern(d$lab, d$ts, min_break_length = 2)
  expect_identical(h2$n_bouts[9:10], c(3L, 1L))
  expect_equal(h2$mean_bout_duration[9], mean(c(20, 15, 22)))
  # the same labels at 30 s span 08:00 to 09:29; four bouts of 10, 7.5, 11 and 15 minutes
  # all start before 09:00
  d30 <- .frag_day(epoch = 30)
  h3 <- hourly.fragmentation.pattern(d30$lab, d30$ts, epoch_length = 30, min_break_length = 1)
  expect_equal(h3$sedentary_min[9:10], c(38.5, 5))
  expect_identical(h3$n_bouts[9:10], c(4L, 0L))
  expect_equal(h3$mean_bout_duration[9], mean(c(10, 7.5, 11, 15)))
  expect_equal(h3$fragmentation_index[9], 4 / 77)
})

test_that("an empty series gives 24 empty hours", {
  h <- hourly.fragmentation.pattern(character(0), as.POSIXct(character(0), tz = "UTC"))
  expect_identical(h$hour, 0:23)
  expect_identical(h$sedentary_min, rep(0, 24))
  expect_identical(h$n_bouts, rep(0L, 24))
  expect_identical(h$fragmentation_index, rep(NA_real_, 24))
})

test_that("compare.survival.curves stacks the curves with a label, W50 and SATP", {
  f1 <- .frag_sample(1)
  f2 <- .frag_sample(2)
  cmp <- compare.survival.curves(first = f1, second = f2)
  expect_identical(names(cmp), c(names(f1$survival_curve), "subject", "W50", "SATP"))
  expect_identical(nrow(cmp), nrow(f1$survival_curve) + nrow(f2$survival_curve))
  a <- cmp[cmp$subject == "first", names(f1$survival_curve)]
  expect_equal(a, f1$survival_curve, check.attributes = FALSE)
  expect_identical(unique(cmp$W50[cmp$subject == "first"]), f1$W50)
  expect_identical(unique(cmp$SATP[cmp$subject == "second"]), f2$SATP)
  # labels win over names, and unnamed results are numbered
  expect_identical(unique(compare.survival.curves(first = f1, f2, labels = c("A", "B"))$subject),
                   c("A", "B"))
  expect_identical(unique(compare.survival.curves(f1, f2)$subject), c("Subject_1", "Subject_2"))
})

test_that("a result with too few bouts for a curve is skipped, and none at all is an error", {
  d <- .frag_day()
  few <- sedentary.fragmentation(d$lab, d$ts)
  expect_null(few$survival_curve)
  cmp <- compare.survival.curves(few = few, sample = .frag_sample(1))
  expect_identical(unique(cmp$subject), "sample")
  expect_error(compare.survival.curves(few), "No valid survival curves found")
  expect_error(compare.survival.curves(), "At least one fragmentation result required")
})

test_that("plot_distribution_comparison draws the empirical survival and the two fits", {
  f1 <- .frag_sample(1)
  fit <- f1$distribution_fit
  p <- plot_distribution_comparison(f1)
  expect_identical(.geoms(p), c("GeomStep", "GeomLine", "GeomLine", "GeomLabel"))
  b <- ggplot2::ggplot_build(p)
  d <- sort(f1$bouts$duration_min)
  n <- length(d)
  # plotting positions 1 - (i - 0.5) / n on log-log axes
  expect_equal(b$data[[1]]$x, log10(d))
  expect_equal(b$data[[1]]$y, log10(1 - (seq_len(n) - 0.5) / n))
  # a power law is a line of slope 1 - alpha on log-log axes
  pl <- b$data[[2]]
  expect_identical(nrow(pl), 200L)
  expect_equal(diff(pl$y) / diff(pl$x), rep(1 - fit$power_law_alpha, 199))
  # an exponential loses lambda / log(10) per minute on a log scale
  ex <- b$data[[3]]
  expect_equal(diff(ex$y) / diff(10^ex$x), rep(-fit$exponential_lambda / log(10), 199))
  expect_identical(vapply(b$data[1:3], function(z) unique(z$colour), ""),
                   c("#2ECC71", "#236192", "#E74C3C"))
  expect_identical(p$labels$title, "Bout Duration Distribution: Power-Law vs Exponential")
  expect_identical(p$labels$subtitle,
                   sprintf("N=%d bouts | %s | SATP=%.3f", n, fit$interpretation, f1$SATP))
  expect_identical(p$labels$x, "Bout Duration (minutes, log scale)")
  expect_match(b$data[[4]]$label, sprintf("Alpha: %.2f", fit$power_law_alpha), fixed = TRUE)
})

test_that("the fits can be left out, the axes kept linear or the palette made colour-blind safe", {
  f1 <- .frag_sample(1)
  expect_identical(.geoms(plot_distribution_comparison(f1, show_fits = FALSE)),
                   c("GeomStep", "GeomLabel"))
  p <- plot_distribution_comparison(f1, log_scale = FALSE)
  expect_equal(ggplot2::layer_data(p, 1)$x, sort(f1$bouts$duration_min))
  expect_identical(p$labels$y, "Survival Probability P(X > x)")
  b <- ggplot2::ggplot_build(plot_distribution_comparison(f1, colorblind_safe = TRUE))
  expect_identical(vapply(b$data[1:3], function(z) unique(z$colour), ""),
                   c("#009E73", "#0072B2", "#D55E00"))
  # without a stored fit the same comparison is computed from the bouts
  f <- f1
  f$distribution_fit <- NULL
  expect_identical(plot_distribution_comparison(f)$labels$subtitle,
                   plot_distribution_comparison(f1)$labels$subtitle)
  expect_error(plot_distribution_comparison(list(bouts = data.frame())), "No bout data available")
})

test_that("the fitted curves start at the fitted xmin and carry the tail fraction there", {
  f1 <- .frag_sample(1)
  fit <- f1$distribution_fit
  d <- f1$bouts$duration_min
  b <- ggplot2::ggplot_build(plot_distribution_comparison(f1))
  for (i in 2:3) {
    expect_equal(min(10^b$data[[i]]$x), fit$xmin)
    expect_equal(10^b$data[[i]]$y[1], mean(d >= fit$xmin))
  }
})

test_that("plot()'s power-law line starts at the fitted xmin and carries the tail fraction", {
  f1 <- .frag_sample(1)
  d <- f1$bouts$duration_min
  l <- ggplot2::layer_data(plot(f1, type = "histogram"), 2)
  expect_equal(min(l$x), f1$alpha_xmin)
  expect_equal(l$y[1], mean(d >= f1$alpha_xmin) * (f1$alpha - 1) / f1$alpha_xmin)
  # a power-law density falls with slope -alpha on log-log axes
  expect_equal(diff(log(l$y)) / diff(log(l$x)), rep(-f1$alpha, 99))
  # three 1 minute bouts give no alpha, and the histogram is drawn without the line
  ts <- seq(as.POSIXct("2024-03-04 08:00:00", tz = "UTC"), by = 60, length.out = 60)
  few <- sedentary.fragmentation(rep(rep(c("sedentary", "light"), c(1, 19)), 3), ts)
  expect_identical(few$alpha, NA_real_)
  expect_identical(.geoms(plot(few, type = "histogram")), c("GeomBar", "GeomVline", "GeomLabel"))
})

test_that("plot_hourly_heatmap tiles hour against day with each metric's labels", {
  d <- .frag_day()
  f <- list(bouts = detect.sedentary.bouts(d$lab, d$ts, min_break_length = 5))
  p <- plot_hourly_heatmap(f)
  expect_identical(.geoms(p), "GeomTile")
  expect_identical(p$labels$title, "Hourly Sedentary Pattern Heatmap")
  expect_identical(p$labels$subtitle, "Number of sedentary bouts per hour")
  expect_identical(p$scales$get_scales("fill")$name, "Bouts")
  expect_identical(p$scales$get_scales("x")$labels, sprintf("%02d:00", seq(0, 23, by = 3)))
  expect_identical(unique(p$data$date), as.Date("2024-03-04"))
  expect_identical(sort(unique(p$data$hour)), c(8L, 9L))
  pd <- plot_hourly_heatmap(f, metric = "duration")
  expect_identical(pd$labels$subtitle, "Total sedentary duration per hour")
  expect_identical(pd$scales$get_scales("fill")$name, "Duration\n(min)")
  pf <- plot_hourly_heatmap(f, metric = "fragmentation")
  expect_identical(pf$labels$subtitle,
                   "Bouts per minute of sedentary time (higher = more fragmented)")
  expect_error(plot_hourly_heatmap(list(bouts = data.frame())), "No bout data available")
})

test_that("each hour of each day is one tile holding its bouts, minutes and bouts per minute", {
  d <- .frag_day()
  f <- list(bouts = detect.sedentary.bouts(d$lab, d$ts, min_break_length = 5))
  p <- plot_hourly_heatmap(f)
  expect_identical(nrow(p$data), 2L)
  expect_identical(p$data$hour, c(8L, 9L))
  expect_equal(p$data$bout_count, c(2, 1))
  expect_equal(p$data$total_duration, c(60, 30))
  expect_equal(p$data$fragmentation, c(2 / 60, 1 / 30))
  expect_equal(p$data$value, c(2, 1))
  expect_equal(plot_hourly_heatmap(f, metric = "duration")$data$value, c(60, 30))
  expect_equal(plot_hourly_heatmap(f, metric = "fragmentation")$data$value, c(2 / 60, 1 / 30))
})

test_that("a bout's day is taken in the zone of its timestamps", {
  ts <- seq(as.POSIXct("2024-03-04 20:00:00", tz = "America/Anchorage"), by = 60, length.out = 60)
  lab <- rep(c("sedentary", "light"), c(30, 30))
  p <- plot_hourly_heatmap(list(bouts = detect.sedentary.bouts(lab, ts)))
  expect_identical(unique(p$data$date), as.Date("2024-03-04"))
  expect_identical(unique(p$data$hour), 20L)
})

test_that("the days counted and the daily table are the days of the timestamps' own clock", {
  # two days in Alaska with 40 sedentary minutes in every hour; in UTC they touch three dates
  ts <- seq(as.POSIXct("2024-03-05 00:00:00", tz = "America/Anchorage"), by = 60, length.out = 2880)
  lab <- rep(rep(c("sedentary", "light"), c(40, 20)), 48)
  f <- sedentary.fragmentation(lab, ts, compare_distributions = FALSE)
  expect_identical(f$n_days_analyzed, 2L)
  expect_identical(f$daily_fragmentation$date, c("2024-03-05", "2024-03-06"))
  expect_equal(f$daily_fragmentation$sedentary_min, c(960, 960))
  expect_identical(f$daily_fragmentation$n_bouts, c(24L, 24L))
  # a recording without a sedentary bout counts its days the same way
  expect_identical(sedentary.fragmentation(rep("light", 2880), ts)$n_days_analyzed, 2L)
})
