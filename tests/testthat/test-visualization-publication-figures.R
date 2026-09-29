# plot_composition_clock() and create_summary_figure() (R/visualization_publication.R)

# two days at one minute: sleep to 07:00, sedentary to 09:00, light and then moderate in the
# two halves of 09:00, vigorous at 18:00 and light otherwise
.clock_days <- function() {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 2 * 1440)
  lt <- as.POSIXlt(ts)
  int <- ifelse(lt$hour < 7, "sleep", ifelse(lt$hour < 9, "sedentary",
                ifelse(lt$hour == 18, "vigorous", "light")))
  int[lt$hour == 9 & lt$min >= 30] <- "moderate"
  data.frame(timestamp = ts, intensity = int, stringsAsFactors = FALSE)
}

# two days of counts: 0 overnight, rising through the day
.count_days <- function() {
  ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 2 * 1440)
  h <- as.POSIXlt(ts)$hour
  data.frame(timestamp = ts, axis1 = ifelse(h < 7, 0, 100 + 50 * h))
}

.geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
.titles <- function(p) vapply(c(p$patches$plots, list(p)), function(z) z$labels$title, "")

test_that("plot_composition_clock turns epochs into each hour's share of each intensity", {
  d <- plot_composition_clock(.clock_days())$data
  expect_identical(sort(unique(d$hour)), as.numeric(0:23))
  expect_equal(as.vector(tapply(d$percentage, d$hour, sum)), rep(100, 24))
  h9 <- d[d$hour == 9 & d$count > 0, ]
  h9 <- h9[order(as.character(h9$intensity)), ]
  expect_identical(as.character(h9$intensity), c("light", "moderate"))
  # 30 minutes on each of two days
  expect_equal(h9$minutes, c(60, 60))
  expect_equal(h9$percentage, c(50, 50))
  expect_equal(d$percentage[d$hour == 3 & d$intensity == "sleep"], 100)
  expect_equal(d$percentage[d$hour == 18 & d$intensity == "vigorous"], 100)
})

test_that("the clock is a stacked area in polar coordinates with the Okabe-Ito colours", {
  p <- plot_composition_clock(.clock_days())
  expect_identical(.geoms(p), "GeomArea")
  expect_s3_class(p$coordinates, "CoordPolar")
  b <- ggplot2::ggplot_build(p)
  ld <- b$data[[1]]
  # stacked, every hour reaches 100
  expect_equal(as.vector(tapply(ld$ymax, ld$x, max)[as.character(0:23)]), rep(100, 24))
  # groups follow the levels sleep, sedentary, light, moderate, vigorous
  key <- unique(ld[, c("group", "fill")])
  expect_identical(key$fill[order(key$group)],
                   c("#0072B2", "#64748B", "#56B4E9", "#009E73", "#E69F00"))
  expect_identical(b$layout$panel_params[[1]]$theta.labels,
                   c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00"))
  expect_identical(p$labels$title, "24-Hour Time-Use Composition")
  expect_identical(p$labels$y, "% of Hour")
})

test_that("other column names, character times and epoch_length are accepted", {
  d <- .clock_days()
  names(d) <- c("when", "level")
  d$when <- format(d$when, "%Y-%m-%d %H:%M:%S")
  p <- plot_composition_clock(d, timestamp_col = "when", intensity_col = "level",
                              epoch_length = 30, title = "Week 1")
  expect_identical(p$labels$title, "Week 1")
  h9 <- p$data[p$data$hour == 9 & p$data$count > 0, ]
  # a 30 s epoch halves the minutes and leaves the shares alone
  expect_equal(sort(h9$minutes), c(30, 30))
  expect_equal(h9$percentage, c(50, 50))
})

test_that("create_summary_figure puts the heatmap, clock and distribution in two columns", {
  skip_if_not_installed("patchwork")
  p <- create_summary_figure(.count_days())
  expect_s3_class(p, "patchwork")
  expect_identical(.titles(p), c("Activity Heatmap", "24-Hour Pattern", "Intensity Distribution"))
  expect_identical(p$patches$layout$ncol, 2)
  expect_identical(p$patches$annotation$tag_levels, list(c("A", "B", "C")))
  skip_if_not_installed("png")
  f <- file.path(withr::local_tempdir(), "summary.png")
  ggplot2::ggsave(f, p, width = 9, height = 7, dpi = 50)
  img <- png::readPNG(f)
  expect_identical(dim(img)[1:2], c(350L, 450L))
  expect_gt(mean(img[, , 1:3] < 0.98), 0.05)
})

test_that("include picks the panels and wear_time reaches the heatmap", {
  skip_if_not_installed("patchwork")
  d <- .count_days()
  lt <- as.POSIXlt(d$timestamp)
  wear <- !(lt$mday == 1 & lt$hour %in% 10:11)
  # the order is heatmap, clock, distribution whatever order is asked for
  p <- create_summary_figure(d, wear_time = wear, include = c("clock", "heatmap"))
  expect_identical(.titles(p), c("Activity Heatmap", "24-Hour Pattern"))
  hm <- p$patches$plots[[1]]$data
  off <- hm$date == as.Date("2024-01-01") & hm$hour %in% 10:11
  expect_identical(sum(off), 2L)
  expect_true(all(is.na(hm$activity[off])))
  expect_false(anyNA(hm$activity[!off]))
  expect_identical(.titles(create_summary_figure(d, include = "distribution")),
                   "Intensity Distribution")
  expect_error(create_summary_figure(d, include = "none"), "No valid panels specified in 'include'")
})

test_that("the title argument titles the whole figure", {
  skip_if_not_installed("patchwork")
  p <- create_summary_figure(.count_days(), title = "Participant 12")
  expect_identical(p$patches$annotation$title, "Participant 12")
})
