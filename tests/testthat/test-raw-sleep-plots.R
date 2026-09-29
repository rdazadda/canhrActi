# Tests for R/raw_sleep_plots.R (the three sleep figures) and the parts 2 to 4 side of
# R/raw_export.R (as.ggir.milestone with part, as.ggir.ms4, write.ggir.milestone with
# parts). Reference data live in the folder named by CANHRACTI_GGIR_REF, and the
# fixtures one level up under ggir-study-p34/fixtures; both skip when absent.
# The figures are ggplot2 where GGIR draws base graphics, so the figure tests check
# that each renders and draws the numbers of the parity-checked tables. The MOS2 milestones
# were stored in the system zone of a machine set to America/Anchorage, so the file runs in
# that zone.

withr::local_timezone("America/Anchorage")

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
p34_fixture <- function(case) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", case, "output_din")
}

PLOT_CASES <- list(
  MOS2 = list(out = c("out", "output_din"),           fn = "MOS2E39230594.gt3x"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x"))

plot_path <- function(case, what) {
  cc <- PLOT_CASES[[case]]
  switch(what,
         basic = do.call(ref_file, c(as.list(cc$out), list("meta", "basic", paste0("meta_", cc$fn, ".RData")))),
         ms2   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms2.out", paste0(cc$fn, ".RData")))),
         ms3   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms3.out", paste0(cc$fn, ".RData")))),
         ms4   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms4.out", paste0(cc$fn, ".RData")))))
}

load_env <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  e
}

# The chain from a stored part-1 milestone (read, impute, part 3, part 4), built once and cached.
plot_run <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref()
    pb <- plot_path(case, "basic"); skip_if_no_file(pb)
    x <- read.ggir.milestone(pb)
    x$imputed <- raw.impute(x$meta, x$wear, x$params)
    x$sleep <- raw.sleep.part3(x)
    out <- list(x = x, nights = raw.sleep.nights(x$sleep))
    assign(case, out, envir = cache)
    out
  }
})

# A part-3 milestone read from a fixture folder.
fixture_sleep <- function(case) {
  d <- p34_fixture(case)
  if (!dir.exists(d)) testthat::skip(paste0("fixture not present: ", case))
  ms3 <- list.files(file.path(d, "meta", "ms3.out"), full.names = TRUE)
  if (length(ms3) != 1) testthat::skip(paste0("fixture has no single ms3 file: ", case))
  load_env(ms3[1])
}

# The minimum canhrActi_raw_sleep a figure needs, built from a stored ms3.
sleep_from_ms3 <- function(e, nights = NULL) {
  structure(list(sib.cla.sum = e$sib.cla.sum, L5list = e$L5list, SPTE_start = e$SPTE_start,
                 SPTE_end = e$SPTE_end, tib.threshold = e$tib.threshold,
                 part3_guider = e$part3_guider, SPTE_corrected = e$SPTE_corrected,
                 SleepRegularityIndex = e$SleepRegularityIndex,
                 longitudinal_axis = e$longitudinal_axis, id = e$ID,
                 rec_starttime = e$rec_starttime, desiredtz_part1 = e$desiredtz_part1,
                 epochs = NULL, nights = nights, sib = NULL,
                 settings = list(windowsizes = c(5, 900, 3600), desiredtz = e$desiredtz_part1),
                 file = list(filename = NA_character_),
                 status = list(state = "ok", detection_failed = FALSE)),
            class = "canhrActi_raw_sleep")
}

render_size <- function(p, width = 9, height = 7) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  suppressWarnings(suppressMessages(
    ggplot2::ggsave(f, p, width = width, height = height, dpi = 110)))
  file.size(f)
}

test_that("plot_raw_sleep renders on MOS2 and carries the layers it promises", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep(r$x, nights = r$nights)
  expect_s3_class(p, "ggplot")
  expect_false(.raw.plot.is.empty(p))
  expect_gt(render_size(p, width = 11, height = 13), 5000)
  # the panels are the seven noon-to-noon windows part 3 found, night 1 first
  expect_identical(nrow(r$x$sleep$nights), 7L)
  b <- ggplot2::ggplot_build(p)
  fac <- b$layout$layout$night
  expect_identical(as.character(fac[1]), "Night 1: Tue 07 Oct 2025")
  expect_identical(as.character(fac[7]), "Night 7: Mon 13 Oct 2025")
  expect_identical(p$labels$title, "Sustained inactivity bouts and the guider, night by night")
  expect_true(grepl("7 noon-to-noon nights", p$labels$subtitle, fixed = TRUE))
  expect_true(grepl("111 sustained inactivity bouts over 4 of them", p$labels$subtitle, fixed = TRUE))
  expect_true(grepl("guider HDCZA", p$labels$subtitle, fixed = TRUE))
  expect_true(grepl("MOS2E39230594.gt3x", p$labels$caption, fixed = TRUE))
  expect_true(grepl("America/Anchorage", p$labels$caption, fixed = TRUE))
})

test_that("plot_raw_sleep_nights renders on MOS2", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep_nights(r$nights)
  expect_s3_class(p, "ggplot")
  expect_false(.raw.plot.is.empty(p))
  expect_gt(render_size(p, width = 10, height = 4.4), 5000)
  expect_identical(p$labels$title, "Nights on one axis: bouts against the guider window")
  ep <- attr(r$nights, "canhrActi")$episodes
  expect_identical(nrow(ep), 111L)
  expect_identical(length(which(ep$overlapGuider == 1)), 47L)
  expect_true(grepl("4 nights, 111 bouts, 47 of them inside the sleep period",
                    p$labels$subtitle, fixed = TRUE))
})

test_that("plot_raw_sleep_regularity renders on MOS2 and reports the stored SRI", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep_regularity(r$x)
  expect_s3_class(p, "ggplot")
  expect_false(.raw.plot.is.empty(p))
  expect_gt(render_size(p, width = 9.5, height = 5), 5000)
  sri <- r$x$sleep$SleepRegularityIndex
  expect_identical(sri$SleepRegularityIndex, c(17.619, 30.256, 46.738, 0, 0, 0, 0))
  expect_identical(sri$frac_valid, c(0.1458, 0.9479, 0.9688, 0, 0, 0, 0))
  expect_true(grepl("7 day pairs; 2 with more than 16 of 24 hours valid",
                    p$labels$subtitle, fixed = TRUE))
  expect_true(grepl("those average 38.5", p$labels$subtitle, fixed = TRUE))
  expect_identical(round(mean(c(30.256, 46.738)), 1), 38.5)
  d <- ggplot2::layer_data(p, 2)
  expect_identical(d$y, c(17.619, 30.256, 46.738, 0, 0, 0, 0))
})

test_that("the three figures render on EE too", {
  r <- plot_run("EE")
  expect_gt(render_size(plot_raw_sleep(r$x, nights = r$nights), width = 11, height = 12), 5000)
  expect_gt(render_size(plot_raw_sleep_nights(r$nights), width = 10, height = 5), 5000)
  expect_gt(render_size(plot_raw_sleep_regularity(r$x), width = 9.5, height = 5), 5000)
  # EE carries its own zone, so the caption is machine independent
  expect_true(grepl("Europe/Helsinki", plot_raw_sleep(r$x)$labels$caption, fixed = TRUE))
  expect_identical(nrow(attr(r$nights, "canhrActi")$episodes), 132L)
})

test_that("the sib bars of plot_raw_sleep are sib.cla.sum, epoch for epoch", {
  r <- plot_run("MOS2")
  sleep <- r$x$sleep
  scs <- sleep$sib.cla.sum
  expect_identical(dim(scs), c(111L, 9L))
  p <- plot_raw_sleep(r$x, nights = r$nights)
  # the sib layer is found by its row count
  ld <- lapply(seq_along(p$layers), function(i) ggplot2::layer_data(p, i))
  cand <- which(vapply(ld, function(d) is.data.frame(d) && nrow(d) == nrow(scs) &&
                         all(c("xmin", "xmax", "ymin", "ymax") %in% names(d)), logical(1)))
  expect_identical(length(cand), 1L)
  bar <- ld[[cand]]
  # expected positions recomputed from the table
  tz <- "America/Anchorage"
  nt <- sleep$nights
  mid <- as.POSIXct(paste0(nt$date[match(scs$night, nt$night)], " 00:00:00"), tz = tz)
  onset <- as.POSIXct(as.character(scs$sib.onset.time), format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  endt <- as.POSIXct(as.character(scs$sib.end.time), format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  x0 <- as.numeric(difftime(onset, mid, units = "hours"))
  x1 <- as.numeric(difftime(endt, mid, units = "hours")) + 5 / 3600
  # the layer follows sib.cla.sum's own order
  expect_identical(bar$xmin, x0)
  expect_identical(bar$xmax, x1)
  expect_identical(as.character(scs$sib.onset.time[1]), "2025-10-07T21:11:00-0800")
  expect_identical(as.character(scs$sib.end.time[1]), "2025-10-07T21:18:50-0800")
  expect_equal(bar$xmin[1], 21 + 11 / 60, tolerance = 1e-12)
  expect_equal(bar$xmax[1], 21 + 18 / 60 + 55 / 3600, tolerance = 1e-12)
  # the helper on its own gives the same table
  hb <- .raw.sleep.plot.sibbars(scs, nt, tz, 5)
  expect_identical(hb$xmin, x0)
  expect_identical(hb$xmax, x1)
  expect_identical(hb$night, as.numeric(scs$night))
  expect_identical(hb$definition, as.character(scs$definition))
  expect_identical(hb$hours, as.numeric(scs$tot.sib.dur.hrs))
})

test_that("the bars of plot_raw_sleep_nights are the part-4 episode table", {
  r <- plot_run("MOS2")
  ep <- attr(r$nights, "canhrActi")$episodes
  p <- plot_raw_sleep_nights(r$nights)
  # the two rect layers: the bars, then the guider outline
  rects <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))
  expect_identical(length(rects), 2L)
  bar <- ggplot2::layer_data(p, rects[1])
  gb <- ggplot2::layer_data(p, rects[2])
  expect_identical(nrow(bar), nrow(ep))
  expect_identical(bar$xmin, ep$start)
  expect_identical(bar$xmax, ep$end)
  # night 1 is drawn at the top, row 4 of 4
  expect_equal(sort(unique(ep$night)), c(1, 2, 3, 4))
  expect_identical(unname(bar$ymin[ep$night == 1][1]), 4 - 0.3)
  expect_identical(unname(bar$ymax[ep$night == 1][1]), 4 + 0.3)
  expect_identical(unname(bar$ymin[ep$night == 4][1]), 1 - 0.3)
  gt <- attr(r$nights, "canhrActi")$guiders
  expect_identical(nrow(gb), nrow(gt))
  expect_identical(gb$xmin, gt$guider_onset)
  expect_identical(gb$xmax, gt$guider_wakeup)
  expect_true(all(is.na(gb$fill)))
  expect_equal(gt$guider_onset,
               c(23.0744444444444, 23.4597222222222, 23.7236111111111, 22.0402777777778),
               tolerance = 1e-10)
  expect_equal(gt$guider_wakeup,
               c(31.3338888888889, 31.8597222222222, 31.0708333333333, 31.0708333333333),
               tolerance = 1e-10)
  # cleaningcode 2 nights are faded
  expect_identical(gt$cleaningcode, c(2, 1, 1, 2))
  expect_identical(sort(unique(bar$alpha)), c(0.35, 1))
  expect_equal(sort(unique(ep$night[bar$alpha == 0.35])), c(1, 4))
})

test_that("plot_raw_sleep without a night table uses the part-3 sleep period estimate", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep(r$x)
  expect_false(.raw.plot.is.empty(p))
  ld <- lapply(seq_along(p$layers), function(i) ggplot2::layer_data(p, i))
  # SPTE_start[1] is NA (the partial first day), so three nights have a band
  nt <- r$x$sleep$nights
  expect_equal(nt$SPTE_start,
               c(NA, 23.4597222222222, 23.7236111111111, 22.0402777777778, NA, NA, NA),
               tolerance = 1e-10)
  g <- which(vapply(ld, function(d) is.data.frame(d) && nrow(d) == 3L &&
                      "xmin" %in% names(d) && all(!is.na(d$xmin)), logical(1)))
  expect_true(length(g) >= 1)
  gb <- ld[[g[1]]]
  expect_identical(sort(gb$xmin), sort(nt$SPTE_start[!is.na(nt$SPTE_start)]))
  expect_identical(sort(gb$xmax), sort(nt$SPTE_end[!is.na(nt$SPTE_end)]))
  # with no night table there are no onset or wake markers
  with_marks <- plot_raw_sleep(r$x, nights = r$nights)
  expect_gt(length(with_marks$layers), length(p$layers))
})

test_that("plot_raw_sleep_nights on a part-3 object recomputes the overlap flag and says so", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep_nights(r$x)
  expect_false(.raw.plot.is.empty(p))
  expect_true(grepl("the overlap flag was recomputed here", p$labels$caption, fixed = TRUE))
  ld <- lapply(seq_along(p$layers), function(i) ggplot2::layer_data(p, i))
  bar <- ld[[which(vapply(ld, function(d) is.data.frame(d) && nrow(d) == 111L &&
                            "fill" %in% names(d), logical(1)))[1]]]
  inside <- length(which(bar$fill == .raw.sleep.plot.colours()[["inside"]]))
  # part 4 flags 47 bouts, this figure 29: night 1 has no part-3 SPTE (SPTE_start NA), and
  # part 4 gives it a guider from the other nights' mean, which labels its 18 sibs
  ep <- attr(r$nights, "canhrActi")$episodes
  expect_identical(length(which(ep$overlapGuider == 1)), 47L)
  expect_identical(length(which(ep$overlapGuider == 1 & ep$night == 1)), 18L)
  expect_identical(inside, 29L)
  expect_true(is.na(r$x$sleep$nights$SPTE_start[1]))
  expect_gt(render_size(p, width = 10, height = 4.4), 5000)
})

test_that("every figure gives a one-sentence empty state instead of erroring", {
  # nothing at all
  for (f in list(plot_raw_sleep, plot_raw_sleep_nights, plot_raw_sleep_regularity)) {
    p <- f(list(a = 1))
    expect_s3_class(p, "ggplot")
    expect_true(.raw.plot.is.empty(p))
    expect_true(grepl("Run raw.sleep.part3()", .raw.plot.empty.message(p), fixed = TRUE))
    expect_gt(render_size(p, width = 7, height = 4), 2000)
  }
  # a corrupt recording: part 3 states "skipped"
  s <- structure(list(status = list(state = "skipped"), nights = NULL,
                      SleepRegularityIndex = NA, desiredtz_part1 = ""),
                 class = "canhrActi_raw_sleep")
  for (f in list(plot_raw_sleep, plot_raw_sleep_nights, plot_raw_sleep_regularity)) {
    p <- f(s)
    expect_true(.raw.plot.is.empty(p))
    expect_identical(.raw.plot.empty.message(p),
                     "The recording is corrupt, too short or was skipped, so part 3 estimated nothing.")
  }
  s$status$state <- "too_short_for_sleep"
  expect_true(grepl("less than a fifth of a day",
                    .raw.plot.empty.message(plot_raw_sleep(s)), fixed = TRUE))
  s$status$state <- "detection_failed"
  expect_identical(.raw.plot.empty.message(plot_raw_sleep_nights(s)),
                   "The sustained inactivity detection failed, so there is nothing to draw.")
  s$status$state <- "something_else"
  expect_true(grepl("something_else", .raw.plot.empty.message(plot_raw_sleep(s)), fixed = TRUE))
})

test_that("a part-3 object with no bouts and no SRI gives the right sentences", {
  e <- fixture_sleep("novalid")
  s <- sleep_from_ms3(e)
  # novalid: every epoch invalid, a zero-row sib.cla.sum, seven zero SRI rows
  expect_identical(dim(e$sib.cla.sum), c(0L, 9L))
  expect_true(all(is.na(e$SPTE_start)))
  p <- plot_raw_sleep_nights(s)
  expect_true(.raw.plot.is.empty(p))
  expect_true(grepl("Run raw.sleep.nights()", .raw.plot.empty.message(p), fixed = TRUE))
  p2 <- plot_raw_sleep(s)
  expect_true(.raw.plot.is.empty(p2))
  expect_identical(.raw.plot.empty.message(p2),
                   "Part 3 found no noon-to-noon night windows in this recording.")
  # the SRI is a frame here, so it draws
  expect_true(is.data.frame(e$SleepRegularityIndex))
  expect_identical(e$SleepRegularityIndex$SleepRegularityIndex, rep(0, 7))
  p3 <- plot_raw_sleep_regularity(s)
  expect_false(.raw.plot.is.empty(p3))
  expect_true(grepl("7 day pairs; 0 with more than 16 of 24 hours valid",
                    p3$labels$subtitle, fixed = TRUE))
  expect_gt(render_size(p3, width = 9.5, height = 5), 5000)
})

test_that("an SRI that is the atomic NA gives the empty state, not an error", {
  e <- fixture_sleep("nonights")
  expect_true(is.logical(e$SleepRegularityIndex) && length(e$SleepRegularityIndex) == 1)
  expect_true(is.na(e$SleepRegularityIndex))
  p <- plot_raw_sleep_regularity(sleep_from_ms3(e))
  expect_true(.raw.plot.is.empty(p))
  expect_identical(.raw.plot.empty.message(p),
                   paste0("No Sleep Regularity Index was computed: the index needs at least ",
                          "three calendar days of data."))
  expect_gt(render_size(p, width = 7, height = 4), 2000)
})

test_that("a night table on its own draws its own figure and refuses the other two", {
  r <- plot_run("MOS2")
  expect_false(.raw.plot.is.empty(plot_raw_sleep_nights(r$nights)))
  p <- plot_raw_sleep(r$nights)
  expect_true(.raw.plot.is.empty(p))
  expect_true(grepl("carries no angle series", .raw.plot.empty.message(p), fixed = TRUE))
  p2 <- plot_raw_sleep_regularity(r$nights)
  expect_true(.raw.plot.is.empty(p2))
  expect_true(grepl("carries no Sleep Regularity Index", .raw.plot.empty.message(p2), fixed = TRUE))
})

test_that("plot_raw_sleep_regularity takes the SRI data frame on its own", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep_regularity(r$x$sleep$SleepRegularityIndex)
  expect_false(.raw.plot.is.empty(p))
  expect_identical(ggplot2::layer_data(p, 2)$y, c(17.619, 30.256, 46.738, 0, 0, 0, 0))
  expect_gt(render_size(p, width = 9.5, height = 5), 5000)
})

test_that("asking for a night that is not there is a sentence, and a night subset works", {
  r <- plot_run("MOS2")
  p <- plot_raw_sleep(r$x, night = 99)
  expect_true(.raw.plot.is.empty(p))
  expect_identical(.raw.plot.empty.message(p), "This recording has no night 99.")
  p2 <- plot_raw_sleep(r$x, nights = r$nights, night = 2:3)
  expect_false(.raw.plot.is.empty(p2))
  expect_true(grepl("2 noon-to-noon nights", p2$labels$subtitle, fixed = TRUE))
  ld <- lapply(seq_along(p2$layers), function(i) ggplot2::layer_data(p2, i))
  # nights 2 and 3 carry 38 and 36 bouts
  expect_true(any(vapply(ld, function(d) is.data.frame(d) && nrow(d) == 74L, logical(1))))
})

test_that("the day-sleeper fixture draws, with its three guider ends past 36", {
  e <- fixture_sleep("daysleeper")
  d <- p34_fixture("daysleeper")
  ms4 <- file.path(d, "meta", "ms4.out", "MOSday.gt3x.RData")
  skip_if_no_file(ms4)
  g <- load_env(ms4)
  # three guider wake-ups past 36, the last capped
  expect_equal(g$nightsummary$guider_wakeup,
               c(37.3338888888889, 37.8597222222222, 37.0708333333333, 36), tolerance = 1e-10)
  expect_identical(g$nightsummary$daysleeper, c(1, 1, 1, 0))
  # a night table rebuilt from the stored ms4
  ns <- structure(as.data.frame(g$nightsummary),
                  class = c("canhrActi_raw_nights", "data.frame"))
  attr(ns, "canhrActi") <- list(
    id = "MOSday.gt3x", filename = "MOSday.gt3x", desiredtz = "America/Anchorage",
    episodes = data.frame(night = g$nightsummary$night, def = "T5A5", nb = 1,
                          start = g$nightsummary$sleeponset, end = g$nightsummary$wakeup,
                          overlapGuider = 1, duration = g$nightsummary$SptDuration,
                          stringsAsFactors = FALSE),
    guiders = data.frame(night = g$nightsummary$night, guider = g$nightsummary$guider,
                         guider_onset = g$nightsummary$guider_onset,
                         guider_wakeup = g$nightsummary$guider_wakeup,
                         daysleeper = g$nightsummary$daysleeper == 1,
                         cleaningcode = g$nightsummary$cleaningcode,
                         calendar_date = g$nightsummary$calendar_date,
                         stringsAsFactors = FALSE))
  p <- plot_raw_sleep_nights(ns)
  expect_false(.raw.plot.is.empty(p))
  rects <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))
  gb <- ggplot2::layer_data(p, rects[2])
  # 37.33 - 24 = 13.33, which is less than its onset 29.07, so the window wraps into two bars
  expect_equal(max(gb$xmax), 36, tolerance = 1e-10)
  expect_equal(min(gb$xmin), 12, tolerance = 1e-10)
  # three day sleepers get a vertical mark at 18
  seg <- ggplot2::layer_data(p, which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomSegment"), logical(1)))[1])
  expect_identical(nrow(seg), 3L)
  expect_identical(unique(seg$x), 18)
  expect_gt(render_size(p, width = 9, height = 3.6), 5000)
})

test_that("a guider window past 36 is pulled back by 24 and a wrapped bar becomes two", {
  # GGIR's rule, exercised on a made-up table
  ns <- data.frame(ID = "x", night = 1:2, stringsAsFactors = FALSE)
  attr(ns, "canhrActi") <- list(
    id = "x", filename = "x.gt3x", desiredtz = "UTC",
    episodes = data.frame(night = c(1, 1, 2), def = "T5A5", nb = c(1, 2, 1),
                          start = c(20, 34, 22), end = c(21, 13, 30),
                          overlapGuider = c(0, 1, 1), duration = c(1, 3, 8),
                          stringsAsFactors = FALSE),
    guiders = data.frame(night = c(1, 2), guider = "HDCZA", guider_onset = c(33, 22),
                         guider_wakeup = c(39, 30), daysleeper = c(TRUE, FALSE),
                         cleaningcode = c(0, 0), calendar_date = c("1/1/2025", "2/1/2025"),
                         stringsAsFactors = FALSE))
  class(ns) <- c("canhrActi_raw_nights", "data.frame")
  p <- plot_raw_sleep_nights(ns)
  expect_false(.raw.plot.is.empty(p))
  rects <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))
  bar <- ggplot2::layer_data(p, rects[1])
  gb <- ggplot2::layer_data(p, rects[2])
  # the 34 to 13 bout became two rectangles, 34 to 36 and 12 to 13
  expect_identical(nrow(bar), 4L)
  expect_identical(sort(bar$xmin), c(12, 20, 22, 34))
  expect_identical(sort(bar$xmax), c(13, 21, 30, 36))
  # the night-1 guider 33 to 39 became 33 to 36 plus 12 to 15 (39 - 24 = 15, and 33 > 15)
  expect_true(all(is.na(gb$fill)))
  expect_identical(sort(gb$xmin), c(12, 22, 33))
  expect_identical(sort(gb$xmax), c(15, 30, 36))
  seg <- ggplot2::layer_data(p, which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomSegment"), logical(1)))[1])
  expect_identical(nrow(seg), 1L)
  expect_identical(seg$x, 18)
  expect_gt(render_size(p, width = 9, height = 3.4), 5000)
})

test_that("the daylight saving fixtures draw without error", {
  for (case in c("spring", "autumn", "autumn_edge")) {
    e <- fixture_sleep(case)
    s <- sleep_from_ms3(e)
    expect_false(.raw.plot.is.empty(plot_raw_sleep_regularity(s)))
    # spring loses an hour and autumn gains one; the SRI frame is 7 rows either way
    expect_identical(nrow(e$SleepRegularityIndex), 7L)
  }
  sp <- fixture_sleep("spring")
  expect_identical(round(sp$SleepRegularityIndex$SleepRegularityIndex, 3),
                   c(63.277, 56.667, 63.686, 64.177, 58.074, 42.857, 0))
  au <- fixture_sleep("autumn")
  expect_identical(round(au$SleepRegularityIndex$SleepRegularityIndex, 3),
                   c(63.277, 56.667, 63.686, 64.177, 61.489, 35.054, 0))
})

test_that("as.ggir.milestone keeps its part-1 behaviour and gains parts 2, 3 and 4", {
  r <- plot_run("MOS2")
  x <- r$x
  m1 <- as.ggir.milestone(x)
  expect_identical(names(m1), c("M", "I", "C", "desiredtz_part1", "GGIRversion",
                                "tail_expansion_log"))
  expect_identical(as.ggir.milestone(x, part = 1), m1)
  m2 <- as.ggir.milestone(x, part = 2)
  expect_identical(names(m2), c("SUM", "IMP", "tail_expansion_log", "GGIRversion"))
  expect_identical(m2$IMP, as.ggir.IMP(x$imputed))
  expect_identical(m2$SUM$summary$ID, "MOS2E39230594.gt3x")
  expect_true(is.na(m2$SUM$summary$if_hip_long_axis_id))
  # the full SUM g.part2 saves; a configuration the port does not analyse keeps the two
  # fields part 3 reads
  expect_identical(names(m2$SUM), c("summary", "daysummary", "cosinor_ts"))
  expect_null(attr(m2$SUM, "canhrActi_partial"))
  expect_identical(m2$SUM$summary[["GGIR version"]], "3.3.6")
  q <- x; q$params$qlevels <- 0.5
  mq <- as.ggir.milestone(q, part = 2)
  expect_true(isTRUE(attr(mq$SUM, "canhrActi_partial")))
  expect_identical(names(mq$SUM$summary), c("ID", "if_hip_long_axis_id"))
  expect_identical(mq$SUM$summary$ID, "MOS2E39230594.gt3x")
  m3 <- as.ggir.milestone(x, part = 3)
  expect_identical(names(m3),
                   c("sib.cla.sum", "L5list", "SPTE_end", "SPTE_start", "tib.threshold",
                     "rec_starttime", "ID", "longitudinal_axis", "SleepRegularityIndex",
                     "tail_expansion_log", "GGIRversion", "part3_guider", "desiredtz_part1",
                     "SPTE_corrected"))
  m4 <- as.ggir.milestone(x, part = 4, nights = r$nights)
  expect_identical(names(m4), c("nightsummary", "tail_expansion_log", "GGIRversion"))
  expect_identical(m4, as.ggir.ms4(r$nights))
  expect_identical(m4$nightsummary, as.ggir.nightsummary(r$nights))
  # part 5 asks for timeuse as part 4 asks for nights; 6 is the first part out of range
  expect_error(as.ggir.milestone(x, part = 6), "part must be 1, 2, 3, 4 or 5")
  expect_error(as.ggir.milestone(x, part = 5), "pass timeuse = raw.timeuse")
  expect_error(as.ggir.milestone(x, part = 4), "pass nights = raw.sleep.nights")
  expect_error(as.ggir.milestone(list(a = 1), part = 2), "canhrActi_raw object")
  y <- x; y$imputed <- NULL
  expect_error(as.ggir.milestone(y, part = 2), "raw.impute")
  z <- x; z$sleep <- NULL
  expect_error(as.ggir.milestone(z, part = 3), "raw.sleep.part3")
  expect_error(as.ggir.ms4(r$x), "canhrActi_raw_nights")
})

test_that("the milestone objects are identical() to the stored ones, object by object", {
  for (case in c("MOS2", "EE")) {
    r <- plot_run(case)
    st3 <- load_env(plot_path(case, "ms3"))
    m3 <- as.ggir.milestone(r$x, part = 3)
    for (nm in c("sib.cla.sum", "L5list", "SPTE_end", "SPTE_start", "tib.threshold",
                 "rec_starttime", "ID", "longitudinal_axis", "SleepRegularityIndex",
                 "tail_expansion_log", "part3_guider", "desiredtz_part1", "SPTE_corrected")) {
      expect_identical(m3[[nm]], get(nm, envir = st3), info = paste(case, nm))
    }
    st4 <- load_env(plot_path(case, "ms4"))
    m4 <- as.ggir.milestone(r$x, part = 4, nights = r$nights)
    expect_identical(m4$nightsummary, st4$nightsummary, info = case)
    st2 <- load_env(plot_path(case, "ms2"))
    m2 <- as.ggir.milestone(r$x, part = 2)
    expect_identical(m2$IMP$metashort, st2$IMP$metashort, info = case)
    expect_identical(m2$IMP$r5long, st2$IMP$r5long, info = case)
    expect_identical(m2$IMP$averageday, st2$IMP$averageday, info = case)
    expect_identical(m2$SUM, st2$SUM, info = case)
  }
})

test_that("the part-1 call is unchanged when parts is left at its default", {
  r <- plot_run("MOS2")
  out <- file.path(tempdir(), "canhrActi_p34_one", "meta", "basic")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  f <- write.ggir.milestone(r$x, out)
  expect_identical(basename(f), "meta_MOS2E39230594.gt3x.RData")
  e <- load_env(f)
  expect_identical(ls(e), sort(c("M", "I", "C", "filename_dir", "filefoldername",
                                 "tail_expansion_log", "GGIRversion", "desiredtz_part1")))
  # a path that does not exist is taken as a file name
  f2 <- write.ggir.milestone(r$x, file.path(tempdir(), "canhrActi_p34_file"))
  expect_identical(basename(f2), "canhrActi_p34_file")
  expect_true(file.exists(f2))
  # part 5 needs a timeuse object; 6 is the first part out of range
  expect_error(write.ggir.milestone(r$x, out, parts = 6), "parts must be one or more")
  expect_error(write.ggir.milestone(r$x, out, parts = 5), "timeuse = raw.timeuse")
  expect_error(write.ggir.milestone(r$x, out, parts = numeric(0)), "parts must be one or more")
  expect_error(write.ggir.milestone(r$x, ""), "directory or a file path")
  expect_error(write.ggir.milestone(list(a = 1), out), "canhrActi_raw object")
  expect_error(write.ggir.milestone(as.ggir.milestone(r$x), out, parts = 1:3),
               "must be a canhrActi_raw object to write more than")
})

test_that("write.ggir.milestone writes the four folders GGIR expects, with GGIR's names", {
  r <- plot_run("MOS2")
  root <- file.path(tempdir(), "canhrActi_p34_all")
  unlink(root, recursive = TRUE)
  f <- write.ggir.milestone(r$x, root, parts = 1:4, nights = r$nights)
  expect_identical(names(f), c("part1", "part2", "part3", "part4"))
  expect_identical(sort(list.files(root, recursive = TRUE)),
                   sort(c("meta/basic/meta_MOS2E39230594.gt3x.RData",
                          "meta/ms2.out/MOS2E39230594.gt3x.RData",
                          "meta/ms3.out/MOS2E39230594.gt3x.RData",
                          "meta/ms4.out/MOS2E39230594.gt3x.RData")))
  # checkMilestoneFolders also makes meta/sleep.qc from part 3 onwards
  expect_true(dir.exists(file.path(root, "meta", "sleep.qc")))
  # the object names and their order are GGIR's save order
  nm <- function(p) { e <- new.env(parent = emptyenv()); load(p, envir = e) }
  expect_identical(nm(f[["part1"]]),
                   c("M", "I", "C", "filename_dir", "filefoldername", "tail_expansion_log",
                     "GGIRversion", "desiredtz_part1"))
  expect_identical(nm(f[["part2"]]), c("SUM", "IMP", "tail_expansion_log", "GGIRversion"))
  expect_identical(nm(f[["part3"]]),
                   c("sib.cla.sum", "L5list", "SPTE_end", "SPTE_start", "tib.threshold",
                     "rec_starttime", "ID", "longitudinal_axis", "SleepRegularityIndex",
                     "tail_expansion_log", "GGIRversion", "part3_guider", "desiredtz_part1",
                     "SPTE_corrected"))
  expect_identical(nm(f[["part4"]]), c("nightsummary", "tail_expansion_log", "GGIRversion"))
  st3 <- load_env(plot_path("MOS2", "ms3")); o3 <- load_env(f[["part3"]])
  for (n in ls(st3)) expect_identical(get(n, o3), get(n, st3), info = n)
  st4 <- load_env(plot_path("MOS2", "ms4")); o4 <- load_env(f[["part4"]])
  expect_identical(o4$nightsummary, st4$nightsummary)
  expect_identical(dim(o4$nightsummary), c(4L, 39L))
  root2 <- file.path(tempdir(), "canhrActi_p34_sub")
  unlink(root2, recursive = TRUE)
  f2 <- write.ggir.milestone(r$x, root2, parts = c(3, 1))
  expect_identical(names(f2), c("part1", "part3"))
  expect_identical(sort(list.files(root2, recursive = TRUE)),
                   sort(c("meta/basic/meta_MOS2E39230594.gt3x.RData",
                          "meta/ms3.out/MOS2E39230594.gt3x.RData")))
})

test_that("the part-2-and-later file name rule is GGIR's, with the .RD guard", {
  expect_identical(.raw.milestone.name("MOS2E39230594.gt3x"), "MOS2E39230594.gt3x.RData")
  expect_identical(.raw.milestone.name("a/b/c/MOS2E39230594.gt3x"), "MOS2E39230594.gt3x.RData")
  # a name that already holds ".RD" is left alone, as in GGIR
  expect_identical(.raw.milestone.name("already.RData"), "already.RData")
  expect_identical(.raw.milestone.name("x.RDX"), "x.RDX")
  expect_identical(.raw.milestone.name("plain.bin"), "plain.bin.RData")
})

test_that("GGIR's own g.part4 on canhrActi's milestone reproduces the stored night summary", {
  skip_if_no_ggir()
  for (case in c("MOS2", "EE")) {
    r <- plot_run(case)
    root <- file.path(tempdir(), paste0("canhrActi_s6c_", case))
    unlink(root, recursive = TRUE)
    write.ggir.milestone(r$x, root, parts = 1:3)
    dir.create(file.path(root, "meta", "ms4.out"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(root, "results"), recursive = TRUE, showWarnings = FALSE)
    suppressWarnings(GGIR:::g.part4(datadir = c(), metadatadir = root, f0 = 1, f1 = 1,
                                    verbose = FALSE, do.visual = FALSE, overwrite = TRUE))
    written <- list.files(file.path(root, "meta", "ms4.out"), full.names = TRUE)
    expect_identical(length(written), 1L, info = case)
    g4 <- load_env(written[1])
    st4 <- load_env(plot_path(case, "ms4"))
    expect_identical(g4$nightsummary, st4$nightsummary, info = case)
    expect_identical(g4$nightsummary, as.ggir.nightsummary(r$nights), info = case)
  }
})

test_that("GGIR's own g.part3 on canhrActi's parts 1 and 2 reproduces the stored ms3", {
  skip_if_no_ggir()
  for (case in c("MOS2", "EE")) {
    r <- plot_run(case)
    root <- file.path(tempdir(), paste0("canhrActi_p3rt_", case))
    unlink(root, recursive = TRUE)
    write.ggir.milestone(r$x, root, parts = 1:2)
    dir.create(file.path(root, "meta", "ms3.out"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(root, "meta", "sleep.qc"), recursive = TRUE, showWarnings = FALSE)
    suppressWarnings(GGIR:::g.part3(metadatadir = root, f0 = 1, f1 = 1, verbose = FALSE,
                                    do.parallel = FALSE, overwrite = TRUE))
    written <- list.files(file.path(root, "meta", "ms3.out"), full.names = TRUE)
    expect_identical(length(written), 1L, info = case)
    g3 <- load_env(written[1])
    st3 <- load_env(plot_path(case, "ms3"))
    for (nm in ls(st3)) expect_identical(get(nm, g3), get(nm, st3), info = paste(case, nm))
  }
})

test_that("GGIR's own g.report.part4 on canhrActi's ms4 reproduces the stored person summary", {
  skip_if_no_ggir()
  if (!requireNamespace("data.table", quietly = TRUE)) {
    testthat::skip("data.table is not installed; the csv writer cannot run")
  }
  r <- plot_run("MOS2")
  root <- file.path(tempdir(), "canhrActi_rep4")
  unlink(root, recursive = TRUE)
  write.ggir.milestone(r$x, root, parts = c(1, 3, 4), nights = r$nights)
  dir.create(file.path(root, "meta", "ms2.out"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  P <- GGIR::load_params()
  suppressWarnings(GGIR:::g.report.part4(datadir = c(), metadatadir = root, f0 = 1, f1 = 1,
                                         params_sleep = P$params_sleep,
                                         params_output = P$params_output, verbose = FALSE))
  ref <- ref_file("out", "output_din")
  # the cleaned person summary has no part-5 columns, so it is byte-identical
  expect_identical(readLines(file.path(root, "results/part4_summary_sleep_cleaned.csv")),
                   readLines(file.path(ref, "results/part4_summary_sleep_cleaned.csv")))
  # the others differ only by the part-5 merge columns; the reference run had an ms5
  a <- utils::read.csv(file.path(root, "results/part4_nightsummary_sleep_cleaned.csv"))
  b <- utils::read.csv(file.path(ref, "results/part4_nightsummary_sleep_cleaned.csv"))
  expect_identical(dim(a), c(2L, 36L))
  expect_identical(dim(b), c(2L, 39L))
  expect_identical(setdiff(names(b), names(a)), c("window", "nonwear_perc_spt", "ACC_spt_mg"))
  expect_identical(a[names(a)], b[names(a)])
  a2 <- utils::read.csv(file.path(root, "results/QC/part4_summary_sleep_full.csv"))
  b2 <- utils::read.csv(file.path(ref, "results/QC/part4_summary_sleep_full.csv"))
  expect_identical(dim(a2), c(1L, 113L))
  expect_identical(dim(b2), c(1L, 119L))
  expect_identical(a2[names(a2)], b2[names(a2)])
})
