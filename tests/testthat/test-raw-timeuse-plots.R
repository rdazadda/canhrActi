# Tests for R/raw_timeuse_plots.R (the four part-5 figures and their helpers). GGIR's
# part-5 figure is a base-graphics page with no number to compare against, so every drawn
# coordinate is checked against the parity-checked data: the stored ms5.outraw class ids,
# the day table's dur_*_min, Nblocks_* and ig_day_* columns. Reference data live in the
# folder named by CANHRACTI_GGIR_REF; tests skip when it is unset. No test needs GGIR.

withr::local_locale(c(LC_TIME = "C"))
# GGIR wrote the reference outputs in an America/Anchorage session
withr::local_timezone("America/Anchorage")

.tup_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.tup_ref == "" || !dir.exists(.tup_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
tup_file <- function(...) file.path(sub("/+$", "", .tup_ref), ...)
tup_loadenv <- function(path) {
  e <- new.env()
  load(path, envir = e)
  e
}

TUP_CASES <- list(
  MOS2 = list(out = c("out", "output_din"), days = 8L, tz = "America/Anchorage"),
  EE   = list(out = c("timing_out", "output_timing"), days = 8L, tz = "Europe/Helsinki"))

tup_cache <- new.env(parent = emptyenv())

# the canhrActi_raw and the night table part 5 reads, from the stored milestones
tup_build <- function(metadir) {
  fn <- dir(file.path(metadir, "ms3.out"))
  if (length(fn) == 0) testthat::skip(paste0("no ms3 milestone under ", metadir))
  fn <- fn[1]
  x <- read.ggir.milestone(file.path(metadir, "basic", paste0("meta_", fn)))
  ms2 <- tup_loadenv(file.path(metadir, "ms2.out", fn))
  ms3 <- tup_loadenv(file.path(metadir, "ms3.out", fn))
  ms4 <- tup_loadenv(file.path(metadir, "ms4.out", fn))
  x$imputed <- ms2$IMP
  x$sleep <- list(sib.cla.sum = ms3$sib.cla.sum, SPTE_start = ms3$SPTE_start,
                  SPTE_end = ms3$SPTE_end, longitudinal_axis = ms3$longitudinal_axis,
                  tail_expansion_log = ms3$tail_expansion_log,
                  rec_starttime = ms3$rec_starttime, id = ms3$ID)
  list(x = x, nights = ms4$nightsummary, fn = fn, metadir = metadir)
}

tup_run <- function(key, ...) {
  cachekey <- paste0(key, "|", paste(names(list(...)), unlist(list(...)), collapse = ","))
  if (!is.null(tup_cache[[cachekey]])) return(tup_cache[[cachekey]])
  skip_if_no_ggir_ref()
  cs <- TUP_CASES[[key]]
  metadir <- tup_file(cs$out[1], cs$out[2], "meta")
  testthat::skip_if_not(dir.exists(metadir), paste0("no reference run under ", metadir))
  S <- tup_build(metadir)
  tu <- raw.timeuse(S$x, S$nights, params = raw.params(ggir_version_label = "3.3.6", ...))
  stored_ms5 <- if (file.exists(file.path(metadir, "ms5.out", S$fn))) {
    tup_loadenv(file.path(metadir, "ms5.out", S$fn))$output
  } else NULL
  d <- file.path(metadir, "ms5.outraw", "40_100_400")
  stored_mdat <- if (dir.exists(d) && length(dir(d)) > 0) {
    tup_loadenv(dir(d, full.names = TRUE)[1])$mdat
  } else NULL
  out <- list(S = S, tu = tu, output = stored_ms5, mdat = stored_mdat, metadir = metadir)
  tup_cache[[cachekey]] <- out
  out
}

tup_layer <- function(p, i) ggplot2::layer_data(p, i)

tup_png <- function(p, name, width = 12, height = 7) {
  f <- file.path(tempdir(), paste0(name, ".png"))
  if (file.exists(f)) unlink(f)
  ggplot2::ggsave(f, p, width = width, height = height, dpi = 110, bg = "white")
  file.size(f)
}

num <- function(v) suppressWarnings(as.numeric(as.character(v)))

LN18 <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
          "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt",
          "day_MVPA_bts_10", "day_MVPA_bts_5_10", "day_MVPA_bts_1_5",
          "day_IN_bts_30", "day_IN_bts_20_30", "day_IN_bts_10_20",
          "day_LIG_bts_10", "day_LIG_bts_5_10", "day_LIG_bts_1_5")

test_that("every default class name is parsed into a family, an intensity and a rank", {
  p <- .raw.timeuse.plot.classparts(LN18)
  expect_identical(nrow(p), 18L)
  expect_identical(p$name, LN18)
  expect_identical(p$family,
                   c("sleep", rep("wake", 4), "IN", "LIG", "MVPA", "MVPA", rep("MVPA", 3),
                     rep("IN", 3), rep("LIG", 3)))
  # the wake classes sort by intensity, the day classes by the lower edge of their band
  expect_identical(p$rank[2:5], c(1, 2, 3, 4))
  expect_identical(p$rank[p$name == "day_IN_unbt"], -1 + 1 / 100)
  expect_identical(p$rank[p$name == "day_LIG_unbt"], -1 + 2 / 100)
  expect_identical(p$rank[p$name == "day_MOD_unbt"], -1 + 3 / 100)
  expect_identical(p$rank[p$name == "day_VIG_unbt"], -1 + 4 / 100)
  expect_identical(p$rank[p$name == "day_IN_bts_30"], 30)
  expect_identical(p$rank[p$name == "day_IN_bts_20_30"], 20)
  expect_identical(p$rank[p$name == "day_IN_bts_10_20"], 10)
  expect_identical(p$rank[p$name == "day_MVPA_bts_1_5"], 1)
  expect_identical(p$bout_min[p$name == "day_MVPA_bts_5_10"], 5)
  expect_true(all(is.na(p$bout_min[!grepl("_bts_", p$name)])))
  # the nap class and a name from a different bout-duration set
  q <- .raw.timeuse.plot.classparts(c("day_nap", "day_IN_bts_45_60", "nonsense"))
  expect_identical(q$family, c("nap", "IN", "other"))
  expect_identical(q$bout_min, c(NA, 45, NA))
})

test_that("each class gets its own colour and the longer bout is the darker one", {
  cols <- .raw.timeuse.plot.classcolours(LN18)
  expect_identical(names(cols), LN18)
  expect_identical(length(unique(unname(cols))), 18L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  lum <- function(h) sum(grDevices::col2rgb(h)[, 1] * c(0.299, 0.587, 0.114))
  for (fam in list(c("day_IN_unbt", "day_IN_bts_10_20", "day_IN_bts_20_30", "day_IN_bts_30"),
                   c("day_LIG_unbt", "day_LIG_bts_1_5", "day_LIG_bts_5_10", "day_LIG_bts_10"),
                   c("day_MOD_unbt", "day_VIG_unbt", "day_MVPA_bts_1_5",
                     "day_MVPA_bts_5_10", "day_MVPA_bts_10"),
                   c("spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG"))) {
    l <- vapply(cols[fam], lum, numeric(1))
    expect_true(all(diff(l) < 0),
                info = paste0("not monotone: ", paste(round(l), collapse = " ")))
  }
  # the sleep period is the darkest thing on the page
  expect_lt(lum(cols[["spt_sleep"]]), min(vapply(cols[-1], lum, numeric(1))))
  other <- .raw.timeuse.plot.classcolours(c("spt_sleep", "day_IN_unbt", "day_IN_bts_60",
                                            "day_IN_bts_15_60", "day_nap"))
  expect_identical(length(unique(unname(other))), 5L)
})

test_that("a run of equal values never crosses a day boundary and ends one epoch late", {
  day <- c(rep("2025-10-07", 4), rep("2025-10-08", 3))
  hour <- c(23.99, 23.9925, 23.995, 23.9975, 0, 0.0025, 0.005)
  v <- c(1, 1, 1, 1, 1, 2, 2)
  r <- .raw.timeuse.plot.runs(v, day, hour, ws3 = 9)
  expect_identical(nrow(r), 3L)
  expect_identical(r$day, c("2025-10-07", "2025-10-08", "2025-10-08"))
  expect_identical(r$value, c("1", "1", "2"))
  expect_identical(r$n_epochs, c(4L, 1L, 2L))
  expect_identical(r$xmin, c(23.99, 0, 0.0025))
  expect_identical(r$xmax, c(23.9975 + 9 / 3600, 0 + 9 / 3600, 0.005 + 9 / 3600))
  expect_identical(r$first_epoch, c(1L, 5L, 6L))
  # keep = 1 gives the marker runs only
  expect_identical(.raw.timeuse.plot.runs(c(0, 1, 1, 0, 1), rep("d", 5), 0:4 / 10, 5,
                                          keep = 1)$n_epochs, c(2L, 1L))
  expect_identical(nrow(.raw.timeuse.plot.runs(numeric(0), character(0), numeric(0), 5)), 0L)
})

test_that("the four figures return the empty state rather than erroring", {
  fake <- structure(list(daysummary = data.frame(), series = NULL, levels = LN18,
                         legend = NULL, settings = list(desiredtz = "UTC", windowsizes = 5),
                         status = list(state = "skipped", messages = "corrupt")),
                    class = "canhrActi_raw_timeuse")
  for (f in list(plot_raw_timeuse, plot_raw_timeuse_day, plot_raw_timeuse_bouts,
                 plot_raw_timeuse_gradient)) {
    p <- f(fake)
    expect_s3_class(p, "ggplot")
    expect_true(.raw.plot.is.empty(p))
    expect_match(.raw.plot.empty.message(p), "corrupt, too short or was skipped")
  }
  for (st in c("no_nights", "no_valid_night", "no_sib_definitions", "no_windows")) {
    fake$status$state <- st
    expect_true(.raw.plot.is.empty(plot_raw_timeuse(fake)))
  }
  # an "ok" object with no series: the three series figures say so, the day figure draws
  fake$status$state <- "ok"
  expect_match(.raw.plot.empty.message(plot_raw_timeuse(fake)), "save_ms5rawlevels")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse_bouts(fake)), "save_ms5rawlevels")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse_gradient(fake)), "save_ms5rawlevels")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse_day(fake)), "no rows")
  fake$series <- list("40_100_400" = list(T5A5 = list(MM = data.frame(timenum = 1))))
  expect_match(.raw.plot.empty.message(plot_raw_timeuse(fake, threshold = "18_60_400")),
               "no threshold set called")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse(fake, sibdef = "T10A5")),
               "no sleep definition called")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse(fake, timewindow = "WW")),
               "no time window called")
})

test_that("something that is not a part-5 object raises", {
  expect_error(plot_raw_timeuse(list(a = 1)), "canhrActi_raw_timeuse")
  expect_error(plot_raw_timeuse_day("no"), "canhrActi_raw_timeuse")
  expect_error(plot_raw_timeuse_bouts(42), "canhrActi_raw_timeuse")
  expect_error(plot_raw_timeuse_gradient(NULL), "canhrActi_raw_timeuse")
})

test_that("the class bands are the stored mdat's class_id, rectangle by rectangle", {
  R <- tup_run("MOS2")
  skip_if(is.null(R$mdat), "no stored ms5.outraw series")
  m <- R$mdat
  Lnames <- R$tu$levels
  mine <- R$tu$series[["40_100_400"]][["T5A5"]][["WW"]]
  expect_identical(mine$class_id, m$class_id)
  expect_identical(mine$SleepPeriodTime, m$SleepPeriodTime)
  expect_identical(mine$invalidepoch, m$invalidepoch)
  expect_identical(mine$window, m$window)

  # the rectangles rebuilt from the stored series
  day <- format(m$timestamp, "%Y-%m-%d")
  hour <- as.numeric(format(m$timestamp, "%H")) + as.numeric(format(m$timestamp, "%M")) / 60 +
    as.numeric(format(m$timestamp, "%S")) / 3600
  cls <- Lnames[m$class_id + 1L]
  rl <- rle(paste0(day, "\r", cls))
  ends <- cumsum(rl$lengths)
  starts <- ends - rl$lengths + 1L
  udays <- unique(day)
  exp_bands <- data.frame(xmin = hour[starts], xmax = hour[ends] + 5 / 3600,
                          row = match(day[starts], udays), class = cls[starts],
                          stringsAsFactors = FALSE)

  p <- plot_raw_timeuse(R$tu, timewindow = "WW")
  expect_s3_class(p, "ggplot")
  ld <- tup_layer(p, 2)   # layer 1 is the sleep-period strip, layer 2 the class band
  expect_identical(nrow(ld), nrow(exp_bands))
  expect_identical(ld$xmin, exp_bands$xmin)
  expect_identical(ld$xmax, exp_bands$xmax)
  # the y scale is reversed, so the first day is the top row and layer_data negates
  expect_identical(ld$ymin, -(exp_bands$row - 0.30))
  expect_identical(ld$ymax, -(exp_bands$row + 0.30))
  cols <- .raw.timeuse.plot.classcolours(Lnames)
  expect_identical(ld$fill, unname(cols[exp_bands$class]))
  # 7540 runs of one class, 7547 once the seven midnights split them
  expect_identical(length(rle(m$class_id)$lengths), 7540L)
  expect_identical(nrow(ld), 7547L)
  epochs <- function(l) as.integer(sum(round((l$xmax - l$xmin) * 3600 / 5)))
  expect_identical(epochs(ld), nrow(m))
  expect_identical(length(unique(ld$ymin)), 8L)

  # the three markers, each from its own column of the stored series
  expect_identical(epochs(tup_layer(p, 1)), sum(m$SleepPeriodTime == 1))
  expect_identical(epochs(tup_layer(p, 4)), sum(m$invalidepoch == 1))
  expect_identical(epochs(tup_layer(p, 5)), sum(m$window > 0))
  expect_identical(sum(m$window > 0), 46130L)
  expect_identical(sum(m$SleepPeriodTime == 1), 17903L)
  expect_identical(sum(m$invalidepoch == 1), 65880L)

  # the legend: eighteen classes and three markers
  keys <- tup_layer(p, 6)
  expect_identical(nrow(keys), 21L)
  expect_false(any(is.na(keys$fill)))
  expect_identical(keys$fill[1:18], unname(cols))

  # a day subset draws only that day
  p1 <- plot_raw_timeuse(R$tu, days = "2025-10-08")
  expect_identical(length(unique(tup_layer(p1, 2)$ymin)), 1L)
  expect_identical(epochs(tup_layer(p1, 2)), sum(day == "2025-10-08"))
  expect_match(.raw.plot.empty.message(plot_raw_timeuse(R$tu, days = "2001-01-01")),
               "No epoch of this recording falls on the day")
  # show_invalid = FALSE drops the wash and the non-wear strip
  expect_identical(length(plot_raw_timeuse(R$tu, show_invalid = FALSE)$layers),
                   length(p$layers) - 2L)
})

test_that("the stacked minutes are the stored day table's dur_*_min columns", {
  R <- tup_run("MOS2")
  skip_if(is.null(R$output), "no stored ms5.out day table")
  ds <- R$output
  Lnames <- R$tu$levels
  cols <- paste0("dur_", Lnames, "_min")
  expect_true(all(cols %in% names(ds)))
  expect_identical(R$tu$daysummary, ds)   # the table drawn is the stored one

  p <- plot_raw_timeuse_day(R$tu)
  ld <- tup_layer(p, 1)
  expect_identical(nrow(ld), nrow(ds) * length(Lnames))
  # a stacked bar reports the cumulative top, so the segment height is ymax - ymin
  got2 <- matrix(NA_real_, nrow = nrow(ds), ncol = length(Lnames),
                 dimnames = list(NULL, Lnames))
  fills <- .raw.timeuse.plot.classcolours(Lnames)
  for (k in seq_along(Lnames)) {
    rows <- which(ld$fill == fills[[k]])
    expect_identical(length(rows), nrow(ds))
    o <- order(ld$PANEL[rows], ld$x[rows])
    got2[, k] <- (ld$ymax[rows] - ld$ymin[rows])[o]
  }
  want <- matrix(NA_real_, nrow = nrow(ds), ncol = length(Lnames),
                 dimnames = list(NULL, Lnames))
  ord <- order(match(as.character(ds$window), unique(as.character(ds$window))),
               seq_len(nrow(ds)))
  for (k in seq_along(Lnames)) want[, k] <- num(ds[[cols[k]]])[ord]
  expect_equal(got2, want, tolerance = 1e-9)
  # the stack is dur_day_spt_min to within floating point
  expect_equal(rowSums(want), num(ds$dur_day_spt_min)[ord], tolerance = 1e-9)
  expect_identical(round(rowSums(want)[1:4]), c(210, 1440, 1440, 1440))
  # the four dur_day_total_* columns are not in the stack, and they do not nest
  drawn <- unique(ld$fill)
  expect_false(any(drawn %in% c("#000000")))
  expect_identical(length(drawn), length(Lnames))
  # dur_day_total_IN_min counts an epoch by its own intensity, not by the bout that claimed it
  in_classes <- rowSums(want[, grep("^day_IN_", Lnames), drop = FALSE])
  expect_equal(round(in_classes[5], 3), 826.5)
  expect_equal(round(num(ds$dur_day_total_IN_min)[ord][5], 4), 797.6667)
  # the dash is dur_day_spt_min
  expect_equal(tup_layer(p, 2)$y, num(ds$dur_day_spt_min)[ord], tolerance = 1e-9)
  pm <- plot_raw_timeuse_day(R$tu, timewindow = "MM")
  expect_identical(nrow(tup_layer(pm, 1)), 4L * length(Lnames))
  expect_match(.raw.plot.empty.message(plot_raw_timeuse_day(R$tu, timewindow = "OO")),
               "no time window called")
})

test_that("the blocks drawn are the day table's Nblocks, class by class and window by window", {
  R <- tup_run("MOS2")
  skip_if(is.null(R$output), "no stored ms5.out day table")
  ds <- R$output
  Lnames <- R$tu$levels
  for (tw in c("MM", "WW")) {
    m <- R$tu$series[["40_100_400"]][["T5A5"]][[tw]]
    rows <- which(as.character(ds$window) == tw)
    p <- plot_raw_timeuse_bouts(R$tu, timewindow = tw)
    expect_s3_class(p, "ggplot")
    # layer 2 is the jittered blocks: y around the class position, x as log10 of the length
    pts <- tup_layer(p, 2)
    present <- Lnames[Lnames %in% Lnames[unique(round(pts$y))]]
    want <- vapply(Lnames, function(L) sum(num(ds[[paste0("Nblocks_", L)]])[rows]),
                   numeric(1))
    present <- rev(Lnames[want > 0])     # the y axis runs from the last class upwards
    got <- table(factor(round(pts$y), levels = seq_along(present)))
    expect_identical(length(got), length(present))
    expect_identical(as.integer(unname(got)), as.integer(unname(want[want > 0][
      rev(seq_len(sum(want > 0)))])))
    expect_identical(sum(as.integer(got)), as.integer(sum(want)))
    # per window: one rle per window
    for (i in seq_along(rows)) {
      w <- num(ds$window_number)[rows[i]]
      rl <- rle(as.integer(m$class_id[m$window == w]))
      for (k in seq_along(Lnames)) {
        expect_identical(sum(rl$values == (k - 1L)),
                         as.integer(num(ds[[paste0("Nblocks_", Lnames[k])]])[rows[i]]),
                         info = paste(tw, w, Lnames[k]))
      }
    }
    # every block length is a whole number of epochs and they tile the analysed windows
    mins <- 10^pts$x
    expect_true(all(abs(mins * 60 / 5 - round(mins * 60 / 5)) < 1e-6))
    expect_identical(as.integer(round(sum(mins) * 60 / 5)), sum(m$window > 0))
  }
  # the reference numbers of the MM pass
  p <- plot_raw_timeuse_bouts(R$tu, timewindow = "MM")
  pts <- tup_layer(p, 2)
  expect_identical(nrow(pts), 3707L)
  expect_equal(max(10^pts$x), 253.916666666667, tolerance = 1e-9)
  expect_identical(length(unique(round(pts$y))), 14L)   # 14 of the 18 classes occur
  p2 <- plot_raw_timeuse_bouts(R$tu, classes = "day_IN_bts_30")
  expect_identical(nrow(tup_layer(p2, 2)),
                   as.integer(sum(num(ds$Nblocks_day_IN_bts_30)[
                     as.character(ds$window) == "MM"])))
})

test_that("the fitted line is the day table's intensity gradient", {
  R <- tup_run("MOS2", iglevels = 1)
  ds <- R$tu$daysummary
  expect_true(all(c("ig_day_gradient", "ig_day_intercept", "ig_day_rsquared") %in% names(ds)))
  ww <- which(as.character(ds$window) == "WW")
  # the stored MOS2 WW values
  expect_identical(num(ds$ig_day_gradient)[ww],
                   c(-2.06856538422637, -2.20212177453805, -1.96465568611227))
  expect_identical(num(ds$ig_day_intercept)[ww],
                   c(11.9295377243597, 12.3838234505366, 11.759443017453))
  expect_identical(num(ds$ig_day_rsquared)[ww],
                   c(0.91322148068728, 0.928570189196511, 0.914163760725516))

  p <- plot_raw_timeuse_gradient(R$tu, timewindow = "WW")
  expect_s3_class(p, "ggplot")
  pts <- tup_layer(p, 1)
  lines <- tup_layer(p, 2)
  for (i in seq_along(ww)) {
    l <- lines[lines$group == i, ]
    expect_identical(nrow(l), 64L)
    slope <- diff(range(l$y)) / diff(range(l$x)) * sign(diff(l$y[1:2]))
    expect_equal(slope, num(ds$ig_day_gradient)[ww][i], tolerance = 1e-8)
    expect_equal(l$y[1] - slope * l$x[1], num(ds$ig_day_intercept)[ww][i] / log(10),
                 tolerance = 1e-8)
  }
  # the points are the non-empty bins, minutes per bin
  m <- R$tu$series[["40_100_400"]][["T5A5"]][["WW"]]
  iglev <- c(seq(0, 4000, by = 25), 8000)
  for (i in seq_along(ww)) {
    w <- num(ds$window_number)[ww][i]
    j <- which(m$window == w & m$SleepPeriodTime == 0)
    y <- (as.numeric(table(cut(m$ACC[j], breaks = iglev, right = FALSE))) * 5) / 60
    got <- pts$y[pts$group == i]
    expect_equal(sort(10^got), sort(y[y > 0]), tolerance = 1e-9)
    # the fit recomputed from the series is the stored column
    ig <- .raw.intensity.gradient(zoo::rollmean(iglev, 2), y)
    # the day table holds a 15-significant-digit string, so the comparison is on that string
    expect_identical(as.character(signif(unname(ig$gradient), 15)),
                     as.character(ds$ig_day_gradient[ww][i]))
    expect_identical(as.character(signif(unname(ig$y_intercept), 15)),
                     as.character(ds$ig_day_intercept[ww][i]))
  }
  # without iglevels the figure still draws, from the default expansion
  R0 <- tup_run("MOS2")
  p0 <- plot_raw_timeuse_gradient(R0$tu)
  expect_false(.raw.plot.is.empty(p0))
  expect_match(p0$labels$caption, "iglevels was not set")
  expect_identical(length(unique(tup_layer(p0, 2)$group)), 4L)
  # period = "day_spt" is the other triple
  pspt <- plot_raw_timeuse_gradient(R$tu, period = "day_spt", timewindow = "WW")
  for (i in seq_along(ww)) {
    l <- tup_layer(pspt, 2)
    l <- l[l$group == i, ]
    slope <- diff(range(l$y)) / diff(range(l$x)) * sign(diff(l$y[1:2]))
    expect_equal(slope, num(ds$ig_day_spt_gradient)[ww][i], tolerance = 1e-8)
  }
})

test_that("every figure renders to a PNG well above 5 kB on both reference recordings", {
  for (key in names(TUP_CASES)) {
    R <- tup_run(key)
    sizes <- c(
      timeuse = tup_png(plot_raw_timeuse(R$tu), paste0("tu_", key), 13, 7),
      day = tup_png(plot_raw_timeuse_day(R$tu), paste0("tuday_", key), 11, 8),
      bouts = tup_png(plot_raw_timeuse_bouts(R$tu), paste0("tubouts_", key), 11, 7),
      gradient = tup_png(plot_raw_timeuse_gradient(R$tu), paste0("tugrad_", key), 10, 6.5))
    expect_true(all(sizes > 5000),
                info = paste0(key, ": ", paste(names(sizes), sizes, collapse = ", ")))
    expect_true(all(sizes > 40000), info = paste(key, paste(sizes, collapse = ", ")))
  }
  # the empty state renders too
  fake <- structure(list(daysummary = data.frame(), levels = LN18,
                         settings = list(desiredtz = "UTC"),
                         status = list(state = "skipped")), class = "canhrActi_raw_timeuse")
  expect_gt(tup_png(plot_raw_timeuse(fake), "tu_empty", 8, 4), 5000)
})

test_that("the second reference recording draws its own eleven windows and five days", {
  R <- tup_run("EE")
  skip_if(is.null(R$output), "no stored EE day table")
  ds <- R$output
  expect_identical(nrow(ds), 11L)
  p <- plot_raw_timeuse_day(R$tu)
  ld <- tup_layer(p, 1)
  expect_identical(nrow(ld), 11L * length(R$tu$levels))
  ord <- order(match(as.character(ds$window), unique(as.character(ds$window))),
               seq_len(nrow(ds)))
  # the EE window lengths
  expect_identical(num(ds$dur_day_spt_min)[ord][1:6], c(885, 1440, 1440, 1440, 1440, 1440))
  expect_equal(num(ds$dur_day_spt_min)[ord][7:11],
               c(1612.41666666667, 1407.5, 1326.08333333333, 1569.41666666667, 1291),
               tolerance = 1e-9)
  tot <- tapply(ld$ymax - ld$ymin, paste(ld$PANEL, ld$x), sum)
  expect_equal(sort(as.numeric(tot)), sort(num(ds$dur_day_spt_min)), tolerance = 1e-8)
  # the classified series, in Europe/Helsinki
  pt <- plot_raw_timeuse(R$tu)
  expect_match(pt$labels$caption, "Europe/Helsinki")
  expect_identical(as.integer(sum(round((tup_layer(pt, 2)$xmax -
                                           tup_layer(pt, 2)$xmin) * 3600 / 5))),
                   nrow(R$tu$series[["40_100_400"]][["T5A5"]][[1]]))
})

test_that("a corrupt recording gives the empty state from raw.timeuse itself", {
  skip_if_no_ggir_ref()
  metadir <- tup_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tup_build(metadir)
  xc <- S$x
  xc$meta$filecorrupt <- TRUE
  tu <- raw.timeuse(xc, S$nights, params = raw.params(ggir_version_label = "3.3.6"))
  expect_identical(tu$status$state, "skipped")
  expect_identical(nrow(tu$daysummary), 0L)
  for (f in list(plot_raw_timeuse, plot_raw_timeuse_day, plot_raw_timeuse_bouts,
                 plot_raw_timeuse_gradient)) {
    p <- f(tu)
    expect_s3_class(p, "ggplot")
    expect_true(.raw.plot.is.empty(p))
    expect_match(.raw.plot.empty.message(p), "corrupt, too short or was skipped")
    expect_gt(tup_png(p, "corrupt", 8, 4), 5000)
  }
  # a recording with no analysable window: state "no_windows"
  xt <- S$x
  keep <- 2421:2700
  xt$imputed$metashort <- xt$imputed$metashort[keep, ]
  xt$imputed$rout <- xt$imputed$rout[1:2, ]
  xt$meta$metashort <- xt$meta$metashort[keep, ]
  xt$meta$metalong <- xt$meta$metalong[1:2, ]
  tu2 <- raw.timeuse(xt, S$nights, params = raw.params(ggir_version_label = "3.3.6"))
  expect_identical(tu2$status$state, "no_windows")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse(tu2)), "900 second minimum")
  expect_match(.raw.plot.empty.message(plot_raw_timeuse_day(tu2)), "900 second minimum")
})
