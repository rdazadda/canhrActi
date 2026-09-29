# Tests for R/raw_plots.R (the seven part-1 and part-2 figures and their helpers).
# Reference data live in the folder named by CANHRACTI_GGIR_REF; the MOS2 recording is
# read end to end once and cached (about 65 s), and tests skip when a file is missing.
# The numbers the plots are checked against are recomputed here with GGIR's own
# expressions, so live GGIR is not needed.

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
TRUNC <- function() ref_file("failmodes", "din", "truncated.gt3x")
SHORT <- function() ref_file("failmodes", "din", "tooshort.gt3x")
MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to

.cache <- new.env()
mos2 <- function() {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2())
  if (is.null(.cache$x)) {
    t0 <- Sys.time()
    .cache$x <- read.raw.accelerometer(MOS2(), desiredtz = MOS2_TZ)
    cat(sprintf("\n[timing] read.raw.accelerometer(MOS2): %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  .cache$x
}
trunc_obj <- function() {
  skip_if_no_ggir_ref(); skip_if_no_file(TRUNC())
  if (is.null(.cache$trunc)) .cache$trunc <- read.raw.accelerometer(TRUNC(), desiredtz = MOS2_TZ)
  .cache$trunc
}
short_obj <- function() {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  if (is.null(.cache$short)) .cache$short <- read.raw.accelerometer(SHORT(), desiredtz = MOS2_TZ)
  .cache$short
}
stored_M <- function() {
  skip_if_no_ggir_ref(); skip_if_no_file(MOS2_RDATA())
  if (is.null(.cache$M)) { e <- new.env(); load(MOS2_RDATA(), envir = e); .cache$M <- e$M }
  .cache$M
}

# render to a PNG in a temporary file and return its size in bytes
png_size <- function(g, width = 9, height = 6) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  ggplot2::ggsave(f, g, width = width, height = height, dpi = 100, bg = "white")
  file.size(f)
}
# the plot's own data or the first layer's data that carries a given column
layer_with <- function(g, column) {
  if (is.data.frame(g$data) && column %in% names(g$data)) return(g$data)
  hits <- Filter(function(L) is.data.frame(L$data) && column %in% names(L$data), g$layers)
  if (length(hits) == 0) NULL else hits[[1]]$data
}
# runs of ones as inclusive epoch ranges
expected_runs <- function(r) {
  rl <- rle(as.integer(r == 1)); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1L
  data.frame(start_epoch = starts[rl$values == 1L], end_epoch = ends[rl$values == 1L])
}

PLOTS <- list(quality = function(x) plot_raw_quality(x), days = function(x) plot_raw_days(x),
              nights = function(x) plot_raw_nights(x), calibration = function(x) plot_raw_calibration(x),
              gaps = function(x) plot_raw_gaps(x), chunks = function(x) plot_raw_chunks(x),
              wear_days = function(x) plot_raw_wear_days(x))

test_that("each of the seven plots on MOS2 is a ggplot that renders to a PNG above 5 kB", {
  x <- mos2()
  sizes <- c()
  for (nm in names(PLOTS)) {
    g <- PLOTS[[nm]](x)
    expect_true(inherits(g, "ggplot"), info = nm)
    expect_false(.raw.plot.is.empty(g), info = nm)
    expect_true(length(g$layers) >= 3, info = nm)
    s <- png_size(g)
    expect_gt(s, 5 * 1024, label = paste0(nm, " png bytes"))
    sizes[nm] <- s
  }
  cat("\n[render] png bytes:", paste(names(sizes), round(sizes / 1024), "kB", collapse = "; "), "\n")
})

test_that("each plot returns the empty state (one sentence) for a corrupt recording", {
  z <- trunc_obj()
  expect_true(isTRUE(z$status$corrupt))
  for (nm in names(PLOTS)) {
    g <- PLOTS[[nm]](z)
    expect_true(inherits(g, "ggplot"), info = nm)
    expect_true(.raw.plot.is.empty(g), info = nm)
    msg <- .raw.plot.empty.message(g)
    expect_true(is.character(msg) && nzchar(msg), info = nm)
    expect_true(grepl("\\.$", msg), info = paste(nm, "ends with a full stop"))
    expect_true(grepl("corrupt", msg, fixed = TRUE), info = paste(nm, "names the corrupt state"))
    expect_gt(png_size(g, 6, 4), 5 * 1024)
  }
})

test_that("each plot returns the empty state for a too-short recording and a skipped one", {
  s <- short_obj()
  expect_true(isTRUE(s$status$too_short))
  for (nm in names(PLOTS)) {
    g <- PLOTS[[nm]](s)
    expect_true(.raw.plot.is.empty(g), info = nm)
    expect_true(grepl("two hours", .raw.plot.empty.message(g), fixed = TRUE), info = nm)
  }
  skip_if_no_file(SHORT())
  sk <- read.raw.accelerometer(SHORT(), desiredtz = MOS2_TZ, skip_small_files = TRUE)
  expect_true(isTRUE(sk$status$skipped))
  for (nm in names(PLOTS)) {
    g <- PLOTS[[nm]](sk)
    expect_true(.raw.plot.is.empty(g), info = nm)
    expect_true(grepl("skipped", .raw.plot.empty.message(g), fixed = TRUE), info = nm)
  }
})

test_that("the empty-state helpers and input validation behave", {
  e <- .raw.plot.empty("Nothing here.", "T")
  expect_true(inherits(e, "ggplot"))
  expect_true(.raw.plot.is.empty(e))
  expect_identical(.raw.plot.empty.message(e), "Nothing here.")
  expect_false(.raw.plot.is.empty(ggplot2::ggplot()))
  expect_null(.raw.plot.empty.message(ggplot2::ggplot()))
  expect_error(plot_raw_quality(42), "canhrActi_raw")
  expect_error(plot_raw_days("a"), "canhrActi_raw")
  g <- plot_raw_quality(list(meta = NULL))
  expect_true(.raw.plot.is.empty(g))
  expect_match(.raw.plot.empty.message(g), "No epoch tables")
  # a calibration object alone: chunks and sphere draw, the table plots are empty
  x <- mos2()
  expect_false(.raw.plot.is.empty(plot_raw_calibration(x$calibration)))
  expect_false(.raw.plot.is.empty(plot_raw_chunks(x$calibration)))
  expect_true(.raw.plot.is.empty(plot_raw_quality(list(calibration = x$calibration))))
  expect_error(plot_raw_quality(x, metric = "nope"), "not a column")
})

test_that("plot_raw_quality band positions agree with wear$rout (MOS2)", {
  x <- mos2()
  rout <- x$wear$rout
  expect_identical(unname(colSums(rout[, c("r1", "r2", "r3", "r4", "r5")])), c(343, 0, 23, 0, 366))
  g <- plot_raw_quality(x)
  bands <- layer_with(g, "start_epoch")
  expect_true(is.data.frame(bands))
  expect_identical(names(bands), c("category", "start_epoch", "end_epoch", "xmin", "xmax"))
  expect_identical(levels(droplevels(bands$category)), c("not worn", "also not worn"))
  exp_r1 <- expected_runs(rout$r1)
  exp_r3 <- expected_runs(rout$r3)
  got_r1 <- bands[bands$category == "not worn", c("start_epoch", "end_epoch")]
  got_r3 <- bands[bands$category == "also not worn", c("start_epoch", "end_epoch")]
  rownames(got_r1) <- NULL; rownames(got_r3) <- NULL
  expect_identical(got_r1, exp_r1)
  expect_identical(got_r3, exp_r3)
  expect_identical(nrow(exp_r1), 7L)
  expect_identical(nrow(exp_r3), 5L)
  expect_identical(exp_r1$start_epoch, c(19L, 300L, 311L, 397L, 441L, 498L, 590L))
  expect_identical(exp_r1$end_epoch, c(23L, 307L, 390L, 436L, 491L, 585L, 660L))
  expect_identical(exp_r3$start_epoch, c(308L, 391L, 437L, 492L, 586L))
  expect_identical(exp_r3$end_epoch, c(310L, 396L, 440L, 497L, 589L))
  expect_identical(sum(got_r1$end_epoch - got_r1$start_epoch + 1L), 343L)
  expect_identical(sum(got_r3$end_epoch - got_r3$start_epoch + 1L), 23L)
  covered <- unlist(mapply(seq, got_r1$start_epoch, got_r1$end_epoch, SIMPLIFY = FALSE))
  expect_identical(sort(covered), which(rout$r1 == 1))
  covered3 <- unlist(mapply(seq, got_r3$start_epoch, got_r3$end_epoch, SIMPLIFY = FALSE))
  expect_identical(sort(covered3), which(rout$r3 == 1))
  # wall-clock edges: start of the first epoch, end of the last (start + ws2)
  tl <- as.POSIXct(x$meta$metalong$time, origin = "1970-01-01", tz = MOS2_TZ)
  expect_equal(as.numeric(bands$xmin), as.numeric(tl[bands$start_epoch]))
  expect_equal(as.numeric(bands$xmax), as.numeric(tl[bands$end_epoch]) + 900)
  expect_identical(format(bands$xmin[1], "%Y-%m-%d %H:%M:%S", tz = MOS2_TZ), "2025-10-08 01:00:00")
  expect_identical(format(bands$xmax[1], "%Y-%m-%d %H:%M:%S", tz = MOS2_TZ), "2025-10-08 02:15:00")
  # GGIR's createcoordinates gives the same starts, and the epoch after each run as x1
  cd1 <- .raw.plot.createcoordinates(rout$r1, 1:nrow(rout))
  expect_identical(as.integer(cd1$x0), exp_r1$start_epoch)
  expect_identical(as.integer(cd1$x1), c(exp_r1$end_epoch[-7] + 1L, 660L))
  cd3 <- .raw.plot.createcoordinates(rout$r3, 1:nrow(rout))
  expect_identical(as.integer(cd3$x0), exp_r3$start_epoch)
  expect_identical(as.integer(cd3$x1), exp_r3$end_epoch + 1L)
  expect_identical(.raw.plot.createcoordinates(rout$r2, 1:nrow(rout))$x0, c())
  expect_identical(nrow(.raw.plot.runs(c(0, 0, 0))), 0L)
  expect_identical(.raw.plot.runs(c(1, 1, 0, 1)), data.frame(start_epoch = c(1L, 4L), end_epoch = c(2L, 4L)))
  expect_identical(.raw.plot.runs(c(1, 1, 1)), data.frame(start_epoch = 1L, end_epoch = 3L))
})

test_that("plot_raw_quality draws the metric averaged to 15 min exactly as g.plot does, with GGIR's grid", {
  x <- mos2()
  ms <- x$meta$metashort; ml <- x$meta$metalong
  ws3 <- 5; ws2 <- 900
  # g.plot's averaging, computed here
  IndOtMetric <- which(colnames(ms) %in% c("timestamp", "time", "anglex", "angley", "anglez") == FALSE)[1]
  expect_identical(colnames(ms)[IndOtMetric], "ENMO")
  expect_identical(.raw.plot.metric(ms), "ENMO")
  accel <- as.numeric(as.matrix(ms[, IndOtMetric]))
  accel2 <- cumsum(c(0, accel))
  select <- seq(1, length(accel2), by = ws2 / ws3)
  Acceleration <- diff(accel2[round(select)]) / abs(diff(round(select[1:(length(select))])))
  expect_identical(length(Acceleration), 660L)
  got <- .raw.plot.average.long(ms$ENMO, ws3, ws2, nrow(ml))
  expect_identical(got, Acceleration)
  expect_equal(got[1], mean(ms$ENMO[1:180]), tolerance = 1e-12)
  expect_equal(got[660], mean(ms$ENMO[118621:118800]), tolerance = 1e-12)
  expect_identical(.raw.plot.average.long(ms$ENMO, ws3, ws2, 662)[661:662], c(0, 0))
  expect_identical(.raw.plot.average.long(ms$ENMO, ws3, ws2, 10), Acceleration[1:10])
  g <- plot_raw_quality(x)
  series <- Filter(function(L) is.data.frame(L$data) && all(c("panel", "time", "y") %in% names(L$data)) &&
                     any(grepl("ENMO", L$data$panel)), g$layers)
  expect_true(length(series) >= 1)
  d <- series[[1]]$data
  expect_identical(d$y, Acceleration)
  expect_identical(as.numeric(d$time), as.numeric(ml$time))
  score <- Filter(function(L) is.data.frame(L$data) && all(c("panel", "time", "y") %in% names(L$data)) &&
                    any(grepl("Non-wear", L$data$panel)), g$layers)
  expect_identical(score[[1]]$data$y, as.numeric(ml$nonwearscore))
  expect_identical(as.integer(table(score[[1]]$data$y)), c(308L, 9L, 343L))
  # midnights and noons from the timestamp strings, as g.plot greps them
  clock <- .raw.plot.clock(ml$timestamp)
  expect_identical(clock[1], "20:30:00")
  expect_identical(which(clock == "00:00:00"), grep("00:00:00", ml$timestamp))
  tl <- .raw.plot.time(ml, MOS2_TZ)
  grid <- .raw.plot.daygrid(tl, clock)
  expect_identical(length(grid$midnights), 7L)
  expect_identical(length(grid$noons), 7L)
  expect_identical(format(grid$midnights[1], "%Y-%m-%d %H:%M", tz = MOS2_TZ), "2025-10-08 00:00")
  expect_identical(format(grid$noons[7], "%Y-%m-%d %H:%M", tz = MOS2_TZ), "2025-10-14 12:00")
  expect_identical(sub("\n.*$", "", grid$labels), paste0("Day ", 2:8))
  expect_true(.raw.plot.metric.is.g("ENMO")); expect_true(.raw.plot.metric.is.g("BFEN"))
  expect_false(.raw.plot.metric.is.g("ZCX")); expect_false(.raw.plot.metric.is.g("NeishabouriCount_x"))
  expect_false(.raw.plot.metric.is.g("ExtAct"))
})

test_that("plot_raw_quality accepts a GGIR M list and a wear object, and matches the canhrActi path", {
  x <- mos2()
  M <- stored_M()
  g1 <- plot_raw_quality(x)
  g2 <- plot_raw_quality(M, wear = x$wear)           # GGIR tables, the wear decision supplied
  g3 <- plot_raw_quality(x$meta)                     # meta alone: wear computed inside
  b1 <- layer_with(g1, "start_epoch"); b2 <- layer_with(g2, "start_epoch"); b3 <- layer_with(g3, "start_epoch")
  expect_identical(b1[, c("category", "start_epoch", "end_epoch")], b2[, c("category", "start_epoch", "end_epoch")])
  expect_identical(b1[, c("category", "start_epoch", "end_epoch")], b3[, c("category", "start_epoch", "end_epoch")])
  # the GGIR list has no numeric time column: times come from the timestamp strings
  tl <- .raw.plot.time(M$metalong, MOS2_TZ)
  expect_identical(as.numeric(tl), as.numeric(x$meta$metalong$time))
  expect_gt(png_size(g2), 5 * 1024)
})

test_that("plot_raw_days uses visualReport's 36-hour windows and boxes the r5long epochs", {
  x <- mos2()
  ms <- x$meta$metashort
  clock <- .raw.plot.clock(ms$timestamp)
  win <- .raw.plot.windows(clock, 5, 36, 0)
  # visualReport's window rule, computed here; the recording does not start at an edge
  dayedges <- which(clock == "00:00:00")
  expect_identical(dayedges[1] != 1, TRUE)
  subploti <- c(1, dayedges)
  dayEnds <- c(dayedges + ((36 - 24) * (3600/5)) - 1, nrow(ms))
  subploti <- cbind(subploti, dayEnds)
  subploti[which(subploti[, 2] > nrow(ms)), 2] <- nrow(ms)
  expect_identical(win$first_epoch, as.integer(subploti[, 1]))
  expect_identical(win$last_epoch, as.integer(subploti[, 2]))
  expect_identical(nrow(win), 8L)
  expect_equal(win$first_epoch, as.integer(x$wear$daily$first_epoch))
  expect_identical(win$last_epoch[1], 2521L + 8640L - 1L)
  expect_identical(win$last_epoch[8], 118800L)
  # a recording that starts at an edge
  w2 <- .raw.plot.windows(clock[2521:118800], 5, 36, 0)
  expect_identical(w2$first_epoch[1], 1L)
  expect_identical(nrow(w2), 7L)
  w3 <- .raw.plot.windows(clock[1:100], 5, 36, 0)
  expect_identical(w3, data.frame(window = 1L, first_epoch = 1L, last_epoch = 100L))
  g <- plot_raw_days(x)
  boxes <- layer_with(g, "start_epoch")
  expect_true(is.data.frame(boxes))
  r5 <- as.numeric(x$wear$r5long)
  for (i in seq_len(nrow(boxes))) {
    expect_true(all(r5[boxes$start_epoch[i]:boxes$end_epoch[i]] == 1))
  }
  covered <- unique(unlist(mapply(seq, boxes$start_epoch, boxes$end_epoch, SIMPLIFY = FALSE)))
  expect_setequal(covered, which(r5 == 1))
  expect_identical(sum(r5 == 1), 366L * 180L)
  expect_identical(length(levels(boxes$window)), 8L)
  # visualReport's two stacked bands, widened apart: the angle in 0..36 and the metric in
  # 54..100 (visualReport uses 0..40 and 50..100) so the break labels do not collide
  series <- layer_with(g, "acc")
  expect_equal(series$acc[1], min(x$meta$metashort$ENMO[1] * 1000 * 46 / 500 + 54, 100))
  expect_equal(series$ang[1], (x$meta$metashort$anglez[1] + 90) * 36 / 180)
  expect_true(all(series$ang >= 0 & series$ang <= 36))
  expect_true(all(series$acc >= 54 & series$acc <= 100))
  # the angle is labelled on the left axis and the metric on the right; with eight windows
  # each band keeps only its two end labels
  yb <- ggplot2::layer_scales(g)$y$get_breaks()
  expect_identical(as.numeric(yb[!is.na(yb)]), c(0, 36))
  expect_true(!is.null(ggplot2::layer_scales(g)$y$secondary.axis))
  # a shorter recording has fewer windows, and then the middle label comes back
  keep <- 3L * 17280L
  x3 <- x
  x3$meta$metashort <- x$meta$metashort[seq_len(keep), , drop = FALSE]
  x3$wear$r5long <- x$wear$r5long[seq_len(keep)]
  n3 <- nrow(.raw.plot.windows(.raw.plot.clock(x3$meta$metashort$timestamp), 5, 36, 0))
  expect_identical(n3, 4L)
  yb3 <- ggplot2::layer_scales(plot_raw_days(x3))$y$get_breaks()
  expect_identical(as.numeric(yb3[!is.na(yb3)]), c(0, 18, 36))
  g4 <- plot_raw_days(x, dayborder = 4)
  w4 <- .raw.plot.windows(clock, 5, 36, 4)
  expect_identical(w4$first_epoch[2], which(clock == "04:00:00")[1])
  expect_gt(png_size(g4, 9, 12), 5 * 1024)
})

test_that("plot_raw_nights builds one window per noon-to-noon night with the angle and the scaled metric", {
  x <- mos2()
  g <- plot_raw_nights(x)
  series <- layer_with(g, "angle")
  expect_true(is.data.frame(series))
  expect_identical(length(levels(series$night)), 8L)
  expect_identical(levels(series$night)[1], "Night 1: Tue 07 Oct 12:00 to next noon")
  expect_identical(levels(series$night)[8], "Night 8: Tue 14 Oct 12:00 to next noon")
  expect_identical(nrow(series), 118800L)
  # night 1 starts 8.5 h after the noon it belongs to (20:30) and ends just before the next noon
  n1 <- series[series$night == levels(series$night)[1], ]
  expect_equal(n1$h[1], 8.5)
  expect_equal(max(n1$h), 24 - 5 / 3600)
  expect_identical(nrow(n1), 2520L + 8640L)
  # the angle is drawn as it is; the metric is scaled into the band 0..500 mg -> -260..-150
  expect_identical(n1$angle, x$meta$metashort$anglez[1:nrow(n1)])
  expect_equal(n1$acc[1], -260 + min(x$meta$metashort$ENMO[1] * 1000, 500) / 500 * 110)
  boxes <- layer_with(g, "xmin")
  expect_true(is.data.frame(boxes))
  expect_true(all(boxes$xmax > boxes$xmin))
})

test_that("plot_raw_calibration recomputes the corrected still windows with g.calibrate's scale() call", {
  x <- mos2()
  cal <- x$calibration
  sd <- cal$spheredata
  expect_identical(dim(sd), c(1176L, 7L))
  after <- .raw.plot.sphere.after(sd, cal$scale, cal$offset, cal$tempoffset, use_temp = FALSE)
  # g.calibrate's own scale() call
  features_temp2 <- scale(as.matrix(sd[, 2:4]), center = -cal$offset, scale = 1/cal$scale)
  expect_identical(unname(after), unname(matrix(as.numeric(features_temp2), ncol = 3)))
  # the errors GGIR reports are the mean abs(EN - 1) of these clouds, rounded to 5 decimals
  before <- as.matrix(sd[, c("meanx", "meany", "meanz")])
  err0 <- round(mean(abs(sqrt(rowSums(before^2)) - 1)), 5)
  err1 <- round(mean(abs(sqrt(rowSums(after^2)) - 1)), 5)
  expect_identical(err0, cal$cal_error_start)
  expect_identical(err1, cal$cal_error_end)
  expect_identical(err0, 0.01614); expect_identical(err1, 0.0058)
  g <- plot_raw_calibration(x)
  pts <- layer_with(g, "stage")
  expect_identical(nrow(pts), 1176L * 3L * 2L)
  expect_identical(levels(pts$pair), c("x against y", "x against z", "y against z"))
  xy_after <- pts[pts$pair == "x against y" & pts$stage == "after", ]
  expect_equal(xy_after$a, unname(after[, "x"]))
  expect_equal(xy_after$b, unname(after[, "y"]))
  xy_before <- pts[pts$pair == "x against y" & pts$stage == "before", ]
  expect_equal(xy_before$a, unname(sd$meanx))
  expect_true(grepl("16.1 mg", g$labels$subtitle, fixed = TRUE))
  expect_true(grepl("5.8 mg", g$labels$subtitle, fixed = TRUE))
  expect_true(grepl("applied", g$labels$subtitle, fixed = TRUE))
  # every axis crosses +/- 0.3 g, which is what the dashed lines show
  expect_true(all(apply(before, 2, min) < -0.3) && all(apply(before, 2, max) > 0.3))
  # no still windows: the empty state
  cal2 <- cal; cal2["spheredata"] <- list(NULL)
  e <- plot_raw_calibration(cal2)
  expect_true(.raw.plot.is.empty(e))
  expect_match(.raw.plot.empty.message(e), "No still windows")
  cal3 <- cal2; cal3$source <- "supplied"
  expect_match(.raw.plot.empty.message(plot_raw_calibration(cal3)), "supplied")
  # the temperature branch uses the eighth column when asked for
  sdt <- sd; sdt$temperature <- 20 + seq_len(nrow(sd)) / 100
  aft <- .raw.plot.sphere.after(sdt, cal$scale, cal$offset, c(0.001, 0.002, 0.003), use_temp = TRUE, meantempcal = 25)
  yy <- as.matrix(cbind(sdt[, 8], sdt[, 8], sdt[, 8]))
  ref <- scale(as.matrix(sdt[, 2:4]), center = -cal$offset, scale = 1/cal$scale) +
    scale(yy, center = rep(25, 3), scale = 1/c(0.001, 0.002, 0.003))
  expect_identical(unname(aft), unname(matrix(as.numeric(ref), ncol = 3)))
})

test_that("plot_raw_gaps draws the qclog blocks and the raw-level and epoch-level fills of MOS2", {
  x <- mos2()
  ms <- x$meta$metashort
  runs <- .raw.plot.fill.runs(ms, 5, 900)
  expect_identical(names(runs), c("start_epoch", "end_epoch", "length", "level"))
  expect_true(all(runs$length >= 2))
  long <- runs[runs$level == "epoch", ]
  expect_identical(nrow(long), 15L)
  expect_identical(long$length, c(1261L, 5041L, 1981L, 3781L, 1261L, 1441L, 7021L, 9001L, 5581L, 6841L,
                                  1621L, 1081L, 8461L, 1261L, 2161L))
  expect_identical(sum(long$length - 1L), 57780L)                 # the epoch-level replicas of the getmeta trace
  expect_identical(sum(long$length - 1L), as.integer(sum(x$meta$chunks$epochs_short_added)))
  expect_true(all(runs$length[runs$level == "raw"] <= 1080))
  expect_true(all(long$length > 1080))
  for (i in seq_len(nrow(long))) {
    idx <- long$start_epoch[i]:long$end_epoch[i]
    expect_identical(length(unique(ms$ENMO[idx])), 1L); expect_identical(length(unique(ms$anglez[idx])), 1L)
    expect_identical(unique(ms$ENMO[idx]), 0)
  }
  # the epoch-level runs sit inside the non-wear 3 long epochs
  nw3 <- which(x$meta$metalong$nonwearscore == 3)
  for (i in seq_len(nrow(long))) {
    le <- unique(ceiling((long$start_epoch[i]:long$end_epoch[i]) / 180))
    expect_true(mean(le %in% nw3) > 0.95, info = paste("long run", i))
  }
  g <- plot_raw_gaps(x)
  blocks <- layer_with(g, "gaps")
  expect_identical(nrow(blocks), 2L)
  expect_identical(blocks$gaps, c(915, 525))
  expect_equal(blocks$minutes, x$meta$qclog$timegaps_min)
  expect_identical(as.numeric(blocks$xmin), as.numeric(x$meta$qclog$start))
  expect_identical(as.numeric(blocks$xmax), as.numeric(x$meta$qclog$start + x$meta$qclog$blockLengthSeconds))
  # three short lines, so the label fits inside the block
  expect_true(grepl("block 1\n915 gaps\n1358.5 min", blocks$label[1], fixed = TRUE))
  # row labels stay short; a discrete y scale reserves the width of its longest label
  expect_true(all(nchar(levels(blocks$row)) <= 20))
  fills <- layer_with(g, "level")
  expect_identical(nrow(fills), nrow(runs))
  expect_identical(sum(fills$level == "epoch"), 15L)
  expect_true(grepl("1440 gaps totalling 7451.7 min", g$labels$subtitle, fixed = TRUE))
  expect_true(grepl("15 exceeded 90 min", g$labels$subtitle, fixed = TRUE))
  expect_true(grepl("80.2 h", g$labels$subtitle, fixed = TRUE))
  miss <- data.frame(time = as.POSIXct(ms$time[c(100, 5000)], origin = "1970-01-01", tz = MOS2_TZ), n_missing = c(60, 300))
  g2 <- plot_raw_gaps(x, missingness = miss)
  smp <- layer_with(g2, "n_missing")
  expect_identical(nrow(smp), 2L)
  # 60 samples at 30 Hz are 2 s, drawn no narrower than one 5-s epoch; 300 samples are 10 s
  expect_equal(as.numeric(difftime(smp$xmax, smp$xmin, units = "secs")), pmax(c(60, 300) / 30, 5))
  expect_identical(length(levels(smp$row)), 4L)
  expect_gt(png_size(g2), 5 * 1024)
  m2 <- x$meta; m2["qclog"] <- list(NULL)
  g3 <- plot_raw_gaps(m2)
  expect_false(.raw.plot.is.empty(g3))
  expect_null(layer_with(g3, "gaps"))
  expect_match(g3$labels$subtitle, "No gap log")
})

test_that("plot_raw_chunks carries the calibration trace of every chunk", {
  x <- mos2()
  ch <- x$calibration$chunks
  expect_identical(nrow(ch), 5L)
  g <- plot_raw_chunks(x)
  d <- layer_with(g, "series")
  expect_identical(d$y[d$series == "before fit"], ch$error_start * 1000)
  expect_identical(d$y[d$series == "after fit"], ch$error_end * 1000)
  expect_identical(d$y[d$series == "still windows"], as.numeric(ch$still_windows))
  expect_identical(d$block[d$series == "before fit"], ch$block)
  expect_identical(ch$hours, c(12, 24, 36, 41, 41))
  expect_identical(ch$still_windows, c(346L, 757L, 1100L, 1176L, 1176L))
  expect_true(grepl("reached 41 h and was not accepted", g$labels$subtitle, fixed = TRUE))
  expect_true(grepl("168 h", g$labels$subtitle, fixed = TRUE))
  c0 <- x$calibration; c0$chunks <- c0$chunks[0, ]
  expect_true(.raw.plot.is.empty(plot_raw_chunks(c0)))
  c1 <- c0; c1$source <- "supplied"
  expect_match(.raw.plot.empty.message(plot_raw_chunks(c1)), "supplied")
  # a calibration whose only chunk read nothing: empty state too
  s <- short_obj()
  expect_identical(nrow(s$calibration$chunks), 1L)
  expect_identical(s$calibration$chunks$rows, 0)
  expect_match(.raw.plot.empty.message(plot_raw_chunks(s$calibration)), "No calibration chunk with data")
})

test_that("plot_raw_wear_days draws every calendar day against includedaycrit", {
  x <- mos2()
  g <- plot_raw_wear_days(x)
  d <- layer_with(g, "n_valid_hours")
  expect_identical(nrow(d), 8L)
  expect_identical(d$n_valid_hours, c(3.5, 22.75, 24, 23.25, 0, 0, 0, 0))
  expect_identical(d$n_hours, c(3.5, 24, 24, 24, 24, 24, 24, 17.5))
  expect_identical(as.character(d$validity), c("below the criterion", "valid day", "valid day", "valid day",
                                              rep("below the criterion", 4)))
  expect_identical(as.character(d$label)[1], "Tue\n07 Oct")
  expect_true(grepl("3 of 8 days reach 16 valid hours", g$labels$subtitle, fixed = TRUE))
  hl <- Filter(function(L) inherits(L$geom, "GeomHline"), g$layers)
  expect_identical(length(hl), 1L)
  expect_identical(hl[[1]]$data$yintercept, 16)
  expect_false(.raw.plot.is.empty(plot_raw_wear_days(x$wear)))
  g2 <- plot_raw_wear_days(x$meta)
  expect_identical(layer_with(g2, "n_valid_hours")$n_valid_hours, d$n_valid_hours)
})

test_that("raw_plots.R carries the attribution header and no em-dashes", {
  pkg_root <- testthat::test_path("..", "..")
  f <- file.path(pkg_root, "R", "raw_plots.R")
  skip_if_no_file(f)
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  expect_match(lines[1], "^# Ported from GGIR 3.3-9 R/g.plot.R")
  expect_true(any(grepl("Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR", lines, fixed = TRUE)))
  expect_true(any(grepl("This file is a MODIFIED version of the original", lines, fixed = TRUE)))
  expect_false(any(grepl(intToUtf8(0x2014), lines, fixed = TRUE)))
  expect_false(any(grepl("[^\x01-\x7F]", lines, perl = TRUE, useBytes = TRUE)))
  expect_identical(sum(grepl("^#' @export", lines)), 7L)
})
