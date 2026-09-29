# raw.ggir.panelplot() and raw.ggir.panel.png() (R/raw_ggir_panelplot.R): GGIR's report panel
# over a milestone tree. A small tree written here has its classes at known clock times;
# GGIR's own MOS2 tree in the folder named by CANHRACTI_GGIR_REF has the real ones. Both
# keep their time series in the system zone, which for MOS2 was America/Anchorage.

withr::local_timezone("America/Anchorage")

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
# GGIR's own milestone tree for MOS2
mos2_tree <- function() {
  skip_if_no_ggir_ref()
  d <- file.path(sub("/+$", "", .ggir_ref), "out", "output_din")
  f <- file.path(d, "meta", "ms5.outraw", "40_100_400", "MOS2E39230594_T5A5.RData")
  if (!file.exists(f)) testthat::skip(paste0("reference file not found: ", f))
  d
}

# the 18 behavioural classes GGIR writes to meta/ms5.outraw/behavioralcodes<date>.csv
GGIR_CLASSES <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
                  "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt",
                  "day_MVPA_bts_10", "day_MVPA_bts_5_10", "day_MVPA_bts_1_5", "day_IN_bts_30",
                  "day_IN_bts_20_30", "day_IN_bts_10_20", "day_LIG_bts_10", "day_LIG_bts_5_10",
                  "day_LIG_bts_1_5")

# A tree with GGIR's time series columns: 36 hours from noon at one minute, asleep 23:00 to
# 07:00, unbouted light by day, unbouted moderate 14:00 to 14:30 and a 30 minute inactive bout
# class 16:00 to 17:00 on both days.
.write_tree <- function(root, empty = FALSE) {
  raw <- file.path(root, "meta", "ms5.outraw")
  dir.create(file.path(raw, "40_100_400"), recursive = TRUE)
  utils::write.csv(data.frame(class_name = GGIR_CLASSES, class_id = seq_along(GGIR_CLASSES) - 1L),
                   file.path(raw, "behavioralcodes2024-03-04.csv"), row.names = FALSE)
  ts <- seq(as.POSIXct("2024-03-04 12:00:00"), by = 60, length.out = 36 * 60)
  lt <- as.POSIXlt(ts)
  h <- lt$hour + lt$min / 60
  spt <- as.numeric(h >= 23 | h < 7)
  cls <- ifelse(spt == 1, 0, 6)
  cls[h >= 14 & h < 14.5] <- 7
  cls[h >= 16 & h < 17] <- 12
  mdat <- data.frame(timenum = as.numeric(ts), ACC = ifelse(spt == 1, 5, 60),
                     SleepPeriodTime = spt, invalidepoch = 0, guider = 2,
                     window = as.numeric(lt$mday - 3), sibdetection = 0,
                     selfreported = factor(rep(NA, length(ts))),
                     angle = ifelse(spt == 1, -60, 20), class_id = cls, invalid_fullwindow = 0,
                     invalid_sleepperiod = 0, invalid_wakinghours = 0, timestamp = ts)
  if (empty) mdat <- mdat[0, ]
  filename <- "rec.gt3x"
  Lnames <- GGIR_CLASSES
  desiredtz <- ""
  save(mdat, filename, Lnames, desiredtz,
       file = file.path(raw, "40_100_400", "rec_T5A5.RData"))
  root
}

.png_colours <- function(file) {
  img <- png::readPNG(file)
  table(grDevices::rgb(img[, , 1], img[, , 2], img[, , 3]))
}
.n_px <- function(px, colour) if (colour %in% names(px)) px[[colour]] else 0L

test_that("the rows are one per calendar day plus a shorter legend band", {
  root <- .write_tree(withr::local_tempdir())
  n <- raw.ggir.panelplot(root, count_only = TRUE)
  expect_identical(n$state, "ok")
  expect_identical(c(n$panels, n$rows), c(2L, 3L))
  expect_equal(n$height_rows, 2.7)
  expect_true(n$legend)
  n2 <- raw.ggir.panelplot(root, count_only = TRUE, legend = FALSE)
  expect_identical(c(n2$panels, n2$rows), c(2L, 2L))
  expect_equal(n2$height_rows, 2)
  expect_false(n2$legend)
})

test_that("the class bars sit at the clock times of the classes, in GGIR's colours", {
  root <- .write_tree(withr::local_tempdir())
  rects <- list()
  legends <- list()
  local_mocked_bindings(
    rect = function(...) {
      rects[[length(rects) + 1]] <<- list(...)
      graphics::rect(...)
    },
    legend = function(...) {
      legends[[length(legends) + 1]] <<- list(...)
      graphics::legend(...)
    })
  f <- file.path(withr::local_tempdir(), "panel.png")
  grDevices::png(f, width = 900, height = 450)
  out <- raw.ggir.panelplot(root)
  grDevices::dev.off()
  expect_identical(out$panels, 2L)
  # GGIR's colour for a class, before its alpha
  of <- function(colour) Filter(function(z) substr(z$col[1], 1, 7) == colour, rects)
  at <- function(s) as.POSIXct(paste("2024-03-0", s, sep = ""))
  # unbouted moderate is orange in the lower band, a bar ends at the epoch after the class
  mod <- of("#D55E00")
  expect_identical(length(mod), 2L)
  expect_equal(c(mod[[1]]$xleft, mod[[1]]$xright), at(c("4 14:00:00", "4 14:30:00")))
  expect_equal(c(mod[[2]]$xleft, mod[[2]]$xright), at(c("5 14:00:00", "5 14:30:00")))
  expect_identical(c(mod[[1]]$ybottom, mod[[1]]$ytop), c(50, 90))
  # the 30 minute inactive bout class is pink and fills the full band
  inact <- of("#CC79A7")
  expect_identical(length(inact), 2L)
  expect_equal(c(inact[[2]]$xleft, inact[[2]]$xright), at(c("5 16:00:00", "5 17:00:00")))
  expect_identical(c(inact[[2]]$ybottom, inact[[2]]$ytop), c(50, 100))
  # unbouted light is green, three stretches a day
  expect_identical(vapply(of("#009E73"), function(z) length(z$xleft), 0L), c(3L, 3L))
  # GGIR's legend, with its bout names spelled out
  expect_identical(length(legends), 1L)
  lab <- legends[[1]]$legend
  expect_true(all(c("spt sleep", "inactive bts >=30 mins", "lipa bts [5,10) mins",
                    "no movement (sib daytime)", "ignored/imputed") %in% lab))
  skip_if_not_installed("png")
  px <- .png_colours(f)
  expect_gt(1 - .n_px(px, "#FFFFFF") / sum(px), 0.05)
  expect_gt(.n_px(px, "#009E73"), 1000)
})

test_that("a tree without a usable time series says why and draws nothing", {
  root <- withr::local_tempdir()
  expect_identical(raw.ggir.panelplot(root)$state, "no_ms5outraw")
  dir.create(file.path(root, "meta", "ms5.outraw", "40_100_400"), recursive = TRUE)
  expect_identical(raw.ggir.panelplot(root)$state, "no_timeseries")
  e <- raw.ggir.panelplot(.write_tree(withr::local_tempdir(), empty = TRUE))
  expect_identical(e$state, "empty")
  expect_identical(e$rows, 0L)
  skip_if_not_installed("pdftools")
  skip_if_not_installed("png")
  f <- file.path(withr::local_tempdir(), "none.png")
  r <- raw.ggir.panel.png(root, f)
  expect_identical(r$state, "no_timeseries")
  expect_false(file.exists(f))
})

test_that("raw.ggir.panel.png rasterises the panel at GGIR's row geometry", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("png")
  root <- .write_tree(withr::local_tempdir())
  f <- file.path(withr::local_tempdir(), "panel.png")
  r <- suppressMessages(raw.ggir.panel.png(root, f, width_px = 600))
  expect_identical(r$state, "ok")
  expect_identical(r$file, f)
  expect_identical(r$rows, 3L)
  img <- png::readPNG(f)
  expect_identical(dim(img)[2:1], c(r$width, r$height))
  # a row is 7.77 by 11.19 / 8 inches and the legend band 0.7 of one
  expect_lte(abs(r$width - 600), 1)
  expect_lte(abs(r$height - 600 / 7.77 * 11.19 / 8 * 2.7), 2)
  px <- .png_colours(f)
  expect_gt(1 - .n_px(px, "#FFFFFF") / sum(px), 0.05)
  expect_gt(.n_px(px, "#009E73"), 1000)
})

test_that("GGIR's MOS2 tree has a row per calendar day of the recording", {
  md <- mos2_tree()
  n <- raw.ggir.panelplot(md, count_only = TRUE)
  expect_identical(n$state, "ok")
  expect_identical(c(n$panels, n$rows), c(8L, 9L))
  expect_equal(n$height_rows, 8.7)
  # rows that start at noon also give eight windows for this recording
  expect_identical(raw.ggir.panelplot(md, count_only = TRUE, focus = "night")$panels, 8L)
})

test_that("validcrit keeps the MOS2 days whose share of valid epochs reaches it", {
  md <- mos2_tree()
  e <- new.env()
  load(file.path(md, "meta", "ms5.outraw", "40_100_400", "MOS2E39230594_T5A5.RData"), envir = e)
  valid <- tapply(e$mdat$invalidepoch == 0, format(e$mdat$timestamp, "%Y-%m-%d"), mean)
  expect_identical(length(valid), 8L)
  # three days reach 96 %, one of them only just
  expect_identical(sum(valid >= 0.96), 3L)
  expect_identical(raw.ggir.panelplot(md, count_only = TRUE, validcrit = 0.96)$panels, 3L)
  # two days are wholly valid, which the drawing tests below rely on
  expect_identical(sum(valid == 1), 2L)
  # no day can reach more than all of it; GGIR says so and there is nothing to draw
  expect_message(n0 <- raw.ggir.panelplot(md, count_only = TRUE, validcrit = 1.01),
                 "valid data criteria")
  expect_identical(c(n0$panels, n0$rows), c(0L, 0L))
})

test_that("the MOS2 panel draws straight to a png device", {
  skip_if_not_installed("png")
  md <- mos2_tree()
  f <- file.path(withr::local_tempdir(), "device.png")
  grDevices::png(f, width = 800, height = 290)
  out <- raw.ggir.panelplot(md, validcrit = 0.99, legend = FALSE)
  grDevices::dev.off()
  expect_identical(c(out$panels, out$rows), c(2L, 2L))
  px <- .png_colours(f)
  expect_gt(1 - .n_px(px, "#FFFFFF") / sum(px), 0.05)
  # the acceleration and angle traces
  expect_gt(.n_px(px, "#000000"), 1000)
})

test_that("the MOS2 panel through the pdf keeps GGIR's class colours", {
  skip_if_not_installed("pdftools")
  skip_if_not_installed("png")
  md <- mos2_tree()
  f <- file.path(withr::local_tempdir(), "pdf.png")
  r <- suppressMessages(raw.ggir.panel.png(md, f, width_px = 800, validcrit = 0.99,
                                           legend = FALSE, desiredtz = "America/Anchorage"))
  expect_identical(r$state, "ok")
  expect_identical(r$rows, 2L)
  # without the legend these colours can only come from the class bars
  px <- .png_colours(f)
  for (colour in c("#CC79A7", "#009E73", "#D55E00")) {
    expect_gt(.n_px(px, colour), 0, label = colour)
  }
})
