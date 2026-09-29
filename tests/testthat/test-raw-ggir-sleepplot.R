# raw.ggir.sleepplot() (R/raw_ggir_sleepplot.R): GGIR's part-4 sleep figure on the current
# device. A two-night table built here checks the bars, colours and labels drawn; the MOS2
# nights come from GGIR's stored part-3 milestone in the folder named by CANHRACTI_GGIR_REF,
# which was written in the system zone of a machine set to America/Anchorage.

withr::local_timezone("America/Anchorage")

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

# the night table shape raw.sleep.nights() returns, cut to what the figure reads
.nights <- function(episodes, guiders, dolog = FALSE, id = "rec.gt3x") {
  ns <- data.frame(ID = rep(id, nrow(guiders)), night = guiders$night,
                   sleepparam = rep("T5A5", nrow(guiders)))
  attr(ns, "canhrActi") <- list(episodes = episodes, guiders = guiders, dolog = dolog, id = id)
  ns
}
# night 1: a 22:30 to 06:00 bout inside the guider and a 14:00 to 15:00 one outside it;
# night 2: a day sleeper whose bout wraps from 11:00 to 13:00
.two_nights <- function(first = 1) {
  ep <- data.frame(night = first + c(0, 0, 1), def = "T5A5", start = c(22.5, 14, 35),
                   end = c(30, 15, 13), overlapGuider = c(1, 0, 1))
  gu <- data.frame(night = first + 0:1, guider_onset = c(23, 23.5),
                   guider_wakeup = c(31, 31.5), cleaningcode = c(0, 0),
                   daysleeper = c(FALSE, TRUE))
  .nights(ep, gu)
}

# draw into a png while recording the rect, lines, legend and axis calls
.draw_recorded <- function(nights, file, ...) {
  calls <- list(rect = list(), lines = list(), legend = list(), axis = list())
  keep <- function(what, fun) {
    force(fun)
    function(...) {
      calls[[what]][[length(calls[[what]]) + 1]] <<- list(...)
      fun(...)
    }
  }
  local_mocked_bindings(rect = keep("rect", graphics::rect),
                        lines = keep("lines", graphics::lines),
                        legend = keep("legend", graphics::legend),
                        axis = keep("axis", graphics::axis))
  grDevices::png(file, width = 900, height = 500)
  out <- tryCatch(raw.ggir.sleepplot(nights, ...), finally = grDevices::dev.off())
  list(out = out, calls = calls, file = file)
}
.bars <- function(d) Filter(function(z) is.null(z$density), d$calls$rect)
.guider_boxes <- function(d) Filter(function(z) !is.null(z$density), d$calls$rect)
.y_axis <- function(d) Filter(function(z) identical(z$side, 2), d$calls$axis)[[1]]

# pixel count of each colour in a png
.png_colours <- function(file) {
  img <- png::readPNG(file)
  table(grDevices::rgb(img[, , 1], img[, , 2], img[, , 3]))
}
.n_px <- function(px, colour) if (colour %in% names(px)) px[[colour]] else 0L

test_that("each bout is a bar on its night's row, coloured by whether it meets the guider", {
  d <- .draw_recorded(.two_nights(), file.path(withr::local_tempdir(), "sleep.png"))
  expect_identical(d$out, list(state = "ok", nights = 2L))
  b <- .bars(d)
  # the wrapping bout is split at the two ends of the 12 to 36 axis
  expect_identical(vapply(b, function(z) c(z$xleft, z$xright), numeric(2)),
                   cbind(c(22.5, 30), c(14, 15), c(35, 36), c(12, 13)))
  expect_identical(vapply(b, `[[`, "", "col"), c("#3300FF", "#CCFF00", "#3300FF", "#3300FF"))
  # one sib definition fills its row from 0.3 below to 0.3 above
  expect_equal(vapply(b, function(z) c(z$ybottom, z$ytop), numeric(2)),
               cbind(c(0.7, 1.3), c(0.7, 1.3), c(1.7, 2.3), c(1.7, 2.3)))
  # the guider window is hatched black
  g <- .guider_boxes(d)
  expect_identical(vapply(g, function(z) c(z$xleft, z$xright), numeric(2)),
                   cbind(c(23, 31), c(23.5, 31.5)))
  expect_identical(vapply(g, `[[`, "", "col"), c("black", "black"))
  expect_identical(vapply(g, `[[`, 0, "density"), c(20, 20))
  # the day sleeper gets GGIR's dashed mark at 18:00 on its row
  ds <- Filter(function(z) identical(z$x, c(18, 18)), d$calls$lines)
  expect_identical(length(ds), 1L)
  expect_equal(ds[[1]]$y, c(1.7, 2.3))
})

test_that("the axis runs noon to noon, the legend is GGIR's and the rows are labelled", {
  d <- .draw_recorded(.two_nights(), file.path(withr::local_tempdir(), "sleep.png"))
  x_axis <- Filter(function(z) identical(z$side, 1), d$calls$axis)[[1]]
  expect_identical(x_axis$at, 12:36)
  expect_identical(x_axis$labels, c(12:24, 1:12))
  expect_identical(.y_axis(d)$labels[1:2], c("IDrec.gt3x night1", "IDrec.gt3x night2"))
  expect_identical(length(d$calls$legend), 1L)
  expect_identical(d$calls$legend[[1]]$legend,
                   c("sibT5A5_spt", "sibT5A5_day", "guider, e.g. diary"))
  expect_identical(d$calls$legend[[1]]$fill, c("#3300FF", "#CCFF00", "black"))
  # the png holds both bar colours
  skip_if_not_installed("png")
  px <- .png_colours(d$file)
  expect_gt(.n_px(px, "#3300FF"), 1000)
  expect_gt(.n_px(px, "#CCFF00"), 100)
  expect_gt(1 - .n_px(px, "#FFFFFF") / sum(px), 0.02)
})

test_that("rows carry the night number when the first night is not in the table", {
  d <- .draw_recorded(.two_nights(first = 2), file.path(withr::local_tempdir(), "sleep.png"))
  expect_identical(.y_axis(d)$labels[1:2], c("IDrec.gt3x night2", "IDrec.gt3x night3"))
})

test_that("legend = FALSE drops the legend and nnpp fixes the number of rows", {
  d <- .draw_recorded(.two_nights(), file.path(withr::local_tempdir(), "sleep.png"),
                      legend = FALSE, nnpp = 40)
  expect_identical(length(d$calls$legend), 0L)
  y <- .y_axis(d)
  expect_identical(y$at, 1:40)
  expect_identical(y$labels[3:40], rep(" ", 38))
})

test_that("nights GGIR cleans out are not drawn, and a diary tightens the rule", {
  ns <- .two_nights()
  a <- attr(ns, "canhrActi")
  a$guiders$cleaningcode <- c(2, 1)
  attr(ns, "canhrActi") <- a
  d <- .draw_recorded(ns, file.path(withr::local_tempdir(), "sleep.png"))
  expect_identical(d$out, list(state = "ok", nights = 1L))
  # only night 2 is drawn, on the first row
  b <- .bars(d)
  expect_identical(vapply(b, `[[`, 0, "xleft"), c(35, 12))
  expect_equal(b[[1]]$ybottom, 0.7)
  # with a diary, cleaningcode 1 is cleaned as well
  a$dolog <- TRUE
  attr(ns, "canhrActi") <- a
  expect_identical(raw.ggir.sleepplot(ns), list(state = "all_cleaned", nights = 0L))
})

test_that("a table without episodes or nights says so", {
  expect_identical(raw.ggir.sleepplot(data.frame(ID = "rec")),
                   list(state = "no_episodes", nights = 0L))
  ns <- .nights(data.frame(night = integer(0), def = character(0)), data.frame())
  expect_identical(raw.ggir.sleepplot(ns)$state, "no_nights")
})

test_that("on MOS2 the two nights GGIR keeps are drawn bout for bout", {
  skip_if_no_ggir_ref()
  p3 <- ref_file("out", "output_din", "meta", "ms3.out", "MOS2E39230594.gt3x.RData")
  skip_if_no_file(p3)
  e <- new.env(parent = emptyenv())
  load(p3, envir = e)
  ns <- suppressWarnings(raw.sleep.nights(as.list(e), filename = "MOS2E39230594.gt3x.RData",
                                          do.visual = FALSE))
  a <- attr(ns, "canhrActi")
  # without a diary GGIR cleans nights 1 and 4, whose cleaningcode is 2
  expect_identical(a$guiders$cleaningcode, c(2, 1, 1, 2))
  d <- .draw_recorded(ns, file.path(withr::local_tempdir(), "mos2.png"))
  expect_identical(d$out, list(state = "ok", nights = 2L))
  ep <- a$episodes[a$episodes$night %in% c(2, 3), ]
  b <- .bars(d)
  expect_identical(length(b), nrow(ep) + sum(ep$start > ep$end))
  expect_identical(sum(vapply(b, `[[`, "", "col") == "#3300FF"), sum(ep$overlapGuider == 1))
  expect_equal(vapply(.guider_boxes(d), `[[`, 0, "xleft"), a$guiders$guider_onset[2:3])
  expect_identical(sub(".* ", "", .y_axis(d)$labels[1:2]), c("night2", "night3"))
  skip_if_not_installed("png")
  px <- .png_colours(d$file)
  expect_gt(.n_px(px, "#3300FF"), 1000)
  expect_gt(.n_px(px, "#CCFF00"), 1000)
})

test_that("with excludefirstlast on MOS2, the rows are named as GGIR's own part 4 names them", {
  skip_if_no_ggir_ref()
  skip_if_not_installed("GGIR")
  skip_if_not_installed("pdftools")
  p3 <- ref_file("out", "output_din", "meta", "ms3.out", "MOS2E39230594.gt3x.RData")
  skip_if_no_file(p3)
  # GGIR's own part 4 on a copy of the stored part-3 milestone
  md <- withr::local_tempdir()
  dir.create(file.path(md, "meta", "ms3.out"), recursive = TRUE)
  dir.create(file.path(md, "results"))
  file.copy(p3, file.path(md, "meta", "ms3.out"))
  suppressWarnings(GGIR::g.part4(metadatadir = md, f0 = 1, f1 = 1, excludefirstlast = TRUE,
                                 do.visual = TRUE, verbose = FALSE))
  txt <- pdftools::pdf_text(file.path(md, "results", "visualisation_sleep.pdf"))
  # the page reads top down and the rows count up from the bottom
  ggir_rows <- rev(regmatches(txt, gregexpr("night[0-9]+", txt))[[1]])
  expect_identical(ggir_rows, c("night2", "night3"))
  e <- new.env(parent = emptyenv())
  load(p3, envir = e)
  ns <- suppressWarnings(raw.sleep.nights(as.list(e), filename = "MOS2E39230594.gt3x.RData",
                                          do.visual = FALSE, excludefirstlast = TRUE))
  expect_equal(attr(ns, "canhrActi")$guiders$night, c(2, 3))
  d <- .draw_recorded(ns, file.path(withr::local_tempdir(), "mos2.png"))
  expect_identical(d$out, list(state = "ok", nights = 2L))
  expect_identical(sub(".* ", "", .y_axis(d)$labels[1:2]), ggir_rows)
})
