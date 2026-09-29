# Discrete colour and fill scales and the session-wide theme setter (R/theme.R)

.three <- data.frame(x = 1:3, y = 1:3, g = factor(c("b", "a", "c")))

test_that("scale_color_canhrActi maps factor levels to the categorical palette in level order", {
  p <- ggplot2::ggplot(.three, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_point() +
    scale_color_canhrActi()
  pts <- ggplot2::ggplot_build(p)$data[[1]]
  pal <- canhrActi_colors(type = "categorical")
  # rows are b, a, c, so level 2, 1, 3
  expect_identical(pts$colour, pal[c(2, 1, 3)])
  # Okabe-Ito blue, orange and bluish green lead the palette
  expect_identical(pal[1:3], c("#0072B2", "#E69F00", "#009E73"))
})

test_that("scale_color_canhrActi is a discrete colour scale that passes its extra arguments on", {
  s <- scale_color_canhrActi(name = "Group")
  expect_s3_class(s, "ScaleDiscrete")
  expect_identical(s$aesthetics, "colour")
  expect_identical(s$name, "Group")

  p <- ggplot2::ggplot(.three, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_point() +
    scale_color_canhrActi(type = "diverging")
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$colour,
                   canhrActi_colors(type = "diverging")[c(2, 1, 3)])
})

test_that("scale_fill_canhrActi fills with the requested palette type", {
  s <- scale_fill_canhrActi(name = "Band")
  expect_s3_class(s, "ScaleDiscrete")
  expect_identical(s$aesthetics, "fill")
  expect_identical(s$name, "Band")

  p <- ggplot2::ggplot(.three, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col() +
    scale_fill_canhrActi(type = "sequential")
  expect_identical(ggplot2::ggplot_build(p)$data[[1]]$fill, c("#DEEBF7", "#F7FBFF", "#C6DBEF"))
})

test_that("the named intensity palette is matched by level name, not by position", {
  d <- data.frame(x = 1:3, y = 1:3, g = c("vigorous", "sedentary", "light"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col() +
    scale_fill_canhrActi(type = "intensity_named")
  got <- ggplot2::ggplot_build(p)$data[[1]]
  expect_identical(got$fill[order(got$x)], c("#E69F00", "#64748B", "#56B4E9"))

  q <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_point() +
    scale_color_canhrActi(type = "intensity_named")
  got <- ggplot2::ggplot_build(q)$data[[1]]
  expect_identical(got$colour[order(got$x)], unname(canhrActi_colors(type = "intensity_named")[d$g]))
})

test_that("set_canhrActi_theme makes theme_canhrActi() the session default with the arguments given", {
  old <- ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(old))

  v <- withVisible(set_canhrActi_theme(base_size = 10, dark = TRUE))
  expect_identical(v$value, NULL)
  expect_identical(v$visible, FALSE)
  th <- ggplot2::theme_get()
  expect_identical(th, theme_canhrActi(base_size = 10, dark = TRUE))
  # dark background, base size 10, titles at the 1.43 heading step
  expect_identical(th$plot.background$fill, "#1E293B")
  expect_equal(th$text$size, 10)
  expect_equal(th$plot.title$size, 14.3)

  set_canhrActi_theme(grid = FALSE)
  th <- ggplot2::theme_get()
  expect_identical(th, theme_canhrActi(grid = FALSE))
  expect_identical(th$plot.background$fill, "#FFFFFF")
  expect_equal(th$plot.title$size, 14 * 1.43)
  expect_identical(th$panel.grid.major, ggplot2::element_blank())
})
