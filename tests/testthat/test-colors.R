# Single-colour lookup and the console listing of the palettes (R/colors.R)

.palette_names <- c("intensity", "status", "sleep", "primary")

test_that("canhrActi_color returns the same hex as the palette that holds the name", {
  for (pal in .palette_names) {
    cols <- canhrActi_palette(pal)
    got <- vapply(names(cols), canhrActi_color, character(1), USE.NAMES = FALSE)
    expect_identical(got, unname(cols), info = pal)
  }
  # the six primary colours
  expect_identical(
    vapply(c("blue", "yellow", "orange", "cyan", "green", "pink"), canhrActi_color, character(1), USE.NAMES = FALSE),
    c("#236192", "#FFCD00", "#DF6A2E", "#87D1E6", "#71984A", "#F45197")
  )
})

test_that("canhrActi_color gives one unnamed hex string, and mvpa shares the moderate colour", {
  x <- canhrActi_color("moderate")
  expect_identical(x, "#FFB800")
  expect_identical(canhrActi_color("mvpa"), x)
  expect_identical(canhrActi_color("very_vigorous"), "#E6358B")
})

test_that("an unknown colour name stops and lists every name that exists", {
  msg <- tryCatch(canhrActi_color("mauve"), error = conditionMessage)
  expect_match(msg, "Color 'mauve' not found in canhrActi palette.", fixed = TRUE)
  known <- unique(unlist(lapply(.palette_names, function(p) names(canhrActi_palette(p)))))
  listed <- strsplit(sub(".*Available colors: ", "", msg), ", ", fixed = TRUE)[[1]]
  expect_setequal(listed, known)
  expect_error(canhrActi_color("Blue"), "not found")
})

test_that("show_canhrActi_colors prints one aligned line per colour and returns NULL invisibly", {
  out <- utils::capture.output(v <- withVisible(show_canhrActi_colors()))
  expect_identical(v$value, NULL)
  expect_identical(v$visible, FALSE)

  expected <- unlist(lapply(.palette_names, function(p) {
    cols <- canhrActi_palette(p)
    sprintf("  %-15s %s", names(cols), cols)
  }))
  colour_lines <- out[grepl("^  ", out)]
  expect_identical(colour_lines, expected)
  # a blank line and a header before each of the four palettes
  sizes <- vapply(.palette_names, function(p) length(canhrActi_palette(p)), integer(1))
  blank <- which(!nzchar(out))
  expect_identical(blank, unname(cumsum(c(1L, head(sizes + 2L, -1)))))
  expect_identical(startsWith(out[blank + 1L], toupper(.palette_names)), rep(TRUE, 4))
  expect_identical(length(out), length(expected) + 2L * length(.palette_names))
})

test_that("show_canhrActi_colors names each palette in a readable header", {
  out <- utils::capture.output(show_canhrActi_colors())
  expect_identical(out[grepl("Palette:", out, fixed = TRUE)],
                   c("INTENSITY Palette:", "STATUS Palette:", "SLEEP Palette:", "PRIMARY Palette:"))
})
