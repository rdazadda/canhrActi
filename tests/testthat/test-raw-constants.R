# Tests for R/raw_constants.R (MONITOR and FORMAT code tables) and for the
# GGIR attribution: the files shipped under inst/ and the header of every file with GGIR code.

ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF")
ggir_ref_ok <- nzchar(ggir_ref) && dir.exists(ggir_ref)
# The GGIR 3.3-9 source clone is expected beside the reference data folder, in ggir-src/GGIR.
ggir_clone <- if (ggir_ref_ok) {
  file.path(dirname(sub("[/\\\\]+$", "", ggir_ref)), "ggir-src", "GGIR")
} else {
  ""
}
ggir_clone_ok <- nzchar(ggir_clone) && file.exists(file.path(ggir_clone, "LICENSE"))
ggir_installed <- requireNamespace("GGIR", quietly = TRUE)

# Locate the package root whether run via load_all, test_file or R CMD check.
pkg_root <- local({
  cands <- c(testthat::test_path("..", ".."), system.file(package = "canhrActi"))
  cands <- cands[nzchar(cands)]
  hit <- cands[file.exists(file.path(cands, "inst", "LICENSE.GGIR")) |
                 file.exists(file.path(cands, "LICENSE.GGIR"))]
  if (length(hit)) normalizePath(hit[1], winslash = "/") else NA_character_
})
inst_file <- function(name) {
  a <- file.path(pkg_root, "inst", name)
  if (file.exists(a)) a else file.path(pkg_root, name)
}
# Every R/raw_*.R and R/circadian_ggir.R holds GGIR code but three: the .gt3x reader, written
# from ActiGraph's format description, the wrapper that runs GGIR's own report, and the cut
# point values parsed from GGIR's CutPoints vignette. An installed package has no R sources.
ported_files <- function() {
  if (is.na(pkg_root)) return(character(0))
  src <- list.files(file.path(pkg_root, "R"), pattern = "^(raw_.*|circadian_ggir)[.]R$")
  setdiff(src, c("raw_gt3x_stream.R", "raw_ggir_report.R", "raw_cutpoints.R"))
}

test_that(".RAW_MONITOR has GGIR's exact names and codes", {
  expect_true(is.integer(.RAW_MONITOR))
  expect_identical(
    .RAW_MONITOR,
    setNames(0:7, c("AD_HOC", "GENEA", "GENEACTIV", "ACTIGRAPH", "AXIVITY",
                    "MOVISENS", "VERISENSE", "PARMAY_MTX"))
  )
  expect_identical(unname(.RAW_MONITOR[["AD_HOC"]]), 0L)
  expect_identical(unname(.RAW_MONITOR[["GENEACTIV"]]), 2L)
  expect_identical(unname(.RAW_MONITOR[["ACTIGRAPH"]]), 3L)
  expect_identical(unname(.RAW_MONITOR[["AXIVITY"]]), 4L)
  expect_identical(unname(.RAW_MONITOR[["PARMAY_MTX"]]), 7L)
})

test_that(".RAW_FORMAT has GGIR's exact names and codes", {
  expect_true(is.integer(.RAW_FORMAT))
  expect_identical(
    .RAW_FORMAT,
    setNames(1:6, c("BIN", "CSV", "WAV", "CWA", "AD_HOC_CSV", "GT3X"))
  )
  expect_identical(unname(.RAW_FORMAT[["BIN"]]), 1L)
  expect_identical(unname(.RAW_FORMAT[["CSV"]]), 2L)
  expect_identical(unname(.RAW_FORMAT[["CWA"]]), 4L)
  expect_identical(unname(.RAW_FORMAT[["GT3X"]]), 6L)
})

test_that("code tables equal the installed GGIR MONITOR and FORMAT environments", {
  skip_if_not(ggir_installed, "GGIR is not installed")
  ggir_monitor <- get("MONITOR", envir = asNamespace("GGIR"))
  ggir_format <- get("FORMAT", envir = asNamespace("GGIR"))
  expect_true(is.environment(ggir_monitor))
  expect_true(is.environment(ggir_format))
  # Compare as sorted lists: environments carry no order.
  ours_m <- as.list(.RAW_MONITOR)
  ours_f <- as.list(.RAW_FORMAT)
  theirs_m <- as.list(ggir_monitor)
  theirs_f <- as.list(ggir_format)
  expect_identical(length(theirs_m), 8L)
  expect_identical(length(theirs_f), 6L)
  expect_identical(ours_m[sort(names(ours_m))], theirs_m[sort(names(theirs_m))])
  expect_identical(ours_f[sort(names(ours_f))], theirs_f[sort(names(theirs_f))])
  # Spot-check the codes the rest of the pipeline keys on.
  expect_identical(ggir_monitor$ACTIGRAPH, .RAW_MONITOR[["ACTIGRAPH"]])
  expect_identical(ggir_format$GT3X, .RAW_FORMAT[["GT3X"]])
})

test_that("code tables equal the clone's monitor_types.R when the clone is available", {
  skip_if_not(ggir_clone_ok, "GGIR source clone not available (set CANHRACTI_GGIR_REF)")
  src <- file.path(ggir_clone, "R", "monitor_types.R")
  expect_true(file.exists(src))
  e <- new.env()
  sys.source(src, envir = e)
  expect_identical(as.list(.RAW_MONITOR)[sort(names(.RAW_MONITOR))],
                   as.list(e$MONITOR)[sort(names(.RAW_MONITOR))])
  expect_identical(as.list(.RAW_FORMAT)[sort(names(.RAW_FORMAT))],
                   as.list(e$FORMAT)[sort(names(.RAW_FORMAT))])
})

test_that(".raw.monitor.name and .raw.format.name reverse the tables", {
  expect_identical(.raw.monitor.name(3), "ACTIGRAPH")
  expect_identical(.raw.monitor.name(3L), "ACTIGRAPH")
  expect_identical(.raw.monitor.name(0:7), names(.RAW_MONITOR))
  expect_identical(.raw.monitor.name(c(2, 4, 7)), c("GENEACTIV", "AXIVITY", "PARMAY_MTX"))
  expect_identical(.raw.monitor.name(8), NA_character_)
  expect_identical(.raw.monitor.name(-1), NA_character_)
  expect_identical(.raw.monitor.name(NA), NA_character_)
  expect_identical(.raw.monitor.name(3.5), NA_character_)
  expect_identical(.raw.monitor.name(integer(0)), character(0))
  expect_error(.raw.monitor.name("ACTIGRAPH"), "numeric")

  expect_identical(.raw.format.name(6), "GT3X")
  expect_identical(.raw.format.name(1:6), names(.RAW_FORMAT))
  expect_identical(.raw.format.name(0), NA_character_)
  expect_identical(.raw.format.name(7), NA_character_)
  expect_identical(.raw.format.name(c(1, NA, 4)), c("BIN", NA, "CWA"))
  # Round trip both ways for every code.
  expect_identical(unname(.RAW_MONITOR[.raw.monitor.name(.RAW_MONITOR)]), unname(.RAW_MONITOR))
  expect_identical(unname(.RAW_FORMAT[.raw.format.name(.RAW_FORMAT)]), unname(.RAW_FORMAT))
})

test_that("inst/LICENSE.GGIR is the Apache 2.0 text and byte-identical to the clone's LICENSE", {
  skip_if(is.na(pkg_root), "package root not found")
  lic <- inst_file("LICENSE.GGIR")
  expect_true(file.exists(lic))
  txt <- readLines(lic, warn = FALSE)
  expect_true(any(grepl("Apache License", txt, fixed = TRUE)))
  expect_true(any(grepl("Version 2.0, January 2004", txt, fixed = TRUE)))
  expect_true(any(grepl("TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION", txt, fixed = TRUE)))

  skip_if_not(ggir_clone_ok, "GGIR source clone not available (set CANHRACTI_GGIR_REF)")
  ref <- file.path(ggir_clone, "LICENSE")
  ours <- readBin(lic, "raw", n = file.size(lic))
  theirs <- readBin(ref, "raw", n = file.size(ref))
  expect_identical(length(ours), 11558L)
  expect_identical(length(ours), length(theirs))
  expect_identical(ours, theirs)
})

test_that("inst/COPYRIGHTS.raw names every ported file and every dependency", {
  skip_if(is.na(pkg_root), "package root not found")
  cp <- inst_file("COPYRIGHTS.raw")
  expect_true(file.exists(cp))
  txt <- paste(readLines(cp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ported <- ported_files()
  if (length(ported) == 0) {
    # the part 1-2 files stand in where there are no R sources to list
    ported <- c("raw_params.R", "raw_inspect.R", "raw_read_block.R", "raw_impute_timegaps.R",
                "raw_calibrate.R", "raw_metrics.R", "raw_nonwear.R", "raw_starttime.R",
                "raw_getmeta.R", "raw_weardec.R", "raw_quality.R", "raw_constants.R")
  }
  unnamed <- ported[!vapply(paste0("R/", ported), grepl, logical(1), x = txt, fixed = TRUE)]
  expect_identical(unnamed, character(0), info = toString(unnamed))
  for (s in c("GGIR", "3.3-9", "https://github.com/wadpac/GGIR/", "Apache License, Version 2.0",
              "Medical Research Council UK", "Accelting", "French National Research Agency",
              "Vincent T van Hees", "Jairo H Migueles", "Xiaoyu Zong",
              "GGIRread", "Evgeny Mirkes", "Dan Jackson",
              "read.gt3x", "EUPL", "Tuomo Nieminen",
              "actilifecounts", "LGPL", "Jairo Hidalgo Migueles",
              "not endorsed", "inst/LICENSE.GGIR")) {
    expect_true(grepl(s, txt, fixed = TRUE), info = s)
  }
  expect_false(grepl("\u2014", txt))
})

test_that("R/raw_constants.R carries the attribution header and no em-dashes", {
  skip_if(is.na(pkg_root), "package root not found")
  src <- file.path(pkg_root, "R", "raw_constants.R")
  skip_if_not(file.exists(src), "source tree not available (installed package)")
  txt <- readLines(src, warn = FALSE, encoding = "UTF-8")
  expect_match(txt[1], "^# Ported from GGIR 3\\.3-9 R/monitor_types\\.R")
  expect_true(any(grepl("Apache License, Version 2.0", txt, fixed = TRUE)))
  expect_true(any(grepl("MODIFIED version of the original", txt, fixed = TRUE)))
  expect_false(any(grepl("\u2014", txt)))
})

test_that("every file with GGIR code names its GGIR source and licence and has no em-dashes", {
  ported <- ported_files()
  skip_if(length(ported) == 0, "source tree not available (installed package)")
  txt <- lapply(file.path(pkg_root, "R", ported), readLines, warn = FALSE, encoding = "UTF-8")
  # the header is the run of plain comment and blank lines at the top
  heads <- vapply(txt, function(x) {
    end <- which(!grepl("^(#([^']|$)|\\s*$)", x))[1]
    paste(x[seq_len(if (is.na(end)) length(x) else end - 1)], collapse = "\n")
  }, character(1))
  emdash <- vapply(txt, function(x) any(grepl("\u2014", x, fixed = TRUE)), logical(1))
  bad <- c(sprintf("%s: no GGIR version and file",
                   ported[!grepl("GGIR [0-9]+[.][0-9]+-[0-9]+ R/[A-Za-z0-9_.]+[.]R", heads)]),
           sprintf("%s: no licence", ported[!grepl("Apache(-| License, Version )2\\.0", heads)]),
           sprintf("%s: no inst/LICENSE.GGIR",
                   ported[!grepl("inst/LICENSE.GGIR", heads, fixed = TRUE)]),
           sprintf("%s: em-dash", ported[emdash]))
  expect_identical(bad, character(0), info = toString(bad))
})
