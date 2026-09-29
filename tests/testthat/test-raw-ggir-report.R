# raw.ggir.report() and ggir.report.tables() (R/raw_ggir_report.R). The early exits need no
# data. The MOS2 run needs GGIR and the folder named by CANHRACTI_GGIR_REF, and the csv files
# GGIR's report layer writes from our milestones must be the ones GGIR wrote in its own run.
# The recording, its nights and its time use are built once from the stored part-1 milestone
# (about 12 s); the report with its pdf takes about 6 s more.

withr::local_locale(c(LC_TIME = "C"))
# GGIR wrote the reference outputs in an America/Anchorage session
withr::local_timezone("America/Anchorage")

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed, so its report cannot run")
  }
}
ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)

# the six part-5 csv files of GGIR's MOS2 run, relative to results/
MOS2_PART5 <- c("part5_daysummary_MM_L40M100V400_T5A5.csv",
                "part5_daysummary_WW_L40M100V400_T5A5.csv",
                "part5_personsummary_MM_L40M100V400_T5A5.csv",
                "part5_personsummary_WW_L40M100V400_T5A5.csv",
                "QC/part5_daysummary_full_MM_L40M100V400_T5A5.csv",
                "QC/part5_daysummary_full_WW_L40M100V400_T5A5.csv")

# the recording, nights and time use the dashboard hands to raw.ggir.report(), built once
.report_cache <- new.env(parent = emptyenv())
report_inputs <- function() {
  skip_if_no_ggir_ref()
  basic <- ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
  if (!file.exists(basic)) testthat::skip(paste0("reference file not found: ", basic))
  if (is.null(.report_cache$x)) {
    x <- read.ggir.milestone(basic)
    x$imputed <- raw.impute(x$meta, x$wear, x$params)
    x$sleep <- raw.sleep.part3(x)
    nights <- raw.sleep.nights(x)
    .report_cache$tu <- suppressWarnings(raw.timeuse(x, nights = nights))
    .report_cache$nights <- nights
    .report_cache$x <- x
  }
  list(x = .report_cache$x, nights = .report_cache$nights, tu = .report_cache$tu)
}

test_that("ggir.report.tables sorts GGIR's result files into the three part-5 tables", {
  f <- c("part5_personsummary_WW_L40M100V400_T5A5.csv", "part5_daysummary_WW_L40M100V400_T5A5.csv",
         "part5_daysummary_full_MM_L40M100V400_T5A5.csv", "part4_nightsummary_sleep_cleaned.csv",
         "part5_daysummary_MM_L40M100V400_T5A5.csv", "part5_personsummary_MM_L40M100V400_T5A5.csv")
  tb <- ggir.report.tables(list(csv = stats::setNames(file.path("results", f), f)))
  expect_identical(names(tb), c("daysummary", "personsummary", "daysummary_full"))
  # the full day summary also starts part5_daysummary_ and must not land there
  expect_identical(names(tb$daysummary), c("part5_daysummary_MM_L40M100V400_T5A5.csv",
                                           "part5_daysummary_WW_L40M100V400_T5A5.csv"))
  expect_identical(unname(tb$daysummary), file.path("results", names(tb$daysummary)))
  expect_identical(names(tb$personsummary), c("part5_personsummary_MM_L40M100V400_T5A5.csv",
                                              "part5_personsummary_WW_L40M100V400_T5A5.csv"))
  expect_identical(names(tb$daysummary_full), "part5_daysummary_full_MM_L40M100V400_T5A5.csv")
  # no files, no tables
  expect_identical(ggir.report.tables(list(csv = character(0))), list())
  expect_identical(ggir.report.tables(list(state = "no_ggir")), list())
})

test_that("raw.ggir.report stops before writing when there is no time use", {
  skip_if_no_ggir()
  r <- raw.ggir.report(NULL, NULL, NULL)
  expect_identical(r$state, "no_timeuse")
  expect_identical(c(r$dir, r$pdf), c(NA_character_, NA_character_))
  expect_identical(r$csv, character(0))
  # a time use that failed passes its own state and messages on
  tu <- list(status = list(state = "no_valid_night", messages = "no night could be placed"))
  r2 <- raw.ggir.report(NULL, NULL, tu)
  expect_identical(r2$state, "no_valid_night")
  expect_identical(r2$messages, "no night could be placed")
  expect_identical(r2$dir, NA_character_)
})

test_that("a milestone tree that cannot be written is a state, not an error", {
  skip_if_no_ggir()
  d <- withr::local_tempdir()
  r <- raw.ggir.report(list(), NULL, list(status = list(state = "ok")), dir = d)
  expect_identical(r$state, "milestone_failed")
  expect_identical(r$dir, d)
  expect_match(r$messages, "part 5 needs timeuse", fixed = TRUE)
  expect_identical(r$csv, character(0))
  expect_identical(list.files(d, recursive = TRUE), character(0))
})

test_that("on MOS2 GGIR's report writes its own six part-5 csv files and its report pdf", {
  skip_if_no_ggir()
  inp <- report_inputs()
  d <- withr::local_tempdir()
  r <- raw.ggir.report(inp$x, inp$nights, inp$tu, dir = d)
  expect_identical(r$state, "ok")
  expect_identical(r$messages, character(0))
  expect_identical(r$dir, d)
  # the tree holds every milestone GGIR's report layer reads
  expect_true(all(dir.exists(file.path(d, "meta", c("basic", "ms2.out", "ms3.out", "ms4.out",
                                                    "ms5.out", "ms5.outraw")))))
  expect_setequal(names(r$csv), basename(MOS2_PART5))
  for (f in MOS2_PART5) {
    expect_identical(readLines(r$csv[[basename(f)]]),
                     readLines(ref_file("out", "output_din", "results", f)), info = f)
  }
  tb <- ggir.report.tables(r)
  expect_identical(lengths(tb), c(daysummary = 2L, personsummary = 2L, daysummary_full = 2L))
  # visualReport's pdf, as many pages as GGIR's own report
  expect_identical(basename(r$pdf), "report_MOS2E39230594_T5A5.pdf")
  expect_identical(readBin(r$pdf, "raw", 4), charToRaw("%PDF"))
  skip_if_not_installed("pdftools")
  own <- ref_file("out", "output_din", "results", "file summary reports",
                  "report_MOS2E39230594_T5A5.pdf")
  expect_identical(pdftools::pdf_info(r$pdf)$pages, pdftools::pdf_info(own)$pages)
})

test_that("named overrides reach GGIR's report and unknown names are dropped", {
  skip_if_no_ggir()
  inp <- report_inputs()
  d <- withr::local_tempdir()
  r <- raw.ggir.report(inp$x, inp$nights, inp$tu, dir = d, report = FALSE,
                       params_cleaning = list(segmentWEARcrit.part5 = 0.5),
                       params_output = list(week_weekend_aggregate.part5 = TRUE,
                                            not_a_ggir_setting = 1))
  expect_identical(r$state, "ok")
  expect_identical(r$pdf, NA_character_)
  # GGIR's own report over a copy of the same part-5 milestone with the same two overrides
  g <- withr::local_tempdir()
  dir.create(file.path(g, "meta", "ms5.out"), recursive = TRUE)
  dir.create(file.path(g, "results", "QC"), recursive = TRUE)
  file.copy(list.files(file.path(d, "meta", "ms5.out"), full.names = TRUE),
            file.path(g, "meta", "ms5.out"))
  p <- GGIR::load_params()
  p$params_cleaning$segmentWEARcrit.part5 <- 0.5
  p$params_output$week_weekend_aggregate.part5 <- TRUE
  suppressWarnings(GGIR:::g.report.part5(metadatadir = g, f0 = 1, f1 = 1,
                                         params_cleaning = p$params_cleaning,
                                         params_output = p$params_output, verbose = FALSE))
  own <- list.files(file.path(g, "results"), pattern = "[.]csv$", recursive = TRUE,
                    full.names = TRUE)
  expect_setequal(names(r$csv), basename(own))
  for (f in own) {
    expect_identical(readLines(r$csv[[basename(f)]]), readLines(f), info = basename(f))
  }
  # the weekday and weekend aggregates widen the person summary
  pm <- utils::read.csv(r$csv[["part5_personsummary_MM_L40M100V400_T5A5.csv"]],
                        check.names = FALSE)
  expect_true(all(c("sleeponset_WD", "sleeponset_WE") %in% names(pm)))
})
