# Parity tests for raw.sleep.report() and write.raw.sleep.report() against GGIR's
# g.report.part4. The gate is byte equality of the four csv files, against the stored
# reference and fixture csvs and against a live g.report.part4 on the same input; the frames
# the port returns are unrounded. Reference data are found through CANHRACTI_GGIR_REF and
# the fixtures under <ref>/../ggir-study-p34/fixtures; every block skips without them, and
# the live comparisons skip when GGIR is not installed. The MOS2 milestones were stored in
# the system zone of a machine set to America/Anchorage, so the file runs in that zone.

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
skip_if_no_dir <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    testthat::skip(paste0("reference folder not found: ", path))
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
p34_fixture <- function(...) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", ...)
}

REP_CSVS <- c("results/QC/part4_nightsummary_sleep_full.csv",
              "results/QC/part4_summary_sleep_full.csv",
              "results/part4_nightsummary_sleep_cleaned.csv",
              "results/part4_summary_sleep_cleaned.csv")

MOS2_OUT <- function() ref_file("out", "output_din")
EE_OUT <- function() ref_file("timing_out", "output_timing")

# Every object of one ms4 milestone, in the order the folder lists them
rep_ms4 <- function(src) {
  fs <- list.files(file.path(src, "meta/ms4.out"), full.names = TRUE)
  lapply(fs, function(f) {
    e <- new.env(parent = emptyenv())
    load(f, envir = e)
    as.list(e)
  })
}
# The part-5 window tables of one output folder, or NULL when part 5 never ran
rep_ms5 <- function(src) {
  d <- file.path(src, "meta/ms5.out")
  if (!dir.exists(d)) return(NULL)
  fs <- list.files(d, full.names = TRUE)
  if (length(fs) == 0) return(NULL)
  lapply(fs, function(f) {
    e <- new.env(parent = emptyenv())
    load(f, envir = e)
    e$output
  })
}

rep_run <- function(src, part5 = "auto", params = NULL, ...) {
  objs <- rep_ms4(src)
  if (identical(part5, "auto")) part5 <- rep_ms5(src)
  if (is.null(params)) params <- raw.params(...)
  suppressWarnings(raw.sleep.report(
    nights = lapply(objs, function(o) o$nightsummary),
    params = params, part5 = part5,
    tail_expansion_log = lapply(objs, function(o) o$tail_expansion_log)))
}

rep_write <- function(x) {
  d <- file.path(tempdir(), paste0("canhract_rep_", basename(tempfile(""))))
  dir.create(file.path(d, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  write.raw.sleep.report(x, d)
  d
}

# Compare the four csvs of two folders, reporting the first differing line of each
expect_same_csvs <- function(ours, theirs, label = "") {
  for (f in REP_CSVS) {
    a_ex <- file.exists(file.path(ours, f))
    b_ex <- file.exists(file.path(theirs, f))
    testthat::expect_identical(a_ex, b_ex,
                               info = paste(label, basename(f), "written by one side only"))
    if (!a_ex || !b_ex) next
    a <- readLines(file.path(ours, f))
    b <- readLines(file.path(theirs, f))
    if (!identical(a, b)) {
      for (i in seq_len(max(length(a), length(b)))) {
        if (!identical(a[i], b[i])) {
          x1 <- strsplit(a[i], ",")[[1]]
          x2 <- strsplit(b[i], ",")[[1]]
          n <- min(length(x1), length(x2))
          dd <- if (n > 0) which(x1[seq_len(n)] != x2[seq_len(n)]) else integer(0)
          cat("\n", label, " ", basename(f), " first differing line ", i,
              " (", length(x1), " vs ", length(x2), " fields)\n", sep = "")
          for (k in utils::head(dd, 8)) {
            cat("   field ", k, ": ours=", x1[k], " ggir=", x2[k], "\n", sep = "")
          }
          break
        }
      }
    }
    testthat::expect_identical(a, b, info = paste(label, basename(f)))
  }
  invisible(TRUE)
}

# A private metadatadir that GGIR:::g.report.part4 can be pointed at
rep_ggir_dir <- function(ms4, ms5 = NULL) {
  d <- file.path(tempdir(), paste0("canhract_ggir_", basename(tempfile(""))))
  for (sub in c("meta/ms3.out", "meta/ms4.out", "results/QC")) {
    dir.create(file.path(d, sub), recursive = TRUE, showWarnings = FALSE)
  }
  for (nm in names(ms4)) {
    obj <- ms4[[nm]]
    nightsummary <- obj$nightsummary
    tail_expansion_log <- obj$tail_expansion_log
    GGIRversion <- obj$GGIRversion
    save(nightsummary, tail_expansion_log, GGIRversion,
         file = file.path(d, "meta/ms4.out", paste0(nm, ".RData")))
    # only the length of the ms3 listing is read, to cap f1
    saveRDS(1, file.path(d, "meta/ms3.out", paste0(nm, ".RData")))
  }
  if (!is.null(ms5)) {
    dir.create(file.path(d, "meta/ms5.out"), recursive = TRUE, showWarnings = FALSE)
    for (nm in names(ms5)) {
      output <- ms5[[nm]]
      save(output, file = file.path(d, "meta/ms5.out", paste0(nm, ".RData")))
    }
  }
  d
}

rep_ggir_report <- function(d, sleep = list(), output = list(), data_cleaning_file = c()) {
  lp <- GGIR::load_params()
  ps <- lp$params_sleep
  po <- lp$params_output
  for (nm in names(sleep)) ps[[nm]] <- sleep[[nm]]
  for (nm in names(output)) po[[nm]] <- output[[nm]]
  suppressWarnings(GGIR:::g.report.part4(datadir = c(), metadatadir = d, f0 = 1, f1 = 1,
                                         data_cleaning_file = data_cleaning_file,
                                         params_sleep = ps, params_output = po,
                                         verbose = FALSE))
  d
}

# The same ms4 (and ms5) objects through GGIR and through the port
expect_matches_ggir <- function(label, ms4, ms5 = NULL, sleep = list(), output = list(),
                                data_cleaning_file = c()) {
  g <- rep_ggir_dir(ms4, ms5)
  rep_ggir_report(g, sleep = sleep, output = output,
                  data_cleaning_file = data_cleaning_file)
  pargs <- c(sleep, output,
             if (length(data_cleaning_file)) list(data_cleaning_file = data_cleaning_file))
  r <- suppressWarnings(raw.sleep.report(
    nights = unname(lapply(ms4, function(o) o$nightsummary)),
    params = do.call(raw.params, pargs),
    part5 = if (is.null(ms5)) NULL else unname(ms5),
    tail_expansion_log = unname(lapply(ms4, function(o) o$tail_expansion_log))))
  expect_same_csvs(rep_write(r), g, label)
  r
}

chr <- function(x) as.character(x)
num <- function(x) as.numeric(as.character(x))

test_that("R1: MOS2 with part5 gives 4x42, 2x39, 1x119 and 1x95", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  expect_s3_class(r, "canhrActi_raw_sleep_report")
  expect_identical(names(r), c("nightsummary_full", "nightsummary_cleaned",
                               "personsummary_full", "personsummary_cleaned"))
  expect_identical(dim(r$nightsummary_full), c(4L, 42L))
  expect_identical(dim(r$nightsummary_cleaned), c(2L, 39L))
  expect_identical(dim(r$personsummary_full), c(1L, 119L))
  expect_identical(dim(r$personsummary_cleaned), c(1L, 95L))
  expect_identical(tail(names(r$nightsummary_full), 3),
                   c("window", "nonwear_perc_spt", "ACC_spt_mg"))
  expect_identical(setdiff(names(r$nightsummary_full), names(r$nightsummary_cleaned)),
                   c("error_onset", "error_wake", "error_dur"))
  extra <- setdiff(names(r$personsummary_full), names(r$personsummary_cleaned))
  expect_length(extra, 24L)
  expect_identical(sort(extra), sort(c(
    as.vector(outer(c("guider_SptDuration", "guider_onset", "guider_wakeup"),
                    c("AD", "WD", "WE"), paste, sep = "_")) |>
      (function(v) as.vector(outer(v, c("mn", "sd"), paste, sep = "_")))(),
    paste0("nonwear_perc_spt_", c("AD", "WD", "WE"), "_mn"),
    paste0("ACC_spt_mg_", c("AD", "WD", "WE"), "_mn"))))
  # 95 = 12 general + 3 windows x 27 + N_nights_guider_corrected + GGIRversion
  expect_identical(12L + 3L * 27L + 1L + 1L, ncol(r$personsummary_cleaned))
  expect_identical(95L + 24L, ncol(r$personsummary_full))
})

test_that("R1: the nights dropped by the cleaned pass are 1 and 4, both cleaningcode 2", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  expect_identical(r$nightsummary_full$night, c(1, 2, 3, 4))
  expect_identical(r$nightsummary_full$cleaningcode, c(2, 1, 1, 2))
  expect_identical(r$nightsummary_cleaned$night, c(2, 3))
  expect_identical(setdiff(r$nightsummary_full$night, r$nightsummary_cleaned$night), c(1, 4))
  # the rule is cleaningcode > 1, with no diary; no night carries a NotWorn guider here
  expect_identical(unique(r$nightsummary_full$guider), "HDCZA")
  # part 5 reported nights 2 to 4, so night 1 has no window
  expect_identical(r$nightsummary_full$window, c(NA, "WW", "WW", "WW"))
  expect_identical(r$nightsummary_full$nonwear_perc_spt, c(NA, 0, 0, 0))
  expect_equal(r$nightsummary_full$ACC_spt_mg,
               c(NA, 1.90684839306747, 2.79535376903798, 0.24324942791762), tolerance = 1e-12)
})

test_that("R2: EE without part5 gives 6x39, 6x36, 1x113 and 1x95, and 113 is 119 minus 6", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(EE_OUT())
  r <- rep_run(EE_OUT(), part5 = NULL)
  expect_identical(dim(r$nightsummary_full), c(6L, 39L))
  expect_identical(dim(r$nightsummary_cleaned), c(6L, 36L))
  expect_identical(dim(r$personsummary_full), c(1L, 113L))
  expect_identical(dim(r$personsummary_cleaned), c(1L, 95L))
  expect_false("window" %in% names(r$nightsummary_full))
  expect_false(any(grepl("nonwear_perc_spt|ACC_spt_mg", names(r$personsummary_full))))
  expect_identical(r$nightsummary_full$cleaningcode, rep(1, 6))
  expect_identical(r$nightsummary_cleaned$night, c(1, 2, 3, 4, 5, 6))
  r5 <- rep_run(EE_OUT())
  expect_identical(dim(r5$nightsummary_full), c(6L, 42L))
  expect_identical(dim(r5$personsummary_full), c(1L, 119L))
  expect_identical(ncol(r5$personsummary_full) - ncol(r$personsummary_full), 6L)
})

test_that("R3: the four MOS2 csv files are byte-identical to the stored ones", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  expect_same_csvs(rep_write(r), MOS2_OUT(), "MOS2")
})

test_that("R3: the four EE csv files are byte-identical to the stored ones", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(EE_OUT())
  # EE's stored reports were written before part 5 ran, so no part5 is supplied
  r <- rep_run(EE_OUT(), part5 = NULL)
  expect_same_csvs(rep_write(r), EE_OUT(), "EE")
})

test_that("R4: the MOS2 cleaned person row reproduces every quoted number", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  p <- rep_run(MOS2_OUT())$personsummary_cleaned
  expect_identical(nrow(p), 1L)
  expect_identical(chr(p$ID), "MOS2E39230594.gt3x")
  expect_identical(chr(p$filename), "MOS2E39230594.gt3x")
  expect_identical(chr(p$sleeplog_used), "0")
  expect_identical(chr(p$sleeplog_ID), "")
  expect_identical(chr(p$n_nights_acc), "2")
  expect_identical(chr(p$n_nights_sleeplog), "0")
  expect_identical(chr(p$n_WE_nights_complete), "0")
  expect_identical(chr(p$n_WD_nights_complete), "2")
  expect_identical(chr(p$n_WEnights_daysleeper), "0")
  expect_identical(chr(p$n_WDnights_daysleeper), "0")
  # the two surviving nights, at the seven significant digits the character conversion leaves
  expect_identical(num(p$SptDuration_AD_T5A5_mn), mean(c(8.254167, 7.204167)))
  expect_identical(round(num(p$SptDuration_AD_T5A5_mn), 3), 7.729)
  # the cell is text with 15 significant digits, so the sd loses its last bit on the round trip
  expect_equal(num(p$SptDuration_AD_T5A5_sd), stats::sd(c(8.254167, 7.204167)),
               tolerance = 1e-14)
  expect_identical(round(num(p$SptDuration_AD_T5A5_sd), 3), 0.742)
  expect_identical(num(p$WASO_AD_T5A5_mn), mean(c(0.3583333, 0.4833333)))
  expect_identical(round(num(p$WASO_AD_T5A5_mn), 3), 0.421)
  # the mean of per-night ratios, not total over total
  expect_equal(num(p$average_dur_sib_wakinghours_AD_T5A5_mn),
               mean(c(4.4791667 / 25, 3.4166667 / 21)), tolerance = 1e-14)
  expect_identical(round(num(p$average_dur_sib_wakinghours_AD_T5A5_mn), 3), 0.171)
  expect_false(isTRUE(all.equal(num(p$average_dur_sib_wakinghours_AD_T5A5_mn),
                                (4.4791667 + 3.4166667) / (25 + 21))))
  expect_identical(chr(p$n_days_w_sib_wakinghours_AD_T5A5), "2")
  expect_identical(num(p$SleepRegularityIndex1_AD_T5A5_mn), mean(c(30.256, 46.738)))
  expect_identical(round(num(p$SleepRegularityIndex1_AD_T5A5_mn), 3), 38.497)
  expect_identical(num(p$SriFractionValid_AD_T5A5_mn), mean(c(0.9479, 0.9688)))
  expect_identical(round(num(p$SriFractionValid_AD_T5A5_mn), 3), 0.958)
  expect_identical(chr(p$N_nights_guider_corrected), "0")
  # both surviving nights are Wednesday and Thursday, so WD equals AD throughout
  wd <- names(p)[grepl("_WD_T5A5", names(p))]
  ad <- sub("_WD_T5A5", "_AD_T5A5", wd)
  expect_length(wd, 27L)
  expect_true(all(ad %in% names(p)))
  expect_identical(unname(vapply(p[wd], chr, character(1))),
                   unname(vapply(p[ad], chr, character(1))))
  # no weekend night survived, so the WE block is blank except the count, a length() and so 0
  we <- setdiff(names(p)[grepl("_WE_T5A5", names(p))], "n_days_w_sib_wakinghours_WE_T5A5")
  expect_true(all(vapply(p[we], chr, character(1)) == ""))
  expect_identical(chr(p$n_days_w_sib_wakinghours_WE_T5A5), "0")
})

test_that("R5: the full report averages the invalid nights too, at seven significant digits", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  full <- num(r$personsummary_full$SptDuration_AD_T5A5_mn)
  clean <- num(r$personsummary_cleaned$SptDuration_AD_T5A5_mn)
  expect_identical(full, mean(c(8.193056, 8.254167, 7.204167, 1.213889)))
  expect_identical(full, 6.21631975)
  expect_identical(round(full, 3), 6.216)
  expect_identical(round(clean, 3), 7.729)
  # the two nights with cleaningcode 2 are in the full average
  expect_identical(chr(r$personsummary_full$n_days_w_sib_wakinghours_AD_T5A5), "4")
  expect_identical(chr(r$personsummary_cleaned$n_days_w_sib_wakinghours_AD_T5A5), "2")
  # as.matrix() renders every numeric column through format(), at seven significant digits
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  stored <- e$nightsummary$SptDuration
  expect_false(identical(full, mean(stored)))
  expect_equal(full - mean(stored), 3.0555e-07, tolerance = 1e-10)
  expect_identical(r$nightsummary_full$SptDuration,
                   c("8.193056", "8.254167", "7.204167", "1.213889"))
})

test_that("R6: the person row dates come from the first SURVIVING night", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  expect_identical(chr(r$personsummary_full$calendar_date), "2025-10-07")
  expect_identical(chr(r$personsummary_full$weekday), "Tuesday")
  expect_identical(chr(r$personsummary_cleaned$calendar_date), "2025-10-08")
  expect_identical(chr(r$personsummary_cleaned$weekday), "Wednesday")
  # the stored night table holds d/m/Y with no zero padding; the report rewrites it
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  expect_identical(e$nightsummary$calendar_date,
                   c("7/10/2025", "8/10/2025", "9/10/2025", "10/10/2025"))
  expect_identical(r$nightsummary_full$calendar_date,
                   c("2025-10-07", "2025-10-08", "2025-10-09", "2025-10-10"))
})

test_that("R7: Friday and Saturday are weekend nights and Sunday is a weekday night", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  ns <- e$nightsummary
  ns$weekday <- c("Thursday", "Friday", "Saturday", "Sunday")
  ns$cleaningcode <- 1
  p <- suppressWarnings(raw.sleep.report(ns, params = raw.params()))$personsummary_cleaned
  expect_identical(chr(p$n_WE_nights_complete), "2")
  expect_identical(chr(p$n_WD_nights_complete), "2")
  # nights 2 and 3 are the weekend ones, nights 1 and 4 the weekday ones
  expect_identical(num(p$SptDuration_WE_T5A5_mn), mean(c(8.254167, 7.204167)))
  expect_identical(num(p$SptDuration_WD_T5A5_mn), mean(c(8.193056, 1.213889)))
  expect_identical(num(p$SptDuration_AD_T5A5_mn),
                   mean(c(8.193056, 8.254167, 7.204167, 1.213889)))
  ns$daysleeper <- c(0, 1, 1, 0)
  p2 <- suppressWarnings(raw.sleep.report(ns, params = raw.params()))$personsummary_cleaned
  expect_identical(chr(p2$n_WEnights_daysleeper), "2")
  expect_identical(chr(p2$n_WDnights_daysleeper), "0")
  wk <- ns[rep(1, 7), ]
  wk$night <- 1:7
  wk$weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  wk$cleaningcode <- 1
  p3 <- suppressWarnings(raw.sleep.report(wk, params = raw.params()))$personsummary_cleaned
  expect_identical(chr(p3$n_WE_nights_complete), "2")
  expect_identical(chr(p3$n_WD_nights_complete), "5")
})

test_that("R8: N_nights_guider_corrected is emitted once, between the AD and WD blocks", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  for (p in list(r$personsummary_full, r$personsummary_cleaned)) {
    idx <- which(names(p) == "N_nights_guider_corrected")
    expect_length(idx, 1L)
    expect_identical(names(p)[idx - 1], "SriFractionValid_AD_T5A5_sd")
    expect_identical(names(p)[idx + 1], "SptDuration_WD_T5A5_mn")
  }
  expect_identical(which(names(r$personsummary_cleaned) == "N_nights_guider_corrected"), 40L)
  # the value is sum(as.numeric(guider_corrected), na.rm = TRUE), so an all-NA column is 0
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  expect_true(all(is.na(e$nightsummary$guider_corrected)))
  expect_identical(chr(r$personsummary_cleaned$N_nights_guider_corrected), "0")
  ns <- e$nightsummary
  ns$guider_corrected <- c(1, NA, 1, 0)
  p <- suppressWarnings(raw.sleep.report(ns, params = raw.params()))$personsummary_full
  expect_identical(chr(p$N_nights_guider_corrected), "2")
  expect_length(which(names(p) == "N_nights_guider_corrected"), 1L)
})

test_that("R9: without a diary the cleaned pass deletes cleaningcode > 1 and the two NotWorn labels", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  ns <- e$nightsummary
  ns$cleaningcode <- c(0, 1, 2, 1)
  ns$guider <- c("HDCZA", "NotWorn", "HDCZA", "NotWorn+invalid")
  r <- suppressWarnings(raw.sleep.report(ns, params = raw.params()))
  expect_identical(r$nightsummary_cleaned$night, 1)
  # validcleaningcode is 1 without a diary, so night 2's code 1 still counts as complete
  ns2 <- e$nightsummary
  ns2$cleaningcode <- c(0, 1, 2, 3)
  r2 <- suppressWarnings(raw.sleep.report(ns2, params = raw.params()))
  expect_identical(r2$nightsummary_cleaned$night, c(1, 2))
  expect_identical(chr(r2$personsummary_cleaned$n_WD_nights_complete), "2")
})

test_that("R9: with a diary the cleaned pass deletes every cleaningcode above 0", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  slog <- p34_fixture("diary", "slog.csv")
  if (!file.exists(slog)) skip("the diary fixture is absent")
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  ns <- e$nightsummary
  ns$cleaningcode <- c(0, 1, 0, 2)
  ns$guider <- "sleeplog"
  ns$sleeplog_used <- 1
  r <- suppressWarnings(raw.sleep.report(ns, params = raw.params(loglocation = slog)))
  expect_identical(r$nightsummary_cleaned$night, c(1, 3))
  # validcleaningcode moves to 0 as well
  expect_identical(chr(r$personsummary_cleaned$n_WD_nights_complete), "2")
  expect_identical(chr(r$personsummary_cleaned$n_nights_sleeplog), "2")
  # the sleeplog_used == "FALSE" arm never matches a 0/1 column; GGIR does the same
  ns$sleeplog_used <- 0
  r2 <- suppressWarnings(raw.sleep.report(ns, params = raw.params(loglocation = slog)))
  expect_identical(r2$nightsummary_cleaned$night, c(1, 3))
})

test_that("R9: a del vector covering every row gives a zero-row frame and a message, not an error", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  ns <- e$nightsummary
  ns$cleaningcode <- 2
  r <- suppressWarnings(raw.sleep.report(ns, params = raw.params()))
  expect_identical(nrow(r$nightsummary_full), 4L)
  expect_identical(nrow(r$nightsummary_cleaned), 0L)
  expect_identical(nrow(r$personsummary_full), 1L)
  expect_identical(nrow(r$personsummary_cleaned), 0L)
  expect_true(any(grepl("cleaned report not stored",
                        attr(r, "canhrActi")$status$messages)))
  expect_identical(attr(r, "canhrActi")$status$state, "ok")
})

test_that("R9: the novalid_keepsibs fixture reproduces its stored full pair and writes no cleaned pair", {
  skip_if_no_ggir_ref()
  src <- p34_fixture("novalid_keepsibs", "output_din")
  skip_if_no_dir(src)
  r <- rep_run(src)
  expect_identical(dim(r$nightsummary_full), c(7L, 42L))
  expect_identical(r$nightsummary_full$cleaningcode, rep(2, 7))
  expect_identical(unique(r$nightsummary_full$guider), "L512")
  expect_identical(nrow(r$nightsummary_cleaned), 0L)
  d <- rep_write(r)
  expect_true(file.exists(file.path(d, REP_CSVS[1])))
  expect_false(file.exists(file.path(d, REP_CSVS[3])))
  expect_same_csvs(d, src, "novalid_keepsibs")
})

test_that("two recordings give two person rows and GGIR's duplicated row names", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  skip_if_no_dir(EE_OUT())
  a <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  b <- rep_ms4(EE_OUT())[[1]]$nightsummary
  r <- suppressWarnings(raw.sleep.report(list(a, b), params = raw.params()))
  expect_identical(dim(r$nightsummary_full), c(10L, 39L))
  expect_identical(dim(r$nightsummary_cleaned), c(8L, 36L))
  expect_identical(dim(r$personsummary_full), c(2L, 113L))
  expect_identical(dim(r$personsummary_cleaned), c(2L, 95L))
  p <- r$personsummary_cleaned
  expect_identical(chr(p$ID), c("MOS2E39230594.gt3x", "EE_left_29.5.2017-05-30.gt3x"))
  expect_identical(chr(p$n_nights_acc), c("2", "6"))
  expect_identical(chr(p$n_WE_nights_complete), c("0", "2"))
  expect_identical(chr(p$calendar_date), c("2025-10-08", "2017-05-23"))
  # as.matrix keeps each table's row names, and as.data.frame makes the duplicates unique
  expect_identical(utils::head(row.names(r$nightsummary_full), 6),
                   c("X1", "X2", "X3", "X4", "X1.1", "X2.1"))
  expect_identical(r$nightsummary_full$night, c(1, 2, 3, 4, 1, 2, 3, 4, 5, 6))
  r2 <- suppressWarnings(raw.sleep.report(list(a, a), params = raw.params()))
  expect_identical(nrow(r2$personsummary_cleaned), 1L)
})

test_that("a cohort matches live GGIR when one recording has no surviving cleaned night", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_OUT())
  ns <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  b <- ns
  b$ID <- "SECOND.gt3x"
  b$filename <- "SECOND.gt3x.RData"
  b$weekday <- c("Friday", "Saturday", "Sunday", "Monday")
  b$SptDuration <- b$SptDuration + 1
  cc <- ns
  cc$ID <- "THIRD.gt3x"
  cc$filename <- "THIRD.gt3x.RData"
  cc$cleaningcode <- 2
  ms4 <- list(MOS2E39230594.gt3x = list(nightsummary = ns, tail_expansion_log = NULL,
                                        GGIRversion = "3.3.6"),
              SECOND.gt3x = list(nightsummary = b, tail_expansion_log = NULL,
                                 GGIRversion = "3.3.6"),
              THIRD.gt3x = list(nightsummary = cc, tail_expansion_log = NULL,
                                GGIRversion = "3.3.6"))
  r <- expect_matches_ggir("three recordings, one emptied by cleaning", ms4)
  expect_identical(nrow(r$personsummary_full), 3L)
  # the third recording loses every night to the cleaned rule, and so its person row too
  expect_identical(nrow(r$personsummary_cleaned), 2L)
  expect_identical(nrow(r$nightsummary_cleaned), 4L)
  expect_false("THIRD.gt3x" %in% chr(r$personsummary_cleaned$ID))
  expect_true("THIRD.gt3x" %in% chr(r$personsummary_full$ID))
})

test_that("an empty cohort returns four empty frames and a state, not an error", {
  r <- raw.sleep.report(list(), params = raw.params())
  expect_s3_class(r, "canhrActi_raw_sleep_report")
  expect_identical(attr(r, "canhrActi")$status$state, "no_nights")
  for (nm in names(r)) expect_identical(nrow(r[[nm]]), 0L)
  expect_true(any(grepl("No night tables", attr(r, "canhrActi")$status$messages)))
})

test_that("a NULL element of the cohort is skipped with a warning (F6)", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  a <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  expect_warning(r <- raw.sleep.report(list(a, NULL), params = raw.params()),
                 "element 2 is NULL")
  expect_identical(nrow(r$nightsummary_full), 4L)
  expect_identical(attr(r, "canhrActi")$n_recordings, 1L)
  expect_warning(r2 <- raw.sleep.report(list(NULL), params = raw.params()), "is NULL")
  expect_identical(attr(r2, "canhrActi")$status$state, "no_nights")
})

test_that("an all-invalid recording produces no report at all, exactly as GGIR writes none", {
  skip_if_no_ggir_ref()
  src <- p34_fixture("novalid", "output_din")
  skip_if_no_dir(src)
  # GGIR writes no ms4 milestone here, so the port gets the zero-row night table instead
  basic <- list.files(file.path(src, "meta/basic"), full.names = TRUE)[1]
  ms <- read.ggir.milestone(basic)
  p3 <- raw.sleep.part3(ms, params = raw.params())
  nt <- raw.sleep.nights(p3, params = raw.params())
  expect_identical(nrow(nt), 0L)
  expect_warning(r <- raw.sleep.report(nt, params = raw.params()),
                 "no sleep estimates present")
  expect_identical(attr(r, "canhrActi")$status$state, "no_estimates")
  for (nm in names(r)) expect_identical(nrow(r[[nm]]), 0L)
  d <- rep_write(r)
  expect_length(list.files(d, recursive = TRUE), 0L)
  expect_length(list.files(file.path(src, "results"), pattern = "part4"), 0L)
})

test_that("the nonights fixture, one row of NA, aborts the report the way GGIR does", {
  skip_if_no_ggir_ref()
  src <- p34_fixture("nonights", "output_din")
  skip_if_no_dir(src)
  ns <- rep_ms4(src)[[1]]$nightsummary
  expect_identical(dim(ns), c(1L, 39L))
  expect_identical(ns$night, "0")           # character, because c(accid, 0) coerces
  expect_identical(ns$cleaningcode, 4)
  expect_warning(r <- raw.sleep.report(ns, params = raw.params()),
                 "no sleep estimates present")
  expect_identical(attr(r, "canhrActi")$status$state, "no_estimates")
  d <- rep_write(r)
  expect_length(list.files(d, recursive = TRUE), 0L)
  expect_false(dir.exists(file.path(src, "results", "QC")) &&
                 length(list.files(file.path(src, "results", "QC"), pattern = "part4")) > 0)
})

test_that("the daylight saving and failure-mode fixtures reproduce their stored csv files", {
  skip_if_no_ggir_ref()
  for (nm in c("spring", "autumn", "autumn_edge", "daysleeper", "keepsibs")) {
    src <- p34_fixture(nm, "output_din")
    if (!dir.exists(src)) next
    r <- rep_run(src)
    expect_same_csvs(rep_write(r), src, nm)
  }
  # the two DST fixtures differ where the clock change lands
  sp <- p34_fixture("spring", "output_din")
  au <- p34_fixture("autumn", "output_din")
  if (dir.exists(sp) && dir.exists(au)) {
    rs <- rep_run(sp)
    ra <- rep_run(au)
    expect_identical(rs$nightsummary_full$calendar_date[5], "2017-03-25")
    expect_identical(ra$nightsummary_full$calendar_date[5], "2017-10-28")
    expect_identical(rs$nightsummary_full$SptDuration[5], "8.427778")
    expect_identical(ra$nightsummary_full$SptDuration[5], "8.427778")
    expect_identical(rs$nightsummary_full$wakeup[5], "33.71667")
    expect_identical(ra$nightsummary_full$wakeup[5], "31.71667")
  }
})

test_that("the daysleeper fixture keeps its day sleepers through the person row", {
  skip_if_no_ggir_ref()
  src <- p34_fixture("daysleeper", "output_din")
  skip_if_no_dir(src)
  r <- rep_run(src)
  expect_identical(r$nightsummary_full$daysleeper, c("1", "1", "1", "0"))
  p <- r$personsummary_full
  expect_identical(num(p$n_WEnights_daysleeper) + num(p$n_WDnights_daysleeper), 3)
  expect_same_csvs(rep_write(r), src, "daysleeper")
})

test_that("the nine diary fixtures reproduce their stored csv files", {
  skip_if_no_ggir_ref()
  dl <- p34_fixture("diary")
  skip_if_no_dir(dl)
  slog <- file.path(dl, "slog.csv")
  slog2 <- file.path(dl, "slog2.csv")
  alog <- file.path(dl, "alog.csv")
  runs <- list(
    list(n = "spt", s = list(loglocation = slog)),
    list(n = "tib", s = list(loglocation = slog, sleepwindowType = "TimeInBed")),
    list(n = "nodiary", s = list()),
    list(n = "adv", s = list(loglocation = alog)),
    list(n = "spt_rely", s = list(loglocation = slog, relyonguider = TRUE)),
    list(n = "tib_TT", s = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                sib_must_fully_overlap_with_TimeInBed = c(TRUE, TRUE))),
    list(n = "tib_FF", s = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                sib_must_fully_overlap_with_TimeInBed = c(FALSE, FALSE))),
    list(n = "tib_FT", s = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                sib_must_fully_overlap_with_TimeInBed = c(FALSE, TRUE))),
    list(n = "tib_TF", s = list(loglocation = slog2, sleepwindowType = "TimeInBed",
                                sib_must_fully_overlap_with_TimeInBed = c(TRUE, FALSE))))
  for (rn in runs) {
    src <- file.path(dl, rn$n, "output_din")
    if (!dir.exists(src)) next
    r <- rep_run(src, params = do.call(raw.params, rn$s))
    expect_same_csvs(rep_write(r), src, paste0("diary/", rn$n))
  }
})

test_that("TimeInBed renames the three guider columns and adds the in-bed person block", {
  skip_if_no_ggir_ref()
  src <- p34_fixture("diary", "tib", "output_din")
  skip_if_no_dir(src)
  slog <- p34_fixture("diary", "slog.csv")
  r <- rep_run(src, params = raw.params(loglocation = slog, sleepwindowType = "TimeInBed"))
  expect_true(all(c("guider_inbedStart", "guider_inbedEnd", "guider_inbedDuration") %in%
                    names(r$nightsummary_full)))
  expect_false(any(c("guider_onset", "guider_wakeup", "guider_SptDuration") %in%
                     names(r$nightsummary_full)))
  expect_true(all(c("sleeplatency", "sleepefficiency") %in% names(r$nightsummary_full)))
  p <- r$personsummary_cleaned
  for (tw in c("AD", "WD", "WE")) {
    for (v in c("sleep_efficiency", "sleeplatency", "guider_inbedStart", "guider_inbedEnd",
                "guider_inbedDuration")) {
      expect_true(paste0(v, "_", tw, "_T5A5_mn") %in% names(p))
    }
  }
  expect_false("N_nights_negative_latency" %in% names(p))
  # the FALSE first element switches the negative-latency count on
  src2 <- p34_fixture("diary", "tib_FF", "output_din")
  if (dir.exists(src2)) {
    slog2 <- p34_fixture("diary", "slog2.csv")
    r2 <- rep_run(src2, params = raw.params(
      loglocation = slog2, sleepwindowType = "TimeInBed",
      sib_must_fully_overlap_with_TimeInBed = c(FALSE, FALSE)))
    expect_true("N_nights_negative_latency" %in% names(r2$personsummary_cleaned))
  }
})

test_that("the port matches live GGIR across the parameter surface", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_OUT())
  o <- rep_ms4(MOS2_OUT())[[1]]
  ns <- o$nightsummary
  p5 <- rep_ms5(MOS2_OUT())[[1]]
  one <- function(x, tel = NULL) {
    list(MOS2E39230594.gt3x = list(nightsummary = x, tail_expansion_log = tel,
                                   GGIRversion = "3.3.6"))
  }
  five <- function(x) list(MOS2E39230594.gt3x = x)

  expect_matches_ggir("defaults, with part 5", one(ns), five(p5))
  expect_matches_ggir("defaults, without part 5", one(ns))
  expect_matches_ggir("storefolderstructure TRUE", one(ns), five(p5),
                      output = list(storefolderstructure = TRUE))
  suppressWarnings(expect_matches_ggir("consider_marker_button TRUE", one(ns), five(p5),
                                       sleep = list(consider_marker_button = TRUE)))
  # a non-empty tail expansion log drops the highest-numbered night
  r <- expect_matches_ggir("tail_expansion_log", one(ns, list(short = 100, long = 1)))
  expect_identical(r$nightsummary_full$night, c(1, 2, 3))

  nsa <- ns
  nsa$sleepparam <- "sleep.T5A5"
  nsb <- ns
  nsb$sleepparam <- "sleep.T10A5"
  nsb$SptDuration <- nsb$SptDuration * 0.9
  nsb$number_sib_wakinghours <- c(0, 5, 3, 0)
  nsd <- rbind(nsa, nsb)
  nsd <- nsd[order(nsd$night), ]
  row.names(nsd) <- seq_len(nrow(nsd))
  r2 <- expect_matches_ggir("two sib definitions", one(nsd))
  expect_identical(nrow(r2$nightsummary_full), 8L)
  expect_identical(nrow(r2$personsummary_full), 1L)
  expect_true(all(c("SptDuration_AD_sleep.T5A5_mn", "SptDuration_AD_sleep.T10A5_mn") %in%
                    names(r2$personsummary_full)))
  expect_length(which(names(r2$personsummary_full) == "N_nights_guider_corrected"), 1L)

  nsw <- ns
  nsw$guider <- c("HDCZA", "NotWorn", "NotWorn+invalid", "HDCZA")
  nsw$cleaningcode <- 1
  expect_matches_ggir("NotWorn guiders", one(nsw))
  nsz <- ns
  nsz$number_sib_wakinghours <- c(0, 25, 21, 0)
  nsz$cleaningcode <- 1
  expect_matches_ggir("zero daytime bouts on two nights", one(nsz))
  nswe <- ns
  nswe$weekday <- c("Friday", "Saturday", "Friday", "Saturday")
  nswe$cleaningcode <- 1
  expect_matches_ggir("weekend nights only", one(nswe))
  nse <- ns
  nse$ID[2] <- ""
  expect_matches_ggir("one row with an empty ID", one(nse))
  nsv <- ns[, colnames(ns) != "GGIRversion"]
  expect_matches_ggir("night table from before GGIR 3.0-10", one(nsv))
  nssp <- ns
  nssp$filename <- "MOS2E39230594_split3_1TO2.gt3x.RData"
  expect_matches_ggir("segmented recording name", one(nssp))
})

test_that("the port matches live GGIR on the three paths that leave columns unnamed", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_OUT())
  slog <- p34_fixture("diary", "slog.csv")
  if (!file.exists(slog)) skip("the diary fixture is absent")
  ns <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  # with a diary and no cleaningcode 0 the accelerometer block is skipped in the full pass
  n1 <- ns
  n1$cleaningcode <- c(1, 2, 1, 2)
  r <- expect_matches_ggir("CRIT empty in the full pass",
                           list(MOS2E39230594.gt3x = list(nightsummary = n1,
                                                          tail_expansion_log = NULL,
                                                          GGIRversion = "3.3.6")),
                           sleep = list(loglocation = slog))
  # 12 general columns plus the 18 of the guider block, which runs outside the CRIT gate
  expect_identical(ncol(r$personsummary_full), 30L)
  expect_false("SptDuration_AD_T5A5_mn" %in% names(r$personsummary_full))
  expect_identical(names(r$personsummary_full)[13], "guider_SptDuration_AD_mn")
  expect_identical(names(r$personsummary_full)[30], "guider_wakeup_WE_sd")
  # with the first recording empty, the second one's names are used for the whole matrix
  a <- n1
  a$ID <- "AA.gt3x"
  a$filename <- "AA.gt3x.RData"
  b <- ns
  b$ID <- "BB.gt3x"
  b$filename <- "BB.gt3x.RData"
  b$cleaningcode <- 0
  r2 <- expect_matches_ggir("first recording empty, second with data",
                            list(AA.gt3x = list(nightsummary = a, tail_expansion_log = NULL,
                                                GGIRversion = "3.3.6"),
                                 BB.gt3x = list(nightsummary = b, tail_expansion_log = NULL,
                                                GGIRversion = "3.3.6")),
                            sleep = list(loglocation = slog))
  expect_identical(nrow(r2$personsummary_full), 2L)
  expect_true("SptDuration_AD_T5A5_mn" %in% names(r2$personsummary_full))
  # a sleepparam of "0" is dropped from udef but the row stays in the night table
  n3 <- ns
  n3$sleepparam <- c("T5A5", "0", "T5A5", "T5A5")
  r3 <- expect_matches_ggir("a sleepparam of 0",
                            list(MOS2E39230594.gt3x = list(nightsummary = n3,
                                                           tail_expansion_log = NULL,
                                                           GGIRversion = "3.3.6")))
  expect_identical(nrow(r3$nightsummary_full), 4L)
  # one definition survives the trim, so the AD block is the 6 guider columns plus 27
  expect_length(grep("_AD_", names(r3$personsummary_full)), 33L)
  expect_identical(ncol(r3$personsummary_full), 113L)
})

test_that("the report separators reach the csv files", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_OUT())
  ns <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  g <- rep_ggir_dir(list(MOS2E39230594.gt3x = list(nightsummary = ns,
                                                   tail_expansion_log = NULL,
                                                   GGIRversion = "3.3.6")))
  rep_ggir_report(g, output = list(sep_reports = ";", dec_reports = ","))
  r <- suppressWarnings(raw.sleep.report(
    ns, params = raw.params(sep_reports = ";", dec_reports = ",")))
  expect_same_csvs(rep_write(r), g, "sep ; dec ,")
})

test_that("the data cleaning file drops the listed (ID, night) pairs from the cleaned pass", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_OUT())
  if (!requireNamespace("data.table", quietly = TRUE)) skip("data.table is not installed")
  ns <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  dcf <- file.path(tempdir(), "canhract_dclean.csv")
  utils::write.csv(data.frame(ID = "MOS2E39230594.gt3x", night_part4 = 3), dcf,
                   row.names = FALSE)
  r <- expect_matches_ggir("data_cleaning_file drops night 3",
                           list(MOS2E39230594.gt3x = list(nightsummary = ns,
                                                          tail_expansion_log = NULL,
                                                          GGIRversion = "3.3.6")),
                           data_cleaning_file = dcf)
  expect_identical(r$nightsummary_full$night, c(1, 2, 3, 4))
  expect_identical(r$nightsummary_cleaned$night, 2)
})

test_that("the part-5 merge follows GGIR's gate, duplicate rule and ID normalisation", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_OUT())
  ns <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  p5 <- rep_ms5(MOS2_OUT())[[1]]
  one <- list(MOS2E39230594.gt3x = list(nightsummary = ns, tail_expansion_log = NULL,
                                        GGIRversion = "3.3.6"))
  # the gate reads the window column of the first table only
  expect_null(.raw.nights.merge.part5(NULL))
  expect_null(.raw.nights.merge.part5(list()))
  p5m <- p5
  p5m$window <- "MM"
  expect_null(.raw.nights.merge.part5(p5m))
  expect_matches_ggir("part 5 without a WW window", one, list(MOS2E39230594.gt3x = p5m))
  got <- .raw.nights.merge.part5(p5)
  expect_identical(names(got), c("ID", "window", "night", "nonwear_perc_spt", "ACC_spt_mg"))
  expect_identical(got$night, c("2", "3", "4"))
  # a duplicated (ID, night) keeps the first occurrence, whatever GGIR's own comment says
  ww <- p5[p5$window == "WW", ]
  ww2 <- ww
  ww2$ACC_spt_mg <- 999
  dup <- .raw.nights.merge.part5(rbind(ww, ww2))
  expect_identical(nrow(dup), 3L)
  expect_identical(dup$ACC_spt_mg, ww$ACC_spt_mg)
  expect_false(any(dup$ACC_spt_mg == "999"))
  expect_matches_ggir("part 5 with a duplicated night", one,
                      list(MOS2E39230594.gt3x = rbind(ww, ww2)))
  # a numeric ID stored with a leading zero is normalised for the merge and restored after
  nsn <- ns
  nsn$ID <- "0042"
  p5n <- p5
  p5n$ID <- "42"
  r <- expect_matches_ggir("numeric ID with a leading zero",
                           list(MOS2E39230594.gt3x = list(nightsummary = nsn,
                                                          tail_expansion_log = NULL,
                                                          GGIRversion = "3.3.6")),
                           list(MOS2E39230594.gt3x = p5n))
  expect_identical(unique(r$nightsummary_full$ID), "0042")
  expect_identical(r$nightsummary_full$window, c(NA, "WW", "WW", "WW"))
  expect_false("ID_old" %in% names(r$nightsummary_full))
})

test_that(".raw.report.tidyup rounds what parses and leaves what does not", {
  df <- data.frame(id = c("a1", "b2"), x = c("1.23456", "2.5"),
                   d = c("2025-10-07", "2025-10-08"), ts = c("23:04:28", "07:20:02"),
                   FRAG_a = c("0.1234567", "0.7654321"),
                   FRAG_b = c("0.1111111", "0.2222222"), stringsAsFactors = FALSE)
  out <- .raw.report.tidyup(df)
  expect_identical(out$id, c("a1", "b2"))          # never parses, so untouched
  expect_identical(out$x, c(1.235, 2.5))           # three decimals
  expect_identical(out$d, c("2025-10-07", "2025-10-08"))
  expect_identical(out$ts, c("23:04:28", "07:20:02"))
  expect_identical(out$FRAG_a, c(0.123457, 0.765432))  # six decimals
  expect_identical(out$FRAG_b, c(0.111111, 0.222222))
  # a single matching column is mangled, because df[, colsID] drops to a vector; GGIR does the same
  df1 <- data.frame(id = c("a1", "b2"), FRAG_a = c("0.1234567", "0.7654321"),
                    stringsAsFactors = FALSE)
  out1 <- suppressWarnings(.raw.report.tidyup(df1))
  expect_identical(out1$FRAG_a, c(0.123457, 0.123457))
  # all-NA rows are dropped
  df2 <- data.frame(a = c(1, NA, 3), b = c("x", NA, "z"), stringsAsFactors = FALSE)
  expect_identical(nrow(.raw.report.tidyup(df2)), 2L)
  # a one-column frame collapses to a vector and errors on ncol(), as in GGIR
  df3 <- data.frame(a = c("1.23456", "2.5"), stringsAsFactors = FALSE)
  expect_error(.raw.report.tidyup(df3), "argument is of length zero")
  # a factor column is parsed through as.character first
  df4 <- data.frame(a = factor(c("1.23456", "2.5")), b = 1:2)
  expect_identical(.raw.report.tidyup(df4)$a, c(1.235, 2.5))
})

test_that(".raw.report.tidyup matches GGIR's tidyup_df", {
  skip_if_no_ggir()
  df <- data.frame(id = c("a1", "b2"), x = c("1.23456", "2.5"),
                   ABI = c("0.1234567", "0.7654321"), stringsAsFactors = FALSE)
  expect_identical(suppressWarnings(.raw.report.tidyup(df)),
                   suppressWarnings(GGIR:::tidyup_df(df)))
  df2 <- data.frame(a = c(1.23456, NA), b = c("p", "q"), stringsAsFactors = FALSE)
  expect_identical(.raw.report.tidyup(df2), GGIR:::tidyup_df(df2))
  df3 <- data.frame(a = c("1.23456", "2.5"), stringsAsFactors = FALSE)
  expect_error(GGIR:::tidyup_df(df3), "argument is of length zero")
})

test_that("the split-name helpers match GGIR and only touch a segmented file name", {
  plain <- .raw.report.splitnames("MOS2E39230594.gt3x")
  expect_null(plain$segment_names)
  expect_identical(plain$filename, "MOS2E39230594.gt3x")
  seg <- .raw.report.splitnames("MOS2E39230594_split3_1TO2.gt3x")
  expect_identical(seg$segment_names, c("3", "1", "2"))
  expect_identical(seg$filename, "MOS2E39230594.gt3x")
  # the only effect on an ordinary frame is that filename becomes a factor
  x <- data.frame(ID = "a", filename = "MOS2E39230594.gt3x", v = 1,
                  stringsAsFactors = FALSE)
  y <- .raw.report.addsplitnames(x)
  expect_s3_class(y$filename, "factor")
  expect_identical(names(y), names(x))
  z <- .raw.report.addsplitnames(x[0, ])
  expect_identical(class(z$filename), "character")
  s <- data.frame(ID = "a", filename = "MOS2E39230594_split3_1TO2.gt3x", v = 1,
                  w = 2, stringsAsFactors = FALSE)
  s2 <- .raw.report.addsplitnames(s)
  expect_true(all(c("split1_name", "split2_name") %in% names(s2)))
  expect_identical(which(names(s2) == "split1_name"), 3L)
  expect_identical(s2$split1_name, "1")
  expect_identical(s2$split2_name, "2")
})

test_that("the split-name helpers are identical to GGIR's", {
  skip_if_no_ggir()
  for (fn in c("MOS2E39230594.gt3x", "MOS2E39230594_split3_1TO2.gt3x",
               "a_split1_2TO3.csv")) {
    expect_identical(.raw.report.splitnames(fn), GGIR:::getSplitNames(fn))
  }
  x <- data.frame(ID = c("a", "b"), filename = c("r_split3_1TO2.gt3x", "r_split3_2TO3.gt3x"),
                  v = 1:2, w = 3:4, stringsAsFactors = FALSE)
  expect_identical(.raw.report.addsplitnames(x), GGIR:::addSplitNames(x))
})

test_that("the night table is rendered at seven significant digits before anything is averaged", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  e <- new.env(parent = emptyenv())
  load(file.path(MOS2_OUT(), "meta/ms4.out/MOS2E39230594.gt3x.RData"), envir = e)
  m <- .raw.report.nights.one(e$nightsummary, NULL)
  expect_true(is.matrix(m))
  expect_identical(typeof(m), "character")
  expect_identical(unname(m[, "SptDuration"]),
                   c("8.193056", "8.254167", "7.204167", "1.213889"))
  # the number of decimals is chosen per column
  expect_identical(unname(m[, "duration_sib_wakinghours"]),
                   c("2.7236111", "4.4791667", "3.4166667", "0.1583333"))
  expect_identical(unname(m[, "night"]), c("1", "2", "3", "4"))
  expect_identical(m, as.matrix(e$nightsummary))
  # the tail expansion log drops the last night before the conversion
  m2 <- .raw.report.nights.one(e$nightsummary, list(short = 1))
  expect_identical(nrow(m2), 3L)
  # a row whose first column is empty is dropped, unless that empties the frame
  ns <- e$nightsummary
  ns$ID[2] <- ""
  expect_identical(nrow(.raw.report.nights.one(ns, NULL)), 3L)
  ns2 <- e$nightsummary
  ns2$ID <- ""
  expect_identical(nrow(.raw.report.nights.one(ns2, NULL)), 4L)
  # a milestone from before GGIR 3.0-10 gains an empty GGIRversion column
  ns3 <- e$nightsummary[, colnames(e$nightsummary) != "GGIRversion"]
  m3 <- .raw.report.nights.one(ns3, NULL)
  expect_identical(colnames(m3)[ncol(m3)], "GGIRversion")
  expect_identical(unname(m3[, "GGIRversion"]), rep("", 4))
  m4 <- .raw.report.nights.one(ns3[0, ], NULL)
  expect_identical(nrow(m4), 0L)
  expect_identical(colnames(m4)[ncol(m4)], "GGIRversion")
})

test_that("the returned frames are unrounded and the writer is what rounds them", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  r <- rep_run(MOS2_OUT())
  expect_identical(num(r$personsummary_cleaned$SleepRegularityIndex1_AD_T5A5_mn), 38.497)
  expect_equal(num(r$personsummary_cleaned$SptDuration_AD_T5A5_sd),
               stats::sd(c(8.254167, 7.204167)), tolerance = 1e-14)
  expect_true(num(r$personsummary_cleaned$SptDuration_AD_T5A5_sd) != 0.742)
  d <- rep_write(r)
  csv <- utils::read.csv(file.path(d, REP_CSVS[4]), stringsAsFactors = FALSE)
  expect_identical(csv$SptDuration_AD_T5A5_sd, 0.742)
  expect_identical(csv$SptDuration_AD_T5A5_mn, 7.729)
  # na = "": the blank weekend cells are empty strings, not the text NA
  line <- readLines(file.path(d, REP_CSVS[4]))[2]
  expect_false(grepl("NA", line))
  expect_true(grepl(",,", line))
})

test_that("write.raw.sleep.report honours the separators and refuses a foreign object", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  if (!requireNamespace("data.table", quietly = TRUE)) skip("data.table is not installed")
  r <- rep_run(MOS2_OUT(), params = raw.params(sep_reports = ";", dec_reports = ","))
  d <- rep_write(r)
  h <- readLines(file.path(d, REP_CSVS[3]))
  expect_true(grepl("^ID;night;sleeponset", h[1]))
  expect_true(grepl("23,512", h[2]))
  expect_error(write.raw.sleep.report(list(), tempdir()), "canhrActi_raw_sleep_report")
  expect_error(raw.sleep.report(NULL), "canhrActi_raw_nights")
  expect_error(raw.sleep.report(list(1:3)), "must be a canhrActi_raw_nights")
})

test_that("the whole pipeline from a part-1 milestone reproduces the stored MOS2 csv files", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  basic <- file.path(MOS2_OUT(), "meta/basic/meta_MOS2E39230594.gt3x.RData")
  if (!file.exists(basic)) skip("the MOS2 part-1 milestone is absent")
  ms <- read.ggir.milestone(basic)
  p3 <- raw.sleep.part3(ms, params = raw.params())
  nt <- raw.sleep.nights(p3, params = raw.params())
  expect_s3_class(nt, "canhrActi_raw_nights")
  p5 <- rep_ms5(MOS2_OUT())[[1]]
  # params NULL reuses the parameters the night table was built with
  r <- raw.sleep.report(nt, part5 = p5)
  expect_same_csvs(rep_write(r), MOS2_OUT(), "end to end")
  expect_identical(attr(r, "canhrActi")$n_recordings, 1L)
  expect_true(attr(r, "canhrActi")$settings$part5_merged)
  expect_false(attr(r, "canhrActi")$settings$only.use.sleeplog)
})

test_that("a diary passed as an object still turns the cleaned rule on", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  ns <- rep_ms4(MOS2_OUT())[[1]]$nightsummary
  ns$cleaningcode <- c(0, 1, 0, 2)
  fake <- ns
  attr(fake, "canhrActi") <- list(dolog = TRUE)
  class(fake) <- c("canhrActi_raw_nights", "data.frame")
  r <- suppressWarnings(raw.sleep.report(fake, params = raw.params()))
  expect_true(attr(r, "canhrActi")$settings$only.use.sleeplog)
  expect_identical(r$nightsummary_cleaned$night, c(1, 3))
  expect_true(any(grepl("loglocation is empty", attr(r, "canhrActi")$status$messages)))
  # without that flag the same table keeps night 2
  r2 <- suppressWarnings(raw.sleep.report(ns, params = raw.params()))
  expect_identical(r2$nightsummary_cleaned$night, c(1, 2, 3))
})

test_that("the print method describes the report and LC_TIME is restored", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  before <- Sys.getlocale("LC_TIME")
  r <- rep_run(MOS2_OUT())
  expect_identical(Sys.getlocale("LC_TIME"), before)
  out <- utils::capture.output(print.canhrActi_raw_sleep_report(r))
  expect_true(any(grepl("canhrActi sleep report", out)))
  expect_true(any(grepl("nightsummary_full", out)))
  expect_true(any(grepl("4 rows x 42 columns", out)))
  expect_true(any(grepl("part 5:      merged", out)))
  utils::capture.output(expect_invisible(print.canhrActi_raw_sleep_report(r)))
  # also restored after an error
  expect_error(raw.sleep.report(list(1:3)), "must be a canhrActi_raw_nights or a data frame")
  expect_identical(Sys.getlocale("LC_TIME"), before)
})

test_that("V1: the four ported GGIR functions have not changed in the installed namespace", {
  skip_if_no_ggir()
  ns <- asNamespace("GGIR")
  fns <- c("g.report.part4", "tidyup_df", "addSplitNames", "getSplitNames")
  missing_fns <- fns[!vapply(fns, function(fn) {
    is.function(get0(fn, envir = ns, inherits = FALSE))
  }, logical(1))]
  expect_identical(missing_fns, character(0), info = toString(missing_fns))
  # a GGIR other than the one the lengths below come from is reported rather than failed
  v <- as.character(utils::packageVersion("GGIR"))
  if (v != "3.3.6") {
    testthat::skip(paste0("GGIR ", v, " is not the 3.3.6 whose body lengths are pinned here; ",
                          "re-check g.report.part4 before trusting parity"))
  }
  lens <- vapply(fns, function(fn) length(deparse(get(fn, envir = ns))), integer(1))
  pinned <- c(g.report.part4 = 789L, tidyup_df = 27L, addSplitNames = 26L, getSplitNames = 11L)
  changed <- names(pinned)[lens[names(pinned)] != pinned]
  expect_identical(changed, character(0),
                   info = paste0(changed, ": ", lens[changed], " lines, pinned ", pinned[changed], collapse = "; "))
})

test_that("GGIRversion is the ggir_version_label setting, not the installed GGIR's version", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_OUT())
  basic <- file.path(MOS2_OUT(), "meta/basic/meta_MOS2E39230594.gt3x.RData")
  if (!file.exists(basic)) skip("the MOS2 part-1 milestone is absent")
  ms <- read.ggir.milestone(basic)
  p3 <- raw.sleep.part3(ms, params = raw.params())
  p5 <- rep_ms5(MOS2_OUT())[[1]]
  # an installed GGIR other than the one the port matches, such as a desktop bundle's
  local_mocked_bindings(packageVersion = function(pkg, ...) package_version("3.3.0"),
                        .package = "utils")
  nt <- raw.sleep.nights(p3, params = raw.params())
  expect_identical(unique(nt$GGIRversion), .RAW_GGIR_VERSION_LABEL)
  expect_identical(.RAW_GGIR_VERSION_LABEL, "3.3.6")
  expect_same_csvs(rep_write(raw.sleep.report(nt, part5 = p5)), MOS2_OUT(), "installed 3.3.0")
  # another label reaches all four files and changes nothing else
  nt9 <- raw.sleep.nights(p3, params = raw.params(ggir_version_label = "9.9.9"))
  d <- rep_write(raw.sleep.report(nt9, part5 = p5))
  for (f in REP_CSVS) {
    a <- utils::read.csv(file.path(d, f), colClasses = "character", check.names = FALSE)
    b <- utils::read.csv(file.path(MOS2_OUT(), f), colClasses = "character", check.names = FALSE)
    expect_identical(unique(a$GGIRversion), "9.9.9", info = f)
    b$GGIRversion <- "9.9.9"
    expect_identical(a, b, info = f)
  }
})
