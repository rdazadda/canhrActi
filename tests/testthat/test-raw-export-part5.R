# The part-5 half of write.ggir.milestone and as.ggir.milestone against everything GGIR part 5
# writes: meta/ms5.out, meta/ms5.outraw and results. The stored objects and csv files of the
# two reference recordings and the part-5 fixtures are compared with identical(); then our
# milestone is handed to GGIR:::g.report.part5, which must reproduce GGIR's own csv files line
# for line. The slow chain from the raw .gt3x runs only under CANHRACTI_GGIR_SLOW. Reference
# data come from CANHRACTI_GGIR_REF, the fixtures from CANHRACTI_GGIR_P5FIX (falling back to
# siblings of the reference folder); live GGIR comparisons skip when GGIR is absent.

withr::local_locale(c(LC_TIME = "C"))
# GGIR wrote the reference outputs in an America/Anchorage session
withr::local_timezone("America/Anchorage")

.ggir_ref_x5 <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

x5_skip_ref <- function() {
  if (.ggir_ref_x5 == "" || !dir.exists(.ggir_ref_x5)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
x5_skip_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
x5_ref <- function(...) file.path(sub("/+$", "", .ggir_ref_x5), ...)

x5_sibling <- function(env, folder, name) {
  base <- Sys.getenv(env, unset = "")
  if (base == "") {
    if (.ggir_ref_x5 == "") return("")
    base <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref_x5, winslash = "/",
                                                           mustWork = FALSE))),
                      folder, "fixtures")
  }
  file.path(base, name)
}
x5_p5fix <- function(name) x5_sibling("CANHRACTI_GGIR_P5FIX", "ggir-study-p5", name)

x5_loadenv <- function(path) {
  e <- new.env()
  load(path, envir = e)
  e
}
x5_tmp <- function(name) {
  p <- file.path(tempdir(), paste0("canhrActi_p5exp_", name))
  unlink(p, recursive = TRUE)
  p
}

# The canhrActi_raw and the night table part 5 reads, built from stored milestones so that the
# input is identical to GGIR's.
x5_build <- function(metadir) {
  fn <- dir(file.path(metadir, "ms3.out"))
  if (length(fn) == 0) testthat::skip(paste0("no ms3 milestone under ", metadir))
  fn <- fn[1]
  x <- read.ggir.milestone(file.path(metadir, "basic", paste0("meta_", fn)))
  ms2 <- x5_loadenv(file.path(metadir, "ms2.out", fn))
  ms3 <- x5_loadenv(file.path(metadir, "ms3.out", fn))
  ms4 <- x5_loadenv(file.path(metadir, "ms4.out", fn))
  x$imputed <- ms2$IMP
  x$sleep <- list(sib.cla.sum = ms3$sib.cla.sum, SPTE_start = ms3$SPTE_start,
                  SPTE_end = ms3$SPTE_end, longitudinal_axis = ms3$longitudinal_axis,
                  tail_expansion_log = ms3$tail_expansion_log,
                  rec_starttime = ms3$rec_starttime, id = ms3$ID)
  list(x = x, nights = ms4$nightsummary, fn = fn)
}

# raw.timeuse takes about 6 s a recording, so each (metadir, pars) pair is computed once.
.x5_cache <- new.env(parent = emptyenv())
x5_timeuse <- function(key, metadir, pars = list()) {
  if (!is.null(.x5_cache[[key]])) return(.x5_cache[[key]])
  b <- x5_build(metadir)
  args <- c(list(b$x, b$nights, ggir_version_label = "3.3.6"), pars)
  tu <- suppressWarnings(do.call(raw.timeuse, args))
  out <- list(tu = tu, x = b$x, nights = b$nights, fn = b$fn)
  .x5_cache[[key]] <- out
  out
}

x5_lines_equal <- function(a, b) identical(readLines(a), readLines(b))

# the legend file is named after the day it was written, so the date is matched as a pattern
x5_undate <- function(x) sub("behavioralcodes[0-9]{4}-[0-9]{2}-[0-9]{2}[.]csv$",
                             "behavioralcodes<date>.csv", x)

# the first differing line, for failure messages
x5_first_diff <- function(a, b) {
  la <- readLines(a); lb <- readLines(b)
  n <- min(length(la), length(lb))
  k <- which(la[seq_len(n)] != lb[seq_len(n)])
  if (length(k) == 0) {
    return(paste0(basename(a), ": same first ", n, " lines, lengths ", length(la), " and ",
                  length(lb)))
  }
  paste0(basename(a), " line ", k[1], "\n  mine: ", substr(la[k[1]], 1, 300),
         "\n  ref : ", substr(lb[k[1]], 1, 300))
}

# GGIR's own report layer over a tree this package wrote; the two params_output members are
# read by the report and not by part 5 itself.
x5_ggir_report <- function(root, f1 = 1, pars = list()) {
  p <- GGIR::load_params()
  for (nm in c("require_complete_lastnight_part5", "week_weekend_aggregate.part5")) {
    if (!is.null(pars[[nm]])) p$params_output[[nm]] <- pars[[nm]]
  }
  rn <- normalizePath(root, winslash = "/")
  GGIR:::g.report.part5(metadatadir = rn, f0 = 1, f1 = f1, loglocation = c(),
                        params_cleaning = p$params_cleaning, params_output = p$params_output,
                        LUX_day_segments = c(), verbose = FALSE)
  GGIR:::g.report.part5_dictionary(metadatadir = rn, params_output = p$params_output)
  rn
}

test_that("MOS2: the written tree has GGIR's folder names, file names and save order", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("MOS2", metadir)
  root <- x5_tmp("mos2")
  w <- write.ggir.milestone(r$tu, root, parts = 5)
  expect_identical(sort(x5_undate(list.files(root, recursive = TRUE))),
                   sort(c("meta/ms5.out/MOS2E39230594.gt3x.RData",
                          "meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData",
                          "meta/ms5.outraw/behavioralcodes<date>.csv",
                          "meta/ms5.outraw/sib.reports/sib_report_MOS2E39230594gt3x_T5A5.csv")))
  # the time series stem drops gt3x and the sib report stem does not
  expect_identical(names(w), c("part5_sibreport", "part5_legend", "part5_series", "part5"))
  # results/QC is created empty because GGIR's report layer errors on a missing folder
  expect_true(dir.exists(file.path(root, "results", "QC")))

  # the ms5.outraw objects; the timezone one is called desiredtz, as GGIR's save() names it
  mine <- x5_loadenv(file.path(root, "meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData"))
  stored <- x5_loadenv(file.path(metadir, "ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData"))
  expect_identical(sort(ls(mine)), c("desiredtz", "filename", "Lnames", "mdat"))
  expect_identical(sort(ls(mine)), sort(ls(stored)))
  expect_false("desiredtz_part1" %in% ls(mine))
  expect_identical(dim(mine$mdat), c(118800L, 14L))
  expect_identical(names(mine$mdat),
                   c("timenum", "ACC", "SleepPeriodTime", "invalidepoch", "guider", "window",
                     "sibdetection", "selfreported", "angle", "class_id",
                     "invalid_fullwindow", "invalid_sleepperiod", "invalid_wakinghours",
                     "timestamp"))
  expect_identical(mine$mdat, stored$mdat)
  expect_identical(mine$filename, stored$filename)
  expect_identical(mine$filename, "MOS2E39230594.gt3x")
  expect_identical(mine$Lnames, stored$Lnames)
  expect_identical(length(mine$Lnames), 18L)
  expect_identical(mine$desiredtz, stored$desiredtz)
  expect_identical(mine$desiredtz, "")

  # the ms5 milestone itself
  m5 <- x5_loadenv(file.path(root, "meta/ms5.out/MOS2E39230594.gt3x.RData"))
  s5 <- x5_loadenv(file.path(metadir, "ms5.out/MOS2E39230594.gt3x.RData"))
  expect_identical(sort(ls(m5)),
                   sort(c("output", "tail_expansion_log", "GGIRversion", "last_timestamp")))
  expect_identical(m5$output, s5$output)
  expect_identical(dim(m5$output), c(7L, 119L))
  expect_identical(m5$tail_expansion_log, s5$tail_expansion_log)
  expect_identical(m5$GGIRversion, s5$GGIRversion)
  expect_identical(m5$GGIRversion, "3.3.6")
  expect_identical(m5$last_timestamp, s5$last_timestamp)

  # the two csv files, byte for byte
  sib <- file.path(root, "meta/ms5.outraw/sib.reports/sib_report_MOS2E39230594gt3x_T5A5.csv")
  sibref <- file.path(metadir, "ms5.outraw/sib.reports/sib_report_MOS2E39230594gt3x_T5A5.csv")
  expect_true(x5_lines_equal(sib, sibref), info = x5_first_diff(sib, sibref))
  expect_identical(length(readLines(sib)), 67L)
  leg <- dir(file.path(root, "meta/ms5.outraw"), pattern = "^behavioralcodes",
             full.names = TRUE)
  legref <- dir(file.path(metadir, "ms5.outraw"), pattern = "^behavioralcodes",
                full.names = TRUE)
  expect_identical(length(leg), 1L)
  expect_true(x5_lines_equal(leg, legref), info = x5_first_diff(leg, legref))
  expect_identical(length(readLines(leg)), 19L)
})

test_that("EE: the same, with a dot inside the file name that both strip rules eat", {
  x5_skip_ref()
  metadir <- x5_ref("timing_out", "output_timing", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("EE", metadir)
  root <- x5_tmp("ee")
  write.ggir.milestone(r$tu, root, parts = 5)
  expect_identical(sort(x5_undate(list.files(root, recursive = TRUE))),
                   sort(c("meta/ms5.out/EE_left_29.5.2017-05-30.gt3x.RData",
                          "meta/ms5.outraw/40_100_400/EE_left_2952017-05-30_T5A5.RData",
                          "meta/ms5.outraw/behavioralcodes<date>.csv",
                          "meta/ms5.outraw/sib.reports/sib_report_EE_left_2952017-05-30gt3x_T5A5.csv")))
  mine <- x5_loadenv(file.path(root, "meta/ms5.outraw/40_100_400/EE_left_2952017-05-30_T5A5.RData"))
  stored <- x5_loadenv(file.path(metadir, "ms5.outraw/40_100_400/EE_left_2952017-05-30_T5A5.RData"))
  expect_identical(dim(mine$mdat), c(120960L, 14L))
  expect_identical(mine$mdat, stored$mdat)
  expect_identical(mine$desiredtz, "Europe/Helsinki")
  expect_identical(mine$desiredtz, stored$desiredtz)
  expect_identical(mine$Lnames, stored$Lnames)
  m5 <- x5_loadenv(file.path(root, "meta/ms5.out/EE_left_29.5.2017-05-30.gt3x.RData"))
  s5 <- x5_loadenv(file.path(metadir, "ms5.out/EE_left_29.5.2017-05-30.gt3x.RData"))
  expect_identical(m5$output, s5$output)
  expect_identical(dim(m5$output), c(11L, 119L))
  expect_identical(m5$last_timestamp, s5$last_timestamp)
  sib <- file.path(root, "meta/ms5.outraw/sib.reports/sib_report_EE_left_2952017-05-30gt3x_T5A5.csv")
  sibref <- file.path(metadir, "ms5.outraw/sib.reports/sib_report_EE_left_2952017-05-30gt3x_T5A5.csv")
  expect_true(x5_lines_equal(sib, sibref), info = x5_first_diff(sib, sibref))
})

test_that("the two basename strip rules are GGIR's, verbatim", {
  # the dot class is unanchored, so it also eats the dot inside EE's name
  expect_identical(.raw.ms5.strip("MOS2E39230594.gt3x.RData", TRUE), "MOS2E39230594")
  expect_identical(.raw.ms5.strip("MOS2E39230594.gt3x.RData", FALSE), "MOS2E39230594gt3x")
  expect_identical(.raw.ms5.strip("EE_left_29.5.2017-05-30.gt3x.RData", TRUE),
                   "EE_left_2952017-05-30")
  expect_identical(.raw.ms5.strip("EE_left_29.5.2017-05-30.gt3x.RData", FALSE),
                   "EE_left_2952017-05-30gt3x")
  # the rules are case insensitive and also strip csv, cwa and bin
  expect_identical(.raw.ms5.strip("rec.CWA.RData", TRUE), "rec")
  expect_identical(.raw.ms5.strip("rec.bin.RData", FALSE), "rec")
})

test_that("P8a MOS2: GGIR's report layer reproduces its own eight csv files from our ms5", {
  x5_skip_ref()
  x5_skip_ggir()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("MOS2", metadir)
  root <- x5_tmp("mos2_report")
  write.ggir.milestone(r$tu, root, parts = 5)
  rn <- x5_ggir_report(root)
  refres <- x5_ref("out", "output_din", "results")
  expected <- c("part5_daysummary_MM_L40M100V400_T5A5.csv",
                "part5_daysummary_WW_L40M100V400_T5A5.csv",
                "part5_personsummary_MM_L40M100V400_T5A5.csv",
                "part5_personsummary_WW_L40M100V400_T5A5.csv",
                "QC/part5_daysummary_full_MM_L40M100V400_T5A5.csv",
                "QC/part5_daysummary_full_WW_L40M100V400_T5A5.csv",
                "variableDictionary/part5_dictionary_daysummary.csv",
                "variableDictionary/part5_dictionary_personsummary.csv")
  expect_identical(sort(list.files(file.path(rn, "results"), recursive = TRUE)),
                   sort(expected))
  for (f in expected) {
    a <- file.path(rn, "results", f)
    b <- file.path(refres, f)
    expect_true(x5_lines_equal(a, b), info = x5_first_diff(a, b))
  }
  # the shapes GGIR's own run produced
  nl <- function(f) length(readLines(file.path(rn, "results", f)))
  expect_identical(nl("part5_daysummary_MM_L40M100V400_T5A5.csv"), 4L)
  expect_identical(nl("part5_daysummary_WW_L40M100V400_T5A5.csv"), 4L)
  expect_identical(nl("part5_personsummary_MM_L40M100V400_T5A5.csv"), 2L)
  expect_identical(nl("QC/part5_daysummary_full_MM_L40M100V400_T5A5.csv"), 5L)
  expect_identical(nl("QC/part5_daysummary_full_WW_L40M100V400_T5A5.csv"), 4L)
  d <- data.table::fread(file.path(rn, "results", "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  expect_identical(ncol(d), 115L)
  p <- data.table::fread(file.path(rn, "results", "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  expect_identical(ncol(p), 207L)
  pw <- data.table::fread(file.path(rn, "results", "part5_personsummary_WW_L40M100V400_T5A5.csv"))
  expect_identical(ncol(pw), 206L)
})

test_that("P8a EE: the same eight csv files, with 5 day rows and a 208-column person row", {
  x5_skip_ref()
  x5_skip_ggir()
  metadir <- x5_ref("timing_out", "output_timing", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("EE", metadir)
  root <- x5_tmp("ee_report")
  write.ggir.milestone(r$tu, root, parts = 5)
  rn <- x5_ggir_report(root)
  refres <- x5_ref("timing_out", "output_timing", "results")
  fs <- sort(list.files(file.path(rn, "results"), recursive = TRUE))
  expect_identical(length(fs), 8L)
  for (f in fs) {
    a <- file.path(rn, "results", f)
    b <- file.path(refres, f)
    expect_true(file.exists(b))
    expect_true(x5_lines_equal(a, b), info = x5_first_diff(a, b))
  }
  expect_identical(length(readLines(file.path(rn, "results",
                                              "part5_daysummary_MM_L40M100V400_T5A5.csv"))), 6L)
  p <- data.table::fread(file.path(rn, "results", "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  expect_identical(ncol(p), 208L)
})

test_that("the part-5 fixtures write trees GGIR's report layer cannot tell from its own", {
  x5_skip_ref()
  x5_skip_ggir()
  cases <- list(
    # the first-wake repair, plain shapes
    excludefirst = list(dir = "excludefirst", pars = list(), nfiles = 8L),
    # qwindow adds a third window type, Segments
    qwindow = list(dir = "qwindow", pars = list(qwindow = c(0, 8, 24)), nfiles = 11L),
    # four threshold triples: four ms5.outraw folders, one legend, one sib report
    thresholds = list(dir = "thresholds",
                      pars = list(threshold.lig = c(30, 40), threshold.mod = c(100, 125),
                                  threshold.vig = 400), nfiles = 26L),
    # the truncated recording, where the report drops the last WW window
    lastnight = list(dir = "lastnight",
                     pars = list(require_complete_lastnight_part5 = TRUE), nfiles = 8L),
    # the week and weekend expansion, a 385-column person summary
    weekend_agg = list(dir = "weekend_agg",
                       pars = list(week_weekend_aggregate.part5 = TRUE), nfiles = 8L))
  ran <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    metadir <- file.path(x5_p5fix(cs$dir), "output_din", "meta")
    if (!dir.exists(metadir)) next
    ran <- ran + 1L
    r <- x5_timeuse(paste0("fix_", nm), metadir, cs$pars)
    root <- x5_tmp(paste0("fix_", nm))
    write.ggir.milestone(r$tu, root, parts = 5)
    # every file of the fixture's meta/ms5.outraw
    refraw <- file.path(metadir, "ms5.outraw")
    for (f in list.files(refraw, recursive = TRUE)) {
      mine <- file.path(root, "meta/ms5.outraw", f)
      if (grepl("behavioralcodes", f)) {
        mine <- dir(file.path(root, "meta/ms5.outraw"), pattern = "^behavioralcodes",
                    full.names = TRUE)
      }
      expect_identical(length(mine), 1L, info = paste(nm, f))
      expect_true(file.exists(mine), info = paste(nm, "missing", f))
      if (grepl("RData$", f)) {
        a <- x5_loadenv(mine)
        b <- x5_loadenv(file.path(refraw, f))
        for (o in ls(b)) {
          expect_identical(get(o, a), get(o, b), info = paste(nm, f, o))
        }
      } else {
        expect_true(x5_lines_equal(mine, file.path(refraw, f)),
                    info = x5_first_diff(mine, file.path(refraw, f)))
      }
    }
    # the ms5 milestone
    a5 <- x5_loadenv(file.path(root, "meta/ms5.out", r$fn))
    b5 <- x5_loadenv(file.path(metadir, "ms5.out", r$fn))
    for (o in ls(b5)) expect_identical(get(o, a5), get(o, b5), info = paste(nm, "ms5", o))
    # GGIR's report layer over our tree against the fixture's results
    rn <- x5_ggir_report(root, pars = cs$pars)
    refres <- file.path(x5_p5fix(cs$dir), "output_din", "results")
    mine_csv <- grep("part5", sort(list.files(file.path(rn, "results"), recursive = TRUE)),
                     value = TRUE)
    ref_csv <- grep("part5", sort(list.files(refres, recursive = TRUE)), value = TRUE)
    expect_identical(mine_csv, ref_csv, info = nm)
    expect_identical(length(mine_csv), cs$nfiles, info = nm)
    for (f in mine_csv) {
      a <- file.path(rn, "results", f)
      b <- file.path(refres, f)
      expect_true(x5_lines_equal(a, b), info = paste(nm, x5_first_diff(a, b)))
    }
  }
  if (ran == 0L) skip("no part-5 fixture found")
  expect_gt(ran, 0L)
})

test_that("the threshold grid creates one ms5.outraw folder per configuration", {
  x5_skip_ref()
  metadir <- file.path(x5_p5fix("thresholds"), "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("fix_thresholds", metadir,
                  list(threshold.lig = c(30, 40), threshold.mod = c(100, 125),
                       threshold.vig = 400))
  root <- x5_tmp("thresholds_folders")
  write.ggir.milestone(r$tu, root, parts = 5)
  # GGIR's nesting order is light, then moderate, then vigorous
  expect_identical(sort(x5_undate(dir(file.path(root, "meta/ms5.outraw")))),
                   sort(c("30_100_400", "30_125_400", "40_100_400", "40_125_400",
                          "sib.reports", "behavioralcodes<date>.csv")))
  for (tri in c("30_100_400", "30_125_400", "40_100_400", "40_125_400")) {
    expect_identical(dir(file.path(root, "meta/ms5.outraw", tri)),
                     "MOS2E39230594_T5A5.RData")
  }
  # one legend and one sib report for the whole grid
  expect_identical(length(dir(file.path(root, "meta/ms5.outraw"), pattern = "^behavioralcodes")),
                   1L)
  expect_identical(length(dir(file.path(root, "meta/ms5.outraw/sib.reports"))), 1L)
  # the 40/100/400 series is the reference run's; the fixture stored desiredtz_part1
  # "America/Anchorage" where the reference stored "", so only the tzone attribute differs
  mine <- x5_loadenv(file.path(root, "meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData"))
  stored <- x5_loadenv(x5_ref("out", "output_din", "meta", "ms5.outraw", "40_100_400",
                              "MOS2E39230594_T5A5.RData"))
  expect_identical(mine$mdat[, setdiff(names(mine$mdat), "timestamp")],
                   stored$mdat[, setdiff(names(stored$mdat), "timestamp")])
  expect_identical(as.numeric(mine$mdat$timestamp), as.numeric(stored$mdat$timestamp))
  expect_identical(mine$desiredtz, "America/Anchorage")
  expect_identical(stored$desiredtz, "")
})

test_that("two recordings write into one tree and share one behavioural-codes file", {
  x5_skip_ref()
  x5_skip_ggir()
  wide <- file.path(x5_p5fix("cohort_src"), "wide", "output_din", "meta")
  narrow <- file.path(x5_p5fix("cohort_src"), "narrow", "output_din", "meta")
  refres <- file.path(x5_p5fix("cohort"), "output_din", "results")
  skip_if_not(dir.exists(wide) && dir.exists(narrow) && dir.exists(refres))
  rw <- x5_timeuse("cohort_wide", wide, list(frag.metrics = "all"))
  rn2 <- x5_timeuse("cohort_narrow", narrow, list(frag.metrics = c()))
  expect_identical(dim(rw$tu$daysummary), c(7L, 149L))
  expect_identical(dim(rn2$tu$daysummary), c(11L, 119L))
  root <- x5_tmp("cohort")
  write.ggir.milestone(rw$tu, root, parts = 5)
  write.ggir.milestone(rn2$tu, root, parts = 5)
  # the second write finds the legend file already there and leaves it alone
  expect_identical(length(dir(file.path(root, "meta/ms5.out"))), 2L)
  expect_identical(length(dir(file.path(root, "meta/ms5.outraw/40_100_400"))), 2L)
  expect_identical(length(dir(file.path(root, "meta/ms5.outraw/sib.reports"))), 2L)
  expect_identical(length(dir(file.path(root, "meta/ms5.outraw"), pattern = "^behavioralcodes")),
                   1L)
  for (nm in c("MOS2E39230594.gt3x.RData", "EE_left_29.5.2017-05-30.gt3x.RData")) {
    a <- x5_loadenv(file.path(root, "meta/ms5.out", nm))
    b <- x5_loadenv(file.path(x5_p5fix("cohort"), "output_din", "meta", "ms5.out", nm))
    expect_identical(a$output, b$output)
    expect_identical(a$last_timestamp, b$last_timestamp)
  }
  # GGIR's report merges the 149-column and the 119-column milestones into six csv files
  rn <- x5_ggir_report(root, f1 = 2)
  for (f in sort(list.files(refres, recursive = TRUE))) {
    a <- file.path(rn, "results", f)
    b <- file.path(refres, f)
    expect_true(file.exists(a), info = f)
    expect_true(x5_lines_equal(a, b), info = x5_first_diff(a, b))
  }
  expect_identical(length(readLines(file.path(rn, "results",
                                              "part5_personsummary_MM_L40M100V400_T5A5.csv"))),
                   3L)
})

test_that("P6f save_ms5raw_format: the csv has 13 columns and the RData 14", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  tu <- x5_timeuse("MOS2", metadir)$tu
  tu$settings$params[["save_ms5raw_format"]] <- c("csv", "RData")
  root <- x5_tmp("format")
  write.ggir.milestone(tu, root, parts = 5)
  d <- file.path(root, "meta/ms5.outraw/40_100_400")
  expect_identical(sort(dir(d)), sort(c("MOS2E39230594_T5A5.RData", "MOS2E39230594_T5A5.csv")))
  e <- x5_loadenv(file.path(d, "MOS2E39230594_T5A5.RData"))
  cc <- data.table::fread(file.path(d, "MOS2E39230594_T5A5.csv"), nrows = 5)
  expect_identical(ncol(e$mdat), 14L)
  expect_identical(ncol(cc), 13L)
  # timestamp is the column GGIR re-appends after writing the csv
  expect_true("timestamp" %in% names(e$mdat))
  expect_false("timestamp" %in% names(cc))
  expect_identical(colnames(cc), setdiff(names(e$mdat), "timestamp"))
  # csv alone writes no RData
  tu2 <- tu
  tu2$settings$params[["save_ms5raw_format"]] <- "csv"
  root2 <- x5_tmp("format_csv")
  write.ggir.milestone(tu2, root2, parts = 5)
  expect_identical(dir(file.path(root2, "meta/ms5.outraw/40_100_400")),
                   "MOS2E39230594_T5A5.csv")
})

test_that("save_ms5rawlevels and do.sibreport gate the ms5.outraw tree the way GGIR does", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  tu <- x5_timeuse("MOS2", metadir)$tu
  # no series: the folder still exists for the sib report
  a <- tu; a$settings$params[["save_ms5rawlevels"]] <- FALSE
  ra <- x5_tmp("norawlevels")
  write.ggir.milestone(a, ra, parts = 5)
  expect_identical(sort(list.files(ra, recursive = TRUE)),
                   sort(c("meta/ms5.out/MOS2E39230594.gt3x.RData",
                          "meta/ms5.outraw/sib.reports/sib_report_MOS2E39230594gt3x_T5A5.csv")))
  # no sib report: the series and the legend are still written
  b <- tu; b$settings$params[["do.sibreport"]] <- FALSE
  rb <- x5_tmp("nosibreport")
  write.ggir.milestone(b, rb, parts = 5)
  expect_false(dir.exists(file.path(rb, "meta/ms5.outraw/sib.reports")))
  expect_true(file.exists(file.path(rb, "meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")))
  # neither: meta/ms5.outraw is not created
  cc <- tu
  cc$settings$params[["save_ms5rawlevels"]] <- FALSE
  cc$settings$params[["do.sibreport"]] <- FALSE
  rc <- x5_tmp("neither")
  write.ggir.milestone(cc, rc, parts = 5)
  expect_false(dir.exists(file.path(rc, "meta/ms5.outraw")))
  expect_identical(list.files(rc, recursive = TRUE), "meta/ms5.out/MOS2E39230594.gt3x.RData")
})

test_that("an empty day table writes no ms5 milestone, as GGIR's own gate does not", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  tu <- x5_timeuse("MOS2", metadir)$tu
  empty <- tu
  empty$daysummary <- tu$daysummary[0, ]
  root <- x5_tmp("emptyday")
  w <- write.ggir.milestone(empty, root, parts = 5)
  expect_identical(length(dir(file.path(root, "meta/ms5.out"))), 0L)
  expect_false("part5" %in% names(w))
  # the same when the boutdur.mvpa column, GGIR's own boundary marker, is renamed away
  noboundary <- tu
  names(noboundary$daysummary)[names(noboundary$daysummary) == "boutdur.mvpa"] <- "zzz"
  root2 <- x5_tmp("noboundary")
  w2 <- write.ggir.milestone(noboundary, root2, parts = 5)
  expect_identical(length(dir(file.path(root2, "meta/ms5.out"))), 0L)
  expect_false("part5" %in% names(w2))
})

test_that("the reports and the two dictionaries are written where GGIR writes them", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  refres <- x5_ref("out", "output_din", "results")
  skip_if_not(dir.exists(metadir))
  tu <- x5_timeuse("MOS2", metadir)$tu
  rd <- function(f) as.data.frame(data.table::fread(f))
  reports <- list()
  for (win in c("MM", "WW")) {
    sfx <- paste0(win, "_L40M100V400_T5A5")
    reports[[sfx]] <- list(
      daysummary_full = rd(file.path(refres, "QC", paste0("part5_daysummary_full_", sfx, ".csv"))),
      daysummary_cleaned = rd(file.path(refres, paste0("part5_daysummary_", sfx, ".csv"))),
      personsummary = rd(file.path(refres, paste0("part5_personsummary_", sfx, ".csv"))))
  }
  root <- x5_tmp("reports")
  w <- write.ggir.milestone(tu, root, parts = 5, reports = reports)
  expect_identical(sort(list.files(file.path(root, "results"), recursive = TRUE)),
                   sort(c("part5_daysummary_MM_L40M100V400_T5A5.csv",
                          "part5_daysummary_WW_L40M100V400_T5A5.csv",
                          "part5_personsummary_MM_L40M100V400_T5A5.csv",
                          "part5_personsummary_WW_L40M100V400_T5A5.csv",
                          "QC/part5_daysummary_full_MM_L40M100V400_T5A5.csv",
                          "QC/part5_daysummary_full_WW_L40M100V400_T5A5.csv",
                          "variableDictionary/part5_dictionary_daysummary.csv",
                          "variableDictionary/part5_dictionary_personsummary.csv")))
  expect_true(all(c("part5_report", "part5_dictionary") %in% names(w)))
  # all eight, line for line
  for (f in sort(list.files(file.path(root, "results"), recursive = TRUE))) {
    a <- file.path(root, "results", f)
    b <- file.path(refres, f)
    expect_true(x5_lines_equal(a, b), info = x5_first_diff(a, b))
  }
  # a configuration may also be identified by five members instead of by its list name
  reports2 <- list(list(window = "MM", TRLi = 40, TRMi = 100, TRVi = 400,
                        sleepparam = "T5A5",
                        daysummary_full = reports[[1]]$daysummary_full,
                        daysummary_cleaned = NULL, personsummary = NULL))
  root2 <- x5_tmp("reports_members")
  write.ggir.milestone(tu, root2, parts = 5, reports = reports2)
  expect_true(file.exists(file.path(root2, "results", "QC",
                                    "part5_daysummary_full_MM_L40M100V400_T5A5.csv")))
  expect_error(write.ggir.milestone(tu, x5_tmp("reports_bad"), parts = 5,
                                    reports = list(list(daysummary_full = reports[[1]]$daysummary_full))),
               "report suffix GGIR would use")
})

test_that("P7m with no cleaned report the dictionary falls back to results/QC", {
  x5_skip_ref()
  x5_skip_ggir()
  metadir <- x5_ref("out", "output_din", "meta")
  refres <- x5_ref("out", "output_din", "results")
  skip_if_not(dir.exists(metadir))
  tu <- x5_timeuse("MOS2", metadir)$tu
  rd <- function(f) as.data.frame(data.table::fread(f))
  reports <- list()
  for (win in c("MM", "WW")) {
    sfx <- paste0(win, "_L40M100V400_T5A5")
    reports[[sfx]] <- list(
      daysummary_full = rd(file.path(refres, "QC", paste0("part5_daysummary_full_", sfx, ".csv"))),
      daysummary_cleaned = NULL, personsummary = NULL)
  }
  root <- x5_tmp("qcfallback")
  write.ggir.milestone(tu, root, parts = 5, reports = reports)
  # both QC files map to the same output name, so one file is written and the last wins
  expect_identical(dir(file.path(root, "results", "variableDictionary")),
                   "part5_dictionary_daysummary_full.csv")
  mine <- file.path(root, "results/variableDictionary/part5_dictionary_daysummary_full.csv")
  expect_identical(length(readLines(mine)), 118L)
  # against GGIR's own function over the same two QC files
  root2 <- x5_tmp("qcfallback_ggir")
  dir.create(file.path(root2, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  for (win in c("MM", "WW")) {
    f <- paste0("part5_daysummary_full_", win, "_L40M100V400_T5A5.csv")
    file.copy(file.path(refres, "QC", f), file.path(root2, "results", "QC", f))
  }
  GGIR:::g.report.part5_dictionary(metadatadir = normalizePath(root2, winslash = "/"),
                                   params_output = GGIR::load_params()$params_output)
  ggirfile <- file.path(root2, "results/variableDictionary/part5_dictionary_daysummary_full.csv")
  expect_true(file.exists(ggirfile))
  expect_true(x5_lines_equal(mine, ggirfile), info = x5_first_diff(mine, ggirfile))
})

test_that("dictionary_per_report describes each report with its own columns", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  refres <- x5_ref("out", "output_din", "results")
  skip_if_not(dir.exists(metadir))
  tu <- x5_timeuse("MOS2", metadir)$tu
  rd <- function(f) as.data.frame(data.table::fread(f))
  reports <- list()
  for (win in c("MM", "WW")) {
    sfx <- paste0(win, "_L40M100V400_T5A5")
    reports[[sfx]] <- list(
      daysummary_full = rd(file.path(refres, "QC", paste0("part5_daysummary_full_", sfx, ".csv"))),
      daysummary_cleaned = rd(file.path(refres, paste0("part5_daysummary_", sfx, ".csv"))),
      personsummary = rd(file.path(refres, paste0("part5_personsummary_", sfx, ".csv"))))
  }
  root <- x5_tmp("perreport")
  write.ggir.milestone(tu, root, parts = 5, reports = reports, dictionary_per_report = TRUE)
  expect_identical(sort(dir(file.path(root, "results", "variableDictionary"))),
                   sort(c("part5_dictionary_daysummary_MM_L40M100V400_T5A5.csv",
                          "part5_dictionary_daysummary_WW_L40M100V400_T5A5.csv",
                          "part5_dictionary_personsummary_MM_L40M100V400_T5A5.csv",
                          "part5_dictionary_personsummary_WW_L40M100V400_T5A5.csv")))
  vd <- function(f) read.csv(file.path(root, "results", "variableDictionary", f),
                             stringsAsFactors = FALSE)
  mm <- vd("part5_dictionary_personsummary_MM_L40M100V400_T5A5.csv")
  ww <- vd("part5_dictionary_personsummary_WW_L40M100V400_T5A5.csv")
  expect_identical(nrow(mm), 207L)
  expect_identical(nrow(ww), 206L)
  # the WW dictionary names WW's column, not MM's
  expect_true("ACC_spt_wake_MOD_mg_pla" %in% mm$Variable)
  expect_false("ACC_spt_wake_MOD_mg_pla" %in% ww$Variable)
  expect_true("ACC_spt_wake_MOD_mg" %in% ww$Variable)
  # the default keeps GGIR's names
  root2 <- x5_tmp("perreport_off")
  write.ggir.milestone(tu, root2, parts = 5, reports = reports)
  expect_identical(sort(dir(file.path(root2, "results", "variableDictionary"))),
                   c("part5_dictionary_daysummary.csv", "part5_dictionary_personsummary.csv"))
})

test_that("as.ggir.milestone(part = 5) is as.ggir.ms5 and needs no recording", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("MOS2", metadir)
  m <- as.ggir.milestone(NULL, part = 5, timeuse = r$tu)
  expect_identical(names(m), c("output", "tail_expansion_log", "GGIRversion", "last_timestamp"))
  expect_identical(m, as.ggir.ms5(r$tu))
  # the recording is accepted and ignored
  expect_identical(as.ggir.milestone(r$x, part = 5, timeuse = r$tu), m)
  expect_error(as.ggir.milestone(r$x, part = 5), "pass timeuse = raw.timeuse")
  expect_error(as.ggir.milestone(r$x, part = 6), "part must be 1, 2, 3, 4 or 5")
})

test_that("write.ggir.milestone's part-5 arguments are checked", {
  x5_skip_ref()
  metadir <- x5_ref("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  r <- x5_timeuse("MOS2", metadir)
  out <- x5_tmp("argcheck")
  expect_error(write.ggir.milestone(r$x, out, parts = 6), "parts must be one or more")
  expect_error(write.ggir.milestone(r$x, out, parts = 5), "timeuse = raw.timeuse")
  expect_error(write.ggir.milestone(r$x, out, parts = 1:5, nights = r$nights),
               "timeuse = raw.timeuse")
  expect_error(write.ggir.milestone(r$tu, out, parts = 5, dictionary_per_report = NA),
               "TRUE or FALSE")
  expect_error(write.ggir.milestone(r$tu, out, parts = 5, reports = "no"),
               "list of configurations")
  expect_error(write.ggir.milestone(r$tu, out, parts = 5, reports = list("no")),
               "must be a list of daysummary_full")
  expect_error(write.ggir.milestone(r$x, out, parts = 1:4, nights = r$nights,
                                    reports = list()),
               "belong to part 5")
  expect_error(write.ggir.milestone(r$x, out, parts = 1, dictionary_per_report = TRUE),
               "belong to part 5")
  # the time-use object may stand in for x when part 5 is the only part asked for
  w <- write.ggir.milestone(r$tu, out, parts = 5)
  expect_true(file.exists(file.path(out, "meta/ms5.out/MOS2E39230594.gt3x.RData")))
  expect_type(w, "character")
  # but not when a recording is needed as well
  expect_error(write.ggir.milestone(r$tu, x5_tmp("argcheck2"), parts = c(3, 5), timeuse = r$tu),
               "canhrActi_raw object")
  bad <- r$tu
  bad$settings$filename <- NA_character_
  bad$settings$filename_dir <- NA_character_
  expect_error(write.ggir.milestone(bad, x5_tmp("argcheck3"), parts = 5),
               "carries no file name")
})

test_that("P8a the whole chain: read the .gt3x, write parts 1 to 5, let GGIR report it", {
  x5_skip_ref()
  x5_skip_ggir()
  if (!canhr_flag("CANHRACTI_GGIR_SLOW")) {
    skip("set CANHRACTI_GGIR_SLOW=1 to run the full chain from the raw file (about 65 s)")
  }
  gt3x <- x5_ref("din", "MOS2E39230594.gt3x")
  skip_if_not(file.exists(gt3x))
  x <- read.raw.accelerometer(gt3x, params = raw.params(), sleep = TRUE)
  if (is.null(x$imputed)) x$imputed <- raw.impute(x)
  if (is.null(x$sleep)) x$sleep <- raw.sleep.part3(x)
  nights <- raw.sleep.nights(x$sleep)
  tu <- raw.timeuse(x, nights, ggir_version_label = "3.3.6")
  root <- x5_tmp("fullchain")
  w <- write.ggir.milestone(x, root, parts = 1:5, nights = nights, timeuse = tu)
  expect_identical(sort(x5_undate(list.files(root, recursive = TRUE))),
                   sort(c("meta/basic/meta_MOS2E39230594.gt3x.RData",
                          "meta/ms2.out/MOS2E39230594.gt3x.RData",
                          "meta/ms3.out/MOS2E39230594.gt3x.RData",
                          "meta/ms4.out/MOS2E39230594.gt3x.RData",
                          "meta/ms5.out/MOS2E39230594.gt3x.RData",
                          "meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData",
                          "meta/ms5.outraw/behavioralcodes<date>.csv",
                          "meta/ms5.outraw/sib.reports/sib_report_MOS2E39230594gt3x_T5A5.csv")))
  expect_identical(names(w)[1:4], c("part1", "part2", "part3", "part4"))
  metadir <- x5_ref("out", "output_din", "meta")
  a5 <- x5_loadenv(file.path(root, "meta/ms5.out/MOS2E39230594.gt3x.RData"))
  b5 <- x5_loadenv(file.path(metadir, "ms5.out/MOS2E39230594.gt3x.RData"))
  expect_identical(a5$output, b5$output)
  expect_identical(a5$last_timestamp, b5$last_timestamp)
  expect_identical(a5$GGIRversion, b5$GGIRversion)
  ar <- x5_loadenv(file.path(root, "meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData"))
  br <- x5_loadenv(file.path(metadir, "ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData"))
  expect_identical(ar$mdat, br$mdat)
  expect_identical(ar$Lnames, br$Lnames)
  expect_identical(ar$desiredtz, br$desiredtz)
  rn <- x5_ggir_report(root)
  refres <- x5_ref("out", "output_din", "results")
  fs <- sort(list.files(file.path(rn, "results"), recursive = TRUE))
  expect_identical(length(fs), 8L)
  for (f in fs) {
    a <- file.path(rn, "results", f)
    b <- file.path(refres, f)
    expect_true(x5_lines_equal(a, b), info = x5_first_diff(a, b))
  }
})
