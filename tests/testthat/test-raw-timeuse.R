# Parity tests for raw.timeuse() and as.ggir.ms5() against GGIR part 5's g.part5, driven
# from the stored part-1 to part-4 milestones; the full chain from the .gt3x runs once when
# CANHRACTI_GGIR_SLOW is set. Reference data are found through CANHRACTI_GGIR_REF and the
# fixtures through CANHRACTI_GGIR_P5FIX and CANHRACTI_GGIR_P34FIX; every block skips without
# them, and the live comparisons skip when GGIR is not installed.

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
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)

tu_sibling <- function(env, folder, name) {
  base <- Sys.getenv(env, unset = "")
  if (base == "") {
    if (.ggir_ref == "") return("")
    base <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref, winslash = "/",
                                                           mustWork = FALSE))),
                      folder, "fixtures")
  }
  file.path(base, name)
}
tu_p5fix <- function(name) tu_sibling("CANHRACTI_GGIR_P5FIX", "ggir-study-p5", name)
tu_p34fix <- function(name) tu_sibling("CANHRACTI_GGIR_P34FIX", "ggir-study-p34", name)

tu_loadenv <- function(path) {
  e <- new.env()
  load(path, envir = e)
  e
}

# The canhrActi_raw and the night table part 5 reads, from the stored milestones.
tu_build <- function(metadir) {
  fn <- dir(file.path(metadir, "ms3.out"))
  if (length(fn) == 0) testthat::skip(paste0("no ms3 milestone under ", metadir))
  fn <- fn[1]
  x <- read.ggir.milestone(file.path(metadir, "basic", paste0("meta_", fn)))
  ms2 <- tu_loadenv(file.path(metadir, "ms2.out", fn))
  ms3 <- tu_loadenv(file.path(metadir, "ms3.out", fn))
  ms4 <- tu_loadenv(file.path(metadir, "ms4.out", fn))
  x$imputed <- ms2$IMP
  x$sleep <- list(sib.cla.sum = ms3$sib.cla.sum, SPTE_start = ms3$SPTE_start,
                  SPTE_end = ms3$SPTE_end, longitudinal_axis = ms3$longitudinal_axis,
                  tail_expansion_log = ms3$tail_expansion_log,
                  rec_starttime = ms3$rec_starttime, id = ms3$ID)
  list(x = x, nights = ms4$nightsummary, fn = fn, metadir = metadir)
}

# the stored ms5 objects of a run, or NULL when GGIR wrote no file
tu_stored_ms5 <- function(metadir, fn) {
  p <- file.path(metadir, "ms5.out", fn)
  if (!file.exists(p)) return(NULL)
  tu_loadenv(p)
}

# the stored mdat of one threshold triple
tu_stored_mdat <- function(metadir, triple = "40_100_400") {
  d <- file.path(metadir, "ms5.outraw", triple)
  if (!dir.exists(d)) return(NULL)
  f <- dir(d, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  tu_loadenv(f[1])
}

# the frame GGIR writes: the last timewindow's, since savetimeseries runs after the loop
tu_ggir_series <- function(tu, triple = "40_100_400", sibDef = NULL) {
  s <- tu$series[[triple]]
  if (is.null(s)) return(NULL)
  s <- if (is.null(sibDef)) s[[1]] else s[[sibDef]]
  s[[length(s)]]
}

# report the first differing cell rather than only "not identical"
tu_first_diff <- function(a, b) {
  if (!identical(dim(a), dim(b))) return(paste0("dim ", paste(dim(a), collapse = "x"),
                                                " vs ", paste(dim(b), collapse = "x")))
  if (!identical(colnames(a), colnames(b))) {
    return(paste0("names differ: only mine ",
                  paste(setdiff(colnames(a), colnames(b)), collapse = ","),
                  " | only reference ",
                  paste(setdiff(colnames(b), colnames(a)), collapse = ",")))
  }
  for (k in colnames(a)) {
    if (!identical(a[[k]], b[[k]])) {
      d <- which(a[[k]] != b[[k]] | is.na(a[[k]]) != is.na(b[[k]]))
      if (length(d) == 0) d <- 1
      return(paste0("column ", k, ", row ", d[1], ": mine ", a[[k]][d[1]],
                    " vs reference ", b[[k]][d[1]], " (", length(d), " cells differ)"))
    }
  }
  "attributes differ"
}

TU_CASES <- list(
  MOS2 = list(out = c("out", "output_din"), nrow = 7L, tz = ""),
  EE   = list(out = c("timing_out", "output_timing"), nrow = 11L, tz = "Europe/Helsinki"))

tu_cache <- new.env(parent = emptyenv())
tu_run <- function(key, metadir, ...) {
  if (!is.null(tu_cache[[key]])) return(tu_cache[[key]])
  S <- tu_build(metadir)
  tu <- raw.timeuse(S$x, S$nights, params = raw.params(ggir_version_label = "3.3.6", ...))
  tu_cache[[key]] <- list(S = S, tu = tu)
  tu_cache[[key]]
}

test_that("raw.timeuse reproduces the stored ms5 output of both reference recordings", {
  skip_if_no_ggir_ref()
  for (nm in names(TU_CASES)) {
    cs <- TU_CASES[[nm]]
    metadir <- ref_file(cs$out[1], cs$out[2], "meta")
    if (!dir.exists(metadir)) next
    R <- tu_run(paste0("base_", nm), metadir)
    tu <- R$tu
    stored <- tu_stored_ms5(metadir, R$S$fn)
    expect_false(is.null(stored))
    expect_identical(tu$status$state, "ok")
    expect_identical(dim(tu$daysummary), c(cs$nrow, 119L))
    expect_identical(dim(stored$output), c(cs$nrow, 119L))
    expect_true(identical(tu$daysummary, stored$output),
                info = paste0(nm, ": ", tu_first_diff(tu$daysummary, stored$output)))
    expect_true(all(vapply(tu$daysummary, is.character, logical(1))))
    # the timezone part 1 recorded wins over the parameter
    expect_identical(tu$settings$desiredtz, cs$tz)
  }
})

test_that("as.ggir.ms5 returns GGIR's four ms5.out objects, identical to the stored ones", {
  skip_if_no_ggir_ref()
  for (nm in names(TU_CASES)) {
    cs <- TU_CASES[[nm]]
    metadir <- ref_file(cs$out[1], cs$out[2], "meta")
    if (!dir.exists(metadir)) next
    R <- tu_run(paste0("base_", nm), metadir)
    g <- as.ggir.ms5(R$tu)
    stored <- tu_stored_ms5(metadir, R$S$fn)
    expect_identical(names(g), c("output", "tail_expansion_log", "GGIRversion",
                                 "last_timestamp"))
    expect_true(identical(g$output, stored$output))
    expect_identical(g$last_timestamp, stored$last_timestamp)
    expect_identical(g$GGIRversion, stored$GGIRversion)
    expect_identical(g$tail_expansion_log, stored$tail_expansion_log)
    # tail_expansion_log is NULL on both recordings and the member must survive
    expect_true("tail_expansion_log" %in% names(g))
    expect_null(g$tail_expansion_log)
  }
  expect_error(as.ggir.ms5(list()), "canhrActi_raw_timeuse")
})

test_that("the stored MOS2 numbers the spec quotes come back exactly", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  o <- tu_run("base_MOS2", metadir)$tu$daysummary
  mm <- o[o$window == "MM", ]
  ww <- o[o$window == "WW", ]
  expect_identical(mm$calendar_date, c("2025-10-07", "2025-10-08", "2025-10-09",
                                       "2025-10-10"))
  expect_identical(ww$start_end_window, c("07:10:45-07:45:55", "07:46:00-06:58:15",
                                          "06:58:20-23:14:50"))
  # the reference minutes at 40/100/400 mg
  expect_identical(as.numeric(mm$dur_day_min[2:4]), c(980, 960.083333333333,
                                                      948.833333333333))
  expect_identical(as.numeric(mm$dur_day_total_IN_min[2:4]),
                   c(797.666666666667, 808.083333333333, 700.166666666667))
  expect_identical(round(as.numeric(mm$ACC_day_mg[2:4]), 3), c(23.772, 21.145, 39.567))
  expect_true(all(as.numeric(o$dur_day_MVPA_bts_10_min) == 0))
  # the 24 hour accounting closes on every row
  spt <- paste0("dur_", c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD",
                          "spt_wake_VIG"), "_min")
  day <- paste0("dur_", c("day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt",
                          "day_MVPA_bts_10", "day_MVPA_bts_5_10", "day_MVPA_bts_1_5",
                          "day_IN_bts_30", "day_IN_bts_20_30", "day_IN_bts_10_20",
                          "day_LIG_bts_10", "day_LIG_bts_5_10", "day_LIG_bts_1_5"), "_min")
  tot <- paste0("dur_day_total_", c("IN", "LIG", "MOD", "VIG"), "_min")
  num <- function(k) as.numeric(o[[k]])
  ssum <- rowSums(vapply(spt, num, numeric(nrow(o))))
  dsum <- rowSums(vapply(day, num, numeric(nrow(o))))
  tsum <- rowSums(vapply(tot, num, numeric(nrow(o))))
  expect_lt(max(abs(ssum - num("dur_spt_min"))), 1e-9)
  expect_lt(max(abs(dsum - num("dur_day_min"))), 1e-9)
  expect_lt(max(abs(tsum - num("dur_day_min"))), 1e-9)
  expect_lt(max(abs(ssum + dsum - num("dur_day_spt_min"))), 1e-9)
  # total and unbouted do not nest
  r <- which(o$window == "MM" & o$calendar_date == "2025-10-08")
  inb <- num("dur_day_IN_unbt_min")[r] + num("dur_day_IN_bts_30_min")[r] +
    num("dur_day_IN_bts_20_30_min")[r] + num("dur_day_IN_bts_10_20_min")[r]
  expect_equal(round(inb, 3), 826.5)
  expect_equal(round(num("dur_day_total_IN_min")[r], 3), 797.667)
  # NaN is stored as the literal string and a real NA as ""
  m <- as.matrix(o)
  expect_identical(sum(m == "NaN"), 42L)
  expect_identical(sum(m == ""), 2L)
  expect_false(any(is.na(m)))
  # the quantile columns are populated on every row; the 3.3-9 gate is not ported
  expect_true(all(o$quantile_mostactive60min_mg != ""))
  expect_true(all(o$quantile_mostactive30min_mg != ""))
  # no lux column on a .gt3x
  expect_false(any(grepl("^LUX_", colnames(o))))
})

test_that("the EE day table carries the spec's window lengths", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("timing_out", "output_timing", "meta")
  skip_if_not(dir.exists(metadir))
  o <- tu_run("base_EE", metadir)$tu$daysummary
  expect_identical(as.numeric(o$dur_day_spt_min[o$window == "MM"]),
                   c(885, 1440, 1440, 1440, 1440, 1440))
  expect_identical(as.numeric(o$dur_day_spt_min[o$window == "WW"]),
                   c(1612.41666666667, 1407.5, 1326.08333333333, 1569.41666666667, 1291))
})

test_that("raw.timeuse reproduces the stored exported time series under identical()", {
  skip_if_no_ggir_ref()
  for (nm in names(TU_CASES)) {
    cs <- TU_CASES[[nm]]
    metadir <- ref_file(cs$out[1], cs$out[2], "meta")
    if (!dir.exists(metadir)) next
    R <- tu_run(paste0("base_", nm), metadir)
    stored <- tu_stored_mdat(metadir)
    expect_false(is.null(stored))
    mine <- tu_ggir_series(R$tu)
    expect_true(identical(mine, stored$mdat),
                info = paste0(nm, ": ", tu_first_diff(mine, stored$mdat)))
    expect_identical(names(mine),
                     c("timenum", "ACC", "SleepPeriodTime", "invalidepoch", "guider",
                       "window", "sibdetection", "selfreported", "angle", "class_id",
                       "invalid_fullwindow", "invalid_sleepperiod", "invalid_wakinghours",
                       "timestamp"))
    # Lnames and the timezone travel with the frame
    expect_identical(R$tu$levels, stored$Lnames)
    expect_identical(sort(ls(stored)), c("desiredtz", "filename", "Lnames", "mdat"))
    expect_identical(stored$desiredtz, cs$tz)
    expect_identical(R$tu$settings$desiredtz, cs$tz)
  }
})

test_that("the exported series mixes the last timewindow's windows with every pass's guider", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  R <- tu_run("base_MOS2", metadir)
  mdat <- tu_ggir_series(R$tu)
  # the window column is the WW numbering and the guider column is the MM pass, because
  # savetimeseries runs after the timewindow loop
  expect_identical(as.integer(table(mdat$window)), c(72670L, 17703L, 16708L, 11719L))
  expect_identical(as.integer(table(mdat$guider)), c(64440L, 54360L))
  # the three invalid columns are 100 outside every window and 0 inside
  expect_identical(which(names(mdat) %in% c("invalid_fullwindow", "invalid_sleepperiod",
                                            "invalid_wakinghours")), 11:13)
  out <- mdat$window == 0
  expect_true(all(mdat$invalid_fullwindow[out] == 100))
  expect_true(all(mdat$invalid_sleepperiod[out] == 100))
  expect_true(all(mdat$invalid_wakinghours[out] == 100))
  expect_true(all(mdat$invalid_fullwindow[!out] == 0))
  # the per-timewindow MM frame differs from GGIR's only in the four columns computed from window
  s <- R$tu$series[["40_100_400"]][[1]]
  expect_identical(names(s), c("MM", "WW"))
  diffcols <- names(s$MM)[!vapply(names(s$MM),
                                  function(k) identical(s$MM[[k]], s$WW[[k]]), logical(1))]
  expect_identical(sort(diffcols), sort(c("window", "invalid_fullwindow",
                                          "invalid_sleepperiod", "invalid_wakinghours")))
  expect_identical(s$WW, mdat)
})

test_that("the sib report and the class legend come back with the series", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  R <- tu_run("base_MOS2", metadir)
  expect_identical(names(R$tu$sibreport), "T5A5")
  expect_identical(nrow(R$tu$sibreport[["T5A5"]]), 66L)
  expect_true(all(R$tu$sibreport[["T5A5"]]$type == "sib"))
  csv <- file.path(metadir, "ms5.outraw", "sib.reports",
                   "sib_report_MOS2E39230594gt3x_T5A5.csv")
  if (file.exists(csv)) {
    stored <- utils::read.csv(csv, stringsAsFactors = FALSE)
    expect_identical(dim(R$tu$sibreport[["T5A5"]]), dim(stored))
  }
  expect_identical(R$tu$legend$class_name, R$tu$levels)
  expect_identical(R$tu$legend$class_id, 0:17)
})

test_that("the full chain from the raw .gt3x reproduces the stored ms5 and mdat", {
  skip_if_no_ggir_ref()
  if (!canhr_flag("CANHRACTI_GGIR_SLOW")) {
    skip("set CANHRACTI_GGIR_SLOW=1 to run the full chain from the raw file (about 70 s)")
  }
  gt3x <- ref_file("din", "MOS2E39230594.gt3x")
  skip_if_not(file.exists(gt3x))
  x <- read.raw.accelerometer(gt3x, desiredtz = "")
  x$imputed <- raw.impute(x)
  x$sleep <- raw.sleep.part3(x)
  nights <- raw.sleep.nights(x)
  tu <- raw.timeuse(x, nights, params = raw.params(ggir_version_label = "3.3.6"))
  metadir <- ref_file("out", "output_din", "meta")
  stored <- tu_stored_ms5(metadir, "MOS2E39230594.gt3x.RData")
  g <- as.ggir.ms5(tu)
  expect_true(identical(g$output, stored$output),
              info = tu_first_diff(g$output, stored$output))
  expect_identical(g$last_timestamp, stored$last_timestamp)
  expect_true(identical(tu_ggir_series(tu), tu_stored_mdat(metadir)$mdat))
})

test_that("the threshold grid stacks four triples into one table and four series", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tu_p5fix("thresholds"), "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tu_build(metadir)
  tu <- raw.timeuse(S$x, S$nights,
                    params = raw.params(ggir_version_label = "3.3.6",
                                        threshold.lig = c(30, 40),
                                        threshold.mod = c(100, 125),
                                        threshold.vig = 400))
  stored <- tu_stored_ms5(metadir, S$fn)
  expect_identical(dim(tu$daysummary), c(28L, 119L))
  expect_true(identical(tu$daysummary, stored$output),
              info = tu_first_diff(tu$daysummary, stored$output))
  expect_identical(names(tu$series), c("30_100_400", "30_125_400", "40_100_400",
                                       "40_125_400"))
  for (k in names(tu$series)) {
    sk <- tu_stored_mdat(metadir, k)
    expect_false(is.null(sk))
    expect_true(identical(tu_ggir_series(tu, k), sk$mdat),
                info = paste0(k, ": ", tu_first_diff(tu_ggir_series(tu, k), sk$mdat)))
  }
  # the 40/100/400 subset is the single-threshold reference run
  refdir <- ref_file("out", "output_din", "meta")
  if (dir.exists(refdir)) {
    single <- tu_stored_ms5(refdir, S$fn)$output
    sub <- tu$daysummary[tu$daysummary$TRLi == "40" & tu$daysummary$TRMi == "100", ]
    row.names(sub) <- NULL
    expect_true(identical(sub, single), info = tu_first_diff(sub, single))
  }
  # the minutes move with the thresholds and still sum to the same waking day
  mm2 <- tu$daysummary[tu$daysummary$window == "MM" & tu$daysummary$window_number == "2", ]
  expect_identical(as.numeric(mm2$dur_day_total_IN_min),
                   c(762.833333333333, 762.833333333333, 797.666666666667,
                     797.666666666667))
  expect_identical(as.numeric(mm2$dur_day_total_LIG_min),
                   c(165.916666666667, 185.25, 131.083333333333, 150.416666666667))
  expect_identical(as.numeric(mm2$dur_day_total_MOD_min),
                   c(49, 29.6666666666667, 49, 29.6666666666667))
  expect_true(all(as.numeric(mm2$dur_day_total_VIG_min) == 2.25))
  expect_true(all(abs(as.numeric(mm2$dur_day_total_IN_min) +
                        as.numeric(mm2$dur_day_total_LIG_min) +
                        as.numeric(mm2$dur_day_total_MOD_min) +
                        as.numeric(mm2$dur_day_total_VIG_min) - 980) < 1e-9))
  # one Lnames for all four triples, so levels stays a character vector
  expect_true(is.character(tu$levels))
})

tu_fixture_case <- function(metadir, pars = list(), label = "") {
  S <- tu_build(metadir)
  stored <- tu_stored_ms5(metadir, S$fn)
  tu <- do.call(raw.timeuse,
                list(S$x, S$nights,
                     params = do.call(raw.params,
                                      c(list(ggir_version_label = "3.3.6"), pars))))
  expect_identical(tu$status$state, "ok")
  expect_true(identical(tu$daysummary, stored$output),
              info = paste0(label, ": ", tu_first_diff(tu$daysummary, stored$output)))
  expect_identical(tu$last_timestamp, stored$last_timestamp)
  rr <- file.path(metadir, "ms5.outraw")
  if (dir.exists(rr)) {
    for (k in setdiff(dir(rr), "sib.reports")) {
      if (!dir.exists(file.path(rr, k))) next
      sk <- tu_stored_mdat(metadir, k)
      if (is.null(sk)) next
      mine <- tu_ggir_series(tu, k)
      expect_true(identical(mine, sk$mdat),
                  info = paste0(label, " ", k, ": ", tu_first_diff(mine, sk$mdat)))
    }
  }
  tu
}

test_that("the part-5 fixtures reproduce their stored ms5 objects", {
  skip_if_no_ggir_ref()
  cases <- list(
    excludefirst   = list(dir = "excludefirst", pars = list(), rows = 7L),
    missingnight   = list(dir = "missingnight", pars = list(), rows = 7L),
    qwindow        = list(dir = "qwindow", pars = list(qwindow = c(0, 8, 24)), rows = 15L),
    lastnight      = list(dir = "lastnight",
                          pars = list(require_complete_lastnight_part5 = TRUE), rows = 8L),
    lastnight_MM   = list(dir = "lastnight_MM",
                          pars = list(require_complete_lastnight_part5 = TRUE,
                                      timewindow = "MM"), rows = 5L),
    lastnight_ctrl = list(dir = "lastnight_ctrl", pars = list(), rows = 8L),
    weekend        = list(dir = "weekend", pars = list(), rows = 7L),
    weekend_agg    = list(dir = "weekend_agg",
                          pars = list(week_weekend_aggregate.part5 = TRUE), rows = 7L))
  ran <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    metadir <- file.path(tu_p5fix(cs$dir), "output_din", "meta")
    if (!dir.exists(metadir)) next
    tu <- tu_fixture_case(metadir, cs$pars, nm)
    expect_identical(nrow(tu$daysummary), cs$rows)
    ran <- ran + 1L
  }
  if (ran == 0L) skip("no part-5 fixture folder found")
  expect_gt(ran, 0L)
})

test_that("the qwindow fixture emits segment rows and one empty segment", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tu_p5fix("qwindow"), "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tu_build(metadir)
  tu <- raw.timeuse(S$x, S$nights,
                    params = raw.params(ggir_version_label = "3.3.6",
                                        qwindow = c(0, 8, 24)))
  o <- tu$daysummary
  expect_identical(nrow(o), 15L)
  expect_identical(sort(unique(o$window)), c("MM", "segment1", "segment2", "WW"))
  # 3.3.6 keeps the qwindow block inside the MM branch, so WW rows carry no segment
  expect_identical(sum(o$window %in% c("segment1", "segment2")), 8L)
  expect_identical(unique(o$start_end_window[o$window == "WW"]),
                   c("07:10:45-07:45:55", "07:46:00-06:58:15", "06:58:20-23:14:50"))
  # MM window 1 starts at 20:30, so its segment1 is empty and the row stops after TRVi
  blank <- o[o$window == "segment1" & o$window_number == "1", ]
  expect_identical(nrow(blank), 1L)
  expect_identical(blank$dur_day_spt_min, "")
  expect_identical(blank$TRVi, "400")
  expect_true(nzchar(blank$ID) && nzchar(blank$filename))
  # with segmentation on, the full-window label becomes the real clock edges
  expect_identical(o$start_end_window[o$window == "MM" & o$window_number == "1"],
                   "20:30:00-23:59:55")
})

test_that("the parts 3 and 4 fixtures that reach part 5 reproduce their stored ms5", {
  skip_if_no_ggir_ref()
  # those runs were made with save_ms5raw_without_invalid TRUE
  pars <- list(save_ms5raw_without_invalid = TRUE)
  cases <- list(spring = 11L, autumn = 11L, autumn_edge = 11L, daysleeper = 6L,
                novalid_keepsibs = 14L)
  ran <- 0L
  for (nm in names(cases)) {
    metadir <- file.path(tu_p34fix(nm), "output_din", "meta")
    if (!dir.exists(metadir)) next
    if (length(dir(file.path(metadir, "ms4.out"))) == 0) next
    tu <- tu_fixture_case(metadir, pars, nm)
    expect_identical(nrow(tu$daysummary), cases[[nm]])
    ran <- ran + 1L
  }
  if (ran == 0L) skip("no parts 3 and 4 fixture folder found")
  # a DST day is a real 23 or 25 hour window
  for (nm in c("spring", "autumn")) {
    metadir <- file.path(tu_p34fix(nm), "output_din", "meta")
    if (!dir.exists(metadir)) next
    S <- tu_build(metadir)
    tu <- raw.timeuse(S$x, S$nights, params = raw.params(ggir_version_label = "3.3.6"))
    mm <- tu$daysummary[tu$daysummary$window == "MM", ]
    expect_identical(as.numeric(mm$dur_day_spt_min[6]),
                     if (nm == "spring") 1380 else 1500)
  }
})

test_that("novalid has no part-4 milestone, and daysleeper carries the wrong night's metadata", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tu_p34fix("novalid"), "output_din", "meta")
  if (dir.exists(metadir)) {
    # GGIR skips a recording with no ms4 entry: meta/ms5.out is empty
    expect_identical(length(dir(file.path(metadir, "ms4.out"))), 0L)
    expect_identical(length(dir(file.path(metadir, "ms5.out"))), 0L)
  }
  metadir <- file.path(tu_p34fix("daysleeper"), "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tu_build(metadir)
  tu <- raw.timeuse(S$x, S$nights, params = raw.params(ggir_version_label = "3.3.6"))
  o <- tu$daysummary
  expect_identical(nrow(o), 6L)
  expect_identical(o$start_end_window[o$window == "WW"][1], "13:10:45-13:45:55")
  # MM row 1 carries night 2's metadata with night 1's sleep period: the pass-throughs are
  # looked up by calendar date
  mm1 <- o[o$window == "MM", ][1, ]
  expect_identical(mm1$night_number, "2")
  expect_identical(mm1$cleaningcode, "1")
  expect_identical(round(as.numeric(mm1$sleeponset), 5), 28.98611)
  expect_identical(round(as.numeric(mm1$wakeup), 5), 37.17917)
  expect_identical(round(as.numeric(mm1$dur_spt_min), 5), 491.58333)
})

test_that("the diary fixtures reproduce their stored ms5, WW back-fill included", {
  skip_if_no_ggir_ref()
  p34diary <- file.path(tu_p34fix("diary"), "slog.csv")
  cases <- list(
    diary_spt = list(dir = "diary_spt", log = p34diary, swt = "SPT"),
    diary_tib = list(dir = "diary_tib", log = p34diary, swt = "TimeInBed"),
    diary_gap = list(dir = "diary_gap", log = file.path(tu_p5fix("diary_gap"),
                                                        "slog_gap.csv"), swt = "SPT"))
  ran <- 0L
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    metadir <- file.path(tu_p5fix(cs$dir), "output_din", "meta")
    if (!dir.exists(metadir) || !file.exists(cs$log)) next
    tu <- tu_fixture_case(metadir, list(loglocation = cs$log,
                                        sleepwindowType = cs$swt), nm)
    expect_identical(nrow(tu$daysummary), 7L)
    if (nm == "diary_gap") {
      # a WW row with sleeplog_used "0" forces the previous WW row to "0" too, since a WW
      # window depends on both the night that opens it and the one that closes it
      expect_identical(tu$daysummary$sleeplog_used, c("1", "1", "0", "1", "0", "0", "1"))
      expect_identical(tu$daysummary$guider[5], "sleeplog")
    } else {
      expect_true(all(tu$daysummary$sleeplog_used == "1"))
      expect_true(all(tu$daysummary$guider == "sleeplog"))
    }
    ran <- ran + 1L
  }
  if (ran == 0L) skip("no diary fixture found")
  expect_gt(ran, 0L)
})

test_that("the windows table records every window attempt and the days GGIR drops", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  w <- tu_run("base_MOS2", metadir)$tu$windows
  expect_true(is.data.frame(w))
  expect_identical(names(w), c("sleepparam", "TRLi", "TRMi", "TRVi", "window",
                               "window_number", "start_index", "end_index", "start_time",
                               "end_time", "calendar_date", "n_segments", "n_rows",
                               "lastDay", "state", "reason"))
  mm <- w[w$window == "MM" & w$state == "analysed", ]
  # the MM qqq pairs and the lastDay flag
  expect_identical(mm$start_index, c(1L, 2521L, 19801L, 37081L))
  expect_identical(mm$end_index, c(2520L, 19800L, 37080L, 54360L))
  expect_identical(mm$lastDay, c(FALSE, FALSE, FALSE, TRUE))
  # the WW pairs and the c(NA, NA) tail
  ww <- w[w$window == "WW" & w$state == "analysed", ]
  expect_identical(ww$start_index, c(7690L, 25393L, 42101L))
  expect_identical(ww$end_index, c(25392L, 42100L, 53819L))
  na <- w[w$window == "WW" & w$state == "no_window", ]
  expect_identical(nrow(na), 1L)
  expect_true(all(is.na(na$start_index)))
  # the MM twelve-hour trim drops 2025-10-12 to 10-14, and the window loop stops before 10-11
  trimmed <- w[w$window == "MM" & w$state == "midnight_trimmed", ]
  expect_identical(as.character(trimmed$calendar_date),
                   c("2025-10-12", "2025-10-13", "2025-10-14"))
  expect_true(all(grepl("twelve hours", trimmed$reason)))
  tail <- w[w$window == "MM" & w$state == "not_analysed", ]
  expect_identical(nrow(tail), 1L)
  expect_identical(tail$start_index, 54361L)
  expect_identical(tail$end_index, 118800L)
  expect_identical(as.character(tail$calendar_date), "2025-10-11")
  expect_false(any(tu_run("base_MOS2", metadir)$tu$daysummary$calendar_date %in%
                     c("2025-10-11", "2025-10-12", "2025-10-13", "2025-10-14")))
})

# A private copy of the reference milestones for a live GGIR::g.part5 run.
tu_live_dir <- function(name) {
  d <- file.path(tempdir(), paste0("canhrActi_p5_", name))
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "meta"), recursive = TRUE, showWarnings = FALSE)
  src <- ref_file("out", "output_din", "meta")
  for (sub in c("basic", "ms2.out", "ms3.out", "ms4.out")) {
    file.copy(file.path(src, sub), file.path(d, "meta"), recursive = TRUE)
  }
  normalizePath(d, winslash = "/")
}

tu_live_run <- function(d, po = list(), pg = list(), p247 = list(), pph = list(),
                        datadir = c()) {
  p <- GGIR:::load_params()
  mod <- function(base, over) {
    for (nm in names(over)) base[nm] <- list(over[[nm]])
    base
  }
  base_po <- list(save_ms5rawlevels = TRUE, save_ms5raw_format = "RData",
                  save_ms5raw_without_invalid = FALSE, do.sibreport = TRUE,
                  visualreport = FALSE)
  GGIR::g.part5(datadir = datadir, metadatadir = d, f0 = 1, f1 = 1,
                params_sleep = p$params_sleep, params_metrics = p$params_metrics,
                params_247 = mod(p$params_247, p247),
                params_phyact = mod(p$params_phyact, pph),
                params_cleaning = p$params_cleaning,
                params_output = mod(mod(p$params_output, base_po), po),
                params_general = mod(p$params_general, pg), verbose = FALSE)
  f <- dir(file.path(d, "meta/ms5.out"), full.names = TRUE)
  if (length(f) == 0) return(NULL)
  tu_loadenv(f[1])
}

test_that("eight configurations no stored object covers reproduce a live GGIR run", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_not(dir.exists(ref_file("out", "output_din", "meta")))
  scen <- list(
    OO = list(po = list(timewindow = "OO"), mine = list(timewindow = "OO"), ncol = 119L),
    agg60 = list(pg = list(part5_agg2_60seconds = TRUE),
                 mine = list(part5_agg2_60seconds = TRUE), ncol = 119L),
    dayborder4 = list(pg = list(dayborder = 4), mine = list(dayborder = 4), ncol = 119L),
    frag_all = list(pph = list(frag.metrics = "all"),
                    mine = list(frag.metrics = "all"), ncol = 149L),
    iglevels = list(p247 = list(iglevels = 1), mine = list(iglevels = 1), ncol = 125L),
    no_sibreport = list(po = list(do.sibreport = FALSE),
                        mine = list(do.sibreport = FALSE), ncol = 119L),
    without_invalid = list(po = list(save_ms5raw_without_invalid = TRUE),
                           mine = list(save_ms5raw_without_invalid = TRUE), ncol = 119L))
  for (nm in names(scen)) {
    s <- scen[[nm]]
    d <- tu_live_dir(nm)
    g <- suppressWarnings(tu_live_run(d,
                                      po = if (is.null(s$po)) list() else s$po,
                                      pg = if (is.null(s$pg)) list() else s$pg,
                                      p247 = if (is.null(s$p247)) list() else s$p247,
                                      pph = if (is.null(s$pph)) list() else s$pph))
    S <- tu_build(file.path(d, "meta"))
    tu <- suppressWarnings(
      do.call(raw.timeuse,
              list(S$x, S$nights,
                   params = do.call(raw.params,
                                    c(list(ggir_version_label = "3.3.6"), s$mine)))))
    expect_identical(ncol(tu$daysummary), s$ncol)
    expect_true(identical(tu$daysummary, g$output),
                info = paste0(nm, ": ", tu_first_diff(tu$daysummary, g$output)))
    expect_identical(tu$last_timestamp, g$last_timestamp)
    stored <- tu_stored_mdat(file.path(d, "meta"))
    if (!is.null(stored)) {
      mine <- tu_ggir_series(tu)
      expect_true(identical(mine, stored$mdat),
                  info = paste0(nm, " mdat: ", tu_first_diff(mine, stored$mdat)))
    }
    unlink(d, recursive = TRUE)
  }
  # storefolderstructure needs a datadir, because GGIR scans it for the raw file
  din <- ref_file("din")
  if (dir.exists(din)) {
    d <- tu_live_dir("sfs")
    g <- tu_live_run(d, po = list(storefolderstructure = TRUE), datadir = din)
    S <- tu_build(file.path(d, "meta"))
    S$x$file$path <- file.path(din, "MOS2E39230594.gt3x")
    tu <- raw.timeuse(S$x, S$nights,
                      params = raw.params(ggir_version_label = "3.3.6",
                                          storefolderstructure = TRUE))
    expect_identical(ncol(tu$daysummary), 121L)
    expect_true(identical(tu$daysummary, g$output),
                info = paste0("sfs: ", tu_first_diff(tu$daysummary, g$output)))
    expect_identical(unique(tu$daysummary$foldername), "din")
    unlink(d, recursive = TRUE)
  }
})

test_that("two sib definitions reproduce a live GGIR run, with and without 60 s epochs", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_not(dir.exists(ref_file("out", "output_din", "meta")))
  for (agg in c(FALSE, TRUE)) {
    d <- tu_live_dir(paste0("twosib", agg))
    fn <- dir(file.path(d, "meta/ms3.out"))[1]
    e3 <- new.env(); load(file.path(d, "meta/ms3.out", fn), envir = e3)
    s2 <- e3$sib.cla.sum; s2$definition <- "T10A5"
    e3$sib.cla.sum <- rbind(e3$sib.cla.sum, s2)
    save(list = ls(e3), envir = e3, file = file.path(d, "meta/ms3.out", fn))
    e4 <- new.env(); load(file.path(d, "meta/ms4.out", fn), envir = e4)
    n2 <- e4$nightsummary; n2$sleepparam <- "T10A5"
    e4$nightsummary <- rbind(e4$nightsummary, n2)
    save(list = ls(e4), envir = e4, file = file.path(d, "meta/ms4.out", fn))
    g <- tu_live_run(d, pg = list(part5_agg2_60seconds = agg))
    S <- tu_build(file.path(d, "meta"))
    calls <- 0L
    tu <- raw.timeuse(S$x, S$nights,
                      progress = function(stage, i, n, message) {
                        calls <<- calls + 1L
                        expect_identical(stage, "timeuse")
                        expect_identical(n, 4L)
                      },
                      params = raw.params(ggir_version_label = "3.3.6",
                                          part5_agg2_60seconds = agg))
    expect_identical(dim(tu$daysummary), c(14L, 119L))
    expect_true(identical(tu$daysummary, g$output),
                info = paste0("two sib defs agg=", agg, ": ",
                              tu_first_diff(tu$daysummary, g$output)))
    expect_identical(tu$last_timestamp, g$last_timestamp)
    expect_identical(calls, 4L)
    expect_identical(names(tu$series[["40_100_400"]]), c("T5A5", "T10A5"))
    expect_identical(names(tu$sibreport), c("T5A5", "T10A5"))
    for (sd in c("T5A5", "T10A5")) {
      f <- file.path(d, "meta/ms5.outraw/40_100_400",
                     paste0("MOS2E39230594_", sd, ".RData"))
      if (!file.exists(f)) next
      rw <- tu_loadenv(f)
      mine <- tu_ggir_series(tu, "40_100_400", sd)
      expect_true(identical(mine, rw$mdat),
                  info = paste0(sd, " agg=", agg, ": ", tu_first_diff(mine, rw$mdat)))
    }
    # the 60 s branch restores the 5 s series for the second sib definition, so neither
    # frame is aggregated twice
    if (agg) expect_identical(nrow(tu_ggir_series(tu, "40_100_400", "T10A5")), 9900L)
    unlink(d, recursive = TRUE)
  }
})

test_that("the failure modes return a state and an empty day table with the right columns", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tu_build(metadir)
  P <- function(...) raw.params(ggir_version_label = "3.3.6", ...)
  expect_empty_day <- function(tu, state) {
    expect_identical(tu$status$state, state)
    expect_identical(nrow(tu$daysummary), 0L)
    expect_identical(ncol(tu$daysummary), 119L)
    expect_true(all(vapply(tu$daysummary, is.character, logical(1))))
    expect_identical(colnames(tu$daysummary)[c(1, 2, 119)],
                     c("ID", "filename", "GGIRversion"))
    expect_null(tu$series)
    expect_gt(length(tu$status$messages), 0L)
    expect_false(is.na(tu$elapsed))
  }
  # no part-4 night at all
  expect_empty_day(raw.timeuse(S$x, S$nights[0, ], params = P()), "no_nights")
  # only the night-0 placeholder, which the impossible-row cut removes
  n2 <- S$nights[1, ]
  n2$sleeponset <- 0; n2$wakeup <- 0; n2$SptDuration <- 0
  expect_empty_day(raw.timeuse(S$x, n2, params = P()), "no_valid_night")
  # part 1 marked the file corrupt or too short: GGIR writes nothing and says nothing
  xc <- S$x; xc$meta$filecorrupt <- TRUE
  expect_empty_day(raw.timeuse(xc, S$nights, params = P()), "skipped")
  xs <- S$x; xs$meta$filetooshort <- TRUE
  expect_empty_day(raw.timeuse(xs, S$nights, params = P()), "skipped")
  # part 3 found no sustained inactivity bout, so there is no definition to loop over
  xn <- S$x; xn$sleep$sib.cla.sum <- S$x$sleep$sib.cla.sum[0, ]
  expect_empty_day(raw.timeuse(xn, S$nights, params = P()), "no_sib_definitions")
  # 280 epochs around one midnight give two MM attempts of 495 s and 895 s, under the 900 s gate
  xt <- S$x
  keep <- 2421:2700
  xt$imputed$metashort <- xt$imputed$metashort[keep, ]
  xt$imputed$rout <- xt$imputed$rout[1:2, ]
  xt$meta$metashort <- xt$meta$metashort[keep, ]
  xt$meta$metalong <- xt$meta$metalong[1:2, ]
  tu <- raw.timeuse(xt, S$nights, params = P())
  expect_empty_day(tu, "no_windows")
  short <- tu$windows[tu$windows$state == "too_short", ]
  expect_identical(nrow(short), 2L)
  expect_true(all(grepl("900 s minimum", short$reason)))
  expect_true(any(grepl("495 s", short$reason)))
  expect_true(any(grepl("895 s", short$reason)))
})

test_that("missing inputs raise a named error rather than loading NA", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tu_build(metadir)
  expect_error(raw.timeuse(list(), S$nights), "canhrActi_raw")
  xm <- S$x; xm$imputed <- NULL
  expect_error(raw.timeuse(xm, S$nights), "raw.impute")
  xm <- S$x; xm$sleep <- NULL
  expect_error(raw.timeuse(xm, S$nights), "raw.sleep.part3")
  xm <- S$x; xm$meta <- NULL
  expect_error(raw.timeuse(xm, S$nights), "part-1 epoch tables")
  expect_error(raw.timeuse(S$x, "not a night table"), "canhrActi_raw_nights")
  expect_error(raw.timeuse(S$x, S$nights, progress = "no"), "progress must be")
  expect_error(raw.timeuse(S$x, S$nights, segment_all_windows = NA), "TRUE or FALSE")
  # GGIR accepts an unknown timewindow and then spins the window loop forever
  expect_error(raw.params(timewindow = "ZZ"), "MM")
  expect_error(.raw.timeuse.trim.nights(1:3, data.frame(diur = rep(0, 10)), "ZZ", 5), "MM")
})

test_that("the object carries the settings, the timings and a wall clock", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  tu <- tu_run("base_MOS2", metadir)$tu
  expect_s3_class(tu, "canhrActi_raw_timeuse")
  expect_identical(tu$settings$threshold.lig, 40)
  expect_identical(tu$settings$threshold.mod, 100)
  expect_identical(tu$settings$threshold.vig, 400)
  # the bout durations are sorted descending here, not by the parameter checker
  expect_identical(tu$settings$boutdur.mvpa, c(10, 5, 1))
  expect_identical(tu$settings$boutdur.in, c(30, 20, 10))
  expect_identical(tu$settings$boutdur.lig, c(10, 5, 1))
  expect_identical(raw.params()$boutdur.in, c(10, 20, 30))
  expect_identical(tu$settings$sleepparam, "T5A5")
  expect_identical(tu$settings$timewindow, c("MM", "WW"))
  expect_identical(tu$settings$ggir_version_label, "3.3.6")
  expect_identical(unique(tu$daysummary$GGIRversion), "3.3.6")
  expect_true(is.numeric(tu$elapsed) && tu$elapsed > 0)
  expect_identical(names(tu$status$stage_times),
                   c("series", "sleep", "sibreport", "levels", "windows", "timeseries",
                     "total"))
  expect_true(all(is.finite(tu$status$stage_times)))
  expect_lte(sum(tu$status$stage_times[1:6]), tu$status$stage_times[["total"]] + 1e-6)
  # called by name, so the test does not depend on the NAMESPACE entry
  out <- utils::capture.output(print.canhrActi_raw_timeuse(tu))
  expect_true(any(grepl("time-use analysis", out)))
  expect_true(any(grepl("40 light, 100 moderate, 400 vigorous", out)))
  expect_true(any(grepl("7 rows x 119 columns", out)))
  expect_true(any(grepl("midnight_trimmed", out)))
})

test_that("the fragmentation epoch warning is raised once per call, not once per segment", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  S <- tu_build(metadir)
  w <- testthat::capture_warnings(
    tu <- raw.timeuse(S$x, S$nights,
                      params = raw.params(ggir_version_label = "3.3.6",
                                          frag.metrics = c("mean", "TP"))))
  expect_identical(length(w), 1L)
  expect_match(w, "xmin is 12 epochs")
  expect_true(any(grepl("xmin is 12 epochs", tu$status$messages)))
  # no warning once the series is aggregated to one minute, where xmin is 1
  w2 <- testthat::capture_warnings(
    raw.timeuse(S$x, S$nights,
                params = raw.params(ggir_version_label = "3.3.6",
                                    frag.metrics = c("mean", "TP"),
                                    part5_agg2_60seconds = TRUE)))
  expect_identical(length(w2), 0L)
})

test_that("the behaviour class ladder and the empty day table agree with the reference", {
  skip_if_no_ggir_ref()
  metadir <- ref_file("out", "output_din", "meta")
  skip_if_not(dir.exists(metadir))
  stored <- tu_stored_mdat(metadir)
  skip_if(is.null(stored))
  Ln <- .raw.timeuse.level.names(c(10, 5, 1), c(30, 20, 10), c(10, 5, 1))
  expect_identical(Ln, stored$Lnames)
  expect_identical(.raw.timeuse.default.columns(Ln),
                   colnames(tu_stored_ms5(metadir, dir(file.path(metadir,
                                                                 "ms5.out"))[1])$output))
})

test_that(".raw.timeuse.level.names follows the bout configuration", {
  Ln <- .raw.timeuse.level.names(c(8, 2), 15, c(12, 6, 3))
  expect_identical(length(Ln), 15L)
  expect_identical(Ln[10:15], c("day_MVPA_bts_8", "day_MVPA_bts_2_8", "day_IN_bts_15",
                                "day_LIG_bts_12", "day_LIG_bts_6_12", "day_LIG_bts_3_6"))
  expect_identical(Ln[1:9], c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD",
                              "spt_wake_VIG", "day_IN_unbt", "day_LIG_unbt",
                              "day_MOD_unbt", "day_VIG_unbt"))
  # a single band, and none at all
  expect_identical(.raw.timeuse.level.names(5, 5, 5)[10:12],
                   c("day_MVPA_bts_5", "day_IN_bts_5", "day_LIG_bts_5"))
  expect_identical(length(.raw.timeuse.level.names(numeric(0), numeric(0), numeric(0))), 9L)
  # the default column set grows with the ladder: 18 classes give 119 columns and this
  # 15-class ladder 119 - 3 (dur) - 3 (ACC) - 3 (Nbouts) - 3 (Nblocks) = 107
  expect_identical(length(.raw.timeuse.default.columns(
    .raw.timeuse.level.names())), 119L)
  expect_identical(length(.raw.timeuse.default.columns(Ln)), 107L)
})

test_that(".raw.timeuse.merge.names merges in first-written order", {
  # the named-list accumulator replaces GGIR's positional ds_names, so the column order is
  # assembled here
  expect_identical(.raw.timeuse.merge.names(character(0), c("a", "b")), c("a", "b"))
  expect_identical(.raw.timeuse.merge.names(c("a", "b"), c("a", "b")), c("a", "b"))
  # a short row is a strict prefix: nothing moves
  expect_identical(.raw.timeuse.merge.names(c("a", "b", "c"), c("a", "b")),
                   c("a", "b", "c"))
  expect_identical(.raw.timeuse.merge.names(c("a", "b"), c("a", "b", "c")),
                   c("a", "b", "c"))
  # a ragged middle block lands in the middle, not at the end
  expect_identical(.raw.timeuse.merge.names(c("a", "b", "z"), c("a", "b", "x", "y", "z")),
                   c("a", "b", "x", "y", "z"))
  # a run of new names in the middle goes before the next shared name, not at the front
  expect_identical(.raw.timeuse.merge.names(c("a", "b", "c"), c("x", "b")),
                   c("a", "x", "b", "c"))
  # a wholly new row appends in its own order
  expect_identical(.raw.timeuse.merge.names(c("a"), c("p", "q")), c("a", "p", "q"))
})

test_that(".raw.timeuse.bind reproduces GGIR's character frame and its row names", {
  rows <- list(list(ID = "x", filename = "f.RData", a = 1.5, b = NaN, c = NA_real_),
               list(ID = "x", filename = "f.RData", a = 2, b = 3, c = 4))
  nm <- .raw.timeuse.merge.names(character(0), names(rows[[1]]))
  out <- .raw.timeuse.bind(rows, nm)
  expect_true(all(vapply(out, is.character, logical(1))))
  expect_identical(out$a, c("1.5", "2"))
  # NaN keeps the literal string and a real NA becomes ""
  expect_identical(out$b, c("NaN", "3"))
  expect_identical(out$c, c("", "4"))
  # the row names are the ones data.frame(matrix) makes
  ref <- data.frame(matrix("", 2, 5), stringsAsFactors = FALSE)
  expect_identical(attr(out, "row.names"), attr(ref, "row.names"))
  # a cell no row wrote is "", not NA
  wide <- .raw.timeuse.bind(rows, c(nm, "d"))
  expect_identical(wide$d, c("", ""))
  # GGIR's empty-row drop
  rows2 <- c(rows, list(list(ID = "", filename = "")))
  expect_identical(nrow(.raw.timeuse.bind(rows2, nm)), 2L)
  expect_identical(nrow(.raw.timeuse.bind(list(), nm)), 0L)
})

test_that(".raw.timeuse.aggregate60 aggregates the way GGIR does", {
  tz <- "UTC"
  n <- 24L # two minutes of 5 s epochs
  t0 <- as.POSIXct("2025-10-08 00:00:00", tz = tz)
  ts <- data.frame(time = t0 + seq(0, by = 5, length.out = n),
                   ACC = as.numeric(seq_len(n)),
                   sibdetection = rep(c(0, 1), each = n / 2),
                   diur = c(rep(0, n / 2), rep(c(1, 0), each = n / 4)),
                   nonwear = rep(0, n),
                   angle = as.numeric(seq_len(n)),
                   guider = rep(c("HDCZA", "unknown"), each = n / 2),
                   step_count = rep(1, n),
                   stringsAsFactors = FALSE)
  ag <- .raw.timeuse.aggregate60(ts, desiredtz = tz, dayborder = 0)
  expect_identical(ag$ws3new, 60)
  expect_identical(nrow(ag$ts), 2L)
  expect_identical(ag$ts$ACC, c(mean(1:12), mean(13:24)))
  # the flags are averaged and then rounded back to 0 and 1
  expect_identical(ag$ts$sibdetection, c(0, 1))
  expect_identical(ag$ts$diur, c(0, 0))
  # guider is the first value of the minute, step_count the sum
  expect_identical(ag$ts$guider, c("HDCZA", "unknown"))
  expect_identical(ag$ts$step_count, c(12, 12))
  # columns the aggregation does not name are dropped
  ts2 <- ts; ts2$window <- 1; ts2$selfreported <- "nap"
  expect_false(any(c("window", "selfreported") %in%
                     names(.raw.timeuse.aggregate60(ts2, tz, 0)$ts)))
  # the midnight grid is recomputed on the aggregated series
  expect_identical(ag$nightsi, 1L)
  expect_identical(ag$Nts, 2L)
  expect_identical(ag$time_POSIX, ag$ts$time)
})

test_that("the windows table is well formed even when nothing was analysed", {
  w <- .raw.timeuse.empty.windows()
  expect_identical(nrow(w), 0L)
  expect_identical(ncol(w), 16L)
  expect_identical(nrow(.raw.timeuse.window.table(list())), 0L)
  one <- .raw.timeuse.window.row("T5A5", 40, 100, 400, "MM", 1L, 1L, 10L,
                                 as.POSIXct("2025-10-08 00:00:00", tz = "UTC") +
                                   seq(0, by = 5, length.out = 10),
                                 FALSE, 1L, 1L, "analysed", "", tz = "UTC")
  expect_identical(nrow(one), 1L)
  expect_identical(as.character(one$calendar_date), "2025-10-08")
  expect_identical(nrow(.raw.timeuse.window.table(list(one, one))), 2L)
})
