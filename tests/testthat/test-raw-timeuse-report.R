# Parity tests for raw.timeuse.report() against GGIR 3.3.6's g.report.part5: the csv files
# GGIR wrote for the reference recordings and the part-5 fixtures, compared byte for byte
# after the same fwrite call, and a live g.report.part5 over the same milestones. Reference
# data are found through CANHRACTI_GGIR_REF and the fixtures through CANHRACTI_GGIR_P5FIX;
# every block skips without them, and the live comparisons skip when GGIR is not installed.

withr::local_locale(c(LC_TIME = "C"))
# GGIR wrote the reference outputs in an America/Anchorage session
withr::local_timezone("America/Anchorage")

.ggir_ref_r5 <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

r5_skip_ref <- function() {
  if (.ggir_ref_r5 == "" || !dir.exists(.ggir_ref_r5)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
r5_skip_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
r5_skip_dt <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    testthat::skip("data.table is not installed")
  }
}
r5_ref <- function(...) file.path(sub("/+$", "", .ggir_ref_r5), ...)
r5_p5fix <- function(name) {
  base <- Sys.getenv("CANHRACTI_GGIR_P5FIX", unset = "")
  if (base == "") {
    if (.ggir_ref_r5 == "") return("")
    base <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref_r5, winslash = "/",
                                                           mustWork = FALSE))),
                      "ggir-study-p5", "fixtures")
  }
  file.path(base, name)
}
# The 3.3-9 clone's R folder: CANHRACTI_GGIR_SRC, else ggir-src beside the reference folder
r5_clone <- function() {
  src <- Sys.getenv("CANHRACTI_GGIR_SRC", unset = "")
  if (src == "" && .ggir_ref_r5 != "") {
    src <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref_r5, winslash = "/",
                                                          mustWork = FALSE))),
                     "ggir-src", "GGIR", "R")
  }
  src
}
MOS2_OUT5 <- function() r5_ref("out", "output_din")
EE_OUT5 <- function() r5_ref("timing_out", "output_timing")

r5_skip_dir <- function(d) {
  if (d == "" || !dir.exists(file.path(d, "meta", "ms5.out"))) {
    testthat::skip(paste0("no meta/ms5.out under ", d))
  }
}

# Every ms5 milestone of one output folder, in the shape raw.timeuse.report() accepts.
r5_load <- function(dir) {
  fn <- list.files(file.path(dir, "meta", "ms5.out"), full.names = TRUE)
  lapply(fn, function(f) {
    e <- new.env()
    load(f, envir = e)
    list(output = e$output, last_timestamp = e$last_timestamp,
         tail_expansion_log = e$tail_expansion_log, label = basename(f))
  })
}

# The export layer's own fwrite call, so the comparison is with the csv the package writes.
r5_write <- function(df) {
  p <- tempfile(fileext = ".csv")
  data.table::fwrite(df, p, row.names = FALSE, na = "", sep = ",", dec = ".")
  p
}
r5_same_csv <- function(df, path) {
  if (is.null(df)) return("frame is NULL")
  a <- readLines(r5_write(df))
  b <- readLines(path)
  if (identical(a, b)) return(TRUE)
  n <- min(length(a), length(b))
  k <- which(a[seq_len(n)] != b[seq_len(n)])
  if (length(k) == 0) {
    return(paste0(basename(path), ": same first ", n, " lines, lengths ", length(a),
                  " and ", length(b)))
  }
  paste0(basename(path), " line ", k[1], "\n  mine: ", substr(a[k[1]], 1, 260),
         "\n  ref : ", substr(b[k[1]], 1, 260))
}

# Every configuration of one report against the csv files GGIR left in that folder.
r5_expect_folder <- function(dir, params = raw.params(), label = basename(dir)) {
  items <- r5_load(dir)
  rep <- suppressWarnings(raw.timeuse.report(items, params = params))
  seen <- character()
  for (k in names(rep)) {
    en <- rep[[k]]
    trio <- list(
      list("daysummary_full",
           file.path(dir, "results", "QC", paste0("part5_daysummary_full_", k, ".csv"))),
      list("daysummary_cleaned",
           file.path(dir, "results", paste0("part5_daysummary_", k, ".csv"))),
      list("personsummary",
           file.path(dir, "results", paste0("part5_personsummary_", k, ".csv"))))
    for (t in trio) {
      df <- en[[t[[1]]]]
      p <- t[[2]]
      if (!file.exists(p)) {
        # GGIR writes no cleaned day csv and no person csv when no window is valid.
        expect_null(df, info = paste(label, k, t[[1]], "reference absent"))
        next
      }
      seen <- c(seen, normalizePath(p, winslash = "/"))
      expect_true(isTRUE(r5_same_csv(df, p)),
                  info = paste0(label, " / ", k, " / ", t[[1]], ": ",
                                r5_same_csv(df, p)))
    }
  }
  allfiles <- c(list.files(file.path(dir, "results"), pattern = "^part5_", full.names = TRUE),
                list.files(file.path(dir, "results", "QC"), pattern = "^part5_",
                           full.names = TRUE))
  expect_identical(setdiff(normalizePath(allfiles, winslash = "/"), seen), character(0))
  rep
}

# raw.timeuse is slow, so its one object is built once.
.r5_cache <- new.env(parent = emptyenv())
r5_timeuse <- function(metadir) {
  key <- paste0("tu_", metadir)
  if (!is.null(.r5_cache[[key]])) return(.r5_cache[[key]])
  fn <- dir(file.path(metadir, "meta", "ms3.out"))
  if (length(fn) == 0) testthat::skip(paste0("no ms3 milestone under ", metadir))
  fn <- fn[1]
  md <- file.path(metadir, "meta")
  x <- read.ggir.milestone(file.path(md, "basic", paste0("meta_", fn)))
  ld <- function(p) { e <- new.env(); load(p, envir = e); e }
  ms2 <- ld(file.path(md, "ms2.out", fn))
  ms3 <- ld(file.path(md, "ms3.out", fn))
  ms4 <- ld(file.path(md, "ms4.out", fn))
  x$imputed <- ms2$IMP
  x$sleep <- list(sib.cla.sum = ms3$sib.cla.sum, SPTE_start = ms3$SPTE_start,
                  SPTE_end = ms3$SPTE_end, longitudinal_axis = ms3$longitudinal_axis,
                  tail_expansion_log = ms3$tail_expansion_log,
                  rec_starttime = ms3$rec_starttime, id = ms3$ID)
  tu <- suppressWarnings(raw.timeuse(x, ms4$nightsummary, ggir_version_label = "3.3.6"))
  out <- list(tu = tu, x = x, nights = ms4$nightsummary, fn = fn)
  .r5_cache[[key]] <- out
  out
}

num5 <- function(x) as.numeric(as.character(x))

test_that("P7a: MOS2 reproduces all six part-5 csv files byte for byte", {
  r5_skip_ref()
  r5_skip_dt()
  r5_skip_dir(MOS2_OUT5())
  rep <- r5_expect_folder(MOS2_OUT5(), label = "MOS2")
  expect_identical(names(rep), c("MM_L40M100V400_T5A5", "WW_L40M100V400_T5A5"))
  mm <- rep[["MM_L40M100V400_T5A5"]]
  ww <- rep[["WW_L40M100V400_T5A5"]]
  expect_identical(dim(mm$daysummary_cleaned), c(3L, 115L))
  expect_identical(dim(ww$daysummary_cleaned), c(3L, 115L))
  expect_identical(dim(mm$personsummary), c(1L, 207L))
  expect_identical(dim(ww$personsummary), c(1L, 206L))
  expect_identical(dim(mm$daysummary_full), c(4L, 117L))
  expect_identical(dim(ww$daysummary_full), c(3L, 117L))
  expect_s3_class(rep, "canhrActi_raw_timeuse_report")
  expect_identical(attr(rep, "canhrActi")$status$state, "ok")
})

test_that("P7b: EE reproduces all six, at 5x115, 5x115, 1x208, 1x208, 6x117 and 5x117", {
  r5_skip_ref()
  r5_skip_dt()
  r5_skip_dir(EE_OUT5())
  rep <- r5_expect_folder(EE_OUT5(), label = "EE")
  mm <- rep[["MM_L40M100V400_T5A5"]]
  ww <- rep[["WW_L40M100V400_T5A5"]]
  expect_identical(dim(mm$daysummary_cleaned), c(5L, 115L))
  expect_identical(dim(ww$daysummary_cleaned), c(5L, 115L))
  expect_identical(dim(mm$personsummary), c(1L, 208L))
  expect_identical(dim(ww$personsummary), c(1L, 208L))
  expect_identical(dim(mm$daysummary_full), c(6L, 117L))
  expect_identical(dim(ww$daysummary_full), c(5L, 117L))
  # EE keeps a Saturday and a Sunday, so its weekend half is populated
  expect_identical(num5(mm$personsummary$Nvaliddays), 5)
  expect_identical(num5(mm$personsummary$Nvaliddays_WD), 3)
  expect_identical(num5(mm$personsummary$Nvaliddays_WE), 2)
  expect_true(all(c("Saturday", "Sunday") %in% mm$daysummary_cleaned$weekday))
})

test_that("P7d: the column accounting reaches 117, 115 and 206 by GGIR's own route", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  items <- r5_load(MOS2_OUT5())
  rep <- raw.timeuse.report(items, params = raw.params())
  ww <- rep[["WW_L40M100V400_T5A5"]]
  ms5cols <- names(items[[1]]$output)
  expect_identical(length(ms5cols), 119L)                 # 119 in the milestone
  # +2 lastHour and lastDate, +1 daytype, -5 identifiers = 117
  expect_identical(ncol(ww$daysummary_full), 117L)
  expect_identical(setdiff(ms5cols, names(ww$daysummary_full)),
                   c("window", "sleepparam", "TRLi", "TRMi", "TRVi"))
  expect_identical(setdiff(names(ww$daysummary_full), ms5cols),
                   c("lastHour", "lastDate", "daytype"))
  expect_identical(ncol(ww$daysummary_cleaned), 115L)
  # of the 117: 1 filename, 91 numcol and 25 charcol
  expect_identical(length(ww$numcol), 91L)
  expect_identical(length(ww$charcol), 25L)
  expect_identical(1L + length(ww$numcol) + length(ww$charcol), 117L)
  # 91*2 + 25 + filename = 208, +1 len = 209, -2 len and daytype = 207
  expect_identical(2L * 91L + 25L + 1L + 1L - 2L, 207L)
  # +13 counts = 220, -11 cut = 209, -3 lastHour_pla/_wei and lastDate = 206
  expect_identical(207L + 13L - 11L - 3L, 206L)
  expect_identical(ncol(ww$personsummary), 206L)
  counts <- c("Nvaliddays", "Nvaliddays_WD", "Nvaliddays_WE", "Ndaysleeper",
              "Ncleaningcodezero", paste0("Ncleaningcode", 1:6), "Nsleeplog_used",
              "Nacc_available")
  expect_identical(length(counts), 13L)
  expect_true(all(counts %in% names(ww$personsummary)))
  expect_identical(names(ww$personsummary)[1:17],
                   c("filename", "ID", "startday", "calendar_date", counts))
  expect_identical(grep("lastHour|lastDate", names(ww$personsummary), value = TRUE),
                   character(0))
  expect_identical(setdiff(names(ww$daysummary_full), names(ww$daysummary_cleaned)),
                   c("lastHour", "lastDate"))
})

test_that("P7e: the charcol list is exactly the 25 names on WW and those minus one on MM", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  rep <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  ww <- rep[["WW_L40M100V400_T5A5"]]
  mm <- rep[["MM_L40M100V400_T5A5"]]
  expected_ww <- c("ID", "weekday", "calendar_date", "start_end_window",
                   "sleeponset_ts", "wakeup_ts",
                   "night_number", "daysleeper", "cleaningcode", "guider", "acc_available",
                   "ACC_spt_wake_MOD_mg", "ACC_spt_wake_VIG_mg", "ACC_day_MVPA_bts_10_mg",
                   "ACC_day_MVPA_bts_5_10_mg", "ACC_day_LIG_bts_10_mg",
                   "boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                   "boutdur.in", "boutdur.lig", "boutdur.mvpa",
                   "GGIRversion", "lastDate", "daytype")
  expect_identical(ww$charcol, expected_ww)
  expect_identical(mm$charcol, setdiff(expected_ww, "ACC_spt_wake_MOD_mg"))
  # dur_spt_wake_MOD_min is 0 on all three WW days and non-zero on one MM day
  expect_identical(num5(ww$daysummary_cleaned$dur_spt_wake_MOD_min), c(0, 0, 0))
  expect_true(any(num5(mm$daysummary_full$dur_spt_wake_MOD_min) > 0))
  expect_true("ACC_spt_wake_MOD_mg_pla" %in% names(mm$personsummary))
  expect_true("ACC_spt_wake_MOD_mg_wei" %in% names(mm$personsummary))
  expect_true("ACC_spt_wake_MOD_mg" %in% names(ww$personsummary))
  expect_false("ACC_spt_wake_MOD_mg_pla" %in% names(ww$personsummary))
})

test_that("P7g: lastHour is the local clock hour and lastDate is the UTC date", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  items <- r5_load(MOS2_OUT5())
  lt <- items[[1]]$last_timestamp
  expect_identical(format(lt, "%Y-%m-%d %H:%M:%S"), "2025-10-14 17:29:55")
  rep <- raw.timeuse.report(items, params = raw.params())
  full <- rep[["WW_L40M100V400_T5A5"]]$daysummary_full
  expect_identical(unique(num5(full$lastHour)), 17)
  expect_identical(unique(as.character(full$lastDate)), "2025-10-15")
  # 17:29 AKDT is 01:29 the next day in UTC
  expect_identical(as.character(as.Date(lt)), "2025-10-15")
  expect_identical(format(lt, "%H"), "17")
})

test_that("P7j: the one dropped MM window is window 1 and the reason is minimum_MM_length", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  rep <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  mm <- rep[["MM_L40M100V400_T5A5"]]
  w <- mm$windows
  expect_identical(dim(w), c(4L, 10L))
  expect_identical(w$valid, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(w$reason, c("minimum_MM_length", "", "", ""))
  expect_identical(w$window_number, c(1, 2, 3, 4))
  expect_identical(w$calendar_date,
                   c("2025-10-07", "2025-10-08", "2025-10-09", "2025-10-10"))
  expect_identical(mm$validdaysi_day, 2:4)
  expect_identical(mm$validdaysi_person, 2:4)
  # every other criterion passes on that row
  f <- mm$daysummary_full
  expect_identical(num5(f$dur_day_spt_min)[1], 210)
  expect_true(210 < 23 * 60)
  expect_identical(num5(f$nonwear_perc_day)[1], 0)
  expect_identical(100 - num5(f$nonwear_perc_day)[1], 100)
  expect_true(num5(f$dur_spt_min)[1] > 0 && num5(f$dur_day_min)[1] > 0)
  rep3 <- raw.timeuse.report(r5_load(MOS2_OUT5()),
                             params = raw.params(minimum_MM_length.part5 = 3))
  expect_identical(dim(rep3[["MM_L40M100V400_T5A5"]]$daysummary_cleaned), c(4L, 115L))
  expect_true(all(rep3[["MM_L40M100V400_T5A5"]]$windows$valid))
  expect_true(all(rep[["WW_L40M100V400_T5A5"]]$windows$valid))
})

test_that("P7k: the MOS2 WW counts and one hand-computed _pla / _wei pair", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  rep <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  ww <- rep[["WW_L40M100V400_T5A5"]]
  p <- ww$personsummary
  d <- ww$daysummary_cleaned
  expect_identical(num5(p$Nvaliddays), 3)
  expect_identical(num5(p$Nvaliddays_WD), 3)
  expect_identical(num5(p$Nvaliddays_WE), 0)
  expect_identical(num5(p$Ndaysleeper), 0)
  expect_identical(num5(p$Ncleaningcodezero), 0)
  # the three WW nights are part 4's nights 2, 3 and 4, with cleaning codes 1, 1 and 2
  expect_identical(num5(p$Ncleaningcode1), 2)
  expect_identical(num5(p$Ncleaningcode2), 1)
  expect_identical(num5(p$Nacc_available), 3)
  expect_identical(num5(p$Nsleeplog_used), 0)
  # three weekdays, so every weight is 5 and the plain and weighted means coincide
  expect_identical(as.character(d$weekday), c("Wednesday", "Thursday", "Friday"))
  expect_identical(num5(d$dur_day_min), c(980, 960.083, 903.75))
  expect_equal(num5(p$dur_day_min_pla), round(mean(num5(d$dur_day_min)), 3))
  expect_equal(num5(p$dur_day_min_wei),
               round(stats::weighted.mean(num5(d$dur_day_min), w = c(5, 5, 5)), 3))
  expect_identical(num5(p$dur_day_min_pla), num5(p$dur_day_min_wei))
  # plain_mean's x[1] fallback is what carries the non-numeric columns through unchanged
  expect_identical(as.character(p$startday), "Wednesday")
  expect_identical(as.character(p$calendar_date), "2025-10-08")
  expect_identical(as.character(p$GGIRversion), "3.3.6")
})

test_that("the native cut-point totals partition the window and do not nest with GGIR's", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  rep <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  ww <- rep[["WW_L40M100V400_T5A5"]]
  nt <- ww$daysummary_native
  d <- ww$daysummary_cleaned
  expect_identical(setdiff(names(nt), names(d)),
                   c("minutes_waking_cutpoint", "minutes_sedentary_cutpoint",
                     "minutes_light_cutpoint", "minutes_moderate_cutpoint",
                     "minutes_vigorous_cutpoint", "minutes_mvpa_cutpoint",
                     "minutes_sleepperiod_cutpoint", "minutes_sleepperiod_sleep_cutpoint",
                     "minutes_sleepperiod_wake_cutpoint"))
  expect_identical(nt[, names(d)], d)
  cls <- c("dur_spt_sleep_min", "dur_spt_wake_IN_min", "dur_spt_wake_LIG_min",
           "dur_spt_wake_MOD_min", "dur_spt_wake_VIG_min",
           "dur_day_IN_unbt_min", "dur_day_LIG_unbt_min", "dur_day_MOD_unbt_min",
           "dur_day_VIG_unbt_min", "dur_day_MVPA_bts_10_min", "dur_day_MVPA_bts_5_10_min",
           "dur_day_MVPA_bts_1_5_min", "dur_day_IN_bts_30_min", "dur_day_IN_bts_20_30_min",
           "dur_day_IN_bts_10_20_min", "dur_day_LIG_bts_10_min", "dur_day_LIG_bts_5_10_min",
           "dur_day_LIG_bts_1_5_min")
  expect_identical(length(cls), 18L)
  expect_true(all(cls %in% names(d)))
  expect_equal(nt$minutes_sedentary_cutpoint + nt$minutes_light_cutpoint +
                 nt$minutes_moderate_cutpoint + nt$minutes_vigorous_cutpoint,
               nt$minutes_waking_cutpoint)
  expect_equal(nt$minutes_moderate_cutpoint + nt$minutes_vigorous_cutpoint,
               nt$minutes_mvpa_cutpoint)
  expect_equal(nt$minutes_sleepperiod_sleep_cutpoint + nt$minutes_sleepperiod_wake_cutpoint,
               nt$minutes_sleepperiod_cutpoint)
  # the thirteen waking classes are the other partition
  waking13 <- c("dur_day_IN_unbt_min", "dur_day_LIG_unbt_min", "dur_day_MOD_unbt_min",
                "dur_day_VIG_unbt_min", "dur_day_MVPA_bts_10_min",
                "dur_day_MVPA_bts_5_10_min", "dur_day_MVPA_bts_1_5_min",
                "dur_day_IN_bts_30_min", "dur_day_IN_bts_20_30_min",
                "dur_day_IN_bts_10_20_min", "dur_day_LIG_bts_10_min",
                "dur_day_LIG_bts_5_10_min", "dur_day_LIG_bts_1_5_min")
  # each term has already been rounded to three decimals by tidyup_df, so thirteen of them
  # can miss the rounded whole by up to 13 * 0.0005
  expect_lt(max(abs(rowSums(sapply(waking13, function(cn) num5(d[[cn]]))) -
                      num5(d$dur_day_min))), 0.0065)
  # the two partitions must not be mixed: the four IN classes over-count the IN total by 28.833
  in4 <- num5(d$dur_day_IN_unbt_min[1]) + num5(d$dur_day_IN_bts_30_min[1]) +
    num5(d$dur_day_IN_bts_20_30_min[1]) + num5(d$dur_day_IN_bts_10_20_min[1])
  expect_equal(in4, 826.5)
  expect_equal(num5(d$dur_day_total_IN_min[1]), 797.667)
  expect_equal(round(in4 - num5(d$dur_day_total_IN_min[1]), 3), 28.833)
  expect_identical(nt$minutes_sedentary_cutpoint[1], 797.667)
})

test_that("every part-5 fixture reproduces every part5 csv byte for byte", {
  r5_skip_ref()
  r5_skip_dt()
  cases <- list(
    list("excludefirst", list()),
    list("diary_spt", list()),
    list("diary_tib", list()),
    list("diary_gap", list()),
    list("missingnight", list()),
    list("qwindow", list()),
    list("thresholds", list()),
    list("lastnight", list(require_complete_lastnight_part5 = TRUE)),
    list("lastnight_MM", list(require_complete_lastnight_part5 = TRUE)),
    list("lastnight_ctrl", list()),
    list("weekend", list()),
    list("weekend_agg", list(week_weekend_aggregate.part5 = TRUE)))
  ran <- 0L
  for (cs in cases) {
    d <- r5_p5fix(file.path(cs[[1]], "output_din"))
    if (d == "" || !dir.exists(file.path(d, "meta", "ms5.out"))) next
    r5_expect_folder(d, params = do.call(raw.params, cs[[2]]), label = cs[[1]])
    ran <- ran + 1L
  }
  if (ran == 0L) testthat::skip("no part-5 fixture folder found")
  expect_gte(ran, 1L)
})

test_that("F-P5-7 weekend: a retained weekend day gives daytype WE and the 5:2 weighting", {
  r5_skip_ref()
  d <- r5_p5fix(file.path("weekend", "output_din"))
  r5_skip_dir(d)
  rep <- raw.timeuse.report(r5_load(d), params = raw.params())
  mm <- rep[["MM_L40M100V400_T5A5"]]
  ww <- rep[["WW_L40M100V400_T5A5"]]
  expect_identical(as.character(mm$daysummary_full$weekday),
                   c("Thursday", "Friday", "Saturday", "Sunday"))
  expect_identical(as.character(mm$daysummary_full$daytype), c("WD", "WD", "WE", "WE"))
  expect_identical(as.character(mm$daysummary_cleaned$daytype), c("WD", "WE", "WE"))
  expect_identical(num5(mm$personsummary$Nvaliddays), 3)
  expect_identical(num5(mm$personsummary$Nvaliddays_WD), 1)
  expect_identical(num5(mm$personsummary$Nvaliddays_WE), 2)
  expect_identical(num5(ww$personsummary$Nvaliddays_WD), 1)
  expect_identical(num5(ww$personsummary$Nvaliddays_WE), 2)
  # one weekday at weight 5 and two weekend days at 2, so the two means combine 5 to 4
  dd <- mm$daysummary_cleaned
  v <- num5(dd$dur_day_min)
  expect_identical(v, c(980, 960.083, 948.833))
  wd <- v[1]
  we <- mean(v[2:3])
  expect_equal(num5(mm$personsummary$dur_day_min_wei),
               round(stats::weighted.mean(c(wd, we), w = c(5, 2)), 3))
  expect_equal(num5(mm$personsummary$dur_day_min_pla), round(mean(v), 3))
  expect_false(isTRUE(all.equal(num5(mm$personsummary$dur_day_min_wei),
                                num5(mm$personsummary$dur_day_min_pla))))
})

test_that("F-P5-7 weekend_agg: week_weekend_aggregate.part5 widens to 385 and 382", {
  r5_skip_ref()
  d <- r5_p5fix(file.path("weekend_agg", "output_din"))
  r5_skip_dir(d)
  rep <- raw.timeuse.report(r5_load(d),
                            params = raw.params(week_weekend_aggregate.part5 = TRUE))
  expect_identical(dim(rep[["MM_L40M100V400_T5A5"]]$personsummary), c(1L, 385L))
  expect_identical(dim(rep[["WW_L40M100V400_T5A5"]]$personsummary), c(1L, 382L))
  # the width alone does not show the WE half was populated
  p <- rep[["MM_L40M100V400_T5A5"]]$personsummary
  expect_identical(num5(p$Nvaliddays_WE), 2)
  expect_true("dur_day_min_WD" %in% names(p))
  expect_true("dur_day_min_WE" %in% names(p))
  expect_equal(num5(p$dur_day_min_WD), 980)
  expect_equal(num5(p$dur_day_min_WE), round(mean(c(960.083, 948.833)), 3))
  expect_identical(dim(rep[["MM_L40M100V400_T5A5"]]$daysummary_cleaned), c(3L, 115L))
})

test_that("F-P5-8 cohort: two recordings with different column sets are reconciled", {
  r5_skip_ref()
  r5_skip_dt()
  d <- r5_p5fix(file.path("cohort", "output_din"))
  r5_skip_dir(d)
  items <- r5_load(d)
  expect_identical(length(items), 2L)
  # EE is 119 columns wide and MOS2, run with frag.metrics = "all", is 149
  w <- vapply(items, function(z) ncol(z$output), integer(1))
  expect_identical(sort(w), c(119L, 149L))
  expect_identical(length(setdiff(names(items[[which.max(w)]]$output),
                                  names(items[[which.min(w)]]$output))), 30L)
  expect_true(all(grepl("^FRAG_", setdiff(names(items[[which.max(w)]]$output),
                                          names(items[[which.min(w)]]$output)))))
  rep <- r5_expect_folder(d, label = "cohort")
  mm <- rep[["MM_L40M100V400_T5A5"]]
  expect_identical(dim(mm$daysummary_full), c(10L, 147L))
  expect_identical(dim(mm$daysummary_cleaned), c(8L, 145L))
  expect_identical(dim(mm$personsummary), c(2L, 269L))
  expect_identical(dim(rep[["WW_L40M100V400_T5A5"]]$personsummary), c(2L, 269L))
  # one row per recording, EE first because the frame is ordered by filename
  expect_identical(as.character(mm$personsummary$filename),
                   c("EE_left_29.5.2017-05-30.gt3x", "MOS2E39230594.gt3x"))
  expect_identical(num5(mm$personsummary$Nvaliddays), c(5, 3))
  expect_identical(num5(mm$personsummary$Nvaliddays_WD), c(3, 3))
  expect_identical(num5(mm$personsummary$Nvaliddays_WE), c(2, 0))
  expect_identical(num5(mm$personsummary$Ncleaningcode1), c(5, 2))
  expect_identical(num5(mm$personsummary$Nacc_available), c(5, 3))
  expect_identical(as.character(mm$daysummary_cleaned$calendar_date),
                   c("2017-05-24", "2017-05-25", "2017-05-26", "2017-05-27", "2017-05-28",
                     "2025-10-08", "2025-10-09", "2025-10-10"))
  expect_true("FRAG_Nfrag_PA2IN_day" %in% names(mm$daysummary_cleaned))
  expect_true(all(is.na(mm$daysummary_cleaned$FRAG_Nfrag_PA2IN_day[1:5]) |
                    mm$daysummary_cleaned$FRAG_Nfrag_PA2IN_day[1:5] == ""))
})

test_that("F-P5-8 cohort_wide: the expectedCols failure is refused, not reproduced", {
  r5_skip_ref()
  d <- r5_p5fix(file.path("cohort_wide", "output_din"))
  r5_skip_dir(d)
  items <- r5_load(d)
  expect_identical(length(items), 6L)
  expect_identical(vapply(items, function(z) ncol(z$output), integer(1)),
                   c(119L, 119L, 149L, 119L, 119L, 119L))
  # the probe visits 4, 2, 5, 1 and 6, so index 3, the widest, is the one never seen
  set.seed(1234)
  expect_identical(unique(sample(x = 1:6, size = 5)), c(4L, 2L, 5L, 1L, 6L))
  # GGIR warns and then dies in rbind, writing no results
  expect_warning(try(raw.timeuse.report(items, params = raw.params()), silent = TRUE),
                 "Columns dropped in output part5 for R3")
  expect_error(suppressWarnings(raw.timeuse.report(items, params = raw.params())),
               "do not share a column set")
  expect_error(suppressWarnings(raw.timeuse.report(items, params = raw.params())),
               "R3.gt3x.RData has 151 columns")
  expect_identical(list.files(file.path(d, "results"), pattern = "^part5_"), character(0))
})

test_that("F-P5-4 qwindow: the day-segment configuration is reported and named GGIR's way", {
  r5_skip_ref()
  r5_skip_dt()
  d <- r5_p5fix(file.path("qwindow", "output_din"))
  r5_skip_dir(d)
  items <- r5_load(d)
  expect_identical(sort(unique(as.character(items[[1]]$output$window))),
                   c("MM", "segment1", "segment2", "WW"))
  rep <- raw.timeuse.report(items, params = raw.params())
  # 3.3.6 lumps every segment row into one Segments report, which "auto" infers from the names
  expect_identical(names(rep), c("MM_L40M100V400_T5A5", "WW_L40M100V400_T5A5",
                                 "Segments_L40M100V400_T5A5"))
  sg <- rep[["Segments_L40M100V400_T5A5"]]
  # a segment report keeps the window column, so it is one column wider than MM and WW
  expect_identical(dim(sg$daysummary_full), c(8L, 118L))
  expect_identical(dim(sg$daysummary_cleaned), c(3L, 116L))
  expect_identical(dim(sg$personsummary), c(1L, 206L))
  expect_true("window" %in% names(sg$daysummary_full))
  expect_false("window" %in% names(rep[["MM_L40M100V400_T5A5"]]$daysummary_full))
  # segmentDAYSPTcrit.part5[1] = 0.9 drops every segment1 and window 1's segment2
  expect_identical(as.character(sg$daysummary_cleaned$window), rep("segment2", 3))
  expect_identical(sg$windows$valid, c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_true(all(grepl("segmentDAYcrit", sg$windows$reason[!sg$windows$valid])))
  expect_identical(sg$windows$segment, rep(c("segment1", "segment2"), 4))
  expect_identical(sg$windows$reason[1],
                   "nonwear_perc_day_spt, segmentDAYcrit, segmentSPTcrit")
  r5_expect_folder(d, label = "qwindow")
})

test_that("the segment naming follows the version that produced the table", {
  r5_skip_ref()
  d <- r5_p5fix(file.path("qwindow", "output_din"))
  r5_skip_dir(d)
  items <- r5_load(d)
  # 3.3.6 names: one lumped Segments report
  expect_identical(names(raw.timeuse.report(items, params = raw.params())),
                   c("MM_L40M100V400_T5A5", "WW_L40M100V400_T5A5",
                     "Segments_L40M100V400_T5A5"))
  # 3.3-9 names (segment_all_windows = TRUE): one report per parent window type
  it9 <- items
  w <- as.character(it9[[1]]$output$window)
  it9[[1]]$output$window <- ifelse(grepl("^segment", w), paste0("MM", w), w)
  # unique() keeps the order of the window column, so MMsegment lands between MM and WW
  expect_identical(names(raw.timeuse.report(it9, params = raw.params())),
                   c("MM_L40M100V400_T5A5", "MMsegment_L40M100V400_T5A5",
                     "WW_L40M100V400_T5A5"))
  expect_identical(names(raw.timeuse.report(items, params = raw.params(),
                                            segment_naming = "lumped")),
                   c("MM_L40M100V400_T5A5", "WW_L40M100V400_T5A5",
                     "Segments_L40M100V400_T5A5"))
  expect_identical(names(raw.timeuse.report(it9, params = raw.params(),
                                            segment_naming = "lumped")),
                   c("MM_L40M100V400_T5A5", "WW_L40M100V400_T5A5",
                     "Segments_L40M100V400_T5A5"))
  a <- raw.timeuse.report(items, params = raw.params())[["Segments_L40M100V400_T5A5"]]
  b <- raw.timeuse.report(it9, params = raw.params())[["MMsegment_L40M100V400_T5A5"]]
  expect_identical(dim(a$daysummary_cleaned), dim(b$daysummary_cleaned))
  expect_identical(num5(a$daysummary_cleaned$dur_day_min),
                   num5(b$daysummary_cleaned$dur_day_min))
})

test_that("P7l: a part-5 result without last_timestamp is refused, not crashed through", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  items <- r5_load(MOS2_OUT5())
  bad <- items
  bad[[1]]$last_timestamp <- NULL
  expect_error(raw.timeuse.report(bad, params = raw.params()),
               "carries no last_timestamp")
  expect_error(raw.timeuse.report(bad, params = raw.params()),
               "replacement has 0 rows, data has 7")
  bad2 <- items
  bad2[[1]]$last_timestamp <- as.POSIXct(NA)
  expect_error(raw.timeuse.report(bad2, params = raw.params()), "carries no last_timestamp")
})

test_that("P7m: no valid window leaves the QC frame and nothing else", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  items <- r5_load(MOS2_OUT5())
  rep <- suppressWarnings(raw.timeuse.report(items,
                                             params = raw.params(includedaycrit.part5 = 20)))
  for (k in names(rep)) {
    expect_false(is.null(rep[[k]]$daysummary_full))
    expect_null(rep[[k]]$daysummary_cleaned)
    expect_null(rep[[k]]$personsummary)
    expect_null(rep[[k]]$daysummary_native)
    expect_identical(rep[[k]]$validdaysi_day, integer(0))
    expect_identical(rep[[k]]$charcol, character(0))
    expect_false(any(rep[[k]]$windows$valid))
  }
  expect_identical(dim(rep[["MM_L40M100V400_T5A5"]]$daysummary_full), c(4L, 117L))
  expect_match(attr(rep, "canhrActi")$messages[1], "No window met the inclusion criteria")
  # includedaycrit.part5 above 1 is an hours criterion and GGIR warns about the change
  expect_warning(raw.timeuse.report(items, params = raw.params(includedaycrit.part5 = 20)),
                 "behaviour of parameter includedaycrit.part5")
  # a length-2 includedaycrit, whose second element is 600 hours
  rep2 <- suppressWarnings(
    raw.timeuse.report(items, params = raw.params(includedaycrit = c(16, 600))))
  expect_null(rep2[["MM_L40M100V400_T5A5"]]$daysummary_cleaned)
  expect_true(all(grepl("wear_min_day_spt",
                        rep2[["MM_L40M100V400_T5A5"]]$windows$reason)))
})

test_that("the OO branch and the last-window hour floors match GGIR", {
  r5_skip_ref()
  r5_skip_ggir()
  r5_skip_dt()
  r5_skip_dir(MOS2_OUT5())
  # No reference was run with timewindow = "OO", so the three arms of the last-window clause
  # (MM above 9, WW at least 15, OO at least 9) are driven here with the WW rows relabelled
  # OO and a last_timestamp at 12:29, where the WW floor fails and the OO floor passes.
  e <- new.env()
  load(file.path(MOS2_OUT5(), "meta", "ms5.out", "MOS2E39230594.gt3x.RData"), envir = e)
  oo <- e$output
  oo$window[oo$window == "OO" | oo$window == "WW"] <- "OO"
  lt12 <- as.POSIXct("2025-10-14 12:29:55", tz = "")
  lp <- GGIR::load_params()
  runpair <- function(out, lt, lab) {
    d <- file.path(tempdir(), paste0("canhrActi_p5rep_", lab))
    unlink(d, recursive = TRUE)
    dir.create(file.path(d, "meta", "ms5.out"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(d, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
    output <- out
    last_timestamp <- lt
    tail_expansion_log <- NULL
    GGIRversion <- "3.3.6"
    save(output, last_timestamp, tail_expansion_log, GGIRversion,
         file = file.path(d, "meta", "ms5.out", "MOS2E39230594.gt3x.RData"))
    po <- lp$params_output
    po[["require_complete_lastnight_part5"]] <- TRUE
    suppressWarnings(GGIR:::g.report.part5(metadatadir = normalizePath(d, winslash = "/"),
                                           f0 = 1, f1 = 1, loglocation = c(),
                                           params_cleaning = lp$params_cleaning,
                                           LUX_day_segments = c(), params_output = po,
                                           verbose = FALSE))
    items <- list(list(output = output, last_timestamp = last_timestamp,
                       tail_expansion_log = NULL, label = "MOS2E39230594.gt3x.RData"))
    r <- suppressWarnings(raw.timeuse.report(
      items, params = raw.params(require_complete_lastnight_part5 = TRUE)))
    for (k in names(r)) {
      for (t in list(list("daysummary_full",
                          file.path(d, "results", "QC",
                                    paste0("part5_daysummary_full_", k, ".csv"))),
                     list("daysummary_cleaned",
                          file.path(d, "results", paste0("part5_daysummary_", k, ".csv"))),
                     list("personsummary",
                          file.path(d, "results",
                                    paste0("part5_personsummary_", k, ".csv"))))) {
        if (!file.exists(t[[2]])) {
          expect_null(r[[k]][[t[[1]]]], info = paste(lab, k, t[[1]]))
          next
        }
        expect_true(isTRUE(r5_same_csv(r[[k]][[t[[1]]]], t[[2]])),
                    info = paste0(lab, " / ", k, " / ", t[[1]], ": ",
                                  r5_same_csv(r[[k]][[t[[1]]]], t[[2]])))
      }
    }
    unlink(d, recursive = TRUE)
    r
  }
  a <- runpair(oo, lt12, "oo12")
  expect_identical(names(a), c("MM_L40M100V400_T5A5", "OO_L40M100V400_T5A5"))
  expect_identical(dim(a[["OO_L40M100V400_T5A5"]]$daysummary_cleaned), c(3L, 115L))
  expect_identical(a[["OO_L40M100V400_T5A5"]]$windows$reason, c("", "", ""))
  b <- runpair(e$output, lt12, "ww12")
  expect_identical(dim(b[["WW_L40M100V400_T5A5"]]$daysummary_cleaned), c(2L, 115L))
  expect_identical(b[["WW_L40M100V400_T5A5"]]$windows$reason, c("", "", "lastWindow"))
  cc <- runpair(e$output, e$last_timestamp, "ww17")
  expect_identical(dim(cc[["WW_L40M100V400_T5A5"]]$daysummary_cleaned), c(3L, 115L))
  expect_identical(cc[["WW_L40M100V400_T5A5"]]$windows$reason, c("", "", ""))
  # the MM floor is above 9, so all three agree there
  for (r in list(a, b, cc)) {
    expect_identical(r[["MM_L40M100V400_T5A5"]]$windows$reason,
                     c("minimum_MM_length", "", "", ""))
  }
})

test_that("data_cleaning_file blanks the named (ID, window_number) rows, as GGIR does", {
  r5_skip_ref()
  r5_skip_dt()
  r5_skip_dir(MOS2_OUT5())
  items <- r5_load(MOS2_OUT5())
  dcf <- file.path(tempdir(), "canhrActi_p5rep_dcf.csv")
  utils::write.csv(data.frame(ID = "MOS2E39230594.gt3x", day_part5 = 2), dcf,
                   row.names = FALSE)
  rep <- suppressWarnings(raw.timeuse.report(items,
                                             params = raw.params(data_cleaning_file = dcf)))
  mm <- rep[["MM_L40M100V400_T5A5"]]
  expect_identical(dim(mm$daysummary_cleaned), c(2L, 115L))
  expect_identical(mm$validdaysi_day, 3:4)
  expect_identical(mm$windows$reason,
                   c("minimum_MM_length", "include_window", "", ""))
  # GGIR reads the file inside getValidDayIndices, not in part 5
  r5_skip_ggir()
  lp <- GGIR::load_params()
  pc <- lp$params_cleaning
  po <- lp$params_output
  po[["require_complete_lastnight_part5"]] <- FALSE
  pc[["data_cleaning_file"]] <- dcf
  g <- file.path(tempdir(), "canhrActi_p5rep_dcfrun")
  unlink(g, recursive = TRUE)
  dir.create(file.path(g, "meta", "ms5.out"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(g, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(MOS2_OUT5(), "meta", "ms5.out"), full.names = TRUE),
            file.path(g, "meta", "ms5.out"))
  suppressWarnings(GGIR:::g.report.part5(metadatadir = normalizePath(g, winslash = "/"),
                                         f0 = 1, f1 = 1, loglocation = c(),
                                         params_cleaning = pc, LUX_day_segments = c(),
                                         params_output = po, verbose = FALSE))
  for (k in names(rep)) {
    for (t in list(list("daysummary_full",
                        file.path(g, "results", "QC",
                                  paste0("part5_daysummary_full_", k, ".csv"))),
                   list("daysummary_cleaned",
                        file.path(g, "results", paste0("part5_daysummary_", k, ".csv"))),
                   list("personsummary",
                        file.path(g, "results",
                                  paste0("part5_personsummary_", k, ".csv"))))) {
      expect_true(isTRUE(r5_same_csv(rep[[k]][[t[[1]]]], t[[2]])),
                  info = paste0(k, " / ", t[[1]], ": ",
                                r5_same_csv(rep[[k]][[t[[1]]]], t[[2]])))
    }
  }
  unlink(g, recursive = TRUE)
  unlink(dcf)
})

test_that("a non-empty tail_expansion_log blanks the last window's sleep columns", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  # No reference or fixture has a tail expansion, so the branch is driven synthetically.
  items <- r5_load(MOS2_OUT5())
  expect_null(items[[1]]$tail_expansion_log)
  plain <- raw.timeuse.report(items, params = raw.params())
  items[[1]]$tail_expansion_log <- list(short = 120, long = 1440)
  rep <- raw.timeuse.report(items, params = raw.params())
  f <- rep[["MM_L40M100V400_T5A5"]]$daysummary_full
  p <- plain[["MM_L40M100V400_T5A5"]]$daysummary_full
  expect_identical(dim(f), dim(p))
  # only the last window number is touched, and only the columns the grep names
  hit <- c("dur_spt_sleep_min", "dur_spt_wake_IN_min", "dur_spt_wake_LIG_min",
           "dur_spt_wake_MOD_min", "dur_spt_wake_VIG_min", "ACC_spt_sleep_mg",
           "sleep_efficiency_after_onset", "N_atleast5minwakenight", "daysleeper",
           "sleeplog_used")
  for (cn in hit) expect_true(is.na(f[[cn]][4]), info = cn)
  for (cn in hit) expect_identical(f[[cn]][1:3], p[[cn]][1:3], info = cn)
  # dur_spt_min is outside the grep, so it still gates the last window in
  expect_identical(f$dur_spt_min, p$dur_spt_min)
  expect_identical(f$dur_day_min, p$dur_day_min)
  expect_identical(nrow(rep[["MM_L40M100V400_T5A5"]]$daysummary_cleaned), 3L)
})

test_that("f0 and f1 select a subset, and f1 is clamped as GGIR clamps it", {
  r5_skip_ref()
  d <- r5_p5fix(file.path("cohort", "output_din"))
  r5_skip_dir(d)
  items <- r5_load(d)
  both <- suppressWarnings(raw.timeuse.report(items, params = raw.params()))
  expect_identical(nrow(both[["MM_L40M100V400_T5A5"]]$personsummary), 2L)
  first <- suppressWarnings(raw.timeuse.report(items, params = raw.params(), f1 = 1))
  expect_identical(nrow(first[["MM_L40M100V400_T5A5"]]$personsummary), 1L)
  expect_identical(as.character(first[["MM_L40M100V400_T5A5"]]$personsummary$filename),
                   "EE_left_29.5.2017-05-30.gt3x")
  second <- suppressWarnings(raw.timeuse.report(items, params = raw.params(), f0 = 2))
  expect_identical(as.character(second[["MM_L40M100V400_T5A5"]]$personsummary$filename),
                   "MOS2E39230594.gt3x")
  clamped <- suppressWarnings(raw.timeuse.report(items, params = raw.params(), f1 = 99))
  expect_identical(nrow(clamped[["MM_L40M100V400_T5A5"]]$personsummary), 2L)
})

test_that("an empty input no-ops with a warning and a status, as GGIR does", {
  empty <- list(output = data.frame(ID = character(0)), last_timestamp = Sys.time())
  expect_warning(r <- raw.timeuse.report(empty, params = raw.params()),
                 "no report was generated")
  expect_identical(length(r), 0L)
  expect_identical(attr(r, "canhrActi")$status$state, "no_input")
  expect_s3_class(r, "canhrActi_raw_timeuse_report")
  expect_error(raw.timeuse.report(42, params = raw.params()),
               "must be a canhrActi_raw_timeuse")
  expect_error(raw.timeuse.report(list(list(a = 1), list(b = 2)), params = raw.params()),
               "elements 1, 2 of timeuse")
})

test_that("a canhrActi_raw_timeuse and its ms5 form give the identical report", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  b <- r5_timeuse(MOS2_OUT5())
  from_object <- raw.timeuse.report(b$tu, params = raw.params())
  from_ms5 <- raw.timeuse.report(as.ggir.ms5(b$tu), params = raw.params())
  from_stored <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  strip <- function(r) lapply(unclass(r), function(e) {
    e$daysummary_full$filename <- as.character(e$daysummary_full$filename)
    e
  })
  expect_identical(names(from_object), names(from_ms5))
  expect_identical(strip(from_object), strip(from_ms5))
  expect_identical(strip(from_object), strip(from_stored))
  expect_identical(from_object[["WW_L40M100V400_T5A5"]]$personsummary,
                   from_ms5[["WW_L40M100V400_T5A5"]]$personsummary)
  expect_identical(from_object[["WW_L40M100V400_T5A5"]]$daysummary_cleaned,
                   from_stored[["WW_L40M100V400_T5A5"]]$daysummary_cleaned)
  expect_identical(from_object[["MM_L40M100V400_T5A5"]]$personsummary,
                   from_stored[["MM_L40M100V400_T5A5"]]$personsummary)
  # with params NULL the part-5 result's own parameters are reused
  reused <- raw.timeuse.report(b$tu)
  expect_identical(reused[["WW_L40M100V400_T5A5"]]$personsummary,
                   from_object[["WW_L40M100V400_T5A5"]]$personsummary)
  expect_identical(attr(reused, "canhrActi")$settings$minimum_MM_length.part5, 23)
})

test_that("the grouped weighted mean is bit identical to data.table's", {
  r5_skip_dt()
  set.seed(7)
  dt <- data.frame(filename = rep(c("b.gt3x", "a.gt3x", "c.gt3x"), each = 2),
                   len = c(5, 2, 5, 2, 5, 0),
                   v1 = rnorm(6), v2 = c(rnorm(5), NA), v3 = rep(NA_real_, 6),
                   stringsAsFactors = FALSE)
  mine <- .raw.timeuse.report.weighted(dt, "filename")
  d <- data.table::as.data.table(dt)
  # data.table refuses its own syntax when called from a namespace that only suggests it,
  # so the reference is computed outside the package namespace
  dt_env <- list2env(list(d = d), parent = globalenv())
  ref <- suppressWarnings(evalq(
    d[, lapply(.SD, stats::weighted.mean, w = len, na.rm = TRUE), by = list(filename)],
    dt_env))
  ref <- as.data.frame(ref, stringsAsFactors = TRUE)
  expect_identical(names(mine), names(ref))
  expect_identical(mine$filename, ref$filename)
  for (cn in setdiff(names(ref), "filename")) {
    expect_identical(mine[[cn]], ref[[cn]], info = cn)
  }
  # group order is first appearance, not alphabetical, in both
  expect_identical(mine$filename, c("b.gt3x", "a.gt3x", "c.gt3x"))
  # len is averaged with itself as the weight: (5*5 + 2*2)/(5 + 2), or 5 when the second
  # day weighs 0
  expect_equal(mine$len, c(29 / 7, 29 / 7, 5))
  # an all-NA column comes back NaN, never NA
  expect_true(all(is.nan(mine$v3)))
  expect_false(any(is.na(mine$v3) & !is.nan(mine$v3)))
  dt2 <- cbind(dt, window = rep(c("segment1", "segment2"), 3))
  m2 <- .raw.timeuse.report.weighted(dt2[, c("filename", "window", "len", "v1")],
                                     c("filename", "window"))
  dt_env$d2 <- data.table::as.data.table(dt2[, c("filename", "window", "len", "v1")])
  r2 <- suppressWarnings(evalq(
    d2[, lapply(.SD, stats::weighted.mean, w = len, na.rm = TRUE), by = list(filename, window)],
    dt_env))
  expect_identical(m2$v1, as.data.frame(r2)$v1)
})

test_that("LC_TIME is forced to C and restored", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  # a German caller, so a missing switch to C or a missing restore shows
  local_german_time()
  before <- Sys.getlocale("LC_TIME")
  rep <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  expect_identical(Sys.getlocale("LC_TIME"), before)
  # daytype comes from an English weekday-name comparison, so it is never the literal "0"
  expect_true(all(rep[["MM_L40M100V400_T5A5"]]$daysummary_full$daytype %in% c("WD", "WE")))
})

test_that("the loglocation argument is gone and unknown overrides are refused", {
  expect_false("loglocation" %in% names(formals(raw.timeuse.report)))
  expect_identical(names(formals(raw.timeuse.report))[1:5],
                   c("timeuse", "params", "f0", "f1", "segment_naming"))
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  expect_error(raw.timeuse.report(r5_load(MOS2_OUT5()), notAParameter = 1),
               "notAParameter is unknown to raw.params")
})

test_that("P7h and P7i: the nine-row sweep matches GGIR row for row and byte for byte", {
  r5_skip_ref()
  r5_skip_ggir()
  r5_skip_dt()
  r5_skip_dir(MOS2_OUT5())
  items <- r5_load(MOS2_OUT5())
  lp <- GGIR::load_params()
  cases <- list(
    list("baseline", list(), c("3x115", "3x115", "1x207", "1x206")),
    list("require_complete_lastnight_part5 = TRUE",
         list(require_complete_lastnight_part5 = TRUE),
         c("3x115", "3x115", "1x207", "1x206")),
    list("excludefirstlast.part5 = TRUE", list(excludefirstlast.part5 = TRUE),
         c("2x115", "1x115", "1x207", "1x205")),
    list("includedaycrit.part5 = 16", list(includedaycrit.part5 = 16),
         c("2x115", "2x115", "1x207", "1x206")),
    list("includedaycrit.part5 = 20", list(includedaycrit.part5 = 20),
         c("none", "none", "none", "none")),
    list("includenightcrit.part5 = 0.9", list(includenightcrit.part5 = 0.9),
         c("2x115", "3x115", "1x206", "1x206")),
    list("minimum_MM_length.part5 = 3", list(minimum_MM_length.part5 = 3),
         c("4x115", "3x115", "1x207", "1x206")),
    list("includedaycrit = c(16, 600)", list(includedaycrit = c(16, 600)),
         c("none", "none", "none", "none")),
    list("week_weekend_aggregate.part5 = TRUE", list(week_weekend_aggregate.part5 = TRUE),
         c("3x115", "3x115", "1x385", "1x382")))
  dimstr <- function(df) if (is.null(df)) "none" else paste0(nrow(df), "x", ncol(df))
  for (cs in cases) {
    nm <- cs[[1]]
    ov <- cs[[2]]
    # GGIR's own run over the same milestone
    g <- file.path(tempdir(), paste0("canhrActi_p5rep_", gsub("[^A-Za-z0-9]", "", nm)))
    unlink(g, recursive = TRUE)
    dir.create(file.path(g, "meta", "ms5.out"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(g, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(file.path(MOS2_OUT5(), "meta", "ms5.out"), full.names = TRUE),
              file.path(g, "meta", "ms5.out"))
    pc <- lp$params_cleaning
    po <- lp$params_output
    po[["require_complete_lastnight_part5"]] <- FALSE
    for (k in names(ov)) {
      if (k %in% names(pc)) pc[[k]] <- ov[[k]] else po[[k]] <- ov[[k]]
    }
    suppressWarnings(GGIR:::g.report.part5(metadatadir = normalizePath(g, winslash = "/"),
                                           f0 = 1, f1 = 1, loglocation = c(),
                                           params_cleaning = pc, LUX_day_segments = c(),
                                           params_output = po, verbose = FALSE))
    rep <- suppressWarnings(raw.timeuse.report(items, params = do.call(raw.params, ov)))
    mm <- rep[["MM_L40M100V400_T5A5"]]
    ww <- rep[["WW_L40M100V400_T5A5"]]
    expect_identical(c(dimstr(mm$daysummary_cleaned), dimstr(ww$daysummary_cleaned),
                       dimstr(mm$personsummary), dimstr(ww$personsummary)),
                     cs[[3]], info = nm)
    for (k in names(rep)) {
      for (t in list(list("daysummary_full",
                          file.path(g, "results", "QC",
                                    paste0("part5_daysummary_full_", k, ".csv"))),
                     list("daysummary_cleaned",
                          file.path(g, "results", paste0("part5_daysummary_", k, ".csv"))),
                     list("personsummary",
                          file.path(g, "results",
                                    paste0("part5_personsummary_", k, ".csv"))))) {
        df <- rep[[k]][[t[[1]]]]
        p <- t[[2]]
        if (!file.exists(p)) {
          expect_null(df, info = paste(nm, k, t[[1]]))
          next
        }
        expect_true(isTRUE(r5_same_csv(df, p)),
                    info = paste0(nm, " / ", k, " / ", t[[1]], ": ", r5_same_csv(df, p)))
      }
    }
    unlink(g, recursive = TRUE)
  }
})

test_that("V1: the installed g.report.part5 still has the body this port was taken from", {
  r5_skip_ggir()
  body_lines <- deparse(GGIR:::g.report.part5)
  expect_identical(length(body_lines), 729L)
  # anchors are taken from deparse(), not the source file, so the wrapping matches
  anchors <- c("getValidDayIndices = function(x, window, params_cleaning) {",
               "minimumValidMinutesMM = 0",
               "x$lastHour = as.numeric(x$lastHour)",
               "set.seed(1234)",
               "testfiles = unique(sample(x = f0:f1, size = pmin(5, f1 - ",
               "outputfinal$filename = gsub(\".RData$\", \"\", outputfinal$filename)",
               "outputfinal$daytype = 0",
               "ignorevar = c(\"daysleeper\", \"cleaningcode\", ",
               "agg_plainNweighted = function(df, filename = \"filename\", ",
               "plain_mean = function(x) {",
               "weighted.mean, w = len, na.rm = TRUE), ",
               "foo34 = function(df, aggPerIndividual, ",
               "OF4 = cbind(OF4[, 1:5], OF4[, (ncol(OF4) - ",
               "names(OF4)[which(names(OF4) == \"weekday\")] = \"startday\"",
               "OF4 = OF4[, unique(c(1:4, Nvaliddays_variables, ")
  for (a in anchors) {
    expect_gte(length(grep(a, body_lines, fixed = TRUE)), 1L,
               label = paste0("anchor absent from the installed g.report.part5: ", a))
  }
  # the rounding helper is shared with raw_sleep_report.R; FRAG and SSP columns keep six decimals
  expect_identical(deparse(args(.raw.report.tidyup)), deparse(args(GGIR:::tidyup_df)))
  d <- data.frame(a = c("1.23456789", "2"), FRAG_x = c("0.12345678", "1"),
                  SSPz = c("0.987654321", "0"), lbl = c("keep", "me"),
                  stringsAsFactors = FALSE)
  t1 <- .raw.report.tidyup(d)
  t2 <- GGIR:::tidyup_df(d)
  expect_identical(t1, t2)
  expect_identical(t1$a, c(1.235, 2))
  expect_identical(t1$FRAG_x, c(0.123457, 1))
  expect_identical(t1$SSPz, c(0.987654, 0))
  expect_identical(t1$lbl, c("keep", "me"))
  # 3.3.6 uses the literal "Segments"; 3.3-9 greps "segment". The port carries both.
  v <- as.character(utils::packageVersion("GGIR"))
  seg336 <- length(grep("window != \"Segments\"", body_lines, fixed = TRUE)) > 0
  if (v == "3.3.6") {
    expect_true(seg336)
  } else {
    expect_true(length(grep("window_is_segment = length(grep(pattern = \"segment\", ",
                            body_lines, fixed = TRUE)) > 0)
  }
})

test_that("V3: 3.3.6 and 3.3-9 differ in this file only in the segment naming", {
  r5_skip_ggir()
  src <- r5_clone()
  if (src == "" || !file.exists(file.path(src, "g.report.part5.R"))) {
    testthat::skip("no GGIR 3.3-9 clone at CANHRACTI_GGIR_SRC or beside the reference")
  }
  e <- new.env()
  sys.source(file.path(src, "g.report.part5.R"), envir = e)
  a <- deparse(GGIR:::g.report.part5)
  b <- deparse(e$g.report.part5)
  added <- trimws(setdiff(b, a))
  removed <- trimws(setdiff(a, b))
  # every line that differs between the two versions belongs to the segment test or name
  allowed <- paste0("segment|Segment|uwi|select_window|x = window|outputfinal[$]window|",
                    "^0$|^[{]$|^[}]$|^$|^c[(]\"MM\", \"WW\", \"OO\"[)][)]$")
  expect_identical(grep(allowed, added, invert = TRUE, value = TRUE), character(0))
  expect_identical(grep(allowed, removed, invert = TRUE, value = TRUE), character(0))
  expect_identical(length(added), 29L)
  expect_true(any(grepl("MMsegment", added, fixed = TRUE)))
  expect_true(any(grepl("window_is_segment", added, fixed = TRUE)))
  expect_false(any(grepl("MMsegment", a, fixed = TRUE)))
  expect_true(any(grepl("\"Segments\"", a, fixed = TRUE)))
})

test_that("raw.sleep.report takes the part-5 day table and reproduces GGIR's merge", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  b <- r5_timeuse(MOS2_OUT5())
  stored <- r5_load(MOS2_OUT5())[[1]]$output
  # the part-5 day table is GGIR's output frame
  expect_identical(b$tu$daysummary, stored)
  none <- suppressWarnings(raw.sleep.report(b$nights, params = raw.params(), part5 = NULL))
  with5 <- suppressWarnings(raw.sleep.report(b$nights, params = raw.params(),
                                             part5 = b$tu$daysummary))
  fromstored <- suppressWarnings(raw.sleep.report(b$nights, params = raw.params(),
                                                  part5 = stored))
  for (fr in names(with5)) expect_identical(with5[[fr]], fromstored[[fr]], info = fr)
  expect_identical(setdiff(names(with5$nightsummary_full), names(none$nightsummary_full)),
                   c("window", "nonwear_perc_spt", "ACC_spt_mg"))
  expect_identical(ncol(with5$nightsummary_full) - ncol(none$nightsummary_full), 3L)
  expect_identical(ncol(with5$personsummary_full) - ncol(none$personsummary_full), 6L)
  # the merged values are the WW rows of the part-5 table, matched on (ID, night_number)
  ww <- stored[stored$window == "WW", ]
  expect_identical(with5$nightsummary_full$window, c(NA, "WW", "WW", "WW"))
  expect_equal(with5$nightsummary_full$nonwear_perc_spt[2:4],
               as.numeric(ww$nonwear_perc_spt))
  expect_equal(with5$nightsummary_full$ACC_spt_mg[2:4], as.numeric(ww$ACC_spt_mg))
  p <- r5_ref("out", "output_din", "results", "QC", "part4_nightsummary_sleep_full.csv")
  if (file.exists(p) && requireNamespace("data.table", quietly = TRUE)) {
    g <- data.table::fread(p, data.table = FALSE)
    expect_true(all(c("window", "nonwear_perc_spt", "ACC_spt_mg") %in% names(g)))
    expect_identical(as.character(g$window), c("", "WW", "WW", "WW"))
    expect_equal(as.numeric(g$nonwear_perc_spt)[2:4],
                 round(with5$nightsummary_full$nonwear_perc_spt[2:4], 3))
    expect_equal(as.numeric(g$ACC_spt_mg)[2:4],
                 round(with5$nightsummary_full$ACC_spt_mg[2:4], 3))
  }
  # the part-5 object is not accepted, so part 4 never depends on part 5
  expect_error(.raw.nights.merge.part5(b$tu), "takes the part-5 DAY TABLE")
  expect_error(raw.sleep.report(b$nights, params = raw.params(), part5 = b$tu),
               "takes the part-5 DAY TABLE")
  expect_error(.raw.nights.merge.part5(list(b$tu)), "takes the part-5 DAY TABLE")
  expect_s3_class(.raw.nights.merge.part5(b$tu$daysummary), "data.frame")
  expect_identical(dim(.raw.nights.merge.part5(b$tu$daysummary)), c(3L, 5L))
  expect_identical(colnames(.raw.nights.merge.part5(b$tu$daysummary)),
                   c("ID", "window", "night", "nonwear_perc_spt", "ACC_spt_mg"))
})

test_that("the print method names every configuration and its three shapes", {
  r5_skip_ref()
  r5_skip_dir(MOS2_OUT5())
  rep <- raw.timeuse.report(r5_load(MOS2_OUT5()), params = raw.params())
  # called by name, so the test does not depend on the NAMESPACE entry
  out <- utils::capture.output(print.canhrActi_raw_timeuse_report(rep))
  expect_true(any(grepl("canhrActi GGIR part-5 report", out)))
  expect_true(any(grepl("WW_L40M100V400_T5A5", out)))
  expect_true(any(grepl("person 1 x 206", out)))
  expect_true(any(grepl("day full 4 x 117", out)))
})

test_that("the study report over several recordings is GGIR's g.report.part5 over one folder", {
  r5_skip_ref()
  r5_skip_ggir()
  r5_skip_dt()
  r5_skip_dir(MOS2_OUT5())
  r5_skip_dir(EE_OUT5())
  # one stand-in raw.ggir.report() result per recording: a folder holding its ms5 milestone
  one <- function(src) {
    d <- tempfile("canhrActi_rep_")
    dir.create(file.path(d, "meta", "ms5.out"), recursive = TRUE)
    f <- list.files(file.path(src, "meta", "ms5.out"), full.names = TRUE)
    file.copy(f, file.path(d, "meta", "ms5.out", basename(f)))
    list(state = "ok", dir = d)
  }
  reps <- list(one(MOS2_OUT5()), one(EE_OUT5()))
  # GGIR's own report over a folder holding both milestones, as its study run has them;
  # the second pass has the overrides the dashboard passes for a scheduled run
  for (po_set in list(list(), list(week_weekend_aggregate.part5 = TRUE))) {
    pc_set <- if (length(po_set)) list(segmentWEARcrit.part5 = 0.5) else list()
    st <- .raw.ggir.report.study(reps, params_cleaning = pc_set, params_output = po_set)
    expect_identical(st$state, "ok")
    g <- tempfile("canhrActi_ggir_")
    dir.create(file.path(g, "meta", "ms5.out"), recursive = TRUE)
    dir.create(file.path(g, "results", "QC"), recursive = TRUE)
    for (r in reps) {
      f <- list.files(file.path(r$dir, "meta", "ms5.out"), full.names = TRUE)
      file.copy(f, file.path(g, "meta", "ms5.out", basename(f)))
    }
    lp <- GGIR::load_params()
    for (n in names(pc_set)) lp$params_cleaning[[n]] <- pc_set[[n]]
    for (n in names(po_set)) lp$params_output[[n]] <- po_set[[n]]
    suppressWarnings(GGIR:::g.report.part5(metadatadir = g, f0 = 1, f1 = 2,
                                           params_cleaning = lp$params_cleaning,
                                           params_output = lp$params_output, verbose = FALSE))
    gf <- list.files(file.path(g, "results"), pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
    expect_length(gf, 6L)
    expect_setequal(names(st$csv), basename(gf))
    for (f in gf) {
      expect_identical(readBin(st$csv[[basename(f)]], "raw", 1e7), readBin(f, "raw", 1e7),
                       info = basename(f))
    }
    pm <- utils::read.csv(st$csv[["part5_personsummary_MM_L40M100V400_T5A5.csv"]],
                          check.names = FALSE, colClasses = "character")
    expect_identical(pm$filename, c("EE_left_29.5.2017-05-30.gt3x", "MOS2E39230594.gt3x"))
    unlink(c(st$dir, g), recursive = TRUE)
  }
  # the same file name twice is refused, and no milestone means no report
  dup <- .raw.ggir.report.study(list(reps[[1]], reps[[1]]))
  expect_identical(dup$state, "duplicate_names")
  expect_match(dup$messages, "MOS2E39230594.gt3x", fixed = TRUE)
  expect_identical(.raw.ggir.report.study(list(list(state = "error", dir = NA_character_)))$state,
                   "no_timeuse")
  unlink(vapply(reps, `[[`, "", "dir"), recursive = TRUE)
})
