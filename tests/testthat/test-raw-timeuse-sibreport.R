# Parity tests for R/raw_timeuse_sibreport.R against GGIR's g.sibreport.
# Reference data live in the folder named by CANHRACTI_GGIR_REF; the diary and
# part-5 fixtures are its siblings ggir-study-p34/fixtures and
# ggir-study-p5/fixtures. Tests skip when what they need is absent.

# LC_TIME "C" for deterministic date formatting.
withr::local_locale(c(LC_TIME = "C"))
# GGIR wrote the reference outputs in an America/Anchorage session
withr::local_timezone("America/Anchorage")

ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
ggir_ref_ok <- nzchar(ggir_ref) && dir.exists(ggir_ref)
scratch <- if (ggir_ref_ok) dirname(sub("[/\\\\]+$", "", ggir_ref)) else ""
p34_fixtures <- if (nzchar(scratch)) file.path(scratch, "ggir-study-p34", "fixtures") else ""
p5_fixtures <- if (nzchar(scratch)) file.path(scratch, "ggir-study-p5", "fixtures") else ""

mos2_series <- function() {
  file.path(ggir_ref, "out", "output_din", "meta", "ms5.outraw", "40_100_400",
            "MOS2E39230594_T5A5.RData")
}
mos2_sibreport_csv <- function() {
  file.path(ggir_ref, "out", "output_din", "meta", "ms5.outraw", "sib.reports",
            "sib_report_MOS2E39230594gt3x_T5A5.csv")
}

skip_without_ref <- function(path) {
  testthat::skip_if_not(ggir_ref_ok, "CANHRACTI_GGIR_REF is unset or does not point to a folder")
  testthat::skip_if_not(file.exists(path), paste0("reference file not found: ", path))
}

ggir_available <- function() {
  isTRUE(requireNamespace("GGIR", quietly = TRUE)) &&
    isTRUE(exists("g.sibreport", envir = asNamespace("GGIR"), inherits = FALSE))
}
skip_without_ggir <- function() {
  testthat::skip_if_not(ggir_available(), "GGIR (with g.sibreport) is not installed")
}

# The MOS2 part-5 ts rebuilt from the stored series. mdat$ACC is round(ACC, 3), which
# the report's own 3-decimal rounding absorbs.
mos2_ts <- function() {
  env <- new.env()
  load(mos2_series(), envir = env)
  mdat <- env$mdat
  data.frame(time = mdat$timestamp, ACC = mdat$ACC, diur = mdat$SleepPeriodTime,
             sibdetection = mdat$sibdetection)
}
mos2_mdat <- function() {
  env <- new.env()
  load(mos2_series(), envir = env)
  env$mdat
}
MOS2_ID <- "MOS2E39230594.gt3x"

# A small all-waking series for the synthetic cases.
synth_ts <- function(n = 2000, start = "2025-10-07 20:30:00", tz = "UTC", acc = 10) {
  data.frame(time = as.POSIXct(start, tz = tz) + seq(0, by = 5, length.out = n),
             ACC = rep(acc, n), diur = rep(0, n), sibdetection = rep(0, n))
}

test_that("MOS2: the report has GGIR's shape, 66 rows and seven named columns", {
  skip_without_ref(mos2_series())
  sr <- raw.sib.report(mos2_ts(), ID = MOS2_ID, epochlength = 5, desiredtz = "")
  expect_s3_class(sr, "data.frame")
  expect_identical(dim(sr), c(66L, 7L))
  expect_identical(names(sr),
                   c("ID", "type", "start", "end", "duration",
                     "mean_acc_1min_before", "mean_acc_1min_after"))
  expect_identical(unique(sr$type), "sib")
  expect_identical(unique(sr$ID), MOS2_ID)
  expect_s3_class(sr$start, "POSIXct")
  expect_s3_class(sr$end, "POSIXct")
})

test_that("MOS2: every column matches the stored csv, strings exact, numbers at csv precision", {
  skip_without_ref(mos2_series())
  skip_without_ref(mos2_sibreport_csv())
  sr <- raw.sib.report(mos2_ts(), ID = MOS2_ID, epochlength = 5, desiredtz = "")
  ref <- utils::read.csv(mos2_sibreport_csv(), stringsAsFactors = FALSE)

  expect_identical(nrow(ref), 66L)
  expect_identical(readLines(mos2_sibreport_csv(), n = 1L),
                   "ID,type,start,end,duration,mean_acc_1min_before,mean_acc_1min_after")
  expect_identical(names(ref), names(sr))

  expect_identical(sr$ID, ref$ID)
  expect_identical(sr$type, ref$type)
  expect_identical(format(sr$start), ref$start)
  expect_identical(format(sr$end), ref$end)

  # the csv carries 15 significant digits, so duration differs by a few 1e-14
  dur_maxdiff <- max(abs(sr$duration - ref$duration))
  expect_lt(dur_maxdiff, 1e-12)
  expect_identical(round(sr$duration, 9), round(ref$duration, 9))
  expect_identical(max(abs(sr$mean_acc_1min_before - ref$mean_acc_1min_before)), 0)
  expect_identical(max(abs(sr$mean_acc_1min_after - ref$mean_acc_1min_after)), 0)

  expect_identical(format(sr$start[1:3]),
                   c("2025-10-07 21:11:00", "2025-10-07 21:36:45", "2025-10-07 21:49:00"))
  expect_identical(format(sr$end[1:3]),
                   c("2025-10-07 21:18:50", "2025-10-07 21:42:40", "2025-10-07 22:00:30"))
  expect_identical(round(sr$duration[1:3], 5), c(7.91667, 6, 11.58333))
  expect_identical(sr$mean_acc_1min_before[1:3], c(24.408, 4.708, 8.475))
  expect_identical(sr$mean_acc_1min_after[1:3], c(9, 2.592, 3.25))
})

test_that("MOS2: the numbers the spec quotes for the sib report", {
  skip_without_ref(mos2_series())
  sr <- raw.sib.report(mos2_ts(), ID = MOS2_ID, epochlength = 5, desiredtz = "")
  expect_identical(round(min(sr$duration), 4), 0.0833)
  expect_identical(max(sr$duration), 38.5)
  expect_identical(sum(sr$duration == 1 / 12), 4L)
  expect_identical(sum(sr$start == sr$end), 4L)
  # neither GGIR guard defect fires on this recording
  expect_identical(sum(sr$mean_acc_1min_before == 0), 0L)
  expect_identical(sum(is.na(sr$mean_acc_1min_after)), 0L)
  expect_identical(sum(is.na(sr$mean_acc_1min_before)), 0L)
})

test_that("MOS2: the one-minute windows are indexed in the waking subsequence", {
  skip_without_ref(mos2_series())
  ts <- mos2_ts()
  dayind <- which(ts$diur == 0)
  ss <- which(diff(c(0, ts$sibdetection[dayind], 0)) == 1)
  elapsed <- vapply(seq_along(ss), function(i) {
    mb <- (ss[i] - 12):(ss[i] - 1)
    if (min(mb) < 1) return(NA_real_)
    as.numeric(difftime(ts$time[dayind[ss[i]]], ts$time[dayind[mb[1]]], units = "mins"))
  }, numeric(1))
  expect_identical(sum(elapsed > 1.1, na.rm = TRUE), 4L)
  expect_identical(round(max(elapsed, na.rm = TRUE), 2), 496.25)
  expect_identical(round(min(elapsed, na.rm = TRUE), 6), 1)
})

test_that("MOS2: the rest analysis reads 6 candidates, 3/3/0 per window, 59.333 and 80.167 minutes", {
  # The nap candidate rule of g.part5.analyseRest (window 9 to 18, duration 15 to 240 min),
  # reproduced here because every input to it comes from raw.sib.report.
  skip_without_ref(mos2_series())
  sr <- raw.sib.report(mos2_ts(), ID = MOS2_ID, epochlength = 5, desiredtz = "")
  s <- sr
  s$acc_edge <- pmax(s$mean_acc_1min_before, s$mean_acc_1min_after)
  s$ignore <- FALSE
  s$startHour <- as.numeric(format(s$start, "%H"))
  s$endHour <- as.numeric(format(s$end, "%H"))
  overlapMidnight <- which(s$endHour < s$startHour)
  if (length(overlapMidnight) > 0) s$endHour[overlapMidnight] <- s$endHour[overlapMidnight] + 24
  longboutsi <- which((s$type == "sib" & s$duration >= 15 & s$duration < 240 &
                         s$acc_edge <= Inf & s$startHour >= 9 & s$endHour < 18 &
                         s$ignore == FALSE) |
                        (s$type != "sib" & s$duration >= 1))
  expect_identical(length(longboutsi), 6L)        # sibreport_n_items
  cand <- s[longboutsi, ]
  expect_identical(format(cand$start),
                   c("2025-10-08 09:03:10", "2025-10-08 11:17:50", "2025-10-08 15:51:05",
                     "2025-10-09 10:51:45", "2025-10-09 11:33:10", "2025-10-09 12:17:30"))

  # per WW window; mdat$window carries the last timewindow's numbering, which is WW
  mdat <- mos2_mdat()
  n_items_day <- integer(3)
  denap_min <- character(3)
  for (w in 1:3) {
    idx <- which(mdat$window == w & mdat$SleepPeriodTime == 0)
    tw <- mdat$timestamp[idx]
    st <- cand[which(cand$start >= min(tw) & cand$end <= max(tw)), ]
    drop <- integer(0)
    if (nrow(st) > 0) {
      for (k in seq_len(nrow(st))) {
        sibnap <- which(tw >= st$start[k] & tw <= st$end[k])
        if (length(sibnap) > 0) {
          fractionInvalid <- length(which(mdat$invalidepoch[idx][sibnap] == 1)) / length(sibnap)
          if (fractionInvalid >= 0.1) drop <- c(drop, k)
        }
      }
    }
    if (length(drop) > 0) st <- st[-drop, ]
    n_items_day[w] <- nrow(st)
    denap_min[w] <- if (nrow(st) > 0) format(round(sum(st$duration), 3), nsmall = 3) else ""
  }
  expect_identical(n_items_day, c(3L, 3L, 0L))     # sibreport_n_items_day
  expect_identical(denap_min, c("59.333", "80.167", ""))  # dur_day_denap_min
})

test_that("MOS2: identical() to GGIR:::g.sibreport with no diary", {
  skip_without_ggir()
  skip_without_ref(mos2_series())
  ts <- mos2_ts()
  mine <- raw.sib.report(ts, ID = MOS2_ID, epochlength = 5, desiredtz = "")
  theirs <- GGIR:::g.sibreport(ts, ID = MOS2_ID, epochlength = 5, logs_diaries = c(),
                               desiredtz = "")
  if (!identical(mine, theirs)) {
    for (cc in union(names(mine), names(theirs))) {
      a <- mine[[cc]]
      b <- theirs[[cc]]
      bad <- which(as.character(a) != as.character(b))
      if (length(bad) > 0) {
        message("column ", cc, " first differs at row ", bad[1], ": ",
                as.character(a[bad[1]]), " vs ", as.character(b[bad[1]]))
      }
    }
  }
  expect_identical(mine, theirs)
})

test_that("MOS2: identical() to GGIR with each of the three parts 3 and 4 diary fixtures", {
  skip_without_ggir()
  skip_without_ref(mos2_series())
  testthat::skip_if_not(nzchar(p34_fixtures) && dir.exists(file.path(p34_fixtures, "diary")),
                        "the parts 3 and 4 diary fixtures are not available")
  ts <- mos2_ts()
  expected_rows <- c(spt = 73L, adv = 97L, tib = 73L)
  for (fixture in names(expected_rows)) {
    path <- list.files(file.path(p34_fixtures, "diary", fixture, "output_din", "meta"),
                       pattern = "^sleeplog_", full.names = TRUE)
    if (length(path) != 1) next
    env <- new.env()
    load(path, envir = env)
    ld <- env$logs_diaries
    expect_identical(names(ld), c("sleeplog", "nonwearlog", "naplog", "bedlog",
                                  "imputecodelog", "dateformat"))
    mine <- raw.sib.report(ts, ID = MOS2_ID, epochlength = 5, logs_diaries = ld, desiredtz = "")
    theirs <- GGIR:::g.sibreport(ts, ID = MOS2_ID, epochlength = 5, logs_diaries = ld,
                                 desiredtz = "")
    expect_identical(mine, theirs)
    expect_identical(nrow(mine), unname(expected_rows[fixture]))
  }
  load_ld <- function(fixture) {
    env <- new.env()
    load(list.files(file.path(p34_fixtures, "diary", fixture, "output_din", "meta"),
                    pattern = "^sleeplog_", full.names = TRUE), envir = env)
    env$logs_diaries
  }
  spt <- raw.sib.report(ts, MOS2_ID, 5, load_ld("spt"), "")
  expect_identical(as.integer(table(spt$type)[c("sib", "sleeplog")]), c(66L, 7L))
  adv <- raw.sib.report(ts, MOS2_ID, 5, load_ld("adv"), "")
  expect_identical(as.integer(table(adv$type)[c("nap", "nonwear", "sib", "sleeplog")]),
                   c(16L, 8L, 66L, 7L))
  tib <- raw.sib.report(ts, MOS2_ID, 5, load_ld("tib"), "")
  expect_identical(as.integer(table(tib$type)[c("bedlog", "sib")]), c(7L, 66L))
})

# A synthetic advanced diary with a bedlog and an imputecodelog, which no stored fixture has.
make_impute_diary <- function() {
  root <- tempfile("canhrActi_sibreport_")
  dir.create(root, recursive = TRUE)
  msf <- file.path(root, "ms3")
  dir.create(msf)
  ID <- MOS2_ID
  rec_starttime <- "2025-10-07T20:30:00-0800"
  save(ID, rec_starttime, file = file.path(msf, "startdate.RData"))
  dates <- as.character(seq(as.Date("2025-10-07"), by = 1, length.out = 5))
  nm <- character(0)
  val <- character(0)
  for (di in seq_along(dates)) {
    nm <- c(nm, paste0("D", di, c("_date", "_wakeup", "_onset", "_bedstart", "_bedend",
                                  "_imputecode", "_nap1_start", "_nap1_end",
                                  "_nonwear1_off", "_nonwear1_on")))
    val <- c(val, dates[di], "07:00:00", "23:00:00", "22:30:00", "07:30:00",
             as.character(di), "23:00:00", "01:00:00", "17:00:00", "17:15:00")
  }
  log <- as.data.frame(t(c(ID, val)), stringsAsFactors = FALSE)
  colnames(log) <- c("ID", nm)
  path <- file.path(root, "imputediary.csv")
  utils::write.csv(log, path, row.names = FALSE, quote = TRUE)
  list(path = path, meta_sleep_folder = msf, rec_starttime = rec_starttime, id = ID)
}

test_that("raw.sleeplog is the adapter: identical() to GGIR g.loadlog on a diary with every member", {
  skip_without_ggir()
  testthat::skip_if_not(exists("g.loadlog", envir = asNamespace("GGIR"), inherits = FALSE),
                        "GGIR:::g.loadlog is not available")
  d <- make_impute_diary()
  mine <- raw.sleeplog(d$path, desiredtz = "", rec_starttime = d$rec_starttime, id = d$id)
  theirs <- GGIR:::g.loadlog(d$path, coln1 = 2, colid = 1, desiredtz = "",
                             meta.sleep.folder = d$meta_sleep_folder)
  expect_identical(mine, theirs)
  expect_identical(names(mine), c("sleeplog", "nonwearlog", "naplog", "bedlog",
                                  "imputecodelog", "dateformat"))
  expect_identical(dim(mine$sleeplog), c(4L, 5L))
  expect_identical(dim(mine$bedlog), c(4L, 5L))
  expect_identical(dim(mine$imputecodelog), c(4L, 3L))
  expect_identical(mine$imputecodelog$imputecode, c("2", "3", "4", "5"))
})

test_that("MOS2: identical() to GGIR with a bedlog and an imputecodelog merged in", {
  skip_without_ggir()
  skip_without_ref(mos2_series())
  d <- make_impute_diary()
  ld <- GGIR:::g.loadlog(d$path, coln1 = 2, colid = 1, desiredtz = "",
                         meta.sleep.folder = d$meta_sleep_folder)
  ts <- mos2_ts()
  mine <- raw.sib.report(ts, ID = MOS2_ID, epochlength = 5, logs_diaries = ld, desiredtz = "")
  theirs <- GGIR:::g.sibreport(ts, ID = MOS2_ID, epochlength = 5, logs_diaries = ld,
                               desiredtz = "")
  expect_identical(mine, theirs)
  expect_identical(names(mine), c("ID", "type", "start", "end", "duration", "imputecode",
                                  "mean_acc_1min_before", "mean_acc_1min_after"))
  expect_identical(nrow(mine), 84L)
  expect_identical(as.integer(table(mine$type)[c("bedlog", "nap", "nonwear", "sib", "sleeplog")]),
                   c(4L, 5L, 5L, 66L, 4L))
  expect_identical(mine$imputecode[mine$type == "bedlog"], c("2", "3", "4", "5"))
  expect_identical(mine$imputecode[mine$type == "sleeplog"], c("2", "3", "4", "5"))
  expect_true(all(is.na(mine$imputecode[mine$type %in% c("sib", "nap", "nonwear")])))
  expect_true(all(is.na(mine$mean_acc_1min_before[mine$type != "sib"])))
})

# A sleeplog of the shape raw.sleeplog returns.
sleeplog_frame <- function(onset, wake, id = "A") {
  data.frame(ID = rep(id, length(onset)), night = seq_along(onset),
             duration = rep(8, length(onset)), sleeponset = onset, sleepwake = wake,
             stringsAsFactors = FALSE)
}
naplog_frame <- function(dates, starts, ends, id = "A") {
  out <- data.frame(ID = rep(id, length(dates)), date = dates, nap1 = starts, nap1b = ends,
                    stringsAsFactors = FALSE)
  names(out)[3:4] <- c("nap1", "nap1")
  out
}

test_that("sleeplog date arithmetic: after midnight, day sleeper, and the 11am to 9am rule", {
  ts <- synth_ts(n = 100, start = "2025-10-07 20:30:00", tz = "UTC")
  ld <- list(sleeplog = sleeplog_frame(onset = c("23:00:00", "03:00:00", "11:00:00"),
                                       wake = c("07:00:00", "17:00:00", "09:00:00")),
             nonwearlog = NULL, naplog = NULL, bedlog = NULL, imputecodelog = NULL,
             dateformat = "%Y-%m-%d")
  sr <- raw.sib.report(ts, ID = "A", epochlength = 5, logs_diaries = ld, desiredtz = "UTC")
  sl <- sr[sr$type == "sleeplog", ]
  expect_identical(nrow(sl), 3L)

  # night n is dated firstDate + n - 1; an AM wake moves to the next day
  expect_identical(format(sl$start[1]), "2025-10-07 23:00:00")
  expect_identical(format(sl$end[1]), "2025-10-08 07:00:00")
  expect_identical(sl$duration[1], 480)
  # night 2: an AM onset, and a wake between noon and 18:00 counts as AM too, so both move
  expect_identical(format(sl$start[2]), "2025-10-09 03:00:00")
  expect_identical(format(sl$end[2]), "2025-10-09 17:00:00")
  expect_identical(sl$duration[2], 840)
  # night 3: both AM, but the onset is later on the clock than the wake, so it stays put
  expect_identical(format(sl$start[3]), "2025-10-09 11:00:00")
  expect_identical(format(sl$end[3]), "2025-10-10 09:00:00")
  expect_identical(sl$duration[3], 1320)

  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, ID = "A", epochlength = 5, logs_diaries = ld,
                                            desiredtz = "UTC"))
  }
})

test_that("a recording that starts before 04:00 counts the previous date as the first night", {
  ld <- list(sleeplog = sleeplog_frame(onset = "23:00:00", wake = "07:00:00"),
             nonwearlog = NULL, naplog = NULL, bedlog = NULL, imputecodelog = NULL,
             dateformat = "%Y-%m-%d")
  late <- synth_ts(n = 100, start = "2025-10-08 03:59:55", tz = "UTC")
  early <- synth_ts(n = 100, start = "2025-10-08 04:00:00", tz = "UTC")
  sr_late <- raw.sib.report(late, "A", 5, ld, desiredtz = "UTC")
  sr_early <- raw.sib.report(early, "A", 5, ld, desiredtz = "UTC")
  # hour 3 is below 4, so firstDate steps back a day
  expect_identical(format(sr_late$start[1]), "2025-10-07 23:00:00")
  expect_identical(format(sr_late$end[1]), "2025-10-08 07:00:00")
  expect_identical(format(sr_early$start[1]), "2025-10-08 23:00:00")
  expect_identical(format(sr_early$end[1]), "2025-10-09 07:00:00")
  if (ggir_available()) {
    expect_identical(sr_late, GGIR:::g.sibreport(late, "A", 5, ld, desiredtz = "UTC"))
    expect_identical(sr_early, GGIR:::g.sibreport(early, "A", 5, ld, desiredtz = "UTC"))
  }
})

test_that("a nap that crosses midnight is dated on one day, so sort() inverts it", {
  # GGIR dates both ends on the nap's own date and sorts them; reproduced, not fixed
  ts <- synth_ts(n = 100, start = "2025-10-07 20:30:00", tz = "UTC")
  ld <- list(sleeplog = NULL, nonwearlog = NULL, bedlog = NULL, imputecodelog = NULL,
             naplog = naplog_frame("2025-10-07", "23:00:00", "01:00:00"),
             dateformat = "%Y-%m-%d")
  sr <- raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC")
  expect_identical(nrow(sr), 1L)
  expect_identical(sr$type, "nap")
  expect_identical(format(sr$start), "2025-10-07 01:00:00")
  expect_identical(format(sr$end), "2025-10-07 23:00:00")
  expect_identical(sr$duration, 1320)
  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, "A", 5, ld, desiredtz = "UTC"))
  }
})

test_that("a nap wholly inside one day keeps its own clock times", {
  ts <- synth_ts(n = 100, start = "2025-10-07 20:30:00", tz = "UTC")
  ld <- list(sleeplog = NULL, nonwearlog = NULL, bedlog = NULL, imputecodelog = NULL,
             naplog = naplog_frame(c("2025-10-07", "2025-10-08"),
                                   c("13:00:01", "13:00:02"), c("13:30:00", "13:30:00")),
             dateformat = "%Y-%m-%d")
  sr <- raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC")
  expect_identical(format(sr$start), c("2025-10-07 13:00:01", "2025-10-08 13:00:02"))
  expect_identical(format(sr$end), c("2025-10-07 13:30:00", "2025-10-08 13:30:00"))
  expect_identical(round(sr$duration, 5), c(29.98333, 29.96667))
  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, "A", 5, ld, desiredtz = "UTC"))
  }
})

test_that("the minute-before guard skips a bout that starts at waking epoch 13", {
  ts <- synth_ts(n = 100, tz = "UTC")
  ts$ACC <- seq_len(100) * 1.0
  ts$sibdetection[13:20] <- 1
  exact <- raw.sib.report(ts, "A", 5, desiredtz = "UTC")
  fixed <- raw.sib.report(ts, "A", 5, desiredtz = "UTC", ggir_exact = FALSE)
  # GGIR's guard is "> 1", so minute_before 1:12 is skipped and the initial 0 stands
  expect_identical(exact$mean_acc_1min_before, 0)
  # with ">= 1" it is mean(1:12)
  expect_identical(fixed$mean_acc_1min_before, 6.5)
  expect_identical(exact$mean_acc_1min_after, fixed$mean_acc_1min_after)
  expect_identical(exact$duration, 8 / 12)
  if (ggir_available()) {
    expect_identical(exact, GGIR:::g.sibreport(ts, "A", 5, c(), desiredtz = "UTC"))
  }
})

test_that("the minute-after guard compares against the wrong length and yields NA", {
  # dayind has 50 entries while ts has 100 rows; GGIR's guard compares 57 with
  # nrow(ts), so dayind[51:57] is NA and so is the mean
  ts <- synth_ts(n = 100, tz = "UTC")
  ts$diur <- rep(c(1, 0), each = 50)
  ts$sibdetection[which(ts$diur == 0)[44:45]] <- 1
  exact <- raw.sib.report(ts, "A", 5, desiredtz = "UTC")
  fixed <- raw.sib.report(ts, "A", 5, desiredtz = "UTC", ggir_exact = FALSE)
  expect_identical(nrow(exact), 1L)
  expect_true(is.na(exact$mean_acc_1min_after))
  # the corrected guard compares with length(dayind), so the initial 0 stands
  expect_identical(fixed$mean_acc_1min_after, 0)
  expect_identical(exact$mean_acc_1min_before, fixed$mean_acc_1min_before)
  if (ggir_available()) {
    expect_identical(exact, GGIR:::g.sibreport(ts, "A", 5, c(), desiredtz = "UTC"))
  }
})

test_that("a diary row with no usable timestamps re-appends the previous row's block", {
  ts <- synth_ts(n = 100, tz = "UTC")
  bad_second <- data.frame(ID = c("A", "A"), date = c("2025-10-07", "2025-10-08"),
                           nap1 = c("22:00:00", " "), nap1b = c("23:00:00", " "),
                           stringsAsFactors = FALSE)
  names(bad_second)[3:4] <- c("nap1", "nap1")
  ld <- list(naplog = bad_second, dateformat = "%Y-%m-%d")
  exact <- raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC")
  # a space passes the non-empty test but the gsub strips it, so row 1's block is appended again
  expect_identical(nrow(exact), 2L)
  expect_identical(exact$start[1], exact$start[2])
  expect_identical(exact$end[1], exact$end[2])
  fixed <- raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC", ggir_exact = FALSE)
  expect_identical(nrow(fixed), 1L)
  if (ggir_available()) {
    expect_identical(exact, GGIR:::g.sibreport(ts, "A", 5, ld, desiredtz = "UTC"))
  }
})

test_that("when the FIRST diary row has no usable timestamps both GGIR and the port error", {
  ts <- synth_ts(n = 100, tz = "UTC")
  bad_first <- data.frame(ID = c("A", "A"), date = c("2025-10-07", "2025-10-08"),
                          nap1 = c(" ", "22:00:00"), nap1b = c(" ", "23:00:00"),
                          stringsAsFactors = FALSE)
  names(bad_first)[3:4] <- c("nap1", "nap1")
  ld <- list(naplog = bad_first, dateformat = "%Y-%m-%d")
  expect_error(raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC"),
               "object 'logreport_tmp' not found", fixed = TRUE)
  if (ggir_available()) {
    expect_error(GGIR:::g.sibreport(ts, "A", 5, ld, desiredtz = "UTC"),
                 "object 'logreport_tmp' not found", fixed = TRUE)
  }
  fixed <- raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC", ggir_exact = FALSE)
  expect_identical(nrow(fixed), 1L)
  expect_identical(format(fixed$start), "2025-10-08 22:00:00")
})

test_that("an ISO 8601 character time column is converted, as GGIR does", {
  n <- 100
  posix <- as.POSIXct("2025-10-07 20:30:00", tz = "UTC") + seq(0, by = 5, length.out = n)
  ts <- data.frame(time = format(posix, "%Y-%m-%dT%H:%M:%S%z"), ACC = rep(10, n),
                   diur = rep(0, n), sibdetection = rep(0, n), stringsAsFactors = FALSE)
  ts$sibdetection[20:31] <- 1
  sr <- raw.sib.report(ts, "A", 5, desiredtz = "UTC")
  expect_s3_class(sr$start, "POSIXct")
  expect_identical(format(sr$start), "2025-10-07 20:31:35")
  expect_identical(format(sr$end), "2025-10-07 20:32:30")
  expect_identical(sr$duration, 1)
  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, "A", 5, c(), desiredtz = "UTC"))
  }
})

test_that("a report with nothing in it returns an empty frame, where GGIR returns a list", {
  ts <- synth_ts(n = 100, tz = "UTC")
  sr <- raw.sib.report(ts, "A", 5, desiredtz = "UTC")
  expect_s3_class(sr, "data.frame")
  expect_identical(dim(sr), c(0L, 7L))
  expect_identical(names(sr), c("ID", "type", "start", "end", "duration",
                                "mean_acc_1min_before", "mean_acc_1min_after"))
  expect_s3_class(sr$start, "POSIXct")
  expect_identical(attr(sr$start, "tzone"), "UTC")
  if (ggir_available()) {
    # the one deliberate difference from GGIR
    theirs <- GGIR:::g.sibreport(ts, "A", 5, c(), desiredtz = "UTC")
    expect_type(theirs, "list")
    expect_false(is.data.frame(theirs))
    expect_identical(names(theirs), c("start", "end"))
    expect_identical(length(theirs$start), 0L)
  }
})

test_that("a diary with no sib at all still produces the five diary columns", {
  ts <- synth_ts(n = 100, tz = "UTC")
  ld <- list(sleeplog = NULL, nonwearlog = NULL, bedlog = NULL, imputecodelog = NULL,
             naplog = naplog_frame("2025-10-07", "13:00:00", "13:30:00"),
             dateformat = "%Y-%m-%d")
  sr <- raw.sib.report(ts, "A", 5, ld, desiredtz = "UTC")
  expect_identical(names(sr), c("ID", "type", "start", "end", "duration"))
  expect_identical(nrow(sr), 1L)
  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, "A", 5, ld, desiredtz = "UTC"))
  }
})

test_that("a diary ID that matches nothing contributes no rows", {
  skip_without_ref(mos2_series())
  ts <- mos2_ts()
  ld <- list(sleeplog = sleeplog_frame(onset = "23:00:00", wake = "07:00:00", id = "someone_else"),
             nonwearlog = NULL, naplog = NULL, bedlog = NULL, imputecodelog = NULL,
             dateformat = "%Y-%m-%d")
  sr <- raw.sib.report(ts, ID = MOS2_ID, epochlength = 5, logs_diaries = ld, desiredtz = "")
  expect_identical(nrow(sr), 66L)
  expect_identical(unique(sr$type), "sib")
  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, MOS2_ID, 5, ld, desiredtz = ""))
  }
})

test_that("the epoch length drives both the window size and the minutes conversion", {
  # at 60 s epochs the one-minute windows are one epoch wide
  n <- 200
  ts <- data.frame(time = as.POSIXct("2025-10-07 20:30:00", tz = "UTC") + seq(0, by = 60,
                                                                              length.out = n),
                   ACC = seq_len(n) * 1.0, diur = rep(0, n), sibdetection = rep(0, n))
  ts$sibdetection[50:59] <- 1
  sr <- raw.sib.report(ts, "A", 60, desiredtz = "UTC")
  expect_identical(sr$duration, 10)
  expect_identical(sr$mean_acc_1min_before, 49)
  expect_identical(sr$mean_acc_1min_after, 60)
  if (ggir_available()) {
    expect_identical(sr, GGIR:::g.sibreport(ts, "A", 60, c(), desiredtz = "UTC"))
  }
})

test_that("bad input is named rather than silently reported as no bouts", {
  ts <- synth_ts(n = 50, tz = "UTC")
  expect_error(raw.sib.report(as.matrix(1:10), "A", 5), "must be a data.frame", fixed = TRUE)
  expect_error(raw.sib.report(ts[, c("time", "ACC")], "A", 5), "sibdetection", fixed = TRUE)
  expect_error(raw.sib.report(ts, c("A", "B"), 5), "single, non-missing identifier",
               fixed = TRUE)
  expect_error(raw.sib.report(ts, "A", 0), "positive number of seconds", fixed = TRUE)
  expect_error(raw.sib.report(ts, "A", NA_real_), "positive number of seconds", fixed = TRUE)
})

test_that("the internal helpers behave as their GGIR originals do", {
  # .raw.is.iso8601 lives in R/raw_timeuse_series.R
  expect_true(is.function(.raw.is.iso8601))
  expect_true(.raw.is.iso8601("2025-10-07T21:11:00-0800"))   # NNeg 4
  expect_true(.raw.is.iso8601("2025-10"))                    # NNeg 2
  expect_true(.raw.is.iso8601("2025+10"))                    # NPos 2
  expect_false(.raw.is.iso8601("2025-10-07"))                # NNeg 3
  expect_false(.raw.is.iso8601("2025-10-07 21:11:00"))       # NNeg 3
  expect_false(.raw.is.iso8601(as.POSIXct("2025-10-07", tz = "UTC")))
  expect_identical(.raw.sib.report.convert2posix("2025-10-08"), "2025-10-08 00:00:00")
  expect_identical(.raw.sib.report.convert2posix("2025-10-08 07:00:00"), "2025-10-08 07:00:00")
  x <- data.frame(start = c("2025-10-07 23:00:00", "2025-10-08 06:00:00"),
                  stringsAsFactors = FALSE)
  dated <- .raw.sib.report.add.date(x, tz = "UTC")
  expect_identical(dated$date, as.Date(c("2025-10-07", "2025-10-07")))
  if (ggir_available()) {
    expect_identical(.raw.is.iso8601("2025-10-07T21:11:00-0800"),
                     GGIR:::is.ISO8601("2025-10-07T21:11:00-0800"))
    expect_identical(.raw.is.iso8601("2025-10-07 21:11:00"),
                     GGIR:::is.ISO8601("2025-10-07 21:11:00"))
  }
})

test_that("F-P5-2: the stored diary run's sib report is reproduced", {
  testthat::skip_if_not(nzchar(p5_fixtures) && dir.exists(p5_fixtures),
                        "the part-5 fixtures are not available yet")
  csvs <- list.files(p5_fixtures, pattern = "^sib_report_.*\\.csv$", recursive = TRUE,
                     full.names = TRUE)
  series <- list.files(p5_fixtures, pattern = "_T5A5\\.RData$", recursive = TRUE,
                       full.names = TRUE)
  series <- series[grepl("ms5\\.outraw", series) & !grepl("sib\\.reports", series)]
  testthat::skip_if_not(length(csvs) > 0 && length(series) > 0,
                        "no F-P5-2 sib report and series pair in the fixtures folder")
  checked <- 0L
  with_diary <- 0L
  for (csv in csvs) {
    root <- sub("meta[/\\\\]ms5\\.outraw.*$", "", csv)
    mates <- series[startsWith(series, root)]
    if (length(mates) == 0) next
    env <- new.env()
    load(mates[1], envir = env)
    mdat <- env$mdat
    ts <- data.frame(time = mdat$timestamp, ACC = mdat$ACC, diur = mdat$SleepPeriodTime,
                     sibdetection = mdat$sibdetection)
    ref <- utils::read.csv(csv, stringsAsFactors = FALSE)
    id <- ref$ID[1]
    epochlength <- as.numeric(difftime(mdat$timestamp[2], mdat$timestamp[1], units = "secs"))
    logpath <- list.files(file.path(root, "meta"), pattern = "^sleeplog", full.names = TRUE)
    ld <- NULL
    if (length(logpath) == 1) {
      lenv <- new.env()
      load(logpath[1], envir = lenv)
      ld <- lenv$logs_diaries
    }
    sr <- raw.sib.report(ts, ID = id, epochlength = epochlength, logs_diaries = ld,
                         desiredtz = env$desiredtz)
    expect_identical(nrow(sr), nrow(ref))
    expect_identical(names(sr), names(ref))
    expect_identical(sr$ID, ref$ID)
    expect_identical(sr$type, ref$type)
    expect_identical(format(sr$start), ref$start)
    expect_identical(format(sr$end), ref$end)
    expect_lt(max(abs(sr$duration - ref$duration)), 1e-9)
    # diary rows carry NA in both acceleration columns
    expect_identical(is.na(sr$mean_acc_1min_before), is.na(ref$mean_acc_1min_before))
    expect_identical(is.na(sr$mean_acc_1min_after), is.na(ref$mean_acc_1min_after))
    expect_identical(max(abs(sr$mean_acc_1min_before - ref$mean_acc_1min_before),
                         na.rm = TRUE), 0)
    expect_identical(max(abs(sr$mean_acc_1min_after - ref$mean_acc_1min_after),
                         na.rm = TRUE), 0)
    if (!is.null(ld)) {
      with_diary <- with_diary + 1L
      expect_true(any(ref$type != "sib"))
      expect_identical(sort(unique(sr$type)), sort(unique(ref$type)))
    }
    checked <- checked + 1L
  }
  testthat::skip_if_not(checked > 0, "no fixture sib report could be paired with a series")
  expect_gt(checked, 0L)
  expect_gt(with_diary, 0L)
})

test_that("V1: g.sibreport in the installed GGIR still has the body this port transcribes", {
  skip_without_ggir()
  # deparse() rewraps long calls, so whitespace is normalised first
  body_text <- gsub("[[:space:]]+", " ", paste(deparse(GGIR:::g.sibreport), collapse = " "))
  for (pat in c("dayind = which(ts$diur == 0)",
                "sib_starts = which(diff(c(0, ts$sibdetection[dayind], 0)) == 1)",
                "sib_ends = which(diff(c(ts$sibdetection[dayind], 0)) == -1)",
                "((sib_ends[sibi] - sib_starts[sibi]) + 1)/(60/epochlength)",
                "minute_before = (sib_starts[sibi] - (60/epochlength)):(sib_starts[sibi] - 1)",
                "minute_after = (sib_ends[sibi] + 1):(sib_ends[sibi] + (60/epochlength))",
                "if (min(minute_before) > 1)",
                "if (max(minute_after) < nrow(ts))",
                "digits = 3",
                "if (as.numeric(format(ts$time[1], \"%H\")) < 4)",
                "AM = which(hour <= 12)",
                "if (hour[2] > 12 & hour[2] <= 18)",
                "if (hour[1] > hour[2] && 1 %in% AM)",
                "if (hour[1] >= 12 & hour[2] < 12)",
                "by = c(\"ID\", \"date\")",
                "by = c(\"ID\", \"type\", \"start\", \"end\", \"duration\"), all = TRUE")) {
    expect_true(grepl(pat, body_text, fixed = TRUE), label = paste0("g.sibreport body: ", pat))
  }
  expect_identical(names(formals(GGIR:::g.sibreport)),
                   c("ts", "ID", "epochlength", "logs_diaries", "desiredtz"))
})
