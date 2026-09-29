# Parity tests for R/raw_timeuse_windows.R against GGIR part 5's day windows: the midnight
# and night-trimming blocks of g.part5, g.part5.definedays, g.part5.wakesleepwindows,
# g.part5.addfirstwake, g.part5.fixmissingnight and g.part5.onsetwaketiming. Reference data
# are found through CANHRACTI_GGIR_REF, with the parts 3 and 4 fixtures, the part-5 fixtures
# and the 3.3-9 clone next to it; every block skips without them, and the live comparisons
# skip when GGIR is not installed.

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
skip_if_no_dir <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    testthat::skip(paste0("reference folder not found: ", path))
  }
}

ref_root <- function() sub("/+$", "", .ggir_ref)
p34_fixture <- function(...) {
  file.path(dirname(ref_root()), "ggir-study-p34", "fixtures", ...)
}
p5_fixture_root <- function() {
  file.path(dirname(ref_root()), "ggir-study-p5", "fixtures")
}
# Any GGIR output folder under a fixture root that has the four milestones part 5 reads.
tuw_meta_dirs <- function(root) {
  if (!dir.exists(root)) return(character(0))
  cand <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  cand <- cand[basename(cand) == "meta"]
  keep <- vapply(cand, function(d) {
    all(dir.exists(file.path(d, c("basic", "ms2.out", "ms3.out", "ms4.out", "ms5.out")))) &&
      length(dir(file.path(d, "ms2.out"))) > 0 &&
      length(dir(file.path(d, "ms5.out"))) > 0 &&
      dir.exists(file.path(d, "ms5.outraw", "40_100_400"))
  }, logical(1))
  unname(cand[keep])
}
clone_definedays <- function() {
  # The GGIR 3.3-9 clone, used only to check segment_all_windows = TRUE.
  file.path(dirname(ref_root()), "ggir-src", "GGIR", "R", "g.part5.definedays.R")
}

tuw_loadenv <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  as.list(e)
}

# The milestones are large and shared by several tests, so both builders are memoised.
.tuw_cache <- new.env(parent = emptyenv())
tuw_cached <- function(key, expr) {
  if (!exists(key, envir = .tuw_cache, inherits = FALSE)) {
    assign(key, expr, envir = .tuw_cache)
  }
  get(key, envir = .tuw_cache, inherits = FALSE)
}

# One reference recording, rebuilt from the stored objects so the T1 tests need no GGIR.
# MOS2 and EE stored the whole series, so sibdetection is read back from it; the parts 3
# and 4 fixtures stored a filtered series, so sibdetection is recomputed with GGIR's addsib.
tuw_recording <- function(metadir, sibDef = NULL) {
  tuw_cached(paste0("rec:", metadir), .tuw_recording_build(metadir, sibDef))
}
.tuw_recording_build <- function(metadir, sibDef = NULL) {
  fn <- dir(file.path(metadir, "ms2.out"))[1]
  imp <- tuw_loadenv(file.path(metadir, "ms2.out", fn))
  bas <- tuw_loadenv(file.path(metadir, "basic", paste0("meta_", fn)))
  ms3 <- tuw_loadenv(file.path(metadir, "ms3.out", fn))
  ms4 <- tuw_loadenv(file.path(metadir, "ms4.out", fn))
  ms5 <- tuw_loadenv(file.path(metadir, "ms5.out", fn))
  rawdir <- file.path(metadir, "ms5.outraw", "40_100_400")
  mdat <- tuw_loadenv(file.path(rawdir, dir(rawdir)[1]))$mdat
  if (is.null(sibDef)) sibDef <- unique(ms3$sib.cla.sum$definition)[1]
  tz <- bas$desiredtz_part1
  ws3 <- imp$IMP$windowsizes[1]
  nts <- nrow(imp$IMP$metashort)
  full_series <- nrow(mdat) == nts
  ts <- data.frame(time = imp$IMP$metashort$timestamp,
                   guider = rep("unknown", nts),
                   angle = as.numeric(imp$IMP$metashort$anglez),
                   nonwear = rep(imp$IMP$rout[, 5],
                                 each = imp$IMP$windowsizes[2] / imp$IMP$windowsizes[1]),
                   diur = 0,
                   stringsAsFactors = FALSE)
  if (full_series) {
    ts$sibdetection <- mdat$sibdetection
  } else {
    if (!requireNamespace("GGIR", quietly = TRUE)) {
      testthat::skip("stored series is filtered and GGIR is absent, cannot rebuild sibs")
    }
    nightsi <- .raw.timeuse.midnights(.raw.iso8601.to.posix(ts$time, tz = tz), 0,
                                      ts$time, tz)$nightsi
    ts <- GGIR:::g.part5.addsib(ts, epochSize = ws3,
                                part3_output = ms3$sib.cla.sum[
                                  ms3$sib.cla.sum$definition == sibDef, ],
                                desiredtz = tz, sibDefinition = sibDef, nightsi = nightsi)
  }
  list(ts = ts, mdat = mdat, output = ms5$output, tz = tz, ws3 = ws3, full_series = full_series,
       ID = sub("[.]RData$", "", fn), sibDef = sibDef,
       nights = ms4$nightsummary[ms4$nightsummary$sleepparam == sibDef, ],
       SPTE_end = ms3$SPTE_end, IMP = imp$IMP, M = bas$M,
       sib.cla.sum = ms3$sib.cla.sum, longitudinal_axis = ms3$longitudinal_axis)
}

MOS2_META <- function() file.path(ref_root(), "out", "output_din", "meta")
EE_META <- function() file.path(ref_root(), "timing_out", "output_timing", "meta")

test_that("midnights on MOS2 are the seven local midnights, and dayborder shifts only nightsi", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  tp <- .raw.iso8601.to.posix(r$ts$time, tz = r$tz)

  mn <- .raw.timeuse.midnights(tp, dayborder = 0, time_char = r$ts$time, desiredtz = r$tz)
  expect_identical(mn$nightsi, c(2521L, 19801L, 37081L, 54361L, 71641L, 88921L, 106201L))
  expect_identical(mn$nightsi2, mn$nightsi)
  expect_identical(length(mn$sec), nrow(r$ts))
  # the clock vectors are read by index downstream, so pin one epoch of each
  expect_identical(c(mn$hour[1], mn$min[1], mn$sec[1]), c(20L, 30L, 0))
  expect_identical(c(mn$hour[2521], mn$min[2521], mn$sec[2521]), c(0L, 0L, 0))
  # sleeponset 22:59:10 on MM window 1 is epoch 1791
  expect_equal(mn$hour[1791] + mn$min[1791] / 60 + mn$sec[1791] / 3600, 22.9861111111111,
               tolerance = 1e-12)

  mn4 <- .raw.timeuse.midnights(tp, dayborder = 4, time_char = r$ts$time, desiredtz = r$tz)
  expect_identical(mn4$nightsi, c(5401L, 22681L, 39961L, 57241L, 74521L, 91801L, 109081L))
  expect_identical(mn4$nightsi2, mn$nightsi) # nightsi2 is always the true midnights
})

test_that("midnights match GGIR's own block on MOS2 and EE, at dayborder 0 and 4", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  for (metadir in c(MOS2_META(), EE_META())) {
    skip_if_no_dir(metadir)
    r <- tuw_recording(metadir)
    tp <- .raw.iso8601.to.posix(r$ts$time, tz = r$tz)
    tempp <- as.POSIXlt(GGIR:::iso8601chartime2POSIX(r$ts$time, tz = r$tz))
    for (db in c(0, 4)) {
      ggir_nightsi <- if (db == 0) {
        which(tempp$sec == 0 & tempp$min == 0 & tempp$hour == 0)
      } else {
        which(tempp$sec == 0 & tempp$min == (db - floor(db)) * 60 & tempp$hour == floor(db))
      }
      mn <- .raw.timeuse.midnights(tp, db, r$ts$time, r$tz)
      expect_identical(mn$nightsi, ggir_nightsi)
    }
    mn <- .raw.timeuse.midnights(tp, 0, r$ts$time, r$tz)
    expect_identical(mn$sec, tempp$sec)
    expect_identical(mn$min, tempp$min)
    expect_identical(mn$hour, tempp$hour)
  }
})

test_that("wakesleep rebuilds the stored SPT epoch for epoch on MOS2, half open at the wake", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  ts <- .raw.timeuse.wakesleep(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID,
                               3600 / r$ws3)
  expect_identical(ts$diur, r$mdat$SleepPeriodTime)
  expect_identical(sum(ts$diur), 17903)
  SO <- which(diff(ts$diur) == 1)
  FM <- which(diff(ts$diur) == -1)
  expect_identical(SO, c(1790L, 19449L, 36913L, 52945L))
  expect_identical(FM, c(7689L, 25392L, 42100L, 53819L))
  # night 2 is 5943 epochs = 8.254167 h, part 4's SptDuration; the wake epoch itself is not SPT
  expect_identical(FM[2] - (SO[2] + 1) + 1, 5943)
  expect_equal((FM[2] - (SO[2] + 1) + 1) * r$ws3 / 3600, r$nights$SptDuration[2],
               tolerance = 1e-6)
  expect_identical(ts$diur[FM[2]], 1)
  expect_identical(ts$diur[FM[2] + 1], 0)
})

test_that("wakesleep and addfirstwake are identical to GGIR on MOS2, EE and the fixtures", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  withr::local_locale(c(LC_TIME = "C"))
  dirs <- c(MOS2_META(), EE_META(),
            p34_fixture("daysleeper", "output_din", "meta"),
            p34_fixture("spring", "output_din", "meta"),
            p34_fixture("autumn", "output_din", "meta"),
            p34_fixture("autumn_edge", "output_din", "meta"),
            p34_fixture("novalid_keepsibs", "output_din", "meta"))
  dirs <- dirs[dir.exists(dirs)]
  skip_if(length(dirs) < 2, "fewer than two reference folders present")
  for (metadir in dirs) {
    r <- tuw_recording(metadir)
    mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0,
                                 r$ts$time, r$tz)
    Neph <- 3600 / r$ws3
    g <- GGIR:::g.part5.wakesleepwindows(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3,
                                         r$ID, Neph)
    o <- .raw.timeuse.wakesleep(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID, Neph)
    expect_identical(o, g, info = metadir)
    g2 <- GGIR:::g.part5.addfirstwake(o, r$nights, mn$nightsi, c(), r$ID, Neph, r$SPTE_end)
    o2 <- .raw.timeuse.addfirstwake(o, r$nights, mn$nightsi, c(), r$ID, Neph, r$SPTE_end)
    expect_identical(o2, g2, info = metadir)
  }
})

test_that("wakesleep keeps GGIR's operator precedence: a blank sleeponset_ts with no diary", {
  # & binds tighter than |, so the guard is (sleeplog and nonwear > 0.33) or
  # sleeponset_ts == ""; the second arm fires with no diary and leaves s0 and s1 untouched
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  blanked <- r$nights
  blanked$sleeponset_ts[2] <- ""
  g <- GGIR:::g.part5.wakesleepwindows(r$ts, blanked, r$tz, mn$nightsi2, c(), r$ws3, r$ID, 720)
  o <- .raw.timeuse.wakesleep(r$ts, blanked, r$tz, mn$nightsi2, c(), r$ws3, r$ID, 720)
  expect_identical(o, g)
  expect_identical(sum(o$diur), 29663) # the blanked night now opens at midnight
})

test_that("addfirstwake is a no-op on MOS2 as stored", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  ts <- .raw.timeuse.wakesleep(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID, 720)
  out <- .raw.timeuse.addfirstwake(ts, r$nights, mn$nightsi, c(), r$ID, 720, r$SPTE_end)
  expect_identical(out, ts)
  expect_identical(sum(out$diur), 17903)
  expect_true(all(out$guider == "unknown"))
})

test_that("addfirstwake repairs a deleted first night through the part-3 estimate", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  nights <- r$nights[r$nights$night != 1, ]
  ts <- .raw.timeuse.wakesleep(r$ts, nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID, 720)
  firstwake <- which(diff(ts$diur) == -1)[1]
  expect_identical(firstwake, 25392L)
  expect_true(firstwake > mn$nightsi[2]) # 19801, the entry condition
  stripped <- r$SPTE_end[!is.na(r$SPTE_end)]
  expect_equal(stripped[1], 31.85972, tolerance = 1e-5)
  wake_night1_index <- mn$nightsi[1] + round((stripped[1] - 24) * 720)
  expect_identical(wake_night1_index, 8180)
  expect_identical(max(which(ts$sibdetection[1:(wake_night1_index - 1)] == 1)), 7690L)

  out <- .raw.timeuse.addfirstwake(ts, nights, mn$nightsi, c(), r$ID, 720, r$SPTE_end)
  expect_identical(out$diur[1:7690], rep(1, 7690))
  expect_identical(out$diur[7691], 0)
  expect_identical(unique(out$guider[1:7690]), "part3_estimate")
  expect_identical(out$guider[7691], "unknown")
  expect_identical(sum(out$diur), 19694)
})

test_that("addfirstwake errors on an empty SPTE_end, as GGIR does", {
  # if (is.na(c()[1])) is if (logical(0)); reproduced, not fixed
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  ts <- .raw.timeuse.wakesleep(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID, 720)
  expect_error(.raw.timeuse.addfirstwake(ts, r$nights, mn$nightsi, c(), r$ID, 720, c()),
               "argument is of length zero")
})

test_that("addfirstwake returns unchanged when there are fewer than two midnights", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  ts <- .raw.timeuse.wakesleep(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID, 720)
  expect_identical(.raw.timeuse.addfirstwake(ts, r$nights, mn$nightsi[1], c(), r$ID, 720,
                                             r$SPTE_end), ts)
})

test_that("fixmissingnight splices a placeholder row for a hole at night 2", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  hole <- r$nights[r$nights$night != 2, ]
  out <- .raw.timeuse.fixmissingnight(hole, sleeplog = c(), ID = r$ID)
  expect_identical(nrow(out), 4L)
  expect_identical(out$night, c(1, 2, 3, 4))
  new <- out[out$night == 2, ]
  expect_identical(new$calendar_date, "8/10/2025") # 7/10/2025 plus 36 hours, zeros stripped
  expect_identical(new$cleaningcode, 5)
  expect_identical(new$acc_available, 0)
  expect_identical(new$daysleeper, 0)
  expect_identical(new$sleeplog_used, 0)
  expect_identical(new$guider, "nosleeplog_accnotworn")
  expect_identical(new$ID, r$nights$ID[1])
  expect_identical(new$sleepparam, r$sibDef)
  kept <- c("ID", "night", "sleepparam", "filename", "filename_dir", "foldername",
            "calendar_date", "daysleeper", "acc_available", "cleaningcode", "sleeplog_used",
            "guider")
  expect_true(all(is.na(unlist(new[, setdiff(names(new), kept)]))))
  expect_identical(.raw.timeuse.fixmissingnight(r$nights, c(), r$ID), r$nights)
})

test_that("fixmissingnight reproduces GGIR on holes, and on the diary branch", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  r <- tuw_recording(MOS2_META())
  hole <- r$nights[r$nights$night != 2, ]
  expect_identical(.raw.timeuse.fixmissingnight(hole, c(), r$ID),
                   GGIR:::g.part5.fixmissingnight(hole, c(), r$ID))
  # two holes: mi indexes the missing-night position, not the row, so the second repaired
  # row gets the wrong date; reproduced, not fixed
  twoholes <- rbind(r$nights[r$nights$night %in% c(1, 3), ], r$nights[4, ])
  twoholes$night <- c(1, 3, 5)
  o2 <- .raw.timeuse.fixmissingnight(twoholes, c(), r$ID)
  expect_identical(o2, GGIR:::g.part5.fixmissingnight(twoholes, c(), r$ID))
  expect_identical(o2$night, c(1, 2, 3, 4, 5))
  expect_identical(o2$calendar_date[5], o2$calendar_date[4]) # the defect, visible
  # a diary night at normal hours fills the numbers but leaves sleeplog_used 0 and the _ts
  # columns NA, because the writes sit inside if (sleeplogwake_hr > 36)
  diary <- data.frame(ID = r$ID, night = 2, sleeponset = "23:30:00", sleepwake = "07:30:00",
                      stringsAsFactors = FALSE)
  d1 <- .raw.timeuse.fixmissingnight(hole, diary, r$ID)
  expect_identical(d1, GGIR:::g.part5.fixmissingnight(hole, diary, r$ID))
  expect_identical(d1$sleeponset[2], 23.5)
  expect_identical(d1$wakeup[2], 7.5)
  expect_identical(d1$sleeplog_used[2], 0)
  expect_identical(d1$guider[2], "nosleeplog_accnotworn")
  expect_true(is.na(d1$sleeponset_ts[2]))
  # a day sleeper above 36 takes the other path
  diary2 <- data.frame(ID = r$ID, night = 2, sleeponset = "28:30:00", sleepwake = "37:30:00",
                       stringsAsFactors = FALSE)
  d2 <- .raw.timeuse.fixmissingnight(hole, diary2, r$ID)
  expect_identical(d2, GGIR:::g.part5.fixmissingnight(hole, diary2, r$ID))
  expect_identical(d2$daysleeper[2], 1)
  expect_identical(d2$sleeplog_used[2], 1)
  expect_identical(d2$guider[2], "sleeplog")
  expect_identical(c(d2$sleeponset_ts[2], d2$wakeup_ts[2]), c("28:30:00", "37:30:00"))
})

# The series with diur set and time as POSIXct, the state g.part5 is in when it enters the
# timewindow loop.
tuw_window_state <- function(metadir) {
  tuw_cached(paste0("win:", metadir), .tuw_window_state_build(metadir))
}
.tuw_window_state_build <- function(metadir) {
  r <- tuw_recording(metadir)
  mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0, r$ts$time, r$tz)
  ts <- .raw.timeuse.wakesleep(r$ts, r$nights, r$tz, mn$nightsi2, c(), r$ws3, r$ID,
                               3600 / r$ws3)
  ts <- .raw.timeuse.addfirstwake(ts, r$nights, mn$nightsi, c(), r$ID, 3600 / r$ws3,
                                  r$SPTE_end)
  if (r$full_series) stopifnot(identical(ts$diur, r$mdat$SleepPeriodTime))
  ts$time <- .raw.iso8601.to.posix(ts$time, tz = r$tz)
  r$mn <- mn
  r$ts <- ts
  r$FM <- which(diff(ts$diur) == -1)
  r$SO <- which(diff(ts$diur) == 1)
  r
}

test_that("night trimming keeps the documented midnights on MOS2", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(MOS2_META())
  mm <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "MM", s$ws3, s$FM, s$SO)
  ww <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "WW", s$ws3, s$FM, s$SO)
  oo <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "OO", s$ws3, s$FM, s$SO)
  expect_identical(mm$nightsi, c(2521L, 19801L, 37081L, 54361L))
  expect_identical(mm$Nwindows, 5) # length(nightsi) + 1, so a double, as in GGIR
  expect_identical(ww$nightsi, c(19801L, 37081L))
  expect_identical(ww$Nwindows, 4L)
  expect_identical(oo$nightsi, c(2521L, 19801L, 37081L))
  expect_identical(oo$Nwindows, 4L)
  # the MM twelve-hour trim deletes calendar days, which then appear in no part-5 output
  dropped <- setdiff(s$mn$nightsi, mm$nightsi)
  expect_identical(dropped, c(71641L, 88921L, 106201L))
  expect_identical(format(s$ts$time[dropped], "%Y-%m-%d"),
                   c("2025-10-12", "2025-10-13", "2025-10-14"))
  # 2025-10-11 survives the trim but never opens a window, because lastDay fires at wi = 4
  expect_false(any(s$output$calendar_date %in% c("2025-10-11", "2025-10-12", "2025-10-13",
                                                 "2025-10-14")))
  # an unrecognised timewindow raises rather than hanging the caller
  expect_error(.raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "ZZ", s$ws3), "MM, WW or OO")
})

test_that("MM, WW and OO windows on MOS2 are the stored ones, with the stored labels", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(MOS2_META())
  expected <- list(
    MM = list(qqq = list(c(1, 2520), c(2521, 19800), c(19801, 37080), c(37081, 54360)),
              last = c(FALSE, FALSE, FALSE, TRUE),
              lab = rep("00:00:00-23:59:55", 4)),
    WW = list(qqq = list(c(7690, 25392), c(25393, 42100), c(42101, 53819), c(NA, NA)),
              last = c(FALSE, FALSE, FALSE, TRUE),
              lab = c("07:10:45-07:45:55", "07:46:00-06:58:15", "06:58:20-23:14:50", NA)),
    OO = list(qqq = list(c(1791, 19449), c(19450, 36913), c(36914, 52945), c(NA, NA)),
              last = c(FALSE, FALSE, FALSE, TRUE),
              lab = c("22:59:10-23:30:40", "23:30:45-23:46:00", "23:46:05-22:02:00", NA)))
  for (tw in names(expected)) {
    tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, tw, s$ws3, s$FM, s$SO)
    for (wi in seq_along(expected[[tw]]$qqq)) {
      d <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows, c(0, 24),
                                    s$ID)
      if (is.na(expected[[tw]]$lab[wi])) {
        # the WW/OO tail window: qqq is GGIR's logical c(NA, NA) and there are no segments
        expect_identical(d$qqq, c(NA, NA), info = paste(tw, wi))
        expect_identical(d$segments, c())
        expect_identical(d$segments_names, c())
      } else {
        expect_identical(d$qqq, as.numeric(expected[[tw]]$qqq[[wi]]), info = paste(tw, wi))
        expect_identical(names(d$segments), expected[[tw]]$lab[wi], info = paste(tw, wi))
        expect_identical(d$segments_names, tw, info = paste(tw, wi))
        # MM's single segment is range(fullQqq), an integer pair; WW and OO's is qqq itself
        expect_identical(d$segments[[1]],
                         if (tw == "MM") as.integer(expected[[tw]]$qqq[[wi]]) else
                           as.numeric(expected[[tw]]$qqq[[wi]]), info = paste(tw, wi))
      }
      expect_identical(d$lastDay, expected[[tw]]$last[wi], info = paste(tw, wi))
    }
  }
  expect_identical(s$output$start_end_window,
                   c(rep("00:00:00-23:59:55", 4), "07:10:45-07:45:55", "07:46:00-06:58:15",
                     "06:58:20-23:14:50"))
  # window_number restarts at 1 per timewindow
  expect_identical(s$output$window_number, c("1", "2", "3", "4", "1", "2", "3"))
})

test_that("define.days and onsetwake are identical to GGIR across MM, WW and OO", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  withr::local_locale(c(LC_TIME = "C"))
  dirs <- c(MOS2_META(), EE_META(),
            p34_fixture("daysleeper", "output_din", "meta"),
            p34_fixture("spring", "output_din", "meta"),
            p34_fixture("autumn", "output_din", "meta"))
  dirs <- dirs[dir.exists(dirs)]
  skip_if(length(dirs) < 2, "fewer than two reference folders present")
  nwin <- 0L
  for (metadir in dirs) {
    s <- tuw_window_state(metadir)
    for (tw in c("MM", "WW", "OO")) {
      tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, tw, s$ws3, s$FM, s$SO)
      # GGIR's own trimming block, transcribed here as the reference
      ggir_nightsi <- s$mn$nightsi
      if (tw == "WW") {
        if (length(s$FM) > 0) {
          ggir_nightsi <- ggir_nightsi[ggir_nightsi > s$FM[1] &
                                         ggir_nightsi < s$FM[length(s$FM)]]
        }
      } else if (tw == "OO") {
        if (length(s$SO) > 0) {
          ggir_nightsi <- ggir_nightsi[ggir_nightsi > s$SO[1] &
                                         ggir_nightsi < s$SO[length(s$SO)]]
        }
      } else {
        se <- which(abs(diff(s$ts$diur)) == 1)
        n12 <- (60 / s$ws3) * 60 * 12
        ggir_nightsi <- ggir_nightsi[ggir_nightsi >= (se[1] - n12) &
                                       ggir_nightsi <= (se[length(se)] + n12)]
      }
      expect_identical(tr$nightsi, ggir_nightsi, info = paste(metadir, tw))
      wi <- 1
      lastDay <- ifelse(tr$Nwindows > 0 && length(tr$nightsi) > 0, FALSE, TRUE)
      while (lastDay == FALSE && wi <= 40) {
        g <- GGIR:::g.part5.definedays(tr$nightsi, wi, indjump = 1, epochSize = s$ws3,
                                       qqq_backup = c(), ts = s$ts, timewindowi = tw,
                                       Nwindows = tr$Nwindows, qwindow = c(0, 24),
                                       ID = s$ID, dayborder = 0)
        o <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows,
                                      c(0, 24), s$ID)
        expect_identical(o$qqq, g$qqq, info = paste(metadir, tw, wi))
        expect_identical(o$lastDay, g$lastDay, info = paste(metadir, tw, wi))
        expect_identical(o$segments, g$segments, info = paste(metadir, tw, wi))
        expect_identical(o$segments_names, g$segments_names, info = paste(metadir, tw, wi))
        if (length(which(is.na(o$qqq))) == 0 && (o$qqq[2] - o$qqq[1]) * s$ws3 > 900) {
          expect_identical(
            .raw.timeuse.onsetwake(o$qqq, s$ts, s$mn$min, s$mn$sec, s$mn$hour, tw),
            GGIR:::g.part5.onsetwaketiming(o$qqq, s$ts, s$mn$min, s$mn$sec, s$mn$hour, tw),
            info = paste(metadir, tw, wi))
        }
        nwin <- nwin + 1L
        lastDay <- o$lastDay
        wi <- wi + 1
      }
    }
  }
  expect_gt(nwin, 30L)
})

test_that("the WW or OO tail with only one wake returns cleanly instead of crashing", {
  # GGIR 3.3-9 runs if (qqq[2] >= Nts - 1) on c(NA, NA) and dies; the port follows 3.3.6
  # and ends the loop
  withr::local_locale(c(LC_TIME = "C"))
  t0 <- as.POSIXct("2025-03-01 19:00:00", tz = "America/Anchorage")
  ts <- data.frame(time = seq(t0, by = 60, length.out = 2160), diur = 0)
  ts$diur[500:700] <- 1
  expect_identical(which(diff(ts$diur) == -1), 700L)
  for (tw in c("WW", "OO")) {
    for (saw in c(FALSE, TRUE)) {
      d <- .raw.timeuse.define.days(c(301, 1741), 1, 60, ts, tw, 1, c(0, 24), "x",
                                    segment_all_windows = saw)
      expect_identical(d$qqq, c(NA, NA))
      expect_true(d$lastDay)
      expect_identical(d$segments, c())
    }
  }
  expect_error(.raw.timeuse.define.days(c(301, 1741), 1, 60, ts, "ZZ", 1, c(0, 24), "x"),
               "MM, WW or OO")
})

test_that("the 900 second window gate is strict and uses the epoch size", {
  # MOS2 cut to start n epochs before its first midnight, so MM window 1 is n epochs long:
  # 181 at 5 s is exactly 900 s and is dropped, 182 is kept; at 60 s the edge is 16 and 17
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  metadir <- MOS2_META()
  fn <- dir(file.path(metadir, "ms2.out"))[1]
  x <- read.ggir.milestone(file.path(metadir, "basic", paste0("meta_", fn)))
  x$imputed <- tuw_loadenv(file.path(metadir, "ms2.out", fn))$IMP
  ms3 <- tuw_loadenv(file.path(metadir, "ms3.out", fn))
  x$sleep <- list(sib.cla.sum = ms3$sib.cla.sum, SPTE_start = ms3$SPTE_start,
                  SPTE_end = ms3$SPTE_end, longitudinal_axis = ms3$longitudinal_axis,
                  tail_expansion_log = ms3$tail_expansion_log,
                  rec_starttime = ms3$rec_starttime, id = ms3$ID)
  nights <- tuw_loadenv(file.path(metadir, "ms4.out", fn))$nightsummary
  midnight <- grep("T00:00:00", x$imputed$metashort$timestamp)[1]
  per_long <- x$imputed$windowsizes[2] / x$imputed$windowsizes[1]
  first_window <- function(n, agg2_60) {
    keep <- (midnight - n):(midnight + 8639) # to noon, past the first wake
    long <- unique((keep - 1) %/% per_long + 1)
    xt <- x
    xt$imputed$metashort <- x$imputed$metashort[keep, ]
    xt$meta$metashort <- x$meta$metashort[keep, ]
    xt$imputed$rout <- x$imputed$rout[long, ]
    xt$meta$metalong <- x$meta$metalong[long, ]
    tu <- raw.timeuse(xt, nights, params = raw.params(
      ggir_version_label = "3.3.6", timewindow = "MM", do.sibreport = FALSE,
      save_ms5rawlevels = FALSE, part5_agg2_60seconds = agg2_60))
    w <- tu$windows[which(tu$windows$window_number == 1), ]
    list(w = w, in_output = any(tu$daysummary$window_number == "1"))
  }
  for (cs in list(list(n = 181, agg = FALSE, epochs = 181L, kept = FALSE, ws = 5),
                  list(n = 182, agg = FALSE, epochs = 182L, kept = TRUE, ws = 5),
                  list(n = 16 * 12, agg = TRUE, epochs = 16L, kept = FALSE, ws = 60),
                  list(n = 17 * 12, agg = TRUE, epochs = 17L, kept = TRUE, ws = 60))) {
    o <- first_window(cs$n, cs$agg)
    lab <- paste(cs$epochs, "epochs at", cs$ws, "s")
    expect_identical(nrow(o$w), 1L, info = lab)
    expect_identical(o$w$end_index - o$w$start_index + 1L, cs$epochs, info = lab)
    expect_identical(o$w$state, if (cs$kept) "analysed" else "too_short", info = lab)
    expect_identical(o$in_output, cs$kept, info = lab)
    if (!cs$kept) {
      expect_identical(o$w$reason, paste0("(qqq[2] - qqq[1]) * ", cs$ws, " = 900 s is not ",
                                          "above the 900 s minimum"), info = lab)
    }
  }
})

test_that("onset and wake are decimal hours from the window's own midnight on MOS2", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(MOS2_META())
  got <- list()
  for (tw in c("MM", "WW")) {
    tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, tw, s$ws3, s$FM, s$SO)
    wi <- 1
    lastDay <- FALSE
    while (lastDay == FALSE) {
      d <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows, c(0, 24),
                                    s$ID)
      if (length(which(is.na(d$qqq))) == 0) {
        o <- .raw.timeuse.onsetwake(d$qqq, s$ts, s$mn$min, s$mn$sec, s$mn$hour, tw)
        got[[length(got) + 1]] <- o
      }
      lastDay <- d$lastDay
      wi <- wi + 1
    }
  }
  expect_identical(length(got), 7L)
  onset <- vapply(got, function(z) if (z$skiponset) NA_real_ else z$onset, numeric(1))
  wake <- vapply(got, function(z) if (z$skipwake) NA_real_ else z$wake, numeric(1))
  stored_onset <- suppressWarnings(as.numeric(s$output$sleeponset))
  stored_wake <- suppressWarnings(as.numeric(s$output$wakeup))
  expect_identical(as.character(onset[!is.na(onset)]),
                   s$output$sleeponset[!is.na(stored_onset)])
  expect_identical(as.character(wake[!is.na(wake)]), s$output$wakeup[!is.na(stored_wake)])
  # MM window 1 has an onset and no wake at all
  expect_true(got[[1]]$skipwake)
  expect_false(got[[1]]$skiponset)
  expect_identical(got[[1]]$wake, 0) # the initial value, which the caller writes as NA
  expect_equal(got[[1]]$onset, 22.9861111111111, tolerance = 1e-12)
  expect_identical(got[[1]]$onseti, 1791)
  # the _ts column is the raw clock time of the same epoch
  expect_identical(format(s$ts$time[got[[2]]$wakei], "%H:%M:%S"), "07:10:45")
  expect_equal(got[[2]]$wake, 31.1791666666667, tolerance = 1e-12)
  # WW takes its wake from the window edge, qqq[2] + 1
  expect_identical(got[[5]]$wakei, 25393)
})

test_that("for OO the reported onset is two epochs after part 4's", {
  # qqq[1] is the onset transition plus one, and onseti is qqq[1] + 1
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(MOS2_META())
  tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "OO", s$ws3, s$FM, s$SO)
  d <- .raw.timeuse.define.days(tr$nightsi, 1, s$ws3, s$ts, "OO", tr$Nwindows, c(0, 24), s$ID)
  o <- .raw.timeuse.onsetwake(d$qqq, s$ts, s$mn$min, s$mn$sec, s$mn$hour, "OO")
  expect_identical(o$onseti, 1792)
  expect_identical(o$onseti, s$SO[1] + 2)        # two epochs past the diur transition
  expect_equal(o$onset, 22.9875, tolerance = 1e-12)
  # part 4 reports the transition epoch itself, so the clock times differ by one epoch
  expect_equal(s$nights$sleeponset[1], 22.9861111111111, tolerance = 1e-10)
  expect_equal(o$onset - s$nights$sleeponset[1], s$ws3 / 3600, tolerance = 1e-9)
})

test_that("the day sleeper fixture takes the afternoon-wake branch of onsetwake", {
  # the pair comes from the wake >= 12 & wake <= 18 arm
  skip_if_no_ggir_ref()
  metadir <- p34_fixture("daysleeper", "output_din", "meta")
  skip_if_no_dir(metadir)
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(metadir)
  tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "WW", s$ws3, s$FM, s$SO)
  d <- .raw.timeuse.define.days(tr$nightsi, 1, s$ws3, s$ts, "WW", tr$Nwindows, c(0, 24), s$ID)
  expect_identical(names(d$segments), "13:10:45-13:45:55")
  o <- .raw.timeuse.onsetwake(d$qqq, s$ts, s$mn$min, s$mn$sec, s$mn$hour, "WW")
  expect_identical(format(s$ts$time[o$wakei], "%H:%M:%S"), "13:46:00")
  expect_equal(o$wake, 13.7666666666667 + 24, tolerance = 1e-12)
  expect_equal(o$onset, 5.5125 + 24, tolerance = 1e-12)
  expect_identical(s$output$start_end_window[s$output$window == "WW"][1],
                   "13:10:45-13:45:55")
})

test_that("the two label helpers snap seconds to the epoch and carry a 60 second snap", {
  # these two produce every start_end_window string in the output
  expect_identical(.raw.timeuse.qwindow2timestamp(c(0, 8, 24), 5),
                   c("00:00:00", "08:00:00", "24:00:00"))
  expect_identical(.raw.timeuse.qwindow2timestamp(c(8, 24), 5),
                   c("00:00:00", "08:00:00", "24:00:00")) # midnight is prepended
  # 8.51 h is 08:30:36; the seconds snap to the nearest multiple of the epoch
  expect_identical(.raw.timeuse.qwindow2timestamp(c(0, 8.51, 24), 5)[2], "08:30:35")
  # at a 60 s epoch the snap lands on 60 and carries into the minute
  expect_identical(.raw.timeuse.qwindow2timestamp(c(0, 8.51, 24), 60)[2], "08:31:00")
  expect_identical(.raw.timeuse.subtract.epoch(c("08:00:00", "24:00:00"), 5),
                   c("07:59:55", "23:59:55"))
  expect_identical(.raw.timeuse.subtract.epoch("24:00:00", 60), "23:59:00")
  expect_identical(.raw.timeuse.subtract.epoch("00:00:00", 5), "23:59:55")
})

test_that("qwindow c(0, 8, 24) segments MM windows and relabels the full window", {
  # the default is GGIR 3.3.6: unprefixed segment names, and WW keeps its clock edges
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(MOS2_META())
  tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "MM", s$ws3, s$FM, s$SO)
  d1 <- .raw.timeuse.define.days(tr$nightsi, 1, s$ws3, s$ts, "MM", tr$Nwindows, c(0, 8, 24),
                                 s$ID)
  d2 <- .raw.timeuse.define.days(tr$nightsi, 2, s$ws3, s$ts, "MM", tr$Nwindows, c(0, 8, 24),
                                 s$ID)
  expect_identical(d2$segments_names, c("MM", "segment1", "segment2"))
  expect_identical(names(d2$segments),
                   c("00:00:00-23:59:55", "00:00:00-07:59:55", "08:00:00-23:59:55"))
  expect_identical(d2$segments[[2]], c(2521L, 8280L))
  expect_identical(d2$segments[[3]], c(8281L, 19800L))
  # the partial first window has no 00:00-08:00 epochs; segmentation relabels it with its edges
  expect_identical(names(d1$segments)[1], "20:30:00-23:59:55")
  expect_identical(d1$segments[[2]], c(NA, NA))
  # WW under 3.3.6 gets no segments however qwindow is set
  trw <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "WW", s$ws3, s$FM, s$SO)
  w1 <- .raw.timeuse.define.days(trw$nightsi, 1, s$ws3, s$ts, "WW", trw$Nwindows,
                                 c(0, 8, 24), s$ID)
  expect_identical(w1$segments_names, "WW")
  expect_identical(names(w1$segments), "07:10:45-07:45:55")
})

test_that("segment_all_windows = TRUE reproduces the GGIR 3.3-9 body exactly", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  clone_path <- clone_definedays()
  if (!file.exists(clone_path)) skip("GGIR 3.3-9 clone not found")
  withr::local_locale(c(LC_TIME = "C"))
  clone <- new.env(parent = globalenv())
  sys.source(clone_path, envir = clone)
  s <- tuw_window_state(MOS2_META())
  for (tw in c("MM", "WW")) {
    tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, tw, s$ws3, s$FM, s$SO)
    for (wi in 1:3) {
      g <- clone$g.part5.definedays(tr$nightsi, wi, 1, s$ws3, c(), s$ts, tw, tr$Nwindows,
                                    c(0, 8, 24), s$ID, 0)
      o <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows,
                                    c(0, 8, 24), s$ID, segment_all_windows = TRUE)
      expect_identical(o$qqq, g$qqq, info = paste(tw, wi))
      expect_identical(o$lastDay, g$lastDay, info = paste(tw, wi))
      expect_identical(o$segments, g$segments, info = paste(tw, wi))
      expect_identical(o$segments_names, g$segments_names, info = paste(tw, wi))
    }
  }
  trw <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "WW", s$ws3, s$FM, s$SO)
  w39 <- .raw.timeuse.define.days(trw$nightsi, 1, s$ws3, s$ts, "WW", trw$Nwindows,
                                  c(0, 8, 24), s$ID, segment_all_windows = TRUE)
  expect_identical(w39$segments_names, c("WW", "WWsegment1", "WWsegment2"))
  expect_identical(names(w39$segments)[1], "07:10:45-07:45:55")
  trm <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "MM", s$ws3, s$FM, s$SO)
  m39 <- .raw.timeuse.define.days(trm$nightsi, 2, s$ws3, s$ts, "MM", trm$Nwindows,
                                  c(0, 8, 24), s$ID, segment_all_windows = TRUE)
  expect_identical(m39$segments_names, c("MM", "MMsegment1", "MMsegment2"))
  # 3.3-9 ends the WW loop one window earlier than 3.3.6 does
  w3_39 <- .raw.timeuse.define.days(trw$nightsi, 3, s$ws3, s$ts, "WW", trw$Nwindows,
                                    c(0, 24), s$ID, segment_all_windows = TRUE)
  w3_36 <- .raw.timeuse.define.days(trw$nightsi, 3, s$ws3, s$ts, "WW", trw$Nwindows,
                                    c(0, 24), s$ID)
  expect_true(w3_39$lastDay)
  expect_false(w3_36$lastDay)
})

test_that("a clock segment that wraps the window start splits in 3.3-9 and collapses in 3.3.6", {
  # in 3.3.6 the 17:00-23:59 segment of a window running 23:00 to 22:59 collapses to the
  # whole window
  withr::local_locale(c(LC_TIME = "C"))
  t0 <- as.POSIXct("2025-05-01 23:00:00", tz = "America/Anchorage")
  ts <- data.frame(time = seq(t0, by = 60, length.out = 1440 * 3), diur = 0)
  nightsi <- c(1, 1441, 2881) # the 23:00 grid, that is dayborder = 23
  d36 <- .raw.timeuse.define.days(nightsi, 1, 60, ts, "MM", 4, c(0, 9, 17, 24), "x")
  d39 <- .raw.timeuse.define.days(nightsi, 1, 60, ts, "MM", 4, c(0, 9, 17, 24), "x",
                                  segment_all_windows = TRUE)
  expect_identical(d36$qqq, c(1, 1440))
  expect_identical(d36$segments_names, c("MM", "segment1", "segment2", "segment3"))
  expect_identical(d36$segments[[4]], c(1L, 1440L))  # the whole window, under a segment label
  expect_identical(d36$segments[[1]], c(NA, NA))   # and the full window itself is empty
  expect_identical(d39$segments[[4]], c(1L, 60L, 1081L, 1440L)) # the two real pieces
  expect_identical(d39$segments[[1]], c(1L, 1440L))
  expect_identical(d39$segments_names, c("MM", "MMsegment1", "MMsegment2", "MMsegment3"))
  expect_identical(d36$segments[[2]], d39$segments[[2]])
  expect_identical(d36$segments[[3]], d39$segments[[3]])
})

test_that("EE gives six MM and five WW windows with the stored labels and timings", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(EE_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(EE_META())
  expect_identical(s$mn$nightsi,
                   c(10621L, 27901L, 45181L, 62461L, 79741L, 97021L, 114301L))
  mm <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "MM", s$ws3, s$FM, s$SO)
  ww <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "WW", s$ws3, s$FM, s$SO)
  expect_identical(mm$Nwindows, 7)   # length(nightsi) + 1, a double
  expect_identical(ww$Nwindows, 6L)
  qqqs <- list()
  labs <- character(0)
  for (tw in c("MM", "WW")) {
    tr <- if (tw == "MM") mm else ww
    wi <- 1
    lastDay <- FALSE
    while (lastDay == FALSE) {
      d <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows, c(0, 24),
                                    s$ID)
      if (length(which(is.na(d$qqq))) == 0) {
        qqqs[[length(qqqs) + 1]] <- d$qqq
        labs <- c(labs, names(d$segments))
      }
      lastDay <- d$lastDay
      wi <- wi + 1
    }
  }
  expect_identical(length(qqqs), 11L)
  expect_identical(labs, s$output$start_end_window)
  # window lengths are the stored dur_day_spt_min to the epoch
  mins <- vapply(qqqs, function(q) (q[2] - q[1] + 1) * s$ws3 / 60, numeric(1))
  expect_identical(as.character(mins), s$output$dur_day_spt_min)
})

test_that("every part-5 fixture present replays identically to GGIR, window for window", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  dirs <- tuw_meta_dirs(p5_fixture_root())
  skip_if(length(dirs) == 0, "no part-5 fixtures generated yet")
  withr::local_locale(c(LC_TIME = "C"))
  for (metadir in dirs) {
    r <- tuw_recording(metadir)
    mn <- .raw.timeuse.midnights(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), 0,
                                 r$ts$time, r$tz)
    Neph <- 3600 / r$ws3
    # the diary fixtures keep GGIR's parsed diary next to the milestones
    slogfile <- dir(metadir, pattern = "^sleeplog_.*[.]RData$", full.names = TRUE)
    sleeplog <- c()
    if (length(slogfile) == 1) sleeplog <- tuw_loadenv(slogfile)$logs_diaries$sleeplog
    nights <- .raw.timeuse.fixmissingnight(r$nights, sleeplog, r$ID)
    expect_identical(nights, GGIR:::g.part5.fixmissingnight(r$nights, sleeplog, r$ID),
                     info = metadir)
    ts <- .raw.timeuse.wakesleep(r$ts, nights, r$tz, mn$nightsi2, sleeplog, r$ws3, r$ID, Neph)
    expect_identical(ts, GGIR:::g.part5.wakesleepwindows(r$ts, nights, r$tz, mn$nightsi2,
                                                         sleeplog, r$ws3, r$ID, Neph),
                     info = metadir)
    before_ts <- ts
    before <- ts$diur
    ts <- .raw.timeuse.addfirstwake(before_ts, nights, mn$nightsi, sleeplog, r$ID, Neph,
                                    r$SPTE_end)
    expect_identical(ts, GGIR:::g.part5.addfirstwake(before_ts, nights, mn$nightsi, sleeplog,
                                                     r$ID, Neph, r$SPTE_end),
                     info = metadir)
    fixture <- basename(dirname(dirname(metadir)))
    if (fixture == "excludefirst") {
      # part 4 was run with excludefirst.part4, so night 1 is missing and the repair fires
      expect_identical(nights$night, c(2, 3, 4))
      expect_identical(which(diff(before) == -1)[1], 25392L)
      expect_true(which(diff(before) == -1)[1] > mn$nightsi[2])
      expect_identical(sum(before), 12004)
      expect_identical(sum(ts$diur), 19694)
      expect_identical(ts$diur[1:7690], rep(1, 7690))
      expect_identical(ts$diur[7691], 0)
      expect_identical(unique(ts$guider[1:7690]), "part3_estimate")
    }
    if (fixture == "missingnight") {
      # part 4's night 2 is gone and the placeholder row is spliced back
      expect_identical(r$nights$night, c(1, 3, 4))
      expect_identical(nights$night, c(1, 2, 3, 4))
      expect_identical(nights$cleaningcode, c(2, 5, 1, 2))
      expect_identical(nights$guider[2], "nosleeplog_accnotworn")
      expect_identical(ts$diur, before) # the first wake needs no repair here
    }
    ts$time <- .raw.iso8601.to.posix(ts$time, tz = r$tz)
    FM <- which(diff(ts$diur) == -1)
    SO <- which(diff(ts$diur) == 1)
    # the qwindow fixture ran with qwindow = c(0, 8, 24), so segment it both ways
    qwindows <- if (fixture == "qwindow") list(c(0, 24), c(0, 8, 24)) else list(c(0, 24))
    for (qw in qwindows) {
      for (tw in c("MM", "WW", "OO")) {
        tr <- .raw.timeuse.trim.nights(mn$nightsi, ts, tw, r$ws3, FM, SO)
        wi <- 1
        lastDay <- ifelse(tr$Nwindows > 0 && length(tr$nightsi) > 0, FALSE, TRUE)
        while (lastDay == FALSE && wi <= 40) {
          g <- GGIR:::g.part5.definedays(tr$nightsi, wi, 1, r$ws3, c(), ts, tw, tr$Nwindows,
                                         qw, r$ID, 0)
          o <- .raw.timeuse.define.days(tr$nightsi, wi, r$ws3, ts, tw, tr$Nwindows, qw,
                                        r$ID)
          expect_identical(o[c("qqq", "lastDay", "segments", "segments_names")],
                           g[c("qqq", "lastDay", "segments", "segments_names")],
                           info = paste(metadir, tw, wi, length(qw)))
          if (length(which(is.na(o$qqq))) == 0 && (o$qqq[2] - o$qqq[1]) * r$ws3 > 900) {
            expect_identical(
              .raw.timeuse.onsetwake(o$qqq, ts, mn$min, mn$sec, mn$hour, tw),
              GGIR:::g.part5.onsetwaketiming(o$qqq, ts, mn$min, mn$sec, mn$hour, tw),
              info = paste(metadir, tw, wi))
          }
          lastDay <- o$lastDay
          wi <- wi + 1
        }
      }
    }
  }
})

test_that("an activity log as qwindow names the segments from the diary labels", {
  # no reference recording has an activity log, so this runs against GGIR on a synthetic one
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(MOS2_META())
  alog <- data.frame(ID = s$ID, date = c("2025-10-08", "2025-10-09"), stringsAsFactors = FALSE)
  alog$qwindow_values <- list(c(0, 7.5, 13, 24), c(0, 9, 24))
  alog$qwindow_names <- list(c("midnight", "wakeup", "lunch", "midnight"),
                             c("midnight", "work", "midnight"))
  tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, "MM", s$ws3, s$FM, s$SO)
  for (wi in 1:4) {
    g <- GGIR:::g.part5.definedays(tr$nightsi, wi, 1, s$ws3, c(), s$ts, "MM", tr$Nwindows,
                                   alog, s$ID, 0)
    o <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, "MM", tr$Nwindows, alog, s$ID)
    expect_identical(o$segments, g$segments, info = paste("wi", wi))
    expect_identical(o$segments_names, g$segments_names, info = paste("wi", wi))
  }
  # window 2 matches the log's first row and is cut at 07:30 and 13:00
  d2 <- .raw.timeuse.define.days(tr$nightsi, 2, s$ws3, s$ts, "MM", tr$Nwindows, alog, s$ID)
  expect_identical(d2$segments_names,
                   c("MM", "midnight-wakeup", "wakeup-lunch", "lunch-midnight"))
  expect_identical(names(d2$segments),
                   c("00:00:00-23:59:55", "00:00:00-07:29:55", "07:30:00-12:59:55",
                     "13:00:00-23:59:55"))
  expect_identical(d2$segments[[2]], c(2521L, 7920L))
  expect_identical(d2$segments[[3]], c(7921L, 11880L))
  expect_identical(d2$segments[[4]], c(11881L, 19800L))
  # window 1's date is not in the log, so qwindow falls back to c(0, 24): one segment
  d1 <- .raw.timeuse.define.days(tr$nightsi, 1, s$ws3, s$ts, "MM", tr$Nwindows, alog, s$ID)
  expect_identical(d1$segments_names, "MM")
  expect_identical(names(d1$segments), "00:00:00-23:59:55")
  # 3.3-9 prefixes the diary names with the timewindow
  clone_path <- clone_definedays()
  if (file.exists(clone_path)) {
    clone <- new.env(parent = globalenv())
    sys.source(clone_path, envir = clone)
    o39 <- .raw.timeuse.define.days(tr$nightsi, 2, s$ws3, s$ts, "MM", tr$Nwindows, alog,
                                    s$ID, segment_all_windows = TRUE)
    g39 <- clone$g.part5.definedays(tr$nightsi, 2, 1, s$ws3, c(), s$ts, "MM", tr$Nwindows,
                                    alog, s$ID, 0)
    expect_identical(o39$segments_names, g39$segments_names)
    expect_identical(o39$segments_names[2], "MMsegment-midnight-wakeup")
  }
})

test_that("the qwindow fixture's stored window and start_end_window columns are reproduced", {
  skip_if_no_ggir_ref()
  metadir <- file.path(p5_fixture_root(), "qwindow", "output_din", "meta")
  skip_if_no_dir(metadir)
  withr::local_locale(c(LC_TIME = "C"))
  s <- tuw_window_state(metadir)
  win <- lab <- character(0)
  for (tw in c("MM", "WW")) { # the fixture ran timewindow = c("MM", "WW")
    tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, tw, s$ws3, s$FM, s$SO)
    wi <- 1
    lastDay <- FALSE
    while (lastDay == FALSE && wi <= 40) {
      d <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows,
                                    c(0, 8, 24), s$ID)
      if (length(which(is.na(d$qqq))) == 0 && (d$qqq[2] - d$qqq[1]) * s$ws3 > 900) {
        win <- c(win, d$segments_names)
        lab <- c(lab, names(d$segments))
      }
      lastDay <- d$lastDay
      wi <- wi + 1
    }
  }
  expect_identical(nrow(s$output), 15L)
  expect_identical(win, s$output$window)
  expect_identical(lab, s$output$start_end_window)
  expect_identical(unique(win), c("MM", "segment1", "segment2", "WW"))
  expect_identical(lab[1], "20:30:00-23:59:55")
})

test_that("windows of one timewindow type tile the analysed recording without gap or overlap", {
  skip_if_no_ggir_ref()
  skip_if_no_dir(MOS2_META())
  withr::local_locale(c(LC_TIME = "C"))
  for (metadir in c(MOS2_META(), EE_META())) {
    if (!dir.exists(metadir)) next
    s <- tuw_window_state(metadir)
    for (tw in c("MM", "WW", "OO")) {
      tr <- .raw.timeuse.trim.nights(s$mn$nightsi, s$ts, tw, s$ws3, s$FM, s$SO)
      wi <- 1
      lastDay <- FALSE
      qs <- list()
      while (lastDay == FALSE && wi <= 40) {
        d <- .raw.timeuse.define.days(tr$nightsi, wi, s$ws3, s$ts, tw, tr$Nwindows,
                                      c(0, 24), s$ID)
        if (length(which(is.na(d$qqq))) == 0) qs[[length(qs) + 1]] <- d$qqq
        lastDay <- d$lastDay
        wi <- wi + 1
      }
      m <- do.call(rbind, qs)
      expect_true(nrow(m) >= 3, info = paste(metadir, tw))
      # each window starts exactly one epoch after the previous one ends
      expect_identical(m[-1, 1] - m[-nrow(m), 2], rep(1, nrow(m) - 1),
                       info = paste(metadir, tw))
      expect_true(all(m[, 2] > m[, 1]), info = paste(metadir, tw))
    }
  }
})
