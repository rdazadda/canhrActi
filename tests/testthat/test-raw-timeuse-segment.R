# Parity tests for R/raw_timeuse_segment.R against GGIR's g.part5_analyseSegment and
# g.part5.lux_persegment. Reference data are found through CANHRACTI_GGIR_REF, the part-5
# fixtures through CANHRACTI_GGIR_P5FIX and the 3.3-9 clone through CANHRACTI_GGIR_SRC,
# both falling back to siblings of the reference folder; every block skips without them,
# and the live comparisons skip when GGIR is not installed.

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

tus_fixdir <- function(name) {
  base <- Sys.getenv("CANHRACTI_GGIR_P5FIX", unset = "")
  if (base == "") {
    if (.ggir_ref == "") return("")
    base <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref, winslash = "/",
                                                           mustWork = FALSE))),
                      "ggir-study-p5", "fixtures")
  }
  file.path(base, name)
}
# The 3.3-9 clone's R folder: CANHRACTI_GGIR_SRC, else ggir-src beside the reference folder
tus_clone <- function() {
  src <- Sys.getenv("CANHRACTI_GGIR_SRC", unset = "")
  if (src == "" && .ggir_ref != "") {
    src <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref, winslash = "/",
                                                          mustWork = FALSE))),
                     "ggir-src", "GGIR", "R")
  }
  src
}

tus_loadenv <- function(path) {
  e <- new.env()
  load(path, envir = e)
  e
}

# The two reference recordings.
TUS_CASES <- list(
  MOS2 = list(out = c("out", "output_din"), nrows = 7L, nwin = c(MM = 4L, WW = 3L)),
  EE   = list(out = c("timing_out", "output_timing"), nrows = 11L, nwin = c(MM = 6L, WW = 5L)))

# The state g.part5 hands to g.part5_analyseSegment, from stored milestones. diur and
# sibdetection come from the stored mdat, so the sleep modules are not under test here.
tus_state <- function(metadir, TRLi = 40, TRMi = 100, TRVi = 400) {
  fn <- dir(file.path(metadir, "ms2.out"))[1]
  imp <- tus_loadenv(file.path(metadir, "ms2.out", fn))
  bas <- tus_loadenv(file.path(metadir, "basic", paste0("meta_", fn)))
  ms4 <- tus_loadenv(file.path(metadir, "ms4.out", fn))
  ms5 <- tus_loadenv(file.path(metadir, "ms5.out", fn))
  rawdir <- file.path(metadir, "ms5.outraw", "40_100_400")
  mdat <- tus_loadenv(file.path(rawdir, dir(rawdir)[1]))$mdat
  tz <- bas$desiredtz_part1
  if (is.null(tz)) tz <- ""
  ws3new <- imp$IMP$windowsizes[1]
  ts <- .raw.timeuse.init.ts(imp$IMP, bas$M, acc_metric = "ENMO", sensor_location = "wrist")
  if (nrow(mdat) != nrow(ts)) {
    testthat::skip("the stored series is filtered; diur and sibdetection cannot be recovered")
  }
  ts$diur <- mdat$SleepPeriodTime
  ts$sibdetection <- mdat$sibdetection
  ts$selfreported <- mdat$selfreported
  ts$window <- 0
  mid <- .raw.timeuse.midnights(.raw.iso8601.to.posix(ts$time, tz = tz), dayborder = 0,
                                time_char = ts$time, desiredtz = tz)
  ts$time <- mid$time_POSIX
  pp <- list(boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8,
             boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
             boutdur.lig = c(10, 5, 1))
  LL <- .raw.identify.levels(ts = ts, TRLi = TRLi, TRMi = TRMi, TRVi = TRVi, ws3 = ws3new,
                             params_phyact = pp)
  sibDef <- unique(ms4$nightsummary$sleepparam)[1]
  list(ts = ts, mid = mid, ws3new = ws3new, pp = pp, mdat = mdat, out = ms5$output,
       sumSleep = ms4$nightsummary[ms4$nightsummary$sleepparam == sibDef, ],
       tz = tz, sibDef = sibDef, ID = sub("[.]RData$", "", fn), fname = fn,
       levelList = list(threshold = c(TRLi, TRMi, TRVi), LEVELS = LL$LEVELS,
                        Lnames = LL$Lnames, OLEVELS = LL$OLEVELS, bc.mvpa = LL$bc.mvpa,
                        bc.in = LL$bc.in, bc.lig = LL$bc.lig))
}

# GGIR's loop over windows and segments, with the per-segment body left to fun.
tus_walk <- function(S, timewindowi, fun, qwindow = c(0, 24), segment_all_windows = FALSE) {
  ts <- S$ts
  ts$window <- 0
  tn <- .raw.timeuse.trim.nights(S$mid$nightsi, ts, timewindowi, S$ws3new)
  nightsi <- tn$nightsi
  Nwindows <- tn$Nwindows
  add_one_day_to_next_date <- FALSE
  lastDay <- ifelse(Nwindows > 0 && length(nightsi) > 0, FALSE, TRUE)
  wi <- 1; di <- 1; si_used <- 0
  state <- list(ts = ts, levelList = S$levelList, rows = list())
  while (lastDay == FALSE) {
    dd <- .raw.timeuse.define.days(nightsi, wi, S$ws3new, state$ts, timewindowi, Nwindows,
                                   qwindow = qwindow, ID = S$ID,
                                   segment_all_windows = segment_all_windows)
    qqq <- dd$qqq; lastDay <- dd$lastDay
    if (length(which(is.na(qqq) == TRUE)) == 0) {
      if ((qqq[2] - qqq[1]) * S$ws3new > 900) {
        state$ts$window[qqq[1]:qqq[2]] <- wi
        next_si <- if (di == 1) 1 else si_used + 1
        for (si in next_si:(next_si + length(dd$segments) - 1)) {
          csi <- si - next_si + 1
          Nind <- length(dd$segments[[csi]])
          segStart <- dd$segments[[csi]][seq(1, Nind, by = 2)]
          segEnd <- dd$segments[[csi]][seq(2, Nind, by = 2)]
          state <- fun(state, S, timewindowi, wi, qqq, si, csi, segStart, segEnd,
                       dd$segments, dd$segments_names, add_one_day_to_next_date)
          add_one_day_to_next_date <- state$aod
        }
        si_used <- si
        di <- di + 1
      }
    }
    di <- di + 1; wi <- wi + 1
  }
  state
}

tus_opts_default <- list(frag = NULL, iglevels = NULL, sfs = FALSE, ge = TRUE,
                         LUXthresholds = c(0, 100, 500, 1000, 3000, 5000, 10000),
                         LUX_day_segments = NULL)

tus_mine <- function(state, S, timewindowi, wi, qqq, si, csi, segStart, segEnd,
                     segments, segments_names, aod) {
  o <- state$opts
  gas <- .raw.timeuse.segment(
    indexlog = list(fileIndex = 1, winType = timewindowi, winIndex = wi, winStartEnd = qqq,
                    segIndex1 = si, segIndex2 = csi,
                    segStartEnd = c(segStart, segEnd), columnIndex = 3),
    timeList = list(ts = state$ts, sec = S$mid$sec, min = S$mid$min, hour = S$mid$hour,
                    time_POSIX = S$mid$time_POSIX, epochSize = S$ws3new),
    levelList = state$levelList, segments = segments, segments_names = segments_names,
    dsummary = state$rows, sumSleep = S$sumSleep, sibDef = S$sibDef,
    add_one_day_to_next_date = aod, desiredtz = S$tz,
    boutdur.mvpa = S$pp$boutdur.mvpa, boutdur.in = S$pp$boutdur.in,
    boutdur.lig = S$pp$boutdur.lig, boutcriter.mvpa = S$pp$boutcriter.mvpa,
    boutcriter.in = S$pp$boutcriter.in, boutcriter.lig = S$pp$boutcriter.lig,
    frag.metrics = o$frag, iglevels = o$iglevels,
    LUXthresholds = o$LUXthresholds, LUX_day_segments = o$LUX_day_segments,
    do.sibreport = TRUE, storefolderstructure = o$sfs,
    fullFilename = "FULL/PATH", foldernamei = "FOLDER",
    tail_expansion_log = NULL, sibreport = NULL, warn_xmin = FALSE, ggir_exact = o$ge)
  state$rows <- gas$dsummary
  state$ts <- gas$timeList$ts
  state$levelList$Lnames <- gas$timeList$Lnames
  state$levelList$LEVELS <- gas$timeList$LEVELS
  state$aod <- gas$add_one_day_to_next_date
  state$fi <- gas$indexlog$columnIndex
  state$doNext <- gas$doNext
  state$indexlog_out <- gas$indexlog
  state
}

tus_ggir <- function(state, S, timewindowi, wi, qqq, si, csi, segStart, segEnd,
                     segments, segments_names, aod) {
  o <- state$opts
  if (is.null(state$acc)) {
    state$acc <- list(dsummary = matrix("", 40, 500), ds_names = rep("", 500))
  }
  ds <- state$acc$dsummary; dn <- state$acc$ds_names
  ds[si, 1:2] <- c(S$ID, S$fname); dn[1:2] <- c("ID", "filename")
  gas <- GGIR:::g.part5_analyseSegment(
    indexlog = list(fileIndex = 1, winType = timewindowi, winIndex = wi, winStartEnd = qqq,
                    segIndex1 = si, segIndex2 = csi,
                    segStartEnd = c(segStart, segEnd), columnIndex = 3),
    timeList = list(ts = state$ts, sec = S$mid$sec, min = S$mid$min, hour = S$mid$hour,
                    time_POSIX = S$mid$time_POSIX, epochSize = S$ws3new),
    levelList = state$levelList, segments = segments, segments_names = segments_names,
    dsummary = ds, ds_names = dn,
    params_general = list(desiredtz = S$tz),
    params_output = list(do.sibreport = TRUE, storefolderstructure = o$sfs),
    params_sleep = list(possible_nap_window = NULL, possible_nap_dur = NULL,
                        nap_model = NULL),
    params_247 = list(iglevels = o$iglevels, LUXthresholds = o$LUXthresholds,
                      LUX_day_segments = o$LUX_day_segments),
    params_phyact = c(S$pp, list(frag.metrics = o$frag)),
    sumSleep = S$sumSleep, sibDef = S$sibDef, fullFilename = "FULL/PATH",
    add_one_day_to_next_date = aod, lightpeak_available = FALSE,
    tail_expansion_log = NULL, foldernamei = "FOLDER", sibreport = NULL)
  state$acc$dsummary <- gas$dsummary
  state$acc$ds_names <- gas$ds_names
  state$ts <- gas$timeList$ts
  state$levelList$Lnames <- gas$timeList$Lnames
  state$levelList$LEVELS <- gas$timeList$LEVELS
  state$aod <- gas$add_one_day_to_next_date
  state$fi <- gas$indexlog$columnIndex
  state$doNext <- gas$doNext
  state$indexlog_out <- gas$indexlog
  state
}

tus_run <- function(S, timewindowi, fun, opts = list(), qwindow = c(0, 24)) {
  o <- utils::modifyList(tus_opts_default, opts)
  tus_walk(S, timewindowi, function(st, ...) { st$opts <- o; fun(st, ...) }, qwindow = qwindow)
}

# the character table the export layer would write, from either side
tus_tab_mine <- function(st, S) {
  chr <- lapply(st$rows, function(rw) c(ID = S$ID, filename = S$fname,
                                        .raw.timeuse.row.chr(rw)))
  nm <- names(chr[[which.max(vapply(chr, length, integer(1)))]])
  d <- as.data.frame(do.call(rbind, lapply(chr, function(x) {
    v <- x[nm]; v[is.na(v)] <- ""; v
  })), stringsAsFactors = FALSE)
  names(d) <- nm
  rownames(d) <- NULL
  d
}
tus_tab_ggir <- function(st) {
  ds <- st$acc$dsummary; dn <- st$acc$ds_names
  keep <- which(ds[, 1] != "")
  nc <- max(which(dn != ""))
  d <- as.data.frame(ds[keep, 1:nc, drop = FALSE], stringsAsFactors = FALSE)
  names(d) <- dn[1:nc]
  d[is.na(d)] <- ""
  rownames(d) <- NULL
  d
}

tus_both <- function(S, opts = list(), tws = c("MM", "WW"), qwindow = c(0, 24)) {
  tabs <- lapply(tws, function(tw) tus_tab_mine(tus_run(S, tw, tus_mine, opts, qwindow), S))
  out <- do.call(rbind, tabs)
  rownames(out) <- NULL
  out
}

tus_cache <- new.env(parent = emptyenv())
tus_case <- function(case) {
  if (!is.null(tus_cache[[case]])) return(tus_cache[[case]])
  skip_if_no_ggir_ref()
  metadir <- do.call(ref_file, c(as.list(TUS_CASES[[case]]$out), list("meta")))
  if (!dir.exists(metadir)) testthat::skip(paste0("reference folder not found: ", metadir))
  S <- tus_state(metadir)
  tus_cache[[case]] <- S
  S
}

test_that("P5a the stored MOS2 output object is reproduced, 7 x 119, identical()", {
  S <- tus_case("MOS2")
  mine <- tus_both(S)
  mine$GGIRversion <- "3.3.6" # the caller's column, set here for parity
  stored <- as.data.frame(S$out, stringsAsFactors = FALSE)
  expect_identical(dim(mine), c(7L, 119L))
  expect_identical(names(mine), colnames(S$out))
  for (nm in names(mine)) expect_identical(mine[[nm]], stored[[nm]], info = nm)
  expect_identical(mine, stored)
  expect_true(all(vapply(mine, is.character, logical(1))))
})

test_that("P5b the stored EE output object is reproduced, 11 x 119, identical()", {
  S <- tus_case("EE")
  mine <- tus_both(S)
  mine$GGIRversion <- "3.3.6"
  stored <- as.data.frame(S$out, stringsAsFactors = FALSE)
  expect_identical(dim(mine), c(11L, 119L))
  expect_identical(mine, stored)
  expect_identical(mine$dur_day_spt_min[mine$window == "MM"],
                   c("885", "1440", "1440", "1440", "1440", "1440"))
  expect_identical(mine$dur_day_spt_min[mine$window == "WW"],
                   c("1612.41666666667", "1407.5", "1326.08333333333",
                     "1569.41666666667", "1291"))
})

test_that("P5c the MOS2 reference minutes are reproduced exactly", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  num <- function(row, col) as.numeric(m[[col]][row])
  rnd <- function(x) round(x, 3)
  # MM 2025-10-08, 10-09, 10-10 then WW 2025-10-08, 10-09, 10-10
  expect_identical(rnd(num(2, "dur_day_min")), 980)
  expect_identical(rnd(num(2, "dur_spt_min")), 460)
  expect_identical(rnd(num(2, "dur_day_total_IN_min")), 797.667)
  expect_identical(rnd(num(2, "dur_day_total_LIG_min")), 131.083)
  expect_identical(rnd(num(2, "dur_day_total_MOD_min")), 49)
  expect_identical(rnd(num(2, "dur_day_total_VIG_min")), 2.25)
  expect_identical(rnd(num(2, "dur_day_MVPA_bts_1_5_min")), 4.417)
  expect_identical(rnd(num(2, "ACC_day_mg")), 23.772)
  expect_identical(rnd(num(2, "quantile_mostactive60min_mg")), 92.004)
  expect_identical(rnd(num(3, "dur_day_min")), 960.083)
  expect_identical(rnd(num(3, "dur_spt_min")), 479.917)
  expect_identical(rnd(num(3, "dur_day_total_IN_min")), 808.083)
  expect_identical(rnd(num(3, "dur_day_total_LIG_min")), 115.25)
  expect_identical(rnd(num(3, "dur_day_total_MOD_min")), 35.583)
  expect_identical(rnd(num(3, "dur_day_total_VIG_min")), 1.167)
  expect_identical(rnd(num(3, "dur_day_MVPA_bts_1_5_min")), 6.167)
  expect_identical(rnd(num(3, "ACC_day_mg")), 21.145)
  expect_identical(rnd(num(3, "quantile_mostactive60min_mg")), 78.9)
  expect_identical(rnd(num(4, "dur_day_min")), 948.833)
  expect_identical(rnd(num(4, "dur_spt_min")), 491.167)
  expect_identical(rnd(num(4, "dur_day_total_IN_min")), 700.167)
  expect_identical(rnd(num(4, "dur_day_total_LIG_min")), 158.167)
  expect_identical(rnd(num(4, "dur_day_total_MOD_min")), 82)
  expect_identical(rnd(num(4, "dur_day_total_VIG_min")), 8.5)
  expect_identical(rnd(num(4, "dur_day_MVPA_bts_1_5_min")), 31.25)
  expect_identical(rnd(num(4, "ACC_day_mg")), 39.567)
  expect_identical(rnd(num(4, "quantile_mostactive60min_mg")), 126.5)
  expect_identical(rnd(num(5, "dur_day_min")), 980)
  expect_identical(rnd(num(5, "dur_spt_min")), 495.25)
  expect_identical(rnd(num(5, "quantile_mostactive60min_mg")), 92.004)
  expect_identical(rnd(num(6, "dur_day_min")), 960.083)
  expect_identical(rnd(num(6, "dur_spt_min")), 432.25)
  expect_identical(rnd(num(6, "quantile_mostactive60min_mg")), 78.6)
  expect_identical(rnd(num(7, "dur_day_min")), 903.75)
  expect_identical(rnd(num(7, "dur_spt_min")), 72.833)
  expect_identical(rnd(num(7, "dur_day_total_IN_min")), 660.5)
  expect_identical(rnd(num(7, "dur_day_total_LIG_min")), 153.917)
  expect_identical(rnd(num(7, "dur_day_total_MOD_min")), 81.5)
  expect_identical(rnd(num(7, "dur_day_total_VIG_min")), 7.833)
  expect_identical(rnd(num(7, "dur_day_MVPA_bts_1_5_min")), 31.25)
  expect_identical(rnd(num(7, "ACC_day_mg")), 40.363)
  expect_identical(rnd(num(7, "quantile_mostactive60min_mg")), 124.925)
  expect_true(all(m$dur_day_MVPA_bts_10_min == "0"))
  expect_true(all(m$dur_day_MVPA_bts_5_10_min == "0"))
})

test_that("P5d the 24 hour accounting closes on every row, to machine precision", {
  for (case in names(TUS_CASES)) {
    S <- tus_case(case)
    m <- tus_both(S)
    Ln <- S$levelList$Lnames
    sptcols <- paste0("dur_", Ln[1:5], "_min")
    daycols <- paste0("dur_", Ln[6:length(Ln)], "_min")
    ocols <- c("dur_day_total_IN_min", "dur_day_total_LIG_min",
               "dur_day_total_MOD_min", "dur_day_total_VIG_min")
    g <- function(cols) rowSums(vapply(m[cols], as.numeric, numeric(nrow(m))))
    expect_equal(g(sptcols), as.numeric(m$dur_spt_min), tolerance = 1e-12, info = case)
    expect_equal(g(daycols), as.numeric(m$dur_day_min), tolerance = 1e-12, info = case)
    expect_equal(g(ocols), as.numeric(m$dur_day_min), tolerance = 1e-12, info = case)
    expect_equal(as.numeric(m$dur_spt_min) + as.numeric(m$dur_day_min),
                 as.numeric(m$dur_day_spt_min), tolerance = 1e-12, info = case)
  }
})

test_that("P5e total and unbouted do NOT nest, and the three MOS2 gaps are exact", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  r <- 2 # MM 2025-10-08
  n <- function(col) as.numeric(m[[col]][r])
  inSum <- n("dur_day_IN_unbt_min") + n("dur_day_IN_bts_30_min") +
    n("dur_day_IN_bts_20_30_min") + n("dur_day_IN_bts_10_20_min")
  ligSum <- n("dur_day_LIG_unbt_min") + n("dur_day_LIG_bts_10_min") +
    n("dur_day_LIG_bts_5_10_min") + n("dur_day_LIG_bts_1_5_min")
  mvSum <- n("dur_day_MOD_unbt_min") + n("dur_day_VIG_unbt_min") +
    n("dur_day_MVPA_bts_10_min") + n("dur_day_MVPA_bts_5_10_min") +
    n("dur_day_MVPA_bts_1_5_min")
  expect_identical(round(inSum, 3), 826.5)
  expect_identical(round(ligSum, 3), 109.917)
  expect_identical(round(mvSum, 3), 43.583)
  expect_identical(round(inSum - n("dur_day_total_IN_min"), 2), 28.83)
  expect_identical(round(ligSum - n("dur_day_total_LIG_min"), 2), -21.17)
  expect_identical(round(mvSum - n("dur_day_total_MOD_min") - n("dur_day_total_VIG_min"), 2),
                   -7.67)
})

test_that("P5f NaN is the string NaN and a real NA is the empty string", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  mm <- as.matrix(m)
  expect_identical(sum(mm == "NaN"), 42L)
  expect_identical(sum(mm == ""), 2L)
  expect_identical(sum(is.na(mm)), 0L)
  expect_true(all(grepl("^ACC_", colnames(m)[which(mm == "NaN", arr.ind = TRUE)[, 2]])))
  expect_identical(colnames(m)[which(mm == "", arr.ind = TRUE)[, 2]],
                   c("wakeup", "wakeup_ts"))
  expect_identical(as.integer(which(mm == "", arr.ind = TRUE)[, 1]), c(1L, 1L))
})

test_that("P5r the two quantile columns are populated on EVERY row, not just the first", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  expect_true(all(m$quantile_mostactive60min_mg != ""))
  expect_true(all(m$quantile_mostactive30min_mg != ""))
  expect_identical(m$quantile_mostactive60min_mg,
                   c("9.6", "92.0041666666668", "78.9", "126.5", "92.00406710727",
                     "78.6", "124.924575475723"))
})

test_that("P5g the MM wakeup is the PREVIOUS night's and the first MM row has none", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  ns <- S$sumSleep
  expect_identical(m$wakeup[1], "")
  expect_identical(m$wakeup_ts[1], "")
  expect_identical(m$night_number[1], "1")
  expect_identical(m$calendar_date[2], "2025-10-08")
  expect_identical(m$night_number[2], "2")
  # compare as strings; the round trip through as.numeric is lossy in the 15th digit
  expect_identical(m$wakeup[2], as.character(ns$wakeup[ns$night == 1]))
  expect_identical(m$wakeup[3], as.character(ns$wakeup[ns$night == 2]))
  expect_identical(m$wakeup[4], as.character(ns$wakeup[ns$night == 3]))
  for (i in 5:7) {
    nb <- as.numeric(m$night_number[i])
    expect_identical(m$wakeup[i], as.character(ns$wakeup[ns$night == nb]),
                     info = paste("WW row", i))
  }
})

test_that("P5h dur_spt_sleep_min is NOT SleepDurationInSpt * 60, and the exact offset holds", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  ns <- S$sumSleep
  for (i in 5:7) {
    nb <- as.numeric(m$night_number[i])
    k <- which(ns$night == nb)
    lhs <- as.numeric(m$dur_spt_sleep_min[i])
    expect_false(isTRUE(all.equal(lhs, ns$SleepDurationInSpt[k] * 60, tolerance = 1e-9)) &&
                   ns$number_sib_sleepperiod[k] > 1, info = nb)
    rhs <- ns$SleepDurationInSpt[k] * 60 + (ns$number_sib_sleepperiod[k] - 1) * S$ws3new / 60
    expect_equal(lhs, rhs, tolerance = 1e-9, info = nb)
  }
  expect_equal(as.numeric(m$dur_spt_sleep_min[5]), 474.75, tolerance = 1e-9)
  expect_equal(as.numeric(m$dur_spt_sleep_min[6]), 404.41667, tolerance = 1e-6)
  expect_equal(as.numeric(m$dur_spt_sleep_min[7]), 72.83333, tolerance = 1e-6)
})

test_that("P5i dur_spt_min equals SptDuration * 60 on the WW rows and on none of the MM", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  ns <- S$sumSleep
  for (i in 5:7) {
    nb <- as.numeric(m$night_number[i])
    expect_equal(as.numeric(m$dur_spt_min[i]), ns$SptDuration[ns$night == nb] * 60,
                 tolerance = 1e-9, info = nb)
  }
  for (i in 1:4) {
    nb <- as.numeric(m$night_number[i])
    expect_false(isTRUE(all.equal(as.numeric(m$dur_spt_min[i]),
                                  ns$SptDuration[ns$night == nb] * 60, tolerance = 1e-6)),
                 info = nb)
  }
})

test_that("P5j N_atleast5minwakenight is clamped to 0 by the unconditional -2", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  expect_identical(m$N_atleast5minwakenight, rep("0", 7))
  # the three WW windows are nights 2, 3 and 4
  expect_identical(S$sumSleep$number_of_awakenings[S$sumSleep$night %in% 2:4], c(12, 14, 0))
  # ggir_exact = FALSE drops the -2
  mf <- tus_both(S, opts = list(ge = FALSE))
  expect_identical(mf$N_atleast5minwakenight, c("2", "2", "1", "2", "0", "1", "0"))
})

test_that("P5k sleep_efficiency_after_onset is not part 4's sleep efficiency", {
  S <- tus_case("MOS2")
  m <- tus_both(S)
  expect_identical(m$sleep_efficiency_after_onset[5], "0.958606764260475")
  expect_identical(m$night_number[5], "2")
  ns <- S$sumSleep
  p4 <- ns$SleepDurationInSpt[ns$night == 2] / ns$SptDuration[ns$night == 2]
  expect_false(isTRUE(all.equal(as.numeric(m$sleep_efficiency_after_onset[5]), p4,
                                tolerance = 1e-5)))
  expect_equal(round(p4, 6), 0.956588, tolerance = 1e-9)
})

tus_compare_live <- function(case, opts = list(), tws = c("MM", "WW"), qwindow = c(0, 24)) {
  S <- tus_case(case)
  for (tw in tws) {
    g <- tus_run(S, tw, tus_ggir, opts, qwindow)
    m <- tus_run(S, tw, tus_mine, opts, qwindow)
    gt <- tus_tab_ggir(g)
    mt <- tus_tab_mine(m, S)
    expect_identical(names(mt), names(gt), info = paste(case, tw, "column names"))
    for (nm in intersect(names(gt), names(mt))) {
      expect_identical(mt[[nm]], gt[[nm]], info = paste(case, tw, nm))
    }
    expect_identical(mt, gt, info = paste(case, tw, "whole table"))
    expect_identical(m$ts, g$ts, info = paste(case, tw, "ts"))
    expect_identical(m$levelList$LEVELS, g$levelList$LEVELS, info = paste(case, tw, "LEVELS"))
    expect_identical(m$levelList$Lnames, g$levelList$Lnames, info = paste(case, tw, "Lnames"))
    expect_identical(m$fi, g$fi, info = paste(case, tw, "columnIndex"))
    keys <- c("fileIndex", "winStartEnd", "segIndex1", "segIndex2", "segStartEnd")
    expect_identical(m$indexlog_out[keys], g$indexlog_out[keys],
                     info = paste(case, tw, "indexlog"))
    expect_identical(m$aod, g$aod, info = paste(case, tw, "add_one_day_to_next_date"))
  }
}

test_that("every MOS2 and EE window, MM and WW, is identical() to GGIR's analyseSegment", {
  skip_if_no_ggir()
  tus_compare_live("MOS2")
  tus_compare_live("EE")
})

test_that("the optional blocks are identical() to GGIR's too", {
  skip_if_no_ggir()
  tus_compare_live("MOS2", opts = list(frag = "all"))
  # iglevels as check_params expands iglevels = 1
  tus_compare_live("MOS2", opts = list(iglevels = c(seq(0, 4000, by = 25), 8000)))
  tus_compare_live("MOS2", opts = list(sfs = TRUE))
})

test_that("qwindow segment rows are identical() to GGIR's, including the empty one", {
  skip_if_no_ggir()
  # 4 windows x 3 segments = 12 MM rows, one of which has no epochs and returns doNext
  tus_compare_live("MOS2", tws = "MM", qwindow = c(0, 8, 24))
  S <- tus_case("MOS2")
  m <- tus_run(S, "MM", tus_mine, list(), qwindow = c(0, 8, 24))
  expect_identical(length(m$rows), 12L)
  widths <- vapply(m$rows, length, integer(1))
  expect_identical(sum(widths == 19L), 1L) # the doNext row stops after TRVi
  expect_identical(sum(widths == 116L), 11L)
})

test_that("P5l the intensity gradient reproduces the quoted MOS2 WW triples", {
  S <- tus_case("MOS2")
  m <- tus_both(S, opts = list(iglevels = c(seq(0, 4000, by = 25), 8000)))
  ww <- m[m$window == "WW", ]
  expect_identical(ww$ig_day_gradient,
                   c("-2.06856538422637", "-2.20212177453805", "-1.96465568611227"))
  expect_identical(ww$ig_day_intercept,
                   c("11.9295377243597", "12.3838234505366", "11.759443017453"))
  expect_identical(ww$ig_day_rsquared,
                   c("0.91322148068728", "0.928570189196511", "0.914163760725516"))
  expect_identical(ncol(m), 124L)
})

test_that("P5m the fragmentation block adds exactly 30 columns in the documented order", {
  S <- tus_case("MOS2")
  base <- tus_both(S)
  frag <- tus_both(S, opts = list(frag = "all"))
  expect_identical(ncol(base), 118L)
  expect_identical(ncol(frag), 148L)
  shared <- setdiff(names(base), "GGIRversion")
  for (nm in shared) expect_identical(frag[[nm]], base[[nm]], info = nm)
  expect_identical(setdiff(names(frag), names(base)),
                   paste0("FRAG_", c("Nfrag_PA2IN", "Nfrag_IN2PA", "TP_PA2IN", "TP_IN2PA",
                                     "Nfrag_IN2LIPA", "TP_IN2LIPA", "Nfrag_IN2MVPA",
                                     "TP_IN2MVPA", "mean_dur_LIPA", "Nfrag_LIPA",
                                     "mean_dur_MVPA", "Nfrag_MVPA", "Nfrag_PA", "Nfrag_IN",
                                     "mean_dur_IN", "mean_dur_PA", "Gini_dur_IN",
                                     "Gini_dur_PA", "CoV_dur_IN", "CoV_dur_PA",
                                     "alpha_dur_IN", "alpha_dur_PA", "x0.5_dur_IN",
                                     "x0.5_dur_PA", "W0.5_dur_IN", "W0.5_dur_PA",
                                     "SD_dur_IN", "SD_dur_PA", "NFragPM_PA", "NFragPM_IN"),
                          "_day"))
})

test_that("F-P5-5 all four threshold triples reproduce the stored 28 x 119 output", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tus_fixdir("thresholds"), "output_din", "meta")
  if (!dir.exists(metadir)) testthat::skip(paste0("fixture not found: ", metadir))
  stored <- as.data.frame(tus_loadenv(file.path(metadir, "ms5.out",
                                                dir(file.path(metadir, "ms5.out"))[1]))$output,
                          stringsAsFactors = FALSE)
  expect_identical(dim(stored), c(28L, 119L))
  triples <- unique(stored[, c("TRLi", "TRMi", "TRVi")])
  expect_identical(nrow(triples), 4L)
  mine <- NULL
  for (k in seq_len(nrow(triples))) {
    tr <- as.numeric(triples[k, ])
    S <- tus_state(metadir, TRLi = tr[1], TRMi = tr[2], TRVi = tr[3])
    one <- tus_both(S)
    one$GGIRversion <- "3.3.6"
    mine <- rbind(mine, one)
  }
  rownames(mine) <- NULL
  expect_identical(names(mine), colnames(stored))
  for (nm in names(mine)) expect_identical(mine[[nm]], stored[[nm]], info = nm)
  expect_identical(mine, stored)
  # MM window 2, one row per triple
  w2 <- mine[mine$window == "MM" & mine$window_number == "2", ]
  expect_identical(w2$dur_day_total_IN_min,
                   c("762.833333333333", "762.833333333333",
                     "797.666666666667", "797.666666666667"))
  expect_identical(w2$dur_day_total_LIG_min,
                   c("165.916666666667", "185.25", "131.083333333333", "150.416666666667"))
  expect_identical(w2$dur_day_total_MOD_min,
                   c("49", "29.6666666666667", "49", "29.6666666666667"))
  expect_identical(w2$dur_day_total_VIG_min, rep("2.25", 4))
  tot <- as.numeric(w2$dur_day_total_IN_min) + as.numeric(w2$dur_day_total_LIG_min) +
    as.numeric(w2$dur_day_total_MOD_min) + as.numeric(w2$dur_day_total_VIG_min)
  expect_equal(tot, rep(980, 4), tolerance = 1e-9)
})

test_that("every column carries the unit the man page claims", {
  S <- tus_case("MOS2")
  m <- tus_both(S, opts = list(iglevels = c(seq(0, 4000, by = 25), 8000), frag = "all",
                               sfs = TRUE))
  n <- function(col, row = 2) as.numeric(m[[col]][row])
  ws3 <- S$ws3new
  Ln <- S$levelList$Lnames
  # dur_* are minutes, each a multiple of the epoch in minutes
  durcols <- grep("^dur_", names(m), value = TRUE)
  for (nm in durcols) {
    v <- as.numeric(m[[nm]])
    expect_equal(v / (ws3 / 60), round(v / (ws3 / 60)), tolerance = 1e-9, info = nm)
  }
  expect_identical(n("dur_day_spt_min"), 1440)
  # nonwear_perc_* are percent
  for (nm in c("nonwear_perc_day", "nonwear_perc_spt", "nonwear_perc_day_spt")) {
    v <- suppressWarnings(as.numeric(m[[nm]]))
    v <- v[!is.na(v)]
    expect_true(all(v >= 0 & v <= 100), info = nm)
  }
  expect_equal(n("nonwear_perc_spt"), 16.304347826087, tolerance = 1e-9)
  # sleep_efficiency_after_onset is a fraction, though the dictionary calls it a percent
  v <- suppressWarnings(as.numeric(m$sleep_efficiency_after_onset))
  v <- v[!is.na(v)]
  expect_true(all(v >= 0 & v <= 1))
  # ACC_* are milli-g
  expect_identical(m$ACC_day_mg[2], "23.771768707483")
  expect_true(n("ACC_day_mg") > 1)
  # sleeponset and wakeup are decimal hours from the window's opening midnight; the _ts
  # pair is the local clock time of the same epoch
  expect_identical(m$sleeponset[3], "23.7680555555556")
  expect_identical(m$sleeponset_ts[3], "23:46:05")
  expect_identical(m$wakeup[3], "31.7666666666667")
  expect_identical(m$wakeup_ts[3], "07:46:00") # 07:46 the next morning
  # counts are non-negative integers
  cntcols <- c(grep("^Nbouts_|^Nblocks_", names(m), value = TRUE),
               "N_atleast5minwakenight")
  for (nm in cntcols) {
    v <- as.numeric(m[[nm]])
    expect_identical(v, round(v), info = nm)
    expect_true(all(v >= 0), info = nm)
  }
  expect_true(as.numeric(m$ig_day_gradient[2]) < 0)
  expect_true(all(as.numeric(m$ig_day_rsquared) >= 0 & as.numeric(m$ig_day_rsquared) <= 1))
  # fragmentation durations are epochs, not minutes
  expect_true(as.numeric(m$FRAG_mean_dur_IN_day[2]) > 5)
  expect_identical(m$boutcriter.in[2], "0.9")
  expect_identical(m$boutdur.in[2], "30_20_10")
  expect_identical(m$boutdur.mvpa[2], "10_5_1")
  expect_identical(m$TRLi[2], "40")
  expect_identical(m$filename_dir[2], "FULL/PATH")
  expect_identical(m$foldername[2], "FOLDER")
  expect_identical(m$tail_expansion_minutes[2], "0")
  expect_identical(length(Ln), 18L)
})

# Synthetic 5 s epochs with a lightpeak column.
tus_synth <- function(start = "2025-03-04 00:00:00", ndays = 2, ws3 = 5, tz = "UTC") {
  n <- ndays * 24 * (3600 / ws3)
  tm <- as.POSIXct(start, tz = tz) + (0:(n - 1)) * ws3
  hr <- as.numeric(format(tm, "%H"))
  diur <- as.numeric(hr >= 23 | hr < 7)
  set.seed(42)
  lp <- round(ifelse(diur == 1, 0.5, 50 + hr * 40) * exp(stats::rnorm(n, 0, 0.6)), 1)
  code <- rep(0, n)
  code[seq(10, n, by = 997)] <- 1
  code[seq(300, n, by = 1301)] <- 2
  data.frame(time = tm, ACC = round(abs(stats::rnorm(n, 30, 30)), 4),
             guider = "HDCZA", angle = 0, nonwear = 0, diur = diur, sibdetection = 0,
             lightpeak = lp, lightpeak_imputationcode = code, stringsAsFactors = FALSE)
}

test_that("P5p .raw.timeuse.lux.persegment is identical() to GGIR on an aligned window", {
  skip_if_no_ggir()
  ts <- tus_synth()
  seg <- c(0, 6, 12, 18, 24)
  sse <- 1:(24 * 720)
  mine <- .raw.timeuse.lux.persegment(ts, sse, seg, 5, "UTC")
  ggr <- GGIR:::g.part5.lux_persegment(ts, sse, seg, 5, "UTC")
  expect_identical(mine$names, ggr$names)
  expect_identical(mine$values, ggr$values)
  expect_identical(length(mine$values), 20L) # 5 metrics x 4 segments
  expect_identical(mine$names[1:4],
                   c("LUX_above1000_0_6hr_day", "LUX_above1000_6_12hr_day",
                     "LUX_above1000_12_18hr_day", "LUX_above1000_18_24hr_day"))
  # 0 to 6 h is all sleep period and 18 to 24 h is the dropped last segment, so both are NA
  expect_true(is.na(mine$values[1]))
  expect_true(is.na(mine$values[4]))
  expect_equal(mine$values[6], 300, tolerance = 1e-9)  # LUX_timeawake_6_12hr_day, minutes
  expect_equal(mine$values[7], 360, tolerance = 1e-9)
})

test_that("P5p GGIR 3.3.6 CRASHES on a 20:30 start and the 3.3-9 body does not", {
  skip_if_no_ggir()
  ts <- tus_synth(start = "2025-03-04 20:30:00", ndays = 3)
  seg <- c(0, 6, 12, 18, 24)
  sse <- 1:(24 * 720)
  # the first-segment back-fill runs off the front of the series; 3.3.6 has no guard for it
  expect_error(GGIR:::g.part5.lux_persegment(ts, sse, seg, 5, "UTC"))
  mine <- .raw.timeuse.lux.persegment(ts, sse, seg, 5, "UTC")
  expect_identical(length(mine$values), 20L)
  expect_identical(sum(is.na(mine$values)), 5L) # only the all-sleep 0 to 6 segment
  expect_equal(mine$values[8], 150, tolerance = 1e-9) # LUX_timeawake_18_24hr_day
})

test_that("P5p the whole 31-column lux block is identical() to GGIR's", {
  skip_if_no_ggir()
  ts <- tus_synth()
  tp <- as.POSIXlt(ts$time)
  pp <- list(boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8,
             boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
             boutdur.lig = c(10, 5, 1))
  LL <- .raw.identify.levels(ts = ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = 5,
                             params_phyact = pp)
  levelList <- list(threshold = c(40, 100, 400), LEVELS = LL$LEVELS, Lnames = LL$Lnames,
                    OLEVELS = LL$OLEVELS, bc.mvpa = LL$bc.mvpa, bc.in = LL$bc.in,
                    bc.lig = LL$bc.lig)
  Nday <- 24 * 720
  segments <- list(`00:00:00-23:59:55` = c(1, Nday))
  sumSleep <- data.frame(night = 1, calendar_date = "4/3/2025", daysleeper = 0,
                         cleaningcode = 1, guider = "HDCZA", sleeplog_used = 0,
                         acc_available = 1, stringsAsFactors = FALSE)
  indexlog <- list(fileIndex = 1, winType = "MM", winIndex = 1, winStartEnd = c(1, Nday),
                   segIndex1 = 1, segIndex2 = 1, segStartEnd = c(1, Nday), columnIndex = 3)
  timeList <- list(ts = ts, sec = tp$sec, min = tp$min, hour = tp$hour, time_POSIX = ts$time,
                   epochSize = 5)
  LUXthr <- c(0, 100, 500, 1000, 3000, 5000, 10000)
  seg <- c(0, 6, 12, 18, 24)
  dd <- matrix("", 3, 500); dd[1, 1:2] <- c("IDx", "FNx")
  nn <- rep("", 500); nn[1:2] <- c("ID", "filename")
  g <- GGIR:::g.part5_analyseSegment(
    indexlog, timeList, levelList, segments, "MM", dd, nn,
    params_general = list(desiredtz = "UTC"),
    params_output = list(do.sibreport = TRUE, storefolderstructure = FALSE),
    params_sleep = list(possible_nap_window = NULL, possible_nap_dur = NULL,
                        nap_model = NULL),
    params_247 = list(iglevels = NULL, LUXthresholds = LUXthr, LUX_day_segments = seg),
    params_phyact = pp, sumSleep = sumSleep, sibDef = "T5A5", fullFilename = "F",
    add_one_day_to_next_date = FALSE, lightpeak_available = TRUE,
    tail_expansion_log = NULL, foldernamei = "D", sibreport = NULL)
  gn <- g$ds_names[seq_len(max(which(g$ds_names != "")))]
  gv <- g$dsummary[1, seq_along(gn)]
  names(gv) <- gn
  gv[is.na(gv)] <- ""
  m <- .raw.timeuse.segment(indexlog, timeList, levelList, segments, "MM",
                            dsummary = list(), sumSleep = sumSleep, sibDef = "T5A5",
                            desiredtz = "UTC",
                            boutdur.mvpa = pp$boutdur.mvpa, boutdur.in = pp$boutdur.in,
                            boutdur.lig = pp$boutdur.lig,
                            boutcriter.mvpa = pp$boutcriter.mvpa,
                            boutcriter.in = pp$boutcriter.in,
                            boutcriter.lig = pp$boutcriter.lig,
                            LUXthresholds = LUXthr, LUX_day_segments = seg)
  mv <- c(ID = "IDx", filename = "FNx", .raw.timeuse.row.chr(m$dsummary[[1]]))
  expect_identical(names(mv), names(gv))
  expect_identical(mv, gv)
  lux <- grep("^LUX_", names(mv), value = TRUE)
  expect_identical(length(lux), 31L) # 4 scalars + 7 thresholds + 5 x 4 segments
  expect_identical(lux[1:4], c("LUX_max_day", "LUX_mean_day", "LUX_mean_spt",
                               "LUX_mean_day_mvpa"))
  expect_identical(lux[11], "LUX_min_10000_inf_day")
  expect_identical(mv[["LUX_max_day"]], "6576.5")
  expect_identical(mv[["LUX_mean_spt"]], "0.6")
})

test_that("P5q with no lightpeak column the LUX columns are ABSENT, not empty", {
  S <- tus_case("MOS2")
  expect_false("lightpeak" %in% names(S$ts))
  m <- tus_both(S, opts = list(LUX_day_segments = c(0, 6, 12, 18, 24)))
  expect_false(any(grepl("^LUX_", names(m))))
  expect_identical(ncol(m), 118L)
})

test_that("on MOS2 ggir_exact = FALSE changes N_atleast5minwakenight and nothing else", {
  S <- tus_case("MOS2")
  a <- tus_both(S, opts = list(ge = TRUE))
  b <- tus_both(S, opts = list(ge = FALSE))
  changed <- names(a)[!vapply(names(a), function(n) identical(a[[n]], b[[n]]), logical(1))]
  expect_identical(changed, "N_atleast5minwakenight")
  expect_identical(a$N_atleast5minwakenight, rep("0", 7))
  expect_identical(b$N_atleast5minwakenight, c("2", "2", "1", "2", "0", "1", "0"))
})

test_that("trap 34: an MM window with neither onset nor wake, both ways", {
  skip_if_no_ggir()
  ts <- tus_synth()
  ts$sibdetection <- as.numeric(ts$diur == 1 & (seq_len(nrow(ts)) %% 7 != 0))
  tp <- as.POSIXlt(ts$time)
  pp <- list(boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8,
             boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
             boutdur.lig = c(10, 5, 1))
  LL <- .raw.identify.levels(ts = ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = 5,
                             params_phyact = pp)
  levelList <- list(threshold = c(40, 100, 400), LEVELS = LL$LEVELS, Lnames = LL$Lnames,
                    OLEVELS = LL$OLEVELS, bc.mvpa = LL$bc.mvpa, bc.in = LL$bc.in,
                    bc.lig = LL$bc.lig)
  # a window wholly inside the sleep period: no diur transition inside it
  i0 <- which(format(ts$time, "%H:%M:%S") == "23:30:00")[1]
  i1 <- which(format(ts$time, "%H:%M:%S") == "05:30:00")[2]
  expect_identical(unique(ts$diur[i0:i1]), 1)
  segments <- list(`00:00:00-23:59:55` = c(i0, i1))
  sumSleep <- data.frame(night = 1, calendar_date = "4/3/2025", daysleeper = 0,
                         cleaningcode = 1, guider = "HDCZA", sleeplog_used = 0,
                         acc_available = 1, stringsAsFactors = FALSE)
  indexlog <- list(fileIndex = 1, winType = "MM", winIndex = 1, winStartEnd = c(i0, i1),
                   segIndex1 = 1, segIndex2 = 1, segStartEnd = c(i0, i1), columnIndex = 3)
  timeList <- list(ts = ts, sec = tp$sec, min = tp$min, hour = tp$hour, time_POSIX = ts$time,
                   epochSize = 5)
  dd <- matrix("", 3, 500); dd[1, 1:2] <- c("IDx", "FNx")
  nn <- rep("", 500); nn[1:2] <- c("ID", "filename")
  g <- GGIR:::g.part5_analyseSegment(
    indexlog, timeList, levelList, segments, "MM", dd, nn,
    params_general = list(desiredtz = "UTC"),
    params_output = list(do.sibreport = TRUE, storefolderstructure = FALSE),
    params_sleep = list(possible_nap_window = NULL, possible_nap_dur = NULL,
                        nap_model = NULL),
    params_247 = list(iglevels = NULL, LUXthresholds = NULL, LUX_day_segments = NULL),
    params_phyact = pp, sumSleep = sumSleep, sibDef = "T5A5", fullFilename = "F",
    add_one_day_to_next_date = FALSE, lightpeak_available = FALSE,
    tail_expansion_log = NULL, foldernamei = "D", sibreport = NULL)
  gn <- g$ds_names[seq_len(max(which(g$ds_names != "")))]
  gv <- g$dsummary[1, seq_along(gn)]
  names(gv) <- gn
  gv[is.na(gv)] <- ""
  run <- function(ge) {
    m <- .raw.timeuse.segment(indexlog, timeList, levelList, segments, "MM",
                              dsummary = list(), sumSleep = sumSleep, sibDef = "T5A5",
                              desiredtz = "UTC",
                              boutdur.mvpa = pp$boutdur.mvpa, boutdur.in = pp$boutdur.in,
                              boutdur.lig = pp$boutdur.lig,
                              boutcriter.mvpa = pp$boutcriter.mvpa,
                              boutcriter.in = pp$boutcriter.in,
                              boutcriter.lig = pp$boutcriter.lig, ggir_exact = ge)
    list(chr = c(ID = "IDx", filename = "FNx", .raw.timeuse.row.chr(m$dsummary[[1]])),
         ts = m$timeList$ts)
  }
  mT <- run(TRUE)
  mF <- run(FALSE)
  expect_identical(mT$chr, gv)
  expect_identical(mT$ts, g$timeList$ts)
  expect_false("diur_bu" %in% names(mT$ts))
  # GGIR's defect: dur_spt_min is 0 while the sleep classes report 308.67 minutes
  expect_identical(mT$chr[["dur_spt_min"]], "0")
  expect_identical(mT$chr[["dur_day_min"]], "360.083333333333")
  expect_identical(mT$chr[["dur_spt_sleep_min"]], "308.666666666667")
  expect_identical(mT$chr[["sleep_efficiency_after_onset"]], "NaN")
  expect_identical(mT$chr[["ACC_spt_mg"]], "NaN")
  expect_identical(mT$chr[["ACC_spt_mg_median"]], "") # median of nothing is NA, not NaN
  # with ggir_exact = FALSE the durations agree with the classes
  changed <- names(mT$chr)[mT$chr != mF$chr]
  expect_identical(sort(changed),
                   sort(c("nonwear_perc_day", "nonwear_perc_spt", "dur_day_min",
                          "dur_spt_min", "sleep_efficiency_after_onset", "ACC_day_mg",
                          "ACC_spt_mg", "ACC_spt_mg_median", "ACC_spt_mg_stdev")))
  expect_identical(mF$chr[["dur_spt_min"]], "360.083333333333")
  expect_identical(mF$chr[["dur_day_min"]], "0")
  expect_identical(mF$chr[["sleep_efficiency_after_onset"]], "0.857208979402916")
})

test_that(".raw.timeuse.row.chr keeps NaN and NA apart and gives 15 significant digits", {
  row <- list(a = 1 / 3, b = NaN, c = NA, d = NA_real_, e = "text", f = 0,
              g = NA_character_, h = 1e-17, i = 1234567890123456)
  out <- .raw.timeuse.row.chr(row)
  expect_identical(out[["a"]], "0.333333333333333")
  expect_identical(out[["a"]], as.character(1 / 3))
  expect_identical(out[["b"]], "NaN")
  expect_identical(out[["c"]], "")
  expect_identical(out[["d"]], "")
  expect_identical(out[["e"]], "text")
  expect_identical(out[["f"]], "0")
  expect_identical(out[["g"]], "")
  expect_identical(out[["h"]], as.character(1e-17))
  expect_identical(out[["i"]], as.character(1234567890123456))
  expect_identical(names(out), names(row))
  expect_true(is.character(out))
})

test_that(".raw.timeuse.checkshape turns a single bout setting into a one-row matrix", {
  v <- c(0, 1, 1, 0, 1)
  expect_identical(dim(.raw.timeuse.checkshape(v)), c(1L, 5L))
  expect_identical(.raw.timeuse.checkshape(v)[1, ], v)
  m <- rbind(c(0, 1), c(1, 0), c(1, 1))
  expect_identical(.raw.timeuse.checkshape(m), m)
  one <- .raw.timeuse.checkshape(1)
  expect_identical(dim(one), c(1L, 1L)) # nrow is not > ncol, so no transpose
})

test_that(".raw.lux.standardise.persegment pads to the full segment list and names the ends", {
  x <- data.frame(g = c(6, 12), v = c(10, 20))
  out <- .raw.lux.standardise.persegment(x, c(0, 6, 12, 18, 24), "mean")
  expect_identical(out$names, c("LUX_mean_0_6hr_day", "LUX_mean_6_12hr_day",
                                "LUX_mean_12_18hr_day", "LUX_mean_18_24hr_day"))
  expect_identical(out$values, c(NA, 10, 20, NA))
})

test_that("the two SPEC additions raise instead of emitting a nonsense column name", {
  S <- tus_case("MOS2")
  base <- list(indexlog = list(fileIndex = 1, winType = "MM", winIndex = 1,
                               winStartEnd = c(1, 100), segIndex1 = 1, segIndex2 = 1,
                               segStartEnd = c(1, 100), columnIndex = 3),
               timeList = list(ts = S$ts, sec = S$mid$sec, min = S$mid$min,
                               hour = S$mid$hour, time_POSIX = S$mid$time_POSIX,
                               epochSize = S$ws3new),
               levelList = S$levelList,
               segments = list(`a-b` = c(1, 100)), segments_names = "MM")
  call1 <- function(...) {
    args <- utils::modifyList(base, list(...))
    do.call(.raw.timeuse.segment,
            c(args, list(dsummary = list(), sumSleep = S$sumSleep, sibDef = S$sibDef,
                         desiredtz = S$tz,
                         boutdur.mvpa = S$pp$boutdur.mvpa, boutdur.in = S$pp$boutdur.in,
                         boutdur.lig = S$pp$boutdur.lig,
                         boutcriter.mvpa = S$pp$boutcriter.mvpa,
                         boutcriter.in = S$pp$boutcriter.in,
                         boutcriter.lig = S$pp$boutcriter.lig)))
  }
  # three index ranges would be silently truncated by GGIR
  il <- base$indexlog; il$segStartEnd <- c(1, 10, 20, 30, 40, 50)
  expect_error(call1(indexlog = il), "one or two index ranges")
  # a bout matrix whose rows do not match boutdur would produce "Nbouts_day_IN_bts_NA"
  ll <- S$levelList; ll$bc.in <- ll$bc.in[1:2, , drop = FALSE]
  expect_error(call1(levelList = ll), "one row per boutdur entry")
  expect_silent(invisible(call1()))
})

test_that("an empty segment returns doNext with the 19 header columns and no more", {
  S <- tus_case("MOS2")
  indexlog <- list(fileIndex = 1, winType = "MM", winIndex = 1, winStartEnd = c(1, 100),
                   segIndex1 = 1, segIndex2 = 1, segStartEnd = c(NA, NA), columnIndex = 3)
  timeList <- list(ts = S$ts, sec = S$mid$sec, min = S$mid$min, hour = S$mid$hour,
                   time_POSIX = S$mid$time_POSIX, epochSize = S$ws3new)
  m <- .raw.timeuse.segment(indexlog, timeList, S$levelList,
                            list(`00:00:00-23:59:55` = c(NA, NA)), "MM",
                            dsummary = list(), sumSleep = S$sumSleep, sibDef = S$sibDef,
                            desiredtz = S$tz,
                            boutdur.mvpa = S$pp$boutdur.mvpa, boutdur.in = S$pp$boutdur.in,
                            boutdur.lig = S$pp$boutdur.lig,
                            boutcriter.mvpa = S$pp$boutcriter.mvpa,
                            boutcriter.in = S$pp$boutcriter.in,
                            boutcriter.lig = S$pp$boutcriter.lig)
  expect_true(m$doNext)
  expect_identical(length(m$dsummary[[1]]), 19L)
  expect_identical(m$ds_names[19], "TRVi")
  expect_identical(m$indexlog$columnIndex, 22) # 3 + 19, as GGIR's fi reaches
})

test_that("the parameter object is only a fallback and an explicit argument always wins", {
  S <- tus_case("MOS2")
  p <- raw.params()
  p$boutdur.mvpa <- c(10, 5, 1)
  p$boutdur.in <- c(30, 20, 10)
  p$boutdur.lig <- c(10, 5, 1)
  p$desiredtz <- S$tz
  indexlog <- list(fileIndex = 1, winType = "MM", winIndex = 2, winStartEnd = c(7691, 24970),
                   segIndex1 = 1, segIndex2 = 1, segStartEnd = c(7691, 24970),
                   columnIndex = 3)
  timeList <- list(ts = S$ts, sec = S$mid$sec, min = S$mid$min, hour = S$mid$hour,
                   time_POSIX = S$mid$time_POSIX, epochSize = S$ws3new)
  segs <- list(`00:00:00-23:59:55` = c(7691, 24970))
  viaparams <- .raw.timeuse.segment(indexlog, timeList, S$levelList, segs, "MM",
                                    dsummary = list(), sumSleep = S$sumSleep,
                                    sibDef = S$sibDef, params = p)
  explicit <- .raw.timeuse.segment(indexlog, timeList, S$levelList, segs, "MM",
                                   dsummary = list(), sumSleep = S$sumSleep,
                                   sibDef = S$sibDef, desiredtz = S$tz,
                                   boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
                                   boutdur.lig = c(10, 5, 1), boutcriter.mvpa = 0.8,
                                   boutcriter.in = 0.9, boutcriter.lig = 0.8)
  expect_identical(viaparams$dsummary[[1]], explicit$dsummary[[1]])
  beaten <- .raw.timeuse.segment(indexlog, timeList, S$levelList, segs, "MM",
                                 dsummary = list(), sumSleep = S$sumSleep,
                                 sibDef = S$sibDef, params = p, storefolderstructure = TRUE,
                                 fullFilename = "X", foldernamei = "Y")
  expect_identical(beaten$dsummary[[1]]$filename_dir, "X")
  expect_false("filename_dir" %in% names(viaparams$dsummary[[1]]))
  # a different desiredtz in the object moves calendar_date, so the object is really read
  p2 <- p
  p2$desiredtz <- "UTC"
  utc <- .raw.timeuse.segment(indexlog, timeList, S$levelList, segs, "MM",
                              dsummary = list(), sumSleep = S$sumSleep,
                              sibDef = S$sibDef, params = p2)
  expect_identical(viaparams$dsummary[[1]]$calendar_date, "2025-10-08")
  expect_identical(utc$dsummary[[1]]$calendar_date,
                   as.character(as.Date(S$ts$time[7691], tz = "UTC")))
  # the _ts pair does not move: strftime(format(POSIXct), tz = ...) renders the local clock
  # first, so the timezone is a no-op there
  expect_identical(viaparams$dsummary[[1]]$sleeponset_ts,
                   utc$dsummary[[1]]$sleeponset_ts)
})

test_that("P5o ragged frag.metrics keeps every row's own names, and still matches GGIR", {
  skip_if_no_ggir()
  S <- tus_case("MOS2")
  # a quarter-hour first segment has fewer than ten fragments on three of the four days, so
  # .raw.fragmentation returns 16 columns there and 18 everywhere else
  opts <- list(frag = c("mean", "TP"))
  qw <- c(0, 0.25, 24)
  m <- tus_run(S, "MM", tus_mine, opts, qwindow = qw)
  widths <- vapply(m$rows, function(r) sum(grepl("^FRAG_", names(r))), integer(1))
  expect_identical(widths, c(18L, 0L, 18L, 18L, 16L, 18L, 18L, 16L, 18L, 18L, 16L, 18L))
  expect_identical(tail(names(m$rows[[5]]), 2L), c("FRAG_mean_dur_IN_day",
                                                   "FRAG_mean_dur_PA_day"))
  expect_identical(tail(names(m$rows[[6]]), 2L), c("FRAG_SD_dur_PA_day",
                                                   "FRAG_SD_dur_IN_day"))
  tus_compare_live("MOS2", opts = opts, tws = "MM", qwindow = qw)
  # two more columns after the ragged block, where GGIR's shared ds_names would drift
  opts2 <- list(frag = c("mean", "TP"), sfs = TRUE)
  tus_compare_live("MOS2", opts = opts2, tws = "MM", qwindow = qw)
  m2 <- tus_run(S, "MM", tus_mine, opts2, qwindow = qw)
  expect_identical(tail(names(m2$rows[[5]]), 2L), c("filename_dir", "foldername"))
  expect_identical(tail(names(m2$rows[[6]]), 2L), c("filename_dir", "foldername"))
  t2 <- tus_tab_mine(m2, S)
  expect_identical(ncol(t2), 138L)
  expect_identical(t2$filename_dir[5], "FULL/PATH")
  expect_identical(t2$FRAG_SD_dur_PA_day[5], "") # the column this row never emitted
  expect_identical(t2$FRAG_SD_dur_PA_day[6] != "", TRUE)
})

test_that("a segment carrying two index ranges is summed over both, not just the first", {
  # with segment_all_windows = TRUE (GGIR 3.3-9's form) the 00:00 to 08:00 segment of MOS2's
  # WW window 1 falls at both ends of the window: indices 7690, 8280, 19801 and 25392
  S <- tus_case("MOS2")
  m2 <- tus_walk(S, "WW", function(st, ...) { st$opts <- tus_opts_default; tus_mine(st, ...) },
                 qwindow = c(0, 8, 24), segment_all_windows = TRUE)
  expect_identical(length(m2$rows), 9L) # 3 windows x 3 segments, none empty
  r <- m2$rows[[2]]
  expect_identical(r$window, "WWsegment1")
  expect_identical(r$start_end_window, "00:00:00-07:59:55")
  # sse is the concatenation of both ranges: 591 + 5592 epochs
  n1 <- 8280 - 7690 + 1
  n2 <- 25392 - 19801 + 1
  expect_identical(n1 + n2, 6183)
  expect_identical(r$dur_day_spt_min, (n1 + n2) * S$ws3new / 60)
  expect_identical(r$dur_day_spt_min, 515.25)
  expect_identical(r$dur_day_min + r$dur_spt_min, r$dur_day_spt_min)
  # WLH in the two-range form, sum(qqq2 - qqq1) + 1, without the 1.001 floor
  WLH <- ((8280 - 7690) + (25392 - 19801) + 1) / ((60 / S$ws3new) * 60)
  acc <- S$ts$ACC[c(7690:8280, 19801:25392)]
  expect_identical(r$quantile_mostactive60min_mg,
                   as.numeric(stats::quantile(acc, probs = (WLH - 1) / WLH, na.rm = TRUE)))
  expect_identical(r$quantile_mostactive30min_mg,
                   as.numeric(stats::quantile(acc, probs = (WLH - 0.5) / WLH, na.rm = TRUE)))
  expect_identical(r$quantile_mostactive60min_mg, 8.6)
  expect_identical(r$quantile_mostactive30min_mg, 43.5)
  # the guider loop writes over both ranges
  expect_identical(unique(m2$ts$guider[7690:8280]), "HDCZA")
  expect_identical(unique(m2$ts$guider[19801:25392]), "HDCZA")
})

test_that("against the GGIR 3.3-9 clone the ONLY difference is the two quantile columns", {
  src <- tus_clone()
  if (src == "" || !file.exists(file.path(src, "g.part5_analyseSegment.R"))) {
    testthat::skip("no GGIR 3.3-9 clone at CANHRACTI_GGIR_SRC or beside the reference")
  }
  skip_if_no_ggir()
  env <- new.env(parent = asNamespace("GGIR"))
  for (f in c("g.part5_analyseSegment.R", "g.part5.onsetwaketiming.R",
              "g.part5.lux_persegment.R")) {
    sys.source(file.path(src, f), envir = env)
  }
  seg339 <- get("g.part5_analyseSegment", envir = env)
  S <- tus_case("MOS2")
  clone_fun <- function(state, S2, timewindowi, wi, qqq, si, csi, segStart, segEnd,
                        segments, segments_names, aod) {
    if (is.null(state$acc)) {
      state$acc <- list(dsummary = matrix("", 40, 500), ds_names = rep("", 500))
    }
    ds <- state$acc$dsummary; dn <- state$acc$ds_names
    ds[si, 1:2] <- c(S2$ID, S2$fname); dn[1:2] <- c("ID", "filename")
    gas <- seg339(
      indexlog = list(fileIndex = 1, winType = timewindowi, winIndex = wi, winStartEnd = qqq,
                      segIndex1 = si, segIndex2 = csi,
                      segStartEnd = c(segStart, segEnd), columnIndex = 3),
      timeList = list(ts = state$ts, sec = S2$mid$sec, min = S2$mid$min, hour = S2$mid$hour,
                      time_POSIX = S2$mid$time_POSIX, epochSize = S2$ws3new),
      levelList = state$levelList, segments = segments, segments_names = segments_names,
      dsummary = ds, ds_names = dn,
      params_general = list(desiredtz = S2$tz),
      params_output = list(do.sibreport = TRUE, storefolderstructure = FALSE),
      params_sleep = list(possible_nap_window = NULL, possible_nap_dur = NULL,
                          nap_model = NULL),
      params_247 = list(iglevels = NULL, LUXthresholds = NULL, LUX_day_segments = NULL),
      params_phyact = S2$pp, sumSleep = S2$sumSleep, sibDef = S2$sibDef,
      fullFilename = "FULL/PATH", add_one_day_to_next_date = aod,
      lightpeak_available = FALSE, tail_expansion_log = NULL, foldernamei = "FOLDER",
      sibreport = NULL)
    state$acc$dsummary <- gas$dsummary
    state$acc$ds_names <- gas$ds_names
    state$ts <- gas$timeList$ts
    state$levelList$Lnames <- gas$timeList$Lnames
    state$levelList$LEVELS <- gas$timeList$LEVELS
    state$aod <- gas$add_one_day_to_next_date
    state
  }
  for (tw in c("MM", "WW")) {
    g <- tus_walk(S, tw, function(st, ...) { st$opts <- tus_opts_default; clone_fun(st, ...) },
                  qwindow = c(0, 8, 24), segment_all_windows = TRUE)
    m <- tus_walk(S, tw, function(st, ...) { st$opts <- tus_opts_default; tus_mine(st, ...) },
                  qwindow = c(0, 8, 24), segment_all_windows = TRUE)
    gt <- tus_tab_ggir(g)
    mt <- tus_tab_mine(m, S)
    expect_identical(names(mt), names(gt), info = tw)
    differ <- names(gt)[!vapply(names(gt), function(n) identical(gt[[n]], mt[[n]]),
                                logical(1))]
    expect_identical(differ, c("quantile_mostactive60min_mg", "quantile_mostactive30min_mg"),
                     info = tw)
    # 3.3-9 blanks them on every row after the first, this port fills every row
    expect_true(all(gt$quantile_mostactive60min_mg[-1] == ""), info = tw)
    filled <- mt$dur_day_spt_min != "" # the MM run has one empty segment row
    expect_true(all(mt$quantile_mostactive60min_mg[filled] != ""), info = tw)
    expect_identical(gt$quantile_mostactive60min_mg[1], mt$quantile_mostactive60min_mg[1],
                     info = tw)
    expect_identical(m$ts, g$ts, info = tw)
  }
})

test_that("the OO timewindow is identical() to GGIR's, and shows the two-epoch onset offset", {
  skip_if_no_ggir()
  tus_compare_live("MOS2", tws = "OO")
  S <- tus_case("MOS2")
  m <- tus_tab_mine(tus_run(S, "OO", tus_mine), S)
  expect_identical(nrow(m), 3L)
  expect_identical(m$window, rep("OO", 3))
  # qqq[1] is the onset transition plus one and onseti is qqq[1] + 1; diff() absorbs one of
  # them, so the onset lands one epoch after part 4's
  ns <- S$sumSleep
  for (i in 1:3) {
    nb <- as.numeric(m$night_number[i])
    expect_equal(as.numeric(m$sleeponset[i]),
                 ns$sleeponset[ns$night == nb] + 1 * S$ws3new / 3600,
                 tolerance = 1e-12, info = paste("night", nb))
  }
  expect_identical(m$sleeponset[1], "22.9875")
  expect_identical(m$sleeponset_ts[1], "22:59:15")
  expect_identical(ns$sleeponset_ts[ns$night == 1], "22:59:10")
})

test_that("a non-empty tail_expansion_log fills tail_expansion_minutes, as GGIR does", {
  skip_if_no_ggir()
  S <- tus_case("MOS2")
  tel <- list(short = 120, long = 10)
  args <- list(indexlog = list(fileIndex = 1, winType = "MM", winIndex = 2,
                               winStartEnd = c(7691, 24970), segIndex1 = 1, segIndex2 = 1,
                               segStartEnd = c(7691, 24970), columnIndex = 3),
               timeList = list(ts = S$ts, sec = S$mid$sec, min = S$mid$min,
                               hour = S$mid$hour, time_POSIX = S$mid$time_POSIX,
                               epochSize = S$ws3new),
               levelList = S$levelList,
               segments = list(`00:00:00-23:59:55` = c(7691, 24970)), segments_names = "MM")
  m <- .raw.timeuse.segment(args$indexlog, args$timeList, args$levelList, args$segments, "MM",
                            dsummary = list(), sumSleep = S$sumSleep, sibDef = S$sibDef,
                            desiredtz = S$tz, boutdur.mvpa = S$pp$boutdur.mvpa,
                            boutdur.in = S$pp$boutdur.in, boutdur.lig = S$pp$boutdur.lig,
                            boutcriter.mvpa = S$pp$boutcriter.mvpa,
                            boutcriter.in = S$pp$boutcriter.in,
                            boutcriter.lig = S$pp$boutcriter.lig, tail_expansion_log = tel)
  expect_identical(m$dsummary[[1]]$tail_expansion_minutes, 120 * S$ws3new / 60)
  expect_identical(m$dsummary[[1]]$tail_expansion_minutes, 10)
  dd <- matrix("", 3, 500); dd[1, 1:2] <- c("IDx", "FNx")
  nn <- rep("", 500); nn[1:2] <- c("ID", "filename")
  g <- GGIR:::g.part5_analyseSegment(
    args$indexlog, args$timeList, args$levelList, args$segments, "MM", dd, nn,
    params_general = list(desiredtz = S$tz),
    params_output = list(do.sibreport = TRUE, storefolderstructure = FALSE),
    params_sleep = list(possible_nap_window = NULL, possible_nap_dur = NULL,
                        nap_model = NULL),
    params_247 = list(iglevels = NULL, LUXthresholds = NULL, LUX_day_segments = NULL),
    params_phyact = S$pp, sumSleep = S$sumSleep, sibDef = S$sibDef, fullFilename = "F",
    add_one_day_to_next_date = FALSE, lightpeak_available = FALSE,
    tail_expansion_log = tel, foldernamei = "D", sibreport = NULL)
  gn <- g$ds_names[seq_len(max(which(g$ds_names != "")))]
  gv <- g$dsummary[1, seq_along(gn)]
  names(gv) <- gn
  gv[is.na(gv)] <- ""
  mv <- c(ID = "IDx", filename = "FNx", .raw.timeuse.row.chr(m$dsummary[[1]]))
  expect_identical(mv, gv)
})
