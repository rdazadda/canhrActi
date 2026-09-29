# Parity tests for R/raw_timeuse_timeseries.R against GGIR part 5's g.part5.savetimeseries.
# The series is rebuilt the way part 5 builds it, with ts threaded through the window loop,
# so the guider column is real rather than copied out of the answer. Reference data are found
# through CANHRACTI_GGIR_REF, the part-5 fixtures through CANHRACTI_GGIR_P5FIX and the 3.3-9
# clone through CANHRACTI_GGIR_SRC, both falling back to siblings of the reference folder;
# every block skips without them, and the live comparisons skip when GGIR is not installed.

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

tut_fixdir <- function(name) {
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
tut_clone <- function() {
  src <- Sys.getenv("CANHRACTI_GGIR_SRC", unset = "")
  if (src == "" && .ggir_ref != "") {
    src <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref, winslash = "/",
                                                          mustWork = FALSE))),
                     "ggir-src", "GGIR", "R")
  }
  src
}

tut_loadenv <- function(path) {
  e <- new.env()
  load(path, envir = e)
  e
}

# the stored series of one threshold folder, with the objects saved beside it
tut_stored <- function(metadir, folder = "40_100_400") {
  d <- file.path(metadir, "ms5.outraw", folder)
  if (!dir.exists(d) || length(dir(d)) == 0) {
    testthat::skip(paste("no stored series in", d))
  }
  tut_loadenv(file.path(d, dir(d)[1]))
}

# The state g.part5 holds when it calls g.part5.savetimeseries, from the stored milestones.
tut_state <- function(metadir, TRLi = 40, TRMi = 100, TRVi = 400) {
  fn <- dir(file.path(metadir, "ms2.out"))[1]
  imp <- tut_loadenv(file.path(metadir, "ms2.out", fn))
  bas <- tut_loadenv(file.path(metadir, "basic", paste0("meta_", fn)))
  ms4 <- tut_loadenv(file.path(metadir, "ms4.out", fn))
  mdat <- tut_stored(metadir)$mdat
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
  list(ts = ts, mid = mid, ws3new = ws3new, pp = pp, tz = tz, sibDef = sibDef,
       sumSleep = ms4$nightsummary[ms4$nightsummary$sleepparam == sibDef, ],
       ID = sub("[.]RData$", "", fn), fname = fn,
       levelList = list(threshold = c(TRLi, TRMi, TRVi), LEVELS = LL$LEVELS,
                        Lnames = LL$Lnames, OLEVELS = LL$OLEVELS, bc.mvpa = LL$bc.mvpa,
                        bc.in = LL$bc.in, bc.lig = LL$bc.lig))
}

# GGIR's window loop for one timewindow pass, ts threaded in and out
tut_walk <- function(S, ts, levelList, timewindowi) {
  ts$window <- 0
  tn <- .raw.timeuse.trim.nights(S$mid$nightsi, ts, timewindowi, S$ws3new)
  nightsi <- tn$nightsi
  Nwindows <- tn$Nwindows
  aod <- FALSE
  lastDay <- ifelse(Nwindows > 0 && length(nightsi) > 0, FALSE, TRUE)
  wi <- 1; di <- 1; si_used <- 0; rows <- list()
  while (lastDay == FALSE) {
    dd <- .raw.timeuse.define.days(nightsi, wi, S$ws3new, ts, timewindowi, Nwindows,
                                   qwindow = c(0, 24), ID = S$ID)
    qqq <- dd$qqq; lastDay <- dd$lastDay
    if (length(which(is.na(qqq) == TRUE)) == 0) {
      if ((qqq[2] - qqq[1]) * S$ws3new > 900) {
        ts$window[qqq[1]:qqq[2]] <- wi
        next_si <- if (di == 1) 1 else si_used + 1
        for (si in next_si:(next_si + length(dd$segments) - 1)) {
          csi <- si - next_si + 1
          Nind <- length(dd$segments[[csi]])
          segStart <- dd$segments[[csi]][seq(1, Nind, by = 2)]
          segEnd <- dd$segments[[csi]][seq(2, Nind, by = 2)]
          gas <- .raw.timeuse.segment(
            indexlog = list(fileIndex = 1, winType = timewindowi, winIndex = wi,
                            winStartEnd = qqq, segIndex1 = si, segIndex2 = csi,
                            segStartEnd = c(segStart, segEnd), columnIndex = 3),
            timeList = list(ts = ts, sec = S$mid$sec, min = S$mid$min, hour = S$mid$hour,
                            time_POSIX = S$mid$time_POSIX, epochSize = S$ws3new),
            levelList = levelList, segments = dd$segments,
            segments_names = dd$segments_names, dsummary = rows, sumSleep = S$sumSleep,
            sibDef = S$sibDef, add_one_day_to_next_date = aod, desiredtz = S$tz,
            boutdur.mvpa = S$pp$boutdur.mvpa, boutdur.in = S$pp$boutdur.in,
            boutdur.lig = S$pp$boutdur.lig, boutcriter.mvpa = S$pp$boutcriter.mvpa,
            boutcriter.in = S$pp$boutcriter.in, boutcriter.lig = S$pp$boutcriter.lig,
            frag.metrics = NULL, iglevels = NULL,
            LUXthresholds = c(0, 100, 500, 1000, 3000, 5000, 10000),
            LUX_day_segments = NULL, do.sibreport = TRUE, storefolderstructure = FALSE,
            fullFilename = "FULL/PATH", foldernamei = "FOLDER", tail_expansion_log = NULL,
            sibreport = NULL, warn_xmin = FALSE, ggir_exact = TRUE)
          rows <- gas$dsummary
          ts <- gas$timeList$ts
          levelList$Lnames <- gas$timeList$Lnames
          levelList$LEVELS <- gas$timeList$LEVELS
          aod <- gas$add_one_day_to_next_date
        }
        si_used <- si
        di <- di + 1
      }
    }
    di <- di + 1; wi <- wi + 1
  }
  list(ts = ts, levelList = levelList)
}

# every timewindow in order, keeping the snapshot at the end of each pass
tut_pipeline <- function(metadir, tws = c("MM", "WW"), TR = c(40, 100, 400)) {
  S <- tut_state(metadir, TR[1], TR[2], TR[3])
  ts <- S$ts
  LL <- S$levelList
  snapshots <- list()
  for (tw in tws) {
    w <- tut_walk(S, ts, LL, tw)
    ts <- w$ts
    LL <- w$levelList
    snapshots[[tw]] <- ts
  }
  list(S = S, ts = ts, snapshots = snapshots, LEVELS = LL$LEVELS, Lnames = LL$Lnames,
       tz = S$tz, ID = S$ID, last = tws[length(tws)])
}

tut_cache <- new.env(parent = emptyenv())
tut_case <- function(key, metadir, tws = c("MM", "WW"), TR = c(40, 100, 400)) {
  if (!is.null(tut_cache[[key]])) return(tut_cache[[key]])
  skip_if_no_ggir_ref()
  if (!dir.exists(metadir)) testthat::skip(paste("no meta folder at", metadir))
  P <- tut_pipeline(metadir, tws, TR)
  P$metadir <- metadir
  tut_cache[[key]] <- P
  P
}

tut_ref_meta <- function(case) {
  if (case == "MOS2") ref_file("out", "output_din", "meta")
  else ref_file("timing_out", "output_timing", "meta")
}

# a live GGIR call, through a temporary file, returning the RData frame and the csv
tut_ggir <- function(ts, LEVELS, desiredtz = "UTC", timewindow = "MM", DaCleanFile = NULL,
                     includedaycrit.part5 = 2 / 3, includenightcrit.part5 = 0, ID = "S1",
                     Lnames = NULL, filename = "f.gt3x", params_output = list()) {
  dir <- file.path(tempdir(), paste0("tut", as.integer(runif(1, 1, 1e9))))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  csvname <- file.path(dir, "s.csv")
  po <- list(save_ms5raw_format = c("csv", "RData"), save_ms5raw_without_invalid = FALSE,
             sep_reports = ",", dec_reports = ".",
             require_complete_lastnight_part5 = FALSE)
  po <- utils::modifyList(po, params_output)
  GGIR:::g.part5.savetimeseries(
    ts = ts, LEVELS = LEVELS, desiredtz = desiredtz,
    rawlevels_fname = c(csvname, sub("csv$", "RData", csvname)), DaCleanFile = DaCleanFile,
    includedaycrit.part5 = includedaycrit.part5,
    includenightcrit.part5 = includenightcrit.part5, ID = ID, params_output = po,
    params_247 = list(part6HCA = FALSE, part6CR = FALSE), Lnames = Lnames,
    timewindow = timewindow, filename = filename)
  rdata <- sub("csv$", "RData", csvname)
  if (!file.exists(rdata)) return(NULL)
  e <- tut_loadenv(rdata)
  list(mdat = e$mdat, objects = sort(ls(e)),
       csv = data.table::fread(csvname, data.table = FALSE))
}

# a small synthetic series, four hours at 5 s
tut_synth <- function(n = 2880, tz = "UTC") {
  t0 <- as.POSIXct("2025-03-01 00:00:00", tz = tz)
  x <- data.frame(time = t0 + (0:(n - 1)) * 5,
                  ACC = round(seq(0, 300, length.out = n), 7),
                  diur = rep(c(1, 0, 1, 0), each = n / 4),
                  nonwear = rep(0, n), guider = "HDCZA", window = 0, sibdetection = 0,
                  stringsAsFactors = FALSE)
  x$nonwear[1:200] <- 1
  x$nonwear[1500:1600] <- 1
  x
}
tut_levels <- function(n = 2880) rep(0:5, length.out = n)

# name every column on which two frames disagree
tut_diffcols <- function(a, b) {
  nm <- union(names(a), names(b))
  nm[!vapply(nm, function(n) identical(a[[n]], b[[n]]), logical(1))]
}

test_that("P6a the MOS2 series is identical() to the stored 118800 x 14 frame", {
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = P$last,
                                  filename = E$filename)
  expect_identical(dim(mine), c(118800L, 14L))
  expect_identical(names(mine),
                   c("timenum", "ACC", "SleepPeriodTime", "invalidepoch", "guider",
                     "window", "sibdetection", "selfreported", "angle", "class_id",
                     "invalid_fullwindow", "invalid_sleepperiod", "invalid_wakinghours",
                     "timestamp"))
  # column by column first, so a failure says which column and where
  for (nm in names(E$mdat)) {
    ok <- identical(mine[[nm]], E$mdat[[nm]])
    if (!ok) {
      i <- which(!(mine[[nm]] == E$mdat[[nm]]) |
                   xor(is.na(mine[[nm]]), is.na(E$mdat[[nm]])))[1]
      fail(paste0("column ", nm, " differs first at index ", i, ": mine ",
                  format(mine[[nm]][i]), " stored ", format(E$mdat[[nm]][i])))
    }
    expect_true(ok)
  }
  expect_identical(mine, E$mdat)              # whole frame, attributes included
  expect_identical(attr(mine, "row.names"), attr(E$mdat, "row.names"))
  expect_identical(unique(diff(mine$timenum)), 5)
  expect_identical(mine$timenum[1], as.numeric(mine$timestamp[1]))
})

test_that("P6d the stored series carries a WW window column beside an MM guider column", {
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW")
  # window is the last timewindow's numbering: three WW windows
  expect_identical(table(mine$window), table(E$mdat$window))
  expect_identical(as.integer(table(mine$window)), c(72670L, 17703L, 16708L, 11719L))
  w <- rle(mine$window)
  expect_identical(w$lengths, c(7689L, 17703L, 16708L, 11719L, 64981L))
  expect_identical(w$values, c(0, 1, 2, 3, 0))
  # guider accumulated across both passes: 2 (HDCZA) over the four MM windows
  expect_identical(as.integer(table(mine$guider)), c(64440L, 54360L))
  g <- rle(mine$guider)
  expect_identical(g$lengths, c(54360L, 64440L))
  expect_identical(g$values, c(2, 0))
  # the MM pass reached 54360, the WW pass only 53819
  expect_true(max(which(mine$guider != 0)) > max(which(mine$window != 0)))
})

test_that("P6e the three invalid columns keep their 100 sentinel outside every window", {
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW")
  # the chained assignment creates them right to left, so this is the order
  expect_identical(names(mine)[11:13],
                   c("invalid_fullwindow", "invalid_sleepperiod", "invalid_wakinghours"))
  out <- mine$window == 0
  expect_identical(sum(out), 72670L)
  for (nm in names(mine)[11:13]) {
    expect_true(all(mine[[nm]][out] == 100))
    expect_true(all(mine[[nm]][!out] == 0))
  }
  # the leading and trailing runs are the sentinel, because the fill loop stops one run short
  expect_identical(unique(mine$invalid_fullwindow[1:7689]), 100)
  expect_identical(unique(mine$invalid_fullwindow[53820:118800]), 100)
  # the leading run is 11.71 percent invalid and the column says 100
  expect_identical(sum(mine$invalidepoch[1:7689]), 900)
  expect_equal(round(mean(mine$invalidepoch[1:7689]) * 100, 2), 11.71)
  # the trailing run happens to be 100 percent invalid as well
  expect_equal(round(mean(mine$invalidepoch[53820:118800]) * 100, 2), 100)
})

test_that("P6c the EE series is identical() to the stored 120960 x 14 frame", {
  P <- tut_case("EE", tut_ref_meta("EE"))
  E <- tut_stored(P$metadir)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = P$last,
                                  filename = E$filename)
  expect_identical(dim(mine), c(120960L, 14L))
  expect_identical(E$desiredtz, "Europe/Helsinki")
  expect_identical(attr(mine$timestamp, "tzone"), "Europe/Helsinki")
  for (nm in names(E$mdat)) expect_identical(mine[[nm]], E$mdat[[nm]])
  expect_identical(mine, E$mdat)
  expect_identical(as.integer(table(mine$window)),
                   c(34483L, 19349L, 16890L, 15913L, 18833L, 15492L))
  # EE is the recording with a partly invalid window, so the rounding is under test here
  triples <- unique(mine[, c("invalid_fullwindow", "invalid_sleepperiod",
                             "invalid_wakinghours")])
  rownames(triples) <- NULL
  expect_identical(triples,
                   data.frame(invalid_fullwindow = c(100, 0, 14.7),
                              invalid_sleepperiod = c(100, 0, 0),
                              invalid_wakinghours = c(100, 0, 20.22)))
})

test_that("P6b the objects stored beside the frame, and class_id indexes Lnames", {
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  # save() takes its names from substitute, so desiredtz_part1 = desiredtz is stored as desiredtz
  expect_identical(sort(ls(E)), c("desiredtz", "filename", "Lnames", "mdat"))
  expect_identical(E$filename, "MOS2E39230594.gt3x")
  expect_identical(length(E$Lnames), 18L)
  expect_identical(E$Lnames[1:5], c("spt_sleep", "spt_wake_IN", "spt_wake_LIG",
                                    "spt_wake_MOD", "spt_wake_VIG"))
  expect_identical(E$Lnames[18], "day_LIG_bts_1_5")
  expect_identical(P$Lnames, E$Lnames)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW")
  expect_true(all(mine$class_id >= 0 & mine$class_id <= 17))
  expect_identical(sort(unique(mine$class_id)),
                   c(0, 1, 2, 3, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16, 17))
  expect_identical(as.integer(table(mine$class_id)[c("0", "12")]), c(16650L, 68353L))
})

test_that("the WW arm of the last-window suppression reproduces the lastnight fixture", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tut_fixdir("lastnight"), "output_din", "meta")
  if (!dir.exists(metadir)) skip("the F-P5-6 lastnight fixture is not present")
  P <- tut_case("lastnight", metadir)
  E <- tut_stored(metadir)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW",
                                  require_complete_lastnight_part5 = TRUE)
  expect_identical(dim(mine), c(60120L, 14L))
  expect_identical(mine, E$mdat)
  # WW window 3 is relabelled 0, so 11719 epochs move out of a window
  expect_identical(as.integer(table(mine$window)), c(25709L, 17703L, 16708L))
  expect_identical(sum(mine$SleepPeriodTime), 17903)
  expect_identical(sum(mine$invalidepoch), 7200)
  expect_identical(sum(mine$sibdetection), 24478)
  expect_identical(as.integer(table(mine$guider)), c(5760L, 54360L))
  # the recording ends at 07:59:55, so lastHour is 7 and the trailing run is 8.75 hours
  expect_identical(format(mine$timestamp[nrow(mine)], "%H:%M:%S"), "07:59:55")
  # before the suppression the trailing run was 6302 epochs, 8.7528 hours, below the WW bound of 21
  off <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                 Lnames = P$Lnames, timewindow = "WW")
  expect_identical(which(rev(off$window) != 0)[1], 6302L)
  expect_equal((6302 * 5) / 3600, 8.7528, tolerance = 1e-4)
  # afterwards the zeroed window 3 has joined that run
  expect_identical(which(rev(mine$window) != 0)[1], 18021L)
})

test_that("the MM arm of the last-window suppression reproduces the lastnight_MM fixture", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tut_fixdir("lastnight_MM"), "output_din", "meta")
  if (!dir.exists(metadir)) skip("the F-P5-6 lastnight_MM fixture is not present")
  P <- tut_case("lastnight_MM", metadir, tws = "MM")
  E <- tut_stored(metadir)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "MM",
                                  require_complete_lastnight_part5 = TRUE)
  expect_identical(mine, E$mdat)
  # window 5, the 5760-epoch partial tail, is relabelled 0
  expect_identical(as.integer(table(mine$window)),
                   c(5760L, 2520L, 17280L, 17280L, 17280L))
  expect_identical(max(mine$window), 4)
})

test_that("the matched control shows the setting changes exactly four columns", {
  skip_if_no_ggir_ref()
  md1 <- file.path(tut_fixdir("lastnight"), "output_din", "meta")
  md0 <- file.path(tut_fixdir("lastnight_ctrl"), "output_din", "meta")
  if (!dir.exists(md1) || !dir.exists(md0)) skip("the F-P5-6 fixtures are not present")
  P1 <- tut_case("lastnight", md1)
  P0 <- tut_case("lastnight_ctrl", md0)
  E0 <- tut_stored(md0)
  on_ <- .raw.timeuse.timeseries(ts = P1$ts, LEVELS = P1$LEVELS, desiredtz = E0$desiredtz,
                                 Lnames = P1$Lnames, timewindow = "WW",
                                 require_complete_lastnight_part5 = TRUE)
  off <- .raw.timeuse.timeseries(ts = P0$ts, LEVELS = P0$LEVELS, desiredtz = E0$desiredtz,
                                 Lnames = P0$Lnames, timewindow = "WW",
                                 require_complete_lastnight_part5 = FALSE)
  expect_identical(off, E0$mdat)
  expect_identical(as.integer(table(off$window)), c(13990L, 17703L, 16708L, 11719L))
  expect_identical(tut_diffcols(on_, off),
                   c("window", "invalid_fullwindow", "invalid_sleepperiod",
                     "invalid_wakinghours"))
  moved <- which(on_$window != off$window)
  expect_identical(length(moved), 11719L)
  expect_identical(unique(off$window[moved]), 3)
  expect_identical(unique(on_$window[moved]), 0)
  expect_true(all(off$invalid_fullwindow[moved] == 0))
  expect_true(all(on_$invalid_fullwindow[moved] == 100))
})

test_that("each threshold folder of the F-P5-5 fixture is reproduced identically", {
  skip_if_no_ggir_ref()
  metadir <- file.path(tut_fixdir("thresholds"), "output_din", "meta")
  if (!dir.exists(metadir)) skip("the F-P5-5 thresholds fixture is not present")
  folders <- c("30_100_400", "30_125_400", "40_100_400", "40_125_400")
  expect_true(all(dir.exists(file.path(metadir, "ms5.outraw", folders))))
  frames <- list()
  for (f in folders) {
    TR <- as.numeric(strsplit(f, "_", fixed = TRUE)[[1]])
    P <- tut_case(paste0("thr_", f), metadir, TR = TR)
    E <- tut_stored(metadir, f)
    mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                    Lnames = P$Lnames, timewindow = "WW")
    expect_identical(dim(mine), c(118800L, 14L))
    expect_identical(mine, E$mdat)
    frames[[f]] <- mine
  }
  # the threshold set moves class_id and nothing else
  for (f in folders[-1]) {
    expect_identical(tut_diffcols(frames[[f]], frames[[1]]), "class_id")
  }
  expect_identical(as.integer(table(frames[["30_100_400"]]$class_id)[c("12", "15")]),
                   c(60247L, 3577L))
  expect_identical(as.integer(table(frames[["40_100_400"]]$class_id)[c("12", "15")]),
                   c(68353L, 363L))
  expect_identical(as.integer(table(frames[["30_125_400"]]$class_id)[c("15", "16", "17")]),
                   c(6540L, 2767L, 4659L))
  expect_identical(as.integer(table(frames[["40_125_400"]]$class_id)[c("15", "16", "17")]),
                   c(881L, 1333L, 4447L))
  for (f in folders) {
    expect_identical(sum(frames[[f]]$SleepPeriodTime), 17903)
    expect_identical(sum(frames[[f]]$invalidepoch), 65880)
    expect_identical(sum(frames[[f]]$sibdetection), 24478)
  }
})

test_that("the per-timewindow series differs from the faithful one in exactly four columns", {
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  faithful <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                      Lnames = P$Lnames, timewindow = "WW")
  native <- .raw.timeuse.timeseries.bywindow(P$snapshots, LEVELS = P$LEVELS,
                                             desiredtz = E$desiredtz, Lnames = P$Lnames)
  expect_identical(names(native), c("MM", "WW"))
  # the last timewindow's snapshot is the state the faithful call sees
  expect_identical(native$WW, faithful)
  expect_identical(native$WW, E$mdat)
  expect_identical(tut_diffcols(native$MM, faithful),
                   c("window", "invalid_fullwindow", "invalid_sleepperiod",
                     "invalid_wakinghours"))
  expect_identical(native$MM$guider, faithful$guider) # the WW pass wrote no new guider
  expect_identical(as.integer(table(native$MM$window)),
                   c(64440L, 2520L, 17280L, 17280L, 17280L))
  # the MM grid starts at epoch 1, so there is no leading sentinel run in the MM frame
  expect_identical(native$MM$invalid_fullwindow[1], 0)
  expect_identical(faithful$invalid_fullwindow[1], 100)
  expect_identical(native$MM$invalid_fullwindow[118800], 100)
  expect_identical(faithful$invalid_fullwindow[118800], 100)
  # the per-epoch columns are the ones no timewindow pass touches
  for (nm in c("timenum", "ACC", "SleepPeriodTime", "invalidepoch", "sibdetection",
               "selfreported", "angle", "class_id", "timestamp")) {
    expect_identical(native$MM[[nm]], faithful[[nm]])
  }
})

test_that("the per-timewindow entry point validates its list", {
  ts <- tut_synth()
  ts$window[289:864] <- 1
  L <- tut_levels()
  expect_error(.raw.timeuse.timeseries.bywindow(ts, L), "named list")
  expect_error(.raw.timeuse.timeseries.bywindow(list(ts, ts), L), "must be named")
  expect_error(.raw.timeuse.timeseries.bywindow(list(ZZ = ts), L), "the timewindows are")
  expect_error(.raw.timeuse.timeseries.bywindow(stats::setNames(list(ts, ts), c("MM", "MM")),
                                                L), "twice")
  expect_error(.raw.timeuse.timeseries.bywindow(list(MM = ts), L, timewindow = "MM"),
               "must not be passed")
  out <- .raw.timeuse.timeseries.bywindow(list(MM = ts), L, desiredtz = "UTC")
  expect_identical(names(out), "MM")
  expect_identical(out$MM, .raw.timeuse.timeseries(ts, L, "UTC", timewindow = "MM"))
})

test_that("P6f the MOS2 frame is identical() to a live g.part5.savetimeseries, 13 and 14", {
  skip_if_no_ggir()
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  # GGIR is handed the column subset its own call site makes; this port folds that in
  sub <- P$ts[, c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection",
                  "selfreported", "angle")]
  g <- tut_ggir(sub, P$LEVELS, desiredtz = E$desiredtz, timewindow = "WW",
                Lnames = P$Lnames, filename = E$filename, ID = P$ID)
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW")
  expect_identical(mine, g$mdat)
  expect_identical(g$objects, c("desiredtz", "filename", "Lnames", "mdat"))
  # the csv has 13 columns and the RData 14: timestamp is re-appended after the csv is written
  expect_identical(ncol(g$csv), 13L)
  expect_identical(ncol(g$mdat), 14L)
  expect_identical(names(g$csv), setdiff(names(mine), "timestamp"))
})

test_that("P6g save_ms5raw_without_invalid drops MOS2 to 46130 rows, identically", {
  skip_if_no_ggir()
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  sub <- P$ts[, c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection",
                  "selfreported", "angle")]
  g <- tut_ggir(sub, P$LEVELS, desiredtz = E$desiredtz, timewindow = "WW",
                Lnames = P$Lnames, ID = P$ID,
                params_output = list(save_ms5raw_without_invalid = TRUE))
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW",
                                  save_ms5raw_without_invalid = TRUE)
  expect_identical(nrow(mine), 46130L)
  expect_identical(mine, g$mdat)
  expect_identical(sort(unique(mine$window)), c(1, 2, 3))
  # the dropped rows are the window 0 rows, so the row names are no longer 1:n
  expect_identical(rownames(mine)[1], "7690")
})

test_that("the data_cleaning_file drop is identical to GGIR's", {
  skip_if_no_ggir()
  P <- tut_case("MOS2", tut_ref_meta("MOS2"))
  E <- tut_stored(P$metadir)
  dcf <- data.frame(ID = P$ID, day_part5 = 2, stringsAsFactors = FALSE)
  sub <- P$ts[, c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection",
                  "selfreported", "angle")]
  g <- tut_ggir(sub, P$LEVELS, desiredtz = E$desiredtz, timewindow = "WW",
                Lnames = P$Lnames, ID = P$ID, DaCleanFile = dcf,
                params_output = list(save_ms5raw_without_invalid = TRUE))
  mine <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                  Lnames = P$Lnames, timewindow = "WW", ID = P$ID,
                                  DaCleanFile = dcf, save_ms5raw_without_invalid = TRUE)
  expect_identical(mine, g$mdat)
  expect_identical(nrow(mine), 29422L)
  expect_identical(sort(unique(mine$window)), c(1, 3))
  # an ID that is not in the file changes nothing
  mine2 <- .raw.timeuse.timeseries(ts = P$ts, LEVELS = P$LEVELS, desiredtz = E$desiredtz,
                                   Lnames = P$Lnames, timewindow = "WW", ID = "somebody",
                                   DaCleanFile = dcf, save_ms5raw_without_invalid = TRUE)
  expect_identical(nrow(mine2), 46130L)
})

test_that("the edge cases of the invalid-percentage loop are identical to GGIR", {
  skip_if_no_ggir()
  L <- tut_levels()
  # a gap between two windows gets its own percentage, not the 100 sentinel
  a <- tut_synth()
  a$window[289:864] <- 1
  a$window[1153:1728] <- 2
  g <- tut_ggir(a, L)
  mine <- .raw.timeuse.timeseries(a, L, "UTC", timewindow = "MM")
  expect_identical(mine, g$mdat)
  expect_identical(unique(mine$invalid_fullwindow[865:1152]), 0)   # the gap, not 100
  expect_identical(unique(mine$invalid_fullwindow[1:288]), 100)    # the leading run
  expect_identical(unique(mine$invalid_fullwindow[1729:2880]), 100) # the trailing run
  expect_identical(unique(mine$invalid_fullwindow[1153:1728]), 17.53)
  expect_identical(unique(mine$invalid_sleepperiod[1153:1728]), 35.07)
  expect_identical(unique(mine$invalid_wakinghours[1153:1728]), 0)
  # a window with no waking epoch at all: mean() of nothing is NaN, and it stays NaN
  b <- tut_synth()
  b$diur <- 1
  b$window[289:864] <- 1
  gb <- tut_ggir(b, L)
  mb <- .raw.timeuse.timeseries(b, L, "UTC", timewindow = "MM")
  expect_identical(mb, gb$mdat)
  expect_identical(sum(is.nan(mb$invalid_wakinghours)), 576L)
  expect_true(all(is.nan(mb$invalid_wakinghours[289:864])))
  expect_false(any(is.na(mb$invalid_wakinghours) & !is.nan(mb$invalid_wakinghours)))
  # a NaN survives the optional row drop, because which() drops the NA that NaN > x gives
  mb2 <- .raw.timeuse.timeseries(b, L, "UTC", timewindow = "MM",
                                 save_ms5raw_without_invalid = TRUE)
  expect_identical(nrow(mb2), 576L)
  expect_identical(unique(mb2$window), 1)
})

test_that("the ISO 8601 character branch and the merge order are identical to GGIR", {
  skip_if_no_ggir()
  L <- tut_levels()
  iso <- tut_synth()
  iso$window[1:2880] <- 1
  iso$time <- .raw.posix.to.iso8601(iso$time, "UTC")
  expect_false(grepl(" ", iso$time[1]))      # the branch test is a grep for a space
  g <- tut_ggir(iso, L)
  mine <- .raw.timeuse.timeseries(iso, L, "UTC", timewindow = "MM")
  expect_identical(mine, g$mdat)
  expect_s3_class(mine$timestamp, "POSIXct")
  # the merge is on timenum, not on row position, so a shuffled series comes back sorted
  o <- sample.int(2880)
  sh <- iso[o, ]
  gs <- tut_ggir(sh, L[o])
  ms <- .raw.timeuse.timeseries(sh, L[o], "UTC", timewindow = "MM")
  expect_identical(ms, gs$mdat)
  expect_identical(ms$timenum, mine$timenum)
  expect_false(is.unsorted(ms$timenum))
})

test_that("the optional channels keep GGIR's column order and rounding", {
  skip_if_no_ggir()
  L <- tut_levels()
  x <- tut_synth()
  x$window[1:2880] <- 1
  x$lightpeak <- seq(0.4, 999.6, length.out = 2880)
  x$selfreported <- factor(rep(NA_character_, 2880))
  x$temperature <- seq(20.001, 29.999, length.out = 2880)
  x$step_count <- 1:2880
  x$marker <- 0
  order_ggir <- c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection",
                  "lightpeak", "selfreported", "temperature", "step_count", "marker")
  g <- tut_ggir(x[, order_ggir], L)
  mine <- .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM")
  expect_identical(mine, g$mdat)
  expect_identical(names(mine),
                   c("timenum", "ACC", "SleepPeriodTime", "invalidepoch", "guider",
                     "window", "sibdetection", "lightpeak", "selfreported", "temperature",
                     "step_count", "marker", "class_id", "invalid_fullwindow",
                     "invalid_sleepperiod", "invalid_wakinghours", "timestamp"))
  expect_identical(mine$lightpeak, round(x$lightpeak))
  expect_identical(mine$temperature, round(x$temperature, 2))
  expect_identical(mine$ACC, round(x$ACC, 3))
})

test_that("the guider table is GGIR's, plus the one code 3.3-9 added", {
  skip_if_no_ggir()
  L <- tut_levels()
  labs <- c("sleeplog", "HDCZA", "HDCZA+invalid", "HorAngle", "HorAngle+invalid", "NotWorn",
            "NotWorn+invalid", "setwindow", "L512", "markerbutton", "HLRB", "MotionWare",
            "LowAcc", "", "part3_estimate", "nosleeplog_accnotworn")
  x <- tut_synth()
  x$window[1:2880] <- 1
  x$guider <- rep(labs, length.out = 2880)
  g <- tut_ggir(x, L)
  mine <- .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM")
  map <- unique(data.frame(label = x$guider, mine = mine$guider, ggir = g$mdat$guider,
                           stringsAsFactors = FALSE))
  rownames(map) <- NULL
  expect_identical(map$mine, c(1, 2, 2, 5, 5, 6, 6, 3, 4, 7, 8, 9, 10, 0, 0, 0))
  # every label except LowAcc agrees with the installed GGIR 3.3.6
  same <- map$label != "LowAcc"
  expect_identical(map$mine[same], map$ggir[same])
  expect_identical(tut_diffcols(mine, g$mdat), "guider")
  # LowAcc is the whole 3.3.6 to 3.3-9 delta: 3.3.6 falls through to 0, 3.3-9 maps it to 10
  expect_identical(map$ggir[map$label == "LowAcc"], 0)
  expect_identical(map$mine[map$label == "LowAcc"], 10)
  expect_false(any(grepl("LowAcc", deparse(GGIR:::g.part5.savetimeseries))))
})

test_that("the installed 3.3.6 body and the 3.3-9 clone differ only in the guider table", {
  skip_if_no_ggir()
  src <- tut_clone()
  if (src == "" || !file.exists(file.path(src, "g.part5.savetimeseries.R"))) {
    skip("no GGIR 3.3-9 clone at CANHRACTI_GGIR_SRC or beside the reference")
  }
  e <- new.env()
  sys.source(file.path(src, "g.part5.savetimeseries.R"), envir = e)
  a <- deparse(e$g.part5.savetimeseries)
  b <- deparse(GGIR:::g.part5.savetimeseries)
  d <- setdiff(a, b)
  expect_true(all(grepl("MotionWare|guidernumbers", d)))
  expect_true(any(grepl("LowAcc", d)))
  expect_identical(length(setdiff(b, a)), length(d))
})

test_that("a recording with no window at all returns NULL, as GGIR writes no file", {
  L <- tut_levels()
  x <- tut_synth()                       # window is 0 everywhere
  expect_null(.raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM"))
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_null(tut_ggir(x, L))          # GGIR wrote neither the csv nor the RData
  }
})

test_that("the single-window branch is unreachable, because the diff is zero padded", {
  # exhaustive over every window pattern of length 1 to 8 built from 0, 1 and 2
  seen <- integer(0)
  for (n in 1:8) {
    grid <- expand.grid(rep(list(c(0, 1, 2)), n))
    for (i in seq_len(nrow(grid))) {
      w <- as.numeric(grid[i, ])
      seen <- c(seen, length(which(abs(diff(c(0, w, 0))) > 0)))
    }
  }
  # the padded vector starts and ends at 0, so a non-zero epoch forces at least two
  # transitions; odd counts occur when window 1 steps straight to window 2
  expect_false(any(seen == 1))
  expect_true(all(seen == 0 | seen >= 2))
  expect_identical(sort(unique(seen)), c(0L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L))
})

test_that("P6h the two inclusion criteria are read as percentages here, minutes there", {
  L <- rep(0:5, length.out = 2000)
  x <- data.frame(time = as.POSIXct("2025-03-01 00:00:00", tz = "UTC") + (0:1999) * 5,
                  ACC = 1, diur = 0, nonwear = 0, guider = "HDCZA", window = 0,
                  sibdetection = 0, stringsAsFactors = FALSE)
  x$window[1001:2000] <- 1
  x$nonwear[1001:1400] <- 1            # 40 percent of the waking hours of window 1
  m <- .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM")
  expect_identical(unique(m$invalid_wakinghours[1001:2000]), 40)
  # a value in (1, 25] is hours and becomes (value / 24) * 100 percent, so 16 allows 33.33
  # percent of non-wear and the 40 percent window is dropped
  keep16 <- .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM",
                                    includedaycrit.part5 = 16,
                                    save_ms5raw_without_invalid = TRUE)
  expect_identical(nrow(keep16), 0L)     # every row cut, and GGIR writes the empty frame
  expect_equal(100 - (16 / 24) * 100, 33.33333, tolerance = 1e-5)
  if (requireNamespace("GGIR", quietly = TRUE)) {
    g16 <- tut_ggir(x, L, includedaycrit.part5 = 16,
                    params_output = list(save_ms5raw_without_invalid = TRUE))
    expect_identical(keep16, g16$mdat)
  }
  # a value in [0, 1] is a fraction and becomes value * 100, so 0.5 gives 50 percent
  keep05 <- .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM",
                                    includedaycrit.part5 = 0.5,
                                    save_ms5raw_without_invalid = TRUE)
  expect_identical(nrow(keep05), 1000L)
  # a value above 25 has no branch at all and is used as it stands, so 26 gives 74 percent
  keep26 <- .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM",
                                    includedaycrit.part5 = 26,
                                    save_ms5raw_without_invalid = TRUE)
  expect_identical(nrow(keep26), 1000L)
  # the sleep-period criterion is read by the same two rules
  y <- x
  y$diur[1001:1500] <- 1
  y$nonwear[1001:1400] <- 1            # 80 percent of the sleep period of window 1
  m2 <- .raw.timeuse.timeseries(y, L, "UTC", timewindow = "MM")
  expect_identical(unique(m2$invalid_sleepperiod[1001:2000]), 80)
  expect_identical(nrow(.raw.timeuse.timeseries(y, L, "UTC", timewindow = "MM",
                                                includenightcrit.part5 = 6,
                                                save_ms5raw_without_invalid = TRUE)), 0L)
  expect_identical(nrow(.raw.timeuse.timeseries(y, L, "UTC", timewindow = "MM",
                                                includenightcrit.part5 = 0,
                                                save_ms5raw_without_invalid = TRUE)), 1000L)
  # the report reads the same parameter as minutes, value * 60
  if (requireNamespace("GGIR", quietly = TRUE)) {
    body_txt <- paste(deparse(GGIR:::g.report.part5, width.cutoff = 500L), collapse = " ")
    expect_true(grepl("includeday_absolute = params_cleaning[[\"includedaycrit.part5\"]] * 60",
                      body_txt, fixed = TRUE))
    expect_true(grepl("includenight_absolute = params_cleaning[[\"includenightcrit.part5\"]] * 60",
                      body_txt, fixed = TRUE))
  }
})

test_that("the guider helper maps every label and returns a double", {
  labs <- c("sleeplog", "HDCZA", "HDCZA+invalid", "HorAngle", "HorAngle+invalid",
            "NotWorn", "NotWorn+invalid", "setwindow", "L512", "markerbutton", "HLRB",
            "MotionWare", "LowAcc")
  expect_identical(.raw.timeuse.guider.code(labs),
                   c(1, 2, 2, 5, 5, 6, 6, 3, 4, 7, 8, 9, 10))
  expect_type(.raw.timeuse.guider.code("HDCZA"), "double")
  # everything the table does not list becomes 0, with no warning
  expect_identical(.raw.timeuse.guider.code(c("", "unknown", "part3_estimate",
                                              "nosleeplog_accnotworn", "sleeplog+invalid")),
                   c(0, 0, 0, 0, 0))
  # the "+invalid" suffix is accepted by the ifelse chain and not by the six-name loop
  expect_identical(.raw.timeuse.guider.code("setwindow+invalid"), 0)
  expect_identical(.raw.timeuse.guider.code("NotWorn+invalid"), 6)
  # a factor works, because == compares the labels
  expect_identical(.raw.timeuse.guider.code(factor(c("sleeplog", "LowAcc"))), c(1, 10))
  expect_identical(.raw.timeuse.guider.code(character(0)), logical(0))
})

test_that("the column selector reproduces GGIR's order and its greps", {
  ts <- data.frame(angle = 1, marker = 1, time = 1, step_count = 1, ACC = 1,
                   temperature = 1, diur = 1, selfreported = 1, nonwear = 1, guider = 1,
                   diaryImputationCode = 1, window = 1, lightpeak = 1, sibdetection = 1,
                   nap1_nonwear2 = 1)
  expect_identical(.raw.timeuse.timeseries.columns(ts),
                   c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection",
                     "nap1_nonwear2", "lightpeak", "selfreported", "angle", "temperature",
                     "step_count", "diaryImputationCode", "marker"))
  # GGIR greps for "angle", so a hip series contributes all four angle columns
  hip <- data.frame(time = 1, ACC = 1, diur = 1, nonwear = 1, guider = 1, window = 1,
                    sibdetection = 1, anglex = 1, angley = 1, anglez = 1, angle = 1)
  expect_identical(.raw.timeuse.timeseries.columns(hip),
                   c("time", "ACC", "diur", "nonwear", "guider", "window", "sibdetection",
                     "anglex", "angley", "anglez", "angle"))
  # a series with only the seven fixed columns passes through
  bare <- hip[, 1:7]
  expect_identical(.raw.timeuse.timeseries.columns(bare), names(bare))
  expect_error(.raw.timeuse.timeseries.columns(bare[, -3]), "no column named diur")
  expect_error(.raw.timeuse.timeseries.columns(bare[, c(-1, -6)]),
               "no column named time, window")
})

test_that("the three SPEC additions raise where GGIR would build a wrong frame", {
  L <- tut_levels()
  x <- tut_synth()
  x$window[289:864] <- 1
  # a repeated timestamp turns the merge into a cartesian product
  dup <- x
  dup$time[2] <- dup$time[1]
  expect_error(.raw.timeuse.timeseries(dup, L, "UTC", timewindow = "MM"),
               "repeats the timestamp of epoch 2")
  if (requireNamespace("GGIR", quietly = TRUE)) {
    g <- tut_ggir(dup, L)                     # GGIR silently lengthens the series
    expect_true(nrow(g$mdat) > nrow(dup))
    expect_identical(nrow(g$mdat), 2882L)
  }
  # a LEVELS of the wrong length would be recycled by data.frame()
  expect_error(.raw.timeuse.timeseries(x, L[1:1440], "UTC", timewindow = "MM"),
               "LEVELS has 1440 values and the time series has 2880 epochs")
  # the last-window suppression needs one timewindow to know which rule to apply
  expect_error(.raw.timeuse.timeseries(x, L, "UTC", timewindow = NULL,
                                       require_complete_lastnight_part5 = TRUE),
               "needs one timewindow")
  expect_error(.raw.timeuse.timeseries(x, L, "UTC", timewindow = c("MM", "WW"),
                                       require_complete_lastnight_part5 = TRUE),
               "needs one timewindow")
  expect_error(.raw.timeuse.timeseries(x, L, "UTC", timewindow = "ZZ",
                                       require_complete_lastnight_part5 = TRUE),
               "needs one timewindow")
  # with the switch off the timewindow is never read, exactly as in GGIR
  expect_s3_class(.raw.timeuse.timeseries(x, L, "UTC", timewindow = NULL), "data.frame")
  # the class names, when given, must cover LEVELS
  expect_error(.raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM",
                                       Lnames = paste0("L", 1:3)),
               "not a 0-based index into the 3 behaviour class names")
  expect_error(.raw.timeuse.timeseries(x[0, ], L, "UTC"), "no rows")
  expect_error(.raw.timeuse.timeseries(as.matrix(1:10), L, "UTC"), "must be a data.frame")
})

test_that("settings come from an explicit argument first and a params object second", {
  L <- tut_levels()
  x <- tut_synth()
  x$window[289:864] <- 1
  p <- raw.params()
  p$save_ms5raw_without_invalid <- TRUE
  p$desiredtz <- "UTC"
  # the flat parameter object is the fallback
  from_params <- .raw.timeuse.timeseries(x, L, params = p, timewindow = "MM")
  expect_identical(nrow(from_params), 576L)
  expect_identical(attr(from_params$timestamp, "tzone"), "UTC")
  from_arg <- .raw.timeuse.timeseries(x, L, params = p, timewindow = "MM",
                                      save_ms5raw_without_invalid = FALSE)
  expect_identical(nrow(from_arg), 2880L)
  expect_identical(from_arg, .raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM"))
  # with neither, GGIR's own defaults apply
  expect_identical(nrow(.raw.timeuse.timeseries(x, L, "UTC", timewindow = "MM")), 2880L)
  expect_identical(.raw.timeuse.timeseries.setting(NULL, p, "include_no_such_member", 7), 7)
  expect_identical(.raw.timeuse.timeseries.setting(1, p, "desiredtz", 7), 1)
})

test_that("the last-window suppression fires on the two clock rules and not otherwise", {
  mk <- function(endclock, wlast, n = 17280) {
    t0 <- as.POSIXct(endclock, tz = "UTC") - (n - 1) * 5
    d <- data.frame(time = t0 + (0:(n - 1)) * 5, ACC = 1, diur = 0, nonwear = 0,
                    guider = "HDCZA", window = 0, sibdetection = 0,
                    stringsAsFactors = FALSE)
    d$window[1:8640] <- 1
    d$window[8641:(8640 + wlast)] <- 2
    d
  }
  lev <- function(d) rep(0:5, length.out = nrow(d))
  L <- rep(0:5, length.out = 17280)      # one day at 5 s
  # WW: the last hour is 7, below 15, and the trailing run of window 0 is 2 hours, below 21
  a <- mk("2025-03-02 07:59:55", 6480)
  ma <- .raw.timeuse.timeseries(a, L, "UTC", timewindow = "WW",
                                require_complete_lastnight_part5 = TRUE)
  expect_identical(sort(unique(ma$window)), c(0, 1))
  expect_identical(as.integer(sum(ma$window == 0)), 8640L)
  # MM: the last hour must be below 9 and the trailing run below 15 hours
  mb <- .raw.timeuse.timeseries(a, L, "UTC", timewindow = "MM",
                                require_complete_lastnight_part5 = TRUE)
  expect_identical(sort(unique(mb$window)), c(0, 1))
  # an afternoon end: 16 is above both 9 and 15, so neither rule fires
  b <- mk("2025-03-02 16:59:55", 6480)
  for (tw in c("MM", "WW", "OO")) {
    m <- .raw.timeuse.timeseries(b, L, "UTC", timewindow = tw,
                                 require_complete_lastnight_part5 = TRUE)
    expect_identical(sort(unique(m$window)), c(0, 1, 2))
  }
  # a morning end with a 35.92 hour trailing run, above both bounds, so neither rule fires
  cc <- mk("2025-03-03 07:59:55", 60, n = 34560)
  Lc <- lev(cc)
  expect_identical(which(rev(cc$window) != 0)[1], 25861L)
  expect_equal((25861 * 5) / 3600, 35.92, tolerance = 1e-2)
  mc <- .raw.timeuse.timeseries(cc, Lc, "UTC", timewindow = "WW",
                                require_complete_lastnight_part5 = TRUE)
  expect_identical(sort(unique(mc$window)), c(0, 1, 2))
  # OO takes the MM rule, not the WW one
  md <- .raw.timeuse.timeseries(cc, Lc, "UTC", timewindow = "OO",
                                require_complete_lastnight_part5 = TRUE)
  expect_identical(sort(unique(md$window)), c(0, 1, 2))
  if (requireNamespace("GGIR", quietly = TRUE)) {
    po <- list(require_complete_lastnight_part5 = TRUE)
    expect_identical(ma, tut_ggir(a, L, timewindow = "WW", params_output = po)$mdat)
    expect_identical(mb, tut_ggir(a, L, timewindow = "MM", params_output = po)$mdat)
    expect_identical(mc, tut_ggir(cc, Lc, timewindow = "WW", params_output = po)$mdat)
    expect_identical(md, tut_ggir(cc, Lc, timewindow = "OO", params_output = po)$mdat)
  }
})
