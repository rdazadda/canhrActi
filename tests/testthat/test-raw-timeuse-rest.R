# Parity tests for R/raw_timeuse_rest.R against GGIR part 5's g.part5.analyseRest and
# markerButtonForRest. The rest analysis is off in both reference runs, so the values are
# checked against a live GGIR on the MOS2 series, the diary fixture and synthetic segments.
# Reference data are found through CANHRACTI_GGIR_REF, the part-5 fixtures through
# CANHRACTI_GGIR_P5FIX and the 3.3-9 clone through CANHRACTI_GGIR_SRC, both falling back to
# siblings of the reference folder; every block skips without them, and the live comparisons
# skip when GGIR is not installed.

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

tur_fixdir <- function(name) {
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
tur_clone <- function() {
  src <- Sys.getenv("CANHRACTI_GGIR_SRC", unset = "")
  if (src == "" && .ggir_ref != "") {
    src <- file.path(dirname(sub("/+$", "", normalizePath(.ggir_ref, winslash = "/",
                                                          mustWork = FALSE))),
                     "ggir-src", "GGIR", "R")
  }
  src
}

tur_loadenv <- function(path) {
  e <- new.env()
  load(path, envir = e)
  e
}

# The nap parameter set used everywhere below.
tur_ps <- function(...) {
  ps <- list(possible_nap_window = c(9, 18), possible_nap_dur = c(15, 240),
             possible_nap_gap = 0, possible_nap_edge_acc = Inf,
             nap_markerbutton_method = 0, nap_markerbutton_max_distance = 30)
  utils::modifyList(ps, list(...))
}

# One window of a recording, as the two implementations see it.
tur_series <- function(mdat, tz) {
  data.frame(time = as.POSIXct(mdat$timenum, origin = "1970-01-01", tz = tz),
             ACC = mdat$ACC, nonwear = mdat$invalidepoch, sibdetection = mdat$sibdetection,
             diur = mdat$SleepPeriodTime, window = mdat$window,
             selfreported = if ("selfreported" %in% colnames(mdat)) mdat$selfreported else NA,
             stringsAsFactors = FALSE)
}
tur_waking <- function(ts, w) {
  sse <- which(ts$window == w)
  ts[sse[ts$diur[sse] == 0], ]
}

# GGIR's answer as a named character vector of the 27 cells.
tur_ggir <- function(sibreport, ts, tz, ps) {
  res <- GGIR:::g.part5.analyseRest(sibreport = sibreport, dsummary = matrix("", 2, 500),
                                    ds_names = rep("", 500), fi = 1, di = 1,
                                    ts = ts, tz = tz, params_sleep = ps)
  out <- res$dsummary[1, 1:27]
  names(out) <- res$ds_names[1:27]
  list(row = out, ts = res$ts, fi = res$fi)
}

tur_mine <- function(sibreport, ts, tz, ps, ggir_exact = TRUE) {
  res <- .raw.timeuse.rest(sibreport = sibreport, row = list(), ts = ts, tz = tz,
                           params_sleep = ps, ggir_exact = ggir_exact)
  list(row = .raw.timeuse.row.chr(res$row), ts = res$ts, raw = res$row)
}

# Every contiguous run of sibdetection == 2, as clock times.
tur_napranges <- function(ts) {
  i <- which(ts$sibdetection == 2)
  if (length(i) == 0) return(data.frame(from = character(0), to = character(0),
                                        n = integer(0), stringsAsFactors = FALSE))
  st <- i[c(1, which(diff(i) != 1) + 1)]
  en <- i[c(which(diff(i) != 1), length(i))]
  data.frame(from = format(ts$time[st]), to = format(ts$time[en]), n = en - st + 1L,
             stringsAsFactors = FALSE)
}

# Report the first difference rather than just failing.
tur_expect_same <- function(mine, ggir) {
  bad <- which(mine != ggir)
  if (length(bad) > 0) {
    i <- bad[1]
    testthat::fail(sprintf("first difference at column %d (%s): canhrActi '%s', GGIR '%s'",
                           i, names(ggir)[i], mine[i], ggir[i]))
  }
  expect_identical(unname(mine), unname(ggir))
  expect_identical(names(mine), names(ggir))
}

# A flat synthetic day of waking 5 s epochs.
tur_synth_ts <- function(n = 17280, from = "2025-10-08 00:00:00", tz = "UTC", marker = NULL) {
  tm <- as.POSIXct(from, tz = tz) + (0:(n - 1)) * 5
  out <- data.frame(time = tm, ACC = 1, nonwear = 0, sibdetection = 0, diur = 0,
                    selfreported = NA, stringsAsFactors = FALSE)
  if (!is.null(marker)) {
    out$marker <- as.numeric(out$time %in% as.POSIXct(marker, tz = tz))
  }
  out
}

tur_synth_row <- function(type, s, e, dur, day = "2025-10-08", before = 0, after = 0) {
  data.frame(ID = "X", type = type, start = paste(day, s), end = paste(day, e),
             duration = dur, mean_acc_1min_before = before, mean_acc_1min_after = after,
             stringsAsFactors = FALSE)
}

test_that("the 27 rest column names are GGIR's, in GGIR's order", {
  nm <- .raw.timeuse.rest.names()
  expect_identical(length(nm), 27L)
  expect_identical(nm, c(
    "sibreport_n_items", "sibreport_n_items_day", "nbouts_day_denap", "nbouts_day_srnap",
    "nbouts_day_srnonw", "noverl_denap_srnap", "noverl_denap_srnonw", "noverl_srnap_denap",
    "noverl_srnonw_denap", "frag_mean_dur_denap_day", "dur_day_denap_min",
    "frag_mean_dur_srnap_day", "dur_day_srnap_min", "frag_mean_dur_srnonw_day",
    "dur_day_srnonw_min", "mdur_denap_overl_srnap", "tdur_denap_overl_srnap",
    "perc_denap_overl_srnap", "mdur_srnap_overl_denap", "tdur_srnap_overl_denap",
    "perc_srnap_overl_denap", "mdur_denap_overl_srnonw", "tdur_denap_overl_srnonw",
    "perc_denap_overl_srnonw", "mdur_srnonw_overl_denap", "tdur_srnonw_overl_denap",
    "perc_srnonw_overl_denap"))
  expect_false(any(duplicated(nm)))
})

test_that("the names are identical() to the ones GGIR writes at :141-154", {
  skip_if_no_ggir()
  g <- tur_ggir(NULL, tur_synth_ts(n = 10), "UTC", tur_ps())
  expect_identical(names(g$row), .raw.timeuse.rest.names())
})

test_that("the block consumes exactly 27 column slots in all three branches", {
  skip_if_no_ggir()
  ts <- tur_synth_ts()
  ps <- tur_ps()
  cases <- list(
    `no sibreport` = NULL,
    `one row only` = tur_synth_row("sib", "10:00:00", "10:30:00", 30),
    `no candidate` = rbind(tur_synth_row("sib", "02:00:00", "02:20:00", 20),
                           tur_synth_row("sib", "03:00:00", "03:20:00", 20)),
    `candidates`   = rbind(tur_synth_row("sib", "02:00:00", "02:20:00", 20),
                           tur_synth_row("sib", "13:00:00", "13:30:00", 30)))
  for (lab in names(cases)) {
    g <- tur_ggir(cases[[lab]], ts, "UTC", ps)
    m <- tur_mine(cases[[lab]], ts, "UTC", ps)
    expect_identical(length(m$row), 27L, info = lab)
    expect_identical(g$fi, 28, info = lab)  # fi 1 -> 28 whichever branch ran
    tur_expect_same(m$row, g$row)
  }
})

test_that("a NULL or one-row sibreport gives n_items 0 and 26 empty cells", {
  ts <- tur_synth_ts()
  for (sib in list(NULL, tur_synth_row("sib", "10:00:00", "10:30:00", 30))) {
    m <- tur_mine(sib, ts, "UTC", tur_ps())
    expect_identical(m$row[["sibreport_n_items"]], "0")
    expect_identical(unname(m$row[2:27]), rep("", 26))
    # the unwritten cells are real NA in the row, which the csv writes as ""
    expect_true(all(vapply(m$raw[2:27], function(x) is.na(x) && !is.nan(x), logical(1))))
    expect_identical(m$ts, ts) # nothing marked, series untouched
  }
})

test_that("the row entries are appended in order, after whatever the caller already wrote", {
  m <- .raw.timeuse.rest(sibreport = NULL, row = list(ID = "X", filename = "F"),
                         ts = tur_synth_ts(), tz = "UTC", params_sleep = tur_ps())
  expect_identical(names(m$row), c("ID", "filename", .raw.timeuse.rest.names()))
})

test_that("the 27 columns are ABSENT from the stored MOS2 output, not NA", {
  skip_if_no_ggir_ref()
  f <- ref_file("out/output_din/meta/ms5.out/MOS2E39230594.gt3x.RData")
  skip_if_not(file.exists(f))
  o <- tur_loadenv(f)$output
  expect_identical(dim(o), c(7L, 119L))
  expect_identical(sum(.raw.timeuse.rest.names() %in% colnames(o)), 0L)
})

test_that("MOS2: .raw.timeuse.rest is identical() to GGIR on all three WW windows", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  f <- ref_file("out/output_din/meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")
  skip_if_not(file.exists(f))
  csv <- ref_file("out/output_din/meta/ms5.outraw/sib.reports",
                  "sib_report_MOS2E39230594gt3x_T5A5.csv")
  skip_if_not(file.exists(csv))
  tz <- "America/Anchorage"
  ts <- tur_series(tur_loadenv(f)$mdat, tz)
  sib <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_identical(nrow(sib), 66L)
  ps <- tur_ps()
  marked <- integer(0)
  for (w in 1:3) {
    day <- tur_waking(ts, w)
    g <- tur_ggir(sib, day, tz, ps)
    m <- tur_mine(sib, day, tz, ps)
    tur_expect_same(m$row, g$row)
    expect_identical(m$ts, g$ts)
    marked <- c(marked, sum(m$ts$sibdetection == 2))
  }
  expect_identical(marked, c(712L, 962L, 0L))
})

test_that("MOS2: the numbers BUILD_SPEC_P5 3.4 quotes", {
  skip_if_no_ggir_ref()
  f <- ref_file("out/output_din/meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")
  skip_if_not(file.exists(f))
  csv <- ref_file("out/output_din/meta/ms5.outraw/sib.reports",
                  "sib_report_MOS2E39230594gt3x_T5A5.csv")
  skip_if_not(file.exists(csv))
  tz <- "America/Anchorage"
  ts <- tur_series(tur_loadenv(f)$mdat, tz)
  sib <- utils::read.csv(csv, stringsAsFactors = FALSE)
  v <- lapply(1:3, function(w) tur_mine(sib, tur_waking(ts, w), tz, tur_ps())$row)
  # sibreport_n_items counts the candidates over the whole recording, so it repeats
  expect_identical(vapply(v, function(x) x[["sibreport_n_items"]], ""), c("6", "6", "6"))
  expect_identical(vapply(v, function(x) x[["sibreport_n_items_day"]], ""), c("3", "3", "0"))
  expect_identical(v[[1]][["nbouts_day_denap"]], "3")
  expect_identical(v[[2]][["nbouts_day_denap"]], "3")
  expect_identical(v[[1]][["frag_mean_dur_denap_day"]], "19.7777777777778")
  expect_identical(v[[1]][["dur_day_denap_min"]], "59.3333333333333")
  expect_identical(v[[2]][["frag_mean_dur_denap_day"]], "26.7222222222222")
  expect_identical(v[[2]][["dur_day_denap_min"]], "80.1666666666666")
  # window 3 has no qualifying bout: n_items and n_items_day are written, the other 25 are not
  expect_identical(unname(v[[3]][3:27]), rep("", 25))
})

test_that("MOS2: the six candidates over the whole recording are the ones the spec lists", {
  skip_if_no_ggir_ref()
  f <- ref_file("out/output_din/meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")
  skip_if_not(file.exists(f))
  csv <- ref_file("out/output_din/meta/ms5.outraw/sib.reports",
                  "sib_report_MOS2E39230594gt3x_T5A5.csv")
  skip_if_not(file.exists(csv))
  tz <- "America/Anchorage"
  ts <- tur_series(tur_loadenv(f)$mdat, tz)
  sib <- utils::read.csv(csv, stringsAsFactors = FALSE)
  # every waking epoch of the recording as one window, so all six candidates are inside it
  day <- ts[ts$diur == 0, ]
  m <- tur_mine(sib, day, tz, tur_ps())
  expect_identical(m$row[["sibreport_n_items"]], "6")
  expect_identical(m$row[["sibreport_n_items_day"]], "6")
  expect_identical(m$row[["nbouts_day_denap"]], "6")
  expect_identical(m$row[["frag_mean_dur_denap_day"]], "23.25")
  expect_identical(m$row[["dur_day_denap_min"]], "139.5")
  rng <- tur_napranges(m$ts)
  expect_identical(rng$from, c("2025-10-08 09:03:10", "2025-10-08 11:17:50",
                               "2025-10-08 15:51:05", "2025-10-09 10:51:45",
                               "2025-10-09 11:33:10", "2025-10-09 12:17:30"))
  expect_identical(rng$to, c("2025-10-08 09:21:30", "2025-10-08 11:34:35",
                             "2025-10-08 16:15:05", "2025-10-09 11:08:45",
                             "2025-10-09 11:57:40", "2025-10-09 12:55:55"))
  # the three rejected bouts are in the report and are not marked
  rejected <- c("2025-10-08 08:40:35", "2025-10-08 20:31:30", "2025-10-10 08:35:50")
  expect_true(all(rejected %in% sib$start))
  expect_false(any(rejected %in% rng$from))
})

test_that("MOS2: ggir_exact = FALSE changes nothing without a diary", {
  skip_if_no_ggir_ref()
  f <- ref_file("out/output_din/meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")
  skip_if_not(file.exists(f))
  csv <- ref_file("out/output_din/meta/ms5.outraw/sib.reports",
                  "sib_report_MOS2E39230594gt3x_T5A5.csv")
  skip_if_not(file.exists(csv))
  tz <- "America/Anchorage"
  ts <- tur_series(tur_loadenv(f)$mdat, tz)
  sib <- utils::read.csv(csv, stringsAsFactors = FALSE)
  for (w in 1:3) {
    day <- tur_waking(ts, w)
    a <- tur_mine(sib, day, tz, tur_ps(), ggir_exact = TRUE)
    b <- tur_mine(sib, day, tz, tur_ps(), ggir_exact = FALSE)
    expect_identical(a$row, b$row)
    expect_identical(a$ts, b$ts)
  }
})

test_that("F-P5-2 diary: identical() to GGIR on every window", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  dir <- tur_fixdir("diary_spt")
  f <- file.path(dir, "output_din/meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")
  csv <- file.path(dir, "output_din/meta/ms5.outraw/sib.reports",
                   "sib_report_MOS2E39230594gt3x_T5A5.csv")
  skip_if_not(file.exists(f) && file.exists(csv))
  tz <- "America/Anchorage"
  ts <- tur_series(tur_loadenv(f)$mdat, tz)
  sib <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_identical(nrow(sib), 63L)                    # 56 sib rows and 7 sleeplog rows
  expect_identical(unname(table(sib$type)[["sleeplog"]]), 7L)
  expect_true(all(which(sib$type == "sleeplog") > max(which(sib$type == "sib"))))
  for (w in 1:3) {
    day <- tur_waking(ts, w)
    g <- tur_ggir(sib, day, tz, tur_ps())
    m <- tur_mine(sib, day, tz, tur_ps())
    tur_expect_same(m$row, g$row)
    expect_identical(m$ts, g$ts)
  }
})

test_that("F-P5-2 diary: defect 1 inflates sibreport_n_items and the fix deflates it", {
  skip_if_no_ggir_ref()
  dir <- tur_fixdir("diary_spt")
  f <- file.path(dir, "output_din/meta/ms5.outraw/40_100_400/MOS2E39230594_T5A5.RData")
  csv <- file.path(dir, "output_din/meta/ms5.outraw/sib.reports",
                   "sib_report_MOS2E39230594gt3x_T5A5.csv")
  skip_if_not(file.exists(f) && file.exists(csv))
  tz <- "America/Anchorage"
  ts <- tur_series(tur_loadenv(f)$mdat, tz)
  sib <- utils::read.csv(csv, stringsAsFactors = FALSE)
  exact <- lapply(1:3, function(w) tur_mine(sib, tur_waking(ts, w), tz, tur_ps()))
  fixed <- lapply(1:3, function(w) tur_mine(sib, tur_waking(ts, w), tz, tur_ps(),
                                            ggir_exact = FALSE))
  # GGIR counts 3 qualifying bouts plus all 7 whole-recording sleeplog rows, on every window
  expect_identical(vapply(exact, function(x) x$row[["sibreport_n_items"]], ""),
                   c("10", "10", "10"))
  # the fix counts the 3 bouts plus this window's own sleeplog segments, 0, 2 and 1 of them
  expect_identical(vapply(fixed, function(x) x$row[["sibreport_n_items"]], ""),
                   c("3", "5", "4"))
  waking_sleeplog <- vapply(1:3, function(w) {
    d <- tur_waking(ts, w)
    sum(as.character(d$selfreported) %in% c("sleeplog", "sleeplog+bedlog"))
  }, integer(1))
  expect_identical(waking_sleeplog, c(0L, 658L, 20L))
  # every other cell agrees: the sleeplog rows sort last, so the bout indices survive
  for (i in 1:3) {
    expect_identical(exact[[i]]$row[-1], fixed[[i]]$row[-1])
    expect_identical(exact[[i]]$ts, fixed[[i]]$ts)
  }
  expect_identical(vapply(exact, function(x) x$row[["sibreport_n_items_day"]], ""),
                   c("3", "2", "1"))
  expect_identical(exact[[1]]$row[["dur_day_denap_min"]], "59.3333333333333")
  expect_identical(sum(exact[[1]]$ts$sibdetection == 2), 712L)
})

# Defect 1: longboutsi is applied to a frame it was not computed on.

test_that("defect 1 selects the wrong bouts when a sleeplog row comes first", {
  skip_if_no_ggir()
  sib <- rbind(tur_synth_row("sleeplog", "01:00:00", "05:00:00", 240),
               tur_synth_row("sib", "10:00:00", "10:10:00", 10),
               tur_synth_row("sib", "11:00:00", "11:20:00", 20),
               tur_synth_row("sib", "13:00:00", "13:30:00", 30))
  ts <- tur_synth_ts()
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  m <- tur_mine(sib, ts, "UTC", tur_ps())
  tur_expect_same(m$row, g$row)
  expect_identical(m$ts, g$ts)
  # longboutsi is 1, 3, 4 (the sleeplog row and the 20 and 30 minute bouts); the rebuilt frame
  # is bout10, bout20, bout30, so rows 1, 3 and 4 select bout10, bout30 and nothing
  rng <- tur_napranges(m$ts)
  expect_identical(rng$from, c("2025-10-08 10:00:00", "2025-10-08 13:00:00"))
  expect_identical(rng$to, c("2025-10-08 10:10:00", "2025-10-08 13:30:00"))
  expect_identical(m$row[["sibreport_n_items"]], "3")
  expect_identical(m$row[["nbouts_day_denap"]], "2")
  expect_identical(m$row[["frag_mean_dur_denap_day"]], "20")  # (10 + 30) / 2
  expect_identical(m$row[["dur_day_denap_min"]], "40")
  # with ggir_exact = FALSE the 20 and 30 minute bouts are the ones marked
  f <- tur_mine(sib, ts, "UTC", tur_ps(), ggir_exact = FALSE)
  rngf <- tur_napranges(f$ts)
  expect_identical(rngf$from, c("2025-10-08 11:00:00", "2025-10-08 13:00:00"))
  expect_identical(rngf$to, c("2025-10-08 11:20:00", "2025-10-08 13:30:00"))
  expect_identical(f$row[["sibreport_n_items"]], "2")
  expect_identical(f$row[["frag_mean_dur_denap_day"]], "25")  # (20 + 30) / 2
  expect_identical(f$row[["dur_day_denap_min"]], "50")
})

test_that("defect 1 is inert when the sib report carries no sleeplog row", {
  skip_if_no_ggir()
  sib <- rbind(tur_synth_row("sib", "10:00:00", "10:10:00", 10),
               tur_synth_row("sib", "11:00:00", "11:20:00", 20),
               tur_synth_row("sib", "13:00:00", "13:30:00", 30))
  ts <- tur_synth_ts()
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  a <- tur_mine(sib, ts, "UTC", tur_ps())
  b <- tur_mine(sib, ts, "UTC", tur_ps(), ggir_exact = FALSE)
  tur_expect_same(a$row, g$row)
  expect_identical(a$row, b$row)
  expect_identical(a$ts, b$ts)
  rng <- tur_napranges(a$ts)
  expect_identical(rng$from, c("2025-10-08 11:00:00", "2025-10-08 13:00:00"))
})

tur_two_sleeplog_runs <- function(tz) {
  ts <- tur_synth_ts(tz = tz)
  # two separate waking sleeplog runs, 08:00 to 08:30 and 20:00 to 20:15
  r1 <- which(ts$time >= as.POSIXct("2025-10-08 08:00:00", tz = tz) &
                ts$time <= as.POSIXct("2025-10-08 08:30:00", tz = tz))
  r2 <- which(ts$time >= as.POSIXct("2025-10-08 20:00:00", tz = tz) &
                ts$time <= as.POSIXct("2025-10-08 20:15:00", tz = tz))
  ts$selfreported <- NA_character_
  ts$selfreported[r1] <- "sleeplog"
  ts$selfreported[r2] <- "sleeplog+bedlog"
  ts$selfreported <- as.factor(ts$selfreported)
  ts
}

test_that("the appended sleeplog segments come from ts$selfreported, one row per run", {
  skip_if_no_ggir()
  # the appended rows are re-parsed in the session timezone, so the session is put on the
  # data's clock
  old <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "UTC")
  on.exit(if (is.na(old)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old), add = TRUE)
  ts <- tur_two_sleeplog_runs("UTC")
  sib <- rbind(tur_synth_row("sib", "13:00:00", "13:30:00", 30),
               tur_synth_row("sleeplog", "08:00:00", "08:30:00", 30))
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  m <- tur_mine(sib, ts, "UTC", tur_ps())
  tur_expect_same(m$row, g$row)
  expect_identical(m$ts, g$ts)
  # 1 bout plus 1 sleeplog row qualify; the frame is rebuilt as bout, run1, run2 and rows 1
  # and 2 are taken, so the second run is lost
  expect_identical(m$row[["sibreport_n_items"]], "2")
  expect_identical(m$row[["sibreport_n_items_day"]], "2")
  f <- tur_mine(sib, ts, "UTC", tur_ps(), ggir_exact = FALSE)
  expect_identical(f$row[["sibreport_n_items"]], "3")   # the bout and both runs
  expect_identical(f$row[["sibreport_n_items_day"]], "3")
})

test_that("the appended rows are re-parsed in the session timezone, not in tz", {
  skip_if_no_ggir()
  # the appended sleeplog rows go in as format(ts$time[...]), a clock string with no offset,
  # and the POSIXct column re-parses them in the session timezone, not tz; eight hours west
  # of the data the 20:00 run lands at 04:00 the next morning, outside the window
  old <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "America/Anchorage")
  on.exit(if (is.na(old)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old), add = TRUE)
  ts <- tur_two_sleeplog_runs("UTC")
  sib <- rbind(tur_synth_row("sib", "13:00:00", "13:30:00", 30),
               tur_synth_row("sleeplog", "08:00:00", "08:30:00", 30))
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  f <- tur_mine(sib, ts, "UTC", tur_ps(), ggir_exact = FALSE)
  m <- tur_mine(sib, ts, "UTC", tur_ps())
  tur_expect_same(m$row, g$row)   # GGIR does the same
  expect_identical(f$row[["sibreport_n_items"]], "3")
  expect_identical(f$row[["sibreport_n_items_day"]], "2") # the 20:00 run has left the window
})

# Defect 2: summarise_overlap tests length(xi) twice.

test_that("defect 2 writes NaN, 0, NaN where it should write 0, 0, 0", {
  skip_if_no_ggir()
  # a two-epoch bout inside a four-hour reported nap: the bout is 100 percent inside the nap
  # and the nap rounds to 0 percent inside the bout, so xi is non-empty and yi is empty
  sib <- rbind(tur_synth_row("nap", "09:00:00", "13:00:00", 240),
               tur_synth_row("sib", "10:00:00", "10:00:05", 2 / 12))
  ts <- tur_synth_ts()
  ps <- tur_ps(possible_nap_dur = c(0.05, 240))
  g <- tur_ggir(sib, ts, "UTC", ps)
  m <- tur_mine(sib, ts, "UTC", ps)
  tur_expect_same(m$row, g$row)
  expect_identical(m$row[["noverl_denap_srnap"]], "1")   # xi
  expect_identical(m$row[["noverl_srnap_denap"]], "0")   # yi is empty
  expect_identical(m$row[["mdur_denap_overl_srnap"]], "0.166666666666667")
  expect_identical(m$row[["perc_denap_overl_srnap"]], "100")
  expect_identical(m$row[["mdur_srnap_overl_denap"]], "NaN")
  expect_identical(m$row[["tdur_srnap_overl_denap"]], "0")
  expect_identical(m$row[["perc_srnap_overl_denap"]], "NaN")
  # the NaN is a literal string in the exported table, never an NA
  expect_true(is.nan(m$raw[["mdur_srnap_overl_denap"]]))
  f <- tur_mine(sib, ts, "UTC", ps, ggir_exact = FALSE)
  expect_identical(f$row[["mdur_srnap_overl_denap"]], "0")
  expect_identical(f$row[["perc_srnap_overl_denap"]], "0")
  expect_identical(f$row[-which(names(f$row) %in% c("mdur_srnap_overl_denap",
                                                    "perc_srnap_overl_denap"))],
                   m$row[-which(names(m$row) %in% c("mdur_srnap_overl_denap",
                                                    "perc_srnap_overl_denap"))])
})

test_that(".raw.timeuse.rest.overlap on its own reproduces both arms", {
  srep <- data.frame(duration = c(10, 20), SIBoverlapNap = c(50, 0), NapOverlapSIB = c(0, 0),
                     stringsAsFactors = FALSE)
  a <- .raw.timeuse.rest.overlap(list(), srep, "SIBoverlapNap", "NapOverlapSIB",
                                 xi = 1L, yi = integer(0), name = "srnap")
  expect_identical(names(a), c("mdur_denap_overl_srnap", "tdur_denap_overl_srnap",
                               "perc_denap_overl_srnap", "mdur_srnap_overl_denap",
                               "tdur_srnap_overl_denap", "perc_srnap_overl_denap"))
  expect_identical(a$mdur_denap_overl_srnap, 10)
  expect_identical(a$perc_denap_overl_srnap, 50)   # duration weighted over one row
  expect_true(is.nan(a$mdur_srnap_overl_denap))
  expect_identical(a$tdur_srnap_overl_denap, 0)
  b <- .raw.timeuse.rest.overlap(list(), srep, "SIBoverlapNap", "NapOverlapSIB",
                                 xi = 1L, yi = integer(0), name = "srnap", ggir_exact = FALSE)
  expect_identical(b$mdur_srnap_overl_denap, 0)
  expect_identical(b$perc_srnap_overl_denap, 0)
  # an empty xi takes the zero arm in both modes
  z <- .raw.timeuse.rest.overlap(list(), srep, "SIBoverlapNap", "NapOverlapSIB",
                                 xi = integer(0), yi = 1L, name = "srnonw")
  expect_identical(unname(unlist(z)), c(0, 0, 0, 0, 0, 0))
  # the duration weighting is sum(overlap * duration) / sum(duration), not a plain mean
  srep2 <- data.frame(duration = c(10, 30), SIBoverlapNap = c(100, 20),
                      NapOverlapSIB = c(0, 0), stringsAsFactors = FALSE)
  w <- .raw.timeuse.rest.overlap(list(), srep2, "SIBoverlapNap", "NapOverlapSIB",
                                 xi = 1:2, yi = integer(0), name = "srnap")
  expect_identical(w$perc_denap_overl_srnap, (100 * 10 + 20 * 30) / 40)
})

# Defect 3: the re-merged gap is recomputed in seconds.

test_that("defect 3 suppresses the second merge of a chain of three bouts", {
  skip_if_no_ggir()
  sib <- rbind(tur_synth_row("sib", "10:00:00", "10:20:00", 20),
               tur_synth_row("sib", "10:22:00", "10:40:00", 18),
               tur_synth_row("sib", "10:43:00", "11:00:00", 17))
  ts <- tur_synth_ts()
  ps <- tur_ps(possible_nap_gap = 5)  # minutes, despite the Rd saying seconds
  g <- tur_ggir(sib, ts, "UTC", ps)
  m <- tur_mine(sib, ts, "UTC", ps)
  tur_expect_same(m$row, g$row)
  expect_identical(m$ts, g$ts)
  # the 2 minute gap merges, the recomputed gap is then 180 (seconds) and 180 < 5 is FALSE,
  # so the 3 minute gap does not merge: two bouts, 40.0833 and 17.0833 minutes
  expect_identical(m$row[["nbouts_day_denap"]], "2")
  expect_identical(m$row[["dur_day_denap_min"]], "57.1666666666667")
  expect_identical(m$row[["frag_mean_dur_denap_day"]], "28.5833333333333")
  rng <- tur_napranges(m$ts)
  expect_identical(rng$n, c(481L, 205L))
  f <- tur_mine(sib, ts, "UTC", ps, ggir_exact = FALSE)
  expect_identical(f$row[["nbouts_day_denap"]], "1")
  expect_identical(f$row[["dur_day_denap_min"]], "60.0833333333333")
  expect_identical(tur_napranges(f$ts)$n, 721L)
})

test_that("the gap merge adds one epoch to the duration of every row, of every type", {
  skip_if_no_ggir()
  sib <- rbind(tur_synth_row("nonwear", "16:00:00", "17:00:00", 60),
               tur_synth_row("sib", "10:00:00", "10:20:00", 20),
               tur_synth_row("sib", "13:00:00", "13:30:00", 30))
  ts <- tur_synth_ts()
  ps <- tur_ps(possible_nap_gap = 5)
  g <- tur_ggir(sib, ts, "UTC", ps)
  m <- tur_mine(sib, ts, "UTC", ps)
  tur_expect_same(m$row, g$row)
  # nothing merges here (the gaps are hours), but every duration gains one 5 s epoch
  expect_identical(m$row[["dur_day_srnonw_min"]], "60.0833333333333")
  expect_identical(m$row[["dur_day_denap_min"]], "50.1666666666667") # 20.0833 + 30.0833
  # with possible_nap_gap 0 the whole block is skipped and the reported durations stand
  m0 <- tur_mine(sib, ts, "UTC", tur_ps())
  expect_identical(m0$row[["dur_day_srnonw_min"]], "60")
  expect_identical(m0$row[["dur_day_denap_min"]], "50")
})

test_that(".raw.markerbutton.rest is identical() to GGIR for methods 0 to 3", {
  skip_if_no_ggir()
  tz <- "UTC"
  ts <- tur_synth_ts(marker = paste("2025-10-08", c("09:58:00", "10:31:00", "12:05:00")))
  expect_identical(sum(ts$marker), 3)
  sib <- data.frame(
    ID = "X", type = "sib",
    start = as.POSIXct(paste("2025-10-08", c("10:00:00", "12:00:00", "15:00:00")), tz = tz),
    end = as.POSIXct(paste("2025-10-08", c("10:30:00", "12:20:00", "15:40:00")), tz = tz),
    duration = c(30, 20, 40), mean_acc_1min_before = 0, mean_acc_1min_after = 0,
    stringsAsFactors = FALSE)
  for (meth in 0:3) {
    ps <- list(nap_markerbutton_method = meth, nap_markerbutton_max_distance = 30)
    g <- GGIR:::markerButtonForRest(sib, ps, ts)
    m <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = meth,
                                nap_markerbutton_max_distance = 30)
    expect_identical(m, g, info = paste("method", meth))
  }
  # what the four methods do here
  m0 <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 0)
  expect_identical(m0$ignore, c(FALSE, FALSE, FALSE))
  expect_identical(m0$start, sib$start)                       # no re-timing
  m1 <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 1)
  expect_identical(format(m1$start[1]), "2025-10-08 09:58:00")  # timing copied from the press
  expect_identical(format(m1$end[1]), "2025-10-08 10:31:00")
  expect_identical(m1$ignore, c(FALSE, FALSE, FALSE))
  m2 <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 2)
  expect_identical(m2$ignore, c(FALSE, FALSE, TRUE))            # press required, not re-timed
  expect_identical(m2$start, sib$start)
  expect_identical(m2$start_to_marker, c(1, -5, 175))           # minutes from the bout edge
  m3 <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 3)
  expect_identical(m3$ignore, c(FALSE, TRUE, TRUE))
})

test_that("with no marker channel method 0 is inert and method 2 ignores every bout", {
  skip_if_no_ggir()
  tz <- "UTC"
  ts <- tur_synth_ts()
  sib <- data.frame(
    ID = "X", type = "sib",
    start = as.POSIXct(paste("2025-10-08", c("10:00:00", "13:00:00")), tz = tz),
    end = as.POSIXct(paste("2025-10-08", c("10:30:00", "13:40:00")), tz = tz),
    duration = c(30, 40), mean_acc_1min_before = 0, mean_acc_1min_after = 0,
    stringsAsFactors = FALSE)
  for (meth in c(0, 2)) {
    g <- GGIR:::markerButtonForRest(sib, list(nap_markerbutton_method = meth,
                                              nap_markerbutton_max_distance = 30), ts)
    m <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = meth,
                                nap_markerbutton_max_distance = 30)
    expect_identical(m, g)
  }
  expect_identical(.raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 0)$ignore,
                   c(FALSE, FALSE))
  expect_identical(.raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 2)$ignore,
                   c(TRUE, TRUE))
  ps <- tur_ps(nap_markerbutton_method = 2)
  g <- tur_ggir(sib, ts, tz, ps)
  m <- tur_mine(sib, ts, tz, ps)
  tur_expect_same(m$row, g$row)
  expect_identical(m$row[["sibreport_n_items"]], "0")
  expect_identical(sum(m$ts$sibdetection == 2), 0L)
})

test_that("the marker pass de-duplicates on (start, end) even at method 0", {
  skip_if_no_ggir()
  tz <- "UTC"
  ts <- tur_synth_ts()
  sib <- data.frame(
    ID = "X", type = "sib",
    start = as.POSIXct(paste("2025-10-08", c("10:00:00", "13:00:00", "10:00:00")), tz = tz),
    end = as.POSIXct(paste("2025-10-08", c("10:30:00", "13:40:00", "10:30:00")), tz = tz),
    duration = c(30, 40, 30), mean_acc_1min_before = 0, mean_acc_1min_after = 0,
    stringsAsFactors = FALSE)
  g <- GGIR:::markerButtonForRest(sib, list(nap_markerbutton_method = 0,
                                            nap_markerbutton_max_distance = 30), ts)
  m <- .raw.markerbutton.rest(sib, ts = ts, nap_markerbutton_method = 0)
  expect_identical(m, g)
  expect_identical(nrow(m), 2L)
})

test_that("the whole rest analysis is identical() to GGIR with the marker button on", {
  skip_if_no_ggir()
  tz <- "UTC"
  ts <- tur_synth_ts(marker = paste("2025-10-08", c("09:58:00", "10:31:00", "12:05:00")))
  sib <- data.frame(
    ID = "X", type = "sib",
    start = as.POSIXct(paste("2025-10-08", c("10:00:00", "12:00:00", "15:00:00")), tz = tz),
    end = as.POSIXct(paste("2025-10-08", c("10:30:00", "12:20:00", "15:40:00")), tz = tz),
    duration = c(30, 20, 40), mean_acc_1min_before = 0, mean_acc_1min_after = 0,
    stringsAsFactors = FALSE)
  for (meth in 0:3) {
    ps <- tur_ps(nap_markerbutton_method = meth)
    g <- tur_ggir(sib, ts, tz, ps)
    m <- tur_mine(sib, ts, tz, ps)
    tur_expect_same(m$row, g$row)
    expect_identical(m$ts, g$ts)
  }
  # method 3 keeps only the bout with a press on both sides, re-timed to 09:58 to 10:31
  m3 <- tur_mine(sib, ts, tz, tur_ps(nap_markerbutton_method = 3))
  expect_identical(m3$row[["sibreport_n_items"]], "1")
  expect_identical(m3$row[["dur_day_denap_min"]], "30")
  expect_identical(sum(m3$ts$sibdetection == 2), 397L)  # 09:58:00 to 10:31:00 inclusive
})

test_that("the durations are minutes and the overlap percentages are percent", {
  skip_if_no_ggir()
  # a one-hour bout half covered by a two-hour reported non-wear block
  sib <- rbind(tur_synth_row("nonwear", "10:30:00", "12:30:00", 120),
               tur_synth_row("sib", "10:00:00", "11:00:00", 60 + 1 / 12))
  ts <- tur_synth_ts()
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  m <- tur_mine(sib, ts, "UTC", tur_ps())
  tur_expect_same(m$row, g$row)
  r <- m$raw
  # counts
  for (nm in c("sibreport_n_items", "sibreport_n_items_day", "nbouts_day_denap",
               "nbouts_day_srnap", "nbouts_day_srnonw", "noverl_denap_srnap",
               "noverl_denap_srnonw", "noverl_srnap_denap", "noverl_srnonw_denap")) {
    expect_identical(r[[nm]], as.integer(r[[nm]]), info = nm)
    expect_gte(r[[nm]], 0L)
  }
  expect_identical(r$nbouts_day_denap, 1L)
  expect_identical(r$nbouts_day_srnonw, 1L)
  expect_identical(r$noverl_denap_srnonw, 1L)
  expect_identical(r$noverl_srnonw_denap, 1L)
  # minutes, and the sib duration is the sib report's own, one epoch longer than the clock span
  expect_equal(r$dur_day_denap_min, 60 + 1 / 12, tolerance = 1e-12)
  expect_identical(r$frag_mean_dur_denap_day, r$dur_day_denap_min)
  expect_identical(r$dur_day_srnonw_min, 120)
  expect_identical(r$mdur_denap_overl_srnonw, r$dur_day_denap_min)
  expect_identical(r$mdur_srnonw_overl_denap, 120)
  # percent: 30 of the bout's 60 minutes, and 30 of the block's 120
  expect_identical(r$perc_denap_overl_srnonw, 50)
  expect_identical(r$perc_srnonw_overl_denap, 25)
  for (nm in c("perc_denap_overl_srnap", "perc_srnap_overl_denap",
               "perc_denap_overl_srnonw", "perc_srnonw_overl_denap")) {
    expect_gte(r[[nm]], 0)
    expect_lte(r[[nm]], 100)
  }
  # the self-reported nap duration is recomputed as a plain clock span, the non-wear one is not
  expect_identical(r$dur_day_srnap_min, 0)
})

test_that("a bout of n epochs marks n+1 epochs, inclusive at both ends", {
  skip_if_no_ggir()
  sib <- rbind(tur_synth_row("sib", "02:00:00", "02:20:00", 20),   # outside the clock window
               tur_synth_row("sib", "13:00:00", "13:30:00", 30))
  ts <- tur_synth_ts()
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  m <- tur_mine(sib, ts, "UTC", tur_ps())
  tur_expect_same(m$row, g$row)
  expect_identical(m$ts, g$ts)
  expect_identical(sum(m$ts$sibdetection == 2), 361L)  # 30 min at 5 s is 360 epochs, plus 1
  expect_identical(m$row[["dur_day_denap_min"]], "30") # the duration column is untouched
})

test_that("the hard coded 10 percent non-wear rule drops the bout and its row", {
  skip_if_no_ggir()
  decoy <- tur_synth_row("sib", "02:00:00", "02:20:00", 20)
  bout <- tur_synth_row("sib", "13:00:00", "13:30:00", 30)
  i <- which(tur_synth_ts()$time >= as.POSIXct("2025-10-08 13:00:00", tz = "UTC") &
               tur_synth_ts()$time <= as.POSIXct("2025-10-08 13:30:00", tz = "UTC"))
  expect_identical(length(i), 361L)
  over <- tur_synth_ts(); over$nonwear[i[1:37]] <- 1   # 37/361 = 0.10249, not below 0.1
  under <- tur_synth_ts(); under$nonwear[i[1:36]] <- 1 # 36/361 = 0.09972, below 0.1
  for (nm in c("over", "under")) {
    ts <- get(nm)
    g <- tur_ggir(rbind(decoy, bout), ts, "UTC", tur_ps())
    m <- tur_mine(rbind(decoy, bout), ts, "UTC", tur_ps())
    tur_expect_same(m$row, g$row)
    expect_identical(m$ts, g$ts)
  }
  o <- tur_mine(rbind(decoy, bout), over, "UTC", tur_ps())
  u <- tur_mine(rbind(decoy, bout), under, "UTC", tur_ps())
  expect_identical(o$row[["sibreport_n_items"]], "1")     # the candidate still counts
  expect_identical(o$row[["sibreport_n_items_day"]], "0") # but the row is deleted
  expect_identical(sum(o$ts$sibdetection == 2), 0L)
  expect_identical(u$row[["sibreport_n_items_day"]], "1")
  expect_identical(sum(u$ts$sibdetection == 2), 361L)
})

test_that("a bout straddling the window edge is dropped, not truncated", {
  skip_if_no_ggir()
  decoy <- tur_synth_row("sib", "02:00:00", "02:20:00", 20)
  half <- tur_synth_ts(n = 12 * 720)  # 00:00:00 to 11:59:55
  for (sp in list(c("11:40:00", "12:10:00"), c("10:40:00", "11:10:00"))) {
    sib <- rbind(decoy, tur_synth_row("sib", sp[1], sp[2], 30))
    g <- tur_ggir(sib, half, "UTC", tur_ps())
    m <- tur_mine(sib, half, "UTC", tur_ps())
    tur_expect_same(m$row, g$row)
    expect_identical(m$ts, g$ts)
  }
  out <- tur_mine(rbind(decoy, tur_synth_row("sib", "11:40:00", "12:10:00", 30)), half,
                  "UTC", tur_ps())
  inn <- tur_mine(rbind(decoy, tur_synth_row("sib", "10:40:00", "11:10:00", 30)), half,
                  "UTC", tur_ps())
  expect_identical(out$row[["sibreport_n_items"]], "1")
  expect_identical(out$row[["sibreport_n_items_day"]], "0")
  expect_identical(sum(out$ts$sibdetection == 2), 0L)
  expect_identical(inn$row[["sibreport_n_items_day"]], "1")
  expect_identical(sum(inn$ts$sibdetection == 2), 361L)
})

test_that("every non-sib row qualifies on a duration of one minute alone", {
  skip_if_no_ggir()
  sib <- rbind(tur_synth_row("nap", "03:00:00", "03:01:00", 1),      # 03:00 is outside 9 to 18
               tur_synth_row("nonwear", "04:00:00", "04:00:30", 0.5), # under a minute
               tur_synth_row("sib", "03:30:00", "04:30:00", 60))      # outside the clock window
  ts <- tur_synth_ts()
  g <- tur_ggir(sib, ts, "UTC", tur_ps())
  m <- tur_mine(sib, ts, "UTC", tur_ps())
  tur_expect_same(m$row, g$row)
  expect_identical(m$row[["sibreport_n_items"]], "1")   # only the one minute nap
  expect_identical(m$row[["nbouts_day_srnap"]], "1")
  expect_identical(m$row[["nbouts_day_srnonw"]], "0")
  expect_identical(m$row[["nbouts_day_denap"]], "0")
  expect_identical(m$row[["dur_day_srnap_min"]], "1")   # recomputed as the clock span
})

test_that("the clock window is whole hours and endHour gains 24 across midnight", {
  skip_if_no_ggir()
  ts <- tur_synth_ts(n = 2 * 17280)
  decoy <- tur_synth_row("sib", "02:00:00", "02:20:00", 20)
  cases <- list(
    `ends at 17:59:55` = tur_synth_row("sib", "17:30:00", "17:59:55", 30),
    `ends at 18:00:05` = tur_synth_row("sib", "17:30:00", "18:00:05", 30),
    `starts at 08:59` = tur_synth_row("sib", "08:59:00", "09:29:00", 30),
    `starts at 09:00` = tur_synth_row("sib", "09:00:00", "09:30:00", 30))
  got <- character(0)
  for (lab in names(cases)) {
    sib <- rbind(decoy, cases[[lab]])
    g <- tur_ggir(sib, ts, "UTC", tur_ps())
    m <- tur_mine(sib, ts, "UTC", tur_ps())
    tur_expect_same(m$row, g$row)
    got <- c(got, m$row[["sibreport_n_items"]])
  }
  expect_identical(got, c("1", "0", "0", "1"))
  # a bout crossing midnight gets endHour + 24; the decoy at 02:00 qualifies under a 0 to 24
  # window, so the count to watch is 1 against 2
  cross <- rbind(decoy, tur_synth_row("sib", "23:50:00", "00:20:00", 30))
  cross$end[2] <- "2025-10-09 00:20:00"
  for (win in list(c(0, 24), c(0, 25))) {
    g <- tur_ggir(cross, ts, "UTC", tur_ps(possible_nap_window = win))
    m <- tur_mine(cross, ts, "UTC", tur_ps(possible_nap_window = win))
    tur_expect_same(m$row, g$row)
  }
  # endHour 0 becomes 24: 24 < 24 fails and 24 < 25 passes
  expect_identical(tur_mine(cross, ts, "UTC",
                            tur_ps(possible_nap_window = c(0, 24)))$row[["sibreport_n_items"]],
                   "1")
  expect_identical(tur_mine(cross, ts, "UTC",
                            tur_ps(possible_nap_window = c(0, 25)))$row[["sibreport_n_items"]],
                   "2")
})

test_that("possible_nap_edge_acc reads the larger of the two one-minute means", {
  skip_if_no_ggir()
  ts <- tur_synth_ts()
  sib <- rbind(tur_synth_row("sib", "10:00:00", "10:30:00", 30, before = 10, after = 40),
               tur_synth_row("sib", "13:00:00", "13:30:00", 30, before = 5, after = 6))
  for (edge in c(Inf, 40, 39, 6, 5)) {
    ps <- tur_ps(possible_nap_edge_acc = edge)
    g <- tur_ggir(sib, ts, "UTC", ps)
    m <- tur_mine(sib, ts, "UTC", ps)
    tur_expect_same(m$row, g$row)
  }
  n <- vapply(c(Inf, 40, 39, 6, 5), function(e) {
    tur_mine(sib, ts, "UTC", tur_ps(possible_nap_edge_acc = e))$row[["sibreport_n_items"]]
  }, "")
  expect_identical(n, c("2", "2", "1", "1", "0"))
  # a sib report with no mean_acc_1min columns gets acc_edge 0, so the test always passes
  bare <- sib[, c("ID", "type", "start", "end", "duration")]
  g <- tur_ggir(bare, ts, "UTC", tur_ps(possible_nap_edge_acc = 0))
  m <- tur_mine(bare, ts, "UTC", tur_ps(possible_nap_edge_acc = 0))
  tur_expect_same(m$row, g$row)
  expect_identical(m$row[["sibreport_n_items"]], "2")
})

# One synthetic day through the whole segment analyser, GGIR's and ours.
tur_segment_case <- function(napwin, napdur) {
  tz <- "UTC"; ws3 <- 5
  n <- 2 * 24 * 720
  tm <- as.POSIXct("2025-03-04 00:00:00", tz = tz) + (0:(n - 1)) * ws3
  hr <- as.numeric(format(tm, "%H"))
  diur <- as.numeric(hr >= 23 | hr < 7)
  set.seed(42)
  ACC <- round(abs(stats::rnorm(n, 30, 30)), 4)
  sibdet <- as.numeric(diur == 1)
  inday <- function(a, b) which(tm >= as.POSIXct(paste("2025-03-04", a), tz = tz) &
                                  tm <= as.POSIXct(paste("2025-03-04", b), tz = tz))
  bouts <- c(inday("10:00:00", "10:40:00"), inday("14:00:00", "14:25:00"))
  sibdet[bouts] <- 1
  ACC[bouts] <- 2
  ts <- data.frame(time = tm, ACC = ACC, guider = "HDCZA", angle = 0, nonwear = 0,
                   diur = diur, sibdetection = sibdet, stringsAsFactors = FALSE)
  sib <- .raw.sib.report.acc(ts, ID = "X", epochlength = ws3, desiredtz = tz)
  sib$start <- as.POSIXct(sib$start, tz = tz)
  sib$end <- as.POSIXct(sib$end, tz = tz)
  pp <- list(boutcriter.in = 0.9, boutcriter.lig = 0.8, boutcriter.mvpa = 0.8,
             boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10), boutdur.lig = c(10, 5, 1))
  LL <- .raw.identify.levels(ts = ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = ws3,
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
  tp <- as.POSIXlt(ts$time)
  timeList <- list(ts = ts, sec = tp$sec, min = tp$min, hour = tp$hour, time_POSIX = ts$time,
                   epochSize = ws3)
  dd <- matrix("", 3, 500); dd[1, 1:2] <- c("IDx", "FNx")
  nn <- rep("", 500); nn[1:2] <- c("ID", "filename")
  g <- GGIR:::g.part5_analyseSegment(
    indexlog, timeList, levelList, segments, "MM", dd, nn,
    params_general = list(desiredtz = tz),
    params_output = list(do.sibreport = TRUE, storefolderstructure = FALSE),
    params_sleep = list(possible_nap_window = napwin, possible_nap_dur = napdur,
                        possible_nap_gap = 0, possible_nap_edge_acc = Inf,
                        nap_markerbutton_method = 0, nap_markerbutton_max_distance = 30,
                        nap_model = NULL),
    params_247 = list(iglevels = NULL, LUXthresholds = NULL, LUX_day_segments = NULL),
    params_phyact = pp, sumSleep = sumSleep, sibDef = "T5A5", fullFilename = "F",
    add_one_day_to_next_date = FALSE, lightpeak_available = FALSE,
    tail_expansion_log = NULL, foldernamei = "D", sibreport = sib)
  gn <- g$ds_names[seq_len(max(which(g$ds_names != "")))]
  gv <- g$dsummary[1, seq_along(gn)]
  names(gv) <- gn
  gv[is.na(gv)] <- ""
  m <- .raw.timeuse.segment(indexlog, timeList, levelList, segments, "MM",
                            dsummary = list(), sumSleep = sumSleep, sibDef = "T5A5",
                            desiredtz = tz, do.sibreport = TRUE,
                            boutdur.mvpa = pp$boutdur.mvpa, boutdur.in = pp$boutdur.in,
                            boutdur.lig = pp$boutdur.lig,
                            boutcriter.mvpa = pp$boutcriter.mvpa,
                            boutcriter.in = pp$boutcriter.in,
                            boutcriter.lig = pp$boutcriter.lig,
                            possible_nap_window = napwin, possible_nap_dur = napdur,
                            possible_nap_gap = 0, possible_nap_edge_acc = Inf,
                            nap_markerbutton_method = 0, nap_markerbutton_max_distance = 30,
                            sibreport = sib)
  list(ggir = gv, mine = c(ID = "IDx", filename = "FNx",
                           .raw.timeuse.row.chr(m$dsummary[[1]])))
}

test_that("with the nap parameters at their defaults the 27 columns are absent, not NA", {
  skip_if_no_ggir()
  off <- tur_segment_case(NULL, NULL)
  tur_expect_same(off$mine, off$ggir)
  expect_identical(length(off$mine), 118L)
  expect_false(any(.raw.timeuse.rest.names() %in% names(off$mine)))
  expect_false(any(grepl("denap|srnap|srnonw|day_nap", names(off$mine))))
})

test_that("with both nap parameters set the row gains exactly 27 + 3 columns", {
  skip_if_no_ggir()
  off <- tur_segment_case(NULL, NULL)
  on <- tur_segment_case(c(9, 18), c(15, 240))
  tur_expect_same(on$mine, on$ggir)
  expect_identical(length(on$mine), 148L)
  expect_identical(setdiff(names(on$mine), names(off$mine)),
                   c(.raw.timeuse.rest.names(),
                     "dur_day_nap_min", "ACC_day_nap_mg", "Nblocks_day_nap"))
  # the block sits straight after nonwear_perc_day_spt
  first <- which(names(on$mine) == "sibreport_n_items")
  expect_identical(names(on$mine)[first - 1], "nonwear_perc_day_spt")
  expect_identical(names(on$mine)[first:(first + 26)], .raw.timeuse.rest.names())
})

test_that("nap minutes leave day_IN_bts_30 for day_nap and the IN total does not move", {
  skip_if_no_ggir()
  off <- tur_segment_case(NULL, NULL)
  on <- tur_segment_case(c(9, 18), c(15, 240))
  num <- function(x, n) as.numeric(x[[n]])
  expect_identical(on$mine[["nbouts_day_denap"]], "2")
  expect_identical(on$mine[["dur_day_denap_min"]], "65.1666666666667")
  expect_identical(on$mine[["dur_day_nap_min"]], "65.1666666666667")
  expect_identical(on$mine[["Nblocks_day_nap"]], "2")
  expect_equal(num(off$mine, "dur_day_IN_bts_30_min") - num(on$mine, "dur_day_IN_bts_30_min"),
               num(on$mine, "dur_day_nap_min"), tolerance = 1e-12)
  # OLEVELS is never touched, so the two families of duration columns stop agreeing
  expect_identical(on$mine[["dur_day_total_IN_min"]], off$mine[["dur_day_total_IN_min"]])
  expect_identical(on$mine[["dur_day_min"]], off$mine[["dur_day_min"]])
})

test_that("explicit arguments and a params_sleep list give the same row", {
  sib <- rbind(tur_synth_row("sib", "02:00:00", "02:20:00", 20),
               tur_synth_row("sib", "13:00:00", "13:30:00", 30))
  ts <- tur_synth_ts()
  a <- .raw.timeuse.rest(sibreport = sib, row = list(), ts = ts, tz = "UTC",
                         params_sleep = tur_ps())
  b <- .raw.timeuse.rest(sibreport = sib, row = list(), ts = ts, tz = "UTC",
                         possible_nap_window = c(9, 18), possible_nap_dur = c(15, 240),
                         possible_nap_gap = 0, possible_nap_edge_acc = Inf,
                         nap_markerbutton_method = 0, nap_markerbutton_max_distance = 30)
  expect_identical(a$row, b$row)
  expect_identical(a$ts, b$ts)
  # an explicit argument beats the list
  c1 <- .raw.timeuse.rest(sibreport = sib, row = list(), ts = ts, tz = "UTC",
                          possible_nap_window = c(0, 3), params_sleep = tur_ps())
  expect_identical(c1$row[["sibreport_n_items"]], 1L)  # only the 02:00 bout now qualifies
  expect_identical(.raw.timeuse.rest.setting(NULL, list(a = 5), "a", 9), 5)
  expect_identical(.raw.timeuse.rest.setting(NULL, list(a = NULL), "a", 9), 9)
  expect_identical(.raw.timeuse.rest.setting(1, list(a = 5), "a", 9), 1)
  expect_identical(.raw.timeuse.rest.setting(NULL, NULL, "a"), NULL)
})

test_that("the SPEC guards fire instead of silently dropping every candidate", {
  ts <- tur_synth_ts()
  sib <- rbind(tur_synth_row("sib", "02:00:00", "02:20:00", 20),
               tur_synth_row("sib", "13:00:00", "13:30:00", 30))
  expect_error(.raw.timeuse.rest(sib, list(), ts, "UTC",
                                 params_sleep = tur_ps(possible_nap_window = 9)),
               "possible_nap_window")
  expect_error(.raw.timeuse.rest(sib, list(), ts, "UTC",
                                 params_sleep = tur_ps(possible_nap_dur = c(1, 2, 3))),
               "possible_nap_dur")
  expect_error(.raw.timeuse.rest(sib, list(), ts[, c("time", "ACC")], "UTC",
                                 params_sleep = tur_ps()),
               "nonwear, sibdetection")
  expect_error(.raw.timeuse.rest(sib[, c("ID", "type", "start")], list(), ts, "UTC",
                                 params_sleep = tur_ps()),
               "end, duration")
})

test_that("GGIR 3.3.6 and the 3.3-9 clone carry the same two function bodies", {
  skip_if_no_ggir()
  src <- tur_clone()
  if (src == "" || !dir.exists(src)) {
    skip("no GGIR 3.3-9 clone at CANHRACTI_GGIR_SRC or beside the reference")
  }
  clone_body <- function(file) {
    p <- parse(file.path(src, file))
    deparse(eval(p[[1]][[3]]))
  }
  expect_identical(deparse(GGIR:::g.part5.analyseRest), clone_body("g.part5.analyseRest.R"))
  expect_identical(deparse(GGIR:::markerButtonForRest), clone_body("markerButtonForRest.R"))
})
