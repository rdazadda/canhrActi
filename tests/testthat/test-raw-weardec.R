# Parity tests for R/raw_weardec.R against GGIR's g.weardec, g.impute, g.detecmidnight
# and the day slicing of g.analyse.perday. Reference data live in the folder named by
# CANHRACTI_GGIR_REF (the MOS2 and EE milestones and their part-2 csvs); tests skip
# when a file or GGIR is missing.

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
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
MOS2_DAYSUM <- function() ref_file("out", "output_din", "results", "part2_daysummary.csv")
MOS2_SUM <- function() ref_file("out", "output_din", "results", "part2_summary.csv")
EE_RDATA <- function() ref_file("timing_out", "output_timing", "meta", "basic", "meta_EE_left_29.5.2017-05-30.gt3x.RData")
EE_DAYSUM <- function() ref_file("timing_out", "output_timing", "results", "part2_daysummary.csv")
EE_SUM <- function() ref_file("timing_out", "output_timing", "results", "part2_summary.csv")

MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to
EE_TZ <- "Europe/Helsinki"

memo <- new.env()

stored_M <- function(which = c("mos2", "ee")) {
  which <- match.arg(which)
  key <- paste0("M_", which)
  if (is.null(memo[[key]])) {
    path <- if (which == "mos2") MOS2_RDATA() else EE_RDATA()
    skip_if_no_file(path)
    e <- new.env(); load(path, envir = e)
    memo[[key]] <- list(M = e$M, I = e$I, C = e$C, desiredtz_part1 = e$desiredtz_part1)
  }
  memo[[key]]
}
read_csv_ref <- function(path) {
  skip_if_no_file(path)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}
# GGIR's params_cleaning with load_params defaults and optional overrides
ggir_cleaning <- function(...) {
  pc <- GGIR::load_params()$params_cleaning
  ov <- list(...)
  for (nm in names(ov)) pc[nm] <- list(ov[[nm]])
  pc
}
ggir_impute <- function(M, I, desiredtz, dayborder = 0, ...) {
  GGIR::g.impute(M, I, params_cleaning = ggir_cleaning(...), desiredtz = desiredtz,
                 dayborder = dayborder, ID = "x")
}
# g.impute then g.analyse with load_params defaults plus cleaning overrides
ggir_analyse <- function(M, I, C, desiredtz, dayborder = 0, ...) {
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- desiredtz
  P$params_general[["dayborder"]] <- dayborder
  ov <- list(...)
  for (nm in names(ov)) P$params_cleaning[nm] <- list(ov[[nm]])
  IMP <- GGIR::g.impute(M, I, params_cleaning = P$params_cleaning, desiredtz = desiredtz,
                        dayborder = dayborder, ID = "x")
  SUM <- GGIR::g.analyse(I, C, M, IMP, params_247 = P$params_247, params_phyact = P$params_phyact,
                         params_general = P$params_general, params_cleaning = P$params_cleaning, ID = "x")
  list(IMP = IMP, daysummary = SUM$daysummary, summary = SUM$summary)
}
# GGIR's daysummary and summary are character matrices, so the numbers are compared as
# the strings GGIR holds; a value like 2339/720 does not survive as.numeric() exactly.
expect_daily_matches_ggir <- function(w, A, label) {
  ds <- A$daysummary
  chr <- function(x) as.character(x)
  expect_identical(w$rout, A$IMP$rout, label = label)
  expect_identical(w$r5long, A$IMP$r5long, label = label)
  expect_identical(nrow(w$daily), nrow(ds), label = label)
  expect_identical(chr(w$daily$n_hours), chr(ds[, "N hours"]), label = label)
  expect_identical(chr(w$daily$n_valid_hours), chr(ds[, "N valid hours"]), label = label)
  expect_identical(w$daily$weekday, chr(ds[, "weekday"]), label = label)
  expect_identical(w$daily$day, as.integer(ds[, "measurementday"]), label = label)
  expect_identical(w$daily$start_time, chr(ds[, "calendar_date"]), label = label)
  sm <- A$summary
  expect_identical(chr(w$wear_dur_def_proto_day), chr(sm[, "wear_dur_def_proto_day"]), label = label)
  expect_identical(chr(w$meas_dur_dys), chr(sm[, "meas_dur_dys"]), label = label)
  expect_identical(chr(w$meas_dur_def_proto_day), chr(sm[, "meas_dur_def_proto_day"]), label = label)
  expect_identical(chr(w$clipping_score), chr(sm[, "clipping_score"]), label = label)
  expect_identical(w$n_valid_weekdays, as.integer(sm[, "N valid weekdays (WD)"]), label = label)
  expect_identical(w$n_valid_weekend_days, as.integer(sm[, "N valid weekend days (WE)"]), label = label)
  invisible(NULL)
}
# a subset of a stored M (long epochs rows), with the matching short epochs
subset_M <- function(M, rows) {
  n <- M$windowsizes[2] / M$windowsizes[1]
  M$metalong <- M$metalong[rows, ]
  rownames(M$metalong) <- NULL
  short <- ((rows[1] - 1) * n + 1):(rows[length(rows)] * n)
  M$metashort <- M$metashort[short, ]
  rownames(M$metashort) <- NULL
  M
}
# a canhrActi_raw_meta built from a GGIR M list
meta_from_M <- function(M, desiredtz) {
  add_time <- function(tbl) {
    tbl$time <- as.numeric(.raw.iso8601.to.posix(tbl$timestamp, tz = desiredtz))
    tbl[c("timestamp", "time", setdiff(names(tbl), c("timestamp", "time")))]
  }
  structure(list(metashort = add_time(M$metashort), metalong = add_time(M$metalong),
                 qclog = M$QClog, wday = M$wday, wdayname = M$wdayname, windowsizes = M$windowsizes,
                 filecorrupt = M$filecorrupt, filetooshort = M$filetooshort,
                 nfilepagesskipped = M$NFilePagesSkipped, tail_expansion_log = NULL, nonwear = NULL,
                 settings = list(desiredtz = desiredtz),
                 file = list(path = NA_character_, filename = "from_M")),
            class = "canhrActi_raw_meta")
}

test_that("P10 MOS2: rout and r5long identical to GGIR g.impute, colSums 343/0/23/0/366", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  expect_equal(nrow(s$M$metalong), 660L)
  expect_equal(nrow(s$M$metashort), 118800L)
  w <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  expect_s3_class(w, "canhrActi_raw_wear")
  expect_identical(w$status, "ok")
  expect_identical(unname(colSums(w$rout)), c(343, 0, 23, 0, 366))
  expect_identical(names(w$rout), c("r1", "r2", "r3", "r4", "r5"))
  expect_identical(dim(w$r5long), c(118800L, 1L))
  expect_identical(sum(w$r5long == 1), 366L * 180L)
  IMP <- ggir_impute(s$M, s$I, MOS2_TZ)
  expect_identical(unname(colSums(IMP$rout)), c(343, 0, 23, 0, 366))
  expect_identical(w$rout, IMP$rout)
  expect_identical(w$r5long, IMP$r5long)
  expect_identical(w$LC, IMP$LC)
  expect_identical(w$LC2, IMP$LC2)
  expect_identical(w$LC2, 0L)
  expect_identical(w$nonwearHoursFiltered, IMP$nonwearHoursFiltered)
  expect_identical(w$nonwearEventsFiltered, IMP$nonwearEventsFiltered)
  expect_identical(w$hrs.del.start, IMP$hrs.del.start)
  expect_identical(w$hrs.del.end, IMP$hrs.del.end)
  expect_identical(w$maxdur, IMP$maxdur)
  # the g.weardec stage alone
  out <- .raw.weardec(s$M$metalong, 2, 900, nonWearEdgeCorrection = TRUE, desiredtz = MOS2_TZ)
  gg <- GGIR:::g.weardec(s$M$metalong, 2, 900, params_cleaning = ggir_cleaning(),
                         desiredtz = MOS2_TZ, qwindowImp = c())
  expect_identical(out, gg)
  expect_identical(sum(out$r1), 343)
  expect_identical(sum(out$r3), 23)
  # the 23 r3 epochs are all wear in r1
  expect_identical(sum(out$r1[out$r3 == 1]), 0)
  # r1 is nonwearscore >= 2: 343 with score 3, none with 2
  expect_identical(which(out$r1 == 1), which(s$M$metalong$nonwearscore >= 2))
  expect_identical(as.integer(table(s$M$metalong$nonwearscore)), c(308L, 9L, 343L))
  expect_identical(w$wear_dur_def_proto_day, 3.0625)
  expect_identical(w$wear_days, 3.0625)
  expect_identical(w$wear_dur_def_proto_day, (660 - 366) * 15 / 1440)
  expect_identical(w$meas_dur_dys, 6.875)
  expect_identical(w$meas_dur_def_proto_day, 6.875)
  expect_identical(w$clipping_score, 0)
  expect_identical(w$n_long, 660L)
  expect_identical(w$n_short, 118800L)
  expect_identical(w$n_expanded_long, 0L)
  expect_identical(w$settings$wearthreshold, 2)
  expect_identical(w$settings$desiredtz, MOS2_TZ)
  expect_length(w$messages, 0)
})

test_that("P10 MOS2: midnights identical to GGIR g.detecmidnight", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  time <- as.character(s$M$metalong$timestamp)
  dm <- .raw.detect.midnight(time, MOS2_TZ, 0)
  gg <- GGIR:::g.detecmidnight(time, MOS2_TZ, 0)
  expect_identical(dm, gg)
  expect_identical(dm$midnightsi, c(15L, 111L, 207L, 303L, 399L, 495L, 591L))
  expect_identical(dm$firstmidnight, "2025-10-08T00:00:00-0800")
  expect_identical(dm$lastmidnight, "2025-10-14T00:00:00-0800")
  expect_identical(dm$firstmidnighti, 15L)
  expect_identical(dm$lastmidnighti, 591L)
  dm4 <- .raw.detect.midnight(time, MOS2_TZ, 4)
  gg4 <- GGIR:::g.detecmidnight(time, MOS2_TZ, 4)
  expect_identical(dm4, gg4)
  expect_identical(dm4$midnightsi, c(31L, 127L, 223L, 319L, 415L, 511L, 607L))
  expect_identical(dm4$firstmidnight, "2025-10-08T04:00:00-0800")
  # the "date time" (space) branch, which returns named vectors in GGIR
  tsp <- format(.raw.iso8601.to.posix(time[1:200], tz = MOS2_TZ), "%Y-%m-%d %H:%M:%S")
  dms <- .raw.detect.midnight(tsp, MOS2_TZ, 0)
  ggs <- GGIR:::g.detecmidnight(tsp, MOS2_TZ, 0)
  expect_identical(dms, ggs)
  expect_identical(unname(dms$midnightsi), c(15L, 111L))
  expect_identical(names(dms$midnightsi), c("2025-10-08 00:00:00", "2025-10-09 00:00:00"))
  # no midnight: GGIR's dummy midnight
  dmn <- .raw.detect.midnight(time[1:13], MOS2_TZ, 0)
  ggn <- GGIR:::g.detecmidnight(time[1:13], MOS2_TZ, 0)
  expect_identical(dmn, ggn)
  expect_identical(dmn$midnightsi, 13L)
  expect_identical(dmn$firstmidnighti, 1)
  expect_identical(dmn$lastmidnighti, 13L)
  expect_identical(dmn$midnights, time[13])
})

test_that("P10 MOS2: daily hours equal the stored part2_daysummary.csv and part2_summary.csv", {
  skip_if_no_ggir_ref()
  s <- stored_M("mos2")
  w <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  d <- w$daily
  expect_identical(nrow(d), 8L)
  expect_identical(d$n_hours, c(3.5, 24, 24, 24, 24, 24, 24, 17.5))
  expect_identical(d$n_valid_hours, c(3.5, 22.75, 24, 23.25, 0, 0, 0, 0))
  expect_identical(d$valid, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(d$weekday, c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
                                "Monday", "Tuesday"))
  expect_identical(d$weekend, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(d$day, 1:8)
  expect_identical(as.character(d$date), c("2025-10-07", "2025-10-08", "2025-10-09", "2025-10-10",
                                           "2025-10-11", "2025-10-12", "2025-10-13", "2025-10-14"))
  expect_s3_class(d$date, "Date")
  expect_identical(d$start_time[1], "2025-10-07T20:30:00-0800")
  expect_identical(d$start_time[2], "2025-10-08T00:00:00-0800")
  expect_identical(d$start_time[8], "2025-10-14T00:00:00-0800")
  expect_identical(d$first_epoch, c(1, 2521, 19801, 37081, 54361, 71641, 88921, 106201))
  expect_identical(d$last_epoch, c(2520, 19800, 37080, 54360, 71640, 88920, 106200, 118800))
  expect_identical(sum(d$n_hours), 165)
  expect_identical(sum(d$n_hours) * 60 / 15, 660)
  expect_identical(w$n_valid_days, 3L)
  expect_identical(w$n_valid_weekdays, 3L)
  expect_identical(w$n_valid_weekend_days, 0L)
  ds <- read_csv_ref(MOS2_DAYSUM())
  expect_identical(nrow(ds), 8L)
  expect_identical(d$n_hours, as.numeric(ds[["N hours"]]))
  expect_identical(d$n_valid_hours, as.numeric(ds[["N valid hours"]]))
  expect_identical(d$weekday, ds$weekday)
  expect_identical(d$day, as.integer(ds$measurementday))
  expect_identical(as.character(d$date), ds$calendar_date)
  expect_identical(d$start_time, ds$start_time)
  sm <- read_csv_ref(MOS2_SUM())
  expect_identical(nrow(sm), 1L)
  expect_identical(round(w$wear_dur_def_proto_day, 3), as.numeric(sm$wear_dur_def_proto_day))
  expect_identical(as.numeric(sm$wear_dur_def_proto_day), 3.062)
  expect_identical(w$meas_dur_dys, as.numeric(sm$meas_dur_dys))
  expect_identical(w$meas_dur_def_proto_day, as.numeric(sm$meas_dur_def_proto_day))
  expect_identical(w$clipping_score, as.numeric(sm$clipping_score))
  expect_identical(w$n_valid_weekdays, as.integer(sm[["N valid weekdays (WD)"]]))
  expect_identical(w$n_valid_weekend_days, as.integer(sm[["N valid weekend days (WE)"]]))
  expect_identical(as.integer(sm[["N valid weekdays (WD)"]]), 3L)
  expect_identical(as.integer(sm[["N valid weekend days (WE)"]]), 0L)
  expect_identical(as.numeric(sm$meas_dur_dys), 6.875)
  # the day slices alone
  dm <- w$midnights
  sl <- .raw.day.slices(as.character(s$M$metalong$timestamp), 118800L, dm, ws3 = 5, ws2 = 900)
  expect_identical(attr(sl, "ndays"), 8)
  expect_identical(attr(sl, "nfulldays"), 6)
  expect_identical(attr(sl, "startatmidnight"), 0)
  expect_identical(attr(sl, "endatmidnight"), 0)
  expect_identical(sl$first_epoch, d$first_epoch)
  expect_identical(sl$last_epoch, d$last_epoch)
})

test_that("P10 MOS2 through raw.getmeta: the canhrActi_raw_meta path gives the same decision", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  skip_if_no_file(MOS2())
  s <- stored_M("mos2")
  info <- raw.inspect(MOS2(), desiredtz = MOS2_TZ)
  meta <- raw.getmeta(info, calibration = s$C)
  expect_s3_class(meta, "canhrActi_raw_meta")
  expect_identical(as.ggir.M(meta)$metalong, s$M$metalong)
  w_meta <- raw.wear.decision(meta)      # params NULL: desiredtz comes from meta$settings
  w_M <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  expect_identical(w_meta$settings$desiredtz, MOS2_TZ)
  # the default params desiredtz "" differs from the recorded one, so a note is made
  expect_true(any(grepl("desiredtz taken from the epoch tables", w_meta$messages)))
  expect_identical(w_meta$rout, w_M$rout)
  expect_identical(w_meta$r5long, w_M$r5long)
  expect_identical(w_meta$daily, w_M$daily)
  expect_identical(w_meta$midnights, w_M$midnights)
  expect_identical(w_meta$wear_dur_def_proto_day, 3.0625)
  expect_identical(w_meta$file$filename, "MOS2E39230594.gt3x")
  IMP <- ggir_impute(s$M, s$I, MOS2_TZ)
  expect_identical(w_meta$rout, IMP$rout)
  expect_identical(w_meta$r5long, IMP$r5long)
  # the same desiredtz given explicitly: no note
  w2 <- raw.wear.decision(meta, desiredtz = MOS2_TZ)
  expect_length(w2$messages, 0)
  expect_identical(w2$rout, w_M$rout)
  memo$meta_mos2 <- meta
})

test_that("P10 EE: 106/0/5/0/111, wear 5.84375 days, daily hours and valid days", {
  skip_if_no_ggir_ref()
  s <- stored_M("ee")
  expect_identical(s$desiredtz_part1, EE_TZ)
  expect_equal(nrow(s$M$metalong), 672L)
  w <- raw.wear.decision(s$M, desiredtz = EE_TZ)
  expect_identical(unname(colSums(w$rout)), c(106, 0, 5, 0, 111))
  expect_identical(w$wear_dur_def_proto_day, 5.84375)
  expect_identical(w$wear_dur_def_proto_day, (672 - 111) * 15 / 1440)
  expect_identical(w$meas_dur_dys, 7)
  expect_identical(w$clipping_score, 0)
  expect_identical(w$midnights$midnightsi, c(60L, 156L, 252L, 348L, 444L, 540L, 636L))
  d <- w$daily
  expect_identical(nrow(d), 8L)
  expect_identical(d$n_hours, c(14.75, 24, 24, 24, 24, 24, 24, 9.25))
  expect_identical(d$n_valid_hours, c(14.75, 24, 24, 21.25, 23.5, 24, 8.75, 0))
  expect_identical(d$valid, c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(d$weekday, c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday",
                                "Monday", "Tuesday"))
  expect_identical(as.character(d$date), paste0("2017-05-", 23:30))
  expect_identical(d$start_time[1], "2017-05-23T09:15:00+0300")
  expect_identical(w$n_valid_weekdays, 3L)
  expect_identical(w$n_valid_weekend_days, 2L)
  expect_identical(w$n_valid_days, 5L)
  ds <- read_csv_ref(EE_DAYSUM())
  expect_identical(d$n_hours, as.numeric(ds[["N hours"]]))
  expect_identical(d$n_valid_hours, as.numeric(ds[["N valid hours"]]))
  expect_identical(d$weekday, ds$weekday)
  expect_identical(as.character(d$date), ds$calendar_date)
  expect_identical(d$start_time, ds$start_time)
  sm <- read_csv_ref(EE_SUM())
  expect_identical(round(w$wear_dur_def_proto_day, 3), as.numeric(sm$wear_dur_def_proto_day))
  expect_identical(as.numeric(sm$wear_dur_def_proto_day), 5.844)
  expect_identical(w$meas_dur_dys, as.numeric(sm$meas_dur_dys))
  expect_identical(w$n_valid_weekdays, as.integer(sm[["N valid weekdays (WD)"]]))
  expect_identical(w$n_valid_weekend_days, as.integer(sm[["N valid weekend days (WE)"]]))
})

test_that("P10 EE: rout and r5long identical to live GGIR g.impute and g.weardec", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("ee")
  w <- raw.wear.decision(s$M, desiredtz = EE_TZ)
  IMP <- ggir_impute(s$M, s$I, EE_TZ)
  expect_identical(w$rout, IMP$rout)
  expect_identical(w$r5long, IMP$r5long)
  expect_identical(w$LC2, IMP$LC2)
  out <- .raw.weardec(s$M$metalong, 2, 900, desiredtz = EE_TZ)
  gg <- GGIR:::g.weardec(s$M$metalong, 2, 900, params_cleaning = ggir_cleaning(), desiredtz = EE_TZ)
  expect_identical(out, gg)
  expect_identical(w$midnights, GGIR:::g.detecmidnight(as.character(s$M$metalong$timestamp), EE_TZ, 0))
})

test_that("masking strategies and protocol deletions are identical to live GGIR g.impute", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  check <- function(label, expected, ..., ndayswindow = 7, max_calendar_days = 0) {
    w <- raw.wear.decision(s$M, desiredtz = MOS2_TZ, ..., ndayswindow = ndayswindow,
                           max_calendar_days = max_calendar_days)
    IMP <- ggir_impute(s$M, s$I, MOS2_TZ, ..., ndayswindow = ndayswindow,
                       max_calendar_days = max_calendar_days)
    expect_identical(unname(colSums(w$rout)), expected, label = label)
    expect_identical(w$rout, IMP$rout, label = label)
    expect_identical(w$r5long, IMP$r5long, label = label)
    expect_identical(w$hrs.del.start, IMP$hrs.del.start, label = label)
    expect_identical(w$maxdur, IMP$maxdur, label = label)
    invisible(w)
  }
  # strategy 2: outside first and last midnight (14 epochs before, 70 after)
  w2 <- check("strategy 2", c(343, 0, 23, 84, 380), data_masking_strategy = 2)
  expect_identical(which(w2$rout$r4 == 1), c(1:14, 591:660))
  expect_identical(w2$starttimei, 15L)
  expect_identical(w2$endtimei, 591L)
  # strategy 4: before the first midnight only
  w4 <- check("strategy 4", c(343, 0, 23, 14, 380), data_masking_strategy = 4)
  expect_identical(which(w4$rout$r4 == 1), 1:14)
  # strategy 3: the most active 3-day window (7 days would exceed the recording)
  w3 <- check("strategy 3", c(343, 0, 23, 372, 377), data_masking_strategy = 3, ndayswindow = 3)
  expect_identical(w3$hrs.del.start, 1)
  expect_identical(w3$maxdur, 3 + 1/24)
  # strategy 5: the most active calendar days
  check("strategy 5, 7 days", c(343, 0, 23, 84, 380), data_masking_strategy = 5)
  check("strategy 5, 3 days", c(343, 0, 23, 372, 380), data_masking_strategy = 5, ndayswindow = 3)
  # strategy 1 with deletions and maxdur
  w1 <- check("hrs.del 2/3 maxdur 4", c(343, 0, 23, 284, 374), hrs.del.start = 2, hrs.del.end = 3, maxdur = 4)
  expect_identical(which(w1$rout$r4 == 1), c(1:8, 385:660))
  # max_calendar_days: GGIR takes as.Date() of the POSIXct timestamps, the UTC date, so
  # the three days of this -0800 recording end at 16:00 local; transcribed as is
  wm <- check("max_calendar_days 3", c(343, 0, 23, 390, 395), max_calendar_days = 3)
  expect_identical(which(wm$rout$r4 == 1), 271:660)
  expect_identical(s$M$metalong$timestamp[271], "2025-10-10T16:00:00-0800")
  # the wear summary follows r4
  expect_identical(w1$meas_dur_def_proto_day, (660 - 284) * 15 / 1440)
  expect_identical(w1$wear_dur_def_proto_day, sum(w1$rout$r5[w1$rout$r4 == 0] < 1) * 15 / 1440)
  # every strategy leaves r1, r2 and r3 unchanged
  w0 <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  for (w in list(w2, w4, w3, w1, wm)) {
    expect_identical(w$rout[, c("r1", "r2", "r3")], w0$rout[, c("r1", "r2", "r3")])
  }
  expect_error(.raw.wear.mask(w0$rout$r1, w0$rout$r2, s$M$metalong, s$M$metashort, w0$midnights,
                              data_masking_strategy = 6), "data_masking_strategy")
})

test_that("night filter and edge correction switches are identical to live GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  # nonWearEdgeCorrection FALSE changes nothing on MOS2
  wE <- raw.wear.decision(s$M, desiredtz = MOS2_TZ, nonWearEdgeCorrection = FALSE)
  IMP <- ggir_impute(s$M, s$I, MOS2_TZ, nonWearEdgeCorrection = FALSE)
  expect_identical(wE$rout, IMP$rout)
  expect_identical(unname(colSums(wE$rout)), c(343, 0, 23, 0, 366))
  # the night filter: non-wear episodes under 3 h starting between 21:00 and 07:00 become wear
  wF <- raw.wear.decision(s$M, desiredtz = MOS2_TZ, nonwearFiltermaxHours = 3,
                          nonwearFilterWindow = c(21, 7))
  IMPF <- ggir_impute(s$M, s$I, MOS2_TZ, nonwearFiltermaxHours = 3, nonwearFilterWindow = c(21, 7))
  expect_identical(wF$rout, IMPF$rout)
  expect_identical(wF$r5long, IMPF$r5long)
  expect_identical(unname(colSums(wF$rout)), c(330, 0, 20, 0, 350))
  expect_identical(wF$nonwearHoursFiltered, IMPF$nonwearHoursFiltered)
  expect_identical(wF$nonwearEventsFiltered, IMPF$nonwearEventsFiltered)
  expect_gt(wF$nonwearEventsFiltered, 0)
  # the filter stage alone
  r1 <- matrix(as.numeric(s$M$metalong$nonwearscore >= 2), ncol = 1)
  fnn <- .raw.filter.nonwear.night(r1, s$M$metalong, NULL, MOS2_TZ, nonwearFiltermaxHours = 3,
                                   nonwearFilterWindow = c(21, 7), ws2 = 900)
  ggf <- GGIR:::filterNonwearNight(r1, s$M$metalong, NULL, MOS2_TZ,
                                   ggir_cleaning(nonwearFiltermaxHours = 3, nonwearFilterWindow = c(21, 7)), 900)
  expect_identical(fnn, ggf)
  expect_identical(sum(fnn$r1), 330)
  expect_error(raw.wear.decision(s$M, desiredtz = MOS2_TZ, nonwearFiltermaxHours = 3),
               "Please specify parameter nonwearFilterWindow")
  expect_error(GGIR::g.impute(s$M, s$I, params_cleaning = ggir_cleaning(nonwearFiltermaxHours = 3),
                              desiredtz = MOS2_TZ, dayborder = 0, ID = "x"),
               "Please specify parameter nonwearFilterWindow")
  # ggir_exact FALSE honours nonWearEdgeCorrection in the extra passes; same result on both files
  for (which in c("mos2", "ee")) {
    m <- stored_M(which)$M
    tz <- if (which == "mos2") MOS2_TZ else EE_TZ
    a <- .raw.weardec(m$metalong, 2, 900, desiredtz = tz, ggir_exact = TRUE)
    b <- .raw.weardec(m$metalong, 2, 900, desiredtz = tz, ggir_exact = FALSE)
    expect_identical(a, b, label = which)
  }
  wX <- raw.wear.decision(s$M, desiredtz = MOS2_TZ, ggir_exact = FALSE)
  expect_identical(wX$rout, IMP$rout)
  expect_false(wX$settings$ggir_exact)
})

test_that("dayborder 4 slices days at 04:00", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  w <- raw.wear.decision(s$M, desiredtz = MOS2_TZ, dayborder = 4)
  expect_identical(w$midnights, GGIR:::g.detecmidnight(as.character(s$M$metalong$timestamp), MOS2_TZ, 4))
  IMP <- ggir_impute(s$M, s$I, MOS2_TZ, dayborder = 4)
  expect_identical(w$rout, IMP$rout)
  d <- w$daily
  expect_identical(nrow(d), 8L)
  expect_identical(d$n_hours, c(7.5, 24, 24, 24, 24, 24, 24, 13.5))
  expect_identical(sum(d$n_hours), 165)
  expect_identical(d$start_time[2], "2025-10-08T04:00:00-0800")
  expect_identical(as.character(d$date[2]), "2025-10-08")
  expect_identical(d$first_epoch[2], 30 * 180 + 1)
  expect_identical(d$last_epoch[8], 118800)
  expect_identical(sum(d$n_valid_hours), (660 - 366) * 0.25)
  expect_daily_matches_ggir(w, ggir_analyse(s$M, s$I, s$C, MOS2_TZ, dayborder = 4), "dayborder 4")
})

test_that("recordings without a midnight, starting or ending at midnight, or under a day follow GGIR", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  # (a) 13 epochs before the first midnight: GGIR's dummy midnight and last-epoch-minus-one
  Ma <- subset_M(s$M, 1:13)
  wa <- raw.wear.decision(Ma, desiredtz = MOS2_TZ)
  IMPa <- ggir_impute(Ma, s$I, MOS2_TZ)
  expect_identical(wa$rout, IMPa$rout)
  expect_identical(wa$r5long, IMPa$r5long)
  expect_identical(unname(colSums(wa$rout)), c(0, 0, 0, 0, 0))
  expect_identical(nrow(wa$daily), 1L)
  expect_identical(wa$daily$n_hours, (13 * 180 - 1) / 720)
  expect_equal(wa$daily$n_hours, 3.24861111111111, tolerance = 1e-14)
  expect_identical(wa$daily$last_epoch, 13 * 180 - 1)
  expect_identical(wa$daily$weekday, "Tuesday")
  expect_false(wa$daily$valid)
  expect_identical(wa$meas_dur_dys, 13 * 15 / 1440)
  expect_identical(wa$midnights$midnightsi, 13L)
  # (b) six whole days starting at a midnight
  Mb <- subset_M(s$M, 15:590)
  wb <- raw.wear.decision(Mb, desiredtz = MOS2_TZ)
  IMPb <- ggir_impute(Mb, s$I, MOS2_TZ)
  expect_identical(wb$rout, IMPb$rout)
  expect_identical(nrow(wb$daily), 6L)
  expect_identical(wb$daily$n_hours, rep(24, 6))
  # valid hours from a live g.analyse run on this subset
  expect_identical(wb$daily$n_valid_hours, c(21.75, 24, 23.25, 0, 0, 0))
  expect_identical(wb$daily$first_epoch, c(1, 17281, 34561, 51841, 69121, 86401))
  expect_identical(wb$daily$last_epoch, c(17280, 34560, 51840, 69120, 86400, 103680))
  sl <- .raw.day.slices(as.character(Mb$metalong$timestamp), nrow(Mb$metashort),
                        .raw.detect.midnight(as.character(Mb$metalong$timestamp), MOS2_TZ, 0))
  expect_identical(attr(sl, "startatmidnight"), 1)
  expect_identical(attr(sl, "endatmidnight"), 0)
  expect_identical(attr(sl, "ndays"), 6)
  # (c) ending on a midnight epoch: the last day is the single 00:00 epoch
  Mc <- subset_M(s$M, 15:591)
  wc <- raw.wear.decision(Mc, desiredtz = MOS2_TZ)
  expect_identical(wc$rout, ggir_impute(Mc, s$I, MOS2_TZ)$rout)
  expect_identical(nrow(wc$daily), 7L)
  expect_identical(wc$daily$n_hours, c(rep(24, 6), 0.25))
  expect_identical(wc$daily$start_time[7], "2025-10-14T00:00:00-0800")
  # (d) midnight to the next midnight epoch: the closing 00:00 epoch is a second day of 15 min
  Md <- subset_M(s$M, 15:111)
  wd <- raw.wear.decision(Md, desiredtz = MOS2_TZ)
  expect_identical(wd$rout, ggir_impute(Md, s$I, MOS2_TZ)$rout)
  sld <- .raw.day.slices(as.character(Md$metalong$timestamp), nrow(Md$metashort),
                         .raw.detect.midnight(as.character(Md$metalong$timestamp), MOS2_TZ, 0))
  expect_identical(attr(sld, "nfulldays"), 1)
  expect_identical(attr(sld, "startatmidnight"), 1)
  expect_identical(attr(sld, "endatmidnight"), 0)
  expect_identical(attr(sld, "ndays"), 2)
  expect_identical(nrow(wd$daily), 2L)
  expect_identical(wd$daily$first_epoch, c(1, 17281))
  expect_identical(wd$daily$last_epoch, c(17280, 17460))
  expect_identical(wd$daily$n_hours, c(24, 0.25))
  # case (a) is the one that reaches GGIR's endatmidnight branch
  sla <- .raw.day.slices(as.character(Ma$metalong$timestamp), nrow(Ma$metashort),
                         .raw.detect.midnight(as.character(Ma$metalong$timestamp), MOS2_TZ, 0))
  expect_identical(attr(sla, "nfulldays"), 0.125)
  expect_identical(attr(sla, "startatmidnight"), 0)
  expect_identical(attr(sla, "endatmidnight"), 1)
  expect_identical(attr(sla, "ndays"), 1)
  # (e) under 1440 min, with hrs.del.end beyond its length
  Me <- subset_M(s$M, 1:40)
  we <- raw.wear.decision(Me, desiredtz = MOS2_TZ, hrs.del.end = 12)
  IMPe <- ggir_impute(Me, s$I, MOS2_TZ, hrs.del.end = 12)
  expect_identical(we$rout, IMPe$rout)
  expect_identical(sum(we$rout$r4), 40)
  expect_identical(we$meas_dur_def_proto_day, 0)
  expect_identical(we$wear_dur_def_proto_day, 0)
  expect_identical(nrow(we$daily), 2L)
  expect_identical(we$daily$n_hours, c(3.5, 6.5))
  expect_daily_matches_ggir(wa, ggir_analyse(Ma, s$I, s$C, MOS2_TZ), "13 epochs, no midnight")
  expect_daily_matches_ggir(wb, ggir_analyse(Mb, s$I, s$C, MOS2_TZ), "six days from midnight")
  expect_daily_matches_ggir(wc, ggir_analyse(Mc, s$I, s$C, MOS2_TZ), "ending on a midnight epoch")
  expect_daily_matches_ggir(wd, ggir_analyse(Md, s$I, s$C, MOS2_TZ), "midnight to midnight")
  expect_daily_matches_ggir(we, ggir_analyse(Me, s$I, s$C, MOS2_TZ, hrs.del.end = 12), "40 epochs, hrs.del.end 12")
})

test_that("tail-expansion epochs carry r5 -1 and are left out of the days and summaries", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored_M("mos2")
  # cut the stored tables at 21:45 so the expansion rule fires
  cut_long <- which(s$M$metalong$timestamp == "2025-10-09T21:45:00-0800")
  expect_length(cut_long, 1)
  Mcut <- subset_M(s$M, 1:cut_long)
  meta <- meta_from_M(Mcut, MOS2_TZ)
  meta_x <- .raw.tail.expand(meta, recordingEndSleepHour = 19, dayborder = 0, desiredtz = MOS2_TZ)
  expect_identical(meta_x$tail_expansion_log, list(short = 7560L, long = 42L))
  Mx <- as.ggir.M(meta_x)
  expect_identical(sum(Mx$metalong$nonwearscore == -1), 42L)
  w <- raw.wear.decision(meta_x)
  expect_identical(w$n_expanded_long, 42L)
  expect_identical(w$n_expanded_short, 7560L)
  expect_identical(sum(w$rout$r5 == -1), 42L)
  expect_identical(sum(w$r5long == -1), 7560L)
  expect_identical(nrow(w$rout), cut_long + 42L)
  expect_identical(which(w$rout$r5 == -1), (cut_long + 1L):(cut_long + 42L))
  IMP <- ggir_impute(Mx, s$I, MOS2_TZ)
  expect_identical(w$rout, IMP$rout)
  expect_identical(w$r5long, IMP$r5long)
  # as in GGIR, the days and summaries use the un-expanded tables, the decision the expanded ones
  w0 <- raw.wear.decision(meta)
  expect_identical(nrow(w$daily), 3L)
  expect_identical(w$daily[, c("day", "date", "weekday", "start_time", "n_hours", "first_epoch", "last_epoch")],
                   w0$daily[, c("day", "date", "weekday", "start_time", "n_hours", "first_epoch", "last_epoch")])
  expect_identical(w$daily$n_hours[3], (cut_long - 111 + 1) * 0.25)
  expect_identical(w$daily$last_epoch[3], cut_long * 180)
  r5a <- w$r5long[1:(cut_long * 180)]
  expect_identical(w$daily$n_valid_hours,
                   vapply(1:3, function(i) sum(r5a[w$daily$first_epoch[i]:w$daily$last_epoch[i]] == 0) / 720, numeric(1)))
  expect_identical(w$meas_dur_dys, cut_long * 15 / 1440)
  expect_identical(w$meas_dur_dys, w0$meas_dur_dys)
  expect_identical(w$meas_dur_def_proto_day, w0$meas_dur_def_proto_day)
  expect_identical(w$wear_dur_def_proto_day,
                   sum(w$rout$r5[1:cut_long][w$rout$r4[1:cut_long] == 0] < 1) * 15 / 1440)
  expect_identical(w$rout[1:cut_long, c("r1", "r2", "r4")], w0$rout[, c("r1", "r2", "r4")])
  # the stored tables as is do not expand (they end at 17:15)
  expect_null(.raw.tail.expand(meta_from_M(s$M, MOS2_TZ), recordingEndSleepHour = 19,
                               desiredtz = MOS2_TZ)$tail_expansion_log)
})

test_that(".raw.r5long reproduces GGIR's expansion and the decision handles empty input", {
  r5 <- c(0, 1, -1, 0)
  x <- .raw.r5long(r5, 3)
  expect_identical(dim(x), c(12L, 1L))
  expect_identical(as.numeric(x), rep(r5, each = 3))
  expect_true(is.matrix(x))
  # no tables: a no_data object, no error
  meta0 <- structure(list(metashort = NULL, metalong = NULL, windowsizes = NULL, wday = NULL,
                          wdayname = NULL, filecorrupt = TRUE, filetooshort = FALSE,
                          settings = list(desiredtz = "UTC"),
                          file = list(path = NA_character_, filename = "corrupt.gt3x")),
                     class = "canhrActi_raw_meta")
  w0 <- raw.wear.decision(meta0)
  expect_s3_class(w0, "canhrActi_raw_wear")
  expect_identical(w0$status, "no_data")
  expect_null(w0$rout)
  expect_null(w0$daily)
  expect_true(is.na(w0$wear_days))
  expect_identical(w0$n_valid_days, 0L)
  expect_true(any(grepl("No epoch tables", w0$messages)))
  expect_output(print(w0), "no_data")
  expect_error(raw.wear.decision(42), "meta must be")
  expect_error(raw.wear.decision(list(metalong = data.frame(), metashort = data.frame(a = 1)),
                                 desiredtz = "UTC"), NA)
  expect_error(raw.wear.decision(list(metalong = data.frame(timestamp = "x", nonwearscore = 0,
                                                            clippingscore = 0),
                                      metashort = data.frame(timestamp = "x", ENMO = 0)),
                                 desiredtz = "UTC"), "windowsizes")
  expect_error(raw.wear.decision(meta0, not_a_parameter = 1), "unknown")
})

test_that("print method reports the decision", {
  skip_if_no_ggir_ref()
  s <- stored_M("mos2")
  w <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  out <- capture.output(print.canhrActi_raw_wear(w))
  expect_true(any(grepl("non-wear r1 343, clipping r2 0, additional r3 23, protocol r4 0, any r5 366", out, fixed = TRUE)))
  expect_true(any(grepl("wear 3.0625 days", out, fixed = TRUE)))
  expect_true(any(grepl("valid days:  3 of 8 (3 weekdays, 0 weekend days; criterion 16 h)", out, fixed = TRUE)))
  expect_true(any(grepl("Wednesday", out)))
})

test_that("daily hours, valid-day counts and summaries equal a live GGIR g.analyse run (MOS2, EE)", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  for (which in c("mos2", "ee")) {
    s <- stored_M(which)
    tz <- if (which == "mos2") MOS2_TZ else EE_TZ
    w <- raw.wear.decision(s$M, desiredtz = tz)
    A <- ggir_analyse(s$M, s$I, s$C, tz)
    expect_daily_matches_ggir(w, A, which)
    expect_identical(as.character(A$summary[, "wear_dur_def_proto_day"]),
                     if (which == "mos2") "3.0625" else "5.84375")
    # two of the masking strategies against live g.analyse
    w2 <- raw.wear.decision(s$M, desiredtz = tz, data_masking_strategy = 2)
    expect_daily_matches_ggir(w2, ggir_analyse(s$M, s$I, s$C, tz, data_masking_strategy = 2),
                              paste(which, "strategy 2"))
    w1 <- raw.wear.decision(s$M, desiredtz = tz, hrs.del.start = 2, hrs.del.end = 3, maxdur = 4)
    expect_daily_matches_ggir(w1, ggir_analyse(s$M, s$I, s$C, tz, hrs.del.start = 2, hrs.del.end = 3, maxdur = 4),
                              paste(which, "hrs.del maxdur"))
  }
})
