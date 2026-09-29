# raw.sib.detect against GGIR's g.sib.det: identical() on the whole returned list.
# Reference data come from CANHRACTI_GGIR_REF and the part-3 fixtures beside it, and are
# skipped when absent; live GGIR comparisons are skipped when GGIR is not installed.
# The MOS2 milestones were stored with desiredtz "" (the system timezone) on a machine set
# to America/Anchorage, so the file runs in that zone.

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
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
p34_fixture <- function(case) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", case, "output_din")
}

DET_CASES <- list(
  MOS2 = list(out = c("out", "output_din"),           fn = "MOS2E39230594.gt3x"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x"))

det_path <- function(case, what) {
  cc <- DET_CASES[[case]]
  switch(what,
         basic = do.call(ref_file, c(as.list(cc$out), list("meta", "basic",
                                                           paste0("meta_", cc$fn, ".RData")))),
         ms2   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms2.out",
                                                           paste0(cc$fn, ".RData")))),
         ms3   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms3.out",
                                                           paste0(cc$fn, ".RData")))))
}

# Both GGIR and the port are given the stored desiredtz_part1.
det_load <- function(basic, ms2, ms3) {
  e1 <- new.env(parent = emptyenv()); load(basic, envir = e1)
  e2 <- new.env(parent = emptyenv()); load(ms2, envir = e2)
  e3 <- new.env(parent = emptyenv()); load(ms3, envir = e3)
  list(M = e1$M, I = e1$I, IMP = e2$IMP, ms3 = e3, tz = e3$desiredtz_part1)
}

# GGIR's own detector on the same input, with the same parameters g.part3 passes.
det_ggir <- function(d, params_sleep = NULL, twd = c(-12, 12), IMP = NULL) {
  P <- GGIR::load_params(topic = c("sleep", "metrics", "general"))
  GGIR:::g.sib.det(M = d$M, IMP = if (is.null(IMP)) d$IMP else IMP, I = d$I, twd = twd,
                   acc.metric = P$params_general$acc.metric, desiredtz = d$tz, myfun = c(),
                   sensor.location = P$params_general$sensor.location,
                   params_sleep = if (is.null(params_sleep)) P$params_sleep else params_sleep,
                   zc.scale = P$params_metrics$zc.scale)
}

det_ours <- function(d, IMP = NULL, twd = c(-12, 12), ...) {
  raw.sib.detect(meta = d$M, imputed = if (is.null(IMP)) d$IMP else IMP, twd = twd,
                 params = raw.params(desiredtz = d$tz, ...))
}

# One memoised (load, GGIR run, port run) triple per reference recording.
det_case <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref(); skip_if_no_ggir()
    pb <- det_path(case, "basic"); skip_if_no_file(pb)
    p2 <- det_path(case, "ms2");   skip_if_no_file(p2)
    p3 <- det_path(case, "ms3");   skip_if_no_file(p3)
    d <- det_load(pb, p2, p3)
    out <- list(d = d, ggir = det_ggir(d), ours = det_ours(d))
    assign(case, out, envir = cache)
    out
  }
})

# The same for a part-3 fixture.
det_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref(); skip_if_no_ggir()
    root <- p34_fixture(case)
    if (!dir.exists(root)) testthat::skip(paste0("fixture not present: ", case))
    pb <- list.files(file.path(root, "meta", "basic"), full.names = TRUE)
    p2 <- list.files(file.path(root, "meta", "ms2.out"), full.names = TRUE)
    p3 <- list.files(file.path(root, "meta", "ms3.out"), full.names = TRUE)
    if (length(pb) == 0 || length(p2) == 0 || length(p3) == 0) {
      testthat::skip(paste0("fixture incomplete: ", case))
    }
    d <- det_load(pb[1], p2[1], p3[1])
    out <- list(d = d, ggir = det_ggir(d), ours = det_ours(d))
    assign(case, out, envir = cache)
    out
  }
})

# A synthetic recording at 60 s epochs, for the tests that need no reference file.
det_synth <- function(start = "2024-05-07 20:30:00", tz = "UTC", ws3 = 60, n = 4320,
                      extra = NULL, invalid_long = NULL) {
  tt <- seq(as.POSIXct(start, tz = tz), by = ws3, length.out = n)
  iso <- strftime(tt, format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  anglez <- rep(c(0, 30), length.out = n)
  hm <- format(tt, "%H:%M", tz = tz)
  still <- hm >= "23:00" | hm < "07:00"
  anglez[still] <- 10
  ms <- data.frame(timestamp = iso, anglez = anglez, ENMO = ifelse(still, 0.001, 0.05),
                   stringsAsFactors = FALSE)
  if (!is.null(extra)) for (nm in names(extra)) ms[[nm]] <- extra[[nm]]
  nlong <- ceiling(n / (900 / ws3))
  r5 <- if (is.null(invalid_long)) rep(0, nlong) else invalid_long
  rout <- data.frame(r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = r5)
  list(M = list(metashort = ms, windowsizes = c(ws3, 900, 3600)),
       IMP = list(metashort = ms, rout = rout),
       I = list(monn = "actigraph"), tz = tz)
}

det_synth_ggir <- function(s, params_sleep = NULL, twd = c(-12, 12)) {
  P <- GGIR::load_params(topic = c("sleep", "metrics", "general"))
  GGIR:::g.sib.det(M = s$M, IMP = s$IMP, I = s$I, twd = twd, acc.metric = "ENMO",
                   desiredtz = s$tz, myfun = c(), sensor.location = "wrist",
                   params_sleep = if (is.null(params_sleep)) P$params_sleep else params_sleep,
                   zc.scale = 1)
}

# Drop the first k_short short epochs (a whole number of long epochs) and the matching long
# epochs, so the recording starts at a later clock time.
det_trunc_lead <- function(IMP, k_short, ws2_over_ws3 = 180) {
  IMP2 <- IMP
  IMP2$metashort <- IMP$metashort[-(1:k_short), ]
  rownames(IMP2$metashort) <- NULL
  IMP2$rout <- IMP$rout[-(1:(k_short / ws2_over_ws3)), ]
  IMP2
}

test_that("raw.sib.detect is identical() to g.sib.det on MOS2, member by member", {
  cc <- det_case("MOS2")
  g <- as.ggir.SLE(cc$ours)
  expect_identical(g$output, cc$ggir$output)
  expect_identical(dim(g$output), c(118800L, 5L))
  expect_identical(colnames(g$output),
                   c("time", "invalid", "night", "T5A5", "spt_crude_estimate"))
  expect_identical(unname(sapply(g$output, function(z) class(z)[1])),
                   c("factor", "numeric", "numeric", "numeric", "numeric"))
  for (nm in c("detection.failed", "L5list", "SPTE_end", "SPTE_start", "tib.threshold",
               "longitudinal_axis", "part3_guider")) {
    expect_identical(g[[nm]], cc$ggir[[nm]], info = nm)
  }
  expect_identical(g, cc$ggir)
  expect_identical(names(g), c("output", "detection.failed", "L5list", "SPTE_end",
                               "SPTE_start", "tib.threshold", "longitudinal_axis",
                               "part3_guider"))
})

test_that("raw.sib.detect reproduces the five per-night vectors stored in MOS2's ms3", {
  cc <- det_case("MOS2")
  o <- cc$ours; m3 <- cc$d$ms3
  expect_identical(o$L5list, m3$L5list)
  expect_identical(o$SPTE_start, m3$SPTE_start)
  expect_identical(o$SPTE_end, m3$SPTE_end)
  expect_identical(o$tib.threshold, m3$tib.threshold)
  expect_identical(o$part3_guider, m3$part3_guider)
  expect_identical(o$longitudinal_axis, m3$longitudinal_axis)
  expect_null(o$longitudinal_axis)
  expect_false(o$detection.failed)
  expect_identical(o$rec_starttime, m3$rec_starttime)
  expect_identical(o$rec_starttime, "2025-10-07T20:30:00-0800")
})

test_that("the NA patterns of the four MOS2 per-night vectors are asymmetric", {
  # night 1 is a partial first day; nights 5 to 7 are wholly non-wear
  cc <- det_case("MOS2")
  o <- cc$ours
  expect_true(is.na(o$SPTE_start[1]))
  expect_true(is.na(o$SPTE_end[1]))
  expect_identical(o$tib.threshold[1], 0.13)
  expect_identical(o$part3_guider[1], "HDCZA")
  expect_identical(o$part3_guider,
                   c("HDCZA", "HDCZA", "HDCZA", "HDCZA", NA, NA, NA))
  expect_identical(which(is.na(o$SPTE_start)), c(1L, 5L, 6L, 7L))
  expect_identical(which(is.na(o$tib.threshold)), c(5L, 6L, 7L))
  expect_identical(which(is.na(o$L5list)), integer(0))
  # the "10%" name from quantile() is dropped
  expect_null(names(o$tib.threshold))
})

test_that("MOS2 night 2 and 3 SPTE values are the guider indices over 720 plus 12", {
  cc <- det_case("MOS2")
  o <- cc$ours
  # 8251 and 14299 are the guider's epoch indices for night 2
  expect_identical(o$SPTE_start[2], 8251 / 720 + 12)
  expect_identical(o$SPTE_end[2], 14299 / 720 + 12)
  expect_identical(round(o$SPTE_start[2], 13), 23.4597222222222)
  expect_identical(round(o$SPTE_end[2], 13), 31.8597222222222)
  expect_identical(round(o$SPTE_start[3], 13), 23.7236111111111)
  expect_identical(round(o$SPTE_end[3], 13), 31.0708333333333)
  expect_identical(o$SPTE_start[4], 7229 / 720 + 12)
  expect_identical(o$SPTE_end[4], 13731 / 720 + 12)
  # 31.86 is 07:51:35 the next morning; 12 is noon of the night's own date
  expect_identical(round((o$SPTE_end[2] - 24) * 3600), 28295)
  expect_identical(round(o$L5list, 5),
                   c(19.04306, 26.01389, 28.00139, 27.95139, 27.95139, 27.95139, 27.95139))
})

test_that("MOS2 night indexing and the per-epoch night column", {
  cc <- det_case("MOS2")
  o <- cc$ours
  expect_identical(o$midnights, c(2521L, 19801L, 37081L, 54361L, 71641L, 88921L, 106201L))
  expect_identical(o$midn_start, 1)
  expect_identical(unname(table(o$output$night)),
                   unname(table(rep(0:7, c(3959, 11161, rep(17280, 6))))))
  expect_identical(nrow(o$windows), 7L)
  expect_identical(o$windows$qqq1_unclamped,
                   c(-6118, 11162, 28442, 45722, 63002, 80282, 97562))
  expect_identical(o$windows$qqq2_unclamped,
                   c(11161, 28441, 45721, 63001, 80281, 97561, 114841))
  expect_identical(o$windows$qqq1, c(1, 11162, 28442, 45722, 63002, 80282, 97562))
  expect_identical(o$windows$n_epochs, c(11161, rep(17280, 6)))
  expect_identical(o$windows$start.time[1], "2025-10-07T20:30:00-0800")
  expect_identical(o$windows$end.time[1], "2025-10-08T12:00:00-0800")
  expect_false(any(o$windows$skipped))
  expect_identical(o$windows$partial_first_day, c(TRUE, rep(FALSE, 6)))
  expect_identical(round(o$windows$nonwear_percentage, 4),
                   c(8.0638, 0, 0, 53.1308, 100, 100, 100))
  expect_identical(o$windows$guider_to_use, rep(1, 7))
  expect_identical(o$windows$daysleep_offset, rep(0, 7))
})

test_that("the MOS2 midnights and 04:00 rule come out of the shared helper", {
  cc <- det_case("MOS2")
  time <- format(cc$d$IMP$metashort[, 1])
  dm <- .raw.detect.midnight(time, cc$d$tz, dayborder = 0)
  expect_identical(dm, GGIR:::g.detecmidnight(time, cc$d$tz, dayborder = 0))
  expect_identical(dm$midnightsi, c(2521L, 19801L, 37081L, 54361L, 71641L, 88921L, 106201L))
  expect_identical(dm$firstmidnighti, 2521L)
  first4am <- grep("04:00:00", time[1:pmin(length(time), 17281)])[1]
  expect_identical(first4am, 5401L)
  expect_identical(time[first4am], "2025-10-08T04:00:00-0800")
  expect_true(first4am > dm$firstmidnighti)   # so midn_start is 1
})

test_that("raw.sib.detect is identical() to g.sib.det on EE, and to EE's stored ms3", {
  cc <- det_case("EE")
  expect_identical(as.ggir.SLE(cc$ours), cc$ggir)
  o <- cc$ours; m3 <- cc$d$ms3
  expect_identical(cc$d$tz, "Europe/Helsinki")
  expect_identical(o$L5list, m3$L5list)
  expect_identical(o$SPTE_start, m3$SPTE_start)
  expect_identical(o$SPTE_end, m3$SPTE_end)
  expect_identical(o$tib.threshold, m3$tib.threshold)
  expect_identical(o$part3_guider, m3$part3_guider)
  expect_identical(o$rec_starttime, "2017-05-23T09:15:00+0300")
  # EE starts at 09:15, so its first night is a full day
  expect_false(o$windows$partial_first_day[1])
  expect_identical(round(o$SPTE_start, 5),
                   c(22.27778, 23.62222, 25.02222, 24.33333, 24.28472, 22.77639, NA))
  expect_identical(round(o$SPTE_end, 5),
                   c(30.11806, 33.01667, 32.46944, 30.65833, 32.825, 30.24722, NA))
  # 0.13 is a floor, not a constant
  expect_identical(round(o$tib.threshold, 5),
                   c(0.13, 0.20385, 0.132, 0.207, 0.1785, 0.1725, NA))
  expect_identical(round(o$L5list, 5),
                   c(24.19861, 29.87639, 28.38611, 28.16528, 28.96806, 27.56806, 27.76389))
  expect_identical(o$part3_guider, c(rep("HDCZA", 6), NA))
})

test_that("the part-1 timezone overrides the caller, as g.part3 does", {
  cc <- det_case("EE")
  cmeta <- structure(list(metashort = cc$d$M$metashort, metalong = cc$d$M$metalong,
                          windowsizes = cc$d$M$windowsizes,
                          settings = list(desiredtz = "Europe/Helsinki")),
                     class = "canhrActi_raw_meta")
  o <- raw.sib.detect(meta = cmeta, imputed = cc$d$IMP,
                      params = raw.params(desiredtz = "America/Anchorage"))
  expect_identical(o$desiredtz, "Europe/Helsinki")
  expect_identical(as.ggir.SLE(o), cc$ggir)
  cm2 <- cmeta; cm2$settings$desiredtz <- ""
  expect_identical(raw.sib.detect(meta = cm2, imputed = cc$d$IMP,
                                  params = raw.params(desiredtz = "America/Anchorage"),
                                  ggir_exact = TRUE)$desiredtz, "")
})

test_that(".raw.sleep.invalid reproduces GGIR's expansion of rout column 5", {
  cc <- det_case("MOS2")
  IMP <- cc$d$IMP
  inv <- .raw.sleep.invalid(IMP$rout, nrow(IMP$metashort), 900, 5)
  expect_identical(unname(colSums(IMP$rout)), c(343, 0, 23, 0, 366))
  expect_identical(length(inv), 118800L)
  expect_identical(sum(inv), 366 * 180)
  expect_identical(sum(inv), 65880)
  expect_identical(as.numeric(IMP$r5long), as.numeric(inv))
  expect_identical(inv, as.numeric(cc$ours$output$invalid))
  expect_identical(cc$ours$n_invalid_short, 65880L)
})

test_that(".raw.sleep.invalid clips when the expansion is long and pads with VALID when short", {
  rout <- data.frame(r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = c(0, 1, 1))
  # ws2/ws3 = 4, so the expansion is 12 long
  expect_identical(.raw.sleep.invalid(rout, 12, 4, 1),
                   c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1))
  expect_identical(.raw.sleep.invalid(rout, 6, 4, 1), c(0, 0, 0, 0, 1, 1))
  expect_identical(.raw.sleep.invalid(rout, 15, 4, 1),
                   c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0))
  expect_error(.raw.sleep.invalid(rout[, 1:3], 12, 4, 1), "five columns")
})

test_that("the partial-first-day unlist() defect is reproduced and can be switched off", {
  cc <- det_case("MOS2")
  exact <- cc$ours
  expect_true(is.na(exact$SPTE_start[1]))
  expect_true(is.na(exact$SPTE_end[1]))
  loose <- det_ours(cc$d, ggir_exact = FALSE)
  # the lost offset is 20.5 h (a 20:30 start); 1539 and 7689 are the night 1 guider indices
  expect_identical(loose$SPTE_start[1], 1539 / 720 + 20.5)
  expect_identical(loose$SPTE_start[1], 22.6375)
  expect_identical(loose$SPTE_end[1], 7689 / 720 + 20.5)
  expect_identical(round(loose$SPTE_end[1], 13), 31.1791666666667)
  expect_identical(loose$SPTE_start[2:7], exact$SPTE_start[2:7])
  expect_identical(loose$SPTE_end[2:7], exact$SPTE_end[2:7])
  expect_identical(loose$tib.threshold, exact$tib.threshold)
  expect_identical(loose$L5list, exact$L5list)
  expect_identical(loose$output, exact$output)
})

test_that(".raw.sleep.spte.hours is the only place the defect lives", {
  sp <- list(SPTE_start = 720, SPTE_end = 1440)
  tt <- rep("2025-10-07T20:30:00-0800", 1440)
  # an ordinary night: offset 12
  a <- .raw.sleep.spte.hours(sp, ws3 = 5, qqq1 = 100, partialFirstDay = FALSE,
                             daysleep_offset = 0,
                             rec_starttime = "2025-10-07T20:30:00-0800", tmpTIME = tt,
                             desiredtz = "America/Anchorage")
  expect_identical(a$SPTE_start, 13)
  expect_identical(a$SPTE_end, 14)
  # a day-sleeper re-run: offset 18
  b <- .raw.sleep.spte.hours(sp, ws3 = 5, qqq1 = 100, partialFirstDay = FALSE,
                             daysleep_offset = 6,
                             rec_starttime = "2025-10-07T20:30:00-0800", tmpTIME = tt,
                             desiredtz = "America/Anchorage")
  expect_identical(b$SPTE_start, 19)
  expect_identical(b$SPTE_end, 20)
  # a partial first day: NA under ggir_exact, the start hour under FALSE
  cexact <- .raw.sleep.spte.hours(sp, ws3 = 5, qqq1 = 1, partialFirstDay = TRUE,
                                  daysleep_offset = 0,
                                  rec_starttime = "2025-10-07T20:30:00-0800", tmpTIME = tt,
                                  desiredtz = "America/Anchorage")
  expect_true(is.na(cexact$SPTE_start) && is.na(cexact$SPTE_end))
  cloose <- .raw.sleep.spte.hours(sp, ws3 = 5, qqq1 = 1, partialFirstDay = TRUE,
                                  daysleep_offset = 0,
                                  rec_starttime = "2025-10-07T20:30:00-0800", tmpTIME = tt,
                                  desiredtz = "America/Anchorage", ggir_exact = FALSE)
  expect_identical(cloose$SPTE_start, 21.5)
  expect_identical(cloose$SPTE_end, 22.5)
  # qqq1 must be 1 as well, or the offset is 12
  d <- .raw.sleep.spte.hours(sp, ws3 = 5, qqq1 = 2, partialFirstDay = TRUE,
                             daysleep_offset = 0,
                             rec_starttime = "2025-10-07T20:30:00-0800", tmpTIME = tt,
                             desiredtz = "America/Anchorage")
  expect_identical(d$SPTE_start, 13)
})

test_that("a recording starting between midnight and 04:00 runs one night more than it has slots", {
  cc <- det_case("MOS2")
  IMP2 <- det_trunc_lead(cc$d$IMP, 3960)      # 20:30 plus 5.5 h is 02:00
  expect_identical(IMP2$metashort$timestamp[1], "2025-10-08T02:00:00-0800")
  g <- det_ggir(cc$d, IMP = IMP2)
  o <- det_ours(cc$d, IMP = IMP2)
  expect_identical(as.ggir.SLE(o), g)
  expect_identical(o$midn_start, 0)
  expect_identical(length(o$midnights), 6L)
  expect_identical(max(o$output$night), 7)
  # L5list grows; the other four are pre-allocated to the number of midnights
  expect_identical(length(o$L5list), 7L)
  expect_identical(length(o$SPTE_start), 6L)
  expect_identical(length(o$SPTE_end), 6L)
  expect_identical(length(o$tib.threshold), 6L)
  expect_identical(length(o$part3_guider), 6L)
  loose <- det_ours(cc$d, IMP = IMP2, ggir_exact = FALSE)
  expect_identical(length(loose$SPTE_start), 7L)
  expect_identical(length(loose$part3_guider), 7L)
  expect_identical(loose$SPTE_start[2:4], o$SPTE_start[2:4])
})

test_that("the L5 offset is 12 even on a partial first day, where SPTE is not (spec S3f)", {
  cc <- det_case("MOS2")
  expect_identical(round(cc$ours$L5list[1], 5), 19.04306)
  IMP2 <- det_trunc_lead(cc$d$IMP, 3960)
  o <- det_ours(cc$d, IMP = IMP2)
  expect_identical(round(o$L5list[1], 6), 14.679167)
  # the two window starts are 3142 epochs apart
  expect_equal(cc$ours$L5list[1] - o$L5list[1], 3142 / 720, tolerance = 1e-12)
})

test_that("recordings starting at 05:30 and 13:30 give midn_start 1 and no extra night", {
  cc <- det_case("MOS2")
  for (k in c(6480, 12240)) {
    IMP2 <- det_trunc_lead(cc$d$IMP, k)
    g <- det_ggir(cc$d, IMP = IMP2)
    o <- det_ours(cc$d, IMP = IMP2)
    expect_identical(as.ggir.SLE(o), g)
    expect_identical(o$midn_start, 1)
    expect_identical(length(o$midnights), 6L)
    expect_identical(length(o$L5list), 6L)
    expect_identical(length(o$SPTE_start), 6L)
    expect_identical(max(o$output$night), 6)
  }
  expect_identical(det_trunc_lead(cc$d$IMP, 6480)$metashort$timestamp[1],
                   "2025-10-08T05:30:00-0800")
  expect_identical(det_trunc_lead(cc$d$IMP, 12240)$metashort$timestamp[1],
                   "2025-10-08T13:30:00-0800")
})

test_that("a skipped night still consumes a night index and leaves an all-NA slot", {
  # MOS2 from 02:00 with no midnight left: the j = 0 window ends 2640 epochs before the
  # recording starts, so it is skipped after sptei has already been incremented
  cc <- det_case("MOS2")
  IMP2 <- cc$d$IMP
  IMP2$metashort <- cc$d$IMP$metashort[3961:9960, ]
  rownames(IMP2$metashort) <- NULL
  IMP2$rout <- cc$d$IMP$rout[-(1:22), ]
  g <- det_ggir(cc$d, IMP = IMP2)
  o <- det_ours(cc$d, IMP = IMP2)
  expect_identical(as.ggir.SLE(o), g)
  expect_identical(o$midn_start, 0)
  expect_identical(length(o$midnights), 1L)
  expect_identical(sort(unique(o$output$night)), 2)   # night 1 was consumed by the skip
  expect_identical(nrow(o$windows), 2L)
  expect_identical(o$windows$skipped, c(TRUE, FALSE))
  expect_identical(o$windows$qqq1_unclamped, c(1, -2639))
  expect_identical(o$windows$qqq2_unclamped, c(-2640, 14640))
  expect_identical(o$windows$qqq1, c(NA, 1))
  expect_identical(o$windows$qqq2, c(NA, 6000))
  expect_true(is.na(o$L5list[1]))
  expect_identical(round(o$L5list[2], 6), 14.679167)
  expect_identical(length(o$L5list), 2L)
  expect_identical(o$tib.threshold, c(NA, 0.13))
  expect_identical(o$part3_guider, c(NA, "HDCZA"))
  # night 2 is a partial first day, so its SPTE is NA under ggir_exact
  expect_true(all(is.na(o$SPTE_start)))
  loose <- det_ours(cc$d, IMP = IMP2, ggir_exact = FALSE)
  expect_true(is.na(loose$SPTE_start[1]))
  expect_false(is.na(loose$SPTE_start[2]))
})

test_that("the too-short gate is nD/17280 > 0.2, tested strictly (spec S3g)", {
  cc <- det_case("MOS2")
  for (n in c(100, 2000, 3000, 3455, 3456)) {
    IMP2 <- cc$d$IMP; IMP2$metashort <- cc$d$IMP$metashort[1:n, ]
    o <- det_ours(cc$d, IMP = IMP2)
    expect_true(o$detection.failed, info = n)
    expect_null(o$output)
    expect_null(o$L5list)
    expect_null(o$SPTE_start)
    expect_identical(o$status, "too_short")
    expect_identical(as.ggir.SLE(o), det_ggir(cc$d, IMP = IMP2), info = n)
  }
  for (n in c(3457, 6000)) {
    IMP2 <- cc$d$IMP; IMP2$metashort <- cc$d$IMP$metashort[1:n, ]
    o <- det_ours(cc$d, IMP = IMP2)
    expect_false(o$detection.failed, info = n)
    expect_identical(max(o$output$night), 1)
    expect_identical(as.ggir.SLE(o), det_ggir(cc$d, IMP = IMP2), info = n)
  }
  expect_identical(3456 / 17280, 0.2)   # the boundary is excluded
})

test_that("every part-3 fixture is identical() to GGIR, whole list included", {
  for (fx in c("spring", "autumn", "autumn_edge", "daysleeper", "novalid",
               "novalid_keepsibs", "keepsibs", "nonights")) {
    cc <- det_fixture(fx)
    expect_identical(as.ggir.SLE(cc$ours), cc$ggir, info = fx)
    expect_identical(cc$ours$L5list, cc$d$ms3$L5list, info = fx)
    expect_identical(cc$ours$SPTE_start, cc$d$ms3$SPTE_start, info = fx)
    expect_identical(cc$ours$SPTE_end, cc$d$ms3$SPTE_end, info = fx)
    expect_identical(cc$ours$tib.threshold, cc$d$ms3$tib.threshold, info = fx)
    expect_identical(cc$ours$part3_guider, cc$d$ms3$part3_guider, info = fx)
  }
})

test_that("the day-sleeper fixture exercises the 6 hour re-run and the missing slot", {
  cc <- det_fixture("daysleeper")
  o <- cc$ours
  # nights 2 to 4 accepted the shifted window; night 1 triggered it but could not run it
  expect_identical(o$windows$daysleep_offset, c(0, 6, 6, 6, 0, 0, 0))
  # 36 is the next noon
  expect_identical(round(o$SPTE_end, 5), c(NA, 37.85972, 37.07083, 37.07083, NA, NA))
  expect_true(all(o$SPTE_end[2:4] > 36))
  expect_identical(round(o$SPTE_start, 5), c(NA, 29.45972, 29.72361, 28.04028, NA, NA))
  # midn_start is 0, so the four vectors are one short of max(night)
  expect_identical(o$midn_start, 0)
  expect_identical(length(o$L5list), 7L)
  expect_identical(length(o$SPTE_start), 6L)
  expect_identical(max(o$output$night), 7)
  expect_identical(round(o$L5list, 5),
                   c(19.00139, 32.01389, 33.5, 33.5, 33.5, 33.5, 33.5))
  loose <- det_ours(cc$d, ggir_exact = FALSE)
  expect_identical(length(loose$SPTE_start), 7L)
  expect_identical(loose$SPTE_start[2:4], o$SPTE_start[2:4])
  expect_identical(loose$SPTE_start[1], 4.6375)
})

test_that("the DST fixtures move the window end by an hour and correct the SPTE pair", {
  spring <- det_fixture("spring")
  autumn <- det_fixture("autumn")
  edge <- det_fixture("autumn_edge")
  for (cc in list(spring, autumn, edge)) {
    expect_identical(cc$d$tz, "Europe/Helsinki")
  }
  delta <- function(o) o$windows$qqq2_unclamped - (o$midnights + 12 * 720)
  expect_identical(delta(spring$ours), c(0, 0, 0, 0, -720, 0, 0))
  expect_identical(delta(autumn$ours), c(0, 0, 0, 0, 720, 0, 0))
  expect_identical(delta(edge$ours), c(0, 0, 0, 720, 0, 0, 0))
  # the fixtures are copies of EE; night 5 ends an hour later in spring, an hour earlier in autumn
  ee <- det_case("EE")
  expect_identical(round(ee$ours$SPTE_end[5], 5), 32.825)
  expect_identical(round(spring$ours$SPTE_end[5], 5), 33.825)
  expect_identical(round(autumn$ours$SPTE_end[5], 5), 31.825)
  expect_identical(round(spring$ours$SPTE_start[5], 5), 24.28472)
  expect_identical(round(autumn$ours$SPTE_start[5], 5), 24.28194)
})

test_that("a wholly invalid recording gives NA guider output but does not fail detection", {
  cc <- det_fixture("novalid")
  o <- cc$ours
  expect_false(o$detection.failed)
  expect_identical(o$n_invalid_short, 118800L)
  # never assigned, so the pre-allocated rep(NA, countmidn) is still logical
  expect_true(is.logical(o$SPTE_start))
  expect_true(is.logical(o$SPTE_end))
  expect_true(is.logical(o$tib.threshold))
  expect_true(is.logical(o$part3_guider))
  expect_identical(o$SPTE_start, rep(NA, 7))
  expect_identical(o$part3_guider, rep(NA, 7))
  expect_identical(o$L5list, rep(0, 7))
  # anglez is constant, so the fewer-than-10-posture-changes fallback marks the whole
  # recording one bout
  expect_identical(length(which(diff(as.numeric(cc$d$IMP$metashort$anglez)) != 0)), 0L)
  expect_identical(sum(o$output$T5A5), 118800)
})

test_that("part 3 reads the imputed series, and the part-1 series gives different numbers", {
  cc <- det_case("MOS2")
  M <- cc$d$M; IMP <- cc$d$IMP
  expect_identical(M$metashort$timestamp, IMP$metashort$timestamp)
  expect_identical(sum(M$metashort$anglez != IMP$metashort$anglez), 65880L)
  expect_identical(sum(cc$ours$output$T5A5), 43664)
  wrong <- IMP; wrong$metashort <- M$metashort
  o <- det_ours(cc$d, IMP = wrong)
  expect_identical(sum(o$output$T5A5), 76793)
  expect_false(identical(o$output$T5A5, cc$ours$output$T5A5))
  expect_identical(as.ggir.SLE(o), det_ggir(cc$d, IMP = wrong))
})

test_that("raw.sib.detect can build the imputed series itself", {
  cc <- det_case("MOS2")
  o <- raw.sib.detect(meta = cc$d$M, wear = cc$d$IMP, params = raw.params(desiredtz = cc$d$tz))
  expect_identical(as.ggir.SLE(o), cc$ggir)
})

test_that("a canhrActi_raw is unwrapped into its meta, imputed and wear members", {
  cc <- det_case("MOS2")
  cmeta <- structure(list(metashort = cc$d$M$metashort, metalong = cc$d$M$metalong,
                          windowsizes = cc$d$M$windowsizes,
                          settings = list(desiredtz = ""),
                          file = list(filename = "MOS2E39230594.gt3x")),
                     class = "canhrActi_raw_meta")
  x <- structure(list(meta = cmeta, imputed = cc$d$IMP,
                      wear = list(rout = cc$d$IMP$rout, r5long = cc$d$IMP$r5long)),
                 class = "canhrActi_raw")
  o <- raw.sib.detect(x)
  expect_identical(as.ggir.SLE(o), cc$ggir)
  expect_identical(o$file$filename, "MOS2E39230594.gt3x")
  expect_identical(o$desiredtz, "")
})

test_that("several sib definitions rename the columns and reach the guider only as the first", {
  cc <- det_case("MOS2")
  P <- GGIR::load_params(topic = "sleep")$params_sleep
  P$timethreshold <- c(5, 10); P$anglethreshold <- c(5, 6)
  g <- det_ggir(cc$d, params_sleep = P)
  o <- raw.sib.detect(meta = cc$d$M, imputed = cc$d$IMP,
                      params = raw.params(desiredtz = cc$d$tz, timethreshold = c(5, 10),
                                          anglethreshold = c(5, 6)))
  expect_identical(as.ggir.SLE(o), g)
  expect_identical(o$definitions,
                   c("sleep.T5A5", "sleep.T5A6", "sleep.T10A5", "sleep.T10A6"))
  expect_identical(colnames(o$output),
                   c("time", "invalid", "night", "sleep.T5A5", "sleep.T5A6", "sleep.T10A5",
                     "sleep.T10A6", "spt_crude_estimate"))
  expect_identical(o$SPTE_start, cc$ours$SPTE_start)
  expect_identical(o$output$sleep.T5A5, cc$ours$output$T5A5)
})

test_that(".raw.sleep.decide.guider returns 2 only for the NotWorn pair under 25 percent", {
  expect_identical(.raw.sleep.decide.guider(c("NotWorn", "HDCZA"), 24.9), 2)
  expect_identical(.raw.sleep.decide.guider(c("NotWorn", "HDCZA"), 25), 1)
  expect_identical(.raw.sleep.decide.guider(c("NotWorn", "HDCZA"), 100), 1)
  expect_identical(.raw.sleep.decide.guider("NotWorn", 0), 1)
  expect_identical(.raw.sleep.decide.guider(c("HDCZA", "NotWorn"), 0), 1)
  expect_identical(.raw.sleep.decide.guider("HDCZA", 0), 1)
  expect_identical(.raw.sleep.decide.guider(c("NotWorn", "HDCZA", "HLRB"), 0), 1)
})

test_that(".raw.sleep.l5 forces an odd kernel and reports hours with an unconditional +12", {
  # a window no longer than the kernel, or a flat one, returns 0
  expect_identical(.raw.sleep.l5(rep(1, 3601), 5), 0)
  expect_identical(.raw.sleep.l5(numeric(0), 5), 0)
  expect_identical(.raw.sleep.l5(rep(0.02, 5000), 5), 0)
  # at ws3 = 60 the kernel is 300, forced to 301
  x <- rep(1, 1440); x[701:1000] <- 0
  got <- .raw.sleep.l5(x, 60)
  zrm <- zoo::rollmean(x, k = 301, fill = "extend", align = "center")
  expect_identical(got, (which(zrm == min(zrm))[1] / 60) + 12)
  expect_true(got > 12)
})

test_that(".raw.sleep.dst.check leaves an ordinary night alone and shifts a clock-change one", {
  tz <- "Europe/Helsinki"
  same <- rep("2017-03-25T00:00:00+0200", 3)
  expect_identical(.raw.sleep.dst.check(same, list(SPTE_start = 2, SPTE_end = 3), tz, 30, 22),
                   c(22, 30))
  # only the end crosses into +0300
  cross <- c("2017-03-26T00:00:00+0200", "2017-03-26T01:00:00+0200",
             "2017-03-26T04:00:00+0300")
  expect_identical(.raw.sleep.dst.check(cross, list(SPTE_start = 2, SPTE_end = 3), tz, 30, 22),
                   c(22, 31))
  # autumn: both ends fall back to +0200
  back <- c("2017-10-29T00:00:00+0300", "2017-10-29T04:00:00+0200",
            "2017-10-29T07:00:00+0200")
  expect_identical(.raw.sleep.dst.check(back, list(SPTE_start = 2, SPTE_end = 3), tz, 30, 22),
                   c(21, 29))
  ak <- c("2025-11-02T00:00:00-0800", "2025-11-02T00:30:00-0800",
          "2025-11-02T05:00:00-0900")
  expect_identical(.raw.sleep.dst.check(ak, list(SPTE_start = 2, SPTE_end = 3),
                                        "America/Anchorage", 30, 22), c(22, 29))
  # Lord Howe's clock change is half an hour
  lh <- c("2017-10-01T00:00:00+1030", "2017-10-01T01:00:00+1030",
          "2017-10-01T05:00:00+1100")
  expect_identical(.raw.sleep.dst.check(lh, list(SPTE_start = 2, SPTE_end = 3),
                                        "Australia/Lord_Howe", 30, 22), c(22, 30.5))
  # the offsets are re-read in tz, not taken from the strings
  expect_identical(format(.raw.iso8601.to.posix(lh, "Australia/Lord_Howe"), "%z"),
                   c("+1030", "+1030", "+1100"))
})

test_that(".raw.sleep.fix.na replaces NA with zero and leaves everything else alone", {
  expect_identical(.raw.sleep.fix.na(c(1, NA, 3)), c(1, 0, 3))
  expect_identical(.raw.sleep.fix.na(c(1, 2, 3)), c(1, 2, 3))
  expect_identical(.raw.sleep.fix.na(numeric(0)), numeric(0))
})

test_that(".raw.sleep.marker.impute copies presses to the other days with a falling weight", {
  MARKER <- rep(0, 4320); MARKER[c(100, 1600)] <- 1     # 60 s epochs, 1440 per day
  out <- .raw.sleep.marker.impute(MARKER, countmidn = 2, ws3 = 60,
                                  impute_marker_button = TRUE)
  expect_identical(which(out != 0), c(100L, 160L, 1540L, 1600L, 2980L, 3040L))
  expect_identical(out[c(100, 1600)], c(1, 1))
  expect_identical(out[c(160, 1540, 3040)], rep(0.45, 3))   # one day away
  expect_identical(out[2980], 0.3)                      # two days away
  expect_identical(.raw.sleep.marker.impute(MARKER, 2, 60, FALSE), MARKER)
  expect_null(.raw.sleep.marker.impute(NULL, 2, 60, TRUE))
})

test_that("a synthetic three-night recording at 60 s epochs is identical() to GGIR", {
  skip_if_no_ggir()
  s <- det_synth()
  o <- raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz))
  expect_identical(as.ggir.SLE(o), det_synth_ggir(s))
  expect_identical(nrow(o$windows), 3L)
  expect_identical(o$part3_guider, rep("HDCZA", 3))
  expect_identical(round(o$SPTE_start, 6), c(NA, 23.016667, 23.016667))
  expect_identical(o$settings$windowsizes, c(60, 900, 3600))
})

test_that("the acceleration metric falls back to ExtAct and otherwise stops, as GGIR does", {
  skip_if_no_ggir()
  s <- det_synth()
  s2 <- s; colnames(s2$IMP$metashort)[3] <- "ExtAct"
  o <- raw.sib.detect(meta = s2$M, imputed = s2$IMP, params = raw.params(desiredtz = s2$tz))
  expect_identical(o$settings$acc.metric, "ExtAct")
  expect_identical(as.ggir.SLE(o), det_synth_ggir(s2))
  s3 <- s; colnames(s3$IMP$metashort)[3] <- "MAD"
  expect_error(raw.sib.detect(meta = s3$M, imputed = s3$IMP,
                              params = raw.params(desiredtz = s3$tz)),
               "Argument acc.metric is set to ENMO but not found in GGIR part 1 output data",
               fixed = TRUE)
})

test_that("a count-based algorithm needs its zero-crossing column, and errors without it", {
  skip_if_no_ggir()
  s <- det_synth()
  P <- GGIR::load_params(topic = "sleep")$params_sleep; P$HASIB.algo <- "Sadeh1994"
  # GGIR raises "undefined columns selected" from metashort[, NA]
  expect_error(raw.sib.detect(meta = s$M, imputed = s$IMP,
                              params = raw.params(desiredtz = s$tz,
                                                  HASIB.algo = "Sadeh1994")),
               "undefined columns selected")
  expect_error(det_synth_ggir(s, params_sleep = P), "undefined columns selected")
  sz <- det_synth(extra = list(ZCY = rep(c(0, 40), length.out = 4320)))
  o <- raw.sib.detect(meta = sz$M, imputed = sz$IMP,
                      params = raw.params(desiredtz = sz$tz, HASIB.algo = "Sadeh1994"))
  expect_identical(as.ggir.SLE(o), det_synth_ggir(sz, params_sleep = P))
  expect_identical(o$definitions, "Sadeh1994_ZC")
})

test_that("a non-default twd is honoured and can skip every night", {
  skip_if_no_ggir()
  s <- det_synth()
  # a 3 minute window fails the 60 epoch test at 60 s epochs
  o <- raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz),
                      twd = c(-12, -11.95))
  expect_identical(as.ggir.SLE(o), det_synth_ggir(s, twd = c(-12, -11.95)))
  expect_true(all(o$windows$skipped))
  expect_identical(sort(unique(o$output$night)), 0)
  expect_identical(o$L5list, rep(NA, 3))
  expect_identical(o$settings$twd, c(-12, -11.95))
})

test_that("the external-function branch raises a named error instead of being half ported", {
  s <- det_synth()
  err <- tryCatch(raw.sib.detect(meta = s$M, imputed = s$IMP,
                                 params = raw.params(desiredtz = s$tz, HASIB.algo = "data")),
                  error = function(e) e)
  expect_s3_class(err, "canhrActi_raw_sleep_error")
  expect_match(conditionMessage(err), "applyExtFunction", fixed = TRUE)
  expect_match(conditionMessage(err), "not ported", fixed = TRUE)
})

test_that("the argument guards fire before anything is computed", {
  s <- det_synth()
  expect_error(raw.sib.detect(s$M, s$IMP, twd = 1), "twd must be two numbers")
  expect_error(raw.sib.detect(s$M, s$IMP, progress = 1), "progress must be NULL")
  bad <- s$IMP; bad$rout <- bad$rout[, 1:3]
  expect_error(raw.sib.detect(s$M, bad), "five columns")
  bad2 <- s$IMP; colnames(bad2$metashort)[1] <- "tijd"
  expect_error(raw.sib.detect(s$M, bad2), "timestamp")
  expect_error(raw.sib.detect(list(metashort = 1)), "windowsizes")
  expect_error(raw.sib.detect(s$M, list(metashort = NULL)), "IMPUTED series")
  bad3 <- s$IMP; bad3$rout <- NULL
  expect_error(raw.sib.detect(s$M, bad3), "no wear decision matrix")
})

test_that("the progress callback is called once per night", {
  s <- det_synth()
  seen <- list()
  p <- function(stage, i, n, message) seen[[length(seen) + 1]] <<- list(stage, i, n, message)
  raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz),
                 progress = p)
  expect_identical(length(seen), 3L)
  expect_identical(seen[[1]][[1]], "sib_detect")
  # i and n are integers, as in the other progress emitters
  expect_identical(vapply(seen, function(z) z[[2]], integer(1)), 1:3)
  expect_identical(seen[[1]][[3]], 3L)
})

test_that("the LC_TIME locale is restored, including after an error", {
  s <- det_synth()
  before <- Sys.getlocale("LC_TIME")
  raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz))
  expect_identical(Sys.getlocale("LC_TIME"), before)
  bad <- s$IMP; bad$rout <- bad$rout[, 1:3]
  expect_error(raw.sib.detect(s$M, bad), "rout needs five columns")
  expect_identical(Sys.getlocale("LC_TIME"), before)
})

test_that("as.ggir.SLE returns GGIR's shape and refuses anything else", {
  s <- det_synth()
  o <- raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz))
  expect_s3_class(o, "canhrActi_raw_sib")
  expect_true(is.character(o$output$time))
  g <- as.ggir.SLE(o)
  expect_s3_class(g$output$time, "factor")     # GGIR's stringsAsFactors = TRUE shape
  expect_identical(levels(g$output$time), sort(unique(o$output$time)))
  expect_identical(names(g), c("output", "detection.failed", "L5list", "SPTE_end",
                               "SPTE_start", "tib.threshold", "longitudinal_axis",
                               "part3_guider"))
  expect_identical(as.ggir.SLE(structure(list(sleep = list(sib = o)),
                                         class = "canhrActi_raw")), g)
  expect_error(as.ggir.SLE(list()), "canhrActi_raw_sib")
})

test_that("the object carries the canhrActi additions the rest of part 3 needs", {
  s <- det_synth()
  o <- raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz))
  expect_identical(o$rec_starttime, s$IMP$metashort$timestamp[1])
  expect_identical(o$n_short, 4320L)
  expect_identical(o$n_invalid_short, 0L)
  expect_identical(o$definitions, "T5A5")
  expect_identical(o$desiredtz, "UTC")
  expect_identical(o$status, "ok")
  expect_identical(o$settings$ggir_exact, TRUE)
  expect_identical(o$settings$params_sleep$HASPT.algo, "HDCZA")
  expect_identical(colnames(o$windows),
                   c("night", "midnight", "qqq1_unclamped", "qqq2_unclamped", "qqq1", "qqq2",
                     "skipped", "start.time", "end.time", "n_epochs", "n_invalid",
                     "nonwear_percentage", "guider_to_use", "partial_first_day",
                     "daysleep_offset"))
})

test_that("the print method names the file, the nights and the per-night vectors", {
  s <- det_synth()
  o <- raw.sib.detect(meta = s$M, imputed = s$IMP, params = raw.params(desiredtz = s$tz))
  # called directly: NAMESPACE has no S3method line for it yet
  txt <- capture.output(print.canhrActi_raw_sib(o))
  expect_true(any(grepl("noon-to-noon windows from 3 midnights", txt)))
  expect_true(any(grepl("definitions: T5A5", txt)))
  expect_true(any(grepl("SPTE start:  NA 23.017 23.017", txt)))
  o2 <- o; o2$detection.failed <- TRUE
  expect_true(any(grepl("failed, no nights", capture.output(print.canhrActi_raw_sib(o2)))))
})

test_that("the detector output feeds raw.sib.summary once the crude estimate is dropped", {
  cc <- det_case("MOS2")
  o <- cc$ours
  expect_error(raw.sib.summary(o, cc$d$M, desiredtz = cc$d$tz), "spt_crude_estimate")
  o2 <- o
  o2$output <- o$output[, -which(names(o$output) == "spt_crude_estimate")]
  got <- raw.sib.summary(o2, cc$d$M, ignorenonwear = TRUE, desiredtz = cc$d$tz)
  expect_identical(got, cc$d$ms3$sib.cla.sum)
  expect_identical(dim(got), c(111L, 9L))
})
