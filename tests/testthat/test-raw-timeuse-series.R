# Parity tests for R/raw_timeuse_series.R against GGIR's g.part5_initialise_ts,
# g.part5.handle_lux_extremes and g.part5.addsib. Reference data live in the folder
# named by CANHRACTI_GGIR_REF, the autumn fixture one level up under
# ggir-study-p34/fixtures; both skip when absent, as do the live comparisons when
# GGIR is not installed. The MOS2 milestones were stored in the system zone of a machine
# set to America/Anchorage, so the file runs in that zone.

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

# GGIR strips the dots out of the recording name for the part-5 series file.
TS_CASES <- list(
  MOS2 = list(out = c("out", "output_din"), fn = "MOS2E39230594.gt3x",
              raw = "MOS2E39230594_T5A5.RData"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x",
              raw = "EE_left_2952017-05-30_T5A5.RData"))

ts_path <- function(case, what) {
  cc <- TS_CASES[[case]]
  p <- function(...) do.call(ref_file, c(as.list(cc$out), list(...)))
  switch(what,
         basic  = p("meta", "basic", paste0("meta_", cc$fn, ".RData")),
         ms2    = p("meta", "ms2.out", paste0(cc$fn, ".RData")),
         ms3    = p("meta", "ms3.out", paste0(cc$fn, ".RData")),
         ms5raw = p("meta", "ms5.outraw", "40_100_400", cc$raw))
}

# The milestones one recording needs, plus the midnight indices and the sib table
# cut to one definition, derived as g.part5 does before it calls addsib.
ts_load <- function(basic, ms2, ms3, ms5raw = NULL) {
  e1 <- new.env(parent = emptyenv()); load(basic, envir = e1)
  e2 <- new.env(parent = emptyenv()); load(ms2, envir = e2)
  e3 <- new.env(parent = emptyenv()); load(ms3, envir = e3)
  mdat <- NULL
  if (!is.null(ms5raw) && file.exists(ms5raw)) {
    e5 <- new.env(parent = emptyenv()); load(ms5raw, envir = e5)
    mdat <- e5$mdat
  }
  tz <- e1$desiredtz_part1
  ts <- .raw.timeuse.init.ts(e2$IMP, e1$M, longitudinal_axis = e3$longitudinal_axis)
  time_POSIX <- .raw.iso8601.to.posix(ts$time, tz = tz)
  tempp <- as.POSIXlt(time_POSIX)
  nightsi <- which(tempp$sec == 0 & tempp$min == 0 & tempp$hour == 0)
  S <- e3$sib.cla.sum
  cut <- which(S$fraction.night.invalid > 0.9 | S$nsib.periods == 0)
  if (length(cut) > 0) S <- S[-cut, ]
  list(IMP = e2$IMP, M = e1$M, ms3 = e3, tz = tz, ts = ts, mdat = mdat, nightsi = nightsi,
       S = S, def = unique(S$definition), timenum = as.numeric(time_POSIX))
}

ts_ref <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref()
    pb <- ts_path(case, "basic");  skip_if_no_file(pb)
    p2 <- ts_path(case, "ms2");    skip_if_no_file(p2)
    p3 <- ts_path(case, "ms3");    skip_if_no_file(p3)
    p5 <- ts_path(case, "ms5raw")
    out <- ts_load(pb, p2, p3, p5)
    assign(case, out, envir = cache)
    out
  }
})

# A minimal IMP/M pair, in the shape g.part5_initialise_ts reads them.
ts_synth <- function(n = 1440, ws = c(5, 900, 3600), short = c("anglez", "ENMO"),
                     long = character(), tz = "Europe/Helsinki",
                     start = "2021-06-01 12:00:00", seed = 7) {
  st <- as.POSIXct(start, tz = tz)
  nlong <- ceiling(n / (ws[2] / ws[1]))
  set.seed(seed)
  ms <- data.frame(timestamp = strftime(seq(st, by = ws[1], length.out = n),
                                        format = "%Y-%m-%dT%H:%M:%S%z", tz = tz),
                   stringsAsFactors = FALSE)
  for (cc in short) ms[[cc]] <- round(runif(n, -90, 90), 4)
  ml <- data.frame(timestamp = strftime(seq(st, by = ws[2], length.out = nlong),
                                        format = "%Y-%m-%dT%H:%M:%S%z", tz = tz),
                   stringsAsFactors = FALSE)
  for (cc in long) ml[[cc]] <- round(runif(nlong, 0, 200000), 3)
  rout <- data.frame(r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = rbinom(nlong, 1, 0.3))
  list(IMP = list(metashort = ms, rout = rout, windowsizes = ws),
       M = list(metalong = ml, metashort = ms))
}

expect_init_ts_same_as_ggir <- function(got, d, acc_metric = "ENMO", sensor_location = "wrist",
                                        lux_cal_constant = c(), lux_cal_exponent = c(),
                                        longitudinal_axis = NULL) {
  if (!requireNamespace("GGIR", quietly = TRUE)) return(invisible(NULL))
  P <- GGIR::load_params(topic = c("general", "247"))
  pg <- P$params_general
  pg[["acc.metric"]] <- acc_metric
  pg[["sensor.location"]] <- sensor_location
  p247 <- P$params_247
  p247[["LUX_cal_constant"]] <- lux_cal_constant
  p247[["LUX_cal_exponent"]] <- lux_cal_exponent
  ggir <- GGIR:::g.part5_initialise_ts(d$IMP, d$M, p247, pg,
                                       longitudinal_axis = longitudinal_axis)
  expect_identical(got, ggir)
}

expect_addsib_same_as_ggir <- function(got, ts, epochSize, part3_output, desiredtz,
                                       sibDefinition, nightsi) {
  if (!requireNamespace("GGIR", quietly = TRUE)) return(invisible(NULL))
  ggir <- GGIR:::g.part5.addsib(ts, epochSize = epochSize, part3_output = part3_output,
                                desiredtz = desiredtz, sibDefinition = sibDefinition,
                                nightsi = nightsi)
  expect_identical(got, ggir)
}

# A synthetic series in the shape init.ts returns, for the addsib tests.
addsib_synth <- function(n = 20000, start = "2021-06-01 22:00:00", tz = "Europe/Helsinki",
                         ws3 = 5, na_at = NULL, seed = 11) {
  st <- as.POSIXct(start, tz = tz)
  set.seed(seed)
  ang <- round(cumsum(rnorm(n, 0, 2)), 4)
  if (!is.null(na_at)) ang[na_at] <- NA
  data.frame(time = strftime(seq(st, by = ws3, length.out = n),
                             format = "%Y-%m-%dT%H:%M:%S%z", tz = tz),
             ACC = round(runif(n, 0, 300), 4), guider = "unknown", angle = ang, nonwear = 0,
             stringsAsFactors = FALSE)
}
addsib_nights <- function(ts, tz = "Europe/Helsinki") {
  p <- as.POSIXlt(.raw.iso8601.to.posix(ts$time, tz = tz))
  which(p$sec == 0 & p$min == 0 & p$hour == 0)
}
addsib_p3 <- function(ts, starts, lens, definition = "T5A5") {
  data.frame(definition = definition, sib.onset.time = ts$time[starts],
             sib.end.time = ts$time[starts + lens], stringsAsFactors = FALSE)
}

test_that("P1a MOS2: the 118800 x 5 time series is identical to GGIR's", {
  r <- ts_ref("MOS2")
  expect_identical(r$tz, "")
  expect_identical(r$IMP$windowsizes, c(5, 900, 3600))

  ts <- r$ts
  expect_identical(dim(ts), c(118800L, 5L))
  expect_identical(names(ts), c("time", "ACC", "guider", "angle", "nonwear"))
  expect_identical(unname(sapply(ts, class)),
                   c("character", "numeric", "character", "numeric", "numeric"))
  # ACC is in milli-g
  expect_identical(ts$ACC, r$IMP$metashort$ENMO * 1000)
  expect_identical(ts$angle, r$IMP$metashort$anglez)
  expect_identical(ts$time, r$IMP$metashort$timestamp)
  expect_true(all(ts$guider == "unknown"))
  # 660 long epochs, 180 short epochs each
  expect_identical(ts$nonwear, rep(r$IMP$rout[, 5], each = 180))
  expect_identical(sum(ts$nonwear), 65880)
  expect_identical(ts$time[1], "2025-10-07T20:30:00-0800")

  skip_if_no_ggir()
  expect_init_ts_same_as_ggir(ts, list(IMP = r$IMP, M = r$M),
                              longitudinal_axis = r$ms3$longitudinal_axis)
})

test_that("P1b EE: the 120960 x 5 time series is identical to GGIR's", {
  r <- ts_ref("EE")
  expect_identical(r$tz, "Europe/Helsinki")

  ts <- r$ts
  expect_identical(dim(ts), c(120960L, 5L))
  expect_identical(names(ts), c("time", "ACC", "guider", "angle", "nonwear"))
  expect_identical(ts$ACC, r$IMP$metashort$ENMO * 1000)
  expect_identical(ts$angle, r$IMP$metashort$anglez)
  expect_identical(ts$nonwear, rep(r$IMP$rout[, 5], each = 180))
  expect_identical(sum(ts$nonwear), 19980)
  expect_identical(ts$time[1], "2017-05-23T09:15:00+0300")

  skip_if_no_ggir()
  expect_init_ts_same_as_ggir(ts, list(IMP = r$IMP, M = r$M),
                              longitudinal_axis = r$ms3$longitudinal_axis)
})

test_that("P1c a count metric is scaled by 1 and everything else by 1000", {
  metrics <- c(ENMO = 1000, MAD = 1000, LFENMO = 1000, NeishabouriCount_y = 1,
               ZCX = 1, BrondCount_x = 1, ExtAct = 1, ExtHeartRate = 1)
  for (m in names(metrics)) {
    d <- ts_synth(n = 360, short = c("anglez", m))
    got <- .raw.timeuse.init.ts(d$IMP, d$M, acc_metric = m)
    expect_identical(got$ACC, d$IMP$metashort[[m]] * metrics[[m]],
                     info = paste("metric", m))
    expect_init_ts_same_as_ggir(got, d, acc_metric = m)
  }
  # the regexp is anchored at the start, so a name that merely contains "ZC" is a g-unit metric
  d <- ts_synth(n = 360, short = c("anglez", "myZCmetric"))
  got <- .raw.timeuse.init.ts(d$IMP, d$M, acc_metric = "myZCmetric")
  expect_identical(got$ACC, d$IMP$metashort$myZCmetric * 1000)
  expect_init_ts_same_as_ggir(got, d, acc_metric = "myZCmetric")
})

test_that("P1c an absent acceleration column is refused with a readable message", {
  d <- ts_synth(n = 360)
  expect_error(.raw.timeuse.init.ts(d$IMP, d$M, acc_metric = "ZCX"),
               "no column named ZCX")
  expect_error(.raw.timeuse.init.ts(list(metashort = d$IMP$metashort), d$M),
               "GGIR IMP list")
  expect_error(.raw.timeuse.init.ts(d$IMP, list(metashort = d$M$metashort)),
               "GGIR M list")
})

test_that("init.ts accepts a canhrActi_raw and a canhrActi_raw_imputed as well as GGIR lists", {
  d <- ts_synth(n = 720, long = c("lightpeak", "temperaturemean"))
  plain <- .raw.timeuse.init.ts(d$IMP, d$M)
  imp <- structure(d$IMP, class = "canhrActi_raw_imputed")
  meta <- structure(d$M, class = "canhrActi_raw_meta")
  expect_identical(.raw.timeuse.init.ts(imp, meta), plain)
  x <- structure(list(meta = meta, imputed = imp), class = "canhrActi_raw")
  expect_identical(.raw.timeuse.init.ts(x), plain)
  expect_identical(.raw.timeuse.init.ts(x, meta), plain)
})

test_that("P1d ACC matches the exported mdat only after rounding to 3 decimals", {
  for (case in c("MOS2", "EE")) {
    r <- ts_ref(case)
    if (is.null(r$mdat)) next
    # the exported series is in timenum order; on both recordings that is row order
    expect_identical(nrow(r$mdat), nrow(r$ts))
    expect_identical(round(r$ts$ACC, 3), r$mdat$ACC)
    expect_false(identical(r$ts$ACC, r$mdat$ACC))
    expect_identical(r$ts$angle, r$mdat$angle)
    expect_identical(r$ts$nonwear, r$mdat$invalidepoch)
  }
})

test_that("the optional short-epoch channels are attached in GGIR's order", {
  d <- ts_synth(n = 720, short = c("anglex", "angley", "anglez", "ENMO"))
  d$IMP$metashort$step_count <- rep(c(0L, 3L, 17L), length.out = 720)
  d$IMP$metashort$marker <- rep(c(0L, 1L), length.out = 720)
  d$M$metashort <- d$IMP$metashort
  got <- .raw.timeuse.init.ts(d$IMP, d$M)
  expect_identical(names(got), c("time", "ACC", "guider", "angle", "anglex", "angley",
                                 "step_count", "nonwear", "marker"))
  expect_identical(got$angle, d$IMP$metashort$anglez)   # angle is anglez, not repeated again
  expect_identical(got$anglex, d$IMP$metashort$anglex)
  expect_identical(got$angley, d$IMP$metashort$angley)
  expect_identical(got$step_count, d$IMP$metashort$step_count)
  expect_identical(got$marker, d$IMP$metashort$marker)
  expect_init_ts_same_as_ggir(got, d)
})

test_that("the marker test reads the part-1 table and the copy reads the imputed one", {
  # a marker column in M$metashort that the imputed table lacks is created and deleted again
  d <- ts_synth(n = 360)
  d$M$metashort$marker <- 1L
  got <- .raw.timeuse.init.ts(d$IMP, d$M)
  expect_false("marker" %in% names(got))
  expect_init_ts_same_as_ggir(got, d)
  # the mirror case: a marker only in the imputed table is ignored
  d2 <- ts_synth(n = 360)
  d2$IMP$metashort$marker <- 1L
  got2 <- .raw.timeuse.init.ts(d2$IMP, d2$M)
  expect_false("marker" %in% names(got2))
  expect_init_ts_same_as_ggir(got2, d2)
})

test_that("light and temperature are repeated out from the long epochs", {
  d <- ts_synth(n = 1440, long = c("lightpeak", "temperaturemean"))
  got <- .raw.timeuse.init.ts(d$IMP, d$M)
  expect_identical(names(got), c("time", "ACC", "guider", "angle", "nonwear", "lightpeak",
                                 "lightpeak_imputationcode", "temperature"))
  expect_identical(got$temperature, rep(d$M$metalong$temperaturemean, each = 180))
  lx <- .raw.timeuse.lux.extremes(d$M$metalong$lightpeak)
  expect_identical(got$lightpeak, rep(lx$lux, each = 180))
  expect_identical(got$lightpeak_imputationcode, rep(lx$correction_log, each = 180))
  expect_init_ts_same_as_ggir(got, d)
  # recalibration happens before the extremes are handled
  got2 <- .raw.timeuse.init.ts(d$IMP, d$M, lux_cal_constant = 0.5, lux_cal_exponent = 0.0002)
  lx2 <- .raw.timeuse.lux.extremes(0.5 * exp(0.0002 * d$M$metalong$lightpeak))
  expect_identical(got2$lightpeak, rep(lx2$lux, each = 180))
  expect_false(identical(got$lightpeak, got2$lightpeak))
  expect_init_ts_same_as_ggir(got2, d, lux_cal_constant = 0.5, lux_cal_exponent = 0.0002)
  # one empty calibration member is enough to skip the recalibration entirely
  got3 <- .raw.timeuse.init.ts(d$IMP, d$M, lux_cal_constant = 0.5)
  expect_identical(got3$lightpeak, got$lightpeak)
  expect_init_ts_same_as_ggir(got3, d, lux_cal_constant = 0.5)
})

test_that("the non-wear vector is truncated or zero padded to the number of short epochs", {
  d <- ts_synth(n = 1500)                     # 9 long epochs give 1620 values for 1500 epochs
  got <- .raw.timeuse.init.ts(d$IMP, d$M)
  expect_identical(length(got$nonwear), 1500L)
  expect_identical(got$nonwear, rep(d$IMP$rout[, 5], each = 180)[1:1500])
  expect_init_ts_same_as_ggir(got, d)

  d2 <- ts_synth(n = 1440)
  d2$IMP$rout <- d2$IMP$rout[1:4, ]           # 720 values for 1440 epochs
  got2 <- .raw.timeuse.init.ts(d2$IMP, d2$M)
  expect_identical(got2$nonwear, c(rep(d2$IMP$rout[, 5], each = 180), rep(0, 720)))
  expect_init_ts_same_as_ggir(got2, d2)
})

test_that("a hip sensor takes the longitudinal axis, with GGIR's arm-selection defect", {
  d <- ts_synth(n = 720, short = c("anglex", "angley", "anglez", "ENMO"))
  g1 <- .raw.timeuse.init.ts(d$IMP, d$M, sensor_location = "hip", longitudinal_axis = 1)
  expect_identical(g1$angle, d$IMP$metashort$anglex)
  expect_identical(names(g1), c("time", "ACC", "guider", "angle", "angley", "anglez", "nonwear"))
  expect_init_ts_same_as_ggir(g1, d, sensor_location = "hip", longitudinal_axis = 1)
  # axis 2 gives angley, axis 3 gives anglez, both selected on an "anglex" presence test
  g2 <- .raw.timeuse.init.ts(d$IMP, d$M, sensor_location = "hip", longitudinal_axis = 2)
  expect_identical(g2$angle, d$IMP$metashort$angley)
  expect_init_ts_same_as_ggir(g2, d, sensor_location = "hip", longitudinal_axis = 2)
  g3 <- .raw.timeuse.init.ts(d$IMP, d$M, sensor_location = "hip", longitudinal_axis = 3)
  expect_identical(g3$angle, d$IMP$metashort$anglez)
  expect_init_ts_same_as_ggir(g3, d, sensor_location = "hip", longitudinal_axis = 3)
  gw <- .raw.timeuse.init.ts(d$IMP, d$M, sensor_location = "wrist", longitudinal_axis = 2)
  expect_identical(gw$angle, d$IMP$metashort$anglez)
  expect_identical(names(gw), c("time", "ACC", "guider", "angle", "anglex", "angley", "nonwear"))
  expect_init_ts_same_as_ggir(gw, d, sensor_location = "wrist", longitudinal_axis = 2)
})

test_that("a table with no angle column gives the four-column series, or GGIR's error", {
  d <- ts_synth(n = 360, short = "ENMO")
  got <- .raw.timeuse.init.ts(d$IMP, d$M)
  expect_identical(names(got), c("time", "ACC", "guider", "nonwear"))
  expect_init_ts_same_as_ggir(got, d)
  # anglex present, anglez absent, no axis: GGIR evaluates NULL == 1 and errors; reproduced
  d2 <- ts_synth(n = 360, short = c("anglex", "ENMO"))
  expect_error(.raw.timeuse.init.ts(d2$IMP, d2$M), "missing value where TRUE/FALSE needed")
  if (requireNamespace("GGIR", quietly = TRUE)) {
    P <- GGIR::load_params(topic = c("general", "247"))
    expect_error(GGIR:::g.part5_initialise_ts(d2$IMP, d2$M, P$params_247, P$params_general),
                 "missing value where TRUE/FALSE needed")
  }
})

test_that("P1e eight isolated spikes are imputed and a run of four is blanked", {
  lux <- rep(c(100, 200, 300, 400), length.out = 40)
  spikes <- c(3, 7, 11, 15, 19, 23, 27, 31)
  lux[spikes] <- 150000
  run <- 35:38
  lux[run] <- 200000
  got <- .raw.timeuse.lux.extremes(lux)

  expect_identical(got$correction_log[run], rep(2, 4))
  expect_true(all(is.na(got$lux[run])))
  expect_identical(got$correction_log[spikes], rep(1, 8))
  expect_identical(got$lux[spikes], (lux[spikes - 1] + lux[spikes + 1]) / 2)
  untouched <- setdiff(seq_along(lux), c(spikes, run))
  expect_identical(got$lux[untouched], lux[untouched])
  expect_identical(got$correction_log[untouched], rep(0, length(untouched)))
  expect_identical(sum(got$correction_log), 8 + 8)
  expect_identical(got$lux[3], 300)     # neighbours 200 and 400
  expect_identical(got$lux[7], 300)     # neighbours 200 and 400
  expect_identical(got$lux[31], 300)    # neighbours 200 and 400

  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(got, GGIR:::g.part5.handle_lux_extremes(lux))
  }
})

test_that("P1e the lux edge cases follow GGIR exactly", {
  lux <- c(150000, 100, 200, 300, 150000)
  got <- .raw.timeuse.lux.extremes(lux)
  expect_identical(got$correction_log, c(1, 0, 0, 0, 1))
  expect_identical(got$lux[1], 100)     # mean of (NA, 100) with na.rm
  expect_identical(got$lux[5], 300)     # mean of (300, NA) with na.rm
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(got, GGIR:::g.part5.handle_lux_extremes(lux))
  }
  # two adjacent extremes are not a run; each is imputed, the first seeing the second blanked
  lux2 <- c(100, 150000, 150000, 400, 500)
  got2 <- .raw.timeuse.lux.extremes(lux2)
  expect_identical(got2$correction_log, c(0, 1, 1, 0, 0))
  expect_identical(got2$lux[2], 100)    # mean of (100, NA)
  expect_identical(got2$lux[3], 250)    # mean of (100 imputed above, NA, 400)
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(got2, GGIR:::g.part5.handle_lux_extremes(lux2))
  }
  # a value exactly on the threshold is not extreme, and a series with none is untouched
  lux3 <- c(120000, 119999, 100)
  got3 <- .raw.timeuse.lux.extremes(lux3)
  expect_identical(got3$lux, lux3)
  expect_identical(got3$correction_log, c(0, 0, 0))
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(got3, GGIR:::g.part5.handle_lux_extremes(lux3))
  }
  lux4 <- rep(200000, 6)
  got4 <- .raw.timeuse.lux.extremes(lux4)
  expect_true(all(is.na(got4$lux)))
  expect_identical(got4$correction_log, rep(2, 6))
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(got4, GGIR:::g.part5.handle_lux_extremes(lux4))
  }
})

test_that("P2a MOS2 sibdetection sums to 24478 and is identical to the stored series", {
  r <- ts_ref("MOS2")
  expect_identical(r$def, "T5A5")
  expect_identical(nrow(r$S), 111L)

  got <- .raw.timeuse.addsib(r$ts, epochSize = 5, part3_output = r$S[r$S$definition == "T5A5", ],
                             desiredtz = r$tz, sibDefinition = "T5A5", nightsi = r$nightsi)
  expect_identical(names(got), c(names(r$ts), "sibdetection"))
  expect_identical(sum(got$sibdetection), 24478)
  expect_identical(sort(unique(got$sibdetection)), c(0, 1))
  if (!is.null(r$mdat)) expect_identical(got$sibdetection, r$mdat$sibdetection)
  expect_identical(r$nightsi, c(2521L, 19801L, 37081L, 54361L, 71641L, 88921L, 106201L))

  skip_if_no_ggir()
  expect_addsib_same_as_ggir(got, r$ts, 5, r$S[r$S$definition == "T5A5", ], r$tz, "T5A5",
                             r$nightsi)
})

test_that("P2b MOS2 part B rewrites 875 epochs of the first-midnight window", {
  r <- ts_ref("MOS2")
  p3 <- r$S[r$S$definition == "T5A5", ]
  with_redo <- .raw.timeuse.addsib(r$ts, 5, p3, r$tz, "T5A5", r$nightsi)
  # a definition without both an A and a T skips part B
  no_redo <- .raw.timeuse.addsib(r$ts, 5, p3, r$tz, "L5", r$nightsi)

  expect_identical(sum(with_redo$sibdetection), 24478)
  expect_identical(sum(no_redo$sibdetection), 23805)
  redo1 <- r$nightsi[1] - (60 / 5) * 60
  redo2 <- r$nightsi[1] + 14 * (60 / 5) * 60
  expect_identical(c(redo1, redo2), c(1801, 12601))
  expect_identical(sum(with_redo$sibdetection != no_redo$sibdetection), 875L)
  expect_identical(sum(no_redo$sibdetection[redo1:redo2]), 6141)
  expect_identical(sum(with_redo$sibdetection[redo1:redo2]), 6814)
  outside <- setdiff(seq_len(nrow(r$ts)), redo1:redo2)
  expect_identical(with_redo$sibdetection[outside], no_redo$sibdetection[outside])
  # the thresholds are parsed out of the definition string
  t2a2 <- .raw.timeuse.addsib(r$ts, 5, p3, r$tz, "T2A2", r$nightsi)
  expect_false(identical(t2a2$sibdetection[redo1:redo2],
                         with_redo$sibdetection[redo1:redo2]))

  skip_if_no_ggir()
  expect_addsib_same_as_ggir(no_redo, r$ts, 5, p3, r$tz, "L5", r$nightsi)
  expect_addsib_same_as_ggir(t2a2, r$ts, 5, p3, r$tz, "T2A2", r$nightsi)
})

test_that("P2a EE sibdetection sums to 37691 and is identical to the stored series", {
  r <- ts_ref("EE")
  p3 <- r$S[r$S$definition == "T5A5", ]
  expect_identical(nrow(p3), 132L)
  got <- .raw.timeuse.addsib(r$ts, 5, p3, r$tz, "T5A5", r$nightsi)
  no_redo <- .raw.timeuse.addsib(r$ts, 5, p3, r$tz, "L5", r$nightsi)

  expect_identical(sum(got$sibdetection), 37691)
  expect_identical(sum(no_redo$sibdetection), 37808)   # here part B removes sib epochs
  expect_identical(sum(got$sibdetection != no_redo$sibdetection), 117L)
  expect_identical(c(r$nightsi[1] - 720, r$nightsi[1] + 10080), c(9901, 20701))
  if (!is.null(r$mdat)) expect_identical(got$sibdetection, r$mdat$sibdetection)

  skip_if_no_ggir()
  expect_addsib_same_as_ggir(got, r$ts, 5, p3, r$tz, "T5A5", r$nightsi)
})

test_that("addsib matches the right occurrence of an autumn repeated hour", {
  skip_if_no_ggir_ref()
  fx <- p34_fixture("autumn")
  pb <- file.path(fx, "meta", "basic", "meta_EEautumn.gt3x.RData")
  p2 <- file.path(fx, "meta", "ms2.out", "EEautumn.gt3x.RData")
  p3f <- file.path(fx, "meta", "ms3.out", "EEautumn.gt3x.RData")
  p5 <- file.path(fx, "meta", "ms5.outraw", "40_100_400", "EEautumn_T5A5.RData")
  skip_if_no_file(pb); skip_if_no_file(p2); skip_if_no_file(p3f)

  r <- ts_load(pb, p2, p3f, p5)
  expect_identical(r$tz, "Europe/Helsinki")
  p3 <- r$S[r$S$definition == "T5A5", ]
  got <- .raw.timeuse.addsib(r$ts, 5, p3, r$tz, "T5A5", r$nightsi)

  # the doubled hour: 720 epochs whose local clock string repeats, with different UTC offsets
  local_clock <- format(.raw.iso8601.to.posix(r$ts$time, tz = r$tz), "%Y-%m-%d %H:%M:%S")
  dup <- which(duplicated(local_clock))
  expect_identical(length(dup), 720L)
  expect_identical(range(dup), c(82621L, 83340L))
  expect_identical(r$ts$time[81901], "2017-10-29T03:00:00+0300")
  expect_identical(r$ts$time[82621], "2017-10-29T03:00:00+0200")
  expect_identical(local_clock[81901], local_clock[82621])

  # the two bouts that straddle the transition, matched on the ISO strings
  expect_identical(p3$sib.onset.time[101], "2017-10-29T03:44:30+0300")
  expect_identical(p3$sib.end.time[101], "2017-10-29T03:44:20+0200")
  expect_identical(p3$sib.onset.time[102], "2017-10-29T03:44:30+0200")
  expect_identical(match(p3$sib.onset.time[101], r$ts$time), 82435L)
  expect_identical(match(p3$sib.end.time[101], r$ts$time), 83153L)
  expect_identical(match(p3$sib.onset.time[102], r$ts$time), 83155L)
  expect_identical(match(p3$sib.end.time[102], r$ts$time), 83783L)
  # a local-clock comparison would put bout 102's onset 720 epochs early, on top of bout 101
  naive <- match(format(.raw.iso8601.to.posix(p3$sib.onset.time[102], tz = r$tz),
                        "%Y-%m-%d %H:%M:%S"), local_clock)
  expect_identical(naive, 82435L)

  expect_true(all(got$sibdetection[82435:83153] == 1))
  expect_identical(got$sibdetection[83154], 0)
  expect_true(all(got$sibdetection[83155:83783] == 1))
  expect_identical(sum(got$sibdetection), 37691)

  # the fixture's exported series is the valid-epoch subset, matched by timenum
  if (!is.null(r$mdat)) {
    idx <- match(r$mdat$timenum, r$timenum)
    expect_false(anyNA(idx))
    expect_identical(nrow(r$mdat), 86477L)
    expect_identical(got$sibdetection[idx], r$mdat$sibdetection)
    expect_identical(sum(r$mdat$sibdetection), 31115)
  }

  skip_if_no_ggir()
  expect_addsib_same_as_ggir(got, r$ts, 5, p3, r$tz, "T5A5", r$nightsi)
})

test_that("addsib part A: the six-day search window, the misses and the reset", {
  ts <- addsib_synth()
  ni <- addsib_nights(ts)
  expect_identical(ni, c(1441L, 18721L))
  p3 <- addsib_p3(ts, c(500, 3000, 8000, 15000), c(120, 300, 60, 400))

  got <- .raw.timeuse.addsib(ts, 5, p3, "Europe/Helsinki", "L5", ni)   # part A alone
  # each bout is marked inclusive of both ends
  expect_identical(sum(got$sibdetection), 121 + 301 + 61 + 401)
  expect_true(all(got$sibdetection[500:620] == 1))
  expect_identical(got$sibdetection[499], 0)
  expect_identical(got$sibdetection[621], 0)
  expect_addsib_same_as_ggir(got, ts, 5, p3, "Europe/Helsinki", "L5", ni)

  # an unmatched onset resets the search start to six days on, which on a short recording
  # pushes the window past the end, so bouts 3 and 4 miss as well
  miss_on <- p3; miss_on$sib.onset.time[2] <- "2019-01-01T00:00:00+0200"
  g1 <- .raw.timeuse.addsib(ts, 5, miss_on, "Europe/Helsinki", "L5", ni)
  expect_identical(sum(g1$sibdetection), 121)
  expect_true(all(g1$sibdetection[500:620] == 1))
  expect_addsib_same_as_ggir(g1, ts, 5, miss_on, "Europe/Helsinki", "L5", ni)
  miss_end <- p3; miss_end$sib.end.time[4] <- "2019-01-01T00:00:00+0200"
  g2 <- .raw.timeuse.addsib(ts, 5, miss_end, "Europe/Helsinki", "L5", ni)
  expect_identical(sum(g2$sibdetection), 121 + 301 + 61)
  expect_addsib_same_as_ggir(g2, ts, 5, miss_end, "Europe/Helsinki", "L5", ni)

  # an empty bout table leaves sibdetection at zero and still runs part B
  g3 <- .raw.timeuse.addsib(ts, 5, p3[0, ], "Europe/Helsinki", "L5", ni)
  expect_identical(sum(g3$sibdetection), 0)
  expect_addsib_same_as_ggir(g3, ts, 5, p3[0, ], "Europe/Helsinki", "L5", ni)
})

test_that("addsib converts a POSIX-style time column before matching", {
  ts <- addsib_synth()
  ni <- addsib_nights(ts)
  p3 <- addsib_p3(ts, c(500, 3000), c(120, 300))     # bout edges stay ISO, as part 3 stores them
  posix_ts <- ts
  posix_ts$time <- format(.raw.iso8601.to.posix(ts$time, tz = "Europe/Helsinki"))
  expect_false(.raw.is.iso8601(posix_ts$time[1]))

  got <- .raw.timeuse.addsib(posix_ts, 5, p3, "Europe/Helsinki", "L5", ni)
  expect_identical(sum(got$sibdetection), 121 + 301)
  expect_true(all(got$sibdetection[500:620] == 1))
  expect_addsib_same_as_ggir(got, posix_ts, 5, p3, "Europe/Helsinki", "L5", ni)
})

test_that("addsib part B: the window clamps, the epoch size and the no-gap fallbacks", {
  # redo1 clamped to 1 because the first midnight is less than an hour in
  ts <- addsib_synth(start = "2021-06-01 23:30:00")
  ni <- addsib_nights(ts)
  expect_identical(ni[1], 361L)
  p3 <- addsib_p3(ts, c(200, 5000), c(100, 200))
  got <- .raw.timeuse.addsib(ts, 5, p3, "Europe/Helsinki", "T5A5", ni)
  expect_addsib_same_as_ggir(got, ts, 5, p3, "Europe/Helsinki", "T5A5", ni)

  # redo2 clamped to Nts on a recording shorter than 14 h past the first midnight
  ts2 <- addsib_synth(n = 3000)
  ni2 <- addsib_nights(ts2)
  p32 <- addsib_p3(ts2, c(100, 500), c(50, 100))
  got2 <- .raw.timeuse.addsib(ts2, 5, p32, "Europe/Helsinki", "T5A5", ni2)
  expect_identical(length(got2$sibdetection), 3000L)
  expect_addsib_same_as_ggir(got2, ts2, 5, p32, "Europe/Helsinki", "T5A5", ni2)

  # an empty nightsi skips part B, whatever the definition is called
  a <- .raw.timeuse.addsib(ts, 5, p3, "Europe/Helsinki", "T5A5", integer(0))
  b <- .raw.timeuse.addsib(ts, 5, p3, "Europe/Helsinki", "L5", ni)
  expect_identical(a$sibdetection, b$sibdetection)
  expect_addsib_same_as_ggir(a, ts, 5, p3, "Europe/Helsinki", "T5A5", integer(0))

  # at 60 s epochs the search window and the time threshold both scale
  ts60 <- addsib_synth(n = 3000, ws3 = 60)
  ni60 <- addsib_nights(ts60)
  p360 <- addsib_p3(ts60, c(40, 300), c(10, 20))
  got60 <- .raw.timeuse.addsib(ts60, 60, p360, "Europe/Helsinki", "T5A5", ni60)
  expect_addsib_same_as_ggir(got60, ts60, 60, p360, "Europe/Helsinki", "T5A5", ni60)

  # fallbacks: a flat angle fills the window with 1, a constantly changing one with 0
  flat <- addsib_synth(n = 3000)
  flat$angle <- 0
  gf <- .raw.timeuse.addsib(flat, 5, p32, "Europe/Helsinki", "T5A5", ni2)
  redo1 <- 1; redo2 <- min(nrow(flat), ni2[1] + 10080)
  expect_true(all(gf$sibdetection[max(1, ni2[1] - 720):redo2] == 1))
  expect_addsib_same_as_ggir(gf, flat, 5, p32, "Europe/Helsinki", "T5A5", ni2)
  wobble <- addsib_synth(n = 3000)
  wobble$angle <- rep(c(-45, 45), length.out = 3000)   # a posture change every epoch
  gw <- .raw.timeuse.addsib(wobble, 5, p32, "Europe/Helsinki", "T5A5", ni2)
  expect_true(all(gw$sibdetection[max(1, ni2[1] - 720):redo2] == 0))
  expect_addsib_same_as_ggir(gw, wobble, 5, p32, "Europe/Helsinki", "T5A5", ni2)
})

test_that("addsib reproduces the NA-angle repair defect under ggir_exact", {
  # GGIR zeroes the window-relative NA positions as absolute indices, so on a flat angle the
  # sib block moves by the 720-epoch offset between the two branches
  ts <- addsib_synth()
  ni <- addsib_nights(ts)
  redo1 <- ni[1] - 720
  redo2 <- ni[1] + 10080
  expect_identical(c(redo1, redo2), c(721, 11521))
  ts$angle <- 60
  ts$angle[(redo1 - 1) + c(1000, 3000, 5000, 7000, 9000)] <- NA
  p3 <- addsib_p3(ts, c(500, 15000), c(100, 200))

  exact <- .raw.timeuse.addsib(ts, 5, p3, "Europe/Helsinki", "T5A5", ni, ggir_exact = TRUE)
  fixed <- .raw.timeuse.addsib(ts, 5, p3, "Europe/Helsinki", "T5A5", ni, ggir_exact = FALSE)

  expect_false(identical(exact$sibdetection, fixed$sibdetection))
  expect_identical(sum(exact$sibdetection != fixed$sibdetection), 1440L)
  expect_identical(as.integer(which(exact$sibdetection[redo1:redo2] == 1) + redo1 - 1),
                   1000:8999)
  expect_identical(as.integer(which(fixed$sibdetection[redo1:redo2] == 1) + redo1 - 1),
                   1720:9719)
  expect_identical(sum(exact$sibdetection), 8302)
  expect_identical(sum(fixed$sibdetection), 8302)

  # GGIR's own answer is the exact one
  expect_addsib_same_as_ggir(exact, ts, 5, p3, "Europe/Helsinki", "T5A5", ni)
})

test_that(".raw.is.iso8601 agrees with GGIR's is.ISO8601", {
  cases <- list("2025-10-07T20:30:00-0800", "2017-05-30T09:15:00+0300",
                "2025-10-07 20:30:00", "2025-10-07", "20:30:00", "",
                1L, as.POSIXct("2025-10-07 20:30:00", tz = "UTC"))
  got <- vapply(cases, .raw.is.iso8601, logical(1))
  expect_identical(got, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(got, vapply(cases, GGIR:::is.ISO8601, logical(1)))
  }
})
