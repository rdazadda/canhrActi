# Parity tests for R/raw_metrics.R against GGIR's g.applymetrics. Every parity
# assertion is identical(); the port keeps GGIR's operand order and library calls.
# Reference data come from CANHRACTI_GGIR_REF; those tests skip when it is unset,
# and the live comparisons skip when GGIR is not installed.

ggir_ref_dir <- function() {
  ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
  if (!nzchar(ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is not set; GGIR reference data unavailable")
  }
  if (!dir.exists(ref)) {
    testthat::skip(paste0("CANHRACTI_GGIR_REF does not exist: ", ref))
  }
  ref
}

ggir_ref_file <- function(...) {
  f <- file.path(ggir_ref_dir(), ...)
  if (!file.exists(f)) testthat::skip(paste0("Reference file missing: ", f))
  f
}

skip_if_no_ggir <- function() {
  testthat::skip_if_not_installed("GGIR")
}

skip_if_no_phase2 <- function() {
  testthat::skip_if_not_installed("signal")
  testthat::skip_if_not_installed("actilifecounts")
}

# Pull a closure GGIR nests inside g.applymetrics (averagePerEpoch, sumPerEpoch,
# rollmed) out of its body, so the port is compared with GGIR's own code.
ggir_find_assign <- function(expr, name) {
  if (is.call(expr)) {
    if ((identical(expr[[1]], as.name("=")) || identical(expr[[1]], as.name("<-"))) &&
        identical(expr[[2]], as.name(name))) {
      return(expr[[3]])
    }
    for (i in seq_along(expr)) {
      if (is.call(expr[[i]])) {
        r <- ggir_find_assign(expr[[i]], name)
        if (!is.null(r)) return(r)
      }
    }
  }
  NULL
}

ggir_closure <- function(name, parent = NULL) {
  skip_if_no_ggir()
  body_expr <- body(GGIR::g.applymetrics)
  if (!is.null(parent)) {
    body_expr <- ggir_find_assign(body_expr, parent)
    if (is.null(body_expr)) testthat::skip(paste0("GGIR closure not found: ", parent))
  }
  fexpr <- ggir_find_assign(body_expr, name)
  if (is.null(fexpr)) testthat::skip(paste0("GGIR closure not found: ", name))
  eval(fexpr, envir = asNamespace("GGIR"))
}

# metrics2do as g.getmeta builds it, from this module's flag builder
ggir_metrics2do <- function(flags) {
  as.data.frame(flags)
}

# 1 h synthetic recording: gravity on z with slow drift, 2 Hz on x, 1 Hz on y, noise,
# and two still stretches to exercise the zero-crossing stop band and rollmed patching.
synth_hour <- function(sf, seed = 11) {
  set.seed(seed)
  N <- sf * 3600
  t <- (1:N) / sf
  x <- 0.15 * sin(2 * pi * 2 * t) + rnorm(N, 0, 0.02)
  y <- 0.10 * sin(2 * pi * 1 * t + 1) + rnorm(N, 0, 0.02) - 0.1
  z <- 0.95 + 0.05 * sin(2 * pi * t / 600) + rnorm(N, 0, 0.02)
  still <- c(1:(sf * 120), (sf * 1800 + 1):(sf * 2100))
  x[still] <- 0.02 + rnorm(length(still), 0, 0.001)
  y[still] <- -0.05 + rnorm(length(still), 0, 0.001)
  z[still] <- 0.99 + rnorm(length(still), 0, 0.001)
  d <- cbind(x = x, y = y, z = z)
  colnames(d) <- c("x", "y", "z")
  d
}

# MOS2 block 1 as g.getmeta hands it to g.applymetrics: read, gaps imputed, start
# aligned to the 15 min boundary, cut to whole ws2 windows, calibrated. Built once.
.mos2_cache <- new.env(parent = emptyenv())
mos2_block1 <- function() {
  if (!is.null(.mos2_cache$block)) return(.mos2_cache$block)
  skip_if_no_ggir()
  f <- ggir_ref_file("din", "MOS2E39230594.gt3x")
  meta <- ggir_ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
  e <- new.env()
  load(meta, envir = e)
  C <- e$C
  M <- e$M
  sf <- 30; ws3 <- 5; ws2 <- 900; ws <- 3600
  tz <- "America/Anchorage"
  params <- GGIR:::extract_params(params2check = c("metrics", "rawdata", "general", "cleaning"))
  I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = tz,
                                            params_rawdata = params$params_rawdata, configtz = c()))
  ncb <- GGIR:::get_nw_clip_block_params(monc = I$monc, dformat = I$dformc,
                                         deviceSerialNumber = GGIR:::g.extractheadervars(I)$deviceSerialNumber,
                                         sf = sf, params_rawdata = params$params_rawdata)
  fq <- data.frame(filetooshort = FALSE, filecorrupt = FALSE, filedoesnotholdday = FALSE,
                   NFilePagesSkipped = 0)
  pg <- params$params_general
  pg$desiredtz <- tz
  pg$configtz <- tz
  acc <- GGIR:::g.readaccfile(filename = f, blocksize = ncb$blocksize, blocknumber = 1,
                              filequality = fq, ws = ws, PreviousEndPage = c(),
                              inspectfileobject = I, PreviousLastValue = c(0, 0, 1),
                              PreviousLastTime = NULL, params_rawdata = params$params_rawdata,
                              params_general = pg, header = NULL)
  d <- acc$P$data
  P <- GGIR:::g.imputeTimegaps(d, sf = sf, k = 0.25, PreviousLastValue = c(0, 0, 1),
                               PreviousLastTime = NULL, epochsize = c(ws3, ws2))
  xm <- as.matrix(P$x, rownames.force = FALSE)
  SW <- GGIR:::get_starttime_weekday_truncdata(I$monc, I$dformc, xm, NULL, desiredtz = tz,
                                               sf, f, ws2, configtz = NULL)
  xm <- SW$data
  LD <- nrow(xm)
  use <- floor(LD / (ws2 * sf)) * (ws2 * sf)
  xm <- xm[1:use, ]
  if ("remaining_epochs" %in% colnames(xm)) {
    xm <- xm[, colnames(xm) != "remaining_epochs"]
  }
  rawz <- xm[, "z"]
  # the calibration call as g.getmeta writes it
  xm[, c("x", "y", "z")] <- scale(xm[, c("x", "y", "z")], center = -C$offset, scale = 1 / C$scale)
  xyz <- xm[, c("x", "y", "z")]
  .mos2_cache$block <- list(xyz = xyz, rawz = rawz, rows_read = nrow(d),
                            rows_imputed = nrow(P$x), rows_aligned = LD, use = use,
                            starttime = SW$starttime, metashort = M$metashort)
  .mos2_cache$block
}

# STRUCTURE HELPERS

test_that("metric name order and flag builder match g.applymetrics", {
  nm <- .raw.metric.names()
  expect_length(nm, 34)
  expect_identical(nm[1:4], c("BFX", "BFY", "BFZ", "BFEN"))
  expect_identical(nm[24:30], c("angle_x", "angle_y", "angle_z", "ENMO", "MAD", "EN", "ENMOa"))
  expect_identical(nm[31:34], c("NeishabouriCount_x", "NeishabouriCount_y",
                                "NeishabouriCount_z", "NeishabouriCount_vm"))

  fl <- .raw.metric.flags()
  expect_length(fl, 32)
  expect_true(all(startsWith(names(fl), "do.")))
  expect_identical(names(fl)[fl == TRUE], c("do.enmo", "do.anglez"))

  fl_all <- .raw.metric.flags(all = TRUE)
  expect_identical(sum(unlist(fl_all)), 31L)
  expect_false(fl_all$do.brondcounts)

  fl2 <- .raw.metric.flags(do.en = TRUE, do.anglez = FALSE)
  expect_true(fl2$do.en)
  expect_false(fl2$do.anglez)
  expect_error(.raw.metric.flags(do.nothing = TRUE), "unknown metric flag")

  # a parameter list with unrelated members is accepted; only do.* is read
  p <- list(desiredtz = "UTC", do.mad = TRUE, hb = 15, do.zcx = TRUE)
  fl3 <- .raw.metric.flags(p)
  expect_true(fl3$do.mad)
  expect_true(fl3$do.zcx)
  expect_true(fl3$do.enmo)
  expect_false(fl3$do.bfen)
})

test_that("epoch averaging and summing drop the trailing partial epoch (1000 samples at 30 Hz, ws3 5 -> 6)", {
  a <- .raw.average.per.epoch(rep(1, 1000), 30, 5)
  expect_identical(a, rep(1, 6))
  s <- .raw.sum.per.epoch(rep(1, 1000), 30, 5)
  expect_identical(s, rep(150, 6))
  expect_identical(.raw.average.per.epoch(as.numeric(1:300), 30, 5), c(75.5, 225.5))
  expect_identical(.raw.sum.per.epoch(as.numeric(1:300), 30, 5), c(11325, 33825))
  # exactly 6 whole epochs, nothing for the 100 leftover samples
  expect_length(.raw.average.per.epoch(rnorm(1000), 30, 5), 6L)
  expect_length(.raw.average.per.epoch(rnorm(900), 30, 5), 6L)
  expect_length(.raw.average.per.epoch(rnorm(899), 30, 5), 5L)
})

test_that("epoch helpers are identical to GGIR's nested averagePerEpoch and sumPerEpoch", {
  skip_if_no_ggir()
  g_avg <- ggir_closure("averagePerEpoch")
  g_sum <- ggir_closure("sumPerEpoch")
  set.seed(3)
  x <- rnorm(1000)
  expect_identical(.raw.average.per.epoch(x, 30, 5), g_avg(x, 30, 5))
  expect_identical(.raw.sum.per.epoch(x, 30, 5), g_sum(x, 30, 5))
  x100 <- rnorm(100 * 3600)
  expect_identical(.raw.average.per.epoch(x100, 100, 5), g_avg(x100, 100, 5))
  expect_identical(.raw.sum.per.epoch(x100, 100, 5), g_sum(x100, 100, 5))
})

test_that("hb is coerced to round(sf/2) - 1 when sf <= 2 * hb (14 at 30 Hz)", {
  expect_identical(.raw.coerce.hb(30, 15), 14)
  expect_identical(.raw.coerce.hb(100, 15), 15)
  expect_identical(.raw.coerce.hb(28, 15), 13)
  expect_identical(.raw.coerce.hb(50, 25), 24)
  expect_identical(.raw.coerce.hb(86, 15), 15)
  expect_identical(.raw.coerce.hb(25, 15), 11) # round(12.5) is round-half-even = 12, minus 1
})

test_that(".raw.round.metrics is lapply(round, 4) and passes NULL through", {
  m <- list(ENMO = c(0.01746939, 0.06043801), angle_z = c(48.066320, 2.251661))
  expect_identical(.raw.round.metrics(m), list(ENMO = c(0.0175, 0.0604), angle_z = c(48.0663, 2.2517)))
  expect_identical(.raw.round.metrics(m), lapply(m, round, 4))
  expect_null(.raw.round.metrics(NULL))
})

# FILTERS

test_that(".raw.filter is one causal pass from zero state, not filtfilt", {
  testthat::skip_if_not_installed("signal")
  d <- synth_hour(30)[1:600, ]
  hp <- .raw.filter(d, type = "high", lb = 0.2, hb = 14, n = 4, sf = 30)
  expect_identical(dim(hp), dim(d))
  coef <- signal::butter(4, 0.2 / 15, type = "high")
  direct <- as.numeric(signal::filter(coef, d[, 1]))
  expect_identical(as.numeric(hp[, 1]), direct)
  # manual direct-form recursion with zero initial state
  b <- coef$b; a <- coef$a; x <- d[, 1]; y2 <- numeric(length(x))
  for (t in seq_along(x)) {
    acc <- 0
    for (k in seq_along(b)) if (t - k + 1 >= 1) acc <- acc + b[k] * x[t - k + 1]
    for (k in 2:length(a)) if (t - k + 1 >= 1) acc <- acc - a[k] * y2[t - k + 1]
    y2[t] <- acc / a[1]
  }
  expect_equal(as.numeric(hp[, 1]), y2, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(as.numeric(hp[, 1]), as.numeric(signal::filtfilt(coef, d[, 1])))))
  # low-pass uses hb, band-pass uses both, and a band-pass of order 4 has 9 coefficients
  bp <- signal::butter(4, c(0.2 / 15, 14 / 15), type = "pass")
  expect_length(bp$b, 9L)
  lp <- .raw.filter(d, type = "low", lb = 0.2, hb = 14, n = 4, sf = 30)
  expect_identical(as.numeric(lp[, 2]), as.numeric(signal::filter(signal::butter(4, 14 / 15, type = "low"), d[, 2])))
  bpd <- .raw.filter(d, type = "pass", lb = 0.2, hb = 14, n = 4, sf = 30)
  expect_identical(as.numeric(bpd[, 3]), as.numeric(signal::filter(bp, d[, 3])))
  # GGIR warns "sf not found" before failing inside signal::butter
  expect_warning(tryCatch(.raw.filter(d, type = "high", lb = 0.2, hb = 14, n = 4),
                          error = function(e) NULL), "sf not found")
})

test_that("low-pass metrics use the coerced hb (14 at 30 Hz): GGIR LFX matches hb 14 and not hb 15", {
  skip_if_no_ggir()
  testthat::skip_if_not_installed("signal")
  d <- synth_hour(30)
  fl <- .raw.metric.flags(all = FALSE, do.lfx = TRUE)
  g <- GGIR::g.applymetrics(data = d, sf = 30, ws3 = 5, metrics2do = ggir_metrics2do(fl),
                            n = 4, lb = 0.2, hb = 15)
  lfx14 <- .raw.average.per.epoch(abs(.raw.filter(d, type = "low", lb = 0.2, hb = 14, n = 4, sf = 30))[, 1], 30, 5)
  lfx15 <- .raw.average.per.epoch(abs(.raw.filter(d, type = "low", lb = 0.2, hb = 15, n = 4, sf = 30))[, 1], 30, 5)
  expect_identical(g$LFX, lfx14)
  expect_false(identical(g$LFX, lfx15))
  ours <- do.call(.raw.apply.metrics, c(list(data = d, sf = 30, ws3 = 5), fl))
  expect_identical(ours$LFX, g$LFX)
})

# ROLLMED

test_that(".raw.rollmed is identical to GGIR's nested rollmed at 30 Hz and 100 Hz (synthetic)", {
  skip_if_no_ggir()
  g_rollmed <- ggir_closure("rollmed", parent = "process_axes")
  d30 <- synth_hour(30)
  for (j in 1:3) expect_identical(.raw.rollmed(d30[, j], 30), g_rollmed(d30[, j], 30))
  d100 <- synth_hour(100)
  for (j in 1:3) expect_identical(.raw.rollmed(d100[, j], 100), g_rollmed(d100[, j], 100))
  expect_length(.raw.rollmed(d30[, 3], 30), 30 * 3600)
  expect_length(.raw.rollmed(d100[, 3], 100), 100 * 3600)
  # window 51 samples at the 10 Hz working rate, stepsize 3 at 30 Hz and 10 at 100 Hz,
  # so the output is piecewise constant over stepsize samples
  r30 <- .raw.rollmed(d30[, 3], 30)
  expect_true(all(r30[seq(1, length(r30), by = 3)] == r30[seq(2, length(r30), by = 3)]))
  expect_true(all(r30[seq(1, length(r30), by = 3)] == r30[seq(3, length(r30), by = 3)]))
  r100 <- .raw.rollmed(d100[, 3], 100)
  expect_true(all(r100[seq(1, length(r100), by = 10)] == r100[seq(10, length(r100), by = 10)]))
  # the zero fill of the first 25 decimated samples is patched with the first non-zero median
  dec <- d30[seq(1, nrow(d30), by = 3), 3]
  first_med <- stats::median(dec[1:51])
  expect_identical(r30[1], first_med)
  expect_identical(r30[3 * 25 + 1], first_med)
  expect_identical(r30[3 * 26 + 1], stats::median(dec[2:52]))
})

test_that(".raw.rollmed is identical to GGIR's rollmed on the MOS2 block-1 z axis (stepsize 3, window 51, S2check 1800)", {
  skip_if_no_ggir()
  blk <- mos2_block1()
  g_rollmed <- ggir_closure("rollmed", parent = "process_axes")
  expect_identical(blk$use, 5022000)
  expect_identical(.raw.rollmed(blk$rawz, 30), g_rollmed(blk$rawz, 30))
  expect_identical(.raw.rollmed(blk$xyz[, "z"], 30), g_rollmed(blk$xyz[, "z"], 30))
  expect_identical(max(c(floor(30 / 10), 1)), 3)
  expect_identical(max(c(30 * 60, 1000)), 1800)
})

test_that("rollmed length fallback: GGIR returns the raw axis when length is not a multiple of stepsize; ggir_exact FALSE returns the median", {
  skip_if_no_ggir()
  g_rollmed <- ggir_closure("rollmed", parent = "process_axes")
  set.seed(5)
  x <- 0.9 + rnorm(1000, 0, 0.05) # 1000 is not a multiple of 3
  expect_identical(.raw.rollmed(x, 30), g_rollmed(x, 30))
  expect_identical(.raw.rollmed(x, 30), x) # GGIR returns the raw axis here
  fixed <- .raw.rollmed(x, 30, ggir_exact = FALSE)
  expect_false(identical(fixed, x))
  expect_length(fixed, 1000L)
  # equal to the median series of the same 334 decimated samples: padding two
  # samples makes the length a multiple of 3 without changing seq(1, n, by = 3)
  expect_identical(fixed, .raw.rollmed(c(x, x[1000], x[1000]), 30)[1:1000])
})

test_that(".raw.rollmedian0 is identical() to zoo::rollmedian with a zero fill", {
  zoo_med <- function(x, k) {
    tryCatch(suppressWarnings(zoo::rollmedian(x, k = k, fill = c(0, 0, 0), na.pad = FALSE)),
             error = function(e) conditionMessage(e))
  }
  ours <- function(x, k) {
    tryCatch(suppressWarnings(.raw.rollmedian0(x, k)), error = function(e) conditionMessage(e))
  }
  set.seed(21)
  for (n in c(1L, 2L, 3L, 5L, 50L, 51L, 52L, 299L, 1000L, 20001L)) {
    # accelerometer-like values: three decimals, runs of ties and idle zeros
    x <- round(stats::rnorm(n, 0.2, 0.6), 3)
    if (n > 100) x[40:90] <- 0
    if (n > 1000) x[5000:5600] <- x[5000]
    for (k in c(1, 3, 5, 21, 51, 101, 2, 4, 50, 51.5)) {
      expect_identical(ours(x, k), zoo_med(x, k), label = paste("n", n, "k", k))
      xna <- x
      xna[ceiling(n / 2)] <- NA
      expect_identical(ours(xna, k), zoo_med(xna, k), label = paste("NA, n", n, "k", k))
    }
    expect_identical(ours(as.integer(round(x * 1000)), 5), zoo_med(as.integer(round(x * 1000)), 5))
    expect_identical(ours(stats::setNames(x, seq_len(n)), 3), zoo_med(stats::setNames(x, seq_len(n)), 3))
  }
  expect_identical(ours(numeric(0), 3), zoo_med(numeric(0), 3))
  # the plain double vectors with an odd k do not reach zoo
  local_mocked_bindings(rollmedian = function(...) stop("zoo reached"), .package = "zoo")
  x <- round(stats::rnorm(20001, 0.2, 0.6), 3)
  expect_no_error(.raw.rollmedian0(x, 51))
  expect_error(.raw.rollmedian0(c(x[1:10], NA), 5), "zoo reached")
})

# ALL METRICS, SYNTHETIC

check_all_metrics_identical <- function(d, sf, ws3 = 5) {
  fl <- .raw.metric.flags(all = TRUE)
  g <- GGIR::g.applymetrics(data = d, sf = sf, ws3 = ws3, metrics2do = ggir_metrics2do(fl),
                            n = 4, lb = 0.2, hb = 15, zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01,
                            zc.order = 2, actilife_LFE = FALSE)
  ours <- do.call(.raw.apply.metrics, c(list(data = d, sf = sf, ws3 = ws3), fl))
  expect_identical(names(ours), .raw.metric.names())
  expect_identical(names(ours), names(g))
  for (nm in names(g)) {
    expect_identical(ours[[nm]], g[[nm]], label = paste0("metric ", nm, " at ", sf, " Hz"))
  }
  expect_identical(ours, g)
  list(g = g, ours = ours)
}

test_that("all 34 metrics are identical to GGIR::g.applymetrics on a 1 h synthetic at 30 Hz", {
  skip_if_no_ggir()
  skip_if_no_phase2()
  d <- synth_hour(30)
  res <- check_all_metrics_identical(d, 30)
  expect_true(all(vapply(res$ours, length, 1L) == 720L))
  # NeishabouriCount_* identical to a direct actilifecounts call on the same chunk
  direct <- actilifecounts::get_counts(raw = d, sf = 30, epoch = 5, lfe_select = FALSE, verbose = FALSE)
  expect_identical(res$ours$NeishabouriCount_x, direct[, 1])
  expect_identical(res$ours$NeishabouriCount_y, direct[, 2])
  expect_identical(res$ours$NeishabouriCount_z, direct[, 3])
  expect_identical(res$ours$NeishabouriCount_vm, direct[, 4])
  # the counts are whole numbers per axis; vm is their Euclidean norm
  expect_true(all(res$ours$NeishabouriCount_x == round(res$ours$NeishabouriCount_x)))
  expect_identical(res$ours$NeishabouriCount_vm,
                   sqrt(res$ours$NeishabouriCount_x^2 + res$ours$NeishabouriCount_y^2 + res$ours$NeishabouriCount_z^2))
})

test_that("all 34 metrics are identical to GGIR::g.applymetrics on a 1 h synthetic at 100 Hz", {
  skip_if_no_ggir()
  skip_if_no_phase2()
  d <- synth_hour(100)
  res <- check_all_metrics_identical(d, 100)
  expect_true(all(vapply(res$ours, length, 1L) == 720L))
  direct <- actilifecounts::get_counts(raw = d, sf = 100, epoch = 5, lfe_select = FALSE, verbose = FALSE)
  expect_identical(res$ours$NeishabouriCount_vm, direct[, 4])
  expect_identical(res$ours$NeishabouriCount_x, direct[, 1])
})

test_that("zero-crossing counts are whole numbers and equal an independent count of sign changes", {
  skip_if_no_ggir()
  testthat::skip_if_not_installed("signal")
  d <- synth_hour(30)
  fl <- .raw.metric.flags(all = FALSE, do.zcx = TRUE, do.zcy = TRUE, do.zcz = TRUE, do.enmo = FALSE, do.anglez = FALSE)
  ours <- do.call(.raw.apply.metrics, c(list(data = d, sf = 30, ws3 = 5), fl))
  g <- GGIR::g.applymetrics(data = d, sf = 30, ws3 = 5, metrics2do = ggir_metrics2do(fl),
                            n = 4, lb = 0.2, hb = 15, zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2)
  expect_identical(names(ours), c("ZCX", "ZCY", "ZCZ"))
  expect_identical(ours, g)
  # independent reconstruction: band-pass 0.25 to 3 Hz order 2, stop band 0.01 g,
  # sign with 0 as +1, then sign changes per epoch counting the change at the
  # epoch's left boundary (GGIR assigns a crossing to the later sample)
  bp <- signal::butter(2, c(0.25 / 15, 3 / 15), type = "pass")
  for (j in 1:3) {
    v <- as.numeric(signal::filter(bp, d[, j]))
    v[abs(v) < 0.01] <- 0
    s <- ifelse(v >= 0, 1, -1)
    chg <- c(s[2] != s[1], s[-1] != s[-length(s)]) # element i is the change between i-1 and i; element 1 duplicated
    ep <- rep(seq_len(720), each = 150)
    indep <- as.numeric(tapply(as.numeric(chg), ep, sum))
    expect_identical(unname(ours[[j]]), indep)
  }
  expect_true(all(ours$ZCX == round(ours$ZCX)))
  expect_true(all(ours$ZCX >= 0))
  expect_true(any(ours$ZCX > 0))
  # the still stretch at the start (24 epochs) has few crossings after the stop band
  expect_true(mean(ours$ZCX[3:24]) < mean(ours$ZCX[100:700]))
})

test_that("partial trailing epoch is dropped by every metric (1000 samples at 30 Hz, ws3 5 -> 6 epochs)", {
  skip_if_no_ggir()
  testthat::skip_if_not_installed("signal")
  d <- synth_hour(30)[1:1000, ]
  fl <- .raw.metric.flags(all = FALSE, do.enmo = TRUE, do.anglez = TRUE, do.en = TRUE, do.enmoa = TRUE,
                          do.lfx = TRUE, do.hfen = TRUE, do.bfen = TRUE, do.zcy = TRUE,
                          do.roll_med_acc_z = TRUE, do.dev_roll_med_acc_x = TRUE, do.anglex = TRUE)
  ours <- do.call(.raw.apply.metrics, c(list(data = d, sf = 30, ws3 = 5), fl))
  g <- GGIR::g.applymetrics(data = d, sf = 30, ws3 = 5, metrics2do = ggir_metrics2do(fl),
                            n = 4, lb = 0.2, hb = 15, zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2)
  expect_identical(ours, g)
  expect_true(all(vapply(ours, length, 1L) == 6L))
  expect_identical(names(ours), c("BFEN", "ZCY", "LFX", "HFEN", "roll_med_acc_x", "roll_med_acc_y",
                                  "roll_med_acc_z", "dev_roll_med_acc_x", "dev_roll_med_acc_y",
                                  "dev_roll_med_acc_z", "angle_x", "angle_z", "ENMO", "EN", "ENMOa"))
  # any roll_med or dev_roll_med flag computes all three axes
  expect_true(all(c("roll_med_acc_x", "roll_med_acc_y", "roll_med_acc_z",
                    "dev_roll_med_acc_x", "dev_roll_med_acc_y", "dev_roll_med_acc_z") %in% names(ours)))
  # HFEN is present whenever any hf flag is on, even without do.hfen
  fl2 <- .raw.metric.flags(all = FALSE, do.enmo = FALSE, do.anglez = FALSE, do.hfx = TRUE)
  ours2 <- do.call(.raw.apply.metrics, c(list(data = d, sf = 30, ws3 = 5), fl2))
  expect_identical(names(ours2), c("HFX", "HFEN"))
  g2 <- GGIR::g.applymetrics(data = d, sf = 30, ws3 = 5, metrics2do = ggir_metrics2do(fl2), n = 4, lb = 0.2, hb = 15)
  expect_identical(ours2, g2)
})

test_that("HFENplus: GGIR low-passes at hb (closure), reproduced under ggir_exact; ggir_exact FALSE low-passes at lb", {
  skip_if_no_ggir()
  testthat::skip_if_not_installed("signal")
  d <- synth_hour(30)[1:(30 * 600), ]
  fl <- .raw.metric.flags(all = FALSE, do.enmo = FALSE, do.anglez = FALSE, do.hfenplus = TRUE)
  g <- GGIR::g.applymetrics(data = d, sf = 30, ws3 = 5, metrics2do = ggir_metrics2do(fl), n = 4, lb = 0.2, hb = 15)
  exact <- do.call(.raw.apply.metrics, c(list(data = d, sf = 30, ws3 = 5), fl))
  fixed <- do.call(.raw.apply.metrics, c(list(data = d, sf = 30, ws3 = 5, ggir_exact = FALSE), fl))
  expect_identical(exact, g)
  expect_false(identical(fixed$HFENplus, g$HFENplus))
  EN <- function(m) sqrt((m[, 1]^2) + (m[, 2]^2) + (m[, 3]^2))
  build <- function(low_cut) {
    lp <- .raw.filter(d, type = "low", lb = 0.2, hb = low_cut, n = 4, sf = 30)
    GCP <- EN(lp) - 1
    hp <- .raw.filter(d, type = "high", lb = 0.2, hb = 14, n = 4, sf = 30)
    H <- EN(hp) + GCP
    H[which(H < 0)] <- 0
    .raw.average.per.epoch(H, 30, 5)
  }
  expect_identical(exact$HFENplus, build(14))
  expect_identical(fixed$HFENplus, build(0.2))
})

test_that("input handling: unnamed 3-column matrix accepted, extra columns ignored, no flags -> NULL, brondcounts errors", {
  d <- synth_hour(30)[1:1500, ]
  named <- .raw.apply.metrics(d, sf = 30, ws3 = 5)
  unnamed <- .raw.apply.metrics(unname(d), sf = 30, ws3 = 5)
  expect_identical(named, unnamed)
  expect_identical(names(named), c("angle_z", "ENMO"))
  with_time <- cbind(time = seq_len(nrow(d)), d)
  expect_identical(.raw.apply.metrics(with_time, sf = 30, ws3 = 5), named)
  expect_error(.raw.apply.metrics(cbind(a = d[, 1], b = d[, 2], c = d[, 3]), sf = 30, ws3 = 5), "named x, y and z")
  expect_null(.raw.apply.metrics(d, sf = 30, ws3 = 5, do.enmo = FALSE, do.anglez = FALSE))
  expect_error(.raw.apply.metrics(d, sf = 30, ws3 = 5, do.brondcounts = TRUE), "brondcounts option has been deprecated")
  # ENMO clips negatives per sample before averaging
  still <- matrix(c(rep(0, 300), rep(0, 300), rep(0.98, 300)), ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
  expect_identical(.raw.apply.metrics(still, sf = 30, ws3 = 5, do.anglez = FALSE)$ENMO, c(0, 0))
  expect_identical(.raw.apply.metrics(still, sf = 30, ws3 = 5, do.anglez = FALSE, do.enmoa = TRUE)$ENMOa,
                   .raw.average.per.epoch(abs(rep(0.98, 300) - 1), 30, 5))
})

# MOS2 BLOCK 1

test_that("MOS2 block 1 rebuild has the documented shape", {
  blk <- mos2_block1()
  expect_identical(blk$rows_read, 2592000L)
  expect_identical(blk$rows_imputed, 5036460L)
  expect_identical(blk$rows_aligned, 5032980L)
  expect_identical(blk$use, 5022000)
  expect_identical(dim(blk$xyz), c(5022000L, 3L))
  expect_identical(colnames(blk$xyz), c("x", "y", "z"))
  expect_identical(typeof(blk$xyz), "double")
  expect_identical(unname(format(blk$starttime, "%Y-%m-%d %H:%M:%S")), "2025-10-07 20:30:00")
})

test_that("chunk-1 replication: ENMO and anglez after round(4) and the character round trip equal the stored metashort", {
  blk <- mos2_block1()
  ours <- .raw.apply.metrics(blk$xyz, sf = 30, ws3 = 5) # GGIR defaults: do.enmo, do.anglez
  expect_identical(names(ours), c("angle_z", "ENMO"))
  expect_length(ours$ENMO, 33480L)
  r <- .raw.round.metrics(ours, 4)
  # g.getmeta stores the rounded values in a character matrix and reads them back with as.numeric
  enmo <- as.numeric(as.character(r$ENMO))
  anglez <- as.numeric(as.character(r$angle_z))
  expect_identical(enmo[1:6], c(0.0175, 0.0604, 0.0581, 0.0659, 0.0396, 0.0896))
  expect_identical(anglez[1:6], c(48.0663, 2.2517, 7.3921, 29.8733, 10.2377, 29.3146))
  stored <- blk$metashort
  expect_identical(enmo, stored$ENMO[1:33480])
  expect_identical(anglez, stored$anglez[1:33480])
  # unrounded first values, so a drift in the arithmetic shows before rounding
  expect_equal(ours$ENMO[1:3], c(0.01746939, 0.06043801, 0.05807307), tolerance = 1e-7)
  expect_equal(ours$angle_z[1:3], c(48.066320, 2.251661, 7.392135), tolerance = 1e-7)
})

test_that("MOS2 block 1 with every metric on is identical to GGIR::g.applymetrics, column by column and in order", {
  skip_if_no_phase2()
  blk <- mos2_block1()
  res <- check_all_metrics_identical(blk$xyz, 30)
  expect_true(all(vapply(res$ours, length, 1L) == 33480L))
  direct <- actilifecounts::get_counts(raw = blk$xyz, sf = 30, epoch = 5, lfe_select = FALSE, verbose = FALSE)
  expect_identical(res$ours$NeishabouriCount_x, direct[, 1])
  expect_identical(res$ours$NeishabouriCount_y, direct[, 2])
  expect_identical(res$ours$NeishabouriCount_z, direct[, 3])
  expect_identical(res$ours$NeishabouriCount_vm, direct[, 4])
  # values seen on the study machine (GGIR 3.3.6, actilifecounts 1.1.1, signal 1.8.1, zoo 1.8.15)
  expect_identical(res$ours$ZCX[1:3], c(7, 11, 17))
  expect_identical(res$ours$ZCY[1:3], c(7, 10, 8))
  expect_identical(res$ours$ZCZ[1:3], c(4, 7, 4))
  expect_identical(res$ours$NeishabouriCount_x[1:3], c(7, 235, 170))
  expect_identical(res$ours$NeishabouriCount_y[1:3], c(2, 307, 203))
  expect_identical(res$ours$NeishabouriCount_z[1:3], c(24, 590, 674))
  expect_equal(res$ours$BFEN[1:3], c(0.1937699, 0.3955429, 0.4657570), tolerance = 1e-6)
  expect_equal(res$ours$HFENplus[1:3], c(0.1997404, 0.4237547, 0.4915428), tolerance = 1e-6)
  expect_equal(res$ours$MAD[1:3], c(0.02956888, 0.09470277, 0.09091236), tolerance = 1e-6)
  expect_equal(res$ours$roll_med_acc_z[1:3], c(0.74575420, 0.02499929, 0.11274719), tolerance = 1e-6)
  expect_equal(res$ours$angle_x[1:3], c(-41.50130, -71.55829, -80.67868), tolerance = 1e-6)
})
