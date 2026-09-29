# raw.guider and its internals against GGIR's HASPT, identical() on the whole returned list.
# Reference data come from CANHRACTI_GGIR_REF; the noon-to-noon windows HASPT is called on are
# rebuilt here from the stored part-1 and part-2 milestones. Tests against the stored ms3
# numbers need no GGIR; tests against GGIR:::HASPT itself, and the ones that need a sustained
# inactivity classification (HLRB, the marker button), skip when GGIR is absent. The LowAcc
# branch exists only in GGIR 3.3-9, so it is compared against the clone source when that is
# next to the reference folder. The whole file runs with the C time locale and in
# America/Anchorage, the system zone the MOS2 milestones were stored in.

withr::local_locale(c(LC_TIME = "C"))
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
# HASPT of the GGIR 3.3-9 clone, which has the LowAcc branch and HDCZA_roll_windowsize.
clone_haspt <- function() {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIR", "R", "HASPT.R")
}
# The study's fixture recordings (daylight saving and a day sleeper), one level up.
p34_fixture <- function(case) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", case, "output_din")
}

GD_CASES <- list(
  MOS2 = list(out = c("out", "output_din"),           fn = "MOS2E39230594.gt3x"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x")
)
gd_path <- function(case, what) {
  cc <- GD_CASES[[case]]
  switch(what,
         basic = do.call(ref_file, c(as.list(cc$out), list("meta", "basic", paste0("meta_", cc$fn, ".RData")))),
         ms2   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms2.out", paste0(cc$fn, ".RData")))),
         ms3   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms3.out", paste0(cc$fn, ".RData")))))
}

# The night windows HASPT is called on, rebuilt from the stored milestones with the loop head
# of g.sib.det (invalid expansion, midnights, first-04:00 rule, twd, DST correction, clamps).
gd_build <- function(path_basic, path_ms2, path_ms3) {
  e1 <- new.env(parent = emptyenv()); load(path_basic, envir = e1)
  e2 <- new.env(parent = emptyenv()); load(path_ms2, envir = e2)
  e3 <- new.env(parent = emptyenv()); load(path_ms3, envir = e3)
  M <- e1$M; IMP <- e2$IMP
  tz <- e3$desiredtz_part1            # the stored run's timezone
  ws3 <- M$windowsizes[1]; ws2 <- M$windowsizes[2]
  nD <- nrow(IMP$metashort)
  n_ws3_perday <- (1440 * 60) / ws3
  time <- as.character(IMP$metashort[, 1])
  anglez <- as.numeric(as.matrix(IMP$metashort[, which(colnames(IMP$metashort) == "anglez")]))
  anglez[is.na(anglez)] <- 0
  ACC <- as.numeric(as.matrix(IMP$metashort[, which(colnames(IMP$metashort) == "ENMO")]))
  r5 <- as.numeric(as.matrix(IMP$rout[, 5]))
  r5long <- matrix(0, length(r5), (ws2 / ws3))
  r5long <- replace(r5long, 1:length(r5long), r5)
  r5long <- t(r5long)
  dim(r5long) <- c((length(r5) * (ws2 / ws3)), 1)
  invalid <- if (nD < length(r5long)) r5long[1:nD] else c(r5long, rep(0, (nD - length(r5long))))
  dm <- .raw.detect.midnight(time, tz, dayborder = 0)
  midnightsi <- dm$midnightsi
  countmidn <- length(midnightsi)
  first4am <- grep("04:00:00", time[1:pmin(nD, (n_ws3_perday + 1))])[1]
  midn_start <- if (!is.na(first4am)) {
    if (first4am < midnightsi[1]) 0 else 1
  } else {
    countmidn
  }
  twd <- c(-12, 12)
  nights <- list(); sptei <- 0
  for (j in midn_start:countmidn) {
    if (j == 0) {
      qqq1 <- 1
      qqq2 <- midnightsi[1] + (twd[1] * (3600 / ws3))
    } else {
      qqq1 <- midnightsi[j] + (twd[1] * (3600 / ws3)) + 1
      qqq2 <- midnightsi[j] + (twd[2] * (3600 / ws3))
    }
    if (qqq2 < length(time) & qqq2 > 0) {
      qqq2_hour <- as.numeric(format(.raw.iso8601.to.posix(time[qqq2], tz = tz), "%H"))
      if (qqq2_hour == 11) {
        qqq2 <- qqq2 + (3600 / ws3)
      } else if (qqq2_hour == 13) {
        qqq2 <- qqq2 - (3600 / ws3)
      }
    }
    sptei <- sptei + 1
    if (qqq2 - qqq1 < 60) next
    if (qqq2 > length(time)) qqq2 <- length(time)
    if (qqq1 < 1) qqq1 <- 1
    tSegment <- qqq1:qqq2
    nights[[sptei]] <- list(sptei = sptei, qqq1 = qqq1, qqq2 = qqq2,
                            angle = anglez[tSegment], invalid = invalid[tSegment],
                            activity = ACC[tSegment], t1 = time[qqq1], t2 = time[qqq2],
                            nonwear_percentage = (length(which(invalid[tSegment] == 1)) /
                                                    (qqq2 - qqq1 + 1)) * 100)
  }
  list(ws3 = ws3, desiredtz = tz, midnightsi = midnightsi, midn_start = midn_start,
       nights = nights, time = time, anglez = anglez, ACC = ACC, invalid = invalid,
       stored = list(SPTE_start = e3$SPTE_start, SPTE_end = e3$SPTE_end,
                     tib.threshold = e3$tib.threshold, part3_guider = e3$part3_guider))
}

gd_nights <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref()
    pb <- gd_path(case, "basic"); skip_if_no_file(pb)
    p2 <- gd_path(case, "ms2");   skip_if_no_file(p2)
    p3 <- gd_path(case, "ms3");   skip_if_no_file(p3)
    out <- gd_build(pb, p2, p3)
    assign(case, out, envir = cache)
    out
  }
})

# The sustained inactivity classification for HLRB and the marker button, taken from GGIR.
gd_sibs <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir()
    d <- gd_nights(case)
    pp <- GGIR::load_params()$params_sleep
    sleep <- GGIR:::HASIB(HASIB.algo = pp[["HASIB.algo"]], timethreshold = pp[["timethreshold"]],
                          anglethreshold = pp[["anglethreshold"]], time = d$time,
                          anglez = d$anglez, ws3 = d$ws3, zeroCrossingCount = c(),
                          NeishabouriCount = c(), activity = d$ACC,
                          oakley_threshold = pp[["oakley_threshold"]])
    out <- lapply(d$nights, function(nn) sleep[nn$qqq1:nn$qqq2, 1])
    assign(case, out, envir = cache)
    out
  }
})

# GGIR's params_sleep with overrides, and the 3.3-9 members the installed 3.3.6 lacks
gd_params <- function(...) {
  pp <- GGIR::load_params()$params_sleep
  pp[["HDCZA_roll_windowsize"]] <- 5
  pp[["LowAcc_threshold"]] <- 0.014
  ov <- list(...)
  for (nm in names(ov)) pp[nm] <- list(ov[[nm]])
  pp
}
# the same without GGIR: the nine members the guider reads
gd_defaults <- function(...) {
  pp <- .raw.guider.defaults()
  ov <- list(...)
  for (nm in names(ov)) pp[nm] <- list(ov[[nm]])
  pp
}
# exact no-movement blocks for HorAngle: 90 degrees is above its threshold of 60, 0 is below
gd_horangle <- function(n, blocks) {
  angle <- rep(90, n)
  for (b in blocks) angle[b[1]:b[2]] <- 0
  angle
}

test_that("S2a HDCZA on MOS2 gives the stored guider indices, threshold and label", {
  d <- gd_nights("MOS2")
  expect_identical(d$desiredtz, "")
  expect_identical(d$ws3, 5)
  expect_identical(length(d$nights), 7L)
  expect_identical(c(d$nights[[2]]$qqq1, d$nights[[2]]$qqq2), c(11162, 28441))

  g <- raw.guider(angle = d$nights[[2]]$angle, params_sleep = gd_defaults(), ws3 = 5,
                  HASPT.algo = "HDCZA", invalid = d$nights[[2]]$invalid,
                  activity = d$nights[[2]]$activity)
  expect_identical(names(g), c("SPTE_start", "SPTE_end", "tib.threshold", "part3_guider",
                               "spt_crude_estimate"))
  expect_identical(g$SPTE_start, 8251)
  expect_identical(g$SPTE_end, 14299)
  # which() gives an integer; the "- 1" literal promotes it to double
  expect_identical(typeof(g$SPTE_start), "double")
  expect_identical(typeof(g$SPTE_end), "double")
  expect_identical(unname(g$tib.threshold), 0.13)
  expect_identical(g$part3_guider, "HDCZA")
  expect_identical(length(g$spt_crude_estimate), 17280L)
  expect_identical(g$SPTE_end - g$SPTE_start, 6048)

  # nights 2 to 4 convert to the stored hours as idx/720 + 12
  idx <- lapply(2:4, function(k) {
    raw.guider(angle = d$nights[[k]]$angle, params_sleep = gd_defaults(), ws3 = 5,
               HASPT.algo = "HDCZA", invalid = d$nights[[k]]$invalid,
               activity = d$nights[[k]]$activity)
  })
  expect_identical(vapply(idx, function(z) z$SPTE_start, 0), c(8251, 8441, 7229))
  expect_identical(vapply(idx, function(z) z$SPTE_end, 0), c(14299, 13731, 13731))
  expect_identical(vapply(idx, function(z) z$SPTE_start / 720 + 12, 0),
                   d$stored$SPTE_start[2:4])
  expect_identical(vapply(idx, function(z) z$SPTE_end / 720 + 12, 0), d$stored$SPTE_end[2:4])
  expect_identical(vapply(idx, function(z) unname(z$tib.threshold), 0), rep(0.13, 3))
  expect_identical(vapply(idx, function(z) z$part3_guider, ""), rep("HDCZA", 3))

  # night 1 is clipped; its stored NA is the detector's partial-first-day defect, not the guider's
  g1 <- raw.guider(angle = d$nights[[1]]$angle, params_sleep = gd_defaults(), ws3 = 5,
                   HASPT.algo = "HDCZA", invalid = d$nights[[1]]$invalid,
                   activity = d$nights[[1]]$activity)
  expect_identical(g1$SPTE_start, 1539)
  expect_identical(g1$SPTE_end, 7689)
  expect_identical(g1$part3_guider, "HDCZA")
  expect_true(is.na(d$stored$SPTE_start[1]))
  expect_identical(g1$SPTE_start / 720 + 20.5, 22.637499999999999)
})

test_that("S2b HDCZA threshold construction on EE, all seven nights", {
  d <- gd_nights("EE")
  expect_identical(d$desiredtz, "Europe/Helsinki")
  q10 <- numeric(7); thr <- numeric(7); nms <- character(7)
  for (k in 1:7) {
    nn <- d$nights[[k]]
    x <- .raw.guider.hdcza(nn$angle, ws3 = 5, HDCZA_roll_windowsize = 5,
                           HDCZA_threshold = c(10, 15))
    q10[k] <- unname(stats::quantile(x$x, probs = 0.1))
    thr[k] <- unname(x$threshold)
    nms[k] <- if (is.null(names(x$threshold))) "" else names(x$threshold)
  }
  # the 10th percentile times 15, then the [0.13, 0.50] clamp
  expect_equal(q10 * 15, c(0.1095, 0.20385, 0.132, 0.207, 0.1785, 0.1725, 0.3135),
               tolerance = 1e-12)
  expect_identical(thr[1], 0.13)                      # the clamp fires on night 1 only
  expect_identical(thr[2:6], d$stored$tib.threshold[2:6])
  expect_identical(thr[7], 0.31349999999999878)       # night 7 is 100 % invalid, stored NA
  expect_true(is.na(d$stored$tib.threshold[7]))
  # the clamped value is a bare literal; the unclamped one keeps quantile()'s name
  expect_identical(nms, c("", rep("10%", 6)))

  for (k in 1:6) {
    g <- raw.guider(angle = d$nights[[k]]$angle, params_sleep = gd_defaults(), ws3 = 5,
                    HASPT.algo = "HDCZA", invalid = d$nights[[k]]$invalid,
                    activity = d$nights[[k]]$activity)
    expect_identical(g$SPTE_start / 720 + 12, d$stored$SPTE_start[k])
    expect_identical(g$SPTE_end / 720 + 12, d$stored$SPTE_end[k])
    expect_identical(unname(g$tib.threshold), d$stored$tib.threshold[k])
    expect_identical(g$part3_guider, "HDCZA")
  }
  expect_identical(vapply(1:6, function(k) {
    raw.guider(angle = d$nights[[k]]$angle, params_sleep = gd_defaults(), ws3 = 5,
               HASPT.algo = "HDCZA", invalid = d$nights[[k]]$invalid,
               activity = d$nights[[k]]$activity)$SPTE_start
  }, 0), c(7400, 8368, 9376, 8880, 8845, 7759))
})

test_that("S2h a 100 percent invalid window short circuits to an empty SPT", {
  # the step block is skipped: SPTE empty, guider "none", tib.threshold still set
  d <- gd_nights("MOS2")
  for (k in 5:7) {
    expect_identical(d$nights[[k]]$nonwear_percentage, 100)
    g <- raw.guider(angle = d$nights[[k]]$angle, params_sleep = gd_defaults(), ws3 = 5,
                    HASPT.algo = "HDCZA", invalid = d$nights[[k]]$invalid,
                    activity = d$nights[[k]]$activity)
    expect_null(g$SPTE_start)
    expect_null(g$SPTE_end)
    expect_null(g$spt_crude_estimate)
    expect_identical(g$part3_guider, "none")
    expect_identical(unname(g$tib.threshold), 0.13)
    expect_true(is.na(d$stored$SPTE_start[k]))
    expect_true(is.na(d$stored$tib.threshold[k]))     # the detector never wrote it
  }
  e <- gd_nights("EE")
  g7 <- raw.guider(angle = e$nights[[7]]$angle, params_sleep = gd_defaults(), ws3 = 5,
                   HASPT.algo = "HDCZA", invalid = e$nights[[7]]$invalid,
                   activity = e$nights[[7]]$activity)
  expect_null(g7$SPTE_start)
  expect_identical(g7$part3_guider, "none")
})

test_that("S2f HASPT.ignore.invalid has three states, on MOS2 night 4", {
  d <- gd_nights("MOS2")
  n4 <- d$nights[[4]]
  expect_equal(n4$nonwear_percentage, 53.1308, tolerance = 1e-4)
  got <- lapply(list(FALSE, TRUE, NA), function(ii) {
    raw.guider(angle = n4$angle, params_sleep = gd_defaults(HASPT.ignore.invalid = ii),
               ws3 = 5, HASPT.algo = "HDCZA", invalid = n4$invalid, activity = n4$activity)
  })
  expect_identical(vapply(got, function(z) z$SPTE_start, 0), c(7229, 7229, 7229))
  expect_identical(vapply(got, function(z) z$SPTE_end, 0), c(13731, 8100, 17280))
  expect_identical(vapply(got, function(z) z$SPTE_start / 720 + 12, 0), rep(22.040277777777778, 3))
  expect_identical(vapply(got, function(z) z$SPTE_end / 720 + 12, 0),
                   c(31.070833333333333, 23.25, 36))
  # the "+invalid" tag only in the NA state, with invalid time inside the window
  expect_identical(vapply(got, function(z) z$part3_guider, ""),
                   c("HDCZA", "HDCZA", "HDCZA+invalid"))
  n2 <- d$nights[[2]]
  expect_identical(raw.guider(angle = n2$angle,
                              params_sleep = gd_defaults(HASPT.ignore.invalid = NA), ws3 = 5,
                              HASPT.algo = "HDCZA", invalid = n2$invalid,
                              activity = n2$activity)$part3_guider, "HDCZA")
})

test_that("S2g NotWorn on the day-sleeper shifted window of MOS2 night 4", {
  # the window moved 6 h forward, as the detector re-runs a night whose SPT ends near noon
  d <- gd_nights("MOS2")
  qqq1 <- d$nights[[4]]$qqq1; qqq2 <- d$nights[[4]]$qqq2
  tS <- (qqq1 + 6 * 720):(qqq2 + 6 * 720)
  g <- raw.guider(angle = d$anglez[tS], params_sleep = gd_defaults(), ws3 = 5,
                  HASPT.algo = "NotWorn", invalid = d$invalid[tS], activity = d$ACC[tS])
  expect_identical(g$SPTE_start, 2934)
  expect_identical(g$SPTE_end, 17280)
  expect_identical(unname(g$tib.threshold), 0.0025298240163371716)
  expect_identical(g$part3_guider, "NotWorn+invalid")
  # with daysleep_offset 6 the detector adds 6 + 12 hours
  expect_identical(g$SPTE_start / 720 + 18, 22.075)
  expect_identical(g$SPTE_end / 720 + 18, 42)
})

test_that("S2e the per-night NotWorn, HorAngle and HLRB numbers on MOS2 night 2", {
  d <- gd_nights("MOS2")
  n2 <- d$nights[[2]]
  h <- raw.guider(angle = n2$angle, params_sleep = gd_defaults(), ws3 = 5,
                  HASPT.algo = "HorAngle", invalid = n2$invalid, activity = n2$activity)
  expect_identical(c(h$SPTE_start, h$SPTE_end), c(9983, 13928))
  expect_identical(h$tib.threshold, 60)
  expect_identical(h$part3_guider, "HorAngle")

  w <- raw.guider(angle = n2$angle, params_sleep = gd_defaults(), ws3 = 5,
                  HASPT.algo = "NotWorn", invalid = n2$invalid, activity = n2$activity)
  expect_identical(c(w$SPTE_start, w$SPTE_end), c(8307, 13652))
  expect_identical(unname(w$tib.threshold), 0.0024056552609936577)
  expect_identical(w$part3_guider, "NotWorn")
  # min(activity) is 0, so NotWorn's 10th-percentile fallback does not fire
  expect_identical(min(n2$activity), 0)

  sibs <- gd_sibs("MOS2")[[2]]                       # needs GGIR for HASIB
  l <- raw.guider(angle = n2$angle, params_sleep = gd_defaults(), ws3 = 5, HASPT.algo = "HLRB",
                  invalid = n2$invalid, activity = n2$activity, sibs = sibs)
  expect_identical(c(l$SPTE_start, l$SPTE_end), c(8131L, 14199L))
  expect_identical(l$tib.threshold, 0)
  expect_identical(l$part3_guider, "HLRB")
})

test_that("the return shape differs by branch, as the guider corrector expects", {
  d <- gd_nights("MOS2")
  n2 <- d$nights[[2]]
  five <- c("SPTE_start", "SPTE_end", "tib.threshold", "part3_guider", "spt_crude_estimate")
  nu <- raw.guider(angle = n2$angle, params_sleep = gd_defaults(), ws3 = 5,
                   HASPT.algo = "notused", invalid = n2$invalid, activity = n2$activity)
  expect_identical(names(nu), five)
  expect_true(all(vapply(nu, is.null, TRUE)))
  sibs <- gd_sibs("MOS2")[[2]]
  l <- raw.guider(angle = n2$angle, params_sleep = gd_defaults(), ws3 = 5, HASPT.algo = "HLRB",
                  invalid = n2$invalid, sibs = sibs)
  expect_identical(names(l), five[1:4])
  expect_false("spt_crude_estimate" %in% names(l))
})

test_that("raw.guider is identical() to GGIR HASPT on every night of both recordings", {
  skip_if_no_ggir()
  n_compared <- 0L
  for (case in c("MOS2", "EE")) {
    d <- gd_nights(case)
    sibs <- gd_sibs(case)
    for (k in seq_along(d$nights)) {
      nn <- d$nights[[k]]
      for (algo in c("HDCZA", "HorAngle", "NotWorn", "HLRB", "notused")) {
        for (ii in list(FALSE, TRUE, NA)) {
          pp <- gd_params(HASPT.ignore.invalid = ii)
          ggir <- GGIR:::HASPT(angle = nn$angle, params_sleep = pp, ws3 = d$ws3,
                               HASPT.algo = algo, invalid = nn$invalid,
                               activity = nn$activity, marker = NULL, sibs = sibs[[k]])
          port <- raw.guider(angle = nn$angle, params_sleep = pp, ws3 = d$ws3,
                             HASPT.algo = algo, invalid = nn$invalid,
                             activity = nn$activity, marker = NULL, sibs = sibs[[k]])
          expect_identical(port, ggir,
                           info = paste(case, "night", k, algo, "ignore.invalid", ii))
          n_compared <- n_compared + 1L
        }
      }
    }
  }
  expect_identical(n_compared, 210L)   # 14 nights x 5 algorithms x 3 invalid strategies
})

test_that("S2e MotionWare matches GGIR at 15, 30 and 60 s and errors at 5 s", {
  skip_if_no_ggir()
  d <- gd_nights("MOS2")
  n2 <- d$nights[[2]]
  agg <- function(v, by) colMeans(matrix(v[1:((length(v) %/% by) * by)], nrow = by))
  for (w in c(15, 30, 60)) {
    by <- w / 5
    a <- agg(n2$activity, by) * 1000      # MotionWare expects counts, not ENMO in g
    i <- round(agg(n2$invalid, by))
    ggir <- GGIR:::HASPT(angle = agg(n2$angle, by), params_sleep = gd_params(), ws3 = w,
                         HASPT.algo = "MotionWare", invalid = i, activity = a, marker = NULL,
                         sibs = NULL)
    port <- raw.guider(angle = agg(n2$angle, by), params_sleep = gd_params(), ws3 = w,
                       HASPT.algo = "MotionWare", invalid = i, activity = a, marker = NULL,
                       sibs = NULL)
    expect_identical(port, ggir, info = paste("MotionWare ws3", w))
    expect_identical(names(port), c("SPTE_start", "SPTE_end", "tib.threshold", "part3_guider"))
    expect_identical(port$tib.threshold, 0)
    expect_identical(port$part3_guider, "MotionWare")
  }
  a60 <- agg(n2$activity, 12) * 1000
  m60 <- raw.guider(angle = agg(n2$angle, 12), params_sleep = gd_params(), ws3 = 60,
                    HASPT.algo = "MotionWare", invalid = round(agg(n2$invalid, 12)),
                    activity = a60)
  expect_identical(c(m60$SPTE_start, m60$SPTE_end), c(114, 1441))
  # two presses an hour either side of the found window snap both edges
  i60 <- round(agg(n2$invalid, 12))
  mk <- rep(0, length(a60)); mk[c(114 - 60, 1441 - 60)] <- 1
  snapped <- raw.guider(angle = agg(n2$angle, 12), params_sleep = gd_params(), ws3 = 60,
                        HASPT.algo = "MotionWare", invalid = i60, activity = a60, marker = mk)
  expect_identical(snapped,
                   GGIR:::HASPT(angle = agg(n2$angle, 12), params_sleep = gd_params(),
                                ws3 = 60, HASPT.algo = "MotionWare", invalid = i60,
                                activity = a60, marker = mk, sibs = NULL))
  expect_identical(c(snapped$SPTE_start, snapped$SPTE_end), c(54L, 1381L))
  expect_identical(snapped$part3_guider, "MotionWare")
  # a single press is ignored
  mk1 <- rep(0, length(a60)); mk1[54] <- 1
  expect_identical(raw.guider(angle = agg(n2$angle, 12), params_sleep = gd_params(), ws3 = 60,
                              HASPT.algo = "MotionWare", invalid = i60, activity = a60,
                              marker = mk1),
                   GGIR:::HASPT(angle = agg(n2$angle, 12), params_sleep = gd_params(),
                                ws3 = 60, HASPT.algo = "MotionWare", invalid = i60,
                                activity = a60, marker = mk1, sibs = NULL))
  # GGIR dies with "object 'score_threshold' not found" at any other epoch length
  expect_error(raw.guider(angle = n2$angle, params_sleep = gd_params(), ws3 = 5,
                          HASPT.algo = "MotionWare", invalid = n2$invalid,
                          activity = n2$activity),
               "15, 30 or 60 seconds")
  expect_error(GGIR:::HASPT(angle = n2$angle, params_sleep = gd_params(), ws3 = 5,
                            HASPT.algo = "MotionWare", invalid = n2$invalid,
                            activity = n2$activity, marker = NULL, sibs = NULL))
})

test_that("S2j the marker button pre-empts the algorithm, with GGIR's two defects", {
  skip_if_no_ggir()
  d <- gd_nights("MOS2")
  n2 <- d$nights[[2]]
  sibs <- gd_sibs("MOS2")[[2]]
  pp_on <- gd_params(consider_marker_button = TRUE)
  call_both <- function(marker, invalid = n2$invalid, pp = pp_on, ggir_exact = TRUE) {
    ggir <- GGIR:::HASPT(angle = n2$angle, params_sleep = pp, ws3 = 5, HASPT.algo = "HDCZA",
                         invalid = invalid, activity = n2$activity, marker = marker,
                         sibs = sibs)
    port <- raw.guider(angle = n2$angle, params_sleep = pp, ws3 = 5, HASPT.algo = "HDCZA",
                       invalid = invalid, activity = n2$activity, marker = marker,
                       sibs = sibs, ggir_exact = ggir_exact)
    if (isTRUE(ggir_exact)) expect_identical(port, ggir)
    port
  }
  # two presses win
  mk <- rep(0, length(n2$angle)); mk[c(8300, 14200)] <- 1
  two <- call_both(mk)
  expect_identical(names(two), c("SPTE_start", "SPTE_end", "tib.threshold", "part3_guider"))
  expect_identical(c(two$SPTE_start, two$SPTE_end), c(8300L, 14200L))
  expect_identical(two$tib.threshold, 0)
  expect_identical(two$part3_guider, "markerbutton")
  # with 2 presses pmax(2, 10) indexes 8 rows past the end; the NAs are survived
  bp <- which(mk != 0)
  ranking <- sort(mk[bp], decreasing = TRUE, index.return = TRUE)$ix
  expect_identical(sum(is.na(bp[ranking[1:pmax(length(which(mk == 1)), 10)]])), 8L)
  # one press falls through to HDCZA
  mk1 <- rep(0, length(n2$angle)); mk1[8300] <- 1
  one <- call_both(mk1)
  expect_identical(c(one$SPTE_start, one$SPTE_end), c(8251, 14299))
  expect_identical(one$part3_guider, "HDCZA")
  # more than half the window invalid falls through to HDCZA
  inv52 <- n2$invalid; inv52[1:round(0.52 * length(inv52))] <- 1
  half <- call_both(mk, invalid = inv52)
  expect_identical(half$part3_guider, "HDCZA")
  # consider_marker_button FALSE ignores the column entirely
  off <- call_both(mk, pp = gd_params())
  expect_identical(off$part3_guider, "HDCZA")
  # GGIR's hour guard multiplies where it should divide, so two presses 30 s apart are
  # accepted under ggir_exact and rejected with it off
  mk30 <- rep(0, length(n2$angle)); mk30[c(8300, 8306)] <- 1
  short <- call_both(mk30)
  expect_identical(c(short$SPTE_start, short$SPTE_end), c(8300L, 8306L))
  expect_identical(short$part3_guider, "markerbutton")
  fixed <- call_both(mk30, ggir_exact = FALSE)
  expect_identical(fixed$part3_guider, "HDCZA")
  expect_identical(fixed$SPTE_start, 8251)
  still <- call_both(mk, ggir_exact = FALSE)
  expect_identical(still$part3_guider, "markerbutton")
})

test_that("the LowAcc branch is identical() to the GGIR 3.3-9 clone", {
  skip_if_no_ggir_ref()
  src <- clone_haspt()
  skip_if_no_file(src)
  skip_if_no_ggir()
  ce <- new.env(parent = globalenv())
  sys.source(src, envir = ce)                        # HASPT of GGIR 3.3-9
  d <- gd_nights("MOS2")
  n2 <- d$nights[[2]]
  sibs <- gd_sibs("MOS2")[[2]]
  for (algo in c("LowAcc", "HDCZA", "HorAngle", "NotWorn", "HLRB", "notused")) {
    pp <- gd_params()
    ggir9 <- ce$HASPT(angle = n2$angle, params_sleep = pp, ws3 = 5, HASPT.algo = algo,
                      invalid = n2$invalid, activity = n2$activity, marker = NULL, sibs = sibs)
    port <- raw.guider(angle = n2$angle, params_sleep = pp, ws3 = 5, HASPT.algo = algo,
                       invalid = n2$invalid, activity = n2$activity, marker = NULL,
                       sibs = sibs)
    expect_identical(port, ggir9, info = paste("clone", algo))
  }
  low <- raw.guider(angle = n2$angle, params_sleep = gd_params(), ws3 = 5, HASPT.algo = "LowAcc",
                    invalid = n2$invalid, activity = n2$activity)
  expect_identical(c(low$SPTE_start, low$SPTE_end), c(5492, 17280))
  expect_identical(low$tib.threshold, 0.014)
  expect_identical(low$part3_guider, "LowAcc")
  # HDCZA_roll_windowsize is the other 3.3-9 addition; the default of 5 reproduces 3.3.6
  for (k in c(1, 3, 5, 10, 15)) {
    pp <- gd_params(HDCZA_roll_windowsize = k)
    expect_identical(raw.guider(angle = n2$angle, params_sleep = pp, ws3 = 5,
                                HASPT.algo = "HDCZA", invalid = n2$invalid,
                                activity = n2$activity),
                     ce$HASPT(angle = n2$angle, params_sleep = pp, ws3 = 5,
                              HASPT.algo = "HDCZA", invalid = n2$invalid,
                              activity = n2$activity, marker = NULL, sibs = NULL),
                     info = paste("roll_windowsize", k))
  }
})

test_that("S2c the SPTE interval is half open and the final epoch is unreachable", {
  pp <- gd_defaults()
  # one block, 601 to 1100 inclusive, inside a 2000-epoch window
  g <- raw.guider(angle = gd_horangle(2000, list(c(601, 1100))), params_sleep = pp, ws3 = 5,
                  HASPT.algo = "HorAngle", invalid = rep(0, 2000))
  expect_identical(g$SPTE_start, 601)      # the first epoch of the block
  expect_identical(g$SPTE_end, 1101)       # the first epoch after it
  expect_identical(g$SPTE_end - g$SPTE_start, 500)
  # the crude estimate writes 2 over [SPTE_start, SPTE_end] inclusive, one more than the block
  expect_identical(as.integer(table(g$spt_crude_estimate)[["2"]]), 501L)
  expect_identical(length(g$spt_crude_estimate), 2000L)

  # a block running to the end: rebuild_rle truncates to N, so SPTE_end is N, not N + 1
  e <- raw.guider(angle = gd_horangle(2000, list(c(1500, 2000))), params_sleep = pp, ws3 = 5,
                  HASPT.algo = "HorAngle", invalid = rep(0, 2000))
  expect_identical(e$SPTE_start, 1500)
  expect_identical(e$SPTE_end, 2000)       # N, not 2001
  expect_identical(e$SPTE_end - e$SPTE_start, 500)   # one epoch short of the true 501

  s <- raw.guider(angle = gd_horangle(2000, list(c(1, 700))), params_sleep = pp, ws3 = 5,
                  HASPT.algo = "HorAngle", invalid = rep(0, 2000))
  expect_identical(c(s$SPTE_start, s$SPTE_end), c(1, 701))
})

test_that("the block and gap rules use GGIR's comparison directions", {
  pp <- gd_defaults()
  # blocks with length <= (60/ws3) * spt_min_block_dur are removed: 360 goes, 361 stays
  keep <- function(len) {
    g <- raw.guider(angle = gd_horangle(6000, list(c(1001, 1000 + len), c(3001, 4000))),
                    params_sleep = pp, ws3 = 5, HASPT.algo = "HorAngle",
                    invalid = rep(0, 6000))
    "1" %in% names(table(g$spt_crude_estimate))      # the short block survived as a loser
  }
  expect_false(keep(359))
  expect_false(keep(360))
  expect_true(keep(361))
  # gaps with length < (60/ws3) * spt_max_gap_dur are filled: 719 is, 720 is not
  merged <- function(gap) {
    g <- raw.guider(angle = gd_horangle(6000, list(c(1001, 2000), c(2001 + gap, 3000 + gap))),
                    params_sleep = pp, ws3 = 5, HASPT.algo = "HorAngle",
                    invalid = rep(0, 6000))
    g$SPTE_end - g$SPTE_start
  }
  expect_identical(merged(719), 2719)      # 1000 + 719 + 1000
  expect_identical(merged(720), 1000)
  expect_identical(merged(721), 1000)

  # nomov is padded to c(0, nomov, 0) before the rle, so the first and last segments are never
  # no-movement blocks and a short block at either edge is removed like any other
  lead <- raw.guider(angle = gd_horangle(4000, list(c(1, 100), c(2001, 3000))),
                     params_sleep = pp, ws3 = 5, HASPT.algo = "HorAngle",
                     invalid = rep(0, 4000))
  expect_identical(c(lead$SPTE_start, lead$SPTE_end), c(2001, 3001))
  expect_false("1" %in% names(table(lead$spt_crude_estimate)))   # the 100 block was removed
  trail <- raw.guider(angle = gd_horangle(4000, list(c(1001, 2000), c(3951, 4000))),
                      params_sleep = pp, ws3 = 5, HASPT.algo = "HorAngle",
                      invalid = rep(0, 4000))
  expect_identical(c(trail$SPTE_start, trail$SPTE_end), c(1001, 2001))
  expect_false("1" %in% names(table(trail$spt_crude_estimate)))
  lead4 <- raw.guider(angle = gd_horangle(4000, list(c(1, 400), c(2001, 3000))),
                      params_sleep = pp, ws3 = 5, HASPT.algo = "HorAngle",
                      invalid = rep(0, 4000))
  expect_identical(as.integer(table(lead4$spt_crude_estimate)[["1"]]), 400L)
  # a leading gap merges with the pad into segment 1 and is never filled
  gap <- raw.guider(angle = gd_horangle(4000, list(c(101, 1200))), params_sleep = pp, ws3 = 5,
                    HASPT.algo = "HorAngle", invalid = rep(0, 4000))
  expect_identical(c(gap$SPTE_start, gap$SPTE_end), c(101, 1201))
})

test_that("S2b spt_max_gap_ratio only bites below 1, on MOS2 night 3", {
  # the branch needs a ratio below 1 and more than three segments; spt_min_block_dur = 5
  # leaves enough segments
  d <- gd_nights("MOS2")
  n3 <- d$nights[[3]]
  edges <- function(ratio) {
    pp <- gd_defaults(spt_min_block_dur = 5)
    pp["spt_max_gap_ratio"] <- list(ratio)
    g <- raw.guider(angle = n3$angle, params_sleep = pp, ws3 = 5, HASPT.algo = "HDCZA",
                    invalid = n3$invalid, activity = n3$activity)
    c(g$SPTE_start, g$SPTE_end)
  }
  expect_identical(edges(1), c(8061, 13731))
  expect_identical(edges(NULL), c(8061, 13731))
  for (r in c(0.9, 0.7, 0.5, 0.3, 0.1)) expect_identical(edges(r), c(8441, 13731))
})

test_that("S2a HDCZA_threshold: two elements clamp, one element does not", {
  d <- gd_nights("MOS2")
  n2 <- d$nights[[2]]
  run <- function(th) {
    pp <- gd_defaults(); pp["HDCZA_threshold"] <- list(th)
    raw.guider(angle = n2$angle, params_sleep = pp, ws3 = 5, HASPT.algo = "HDCZA",
               invalid = n2$invalid, activity = n2$activity)
  }
  # quantile(x, 0.10) is exactly 0 on this night, so the percentile and multiplier do not matter
  expect_identical(unname(run(NULL)$tib.threshold), 0.13)
  expect_identical(unname(run(c(10, 15))$tib.threshold), 0.13)
  expect_identical(unname(run(c(5, 15))$tib.threshold), 0.13)
  expect_identical(unname(run(c(10, 10))$tib.threshold), 0.13)
  expect_identical(unname(run(c(90, 15))$tib.threshold), 0.5)      # the upper clamp
  # a single value is used as given, even outside [0.13, 0.50]
  expect_identical(run(0.05)$tib.threshold, 0.05)
  expect_identical(c(run(0.05)$SPTE_start, run(0.05)$SPTE_end), c(8256, 14297))
  expect_identical(run(0.6)$tib.threshold, 0.6)
  expect_identical(c(run(0.6)$SPTE_start, run(0.6)$SPTE_end), c(8238, 14301))
})

test_that("S2d the rolling median is centred on an even window, with a literal zero pad", {
  set.seed(4)
  angle <- cumsum(stats::rnorm(1000))
  medabsdi <- function(a) stats::median(abs(diff(a)))
  x <- .raw.guider.hdcza(angle, ws3 = 5, HDCZA_roll_windowsize = 5,
                         HDCZA_threshold = c(10, 15))$x
  expect_identical(length(x), 1000L)
  # k1 = 60, so x[i] covers angle[(i - 29):(i + 30)]; 29 leading and 30 trailing epochs are 0
  expect_true(all(x[1:29] == 0))
  expect_true(all(x[971:1000] == 0))
  expect_identical(x[30], medabsdi(angle[1:60]))
  expect_identical(x[100], medabsdi(angle[71:130]))
  expect_identical(x[970], medabsdi(angle[941:1000]))
  # an odd window is symmetric
  xodd <- .raw.guider.hdcza(angle, ws3 = 5, HDCZA_roll_windowsize = 61 / 12)$x
  expect_identical(xodd[100], medabsdi(angle[70:130]))
  # the zero pad is below every threshold, so those epochs are always no movement
  thr <- .raw.guider.hdcza(angle, ws3 = 5)$threshold
  expect_gt(unname(thr), 0)
  expect_true(all(x[c(1:29, 971:1000)] < thr))
})

test_that(".raw.roll.medabsdi is identical() to zoo::rollapply of median(abs(diff))", {
  medabsdi <- function(angle) stats::median(abs(diff(angle)))
  zoo_roll <- function(a, k1) {
    tryCatch(zoo::rollapply(a, width = k1, FUN = medabsdi, fill = 0), error = function(e) conditionMessage(e))
  }
  ours <- function(a, k1) {
    tryCatch(.raw.roll.medabsdi(a, k1, medabsdi), error = function(e) conditionMessage(e))
  }
  set.seed(8)
  for (n in c(1L, 3L, 59L, 60L, 61L, 62L, 63L, 200L, 1439L, 17280L)) {
    # anglez-like: four decimals, a still stretch of ties
    a <- round(cumsum(stats::rnorm(n, 0, 3)) %% 180 - 90, 4)
    if (n > 1000) a[300:900] <- a[300]
    for (k1 in c(60, 12, 4, 2, 120, 61, 59, 5, 61 / 12 * 12, 60.5)) {
      expect_identical(ours(a, k1), zoo_roll(a, k1), label = paste("n", n, "k1", k1))
      ana <- a
      ana[ceiling(n / 2)] <- NA
      expect_identical(ours(ana, k1), zoo_roll(ana, k1), label = paste("NA, n", n, "k1", k1))
    }
    # Inf, -Inf and NaN go to zoo as well
    for (v in c(Inf, -Inf, NaN)) {
      ainf <- a
      ainf[ceiling(n / 2)] <- v
      expect_identical(ours(ainf, 60), zoo_roll(ainf, 60), label = paste(v, "n", n))
    }
    expect_identical(ours(stats::setNames(a, seq_len(n)), 60), zoo_roll(stats::setNames(a, seq_len(n)), 60))
  }
  # the guider itself on a night-length series at 5 s epochs (k1 = 60)
  a <- round(cumsum(stats::rnorm(8640, 0, 2)) %% 180 - 90, 4)
  g <- .raw.guider.hdcza(a, ws3 = 5)
  expect_identical(g$x, zoo_roll(a, 60))
  # a plain double series with an even width does not reach zoo
  local_mocked_bindings(rollapply = function(...) stop("zoo reached"), .package = "zoo")
  expect_no_error(.raw.roll.medabsdi(a, 60, medabsdi))
  expect_error(.raw.roll.medabsdi(a, 61, medabsdi), "zoo reached")
})

test_that("the internal helpers behave as GGIR's inner functions", {
  # adjustlength clips or pads with 0 (valid)
  expect_identical(.raw.guider.adjustlength(1:5, c(1, 1, 1, 1, 1, 1, 1)), c(1, 1, 1, 1, 1))
  expect_identical(.raw.guider.adjustlength(1:5, c(1, 1)), c(1, 1, 0, 0, 0))
  expect_identical(.raw.guider.adjustlength(1:5, rep(1, 5)), rep(1, 5))
  # rebuild_rle expands, truncates to N and re-encodes
  r <- rle(c(0, 1, 1, 1, 0, 0))
  r$values[2] <- 0
  rr <- .raw.guider.rebuild.rle(r, 6)
  expect_identical(rr$values, 0)
  expect_identical(rr$lengths, 6L)
  expect_identical(.raw.guider.rebuild.rle(rle(c(0, 1, 1, 0)), 3)$lengths, c(1L, 2L))
  p <- .raw.guider.params(list(spt_min_block_dur = 7, spt_max_gap_ratio = NULL))
  expect_identical(p[["spt_min_block_dur"]], 7)
  expect_null(p[["spt_max_gap_ratio"]])
  expect_identical(p[["spt_max_gap_dur"]], 60)
  expect_identical(sort(names(.raw.guider.params(NULL))),
                   sort(c("HASPT.ignore.invalid", "HDCZA_threshold", "HDCZA_roll_windowsize",
                          "HorAngle_threshold", "LowAcc_threshold", "spt_min_block_dur",
                          "spt_max_gap_dur", "spt_max_gap_ratio", "consider_marker_button")))
  expect_error(.raw.guider.params("nope"), "must be a list")
})

test_that("an unrecognised HASPT.algo raises a named error", {
  # GGIR dies with "object 'x' not found"; the port names the parameter instead
  angle <- gd_horangle(2000, list(c(601, 1100)))
  expect_error(raw.guider(angle = angle, ws3 = 5, HASPT.algo = "HDCZAxyz",
                          invalid = rep(0, 2000)),
               "unknown HASPT.algo")
  expect_error(raw.guider(angle = angle, ws3 = 5, HASPT.algo = "hdcza",
                          invalid = rep(0, 2000)),
               "unknown HASPT.algo")
  # a two-element value is the detector's to resolve, one element per night
  expect_error(raw.guider(angle = angle, ws3 = 5, HASPT.algo = c("NotWorn", "HDCZA"),
                          invalid = rep(0, 2000)),
               "HASPT.algo must be one of")
  expect_error(raw.guider(angle = angle, ws3 = 5, HASPT.algo = NA_character_,
                          invalid = rep(0, 2000)),
               "HASPT.algo must be one of")
})

test_that("the guider runs on its own defaults, with no parameter object", {
  n <- 17280
  angle <- rep(c(0, 30), length.out = n)
  angle[8001:14000] <- 10
  g <- raw.guider(angle = angle, ws3 = 5, invalid = rep(0, n))
  expect_identical(g$part3_guider, "HDCZA")
  expect_identical(unname(g$tib.threshold), 0.13)
  expect_identical(g$SPTE_start, 8001)
  expect_identical(g$SPTE_end, 14000)
  expect_identical(g$SPTE_end - g$SPTE_start, 5999)
})

# Only the comparison against GGIR is asserted on the fixtures: the DST offset correction and
# the day-sleeper re-run belong to the detector, so the stored ms3 hours are not.
test_that("raw.guider is identical() to GGIR HASPT on the study fixtures", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  cases <- c("daysleeper", "spring", "autumn", "autumn_edge")
  present <- cases[dir.exists(vapply(cases, function(c1) p34_fixture(c1), ""))]
  if (length(present) == 0) {
    testthat::skip("no ggir-study-p34 fixtures next to the reference folder")
  }
  pp <- GGIR::load_params()$params_sleep
  n_nights <- 0L
  for (case in present) {
    root <- p34_fixture(case)
    pb <- list.files(file.path(root, "meta", "basic"), full.names = TRUE)[1]
    if (is.na(pb) || !file.exists(pb)) next
    fn <- sub("^meta_", "", basename(pb))
    p2 <- file.path(root, "meta", "ms2.out", fn)
    p3 <- file.path(root, "meta", "ms3.out", fn)
    if (!file.exists(p2) || !file.exists(p3)) next
    d <- gd_build(pb, p2, p3)
    sleep <- GGIR:::HASIB(HASIB.algo = pp[["HASIB.algo"]], timethreshold = pp[["timethreshold"]],
                          anglethreshold = pp[["anglethreshold"]], time = d$time,
                          anglez = d$anglez, ws3 = d$ws3, zeroCrossingCount = c(),
                          NeishabouriCount = c(), activity = d$ACC,
                          oakley_threshold = pp[["oakley_threshold"]])
    for (k in seq_along(d$nights)) {
      nn <- d$nights[[k]]
      if (is.null(nn)) next
      sibs <- sleep[nn$qqq1:nn$qqq2, 1]
      for (algo in c("HDCZA", "NotWorn", "HLRB")) {
        expect_identical(raw.guider(angle = nn$angle, params_sleep = pp, ws3 = d$ws3,
                                    HASPT.algo = algo, invalid = nn$invalid,
                                    activity = nn$activity, sibs = sibs),
                         GGIR:::HASPT(angle = nn$angle, params_sleep = pp, ws3 = d$ws3,
                                      HASPT.algo = algo, invalid = nn$invalid,
                                      activity = nn$activity, marker = NULL, sibs = sibs),
                         info = paste(case, "night", k, algo))
      }
      n_nights <- n_nights + 1L
    }
    # a daylight saving transition gives a 23 h (16560 epoch) or 25 h (18000 epoch) window
    lens <- vapply(d$nights, function(nn) if (is.null(nn)) 0L else length(nn$angle), 0L)
    if (case == "spring") expect_true(16560L %in% lens)
    if (case %in% c("autumn", "autumn_edge")) expect_true(18000L %in% lens)
    if (case == "daysleeper") expect_true(17280L %in% lens)
  }
  expect_gt(n_nights, 0L)
})
