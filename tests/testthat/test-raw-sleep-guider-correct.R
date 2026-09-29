# Parity tests for R/raw_sleep_guider_correct.R against GGIR's g.part3_correct_guider and
# g.part3_alignIndexVectors. The correction is off by default in GGIR, so neither stored
# reference recording exercises it; most of the evidence is synthetic and compared against
# live GGIR. The MOS2 sib detection object is rebuilt with GGIR:::g.sib.det from the stored
# milestones in CANHRACTI_GGIR_REF, which were stored in the system zone of a machine set to
# America/Anchorage, so the file runs in that zone.

withr::local_timezone("America/Anchorage")

ggir_available <- requireNamespace("GGIR", quietly = TRUE)

# GGIR's load_params(topic = "sleep") defaults, transcribed so the tests do not need GGIR.
gc_params <- function(...) {
  p <- list(guider_cor_do = FALSE,
            guider_cor_maxgap_hrs = 2,
            guider_cor_min_frac_sib = 0.5,
            guider_cor_min_hrs = 2,
            guider_cor_meme_frac_out = 0.9,
            guider_cor_meme_frac_in = 0.4,
            guider_cor_meme_min_hrs = 1,
            guider_cor_meme_min_dys = 3)
  changes <- list(...)
  for (nm in names(changes)) p[nm] <- changes[nm]
  p
}

# A synthetic sib detection object: minute epochs from noon, nights running noon to noon,
# and per night a list of blocks c(start_hour, end_hour, crude_estimate_code) counted from
# the midnight that opens the night's date. Every marked block is sib.
gc_build_sle <- function(spec, nsib = 1, tz = "UTC", epochSize = 60,
                         startclock = "2024-03-01 12:00:00", ndays = 9) {
  start <- as.POSIXct(startclock, tz = tz)
  n <- ndays * (86400 / epochSize)
  tsec <- start + (0:(n - 1)) * epochSize
  iso <- format(tsec, "%Y-%m-%dT%H:%M:%S%z")
  clk <- format(tsec, "%H:%M:%S")
  hh <- as.numeric(substr(clk, 1, 2)) + as.numeric(substr(clk, 4, 5)) / 60
  dayi <- as.numeric(as.Date(format(tsec, "%Y-%m-%d")) - as.Date(format(start, "%Y-%m-%d"))) + 1
  night <- rep(0, n)
  for (j in 1:(ndays - 1)) night[which((dayi == j & hh >= 12) | (dayi == j + 1 & hh < 12))] <- j
  crude <- rep(0, n)
  sib <- rep(0, n)
  idxof <- function(day, h) which(dayi == day + (h >= 24) & abs(hh - (h %% 24)) < 1e-9)[1]
  SPTE_start <- SPTE_end <- rep(NA_real_, ndays - 1)
  for (j in 1:(ndays - 1)) {
    s <- spec[[as.character(j)]]
    if (is.null(s)) s <- spec[["default"]]
    for (b in s$blocks) {
      i0 <- idxof(j, b[1])
      i1 <- idxof(j, b[2])
      if (!is.na(i0) && !is.na(i1)) {
        crude[i0:(i1 - 1)] <- b[3]
        sib[i0:(i1 - 1)] <- 1
      }
    }
    SPTE_start[j] <- s$spte[1]
    SPTE_end[j] <- s$spte[2]
  }
  df <- data.frame(time = iso, invalid = 0, night = night, T5A5 = sib, stringsAsFactors = FALSE)
  if (nsib == 2) df$Sadeh1994 <- sib
  df$spt_crude_estimate <- crude
  list(output = df, SPTE_start = SPTE_start, SPTE_end = SPTE_end)
}

gc_default_night <- list(blocks = list(c(23.5, 31.5, 2)), spte = c(23.5, 31.5))

test_that(".raw.align.index.vectors reproduces GGIR's own unit test, all eleven cases", {
  N <- 280
  x <- c(1, 100, 200)
  y <- c(80, 180, 280)
  for (i in 1:11) {
    a <- c(20, 120, 220)
    b <- c(60, 160, 260)
    if (i == 2) {
      a <- c(20, 120, 220, 320)          # a one longer beyond N
    } else if (i == 3) {
      a <- c(20, 120, 220, 275)          # a one longer under N
    } else if (i == 4) {
      a <- c(20, 120, 220, 320)          # a and b one longer beyond N
      b <- c(60, 160, 260, 360)
    } else if (i == 5) {
      b <- c(60, 160, 260, N)            # b one longer equal to N
    } else if (i == 6) {
      a <- c(20, 120, 220, N)            # a one longer equal to N
    } else if (i == 7) {
      a <- c(120, 220, N)                # a and b with missing start, a equal to N
      b <- c(160, 260)
    } else if (i == 8) {
      a <- c(120, 220)                   # a and b with missing start
      b <- c(160, 260)
    } else if (i == 9) {
      a <- c(120, 220)                   # a with missing start
    } else if (i == 10) {
      a <- c(20, 120)                    # a and b with missing end
      b <- c(60, 160)
    } else if (i == 11) {
      b <- c(20, 120)                    # b with missing start
    }
    got <- .raw.align.index.vectors(x, y, a, b, N)
    # GGIR's invariants
    expect_equal(length(got$x), 3)
    expect_equal(length(got$y), 3)
    expect_equal(length(got$a), 3)
    expect_equal(length(got$b), 3)
    expect_true(all(got$x <= got$a))
    expect_true(all(got$a <= got$b))
    expect_true(all(got$b <= got$y))
    if (ggir_available) {
      expect_identical(got, GGIR:::g.part3_alignIndexVectors(x, y, a, b, N))
    }
  }
})

test_that(".raw.align.index.vectors applies each of the six rules with the expected values", {
  N <- 280
  x <- c(1, 100, 200)
  y <- c(80, 180, 280)
  # untouched
  expect_identical(.raw.align.index.vectors(x, y, c(20, 120, 220), c(60, 160, 260), N)$a,
                   c(20, 120, 220))
  # missing start of a and of b: a 1 is prepended, then the trailing element is trimmed
  got <- .raw.align.index.vectors(x, y, c(120, 220), c(160, 260), N)
  expect_identical(got$a, c(1, 120, 220))
  expect_identical(got$b, c(1, 160, 260))
  # missing end, with y ending at N: N is appended
  got <- .raw.align.index.vectors(x, y, c(20, 120), c(60, 160), N)
  expect_identical(got$a, c(20, 120, 280))
  expect_identical(got$b, c(60, 160, 280))
  # end past the end of y: trimmed
  expect_identical(.raw.align.index.vectors(x, y, c(20, 120, 220, 320), c(60, 160, 260), N)$a,
                   c(20, 120, 220))
  # longer than both x and y: trimmed
  expect_identical(.raw.align.index.vectors(x, y, c(20, 120, 220, 275), c(60, 160, 260), N)$a,
                   c(20, 120, 220))
})

test_that(".raw.guider.correct.dst inserts one index across a forward clock change", {
  # a gap wider than 25 h gets one synthetic index 24 h after the index before it
  x <- c(931, 2371, 3811, 6631, 8071)
  expect_identical(.raw.guider.correct.dst(x, 60), c(931, 2371, 3811, 5251, 6631, 8071))
  # only the first such gap is repaired, as in GGIR
  y <- c(100, 1540, 4400, 7300)
  expect_identical(.raw.guider.correct.dst(y, 60), c(100, 1540, 2980, 4400, 7300))
  expect_identical(.raw.guider.correct.dst(c(1, 1441, 2881), 60), c(1, 1441, 2881))
})

test_that(".raw.guider.matching.indices builds the clock strings and applies the two fixups", {
  tz <- "UTC"
  # a recording that starts at noon needs no fixup: both edges match once per night
  tsec <- as.POSIXct("2024-03-01 12:00:00", tz = tz) + (0:(3 * 1440 - 1)) * 60
  clk <- format(tsec, "%H:%M:%S")
  # 23.5 -> "23:30:00", 31.5 -> 7.5 -> "07:30:00"
  got <- .raw.guider.matching.indices(clk, c(23.5, 31.5), 60)
  expect_identical(got$ref_min, which(clk == "23:30:00"))
  expect_identical(got$ref_max, which(clk == "07:30:00"))
  expect_identical(length(got$ref_min), length(got$ref_max))
  # seconds are floored onto the epoch grid: 23.5 + 0.1/60 is still 23:30:00 at 60 s epochs
  expect_identical(.raw.guider.matching.indices(clk, c(23 + 30.1 / 60, 31.5), 60)$ref_min,
                   got$ref_min)
  # a recording that starts at midnight needs both fixups: a 1 is prepended to ref_min and
  # the last index is appended to ref_max
  tsec <- as.POSIXct("2024-03-01 00:00:00", tz = tz) + (0:(3 * 1440 - 1)) * 60
  clk <- format(tsec, "%H:%M:%S")
  got <- .raw.guider.matching.indices(clk, c(23.5, 31.5), 60)
  # GGIR prepends a numeric 1, so ref_min becomes double while ref_max stays integer
  expect_identical(got$ref_min, c(1, which(clk == "23:30:00")))
  expect_identical(got$ref_max, c(which(clk == "07:30:00"), length(clk)))
  expect_identical(length(got$ref_min), length(got$ref_max))
})

test_that(".raw.guider.ts.to.hours takes the inclusive range and reconciles the pair", {
  clk <- c("23:30:00", "23:31:00", "00:00:00", "07:29:00", "07:30:00")
  crude <- c(1, 1, 0, 1, 0)
  # range across both 1-blocks, wake gap included; 07:29 is at or below 12 so it gains 24
  expect_equal(.raw.guider.ts.to.hours(clk, crude, 1:5, ref_value = 1), c(23.5, 31.483))
  # a single epoch block gives start equal to end
  expect_equal(.raw.guider.ts.to.hours(clk, c(0, 0, 0, 2, 0), 1:5, ref_value = 2),
               c(31.483, 31.483))
})

test_that(".raw.guider.crude.estimate drops 1-blocks below the hardcoded 0.8 mean sib", {
  out <- data.frame(time = "x", invalid = 0, night = 1,
                    T5A5 = c(rep(1, 5), rep(0, 5), rep(1, 4), 0, rep(1, 5)),
                    spt_crude_estimate = c(rep(1, 10), rep(1, 5), rep(2, 5)),
                    stringsAsFactors = FALSE)
  # block 1: epochs 1 to 15, mean sib = 10/15 = 0.667, below 0.8, so blanked to 0
  got <- .raw.guider.crude.estimate(out, 1:20)
  expect_identical(got, c(rep(0, 15), rep(2, 5)))
  # raise the sib of the same block above 0.8 and it survives
  out$T5A5 <- c(rep(1, 13), 0, 0, rep(1, 5))
  expect_identical(.raw.guider.crude.estimate(out, 1:20), c(rep(1, 15), rep(2, 5)))
  # a 2-block alone is never touched
  out$spt_crude_estimate <- c(rep(0, 15), rep(2, 5))
  expect_identical(.raw.guider.crude.estimate(out, 1:20), c(rep(0, 15), rep(2, 5)))
})

test_that(".raw.guider.clean removes the four temporary columns", {
  out <- data.frame(time = "x", invalid = 0, night = 1, T5A5 = 1, spt_crude_estimate = 0,
                    time_POSIX = Sys.time(), clocktime = "00:00:00", medmed = 0,
                    stringsAsFactors = FALSE)
  got <- .raw.guider.clean(list(output = out))
  expect_identical(colnames(got$output), c("time", "invalid", "night", "T5A5"))
})

test_that("raw.guider.correct produces GGIR's four correction codes on synthetic nights", {
  tz <- "UTC"
  run <- function(spec, params = gc_params(), nsib = 1) {
    sle <- gc_build_sle(spec, nsib = nsib, tz = tz)
    got <- raw.guider.correct(sle, desiredtz = tz, epochSize = 60, params_sleep = params)
    if (ggir_available) {
      expect_identical(got, GGIR:::g.part3_correct_guider(sle, desiredtz = tz, epochSize = 60,
                                                          params_sleep = params))
    }
    got
  }

  # step 4 only: a 1 h gap then a 3 h rest block; the guider expands to its last epoch, 30.983
  got <- run(list(default = gc_default_night,
                  "7" = list(blocks = list(c(23.5, 27, 2), c(28, 31, 1)), spte = c(23.5, 27))))
  expect_identical(got$SPTE_corrected, c(0, 0, 0, 0, 0, 0, 1, 0))
  expect_equal(got$SPTE_start[7], 23.5)
  expect_equal(got$SPTE_end[7], 30.983)
  expect_equal(got$SPTE_start[-7], rep(23.5, 7))
  expect_equal(got$SPTE_end[-7], rep(31.5, 7))
  # the temporary columns, spt_crude_estimate included, are gone
  expect_identical(colnames(got$output), c("time", "invalid", "night", "T5A5"))

  # step 3 only: the main block is a daytime nap and the night rest is the candidate
  got <- run(list(default = gc_default_night,
                  "4" = list(blocks = list(c(14, 18, 2), c(23.75, 30, 1)), spte = c(14, 18))))
  expect_identical(got$SPTE_corrected, c(0, 0, 0, 2, 0, 0, 0, 0))
  expect_equal(got$SPTE_start[4], 23.75)
  expect_equal(got$SPTE_end[4], 29.983)

  # both: step 3 moves the window and step 4 expands it; GGIR warns because two candidates qualify
  spec <- list(default = gc_default_night,
               "4" = list(blocks = list(c(14, 18, 2), c(23.75, 27, 1), c(28, 30.5, 1)),
                          spte = c(14, 18)))
  expect_warning(got <- run(spec), "only the first used")
  expect_identical(got$SPTE_corrected, c(0, 0, 0, 3, 0, 0, 0, 0))
  expect_equal(got$SPTE_start[4], 23.75)
  expect_equal(got$SPTE_end[4], 30.483)

  # nothing corrected: a 2.5 h wake gap reaches guider_cor_maxgap_hrs
  got <- run(list(default = gc_default_night,
                  "7" = list(blocks = list(c(23.5, 26.5, 2), c(29, 31.5, 1)),
                             spte = c(23.5, 26.5))))
  expect_identical(got$SPTE_corrected, rep(0, 8))
  expect_equal(got$SPTE_end[7], 26.5)
})

test_that("raw.guider.correct honours every parameter gate of the correction", {
  tz <- "UTC"
  run <- function(spec, params, nsib = 1) {
    sle <- gc_build_sle(spec, nsib = nsib, tz = tz)
    got <- raw.guider.correct(sle, desiredtz = tz, epochSize = 60, params_sleep = params)
    if (ggir_available) {
      expect_identical(got, GGIR:::g.part3_correct_guider(sle, desiredtz = tz, epochSize = 60,
                                                          params_sleep = params))
    }
    got
  }
  gap_spec <- list(default = gc_default_night,
                   "7" = list(blocks = list(c(23.5, 26.5, 2), c(29, 31.5, 1)),
                              spte = c(23.5, 26.5)))
  near_spec <- list(default = gc_default_night,
                    "7" = list(blocks = list(c(23.5, 27, 2), c(28, 31, 1)), spte = c(23.5, 27)))

  # guider_cor_maxgap_hrs switched off by Inf or by NULL: the same 2.5 h gap now passes
  got <- run(gap_spec, gc_params(guider_cor_maxgap_hrs = Inf))
  expect_identical(got$SPTE_corrected, c(0, 0, 0, 0, 0, 0, 1, 0))
  expect_equal(got$SPTE_end[7], 31.483)
  got <- run(gap_spec, gc_params(guider_cor_maxgap_hrs = NULL))
  expect_identical(got$SPTE_corrected, c(0, 0, 0, 0, 0, 0, 1, 0))
  expect_equal(got$SPTE_end[7], 31.483)

  # a rest block shorter than guider_cor_min_hrs is not long rest
  got <- run(list(default = gc_default_night,
                  "7" = list(blocks = list(c(23.5, 27, 2), c(28, 29.5, 1)), spte = c(23.5, 27))),
             gc_params())
  expect_identical(got$SPTE_corrected, rep(0, 8))
  # the 3 h block fails once the minimum is raised to 4 h
  got <- run(near_spec, gc_params(guider_cor_min_hrs = 4))
  expect_identical(got$SPTE_corrected, rep(0, 8))
  expect_equal(got$SPTE_end[7], 27)

  # guider_cor_meme_min_dys above the number of valid nights skips steps 2 and 3
  got <- run(list(default = gc_default_night,
                  "4" = list(blocks = list(c(14, 18, 2), c(23.75, 30, 1)), spte = c(14, 18))),
             gc_params(guider_cor_meme_min_dys = 99))
  expect_identical(got$SPTE_corrected, rep(0, 8))
  expect_equal(got$SPTE_start[4], 14)
  expect_equal(got$SPTE_end[4], 18)
})

test_that("raw.guider.correct repairs the missing index of a forward clock change", {
  tz <- "Europe/Helsinki"
  # nine days across the 2024-03-31 spring forward, so the 03:30 clock time is missing once
  spec <- list(default = list(blocks = list(c(23.5, 27.5, 2)), spte = c(23.5, 27.5)),
               "6" = list(blocks = list(c(23.5, 26.5, 2), c(27.5, 30, 1)), spte = c(23.5, 26.5)))
  sle <- gc_build_sle(spec, tz = tz, startclock = "2024-03-27 12:00:00")
  clk <- format(.raw.iso8601.to.posix(sle$output$time, tz = tz), format = "%H:%M:%S")
  expect_identical(length(which(clk == "03:30:00")), 8L)
  expect_true(2820 %in% diff(which(clk == "03:30:00")))    # the 47 h gap across the change
  idx <- .raw.guider.matching.indices(clk, c(23.5, 27.5), 60)
  expect_identical(length(idx$ref_min), 9L)
  # eight raw matches, one appended for the short last night, one inserted for the clock change
  expect_identical(length(idx$ref_max), 10L)
  expect_true(5251 %in% idx$ref_max)
  got <- raw.guider.correct(sle, desiredtz = tz, epochSize = 60, params_sleep = gc_params())
  expect_identical(got$SPTE_corrected, rep(0, 8))
  if (ggir_available) {
    expect_identical(got, GGIR:::g.part3_correct_guider(sle, desiredtz = tz, epochSize = 60,
                                                        params_sleep = gc_params()))
  }
})

test_that("more than one sib definition crashes as GGIR does, and is refused when not exact", {
  tz <- "UTC"
  spec <- list(default = gc_default_night,
               "7" = list(blocks = list(c(23.5, 27, 2), c(28, 31, 1)), spte = c(23.5, 27)))
  sle <- gc_build_sle(spec, nsib = 2, tz = tz)
  # GGIR indexes a two column data.frame by column and crashes
  expect_error(raw.guider.correct(sle, desiredtz = tz, epochSize = 60,
                                  params_sleep = gc_params()),
               "undefined columns selected")
  if (ggir_available) {
    expect_error(GGIR:::g.part3_correct_guider(sle, desiredtz = tz, epochSize = 60,
                                               params_sleep = gc_params()),
                 "undefined columns selected")
  }
  expect_error(raw.guider.correct(sle, desiredtz = tz, epochSize = 60,
                                  params_sleep = gc_params(ggir_exact = FALSE)),
               "exactly one sustained inactivity definition")
})

test_that("several step 3 candidates keep GGIR's first-one-only behaviour, with a clearer warning", {
  tz <- "UTC"
  spec <- list(default = gc_default_night,
               "4" = list(blocks = list(c(14, 18, 2), c(23.75, 27, 1), c(28, 30.5, 1)),
                          spte = c(14, 18)))
  sle <- gc_build_sle(spec, tz = tz)
  expect_warning(exact <- raw.guider.correct(sle, desiredtz = tz, epochSize = 60,
                                             params_sleep = gc_params()),
                 "only the first used")
  expect_warning(loose <- raw.guider.correct(sle, desiredtz = tz, epochSize = 60,
                                             params_sleep = gc_params(ggir_exact = FALSE)),
                 "candidate resting blocks")
  # the numbers do not change, only the message
  expect_identical(loose$SPTE_start, exact$SPTE_start)
  expect_identical(loose$SPTE_end, exact$SPTE_end)
  expect_identical(loose$SPTE_corrected, exact$SPTE_corrected)
})

test_that("on MOS2 the correction changes nothing and every code is zero", {
  ref <- Sys.getenv("CANHRACTI_GGIR_REF")
  if (!nzchar(ref)) skip("CANHRACTI_GGIR_REF not set")
  if (!ggir_available) skip("GGIR is not installed")
  meta <- file.path(ref, "out", "output_din", "meta")
  fn <- "MOS2E39230594.gt3x"
  paths <- c(file.path(meta, "basic", paste0("meta_", fn, ".RData")),
             file.path(meta, "ms2.out", paste0(fn, ".RData")),
             file.path(meta, "ms3.out", paste0(fn, ".RData")))
  if (!all(file.exists(paths))) skip("MOS2 milestones not available")
  e <- lapply(paths, function(p) {
    env <- new.env(parent = emptyenv())
    load(p, envir = env)
    env
  })
  # GGIR's own sib detection, with the parameters g.part3 passes
  P <- GGIR::load_params(topic = c("sleep", "metrics", "general"))
  sle <- GGIR:::g.sib.det(M = e[[1]]$M, IMP = e[[2]]$IMP, I = e[[1]]$I, twd = c(-12, 12),
                          acc.metric = P$params_general$acc.metric,
                          desiredtz = e[[3]]$desiredtz_part1, myfun = c(),
                          sensor.location = P$params_general$sensor.location,
                          params_sleep = P$params_sleep, zc.scale = P$params_metrics$zc.scale)
  tz <- "America/Anchorage"
  params <- gc_params()
  got <- raw.guider.correct(sle, desiredtz = tz, epochSize = 5, params_sleep = params)
  # MOS2 has two valid nights, below guider_cor_meme_min_dys, so steps 2 and 3 never run
  expect_identical(got$SPTE_start, sle$SPTE_start)
  expect_identical(got$SPTE_end, sle$SPTE_end)
  expect_identical(got$SPTE_corrected, rep(0, 7))
  expect_identical(colnames(got$output), c("time", "invalid", "night", "T5A5"))
  # the valid night selection itself
  ipn <- aggregate(x = sle$output$invalid, by = list(sle$output$night), FUN = sum)
  names(ipn) <- c("night", "count")
  valid <- ipn$night[which(ipn$night > 0 & ipn$count < 24 * 3600 / 5 * 0.333)]
  valid <- valid[which(is.na(sle$SPTE_start[valid]) == FALSE & is.na(sle$SPTE_end[valid]) == FALSE)]
  expect_identical(valid, c(2, 3))
  expect_identical(got, GGIR:::g.part3_correct_guider(sle, desiredtz = tz, epochSize = 5,
                                                      params_sleep = params))
})

test_that("the stored MOS2 milestone has no SPTE_corrected, the correction being off by default", {
  ref <- Sys.getenv("CANHRACTI_GGIR_REF")
  if (!nzchar(ref)) skip("CANHRACTI_GGIR_REF not set")
  ms3 <- file.path(ref, "out", "output_din", "meta", "ms3.out", "MOS2E39230594.gt3x.RData")
  if (!file.exists(ms3)) skip("MOS2 ms3 milestone not available")
  e <- new.env()
  load(ms3, envir = e)
  expect_null(e$SPTE_corrected)
  expect_equal(e$SPTE_start,
               c(NA, 23.4597222222222, 23.7236111111111, 22.0402777777778, NA, NA, NA),
               tolerance = 1e-9)
  ee <- file.path(ref, "timing_out", "output_timing", "meta", "ms3.out",
                  "EE_left_29.5.2017-05-30.gt3x.RData")
  if (!file.exists(ee)) skip("EE ms3 milestone not available")
  e2 <- new.env()
  load(ee, envir = e2)
  expect_null(e2$SPTE_corrected)
  expect_equal(e2$SPTE_start,
               c(22.2777777777778, 23.6222222222222, 25.0222222222222, 24.3333333333333,
                 24.2847222222222, 22.7763888888889, NA),
               tolerance = 1e-9)
})
