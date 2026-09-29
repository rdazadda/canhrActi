# .raw.gini, .raw.frag.transprob, .raw.fragmentation and .raw.intensity.gradient against
# GGIR's g.fragmentation, g.intensitygradient and ineq::Gini. Reference data come from
# CANHRACTI_GGIR_REF; the stored MOS2 ms5 output has no FRAG columns (its run had
# frag.metrics = c()), so three further sets live beside it in ggir-study-p5: a re-run with
# frag.metrics = "all" (state_MM.rds, state_WW.rds), the same recording aggregated to 60 s
# with GGIR's own fragmentation list (h2h_state.rds), and a re-run with iglevels = 1 for the
# intensity gradient. Tests skip when a file is missing and live comparisons when GGIR is
# not installed. Parity is identical() everywhere.

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
# The part-5 study folder, one level up from the reference folder
p5_file <- function(...) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p5", ...)
}
# The GGIR 3.3-9 clone, also one level up
clone_file <- function(...) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-src", "GGIR", "R", ...)
}

load_rds <- function(path) {
  skip_if_no_file(path)
  readRDS(path)
}
load_rdata <- function(path) {
  skip_if_no_file(path)
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  e
}

# The stored MOS2 part-5 time series; the window column belongs to the last timewindow GGIR
# processed, which is WW.
mos2_series <- function() {
  load_rdata(ref_file("out", "output_din", "meta", "ms5.outraw", "40_100_400",
                      "MOS2E39230594_T5A5.RData"))
}

# The eighteen class names GGIR writes at the defaults, in GGIR's order.
LNAMES18 <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
              "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt",
              "day_MVPA_bts_10", "day_MVPA_bts_5_10", "day_MVPA_bts_1_5",
              "day_IN_bts_30", "day_IN_bts_20_30", "day_IN_bts_10_20",
              "day_LIG_bts_10", "day_LIG_bts_5_10", "day_LIG_bts_1_5")

# The thirty output names, in the order R's right-to-left evaluation of GGIR's chained NA
# pre-allocations gives them.
FRAG30 <- c("Nfrag_PA2IN", "Nfrag_IN2PA", "TP_PA2IN", "TP_IN2PA",
            "Nfrag_IN2LIPA", "TP_IN2LIPA", "Nfrag_IN2MVPA", "TP_IN2MVPA",
            "mean_dur_LIPA", "Nfrag_LIPA", "mean_dur_MVPA", "Nfrag_MVPA",
            "Nfrag_PA", "Nfrag_IN", "mean_dur_IN", "mean_dur_PA",
            "Gini_dur_IN", "Gini_dur_PA", "CoV_dur_IN", "CoV_dur_PA",
            "alpha_dur_IN", "alpha_dur_PA", "x0.5_dur_IN", "x0.5_dur_PA",
            "W0.5_dur_IN", "W0.5_dur_PA", "SD_dur_IN", "SD_dur_PA",
            "NFragPM_PA", "NFragPM_IN")

# The xmin warning is the subject of one test and noise in the others.
frag <- function(...) suppressWarnings(.raw.fragmentation(..., warn_xmin = FALSE))

# GGIR's nested TransProb, lifted out of the body of g.fragmentation.
ggir_transprob <- function() {
  skip_if_no_ggir()
  fn <- tryCatch({
    ev <- new.env(parent = asNamespace("GGIR"))
    eval(body(GGIR:::g.fragmentation)[[2]], ev)
    get("TransProb", envir = ev)
  }, error = function(e) NULL)
  if (!is.function(fn)) {
    testthat::skip("could not lift TransProb out of GGIR's g.fragmentation body")
  }
  fn
}

# One day window as GGIR hands it to g.fragmentation: the waking subset of LEVELS.
waking_levels <- function(mdat, levels_vec, wi) {
  sse <- which(mdat$window == wi)
  levels_vec[sse[mdat$SleepPeriodTime[sse] == 0]]
}

test_that(".raw.gini is identical() to ineq::Gini over 2000 random vectors", {
  skip_if_not_installed("ineq")
  set.seed(1)
  n_checked <- 0L
  n_bad <- 0L
  first_bad <- NULL
  for (i in 1:2000) {
    x <- round(stats::rexp(sample(2:500, 1), 1 / stats::runif(1, 1, 100))) + 1
    for (cr in c(TRUE, FALSE)) {
      a <- ineq::Gini(x, corr = cr)
      b <- .raw.gini(x, corr = cr)
      n_checked <- n_checked + 1L
      if (!identical(a, b)) {
        n_bad <- n_bad + 1L
        if (is.null(first_bad)) first_bad <- c(i = i, corr = cr, ineq = a, raw = b)
      }
    }
  }
  expect_identical(n_checked, 4000L)
  expect_identical(n_bad, 0L, info = paste(names(first_bad), first_bad, collapse = " "))
})

test_that(".raw.gini reproduces ineq's NaN on the degenerate cases, where .calculate.gini gives NA", {
  # n = 1 and an all-zero vector both give NaN, not NA
  expect_true(is.nan(.raw.gini(5, corr = TRUE)))
  expect_true(is.nan(.raw.gini(c(0, 0, 0), corr = TRUE)))
  expect_identical(.raw.gini(c(1, NA, 3), na.rm = FALSE), NA_real_)
  expect_identical(.raw.gini(c(1, NA, 3), na.rm = TRUE), .raw.gini(c(1, 3), na.rm = TRUE))
  # .calculate.gini returns NA there
  expect_true(is.na(.calculate.gini(5)) && !is.nan(.calculate.gini(5)))
  expect_true(is.na(.calculate.gini(c(0, 0, 0))) && !is.nan(.calculate.gini(c(0, 0, 0))))
})

test_that(".raw.gini on the degenerate cases is identical() to ineq::Gini", {
  skip_if_not_installed("ineq")
  expect_identical(.raw.gini(5, corr = TRUE), ineq::Gini(5, corr = TRUE))
  expect_identical(.raw.gini(c(0, 0, 0), corr = TRUE), ineq::Gini(c(0, 0, 0), corr = TRUE))
})

test_that(".calculate.gini is measurably not bit-identical to .raw.gini, so it cannot be reused", {
  # 1258 of 2000 random vectors disagree, always in the 16th significant digit
  set.seed(1)
  n_diff <- 0L
  max_abs <- 0
  for (i in 1:2000) {
    x <- round(stats::rexp(sample(2:500, 1), 1 / stats::runif(1, 1, 100))) + 1
    a <- .raw.gini(x, corr = TRUE)
    b <- .calculate.gini(x)
    if (!identical(a, b)) {
      n_diff <- n_diff + 1L
      max_abs <- max(max_abs, abs(a - b))
    }
  }
  expect_identical(n_diff, 1258L)
  expect_lt(max_abs, 1e-15)
  expect_gt(max_abs, 0)
})

test_that(".raw.gini reproduces the MOS2 inactivity Gini to the last bit", {
  DurIN <- c(6L, 2L, 3L, 168L, 1L, 1L, 1L, 135L, 3L, 1L, 6L, 2L, 4L, 25L,
             94L, 14L, 8L, 6L, 2L, 2L, 3L, 5L, 1L, 1L, 3L, 2L, 2L, 115L, 46L,
             131L, 1L, 2L, 6L)
  # the 33 inactivity fragment durations of MOS2 WW window 2 at 60 s epochs
  expect_identical(length(DurIN), 33L)
  expect_identical(sum(DurIN), 802L)
  expect_identical(.raw.gini(DurIN, corr = TRUE), 0.78631546134663344)
})


test_that(".raw.frag.transprob is identical() to GGIR's nested TransProb on 3500 random cases", {
  skip_if_no_ggir()
  TPg <- ggir_transprob()
  set.seed(7)
  pairs <- list(list(1, c(2, 3)), list(1, 2), list(1, 3), list(2, c(1, 3)),
                list(3, c(1, 2)), list(1, 0), list(0, 1))
  n_checked <- 0L
  n_bad <- 0L
  for (i in 1:500) {
    x <- sample(c(0L, 1L, 2L, 3L, NA_integer_), sample(1:60, 1), replace = TRUE,
                prob = c(0.3, 0.3, 0.2, 0.15, 0.05))
    for (ab in pairs) {
      a <- TPg(x, a = ab[[1]], b = ab[[2]])
      b <- .raw.frag.transprob(x, a = ab[[1]], b = ab[[2]])
      n_checked <- n_checked + 1L
      if (!identical(a, b)) {
        n_bad <- n_bad + 1L
        if (n_bad == 1L) {
          cat("\nfirst TransProb mismatch, series:\n"); print(x)
          cat("a ="); print(ab[[1]]); cat("b ="); print(ab[[2]])
          cat("GGIR:\n"); print(a); cat("port:\n"); print(b)
        }
      }
    }
  }
  expect_identical(n_checked, 3500L)
  expect_identical(n_bad, 0L)
})

test_that(".raw.frag.transprob keeps the two meanings of totDur_ab (trap 55)", {
  # runs a4 b1 a4 b1 a4: only the first two a-runs are followed by b
  x <- c(rep(1L, 4), 0L, rep(1L, 4), 0L, rep(1L, 4))
  out <- .raw.frag.transprob(x, a = 1, b = 0)
  expect_identical(out$Nab, 2L)                 # transitions, not runs
  expect_identical(out$Nba, 2L)
  # the returned totDur_ab counts only the a-runs followed by b
  expect_identical(out$totDur_ab, 8L)
  # TPab's denominator is all 12 minus the terminal run, with epsilon 1e-6
  expect_identical(out$TPab, round((2 + 1e-6) / (11 + 1e-6), digits = 6))
  expect_identical(out$TPba, round((2 + 1e-6) / (2 + 1e-6), digits = 6))
  expect_identical(out$TPba, 1)                 # the epsilon cancels
})

test_that(".raw.frag.transprob hard-codes the single-run and empty cases", {
  expect_identical(.raw.frag.transprob(rep(1L, 10), a = 1, b = 0)[c("TPab", "TPba", "Nab", "Nba")],
                   list(TPab = 0, TPba = 1, Nab = 1, Nba = 0))
  expect_identical(.raw.frag.transprob(rep(0L, 10), a = 1, b = 0)[c("TPab", "TPba", "Nab", "Nba")],
                   list(TPab = 1, TPba = 0, Nab = 0, Nba = 1))
  expect_identical(.raw.frag.transprob(rep(1L, 10), a = 1, b = 0)$totDur_ab, 10L)
  empty <- .raw.frag.transprob(integer(0), a = 1, b = 0)
  expect_identical(empty, list(TPab = NA, TPba = NA, Nab = 0, Nba = 0,
                               totDur_ab = 0, totDur_ba = 0))
})


test_that(".raw.fragmentation is identical() to g.fragmentation on MOS2, mode day", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- mos2_series()
  mdat <- s$mdat
  expect_identical(dim(mdat), c(118800L, 14L))
  expect_identical(s$Lnames, LNAMES18)
  n_win <- 0L
  for (wi in 1:3) {
    LEV <- waking_levels(mdat, mdat$class_id, wi)
    a <- GGIR:::g.fragmentation(frag.metrics = "all", LEVELS = LEV, Lnames = s$Lnames,
                                xmin = 60 / 5, mode = "day")
    b <- frag(frag.metrics = "all", LEVELS = LEV, Lnames = s$Lnames,
              xmin = 60 / 5, mode = "day")
    expect_identical(length(a), 30L)
    expect_identical(names(b), FRAG30)
    if (!identical(a, b)) {
      for (nm in union(names(a), names(b))) {
        if (!identical(a[[nm]], b[[nm]])) {
          cat("\nWW", wi, nm, "GGIR", format(a[[nm]], digits = 17),
              "port", format(b[[nm]], digits = 17), "\n")
        }
      }
    }
    expect_identical(b, a)
    n_win <- n_win + 1L
  }
  expect_identical(n_win, 3L)
  # the number of waking epochs GGIR feeds in
  expect_identical(sum(mdat$SleepPeriodTime == 0), 100897L)
})

test_that(".raw.fragmentation is identical() to g.fragmentation on MOS2, mode spt", {
  # mode = "spt" is part-6 only, ported anyway
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- mos2_series()
  mdat <- s$mdat
  for (wi in 1:3) {
    sse <- which(mdat$window == wi)
    LEV <- mdat$class_id[sse[mdat$SleepPeriodTime[sse] == 1]]
    a <- GGIR:::g.fragmentation(frag.metrics = "all", LEVELS = LEV, Lnames = s$Lnames,
                                xmin = 60 / 5, mode = "spt")
    b <- frag(frag.metrics = "all", LEVELS = LEV, Lnames = s$Lnames,
              xmin = 60 / 5, mode = "spt")
    expect_identical(length(a), 8L)
    expect_identical(names(b), c("Nfrag_IN", "Nfrag_PA", "TP_IN2PA", "TP_PA2IN",
                                 "Nfrag_sleep", "Nfrag_wake", "TP_sleep2wake",
                                 "TP_wake2sleep"))
    expect_identical(b, a)
  }
})

test_that(".raw.fragmentation reproduces the 30 stored FRAG columns on all 7 MOS2 rows (P5m)", {
  # the call site applies round(as.numeric(x), 6) and stores a character matrix
  skip_if_no_ggir_ref()
  n_rows <- 0L
  n_cells <- 0L
  n_bad <- 0L
  for (tw in c("MM", "WW")) {
    S <- load_rds(p5_file(paste0("state_", tw, ".rds")))
    out <- S$out
    expect_identical(ncol(out), 149L)
    expect_identical(grep("^FRAG_", names(out), value = TRUE), paste0("FRAG_", FRAG30, "_day"))
    expect_identical(S$ws3, 5)
    for (r in seq_len(nrow(out))) {
      wi <- as.numeric(out$window_number[r])
      LEV <- waking_levels(S$mdat, S$LL$LEVELS, wi)
      fo <- frag(frag.metrics = "all", LEVELS = LEV, Lnames = S$LL$Lnames,
                 xmin = 60 / S$ws3, mode = "day")
      mine <- as.character(round(as.numeric(fo), digits = 6))
      stored <- as.character(out[r, paste0("FRAG_", names(fo), "_day")])
      n_cells <- n_cells + length(mine)
      if (!identical(mine, stored)) {
        n_bad <- n_bad + sum(mine != stored)
        for (i in which(mine != stored)) {
          cat("\n", tw, "row", r, names(fo)[i], "port", mine[i], "stored", stored[i], "\n")
        }
      }
      expect_identical(mine, stored)
      n_rows <- n_rows + 1L
    }
  }
  expect_identical(n_rows, 7L)     # 4 MM windows plus 3 WW windows
  expect_identical(n_cells, 210L)  # 7 rows by 30 columns
  expect_identical(n_bad, 0L)
})


frag_cases <- list(
  empty          = integer(0),
  one_epoch      = 5L,
  two_same       = c(5L, 5L),
  all_IN         = rep(5L, 50),
  all_LIG        = rep(6L, 50),
  all_MVPA       = rep(7L, 50),
  one_fragment   = c(rep(5L, 20), rep(6L, 20)),
  alternating2   = rep(c(5L, 6L), 30),
  alternating3   = rep(c(5L, 6L, 7L), 40),
  nine_fragments = rep(c(5L, 6L), length.out = 9),
  ten_fragments  = rep(c(5L, 6L), length.out = 10),
  with_NA        = c(rep(5L, 10), rep(NA_integer_, 5), rep(6L, 10), rep(5L, 10)),
  unknown_id     = c(rep(0L, 10), rep(5L, 10), rep(6L, 10)),
  bouted_classes = c(rep(12L, 30), rep(15L, 10), rep(9L, 5), rep(13L, 40), rep(16L, 6)),
  zero_sd_IN     = rep(c(5L, 5L, 6L), 40),
  ends_on_PA     = c(rep(c(5L, 5L, 6L), 20), 6L, 6L, 6L)
)

test_that(".raw.fragmentation is identical() to g.fragmentation on 16 synthetic edge cases", {
  skip_if_no_ggir()
  set.seed(5)
  cases <- c(frag_cases, list(
    random_flat = sample(c(5L, 6L, 7L, 8L, 12L, 13L, 15L, 16L), 500, replace = TRUE),
    random_runs = rep(sample(c(5L, 6L, 7L), 80, replace = TRUE),
                      times = sample(1:40, 80, replace = TRUE))
  ))
  n_checked <- 0L
  n_bad <- 0L
  for (nm in names(cases)) {
    for (md in c("day", "spt")) {
      for (xm in c(1, 12)) {
        LEV <- cases[[nm]]
        a <- GGIR:::g.fragmentation("all", LEV, LNAMES18, xm, md)
        b <- frag("all", LEV, LNAMES18, xm, md)
        n_checked <- n_checked + 1L
        if (!identical(a, b)) {
          n_bad <- n_bad + 1L
          cat("\nmismatch:", nm, md, "xmin", xm, "\n")
          print(a)
          print(b)
        }
      }
    }
  }
  expect_identical(n_checked, 72L)
  expect_identical(n_bad, 0L)
})

test_that("the gates and defaults behave as GGIR's do on hand-built series", {
  # a single run closes both gates: 0 for the means and NFragPM, NA for the rest
  one <- frag("all", rep(5L, 50), LNAMES18, 1, "day")
  # TransProb hard-codes these as doubles, not integers
  expect_identical(one$Nfrag_IN, 1)
  expect_identical(one$Nfrag_PA, 0)
  expect_identical(one$mean_dur_IN, 0)
  expect_identical(one$mean_dur_PA, 0)
  expect_identical(one$NFragPM_IN, 0)
  expect_true(is.na(one$Gini_dur_IN))
  expect_true(is.na(one$alpha_dur_IN))
  expect_true(is.na(one$SD_dur_IN))

  # nine fragments pass the > 1 gate and not the >= 10 gate, which counts IN plus PA
  nine <- frag("all", rep(c(5L, 6L), length.out = 9), LNAMES18, 1, "day")
  expect_false(is.na(nine$mean_dur_IN))
  expect_false(is.na(nine$NFragPM_IN))
  expect_true(is.na(nine$SD_dur_IN))
  expect_true(is.na(nine$Gini_dur_IN))
  ten <- frag("all", rep(c(5L, 6L), length.out = 10), LNAMES18, 1, "day")
  expect_false(is.na(ten$SD_dur_IN))
  expect_false(is.na(ten$Gini_dur_IN))

  # a zero standard deviation closes only the power block for that class
  flat <- frag("all", rep(c(5L, 5L, 6L), 40), LNAMES18, 1, "day")
  expect_identical(flat$SD_dur_IN, 0)
  expect_identical(flat$SD_dur_PA, 0)
  expect_true(is.na(flat$alpha_dur_IN))
  expect_true(is.na(flat$alpha_dur_PA))
  expect_false(is.na(flat$Gini_dur_IN))

  # all one class, with more than 10 epochs but one run
  allmv <- frag("all", rep(7L, 50), LNAMES18, 1, "day")
  expect_identical(allmv$Nfrag_IN, 0)
  expect_identical(allmv$Nfrag_PA, 1)
  expect_identical(allmv$Nfrag_MVPA, 1)
  expect_identical(allmv$TP_IN2PA, 1)
})

test_that("Nfrag counts transitions, not fragments, and Gini keeps the terminal fragment (trap 56)", {
  # three IN runs, the series ends on IN, so Nfrag_IN is 2 while Gini sees three durations
  x <- c(rep(5L, 4), 6L, rep(5L, 4), 6L, rep(5L, 4))
  g <- frag("all", x, LNAMES18, 1, "day")
  expect_identical(g$Nfrag_IN, 2L)
  expect_identical(g$mean_dur_IN, 4)      # 8 epochs over 2 transitions
  expect_identical(g$NFragPM_IN, 0.25)    # 1 over mean_dur_IN
  r <- rle(as.integer(x %in% c(5L, 12L, 13L, 14L)))
  expect_identical(r$lengths[r$values == 1], c(4L, 4L, 4L))   # Gini would use all three
})


test_that("the power block reproduces GGIR's nonsense values at 5 s epochs (P5n)", {
  skip_if_no_ggir_ref()
  S <- load_rds(p5_file("state_WW.rds"))
  LEV <- waking_levels(S$mdat, S$LL$LEVELS, 2)     # window 2 is 2025-10-09
  f5 <- frag("all", LEV, S$LL$Lnames, 60 / 5, "day")
  expect_identical(f5$alpha_dur_IN, 0.25342055224232396)
  expect_identical(f5$x0.5_dur_IN, 1.4502829159447392e-05)
  expect_identical(f5$W0.5_dur_IN, 1)
  expect_identical(f5$Nfrag_IN, 352L)
  expect_identical(f5$mean_dur_IN, 28.144886363636363)
  expect_identical(round(f5$Gini_dur_IN, 6), 0.911632)
  expect_identical(round(f5$CoV_dur_IN, 6), 1.143013)
  expect_identical(round(f5$SD_dur_IN, 6), 182.973591)
  # 306 of the 353 inactivity fragments sit below xmin = 12 (86.7 percent)
  inids <- which(S$LL$Lnames %in% c("day_IN_unbt",
                                    grep("day_IN_bts", S$LL$Lnames, value = TRUE))) - 1
  r <- rle(as.integer(LEV %in% inids))
  D <- r$lengths[r$values == 1]
  expect_identical(length(D), 353L)
  expect_identical(sum(D < 12), 306L)
  expect_identical(round(sum(log(D / 12)), 4), -472.8231)
})

test_that("the power block is sane at 60 s epochs, and matches GGIR's stored list (P5n)", {
  skip_if_no_ggir_ref()
  H <- load_rds(p5_file("h2h_state.rds"))
  f60 <- frag("all", H$LEV, H$Lnames, 60 / 60, "day")
  expect_identical(f60$alpha_dur_IN, 1.5960129049902871)
  expect_identical(f60$x0.5_dur_IN, 3.1994324785445505)
  expect_identical(f60$W0.5_dur_IN, 0.95885286783042389)
  # MOS2 2025-10-09 at 60 s
  expect_identical(f60$Nfrag_IN, 32L)
  expect_identical(f60$mean_dur_IN, 24.875)              # 796 over 32, terminal 6 dropped
  expect_identical(round(f60$NFragPM_IN, 6), 0.040201)   # 32 over 796
  expect_identical(f60$TP_IN2PA, 0.03995)                # 32 over 801
  expect_identical(f60$TP_PA2IN, 0.202532)               # 32 over 158
  # the series does not end on PA, so NFragPM_PA and TP_PA2IN are the same ratio
  expect_identical(round(f60$NFragPM_PA, 6), f60$TP_PA2IN)
  expect_identical(f60$NFragPM_PA, 32 / 158)
  expect_identical(f60$Gini_dur_IN, 0.78631546134663344)
  expect_identical(f60$SD_dur_IN, 46.582242357257805)
  # TP_IN2LIPA plus TP_IN2MVPA equals TP_IN2PA, because the denominator is all IN
  expect_identical(f60$TP_IN2LIPA + f60$TP_IN2MVPA, f60$TP_IN2PA)
  expect_identical(f60, H$frag)
})

test_that("the port warns when the power metrics are asked for at an xmin other than 1", {
  x <- rep(c(5L, 6L), length.out = 60)
  expect_warning(.raw.fragmentation("all", x, LNAMES18, 12, "day"),
                 "not interpretable at this")
  expect_warning(.raw.fragmentation("all", x, LNAMES18, 12, "day"),
                 "part5_agg2_60seconds")
  expect_silent(.raw.fragmentation("all", x, LNAMES18, 1, "day"))
  expect_silent(.raw.fragmentation(c("mean", "TP"), x, LNAMES18, 12, "day"))
  expect_silent(.raw.fragmentation("all", x, LNAMES18, 12, "day", warn_xmin = FALSE))
})

test_that("ggir_exact = FALSE corrects both power defects and nothing else", {
  skip_if_no_ggir_ref()
  S <- load_rds(p5_file("state_WW.rds"))
  LEV <- waking_levels(S$mdat, S$LL$LEVELS, 2)
  a <- frag("all", LEV, S$LL$Lnames, 60 / 5, "day", ggir_exact = TRUE)
  b <- frag("all", LEV, S$LL$Lnames, 60 / 5, "day", ggir_exact = FALSE)
  # the six power columns move, and only those six
  moved <- names(a)[!vapply(names(a), function(n) identical(a[[n]], b[[n]]), logical(1))]
  expect_setequal(moved, c("alpha_dur_IN", "alpha_dur_PA", "x0.5_dur_IN", "x0.5_dur_PA",
                           "W0.5_dur_IN", "W0.5_dur_PA"))
  # the corrected alpha is .calculate.alpha.simple's, which filters the durations below xmin;
  # the corrected x0.5 is Chastin's parenthesisation
  inids <- which(S$LL$Lnames %in% c("day_IN_unbt",
                                    grep("day_IN_bts", S$LL$Lnames, value = TRUE))) - 1
  r <- rle(as.integer(LEV %in% inids))
  D <- r$lengths[r$values == 1]
  expect_identical(round(b$alpha_dur_IN, 3), .calculate.alpha.simple(D, xmin = 12))
  expect_identical(b$alpha_dur_IN, 1.8654928704465137)
  expect_identical(b$x0.5_dur_IN, 12 * 2^(1 / (b$alpha_dur_IN - 1)))
  expect_identical(a$x0.5_dur_IN, 2^(1 / (a$alpha_dur_IN - 1) * 12))
  # at xmin = 1 the two settings coincide
  H <- load_rds(p5_file("h2h_state.rds"))
  expect_identical(frag("all", H$LEV, H$Lnames, 1, "day", ggir_exact = TRUE),
                   frag("all", H$LEV, H$Lnames, 1, "day", ggir_exact = FALSE))
})

test_that("a partial frag.metrics gives GGIR's ragged column counts (P5o)", {
  # 4 and 65 fragments, the two sides of the >= 10 gate
  set.seed(11)
  mk <- function(nfrag) {
    v <- integer(0)
    for (i in seq_len(nfrag)) v <- c(v, rep(if (i %% 2 == 1) 5L else 6L, sample(3:25, 1)))
    v
  }
  d4 <- mk(4)
  d65 <- mk(65)
  expect_identical(length(rle(as.integer(d4 == 5L))$values), 4L)
  expect_identical(length(rle(as.integer(d65 == 5L))$values), 65L)

  expect_identical(length(frag("all", d4, LNAMES18, 1, "day")), 30L)
  expect_identical(length(frag("all", d65, LNAMES18, 1, "day")), 30L)
  expect_identical(length(frag(c("mean", "TP"), d4, LNAMES18, 1, "day")), 16L)
  expect_identical(length(frag(c("mean", "TP"), d65, LNAMES18, 1, "day")), 18L)
  expect_identical(length(frag("mean", d4, LNAMES18, 1, "day")), 4L)
  expect_identical(length(frag("mean", d65, LNAMES18, 1, "day")), 6L)
  expect_identical(length(frag("NFragPM", d4, LNAMES18, 1, "day")), 4L)
  expect_identical(length(frag("NFragPM", d65, LNAMES18, 1, "day")), 6L)

  # the SD pair order flips between the pre-allocated and the assigned path
  expect_identical(tail(names(frag("all", d65, LNAMES18, 1, "day")), 4),
                   c("SD_dur_IN", "SD_dur_PA", "NFragPM_PA", "NFragPM_IN"))
  expect_identical(tail(names(frag("mean", d65, LNAMES18, 1, "day")), 2),
                   c("SD_dur_PA", "SD_dur_IN"))
})

test_that("every subset of frag.metrics is identical() to GGIR's", {
  skip_if_no_ggir()
  set.seed(11)
  mk <- function(nfrag) {
    v <- integer(0)
    for (i in seq_len(nfrag)) v <- c(v, rep(if (i %% 2 == 1) 5L else 6L, sample(3:25, 1)))
    v
  }
  series <- list(small = mk(4), large = mk(65),
                 mixed = rep(c(5L, 6L, 7L, 13L, 16L), length.out = 400))
  sets <- list("all", c("mean", "TP"), "mean", "NFragPM", "TP", "Gini", "CoV", "power",
               c("power", "CoV"), c("Gini", "NFragPM"), character(0),
               c("mean", "TP", "Gini", "power", "CoV", "NFragPM"))
  n_checked <- 0L
  n_bad <- 0L
  for (s in sets) {
    for (nm in names(series)) {
      a <- GGIR:::g.fragmentation(s, series[[nm]], LNAMES18, 12, "day")
      b <- frag(s, series[[nm]], LNAMES18, 12, "day")
      n_checked <- n_checked + 1L
      if (!identical(a, b)) {
        n_bad <- n_bad + 1L
        cat("\nmismatch for", paste(s, collapse = "+"), "on", nm, "\n")
        print(a); print(b)
      }
    }
  }
  expect_identical(n_checked, 36L)
  expect_identical(n_bad, 0L)
})


test_that(".raw.intensity.gradient is identical() to g.intensitygradient on random inputs", {
  skip_if_no_ggir()
  set.seed(3)
  n_bad <- 0L
  for (i in 1:500) {
    n <- sample(3:200, 1)
    xx <- sort(stats::runif(n, 1, 4000))
    yy <- round(stats::rexp(n, 1 / 20), 3) * stats::rbinom(n, 1, 0.7)
    # a two-point fit makes summary.lm warn on both sides
    a <- suppressWarnings(GGIR:::g.intensitygradient(xx, yy))
    b <- suppressWarnings(.raw.intensity.gradient(xx, yy))
    if (!identical(a, b)) {
      n_bad <- n_bad + 1L
      if (n_bad == 1L) { cat("\nfirst IG mismatch at", i, "\n"); print(a); print(b) }
    }
  }
  expect_identical(n_bad, 0L)
  # the degenerate branches: empty bins, a single non-empty bin, no variance in x or in y
  expect_identical(.raw.intensity.gradient(c(1, 2, 3), c(0, 0, 0)),
                   GGIR:::g.intensitygradient(c(1, 2, 3), c(0, 0, 0)))
  expect_identical(.raw.intensity.gradient(c(1, 2, 3), c(0, 5, 0)),
                   GGIR:::g.intensitygradient(c(1, 2, 3), c(0, 5, 0)))
  expect_identical(.raw.intensity.gradient(c(2, 2, 2), c(1, 2, 3)),
                   GGIR:::g.intensitygradient(c(2, 2, 2), c(1, 2, 3)))
  # a negative bin is dropped by the same y <= 0 test as an empty one
  expect_identical(suppressWarnings(.raw.intensity.gradient(c(1, 2, 3, 4), c(-1, 2, 3, 4))),
                   suppressWarnings(GGIR:::g.intensitygradient(c(1, 2, 3, 4), c(-1, 2, 3, 4))))
  expect_identical(.raw.intensity.gradient(c(1, 2, 3, 4), c(-1, 2, 3, 40))$rsquared,
                   GGIR:::g.intensitygradient(c(1, 2, 3, 4), c(-1, 2, 3, 40))$rsquared)
  expect_identical(.raw.intensity.gradient(c(1, 2, 3), c(0, 0, 0)),
                   list(gradient = NA, y_intercept = NA, rsquared = NA))
})

test_that(".raw.intensity.gradient reproduces the three stored MOS2 WW triples (P5l)", {
  skip_if_no_ggir_ref()
  s <- mos2_series()
  mdat <- s$mdat
  o <- load_rdata(p5_file("work", "ig", "meta", "ms5.out", "MOS2E39230594.gt3x.RData"))
  out <- o$output
  iglevels <- c(seq(0, 4000, by = 25), 8000)        # the check_params expansion of iglevels = 1
  expect_identical(length(iglevels), 162L)
  x_ig <- zoo::rollmean(iglevels, k = 2)
  expect_identical(length(x_ig), 161L)
  ws3new <- 5
  sel <- which(out$TRLi == "40" & out$TRMi == "100" & out$TRVi == "400" & out$window == "WW")
  expect_identical(length(sel), 3L)
  n_checked <- 0L
  n_ok <- 0L
  for (row in sel) {
    wi <- as.numeric(out$window_number[row])
    sse <- which(mdat$window == wi)
    for (half in c("day", "day_spt")) {
      idx <- if (half == "day") sse[mdat$SleepPeriodTime[sse] == 0] else sse
      y_ig <- (as.numeric(table(cut(mdat$ACC[idx], breaks = iglevels, right = FALSE))) *
                 ws3new) / 60
      ig <- .raw.intensity.gradient(x_ig, y_ig)
      for (k in c("gradient", "intercept", "rsquared")) {
        mine <- as.character(if (k == "intercept") ig$y_intercept else ig[[k]])
        stored <- out[[paste0("ig_", half, "_", k)]][row]
        n_checked <- n_checked + 1L
        if (identical(mine, stored)) n_ok <- n_ok + 1L else {
          cat("\nIG mismatch WW", wi, half, k, "port", mine, "stored", stored, "\n")
        }
      }
    }
  }
  expect_identical(n_checked, 18L)
  expect_identical(n_ok, 18L)
  expect_identical(out$ig_day_gradient[sel],
                   c("-2.06856538422637", "-2.20212177453805", "-1.96465568611227"))
  expect_identical(out$ig_day_intercept[sel],
                   c("11.9295377243597", "12.3838234505366", "11.759443017453"))
  expect_identical(out$ig_day_rsquared[sel],
                   c("0.91322148068728", "0.928570189196511", "0.914163760725516"))
})


test_that("GGIR's fragmentation and sedentary.fragmentation disagree, in the documented ways", {
  # the same recording, day and epochs, with GGIR's own LEVELS defining sedentary, so only
  # the methods differ; min_break_length = 0 so canhrActi does no gap bridging
  skip_if_no_ggir_ref()
  H <- load_rds(p5_file("h2h_state.rds"))
  g <- frag("all", H$LEV, H$Lnames, 1, "day")

  ligids <- which(H$Lnames %in% c("day_LIG_unbt",
                                  grep("day_LIG_bts", H$Lnames, value = TRUE))) - 1
  intensity <- ifelse(H$LEV %in% H$inids, "sedentary",
                      ifelse(H$LEV %in% ligids, "light", "moderate"))
  tstamp <- as.POSIXct(H$mdat$timenum[H$day], origin = "1970-01-01", tz = "UTC")
  set.seed(42)
  ca <- sedentary.fragmentation(intensity = intensity, timestamps = tstamp,
                                wear_time = H$mdat$invalidepoch[H$day] == 0,
                                epoch_length = 60, min_break_length = 0,
                                robust_alpha = TRUE, compare_distributions = FALSE)
  expect_identical(length(H$LEV), 960L)
  expect_identical(length(H$DurIN), 33L)
  expect_identical(sum(H$DurIN), 802L)
  expect_identical(length(H$DurPA), 32L)
  expect_identical(sum(H$DurPA), 158L)

  # the same number from both engines
  expect_identical(g$Nfrag_PA, 32L)
  expect_identical(ca$n_active_bouts, 32L)
  expect_identical(round(g$mean_dur_PA, 2), 4.94)
  expect_identical(ca$mean_active_bout, 4.94)
  expect_identical(round(g$TP_PA2IN, 5), ca$ASTP)                 # 0.20253 both ways
  expect_identical(round(g$Gini_dur_IN, 4), ca$gini)              # 0.7863 both ways
  expect_identical(round(g$SD_dur_IN, 2), ca$sd_bout_duration)    # 46.58 both ways
  expect_identical(.raw.gini(H$DurIN, corr = TRUE), .calculate.gini(H$DurIN))

  # a different number: GGIR counts transitions, canhrActi counts fragments
  expect_identical(g$Nfrag_IN, 32L)
  expect_identical(ca$total_bouts, 33L)
  expect_identical(g$mean_dur_IN, 24.875)          # 796 over 32
  expect_identical(ca$mean_bout_duration, 24.3)    # 802 over 33
  expect_identical(g$TP_IN2PA, 0.03995)            # 32 over 801
  expect_identical(ca$SATP, 0.04115)               # 33 over 802
  expect_identical(round(1 / ca$mean_bout_duration, 6), 0.041152)
  expect_identical(round(g$NFragPM_IN, 6), 0.040201)
  # the estimator: with the Clauset xmin search off, the two alphas are the same number
  set.seed(42)
  ca_hill <- sedentary.fragmentation(intensity = intensity, timestamps = tstamp,
                                     wear_time = H$mdat$invalidepoch[H$day] == 0,
                                     epoch_length = 60, min_break_length = 0,
                                     robust_alpha = FALSE, compare_distributions = FALSE)
  expect_identical(ca_hill$alpha_xmin, 1)
  expect_identical(ca_hill$alpha, round(g$alpha_dur_IN, 3))   # 1.596 both ways
  # with it on, the tail is selected
  expect_identical(ca$alpha_xmin, 2)
  expect_identical(ca$alpha, 1.58)
  # usual bout duration: a model quantity against an empirical one
  expect_identical(round(g$x0.5_dur_IN, 4), 3.1994)
  expect_identical(ca$W50, 131)

  # present in one engine only
  expect_false(is.na(g$CoV_dur_IN))                 # GGIR only
  expect_identical(round(g$CoV_dur_IN, 6), 0.978192)
  expect_identical(g$Nfrag_LIPA, 38L)               # GGIR only, destination split
  expect_identical(round(g$TP_IN2LIPA, 6), 0.034956)
  for (nm in c("alpha_xmin", "alpha_ci_lower", "weibull_shape", "hazard_rate",
               "breaks_per_sed_hour", "W25", "W75", "W90")) {
    expect_true(nm %in% names(ca))                  # canhrActi only
    expect_false(nm %in% names(g))
  }
})


test_that("g.fragmentation and g.intensitygradient are unchanged between 3.3.6 and 3.3-9", {
  skip_if_no_ggir(); skip_if_no_ggir_ref()
  for (f in c("g.fragmentation", "g.intensitygradient")) {
    src <- clone_file(paste0(f, ".R"))
    skip_if_no_file(src)
    e <- new.env(parent = asNamespace("GGIR"))
    sys.source(src, envir = e)
    inst <- deparse(get(f, envir = asNamespace("GGIR")))
    clone <- deparse(get(f, envir = e))
    if (!identical(inst, clone)) {
      d <- which(inst[seq_len(min(length(inst), length(clone)))] !=
                   clone[seq_len(min(length(inst), length(clone)))])
      cat("\n", f, "differs at deparsed lines", utils::head(d, 10), "\n")
      for (i in utils::head(d, 6)) {
        cat("  installed:", inst[i], "\n  clone    :", clone[i], "\n")
      }
    }
    expect_identical(clone, inst)
  }
  expect_identical(length(deparse(GGIR:::g.fragmentation)), 243L)
  expect_identical(length(deparse(GGIR:::g.intensitygradient)), 21L)
})
