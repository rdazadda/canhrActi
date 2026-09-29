# Parity tests for R/raw_timeuse_levels.R (.raw.getbout, .raw.identify.levels and the
# class dictionary) against GGIR part 5's g.getbout, identify_levels and legend table.
# The classifier needs only ACC, diur and sibdetection, so the reference ts is rebuilt
# from the stored part-5 mdat. Reference data live in the folder named by
# CANHRACTI_GGIR_REF; tests skip when it is unset or GGIR is not installed.

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

P5_CASES <- list(
  MOS2 = list(out = c("out", "output_din"), nepochs = 118800L),
  EE   = list(out = c("timing_out", "output_timing"), nepochs = 120960L))

# The stored ms5.outraw series, with class_id and Lnames.
p5_series <- local({
  cache <- list()
  function(case) {
    if (!is.null(cache[[case]])) return(cache[[case]])
    folder <- do.call(ref_file, c(as.list(P5_CASES[[case]]$out),
                                  list("meta", "ms5.outraw", "40_100_400")))
    if (!dir.exists(folder)) testthat::skip(paste0("reference folder not found: ", folder))
    files <- list.files(folder, pattern = "\\.RData$", full.names = TRUE)
    if (length(files) != 1) testthat::skip(paste0("expected one RData in ", folder))
    e <- new.env()
    load(files[1], envir = e)
    ts <- data.frame(time = e$mdat$timestamp, ACC = e$mdat$ACC,
                     diur = e$mdat$SleepPeriodTime, sibdetection = e$mdat$sibdetection)
    out <- list(ts = ts, class_id = e$mdat$class_id, Lnames = e$Lnames,
                ws3 = as.numeric(e$mdat$timenum[2] - e$mdat$timenum[1]))
    cache[[case]] <<- out
    out
  }
})

p5_codes <- function(case) {
  folder <- do.call(ref_file, c(as.list(P5_CASES[[case]]$out),
                                list("meta", "ms5.outraw")))
  files <- list.files(folder, pattern = "^behavioralcodes.*\\.csv$", full.names = TRUE)
  if (length(files) != 1) testthat::skip(paste0("no behavioralcodes csv in ", folder))
  utils::read.csv(files[1], stringsAsFactors = FALSE)
}

# GGIR's phyact defaults, sorted descending as g.part5 does before the classifier sees them.
PHYACT <- list(boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
               boutdur.lig = c(10, 5, 1), boutcriter.mvpa = 0.8, boutcriter.in = 0.9,
               boutcriter.lig = 0.8)

# The 18 classes of the default configuration, in class order (id = position - 1).
DEFAULT_CLASSES <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD",
                     "spt_wake_VIG", "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt",
                     "day_VIG_unbt", "day_MVPA_bts_10", "day_MVPA_bts_5_10",
                     "day_MVPA_bts_1_5", "day_IN_bts_30", "day_IN_bts_20_30",
                     "day_IN_bts_10_20", "day_LIG_bts_10", "day_LIG_bts_5_10",
                     "day_LIG_bts_1_5")

bits <- function(s) as.numeric(strsplit(s, "")[[1]])

# The nine bout passes in execution order, with the running refe exclusion.
mos2_bout_passes <- function(ts, ws3, ggir_exact = TRUE) {
  specs <- list(list("MVPA", c(10, 5, 1), 0.8), list("IN", c(30, 20, 10), 0.9),
                list("LIG", c(10, 5, 1), 0.8))
  refe <- rep(0, nrow(ts))
  out <- list()
  for (sp in specs) {
    for (d in sp[[2]]) {
      if (sp[[1]] == "MVPA") {
        seed <- as.numeric(ts$ACC >= 100 & refe == 0 & ts$diur == 0)
      } else if (sp[[1]] == "IN") {
        seed <- as.numeric(ts$ACC < 40 & refe == 0 & ts$diur == 0)
      } else {
        seed <- as.numeric(ts$ACC >= 40 & refe == 0 & ts$ACC < 100 & ts$diur == 0)
      }
      bout <- .raw.getbout(seed, d * (60 / ws3), sp[[3]], ws3, ggir_exact = ggir_exact)
      out[[length(out) + 1]] <- list(tag = sp[[1]], dur = d, crit = sp[[3]],
                                     seed = seed, bout = bout)
      refe <- refe + bout
    }
  }
  out
}

# Runs of 1: count, lengths and starts.
bout_runs <- function(x) {
  r <- rle(x)
  lens <- r$lengths[r$values == 1]
  starts <- cumsum(c(1, r$lengths))[-(length(r$lengths) + 1)][r$values == 1]
  list(n = length(lens), lens = lens, starts = starts)
}

test_that("P3a .raw.getbout reproduces the six worked examples at a one minute epoch", {
  expect_identical(.raw.getbout(bits("01111100"), 5, 1.0, ws3 = 60), bits("01111100"))
  expect_identical(.raw.getbout(bits("01111000"), 5, 1.0, ws3 = 60), bits("00000000"))
  expect_identical(.raw.getbout(bits("01101110"), 5, 0.8, ws3 = 60), bits("01111110"))
  expect_identical(.raw.getbout(bits("11111011111"), 5, 0.8, ws3 = 60), bits("11111111111"))
  expect_identical(.raw.getbout(bits("111110011111"), 5, 0.8, ws3 = 60), bits("111110011111"))
  expect_identical(.raw.getbout(bits("11111011111"), 5, 1.0, ws3 = 60), bits("11111011111"))
})

test_that("P3a the worked examples are identical() to GGIR:::g.getbout", {
  skip_if_no_ggir()
  GB <- GGIR:::g.getbout
  cases <- list(list("01111100", 5, 1.0), list("01111000", 5, 1.0), list("01101110", 5, 0.8),
                list("11111011111", 5, 0.8), list("111110011111", 5, 0.8),
                list("11111011111", 5, 1.0))
  for (cs in cases) {
    x <- bits(cs[[1]])
    expect_identical(.raw.getbout(x, cs[[2]], cs[[3]], ws3 = 60),
                     GB(x, cs[[2]], cs[[3]], ws3 = 60),
                     info = paste(cs[[1]], cs[[2]], cs[[3]]))
  }
})

test_that("the defaults and the signature match g.getbout", {
  fm <- formals(.raw.getbout)
  expect_identical(fm$boutcriter, 0.8)
  expect_identical(fm$ws3, 5)
  expect_true(isTRUE(fm$ggir_exact))
})

test_that("P3b .raw.getbout is identical() to GGIR:::g.getbout on 504 random series", {
  skip_if_no_ggir()
  GB <- GGIR:::g.getbout
  set.seed(1)
  ncomp <- 0
  for (ws3 in c(5, 30, 60)) {
    for (bd in c(1, 5, 12, 60, 120, 360)) {
      for (rep in 1:28) {
        n <- sample(c(50, 200, 1000, 5000), 1)
        x <- as.numeric(runif(n) < runif(1, 0.05, 0.95))
        crit <- sample(c(0.5, 0.8, 0.9, 1.0), 1)
        expect_identical(.raw.getbout(x, bd, crit, ws3), GB(x, bd, crit, ws3),
                         info = paste("ws3", ws3, "bd", bd, "crit", crit, "n", n))
        ncomp <- ncomp + 1
      }
    }
  }
  expect_identical(ncomp, 504)
})

test_that("P3f the edge cases behave and agree with GGIR", {
  cases <- list(
    list(x = rep(0, 20), bd = 5, crit = 0.8, ws3 = 60, want = rep(0, 20)),
    list(x = rep(1, 20), bd = 5, crit = 0.8, ws3 = 60, want = rep(1, 20)),
    list(x = rep(1, 3), bd = 5, crit = 0.8, ws3 = 60, want = rep(0, 3)),
    list(x = c(1, 1, NA, 1, 1, 1, 0, 0), bd = 5, crit = 0.8, ws3 = 60,
         want = bits("11111100")),
    list(x = c(1, 0, 1, 1, 0, 1), bd = 1, crit = 0.8, ws3 = 60, want = c(1, 0, 1, 1, 0, 1)),
    list(x = 1, bd = 1, crit = 0.8, ws3 = 60, want = 1),
    list(x = c(1, 1, 1, 0, 1, 1), bd = 2.5, crit = 0.8, ws3 = 60, want = c(1, 1, 1, 0, 1, 1)),
    list(x = c(1, 0, 1, 0), bd = 5, crit = 0, ws3 = 60, want = c(0, 0, 0, 0)),
    list(x = rep(0, 10), bd = 5, crit = -1, ws3 = 60, want = rep(0, 10)))
  for (cs in cases) {
    got <- .raw.getbout(cs$x, cs$bd, cs$crit, cs$ws3)
    expect_identical(got, cs$want, info = paste(paste(cs$x, collapse = ""), cs$bd, cs$crit))
  }
  skip_if_no_ggir()
  GB <- GGIR:::g.getbout
  for (cs in cases) {
    expect_identical(.raw.getbout(cs$x, cs$bd, cs$crit, cs$ws3),
                     GB(cs$x, cs$bd, cs$crit, cs$ws3),
                     info = paste(paste(cs$x, collapse = ""), cs$bd, cs$crit))
  }
})

test_that("P3f split(integer(0)) is the one-element list the no-bout guard tests for", {
  p <- integer(0)
  db <- split(p, cumsum(c(1, diff(p) != 1)))
  expect_identical(length(db), 1L)
  expect_identical(length(db[[1]]), 0L)
  expect_true(length(db) == 1 & length(db[[1]]) == 0)
})

test_that("P3c all nine MOS2 bout passes are identical() to GGIR:::g.getbout", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  GB <- GGIR:::g.getbout
  S <- p5_series("MOS2")
  expect_identical(nrow(S$ts), P5_CASES$MOS2$nepochs)
  passes <- mos2_bout_passes(S$ts, S$ws3)
  expect_identical(length(passes), 9L)
  for (ps in passes) {
    expect_identical(ps$bout, GB(ps$seed, ps$dur * (60 / S$ws3), ps$crit, S$ws3),
                     info = paste(ps$tag, ps$dur))
  }
})

test_that("P3d the short-bout artifact of the in-place marking is reproduced exactly", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  passes <- mos2_bout_passes(S$ts, S$ws3)
  # tag, boutdur, Nbouts, shortest run in epochs
  want <- list(c("MVPA", 10, 0, NA), c("MVPA", 5, 0, NA), c("MVPA", 1, 48, 2),
               c("IN", 30, 50, 55), c("IN", 20, 19, 1), c("IN", 10, 22, 123),
               c("LIG", 10, 6, 1), c("LIG", 5, 6, 58), c("LIG", 1, 263, 1))
  for (i in seq_along(passes)) {
    ps <- passes[[i]]
    w <- want[[i]]
    expect_identical(ps$tag, w[1])
    expect_identical(ps$dur, as.numeric(w[2]))
    rr <- bout_runs(ps$bout)
    expect_identical(rr$n, as.integer(w[3]), info = paste("Nbouts", ps$tag, ps$dur))
    if (!is.na(w[4])) {
      expect_identical(min(rr$lens), as.integer(w[4]),
                       info = paste("shortest bout", ps$tag, ps$dur))
    }
  }
  # the realised criterion fraction can sit below boutcriter, and no gap exceeds one minute
  fracmin <- function(ps) {
    rr <- bout_runs(ps$bout)
    if (rr$n == 0) return(NA_real_)
    min(mapply(function(s, l) mean(ps$seed[s:(s + l - 1)] == 1), rr$starts, rr$lens))
  }
  maxgap <- function(ps) {
    rr <- bout_runs(ps$bout)
    if (rr$n == 0) return(NA_integer_)
    max(mapply(function(s, l) {
      g <- rle(ps$seed[s:(s + l - 1)] == 0)
      if (any(g$values)) max(g$lengths[g$values]) else 0L
    }, rr$starts, rr$lens))
  }
  expect_equal(round(fracmin(passes[[3]]), 4), 0.8125)   # MVPA bd 1, crit 0.8
  expect_equal(round(fracmin(passes[[4]]), 4), 0.8641)   # IN   bd 30, crit 0.9
  expect_equal(round(fracmin(passes[[5]]), 4), 0.8875)
  expect_equal(round(fracmin(passes[[6]]), 4), 0.8571)
  expect_equal(round(fracmin(passes[[7]]), 4), 0.8000)
  expect_equal(round(fracmin(passes[[8]]), 4), 0.7812)
  expect_equal(round(fracmin(passes[[9]]), 4), 0.7778)   # LIG bd 1, crit 0.8, below it
  for (i in 3:9) {
    expect_lte(maxgap(passes[[i]]), 12)                  # 60 / ws3 epochs, hard wired
  }
})

test_that("P3d the traced MVPA pass at MOS2 epochs 51910 to 51935 is reproduced", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  passes <- mos2_bout_passes(S$ts, S$ws3)
  o <- passes[[3]]$bout                                  # MVPA, boutdur 1 minute
  # the second group is pushed past the region the first claimed, so two runs, not one
  expect_true(all(o[51913:51928] == 1))
  expect_identical(o[51929], 0)
  expect_true(all(o[51930:51931] == 1))
  expect_identical(o[51932], 0)
  expect_identical(o[51910], 0)                          # the seed is 1 here and the bout is not
  expect_identical(passes[[3]]$seed[51910], 1)
})

test_that("P3e the end-of-bout gap test is vacuous and never moved an end on MOS2", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  # A replica of .raw.getbout that records, per bout group, the end index chosen with
  # GGIR's look4start test and with the look4end the code intends.
  instrumented <- function(x, boutduration, boutcriter, ws3) {
    x[is.na(x)] <- 0
    zeroes <- rep(0, ceiling(60 / ws3 / 2))
    xtmp <- c(zeroes, x, zeroes)
    lfb <- zoo::rollmean(x = xtmp, k = (60 / ws3) + 1, align = "center", fill = rep(0, 3))
    keep <- (length(zeroes) + 1):(length(lfb) - length(zeroes))
    xtmp[lfb == 0] <- -boutduration
    xt <- xtmp[keep]
    ap <- rep(-boutduration, ceiling(boutduration / 2))
    RM <- zoo::rollmean(x = c(ap, xt, ap), k = boutduration, align = "center",
                        fill = rep(0, 3))
    RM <- RM[(length(ap) + 1):(length(RM) - length(ap))]
    p <- which(RM >= boutcriter)
    half1 <- floor(boutduration / 2)
    db <- split(p, cumsum(c(1, diff(p) != 1)))
    ends_vacuous <- c()
    ends_intended <- c()
    if (!(length(db) == 1 & length(db[[1]]) == 0)) {
      for (k in 1:length(db)) {
        bout <- db[[k]]
        found <- FALSE
        adjust <- 0
        while (!found) {
          st <- min(bout) - half1 + adjust
          if (st < 1 || xt[st] != 1) { adjust <- adjust + 1; next }
          sp <- st:max(bout)
          look4start <- split(xt[sp], cumsum(c(1, diff(xt[sp]) != 0)))
          if (all(sapply(look4start, function(v) sum(v == 0)) <= 60 / ws3)) found <- TRUE
          adjust <- adjust + 1
        }
        find_end <- function(use_look4end) {
          done <- FALSE
          adjust <- 0
          while (!done) {
            en <- max(bout) + half1 - adjust
            if (en > length(xt) || xt[en] != 1) { adjust <- adjust + 1; next }
            ep <- min(bout):en
            look4end <- split(xt[ep], cumsum(c(1, diff(xt[ep]) != 0)))
            gp <- if (use_look4end) look4end else look4start
            if (all(sapply(gp, function(v) sum(v == 0)) <= 60 / ws3)) done <- TRUE
            adjust <- adjust + 1
          }
          en
        }
        e_vacuous <- find_end(FALSE)
        ends_vacuous <- c(ends_vacuous, e_vacuous)
        ends_intended <- c(ends_intended, find_end(TRUE))
        xt[st:e_vacuous] <- 2
      }
    }
    y <- x
    y[which(xt != 2)] <- 0
    y[which(xt == 2)] <- 1
    list(y = y, ends_vacuous = ends_vacuous, ends_intended = ends_intended)
  }
  passes <- mos2_bout_passes(S$ts, S$ws3)
  ngroups <- 0
  for (ps in passes) {
    ins <- instrumented(ps$seed, ps$dur * (60 / S$ws3), ps$crit, S$ws3)
    expect_identical(ins$y, ps$bout, info = paste("replica", ps$tag, ps$dur))
    expect_identical(ins$ends_vacuous, ins$ends_intended,
                     info = paste("end index", ps$tag, ps$dur))
    ngroups <- ngroups + length(ins$ends_vacuous)
  }
  expect_identical(ngroups, 414)      # bout groups examined across the nine passes
})

test_that("P3e the ported closing loop still reads the START object, as GGIR does", {
  src <- gsub("\\s+", " ", paste(deparse(.raw.getbout), collapse = " "))
  expect_true(grepl("look4gap <- if (ggir_exact) look4start else look4end", src, fixed = TRUE))
  expect_true(grepl("sum(look4gap[[i]] == 0)", src, fixed = TRUE))
})

test_that("ggir_exact = FALSE changes the MOS2 bouts, so the defects are real and switchable", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  exact <- mos2_bout_passes(S$ts, S$ws3, ggir_exact = TRUE)
  # the relaxed run uses its own refe chain
  relaxed <- mos2_bout_passes(S$ts, S$ws3, ggir_exact = FALSE)
  ne <- vapply(exact, function(ps) bout_runs(ps$bout)$n, integer(1))
  nr <- vapply(relaxed, function(ps) bout_runs(ps$bout)$n, integer(1))
  expect_identical(ne, c(0L, 0L, 48L, 50L, 19L, 22L, 6L, 6L, 263L))
  expect_identical(nr, c(0L, 0L, 42L, 36L, 16L, 22L, 3L, 6L, 222L))
  # the sub-boutduration runs disappear without the in-place marking
  expect_identical(min(bout_runs(exact[[3]]$bout)$lens), 2L)
  expect_identical(min(bout_runs(relaxed[[3]]$bout)$lens), 11L)
  expect_identical(min(bout_runs(exact[[4]]$bout)$lens), 55L)
  expect_identical(min(bout_runs(relaxed[[4]]$bout)$lens), 533L)
  expect_identical(min(bout_runs(exact[[9]]$bout)$lens), 1L)
  expect_identical(min(bout_runs(relaxed[[9]]$bout)$lens), 10L)
})

test_that("P3g .raw.identify.levels reproduces the stored class_id on both recordings", {
  skip_if_no_ggir_ref()
  for (case in names(P5_CASES)) {
    S <- p5_series(case)
    expect_identical(nrow(S$ts), P5_CASES[[case]]$nepochs)
    LL <- .raw.identify.levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                               params_phyact = PHYACT)
    expect_identical(as.numeric(LL$LEVELS), as.numeric(S$class_id), info = case)
    expect_identical(sum(LL$LEVELS != S$class_id), 0L, info = case)
    expect_identical(LL$Lnames, S$Lnames, info = case)
    expect_identical(LL$Lnames, DEFAULT_CLASSES, info = case)
    expect_identical(LL$threshold, c(40, 100, 400))
    expect_identical(names(LL), c("LEVELS", "OLEVELS", "Lnames", "bc.mvpa", "bc.lig",
                                  "bc.in", "ts", "threshold"))
    expect_identical(LL$ts, S$ts)
  }
})

test_that("P3g .raw.identify.levels is identical() to GGIR:::identify_levels, four triples", {
  skip_if_no_ggir_ref()
  skip_if_no_ggir()
  IL <- GGIR:::identify_levels
  S <- p5_series("MOS2")
  for (tr in list(c(40, 100, 400), c(18, 60, 400), c(30, 100, 400), c(50, 150, 500))) {
    mine <- .raw.identify.levels(ts = S$ts, TRLi = tr[1], TRMi = tr[2], TRVi = tr[3],
                                 ws3 = S$ws3, params_phyact = PHYACT)
    theirs <- IL(ts = S$ts, TRLi = tr[1], TRMi = tr[2], TRVi = tr[3], ws3 = S$ws3,
                 params_phyact = PHYACT)
    expect_identical(mine, theirs, info = paste(tr, collapse = "/"))
  }
  SE <- p5_series("EE")
  expect_identical(.raw.identify.levels(ts = SE$ts, TRLi = 40, TRMi = 100, TRVi = 400,
                                        ws3 = SE$ws3, params_phyact = PHYACT),
                   IL(ts = SE$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = SE$ws3,
                      params_phyact = PHYACT))
})

test_that("P3h the MOS2 class distribution is exactly the stored one", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  LL <- .raw.identify.levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                             params_phyact = PHYACT)
  expect_identical(sort(unique(LL$LEVELS)),
                   c(0, 1, 2, 3, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16, 17))
  # 4 (vigorous wake inside the SPT), 9 and 10 (MVPA bouts of 5 minutes or more) never occur
  expect_false(any(LL$LEVELS %in% c(4, 9, 10)))
  tab <- table(LL$LEVELS)
  expect_identical(as.integer(tab),
                   c(16650L, 1216L, 36L, 1L, 10795L, 6082L, 2260L, 72L, 777L, 68353L,
                     4742L, 3303L, 363L, 372L, 3778L))
  expect_identical(sum(as.integer(tab)), 118800L)
  expect_identical(as.integer(table(LL$LEVELS, useNA = "ifany")), as.integer(tab))
  # OLEVELS is the bout-free partition and is 0 exactly on the sleep period
  expect_identical(sum(LL$OLEVELS == 0), sum(S$ts$diur == 1))
  expect_identical(sort(unique(LL$OLEVELS)), c(0, 1, 2, 3, 4))
})

test_that("P3h the two partitions of waking time each add up, and do not nest", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  LL <- .raw.identify.levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                             params_phyact = PHYACT)
  day <- which(S$ts$diur == 0)
  expect_identical(sum(LL$LEVELS[day] %in% 5:17), length(day))
  expect_identical(sum(LL$OLEVELS[day] %in% 1:4), length(day))
  # total IN (the OLEVELS partition) is not unbouted IN plus the three IN bout classes
  total_in <- sum(LL$OLEVELS[day] == 1)
  levels_in <- sum(LL$LEVELS[day] %in% c(5, 12, 13, 14))
  expect_false(total_in == levels_in)
  expect_identical(length(day), 100897L)
  expect_identical(total_in, 84259L)
  expect_identical(levels_in, 87193L)          # 2934 epochs, 244.5 minutes, of bridged gaps
  expect_identical(sum(LL$OLEVELS[day] == 2), 12994L)
  expect_identical(sum(LL$LEVELS[day] %in% c(6, 15, 16, 17)), 10595L)
  expect_identical(sum(LL$OLEVELS[day] %in% c(3, 4)), 3644L)
  expect_identical(sum(LL$LEVELS[day] %in% c(7, 8, 9, 10, 11)), 3109L)
})

test_that("P3i the ladder follows the boutdur vectors, in the order the caller supplies", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  cfg <- list(boutdur.mvpa = sort(c(2, 8), decreasing = TRUE),
              boutdur.in = sort(c(15), decreasing = TRUE),
              boutdur.lig = sort(c(3, 6, 12), decreasing = TRUE),
              boutcriter.mvpa = 0.8, boutcriter.in = 0.9, boutcriter.lig = 0.8)
  LL <- .raw.identify.levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                             params_phyact = cfg)
  expect_identical(length(LL$Lnames), 15L)          # 9 fixed + 2 + 1 + 3
  expect_identical(LL$Lnames[10:15],
                   c("day_MVPA_bts_8", "day_MVPA_bts_2_8", "day_IN_bts_15",
                     "day_LIG_bts_12", "day_LIG_bts_6_12", "day_LIG_bts_3_6"))
  expect_true(max(LL$LEVELS) <= 14)
  # the descending sort is the caller's job, as in GGIR
  asc <- cfg
  asc$boutdur.mvpa <- c(2, 8)
  asc$boutdur.lig <- c(3, 6, 12)
  LA <- .raw.identify.levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                             params_phyact = asc)
  expect_identical(LA$Lnames[10:15],
                   c("day_MVPA_bts_2", "day_MVPA_bts_8_2", "day_IN_bts_15",
                     "day_LIG_bts_3", "day_LIG_bts_6_3", "day_LIG_bts_12_6"))
  expect_false(identical(LA$LEVELS, LL$LEVELS))
  skip_if_no_ggir()
  IL <- GGIR:::identify_levels
  expect_identical(LL, IL(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                          params_phyact = cfg))
  expect_identical(LA, IL(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                          params_phyact = asc))
})

test_that("P3j a single-element boutdur vector keeps its row and is not dropped", {
  skip_if_no_ggir_ref()
  S <- p5_series("MOS2")
  cfg <- list(boutdur.mvpa = 10, boutdur.in = 15, boutdur.lig = 5,
              boutcriter.mvpa = 0.8, boutcriter.in = 0.9, boutcriter.lig = 0.8)
  LL <- .raw.identify.levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400, ws3 = S$ws3,
                             params_phyact = cfg)
  # rbind(c(), out1) gives a one-row matrix, not a vector
  for (bc in list(LL$bc.mvpa, LL$bc.in, LL$bc.lig)) {
    expect_true(is.matrix(bc))
    expect_identical(dim(bc), c(1L, 118800L))
  }
  expect_identical(length(LL$Lnames), 12L)
  expect_identical(LL$Lnames[10:12], c("day_MVPA_bts_10", "day_IN_bts_15", "day_LIG_bts_5"))
  nb <- vapply(list(LL$bc.mvpa, LL$bc.in, LL$bc.lig),
               function(bc) bout_runs(bc[1, ])$n, integer(1))
  expect_identical(length(nb), 3L)
  expect_identical(nb, c(0L, 85L, 9L))
  skip_if_no_ggir()
  expect_identical(LL, GGIR:::identify_levels(ts = S$ts, TRLi = 40, TRMi = 100, TRVi = 400,
                                              ws3 = S$ws3, params_phyact = cfg))
})

test_that("P3k vigorous epochs are bouted as MVPA and threshold.vig only splits the unbouted", {
  set.seed(7)
  n <- 3600
  ACC <- round(exp(rnorm(n, log(60), 1.3)), 2)
  ACC[501:536] <- 600                     # three minutes above any vigorous threshold
  tsx <- data.frame(time = seq_len(n), ACC = ACC,
                    diur = c(rep(0, 2400), rep(1, 1200)),
                    sibdetection = c(rep(0, 2400), rep(c(1, 0), each = 600)))
  a <- .raw.identify.levels(tsx, 40, 100, 400, 5, params_phyact = PHYACT)
  b <- .raw.identify.levels(tsx, 40, 100, 1000, 5, params_phyact = PHYACT)
  expect_false(any(grepl("VIG_bts", a$Lnames)))          # there is no vigorous bout class
  expect_identical(length(a$Lnames), 18L)
  vig_day <- which(ACC >= 400 & tsx$diur == 0)
  expect_identical(length(vig_day), 218L)
  expect_identical(sort(unique(a$LEVELS[vig_day])), c(8, 11))
  expect_identical(sum(a$LEVELS[vig_day] == 11), 39L)    # inside an MVPA bout band
  expect_identical(sum(a$LEVELS[vig_day] == 8), 179L)    # unbouted vigorous
  # raising threshold.vig moves epochs only between the unbouted classes 4 -> 3 and 8 -> 7
  changed <- which(a$LEVELS != b$LEVELS)
  expect_identical(length(changed), 168L)
  expect_identical(sort(unique(a$LEVELS[changed])), c(4, 8))
  expect_identical(sort(unique(b$LEVELS[changed])), c(3, 7))
  expect_identical(a$bc.mvpa, b$bc.mvpa)
  expect_identical(a$bc.in, b$bc.in)
  expect_identical(a$bc.lig, b$bc.lig)
  skip_if_no_ggir()
  IL <- GGIR:::identify_levels
  expect_identical(a, IL(tsx, 40, 100, 400, 5, params_phyact = PHYACT))
  expect_identical(b, IL(tsx, 40, 100, 1000, 5, params_phyact = PHYACT))
})

test_that("the six bout settings can be passed explicitly and win over params_phyact", {
  set.seed(11)
  n <- 1200
  tsx <- data.frame(time = seq_len(n), ACC = round(exp(rnorm(n, log(60), 1.2)), 2),
                    diur = rep(0, n), sibdetection = rep(0, n))
  viaparams <- .raw.identify.levels(tsx, 40, 100, 400, 5, params_phyact = PHYACT)
  explicit <- .raw.identify.levels(tsx, 40, 100, 400, 5,
                                   boutdur.mvpa = c(10, 5, 1), boutdur.in = c(30, 20, 10),
                                   boutdur.lig = c(10, 5, 1), boutcriter.mvpa = 0.8,
                                   boutcriter.in = 0.9, boutcriter.lig = 0.8)
  expect_identical(explicit, viaparams)
  override <- .raw.identify.levels(tsx, 40, 100, 400, 5, params_phyact = PHYACT,
                                   boutdur.lig = 5)
  expect_identical(override$Lnames[length(override$Lnames)], "day_LIG_bts_5")
  expect_error(.raw.identify.levels(tsx, 40, 100, 400, 5), "boutdur.mvpa")
  expect_error(.raw.identify.levels(tsx[, c("time", "ACC")], 40, 100, 400, 5,
                                    params_phyact = PHYACT), "diur")
})

test_that("the class dictionary is the full 18-class table in class order", {
  dict <- .raw.timeuse.class.dictionary(DEFAULT_CLASSES)
  expect_identical(names(dict), c("class_name", "class_id"))
  expect_identical(nrow(dict), 18L)
  expect_identical(dict$class_id, 0:17)
  expect_identical(dict$class_name, DEFAULT_CLASSES)
  expect_identical(dict$class_name[dict$class_id == 0], "spt_sleep")
  expect_identical(dict$class_name[dict$class_id == 8], "day_VIG_unbt")
  expect_identical(dict$class_name[dict$class_id == 17], "day_LIG_bts_1_5")
  expect_false(is.factor(dict$class_name))
  expect_error(.raw.timeuse.class.dictionary(character(0)), "character vector")
  expect_error(.raw.timeuse.class.dictionary(0:17), "character vector")
})

test_that("the class dictionary is identical() to GGIR's behavioralcodes csv, both recordings", {
  skip_if_no_ggir_ref()
  for (case in names(P5_CASES)) {
    S <- p5_series(case)
    expect_identical(.raw.timeuse.class.dictionary(S$Lnames), p5_codes(case), info = case)
  }
})

test_that("the class dictionary follows the configuration", {
  cfg <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
           "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt",
           "day_MVPA_bts_8", "day_MVPA_bts_2_8", "day_IN_bts_15", "day_LIG_bts_12",
           "day_LIG_bts_6_12", "day_LIG_bts_3_6")
  dict <- .raw.timeuse.class.dictionary(cfg)
  expect_identical(nrow(dict), 15L)
  expect_identical(dict$class_id, 0:14)
  # the nap class appends one more row
  napped <- .raw.timeuse.class.dictionary(c(cfg, "day_nap"))
  expect_identical(nrow(napped), 16L)
  expect_identical(napped$class_id[napped$class_name == "day_nap"], 15L)
})
