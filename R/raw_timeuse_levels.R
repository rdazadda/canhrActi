# Ported from GGIR 3.3-9 R/g.getbout.R and R/identify_level.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. g.getbout became .raw.getbout and
# identify_levels became .raw.identify.levels with explicit arguments (a params_phyact
# list is still accepted); three dead lines of g.getbout are not transcribed; the two
# while loops gained an unreachable iteration guard; the two g.getbout defects that
# parity requires are reproduced by default and switched off with ggir_exact = FALSE; the
# legend table GGIR builds inline became .raw.timeuse.class.dictionary. The intensity
# gradient lives in R/raw_timeuse_fragmentation.R.

#' Detect Activity Bouts the Way GGIR Part 5 Does
#'
#' Marks every epoch that belongs to a bout of the behaviour \code{x} encodes. A bout is a
#' run of epochs around a core window whose mean qualifying fraction reaches
#' \code{boutcriter}, opened and closed on epochs that themselves qualify, with gaps of up
#' to one minute bridged and counted as bout time. \code{boutcriter} is a floor on a centred
#' window of \code{boutduration} epochs, not on the bout finally marked, so
#' \code{boutduration} is a soft minimum, and the one-minute gap is hard-wired through a
#' \code{-boutduration} sentinel rather than a length test.
#'
#' @details \code{ggir_exact = TRUE} reproduces two GGIR behaviours every GGIR number depends
#'   on: the end-of-bout gap test sums over \code{look4start} rather than the \code{look4end}
#'   just computed, so it is vacuous and closing reduces to the last qualifying epoch at or
#'   before \code{max(group) + half1}; and the marking \code{xt[start:end] = 2} happens in
#'   place inside the group loop, so a later group sees 2 where an earlier group claimed
#'   epochs and its start can be pushed past the claimed region, which is how a one-minute
#'   bout definition produces runs of one or two epochs. \code{ggir_exact = FALSE} reads
#'   \code{look4end} and a snapshot of \code{xt}; it is for sensitivity work only. The
#'   iteration guard on the two while loops cannot fire on a terminating search.
#'
#' @param x Numeric 0/1 vector over the epoch grid, 1 where the epoch meets the intensity
#'   criterion. NA is treated as 0.
#' @param boutduration Bout duration in epochs, not minutes. GGIR's callers pass
#'   \code{boutdur * (60 / ws3)}.
#' @param boutcriter Minimum fraction, between 0 and 1, of a centred window of \code{boutduration}
#'   epochs that must meet the criterion.
#' @param ws3 Epoch length in seconds. Sets the one-minute gap tolerance, \code{60 / ws3}
#'   epochs.
#' @param ggir_exact TRUE reproduces the vacuous end test and the in-place marking. Parity
#'   requires TRUE.
#' @param max_iterations Iteration guard for the two search loops. NULL picks a bound no
#'   terminating search can reach.
#' @return Numeric vector the length of \code{x}, 1 on every epoch inside a detected bout and
#'   0 elsewhere, gap epochs inside a bout included.
#' @keywords internal
#' @noRd
.raw.getbout <- function(x, boutduration, boutcriter = 0.8, ws3 = 5,
                         ggir_exact = TRUE, max_iterations = NULL) {
  x[is.na(x)] <- 0 # NA counts as 0
  # breaks larger than 1 minute; the + 1 makes it "larger than", and the padding keeps the
  # first and last epochs in play
  zeroes <- rep(0, ceiling(60 / ws3 / 2))
  xtmp <- c(zeroes, x, zeroes)
  lookforbreaks <- zoo::rollmean(x = xtmp, k = (60 / ws3) + 1, align = "center",
                                 fill = rep(0, 3))
  keep <- (length(zeroes) + 1):(length(lookforbreaks) - length(zeroes))
  # negative sentinels so that a break longer than 1 minute cannot be counted in a bout
  xtmp[lookforbreaks == 0] <- -boutduration
  xt <- xtmp[keep]
  # pad for the centred criterion window
  append <- rep(-boutduration, ceiling(boutduration / 2))
  xtmp <- c(append, xt, append)
  RM <- zoo::rollmean(x = xtmp, k = boutduration, align = "center", fill = rep(0, 3))
  keep <- (length(append) + 1):(length(RM) - length(append))
  RM <- RM[keep]
  p <- which(RM >= boutcriter)
  half1 <- floor(boutduration / 2)

  # now mark all epochs that are covered by the detected bout/s
  detected_bouts <- split(p, cumsum(c(1, diff(p) != 1)))
  if (length(detected_bouts) == 1 & length(detected_bouts[[1]]) == 0) {
    x[which(xt != 2)] <- 0
    x[which(xt == 2)] <- 1
  } else {
    # xt_search is read only when ggir_exact is FALSE
    xt_search <- xt
    if (is.null(max_iterations)) {
      max_iterations <- length(xt) + ceiling(boutduration) + 10L
    }
    for (bout_i in 1:length(detected_bouts)) {
      bout <- detected_bouts[[bout_i]]
      xs <- if (ggir_exact) xt else xt_search
      # find start of bout
      start_found <- FALSE
      adjust <- 0
      iteration <- 0
      while (start_found == FALSE) {
        iteration <- iteration + 1
        if (iteration > max_iterations) {
          stop("bout start search did not terminate within ", max_iterations,
               " iterations", call. = FALSE)
        }
        start <- min(bout) - half1 + adjust
        if (start < 1) {
          adjust <- adjust + 1
          next
        }
        if (xs[start] != 1) {
          # a bout cannot start without meetin threshold crit
          adjust <- adjust + 1
          next
        }
        start_period <- start:max(bout)
        look4start <- split(xs[start_period], cumsum(c(1, diff(xs[start_period]) != 0)))
        max_gap <- 60 / ws3
        zeros <- c()
        for (i in 1:length(look4start)) {
          zeros <- c(zeros, sum(look4start[[i]] == 0))
        }
        if (all(zeros <= max_gap)) start_found <- TRUE
        adjust <- adjust + 1
      }
      # find end of bout
      end_found <- FALSE
      adjust <- 0
      iteration <- 0
      while (end_found == FALSE) {
        iteration <- iteration + 1
        if (iteration > max_iterations) {
          stop("bout end search did not terminate within ", max_iterations,
               " iterations", call. = FALSE)
        }
        end <- max(bout) + half1 - adjust
        if (end > length(xs)) {
          adjust <- adjust + 1
          next
        }
        if (xs[end] != 1) {
          adjust <- adjust + 1
          next
        }
        end_period <- min(bout):end
        look4end <- split(xs[end_period], cumsum(c(1, diff(xs[end_period]) != 0)))
        max_gap <- 60 / ws3
        zeros <- c()
        # GGIR sums look4start here, not look4end, so the end gap test is vacuous
        look4gap <- if (ggir_exact) look4start else look4end
        for (i in 1:length(look4gap)) {
          zeros <- c(zeros, sum(look4gap[[i]] == 0))
        }
        if (all(zeros <= max_gap)) end_found <- TRUE
        adjust <- adjust + 1
      }
      xt[start:end] <- 2
    }
    x[which(xt != 2)] <- 0
    x[which(xt == 2)] <- 1
  }
  return(x)
}

#' Resolve One Bout Setting From an Explicit Argument or a params_phyact List
#'
#' An explicit argument wins; the list is consulted only when the argument is NULL.
#'
#' @param value The explicit argument, or NULL.
#' @param params_phyact A named list, or NULL.
#' @param name Member name to look up in \code{params_phyact}.
#' @return The resolved value.
#' @keywords internal
#' @noRd
.raw.levels.setting <- function(value, params_phyact, name) {
  if (!is.null(value)) return(value)
  if (!is.null(params_phyact) && name %in% names(params_phyact) &&
      length(params_phyact[[name]]) > 0) {
    return(params_phyact[[name]])
  }
  stop("identify.levels needs ", name,
       ", either as an argument or as a member of params_phyact", call. = FALSE)
}

#' Classify Every Epoch Into GGIR Part 5's Behaviour Ladder
#'
#' Labels each epoch of the part-5 time series with one integer class covering intensity,
#' context (waking or sleep period time) and bout membership, and returns the parallel class
#' names, the bout-free intensity partition and the three bout indicator matrices.
#'
#' The first nine classes are fixed:
#'
#' \tabular{rll}{
#'   id \tab name \tab rule \cr
#'   0 \tab spt_sleep    \tab sibdetection == 1 and diur == 1 \cr
#'   1 \tab spt_wake_IN  \tab sibdetection == 0 and diur == 1 \cr
#'   2 \tab spt_wake_LIG \tab plus TRLi <= ACC < TRMi \cr
#'   3 \tab spt_wake_MOD \tab plus TRMi <= ACC < TRVi \cr
#'   4 \tab spt_wake_VIG \tab plus ACC >= TRVi \cr
#'   5 \tab day_IN_unbt  \tab diur == 0 and ACC < TRLi \cr
#'   6 \tab day_LIG_unbt \tab diur == 0 and TRLi <= ACC < TRMi \cr
#'   7 \tab day_MOD_unbt \tab diur == 0 and TRMi <= ACC < TRVi \cr
#'   8 \tab day_VIG_unbt \tab diur == 0 and ACC >= TRVi
#' }
#'
#' Class 9 upward are the bout classes and depend on the configuration. At the GGIR defaults
#' they are day_MVPA_bts_10, day_MVPA_bts_5_10, day_MVPA_bts_1_5, day_IN_bts_30,
#' day_IN_bts_20_30, day_IN_bts_10_20, day_LIG_bts_10, day_LIG_bts_5_10 and day_LIG_bts_1_5,
#' 18 classes in all.
#'
#' @details The three passes run in the fixed order MVPA, inactivity, light, each over its
#'   \code{boutdur} vector in the order given (GGIR's caller sorts it descending), each seeded
#'   only where \code{refe == 0}, so the bands are complementary residuals. The MVPA seed is
#'   \code{ACC >= TRMi} with no upper bound; \code{TRVi} only splits the unbouted waking
#'   classes. A bout absorbs gaps of a different intensity, so the unbouted class plus the
#'   bout classes of one intensity is not that intensity's total; \code{LEVELS} and
#'   \code{OLEVELS} (1 IN, 2 LIG, 3 MOD, 4 VIG, 0 in the sleep period, taken before any bout
#'   pass) are two partitions of the same waking time.
#'
#'   \code{ACC} is milli-g for a g-unit metric such as ENMO and counts per epoch for a count
#'   metric. The 40 / 100 / 400 defaults are the rounded Hildebrand adult non-dominant-wrist
#'   ENMO values; for older adults Migueles et al. (2021) give 18 mg light and 60 mg moderate
#'   for the ActiGraph non-dominant wrist. These thresholds are not comparable with the
#'   counts-per-minute cut-points in \code{apply_cutpoints}.
#'
#' @param ts Data frame with at least \code{time} (used for its length only), \code{ACC},
#'   \code{diur} and \code{sibdetection}, one row per epoch.
#' @param TRLi,TRMi,TRVi Light, moderate and vigorous thresholds on \code{ts$ACC}.
#' @param ws3 Epoch length in seconds.
#' @param params_phyact Optional GGIR-shaped list holding boutdur.mvpa, boutdur.in,
#'   boutdur.lig, boutcriter.mvpa, boutcriter.in and boutcriter.lig. Any explicit argument
#'   below wins over it.
#' @param boutdur.mvpa,boutdur.in,boutdur.lig Bout duration bands in minutes, sorted
#'   descending by the caller.
#' @param boutcriter.mvpa,boutcriter.in,boutcriter.lig Minimum qualifying fraction per pass.
#' @param ggir_exact Passed to \code{.raw.getbout}. Parity requires TRUE.
#' @return Invisibly, a list with \code{LEVELS} (integer class per epoch), \code{OLEVELS}
#'   (the bout-free intensity partition), \code{Lnames}, the bout indicator matrices
#'   \code{bc.mvpa}, \code{bc.lig} and \code{bc.in} (one row per band), \code{ts} unchanged
#'   and \code{threshold}, in GGIR's order.
#' @keywords internal
#' @noRd
.raw.identify.levels <- function(ts, TRLi, TRMi, TRVi, ws3, params_phyact = NULL,
                                 boutdur.mvpa = NULL, boutdur.in = NULL, boutdur.lig = NULL,
                                 boutcriter.mvpa = NULL, boutcriter.in = NULL,
                                 boutcriter.lig = NULL, ggir_exact = TRUE) {
  needed <- c("time", "ACC", "diur", "sibdetection")
  missing_cols <- needed[!needed %in% names(ts)]
  if (length(missing_cols) > 0) {
    # GGIR does not check; a missing column would label nothing, silently
    stop("ts is missing the column(s) ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  boutdur.mvpa <- .raw.levels.setting(boutdur.mvpa, params_phyact, "boutdur.mvpa")
  boutdur.in <- .raw.levels.setting(boutdur.in, params_phyact, "boutdur.in")
  boutdur.lig <- .raw.levels.setting(boutdur.lig, params_phyact, "boutdur.lig")
  boutcriter.mvpa <- .raw.levels.setting(boutcriter.mvpa, params_phyact, "boutcriter.mvpa")
  boutcriter.in <- .raw.levels.setting(boutcriter.in, params_phyact, "boutcriter.in")
  boutcriter.lig <- .raw.levels.setting(boutcriter.lig, params_phyact, "boutcriter.lig")

  # label intensity levels
  LEVELS <- rep(0, length(ts$time))
  OLEVELS <- rep(0, length(ts$time)) # to capture moderate and vigorous seperately
  LEVELS[ts$sibdetection == 1 & ts$diur == 1] <- 0 # Sleep during the Sleep Period Time Window
  LEVELS[ts$sibdetection == 0 & ts$diur == 1] <- 1 # Wakefullness during Sleep Period Time Window
  # activity during the night
  LEVELS[ts$sibdetection == 0 & ts$diur == 1 & ts$ACC >= TRLi & ts$ACC < TRMi] <- 2 # LIGHT
  LEVELS[ts$sibdetection == 0 & ts$diur == 1 & ts$ACC >= TRMi & ts$ACC < TRVi] <- 3 # MODERATE
  LEVELS[ts$sibdetection == 0 & ts$diur == 1 & ts$ACC >= TRVi] <- 4 # VIGOROUS
  Lnames <- c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
  # activity during the day
  LEVELS[ts$diur == 0 & ts$ACC < TRLi] <- 5 # INACTIVE
  LEVELS[ts$diur == 0 & ts$ACC >= TRLi & ts$ACC < TRMi] <- 6 # LIGHT
  LEVELS[ts$diur == 0 & ts$ACC >= TRMi & ts$ACC < TRVi] <- 7 # MODERATE
  LEVELS[ts$diur == 0 & ts$ACC >= TRVi] <- 8 # VIGOROUS
  Lnames <- c(Lnames, "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt")
  # bout-free copy; 0 is the sleep period time window
  OLEVELS[LEVELS == 5] <- 1 # IN
  OLEVELS[LEVELS == 6] <- 2 # LIGHT
  OLEVELS[LEVELS == 7] <- 3 # MOD
  OLEVELS[LEVELS == 8] <- 4 # VIG

  # MVPA BOUTS
  LN <- length(ts$time)
  boutduration <- boutdur.mvpa * (60 / ws3)
  NBL <- length(boutduration) # number of bout lengths
  CL <- 9 # current level
  refe <- rep(0, LN)
  bc.mvpa <- c()
  for (BL in 1:NBL) {
    rr1 <- rep(0, LN)
    p <- which(ts$ACC >= TRMi & refe == 0 & ts$diur == 0); rr1[p] <- 1
    out1 <- .raw.getbout(x = rr1, boutduration = boutduration[BL],
                         boutcriter = boutcriter.mvpa, ws3 = ws3, ggir_exact = ggir_exact)
    LEVELS[ts$diur == 0 & out1 == 1] <- CL
    bc.mvpa <- rbind(bc.mvpa, out1)
    refe <- refe + out1
    if (BL == 1) {
      Lnames <- c(Lnames, paste0("day_MVPA_bts_", boutdur.mvpa[BL]))
    } else {
      Lnames <- c(Lnames, paste0("day_MVPA_bts_", boutdur.mvpa[BL], "_",
                                 boutdur.mvpa[BL - 1]))
    }
    CL <- CL + 1
  }
  # INACTIVITY BOUTS
  LN <- length(ts$time)
  boutduration <- boutdur.in * (60 / ws3)
  NBL <- length(boutduration)
  bc.in <- c()
  for (BL in 1:NBL) {
    rr1 <- rep(0, LN)
    p <- which(ts$ACC < TRLi & refe == 0 & ts$diur == 0); rr1[p] <- 1
    out1 <- .raw.getbout(x = rr1, boutduration = boutduration[BL],
                         boutcriter = boutcriter.in, ws3 = ws3, ggir_exact = ggir_exact)
    LEVELS[ts$diur == 0 & out1 == 1] <- CL
    bc.in <- rbind(bc.in, out1)
    refe <- refe + out1
    if (BL == 1) {
      Lnames <- c(Lnames, paste0("day_IN_bts_", boutdur.in[BL]))
    } else {
      Lnames <- c(Lnames, paste0("day_IN_bts_", boutdur.in[BL], "_", boutdur.in[BL - 1]))
    }
    CL <- CL + 1
  }
  # LIGHT BOUTS
  LN <- length(ts$time)
  boutduration <- boutdur.lig * (60 / ws3)
  NBL <- length(boutduration)
  bc.lig <- c()
  for (BL in 1:NBL) {
    rr1 <- rep(0, LN)
    p <- which(ts$ACC >= TRLi & refe == 0 & ts$ACC < TRMi & ts$diur == 0); rr1[p] <- 1
    out1 <- .raw.getbout(x = rr1, boutduration = boutduration[BL],
                         boutcriter = boutcriter.lig, ws3 = ws3, ggir_exact = ggir_exact)
    LEVELS[ts$diur == 0 & out1 == 1] <- CL
    bc.lig <- rbind(bc.lig, out1)
    refe <- refe + out1
    if (BL == 1) {
      Lnames <- c(Lnames, paste0("day_LIG_bts_", boutdur.lig[BL]))
    } else {
      Lnames <- c(Lnames, paste0("day_LIG_bts_", boutdur.lig[BL], "_", boutdur.lig[BL - 1]))
    }
    CL <- CL + 1
  }
  invisible(list(LEVELS = LEVELS, OLEVELS = OLEVELS, Lnames = Lnames,
                 bc.mvpa = bc.mvpa, bc.lig = bc.lig, bc.in = bc.in, ts = ts,
                 threshold = c(TRLi, TRMi, TRVi)))
}

#' The Behaviour Class Dictionary for One Classified Time Series
#'
#' Pairs each class name with the integer that stands for it in the classified series, the
#' legend GGIR writes to meta/ms5.outraw/behavioralcodes<date>.csv. Ids 0 to 8 are fixed, ids
#' 9 upward depend on the three \code{boutdur} vectors, and the nap branch appends day_nap, so
#' always read the dictionary from the run that produced the series.
#'
#' @param Lnames Character vector of class names, in class order, as returned by
#'   \code{.raw.identify.levels}.
#' @return Data frame with \code{class_name} and \code{class_id}, the ids running
#'   0 to \code{length(Lnames) - 1}.
#' @keywords internal
#' @noRd
.raw.timeuse.class.dictionary <- function(Lnames) {
  if (length(Lnames) == 0 || !is.character(Lnames)) {
    stop("Lnames must be a character vector of class names", call. = FALSE)
  }
  data.frame(class_name = Lnames, class_id = 0:(length(Lnames) - 1),
             stringsAsFactors = FALSE)
}
