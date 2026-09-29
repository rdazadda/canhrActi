# Ported from GGIR 3.3-9 R/g.fragmentation.R and R/g.intensitygradient.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The nested TransProb is lifted out as
# .raw.frag.transprob; the two defects of the power-law block (the unfiltered xmin Hill
# fit and the mis-parenthesised x0.5) sit behind ggir_exact, which defaults to TRUE; the
# unread do.frag assignment is dropped; rle's partial matches frag$value and frag$length
# are spelled out; ineq::Gini (R package ineq, Achim Zeileis, GPL-2 | GPL-3) is
# transcribed as .raw.gini so that ineq is not a dependency; and a warning is raised when
# the power-law metrics are requested at an xmin other than 1.

# GINI

#' Gini Coefficient of a Numeric Vector
#'
#' A transcription of \code{ineq::Gini}, as GGIR's fragmentation code calls it with
#' \code{corr = TRUE}. canhrActi's own \code{.calculate.gini} forms the numerator and
#' denominator differently, clamps to 0 to 1 and returns NA where this returns NaN, so it
#' is not bit-identical and cannot stand in; nothing else in the package should call this.
#'
#' @param x Numeric vector. For GGIR's fragmentation these are fragment durations in
#'   epochs, not minutes.
#' @param corr TRUE applies the small-sample correction, dividing by \code{n - 1} instead of
#'   \code{n}. GGIR always passes TRUE.
#' @param na.rm TRUE drops NA before the calculation; FALSE returns NA_real_ when any
#'   element is NA.
#'
#' @return A single numeric. NaN when \code{sum(x)} is zero, and NaN when \code{corr} is
#'   TRUE and \code{x} has one element.
#' @keywords internal
#' @noRd
.raw.gini <- function(x, corr = FALSE, na.rm = TRUE) {
  if (!na.rm && any(is.na(x))) return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- sum(x * 1L:n)
  G <- 2 * G / sum(x) - (n + 1L)
  if (corr) G / (n - 1L) else G / n
}

# TRANSITION COUNTER

#' Count Transitions Between Two Sets of Class Ids
#'
#' GGIR's nested \code{TransProb}, the single transition counter behind every part-5
#' fragmentation metric. Given a class series it counts the runs of class \code{a}
#' immediately followed by a run of class \code{b} and the reverse, and returns both
#' per-epoch transition probabilities together with two duration totals.
#'
#' @details \code{totDur_ab} holds two different quantities inside one call: first the total
#'   duration of all \code{a} runs minus the number of strings ending in \code{a}, which the
#'   transition probability is divided by, then the sum of only those \code{a} runs followed
#'   by a \code{b} run, which is returned and used for \code{mean_dur_*} and
#'   \code{NFragPM_*}. \code{Nab} counts transitions, not runs, so it is one fewer than the
#'   number of \code{a} runs when the series ends on \code{a}. An epsilon of 1e-6 is added to
#'   numerator and denominator before rounding to six digits. The \code{lastitems}
#'   construction has a misplaced closing bracket,
#'   \code{is.na(frag$values[2:Nsegments] == TRUE)}, harmless in part 5 because LEVELS carry
#'   no NA; transcribed as written because part 6 splits the series with NA runs.
#'
#' @param x Integer or numeric class series, one element per epoch. NA is permitted.
#' @param a,b Class ids, or vectors of class ids, defining the two sets.
#'
#' @return An invisible list with TPab, TPba (per-epoch transition probabilities, rounded to
#'   six digits, NA when the series holds no runs), Nab, Nba (transition counts) and
#'   totDur_ab, totDur_ba (the overwritten totals, in epochs).
#' @keywords internal
#' @noRd
.raw.frag.transprob <- function(x, a = 1, b = c(2, 3)) {
  TPab <- TPba <- NA
  Nab <- Nba <- 0
  totDur_ab <- totDur_ba <- 0
  if (length(x) > 0) {
    frag <- rle(x)
    Nsegments <- length(frag$values)
    if (Nsegments > 1) {
      # more than one segment
      ab <- which(frag$values[1:(Nsegments - 1)] %in% a &
                    frag$values[2:Nsegments] %in% b)
      ba <- which(frag$values[1:(Nsegments - 1)] %in% b &
                    frag$values[2:Nsegments] %in% a)
      Nab <- length(ab)
      Nba <- length(ba)

      # part 6 splits the series with NA runs; in part 5 only the last item is needed
      lastitems <- which(is.na(frag$values[1:(Nsegments - 1)]) == FALSE &
                           is.na(frag$values[2:Nsegments] == TRUE))
      if (!is.na(frag$values[Nsegments])) {
        lastitems <- unique(c(lastitems, Nsegments))
      }
      tmp <- frag$values[-lastitems]
      # how often the last value of each string is a reference class
      if (length(lastitems) > 0 & length(which(is.na(tmp) == FALSE)) > 1) {
        count_a_ending <- sum(ifelse(frag$values[lastitems] %in% a, 1, 0))
        count_b_ending <- sum(ifelse(frag$values[lastitems] %in% b, 1, 0))
      } else {
        count_a_ending <- 0
        count_b_ending <- 0
      }
      # Total duration from a to b
      totDur_ab <- sum(frag$lengths[which(frag$values %in% a)]) - count_a_ending
      totDur_ba <- sum(frag$lengths[which(frag$values %in% b)]) - count_b_ending
      epsilon <- 1e-6
      TPab <- (Nab + epsilon) / (totDur_ab + epsilon)
      TPba <- (Nba + epsilon) / (totDur_ba + epsilon)
      # Round to 6 digits because the preceding step introduces bias at 7 decimal places
      TPab <- round(TPab, digits = 6)
      TPba <- round(TPba, digits = 6)
      durations_ab <- frag$lengths[ab]
      durations_ba <- frag$lengths[ba]
      totDur_ab <- sum(durations_ab)
      totDur_ba <- sum(durations_ba)
    } else if (Nsegments == 1) {
      # only one segment
      if (frag$values[1] %in% a) { # Only a
        TPab <- 0
        TPba <- 1
        totDur_ab <- frag$lengths[1]
        totDur_ba <- 0
        Nab <- 1
        Nba <- 0
      } else { # Only b
        TPab <- 1
        TPba <- 0
        totDur_ab <- 0
        totDur_ba <- frag$lengths[1]
        Nab <- 0
        Nba <- 1
      }
    }
  }
  invisible(list(TPab = TPab,
                 TPba = TPba,
                 Nab = Nab, Nba = Nba,
                 totDur_ab = totDur_ab,
                 totDur_ba = totDur_ba))
}

# FRAGMENTATION

#' GGIR Part-5 Behavioural Fragmentation Metrics
#'
#' GGIR's \code{g.fragmentation}: the thirty fragmentation columns of the part-5 day
#' summary, computed from one segment of the classified behaviour series, so that the
#' GGIR-format export carries the \code{FRAG_} columns GGIR would have written.
#'
#' @section This is not canhrActi's fragmentation:
#' Use \code{sedentary.fragmentation} for any fragmentation number canhrActi reports. GGIR
#' fits the power-law exponent with a closed-form Hill estimator at a fixed theoretical
#' \code{xmin} over every fragment, where \code{sedentary.fragmentation} runs the Clauset
#' xmin search, fits the tail only, bootstraps a confidence interval and a goodness-of-fit
#' p-value, and tests the power law against an exponential. \code{xmin} is passed as 60
#' divided by the epoch length, so at a 5 s epoch it is 12 while the shortest observable
#' fragment is 1; most fragments then sit below xmin, alpha falls below 1 and x0.5 collapses
#' towards zero. Those numbers match GGIR and mean nothing; they are interpretable only with
#' \code{part5_agg2_60seconds = TRUE}, and this function warns at any other xmin. The two
#' engines also count differently: GGIR counts transitions and drops the terminal fragment,
#' canhrActi counts fragments; GGIR's gap bridging is a fixed one minute inside
#' \code{.raw.getbout}, canhrActi bridges up to \code{min_break_length}.
#'
#' @section Units:
#' Every duration is in epochs, not minutes, including \code{mean_dur_*}, \code{SD_dur_*},
#' \code{x0.5_*} and every \code{TP} and \code{NFragPM} denominator.
#'
#' @details Class ids are recovered by name from \code{Lnames}, so a nap-modified Lnames
#'   still works. The input is the waking subset of LEVELS after bout detection, not a
#'   threshold mask. The output name order is fixed by R's right-to-left evaluation of the
#'   chained NA pre-allocations, which is why \code{Nfrag_PA2IN} precedes \code{Nfrag_IN2PA}.
#'   Both gates count inactive plus active fragments together: more than 1 adds the mean
#'   durations and NFragPM, 10 or more adds SD, Gini, CoV and the power block. Gini and CoV
#'   run on the binary run-length encoding including the terminal fragment, while
#'   \code{mean_dur_*} excludes it. A partial \code{frag.metrics} gives a ragged column
#'   count, because \code{SD_dur_*} is pre-allocated only when power, CoV or Gini is asked
#'   for; the raggedness is reproduced. The \code{mode = "spt"} branch belongs to GGIR part
#'   6 and is transcribed because the function is shared with it.
#'
#' @param frag.metrics Character vector, any of "mean", "TP", "Gini", "power", "CoV",
#'   "NFragPM" and "all". Anything else is silently ignored, as in GGIR.
#' @param LEVELS Integer class ids for the epochs of one segment, as produced by
#'   \code{.raw.identify.levels}. In part 5 this is the waking subset of one day window.
#' @param Lnames The parallel class-name vector. Class ids are recovered from it by name.
#' @param xmin Shortest recordable fragment length in epochs. GGIR part 5 passes 60 divided
#'   by the epoch length in seconds.
#' @param mode "day" for the waking-hours metrics of part 5, or "spt" for the eight
#'   sleep-period-time columns part 6 uses.
#' @param ggir_exact TRUE, the default, reproduces GGIR bit for bit, including two defects
#'   of the power block: the Hill fit uses every fragment, including those below xmin, and
#'   \code{x0.5} is \code{2^(1 / (alpha - 1) * xmin)}, whose precedence makes the exponent
#'   \code{xmin/(alpha - 1)}, where Chastin (2010) gives \code{xmin * 2^(1/(alpha - 1))}.
#'   FALSE filters the durations to those at or above xmin and applies Chastin's
#'   parenthesisation, and breaks GGIR parity.
#' @param warn_xmin TRUE raises one warning per call when the power metrics are requested at
#'   an xmin other than 1.
#'
#' @return A named list. With \code{mode = "day"} and \code{frag.metrics = "all"} it has
#'   thirty elements in this order: Nfrag_PA2IN, Nfrag_IN2PA, TP_PA2IN, TP_IN2PA,
#'   Nfrag_IN2LIPA, TP_IN2LIPA, Nfrag_IN2MVPA, TP_IN2MVPA, mean_dur_LIPA, Nfrag_LIPA,
#'   mean_dur_MVPA, Nfrag_MVPA, Nfrag_PA, Nfrag_IN, mean_dur_IN, mean_dur_PA, Gini_dur_IN,
#'   Gini_dur_PA, CoV_dur_IN, CoV_dur_PA, alpha_dur_IN, alpha_dur_PA, x0.5_dur_IN,
#'   x0.5_dur_PA, W0.5_dur_IN, W0.5_dur_PA, SD_dur_IN, SD_dur_PA, NFragPM_PA, NFragPM_IN.
#'   A partial \code{frag.metrics} returns a shorter list. With \code{mode = "spt"} it has
#'   eight elements: Nfrag_IN, Nfrag_PA, TP_IN2PA, TP_PA2IN, Nfrag_sleep, Nfrag_wake,
#'   TP_sleep2wake, TP_wake2sleep.
#' @keywords internal
#' @noRd
.raw.fragmentation <- function(frag.metrics = c("mean", "TP", "Gini", "power",
                                                "CoV", "NFragPM", "all"),
                               LEVELS = c(),
                               Lnames = c(), xmin = 1,
                               mode = "day",
                               ggir_exact = TRUE,
                               warn_xmin = TRUE) {
  # loosely inspired by the ActFrag package (Junrui Di); non-wear and missing values are
  # assumed to be imputed already or set to NA
  if ("all" %in% frag.metrics) {
    frag.metrics <- c("mean", "TP", "Gini", "power",
                      "CoV", "NFragPM", "all")
  }
  if (isTRUE(warn_xmin) && "power" %in% frag.metrics && xmin != 1) {
    warning(paste0("GGIR's power-law fragmentation metrics are not interpretable at this ",
                   "epoch length: xmin is ", xmin, " epochs while the shortest observable ",
                   "fragment is 1 epoch, so most fragments sit below xmin, alpha_dur_* ",
                   "falls below 1 and x0.5_dur_* collapses towards zero. Aggregate to 60 s ",
                   "epochs (part5_agg2_60seconds = TRUE), which makes xmin 1, or use ",
                   "sedentary.fragmentation() instead."),
            call. = FALSE)
  }
  output <- list()
  Nepochs <- length(LEVELS)
  if (mode == "day") {
    # convert class names to numeric class ids for inactive, LIPA and MVPA:
    classes.in <- c("day_IN_unbt", Lnames[grep(pattern = "day_IN_bts", x = Lnames)])
    class.in.ids <- which(Lnames %in% classes.in) - 1
    classes.lig <- c("day_LIG_unbt", Lnames[grep(pattern = "day_LIG_bts", x = Lnames)])
    class.lig.ids <- which(Lnames %in% classes.lig) - 1
    classes.mvpa <- c("day_MOD_unbt", "day_VIG_unbt",
                      Lnames[grep(pattern = "day_MVPA_bts", x = Lnames)])
    class.mvpa.ids <- which(Lnames %in% classes.mvpa) - 1
  }
  # expected output, to standardise length and names; R evaluates the chained assignments
  # right to left, which fixes the column order
  if ("TP" %in% frag.metrics && mode == "day") {
    output[["TP_IN2PA"]] <- output[["TP_PA2IN"]] <-
      output[["Nfrag_IN2PA"]] <- output[["Nfrag_PA2IN"]] <- NA
    output[["TP_IN2LIPA"]] <- output[["Nfrag_IN2LIPA"]] <- NA
    output[["TP_IN2MVPA"]] <- output[["Nfrag_IN2MVPA"]] <- NA
    output[["Nfrag_LIPA"]] <- output[["mean_dur_LIPA"]] <- NA
    output[["Nfrag_MVPA"]] <- output[["mean_dur_MVPA"]] <- NA
  }
  if (Nepochs > 1 & mode == "day") { # metrics that require more than just binary
    # LEVELS in three classes: inactivity (1), LIPA (2), MVPA (3)
    y <- rep(0, Nepochs)
    is.na(y[is.na(LEVELS)]) <- TRUE
    y[which(LEVELS %in% class.in.ids)] <- 1
    y[which(LEVELS %in% class.lig.ids)] <- 2
    y[which(LEVELS %in% class.mvpa.ids)] <- 3
    # TP metrics that depend on multiple classes
    if ("TP" %in% frag.metrics) {
      out <- .raw.frag.transprob(y, a = 1, b = c(2, 3)) # IN to and from PA
      output[["TP_IN2PA"]] <- out$TPab
      output[["TP_PA2IN"]] <- out$TPba
      output[["Nfrag_IN2PA"]] <- out$Nab
      output[["Nfrag_PA2IN"]] <- out$Nba

      out <- .raw.frag.transprob(y, a = 1, b = 2) # IN to LIPA
      output[["TP_IN2LIPA"]] <- out$TPab
      output[["Nfrag_IN2LIPA"]] <- out$Nab

      out <- .raw.frag.transprob(y, a = 1, b = 3) # IN to MVPA
      output[["TP_IN2MVPA"]] <- out$TPab
      output[["Nfrag_IN2MVPA"]] <- out$Nab

      out <- .raw.frag.transprob(y, a = 2, b = c(1, 3)) # LIPA to and from the rest
      output[["Nfrag_LIPA"]] <- out$Nab
      output[["mean_dur_LIPA"]] <- out$totDur_ab / out$Nab

      out <- .raw.frag.transprob(y, a = 3, b = c(1, 2)) # MVPA to and from the rest
      output[["Nfrag_MVPA"]] <- out$Nab
      output[["mean_dur_MVPA"]] <- out$totDur_ab / out$Nab
    }
  }
  # binary fragmentation for the metrics that do not depend on multiple classes
  if (mode == "day") {
    x <- rep(0, Nepochs)
    is.na(x[is.na(LEVELS)]) <- TRUE
    # inactivity becomes 1 because this is the behaviour of interest
    x[which(LEVELS %in% class.in.ids)] <- 1
    x <- as.integer(x)
    frag2levels <- rle(x)
    Nfrag2levels <- length(which(is.na(frag2levels$values) == FALSE))
    out <- .raw.frag.transprob(x, a = 1, b = 0) # IN to and from PA
    output[["Nfrag_PA"]] <- out$Nba
    output[["Nfrag_IN"]] <- out$Nab
    # Define default values
    if ("mean" %in% frag.metrics) {
      output[["mean_dur_PA"]] <- output[["mean_dur_IN"]] <- 0
    }
    if ("Gini" %in% frag.metrics) {
      output[["Gini_dur_PA"]] <- output[["Gini_dur_IN"]] <- NA
    }
    if ("CoV" %in% frag.metrics) {
      output[["CoV_dur_PA"]] <- output[["CoV_dur_IN"]] <- NA
    }
    if ("power" %in% frag.metrics) {
      output[["alpha_dur_PA"]] <- output[["alpha_dur_IN"]] <- NA
      output[["x0.5_dur_PA"]] <- output[["x0.5_dur_IN"]] <- NA
      output[["W0.5_dur_PA"]] <- output[["W0.5_dur_IN"]] <- NA
    }
    if ("power" %in% frag.metrics | "CoV" %in% frag.metrics | "Gini" %in% frag.metrics) {
      output[["SD_dur_PA"]] <- output[["SD_dur_IN"]] <- NA
    }
    if ("NFragPM" %in% frag.metrics) {
      output[["NFragPM_PA"]] <- 0
      output[["NFragPM_IN"]] <- 0
    }
    if (Nfrag2levels > 1) {
      if ("mean" %in% frag.metrics) {
        output[["mean_dur_PA"]] <- out$totDur_ba / out$Nba
        output[["mean_dur_IN"]] <- out$totDur_ab / out$Nab
      }
      if ("NFragPM" %in% frag.metrics) {
        # Chastin's fragmentation index, renamed Number of Fragments Per Minute
        output[["NFragPM_PA"]] <- output[["Nfrag_PA"]] / out$totDur_ba
        output[["NFragPM_IN"]] <- output[["Nfrag_IN"]] / out$totDur_ab
      }
      # at least 10 fragments in total, because the metrics below need more than a few
      if (Nfrag2levels >= 10) {
        DurationIN <- frag2levels$lengths[which(frag2levels$values == 1)]
        DurationPA <- frag2levels$lengths[which(frag2levels$values == 0)]
        SD0 <- stats::sd(DurationPA, na.rm = TRUE)
        SD1 <- stats::sd(DurationIN, na.rm = TRUE)
        output[["SD_dur_PA"]] <- SD0
        output[["SD_dur_IN"]] <- SD1
        if ("Gini" %in% frag.metrics) {
          output[["Gini_dur_PA"]] <- .raw.gini(DurationPA, corr = TRUE)
          output[["Gini_dur_IN"]] <- .raw.gini(DurationIN, corr = TRUE)
        }
        if ("CoV" %in% frag.metrics) { # coefficient of variation as described by Blikman 2015
          output[["CoV_dur_PA"]] <- stats::sd(log(DurationPA)) / mean(log(DurationPA))
          output[["CoV_dur_IN"]] <- stats::sd(log(DurationIN)) / mean(log(DurationIN))
        }
        if ("power" %in% frag.metrics) {
          calc_alpha <- function(x, xmin) {
            if (!ggir_exact) {
              # GGIR keeps every fragment
              x <- x[x >= xmin]
            }
            nr <- length(x)
            alpha <- 1 + nr / sum(log(x / (xmin))) # adapted to match Chastin 2010, not ActFrag
            return(alpha)
          }
          if (SD0 != 0) {
            output[["alpha_dur_PA"]] <- calc_alpha(DurationPA, xmin)
            output[["x0.5_dur_PA"]] <- if (ggir_exact) {
              # GGIR's precedence makes the exponent xmin/(alpha - 1)
              2^(1 / (output[["alpha_dur_PA"]] - 1) * xmin)
            } else {
              xmin * 2^(1 / (output[["alpha_dur_PA"]] - 1)) # what Chastin 2010 gives
            }
            output[["W0.5_dur_PA"]] <-
              sum(DurationPA[which(DurationPA > output[["x0.5_dur_PA"]])]) / sum(DurationPA)
          }
          if (SD1 != 0) {
            output[["alpha_dur_IN"]] <- calc_alpha(DurationIN, xmin)
            output[["x0.5_dur_IN"]] <- if (ggir_exact) {
              2^(1 / (output[["alpha_dur_IN"]] - 1) * xmin)
            } else {
              xmin * 2^(1 / (output[["alpha_dur_IN"]] - 1))
            }
            output[["W0.5_dur_IN"]] <-
              sum(DurationIN[which(DurationIN > output[["x0.5_dur_IN"]])]) / sum(DurationIN)
          }
        }
      }
    }
  } else if (mode == "spt") {
    # active to rest transitions during SPT
    x <- rep(0, Nepochs)
    is.na(x[is.na(LEVELS)]) <- TRUE
    classes.pa <- c("spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
    class.pa <- which(Lnames %in% classes.pa) - 1
    PAi <- which(LEVELS %in% class.pa)
    if (length(PAi) > 0) {
      x[PAi] <- 1
    }
    out <- .raw.frag.transprob(x = x, a = 0, b = 1)
    output[["Nfrag_IN"]] <- out$Nab
    output[["Nfrag_PA"]] <- out$Nba
    output[["TP_IN2PA"]] <- out$TPab
    output[["TP_PA2IN"]] <- out$TPba
    # Wake - Sleep transitions during SPT:
    x <- rep(0, Nepochs)
    is.na(x[is.na(LEVELS)]) <- TRUE
    classes.wake <- c("spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
    class.wake <- which(Lnames %in% classes.wake) - 1
    wakei <- which(LEVELS %in% class.wake)
    if (length(wakei) > 0) {
      x[wakei] <- 1
    }
    out <- .raw.frag.transprob(x = x, a = 0, b = 1)
    output[["Nfrag_sleep"]] <- out$Nab
    output[["Nfrag_wake"]] <- out$Nba
    output[["TP_sleep2wake"]] <- out$TPab
    output[["TP_wake2sleep"]] <- out$TPba
  }
  return(output)
}

# INTENSITY GRADIENT

#' Intensity Gradient of an Acceleration Distribution
#'
#' GGIR's \code{g.intensitygradient}: Rowlands' intensity gradient, an ordinary least
#' squares fit of the log of the time spent in each acceleration bin on the log of the bin
#' mid-point. Part 5 calls it over the waking epochs for the \code{ig_day_*} triple and
#' over the whole window for \code{ig_day_spt_*}.
#'
#' @details Empty bins are set to NA before the log, with a non-strict \code{y <= 0} test,
#'   and \code{lm} drops them. All three values stay NA unless both log vectors have more
#'   than one non-NA element and a non-zero standard deviation, taken over the whole vectors.
#'
#' @param x Numeric vector of bin mid-points, in the acceleration unit of the series.
#' @param y Numeric vector of time spent in each bin, in minutes, the same length as
#'   \code{x}.
#'
#' @return An invisible list with gradient, y_intercept and rsquared, all NA when the fit
#'   cannot be made.
#' @keywords internal
#' @noRd
.raw.intensity.gradient <- function(x, y) {
  y <- ifelse(test = y <= 0, yes = NA, no = y)
  ly <- log(y)
  lx <- log(x)
  y_intercept <- NA
  gradient <- NA
  rsquared <- NA
  if (length(which(is.na(lx) == FALSE)) > 1 & length(which(is.na(ly) == FALSE)) > 1) {
    if (stats::sd(lx, na.rm = TRUE) != 0 & stats::sd(ly, na.rm = TRUE) != 0) {
      fitsum <- summary(stats::lm(ly ~ lx))
      y_intercept <- stats::coef(fitsum)[1, 1]
      gradient <- stats::coef(fitsum)[2, 1]
      rsquared <- fitsum$r.squared
    }
  }
  invisible(list(gradient = gradient, y_intercept = y_intercept, rsquared = rsquared))
}
