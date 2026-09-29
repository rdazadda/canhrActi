# Ported from GGIR 3.3-9 R/HASIB.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. HASIB became raw.sib, its nested
# helpers and the six algorithm branches became file-level internals, base generics are
# namespace qualified, and an unrecognised algorithm raises a named condition instead of
# GGIR's "object 'sib_classification' not found". Every threshold, kernel, index
# construction and built-in time misalignment is transcribed as it stands.

# The six values HASIB dispatches on, in the order of its if/else if chain
.raw.sib.algorithms <- c("vanHees2015", "Sadeh1994", "ColeKripke1992",
                         "Galland2012", "Oakley1997", "NotWorn")

#' Aggregate a Short-Epoch Series to a Longer Window
#'
#' Sums counts per epoch into counts per \code{summingwindow} seconds by cumulative-sum
#' differencing. A trailing partial window is dropped, and nothing checks that the stride
#' is a whole number, as in GGIR.
#'
#' @param x Numeric vector of counts, one value per epoch.
#' @param epochsize Epoch length of \code{x} in seconds.
#' @param summingwindow Aggregation window in seconds.
#' @return Numeric vector of window sums.
#' @keywords internal
#' @noRd
.raw.sib.sum.per.window <- function(x, epochsize, summingwindow = 60) {
  x2 <- cumsum(c(0, x))
  select <- seq(1, length(x2), by = summingwindow / epochsize)
  x3 <- diff(x2[select])
  return(x3)
}

#' Centred Sliding-Window Matrix for the Count-Based Algorithms
#'
#' Builds the \code{length(x)} by \code{Ncol} matrix that turns a rolling kernel into one
#' matrix multiplication. \code{out[r, jj] == x[r + ceiling(Ncol/2) - jj]}, so the columns
#' run future to past from left to right, and positions outside the series repeat
#' \code{x[1]} at the head and the last value at the tail. Every kernel below is written in
#' that column order.
#'
#' @param x Numeric vector.
#' @param Ncol Window width in elements.
#' @return A numeric matrix with \code{length(x)} rows and \code{Ncol} columns.
#' @keywords internal
#' @noRd
.raw.sib.rollfun.mat <- function(x, Ncol) {
  Nz <- length(x)
  # Generate matrix to ease applying rolling function
  x_matrix <- matrix(0, Nz + (Ncol - 1), Ncol)
  for (jj in 1:Ncol) {
    x_matrix[jj:(Nz + jj - 1), jj] <- x
    if (jj > 1) {
      x_matrix[1:(jj - 1), jj] <- x[1]
    }
    if (jj < Ncol) {
      x_matrix[((Nz - ((Ncol - 1) - jj)):Nz) + (Ncol - 1), jj] <- utils::tail(x, 1)
    }
  }
  # Remove redundant rows at start and end
  Nremove <- ceiling(Ncol / 2)
  x_matrix <- x_matrix[Nremove:(nrow(x_matrix) - (Nremove - 1)), ]
  return(x_matrix)
}

#' Upsample a Per-Window Score Back to the Short Epoch
#'
#' Repeats every window score \code{current_epochsize / new_epochsize} times, then pads
#' with zeros or truncates to \code{length(time)}. Epochs the algorithm could not score at
#' the end of the recording are therefore awake, not NA.
#'
#' @param x Numeric vector of per-window scores.
#' @param time The time vector of the recording; only its length is used.
#' @param new_epochsize Short epoch length in seconds (the output resolution).
#' @param current_epochsize Window length in seconds (the resolution of \code{x}).
#' @return A one-column numeric matrix with \code{length(time)} rows.
#' @keywords internal
#' @noRd
.raw.sib.reformat.output <- function(x, time, new_epochsize = 5, current_epochsize = 60) {
  x <- rep(x, each = (current_epochsize / new_epochsize))
  if (length(x) < length(time)) {
    x <- c(x, rep(0, length(time) - length(x)))
  } else if (length(x) > length(time)) {
    x <- x[1:length(time)]
  }
  sib_classification <- matrix(0, length(x), 1)
  sib_classification[, 1] <- as.matrix(x)
  return(sib_classification)
}

#' Which Count Series the Count-Based Algorithms Iterate Over
#'
#' Zero-crossing counts come first when both series are present.
#'
#' @param zeroCrossingCount,NeishabouriCount The two count series, either empty.
#' @return A character vector, possibly of length 0.
#' @keywords internal
#' @noRd
.raw.sib.count.types <- function(zeroCrossingCount, NeishabouriCount) {
  count_types <- c()
  if (length(zeroCrossingCount) > 0) count_types <- "zeroCrossingCount"
  if (length(NeishabouriCount) > 0) count_types <- c(count_types, "NeishabouriCount")
  return(count_types)
}

#' Sustained Inactivity From the Z-Angle (GGIR vanHees2015)
#'
#' The epochs before a posture change are \code{which(abs(diff(anglez)) > anglethreshold)},
#' and every gap between consecutive posture changes longer than \code{timethreshold}
#' minutes is filled with 1 from the change that opens it to the one that closes it, both
#' inclusive. The gate is a strict \code{>}, so the shortest sib at the defaults is 62
#' epochs (310 s); adjacent qualifying gaps merge into one run; and when no gap qualifies,
#' fewer than 10 posture changes marks the whole recording a sib and ten or more marks none
#' of it.
#'
#' @param timethreshold Numeric vector, minutes without a posture change.
#' @param anglethreshold Numeric vector, degrees of z-angle change.
#' @param time The time vector of the recording.
#' @param anglez The 5-second z-angle, NA already replaced by 0 by the caller.
#' @param epochsize Short epoch length in seconds.
#' @param Nvalues Number of rows of the returned object.
#' @return A data.frame with \code{Nvalues} rows and one column per threshold pair.
#' @keywords internal
#' @noRd
.raw.sib.vanhees2015 <- function(timethreshold, anglethreshold, time, anglez,
                                 epochsize, Nvalues) {
  cnt <- 1
  Ndefs <- length(timethreshold) * length(anglethreshold)
  sib_classification <- matrix(0, Nvalues, Ndefs)
  for (i in timethreshold) {
    for (j in anglethreshold) {
      sdl1 <- rep(0, length(time))
      postch <- which(abs(diff(anglez)) > j) # posture change of at least j degrees
      q1 <- c()
      if (length(postch) > 1) {
        q1 <- which(diff(postch) > (i * (60 / epochsize))) # less than once per i minutes
      }
      if (length(q1) > 0) {
        for (gi in 1:length(q1)) {
          sdl1[postch[q1[gi]]:postch[q1[gi] + 1]] <- 1 # periods with no posture change
        }
      } else {
        if (length(postch) < 10) { # possibly a day without wearing
          sdl1[1:length(sdl1)] <- 1
        } else { # possibly a day with constant posture changes
          sdl1[1:length(sdl1)] <- 0
        }
      }
      sib_classification[, cnt] <- sdl1
      cnt <- cnt + 1
    }
  }
  cnt <- 1
  sib_classification <- as.data.frame(sib_classification, stringsAsFactors = TRUE)
  for (i in timethreshold) {
    for (j in anglethreshold) {
      colnames(sib_classification)[cnt] <- paste("T", i, "A", j, sep = "")
      cnt <- cnt + 1
    }
  }
  return(sib_classification)
}

#' Sustained Inactivity From Counts (GGIR Sadeh1994)
#'
#' Counts are summed per minute, an 11-minute centred window gives the four Sadeh features
#' and \code{PS = 7.601 - 0.065*MeanW5 - 1.08*NAT - 0.056*SDlast - 0.703*LOGact}, with
#' sleep where \code{PS >= 0}.
#'
#' @details Kept as GGIR has it: \code{MeanW5} is the mean of all eleven window minutes;
#'   \code{SDlast = sd(x[6:11])} is six values; \code{NAT} uses the strict bounds
#'   \code{> 50 & < 100} where the package's own \code{sleep.sadeh} uses \code{>= 50}; the
#'   300 cap applies to Neishabouri counts and not to zero-crossing counts; and the
#'   \code{c(rep(-2, 5), PS)} prefix shifts the whole score series five minutes later,
#'   because the edge clamping in \code{.raw.sib.rollfun.mat} already scored every minute.
#'
#' @param time The time vector of the recording.
#' @param zeroCrossingCount,NeishabouriCount The two count series, either empty.
#' @param epochsize Short epoch length in seconds.
#' @param Nvalues Number of rows of the returned object.
#' @param HASIB.algo Name used to build the column names.
#' @return A data.frame with \code{Nvalues} rows and one column per count series.
#' @keywords internal
#' @noRd
.raw.sib.sadeh1994 <- function(time, zeroCrossingCount, NeishabouriCount,
                               epochsize, Nvalues, HASIB.algo = "Sadeh1994") {
  count_types <- .raw.sib.count.types(zeroCrossingCount, NeishabouriCount)
  sib_classification <- as.data.frame(matrix(0, Nvalues, length(count_types)))
  cti <- 1
  for (count_type in count_types) {
    if (count_type == "zeroCrossingCount") {
      Countpermin <- .raw.sib.sum.per.window(zeroCrossingCount, epochsize = epochsize,
                                             summingwindow = 60)
    } else if (count_type == "NeishabouriCount") {
      Countpermin <- .raw.sib.sum.per.window(NeishabouriCount, epochsize = epochsize,
                                             summingwindow = 60)
    }
    if (count_type != "zeroCrossingCount") {
      # because this is what ActiLife does for their counts
      Countpermin <- ifelse(test = Countpermin > 300, yes = 300, no = Countpermin)
    }
    Countpermin_matrix <- .raw.sib.rollfun.mat(Countpermin, Ncol = 11)
    CalcSadehFT <- function(x) {
      MeanW5 <- mean(x, na.rm = TRUE)
      SDlast <- stats::sd(x[6:11]) # last five in this matrix means columns 6:11
      NAT <- length(which(x > 50 & x < 100))
      LOGact <- log(x[6] + 1)
      return(data.frame(MeanW5 = MeanW5, SDlast = SDlast,
                        NAT = NAT, LOGact = LOGact))
    }
    SadehFT1 <- apply(X = Countpermin_matrix, MARGIN = 1, FUN = CalcSadehFT)
    rm(Countpermin_matrix)
    SadehFT <- data.frame(matrix(unlist(SadehFT1), nrow = length(SadehFT1), byrow = TRUE))
    rm(SadehFT1)
    colnames(SadehFT) <- c("MeanW5", "SDlast", "NAT", "LOGact")
    PS <- 7.601 - (0.065 * SadehFT$MeanW5) - (1.08 * SadehFT$NAT) - (0.056 * SadehFT$SDlast) - (0.703 * SadehFT$LOGact)
    PS <- c(rep(-2, 5), PS) # GGIR's prefix; shifts the score series five minutes later

    PSscores <- rep(0, length(PS))
    PSsibs <- which(PS >= 0) # sleep
    if (length(PSsibs) > 0) {
      PSscores[PSsibs] <- 1
    }
    sib_classification[, cti] <- .raw.sib.reformat.output(x = PSscores, time,
                                                          new_epochsize = epochsize,
                                                          current_epochsize = 60)
    if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_ZC")
    if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_Neishabouri")
    cti <- cti + 1
  }
  return(sib_classification)
}

#' Sustained Inactivity From Counts (GGIR ColeKripke1992)
#'
#' Counts are summed per minute and divided by 6 (GGIR's reading of counts per 10 seconds),
#' a 7-minute centred window is weighted with \code{c(67, 74, 230, 76, 58, 54, 106)} and
#' sleep is where \code{0.001 * weighted sum < 1}. Cole's re-scoring rules are not
#' implemented, as in GGIR. Because the window columns run future to past, the weight 230
#' lands one minute in the future, and \code{c(rep(2, 4), PS)} shifts the whole score series
#' four minutes later. The \code{aggwindow != 60} rescale is dead code in this branch.
#'
#' @param time The time vector of the recording.
#' @param zeroCrossingCount,NeishabouriCount The two count series, either empty.
#' @param epochsize Short epoch length in seconds.
#' @param Nvalues Number of rows of the returned object.
#' @param HASIB.algo Name used to build the column names.
#' @return A data.frame with \code{Nvalues} rows and one column per count series.
#' @keywords internal
#' @noRd
.raw.sib.colekripke1992 <- function(time, zeroCrossingCount, NeishabouriCount,
                                    epochsize, Nvalues, HASIB.algo = "ColeKripke1992") {
  count_types <- .raw.sib.count.types(zeroCrossingCount, NeishabouriCount)
  sib_classification <- as.data.frame(matrix(0, Nvalues, length(count_types)))

  if (epochsize <= 60) {
    aggwindow <- 60
  } else if (epochsize > 60) {
    stop("Cole Kripke algorithm is not designed for epochs larger than 1 minute")
  }
  cti <- 1
  for (count_type in count_types) {
    # Page 6 of Cole and Kripke (1992), doi:10.1093/sleep/15.5.461, non-overlapping
    # 1 minute epochs
    if (count_type == "zeroCrossingCount") {
      CountperWindow <- .raw.sib.sum.per.window(zeroCrossingCount, epochsize = epochsize,
                                                summingwindow = aggwindow)
    } else if (count_type == "NeishabouriCount") {
      CountperWindow <- .raw.sib.sum.per.window(NeishabouriCount, epochsize = epochsize,
                                                summingwindow = aggwindow)
    }
    if (aggwindow != 60) CountperWindow <- CountperWindow * (60 / aggwindow)
    # counts per 10 seconds, GGIR's reading of the paper
    CountperWindow <- CountperWindow / 6
    CountperWindow_matrix <- .raw.sib.rollfun.mat(CountperWindow, Ncol = 7)
    CKweights <- c(67, 74, 230, 76, 58, 54, 106) # reversed to match order of matrix
    PS <- 0.001 * (CountperWindow_matrix %*% CKweights)
    PS <- c(rep(2, 4), PS) # GGIR's prefix; shifts the score series four minutes later
    # No rescoring as described by Cole 1992: the reported accuracy gain is marginal
    PSscores <- rep(0, length(PS))
    PSsibs <- which(PS < 1) # sleep
    if (length(PSsibs) > 0) {
      PSscores[PSsibs] <- 1
    }
    sib_classification[, cti] <- .raw.sib.reformat.output(x = PSscores, time,
                                                          new_epochsize = epochsize,
                                                          current_epochsize = 60)
    if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_ZC")
    if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_Neishabouri")
    cti <- cti + 1
  }
  return(sib_classification)
}

#' Sustained Inactivity From Counts (GGIR Galland2012)
#'
#' Counts are summed per minute and scaled by the mean of the non-zero minutes of the whole
#' recording, so the algorithm is neither causal nor window-local. A 7-minute centred window
#' is weighted with \code{c(1, 3, 5:1)}, the absolute value is multiplied by 2.7, and sleep
#' is where the result is \code{< 1}. \code{WeightCounts[-c(1:2)]} shifts the series two
#' minutes earlier and the last two minutes are padded awake. An all-zero count series gives
#' a NaN scale and scores the whole recording awake with no warning, as in GGIR.
#'
#' @param time The time vector of the recording.
#' @param zeroCrossingCount,NeishabouriCount The two count series, either empty.
#' @param epochsize Short epoch length in seconds.
#' @param Nvalues Number of rows of the returned object.
#' @param HASIB.algo Name used to build the column names.
#' @return A data.frame with \code{Nvalues} rows and one column per count series.
#' @keywords internal
#' @noRd
.raw.sib.galland2012 <- function(time, zeroCrossingCount, NeishabouriCount,
                                 epochsize, Nvalues, HASIB.algo = "Galland2012") {
  count_types <- .raw.sib.count.types(zeroCrossingCount, NeishabouriCount)
  sib_classification <- as.data.frame(matrix(0, Nvalues, length(count_types)))
  cti <- 1
  for (count_type in count_types) {
    if (count_type == "zeroCrossingCount") {
      Countpermin <- .raw.sib.sum.per.window(zeroCrossingCount, epochsize = epochsize,
                                             summingwindow = 60)
    } else if (count_type == "NeishabouriCount") {
      Countpermin <- .raw.sib.sum.per.window(NeishabouriCount, epochsize = epochsize,
                                             summingwindow = 60)
    }
    mean_nonzero <- mean(Countpermin[which(Countpermin != 0)])
    CountScaled <- Countpermin / mean_nonzero
    CountScaled_matrix <- .raw.sib.rollfun.mat(CountScaled, Ncol = 7)
    # kernel reversed relative to the paper because the matrix columns run future to past
    WeightCounts <- abs(CountScaled_matrix %*% c(1, 3, 5:1)) * 2.7
    WeightCounts <- WeightCounts[-c(1:2)] # remove first two time stamps, to align with 5th element (7-5=2)
    GallandScore <- rep(0, length(WeightCounts))
    GallandScore[which(WeightCounts < 1)] <- 1
    sib_classification[, cti] <- .raw.sib.reformat.output(x = GallandScore, time,
                                                          new_epochsize = epochsize,
                                                          current_epochsize = 60)
    if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_ZC")
    if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_Neishabouri")
    cti <- cti + 1
  }
  return(sib_classification)
}

#' Sustained Inactivity From Counts (GGIR Oakley1997)
#'
#' Counts are aggregated to 15, 30 or 60 seconds, a symmetric kernel of 13, 9 or 5 windows
#' is applied, and sleep is where the weighted sum is \code{<= oakley_threshold} (GGIR
#' default 20). There is no time shift. Kept as GGIR has it: when \code{epochsize} is
#' already 15, 30 or 60 the branch assigns \code{counts <- zeroCrossingCount} for both count
#' types, so the Neishabouri column duplicates the zero-crossing one, and an
#' \code{aggwindow} other than 15, 30 or 60 leaves \code{oakley_coef} undefined.
#'
#' @param time The time vector of the recording.
#' @param zeroCrossingCount,NeishabouriCount The two count series, either empty.
#' @param epochsize Short epoch length in seconds.
#' @param Nvalues Number of rows of the returned object.
#' @param oakley_threshold The decision threshold, absolute.
#' @param HASIB.algo Name used to build the column names.
#' @return A data.frame with \code{Nvalues} rows and one column per count series.
#' @keywords internal
#' @noRd
.raw.sib.oakley1997 <- function(time, zeroCrossingCount, NeishabouriCount,
                                epochsize, Nvalues, oakley_threshold,
                                HASIB.algo = "Oakley1997") {
  # Information bulletin no.3, sleep algorithms, Cambridge Neurotechnologies
  if (epochsize > 60) {
    stop("GGIR only facilitates the Oakley algorithm for epoch sizes up to 1 minute.", call. = FALSE)
  }
  count_types <- .raw.sib.count.types(zeroCrossingCount, NeishabouriCount)
  sib_classification <- as.data.frame(matrix(0, Nvalues, length(count_types)))
  cti <- 1
  for (count_type in count_types) {
    if (epochsize %in% c(15, 30, 60) == FALSE) {
      if (epochsize < 15 && 15 %% epochsize == 0) {
        aggwindow <- 15
      } else if (epochsize < 30 && 30 %% epochsize == 0) {
        aggwindow <- 30
      } else {
        aggwindow <- 60
      }
      if (count_type == "zeroCrossingCount") {
        counts <- .raw.sib.sum.per.window(zeroCrossingCount, epochsize = epochsize,
                                          summingwindow = aggwindow)
      } else if (count_type == "NeishabouriCount") {
        counts <- .raw.sib.sum.per.window(NeishabouriCount, epochsize = epochsize,
                                          summingwindow = aggwindow)
      }
    } else {
      counts <- zeroCrossingCount
      aggwindow <- epochsize
    }
    # Oakley coefficients are epoch size specific
    if (aggwindow == 15) {
      oakley_coef <- c(rep(0.04, 4), rep(0.20, 4), 4, rep(0.20, 4), rep(0.04, 4))
    } else if (aggwindow == 30) {
      oakley_coef <- c(0.04, 0.04, 0.20, 0.20, 2.0, 0.2, 0.2, 0.04, 0.04)
    } else if (aggwindow == 60) {
      oakley_coef <- c(0.04, 0.2, 1, 0.20, 0.04)
    }
    Noak <- length(oakley_coef)
    counts_matrix <- .raw.sib.rollfun.mat(counts, Ncol = Noak)
    PS <- counts_matrix %*% oakley_coef
    rm(counts_matrix)
    PSscores <- rep(0, length(PS))
    PSsibs <- which(PS <= oakley_threshold) # sleep
    if (length(PSsibs) > 0) {
      PSscores[PSsibs] <- 1
    }
    sib_classification[, cti] <- .raw.sib.reformat.output(x = PSscores, time,
                                                          new_epochsize = epochsize,
                                                          current_epochsize = aggwindow)
    if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_ZC")
    if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] <- paste0(HASIB.algo, "_Neishabouri")
    cti <- cti + 1
  }
  return(sib_classification)
}

#' Sustained Inactivity Where the Sensor Is Not Worn at Night (GGIR NotWorn)
#'
#' A 300-second centred rolling maximum of the acceleration metric, thresholded at the
#' standard deviation of its non-zero values times 0.05, with a fallback to their 10th
#' percentile when that threshold falls below the minimum of the unsmoothed series.
#' \code{rollmax(..., fill = 1)} writes a literal 1 into the first and last 150 s, which for
#' a metric in g forces the edges awake, as in GGIR.
#'
#' @param activity The acc.metric series (ENMO by default).
#' @param epochsize Short epoch length in seconds.
#' @param Nvalues Number of rows of the returned object.
#' @return A data.frame with \code{Nvalues} rows and a single column named NotWorn.
#' @keywords internal
#' @noRd
.raw.sib.notworn <- function(activity, epochsize, Nvalues) {
  # the sensor is not worn at night, but the GGIR framework still needs an estimate
  sib_classification <- as.data.frame(matrix(0, Nvalues, 1))
  activity2 <- zoo::rollmax(x = activity, k = 300 / epochsize, fill = 1)
  # ignore zeros because in ActiGraph with many zeros it skews the distribution
  nonzero <- which(activity2 != 0)
  if (length(nonzero) > 0) {
    activityThreshold <- stats::sd(activity2[nonzero], na.rm = TRUE) * 0.05
    if (activityThreshold < min(activity)) {
      activityThreshold <- stats::quantile(activity2[nonzero], probs = 0.1)
    }
  } else {
    activityThreshold <- 0
  }
  zeroMovement <- which(activity2 <= activityThreshold)
  if (length(zeroMovement) > 0) {
    sib_classification[zeroMovement, 1] <- 1
  }
  colnames(sib_classification) <- "NotWorn"
  return(sib_classification)
}

#' Detect Sustained Inactivity Bouts (GGIR HASIB)
#'
#' Classifies every short epoch of a recording as sustained inactivity (1) or not (0), with
#' one column per definition. This is stage 2 of GGIR part 3 and runs once over the whole
#' recording. Six algorithms: \code{"vanHees2015"} (the default, on the z-angle),
#' \code{"NotWorn"} (on the acceleration metric) and the four count-based ones
#' \code{"Sadeh1994"}, \code{"ColeKripke1992"}, \code{"Galland2012"} and
#' \code{"Oakley1997"}, which GGIR's vignette marks experimental.
#'
#' @details The series must come from the part-2 imputed short-epoch table, not part 1's.
#'   Each count-based algorithm carries GGIR's built-in time shift: Sadeh 5 minutes later,
#'   Cole-Kripke 4 minutes later, Galland 2 minutes earlier with a 2-minute awake pad at the
#'   end. Oakley and the two acceleration-based algorithms are not shifted. The one deviation
#'   from GGIR is that an unrecognised algorithm raises a \code{canhrActi_raw_sleep_error}
#'   listing the valid values instead of failing on an undefined object.
#'
#' @param HASIB.algo One of "vanHees2015", "Sadeh1994", "ColeKripke1992", "Galland2012",
#'   "Oakley1997", "NotWorn".
#' @param timethreshold Numeric vector, minutes without a posture change (vanHees2015 only).
#' @param anglethreshold Numeric vector, degrees of z-angle change (vanHees2015 only).
#' @param time The time vector of the recording; only its length is used.
#' @param anglez The imputed 5-second z-angle, NA already replaced by 0 by the caller.
#' @param ws3 Short epoch length in seconds.
#' @param zeroCrossingCount,NeishabouriCount The two count series for the count-based
#'   algorithms, either empty. Zero-crossing counts are scaled by \code{zc.scale} by the
#'   caller; Neishabouri counts are not.
#' @param activity The acc.metric series (ENMO by default), used by NotWorn.
#' @param oakley_threshold The Oakley1997 decision threshold, absolute (GGIR default 20).
#' @return A data.frame with \code{Nvalues} rows and one numeric 0/1 column per definition.
#'   Column names are \code{paste0("T", timethreshold, "A", anglethreshold)} for vanHees2015,
#'   \code{paste0(HASIB.algo, "_ZC")} and \code{paste0(HASIB.algo, "_Neishabouri")} for the
#'   count algorithms, and \code{"NotWorn"} for NotWorn.
#' @export
raw.sib <- function(HASIB.algo = "vanHees2015", timethreshold = c(), anglethreshold = c(),
                    time = c(), anglez = c(), ws3 = c(),
                    zeroCrossingCount = c(), NeishabouriCount = c(),
                    activity = NULL, oakley_threshold = NULL) {
  epochsize <- ws3 # epochsize in seconds
  Nvalues <- max(length(anglez), length(zeroCrossingCount), length(NeishabouriCount),
                 length(activity))
  if (length(HASIB.algo) != 1 || is.na(HASIB.algo) ||
      !(HASIB.algo %in% .raw.sib.algorithms)) {
    .raw.sib.stop(HASIB.algo)
  }
  if (HASIB.algo == "vanHees2015") {
    sib_classification <- .raw.sib.vanhees2015(timethreshold = timethreshold,
                                               anglethreshold = anglethreshold,
                                               time = time, anglez = anglez,
                                               epochsize = epochsize, Nvalues = Nvalues)
  } else if (HASIB.algo == "Sadeh1994") {
    sib_classification <- .raw.sib.sadeh1994(time = time,
                                             zeroCrossingCount = zeroCrossingCount,
                                             NeishabouriCount = NeishabouriCount,
                                             epochsize = epochsize, Nvalues = Nvalues,
                                             HASIB.algo = HASIB.algo)
  } else if (HASIB.algo == "ColeKripke1992") {
    sib_classification <- .raw.sib.colekripke1992(time = time,
                                                  zeroCrossingCount = zeroCrossingCount,
                                                  NeishabouriCount = NeishabouriCount,
                                                  epochsize = epochsize, Nvalues = Nvalues,
                                                  HASIB.algo = HASIB.algo)
  } else if (HASIB.algo == "Galland2012") {
    sib_classification <- .raw.sib.galland2012(time = time,
                                               zeroCrossingCount = zeroCrossingCount,
                                               NeishabouriCount = NeishabouriCount,
                                               epochsize = epochsize, Nvalues = Nvalues,
                                               HASIB.algo = HASIB.algo)
  } else if (HASIB.algo == "Oakley1997") {
    sib_classification <- .raw.sib.oakley1997(time = time,
                                              zeroCrossingCount = zeroCrossingCount,
                                              NeishabouriCount = NeishabouriCount,
                                              epochsize = epochsize, Nvalues = Nvalues,
                                              oakley_threshold = oakley_threshold,
                                              HASIB.algo = HASIB.algo)
  } else if (HASIB.algo == "NotWorn") {
    sib_classification <- .raw.sib.notworn(activity = activity, epochsize = epochsize,
                                           Nvalues = Nvalues)
  }
  return(sib_classification)
}

#' The Named Error That Replaces GGIR's Missing Else
#'
#' @param HASIB.algo The value that was not recognised.
#' @keywords internal
#' @noRd
.raw.sib.stop <- function(HASIB.algo) {
  shown <- if (length(HASIB.algo) == 1 && !is.na(HASIB.algo)) {
    paste0('"', HASIB.algo, '"')
  } else {
    paste0("a value of length ", length(HASIB.algo))
  }
  stop(structure(class = c("canhrActi_raw_sleep_error", "error", "condition"),
                 list(message = paste0("unknown HASIB.algo: ", shown,
                                       ". Valid values are ",
                                       paste0('"', .raw.sib.algorithms, '"', collapse = ", "),
                                       "."),
                      call = NULL, HASIB.algo = HASIB.algo)))
}
