# Ported from GGIR 3.3-9 R/detect_nonwear_clipping.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: renamed, the unused
# params_rawdata argument dropped, nonwear_approach is approach, and the return list is
# named nonwear, clipping, nmin. The arithmetic, the loop structure, the index
# construction and the operand order are unchanged.

#' Non-Wear and Clipping Scores per Long Epoch
#'
#' GGIR's detect_nonwear_clipping: scores every ws2 epoch of one data chunk for
#' non-wear (0 to 3, the number of axes judged still) and clipping (fraction of samples
#' beyond the dynamic range, maximum over axes). Called once per chunk by the epoch
#' assembly, so the window truncation at the right edge applies at every chunk
#' boundary, as in GGIR.
#'
#' @param data Numeric matrix or data.frame with columns \code{x}, \code{y}, \code{z}
#'   in g, already calibrated. An optional \code{wear} column switches the non-wear
#'   score to three times the majority value of that column in the window.
#' @param windowsizes Numeric vector \code{c(ws3, ws2, ws)} in seconds: short epoch,
#'   long epoch and the non-wear analysis window. GGIR default \code{c(5, 900, 3600)}.
#' @param sf Sample frequency in Hz.
#' @param clipthres Clipping threshold in g (dynamic range minus 0.5 g).
#' @param sdcriter SD criterion in g below which an axis is still (GGIR 0.013).
#' @param racriter Range criterion in g below which an axis is still (GGIR
#'   \code{nonwear_range_threshold / 1000}, 0.15 by default).
#' @param approach "2023" (GGIR default: forward one hour window from the epoch start,
#'   5 Hz subsampled, flags the epoch and the following three) or "2013" (one hour
#'   window centred on the epoch, every sample used, flags only that epoch). Any other
#'   value leaves every score at 0, as in GGIR.
#'
#' @details The number of epochs is \code{floor(nrow(data) / (ws2 * sf))}; rows beyond
#'   the last whole long epoch still take part in the "2023" non-wear windows but never
#'   in clipping. A lone 1 whose two neighbours are both above 1 becomes a 2.
#'
#' @return A list with \code{nonwear} (one score 0 to 3 per long epoch, GGIR's NWav),
#'   \code{clipping} (fraction 0 to 1 per long epoch, unrounded, GGIR's CWav) and
#'   \code{nmin} (the number of long epochs scored).
#' @keywords internal
.raw.nonwear.clipping <- function(data, windowsizes = c(5, 900, 3600), sf = 100,
                                  clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                                  approach = "2023") {
  MediumEpochSize = windowsizes[2] * sf # samples
  LongEpochSize = windowsizes[3] * sf # samples
  NMediumEpochs = floor(nrow(data)/(MediumEpochSize))
  ClipLog = NonwearLog = matrix(0,NMediumEpochs,3)
  ClipLogCollapsed = NonwearLogCollapsed = rep(0, NMediumEpochs)
  minimumEpochCount = ((LongEpochSize/MediumEpochSize)/2) + 1

  if (approach %in% c("2013", "2023")) {
    for (h in 1:NMediumEpochs) {
      clipstart = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) - MediumEpochSize * 0.5
      clipend = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) + MediumEpochSize * 0.5
      if (approach == "2013") {
        NonwearLogflag = h
        if (h <= minimumEpochCount) {
          nwstart = 1
          nwend = LongEpochSize
        } else if (h >= (NMediumEpochs - minimumEpochCount)) {
          nwstart = (NMediumEpochs - minimumEpochCount) * MediumEpochSize
          nwend = NMediumEpochs * MediumEpochSize
        } else if (h > minimumEpochCount & h < (NMediumEpochs - minimumEpochCount)) {
          nwstart = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) - LongEpochSize * 0.5
          nwend = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) + LongEpochSize * 0.5
        }
      } else if (approach == "2023") {
        NonwearLogflag = h:(h + LongEpochSize/MediumEpochSize - 1)
        if (NonwearLogflag[length(NonwearLogflag)] > NMediumEpochs) NonwearLogflag = NonwearLogflag[-which(NonwearLogflag > NMediumEpochs)]
        nwstart = h * MediumEpochSize - MediumEpochSize
        nwend = nwstart + LongEpochSize
        if (nwend > nrow(data)) {
          nwend = nrow(data)
        }
      }
      xyzCol = which(colnames(data) %in% c("x", "y", "z"))
      for (jj in seq(3)) {
        # clipping
        aboveThreshold = which(abs(data[(1 + clipstart):clipend, xyzCol[jj]]) > clipthres)
        ClipLog[h, jj] = length(aboveThreshold)
        if (length(aboveThreshold) > 0) {
          if (length(which(abs(data[c((1 + clipstart):clipend)[aboveThreshold],  xyzCol[jj]]) > clipthres * 1.5)) > 0) {
            ClipLog[h, jj] = MediumEpochSize # a value past 150 percent of the range fails the whole epoch
          }
        }
        # non-wear
        if (approach == "2013") {
          indices = (1 + nwstart):nwend
        } else if (approach == "2023") {
          indices = seq((1 + nwstart), nwend, by = ceiling(sf / 5))
        }
        maxwacc = max(data[indices, xyzCol[jj]], na.rm = TRUE)
        minwacc = min(data[indices, xyzCol[jj]], na.rm = TRUE)
        absrange = abs(maxwacc - minwacc)
        if (absrange < racriter) {
          sdwacc = sd(data[indices, xyzCol[jj]], na.rm = TRUE)
          if (sdwacc < sdcriter) {
            NonwearLog[NonwearLogflag,jj] = 1
          }
        }
      }
      ClipLog = ClipLog / (MediumEpochSize) # fraction of the epoch
      ClipLogCollapsed[h] = max(c(ClipLog[h, 1], ClipLog[h, 2], ClipLog[h, 3]))

      if ("wear" %in% colnames(data)) {
        wearTable = table(data[(1 + nwstart):nwend, "wear"], useNA = "no")
        NonwearLogCollapsed[h] = as.numeric(tail(names(sort(wearTable)), 1)) * 3 # times 3 to match the three-axis score
      }
      if (!("wear" %in% colnames(data))) {
        NonwearLogCollapsed[h] = (NonwearLog[h,1] + NonwearLog[h,2] + NonwearLog[h,3])
      }

    }
  }
  # a lone 1 between values above 1 becomes 2
  ones = which(NonwearLogCollapsed == 1)
  if (length(ones) > 0) {
    for (one_i in ones) {
      if (one_i - 1 < 1) next
      if (one_i + 1 > length(NonwearLogCollapsed)) next
      if (NonwearLogCollapsed[one_i - 1] > 1 & NonwearLogCollapsed[one_i + 1] > 1) NonwearLogCollapsed[one_i] = 2
    }
  }
  return(list(nonwear = NonwearLogCollapsed, clipping = ClipLogCollapsed, nmin = NMediumEpochs))
}
