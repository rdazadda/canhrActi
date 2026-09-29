# Ported from GGIR 3.3-6 R/g.getM5L5.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Renamed; the intensity-gradient and
# qM5L5 branches are not ported, and asking for either is refused by the caller. The
# arithmetic of the four columns, the window loop, the tie-breaking and the +24 hour wrap
# are unchanged.

#' Least and Most Active Window of a Day
#'
#' GGIR's g.getM5L5: the lowest and highest \code{winhr}-hour rolling mean of one day
#' of a metric, and the hour each starts at, as the \code{L5hr}, \code{L5},
#' \code{M5hr} and \code{M5} columns, named for whatever \code{winhr} is set to.
#'
#' @param varnum Numeric vector, one element per short epoch, for one day.
#' @param epochSize Short epoch length in seconds (GGIR's ws3).
#' @param t0_LFMF,t1_LFMF First and last hour of the day to search, as hours from the
#'   start of the window.
#' @param M5L5res Resolution of the search in minutes.
#' @param winhr Window length in hours.
#' @param UnitReScale 1000 for a gravity-unit metric, so the values are mg.
#' @return A one-row data frame with the four columns, named from \code{winhr}.
#' @keywords internal
#' @noRd
.raw.getmx <- function(varnum, epochSize, t0_LFMF, t1_LFMF, M5L5res, winhr,
                       UnitReScale = 1000) {
  meanVarnum <- mean(varnum)
  do.M5L5 <- meanVarnum > 0
  nwindow_f <- (t1_LFMF - winhr) - t0_LFMF
  if (length(do.M5L5) == 0 | is.na(do.M5L5) == TRUE | nwindow_f < 1) do.M5L5 <- FALSE

  if (do.M5L5 == TRUE) {
    reso <- M5L5res
    nwindow_f <- nwindow_f * (60 / reso)
    DAYrunav5 <- matrix(NA, nwindow_f, 1)
    first_hri <- (t0_LFMF * (60 / reso))
    last_hri <- min(nrow(DAYrunav5), floor(((t1_LFMF - winhr) * (60 / reso)) - 1))
    for (hri in first_hri:last_hri) {
      e1 <- (hri * reso * (60 / epochSize)) + 1
      e2 <- (hri + (winhr * (60 / reso))) * reso * (60 / epochSize)
      # GGIR wraps past the end of the day rather than dropping the window
      if (e2 <= length(varnum)) {
        einclude <- e1:e2
      } else {
        einclude <- c(1:(e2 - length(varnum)), e1:length(varnum))
      }
      DAYrunav5[((hri - (t0_LFMF * (60 / reso))) + 1), 1] <- mean(varnum[einclude])
    }
    valid <- which(is.na(DAYrunav5) == F)
    DAYL5HOUR <- ((which(DAYrunav5 == min(DAYrunav5[valid], na.rm = T) &
                           is.na(DAYrunav5) == F) - 1) / (60 / reso)) + t0_LFMF
    DAYL5VALUE <- min(DAYrunav5[valid]) * UnitReScale
    DAYM5HOUR <- ((which(DAYrunav5 == max(DAYrunav5[valid], na.rm = T) &
                           is.na(DAYrunav5) == F) - 1) / (60 / reso)) + t0_LFMF
    DAYM5VALUE <- max(DAYrunav5[valid]) * UnitReScale
    # ties take the upper middle, not the first
    if (length(DAYL5VALUE) > 1) DAYL5VALUE <- sort(DAYL5VALUE)[ceiling(length(DAYL5VALUE) / 2)]
    if (length(DAYL5HOUR) > 1) DAYL5HOUR <- sort(DAYL5HOUR)[ceiling(length(DAYL5HOUR) / 2)]
    if (length(DAYM5VALUE) > 1) DAYM5VALUE <- sort(DAYM5VALUE)[ceiling(length(DAYM5VALUE) / 2)]
    if (length(DAYM5HOUR) > 1) DAYM5HOUR <- sort(DAYM5HOUR)[ceiling(length(DAYM5HOUR) / 2)]
    M5L5vars <- data.frame(DAYL5HOUR = DAYL5HOUR[1], DAYL5VALUE = DAYL5VALUE,
                           DAYM5HOUR = DAYM5HOUR[1], DAYM5VALUE = DAYM5VALUE,
                           stringsAsFactors = TRUE)
  } else {
    M5L5vars <- data.frame(DAYL5HOUR = NA, DAYL5VALUE = NA,
                           DAYM5HOUR = NA, DAYM5VALUE = NA, stringsAsFactors = TRUE)
  }
  ML5N <- c(paste0("L", winhr, "hr"), paste0("L", winhr),
            paste0("M", winhr, "hr"), paste0("M", winhr))
  names(M5L5vars) <- ML5N

  # an L5 starting before noon is reported on the next day's clock
  if (do.M5L5 == TRUE) {
    if (!is.null(M5L5vars[1]) && is.na(M5L5vars[1]) == FALSE && M5L5vars[1] < 12) {
      M5L5vars[1] <- M5L5vars[1] + 24
    }
  }
  M5L5vars
}
