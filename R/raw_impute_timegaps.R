# Ported from GGIR 3.3-9 R/g.imputeTimegaps.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the nested imputeRaw is
# .raw.impute.raw, with its timestamp loop replaced by one vectorised expression that
# gives the same doubles; argument and return names follow canhrActi; the long-gap
# alignment takes hour, minute and truncated second through as.POSIXlt in
# .raw.clock.seconds instead of data.table. Every other expression, its operand order and
# the order of the steps are GGIR's.

#' Seconds Since Midnight of a Timestamp
#'
#' Reproduces \code{data.table::hour(t) * 3600 + minute(t) * 60 + second(t)}, which
#' g.imputeTimegaps uses to align a long gap to the long-epoch grid, without data.table.
#' \code{as.POSIXlt(t)} is called with \code{tz} missing, so numeric Unix seconds (the
#' pipeline case) are read in the system timezone, as data.table does on R >= 4.3, and
#' a POSIXct keeps its own tzone. The second is truncated, as data.table::second does;
#' a gt3x sample at hh:mm:25.9667 counts as second 25, and the raw fill length depends
#' on it.
#'
#' @param t A numeric Unix time or a POSIXct; a vector is handled elementwise.
#' @return Numeric seconds since local midnight, without the "+ 1" the caller adds.
#' @keywords internal
#' @noRd
.raw.clock.seconds <- function(t) {
  lt <- as.POSIXlt(t)
  lt$hour * 60^2 + lt$min * 60 + as.integer(lt$sec)
}

#' Replicate Gap Rows and Regenerate Their Timestamps
#'
#' GGIR's imputeRaw: every row of \code{x} is repeated \code{x$gap} times (all columns),
#' the replicated rows of a gap get timestamps \code{t_gap + (0:(gap-1))/sf}, rows with
#' \code{gap == 1} keep their timestamp, and the \code{gap} column is removed.
#'
#' @details GGIR grows the new time vector with \code{c()} inside a loop over the gaps,
#'   which costs O(gaps x samples). Here \code{rep(time, gap) + (sequence(gap) - 1L) *
#'   (1/sf)} gives bit-identical doubles in one allocation, because \code{seq(0, by = b,
#'   length.out = m)} is evaluated as \code{0 + (0:(m - 1)) * b}. Two quirks of GGIR's
#'   index arithmetic cannot be reproduced by the vectorised form (adjacent gaps give a
#'   descending two-element range, a trailing gap an NA plus a duplicate), and
#'   \code{c()} dispatches on a POSIXct time; in those cases
#'   \code{.raw.impute.raw.pieces} keeps the original expressions.
#'
#' @param x A data.frame with at least the columns \code{time} and \code{gap}
#'   (\code{gap} is 1 for an ordinary row and the number of samples to emit for the row
#'   before a gap).
#' @param sf Sample frequency in Hz.
#' @return The replicated data.frame without the \code{gap} column.
#' @keywords internal
#' @noRd
.raw.impute.raw <- function(x, sf) {
  gapp <- which(x$gap != 1)
  n <- length(x$time)
  if (length(gapp) > 0) {
    # cases the vectorised form cannot reproduce; see Details
    quirk <- is.object(x$time) || any(diff(gapp) == 1L) || gapp[length(gapp)] == n
    if (quirk) return(.raw.impute.raw.pieces(x, sf))
    gap <- x$gap
    newTime <- rep(x$time, gap) + (sequence(gap) - 1L) * (1/sf)
  }
  x <- as.data.frame(lapply(x, rep, x$gap))

  if (length(gapp) > 0) {
    x$time <- newTime[1:nrow(x)]
  }
  x <- x[, which(colnames(x) != "gap")]
  return(x)
}

#' The Original imputeRaw Loop, Joined Once
#'
#' Each piece is computed with GGIR's original expression in the original order, so the
#' two index quirks reproduce exactly; the pieces are joined once instead of grown with
#' \code{c()}.
#' @keywords internal
#' @noRd
.raw.impute.raw.pieces <- function(x, sf) {
  gapp <- which(x$gap != 1)
  if (length(gapp) > 0) {
    ng <- length(gapp)
    pieces <- vector("list", 2L * ng + 1L)
    if (gapp[1] > 1) {
      pieces[[1L]] <- x$time[1:(gapp[1] - 1)]
    }
    for (g in 1:ng) {
      pieces[[2L * g]] <- x$time[gapp[g]] + seq(0, by = 1/sf, length.out = x$gap[gapp[g]])
      if (g < ng) {
        pieces[[2L * g + 1L]] <- x$time[(gapp[g] + 1):(gapp[g + 1] - 1)]
      }
    }
    pieces[[2L * ng + 1L]] <- x$time[(gapp[g] + 1):length(x$time)]
    if (is.object(x$time)) {
      # c() dispatches on the first piece, as in the loop: an empty first piece (a gap on
      # row 1) selects the default method, so the time comes back numeric, as in GGIR
      newTime <- do.call(c, pieces)
    } else {
      newTime <- unlist(pieces, recursive = FALSE, use.names = FALSE)
    }
  }
  x <- as.data.frame(lapply(x, rep, x$gap))
  if (length(gapp) > 0) {
    x$time <- newTime[1:nrow(x)]
  }
  x <- x[, which(colnames(x) != "gap")]
  return(x)
}

#' Impute Time Gaps and All-Zero Samples in a Block of Raw Acceleration
#'
#' GGIR's g.imputeTimegaps: fills gaps in the timestamp sequence of one block by last
#' observation carried forward, normalised to 1 g when the carried sample is more than
#' 5 mg above 1 g, and logs what was done. Gaps longer than
#' \code{max(6 * long epoch, 90 min)} are only filled at raw level up to the next
#' long-epoch boundary and from the boundary before the resume time; the middle is
#' handed to the epoch level through a \code{remaining_epochs} column.
#'
#' @details
#' In GGIR's order: without a \code{time} column a synthetic \code{Sys.time()} axis is
#' added and removed at the end; all-zero rows are deleted, except that a zero first
#' row takes \code{previous_last_value} and a zero last row is kept and overwritten
#' with the row before it at the end; \code{k} is floored at \code{2/sf}; when the
#' block starts \code{>= k} seconds after \code{previous_last_time} a synthetic first
#' row at that time is prepended, so the between-block fill holds one duplicate sample
#' (GGIR's own test encodes it); a gap is any \code{diff(time) >= k} and its size is
#' \code{round(deltatime * sf)}; the row before a gap is rescaled to unit norm only when
#' its norm exceeds 1.005; GapsLength is summed before any long-gap shortening. With
#' \code{impute = FALSE} (the g.calibrate call) zero rows are removed and nothing is
#' filled, and the last row is still overwritten after a trailing zero row was removed.
#' In the QClog, \code{end} is \code{start + nrow(x)}, a sample count added to seconds,
#' kept so the log equals GGIR's; it is not a time. A run of fewer than
#' \code{ceiling(k * sf) - 1} lost samples is deleted and never refilled; GGIR epochs by
#' sample count from here on.
#'
#' @param x A data.frame with numeric columns \code{x}, \code{y}, \code{z} in g and
#'   optionally \code{time} (numeric Unix seconds or POSIXct); further columns are
#'   carried through and replicated with the gap rows.
#' @param sf Sample frequency in Hz.
#' @param k Minimum gap length in seconds to impute (GGIR's callers use 0.25); floored
#'   at \code{2/sf}.
#' @param impute FALSE reproduces the g.calibrate call, which only removes all-zero rows.
#' @param previous_last_value The last x, y, z of the previous block's imputed output;
#'   \code{c(0, 0, 1)} for the first block.
#' @param previous_last_time The last time of the previous block's imputed output, or
#'   NULL for the first block.
#' @param epochsize \code{c(short epoch, long epoch)} in seconds (GGIR passes
#'   \code{c(ws3, ws2)}), or NULL to fill every gap at raw level.
#' @return A list with \code{x}, the imputed data.frame (without \code{time} when none
#'   was supplied, plus \code{remaining_epochs} when a gap above the limit was found),
#'   and \code{qclog}, a one-row data.frame (imputed, start, end, blockLengthSeconds,
#'   timegaps_n, timegaps_min) built as GGIR's QClog.
#' @keywords internal
#' @noRd
.raw.impute.timegaps <- function(x, sf, k = 0.25, impute = TRUE,
                                 previous_last_value = c(0, 0, 1),
                                 previous_last_time = NULL,
                                 epochsize = NULL) {
  PreviousLastValue <- previous_last_value
  PreviousLastTime <- previous_last_time
  if (!is.null(epochsize)) {
    shortEpochSize <- epochsize[1]
    longEpochSize <- epochsize[2]
  }
  remove_time_at_end <- FirstRowZeros <- imputelast <- FALSE
  NumberOfGaps <- GapsLength <- 0

  if (!("time" %in% colnames(x))) {
    x$time <- seq(from = Sys.time(), by = 1/sf, length.out = nrow(x))
    remove_time_at_end <- TRUE
  }

  xyzCol <- which(colnames(x) %in% c("x", "y", "z"))

  # remove all-zero rows
  zeros <- which(x$x == 0 & x$y == 0 & x$z == 0)
  if (length(zeros) > 0) {
    # a zero first row takes the previous block's last value
    if (zeros[1] == 1) {
      zeros <- zeros[-1]
      x[1, xyzCol] <- PreviousLastValue
      FirstRowZeros <- TRUE
    }
    # a zero last row is kept for its time and imputed at the end
    if (length(zeros) > 0) {
      if (zeros[length(zeros)] == nrow(x)) {
        zeros <- zeros[-length(zeros)]
        imputelast <- TRUE
      }
    }
    if (length(zeros) > 0) x <- x[-zeros,]
  }
  if (impute == TRUE) { # FALSE in g.calibrate
    if (k < 2/sf) { # at least 2 samples
      k <- 2/sf
    }
    deltatime <- diff(x$time)
    if (!is.numeric(deltatime)) {
      units(deltatime) <- "secs"
      deltatime <- as.numeric(deltatime)
    }
    # gap between this block and the previous one
    if (!is.null(PreviousLastTime)) {
      if (!inherits(x = PreviousLastTime, what = "numeric") &&
          inherits(x = x$time[1], what = "numeric")) {
        PreviousLastTime <- as.numeric(PreviousLastTime)
      }
      first_deltatime <- diff(c(PreviousLastTime, x$time[1]))
      if (!is.numeric(first_deltatime)) {
        units(first_deltatime) <- "secs"
        first_deltatime <- as.numeric(first_deltatime)
      }
      if (first_deltatime >= k) {
        x <- rbind(x[1,], x)
        x$time[1] <- PreviousLastTime
        x[1, xyzCol] <- PreviousLastValue
        deltatime <- c(first_deltatime, deltatime)
      }
    }
    gapsi <- which(deltatime >= k)
    NumberOfGaps <- length(gapsi)
    if (NumberOfGaps > 0) {
      x$gap <- 1
      x$gap[gapsi] <- round(deltatime[gapsi] * sf) # round, not as.integer: near-whole values lost a row
      GapsLength <- sum(x$gap[gapsi])
      # normalise the carried sample to 1 g
      normalise <- which(x$gap > 1)
      en_lastknownvalue <- sqrt(rowSums(x[normalise, xyzCol]^2))
      # one-sided: only norms above 1.005 are rescaled
      good_to_normalise <- which((abs(en_lastknownvalue) - 1) > 0.005)
      if (length(good_to_normalise) > 0) {
        x[normalise[good_to_normalise], xyzCol] <- x[normalise[good_to_normalise], xyzCol] / en_lastknownvalue[good_to_normalise]
      }
      imputation_done <- FALSE
      if (!is.null(epochsize)) {
        # gaps above 6 long epochs or 90 min, whichever is larger
        GapLimit <-  max(c(((longEpochSize / 60) * 6), 90)) * 60 * sf
        gap90 <- ifelse(x$gap > GapLimit, x$gap, 1)
        gap90i <- which(gap90 > 1)
        if (length(gap90i) > 0) {
          # fill only up to the long-epoch cut; the rest goes to remaining_epochs
          x$remaining_epochs <- 1
          x$next_epoch_delay <- 0
          longEpochDayCut <- seq(0, 24 * 60^2, by = longEpochSize)
          x$imputation <- 0; imp <- 0
          # GGIR's name, though it is TRUE for R >= 4.3; kept so the timezone of the
          # alignment tracks GGIR on every R version
          Rversion_lt_420 <- as.numeric(R.Version()$major) >= 4 && as.numeric(R.Version()$minor) > 2
          for (i in gap90i) {
            imp <- imp + 1
            if (Rversion_lt_420) {
              time_i <- x$time[i]
              time_ip1 <- x$time[i + 1]
            } else {
              # older R: numeric time needs an origin; GGIR uses GMT
              time_i <- as.POSIXct(x$time[i], origin = "1970-01-01", tz = "GMT")
              time_ip1 <- as.POSIXct(x$time[i + 1], origin = "1970-01-01", tz = "GMT")
            }
            # short epochs up to the next long-epoch cut
            seconds <- .raw.clock.seconds(time_i) + 1
            seconds_from_prevCut <- seconds - max(longEpochDayCut[which(longEpochDayCut <= seconds)])
            shortEpochs2add_1 <- (longEpochSize - seconds_from_prevCut) / shortEpochSize
            # short epochs after the gap
            seconds <- .raw.clock.seconds(time_ip1) + 1
            seconds_from_prevCut <- seconds - max(longEpochDayCut[which(longEpochDayCut <= seconds)])
            shortEpochs2add_2 <- (seconds_from_prevCut - 1) / shortEpochSize
            shortEpochs2add <- shortEpochs2add_1 + shortEpochs2add_2
            time2add <- (shortEpochs2add * sf * shortEpochSize) + 1
            x$remaining_epochs[i] <- ((x$gap[i] - time2add) / (sf * shortEpochSize)) + 1 # plus 1 for the current epoch
            x$gap[i] <- time2add
            x$imputation[i] <- imp
            # a fractional remainder means part of the next epoch is filled now too
            decs <- x$remaining_epochs[i] - floor(x$remaining_epochs[i])
            if (decs > 0) {
              x$next_epoch_delay[i] <- (decs * (sf * shortEpochSize))
              x$gap[i] <- x$gap[i] +  x$next_epoch_delay[i]
              x$remaining_epochs[i] <- x$remaining_epochs[i] - (x$next_epoch_delay[i] / (sf * shortEpochSize))
            }
          }
          x$gap <- round(x$gap)
          x$next_epoch_delay <- round(x$next_epoch_delay)

          x <- .raw.impute.raw(x, sf)

          imputation_done <- TRUE
          # keep only the last record of remaining_epochs per gap
          keep_remaining_epochs <- data.frame(index = which(x$remaining_epochs > 1),
                                              delay = x$next_epoch_delay[which(x$remaining_epochs > 1)],
                                              imp = x$imputation[which(x$remaining_epochs > 1)])
          keep_remaining_epochs$index <- keep_remaining_epochs$index - keep_remaining_epochs$delay
          points2keep <- stats::aggregate(index ~ imp, data = keep_remaining_epochs, FUN = max)
          points2keep <- points2keep$index
          x$remaining_epochs[-c(points2keep)] <- 1
          x <- x[, which(colnames(x) != "next_epoch_delay")]
          x <- x[, which(colnames(x) != "imputation")]
        }
      }
      if (imputation_done == FALSE) {
        x <- .raw.impute.raw(x, sf)
      }
    }
  } else if (impute == FALSE) {
    # the zero first and last rows kept above go too
    if (FirstRowZeros == TRUE) x <- x[-1,]
    if (imputelast == TRUE) x <- x[-nrow(x),]
  }
  if (imputelast) x[nrow(x), xyzCol] <- x[nrow(x) - 1, xyzCol]

  start <- as.numeric(as.POSIXct(x$time[1], origin = "1970-1-1"))
  end <- start + nrow(x)
  imputed <- NumberOfGaps > 0
  QClog <- data.frame(imputed = imputed,
                      start = start, end = end,
                      blockLengthSeconds = (end - start) / sf,
                      timegaps_n = NumberOfGaps, timegaps_min = GapsLength/sf/60)

  if (remove_time_at_end == TRUE) {
    x <- x[, grep(pattern = "time", x = colnames(x), invert = TRUE)]
  }

  return(list(x = x, qclog = QClog))
}
