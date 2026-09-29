# Ported from GGIR 3.3-9 R/g.part3_correct_guider.R and R/g.part3_alignIndexVectors.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees et al.; Medical Research Council UK; Accelting; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. g.part3_correct_guider became
# raw.guider.correct and its local helpers became file-level internals taking explicit
# arguments; LC_TIME is forced to "C" for the call. Two GGIR defects (the crash on more
# than one sustained inactivity definition, the silent use of the first of several step 3
# candidates) are kept under ggir_exact = TRUE and reported, never repaired, under
# ggir_exact = FALSE.

#' Reconcile the Min-Max and Median-Median Index Vectors (GGIR g.part3_alignIndexVectors)
#'
#' Makes the median-median start and end vectors (\code{a}, \code{b}) the same length as the
#' min-max ones (\code{x}, \code{y}) when the first or last night of the recording is
#' incomplete. Assumes \code{x} and \code{y} have equal length and \code{y > x} throughout.
#' The block GGIR has commented out (a further trim when \code{a} or \code{b} ends exactly at
#' the end of \code{y}) is not ported.
#'
#' @param x Start indices of the min-max reference window, one per night.
#' @param y End indices of the min-max reference window, one per night.
#' @param a Start indices of the median-median window.
#' @param b End indices of the median-median window.
#' @param N Length of the epoch level time series.
#' @return list(x, y, a, b) with a and b adjusted.
#' @keywords internal
#' @noRd
.raw.align.index.vectors <- function(x, y, a, b, N) {
  # start of a or b is missing
  if (a[1] > y[1]) {
    a <- c(1, a)
  }
  if (b[1] > y[1]) {
    b <- c(1, b)
  }
  # end of a or b is missing and y ends at N
  if (a[length(a)] < x[length(x)] && y[length(y)] == N) {
    a <- c(a, N)
  }
  if (b[length(b)] < x[length(x)] && y[length(y)] == N) {
    b <- c(b, N)
  }
  # end of a or b is past the end of y
  if (a[length(a)] > y[length(y)]) {
    a <- a[1:(length(a) - 1)]
  }
  if (b[length(b)] > y[length(y)]) {
    b <- b[1:(length(b) - 1)]
  }
  # a or b is longer than both y and x
  if (length(a) > length(y) &&
      length(a) > length(x)) {
    a <- a[1:(length(a) - 1)]
  }
  if (length(b) > length(y) &&
      length(b) > length(x)) {
    b <- b[1:(length(b) - 1)]
  }
  return(list(
    x = x,
    y = y,
    a = a,
    b = b
  ))
}

#' Insert a Synthetic Index Across a Forward Clock Change (GGIR correct_for_DST)
#'
#' When the clock moves forward the matching clock time is missing for one day, so the
#' first gap wider than 25 hours gets one synthetic index inserted 24 hours after the index
#' before it. Only the first such gap is repaired, as in GGIR.
#'
#' @param x Integer index vector, sorted.
#' @param epochSize Short epoch length in seconds.
#' @return The index vector, possibly one element longer.
#' @keywords internal
#' @noRd
.raw.guider.correct.dst <- function(x, epochSize) {
  missingday <- which(diff(x) > (25 * 3600 / epochSize))
  if (length(missingday) > 0) {
    missingday <- missingday[1]
    x <- c(x[1:missingday],
           x[missingday] + 24 * 3600 / epochSize,
           x[(missingday + 1):length(x)])
  }
  return(x)
}

#' Indices of Every Epoch Matching the Edges of a Reference Window (GGIR get_matching_indices)
#'
#' Converts a pair of decimal hours to "HH:MM:SS" strings on the epoch grid and returns
#' every index of the recording whose clock time equals each string, one per night. A
#' decimal hour of 24 or more has 24 subtracted once; a 1 is prepended when the first end
#' precedes the first start and \code{length(clocktime)} appended when there are more starts
#' than ends; both vectors then pass through \code{.raw.guider.correct.dst}. An edge that
#' matches no epoch errors, as in GGIR.
#'
#' @param clocktime Character vector of "%H:%M:%S" clock times, one per short epoch.
#' @param reference_window Length 2 numeric, the start and end of the window in decimal hours.
#' @param epochSize Short epoch length in seconds.
#' @return list(ref_min, ref_max).
#' @keywords internal
#' @noRd
.raw.guider.matching.indices <- function(clocktime, reference_window, epochSize) {
  ref_min <- ref_max <- NULL
  for (ki in 1:2) {
    if (reference_window[ki] >= 24) {
      reference_window[ki] <- reference_window[ki] - 24
    }
    HR <- floor(reference_window[ki])
    MIN <- floor((reference_window[ki] - HR) * 60)
    SEC <- floor((reference_window[ki] - HR - (MIN / 60)) * 3600)
    SEC <- floor(SEC / epochSize) * epochSize
    HR <- ifelse(HR < 10, yes = paste0("0", HR), no = as.character(HR))
    MIN <- ifelse(MIN < 10, yes = paste0("0", MIN), no = as.character(MIN))
    SEC <- ifelse(SEC < 10, yes = paste0("0", SEC), no = as.character(SEC))
    ref_time <- paste(HR, MIN, SEC, sep = ":")
    if (ki == 1) {
      ref_min <- which(clocktime == ref_time)
    } else if (ki == 2) {
      ref_max <- which(clocktime == ref_time)
    }
  }
  if (ref_max[1] < ref_min[1]) {
    # recording started after the start of the reference window
    ref_min <- c(1, ref_min)
  }
  if (length(ref_min) > length(ref_max)) {
    # recording ended before the end of the reference window
    ref_max <- c(ref_max, length(clocktime))
  }
  ref_min <- .raw.guider.correct.dst(ref_min, epochSize)
  ref_max <- .raw.guider.correct.dst(ref_max, epochSize)
  return(list(ref_min = ref_min, ref_max = ref_max))
}

#' Crude Sleep Period Estimate Inside One Min-Max Segment (GGIR get_crude_estimate)
#'
#' Pulls \code{spt_crude_estimate} over the segment (2 the guider's main sleep window, 1
#' other resting windows, 0 the rest) and blanks every code-1 block whose mean sustained
#' inactivity is below 0.8. That 0.8 is hardcoded in GGIR and runs before
#' \code{guider_cor_min_frac_sib}, which is therefore inert below 0.8. With more than one
#' sustained inactivity definition the sib selection stays a data.frame and \code{sib[a:b]}
#' indexes columns, which is GGIR's "undefined columns selected" crash.
#'
#' @param output The per-epoch data.frame of the sib detection result.
#' @param tSegment Integer indices of the segment within \code{output}.
#' @return Numeric vector of crude estimate codes, one per epoch of the segment.
#' @keywords internal
#' @noRd
.raw.guider.crude.estimate <- function(output, tSegment) {
  crude_est <- output$spt_crude_estimate[tSegment]
  sib <- output[tSegment, grep(pattern = "time|invalid|night|estimate|medmed", x = colnames(output), invert = TRUE)]
  if (1 %in% crude_est) {
    # omit 1-segments that have less than 80% sib
    bin_est <- as.integer(crude_est == 1) # 2 maps to 0 so adjacent 1 and 2 runs stay separate
    class_changes <- diff(c(0, bin_est, 0))
    segment_start <- which(class_changes == 1)
    segment_end <- which(class_changes == -1) - 1
    if (length(segment_end) == 0) {
      crude_est[which(crude_est == 1)] <- 0
    } else {
      for (gi in seq_along(segment_start)) {
        if (mean(sib[segment_start[gi]:segment_end[gi]]) < 0.8) {
          crude_est[segment_start[gi]:segment_end[gi]] <- 0
        }
      }
    }
  }
  return(crude_est)
}

#' Two Clock Times of a Corrected Block, as Decimal Hours (GGIR convert_ts_to_hours)
#'
#' The block is \code{range(which(crude_est == ref_value))}, both ends inclusive, so the
#' corrected end is the last epoch of the block, one epoch earlier than the half-open
#' convention HASPT uses. Each clock time becomes hours rounded to 3 digits, anything at or
#' below 12 gains 24, and the pair is then reconciled across midnight. When several blocks
#' carry \code{ref_value} the range spans all of them, wake gaps included.
#'
#' @param clocktime Character vector of "%H:%M:%S" clock times, one per short epoch.
#' @param crude_est Crude estimate codes of the segment.
#' @param tSegment Integer indices of the segment within the recording.
#' @param ref_value The code whose range defines the new window.
#' @return Length 2 numeric, the new start and end in decimal hours.
#' @keywords internal
#' @noRd
.raw.guider.ts.to.hours <- function(clocktime, crude_est, tSegment, ref_value = 1) {
  new_window <- range(which(crude_est == ref_value))
  new_SPTE <- clocktime[tSegment[new_window]]
  convert_time <- function(x) {
    spit_time <- as.numeric(unlist(strsplit(x, ":")))
    time_hours <- round(sum((spit_time * c(3600, 60, 1)) / 3600), digits = 3)
    if (time_hours <= 12) time_hours <- time_hours + 24
    return(time_hours)
  }
  new_SPTE <- unlist(lapply(X = new_SPTE, FUN = convert_time))
  if (new_SPTE[1] > new_SPTE[2] && new_SPTE[2] > 12 && new_SPTE[1] < 24) {
    new_SPTE[2] <- new_SPTE[2] + 24
  } else if (new_SPTE[1] > new_SPTE[2] & new_SPTE[1] > 24) {
    new_SPTE[1] <- new_SPTE[1] - 24
  }
  return(new_SPTE)
}

#' Drop the Temporary Columns of the Guider Correction (GGIR clean_SLE)
#'
#' Removes clocktime, time_POSIX, spt_crude_estimate and medmed from \code{$output}.
#' spt_crude_estimate goes too, as in GGIR; g.part3 drops it anyway on the uncorrected path.
#'
#' @param sle The sib detection result list.
#' @return The same list with the temporary columns removed from \code{$output}.
#' @keywords internal
#' @noRd
.raw.guider.clean <- function(sle) {
  temp_columns <- c("clocktime", "time_POSIX", "spt_crude_estimate", "medmed")
  sle$output <- sle$output[, which(names(sle$output) %in% temp_columns == FALSE)]
  return(sle)
}

#' Correct the Sleep Period Time Guider Against the Rest of the Recording
#'
#' GGIR's optional correction of the per-night guider (parameter \code{guider_cor_do}, FALSE
#' by default). Inside a reference window built from all valid nights it replaces a main
#' sleep window that sits almost entirely outside the median-median window with a better
#' placed resting block (step 3), and expands the main window across neighbouring long
#' resting blocks (step 4).
#'
#' @details The input must still carry its \code{spt_crude_estimate} column. Nights count as
#'   valid when \code{night > 0}, fewer than a third of a day's epochs are invalid and both
#'   guider edges are non-NA. Steps 2 and 3 need at least \code{guider_cor_meme_min_dys}
#'   valid nights. Step 4 keeps resting blocks of at least \code{guider_cor_min_hrs}, drops
#'   any block a wake run of at least \code{guider_cor_maxgap_hrs} separates from the main
#'   window, and takes the range across the survivors, wake gaps included; it does not write
#'   back into \code{spt_crude_estimate}. Parameter defaults follow GGIR's
#'   \code{load_params}, not the guider vignette table, which disagrees with the code in four
#'   places.
#'
#'   Under \code{params_sleep$ggir_exact} TRUE (the default) two GGIR behaviours are kept:
#'   more than one sustained inactivity definition crashes with "undefined columns selected",
#'   and when several candidate blocks qualify in step 3 the first is used with R's "only the
#'   first used" warning. Under FALSE the first case is refused up front with a named error
#'   and the second warns with the night and the number of candidates; the result is the
#'   same. LC_TIME is forced to "C" for the duration of the call.
#'
#' @param sle Sib detection result, a list with \code{$output} (columns time, invalid, night,
#'   one column per sustained inactivity definition, spt_crude_estimate), \code{$SPTE_start} and
#'   \code{$SPTE_end}, one element per night.
#' @param desiredtz Timezone the timestamps are interpreted in.
#' @param epochSize Short epoch length in seconds.
#' @param params_sleep Named list holding \code{guider_cor_maxgap_hrs},
#'   \code{guider_cor_min_frac_sib}, \code{guider_cor_min_hrs}, \code{guider_cor_meme_frac_out},
#'   \code{guider_cor_meme_frac_in}, \code{guider_cor_meme_min_hrs},
#'   \code{guider_cor_meme_min_dys} and optionally \code{ggir_exact}.
#' @return The \code{sle} list with \code{SPTE_start}, \code{SPTE_end} and the new
#'   \code{SPTE_corrected} (per night: 0 nothing corrected, 1 step 4 only, 2 step 3 only, 3
#'   both), with the temporary columns removed from \code{$output}.
#' @export
raw.guider.correct <- function(sle, desiredtz, epochSize, params_sleep) {

  guider_cor_maxgap_hrs <- params_sleep[["guider_cor_maxgap_hrs"]]
  guider_cor_min_frac_sib <- params_sleep[["guider_cor_min_frac_sib"]]
  guider_cor_min_hrs <- params_sleep[["guider_cor_min_hrs"]]
  guider_cor_meme_frac_out <- params_sleep[["guider_cor_meme_frac_out"]]
  guider_cor_meme_frac_in <- params_sleep[["guider_cor_meme_frac_in"]]
  guider_cor_meme_min_hrs <- params_sleep[["guider_cor_meme_min_hrs"]]
  guider_cor_meme_min_dys <- params_sleep[["guider_cor_meme_min_dys"]]
  ggir_exact <- params_sleep[["ggir_exact"]]
  if (is.null(ggir_exact)) ggir_exact <- TRUE

  old_lctime <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  if (nzchar(old_lctime)) {
    on.exit(Sys.setlocale("LC_TIME", old_lctime), add = TRUE)
  }

  if (!ggir_exact) {
    # refuse more than one definition up front; GGIR crashes on it
    Ndefs <- length(grep(pattern = "time|invalid|night|estimate|medmed",
                         x = colnames(sle$output), invert = TRUE))
    if (Ndefs != 1) {
      stop(paste0("raw.guider.correct() works with exactly one sustained inactivity ",
                  "definition, found ", Ndefs, ". GGIR crashes on this combination with ",
                  "\"undefined columns selected\"; set ggir_exact = TRUE to reproduce that."),
           call. = FALSE)
    }
  }

  # HASPT estimates every night; only the valid nights are used here
  invalid_per_night <- aggregate(x = sle$output$invalid, by = list(sle$output$night), FUN = sum)
  names(invalid_per_night) <- c("night", "count")
  valid_nights <- invalid_per_night$night[which(invalid_per_night$night > 0 & invalid_per_night$count < 24 * 3600 / epochSize * 0.333)]
  if (length(valid_nights) > 0) {
    valid_nights <- valid_nights[which(is.na(sle$SPTE_start[valid_nights]) == FALSE &
                                         is.na(sle$SPTE_end[valid_nights]) == FALSE)]
  }
  sle$SPTE_corrected <- rep(0, length(sle$SPTE_start))
  if (length(valid_nights) < 1) {
    sle <- .raw.guider.clean(sle)
    return(sle)
  }

  # Step 1: reference window from the earliest start to the latest end across nights
  reference_window <- c(min(sle$SPTE_start[valid_nights], na.rm = TRUE),
                        max(sle$SPTE_end[valid_nights], na.rm = TRUE))
  sle$output$time_POSIX <- .raw.iso8601.to.posix(sle$output$time, tz = desiredtz)
  sle$output$clocktime <- format(sle$output$time_POSIX, format = "%H:%M:%S")

  ref_indices <- .raw.guider.matching.indices(sle$output$clocktime, reference_window, epochSize)
  ref_min <- ref_indices$ref_min
  ref_max <- ref_indices$ref_max
  if (length(ref_min) < 2 || length(ref_max) < 2) {
    sle <- .raw.guider.clean(sle)
    return(sle)
  }
  if (length(valid_nights) >= guider_cor_meme_min_dys) {
    # Step 2: median-median window, used to decide whether the HDCZA window is replaced
    medmed_reference_window <- c(median(sle$SPTE_start[valid_nights], na.rm = TRUE),
                                 median(sle$SPTE_end[valid_nights], na.rm = TRUE))
    ref_indices <- .raw.guider.matching.indices(sle$output$clocktime, medmed_reference_window, epochSize)
    ref_med1 <- ref_indices$ref_min
    ref_med2 <- ref_indices$ref_max
    if (length(ref_med1) < 2 || length(ref_med2) < 2) {
      sle <- .raw.guider.clean(sle)
      return(sle)
    }
    # Deal with non-matching index vectors caused by incomplete first or last night
    Ntimepoints <- length(sle$output$clocktime)
    newVectors <- .raw.align.index.vectors(x = ref_min, y = ref_max,
                                           a = ref_med1, b = ref_med2,
                                           N = Ntimepoints)
    ref_min <- newVectors$x
    ref_max <- newVectors$y
    ref_med1 <- newVectors$a
    ref_med2 <- newVectors$b

    if (length(ref_min) != length(ref_med1) ||
        length(ref_max) != length(ref_med2)) {
      stop("index vectors do not match")
    }
    # Step 3: replace the HDCZA window when it falls outside med-med and a secondary
    # HDCZA lies inside it
    for (ji in 1:length(ref_min)) {
      if (ji %in% valid_nights) {
        tSegment <- ref_min[ji]:ref_max[ji]
        crude_est <- .raw.guider.crude.estimate(sle$output, tSegment)
        sib <- sle$output[tSegment, grep(pattern = "time|invalid|night|estimate|medmed", x = colnames(sle$output), invert = TRUE)]
        tSegment_med <- ref_med1[ji]:ref_med2[ji]
        sle$output$medmed <- 0
        sle$output$medmed[tSegment_med] <- 1
        medmed <- sle$output$medmed[tSegment]
        # Is original HDCZA estimate largely outside median-median window?
        if (length(tSegment_med) > 3600 / epochSize &&
            length(which(crude_est == 2)) != 0  &&
            length(which(crude_est == 2 & medmed == 0)) /
            length(which(crude_est == 2)) > guider_cor_meme_frac_out) {
          # summary per segment; each non-zero segment gets a unique id
          temp_rle <- rle(crude_est)
          nonzero <- which(temp_rle$values != 0)
          temp_rle$values[nonzero] <- 1:length(nonzero)
          seg_id <- rep(temp_rle$values, temp_rle$lengths)

          df <- data.frame(crude_est = crude_est, medmed = medmed,
                           index = 1:length(crude_est), sib = sib,
                           seg_id = seg_id)
          segment_level <- aggregate(df[, c("crude_est", "seg_id")], by = list(df$seg_id), FUN = mean)[, 1:2]
          names(segment_level) <- c("seg_id", "crude_est")
          segment_sib <- aggregate(df[, c("sib", "seg_id")], by = list(df$seg_id), FUN = mean)[, 1:2]
          names(segment_sib) <- c("seg_id", "sib")
          segment_summary <- merge(segment_level, segment_sib, by = "seg_id")

          perc_value_one <- function(x) {
            return(length(which(x == 1)) / length(x))
          }
          segment_overlap_medmed <- aggregate(df[, c("medmed", "seg_id")], by = list(df$seg_id), FUN = perc_value_one)[, 1:2]
          names(segment_overlap_medmed) <- c("seg_id", "medmed")
          segment_summary <- merge(segment_summary, segment_overlap_medmed, by = "seg_id")

          segment_size_hours <- aggregate(df$index, by = list(df$seg_id), FUN = length)[, 1:2]
          segment_size_hours[, 2] <- segment_size_hours[, 2] / (3600 / epochSize)
          names(segment_size_hours) <- c("seg_id", "segment_size_hours")
          segment_summary <- merge(segment_summary, segment_size_hours, by = "seg_id")

          segment_start <- aggregate(df[, c("index", "seg_id")], by = list(df$seg_id), FUN = min)[, 1:2]
          names(segment_start) <- c("seg_id", "start_index")
          segment_summary <- merge(segment_summary, segment_start, by = "seg_id")

          segment_end <- aggregate(df[, c("index", "seg_id")], by = list(df$seg_id), FUN = max)[, 1:2]
          names(segment_end) <- c("seg_id", "end_index")
          segment_summary <- merge(segment_summary, segment_end, by = "seg_id")

          rm(segment_sib, segment_overlap_medmed, segment_start, segment_size_hours, segment_end)

          sle$output <- sle$output[, -which(colnames(sle$output) == "medmed")]
          # a secondary HDCZA window: mostly inside med-med, long enough, mostly sib
          new_main_HDCZA <- which(segment_summary$crude_est == 1 &
                                    segment_summary$medmed > guider_cor_meme_frac_in &
                                    segment_summary$segment_size_hours > guider_cor_meme_min_hrs &
                                    segment_summary$sib > guider_cor_min_frac_sib)

          if (length(new_main_HDCZA) > 0) {
            if (!ggir_exact && length(new_main_HDCZA) > 1) {
              warning(paste0("night ", ji, ": ", length(new_main_HDCZA), " candidate resting ",
                             "blocks qualify as the new main sleep window, GGIR uses the first ",
                             "one only"), call. = FALSE)
            }
            # the old window becomes 1, the new one 2
            old_2 <- which(segment_summary$crude_est == 2)
            segment_summary$crude_est[old_2] <- 1
            crude_est[segment_summary$start_index[old_2]:segment_summary$end_index[old_2]] <- 1
            segment_summary$crude_est[new_main_HDCZA] <- 2
            crude_est[segment_summary$start_index[new_main_HDCZA]:segment_summary$end_index[new_main_HDCZA]] <- 2

            new_SPTE <- .raw.guider.ts.to.hours(sle$output$clocktime, crude_est, tSegment, ref_value = 2)
            sle$output$spt_crude_estimate[tSegment] <- crude_est
            sle$SPTE_start[ji] <- new_SPTE[1]
            sle$SPTE_end[ji] <- new_SPTE[2]
            sle$SPTE_corrected[ji] <- 2
          }
        }
      }
    }
    if (any(sle$SPTE_corrected != 0)) {
      # a corrected night changes the reference window, so rebuild it
      reference_window <- c(min(sle$SPTE_start[valid_nights], na.rm = TRUE),
                            max(sle$SPTE_end[valid_nights], na.rm = TRUE))

      ref_indices <- .raw.guider.matching.indices(sle$output$clocktime, reference_window, epochSize)
      ref_min <- ref_indices$ref_min
      ref_max <- ref_indices$ref_max
      if (length(ref_min) < 2 || length(ref_max) < 2) {
        sle <- .raw.guider.clean(sle)
        return(sle)
      }
    }
  }
  # Step 4: expand the HDCZA window across neighbouring long resting blocks
  for (ji in 1:length(ref_min)) {
    if (ji %in% valid_nights) {
      tSegment <- ref_min[ji]:ref_max[ji]
      crude_est <- .raw.guider.crude.estimate(sle$output, tSegment)

      # only when there is rest (1) outside the guider (2)
      if (1 %in% crude_est & 2 %in% crude_est) {
        rle_rest <- rle(crude_est)
        # long resting blocks
        long_rest <- which(rle_rest$values == 1 & rle_rest$lengths * epochSize >= guider_cor_min_hrs * 3600)
        # drop a long rest that a long wake run separates from the main sleep
        if (length(long_rest) > 0) {
          if (!is.null(guider_cor_maxgap_hrs) &&
              !is.infinite(guider_cor_maxgap_hrs)) {
            long_wake <- which(rle_rest$values == 0 & rle_rest$lengths * epochSize >= guider_cor_maxgap_hrs * 3600)
            if (length(long_wake) > 0) {
              rle_rest$values[long_wake] <- -1
              ind2remove <- NULL
              original <- which(rle_rest$values == 2) # only one segment is expected to be 2
              for (lri in 1:length(long_rest)) {
                this_long_rest <- long_rest[lri]
                too_long_wake <- which(rle_rest$values == -1)
                if (any(too_long_wake > original & too_long_wake < this_long_rest) |
                    any(too_long_wake < original & too_long_wake > this_long_rest)) {
                  ind2remove <- c(ind2remove, lri)
                }
              }
              if (!is.null(ind2remove)) {
                long_rest <- long_rest[-ind2remove]
              }
            }
          }
        }
        # the survivors join the main window
        if (length(long_rest) > 0) {
          rle_rest$values[long_rest] <- 2
          rle_rest$values[which(rle_rest$values == 1)] <- 0
          rle_rest$values[which(rle_rest$values == 2)] <- 1
          N <- length(crude_est)
          crude_est <- rep(rle_rest$values, rle_rest$lengths)[1:N]

          new_SPTE <- .raw.guider.ts.to.hours(sle$output$clocktime, crude_est, tSegment, ref_value = 1)
          sle$SPTE_start[ji] <- new_SPTE[1]
          sle$SPTE_end[ji] <- new_SPTE[2]
          if (sle$SPTE_corrected[ji] == 0) {
            sle$SPTE_corrected[ji] <- 1
          } else {
            sle$SPTE_corrected[ji] <- 3
          }
        }
      }
    }
  }
  sle <- .raw.guider.clean(sle)
  return(sle)
}
