# Ported from GGIR 3.3-9 R/g.part5.analyseRest.R and R/markerButtonForRest.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The nested summarise_overlap is
# hoisted to file level with explicit arguments; the character matrix column machine is
# replaced by the named-list row, so the 27 names are created in one place; params_sleep
# becomes explicit arguments, with the list still accepted as a fallback; is.ISO8601 and
# iso8601chartime2POSIX are reused as .raw.is.iso8601 and .raw.iso8601.to.posix; the
# unread SleeplogOverlapSIB_indices assignment is not ported; three defects are
# reproduced under ggir_exact = TRUE and corrected under FALSE.

# HELPERS

#' Resolve One Rest-Analysis Setting
#'
#' An explicit argument wins; the GGIR-shaped \code{params_sleep} list is consulted only when
#' the argument is NULL.
#'
#' @param value The explicit argument, or NULL.
#' @param params_sleep A named list of sleep parameters, or NULL.
#' @param name Member name to look up.
#' @param default Value to use when neither supplies one.
#' @return The resolved value.
#' @keywords internal
#' @noRd
.raw.timeuse.rest.setting <- function(value, params_sleep, name, default = NULL) {
  if (!is.null(value)) return(value)
  if (!is.null(params_sleep) && name %in% names(params_sleep) &&
      !is.null(params_sleep[[name]])) {
    return(params_sleep[[name]])
  }
  return(default)
}

#' The 27 Rest-Analysis Column Names, in GGIR's Order
#'
#' GGIR writes these names unconditionally before any value is stored, so the block always
#' consumes exactly 27 column slots. Columns 1 to 9 are counts, 10 to 15 and the mdur and
#' tdur columns are minutes (from the sib report's own \code{duration} column), and the four
#' perc columns are duration-weighted mean overlap percentages, 0 to 100.
#'
#' @return A character vector of length 27.
#' @keywords internal
#' @noRd
.raw.timeuse.rest.names <- function() {
  c("sibreport_n_items",
    "sibreport_n_items_day", "nbouts_day_denap",
    "nbouts_day_srnap", "nbouts_day_srnonw",
    "noverl_denap_srnap", "noverl_denap_srnonw",
    "noverl_srnap_denap", "noverl_srnonw_denap",
    "frag_mean_dur_denap_day", "dur_day_denap_min",
    "frag_mean_dur_srnap_day", "dur_day_srnap_min",
    "frag_mean_dur_srnonw_day", "dur_day_srnonw_min",
    "mdur_denap_overl_srnap", "tdur_denap_overl_srnap",
    "perc_denap_overl_srnap", "mdur_srnap_overl_denap",
    "tdur_srnap_overl_denap", "perc_srnap_overl_denap",
    "mdur_denap_overl_srnonw", "tdur_denap_overl_srnonw",
    "perc_denap_overl_srnonw", "mdur_srnonw_overl_denap",
    "tdur_srnonw_overl_denap", "perc_srnonw_overl_denap")
}

# MARKER BUTTONS

#' Use Marker-Button Presses to Accept, Reject or Re-Time Candidate Naps
#'
#' GGIR's \code{markerButtonForRest}. Adds the \code{ignore} column that the candidate filter
#' of \code{.raw.timeuse.rest} reads and, when a marker channel is available and
#' \code{nap_markerbutton_method} is above 0, uses the presses near the edges of each
#' sustained inactivity bout to confirm the bout (methods 2 and 3) or to overwrite its start
#' and end (methods 1 and 3).
#'
#' @details The de-duplication on (start, end) runs on every call, so this is never a pure
#'   pass-through. \code{ignore} is set TRUE for every row when the method is above 1, so with
#'   method 2 or 3 and no marker channel every bout is ignored. The distance to the nearest
#'   press is measured from the bout edge through the bout midtime, with a bare less-than
#'   against \code{nap_markerbutton_max_distance}. When the timing is copied, the overwrite
#'   also drags other bouts' edges onto this one so that they collapse into duplicate rows.
#'   No reference recording carries a marker channel; the tests drive it on a synthetic series.
#'
#' @param sibreport The sib report, with start, end and duration columns.
#' @param ts The waking epochs of this window, optionally with a \code{marker} column.
#' @param nap_markerbutton_method 0 to 3.
#' @param nap_markerbutton_max_distance Maximum distance in minutes between a bout edge and a
#'   press.
#' @param params_sleep Optional GGIR-shaped sleep parameter list, consulted only for an argument
#'   left NULL.
#' @return The sib report with an \code{ignore} column, de-duplicated on (start, end), and with
#'   \code{midtime}, \code{start_to_marker} and \code{end_to_marker} added when the marker
#'   branch ran.
#' @keywords internal
#' @noRd
.raw.markerbutton.rest <- function(sibreport, ts = NULL,
                                   nap_markerbutton_method = NULL,
                                   nap_markerbutton_max_distance = NULL,
                                   params_sleep = NULL) {
  nap_markerbutton_method <- .raw.timeuse.rest.setting(nap_markerbutton_method, params_sleep,
                                                       "nap_markerbutton_method", 0)
  nap_markerbutton_max_distance <-
    .raw.timeuse.rest.setting(nap_markerbutton_max_distance, params_sleep,
                              "nap_markerbutton_max_distance", 30)
  sibreport$ignore <- FALSE # if no marker button than keep sibs
  if (nap_markerbutton_method > 0) {
    if (nap_markerbutton_method > 1) {
      sibreport$ignore <- TRUE # if no marker button then ignore all sibs
    }
    nap_mb_max_dist <- nap_markerbutton_max_distance
    if ("marker" %in% colnames(ts)) {
      if (1 %in% ts$marker) {
        if (nap_markerbutton_method == 1) {
          nap_require_mb <- FALSE
          nap_copy_timing_mb <- TRUE
        } else if (nap_markerbutton_method == 2) {
          nap_require_mb <- TRUE
          nap_copy_timing_mb <- FALSE
        } else if (nap_markerbutton_method == 3) {
          nap_require_mb <- TRUE
          nap_copy_timing_mb <- TRUE
        }
        # time in minutes to the nearest press before and after the sib midtime
        sibreport$midtime <- sibreport$start + (sibreport$end - sibreport$start) / 2
        marker_times <- ts$time[which(ts$marker == 1)]
        sibreport$start_to_marker <- NA
        sibreport$end_to_marker <- NA
        for (hi in 1:nrow(sibreport)) {
          delta_times <- as.numeric(sibreport$midtime[hi]) - as.numeric(marker_times)
          if (nap_copy_timing_mb == TRUE) {
            # for the start, only presses before midtime; for the end, only presses after
            end_delta_times <- delta_times[which(delta_times < 0)]
            start_delta_times <- delta_times[which(delta_times > 0)]
          } else {
            # any nearby press will do
            start_delta_times <- end_delta_times <- delta_times
          }
          nap_start_found <- nap_end_found <- FALSE
          half_sib_dur <- sibreport$duration[hi] / 2
          tmp_start <- tmp_end <- NULL
          # find marker for start nap
          if (length(start_delta_times) > 0) {
            sibreport$start_to_marker[hi] <- min(abs(start_delta_times)) / 60 - half_sib_dur
            if (any(sibreport$start_to_marker[hi] < nap_mb_max_dist)) {
              if (nap_copy_timing_mb == TRUE) {
                tmp_start <- sibreport$midtime[hi] -
                  start_delta_times[which.min(abs(start_delta_times))]
              }
              nap_start_found <- TRUE
            }
          }
          # find marker for end nap
          if (length(end_delta_times) > 0) {
            sibreport$end_to_marker[hi] <- min(abs(end_delta_times)) / 60 - half_sib_dur
            if (any(sibreport$end_to_marker[hi] < nap_mb_max_dist)) {
              if (nap_copy_timing_mb == TRUE) {
                tmp_end <- sibreport$midtime[hi] +
                  abs(end_delta_times[which.min(abs(end_delta_times))])
              }
              nap_end_found <- TRUE
            }
          }
          if (nap_require_mb == TRUE && (nap_start_found == FALSE || nap_end_found == FALSE)) {
            # a press was required and none was near
            sibreport$ignore[hi] <- TRUE
          } else {
            sibreport$ignore[hi] <- FALSE
            # overwrite all other sibs that overlap such that they form duplicated rows
            if (length(tmp_start) > 0) {
              sibreport$start[hi] <- tmp_start
              sibreport$start[which(sibreport$start < sibreport$midtime[hi] &
                                      sibreport$start >= tmp_start)] <- sibreport$start[hi]
            }
            if (length(tmp_end) > 0) {
              sibreport$end[hi] <- tmp_end
              sibreport$end[which(sibreport$end < sibreport$midtime[hi] &
                                    sibreport$end >= tmp_end)] <- sibreport$end[hi]
            }
            sibreport$midtime <- sibreport$start + (sibreport$end - sibreport$start) / 2
          }
        }
      }
    }
  }
  # remove duplicate rows
  sibreport <- sibreport[!duplicated(sibreport[, c("start", "end")]), ]
  return(sibreport)
}

# REST ANALYSIS

#' Summarise the Overlap Between Detected Naps and One Self-Reported Behaviour
#'
#' Writes six of the 27 columns: mean duration, total duration and duration-weighted mean
#' overlap percentage, first for the accelerometer bouts that overlap a self-reported episode
#' and then for the self-reported episodes that overlap an accelerometer bout.
#'
#' @details GGIR's nested \code{summarise_overlap}. The percentage is
#'   \code{sum(overlap * duration) / sum(duration)} over the already rounded per-pair
#'   percentages, so it is not itself rounded. GGIR's second half tests \code{length(xi) > 0}
#'   where it means \code{length(yi) > 0}, so with \code{xi} non-empty and \code{yi} empty
#'   the row reads NaN, 0, NaN instead of 0, 0, 0; reproduced under \code{ggir_exact = TRUE}.
#'
#' @param row The named-list day-summary row being built.
#' @param srep_tmp The restricted sib report for this window.
#' @param X Column name holding the overlap of a bout with the self-report.
#' @param Y Column name holding the overlap of the self-report with a bout.
#' @param xi Row indices of the overlapping bouts, for X.
#' @param yi Row indices of the overlapping self-reports, for Y.
#' @param name Middle part of the six column names, "srnap" or "srnonw".
#' @param ggir_exact TRUE reproduces the wrong index test described above.
#' @return The row with six more entries.
#' @keywords internal
#' @noRd
.raw.timeuse.rest.overlap <- function(row, srep_tmp, X, Y, xi, yi, name = "",
                                      ggir_exact = TRUE) {
  calcOverlapPercentage <- function(overlap, duration) {
    return(sum(overlap * duration) / sum(duration))
  }
  # Overlap sib with selfreport
  ds_names <- c(paste0("mdur_denap_overl_", name),
                paste0("tdur_denap_overl_", name),
                paste0("perc_denap_overl_", name))
  if (length(xi) > 0) {
    row[ds_names] <- list(mean(srep_tmp$duration[xi]),
                          sum(srep_tmp$duration[xi]),
                          calcOverlapPercentage(overlap = srep_tmp[xi, X],
                                                duration = srep_tmp$duration[xi]))
  } else {
    row[ds_names] <- list(0, 0, 0)
  }
  # Overlap selfreport with sib
  ds_names <- c(paste0("mdur_", name, "_overl_denap"),
                paste0("tdur_", name, "_overl_denap"),
                paste0("perc_", name , "_overl_denap"))
  if ((isTRUE(ggir_exact) && length(xi) > 0) ||
      (!isTRUE(ggir_exact) && length(yi) > 0)) {
    row[ds_names] <- list(mean(srep_tmp$duration[yi]),
                          sum(srep_tmp$duration[yi]),
                          calcOverlapPercentage(overlap = srep_tmp[yi, Y],
                                                duration = srep_tmp$duration[yi]))
  } else {
    row[ds_names] <- list(0, 0, 0)
  }
  return(row)
}

#' Daytime Rest, Naps and Their Overlap With a Diary
#'
#' GGIR's \code{g.part5.analyseRest}. Turns the whole-recording sib report into 27
#' day-summary columns describing daytime rest, and marks every accepted nap in the time
#' series with \code{sibdetection == 2}, which moves those epochs into the \code{day_nap}
#' class. Off by default: \code{possible_nap_window} and \code{possible_nap_dur} are both
#' NULL in \code{raw.params()}, and when it is off the 27 columns are absent from the day
#' summary, not NA.
#'
#' An accelerometer bout qualifies when its type is "sib", its duration is at least
#' \code{possible_nap_dur[1]} and below \code{possible_nap_dur[2]} minutes, the larger of the
#' mean acceleration in the minute before and after is at most \code{possible_nap_edge_acc},
#' the whole hour it starts in is at or after \code{possible_nap_window[1]}, the whole hour
#' it ends in is before \code{possible_nap_window[2]}, and the marker-button pass did not set
#' \code{ignore}; every non-sib row qualifies on a duration of one minute or more. A
#' qualifying bout is dropped again if 10 percent or more of its epochs are non-wear.
#'
#' @details The 27 names are created once, in order, with NA values, and the branches fill
#'   what they fill; an NA becomes "" in \code{.raw.timeuse.row.chr}, which is what GGIR's
#'   unfilled cells hold. GGIR's conventions, reproduced: the first half runs only when the
#'   sib report has more than one row; \code{sibreport_n_items} counts the candidates over
#'   the whole recording; the window restriction needs both endpoints inside the waking
#'   epochs of the window; the epoch marking is start- and end-inclusive; \code{startHour}
#'   and \code{endHour} are whole hours, with 24 added to \code{endHour} across midnight; the
#'   self-reported nap durations are recomputed as end minus start with no epoch added;
#'   \code{possible_nap_gap} is compared in minutes although GGIR documents it as seconds.
#'
#'   Three defects are reproduced under \code{ggir_exact = TRUE} and corrected under FALSE;
#'   none changes anything without a diary and without \code{possible_nap_gap}. (1)
#'   \code{longboutsi} is computed against the row numbering before the sleep-log rows are
#'   removed and the window's own sleep-log segments appended, then applied to the rebuilt
#'   frame; under FALSE the selection travels with the rows as a logical flag. (2) The wrong
#'   index vector in \code{.raw.timeuse.rest.overlap}. (3) The gap recomputed after a merge
#'   is in seconds where the vectorised gap is in minutes, so a second merge of the same
#'   pair is suppressed; under FALSE it divides by 60.
#'
#'   Not ported: three assignments nothing reads (\code{SleeplogOverlapSIB_indices},
#'   \code{classes} and \code{SIBoverlapSleeplog_indices}) and the commented-out column-name
#'   assertions. One further defect is kept and not behind \code{ggir_exact}: the appended
#'   sleep-log rows are written as \code{format(ts$time[...])}, a clock string with no UTC
#'   offset, which the POSIXct column re-parses in the session timezone.
#'
#' @param sibreport The whole-recording sib report from \code{raw.sib.report()}: ID, type,
#'   start, end, duration in minutes, and, for accelerometer rows, mean_acc_1min_before and
#'   mean_acc_1min_after.
#' @param row The named-list day-summary row being built by \code{.raw.timeuse.segment}.
#' @param ts The waking epochs of this window only, \code{ts[sse[ts$diur[sse] == 0], ]}, with
#'   time, nonwear, sibdetection and, when a diary was used, selfreported. The epoch size is
#'   taken from the first two rows only, because the rows need not be contiguous.
#' @param tz Timezone for every character to POSIX coercion.
#' @param possible_nap_window Numeric length 2, clock hours. Nap detection is off when NULL.
#' @param possible_nap_dur Numeric length 2, minutes, lower bound inclusive and upper
#'   exclusive. Nap detection is off when NULL.
#' @param possible_nap_gap Minutes. Above 0 it merges bouts separated by a shorter gap and
#'   recomputes every duration with one epoch added.
#' @param possible_nap_edge_acc Upper bound on the larger of the two one-minute edge means, in
#'   the unit of ts$ACC.
#' @param nap_markerbutton_method,nap_markerbutton_max_distance Passed to
#'   \code{.raw.markerbutton.rest}.
#' @param params_sleep Optional GGIR-shaped sleep parameter list, consulted only for an argument
#'   left NULL.
#' @param ggir_exact TRUE, the default, reproduces the three defects above.
#' @return \code{list(row, ts)}: the row with 27 more entries, and the waking-epoch slice of the
#'   series with \code{sibdetection} set to 2 on every accepted nap.
#' @keywords internal
#' @noRd
.raw.timeuse.rest <- function(sibreport = NULL, row = list(), ts = NULL, tz = NULL,
                              possible_nap_window = NULL, possible_nap_dur = NULL,
                              possible_nap_gap = NULL, possible_nap_edge_acc = NULL,
                              nap_markerbutton_method = NULL,
                              nap_markerbutton_max_distance = NULL,
                              params_sleep = NULL, ggir_exact = TRUE) {
  possible_nap_window <- .raw.timeuse.rest.setting(possible_nap_window, params_sleep,
                                                   "possible_nap_window")
  possible_nap_dur <- .raw.timeuse.rest.setting(possible_nap_dur, params_sleep,
                                                "possible_nap_dur")
  possible_nap_gap <- .raw.timeuse.rest.setting(possible_nap_gap, params_sleep,
                                                "possible_nap_gap", 0)
  possible_nap_edge_acc <- .raw.timeuse.rest.setting(possible_nap_edge_acc, params_sleep,
                                                     "possible_nap_edge_acc", Inf)
  if (is.null(row)) row <- list()
  # GGIR reads both from params_sleep and silently produces no candidate when one is missing
  if (length(possible_nap_window) != 2 || !is.numeric(possible_nap_window)) {
    stop("the rest analysis needs possible_nap_window as two clock hours", call. = FALSE)
  }
  if (length(possible_nap_dur) != 2 || !is.numeric(possible_nap_dur)) {
    stop("the rest analysis needs possible_nap_dur as two durations in minutes", call. = FALSE)
  }
  keep <- NULL # the selection under ggir_exact = FALSE
  if (!is.null(sibreport) &&
      length(sibreport[[1]]) > 1)  {
    # GGIR indexes these columns without checking
    missing_cols <- setdiff(c("time", "nonwear", "sibdetection"), colnames(ts))
    if (length(missing_cols) > 0) {
      stop("the rest analysis needs ts columns ", paste(missing_cols, collapse = ", "),
           call. = FALSE)
    }
    missing_cols <- setdiff(c("ID", "type", "start", "end", "duration"), colnames(sibreport))
    if (length(missing_cols) > 0) {
      stop("the rest analysis needs sibreport columns ", paste(missing_cols, collapse = ", "),
           call. = FALSE)
    }
    # transform time to POSIX
    if (.raw.is.iso8601(as.character(ts$time[1]))) {
      ts$time <- .raw.iso8601.to.posix(ts$time, tz = tz)
    }
    sibreport$end <- as.POSIXct(sibreport$end, tz = tz)
    sibreport$start <- as.POSIXct(sibreport$start, tz = tz)
    # merge sibs when gap is shorter than possible_nap_gap
    if (possible_nap_gap > 0) {
      sibreport$gap2next <- NA
      Nrow <- nrow(sibreport)
      sibreport$gap2next[1:(Nrow - 1)] <- (as.numeric(sibreport$start[2:Nrow]) -
                                             as.numeric(sibreport$end[1:(Nrow - 1)])) / 60
      sibreport$gap2next[which(sibreport$type != "sib" | sibreport$gap2next < 0)] <- NA
      iter <- 1
      while (iter < nrow(sibreport)) {
        if (!is.na(sibreport$gap2next[iter]) &&
            sibreport$gap2next[iter] < possible_nap_gap) {
          sibreport$end[iter] <- sibreport$end[iter + 1]
          sibreport$mean_acc_1min_after[iter] <- sibreport$mean_acc_1min_after[iter + 1]
          sibreport <- sibreport[-(iter + 1),]
          # GGIR does not divide by 60 here, so this gap is in seconds
          if (isTRUE(ggir_exact)) {
            sibreport$gap2next[iter] <- as.numeric(sibreport$start[iter + 1]) -
              as.numeric(sibreport$end[iter])
          } else {
            sibreport$gap2next[iter] <- (as.numeric(sibreport$start[iter + 1]) -
                                           as.numeric(sibreport$end[iter])) / 60
          }
          # merging makes iter refer to the next gap, so no increment
        } else {
          iter <- iter + 1
        }
        if (iter > nrow(sibreport) - 1) {
          break()
        }
      }
      epochSize <- as.numeric(difftime(ts$time[2], ts$time[1], units = "mins"))
      sibreport$duration <- as.numeric(difftime(sibreport$end, sibreport$start,
                                                units = "mins")) + epochSize
    }
    # Only consider sib episodes with minimum duration
    if (length(grep(pattern = "mean_acc_1min", x = colnames(sibreport))) > 0) {
      sibreport$acc_edge <- pmax(sibreport$mean_acc_1min_before, sibreport$mean_acc_1min_after)
    } else {
      sibreport$acc_edge <- 0
    }
    # marker button data may aid nap detection
    sibreport <- .raw.markerbutton.rest(sibreport, ts = ts,
                                        nap_markerbutton_method = nap_markerbutton_method,
                                        nap_markerbutton_max_distance =
                                          nap_markerbutton_max_distance,
                                        params_sleep = params_sleep)
    sibreport$startHour <- as.numeric(format(sibreport$start, "%H"))
    sibreport$endHour <- as.numeric(format(sibreport$end, "%H"))
    overlapMidnight <- which(sibreport$endHour < sibreport$startHour)
    if (length(overlapMidnight) > 0) {
      sibreport$endHour[overlapMidnight] <- sibreport$endHour[overlapMidnight] + 24
    }
    longboutsi <- which((sibreport$type == "sib" &
                           sibreport$duration >= possible_nap_dur[1] &
                           sibreport$duration < possible_nap_dur[2] &
                           sibreport$acc_edge <= possible_nap_edge_acc &
                           sibreport$startHour >= possible_nap_window[1] &
                           sibreport$endHour < possible_nap_window[2] &
                           sibreport$ignore == FALSE) |
                          (sibreport$type != "sib" & sibreport$duration >= 1))
    Nlongbouts  <- length(longboutsi)
    # GGIR applies longboutsi to the frame rebuilt below; under ggir_exact = FALSE the
    # selection travels with the rows
    if (!isTRUE(ggir_exact)) {
      keep <- rep(FALSE, nrow(sibreport))
      keep[longboutsi] <- TRUE
    }
    # the window's sleeplog segments are added here so they are inside the time range
    notsleeplog <- which(sibreport$type != "sleeplog" & sibreport$type != "sleeplog+bedlog")
    sibreport <- sibreport[notsleeplog,]
    if (!isTRUE(ggir_exact)) keep <- keep[notsleeplog]
    sleeplogi <- which(ts$selfreported == "sleeplog" | ts$selfreported == "sleeplog+bedlog")
    if (length(sleeplogi) > 0) {
      dsi <- diff(sleeplogi)
      sl_starts <- c(1, which(dsi != 1) + 1)
      sl_ends <- c(which(dsi != 1), length(sleeplogi))
      if (length(sl_starts) > 0) {
        for (Nsi in 1:length(sl_starts)) {
          newline <- nrow(sibreport) + 1
          sibreport[newline,] <- NA
          sibreport$ID[newline] <- sibreport$ID[newline - 1]
          sibreport$type[newline] <- "sleeplog"
          sibreport$start[newline] <- format(ts$time[sleeplogi[sl_starts[Nsi]]])
          sibreport$end[newline] <- format(ts$time[sleeplogi[sl_ends[Nsi]]])
          if (!isTRUE(ggir_exact)) keep <- c(keep, TRUE)
        }
      }
    }
    if (!isTRUE(ggir_exact)) {
      longboutsi <- which(keep)
      Nlongbouts <- length(longboutsi)
    }
  } else {
    Nlongbouts <- 0
    longboutsi <- NULL
  }
  row[.raw.timeuse.rest.names()] <- NA
  row$sibreport_n_items <- Nlongbouts
  if (length(longboutsi) > 0) {
    sibreport <- sibreport[longboutsi,]
    srep_tmp <- sibreport[which(sibreport$start >= min(ts$time) &
                                  sibreport$end <= max(ts$time)),]
    # update ts time series with the classified naps
    if ("sib" %in% srep_tmp$type) {
      sibnaps <- which(srep_tmp$type == "sib")
      srep_tmp_rowsdelete <- NULL
      if (length(sibnaps) > 0) {
        for (sni in 1:length(sibnaps)) {
          sibnap <- which(ts$time >= srep_tmp$start[sibnaps[sni]] &
                            ts$time <= srep_tmp$end[sibnaps[sni]])
          if (length(sibnap) > 0) {
            # Only consider nap it does not overlap for more than 10 percent with known nonwear
            fractionInvalid <- length(which(ts$nonwear[sibnap] == 1)) / length(sibnap)
            if (fractionInvalid < 0.1) {
              ts$sibdetection[sibnap] <- 2
            } else {
              srep_tmp_rowsdelete <- c(srep_tmp_rowsdelete , sibnaps[sni])
            }
          }
        }
        if (!is.null(srep_tmp_rowsdelete )) {
          srep_tmp <- srep_tmp[-srep_tmp_rowsdelete,]
        }
      }
    }
    # count and summarise each category, allowing for absent ones
    row$sibreport_n_items_day <- nrow(srep_tmp)
    if (nrow(srep_tmp) > 0) {
      sibs <- which(srep_tmp$type == "sib")
      srep_tmp$SIBoverlapNonwear <- 0
      srep_tmp$SIBoverlapNap <- 0
      srep_tmp$SIBoverlapSleeplog <- 0
      srep_tmp$NonwearOverlapSIB <- 0
      srep_tmp$NapOverlapSIB <- 0
      srep_tmp$SleeplogOverlapSIB <- 0
      srep_tmp$start <- as.POSIXct(srep_tmp$start, tz = tz)
      srep_tmp$end <- as.POSIXct(srep_tmp$end, tz = tz)
      if (length(sibs) > 0) {
        selfreport <- which(srep_tmp$type == "nonwear" | srep_tmp$type == "nap" |
                              srep_tmp$type == "sleeplog")
        # summarise overlap between selfreported and accelerometer-based SIB
        if (length(selfreport) > 0) {
          for (si in sibs) {
            for (sr in selfreport) {
              # SIB overlap with selfreported behaviour
              if (srep_tmp$start[si] <= srep_tmp$end[sr] &
                  srep_tmp$end[si] >= srep_tmp$start[sr]) {
                end_overlap <- as.numeric(pmin(srep_tmp$end[si], srep_tmp$end[sr]))
                start_overlap <- as.numeric(pmax(srep_tmp$start[si], srep_tmp$start[sr]))
                duration_overlap <- end_overlap - start_overlap
                duration_sib <- as.numeric(srep_tmp$end[si]) - as.numeric(srep_tmp$start[si])
                perc_overlap <- round(100 * (duration_overlap / duration_sib), digits = 1)
                if (srep_tmp$type[sr] == "nonwear") {
                  srep_tmp$SIBoverlapNonwear[si] <- perc_overlap
                } else if (srep_tmp$type[sr] == "nap") {
                  srep_tmp$SIBoverlapNap[si] <- perc_overlap
                } else if (srep_tmp$type[sr] == "sleeplog") {
                  srep_tmp$SIBoverlapSleeplog[si] <- perc_overlap
                }
              }
              # Selfreport behaviour overlap with SIB
              if (srep_tmp$start[sr] <= srep_tmp$end[si] &
                  srep_tmp$end[sr] >= srep_tmp$start[si]) {
                end_overlap <- as.numeric(pmin(srep_tmp$end[si], srep_tmp$end[sr]))
                start_overlap <- as.numeric(pmax(srep_tmp$start[si], srep_tmp$start[sr]))
                duration_overlap <- end_overlap - start_overlap
                duration_sr <- as.numeric(srep_tmp$end[sr]) - as.numeric(srep_tmp$start[sr])
                perc_overlap <- round(100 * (duration_overlap / duration_sr), digits = 1)
                if (srep_tmp$type[sr] == "nonwear") {
                  srep_tmp$NonwearOverlapSIB[sr] <- perc_overlap
                } else if (srep_tmp$type[sr] == "nap") {
                  srep_tmp$NapOverlapSIB[sr] <- perc_overlap
                } else if (srep_tmp$type[sr] == "sleeplog") {
                  srep_tmp$SleeplogOverlapSIB[sr] <- perc_overlap
                }
              }
            }
          }
        }
      }
      # Identify where segments overlap
      sibs_indices <- which(srep_tmp$type == "sib")
      nap_indices <- which(srep_tmp$type == "nap")
      nonwear_indices <- which(srep_tmp$type == "nonwear")
      SIBoverlapNap_indices <- which(srep_tmp$SIBoverlapNap != 0)
      SIBoverlapNonwear_indices <- which(srep_tmp$SIBoverlapNonwear != 0)
      NapOverlapSIB_indices <- which(srep_tmp$NapOverlapSIB != 0)
      NonwearOverlapSIB_indices <- which(srep_tmp$NonwearOverlapSIB != 0)
      # Count number of occurrences (do not count sleeplog because not informative)
      row[c("nbouts_day_denap", "nbouts_day_srnap", "nbouts_day_srnonw",
            "noverl_denap_srnap", "noverl_denap_srnonw",
            "noverl_srnap_denap", "noverl_srnonw_denap")] <-
        list(length(sibs_indices),
             length(nap_indices),
             length(nonwear_indices),
             length(SIBoverlapNap_indices),
             length(SIBoverlapNonwear_indices),
             length(NapOverlapSIB_indices),
             length(NonwearOverlapSIB_indices))
      # mean and total duration in sib per day
      if (length(sibs_indices) > 0) {
        row[c("frag_mean_dur_denap_day", "dur_day_denap_min")] <-
          list(mean(srep_tmp$duration[sibs_indices]),
               sum(srep_tmp$duration[sibs_indices]))
      } else {
        row[c("frag_mean_dur_denap_day", "dur_day_denap_min")] <- list(0, 0)
      }
      # mean and total duration in self-reported naps per day
      if (length(nap_indices) > 0) {
        srep_tmp$duration[nap_indices] <- (as.numeric(srep_tmp$end[nap_indices]) -
                                             as.numeric(srep_tmp$start[nap_indices])) / 60
        row[c("frag_mean_dur_srnap_day", "dur_day_srnap_min")] <-
          list(mean(srep_tmp$duration[nap_indices]),
               sum(srep_tmp$duration[nap_indices]))
      } else {
        row[c("frag_mean_dur_srnap_day", "dur_day_srnap_min")] <- list(0, 0)
      }
      # mean and total duration in self-reported nonwear per day
      if (length(nonwear_indices) > 0) {
        row[c("frag_mean_dur_srnonw_day", "dur_day_srnonw_min")] <-
          list(mean(srep_tmp$duration[nonwear_indices]),
               sum(srep_tmp$duration[nonwear_indices]))
      } else {
        row[c("frag_mean_dur_srnonw_day", "dur_day_srnonw_min")] <- list(0, 0)
      }
      # Self-reported naps
      row <- .raw.timeuse.rest.overlap(
        row,
        srep_tmp,
        X = "SIBoverlapNap",
        Y = "NapOverlapSIB",
        xi = SIBoverlapNap_indices,
        yi = NapOverlapSIB_indices,
        name = "srnap",
        ggir_exact = ggir_exact
      )
      # Self-reported nonwear
      row <- .raw.timeuse.rest.overlap(
        row,
        srep_tmp,
        X = "SIBoverlapNonwear",
        Y = "NonwearOverlapSIB",
        xi = SIBoverlapNonwear_indices,
        yi = NonwearOverlapSIB_indices,
        name = "srnonw",
        ggir_exact = ggir_exact
      )
      rm(srep_tmp)
    }
  }
  invisible(list(row = row, ts = ts))
}
