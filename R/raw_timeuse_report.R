# Ported from GGIR 3.3-9 R/g.report.part5.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The report is computed from part-5 day
# tables passed in as objects, so the folder machinery and the six fwrite calls are gone
# and the three frames of every configuration are returned for write.ggir.milestone() to
# write. The dead loglocation argument is dropped, LC_TIME is forced to "C" for the call
# because every daytype decision is an English weekday-name comparison, and the
# expectedCols failure that makes GGIR's rbind die is reported instead of reproduced.
# tidyup_df, addSplitNames and getSplitNames are in R/raw_sleep_report.R.

# HELPERS

#' Is This Window Type a Day Segment?
#'
#' @details GGIR 3.3.6 asks \code{window == "Segments"} and 3.3-9 asks
#'   \code{length(grep("segment", window)) > 0}; this is the union, so a table produced by
#'   either version takes the arm that version's report would have taken.
#'
#' @param window One window type, for example "MM", "Segments" or "MMsegment".
#' @return TRUE when the window type names day segments.
#' @keywords internal
#' @noRd
.raw.timeuse.is.segment <- function(window) {
  length(grep(pattern = "segment", x = window)) > 0 || any(window == "Segments")
}

#' Collapse the Distinct Window Values to the Report's Window Types
#'
#' @details The 3.3-9 form maps "MMsegment1" and "MMsegment2" onto one "MMsegment" report;
#'   the 3.3.6 form maps every non-MM/WW/OO value onto one "Segments" report.
#'   \code{naming = "auto"} runs the 3.3-9 mapping first and falls back to 3.3.6's lumping
#'   when it matched nothing and unprefixed segment names are present, which is the shape
#'   \code{segment_all_windows = FALSE} produces.
#'
#' @param uwi The distinct window values, as character.
#' @param naming "auto", "prefixed" (3.3-9) or "lumped" (3.3.6).
#' @return The window types to loop over.
#' @keywords internal
#' @noRd
.raw.timeuse.report.windowtypes <- function(uwi, naming = c("auto", "prefixed", "lumped")) {
  naming <- match.arg(naming)
  if (all(uwi %in% c("MM", "WW", "OO"))) return(uwi)
  if (naming == "lumped") {
    return(unique(c(uwi[uwi %in% c("MM", "WW", "OO")], "Segments")))
  }
  hit <- c(grep(pattern = "MMseg", x = uwi), grep(pattern = "WWseg", x = uwi),
           grep(pattern = "OOseg", x = uwi))
  if (naming == "auto" && length(hit) == 0) {
    # 3.3.6-shaped names: lump them
    return(unique(c(uwi[uwi %in% c("MM", "WW", "OO")], "Segments")))
  }
  uwi[grep(pattern = "MMseg", x = uwi)] <- "MMsegment"
  uwi[grep(pattern = "WWseg", x = uwi)] <- "WWsegment"
  uwi[grep(pattern = "OOseg", x = uwi)] <- "OOsegment"
  unique(uwi)
}

#' Which Rows of the Day Table Belong to One Window Type
#'
#' @details For MM, WW and OO an equality test; for a 3.3-9 segment type a grep of the type
#'   against the window column; for the 3.3.6 lumped name "Segments" every row whose window
#'   is not MM, WW or OO.
#'
#' @param window The window column of the stacked table.
#' @param type One window type from \code{\link{.raw.timeuse.report.windowtypes}}.
#' @return A logical vector.
#' @keywords internal
#' @noRd
.raw.timeuse.report.selectwindow <- function(window, type) {
  select_window <- rep(FALSE, length(window))
  if (type %in% c("MM", "WW", "OO")) {
    uwi_available <- which(window == type)
  } else if (type == "Segments") {
    uwi_available <- which(!(as.character(window) %in% c("MM", "WW", "OO")))
  } else {
    uwi_available <- grep(pattern = type, x = window)
  }
  if (length(uwi_available) > 0) select_window[uwi_available] <- TRUE
  select_window
}

# LOADING AND STACKING

#' One Part-5 Day Table, Prepared for Stacking
#'
#' GGIR's \code{myfun} closure, which loads one ms5 milestone and turns it into the character
#' matrix the report stacks.
#'
#' @details \code{lastHour} and \code{lastDate} are added here, taking the table from 119
#'   columns to 121. \code{lastHour} is the local clock hour and \code{lastDate} a UTC date
#'   (\code{as.Date} on a POSIXct defaults to UTC); both are read by the last-window clause
#'   of \code{\link{.raw.timeuse.validwindows}}. \code{as.matrix()} makes every cell a string
#'   and pads the numeric column to a common width, which is why the caller later strips
#'   spaces from \code{window_number}. The \code{expectedCols} merge appends columns rather
#'   than dropping them, and sorts. A missing or non-scalar \code{last_timestamp} is refused
#'   with a message; GGIR's \code{exists()} test is always TRUE and it fails later.
#'
#' @param output One part-5 day table, GGIR's \code{output}.
#' @param last_timestamp The POSIXct of the last epoch of the recording.
#' @param tail_expansion_log The part-3 tail expansion log, or NULL.
#' @param expectedCols The column set of the widest recording in the dataset, or NULL on the
#'   probe pass.
#' @param label A name for this recording, used only in messages.
#' @return A character matrix, or a data frame when the merge ran.
#' @keywords internal
#' @noRd
.raw.timeuse.report.one <- function(output, last_timestamp, tail_expansion_log = NULL,
                                    expectedCols = c(), label = "") {
  output <- as.data.frame(output, stringsAsFactors = FALSE)
  cut <- which(output[, 1] == "")
  if (length(cut) > 0 & length(cut) < nrow(output)) {
    output <- output[-cut, which(colnames(output) != "")]
  }
  if (is.null(last_timestamp) || length(last_timestamp) != 1 || is.na(last_timestamp)) {
    stop("the part-5 result for ", if (nzchar(label)) label else "this recording",
         " carries no last_timestamp. GGIR's own report reaches the same state and dies with ",
         "\"replacement has 0 rows, data has ", nrow(output), "\", because its exists() test ",
         "is always TRUE (g.report.part5.R:152). Re-run part 5 on this ",
         "recording, or supply last_timestamp", call. = FALSE)
  }
  output$lastHour <- as.numeric(format(last_timestamp, "%H"))
  output$lastDate <- as.Date(last_timestamp)
  out <- as.matrix(output)
  if (length(expectedCols) > 0) {
    tmp <- as.data.frame(matrix(0, 0, length(expectedCols)))
    colnames(tmp) <- expectedCols
    out <- base::merge(tmp, out, all = TRUE)
  }
  if (!is.null(expectedCols) && ncol(out) > length(expectedCols)) {
    # GGIR warns and carries on; the caller turns the width mismatch into an error
    warning(paste0("Columns dropped in output part5 for ", label,
                   " because these could not be matched to columns for earlier",
                   " recordings in this dataset. Please check whether these recordings ",
                   " were processed with a",
                   " different GGIR version or configuration. If yes, reprocess",
                   " consistently. If not, ",
                   " consider renaming this file such that it is",
                   " alphabetically first and by that processed first, which",
                   " should address the issue."), call. = FALSE)
  }
  if (length(tail_expansion_log) != 0) {
    col2na <- grep(pattern = paste0("sleep_efficiency|N_atleast5minwakenight|daysleeper|",
                                    "daysleeper|sleeplog_used|_spt_sleep|_spt_wake"),
                   x = names(out), value = FALSE)
    window_number <- as.numeric(out[, "window_number"])
    lastwindow_indices <- which(window_number == max(window_number, na.rm = TRUE))
    if (length(col2na) > 0 & length(lastwindow_indices) > 0) {
      out[lastwindow_indices, col2na] <- "" # set last row to NA for all sleep related variables
    }
  }
  out
}

#' Stack Every Recording's Day Table Into One Frame
#'
#' @details The \code{set.seed(1234)} probe samples at most five recordings to find the
#'   widest column set, so with six or more the widest can be missed and GGIR then fails in
#'   \code{rbind}; \code{sample(x = f0:f1, ...)} with \code{f0 == f1 > 1} also draws from
#'   \code{1:f1}. Both are transcribed. \code{gsub(".RData$", "", filename)} has an unescaped
#'   dot, harmless for "x.gt3x.RData". \code{daytype} compares \code{weekday} against English
#'   names, which is why the entry point forces \code{LC_TIME = "C"}; Saturday and Sunday are
#'   the weekend here, unlike part 4's Friday and Saturday.
#'
#' @param items A list of \code{list(output, last_timestamp, tail_expansion_log, label)}.
#' @param f0,f1 The first and last recording to include, as GGIR's file indices.
#' @return The stacked data frame, 122 columns at the defaults.
#' @keywords internal
#' @noRd
.raw.timeuse.report.stack <- function(items, f0 = 1, f1 = NULL) {
  if (is.null(f1)) f1 <- length(items)
  if (f1 > length(items)) {
    f1 <- length(items)
  }
  myfun <- function(i, expectedCols = c()) {
    .raw.timeuse.report.one(items[[i]]$output, items[[i]]$last_timestamp,
                            items[[i]]$tail_expansion_log, expectedCols,
                            label = items[[i]]$label)
  }
  # probe a random sample for the widest column set
  expectedCols <- NULL
  set.seed(1234)
  testfiles <- unique(sample(x = f0:f1, size = pmin(5, f1 - f0 + 1)))
  for (testf in testfiles) {
    out_try <- myfun(testf)
    if (ncol(out_try) > length(expectedCols)) {
      expectedCols <- colnames(out_try)
    }
  }
  pieces <- lapply(f0:f1, myfun, expectedCols)
  widths <- vapply(pieces, ncol, integer(1))
  if (length(unique(widths)) > 1) {
    # GGIR dies in rbind here; refuse with the cause instead
    idx <- f0:f1
    wide <- which(widths > length(expectedCols))
    stop("the recordings in this dataset do not share a column set: ",
         paste(vapply(wide, function(k) paste0(items[[idx[k]]]$label, " has ", widths[k],
                                               " columns"), character(1)), collapse = ", "),
         " against ", length(expectedCols), " for the rest. GGIR samples at most five ",
         "recordings with set.seed(1234) to find the widest one and cannot see the widest ",
         "when there are more than five (g.report.part5.R:196-207), then fails in rbind. ",
         "Re-run part 5 on every recording with the same configuration, or report them in ",
         "separate calls", call. = FALSE)
  }
  outputfinal <- as.data.frame(do.call(rbind, pieces), stringsAsFactors = FALSE)
  # Find columns filled with missing values
  cut <- which(sapply(outputfinal, function(x) all(x == "")) == TRUE)
  if (length(cut) > 0) {
    outputfinal <- outputfinal[, -cut]
  }
  outputfinal$filename <- gsub(".RData$", "", outputfinal$filename)
  outputfinal$window_number <- as.numeric(gsub(" ", "", outputfinal$window_number))
  outputfinal <- outputfinal[order(outputfinal$filename, outputfinal$window_number,
                                   outputfinal$window), ]
  # replace NaN by empty cell value
  for (kra in 1:ncol(outputfinal)) {
    krad <- which(is.nan(outputfinal[, kra]) == TRUE)
    if (length(krad) > 0) {
      outputfinal[krad, kra] <- ""
    }
  }
  outputfinal$daytype <- 0
  outputfinal$daytype[which(outputfinal$weekday == "Sunday" |
                              outputfinal$weekday == "Saturday")] <- "WE"
  outputfinal$daytype[which(outputfinal$weekday == "Monday" |
                              outputfinal$weekday == "Tuesday" |
                              outputfinal$weekday == "Wednesday" |
                              outputfinal$weekday == "Thursday" |
                              outputfinal$weekday == "Friday")] <- "WD"
  outputfinal$nonwear_perc_day <- as.numeric(outputfinal$nonwear_perc_day)
  outputfinal$nonwear_perc_spt <- as.numeric(outputfinal$nonwear_perc_spt)
  outputfinal$dur_spt_min <- as.numeric(outputfinal$dur_spt_min)
  outputfinal$dur_day_min <- as.numeric(outputfinal$dur_day_min)
  outputfinal$guider <- as.character(outputfinal$guider)
  outputfinal$sleeplog_used <- as.numeric(outputfinal$sleeplog_used)
  outputfinal$dur_spt_min <- as.numeric(outputfinal$dur_spt_min)
  outputfinal$dur_day_min <- as.numeric(outputfinal$dur_day_min)
  outputfinal$dur_day_spt_min <- as.numeric(outputfinal$dur_day_spt_min)
  outputfinal
}

# DAY INCLUSION

#' Which Windows of a Day Table Meet the Inclusion Criteria
#'
#' GGIR's \code{getValidDayIndices}, the last of part 5's three validity filters. The other
#' two are inside \code{\link{raw.timeuse}}: a window whose midnight was trimmed, or which
#' is 900 seconds or shorter, never reaches the day table.
#'
#' @details \code{includedaycrit.part5} and \code{includenightcrit.part5} each have two
#'   regimes: a value in 0 to 1 is a wear fraction and becomes a percentage floor, a value
#'   above 1 and up to 25 is a number of hours and becomes an absolute minute floor.
#'   \code{minimumValidMinutesMM} is 0 unless \code{includedaycrit} (the part 2 and 4
#'   parameter) has length 2, in which case its second element times 60 is an extra floor
#'   on \code{wear_min_day_spt} for every window type. The last-window clause is vacuous
#'   unless \code{require_complete_lastnight_part5} is TRUE, and then
#'   \code{max(window_number)} is taken over all rows of the table. Segment rows take a
#'   different branch: a non-wear ceiling and the two day-over-window and spt-over-window
#'   ratios, with no \code{data_cleaning_file}. \code{excludefirstlast.part5} runs after
#'   either branch. The function is called twice per configuration, on the rounded table for
#'   the day csv and on the unrounded one for the person summary, so both index vectors are
#'   kept.
#'
#' @param x A day table: the stacked frame reduced to one configuration, rounded or not.
#' @param window The window type, "MM", "WW", "OO", "Segments" or "MMsegment".
#' @param includedaycrit.part5,includenightcrit.part5 The two wear criteria, each a fraction
#'   in 0 to 1 or a number of hours above 1.
#' @param minimum_MM_length.part5 Hours a MM window must span.
#' @param includedaycrit The part 2 and 4 criterion; only its second element is read, as the
#'   minimum number of valid hours over the whole window.
#' @param data_cleaning_file A path or an already-parsed frame with ID and day_part5 columns,
#'   or NULL.
#' @param excludefirstlast.part5 Drop each recording's first and last window.
#' @param segmentWEARcrit.part5,segmentDAYSPTcrit.part5 The segment-row criteria.
#' @param require_complete_lastnight_part5 Apply the last-window clause.
#' @param detail Attach the per-clause logical matrix as the "clauses" attribute.
#' @return The row indices that meet the criteria, in increasing order.
#' @keywords internal
#' @noRd
.raw.timeuse.validwindows <- function(x, window,
                                      includedaycrit.part5 = 2 / 3,
                                      includenightcrit.part5 = 0,
                                      minimum_MM_length.part5 = 23,
                                      includedaycrit = 16,
                                      data_cleaning_file = NULL,
                                      excludefirstlast.part5 = FALSE,
                                      segmentWEARcrit.part5 = 0.5,
                                      segmentDAYSPTcrit.part5 = c(0.9, 0),
                                      require_complete_lastnight_part5 = FALSE,
                                      detail = FALSE) {
  window_is_segment <- .raw.timeuse.is.segment(window)
  clauses <- list()
  if (window_is_segment == FALSE) {
    includeday_wearPercentage <- includeday_absolute <- NULL
    if (includedaycrit.part5 >= 0 & includedaycrit.part5 <= 1) {
      # if includedaycrit.part5 is used as a ratio
      includeday_wearPercentage <- includedaycrit.part5 * 100
      includeday_absolute <- 0
    } else if (includedaycrit.part5 > 1 & includedaycrit.part5 <= 25) {
      # if includedaycrit.part5 is used like includedaycrit, as a number of hours
      includeday_wearPercentage <- 0
      includeday_absolute <- includedaycrit.part5 * 60
    }
    includenight_wearPercentage <- includenight_absolute <- NULL
    if (includenightcrit.part5 >= 0 & includenightcrit.part5 <= 1) {
      includenight_wearPercentage <- includenightcrit.part5 * 100
      includenight_absolute <- 0
    } else if (includenightcrit.part5 > 1 & includenightcrit.part5 <= 25) {
      includenight_wearPercentage <- 0
      includenight_absolute <- includenightcrit.part5 * 60
    }
    if (is.null(includeday_wearPercentage) || is.null(includenight_wearPercentage)) {
      # GGIR has no else here and fails on first use
      stop("includedaycrit.part5 and includenightcrit.part5 must each be a wear fraction in ",
           "0 to 1 or a number of hours above 1 and at most 25; GGIR leaves the thresholds ",
           "undefined outside those two regimes (g.report.part5.R:20-37)", call. = FALSE)
    }
    include_window <- rep(TRUE, nrow(x))
    if (length(data_cleaning_file) > 0) {
      # allow for forced relying on guider based on an external data_cleaning_file
      DaCleanFile <- .raw.timeuse.cleaning.file(data_cleaning_file)
      days2exclude <- which(paste(x$ID, x$window_number) %in%
                              paste(DaCleanFile$ID, DaCleanFile$day_part5))
      if (length(days2exclude) > 0) {
        include_window[days2exclude] <- FALSE
      }
    } else {
      include_window <- rep(TRUE, nrow(x))
    }
    # only daytime gets a wear criterion; the night only needs the SPT edges
    x$nonwear_perc_day_spt <- as.numeric(x$nonwear_perc_day_spt)
    x$nonwear_perc_day <- as.numeric(x$nonwear_perc_day)
    x$nonwear_perc_spt <- as.numeric(x$nonwear_perc_spt)
    x$wear_min_day_spt <- (1 - (x$nonwear_perc_day_spt / 100)) * x$dur_day_spt_min
    x$wear_min_day <- (1 - (x$nonwear_perc_day / 100)) * x$dur_day_min
    x$wear_perc_day <- 100 - x$nonwear_perc_day
    x$wear_min_spt <- (1 - (x$nonwear_perc_spt / 100)) * x$dur_spt_min
    x$wear_perc_spt <- 100 - x$nonwear_perc_spt
    x$lastHour <- as.numeric(x$lastHour)
    x$calendar_date <- as.Date(x$calendar_date)
    minimumValidMinutesMM <- 0
    if (length(includedaycrit) == 2) {
      minimumValidMinutesMM <- includedaycrit[2] * 60
    }
    if (require_complete_lastnight_part5 == FALSE) {
      x$lastWindow <- FALSE
      x$lastDate <- x$calendar_date
    } else {
      x$lastWindow <- x$window_number == max(x$window_number)
      x$lastDate <- as.Date(x$lastDate)
    }
    if (window == "WW" | window == "OO") {
      clauses <- list(
        wear_perc_day = x$wear_perc_day >= includeday_wearPercentage,
        wear_min_day = x$wear_min_day >= includeday_absolute,
        wear_perc_spt = x$wear_perc_spt >= includenight_wearPercentage,
        wear_min_spt = x$wear_min_spt >= includenight_absolute,
        dur_spt_min = x$dur_spt_min > 0,
        dur_day_min = x$dur_day_min > 0,
        lastWindow = ((x$lastWindow == TRUE & x$lastHour >= 15 &
                         (x$lastDate - x$calendar_date) >= -1 & window == "WW") |
                        (x$lastWindow == TRUE & x$lastHour >= 9 &
                           (x$lastDate - x$calendar_date) >= -1 & window == "OO") |
                        x$lastWindow == FALSE),
        include_window = include_window == TRUE,
        wear_min_day_spt = x$wear_min_day_spt >= minimumValidMinutesMM)
    } else if (window == "MM") {
      clauses <- list(
        wear_perc_day = x$wear_perc_day >= includeday_wearPercentage,
        wear_min_day = x$wear_min_day >= includeday_absolute,
        wear_perc_spt = x$wear_perc_spt >= includenight_wearPercentage,
        wear_min_spt = x$wear_min_spt >= includenight_absolute,
        dur_spt_min = x$dur_spt_min > 0,
        dur_day_min = x$dur_day_min > 0,
        lastWindow = ((x$lastWindow == TRUE & x$lastHour > 9 &
                         (x$lastDate - x$calendar_date) >= -1) | x$lastWindow == FALSE),
        minimum_MM_length = x$dur_day_spt_min >= (minimum_MM_length.part5 * 60),
        include_window = include_window == TRUE,
        wear_min_day_spt = x$wear_min_day_spt >= minimumValidMinutesMM)
    } else {
      # GGIR leaves indices undefined here
      stop("window must be MM, WW, OO or a day-segment type, not \"", window, "\"",
           call. = FALSE)
    }
  } else if (window_is_segment) {
    # a segment can be valid inside an invalid day
    maxpernwday <- 100 - (segmentWEARcrit.part5 * 100)
    include_window <- rep(TRUE, nrow(x))
    clauses <- list(
      nonwear_perc_day_spt = as.numeric(x$nonwear_perc_day_spt) <= maxpernwday,
      segmentDAYcrit = as.numeric(x$dur_day_min) / as.numeric(x$dur_day_spt_min) >=
        segmentDAYSPTcrit.part5[1],
      segmentSPTcrit = as.numeric(x$dur_spt_min) / as.numeric(x$dur_day_spt_min) >=
        segmentDAYSPTcrit.part5[2],
      include_window = include_window == TRUE)
  }
  # GGIR's & chain, kept as a named list so the reason a window was dropped can be named
  indices <- which(Reduce("&", clauses))
  if (excludefirstlast.part5 == TRUE) {
    x$window_number <- as.numeric(x$window_number)
    first_days <- stats::aggregate(window_number ~ filename, data = x, FUN = min, na.rm = TRUE)
    last_days <- stats::aggregate(window_number ~ filename, data = x, FUN = max, na.rm = TRUE)
    exclude_firsts <- which(paste(x$filename, x$window_number) %in%
                              paste(first_days$filename, first_days$window_number))
    exclude_lasts <- which(paste(x$filename, x$window_number) %in%
                             paste(last_days$filename, last_days$window_number))
    indices2exclude <- which(indices %in% c(exclude_firsts, exclude_lasts))
    if (length(indices2exclude) > 0) indices <- indices[-indices2exclude]
  }
  if (detail == TRUE) {
    m <- matrix(unlist(clauses, use.names = FALSE), nrow = nrow(x),
                dimnames = list(NULL, names(clauses)))
    attr(indices, "clauses") <- m
  }
  indices
}

#' Why Each Window Was Kept or Dropped
#'
#' A canhrActi addition: GGIR decides inclusion inside \code{getValidDayIndices} and records
#' nothing.
#'
#' @param x The day table the decision was taken on.
#' @param indices The result of \code{\link{.raw.timeuse.validwindows}} with
#'   \code{detail = TRUE}.
#' @param window The window type.
#' @return A data frame with one row per window: the identifying columns, the row's own
#'   window value in \code{segment} when the report is a segment report, a logical
#'   \code{valid}, and \code{reason}, the names of the clauses that were not TRUE.
#' @keywords internal
#' @noRd
.raw.timeuse.report.reasons <- function(x, indices, window) {
  m <- attr(indices, "clauses")
  n <- nrow(x)
  valid <- rep(FALSE, n)
  valid[indices] <- TRUE
  reason <- rep("", n)
  if (!is.null(m)) {
    for (i in seq_len(n)) {
      bad <- colnames(m)[which(!(m[i, ] %in% TRUE))]
      if (length(bad) > 0) reason[i] <- paste(bad, collapse = ", ")
    }
  }
  # valid by every clause but outside indices: removed by excludefirstlast
  removed <- which(valid == FALSE & reason == "")
  if (length(removed) > 0) reason[removed] <- "excludefirstlast.part5"
  data.frame(ID = as.character(x$ID), filename = as.character(x$filename),
             window = window,
             # a segment report keeps the row's own window value; MM, WW and OO do not
             segment = if ("window" %in% names(x)) as.character(x$window) else NA_character_,
             window_number = suppressWarnings(as.numeric(as.character(x$window_number))),
             calendar_date = as.character(x$calendar_date),
             weekday = as.character(x$weekday),
             daytype = as.character(x$daytype),
             valid = valid, reason = reason, stringsAsFactors = FALSE)
}

# PERSON SUMMARY

#' Add the Lux Segment Columns a Recording Did Not Produce
#'
#' @details GGIR's nested \code{add_missing_LUX}. It fires only when between 1 and 23 lux
#'   segment columns are present and \code{LUX_day_segments} is set, which needs a light
#'   channel. GGIR computes \code{neworder} and never uses it; the re-order is by the expected
#'   names.
#'
#' @param x The frame to widen.
#' @param LUX_day_segments The segment boundaries in hours.
#' @param weeksegment "WD", "WE" or empty.
#' @param LUXmetrics The five metric names.
#' @return The frame, widened and re-ordered.
#' @keywords internal
#' @noRd
.raw.timeuse.report.addlux <- function(x, LUX_day_segments, weeksegment = c(), LUXmetrics) {
  NLUXseg <- length(LUX_day_segments)
  if (length(weeksegment) > 0) {
    LUX_segment_vars_expected <- paste0("LUX_", LUXmetrics, "_",
                                        LUX_day_segments[1:(NLUXseg - 1)],
                                        "-", LUX_day_segments[2:(NLUXseg)],
                                        "hr_day_", weeksegment)
  } else {
    luxvars <- paste0("LUX_", LUXmetrics, "_")
    segments <- paste0(LUX_day_segments[1:(NLUXseg - 1)],
                       "_", LUX_day_segments[2:(NLUXseg)], "hr_day")
    LUX_segment_vars_expected <- c()
    for (luxi in 1:length(luxvars)) {
      LUX_segment_vars_expected <- c(LUX_segment_vars_expected,
                                     paste0(luxvars[luxi], segments))
    }
  }
  dummy_df <- as.data.frame(matrix(NaN, 1, length(LUX_segment_vars_expected)))
  colnames(dummy_df) <- LUX_segment_vars_expected
  if (length(which(LUX_segment_vars_expected %in% colnames(x))) > 0) {
    x <- as.data.frame(merge(x, dummy_df, all.x = TRUE))
    current_location <- which(colnames(x) %in% LUX_segment_vars_expected == TRUE)
    x <- cbind(x[, -current_location], x[, LUX_segment_vars_expected])
  }
  x
}

#' The Grouped Weighted Mean of Every Numeric Column
#'
#' @details GGIR runs \code{dt[, lapply(.SD, weighted.mean, w = len, na.rm = TRUE), by]} on
#'   a data.table; data.table is only in Suggests, so the same grouping is done in base R:
#'   groups in order of first appearance, rows in original order, the value from
#'   \code{stats::weighted.mean} itself, so the result is bit identical. \code{len} is itself
#'   one of the columns averaged, which is where the person summary's extra \code{len} column
#'   comes from.
#'
#' @param dt The frame of the grouping columns plus every numeric column, including len.
#' @param by_cols The grouping column names, "filename" or c("filename", "window").
#' @return A data frame: the grouping columns then every other column, one row per group.
#' @keywords internal
#' @noRd
.raw.timeuse.report.weighted <- function(dt, by_cols) {
  key <- do.call(paste, c(unname(as.list(dt[, by_cols, drop = FALSE])), list(sep = "\r")))
  ukey <- unique(key)
  value_cols <- setdiff(names(dt), by_cols)
  out <- dt[match(ukey, key), by_cols, drop = FALSE]
  row.names(out) <- NULL
  w_all <- dt[["len"]]
  for (cn in value_cols) {
    v <- dt[[cn]]
    out[[cn]] <- vapply(ukey, function(k) {
      i <- which(key == k)
      suppressWarnings(stats::weighted.mean(v[i], w = w_all[i], na.rm = TRUE))
    }, numeric(1), USE.NAMES = FALSE)
  }
  out
}

#' Collapse a Day Table to One Row Per Recording
#'
#' GGIR's \code{agg_plainNweighted}: the plain average and the weekday-weighted average of
#' every numeric variable, merged side by side.
#'
#' @details The numeric coercion is trialled on the first 30 rows only, so a column empty on
#'   the first 30 days and populated later stays character. \code{plain_mean} falls back to
#'   \code{x[1]} when \code{mean} returns NA, which is how ID, weekday, calendar_date and the
#'   parameter echoes survive unchanged. A weekday weighs 5 and a weekend day 2; a row whose
#'   daytype is neither weighs 0 and makes every weighted column NaN. The merge decides the
#'   person summary's width and that width is data dependent: only columns numeric in both
#'   frames get a \code{_pla} and a \code{_wei} twin, a column empty on every retained day
#'   appears once under its plain name. GGIR's \code{LUX_segment_vars} loop overwrites its
#'   accumulator on every iteration, so only the last metric's mask survives; transcribed.
#'
#' @param df The day table of one configuration, already reduced to the valid windows.
#' @param validdaysi Row indices to keep, or NULL when \code{df} is already filtered.
#' @param filename The name of the identifier column, always "filename".
#' @param daytype The name of the day-type column, always "daytype".
#' @param window The window type.
#' @param week_weekend_aggregate.part5 Add the WD and WE column blocks.
#' @param LUX_day_segments The lux segment boundaries, or NULL.
#' @return One row per recording (per recording and segment for a segment window type).
#' @keywords internal
#' @noRd
.raw.timeuse.person.row <- function(df, validdaysi = NULL, filename = "filename",
                                    daytype = "daytype", window = "MM",
                                    week_weekend_aggregate.part5 = FALSE,
                                    LUX_day_segments = c()) {
  if (!is.null(validdaysi)) df <- df[validdaysi, ]
  ignorevar <- c("daysleeper", "cleaningcode", "night_number", "sleeplog_used",
                 "ID", "acc_available", "window_number",
                 "boutcriter.mvpa", "boutcriter.lig", "boutcriter.in", "bout.metric")
  for (ee in 1:ncol(df)) { # make sure that numeric columns have class numeric
    nr <- nrow(df)
    if (nr > 30) nr <- 30
    trynum <- suppressWarnings(as.numeric(as.character(df[1:nr, ee])))
    if (length(which(is.na(trynum) == TRUE)) != nr &
        length(which(ignorevar == names(df)[ee])) == 0) {
      suppressWarnings(class(df[, ee]) <- "numeric")
    }
  }
  plain_mean <- function(x) {
    plain_mean <- suppressWarnings(mean(x, na.rm = TRUE))
    if (is.na(plain_mean) == TRUE) {
      plain_mean <- x[1]
    }
    return(plain_mean)
  }
  # aggregate across all days
  window_is_segment <- .raw.timeuse.is.segment(window)
  if (window_is_segment) {
    by <- list(df$filename, df$window)
  } else {
    by <- list(df$filename)
  }
  PlainAggregate <- stats::aggregate.data.frame(df, by = by, FUN = plain_mean)
  PlainAggregate <- PlainAggregate[, -grep("^Group", colnames(PlainAggregate))]
  # aggregate per day type (weekday or weekend days)
  if (window_is_segment) {
    by <- list(df$filename, df$window, df$daytype)
  } else {
    by <- list(df$filename, df$daytype)
  }
  AggregateWDWE <- stats::aggregate.data.frame(df, by = by, plain_mean)
  AggregateWDWE <- AggregateWDWE[, -grep("^Group", colnames(AggregateWDWE))]
  # count days for the fragmentation variables, which drop days with too few fragments
  vars_with_mininum_Nfrag <- c("FRAG_Gini_dur_PA_day", "FRAG_CoV_dur_PA_day",
                               "FRAG_alpha_dur_PA_day", "FRAG_Gini_dur_IN_day",
                               "FRAG_CoV_dur_IN_day")
  vars_with_mininum_Nfrag_i <- which(vars_with_mininum_Nfrag %in% colnames(df) == TRUE)
  if (length(vars_with_mininum_Nfrag_i) > 0) {
    varname_minfrag <- vars_with_mininum_Nfrag[vars_with_mininum_Nfrag_i[1]]
    DAYCOUNT_Frag_Multiclass <- stats::aggregate.data.frame(
      df[, varname_minfrag], by = by,
      FUN = function(x) length(which(is.na(x) == FALSE)))
    if (window_is_segment == FALSE) {
      colnames(DAYCOUNT_Frag_Multiclass)[1:2] <- c("filename", "daytype")
      # AL10F, abbreviation for: at least 10 fragments
      colnames(DAYCOUNT_Frag_Multiclass)[3] <- "Nvaliddays_AL10F"
      by.x <- c("filename", "daytype")
    } else if (window_is_segment) {
      colnames(DAYCOUNT_Frag_Multiclass)[1:3] <- c("filename", "window", "daytype")
      colnames(DAYCOUNT_Frag_Multiclass)[4] <- "Nvalidsegments_AL10F"
      by.x <- c("filename", "window", "daytype")
    }
    # by.y is not given, as in GGIR; the intersect fallback is the same set
    AggregateWDWE <- merge(AggregateWDWE, DAYCOUNT_Frag_Multiclass, by.x = by.x)
  }
  AggregateWDWE$len <- 0
  AggregateWDWE$len[which(as.character(AggregateWDWE$daytype) == "WD")] <- 5 # weekdays
  AggregateWDWE$len[which(as.character(AggregateWDWE$daytype) == "WE")] <- 2 # weekend days
  if (window_is_segment) {
    dt <- AggregateWDWE[, which(lapply(AggregateWDWE, class) == "numeric" |
                                  names(AggregateWDWE) == filename |
                                  names(AggregateWDWE) == "window")]
    WeightedAggregate <- .raw.timeuse.report.weighted(dt, c(filename, "window"))
  } else {
    dt <- AggregateWDWE[, which(lapply(AggregateWDWE, class) == "numeric" |
                                  names(AggregateWDWE) == filename)]
    WeightedAggregate <- .raw.timeuse.report.weighted(dt, filename)
  }
  LUXmetrics <- c("above1000", "timeawake", "mean", "imputed", "ignored")
  LUX_segment_vars <- c()
  for (li in 1:length(LUXmetrics)) {
    LUX_segment_vars <- c(LUX_segment_vars,
                          grep(pattern = paste0("LUX_", LUXmetrics[li]),
                               x = colnames(WeightedAggregate), value = TRUE))
  }
  if (length(LUX_segment_vars) > 0 &
      length(LUX_segment_vars) < 24 &
      length(LUX_day_segments) > 0) {
    WeightedAggregate <- .raw.timeuse.report.addlux(WeightedAggregate, LUX_day_segments,
                                                    weeksegment = c(),
                                                    LUXmetrics = LUXmetrics)
  }
  # merge them into one output data frame (G)
  LUX_segment_vars <- c()
  for (li in 1:length(LUXmetrics)) {
    # assigns rather than appends, as in GGIR
    LUX_segment_vars <- colnames(PlainAggregate) %in%
      grep(x = colnames(PlainAggregate), pattern = paste0("LUX_", LUXmetrics[li]),
           value = TRUE)
  }
  charcol <- which(lapply(PlainAggregate, class) != "numeric" &
                     names(PlainAggregate) != filename & !(LUX_segment_vars))
  numcol <- which(lapply(PlainAggregate, class) == "numeric" | LUX_segment_vars)
  # keep the split that decided the width; the WD/WE block below reassigns both names
  charcol_names <- names(PlainAggregate)[charcol]
  numcol_names <- names(PlainAggregate)[numcol]
  WeightedAggregate <- as.data.frame(WeightedAggregate, stringsAsFactors = TRUE)
  if (window_is_segment) {
    by <- c("filename", "window")
  } else {
    by <- "filename"
  }
  G <- base::merge(PlainAggregate, WeightedAggregate, by = by, all.x = TRUE)
  p0b <- paste0(names(PlainAggregate[, charcol]), ".x")
  p1 <- paste0(names(PlainAggregate[, numcol]), ".x")
  p2 <- paste0(names(PlainAggregate[, numcol]), ".y")
  for (i in 1:length(p0b)) {
    names(G)[which(names(G) == p0b[i])] <- paste0(names(PlainAggregate[, charcol])[i])
  }
  for (i in 1:length(p1)) {
    names(G)[which(names(G) == p1[i])] <- paste0(names(PlainAggregate[, numcol])[i], "_pla")
  }
  for (i in 1:length(p2)) {
    names(G)[which(names(G) == p2[i])] <- paste0(names(PlainAggregate[, numcol])[i], "_wei")
  }
  # expand output with weekday (WD) and weekend (WE) day aggregates
  if (week_weekend_aggregate.part5 == TRUE) {
    for (weeksegment in c("WD", "WE")) {
      temp_aggregate <- AggregateWDWE[which(AggregateWDWE$daytype == weeksegment), ]
      charcol <- which(lapply(temp_aggregate, class) != "numeric" &
                         names(temp_aggregate) != filename)
      numcol <- which(lapply(temp_aggregate, class) %in% c("numeric", "integer") == TRUE)
      names(temp_aggregate)[numcol] <- paste0(names(temp_aggregate)[numcol], "_", weeksegment)
      if (window_is_segment) {
        temp_aggregate <- temp_aggregate[, c(which(colnames(temp_aggregate) == "filename" |
                                                     colnames(temp_aggregate) == "window"),
                                             numcol)]
      } else {
        temp_aggregate <- temp_aggregate[, c(which(colnames(temp_aggregate) == "filename"),
                                             numcol)]
      }
      LUX_segment_vars <- c()
      for (li in 1:length(LUXmetrics)) {
        LUX_segment_vars <- grep(pattern = paste0("LUX_", LUXmetrics[li]),
                                 x = colnames(temp_aggregate), value = TRUE)
      }
      if (length(LUX_segment_vars) > 0 &
          length(LUX_segment_vars) < 24 & length(LUX_day_segments) > 0) {
        temp_aggregate <- .raw.timeuse.report.addlux(temp_aggregate, LUX_day_segments,
                                                     weeksegment, LUXmetrics)
      }
      G <- base::merge(G, temp_aggregate, by = by, all.x = TRUE)
    }
  }
  G <- G[, -which(names(G) %in% c("len", "daytype", "len_WE", "len_WD"))]
  attr(G, "charcol") <- charcol_names
  attr(G, "numcol") <- numcol_names
  G
}

# DAY COUNTS

#' Count the Days of One Recording That Meet One Criterion
#'
#' @details GGIR's nested \code{foo34}. The criterion is \code{length(which(x == cval))}
#'   applied to every column by \code{aggregate.data.frame}, of which only \code{nameold} is
#'   read back.
#'
#' @param df The day table, or the valid rows of it.
#' @param aggPerIndividual The person summary being grown.
#' @param nameold The column to count in.
#' @param namenew The name of the new count column.
#' @param cval The value to count.
#' @param window The window type.
#' @return \code{aggPerIndividual} with one more column.
#' @keywords internal
#' @noRd
.raw.timeuse.report.count <- function(df, aggPerIndividual, nameold, namenew, cval, window) {
  df2 <- function(x) df2 <- length(which(x == cval))
  window_is_segment <- .raw.timeuse.is.segment(window)
  if (window_is_segment) {
    by <- list(df$filename, df$window)
  } else {
    by <- list(df$filename)
  }
  mmm <- as.data.frame(stats::aggregate.data.frame(df, by = by, FUN = df2),
                       stringsAsFactors = TRUE)
  mmm2 <- data.frame(filename = mmm$Group.1, cc = mmm[, nameold], stringsAsFactors = TRUE)
  if (window_is_segment) {
    mmm2$window <- mmm$Group.2
    by <- c("filename", "window")
  } else {
    by <- "filename"
  }
  aggPerIndividual <- merge(aggPerIndividual, mmm2, by = by)
  names(aggPerIndividual)[which(names(aggPerIndividual) == "cc")] <- namenew
  aggPerIndividual
}

#' Add the Day-Count Columns to a Person Summary
#'
#' @details The thirteen counts, the optional fourteenth for a marker-button guider, the
#'   reorder that hard-codes eleven appended columns, the eleven-name cut list, the rename of
#'   \code{weekday} to \code{startday}, the NA to empty sweep and the move of the
#'   \code{Nvaliddays} columns to the front. Two GGIR defects are reproduced: the
#'   \code{sleeplog_used} count compares a numeric column against TRUE, and the marker-button
#'   flag is written with \code{validdaysi}-relative indices into the full-length vector, so
#'   it lands on the wrong rows whenever \code{validdaysi} is not \code{1:nrow}.
#'
#' @param OF3tmp The twelve-column subset of the unrounded, unfiltered day table.
#' @param OF4 The person summary from \code{\link{.raw.timeuse.person.row}}.
#' @param validdaysi The valid row indices of the same table.
#' @param window The window type.
#' @param guider The guider column of the unfiltered day table, for the marker-button test.
#' @return The person summary with the counts added and the columns reordered and cut.
#' @keywords internal
#' @noRd
.raw.timeuse.daycounts <- function(OF3tmp, OF4, validdaysi, window, guider = character(0)) {
  window_is_segment <- .raw.timeuse.is.segment(window)
  foo34 <- .raw.timeuse.report.count
  # calculate number of valid days (both night and day criteria met)
  OF3tmp$validdays <- 0
  OF3tmp$validdays[validdaysi] <- 1
  namenew <- "Nvaliddays"
  if (window_is_segment) namenew <- "Nvalidsegments"
  OF4 <- foo34(df = OF3tmp, aggPerIndividual = OF4, nameold = "validdays",
               namenew = namenew, cval = 1, window = window)
  # do the same for WE (weekend days)
  OF3tmp$validdays <- 0
  OF3tmp$validdays[validdaysi[which(OF3tmp$daytype[validdaysi] == "WE")]] <- 1
  namenew <- "Nvaliddays_WE"
  if (window_is_segment) namenew <- "Nvalidsegments_WE"
  OF4 <- foo34(df = OF3tmp, aggPerIndividual = OF4, nameold = "validdays",
               namenew = namenew, cval = 1, window = window)
  # do the same for WD (weekdays)
  OF3tmp$validdays <- 0
  OF3tmp$validdays[validdaysi[which(OF3tmp$daytype[validdaysi] == "WD")]] <- 1
  namenew <- "Nvaliddays_WD"
  if (window_is_segment) namenew <- "Nvalidsegments_WD"
  OF4 <- foo34(df = OF3tmp, aggPerIndividual = OF4, nameold = "validdays",
               namenew = namenew, cval = 1, window = window)
  # do the same for daysleeper, cleaningcode, sleeplog_used and acc_available
  OF3tmp$validdays <- 1
  OF4 <- foo34(df = OF3tmp[validdaysi, ], aggPerIndividual = OF4, nameold = "daysleeper",
               namenew = "Ndaysleeper", cval = 1, window = window)
  OF4 <- foo34(df = OF3tmp[validdaysi, ], aggPerIndividual = OF4, nameold = "cleaningcode",
               namenew = "Ncleaningcodezero", cval = 0, window = window)
  for (ccode in 1:6) {
    OF4 <- foo34(df = OF3tmp[validdaysi, ], aggPerIndividual = OF4, nameold = "cleaningcode",
                 namenew = paste0("Ncleaningcode", ccode), cval = ccode, window = window)
  }
  OF4 <- foo34(df = OF3tmp[validdaysi, ], aggPerIndividual = OF4, nameold = "sleeplog_used",
               namenew = "Nsleeplog_used", cval = TRUE, window = window)
  OF4 <- foo34(df = OF3tmp[validdaysi, ], aggPerIndividual = OF4, nameold = "acc_available",
               namenew = "Nacc_available", cval = 1, window = window)
  if ("markerbutton" %in% guider) {
    OF3tmp$markerbutton_guider <- 0
    OF3tmp$markerbutton_guider[which(OF3tmp$guider[validdaysi] == "markerbutton")] <- 1
    OF4 <- foo34(df = OF3tmp[validdaysi, ], aggPerIndividual = OF4,
                 nameold = "markerbutton_guider", namenew = "Nmarkerbutton_used",
                 cval = 1, window = window)
  }
  # Move valid day count variables to the beginning of the data frame
  OF4 <- cbind(OF4[, 1:5],
               OF4[, (ncol(OF4) - 10):ncol(OF4)],
               OF4[, 6:(ncol(OF4) - 11)])
  nom <- names(OF4)
  cut <- which(nom == "sleeponset_ts" | nom == "wakeup_ts"
               | nom == "night_number" | nom == "window_number"
               | nom == "daysleeper" | nom == "cleaningcode"
               | nom == "acc_available"
               | nom == "guider" | nom == "L5TIME" | nom == "M5TIME"
               | nom == "L10TIME" | nom == "M10TIME"
               | nom == "acc_available" | nom == "daytype"
               | nom %in% paste0("sleeplog_used_", c("pla", "wei", "WD", "WE"))
               | nom %in% paste0("window_number_", c("pla", "wei", "WD", "WE")))
  names(OF4)[which(names(OF4) == "weekday")] <- "startday"
  OF4 <- OF4[, -cut]
  for (col4 in 1:ncol(OF4)) {
    navalues <- which(is.na(OF4[, col4]) == TRUE)
    if (length(navalues) > 0) {
      OF4[navalues, col4] <- ""
    }
  }
  # Move the Nvaliddays variables to the front of the spreadsheet
  Nvaliddays_variables <- grep(x = colnames(OF4), pattern = "Nvaliddays", value = FALSE)
  Nvaliddays_variables <- unique(c(which(colnames(OF4) == "Nvaliddays"),
                                   which(colnames(OF4) == "Nvaliddays_WD"),
                                   which(colnames(OF4) == "Nvaliddays_WE"),
                                   Nvaliddays_variables))
  OF4 <- OF4[, unique(c(1:4, Nvaliddays_variables, 5:ncol(OF4)))]
  OF4
}

# NATIVE DAY TABLE

#' Add Unambiguous Cut-Point Totals to a Day Table
#'
#' A canhrActi addition. The nine columns are taken from the four \code{dur_day_total_*}
#' classes and the sleep period: the four waking totals sum to \code{dur_day_min} and the
#' two sleep-period columns to \code{dur_spt_min}, exactly, on every row.
#'
#' @section Warning:
#'   Do not add the \code{minutes_*_cutpoint} columns to GGIR's \code{dur_day_*} columns.
#'   Total and unbouted classes do not nest: a bout absorbs gaps of up to 60 seconds at a
#'   different intensity, so \code{dur_day_IN_unbt_min} plus the three IN bout classes is not
#'   \code{dur_day_total_IN_min}.
#'
#' @param df A cleaned day table, as returned in \code{daysummary_cleaned}, already rounded.
#' @param source The same rows of the unrounded table, or NULL. The two derived columns are
#'   summed there and rounded once, so a sum of rounded parts cannot miss the rounded whole.
#' @return The same frame with nine numeric columns appended.
#' @keywords internal
#' @noRd
.raw.timeuse.native.totals <- function(df, source = NULL) {
  if (!is.null(source) && nrow(source) != nrow(df)) source <- NULL
  num <- function(nm, from = df) {
    if (!(nm %in% names(from))) return(rep(NA_real_, nrow(from)))
    suppressWarnings(as.numeric(as.character(from[[nm]])))
  }
  raw <- if (is.null(source)) df else source
  spt_wake <- round(num("dur_spt_wake_IN_min", raw) + num("dur_spt_wake_LIG_min", raw) +
                      num("dur_spt_wake_MOD_min", raw) + num("dur_spt_wake_VIG_min", raw), 3)
  mvpa <- round(num("dur_day_total_MOD_min", raw) + num("dur_day_total_VIG_min", raw), 3)
  df$minutes_waking_cutpoint <- num("dur_day_min")
  df$minutes_sedentary_cutpoint <- num("dur_day_total_IN_min")
  df$minutes_light_cutpoint <- num("dur_day_total_LIG_min")
  df$minutes_moderate_cutpoint <- num("dur_day_total_MOD_min")
  df$minutes_vigorous_cutpoint <- num("dur_day_total_VIG_min")
  df$minutes_mvpa_cutpoint <- mvpa
  df$minutes_sleepperiod_cutpoint <- num("dur_spt_min")
  df$minutes_sleepperiod_sleep_cutpoint <- num("dur_spt_sleep_min")
  df$minutes_sleepperiod_wake_cutpoint <- spt_wake
  df
}

# INPUT RESOLUTION

#' Resolve the Timeuse Argument to a List of Day Tables
#'
#' @param timeuse One \code{canhrActi_raw_timeuse}, one ms5-shaped list, or a list of either.
#' @return A list of \code{list(output, last_timestamp, tail_expansion_log, label, settings)}.
#' @keywords internal
#' @noRd
.raw.timeuse.report.items <- function(timeuse) {
  one <- function(z, i) {
    if (inherits(z, "canhrActi_raw_timeuse")) {
      lab <- z$settings$filename
      if (is.null(lab) || length(lab) != 1 || is.na(lab)) lab <- paste0("recording ", i)
      return(list(output = z$daysummary, last_timestamp = z$last_timestamp,
                  tail_expansion_log = z$tail_expansion_log, label = as.character(lab),
                  settings = z$settings))
    }
    if (is.list(z) && "output" %in% names(z) && is.data.frame(z$output)) {
      lab <- if (!is.null(z$label)) z$label else {
        fn <- unique(as.character(z$output$filename))
        if (length(fn) == 1) fn else paste0("recording ", i)
      }
      return(list(output = z$output, last_timestamp = z$last_timestamp,
                  tail_expansion_log = z$tail_expansion_log, label = as.character(lab),
                  settings = NULL))
    }
    NULL
  }
  items <- NULL
  if (inherits(timeuse, "canhrActi_raw_timeuse")) {
    items <- list(one(timeuse, 1))
  } else if (is.list(timeuse) && "output" %in% names(timeuse) &&
             is.data.frame(timeuse$output)) {
    items <- list(one(timeuse, 1))
  } else if (is.list(timeuse)) {
    items <- lapply(seq_along(timeuse), function(i) one(timeuse[[i]], i))
    bad <- which(vapply(items, is.null, logical(1)))
    if (length(bad) > 0) {
      stop("element", if (length(bad) > 1) "s " else " ", paste(bad, collapse = ", "),
           " of timeuse ", if (length(bad) > 1) "are" else "is",
           " neither a canhrActi_raw_timeuse from raw.timeuse() nor a list with an output ",
           "data frame (an ms5 milestone)", call. = FALSE)
    }
  }
  if (is.null(items)) {
    stop("timeuse must be a canhrActi_raw_timeuse from raw.timeuse(), an ms5 milestone ",
         "(a list with output and last_timestamp), or a list of either", call. = FALSE)
  }
  keep <- vapply(items, function(z) is.data.frame(z$output) && nrow(z$output) > 0, logical(1))
  items[keep]
}

# ENTRY POINT

#' The GGIR Part-5 Day and Person Reports
#'
#' Turns the per-window table \code{\link{raw.timeuse}} produces into the three report
#' frames GGIR writes per configuration: the QC full day summary, the cleaned day summary
#' and the person summary. One configuration is one combination of window type, the three
#' thresholds and sib definition, so a run with two light thresholds and two window types
#' returns four.
#'
#' @details Reproduces GGIR 3.3-9 R/g.report.part5.R. The recordings arrive as objects and
#'   the six \code{fwrite} calls move to \code{write.ggir.milestone(..., parts = 5)}, which
#'   takes the returned list as its \code{reports} argument.
#'
#'   \code{LC_TIME} is forced to "C" for the duration of the call and restored on exit,
#'   because every \code{daytype} decision is a comparison against an English weekday name;
#'   under another locale every weight is 0 and every \code{_wei} column NaN.
#'
#'   The width of the person summary is data dependent: a variable that is empty on every
#'   retained day never becomes numeric, so it has no weighted twin and appears once under
#'   its plain name instead of as \code{_pla} and \code{_wei}. Read this report by column
#'   name. The split that decided the width is returned as \code{charcol} and \code{numcol}.
#'
#'   Everything returned has been through \code{tidyup_df}, which rounds numeric columns to
#'   six decimals when the name matches \code{FRAG_|ABI|SSP} and to three otherwise; the
#'   part-5 milestone itself is unrounded. Day inclusion (\code{getValidDayIndices}) runs
#'   twice per configuration, on the rounded table for the cleaned csv and on the unrounded
#'   one for the person summary, so both index vectors and a per-window reason are returned.
#'
#'   GGIR 3.3.6 lumps every day-segment row into a single "Segments" report and 3.3-9 writes
#'   one per parent window type ("MMsegment"); \code{segment_naming = "auto"} follows the
#'   names present in the table. For MM, WW and OO the two versions are identical. The cut
#'   points are read back out of the table; set them on \code{\link{raw.timeuse}}.
#'
#' @param timeuse One \code{canhrActi_raw_timeuse} from \code{\link{raw.timeuse}}, one ms5
#'   milestone (a list with \code{output} and \code{last_timestamp}), or a list of either. The
#'   order stands in for GGIR's alphabetical \code{dir()} order and matters only for the
#'   column-set probe.
#' @param params A parameter object from \code{\link{raw.params}}, or NULL to reuse the
#'   parameters the first part-5 result was computed with.
#' @param f0,f1 The first and last recording to report on, as in GGIR. \code{f1} is clamped to
#'   the number of recordings.
#' @param segment_naming "auto", "lumped" (GGIR 3.3.6) or "prefixed" (GGIR 3.3-9).
#' @param ... Individual parameter overrides, for example
#'   \code{minimum_MM_length.part5 = 3}.
#' @return A named list of class \code{canhrActi_raw_timeuse_report}, one element per
#'   configuration, named as GGIR names the csv file:
#'   \code{<window>_L<TRLi>M<TRMi>V<TRVi>_<sleepparam>}. Each element holds \code{window},
#'   \code{TRLi}, \code{TRMi}, \code{TRVi} and \code{sleepparam}; the three GGIR frames
#'   \code{daysummary_full}, \code{daysummary_cleaned} (NULL when no window is valid) and
#'   \code{personsummary} (likewise NULL); \code{daysummary_native}, the cleaned day summary
#'   with cut-point totals appended; \code{windows}, one row per window with the inclusion
#'   decision and the reason; the two index vectors \code{validdaysi_day} and
#'   \code{validdaysi_person}; and \code{charcol} and \code{numcol}. The \code{"canhrActi"}
#'   attribute carries the settings, the messages and a status. The list is the shape
#'   \code{write.ggir.milestone(..., parts = 5, reports = )} expects.
#' @section Warning:
#'   \code{daysummary_native} adds \code{minutes_sedentary_cutpoint} and its eight companions
#'   because GGIR's class durations are easy to add up wrongly. Total and unbouted classes do
#'   not nest: a bout absorbs gaps of up to 60 seconds at another intensity, so
#'   \code{dur_day_IN_unbt_min} plus the three inactivity bout classes is not
#'   \code{dur_day_total_IN_min}. Use either the four totals or the thirteen classes.
#' @seealso \code{\link{raw.timeuse}} for the input, \code{\link{raw.timeuse.dictionary}} for
#'   the variable dictionary and \code{write.ggir.milestone} for the csv files. To merge part 5
#'   into the part-4 report, pass the day table itself:
#'   \code{raw.sleep.report(nights, params, part5 = tu$daysummary)}.
#' @examples
#' \dontrun{
#' tu <- raw.timeuse(x, nights)
#' rep <- raw.timeuse.report(tu)
#' names(rep)
#' dim(rep[["WW_L40M100V400_T5A5"]]$personsummary)
#' subset(rep[["MM_L40M100V400_T5A5"]]$windows, !valid)[, c("calendar_date", "reason")]
#' }
#' @export
raw.timeuse.report <- function(timeuse, params = NULL, f0 = 1, f1 = NULL,
                               segment_naming = c("auto", "lumped", "prefixed"), ...) {
  t_total <- Sys.time()
  segment_naming <- match.arg(segment_naming)
  # C time locale; every daytype decision is an English weekday-name comparison
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  messages <- character()
  items <- .raw.timeuse.report.items(timeuse)
  # Parameters; NULL reuses the ones the first part-5 result was computed with
  stored <- NULL
  for (it in items) {
    if (is.list(it$settings) && is.list(it$settings$params)) {
      stored <- it$settings$params
      break
    }
  }
  params <- .raw.calibrate.params(list(params = stored), params, list(...))
  includedaycrit.part5 <- .raw.param(params, "includedaycrit.part5", 2 / 3)
  includenightcrit.part5 <- .raw.param(params, "includenightcrit.part5", 0)
  minimum_MM_length.part5 <- .raw.param(params, "minimum_MM_length.part5", 23)
  includedaycrit <- .raw.param(params, "includedaycrit", 16)
  data_cleaning_file <- .raw.param(params, "data_cleaning_file", NULL)
  excludefirstlast.part5 <- .raw.param(params, "excludefirstlast.part5", FALSE)
  segmentWEARcrit.part5 <- .raw.param(params, "segmentWEARcrit.part5", 0.5)
  segmentDAYSPTcrit.part5 <- .raw.param(params, "segmentDAYSPTcrit.part5", c(0.9, 0))
  require_complete_lastnight_part5 <-
    .raw.param(params, "require_complete_lastnight_part5", FALSE)
  week_weekend_aggregate.part5 <- .raw.param(params, "week_weekend_aggregate.part5", FALSE)
  LUX_day_segments <- .raw.param(params, "LUX_day_segments", c())
  if (includedaycrit.part5 > 1) {
    # GGIR warns here rather than in check_params
    warning(paste0("\nNote that the behaviour of parameter includedaycrit.part5 ",
                   "has changed for values above 1. These are now treated as the mimimum ",
                   "absolute number of valid hours during the waking hours of a day.",
                   "If you prefer to keep the old functionality then divide ",
                   "your current value (which is above 1) by 24."), call. = FALSE)
  }
  settings <- list(includedaycrit.part5 = includedaycrit.part5,
                   includenightcrit.part5 = includenightcrit.part5,
                   minimum_MM_length.part5 = minimum_MM_length.part5,
                   includedaycrit = includedaycrit,
                   excludefirstlast.part5 = excludefirstlast.part5,
                   segmentWEARcrit.part5 = segmentWEARcrit.part5,
                   segmentDAYSPTcrit.part5 = segmentDAYSPTcrit.part5,
                   require_complete_lastnight_part5 = require_complete_lastnight_part5,
                   week_weekend_aggregate.part5 = week_weekend_aggregate.part5,
                   LUX_day_segments = LUX_day_segments,
                   segment_naming = segment_naming,
                   n_recordings = length(items), params = params)
  finish <- function(out, state) {
    structure(out, class = "canhrActi_raw_timeuse_report",
              canhrActi = list(settings = settings, messages = messages,
                               status = list(state = state, messages = messages,
                                             elapsed = as.numeric(difftime(Sys.time(), t_total,
                                                                           units = "secs")))))
  }
  if (length(items) == 0) {
    # GGIR warns and returns without writing anything
    messages <- c(messages, paste0("No part-5 day table has any rows, so no report was ",
                                   "generated. GGIR no-ops in the same way when ",
                                   "meta/ms5.out is missing or empty."))
    warning(messages[length(messages)], call. = FALSE)
    return(finish(list(), "no_input"))
  }
  if (is.null(f1)) f1 <- length(items)
  outputfinal <- .raw.timeuse.report.stack(items, f0 = f0, f1 = f1)
  # one report per configuration
  uwi <- .raw.timeuse.report.windowtypes(as.character(unique(outputfinal$window)),
                                         naming = segment_naming)
  uTRLi <- as.character(unique(outputfinal$TRLi))
  uTRMi <- as.character(unique(outputfinal$TRMi))
  uTRVi <- as.character(unique(outputfinal$TRVi))
  usleepparam <- as.character(unique(outputfinal$sleepparam))
  out <- list()
  for (j in 1:length(uwi)) {
    window_is_segment <- .raw.timeuse.is.segment(uwi[j])
    for (h1 in 1:length(uTRLi)) {
      for (h2 in 1:length(uTRMi)) {
        for (h3 in 1:length(uTRVi)) {
          for (h4 in 1:length(usleepparam)) {
            select_window <- .raw.timeuse.report.selectwindow(outputfinal$window, uwi[j])
            seluwi <- which(select_window &
                              as.character(outputfinal$TRLi) == uTRLi[h1] &
                              as.character(outputfinal$TRMi) == uTRMi[h2] &
                              as.character(outputfinal$TRVi) == uTRVi[h3] &
                              as.character(outputfinal$sleepparam) == usleepparam[h4])
            if (nrow(outputfinal[seluwi, ]) == 0) {
              next
            }
            CN <- colnames(outputfinal)
            outputfinal2 <- outputfinal
            colnames(outputfinal2) <- CN
            delcol <- grep(pattern = "TRLi|TRMi|TRVi|sleepparam", x = colnames(outputfinal2))
            if (window_is_segment == FALSE) {
              delcol <- c(delcol, which(colnames(outputfinal2) == "window"))
            }
            outputfinal2 <- outputfinal2[, -delcol]
            OF3 <- outputfinal2[seluwi, ]
            OF3 <- as.data.frame(OF3, stringsAsFactors = TRUE)
            # all summaries without cleaning criteria
            OF3_clean <- .raw.report.tidyup(OF3)
            OF3_clean <- .raw.report.addsplitnames(OF3_clean) # If recording was split
            daysummary_full <- OF3_clean
            # all summaries with cleaning criteria
            validdaysi <- .raw.timeuse.validwindows(
              x = OF3_clean, window = uwi[j],
              includedaycrit.part5 = includedaycrit.part5,
              includenightcrit.part5 = includenightcrit.part5,
              minimum_MM_length.part5 = minimum_MM_length.part5,
              includedaycrit = includedaycrit,
              data_cleaning_file = data_cleaning_file,
              excludefirstlast.part5 = excludefirstlast.part5,
              segmentWEARcrit.part5 = segmentWEARcrit.part5,
              segmentDAYSPTcrit.part5 = segmentDAYSPTcrit.part5,
              require_complete_lastnight_part5 = require_complete_lastnight_part5,
              detail = TRUE)
            reasons <- .raw.timeuse.report.reasons(OF3_clean, validdaysi, uwi[j])
            if ("lastHour" %in% colnames(OF3_clean)) {
              OF3_clean <- OF3_clean[, -which(colnames(OF3_clean) %in%
                                                c("lastHour", "lastDate"))]
            }
            daysummary_cleaned <- NULL
            if (length(validdaysi) > 0) {
              daysummary_cleaned <- OF3_clean[validdaysi, ]
            }
            # the person summary, from the unrounded frame
            validdaysi_person <- .raw.timeuse.validwindows(
              x = OF3, window = uwi[j],
              includedaycrit.part5 = includedaycrit.part5,
              includenightcrit.part5 = includenightcrit.part5,
              minimum_MM_length.part5 = minimum_MM_length.part5,
              includedaycrit = includedaycrit,
              data_cleaning_file = data_cleaning_file,
              excludefirstlast.part5 = excludefirstlast.part5,
              segmentWEARcrit.part5 = segmentWEARcrit.part5,
              segmentDAYSPTcrit.part5 = segmentDAYSPTcrit.part5,
              require_complete_lastnight_part5 = require_complete_lastnight_part5)
            personsummary <- NULL
            charcol <- numcol <- character(0)
            if (length(validdaysi_person) > 0) {
              OF4 <- .raw.timeuse.person.row(
                df = OF3[validdaysi_person, ], filename = "filename", daytype = "daytype",
                window = uwi[j],
                week_weekend_aggregate.part5 = week_weekend_aggregate.part5,
                LUX_day_segments = LUX_day_segments)
              charcol <- attr(OF4, "charcol")
              numcol <- attr(OF4, "numcol")
              columns2keep <- c("filename", "night_number", "daysleeper",
                                "cleaningcode", "sleeplog_used", "guider",
                                "acc_available", "nonwear_perc_day", "nonwear_perc_spt",
                                "daytype", "dur_day_min", "dur_spt_min")
              if (window_is_segment) {
                columns2keep <- c(columns2keep, "window")
              }
              OF3tmp <- OF3[, columns2keep]
              OF4 <- .raw.timeuse.daycounts(OF3tmp, OF4, validdaysi_person, uwi[j],
                                            guider = OF3$guider)
              OF4_clean <- .raw.report.tidyup(OF4)
              OF4_clean <- .raw.report.addsplitnames(OF4_clean) # If recording was split
              # drop the helper columns lastHour and lastDate
              lastHour_lastdate <- grep(pattern = "lastHour|lastDate",
                                        x = colnames(OF4_clean))
              if (length(lastHour_lastdate) > 0) {
                OF4_clean <- OF4_clean[, -lastHour_lastdate]
              }
              personsummary <- OF4_clean
            }
            key <- paste0(uwi[j], "_L", uTRLi[h1], "M", uTRMi[h2], "V", uTRVi[h3],
                          "_", usleepparam[h4])
            out[[key]] <- list(
              window = uwi[j], TRLi = uTRLi[h1], TRMi = uTRMi[h2], TRVi = uTRVi[h3],
              sleepparam = usleepparam[h4],
              daysummary_full = daysummary_full,
              daysummary_cleaned = daysummary_cleaned,
              personsummary = personsummary,
              daysummary_native = if (is.null(daysummary_cleaned)) NULL else {
                # validdaysi indexes the rounded frame; the unrounded rows line up only when
                # tidyup_df dropped no row
                .raw.timeuse.native.totals(
                  daysummary_cleaned,
                  source = if (nrow(OF3) == nrow(daysummary_full)) OF3[validdaysi, ] else NULL)
              },
              windows = reasons,
              validdaysi_day = as.integer(validdaysi),
              validdaysi_person = as.integer(validdaysi_person),
              charcol = charcol, numcol = numcol)
          }
        }
      }
    }
  }
  if (length(out) == 0) {
    messages <- c(messages, "No configuration produced any rows.")
    return(finish(out, "no_windows"))
  }
  nodays <- names(out)[vapply(out, function(z) is.null(z$daysummary_cleaned), logical(1))]
  if (length(nodays) > 0) {
    messages <- c(messages,
                  paste0("No window met the inclusion criteria for ",
                         paste(nodays, collapse = ", "),
                         "; the QC day summary is still returned and the cleaned day ",
                         "summary and the person summary are NULL, exactly as GGIR writes ",
                         "neither csv."))
  }
  finish(out, "ok")
}

#' Print a Part-5 Report
#'
#' @param x A \code{canhrActi_raw_timeuse_report}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.canhrActi_raw_timeuse_report <- function(x, ...) {
  a <- attr(x, "canhrActi")
  cat("canhrActi GGIR part-5 report\n")
  cat("  recordings   : ", if (is.null(a)) NA else a$settings$n_recordings, "\n", sep = "")
  cat("  configurations: ", length(x), "\n", sep = "")
  for (nm in names(x)) {
    e <- x[[nm]]
    cat("   ", nm, ": day full ", nrow(e$daysummary_full), " x ",
        ncol(e$daysummary_full),
        ", cleaned ",
        if (is.null(e$daysummary_cleaned)) "none" else {
          paste0(nrow(e$daysummary_cleaned), " x ", ncol(e$daysummary_cleaned))
        },
        ", person ",
        if (is.null(e$personsummary)) "none" else {
          paste0(nrow(e$personsummary), " x ", ncol(e$personsummary))
        },
        "\n", sep = "")
  }
  if (!is.null(a) && length(a$messages) > 0) {
    cat("  messages:\n")
    for (m in a$messages) cat("   - ", m, "\n", sep = "")
  }
  invisible(x)
}
