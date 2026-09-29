# Ported from GGIR 3.3-9 R/g.report.part4.R, R/tidyup_df.R, R/addSplitNames.R and
# R/getSplitNames.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The night tables arrive as objects
# instead of a dir() scan of meta/ms4.out, the part-5 merge is an explicit argument, and
# the four fwrite calls sit in a separate writer so the returned frames are unrounded, as
# GGIR's in-memory ones are.

# ROUNDING AND FILE-NAME HELPERS

#' Round Every Numeric-Coercible Column of a Report Frame
#'
#' GGIR's \code{tidyup_df}, the last thing it does before writing a csv. Rows that are NA in
#' every column are dropped first. The rounding is \code{round(as.numeric(as.character(x)))}
#' inside a \code{tryCatch} whose warning handler returns the column untouched, so a column
#' with one value that does not parse as a number survives unchanged. Applied by
#' \code{write.raw.sleep.report} only; the frames \code{\link{raw.sleep.report}} returns
#' keep full precision.
#'
#' @param df A data frame.
#' @param digits Decimals for the ordinary pass, 3 as in GGIR.
#' @return The frame, with numeric-coercible columns rounded.
#' @keywords internal
#' @noRd
.raw.report.tidyup <- function(df = c(), digits = 3) {
  df <- df[rowSums(!is.na(df)) > 0, ]
  if (ncol(df) > 1) {
    # fragmentation metrics to 6 decimals, everything else to digits
    for (fragCols in c(TRUE, FALSE)) {
      if (fragCols == TRUE) {
        colsID <- grep(pattern = "FRAG_|ABI|SSP", x = colnames(df), invert = FALSE)
        digitsRound <- 6
      } else {
        colsID <- grep(pattern = "FRAG_|ABI|SSP", x = colnames(df), invert = TRUE)
        digitsRound <- digits
      }
      myRoundFun <- function(x) {
        tryCatch(round(as.numeric(as.character(x)), digits = digitsRound),
                 error = function(cond) return(x),
                 warning = function(cond) return(x))
      }
      if (length(colsID) > 0) {
        df[, colsID] <- lapply(df[, colsID], FUN = myRoundFun)
      }
    }
    df <- as.data.frame(df)
  }
  return(df)
}

#' Split a Segmented Recording Name Into Its Parts
#'
#' GGIR's \code{getSplitNames}. Everything between "_split" and the next dot is the segment
#' label, cut on "_" or on the literal "TO".
#'
#' @param filename One file name.
#' @return list(segment_names, filename), segment_names NULL when the name is not segmented.
#' @keywords internal
#' @noRd
.raw.report.splitnames <- function(filename) {
  segment_names <- NULL
  if (length(grep(pattern = "_split", x = filename)) > 0) {
    filename_tmp1 <- unlist(strsplit(filename, "_split|[.]_split"))
    filename_tmp2 <- unlist(strsplit(filename_tmp1[2], "[.]"))
    segment_names <- unlist(strsplit(filename_tmp2[1], "_|TO"))
    filename <- paste0(filename_tmp1[1], ".", filename_tmp2[2])
  }
  invisible(list(segment_names = segment_names, filename = filename))
}

#' Add the Two Segment-Name Columns to a Report Frame
#'
#' GGIR's \code{addSplitNames}. For an unsegmented recording its only effect is the last
#' line, which turns \code{filename} into a factor; the night summary only passes through
#' here when the person summary came back with \code{split1_name}, which is why its
#' \code{filename} stays character.
#'
#' @param x A data frame with a filename column.
#' @return x, with filename a factor and, for segmented recordings, two new columns inserted
#'   directly after it.
#' @keywords internal
#' @noRd
.raw.report.addsplitnames <- function(x) {
  if (nrow(x) != 0) {
    x$filename <- as.character(x$filename)
    for (spi in 1:nrow(x)) {
      splitnames <- .raw.report.splitnames(as.character(x$filename[spi]))
      segment_names <- splitnames$segment_names
      filename <- splitnames$filename
      if (!is.null(segment_names)) {
        x$filename[spi] <- filename
        if ("split1_name" %in% colnames(x) == FALSE) {
          x$split1_name <- NA
          x$split2_name <- NA
          col_index_filename <- which(colnames(x) == "filename")

          x <- x[, c(1:col_index_filename,
                     which(colnames(x) %in% c("split1_name", "split2_name")),
                     (col_index_filename + 1):(ncol(x) - 2))]
        }
        x$split1_name[spi] <- segment_names[2]
        x$split2_name[spi] <- segment_names[3]
      }
    }
    x$filename <- as.factor(x$filename)
  }
  return(x)
}

# INPUT RESOLUTION

#' One Night Table Ready for rbind
#'
#' The body of GGIR's \code{myfun} closure, with the milestone \code{load()} replaced by the
#' object. Rows whose first column is "" are dropped unless that would empty the frame, a
#' non-empty tail expansion log drops the highest-numbered night, and a missing GGIRversion
#' column is back-filled. The closing \code{as.matrix} renders every number through
#' \code{format()} at seven significant digits, and the person-summary means are taken over
#' those values, so it stays.
#'
#' @param nightsummary One night table: a canhrActi_raw_nights, or the plain data frame GGIR
#'   stores as \code{nightsummary} in an ms4 milestone.
#' @param tail_expansion_log NULL, or the part-1 tail expansion log of that recording.
#' @return A character matrix.
#' @keywords internal
#' @noRd
.raw.report.nights.one <- function(nightsummary, tail_expansion_log = NULL) {
  nightsummary <- as.data.frame(nightsummary)
  cut <- which(nightsummary[, 1] == "")
  if (length(cut) > 0 & length(cut) < nrow(nightsummary)) {
    nightsummary <- nightsummary[-cut, ]
  }
  if (length(tail_expansion_log) != 0) {
    # the expanded last night is not trustworthy
    nightsummary <- nightsummary[-which(nightsummary$night == max(nightsummary$night)), ]
  }
  if ("GGIRversion" %in% colnames(nightsummary) == FALSE) {
    if (nrow(nightsummary) > 0) {
      nightsummary$GGIRversion <- "" # absent before GGIR 3.0-10
    } else {
      nightsummary[1, ] <- NA
      nightsummary$GGIRversion <- NA
      nightsummary <- nightsummary[0, ]
    }
  }
  out <- as.matrix(nightsummary)
  out
}

#' Resolve the Nights Argument to a List of Night Tables
#'
#' Replaces GGIR's \code{dir(meta/ms4.out)} scan. One recording or a whole cohort; a NULL
#' element is skipped with a warning rather than failing the cohort.
#'
#' @param nights A canhrActi_raw_nights, a plain nightsummary data frame, or a list of either.
#' @return list(tables, attrs, skipped).
#' @keywords internal
#' @noRd
.raw.report.nights <- function(nights) {
  if (is.null(nights)) {
    stop("nights must be a canhrActi_raw_nights from raw.sleep.nights(), or a list of them",
         call. = FALSE)
  }
  one <- inherits(nights, "canhrActi_raw_nights") || is.data.frame(nights)
  items <- if (one) list(nights) else nights
  if (!is.list(items)) {
    stop("nights must be a canhrActi_raw_nights from raw.sleep.nights(), or a list of them",
         call. = FALSE)
  }
  tables <- list()
  attrs <- list()
  skipped <- character()
  for (k in seq_along(items)) {
    it <- items[[k]]
    lbl <- if (!is.null(names(items)) && nzchar(names(items)[k])) {
      names(items)[k]
    } else {
      paste0("element ", k)
    }
    if (is.null(it)) {
      skipped <- c(skipped, paste0(lbl, " is NULL and was skipped"))
      next
    }
    if (!is.data.frame(it)) {
      stop(paste0("nights ", lbl, " is a ", class(it)[1],
                  ", and must be a canhrActi_raw_nights or a data frame"), call. = FALSE)
    }
    tables[[length(tables) + 1]] <- it
    # single bracket, so a plain data frame with no attribute still takes a slot
    attrs[length(attrs) + 1] <- list(attr(it, "canhrActi"))
  }
  list(tables = tables, attrs = attrs, skipped = skipped)
}

#' The Part-5 Window Table Reduced to the Columns the Report Merges In
#'
#' GGIR's \code{myfun5} closure with its gate and duplicate removal. The gate reads the
#' first element only, as GGIR reads only the first file of meta/ms5.out: if that one has no
#' WW window the merge is abandoned for the whole cohort. \code{duplicated} flags the later
#' occurrence of a repeated (ID, night_number) pair, so the first sleep period of a day is
#' the one kept, whatever GGIR's comment says.
#'
#' @param part5 NULL, one part-5 window table (GGIR's \code{output}), or a list of them. A
#'   \code{canhrActi_raw_timeuse} is refused with a message naming \code{$daysummary}, so
#'   that the part-4 report never depends on the part-5 class.
#' @return NULL when there is nothing to merge, else a data frame with ID, window, night,
#'   nonwear_perc_spt and ACC_spt_mg.
#' @keywords internal
#' @noRd
.raw.nights.merge.part5 <- function(part5) {
  if (is.null(part5)) return(NULL)
  # part 4 must stay runnable without part 5 loaded, so the day table is passed, not the object
  is_tu <- function(z) inherits(z, "canhrActi_raw_timeuse")
  if (is_tu(part5) || (is.list(part5) && !is.data.frame(part5) &&
                       any(vapply(part5, is_tu, logical(1))))) {
    stop("part5 takes the part-5 DAY TABLE, not the raw.timeuse() object: pass ",
         "tu$daysummary, or list(tu_a$daysummary, tu_b$daysummary)", call. = FALSE)
  }
  items <- if (is.data.frame(part5)) list(part5) else part5
  if (!is.list(items) || length(items) == 0) return(NULL)
  items <- items[!vapply(items, is.null, logical(1))]
  if (length(items) == 0) return(NULL)
  # check WW windows are calculated in the first file
  first <- as.data.frame(items[[1]])
  if (!("window" %in% colnames(first))) return(NULL)
  if (!("WW" %in% first$window)) return(NULL)
  myfun5 <- function(x) {
    output <- as.data.frame(x)
    cut <- which(output[, 1] == "")
    if (length(cut) > 0 & length(cut) < nrow(output)) {
      output <- output[-cut, which(colnames(output) != "")]
    }
    WW <- which(output[, "window"] == "WW")
    out <- as.matrix(output[WW, which(colnames(output) %in%
                                        c("ID", "nonwear_perc_spt", "ACC_spt_mg",
                                          "night_number", "window"))])
    out
  }
  outputp5 <- as.data.frame(do.call(rbind, lapply(items, myfun5)), stringsAsFactors = FALSE)
  dupl <- which(duplicated(outputp5[, c("ID", "night_number")]) == TRUE)
  if (length(dupl) > 0) {
    # a day with two SPTs keeps the first
    outputp5 <- outputp5[-dupl, ]
  }
  colnames(outputp5)[which(colnames(outputp5) == "night_number")] <- "night"
  outputp5
}

# PERSON SUMMARY

#' Collapse a Night Table to One Row per Recording
#'
#' GGIR's person-summary builder, called once per pass of the full/cleaned loop.
#'
#' @details \code{CRIT} is computed and then used only as a gate; every mean and standard
#'   deviation is taken over every row of the recording in the current night table, so the
#'   full/cleaned distinction comes entirely from the caller's row deletion. The matrix
#'   turns to character on the first assignment, so every number is stored as
#'   \code{as.character()} of a double; unwritten cells stay "0" and are dropped at the end
#'   because their name is NA. A weekend night starts on Friday or Saturday.
#'   \code{N_nights_guider_corrected} is emitted once, between the AD and WD blocks.
#'   \code{average_dur_sib_wakinghours} is the mean of per-night ratios, with 0 on nights
#'   that had no daytime bout. \code{n_nights_acc} and the weekend/weekday counts use the
#'   rows of the first sib definition only. \code{calendar_date} and \code{weekday} come from
#'   the first surviving night. The two coercions of \code{cleaningcode} and \code{ID}
#'   outlive the call, which is why the night table is returned alongside the person table.
#'
#' @param nightsummary The night table for this pass, already filtered by the caller.
#' @param dotwice 1 for the full pass, 2 for the cleaned pass. Only the full pass writes the
#'   guider and part-5 block, the 24-column difference between the two person summaries.
#' @param only.use.sleeplog TRUE when a sleep diary was configured. It moves
#'   \code{validcleaningcode} from 1 to 0 and changes the \code{CRIT} gate.
#' @param sleepwindowType "SPT" or "TimeInBed".
#' @param consider_marker_button TRUE to add \code{n_nights_markerbutton} and the in-bed
#'   columns.
#' @param sib_must_fully_overlap_with_TimeInBed Length-2 logical; only its first element is
#'   read here, to decide whether negative sleep latencies are excluded and counted.
#' @param storefolderstructure TRUE only widens the initial guess by two columns.
#' @return list(personSummary, nightsummary, personSummarynames).
#' @keywords internal
#' @noRd
.raw.report.person.row <- function(nightsummary, dotwice, only.use.sleeplog = FALSE,
                                   sleepwindowType = "SPT", consider_marker_button = FALSE,
                                   sib_must_fully_overlap_with_TimeInBed = c(TRUE, TRUE),
                                   storefolderstructure = FALSE) {
  NIDS <- max(c(length(unique(nightsummary$filename)), length(unique(nightsummary$ID))))
  NDEF <- length(unique(nightsummary$sleepparam))
  uuu <- unique(nightsummary$sleepparam)
  rem <- which(uuu == 0 | uuu == "0" | is.na(uuu) == TRUE)
  if (length(rem) > 0) {
    uuu <- uuu[-rem]
    NDEF <- length(uuu)
  }
  if (storefolderstructure == TRUE) {
    personSummary <- matrix(0, NIDS, ((NDEF * 3 * 22) + 15 + (6 * 3)))
  } else {
    personSummary <- matrix(0, NIDS, ((NDEF * 3 * 22) + 13 + (6 * 3)))
  }
  # one row per file name, so repeated measurements of one ID are summarised separately
  uniquefn <- unique(nightsummary$filename)
  personSummarynames <- c()
  if (nrow(nightsummary) > 0) {
    for (i in 1:length(uniquefn)) {
      personSummarynames <- c()
      this_file <- which(nightsummary$filename == uniquefn[i])
      nightsummary.tmp <- nightsummary[this_file, ]
      udef <- as.character(unique(nightsummary.tmp$sleepparam))
      if (length(which(as.character(udef) == "0") > 0)) {
        udef <- udef[-c(which(as.character(udef) == "0"))]
      }
      udefn <- udef
      # general info about the file
      personSummary[i, 1] <- nightsummary.tmp$ID[1]
      personSummarynames <- c(personSummarynames, "ID")
      personSummary[i, 2] <- uniquefn[i]
      if (length(unlist(strsplit(as.character(personSummary[i, 2]), ".RDa"))) > 1) {
        personSummary[i, 2] <- unlist(strsplit(personSummary[i, 2], ".RDa"))[1]
      }
      personSummarynames <- c(personSummarynames, "filename")
      cntt <- 2
      personSummary[i, cntt + 1] <- as.character(nightsummary$calendar_date[this_file[1]])
      personSummarynames <- c(personSummarynames, "calendar_date")
      personSummary[i, cntt + 2] <- nightsummary$weekday[this_file[1]]
      personSummarynames <- c(personSummarynames, "weekday")
      personSummary[i, cntt + 3] <- as.character(nightsummary.tmp$sleeplog_used[1])
      personSummarynames <- c(personSummarynames, paste("sleeplog_used", sep = ""))
      this_sleepparam <- which(nightsummary.tmp$sleepparam == udef[1])
      personSummary[i, cntt + 4] <- as.character(nightsummary.tmp$sleeplog_ID[1])
      personSummarynames <- c(personSummarynames, paste("sleeplog_ID", sep = ""))
      # nights with the accelerometer worn, first sib definition only
      personSummary[i, cntt + 5] <-
        length(which((nightsummary.tmp$acc_available[this_sleepparam] == "TRUE" |
                        nightsummary.tmp$acc_available[this_sleepparam] == "1") &
                       nightsummary.tmp$cleaningcode[this_sleepparam] != 2))
      personSummarynames <- c(personSummarynames, paste("n_nights_acc", sep = ""))
      n_nights_sleeplog <-
        length(nightsummary.tmp$night[which(nightsummary.tmp$sleepparam == udef[1] &
                                              nightsummary.tmp$guider == "sleeplog")])
      personSummary[i, cntt + 6] <- n_nights_sleeplog
      personSummarynames <- c(personSummarynames, paste("n_nights_sleeplog", sep = ""))
      cntt <- cntt + 6
      if (consider_marker_button == TRUE) {
        personSummary[i, cntt + 1] <-
          length(nightsummary.tmp$night[which(nightsummary.tmp$sleepparam == udef[1] &
                                                nightsummary.tmp$guider == "markerbutton")])
        personSummarynames <- c(personSummarynames, "n_nights_markerbutton")
        cntt <- cntt + 1
      }
      # complete weekend and week nights; a weekend night starts on Friday or Saturday
      th3 <- nightsummary.tmp$weekday[this_sleepparam]
      if (only.use.sleeplog == TRUE) {
        validcleaningcode <- 0
      } else if (only.use.sleeplog == FALSE) {
        validcleaningcode <- 1
      }

      personSummary[i, cntt + 1] <-
        length(which(nightsummary.tmp$cleaningcode[this_sleepparam] <= validcleaningcode &
                       (th3 == "Friday" | th3 == "Saturday")))
      personSummary[i, cntt + 2] <-
        length(which(nightsummary.tmp$cleaningcode[this_sleepparam] <= validcleaningcode &
                       (th3 == "Monday" | th3 == "Tuesday" | th3 == "Wednesday" |
                          th3 == "Thursday" | th3 == "Sunday")))
      personSummarynames <- c(personSummarynames, paste("n_WE_nights_complete", sep = ""),
                              paste("n_WD_nights_complete", sep = ""))
      # daysleeper nights; $daysleep reaches the column through partial matching, as in GGIR
      personSummary[i, cntt + 3] <-
        length(which(nightsummary.tmp$daysleep[this_sleepparam] == 1 &
                       (th3 == "Friday" | th3 == "Saturday")))
      personSummary[i, cntt + 4] <-
        length(which(nightsummary.tmp$daysleep[this_sleepparam] == 1 &
                       (th3 == "Monday" | th3 == "Tuesday" | th3 == "Wednesday" |
                          th3 == "Thursday" | th3 == "Sunday")))
      personSummarynames <- c(personSummarynames, paste("n_WEnights_daysleeper", sep = ""),
                              paste("n_WDnights_daysleeper", sep = ""))
      cnt <- cntt + 4
      # guider summary
      turn_numeric <- function(x, varnames) {
        cnx <- colnames(x)
        for (i in 1:length(varnames)) {
          if (varnames[i] %in% cnx) {
            x[, varnames[i]] <- as.numeric(x[, varnames[i]])
          }
        }
        return(x)
      }
      if (sleepwindowType == "SPT" && consider_marker_button == FALSE) {
        gdn <- c("guider_SptDuration", "guider_onset", "guider_wakeup")
      } else if (sleepwindowType == "TimeInBed" || consider_marker_button == TRUE) {
        gdn <- c("guider_inbedDuration", "guider_inbedStart", "guider_inbedEnd")
      }
      if (dotwice == 1) {
        nightsummary.tmp <- turn_numeric(x = nightsummary.tmp, varnames = gdn)
      }
      varnames_tmp <- c("SptDuration", "sleeponset",
                        "wakeup", "WASO", "SleepDurationInSpt",
                        "number_sib_sleepperiod", "duration_sib_wakinghours",
                        "number_of_awakenings", "number_sib_wakinghours",
                        "duration_sib_wakinghours_atleast15min",
                        "sleeplatency", "sleepefficiency", "number_of_awakenings",
                        "guider_inbedDuration", "guider_inbedStart",
                        "guider_inbedEnd", "guider_SptDuration", "guider_onset",
                        "guider_wakeup", "SleepRegularityIndex1",
                        "SriFractionValid", "N_nights_guider_corrected")
      nightsummary.tmp <- turn_numeric(x = nightsummary.tmp,
                                       varnames = varnames_tmp)
      weekday <- nightsummary.tmp$weekday[this_sleepparam]
      if (dotwice == 1) {
        for (k in 1:3) {
          if (k == 1) {
            TW <- "AD"
            Seli <- 1:length(weekday)
          } else if (k == 2) {
            TW <- "WD"
            Seli <- which(weekday == "Monday" | weekday == "Tuesday" |
                            weekday == "Wednesday" | weekday == "Thursday" |
                            weekday == "Sunday")
          } else if (k == 3) {
            TW <- "WE"
            Seli <- which(weekday == "Friday" | weekday == "Saturday")
          }
          relevant_rows <- this_sleepparam[Seli]
          if (length(relevant_rows) > 0) {
            for (gdni in 1:length(gdn)) {
              personSummary[i, cnt + 1] <- mean(nightsummary.tmp[relevant_rows, gdn[gdni]],
                                                na.rm = TRUE)
              personSummary[i, cnt + 2] <- stats::sd(nightsummary.tmp[relevant_rows, gdn[gdni]],
                                                     na.rm = TRUE)
              personSummarynames <- c(personSummarynames,
                                      paste(gdn[gdni], "_", TW, "_mn", sep = ""),
                                      paste(gdn[gdni], "_", TW, "_sd", sep = ""))
              cnt <- cnt + 2
            }
          }
          # the part-5 means are written even when the window holds no night
          if ("nonwear_perc_spt" %in% colnames(nightsummary.tmp)) {
            personSummary[i, cnt + 1] <- mean(nightsummary.tmp$nonwear_perc_spt[this_sleepparam[Seli]],
                                              na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("nonwear_perc_spt_", TW, "_mn", sep = ""))
            cnt <- cnt + 1
          }
          if ("ACC_spt_mg" %in% colnames(nightsummary.tmp)) {
            personSummary[i, cnt + 1] <- mean(nightsummary.tmp$ACC_spt_mg[this_sleepparam[Seli]],
                                              na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("ACC_spt_mg_", TW, "_mn", sep = ""))
            cnt <- cnt + 1
          }
        }
      }
      # these two coercions outlive the call and reach the cleaned pass
      nightsummary$cleaningcode <- as.numeric(nightsummary$cleaningcode)
      nightsummary$ID <- as.character(nightsummary$ID)
      # accelerometer summary
      if (only.use.sleeplog == FALSE) {
        if (dotwice == 2) {
          CRIT <- which(nightsummary$filename == uniquefn[i] &
                          (nightsummary$cleaningcode == 0 | nightsummary$cleaningcode == 1))
        } else {
          CRIT <- which(nightsummary$filename == uniquefn[i])
        }
      } else {
        CRIT <- which(nightsummary$filename == uniquefn[i] &
                        nightsummary$cleaningcode == 0)
      }
      personSummarynames_backup <- c()
      if (length(CRIT) > 0) {
        for (j in 1:length(udef)) {
          weekday <- nightsummary.tmp$weekday[which(nightsummary.tmp$sleepparam == udef[j])]
          for (k in 1:3) {
            if (ncol(personSummary) < (cnt + 50)) {
              # grow the matrix when within 50 columns of the end
              expansion <- matrix(NA, nrow(personSummary), 50)
              if (nrow(expansion) != nrow(personSummary)) expansion <- t(expansion)
              personSummary <- cbind(personSummary, expansion)
            }
            if (k == 1) {
              TW <- "AD"
              Seli <- 1:length(weekday)
            } else if (k == 2) {
              TW <- "WD"
              Seli <- which(weekday == "Monday" | weekday == "Tuesday" |
                              weekday == "Wednesday" | weekday == "Thursday" |
                              weekday == "Sunday")
            } else if (k == 3) {
              TW <- "WE"
              Seli <- which(weekday == "Friday" | weekday == "Saturday")
            }
            indexUdef <- which(nightsummary.tmp$sleepparam == udef[j])[Seli]
            personSummary[i, (cnt + 1)] <- mean(nightsummary.tmp$SptDuration[indexUdef],
                                                na.rm = TRUE)
            personSummary[i, (cnt + 2)] <- stats::sd(nightsummary.tmp$SptDuration[indexUdef],
                                                     na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("SptDuration_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("SptDuration_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 3)] <- mean(nightsummary.tmp$SleepDurationInSpt[indexUdef],
                                                na.rm = TRUE)
            personSummary[i, (cnt + 4)] <- stats::sd(nightsummary.tmp$SleepDurationInSpt[indexUdef],
                                                     na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("SleepDurationInSpt_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("SleepDurationInSpt_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 5)] <- mean(nightsummary.tmp$WASO[indexUdef], na.rm = TRUE)
            personSummary[i, (cnt + 6)] <- stats::sd(nightsummary.tmp$WASO[indexUdef], na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("WASO_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("WASO_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 7)] <- mean(nightsummary.tmp$duration_sib_wakinghours[indexUdef],
                                                na.rm = TRUE)
            personSummary[i, (cnt + 8)] <- stats::sd(nightsummary.tmp$duration_sib_wakinghours[indexUdef],
                                                     na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("duration_sib_wakinghours_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("duration_sib_wakinghours_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 9)] <- mean(nightsummary.tmp$number_sib_sleepperiod[indexUdef],
                                                na.rm = TRUE)
            personSummary[i, (cnt + 10)] <- stats::sd(nightsummary.tmp$number_sib_sleepperiod[indexUdef],
                                                      na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("number_sib_sleepperiod_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("number_sib_sleepperiod_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 11)] <- mean(nightsummary.tmp$number_of_awakenings[indexUdef],
                                                 na.rm = TRUE)
            personSummary[i, (cnt + 12)] <- stats::sd(nightsummary.tmp$number_of_awakenings[indexUdef],
                                                      na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("number_of_awakenings_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("number_of_awakenings_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 13)] <- mean(nightsummary.tmp$number_sib_wakinghours[indexUdef],
                                                 na.rm = TRUE)
            personSummary[i, (cnt + 14)] <- stats::sd(nightsummary.tmp$number_sib_wakinghours[indexUdef],
                                                      na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("number_sib_wakinghours_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("number_sib_wakinghours_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 15)] <-
              mean(nightsummary.tmp$duration_sib_wakinghours_atleast15min[indexUdef], na.rm = TRUE)
            personSummary[i, (cnt + 16)] <-
              stats::sd(nightsummary.tmp$duration_sib_wakinghours_atleast15min[indexUdef], na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("duration_sib_wakinghours_atleast15min_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("duration_sib_wakinghours_atleast15min_", TW, "_", udefn[j], "_sd", sep = ""))
            # mean daytime bout duration per night, 0 on nights without one
            AVEsibdDUR <- c(nightsummary.tmp$duration_sib_wakinghours[indexUdef] /
                              nightsummary.tmp$number_sib_wakinghours[indexUdef])
            if (length(which(nightsummary.tmp$number_sib_wakinghours[indexUdef] == 0))) {
              AVEsibdDUR[which(nightsummary.tmp$number_sib_wakinghours[indexUdef] == 0)] <- 0
            }
            personSummary[i, (cnt + 17)] <- mean(AVEsibdDUR, na.rm = TRUE)
            personSummary[i, (cnt + 18)] <- stats::sd(AVEsibdDUR, na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("average_dur_sib_wakinghours_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("average_dur_sib_wakinghours_", TW, "_", udefn[j], "_sd", sep = ""))
            NDAYsibd <- length(which(nightsummary.tmp$number_sib_wakinghours[indexUdef] > 0))
            if (length(NDAYsibd) == 0) NDAYsibd <- 0
            personSummary[i, (cnt + 19)] <- NDAYsibd
            personSummarynames <- c(personSummarynames,
                                    paste("n_days_w_sib_wakinghours_", TW, "_", udefn[j], sep = ""))
            personSummary[i, (cnt + 20)] <- mean(nightsummary.tmp$sleeponset[indexUdef], na.rm = TRUE)
            personSummary[i, (cnt + 21)] <- stats::sd(nightsummary.tmp$sleeponset[indexUdef], na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("sleeponset_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("sleeponset_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 22)] <- mean(nightsummary.tmp$wakeup[indexUdef], na.rm = TRUE)
            personSummary[i, (cnt + 23)] <- stats::sd(nightsummary.tmp$wakeup[indexUdef], na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("wakeup_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("wakeup_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 24)] <- mean(nightsummary.tmp$SleepRegularityIndex1[indexUdef],
                                                 na.rm = TRUE)
            personSummary[i, (cnt + 25)] <- stats::sd(nightsummary.tmp$SleepRegularityIndex1[indexUdef],
                                                      na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("SleepRegularityIndex1_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("SleepRegularityIndex1_", TW, "_", udefn[j], "_sd", sep = ""))
            personSummary[i, (cnt + 26)] <- mean(nightsummary.tmp$SriFractionValid[indexUdef], na.rm = TRUE)
            personSummary[i, (cnt + 27)] <- stats::sd(nightsummary.tmp$SriFractionValid[indexUdef], na.rm = TRUE)
            personSummarynames <- c(personSummarynames,
                                    paste("SriFractionValid_", TW, "_", udefn[j], "_mn", sep = ""),
                                    paste("SriFractionValid_", TW, "_", udefn[j], "_sd", sep = ""))
            cnt <- cnt + 27
            if ("N_nights_guider_corrected" %in% personSummarynames == FALSE) {
              personSummary[i, (cnt + 1)] <- sum(as.numeric(nightsummary.tmp$guider_corrected[indexUdef]),
                                                 na.rm = TRUE)
              personSummarynames <- c(personSummarynames, "N_nights_guider_corrected")
              cnt <- cnt + 1
            }
            if (sleepwindowType == "TimeInBed" || consider_marker_button == TRUE) {
              sleepefficiency <- nightsummary.tmp$sleepefficiency[indexUdef]
              latency <- nightsummary.tmp$sleeplatency[indexUdef]
              if (sib_must_fully_overlap_with_TimeInBed[1] == FALSE) {
                negative_latency <- which(latency < 0)
                N_neg_lat <- length(negative_latency)
                if (N_neg_lat > 0) latency <- latency[-negative_latency]
                personSummary[i, (cnt + 1)] <- N_neg_lat
                personSummarynames <- c(personSummarynames, "N_nights_negative_latency")
                cnt <- cnt + 1
                Nlatency <- length(latency)
                if (Nlatency > 0) {
                  meanLatency <- mean(latency, na.rm = TRUE)
                  meanSleepefficiency <- mean(sleepefficiency, na.rm = TRUE)
                } else {
                  meanLatency <- NA
                  meanSleepefficiency <- NA
                }
                if (Nlatency > 1) {
                  sdLatency <- stats::sd(latency, na.rm = TRUE)
                  sdSleepefficiency <- stats::sd(sleepefficiency, na.rm = TRUE)
                } else {
                  sdLatency <- NA
                  sdSleepefficiency <- NA
                }
              } else {
                meanLatency <- mean(latency, na.rm = TRUE)
                meanSleepefficiency <- mean(sleepefficiency, na.rm = TRUE)
                sdLatency <- stats::sd(latency, na.rm = TRUE)
                sdSleepefficiency <- stats::sd(sleepefficiency, na.rm = TRUE)
              }
              personSummary[i, (cnt + 1)] <- meanSleepefficiency
              personSummary[i, (cnt + 2)] <- sdSleepefficiency
              personSummarynames <- c(personSummarynames,
                                      paste("sleep_efficiency_", TW, "_", udefn[j], "_mn", sep = ""),
                                      paste("sleep_efficiency_", TW, "_", udefn[j], "_sd", sep = ""))
              cnt <- cnt + 2
              personSummary[i, (cnt + 1)] <- meanLatency
              personSummary[i, (cnt + 2)] <- sdLatency
              personSummarynames <- c(personSummarynames,
                                      paste("sleeplatency_", TW, "_", udefn[j], "_mn", sep = ""),
                                      paste("sleeplatency_", TW, "_", udefn[j], "_sd", sep = ""))
              personSummary[i, (cnt + 3)] <- mean(nightsummary.tmp$guider_inbedStart[indexUdef], na.rm = TRUE)
              personSummary[i, (cnt + 4)] <- stats::sd(nightsummary.tmp$guider_inbedStart[indexUdef], na.rm = TRUE)
              personSummarynames <- c(personSummarynames,
                                      paste("guider_inbedStart_", TW, "_", udefn[j], "_mn", sep = ""),
                                      paste("guider_inbedStart_", TW, "_", udefn[j], "_sd", sep = ""))
              cnt <- cnt + 4
              personSummary[i, (cnt + 1)] <- mean(nightsummary.tmp$guider_inbedEnd[indexUdef], na.rm = TRUE)
              personSummary[i, (cnt + 2)] <- stats::sd(nightsummary.tmp$guider_inbedEnd[indexUdef], na.rm = TRUE)
              personSummarynames <- c(personSummarynames,
                                      paste("guider_inbedEnd_", TW, "_", udefn[j], "_mn", sep = ""),
                                      paste("guider_inbedEnd_", TW, "_", udefn[j], "_sd", sep = ""))
              personSummary[i, (cnt + 3)] <- mean(nightsummary.tmp$guider_inbedDuration[indexUdef], na.rm = TRUE)
              personSummary[i, (cnt + 4)] <- stats::sd(nightsummary.tmp$guider_inbedDuration[indexUdef], na.rm = TRUE)
              personSummarynames <- c(personSummarynames,
                                      paste("guider_inbedDuration_", TW, "_", udefn[j], "_mn", sep = ""),
                                      paste("guider_inbedDuration_", TW, "_", udefn[j], "_sd", sep = ""))
              cnt <- cnt + 4
            }
          }
        }
        personSummary[i, cnt + 1] <- as.character(nightsummary$GGIRversion[this_file[1]])
        cnt <- cnt + 1
        personSummarynames <- c(personSummarynames, "GGIRversion")
        personSummarynames_backup <- personSummarynames
      }
    }
    # NA and NaN cells become empty
    for (colli in 1:ncol(personSummary)) {
      missingv <- which(is.na(personSummary[, colli]) == TRUE |
                          personSummary[, colli] == "NA" |
                          personSummary[, colli] == "NaN")
      if (length(missingv) > 0) {
        personSummary[missingv, colli] <- ""
      }
    }
    personSummary <- as.data.frame(personSummary, stringsAsFactors = TRUE)
    if (length(personSummarynames) != ncol(personSummary)) {
      if (length(personSummarynames_backup) > 0) {
        names(personSummary) <- personSummarynames_backup
      } else {
        if (length(personSummarynames) > ncol(personSummary)) {
          names(personSummary)[1:length(personSummarynames)] <- personSummarynames
        } else {
          names(personSummary) <- personSummarynames[1:ncol(personSummary)]
        }
      }
    } else {
      names(personSummary) <- personSummarynames
    }
    # drop the unnamed, never written columns
    emptycolumns <- which(is.na(colnames(personSummary)) == TRUE)
    if (length(emptycolumns) > 0) {
      personSummary <- personSummary[, -emptycolumns]
    }
  }
  list(personSummary = personSummary, nightsummary = nightsummary,
       personSummarynames = personSummarynames)
}

# THE REPORT

#' Sleep Reports for One Recording or a Cohort
#'
#' Collapses the per-night tables of \code{\link{raw.sleep.nights}} into the four tables GGIR
#' writes at the end of part 4: a night-level and a person-level table, each in a full and a
#' cleaned version. The cleaned pass drops nights GGIR judges untrustworthy and the full pass
#' keeps everything; the person tables are one row per recording with every night-level
#' column averaged over all days, over weekday nights and over weekend nights.
#'
#' @details GGIR 3.3-9 \code{g.report.part4}, in its order: every night table is trimmed,
#'   tail-expansion corrected and turned into a character matrix (seven significant digits,
#'   which is what the person averages are taken over), then stacked; \code{calendar_date}
#'   is rewritten to Y-m-d and ".RData" stripped from \code{filename}; the guider columns
#'   are renamed in TimeInBed or marker-button mode; the part-5 windows are merged on
#'   (ID, night); the full pass runs, then the cleaned pass deletes nights by
#'   \code{cleaningcode} and guider label and by the data cleaning file and drops the three
#'   \code{error_} columns; each pass builds its person table; \code{addSplitNames} runs on
#'   the person table and, only if that produced split columns, on the night table.
#'
#'   The full/cleaned distinction is that row deletion and nothing else, so the full person
#'   row averages the invalid nights too. A weekend night starts on Friday or Saturday. The
#'   cleaned person table has no guider block, the 24-column difference between the two.
#'   The tail expansion log comes from the night table's own attribute when present, else
#'   from the argument. The frames returned are unrounded; \code{ggir_exact} is not
#'   branched on.
#'
#' @param nights One \code{canhrActi_raw_nights} from \code{\link{raw.sleep.nights}}, the
#'   plain nightsummary data frame of an ms4 milestone, or a list of either. A NULL element of
#'   the list is skipped with a warning.
#' @param params A parameter object from \code{\link{raw.params}}, or NULL to reuse the
#'   parameters the first night table ran with.
#' @param part5 NULL, one part-5 window table (the \code{output} object of an ms5 milestone),
#'   or a list of them in the same order as \code{nights}. Supplying it adds the three columns
#'   \code{window}, \code{nonwear_perc_spt} and \code{ACC_spt_mg} to the night tables and six
#'   means to the full person table. Pass the \code{daysummary} member of a
#'   \code{\link{raw.timeuse}} result, not the object itself.
#' @param tail_expansion_log NULL, one part-1 tail expansion log applied to every recording,
#'   or a list of them in the same order as \code{nights}. A non-empty log drops that
#'   recording's highest-numbered night. Ignored for a night table whose own attribute already
#'   carries one.
#' @param ... Individual parameter overrides, for example \code{sleepwindowType = "TimeInBed"}.
#' @return A list of class \code{canhrActi_raw_sleep_report} with four data frames,
#'   \code{nightsummary_full}, \code{nightsummary_cleaned}, \code{personsummary_full} and
#'   \code{personsummary_cleaned}, all unrounded. The \code{"canhrActi"} attribute carries the
#'   settings, the messages and a status.
#' @seealso \code{\link{raw.sleep.nights}} for the input and
#'   \code{\link{write.raw.sleep.report}} for GGIR's four csv files.
#' @examples
#' \dontrun{
#' rep <- raw.sleep.report(list(nights_a, nights_b))
#' rep$personsummary_cleaned[, c("ID", "n_nights_acc", "SptDuration_AD_T5A5_mn")]
#' }
#' @export
raw.sleep.report <- function(nights, params = NULL, part5 = NULL,
                             tail_expansion_log = NULL, ...) {
  t_total <- Sys.time()
  # C time locale for the call, as GGIR's parallel workers do
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  resolved <- .raw.report.nights(nights)
  messages <- character()
  for (m in resolved$skipped) {
    warning(m, call. = FALSE)
    messages <- c(messages, m)
  }
  # Parameters; NULL reuses the ones the first night table was built with
  stored <- NULL
  for (a in resolved$attrs) {
    if (is.list(a) && is.list(a$settings) && is.list(a$settings$params)) {
      stored <- a$settings$params
      break
    }
  }
  params <- .raw.calibrate.params(list(params = stored), params, list(...))
  loglocation <- .raw.param(params, "loglocation", NULL)
  sleepwindowType <- .raw.param(params, "sleepwindowType", "SPT")
  consider_marker_button <- .raw.param(params, "consider_marker_button", FALSE)
  sib_must_fully_overlap_with_TimeInBed <-
    .raw.param(params, "sib_must_fully_overlap_with_TimeInBed", c(TRUE, TRUE))
  storefolderstructure <- .raw.param(params, "storefolderstructure", FALSE)
  data_cleaning_file <- .raw.param(params, "data_cleaning_file", NULL)
  sep_reports <- .raw.param(params, "sep_reports", ",")
  dec_reports <- .raw.param(params, "dec_reports", ".")
  # a diary object counts as a configured diary, as loglocation does in GGIR
  dolog_any <- FALSE
  for (a in resolved$attrs) if (is.list(a) && isTRUE(a$dolog)) dolog_any <- TRUE
  if (length(loglocation) > 0) {
    only.use.sleeplog <- TRUE
  } else {
    only.use.sleeplog <- FALSE
  }
  if (only.use.sleeplog == FALSE && dolog_any == TRUE) {
    only.use.sleeplog <- TRUE
    messages <- c(messages, paste0("A sleep diary was used to build the night tables but ",
                                   "loglocation is empty, so the cleaned pass uses GGIR's ",
                                   "diary rule (cleaningcode above 0)."))
  }
  # Tail expansion logs: the night table's own attribute first, the argument second
  ntab <- length(resolved$tables)
  tel <- vector("list", ntab)
  arg_tel <- if (is.null(tail_expansion_log)) {
    vector("list", ntab)
  } else if (is.list(tail_expansion_log) && length(tail_expansion_log) == ntab &&
             !is.data.frame(tail_expansion_log)) {
    tail_expansion_log
  } else {
    rep(list(tail_expansion_log), ntab)
  }
  for (k in seq_len(ntab)) {
    a <- resolved$attrs[[k]]
    tel[k] <- list(if (is.list(a) && !is.null(a$tail_expansion_log)) {
      a$tail_expansion_log
    } else {
      arg_tel[[k]]
    })
  }
  settings <- list(only.use.sleeplog = only.use.sleeplog, sleepwindowType = sleepwindowType,
                   consider_marker_button = consider_marker_button,
                   sib_must_fully_overlap_with_TimeInBed =
                     sib_must_fully_overlap_with_TimeInBed,
                   storefolderstructure = storefolderstructure,
                   data_cleaning_file = data_cleaning_file,
                   sep_reports = sep_reports, dec_reports = dec_reports,
                   part5_merged = FALSE, params = params)
  empty <- data.frame()
  out <- list(nightsummary_full = empty, nightsummary_cleaned = empty,
              personsummary_full = empty, personsummary_cleaned = empty)
  finish <- function(res, state) {
    structure(res,
              class = c("canhrActi_raw_sleep_report", "list"),
              canhrActi = list(
                n_recordings = ntab,
                settings = settings,
                status = list(state = state, messages = messages),
                elapsed = as.numeric(difftime(Sys.time(), t_total, units = "secs"))))
  }
  if (ntab == 0) {
    # GGIR's try.generate.report = FALSE
    messages <- c(messages, "No night tables were supplied, so no report was produced.")
    return(finish(out, "no_nights"))
  }
  # Stack the night tables
  nightsummary2 <- as.data.frame(do.call(rbind, lapply(seq_len(ntab), function(k) {
    .raw.report.nights.one(resolved$tables[[k]], tel[[k]])
  })), stringsAsFactors = FALSE)
  nightsummary2$night <- as.numeric(gsub(" ", "", nightsummary2$night))
  nightsummary2$calendar_date <- as.Date(nightsummary2$calendar_date, format = "%d/%m/%Y")
  nightsummary2$calendar_date <- format(nightsummary2$calendar_date, format = "%Y-%m-%d")
  nightsummary2$filename <- gsub(".RData$", "", nightsummary2$filename)

  if (sleepwindowType == "TimeInBed" || consider_marker_button == TRUE) {
    colnames(nightsummary2) <- gsub(replacement = "guider_inbedStart",
                                    pattern = "guider_onset", x = colnames(nightsummary2))
    colnames(nightsummary2) <- gsub(replacement = "guider_inbedEnd",
                                    pattern = "guider_wakeup", x = colnames(nightsummary2))
    colnames(nightsummary2) <- gsub(replacement = "guider_inbedDuration",
                                    pattern = "guider_SptDuration", x = colnames(nightsummary2))
  }
  # Non-wear during the SPT from part 5, if available
  outputp5 <- .raw.nights.merge.part5(part5)
  if (!is.null(outputp5)) {
    # a numeric ID stored with a leading zero has none in part 5, so strip it for the merge
    remove_oldID <- FALSE
    if (is.character(nightsummary2$ID) & is.character(outputp5$ID)) {
      testnumeric <- suppressWarnings(!is.na(as.numeric(nightsummary2$ID)))
      if (length(which(testnumeric == TRUE)) > (nrow(nightsummary2) * (2 / 3))) {
        nightsummary2$ID_old <- nightsummary2$ID
        nightsummary2$ID <- as.character(as.numeric(nightsummary2$ID))
        remove_oldID <- TRUE
      }
    }
    outputp5$night <- as.numeric(outputp5$night)
    nightsummary2 <- base::merge(nightsummary2, outputp5, by = c("ID", "night"), all.x = TRUE)
    if (remove_oldID == TRUE) {
      nightsummary2$ID <- nightsummary2$ID_old
      nightsummary2 <- nightsummary2[, -which(names(nightsummary2) == "ID_old")]
    }
    nightsummary2 <- nightsummary2[order(nightsummary2$ID, nightsummary2$night), ]
    nightsummary2$nonwear_perc_spt <- as.numeric(nightsummary2$nonwear_perc_spt)
    nightsummary2$ACC_spt_mg <- as.numeric(nightsummary2$ACC_spt_mg)
    settings$part5_merged <- TRUE
  }
  skip <- FALSE
  if (length(nightsummary2) != 0) {
    NumberNotNA <- length(which(is.na(nightsummary2[, 3:25]) == FALSE))
    if (NumberNotNA == 0) {
      skip <- TRUE
      warning("\nCannot create report part 4 report, because no sleep estimates present in milestone data.",
              call. = FALSE)
    }
  } else {
    skip <- TRUE
    warning("\nCannot create report part 4 report, because no milestone data found for part4.",
            call. = FALSE)
  }
  if (skip == TRUE) {
    messages <- c(messages, "No sleep estimates present, so no report was produced.")
    out$nightsummary_full <- nightsummary2[0, , drop = FALSE]
    out$nightsummary_cleaned <- nightsummary2[0, , drop = FALSE]
    return(finish(out, "no_estimates"))
  }
  nightsummary <- nightsummary2
  # nights with onset, wake and duration all zero; compares character cells, so rarely fires
  pko <- which(nightsummary$sleeponset == 0 & nightsummary$wakeup == 0 &
                 nightsummary$SptDuration == 0)
  if (length(pko) > 0) {
    nightsummary <- nightsummary[-pko, ]
  }
  # collapse to one person row per recording
  if (nrow(nightsummary) == 0) {
    messages <- c(messages, "No report stored, because no results are available.")
  } else {
    nightsummary_bu <- nightsummary
  }
  for (dotwice in 1:2) {
    # once full, once cleaned
    if (dotwice == 2) {
      # with a diary, drop every night that did not use it
      if (only.use.sleeplog == TRUE) {
        del <- which(nightsummary$cleaningcode > 0 | nightsummary$sleeplog_used == "FALSE" |
                       nightsummary$guider == "NotWorn" |
                       nightsummary$guider == "NotWorn+invalid")
      } else {
        # without one, drop only nights with no valid accelerometer data
        del <- which(nightsummary$cleaningcode > 1 | nightsummary$guider == "NotWorn" |
                       nightsummary$guider == "NotWorn+invalid")
      }
      if (length(del) > 0) {
        nightsummary <- nightsummary_bu[-del, ]
      }
      if (length(data_cleaning_file) > 0) {
        DaCleanFile <- data.table::fread(data_cleaning_file, data.table = FALSE)
        if ("night_part4" %in% colnames(DaCleanFile)) {
          days2exclude <- which(paste(nightsummary$ID, nightsummary$night) %in%
                                  paste(DaCleanFile$ID, DaCleanFile$night_part4))
          if (length(days2exclude) > 0) {
            nightsummary <- nightsummary[-days2exclude, ]
          }
        }
      }
      # the error_ columns are for methodological research only
      coldel <- which(colnames(nightsummary) %in% c("error_onset", "error_wake",
                                                    "error_dur") == TRUE)
      if (length(coldel) > 0) {
        nightsummary <- nightsummary[, -coldel]
      }
    }
    pr <- .raw.report.person.row(
      nightsummary = nightsummary, dotwice = dotwice,
      only.use.sleeplog = only.use.sleeplog, sleepwindowType = sleepwindowType,
      consider_marker_button = consider_marker_button,
      sib_must_fully_overlap_with_TimeInBed = sib_must_fully_overlap_with_TimeInBed,
      storefolderstructure = storefolderstructure)
    personSummary <- pr$personSummary
    nightsummary <- pr$nightsummary
    # segment names of split recordings
    if (nrow(nightsummary) != 0) {
      personSummary <- .raw.report.addsplitnames(personSummary)
      if ("split1_name" %in% colnames(personSummary) == TRUE) {
        nightsummary <- .raw.report.addsplitnames(nightsummary)
      }
    }
    if (nrow(nightsummary) == 0) {
      if (dotwice == 1) {
        messages <- c(messages, "part 4 full report not stored, because no results available")
      } else {
        messages <- c(messages,
                      "part 4 cleaned report not stored, because no results available")
      }
      if (dotwice == 1) {
        out$nightsummary_full <- nightsummary
        out$personsummary_full <- data.frame()
      } else {
        out$nightsummary_cleaned <- nightsummary
        out$personsummary_cleaned <- data.frame()
      }
    } else {
      # unrounded; write.raw.sleep.report() applies tidyup
      if (dotwice == 1) {
        out$nightsummary_full <- nightsummary
        out$personsummary_full <- personSummary
      } else {
        out$nightsummary_cleaned <- nightsummary
        out$personsummary_cleaned <- personSummary
      }
    }
  }
  finish(out, "ok")
}

# CSV FILES

#' Write the Four Part-4 csv Files GGIR Writes
#'
#' The other half of \code{\link{raw.sleep.report}}: \code{tidyup_df} rounding followed by
#' \code{data.table::fwrite} into GGIR's four fixed paths. A pass whose night table has no
#' rows writes neither of its two files, as GGIR skips both.
#'
#' @param x A canhrActi_raw_sleep_report from \code{\link{raw.sleep.report}}.
#' @param dir The output directory, the equivalent of GGIR's \code{metadatadir}. The four
#'   files land in \code{dir/results} and \code{dir/results/QC}, which are created when
#'   missing.
#' @param sep Column separator, NULL to use the \code{sep_reports} the report ran with.
#' @param dec Decimal separator, NULL to use \code{dec_reports}.
#' @return The paths written, invisibly.
#' @examples
#' \dontrun{
#' write.raw.sleep.report(rep, tempdir())
#' }
#' @export
write.raw.sleep.report <- function(x, dir, sep = NULL, dec = NULL) {
  if (!inherits(x, "canhrActi_raw_sleep_report")) {
    stop("x must be a canhrActi_raw_sleep_report from raw.sleep.report()", call. = FALSE)
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("write.raw.sleep.report() needs the data.table package, which GGIR writes these ",
         "csv files with. Install it, or write the four frames yourself with ",
         "utils::write.csv().", call. = FALSE)
  }
  meta <- attr(x, "canhrActi")
  if (is.null(sep)) sep <- meta$settings$sep_reports
  if (is.null(dec)) dec <- meta$settings$dec_reports
  if (is.null(sep)) sep <- ","
  if (is.null(dec)) dec <- "."
  results <- file.path(dir, "results")
  qc <- file.path(results, "QC")
  dir.create(qc, recursive = TRUE, showWarnings = FALSE)
  written <- character()
  put <- function(df, path) {
    data.table::fwrite(.raw.report.tidyup(df), file = path, row.names = FALSE, na = "",
                       sep = sep, dec = dec)
    written <<- c(written, path)
  }
  if (nrow(x$nightsummary_full) > 0) {
    put(x$nightsummary_full, file.path(qc, "part4_nightsummary_sleep_full.csv"))
    put(x$personsummary_full, file.path(qc, "part4_summary_sleep_full.csv"))
  }
  if (nrow(x$nightsummary_cleaned) > 0) {
    put(x$nightsummary_cleaned, file.path(results, "part4_nightsummary_sleep_cleaned.csv"))
    put(x$personsummary_cleaned, file.path(results, "part4_summary_sleep_cleaned.csv"))
  }
  invisible(written)
}

#' Print Method for a Sleep Report
#'
#' @param x A canhrActi_raw_sleep_report.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_sleep_report <- function(x, ...) {
  meta <- attr(x, "canhrActi")
  cat("\ncanhrActi sleep report (GGIR part 4: g.report.part4)\n")
  cat("  recordings:  ", meta$n_recordings, "\n", sep = "")
  cat("  window:      ", meta$settings$sleepwindowType,
      if (isTRUE(meta$settings$only.use.sleeplog)) ", sleep diary used" else ", no sleep diary",
      "\n", sep = "")
  cat("  part 5:      ",
      if (isTRUE(meta$settings$part5_merged)) "merged" else "not supplied", "\n", sep = "")
  if (!identical(meta$status$state, "ok")) {
    cat("  state:       ", meta$status$state, "\n", sep = "")
  }
  for (nm in c("nightsummary_full", "nightsummary_cleaned",
               "personsummary_full", "personsummary_cleaned")) {
    cat("  ", formatC(nm, width = -22), nrow(x[[nm]]), " rows x ", ncol(x[[nm]]),
        " columns\n", sep = "")
  }
  if (length(meta$status$messages) > 0) {
    cat("  messages:\n")
    for (m in meta$status$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(meta$elapsed) && !is.na(meta$elapsed)) {
    cat("  elapsed:     ", round(meta$elapsed, 3), " s\n", sep = "")
  }
  invisible(x)
}
