# Ported from GGIR 3.3-9 R/g.report.part2.R, R/g.analyse.R, R/g.analyse.perfile.R,
# R/g.part2.R and R/g.part1.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the data_quality_report.csv
# row and the filehealth, calibration and valid-day fields of part2_summary.csv are built
# in memory from the canhrActi objects instead of from milestone files; the file loop,
# the csv writing, the row padding of mixed runs and the merge by file name are removed;
# the canhrActi-only columns and the 25-row check table are additions.

# QC LOG SUMMARY

#' Summarise the Part-1 QC Log as g.analyse Does
#'
#' The minutes and block counts that GGIR derives from \code{M$QClog}: imputed time gaps
#' (gt3x, csv, ad-hoc csv), imputed, checksum-failed and non-incremental blocks (Axivity
#' cwa, Parmay) and the four sampling-frequency bias bands. Verbatim from g.analyse: the
#' non-incremental rows are selected by \code{blockID_current - blockID_next != 1}, which
#' selects every row of an incrementing log, and the frequency bias is tested for as a
#' column named "frequency_bias" that no reader emits, so the four bias bands are never
#' filled.
#'
#' @param QClog GGIR's M$QClog (a data.frame) or NULL.
#' @param fname The recording's file name (GGIR's file_summary$fname).
#' @return A one-row data.frame with the column fname and whichever of Dur_imputed,
#'   Nblocks_imputed, Dur_chsum_failed, Nblocks_chsum_failed, Dur_nonincremental,
#'   Nblocks_nonincremental and the eight Dur_/Nblock_freqissue members GGIR would have set.
#' @keywords internal
#' @noRd
.raw.quality.file.summary <- function(QClog, fname = NA_character_) {
  file_summary <- data.frame(fname = fname, stringsAsFactors = FALSE)
  M <- list(QClog = QClog)
  if (!is.null(M$QClog)) {
    # Summarise the QC log (currently only expected from cwa Axivity, actigraph, and csv files)
    QCsummarise <- function(QClog, wx) {
      x <- ifelse(test = length(wx) > 0,
                  yes = sum(QClog$end[wx] - QClog$start[wx]) / 60,
                  no = 0)
      return(x)
    }
    # total imputation
    if ("imputed" %in% colnames(M$QClog)) {
      impdone <- which(M$QClog$imputed == TRUE)
      if (any(colnames(M$QClog) == "timegaps_min")) {
        file_summary$Dur_imputed <- sum(M$QClog$timegaps_min)
        file_summary$Nblocks_imputed <- sum(M$QClog$timegaps_n)
      } else {
        file_summary$Dur_imputed <- QCsummarise(M$QClog, impdone)
        file_summary$Nblocks_imputed <- length(impdone)
      }
    }

    # checksum
    if ("checksum_pass" %in% colnames(M$QClog)) {
      chsum_failed <- which(M$QClog$checksum_pass == FALSE)
      file_summary$Dur_chsum_failed <- QCsummarise(M$QClog, chsum_failed)
      file_summary$Nblocks_chsum_failed <- length(chsum_failed)

    }

    # nonincremental block ID
    if ("blockID_current" %in% colnames(M$QClog)) {
      nonincremental <- which(M$QClog$blockID_current - M$QClog$blockID_next != 1)
      file_summary$Dur_nonincremental <- QCsummarise(M$QClog, nonincremental)
      file_summary$Nblocks_nonincremental <- length(nonincremental)
    }

    # sampling frequency issues
    if ("frequency_blockheader" %in% colnames(M$QClog)) {
      freqBlockHead <- M$QClog$frequency_blockheader
      frequency_bias <- abs(M$QClog$frequency_observed - freqBlockHead) / freqBlockHead
    }
    if ("frequency_bias" %in% colnames(M$QClog)) {
      freqissue <- which(frequency_bias >= 0.05 & frequency_bias < 0.1)
      file_summary$Dur_freqissue_5_10 <- QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_5_10 <- length(freqissue)

      freqissue <- which(frequency_bias >= 0.1 & frequency_bias < 0.2)
      file_summary$Dur_freqissue_10_20 <- QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_10_20 <- length(freqissue)

      freqissue <- which(frequency_bias >= 0.2 & frequency_bias < 0.3)
      file_summary$Dur_freqissue_20_30 <- QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_20_30 <- length(freqissue)

      freqissue <- which(frequency_bias >= 0.3)
      file_summary$Dur_freqissue_30 <- QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_30 <- length(freqissue)
    }
  }
  file_summary
}

# FILEHEALTH COLUMNS OF PART2_SUMMARY

#' GGIR's char2num_df
#'
#' Verbatim from g.part2: every column becomes \code{as.numeric(as.character(x))}, and a
#' column that cannot be coerced (a warning or an error) is kept as it is. A blank string
#' such as GGIR's " " placeholder converts to NA without a warning, so placeholder columns
#' end up as numeric NA. The character round trip is a 15-significant-digit round trip.
#' @param df A data.frame.
#' @return The data.frame after the conversion.
#' @keywords internal
#' @noRd
.raw.quality.char2num <- function(df) {
  # save as numeric columns that can be coerced to numeric
  df <- lapply(df, function(x) tryCatch(as.numeric(as.character(x)),
                                        error = function(cond) return(x),
                                        warning = function(cond) return(x)))
  # list to data frame
  df <- as.data.frame(df, check.names = FALSE)
  return(df)
}

#' The Filehealth Block of part2_summary as g.analyse.perfile Builds It
#'
#' With a checksum member (cwa, Parmay) the fourteen columns filehealth_totimp_min,
#' filehealth_checksumfail_min, filehealth_niblockid_min, the four filehealth_fbias*_min
#' and their _N counterparts, each " " when the member is absent (the fbias ones always
#' are); with only an imputation member (gt3x, csv, ad-hoc csv) the two columns
#' filehealth_totimp_min and filehealth_totimp_N; otherwise none. The values pass through
#' GGIR's character matrix and the NA/NaN to " " step, then through g.part2's char2num_df,
#' which is the state in which g.report.part2 merges them into the QC report: numeric
#' everywhere, NA where GGIR had a " " placeholder.
#'
#' @param file_summary The data.frame from \code{.raw.quality.file.summary}.
#' @return A one-row data.frame with the filehealth columns, or NULL when GGIR would have
#'   none.
#' @keywords internal
#' @noRd
.raw.quality.filehealth <- function(file_summary) {
  filesummary <- matrix(" ", 1, 150) #matrix to be stored with summary per participant
  s_names <- rep(" ", ncol(filesummary))
  vi <- 1
  # QClog summary
  if ("Dur_chsum_failed" %in% names(file_summary)) {
    # readAxivity QClog: blocks with faulty data are imputed and logged
    filesummary[vi:(vi + 6)] <- c(ifelse(is.null(file_summary$Dur_imputed), " ", file_summary$Dur_imputed), # total imputed
                                  ifelse(is.null(file_summary$Dur_chsum_failed), " ", file_summary$Dur_chsum_failed), # checksum
                                  ifelse(is.null(file_summary$Dur_nonincremental), " ", file_summary$Dur_nonincremental), # nonincremental id between blocks
                                  ifelse(is.null(file_summary$Dur_freqissue_5_10), " ", file_summary$Dur_freqissue_5_10), # bias 5-10%
                                  ifelse(is.null(file_summary$Dur_freqissue_10_20), " ", file_summary$Dur_freqissue_10_20), # bias 10-20%
                                  ifelse(is.null(file_summary$Dur_freqissue_20_30), " ", file_summary$Dur_freqissue_20_30), # bias 20-30%
                                  ifelse(is.null(file_summary$Dur_freqissue_30), " ", file_summary$Dur_freqissue_30)) # bias >30%
    s_names[vi:(vi + 6)] <- c("filehealth_totimp_min",
                              "filehealth_checksumfail_min",
                              "filehealth_niblockid_min", # non incremental block id
                              "filehealth_fbias0510_min", # frequency bias
                              "filehealth_fbias1020_min",
                              "filehealth_fbias2030_min",
                              "filehealth_fbias30_min")
    vi <- vi + 7
    filesummary[vi:(vi + 6)] <- c( ifelse(is.null(file_summary$Nblocks_imputed), " ", file_summary$Nblocks_imputed),
                                   ifelse(is.null(file_summary$Nblocks_chsum_failed), " ", file_summary$Nblocks_chsum_failed),
                                   ifelse(is.null(file_summary$Nblocks_nonincremental), " ", file_summary$Nblocks_nonincremental),
                                   ifelse(is.null(file_summary$Nblock_freqissue_5_10), " ", file_summary$Nblock_freqissue_5_10),
                                   ifelse(is.null(file_summary$Nblock_freqissue_10_20), " ", file_summary$Nblock_freqissue_10_20),
                                   ifelse(is.null(file_summary$Nblock_freqissue_20_30), " ", file_summary$Nblock_freqissue_20_30),
                                   ifelse(is.null(file_summary$Nblock_freqissue_30), " ", file_summary$Nblock_freqissue_30))
    s_names[vi:(vi + 6)] <- c("filehealth_totimp_N",
                              "filehealth_checksumfail_N",
                              "filehealth_niblockid_N",
                              "filehealth_fbias0510_N",
                              "filehealth_fbias1020_N",
                              "filehealth_fbias2030_N",
                              "filehealth_fbias30_N")
    vi <- vi + 7
  } else if ("Dur_imputed" %in% names(file_summary)) {
    # ActiGraph QClog: time gaps, which are idle sleep mode periods
    filesummary[vi:(vi + 1)] <- c(file_summary$Dur_imputed, # total imputed
                                  file_summary$Nblocks_imputed)
    s_names[vi:(vi + 1)] <- c("filehealth_totimp_min",
                              "filehealth_totimp_N")
    vi <- vi + 2
  }
  if (vi == 1) return(NULL)
  # NA and NaN become " ", unnamed columns are cut
  mw <- which(is.na(filesummary) == T)
  mw <- c(mw, grep(pattern = "NaN", x = filesummary))
  if (length(mw) > 0) {
    filesummary[mw] <- " "
  }
  cut <- which(as.character(s_names) == " " | as.character(s_names) == "" | is.na(s_names) == T | duplicated(s_names))
  if (length(cut) > 0) {
    s_names <- s_names[-cut]
    filesummary <- filesummary[-cut]
  }
  filesummary <- data.frame(value = t(filesummary), stringsAsFactors = FALSE) #needs to be t() because it will be a column otherwise
  names(filesummary) <- s_names
  # the summary is converted before it is saved and later merged
  filesummary <- .raw.quality.char2num(filesummary)
  filesummary
}

#' Empty Filehealth Columns for a Recording That Never Reached GGIR Part 2
#'
#' GGIR writes no data_quality_report.csv when every file of a run is corrupt or too short;
#' in a mixed run it pads the row with NA or " " depending on the order of the files. canhrActi
#' always returns the columns the brand's reader would have produced, filled with NA.
#'
#' @param monc,dformc GGIR's monitor and format codes (NULL for a file skipped at
#'   inspection, whose columns are then chosen from the extension: gt3x and csv give the two
#'   gap columns, cwa the fourteen, anything else none).
#' @param extension The file's extension token (any case).
#' @return A one-row data.frame of NA columns (all numeric: GGIR's conversion turns its " "
#'   placeholders into NA too, see \code{.raw.quality.char2num}), or NULL for brands without
#'   a QC log.
#' @keywords internal
#' @noRd
.raw.quality.filehealth.empty <- function(monc, dformc, extension = "") {
  if (is.null(monc) || is.null(dformc)) {
    ext <- tolower(as.character(extension))
    cwa_like <- identical(ext, "cwa")
    gap_like <- ext %in% c("gt3x", "csv")
  } else {
    cwa_like <- (monc == .RAW_MONITOR[["AXIVITY"]] && dformc == .RAW_FORMAT[["CWA"]]) ||
      (monc == .RAW_MONITOR[["PARMAY_MTX"]] && dformc == .RAW_FORMAT[["BIN"]])
    gap_like <- dformc %in% c(.RAW_FORMAT[["GT3X"]], .RAW_FORMAT[["CSV"]], .RAW_FORMAT[["AD_HOC_CSV"]])
  }
  if (cwa_like) {
    nms <- c("filehealth_totimp_min", "filehealth_checksumfail_min", "filehealth_niblockid_min",
             "filehealth_fbias0510_min", "filehealth_fbias1020_min", "filehealth_fbias2030_min",
             "filehealth_fbias30_min",
             "filehealth_totimp_N", "filehealth_checksumfail_N", "filehealth_niblockid_N",
             "filehealth_fbias0510_N", "filehealth_fbias1020_N", "filehealth_fbias2030_N",
             "filehealth_fbias30_N")
    vals <- as.list(rep(NA_real_, length(nms)))
  } else if (gap_like) {
    nms <- c("filehealth_totimp_min", "filehealth_totimp_N")
    vals <- list(NA_real_, NA_real_)
  } else {
    return(NULL)
  }
  names(vals) <- nms
  as.data.frame(vals, check.names = FALSE, stringsAsFactors = FALSE)
}

# BASE QC ROW OF DATA_QUALITY_REPORT

#' GGIR's Default Calibration Object
#'
#' @param use.temp GGIR's use.temp at that point (TRUE).
#' @return The nine-member C list g.part1 starts from.
#' @keywords internal
#' @noRd
.raw.quality.Cdefault <- function(use.temp = TRUE) {
  C <- list(cal.error.end = 0, cal.error.start = 0)
  C$scale <- c(1,1,1)
  C$offset <- c(0,0,0)
  C$tempoffset <-  c(0,0,0)
  C$QCmessage <- "Autocalibration not done"
  C$npoints <- 0
  C$nhoursused <- 0
  C$use.temp <- use.temp
  C
}

#' The 21 Base Columns of data_quality_report.csv for One Recording
#'
#' Verbatim from g.report.part2, preceded by g.part2's corrupt-file rule (a NULL sf marks
#' the file corrupt): the " " placeholders for missing calibration values, the reset of
#' cal.error.start and npoints to 0 for a too-short or corrupt file, the mean temperature
#' over all but the last long epoch as a character string, the header values with ""
#' replaced by "not stored in header", the device serial per brand, and the
#' \code{data.frame()} call with GGIR's column names, order and types. GGIR's
#' \code{if (header != "no header")} in the ad-hoc csv branch fails in R 4.2 and later for
#' a header with more than one row; its intent (\code{!identical(header, "no header")}) is
#' implemented. The file name is used as it is rather than cut from a milestone name.
#'
#' @param I GGIR's inspection list (header, monc, monn, dformc, dformn, sf, decn, filename).
#' @param C GGIR's calibration list (after g.part1's decision rule).
#' @param M GGIR's epoch-table list (filecorrupt, filetooshort, NFilePagesSkipped, metalong,
#'   QClog).
#' @param filename The file name for the filename column.
#' @param skipped TRUE for a file skipped at inspection (sf NULL without being corrupt); the
#'   g.part2 corrupt rule is not applied to it because GGIR never reports such a file.
#' @return A one-row data.frame with GGIR's 21 columns.
#' @keywords internal
#' @noRd
.raw.quality.ggir.qc <- function(I, C, M, filename, skipped = FALSE) {
  # g.part2's corrupt rule
  if (is.null(I$sf) && !skipped) M$filecorrupt <- TRUE
  if (is.null(M$filecorrupt)) M$filecorrupt <- FALSE
  if (is.null(M$filetooshort)) M$filetooshort <- FALSE
  # create data quality report
  if (length(C$cal.error.end) == 0) C$cal.error.end <- " "
  if (M$filetooshort == TRUE | M$filecorrupt == TRUE) {
    C$cal.error.start <- 0
    C$npoints <- 0
  }
  tm <- which(colnames(M$metalong) == "temperaturemean")
  if (length(tm) > 0) {
    tmean <- as.character(mean(as.numeric(as.matrix(M$metalong[1:(nrow(M$metalong) - 1), tm]))))
  } else {
    tmean <- ""
  }
  header <- I$header
  hnames <- hvalues <- character()
  if (is.null(I$sf) == FALSE) {
    header <- I$header
    hnames <- rownames(header)
    hvalues <- as.character(as.matrix(header))
    pp <- which(hvalues == "")
    hvalues[pp] <- c("not stored in header")
  }
  mon <- I$monn
  deviceSerialNumber <- "not extracted"
  if (is.null(mon)) {
    deviceSerialNumber <- "not extracted" # skipped at inspection: no brand known
  } else if (mon == "genea") {
    deviceSerialNumber <- hvalues[which(hnames == "Serial_Number")] #serial number
  } else if (mon == "geneactive") {
    if ("Device_Unique_Serial_Code" %in% hnames) {
      deviceSerialNumber <- hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
      if (I$dformn == "csv") { #if it was stored in csv-format then underscores were replaced by spaces (by company)
        deviceSerialNumber <- hvalues[which(hnames == "Device Unique Serial Code")] #serial number
      }
    } else {
      deviceSerialNumber <- hvalues[which(hnames == "serial_number")] #serial number
    }
  } else if (mon == "actigraph" | mon == "axivity" | mon == "verisense") {
    deviceSerialNumber <- "not extracted"
  } else if (I$monc %in% c(5, 7, 94, 96, 97, 98, 99)) { #movisense 5, parmay matrix 7, Actiwatch 98, Sensewear 99
    deviceSerialNumber <- "not extracted"
  } else if (I$monc == 0) {
    if (!identical(header, "no header")) {
      deviceSerialNumber <- hvalues[which(hnames == "device_serial_number")]
      if (length(deviceSerialNumber) == 0) {
        deviceSerialNumber <- "not extracted"
      }
    } else {
      deviceSerialNumber <- "not extracted"
    }
  }
  if (length(deviceSerialNumber) == 0) deviceSerialNumber <- "not extracted"
  if (length(C$cal.error.start) == 0) {
    C$cal.error.start <- " "
  }
  if (length(C$npoints) == 0) {
    C$npoints <- " "
  }
  if (length(C$tempoffset) == 0) {
    C$tempoffset <- c(0, 0, 0)
  }
  if (length(M$NFilePagesSkipped) == 0) M$NFilePagesSkipped <- 0 # to make the code work for historical part1 output.
  fname2store <- filename
  QC <- data.frame(filename = fname2store,
                   file.corrupt = M$filecorrupt,
                   file.too.short = M$filetooshort,
                   use.temperature = C$use.temp,
                   scale.x = C$scale[1], scale.y = C$scale[2], scale.z = C$scale[3],
                   offset.x = C$offset[1], offset.y = C$offset[2], offset.z = C$offset[3],
                   temperature.offset.x = C$tempoffset[1],  temperature.offset.y = C$tempoffset[2],
                   temperature.offset.z = C$tempoffset[3],
                   cal.error.start = C$cal.error.start,
                   cal.error.end = C$cal.error.end,
                   n.10sec.windows = C$npoints,
                   n.hours.considered = C$nhoursused, QCmessage = C$QCmessage, mean.temp = tmean,
                   device.serial.number = deviceSerialNumber,
                   NFilePagesSkipped = M$NFilePagesSkipped, stringsAsFactors = FALSE)
  QC
}

# QC FIELDS OF PART2_SUMMARY

#' The QC Fields of part2_summary.csv for One Recording
#'
#' samplefreq and device from the inspection, clipping_score, meas_dur_dys,
#' meas_dur_def_proto_day and wear_dur_def_proto_day from the wear decision, calib_err
#' (" " for a missing value) and calib_status from the calibration, and the valid
#' weekend-day and weekday counts, as g.analyse.perfile writes them. Each goes through
#' GGIR's character matrix and then g.part2's char2num_df, which is the in-memory state of
#' SUM$summary; GGIR's csv additionally rounds to three decimals, the values here are
#' unrounded. complete_24hcycle is not carried because canhrActi does not build g.impute's
#' average day.
#'
#' @param I GGIR's inspection list.
#' @param C GGIR's calibration list.
#' @param W A canhrActi_raw_wear (its clipping_score, meas_dur_dys, meas_dur_def_proto_day,
#'   wear_dur_def_proto_day, n_valid_weekend_days, n_valid_weekdays).
#' @param part2_ran Whether GGIR part 2 would have produced a summary row.
#' @return A one-row data.frame with the columns samplefreq, device, clipping_score,
#'   meas_dur_dys, meas_dur_def_proto_day, wear_dur_def_proto_day, calib_err, calib_status,
#'   "N valid weekend days (WE)", "N valid weekdays (WD)".
#' @keywords internal
#' @noRd
.raw.quality.part2.fields <- function(I, C, W, part2_ran) {
  nms <- c("samplefreq", "device", "clipping_score", "meas_dur_dys", "meas_dur_def_proto_day",
           "wear_dur_def_proto_day", "calib_err", "calib_status",
           "N valid weekend days (WE)", "N valid weekdays (WD)")
  if (!part2_ran) {
    out <- data.frame(samplefreq = if (is.null(I$sf)) NA_real_ else as.numeric(I$sf),
                      device = if (is.null(I$monn)) NA_character_ else I$monn,
                      clipping_score = NA_real_, meas_dur_dys = NA_real_,
                      meas_dur_def_proto_day = NA_real_, wear_dur_def_proto_day = NA_real_,
                      calib_err = NA_real_, calib_status = NA_character_,
                      we = NA_real_, wd = NA_real_, stringsAsFactors = FALSE)
    names(out) <- nms
    return(out)
  }
  # GGIR's filesummary has lost its dims by this point; a character vector coerces the same way
  filesummary <- rep(" ", 10)
  filesummary[1] <- I$sf
  filesummary[2] <- I$monn
  filesummary[3] <- W$clipping_score
  filesummary[4] <- W$meas_dur_dys
  filesummary[5] <- W$meas_dur_def_proto_day
  filesummary[6] <- W$wear_dur_def_proto_day
  if (length(C$cal.error.end) == 0)   C$cal.error.end <- c(" ")
  filesummary[7] <- C$cal.error.end
  filesummary[8] <- C$QCmessage
  filesummary[9:10] <- c(W$n_valid_weekend_days, W$n_valid_weekdays)
  mw <- which(is.na(filesummary) == T)
  mw <- c(mw, grep(pattern = "NaN", x = filesummary))
  if (length(mw) > 0) {
    filesummary[mw] <- " "
  }
  filesummary <- data.frame(value = t(filesummary), stringsAsFactors = FALSE)
  names(filesummary) <- nms
  .raw.quality.char2num(filesummary)
}

# CANHRACTI ADDITIONS

#' Long Gaps Filled at Epoch Level, Read Back From the metashort Table
#'
#' GGIR fills a gap longer than \code{max(6 * ws2 / 60, 90)} minutes only up to the
#' long-epoch cuts at raw level and replicates one zeroed short epoch for the remainder.
#' Those replicas are exact copies of one row whose non-angle metrics are 0 (EN 1), so
#' they form a run of identical rows longer than any raw-level fill can be. This counts
#' those runs and the epochs they added; it is the only source of the count once the raw
#' data are gone. Two back-to-back raw fills with identical carried samples and a total
#' above the limit would count as one long gap. canhrActi addition.
#'
#' @param metashort GGIR's metashort (or the canhrActi table; a time column is ignored).
#' @param ws3,ws2 Short and long epoch lengths in seconds.
#' @return list(count, epochs_added, run_lengths, limit_epochs).
#' @keywords internal
#' @noRd
.raw.quality.long.gaps <- function(metashort, ws3 = 5, ws2 = 900) {
  limit <- max(c(((ws2 / 60) * 6), 90)) * 60 / ws3 # GGIR's long-gap limit in short epochs
  empty <- list(count = 0L, epochs_added = 0, run_lengths = numeric(0), limit_epochs = limit)
  if (is.null(metashort) || nrow(metashort) == 0) return(empty)
  cols <- setdiff(names(metashort), c("timestamp", "time"))
  if (length(cols) == 0) return(empty)
  key <- do.call(paste, c(lapply(metashort[cols], as.character), sep = "\r"))
  r <- rle(key)
  ends <- cumsum(r$lengths)
  metric_cols <- cols[!grepl("angle", cols)]
  zeroed <- rep(TRUE, length(r$lengths))
  for (cl in metric_cols) {
    v <- suppressWarnings(as.numeric(metashort[[cl]][ends]))
    target <- if (cl == "EN") 1 else 0
    zeroed <- zeroed & !is.na(v) & v == target
  }
  long <- which(r$lengths > limit & zeroed)
  list(count = length(long), epochs_added = sum(r$lengths[long] - 1),
       run_lengths = r$lengths[long], limit_epochs = limit)
}

#' A UTC Offset as "+HH:MM" From GGIR's "%z" Digits or a Header String
#' @keywords internal
#' @noRd
.raw.quality.offset <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x)) return(NA_character_)
  m <- regmatches(as.character(x), regexec("([+-]?)(\\d{1,2}):?(\\d{2})", as.character(x)))[[1]]
  if (length(m) == 0) return(NA_character_)
  sprintf("%s%02d:%s", if (m[2] == "-") "-" else "+", as.integer(m[3]), m[4])
}

#' Hours of a "+HH:MM" Offset (NA When Unknown)
#' @keywords internal
#' @noRd
.raw.quality.offset.hours <- function(x) {
  if (is.na(x)) return(NA_real_)
  sign <- if (substr(x, 1, 1) == "-") -1 else 1
  sign * (as.numeric(substr(x, 2, 3)) + as.numeric(substr(x, 5, 6)) / 60)
}

#' A Header Timing Field as Wall-Clock Seconds Since 1970 (Device Clock, No Zone)
#'
#' canhrActi inspections carry the parsed gt3x header as header_list with GMT-labelled
#' POSIXct values built from the ticks; a GGIR I list only has the formatted strings,
#' which read.gt3x 1.2.0 formats in GMT (later versions may shift them by the desiredtz
#' offset).
#' @keywords internal
#' @noRd
.raw.quality.header.time <- function(info, I, name) {
  v <- NULL
  if (!is.null(info$header_list) && !is.null(info$header_list[[name]])) {
    v <- info$header_list[[name]]
    if (inherits(v, "POSIXt")) return(as.numeric(v))
  }
  if (is.data.frame(I$header) && name %in% rownames(I$header)) {
    v <- as.character(I$header[name, 1])
  }
  if (is.null(v) || length(v) != 1 || is.na(v)) return(NA_real_)
  t <- suppressWarnings(as.POSIXct(as.character(v), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (is.na(t)) return(NA_real_)
  as.numeric(t)
}

#' Installed Version of a Package as a String, NA When Absent
#' @keywords internal
#' @noRd
.raw.quality.version <- function(pkg) {
  v <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  if (length(v) != 1) NA_character_ else v
}

#' Format a Number for a Message
#' @keywords internal
#' @noRd
.raw.quality.fmt <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  format(round(x, digits), nsmall = 0, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
}

# INPUTS

#' Resolve the Four Objects Into GGIR's I, C, M and a canhrActi Wear Decision
#'
#' @param info A canhrActi_raw_info or a GGIR I list.
#' @param calibration A canhrActi_raw_calibration, a GGIR C list, a data_quality_report
#'   shaped data.frame or csv path, or NULL (GGIR's default C).
#' @param meta A canhrActi_raw_meta, a GGIR M list, or NULL (allowed only for a corrupt or
#'   skipped inspection).
#' @param wear A canhrActi_raw_wear or NULL (computed from meta when the tables exist).
#' @param params A raw.params() object or NULL (the inspection's, else GGIR's defaults).
#' @param overrides Named overrides from ...
#' @return A list with info, cal, meta, I, C, M, W, params, filename, skipped, part2_ran,
#'   sf, windowsizes, desiredtz, tz_used.
#' @keywords internal
#' @noRd
.raw.quality.inputs <- function(info, calibration = NULL, meta = NULL, wear = NULL,
                                params = NULL, overrides = list()) {
  if (!is.list(info) || !("monc" %in% names(info) || "monn" %in% names(info))) {
    stop("info must be a canhrActi_raw_info from raw.inspect() or a GGIR I list", call. = FALSE)
  }
  is_info <- inherits(info, "canhrActi_raw_info")
  skipped <- isTRUE(info$skipped)
  I <- if (is_info) .raw.ggir.I(info) else info
  filename <- if (!is.null(I$filename)) I$filename else basename(as.character(info$path))
  params <- .raw.calibrate.params(info, params, overrides)

  # calibration
  cal <- NULL
  if (is.null(calibration)) {
    C <- .raw.quality.Cdefault()
  } else if (inherits(calibration, "canhrActi_raw_calibration")) {
    cal <- calibration
    C <- as.ggir.C(calibration)
  } else if (is.data.frame(calibration) || is.character(calibration)) {
    cal <- .raw.calibration.supplied(calibration, filename)
    if (is.null(cal)) stop("The supplied calibration table has no row for ", filename, call. = FALSE)
    C <- as.ggir.C(cal)
  } else if (is.list(calibration) && any(c("QCmessage", "cal.error.start", "scale") %in% names(calibration))) {
    C <- calibration
  } else {
    stop("calibration must be NULL, a canhrActi_raw_calibration, a GGIR C list, ",
         "a data_quality_report-shaped data.frame or a csv path", call. = FALSE)
  }

  # epoch tables
  is_meta <- inherits(meta, "canhrActi_raw_meta")
  if (is.null(meta)) {
    if (!(is.null(I$sf) || skipped)) {
      stop("meta is required (raw.getmeta()) unless the inspection found the file corrupt or skipped it",
           call. = FALSE)
    }
    # the corrupt shape of g.getmeta (a skipped file is too short for GGIR)
    M <- list(filecorrupt = !skipped, filetooshort = skipped, NFilePagesSkipped = 0,
              metalong = c(), metashort = c(), wday = c(), wdayname = c(),
              windowsizes = c(), bsc_qc = data.frame(time = c(), size = c()), QClog = NULL)
  } else if (is_meta) {
    M <- as.ggir.M(meta)
  } else if (is.list(meta) && all(c("filecorrupt", "filetooshort") %in% names(meta))) {
    M <- meta
  } else {
    stop("meta must be a canhrActi_raw_meta from raw.getmeta() or a GGIR M list", call. = FALSE)
  }
  has_tables <- !isTRUE(M$filecorrupt) && !isTRUE(M$filetooshort) &&
    !is.null(M$metalong) && !is.null(M$metashort) &&
    NROW(M$metalong) > 0 && NROW(M$metashort) > 0 && !is.null(I$sf)

  # wear decision
  if (is.null(wear)) {
    if (has_tables) {
      W <- raw.wear.decision(if (is_meta) meta else M, params = params)
    } else {
      W <- NULL
    }
  } else if (inherits(wear, "canhrActi_raw_wear")) {
    W <- wear
  } else {
    stop("wear must be NULL or a canhrActi_raw_wear from raw.wear.decision()", call. = FALSE)
  }
  part2_ran <- has_tables && !is.null(W) && identical(W$status, "ok")

  windowsizes <- if (!is.null(M$windowsizes)) M$windowsizes else .raw.param(params, "windowsizes", c(5, 900, 3600))
  desiredtz <- .raw.param(params, "desiredtz", "")
  if (is_meta && !is.null(meta$settings$desiredtz)) desiredtz <- meta$settings$desiredtz
  if (length(desiredtz) == 0) desiredtz <- ""
  tz_used <- if (nzchar(desiredtz)) desiredtz else Sys.timezone()

  list(info = info, is_info = is_info, cal = cal, meta = if (is_meta) meta else NULL,
       I = I, C = C, M = M, W = W, params = params, filename = filename, skipped = skipped,
       has_tables = has_tables, part2_ran = part2_ran, sf = I$sf, windowsizes = windowsizes,
       desiredtz = desiredtz, tz_used = tz_used)
}

#' The canhrActi Columns of the Quality Report
#'
#' @param ctx The list from \code{.raw.quality.inputs}.
#' @return A one-row data.frame; see \code{raw.quality.report} for the columns.
#' @keywords internal
#' @noRd
.raw.quality.extras <- function(ctx) {
  info <- ctx$info; I <- ctx$I; M <- ctx$M; meta <- ctx$meta; cal <- ctx$cal
  ws3 <- ctx$windowsizes[1]; ws2 <- ctx$windowsizes[2]
  sf <- ctx$sf
  # gaps above the long-gap limit and the epochs they added (canhrActi meta: the block trace)
  lg <- if (ctx$has_tables) .raw.quality.long.gaps(M$metashort, ws3, ws2) else NULL
  gap_count <- if (is.null(lg)) NA_integer_ else as.integer(lg$count)
  minutes_epoch <- NA_real_
  if (!is.null(lg)) {
    if (!is.null(meta) && is.data.frame(meta$chunks) && "epochs_short_added" %in% names(meta$chunks)) {
      minutes_epoch <- sum(meta$chunks$epochs_short_added) * ws3 / 60
    } else {
      minutes_epoch <- lg$epochs_added * ws3 / 60
    }
  }
  # trims (only meaningful when epochs were built)
  seconds_start <- NA_real_
  if (ctx$has_tables && !is.null(meta) && is.data.frame(meta$chunks) && nrow(meta$chunks) > 0 && !is.null(sf)) {
    seconds_start <- meta$chunks$rows_dropped_start[1] / sf
  }
  samples_end <- if (ctx$has_tables && !is.null(meta) && !is.null(meta$rows_discarded_end)) meta$rows_discarded_end else NA_real_
  seconds_end <- NA_real_
  header_last <- .raw.quality.header.time(info, I, "Last Sample Time")
  if (ctx$has_tables && !is.na(header_last)) {
    last_ts <- as.character(M$metashort$timestamp[nrow(M$metashort)])
    last_wall <- suppressWarnings(as.POSIXct(substr(last_ts, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    if (!is.na(last_wall)) seconds_end <- header_last - (as.numeric(last_wall) + ws3)
  }
  # zero triplets removed (the calibration pass is the only place that counts them)
  zeros <- NA_real_
  if (!is.null(cal) && isTRUE(cal$attempted) && is.data.frame(cal$chunks) &&
      all(c("zeros_removed", "rows_read") %in% names(cal$chunks)) && sum(cal$chunks$rows_read) > 0) {
    zeros <- sum(cal$chunks$zeros_removed)
  }
  header_tz <- if (!is.null(info$header_timezone)) as.character(info$header_timezone) else NA_character_
  if (is.na(header_tz) && is.data.frame(I$header) && "TimeZone" %in% rownames(I$header)) {
    header_tz <- as.character(I$header["TimeZone", 1])
  }
  data.frame(
    too_small = if (is.null(info$too_small)) NA else isTRUE(info$too_small),
    uppercase_extension = if (is.null(info$uppercase_extension)) NA else isTRUE(info$uppercase_extension),
    header_timezone = header_tz,
    machine_timezone = Sys.timezone(),
    desiredtz_used = ctx$tz_used,
    read_gt3x_version = .raw.quality.version("read.gt3x"),
    ggirread_version = .raw.quality.version("GGIRread"),
    canhrActi_version = .raw.quality.version("canhrActi"),
    gap_count_over_90min = gap_count,
    minutes_epoch_level_imputed = minutes_epoch,
    seconds_trimmed_start = seconds_start,
    seconds_trimmed_end = seconds_end,
    samples_discarded_end = samples_end,
    zero_triplets_removed = zeros,
    stringsAsFactors = FALSE)
}

# REPORT ROW

#' Data Quality Report of a Raw Accelerometer Recording
#'
#' One row with the columns GGIR writes to results/QC/data_quality_report.csv, the quality
#' fields of results/part2_summary.csv and a set of canhrActi additions, built in memory
#' from the objects of the raw pipeline. With GGIR's defaults the GGIR columns equal the
#' stored csv row column by column.
#'
#' @details
#' GGIR columns, in GGIR's order and with the types of GGIR's in-memory row: filename,
#' file.corrupt, file.too.short, use.temperature, scale.x/y/z, offset.x/y/z,
#' temperature.offset.x/y/z, cal.error.start, cal.error.end, n.10sec.windows,
#' n.hours.considered, QCmessage, mean.temp, device.serial.number, NFilePagesSkipped, then
#' the filehealth columns derived from the QC log: filehealth_totimp_min and
#' filehealth_totimp_N for gt3x, csv and ad-hoc csv; the fourteen cwa columns
#' (checksumfail, niblockid and the four frequency-bias bands, which GGIR never fills and
#' are therefore NA) for Axivity cwa and Parmay; none for GENEActiv and Movisens. GGIR's
#' " " placeholders, its character mean.temp and its character round trip of the
#' filehealth values are kept. When the recording never reached GGIR part 2 (corrupt, too
#' short, skipped) the filehealth columns are NA; for a skipped file the column set
#' follows the extension.
#'
#' part2_summary fields: samplefreq, device, clipping_score, meas_dur_dys,
#' meas_dur_def_proto_day, wear_dur_def_proto_day, calib_err (NA when no fit was made),
#' calib_status, "N valid weekend days (WE)", "N valid weekdays (WD)"; NA when part 2 did
#' not run. GGIR's csv rounds these to three decimals; the values here are unrounded.
#'
#' canhrActi additions: too_small, uppercase_extension, header_timezone (the device's
#' TimeZone header, which GGIR ignores), machine_timezone, desiredtz_used,
#' read_gt3x_version, ggirread_version, canhrActi_version, gap_count_over_90min and
#' minutes_epoch_level_imputed (gaps filled at epoch level with nonwearscore 3),
#' seconds_trimmed_start, seconds_trimmed_end (the header's last sample minus the end of
#' the last epoch), samples_discarded_end and zero_triplets_removed. Values that need the
#' canhrActi block traces are NA when GGIR lists are supplied.
#'
#' @param info A \code{canhrActi_raw_info} from \code{\link{raw.inspect}}, or GGIR's I list
#'   from a meta_*.RData milestone.
#' @param calibration A \code{canhrActi_raw_calibration} from \code{\link{raw.calibrate}}, a
#'   GGIR C list, a data_quality_report-shaped data.frame or csv path, or NULL for GGIR's
#'   default C (identity, "Autocalibration not done").
#' @param meta A \code{canhrActi_raw_meta} from \code{\link{raw.getmeta}} or GGIR's M list;
#'   may be NULL only for a corrupt or skipped inspection.
#' @param wear A \code{canhrActi_raw_wear} from \code{\link{raw.wear.decision}}, or NULL to
#'   compute it from \code{meta}.
#' @param params A \code{\link{raw.params}} object, or NULL for the inspection's parameters
#'   (GGIR's defaults when absent).
#' @param ... Individual parameter overrides, routed through \code{raw.params()}.
#'
#' @return A one-row data.frame (row names removed, non-syntactic names kept) with the
#'   columns described in Details, in that order.
#'
#' @examples
#' \dontrun{
#' info <- raw.inspect("recording.gt3x", desiredtz = "America/Anchorage")
#' cal <- raw.calibrate(info)
#' meta <- raw.getmeta(info, cal)
#' qc <- raw.quality.report(info, cal, meta)
#' t(qc)
#' }
#' @export
raw.quality.report <- function(info, calibration = NULL, meta = NULL, wear = NULL,
                               params = NULL, ...) {
  ctx <- .raw.quality.inputs(info, calibration, meta, wear, params, list(...))
  QC <- .raw.quality.ggir.qc(ctx$I, ctx$C, ctx$M, ctx$filename, skipped = ctx$skipped)
  # the filehealth columns of the part-2 summary, merged by filename
  if (ctx$part2_ran) {
    file_summary <- .raw.quality.file.summary(ctx$M$QClog, fname = ctx$I$filename)
    fh <- .raw.quality.filehealth(file_summary)
    if (!is.null(fh)) {
      SUMMARY <- cbind(data.frame(filename = ctx$filename, stringsAsFactors = FALSE), fh)
      filehealth_cols <- grep(pattern = "filehealth", x = names(SUMMARY), value = FALSE)
      QC <- merge(x = QC, y = SUMMARY[, c(which(names(SUMMARY) == "filename"), filehealth_cols)], by = "filename")
    }
  } else {
    fh <- .raw.quality.filehealth.empty(ctx$I$monc, ctx$I$dformc, extension = .raw.extension(ctx$filename))
    if (!is.null(fh)) QC <- cbind(QC, fh)
  }
  p2 <- .raw.quality.part2.fields(ctx$I, ctx$C, ctx$W, ctx$part2_ran)
  extras <- .raw.quality.extras(ctx)
  out <- cbind(QC, p2, extras)
  names(out) <- c(names(QC), names(p2), names(extras))
  rownames(out) <- NULL
  out
}

# CHECK TABLE

#' One Row of the Check Table
#' @keywords internal
#' @noRd
.raw.quality.check <- function(id, check, status, value, threshold, message) {
  status <- match.arg(status, c("ok", "warn", "fail", "info"))
  data.frame(id = as.integer(id), check = check, status = status,
             value = as.character(value), threshold = as.character(threshold),
             message = message, stringsAsFactors = FALSE)
}

#' Brand Label for Messages
#' @keywords internal
#' @noRd
.raw.quality.brand <- function(monn) {
  if (is.null(monn) || is.na(monn)) return("unknown")
  switch(monn, actigraph = "ActiGraph", geneactive = "GENEActiv", genea = "GENEA",
         axivity = "Axivity", movisens = "Movisens", verisense = "Verisense",
         parmay_mtx = "Parmay Matrix", unknown = "ad-hoc csv", monn)
}

#' Data Quality Checks of a Raw Accelerometer Recording
#'
#' Twenty-five checks, one row each, with a status, the value found, the threshold applied
#' and a sentence for the user. Every number comes from the objects of the raw pipeline
#' (or from \code{\link{raw.quality.report}}); nothing is re-read from the file. The
#' statuses are canhrActi's reading of GGIR's rules: "fail" when GGIR would not analyse
#' the recording (or an input cannot be read), "warn" when a value crosses a GGIR
#' threshold or a plausibility limit stated in the threshold column, "ok" when it does
#' not, "info" for facts GGIR records without judging them.
#'
#' @details
#' Checks: 1 file size against minimumFileSizeMB; 2 read permission; 3 recognised format
#' and extension case; 4 header parsable; 5 sample rate, with the GENEActiv page-header
#' override; 6 time gaps and idle sleep from the QC log; 7 gaps above the 90-minute limit
#' filled at epoch level; 8 all-zero samples removed (counted by the calibration pass); 9
#' the 2-hour floor of the first block; 10 the start trim to the next whole long epoch; 11
#' the samples after the last whole long epoch; 12 to 15 the auto-calibration status, the
#' 0.01 g target, the sphere coverage at spherecrit and the size of the coefficients; 16
#' temperature use; 17 the dynamic range source; 18 clipping; 19 the non-wear score
#' distribution; 20 wear time; 21 valid days at includedaycrit; 22 the device timezone
#' header against the zone the timestamps carry (GGIR ignores the header); 23 unexpected
#' resets from the header (not checked by GGIR); 24 the header span against the tables; 25
#' the parser versions.
#'
#' @inheritParams raw.quality.report
#' @return A data.frame with the columns id, check, status ("ok", "warn", "fail" or "info"),
#'   value, threshold and message (all character except id), one row per check, 25 rows.
#'
#' @examples
#' \dontrun{
#' checks <- raw.quality.checks(info, cal, meta)
#' checks[checks$status != "ok", c("check", "status", "message")]
#' }
#' @export
raw.quality.checks <- function(info, calibration = NULL, meta = NULL, wear = NULL,
                               params = NULL, ...) {
  ctx <- .raw.quality.inputs(info, calibration, meta, wear, params, list(...))
  report <- raw.quality.report(info, calibration, meta, if (is.null(wear)) ctx$W else wear, params, ...)
  info <- ctx$info; I <- ctx$I; C <- ctx$C; M <- ctx$M; W <- ctx$W; cal <- ctx$cal; meta <- ctx$meta
  params <- ctx$params
  sf <- ctx$sf
  ws3 <- ctx$windowsizes[1]; ws2 <- ctx$windowsizes[2]; ws <- ctx$windowsizes[3]
  f <- .raw.quality.fmt
  rows <- list()
  add <- function(...) rows[[length(rows) + 1]] <<- .raw.quality.check(...)
  brand <- .raw.quality.brand(I$monn)
  ext <- .raw.extension(ctx$filename)
  corrupt <- isTRUE(report$file.corrupt)
  tooshort <- isTRUE(report$file.too.short)
  has <- ctx$has_tables
  n_long <- if (has) nrow(M$metalong) else 0L
  n_short <- if (has) nrow(M$metashort) else 0L

  # 1 file size
  size_mb <- if (!is.null(info$size_bytes)) info$size_bytes / 1e6 else NA_real_
  min_mb <- .raw.param(params, "minimumFileSizeMB", 2)
  if (isTRUE(info$too_small)) {
    add(1, "file_size", if (ctx$skipped) "fail" else "warn", paste0(f(size_mb, 3), " MB"), paste0("> ", min_mb, " MB"),
        paste0("This file is below GGIR's ", min_mb, " MB floor; GGIR would skip it.",
               if (ctx$skipped) " It was skipped." else " canhrActi analysed it anyway."))
  } else {
    add(1, "file_size", if (is.na(size_mb)) "info" else "ok", paste0(f(size_mb, 3), " MB"), paste0("> ", min_mb, " MB"),
        if (is.na(size_mb)) "File size not recorded." else paste0(f(size_mb, 1), " MB, above GGIR's ", min_mb, " MB floor."))
  }
  # 2 readable
  path <- info$path
  if (is.null(path) || !file.exists(path)) {
    add(2, "readable", if (is.null(path)) "info" else "fail", "", "file.access mode 4",
        if (is.null(path)) "No path recorded (GGIR milestone input)." else "The file no longer exists at the recorded path.")
  } else if (file.access(path, 4) == 0) {
    add(2, "readable", "ok", "readable", "file.access mode 4", "The file can be read.")
  } else {
    add(2, "readable", "fail", "no read access", "file.access mode 4", "The file cannot be read (permissions).")
  }
  # 3 recognised format
  if (is.null(I$monn)) {
    add(3, "format", "fail", ext, "csv, bin, wav, cwa, gt3x", "Format not recognised (the file was not inspected).")
  } else if (identical(info$ggir_accepts_extension, FALSE)) {
    add(3, "format", "warn", paste0(brand, " .", ext), "GGIR's case-sensitive extension switch",
        paste0("Extension .", ext, " is not recognised by GGIR (case-sensitive). canhrActi matched it as .",
               tolower(ext), " and read it as ", brand, "."))
  } else if (isTRUE(info$uppercase_extension)) {
    add(3, "format", "info", paste0(brand, " .", ext), "GGIR's case-sensitive extension switch",
        paste0("Format detected as ", brand, " .", ext, "; GGIR renames such a file to .", tolower(ext),
               " on disk, canhrActi read a temporary lowercase copy instead."))
  } else {
    add(3, "format", "ok", paste0(brand, " .", ext), "csv, bin, wav, cwa, gt3x",
        paste0("Format detected as ", brand, " .", ext))
  }
  # 4 header parsable
  n_fields <- if (is.data.frame(I$header)) nrow(I$header) else NA_integer_
  if (corrupt || is.null(I$sf)) {
    add(4, "header", "fail", "not parsed", "header readable",
        if (identical(I$dformn, "gt3x")) "info.txt could not be extracted; the file is corrupt or truncated."
        else "The header could not be parsed; the file is corrupt or truncated.")
  } else {
    add(4, "header", "ok", paste0(if (is.na(n_fields)) "no" else n_fields, " fields"), "header readable",
        if (is.na(n_fields)) "The file has no header (none expected for this format)."
        else paste0("Header parsed: ", n_fields, " fields."))
  }
  # 5 sample rate
  page_override <- any(grepl("sample frequency used from page header", info$messages, fixed = TRUE))
  if (is.null(I$sf)) {
    add(5, "sample_rate", "fail", "NA", "sf > 0 from the header", "Sample frequency not recognised.")
  } else if (page_override) {
    add(5, "sample_rate", "warn", paste0(I$sf, " Hz"), "header vs page rate within 5 Hz",
        paste0(I$sf, " Hz: page-header rate differs by > 5 Hz, page value used."))
  } else {
    add(5, "sample_rate", "ok", paste0(I$sf, " Hz"), "sf > 0 from the header", paste0(I$sf, " Hz from the header"))
  }
  # 6 gaps
  gaps_n <- if (!is.null(M$QClog) && "timegaps_n" %in% names(M$QClog)) sum(M$QClog$timegaps_n) else NA_real_
  gaps_min <- if (!is.null(M$QClog) && "timegaps_min" %in% names(M$QClog)) sum(M$QClog$timegaps_min) else NA_real_
  rec_min <- if (has) n_long * ws2 / 60 else NA_real_
  if (is.na(gaps_n)) {
    add(6, "gaps", "info", "not logged", "gaps >= 0.25 s imputed by last value",
        if (has) "No time-gap log for this format (the reader handles gaps itself)." else "No epoch tables; gaps not assessed.")
  } else if (gaps_n == 0) {
    add(6, "gaps", "ok", "0 gaps", "warn above 50 percent of the recording", "0 gaps; no samples were imputed.")
  } else {
    frac <- if (is.na(rec_min) || rec_min == 0) NA_real_ else gaps_min / rec_min
    add(6, "gaps", if (!is.na(frac) && frac > 0.5) "warn" else "info",
        paste0(gaps_n, " gaps; ", f(gaps_min, 2), " min"), "warn above 50 percent of the recording",
        paste0(gaps_n, " gaps, ", f(gaps_min, 2), " min (", f(gaps_min / 1440, 1), " of ",
               f(rec_min / 1440, 1), " days) were imputed by last value."))
  }
  # 7 long gaps
  lim_min <- max(6 * ws2 / 60, 90)
  lg_n <- report$gap_count_over_90min
  lg_min <- report$minutes_epoch_level_imputed
  if (is.na(lg_n)) {
    add(7, "long_gaps", "info", "NA", paste0("gaps above ", lim_min, " min"), "No epoch tables; long gaps not assessed.")
  } else if (lg_n == 0) {
    add(7, "long_gaps", "ok", "0", paste0("gaps above ", lim_min, " min"), paste0("0 gaps over ", lim_min, " min."))
  } else {
    add(7, "long_gaps", "warn", paste0(lg_n, " gaps; ", f(lg_min, 2), " min"), paste0("gaps above ", lim_min, " min"),
        paste0(lg_n, " gaps over ", lim_min, " min; ", f(lg_min / 60, 1),
               " h were filled at epoch level and scored non-wear 3."))
  }
  # 8 zero triplets
  zeros <- report$zero_triplets_removed
  partial <- !is.null(cal) && is.data.frame(cal$chunks) && any(cal$chunks$accepted %in% TRUE)
  if (is.na(zeros)) {
    add(8, "zero_triplets", "info", "not counted", "0", "All-zero samples were not counted (no calibration pass on this file).")
  } else if (zeros == 0 && !partial) {
    add(8, "zero_triplets", "ok", "0", "0", "0 all-zero samples (ActiLife zero imputation not present).")
  } else {
    add(8, "zero_triplets", if (zeros > 0) "warn" else "info", if (partial) paste0(">= ", zeros) else as.character(zeros), "0",
        paste0(if (partial) "At least " else "", zeros, " all-zero samples were removed",
               if (partial) " before the calibration pass stopped early" else "",
               " (ActiLife zero imputation or an idle-sleep export); GGIR refills them by last value."))
  }
  # 9 duration floor
  floor_n <- if (is.null(sf)) NA_real_ else sf * ws * 2 + 1
  rows1 <- if (!is.null(meta) && is.data.frame(meta$chunks) && nrow(meta$chunks) > 0) meta$chunks$rows_read[1] else NA_real_
  if (tooshort || (!has && !corrupt)) {
    add(9, "duration_floor", "fail", if (is.na(rows1)) "" else paste0(rows1, " samples"), paste0(">= ", f(floor_n, 0), " samples in block 1"),
        "Fewer than 2 h of recorded data; nothing was analysed.")
  } else if (corrupt) {
    add(9, "duration_floor", "fail", "", paste0(">= ", f(floor_n, 0), " samples in block 1"), "The file could not be read; nothing was analysed.")
  } else {
    add(9, "duration_floor", "ok", if (is.na(rows1)) paste0(n_short, " epochs") else paste0(rows1, " samples"),
        paste0(">= ", f(floor_n, 0), " samples in block 1"),
        if (is.na(rows1)) paste0("The tables hold ", n_short, " short epochs (", f(n_short * ws3 / 3600, 2), " h), above the 2 h floor.")
        else paste0("Block 1 held ", rows1, " samples, above the 2 h floor of ", f(floor_n, 0), "."))
  }
  # 10 start trim
  st <- report$seconds_trimmed_start
  first_ts <- if (has) as.character(M$metashort$timestamp[1]) else NA_character_
  if (!has) {
    add(10, "start_trim", "info", "NA", paste0("next whole ", ws2 / 60, " min"), "No epoch tables.")
  } else if (is.na(st)) {
    add(10, "start_trim", "info", "NA", paste0("next whole ", ws2 / 60, " min"),
        paste0("The first epoch starts at ", substr(first_ts, 12, 19), " (trim not recorded for GGIR milestone input)."))
  } else {
    add(10, "start_trim", "info", paste0(f(st, 3), " s"), paste0("next whole ", ws2 / 60, " min"),
        paste0(f(st, 3), " s trimmed so the first epoch starts at ", substr(first_ts, 12, 19), "."))
  }
  # 11 end trim
  et <- report$seconds_trimmed_end
  se <- report$samples_discarded_end
  if (!has) {
    add(11, "end_trim", "info", "NA", "last whole long epoch", "No epoch tables.")
  } else if (!is.na(et)) {
    add(11, "end_trim", "info", paste0(f(et, 0), " s"), "last whole long epoch",
        paste0(if (et >= 60) paste0(f(et / 60, 1), " min") else paste0(f(et, 0), " s"),
               " after the last whole ", ws2 / 60, "-min epoch were dropped (header last sample minus last epoch)."))
  } else if (!is.na(se) && !is.null(sf)) {
    add(11, "end_trim", "info", paste0(se, " samples"), "last whole long epoch",
        paste0(f(se / sf / 60, 1), " min of samples after the last whole ", ws2 / 60, "-min epoch were dropped."))
  } else {
    add(11, "end_trim", "info", "NA", "last whole long epoch", "End trim not recorded (no header last sample, no block trace).")
  }
  # 12 calibration status
  minload <- if (!is.null(cal$settings$minloadcrit)) cal$settings$minloadcrit else .raw.param(params, "minloadcrit", 168)
  qcmsg <- if (is.null(C$QCmessage)) "" else C$QCmessage
  applied <- if (!is.null(cal)) isTRUE(cal$applied) else
    !(isTRUE(all(as.numeric(C$scale) == 1)) && isTRUE(all(as.numeric(C$offset) == 0)))
  es <- if (is.numeric(C$cal.error.start) && length(C$cal.error.start) == 1) C$cal.error.start else NA_real_
  ee <- if (is.numeric(C$cal.error.end) && length(C$cal.error.end) == 1) C$cal.error.end else NA_real_
  hours <- if (is.numeric(C$nhoursused) && length(C$nhoursused) == 1) C$nhoursused else NA_real_
  thr12 <- paste0("> ", minload, " h, end error < 0.01 g")
  if ((!is.null(cal) && identical(cal$source, "not_attempted")) || (is.null(cal) && qcmsg == "Autocalibration not done")) {
    add(12, "calibration_status", if (corrupt || tooshort) "fail" else "info", qcmsg, thr12,
        if (corrupt) "Autocalibration not done (the file could not be read)."
        else if (isFALSE(.raw.param(params, "do.cal", TRUE))) "Autocalibration not done (do.cal is FALSE)."
        else "Autocalibration not done.")
  } else if (!is.null(cal) && identical(cal$source, "supplied")) {
    add(12, "calibration_status", "info", qcmsg, thr12,
        paste0("Calibration coefficients supplied (", cal$supplied_from, "); no fit was made here. Status recorded: ", qcmsg, "."))
  } else if (grepl("^recalibration done", qcmsg)) {
    add(12, "calibration_status", "ok", qcmsg, thr12,
        paste0("Calibrated with ", f(hours, 1), " h of data; error ", f(es * 1000, 1), " mg -> ", f(ee * 1000, 1),
               " mg; ", qcmsg, "; coefficients ", if (applied) "applied." else "not applied."))
  } else if (grepl("possibly not good enough", qcmsg, fixed = TRUE)) {
    add(12, "calibration_status", "warn", qcmsg, thr12,
        paste0("Calibrated with ", f(hours, 1), " h of data (GGIR wants > ", minload, " h); error ",
               f(es * 1000, 1), " mg -> ", f(ee * 1000, 1), " mg; coefficients ",
               if (applied) "applied." else "not applied (reset to identity by GGIR's rule)."))
  } else {
    add(12, "calibration_status", if (tooshort || corrupt) "fail" else "warn", qcmsg, thr12,
        paste0("Not calibrated: ", qcmsg, if (!is.na(hours)) paste0(" (", f(hours, 1), " h of data)") else "", "."))
  }
  # 13 calibration error
  not_attempted <- (!is.null(cal) && identical(cal$source, "not_attempted")) ||
    (is.null(cal) && qcmsg == "Autocalibration not done")
  if (is.na(ee) || not_attempted) {
    add(13, "calibration_error", "info", "NA", "< 0.01 g", "No post-calibration error (no fit was made).")
  } else {
    add(13, "calibration_error", if (ee < 0.01) "ok" else "warn", paste0(f(ee * 1000, 2), " mg"), "< 0.01 g",
        paste0("Post-calibration error ", f(ee * 1000, 1), " mg is ", if (ee < 0.01) "under" else "not under", " the 10 mg target."))
  }
  # 14 sphere coverage
  spherecrit <- if (!is.null(cal$settings$spherecrit)) cal$settings$spherecrit else .raw.param(params, "spherecrit", 0.3)
  sd <- C$spheredata
  thr14 <- paste0("+/- ", spherecrit, " g on every axis")
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0 || !all(c("meanx", "meany", "meanz") %in% names(sd))) {
    add(14, "sphere_coverage", "info", "NA", thr14, "No still-window sphere data (no fit was attempted).")
  } else {
    covered <- vapply(c("meanx", "meany", "meanz"), function(a) {
      v <- as.numeric(sd[[a]]); min(v) < -spherecrit & max(v) > spherecrit
    }, logical(1))
    if (all(covered)) {
      add(14, "sphere_coverage", "ok", "3 of 3 axes", thr14,
          paste0("All three axes reach beyond +/- ", spherecrit, " g (fit possible)."))
    } else {
      miss <- c("x", "y", "z")[!covered]
      add(14, "sphere_coverage", "warn", paste0(sum(covered), " of 3 axes"), thr14,
          paste0("Axis ", paste(miss, collapse = ", "), " does not reach beyond +/- ", spherecrit, " g (",
                 sum(covered), " of 3 axes covered); GGIR does not fit the sphere."))
    }
  }
  # 15 coefficient sanity
  sc <- as.numeric(C$scale); of <- as.numeric(C$offset)
  thr15 <- "scale within 10 percent, offsets under 100 mg"
  if (length(sc) != 3 || length(of) != 3 || anyNA(sc) || anyNA(of)) {
    add(15, "coefficient_sanity", "info", "NA", thr15, "No coefficients.")
  } else if (all(sc == 1) && all(of == 0)) {
    add(15, "coefficient_sanity", "info", "identity", thr15, "Identity coefficients (no calibration applied).")
  } else {
    dscale <- ceiling(max(abs(sc - 1)) * 1000) / 10
    doff <- ceiling(max(abs(of)) * 1000)
    plaus <- max(abs(sc - 1)) <= 0.10 && max(abs(of)) <= 0.100
    add(15, "coefficient_sanity", if (plaus) "ok" else "warn",
        paste0("scale ", f(dscale, 1), " percent; offset ", doff, " mg"), thr15,
        paste0("Scale within ", f(dscale, 1), " %, offsets under ", doff, " mg: ",
               if (plaus) "plausible." else "implausible for an auto-calibration; check the device."))
  }
  # 16 temperature
  temp_avail <- if (!is.null(cal)) cal$temp_available else NA
  use_temp <- isTRUE(C$use.temp) && length(C$meantempcal) > 0
  thr16 <- "temperature channel present and usable"
  if (isTRUE(use_temp)) {
    add(16, "temperature", "ok", "used", thr16,
        paste0("Temperature channel used in the calibration (mean ", f(C$meantempcal, 1), " C over still windows)."))
  } else if (isTRUE(temp_avail)) {
    why <- grep("temperature ignored", if (is.null(cal)) character() else cal$messages, value = TRUE)
    add(16, "temperature", "warn", "present, not used", thr16,
        paste0("Temperature channel present but not used", if (length(why) > 0) paste0(": ", trimws(why[1])) else "", "."))
  } else {
    add(16, "temperature", "info", "none", thr16, paste0("No temperature channel (", brand, "); temperature not used."))
  }
  # 17 dynamic range
  src <- info$dynrange_source
  dyn <- info$dynrange
  # a GGIR I list: derive what raw.inspect would have stored
  hv_serial <- if (!is.null(I$sf) && !is.null(I$monn)) {
    tryCatch(.raw.header.vars(I)$deviceSerialNumber, error = function(e) "not extracted")
  } else "not extracted"
  if (is.null(src) && !is.null(I$sf) && !is.null(I$monc)) {
    ncb <- .raw.clip.block.params(list(monc = I$monc, dformc = I$dformc, sf = I$sf,
                                       device_serial = hv_serial), params = params, pass = "getmeta")
    src <- ncb$dynrange_source; dyn <- ncb$dynrange
  }
  prefix <- if (!is.null(info$serial_prefix) && !is.na(info$serial_prefix)) info$serial_prefix else
    if (!identical(hv_serial, "not extracted")) substr(trimws(sub("_firmware_.*$", "", hv_serial)), 1, 3) else NA_character_
  thr17 <- "dynrange from serial, user or file"
  if (is.null(I$sf)) {
    add(17, "dynamic_range", "info", "NA", thr17, "Dynamic range not applicable (the file could not be read).")
  } else if (is.null(src) || is.na(src) || is.null(dyn)) {
    add(17, "dynamic_range", "info", "NA", thr17, "Dynamic range not determined (file not inspected).")
  } else if (src == "serial_prefix") {
    add(17, "dynamic_range", "ok", paste0(dyn, " g"), thr17, paste0(dyn, " g from serial prefix ", prefix))
  } else if (src == "user") {
    add(17, "dynamic_range", "info", paste0(dyn, " g"), thr17, paste0(dyn, " g set by the user (dynrange)"))
  } else if (src == "file") {
    add(17, "dynamic_range", "ok", paste0(dyn, " g"), thr17, paste0(dyn, " g read from the file"))
  } else if (src == "movisens_assumed") {
    add(17, "dynamic_range", "info", paste0(dyn, " g"), thr17, paste0(dyn, " g assumed (Movisens)"))
  } else if (src == "rmc.dynamic_range") {
    add(17, "dynamic_range", "info", paste0(dyn, " g"), thr17, paste0(dyn, " g from rmc.dynamic_range"))
  } else {
    add(17, "dynamic_range", "warn", paste0(dyn, " g"), thr17,
        paste0(if (!is.na(prefix) && nzchar(prefix)) paste0(prefix, " prefix: ") else "", dyn,
               " g assumed, set dynrange if the device differs."))
  }
  # 18 clipping
  clipthres <- if (!is.null(meta$settings$clipthres)) meta$settings$clipthres else info$clipthres
  thr18 <- "> 0.3 of samples per long epoch"
  if (!ctx$part2_ran) {
    add(18, "clipping", "info", "NA", thr18, "No epoch tables; clipping not assessed.")
  } else {
    lc2 <- W$LC2
    add(18, "clipping", if (lc2 == 0) "ok" else "warn", paste0(lc2, " of ", n_long, " blocks"), thr18,
        if (lc2 == 0) paste0("0 of ", n_long, " blocks clipped.")
        else paste0(lc2, " of ", n_long, " blocks clipped (more than 30 percent of samples beyond ",
                    if (is.null(clipthres)) "the clipping threshold" else paste0(clipthres, " g"),
                    "); GGIR clipping score ", f(W$clipping_score, 4), "."))
  }
  # 19 non-wear score
  if (!has) {
    add(19, "nonwear_score", "info", "NA", "score 0 to 3 per long epoch", "No epoch tables.")
  } else {
    nw <- table(factor(M$metalong$nonwearscore, levels = c(-1, 0, 1, 2, 3)))
    n3 <- as.integer(nw[["3"]])
    add(19, "nonwear_score", "info",
        paste0("0:", nw[["0"]], " 1:", nw[["1"]], " 2:", nw[["2"]], " 3:", nw[["3"]],
               if (nw[["-1"]] > 0) paste0(" -1:", nw[["-1"]]) else ""),
        "score 0 to 3 per long epoch",
        paste0(n3, " of ", n_long, " blocks scored 3 (all axes still)."))
  }
  # 20 wear time
  if (!ctx$part2_ran) {
    add(20, "wear_time", "info", "NA", "warn below 50 percent worn", "No wear decision (no epoch tables).")
  } else {
    frac <- W$wear_dur_def_proto_day / W$meas_dur_dys
    add(20, "wear_time", if (!is.na(frac) && frac < 0.5) "warn" else "ok",
        paste0(f(W$wear_dur_def_proto_day, 4), " of ", f(W$meas_dur_dys, 4), " days"), "warn below 50 percent worn",
        paste0(f(W$wear_dur_def_proto_day, 2), " days worn of ", f(W$meas_dur_dys, 2), " recorded."))
  }
  # 21 valid days
  crit <- if (!is.null(W$settings$includedaycrit)) W$settings$includedaycrit[1] else .raw.param(params, "includedaycrit", 16)[1]
  if (!ctx$part2_ran) {
    add(21, "valid_days", "info", "NA", paste0(">= ", crit, " h valid per day"), "No wear decision (no epoch tables).")
  } else {
    add(21, "valid_days", if (W$n_valid_days >= 1) "ok" else "warn",
        paste0(W$n_valid_days, " of ", nrow(W$daily)), paste0(">= ", crit, " h valid per day"),
        paste0(W$n_valid_days, " valid days (>= ", crit, " h), ", W$n_valid_weekend_days, " weekend days."))
  }
  # 22 timezone
  head_off <- .raw.quality.offset(report$header_timezone)
  eff_off <- NA_character_
  if (has) {
    ts1 <- as.character(M$metashort$timestamp[1])
    eff_off <- .raw.quality.offset(substr(ts1, nchar(ts1) - 4, nchar(ts1)))
  } else {
    start_wall <- .raw.quality.header.time(info, I, "Start Date")
    if (!is.na(start_wall)) {
      t <- as.POSIXct(format(as.POSIXct(start_wall, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
                      format = "%Y-%m-%d %H:%M:%S", tz = ctx$tz_used)
      eff_off <- .raw.quality.offset(format(t, "%z"))
    }
  }
  tzlab <- paste0(ctx$tz_used, if (!is.na(eff_off)) paste0(" (", eff_off, ")") else "")
  thr22 <- "header offset == timestamp offset"
  if (is.na(head_off)) {
    add(22, "timezone", "info", paste0("header NA; used ", tzlab), thr22,
        paste0("No timezone in the header; timestamps labelled ", tzlab, "."))
  } else if (!is.na(eff_off) && .raw.quality.offset.hours(head_off) == .raw.quality.offset.hours(eff_off)) {
    add(22, "timezone", "ok", paste0("header ", head_off, "; used ", tzlab), thr22,
        paste0("Device configured at ", head_off, "; timestamps labelled ", tzlab, "."))
  } else {
    add(22, "timezone", "warn", paste0("header ", head_off, "; used ", tzlab), thr22,
        paste0("Device configured at ", head_off, "; timestamps labelled ", tzlab,
               ". Set desiredtz/configtz if this is wrong."))
  }
  # 23 unexpected resets
  resets <- NULL
  if (!is.null(info$header_list) && !is.null(info$header_list[["Unexpected Resets"]])) {
    resets <- info$header_list[["Unexpected Resets"]]
  } else if (is.data.frame(I$header) && "Unexpected Resets" %in% rownames(I$header)) {
    resets <- I$header["Unexpected Resets", 1]
  }
  resets <- suppressWarnings(as.numeric(as.character(resets)))
  if (length(resets) != 1 || is.na(resets)) {
    add(23, "unexpected_resets", "info", "NA", "0 (not checked by GGIR)", "No Unexpected Resets field in the header (GGIR does not check it).")
  } else {
    add(23, "unexpected_resets", if (resets > 0) "warn" else "info", as.character(resets), "0 (not checked by GGIR)",
        paste0(resets, " unexpected resets in the header (GGIR does not check this)."))
  }
  # 24 header span vs tables
  h_start <- .raw.quality.header.time(info, I, "Start Date")
  h_last <- .raw.quality.header.time(info, I, "Last Sample Time")
  h_days <- if (is.na(h_start) || is.na(h_last)) NA_real_ else (h_last - h_start) / 86400
  t_days <- if (has) n_short * ws3 / 86400 else NA_real_
  thr24 <- "difference explained by the trims"
  if (is.na(h_days) && is.na(t_days)) {
    add(24, "header_vs_tables", "info", "NA", thr24, "No header span and no tables.")
  } else if (is.na(h_days)) {
    add(24, "header_vs_tables", "info", paste0(f(t_days, 3), " d in tables"), thr24,
        paste0(f(t_days, 3), " d in tables; the header carries no start and last-sample times."))
  } else if (is.na(t_days)) {
    add(24, "header_vs_tables", "info", paste0(f(h_days, 3), " d in header"), thr24,
        paste0(f(h_days, 3), " d in header; no tables were built."))
  } else {
    add(24, "header_vs_tables", "info", paste0(f(h_days, 3), " d vs ", f(t_days, 3), " d"), thr24,
        paste0(f(h_days, 3), " d in header, ", f(t_days, 3), " d in tables (start and end trims)."))
  }
  # 25 versions
  vers <- c(paste0("read.gt3x ", report$read_gt3x_version), paste0("GGIRread ", report$ggirread_version),
            paste0("canhrActi ", report$canhrActi_version))
  sem <- paste0("GGIR ", .RAW_GGIR_VERSION_LABEL, " semantics")
  add(25, "versions", "info", paste(vers, collapse = ", "), sem,
      paste0(paste(vers, collapse = ", "), " produced these numbers (", sem, ")."))

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
