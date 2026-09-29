# Ported from GGIR 3.3-6 R/g.analyse.perfile.R and R/g.analyse.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The companion to raw_analyse_perday.R:
# only the five aggregates over the valid days and the full-recording means are ported.
# The day sets, the weighting and the operand order are GGIR's.

# The metashort columns with a "<metric>_fullRecordingMean": all but the timestamp and
# the angles, in column order
.raw.full.recording.metrics <- function(ms) {
  nm <- names(ms)[-1]
  nm[!nm %in% c("angle", "anglex", "angley", "anglez", "ExtSleep", "marker")]
}

# GGIR's AveAccAve24hr, the "<metric>_fullRecordingMean" column. Not the mean of
# imputed$averageday: this accumulator is anchored at row 1 and includes the
# tail-expansion epochs the average day leaves out. NA without a single worn long
# epoch, and in the state g.part2 stores it (15 significant digits)
.raw.full.recording.mean <- function(x, metric) {
  ms <- x$imputed$metashort
  if (!is.data.frame(ms) || !metric %in% names(ms)) return(NA_real_)
  rout <- x$imputed$rout %||% x$wear$rout
  if (!is.null(rout) && length(which(as.numeric(as.matrix(rout[, 5])) == 0)) == 0) return(NA_real_)
  ws3 <- as.numeric(x$meta$windowsizes[1])
  n_ws3_perday <- 1440 * (60 / ws3)
  v <- as.numeric(ms[[metric]])
  ND <- length(v) / n_ws3_perday
  acc <- rep(0, n_ws3_perday)
  cnt <- rep(0, n_ws3_perday)
  add <- function(day) {
    val <- which(!is.na(day))
    acc[val] <<- acc[val] + day[val]
    cnt[val] <<- cnt[val] + 1
  }
  if (floor(ND) != 0) {
    for (j in 1:floor(ND)) add(v[(((j - 1) * n_ws3_perday) + 1):(j * n_ws3_perday)])
  }
  if (floor(ND) < ND) {
    add(if (floor(ND) == 0) v else v[((floor(ND) * n_ws3_perday) + 1):length(v)])
  }
  out <- as.numeric(as.character(mean(acc / cnt) * .raw.perday.rescale(metric)))
  if (is.nan(out)) NA_real_ else out
}

# GGIR's day sets: a day counts when n_hours is present and n_valid_hours reaches
# includedaycrit
.raw.perfile.daysets <- function(daily, crit) {
  ok <- !is.na(daily$n_hours) & daily$n_valid_hours >= crit
  we <- daily$weekday %in% c("Saturday", "Sunday")
  list(valid = which(ok), wkend = which(ok & we), wkday = which(ok & !we))
}

#' The Five Per-File Aggregates of a Day Table
#'
#' GGIR's part-2 file summary: AD is the mean over every valid day, WE and WD the
#' means over the valid weekend and weekday days. WWE and WWD are the weighted
#' versions: with more than two weekend days the first and third are averaged
#' together first, and with more than five weekdays the first and sixth are, so a
#' recording spanning eight days does not count one weekday twice.
#'
#' @param daysummary Day table from \code{.raw.analyse.perday}, with every metric's
#'   1-6am column.
#' @param daily The recording's \code{wear$daily}.
#' @param crit includedaycrit.
#' @return A one-row data frame, the metric columns prefixed AD_, WD_, WE_, WWD_ and
#'   WWE_ in that order, which is the order part2_summary uses. The values are those of
#'   GGIR's character summary after g.part2's numeric conversion.
#' @keywords internal
#' @noRd
.raw.analyse.perfile <- function(daysummary, daily, crit) {
  s <- .raw.perfile.daysets(daily, crit)
  cols <- names(daysummary)
  agg <- function(idx, weighted, n_keep) {
    vapply(cols, function(n) {
      # the day values as GGIR's character day table holds them
      v <- suppressWarnings(as.numeric(as.character(daysummary[idx, n])))
      if (weighted && length(v) > n_keep) {
        v <- if (n_keep == 2) c((v[1] + v[3]) / 2, v[2])
             else c((v[1] + v[6]) / 2, v[2:5])
      }
      if (length(v) == 0) return(NA_real_)
      mean(v, na.rm = TRUE)
    }, numeric(1))
  }
  out <- c(
    setNames(agg(s$valid, FALSE, 0), paste0("AD_",  cols)),
    setNames(agg(s$wkday, FALSE, 0), paste0("WD_",  cols)),
    setNames(agg(s$wkend, FALSE, 0), paste0("WE_",  cols)),
    setNames(agg(s$wkday, TRUE,  5), paste0("WWD_", cols)),
    setNames(agg(s$wkend, TRUE,  2), paste0("WWE_", cols)))
  # mean() of nothing is NaN; GGIR leaves those cells empty
  out[is.nan(out)] <- NA_real_
  out <- stats::setNames(as.numeric(as.character(out)), names(out))
  as.data.frame(as.list(out), check.names = FALSE)
}

# g.analyse.perfile's column order for a summary row: everything before the first name
# matching "AD_|WE_|WD_|WWD_|WWE_", then the AD_, WD_, WE_, WWD_ and WWE_ blocks, then the
# rest in place. "MAD_fullRecordingMean" matches too, so with MAD the full-recording
# means after ENMO's and the valid-day counts move behind the blocks.
.raw.perfile.order <- function(cols) {
  first <- grep("AD_|WE_|WD_|WWD_|WWE_", cols)[1]
  if (is.na(first)) return(cols)
  sel <- c(cols[seq_len(first - 1)],
           grep("^AD_", cols, value = TRUE), grep("^WD_", cols, value = TRUE),
           grep("^WE_", cols, value = TRUE), grep("^WWD_", cols, value = TRUE),
           grep("^WWE_", cols, value = TRUE))
  c(sel, cols[!cols %in% sel])
}

# GGIR's character cells: NA and NaN become " "
.raw.part2.cells <- function(v) {
  v[is.na(v)] <- " "
  v[grep("NaN", v)] <- " "
  v
}

#' GGIR's Part-2 SUM of One Recording
#'
#' The summary and daysummary g.analyse returns, in the state g.part2 saves them in
#' meta/ms2.out, so that GGIR's own part-2 report can be run on a folder canhrActi
#' wrote. Both are built as g.analyse.perfile builds them: every cell goes through a
#' character vector, NA and NaN become " ", the day table keeps its first 1-6am column,
#' the summary columns take GGIR's order, and every column but ID becomes numeric where
#' it can (g.part2's char2num_df).
#'
#' @param x A canhrActi_raw with its imputed series.
#' @return list(summary, daysummary, cosinor_ts = NULL), or NULL for a configuration
#'   this port does not build: the refusals of \code{.raw.perday.supported}, cosinor,
#'   nonwearFiltermaxHours, recording_split_times, maxRecordingInterval,
#'   storefolderstructure and a tail expansion.
#' @keywords internal
#' @noRd
.raw.part2.sum <- function(x) {
  pr <- x$params %||% list()
  if (!isTRUE(.raw.perday.supported(.raw.perday.params(pr)))) return(NULL)
  if (isTRUE(pr$cosinor) || !is.null(pr$nonwearFiltermaxHours) ||
      !is.null(pr$recording_split_times) || !is.null(pr$maxRecordingInterval) ||
      isTRUE(pr$storefolderstructure) || length(x$meta$tail_expansion_log) != 0) {
    return(NULL)
  }
  ms <- x$imputed$metashort
  d <- x$wear$daily
  if (!is.data.frame(ms) || nrow(ms) == 0 || !is.data.frame(d) || nrow(d) == 0) return(NULL)
  info <- x$inspection
  hv <- info$header_vars %||% list()
  ID <- as.character(info$id)
  fname <- as.character(info$filename)
  loc <- as.character(hv$sensor.location %||% "not extracted")
  crit <- as.numeric(pr$includedaycrit %||% 16)[1]
  day <- .raw.analyse.perday(x)

  # daysummary: the eight day fields, then the metrics with the first 1-6am column only
  rep <- .raw.perday.report(day)
  n <- nrow(d)
  ds <- list(ID = rep(ID, n), filename = rep(fname, n), calendar_date = d$start_time,
             bodylocation = rep(loc, n), "N valid hours" = d$n_valid_hours,
             "N hours" = d$n_hours, weekday = d$weekday, measurementday = d$day)
  ds <- c(ds, as.list(rep))
  mat <- matrix(" ", n, length(ds))
  for (j in seq_along(ds)) mat[, j] <- .raw.part2.cells(as.character(ds[[j]]))
  daysummary <- data.frame(value = mat, stringsAsFactors = FALSE)
  names(daysummary) <- names(ds)

  # summary: identification, data quality and calibration
  C <- as.ggir.C(x$calibration)
  w <- x$wear
  s <- list(ID = ID, device_sn = hv$deviceSerialNumber %||% info$device_serial,
            bodylocation = loc, filename = fname,
            start_time = as.character(x$meta$metalong$timestamp[1]),
            startday = x$meta$wdayname, samplefreq = info$sf, device = info$monn,
            clipping_score = w$clipping_score, meas_dur_dys = w$meas_dur_dys,
            complete_24hcycle = x$imputed$dcomplscore,
            meas_dur_def_proto_day = w$meas_dur_def_proto_day,
            wear_dur_def_proto_day = w$wear_dur_def_proto_day,
            calib_err = if (length(C$cal.error.end) == 0) " " else C$cal.error.end,
            calib_status = C$QCmessage)
  # the full-recording means, then the filehealth block of the QC log
  fm <- .raw.full.recording.metrics(ms)
  for (m in fm) s[[paste0(m, "_fullRecordingMean")]] <- .raw.full.recording.mean(x, m)
  fh <- .raw.quality.filehealth(.raw.quality.file.summary(x$meta$qclog, fname))
  if (!is.null(fh)) s <- c(s, as.list(fh))
  sets <- .raw.perfile.daysets(d, crit)
  s[["N valid weekend days (WE)"]] <- length(sets$wkend)
  s[["N valid weekdays (WD)"]] <- length(sets$wkday)
  if (ncol(day)) s <- c(s, as.list(.raw.analyse.perfile(day, d, crit)))
  prov <- list(pr$data_masking_strategy, pr$hrs.del.start, pr$hrs.del.end, pr$maxdur,
               pr$windowsizes[1], x$imputed$if_hip_long_axis_id %||% "",
               pr$ggir_version_label %||% .RAW_GGIR_VERSION_LABEL)
  names(prov) <- c(paste0("data exclusion stategy (value=1, ignore specific hours;",
                          " value=2, ignore all data before the first midnight and",
                          " after the last midnight)"),
                   "n hours ignored at start of meas (if data_masking_strategy=1)",
                   "n hours ignored at end of meas (if data_masking_strategy=1)",
                   "n days of measurement after which all data is ignored (if data_masking_strategy=1)",
                   "epoch size to which acceleration was averaged (seconds)",
                   "if_hip_long_axis_id", "GGIR version")
  s <- c(s, prov)
  cells <- .raw.part2.cells(vapply(s, function(v) if (length(v) == 0) " " else as.character(v[1]), ""))
  summary <- data.frame(value = t(unname(cells)), stringsAsFactors = FALSE)
  names(summary) <- names(s)
  if (ncol(summary) > 37) summary <- summary[, .raw.perfile.order(names(summary))]

  # g.part2's char2num_df on every column but ID
  noID <- which(colnames(summary) != "ID")
  summary[noID] <- .raw.quality.char2num(summary[noID])
  noIDday <- which(colnames(daysummary) != "ID")
  daysummary[noIDday] <- .raw.quality.char2num(daysummary[noIDday])
  list(summary = summary, daysummary = daysummary, cosinor_ts = NULL)
}
