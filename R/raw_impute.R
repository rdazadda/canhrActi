# Ported from GGIR 3.3-9 R/g.impute.R (the average-day imputation) and R/g.analyse.R (the
# longitudinal axis estimate) (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the average-day block is
# .raw.impute.averageday with metashort, r5, r5long and ws3 as explicit arguments, the
# orientation block is .raw.longitudinal.axis, raw.impute assembles them into one classed
# object and as.ggir.IMP re-emits GGIR's IMP list. No file I/O and no console printing.
# Index construction, operand order, rounding position and column-name tests are GGIR's.

#' Impute Invalid Short Epochs From the Average Day
#'
#' GGIR's g.impute. Every short epoch whose long-epoch wear decision is not zero is set
#' to NA, the series is folded into a day-by-day matrix on a grid anchored at the first
#' epoch of the recording (not at a dayborder), the average day is the row mean of that
#' matrix (row median for step_count, row minimum over NA-to-0 for marker), and every
#' NA cell takes the average-day value for its epoch-of-day. Tail-expansion epochs
#' (r5long -1) are left out of both the average and the fill. The table is then rounded
#' to 4 decimals. This is the series GGIR part 3 reads; it differs from the part-1
#' series wherever an epoch was invalid.
#'
#' @details Kept as in GGIR: a missing average-day value is 1 for a column named "en"
#'   and 0 otherwise; \code{dcomplscore} is assigned inside the metric loop and so
#'   reports the last metric column; the initial score from \code{r5} is overwritten in
#'   both branches, so \code{r5} has no effect on the result; with \code{ndays <= 1}
#'   nothing is imputed and averageday stays a zero matrix.
#'
#' @param metashort GGIR's short-epoch table (column 1 the timestamp, columns 2 onwards
#'   the metrics). A canhrActi metashort must have its numeric \code{time} column
#'   dropped first (\code{\link{as.ggir.M}} does that).
#' @param r5 GGIR's per-long-epoch wear decision (\code{rout[, 5]}).
#' @param r5long r5 expanded to the short epoch (0 valid, 1 invalid, -1 tail expansion).
#' @param ws3 Short epoch length in seconds.
#' @return A list with \code{metashort} (the imputed table), \code{averageday} (a
#'   \code{wpd} by \code{ncol(metashort) - 1} matrix) and \code{dcomplscore}.
#' @keywords internal
#' @noRd
.raw.impute.averageday <- function(metashort, r5, r5long, ws3) {
  n_shortEpoch_permin <- 60 / ws3
  ENi <- which(colnames(metashort) == "en")
  if (length(ENi) == 0) ENi <- -1
  if (nrow(metashort) > length(r5long)) {
    metashort <- metashort[1:length(r5long),]
  }
  wpd <- 1440 * n_shortEpoch_permin # epochs per day
  averageday <- matrix(0, wpd, ncol(metashort) - 1)

  for (mi in 2:ncol(metashort)) {
    # the average day is anchored at the start of the recording, not at dayborder
    metr <- as.numeric(as.matrix(metashort[, mi]))
    # invalid and tail-expansion epochs become NA
    is.na(metr[which(r5long != 0)]) <- TRUE
    imp <- matrix(NA, wpd, ceiling(length(metr) / wpd))
    ndays <- ncol(imp) # days, rounded up
    dcomplscore <- length(which(r5 == 0)) / length(r5)
    if (ndays > 1 ) {
      # all days except the last
      for (j in 1:(ndays - 1)) {
        imp[, j] <- as.numeric(metr[(((j - 1) * wpd) + 1):(j * wpd)])
      }
      # last day
      lastday <- metr[(((ndays - 1) * wpd) + 1):length(metr)]
      imp[1:length(lastday),ndays] <- as.numeric(lastday)
      if (colnames(metashort)[mi] == "step_count") {
        imp3 <- apply(imp, 1, stats::median, na.rm = TRUE)
      } else if (colnames(metashort)[mi] == "marker") {
        imp[is.na(imp)] <- 0 # marker data is not imputed
        imp3 <- apply(imp, 1, min, na.rm = TRUE)
      } else {
        imp3 <- rowMeans(imp, na.rm = TRUE)
      }
      dcomplscore <- length(which(is.nan(imp3) == FALSE | is.na(imp3) == FALSE)) / length(imp3)

      if (length(imp3) < wpd)  {
        dcomplscore <- dcomplscore * (length(imp3)/wpd)
      }
      if (ENi == mi) { # missing EN is 1
        imp3[which(is.nan(imp3) == TRUE | is.na(imp3) == TRUE)] <- 1
      } else { # missing is 0
        imp3[which(is.nan(imp3) == TRUE | is.na(imp3) == TRUE)] <- 0
      }
      averageday[, (mi - 1)] <- imp3
      for (j in 1:ndays) {
        missing <- which(is.na(imp[,j]) == TRUE)
        if (length(missing) > 0) {
          imp[missing,j] <- imp3[missing]
        }
      }
      dim(imp) <- c(length(imp), 1)
      # tail-expansion epochs are not imputed
      toimpute <- which(r5long != -1)
      metashort[toimpute, mi] <- as.numeric(imp[toimpute])
    } else {
      dcomplscore <- length(which(r5long == 0)) / wpd
    }
  }
  n_decimal_places <- 4

  metashort[,2:ncol(metashort)] <- round(metashort[,2:ncol(metashort)], digits = n_decimal_places)
  list(metashort = metashort, averageday = averageday, dcomplscore = dcomplscore)
}

#' Estimate the Longitudinal Axis of a Hip Recording
#'
#' GGIR's part-2 orientation test (g.analyse): of the three per-axis angles, the one
#' whose first \code{Ndays - 1} days correlate most strongly with its last
#' \code{Ndays - 1} days is taken to be the longitudinal axis. Part 3 uses the result
#' only for a hip-worn device when the user did not set \code{longitudinal_axis}. Needs
#' anglex, angley and anglez and at least two whole days; a per-axis SD of zero gives
#' NA, which \code{which.max} ignores. Computed on the imputed table, as in GGIR.
#'
#' @param metashort The imputed short-epoch table (GGIR's IMP$metashort).
#' @param ws3 Short epoch length in seconds.
#' @return \code{""} or an integer in 1:3.
#' @keywords internal
#' @noRd
.raw.longitudinal.axis <- function(metashort, ws3) {
  longitudinal_axis_id <- ""
  epochday <- 24 * 60 * (60/ws3)
  Ndays <- floor(nrow(metashort)/epochday)
  if (length(which(c("anglex","angley","anglez") %in% colnames(metashort) == FALSE)) == 0 &
      Ndays >= 2) {
    Nhalfdays <- Ndays - 1
    CorrA <- rep(0,3)
    cnt <- 1
    for (anglename in  c("anglex","angley","anglez") ) {
      if (stats::sd(metashort[,anglename]) > 0) {
        CorrA[cnt] <- stats::cor(metashort[1:(Nhalfdays*epochday), anglename],
                                 metashort[(((Ndays - Nhalfdays) *
                                               epochday) + 1):(Ndays * epochday), anglename])
      } else {
        CorrA[cnt] <- NA
      }
      cnt <- cnt + 1
    }
    if (length(which(is.na(CorrA) == FALSE)) > 0) {
      longitudinal_axis_id <- which.max(CorrA)
    } else {
      longitudinal_axis_id <- ""
    }
  }
  longitudinal_axis_id
}

#' Resolve the Wear Decision raw.impute Works From
#'
#' Accepts a canhrActi_raw_wear, a GGIR IMP list or any list carrying rout and r5long, and
#' computes one with \code{raw.wear.decision} when none is given.
#'
#' @param wear The supplied wear decision, or NULL.
#' @param meta The epoch tables, for the fallback call.
#' @param params The parameter list, for the fallback call.
#' @return A list with rout and r5long.
#' @keywords internal
#' @noRd
.raw.impute.wear <- function(wear, meta, params) {
  if (is.null(wear)) {
    return(raw.wear.decision(meta, params = params))
  }
  if (!is.list(wear) || !all(c("rout", "r5long") %in% names(wear))) {
    stop("wear must be a canhrActi_raw_wear from raw.wear.decision(), a GGIR IMP list, or NULL",
         call. = FALSE)
  }
  wear
}

#' Average-Day Imputation of the Short-Epoch Series
#'
#' Fills every invalid short epoch of a recording with the value the average day carries
#' at that epoch-of-day, as GGIR's g.impute does, and returns the imputed table with the
#' average day and the data-completeness score. GGIR part 3 reads this series, not the
#' part-1 series, so every sleep result depends on it.
#'
#' @details The wear decision that supplies rout and r5long is
#'   \code{\link{raw.wear.decision}}. The hip longitudinal axis is computed here because
#'   g.analyse computes it on the imputed table; it is the empty string for a wrist
#'   recording, which carries no anglex or angley column. No parameter of
#'   \code{\link{raw.params}} reaches the imputation itself; params only builds the wear
#'   decision when \code{wear} is NULL. The returned \code{metashort} is GGIR-shaped: the
#'   numeric \code{time} column of a canhrActi_raw_meta is dropped
#'   (\code{\link{as.ggir.M}}), so the table is identical() to GGIR's IMP$metashort.
#'
#' @param meta A \code{canhrActi_raw} (its \code{$meta} and \code{$wear} are used), a
#'   \code{canhrActi_raw_meta} from \code{\link{raw.getmeta}}, or GGIR's M list.
#' @param wear A \code{canhrActi_raw_wear} from \code{\link{raw.wear.decision}}, a GGIR
#'   IMP list, or NULL to run the wear decision here.
#' @param params A \code{\link{raw.params}} object, or NULL for GGIR's defaults.
#' @param ... Individual parameter overrides, routed through \code{raw.params()}.
#'
#' @return An object of class "canhrActi_raw_imputed": a list with
#' \describe{
#'   \item{metashort}{The imputed short-epoch table, every metric column rounded to 4
#'     decimals; identical to \code{g.impute()$metashort}.}
#'   \item{rout, r5long}{The wear decision carried through.}
#'   \item{dcomplscore}{GGIR's data completeness score of the last metric column.}
#'   \item{averageday}{A \code{1440 * (60/ws3)} by \code{ncol(metashort) - 1} matrix,
#'     one column per metric.}
#'   \item{windowsizes}{The three epoch lengths.}
#'   \item{data_masking_strategy, LC, LC2, hrs.del.start, hrs.del.end, maxdur,
#'     nonwearHoursFiltered, nonwearEventsFiltered}{The g.impute pass-throughs, carried
#'     from the wear decision for \code{\link{as.ggir.IMP}}.}
#'   \item{if_hip_long_axis_id}{The longitudinal axis estimate, "" or 1, 2 or 3.}
#'   \item{n_short, n_invalid_short, n_expanded_short}{Short epochs in the table, of
#'     which imputed (r5long 1) and tail expansion (r5long -1, never imputed).}
#'   \item{status, file, settings, messages, elapsed}{Bookkeeping.}
#' }
#' For a recording with no epoch tables the object carries NULL tables and
#' \code{status} "no_data", without an error.
#'
#' @examples
#' \dontrun{
#' x <- read.raw.accelerometer(path)
#' imp <- raw.impute(x)
#' identical(imp$metashort, stored_IMP$metashort)
#' }
#' @export
raw.impute <- function(meta, wear = NULL, params = NULL, ...) {
  t0 <- Sys.time()
  if (inherits(meta, "canhrActi_raw")) {
    if (is.null(wear)) wear <- meta$wear
    meta <- meta$meta
  }
  if (!is.list(meta) || !all(c("metalong", "metashort") %in% names(meta))) {
    stop("meta must be a canhrActi_raw, a canhrActi_raw_meta from raw.getmeta(), or a GGIR M list",
         call. = FALSE)
  }
  is_canhr <- inherits(meta, "canhrActi_raw_meta")
  params <- .raw.calibrate.params(list(params = NULL), params, list(...))
  messages <- character()
  ggir_exact <- isTRUE(.raw.param(params, "ggir_exact", TRUE))
  settings <- list(windowsizes = meta$windowsizes, ggir_exact = ggir_exact)
  finish <- function(obj) {
    obj$settings <- settings
    obj$messages <- messages
    obj$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    structure(obj, class = "canhrActi_raw_imputed")
  }
  empty <- function() {
    list(metashort = NULL, rout = NULL, r5long = NULL, dcomplscore = NA_real_,
         averageday = NULL, windowsizes = meta$windowsizes,
         data_masking_strategy = NA_real_, LC = NA_integer_, LC2 = NA_integer_,
         hrs.del.start = NA_real_, hrs.del.end = NA_real_, maxdur = NA_real_,
         nonwearHoursFiltered = 0, nonwearEventsFiltered = 0,
         if_hip_long_axis_id = "", n_short = 0L, n_invalid_short = 0L, n_expanded_short = 0L,
         status = "no_data", file = meta$file)
  }
  if (isTRUE(meta$filecorrupt) || isTRUE(meta$filetooshort) ||
      is.null(meta$metalong) || is.null(meta$metashort) ||
      nrow(meta$metalong) == 0 || nrow(meta$metashort) == 0) {
    messages <- c(messages, "No epoch tables (corrupt, too short or skipped recording); nothing imputed")
    return(finish(empty()))
  }
  wear <- .raw.impute.wear(wear, meta, params)
  if (is.null(wear$rout) || is.null(wear$r5long)) {
    messages <- c(messages, "The wear decision carries no rout or r5long; nothing imputed")
    return(finish(empty()))
  }
  # GGIR-shaped tables, or the numeric time column would be imputed as a metric
  M <- if (is_canhr) as.ggir.M(meta) else meta
  windowsizes <- M$windowsizes # default c(5, 900, 3600)
  if (is.null(windowsizes)) stop("meta has no windowsizes", call. = FALSE)
  ws3 <- windowsizes[1]
  rout <- wear$rout
  r5long <- wear$r5long
  r5 <- as.numeric(as.matrix(rout[, 5]))
  if (nrow(M$metashort) < length(r5long)) {
    messages <- c(messages, paste0("metashort has ", nrow(M$metashort), " rows but r5long has ",
                                   length(r5long),
                                   "; the wear decision was made on a different recording"))
  }
  out <- .raw.impute.averageday(M$metashort, r5, r5long, ws3)
  # on the imputed table, as in g.analyse
  if_hip_long_axis_id <- .raw.longitudinal.axis(out$metashort, ws3)

  obj <- list(metashort = out$metashort, rout = rout, r5long = r5long,
              dcomplscore = out$dcomplscore, averageday = out$averageday,
              windowsizes = windowsizes,
              data_masking_strategy = .raw.param(wear$settings, "data_masking_strategy",
                                                 .raw.param(wear, "data_masking_strategy", NA_real_)),
              LC = .raw.param(wear, "LC", NA_integer_), LC2 = .raw.param(wear, "LC2", NA_integer_),
              hrs.del.start = .raw.param(wear, "hrs.del.start", NA_real_),
              hrs.del.end = .raw.param(wear, "hrs.del.end", NA_real_),
              maxdur = .raw.param(wear, "maxdur", NA_real_),
              nonwearHoursFiltered = .raw.param(wear, "nonwearHoursFiltered", 0),
              nonwearEventsFiltered = .raw.param(wear, "nonwearEventsFiltered", 0),
              if_hip_long_axis_id = if_hip_long_axis_id,
              n_short = nrow(out$metashort),
              n_invalid_short = length(which(r5long == 1)),
              n_expanded_short = length(which(r5long == -1)),
              status = "ok", file = meta$file)
  finish(obj)
}

#' GGIR's IMP List From a canhrActi Imputed Object
#'
#' Returns the 14 members g.impute returns, in GGIR's order, so identical() comparisons
#' against a stored milestone and a milestone export work.
#'
#' @param x A canhrActi_raw_imputed, or a canhrActi_raw carrying one in \code{$imputed}.
#' @return A named list in GGIR's member order.
#' @examples
#' \dontrun{
#' IMP <- as.ggir.IMP(raw.impute(x))
#' identical(IMP$metashort, stored_IMP$metashort)
#' }
#' @export
as.ggir.IMP <- function(x) {
  if (inherits(x, "canhrActi_raw")) x <- x$imputed
  if (!inherits(x, "canhrActi_raw_imputed")) {
    stop("x must be a canhrActi_raw_imputed object from raw.impute()", call. = FALSE)
  }
  list(metashort = x$metashort, rout = x$rout, r5long = x$r5long,
       dcomplscore = x$dcomplscore, averageday = x$averageday,
       windowsizes = x$windowsizes, data_masking_strategy = x$data_masking_strategy,
       LC = x$LC, LC2 = x$LC2, hrs.del.start = x$hrs.del.start, hrs.del.end = x$hrs.del.end,
       maxdur = x$maxdur, nonwearHoursFiltered = x$nonwearHoursFiltered,
       nonwearEventsFiltered = x$nonwearEventsFiltered)
}

#' Print method for an imputed short-epoch object
#'
#' @param x A canhrActi_raw_imputed.
#' @param ... Not used.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_imputed <- function(x, ...) {
  cat("\ncanhrActi imputed epochs (GGIR part 2: the average day of g.impute)\n")
  if (!is.null(x$file$filename) && !is.na(x$file$filename)) {
    cat("  file:        ", x$file$filename, "\n", sep = "")
  }
  if (!identical(x$status, "ok")) {
    cat("  status:      ", x$status, "\n", sep = "")
  } else {
    ws3 <- x$windowsizes[1]
    cat("  epochs:      ", x$n_short, " short (", ws3, " s), of which ", x$n_invalid_short,
        " imputed (", round(100 * x$n_invalid_short / max(1, x$n_short), 1), " percent)",
        if (x$n_expanded_short > 0) {
          paste0(" and ", x$n_expanded_short, " tail expansion, never imputed")
        } else "", "\n", sep = "")
    cat("  metrics:     ", paste(setdiff(colnames(x$metashort), "timestamp"), collapse = ", "),
        "\n", sep = "")
    cat("  average day: ", nrow(x$averageday), " x ", ncol(x$averageday),
        ", completeness score ", signif(x$dcomplscore, 6), " (last metric column)\n", sep = "")
    if (!identical(x$if_hip_long_axis_id, "")) {
      cat("  longitudinal axis: ", x$if_hip_long_axis_id, "\n", sep = "")
    }
  }
  if (length(x$messages) > 0) {
    cat("  messages:\n")
    for (m in x$messages) cat("    ", trimws(m), "\n", sep = "")
  }
  if (!is.null(x$elapsed) && !is.na(x$elapsed)) {
    cat("  elapsed:     ", round(x$elapsed, 2), " s\n", sep = "")
  }
  invisible(x)
}
