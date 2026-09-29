# Ported from GGIR 3.3-6 R/g.analyse.perday.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; Medical Research Council UK;
# Accelting; French National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Only the twelve columns that
# part2_daysummary carries per accelerometer metric under a single 0-24hr window are
# ported: L5/M5 via .raw.getmx, the 1-6am and 0-24hr means, and the six MVPA columns via
# .raw.getbout, for every metric GGIR analyses. qlevels, ilevels, iglevels, external
# functions, activity logs and recording_split_times are not; a configuration that needs
# them is refused by .raw.perday.supported. The day windows, the validity gate, the
# average-day padding, the unit rescaling, the operand order and the NaN-to-zero rule are
# GGIR's.

# The parameters that appear in the column names, with GGIR's defaults. A NULL
# mvpathreshold means no MVPA columns.
.RAW_PERDAY_DEFAULTS <- list(
  mvpathreshold = NULL,   # may be a vector: 6 columns each
  mvpadur       = c(1, 5, 10),
  boutcriter    = 0.8,
  winhr         = 5,
  M5L5res       = 10,
  qwindow       = c(0, 24),
  MX.ig.min.dur = 10,
  qM5L5         = NULL,
  iglevels      = NULL,
  qlevels       = NULL,
  ilevels       = NULL
)

.raw.perday.params <- function(params = NULL) {
  p <- .RAW_PERDAY_DEFAULTS
  if (is.list(params)) for (k in names(p)) if (!is.null(params[[k]])) p[[k]] <- params[[k]]
  # GGIR fills a NULL mvpathreshold and boutcriter from threshold.mod and
  # boutcriter.mvpa, which is why a default run still writes T100 columns
  if (is.list(params)) {
    if (is.null(p$mvpathreshold)) p$mvpathreshold <- params$threshold.mod
    if (is.null(p$boutcriter))    p$boutcriter    <- params$boutcriter.mvpa
  }
  if (length(p$mvpadur) != 3) p$mvpadur <- c(1, 5, 10)
  if (length(p$mvpadur) > 0) p$mvpadur <- sort(p$mvpadur)
  p
}

# Each of these changes the column set, so the configuration is refused rather than
# guessed at
.raw.perday.supported <- function(p) {
  bad <- character(0)
  if (length(p$qlevels) > 0)  bad <- c(bad, "qlevels")
  if (length(p$ilevels) > 0)  bad <- c(bad, "ilevels")
  if (length(p$iglevels) > 0) bad <- c(bad, "iglevels")
  if (length(p$qM5L5) > 0)    bad <- c(bad, "qM5L5")
  if (!(length(p$qwindow) == 2 && p$qwindow[1] == 0 && p$qwindow[2] == 24)) {
    bad <- c(bad, "qwindow other than c(0, 24)")
  }
  if (length(p$mvpathreshold) > 0 && length(p$mvpadur) != 3) bad <- c(bad, "mvpadur of length != 3")
  if (length(bad) == 0) return(TRUE)
  paste0("This is a partial port of GGIR's part-2 analysis and cannot reproduce ",
         "the columns that would come from: ", paste(bad, collapse = ", "),
         ". Run GGIR itself for that configuration.")
}

# The metashort columns g.analyse.perday analyses (isAccMetric), and those of them that
# are counts rather than g
.RAW_PERDAY_METRICS <- c("ENMO", "LFENMO", "BFEN", "EN", "HFEN", "HFENplus", "MAD", "ENMOa",
                         "ZCX", "ZCY", "ZCZ", "BrondCount_x", "BrondCount_y", "BrondCount_z",
                         "NeishabouriCount_x", "NeishabouriCount_y", "NeishabouriCount_z",
                         "NeishabouriCount_vm", "ExtAct", "ExtHeartRate")
.RAW_PERDAY_COUNTS <- c("ZCX", "ZCY", "ZCZ", "BrondCount_x", "BrondCount_y", "BrondCount_z",
                        "NeishabouriCount_x", "NeishabouriCount_y", "NeishabouriCount_z",
                        "NeishabouriCount_vm", "ExtAct", "ExtHeartRate")

# The analysed metrics of an imputed metashort, in its column order
.raw.perday.metrics <- function(ms) {
  nm <- names(ms)[-1]
  nm[nm %in% .RAW_PERDAY_METRICS]
}

.raw.perday.unit <- function(metric) {
  if (!metric %in% .RAW_PERDAY_COUNTS) "_mg"
  else if (metric == "ExtAct") ""
  else if (metric == "ExtHeartRate") "_bpm"
  else "_cnt"
}

# g to mg, except for the count metrics
.raw.perday.rescale <- function(metric) {
  if (grepl("BrondCount|ZCX|ZCY|ZCZ|marker|NeishabouriCount|ExtAct|ExtHeartRate", metric)) 1 else 1000
}

# The column names, built as GGIR builds them, metric after metric
.raw.perday.colnames <- function(metric, ws3, p) {
  nm <- character(0)
  for (m in metric) {
    unit <- .raw.perday.unit(m)
    nm <- c(nm,
            paste0("L", p$winhr, "hr_", m, unit, "_0-24hr"),
            paste0("L", p$winhr, "_", m, unit, "_0-24hr"),
            paste0("M", p$winhr, "hr_", m, unit, "_0-24hr"),
            paste0("M", p$winhr, "_", m, unit, "_0-24hr"),
            paste0("mean_", m, unit, "_1-6am"),
            paste0("mean_", m, unit, "_0-24hr"))
    for (t in p$mvpathreshold) {
      nm <- c(nm,
        paste0("MVPA_E", ws3, "S_T", t, "_", m, "_0-24hr"),
        paste0("MVPA_E1M_T", t, "_", m, "_0-24hr"),
        paste0("MVPA_E5M_T", t, "_", m, "_0-24hr"),
        paste0("MVPA_E", ws3, "S_B", p$mvpadur[1], "M", p$boutcriter * 100, "%_T", t,
               "_", m, "_0-24hr"),
        paste0("MVPA_E", ws3, "S_B", p$mvpadur[2], "M", p$boutcriter * 100, "%_T", t,
               "_", m, "_0-24hr"),
        paste0("MVPA_E", ws3, "S_B", p$mvpadur[3], "M", p$boutcriter * 100, "%_T", t,
               "_", m, "_0-24hr"))
    }
  }
  nm
}

# part2_daysummary keeps only the first 1-6am column, as g.analyse.perfile does; the
# summary aggregates are taken before that
.raw.perday.report <- function(day) {
  k <- grep("1-6am", names(day))
  if (length(k) > 1) day <- day[, -k[-1], drop = FALSE]
  day
}

# GGIR pads a day shorter than the average day out of the average day itself. A
# 25 hour day keeps all 25: g.analyse.perday means to drop the doubled hour, but
# vari[-c(start:end)] on its data frame drops columns that do not exist
.raw.perday.pad <- function(v, avg, di, ws3) {
  deltaLength <- length(v) - length(avg)
  if (deltaLength < 0) {
    if (di == 1) {
      v <- c(avg[1:abs(deltaLength)], v)
    } else if (length(v) == 23 * 60 * (60 / ws3)) {
      s <- 2 * 60 * (60 / ws3) + 1; e <- 3 * 60 * (60 / ws3)
      v <- c(v[1:(s - 1)], avg[s:e], v[s:length(v)])
    } else {
      v <- c(v, avg[(length(avg) - abs(deltaLength) + 1):length(avg)])
    }
  }
  v
}

.raw.perday.mvpa <- function(varnum, ws3, threshold, p, UnitReScale) {
  mvpa <- rep(0, 6)
  if (length(varnum) < 100) return(mvpa)
  mvpa[1] <- length(which(varnum * UnitReScale >= threshold)) / (60 / ws3)
  varnum2 <- cumsum(c(0, varnum))
  select <- seq(1, length(varnum2), by = 60 / ws3)
  varnum3 <- diff(varnum2[round(select)]) / abs(diff(round(select)))
  mvpa[2] <- length(which(varnum3 * UnitReScale >= threshold))
  select <- seq(1, length(varnum2), by = 300 / ws3)
  varnum3 <- diff(varnum2[round(select)]) / abs(diff(round(select)))
  mvpa[3] <- length(which(varnum3 * UnitReScale >= threshold)) * 5
  for (j in 1:3) {
    boutduration <- p$mvpadur[j] * (60 / ws3)
    rr1 <- matrix(0, length(varnum), 1)
    rr1[which(varnum * UnitReScale >= threshold)] <- 1
    got <- .raw.getbout(rr1, boutduration, p$boutcriter, ws3)
    mvpa[3 + j] <- length(which(got == 1)) / (60 / ws3)
  }
  if (length(which(is.nan(mvpa) == TRUE)) > 0) mvpa[which(is.nan(mvpa) == TRUE)] <- 0
  mvpa
}

#' Per-Day Activity Metrics (GGIR Part 2, Partial Port)
#'
#' The metric half of GGIR's \code{part2_daysummary.csv} under a single 0-24hr window,
#' for every metric g.analyse.perday analyses, metric after metric in metashort order.
#' Reads the imputed short-epoch series, which is the series GGIR's own part-2 analysis
#' reads; the part-1 series would change every day that contains non-wear.
#'
#' @param x A canhrActi_raw pipeline result.
#' @param metric Columns of the imputed metashort to analyse, default every metric
#'   GGIR analyses (\code{.raw.perday.metrics}).
#' @param params Optional overrides for the analysis parameters. Anything not
#'   given falls back to GGIR's default.
#'
#' @return A data frame, one row per day of \code{x$wear$daily}, carrying GGIR's column
#'   names with every metric's 1-6am column, which the summary aggregates read;
#'   \code{.raw.perday.report} drops all but the first, as part2_daysummary does. A day
#'   below \code{includedaycrit} is all NA, which is what GGIR writes for it. GGIR names a
#'   column on the first valid day that has it and puts later ones at the end, so with no
#'   valid day there are no columns, and a first valid day of 6 hours or less moves the
#'   1-6am columns behind the rest. The values are those GGIR's character day table holds
#'   (15 significant digits) after g.part2 turns it back into numbers.
#' @keywords internal
#' @noRd
.raw.analyse.perday <- function(x, metric = NULL, params = NULL) {
  p <- .raw.perday.params(params %||% x$params)
  ok <- .raw.perday.supported(p)
  if (!isTRUE(ok)) stop(ok, call. = FALSE)

  ms <- x$imputed$metashort
  if (!is.data.frame(ms) || nrow(ms) == 0) stop("No imputed series on this recording.", call. = FALSE)
  if (is.null(metric)) metric <- .raw.perday.metrics(ms)
  for (m in metric) {
    if (!m %in% names(ms)) stop("Metric '", m, "' is not in the imputed series.", call. = FALSE)
  }

  d <- x$wear$daily
  ws3 <- as.numeric(x$meta$windowsizes[1])
  crit <- as.numeric(x$params$includedaycrit %||% 16)[1]
  valid <- vapply(seq_len(nrow(d)), function(di) isTRUE(d$n_valid_hours[di] >= crit), logical(1))
  # the 1-6am mean needs more than 6 hours of the day as recorded, before the padding
  has16 <- (d$last_epoch - d$first_epoch + 1) > 6 * 60 * (60 / ws3)
  # averageday is unnamed, in metashort column order minus timestamp
  avg <- x$imputed$averageday
  avg_of <- function(m) {
    if (!(is.matrix(avg) || is.data.frame(avg))) return(NULL)
    cn <- colnames(avg)
    j <- if (!is.null(cn)) match(m, cn) else match(m, setdiff(names(ms), "timestamp"))
    if (is.na(j) || j > ncol(avg)) NULL else as.numeric(avg[, j])
  }
  avgs <- lapply(metric, avg_of)

  all_cols <- .raw.perday.colnames(metric, ws3, p)
  n_per <- length(all_cols) / max(1, length(metric))
  cols <- character(0)
  for (di in which(valid)) {
    today <- if (has16[di]) all_cols else all_cols[!grepl("_1-6am$", all_cols)]
    cols <- c(cols, setdiff(today, cols))
  }
  if (length(cols) == 0) return(data.frame(row.names = seq_len(nrow(d))))
  out <- as.data.frame(setNames(rep(list(rep(NA_real_, nrow(d))), length(cols)), cols),
                       check.names = FALSE)

  for (di in which(valid)) {                          # GGIR leaves other rows NA
    v <- rep(NA_real_, length(all_cols))
    for (k in seq_along(metric)) {
      varnum <- as.numeric(ms[[metric[k]]][d$first_epoch[di]:d$last_epoch[di]])
      if (!is.null(avgs[[k]])) varnum <- .raw.perday.pad(varnum, avgs[[k]], di, ws3)
      UnitReScale <- .raw.perday.rescale(metric[k])
      w <- rep(NA_real_, n_per)
      if (length(varnum) > (60 / ws3) * 60 * p$winhr * 1.2) {
        t1 <- length(varnum) / (60 * (60 / ws3)) + (p$winhr - (p$M5L5res / 60))
        mx <- .raw.getmx(varnum, ws3, 0, t1, p$M5L5res, p$winhr, UnitReScale)
        w[1:4] <- as.numeric(mx)
      }
      if (has16[di]) {
        w[5] <- mean(varnum[((1 * 60 * (60 / ws3)) + 1):(6 * 60 * (60 / ws3))]) * UnitReScale
      }
      w[6] <- mean(varnum) * UnitReScale
      if (length(p$mvpathreshold) > 0) {
        at <- 6
        for (t in p$mvpathreshold) {
          w[(at + 1):(at + 6)] <- .raw.perday.mvpa(varnum, ws3, t, p, UnitReScale)
          at <- at + 6
        }
      }
      v[(k - 1) * n_per + seq_len(n_per)] <- w
    }
    keep <- all_cols %in% cols
    out[di, all_cols[keep]] <- as.list(v[keep])
  }
  # GGIR's day table is a character matrix, turned back into numbers by g.part2
  out[] <- lapply(out, function(v) as.numeric(as.character(v)))
  out
}
