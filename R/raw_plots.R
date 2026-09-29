# Ported from GGIR 3.3-9 R/g.plot.R, R/visualReport.R, R/g.sib.plot.R and
# R/g.plot5.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. GGIR's data preparation (what is
# drawn) is kept; the drawing is ggplot2 on theme_canhrActi(). The calibration sphere,
# gap timeline and calibration trace have no GGIR original. Every function returns an
# empty-state ggplot for corrupt, too-short or skipped input.

# SHARED HELPERS

#' The canhrActi Theme, or theme_minimal If It Errors
#' @keywords internal
#' @noRd
.raw.plot.theme <- function() {
  tryCatch(theme_canhrActi(), error = function(e) ggplot2::theme_minimal())
}

#' Wrap a Subtitle or Caption So It Does Not Run Off the Page
#' @keywords internal
#' @noRd
.raw.plot.wrap <- function(x, width = 88) {
  if (is.null(x) || length(x) == 0) return(x)
  paste(strwrap(x, width = width), collapse = "\n")
}

#' Colours of the Raw Plots
#'
#' Status colours come from canhrActi_palette("status"); the fallbacks are the same hex
#' values.
#'
#' @return A named character vector of hex colours.
#' @keywords internal
#' @noRd
.raw.plot.colours <- function() {
  status <- c(valid = "#71984A", invalid = "#E8E8E8", warning = "#DF6A2E", highlight = "#FFCD00")
  st <- tryCatch(canhrActi_palette("status"), error = function(e) NULL)
  if (!is.null(st)) for (nm in names(status)) if (!is.null(st[[nm]])) status[[nm]] <- st[[nm]]
  c(not_worn = unname(status[["highlight"]]),      # r1: GGIR gold1
    also_not_worn = unname(status[["warning"]]),   # r3: GGIR goldenrod
    clipping = "#CC79A7",                          # r2: GGIR mediumpurple
    protocol = "#56B4E9",                          # r4: GGIR azure, hatched
    trace = "#111111", angle = "#236192", before = "#94A3B8", after = "#0072B2",
    raw_fill = "#56B4E9", epoch_fill = "#D55E00", block = "#CBD5E1",
    valid = unname(status[["valid"]]), invalid = unname(status[["invalid"]]),
    end = "#0072B2", reference = "#374151")
}

#' The Empty-State Plot
#'
#' A valid ggplot with one short sentence, returned for input that has nothing to draw. The
#' sentence is the label of the single text layer.
#'
#' @param message One sentence.
#' @param title Plot title.
#' @return A ggplot object.
#' @keywords internal
#' @noRd
.raw.plot.empty <- function(message, title = NULL) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, hjust = 0.5, vjust = 0.5, size = 4.5) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    .raw.plot.theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

#' Is a Plot the Empty State of This File
#'
#' @param p A ggplot object.
#' @return TRUE when \code{p} was built by the empty-state helper.
#' @keywords internal
#' @noRd
.raw.plot.is.empty <- function(p) {
  if (!inherits(p, "ggplot") || length(p$layers) != 1) return(FALSE)
  lab <- p$layers[[1]]$aes_params$label
  is.character(lab) && length(lab) == 1 && nzchar(lab)
}

#' The Sentence of an Empty-State Plot, or NULL
#' @keywords internal
#' @noRd
.raw.plot.empty.message <- function(p) {
  if (!.raw.plot.is.empty(p)) return(NULL)
  p$layers[[1]]$aes_params$label
}

#' Take the Pieces a Plot Needs Out of a canhrActi_raw or One of Its Parts
#'
#' Accepts a \code{canhrActi_raw}, a \code{canhrActi_raw_meta}, a \code{canhrActi_raw_wear},
#' a \code{canhrActi_raw_calibration}, a GGIR M list, or a plain list with any of the
#' members \code{meta}, \code{wear}, \code{calibration}. The state is "ok" when epoch tables
#' exist, else the sentence the empty-state plot should carry.
#'
#' @param x The object.
#' @param wear An optional \code{canhrActi_raw_wear} overriding the one in \code{x}.
#' @return list(meta, wear, cal, filename, brand, tz, state).
#' @keywords internal
#' @noRd
.raw.plot.parts <- function(x, wear = NULL) {
  meta <- NULL; cal <- NULL; w <- NULL; filename <- NA_character_; brand <- NA_character_
  tz <- ""; skipped <- FALSE; corrupt <- FALSE; too_short <- FALSE
  if (inherits(x, "canhrActi_raw")) {
    meta <- x$meta; cal <- x$calibration; w <- x$wear
    filename <- x$file$filename; brand <- x$device$brand
    tz <- if (!is.null(x$tz$effective_tz)) x$tz$effective_tz else ""
    skipped <- isTRUE(x$status$skipped); corrupt <- isTRUE(x$status$corrupt)
    too_short <- isTRUE(x$status$too_short)
  } else if (inherits(x, "canhrActi_raw_meta")) {
    meta <- x; cal <- x$calibration
    filename <- if (!is.null(x$file$filename)) x$file$filename else NA_character_
    skipped <- isTRUE(x$skipped); corrupt <- isTRUE(x$filecorrupt); too_short <- isTRUE(x$filetooshort)
  } else if (inherits(x, "canhrActi_raw_calibration")) {
    cal <- x
    filename <- if (!is.null(x$file$filename)) x$file$filename else NA_character_
  } else if (inherits(x, "canhrActi_raw_wear")) {
    w <- x
    filename <- if (!is.null(x$file$filename)) x$file$filename else NA_character_
  } else if (is.list(x) && all(c("metalong", "metashort") %in% names(x))) {
    # a GGIR M list
    meta <- x
    corrupt <- isTRUE(x$filecorrupt); too_short <- isTRUE(x$filetooshort)
  } else if (is.list(x)) {
    meta <- x$meta; cal <- x$calibration; w <- x$wear
    if (!is.null(x$file$filename)) filename <- x$file$filename
    if (!is.null(x$device$brand)) brand <- x$device$brand
    if (!is.null(x$tz$effective_tz)) tz <- x$tz$effective_tz
    skipped <- isTRUE(x$status$skipped) || isTRUE(meta$skipped)
    corrupt <- isTRUE(x$status$corrupt) || isTRUE(meta$filecorrupt)
    too_short <- isTRUE(x$status$too_short) || isTRUE(meta$filetooshort)
  } else {
    stop("x must be a canhrActi_raw object (or one of its parts: meta, wear, calibration)", call. = FALSE)
  }
  if (!is.null(wear)) w <- wear
  if (!is.null(meta) && (length(tz) == 0 || !nzchar(tz)) && !is.null(meta$settings$desiredtz)) {
    tz <- meta$settings$desiredtz
  }
  if (length(tz) == 0 || is.na(tz) || !nzchar(tz)) tz <- Sys.timezone()
  if (is.na(filename) && !is.null(cal$file$filename)) filename <- cal$file$filename
  if (is.na(filename) && !is.null(w$file$filename)) filename <- w$file$filename
  has_tables <- !is.null(meta) && is.data.frame(meta$metashort) && nrow(meta$metashort) > 0 &&
    is.data.frame(meta$metalong) && nrow(meta$metalong) > 0
  state <- if (skipped) {
    "The file was skipped at inspection (below the minimum file size); there are no epoch tables to draw."
  } else if (corrupt) {
    "The file is corrupt: no epoch tables were derived, so there is nothing to draw."
  } else if (too_short) {
    "The recording holds fewer than two hours of data (GGIR's floor): no epoch tables were derived."
  } else if (!has_tables) {
    "No epoch tables are available for this recording."
  } else "ok"
  list(meta = meta, wear = w, cal = cal, filename = filename, brand = brand, tz = tz, state = state)
}

#' Epoch Start Times as POSIXct
#'
#' Uses the numeric \code{time} column canhrActi adds to the tables; falls back to parsing
#' GGIR's ISO 8601 timestamp column for a GGIR M list.
#'
#' @param tbl metashort or metalong.
#' @param tz The zone to label the times with.
#' @return POSIXct vector.
#' @keywords internal
#' @noRd
.raw.plot.time <- function(tbl, tz) {
  if ("time" %in% names(tbl)) return(as.POSIXct(as.numeric(tbl$time), origin = "1970-01-01", tz = tz))
  t <- as.POSIXct(as.character(tbl$timestamp), format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  t
}

#' The Clock Part of GGIR's Timestamps
#'
#' GGIR finds midnights and noons by grepping "00:00:00" and "12:00:00" in the ISO 8601
#' strings; the clock digits are characters 12 to 19. This keeps the grid on the zone the
#' tables were built in, whatever the session zone.
#'
#' @param timestamp Character vector of GGIR timestamps.
#' @return "HH:MM:SS" strings.
#' @keywords internal
#' @noRd
.raw.plot.clock <- function(timestamp) substr(as.character(timestamp), 12, 19)

#' The First Acceleration Metric of metashort
#'
#' GGIR's rule: the first column that is not timestamp, anglex, angley or anglez; the
#' canhrActi time column is excluded as well.
#'
#' @param metashort The short-epoch table.
#' @param metric NULL for GGIR's rule, or a column name.
#' @return The column name.
#' @keywords internal
#' @noRd
.raw.plot.metric <- function(metashort, metric = NULL) {
  if (!is.null(metric)) {
    if (!metric %in% names(metashort)) stop("metric '", metric, "' is not a column of metashort", call. = FALSE)
    return(metric)
  }
  IndOtMetric <- which(colnames(metashort) %in% c("timestamp", "time", "anglex", "angley", "anglez") == FALSE)[1]
  if (is.na(IndOtMetric)) stop("metashort has no acceleration metric column", call. = FALSE)
  colnames(metashort)[IndOtMetric]
}

#' Is a Metric on the g Scale
#'
#' As in g.plot: ZCX, ZCY, ExtAct and any name containing "count" are not in g.
#' @keywords internal
#' @noRd
.raw.plot.metric.is.g <- function(metricName) {
  !(metricName %in% c("ZCX", "ZCY", "ZCX", "ExtAct") == TRUE |
      length(grep(pattern = "count", x = metricName, ignore.case = TRUE)) > 0)
}

#' Average a Short-Epoch Metric to the Long Epoch as g.plot Does
#'
#' Cumulative sum, a seq() of long-epoch starts, the difference of the rounded selection
#' divided by its spacing, then padded with zeros or cut to the number of long epochs.
#'
#' @param accel The short-epoch metric.
#' @param ws3,ws2 Short and long epoch lengths in seconds.
#' @param n_long Number of long epochs (the timeline length).
#' @return Numeric vector of length n_long.
#' @keywords internal
#' @noRd
.raw.plot.average.long <- function(accel, ws3, ws2, n_long) {
  accel <- as.numeric(accel)
  accel2 <- cumsum(c(0, accel))
  select <- seq(1, length(accel2), by = ws2 / ws3)
  Acceleration <- diff(accel2[round(select)]) / abs(diff(round(select[1:(length(select))])))
  if (n_long > length(Acceleration)) {
    Acceleration <- c(Acceleration, rep(0, abs(length(Acceleration) - n_long)))
  } else if (n_long < length(Acceleration)) {
    Acceleration <- Acceleration[1:n_long]
  }
  Acceleration
}

#' Runs of Ones in a 0/1 Vector as a Table of Epoch Ranges
#'
#' The same runs g.plot's createcoordinates finds, as inclusive first and last epoch
#' indices, which is what a rectangle from the start of the first epoch to the end of the
#' last needs.
#'
#' @param r 0/1 vector (one value per epoch).
#' @return data.frame(start_epoch, end_epoch), zero rows when there is no run.
#' @keywords internal
#' @noRd
.raw.plot.runs <- function(r) {
  r <- as.integer(as.numeric(r) == 1)
  if (length(r) == 0 || !any(r == 1)) return(data.frame(start_epoch = integer(0), end_epoch = integer(0)))
  rl <- rle(r)
  ends <- cumsum(rl$lengths)
  starts <- ends - rl$lengths + 1L
  keep <- rl$values == 1L
  data.frame(start_epoch = as.integer(starts[keep]), end_epoch = as.integer(ends[keep]))
}

#' GGIR's createcoordinates
#'
#' Kept for the test that checks the bands against GGIR's own coordinates: x0 is the first
#' epoch of each run of ones, x1 the first epoch after it, or the last epoch of the timeline
#' when the recording ends inside a run.
#'
#' @param r 0/1 vector.
#' @param timeline 1:length(r).
#' @return list(x0, x1).
#' @keywords internal
#' @noRd
.raw.plot.createcoordinates <- function(r, timeline) {
  if (length(which(abs(diff(r)) == 1) > 0)) {
    if (r[1] == 1) {
      x0 <- c(timeline[1], timeline[which(diff(r) == 1) + 1])
      x1 <- timeline[which(diff(r) == -1) + 1]
      if (r[length(timeline)] == 1) { #file ends with non-wear
        x1 <- c(x1, timeline[length(timeline)])
      }
    } else {
      x0 <- timeline[which(diff(r) == 1) + 1]
      x1 <- timeline[which(diff(r) == -1) + 1]
      if (r[length(timeline)] == 1) { #file ends with non-wear
        x1 <- c(x1, timeline[length(timeline)])
      }
    }
  } else {
    x0 <- c()
    x1 <- c()
  }
  invisible(list(x0 = x0, x1 = x1))
}

#' The Excluded-Time Bands of the Quality Overview
#'
#' One row per run of ones in each of r1 (not worn), r3 (also not worn), r2 (clipping) and
#' r4 (protocol), with the inclusive epoch range and the wall-clock start and end.
#'
#' @param rout The rout table of raw.wear.decision (columns r1..r5).
#' @param time POSIXct start of every long epoch.
#' @param ws2 Long epoch length in seconds.
#' @return data.frame(category, start_epoch, end_epoch, xmin, xmax).
#' @keywords internal
#' @noRd
.raw.plot.bands <- function(rout, time, ws2) {
  cats <- c(r1 = "not worn", r3 = "also not worn", r2 = "signal clipping", r4 = "study protocol masked")
  out <- lapply(names(cats), function(col) {
    if (!col %in% names(rout)) return(NULL)
    runs <- .raw.plot.runs(rout[[col]])
    if (nrow(runs) == 0) return(NULL)
    data.frame(category = cats[[col]], start_epoch = runs$start_epoch, end_epoch = runs$end_epoch,
               xmin = time[runs$start_epoch], xmax = time[runs$end_epoch] + ws2,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(category = character(0), start_epoch = integer(0), end_epoch = integer(0),
                      xmin = as.POSIXct(character(0), tz = attr(time, "tzone")),
                      xmax = as.POSIXct(character(0), tz = attr(time, "tzone")), stringsAsFactors = FALSE)
  }
  out$category <- factor(out$category, levels = unname(cats))
  out
}

#' Day Numbers for the x Axis of the Quality Overview
#'
#' GGIR labels every noon with a day number, day 1 being the calendar day the recording
#' starts on. Here every noon is labelled "Day k" with its date underneath.
#'
#' @param time POSIXct of the long epochs.
#' @param clock The "HH:MM:SS" strings of the long epochs.
#' @return list(midnights (POSIXct), noons (POSIXct), labels (character, one per noon)).
#' @keywords internal
#' @noRd
.raw.plot.daygrid <- function(time, clock) {
  tz <- attr(time, "tzone"); if (is.null(tz)) tz <- ""
  mnights <- grep("00:00:00", clock)
  noons <- grep("12:00:00", clock)
  start_date <- as.Date(format(time[1], "%Y-%m-%d"))
  noon_dates <- as.Date(format(time[noons], "%Y-%m-%d"))
  daynum <- as.integer(noon_dates - start_date) + 1L
  list(midnights = time[mnights], noons = time[noons],
       labels = paste0("Day ", daynum, "\n", .format_english(time[noons], "%a %d %b")))
}

# QUALITY OVERVIEW

#' Quality Overview of a Raw Recording (GGIR's Part-2 QC Plot)
#'
#' The same information as GGIR's results/QC/plots_part2/plot_<file>.png: full-height bands
#' for the time the part-2 wear decision excludes (not worn, also not worn, signal clipping,
#' study protocol mask), the first acceleration metric averaged to the long epoch as a trace,
#' the non-wear score (0 to 3 axes still) as a step line, a temperature panel with the 20 and
#' 35 C references when the tables carry one, a dotted line at every midnight and a dashed
#' one at every noon, day numbers under the noons, and a line at the last epoch.
#'
#' @details
#' Kept from g.plot: the band coordinates, the metric choice, the averaging to the long
#' epoch, the midnight and noon grid, the day numbering, the 0 to 0.6 g range for g-scale
#' metrics, the 0 to 3 score axis and the temperature references. Changed: the x axis is
#' wall-clock time in the zone the tables carry, the panels are facets of one ggplot, and
#' the colours follow canhrActi. The wear decision comes from \code{x$wear} or is computed
#' with \code{\link{raw.wear.decision}} when \code{wear} is NULL.
#'
#' @param x A \code{canhrActi_raw} object, a \code{canhrActi_raw_meta}, or a GGIR M list.
#' @param wear NULL, or a \code{canhrActi_raw_wear} to use instead of \code{x$wear}.
#' @param metric NULL for GGIR's choice (ENMO by default), or a metashort column name.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot (one sentence) for corrupt, too-short or
#'   skipped input.
#' @examples
#' \dontrun{
#' x <- read.raw.accelerometer("MOS2E39230594.gt3x", desiredtz = "America/Anchorage")
#' plot_raw_quality(x)
#' }
#' @seealso \code{\link{plot_raw_days}}, \code{\link{plot_raw_wear_days}}
#' @export plot_raw_quality
plot_raw_quality <- function(x, wear = NULL, metric = NULL, ...) {
  p <- .raw.plot.parts(x, wear)
  title <- "Data quality overview"
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  M <- p$meta
  ws3 <- M$windowsizes[1]
  ws2 <- M$windowsizes[2]
  w <- p$wear
  if (is.null(w) || !identical(w$status, "ok") || is.null(w$rout)) {
    w <- tryCatch(raw.wear.decision(M), error = function(e) NULL)
  }
  if (is.null(w) || !identical(w$status, "ok") || is.null(w$rout)) {
    return(.raw.plot.empty("The wear decision could not be made for this recording, so the overview cannot be drawn.", title))
  }
  cols <- .raw.plot.colours()
  metalong <- M$metalong
  metashort <- M$metashort
  timeline <- 1:nrow(metalong)
  time_long <- .raw.plot.time(metalong, p$tz)
  clock <- .raw.plot.clock(metalong$timestamp)
  metricName <- .raw.plot.metric(metashort, metric)
  Acceleration <- .raw.plot.average.long(metashort[[metricName]], ws3, ws2, length(timeline))
  is_g <- .raw.plot.metric.is.g(metricName)

  acc_label <- if (is_g) paste0(metricName, " (g), ", ws2 / 60, "-min mean") else
    paste0(metricName, ifelse(metricName == "ExtAct", yes = "", no = " (counts)"), ", ", ws2 / 60, "-min mean")
  score_label <- "Non-wear score (axes still)"
  temp_label <- "Temperature (C)"
  has_temp <- "temperaturemean" %in% names(metalong) && any(!is.na(metalong$temperaturemean))
  panels <- c(if (has_temp) temp_label, score_label, acc_label)

  series <- rbind(
    data.frame(panel = acc_label, time = time_long, y = Acceleration, stringsAsFactors = FALSE),
    data.frame(panel = score_label, time = time_long, y = as.numeric(metalong$nonwearscore), stringsAsFactors = FALSE),
    if (has_temp) data.frame(panel = temp_label, time = time_long,
                             y = as.numeric(metalong$temperaturemean), stringsAsFactors = FALSE))
  series$panel <- factor(series$panel, levels = panels)
  bands <- .raw.plot.bands(w$rout, time_long, ws2)
  grid <- .raw.plot.daygrid(time_long, clock)
  end_time <- time_long[length(time_long)] + ws2

  # y ranges: GGIR's 0..0.6 g for g metrics, 0..3 score, 20..35 C temperature
  limits <- rbind(
    data.frame(panel = acc_label, y = if (is_g) c(0, 0.6) else c(0, max(Acceleration, na.rm = TRUE) * 1.05)),
    data.frame(panel = score_label, y = c(0, 3)),
    if (has_temp) data.frame(panel = temp_label, y = c(20, 35)))
  limits$panel <- factor(limits$panel, levels = panels)
  refs <- if (has_temp) data.frame(panel = factor(temp_label, levels = panels), y = c(20, 35)) else NULL

  fills <- c("not worn" = cols[["not_worn"]], "also not worn" = cols[["also_not_worn"]],
             "signal clipping" = cols[["clipping"]], "study protocol masked" = cols[["protocol"]])
  present <- levels(droplevels(bands$category))

  worn <- w$wear_dur_def_proto_day
  rec <- w$meas_dur_dys
  subtitle <- paste0(if (!is.na(p$brand)) paste0(p$brand, ", ") else "",
                     round(rec, 2), " days recorded, ", round(worn, 2), " days worn after GGIR's part-2 decision; ",
                     w$n_valid_days, " valid day", if (w$n_valid_days == 1) "" else "s",
                     " (", w$settings$includedaycrit, " h or more)")

  g <- ggplot2::ggplot() +
    ggplot2::geom_blank(data = limits, ggplot2::aes(y = .data$y))
  if (nrow(bands) > 0) {
    g <- g + ggplot2::geom_rect(data = bands,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf,
                                             fill = .data$category),
                                alpha = 0.45, colour = NA)
  }
  g <- g +
    ggplot2::geom_vline(xintercept = grid$noons, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = grid$midnights, colour = "grey20", linetype = "dotted", linewidth = 0.4)
  if (!is.null(refs)) {
    g <- g + ggplot2::geom_hline(data = refs, ggplot2::aes(yintercept = .data$y),
                                 colour = cols[["reference"]], linetype = "dashed", linewidth = 0.4)
  }
  g <- g +
    ggplot2::geom_line(data = series[series$panel == acc_label, ],
                       ggplot2::aes(x = .data$time, y = .data$y), colour = cols[["trace"]], linewidth = 0.3) +
    ggplot2::geom_step(data = series[series$panel == score_label, ],
                       ggplot2::aes(x = .data$time, y = .data$y), colour = cols[["trace"]], linewidth = 0.4)
  if (has_temp) {
    g <- g + ggplot2::geom_line(data = series[series$panel == temp_label, ],
                                ggplot2::aes(x = .data$time, y = .data$y), colour = cols[["trace"]], linewidth = 0.3)
  }
  g <- g +
    ggplot2::geom_vline(xintercept = end_time, colour = cols[["end"]], linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = fills, breaks = present, name = "Excluded by GGIR", drop = TRUE) +
    ggplot2::scale_x_datetime(breaks = grid$noons, labels = grid$labels, timezone = p$tz,
                              expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = NULL, y = NULL,
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Dotted lines: midnight; dashed: noon; blue: end of the last epoch. Times in ", p$tz, "."))) +
    .raw.plot.theme() +
    ggplot2::theme(legend.position = "top", strip.placement = "outside",
                   strip.background = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0.6, "lines"))
  g
}

# PER-DAY STRIP

#' The 36-Hour Windows of visualReport
#'
#' On the short-epoch clock strings: the day edges are the epochs at dayborder:00:00; when
#' the recording starts at an edge the windows start there, otherwise the first window
#' starts at the first epoch; every window ends
#' \code{(hrsPerRow - 24) * 3600 / epochSize - 1} epochs after the next edge, the last one at
#' the last epoch.
#'
#' @param clock "HH:MM:SS" strings of the short epochs.
#' @param epochSize Short epoch length in seconds.
#' @param hrsPerRow Window length in hours.
#' @param dayborder Hour of the day edge.
#' @return data.frame(window, first_epoch, last_epoch).
#' @keywords internal
#' @noRd
.raw.plot.windows <- function(clock, epochSize, hrsPerRow = 36, dayborder = 0) {
  n <- length(clock)
  edge <- sprintf("%02d:00:00", as.integer(dayborder))
  dayedges <- which(clock == edge)
  if (length(dayedges) == 0) {
    subploti <- cbind(1, n)
  } else if (dayedges[1] == 1) {
    # recording starts at edge
    subploti <- dayedges
    dayEnds <- c(dayedges[2:length(dayedges)] + ((hrsPerRow - 24) * (3600/epochSize)) - 1, n)
    if (length(dayedges) == 1) dayEnds <- n
    subploti <- cbind(subploti, dayEnds)
  } else {
    # recording does not start at edge
    subploti <- c(1, dayedges)
    dayEnds <- c(dayedges + ((hrsPerRow - 24) * (3600/epochSize)) - 1, n)
    subploti <- cbind(subploti, dayEnds)
  }
  subploti[which(subploti[,2] > n), 2] <- n
  data.frame(window = seq_len(nrow(subploti)), first_epoch = as.integer(subploti[, 1]),
             last_epoch = as.integer(subploti[, 2]))
}

#' Per-Day Strip of a Raw Recording With the Epochs GGIR Ignores Boxed Out
#'
#' The data-quality layer of GGIR's visual report, one row per window of \code{hours_per_row}
#' hours starting at the day border (so consecutive rows overlap by twelve hours, as in
#' GGIR): the acceleration metric in the upper half of the row, the z angle in the lower
#' half, and every run of invalid epochs (the part-2 decision, r5long) as a washed-out box
#' with a grey border drawn over both.
#'
#' @details
#' The window construction and the invalid layer are visualReport's. The vertical mapping
#' follows its panelplot: 0 to 500 mg fill the upper half (larger values capped), the angle
#' from -90 to 90 degrees fills the lower band. Windows come from
#' \code{x$wear$settings$dayborder} (0 by default). No other GGIR layer is drawn.
#'
#' @param x A \code{canhrActi_raw} object, a \code{canhrActi_raw_meta}, or a GGIR M list.
#' @param wear NULL, or a \code{canhrActi_raw_wear} to use instead of \code{x$wear}.
#' @param metric NULL for GGIR's choice (ENMO by default), or a metashort column name.
#' @param hours_per_row Window length in hours (GGIR's visualreport_hrsPerRow, 36).
#' @param dayborder NULL to take the wear decision's day border (0), or an hour 0 to 23.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for corrupt, too-short or skipped input.
#' @examples
#' \dontrun{
#' plot_raw_days(x)
#' }
#' @seealso \code{\link{plot_raw_quality}}, \code{\link{plot_raw_nights}}
#' @export plot_raw_days
plot_raw_days <- function(x, wear = NULL, metric = NULL, hours_per_row = 36, dayborder = NULL, ...) {
  p <- .raw.plot.parts(x, wear)
  title <- "Recording by day, with ignored epochs boxed"
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  M <- p$meta
  ws3 <- M$windowsizes[1]
  ws2 <- M$windowsizes[2]
  w <- p$wear
  if (is.null(w) || !identical(w$status, "ok") || is.null(w$r5long)) {
    w <- tryCatch(raw.wear.decision(M), error = function(e) NULL)
  }
  if (is.null(dayborder)) dayborder <- if (!is.null(w$settings$dayborder)) w$settings$dayborder else 0
  cols <- .raw.plot.colours()
  metashort <- M$metashort
  n <- nrow(metashort)
  time_short <- .raw.plot.time(metashort, p$tz)
  clock <- .raw.plot.clock(metashort$timestamp)
  metricName <- .raw.plot.metric(metashort, metric)
  ACC <- as.numeric(metashort[[metricName]]) * 1000
  angle <- if ("anglez" %in% names(metashort)) as.numeric(metashort$anglez) else rep(NA_real_, n)
  invalid <- if (!is.null(w) && !is.null(w$r5long)) as.numeric(w$r5long) else rep(0, n)
  invalid[is.na(invalid)] <- 0
  invalid <- as.integer(invalid == 1)

  win <- .raw.plot.windows(clock, ws3, hours_per_row, dayborder)
  rows <- lapply(seq_len(nrow(win)), function(i) {
    a <- win$first_epoch[i]; b <- win$last_epoch[i]
    idx <- a:b
    t0 <- time_short[a]
    # x is hours since the window's day edge, so clock hours line up across rows
    edge_hours <- (as.numeric(format(t0, "%H")) + as.numeric(format(t0, "%M")) / 60 +
                     as.numeric(format(t0, "%S")) / 3600 - dayborder) %% 24
    x0 <- as.numeric(t0) - edge_hours * 3600
    h <- (as.numeric(time_short[idx]) - x0) / 3600
    label <- paste0("Window ", i, ": from ", .format_english(t0, "%a %d %b %H:%M"))
    inv <- .raw.plot.runs(invalid[idx])
    # bands 0..36 and 54..100 rather than visualReport's 0..40 and 50..100, so the two
    # inner axis labels do not collide at eight windows
    list(series = data.frame(window = label, h = h,
                             acc = pmin(ACC[idx] * 46 / 500 + 54, 100),
                             ang = (angle[idx] + 90) * 36 / 180, stringsAsFactors = FALSE),
         boxes = if (nrow(inv) > 0) data.frame(window = label,
                                               xmin = h[inv$start_epoch], xmax = h[inv$end_epoch] + ws3 / 3600,
                                               start_epoch = idx[inv$start_epoch], end_epoch = idx[inv$end_epoch],
                                               stringsAsFactors = FALSE) else NULL,
         label = label)
  })
  series <- do.call(rbind, lapply(rows, `[[`, "series"))
  boxes <- do.call(rbind, lapply(rows, `[[`, "boxes"))
  labels <- vapply(rows, `[[`, character(1), "label")
  series$window <- factor(series$window, levels = labels)
  if (!is.null(boxes)) boxes$window <- factor(boxes$window, levels = labels)

  hour_breaks <- seq(0, hours_per_row, by = 3)
  hour_labels <- sprintf("%02d", (hour_breaks + dayborder) %% 24)
  n_inv <- sum(invalid)
  subtitle <- paste0(nrow(win), " window", if (nrow(win) == 1) "" else "s", " of ", hours_per_row,
                     " h from ", sprintf("%02d:00", dayborder), "; ",
                     round(n_inv * ws3 / 3600, 1), " of ", round(n * ws3 / 3600, 1),
                     " h are ignored by GGIR's part-2 decision (boxed)")

  g <- ggplot2::ggplot(series, ggplot2::aes(x = .data$h)) +
    ggplot2::geom_hline(yintercept = 45, colour = "grey75", linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 18, colour = "grey75", linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = 24, colour = "grey40", linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = .data$acc), colour = cols[["trace"]], linewidth = 0.2, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$ang), colour = cols[["angle"]], linewidth = 0.2, na.rm = TRUE)
  if (!is.null(boxes) && nrow(boxes) > 0) {
    g <- g + ggplot2::geom_rect(data = boxes,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 5, ymax = 95),
                                inherit.aes = FALSE, fill = "white", alpha = 0.8, colour = "grey50", linewidth = 0.3)
  }
  g <- g +
    ggplot2::scale_x_continuous(breaks = hour_breaks, labels = hour_labels, limits = c(0, hours_per_row),
                                expand = ggplot2::expansion(mult = c(0.005, 0.005))) +
    # angle labelled on the left, metric on the right; past four windows each band
    # keeps only its two end labels
    ggplot2::scale_y_continuous(
      breaks = if (nrow(win) > 4) c(0, 36) else c(0, 18, 36),
      labels = if (nrow(win) > 4) c("-90", "+90 deg") else c("-90", "0", "+90 deg"),
      limits = c(0, 100),
      sec.axis = ggplot2::sec_axis(
        ~ .,
        breaks = if (nrow(win) > 4) c(54, 100) else c(54, 77, 100),
        labels = if (nrow(win) > 4) c("0", "500 mg") else c("0", "250", "500 mg"))) +
    ggplot2::facet_wrap(~window, ncol = 1) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = "Clock hour", y = NULL,
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Black: ", metricName, " (mg, upper half); blue: z angle (lower half); boxes: epochs ignored. Times in ", p$tz, "."))) +
    .raw.plot.theme() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0.4, "lines"),
                   strip.text = ggplot2::element_text(hjust = 0),
                   strip.background = ggplot2::element_blank())
  g
}

# NOON-TO-NOON NIGHTS

#' Night-by-Night z Angle With the Acceleration Trace Beneath
#'
#' The data view of GGIR's part-3 plot: one row per noon-to-noon window, the z angle as a
#' black line between -90 and +90 degrees and the acceleration metric as a grey trace in a
#' band beneath it, with a dotted line every hour. A perfectly flat angle line is the
#' signature of an imputed gap or a device lying on a table, not of sleep. Epochs the part-2
#' decision ignores are shaded lightly in the background.
#'
#' @details
#' The data elements of g.sib.plot. Changed: the acceleration band uses one fixed scale for
#' every night (0 to 500 mg, capped) instead of GGIR's per-night rescaling, so nights are
#' comparable; the sustained-inactivity bouts, light and temperature lines are not drawn.
#' The first window runs from the start of the recording to the first noon and the last
#' from the last noon to the end, as GGIR numbers its nights.
#'
#' @param x A \code{canhrActi_raw} object, a \code{canhrActi_raw_meta}, or a GGIR M list.
#' @param wear NULL, or a \code{canhrActi_raw_wear} to use instead of \code{x$wear}.
#' @param metric NULL for GGIR's choice (ENMO by default), or a metashort column name.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for corrupt, too-short or skipped input.
#' @examples
#' \dontrun{
#' plot_raw_nights(x)
#' }
#' @seealso \code{\link{plot_raw_days}}
#' @export plot_raw_nights
plot_raw_nights <- function(x, wear = NULL, metric = NULL, ...) {
  p <- .raw.plot.parts(x, wear)
  title <- "Nights, noon to noon: z angle over the acceleration trace"
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  M <- p$meta
  ws3 <- M$windowsizes[1]
  metashort <- M$metashort
  if (!"anglez" %in% names(metashort)) {
    return(.raw.plot.empty("The epoch tables carry no z angle (anglez), so the night view cannot be drawn.", title))
  }
  w <- p$wear
  if (is.null(w) || !identical(w$status, "ok") || is.null(w$r5long)) {
    w <- tryCatch(raw.wear.decision(M), error = function(e) NULL)
  }
  cols <- .raw.plot.colours()
  n <- nrow(metashort)
  time_short <- .raw.plot.time(metashort, p$tz)
  clock <- .raw.plot.clock(metashort$timestamp)
  metricName <- .raw.plot.metric(metashort, metric)
  ENMO <- as.numeric(metashort[[metricName]]) * 1000
  angle <- as.numeric(metashort$anglez)
  if (length(which(is.na(angle) == TRUE)) > 0) {
    if (which(is.na(angle) == TRUE)[1] == length(angle)) {
      angle[length(angle)] <- angle[length(angle) - 1]
    }
  }
  invalid <- if (!is.null(w) && !is.null(w$r5long)) as.integer(as.numeric(w$r5long) == 1) else rep(0L, n)
  invalid[is.na(invalid)] <- 0L

  noons <- which(clock == "12:00:00")
  starts <- unique(c(1L, noons))
  ends <- c(if (length(noons) > 0) noons - 1L, n)
  ends <- ends[ends >= 1]
  if (length(noons) > 0 && noons[1] == 1) ends <- ends[-1]
  ends <- ends[seq_along(starts)]
  ends[length(ends)] <- n
  band_lo <- -260; band_hi <- -150
  rows <- lapply(seq_along(starts), function(i) {
    idx <- starts[i]:ends[i]
    t0 <- time_short[starts[i]]
    # hours since the noon that opens this window (negative offset for a start before it)
    h0 <- as.numeric(format(t0, "%H")) + as.numeric(format(t0, "%M")) / 60 + as.numeric(format(t0, "%S")) / 3600
    since_noon <- (h0 - 12) %% 24
    x0 <- as.numeric(t0) - since_noon * 3600
    h <- (as.numeric(time_short[idx]) - x0) / 3600
    label <- paste0("Night ", i, ": ", .format_english(as.POSIXct(x0, origin = "1970-01-01", tz = p$tz), "%a %d %b"),
                    " 12:00 to next noon")
    inv <- .raw.plot.runs(invalid[idx])
    list(series = data.frame(night = label, h = h, angle = angle[idx],
                             acc = band_lo + pmin(ENMO[idx], 500) / 500 * (band_hi - band_lo),
                             stringsAsFactors = FALSE),
         boxes = if (nrow(inv) > 0) data.frame(night = label, xmin = h[inv$start_epoch],
                                               xmax = h[inv$end_epoch] + ws3 / 3600,
                                               stringsAsFactors = FALSE) else NULL,
         label = label)
  })
  series <- do.call(rbind, lapply(rows, `[[`, "series"))
  boxes <- do.call(rbind, lapply(rows, `[[`, "boxes"))
  labels <- vapply(rows, `[[`, character(1), "label")
  series$night <- factor(series$night, levels = labels)
  if (!is.null(boxes)) boxes$night <- factor(boxes$night, levels = labels)

  subtitle <- paste0(length(labels), " window", if (length(labels) == 1) "" else "s",
                     "; a flat angle line is an imputed gap or a device at rest, not sleep")
  g <- ggplot2::ggplot(series, ggplot2::aes(x = .data$h))
  if (!is.null(boxes) && nrow(boxes) > 0) {
    g <- g + ggplot2::geom_rect(data = boxes,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
                                inherit.aes = FALSE, fill = cols[["invalid"]], alpha = 0.6, colour = NA)
  }
  g <- g +
    ggplot2::geom_vline(xintercept = 0:24, colour = "grey85", linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = band_lo, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = .data$acc), colour = "grey55", linewidth = 0.2, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$angle), colour = cols[["trace"]], linewidth = 0.25, na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = seq(0, 24, by = 2), labels = sprintf("%02d", (seq(0, 24, by = 2) + 12) %% 24),
                                limits = c(0, 24), expand = ggplot2::expansion(mult = c(0.005, 0.005))) +
    # as in plot_raw_days: angle on the left, metric on the right, middle label dropped past four nights
    ggplot2::scale_y_continuous(
      breaks = if (length(labels) > 4) c(-90, 90) else c(-90, 0, 90),
      labels = if (length(labels) > 4) c("-90", "+90 deg") else c("-90", "0", "+90 deg"),
      limits = c(band_lo, 95),
      sec.axis = ggplot2::sec_axis(~ ., breaks = c(band_lo, band_hi),
                                   labels = c("0", "500 mg"))) +
    ggplot2::facet_wrap(~night, ncol = 1) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = "Clock hour", y = NULL,
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Black: z angle; grey: ", metricName, " (mg, capped at 500); shaded: epochs ignored by the part-2 decision. Times in ", p$tz, "."))) +
    .raw.plot.theme() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0.4, "lines"),
                   strip.text = ggplot2::element_text(hjust = 0),
                   strip.background = ggplot2::element_blank())
  g
}

# CALIBRATION SPHERE

#' Corrected Still-Window Means as g.calibrate Computes Them
#'
#' \code{scale(features, center = -offset, scale = 1/scale)} plus, with temperature,
#' \code{scale(temperature, center = meantemp, scale = 1/tempoffset)}.
#'
#' @param spheredata GGIR's spheredata (Euclidean Norm, meanx, meany, meanz, sdx, sdy, sdz
#'   and optionally temperature).
#' @param scale,offset,tempoffset The coefficients.
#' @param use_temp Whether the temperature term is applied.
#' @param meantempcal The mean temperature of the fit.
#' @return A numeric matrix with three columns.
#' @keywords internal
#' @noRd
.raw.plot.sphere.after <- function(spheredata, scale, offset, tempoffset, use_temp = FALSE, meantempcal = NULL) {
  features_temp <- spheredata
  if (use_temp == FALSE || !("temperature" %in% names(features_temp)) || is.null(meantempcal) || is.null(tempoffset)) {
    features_temp2 <- base::scale(as.matrix(features_temp[,2:4]), center = -offset, scale = 1/scale)
  } else {
    yy <- as.matrix(cbind(as.numeric(features_temp[,8]), as.numeric(features_temp[,8]), as.numeric(features_temp[,8])))
    meantemp <- meantempcal
    features_temp2 <- base::scale(as.matrix(features_temp[,2:4]), center = -offset, scale = 1/scale) +
      base::scale(yy, center = rep(meantemp, 3), scale = 1/tempoffset)
  }
  m <- matrix(as.numeric(features_temp2), ncol = 3)
  colnames(m) <- c("x", "y", "z")
  m
}

#' Calibration Sphere: Still-Window Means Before and After Correction
#'
#' Three scatters, one per axis pair, of the still-window means g.calibrate collected
#' (the spheredata of the calibration), before the correction in grey and after it in blue,
#' with the unit circle and the +/- spherecrit lines that every axis has to cross for a fit
#' to be attempted. The subtitle gives the calibration error before and after, the number of
#' still windows and whether the coefficients were applied.
#'
#' @details
#' There is no GGIR plot of this. The corrected cloud is recomputed with g.calibrate's own
#' expression from the coefficients as the calibration object holds them after g.part1's
#' decision rule; when that rule reset the coefficients to the identity the two clouds
#' coincide and the subtitle says so. The sphere criterion is \code{settings$spherecrit}
#' (0.3 g by default).
#'
#' @param x A \code{canhrActi_raw} object or a \code{canhrActi_raw_calibration}.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot when no still windows exist (corrupt,
#'   too-short or skipped input, no non-movement data, or a supplied calibration without
#'   spheredata).
#' @examples
#' \dontrun{
#' plot_raw_calibration(x)
#' }
#' @seealso \code{\link{plot_raw_chunks}}, \code{\link{raw.calibrate}}
#' @export plot_raw_calibration
plot_raw_calibration <- function(x, ...) {
  p <- .raw.plot.parts(x)
  title <- "Auto-calibration: still-window means on the unit sphere"
  cal <- p$cal
  if (is.null(cal)) return(.raw.plot.empty(if (identical(p$state, "ok")) "No calibration object is attached to this recording." else p$state, title))
  sd <- cal$spheredata
  if (is.null(sd) || !is.data.frame(sd) || nrow(sd) == 0 || !all(c("meanx", "meany", "meanz") %in% names(sd))) {
    msg <- if (!identical(p$state, "ok") && !inherits(x, "canhrActi_raw_calibration")) p$state else
      if (identical(cal$source, "supplied")) "The calibration was supplied without its still windows, so the sphere cannot be drawn." else
        if (isTRUE(cal$attempted)) paste0("No still windows were found for the calibration (", cal$qcmessage, ").") else
          "No auto-calibration was attempted for this recording."
    return(.raw.plot.empty(msg, title))
  }
  cols <- .raw.plot.colours()
  spherecrit <- if (!is.null(cal$settings$spherecrit)) cal$settings$spherecrit else 0.3
  before <- as.matrix(sd[, c("meanx", "meany", "meanz")])
  colnames(before) <- c("x", "y", "z")
  after <- .raw.plot.sphere.after(sd, cal$scale, cal$offset, cal$tempoffset,
                                  use_temp = isTRUE(cal$use_temp), meantempcal = cal$meantempcal)
  pairs <- list(c("x", "y"), c("x", "z"), c("y", "z"))
  pts <- do.call(rbind, lapply(pairs, function(pr) {
    lab <- paste0(pr[1], " against ", pr[2])
    rbind(data.frame(pair = lab, stage = "before", a = before[, pr[1]], b = before[, pr[2]], stringsAsFactors = FALSE),
          data.frame(pair = lab, stage = "after", a = after[, pr[1]], b = after[, pr[2]], stringsAsFactors = FALSE))
  }))
  pts$pair <- factor(pts$pair, levels = vapply(pairs, function(pr) paste0(pr[1], " against ", pr[2]), character(1)))
  pts$stage <- factor(pts$stage, levels = c("before", "after"))
  th <- seq(0, 2 * pi, length.out = 181)
  circle <- data.frame(a = cos(th), b = sin(th))
  identical_clouds <- isTRUE(all.equal(unname(before), unname(after)))
  err0 <- cal$cal_error_start; err1 <- cal$cal_error_end
  fmt_mg <- function(v) if (is.null(v) || length(v) == 0 || is.na(v)) "not computed" else paste0(round(v * 1000, 1), " mg")
  subtitle <- paste0(nrow(sd), " still windows over ", cal$nhoursused, " h; error before ", fmt_mg(err0),
                     ", after ", fmt_mg(err1), "; coefficients ",
                     if (isTRUE(cal$applied)) "applied" else if (identical_clouds) "not applied (identity)" else "not applied")
  g <- ggplot2::ggplot() +
    ggplot2::geom_path(data = circle, ggplot2::aes(x = .data$a, y = .data$b), colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = c(-spherecrit, spherecrit), colour = cols[["reference"]], linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = c(-spherecrit, spherecrit), colour = cols[["reference"]], linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = .data$a, y = .data$b, colour = .data$stage, shape = .data$stage),
                        size = 1.1, alpha = 0.7) +
    ggplot2::scale_colour_manual(values = c(before = cols[["before"]], after = cols[["after"]]), name = "Correction") +
    ggplot2::scale_shape_manual(values = c(before = 16, after = 1), name = "Correction") +
    ggplot2::scale_x_continuous(breaks = c(-1, 0, 1)) +
    ggplot2::scale_y_continuous(breaks = c(-1, 0, 1)) +
    ggplot2::coord_fixed(xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25)) +
    ggplot2::facet_wrap(~pair, nrow = 1) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = "g", y = "g",
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Circle: radius 1 g; dashed: +/- ", spherecrit, " g, the reach every axis needs for a fit. GGIR: ",
                                   cal$qcmessage, "."))) +
    .raw.plot.theme() +
    ggplot2::theme(legend.position = "top")
  g
}

# GAP TIMELINE

#' Runs of Identical Consecutive Short Epochs: the Footprint of GGIR's Gap Filling
#'
#' A gap filled by last value carried forward (g.imputeTimegaps) or at epoch level
#' (g.getmeta's impute_at_epoch_level) leaves consecutive short epochs with identical metric
#' values. Runs of at least two such epochs are returned with their length; runs longer than
#' the raw-fill limit (max(6 long epochs, 90 min)) are the epoch-level fills that also score
#' non-wear 3.
#'
#' @param metashort The short-epoch table.
#' @param ws3,ws2 Short and long epoch lengths in seconds.
#' @param min_run Minimum run length in epochs (2).
#' @return data.frame(start_epoch, end_epoch, length, level) with level "raw" or "epoch".
#' @keywords internal
#' @noRd
.raw.plot.fill.runs <- function(metashort, ws3, ws2, min_run = 2) {
  limit <- max(c(((ws2 / 60) * 6), 90)) * 60 / ws3 # g.imputeTimegaps' raw-fill limit, in short epochs
  cols <- setdiff(names(metashort), c("timestamp", "time"))
  empty <- data.frame(start_epoch = integer(0), end_epoch = integer(0), length = integer(0),
                      level = character(0), stringsAsFactors = FALSE)
  if (length(cols) == 0 || nrow(metashort) == 0) return(empty)
  key <- do.call(paste, c(lapply(metashort[cols], as.character), sep = "\r"))
  r <- rle(key)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  keep <- which(r$lengths >= min_run)
  if (length(keep) == 0) return(empty)
  data.frame(start_epoch = as.integer(starts[keep]), end_epoch = as.integer(ends[keep]),
             length = as.integer(r$lengths[keep]),
             level = ifelse(r$lengths[keep] > limit, "epoch", "raw"), stringsAsFactors = FALSE)
}

#' Gap Timeline: Where the Recording Had No Samples and How GGIR Filled Them
#'
#' Rows on one time axis: the blocks the reader loaded (from the qclog, each labelled with
#' the number of gaps and the minutes filled), the stretches filled at raw level by last
#' value carried forward, the stretches filled at epoch level because the gap exceeded the
#' raw-fill limit (which also score non-wear 3), and, when a sample-level missingness table
#' is supplied, the individual gaps the reader reported.
#'
#' @details
#' There is no GGIR plot of this. The per-block figures are GGIR's QClog. The positions of
#' the fills are not stored by GGIR; they are recovered from the epoch tables as runs of at
#' least two consecutive short epochs with identical metric values. Gaps shorter than one
#' short epoch are counted in the block figures but have no visible footprint. A
#' \code{missingness} table (read.gt3x's attribute: \code{time}, \code{n_missing}) is drawn
#' when given or when the meta object carries one as \code{meta$missingness}.
#'
#' @param x A \code{canhrActi_raw} object, a \code{canhrActi_raw_meta}, or a GGIR M list.
#' @param missingness NULL, or a data.frame with columns \code{time} (POSIXct or numeric
#'   seconds) and \code{n_missing} (samples), as read.gt3x reports them.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for corrupt, too-short or skipped input.
#' @examples
#' \dontrun{
#' plot_raw_gaps(x)
#' }
#' @seealso \code{\link{plot_raw_quality}}
#' @export plot_raw_gaps
plot_raw_gaps <- function(x, missingness = NULL, ...) {
  p <- .raw.plot.parts(x)
  title <- "Time gaps and how GGIR filled them"
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  M <- p$meta
  ws3 <- M$windowsizes[1]
  ws2 <- M$windowsizes[2]
  cols <- .raw.plot.colours()
  metashort <- M$metashort
  time_short <- .raw.plot.time(metashort, p$tz)
  n <- nrow(metashort)
  sf <- if (!is.null(M$settings$sf)) M$settings$sf else NA_real_
  rec_start <- time_short[1]
  rec_end <- time_short[n] + ws3

  # short labels: a discrete y axis takes the width of its longest label
  lev_blocks <- "Blocks read"
  lev_raw <- "Filled, raw level"
  lev_epoch <- "Filled, epoch level"
  lev_samples <- "Reader sample gaps"

  # blocks from the qclog
  q <- M$qclog
  blocks <- NULL
  if (is.data.frame(q) && nrow(q) > 0 && all(c("start", "blockLengthSeconds") %in% names(q))) {
    st <- suppressWarnings(as.numeric(q$start))
    ok <- !is.na(st) & st > 1e9
    if (any(ok)) {
      len <- suppressWarnings(as.numeric(q$blockLengthSeconds))
      blocks <- data.frame(row = lev_blocks, block = seq_len(nrow(q))[ok],
                           xmin = as.POSIXct(st[ok], origin = "1970-01-01", tz = p$tz),
                           xmax = as.POSIXct(st[ok] + len[ok], origin = "1970-01-01", tz = p$tz),
                           gaps = if ("timegaps_n" %in% names(q)) as.numeric(q$timegaps_n)[ok] else NA_real_,
                           minutes = if ("timegaps_min" %in% names(q)) as.numeric(q$timegaps_min)[ok] else NA_real_,
                           stringsAsFactors = FALSE)
      # three short lines, since the label has to fit inside the block's own width
      blocks$label <- ifelse(is.na(blocks$gaps), paste0("block ", blocks$block),
                             paste0("block ", blocks$block, "\n", blocks$gaps, " gaps\n", round(blocks$minutes, 1), " min"))
    }
  }
  # fills from the epoch footprint
  runs <- .raw.plot.fill.runs(metashort, ws3, ws2)
  fills <- if (nrow(runs) > 0) data.frame(row = ifelse(runs$level == "epoch", lev_epoch, lev_raw),
                                          xmin = time_short[runs$start_epoch],
                                          xmax = time_short[runs$end_epoch] + ws3,
                                          start_epoch = runs$start_epoch, end_epoch = runs$end_epoch,
                                          length = runs$length, level = runs$level, stringsAsFactors = FALSE) else NULL
  # sample-level missingness when available
  if (is.null(missingness) && !is.null(M$missingness)) missingness <- M$missingness
  samples <- NULL
  if (is.data.frame(missingness) && nrow(missingness) > 0 && all(c("time", "n_missing") %in% names(missingness))) {
    mt <- missingness$time
    mt <- if (inherits(mt, "POSIXt")) as.numeric(mt) else suppressWarnings(as.numeric(mt))
    dur <- if (!is.na(sf)) as.numeric(missingness$n_missing) / sf else rep(0, length(mt))
    samples <- data.frame(row = lev_samples, xmin = as.POSIXct(mt, origin = "1970-01-01", tz = p$tz),
                          xmax = as.POSIXct(mt + pmax(dur, ws3), origin = "1970-01-01", tz = p$tz),
                          n_missing = as.numeric(missingness$n_missing), stringsAsFactors = FALSE)
  }
  rows <- c(lev_blocks, lev_raw, lev_epoch, if (!is.null(samples)) lev_samples)
  n_gaps <- if (!is.null(blocks)) sum(blocks$gaps, na.rm = TRUE) else NA
  min_gaps <- if (!is.null(blocks)) sum(blocks$minutes, na.rm = TRUE) else NA
  n_epoch <- if (!is.null(fills)) sum(fills$level == "epoch") else 0
  # filled epochs are the replicas of each run (the run's first epoch holds the carried value)
  h_epoch <- if (!is.null(fills)) sum(fills$length[fills$level == "epoch"] - 1) * ws3 / 3600 else 0
  limit_min <- max(c(((ws2 / 60) * 6), 90))
  subtitle <- if (!is.na(n_gaps) && n_gaps > 0) {
    paste0(n_gaps, " gaps totalling ", round(min_gaps, 1), " min (", round(min_gaps / 1440, 1), " of ",
           round(n * ws3 / 86400, 1), " days) were filled by last value carried forward; ",
           n_epoch, " exceeded ", limit_min, " min and ", round(h_epoch, 1), " h were filled at epoch level")
  } else if (!is.null(blocks)) {
    "The reader logged no time gaps in this recording"
  } else {
    "No gap log is available for this recording (the reader does not report time gaps for this format)"
  }
  if (is.null(blocks) && is.null(fills) && is.null(samples)) {
    return(.raw.plot.empty("No time gaps were logged and no filled epochs were found in this recording.", title))
  }

  frame <- data.frame(row = factor(rows, levels = rev(rows)))
  g <- ggplot2::ggplot() +
    ggplot2::geom_blank(data = frame, ggplot2::aes(y = .data$row)) +
    ggplot2::annotate("rect", xmin = rec_start, xmax = rec_end, ymin = -Inf, ymax = Inf, fill = "grey97", colour = NA)
  if (!is.null(blocks)) {
    blocks$row <- factor(blocks$row, levels = rev(rows))
    g <- g +
      ggplot2::geom_rect(data = blocks,
                         ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                      ymin = as.numeric(.data$row) - 0.35, ymax = as.numeric(.data$row) + 0.35),
                         fill = cols[["block"]], colour = "grey40", linewidth = 0.3) +
      ggplot2::geom_text(data = blocks,
                         ggplot2::aes(x = .data$xmin, y = as.numeric(.data$row), label = .data$label),
                         hjust = 0, nudge_x = 900, size = 2.5, lineheight = 0.85, colour = "#111111")
  }
  if (!is.null(fills)) {
    fills$row <- factor(fills$row, levels = rev(rows))
    g <- g + ggplot2::geom_rect(data = fills,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = as.numeric(.data$row) - 0.35, ymax = as.numeric(.data$row) + 0.35,
                                             fill = .data$level), colour = NA)
  }
  if (!is.null(samples)) {
    samples$row <- factor(samples$row, levels = rev(rows))
    g <- g + ggplot2::geom_rect(data = samples,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = as.numeric(.data$row) - 0.35, ymax = as.numeric(.data$row) + 0.35),
                                fill = cols[["reference"]], colour = NA)
  }
  g <- g +
    ggplot2::scale_fill_manual(values = c(raw = cols[["raw_fill"]], epoch = cols[["epoch_fill"]]),
                               labels = c(raw = paste0("raw level (up to ", limit_min, " min)"),
                                          epoch = paste0("epoch level (over ", limit_min, " min)")),
                               name = "Fill", drop = TRUE) +
    ggplot2::scale_x_datetime(timezone = p$tz, date_breaks = "1 day", labels = function(x) .format_english(x, "%a %d %b", tz = p$tz),
                              expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = NULL, y = NULL,
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Fill positions are the runs of identical consecutive ", ws3, "-s epochs; gaps shorter than one epoch are counted in the block figures but leave no footprint. Epoch-level fills are scored non-wear 3. Times in ", p$tz, "."))) +
    .raw.plot.theme() +
    ggplot2::theme(legend.position = "top", panel.grid.major.y = ggplot2::element_blank())
  g
}

# CALIBRATION TRACE PER CHUNK

#' Calibration Trace per 12-Hour Chunk
#'
#' What g.calibrate saw as it loaded the recording chunk by chunk: the calibration error
#' before and after the fit (mg) and the number of still windows after every chunk, with the
#' 10 mg acceptance line. It shows whether the fit was converging and whether more data would
#' have reached GGIR's acceptance (error under 10 mg and below the starting error, and more
#' than minloadcrit hours loaded).
#'
#' @details
#' There is no GGIR plot of this; the trace is canhrActi's per-chunk record of g.calibrate
#' (\code{calibration$chunks}). The final pass on an empty block repeats the previous
#' figures, as it does in GGIR.
#'
#' @param x A \code{canhrActi_raw} object or a \code{canhrActi_raw_calibration}.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot when no chunk was read (corrupt, too-short,
#'   skipped input, a supplied calibration, or calibration switched off).
#' @examples
#' \dontrun{
#' plot_raw_chunks(x)
#' }
#' @seealso \code{\link{plot_raw_calibration}}
#' @export plot_raw_chunks
plot_raw_chunks <- function(x, ...) {
  p <- .raw.plot.parts(x)
  title <- "Auto-calibration as the recording was loaded"
  cal <- p$cal
  if (is.null(cal)) return(.raw.plot.empty(if (identical(p$state, "ok")) "No calibration object is attached to this recording." else p$state, title))
  ch <- cal$chunks
  bare <- inherits(x, "canhrActi_raw_calibration")
  if (!bare && !identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  if (!is.data.frame(ch) || nrow(ch) == 0 || all(as.numeric(ch$rows) == 0, na.rm = TRUE)) {
    msg <- if (identical(cal$source, "supplied")) "The calibration was supplied, so no chunks were loaded for a fit." else
      if (is.data.frame(ch) && nrow(ch) > 0) "No calibration chunk with data was read for this recording (every block was empty or below GGIR's floor)." else
        "No calibration chunk was read for this recording."
    return(.raw.plot.empty(msg, title))
  }
  cols <- .raw.plot.colours()
  minload <- if (!is.null(cal$settings$minloadcrit)) cal$settings$minloadcrit else 168
  err_label <- "Error (mg)"
  win_label <- "Still windows"
  d <- rbind(
    data.frame(panel = err_label, block = ch$block, hours = ch$hours, series = "before fit",
               y = as.numeric(ch$error_start) * 1000, accepted = ch$accepted, stringsAsFactors = FALSE),
    data.frame(panel = err_label, block = ch$block, hours = ch$hours, series = "after fit",
               y = as.numeric(ch$error_end) * 1000, accepted = ch$accepted, stringsAsFactors = FALSE),
    data.frame(panel = win_label, block = ch$block, hours = ch$hours, series = "still windows",
               y = as.numeric(ch$still_windows), accepted = ch$accepted, stringsAsFactors = FALSE))
  d$panel <- factor(d$panel, levels = c(err_label, win_label))
  d$series <- factor(d$series, levels = c("before fit", "after fit", "still windows"))
  d <- d[!is.na(d$y), ]
  ref <- data.frame(panel = factor(err_label, levels = c(err_label, win_label)), y = 10)
  x_labels <- paste0(ch$block, "\n", round(ch$hours), " h")
  reached <- max(ch$hours, na.rm = TRUE)
  subtitle <- paste0(nrow(ch), " chunk", if (nrow(ch) == 1) "" else "s", " of ",
                     if (!is.null(cal$settings$blocksize)) paste0(round(cal$settings$blocksize / 3600), " h") else "12 h",
                     "; GGIR accepts when the error falls under 10 mg and below the start, with more than ",
                     minload, " h loaded; this recording reached ", round(reached, 1), " h",
                     if (any(ch$accepted, na.rm = TRUE)) " and was accepted" else " and was not accepted")
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$block, y = .data$y, colour = .data$series)) +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = .data$y), colour = cols[["reference"]],
                        linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$accepted), size = 2.4) +
    ggplot2::scale_colour_manual(values = c("before fit" = cols[["before"]], "after fit" = cols[["after"]],
                                            "still windows" = cols[["angle"]]), name = NULL) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), labels = c(`FALSE` = "not accepted", `TRUE` = "accepted"),
                                name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = ch$block, labels = x_labels) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y", strip.position = "left") +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = "Chunk (hours of data loaded)", y = NULL,
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Dashed: the 10 mg acceptance line. GGIR: ", cal$qcmessage, "."))) +
    .raw.plot.theme() +
    ggplot2::theme(legend.position = "top", strip.placement = "outside",
                   strip.background = ggplot2::element_blank())
  g
}

# HOURS WORN PER DAY

#' Hours Worn per Calendar Day Against the Valid-Day Criterion
#'
#' One bar per calendar day with the valid (worn) hours GGIR counts for it, the recorded
#' hours of the day as a light outline behind it, and a dashed line at includedaycrit (16 h by
#' default): the "Duration monitor worn" chart of GGIR's legacy report, drawn for every day
#' rather than only the valid ones.
#'
#' @details
#' The bars of g.plot5 page 1, from the daily table of \code{\link{raw.wear.decision}}.
#' Changed: days below the criterion are drawn too, the criterion line is the one in use
#' rather than the hard-coded 16, and the bars are filled by validity.
#'
#' @param x A \code{canhrActi_raw} object, a \code{canhrActi_raw_wear}, or a
#'   \code{canhrActi_raw_meta} (the wear decision is then computed).
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for corrupt, too-short or skipped input.
#' @examples
#' \dontrun{
#' plot_raw_wear_days(x)
#' }
#' @seealso \code{\link{plot_raw_quality}}, \code{\link{raw.wear.decision}}
#' @export plot_raw_wear_days
plot_raw_wear_days <- function(x, ...) {
  p <- .raw.plot.parts(x)
  title <- "Hours worn per calendar day"
  w <- p$wear
  if ((is.null(w) || !identical(w$status, "ok")) && identical(p$state, "ok")) {
    w <- tryCatch(raw.wear.decision(p$meta), error = function(e) NULL)
  }
  if (is.null(w) || !identical(w$status, "ok") || !is.data.frame(w$daily) || nrow(w$daily) == 0) {
    return(.raw.plot.empty(if (!identical(p$state, "ok")) p$state else
      "No wear decision is available for this recording, so the days cannot be drawn.", title))
  }
  cols <- .raw.plot.colours()
  crit <- if (!is.null(w$settings$includedaycrit)) w$settings$includedaycrit[1] else 16
  d <- w$daily
  d$label <- paste0(substr(as.character(d$weekday), 1, 3), "\n", .format_english(as.Date(d$date), "%d %b"))
  d$label <- factor(d$label, levels = d$label)
  d$validity <- factor(ifelse(d$valid, "valid day", "below the criterion"), levels = c("valid day", "below the criterion"))
  d$text <- format(round(d$n_valid_hours, 1), nsmall = 1)
  n_valid <- sum(d$valid)
  subtitle <- paste0(n_valid, " of ", nrow(d), " day", if (nrow(d) == 1) "" else "s", " reach", if (n_valid == 1) "es" else "",
                     " ", crit, " valid hours (GGIR includedaycrit); ", round(w$wear_dur_def_proto_day, 2),
                     " of ", round(w$meas_dur_dys, 2), " recorded days worn")
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$label)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_hours), fill = NA, colour = "grey70", linewidth = 0.4, width = 0.7) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_valid_hours, fill = .data$validity), width = 0.7, colour = "grey40", linewidth = 0.2) +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", colour = cols[["reference"]], linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(y = .data$n_valid_hours, label = .data$text), vjust = -0.4, size = 3.4, colour = "#111111") +
    ggplot2::scale_fill_manual(values = c("valid day" = cols[["valid"]], "below the criterion" = cols[["invalid"]]),
                               name = NULL, drop = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0, max(26, max(d$n_hours, na.rm = TRUE) * 1.1)),
                                breaks = seq(0, 24, by = 4), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = NULL, y = "Hours",
                  caption = .raw.plot.wrap(paste0(if (!is.na(p$filename)) paste0(p$filename, ". ") else "",
                                   "Filled: valid (worn) hours; outline: hours recorded that day; dashed: ", crit, " h criterion."))) +
    .raw.plot.theme() +
    ggplot2::theme(legend.position = "top", panel.grid.major.x = ggplot2::element_blank())
  g
}
