# Ported in part from GGIR 3.3-9 R/visualReport.R (the part-5 class ribbon: the legend
# families and within-family lightness ramp, the class rectangles from mdat$class_id, the
# transparent white invalid overlay and the day-edge rows) and R/g.intensitygradient.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. GGIR's base-graphics pages are
# replaced by ggplot2 objects, the colours are canhrActi's, the families are matched on
# the class names part 5 writes (GGIR's own grep patterns match none of them), the traces
# are left to plot_raw_days(), and the sleep period, non-wear and analysed window become
# three thin strips. The per-day composition, block-length and intensity-gradient figures
# have no GGIR original. The four dur_day_total_* columns are never stacked with the
# classes they overlap.

# HELPERS

#' Colours of the Time-Use Figures That Are Not Behaviour Classes
#'
#' The three strips of the classified series (sleep period, non-wear, analysed window), the
#' wash GGIR lays over invalid epochs, and the fitted-line colour of the gradient figure.
#' Sleep colours come from \code{canhrActi_palette("sleep")}; the fallbacks are the same hex
#' values.
#'
#' @return A named character vector of hex colours.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.colours <- function() {
  sleep <- c(sleep = "#236192", wake = "#FFCD00")
  sl <- tryCatch(canhrActi_palette("sleep"), error = function(e) NULL)
  if (!is.null(sl)) for (nm in names(sleep)) if (!is.null(sl[[nm]])) sleep[[nm]] <- sl[[nm]]
  c(spt = unname(sleep[["sleep"]]),   # the sleep period strip above each day row
    nonwear = "#8A94A0",              # GGIR's "grey" invalid legend item
    wash = "#FFFFFF",                 # visualReport's transparantWhite over invalid epochs
    window = "#71984A",               # the analysed-window strip below each day row
    fit = "#111111",                  # the intensity-gradient fit and the reference marks
    reference = "#94A3B8")
}

#' The Three Markers the Classified Series Draws Beside the Behaviour Classes
#'
#' Their names are legend entries, so they live in one place.
#'
#' @return A named character vector: the legend label of each marker.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.markers <- function() {
  c(spt = "sleep period (SPT)", nonwear = "non-wear or invalid",
    window = "in an analysed window")
}

#' Split a Behaviour Class Name Into Its Family and Its Place Inside That Family
#'
#' Part 5 names every class from whether the epoch is inside the sleep period, the intensity
#' band and the bout length that claimed it (\code{spt_sleep}, \code{spt_wake_LIG},
#' \code{day_IN_unbt}, \code{day_IN_bts_20_30}, \code{day_MVPA_bts_10}, \code{day_nap}).
#' Reading the shape off the name makes the colouring work for any bout configuration.
#'
#' @details The family grouping is visualReport's: sleep, wake inside the sleep period,
#'   inactivity, light, and moderate to vigorous. GGIR greps for "inactive", "lipa" and
#'   "mod|vig|mvpa", which match none of the names part 5 writes. \code{rank} orders a family
#'   from the shortest claim to the longest: an unbouted class sorts before every bout band,
#'   and a bout band sorts by its lower edge.
#'
#' @param Lnames The class names, \code{x$levels}.
#' @return data.frame(name, family, intensity, rank, bout_min), one row per class.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.classparts <- function(Lnames) {
  intensities <- c(IN = 1, LIG = 2, MOD = 3, VIG = 4, MVPA = 5)
  n <- length(Lnames)
  family <- rep("other", n)
  intensity <- rep(NA_character_, n)
  rank <- rep(0, n)
  bout_min <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    nm <- Lnames[i]
    if (identical(nm, "spt_sleep")) {
      family[i] <- "sleep"
      next
    }
    if (identical(nm, "day_nap")) {
      family[i] <- "nap"
      next
    }
    if (grepl("^spt_wake_", nm)) {
      family[i] <- "wake"
      intensity[i] <- sub("^spt_wake_", "", nm)
      idx <- intensities[intensity[i]]
      rank[i] <- if (is.na(idx)) 9 else idx
      next
    }
    if (grepl("^day_", nm)) {
      rest <- sub("^day_", "", nm)
      int <- sub("_.*$", "", rest)
      intensity[i] <- int
      family[i] <- if (int %in% c("MOD", "VIG", "MVPA")) "MVPA" else int
      if (grepl("_bts_", rest)) {
        band <- sub("^.*_bts_", "", rest)
        edge <- suppressWarnings(as.numeric(sub("_.*$", "", band)))
        bout_min[i] <- edge
        rank[i] <- if (is.na(edge)) 999 else edge
      } else {
        # unbouted: before every band, ordered among themselves by intensity
        idx <- intensities[int]
        rank[i] <- -1 + (if (is.na(idx)) 9 else idx) / 100
      }
    }
  }
  data.frame(name = Lnames, family = family, intensity = intensity, rank = rank,
             bout_min = bout_min, stringsAsFactors = FALSE)
}

#' One Colour Per Behaviour Class
#'
#' A family colour with a lightness ramp inside the family, visualReport's scheme. The ramp
#' runs from a light tint to a dark shade over the classes ordered by rank, so within one
#' intensity the longer the bout the darker the band.
#'
#' @param Lnames The class names, \code{x$levels}.
#' @return A character vector of hex colours named by class, in the order of \code{Lnames}.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.classcolours <- function(Lnames) {
  ends <- list(
    sleep = c("#12324B", "#12324B"),   # the sleep period itself, the darkest band on the page
    wake  = c("#FFE9A3", "#B8860B"),   # GGIR's yellow3 family, wake inside the sleep period
    IN    = c("#DCE0E6", "#3F4A5A"),   # slate: inactivity
    LIG   = c("#CBEAF9", "#1C7FB6"),   # sky blue: light activity
    MVPA  = c("#FFD08A", "#8C2D04"),   # amber to rust: moderate, vigorous and MVPA bouts
    nap   = c("#C9BEE4", "#8E7CC3"),   # the nap class of the rest analysis
    other = c("#E5E5E5", "#9A9A9A"))
  if (length(Lnames) == 0) return(stats::setNames(character(0), character(0)))
  parts <- .raw.timeuse.plot.classparts(Lnames)
  out <- rep(NA_character_, length(Lnames))
  for (fam in unique(parts$family)) {
    j <- which(parts$family == fam)
    j <- j[order(parts$rank[j], parts$name[j])]
    pal <- if (!is.null(ends[[fam]])) ends[[fam]] else ends[["other"]]
    cols <- if (length(j) == 1) pal[2] else grDevices::colorRampPalette(pal)(length(j))
    out[j] <- cols
  }
  stats::setNames(out, Lnames)
}

#' Take the Pieces a Time-Use Figure Needs Out of a canhrActi_raw_timeuse
#'
#' Accepts the object \code{\link{raw.timeuse}} returns, or a plain list carrying the same
#' members, and resolves which threshold triple, sleep definition and timewindow to draw.
#' Each defaults to the first the object carries.
#'
#' @param x The object.
#' @param threshold NULL for the first threshold triple, or its name ("40_100_400").
#' @param sibdef NULL for the first sleep definition, or its name ("T5A5").
#' @param timewindow NULL for the first timewindow, or "MM", "WW" or "OO".
#' @param need "series" when the figure cannot be drawn without the exported time series,
#'   "daysummary" when the day table is enough.
#' @return list(daysummary, mdat, levels, legend, colours, triple, sibdef, timewindow,
#'   timewindows, tz, filename, id, ws3, settings, state); \code{state} is "ok" or the
#'   sentence the empty-state plot should carry.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.parts <- function(x, threshold = NULL, sibdef = NULL, timewindow = NULL,
                                    need = c("series", "daysummary")) {
  need <- match.arg(need)
  if (!is.list(x) || (!inherits(x, "canhrActi_raw_timeuse") &&
                      !all(c("daysummary", "levels") %in% names(x)))) {
    stop("x must be a canhrActi_raw_timeuse object from raw.timeuse()", call. = FALSE)
  }
  set <- if (is.list(x$settings)) x$settings else list()
  tz <- if (!is.null(set$desiredtz) && length(set$desiredtz) == 1 && nzchar(set$desiredtz)) {
    set$desiredtz
  } else Sys.timezone()
  ws3 <- if (!is.null(set$windowsizes)) as.numeric(set$windowsizes[1]) else NA_real_
  filename <- if (!is.null(set$filename)) as.character(set$filename)[1] else NA_character_
  id <- if (!is.null(set$ID)) as.character(set$ID)[1] else NA_character_
  ds <- x$daysummary
  Lnames <- x$levels
  triple <- NA_character_; sibd <- NA_character_; twi <- NA_character_; mdat <- NULL
  tws <- character(0)
  series <- x$series
  if (!is.null(series) && length(series) > 0) {
    triple <- if (is.null(threshold)) names(series)[1] else as.character(threshold)[1]
    if (!triple %in% names(series)) {
      return(list(state = paste0("This object holds no threshold set called \"", triple,
                                 "\"; it holds ", paste(names(series), collapse = ", "), ".")))
    }
    sset <- series[[triple]]
    sibd <- if (is.null(sibdef)) names(sset)[1] else as.character(sibdef)[1]
    if (!sibd %in% names(sset)) {
      return(list(state = paste0("This object holds no sleep definition called \"", sibd,
                                 "\"; it holds ", paste(names(sset), collapse = ", "), ".")))
    }
    tset <- sset[[sibd]]
    tws <- names(tset)
    twi <- if (is.null(timewindow)) tws[1] else as.character(timewindow)[1]
    if (!twi %in% tws) {
      return(list(state = paste0("This object holds no time window called \"", twi,
                                 "\"; it holds ", paste(tws, collapse = ", "), ".")))
    }
    mdat <- tset[[twi]]
    if (is.list(Lnames) && !is.data.frame(Lnames)) Lnames <- Lnames[[triple]]
  } else {
    if (is.list(Lnames) && !is.data.frame(Lnames)) Lnames <- Lnames[[1]]
    if (is.data.frame(ds) && nrow(ds) > 0) {
      if (all(c("TRLi", "TRMi", "TRVi") %in% names(ds))) {
        triple <- paste(ds$TRLi[1], ds$TRMi[1], ds$TRVi[1], sep = "_")
      }
      if ("sleepparam" %in% names(ds)) sibd <- as.character(ds$sleepparam[1])
      if ("window" %in% names(ds)) {
        tws <- unique(as.character(ds$window))
        tws <- tws[tws %in% c("MM", "WW", "OO")]
        twi <- if (is.null(timewindow)) {
          if (length(tws) > 0) tws[1] else NA_character_
        } else as.character(timewindow)[1]
      }
    }
  }
  if (is.na(ws3) && !is.null(mdat) && nrow(mdat) > 1) ws3 <- mdat$timenum[2] - mdat$timenum[1]
  if (is.na(ws3)) ws3 <- 5
  st <- if (is.list(x$status) && !is.null(x$status$state)) as.character(x$status$state)[1] else "ok"
  msg <- if (is.list(x$status) && length(x$status$messages) > 0) x$status$messages[1] else NULL
  state <- "ok"
  if (!identical(st, "ok")) {
    state <- switch(
      st,
      skipped = "The recording is corrupt, too short or was skipped, so part 5 produced nothing.",
      no_nights = "No part-4 night belongs to this recording, so part 5 produced nothing.",
      no_valid_night = paste0("Part 4 found no usable night for this recording, so part 5 ",
                              "produced nothing."),
      no_sib_definitions = paste0("Part 3 found no sustained inactivity bout, so there is no ",
                                  "sleep definition to analyse."),
      no_windows = paste0("No day window of this recording is longer than the 900 second ",
                          "minimum, so there is nothing to draw."),
      if (!is.null(msg)) msg else paste0("Part 5 produced no output for this recording ",
                                         "(state \"", st, "\")."))
  } else if (need == "series" && (is.null(mdat) || !is.data.frame(mdat) || nrow(mdat) == 0)) {
    state <- paste0("This object carries no exported time series, so the epochs cannot be ",
                    "drawn. Run raw.timeuse() with save_ms5rawlevels = TRUE.")
  } else if (!is.data.frame(ds) || nrow(ds) == 0) {
    state <- "The part-5 day table has no rows, so there is nothing to draw."
  } else if (length(Lnames) == 0) {
    state <- "This object carries no behaviour class names, so the classes cannot be named."
  }
  list(daysummary = ds, mdat = mdat, levels = Lnames, legend = x$legend,
       colours = .raw.timeuse.plot.classcolours(Lnames),
       triple = triple, sibdef = sibd, timewindow = twi, timewindows = tws,
       tz = tz, filename = filename, id = id, ws3 = ws3, settings = set, state = state)
}

#' The Caption Line Every Time-Use Figure Carries
#'
#' @param p The result of \code{\link{.raw.timeuse.plot.parts}}.
#' @param extra One sentence about this figure, or NULL.
#' @return One wrapped string.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.caption <- function(p, extra = NULL) {
  fname <- p$filename
  if (!is.na(fname)) fname <- sub("[.]RData$", "", fname)
  cfg <- NULL
  if (!is.na(p$triple)) {
    thr <- strsplit(p$triple, "_", fixed = TRUE)[[1]]
    if (length(thr) == 3) {
      cfg <- paste0("thresholds ", thr[1], "/", thr[2], "/", thr[3], " mg")
      if (!is.na(p$sibdef)) cfg <- paste0(cfg, ", sleep definition ", p$sibdef)
    }
  }
  bits <- c(if (!is.na(fname)) fname else NULL, cfg, extra,
            paste0("Times in ", p$tz, "."))
  .raw.plot.wrap(paste(bits, collapse = ". "))
}

#' Clock Hour of a POSIXct, in the Zone the Vector Carries
#'
#' Clock time and not elapsed time, so the figure keeps local hours through a daylight-saving
#' change.
#'
#' @param t POSIXct.
#' @return Numeric hours since the local midnight that opens that date.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.hour <- function(t) {
  as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60 +
    as.numeric(format(t, "%S")) / 3600
}

#' Runs of Equal Value That Never Cross a Day Boundary
#'
#' A run is a maximal stretch of epochs with one value inside one calendar day. A rectangle
#' runs from the start of its first epoch to the end of its last one (the last epoch's clock
#' hour plus one epoch), as GGIR's \code{correctRect} does.
#'
#' @param value The vector to run-length encode (class names, or a 0/1 marker).
#' @param day Day label of each epoch, the same length.
#' @param hour Clock hour of each epoch, the same length.
#' @param ws3 Epoch length in seconds.
#' @param keep NULL to keep every run, or the values to keep (1 for a 0/1 marker).
#' @return data.frame(value, day, xmin, xmax, first_epoch, last_epoch, n_epochs).
#' @keywords internal
#' @noRd
.raw.timeuse.plot.runs <- function(value, day, hour, ws3, keep = NULL) {
  n <- length(value)
  empty <- data.frame(value = character(0), day = character(0), xmin = numeric(0),
                      xmax = numeric(0), first_epoch = integer(0), last_epoch = integer(0),
                      n_epochs = integer(0), stringsAsFactors = FALSE)
  if (n == 0) return(empty)
  key <- paste0(day, "\r", as.character(value))
  rl <- rle(key)
  ends <- cumsum(rl$lengths)
  starts <- ends - rl$lengths + 1L
  out <- data.frame(value = as.character(value)[starts], day = day[starts],
                    xmin = hour[starts], xmax = hour[ends] + ws3 / 3600,
                    first_epoch = as.integer(starts), last_epoch = as.integer(ends),
                    n_epochs = as.integer(rl$lengths), stringsAsFactors = FALSE)
  if (!is.null(keep)) out <- out[out$value %in% as.character(keep), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The Day Rows of a Classified Series
#'
#' @param mdat The exported 14-column series.
#' @param days NULL for every day, or the dates to keep ("YYYY-MM-DD" or Date).
#' @return list(keep, day, hour, time), or NULL when no epoch survives.
#' @keywords internal
#' @noRd
.raw.timeuse.plot.days <- function(mdat, days = NULL) {
  t <- mdat$timestamp
  if (is.null(t)) t <- as.POSIXct(mdat$timenum, origin = "1970-01-01")
  day <- format(t, "%Y-%m-%d")
  keep <- rep(TRUE, length(day))
  if (!is.null(days)) keep <- day %in% format(as.Date(days), "%Y-%m-%d")
  if (!any(keep)) return(NULL)
  list(keep = which(keep), day = day[keep], hour = .raw.timeuse.plot.hour(t[keep]),
       time = t[keep])
}

# CLASSIFIED SERIES

#' The Part-5 Behaviour Classes, One Row Per Day
#'
#' Every epoch of the recording drawn as the behaviour class part 5 put it in, one row per
#' calendar day and time of day across, with the sleep period marked above each row, the
#' non-wear and invalid epochs marked below it and washed out of the band, and the part of
#' the day inside an analysed window marked beneath that. The eighteen classes partition
#' every window, so the band of one full day is the 1440 minutes the day table reports.
#' The darkest band is the sleep period, slate is inactivity (darker the longer the bout),
#' sky blue is light activity and amber to rust is moderate to vigorous.
#'
#' @details Redraws the class ribbon of GGIR's visualReport rather than transcribing it: one
#'   row per day, a rectangle for every run of one \code{class_id}, a family colour per class
#'   group with a lightness ramp, and a transparent white wash over the invalid epochs. The
#'   series drawn is \code{x$series[[threshold]][[sibdef]][[timewindow]]}; \code{class_id},
#'   \code{SleepPeriodTime} and \code{invalidepoch} do not depend on the timewindow, so only
#'   the analysed-window strip changes with it.
#'
#' @param x A \code{canhrActi_raw_timeuse} from \code{\link{raw.timeuse}}.
#' @param timewindow NULL for the first timewindow the object carries, or "MM", "WW" or "OO".
#' @param threshold NULL for the first threshold set, or its name, for example "40_100_400".
#' @param sibdef NULL for the first sleep definition, or its name, for example "T5A5".
#' @param days NULL for every day, or the calendar dates to draw.
#' @param show_invalid TRUE to wash out the invalid epochs and draw the non-wear strip.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for a corrupt, too short or windowless
#'   object.
#' @examples
#' \dontrun{
#' plot_raw_timeuse(tu)
#' plot_raw_timeuse(tu, timewindow = "WW", days = "2025-10-08")
#' }
#' @seealso \code{\link{plot_raw_timeuse_day}}, \code{\link{plot_raw_timeuse_bouts}},
#'   \code{\link{plot_raw_timeuse_gradient}}, \code{\link{raw.timeuse}}
#' @export plot_raw_timeuse
plot_raw_timeuse <- function(x, timewindow = NULL, threshold = NULL, sibdef = NULL,
                             days = NULL, show_invalid = TRUE, ...) {
  title <- "Behaviour classes, one row per day"
  p <- .raw.timeuse.plot.parts(x, threshold, sibdef, timewindow, need = "series")
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  m <- p$mdat
  D <- .raw.timeuse.plot.days(m, days)
  if (is.null(D)) {
    return(.raw.plot.empty(paste0("No epoch of this recording falls on the day",
                                  if (length(days) == 1) "" else "s", " asked for."), title))
  }
  idx <- D$keep
  Lnames <- p$levels
  mk <- .raw.timeuse.plot.markers()
  base <- .raw.timeuse.plot.colours()
  ws3 <- p$ws3

  class_name <- Lnames[as.integer(m$class_id[idx]) + 1L]
  class_name[is.na(class_name)] <- "unclassified"
  bands <- .raw.timeuse.plot.runs(class_name, D$day, D$hour, ws3)
  spt <- .raw.timeuse.plot.runs(as.integer(m$SleepPeriodTime[idx] == 1), D$day, D$hour, ws3,
                                keep = 1)
  nonwear <- .raw.timeuse.plot.runs(as.integer(m$invalidepoch[idx] == 1), D$day, D$hour, ws3,
                                    keep = 1)
  inwin <- .raw.timeuse.plot.runs(as.integer(m$window[idx] > 0), D$day, D$hour, ws3, keep = 1)

  udays <- unique(D$day)
  row_of <- stats::setNames(seq_along(udays), udays)
  bands$row <- unname(row_of[bands$day])
  spt$row <- unname(row_of[spt$day])
  nonwear$row <- unname(row_of[nonwear$day])
  inwin$row <- unname(row_of[inwin$day])
  levs <- c(Lnames, if (any(bands$value == "unclassified")) "unclassified" else NULL)
  fill_values <- c(p$colours, unclassified = "#E5E5E5",
                   stats::setNames(c(base[["spt"]], base[["nonwear"]], base[["window"]]),
                                   c(mk[["spt"]], mk[["nonwear"]], mk[["window"]])))
  bands$value <- factor(bands$value, levels = levs)
  spt$marker <- factor(mk[["spt"]], levels = unname(mk))
  nonwear$marker <- factor(mk[["nonwear"]], levels = unname(mk))
  inwin$marker <- factor(mk[["window"]], levels = unname(mk))

  n_ep <- length(idx)
  n_inv <- sum(m$invalidepoch[idx] == 1)
  n_spt <- sum(m$SleepPeriodTime[idx] == 1)
  n_win <- sum(m$window[idx] > 0)
  subtitle <- paste0(length(udays), " day", if (length(udays) == 1) "" else "s", ", ",
                     round(n_ep * ws3 / 3600, 1), " h at ", ws3, " s epochs; ",
                     round(n_win * ws3 / 3600, 1), " h in a ", p$timewindow, " window, ",
                     round(n_spt * ws3 / 3600, 1), " h sleep period, ",
                     round(n_inv * ws3 / 3600, 1), " h non-wear")

  g <- ggplot2::ggplot()
  if (nrow(spt) > 0) {
    g <- g + ggplot2::geom_rect(data = spt,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = .data$row - 0.44, ymax = .data$row - 0.34,
                                             fill = .data$marker))
  }
  g <- g + ggplot2::geom_rect(data = bands,
                              ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                           ymin = .data$row - 0.30, ymax = .data$row + 0.30,
                                           fill = .data$value))
  if (isTRUE(show_invalid) && nrow(nonwear) > 0) {
    # the transparent white over everything already drawn
    g <- g + ggplot2::geom_rect(data = nonwear,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = .data$row - 0.30, ymax = .data$row + 0.30),
                                fill = base[["wash"]], alpha = 0.68) +
      ggplot2::geom_rect(data = nonwear,
                         ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                      ymin = .data$row + 0.34, ymax = .data$row + 0.42,
                                      fill = .data$marker))
  }
  if (nrow(inwin) > 0) {
    g <- g + ggplot2::geom_rect(data = inwin,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = .data$row + 0.45, ymax = .data$row + 0.51,
                                             fill = .data$marker))
  }
  # ggplot2 4 draws an empty legend key for a level no layer carries, so a zero-area
  # rectangle carries every class and marker; drop = FALSE alone keeps the entry, not the colour
  keys <- data.frame(value = factor(c(levs, unname(mk)), levels = c(levs, unname(mk))),
                     stringsAsFactors = FALSE)
  g <- g +
    ggplot2::geom_rect(data = keys, ggplot2::aes(fill = .data$value), xmin = 0, xmax = 0,
                       ymin = 0, ymax = 0, inherit.aes = FALSE) +
    ggplot2::scale_fill_manual(values = fill_values, breaks = c(levs, unname(mk)),
                               name = NULL, drop = FALSE, na.value = "#E5E5E5") +
    ggplot2::scale_x_continuous(breaks = seq(0, 24, by = 3),
                                labels = sprintf("%02d", seq(0, 24, by = 3)),
                                limits = c(0, max(24, max(bands$xmax, na.rm = TRUE))),
                                expand = ggplot2::expansion(mult = c(0.002, 0.002))) +
    ggplot2::scale_y_reverse(breaks = seq_along(udays),
                             labels = .format_english(as.Date(udays), "%a %d %b"),
                             expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = "Clock hour",
                  y = NULL,
                  caption = .raw.timeuse.plot.caption(
                    p, paste0("Band: the behaviour class of every epoch, from the exported ",
                              "series; the strips above and below each row are the sleep ",
                              "period, the non-wear epochs and the part of the day inside a ",
                              p$timewindow, " window"))) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
    .raw.plot.theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.9, "lines"))
  g
}

# PER-DAY COMPOSITION

#' Minutes in Each Behaviour Class, Per Analysed Window
#'
#' One stacked bar per window of the part-5 day table: the minutes the eighteen classes
#' hold, in the order of the class ladder, so the bar of a complete MM day is the 1440
#' minutes of that day. It is the accounting of \code{\link{plot_raw_timeuse}} with the time
#' of day taken out.
#'
#' @section Warning:
#' The four \code{dur_day_total_*} columns and the three summary durations are not drawn in
#' this stack. They do not nest inside the classes, and stacking both would draw a day of
#' more than twenty-four hours. The dash above each bar is \code{dur_day_spt_min}, the length
#' of the window.
#'
#' @details No GGIR original. The columns are read from the day table by name as
#'   \code{paste0("dur_", x$levels, "_min")}, so a run with a different bout-duration set, or
#'   with the nap class, draws its own classes.
#'
#' @param x A \code{canhrActi_raw_timeuse} from \code{\link{raw.timeuse}}.
#' @param timewindow NULL for every timewindow the day table holds, each in its own panel, or
#'   "MM", "WW" or "OO".
#' @param threshold NULL for the first threshold set, or its name.
#' @param sibdef NULL for the first sleep definition, or its name.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for a corrupt, too short or windowless
#'   object.
#' @examples
#' \dontrun{
#' plot_raw_timeuse_day(tu)
#' }
#' @seealso \code{\link{plot_raw_timeuse}}, \code{\link{raw.timeuse}}
#' @export plot_raw_timeuse_day
plot_raw_timeuse_day <- function(x, timewindow = NULL, threshold = NULL, sibdef = NULL, ...) {
  title <- "Minutes in each behaviour class, per window"
  p <- .raw.timeuse.plot.parts(x, threshold, sibdef, timewindow, need = "daysummary")
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  ds <- p$daysummary
  Lnames <- p$levels
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  keep <- rep(TRUE, nrow(ds))
  if (!is.na(p$triple) && all(c("TRLi", "TRMi", "TRVi") %in% names(ds))) {
    keep <- keep & paste(ds$TRLi, ds$TRMi, ds$TRVi, sep = "_") == p$triple
  }
  if (!is.na(p$sibdef) && "sleepparam" %in% names(ds)) {
    keep <- keep & as.character(ds$sleepparam) == p$sibdef
  }
  if (!is.null(timewindow) && "window" %in% names(ds)) {
    keep <- keep & as.character(ds$window) %in% as.character(timewindow)
  }
  ds <- ds[keep, , drop = FALSE]
  cols <- paste0("dur_", Lnames, "_min")
  have <- cols %in% names(ds)
  if (nrow(ds) == 0 || !any(have)) {
    return(.raw.plot.empty(paste0("The part-5 day table holds no window for this ",
                                  "configuration, so there is nothing to draw."), title))
  }
  long <- do.call(rbind, lapply(seq_len(nrow(ds)), function(i) {
    data.frame(row = i, class = Lnames[have],
               minutes = num(unlist(ds[i, cols[have]], use.names = FALSE)),
               stringsAsFactors = FALSE)
  }))
  lab <- paste0(substr(as.character(ds$weekday), 1, 3), "\n",
                .format_english(as.Date(as.character(ds$calendar_date)), "%d %b"))
  if ("window_number" %in% names(ds)) {
    key <- paste(ds$window, lab)
    dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
    if (any(dup)) lab[dup] <- paste0(lab[dup], "\n#", ds$window_number[dup])
  }
  long$label <- factor(lab[long$row], levels = unique(lab))
  long$panel <- factor(as.character(ds$window)[long$row],
                       levels = unique(as.character(ds$window)))
  long$class <- factor(long$class, levels = Lnames)
  long$minutes[is.na(long$minutes)] <- 0
  total <- data.frame(label = factor(lab, levels = unique(lab)),
                      panel = factor(as.character(ds$window),
                                     levels = unique(as.character(ds$window))),
                      window_len = num(ds$dur_day_spt_min), stringsAsFactors = FALSE)
  stacked <- stats::aggregate(list(minutes = long$minutes), list(row = long$row), sum)$minutes
  gap <- suppressWarnings(max(abs(stacked - total$window_len), na.rm = TRUE))
  subtitle <- paste0(nrow(ds), " window", if (nrow(ds) == 1) "" else "s", ", ",
                     sum(have), " classes; the stack is the whole window (largest difference ",
                     "from dur_day_spt_min ", signif(gap, 3), " min)")
  g <- ggplot2::ggplot(long, ggplot2::aes(x = .data$label, y = .data$minutes,
                                          fill = .data$class)) +
    ggplot2::geom_col(width = 0.72, colour = "white", linewidth = 0.15) +
    ggplot2::geom_point(data = total, ggplot2::aes(x = .data$label, y = .data$window_len),
                        inherit.aes = FALSE, shape = 95, size = 5,
                        colour = .raw.timeuse.plot.colours()[["fit"]]) +
    ggplot2::scale_fill_manual(values = p$colours, name = NULL, drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1440, by = 240),
                                expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle), x = NULL, y = "Minutes",
                  caption = .raw.timeuse.plot.caption(
                    p, paste0("Each bar is one row of the part-5 day table, stacked over the ",
                              "dur_*_min columns of the ", sum(have), " classes; the dash is ",
                              "dur_day_spt_min. The dur_day_total_* columns are not drawn, ",
                              "because they overlap the classes rather than nesting inside ",
                              "them"))) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    .raw.plot.theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(0.9, "lines"))
  if (nlevels(long$panel) > 1) {
    # facet_wrap with free_x drops a date one timewindow has and the other does not;
    # facet_grid would leave an empty slot
    g <- g + ggplot2::facet_wrap(ggplot2::vars(.data$panel), ncol = 1, scales = "free_x")
  }
  g
}

# BOUT STRUCTURE

#' How Long the Blocks of Each Behaviour Class Are
#'
#' One point per block, where a block is a maximal run of one behaviour class inside an
#' analysed window, on a logarithmic minute axis with a box over each class. A bouted class
#' should sit at or above its bout length, the dashed mark beside it; a shorter block is a
#' bout cut by the end of the window or split by an epoch the bout criterion allowed inside
#' it.
#'
#' @details No GGIR original. The blocks are the ones GGIR counts in \code{Nblocks_<class>},
#'   \code{length(which(rle(LEVELS[sse])$values == code))}. \code{Nbouts_<class>} is a
#'   different count, taken from the bout indicator matrices, and is not what is drawn.
#'
#' @param x A \code{canhrActi_raw_timeuse} from \code{\link{raw.timeuse}}.
#' @param timewindow NULL for the first timewindow the object carries, or "MM", "WW" or "OO".
#' @param threshold NULL for the first threshold set, or its name.
#' @param sibdef NULL for the first sleep definition, or its name.
#' @param classes NULL for every class with at least one block, or the class names to draw.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for a corrupt, too short or windowless
#'   object.
#' @examples
#' \dontrun{
#' plot_raw_timeuse_bouts(tu)
#' }
#' @seealso \code{\link{plot_raw_timeuse}}, \code{\link{plot_raw_timeuse_day}}
#' @export plot_raw_timeuse_bouts
plot_raw_timeuse_bouts <- function(x, timewindow = NULL, threshold = NULL, sibdef = NULL,
                                   classes = NULL, ...) {
  title <- "Block length by behaviour class"
  p <- .raw.timeuse.plot.parts(x, threshold, sibdef, timewindow, need = "series")
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  m <- p$mdat
  Lnames <- p$levels
  ws3 <- p$ws3
  wnums <- sort(unique(m$window[m$window > 0]))
  if (length(wnums) == 0) {
    return(.raw.plot.empty(paste0("No epoch of this recording belongs to a ", p$timewindow,
                                  " window, so there are no blocks to draw."), title))
  }
  # one run-length encoding per window, so a block never spans two windows
  blocks <- do.call(rbind, lapply(wnums, function(w) {
    j <- which(m$window == w)
    rl <- rle(as.integer(m$class_id[j]))
    ends <- cumsum(rl$lengths)
    starts <- ends - rl$lengths + 1L
    data.frame(window_number = w, class = Lnames[rl$values + 1L],
               minutes = rl$lengths * ws3 / 60, first_epoch = j[starts],
               stringsAsFactors = FALSE)
  }))
  blocks <- blocks[!is.na(blocks$class), , drop = FALSE]
  if (!is.null(classes)) blocks <- blocks[blocks$class %in% classes, , drop = FALSE]
  if (nrow(blocks) == 0) {
    return(.raw.plot.empty("No block of any behaviour class was found in this recording.",
                           title))
  }
  parts <- .raw.timeuse.plot.classparts(Lnames)
  present <- Lnames[Lnames %in% unique(blocks$class)]
  n_by <- table(factor(blocks$class, levels = present))
  labels <- stats::setNames(paste0(present, "  (n = ", as.integer(n_by[present]), ")"), present)
  blocks$class <- factor(blocks$class, levels = rev(present))
  bands <- parts[parts$name %in% present & !is.na(parts$bout_min), , drop = FALSE]
  bands$class <- factor(bands$name, levels = rev(present))
  med <- stats::aggregate(list(minutes = blocks$minutes), list(class = blocks$class),
                          stats::median)
  longest <- which.max(blocks$minutes)
  subtitle <- paste0(nrow(blocks), " blocks in ", length(wnums), " ", p$timewindow, " window",
                     if (length(wnums) == 1) "" else "s", ", ", length(present), " of ",
                     length(Lnames), " classes present; longest block ",
                     round(blocks$minutes[longest], 1), " min in ",
                     as.character(blocks$class[longest]))
  g <- ggplot2::ggplot(blocks, ggplot2::aes(x = .data$minutes, y = .data$class)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = .data$class), outlier.shape = NA, width = 0.6,
                          linewidth = 0.3, colour = "grey30", alpha = 0.85) +
    ggplot2::geom_jitter(height = 0.18, width = 0, size = 0.7, alpha = 0.35,
                         colour = "#111111")
  if (nrow(bands) > 0) {
    g <- g + ggplot2::geom_segment(data = bands,
                                   ggplot2::aes(x = .data$bout_min, xend = .data$bout_min,
                                                y = as.numeric(.data$class) - 0.42,
                                                yend = as.numeric(.data$class) + 0.42),
                                   inherit.aes = FALSE, linetype = "dashed",
                                   colour = .raw.timeuse.plot.colours()[["fit"]],
                                   linewidth = 0.4)
  }
  g <- g +
    ggplot2::geom_point(data = med, ggplot2::aes(x = .data$minutes, y = .data$class),
                        inherit.aes = FALSE, shape = 124, size = 3.2, colour = "#111111") +
    ggplot2::scale_fill_manual(values = p$colours, guide = "none") +
    ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::scale_x_log10(breaks = c(0.1, 1, 10, 60, 240, 1440),
                           labels = c("0.1", "1", "10", "60", "240", "1440")) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle),
                  x = "Block length (minutes, log scale)", y = NULL,
                  caption = .raw.timeuse.plot.caption(
                    p, paste0("One point per block, a maximal run of one class inside a ",
                              p$timewindow, " window; the count beside each class is the day ",
                              "table's Nblocks column. Dashed: the lower edge of a bouted ",
                              "band. The bar is the median"))) +
    .raw.plot.theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  g
}

# INTENSITY GRADIENT

#' The Intensity Gradient of Each Window, With Its Fit
#'
#' Minutes spent in each acceleration bin against the middle of the bin, both on
#' logarithmic axes, one colour per window, with the straight line part 5 fits through them.
#' The slope of that line is \code{ig_day_gradient} in the day table: a steeper (more
#' negative) slope is a day spent almost entirely at low intensity.
#'
#' @details The bin edges are \code{iglevels}, expanded to \code{c(seq(0, 4000, by = 25),
#'   8000)} when it is a single value, as GGIR does; the counts are
#'   \code{cut(ACC, breaks = iglevels, right = FALSE)} over the waking epochs for
#'   \code{period = "day"} and over every epoch for \code{period = "day_spt"}; the line is
#'   \code{.raw.intensity.gradient}, an ordinary least squares fit of log minutes on
#'   log bin middle with the empty bins dropped. When the day table carries
#'   \code{ig_day_gradient} the drawn line is that column; otherwise the fit is computed here
#'   and the caption says so.
#'
#' @param x A \code{canhrActi_raw_timeuse} from \code{\link{raw.timeuse}}.
#' @param period "day" for the waking hours, GGIR's \code{ig_day_*} triple, or "day_spt" for
#'   the whole window including the sleep period.
#' @param timewindow NULL for the first timewindow the object carries, or "MM", "WW" or "OO".
#' @param threshold NULL for the first threshold set, or its name.
#' @param sibdef NULL for the first sleep definition, or its name.
#' @param iglevels NULL for the bin edges the object was built with, a vector of edges, or a
#'   single value for GGIR's expansion.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for a corrupt, too short or windowless
#'   object.
#' @examples
#' \dontrun{
#' plot_raw_timeuse_gradient(raw.timeuse(x, nights, params = raw.params(iglevels = 1)))
#' }
#' @seealso \code{\link{plot_raw_timeuse_day}}, \code{\link{raw.timeuse}}
#' @export plot_raw_timeuse_gradient
plot_raw_timeuse_gradient <- function(x, period = c("day", "day_spt"), timewindow = NULL,
                                      threshold = NULL, sibdef = NULL, iglevels = NULL, ...) {
  period <- match.arg(period)
  title <- "Intensity gradient"
  p <- .raw.timeuse.plot.parts(x, threshold, sibdef, timewindow, need = "series")
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  m <- p$mdat
  ws3 <- p$ws3
  if (is.null(iglevels)) iglevels <- p$settings$iglevels
  from_settings <- TRUE
  if (length(iglevels) <= 1) {
    # a single value is a switch, not a level
    iglevels <- c(seq(0, 4000, by = 25), 8000)
    from_settings <- FALSE
  }
  wnums <- sort(unique(m$window[m$window > 0]))
  if (length(wnums) == 0) {
    return(.raw.plot.empty(paste0("No epoch of this recording belongs to a ", p$timewindow,
                                  " window, so no intensity gradient can be drawn."), title))
  }
  ds <- p$daysummary
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  gcol <- paste0("ig_", period, "_gradient")
  icol <- paste0("ig_", period, "_intercept")
  rcol <- paste0("ig_", period, "_rsquared")
  stored <- is.data.frame(ds) && all(c(gcol, icol) %in% names(ds))
  x_ig <- zoo::rollmean(iglevels, k = 2)
  pts <- list(); fits <- list()
  for (w in wnums) {
    j <- if (period == "day") which(m$window == w & m$SleepPeriodTime == 0) else {
      which(m$window == w)
    }
    if (length(j) == 0) next
    q55 <- cut(m$ACC[j], breaks = iglevels, right = FALSE)
    y_ig <- (as.numeric(table(q55)) * ws3) / 60
    ig <- as.numeric(unlist(.raw.intensity.gradient(x_ig, y_ig)))
    lab <- NA_character_
    if (is.data.frame(ds) && all(c("window_number", "window") %in% names(ds))) {
      k <- which(as.character(ds$window) == p$timewindow & num(ds$window_number) == w)
      if (length(k) == 1) {
        lab <- paste0(substr(as.character(ds$weekday[k]), 1, 3), " ",
                      .format_english(as.Date(as.character(ds$calendar_date[k])), "%d %b"))
        if (stored) {
          ig[1] <- num(ds[[gcol]][k])
          ig[2] <- num(ds[[icol]][k])
          if (rcol %in% names(ds)) ig[3] <- num(ds[[rcol]][k])
        }
      }
    }
    if (is.na(lab)) lab <- paste0(p$timewindow, " window ", w)
    pts[[length(pts) + 1]] <- data.frame(window = lab, x = x_ig, y = y_ig,
                                         stringsAsFactors = FALSE)
    fits[[length(fits) + 1]] <- data.frame(window = lab, gradient = ig[1], intercept = ig[2],
                                           rsquared = ig[3], stringsAsFactors = FALSE)
  }
  pts <- do.call(rbind, pts)
  fits <- do.call(rbind, fits)
  if (is.null(pts) || nrow(pts) == 0) {
    return(.raw.plot.empty("No window of this recording holds an epoch to bin.", title))
  }
  pts <- pts[pts$y > 0, , drop = FALSE]
  if (nrow(pts) == 0) {
    return(.raw.plot.empty(paste0("Every acceleration bin of this recording is empty, so no ",
                                  "gradient can be fitted."), title))
  }
  levs <- unique(c(pts$window, fits$window))
  pts$window <- factor(pts$window, levels = levs)
  fits$window <- factor(fits$window, levels = levs)
  fits$label <- paste0(fits$window, " ", round(fits$gradient, 3))
  xr <- range(pts$x)
  lines <- do.call(rbind, lapply(seq_len(nrow(fits)), function(i) {
    if (is.na(fits$gradient[i])) return(NULL)
    xs <- exp(seq(log(xr[1]), log(xr[2]), length.out = 64))
    data.frame(window = fits$window[i], x = xs,
               y = exp(fits$intercept[i] + fits$gradient[i] * log(xs)),
               stringsAsFactors = FALSE)
  }))
  rs <- fits$rsquared[!is.na(fits$rsquared)]
  subtitle <- paste0(length(levs), " ", p$timewindow, " window",
                     if (length(levs) == 1) "" else "s", ", ", length(iglevels) - 1,
                     " bins from ",
                     if (from_settings) "the levels the object was built with" else
                       "GGIR's default expansion c(seq(0, 4000, by = 25), 8000)",
                     "; gradient ", paste(round(fits$gradient, 3), collapse = ", "),
                     if (length(rs) > 0) {
                       paste0("; r squared ", round(min(rs), 3),
                              if (length(rs) > 1) paste0(" to ", round(max(rs), 3)) else "")
                     } else "")
  g <- ggplot2::ggplot(pts, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$window)) +
    ggplot2::geom_point(size = 0.9, alpha = 0.55)
  if (!is.null(lines) && nrow(lines) > 0) {
    g <- g + ggplot2::geom_line(data = lines,
                                ggplot2::aes(x = .data$x, y = .data$y, colour = .data$window),
                                linewidth = 0.7)
  }
  g <- g +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_colour_manual(values = canhrActi_colors(length(levs)), name = NULL,
                                 labels = stats::setNames(fits$label,
                                                          as.character(fits$window))) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(subtitle),
                  x = "Middle of the acceleration bin (mg, log scale)",
                  y = "Minutes in the bin (log scale)",
                  caption = .raw.timeuse.plot.caption(
                    p, paste0("Points: minutes per acceleration bin over the ",
                              if (period == "day") "waking hours" else "whole window",
                              "; lines: the ordinary least squares fit of log minutes on log ",
                              "bin middle, ",
                              if (stored) paste0("the ig_", period, "_gradient column of the ",
                                                 "day table") else
                                paste0("recomputed here because ig_", period, "_gradient is ",
                                       "not in the day table (iglevels was not set)")))) +
    .raw.plot.theme()
  g
}
