# Ported from GGIR 3.3-9 R/g.sib.plot.R and the visualisation_sleep block of
# R/g.part4.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. What is drawn is GGIR's: the
# noon-to-noon night windows with the z angle over a scaled acceleration trace and one
# bar per sustained inactivity bout, and the one-row-per-night layout on the 12..36 axis.
# The drawing is ggplot2 on theme_canhrActi(), the colours are canhrActi's, and every
# function returns an empty-state ggplot.

# SHARED HELPERS

#' Colours of the Sleep Figures
#'
#' Taken from \code{canhrActi_palette("sleep")} and \code{canhrActi_palette("status")}; the
#' fallbacks are the same hex values.
#'
#' @return A named character vector of hex colours.
#' @keywords internal
#' @noRd
.raw.sleep.plot.colours <- function() {
  sleep <- c(wake = "#FFCD00", sleep = "#236192", bedtime = "#71984A",
             waketime = "#DF6A2E", period = "#87D1E6")
  sl <- tryCatch(canhrActi_palette("sleep"), error = function(e) NULL)
  if (!is.null(sl)) for (nm in names(sleep)) if (!is.null(sl[[nm]])) sleep[[nm]] <- sl[[nm]]
  base <- .raw.plot.colours()
  c(inside = unname(sleep[["sleep"]]),      # sib in the SPT
    outside = "#9DBF6A",                    # daytime sib
    guider = "#374151",                     # GGIR's hatched rectangle
    onset = unname(sleep[["bedtime"]]),
    wake = unname(sleep[["waketime"]]),
    period = unname(sleep[["period"]]),
    angle = "#111111",
    acc = "grey55",
    invalid = unname(base[["invalid"]]),
    reference = unname(base[["reference"]]))
}

#' Midnight That Opens a Calendar Date, in the Zone the Epoch Tables Carry
#'
#' @param date "YYYY-MM-DD".
#' @param tz The zone.
#' @return POSIXct, or NA.
#' @keywords internal
#' @noRd
.raw.sleep.plot.midnight <- function(date, tz) {
  if (length(date) == 0 || is.na(date) || !nzchar(date)) return(as.POSIXct(NA))
  as.POSIXct(paste0(date, " 00:00:00"), tz = tz)
}

#' GGIR's 12..36 Night Axis, as Hours After the Midnight That Opens the Night's Date
#'
#' Part 4 puts every sleep quantity on an axis whose origin is the midnight opening the
#' night's calendar date, so a noon-to-noon window runs 12 to 36. This helper takes the
#' elapsed difference from that midnight, which equals GGIR's clock-field arithmetic on an
#' ordinary day, is monotone across the noon boundary and shows a clock-change day as the
#' 23 or 25 hours it lasted.
#'
#' @param t POSIXct, or an ISO 8601 character vector.
#' @param midnight POSIXct midnight of the night's date.
#' @param tz The zone, used when \code{t} is character.
#' @return Numeric hours.
#' @keywords internal
#' @noRd
.raw.sleep.plot.hours <- function(t, midnight, tz = "") {
  if (is.character(t)) t <- .raw.iso8601.to.posix(t, tz = tz)
  if (length(midnight) == 0 || is.na(midnight)) return(rep(NA_real_, length(t)))
  as.numeric(difftime(t, midnight, units = "hours"))
}

#' One Bar per Sustained Inactivity Bout, From sib.cla.sum
#'
#' The bars come from \code{sib.cla.sum}, so the drawn positions are the numbers in the
#' table: \code{xmin} is \code{sib.onset.time} on the night's 12..36 axis and \code{xmax} is
#' \code{sib.end.time} plus one epoch, because the stored end time is the start of the last
#' epoch of the bout.
#'
#' @param sib.cla.sum The 9-column table of \code{\link{raw.sib.summary}}.
#' @param nights The per-night table of \code{\link{raw.sleep.part3}} (\code{$nights}),
#'   which carries the date that opens each night's axis.
#' @param tz The zone the timestamps carry.
#' @param ws3 The short epoch length in seconds.
#' @return data.frame(night, definition, period, xmin, xmax, hours), zero rows when there is
#'   nothing to draw.
#' @keywords internal
#' @noRd
.raw.sleep.plot.sibbars <- function(sib.cla.sum, nights, tz, ws3) {
  empty <- data.frame(night = numeric(0), definition = character(0), period = numeric(0),
                      xmin = numeric(0), xmax = numeric(0), hours = numeric(0),
                      stringsAsFactors = FALSE)
  if (!is.data.frame(sib.cla.sum) || nrow(sib.cla.sum) == 0) return(empty)
  if (!is.data.frame(nights) || nrow(nights) == 0) return(empty)
  out <- vector("list", nrow(nights))
  for (q in seq_len(nrow(nights))) {
    k <- nights$night[q]
    sub <- sib.cla.sum[which(as.numeric(sib.cla.sum$night) == k), , drop = FALSE]
    if (nrow(sub) == 0) next
    mid <- .raw.sleep.plot.midnight(nights$date[q], tz)
    if (is.na(mid)) next
    out[[q]] <- data.frame(
      night = as.numeric(sub$night),
      definition = as.character(sub$definition),
      period = as.numeric(sub$sib.period),
      xmin = .raw.sleep.plot.hours(as.character(sub$sib.onset.time), mid, tz),
      xmax = .raw.sleep.plot.hours(as.character(sub$sib.end.time), mid, tz) + ws3 / 3600,
      hours = as.numeric(sub$tot.sib.dur.hrs),
      stringsAsFactors = FALSE)
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(empty)
  bars <- do.call(rbind, out)
  row.names(bars) <- NULL
  bars
}

#' Take the Pieces the Sleep Figures Need Out of Whatever the Caller Passed
#'
#' Accepts a \code{canhrActi_raw} (uses \code{$sleep} and \code{$imputed}), a
#' \code{canhrActi_raw_sleep} on its own, or a \code{canhrActi_raw_nights} (which carries the
#' part-4 episodes and guider windows but no series). Returns the parts found and a state:
#' "ok", or the sentence the empty-state plot should carry.
#'
#' @param x The object.
#' @param nights NULL, or a \code{canhrActi_raw_nights} to draw the part-4 quantities from.
#' @return list(sleep, nights, ns_attr, metashort, tz, filename, id, ws3, state).
#' @keywords internal
#' @noRd
.raw.sleep.plot.parts <- function(x, nights = NULL) {
  sleep <- NULL; ns <- NULL; metashort <- NULL; tz <- ""
  filename <- NA_character_; id <- NA_character_; ws3 <- NA_real_
  if (inherits(x, "canhrActi_raw_nights")) {
    ns <- x
    x <- NULL
  }
  if (inherits(nights, "canhrActi_raw_nights")) ns <- nights
  if (inherits(x, "canhrActi_raw")) {
    sleep <- x$sleep
    imp <- x$imputed
    if (!is.null(imp) && !is.null(imp$metashort)) {
      metashort <- imp$metashort
    } else if (!is.null(x$meta) && !is.null(x$meta$metashort)) {
      # the part-1 series is not what the detector saw; the caption says so
      metashort <- x$meta$metashort
      attr(metashort, "canhrActi_part1") <- TRUE
    }
    if (!is.null(x$meta)) ws3 <- x$meta$windowsizes[1]
    filename <- x$file$filename
    if (!is.null(x$tz$effective_tz)) tz <- x$tz$effective_tz
  } else if (inherits(x, "canhrActi_raw_sleep")) {
    sleep <- x
  } else if (inherits(x, "canhrActi_raw_imputed")) {
    metashort <- x$metashort
  }
  if (!is.null(sleep)) {
    if (is.na(filename) && !is.null(sleep$file$filename)) filename <- sleep$file$filename
    if (!is.null(sleep$id) && length(sleep$id) == 1 && !is.na(sleep$id)) id <- as.character(sleep$id)
    if (is.na(ws3) && !is.null(sleep$settings$windowsizes)) ws3 <- sleep$settings$windowsizes[1]
    dtz <- sleep$desiredtz_part1
    if (!is.null(dtz) && is.character(dtz) && length(dtz) == 1 && nzchar(dtz)) tz <- dtz
    if (!nzchar(tz) && !is.null(sleep$settings$desiredtz) && nzchar(sleep$settings$desiredtz)) {
      tz <- sleep$settings$desiredtz
    }
  }
  nsa <- if (!is.null(ns)) attr(ns, "canhrActi") else NULL
  if (!is.null(nsa)) {
    if (is.na(filename) && !is.null(nsa$filename)) filename <- nsa$filename
    if (is.na(id) && !is.null(nsa$id)) id <- nsa$id
    if (!nzchar(tz) && !is.null(nsa$desiredtz) && nzchar(nsa$desiredtz)) tz <- nsa$desiredtz
  }
  if (is.na(ws3)) ws3 <- 5
  state <- "ok"
  if (is.null(sleep) && is.null(ns)) {
    state <- paste0("No sleep results were found on this object. Run raw.sleep.part3() on a ",
                    "recording, and raw.sleep.nights() on its result, and pass one of those.")
  } else if (!is.null(sleep) && !identical(sleep$status$state, "ok")) {
    st <- sleep$status$state
    if (length(st) != 1 || is.na(st)) st <- "unknown"
    state <- switch(
      as.character(st),
      skipped = "The recording is corrupt, too short or was skipped, so part 3 estimated nothing.",
      too_short_for_sleep = paste0("The recording holds less than a fifth of a day of valid ",
                                   "data, so the sustained inactivity detection did not run."),
      detection_failed = "The sustained inactivity detection failed, so there is nothing to draw.",
      paste0("Part 3 produced no sleep estimates for this recording (state \"",
             as.character(st), "\")."))
  }
  list(sleep = sleep, nights = ns, ns_attr = nsa, metashort = metashort, tz = tz,
       filename = filename, id = id, ws3 = ws3, state = state)
}

#' The Caption Line Every Sleep Figure Carries
#' @keywords internal
#' @noRd
.raw.sleep.plot.caption <- function(p, extra = NULL) {
  fname <- p$filename
  if (!is.na(fname)) fname <- sub("[.]RData$", "", fname)
  bits <- c(if (!is.na(fname)) fname else NULL,
            if (!is.na(p$id) && !identical(p$id, fname)) paste0("ID ", p$id) else NULL,
            extra,
            paste0("Times in ", if (nzchar(p$tz)) p$tz else Sys.timezone(), "."))
  .raw.plot.wrap(paste(bits, collapse = ". "))
}

# PER-NIGHT VIEW

#' Sustained Inactivity Bouts and the Guider, Night by Night
#'
#' One panel per noon-to-noon night: the z angle of the imputed series over a scaled
#' acceleration trace, the sustained inactivity bouts as a bar under it, the guider window
#' as a shaded band, the epochs the part-2 decision marked invalid shaded grey, and, when a
#' night table is supplied, the sleep onset and wake-up the night summary settled on.
#'
#' Reading it: a good night is one band of bouts filling the guider window with few gaps,
#' with the angle trace flat inside it and busy outside. A night whose bouts sit well away
#' from the guider band, or whose guider band covers a busy angle trace, is the one to look
#' at. Grey shading is time the part-2 wear decision ignored, so bouts stop at its edges
#' when \code{ignorenonwear} was TRUE.
#'
#' @details GGIR's g.sib.plot redrawn in ggplot2. Kept: the night windows, the z angle with
#'   GGIR's single-trailing-NA repair, the acceleration metric scaled under the angle, and
#'   one line per sib definition. Changed: the axis is the 12..36 night axis of part 4
#'   rather than clock time, so the bars and the guider band sit at the numbers the tables
#'   report; the colours are canhrActi's; the light and temperature traces are dropped in
#'   favour of the invalid-epoch shading. The series drawn is the part-2 imputed one, which
#'   is what the detector saw; without it the part-1 series is drawn and the caption says so.
#'
#' @param x A \code{canhrActi_raw} carrying \code{$sleep} (and ideally \code{$imputed}), or a
#'   \code{canhrActi_raw_sleep} from \code{\link{raw.sleep.part3}}.
#' @param nights NULL, or the \code{canhrActi_raw_nights} of
#'   \code{\link{raw.sleep.nights}}; when given, the sleep onset and wake-up markers and the
#'   part-4 guider window are drawn instead of the part-3 SPT estimate.
#' @param night NULL for every night, or the night numbers to draw.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot for corrupt, too-short or failed input.
#' @examples
#' \dontrun{
#' plot_raw_sleep(x)
#' plot_raw_sleep(x, nights = raw.sleep.nights(x$sleep), night = 2:3)
#' }
#' @seealso \code{\link{plot_raw_sleep_nights}}, \code{\link{plot_raw_sleep_regularity}}
#' @export plot_raw_sleep
plot_raw_sleep <- function(x, nights = NULL, night = NULL, ...) {
  # C time locale for the call, so the facet strips and weekday labels are English
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  p <- .raw.sleep.plot.parts(x, nights)
  title <- "Sustained inactivity bouts and the guider, night by night"
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  sleep <- p$sleep
  if (is.null(sleep)) {
    return(.raw.plot.empty(paste0("This is a night table, which carries no angle series. Pass the ",
                                  "recording (or its part-3 result) as x, and the night table as ",
                                  "nights."), title))
  }
  nt <- sleep$nights
  if (!is.data.frame(nt) || nrow(nt) == 0) {
    return(.raw.plot.empty("Part 3 found no noon-to-noon night windows in this recording.", title))
  }
  if (!is.null(night)) {
    nt <- nt[nt$night %in% night, , drop = FALSE]
    if (nrow(nt) == 0) {
      return(.raw.plot.empty(paste0("This recording has no night ",
                                    paste(night, collapse = ", "), "."), title))
    }
  }
  cols <- .raw.sleep.plot.colours()
  ws3 <- p$ws3
  tz <- p$tz
  epochs <- sleep$epochs
  metashort <- p$metashort
  part1 <- isTRUE(attr(metashort, "canhrActi_part1"))
  has_angle <- !is.null(metashort) && "anglez" %in% names(metashort) &&
    !is.null(epochs) && nrow(metashort) == nrow(epochs)
  metricName <- NA_character_
  if (has_angle) {
    angle <- as.numeric(metashort$anglez)
    # GGIR's single trailing NA repair
    if (length(which(is.na(angle) == TRUE)) > 0) {
      if (which(is.na(angle) == TRUE)[1] == length(angle)) {
        angle[length(angle)] <- angle[length(angle) - 1]
      }
    }
    metricName <- .raw.plot.metric(metashort, NULL)
    ENMO <- as.numeric(metashort[[metricName]]) * 1000
  }

  # panel geometry: angle band on top, acceleration under it, sib bar under that
  ang_lo <- -90; ang_hi <- 90
  acc_lo <- -230; acc_hi <- -140
  sib_lo <- -300; sib_hi <- -255
  ymin <- -315; ymax <- 105

  bars <- .raw.sleep.plot.sibbars(sleep$sib.cla.sum, nt, tz, ws3)
  defs <- sort(unique(bars$definition))
  ndef <- max(1L, length(defs))
  series <- list(); shade <- list(); guide <- list(); marks <- list()
  labels <- character(nrow(nt))
  gtab <- if (!is.null(p$ns_attr)) p$ns_attr$guiders else NULL
  for (q in seq_len(nrow(nt))) {
    k <- nt$night[q]
    mid <- .raw.sleep.plot.midnight(nt$date[q], tz)
    lab <- paste0("Night ", k, ": ", if (is.na(mid)) "date unknown" else .format_english(mid, "%a %d %b %Y"))
    labels[q] <- lab
    idx <- if (!is.null(epochs)) which(epochs$night == k) else integer(0)
    if (length(idx) > 0 && !is.na(mid)) {
      h <- .raw.sleep.plot.hours(as.character(epochs$time[idx]), mid, tz)
      if (has_angle) {
        series[[length(series) + 1]] <- data.frame(
          night = lab, h = h, angle = angle[idx],
          acc = acc_lo + pmin(ENMO[idx], 500) / 500 * (acc_hi - acc_lo),
          stringsAsFactors = FALSE)
      }
      inv <- .raw.plot.runs(epochs$invalid[idx])
      if (nrow(inv) > 0) {
        shade[[length(shade) + 1]] <- data.frame(
          night = lab, xmin = h[inv$start_epoch], xmax = h[inv$end_epoch] + ws3 / 3600,
          stringsAsFactors = FALSE)
      }
    }
    # the guider window: part 4's when a night table is given, the part-3 SPT estimate otherwise
    g0 <- NA_real_; g1 <- NA_real_; gname <- NA_character_
    if (!is.null(gtab) && k %in% gtab$night) {
      gi <- which(gtab$night == k)[1]
      g0 <- gtab$guider_onset[gi]; g1 <- gtab$guider_wakeup[gi]
      gname <- gtab$guider[gi]
    } else {
      g0 <- nt$SPTE_start[q]; g1 <- nt$SPTE_end[q]
      gname <- nt$guider[q]
    }
    if (!is.na(g0) && !is.na(g1)) {
      guide[[length(guide) + 1]] <- data.frame(night = lab, xmin = g0, xmax = g1,
                                               guider = as.character(gname),
                                               stringsAsFactors = FALSE)
    }
    if (!is.null(p$nights) && "night" %in% names(p$nights)) {
      ni <- which(as.numeric(p$nights$night) == k)
      if (length(ni) > 0) {
        ni <- ni[1]
        marks[[length(marks) + 1]] <- data.frame(
          night = lab,
          h = c(as.numeric(p$nights$sleeponset[ni]), as.numeric(p$nights$wakeup[ni])),
          what = c("Sleep onset", "Wake-up"), stringsAsFactors = FALSE)
      }
    }
  }
  bars$night_label <- labels[match(bars$night, nt$night)]
  bars <- bars[!is.na(bars$night_label), , drop = FALSE]
  if (nrow(bars) > 0) {
    di <- match(bars$definition, defs)
    # one lane per definition inside the sib band, as g.sib.plot stacks its lines
    bars$ymin <- sib_lo + (di - 1) / ndef * (sib_hi - sib_lo)
    bars$ymax <- sib_lo + di / ndef * (sib_hi - sib_lo)
  }
  series <- if (length(series) > 0) do.call(rbind, series) else NULL
  shade <- if (length(shade) > 0) do.call(rbind, shade) else NULL
  guide <- if (length(guide) > 0) do.call(rbind, guide) else NULL
  marks <- if (length(marks) > 0) do.call(rbind, marks) else NULL
  marks <- if (!is.null(marks)) marks[!is.na(marks$h), , drop = FALSE] else NULL
  if (!is.null(marks) && nrow(marks) == 0) marks <- NULL
  fct <- function(d) {
    if (is.null(d) || nrow(d) == 0) return(d)
    d$night <- factor(d$night, levels = labels)
    d
  }
  series <- fct(series); shade <- fct(shade); guide <- fct(guide); marks <- fct(marks)
  if (nrow(bars) > 0) bars$night <- factor(bars$night_label, levels = labels)

  xlo <- min(c(12, bars$xmin, guide$xmin, series$h), na.rm = TRUE)
  xhi <- max(c(36, bars$xmax, guide$xmax, series$h), na.rm = TRUE)
  brk <- seq(12, 36, by = 3)
  brk <- brk[brk >= floor(xlo) & brk <= ceiling(xhi)]

  g <- ggplot2::ggplot()
  if (!is.null(shade) && nrow(shade) > 0) {
    g <- g + ggplot2::geom_rect(data = shade,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = ymin, ymax = ymax),
                                fill = cols[["invalid"]], colour = NA, alpha = 0.7)
  }
  if (!is.null(guide) && nrow(guide) > 0) {
    g <- g +
      ggplot2::geom_rect(data = guide,
                         ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                      ymin = ymin, ymax = ymax),
                         fill = cols[["period"]], colour = NA, alpha = 0.35) +
      ggplot2::geom_segment(data = guide,
                            ggplot2::aes(x = .data$xmin, xend = .data$xmax,
                                         y = ymax - 6, yend = ymax - 6),
                            colour = cols[["guider"]], linewidth = 0.8)
  }
  g <- g +
    ggplot2::geom_vline(xintercept = c(18, 24, 30), colour = "grey70", linewidth = 0.3,
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = c(15, 21, 27, 33), colour = "grey85", linewidth = 0.3,
                        linetype = "dotted")
  if (!is.null(series) && nrow(series) > 0) {
    g <- g +
      ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3, linetype = "dotted") +
      ggplot2::geom_line(data = series, ggplot2::aes(x = .data$h, y = .data$acc),
                         colour = cols[["acc"]], linewidth = 0.2, na.rm = TRUE) +
      ggplot2::geom_line(data = series, ggplot2::aes(x = .data$h, y = .data$angle),
                         colour = cols[["angle"]], linewidth = 0.25, na.rm = TRUE)
  }
  if (nrow(bars) > 0) {
    g <- g + ggplot2::geom_rect(data = bars,
                                ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                             ymin = .data$ymin, ymax = .data$ymax,
                                             fill = .data$definition),
                                colour = NA)
  }
  if (!is.null(marks) && nrow(marks) > 0) {
    g <- g + ggplot2::geom_vline(data = marks,
                                 ggplot2::aes(xintercept = .data$h, colour = .data$what),
                                 linewidth = 0.55)
  }
  nnights <- nrow(nt)
  sub <- paste0(nnights, " noon-to-noon night", if (nnights == 1) "" else "s", "; ",
                if (nrow(bars) > 0) paste0(nrow(bars), " sustained inactivity bouts over ",
                                           length(unique(bars$night)), " of them")
                else "no sustained inactivity bouts",
                if (!is.null(guide) && nrow(guide) > 0)
                  paste0("; guider ", paste(sort(unique(as.character(guide$guider))), collapse = ", "))
                else "; no guider window")
  if (length(defs) > 0) {
    g <- g + ggplot2::scale_fill_manual(values = stats::setNames(
      rep(c(cols[["inside"]], cols[["outside"]], cols[["onset"]], cols[["wake"]]),
          length.out = ndef), defs), name = "sib definition")
  }
  if (!is.null(marks) && nrow(marks) > 0) {
    # only when there is something to colour, or ggplot2 warns about an unused manual scale
    g <- g + ggplot2::scale_colour_manual(
      values = c("Sleep onset" = unname(cols[["onset"]]), "Wake-up" = unname(cols[["wake"]])),
      name = NULL)
  }
  g <- g +
    ggplot2::scale_x_continuous(breaks = brk, labels = ifelse(brk >= 24, brk - 24, brk),
                                limits = c(xlo, xhi),
                                expand = ggplot2::expansion(mult = c(0.005, 0.005))) +
    ggplot2::scale_y_continuous(breaks = c(sib_lo + (sib_hi - sib_lo) / 2, acc_lo, acc_hi,
                                           ang_lo, 0, ang_hi),
                                labels = c("bouts", "0 mg", "500 mg", "-90", "0", "+90 deg"),
                                limits = c(ymin, ymax),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::facet_wrap(~night, ncol = 1) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(sub), x = "Clock hour, noon to noon",
                  y = NULL,
                  caption = .raw.sleep.plot.caption(
                    p, paste0("Black: z angle",
                              if (!is.na(metricName)) paste0("; grey: ", metricName,
                                                             " (mg, capped at 500)") else "",
                              "; blue band: the guider window; grey: epochs the part-2 ",
                              "decision ignored",
                              if (part1) paste0(". No imputed series was found, so the ",
                                                "part-1 series is drawn; the detector saw ",
                                                "the imputed one") else ""))) +
    .raw.plot.theme() +
    ggplot2::theme(panel.spacing = ggplot2::unit(0.4, "lines"),
                   strip.text = ggplot2::element_text(hjust = 0),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "bottom")
  g
}

# ONE ROW PER NIGHT

#' Every Night on One Axis, Bouts Inside and Outside the Sleep Period
#'
#' The canhrActi form of GGIR's \code{results/visualisation_sleep.pdf}: a 12 to 36 axis
#' relabelled to clock hours, one row per night, every sustained inactivity bout drawn as a
#' bar coloured by whether it overlaps the guider window, and the guider window itself as an
#' outlined bar behind the row.
#'
#' Reading it: a good night is one solid dark block whose edges line up with the outlined
#' guider bar, with few white gaps inside it. A bad night is a dark block far from the
#' outline, a block riddled with white, or a long pale daytime block next to a short dark
#' one.
#'
#' @details The \code{do.visual} block of GGIR's g.part4 redrawn in ggplot2. Kept: the
#'   12..36 axis relabelled 12..24, 1..12; the dashed verticals at 18, 24 and 30 and the
#'   dotted ones at 15, 21, 27 and 33; one lane per sib definition inside each night's row;
#'   the two colours for bouts that do and do not overlap the guider; the wrap of a bar that
#'   crosses the right edge; and the mark at 18 for a day sleeper. Changed: canhrActi's
#'   colours, an outline instead of the hatch, no page state, and nights GGIR would have
#'   skipped (\code{cleaningcode >= cleaningcriterion}) drawn faded rather than dropped.
#'
#' @param x A \code{canhrActi_raw_nights} from \code{\link{raw.sleep.nights}}, which carries
#'   the labelled bouts and the resolved guider window; or a \code{canhrActi_raw} or
#'   \code{canhrActi_raw_sleep}, in which case the bouts come from \code{sib.cla.sum} and the
#'   guider from the part-3 SPT estimate, and the overlap flag is recomputed here with the
#'   partial-overlap rule part 4 uses.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot when there is nothing to draw.
#' @examples
#' \dontrun{
#' plot_raw_sleep_nights(raw.sleep.nights(x$sleep))
#' }
#' @seealso \code{\link{plot_raw_sleep}}, \code{\link{plot_raw_sleep_regularity}}
#' @export plot_raw_sleep_nights
plot_raw_sleep_nights <- function(x, ...) {
  # C time locale for the call, as the other two figures do
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  p <- .raw.sleep.plot.parts(x)
  title <- "Nights on one axis: bouts against the guider window"
  if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
  cols <- .raw.sleep.plot.colours()
  recomputed <- FALSE
  ep <- if (!is.null(p$ns_attr)) p$ns_attr$episodes else NULL
  gt <- if (!is.null(p$ns_attr)) p$ns_attr$guiders else NULL
  if (is.null(ep) || nrow(ep) == 0) {
    # a part-3 object: build the same table from sib.cla.sum and the SPT estimate
    sleep <- p$sleep
    if (is.null(sleep) || !is.data.frame(sleep$nights) || nrow(sleep$nights) == 0) {
      return(.raw.plot.empty(paste0("No labelled sleep bouts were found. Run raw.sleep.nights() ",
                                    "and pass its result."), title))
    }
    bars <- .raw.sleep.plot.sibbars(sleep$sib.cla.sum, sleep$nights, p$tz, p$ws3)
    if (nrow(bars) == 0) {
      return(.raw.plot.empty("This recording has no sustained inactivity bouts to draw.", title))
    }
    nt <- sleep$nights
    gt <- data.frame(night = nt$night, guider = nt$guider,
                     guider_onset = nt$SPTE_start, guider_wakeup = nt$SPTE_end,
                     daysleeper = FALSE, cleaningcode = NA_real_,
                     calendar_date = nt$date, stringsAsFactors = FALSE)
    gi <- match(bars$night, gt$night)
    # part 4's overlap rule: a bout counts when any part of it falls inside the guider window
    ep <- data.frame(night = bars$night, def = bars$definition, nb = bars$period,
                     start = bars$xmin, end = bars$xmax,
                     overlapGuider = as.numeric(!is.na(gt$guider_onset[gi]) &
                                                  bars$xmax > gt$guider_onset[gi] &
                                                  bars$xmin < gt$guider_wakeup[gi]),
                     duration = bars$xmax - bars$xmin, stringsAsFactors = FALSE)
    recomputed <- TRUE
  }
  if (is.null(ep) || nrow(ep) == 0) {
    return(.raw.plot.empty("This recording has no sustained inactivity bouts to draw.", title))
  }
  if (is.null(gt) || nrow(gt) == 0) {
    gt <- data.frame(night = sort(unique(ep$night)), guider = NA_character_,
                     guider_onset = NA_real_, guider_wakeup = NA_real_,
                     daysleeper = FALSE, cleaningcode = NA_real_,
                     calendar_date = NA_character_, stringsAsFactors = FALSE)
  }
  defs <- sort(unique(as.character(ep$def)))
  ndef <- max(1L, length(defs))
  nn <- sort(unique(c(ep$night, gt$night)))
  # night 1 at the top, the direction plot_raw_sleep() reads in
  rowof <- stats::setNames(rev(seq_along(nn)), as.character(nn))
  lab <- vapply(nn, function(k) {
    gi <- which(gt$night == k)
    d <- if (length(gi) > 0 && !is.null(gt$calendar_date)) as.character(gt$calendar_date[gi[1]]) else NA
    if (is.na(d) || !nzchar(d)) paste0("Night ", k) else paste0("Night ", k, "  ", d)
  }, character(1))

  # a bar whose start is past its end wraps into two, one to 36 and one from 12, as in GGIR
  wrap <- function(df, s, e) {
    out <- list()
    for (i in seq_len(nrow(df))) {
      a <- df[[s]][i]; b <- df[[e]][i]
      if (is.na(a) || is.na(b)) next
      if (a > b) {
        out[[length(out) + 1]] <- cbind(df[i, , drop = FALSE], data.frame(x0 = a, x1 = 36))
        out[[length(out) + 1]] <- cbind(df[i, , drop = FALSE], data.frame(x0 = 12, x1 = b))
      } else {
        out[[length(out) + 1]] <- cbind(df[i, , drop = FALSE], data.frame(x0 = a, x1 = b))
      }
    }
    if (length(out) == 0) return(NULL)
    r <- do.call(rbind, out)
    row.names(r) <- NULL
    r
  }
  ep$row <- unname(rowof[as.character(ep$night)])
  ep$lane <- match(as.character(ep$def), defs)
  bars <- wrap(ep, "start", "end")
  # GGIR's qbot and qtop: the definitions share the middle 0.6 of the row
  bars$ymin <- bars$row + ((bars$lane - 1) / ndef) * 0.6 - 0.3
  bars$ymax <- bars$row + (bars$lane / ndef) * 0.6 - 0.3
  bars$where <- ifelse(bars$overlapGuider == 1, "In the sleep period", "Outside it")
  # GGIR skips a night with cleaningcode >= cleaningcriterion; here it is faded instead
  bars$faded <- FALSE
  if (!is.null(gt$cleaningcode)) {
    bad <- gt$night[which(!is.na(gt$cleaningcode) & gt$cleaningcode >= 2)]
    bars$faded <- bars$night %in% bad
  }
  gt2 <- gt[!is.na(gt$guider_onset) & !is.na(gt$guider_wakeup), , drop = FALSE]
  gbar <- NULL
  if (nrow(gt2) > 0) {
    # a window past 36 is pulled back by 24, as GGIR does
    gt2$guider_wakeup[gt2$guider_wakeup > 36] <- gt2$guider_wakeup[gt2$guider_wakeup > 36] - 24
    gt2$guider_onset[gt2$guider_onset > 36] <- gt2$guider_onset[gt2$guider_onset > 36] - 24
    gt2$row <- unname(rowof[as.character(gt2$night)])
    gbar <- wrap(gt2, "guider_onset", "guider_wakeup")
    gbar$ymin <- gbar$row - 0.3
    gbar$ymax <- gbar$row + 0.3
  }
  dsl <- if (is.null(gt$daysleeper)) gt[0, , drop = FALSE] else
    gt[which(!is.na(gt$daysleeper) & gt$daysleeper), , drop = FALSE]
  if (nrow(dsl) > 0) dsl$row <- unname(rowof[as.character(dsl$night)])

  xlo <- min(c(12, bars$x0, gbar$x0), na.rm = TRUE)
  xhi <- max(c(36, bars$x1, gbar$x1), na.rm = TRUE)
  brk <- seq(ceiling(xlo), floor(xhi), by = 2)
  nfaded <- length(unique(bars$night[bars$faded]))
  sub <- paste0(length(nn), " night", if (length(nn) == 1) "" else "s", ", ",
                nrow(ep), " bout", if (nrow(ep) == 1) "" else "s", ", ",
                length(which(ep$overlapGuider == 1)), " of them inside the sleep period",
                if (nfaded > 0) paste0("; ", nfaded, " night",
                                       if (nfaded == 1) "" else "s",
                                       " GGIR would not have drawn (cleaningcode 2 or more) ",
                                       "are faded") else "")
  g <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = seq_along(nn), colour = "grey88", linewidth = 0.25,
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = c(18, 24, 30), colour = "grey70", linewidth = 0.3,
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = c(15, 21, 27, 33), colour = "grey85", linewidth = 0.3,
                        linetype = "dotted")
  g <- g + ggplot2::geom_rect(data = bars,
                              ggplot2::aes(xmin = .data$x0, xmax = .data$x1,
                                           ymin = .data$ymin, ymax = .data$ymax,
                                           fill = .data$where, alpha = .data$faded),
                              colour = NA)
  if (!is.null(gbar) && nrow(gbar) > 0) {
    # the outline goes on top, or a night whose bouts fill the window would hide it
    g <- g + ggplot2::geom_rect(data = gbar,
                                ggplot2::aes(xmin = .data$x0, xmax = .data$x1,
                                             ymin = .data$ymin, ymax = .data$ymax),
                                fill = NA, colour = cols[["guider"]], linewidth = 0.45)
  }
  if (nrow(dsl) > 0) {
    g <- g + ggplot2::geom_segment(data = dsl,
                                   ggplot2::aes(x = 18, xend = 18, y = .data$row - 0.3,
                                                yend = .data$row + 0.3),
                                   colour = cols[["guider"]], linewidth = 0.9, linetype = "dashed")
  }
  g <- g +
    ggplot2::scale_fill_manual(values = c("In the sleep period" = unname(cols[["inside"]]),
                                          "Outside it" = unname(cols[["outside"]])), name = NULL) +
    ggplot2::scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.35), guide = "none") +
    ggplot2::scale_x_continuous(breaks = brk, labels = ifelse(brk >= 24, brk - 24, brk),
                                limits = c(xlo, xhi),
                                expand = ggplot2::expansion(mult = c(0.005, 0.005))) +
    ggplot2::scale_y_continuous(breaks = rev(seq_along(nn)), labels = lab,
                                limits = c(0.4, length(nn) + 0.75),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(sub),
                  x = "Clock hour, noon to noon", y = NULL,
                  caption = .raw.sleep.plot.caption(
                    p, paste0("Outlined bar: the guider window",
                              if (nrow(dsl) > 0) "; dashed mark at 18:00: a day sleeper" else "",
                              if (recomputed)
                                paste0(". No night table was given, so the guider is part 3's ",
                                       "sleep period estimate and the overlap flag was ",
                                       "recomputed here")
                              else ""))) +
    .raw.plot.theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom")
  g
}

# SLEEP REGULARITY INDEX

#' The Sleep Regularity Index, Day Pair by Day Pair
#'
#' One bar per day pair, with the fraction of the pair that was valid drawn over it on a
#' second axis, because an SRI computed from a tenth of a day says nothing. The dashed line
#' is 16 of 24 hours, the fraction part 4 requires before it copies a day's SRI into the
#' night summary.
#'
#' Reading it: bars near 100 mean the person slept and woke at the same times two days
#' running; bars near 0 mean the two days shared nothing. A bar whose point sits below the
#' dashed line is not evidence either way, and part 4 leaves it out of the night table.
#'
#' @details The values are \code{raw.sleep.regularity()}'s; GGIR draws no figure for them.
#'   The 16/24 line is part 4's inclusion rule, a strict \code{>} comparison.
#'
#' @param x A \code{canhrActi_raw} carrying \code{$sleep}, a \code{canhrActi_raw_sleep}, or
#'   the SRI data frame itself.
#' @param ... Not used.
#' @return A ggplot object; the empty-state plot when the SRI is the scalar NA (fewer than
#'   the three days it needs) or there is nothing to draw.
#' @examples
#' \dontrun{
#' plot_raw_sleep_regularity(x)
#' }
#' @seealso \code{\link{plot_raw_sleep}}, \code{\link{plot_raw_sleep_nights}}
#' @export plot_raw_sleep_regularity
plot_raw_sleep_regularity <- function(x, ...) {
  # C time locale for the call, so the weekday labels are English
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  title <- "Sleep Regularity Index, day pair by day pair"
  sri <- NULL; p <- NULL
  if (is.data.frame(x) && all(c("SleepRegularityIndex", "frac_valid") %in% names(x))) {
    sri <- x
    p <- list(filename = NA_character_, id = NA_character_, tz = "")
  } else {
    p <- .raw.sleep.plot.parts(x)
    if (!identical(p$state, "ok")) return(.raw.plot.empty(p$state, title))
    if (is.null(p$sleep)) {
      return(.raw.plot.empty(paste0("This is a night table, which carries no Sleep Regularity ",
                                    "Index. Pass the recording or its part-3 result."), title))
    }
    sri <- p$sleep$SleepRegularityIndex
  }
  if (!is.data.frame(sri) || nrow(sri) == 0) {
    return(.raw.plot.empty(paste0("No Sleep Regularity Index was computed: the index needs at ",
                                  "least three calendar days of data."), title))
  }
  cols <- .raw.sleep.plot.colours()
  d <- data.frame(day = seq_len(nrow(sri)),
                  sri = as.numeric(sri$SleepRegularityIndex),
                  frac = as.numeric(sri$frac_valid),
                  stringsAsFactors = FALSE)
  lab <- rep(NA_character_, nrow(sri))
  if ("date" %in% names(sri)) lab <- as.character(sri$date)
  if ("weekday" %in% names(sri)) {
    wd <- substr(as.character(sri$weekday), 1, 3)
    lab <- ifelse(is.na(lab), wd, paste0(wd, "\n", lab))
  }
  lab[is.na(lab)] <- paste0("Day ", d$day[is.na(lab)])
  d$label <- lab
  d$used <- d$frac > (16 / 24)
  ymax <- max(c(100, d$sri), na.rm = TRUE)
  nused <- length(which(d$used))
  sub <- paste0(nrow(d), " day pair", if (nrow(d) == 1) "" else "s", "; ", nused,
                " with more than 16 of 24 hours valid, which is what the night summary needs",
                if (nused > 0) paste0("; those average ",
                                      format(round(mean(d$sri[d$used]), 1), trim = TRUE)) else "")
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$day)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(y = .data$sri, fill = .data$used), width = 0.68) +
    ggplot2::geom_hline(yintercept = 100 * (16 / 24), colour = cols[["wake"]],
                        linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data$frac * 100), colour = cols[["reference"]],
                       linewidth = 0.4, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$frac * 100), colour = cols[["reference"]],
                        size = 1.6, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c("TRUE" = unname(cols[["inside"]]),
                                          "FALSE" = unname(cols[["invalid"]])),
                               labels = c("TRUE" = "More than 16 h valid",
                                          "FALSE" = "Too little valid data"),
                               name = NULL, breaks = c("TRUE", "FALSE")) +
    ggplot2::scale_x_continuous(breaks = d$day, labels = d$label,
                                expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
    ggplot2::scale_y_continuous(
      limits = c(min(c(0, d$sri), na.rm = TRUE), max(ymax, 100) * 1.02),
      sec.axis = ggplot2::sec_axis(~ . / 100, name = "Fraction of the day pair valid",
                                   breaks = c(0, 0.25, 0.5, 16 / 24, 1),
                                   labels = c("0", "0.25", "0.5", "16/24", "1"))) +
    ggplot2::labs(title = title, subtitle = .raw.plot.wrap(sub), x = NULL,
                  y = "Sleep Regularity Index",
                  caption = .raw.sleep.plot.caption(
                    p, paste0("Bars: the index, -100 to 100. Dark line and points: the ",
                              "fraction of the day pair that was valid, on the right axis"))) +
    .raw.plot.theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   legend.position = "bottom")
  g
}
