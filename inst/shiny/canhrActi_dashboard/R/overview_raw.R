# The Overview page for raw recordings: the same figure row, table geometry and
# panel chrome as overview_loaded.R. Pure functions: a canhrActi_raw object
# (or a list of them) in, tags out.

# Metrics asked of part 1 at read time. ENMOa and MAD are read with ENMO
# because half of GGIR's published cut points use one of them and part 5 can
# only use a metric stored at read time; none of the three filters, so they
# cost nothing. BFEN filters and is left off (Schaefer 2014 needs a re-read).
# RAW_READ_TAG goes in the cache file name; change it with the metrics. m3:
# an upload is read under its own name, not Shiny's 0.gt3x.
RAW_READ_METRICS <- list(do.enmo = TRUE, do.enmoa = TRUE, do.mad = TRUE)
RAW_READ_TAG <- "m3"

# The metric columns the read produces. A cut point defined against anything
# else cannot be run here, and a re-read would ask for these same three.
RAW_READ_HAS <- c("ENMO", "ENMOa", "MAD")

# ACCESSORS

# A result can be missing any stage when a recording fails partway, so each
# accessor returns NA rather than erroring and the page draws a dash.

ovr_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || !is.finite(x)) default else x
}

# Calibration error after the fit, in mg. GGIR stores it in g.
ovr_cal_mg <- function(r) ovr_num(r$calibration$cal_error_end) * 1000

ovr_valid_days <- function(r) ovr_num(r$wear$n_valid_days)

# Worn and recorded, in days, from the part-2 decision.
ovr_wear <- function(r) {
  worn <- ovr_num(r$wear$wear_dur_def_proto_day)
  total <- ovr_num(r$wear$meas_dur_dys)
  if (is.na(worn) || is.na(total) || total <= 0) return(list(worn = NA_real_, total = total, pct = NA_real_))
  list(worn = worn, total = total, pct = worn / total * 100)
}

# NULL when the recording is usable, otherwise the reason. read.raw.accelerometer
# does not throw on a bad file: it returns a canhrActi_raw with status$corrupt
# set, metashort NULL and status$messages saying what went wrong.
ovr_unusable <- function(r) {
  st <- r$status %||% list()
  why <- if (isTRUE(st$corrupt)) "the file could not be read"
    else if (isTRUE(st$too_short)) "the recording is under two hours"
    else if (isTRUE(st$skipped)) "the file was skipped"
    else if (!is.data.frame(r$meta$metashort) || nrow(r$meta$metashort) == 0)
      "the reader produced no epochs"
    else return(NULL)
  # The pipeline's own messages name the stage and the reason
  msgs <- as.character(st$messages %||% character(0))
  msgs <- trimws(gsub("[\r\n]+", " ", msgs))
  msgs <- unique(msgs[nzchar(msgs)])
  if (length(msgs) == 0) return(why)
  paste0(why, ". ", paste(utils::head(msgs, 3), collapse = " "))
}

# The checks table, split the way the strip reports it.
ovr_checks <- function(r) {
  ch <- r$checks
  # an explicit empty frame, since NULL[0, , drop = FALSE] is NULL and nrow(NULL) is NULL
  empty <- data.frame(id = character(0), check = character(0), status = character(0),
                      value = character(0), threshold = character(0), message = character(0),
                      stringsAsFactors = FALSE)
  if (!is.data.frame(ch) || nrow(ch) == 0) return(list(n = 0L, warn = empty, n_ok = 0L, n_info = 0L))
  st <- as.character(ch$status)
  list(n = nrow(ch), warn = ch[st %in% c("warn", "fail"), , drop = FALSE],
       n_ok = sum(st == "ok"), n_info = sum(st == "info"))
}

# metashort$timestamp is an ISO string and $time the epoch seconds; read $time
# when it is there and fall back to parsing the string.
ovr_time_at <- function(r, i) {
  ms <- r$meta$metashort
  if (!is.data.frame(ms) || nrow(ms) == 0) return(NULL)
  i <- i[i >= 1 & i <= nrow(ms)]
  if (length(i) == 0) return(NULL)
  tz <- ovr_tz(r)
  if ("time" %in% names(ms) && is.numeric(ms$time)) {
    return(as.POSIXct(ms$time[i], origin = "1970-01-01", tz = tz))
  }
  t <- ms$timestamp[i]
  if (inherits(t, "POSIXt")) return(t)
  out <- as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S", tz = tz)
  if (all(is.na(out))) out <- as.POSIXct(t, tz = tz)
  out
}

ovr_span <- function(r) {
  ms <- r$meta$metashort
  if (!is.data.frame(ms) || nrow(ms) == 0) return(NULL)
  ws3 <- ovr_num(r$meta$windowsizes[1], 5)
  ends <- ovr_time_at(r, c(1L, nrow(ms)))
  if (is.null(ends) || any(is.na(ends))) return(NULL)
  list(start = ends[1], end = ends[2] + ws3)
}

# r$tz records the whole timezone decision; the page wants the effective one
ovr_tz <- function(r) {
  z <- r$tz
  if (is.character(z) && length(z) == 1) return(z)
  as.character(z$effective_tz %||% z$machine_tz %||% z$desiredtz %||% "")[1]
}

# r$file is a list: path, filename, size, md5
ovr_name <- function(r, fallback = "") {
  f <- r$file
  nm <- if (is.character(f) && length(f) == 1) basename(f) else as.character(f$filename %||% "")[1]
  if (is.na(nm) || !nzchar(nm)) fallback else nm
}

ovr_subject <- function(r) {
  s <- r$device$serial
  if (is.null(s) || !nzchar(s)) tools::file_path_sans_ext(ovr_name(r)) else s
}

# FIGURES

# One figure of the count row, the markup overview_loaded.R uses. Colour only
# marks a crossed threshold.
ovr_fig <- function(value, unit, label, state = NULL) {
  ink <- switch(state %||% "", bad = "var(--ovl-danger)", warn = "var(--ovl-warn)", NULL)
  tags$div(
    class = "ovl-fig",
    tags$div(class = "ovl-fig-n", style = if (!is.null(ink)) paste0("color:", ink, ";"),
             value, if (!is.null(unit)) tags$span(class = "ovl-fig-u", unit)),
    tags$div(class = "ovl-fig-l", label))
}

ovr_rule <- function() tags$div(class = "ovl-fig-rule")

# The five figures of the raw page: whether the batch can be analysed
ovr_figures <- function(rs) {
  n <- length(rs)
  cal <- vapply(rs, ovr_cal_mg, numeric(1))
  valid <- vapply(rs, ovr_valid_days, numeric(1))
  warns <- vapply(rs, function(r) nrow(ovr_checks(r)$warn), integer(1))
  ready <- sum(is.finite(cal) & cal <= RAW_CAL_LIMIT & is.finite(valid) & valid > 0)
  worst <- suppressWarnings(max(cal, na.rm = TRUE))
  n_valid <- sum(valid[is.finite(valid)])
  n_warn <- sum(warns)

  tags$div(
    class = "ovl-figs",
    ovr_fig(fmt_int(n), NULL, if (n == 1) "Recording" else "Recordings"),
    ovr_rule(),
    ovr_fig(fmt_int(ready), paste("of", n), "Ready to analyse"),
    ovr_rule(),
    ovr_fig(if (is.finite(n_valid)) fmt_int(n_valid) else "–", NULL, "Valid days"),
    ovr_rule(),
    ovr_fig(if (is.finite(worst)) fmt_dec(worst, 1) else "–", "mg", "Worst calibration",
            if (is.finite(worst) && worst > RAW_CAL_LIMIT) "bad" else NULL),
    ovr_rule(),
    ovr_fig(fmt_int(n_warn), NULL, "Checks to review", if (n_warn > 0) "warn" else NULL))
}

# The two thresholds the page colours against
RAW_CAL_LIMIT <- 10      # mg, GGIR's acceptance line
RAW_WEAR_LIMIT <- 16     # hours, a valid day

# COVERAGE LANE

# Worn solid, non-wear hatched, on the window shared by every recording. Under
# it a 3px rail marks the long gaps the pipeline filled at epoch level.

# The gaps filled at epoch level, from the detector the Gaps figure uses. About
# 0.3 s on 118,800 epochs, so it is computed once when a recording is added and
# carried on the record. NULL if the internal detector is unavailable.
ovr_fill_runs <- function(r) {
  ms <- r$meta$metashort
  if (!is.data.frame(ms) || nrow(ms) == 0) return(NULL)
  ws <- r$meta$windowsizes
  out <- tryCatch(
    canhrActi:::.raw.plot.fill.runs(ms, ovr_num(ws[1], 5), ovr_num(ws[2], 900)),
    error = function(e) NULL)
  if (!is.data.frame(out) || nrow(out) == 0 || !"level" %in% names(out)) return(NULL)
  out[out$level == "epoch", , drop = FALSE]
}

ovr_runs <- function(flag) {
  if (length(flag) == 0) return(NULL)
  r <- rle(as.logical(flag))
  stops <- cumsum(r$lengths)
  data.frame(value = r$values, from = stops - r$lengths + 1, to = stops)
}

ovr_lane <- function(r, w0, w1) {
  sp <- ovr_span(r)
  if (is.null(sp)) return(tags$span(class = "ovl-cov-none", "–"))
  total <- as.numeric(difftime(w1, w0, units = "secs"))
  if (!is.finite(total) || total <= 0) return(tags$span(class = "ovl-cov-none", "–"))
  pc <- function(t) max(0, min(100, as.numeric(difftime(t, w0, units = "secs")) / total * 100))

  ws3 <- ovr_num(r$meta$windowsizes[1], 5)
  r5 <- suppressWarnings(as.numeric(r$wear$r5long))
  bars <- list()
  if (length(r5) > 0) {
    r5[is.na(r5)] <- 1
    runs <- ovr_runs(r5 == 0)
    for (i in seq_len(nrow(runs))) {
      t0 <- sp$start + (runs$from[i] - 1) * ws3
      t1 <- sp$start + runs$to[i] * ws3
      a <- pc(t0)
      b <- pc(t1)
      if (b - a <= 0) next
      worn <- isTRUE(runs$value[i])
      bars[[length(bars) + 1]] <- tags$i(
        class = if (worn) "on" else "off",
        title = ovl_span_title(if (worn) "Worn" else "Non-wear", t0, t1),
        style = sprintf("left:%.3f%%;width:%.3f%%;", a, b - a))
    }
  }

  # The rail: the stretches filled at epoch level, from r$imputed_runs. Not
  # metalong$nonwearscore == 3, which is also set on ordinary still non-wear.
  rail <- list()
  runs <- r$imputed_runs
  if (is.data.frame(runs) && nrow(runs) > 0) {
    for (i in seq_len(nrow(runs))) {
      t0 <- sp$start + (runs$start_epoch[i] - 1) * ws3
      t1 <- sp$start + runs$end_epoch[i] * ws3
      a <- pc(t0)
      b <- pc(t1)
      if (b - a <= 0) next
      rail[[length(rail) + 1]] <- tags$i(title = ovl_span_title("Imputed", t0, t1),
                                         style = sprintf("left:%.3f%%;width:%.3f%%;", a, b - a))
    }
  }

  w <- ovr_wear(r)
  # The rail's marks are absolute, so track and rail sit in one positioned box
  # of their own width. The rail is drawn even when empty so rows match in height.
  tags$div(
    class = "ovr-lane",
    tags$div(
      class = "ovr-lane-stack",
      tags$div(class = "ovr-lane-track", bars),
      tags$div(class = "ovr-lane-rail", rail)),
    tags$span(class = "ovr-lane-pct",
              if (is.na(w$pct)) "–" else paste0(round(w$pct), "%")))
}

# RECORDINGS TABLE

# Six columns, every one a GGIR output. Device type is on the Details tab.

ovr_cell_state <- function(value, limit, over = TRUE) {
  if (!is.finite(value)) return(NULL)
  if (over && value > limit) "bad" else if (!over && value <= limit) "bad" else NULL
}

ovr_num_cell <- function(text, state) {
  cls <- paste("r", switch(state %||% "", bad = "ovr-bad", warn = "ovr-warn", ""))
  tags$td(class = trimws(cls), text)
}

OVR_SORT_KEYS <- c("name", "subject", "cal", "valid", "checks")

# Recording ids in the order the table shows them; ties fall back to the name
ovr_order <- function(rs, ids, key = "name", dir = 1) {
  if (length(ids) < 2) return(ids)
  nm <- vapply(ids, function(id) tolower(ovr_name(rs[[id]], id)), character(1))
  value <- switch(key %||% "name",
    subject = vapply(ids, function(id) tolower(ovr_subject(rs[[id]])), character(1)),
    cal = vapply(ids, function(id) ovr_cal_mg(rs[[id]]), numeric(1)),
    valid = vapply(ids, function(id) ovr_valid_days(rs[[id]]), numeric(1)),
    checks = vapply(ids, function(id) as.numeric(nrow(ovr_checks(rs[[id]])$warn)), numeric(1)),
    nm)
  ids[order(value, nm, decreasing = isTRUE(dir < 0), na.last = TRUE)]
}

# ids arrive in table order (ovr_order); sort is the key and direction the
# headers show
ovr_table <- function(rs, ids, selected, ns, w0, w1, sort = list(key = "name", dir = 1)) {
  ax <- ovl_axis(w0, w1)
  head_span <- paste0("Coverage · ", date_span(w0, w1))

  # data-rawsort, not data-sort, which drives the counts table's order
  sort_head <- function(key, label, right = FALSE) {
    active <- identical(sort$key, key)
    up <- !isTRUE(sort$dir < 0)
    cls <- c(if (right) "r", if (active) "is-sorted")
    tags$th(class = if (length(cls)) paste(cls, collapse = " "),
            `aria-sort` = if (active) (if (up) "ascending" else "descending") else "none",
            tags$span(class = "ovl-sortable", `data-rawsort` = key, role = "button", tabindex = "0",
                      title = paste("Sort by", tolower(label)), label, if (active) caret_glyph(up)))
  }

  rows <- lapply(ids, function(fid) {
    r <- rs[[fid]]
    cal <- ovr_cal_mg(r)
    valid <- ovr_valid_days(r)
    ck <- ovr_checks(r)
    n_warn <- nrow(ck$warn)
    tags$tr(
      class = paste("ov-row", if (identical(fid, selected)) "is-selected" else ""),
      `data-fid` = fid, tabindex = "0", role = "option",
      tags$td(class = "ovr-name", tags$a(href = "#", `data-fid` = fid, ovr_name(r, fid))),
      tags$td(class = "sub", ovr_subject(r)),
      ovr_num_cell(if (is.finite(cal)) paste0(fmt_dec(cal, 1), " mg") else "–",
                   ovr_cell_state(cal, RAW_CAL_LIMIT)),
      tags$td(class = "ovr-lane-td", ax$grid, ovr_lane(r, w0, w1)),
      ovr_num_cell(if (is.finite(valid)) fmt_int(valid) else "–",
                   ovr_cell_state(valid, 0, over = FALSE)),
      tags$td(title = if (n_warn > 0) cell_title(paste(ovr_check_label(ck$warn$check), collapse = ", ")),
              if (n_warn == 0) tags$span(class = "sub", "clean")
              else tags$span(class = "ovr-warn", paste(n_warn, "to review"))),
      # one delegated click handler reports the row, as the counts table does
      tags$td(class = "ovl-rm ovr-x",
              tags$button(type = "button", class = "ov-x ovr-x-btn", `data-fid` = fid,
                          title = "Remove this recording", cross_glyph_raw())))
  })

  tagList(
    tags$div(
      class = "ovl-scroll",
      tags$table(
        class = "ovl-gt ovr-gt",
        tags$colgroup(
          tags$col(style = "width: 236px;"), tags$col(style = "width: 132px;"),
          tags$col(style = "width: 104px;"), tags$col(),
          tags$col(style = "width: 88px;"), tags$col(style = "width: 112px;"),
          tags$col(style = "width: 34px;")),
        tags$thead(
          tags$tr(
            class = "ov-col-head",
            sort_head("name", "Recording"), sort_head("subject", "Subject"),
            sort_head("cal", "Calibration", right = TRUE),
            tags$th(head_span), sort_head("valid", "Valid days", right = TRUE),
            sort_head("checks", "Checks"), tags$th()),
          if (!is.null(ax)) tags$tr(
            class = "ovl-axis",
            tags$th(), tags$th(), tags$th(),
            tags$th(tags$span(class = "ovl-axis-row", ax$ticks)),
            tags$th(), tags$th(), tags$th())),
        tags$tbody(class = "ov-list-body", role = "listbox",
                   `aria-label` = "Loaded raw recordings", rows))),
    tags$div(class = "ovl-foot",
      sprintf(paste0("Colour marks a crossed threshold and nothing else: calibration %d mg, ",
                     "a valid day %d h of wear. The amber rail marks gaps over 90 min, filled at ",
                     "epoch level and scored non-wear. GGIR %s parity."),
              RAW_CAL_LIMIT, RAW_WEAR_LIMIT, canhrActi:::.RAW_GGIR_VERSION_LABEL)))
}

cross_glyph_raw <- function() {
  HTML(paste0('<svg viewBox="0 0 12 12" fill="none" stroke="currentColor" stroke-width="1.6" ',
              'stroke-linecap="round" aria-hidden="true"><path d="M2.5 2.5l7 7M9.5 2.5l-7 7"></path></svg>'))
}

# QUALITY EXPORT

# Frames with different columns stacked into one, columns in first-seen order.
# The filehealth columns differ by brand, so the rows do not all match.
ovr_bind <- function(frames) {
  frames <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, frames)
  if (length(frames) == 0) return(data.frame())
  cols <- unique(unlist(lapply(frames, names)))
  out <- do.call(rbind, lapply(frames, function(d) {
    for (col in setdiff(cols, names(d))) d[[col]] <- NA
    d[, cols, drop = FALSE]
  }))
  rownames(out) <- NULL
  out
}

# GGIR's data_quality_report row per recording, then the part-2 and canhrActi
# fields raw.quality.report adds after it
ovr_quality_frame <- function(rs) ovr_bind(lapply(rs, function(r) r$quality))

# g.report.part2 reads the recordings in the order of their part-1 milestone
# names, meta_<file name>.RData
ovr_ggir_order <- function(rs, ids = names(rs)) {
  if (length(rs) == 0) return(integer(0))
  fn <- vapply(seq_along(rs), function(i) ovr_name(rs[[i]], ids[i]), "")
  order(paste0("meta_", fn, ".RData"))
}

# The columns of GGIR's own data_quality_report.csv: the 21 g.report.part2
# builds from the part-1 milestone, then the filehealth block of part 2
OVR_QUALITY_GGIR <- c("filename", "file.corrupt", "file.too.short", "use.temperature",
                      "scale.x", "scale.y", "scale.z", "offset.x", "offset.y", "offset.z",
                      "temperature.offset.x", "temperature.offset.y", "temperature.offset.z",
                      "cal.error.start", "cal.error.end", "n.10sec.windows",
                      "n.hours.considered", "QCmessage", "mean.temp", "device.serial.number",
                      "NFilePagesSkipped")
ovr_quality_ggir_cols <- function(q) {
  c(intersect(OVR_QUALITY_GGIR, names(q)), grep("^filehealth_", names(q), value = TRUE))
}

# GGIR's data_quality_report rows stacked as g.report.part2 stacks them, rs in
# GGIR's order. A corrupt or too short file has no part-2 summary to take
# filehealth columns from. A row with fewer columns is padded with " " and
# takes the earlier names by position; one with more pads the earlier rows
# and sets the column order.
ovr_quality_ggir <- function(rs) {
  QCout <- NULL
  for (i in seq_along(rs)) {
    q <- rs[[i]]$quality
    g <- ovr_quality_ggir_cols(q)
    if (isTRUE(as.logical(q$file.corrupt[1])) || isTRUE(as.logical(q$file.too.short[1]))) {
      g <- g[!grepl("^filehealth_", g)]
    }
    QC <- q[g]
    if (is.null(rs[[i]]$inspection$sf)) {
      if (i == 1) {
        QCout <- QC
      } else {
        QC[setdiff(names(QCout), names(QC))] <- NA
        QCout <- rbind(QCout, QC)
      }
      next
    }
    if (i == 1) {
      QCout <- QC
    } else {
      n1 <- ncol(QCout)
      n2 <- ncol(QC)
      if (n1 > n2) {
        QC <- cbind(QC, matrix(" ", 1, (n1 - n2)))
        colnames(QC) <- colnames(QCout)
        QC <- QC[, colnames(QCout)]
      } else if (n1 < n2) {
        newcolnames <- colnames(QC)[which(colnames(QC) %in% colnames(QCout) == FALSE)]
        newcols <- (n1 + 1):(n1 + length(newcolnames))
        QCout <- cbind(QCout, matrix(" ", 1, n2 - n1))
        colnames(QCout)[newcols] <- newcolnames
        QCout <- QCout[, colnames(QC)]
      }
      QCout <- rbind(QCout, QC)
    }
  }
  QCout
}

# The checks the Data quality tab reads, all recordings in one table
ovr_checks_frame <- function(rs) {
  ovr_bind(lapply(names(rs), function(fid) {
    ch <- rs[[fid]]$checks
    if (!is.data.frame(ch) || nrow(ch) == 0) return(NULL)
    data.frame(filename = rep(ovr_name(rs[[fid]], fid), nrow(ch)), ch,
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

# DATA QUALITY TAB

# Six chips for the six figures the pipeline draws, and the checks in a strip
# along the bottom of the panel so the figure gets the full width.

OVR_CHIPS <- c(overview = "Overview", gaps = "Gaps", calibration = "Calibration",
               days = "Per day", nights = "Nights", chunks = "Chunks")

ovr_chips <- function(active, ns) {
  tags$span(class = "ovr-chips",
    lapply(names(OVR_CHIPS), function(k)
      tags$button(type = "button", class = paste("ovr-chip", if (identical(k, active)) "is-on" else ""),
                  `data-chip` = k, OVR_CHIPS[[k]])))
}

# Gaps, Per day, Nights and Overview grow with the recording, so they are
# drawn to its length and scroll inside the figure box. At res 96 a row of
# Per day or Nights is a 38 px strip, 8 px of spacing and a panel of at least
# 96 px, under at most 260 px of titles, axis and caption; a day of Gaps is
# 100 px plus the axis's 1% at each end, beside about 175 px of row labels,
# and clears its date label.
OVR_FIG_SCROLL <- c("gaps", "days", "nights", "overview")
OVR_FIG_ROW_H <- 144
OVR_FIG_TEXT_H <- 260
OVR_FIG_DAY_W <- 102
OVR_FIG_TEXT_W <- 180
# Overview gives a day the same 100 px, beside 90 px of strip, axis and
# margin. Each panel is at least 240 px tall so the longest strip label
# (236 px) is not cut, 12 px apart, under 265 px of titles, legend, axis
# and caption.
OVR_QC_TEXT_W <- 90
OVR_QC_TEXT_H <- 265
OVR_QC_ROW_H <- 252
# Per day, Nights and Overview are never drawn narrower than their titles:
# the panel starts at most 88 px in and the widest title or subtitle line is
# 738 px
OVR_FIG_MIN_W <- 830
# Shiny draws with ragg, which refuses more than 50000 device px, and a pixel
# ratio of 2 doubles the drawn size, so past this length (less above a ratio
# of 2) the rows get thinner instead
OVR_FIG_MAX <- 24000
# A figure at most this much longer than the box is drawn at the box size
OVR_FIG_SLACK <- 24

# Rows of Per day and Nights, cut at the edges the plots cut at, days of
# timeline for Gaps, or days and panels for Overview; 0 when the plot draws
# its one-sentence empty state
ovr_fig_rows <- function(r, chip) {
  ms <- r$meta$metashort
  ml <- r$meta$metalong
  ws <- suppressWarnings(as.numeric(r$meta$windowsizes)[1])
  if (!is.data.frame(ms) || nrow(ms) == 0 || !"timestamp" %in% names(ms)) return(0)
  if (!is.data.frame(ml) || nrow(ml) == 0 || !isTRUE(ws > 0)) return(0)
  if (isTRUE(r$status$skipped) || isTRUE(r$status$corrupt) || isTRUE(r$status$too_short)) return(0)
  if (identical(chip, "gaps")) return(nrow(ms) * ws / 86400)
  if (identical(chip, "overview")) {
    ws2 <- suppressWarnings(as.numeric(r$meta$windowsizes)[2])
    if (!isTRUE(ws2 > 0)) return(0)
    temp <- "temperaturemean" %in% names(ml) && any(!is.na(ml$temperaturemean))
    return(c(nrow(ml) * ws2 / 86400, 2 + temp))
  }
  clock <- substr(as.character(ms$timestamp), 12, 19)
  if (identical(chip, "nights")) {
    if (!"anglez" %in% names(ms)) return(0)
    return(length(unique(c(1L, which(clock == "12:00:00")))))
  }
  edges <- which(clock == sprintf("%02d:00:00", as.integer(r$wear$settings$dayborder %||% 0)))
  if (length(edges) == 0) 1 else length(edges) + (edges[1] != 1)
}

# The drawn size: the box, or taller (Per day, Nights), wider (Gaps) or either
# (Overview) when the recording needs it, less the scrollbar sb the other way.
# Per day and Nights also go wider in a box too narrow for their titles, and
# then both ways lose sb. pr is the pixel ratio, which multiplies what the
# device is asked for.
ovr_fig_size <- function(chip, rows, w, h, sb = 0, pr = 1) {
  w <- floor(w); h <- floor(h)
  num <- function(x, lo) { x <- suppressWarnings(as.numeric(x)[1]); if (isTRUE(x >= lo)) x else lo }
  sb <- num(sb, 0); pr <- num(pr, 1)
  n <- suppressWarnings(as.numeric(rows))
  rows <- n[1]
  if (!isTRUE(rows > 0)) return(list(width = w, height = h))
  top <- floor(min(OVR_FIG_MAX, 2 * OVR_FIG_MAX / pr))
  if (identical(chip, "gaps")) {
    need <- min(top, ceiling(OVR_FIG_TEXT_W + rows * OVR_FIG_DAY_W))
    if (need > w + OVR_FIG_SLACK) return(list(width = need, height = h - sb))
  } else if (chip %in% c("days", "nights")) {
    need <- min(top, ceiling(OVR_FIG_TEXT_H + rows * OVR_FIG_ROW_H))
    # a bar one way narrows the box the other way, so the two are settled
    # together; the width gets no slack, since under it the titles are cut
    down <- across <- FALSE
    for (i in 1:2) {
      down <- need > h - across * sb + OVR_FIG_SLACK
      across <- OVR_FIG_MIN_W > w - down * sb
    }
    return(list(width = if (across) OVR_FIG_MIN_W else w - down * sb,
                height = if (down) need else h - across * sb))
  } else if (identical(chip, "overview")) {
    # settled the same way; the slack is for the days, not the titles
    wide <- min(top, ceiling(OVR_QC_TEXT_W + rows * OVR_FIG_DAY_W))
    panels <- if (isTRUE(n[2] >= 1)) n[2] else 2
    need <- ceiling(OVR_QC_TEXT_H + panels * OVR_QC_ROW_H)
    down <- across <- FALSE
    for (i in 1:2) {
      room <- w - down * sb
      across <- wide > room + OVR_FIG_SLACK || OVR_FIG_MIN_W > room
      down <- need > h - across * sb + OVR_FIG_SLACK
    }
    return(list(width = if (across) max(wide, OVR_FIG_MIN_W) else w - down * sb,
                height = if (down) need else h - across * sb))
  }
  list(width = w, height = h)
}

# The strip belongs to the figure above it: each chip gets the checks its own
# figure can answer, and the bar says how many are waiting elsewhere.
ovr_checks_strip <- function(r, open, ns, chip = "overview") {
  ck <- ovr_checks(r)
  n_warn <- nrow(ck$warn)
  if (n_warn == 0) {
    return(tags$div(class = "ovr-strip is-clean",
      tags$span(class = "ovr-strip-t", sprintf("All %d checks passed", ck$n))))
  }
  # Overview is the whole-recording figure, so it keeps the whole list.
  mine <- if (identical(chip, "overview")) rep(TRUE, n_warn) else
    vapply(ck$warn$check, function(x) identical(ovr_check_chip(x), chip), logical(1))
  n_mine <- sum(mine)
  elsewhere <- n_warn - n_mine

  here <- ck$warn[mine, , drop = FALSE]
  label <- OVR_CHIPS[[chip]] %||% "this figure"
  title <- if (identical(chip, "overview")) {
    sprintf("%d of %d checks need a decision", n_warn, ck$n)
  } else if (n_mine == 0) {
    sprintf("Nothing on this figure needs a decision")
  } else {
    sprintf("%s explains %s", label, plural(n_mine, "check"))
  }
  bar <- tags$div(
    class = paste("ovr-strip", if (n_mine == 0) "is-clean" else ""),
    if (n_mine > 0) tags$i(class = "ovr-dot"),
    tags$span(class = "ovr-strip-t", title),
    if (n_mine > 0) tags$span(class = "ovr-strip-n",
      paste(tolower(gsub("_", " ", as.character(here$check))), collapse = ", ")),
    # with nothing on this figure, say where the open ones are
    if (elsewhere > 0 && !identical(chip, "overview")) tags$span(
      class = "ovr-strip-e",
      if (n_mine == 0) tagList(plural(elsewhere, "check"), " open on other figures. ",
                               actionLink(ns("raw_chip_overview"), "Show Overview"))
      else sprintf("%d more elsewhere", elsewhere)),
    if (n_mine > 0) actionLink(ns("raw_checks_toggle"), class = "ovr-strip-a",
      label = tagList(if (open) "Hide" else "Review", ovr_chevron(open))))
  if (!open || n_mine == 0) return(bar)

  rows <- lapply(seq_len(n_mine), function(i) {
    tags$div(
      class = "ovr-check",
      tags$span(class = "ovr-check-k", tags$i(class = "ovr-dot"),
                ovr_check_label(here$check[i])),
      tags$span(class = "ovr-check-v", title = as.character(here$value[i]),
                as.character(here$value[i])),
      tags$span(class = "ovr-check-w", as.character(here$message[i] %||% here$value[i])),
      if (!identical(ovr_check_chip(here$check[i]), chip) &&
          !is.null(ovr_check_chip(here$check[i])))
        tags$button(type = "button", class = "ovr-check-go",
                    `data-chip` = ovr_check_chip(here$check[i]), "Show me"))
  })
  tagList(bar, tags$div(class = "ovr-check-list", rows),
    tags$div(class = "ovr-check-foot",
      sprintf("The other %d: %d passed, %d noted. ", ck$n - n_warn, ck$n_ok, ck$n_info),
      actionLink(ns("raw_show_all_checks"), "Show all checks"),
      sprintf(" · %s.", ovr_versions(r))))
}

# A check id reads as a field name; this is the human form.
ovr_check_label <- function(id) {
  id <- as.character(id)
  lab <- gsub("_", " ", id)
  paste0(toupper(substr(lab, 1, 1)), substr(lab, 2, nchar(lab)))
}

# Which figure explains which check, for the Show me link.
ovr_check_chip <- function(id) {
  switch(as.character(id),
         gaps = , long_gaps = "gaps",
         # the chunk figure shows the acceptance test the status check is about
         calibration_error = , sphere_coverage = , coefficient_sanity = "calibration",
         calibration_status = "chunks",
         wear_time = , nonwear_score = , valid_days = , duration_floor = "days",
         NULL)
}

ovr_chevron <- function(open) {
  d <- if (open) "M3 9l4-4 4 4" else "M3 5l4 4 4-4"
  HTML(paste0('<svg viewBox="0 0 14 14" width="11" height="11" fill="none" stroke="currentColor" ',
              'stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
              '<path d="', d, '"></path></svg>'))
}

# DETAILS TAB

# The counts tab's shape: the id line, one trace, closed disclosures, then the
# epoch preview.

# Mean ENMO per hour across the recording, worn hours solid and the rest pale.
ovr_trace <- function(r, uid, width = 1000, height = 88) {
  ms <- r$meta$metashort
  if (!is.data.frame(ms) || nrow(ms) == 0) return(NULL)
  metric <- if ("ENMO" %in% names(ms)) "ENMO" else setdiff(names(ms), c("timestamp", "time", "anglez"))[1]
  if (is.na(metric) || is.null(metric)) return(NULL)
  ws3 <- ovr_num(r$meta$windowsizes[1], 5)
  per_hour <- max(1, round(3600 / ws3))
  hr <- ((seq_len(nrow(ms)) - 1) %/% per_hour) + 1
  vals <- as.numeric(tapply(suppressWarnings(as.numeric(ms[[metric]])) * 1000, hr, mean))
  n <- length(vals)
  peak <- suppressWarnings(max(vals, na.rm = TRUE))
  if (!is.finite(peak) || peak <= 0 || n < 2) return(NULL)

  r5 <- suppressWarnings(as.numeric(r$wear$r5long))
  worn <- if (length(r5) >= nrow(ms)) {
    r5 <- r5[seq_len(nrow(ms))]; r5[is.na(r5)] <- 1
    as.logical(tapply(r5, hr, function(v) mean(v == 0) > 0.5))
  } else rep(TRUE, n)

  bw <- width / n
  # Non-wear stretches get a pale band behind them, the way the counts trace
  # hatches its gaps; a flat hour alone draws only a 1px bar
  runs <- ovr_runs(worn)
  bands <- character(0)
  for (k in which(!runs$value)) {
    x0 <- (runs$from[k] - 1) * bw
    bands <- c(bands, sprintf('<rect x="%.2f" y="0" width="%.2f" height="%d" fill="var(--ovl-gap-fill)"/>',
                              x0, (runs$to[k] - runs$from[k] + 1) * bw, height))
  }
  bars <- vapply(seq_len(n), function(i) {
    v <- if (is.na(vals[i])) 0 else vals[i]
    h <- max(1, v / peak * (height - 2))
    sprintf('<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s"/>',
            (i - 1) * bw, height - h, max(0.6, bw - 0.4), h,
            if (isTRUE(worn[i])) "var(--ovl-worn)" else "var(--ovl-border-strong)")
  }, character(1))
  bars <- c(bands, bars)

  list(peak = peak, n = n, worn = sum(worn, na.rm = TRUE), metric = metric,
       svg = HTML(sprintf(paste0('<svg class="ovr-trace" viewBox="0 0 %d %d" preserveAspectRatio="none" ',
                                 'role="img" aria-label="Mean %s per hour across the recording">%s</svg>'),
                          round(width), height, metric, paste(bars, collapse = ""))))
}

ovr_pair <- function(k, v, state = NULL) {
  tags$div(class = "ovr-kv-row",
    tags$span(class = "ovr-kv-k", k),
    tags$span(class = paste("ovr-kv-v", switch(state %||% "", bad = "ovr-bad", warn = "ovr-warn", "")),
              if (is.null(v) || !nzchar(as.character(v)[1])) "–" else v))
}

ovr_kv <- function(left, right) {
  tags$div(class = "ovr-kv",
    tags$div(class = "ovr-kv-half", left),
    tags$div(class = "ovr-kv-half", right))
}

ovr_disc <- function(id, title, chips, more, body, open = FALSE) {
  tags$details(
    class = "ovl-disc ovr-disc", `data-persist` = paste0("overview.raw.", id),
    if (open) "open" else NULL,
    tags$summary(
      tags$span(class = "ovl-chev", `aria-hidden` = "true", ovl_chevron()),
      tags$span(class = "ovl-disc-t", title),
      tags$span(class = "ovr-chip-row", lapply(chips, function(c) tags$span(class = "ovr-meta-chip", c))),
      tags$span(class = "ovl-disc-more", more)),
    tags$div(class = "ovl-disc-body", body))
}

ovr_fmt_time <- function(t) if (is.null(t) || length(t) == 0 || is.na(t[1])) NULL else format(t[1], "%Y-%m-%d %H:%M:%S")

ovr_details <- function(r, uid, ns, page = 1, series = "imputed") {
  d <- r$device %||% list()
  cal <- r$calibration %||% list()
  ck <- ovr_checks(r)
  sp <- ovr_span(r)
  tr <- ovr_trace(r, uid)
  vec3 <- function(v) if (is.null(v) || length(v) < 3) NULL else paste(round(as.numeric(v), 6), collapse = ", ")

  tags$div(
    class = "ovl-det ovr-det",

    tags$div(class = "ovl-det-id",
      tags$span(class = "who", ovr_subject(r)),
      tags$span(class = "what", ovr_name(r))),

    # The signal first, as on the counts tab
    if (!is.null(tr)) tags$div(
      class = "ovl-block ovr-block",
      tags$div(class = "ovl-block-head",
        tags$span(class = "ovl-block-t", paste("Mean", tr$metric, "per hour")),
        tags$span(class = "ovl-legend",
          tags$span(class = "ovl-key", tags$i(class = "on"), "Worn"),
          tags$span(class = "ovl-key", tags$i(class = "pale"), "Non-wear"),
          tags$span(class = "ovl-aside",
                    sprintf("peak %s mg · %d of %d hours worn",
                            fmt_dec(tr$peak, 1), tr$worn, tr$n)))),
      tags$div(class = "ovr-trace-wrap", tr$svg),
      tags$div(class = "ovl-fine",
        sprintf("%d hours at %s s epochs, averaged to the hour. Pale bars are the hours GGIR's part-2 decision scores non-wear.",
                tr$n, ovr_num(r$meta$windowsizes[1], 5)))),

    ovr_disc("meta", "What the file says about itself",
      c(d$device_type, paste0(d$sf, " Hz"), paste0(d$dynrange, " g"),
        if (!is.null(d$firmware)) paste("firmware", d$firmware)),
      "device and recording",
      ovr_kv(
        list(ovr_pair("Device type", d$device_type),
             ovr_pair("Serial number", d$serial),
             ovr_pair("Firmware", d$firmware),
             ovr_pair("Format", paste(d$brand, d$format)),
             ovr_pair("Header timezone", d$header_timezone)),
        list(ovr_pair("Sample rate", if (!is.null(d$sf)) paste(d$sf, "Hz")),
             ovr_pair("Dynamic range", if (!is.null(d$dynrange)) paste(d$dynrange, "g")),
             ovr_pair("Clipping threshold", if (!is.null(d$clipthres)) paste(d$clipthres, "g")),
             ovr_pair("First epoch", ovr_fmt_time(sp$start)),
             ovr_pair("Last epoch", ovr_fmt_time(sp$end))))),

    ovr_disc("cal", "Calibration",
      c(sprintf("%s to %s mg", fmt_dec(ovr_num(cal$cal_error_start) * 1000, 1),
                fmt_dec(ovr_num(cal$cal_error_end) * 1000, 1)),
        if (isTRUE(cal$applied)) "applied" else "not applied",
        paste0(ovr_num(cal$nhoursused), " h used")),
      "the fit",
      tagList(
        ovr_kv(
          list(ovr_pair("Error before", paste0(fmt_dec(ovr_num(cal$cal_error_start) * 1000, 1), " mg")),
               ovr_pair("Error after", paste0(fmt_dec(ovr_num(cal$cal_error_end) * 1000, 1), " mg"),
                        if (isTRUE(ovr_cal_mg(r) > RAW_CAL_LIMIT)) "bad" else NULL),
               ovr_pair("Still windows used", fmt_int(ovr_num(cal$npoints))),
               ovr_pair("Hours of data", paste0(ovr_num(cal$nhoursused), " h"),
                        if (isTRUE(ovr_num(cal$nhoursused) < 168)) "warn" else NULL),
               ovr_pair("Temperature used", if (isTRUE(cal$use_temp)) "yes" else "no")),
          list(ovr_pair("Scale x, y, z", vec3(cal$scale)),
               ovr_pair("Offset x, y, z", vec3(cal$offset)),
               ovr_pair("Temperature offset", vec3(cal$tempoffset)),
               ovr_pair("Coefficients applied", if (isTRUE(cal$applied)) "yes" else "no"),
               ovr_pair("Source", cal$source))),
        if (!is.null(cal$qcmessage) && nzchar(cal$qcmessage))
          tags$div(class = "ovr-qc", tags$strong("GGIR: "), cal$qcmessage))),

    ovr_disc("checks", sprintf("All %d checks", ck$n),
      c(sprintf("%d passed", ck$n_ok), sprintf("%d noted", ck$n_info),
        sprintf("%d need a decision", nrow(ck$warn))),
      "the full list",
      ovr_check_table(r)),

    ovr_disc("proc", "How it was processed",
      c(paste("GGIR", canhrActi:::.RAW_GGIR_VERSION_LABEL, "parity"),
        paste(setdiff(names(r$meta$metashort), c("timestamp", "time")), collapse = ", "),
        paste0(ovr_num(r$meta$windowsizes[1], 5), " s epochs"),
        sprintf("%.0f s", ovr_num(r$status$stage_times[["total"]]))),
      "parameters, versions and timings",
      ovr_proc_body(r)),

    ovr_epoch_table(r, page, series, ns))
}

ovr_check_table <- function(r) {
  ch <- r$checks
  if (!is.data.frame(ch) || nrow(ch) == 0) return(tags$div(class = "ovl-fine", "No checks were recorded."))
  tags$table(
    class = "ovl-gt ovr-check-tbl",
    tags$colgroup(tags$col(style = "width: 26px;"), tags$col(style = "width: 190px;"),
                  tags$col(style = "width: 90px;"), tags$col()),
    tags$tbody(lapply(seq_len(nrow(ch)), function(i) {
      st <- as.character(ch$status[i])
      tags$tr(
        tags$td(class = "ovr-st", tags$i(class = paste0("ovr-dot is-", st))),
        tags$td(ovr_check_label(ch$check[i])),
        tags$td(class = "sub", st),
        tags$td(class = "ovr-wrap", as.character(ch$value[i])))
    })))
}

ovr_proc_body <- function(r) {
  st <- r$status$stage_times %||% list()
  pr <- r$meta$settings %||% list()
  one <- function(x) if (is.null(x) || length(x) != 1) NULL else as.character(x)
  tagList(
    ovr_kv(
      list(ovr_pair("Metrics", paste(setdiff(names(r$meta$metashort), c("timestamp", "time")), collapse = ", ")),
           ovr_pair("Short epoch", paste(ovr_num(r$meta$windowsizes[1], 5), "s")),
           ovr_pair("Long epoch", paste(ovr_num(r$meta$windowsizes[2], 900), "s")),
           ovr_pair("Non-wear approach", one(pr$nonwear_approach)),
           ovr_pair("Impute time gaps", one(pr$imputeTimegaps)),
           ovr_pair("Timezone", ovr_tz(r))),
      list(ovr_pair("Block size", one(pr$blocksize)),
           ovr_pair("SD criterion", one(pr$sdcriter)),
           ovr_pair("Range criterion", one(pr$racriter)),
           ovr_pair("Monitor code", one(pr$monc)),
           ovr_pair("Data format code", one(pr$dformc)),
           ovr_pair("GGIR exact", one(pr$ggir_exact)))),
    tags$div(class = "ovr-times",
      tags$span(class = "ovr-times-t", "Time spent"),
      lapply(names(st), function(k)
        tags$span(class = "ovr-time", tags$b(k), sprintf("%.1f s", ovr_num(st[[k]]))))),
    tags$div(class = "ovl-fine",
      ovr_versions(r)))
}

OVR_EPOCH_PAGE <- 100

# The epoch preview shows the imputed series by default, which is what parts
# 3 to 5 read; it differs from the measured series wherever a gap was filled.
ovr_epoch_table <- function(r, page, series, ns) {
  ms <- if (identical(series, "measured")) r$meta$metashort else (r$imputed$metashort %||% r$meta$metashort)
  if (!is.data.frame(ms) || nrow(ms) == 0) return(NULL)
  cols <- setdiff(names(ms), "time")
  total <- nrow(ms)
  first <- (page - 1) * OVR_EPOCH_PAGE + 1
  last <- min(total, first + OVR_EPOCH_PAGE - 1)
  idx <- seq(first, last)
  num <- vapply(cols, function(c) is.numeric(ms[[c]]), logical(1))

  # GGIR's column names, unit in the header. ENMO is in g here; the trace
  # above shows mg
  head_for <- function(c) switch(c,
    timestamp = "Epoch start", anglez = "z angle (deg)", ENMO = "ENMO (g)",
    EN = "EN (g)", BFEN = "BFEN (g)", MAD = "MAD (g)", c)

  tags$div(
    class = "ovr-epochs",
    tags$div(class = "ovl-scroll",
      tags$table(
        class = "ovl-gt",
        tags$thead(tags$tr(class = "ov-col-head", lapply(cols, function(c)
          tags$th(class = if (isTRUE(num[[c]])) "r" else NULL, head_for(c))))),
        tags$tbody(lapply(idx, function(i) tags$tr(lapply(cols, function(c) {
          v <- ms[[c]][i]
          tags$td(class = if (isTRUE(num[[c]])) "r" else NULL,
                  if (inherits(v, "POSIXt")) format(v, "%Y-%m-%d %H:%M:%S")
                  else if (is.numeric(v)) fmt_dec(v, 4)
                  # ISO timestamp with a T and an offset; the offset is
                  # already shown under How it was processed
                  else sub("T", " ", sub("[+-][0-9]{2}:?[0-9]{2}$", "", as.character(v))))
        })))))),
    tags$div(class = "ovr-pager",
      tags$span(sprintf("%s to %s of %s epochs", fmt_int(first), fmt_int(last), fmt_int(total))),
      tags$span(class = "ovr-pager-s",
        if (identical(series, "measured")) "The measured series." else "The imputed series, which is what parts 3 to 5 read.",
        actionLink(ns("raw_series_toggle"),
                   if (identical(series, "measured")) "Show the imputed series"
                   else "Show the measured series instead")),
      tags$span(class = "ovr-pager-b",
        actionButton(ns("raw_epoch_prev"), "Previous", class = "btn-default btn-sm",
                     disabled = if (page <= 1) NA else NULL),
        actionButton(ns("raw_epoch_next"), "Next", class = "btn-default btn-sm",
                     disabled = if (last >= total) NA else NULL))))
}

# the package strings only; versions also carries a POSIXct run_time
ovr_versions <- function(r) {
  v <- r$versions %||% list()
  v <- v[vapply(v, function(x) is.character(x) && length(x) == 1, logical(1))]
  v <- v[setdiff(names(v), "R")]
  paste(paste(names(v), unlist(v)), collapse = " · ")
}
