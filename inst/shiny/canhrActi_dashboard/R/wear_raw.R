# The Wear time page, drawn for raw recordings: GGIR's part-2 wear decision
# reported in GGIR's own names. Pure drawing, like overview_raw.R: a
# canhrActi_raw (or a list of them) in, tags out.

wtr_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || !is.finite(x)) default else x
}

# GGIR's includedaycrit, from the params the recording was read with
wtr_daycrit <- function(r) wtr_num(r$params$includedaycrit, 16)

# Valid days a recording needs to meet the criteria; the rule bar and the figure both read it
WTR_MIN_VALID_DAYS <- 3

wtr_daily <- function(r) {
  d <- r$wear$daily
  if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
  d
}

# GGIR's ID, which the read takes as extractID does: for an ActiGraph the file
# name with every space removed
wtr_id <- function(r, fid = NULL) {
  id <- r$inspection$id
  if (is.character(id) && length(id) == 1 && !is.na(id) && nzchar(id)) id
  else gsub(" ", "", ovr_name(r, fid))
}

# The part2_summary row under GGIR's names; NA where there is no value
wtr_summary_row <- function(r, fid = NULL) {
  w <- r$wear %||% list()
  d <- r$device %||% list()
  sp <- ovr_span(r)
  list(
    id = wtr_id(r, fid),
    # GGIR's device_sn is the serial with the firmware appended
    device_sn = if (is.null(d$serial)) NA_character_
                else if (is.null(d$firmware)) d$serial
                else paste0(d$serial, "_firmware_", d$firmware),
    bodylocation = d$bodylocation %||% "not extracted",
    filename = ovr_name(r, fid),
    start_time = if (is.null(sp)) NA_character_ else format(sp$start, "%Y-%m-%dT%H:%M:%S%z"),
    startday = if (is.null(sp)) NA_character_ else fmt_date(sp$start, "%A"),
    samplefreq = wtr_num(d$sf),
    device = d$brand %||% NA_character_,
    clipping_score = wtr_num(w$clipping_score),
    meas_dur_dys = wtr_num(w$meas_dur_dys),
    complete_24hcycle = if (isTRUE(wtr_num(w$meas_dur_dys) >= 1)) 1 else 0,
    meas_dur_def_proto_day = wtr_num(w$meas_dur_def_proto_day),
    wear_dur_def_proto_day = wtr_num(w$wear_dur_def_proto_day),
    calib_err = wtr_num(r$calibration$cal_error_end),
    calib_status = r$calibration$qcmessage %||% NA_character_,
    we_days = wtr_num(w$n_valid_weekend_days),
    wd_days = wtr_num(w$n_valid_weekdays),
    valid_days = wtr_num(w$n_valid_days))
}

WTR_SUM_COLS <- c("ID", "device_sn", "bodylocation", "filename", "start_time", "startday",
                  "samplefreq", "device", "clipping_score", "meas_dur_dys", "complete_24hcycle",
                  "meas_dur_def_proto_day", "wear_dur_def_proto_day", "calib_err",
                  "calib_status", "N valid weekend days (WE)", "N valid weekdays (WD)")
# GGIR's order. start_time is per day: the recording's start on day 1 and
# local midnight after that, which is what $daily$start_time holds.
WTR_DAY_COLS <- c("ID", "filename", "start_time", "calendar_date", "bodylocation",
                  "N valid hours", "N hours", "weekday", "measurementday")

wtr_fmt <- function(v, digits = NULL) {
  if (is.null(v) || length(v) == 0) return("NA")
  if (is.numeric(v)) {
    if (!is.finite(v)) return("NA")
    return(if (is.null(digits)) format(v, trim = TRUE) else fmt_dec(v, digits))
  }
  v <- as.character(v)[1]
  if (is.na(v) || !nzchar(v)) "NA" else v
}

# RULE BAR

# Names the parameters that produced these numbers
wtr_rule <- function(rs, ns, stale = FALSE) {
  r1 <- rs[[1]]
  approach <- as.character(r1$params$nonwear_approach %||% "2023")
  tags$div(
    class = "wt-panel wt-rule",
    tags$span(class = "wt-rule-k", "Non-wear"),
    tags$span(class = "wt-rule-v",
      tags$b("GGIR part 2"), " · ", approach, " approach · ",
      "15 min windows · decided while reading"),
    tags$span(class = "wt-rule-sep", `aria-hidden` = "true"),
    tags$span(class = "wt-rule-k", "Valid day"),
    tags$span(class = "wt-rule-v",
      tags$b(paste0("includedaycrit ", fmt_dec(wtr_daycrit(r1), 0), " h")), " of the 24 · subject at ",
      tags$b(fmt_int(WTR_MIN_VALID_DAYS)), " days"),
    # No controls: GGIR decides wear while reading the file, so there is nothing
    # to change or re-score here without a re-read
    if (stale) tags$span(class = "wt-stale", "changed since the scores below"))
}

# FIGURES

# The counts page's five, asked of the raw batch; weekend days replaces one,
# since GGIR counts weekday and weekend validity separately.

# The same markup as wt_fig on the counts side. code keeps a GGIR name as typed.
wtr_fig <- function(value, unit, label, state = NULL, code = FALSE) {
  ink <- switch(state %||% "", bad = "var(--ovl-danger)", warn = "var(--ovl-warn)", NULL)
  tags$div(class = "wt-fig",
    tags$div(tags$span(class = "wt-fig-n", style = if (!is.null(ink)) paste0("color:", ink, ";"), value),
             if (!is.null(unit)) tags$span(class = "wt-fig-u", unit)),
    tags$div(class = if (isTRUE(code)) "wt-fig-l wtr-code" else "wt-fig-l", label))
}

wtr_figures <- function(rs) {
  n <- length(rs)
  rows <- lapply(rs, wtr_summary_row)
  valid <- vapply(rows, function(x) x$valid_days, numeric(1))
  days <- vapply(rs, function(r) { d <- wtr_daily(r); if (is.null(d)) 0 else nrow(d) }, numeric(1))
  we <- vapply(rows, function(x) x$we_days, numeric(1))
  worn <- vapply(rows, function(x) x$wear_dur_def_proto_day, numeric(1))
  meas <- vapply(rows, function(x) x$meas_dur_def_proto_day, numeric(1))
  pass <- sum(is.finite(valid) & valid >= WTR_MIN_VALID_DAYS)
  tot_valid <- sum(valid[is.finite(valid)])
  tot_we <- sum(we[is.finite(we)])

  tags$div(
    class = "wt-panel wt-figs",
    wtr_fig(fmt_int(n), NULL, if (n == 1) "Recording" else "Recordings"),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    wtr_fig(paste(fmt_int(pass), "of", fmt_int(n)), NULL, "Meet the criteria"),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    wtr_fig(paste(fmt_int(tot_valid), "of", fmt_int(sum(days))), NULL, "Valid days"),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    wtr_fig(paste(fmt_int(tot_we), "of", fmt_int(tot_valid)), NULL, "Valid weekend days (WE)",
            if (tot_we == 0 && tot_valid > 0) "warn" else NULL),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    wtr_fig(if (any(is.finite(worn))) fmt_dec(sum(worn[is.finite(worn)]), 2) else "–",
            paste0("of ", fmt_dec(sum(meas[is.finite(meas)]), 2), " d"), "wear_dur_def_proto_day",
            code = TRUE))
}

# TABLES

# One frame, two tabs: the two exports are the same thing at different grain.

# Which recording the page is about, one native select (selectize = FALSE).
# all adds an "All recordings" entry for a page whose table can show every
# recording; the chart then falls back to the first. Shared with Activity.
wtr_who <- function(rs, ids, sel, input_id, ns, all = FALSE) {
  who <- stats::setNames(ids, vapply(seq_along(ids),
                                     function(k) ovr_name(rs[[k]], ids[k]), ""))
  if (all) who <- c("All recordings" = "all", who)
  cur <- if (is.null(sel) || !sel %in% ids) (if (all) "all" else ids[1]) else sel
  tags$div(class = "wtr-who",
    selectInput(ns(input_id), NULL, width = "100%", selectize = FALSE,
                choices = who, selected = cur))
}

# The same box on a row of its own, for a state with no header to hold it.
# Not drawn for a single recording.
wtr_who_row <- function(rs, ids, sel, input_id, ns, all = FALSE) {
  if (length(rs) < 2) return(NULL)
  tags$div(class = "wtr-pick",
    tags$span(class = "wtr-pick-k", "Recording"),
    wtr_who(rs, ids, sel, input_id, ns, all = all))
}

# Sortable header. The key is the column index, not its name, because the
# metric column names change with the study's analysis parameters.
wtr_th <- function(i, label, sort, dir, right = FALSE, stick = FALSE) {
  on <- identical(sort, i)
  tags$th(
    class = trimws(paste("wt-sortable", if (stick) "wtr-stick" else "",
                         if (on) "is-sorted" else "")),
    `data-rawsort` = i, role = "button", tabindex = "0",
    `aria-sort` = if (on) (if (dir > 0) "ascending" else "descending") else "none",
    style = if (right) "text-align: right;" else NULL,
    label, if (on) wt_caret(dir > 0))
}

# Numeric order when every value looks numeric, alphabetical otherwise
wtr_order <- function(v, dir) {
  n <- suppressWarnings(as.numeric(v))
  k <- if (all(is.na(n) == is.na(v) | is.na(v))) n else tolower(as.character(v))
  order(k, decreasing = dir < 0, na.last = TRUE)
}

wtr_head_row <- function(cols, sort, dir, right = integer(0)) {
  tags$thead(tags$tr(lapply(seq_along(cols), function(i)
    wtr_th(i, cols[i], sort, dir, right = i %in% right, stick = i == 1))))
}

wtr_tab <- function(key, label, active, ns) {
  tags$button(type = "button", class = paste("ov-tab", if (active) "is-active" else ""),
              `data-weartab` = key, role = "tab",
              `aria-selected` = tolower(as.character(active)), label)
}

# right-aligned columns of part2_summary
WTR_SUM_RIGHT <- c(7, 9, 10, 11, 12, 13, 14, 16, 17)

wtr_summary_table <- function(rs, ids, sel, ns, sort = NULL, dir = 1) {
  # Follow the selection, as the day table does
  keep <- if (is.null(sel)) seq_along(rs) else which(ids == sel)
  if (!is.null(sort) && length(keep) > 1) {
    vals <- vapply(keep, function(i) {
      x <- wtr_summary_row(rs[[i]], ids[i]); as.character(wtr_fmt(x[[min(sort, length(x))]]))
    }, character(1))
    keep <- keep[wtr_order(vals, dir)]
  }
  rows <- lapply(keep, function(i) {
    x <- wtr_summary_row(rs[[i]], ids[i])
    bad <- is.finite(x$calib_err) && x$calib_err * 1000 > 10
    tags$tr(
      class = paste("wt-row", if (identical(sel, ids[i])) "is-selected" else ""),
      `data-rawfile` = ids[i], tabindex = "0",
      tags$td(class = "wtr-stick", x$id),
      tags$td(wtr_fmt(x$device_sn)), tags$td(wtr_fmt(x$bodylocation)),
      tags$td(wtr_fmt(x$filename)), tags$td(wtr_fmt(x$start_time)),
      tags$td(wtr_fmt(x$startday)),
      tags$td(class = "r", wtr_fmt(x$samplefreq)), tags$td(wtr_fmt(x$device)),
      tags$td(class = "r", wtr_fmt(x$clipping_score)),
      tags$td(class = "r", wtr_fmt(x$meas_dur_dys, 3)),
      tags$td(class = "r", wtr_fmt(x$complete_24hcycle)),
      tags$td(class = "r", wtr_fmt(x$meas_dur_def_proto_day, 3)),
      tags$td(class = "r", wtr_fmt(x$wear_dur_def_proto_day, 3)),
      tags$td(class = if (bad) "r wtr-bad" else "r", wtr_fmt(x$calib_err, 4)),
      tags$td(class = "wtr-wrap", wtr_fmt(x$calib_status)),
      tags$td(class = if (isTRUE(x$we_days == 0)) "r wtr-warn" else "r", wtr_fmt(x$we_days)),
      tags$td(class = "r", wtr_fmt(x$wd_days)))
  })
  tags$table(class = "wt-table wtr-gt",
    wtr_head_row(WTR_SUM_COLS, sort, dir, WTR_SUM_RIGHT),
    tags$tbody(rows))
}

# right-aligned columns of part2_daysummary
WTR_DAY_RIGHT <- c(6, 7, 9)

wtr_day_table <- function(rs, ids, sel, ns, sort = NULL, dir = 1) {
  keep <- if (is.null(sel)) seq_along(rs) else which(ids == sel)
  rows <- list(); keys <- list()
  for (i in keep) {
    r <- rs[[i]]; d <- wtr_daily(r)
    if (is.null(d)) next
    nm <- ovr_name(r, ids[i])
    id <- wtr_id(r, ids[i])
    loc <- r$device$bodylocation %||% "not extracted"
    for (k in seq_len(nrow(d))) {
      ok <- isTRUE(d$valid[k])
      we <- isTRUE(d$weekend[k])
      # the cell values in column order, kept beside the row for sorting
      keys[[length(keys) + 1]] <- c(id, nm, as.character(d$start_time[k]),
        as.character(d$date[k]), loc, wtr_fmt(d$n_valid_hours[k], 2),
        wtr_fmt(d$n_hours[k], 1), as.character(d$weekday[k]), wtr_fmt(d$day[k]))
      rows[[length(rows) + 1]] <- tags$tr(
        tags$td(class = "wtr-stick", id), tags$td(nm),
        tags$td(class = "sub", wtr_fmt(d$start_time[k])),
        tags$td(as.character(d$date[k])),
        tags$td(class = "sub", loc),
        tags$td(class = if (ok) "r" else "r sub", wtr_fmt(d$n_valid_hours[k], 2)),
        tags$td(class = "r sub", wtr_fmt(d$n_hours[k], 1)),
        tags$td(class = if (we) "wtr-warn" else "sub", as.character(d$weekday[k])),
        tags$td(class = "r sub", wtr_fmt(d$day[k])))
    }
  }
  if (length(rows) == 0) return(tags$div(class = "wt-none", "No days to show."))
  if (!is.null(sort) && length(rows) > 1) {
    j <- min(sort, length(keys[[1]]))
    rows <- rows[wtr_order(vapply(keys, function(v) v[j], character(1)), dir)]
  }
  tags$table(class = "wt-table wtr-gt",
    wtr_head_row(WTR_DAY_COLS, sort, dir, WTR_DAY_RIGHT),
    tags$tbody(rows))
}

wtr_table_panel <- function(rs, ids, sel, tab, ns, sort = NULL, dir = 1) {
  n_days <- sum(vapply(rs, function(r) { d <- wtr_daily(r); if (is.null(d)) 0L else nrow(d) }, integer(1)))
  shown <- if (is.null(sel)) rs else rs[ids == sel]
  sub <- if (identical(tab, "day"))
    paste0(fmt_int(if (is.null(sel)) n_days else nrow(wtr_daily(shown[[1]]) %||% data.frame())),
           " days · ", length(WTR_DAY_COLS), " columns")
  else paste0(fmt_int(length(shown)), if (length(shown) == 1) " recording · " else " recordings · ",
              length(WTR_SUM_COLS), " columns")

  tags$div(
    class = "wt-panel wtr-tables",
    tags$div(class = "wtr-head",
      tags$div(class = "ov-tabs",
        wtr_tab("summary", "part2_summary", identical(tab, "summary"), ns),
        wtr_tab("day", "part2_daysummary", identical(tab, "day"), ns)),
      tags$span(class = "wtr-head-r",
        tags$span(class = "wtr-sub", sub),
        tags$span(class = "wtr-more", "scroll for the rest"),
        downloadButton(ns("raw_export"), "Export csv", class = "wt-btn wt-btn--secondary", icon = NULL))),
    tags$div(class = "wtr-scroll",
      if (identical(tab, "day")) wtr_day_table(rs, ids, sel, ns, sort, dir)
      else wtr_summary_table(rs, ids, sel, ns, sort, dir)))
}

# GGIR's full part-2 schema. The metric half is computed by the port in
# R/raw_analyse_perday.R and R/raw_analyse_perfile.R, for every metric GGIR
# analyses. The column names carry the study's analysis parameters
# ("MVPA_E5S_T100_ENMO_0-24hr"), and mvpathreshold is a vector adding six
# columns each, so the header is built per recording from what it was read
# with, never a fixed list.

# The seven settings columns GGIR tails its summary with. "stategy" is
# GGIR's own spelling and must match.
WTR_PROV_COLS <- c(
  paste0("data exclusion stategy (value=1, ignore specific hours; value=2, ",
         "ignore all data before the first midnight and after the last midnight)"),
  "n hours ignored at start of meas (if data_masking_strategy=1)",
  "n hours ignored at end of meas (if data_masking_strategy=1)",
  "n days of measurement after which all data is ignored (if data_masking_strategy=1)",
  "epoch size to which acceleration was averaged (seconds)",
  "if_hip_long_axis_id",
  "GGIR version")

# The analysis parameters a recording was read with; GGIR's default
# mvpathreshold is NULL, which means no MVPA columns at all.
wtr_apar <- function(r) canhrActi:::.raw.perday.params(r$params)

# Can the port reproduce this configuration? A refusal is passed to the page.
wtr_metrics_ok <- function(r) canhrActi:::.raw.perday.supported(wtr_apar(r))

# The metric half of one recording: the day columns with the one 1-6am column
# part2_daysummary keeps, the summary aggregates (which read every metric's
# 1-6am column) and the full-recording means. NULL when the port cannot
# reproduce the configuration.
wtr_activity <- function(r) {
  if (!isTRUE(wtr_metrics_ok(r)) || !is.data.frame(r$imputed$metashort)) return(NULL)
  day <- canhrActi:::.raw.analyse.perday(r)
  fm <- canhrActi:::.raw.full.recording.metrics(r$imputed$metashort)
  frm <- lapply(fm, function(m) canhrActi:::.raw.full.recording.mean(r, m))
  names(frm) <- paste0(fm, "_fullRecordingMean")
  list(day = canhrActi:::.raw.perday.report(day),
       agg = if (ncol(day)) canhrActi:::.raw.analyse.perfile(day, r$wear$daily, wtr_daycrit(r)),
       frm = if (length(fm)) as.data.frame(frm, check.names = FALSE))
}

# All seven come from the read parameters. "GGIR version" is the GGIR the
# port is proven against, ggir_version_label, as in the part-5 tables.
wtr_prov_row <- function(r) {
  p <- r$params %||% list()
  out <- data.frame(wtr_num(p$data_masking_strategy), wtr_num(p$hrs.del.start),
                    wtr_num(p$hrs.del.end), wtr_num(p$maxdur),
                    wtr_num(r$meta$windowsizes[1]), NA_character_,
                    as.character(p$ggir_version_label %||% canhrActi:::.RAW_GGIR_VERSION_LABEL)[1],
                    stringsAsFactors = FALSE)
  names(out) <- WTR_PROV_COLS
  out
}

# Place a filled frame into a full schema, leaving everything else empty.
wtr_widen <- function(df, cols) {
  n <- nrow(df)
  out <- as.data.frame(setNames(rep(list(rep(NA_character_, n)), length(cols)), cols),
                       check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in intersect(names(df), cols)) out[[nm]] <- df[[nm]]
  out
}

# GGIR reports part-2 numerics to 3 decimals
wtr_round3 <- function(df) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, 3)
  df
}

# Recordings whose headers differ are stacked as g.report.part2 stacks them:
# the columns of the first, then any new ones, empty where a row has none
wtr_stack <- function(parts) {
  as.data.frame(data.table::rbindlist(parts, fill = TRUE), check.names = FALSE)
}

# EXPORT

# The csv is the table's rows under GGIR's full header, the recordings in
# g.report.part2's order; a missing value is an empty cell, as in GGIR's own
# part2 csv, and a "_split" file name is split as addSplitNames splits it.

wtr_keep <- function(rs, ids, sel) {
  keep <- if (is.null(sel)) seq_along(rs) else which(ids == sel)
  keep[ovr_ggir_order(rs[keep], ids[keep])]
}

wtr_summary_df <- function(rs, ids, sel) {
  keep <- wtr_keep(rs, ids, sel)
  if (length(keep) == 0) return(wtr_widen(data.frame(), c(WTR_SUM_COLS, WTR_PROV_COLS)))
  canhrActi:::.raw.report.addsplitnames(wtr_stack(lapply(keep, function(i) {
    r <- rs[[i]]
    x <- wtr_summary_row(r, ids[i])
    out <- data.frame(x$id, x$device_sn, x$bodylocation, x$filename, x$start_time,
                      x$startday, x$samplefreq, x$device, x$clipping_score,
                      x$meas_dur_dys, x$complete_24hcycle, x$meas_dur_def_proto_day,
                      x$wear_dur_def_proto_day, x$calib_err, x$calib_status,
                      x$we_days, x$wd_days,
                      stringsAsFactors = FALSE)
    names(out) <- WTR_SUM_COLS
    out <- wtr_round3(out)
    # identification and calibration, the full-recording means, the valid-day
    # counts, the aggregate blocks and settings, then GGIR's own reordering
    a <- wtr_activity(r)
    row <- cbind(out[1:15], if (!is.null(a$frm)) wtr_round3(a$frm), out[16:17],
                 if (!is.null(a$agg)) wtr_round3(a$agg), wtr_prov_row(r))
    row[canhrActi:::.raw.perfile.order(names(row))]
  })))
}

wtr_day_df <- function(rs, ids, sel) {
  keep <- wtr_keep(rs, ids, sel)
  parts <- list()
  for (i in keep) {
    d <- wtr_daily(rs[[i]])
    if (is.null(d)) next
    r <- rs[[i]]
    nm <- ovr_name(r, ids[i])
    base <- data.frame(
      wtr_id(r, ids[i]), nm, as.character(d$start_time), as.character(d$date),
      r$device$bodylocation %||% "not extracted",
      d$n_valid_hours, d$n_hours, as.character(d$weekday), d$day,
      stringsAsFactors = FALSE)
    names(base) <- WTR_DAY_COLS
    a <- wtr_activity(r)
    if (!is.null(a) && ncol(a$day)) base <- cbind(base, wtr_round3(a$day))
    parts[[length(parts) + 1]] <- base
  }
  if (length(parts) == 0) return(wtr_widen(data.frame(), WTR_DAY_COLS))
  canhrActi:::.raw.report.addsplitnames(wtr_stack(parts))
}

# CHART

# Two views of one wear decision. Hours per day (plot_raw_wear_days) is the
# bar chart of GGIR's legacy g.plot5, with days below the criterion drawn too
# and the line at the criterion in force; Non-wear over time
# (plot_raw_quality) is GGIR's QC/plots_part2. On "All recordings" the chart
# falls back to the first recording.

WTR_CHARTS <- c(days = "Hours per day", quality = "Non-wear over time")

wtr_chart_panel <- function(rs, ids, sel, chart, ns) {
  i <- if (is.null(sel)) 1L else match(sel, ids)
  if (is.na(i)) i <- 1L
  nm <- ovr_name(rs[[i]], ids[i])
  if (!chart %in% names(WTR_CHARTS)) chart <- "days"
  d <- wtr_daily(rs[[i]])
  n_days <- if (is.null(d)) 0L else nrow(d)

  tags$div(
    class = "wt-panel wtr-chart",
    tags$div(class = "wtr-chart-h",
      # One recording has nothing to choose between
      if (length(rs) < 2) tags$span(class = "wtr-showing", tags$b(nm), " · ", plural(n_days, "day"))
      else tagList(
        wtr_who(rs, ids, sel, "rawwho_set", ns, all = TRUE),
        tags$span(class = "wtr-showing",
          if (is.null(sel) || is.na(match(sel, ids)))
            tagList("showing ", tags$b(nm), " · this chart draws one recording at a time")
          else tagList(fmt_int(n_days), " days · the selected recording"))),
      tags$span(class = "wtr-chart-chips",
        lapply(names(WTR_CHARTS), function(k)
          tags$button(type = "button",
                      class = paste("wtr-chip", if (identical(k, chart)) "is-on" else ""),
                      `data-wearchart` = k, WTR_CHARTS[[k]])))),
    tags$div(class = "wtr-chart-b", plotOutput(ns("raw_plot"), height = "auto")))
}

# The plot is drawn at the browser's width and WTR_CHART_ASPECT times shorter,
# so it fills its box exactly. 2.3 is the ratio at which eight day bars fit.
WTR_CHART_ASPECT <- 2.3

wtr_chart_height <- function(width) {
  if (is.null(width) || length(width) != 1 || !is.finite(width) || width < 240) width <- 1280
  max(260, round(width / WTR_CHART_ASPECT))
}
