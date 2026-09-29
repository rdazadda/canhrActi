# The Activity page, drawn for raw recordings: GGIR part 5 time use, every
# epoch placed in a behaviour class, split by sleep period and bout. The
# figure is GGIR's own report (visualReport.R) rendered to an image, and the
# tables are GGIR's part-5 files read back. Pure drawing, like overview_raw.R.

acr_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || !is.finite(x)) default else x
}

acr_day <- function(tu, window = NULL) {
  d <- tu$daysummary
  if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
  # the milestone name ends .RData; GGIR's own csv drops it
  if ("filename" %in% names(d)) {
    d$filename <- sub("\\.RData$", "", as.character(d$filename))
  }
  if (!is.null(window) && "window" %in% names(d)) {
    k <- if (identical(window, "Segments")) d[!d$window %in% c("MM", "WW", "OO"), , drop = FALSE]
         else d[d$window == window, , drop = FALSE]
    if (nrow(k) > 0) return(k)
  }
  d
}

# The windows a recording's part 5 produced
acr_windows <- function(tu) {
  d <- tu$daysummary
  if (!is.data.frame(d) || !"window" %in% names(d)) return(character(0))
  w <- unique(as.character(d$window))
  # segment rows go under one window, Segments, as in GGIR's report
  base <- w[w %in% c("MM", "WW", "OO")]
  if (any(!w %in% c("MM", "WW", "OO"))) c(base, "Segments") else base
}

acr_setting <- function(tu, key, default = NA) {
  v <- tu$settings[[key]]
  if (is.null(v)) default else v
}

# Which guider placed the nights part 5 built its windows on
acr_guider <- function(tu) {
  d <- tu$daysummary
  if (!is.data.frame(d) || !"guider" %in% names(d) || nrow(d) == 0) return("")
  g <- unique(as.character(d$guider))
  g <- g[!is.na(g) & nzchar(g)]
  if (length(g) == 0) "" else paste(g, collapse = ", ")
}

# RULE BAR

# The bar states the thresholds, bout rules and sleep definition GGIR writes
# into its column and file names. MM/WW sits here because it moves everything.

# A threshold as the study wrote it: 40 stays 40, 26.9 stays 26.9.
acr_thr <- function(v) {
  if (!is.finite(v)) return("--")
  if (isTRUE(all.equal(v, round(v)))) fmt_dec(v, 0) else fmt_dec(v, 1)
}

# Titles for GGIR's timewindow codes; the button says the code itself
ACR_WIN_TITLE <- c(MM = "Midnight to midnight",
                   WW = "Waking to waking",
                   OO = "Onset to onset",
                   Segments = "The schedule's windows inside each day")

# One parameter is one box: a single flex item at flex:0 0 auto, so the value
# is never ellipsed and the pair wraps together. plain goes on the title.
acr_g <- function(key, plain, ...) {
  tags$span(class = "wt-rule-g",
    tags$span(class = "wt-rule-k", key),
    tags$span(class = "wt-rule-v", title = plain, ...))
}

# The qualifying part of a value (criterion, brand, site, guider), in less ink
acr_q <- function(x) tags$span(class = "wt-rule-q", x)

# raw.cutpoint.label() joins with " · "; split so the study carries the weight
acr_cut_label <- function(tu) {
  k <- acr_cutpoint_key(tu)
  if (identical(k, ACR_CUSTOM)) return(list(plain = "Custom", ink = tags$b("Custom")))
  lbl <- canhrActi::raw.cutpoint.label(k)
  if (!nzchar(lbl)) return(list(plain = k, ink = tags$b(k)))
  bits <- strsplit(lbl, " · ", fixed = TRUE)[[1]]
  list(plain = lbl,
       ink = tagList(tags$b(bits[1]),
                     if (length(bits) > 1)
                       acr_q(paste0(" · ", paste(bits[-1], collapse = " · ")))))
}

# stale: the settings or the schedule moved since the run the bar describes
acr_rule <- function(tu, window, ns, ran = TRUE, sched_text = NULL, stale = FALSE) {
  thr <- c(acr_num(acr_setting(tu, "threshold.lig"), 40),
           acr_num(acr_setting(tu, "threshold.mod"), 100),
           acr_num(acr_setting(tu, "threshold.vig"), 400))
  bm <- acr_setting(tu, "boutdur.mvpa", c(10, 5, 1))
  bi <- acr_setting(tu, "boutdur.in", c(30, 20, 10))
  cm <- acr_num(acr_setting(tu, "boutcriter.mvpa"), 0.8)
  ci <- acr_num(acr_setting(tu, "boutcriter.in"), 0.9)
  sp <- as.character(acr_setting(tu, "sleepparam", "T5A5"))[1]
  # The metric is named: 332 mg is a moderate MAD value and a vigorous ENMO one
  mt <- as.character(acr_setting(tu, "acc.metric", "ENMO"))[1]
  gd <- acr_guider(tu)
  cut <- acr_cut_label(tu)
  wins <- acr_windows(tu)

  int_plain <- paste0("light ", acr_thr(thr[1]), " · moderate ", acr_thr(thr[2]),
                      " · vigorous ", acr_thr(thr[3]), " mg · ", mt)
  bt_plain <- paste0("MVPA ", paste(bm, collapse = "/"), " min at ",
                     fmt_dec(cm * 100, 0), "% · inactive ",
                     paste(bi, collapse = "/"), " at ", fmt_dec(ci * 100, 0), "%")
  sl_plain <- if (nzchar(gd)) paste0(sp, " · ", gd) else sp

  tags$div(
    class = "wt-panel wt-rule",
    role = "group",
    `aria-label` = "The parameters these numbers were produced with",

    tags$span(
      class = "wt-rule-ps",

      # A decimal only where the published value has one (Aittasalo's 26.9)
      acr_g("Intensity", int_plain,
        "light ", tags$b(acr_thr(thr[1])),
        " · moderate ", tags$b(acr_thr(thr[2])),
        " · vigorous ", tags$b(acr_thr(thr[3])), " mg · ",
        tags$b(mt)),

      # The citation that makes two studies distinguishable; never shortened
      acr_g("Cut points", cut$plain, cut$ink),

      acr_g("Bouts", bt_plain,
        "MVPA ", tags$b(paste(bm, collapse = "/")), " min at ",
        paste0(fmt_dec(cm * 100, 0), "%"),
        " · inactive ", tags$b(paste(bi, collapse = "/")), " at ",
        paste0(fmt_dec(ci * 100, 0), "%")),

      # acr_guider() pastes every distinct guider on a mixed batch, so it is muted
      acr_g("Sleep", sl_plain,
        tags$b(sp),
        if (nzchar(gd)) acr_q(paste0(" · ", gd))),

      if (!is.null(sched_text)) acr_g("Schedule", sched_text, tags$b(sched_text)),

      # a control in the value cell; the delegated handler reads data-acwindow
      if (length(wins) > 1) tags$span(
        class = "wt-rule-g wt-rule-g--seg",
        tags$span(class = "wt-rule-k", "Day"),
        tags$span(class = "wt-rule-v",
          tags$span(class = "acr-seg", role = "group", `aria-label` = "Day window",
            lapply(wins, function(w)
              tags$button(
                type = "button",
                class = paste("acr-seg-b", if (identical(w, window)) "is-on" else ""),
                `aria-pressed` = if (identical(w, window)) "true" else "false",
                title = if (w %in% names(ACR_WIN_TITLE)) unname(ACR_WIN_TITLE[[w]]) else NULL,
                `data-acwindow` = w, w)))))),

    if (isTRUE(ran) && isTRUE(stale)) stale_note("wt"),

    tags$span(class = "wt-rule-actions",
      actionButton(ns("r_sched_open"), "Schedule", class = "wt-btn wt-btn--secondary"),
      actionButton(ns("raw_change"), "Change", class = "wt-btn wt-btn--secondary"),
      # the bar is drawn before the run too, so cut points can be chosen first
      if (isTRUE(ran)) actionButton(ns("raw_reanalyse"), "Re-run",
                                    class = paste(run_button_class("wt", TRUE, stale), "acr-run"))
      else actionButton(ns("raw_run"), "Run analysis",
                        class = paste(run_button_class("wt", FALSE), "acr-run"))))
}

# FIGURES

# Across the study, never one recording. Days analysed is shown against the
# windows part 5 attempted, since it drops a day it could not place one on.

acr_fig <- function(value, unit, label, state = NULL) {
  ink <- switch(state %||% "", bad = "var(--ovl-danger)", warn = "var(--ovl-warn)", NULL)
  # the unit is a sibling of the number, as on the counts strip
  tags$div(class = "wt-fig",
    tags$div(tags$span(class = "wt-fig-n", style = if (!is.null(ink)) paste0("color:", ink, ";"), value),
             if (!is.null(unit)) tags$span(class = "wt-fig-u", unit)),
    tags$div(class = "wt-fig-l", label))
}

acr_mean <- function(rows, col) {
  v <- unlist(lapply(rows, function(d) if (col %in% names(d)) suppressWarnings(as.numeric(d[[col]])) else NULL))
  v <- v[is.finite(v)]
  if (length(v) == 0) NA_real_ else mean(v)
}

acr_figures <- function(tus, window) {
  n <- length(tus)
  rows <- lapply(tus, acr_day, window = window)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  analysed <- sum(vapply(rows, nrow, integer(1)))
  offered <- sum(vapply(tus, function(tu) {
    w <- tu$windows
    if (is.data.frame(w) && nrow(w) > 0) nrow(w) else 0L
  }, integer(1)))
  mvpa <- acr_mean(rows, "dur_day_total_MOD_min") + acr_mean(rows, "dur_day_total_VIG_min")
  inact <- acr_mean(rows, "dur_day_total_IN_min")
  acc <- acr_mean(rows, "ACC_day_mg")

  tags$div(
    class = "wt-panel wt-figs",
    acr_fig(fmt_int(n), NULL, if (n == 1) "Recording" else "Recordings"),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    # on the schedule's windows the rows are segments, not days
    if (identical(window, "Segments"))
      acr_fig(fmt_int(analysed), NULL, "Segments analysed")
    else
      acr_fig(fmt_int(analysed), if (offered > analysed) paste("of", fmt_int(offered), "windows") else NULL,
              "Days analysed", if (offered > analysed) "warn" else NULL),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    acr_fig(if (is.finite(mvpa)) fmt_dec(mvpa, 1) else "–", "min/d", "MVPA"),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    acr_fig(if (is.finite(inact)) fmt_dec(inact, 0) else "–", "min/d", "Inactive"),
    tags$div(class = "wt-fig-rule", `aria-hidden` = "true"),
    acr_fig(if (is.finite(acc)) fmt_dec(acc, 1) else "–", "mg", "ACC day"))
}

# The select box of the Wear time and Sleep raw pages (wtr_who() in wear_raw.R),
# without an All recordings entry: the report is one recording's days.
acr_pick_row <- function(rs, ids, sel, ns) {
  wtr_who_row(rs, ids, sel, "acwho_set", ns)
}

# REPORT PANEL

# GGIR's own report PDF rendered to an image: raw.ggir.report() writes the
# milestones and calls GGIR's visualReport(). The class key is on the figure.

acr_info <- function(k, v) {
  if (is.null(v) || !nzchar(as.character(v)[1]) || is.na(v[1])) return(NULL)
  tags$span(class = "acr-info", k, tags$b(as.character(v)[1]))
}

acr_report_panel <- function(rs, ids, tus, reps, sel, ns) {
  i <- if (is.null(sel)) 1L else match(sel, ids)
  if (is.na(i)) i <- 1L
  r <- rs[[i]]; rep <- reps[[i]]
  sp <- ovr_span(r)
  dur <- if (is.null(sp)) NA else
    paste0(fmt_dec(as.numeric(difftime(sp$end, sp$start, units = "days")), 1), " d")

  tags$div(
    class = "wt-panel acr-report",
    tags$div(class = "wtr-chart-h",
      wtr_who(rs, ids, sel, "acwho_set", ns),
      tags$span(class = "acr-head-r",
        acr_info("start", if (is.null(sp)) NA else format(sp$start, "%Y-%m-%d")),
        acr_info("duration", dur),
        acr_info("tz", ovr_tz(r)),
        tags$span(title = "Save this figure as a PDF",
          downloadButton(ns("raw_pdf"), "Export", class = "wt-btn wt-btn--secondary", icon = NULL)))),
    tags$div(class = "acr-report-b", imageOutput(ns("raw_report"), height = "auto",
                                                 inline = FALSE)))
}


# TABLES

# GGIR's part-5 result files, read back from what g.report.part5() wrote. MM
# and WW are separate files, so the rule bar's window control picks which.

ACR_TABS <- c(daysummary = "part5_daysummary", personsummary = "part5_personsummary",
              daytype = "by kind of day")

# GGIR's day rows for one window grouped by the schedule's kinds of day: the
# mean of every numeric column and the number of days it rests on.
acr_daytype_df <- function(d, sched) {
  if (!is.data.frame(d) || nrow(d) == 0 || !"calendar_date" %in% names(d)) return(NULL)
  cd <- as.character(d$calendar_date)
  dt <- suppressWarnings(as.Date(cd, format = "%Y-%m-%d"))
  alt <- suppressWarnings(as.Date(cd, format = "%d/%m/%Y"))
  dt[is.na(dt)] <- alt[is.na(dt)]
  lab <- unname(sched_type_labels(sched)[sched_day_type(sched, dt)])
  id_cols <- intersect(c("ID", "filename", "window"), names(d))
  numeric_cols <- names(d)[vapply(names(d), function(j) acr_is_num(d[[j]]), logical(1))]
  numeric_cols <- setdiff(numeric_cols, c(id_cols, "calendar_date", "window_number", "night_number",
                                          "start_end_window", "daytype", "weekday", "lastHour", "lastDate"))
  key <- do.call(paste, c(lapply(id_cols, function(k) as.character(d[[k]])), list(lab), sep = "\r"))
  out <- list()
  for (k in unique(key)) {
    g <- d[key == k, , drop = FALSE]
    row <- list()
    for (c0 in id_cols) row[[c0]] <- as.character(g[[c0]][1])
    row[["day_type"]] <- lab[key == k][1]
    row[["days"]] <- as.character(nrow(g))
    for (j in numeric_cols) {
      v <- suppressWarnings(as.numeric(as.character(g[[j]])))
      v <- v[is.finite(v)]
      row[[j]] <- if (length(v) > 0) as.character(mean(v)) else ""
    }
    out[[length(out) + 1]] <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

acr_csv_for <- function(rep, tab, window) {
  tb <- tryCatch(canhrActi::ggir.report.tables(rep), error = function(e) list())
  f <- tb[[if (identical(tab, "personsummary")) "personsummary" else "daysummary"]]
  if (length(f) == 0) return(NA_character_)
  hit <- f[grepl(paste0("_", window, "_"), names(f))]
  if (length(hit) == 0) hit <- f
  unname(hit[1])
}

acr_read_csv <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, check.names = FALSE, colClasses = "character"),
           error = function(e) NULL)
}

acr_table_panel <- function(reps, ids, sel, tab, window, ns, sort = NULL, dir = 1, sched = NULL) {
  keep <- if (is.null(sel)) seq_along(reps) else which(ids == sel)
  # by_type groups GGIR's own day rows by the schedule
  by_type <- identical(tab, "daytype")
  src <- if (by_type) "daysummary" else tab
  parts <- lapply(keep, function(i) acr_read_csv(acr_csv_for(reps[[i]], src, window)))
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (by_type) {
    parts <- if (sched_active(sched)) lapply(parts, acr_daytype_df, sched = sched) else list()
    parts <- parts[!vapply(parts, is.null, logical(1))]
  }
  tabs <- tags$div(class = "ov-tabs",
    lapply(names(ACR_TABS), function(k)
      tags$button(type = "button",
                  class = paste("ov-tab", if (identical(k, tab)) "is-active" else ""),
                  `data-actab` = k, role = "tab",
                  `aria-selected` = tolower(as.character(identical(k, tab))),
                  ACR_TABS[[k]])))
  if (length(parts) == 0) {
    why <- if (by_type && !sched_active(sched))
      "No schedule was in force for this run. Open Schedule in the rule bar, define kinds of day, then re-analyse."
    else "GGIR wrote no rows for this selection."
    return(tags$div(class = "wt-panel wtr-tables",
      tags$div(class = "wtr-head", tabs),
      tags$div(class = "wt-none", why)))
  }
  cols <- names(parts[[1]])
  d <- do.call(rbind, lapply(parts, function(p) p[, cols, drop = FALSE]))
  num <- vapply(d, acr_is_num, logical(1))
  if (!is.null(sort) && sort >= 1 && sort <= ncol(d) && nrow(d) > 1) {
    d <- d[wtr_order(as.character(d[[sort]]), dir), , drop = FALSE]
  }
  head <- acr_head_rows(names(d), sort, dir, which(num))
  tags$div(
    class = "wt-panel wtr-tables",
    tags$div(class = "wtr-head", tabs,
      ac_goto(vapply(head$groups, function(g) g[[1]], ""),
              vapply(head$groups, function(g) g[[2]], integer(1))),
      tags$span(class = "wtr-head-r",
        tags$span(class = "wtr-sub",
                  paste0(fmt_int(nrow(d)),
                         if (identical(tab, "personsummary"))
                           if (nrow(d) == 1) " recording · " else " recordings · "
                         else if (by_type)
                           if (nrow(d) == 1) " row · " else " rows · "
                         else if (nrow(d) == 1) " day · " else " days · ",
                         fmt_int(ncol(d)), " columns")),
        tags$span(class = "wtr-more", "scroll for the rest"),
        downloadButton(ns("raw_export"), "Export csv", class = "wt-btn wt-btn--secondary",
                       icon = NULL),
        # GGIR's study-level files; their person summary can have columns one recording's lacks
        tags$span(title = "GGIR's part-5 report over all recordings, as a zip",
          downloadButton(ns("raw_export_all"), "Export all", class = "wt-btn wt-btn--secondary",
                         icon = NULL)))),
    tags$div(class = "wtr-scroll",
      tags$table(class = paste("wt-table wtr-gt", if (length(head$groups) > 0) "acr-gt"),
        head$thead,
        tags$tbody(lapply(seq_len(nrow(d)), function(k)
          tags$tr(lapply(seq_along(d), function(j)
            tags$td(class = trimws(paste(if (j == 1) "wtr-stick" else "",
                                         if (num[j]) "r" else "")),
                    title = acr_cell_title(d[[j]][k], num[j]),
                    acr_cell(d[[j]][k], num[j]))))))))
  )
}

# COLUMN GROUPS

# GGIR's part-5 columns by family, read from the name as GGIR's own dictionary
# does (g.report.part5_dictionary.R); the first pattern that matches wins
ACR_FAMILIES <- c(
  "Recording" = "^(ID|filename|startday|lastHour|lastDate)$",
  "Day" = "^(weekday|calendar_date|window_number|window|start_end_window|daytype|day_type|days)$",
  "Valid days" = "^Nvalid",
  "Sleep period" = paste0("^(sleeponset|wakeup|sleepparam|night_number|daysleeper|cleaningcode|guider|",
                          "sleeplog_used|acc_available|N_atleast5minwakenight|sleep_efficiency|",
                          "tail_expansion_minutes|Ndaysleeper|Ncleaningcode|Nsleeplog_used|Nacc_available)"),
  "Non-wear" = "^nonwear_",
  "Time, day and sleep" = "^dur_(day|spt|day_spt)_min$",
  "Time, sleep period" = "^dur_spt_",
  "Time, day unbouted" = "^dur_day_.*_unbt_",
  "Time, day bouted" = "^dur_day_.*_bts_",
  "Time, day totals" = "^dur_day_total_",
  "Time" = "^dur_",
  "Acceleration, day and sleep" = "^ACC_(day|spt|day_spt)_mg",
  "Acceleration, sleep period" = "^ACC_spt_",
  "Acceleration, day unbouted" = "^ACC_day_.*_unbt_",
  "Acceleration, day bouted" = "^ACC_day_.*_bts_",
  "Acceleration, day totals" = "^ACC_day_total_",
  "Acceleration" = "^ACC_",
  "Most active minutes" = "^quantile_",
  "Bouts" = "^Nbouts_",
  "Blocks, sleep period" = "^Nblocks_spt_",
  "Blocks, day unbouted" = "^Nblocks_day_.*_unbt",
  "Blocks, day bouted" = "^Nblocks_day_.*_bts_",
  "Blocks, day totals" = "^Nblocks_day_total_",
  "Blocks" = "^Nblocks_",
  "Least and most active" = "^[LM][0-9]+",
  "Intensity gradient" = "^ig_",
  "Light exposure" = "^LUX_",
  "Fragmentation" = "^FRAG_",
  "Parameters" = "^(boutcriter|boutdur)[.]|^TR[LMV]i$|^GGIRversion$")

# How a person summary column was aggregated, from its suffix
ACR_AGG <- c(pla = "plain", wei = "weighted", WD = "weekdays", WE = "weekend")

acr_family <- function(cols) {
  vapply(cols, function(h) {
    base <- sub("[.][xy]$", "", h)
    agg <- NULL
    # GGIR keeps Nvaliddays_WD whole: it counts weekdays, it is not an aggregate
    if (!grepl("^Nvalid", base) && grepl("_(pla|wei|WD|WE)$", base)) {
      agg <- ACR_AGG[[sub("^.*_", "", base)]]
      base <- sub("_(pla|wei|WD|WE)$", "", base)
    }
    hit <- names(ACR_FAMILIES)[vapply(ACR_FAMILIES, grepl, logical(1), x = base)]
    fam <- if (length(hit) > 0) hit[1] else "Other"
    if (is.null(agg)) fam else paste(fam, "·", agg)
  }, character(1), USE.NAMES = FALSE)
}

# The families over the names, in GGIR's column order, so a family it splits
# is named twice. The pinned first column spans both rows, as Subject does.
acr_head_rows <- function(cols, sort, dir, right = integer(0)) {
  n <- length(cols)
  if (n < 2) return(list(thead = wtr_head_row(cols, sort, dir, right), groups = list()))
  runs <- rle(acr_family(cols[-1]))
  groups <- lapply(seq_along(runs$values), function(k) list(runs$values[k], runs$lengths[k]))
  first <- tagAppendAttributes(wtr_th(1, cols[1], sort, dir, right = 1 %in% right, stick = TRUE),
                               class = "acr-gh", rowspan = "2")
  list(
    thead = tags$thead(
      tags$tr(first, lapply(groups, function(g) tags$th(class = "acr-gg", colspan = g[[2]], g[[1]]))),
      tags$tr(lapply(2:n, function(i)
        tagAppendAttributes(wtr_th(i, cols[i], sort, dir, right = i %in% right), class = "acr-ch")))),
    groups = groups)
}

# Part 5 writes "NaN" into otherwise numeric columns, and as.numeric("NaN") is
# NA to is.na(); these tokens are dropped before a column is tested as numeric.
.RAW_NA_TOKENS <- c("NaN", "NA", "Inf", "-Inf", "")

acr_is_num <- function(v) {
  v <- trimws(as.character(v))
  v <- v[!is.na(v) & !(v %in% .RAW_NA_TOKENS)]
  if (length(v) == 0) return(FALSE)
  all(!is.na(suppressWarnings(as.numeric(v))))
}

# As GGIR reports it: three decimals, an empty cell where there is no value
acr_cell <- function(v, numeric_col = FALSE) {
  v <- trimws(as.character(v)[1])
  if (is.na(v) || v %in% .RAW_NA_TOKENS) return("")
  if (!isTRUE(numeric_col)) return(v)
  n <- suppressWarnings(as.numeric(v))
  if (!is.finite(n)) return("")
  if (n == round(n)) format(n, trim = TRUE) else fmt_dec(n, 3)
}

# GGIR's full value on hover, for a cell acr_cell() rounded
acr_cell_title <- function(v, numeric_col = FALSE) {
  if (!isTRUE(numeric_col)) return(NULL)
  v <- trimws(as.character(v)[1])
  n <- suppressWarnings(as.numeric(v))
  if (length(n) != 1 || !is.finite(n) || n == round(n, 3)) return(NULL)
  cell_title(v)
}

# EMPTY STATES

# Part 5 drops a recording whose nights part 4 could not place and GGIR says
# nothing; the page says why from $windows, which records every attempt.

# Before the first run: nothing is scored until Run analysis is pressed
acr_not_run <- function(rs, ids, sel, ns) {
  n <- length(ids)
  tagList(
    acr_pick_row(rs, ids, sel, ns),
    tags$div(class = "wt-panel acr-empty",
      tags$div(class = "acr-empty-t", "Not analysed yet"),
      tags$div(class = "acr-empty-s",
        paste0("Part 5 scores time use against the cut points in the rule bar. ",
               "Press Run analysis to score ",
               if (n == 1) "this recording." else paste(n, "recordings."))),
      tags$div(class = "acr-empty-m",
        "Reading the files already ran GGIR parts 1 to 3. This runs part 4 and part 5 and draws GGIR's report, which takes a few seconds per recording."),
      # their own ids: the rule bar above holds raw_run and raw_change, and the
      # bar's Run analysis is the one filled button
      tags$div(class = "acr-empty-a",
        actionButton(ns("raw_run_empty"), "Run analysis", class = "wt-btn wt-btn--secondary"),
        actionButton(ns("raw_change_empty"), "Change cut points", class = "wt-btn wt-btn--secondary"))))
}

acr_no_part5 <- function(tu, rs, ids, sel, ns) {
  st <- tu$status$state %||% "unknown"
  why <- switch(st,
    no_valid_night = "Part 4 found no usable night, so there is no day to place a window on.",
    no_windows = "Nights were found, but no window survived part 5's own gates.",
    error = "Part 5 stopped with an error.",
    "Part 5 produced no day summary.")
  msg <- tu$status$messages
  tagList(
    acr_pick_row(rs, ids, sel, ns),
    tags$div(class = "wt-panel acr-empty",
      tags$div(class = "acr-empty-t", "No part 5 result for this recording"),
      tags$div(class = "acr-empty-s", why),
      if (length(msg) > 0) tags$div(class = "acr-empty-m", paste(msg, collapse = " ")),
      if (is.data.frame(tu$windows) && nrow(tu$windows) > 0)
        tags$div(class = "acr-empty-m",
                 paste0(nrow(tu$windows), " window attempts are recorded; every one was rejected."))))
}

# SETTINGS

# Every field re-runs part 5 from the milestone; nothing re-reads the .gt3x.
# The fields are the ones that move part 5's answers, as the rule bar states.

# The cut point chooser: all 48 sets GGIR's CutPoints vignette lists. Picking
# one fills the three boxes and sets acc.metric (Aittasalo's 332 mg is MAD,
# not ENMO); editing a box by hand moves the chooser to Custom. Most sets
# define two bands, so the third box stays at GGIR's default and says so.

ACR_CUSTOM <- "custom"

# The metrics a recording carries; a set defined against another needs a re-read
acr_metrics_present <- function(r) {
  ms <- tryCatch(names(r$meta$metashort), error = function(e) NULL)
  if (is.null(ms)) character(0) else setdiff(ms, c("timestamp", "time"))
}

# The select's choices: a Custom entry, then one optgroup per population.
acr_cutpoint_choices <- function(have = NULL) {
  d <- canhrActi::raw.cutpoints(available = have)
  lab <- paste0(d$study,
                ifelse(nzchar(d$variant), paste0(" (", d$variant, ")"), ""),
                " · ",
                ifelse(nzchar(d$brand), paste0(d$brand, " · "), ""),
                d$location, " · ", d$metric,
                ifelse(is.na(d$light) | is.na(d$moderate) | is.na(d$vigorous), " · 2 bands", ""),
                # a metric the import never asks for is not offered, not "needs a re-read"
                ifelse(!d$metric %in% RAW_READ_HAS, " · not available here",
                       if (!is.null(have)) ifelse(d$available, "", " · needs a re-read") else ""))
  out <- list("Custom thresholds" = stats::setNames(ACR_CUSTOM, "Custom thresholds"))
  for (g in unique(d$group)) {
    i <- d$group == g
    out[[g]] <- stats::setNames(d$key[i], lab[i])
  }
  out
}

# The chosen row's key, stored on the part 5 result so a reload shows it
acr_cutpoint_key <- function(tu) {
  k <- acr_setting(tu, "cutpoint", NULL)
  if (is.null(k) || !nzchar(as.character(k)[1])) ACR_CUSTOM else as.character(k)[1]
}

ACR_FIELDS <- list(
  list(id = "thr_lig", key = "threshold.lig", label = "Light at", unit = "mg",
       note = "above this is light activity", min = 0, max = 500),
  list(id = "thr_mod", key = "threshold.mod", label = "Moderate at", unit = "mg",
       note = "above this counts as MVPA", min = 0, max = 1000),
  list(id = "thr_vig", key = "threshold.vig", label = "Vigorous at", unit = "mg",
       note = "the top band", min = 0, max = 3000),
  list(id = "bc_mvpa", key = "boutcriter.mvpa", label = "MVPA bout criterion", unit = "",
       note = "fraction of the window above threshold", min = 0, max = 1, step = 0.05),
  list(id = "bc_in", key = "boutcriter.in", label = "Inactive bout criterion", unit = "",
       note = "inactivity is allowed fewer breaks", min = 0, max = 1, step = 0.05)
)

acr_settings_panel <- function(tu, ns, have = NULL) {
  cur <- function(k, d) {
    v <- suppressWarnings(as.numeric(acr_setting(tu, k, d)))
    if (length(v) != 1 || !is.finite(v)) d else v
  }
  defaults <- c(threshold.lig = 40, threshold.mod = 100, threshold.vig = 400,
                boutcriter.mvpa = 0.8, boutcriter.in = 0.9)
  field <- function(f) {
    tags$div(class = "wt-field",
      tags$div(class = "wt-field-k", f$label),
      tags$div(class = "wt-num",
        numericInput(ns(paste0("acr_", f$id)), NULL, width = "100%",
                     value = cur(f$key, defaults[[f$key]]),
                     min = f$min, max = f$max, step = f$step %||% 1),
        if (nzchar(f$unit)) tags$span(class = "wt-unit", `aria-hidden` = "true", f$unit)),
      tags$div(class = "wt-field-n", f$note))
  }
  dur <- function(id, key, label, note, d, cls = NULL) {
    v <- acr_setting(tu, key, d)
    tags$div(class = paste("wt-field", cls),
      tags$div(class = "wt-field-k", label),
      textInput(ns(paste0("acr_", id)), NULL, width = "100%",
                value = paste(v, collapse = ", ")),
      tags$div(class = "wt-field-n", note))
  }

  sel <- acr_cutpoint_key(tu)

  tags$div(
    class = "wt-panel wt-settings-grid",
    tags$div(class = "wt-fields",
      tags$div(class = "wt-field wt-field--wide wt-select",
        tags$div(class = "wt-field-k", "Cut points"),
        # a native select (selectize = FALSE), like every other select here
        selectInput(ns("acr_cutpoint"), NULL, width = "100%", selectize = FALSE,
                    choices = acr_cutpoint_choices(have), selected = sel),
        tags$div(class = "wt-field-n",
          "sets the three thresholds and the metric they are read against")),
      # row 2 pairs each bout length with its own criterion
      lapply(ACR_FIELDS[1:3], field),
      dur("bd_mvpa", "boutdur.mvpa", "MVPA bout lengths",
          "minutes, longest first", c(10, 5, 1)),
      field(ACR_FIELDS[[4]]),
      dur("bd_in", "boutdur.in", "Inactive bout lengths",
          "minutes, longest first", c(30, 20, 10)),
      field(ACR_FIELDS[[5]])),
    # driven by the chooser, so a set's missing band shows before Re-run
    uiOutput(ns("acr_gap")),
    tags$div(class = "wt-settings-foot",
      tags$span(class = "wtr-more",
                paste("Changing any of these re-runs part 5 from the milestone.",
                      "The file is read again only for a cut point whose metric",
                      "it was not read with, which the list marks.")),
      tags$span(class = "wt-spacer"),
      actionButton(ns("acr_defaults"), "Back to GGIR's defaults", class = "wt-btn wt-btn--text")))
}

# The note for a study that did not define all three bands (21 of the 48 stop
# at moderate); the stand-in number is GGIR's default, not the study's.
acr_cutpoint_gap <- function(key) {
  if (identical(key, ACR_CUSTOM)) return("")
  r <- tryCatch(canhrActi::raw.cutpoints(), error = function(e) NULL)
  if (is.null(r)) return("")
  r <- r[r$key == key, , drop = FALSE]
  if (nrow(r) == 0) return("")
  miss <- c("light", "moderate", "vigorous")[is.na(c(r$light, r$moderate, r$vigorous))]
  if (length(miss) == 0) return("")
  d <- c(light = 40, moderate = 100, vigorous = 400)
  paste0(r$study, " defines no ", paste(miss, collapse = " or "), " threshold. ",
         paste(paste0(d[miss], " mg"), collapse = " and "),
         if (length(miss) > 1) " are" else " is",
         " GGIR's default standing in, not a number from that study.")
}

# The panel's numbers as a params override. NULL for an empty or unparseable
# field, so a half-typed box never re-analyses at some other value.
acr_form_params <- function(input) {
  num <- function(id) {
    v <- suppressWarnings(as.numeric(input[[paste0("acr_", id)]]))
    if (length(v) != 1 || !is.finite(v)) NULL else v
  }
  vec <- function(id) {
    s <- input[[paste0("acr_", id)]]
    if (is.null(s)) return(NULL)
    v <- suppressWarnings(as.numeric(strsplit(as.character(s), "[,;[:space:]]+")[[1]]))
    v <- v[is.finite(v)]
    if (length(v) != 3) NULL else v
  }
  out <- list(threshold.lig = num("thr_lig"), threshold.mod = num("thr_mod"),
              threshold.vig = num("thr_vig"), boutcriter.mvpa = num("bc_mvpa"),
              boutcriter.in = num("bc_in"), boutdur.mvpa = vec("bd_mvpa"),
              boutdur.in = vec("bd_in"))
  out <- out[!vapply(out, is.null, logical(1))]
  # The key travels with the numbers so the page can say which study they came
  # from; raw.timeuse() keeps unknown names such as cutpoint in $settings
  k <- input$acr_cutpoint
  if (!is.null(k) && nzchar(k)) {
    out$cutpoint <- k
    if (!identical(k, ACR_CUSTOM)) {
      cp <- tryCatch(canhrActi::raw.cutpoint(k), error = function(e) NULL)
      if (!is.null(cp)) out$acc.metric <- cp$acc.metric
    }
  }
  out
}

# Does the form differ from what produced the numbers on screen?
acr_settings_moved <- function(tu, input) {
  f <- acr_form_params(input)
  if (length(f) == 0) return(FALSE)
  any(vapply(names(f), function(k) {
    cur <- acr_setting(tu, k, NULL)
    if (is.null(cur)) return(TRUE)
    a <- suppressWarnings(as.numeric(cur)); b <- suppressWarnings(as.numeric(f[[k]]))
    # the cut point key and the metric are words
    if (anyNA(a) || anyNA(b)) return(!identical(as.character(cur), as.character(f[[k]])))
    !isTRUE(all.equal(a, b))
  }, logical(1)))
}
