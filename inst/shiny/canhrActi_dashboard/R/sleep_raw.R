# The Sleep page, drawn for raw recordings: GGIR part 4 nights from the part
# 3 bouts, placed by a guider (a diary or HDCZA). The figure is GGIR's own
# g.part4 drawing (raw.ggir.sleepplot()), which writes no title or caption.
# Cleaned and Full are GGIR's two report passes and differ in rows and in
# columns: cleaned drops error_onset, error_wake, error_dur and the guider_*
# means. Pure drawing, like activity_raw.R.

slr_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || !is.finite(x)) default else x
}

# part 4 settings ride on the night table's attribute, not on the frame.
slr_attr <- function(nights) {
  a <- attr(nights, "canhrActi")
  if (is.null(a)) list() else a
}
slr_set <- function(nights, key, default = NA) {
  s <- slr_attr(nights)$settings
  v <- if (is.null(s)) NULL else s[[key]]
  if (is.null(v)) default else v
}

slr_ok <- function(r) {
  is.list(r) && is.data.frame(r$nightsummary_cleaned)
}

# The sib definition, GGIR's paste0("T", timethreshold, "A", anglethreshold).
# Read from the data: a vector of thresholds gives a row per definition.
slr_params <- function(nights) {
  if (!is.data.frame(nights) || !"sleepparam" %in% names(nights)) return(character(0))
  unique(as.character(nights$sleepparam))
}

# Every distinct guider over the kept nights, with counts
slr_guiders <- function(d) {
  if (!is.data.frame(d) || !"guider" %in% names(d) || nrow(d) == 0) return("")
  g <- table(as.character(d$guider))
  g <- g[order(-g)]
  if (length(g) == 1) names(g)[1]
  else paste(paste0(names(g), " ", as.integer(g)), collapse = " · ")
}

# RULE BAR

# Six welded boxes, the Activity bar's shape

slr_g <- function(key, plain, ...) {
  tags$span(class = "wt-rule-g",
    tags$span(class = "wt-rule-k", key),
    tags$span(class = "wt-rule-v", title = plain, ...))
}
slr_q <- function(x) tags$span(class = "wt-rule-q", x)

slr_rule <- function(nights, rep, ns, ran = TRUE, form = NULL, stale = FALSE) {
  # form: the settings of the run on screen, or the panel's before a run
  f <- form %||% SLR_DEFAULTS
  sp <- slr_params(nights)
  spt <- if (length(sp) == 0) paste0("T", f$timethreshold %||% 5, "A", f$anglethreshold %||% 5)
         else paste(sp, collapse = ", ")
  dolog <- if (is.null(slr_attr(nights)$dolog)) !is.null(f$diary) else isTRUE(slr_attr(nights)$dolog)
  cleaned <- if (slr_ok(rep)) rep$nightsummary_cleaned else NULL
  full <- if (slr_ok(rep)) rep$nightsummary_full else NULL
  gd <- slr_guiders(cleaned)
  if (!nzchar(gd)) gd <- as.character(f$HASPT.algo %||% "HDCZA")[1]
  nfull <- if (is.data.frame(full)) nrow(full) else 0L
  # raw_sleep_report.R's deletion rule, verbatim; it explains the kept count
  rule <- if (dolog) "drop cleaningcode above 0, diary nights only"
          else "drop cleaningcode above 1, and NotWorn"
  incl <- slr_num(slr_set(nights, "includenightcrit"), f$includenightcrit %||% 16)
  win <- as.character(slr_set(nights, "sleepwindowType", f$sleepwindowType %||% "SPT"))[1]
  diary <- slr_attr(nights)$sleeplog_name
  if (is.null(diary) && is.null(slr_attr(nights)$dolog)) diary <- f$diary$name
  ncor <- if (is.data.frame(cleaned) && "guider_corrected" %in% names(cleaned))
            sum(suppressWarnings(as.numeric(cleaned$guider_corrected)) == 1, na.rm = TRUE) else 0
  # the night count is in the title, so the bar keeps one row after a run
  gd_plain <- if (nfull > 0 && !grepl("·", gd)) paste0(gd, " on all ", nfull, " nights") else gd

  tags$div(
    class = "wt-panel wt-rule",
    role = "group",
    `aria-label` = "The parameters these sleep numbers were produced with",
    tags$span(
      class = "wt-rule-ps",
      slr_g("Sleep definition", spt, tags$b(spt)),
      slr_g("Guider", cell_title(gd_plain),
            tags$b(gd),
            if (ncor > 0) slr_q(paste0(" · ", ncor, " corrected"))),
      slr_g("Diary", if (is.null(diary)) "None" else as.character(diary),
            tags$b(if (is.null(diary)) "None" else as.character(diary))),
      slr_g("Night rule", rule, tags$b(rule)),
      slr_g("Valid night", paste0(fmt_dec(incl, 0), " h of 24"),
            tags$b(fmt_dec(incl, 0)), " h of 24"),
      slr_g("Window", win, tags$b(win))),
    if (isTRUE(ran) && isTRUE(stale)) stale_note("wt"),
    tags$span(class = "wt-rule-actions",
      actionButton(ns("sraw_change"), "Change", class = "wt-btn wt-btn--secondary"),
      tags$span(class = "slr-export-wrap",
        tags$span(class = paste(c("wt-btn wt-btn--secondary slr-export-btn", if (!isTRUE(ran)) "is-quiet"),
                                collapse = " "),
                  "Export", tags$span(class = "sl-car", `aria-hidden` = "true", HTML("&#9660;"))),
        slr_export_menu(ns, rep, ready = isTRUE(ran))),
      if (isTRUE(ran))
        actionButton(ns("sraw_reanalyse"), "Re-run",
                     class = paste(run_button_class("wt", TRUE, stale), "sl-run"))
      else actionButton(ns("sraw_run"), "Run analysis",
                        class = paste(run_button_class("wt", FALSE), "sl-run"))))
}

# The menu names the files and their shape; these are GGIR's own four names
slr_export_menu <- function(ns, rep, ready = TRUE) {
  if (!isTRUE(ready)) {
    return(tags$div(class = "slr-exportmenu", style = "display:none;",
      tags$div(class = "slr-ei slr-ei--wait", "Run the analysis first")))
  }
  sz <- function(d) if (is.data.frame(d)) paste0(nrow(d), " × ", ncol(d)) else ""
  item <- function(id, name, d) {
    tags$div(class = "slr-ei",
      downloadLink(ns(id), label = tagList(
        tags$span(class = "slr-fn", name),
        tags$span(class = "slr-sz", sz(d)))))
  }
  ok <- slr_ok(rep)
  tags$div(class = "slr-exportmenu", style = "display:none;",
    tags$div(class = "slr-ei slr-ei--all",
             downloadLink(ns("sraw_dl_all"), "Download all four")),
    tags$div(class = "slr-ei-sep"),
    item("sraw_dl_nc", "part4_nightsummary_sleep_cleaned.csv", if (ok) rep$nightsummary_cleaned),
    item("sraw_dl_nf", "part4_nightsummary_sleep_full.csv",    if (ok) rep$nightsummary_full),
    item("sraw_dl_pc", "part4_summary_sleep_cleaned.csv",      if (ok) rep$personsummary_cleaned),
    item("sraw_dl_pf", "part4_summary_sleep_full.csv",         if (ok) rep$personsummary_full))
}

# PART 5

# GGIR's part-4 report takes window, nonwear_perc_spt and ACC_spt_mg from
# part 5. Its settings are the Activity page's defaults plus this page's.
slr_part5_params <- function(s) {
  pp <- canhrActi::raw.params()
  for (k in c("HASPT.algo", "timethreshold", "anglethreshold", "includenightcrit", "sleepwindowType")) {
    v <- s[[k]]
    if (!is.null(v) && !identical(as.character(v), as.character(pp[[k]]))) pp[[k]] <- v
  }
  if (!is.null(s$diary)) pp$loglocation <- s$diary$path
  pp
}

# raw.timeuse() sorts the bout lengths and takes desiredtz from part 1
slr_part5_same <- function(tu, pp) {
  a <- if (inherits(tu, "canhrActi_raw_timeuse")) tu$settings$params else NULL
  b <- tryCatch(suppressWarnings(canhrActi:::.raw.params.check(pp)), error = function(e) NULL)
  if (!is.list(a) || !is.list(b)) return(FALSE)
  norm <- function(p) {
    p$desiredtz <- NULL
    for (k in c("boutdur.mvpa", "boutdur.in", "boutdur.lig")) {
      if (!is.null(p[[k]])) p[[k]] <- sort(as.numeric(p[[k]]), decreasing = TRUE)
    }
    p[order(names(p))]
  }
  identical(norm(a), norm(b))
}

# GGIR writes an ms5 file only in the "ok" state
slr_part5_day <- function(tu) {
  if (inherits(tu, "canhrActi_raw_timeuse") && identical(tu$status$state, "ok")) tu$daysummary else NULL
}

# EXPORT

# One report over every recording, in GGIR's milestone file order, written
# by GGIR's writer into dir/results and dir/results/QC
slr_write_report <- function(nights, part5, dir) {
  ok <- vapply(nights, function(n) is.data.frame(n) && nrow(n) > 0, logical(1))
  nights <- unname(nights[ok])
  part5 <- unname(part5[ok])
  if (length(nights) > 0) {
    fn <- vapply(nights, function(n) if (length(n$filename)) as.character(n$filename[1]) else "", "")
    o <- order(fn)
    rep <- canhrActi::raw.sleep.report(nights[o], part5 = part5[o])
    canhrActi::write.raw.sleep.report(rep, dir)
  }
  c(nightsummary_cleaned = file.path(dir, "results", "part4_nightsummary_sleep_cleaned.csv"),
    nightsummary_full = file.path(dir, "results", "QC", "part4_nightsummary_sleep_full.csv"),
    personsummary_cleaned = file.path(dir, "results", "part4_summary_sleep_cleaned.csv"),
    personsummary_full = file.path(dir, "results", "QC", "part4_summary_sleep_full.csv"))
}

# FIGURES

# Six, since sleep has four quantities: how much, how broken, how regular,
# how many nights survived. Tiles 3 to 6 are means over the person rows.

slr_fig <- function(value, unit, label, state = NULL) {
  ink <- switch(state %||% "", bad = "var(--ovl-danger)", warn = "var(--ovl-warn)", NULL)
  # the unit is a sibling of the number, as on the counts tiles
  tags$div(class = "wt-fig",
    tags$div(tags$span(class = "wt-fig-n", style = if (!is.null(ink)) paste0("color:", ink, ";"), value),
             if (!is.null(unit)) tags$span(class = "wt-fig-u", unit)),
    tags$div(class = "wt-fig-l", label))
}

# The person columns carry the sib definition: SptDuration_AD_T5A5_mn
slr_pmean <- function(persons, stem, sp) {
  if (!is.data.frame(persons) || nrow(persons) == 0) return(NA_real_)
  nm <- paste0(stem, "_AD_", sp, "_mn")
  if (!nm %in% names(persons)) {
    cand <- grep(paste0("^", stem, "_AD_.*_mn$"), names(persons), value = TRUE)
    if (length(cand) == 0) return(NA_real_)
    nm <- cand[1]
  }
  v <- suppressWarnings(as.numeric(as.character(persons[[nm]])))
  v <- v[is.finite(v)]
  if (length(v) == 0) NA_real_ else mean(v)
}

slr_figures <- function(reps, nrec) {
  cleaned <- do.call(rbind, lapply(reps, function(r) if (slr_ok(r)) r$nightsummary_cleaned))
  full <- do.call(rbind, lapply(reps, function(r) if (slr_ok(r)) r$nightsummary_full))
  pers <- do.call(rbind, lapply(reps, function(r) if (slr_ok(r)) r$personsummary_cleaned))
  nk <- if (is.data.frame(cleaned)) nrow(cleaned) else 0L
  nf <- if (is.data.frame(full)) nrow(full) else 0L
  sp <- if (is.data.frame(cleaned) && "sleepparam" %in% names(cleaned) && nrow(cleaned) > 0)
          as.character(cleaned$sleepparam)[1] else "T5A5"
  spt <- slr_pmean(pers, "SptDuration", sp)
  slp <- slr_pmean(pers, "SleepDurationInSpt", sp)
  waso <- slr_pmean(pers, "WASO", sp)
  sri <- slr_pmean(pers, "SleepRegularityIndex1", sp)
  # SRI exists only where the day pair had over 16 of 24 hours valid, so the
  # count of recordings that have one is part of the number
  nsri <- 0L
  if (is.data.frame(pers)) {
    cand <- grep("^SleepRegularityIndex1_AD_.*_mn$", names(pers), value = TRUE)
    if (length(cand) > 0) nsri <- sum(is.finite(suppressWarnings(as.numeric(as.character(pers[[cand[1]]])))))
  }
  tags$div(class = "wt-panel wt-figs",
    slr_fig(fmt_int(nrec), NULL, "Recordings"),
    tags$div(class = "wt-fig-rule"),
    slr_fig(fmt_int(nk), paste0("of ", fmt_int(nf)), "Nights kept",
            state = if (nk < nf) "warn"),
    tags$div(class = "wt-fig-rule"),
    slr_fig(fmt_dec(spt, 2), "h", "Sleep period"),
    tags$div(class = "wt-fig-rule"),
    slr_fig(fmt_dec(slp, 2), "h", "Asleep in it"),
    tags$div(class = "wt-fig-rule"),
    slr_fig(fmt_dec(waso * 60, 0), "min", "WASO"),
    tags$div(class = "wt-fig-rule"),
    slr_fig(fmt_dec(sri, 1), paste0("on ", nsri, " of ", nrec), "Sleep regularity"))
}

# CHART PANEL

# Two choosers over the figure and a line naming the recording shown; every
# sleep plot draws one recording.

SLR_CHARTS <- c(nights = "Sleep nights", regularity = "Sleep regularity")

slr_chart_panel <- function(rs, ids, sel, chart, ns) {
  i <- if (is.null(sel)) 1L else match(sel, ids)
  if (is.na(i)) i <- 1L
  nm <- ovr_name(rs[[i]], ids[i])
  # native selects (selectize = FALSE), like every other select here
  who <- stats::setNames(ids, vapply(seq_along(ids),
                                     function(k) ovr_name(rs[[k]], ids[k]), ""))
  tags$div(class = "wt-panel slr-chart",
    tags$div(class = "slr-chart-h",
      tags$span(class = "slr-pick",
        selectInput(ns("slchart_set"), NULL, width = "100%", selectize = FALSE,
                    choices = stats::setNames(names(SLR_CHARTS), unname(SLR_CHARTS)),
                    selected = chart)),
      tags$span(class = "slr-pick slr-pick--who",
        selectInput(ns("slwho_set"), NULL, width = "100%", selectize = FALSE,
                    choices = who, selected = ids[i])),
      tags$span(class = "slr-showing",
                "showing ", tags$b(nm),
                " · this plot draws one recording at a time")),
    tags$div(class = "slr-chart-b", plotOutput(ns("sraw_chart"), height = "auto")))
}

# TABLES

.SLR_NA <- c("NaN", "NA", "Inf", "-Inf", "")

# Part 4's frames are not typed (GGIR round-trips through a character matrix),
# so numbers are detected by parsing, or SptDuration sorts 11.35 before 9.27.
slr_is_num <- function(v) {
  v <- trimws(as.character(v))
  v <- v[!is.na(v) & !(v %in% .SLR_NA)]
  if (length(v) == 0) return(FALSE)
  all(!is.na(suppressWarnings(as.numeric(v))))
}
slr_cell <- function(v, num, digits = 3L) {
  v <- trimws(as.character(v))
  v[is.na(v) | v %in% .SLR_NA] <- ""
  if (!num) return(v)
  x <- suppressWarnings(as.numeric(v))
  out <- ifelse(is.na(x), "", fmt_dec(x, digits))
  out
}
# A column of whole numbers prints without decimals; decided on the full column
slr_digits <- function(v) {
  x <- suppressWarnings(as.numeric(trimws(as.character(v))))
  x <- x[is.finite(x)]
  if (length(x) > 0 && all(abs(x - round(x)) < 1e-9)) 0L else 3L
}

# The night bar: the scored sleep period filled inside the guider window as
# an outline. A default raw run has no efficiency column to shade by.
slr_bar <- function(onset, wake, g0, g1, title = NULL) {
  p <- function(h) max(0, min(100, ((h - 12) / 24) * 100))
  f0 <- p(onset); f1 <- p(wake); o0 <- p(g0); o1 <- p(g1)
  tags$div(class = "slr-nb", title = title,
    if (is.finite(f0) && is.finite(f1) && f1 > f0)
      tags$span(class = "slr-nb-f", style = sprintf("left:%.2f%%;width:%.2f%%", f0, f1 - f0)),
    if (is.finite(o0) && is.finite(o1) && o1 > o0)
      tags$span(class = "slr-nb-g", style = sprintf("left:%.2f%%;width:%.2f%%", o0, o1 - o0)))
}

slr_night_head <- function() {
  tags$div(
    tags$div(class = "slr-nh-k", "Each night"),
    tags$div(class = "slr-nh-a",
      lapply(c(12, 18, 24, 30, 36), function(h)
        tags$span(style = sprintf("left:%.1f%%", ((h - 12) / 24) * 100),
                  sprintf("%02d:00", h %% 24)))))
}

slr_table_panel <- function(rep, tab, pass, sort, dir, ns) {
  ok <- slr_ok(rep)
  d <- if (!ok) NULL else rep[[paste0(if (identical(tab, "persons")) "personsummary_" else "nightsummary_", pass)]]
  nk <- if (ok) nrow(rep$nightsummary_cleaned) else 0L
  nf <- if (ok) nrow(rep$nightsummary_full) else 0L
  npc <- if (ok) nrow(rep$personsummary_cleaned) else 0L
  nights <- identical(tab, "nights")

  head <- function(sub = NULL) tags$div(class = "slr-thead",
    tags$div(class = "slr-nav",
      tags$button(type = "button", `data-sltab` = "nights",
                  class = paste("slr-navb", if (identical(tab, "nights")) "is-on"),
                  "Nights ", tags$span(class = "slr-n", fmt_int(if (identical(pass, "full")) nf else nk))),
      tags$button(type = "button", `data-sltab` = "persons",
                  class = paste("slr-navb", if (identical(tab, "persons")) "is-on"),
                  "Recordings ", tags$span(class = "slr-n", fmt_int(npc)))),
    sub,
    tags$span(class = "slr-thead-r",
      tags$span(class = "ovr-seg",
        lapply(c("cleaned", "full"), function(p)
          tags$button(type = "button", `data-slpass` = p,
                      class = paste("ovr-seg-b", if (identical(pass, p)) "is-on"),
                      if (identical(p, "cleaned")) "Cleaned" else "Full"))),
      tags$span(class = "acr-info", "scroll for the rest")))

  if (!is.data.frame(d) || nrow(d) == 0) {
    return(tags$div(class = "wt-panel slr-table", head(),
                    tags$div(class = "wt-none", "Part 4 wrote no rows for this selection.")))
  }

  # page is GGIR's plot paginator, exported but not shown
  show <- setdiff(names(d), if (nights) "page" else character(0))
  num <- vapply(d[show], slr_is_num, logical(1))
  ord <- seq_len(nrow(d))
  if (!is.null(sort) && sort >= 1 && sort <= length(show)) {
    key <- d[[show[sort]]]
    key <- if (num[sort]) suppressWarnings(as.numeric(as.character(key))) else as.character(key)
    ord <- order(key, decreasing = identical(dir, -1), na.last = TRUE)
  }
  d <- d[ord, , drop = FALSE]
  cap <- min(nrow(d), 300L)
  body <- d[seq_len(cap), , drop = FALSE]

  th <- lapply(seq_along(show), function(k) {
    on <- !is.null(sort) && identical(sort, k)
    tags$th(class = if (num[k]) "slr-r" else NULL, `data-slsort` = k,
            show[k], if (on) tags$span(class = "slr-ar", HTML(if (identical(dir, -1)) "&#9660;" else "&#9650;")))
  })

  cells <- lapply(show, function(cn) {
    isn <- slr_is_num(d[[cn]])
    slr_cell(body[[cn]], isn, if (isn) slr_digits(d[[cn]]) else 3L)
  })
  names(cells) <- show
  gcol <- function(cn) if (cn %in% names(body)) suppressWarnings(as.numeric(as.character(body[[cn]]))) else rep(NA_real_, nrow(body))
  on_v <- gcol("sleeponset"); wk_v <- gcol("wakeup")
  g0_v <- gcol("guider_onset"); g1_v <- gcol("guider_wakeup")
  ts0 <- if ("sleeponset_ts" %in% names(body)) as.character(body$sleeponset_ts) else rep("", nrow(body))
  ts1 <- if ("wakeup_ts" %in% names(body)) as.character(body$wakeup_ts) else rep("", nrow(body))
  cc <- gcol("cleaningcode")
  # GGIR's cleaning criterion: 1 with a diary, 2 without
  crit <- if (isTRUE(attr(rep, "canhrActi")$settings$only.use.sleeplog)) 1 else 2

  rows <- lapply(seq_len(nrow(body)), function(r) {
    dropped <- nights && identical(pass, "full") && is.finite(cc[r]) && cc[r] >= crit
    tags$tr(class = if (dropped) "slr-drop" else NULL,
      # held to 136px on Nights, where the night bar is pinned at 152px
      tags$td(class = "slr-stick",
              if (nights) tags$div(class = "slr-id", cells[[1]][r]) else cells[[1]][r]),
      if (nights) tags$td(class = "slr-p1",
        slr_bar(on_v[r], wk_v[r], g0_v[r], g1_v[r],
                title = if (nzchar(ts0[r])) paste(ts0[r], "to", ts1[r]))),
      lapply(seq_along(show)[-1], function(k)
        tags$td(class = if (num[k]) "slr-r" else NULL, cells[[k]][r])))
  })

  tags$div(class = "wt-panel slr-table",
    head(tags$span(class = "wtr-sub",
      tags$b(paste0(fmt_int(nrow(d)), if (nights) " nights" else " recordings")),
      " · ", fmt_int(length(show)), " columns",
      if (nights) " · page column not shown")),
    tags$div(class = "slr-scroll",
      tags$table(class = "wt-table slr-gt",
        tags$thead(tags$tr(
          tags$th(class = "slr-stick", `data-slsort` = 1, show[1]),
          if (nights) tags$th(class = "slr-p1", slr_night_head()),
          th[-1])),
        tags$tbody(rows))),
    if (nrow(d) > cap)
      tags$div(class = "wt-note", paste0("… ", fmt_int(nrow(d) - cap),
        " more rows. The export carries every one.")))
}

slr_not_run <- function(n, ns) {
  tags$div(class = "wt-panel acr-empty",
    tags$div(class = "acr-empty-t", "Not summarised yet"),
    tags$div(class = "acr-empty-s",
      paste0("Part 4 scores the nights against the guider in the rule bar. ",
             "Press Run analysis to summarise ",
             if (n == 1) "this recording." else paste(n, "recordings."))),
    tags$div(class = "acr-empty-m",
      paste("Reading the files already ran GGIR parts 1 to 3. This runs part 4, a second or two",
            "per recording, and part 5 for the three columns GGIR's report takes from it, about",
            "a second per day recorded. Part 3 runs again only for a new guider or bout.")),
    # own ids: the rule bar above draws sraw_run and sraw_change
    tags$div(class = "acr-empty-a",
      actionButton(ns("sraw_run_empty"), "Run analysis", class = "wt-btn wt-btn--secondary"),
      actionButton(ns("sraw_change_empty"), "Change settings", class = "wt-btn wt-btn--secondary")))
}

# SETTINGS

# The parameters the rule bar shows. The guider and the bout definition re-run
# part 3 on the stored recording; the rest re-run part 4. HorAngle needs a hip
# recording and MotionWare an epoch of 15 s or more, so neither is offered.
SLR_GUIDERS <- c("HDCZA", "HLRB", "LowAcc", "NotWorn")
SLR_DEFAULTS <- list(HASPT.algo = "HDCZA", timethreshold = 5, anglethreshold = 5,
                     includenightcrit = 16, sleepwindowType = "SPT", diary = NULL)

slr_settings_panel <- function(ns) {
  field <- function(label, control, note, note_id = NULL) {
    tags$div(class = "wt-field",
      tags$div(class = "wt-field-k", label),
      control,
      tags$div(class = "wt-field-n", id = note_id, note))
  }
  num <- function(id, value, unit, ...) {
    tags$div(class = "wt-num",
      numericInput(ns(id), NULL, value = value, width = "100%", ...),
      tags$span(class = "wt-unit", `aria-hidden` = "true", unit))
  }
  sel <- function(id, choices, selected) {
    tags$div(class = "wt-select",
      selectInput(ns(id), NULL, choices = choices, selected = selected,
                  width = "100%", selectize = FALSE))
  }

  tags$div(
    class = "wt-panel wt-settings-grid",
    tags$div(class = "wt-fields",
      field("Guider", sel("sraw_haspt", SLR_GUIDERS, "HDCZA"),
            "sets the window each night is scored in"),
      field("Still for", num("sraw_time", 5, "min", min = 1, max = 60, step = 1),
            "the T in T5A5"),
      field("Angle under", num("sraw_angle", 5, "deg", min = 1, max = 90, step = 1),
            "the A in T5A5, a change in z-angle"),
      field("Valid night", num("sraw_incl", 16, "h", min = 0, max = 24, step = 1),
            "hours of valid data a night needs"),
      field("Window", sel("sraw_window", c("SPT" = "SPT", "Time in bed" = "TimeInBed"), "SPT"),
            "time in bed needs a diary", note_id = ns("sraw_window_note")),
      field("Diary",
            tags$div(class = "slr-diary",
              # a button rather than the file input's own, which the legacy .btn rule restyles
              tags$button(type = "button", class = "wt-btn wt-btn--secondary",
                          onclick = sprintf("document.getElementById('%s').click(); return false;",
                                            ns("sraw_diary_file")),
                          "Attach csv"),
              actionButton(ns("sraw_diary_clear"), "Remove", class = "wt-btn wt-btn--text",
                           style = "display: none;"),
              tags$div(style = "display: none;",
                       fileInput(ns("sraw_diary_file"), NULL, accept = c(".csv", "text/csv")))),
            "GGIR's sleeplog csv", note_id = ns("sraw_diary_note"))),
    tags$div(class = "wt-settings-foot",
      tags$span(class = "wtr-more",
                "A new guider or bout re-runs part 3 on the stored recording. The file is not read again."),
      tags$span(class = "wt-spacer"),
      actionButton(ns("sraw_defaults"), "Back to GGIR's defaults", class = "wt-btn wt-btn--text")))
}

# The panel as the run reads it. A half-typed box keeps the default.
slr_form_params <- function(input, diary = NULL) {
  num <- function(v, d) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) != 1 || !is.finite(v)) d else v
  }
  algo <- as.character(input$sraw_haspt %||% "HDCZA")[1]
  list(HASPT.algo = if (algo %in% SLR_GUIDERS) algo else "HDCZA",
       timethreshold = num(input$sraw_time, 5),
       anglethreshold = num(input$sraw_angle, 5),
       includenightcrit = num(input$sraw_incl, 16),
       # without a diary GGIR reverts time in bed to SPT
       sleepwindowType = if (is.null(diary)) "SPT" else as.character(input$sraw_window %||% "SPT")[1],
       diary = diary)
}

# Does the panel differ from the settings of the run on screen?
slr_settings_moved <- function(run, form) {
  if (is.null(run)) return(FALSE)
  keys <- c("HASPT.algo", "timethreshold", "anglethreshold", "includenightcrit", "sleepwindowType")
  any(vapply(keys, function(k) !identical(as.character(run[[k]]), as.character(form[[k]])), logical(1))) ||
    !identical(run$diary$path, form$diary$path)
}
