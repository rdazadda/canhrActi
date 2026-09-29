# Circadian: the loaded page. The result frame is built here for both the
# table and the CSV export.

# Every column of the CSV export, one row per recording.
circadian_results_df <- function(res) {
  df <- data.frame(
    subject_id = sapply(res, function(r) r$subject_id),
    file_name = sapply(res, function(r) r$name),
    L5 = sapply(res, function(r) r$L5),
    L5_start = sapply(res, function(r) r$L5_start),
    M10 = sapply(res, function(r) r$M10),
    M10_start = sapply(res, function(r) r$M10_start),
    RA = sapply(res, function(r) r$RA),
    IS = sapply(res, function(r) r$IS),
    IV = sapply(res, function(r) r$IV),
    phi = sapply(res, function(r) r$phi),
    # the extended multi-harmonic cosinor's, named ext_* as the workbook names
    # them, not the single-component cosinor's
    ext_mesor = sapply(res, function(r) r$mesor),
    ext_amplitude = sapply(res, function(r) r$amplitude),
    ext_acrophase = sapply(res, function(r) r$acrophase),
    ext_acrophase_time = sapply(res, function(r) r$acrophase_time),
    ext_r_squared = sapply(res, function(r) r$r_squared),
    ext_pattern_type = sapply(res, function(r) r$pattern_type),
    ext_is_bimodal = sapply(res, function(r) r$is_bimodal),
    h12_amplitude = sapply(res, function(r) r$h12_amplitude),
    h12_power = sapply(res, function(r) r$h12_power),
    ext_r_squared_single = sapply(res, function(r) r$r_squared_single),
    ext_r_squared_improvement = sapply(res, function(r) r$r_squared_improvement),
    coverage_percent = sapply(res, function(r) r$coverage_percent),
    stringsAsFactors = FALSE
  )
  df
}

# DRAWING

# The CSV export's column groups. A column not listed here still reaches the
# page, under "Other".
cr_groups <- function() {
  list(
    list("Recording", c("file_name", "coverage_percent")),
    list("Rest and activity windows", c("L5", "L5_start", "M10", "M10_start", "RA")),
    # phi is the lag-1h autocorrelation, nonparametric
    list("Day to day", c("IS", "IV", "phi")),
    list("Extended cosinor", c("ext_mesor", "ext_amplitude", "ext_acrophase",
                               "ext_acrophase_time", "ext_r_squared")),
    list("Shape of the day", c("ext_pattern_type", "ext_is_bimodal",
                               "h12_amplitude", "h12_power",
                               "ext_r_squared_single", "ext_r_squared_improvement"))
  )
}

# Levels are in the input's unit; ratios, indices and clock times are not
cr_level_cols <- c("L5", "M10", "L1", "M1", "ext_mesor", "ext_amplitude", "h12_amplitude")

# The column header on screen; the file keeps the unambiguous name. A column
# without an entry falls back to its own name. A level names its unit.
cr_label <- function(h, unit = NULL) {
  labs <- c(
    file_name = "File", coverage_percent = "Coverage %",
    L5 = "L5", L5_start = "L5 onset", M10 = "M10", M10_start = "M10 onset", RA = "RA",
    IS = "IS", IV = "IV", phi = "Autocorr 1h",
    ext_mesor = "MESOR", ext_amplitude = "Amplitude", ext_acrophase = "Acrophase",
    ext_acrophase_time = "Acrophase time", ext_r_squared = "R squared",
    ext_pattern_type = "Pattern", ext_is_bimodal = "Bimodal",
    h12_amplitude = "12h amplitude", h12_power = "12h power %",
    ext_r_squared_single = "R squared, single", ext_r_squared_improvement = "R squared gain"
  )
  out <- unname(labs[h])
  if (is.na(out)) out <- h
  if (!is.null(unit) && h %in% cr_level_cols) out <- paste0(out, " (", unit, ")")
  out
}

# Takes the column name. Sized on the label with the longer unit, so the
# counts and raw tables keep one geometry.
cr_col_width <- function(h) {
  if (identical(h, "file_name")) return(190L)
  n <- nchar(cr_label(h, "counts"))
  if (n > 18) return(168L)
  if (n > 12) return(140L)
  if (n > 7) return(116L)
  92L
}

cr_is_num <- function(v) grepl("^-?[0-9,]+(\\.[0-9]+)?$", trimws(as.character(v)))

# One column as shown. Every number takes the most decimals its column holds,
# so the points line up; commas from 10,000, as on the other pages.
cr_col_text <- function(v) {
  s <- trimws(as.character(v))
  miss <- is.na(s) | !nzchar(s)
  out <- ifelse(miss, "–", s)
  num <- !miss & cr_is_num(s)
  if (!any(num)) return(out)
  d <- max(ifelse(grepl("\\.", s[num]), nchar(sub("^.*\\.", "", s[num])), 0L))
  n <- as.numeric(gsub(",", "", s[num]))
  out[num] <- vapply(n, function(x) formatC(x, format = "f", digits = d,
                     big.mark = if (abs(x) >= 10000) "," else ""), character(1))
  out
}

cr_hhmm <- function(x) {
  m <- regmatches(x, regexec("^(\\d+):(\\d+)", as.character(x)))[[1]]
  if (length(m) < 3) return(NA_real_)
  as.numeric(m[2]) * 60 + as.numeric(m[3])
}

# One strip per recording, midnight to midnight, with the least active five
# hours and the most active ten; where they sit is the rhythm.
cr_clock_strip <- function(o) {
  parts <- list()
  band <- function(start_txt, hours, cls) {
    s <- cr_hhmm(start_txt)
    if (is.na(s)) return(invisible(NULL))
    left <- s / 1440
    w <- (hours * 60) / 1440
    piece <- function(l, ww) tags$span(class = paste0("cr-cs-b ", cls),
      style = sprintf("left: %.2f%%; width: %.2f%%;", l * 100, ww * 100))
    if (left + w <= 1) parts[[length(parts) + 1]] <<- piece(left, w)
    else {
      parts[[length(parts) + 1]] <<- piece(left, 1 - left)
      parts[[length(parts) + 1]] <<- piece(0, left + w - 1)
    }
  }
  band(o[["L5_start"]], 5, "l5")     # drawn first, so M10 sits on top if they touch
  band(o[["M10_start"]], 10, "m10")

  tags$span(class = "cr-cs",
    title = paste0(o[["subject_id"]], " · least active 5 h from ", o[["L5_start"]],
                   " · most active 10 h from ", o[["M10_start"]]),
    parts,
    tags$i(style = "left: 25%;"), tags$i(style = "left: 50%;"), tags$i(style = "left: 75%;"))
}

cr_fig <- function(value, label, sub = NULL, unit = NULL) {
  tags$div(class = "cr-fig",
    tags$div(tags$span(class = "cr-fig-n", value),
             if (!is.null(unit)) tags$span(class = "cr-fig-u", unit)),
    tags$div(class = "cr-fig-l", label,
             if (!is.null(sub)) tags$span(class = "cr-fig-s", sub)))
}

cr_rule_div <- function() tags$div(class = "cr-vrule", `aria-hidden` = "true")

# The whole CSV export on the page. The heads are labels, each with the
# file's column name as its title; unit is the run's, for the level columns.
cr_results_grid <- function(df, fid_by_row, sel_fid = NULL, unit = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  keep <- if (is.null(sel_fid)) rep(TRUE, nrow(df)) else fid_by_row == sel_fid
  if (!any(keep)) keep <- rep(TRUE, nrow(df))

  groups <- cr_groups()
  listed <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)
  present_cols <- setdiff(names(df), "subject_id")
  groups <- lapply(groups, function(g) list(g[[1]], intersect(g[[2]], present_cols)))
  groups <- Filter(function(g) length(g[[2]]) > 0, groups)
  leftover <- setdiff(present_cols, listed)
  if (length(leftover)) groups <- c(groups, list(list("Other", leftover)))
  flat <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)

  sub_w <- 74L
  cs_w <- 232L
  widths <- c(sub_w, cs_w, vapply(flat, cr_col_width, integer(1)))
  total <- sum(widths)

  # Formatted over every row, so one selected row keeps the column's decimals
  shown <- lapply(stats::setNames(flat, flat), function(h) cr_col_text(df[[h]]))
  is_num <- vapply(flat, function(h) any(cr_is_num(df[[h]])), logical(1))

  head_group <- tags$tr(
    tags$th(class = "cr-gh cr-pin cr-p0", rowspan = 2, "Subject"),
    tags$th(class = "cr-gh cr-pin cr-p1", rowspan = 2,
      tags$span(class = "cr-cshead",
        tags$span(class = "cr-lbl", "The day"),
        tags$span(class = "cr-tk first", style = "left: 0;", "00:00"),
        tags$span(class = "cr-tk", style = "left: 25%;", "06:00"),
        tags$span(class = "cr-tk", style = "left: 50%;", "12:00"),
        tags$span(class = "cr-tk", style = "left: 75%;", "18:00"),
        tags$span(class = "cr-tk last", style = "left: 100%;", "24:00"))),
    lapply(groups, function(g) tags$th(class = "cr-gg", colspan = length(g[[2]]), g[[1]])))

  head_col <- tags$tr(lapply(flat, function(h)
    tags$th(class = paste("cr-ch", if (is_num[[h]]) "cr-r" else ""),
            title = h, cr_label(h, unit))))

  idx <- which(keep)
  body <- lapply(idx, function(i) {
    o <- as.list(df[i, , drop = FALSE])
    fid <- fid_by_row[i]
    tags$tr(
      class = paste("cr-row", if (identical(fid, sel_fid)) "is-selected" else ""),
      `data-fid` = if (is.na(fid)) NULL else fid,
      tabindex = "0",
      tags$td(class = "cr-sub cr-pin cr-p0", as.character(o[["subject_id"]])),
      tags$td(class = "cr-pin cr-p1", cr_clock_strip(o)),
      lapply(flat, function(h) tags$td(class = if (is_num[[h]]) "cr-r", shown[[h]][i])))
  })

  tags$div(class = "cr-scroll",
    tags$table(class = "cr-gt", style = paste0("width: ", total, "px;"),
      tags$colgroup(lapply(widths, function(w) tags$col(style = paste0("width: ", w, "px;")))),
      tags$thead(head_group, head_col),
      tags$tbody(body)))
}

cr_export_menu <- function(ns, ready = TRUE) {
  if (!ready) {
    return(tags$div(class = "cr-exportmenu", id = ns("export_menu"), style = "display: none;",
      tags$div(class = "cr-ei cr-ei-note",
        tags$span(class = "cr-ei-link", "Run the analysis first"))))
  }
  tags$div(class = "cr-exportmenu", id = ns("export_menu"), style = "display: none;",
    tags$div(class = "cr-ei cr-ei-all",
      tags$span(class = "cr-ei-link cr-ei-strong",
                icon("download"), tags$span(class = "cr-ei-label", "Download both"))),
    tags$div(class = "cr-ei-rule", `aria-hidden` = "true"),
    tags$div(class = "cr-ei", downloadButton(ns("dl_csv"), "circadian_results.csv", class = "cr-ei-link")),
    tags$div(class = "cr-ei", downloadButton(ns("dl_workbook"), "circadian_workbook.xlsx", class = "cr-ei-link")))
}
