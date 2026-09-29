# Sedentary: the loaded page. The table is the workbook's Summary Metrics
# sheet, built once by sedentary_metrics_df() in mod_sedentary_workbook.R.

# DRAWING

# The Summary table's column groups. A column not listed here still reaches
# the page, under "Other".
sd_groups <- function() {
  list(
    list("Recording", c("Filename", "Cut Points", "Sleep Excluded", "Epoch Length (s)",
                        "Weight (lbs)", "Age", "Gender")),
    list("How much", c("Sedentary Time (hours)", "Sedentary (% wear)", "Total Bouts")),
    list("How long the bouts run", c("Mean Bout (min)", "Median Bout (min)", "Max Bout (min)",
                                     "W25 (min)", "W50 Usual Bout (min)", "W75 (min)", "W90 (min)")),
    list("How broken up", c("Breaks per Sedentary Hour", "ASTP (active->sedentary)",
                            "SATP (sedentary->active)")),
    list("Prolonged sitting", c("Prolonged Bouts (>=30 min, n)", "% Time in >=20 min Bouts",
                                "% Time in >=30 min Bouts", "% Time in >=60 min Bouts")),
    list("Shape of the distribution", c("Power-law Alpha", "Alpha 95% CI Lower",
                                        "Alpha 95% CI Upper", "Alpha GoF P Value (Clauset)",
                                        "Gini Index", "Best-fit Distribution",
                                        "Weibull Shape k", "Weibull Hazard Direction")),
    list("Regularity", c("Sedentary Regularity Index", "Activity Balance Index"))
  )
}

# The column the composition bar draws, by name so it follows the sheet
SD_PROLONGED_COL <- "% Time in >=30 min Bouts"

# Filename is sized from its values, so two recordings can be told apart, up to
# the 190px the other tables give a file name; the full name is on hover
sd_col_width <- function(h, v = NULL) {
  if (identical(h, "Filename") && length(v)) {
    n <- max(nchar(as.character(v[!is.na(v)])), 0L)
    return(max(100L, min(190L, as.integer(ceiling(7.2 * n)) + 18L)))
  }
  n <- nchar(h)
  if (n > 26) return(200L)
  if (n > 20) return(176L)
  if (n > 14) return(150L)
  if (n > 9) return(124L)
  100L
}

sd_is_num <- function(v) grepl("^-?[0-9,]+(\\.[0-9]+)?%?$", trimws(as.character(v)))

# One bar per recording: the share of sedentary time in bouts at or over the
# threshold
sd_comp_bar <- function(pct, subject, threshold) {
  p <- suppressWarnings(as.numeric(pct))
  if (is.na(p)) return(tags$span(class = "sd-cb"))
  p <- max(0, min(100, p))
  tags$span(class = "sd-cb",
    title = paste0(subject, " · ", fmt_dec(p, 1), "% of sedentary time in bouts of ",
                   fmt_int(threshold), " min or more"),
    tags$span(class = "sd-cb-b long", style = sprintf("width: %.1f%%;", p)),
    tags$i(style = "left: 25%;"), tags$i(style = "left: 50%;"), tags$i(style = "left: 75%;"))
}

sd_fig <- function(value, label, sub = NULL, unit = NULL) {
  tags$div(class = "sd-fig",
    tags$div(tags$span(class = "sd-fig-n", value),
             if (!is.null(unit) && !identical(value, "–")) tags$span(class = "sd-fig-u", unit)),
    tags$div(class = "sd-fig-l", label,
             if (!is.null(sub)) tags$span(class = "sd-fig-s", sub)))
}

sd_rule_div <- function() tags$div(class = "sd-vrule", `aria-hidden` = "true")

# The groups df fills, in page order, with any unlisted column under "Other"
sd_grid_groups <- function(df) {
  groups <- sd_groups()
  listed <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)
  present_cols <- setdiff(names(df), "Subject")
  groups <- lapply(groups, function(g) list(g[[1]], intersect(g[[2]], present_cols)))
  groups <- Filter(function(g) length(g[[2]]) > 0, groups)
  leftover <- setdiff(present_cols, listed)
  if (length(leftover)) groups <- c(groups, list(list("Other", leftover)))
  groups
}

# The Summary table, on the page, plus the one bar.
sd_summary_grid <- function(df, fid_by_row, sel_fid = NULL, threshold = 30) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  keep <- if (is.null(sel_fid)) rep(TRUE, nrow(df)) else fid_by_row == sel_fid
  if (!any(keep)) keep <- rep(TRUE, nrow(df))

  groups <- sd_grid_groups(df)
  flat <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)

  # Alignment is decided once per column, so a dash sits where its numbers do
  num_col <- vapply(flat, function(h) {
    v <- df[[h]]
    if (is.numeric(v)) return(TRUE)
    v <- v[!is.na(v)]
    length(v) > 0 && all(sd_is_num(v))
  }, logical(1))

  sub_w <- 74L
  cb_w <- 190L
  widths <- c(sub_w, cb_w, vapply(flat, function(h) sd_col_width(h, df[[h]]), integer(1)))
  total <- sum(widths)

  head_group <- tags$tr(
    tags$th(class = "sd-gh sd-pin sd-p0", rowspan = 2, "Subject"),
    tags$th(class = "sd-gh sd-pin sd-p1", rowspan = 2,
      tags$span(class = "sd-cbhead",
        tags$span(class = "sd-lbl", "Where the time sits"),
        tags$span(class = "sd-tk first", style = "left: 0;", "0%"),
        tags$span(class = "sd-tk", style = "left: 25%;", "25"),
        tags$span(class = "sd-tk", style = "left: 50%;", "50"),
        tags$span(class = "sd-tk", style = "left: 75%;", "75"),
        tags$span(class = "sd-tk last", style = "left: 100%;", "100%"))),
    lapply(groups, function(g) tags$th(class = "sd-gg", colspan = length(g[[2]]), g[[1]])))

  head_col <- tags$tr(lapply(flat, function(h)
    tags$th(class = paste("sd-ch", if (num_col[[h]]) "sd-r" else ""), h)))

  body <- lapply(which(keep), function(i) {
    o <- as.list(df[i, , drop = FALSE])
    fid <- fid_by_row[i]
    tags$tr(
      class = paste("sd-row", if (identical(fid, sel_fid)) "is-selected" else ""),
      `data-fid` = if (is.na(fid)) NULL else fid,
      tabindex = "0",
      tags$td(class = "sd-sub sd-pin sd-p0", as.character(o[["Subject"]])),
      tags$td(class = "sd-pin sd-p1",
              sd_comp_bar(o[[SD_PROLONGED_COL]], as.character(o[["Subject"]]), threshold)),
      lapply(flat, function(h) {
        v <- o[[h]]
        txt <- if (is.na(v)) "–" else as.character(v)
        tags$td(class = if (num_col[[h]]) "sd-r" else NULL, txt)
      }))
  })

  tags$div(class = "sd-scroll",
    tags$table(class = "sd-gt", style = paste0("width: ", total, "px;"),
      tags$colgroup(lapply(widths, function(w) tags$col(style = paste0("width: ", w, "px;")))),
      tags$thead(head_group, head_col),
      tags$tbody(body)))
}

# Go to, in the table head: one row per column group; the page script scrolls
# the grid to the group's first column
sd_goto <- function(groups) {
  if (length(groups) < 2) return(NULL)
  tags$span(class = "sd-goto-wrap",
    tags$span(class = "sd-pick sd-goto", tabindex = "0",
              "Go to", tags$span(class = "sd-car", `aria-hidden` = "true", HTML("&#9660;"))),
    tags$div(class = "sd-gomenu", style = "display: none;",
      lapply(seq_along(groups), function(i)
        tags$div(class = "sd-gi", `data-go` = i - 1L,
                 tags$span(class = "sd-tick", `aria-hidden` = "true"),
                 groups[[i]][[1]],
                 tags$span(class = "sd-sc", pluralize(length(groups[[i]][[2]]), "column"))))))
}

# One file to export: this page writes an XLSX workbook and no CSV
sd_export_menu <- function(ns, ready = TRUE) {
  if (!ready) {
    return(tags$div(class = "sd-exportmenu", id = ns("export_menu"), style = "display: none;",
      tags$div(class = "sd-ei sd-ei-note",
        tags$span(class = "sd-ei-link", "Run the analysis first"))))
  }
  tags$div(class = "sd-exportmenu", id = ns("export_menu"), style = "display: none;",
    tags$div(class = "sd-ei sd-ei-all",
      tags$span(class = "sd-ei-link sd-ei-strong",
                icon("download"), tags$span(class = "sd-ei-label", "Download the workbook"))),
    tags$div(class = "sd-ei-rule", `aria-hidden` = "true"),
    tags$div(class = "sd-ei", downloadButton(ns("dl_workbook"), "sedentary_workbook.xlsx", class = "sd-ei-link")))
}

# The columns the page shows, fewer on a raw run; sd_summary_grid() drops a
# group whose columns are all absent
SD_RAW_DROP <- c("Cut Points", "Sleep Excluded", "Epoch Length (s)",
                 "Weight (lbs)", "Age", "Gender")

sd_page_columns <- function(df, family = "counts") {
  if (is.null(df) || !identical(family, "raw")) return(df)
  keep <- setdiff(names(df), SD_RAW_DROP)
  df[, keep, drop = FALSE]
}
