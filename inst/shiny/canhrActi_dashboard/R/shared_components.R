# 
# CANHR ACTIGRAPH DASHBOARD - SHARED HELPERS
# University of Alaska Fairbanks
#
# Helpers the modules share: formatting, icons, the plot theme, the run
# button state and focus between tabs.
#
# Usage: Source this file BEFORE other modules in app.R
# 

# Null coalesce operator (defined once, used everywhere)
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# 
# FORMATTING UTILITIES
# 

#' Format ETA
#'
#' Format ETA for progress messages
#'
#' @param seconds Estimated seconds remaining
#'
format_eta <- function(seconds) {
  if (is.na(seconds) || seconds < 0) return("calculating...")
  if (seconds < 60) return(paste0(round(seconds), "s"))
  if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
  return(paste0(round(seconds / 3600, 1), "h"))
}

# APP STANDARD HELPERS

#' A count with its noun: "1 file", "2 files"
pluralize <- function(n, word, plural = paste0(word, "s")) {
  n <- as.integer(n)
  paste(format(n, big.mark = ","), if (identical(n, 1L)) word else plural)
}

#' Sidebar icons: 20 px line drawings, one per tab
# Material Symbols Outlined glyphs from google/material-design-icons, carried
# as markup so nothing loads over the network. Every path must come from a
# file with viewBox "0 -960 960 960", which msym() wraps them in; the legacy
# 0-24 icons in that repo render as a speck.
msym_paths <- list(
    dataset = 'M280-280h160v-160H280v160Zm240 0h160v-160H520v160ZM280-520h160v-160H280v160Zm240 0h160v-160H520v160ZM200-120q-33 0-56.5-23.5T120-200v-560q0-33 23.5-56.5T200-840h560q33 0 56.5 23.5T840-760v560q0 33-23.5 56.5T760-120H200Zm0-80h560v-560H200v560Zm0-560v560-560Z',
    note_add = 'M440-240h80v-120h120v-80H520v-120h-80v120H320v80h120v120ZM240-80q-33 0-56.5-23.5T160-160v-640q0-33 23.5-56.5T240-880h320l240 240v480q0 33-23.5 56.5T720-80H240Zm280-520v-200H240v640h480v-440H520ZM240-800v200-200 640-640Z',
    create_new_folder = 'M560-320h80v-80h80v-80h-80v-80h-80v80h-80v80h80v80ZM160-160q-33 0-56.5-23.5T80-240v-480q0-33 23.5-56.5T160-800h240l80 80h320q33 0 56.5 23.5T880-640v400q0 33-23.5 56.5T800-160H160Zm0-80h640v-400H447l-80-80H160v480Zm0 0v-480 480Z',
    folder_open = 'M160-160q-33 0-56.5-23.5T80-240v-480q0-33 23.5-56.5T160-800h240l80 80h320q33 0 56.5 23.5T880-640H447l-80-80H160v480l96-320h684L837-217q-8 26-29.5 41.5T760-160H160Zm84-80h516l72-240H316l-72 240Zm0 0 72-240-72 240Zm-84-400v-80 80Z',
    swap_horiz = 'M280-160 80-360l200-200 56 57-103 103h287v80H233l103 103-56 57Zm400-240-56-57 103-103H440v-80h287L624-743l56-57 200 200-200 200Z',
    watch = 'm360-80-54-182q-48-38-77-95t-29-123q0-66 29-123t77-95l54-182h240l54 182q48 38 77 95t29 123q0 66-29 123t-77 95L600-80H360Zm120-200q83 0 141.5-58.5T680-480q0-83-58.5-141.5T480-680q-83 0-141.5 58.5T280-480q0 83 58.5 141.5T480-280Zm-76-470q20-5 38.5-8t37.5-3q19 0 37.5 3t38.5 8l-16-50H420l-16 50Zm16 590h120l16-50q-20 5-38.5 7.5T480-200q-19 0-37.5-2.5T404-210l16 50Zm-16-640h152-152Zm16 640h-16 152-136Z',
    directions_run = 'M520-40v-240l-84-80-40 176-276-56 16-80 192 40 64-324-72 28v136h-80v-188l158-68q35-15 51.5-19.5T480-720q21 0 39 11t29 29l40 64q26 42 70.5 69T760-520v80q-66 0-123.5-27.5T540-540l-24 120 84 80v300h-80Zm20-700q-33 0-56.5-23.5T460-820q0-33 23.5-56.5T540-900q33 0 56.5 23.5T620-820q0 33-23.5 56.5T540-740Z',
    bedtime = 'M524-40q-84 0-157.5-32t-128-86.5Q184-213 152-286.5T120-444q0-146 93-257.5T450-840q-18 99 11 193.5T561-481q71 71 165.5 100T920-370q-26 144-138 237T524-40Zm0-80q88 0 163-44t118-121q-86-8-163-43.5T504-425q-61-61-97-138t-43-163q-77 43-120.5 118.5T200-444q0 135 94.5 229.5T524-120Zm-20-305Z',
    schedule = 'm612-292 56-56-148-148v-184h-80v216l172 172ZM480-80q-83 0-156-31.5T197-197q-54-54-85.5-127T80-480q0-83 31.5-156T197-763q54-54 127-85.5T480-880q83 0 156 31.5T763-763q54 54 85.5 127T880-480q0 83-31.5 156T763-197q-54 54-127 85.5T480-80Zm0-400Zm0 320q133 0 226.5-93.5T800-480q0-133-93.5-226.5T480-800q-133 0-226.5 93.5T160-480q0 133 93.5 226.5T480-160Z',
    chair = 'M200-120q-17 0-28.5-11.5T160-160v-40q-50 0-85-35t-35-85v-200q0-50 35-85t85-35v-80q0-50 35-85t85-35h400q50 0 85 35t35 85v80q50 0 85 35t35 85v200q0 50-35 85t-85 35v40q0 17-11.5 28.5T760-120q-17 0-28.5-11.5T720-160v-40H240v40q0 17-11.5 28.5T200-120Zm-40-160h640q17 0 28.5-11.5T840-320v-200q0-17-11.5-28.5T800-560q-17 0-28.5 11.5T760-520v160H200v-160q0-17-11.5-28.5T160-560q-17 0-28.5 11.5T120-520v200q0 17 11.5 28.5T160-280Zm120-160h400v-80q0-27 11-49t29-39v-112q0-17-11.5-28.5T680-760H280q-17 0-28.5 11.5T240-720v112q18 17 29 39t11 49v80Zm200 0Zm0 160Zm0-80Z',
    show_chart = 'm140-220-60-60 300-300 160 160 284-320 56 56-340 384-160-160-240 240Z',
    search = 'M784-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l252 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T380-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400Z',
    menu = 'M120-240v-80h720v80H120Zm0-200v-80h720v80H120Zm0-200v-80h720v80H120Z',
    expand_less = 'm296-345-56-56 240-240 240 240-56 56-184-184-184 184Z',
    monitoring = 'M120-120v-80l80-80v160h-80Zm160 0v-240l80-80v320h-80Zm160 0v-320l80 81v239h-80Zm160 0v-239l80-80v319h-80Zm160 0v-400l80-80v480h-80ZM120-327v-113l280-280 160 160 280-280v113L560-447 400-607 120-327Z',
    open_in_new = 'M200-120q-33 0-56.5-23.5T120-200v-560q0-33 23.5-56.5T200-840h280v80H200v560h560v-280h80v280q0 33-23.5 56.5T760-120H200Zm188-212-56-56 372-372H560v-80h280v280h-80v-144L388-332Z'
)

msym <- function(name, class = "sh-ico") {
  tags$i(
    class = class, `aria-hidden` = "true",
    HTML(paste0('<svg viewBox="0 -960 960 960" fill="currentColor" aria-hidden="true"><path d="', msym_paths[[name]], '"></path></svg>'))
  )
}

# The tabs name their icon by what they do
sidebar_icon <- function(name) {
  msym(switch(
    name,
    overview  = "monitoring",
    wear_time = "watch",
    activity  = "directions_run",
    sleep     = "bedtime",
    circadian = "schedule",
    sedentary = "chair",
    graphing  = "show_chart",
    name
  ))
}

# Theme patch for charts drawn in dark mode. The shell reports the theme
# (www/shell.js) and app.R puts it on `shared`; in light mode this returns
# NULL and `p + NULL` is `p`.
gg_theme_app <- function(dark = FALSE) {
  if (!isTRUE(dark)) return(NULL)
  # The chart keeps white paper in both themes: the colours inside the plots
  # (titles, reference lines, annotations) were picked to sit on white
  paper <- "#ffffff"
  ggplot2::theme(
    plot.background       = ggplot2::element_rect(fill = paper, colour = NA),
    panel.background      = ggplot2::element_rect(fill = paper, colour = NA),
    legend.background     = ggplot2::element_rect(fill = paper, colour = NA),
    legend.box.background = ggplot2::element_rect(fill = paper, colour = NA),
    legend.key            = ggplot2::element_rect(fill = paper, colour = NA)
  )
}

# Wrap a plot expression so the theme reaches it; NULL or a non-ggplot passes through
gg_app <- function(dark, p) {
  if (inherits(p, "ggplot")) p + gg_theme_app(dark) else p
}

# Ink for base-R plots, which take colours per call rather than from a theme.
base_ink <- function(dark = FALSE) {
  # one set, since base plots keep their paper too
  list(ink = "#333333", muted = "#75797c", grid = "#e0e0e0")
}

# Whether a page's controls have moved since its run. `params` is the
# parameter list the run stored and `spec` maps an input id to the name it
# was stored under; a key the run never recorded is skipped.
settings_moved_from <- function(params, input, spec) {
  if (is.null(params) || !length(spec)) return(FALSE)
  same_num <- function(a, b) {
    a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
    (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && abs(a - b) < 1e-9)
  }
  for (id in names(spec)) {
    key <- spec[[id]]
    if (!key %in% names(params)) next
    ran <- params[[key]]
    now <- input[[id]]
    if (is.null(now)) next            # the control has not rendered yet
    ok <- if (is.logical(ran) || is.logical(now)) {
      identical(isTRUE(ran), isTRUE(now))
    } else if (is.numeric(ran) || suppressWarnings(!is.na(as.numeric(now)))) {
      same_num(ran, now)
    } else {
      identical(as.character(ran), as.character(now))
    }
    if (!ok) return(TRUE)
  }
  FALSE
}

# The wording every page uses
stale_note <- function(class_prefix) {
  tags$span(class = paste0(class_prefix, "-stale"), "changed since the run below")
}

# SHARED PANEL SYSTEM

# The Run button's classes, one rule on every page: filled while there is
# something to run (no results yet, or the settings moved since the run),
# outlined once the results match the settings. prefix is the page's button
# prefix ("wt", "ac", "sl", "cr", "sd", or "gr" for Visualization).
run_button_class <- function(prefix, has_results, stale = FALSE) {
  has_results <- isTRUE(has_results)
  stale <- isTRUE(stale)
  primary <- !has_results || stale
  variant <- if (identical(prefix, "gr")) {
    if (primary) "gr-btn--go" else "gr-btn--quiet"
  } else {
    paste0(prefix, "-btn--", if (primary) "primary" else "secondary")
  }
  paste(c(paste0(prefix, "-btn"), variant, if (has_results && stale) "is-stale"),
        collapse = " ")
}

# The text of a string, a tag or a tag list, as a title attribute reads it
plain_text <- function(x) {
  if (is.null(x)) return("")
  if (inherits(x, "shiny.tag")) return(plain_text(x$children))
  if (inherits(x, "html")) {
    s <- gsub("<[^>]*>", "", as.character(x))
    ents <- c("&middot;" = "·", "&nbsp;" = " ", "&ndash;" = "–",
              "&lt;" = "<", "&gt;" = ">", "&amp;" = "&")
    for (e in names(ents)) s <- gsub(e, ents[[e]], s, fixed = TRUE)
    m <- gregexpr("&#[0-9]+;", s)
    regmatches(s, m) <- lapply(regmatches(s, m), function(v)
      vapply(v, function(e) intToUtf8(as.integer(gsub("[^0-9]", "", e))), character(1)))
    return(s)
  }
  if (is.list(x)) return(paste(vapply(x, plain_text, character(1)), collapse = ""))
  paste(as.character(x), collapse = "")
}

# A title for a cell whose text can be cut, or NULL so no attribute is written.
# Plain cut text already gets one from www/shell.js on hover; this is for a
# cell whose shown text is shorter than the value it stands for.
cell_title <- function(x) {
  txt <- trimws(gsub("\\s+", " ", plain_text(x)))
  if (!nzchar(txt) || txt %in% c("–", "-", "NA")) NULL else txt
}

# One recording choice that follows the user across tabs, as a Grafana
# dashboard variable does. id is a key of shared$files ("file_3") or of
# shared$raw ("raw_2"); NULL, "" or "all" means all recordings.
focus_set <- function(shared, id) {
  id <- if (length(id) == 0 || is.na(id[[1]]) || !nzchar(id[[1]]) ||
            identical(as.character(id[[1]]), "all")) NULL else as.character(id[[1]])
  shared$focus <- id
  invisible(id)
}

# The focused recording if it is one of among (the ids the page can show), else NULL
focus_get <- function(shared, among = NULL) {
  id <- shared$focus
  if (is.null(id) || (!is.null(among) && !id %in% among)) NULL else id
}

# Runs fn() each time the tab named in app.R's tabItems is opened
on_tab_shown <- function(shared, tab, fn) {
  observeEvent(shared$tab, if (identical(shared$tab, tab)) fn())
}
