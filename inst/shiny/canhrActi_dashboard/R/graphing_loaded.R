# Visualization: the figure workbench. One figure for a paper, so the page is
# a toolbar of popovers and an artboard. The figure is drawn at the size the
# Size button names and scaled to fit, and the readout says both.

# The charts, one table read by the chooser and the plot title. The labels
# go inside the figure through chart_title().
gr_families <- function() {
  list(
    list("Activity", c(
      "Daily timeline" = "daily_timeline", "Heatmap" = "heatmap",
      "Intensity Pie" = "intensity_pie", "Intensity Area" = "intensity_area",
      "Intensity distribution" = "intensity_hours", "Hourly pattern" = "hourly_pattern",
      # No Light Exposure entry: at the hip the lux channel is mostly clothing
      # over the sensor, and the sensor's mean error is -68% (Grant et al., SLEEP 2026)
      "24h Clock" = "activity_clock")),
    list("Sleep", c(
      "Hypnogram" = "hypnogram", "Sleep Quality" = "sleep_quality")),
    list("Circadian", c(
      "Polar Chart" = "polar", "IS/IV Analysis" = "is_iv", "Actogram" = "actogram",
      "Periodogram (LS)" = "periodogram", "Chi-square Periodogram" = "chisq",
      "Extended cosinor" = "extcosinor", "Cosinor Ellipse" = "cosinor_ellipse",
      "24-hour profile" = "circ_profile", "Cosinor fit" = "cosinor_fit",
      "DFA" = "dfa")),
    list("Sedentary", c(
      "Bout Histogram" = "bout_histogram", "Accumulation (Lorenz)" = "bout_lorenz",
      "Bout Survival" = "bout_survival", "State Transitions" = "transition_matrix",
      "Timeline" = "sed_timeline", "Hourly heatmap" = "sed_heatmap",
      "Bout occurrence" = "sed_occurrence", "Bout categories" = "bout_categories",
      "Bouts by hour" = "hourly_bouts", "Duration by hour" = "hourly_duration")),
    list("Summary", c(
      "Daily Bars" = "daily_bars", "Weekend vs Weekday" = "weekend_weekday",
      "Day Comparison" = "day_comparison")),
    # The raw figures take a recording out of shared$raw, not shared$files;
    # none takes a title argument, so the label names only the menu row
    list("Raw recording", c(
      "Data quality" = "raw_quality", "Calibration" = "raw_calibration",
      "Gaps" = "raw_gaps", "Chunks" = "raw_chunks",
      "Per day" = "raw_days", "Nights" = "raw_nights",
      "Wear by day" = "raw_wear_days",
      "Sleep period" = "raw_sleep", "Sleep by night" = "raw_sleep_nights",
      "Sleep regularity" = "raw_sleep_regularity",
      "Sleep nights (GGIR part 4)" = "raw_ggir_sleep",
      # the four below need GGIR part 5, scored on the Activity page
      "Block length" = "raw_timeuse_bouts",
      "Intensity gradient" = "raw_timeuse_gradient",
      "Day panel (GGIR part 5)" = "raw_ggir_panel"))
  )
}

# key -> label and key -> family, from the table above
gr_chart_names <- function() {
  out <- unlist(lapply(gr_families(), function(f) f[[2]]))
  stats::setNames(names(out), unname(out))
}
gr_chart_families <- function() {
  fams <- gr_families()
  stats::setNames(
    unlist(lapply(fams, function(f) rep(f[[1]], length(f[[2]])))),
    unlist(lapply(fams, function(f) unname(f[[2]])))
  )
}

# The charts that can draw every recording at once
gr_all_charts <- function() {
  c(
    "intensity_pie", "polar", "daily_bars", "weekend_weekday",
    "intensity_hours", "hourly_pattern",
    "bout_histogram", "bout_lorenz", "bout_survival", "transition_matrix",
    "sed_timeline", "sed_heatmap", "sed_occurrence", "bout_categories",
    "hourly_bouts", "hourly_duration"
  )
}

# The raw family's keys, read off the table above
gr_raw_charts <- function() {
  fams <- gr_families()
  keep <- vapply(fams, function(f) identical(f[[1]], "Raw recording"), logical(1))
  if (!any(keep)) return(character(0))
  unname(unlist(lapply(fams[keep], function(f) f[[2]])))
}
gr_is_raw <- function(chart) (chart %||% "") %in% gr_raw_charts()

# Families drawn from either kind of recording. Circadian charts are shape,
# not level, so the unit does not matter; Sedentary charts are a threshold,
# so on raw they read the published mg cut points, as the Sedentary tab does.
gr_dual_families <- function() c("Circadian", "Sedentary")

# Activity and Summary go chart by chart. Out: daily_bars (plots Steps, which
# a .gt3x does not record), intensity_hours (its raw analogue is GGIR part 5)
# and hourly_pattern (reads the Activity tab's frames, keyed by counts file).
gr_dual_charts <- function() c(
  "daily_timeline", "heatmap", "activity_clock",     # Activity, pure series
  "weekend_weekday", "day_comparison",               # Summary, pure series
  "intensity_pie", "intensity_area"                  # Activity, thresholded
)

gr_is_dual <- function(chart) {
  chart <- chart %||% ""
  fam <- gr_chart_families()[[chart]]
  (!is.null(fam) && fam %in% gr_dual_families()) || chart %in% gr_dual_charts()
}

# GGIR's two figures are base graphics: they draw into the open device and
# return a status list, so the page stores a closure for them and calls it
# inside the device. safe_export_plot() asks the drawing with is.function().

# GGIR's two figures size themselves from the data. The day panel's geometry
# is a ratio, 7.77 in wide and 11.19/8 in per day window, so the height
# follows the width. The sleep figure's is absolute, a 40 px row per night as
# the Sleep page draws it. A drawing carries whichever is its own as an attribute.
GGIR_PANEL_W_IN <- 7.77          # R/raw_ggir_panelplot.R
GGIR_PANEL_ROW_IN <- 11.19 / 8

gr_aspect_of <- function(p) {
  a <- suppressWarnings(as.numeric(attr(p, "gr_aspect")))
  if (length(a) != 1 || is.na(a) || a <= 0) return(NULL)
  min(6, max(0.2, a))            # a 40-night recording is tall, not infinite
}
gr_height_of <- function(p) {
  h <- suppressWarnings(as.numeric(attr(p, "gr_height")))
  if (length(h) != 1 || is.na(h) || h <= 0) return(NULL)
  as.integer(round(h))
}

# The height the Size button should read; NULL when the drawing does not care
gr_height_for <- function(p, width) {
  h <- gr_height_of(p)
  if (is.null(h)) {
    a <- gr_aspect_of(p)
    if (is.null(a)) return(NULL)
    h <- width * a
  }
  max(400L, min(4000L, as.integer(round(h))))
}

# The height the Size field starts at
GR_DEFAULT_H <- 800L

# Charts drawn one facet row per calendar day, in px at 96 dpi: the fixed part
# (title, x axis, margins) and one day's strip and panel, sized so the y labels
# of a row do not print over each other
gr_day_rows <- function() list(
  daily_timeline = c(fixed = 118, row = 142),
  intensity_area = c(fixed = 173, row = 140)
)

# The starting height for a chart drawn one row per day, never below the page
# default; NULL for any other chart. Days are counted as the plots facet them.
gr_day_height <- function(chart, timestamps) {
  g <- gr_day_rows()[[chart %||% ""]]
  if (is.null(g) || length(timestamps) == 0) return(NULL)
  if (!inherits(timestamps, "POSIXct")) timestamps <- as.POSIXct(timestamps)
  n <- length(unique(as.Date(timestamps)))
  max(GR_DEFAULT_H, min(4000L, as.integer(round(g[["fixed"]] + g[["row"]] * n))))
}

# The png route a drawing prefers. GGIR's day panel goes through a pdf and is
# rasterised; drawn straight onto a raster device its thin bouts are dropped.
gr_png_writer_of <- function(p) {
  w <- attr(p, "gr_png_writer")
  if (is.function(w)) w else NULL
}

# The inputs output$chart_options puts on screen for each chart; single-choice
# selects are left out
gr_option_inputs <- function() {
  cp <- list(cuts = TRUE)   # this chart is thresholded by the cut-point set
  list(
    daily_timeline    = list(groups = "show_axes", checks = "show_cutpoints"),
    heatmap           = list(checks = c("heatmap_weekends", "heatmap_normalize")),
    intensity_pie     = list(checks = c("pie_donut", "pie_labels"), cuts = TRUE),
    intensity_area    = cp,
    hypnogram         = list(checks = c("hyp_activity", "hyp_metrics", "hyp_awakenings")),
    polar             = list(checks = c("polar_ribbon", "polar_daytype", "polar_L5M10")),
    bout_histogram    = cp,
    bout_lorenz       = cp,
    bout_survival     = cp,
    transition_matrix = cp,
    sed_timeline      = cp,
    sed_heatmap       = cp,
    sed_occurrence    = cp,
    bout_categories   = cp,
    hourly_bouts      = cp,
    hourly_duration   = cp,
    day_comparison    = list(groups = "compare_days")
  )
}

# Does this chart's output depend on the cut-point set?
gr_uses_cuts <- function(chart) isTRUE(gr_option_inputs()[[chart %||% ""]]$cuts)

# The cut-point sets, named once so the chooser and the button agree.
gr_cut_sets <- function() {
  c("Freedson (1998)" = "freedson", "Evenson (2008)" = "evenson",
    "Troiano (2008)" = "troiano", "CANHR (2025)" = "canhr")
}
# The thresholds this recording can be scored against: a raw recording gets
# the published mg cut points, from the same helper the Sedentary tab uses.
gr_cut_choices_for <- function(shared, sel) {
  if (!is.null(sel) && sel %in% names(shared$raw %||% list())) {
    ch <- tryCatch(sed_cut_choices(shared, "raw"), error = function(e) character(0))
    if (length(ch) > 0) return(ch)
  }
  gr_cut_sets()
}

# What the closed Options button says the threshold is
gr_cut_label <- function(key, shared = NULL, sel = NULL) {
  s <- if (is.null(shared)) gr_cut_sets() else gr_cut_choices_for(shared, sel)
  lab <- names(s)[match(key %||% "", unname(s))]
  if (length(lab) == 1 && !is.na(lab)) return(lab)
  # not a key of this list: its first entry, which sed_resolve_cut() scores against
  if (length(s) > 0) names(s)[1] else "Freedson (1998)"
}

# How many options are switched on, from the live inputs
gr_option_count <- function(chart, input) {
  spec <- gr_option_inputs()[[chart %||% ""]]
  if (is.null(spec)) return(0L)
  n <- 0L
  for (g in spec$groups) n <- n + length(input[[g]])
  for (k in spec$checks) if (isTRUE(input[[k]])) n <- n + 1L
  n
}

gr_has_options <- function(chart) !is.null(gr_option_inputs()[[chart %||% ""]])

# DRAWING

gr_caret <- function() tags$span(class = "gr-car", HTML("&#9660;"))

# One toolbar button; the value sits in the button
gr_tb <- function(ns, key, label, value_output, width, body, right = FALSE) {
  tags$span(
    class = "gr-tb", `data-pop` = key,
    if (!is.null(label)) tags$span(class = "gr-tb-k", label),
    uiOutput(ns(value_output), inline = TRUE),
    gr_caret(),
    tags$div(class = "gr-pop", style = sprintf("width: %dpx;%s", width,
                                               if (right) " right: 0; left: auto;" else ""),
             body)
  )
}

gr_chart_menu <- function(chart) {
  tagList(lapply(gr_families(), function(f) {
    tagList(
      tags$div(class = "gr-pm-head", f[[1]]),
      lapply(seq_along(f[[2]]), function(i) {
        key <- unname(f[[2]][i]); lab <- names(f[[2]])[i]
        tags$div(class = paste("gr-pm-i", if (identical(key, chart)) "is-on" else ""),
                 `data-chart` = key,
                 tags$span(class = "gr-tick", if (identical(key, chart)) HTML("&#10003;") else ""),
                 lab)
      })
    )
  }))
}

gr_who_row <- function(id, label, days, sel) {
  tags$div(class = paste("gr-pm-i", if (identical(sel, id)) "is-on" else ""),
           `data-who` = id,
           tags$span(class = "gr-tick", if (identical(sel, id)) HTML("&#10003;") else ""),
           label,
           tags$span(class = "gr-pm-s",
                     if (is.na(days)) "" else paste(days, "days")))
}

# Calendar days a raw recording covers, from the span of the epoch clock; the
# counts side counts distinct dates in the epoch table instead.
gr_raw_days <- function(r) {
  t <- tryCatch(r$meta$metashort$time, error = function(e) NULL)
  if (is.null(t) || length(t) < 2) return(NA_integer_)
  tz <- tryCatch(r$tz$effective_tz, error = function(e) "")
  if (length(tz) != 1 || is.na(tz) || !nzchar(tz)) tz <- "UTC"
  # as.Date() on a POSIXct defaults to tz = "UTC" and ignores the object's
  # tzone, so tz is passed again or an evening epoch re-dates to the next day
  ends <- tryCatch(
    as.Date(as.POSIXct(as.numeric(range(t, na.rm = TRUE)), origin = "1970-01-01", tz = tz),
            tz = tz),
    error = function(e) NULL)
  if (is.null(ends) || any(is.na(ends))) return(NA_integer_)
  as.integer(ends[2] - ends[1]) + 1L
}

# The recording list. Both stores are in one menu, since this page has no
# Counts/Raw switch; it is sectioned when both kinds are loaded. "All
# recordings" is counts-only: every cohort chart pools shared$files.
gr_who_menu <- function(files, raws, sel, supports_all) {
  files <- files %||% list()
  raws <- raws %||% list()
  ids <- names(files)
  labs <- vapply(seq_along(ids), function(i) {
    f <- files[[i]]
    as.character(f$subject_info$id %||% f$name %||% ids[i])[1]
  }, character(1))
  days <- vapply(files, function(f) {
    if ("timestamp" %in% names(f$data)) length(unique(as.Date(f$data$timestamp))) else NA_integer_
  }, integer(1))

  rids <- names(raws)
  rlabs <- vapply(seq_along(rids), function(i) ovr_name(raws[[i]], rids[i]), character(1))
  rdays <- vapply(raws, gr_raw_days, integer(1))

  sectioned <- length(ids) > 0 && length(rids) > 0

  tagList(
    if (length(ids) > 0) tagList(
      tags$div(class = paste("gr-pm-i", if (identical(sel, "all")) "is-on" else ""),
               `data-who` = "all",
               tags$span(class = "gr-tick", if (identical(sel, "all")) HTML("&#10003;") else ""),
               "All recordings",
               tags$span(class = "gr-pm-s", length(ids))),
      if (!supports_all)
        tags$div(class = "gr-pm-note", "This chart draws one recording at a time."),
      tags$div(class = "gr-pm-rule")),
    if (sectioned) tags$div(class = "gr-pm-head", "Counts"),
    lapply(seq_along(ids), function(i) gr_who_row(ids[i], labs[i], days[i], sel)),
    if (sectioned) tags$div(class = "gr-pm-head", "Raw"),
    lapply(seq_along(rids), function(i) gr_who_row(rids[i], rlabs[i], rdays[i], sel))
  )
}

gr_size_presets <- function() {
  list(
    list(w = 1000, h = 800,  name = "Standard"),
    list(w = 1600, h = 900,  name = "Wide"),
    list(w = 800,  h = 800,  name = "Square"),
    list(w = 1200, h = 1600, name = "Tall")
  )
}

# Size. The two fields live in the static UI; the preset buttons are drawn here
gr_size_extras <- function(w, h, png_dpi = 300) {
  tagList(
    tags$div(class = "gr-sizes",
      lapply(gr_size_presets(), function(p) {
        # the swatch is the preset at its own proportions, inside a 24 x 20 box
        k <- min(24 / p$w, 20 / p$h)
        tags$div(class = paste("gr-size", if (w == p$w && h == p$h) "is-on" else ""),
                 `data-size` = paste0(p$w, "x", p$h),
          tags$span(class = "gr-sw",
            tags$i(style = sprintf("width: %dpx; height: %dpx;",
                                   max(3L, as.integer(round(p$w * k))),
                                   max(3L, as.integer(round(p$h * k)))))),
          tags$span(class = "gr-size-t",
            tags$b(sprintf("%d \u00d7 %d", p$w, p$h)),
            tags$span(p$name)))
      })),
    # The PNG is written at 300 dpi, so the file is not the pixel size above it
    tags$div(class = "gr-pm-note",
             sprintf("PNG comes out %s \u00d7 %s px at %d dpi. PDF and SVG are vector.",
                     format(round(w / 96 * png_dpi), big.mark = ","),
                     format(round(h / 96 * png_dpi), big.mark = ","), png_dpi))
  )
}

# Export. Each row is the file it writes, named from the drawn chart's stem
gr_export_menu <- function(ns, stem) {
  if (is.null(stem)) {
    return(tags$div(class = "gr-pm-note", "Draw a chart first."))
  }
  item <- function(id, name) tags$div(class = "gr-ei", downloadButton(ns(id), name, class = "gr-ei-btn"))
  tagList(
    item("download_pdf", paste0(stem, ".pdf")),
    item("download_svg", paste0(stem, ".svg")),
    item("download_png", paste0(stem, ".png"))
  )
}

# The artboard. The sheet carries the figure's true size in data attributes
gr_sheet <- function(ns, w, h, live) {
  # A live figure is plotly, whose pointer maths cannot survive a CSS
  # transform, so it is not scaled: it fills the canvas at its own size
  if (live) {
    return(tags$div(class = "gr-sheet-box is-live", `data-w` = w, `data-h` = h, `data-live` = "1",
      tags$div(class = "gr-sheet",
        plotly::plotlyOutput(ns("main_plotly"), width = "100%", height = "100%"))))
  }
  # The box states its size inline too, so the un-scaled state is clipped
  # rather than hung off a corner. No transform: R renders at the preview's pixels.
  tags$div(class = "gr-sheet-box", `data-w` = w, `data-h` = h,
    style = sprintf("width: %dpx; height: %dpx;", w, h),
    tags$div(class = "gr-sheet",
      imageOutput(ns("main_plot"), width = "100%", height = "100%"))
  )
}

gr_empty <- function(msg, sub) {
  tags$div(class = "gr-empty",
           tags$div(class = "gr-empty-h", msg),
           tags$div(class = "gr-empty-s", sub))
}

# The page script: one popover at a time, the sheet scaled to the canvas
gr_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }

  function closeMenus() {
    document.querySelectorAll('.gr-page .gr-tb.is-open').forEach(function (b) { b.classList.remove('is-open'); });
  }
  function toggleMenu(btn) {
    var was = btn.classList.contains('is-open');
    closeMenus();
    if (was) return;
    btn.classList.add('is-open');
  }

  // ---- the artboard -------------------------------------------------------
  // Size the sheet to what the canvas can show, tell the server to draw the
  // preview at that scale, and state both numbers. Nothing here resizes an
  // image; the figure is redrawn at whatever size it is shown.
  function layout() {
    var canvas = document.querySelector('.gr-page .gr-canvas');
    if (!canvas) return;
    var box = canvas.querySelector('.gr-sheet-box');
    var out = document.querySelector('.gr-page .gr-ro');
    if (!box) { if (out) out.textContent = ''; return; }

    var w = parseFloat(box.dataset.w), h = parseFloat(box.dataset.h);
    if (!(w > 0 && h > 0)) return;

    // A live figure is sized by the canvas rather than by its own proportions,
    // so there is no percentage to report.
    if (box.dataset.live) {
      if (out) out.innerHTML = '<b>' + w + ' \\u00d7 ' + h + ' px</b> on export &middot; interactive, fitted to the canvas';
      canvas.classList.add('is-live');
      return;
    }
    canvas.classList.remove('is-live');
    // Always fit, and never past the figure's own size: a preview larger than
    // the export would flatter the figure.
    var pad = 20;
    var s = Math.min((canvas.clientWidth - pad * 2) / w, (canvas.clientHeight - pad * 2) / h, 1);
    if (!(s > 0)) s = 1;
    box.style.width = Math.round(w * s) + 'px';
    box.style.height = Math.round(h * s) + 'px';

    // The server draws the preview at this scale. Rounded, so a pixel of
    // window wobble does not queue another render.
    var r = Math.round(s * 1000) / 1000;
    if (box.dataset.sent !== String(r)) {
      box.dataset.sent = String(r);
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(NS + 'fit_scale', r);
    }
    if (out) out.innerHTML = '<b>' + w + ' \\u00d7 ' + h + ' px</b> &middot; shown at ' + Math.round(s * 100) + '%';
  }
  function fullscreen() {
    // The strip goes fullscreen with the canvas: the size and the scale are
    // most worth reading when the figure is biggest.
    var el = document.querySelector('.gr-page .gr-canvas-out');
    if (!el) return;
    if (!document.fullscreenElement) {
      (el.requestFullscreen || el.webkitRequestFullscreen || el.msRequestFullscreen).call(el);
    } else {
      (document.exitFullscreen || document.webkitExitFullscreen || document.msExitFullscreen).call(document);
    }
  }

  document.addEventListener('click', function (e) {
    if (!e.target.closest || !e.target.closest('.gr-page')) { closeMenus(); return; }

    if (e.target.closest('.gr-page .gr-fs')) { fullscreen(); return; }

    var ci = e.target.closest('.gr-page .gr-pm-i[data-chart]');
    if (ci) { closeMenus(); setVal('chart_pick', ci.dataset.chart); return; }

    var wi = e.target.closest('.gr-page .gr-pm-i[data-who]');
    if (wi) { closeMenus(); setVal('who_pick', wi.dataset.who); return; }

    var ps = e.target.closest('.gr-page .gr-size[data-size]');
    if (ps) { setVal('size_pick', ps.dataset.size); return; }


    // Sliders, checkboxes and download links live inside popovers; a click on
    // any of them must not shut the panel it belongs to.
    if (e.target.closest('.gr-page .gr-pop')) return;

    var tb = e.target.closest('.gr-page .gr-tb');
    if (tb) { toggleMenu(tb); return; }

    closeMenus();
  });

  window.addEventListener('resize', layout);
  document.addEventListener('fullscreenchange', function () { setTimeout(layout, 60); });

  // Shiny fires shiny:value through jQuery, and jQuery.trigger() reaches
  // jQuery handlers only, so a native listener here would never hear the one
  // event that means the figure arrived. Watch the canvas instead. childList
  // only: layout() writes inline styles, so observing attributes too would
  // make every layout schedule another one.
  var watched = null;
  function watch() {
    var canvas = document.querySelector('.gr-page .gr-canvas');
    if (!canvas) return;
    if (canvas !== watched) {
      watched = canvas;
      new MutationObserver(function () { setTimeout(layout, 0); })
        .observe(canvas, { childList: true, subtree: true });
      if (window.ResizeObserver) new ResizeObserver(function () { layout(); }).observe(canvas);
    }
    layout();
  }
  if (window.jQuery) jQuery(document).on('shiny:value shiny:visualchange shiny:connected', function () { setTimeout(watch, 0); });
  setInterval(watch, 1000);
  if (document.readyState !== 'loading') setTimeout(watch, 0);
  else document.addEventListener('DOMContentLoaded', function () { setTimeout(watch, 0); });
})();
"
  gsub("__NS__", ns_prefix, js, fixed = TRUE)
}
