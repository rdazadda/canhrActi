#' Sedentary Fragmentation Module - Redesigned
#'
#' Clean, insight-focused analysis of sedentary behavior patterns
#' Emphasizes actionable health insights over raw metrics

# One chart panel at the top and the Summary table underneath, one row per
# recording with a bar for the Prolonged % column.

mod_sedentary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "sd-page",
      uiOutput(ns("switch"), class = "sd-out"),
      uiOutput(ns("rule"), class = "sd-out"),
      tags$div(id = ns("settings_panel"), class = "sd-settings", style = "display: none;",
               sd_settings_panel(ns)),
      uiOutput(ns("figures"), class = "sd-out"),
      uiOutput(ns("plot_panel"), class = "sd-out sd-plot-out"),
      uiOutput(ns("table_panel"), class = "sd-out sd-table-out"),

      # Hidden; the chooser and a row click both set it
      tags$div(class = "sd-hidden",
        selectInput(ns("file_select"), NULL,
                    choices = c("All files (average)" = "all"), selectize = FALSE),
        # which of the three hero views the shared plot output draws
        selectInput(ns("hero_chart_type"), NULL,
                    choices = c("Timeline view" = "timeline", "Hourly Heatmap" = "heatmap",
                                "Bout Occurrence" = "occurrence"),
                    selected = "timeline", selectize = FALSE)),

      tags$div(id = ns("processing_indicator"), class = "sd-busy", style = "display: none;",
               tags$span(class = "sd-spinner", `aria-hidden` = "true"),
               tags$span("Analysing"))
    ),
    tags$script(HTML(sd_page_script(ns(""))))
  )
}

# The charts the page draws, in their groups
sd_charts <- function() {
  list(
    list(key = "timeline",       label = "Timeline",           out = "hero_chart",         group = "The day",      hero = "timeline"),
    list(key = "heatmap",        label = "Hourly heatmap",     out = "hero_chart",         group = "The day",      hero = "heatmap"),
    list(key = "occurrence",     label = "Bout occurrence",    out = "hero_chart",         group = "The day",      hero = "occurrence"),
    list(key = "histogram",      label = "Bout histogram",     out = "bout_histogram",     group = "Distribution"),
    list(key = "categories",     label = "Bout categories",    out = "bout_categories",    group = "Distribution"),
    list(key = "accumulation",   label = "Accumulation curve", out = "accumulation_curve", group = "Accumulation"),
    list(key = "survival",       label = "Survival curve",     out = "survival_curve",     group = "Accumulation"),
    list(key = "hourlybouts",    label = "Bouts by hour",      out = "hourly_bouts",       group = "Hourly"),
    list(key = "hourlyduration", label = "Duration by hour",   out = "hourly_duration",    group = "Hourly"),
    list(key = "transitions",    label = "State transitions",  out = "transition_matrix",  group = "Transitions")
  )
}

# Settings panel: every parameter the run reads
sd_settings_panel <- function(ns) {
  field <- function(label, control, note) {
    tags$div(class = "sd-field",
      tags$div(class = "sd-field-k", label),
      control,
      tags$div(class = "sd-field-n", note))
  }
  num <- function(id, value, unit, ...) {
    tags$div(class = "sd-num",
      numericInput(ns(id), NULL, value = value, width = "100%", ...),
      tags$span(class = "sd-unit", `aria-hidden` = "true", unit))
  }

  tags$div(
    class = "sd-panel sd-settings-grid",
    tags$div(
      class = "sd-fields",
      field("Cut points",
            tags$div(class = "sd-select",
              selectInput(ns("cut_points"), NULL,
                          choices = c("Freedson (1998)" = "freedson", "CANHR (2025)" = "canhr"),
                          selected = "freedson", width = "100%", selectize = FALSE)),
            "what counts as sedentary"),
      field("Prolonged bout at", num("prolonged_threshold", 30, "min", min = 20, max = 60, step = 5),
            "what the Prolonged % column counts"),
      field("Gap bridging", num("min_break_length", 5, "min", min = 1, max = 10, step = 1),
            "Healy: a shorter break does not end a bout"),
      field("Sleep",
            tags$div(class = "sd-check",
              checkboxInput(ns("include_sleep"), "Count sleep as sedentary", value = FALSE)),
            "SBRN: sedentary is waking behaviour")
    ),
    tags$div(class = "sd-settings-foot",
      tags$span(class = "sd-spacer"),
      # rendered, because the note depends on the family
      uiOutput(ns("settings_foot"), inline = TRUE))
  )
}

# Page script. The menus open and close client side; Export starts the
# download inside the click itself, through a hidden iframe, since a browser
# allows a download only during a user action.
sd_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }
  function closeMenus() {
    document.querySelectorAll('.sd-page .sd-menu, .sd-page .sd-whomenu, .sd-page .sd-gomenu, .sd-page .sd-exportmenu').forEach(function (m) {
      m.style.display = 'none';
    });
    document.querySelectorAll('.sd-page .sd-pick, .sd-page .sd-who, .sd-page .sd-export-btn').forEach(function (b) {
      b.classList.remove('is-open');
    });
  }
  function toggleMenu(button, menu) {
    var open = menu && menu.style.display !== 'none';
    closeMenus();
    if (menu && !open) { menu.style.display = 'block'; button.classList.add('is-open'); }
  }

  // Go to: the scrolling columns start just right of the two pinned ones
  function gridOf(el) {
    var panel = el.closest('.sd-tablepanel');
    return panel && panel.querySelector('.sd-scroll');
  }
  function pinEdge(sc) {
    var pin = sc.querySelector('th.sd-p1');
    return (pin || sc).getBoundingClientRect()[pin ? 'right' : 'left'];
  }
  function markGroup(go, menu) {
    var sc = gridOf(go);
    if (!sc || !menu) return;
    var edge = pinEdge(sc), cur = 0;
    sc.querySelectorAll('th.sd-gg').forEach(function (t, i) {
      if (t.getBoundingClientRect().left <= edge + 1) cur = i;
    });
    menu.querySelectorAll('.sd-gi').forEach(function (it) {
      var on = Number(it.dataset.go) === cur;
      it.classList.toggle('is-on', on);
      it.querySelector('.sd-tick').textContent = on ? '\\u2713' : '';
    });
  }
  function goToGroup(item) {
    var sc = gridOf(item);
    var th = sc && sc.querySelectorAll('th.sd-gg')[Number(item.dataset.go)];
    if (th) sc.scrollLeft += th.getBoundingClientRect().left - pinEdge(sc);
  }

  function fireAll(row) {
    var ids = ['dl_workbook'];
    var sent = 0;
    ids.forEach(function (id) {
      var a = document.getElementById(NS + id);
      var href = a && a.getAttribute('href');
      if (!href) return;
      var f = document.createElement('iframe');
      f.style.display = 'none';
      f.src = href;
      document.body.appendChild(f);
      setTimeout(function () { if (f.parentNode) f.parentNode.removeChild(f); }, 120000);
      sent++;
    });
    var label = row.querySelector('.sd-ei-label');
    if (label) {
      if (!label.dataset.rest) label.dataset.rest = label.textContent;
      label.textContent = sent ? 'Workbook sent to your downloads folder' : 'Nothing to export yet';
      row.classList.toggle('is-done', sent > 0);
      clearTimeout(row._t);
      row._t = setTimeout(function () {
        label.textContent = label.dataset.rest;
        row.classList.remove('is-done');
        closeMenus();
      }, 2600);
    }
  }

  document.addEventListener('click', function (e) {
    if (!e.target.closest || !e.target.closest('.sd-page')) { closeMenus(); return; }

    var go = e.target.closest('.sd-page .sd-goto');
    if (go) {
      var gm = go.parentNode.querySelector('.sd-gomenu');
      markGroup(go, gm);
      toggleMenu(go, gm);
      return;
    }

    var gi = e.target.closest('.sd-page .sd-gi[data-go]');
    if (gi) { closeMenus(); goToGroup(gi); return; }

    var pick = e.target.closest('.sd-page .sd-pick');
    if (pick) { toggleMenu(pick, document.querySelector('.sd-page .sd-menu')); return; }

    var mi = e.target.closest('.sd-page .sd-mi[data-chart]');
    if (mi) { closeMenus(); setVal('chart_pick', mi.dataset.chart); return; }

    var who = e.target.closest('.sd-page .sd-who');
    if (who) { toggleMenu(who, document.querySelector('.sd-page .sd-whomenu')); return; }

    var wi = e.target.closest('.sd-page .sd-wi[data-who]');
    if (wi) {
      var fid = wi.dataset.who;
      document.querySelectorAll('.sd-page .sd-row[data-fid]').forEach(function (r) {
        r.classList.toggle('is-selected', fid !== 'all' && r.dataset.fid === fid);
      });
      closeMenus();
      setVal('pick', fid);
      return;
    }

    var ex = e.target.closest('.sd-page .sd-export-btn');
    if (ex) { toggleMenu(ex, document.getElementById(NS + 'export_menu')); return; }

    var all = e.target.closest('.sd-page .sd-ei-all');
    if (all) { fireAll(all); return; }

    if (e.target.closest('.sd-page .sd-exportmenu')) return;

    var row = e.target.closest('.sd-page .sd-row[data-fid]');
    if (row) {
      var was = row.classList.contains('is-selected');
      document.querySelectorAll('.sd-page .sd-row.is-selected').forEach(function (r) { r.classList.remove('is-selected'); });
      if (!was) row.classList.add('is-selected');
      setVal('pick', was ? 'all' : row.dataset.fid);
      closeMenus();
      return;
    }

    closeMenus();
  });

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') { closeMenus(); return; }
    if (!e.target.closest || !e.target.closest('.sd-page')) return;
    var row = e.target.closest('.sd-row[data-fid]');
    if (!row) return;
    var rows = Array.prototype.slice.call(document.querySelectorAll('.sd-page .sd-row[data-fid]'));
    var i = rows.indexOf(row);
    var next = null;
    if (e.key === 'ArrowDown') next = rows[Math.min(i + 1, rows.length - 1)];
    else if (e.key === 'ArrowUp') next = rows[Math.max(i - 1, 0)];
    else if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); row.click(); return; }
    if (next) { e.preventDefault(); next.focus(); next.click(); }
  });
})();
"
  sub("__NS__", ns_prefix, js, fixed = TRUE)
}

mod_sedentary_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())
    not_scored <- reactiveVal(list())

    # The Counts/Raw switch picks which recordings and which threshold, never
    # which analysis; both sides run this page's engine
    local_fam <- reactiveVal(NULL)
    res_fam <- reactiveValues(counts = list(), raw = list())
    not_scored_fam <- reactiveValues(counts = list(), raw = list())

    observe({
      fams <- circ_families(shared)
      cur <- isolate(local_fam())
      if (length(fams) == 0) return(invisible(NULL))
      if (is.null(cur) || !cur %in% fams) local_fam(circ_default_family(shared))
    })
    observeEvent(input$sd_fam_counts, local_fam("counts"))
    observeEvent(input$sd_fam_raw, local_fam("raw"))

    # The recording ids one family can show
    fam_ids <- function(f) {
      if (identical(f, "raw")) names(shared$raw %||% list()) else names(shared$files %||% list())
    }

    # Each side keeps its own run, and a choice from the other side does not carry over
    observeEvent(local_fam(), {
      f <- local_fam() %||% "counts"
      results(res_fam[[f]] %||% list())
      not_scored(not_scored_fam[[f]] %||% list())
      cur <- input$file_select %||% "all"
      if (!identical(cur, "all") && !cur %in% fam_ids(f)) {
        updateSelectInput(session, "file_select",
                          selected = focus_get(shared, among = fam_ids(f)) %||% "all")
      }
    }, ignoreNULL = FALSE)

    on_tab_shown(shared, "sedentary", function() {
      id <- focus_get(shared, among = fam_ids(local_fam() %||% "counts"))
      if (!is.null(id)) updateSelectInput(session, "file_select", selected = id)
    })

    output$switch <- renderUI({
      fams <- circ_families(shared)
      if (length(fams) < 2) return(NULL)
      f <- local_fam() %||% "counts"
      seg <- function(key, label, n) {
        tags$button(id = ns(paste0("sd_fam_", key)), type = "button",
                    class = paste("action-button ovr-seg-b", if (identical(f, key)) "is-on" else ""),
                    label, tags$b(fmt_int(n)))
      }
      tags$div(class = "ovr-seg", role = "tablist",
               seg("counts", "Counts", circ_n_counts(shared)),
               seg("raw", "Raw", circ_n_raw(shared)))
    })

    output$settings_foot <- renderUI({
      tags$span(class = "sd-field-n",
        if (identical(local_fam() %||% "counts", "raw"))
          "Wear comes from GGIR part 2 and the sleep window from part 3, already settled by the read."
        else
          "Needs the Wear time and Sleep analyses to have been run.")
    })
    outputOptions(output, "settings_foot", suspendWhenHidden = FALSE)

    # The threshold list follows the family: count cut points, or the light
    # boundary of the published raw cut points
    observe({
      ch <- sed_cut_choices(shared, local_fam() %||% "counts")
      if (length(ch) == 0) return(invisible(NULL))
      cur <- isolate(input$cut_points)
      sel <- if (!is.null(cur) && cur %in% unname(ch)) cur else unname(ch)[1]
      updateSelectInput(session, "cut_points", choices = ch, selected = sel)
    })

    # Drop the results of recordings removed on Overview, on both families
    # (raw ids are never in names(shared$files))
    sd_prune <- function() {
      live <- c(names(shared$files %||% list()), names(shared$raw %||% list()))
      for (f in c("counts", "raw")) {
        r <- res_fam[[f]] %||% list()
        if (!length(r)) next
        keep <- intersect(names(r), live)
        if (length(keep) != length(r)) res_fam[[f]] <- r[keep]
      }
      res <- results()
      if (!length(res)) return(invisible(NULL))
      keep <- intersect(names(res), live)
      if (length(keep) == length(res)) return(invisible(NULL))
      results(res[keep])
      sh <- shared$results$sedentary %||% list()
      drop <- setdiff(names(res), keep)
      if (length(drop)) shared$results$sedentary <- sh[setdiff(names(sh), drop)]
      invisible(NULL)
    }
    observeEvent(names(shared$files), sd_prune(), ignoreNULL = FALSE)
    observeEvent(names(shared$raw), sd_prune(), ignoreNULL = FALSE)
    run_stamp <- reactiveVal(NULL)

    # file_select carries file ids, with "all" for the cohort

    cut_label <- reactive({
      paste0("Sedentary: ", sed_cut_label(shared, input$cut_points,
                                          local_fam() %||% "counts"))
    })

    chart_pick <- reactiveVal("timeline")
    observeEvent(input$chart_pick, {
      chart_pick(input$chart_pick %||% "timeline")
    })

    # A pick by chooser or row click is the user's, so it becomes the focus
    observeEvent(input$pick, {
      updateSelectInput(session, "file_select", selected = input$pick %||% "all")
      focus_set(shared, input$pick)
    })

    # Kept here because output$rule redraws Change and would lose a client class
    settings_open <- reactiveVal(FALSE)
    observeEvent(input$toggle_settings, {
      settings_open(!settings_open())
      shinyjs::toggle("settings_panel", condition = settings_open())
    })

    sel_fid <- reactive({
      s <- input$file_select %||% "all"
      if (identical(s, "all") || identical(s, "none")) NULL else s
    })

    # The Summary table's frame
    summary_df <- reactive({
      res <- results()
      if (length(res) == 0) return(NULL)
      tryCatch(sedentary_metrics_df(res, shared, cut_label()), error = function(e) {
        message("sedentary_metrics_df failed: ", conditionMessage(e)); NULL
      })
    })
    fid_by_row <- reactive({
      res <- results()
      vapply(res, function(r) as.character(r$file_id), character(1), USE.NAMES = FALSE)
    })

    # The rule bar
    run_params <- reactive({
      r <- results()
      if (length(r) == 0) NULL else r[[1]]$parameters
    })
    settings_moved <- reactive({
      settings_moved_from(run_params(), input, list(
        cut_points = "cut_points", prolonged_threshold = "prolonged_threshold",
        min_break_length = "min_break_length", include_sleep = "include_sleep"))
    })

    # One bar for both families, as label and value boxes. The counts side
    # keeps its three-state wear and sleep wording: "excluded" and "excluded,
    # sleep not scored" are different claims.
    sd_rule_boxes <- function(p, n, fam) {
      g <- function(k, v, sub = NULL) {
        tags$span(class = "wt-rule-g",
          tags$span(class = "wt-rule-k", k),
          tags$span(class = "wt-rule-v", tags$b(v),
                    if (!is.null(sub)) tags$span(class = "sd-rule-sub", sub)))
      }
      raw <- identical(fam, "raw")
      gap <- p$min_break_length %||% input$min_break_length %||% 5
      # Resolved as the run resolves it
      ckey <- sed_resolve_cut(shared, p$cut_points %||% input$cut_points, fam)
      spec <- if (raw) sed_cut_spec(shared, ckey) else NULL

      thresh <- if (raw) {
        if (!is.null(spec)) paste0(format(spec$light, trim = TRUE), " mg") else "not set"
      } else if (identical(ckey, "canhr")) "150 CPM" else "100 CPM"
      study <- if (raw) {
        if (!is.null(spec)) paste(spec$study, spec$group) else NULL
      } else if (identical(ckey, "canhr")) "CANHR 2025" else "Freedson 1998"

      wear_v <- if (raw) "GGIR part 2"
                else if (length(shared$results$wear_time) > 0) "Wear time tab" else "not run"
      # Where the waking-hours window came from, as the run recorded it
      wake_v <- if (!is.null(p)) {
        if (isTRUE(p$include_sleep)) "sleep included"
        else switch(p$sleep_source %||% "none",
                    "none" = "not scored",
                    "Sleep tab" = "Sleep tab",
                    "Cole-Kripke fallback" = "Cole-Kripke, in this tab",
                    p$sleep_source)
      } else if (isTRUE(input$include_sleep)) {
        "sleep included"
      } else if (raw) {
        "GGIR part 3, T5A5"
      } else if (length(shared$results$sleep) == 0) {
        "Cole-Kripke, in this tab"
      } else "Sleep tab"

      tags$div(class = "sd-panel sd-rule sd-rule--boxes",
        g("Sedentary below", thresh, study),
        g("Metric", if (raw) {
            if (is.null(spec)) "not set" else paste0(spec$metric, ", mg")
          } else "axis1, counts"),
        g("Epoch", paste0(fmt_int(p$epoch_length %||% circ_target_epoch(shared, fam)), " s")),
        g("Wear", wear_v),
        g("Waking hours", wake_v),
        g("Gap bridging", paste0(fmt_int(gap), " min")),
        tags$span(class = "wt-rule-g",
          tags$span(class = "wt-rule-k", "Scored"),
          tags$span(class = "wt-rule-v",
            if (n == 0) "not run yet" else tags$b(fmt_int(n)))),
        if (settings_moved()) stale_note("sd") else NULL,
        tags$span(class = "sd-rule-actions",
          actionButton(ns("toggle_settings"), "Change",
                       class = if (settings_open()) "sd-btn sd-btn--secondary is-open" else "sd-btn sd-btn--secondary",
                       `aria-expanded` = if (settings_open()) "true" else "false"),
          tags$span(class = "sd-export-wrap",
            tags$span(class = paste("sd-btn sd-btn--secondary sd-export-btn",
                                    if (n == 0) "is-quiet" else ""),
                      "Export", tags$span(class = "sd-car", `aria-hidden` = "true", HTML("&#9660;"))),
            sd_export_menu(ns, n > 0)),
          actionButton(ns("analyze"), if (n == 0) "Run analysis" else "Re-run",
                       class = paste(run_button_class("sd", n > 0, settings_moved()), "sd-run"))))
    }

    output$rule <- renderUI({
      p <- run_params()
      sd_rule_boxes(p, length(results()),
                    p$family %||% local_fam() %||% "counts")
    })

    # The figures, scoped to whatever is chosen
    output$figures <- renderUI({
      if (length(results()) == 0) return(NULL)
      # current_frag() pools every bout across the chosen recordings and
      # recomputes, which is not the mean of the per-recording columns
      cf <- tryCatch(current_frag(), error = function(e) NULL)
      if (is.null(cf)) return(NULL)
      sel <- sel_fid()
      dash <- "–"
      val <- function(x, digits) {
        if (is.null(x) || length(x) == 0 || is.na(x)) return(dash)
        fmt_dec(x, digits)
      }
      n_res <- length(results())
      who <- if (is.null(sel)) fmt_int(n_res) else {
        r <- results()[[sel]]
        as.character(r$subject_id %||% r$name %||% sel)
      }

      tags$div(
        class = "sd-panel sd-figs",
        sd_fig(who, if (is.null(sel) && n_res != 1) "Recordings" else "Recording"),
        sd_rule_div(),
        sd_fig(val(cf$total_sedentary_min / 60, 1), "Sedentary", "total", unit = "h"),
        sd_rule_div(),
        sd_fig(val(cf$breaks_per_sed_hour, 1), "Breaks", "per hour", unit = "/hr"),
        sd_rule_div(),
        sd_fig(val(cf$W50, 1), "Typical bout", "W50", unit = "min"),
        sd_rule_div(),
        sd_fig(val(cf$alpha, 2), "Alpha", "power law")
      )
    })

    # Plot panel
    output$plot_panel <- renderUI({
      res <- results()
      if (length(res) == 0) {
        return(tags$div(class = "sd-panel sd-plotpanel",
          tags$div(class = "sd-empty",
            tags$div(class = "sd-empty-t", "No sedentary results"),
            tags$div(class = "sd-empty-m",
                     "Run the analysis to detect bouts and fill the table below."))))
      }
      charts <- sd_charts()
      keys <- vapply(charts, function(c) c$key, character(1))
      cur <- chart_pick() %||% "timeline"
      if (!(cur %in% keys)) cur <- "timeline"
      ch <- charts[[match(cur, keys)]]

      sel <- sel_fid()
      label_of <- function(fid) {
        r <- res[[fid]]
        if (is.null(r)) fid else as.character(r$subject_id %||% r$name %||% fid)
      }
      who_label <- if (is.null(sel)) paste("All", pluralize(length(res), "recording")) else label_of(sel)
      covers <- if (is.null(sel)) paste("all", fmt_int(length(res))) else label_of(sel)

      last_group <- ""
      items <- lapply(charts, function(c) {
        head <- if (!identical(c$group, last_group)) {
          last_group <<- c$group
          tags$div(class = "sd-mi-head", c$group)
        } else NULL
        tagList(head,
          tags$div(class = paste("sd-mi", if (identical(c$key, ch$key)) "is-on" else ""),
                   `data-chart` = c$key,
                   tags$span(class = "sd-tick", `aria-hidden` = "true",
                             if (identical(c$key, ch$key)) HTML("&#10003;") else ""),
                   c$label,
                   tags$span(class = "sd-sc", covers)))
      })

      tags$div(class = "sd-panel sd-plotpanel",
        tags$div(class = "sd-plotbar",
          tags$span(class = "sd-pick", tabindex = "0",
                    ch$label, tags$span(class = "sd-car", `aria-hidden` = "true", HTML("&#9660;"))),
          tags$div(class = "sd-menu", style = "display: none;", items),

          tags$span(class = "sd-who-wrap",
            tags$span(class = "sd-who", tabindex = "0",
                      who_label, tags$span(class = "sd-car", `aria-hidden` = "true", HTML("&#9660;"))),
            tags$div(class = "sd-whomenu", style = "display: none;",
              tags$div(class = paste("sd-wi", if (is.null(sel)) "is-on" else ""), `data-who` = "all",
                       tags$span(class = "sd-tick", `aria-hidden` = "true",
                                 if (is.null(sel)) HTML("&#10003;") else ""),
                       "All recordings",
                       tags$span(class = "sd-sc", fmt_int(length(res)))),
              tags$div(class = "sd-wi-rule", `aria-hidden` = "true"),
              lapply(names(res), function(fid)
                tags$div(class = paste("sd-wi", if (identical(fid, sel)) "is-on" else ""),
                         `data-who` = fid,
                         tags$span(class = "sd-tick", `aria-hidden` = "true",
                                   if (identical(fid, sel)) HTML("&#10003;") else ""),
                         label_of(fid)))))),
        tags$div(class = "sd-plotwrap", plotOutput(ns(ch$out), height = "100%")))
    })

    # The three hero views share one plot output; choosing one sets its type
    observeEvent(chart_pick(), {
      charts <- sd_charts()
      keys <- vapply(charts, function(c) c$key, character(1))
      ch <- charts[[match(chart_pick(), keys)]]
      if (!is.null(ch$hero)) {
        updateSelectInput(session, "hero_chart_type", selected = ch$hero)
      }
    }, ignoreInit = FALSE)

    # Table panel
    output$table_panel <- renderUI({
      df <- summary_df()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      sel <- sel_fid()
      n <- if (is.null(sel)) nrow(df) else sum(fid_by_row() == sel)
      thr <- input$prolonged_threshold %||% 30

      # One narrowed frame for both the caption count and the grid
      pdf <- sd_page_columns(df, local_fam() %||% "counts")
      tags$div(class = "sd-panel sd-tablepanel",
        tags$div(class = "sd-table-head",
          tags$span(class = "sd-table-title", "Sitting",
            tags$span(class = "sd-table-sub",
                      paste0(fmt_int(n), if (n == 1) " recording · " else " recordings · ",
                             fmt_int(ncol(pdf)), " columns"))),
          sd_goto(sd_grid_groups(pdf)),
          tags$span(class = "sd-key",
            tags$span(class = "sd-sw", tags$b(class = "long"),
                      paste0("in bouts of ", fmt_int(thr), " min or more")),
            tags$span(class = "sd-sw", tags$b(class = "short"), "the rest"))),
        # On a raw run the page drops the six Recording columns a gt3x cannot
        # fill; the workbook keeps them
        sd_summary_grid(pdf,
                        fid_by_row(), sel, thr))
    })
    # Update file selector when files change
    # Both stores: selecting an id that is not in the list deselects the
    # select and the input comes back NULL
    observe({
      files <- shared$files %||% list()
      raws <- shared$raw %||% list()
      if (length(files) == 0 && length(raws) == 0) return(invisible(NULL))
      choices <- c("All files (average)" = "all")
      for (fid in names(files)) {
        f <- files[[fid]]
        choices[f$subject_info$id %||% f$name %||% fid] <- fid
      }
      for (rid in names(raws)) {
        choices[tryCatch(ovr_subject(raws[[rid]]), error = function(e) rid)] <- rid
      }
      updateSelectInput(session, "file_select", choices = choices,
                        selected = isolate(input$file_select) %||% "all")
    })

    # Run Analysis
    observeEvent(input$analyze, {
      # Either family
      req(length(shared$files) > 0 || length(shared$raw %||% list()) > 0)

      # Check if wear time has been analyzed
      wt_results <- shared$results$wear_time
      # Raw recordings carry GGIR part 2's wear from the read, so the Wear
      # time tab is a precondition for counts only
      use_wear_time <- !is.null(wt_results) && length(wt_results) > 0

      # Warn if wear time not analyzed
      if (!use_wear_time && !identical(local_fam() %||% "counts", "raw")) {
        showNotification(
          HTML("<strong>Recommendation:</strong> Run Wear Time Analysis first for accurate results.<br>
                Currently, non-wear periods (0 counts) may be counted as sedentary."),
          type = "warning",
          duration = 8
        )
      }

      include_sleep <- input$include_sleep

      # One family per run; the threshold decides the metric
      fam0 <- local_fam() %||% "counts"
      src_all <- sed_sources(shared, input$cut_points %||% "freedson", fam0)
      srcs <- circ_scored(src_all)
      not_scored(circ_unscored(src_all))
      not_scored_fam[[fam0]] <- circ_unscored(src_all)
      if (length(srcs) == 0) {
        showNotification(paste0("No loaded recording can be scored under ",
                                sed_cut_label(shared, input$cut_points, fam0),
                                ". Pick another threshold."),
                         type = "warning", duration = 8)
        return(invisible(NULL))
      }

      all_results <- list()
      min_break_length <- input$min_break_length %||% 5

      withProgress(message = "Analyzing sedentary patterns...", value = 0, {
        n_files <- length(srcs)

        for (i in seq_along(srcs)) {
          bnd <- srcs[[i]]
          fid <- bnd$id

          setProgress(value = i / max(1L, length(srcs)), detail = bnd$name)

          # sed_sources() picked the metric the cut point was validated on,
          # brought the series to one epoch (raw in mg) and classified it.
          #
          # Wear and the sleep window come from the bundle too.
          tstamps      <- bnd$timestamps
          epoch_length <- bnd$epoch_length
          intensity    <- bnd$intensity
          # NULL when the family has no wear to give
          wear_mask    <- bnd$wear_time
          # The engine excludes the sleep window; sleep_state uses the "S"/"W"
          # convention, and the include_sleep box turns the mask off
          sleep_mask   <- if (isTRUE(include_sleep) || is.null(bnd$sleep_state)) NULL
                          else bnd$sleep_state %in% "S"


          fragmentation <- tryCatch({
            canhrActi::sedentary.fragmentation(
              intensity = intensity,
              timestamps = tstamps,
              wear_time = wear_mask,
              sleep_mask = sleep_mask,
              epoch_length = epoch_length,
              min_break_length = min_break_length,
              # Real Clauset bootstrap GoF backs the power-law label (~0.1s/file).
              bootstrap_gof = TRUE
            )
          }, error = function(e) {
            showNotification(paste(bnd$name, ":", e$message), type = "error")
            NULL
          })

          if (is.null(fragmentation)) next

          all_results[[fid]] <- list(
            file_id = fid,
            name = bnd$name,
            subject_id = bnd$subject_id,
            fragmentation = fragmentation,
            intensity = intensity,
            timestamps = tstamps,
            wear_mask = wear_mask,
            sleep_excluded = !is.null(sleep_mask),
            sleep_mask = sleep_mask,
            # What this run was computed with; the rule bar reads these
            parameters = list(
              # the resolved key the run scored against, not the input; the
              # workbook derives its metric from it
              cut_points = bnd$cut_key %||% input$cut_points %||% "freedson",
              family = fam0,
              epoch_length = epoch_length,
              threshold = bnd$threshold_txt,
              prolonged_threshold = input$prolonged_threshold %||% 30,
              min_break_length = input$min_break_length %||% 5,
              include_sleep = isTRUE(input$include_sleep),
              sleep_excluded = !is.null(sleep_mask),
              # as the adapter reports it: "Sleep tab", "Cole-Kripke fallback",
              # "GGIR part 3, T5A5" or "none"
              sleep_source = bnd$sleep_source %||% "none",
              include_sleep = isTRUE(include_sleep),
              # for the workbook's Provenance sheet
              wear_time_available = !is.null(bnd$wear_time),
              run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              package_version = as.character(utils::packageVersion("canhrActi"))
            )
          )
        }
      })

      results(all_results)
      res_fam[[fam0]] <- all_results
      run_stamp(Sys.time())
      # Merge: all_results holds only this family
      prev_sed <- shared$results$sedentary %||% list()
      prev_sed[names(all_results)] <- all_results
      shared$results$sedentary <- prev_sed
    })

    # Helper: Safely extract numeric value (handles NULL)
    safe_extract <- function(res_list, field) {
      vapply(res_list, function(r) {
        val <- r$fragmentation[[field]]
        if (is.null(val) || length(val) == 0) NA_real_ else as.numeric(val[1])
      }, FUN.VALUE = numeric(1))
    }

    # Helper: Get filtered results based on file selection
    # Placeholder before a run; each chart draws its own empty state
    sed_run_first <- function(msg = "Run the analysis to see results") {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                          size = 5, hjust = 0.5, color = "#64748b") +
        ggplot2::theme_void()
    }

    # Every bout in scope, pooled once. hour and date are derived per recording
    # before the rbind, since rbind on POSIXct keeps the first frame's timezone.
    pooled_bouts <- reactive({
      res <- filtered_results()
      if (length(res) == 0) return(NULL)
      parts <- lapply(res, function(r) {
        b <- r$fragmentation$bouts
        if (is.null(b) || nrow(b) == 0) return(NULL)
        b$hour <- as.integer(format(b$start_time, "%H"))
        b$date <- as.Date(format(b$start_time, "%Y-%m-%d"))
        b$subject <- r$subject_id
        b
      })
      parts <- Filter(Negate(is.null), parts)
      if (length(parts) == 0) return(NULL)
      do.call(rbind, parts)
    })

    filtered_results <- reactive({
      res <- results()
      if (length(res) == 0) return(list())

      # NULL before the select renders, and `if (sel == "all")` on NULL is an error
      sel <- input$file_select %||% "all"
      if (is.null(sel) || sel == "all") {
        res
      } else if (sel %in% names(res)) {
        res[sel]
      } else {
        res
      }
    })

    # Helper: Get current fragmentation data
    current_frag <- reactive({
      res <- results()
      req(length(res) > 0)

      # NULL before the select renders, and `if (sel == "all")` on NULL is an error
      sel <- input$file_select %||% "all"
      prolonged_thresh <- input$prolonged_threshold %||% 30

      if (sel == "all") {
        all_durations <- unlist(lapply(res, function(r) {
          if (!is.null(r$fragmentation$bouts)) r$fragmentation$bouts$duration_min else NULL
        }))
        abi_result <- if (length(all_durations) > 0) {
          canhrActi::activity.balance.index(all_durations)
        } else {
          list(ABI = NA_real_)
        }

        dist_type <- NA_character_
        for (r in res) {
          if (!is.null(r$fragmentation$distribution_fit)) {
            dist_type <- r$fragmentation$distribution_fit$best_model
            break
          }
        }

        total_sed_time <- sum(all_durations, na.rm = TRUE)
        prolonged_durations <- all_durations[all_durations >= prolonged_thresh]
        prolonged_time <- sum(prolonged_durations, na.rm = TRUE)
        prolonged_pct <- if (total_sed_time > 0) 100 * prolonged_time / total_sed_time else 0

        # Distribution-shape metrics (alpha/Gini/W50/SATP/central tendency) are
        # RE-ESTIMATED on the pooled bout pool, not averaged across per-file values.
        epl_pooled <- tryCatch(
          as.numeric(stats::median(diff(as.numeric(res[[1]]$timestamps)))),
          error = function(e) 60)
        if (!is.finite(epl_pooled) || epl_pooled <= 0) epl_pooled <- 60
        pooled <- canhrActi::bout.distribution.metrics(all_durations, epoch_length = epl_pooled)

        list(
          mode = "all",
          total_sedentary_min = mean(safe_extract(res, "total_sedentary_min"), na.rm = TRUE),
          total_bouts = sum(safe_extract(res, "total_bouts"), na.rm = TRUE),
          mean_bout_duration = pooled$mean_bout,
          median_bout_duration = pooled$median_bout,
          max_bout_duration = pooled$max_bout,
          breaks_per_sed_hour = mean(safe_extract(res, "breaks_per_sed_hour"), na.rm = TRUE),
          alpha = pooled$alpha,
          gini = pooled$gini,
          ASTP = mean(safe_extract(res, "ASTP"), na.rm = TRUE),
          SATP = pooled$SATP,
          W50 = pooled$W50,
          W25 = pooled$W25,
          W75 = pooled$W75,
          W90 = pooled$W90,
          prolonged_percent = prolonged_pct,
          prolonged_count = length(prolonged_durations),
          prolonged_threshold = prolonged_thresh,
          ABI = abi_result$ABI,
          dist_type = dist_type,
          # Weibull pooled on the bout pool; SRI/GoF averaged across recordings.
          weibull_shape = tryCatch(canhrActi::survival.weibull(all_durations)$shape,
                                   error = function(e) NA_real_),
          sedentary_regularity_index = mean(safe_extract(res, "sedentary_regularity_index"), na.rm = TRUE),
          alpha_gof_pvalue = mean(safe_extract(res, "alpha_gof_pvalue"), na.rm = TRUE)
        )
      } else if (sel %in% names(res)) {
        r <- res[[sel]]

        bout_durations <- if (!is.null(r$fragmentation$bouts)) r$fragmentation$bouts$duration_min else numeric(0)

        abi_result <- if (length(bout_durations) > 0) {
          canhrActi::activity.balance.index(bout_durations)
        } else {
          list(ABI = NA_real_)
        }

        dist_type <- if (!is.null(r$fragmentation$distribution_fit)) {
          r$fragmentation$distribution_fit$best_model
        } else {
          NA_character_
        }

        total_sed_time <- sum(bout_durations, na.rm = TRUE)
        prolonged_durations <- bout_durations[bout_durations >= prolonged_thresh]
        prolonged_time <- sum(prolonged_durations, na.rm = TRUE)
        prolonged_pct <- if (total_sed_time > 0) 100 * prolonged_time / total_sed_time else 0

        list(
          mode = "single",
          total_sedentary_min = r$fragmentation$total_sedentary_min,
          total_bouts = r$fragmentation$total_bouts,
          mean_bout_duration = r$fragmentation$mean_bout_duration,
          median_bout_duration = r$fragmentation$median_bout_duration,
          max_bout_duration = r$fragmentation$max_bout_duration,
          breaks_per_sed_hour = r$fragmentation$breaks_per_sed_hour,
          alpha = r$fragmentation$alpha,
          gini = r$fragmentation$gini,
          ASTP = r$fragmentation$ASTP,
          SATP = r$fragmentation$SATP,
          W50 = r$fragmentation$W50,
          W25 = r$fragmentation$W25,
          W75 = r$fragmentation$W75,
          W90 = r$fragmentation$W90,
          prolonged_percent = prolonged_pct,
          prolonged_count = length(prolonged_durations),
          prolonged_threshold = prolonged_thresh,
          ABI = abi_result$ABI,
          dist_type = dist_type,
          weibull_shape = r$fragmentation$weibull_shape,
          sedentary_regularity_index = r$fragmentation$sedentary_regularity_index,
          alpha_gof_pvalue = r$fragmentation$alpha_gof_pvalue
        )
      } else {
        NULL
      }
    })

    # HERO CHART - Daily Pattern
    output$hero_chart <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to visualize your sedentary behavior")
      } else {
        switch(input$hero_chart_type %||% "timeline",
          "heatmap"    = canhrActi::plot_sedentary_heatmap(pooled_bouts()),
          "occurrence" = canhrActi::plot_sedentary_occurrence(pooled_bouts()),
          canhrActi::plot_sedentary_timeline(pooled_bouts()))
      }
      })
    }, bg = "white")

    # BOUT ANALYSIS PLOTS

    output$bout_histogram <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to see results")
      } else {
        canhrActi::plot_bout_histogram_pooled(
          pooled_bouts(),
          # alpha from the fragmentation run (pooled in cohort mode)
          alpha = tryCatch(current_frag()$alpha, error = function(e) NA_real_),
          prolonged_threshold = input$prolonged_threshold %||% 30)
      }
      })
    }, bg = "white")

    # Bout categories
    output$bout_categories <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to see results")
      } else {
        canhrActi::plot_bout_categories(
          do.call(rbind, lapply(filtered_results(),
                                function(r) r$fragmentation$bout_distribution)))
      }
      })
    }, bg = "white")

    # Accumulation curve
    output$accumulation_curve <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to see results")
      } else {
        canhrActi::plot_bout_accumulation(pooled_bouts())
      }
      })
    }, bg = "white")

    # Survival curve (Kaplan-Meier style)
    output$survival_curve <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      res <- filtered_results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run the analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        # Collect bout durations from all subjects
        all_durations <- list()
        groups <- c()
        for (r in res) {
          bouts <- r$fragmentation$bouts
          if (!is.null(bouts) && nrow(bouts) > 0) {
            all_durations[[r$subject_id]] <- bouts$duration_min
            groups <- c(groups, r$subject_id)
          }
        }

        if (length(all_durations) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No bout data", size = 5) +
            ggplot2::theme_void()
        } else {
          # Use the new enhanced Kaplan-Meier survival visualization
          tryCatch({
            # If single subject, pass durations directly; if multiple, pass as list
            if (length(all_durations) == 1) {
              p <- canhrActi::plot_survival_curves(
                bout_durations = all_durations[[1]],
                groups = NULL,
                show_ci = TRUE,
                show_median = TRUE,
                max_time = NULL,
                title = "Sedentary bout survival analysis"
              )
              # Annotate the Weibull hazard direction of the bout durations.
              wb <- tryCatch(canhrActi::survival.weibull(all_durations[[1]]),
                             error = function(e) NULL)
              if (!is.null(wb) && isTRUE(wb$converged)) {
                # Use caption so the plot's own "n = .. | Median: .." subtitle stays.
                p <- p + ggplot2::labs(
                  caption = sprintf("Weibull shape k = %.2f: %s",
                                    wb$shape, wb$hazard_interpretation))
              }
              p
            } else {
              # Multiple subjects - combine into one vector with group labels
              combined_durations <- unlist(all_durations)
              combined_groups <- rep(names(all_durations), sapply(all_durations, length))

              canhrActi::plot_survival_curves(
                bout_durations = combined_durations,
                groups = combined_groups,
                show_ci = TRUE,
                show_median = TRUE,
                max_time = NULL,
                title = "Sedentary bout survival analysis"
              )
            }
          }, error = function(e) {
            # Fallback to original simple survival curve
            all_curves <- do.call(rbind, lapply(res, function(r) {
              if (!is.null(r$fragmentation$survival_curve)) {
                sc <- r$fragmentation$survival_curve
                sc$subject <- r$subject_id
                sc
              } else NULL
            }))

            if (is.null(all_curves) || nrow(all_curves) == 0) {
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No survival data", size = 5) +
                ggplot2::theme_void()
            } else {
              ggplot2::ggplot(all_curves, ggplot2::aes(x = time, y = survival_prob,
                                                       group = subject, color = subject)) +
                ggplot2::geom_step(alpha = 0.8, linewidth = 1) +
                ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "#94a3b8") +
                ggplot2::scale_y_continuous(labels = scales::percent_format()) +
                ggplot2::scale_color_brewer(palette = "Set2") +
                ggplot2::labs(title = "Bout survival curve", x = "Time (minutes)", y = "Survival Probability") +
                canhrActi::theme_canhrActi()
            }
          })
        }
      }
      })
    }, bg = "white")

    # Hourly bouts
    output$hourly_bouts <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to see results")
      } else {
        canhrActi::plot_hourly_bout_frequency(pooled_bouts())
      }
      })
    }, bg = "white")

    # Hourly duration
    output$hourly_duration <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to see results")
      } else {
        canhrActi::plot_hourly_bout_duration(
          pooled_bouts(), threshold = input$prolonged_threshold %||% 30)
      }
      })
    }, bg = "white")

    # Transition matrix
    output$transition_matrix <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      if (length(filtered_results()) == 0) {
        sed_run_first("Run the analysis to see results")
      } else {
        # ASTP/SATP from the fragmentation run (SATP pooled in cohort mode)
        canhrActi::plot_transition_matrix(current_frag())
      }
      })
    }, bg = "white")

    # CSV EXPORT
    # REPRODUCIBLE MULTI-SHEET XLSX WORKBOOK (supersedes the Summary + Bout CSVs)
    output$dl_workbook <- downloadHandler(
      filename = function() {
        paste0("sedentary_workbook_",
               format(run_stamp() %||% Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        res <- results()
        validate(need(length(res) > 0, "Run the analysis first."))
        sedentary_write_workbook(file, res, shared, metric = cut_label())
      }
    )

    # The link sits in a closed menu, and a suspended downloadHandler never
    # receives its href
    outputOptions(output, "dl_workbook", suspendWhenHidden = FALSE)
  })
}
