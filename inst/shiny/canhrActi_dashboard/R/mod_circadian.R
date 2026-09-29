# Circadian Rhythm Analysis Module
# Rule bar, figures, one plot panel, then one table with a row per recording
# (the CSV export in full) and an L5/M10 strip on a clock.

mod_circadian_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "cr-page",
      uiOutput(ns("switch"), class = "cr-out"),
      uiOutput(ns("rule"), class = "cr-out"),
      uiOutput(ns("coverage"), class = "cr-out"),
      shinyjs::hidden(
        tags$div(id = ns("settings_panel"), class = "cr-settings", cr_settings_panel(ns))),
      uiOutput(ns("figures"), class = "cr-out"),
      uiOutput(ns("plot_panel"), class = "cr-out cr-plot-out"),
      uiOutput(ns("table_panel"), class = "cr-out cr-table-out"),

      # Page scope. The chooser and a row click both set it; current_data()
      # and the charts read from it.
      tags$div(class = "cr-hidden",
        selectInput(ns("file_select"), NULL,
                    choices = c("All files (average)" = "all"), selectize = FALSE)),

      tags$div(id = ns("processing_indicator"), class = "cr-busy", style = "display: none;",
               tags$span(class = "cr-spinner", `aria-hidden` = "true"),
               tags$span("Analysing"))
    ),
    tags$script(HTML(cr_page_script(ns(""))))
  )
}

# The seven charts, in tab order. `all` marks the two that draw every
# recording; the others draw one and fall back to the first on the cohort.
cr_charts <- function() {
  list(
    list(key = "profile",    label = "24-hour profile",  all = TRUE),
    list(key = "actogram",   label = "Actogram",         all = FALSE),
    list(key = "cosinor",    label = "Cosinor fit",      all = TRUE),
    list(key = "periodogram", label = "Periodogram",     all = FALSE),
    list(key = "chisq",      label = "Chi-square",       all = FALSE),
    list(key = "extcosinor", label = "Extended cosinor", all = FALSE),
    list(key = "dfa",        label = "DFA",              all = FALSE)
  )
}

# Settings panel: the two parameters and the two fixed constants
cr_settings_panel <- function(ns) {
  field <- function(label, control, note) {
    tags$div(class = "cr-field",
      tags$div(class = "cr-field-k", label),
      control,
      tags$div(class = "cr-field-n", note))
  }
  fixed <- function(label, value, note) {
    tags$div(class = "cr-field",
      tags$div(class = "cr-field-k", label),
      tags$div(class = "cr-fixed", value),
      tags$div(class = "cr-field-n", note))
  }

  tags$div(
    class = "cr-panel cr-settings-grid",
    tags$div(
      class = "cr-fields",
      field("Activity metric",
            tags$div(class = "cr-select",
              selectInput(ns("metric"), NULL,
                          choices = c("Axis 1 (vertical)" = "axis1", "Vector magnitude" = "vm"),
                          selected = "axis1", width = "100%", selectize = FALSE)),
            "what every metric is read from"),
      field("Wear time filter",
            tags$div(class = "cr-check", checkboxInput(ns("use_wear_time"), "Drop non-wear epochs", value = TRUE)),
            "needs wear time run first"),
      fixed("Cosinor period", "24 h", "fixed by the model"),
      fixed("Second harmonic", "12 h", "what is_bimodal is judged on")
    )
  )
}

# Menus open and close on the page; only the choice is sent. Export starts both
# downloads inside the click, through hidden iframes: a browser allows a download
# only while the click still counts as a user action, about a second.
cr_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }
  function closeMenus() {
    document.querySelectorAll('.cr-page .cr-menu, .cr-page .cr-whomenu, .cr-page .cr-exportmenu').forEach(function (m) {
      m.style.display = 'none';
    });
    document.querySelectorAll('.cr-page .cr-pick, .cr-page .cr-who, .cr-page .cr-export-btn').forEach(function (b) {
      b.classList.remove('is-open');
    });
  }
  function toggleMenu(button, menu) {
    var open = menu && menu.style.display !== 'none';
    closeMenus();
    if (menu && !open) { menu.style.display = 'block'; button.classList.add('is-open'); }
  }

  function fireAll(row) {
    var ids = ['dl_csv', 'dl_workbook'];
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
    var label = row.querySelector('.cr-ei-label');
    if (label) {
      if (!label.dataset.rest) label.dataset.rest = label.textContent;
      label.textContent = sent === ids.length
        ? sent + ' files sent to your downloads folder'
        : (sent === 0 ? 'Nothing to export yet' : sent + ' of ' + ids.length + ' sent; try again');
      row.classList.toggle('is-done', sent === ids.length);
      clearTimeout(row._t);
      row._t = setTimeout(function () {
        label.textContent = label.dataset.rest;
        row.classList.remove('is-done');
        closeMenus();
      }, 2600);
    }
  }

  document.addEventListener('click', function (e) {
    if (!e.target.closest || !e.target.closest('.cr-page')) { closeMenus(); return; }

    var pick = e.target.closest('.cr-page .cr-pick');
    if (pick) { toggleMenu(pick, document.querySelector('.cr-page .cr-menu')); return; }

    var mi = e.target.closest('.cr-page .cr-mi[data-chart]');
    if (mi) { closeMenus(); setVal('active_tab', mi.dataset.chart); return; }

    var who = e.target.closest('.cr-page .cr-who');
    if (who) { toggleMenu(who, document.querySelector('.cr-page .cr-whomenu')); return; }

    var wi = e.target.closest('.cr-page .cr-wi[data-who]');
    if (wi) {
      var fid = wi.dataset.who;
      document.querySelectorAll('.cr-page .cr-row[data-fid]').forEach(function (r) {
        r.classList.toggle('is-selected', fid !== 'all' && r.dataset.fid === fid);
      });
      closeMenus();
      setVal('pick', fid);
      return;
    }

    var ex = e.target.closest('.cr-page .cr-export-btn');
    if (ex) { toggleMenu(ex, document.getElementById(NS + 'export_menu')); return; }

    var all = e.target.closest('.cr-page .cr-ei-all');
    if (all) { fireAll(all); return; }

    if (e.target.closest('.cr-page .cr-exportmenu')) return;

    var row = e.target.closest('.cr-page .cr-row[data-fid]');
    if (row) {
      var was = row.classList.contains('is-selected');
      document.querySelectorAll('.cr-page .cr-row.is-selected').forEach(function (r) { r.classList.remove('is-selected'); });
      if (!was) row.classList.add('is-selected');
      setVal('pick', was ? 'all' : row.dataset.fid);
      closeMenus();
      return;
    }

    closeMenus();
  });

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') { closeMenus(); return; }
    if (!e.target.closest || !e.target.closest('.cr-page')) return;
    var row = e.target.closest('.cr-row[data-fid]');
    if (!row) return;
    var rows = Array.prototype.slice.call(document.querySelectorAll('.cr-page .cr-row[data-fid]'));
    var i = rows.indexOf(row);
    var next = null;
    if (e.key === 'ArrowDown') next = rows[Math.min(i + 1, rows.length - 1)];
    else if (e.key === 'ArrowUp') next = rows[Math.max(i - 1, 0)];
    else if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); row.click(); return; }
    if (next) { e.preventDefault(); next.focus(); next.click(); }
  });

  // The table fades at its right edge while columns run on past it
  function edge(el) {
    var p = el.parentElement;
    if (!p) return;
    var more = el.clientWidth > 0 && el.scrollLeft + el.clientWidth < el.scrollWidth - 1;
    if (more) {
      p.style.setProperty('--cr-fade-top', el.offsetTop + 'px');
      p.style.setProperty('--cr-fade-h', el.clientHeight + 'px');
      p.style.setProperty('--cr-fade-right', (p.clientWidth - el.offsetLeft - el.clientWidth) + 'px');
    }
    p.classList.toggle('has-more', more);
  }
  var sized = window.ResizeObserver ? new ResizeObserver(function (list) {
    list.forEach(function (en) { edge(en.target); });
  }) : null;
  function watchTables() {
    document.querySelectorAll('.cr-page .cr-scroll').forEach(function (el) {
      if (el.dataset.crEdge) return;
      el.dataset.crEdge = '1';
      if (sized) sized.observe(el); else edge(el);
    });
  }
  document.addEventListener('scroll', function (e) {
    var t = e.target;
    if (t && t.classList && t.classList.contains('cr-scroll')) edge(t);
  }, true);
  if (!sized) window.addEventListener('resize', function () {
    document.querySelectorAll('.cr-page .cr-scroll').forEach(edge);
  });
  // A new table arrives with every render; only child changes are watched,
  // so the attributes written above cannot loop
  var page = document.querySelector('.cr-page');
  if (page && window.MutationObserver) {
    new MutationObserver(watchTables).observe(page, { childList: true, subtree: true });
  }
  watchTables();
})();
"
  sub("__NS__", ns_prefix, js, fixed = TRUE)
}

mod_circadian_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())
    not_scored <- reactiveVal(list())

    # One store per family, so a raw run and a counts run survive each other
    res_fam <- reactiveValues(counts = list(), raw = list())
    not_scored_fam <- reactiveValues(counts = list(), raw = list())

    # The family this run scores; the metric chooser narrows to its columns
    local_fam <- reactiveVal(NULL)

    observe({
      fams <- circ_families(shared)
      cur <- isolate(local_fam())
      if (length(fams) == 0) return(invisible(NULL))
      # Keep the chosen side while it has files, else fall to the one that does
      if (is.null(cur) || !cur %in% fams) local_fam(circ_default_family(shared))
    })

    observeEvent(input$cr_fam_counts, local_fam("counts"))
    observeEvent(input$cr_fam_raw, local_fam("raw"))

    # Show that family's own run
    observeEvent(local_fam(), {
      f <- local_fam() %||% "counts"
      results(res_fam[[f]] %||% list())
      not_scored(not_scored_fam[[f]] %||% list())
    }, ignoreNULL = FALSE)

    output$switch <- renderUI(circ_switch_ui(ns, shared, local_fam() %||% "counts"))

    # Metric choices for what is loaded, grouped, each labelled with the number
    # of recordings that can supply it. Updated in place to keep the markup.
    observe({
      ch <- circ_metric_choices(shared, local_fam() %||% "counts")
      if (length(ch) == 0) return(invisible(NULL))
      flat <- unname(ch)
      cur <- isolate(input$metric)
      sel <- if (!is.null(cur) && cur %in% flat) cur else flat[1]
      updateSelectInput(session, "metric", choices = ch, selected = sel)
    })

    # Drop the results of recordings removed on Overview, in both family stores.
    # Raw results are keyed by shared$raw ids, never by names(shared$files).
    circ_prune <- function() {
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
      # Narrow the shared copy too, leaving other entries alone
      sh <- shared$results$circadian %||% list()
      drop <- setdiff(names(res), keep)
      if (length(drop)) shared$results$circadian <- sh[setdiff(names(sh), drop)]
      invisible(NULL)
    }
    observeEvent(names(shared$files), circ_prune(), ignoreNULL = FALSE)
    observeEvent(names(shared$raw), circ_prune(), ignoreNULL = FALSE)
    active_tab <- reactiveVal("profile")
    run_stamp <- reactiveVal(NULL)
    settings_open <- reactiveVal(FALSE)

    # circadian_<what>_<ISO date>_<time>.<ext>, one stamp for both files.
    export_name <- function(what, ext) {
      paste0("circadian_", what, "_",
             format(run_stamp() %||% Sys.time(), "%Y-%m-%d_%H%M%S"), ".", ext)
    }

    # file_select carries file ids, with "all" for the cohort. A pick by the
    # chooser or a row click is the user's, so it becomes the app's focus.
    observeEvent(input$pick, {
      updateSelectInput(session, "file_select", selected = input$pick %||% "all")
      focus_set(shared, input$pick)
    })

    # A recording the run did not score reads as all, never as an empty view
    sel_fid <- reactive({
      s <- input$file_select %||% "all"
      if (identical(s, "all") || identical(s, "none")) return(NULL)
      res <- results()
      if (length(res) && !s %in% names(res)) NULL else s
    })

    # Shared by the table on screen and the CSV
    results_df <- reactive({
      res <- results()
      if (length(res) == 0) return(NULL)
      tryCatch(circadian_results_df(res), error = function(e) {
        message("circadian_results_df failed: ", conditionMessage(e)); NULL
      })
    })
    # File id per row of results_df(), which is in result order
    fid_by_row <- reactive({
      res <- results()
      vapply(res, function(r) as.character(r$file_id), character(1), USE.NAMES = FALSE)
    })

    run_params <- reactive({
      r <- results()
      if (length(r) == 0) NULL else r[[1]]$parameters
    })
    settings_moved <- reactive({
      settings_moved_from(run_params(), input, list(
        metric = "metric", use_wear_time = "use_wear_time"))
    })

    output$rule <- renderUI({
      p <- run_params()
      metric <- circ_metric_label(p$metric %||% input$metric %||% "axis1")
      # The run's epoch, not the files': both file types are resampled onto one
      # time base first, since DFA and multiscale entropy read the epoch directly.
      epochs <- as.numeric(p$epoch_length %||% CIRC_TARGET_EPOCH)
      epochs <- epochs[!is.na(epochs)]
      epoch_txt <- if (length(epochs) == 1 && is.finite(epochs)) paste0(fmt_int(epochs), " s epochs")
                   else NULL
      n <- length(results())

      tags$div(
        class = "cr-panel cr-rule",
        tags$span(class = "cr-rule-k", "Metric"),
        tags$span(class = "cr-rule-v",
          tags$b(metric), " · ",
          # Wear filter: off, asked for but not run, or on
          local({
            asked <- if (is.null(p)) isTRUE(input$use_wear_time) else isTRUE(p$use_wear_time)
            have <- if (is.null(p)) (length(shared$results$wear_time) > 0 || length(shared$raw %||% list()) > 0) else isTRUE(p$wear_time_available)
            if (!asked) tagList("wear time filter ", tags$b("off"))
            else if (!have) tagList(tags$b("wear time not run"), " · nothing filtered")
            else tagList("wear time filter ", tags$b("on"))
          }),
          if (!is.null(epoch_txt)) paste0(" · ", epoch_txt) else NULL),
        tags$span(class = "cr-rule-sep", `aria-hidden` = "true"),
        tags$span(class = "cr-rule-k", "Scored"),
        tags$span(class = "cr-rule-v",
          if (n == 0) "not run yet"
          else tagList(tags$b(fmt_int(n)), if (n == 1) " recording" else " recordings")),
        if (settings_moved()) stale_note("cr") else NULL,
        tags$span(class = "cr-rule-actions",
          # The toggle sets is-open itself; a later redraw reads the state
          actionButton(ns("toggle_settings"), "Change",
                       class = paste("cr-btn cr-btn--secondary",
                                     if (isolate(settings_open())) "is-open" else "")),
          tags$span(class = "cr-export-wrap",
            tags$span(class = paste("cr-btn cr-btn--secondary cr-export-btn",
                                    if (n == 0) "is-quiet" else ""),
                      "Export", tags$span(class = "cr-car", `aria-hidden` = "true", HTML("&#9660;"))),
            cr_export_menu(ns, n > 0)),
          actionButton(ns("run_btn"), if (n == 0) "Run analysis" else "Re-run",
                       class = run_button_class("cr", n > 0, settings_moved())))
      )
    })

    # Recordings the run left out (one metric per run), with the reason for each
    output$coverage <- renderUI({
      ns_ <- not_scored()
      if (length(ns_) == 0) return(NULL)
      n_ok <- length(results())
      m <- (run_params()$metric %||% input$metric) %||% "axis1"
      by_reason <- split(ns_, vapply(ns_, function(b) b$reason %||% "", character(1)))
      tags$div(class = "cr-panel cr-coverage",
        tags$div(class = "cr-cov-head",
          tags$b(sprintf("%s scored %s of %s recordings.",
                         circ_metric_label(m), fmt_int(n_ok),
                         fmt_int(n_ok + length(ns_)))),
          " The rest could not supply it."),
        tags$ul(class = "cr-cov-list",
          lapply(names(by_reason), function(rsn) {
            grp <- by_reason[[rsn]]
            tags$li(
              tags$b(paste0(fmt_int(length(grp)),
                            if (length(grp) == 1) " recording" else " recordings")),
              ": ", rsn,
              tags$span(class = "cr-cov-who",
                        paste(vapply(utils::head(grp, 6), function(b) b$name %||% b$id,
                                     character(1)), collapse = ", "),
                        if (length(grp) > 6) sprintf(" and %d more", length(grp) - 6) else ""))
          })))
    })

    # Figures: L5, M10, RA and IS over the chosen scope
    output$figures <- renderUI({
      cd <- tryCatch(current_data(), error = function(e) NULL)
      if (is.null(cd)) return(NULL)
      sel <- sel_fid()
      n <- length(results())
      num <- function(x, digits) if (is.null(x) || is.na(x)) "–" else fmt_dec(x, digits)
      # L5 and M10 are levels in the run's unit; mg are small, so they keep two decimals
      unit <- circ_unit(run_params()$metric %||% input$metric)
      level <- function(x, label, sub) {
        v <- if (is.null(x) || is.na(x)) "–"
             else if (identical(unit, "mg")) fmt_dec(x, 2) else fmt_int(round(x))
        cr_fig(v, label, sub, unit = if (!identical(v, "–")) unit)
      }

      who <- if (is.null(sel)) fmt_int(n) else {
        r <- results()[[sel]]
        as.character(r$subject_id %||% r$name %||% sel)
      }

      tags$div(
        class = "cr-panel cr-figs",
        cr_fig(who, if (is.null(sel) && n != 1) "Recordings" else "Recording"),
        cr_rule_div(),
        level(cd$L5, "L5", "least active 5 h"),
        cr_rule_div(),
        level(cd$M10, "M10", "most active 10 h"),
        cr_rule_div(),
        cr_fig(num(cd$RA, 3), "RA", "relative amplitude"),
        cr_rule_div(),
        cr_fig(num(cd$IS, 2), "IS", "interdaily stability")
      )
    })

    output$plot_panel <- renderUI({
      res <- results()
      if (length(res) == 0) {
        return(tags$div(class = "cr-panel cr-plotpanel",
          tags$div(class = "cr-empty",
            tags$div(class = "cr-empty-t", "No rhythm results"),
            tags$div(class = "cr-empty-m",
                     "Run the analysis to fit the rhythm for each recording."))))
      }
      charts <- cr_charts()
      keys <- vapply(charts, function(c) c$key, character(1))
      cur <- active_tab() %||% "profile"
      if (!(cur %in% keys)) cur <- "profile"
      ch <- charts[[match(cur, keys)]]

      sel <- sel_fid()
      label_of <- function(fid) {
        r <- res[[fid]]
        if (is.null(r)) fid else as.character(r$subject_id %||% r$name %||% fid)
      }
      first_label <- label_of(names(res)[1])
      who_label <- if (!is.null(sel)) label_of(sel)
                   else if (length(res) == 1) "1 recording"
                   else paste("All", fmt_int(length(res)), "recordings")

      # A one-recording chart names the recording it draws; an all-recordings
      # chart over all of them would only repeat the chooser
      scope <- if (isTRUE(ch$all)) {
        if (is.null(sel)) NULL else paste(label_of(sel), "only")
      } else if (is.null(sel)) {
        paste0("showing ", first_label, " · this chart draws one recording at a time")
      } else {
        paste0(label_of(sel), " · the selected recording")
      }

      tags$div(class = "cr-panel cr-plotpanel",
        tags$div(class = "cr-plotbar",
          tags$span(class = "cr-pick", tabindex = "0",
                    ch$label, tags$span(class = "cr-car", `aria-hidden` = "true", HTML("&#9660;"))),
          # Each entry names what it would draw for the current scope
          tags$div(class = "cr-menu", style = "display: none;",
            lapply(charts, function(c)
              tags$div(class = paste("cr-mi", if (identical(c$key, ch$key)) "is-on" else ""),
                       `data-chart` = c$key,
                       tags$span(class = "cr-tick", `aria-hidden` = "true",
                                 if (identical(c$key, ch$key)) HTML("&#10003;") else ""),
                       c$label,
                       tags$span(class = "cr-sc",
                                 if (!is.null(sel)) label_of(sel)
                                 else if (isTRUE(c$all)) "all recordings"
                                 else first_label)))),

          tags$span(class = "cr-who-wrap",
            tags$span(class = "cr-who", tabindex = "0",
                      who_label, tags$span(class = "cr-car", `aria-hidden` = "true", HTML("&#9660;"))),
            tags$div(class = "cr-whomenu", style = "display: none;",
              tags$div(class = paste("cr-wi", if (is.null(sel)) "is-on" else ""), `data-who` = "all",
                       tags$span(class = "cr-tick", `aria-hidden` = "true",
                                 if (is.null(sel)) HTML("&#10003;") else ""),
                       "All recordings",
                       tags$span(class = "cr-sc", "five charts fall back")),
              tags$div(class = "cr-wi-rule", `aria-hidden` = "true"),
              lapply(names(res), function(fid) {
                cov <- res[[fid]]$coverage_percent
                tags$div(class = paste("cr-wi", if (identical(fid, sel)) "is-on" else ""),
                         `data-who` = fid,
                         tags$span(class = "cr-tick", `aria-hidden` = "true",
                                   if (identical(fid, sel)) HTML("&#10003;") else ""),
                         label_of(fid),
                         tags$span(class = "cr-sc",
                                   if (is.null(cov) || is.na(cov)) "" else paste0(fmt_dec(cov, 1), "% covered")))
              }))),

          if (!is.null(scope)) tags$span(class = "cr-scope", scope)),
        tags$div(class = "cr-plotwrap", plotOutput(ns("main_chart"), height = "100%")))
    })

    output$table_panel <- renderUI({
      df <- results_df()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      sel <- sel_fid()
      n <- if (is.null(sel)) nrow(df) else sum(fid_by_row() == sel)

      tags$div(class = "cr-panel cr-tablepanel",
        tags$div(class = "cr-table-head",
          tags$span(class = "cr-table-title", "Rhythm",
            tags$span(class = "cr-table-sub",
                      paste0(fmt_int(n), if (n == 1) " recording · " else " recordings · ",
                             fmt_int(ncol(df)), " columns"))),
          tags$span(class = "cr-key",
            tags$span(class = "cr-sw", tags$b(class = "l5"), "L5, least active 5 h"),
            tags$span(class = "cr-sw", tags$b(class = "m10"), "M10, most active 10 h"))),
        # The level heads name the run's unit, so counts and mg never read alike
        cr_results_grid(df, fid_by_row(), sel,
                        unit = circ_unit(run_params()$metric %||% input$metric)))
    })

    # Toggle settings panel; Change shows it is open, as Export does
    observeEvent(input$toggle_settings, {
      open <- !settings_open()
      settings_open(open)
      shinyjs::toggle("settings_panel", condition = open)
      shinyjs::toggleClass("toggle_settings", "is-open", condition = open)
    })

    # Handle tab clicks
    observeEvent(input$active_tab, {
      active_tab(input$active_tab)
    })

    # Update file selector with the recordings on this side of the switch
    observe({
      fam <- local_fam() %||% "counts"
      # names(list()) is NULL and setNames(NULL, character(0)) is an error, so
      # an empty side is forced to character(0)
      ids <- if (identical(fam, "raw")) names(shared$raw %||% list())
             else names(shared$files %||% list())
      if (is.null(ids)) ids <- character(0)
      recs <- if (length(ids) == 0) character(0) else if (identical(fam, "raw")) {
        rs <- shared$raw
        stats::setNames(ids, vapply(ids,
          function(id) tryCatch(ovr_name(rs[[id]], id), error = function(e) id), character(1)))
      } else {
        fs <- shared$files
        stats::setNames(ids, vapply(ids, function(id)
          tryCatch(paste0(fs[[id]]$subject_info$id, " - ", fs[[id]]$name),
                   error = function(e) id), character(1)))
      }
      if (length(recs) == 0) {
        updateSelectInput(session, "file_select", choices = c("No files loaded" = "none"))
      } else {
        lbl <- if (length(recs) == 1) "The 1 recording" else paste0("All ", length(recs), " recordings (average)")
        updateSelectInput(session, "file_select",
                          choices = c(stats::setNames("all", lbl), recs))
      }
    })

    # Opening the tab takes up the recording picked on another tab, if it is
    # on this side of the switch
    on_tab_shown(shared, "circadian", function() {
      fam <- local_fam() %||% "counts"
      ids <- if (identical(fam, "raw")) names(shared$raw %||% list()) else names(shared$files %||% list())
      id <- focus_get(shared, among = ids)
      if (!is.null(id)) updateSelectInput(session, "file_select", selected = id)
    })

    # Run Analysis
    observeEvent(input$run_btn, {
      # Gate on what the chosen metric can reach, not on shared$file_count:
      # a gt3x-only study has no counts files
      fam0 <- local_fam() %||% "counts"
      src_all <- circ_sources(shared, input$metric %||% "axis1",
                              circ_target_epoch(shared, fam0), fam0)
      srcs <- circ_scored(src_all)
      not_scored(circ_unscored(src_all))
      not_scored_fam[[fam0]] <- circ_unscored(src_all)
      if (length(srcs) == 0) {
        showNotification(
          paste0("No loaded recording can supply ", circ_metric_label(input$metric %||% "axis1"),
                 ". Pick another metric."), type = "warning", duration = 8)
        return(invisible(NULL))
      }

      all_results <- list()
      n_files <- length(srcs)

      withProgress(message = "Analyzing circadian rhythm...", value = 0, {
        for (i in seq_along(srcs)) {
          b <- srcs[[i]]
          fid <- b$id

          setProgress(value = i / max(1L, length(srcs)), detail = paste("File:", b$name))

          # circ_sources() has already chosen the series, converted g to mg and
          # resampled onto one epoch, so nothing below depends on the file type
          activity    <- b$activity
          tstamps     <- b$timestamps
          epoch_s     <- b$epoch_length
          wear_time   <- if (isTRUE(input$use_wear_time)) b$wear_time else NULL
          sleep_state <- b$sleep_state
          sleep_src   <- b$sleep_source

          # The wear run's parameters, for the provenance sheet
          wt_params <- b$wear_params
          if (is.null(wt_params)) wt_params <- list()
          # Counts arrive day-gated by the Wear time tab, so min_valid_hours is 0.
          # Raw arrives epoch-masked only; GGIR's day rule (includedaycrit) goes
          # to the per-day rows through valid_days.
          circ_args <- list(
            counts = activity,
            timestamps = tstamps,
            wear_time = wear_time,
            min_valid_hours = 0,
            sleep_state = sleep_state,
            epoch_length = epoch_s,
            use_cpp = TRUE,
            # Unit stamped on the result for print() and plot()
            value_unit = if (circ_is_raw_metric(b$metric)) "mg" else "counts/min")
          if (!is.null(b$valid_days) &&
              "valid_days" %in% names(formals(canhrActi::circadian.rhythm))) {
            circ_args$valid_days <- b$valid_days
          }
          res <- tryCatch({
            do.call(canhrActi::circadian.rhythm, circ_args)
          }, error = function(e) {
            showNotification(paste0("Circadian analysis incomplete for ", b$name), type = "error")
            return(NULL)
          })

          if (is.null(res)) next

          # Run multi-component cosinor analysis (24h + 12h harmonics)
          cosinor_ext <- tryCatch({
            canhrActi::cosinor.extended(
              counts = activity,
              timestamps = tstamps,
              harmonics = c(24, 12),
              wear_time = wear_time,
              # wear_time is already day-gated above; opt out of the package default.
              min_valid_hours = 0
            )
          }, error = function(e) {
            showNotification(
              paste0("Cosinor analysis incomplete for ", b$name, ": ", e$message),
              type = "warning",
              duration = 5
            )
            NULL
          })

          # Extract 12h component data
          h12_amplitude <- NA_real_
          h12_power <- NA_real_
          if (!is.null(cosinor_ext) && !is.null(cosinor_ext$components)) {
            h12_row <- cosinor_ext$components[cosinor_ext$components$period == 12, ]
            if (nrow(h12_row) > 0) {
              h12_amplitude <- h12_row$amplitude[1]
              h12_power <- h12_row$relative_power[1]
            }
          }

          # Pre-compute workbook export metrics so the XLSX download is instant.
          act_valid <- activity
          if (!is.null(wear_time)) act_valid[!as.logical(wear_time)] <- NA
          cosinor_an   <- tryCatch(canhrActi::cosinor.analysis(activity, tstamps, wear_time = wear_time, min_valid_hours = 0), error = function(e) NULL)
          cosinor_anti <- tryCatch(canhrActi::cosinor.antilogistic(act_valid, tstamps), error = function(e) NULL)
          quotient     <- if (!is.null(cosinor_an)) tryCatch(canhrActi::circadian.quotient(cosinor_an), error = function(e) NULL) else NULL
          ellipse      <- if (!is.null(cosinor_an)) tryCatch(canhrActi::cosinor.confidence.ellipse(cosinor_an), error = function(e) NULL) else NULL
          is_multi     <- tryCatch(canhrActi::circadian.is.multiscale(act_valid, tstamps), error = function(e) NULL)
          # DFA/MSE on the valid-masked series (exclude non-wear).
          dfa          <- tryCatch(canhrActi::fractal.dfa(act_valid), error = function(e) NULL)
          mse          <- tryCatch(canhrActi::multiscale.entropy(act_valid), error = function(e) NULL)
          period_full  <- tryCatch(canhrActi::circadian.period(act_valid, tstamps), error = function(e) NULL)
          chisq_full   <- tryCatch(canhrActi::chi.sq.periodogram(act_valid, tstamps, epoch_length = epoch_s), error = function(e) NULL)
          # Social jet lag from the scored sleep periods (weekday vs weekend mid-sleep).
          sjl <- tryCatch({
            if (is.null(sleep_state)) NULL else {
              sp <- canhrActi::sleep.tudor.locke(sleep.state = sleep_state, timestamps = tstamps, epoch_length = epoch_s)
              if (!is.null(sp) && nrow(sp) > 0) canhrActi::social.jet.lag(sp) else NULL
            }
          }, error = function(e) NULL)

          all_results[[fid]] <- list(
            file_id = fid,
            name = b$name,
            subject_id = b$subject_id,
            # GGIR's per-day decision on raw; NULL on counts
            valid_days = b$valid_days,
            # What this run was computed with, for the rule bar and the workbook
            parameters = list(
              metric = input$metric %||% "axis1",
              use_wear_time = isTRUE(input$use_wear_time),
              wear_time_available = !is.null(b$wear_time),
              epoch_length = epoch_s,
              min_wear_day_min = wt_params$min_wear_day,
              min_valid_days = wt_params$min_valid_days,
              wear_algorithm = wt_params$algorithm %||% (if (identical(b$kind, "raw")) "GGIR part 2" else NULL),
              sleep_source = sleep_src,
              run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              package_version = as.character(utils::packageVersion("canhrActi"))
            ),
            L5 = res$L5,
            L5_start = res$L5_start,
            # decimal hour too; the profile chart places the window band by number
            L5_start_hour = res$L5_start_hour,
            M10 = res$M10,
            M10_start = res$M10_start,
            M10_start_hour = res$M10_start_hour,
            RA = res$RA,
            IS = res$IS,
            IV = res$IV,
            phi = res$phi,
            coverage_percent = res$coverage_percent,
            mesor = if (!is.null(cosinor_ext)) cosinor_ext$mesor else NA_real_,
            amplitude = if (!is.null(cosinor_ext)) cosinor_ext$amplitude else NA_real_,
            acrophase = if (!is.null(cosinor_ext)) cosinor_ext$acrophase else NA_real_,
            acrophase_time = if (!is.null(cosinor_ext)) cosinor_ext$acrophase_time else NA_character_,
            r_squared = if (!is.null(cosinor_ext)) cosinor_ext$r_squared else NA_real_,
            pattern_type = if (!is.null(cosinor_ext)) cosinor_ext$pattern_type else NA_character_,
            is_bimodal = if (!is.null(cosinor_ext)) cosinor_ext$is_bimodal else NA,
            h12_amplitude = h12_amplitude,
            h12_power = h12_power,
            r_squared_improvement = if (!is.null(cosinor_ext)) cosinor_ext$r_squared_improvement else NA_real_,
            r_squared_single = if (!is.null(cosinor_ext)) cosinor_ext$r_squared_single else NA_real_,
            hourly_profile = res$hourly_profile,
            # New advanced rhythm metrics from circadian.rhythm()
            tau = if (!is.null(res$tau)) res$tau else NA_real_,
            period_p_value = if (!is.null(res$period_p_value)) res$period_p_value else NA_real_,
            SRI = if (!is.null(res$SRI)) res$SRI else NA_real_,
            SRI_n_valid_pairs = if (!is.null(res$SRI_n_valid_pairs)) res$SRI_n_valid_pairs else NA_real_,
            CPD = if (!is.null(res$CPD)) res$CPD else NA_real_,
            CPD_precision = if (!is.null(res$CPD_precision)) res$CPD_precision else NA_real_,
            CPD_accuracy = if (!is.null(res$CPD_accuracy)) res$CPD_accuracy else NA_real_,
            L5_onset_mean = if (!is.null(res$L5_onset_mean)) res$L5_onset_mean else NA_real_,
            L5_onset_ci_lower = if (!is.null(res$L5_onset_ci_lower)) res$L5_onset_ci_lower else NA_real_,
            L5_onset_ci_upper = if (!is.null(res$L5_onset_ci_upper)) res$L5_onset_ci_upper else NA_real_,
            # Pre-computed values for the workbook export.
            full_result = res,
            cosinor_analysis = cosinor_an,
            cosinor_antilog = cosinor_anti,
            circadian_quotient_res = quotient,
            cosinor_ellipse = ellipse,
            is_multiscale = is_multi,
            dfa = dfa,
            mse = mse,
            periodogram = if (!is.null(period_full) && length(period_full$scanned) > 0)
              data.frame(period_h = period_full$scanned, power = period_full$power) else NULL,
            chisq = chisq_full,
            social_jet_lag = sjl
          )
        }
      })

      results(all_results)
      res_fam[[fam0]] <- all_results
      run_stamp(Sys.time())
      # Merge, not replace: all_results holds one family and other tabs read both
      prev <- shared$results$circadian %||% list()
      prev[names(all_results)] <- all_results
      shared$results$circadian <- prev
    })

    # Current view data
    current_data <- reactive({
      res <- results()
      req(length(res) > 0)

      sel <- sel_fid() %||% "all"

      if (sel == "all" || sel == "none") {
        pattern_counts <- table(sapply(res, function(r) r$pattern_type))
        dominant_pattern <- if (length(pattern_counts) > 0) names(which.max(pattern_counts)) else NA_character_

        list(
          mode = "all",
          results = res,
          L5 = mean(sapply(res, function(r) r$L5), na.rm = TRUE),
          M10 = mean(sapply(res, function(r) r$M10), na.rm = TRUE),
          RA = mean(sapply(res, function(r) r$RA), na.rm = TRUE),
          IS = mean(sapply(res, function(r) r$IS), na.rm = TRUE),
          IV = mean(sapply(res, function(r) r$IV), na.rm = TRUE),
          phi = mean(sapply(res, function(r) r$phi), na.rm = TRUE),
          coverage_percent = mean(sapply(res, function(r) r$coverage_percent), na.rm = TRUE),
          mesor = mean(sapply(res, function(r) r$mesor), na.rm = TRUE),
          amplitude = mean(sapply(res, function(r) r$amplitude), na.rm = TRUE),
          acrophase = {
            # Use circular mean for acrophase (time is circular 0-24h)
            acro_vals <- sapply(res, function(r) r$acrophase)
            acro_vals <- acro_vals[!is.na(acro_vals)]
            if (length(acro_vals) == 0) NA_real_ else {
              radians <- acro_vals * 2 * pi / 24
              mean_sin <- mean(sin(radians))
              mean_cos <- mean(cos(radians))
              ((atan2(mean_sin, mean_cos) * 24 / (2 * pi)) + 24) %% 24
            }
          },
          r_squared = mean(sapply(res, function(r) r$r_squared), na.rm = TRUE),
          pattern_type = dominant_pattern,
          h12_amplitude = mean(sapply(res, function(r) r$h12_amplitude), na.rm = TRUE),
          h12_power = mean(sapply(res, function(r) r$h12_power), na.rm = TRUE),
          r_squared_improvement = mean(sapply(res, function(r) r$r_squared_improvement), na.rm = TRUE),
          is_bimodal = any(sapply(res, function(r) isTRUE(r$is_bimodal))),
          # New advanced rhythm metrics (averaged where sensible)
          tau = mean(sapply(res, function(r) if (is.null(r$tau)) NA_real_ else r$tau), na.rm = TRUE),
          period_p_value = mean(sapply(res, function(r) if (is.null(r$period_p_value)) NA_real_ else r$period_p_value), na.rm = TRUE),
          SRI = mean(sapply(res, function(r) if (is.null(r$SRI)) NA_real_ else r$SRI), na.rm = TRUE),
          CPD = mean(sapply(res, function(r) if (is.null(r$CPD)) NA_real_ else r$CPD), na.rm = TRUE),
          CPD_precision = mean(sapply(res, function(r) if (is.null(r$CPD_precision)) NA_real_ else r$CPD_precision), na.rm = TRUE),
          L5_onset_mean = mean(sapply(res, function(r) if (is.null(r$L5_onset_mean)) NA_real_ else r$L5_onset_mean), na.rm = TRUE),
          L5_onset_ci_lower = mean(sapply(res, function(r) if (is.null(r$L5_onset_ci_lower)) NA_real_ else r$L5_onset_ci_lower), na.rm = TRUE),
          L5_onset_ci_upper = mean(sapply(res, function(r) if (is.null(r$L5_onset_ci_upper)) NA_real_ else r$L5_onset_ci_upper), na.rm = TRUE)
        )
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        list(
          mode = "single",
          result = r,
          L5 = r$L5,
          L5_start = r$L5_start,
          L5_start_hour = r$L5_start_hour,
          M10 = r$M10,
          M10_start = r$M10_start,
          M10_start_hour = r$M10_start_hour,
          RA = r$RA,
          IS = r$IS,
          IV = r$IV,
          phi = r$phi,
          coverage_percent = r$coverage_percent,
          mesor = r$mesor,
          amplitude = r$amplitude,
          acrophase = r$acrophase,
          acrophase_time = r$acrophase_time,
          r_squared = r$r_squared,
          pattern_type = r$pattern_type,
          h12_amplitude = r$h12_amplitude,
          h12_power = r$h12_power,
          r_squared_improvement = r$r_squared_improvement,
          is_bimodal = r$is_bimodal,
          # New advanced rhythm metrics
          tau = r$tau,
          period_p_value = r$period_p_value,
          SRI = r$SRI,
          SRI_n_valid_pairs = r$SRI_n_valid_pairs,
          CPD = r$CPD,
          CPD_precision = r$CPD_precision,
          CPD_accuracy = r$CPD_accuracy,
          L5_onset_mean = r$L5_onset_mean,
          L5_onset_ci_lower = r$L5_onset_ci_lower,
          L5_onset_ci_upper = r$L5_onset_ci_upper
        )
      } else {
        NULL
      }
    })

    # Main chart
    output$main_chart <- renderPlot({
      # Theme for a server-drawn chart; NULL in light mode
      gg_app(isTRUE(shared$dark), {
      cd <- current_data()

      # User-friendly empty state messaging
      validate(
        need(!is.null(cd),
             "\n\nNo Circadian Data\n\nRun the analysis to see patterns.\nSelect files and configure analysis parameters above."
        )
      )

      tab <- active_tab()

      if (tab == "profile") {
        # 24-hour activity profile
        if (cd$mode == "single") {
          hourly <- cd$result$hourly_profile
          req(hourly)

          # The chart shades the L5 and M10 windows itself, across midnight too.
          # The axis label carries the unit: mg on a raw run, counts/min otherwise.
          canhrActi::plot_circadian_profile(
            hourly,
            L5_start = cd$L5_start_hour %||% cd$L5_start,
            M10_start = cd$M10_start_hour %||% cd$M10_start,
            value_label = circ_axis_label(run_params()$metric %||% input$metric))
        } else {
          # Multi-file average
          all_hourly <- data.frame()
          for (r in cd$results) {
            if (!is.null(r$hourly_profile)) {
              h <- r$hourly_profile
              h$subject <- r$subject_id
              all_hourly <- rbind(all_hourly, h)
            }
          }
          req(nrow(all_hourly) > 0)

          avg_hourly <- aggregate(mean_counts ~ hour, all_hourly, mean, na.rm = TRUE)

          ggplot() +
            geom_line(data = all_hourly, aes(x = hour, y = mean_counts, group = subject),
                      color = "#94a3b8", alpha = 0.4, linewidth = 0.4) +
            geom_line(data = avg_hourly, aes(x = hour, y = mean_counts),
                      color = "#236192", linewidth = 1.5) +
            geom_point(data = avg_hourly, aes(x = hour, y = mean_counts),
                       color = "#236192", size = 2.5) +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0.02, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
            labs(x = NULL, y = circ_axis_label(run_params()$metric %||% input$metric),
                 subtitle = sprintf("Average of %d %s", length(cd$results),
                                  if (length(cd$results) == 1) "subject" else "subjects")) +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.title = element_text(color = "#1a202c"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        }

      } else if (tab == "actogram") {
        # Per-minute double-plotted actogram from the raw epoch series.
        sel_fid <- if (cd$mode == "single") cd$result$file_id else {
          fr <- cd$results[[1]]; if (!is.null(fr)) fr$file_id else NULL
        }
        # Whichever family the run was on. See circ_series_for().
        bser <- circ_series_for(shared, sel_fid,
                                (if (identical(cd$mode, "single")) cd$result$parameters else NULL) %||% run_params())
        validate(
          need(!is.null(bser),
               "Actogram is a per-recording view. Select a single subject above.")
        )
        acts <- bser$activity
        wt <- if (isTRUE(input$use_wear_time)) bser$wear_time else NULL
        sub <- if (cd$mode != "single") {
          paste0("per-recording view; showing ", bser$name)
        } else NULL

        tryCatch({
          p <- canhrActi::plot_actogram(
            acts, bser$timestamps,
            epoch_length = bser$epoch_length,
            wear_time = wt
          )
          if (!is.null(sub)) p <- p + labs(subtitle = sub)
          p +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid = element_blank(),
              axis.text = element_text(color = "#64748b"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        }, error = function(e) {
          validate(need(FALSE, paste0("\n\nActogram unavailable\n\nReason: ",
                                      conditionMessage(e))))
        })

      } else if (tab == "cosinor") {
        # Cosinor fit visualization
        if (cd$mode == "single") {
          hourly <- cd$result$hourly_profile

          # User-friendly validation instead of silent req() failure
          validate(
            need(!is.null(hourly), "Hourly profile data not available"),
            need(!is.na(cd$mesor), "Cosinor analysis failed - MESOR could not be calculated.\nThis may indicate irregular or insufficient activity data."),
            need(!is.na(cd$amplitude), "Cosinor analysis failed - amplitude could not be calculated.\nCheck that the data has sufficient variability."),
            need(!is.na(cd$acrophase), "Cosinor analysis failed - acrophase could not be calculated.")
          )

          # MESOR and amplitude are levels, so they carry the input unit
          canhrActi::plot_cosinor_fit(
            hourly_profile = hourly,
            mesor = cd$mesor, amplitude = cd$amplitude,
            acrophase = cd$acrophase, r_squared = cd$r_squared,
            acrophase_time = cd$acrophase_time,
            value_label = circ_axis_label(run_params()$metric %||% input$metric))
        } else {
          # Average cosinor with data points
          # User-friendly validation instead of silent req() failure
          validate(
            need(!is.na(cd$mesor), "Cosinor analysis failed - MESOR could not be calculated.\nInsufficient data across files for cosinor modeling."),
            need(!is.na(cd$amplitude), "Cosinor analysis failed - amplitude could not be calculated.\nData may lack sufficient circadian variability."),
            need(!is.na(cd$acrophase), "Cosinor analysis failed - acrophase could not be calculated.")
          )

          # Collect hourly profiles from all results
          all_hourly <- data.frame()
          for (r in cd$results) {
            if (!is.null(r$hourly_profile)) {
              h <- r$hourly_profile
              h$subject <- r$subject_id
              all_hourly <- rbind(all_hourly, h)
            }
          }

          # Calculate average hourly profile
          avg_hourly <- if (nrow(all_hourly) > 0) {
            aggregate(mean_counts ~ hour, all_hourly, mean, na.rm = TRUE)
          } else {
            NULL
          }

          hours_fine <- seq(0, 24, by = 0.1)
          acro_rad <- (cd$acrophase / 24) * 2 * pi
          fitted <- cd$mesor + cd$amplitude * cos(2 * pi * hours_fine / 24 - acro_rad)
          fit_df <- data.frame(hour = hours_fine, fitted = fitted)

          p <- ggplot() +
            geom_hline(yintercept = cd$mesor, linetype = "dashed", color = "#FFCD00", linewidth = 0.8) +
            geom_line(data = fit_df, aes(x = hour, y = fitted),
                     color = "#236192", linewidth = 1.5)

          # Add data points if available
          if (!is.null(avg_hourly) && nrow(avg_hourly) > 0) {
            p <- p + geom_point(data = avg_hourly, aes(x = hour, y = mean_counts),
                               color = "#1a202c", fill = "#236192", shape = 21, size = 3, stroke = 0.8)
          }

          p + annotate("text", x = 23, y = cd$mesor, label = "MESOR",
                    hjust = 1, vjust = -0.5, color = "#FFCD00", fontface = "bold", size = 3.5) +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0.02, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.05, 0)) +
            labs(x = NULL, y = circ_axis_label(run_params()$metric %||% input$metric),
                 subtitle = sprintf("Average cosinor fit (n=%d) | R-squared = %.3f",
                                   length(cd$results), cd$r_squared)) +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.title = element_text(color = "#1a202c"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        }

      } else if (tab == "periodogram" || tab == "extcosinor" || tab == "dfa" || tab == "chisq") {
        # Per-recording epoch-level views (Lomb-Scargle periodogram, chi-square
        # periodogram, Marler extended cosinor, and DFA). These operate on raw
        # epoch counts + timestamps for a single recording.

        # White-background override to match the styling of other branches.
        white_bg <- theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.subtitle = element_text(color = "#64748b", size = 11)
        )

        # Resolve which file's raw epoch data to use.
        # For single-file mode use the selected file; for the
        # multi/average case fall back to the first available recording
        # and annotate the subtitle that this is a per-recording metric.
        sel_fid <- NULL
        sel_name <- NULL
        is_fallback <- FALSE

        if (cd$mode == "single") {
          sel_fid <- cd$result$file_id
          sel_name <- cd$result$name
        } else {
          # Multi/average: pick the first analyzed recording
          first_r <- cd$results[[1]]
          if (!is.null(first_r)) {
            sel_fid <- first_r$file_id
            sel_name <- first_r$name
            is_fallback <- TRUE
          }
        }

        bser <- circ_series_for(shared, sel_fid,
                                (if (!is.null(cd$result)) cd$result$parameters else NULL) %||% run_params())
        validate(
          need(!is.null(bser),
               paste0("

Per-Recording View

",
                      "These views are computed per recording.
",
                      "Select a single subject above to display them."))
        )

        # The same series the run scored, not a rebuild from the counts file.
        counts <- bser$activity
        timestamps <- bser$timestamps

        validate(
          need(!is.null(counts) && length(counts) > 0 && !is.null(timestamps),
               "Raw epoch data not available for this recording.")
        )

        fallback_sub <- if (is_fallback) {
          paste0("per-recording metric; showing ", sel_name)
        } else {
          NULL
        }

        plot_fail <- function(title, what, e) {
          msg <- conditionMessage(e)
          hint <- if (grepl("could not find function|not an exported object|there is no package",
                            msg, ignore.case = TRUE)) {
            paste0("\n\nThe installed canhrActi package is out of date.\n",
                   "Reinstall the latest app build to enable this view.")
          } else ""
          validate(need(FALSE, paste0("\n\n", title, "\n\n", what,
                                      "\n\nReason: ", msg, hint)))
        }

        if (tab == "periodogram") {
          tryCatch({
            p <- canhrActi::plot_periodogram(counts, timestamps)
            if (!is.null(fallback_sub)) {
              p <- p + labs(subtitle = fallback_sub)
            }
            p + white_bg
          }, error = function(e) {
            plot_fail("Periodogram unavailable",
                      "Could not compute the Lomb-Scargle periodogram for this recording.", e)
          })

        } else if (tab == "extcosinor") {
          tryCatch({
            p <- canhrActi::plot_extended_cosinor(counts, timestamps)
            if (!is.null(fallback_sub)) {
              p <- p + labs(subtitle = fallback_sub)
            }
            p + white_bg
          }, error = function(e) {
            plot_fail("Extended cosinor unavailable",
                      "Could not compute the extended-cosinor fit for this recording.", e)
          })

        } else if (tab == "chisq") {
          tryCatch({
            p <- canhrActi::plot_chisq(counts, timestamps,
                                       epoch_length = bser$epoch_length)
            if (!is.null(fallback_sub)) {
              p <- p + labs(subtitle = fallback_sub)
            }
            p + white_bg
          }, error = function(e) {
            plot_fail("Chi-square periodogram unavailable",
                      "Could not compute the chi-square periodogram for this recording.", e)
          })

        } else {
          # tab == "dfa"
          tryCatch({
            p <- canhrActi::plot_dfa(counts)
            if (!is.null(fallback_sub)) {
              p <- p + labs(subtitle = fallback_sub)
            }
            p + white_bg
          }, error = function(e) {
            plot_fail("DFA unavailable",
                      "Could not compute detrended fluctuation analysis for this recording.", e)
          })
        }
      }
      })
    }, bg = "white")

    # Export CSV
    output$dl_csv <- downloadHandler(
      filename = function() {
        export_name("results", "csv")
      },
      content = function(file) {
        res <- results()
        req(length(res) > 0)

        df <- circadian_results_df(res)

        write.csv(df, file, row.names = FALSE)
      }
    )

    # Export reproducible workbook (see mod_circadian_workbook.R).
    output$dl_workbook <- downloadHandler(
      filename = function() {
        export_name("workbook", "xlsx")
      },
      content = function(file) {
        res <- results()
        req(length(res) > 0)
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          showNotification("Install the 'openxlsx' package to export the workbook.", type = "error")
          return(NULL)
        }
        tryCatch(
          circadian_write_workbook(
            file, res, shared,
            # The run's metric, not the dropdown's: the headers and the
            # provenance sheet describe the stored numbers
            metric = run_params()$metric %||% input$metric %||% "vm"
          ),
          error = function(e) {
            showNotification(paste("Workbook export failed:", conditionMessage(e)),
                             type = "error", duration = 8)
            openxlsx::saveWorkbook(openxlsx::createWorkbook(), file, overwrite = TRUE)
          }
        )
      }
    )

    # The links sit in a closed menu, and a suspended downloadHandler never
    # receives its href.
    for (out in c("dl_csv", "dl_workbook")) {
      outputOptions(output, out, suspendWhenHidden = FALSE)
    }
  })
}
