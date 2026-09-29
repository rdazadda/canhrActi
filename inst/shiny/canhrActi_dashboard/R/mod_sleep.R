# Module: Sleep
# Rule bar, figures, one plot panel, then one table with a row per sleep period
# (the Details export in full) and a noon to noon bar for each.

mod_sleep_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "sl-page",
      uiOutput(ns("rule"), class = "sl-out"),
      tags$div(id = ns("settings_panel"), class = "sl-settings", style = "display: none;",
               sl_settings_panel(ns)),
      tags$div(id = ns("sraw_settings"), class = "sl-settings", style = "display: none;",
               slr_settings_panel(ns)),
      uiOutput(ns("figures"), class = "sl-out"),
      uiOutput(ns("plot_panel"), class = "sl-out sl-plot-out"),
      uiOutput(ns("table_panel"), class = "sl-out sl-table-out"),

      tags$div(id = ns("processing_indicator"), class = "sl-busy", style = "display: none;",
               tags$span(class = "sl-spinner", `aria-hidden` = "true"),
               tags$span(id = ns("processing_status"), "Scoring"))
    ),
    tags$script(HTML(sl_page_script(ns(""))))
  )
}

sl_algo_label <- function(k) {
  switch(k %||% "cole.kripke", sadeh = "Sadeh (1994)", "Cole-Kripke (1992)")
}

# Settings panel: every parameter the run reads, with its unit and a note.
sl_settings_panel <- function(ns) {
  field <- function(label, control, note, id = NULL) {
    tags$div(class = "sl-field",
      tags$div(class = "sl-field-k", label),
      control,
      tags$div(class = "sl-field-n", id = id, note))
  }
  num <- function(id, value, unit, ...) {
    tags$div(class = "sl-num",
      numericInput(ns(id), NULL, value = value, width = "100%", ...),
      tags$span(class = "sl-unit", `aria-hidden` = "true", unit))
  }
  sel <- function(id, choices, selected) {
    tags$div(class = "sl-select",
      selectInput(ns(id), NULL, choices = choices, selected = selected,
                  width = "100%", selectize = FALSE))
  }
  check <- function(id, label, value = FALSE) {
    tags$div(class = "sl-check", checkboxInput(ns(id), label, value = value))
  }

  tags$div(
    class = "sl-panel sl-settings-grid",
    tags$div(
      class = "sl-fields",
      field("Sleep and wake",
            sel("algorithm", c("Cole-Kripke (1992), adults" = "cole.kripke",
                               "Sadeh (1994), youth" = "sadeh"), "cole.kripke"),
            "scores every epoch asleep or awake"),
      field("Period detection",
            sel("detection_method", c("Tudor-Locke (2014)" = "tudor.locke"), "tudor.locke"),
            "finds where a night starts and ends"),
      field("A period lasts at least", num("min_sleep_period", 160, "min", min = 30, max = 480, step = 10),
            "shorter runs are not a period"),
      field("And at most", num("max_sleep_period", 1440, "min", min = 240, max = 1440, step = 60),
            "1440 is a full day, so nothing is capped"),

      field("Bedtime needs", num("bedtime_start", 5, "min", min = 1, max = 30, step = 1),
            "inactive minutes to start a period"),
      field("Waking needs", num("wake_time_end", 10, "min", min = 1, max = 60, step = 1),
            "active minutes to end one"),
      field("Movement floor", check("use_min_nonzero", "Require some movement", FALSE),
            "off by default"),
      field("At least", num("min_nonzero_epochs", 0, "epochs", min = 0, max = 100, step = 1),
            "only while the floor is on", id = ns("note_nonzero"))
    ),
    tags$div(
      class = "sl-settings-foot",
      tags$span(class = "sl-spacer"),
      actionButton(ns("clear_results"), "Clear results", class = "sl-btn sl-btn--text is-destructive")
    )
  )
}

# Menus open and close on the page; only the choice is sent. Export starts both
# downloads inside the click, through hidden iframes: a browser allows a download
# only while the click still counts as a user action, about a second.
sl_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }
  function closeMenus() {
    document.querySelectorAll('.sl-page .sl-menu, .sl-page .sl-whomenu, .sl-page .sl-exportmenu, .sl-page .slr-exportmenu').forEach(function (m) {
      m.style.display = 'none';
    });
    document.querySelectorAll('.sl-page .sl-pick, .sl-page .sl-who, .sl-page .sl-export-btn, .sl-page .slr-export-btn').forEach(function (b) {
      b.classList.remove('is-open');
    });
  }
  function toggleMenu(button, menu) {
    var open = menu && menu.style.display !== 'none';
    closeMenus();
    if (menu && !open) { menu.style.display = 'block'; button.classList.add('is-open'); }
  }

  function fireAll(row) {
    var ids = ['export_summary', 'export_details'];
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
    var label = row.querySelector('.sl-ei-label');
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
    if (!e.target.closest || !e.target.closest('.sl-page')) { closeMenus(); return; }

    var slt = e.target.closest('.sl-page [data-sltab]');
    if (slt) { setVal('sltab_set', slt.dataset.sltab); return; }

    var slp = e.target.closest('.sl-page [data-slpass]');
    if (slp) { setVal('slpass_set', slp.dataset.slpass); return; }

    var sls = e.target.closest('.sl-page .slr-gt th[data-slsort]');
    if (sls) { setVal('slsort_set', sls.dataset.slsort); return; }

    var sex = e.target.closest('.sl-page .slr-export-btn');
    if (sex) { toggleMenu(sex, sex.parentNode.querySelector('.slr-exportmenu')); return; }
    if (e.target.closest('.sl-page .slr-exportmenu')) { return; }

    var pick = e.target.closest('.sl-page .sl-pick');
    if (pick) { toggleMenu(pick, document.querySelector('.sl-page .sl-menu')); return; }

    var who = e.target.closest('.sl-page .sl-who');
    if (who) { toggleMenu(who, document.querySelector('.sl-page .sl-whomenu')); return; }

    var wi = e.target.closest('.sl-page .sl-wi[data-who]');
    if (wi) {
      var fid = wi.dataset.who;
      document.querySelectorAll('.sl-page .sl-row[data-fid]').forEach(function (r) {
        r.classList.toggle('is-selected', fid !== 'all' && r.dataset.fid === fid);
      });
      closeMenus();
      setVal('pick', fid);
      return;
    }

    var ex = e.target.closest('.sl-page .sl-export-btn');
    if (ex) { toggleMenu(ex, document.getElementById(NS + 'export_menu')); return; }

    var all = e.target.closest('.sl-page .sl-ei-all');
    if (all) { fireAll(all); return; }

    if (e.target.closest('.sl-page .sl-exportmenu')) return;

    var row = e.target.closest('.sl-page .sl-row[data-fid]');
    if (row) {
      var was = row.classList.contains('is-selected');
      document.querySelectorAll('.sl-page .sl-row.is-selected').forEach(function (r) { r.classList.remove('is-selected'); });
      if (!was) row.classList.add('is-selected');
      setVal('pick', was ? 'all' : row.dataset.fid);
      closeMenus();
      return;
    }

    closeMenus();
  });

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') { closeMenus(); return; }
    if (!e.target.closest || !e.target.closest('.sl-page')) return;
    var row = e.target.closest('.sl-row[data-fid]');
    if (!row) return;
    var rows = Array.prototype.slice.call(document.querySelectorAll('.sl-page .sl-row[data-fid]'));
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

mod_sleep_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Module constants
    SLEEP_PLOT_CONSTANTS <- list(
      NIGHT_OFFSET_HOURS = 12,     # nights run noon to noon, as in plot_hypnogram()
      HEIGHT_PER_DAY = 80,         # Pixels per day in hypnogram
      MIN_PLOT_HEIGHT = 320,       # Minimum plot height
      MAX_PLOT_HEIGHT = 800        # Maximum plot height
    )

    results <- reactiveVal(list())

    # Drop the results of recordings removed on Overview
    observeEvent(names(shared$files), {
      res <- results()
      if (!length(res)) return()
      keep <- intersect(names(res), names(shared$files))
      if (length(keep) == length(res)) return()
      results(res[keep])
      shared$results$sleep <- res[keep]
    }, ignoreNULL = FALSE)
    selected_file <- reactiveVal(NULL)
    run_stamp <- reactiveVal(NULL)

    # BatchSleepExport<what>_<ISO date>_<time>.csv, one stamp for the whole set.
    export_name <- function(what) {
      paste0("BatchSleepExport", what, "_",
             format(run_stamp() %||% Sys.time(), "%Y-%m-%d_%H%M%S"), ".csv")
    }

    # Page scope: a subject label, or "all" for the cohort. The chooser and a
    # row click both set it.
    page_pick <- reactiveVal("all")
    observeEvent(input$pick, {
      p <- input$pick %||% "all"
      page_pick(p)
      # The hypnogram draws one recording; on the cohort it shows the first
      first <- {
        res <- results()
        if (length(res) == 0) NULL else as.character(res[[1]]$subject_id %||% res[[1]]$name)
      }
      selected_file(if (identical(p, "all")) first else p)
      # The focus carries the file id behind the label
      if (identical(p, "all")) focus_set(shared, "all")
      else {
        fid <- label_fid(p)
        if (!is.null(fid)) focus_set(shared, fid)
      }
    })

    # A subject label to its file id, and back
    label_of <- function(r) as.character(r$subject_id %||% r$name)
    label_fid <- function(lbl) {
      res <- results()
      hit <- Filter(function(fid) identical(label_of(res[[fid]]), lbl), names(res))
      if (length(hit)) hit[[1]] else NULL
    }

    observeEvent(input$toggle_advanced, {
      shinyjs::toggle("settings_panel")
    })

    observeEvent(input$use_min_nonzero, {
      on <- isTRUE(input$use_min_nonzero)
      shinyjs::toggleState("min_nonzero_epochs", condition = on)
      shinyjs::html("note_nonzero", if (on) "epochs above zero required" else "only while the floor is on")
    }, ignoreInit = FALSE)

    sel_subject <- reactive({
      p <- page_pick()
      if (is.null(p) || identical(p, "all")) NULL else p
    })

    # Subject labels, in result order
    subjects_r <- reactive({
      res <- results()
      vapply(res, function(r) as.character(r$subject_id %||% r$name), character(1), USE.NAMES = FALSE)
    })

    # Shared by the table on screen and the exports
    details_df <- reactive({
      res <- results()
      if (length(res) == 0) return(NULL)
      tryCatch(sleep_details_df(res, shared), error = function(e) {
        message("sleep_details_df failed: ", conditionMessage(e)); NULL
      })
    })
    summary_df <- reactive({
      res <- results()
      if (length(res) == 0) return(NULL)
      tryCatch(sleep_summary_df(res, shared), error = function(e) {
        message("sleep_summary_df failed: ", conditionMessage(e)); NULL
      })
    })

    # Parameters the run on screen was scored with, not the current inputs
    run_params <- reactive({
      r <- results()
      if (length(r) == 0) NULL else r[[1]]$parameters
    })
    settings_moved <- reactive({
      settings_moved_from(run_params(), input, list(
        algorithm = "sleep_algorithm", min_sleep_period = "min_sleep_period",
        bedtime_start = "bedtime_start", wake_time_end = "wake_time_end",
        max_sleep_period = "max_sleep_period", min_nonzero_epochs = "min_nonzero_epochs"))
    })

    # Raw interface. Every raw branch returns early, ahead of the counts code.
    sraw_ids <- reactive(names(shared$raw %||% list()))
    sraw_list <- reactive(shared$raw %||% list())
    has_sraw <- reactive(length(sraw_ids()) > 0)
    has_scounts <- reactive(length(shared$files) > 0)
    sview <- reactive({
      if (!has_sraw()) return("counts")
      if (!has_scounts()) return("raw")
      if (identical(local_sraw$view, "raw")) "raw" else "counts"
    })
    local_sraw <- reactiveValues(view = "raw", sel = NULL, tab = "nights",
                                 pass = "cleaned", chart = "nights",
                                 sort = NULL, dir = 1)

    # Switching branch closes both settings panels
    observeEvent(sview(), {
      shinyjs::hide("settings_panel"); shinyjs::hide("sraw_settings")
    }, ignoreInit = TRUE)

    on_tab_shown(shared, "sleep", function() {
      id <- focus_get(shared, among = names(shared$files))
      r <- if (is.null(id)) NULL else results()[[id]]
      if (!is.null(r)) {
        page_pick(label_of(r))
        selected_file(label_of(r))
      }
      rid <- focus_get(shared, among = sraw_ids())
      if (!is.null(rid)) local_sraw$sel <- rid
    })

    # Nothing is computed until Run analysis is pressed
    sraw_run <- reactiveVal(0L)
    sraw_env <- new.env(parent = emptyenv())
    sraw_env$nights <- list(); sraw_env$reports <- list()
    sraw_env$p3 <- list(); sraw_env$p3_used <- list(); sraw_env$p5 <- list()
    sraw_tick <- reactiveVal(0L)

    # The raw settings: the panel, the run's copy of it, and an attached diary
    sraw_diary <- reactiveVal(NULL)
    sraw_params <- reactiveVal(NULL)
    sraw_form <- reactive(slr_form_params(input, sraw_diary()))
    sraw_moved <- reactive(sraw_run() > 0L && slr_settings_moved(sraw_params(), sraw_form()))

    # Part 3 runs again only when the guider or the bout differs from what the
    # stored recording ran with. Part 4 gets only the members that differ, so a
    # default run is the stored one.
    sraw_part4 <- function(r, id, s) {
      p3 <- if (is.list(r$sleep)) r$sleep$settings$params else NULL
      sraw_env$p3_used[[id]] <- NULL
      if (is.list(p3)) {
        want <- vapply(s[c("HASPT.algo", "timethreshold", "anglethreshold")], function(v) as.character(v)[1], "")
        have <- vapply(list(p3$HASPT.algo, p3$timethreshold, p3$anglethreshold),
                       function(v) paste(as.character(v), collapse = ","), "")
        if (!identical(unname(want), have)) {
          sig <- paste(c(ovr_name(r, id), want), collapse = "|")
          hit <- sraw_env$p3[[id]]
          if (is.null(hit) || !identical(hit$sig, sig)) {
            hit <- list(sig = sig, sleep = canhrActi::raw.sleep.part3(r,
              HASPT.algo = s$HASPT.algo, timethreshold = s$timethreshold,
              anglethreshold = s$anglethreshold))
            sraw_env$p3[[id]] <- hit
          }
          r$sleep <- hit$sleep
          # the regularity chart reads the part 3 this run used
          sraw_env$p3_used[[id]] <- hit$sleep
          p3 <- r$sleep$settings$params
        }
      }
      ov <- list()
      if (!identical(as.character(s$includenightcrit), as.character(p3$includenightcrit %||% 16)))
        ov$includenightcrit <- s$includenightcrit
      if (!identical(s$sleepwindowType, as.character(p3$sleepwindowType %||% "SPT")[1]))
        ov$sleepwindowType <- s$sleepwindowType
      if (!is.null(s$diary)) ov$loglocation <- s$diary$path
      # a call on the symbol, so a warning never deparses the whole recording
      n <- eval(as.call(c(list(quote(canhrActi::raw.sleep.nights), quote(r)), ov)))
      # the rule bar names the diary by the file the user picked
      if (!is.null(s$diary)) {
        a <- attr(n, "canhrActi"); a$sleeplog_name <- s$diary$name; attr(n, "canhrActi") <- a
      }
      n
    }

    sraw_switch <- function() {
      if (!(has_scounts() && has_sraw())) return(NULL)
      v <- sview()
      seg <- function(key, label, n) {
        tags$button(id = ns(paste0("sl_view_", key)), type = "button",
                    class = paste("action-button ovr-seg-b", if (identical(v, key)) "is-on" else ""),
                    label, tags$b(fmt_int(n)))
      }
      tags$div(class = "ovr-seg", role = "tablist",
               seg("counts", "Counts", length(shared$files)),
               seg("raw", "Raw", length(sraw_ids())))
    }
    observeEvent(input$sl_view_counts, local_sraw$view <- "counts")
    observeEvent(input$sl_view_raw, local_sraw$view <- "raw")

    observeEvent(sraw_ids(), {
      sraw_env$p3 <- sraw_env$p3[intersect(names(sraw_env$p3), sraw_ids())]
      sraw_env$p5 <- sraw_env$p5[intersect(names(sraw_env$p5), sraw_ids())]
      if (!is.null(local_sraw$sel) && !local_sraw$sel %in% sraw_ids()) local_sraw$sel <- NULL
      if (is.null(local_sraw$sel) && length(sraw_ids()) > 0) local_sraw$sel <- sraw_ids()[1]
    })
    sraw_cur <- reactive({
      ids <- sraw_ids(); i <- if (is.null(local_sraw$sel)) 1L else match(local_sraw$sel, ids)
      if (is.na(i)) i <- 1L
      i
    })

    # GGIR part 4, one call per recording, cached. raw.sleep.nights() gives the
    # night table and raw.sleep.report() GGIR's four frames.
    sraw_nights <- reactive({
      ids <- sraw_ids(); rs <- sraw_list()
      sraw_tick()
      if (sraw_run() == 0L || length(ids) == 0) return(list())
      cache <- sraw_env$nights %||% list()
      todo <- setdiff(ids, names(cache))
      if (length(todo) > 0) {
        s <- isolate(sraw_params()) %||% SLR_DEFAULTS
        p3_moved <- !identical(s[c("HASPT.algo", "timethreshold", "anglethreshold")],
                               SLR_DEFAULTS[c("HASPT.algo", "timethreshold", "anglethreshold")])
        withProgress(message = if (p3_moved) "Running GGIR parts 3 and 4" else "Running GGIR part 4",
                     value = 0, {
          for (k in seq_along(todo)) {
            id <- todo[k]
            incProgress(1 / length(todo), detail = ovr_name(rs[[id]], id))
            cache[[id]] <- tryCatch(sraw_part4(rs[[id]], id, s),
                                    error = function(e) structure(list(), error = conditionMessage(e)))
          }
        })
        sraw_env$nights <- cache
        isolate(sraw_tick(sraw_tick() + 1L))
      }
      cache[ids]
    })

    # Part 5 for the columns the report merges; kept while its settings hold,
    # and taken from the Activity page when that ran on the same settings
    sraw_part5 <- function(r, id, n, s) {
      pp <- slr_part5_params(s)
      hit <- sraw_env$p5[[id]]
      if (!is.null(hit) && identical(hit$pp, pp) && identical(hit$p3, sraw_env$p3_used[[id]])) return(hit$day)
      tu <- isolate(shared$raw_timeuse %||% list())[[id]]
      if (!slr_part5_same(tu, pp)) {
        if (!is.null(sraw_env$p3_used[[id]])) r$sleep <- sraw_env$p3_used[[id]]
        tu <- canhrActi::raw.timeuse(r, nights = n, params = pp)
      }
      day <- slr_part5_day(tu)
      sraw_env$p5[[id]] <- list(pp = pp, p3 = sraw_env$p3_used[[id]], day = day)
      day
    }

    sraw_reports <- reactive({
      ids <- sraw_ids(); ns_ <- sraw_nights(); rs <- sraw_list()
      if (sraw_run() == 0L || length(ids) == 0) return(list())
      sraw_tick()
      cache <- sraw_env$reports %||% list()
      todo <- setdiff(ids, names(cache))
      if (length(todo) > 0) {
        s <- isolate(sraw_params()) %||% SLR_DEFAULTS
        withProgress(message = "Running GGIR part 5", value = 0, {
          for (k in seq_along(todo)) {
            id <- todo[k]
            n <- ns_[[id]]
            incProgress(1 / length(todo), detail = ovr_name(rs[[id]], id))
            cache[[id]] <- tryCatch({
              if (!is.data.frame(n) || nrow(n) == 0) NULL
              else {
                # to GGIR a failed part 5 is a missing ms5 file
                p5 <- tryCatch(sraw_part5(rs[[id]], id, n, s), error = function(e) NULL)
                canhrActi::raw.sleep.report(n, part5 = p5)
              }
            }, error = function(e) NULL)
          }
        })
        sraw_env$reports <- cache
      }
      cache[ids]
    })

    # Run analysis, its empty-state copy and Re-run do the same thing
    sraw_go <- function() {
      sraw_params(sraw_form())
      sraw_env$nights <- list(); sraw_env$reports <- list(); sraw_env$p3_used <- list()
      sraw_run(isolate(sraw_run()) + 1L)
      sraw_tick(isolate(sraw_tick()) + 1L)
      local_sraw$sort <- NULL; local_sraw$dir <- 1
    }
    observeEvent(input$sraw_run, sraw_go())
    observeEvent(input$sraw_run_empty, sraw_go())
    observeEvent(input$sraw_reanalyse, sraw_go())
    observeEvent(input$sraw_change, shinyjs::toggle("sraw_settings"))
    observeEvent(input$sraw_change_empty, shinyjs::toggle("sraw_settings"))

    # A diary is kept once GGIR's reader accepts it
    observeEvent(input$sraw_diary_file, {
      f <- input$sraw_diary_file
      if (is.null(f) || nrow(f) == 0) return()
      chk <- tryCatch(suppressWarnings(canhrActi::raw.sleeplog(f$datapath[1])),
                      error = function(e) e)
      if (inherits(chk, "error")) {
        shinyjs::html("sraw_diary_note", htmltools::htmlEscape(paste("Not read:", conditionMessage(chk))))
        return()
      }
      sraw_diary(list(name = f$name[1], path = f$datapath[1]))
    })
    observeEvent(input$sraw_diary_clear, sraw_diary(NULL))
    # Time in bed is a diary's window, so it waits for one
    observe({
      d <- sraw_diary()
      shinyjs::toggle("sraw_diary_clear", condition = !is.null(d))
      shinyjs::html("sraw_diary_note", if (is.null(d)) "GGIR's sleeplog csv" else htmltools::htmlEscape(d$name))
      shinyjs::toggleState("sraw_window", condition = !is.null(d))
      shinyjs::html("sraw_window_note", if (is.null(d)) "time in bed needs a diary" else "what the diary records")
      if (is.null(d)) updateSelectInput(session, "sraw_window", selected = "SPT")
    })
    observeEvent(input$sraw_defaults, {
      updateSelectInput(session, "sraw_haspt", selected = SLR_DEFAULTS$HASPT.algo)
      updateNumericInput(session, "sraw_time", value = SLR_DEFAULTS$timethreshold)
      updateNumericInput(session, "sraw_angle", value = SLR_DEFAULTS$anglethreshold)
      updateNumericInput(session, "sraw_incl", value = SLR_DEFAULTS$includenightcrit)
      updateSelectInput(session, "sraw_window", selected = SLR_DEFAULTS$sleepwindowType)
    })

    observeEvent(input$sltab_set, local_sraw$tab <- input$sltab_set, ignoreInit = TRUE)
    observeEvent(input$slpass_set, local_sraw$pass <- input$slpass_set, ignoreInit = TRUE)
    observeEvent(input$slchart_set, local_sraw$chart <- input$slchart_set, ignoreInit = TRUE)
    observeEvent(input$slwho_set, {
      v <- input$slwho_set
      v <- if (identical(v, "all") || !v %in% sraw_ids()) sraw_ids()[1] else v
      # the chooser reports its drawn value too; only a real pick moves the focus
      if (!identical(v, local_sraw$sel)) {
        local_sraw$sel <- v
        focus_set(shared, v)
      }
    }, ignoreInit = TRUE)
    observeEvent(input$slsort_set, {
      k <- suppressWarnings(as.integer(input$slsort_set))
      if (!is.na(k)) {
        if (identical(local_sraw$sort, k)) local_sraw$dir <- -local_sraw$dir
        else { local_sraw$sort <- k; local_sraw$dir <- 1 }
      }
    }, ignoreInit = TRUE)

    # One report pass over all recordings, written as GGIR writes it
    sraw_files <- function(dir) {
      sraw_reports()
      ids <- sraw_ids()
      slr_write_report(sraw_nights()[ids], lapply(ids, function(id) sraw_env$p5[[id]]$day), dir)
    }
    sraw_dl <- function(which, fname) {
      downloadHandler(
        filename = function() fname,
        content = function(file) {
          d <- tempfile("part4_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
          p <- sraw_files(d)[[which]]
          validate(need(file.exists(p), "Part 4 wrote no rows."))
          file.copy(p, file, overwrite = TRUE)
        })
    }
    output$sraw_dl_nc <- sraw_dl("nightsummary_cleaned", "part4_nightsummary_sleep_cleaned.csv")
    output$sraw_dl_nf <- sraw_dl("nightsummary_full",    "part4_nightsummary_sleep_full.csv")
    output$sraw_dl_pc <- sraw_dl("personsummary_cleaned", "part4_summary_sleep_cleaned.csv")
    output$sraw_dl_pf <- sraw_dl("personsummary_full",    "part4_summary_sleep_full.csv")
    output$sraw_dl_all <- downloadHandler(
      filename = function() "part4_sleep.zip",
      content = function(file) {
        d <- tempfile("part4_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
        p <- sraw_files(d)
        p <- p[file.exists(p)]
        validate(need(length(p) > 0, "Part 4 wrote no rows."))
        # zipr, as the Overview's zips: flat, and no zip.exe to find
        zip::zipr(zipfile = file, files = p)
      })
    # the links sit in a closed menu, and a suspended download never gets its href
    for (out in c("sraw_dl_nc", "sraw_dl_nf", "sraw_dl_pc", "sraw_dl_pf", "sraw_dl_all")) {
      outputOptions(output, out, suspendWhenHidden = FALSE)
    }

    # GGIR's own figure: one row per night plus the legend
    output$sraw_chart <- renderPlot({
      ns_ <- sraw_nights(); i <- sraw_cur()
      n <- ns_[[i]]
      req(is.data.frame(n), nrow(n) > 0)
      if (identical(local_sraw$chart, "regularity")) {
        r <- sraw_list()[[sraw_ids()[i]]]
        s3 <- sraw_env$p3_used[[sraw_ids()[i]]]
        if (!is.null(s3)) r$sleep <- s3
        print(canhrActi::plot_raw_sleep_regularity(r))
      } else {
        canhrActi::raw.ggir.sleepplot(n)
      }
    }, res = 108, height = function() {
      if (identical(local_sraw$chart, "regularity")) return(320)
      ns_ <- sraw_nights(); i <- sraw_cur()
      nk <- tryCatch({
        a <- attr(ns_[[i]], "canhrActi")
        # the nights the figure keeps: cleaningcode under 1 with a diary, under 2 without
        sum(suppressWarnings(as.numeric(a$guiders$cleaningcode)) < (if (isTRUE(a$dolog)) 1 else 2),
            na.rm = TRUE)
      }, error = function(e) 4)
      # 40px per night, 96px for GGIR's top and bottom margins at this
      # resolution, 46px for the legend above the top row. 6000px is a
      # graphics device guard, reached past 146 nights.
      max(200, min(6000, round(96 + 40 * max(nk, 1) + 46)))
    })

    output$rule <- renderUI({
      if (identical(sview(), "raw")) {
        if (length(sraw_ids()) == 0) return(sraw_switch())
        ns_ <- sraw_nights(); reps <- sraw_reports()
        n1 <- if (length(ns_) == 0) NULL else ns_[[sraw_cur()]]
        r1 <- if (length(reps) == 0) NULL else reps[[sraw_cur()]]
        ran <- sraw_run() > 0L
        # Drawn before the run too, so the guider and diary can be set first
        return(tagList(sraw_switch(),
                       slr_rule(n1, r1, ns, ran = ran,
                                form = if (ran) sraw_params() else sraw_form(),
                                stale = sraw_moved())))
      }
      p <- run_params()
      alg <- sl_algo_label(p$sleep_algorithm %||% input$algorithm)
      epochs <- unique(vapply(shared$files, function(f) as.numeric(f$epoch_length %||% NA), numeric(1)))
      epochs <- epochs[!is.na(epochs)]
      epoch_txt <- if (length(epochs) == 1) paste0(fmt_int(epochs), " s epochs")
                   else if (length(epochs) > 1) "mixed epochs" else NULL
      ddf <- details_df()
      n_periods <- if (is.null(ddf)) 0 else nrow(ddf)
      n_files <- length(unique(if (is.null(ddf)) character(0) else ddf[["Subject Name"]]))

      # Both branches draw the switch; without it here Counts has no way back
      tagList(sraw_switch(), tags$div(
        class = "sl-panel sl-rule",
        tags$span(class = "sl-rule-k", "Scoring"),
        tags$span(class = "sl-rule-v",
          tags$b(alg), " · Tudor-Locke periods · ",
          fmt_int(p$min_sleep_period %||% input$min_sleep_period %||% 160), " min minimum",
          if (!is.null(epoch_txt)) paste0(" · ", epoch_txt) else NULL),
        tags$span(class = "sl-rule-sep", `aria-hidden` = "true"),
        tags$span(class = "sl-rule-k", "Periods"),
        tags$span(class = "sl-rule-v",
          if (n_periods == 0) "not scored yet"
          else tagList(tags$b(fmt_int(n_periods)), " across ", fmt_int(n_files),
                       if (n_files == 1) " recording" else " recordings")),
        if (settings_moved()) stale_note("sl") else NULL,
        tags$span(class = "sl-rule-actions",
          actionButton(ns("toggle_advanced"), "Change", class = "sl-btn sl-btn--secondary"),
          tags$span(class = "sl-export-wrap",
            tags$span(class = paste("sl-btn sl-btn--secondary sl-export-btn",
                                    if (n_periods == 0) "is-quiet" else ""),
                      "Export", tags$span(class = "sl-car", `aria-hidden` = "true", HTML("&#9660;"))),
            sl_export_menu(ns, n_periods > 0)),
          actionButton(ns("run_btn"), if (length(results()) == 0) "Run analysis" else "Re-run",
                       class = paste(run_button_class("sl", length(results()) > 0, settings_moved()),
                                     "sl-run")))
      ))
    })

    # Figures: the mean of each recording's own mean, over the chosen scope
    output$figures <- renderUI({
      if (identical(sview(), "raw")) {
        reps <- sraw_reports()
        if (length(reps) == 0) return(NULL)
        return(slr_figures(reps, length(sraw_ids())))
      }
      sdf <- summary_df()
      if (is.null(sdf) || nrow(sdf) == 0) return(NULL)
      subj <- sel_subject()
      rows <- if (is.null(subj)) sdf else sdf[sdf[["Subject Name"]] == subj, , drop = FALSE]
      if (nrow(rows) == 0) rows <- sdf

      meanOf <- function(k) {
        v <- suppressWarnings(as.numeric(rows[[k]]))
        v <- v[is.finite(v)]
        if (length(v) == 0) NA_real_ else mean(v)
      }
      periods <- sum(suppressWarnings(as.numeric(rows[["Number of Sleep Periods"]])), na.rm = TRUE)
      dur <- meanOf("Average Total Sleep Time")
      eff <- meanOf("Average Efficiency")
      waso <- meanOf("Average WASO")
      dash <- "–"

      tags$div(
        class = "sl-panel sl-figs",
        sl_fig(if (is.null(subj)) fmt_int(nrow(rows)) else subj, NULL,
               if (is.null(subj)) "Recordings" else "Recording"),
        sl_rule_div(),
        sl_fig(fmt_int(periods), NULL, "Sleep periods"),
        sl_rule_div(),
        sl_fig(if (is.na(dur)) dash else fmt_dec(dur / 60, 1), "h", "Average duration"),
        sl_rule_div(),
        sl_fig(if (is.na(eff)) dash else fmt_dec(eff, 1), "%", "Average efficiency"),
        sl_rule_div(),
        sl_fig(if (is.na(waso)) dash else fmt_int(round(waso)), "min", "Average WASO")
      )
    })

    output$plot_panel <- renderUI({
      if (identical(sview(), "raw")) {
        ids <- sraw_ids(); rs <- sraw_list()
        if (length(ids) == 0) return(NULL)
        if (sraw_run() == 0L) return(slr_not_run(length(ids), ns))
        n <- sraw_nights()[[sraw_cur()]]
        if (!is.data.frame(n) || nrow(n) == 0) {
          err <- attr(n, "error")
          return(tags$div(class = "wt-panel acr-empty",
            tags$div(class = "acr-empty-t", "No nights for this recording"),
            tags$div(class = "acr-empty-s",
              if (is.null(err)) "Part 4 found no night it could place a sleep window on."
              else "Part 4 stopped with an error."),
            if (!is.null(err)) tags$div(class = "acr-empty-m", err)))
        }
        return(slr_chart_panel(rs, ids, local_sraw$sel, local_sraw$chart, ns))
      }
      res <- results()
      if (length(res) == 0) {
        # The empty state; Run analysis is in the rule bar, as on the other counts pages
        return(tags$div(class = "sl-panel sl-plotpanel",
          tags$div(class = "sl-empty",
            tags$div(class = "sl-empty-t", "No sleep results"),
            tags$div(class = "sl-empty-m", "Run the analysis to detect sleep periods."))))
      }
      sel <- sel_subject()
      subjects <- subjects_r()
      first <- if (length(subjects)) subjects[1] else NULL
      subj <- if (is.null(sel)) first else sel
      ddf <- details_df()
      nights_of <- function(s) if (is.null(ddf)) 0 else sum(ddf[["Subject Name"]] == s)

      tags$div(class = "sl-panel sl-plotpanel",
        tags$div(class = "sl-plotbar",
          tags$span(class = "sl-pick", tabindex = "0",
                    "Hypnogram", tags$span(class = "sl-car", `aria-hidden` = "true", HTML("&#9660;"))),
          tags$div(class = "sl-menu", style = "display: none;",
            tags$div(class = "sl-mi is-on",
                     tags$span(class = "sl-tick", `aria-hidden` = "true", HTML("&#10003;")),
                     "Hypnogram",
                     tags$span(class = "sl-sc", if (is.null(subj)) "one recording" else subj))),

          tags$span(class = "sl-who-wrap",
            tags$span(class = "sl-who", tabindex = "0",
                      if (is.null(sel)) paste("All", fmt_int(length(subjects)), "recordings") else subj,
                      tags$span(class = "sl-car", `aria-hidden` = "true", HTML("&#9660;"))),
            tags$div(class = "sl-whomenu", style = "display: none;",
              tags$div(class = paste("sl-wi", if (is.null(sel)) "is-on" else ""), `data-who` = "all",
                       tags$span(class = "sl-tick", `aria-hidden` = "true",
                                 if (is.null(sel)) HTML("&#10003;") else ""),
                       "All recordings",
                       tags$span(class = "sl-sc", "falls back to the first")),
              tags$div(class = "sl-wi-rule", `aria-hidden` = "true"),
              lapply(subjects, function(s) {
                tags$div(class = paste("sl-wi", if (identical(s, subj) && !is.null(sel)) "is-on" else ""),
                         `data-who` = s,
                         tags$span(class = "sl-tick", `aria-hidden` = "true",
                                   if (identical(s, subj) && !is.null(sel)) HTML("&#10003;") else ""),
                         s,
                         tags$span(class = "sl-sc", paste(fmt_int(nights_of(s)), "nights")))
              }))),

          tags$span(class = "sl-scope",
            if (is.null(sel))
              paste0("showing ", subj %||% "the first recording",
                     " · this plot draws one recording at a time")
            else paste(fmt_int(nights_of(subj)), "nights · the selected recording"))),
        tags$div(class = "sl-plotwrap", uiOutput(ns("hypnogram_chart_ui"))))
    })

    output$table_panel <- renderUI({
      if (identical(sview(), "raw")) {
        if (length(sraw_ids()) == 0 || sraw_run() == 0L) return(NULL)
        reps <- sraw_reports()
        rep <- if (length(reps) == 0) NULL else reps[[sraw_cur()]]
        return(slr_table_panel(rep, local_sraw$tab, local_sraw$pass,
                               local_sraw$sort, local_sraw$dir, ns))
      }
      ddf <- details_df()
      if (is.null(ddf) || nrow(ddf) == 0) return(NULL)
      subj <- sel_subject()
      n <- if (is.null(subj)) nrow(ddf) else sum(ddf[["Subject Name"]] == subj)

      tags$div(class = "sl-panel sl-tablepanel",
        tags$div(class = "sl-table-head",
          tags$span(class = "sl-table-title", "Sleep periods",
            tags$span(class = "sl-table-sub",
                      paste0(fmt_int(n), if (n == 1) " period · " else " periods · ",
                             fmt_int(ncol(ddf)), " columns"))),
          tags$span(class = "sl-key",
            tags$span("Efficiency"),
            tags$span(class = "sl-ramp",
              tags$b(class = "s1"), tags$b(class = "s2"), tags$b(class = "s3"), tags$b(class = "s4")),
            tags$span("80 to 100%"))),
        sl_periods_grid(ddf, subj))
    })

    # Calculate number of unique days for dynamic height
    n_days_reactive <- reactive({
      res <- results()
      sel <- selected_file()

      if (length(res) == 0) return(4)  # default

      # Handle empty or NULL selection
      if (is.null(sel) || length(sel) == 0 || sel == "") {
        r <- res[[1]]
      } else {
        # Find selected result
        r <- NULL
        for (result in res) {
          result_id <- result$subject_id %||% result$name
          if (!is.null(result_id) && length(result_id) > 0 && result_id == sel) {
            r <- result
            break
          }
        }
        if (is.null(r)) r <- res[[1]]
      }

      if (is.null(r$timestamps)) return(4)

      # Count the nights, noon to noon
      ts <- tryCatch(as.POSIXct(r$timestamps), error = function(e) NULL)
      if (is.null(ts)) return(4)

      # dated on the timestamps' own clock, as plot_hypnogram() dates its nights
      night_dates <- as.Date(as.POSIXlt(ts - SLEEP_PLOT_CONSTANTS$NIGHT_OFFSET_HOURS * 3600))
      n_days <- length(unique(night_dates))

      max(n_days, 1)
    })

    # Dynamic UI for hypnogram with calculated height
    output$hypnogram_chart_ui <- renderUI({
      n_days <- n_days_reactive()

      # Calculate height: minimum 80px per day, minimum total 320px, maximum 800px
      height_per_day <- SLEEP_PLOT_CONSTANTS$HEIGHT_PER_DAY
      total_height <- max(SLEEP_PLOT_CONSTANTS$MIN_PLOT_HEIGHT, min(SLEEP_PLOT_CONSTANTS$MAX_PLOT_HEIGHT, n_days * height_per_day))

      plotOutput(ns("hypnogram_chart"), height = paste0(total_height, "px"))
    })

    # Clear results handler
    observeEvent(input$clear_results, {
      results(list())
      selected_file(NULL)
    })

    # Hypnogram/Hypnodensity Chart
    output$hypnogram_chart <- renderPlot({
      res <- results()
      sel <- selected_file()

      if (length(res) == 0) {
        # Empty state
        plot(1, type = "n", xlim = c(0, 24), ylim = c(0, 1),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        text(12, 0.5, "Run Sleep Analysis to view sleep patterns",
             col = "#94a3b8", cex = 1.5, font = 2)
        return()
      }

      # Find selected result
      r <- NULL
      # Guard against NULL or empty sel
      if (!is.null(sel) && length(sel) > 0 && nchar(sel) > 0) {
        for (result in res) {
          result_id <- result$subject_id %||% result$name %||% ""
          if (length(result_id) > 0 && result_id == sel) {
            r <- result
            break
          }
        }
      }

      if (is.null(r)) r <- res[[1]]

      # Check if we have sleep state data
      if (is.null(r$sleep_state) || is.null(r$timestamps)) {
        plot(1, type = "n", xlim = c(0, 24), ylim = c(0, 1),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        text(12, 0.5, "No sleep data available for visualization",
             col = "#94a3b8", cex = 1.2)
        return()
      }

      # Get file data for counts
      f <- shared$files[[r$file_id]]
      if (is.null(r$sleep_state) || is.null(r$timestamps)) {
        create_simple_hypnogram(r)
        return()
      }

      # Build the plot frame from the stored sleep series (which may have been
      # reintegrated to 60s), NOT from f$data - their lengths can differ.
      data_for_plot <- data.frame(
        timestamp = r$timestamps,
        sleep_state = r$sleep_state,
        stringsAsFactors = FALSE
      )
      counts_col <- NULL
      if (!is.null(r$counts) && length(r$counts) == nrow(data_for_plot)) {
        data_for_plot$axis1 <- r$counts
        counts_col <- "axis1"
      }

      # Render hypnogram. gg_app() wraps only the last expression because the
      # body above uses return().
      gg_app(isTRUE(shared$dark), tryCatch({
        canhrActi::plot_hypnogram(
          data = data_for_plot,
          timestamp_col = "timestamp",
          sleep_col = "sleep_state",
          counts_col = counts_col,
          sleep_periods = r$periods,
          show_metrics = TRUE,
          show_activity = TRUE,
          show_awakenings = TRUE,
          title = paste("Sleep Hypnogram -", r$subject_id %||% r$name)
        )
      }, error = function(e) {
        create_simple_hypnogram(r)
      }))
    }, bg = "white")

    # Simple hypnogram fallback function
    create_simple_hypnogram <- function(r) {
      if (is.null(r$sleep_state) || is.null(r$timestamps)) {
        plot(1, type = "n", xlim = c(0, 24), ylim = c(0, 1),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        text(12, 0.5, "No sleep data available", col = "#94a3b8", cex = 1.2)
        return()
      }

      ts <- as.POSIXct(r$timestamps)
      sleep <- r$sleep_state

      # Convert to numeric (0 = sleep, 1 = wake)
      if (is.character(sleep)) {
        sleep_num <- ifelse(toupper(sleep) == "W", 1, 0)
      } else {
        sleep_num <- as.numeric(sleep != 0)
      }

      # Time of day
      hours <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60

      # Theme colours for base graphics
      ink <- base_ink(isTRUE(shared$dark))
      par(mar = c(4, 4, 2, 2), bg = "white", fg = ink$muted,
          col.axis = ink$muted, col.lab = ink$ink, col.main = ink$ink)
      plot(hours, sleep_num, type = "n",
           xlim = c(0, 24), ylim = c(-0.1, 1.3),
           xlab = "Time of Day", ylab = "",
           xaxt = "n", yaxt = "n",
           main = paste("Sleep Hypnogram -", r$subject_id %||% r$name))

      # X-axis
      axis(1, at = seq(0, 24, by = 4),
           labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"))

      # Y-axis
      axis(2, at = c(0, 1), labels = c("Sleep", "Wake"), las = 1)

      # Fill rectangles for sleep/wake states
      for (i in 1:(length(hours) - 1)) {
        if (is.na(sleep_num[i])) next
        col <- if (sleep_num[i] == 0) "#1a365d" else "#f56565"
        rect(hours[i], -0.05, hours[i + 1], sleep_num[i] + 0.05, col = col, border = NA)
      }

      # Add grid
      abline(v = seq(0, 24, by = 4), col = ink$grid, lty = 2)
      abline(h = c(0, 1), col = ink$grid, lty = 2)
    }

    # Run sleep analysis, from the rule bar or the empty state
    run_counts <- function() {
      req(shared$data_loaded, shared$file_count > 0)

      #  Check if wear time has been analyzed
      wt_results <- shared$results$wear_time
      use_wear_time <- !is.null(wt_results) && length(wt_results) > 0

      # Warn if wear time not analyzed - this is IMPORTANT for sleep
      if (!use_wear_time) {
        showNotification(
          HTML("<strong>Important:</strong> Run Wear Time Analysis first for accurate sleep detection.<br>
                Non-wear periods (0 counts) may be incorrectly classified as sleep."),
          type = "warning",
          duration = 10
        )
      }

      warned_epoch <- FALSE
      warned_msgs <- character()

      notify_warning <- function(msg) {
        if (is.null(msg) || !nzchar(msg)) return()
        if (!msg %in% warned_msgs) {
          showNotification(msg, type = "warning", duration = 8)
          warned_msgs <<- c(warned_msgs, msg)
        }
      }

      capture_warnings <- function(expr, prefix = NULL) {
        withCallingHandlers(expr, warning = function(w) {
          msg <- conditionMessage(w)
          if (!grepl("validated for 60-second epochs", msg, ignore.case = TRUE)) {
            if (!is.null(prefix) && nzchar(prefix)) {
              msg <- paste(prefix, msg)
            }
            notify_warning(msg)
          }
          invokeRestart("muffleWarning")
        })
      }
      file_ids <- names(shared$files)
      n_files <- length(file_ids)
      all_results <- vector("list", n_files)
      names(all_results) <- file_ids
      start_time <- Sys.time()

      progress_interval <- max(1, min(5, ceiling(n_files / 10)))

      withProgress(message = "Scoring sleep periods...", value = 0, {
        for (i in seq_along(file_ids)) {
          fid <- file_ids[i]
          f <- shared$files[[fid]]
          data <- f$data

          if (i == 1 || i == n_files || i %% progress_interval == 0) {
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            if (i > 1) {
              avg_time <- elapsed / (i - 1)
              eta <- format_eta(avg_time * (n_files - i + 1))
              detail_msg <- paste0("Processing ", i, "/", n_files, " | ETA: ", eta)
            } else {
              detail_msg <- paste0("Processing ", i, "/", n_files)
            }
            setProgress(value = i / n_files, detail = detail_msg)
          }

          counts <- if ("axis1" %in% names(data)) data$axis1 else data[, 1]

          if (!warned_epoch && !is.null(f$epoch_length) && !is.na(f$epoch_length) && f$epoch_length != 60) {
            showNotification(paste0("Sub-minute epochs (", f$epoch_length, "s) are reintegrated to 60s for sleep scoring (Cole-Kripke/Sadeh are 1-minute methods)."), type = "message", duration = 6)
            warned_epoch <- TRUE
          }

          timestamps <- if ("timestamp" %in% names(data)) {
            data$timestamp
          } else if ("dataTimestamp" %in% names(data)) {
            as.POSIXct((data$dataTimestamp / 10000000 - 62135596800), origin = '1970-01-01', tz = 'UTC')
          } else {
            seq(from = Sys.time() - nrow(data) * 60, by = 60, length.out = nrow(data))
          }

          # Get wear time mask for this file
          wear_mask <- NULL
          if (use_wear_time && fid %in% names(wt_results)) {
            wear_mask <- wt_results[[fid]]$wear
          }

          # Cole-Kripke/Sadeh are 1-minute methods: reintegrate sub-minute epochs
          # to 60s so every window/threshold/metric is correct. 60s files unchanged.
          scoring_epoch <- f$epoch_length
          if (!is.null(f$epoch_length) && !is.na(f$epoch_length) && f$epoch_length != 60) {
            ri <- canhrActi::reintegrate.epochs(counts, timestamps, wear_mask,
                                                from_epoch = f$epoch_length, to_epoch = 60)
            counts <- ri$counts
            timestamps <- ri$timestamps
            wear_mask <- ri$wear
            scoring_epoch <- ri$epoch_length
          }

          sleep_state <- NULL
          periods <- NULL
          actual_algorithm <- input$algorithm
          detection_method_used <- "Tudor-Locke"

          if (actual_algorithm %in% c("cole.kripke", "sadeh")) {
            sleep_state <- tryCatch({
              if (actual_algorithm == "cole.kripke") {
                capture_warnings({
                  canhrActi::sleep.cole.kripke(counts, apply_rescoring = TRUE, epoch_length = scoring_epoch)
                }, prefix = paste0(f$name, ":"))
              } else {
                capture_warnings({
                  canhrActi::sleep.sadeh(counts, epoch_length = scoring_epoch)
                }, prefix = paste0(f$name, ":"))
              }
            }, error = function(e) {
              showNotification(paste0("Sleep scoring failed for ", f$name, ": ", e$message), type = "error")
              return(NULL)
            })

            if (!is.null(sleep_state) && "timestamp" %in% names(data)) {
              bedtime_epochs <- input$bedtime_start
              wake_epochs <- input$wake_time_end

              # Run Tudor-Locke period detection on FULL sleep_state (no NA masking yet)
              periods <- tryCatch({
                capture_warnings({
                  canhrActi::sleep.tudor.locke(
                    sleep.state = sleep_state,
                    timestamps = timestamps,
                    counts = counts,
                    bedtime_start = bedtime_epochs,
                    wake_time_end = wake_epochs,
                    min_sleep_period = input$min_sleep_period,
                    max_sleep_period = input$max_sleep_period,
                    min_nonzero_epochs = if (input$use_min_nonzero) input$min_nonzero_epochs else 0,
                    epoch_length = scoring_epoch
                  )
                }, prefix = paste0(f$name, ":"))
              }, error = function(e) {
                showNotification(paste("Period detection error in", f$name, ":", e$message), type = "warning")
                return(NULL)
              })
            }

            # AFTER period detection: Mark non-wear epochs as NA in sleep state
            # This is for display/export purposes - doesn't affect period detection
            if (!is.null(sleep_state) && !is.null(wear_mask) && length(wear_mask) == length(sleep_state)) {
              sleep_state[!wear_mask] <- NA
            }
          }

          if (is.null(sleep_state) && is.null(periods)) next

          n_periods <- if (!is.null(periods) && nrow(periods) > 0) nrow(periods) else 0
          avg_duration <- if (n_periods > 0) mean(periods$sleep_time, na.rm = TRUE) else NA
          avg_efficiency <- if (n_periods > 0) mean(periods$sleep_efficiency, na.rm = TRUE) else NA
          avg_awakenings <- if (n_periods > 0) mean(periods$number_of_awakenings, na.rm = TRUE) else NA
          avg_waso <- if (n_periods > 0) mean(periods$wake_time, na.rm = TRUE) else NA
          avg_latency <- NA
          if (n_periods > 0 && "onset" %in% names(periods) && "in_bed_time" %in% names(periods)) {
            # clock strings read in UTC, which has no DST gap to turn them into dates
            onset_times <- as.POSIXct(periods$onset, tz = "UTC")
            in_bed_times <- as.POSIXct(periods$in_bed_time, tz = "UTC")
            latencies <- as.numeric(difftime(onset_times, in_bed_times, units = "mins"))
            avg_latency <- mean(latencies, na.rm = TRUE)
          }

          enhanced_frag <- NULL
          if ("timestamp" %in% names(data)) {
            enhanced_frag <- tryCatch({
              canhrActi::sleep.fragmentation.enhanced(
                sleep_state = sleep_state,
                timestamps = timestamps,
                epoch_length = scoring_epoch
              )
            }, error = function(e) {
              NULL
            })
          }

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            epoch_length = scoring_epoch,           # epoch sleep was scored at (60s)
            source_epoch_length = f$epoch_length,   # the file's native epoch
            algorithm = actual_algorithm,
            detection_method = detection_method_used,
            periods = periods,
            sleep_state = sleep_state,
            timestamps = timestamps,
            counts = counts,                        # (reintegrated) counts for the hypnogram
            wear_mask = wear_mask,  # Store wear mask for exports
            wear_time_applied = !is.null(wear_mask),  # Track if wear time was applied
            n_periods = n_periods,
            avg_duration = avg_duration,
            avg_efficiency = avg_efficiency,
            avg_awakenings = avg_awakenings,
            avg_waso = avg_waso,
            avg_latency = avg_latency,
            enhanced_fragmentation = enhanced_frag,
            parameters = list(
              sleep_algorithm = actual_algorithm,
              detection_method = detection_method_used,
              min_sleep_period = input$min_sleep_period,
              bedtime_start = input$bedtime_start,
              wake_time_end = input$wake_time_end,
              max_sleep_period = input$max_sleep_period,
              min_nonzero_epochs = input$min_nonzero_epochs,
              enhanced_fragmentation = TRUE,
              wear_time_filtered = !is.null(wear_mask)
            )
          )
        }

        gc(verbose = FALSE)
      })

      all_results <- Filter(Negate(is.null), all_results)
      results(all_results)
      run_stamp(Sys.time())
      shared$results$sleep <- all_results

      # Select the first result so the plots reference it.
      if (length(all_results) > 0) {
        first_id <- all_results[[1]]$subject_id %||% all_results[[1]]$name
        selected_file(first_id)
      }

      n_scored <- sum(sapply(all_results, function(r) {
        np <- r$n_periods
        if (is.null(np) || length(np) == 0) return(FALSE)
        as.numeric(np[1]) > 0
      }))
    }
    observeEvent(input$run_btn, run_counts())

    # Export Details CSV
    output$export_details <- downloadHandler(
      filename = function() {
        export_name("Details")
      },
      content = function(file) {
        res <- results()
        if (length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        df <- sleep_details_df(res, shared)
        if (is.null(df)) {
          write.csv(data.frame(Message = "No sleep periods to export"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE)
      }
    )

    # Export Summary CSV
    output$export_summary <- downloadHandler(
      filename = function() {
        export_name("Summary")
      },
      content = function(file) {
        res <- results()
        if (length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        df <- sleep_summary_df(res, shared)
        if (is.null(df)) {
          write.csv(data.frame(Message = "No sleep periods to export"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE)
      }
    )

    # The links sit in a closed menu, and a suspended downloadHandler never
    # receives its href.
    for (out in c("export_summary", "export_details")) {
      outputOptions(output, out, suspendWhenHidden = FALSE)
    }
  })
}
