# Module: Activity
#
# One plot over the cohort at the top, the Summary export as the table
# underneath. The other result sets are reached through the Export menu.

mod_activity_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "ac-page",
      uiOutput(ns("rule"), class = "ac-out"),
      tags$div(id = ns("settings_panel"), class = "ac-settings", style = "display: none;",
               ac_settings_panel(ns)),
      tags$div(id = ns("raw_settings"), class = "ac-settings", style = "display: none;",
               uiOutput(ns("raw_settings_panel"))),
      # Schedule panels, one per branch
      tags$div(id = ns("schedule_panel_c"), class = "ac-settings", style = "display: none;",
               uiOutput(ns("schedule_ui_c"))),
      tags$div(id = ns("schedule_panel_r"), class = "ac-settings", style = "display: none;",
               uiOutput(ns("schedule_ui_r"))),
      uiOutput(ns("figures"), class = "ac-out"),
      uiOutput(ns("plot_panel"), class = "ac-out ac-plot-out"),
      uiOutput(ns("table_panel"), class = "ac-out ac-table-out"),

      # Hidden; set by clicking a table row and read by everything downstream
      tags$div(class = "ac-hidden",
        selectInput(ns("selected_participant"), NULL,
                    choices = c("All recordings" = "all"), selectize = FALSE)),

      tags$div(id = ns("processing_indicator"), class = "ac-busy", style = "display: none;",
               tags$span(class = "ac-spinner", `aria-hidden` = "true"),
               tags$span(id = ns("processing_status"), "Classifying"))
    ),
    tags$script(HTML(ac_page_script(ns(""))))
  )
}

# The plots the page offers; per_file marks the one that draws a single recording
ac_plots <- function() {
  list(
    list(key = "intensity", label = "Intensity distribution", out = "intensity_plot", per_file = FALSE),
    list(key = "hourly",    label = "Hourly pattern",         out = "hourly_plot",    per_file = FALSE),
    list(key = "heatmap",   label = "VM heatmap",             out = "vm_heatmap_plot", per_file = TRUE)
  )
}

ac_cut_label <- function(k) {
  switch(k %||% "freedson",
    troiano        = "Troiano NHANES (2008)",
    matthews       = "Matthews (2005)",
    evenson        = "Evenson (2008)",
    copeland_older = "Copeland (2009)",
    sasaki_vm3     = "Freedson VM3 (Sasaki 2011)",
    freedson_vm3   = "Freedson VM3 (Sasaki 2011)",
    "Freedson (1998)")
}

# Settings panel: every parameter the run reads, with its unit and a note
ac_settings_panel <- function(ns) {
  field <- function(label, control, note, id = NULL) {
    tags$div(class = "ac-field",
      tags$div(class = "ac-field-k", label),
      control,
      tags$div(class = "ac-field-n", id = id, note))
  }
  num <- function(id, value, unit, ...) {
    tags$div(class = "ac-num",
      numericInput(ns(id), NULL, value = value, width = "100%", ...),
      tags$span(class = "ac-unit", `aria-hidden` = "true", unit))
  }
  sel <- function(id, choices, selected) {
    tags$div(class = "ac-select",
      selectInput(ns(id), NULL, choices = choices, selected = selected,
                  width = "100%", selectize = FALSE))
  }
  check <- function(id, label, value = FALSE) {
    tags$div(class = "ac-check", checkboxInput(ns(id), label, value = value))
  }

  tags$div(
    class = "ac-panel ac-settings-grid",
    tags$div(
      class = "ac-fields",

      # A field that only applies while another box is ticked follows that box
      # and is disabled when it does not apply
      field("Cut points",
            sel("cut_points", c(
              "Freedson (1998)" = "freedson",
              "Troiano NHANES (2008)" = "troiano",
              "Matthews (2005)" = "matthews",
              "Evenson (2008), children" = "evenson",
              "Copeland (2009), older adults" = "copeland_older",
              # ActiLife's Freedson VM3 is Sasaki, John and Freedson's set
              "Freedson VM3 (Sasaki 2011)" = "sasaki_vm3"), "freedson"),
            "sets the intensity thresholds"),
      field("Counts", sel("data_type", c("Axis 1" = "axis1", "Vector magnitude" = "vm"), "axis1"),
            "what the thresholds are read against"),

      field("Wear time filter", check("exclude_nonwear", "Valid days only", TRUE),
            "needs wear time run first"),
      field("METs", check("use_mets", "Calculate METs", TRUE),
            "the Avg METs column"),
      field("METs equation",
            sel("mets_algo", c("Freedson VM3 (Sasaki 2011)" = "freedson.vm3",
                               "Freedson Adult (1998)" = "freedson.adult",
                               "Crouter 2-regression (2010)" = "crouter"), "freedson.vm3"),
            "only while METs is on", id = ns("note_mets_algo")),
      field("Energy expenditure", check("use_ee", "Calculate kcals", TRUE),
            "needs body mass in the file"),

      field("Energy equation",
            sel("ee_algo", c("Freedson combination (1998)" = "freedson.combination",
                             "Freedson (1998)" = "freedson",
                             "Williams work-energy (1998)" = "williams"), "freedson.combination"),
            "only while kcals is on", id = ns("note_ee_algo")),
      field("MVPA bouts", check("use_bouts", "Detect bouts", TRUE),
            "the MVPA Bouts columns"),
      field("A bout lasts at least", num("bout_min", 10, "min", min = 1, max = 60, step = 1),
            "only while bouts are on", id = ns("note_bout_min")),
      field("Bout rule", sel("bout_rule", c("80% of the bout" = "80pct",
                                            "Strictly consecutive" = "consecutive"), "80pct"),
            "only while bouts are on", id = ns("note_bout_rule")),

      field("A sedentary bout lasts", num("sed_min_length", 10, "min", min = 1, max = 60, step = 1),
            "shorter runs are not bouts"),
      field("Sedentary drop time", num("sed_drop_time", 2, "min", min = 0, max = 10, step = 1),
            "movement allowed inside a bout"),
      field("Sedentary at or under", num("sed_threshold", 200, "cpm", min = 50, max = 500, step = 25),
            "counts per minute"),
      field("Sedentary counts", check("sed_use_vm", "Vector magnitude if present", TRUE),
            "otherwise axis 1"),

      field("First break", check("sed_ignore_first", "Ignore the first break of each day", FALSE),
            "waking often reads as a break")
    ),
    tags$div(
      class = "ac-settings-foot",
      tags$span(class = "ac-spacer"),
      actionButton(ns("clear_results"), "Clear results", class = "ac-btn ac-btn--text is-destructive")
    )
  )
}

# Page script. The menus open and close client side; Export clicks each of the
# download links Shiny already put on the page.
ac_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }
  // All four requests go out inside the click itself.
  //
  // The first version clicked the four links 500 ms apart, and only the first
  // two ever arrived: a browser allows a download while the click that asked
  // for it is still counted as a user action, which lasts about a second, and
  // drops the rest without saying anything. Firing them synchronously keeps all
  // four inside that one action. They go through hidden iframes rather than
  // link clicks so none of them is treated as a pop-up.
  function fireAll(row) {
    // every link the menu holds: the four the page always writes, and the
    // schedule's two when a schedule is in force
    var menu = document.getElementById(NS + 'export_menu');
    var ids = menu ? Array.prototype.slice.call(menu.querySelectorAll('a.ac-ei-link')) : [];
    var sent = 0;
    ids.forEach(function (a) {
      var href = a.getAttribute('href');
      if (!href) return;
      var f = document.createElement('iframe');
      f.style.display = 'none';
      f.src = href;
      document.body.appendChild(f);
      setTimeout(function () { if (f.parentNode) f.parentNode.removeChild(f); }, 120000);
      sent++;
    });

    // Files land in a folder the reader cannot see from here, so the menu says
    // what happened instead of closing on a click that looks like it did nothing.
    var label = row.querySelector('.ac-ei-label');
    if (label) {
      if (!label.dataset.rest) label.dataset.rest = label.textContent;
      label.textContent = sent === ids.length
        ? sent + ' files sent to your downloads folder'
        : (sent === 0 ? 'Nothing to export yet' : sent + ' of ' + ids.length + ' sent; try again');
      row.classList.toggle('is-done', sent === ids.length);
      row.classList.toggle('is-partial', sent > 0 && sent < ids.length);
      clearTimeout(row._t);
      row._t = setTimeout(function () {
        label.textContent = label.dataset.rest;
        row.classList.remove('is-done', 'is-partial');
        var back = row.contains(document.activeElement);
        closeMenus(null);
        if (back) focusButton('.ac-page .ac-export-btn');
      }, 2600);
    }
  }

  function closeMenus(except) {
    document.querySelectorAll('.ac-page .ac-menu, .ac-page .ac-whomenu, .ac-page .ac-gomenu, .ac-page .ac-exportmenu').forEach(function (m) {
      if (m !== except) m.style.display = 'none';
    });
    document.querySelectorAll('.ac-page .ac-pick, .ac-page .ac-who, .ac-page .ac-export-btn').forEach(function (b) {
      b.classList.remove('is-open');
      if (b.hasAttribute('aria-expanded')) b.setAttribute('aria-expanded', 'false');
    });
  }

  // Open one menu, closing whatever else was open. Returns nothing; the caller
  // has already decided this click belongs to it.
  function toggleMenu(button, menu) {
    var open = menu && menu.style.display !== 'none';
    closeMenus(null);
    if (menu && !open) {
      menu.style.display = 'block';
      button.classList.add('is-open');
      button.setAttribute('aria-expanded', 'true');
    }
  }

  // Go to: the grid scrolls so the group starts just right of the pinned columns
  function gridOf(el) {
    var panel = el.closest('.ac-tablepanel, .wtr-tables');
    return panel && panel.querySelector('.ac-scroll, .wtr-scroll');
  }
  function pinEdge(sc) {
    var edge = sc.getBoundingClientRect().left;
    sc.querySelectorAll('thead th.ac-pin, thead th.wtr-stick').forEach(function (p) {
      edge = Math.max(edge, p.getBoundingClientRect().right);
    });
    return edge;
  }
  function groupCells(sc) { return sc.querySelectorAll('thead th.ac-gg, thead th.acr-gg'); }
  function markGroup(go, menu) {
    var sc = gridOf(go);
    if (!sc || !menu) return;
    var edge = pinEdge(sc), cur = 0;
    groupCells(sc).forEach(function (t, i) {
      if (t.getBoundingClientRect().left <= edge + 1) cur = i;
    });
    menu.querySelectorAll('.ac-gi').forEach(function (it) {
      var on = Number(it.dataset.go) === cur;
      it.classList.toggle('is-on', on);
      it.querySelector('.ac-tick').textContent = on ? '\\u2713' : '';
    });
  }
  function goToGroup(item) {
    var sc = gridOf(item);
    var th = sc && groupCells(sc)[Number(item.dataset.go)];
    if (th) sc.scrollLeft += th.getBoundingClientRect().left - pinEdge(sc);
  }

  // Keyboard: Enter, Space or Down opens a menu and moves into it, the arrows
  // walk it, Enter or Space picks, Escape closes it and returns to its button
  var ITEMS = '.ac-mi, .ac-wi, .ac-gi, .ac-ei-all, a.ac-ei-link';
  function menuOf(b) {
    if (b.classList.contains('ac-goto')) return b.parentNode.querySelector('.ac-gomenu');
    if (b.classList.contains('ac-pick')) return document.querySelector('.ac-page .ac-menu');
    if (b.classList.contains('ac-who')) return document.querySelector('.ac-page .ac-whomenu');
    return document.getElementById(NS + 'export_menu');
  }
  function buttonSel(menu) {
    if (menu.classList.contains('ac-gomenu')) return null;
    if (menu.classList.contains('ac-menu')) return '.ac-page .ac-pick:not(.ac-goto)';
    if (menu.classList.contains('ac-whomenu')) return '.ac-page .ac-who';
    return '.ac-page .ac-export-btn';
  }
  function buttonOf(menu) {
    var s = buttonSel(menu);
    return s ? document.querySelector(s) : menu.parentNode.querySelector('.ac-goto');
  }
  // A pick redraws the panel holding the button, so the focus follows it to the new one
  var refocus = null;
  function focusButton(sel) {
    var old = document.querySelector(sel);
    if (old) old.focus();
    refocus = { sel: sel, old: old, until: Date.now() + 4000 };
  }
  if (window.jQuery) jQuery(document).on('shiny:value', function () {
    var r = refocus, tries = 0;
    if (!r || Date.now() > r.until) { refocus = null; return; }
    (function again() {
      // the reader has moved on to something else
      var a = document.activeElement;
      if (a && a !== document.body && a !== r.old && document.contains(a)) { if (refocus === r) refocus = null; return; }
      var el = document.querySelector(r.sel);
      if (el && el !== r.old) { if (refocus === r) refocus = null; el.focus(); return; }
      if (++tries < 20) setTimeout(again, 50);
    })();
  });

  document.addEventListener('click', function (e) {
    if (!e.target.closest || !e.target.closest('.ac-page')) { closeMenus(null); return; }

    // the raw interface: its window segments, chart chips and sortable
    // headers. Checked first because they carry their own data attributes
    // and would otherwise fall through to the counts handlers.
    // the schedule panel's remove buttons, one handler for any number of rows
    var swrm = e.target.closest('.ac-page [data-swrm]');
    if (swrm) { e.preventDefault(); setVal('sched_rm_window', swrm.dataset.swrm); return; }
    var srrm = e.target.closest('.ac-page [data-srrm]');
    if (srrm) { e.preventDefault(); setVal('sched_rm_range', srrm.dataset.srrm); return; }
    var aw = e.target.closest('.ac-page [data-acwindow]');
    if (aw) { e.preventDefault(); setVal('acwindow_set', aw.dataset.acwindow); return; }
    var ac = e.target.closest('.ac-page [data-actab]');
    if (ac) { e.preventDefault(); setVal('actab_set', ac.dataset.actab); return; }
    var as = e.target.closest('.ac-page [data-rawsort]');
    if (as) { e.preventDefault(); setVal('acsort_set', as.dataset.rawsort); return; }

    // Go to wears the chooser's class, so it is checked before the plot chooser
    var go = e.target.closest('.ac-page .ac-goto');
    if (go) {
      var gm = go.parentNode.querySelector('.ac-gomenu');
      markGroup(go, gm);
      toggleMenu(go, gm);
      return;
    }
    var gi = e.target.closest('.ac-page .ac-gi[data-go]');
    if (gi) { closeMenus(null); goToGroup(gi); return; }

    var pick = e.target.closest('.ac-page .ac-pick');
    if (pick) { toggleMenu(pick, document.querySelector('.ac-page .ac-menu')); return; }

    var item = e.target.closest('.ac-page .ac-mi[data-plot]');
    if (item) { closeMenus(null); setVal('plot_pick', item.dataset.plot); return; }

    var who = e.target.closest('.ac-page .ac-who');
    if (who) { toggleMenu(who, document.querySelector('.ac-page .ac-whomenu')); return; }

    // Same value the table rows set, so the button and the marked row are one
    // piece of state rather than two that can drift.
    var wi = e.target.closest('.ac-page .ac-wi[data-who]');
    if (wi) {
      var fid = wi.dataset.who;
      document.querySelectorAll('.ac-page .ac-row[data-fid]').forEach(function (r) {
        r.classList.toggle('is-selected', fid !== 'all' && r.dataset.fid === fid);
      });
      closeMenus(null);
      setVal('pick', fid);
      return;
    }

    var ex = e.target.closest('.ac-page .ac-export-btn');
    if (ex) { toggleMenu(ex, document.getElementById(NS + 'export_menu')); return; }

    var all = e.target.closest('.ac-page .ac-ei-all');
    if (all) { fireAll(all); return; }

    if (e.target.closest('.ac-page .ac-exportmenu')) return;

    var row = e.target.closest('.ac-page .ac-row[data-fid]');
    if (row) {
      var was = row.classList.contains('is-selected');
      document.querySelectorAll('.ac-page .ac-row.is-selected').forEach(function (r) { r.classList.remove('is-selected'); });
      if (!was) row.classList.add('is-selected');
      setVal('pick', was ? 'all' : row.dataset.fid);
      closeMenus(null);
      return;
    }

    closeMenus(null);
  });

  document.addEventListener('keydown', function (e) {
    var t = e.target;
    var menu = t.closest ? t.closest('.ac-page .ac-menu, .ac-page .ac-whomenu, .ac-page .ac-gomenu, .ac-page .ac-exportmenu') : null;
    if (e.key === 'Escape') {
      closeMenus(null);
      if (menu) { var mb = buttonOf(menu); if (mb) mb.focus(); }
      return;
    }
    if (!t.closest || !t.closest('.ac-page')) return;

    if (menu) {
      var items = Array.prototype.slice.call(menu.querySelectorAll(ITEMS));
      var i = items.indexOf(t.closest(ITEMS));
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp' || e.key === 'Home' || e.key === 'End') {
        e.preventDefault();
        if (items.length === 0) return;
        var k = e.key === 'Home' ? 0 : e.key === 'End' ? items.length - 1
              : e.key === 'ArrowDown' ? Math.min(i + 1, items.length - 1) : Math.max(i - 1, 0);
        items[k].focus();
      } else if ((e.key === 'Enter' || e.key === ' ') && i >= 0) {
        // a download link acts on Enter by itself
        if (items[i].tagName === 'A' && e.key === 'Enter') return;
        e.preventDefault();
        var sel = buttonSel(menu), gb = sel ? null : buttonOf(menu);
        var stays = menu.classList.contains('ac-exportmenu');
        items[i].click();
        if (gb) gb.focus();
        else if (!stays) focusButton(sel);
      } else if (e.key === 'Tab') {
        closeMenus(null);
      }
      return;
    }

    var btn = t.closest('.ac-pick, .ac-who, .ac-export-btn');
    if (btn && (e.key === 'Enter' || e.key === ' ' || e.key === 'ArrowDown')) {
      e.preventDefault();
      var m = menuOf(btn);
      if (m && m.style.display === 'none') btn.click();
      if (m && m.style.display !== 'none') {
        var its = Array.prototype.slice.call(m.querySelectorAll(ITEMS));
        var on = m.querySelector('.is-on');
        var first = on && its.indexOf(on) >= 0 ? on : its[0];
        if (first) first.focus();
      }
      return;
    }

    var row = e.target.closest('.ac-row[data-fid]');
    if (!row) return;
    var rows = Array.prototype.slice.call(document.querySelectorAll('.ac-page .ac-row[data-fid]'));
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

mod_activity_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Drop the results of recordings removed on Overview
    observeEvent(names(shared$files), {
      res <- results()
      if (!length(res)) return()
      keep <- intersect(names(res), names(shared$files))
      if (length(keep) == length(res)) return()
      results(res[keep])
      shared$results$activity <- res[keep]
    }, ignoreNULL = FALSE)
    run_stamp <- reactiveVal(NULL)

    # Schedules: windows inside the day and kinds of day, one schedule per
    # branch since the branches hold different recordings. The model is in
    # activity_schedule.R and the panel in activity_loaded.R.

    # The epoch the counts files share, for the boundary rule; NULL when mixed.
    sched_epoch <- function() {
      e <- unique(vapply(shared$files, function(f) as.numeric(f$epoch_length %||% NA), numeric(1)))
      e <- e[is.finite(e)]
      if (length(e) == 1) e else NULL
    }

    sched_branch <- function(idp, kind, panel_id, out_id, epoch_fn, is_view, diary_ui = NULL) {
      # Session only
      sched <- reactiveVal(sched_new())
      layout <- reactiveVal(0L)          # bumps when a row is added or removed
      drawn <- reactiveVal(FALSE)
      id <- function(x) paste0(idp, x)

      # Read the boxes back into the schedule. A box that has not rendered yet
      # reads NULL and keeps the schedule's value; an empty tick group also
      # reads NULL, so it is trusted only once the panel has been drawn.
      from_inputs <- function(s) {
        was_drawn <- isolate(drawn())
        wd <- input[[id("st_weekday")]]; we <- input[[id("st_weekend")]]
        s$types <- c(weekday = if (is.null(wd)) s$types[["weekday"]] else trimws(wd),
                     weekend = if (is.null(we)) s$types[["weekend"]] else trimws(we))
        mw <- suppressWarnings(as.numeric(input[[id("s_minwear")]]))
        if (length(mw) == 1 && is.finite(mw)) s$min_wear <- min(1, max(0, mw / 100))
        for (i in seq_along(s$windows)) {
          w <- s$windows[[i]]
          lab <- input[[id(paste0("sw_label_", i))]]; st <- input[[id(paste0("sw_start_", i))]]
          en <- input[[id(paste0("sw_end_", i))]]; ap <- input[[id(paste0("sw_applies_", i))]]
          if (!is.null(lab)) w$label <- trimws(lab)
          if (!is.null(st)) w$start <- sched_parse_time(st)
          if (!is.null(en)) w$end <- sched_parse_time(en)
          if (!is.null(ap) || was_drawn) w$applies <- as.character(ap %||% character(0))
          s$windows[[i]] <- w
        }
        for (i in seq_along(s$ranges)) {
          r <- s$ranges[[i]]
          lab <- input[[id(paste0("sr_label_", i))]]; fr <- input[[id(paste0("sr_from_", i))]]
          to <- input[[id(paste0("sr_to_", i))]]
          if (!is.null(lab)) r$label <- trimws(lab)
          if (!is.null(fr)) r$from <- sched_parse_date(fr)
          if (!is.null(to)) r$to <- sched_parse_date(to)
          s$ranges[[i]] <- r
        }
        s
      }
      # Reading the boxes is what registers them. After a row is added or
      # removed the boxes on screen are still the old rows until the browser
      # redraws, so that run only registers them and applies nothing; the
      # redrawn values arrive as changes. sched() is read in isolation so the
      # write back cannot start a loop.
      seen <- 0L
      observe({
        lay <- layout()
        s <- isolate(sched())
        s2 <- from_inputs(s)
        if (!identical(lay, seen)) { seen <<- lay; return() }
        if (!identical(s2, s)) {
          sched(s2)
          # Update the pills in place; redrawing the panel would take the focus
          # out of the box being typed in
          if (!identical(sched_type_labels(s2), sched_type_labels(s))) {
            ch <- sched_pill_choices(s2)
            for (i in seq_along(s2$windows)) {
              updateCheckboxGroupInput(session, id(paste0("sw_applies_", i)), choices = ch,
                                       selected = intersect(as.character(s2$windows[[i]]$applies), unname(ch)),
                                       inline = TRUE)
            }
          }
        }
      })
      bump <- function(s) { sched(s); layout(isolate(layout()) + 1L) }
      observeEvent(input[[id("sw_add")]], {
        s <- from_inputs(sched())
        s$windows[[length(s$windows) + 1]] <- list(label = "", start = NA_real_, end = NA_real_,
                                                   applies = character(0))
        bump(s)
      })
      observeEvent(input[[id("sr_add")]], {
        s <- from_inputs(sched())
        # windows point at the key, so it survives renaming and reordering
        key <- paste0("range_", format(Sys.time(), "%H%M%S"), "_", length(s$ranges) + 1)
        s$ranges[[length(s$ranges) + 1]] <- list(key = key, label = "", from = as.Date(NA), to = as.Date(NA))
        bump(s)
      })
      observeEvent(input[[id("sched_clear")]], bump(sched_new()))
      rm_window <- function(i) {
        s <- from_inputs(sched())
        if (!is.na(i) && i >= 1 && i <= length(s$windows)) { s$windows[[i]] <- NULL; bump(s) }
      }
      rm_range <- function(i) {
        s <- from_inputs(sched())
        if (!is.na(i) && i >= 1 && i <= length(s$ranges)) {
          key <- s$ranges[[i]]$key
          s$ranges[[i]] <- NULL
          for (j in seq_along(s$windows)) s$windows[[j]]$applies <- setdiff(s$windows[[j]]$applies, key)
          bump(s)
        }
      }
      observeEvent(input[[id("sched_open")]], shinyjs::toggle(panel_id))
      output[[out_id]] <- renderUI({
        layout()
        if (!is_view()) return(NULL)
        s <- isolate(sched())
        isolate(drawn(TRUE))
        ac_sched_panel(ns, s, prefix = if (identical(kind, "raw")) "wt" else "ac", idp = idp,
                       foot = if (identical(kind, "raw")) "Applied by Re-run."
                              else "The By window and By day type views follow this as you type.",
                       diary = if (is.null(diary_ui)) NULL else diary_ui(s))
      })
      # Each fault is reported under the table it belongs to.
      faults <- reactive(sched_validate(sched(), epoch_fn()))
      output[[id("sched_msg_w")]] <- renderUI(ac_sched_msgs(grep("^Window", faults(), value = TRUE)))
      output[[id("sched_msg_r")]] <- renderUI(ac_sched_msgs(grep("^Date range", faults(), value = TRUE)))
      output[[id("sched_msg_t")]] <- renderUI(ac_sched_msgs(grep("^(Window|Date range)", faults(), value = TRUE, invert = TRUE)))
      list(sched = sched, rm_window = rm_window, rm_range = rm_range, bump = bump)
    }

    # Diaries: a csv in GGIR's activity diary layout (one row per participant
    # and date, the start time of each activity under its name), matched to
    # the loaded recordings by subject id, loaded name or device serial. A
    # matched participant's days replace the typed windows on those days.
    diary_ui_for <- function(idp) function(s) list(
      # a button rather than an <a>, which the page's link rules would recolour
      action = tags$button(type = "button", class = "ac-sched-add",
                           onclick = sprintf("document.getElementById('%s').click(); return false;",
                                             ns(paste0(idp, "diary_file"))),
                           ac_plus_icon(), "Import diary"),
      body = tagList(
        tags$div(style = "display: none;",
                 fileInput(ns(paste0(idp, "diary_file")), NULL, accept = c(".csv", "text/csv"))),
        uiOutput(ns(paste0(idp, "diary_note")))))

    sc <- sched_branch("c_", "counts", "schedule_panel_c", "schedule_ui_c",
                       sched_epoch, function() !identical(view(), "raw"), diary_ui_for("c_"))
    sr <- sched_branch("r_", "raw", "schedule_panel_r", "schedule_ui_r",
                       function() NULL, function() identical(view(), "raw"), diary_ui_for("r_"))
    sched_c <- sc$sched
    sched_r <- sr$sched

    counts_recs <- function() {
      lapply(names(shared$files), function(fid) {
        f <- shared$files[[fid]]
        nm <- c(f$subject_info$id, f$name, f$device_info$serial_number)
        nm <- as.character(unlist(nm))
        list(pid = fid, names = nm[!is.na(nm) & nzchar(nm)])
      })
    }
    raw_recs <- function() {
      rs <- raw_list(); ids <- raw_ids()
      lapply(ids, function(id) {
        r <- rs[[id]]
        pid <- tryCatch(as.character(r$inspection$id %||% id), error = function(e) id)
        nm <- tryCatch(c(ovr_name(r, id), as.character(r$device$serial %||% "")), error = function(e) id)
        list(pid = pid, names = c(nm[!is.na(nm) & nzchar(nm)], id))
      })
    }
    diary_import <- function(f, branch, recs) {
      rd <- sched_read_ggir_diary(f$datapath[1])
      m <- sched_match_diary(rd$entries, recs)
      s <- branch$sched()
      s$overrides <- m$overrides
      s$diary <- list(file = f$name[1], ids = length(rd$entries), matched = length(m$matched),
                      unmatched = m$unmatched, dateformat = rd$dateformat, problems = rd$problems,
                      days = sum(vapply(m$overrides, function(o) length(unique(o$date)), integer(1))))
      branch$bump(s)
    }
    observeEvent(input$c_diary_file, {
      f <- input$c_diary_file
      if (!is.null(f) && nrow(f) > 0) diary_import(f, sc, counts_recs())
    })
    observeEvent(input$r_diary_file, {
      f <- input$r_diary_file
      if (!is.null(f) && nrow(f) > 0) diary_import(f, sr, raw_recs())
    })
    observeEvent(input$c_diary_clear, { s <- sched_c(); s$overrides <- list(); s$diary <- NULL; sc$bump(s) })
    observeEvent(input$r_diary_clear, { s <- sched_r(); s$overrides <- list(); s$diary <- NULL; sr$bump(s) })
    diary_note <- function(s, idp) {
      d <- s$diary
      if (is.null(d)) {
        return(tags$div(class = "ac-sched-none",
          "None. Import a diary, one row per participant with a date and the start time of each activity under its name, and its days replace the windows above."))
      }
      fmt_words <- c("%d-%m-%Y" = "day-month-year", "%Y-%m-%d" = "year-month-day", "%d/%m/%Y" = "day/month/year",
                     "%m/%d/%Y" = "month/day/year", "%Y/%m/%d" = "year/month/day")
      tagList(
        tags$div(class = "ac-sched-none",
          paste0(d$file, " · ", d$matched, " of ", d$ids, if (d$ids == 1) " participant" else " participants",
                 " matched a loaded recording · ", d$days, if (d$days == 1) " day" else " days",
                 if (!is.na(d$dateformat)) paste0(" · dates read as ", fmt_words[d$dateformat] %||% d$dateformat) else "")),
        if (length(d$unmatched) > 0)
          tags$div(class = "ac-sched-none", paste0("Not loaded here: ", paste(d$unmatched, collapse = ", "))),
        if (length(d$problems) > 0) ac_sched_msgs(d$problems),
        tags$div(class = "ac-sched-links",
          tags$button(id = ns(paste0(idp, "diary_clear")), type = "button",
                      class = "action-button ac-sched-add is-quiet", "Remove diary")))
    }
    output$c_diary_note <- renderUI(diary_note(sched_c(), "c_"))
    output$r_diary_note <- renderUI(diary_note(sched_r(), "r_"))
    # On counts the schedule only slices scored epochs, so the views follow
    # sched_c() live. On raw it goes into part 5, so the run keeps the
    # schedule it was scored with.
    raw_run_sched <- reactiveVal(NULL)      # the raw run's schedule
    table_view <- reactiveVal("summary")
    observe({ shared$schedule <- list(counts = sched_c(), raw = sched_r()) })
    observeEvent(input$table_view, table_view(input$table_view), ignoreInit = TRUE)
    # Switching branch closes the open schedule panel
    observeEvent(view(), {
      shinyjs::hide("schedule_panel_c"); shinyjs::hide("schedule_panel_r")
    }, ignoreInit = TRUE)
    # Remove buttons report branch and row, "c:2" being the counts branch's second row
    sched_rm_target <- function(v) {
      p <- strsplit(as.character(v), ":", fixed = TRUE)[[1]]
      list(branch = p[1], i = suppressWarnings(as.integer(p[2])))
    }
    observeEvent(input$sched_rm_window, {
      t <- sched_rm_target(input$sched_rm_window)
      if (identical(t$branch, "r")) sr$rm_window(t$i) else sc$rm_window(t$i)
    })
    observeEvent(input$sched_rm_range, {
      t <- sched_rm_target(input$sched_rm_range)
      if (identical(t$branch, "r")) sr$rm_range(t$i) else sc$rm_range(t$i)
    })
    # Raw only: the run carries the signature of the schedule part 5 was given
    raw_sched_moved <- reactive({
      tus <- timeuse()
      if (length(tus) == 0) return(FALSE)
      !identical(tus[[1]]$settings$schedule_sig %||% "", sched_sig(sched_r()))
    })
    # and the cut points and bouts in the settings panel, against a recording that scored
    raw_settings_moved <- reactive({
      ok <- Filter(function(t) length(t$settings) > 0, timeuse())
      length(ok) > 0 && acr_settings_moved(ok[[1]], input)
    })

    # canhrActi_<what>_<ISO date>_<time>.csv
    export_name <- function(what) {
      paste0("canhrActi_", what, "_",
             format(run_stamp() %||% Sys.time(), "%Y-%m-%d_%H%M%S"), ".csv")
    }

    # Update participant selector when results change
    observe({
      res <- results()
      if (length(res) > 0) {
        choices <- c("All participants" = "all")
        for (fid in names(res)) {
          r <- res[[fid]]
          label <- r$subject_id %||% r$name %||% fid
          choices <- c(choices, setNames(fid, label))
        }
        updateSelectInput(session, "selected_participant",
          choices = choices,
          selected = isolate(input$selected_participant) %||% "all")
      } else {
        updateSelectInput(session, "selected_participant",
          choices = c("All participants" = "all"),
          selected = "all")
      }
    })

    # Raw interface. Part 5 is raw.timeuse() over the part-4 nights from
    # raw.sleep.nights(), not the part-3 nights the read leaves behind; both
    # run once per recording and are cached for the session.
    raw_ids <- reactive(names(shared$raw %||% list()))
    raw_list <- reactive(shared$raw %||% list())
    has_raw <- reactive(length(raw_ids()) > 0)
    has_counts <- reactive(length(shared$files) > 0)
    view <- reactive({
      if (!has_raw()) return("counts")
      if (!has_counts()) return("raw")
      if (identical(local_raw$view, "raw")) "raw" else "counts"
    })
    local_raw <- reactiveValues(view = "raw", sel = NULL, tab = "daysummary",
                                window = NULL, sort = NULL, dir = 1)
    # A plain environment, not a reactiveVal: timeuse() both reads and fills
    # the cache, and a reactiveVal would invalidate the expression writing it.
    # tu_tick is the reactive signal, bumped after a fill.
    tu_env <- new.env(parent = emptyenv())
    tu_env$cache <- list()
    tu_env$nights <- list()
    tu_env$reports <- list()
    tu_tick <- reactiveVal(0L)

    raw_switch <- function() {
      if (!(has_counts() && has_raw())) return(NULL)
      v <- view()
      seg <- function(key, label, n) {
        tags$button(id = ns(paste0("act_view_", key)), type = "button",
                    class = paste("action-button ovr-seg-b",
                                  if (identical(v, key)) "is-on" else ""),
                    label, tags$b(fmt_int(n)))
      }
      tags$div(class = "ovr-seg", role = "tablist",
               seg("counts", "Counts", length(shared$files)),
               seg("raw", "Raw", length(raw_ids())))
    }
    observeEvent(input$act_view_counts, local_raw$view <- "counts")
    observeEvent(input$act_view_raw, local_raw$view <- "raw")

    # Part 5 for every loaded raw recording, scored on Run analysis and cached
    raw_run <- reactiveVal(0L)

    timeuse <- reactive({
      ids <- raw_ids(); rs <- raw_list()
      tu_tick()
      if (raw_run() == 0L) return(list())
      if (length(ids) == 0) return(list())
      cache <- tu_env$cache
      todo <- setdiff(ids, names(cache))
      if (length(todo) > 0) {
        withProgress(message = "Running GGIR part 5", value = 0, {
          for (k in seq_along(todo)) {
            id <- todo[k]
            incProgress(1 / length(todo), detail = ovr_name(rs[[id]], id))
            cache[[id]] <- tryCatch({
              nights <- canhrActi::raw.sleep.nights(rs[[id]])
              tu_env$nights[[id]] <- nights
              pp <- canhrActi::raw.params()
              ov <- isolate(acr_params())
              # cutpoint is not a GGIR parameter; it is stamped onto the result
              chosen <- ov$cutpoint
              ov$cutpoint <- NULL
              for (n in names(ov)) pp[[n]] <- ov[[n]]
              # Windows go in as a qwindow diary matched on the ID part 5 takes
              # from the night summary; with no ID the numeric form is used
              s_run <- isolate(raw_run_sched())
              if (sched_has_windows(s_run)) {
                # a fault here is the schedule's, so it is named as such
                pp$qwindow <- tryCatch({
                  pid <- as.character(nights$ID[1] %||% NA)
                  sp <- ovr_span(rs[[id]]); rtz <- ovr_tz(rs[[id]])
                  if (is.null(sp)) stop("the recording has no time span")
                  dd <- seq(as.Date(format(sp$start, "%Y-%m-%d", tz = rtz)),
                            as.Date(format(sp$end, "%Y-%m-%d", tz = rtz)), by = "day")
                  q <- if (length(pid) == 1 && !is.na(pid) && nzchar(pid)) sched_qwindow(s_run, pid, dd) else NULL
                  if (!is.null(q)) q else sched_qwindow_numeric(s_run)
                }, error = function(e) stop("The schedule could not be applied: ", conditionMessage(e), call. = FALSE))
              }
              # The set's metric must have been stored at read time; part 5's
              # own message for a missing one does not say what to do
              have <- acr_metrics_present(rs[[id]])
              m <- pp$acc.metric %||% "ENMO"
              if (length(have) > 0 && !m %in% have) {
                stop(m, " is not in this recording, which was read with ",
                     paste(have, collapse = ", "), ". ",
                     if (m %in% RAW_READ_HAS)
                       "Remove and re-add the file to read it as well."
                     else
                       # not read by the import: its filter bank adds about a
                       # quarter to every read
                       paste0("Reading the file again will not add it: ", m,
                              " needs GGIR's filter bank, which this app does",
                              " not run. Score this recording with raw.timeuse()",
                              " directly if you need it."),
                     call. = FALSE)
              }
              tu <- canhrActi::raw.timeuse(rs[[id]], nights = nights, params = pp)
              tu$settings$cutpoint <- chosen %||% ACR_CUSTOM
              tu$settings$acc.metric <- pp$acc.metric %||% "ENMO"
              tu$settings$schedule_sig <- sched_sig(s_run)
              tu
            }, error = function(e) structure(list(
              daysummary = NULL, windows = NULL, settings = list(),
              status = list(state = "error", messages = conditionMessage(e))),
              class = "canhrActi_raw_timeuse"))
          }
        })
        tu_env$cache <- cache
        # Published for the Visualization tab, which reads it rather than scoring its own
        shared$raw_timeuse <- cache
        isolate(tu_tick(tu_tick() + 1L))
      }
      cache[ids]
    })

    # Which day window the page is on; the first the recordings offer.
    active_window <- reactive({
      tus <- timeuse()
      if (length(tus) == 0) return(NULL)
      w <- unique(unlist(lapply(tus, acr_windows)))
      if (length(w) == 0) return(NULL)
      if (!is.null(local_raw$window) && local_raw$window %in% w) return(local_raw$window)
      # A run made with windows opens on them
      if ("Segments" %in% w && sched_has_windows(isolate(raw_run_sched()))) "Segments" else w[1]
    })

    observeEvent(input$acwindow_set, {
      local_raw$window <- input$acwindow_set
      local_raw$sort <- NULL; local_raw$dir <- 1
    }, ignoreInit = TRUE)
    observeEvent(input$actab_set, {
      if (input$actab_set %in% names(ACR_TABS)) {
        local_raw$tab <- input$actab_set
        # the two tables do not share a column set, so a sort cannot carry
        local_raw$sort <- NULL; local_raw$dir <- 1
      }
    }, ignoreInit = TRUE)
    # The recording chooser; one at a time, no All
    observeEvent(input$acwho_set, {
      v <- input$acwho_set
      was <- local_raw$sel %||% raw_ids()[1]
      local_raw$sel <- if (!v %in% raw_ids()) NULL else v
      # a redrawn box sends the value it was drawn with; only a change is a pick
      if (v %in% raw_ids() && !identical(v, was)) focus_set(shared, v)
    }, ignoreInit = TRUE)
    observeEvent(input$acsort_set, {
      i <- suppressWarnings(as.integer(input$acsort_set))
      if (length(i) != 1 || is.na(i)) return()
      if (identical(local_raw$sort, i)) local_raw$dir <- -local_raw$dir
      else { local_raw$sort <- i; local_raw$dir <- 1 }
    }, ignoreInit = TRUE)
    observeEvent(raw_ids(), {
      if (!is.null(local_raw$sel) && !local_raw$sel %in% raw_ids()) local_raw$sel <- NULL
      if (is.null(local_raw$sel) && length(raw_ids()) > 0) local_raw$sel <- raw_ids()[1]
    })

    raw_current <- reactive({
      ids <- raw_ids(); i <- if (is.null(local_raw$sel)) 1L else match(local_raw$sel, ids)
      if (is.na(i)) i <- 1L
      i
    })

    # GGIR's own report and csv files, one run per recording, cached
    reports <- reactive({
      ids <- raw_ids(); rs <- raw_list(); tus <- timeuse()
      if (raw_run() == 0L) return(list())
      if (length(ids) == 0) return(list())
      tu_tick()
      cache <- tu_env$reports %||% list()
      todo <- setdiff(ids, names(cache))
      if (length(todo) > 0) {
        withProgress(message = "Running GGIR's report", value = 0, {
          for (k in seq_along(todo)) {
            id <- todo[k]
            incProgress(1 / length(todo), detail = ovr_name(rs[[id]], id))
            # With a schedule the report also gets the schedule's segment wear
            # rule and GGIR's weekday and weekend aggregates; the two arguments
            # are passed only when the installed wrapper takes them
            s_run <- isolate(raw_run_sched())
            takes <- all(c("params_cleaning", "params_output") %in% names(formals(canhrActi::raw.ggir.report)))
            cache[[id]] <- tryCatch(
              if (sched_active(s_run) && takes)
                canhrActi::raw.ggir.report(rs[[id]], tu_env$nights[[id]], tus[[id]],
                  params_cleaning = list(segmentWEARcrit.part5 = as.numeric(s_run$min_wear %||% 0.5)),
                  params_output = list(week_weekend_aggregate.part5 = TRUE))
              else
                canhrActi::raw.ggir.report(rs[[id]], tu_env$nights[[id]], tus[[id]]),
              error = function(e) list(state = "error", pdf = NA_character_,
                                       csv = character(0), messages = conditionMessage(e)))
          }
        })
        tu_env$reports <- cache
        # Published for the Visualization tab, which draws the day panel from $dir
        shared$raw_report <- cache
      }
      cache[ids]
    })
    # GGIR's panelplot over the report's milestone tree, drawn to pdf and
    # rasterised: at a 5 s epoch a bout is a fraction of a pixel wide, which
    # a png device drops and a vector device keeps.
    raw_panel_w <- function() {
      w <- session$clientData[[paste0("output_", ns("raw_report"), "_width")]]
      if (is.null(w) || !is.finite(w) || w < 200) return(1100)
      # snapped to 100 px so a window drag does not queue a redraw per pixel
      round(w / 100) * 100
    }

    output$raw_report <- renderImage({
      reps <- reports(); i <- raw_current()
      rep <- reps[[i]]
      req(!is.null(rep), identical(rep$state, "ok"), !is.na(rep$dir))
      tz <- tryCatch(ovr_tz(raw_list()[[raw_ids()[i]]]), error = function(e) "")
      if (is.na(tz) || !nzchar(tz)) tz <- ""
      w <- raw_panel_w()
      f <- tempfile(fileext = ".png")
      r <- canhrActi::raw.ggir.panel.png(rep$dir, f, width_px = round(w * 2),
                                         desiredtz = tz)
      if (!identical(r$state, "ok")) {
        stop(switch(r$state,
                    no_pdftools = "Install the pdftools and png packages to see the report.",
                    no_windows  = "No window in this recording is long enough to draw.",
                    paste("The report could not be drawn:", r$state)))
      }
      list(src = f, contentType = "image/png",
           width = w, height = round(r$height * w / r$width),
           alt = "GGIR time use report")
    }, deleteFile = TRUE)




    # Raw settings. Re-run clears the cache; part 5 starts from the
    # milestone, so the file is not read again.
    output$raw_settings_panel <- renderUI({
      tus <- timeuse()
      if (length(raw_ids()) == 0) return(NULL)
      r <- tryCatch(raw_list()[[raw_ids()[raw_current()]]], error = function(e) NULL)
      # NULL before the first run, so the cut points can be chosen up front.
      acr_settings_panel(if (length(tus) == 0) NULL else tus[[raw_current()]],
                         ns, have = acr_metrics_present(r))
    })
    toggle_raw_settings <- function() shinyjs::toggle("raw_settings")
    observeEvent(input$raw_change, toggle_raw_settings())
    observeEvent(input$raw_change_empty, toggle_raw_settings())

    # A published set fills the three boxes; editing a box afterwards moves
    # the chooser to Custom.
    acr_guard <- reactiveVal(0L)     # > 0 while the boxes are being written from here

    output$acr_gap <- renderUI({
      g <- acr_cutpoint_gap(input$acr_cutpoint %||% ACR_CUSTOM)
      if (!nzchar(g)) return(NULL)
      tags$div(class = "wt-settings-note", g)
    })

    observeEvent(input$acr_cutpoint, {
      k <- input$acr_cutpoint
      if (is.null(k) || identical(k, ACR_CUSTOM)) return()
      cp <- tryCatch(canhrActi::raw.cutpoint(k), error = function(e) NULL)
      if (is.null(cp)) return()
      # A band the set does not define keeps GGIR's default
      d <- c(light = 40, moderate = 100, vigorous = 400)
      acr_guard(isolate(acr_guard()) + 1L)
      updateNumericInput(session, "acr_thr_lig",
                         value = if (is.na(cp$threshold.lig)) d[["light"]] else cp$threshold.lig)
      updateNumericInput(session, "acr_thr_mod",
                         value = if (is.na(cp$threshold.mod)) d[["moderate"]] else cp$threshold.mod)
      updateNumericInput(session, "acr_thr_vig",
                         value = if (is.na(cp$threshold.vig)) d[["vigorous"]] else cp$threshold.vig)
    }, ignoreInit = TRUE)

    observeEvent(list(input$acr_thr_lig, input$acr_thr_mod, input$acr_thr_vig), {
      # the write above fires this too, so one event is swallowed per set applied
      if (isolate(acr_guard()) > 0) { acr_guard(isolate(acr_guard()) - 1L); return() }
      k <- input$acr_cutpoint
      if (is.null(k) || identical(k, ACR_CUSTOM)) return()
      cp <- tryCatch(canhrActi::raw.cutpoint(k), error = function(e) NULL)
      if (is.null(cp)) return()
      d <- c(40, 100, 400)
      want <- c(if (is.na(cp$threshold.lig)) d[1] else cp$threshold.lig,
                if (is.na(cp$threshold.mod)) d[2] else cp$threshold.mod,
                if (is.na(cp$threshold.vig)) d[3] else cp$threshold.vig)
      got <- suppressWarnings(as.numeric(c(input$acr_thr_lig, input$acr_thr_mod, input$acr_thr_vig)))
      if (length(got) == 3 && all(is.finite(got)) && !isTRUE(all.equal(want, got))) {
        updateSelectInput(session, "acr_cutpoint", selected = ACR_CUSTOM)
      }
    }, ignoreInit = TRUE)

    acr_params <- reactiveVal(list())

    # Run analysis (in the bar or the empty panel) and Re-run do the same
    # thing; one observer per button, each calling this
    raw_start <- function() {
      acr_params(acr_form_params(input))
      raw_run_sched(sched_r())
      tu_env$cache <- list(); tu_env$reports <- list()   # every recording re-scored
      shared$raw_timeuse <- list()   # scored against the old settings
      shared$raw_report <- list()   # scored against the old settings
      raw_run(isolate(raw_run()) + 1L)
      tu_tick(isolate(tu_tick()) + 1L)
      local_raw$sort <- NULL; local_raw$dir <- 1
    }
    observeEvent(input$raw_run, raw_start())
    observeEvent(input$raw_run_empty, raw_start())
    observeEvent(input$raw_reanalyse, raw_start())

    observeEvent(input$acr_defaults, {
      d <- list(threshold.lig = 40, threshold.mod = 100, threshold.vig = 400,
                boutcriter.mvpa = 0.8, boutcriter.in = 0.9)
      for (n in names(d)) {
        updateNumericInput(session, paste0("acr_", sub("^threshold\\.", "thr_",
          sub("^boutcriter\\.", "bc_", n))), value = d[[n]])
      }
      updateTextInput(session, "acr_bd_mvpa", value = "10, 5, 1")
      updateTextInput(session, "acr_bd_in", value = "30, 20, 10")
      # GGIR's 40/100/400 defaults are not one of the published rows, so Custom
      updateSelectInput(session, "acr_cutpoint", selected = ACR_CUSTOM)
    })

    # Both downloads hand over GGIR's own files unchanged
    output$raw_pdf <- downloadHandler(
      filename = function() {
        rep <- reports()[[raw_current()]]
        if (!is.null(rep) && !is.na(rep$pdf)) basename(rep$pdf) else "report.pdf"
      },
      content = function(file) {
        rep <- reports()[[raw_current()]]
        validate(need(!is.null(rep) && !is.na(rep$pdf) && file.exists(rep$pdf),
                      "GGIR drew no report for this recording."))
        file.copy(rep$pdf, file, overwrite = TRUE)
      })

    output$raw_export <- downloadHandler(
      filename = function() {
        p <- acr_csv_for(reports()[[raw_current()]], local_raw$tab, active_window())
        if (is.na(p)) "part5.csv" else basename(p)
      },
      content = function(file) {
        p <- acr_csv_for(reports()[[raw_current()]], local_raw$tab, active_window())
        validate(need(!is.na(p) && file.exists(p), "GGIR wrote no file for this selection."))
        file.copy(p, file, overwrite = TRUE)
      })

    # GGIR's study-level report: g.report.part5 over every recording's part-5
    # milestone at once, with the overrides reports() passed, under GGIR's names
    output$raw_export_all <- downloadHandler(
      filename = function() "part5_report.zip",
      content = function(file) {
        s_run <- isolate(raw_run_sched())
        args <- if (sched_active(s_run))
          list(params_cleaning = list(segmentWEARcrit.part5 = as.numeric(s_run$min_wear %||% 0.5)),
               params_output = list(week_weekend_aggregate.part5 = TRUE))
        else list()
        st <- do.call(canhrActi:::.raw.ggir.report.study, c(list(reports()), args))
        if (is.character(st$dir) && !is.na(st$dir)) on.exit(unlink(st$dir, recursive = TRUE), add = TRUE)
        # a failed download shows nothing on the page, so say why first
        if (!identical(st$state, "ok") || length(st$csv) == 0)
          showNotification(paste(c("GGIR wrote no study report.", st$messages), collapse = " "), type = "error")
        validate(need(identical(st$state, "ok") && length(st$csv) > 0,
                      paste(c("GGIR wrote no study report.", st$messages), collapse = " ")))
        zip::zipr(zipfile = file, files = unname(st$csv))
      })

    # A clicked row scopes the page to that recording; everything downstream
    # reads selected_participant
    observeEvent(input$pick, {
      v <- input$pick %||% "all"
      updateSelectInput(session, "selected_participant", selected = v)
      focus_set(shared, v)
    })

    # Opening the tab takes up the recording picked on another tab, on either side
    on_tab_shown(shared, "activity", function() {
      id <- focus_get(shared, among = intersect(names(results()), names(shared$files)))
      if (!is.null(id)) updateSelectInput(session, "selected_participant", selected = id)
      rid <- focus_get(shared, among = raw_ids())
      if (!is.null(rid)) local_raw$sel <- rid
    })

    observeEvent(input$toggle_advanced, {
      shinyjs::toggle("settings_panel")
    })

    # Fields that only apply while another box is ticked are disabled, not hidden
    observeEvent(input$use_mets, {
      on <- isTRUE(input$use_mets)
      shinyjs::toggleState("mets_algo", condition = on)
      shinyjs::html("note_mets_algo", if (on) "fills the Avg METs column" else "only while METs is on")
    }, ignoreInit = FALSE)

    observeEvent(input$use_ee, {
      on <- isTRUE(input$use_ee)
      shinyjs::toggleState("ee_algo", condition = on)
      shinyjs::html("note_ee_algo", if (on) "fills the kcals columns" else "only while kcals is on")
    }, ignoreInit = FALSE)

    observeEvent(input$use_bouts, {
      on <- isTRUE(input$use_bouts)
      shinyjs::toggleState("bout_min", condition = on)
      shinyjs::toggleState("bout_rule", condition = on)
      shinyjs::html("note_bout_min", if (on) "a shorter run is not a bout" else "only while bouts are on")
      shinyjs::html("note_bout_rule", if (on) "how much interruption is allowed"
                    else "only while bouts are on")
    }, ignoreInit = FALSE)

    sel_fid <- reactive({
      s <- input$selected_participant %||% "all"
      if (identical(s, "all")) NULL else s
    })

    # Result frames shared by the table and the exports
    summary_df <- reactive({
      res <- results()
      if (length(res) == 0) return(NULL)
      tryCatch(act_summary_df(res, shared, input$bout_min), error = function(e) {
        message("act_summary_df failed: ", conditionMessage(e)); NULL
      })
    })

    daily_df <- reactive({
      res <- results()
      if (length(res) == 0) return(NULL)
      tryCatch(act_daily_df(res, shared, input$bout_min, sched_c()), error = function(e) {
        message("act_daily_df failed: ", conditionMessage(e)); NULL
      })
    })
    # The schedule views follow the schedule on screen; NULL until there is a
    # run to slice and a schedule to slice by
    window_daily_df <- reactive({
      res <- results(); s <- sched_c()
      if (length(res) == 0 || !sched_active(s)) return(NULL)
      tryCatch(act_window_daily_df(res, shared, s, input$bout_min), error = function(e) {
        message("act_window_daily_df failed: ", conditionMessage(e)); NULL
      })
    })
    daytype_df <- reactive(act_daytype_df(daily_df()))

    # The rule bar reads the run's parameters, not the live boxes
    run_params <- reactive({
      r <- results()
      if (length(r) == 0) NULL else r[[1]]$parameters
    })
    settings_moved <- reactive({
      settings_moved_from(run_params(), input, list(
        cut_points = "cut_points", data_type = "data_type", mets_algo = "mets_algo",
        ee_algo = "ee_algo", exclude_nonwear = "exclude_nonwear"))
    })

    output$rule <- renderUI({
      if (identical(view(), "raw")) {
        if (length(raw_ids()) == 0) return(raw_switch())
        tus <- timeuse()
        # Before the run the bar shows the defaults so the cut points can be set first
        if (length(tus) == 0) return(tagList(raw_switch(),
          acr_rule(NULL, NULL, ns, ran = FALSE, sched_text = sched_text(sched_r()))))
        return(tagList(raw_switch(),
          acr_rule(tus[[1]], active_window(), ns, sched_text = sched_text(raw_run_sched()),
                   stale = raw_sched_moved() || raw_settings_moved())))
      }
      # The Counts/Raw switch must be drawn in both branches, or Counts is a one-way door
      p <- run_params()
      cut <- ac_cut_label(p$cut_points %||% input$cut_points)
      counts <- if (identical(p$data_type %||% input$data_type %||% "axis1", "vm")) "vector magnitude" else "axis 1"
      epochs <- unique(vapply(shared$files, function(f) as.numeric(f$epoch_length %||% NA), numeric(1)))
      epochs <- epochs[!is.na(epochs)]
      epoch_txt <- if (length(epochs) == 1) paste0(fmt_int(epochs), " s epochs")
                   else if (length(epochs) > 1) "mixed epochs" else NULL


      wt <- shared$results$wear_time
      excluded <- if (is.null(p)) isTRUE(input$exclude_nonwear) else isTRUE(p$exclude_nonwear)
      days_txt <- if (!excluded) {
        tags$span("wear time filter ", tags$b("off"))
      } else if (length(wt) == 0) {
        tags$span(tags$b("wear time not run"), " · no day is excluded")
      } else {
        total <- sum(vapply(wt, function(w) as.numeric(w$total_days %||% 0), numeric(1)))
        valid <- sum(vapply(wt, function(w) as.numeric(w$valid_days %||% 0), numeric(1)))
        tags$span("wear time filter on · ", tags$b(fmt_int(valid)), " of ", fmt_int(total), " valid")
      }

      tagList(raw_switch(), tags$div(
        class = "ac-panel ac-rule",
        tags$span(class = "ac-rule-k", "Cut points"),
        tags$span(class = "ac-rule-v",
          tags$b(cut), " · ", counts,
          if (!is.null(epoch_txt)) paste0(" · ", epoch_txt) else NULL),
        tags$span(class = "ac-rule-sep", `aria-hidden` = "true"),
        tags$span(class = "ac-rule-k", "Days"),
        tags$span(class = "ac-rule-v", days_txt),
        tags$span(class = "ac-rule-sep", `aria-hidden` = "true"),
        tags$span(class = "ac-rule-k", "Schedule"),
        tags$span(class = "ac-rule-v", sched_text(sched_c())),
        if (settings_moved()) stale_note("ac") else NULL,
        tags$span(class = "ac-rule-actions",
          actionButton(ns("c_sched_open"), "Schedule", class = "ac-btn ac-btn--secondary"),
          actionButton(ns("toggle_advanced"), "Change", class = "ac-btn ac-btn--secondary"),
          tags$span(class = "ac-export-wrap",
            tags$span(class = paste("ac-btn ac-btn--secondary ac-export-btn",
                                    if (length(results()) == 0) "is-quiet" else ""),
                      tabindex = "0", role = "button", `aria-haspopup` = "menu", `aria-expanded` = "false",
                      "Export", tags$span(class = "ac-car", `aria-hidden` = "true", HTML("&#9660;"))),
            ac_export_menu(ns, length(results()) > 0, sched_active(sched_c()))),
          actionButton(ns("run_btn"),
                       if (length(results()) == 0) "Run analysis" else "Re-run",
                       class = paste(run_button_class("ac", length(results()) > 0, settings_moved()),
                                     "ac-run")))
      ))
    })

    # Figures: the mean of each recording's own daily mean, so a recording
    # with three days and one with eight weigh the same
    output$figures <- renderUI({
      if (identical(view(), "raw")) {
        tus <- timeuse()
        if (length(tus) == 0) return(NULL)
        return(acr_figures(tus, active_window()))
      }
      res <- results()
      if (length(res) == 0) return(NULL)
      sel <- sel_fid()
      scope <- if (is.null(sel)) res else res[names(res) == sel]
      if (length(scope) == 0) scope <- res

      daily_mean <- function(pick) {
        v <- vapply(scope, function(r) {
          d <- r$daily
          if (is.null(d)) return(NA_real_)
          mean(pick(d), na.rm = TRUE)
        }, numeric(1))
        v <- v[is.finite(v)]
        if (length(v) == 0) NA_real_ else mean(v)
      }
      col <- function(d, nm) if (nm %in% names(d)) d[[nm]] else rep(0, nrow(d))

      sed <- daily_mean(function(d) col(d, "sedentary_hrs"))
      light <- daily_mean(function(d) col(d, "light_hrs"))
      mvpa <- daily_mean(function(d)
        (col(d, "moderate_hrs") + col(d, "vigorous_hrs") + col(d, "very_vigorous_hrs")) * 60)

      step_ids <- if (is.null(sel)) names(res) else sel
      steps <- vapply(step_ids, function(fid) {
        f <- shared$files[[fid]]
        if (is.null(f) || !("steps" %in% names(f$data))) return(NA_real_)
        nd <- if ("timestamp" %in% names(f$data)) length(unique(as.Date(f$data$timestamp))) else 1
        if (nd == 0) return(NA_real_)
        sum(f$data$steps, na.rm = TRUE) / nd
      }, numeric(1))
      steps <- steps[is.finite(steps)]
      steps <- if (length(steps) == 0) NA_real_ else mean(steps)

      dash <- "–"
      n1 <- function(x, digits = 1) if (is.na(x)) dash else fmt_dec(x, digits)
      n0 <- function(x) if (is.na(x)) dash else fmt_int(round(x))

      tags$div(
        class = "ac-panel ac-figs",
        ac_fig(if (is.null(sel)) fmt_int(length(res)) else scope[[1]]$subject_id %||% "1",
               NULL, if (is.null(sel)) "Recordings" else "Recording"),
        ac_rule_div(),
        ac_fig(n1(sed), "h", "Sedentary a day"),
        ac_rule_div(),
        ac_fig(n1(light), "h", "Light a day"),
        ac_rule_div(),
        ac_fig(n0(mvpa), "min", "MVPA a day"),
        ac_rule_div(),
        ac_fig(n0(steps), NULL, "Steps a day")
      )
    })

    # Plot panel
    output$plot_panel <- renderUI({
      if (identical(view(), "raw")) {
        rs <- raw_list(); ids <- raw_ids(); tus <- timeuse()
        if (length(ids) == 0) return(NULL)
        if (length(tus) == 0) return(acr_not_run(rs, ids, local_raw$sel, ns))
        st <- tus[[raw_current()]]$status$state
        if (!identical(st, "ok")) return(acr_no_part5(tus[[raw_current()]], rs, ids, local_raw$sel, ns))
        return(acr_report_panel(rs, ids, tus, reports(), local_raw$sel, ns))
      }
      res <- results()
      plots <- ac_plots()
      keys <- vapply(plots, function(p) p$key, character(1))
      pick <- input$plot_pick %||% "intensity"
      if (!(pick %in% keys)) pick <- "intensity"
      p <- plots[[match(pick, keys)]]

      if (length(res) == 0) {
        return(tags$div(class = "ac-panel ac-plotpanel",
          tags$div(class = "ac-empty",
            tags$div(class = "ac-empty-t", "No activity results"),
            tags$div(class = "ac-empty-m",
                     "Run the analysis to classify every epoch and fill the table below."))))
      }

      sel <- sel_fid()
      label_of <- function(fid) res[[fid]]$subject_id %||% res[[fid]]$name %||% fid

      # The participant button and a clicked row set the same value
      who_label <- if (is.null(sel)) {
        paste("All", fmt_int(length(res)), if (length(res) == 1) "recording" else "recordings")
      } else label_of(sel)

      # A per_file plot with everybody chosen falls back to the first recording and says so
      scope <- if (isTRUE(p$per_file) && is.null(sel)) {
        paste0("showing ", label_of(names(res)[1]), " · this plot draws one recording at a time")
      } else ""

      # What each plot in the list would show for the current choice
      covers <- function(q) {
        if (!is.null(sel)) return(label_of(sel))
        if (isTRUE(q$per_file)) return(label_of(names(res)[1]))
        "all recordings"
      }

      tags$div(class = "ac-panel ac-plotpanel",
        tags$div(class = "ac-plotbar",
          tags$span(class = "ac-pick", tabindex = "0", role = "button",
                    `aria-haspopup` = "menu", `aria-expanded` = "false",
                    p$label, tags$span(class = "ac-car", `aria-hidden` = "true", HTML("&#9660;"))),
          tags$div(class = "ac-menu", role = "menu", style = "display: none;",
            lapply(plots, function(q)
              tags$div(class = paste("ac-mi", if (identical(q$key, p$key)) "is-on" else ""),
                       role = "menuitemradio", tabindex = "-1",
                       `aria-checked` = tolower(as.character(identical(q$key, p$key))),
                       `data-plot` = q$key,
                       tags$span(class = "ac-tick", `aria-hidden` = "true",
                                 if (identical(q$key, p$key)) HTML("&#10003;") else ""),
                       q$label,
                       tags$span(class = "ac-sc", covers(q))))),

          tags$span(class = "ac-who-wrap",
          tags$span(class = "ac-who", tabindex = "0", role = "button",
                    `aria-haspopup` = "menu", `aria-expanded` = "false",
                    who_label, tags$span(class = "ac-car", `aria-hidden` = "true", HTML("&#9660;"))),
          tags$div(class = "ac-whomenu", role = "menu", style = "display: none;",
            tags$div(class = paste("ac-wi", if (is.null(sel)) "is-on" else ""), `data-who` = "all",
                     role = "menuitemradio", tabindex = "-1",
                     `aria-checked` = tolower(as.character(is.null(sel))),
                     tags$span(class = "ac-tick", `aria-hidden` = "true",
                               if (is.null(sel)) HTML("&#10003;") else ""),
                     "All recordings",
                     if (isTRUE(p$per_file))
                       tags$span(class = "ac-sc", "falls back to the first") else NULL),
            tags$div(class = "ac-wi-rule", `aria-hidden` = "true"),
            lapply(names(res), function(fid)
              tags$div(class = paste("ac-wi", if (identical(fid, sel)) "is-on" else ""), `data-who` = fid,
                       role = "menuitemradio", tabindex = "-1",
                       `aria-checked` = tolower(as.character(identical(fid, sel))),
                       tags$span(class = "ac-tick", `aria-hidden` = "true",
                                 if (identical(fid, sel)) HTML("&#10003;") else ""),
                       label_of(fid),
                       tags$span(class = "ac-sc",
                                 paste(fmt_int(res[[fid]]$n_days %||% 0), "days")))))),

          if (nzchar(scope)) tags$span(class = "ac-scope", scope) else NULL),
        tags$div(class = "ac-plotwrap", plotOutput(ns(p$out), height = "367px")))
    })

    # Table panel
    output$table_panel <- renderUI({
      if (identical(view(), "raw")) {
        tus <- timeuse()
        if (length(tus) == 0) return(NULL)
        return(acr_table_panel(reports(), raw_ids(), local_raw$sel, local_raw$tab,
                               active_window(), ns, local_raw$sort, local_raw$dir,
                               sched = raw_run_sched()))
      }
      res <- results()
      if (length(res) == 0) return(NULL)
      sdf <- summary_df()
      if (is.null(sdf) || nrow(sdf) == 0) return(NULL)
      ddf <- daily_df()

      # Four views of the same run: Summary, Daily and the schedule's two
      tv <- table_view()
      if (!tv %in% c("summary", "daily", "window", "daytype")) tv <- "summary"
      s_run <- sched_c()
      # By window shows the per-day rows split into windows; the per-window
      # means are in the Windows export
      shown <- switch(tv, summary = sdf, daily = ddf, window = act_window_view(window_daily_df()),
                      daytype = daytype_df())
      sub <- if (is.null(shown) || nrow(shown) == 0) NULL else
        paste0(fmt_int(nrow(shown)),
               switch(tv,
                 summary = if (nrow(shown) == 1) " recording · " else " recordings · ",
                 daily = if (nrow(shown) == 1) " day · " else " days · ",
                 window = if (nrow(shown) == 1) " window day · " else " window days · ",
                 if (nrow(shown) == 1) " row · " else " rows · "),
               fmt_int(ncol(shown)), " columns")
      body <- switch(tv,
        summary = ac_summary_grid(sdf, ddf, names(res), sel_fid()),
        daily = ac_plain_grid(ddf, pin = 2L),
        window = if (!sched_active(s_run)) ac_sched_empty()
                 else ac_plain_grid(shown, pin = 2L) %||% ac_sched_empty(rows = TRUE),
        daytype = if (!sched_active(s_run)) ac_sched_empty()
                  else ac_plain_grid(shown, pin = 2L) %||% ac_sched_empty(rows = TRUE))

      tags$div(class = "ac-panel ac-tablepanel",
        tags$div(class = "ac-table-head",
          tags$span(class = "ac-table-title",
            tags$span(class = "ac-view",
              selectInput(ns("table_view"), NULL, width = "100%", selectize = FALSE,
                          choices = c("Summary" = "summary", "Daily" = "daily",
                                      "By window" = "window", "By day type" = "daytype"),
                          selected = tv)),
            if (!is.null(sub)) tags$span(class = "ac-table-sub", sub)),
          # the Summary is the view with column groups
          if (identical(tv, "summary")) {
            g <- ac_summary_groups(sdf)
            ac_goto(vapply(g, function(x) x[[1]], ""), vapply(g, function(x) length(x[[2]]), integer(1)))
          },
          if (identical(tv, "summary")) tags$span(class = "ac-key",
            tags$span("Each day"),
            tags$span(class = "ac-ramp",
              tags$b(class = "s0"), tags$b(class = "s1"), tags$b(class = "s2"),
              tags$b(class = "s3"), tags$b(class = "s4")),
            tags$span("0 to 450+ min MVPA"),
            tags$span(class = "ac-key-out",
                      tags$span(class = "ac-cell is-out"), "no valid wear"))),
        body)
    })

    # Helper: Format ETA
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    # Run Analysis
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      all_results <- list()
      n_files <- shared$file_count
      wt_results <- shared$results$wear_time
      use_wear_time <- input$exclude_nonwear && length(wt_results) > 0
      start_time <- Sys.time()

      withProgress(message = "Scoring physical activity...", value = 0, {
        for (i in seq_along(names(shared$files))) {
          fid <- names(shared$files)[i]
          f <- shared$files[[fid]]
          data <- f$data

          # Calculate ETA
          if (i > 1) {
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg_time <- elapsed / (i - 1)
            eta <- format_eta(avg_time * (n_files - i + 1))
            detail_msg <- paste0(f$subject_info$id, " (", i, "/", n_files, " | ETA: ", eta, ")")
          } else {
            detail_msg <- paste0(f$subject_info$id, " (", i, "/", n_files, ")")
          }
          setProgress(value = i / n_files, detail = detail_msg)

          epoch_length <- f$epoch_length

          # Start with wear time mask if available and enabled
          n_epochs <- nrow(data)
          analysis_mask <- rep(TRUE, n_epochs)
          if (use_wear_time && fid %in% names(wt_results)) {
            analysis_mask <- wt_results[[fid]]$wear

            # Apply DAY-LEVEL validation so on-screen tables/cards match CSV exports
            wear_result <- wt_results[[fid]]
            if (!is.null(wear_result$daily) && "timestamp" %in% names(data)) {
              daily_valid <- wear_result$daily
              data_dates <- as.Date(data$timestamp)
              for (d in seq_len(nrow(daily_valid))) {
                if (!isTRUE(daily_valid$valid[d])) {
                  day_date <- as.Date(daily_valid$date[d])
                  analysis_mask[data_dates == day_date] <- FALSE
                }
              }
            }
          }

          # Prepare data based on data type selection
          activity_data <- NULL
          counts <- NULL
          if (input$data_type == "axis1") {
            counts <- data$axis1
            activity_data <- canhrActi::to_cpm(counts, epoch_length)
          } else if (input$data_type == "vm") {
            # Calculate Vector Magnitude
            axis1 <- data$axis1
            axis2 <- if ("axis2" %in% names(data)) data$axis2 else rep(0, n_epochs)
            axis3 <- if ("axis3" %in% names(data)) data$axis3 else rep(0, n_epochs)
            counts <- sqrt(axis1^2 + axis2^2 + axis3^2)
            activity_data <- canhrActi::to_cpm(counts, epoch_length)
          }

          # Determine algorithm to use
          selected_algo <- input$cut_points

          # Apply mask to get valid data
          valid_data <- activity_data[analysis_mask]

          # Apply cut-points using unified function
          intensity <- tryCatch({
            canhrActi::apply_cutpoints(
              data = valid_data,
              algorithm = selected_algo
            )
          }, error = function(e) {
            showNotification(paste0("Could not score activity for ", f$name, " - check data format"), type = "error")
            return(NULL)
          })

          algo_used <- selected_algo

          if (is.null(intensity)) next
          intensity <- act_light_lifestyle(intensity)

          # MVPA bouts
          bouts <- NULL
          if (input$use_bouts) {
            bouts <- tryCatch({
              canhrActi::detect.mvpa.bouts(
                intensity = intensity,
                min_bout_length = as.numeric(input$bout_min %||% 10),
                use_80_percent_rule = (input$bout_rule == "80pct")
              )
            }, error = function(e) {
              NULL
            })
          }

          # Sedentary fragmentation analysis
          fragmentation <- NULL
          if ("timestamp" %in% names(data)) {
            full_intensity <- rep(NA_character_, length(counts))
            full_intensity[analysis_mask] <- as.character(intensity)
            full_intensity <- factor(full_intensity,
              levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))

            fragmentation <- tryCatch({
              canhrActi::sedentary.fragmentation(
                intensity = full_intensity,
                timestamps = data$timestamp,
                wear_time = if (use_wear_time && fid %in% names(wt_results)) wt_results[[fid]]$wear else NULL,
                epoch_length = epoch_length
              )
            }, error = function(e) {
              NULL
            })
          }

          # METs calculation
          mets <- NULL
          avg_mets <- NA
          if (input$use_mets) {
            mets <- tryCatch({
              subj_info <- list(
                mass = if (!is.null(f$subject_info$body_mass) && !is.na(f$subject_info$body_mass)) f$subject_info$body_mass else 70,
                age = if (!is.null(f$subject_info$age) && !is.na(f$subject_info$age)) f$subject_info$age else 35
              )
              canhrActi::calculate.mets(
                counts_data = data,
                algorithm = input$mets_algo,
                subject_info = subj_info,
                verbose = FALSE
              )
            }, error = function(e) {
              NULL
            })

            if (!is.null(mets)) {
              mets_valid <- mets[analysis_mask]
              avg_mets <- mean(mets_valid, na.rm = TRUE)
            }
          }

          # Energy expenditure; the per-epoch values let the tables keep worn epochs only
          total_ee <- NA
          kcal_epochs <- NULL
          if (input$use_ee) {
            ee <- tryCatch({
              mass <- if (!is.null(f$subject_info$body_mass) && !is.na(f$subject_info$body_mass)) {
                f$subject_info$body_mass
              } else 70
              canhrActi::calculate.energy.expenditure.direct(
                counts_data = data, body_mass = mass,
                algorithm = input$ee_algo, epoch_length = f$epoch_length
              )
            }, error = function(e) {
              NULL
            })
            if (!is.null(ee)) {
              total_ee <- ee$total_kcal
              kcal_epochs <- ee$kcal_per_epoch
            }
          }

          # Basic epoch counts (for reference)
          int_table <- table(intensity)
          n_valid_epochs <- sum(analysis_mask)

          # Initialize - will be calculated from daily data below
          n_days <- 0
          sedentary_min <- 0
          light_min <- 0
          moderate_min <- 0
          vigorous_min <- 0
          very_vigorous_min <- 0
          mvpa_min <- 0

          # Hourly pattern
          hourly <- NULL
          if ("timestamp" %in% names(data)) {
            temp <- data.frame(hour = as.numeric(format(data$timestamp, "%H")), counts = counts, mask = analysis_mask)
            temp <- temp[temp$mask, ]
            if (nrow(temp) > 0) hourly <- aggregate(counts ~ hour, temp, mean, na.rm = TRUE)
          }

          # Daily summary
          daily <- NULL
          if ("timestamp" %in% names(data)) {
            temp <- data
            temp$analyzed <- analysis_mask
            temp$date <- as.Date(temp$timestamp)
            temp$intensity <- NA
            temp$intensity[analysis_mask] <- as.character(intensity)

            daily <- aggregate(analyzed ~ date, temp, sum)
            daily$analyzed_hours <- daily$analyzed * f$epoch_length / 3600

            for (d in unique(temp$date)) {
              day_data <- temp[temp$date == d & temp$analyzed, ]
              if (nrow(day_data) > 0) {
                day_int <- table(day_data$intensity)
                daily[daily$date == d, "sedentary"] <- if ("sedentary" %in% names(day_int)) day_int["sedentary"] else 0
                daily[daily$date == d, "light"] <- if ("light" %in% names(day_int)) day_int["light"] else 0
                daily[daily$date == d, "moderate"] <- if ("moderate" %in% names(day_int)) day_int["moderate"] else 0
                daily[daily$date == d, "vigorous"] <- if ("vigorous" %in% names(day_int)) day_int["vigorous"] else 0
                daily[daily$date == d, "very_vigorous"] <- if ("very_vigorous" %in% names(day_int)) day_int["very_vigorous"] else 0
              }
            }

            # Convert epoch counts to hours for each day
            epoch_hrs <- f$epoch_length / 3600
            daily$sedentary_hrs <- daily$sedentary * epoch_hrs
            daily$light_hrs <- daily$light * epoch_hrs
            daily$moderate_hrs <- daily$moderate * epoch_hrs
            daily$vigorous_hrs <- daily$vigorous * epoch_hrs
            daily$very_vigorous_hrs <- daily$very_vigorous * epoch_hrs
          }

          # Calculate summary stats FROM daily data (source of truth)
          if (!is.null(daily) && nrow(daily) > 0) {
            n_days <- nrow(daily)
            sedentary_min <- mean(daily$sedentary_hrs, na.rm = TRUE) * 60
            light_min <- mean(daily$light_hrs, na.rm = TRUE) * 60
            moderate_min <- mean(daily$moderate_hrs, na.rm = TRUE) * 60
            vigorous_min <- mean(daily$vigorous_hrs, na.rm = TRUE) * 60
            very_vigorous_min <- mean(daily$very_vigorous_hrs, na.rm = TRUE) * 60
            mvpa_min <- moderate_min + vigorous_min + very_vigorous_min
          }

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            epoch_length = f$epoch_length,
            wear_time_applied = use_wear_time && fid %in% names(wt_results),
            intensity_valid = intensity,
            activity_data = activity_data,
            bouts = bouts,
            mets = mets,
            avg_mets = avg_mets,
            total_ee = total_ee,
            kcal_epochs = kcal_epochs,
            hourly = hourly,
            daily = daily,
            fragmentation = fragmentation,
            n_valid_epochs = n_valid_epochs,
            n_days = n_days,
            sedentary_min = as.numeric(sedentary_min),
            light_min = as.numeric(light_min),
            moderate_min = as.numeric(moderate_min),
            vigorous_min = as.numeric(vigorous_min),
            very_vigorous_min = as.numeric(very_vigorous_min),
            mvpa_min = as.numeric(mvpa_min),
            n_bouts = if (!is.null(bouts)) nrow(bouts) else 0,
            parameters = list(
              cut_points = algo_used,
              data_type = input$data_type,
              mets_algo = input$mets_algo,
              ee_algo = input$ee_algo,
              exclude_nonwear = input$exclude_nonwear
            )
          )
        }

        gc(verbose = FALSE)
      })

      results(all_results)
      run_stamp(Sys.time())
      shared$results$activity <- all_results

      # Store sedentary analysis parameters and detect bouts for use by Sedentary Fragmentation tab
      sed_params <- list(
        threshold = as.numeric(input$sed_threshold %||% 200),
        drop_time = as.numeric(input$sed_drop_time %||% 2),
        min_length = as.numeric(input$sed_min_length %||% 10),
        use_vm = input$sed_use_vm %||% TRUE,
        ignore_first_break = input$sed_ignore_first %||% FALSE
      )

      # Detect sedentary bouts for each file using configured parameters
      sed_bouts_all <- list()
      for (fid in names(all_results)) {
        r <- all_results[[fid]]
        f <- shared$files[[fid]]
        data <- f$data
        epoch_sec <- f$epoch_length

        # Get counts based on VM setting
        if (sed_params$use_vm && all(c("axis1", "axis2", "axis3") %in% names(data))) {
          counts <- sqrt(data$axis1^2 + data$axis2^2 + data$axis3^2)
        } else {
          counts <- data$axis1
        }

        cpm <- counts * (60 / epoch_sec)

        # Get wear time mask
        wear_mask <- if (!is.null(shared$results$wear_time[[fid]])) {
          shared$results$wear_time[[fid]]$wear
        } else {
          rep(TRUE, nrow(data))
        }

        # Sedentary detection
        is_sed <- (cpm < sed_params$threshold) & wear_mask

        # Cumulative drop time algorithm
        drop_epochs <- sed_params$drop_time * (60 / epoch_sec)
        bout_starts <- c()
        bout_ends <- c()
        in_bout <- FALSE
        bout_start <- NA
        cumulative_activity <- 0
        last_sed_idx <- NA

        for (i in seq_along(is_sed)) {
          if (is_sed[i]) {
            if (!in_bout) {
              in_bout <- TRUE
              bout_start <- i
              cumulative_activity <- 0
            }
            last_sed_idx <- i
          } else {
            if (in_bout) {
              cumulative_activity <- cumulative_activity + 1
              if (cumulative_activity > drop_epochs) {
                if (!is.na(last_sed_idx)) {
                  bout_starts <- c(bout_starts, bout_start)
                  bout_ends <- c(bout_ends, last_sed_idx)
                }
                in_bout <- FALSE
                bout_start <- NA
                cumulative_activity <- 0
                last_sed_idx <- NA
              }
            }
          }
        }

        if (in_bout && !is.na(last_sed_idx)) {
          bout_starts <- c(bout_starts, bout_start)
          bout_ends <- c(bout_ends, last_sed_idx)
        }

        if (length(bout_starts) > 0) {
          duration_min <- (bout_ends - bout_starts + 1) * (epoch_sec / 60)
          valid_bouts <- duration_min >= sed_params$min_length

          if (sum(valid_bouts) > 0) {
            sed_bouts_all[[fid]] <- data.frame(
              start_idx = bout_starts[valid_bouts],
              end_idx = bout_ends[valid_bouts],
              start_time = data$timestamp[bout_starts[valid_bouts]],
              end_time = data$timestamp[bout_ends[valid_bouts]] + epoch_sec,
              duration_min = duration_min[valid_bouts],
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Store in shared state for Sedentary Fragmentation tab
      shared$results$sedentary_bouts <- list(
        parameters = sed_params,
        bouts = sed_bouts_all,
        timestamp = Sys.time()
      )
    })

    # Clear results
    observeEvent(input$clear_results, {
      results(list())
      run_stamp(NULL)
      shared$results$activity <- NULL
      shared$results$sedentary_bouts <- NULL
    })

    # HERO CHART: Intensity plot (larger, more prominent)
    output$intensity_plot <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      res <- results()
      sel <- input$selected_participant
      validate(need(length(res) > 0, "No data yet"))
      if (!is.null(sel) && sel != "all" && sel %in% names(res)) res <- list(res[[sel]])
      
      # Hours per intensity, summed over the daily frames
      canhrActi::plot_intensity_hours(lapply(res, function(r) r$daily))
      })
    }, bg = "white")

    # Hourly pattern plot
    output$hourly_plot <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      res <- results()
      sel <- input$selected_participant
      validate(need(length(res) > 0, "No hourly data"))
      if (!is.null(sel) && sel != "all" && sel %in% names(res)) res <- list(res[[sel]])
      
      canhrActi::plot_hourly_pattern(lapply(res, function(r) r$hourly))
      })
    }, bg = "white")

    # Export handlers
    output$export_summary <- downloadHandler(
      filename = function() {
        export_name("Summary")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        df <- act_summary_df(res, shared, input$bout_min)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    output$export_daily <- downloadHandler(
      filename = function() {
        export_name("Daily")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        df <- act_daily_df(res, shared, input$bout_min, sched_c())
        if (is.null(df)) {
          write.csv(data.frame(Message = "No daily data to export"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    # Schedule exports: one row per recording, day and window, and one row per
    # recording and kind of day; only when the run had a schedule
    output$export_windows <- downloadHandler(
      filename = function() export_name("Windows"),
      content = function(file) {
        df <- window_daily_df()
        if (is.null(df)) {
          write.csv(data.frame(Message = "No window rows to export"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )
    output$export_daytypes <- downloadHandler(
      filename = function() export_name("DayTypes"),
      content = function(file) {
        df <- daytype_df()
        if (is.null(df)) {
          write.csv(data.frame(Message = "No day type rows to export"), file, row.names = FALSE)
          return()
        }
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    output$export_hourly <- downloadHandler(
      filename = function() {
        export_name("Hourly")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        all_rows <- list()
        for (r in res) {
          f <- shared$files[[r$file_id]]
          data <- f$data
          epoch_sec <- f$epoch_length

          # Subject info
          weight <- f$subject_info$weight_lbs %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% ""

          # Get algorithm name for bout column headers
          algo <- r$parameters$cut_points %||% "freedson"
          algo_display <- switch(algo,
            "freedson" = "Freedson (1998)",
            "troiano" = "Troiano NHANES (2008)",
            "evenson" = "Evenson (2008)",
            "matthews" = "Matthews (2005)",
            "copeland_older" = "Copeland (2009)",
            "sasaki_vm3" = "Freedson VM3 (Sasaki 2011)",
            "freedson_vm3" = "Freedson VM3 (Sasaki 2011)",
            "Freedson (1998)"
          )

          # Get wear time mask AND daily validation
          wear_result <- shared$results$wear_time[[r$file_id]]
          wear_mask <- if (!is.null(wear_result) && !is.null(wear_result$wear)) {
            wear_result$wear
          } else {
            rep(TRUE, nrow(data))  # Default: assume all worn if no wear time analysis
          }

          # Get daily validation info (which days meet minimum wear time criteria)
          daily_valid <- NULL
          if (!is.null(wear_result) && !is.null(wear_result$daily)) {
            daily_valid <- wear_result$daily
          }

          if ("timestamp" %in% names(data)) {
            data$date <- as.Date(data$timestamp)
            data$hour_24 <- as.numeric(format(data$timestamp, "%H"))

            # Apply DAY-LEVEL validation
            # If a day doesn't meet minimum wear criteria, set ALL its epochs to non-wear
            if (!is.null(daily_valid)) {
              for (d in seq_len(nrow(daily_valid))) {
                if (!daily_valid$valid[d]) {
                  day_date <- as.Date(daily_valid$date[d])
                  wear_mask[data$date == day_date] <- FALSE
                }
              }
            }

            # Pre-calculate intensity for all data
            axis1 <- data$axis1
            axis2 <- if ("axis2" %in% names(data)) data$axis2 else rep(0, nrow(data))
            axis3 <- if ("axis3" %in% names(data)) data$axis3 else rep(0, nrow(data))
            steps <- if ("steps" %in% names(data)) data$steps else rep(0, nrow(data))
            lux <- if ("lux" %in% names(data)) data$lux else rep(0, nrow(data))

            vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

            # The per-minute series the run classified, axis 1 or vector
            # magnitude, with non-wear set to NA before classification
            all_cpm <- r$activity_data
            if (length(all_cpm) != nrow(data)) all_cpm <- canhrActi::to_cpm(axis1, epoch_sec)
            all_cpm[!wear_mask] <- NA
            all_intensity <- tryCatch({
              act_light_lifestyle(canhrActi::apply_cutpoints(all_cpm, algo))
            }, error = function(e) rep(NA_character_, nrow(data)))
            # Explicitly set non-wear to NA
            all_intensity[!wear_mask] <- NA

            # Detect MVPA bouts for the full dataset
            is_mvpa <- all_intensity %in% c("moderate", "vigorous", "very_vigorous")
            mvpa_bouts <- rle(is_mvpa)
            bout_starts <- cumsum(c(1, head(mvpa_bouts$lengths, -1)))
            bout_ends <- cumsum(mvpa_bouts$lengths)

            # Create bout info data frame
            bout_info <- data.frame(
              start = bout_starts[mvpa_bouts$values],
              end = bout_ends[mvpa_bouts$values],
              length = mvpa_bouts$lengths[mvpa_bouts$values]
            )
            # Filter to bouts >= 10 epochs (or configured minimum)
            bout_min_epochs <- as.numeric(input$bout_min %||% 10) * (60 / epoch_sec)
            bout_info <- bout_info[bout_info$length >= bout_min_epochs, ]

            # Detect sedentary bouts
            # Handle NA values from non-wear epochs
            is_sed <- !is.na(all_intensity) & all_intensity == "sedentary"
            sed_bouts_rle <- rle(is_sed)
            sed_bout_starts <- cumsum(c(1, head(sed_bouts_rle$lengths, -1)))
            sed_bout_ends <- cumsum(sed_bouts_rle$lengths)
            sed_valid <- which(sed_bouts_rle$values == TRUE)
            sed_bout_info <- if (length(sed_valid) > 0) {
              data.frame(
                start = sed_bout_starts[sed_valid],
                end = sed_bout_ends[sed_valid],
                length = sed_bouts_rle$lengths[sed_valid]
              )
            } else {
              data.frame(start = integer(0), end = integer(0), length = integer(0))
            }

            # Detect sedentary breaks (non-sedentary WEAR TIME periods)
            is_break <- !is.na(all_intensity) & all_intensity != "sedentary"
            break_bouts_rle <- rle(is_break)
            break_bout_starts <- cumsum(c(1, head(break_bouts_rle$lengths, -1)))
            break_bout_ends <- cumsum(break_bouts_rle$lengths)
            break_valid <- which(break_bouts_rle$values == TRUE)
            break_bout_info <- if (length(break_valid) > 0) {
              data.frame(
                start = break_bout_starts[break_valid],
                end = break_bout_ends[break_valid],
                length = break_bouts_rle$lengths[break_valid]
              )
            } else {
              data.frame(start = integer(0), end = integer(0), length = integer(0))
            }

            dates <- unique(data$date)
            n_calendar_days <- length(dates)

            for (date_i in dates) {
              day_data <- data[data$date == date_i, ]
              day_indices <- which(data$date == date_i)
              hours_present <- unique(day_data$hour_24)

              for (hour_i in hours_present) {
                hour_mask <- day_data$hour_24 == hour_i
                hour_data <- day_data[hour_mask, ]
                hour_indices <- day_indices[hour_mask]
                n_epochs <- nrow(hour_data)
                if (n_epochs == 0) next

                # Get wear time for this specific hour
                hour_wear <- wear_mask[hour_indices]
                n_wear_epochs <- sum(hour_wear, na.rm = TRUE)

                # Get hour data
                h_axis1 <- hour_data$axis1
                h_axis2 <- if ("axis2" %in% names(hour_data)) hour_data$axis2 else rep(0, n_epochs)
                h_axis3 <- if ("axis3" %in% names(hour_data)) hour_data$axis3 else rep(0, n_epochs)
                h_steps <- if ("steps" %in% names(hour_data)) hour_data$steps else rep(0, n_epochs)
                h_lux <- if ("lux" %in% names(hour_data)) hour_data$lux else rep(0, n_epochs)
                h_vm <- sqrt(h_axis1^2 + h_axis2^2 + h_axis3^2)

                # Get intensity for this hour (NA for non-wear)
                hour_intensity <- all_intensity[hour_indices]

                # Intensity counts - only wear time epochs (NA excluded)
                sedentary <- sum(hour_intensity == "sedentary", na.rm = TRUE)
                light <- sum(hour_intensity == "light", na.rm = TRUE)
                moderate <- sum(hour_intensity == "moderate", na.rm = TRUE)
                vigorous <- sum(hour_intensity == "vigorous", na.rm = TRUE)
                very_vigorous <- sum(hour_intensity == "very_vigorous", na.rm = TRUE)
                total_mvpa <- moderate + vigorous + very_vigorous

                # Percentages based on WEAR TIME epochs only
                # Hours with 0 wear time get 0% for all categories
                pct_sed <- if (n_wear_epochs > 0) 100 * sedentary / n_wear_epochs else 0
                pct_light <- if (n_wear_epochs > 0) 100 * light / n_wear_epochs else 0
                pct_mod <- if (n_wear_epochs > 0) 100 * moderate / n_wear_epochs else 0
                pct_vig <- if (n_wear_epochs > 0) 100 * vigorous / n_wear_epochs else 0
                pct_vvig <- if (n_wear_epochs > 0) 100 * very_vigorous / n_wear_epochs else 0
                pct_mvpa <- if (n_wear_epochs > 0) 100 * total_mvpa / n_wear_epochs else 0

                # MVPA Bout metrics for this hour
                hour_start <- min(hour_indices)
                hour_end <- max(hour_indices)

                # Bouts occurring in this hour (any overlap)
                bouts_occurring <- if (nrow(bout_info) > 0) {
                  bout_info[bout_info$start <= hour_end & bout_info$end >= hour_start, ]
                } else data.frame()
                n_bouts_occurring <- nrow(bouts_occurring)

                # Bouts starting in this hour
                bouts_starting <- if (nrow(bout_info) > 0) {
                  bout_info[bout_info$start >= hour_start & bout_info$start <= hour_end, ]
                } else data.frame()
                n_bouts_starting <- nrow(bouts_starting)

                # Bouts ending in this hour
                bouts_ending <- if (nrow(bout_info) > 0) {
                  bout_info[bout_info$end >= hour_start & bout_info$end <= hour_end, ]
                } else data.frame()
                n_bouts_ending <- nrow(bouts_ending)

                # Total time of bouts in this hour (epochs that overlap with this hour)
                total_bout_time <- 0
                total_bout_counts <- 0
                if (nrow(bouts_occurring) > 0) {
                  for (b in seq_len(nrow(bouts_occurring))) {
                    b_start <- max(bouts_occurring$start[b], hour_start)
                    b_end <- min(bouts_occurring$end[b], hour_end)
                    total_bout_time <- total_bout_time + (b_end - b_start + 1)
                    total_bout_counts <- total_bout_counts + sum(axis1[b_start:b_end], na.rm = TRUE)
                  }
                }

                # Sedentary bout metrics for this hour
                sed_bouts_occurring <- if (nrow(sed_bout_info) > 0) {
                  sed_bout_info[sed_bout_info$start <= hour_end & sed_bout_info$end >= hour_start, ]
                } else data.frame()
                n_sed_bouts_occurring <- nrow(sed_bouts_occurring)

                sed_bouts_starting <- if (nrow(sed_bout_info) > 0) {
                  sed_bout_info[sed_bout_info$start >= hour_start & sed_bout_info$start <= hour_end, ]
                } else data.frame()
                n_sed_bouts_starting <- nrow(sed_bouts_starting)

                sed_bouts_ending <- if (nrow(sed_bout_info) > 0) {
                  sed_bout_info[sed_bout_info$end >= hour_start & sed_bout_info$end <= hour_end, ]
                } else data.frame()
                n_sed_bouts_ending <- nrow(sed_bouts_ending)

                # Total time of sedentary bouts in this hour
                total_sed_bout_time <- 0
                if (nrow(sed_bouts_occurring) > 0) {
                  for (b in seq_len(nrow(sed_bouts_occurring))) {
                    b_start <- max(sed_bouts_occurring$start[b], hour_start)
                    b_end <- min(sed_bouts_occurring$end[b], hour_end)
                    total_sed_bout_time <- total_sed_bout_time + (b_end - b_start + 1)
                  }
                }

                # Sedentary break metrics for this hour
                break_bouts_occurring <- if (nrow(break_bout_info) > 0) {
                  break_bout_info[break_bout_info$start <= hour_end & break_bout_info$end >= hour_start, ]
                } else data.frame()
                n_break_bouts_occurring <- nrow(break_bouts_occurring)

                break_bouts_starting <- if (nrow(break_bout_info) > 0) {
                  break_bout_info[break_bout_info$start >= hour_start & break_bout_info$start <= hour_end, ]
                } else data.frame()
                n_break_bouts_starting <- nrow(break_bouts_starting)

                break_bouts_ending <- if (nrow(break_bout_info) > 0) {
                  break_bout_info[break_bout_info$end >= hour_start & break_bout_info$end <= hour_end, ]
                } else data.frame()
                n_break_bouts_ending <- nrow(break_bouts_ending)

                # Total time of breaks in this hour
                total_break_time <- 0
                if (nrow(break_bouts_occurring) > 0) {
                  for (b in seq_len(nrow(break_bouts_occurring))) {
                    b_start <- max(break_bouts_occurring$start[b], hour_start)
                    b_end <- min(break_bouts_occurring$end[b], hour_end)
                    total_break_time <- total_break_time + (b_end - b_start + 1)
                  }
                }

                # Only use WEAR TIME epochs for all count metrics                # Non-wear hours should show 0 for all metrics
                if (n_wear_epochs > 0) {
                  # Filter data by wear mask for this hour
                  w_axis1 <- h_axis1[hour_wear]
                  w_axis2 <- h_axis2[hour_wear]
                  w_axis3 <- h_axis3[hour_wear]
                  w_steps <- h_steps[hour_wear]
                  w_lux <- h_lux[hour_wear]
                  w_vm <- h_vm[hour_wear]

                  # Axis counts (total, average, max, CPM) - wear time only
                  axis1_counts <- sum(w_axis1, na.rm = TRUE)
                  axis2_counts <- sum(w_axis2, na.rm = TRUE)
                  axis3_counts <- sum(w_axis3, na.rm = TRUE)

                  axis1_avg <- mean(w_axis1, na.rm = TRUE)
                  axis2_avg <- mean(w_axis2, na.rm = TRUE)
                  axis3_avg <- mean(w_axis3, na.rm = TRUE)

                  axis1_max <- max(w_axis1, na.rm = TRUE)
                  axis2_max <- max(w_axis2, na.rm = TRUE)
                  axis3_max <- max(w_axis3, na.rm = TRUE)

                  # CPM = counts per minute = average counts * (60 / epoch_sec)
                  axis1_cpm <- axis1_avg * (60 / epoch_sec)
                  axis2_cpm <- axis2_avg * (60 / epoch_sec)
                  axis3_cpm <- axis3_avg * (60 / epoch_sec)

                  # Vector magnitude - wear time only
                  vm_counts <- sum(w_vm, na.rm = TRUE)
                  vm_avg <- mean(w_vm, na.rm = TRUE)
                  vm_max <- max(w_vm, na.rm = TRUE)
                  vm_cpm <- vm_avg * (60 / epoch_sec)

                  # Steps - wear time only
                  steps_counts <- sum(w_steps, na.rm = TRUE)
                  steps_avg <- mean(w_steps, na.rm = TRUE)
                  steps_max <- max(w_steps, na.rm = TRUE)
                  steps_per_min <- steps_avg * (60 / epoch_sec)

                  # Lux - wear time only
                  lux_avg <- mean(w_lux, na.rm = TRUE)
                  lux_max <- max(w_lux, na.rm = TRUE)
                } else {
                  # Non-wear hour - all metrics are 0
                  axis1_counts <- axis2_counts <- axis3_counts <- 0
                  axis1_avg <- axis2_avg <- axis3_avg <- 0
                  axis1_max <- axis2_max <- axis3_max <- 0
                  axis1_cpm <- axis2_cpm <- axis3_cpm <- 0
                  vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
                  steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
                  lux_avg <- lux_max <- 0
                }

                # Energy expenditure for this hour - wear time only
                kcals <- 0
                mets_avg <- 1
                if (n_wear_epochs > 0 && !is.null(r$mets) && length(r$mets) >= max(hour_indices)) {
                  hour_mets <- r$mets[hour_indices]
                  # Only use wear time METs
                  wear_mets <- hour_mets[hour_wear]
                  mets_avg <- mean(wear_mets, na.rm = TRUE)
                  # Approximate kcals from METs: kcal = METs * weight_kg * time_hours
                  weight_kg <- weight * 0.453592
                  time_hours <- n_wear_epochs * epoch_sec / 3600
                  kcals <- mets_avg * weight_kg * time_hours
                } else if (n_wear_epochs == 0) {
                  # Non-wear hour - no energy expenditure
                  mets_avg <- 1  # Default MET value
                  kcals <- 0
                }

                # Day of week
                dow <- fmt_date(as.Date(date_i), "%A")
                dow_num <- as.numeric(format(as.Date(date_i), "%u"))  # 1=Monday, 7=Sunday

                # Time in minutes for this hour - based on wear time epochs                # For non-wear hours, time is 0
                time_min <- if (n_wear_epochs > 0) n_wear_epochs * epoch_sec / 60 else 0
                # Number of epochs reported is wear time epochs (or 0 for non-wear)
                n_epochs_output <- if (n_wear_epochs > 0) n_wear_epochs else 0

                row_data <- data.frame(
                  Subject = r$subject_id,
                  Filename = r$name,
                  Epoch = epoch_sec,
                  `Weight (lbs)` = weight,
                  Age = age,
                  Gender = gender,
                  Date = format(as.Date(date_i), "%m/%d/%Y"),
                  Hour = sprintf("%d:00 %s", ifelse(hour_i == 0, 12, ifelse(hour_i > 12, hour_i - 12, hour_i)),
                                 ifelse(hour_i < 12, "AM", "PM")),
                  `Day of Week` = dow,
                  `Day of Week Num` = dow_num,
                  kcals = round(kcals, 3),
                  METs = round(mets_avg, 3),
                  # MVPA Bout columns
                  `Number of MVPA Bouts occurring in this hour` = n_bouts_occurring,
                  `Number of MVPA Bouts starting in this hour` = n_bouts_starting,
                  `Number of MVPA Bouts ending in this hour` = n_bouts_ending,
                  `Total time of MVPA Bouts occurring in this hour` = act_min(total_bout_time, epoch_sec),
                  `Total activity counts of MVPA Bouts occurring in this hour` = total_bout_counts,
                  # Sedentary Bout columns
                  `Number of Sedentary Bouts occurring in this hour` = n_sed_bouts_occurring,
                  `Number of Sedentary Bouts starting in this hour` = n_sed_bouts_starting,
                  `Number of Sedentary Bouts ending in this hour` = n_sed_bouts_ending,
                  `Total time of Sedentary Bouts occurring in this hour` = act_min(total_sed_bout_time, epoch_sec),
                  # Sedentary Break columns
                  `Number of Sedentary Breaks occurring in this hour` = n_break_bouts_occurring,
                  `Number of Sedentary Breaks starting in this hour` = n_break_bouts_starting,
                  `Number of Sedentary Breaks ending in this hour` = n_break_bouts_ending,
                  `Total time of Sedentary Breaks occurring in this hour` = act_min(total_break_time, epoch_sec),
                  # Intensity minutes
                  Sedentary = act_min(sedentary, epoch_sec),
                  Light = act_min(light, epoch_sec),
                  Moderate = act_min(moderate, epoch_sec),
                  Vigorous = act_min(vigorous, epoch_sec),
                  `Very Vigorous` = act_min(very_vigorous, epoch_sec),
                  # Percentages
                  `% in Sedentary` = sprintf("%.2f%%", pct_sed),
                  `% in Light` = sprintf("%.2f%%", pct_light),
                  `% in Moderate` = sprintf("%.2f%%", pct_mod),
                  `% in Vigorous` = sprintf("%.2f%%", pct_vig),
                  `% in Very Vigorous` = sprintf("%.2f%%", pct_vvig),
                  `Total MVPA` = act_min(total_mvpa, epoch_sec),
                  `% in MVPA` = sprintf("%.2f%%", pct_mvpa),
                  # Axis counts
                  `Axis 1 Counts` = axis1_counts,
                  `Axis 2 Counts` = axis2_counts,
                  `Axis 3 Counts` = axis3_counts,
                  `Axis 1 Average Counts` = round(axis1_avg, 1),
                  `Axis 2 Average Counts` = round(axis2_avg, 1),
                  `Axis 3 Average Counts` = round(axis3_avg, 1),
                  `Axis 1 Max Counts` = axis1_max,
                  `Axis 2 Max Counts` = axis2_max,
                  `Axis 3 Max Counts` = axis3_max,
                  `Axis 1 CPM` = round(axis1_cpm, 1),
                  `Axis 2 CPM` = round(axis2_cpm, 1),
                  `Axis 3 CPM` = round(axis3_cpm, 1),
                  # Vector Magnitude
                  `Vector Magnitude Counts` = round(vm_counts, 1),
                  `Vector Magnitude Average Counts` = round(vm_avg, 1),
                  `Vector Magnitude Max Counts` = round(vm_max, 1),
                  `Vector Magnitude CPM` = round(vm_cpm, 1),
                  # Steps
                  `Steps Counts` = steps_counts,
                  `Steps Average Counts` = round(steps_avg, 1),
                  `Steps Max Counts` = steps_max,
                  `Steps Per Minute` = round(steps_per_min, 1),
                  # Lux
                  `Lux Average Counts` = round(lux_avg, 1),
                  `Lux Max Counts` = lux_max,
                  # Metadata
                  `Number of Epochs` = n_epochs_output,
                  Time = act_min(n_epochs_output, epoch_sec),
                  `Calendar Days` = n_calendar_days,
                  check.names = FALSE,
                  stringsAsFactors = FALSE
                )
                all_rows[[length(all_rows) + 1]] <- row_data
              }
            }
          }
        }

        if (length(all_rows) == 0) {
          write.csv(data.frame(Message = "No hourly data to export"), file, row.names = FALSE)
          return()
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    # Sedentary Bout Export
    output$export_sedentary <- downloadHandler(
      filename = function() {
        export_name("SedentaryAnalysis")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export. Run Activity Analysis first."), file, row.names = FALSE)
          return()
        }

        all_bouts <- list()

        # Sedentary Analysis parameters (from Advanced Options)
        # Use as.numeric() to handle raw HTML inputs that may return strings
        min_bout_minutes <- as.numeric(input$sed_min_length %||% 10)
        sedentary_threshold <- as.numeric(input$sed_threshold %||% 200)
        drop_time_minutes <- as.numeric(input$sed_drop_time %||% 2)
        use_vector_magnitude <- input$sed_use_vm %||% TRUE
        ignore_first_break <- input$sed_ignore_first %||% FALSE

        for (r in res) {
          f <- shared$files[[r$file_id]]
          data <- f$data
          epoch_sec <- f$epoch_length

          # Subject info
          subject_id <- r$subject_id
          weight_lbs <- f$subject_info$weight_lbs %||% 0
          age <- f$subject_info$age %||% 0
          gender_raw <- f$subject_info$sex %||% ""
          gender <- if (tolower(gender_raw) %in% c("female", "f")) "F" else if (tolower(gender_raw) %in% c("male", "m")) "M" else gender_raw

          # Get wear time mask from wear time analysis
          wear_result <- shared$results$wear_time[[r$file_id]]
          wear_mask <- if (!is.null(wear_result) && !is.null(wear_result$wear)) {
            wear_result$wear
          } else {
            rep(TRUE, nrow(data))
          }

          # Apply DAY-LEVEL validation (match other exports / on-screen tables)
          if (!is.null(wear_result) && !is.null(wear_result$daily) && "timestamp" %in% names(data)) {
            daily_valid <- wear_result$daily
            data_dates <- as.Date(data$timestamp)
            for (d in seq_len(nrow(daily_valid))) {
              if (!daily_valid$valid[d]) {
                day_date <- as.Date(daily_valid$date[d])
                wear_mask[data_dates == day_date] <- FALSE
              }
            }
          }

          # Use Vector Magnitude for sedentary detection
          if (use_vector_magnitude && "vector_magnitude" %in% names(data)) {
            counts <- data$vector_magnitude
          } else if (use_vector_magnitude && all(c("axis1", "axis2", "axis3") %in% names(data))) {
            counts <- sqrt(data$axis1^2 + data$axis2^2 + data$axis3^2)
          } else {
            counts <- data$axis1
          }

          # Calculate CPM (for 60-sec epochs, CPM = counts)
          cpm <- counts * (60 / epoch_sec)

          # Sedentary detection: CPM < threshold AND valid wear time
          is_sed <- (cpm < sedentary_threshold) & wear_mask

          # Cumulative drop time algorithm
          drop_epochs <- drop_time_minutes * (60 / epoch_sec)

          bout_starts <- c()
          bout_ends <- c()

          in_bout <- FALSE
          bout_start <- NA
          cumulative_activity <- 0
          last_sed_idx <- NA

          for (i in seq_along(is_sed)) {
            if (is_sed[i]) {  # sedentary epoch
              if (!in_bout) {
                # Start new bout
                in_bout <- TRUE
                bout_start <- i
                cumulative_activity <- 0
              }
              last_sed_idx <- i
            } else {  # activity epoch
              if (in_bout) {
                cumulative_activity <- cumulative_activity + 1
                if (cumulative_activity > drop_epochs) {
                  # End bout - cumulative activity exceeded drop time
                  # Bout ends at last sedentary epoch
                  if (!is.na(last_sed_idx)) {
                    bout_starts <- c(bout_starts, bout_start)
                    bout_ends <- c(bout_ends, last_sed_idx)
                  }
                  in_bout <- FALSE
                  bout_start <- NA
                  cumulative_activity <- 0
                  last_sed_idx <- NA
                }
              }
            }
          }

          # Handle final bout
          if (in_bout && !is.na(last_sed_idx)) {
            bout_starts <- c(bout_starts, bout_start)
            bout_ends <- c(bout_ends, last_sed_idx)
          }

          if (length(bout_starts) == 0) next

          # Calculate durations (total epochs from start to end)
          duration_min <- (bout_ends - bout_starts + 1) * (epoch_sec / 60)
          valid_bouts <- duration_min >= min_bout_minutes

          bout_starts <- bout_starts[valid_bouts]
          bout_ends <- bout_ends[valid_bouts]
          duration_min <- duration_min[valid_bouts]

          # "Ignore First Sedentary Break of Each Day" option
          # Removes the first sedentary bout on each calendar day
          if (ignore_first_break && length(bout_starts) > 0) {
            bout_dates <- as.Date(data$timestamp[bout_starts])
            unique_dates <- unique(bout_dates)
            keep_bouts <- rep(TRUE, length(bout_starts))

            for (d in unique_dates) {
              first_bout_idx <- which(bout_dates == d)[1]
              keep_bouts[first_bout_idx] <- FALSE
            }

            bout_starts <- bout_starts[keep_bouts]
            bout_ends <- bout_ends[keep_bouts]
            duration_min <- duration_min[keep_bouts]
          }

          if (length(bout_starts) == 0) next

          # Build bout-level data for each bout
          for (i in seq_along(bout_starts)) {
            start_idx <- bout_starts[i]
            end_idx <- bout_ends[i]

            bout_data <- data[start_idx:end_idx, ]
            bout_start_time <- data$timestamp[start_idx]
            bout_end_time <- data$timestamp[end_idx] + epoch_sec

            # Inter-bout interval (time since last bout ended)
            if (i == 1) {
              time_since_last <- 0
            } else {
              prev_end_time <- data$timestamp[bout_ends[i - 1]] + epoch_sec
              time_since_last <- as.numeric(difftime(bout_start_time, prev_end_time, units = "mins"))
            }

            n_epochs <- nrow(bout_data)

            # Activity counts - all axes
            axis1_counts <- sum(bout_data$axis1, na.rm = TRUE)
            axis2_counts <- if ("axis2" %in% names(bout_data)) sum(bout_data$axis2, na.rm = TRUE) else 0
            axis3_counts <- if ("axis3" %in% names(bout_data)) sum(bout_data$axis3, na.rm = TRUE) else 0

            axis1_avg <- mean(bout_data$axis1, na.rm = TRUE)
            axis2_avg <- if ("axis2" %in% names(bout_data)) mean(bout_data$axis2, na.rm = TRUE) else 0
            axis3_avg <- if ("axis3" %in% names(bout_data)) mean(bout_data$axis3, na.rm = TRUE) else 0

            axis1_max <- max(bout_data$axis1, na.rm = TRUE)
            axis2_max <- if ("axis2" %in% names(bout_data)) max(bout_data$axis2, na.rm = TRUE) else 0
            axis3_max <- if ("axis3" %in% names(bout_data)) max(bout_data$axis3, na.rm = TRUE) else 0

            axis1_cpm <- axis1_avg * (60 / epoch_sec)
            axis2_cpm <- axis2_avg * (60 / epoch_sec)
            axis3_cpm <- axis3_avg * (60 / epoch_sec)

            # Vector magnitude
            if (all(c("axis1", "axis2", "axis3") %in% names(bout_data))) {
              vm <- sqrt(bout_data$axis1^2 + bout_data$axis2^2 + bout_data$axis3^2)
              vm_counts <- sum(vm, na.rm = TRUE)
              vm_avg <- mean(vm, na.rm = TRUE)
              vm_max <- max(vm, na.rm = TRUE)
              vm_cpm <- vm_avg * (60 / epoch_sec)
            } else {
              vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
            }

            # Steps
            if ("steps" %in% names(bout_data)) {
              steps_counts <- sum(bout_data$steps, na.rm = TRUE)
              steps_avg <- mean(bout_data$steps, na.rm = TRUE)
              steps_max <- max(bout_data$steps, na.rm = TRUE)
              steps_per_min <- steps_avg * (60 / epoch_sec)
            } else {
              steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
            }

            # Lux
            if ("lux" %in% names(bout_data)) {
              lux_avg <- mean(bout_data$lux, na.rm = TRUE)
              lux_max <- max(bout_data$lux, na.rm = TRUE)
            } else {
              lux_avg <- lux_max <- 0
            }

            # Calendar days spanned
            start_date <- as.Date(bout_start_time)
            end_date <- as.Date(bout_end_time)
            calendar_days <- as.numeric(end_date - start_date) + 1

            all_bouts[[length(all_bouts) + 1]] <- data.frame(
              Subject = subject_id,
              Filename = r$name,
              Epoch = epoch_sec,
              `Weight (lbs)` = weight_lbs,
              Age = age,
              Gender = gender,
              `Sedentary Bout Start` = fmt_date(bout_start_time, "%m/%d/%Y %I:%M:%S %p"),
              `Sedentary Bout End` = fmt_date(bout_end_time, "%m/%d/%Y %I:%M:%S %p"),
              `Time in Sedentary Bout` = round(duration_min[i], 2),
              `Time since last Sedentary Bout` = round(time_since_last, 2),
              `Axis 1 Counts` = round(axis1_counts, 0),
              `Axis 2 Counts` = round(axis2_counts, 0),
              `Axis 3 Counts` = round(axis3_counts, 0),
              `Axis 1 Average Counts` = round(axis1_avg, 1),
              `Axis 2 Average Counts` = round(axis2_avg, 1),
              `Axis 3 Average Counts` = round(axis3_avg, 1),
              `Axis 1 Max Counts` = axis1_max,
              `Axis 2 Max Counts` = axis2_max,
              `Axis 3 Max Counts` = axis3_max,
              `Axis 1 CPM` = round(axis1_cpm, 1),
              `Axis 2 CPM` = round(axis2_cpm, 1),
              `Axis 3 CPM` = round(axis3_cpm, 1),
              `Vector Magnitude Counts` = round(vm_counts, 1),
              `Vector Magnitude Average Counts` = round(vm_avg, 1),
              `Vector Magnitude Max Counts` = round(vm_max, 1),
              `Vector Magnitude CPM` = round(vm_cpm, 1),
              `Steps Counts` = steps_counts,
              `Steps Average Counts` = round(steps_avg, 1),
              `Steps Max Counts` = steps_max,
              `Steps Per Minute` = round(steps_per_min, 1),
              `Lux Average Counts` = round(lux_avg, 1),
              `Lux Max Counts` = lux_max,
              `Number of Epochs` = n_epochs,
              Time = round(duration_min[i], 2),
              `Calendar Days` = calendar_days,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }

        if (length(all_bouts) == 0) {
          write.csv(data.frame(Message = "No sedentary bouts >= 10 minutes detected"), file, row.names = FALSE)
          return()
        }

        bout_df <- do.call(rbind, all_bouts)
        write.csv(bout_df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    # VM Heatmap Plot
    output$vm_heatmap_plot <- renderPlot({
      gg_app(isTRUE(shared$dark), {
      res <- results()
      sel <- input$selected_participant

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run the analysis to see VM heatmap",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        # Get selected participant's data (or first if "all")
        if (!is.null(sel) && sel != "all" && sel %in% names(res)) {
          r <- res[[sel]]
        } else {
          r <- res[[1]]
        }
        fid <- r$file_id
        f <- shared$files[[fid]]

        if (is.null(f) || !all(c("axis1", "timestamp") %in% names(f$data))) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for heatmap", size = 5) +
            ggplot2::theme_void()
        } else {
          tryCatch({
            canhrActi::plot_vm_heatmap(
              data = f$data,
              timestamp_col = "timestamp",
              axis1_col = "axis1",
              axis2_col = if ("axis2" %in% names(f$data)) "axis2" else NULL,
              axis3_col = if ("axis3" %in% names(f$data)) "axis3" else NULL,
              aggregation = "15min",
              title = paste("Vector Magnitude Heatmap -", r$subject_id)
            )
          }, error = function(e) {
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 4) +
              ggplot2::theme_void()
          })
        }
      }
      })
    }, bg = "white")

    # The links sit in a closed menu, and a suspended downloadHandler never
    # receives its href, so the links would stay disabled
    for (out in c("export_summary", "export_daily", "export_hourly", "export_sedentary",
                  "export_windows", "export_daytypes")) {
      outputOptions(output, out, suspendWhenHidden = FALSE)
    }
  })
}
