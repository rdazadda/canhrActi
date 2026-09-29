# canhrActi Dashboard
# Center for Alaska Native Health Research (CANHR)
# University of Alaska Fairbanks

# Increase file upload limit (default is 5MB)
# Configuration constants
# 16 GB, the browser upload cap. Large recordings should come in through
# Open from disk, which hands the pipeline the path with no copy.
MAX_UPLOAD_SIZE_MB <- 16 * 1024
options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB * 1024^2)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)

# Load canhrActi package
library(canhrActi)

# Run raw .gt3x -> counts conversions off the main session (else they block the UI).
if (requireNamespace("future", quietly = TRUE) && requireNamespace("promises", quietly = TRUE)) {
  future::plan(future::multisession, workers = 2)
}

# Source components and modules
source("R/shared_components.R")
for (file in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) {
  source(file)
}

ui <- dashboardPage(
  skin = "blue",

  # The brand block is the sidebar's top row: shinydashboard sizes .logo to
  # the sidebar width
  dashboardHeader(
    title = tags$span(
      class = "header-brand",
      tags$img(src = paste0("logo.png?v=", as.integer(file.info(file.path("www","logo.png"))$mtime)),
               alt = "", class = "brand-logo-img"),
      tags$span(class = "brand-name", "CANHRActi"),
      tags$a(href = "#", class = "sidebar-toggle brand-toggle", role = "button",
             `aria-expanded` = "true", `aria-label` = "Toggle sidebar",
             title = "Toggle sidebar",
             msym("menu", class = "sh-ico"))
    ),
    titleWidth = 230,

    # Omnibox. It filters the sidebar's own rows, with no server round trip
    tags$li(
      class = "dropdown header-omnibox",
      tags$div(
        class = "omnibox",
        msym("search", class = "omnibox-ico"),
        tags$input(type = "text", id = "omnibox", class = "omnibox-input",
                   autocomplete = "off", spellcheck = "false",
                   placeholder = "Find a page or action"),
        # Shortcut hint, hidden while the field is in use
        tags$span(class = "omnibox-hint", `aria-hidden` = "true",
                  tags$kbd("Ctrl"), tags$kbd("P")),
        tags$div(class = "omnibox-results", role = "listbox")
      )
    )
  ),

  dashboardSidebar(
    width = 230,

    sidebarMenu(
      id = "tabs",


      # Data upload. Add file(s) and Add folder are labels for the app-level
      # file inputs, so they open from any tab; the extension picks the pipeline.
      tags$li(class = "sh-head", tags$span("Data upload"), msym("expand_less", class = "sh-chev")),
      menuItem(text = "Overview", tabName = "overview", icon = sidebar_icon("overview")),
      tags$li(
        class = "sh-act",
        actionLink("overview-side_demo", class = "sh-a",
                   label = tagList(msym("dataset"), tags$span("Try sample files")))
      ),
      tags$li(
        class = "sh-act",
        tags$label(class = "sh-a", `for` = "overview-files",
                   msym("note_add"), tags$span("Add file(s)"))
      ),
      tags$li(
        class = "sh-act",
        tags$label(class = "sh-a", `for` = "overview-dir_files",
                   msym("create_new_folder"), tags$span("Add folder"))
      ),
      # Open from disk hands the pipeline the path, as GGIR takes a datadir;
      # nothing is copied through the browser
      tags$li(
        class = "sh-act",
        actionLink("overview-side_inplace", class = "sh-a",
                   label = tagList(msym("folder_open"), tags$span("Open from disk")))
      ),
      tags$li(
        class = "sh-act",
        actionLink("overview-side_convert", class = "sh-a",
                   label = tagList(msym("swap_horiz"), tags$span("Convert .gt3x to .agd")))
      ),

      # The analyses, wear time first because it runs first
      tags$li(class = "sh-head", tags$span("Analysis"), msym("expand_less", class = "sh-chev")),
      menuItem(text = "Wear time", tabName = "wear_time", icon = sidebar_icon("wear_time")),
      menuItem(text = "Activity",  tabName = "activity",  icon = sidebar_icon("activity")),
      menuItem(text = "Sleep",     tabName = "sleep",     icon = sidebar_icon("sleep")),
      menuItem(text = "Circadian", tabName = "circadian", icon = sidebar_icon("circadian")),
      menuItem(text = "Sedentary", tabName = "sedentary", icon = sidebar_icon("sedentary")),

      # Output: figures drawn from the analyses' results
      tags$li(class = "sh-head", tags$span("Output"), msym("expand_less", class = "sh-chev")),
      menuItem(text = "Visualization", tabName = "graphing", icon = sidebar_icon("graphing")),

      # Support: a header and one plain line, no row chrome
      tags$li(class = "sh-head", tags$span("Support"), msym("expand_less", class = "sh-chev")),
      tags$li(
        class = "sh-support",
        tags$a(href = "https://github.com/rdazadda/canhrActi", target = "_blank",
               "Documentation & Bugs")
      )
    ),

    tags$div(
      class = "sidebar-footer",
      tags$div(class = "sidebar-attrib", "CANHR · University of Alaska Fairbanks"),
      tags$div(
        class = "sidebar-foot-row",
        uiOutput("sidebar_status", inline = TRUE),
        tags$span(class = "sidebar-version", paste0("v", packageVersion("canhrActi")))
      )
    )
  ),

  dashboardBody(
    useShinyjs(),

    tags$head(
      tags$title("CANHRActi"),
      tags$script(HTML("document.title = 'CANHRActi';")),
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", as.integer(file.info(file.path("www","styles.css"))$mtime))),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$script(src = paste0("shell.js?v=", as.integer(file.info(file.path("www","shell.js"))$mtime))),
      tags$script(HTML("
(function() {
  'use strict';
  const STORAGE_KEY = 'canhrActi.sidebar.collapsed';
  const BREAKPOINT = 992;
  // The script runs from <head>; the body exists once init() runs.
  let body = null;
  let toggleBtn;

  function isWide() { return window.innerWidth >= BREAKPOINT; }

  function setCollapsed(collapsed, persist) {
    body.classList.toggle('sidebar-collapse', collapsed);
    if (persist) localStorage.setItem(STORAGE_KEY, String(collapsed));
    if (toggleBtn) toggleBtn.setAttribute('aria-expanded', String(!collapsed));
  }

  function setOpen(open) {
    body.classList.toggle('sidebar-open', open);
    if (toggleBtn) toggleBtn.setAttribute('aria-expanded', String(open));
  }

  function onToggleClick(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
    if (isWide()) {
      setCollapsed(!body.classList.contains('sidebar-collapse'), true);
    } else {
      setOpen(!body.classList.contains('sidebar-open'));
    }
  }

  function init() {
    body = document.body;
    const toggles = document.querySelectorAll('.sidebar-toggle');
    if (!toggles.length) return;
    toggleBtn = toggles[0];

    if (isWide()) {
      setCollapsed(localStorage.getItem(STORAGE_KEY) === 'true', false);
    }

    toggles.forEach(function(t) { t.addEventListener('click', onToggleClick, true); });

    document.addEventListener('click', function(e) {
      if (!isWide() && body.classList.contains('sidebar-open')) {
        if (!e.target.closest('.main-sidebar') && !e.target.closest('.sidebar-toggle')) {
          setOpen(false);
        }
      }
    });

    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape' && body.classList.contains('sidebar-open')) {
        setOpen(false);
      }
    });

    window.addEventListener('resize', function() {
      if (isWide() && body.classList.contains('sidebar-open')) {
        body.classList.remove('sidebar-open');
      }
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
"))
    ),

    # Tab content
    # Outside tabItems: a label can only open an input that is in the DOM, and
    # an inactive tab is display:none
    mod_overview_inputs("overview"),

    tabItems(
      tabItem(tabName = "overview", mod_overview_ui("overview")),
      tabItem(tabName = "wear_time", mod_wear_time_ui("wear_time")),
      tabItem(tabName = "activity", mod_activity_ui("activity")),
      tabItem(tabName = "sedentary", mod_sedentary_ui("sedentary")),
      tabItem(tabName = "sleep", mod_sleep_ui("sleep")),
      tabItem(tabName = "circadian", mod_circadian_ui("circadian")),
      tabItem(tabName = "graphing", mod_graphing_ui("graphing"))
    )
  )
)

# SERVER DEFINITION

server <- function(input, output, session) {

  # Shared reactive values across all modules
  shared <- reactiveValues(
    files = list(),
    # Raw pipeline results, keyed like files
    raw = list(),
    # GGIR part 5 by shared$raw id, published by the Activity page and read by
    # Visualization. Kept outside results, which removing every counts file
    # clears wholesale.
    raw_timeuse = list(),
    # GGIR's report per raw recording, from the Activity page. $dir is the
    # milestone tree in the session's temp space, so it lasts as long as the app.
    raw_report = list(),
    file_count = 0,
    selected_file = NULL,
    # The recording last picked on any page, NULL for all (focus_set, focus_get)
    focus = NULL,
    # The open tab (on_tab_shown)
    tab = NULL,
    data_loaded = FALSE,
    # Results storage
    results = list(
      wear_time = list(),
      sleep = list(),
      activity = list(),
      sedentary = list(),
      circadian = list()
    )
  )

  # Theme, reported by www/shell.js on connect and on every toggle; the charts
  # are drawn on the server
  observeEvent(input$app_theme, {
    shared$dark <- identical(input$app_theme, "dark")
  }, ignoreNULL = FALSE)

  observeEvent(input$tabs, shared$tab <- input$tabs)

  # Sidebar footer: what is loaded
  output$sidebar_status <- renderUI({
    if (shared$file_count == 0) {
      tags$span(class = "sidebar-badge", "NO DATA")
    } else {
      tags$span(class = "sidebar-badge is-loaded",
                toupper(pluralize(shared$file_count, "file")))
    }
  })

  # Module servers
  mod_overview_server("overview", shared, parent_session = session)
  mod_wear_time_server("wear_time", shared)
  mod_sleep_server("sleep", shared)
  mod_activity_server("activity", shared)
  mod_sedentary_server("sedentary", shared)
  mod_circadian_server("circadian", shared)
  mod_graphing_server("graphing", shared)
}

shinyApp(ui, server)
