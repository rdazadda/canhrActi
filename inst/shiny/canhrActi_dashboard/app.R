# canhrActi Dashboard
# Center for Alaska Native Health Research (CANHR)
# University of Alaska Fairbanks

# Increase file upload limit to 500MB (default is 5MB)
# Configuration constants
MAX_UPLOAD_SIZE_MB <- 500
options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB * 1024^2)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyjs)

# Load canhrActi package
library(canhrActi)

# Optional: loading spinners
if (requireNamespace("shinycssloaders", quietly = TRUE)) {

  library(shinycssloaders)
  has_spinners <- TRUE
} else {
  has_spinners <- FALSE
  withSpinner <- function(x, ...) x
}

# Source components and modules
source("R/shared_components.R")
for (file in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) {
  source(file)
}

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      class = "header-brand",
      tags$img(src = paste0("logo.png?v=", as.integer(file.info(file.path("www","logo.png"))$mtime)), alt = "", class = "brand-logo-img"),
      tags$span(class = "brand-name", "CANHRActi")
    ),
    titleWidth = 260,

    tags$li(
      class = "dropdown header-page-title",
      uiOutput("header_page_title")
    ),

    tags$li(
      class = "dropdown header-file-info",
      uiOutput("header_file_info")
    ),

    # Help/Documentation link
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/rdazadda/canhrActi/issues",
        target = "_blank",
        class = "header-link",
        title = "Report issues or request features",
        icon("question-circle"),
        tags$span(class = "header-link-text", "Support")
      )
    )
  ),

  dashboardSidebar(
    width = 260,

    tags$div(
      class = "sidebar-brand-section",
      title = "Center for Alaska Native Health Research",
      tags$div(class = "sidebar-brand-mark", "CANHR"),
      tags$div(
        class = "sidebar-brand-descriptor",
        "Center for Alaska Native Health Research"
      )
    ),

    sidebarMenu(
      id = "tabs",

      menuItem(text = "Overview", tabName = "overview"),

      tags$div(class = "sidebar-section-header", "Workflow"),
      menuItem(text = "Upload",    tabName = "upload"),
      menuItem(text = "Wear Time", tabName = "wear_time"),

      tags$div(class = "sidebar-section-header", "Analysis"),
      menuItem(text = "Activity",  tabName = "activity"),
      menuItem(text = "Sleep",     tabName = "sleep"),
      menuItem(text = "Circadian", tabName = "circadian"),
      menuItem(text = "Sedentary", tabName = "sedentary"),
      menuItem(
        text = "Visualization",
        tabName = "graphing"
      )
    ),

    # Sidebar footer with version info
    tags$div(
      class = "sidebar-footer",
      tags$div(
        class = "sidebar-version",
        paste0("Version ", packageVersion("canhrActi"))
      ),
      tags$div(
        class = "sidebar-copyright",
        "\u00A9 CANHR | UAF"
      )
    )
  ),

  dashboardBody(
    useShinyjs(),

    tags$head(
      tags$title("CANHRActi"),
      tags$script(HTML("document.title = 'CANHRActi';")),
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", as.integer(file.info(file.path("www","styles.css"))$mtime))),
      # Favicon - add uaf_logo.png to www/ folder to enable
      # tags$link(rel = "icon", type = "image/png", href = "uaf_logo.png"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$script(HTML("
(function() {
  'use strict';
  const STORAGE_KEY = 'canhrActi.sidebar.collapsed';
  const BREAKPOINT = 992;
  const body = document.body;
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
    toggleBtn = document.querySelector('.sidebar-toggle');
    if (!toggleBtn) return;

    if (isWide()) {
      setCollapsed(localStorage.getItem(STORAGE_KEY) === 'true', false);
    }

    toggleBtn.addEventListener('click', onToggleClick, true);

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
    tabItems(
      tabItem(tabName = "overview", mod_overview_ui("overview")),
      tabItem(tabName = "upload", mod_upload_ui("upload")),
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
    file_count = 0,
    selected_file = NULL,
    epoch_length = 60,
    cut_points = "freedson",
    data_loaded = FALSE,
    visualization_complete = FALSE,
    # Parameters for expanded analysis options
    data_type = "axis1",
    auto_cutpoints = FALSE,
    participant_age = NULL,
    sleep_algorithm = "cole.kripke",
    # Workflow completion tracking
    workflow = list(
      data_uploaded = FALSE,
      wear_validated = FALSE,
      analysis_complete = FALSE
    ),
    # Results storage
    results = list(
      wear_time = list(),
      sleep = list(),
      activity = list(),
      sedentary = list(),
      circadian = list(),
      energy = list(),
      graphing = list()
    )
  )

  output$header_file_info <- renderUI({
    if (shared$file_count == 0) {
      tags$div(
        class = "file-badge file-badge-empty",
        icon("folder-open"),
        "Ready to analyze"
      )
    } else {
      file_text <- if (shared$file_count == 1) "1 file loaded" else paste0(shared$file_count, " files loaded")
      tags$div(
        class = "file-badge file-badge-active",
        icon("database"),
        file_text
      )
    }
  })

  output$header_page_title <- renderUI({
    labels <- c(
      overview  = "Overview",
      upload    = "Upload",
      wear_time = "Wear Time",
      activity  = "Activity",
      sleep     = "Sleep",
      circadian = "Circadian",
      sedentary = "Sedentary",
      graphing  = "Visualization"
    )
    current <- input$tabs
    if (is.null(current) || !current %in% names(labels)) return(NULL)
    tags$div(class = "page-title", labels[[current]])
  })

  # Update workflow tracking
  observe({
    shared$workflow$data_uploaded <- shared$file_count > 0
  })

  observe({
    shared$workflow$wear_validated <- length(shared$results$wear_time) > 0
  })

  observe({
    shared$workflow$analysis_complete <-
      length(shared$results$activity) > 0 ||
      length(shared$results$sleep) > 0 ||
      length(shared$results$circadian) > 0
  })

  # Module servers
  mod_overview_server("overview", shared, parent_session = session)
  mod_upload_server("upload", shared)
  mod_wear_time_server("wear_time", shared)
  mod_sleep_server("sleep", shared)
  mod_activity_server("activity", shared)
  mod_sedentary_server("sedentary", shared)
  mod_circadian_server("circadian", shared)
  mod_graphing_server("graphing", shared)
}

shinyApp(ui, server)
