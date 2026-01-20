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

# Load canhrActi
if (!isNamespaceLoaded("canhrActi")) {
  pkg_root <- normalizePath(file.path(dirname(getwd()), "..", ".."), mustWork = FALSE)
  if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    devtools::load_all(pkg_root, quiet = TRUE)
  } else {
    library(canhrActi)
  }
}

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
    title = tags$div(
      class = "header-brand",
      tags$span(class = "brand-logo", "CANHR"),
      tags$span(class = "brand-name", "Acti")
    ),
    titleWidth = 260,  # Increased to accommodate larger brand typography (22px + 18px)

    # File info indicator (shown when data loaded)
    tags$li(
      class = "dropdown header-file-info",
      uiOutput("header_file_info")
    ),

    # Workflow progress indicator
    tags$li(
      class = "dropdown header-progress",
      uiOutput("header_workflow_progress")
    ),

    # Help/Documentation link
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.uaf.edu/canhr/",
        target = "_blank",
        class = "header-link",
        title = "Visit CANHR website for documentation and support",
        icon("question-circle"),
        tags$span(class = "header-link-text", "Support")
      )
    )
  ),

  dashboardSidebar(
    width = 260,

    tags$div(
      class = "sidebar-brand-section",
      tags$div(class = "sidebar-institution", "Center for Alaska Native Health Research"),
      tags$div(class = "sidebar-tagline", "Accelerometer Analysis Suite")
    ),

    sidebarMenu(
      id = "tabs",

      # Overview/Home
      menuItem(
        text = "Overview",
        tabName = "overview"
      ),

      # DATA Section
      tags$div(class = "sidebar-section-header", "DATA"),
      menuItem(
        text = "Upload",
        tabName = "upload"
      ),

      # VALIDATE Section
      tags$div(class = "sidebar-section-header", "VALIDATE"),
      menuItem(
        text = "Wear Time",
        tabName = "wear_time"
      ),

      # ANALYZE Section
      tags$div(class = "sidebar-section-header", "ANALYZE"),
      menuItem(
        text = "Activity",
        tabName = "activity"
      ),
      menuItem(
        text = "Sleep",
        tabName = "sleep"
      ),
      menuItem(
        text = "Circadian",
        tabName = "circadian"
      ),
      menuItem(
        text = "Sedentary",
        tabName = "sedentary"
      ),

      # OUTPUT Section
      tags$div(class = "sidebar-section-header", "OUTPUT"),
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
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", as.character(packageVersion("canhrActi")))),
      # Favicon - add uaf_logo.png to www/ folder to enable
      # tags$link(rel = "icon", type = "image/png", href = "uaf_logo.png"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
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

  output$header_workflow_progress <- renderUI({
    # Determine workflow status
    step1_complete <- shared$file_count > 0
    step2_complete <- length(shared$results$wear_time) > 0
    step3_complete <- length(shared$results$activity) > 0 ||
                      length(shared$results$sleep) > 0 ||
                      length(shared$results$circadian) > 0

    # Current active step
    current_step <- if (!step1_complete) 1
                    else if (!step2_complete) 2
                    else if (!step3_complete) 3
                    else 4

    # Step definitions with labels
    steps <- list(
      list(label = "Upload", icon = "cloud-upload-alt"),
      list(label = "Validate", icon = "check-circle"),
      list(label = "Analyze", icon = "chart-line"),
      list(label = "Export", icon = "download")
    )
    step_complete <- c(step1_complete, step2_complete, step3_complete, FALSE)

    tags$div(
      class = "workflow-indicator",

      # Step 1: Upload
      tags$div(
        class = paste("workflow-step",
                      if (step1_complete) "completed" else if (current_step == 1) "active" else ""),
        title = steps[[1]]$label,
        if (step1_complete) icon("check") else "1"
      ),
      tags$div(class = paste("workflow-connector", if (step1_complete) "completed" else "")),

      # Step 2: Validate
      tags$div(
        class = paste("workflow-step",
                      if (step2_complete) "completed" else if (current_step == 2) "active" else ""),
        title = steps[[2]]$label,
        if (step2_complete) icon("check") else "2"
      ),
      tags$div(class = paste("workflow-connector", if (step2_complete) "completed" else "")),

      # Step 3: Analyze
      tags$div(
        class = paste("workflow-step",
                      if (step3_complete) "completed" else if (current_step == 3) "active" else ""),
        title = steps[[3]]$label,
        if (step3_complete) icon("check") else "3"
      ),
      tags$div(class = paste("workflow-connector", if (step3_complete) "completed" else "")),

      # Step 4: Export
      tags$div(
        class = paste("workflow-step", if (current_step == 4) "active" else ""),
        title = steps[[4]]$label,
        "4"
      )
    )
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
