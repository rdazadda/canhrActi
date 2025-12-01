# canhrActi Dashboard
# Center for Alaska Native Health Research (CANHR)
# University of Alaska Fairbanks

# Increase file upload limit to 500MB (default is 5MB)
# Raw AGD and GT3X files can be 50-200MB+
options(shiny.maxRequestSize = 500 * 1024^2)

library(shiny)
library(shinydashboard)
library(canhrActi)
library(ggplot2)
library(DT)
library(shinyjs)

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(
      tags$img(src = "uaf_logo.png", height = "35px", style = "margin-right: 12px; vertical-align: middle;",
               onerror = "this.style.display='none'"),
      span("canhrActi", style = "font-weight: 600; letter-spacing: 0.5px;")
    ),
    titleWidth = 280,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.uaf.edu/canhr/",
        target = "_blank",
        style = "color: rgba(255,255,255,0.8); font-size: 12px; padding: 15px 20px;",
        icon("external-link-alt", style = "margin-right: 5px;"),
        "CANHR Website"
      )
    )
  ),
  dashboardSidebar(
    width = 280,
    div(
      class = "sidebar-header",
      style = "padding: 20px 15px; text-align: center; border-bottom: 1px solid rgba(255,255,255,0.1);",
      div(
        style = "font-size: 11px; color: rgba(255,255,255,0.7); line-height: 1.5;",
        tags$strong("Center for Alaska Native Health Research"),
        br(),
        "Accelerometer Analysis Platform"
      )
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Data Upload", tabName = "upload", icon = icon("upload"),
               badgeLabel = "Start", badgeColor = "yellow"),
      menuItem("Wear Time Validation", tabName = "wear_time", icon = icon("clock")),
      menuItem("Physical Activity", tabName = "activity", icon = icon("running")),
      menuItem("Sleep Analysis", tabName = "sleep", icon = icon("moon")),
      menuItem("Circadian Rhythm", tabName = "circadian", icon = icon("sun"))
    ),
    hr(style = "border-color: rgba(255,255,255,0.1); margin: 15px 0;"),
    div(
      style = "padding: 10px 20px;",
      div(
        class = "sidebar-info",
        style = "background: rgba(255,205,0,0.1); border-radius: 8px; padding: 12px; margin-bottom: 15px;",
        div(style = "color: #FFCD00; font-weight: 600; font-size: 11px; margin-bottom: 5px;",
            icon("info-circle"), " Quick Tips"),
        div(style = "color: rgba(255,255,255,0.7); font-size: 10px; line-height: 1.5;",
            "Upload AGD, GT3X, or CSV files from ActiGraph devices. ",
            "Raw files are auto-converted to activity counts.")
      )
    ),
    div(
      class = "uaf-credit",
      div(style = "font-size: 13px; font-weight: 600; color: #FFCD00; margin-bottom: 8px;",
          paste0("v", packageVersion("canhrActi"))),
      div(style = "font-size: 10px; color: rgba(255,255,255,0.5); line-height: 1.6;",
          "University of Alaska Fairbanks", br(),
          tags$a(href = "https://www.uaf.edu/canhr/", target = "_blank",
                 style = "color: #FFCD00;", "CANHR"), " | ",
          tags$a(href = "mailto:canhr@alaska.edu",
                 style = "color: rgba(255,255,255,0.6);", "canhr@alaska.edu")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "icon", type = "image/png", href = "uaf_logo.png"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$style(HTML("
        .sidebar-header { background: rgba(0,0,0,0.2); }
        .sidebar-info { transition: all 0.3s ease; }
        .sidebar-info:hover { background: rgba(255,205,0,0.15) !important; }
      "))
    ),
    tabItems(
      tabItem(tabName = "upload", mod_upload_ui("upload")),
      tabItem(tabName = "wear_time", mod_wear_time_ui("wear_time")),
      tabItem(tabName = "activity", mod_activity_ui("activity")),
      tabItem(tabName = "sleep", mod_sleep_ui("sleep")),
      tabItem(tabName = "circadian", mod_circadian_ui("circadian"))
    )
  )
)

server <- function(input, output, session) {
  shared <- reactiveValues(
    files = list(),
    file_count = 0,
    selected_file = NULL,
    epoch_length = 60,
    cut_points = "freedson",
    data_loaded = FALSE,
    results = list(
      wear_time = list(),
      sleep = list(),
      activity = list(),
      circadian = list(),
      energy = list()
    )
  )

  mod_upload_server("upload", shared)
  mod_wear_time_server("wear_time", shared)
  mod_sleep_server("sleep", shared)
  mod_activity_server("activity", shared)
  mod_circadian_server("circadian", shared)
}

shinyApp(ui, server)
