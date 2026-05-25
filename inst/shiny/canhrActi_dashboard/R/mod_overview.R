#' Overview Module
#'
#' Welcome page with workflow guidance and quick stats when data is loaded

mod_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Welcome Section (shown before data is loaded)
    uiOutput(ns("welcome_section")),

    # Quick Stats Section (shown after data is loaded)
    uiOutput(ns("stats_section")),

    # Page Footer
    tags$div(
      class = "app-footer",
      tags$div(
        class = "footer-version",
        uiOutput(ns("footer_version"), inline = TRUE)
      ),
      tags$div(
        class = "footer-links",
        tags$a(href = "https://www.uaf.edu/canhr/", target = "_blank", "CANHR Website"),
        tags$a(href = "#", onclick = "return false;", "Documentation")
      )
    )
  )
}

mod_overview_server <- function(id, shared, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Footer version
    output$footer_version <- renderUI({
      paste0("canhrActi v", packageVersion("canhrActi"))
    })

    # Empty-state Overview: methods-manual front matter.
    # A contents page, not a dashboard. Type and whitespace only.
    output$welcome_section <- renderUI({
      if (!shared$data_loaded || shared$file_count == 0) {

        tagList(
          tags$div(
            class = "overview-shell",

            tags$div(
              class = "welcome-hero",
              tags$div(
                class = "hero-content",
                tags$h1(class = "hero-title", "Welcome to CANHRActi"),
                tags$p(class = "hero-subtitle",
                       "Accelerometer analysis for activity, sedentary behavior, sleep, and circadian research.")
              )
            ),

            tags$div(
              class = "overview-card",
              tags$p(
                class = "overview-card-text",
                "CANHRActi reads ActiGraph files and produces epoch-level ",
                tags$strong("activity-intensity classifications"), ", ",
                tags$strong("sleep-period estimates"), ", ",
                tags$strong("energy-expenditure estimates"), ", ",
                tags$strong("sedentary-bout summaries"), ", and ",
                tags$strong("circadian-rhythm parameters"), "."
              ),
              tags$p(
                class = "overview-card-text",
                "Outputs are wear-time corrected and exported as participant-level CSV and study-level summary tables ready for ",
                tags$strong("Stata, SPSS, R, or Python"),
                ", alongside publication-ready plots (activity heatmaps, hypnograms, circadian polar plots)."
              ),
              tags$div(
                class = "overview-card-action",
                actionButton(
                  ns("go_upload"),
                  HTML("Import data <span class=\"btn-arrow\" aria-hidden=\"true\">&rarr;</span>"),
                  class = "btn btn-overview-cta"
                )
              )
            )
          )
        )
      } else {
        # Minimal header when data is loaded
        file_word <- if (shared$file_count == 1) "file" else "files"
        tags$div(
          class = "page-header-loaded",
          tags$h2(class = "page-header-title", "Analysis Dashboard"),
          tags$p(class = "page-header-subtitle",
                 paste0(shared$file_count, " ", file_word, " ready for analysis. Choose an analysis module from the sidebar to begin."))
        )
      }
    })

    # Navigate to upload page
    observeEvent(input$go_upload, {
      updateTabItems(session = parent_session, "tabs", selected = "upload")
    })

    # Stats Section: Quick metrics and recent activity (after data loaded)
    output$stats_section <- renderUI({
      # Only show when data is loaded
      req(shared$data_loaded, shared$file_count > 0)

      # Compute quick stats
      total_epochs <- sum(sapply(shared$files, function(f) {
        n <- f$n_epochs
        if (is.null(n) || !is.numeric(n)) return(0)
        as.numeric(n[1])
      }), na.rm = TRUE)

      total_duration <- sum(sapply(shared$files, function(f) {
        d <- f$duration_hrs
        if (is.null(d) || !is.numeric(d)) return(0)
        as.numeric(d[1])
      }), na.rm = TRUE)

      # Workflow status
      wear_done <- !is.null(shared$results$wear_time) && length(shared$results$wear_time) > 0
      activity_done <- !is.null(shared$results$activity) && length(shared$results$activity) > 0
      sleep_done <- !is.null(shared$results$sleep) && length(shared$results$sleep) > 0
      circadian_done <- !is.null(shared$results$circadian) && length(shared$results$circadian) > 0
      sedentary_done <- !is.null(shared$results$sedentary) && length(shared$results$sedentary) > 0

      tagList(
        # Quick Stats Row
        fluidRow(
          column(3,
            tags$div(
              class = "metric-card metric-card--inline",
              tags$div(class = "metric-value", shared$file_count),
              tags$div(class = "metric-label", "Files Loaded")
            )
          ),
          column(3,
            tags$div(
              class = "metric-card metric-card--inline",
              tags$div(class = "metric-value", paste0(round(total_duration, 1), "h")),
              tags$div(class = "metric-label", "Total Duration")
            )
          ),
          column(3,
            tags$div(
              class = "metric-card metric-card--inline",
              tags$div(class = "metric-value", format(total_epochs, big.mark = ",")),
              tags$div(class = "metric-label", "Total Epochs")
            )
          ),
          column(3,
            tags$div(
              class = "metric-card metric-card--inline",
              tags$div(class = "metric-value", paste0(shared$epoch_length, "s")),
              tags$div(class = "metric-label", "Epoch Length")
            )
          )
        ),

        # Next Steps Section
        tags$div(
          class = "next-steps-section",
          tags$h3(class = "next-steps-title", "Next Steps"),
          tags$div(
            class = "next-steps-grid",
            tags$div(
              class = paste("next-step-card", if (wear_done) "completed" else ""),
              tags$div(class = "next-step-status",
                if (wear_done) icon("check-circle") else icon("circle")
              ),
              tags$div(class = "next-step-content",
                tags$h4("Wear Time"),
                tags$p(if (wear_done) "Completed" else "Detect valid wear periods")
              ),
              if (!wear_done) {
                actionButton(ns("go_wear"), "Run", class = "btn-next-step")
              }
            ),
            tags$div(
              class = paste("next-step-card", if (activity_done) "completed" else ""),
              tags$div(class = "next-step-status",
                if (activity_done) icon("check-circle") else icon("circle")
              ),
              tags$div(class = "next-step-content",
                tags$h4("Activity"),
                tags$p(if (activity_done) "Completed" else "Classify intensity levels")
              ),
              if (!activity_done) {
                actionButton(ns("go_activity"), "Run", class = "btn-next-step")
              }
            ),
            tags$div(
              class = paste("next-step-card", if (sleep_done) "completed" else ""),
              tags$div(class = "next-step-status",
                if (sleep_done) icon("check-circle") else icon("circle")
              ),
              tags$div(class = "next-step-content",
                tags$h4("Sleep"),
                tags$p(if (sleep_done) "Completed" else "Run sleep analysis")
              ),
              if (!sleep_done) {
                actionButton(ns("go_sleep"), "Run", class = "btn-next-step")
              }
            ),
            tags$div(
              class = paste("next-step-card", if (circadian_done) "completed" else ""),
              tags$div(class = "next-step-status",
                if (circadian_done) icon("check-circle") else icon("circle")
              ),
              tags$div(class = "next-step-content",
                tags$h4("Circadian"),
                tags$p(if (circadian_done) "Completed" else "Run circadian analysis")
              ),
              if (!circadian_done) {
                actionButton(ns("go_circadian"), "Run", class = "btn-next-step")
              }
            ),
            tags$div(
              class = paste("next-step-card", if (sedentary_done) "completed" else ""),
              tags$div(class = "next-step-status",
                if (sedentary_done) icon("check-circle") else icon("circle")
              ),
              tags$div(class = "next-step-content",
                tags$h4("Sedentary"),
                tags$p(if (sedentary_done) "Completed" else "Run sedentary analysis")
              ),
              if (!sedentary_done) {
                actionButton(ns("go_sedentary"), "Run", class = "btn-next-step")
              }
            )
          )
        ),

        # Files Summary Table
        tags$div(
          class = "files-section",
          tags$h3(class = "files-title", "Loaded Files"),
          DT::dataTableOutput(ns("files_summary_table"))
        )
      )
    })

    # Navigation handlers for next step buttons
    observeEvent(input$go_wear, {
      updateTabItems(session = parent_session, "tabs", selected = "wear_time")
    })

    observeEvent(input$go_activity, {
      updateTabItems(session = parent_session, "tabs", selected = "activity")
    })

    observeEvent(input$go_sleep, {
      updateTabItems(session = parent_session, "tabs", selected = "sleep")
    })

    observeEvent(input$go_circadian, {
      updateTabItems(session = parent_session, "tabs", selected = "circadian")
    })

    observeEvent(input$go_sedentary, {
      updateTabItems(session = parent_session, "tabs", selected = "sedentary")
    })

    # Files summary table
    output$files_summary_table <- DT::renderDataTable({
      req(shared$file_count > 0)

      df <- data.frame(
        File = sapply(shared$files, function(f) f$name),
        Subject = sapply(shared$files, function(f) f$subject_info$id %||% "Unknown"),
        Duration = sapply(shared$files, function(f) {
          d <- f$duration_hrs
          if (is.null(d) || is.na(d)) return("--")
          paste0(round(d, 1), "h")
        }),
        Epochs = sapply(shared$files, function(f) {
          n <- f$n_epochs
          if (is.null(n) || is.na(n)) return("--")
          format(n, big.mark = ",")
        }),
        Epoch_Length = sapply(shared$files, function(f) paste0(f$epoch_length, "s")),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(
          pageLength = 5,
          dom = 'tip',
          scrollX = TRUE,
          language = list(
            emptyTable = "No files loaded"
          )
        ),
        rownames = FALSE,
        colnames = c("File Name", "Subject ID", "Duration", "Epochs", "Epoch Length"),
        selection = "none"
      )
    })
  })
}
