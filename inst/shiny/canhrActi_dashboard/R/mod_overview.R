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

    # Welcome Section: Workflow guidance (before data loaded)
    output$welcome_section <- renderUI({
      # Show welcome content or minimal header based on data state
      if (!shared$data_loaded || shared$file_count == 0) {
        # Full welcome view
        tagList(
          tags$div(
            class = "overview-shell",
            # Hero Section
            tags$div(
              class = "welcome-hero",
              tags$div(
                class = "hero-content",
                tags$h1(class = "hero-title", "Welcome to CANHRActi"),
                tags$p(class = "hero-subtitle",
                       "Comprehensive accelerometer analysis for physical activity, sleep, and circadian research")
              )
            ),

            # Overview Grid
            tags$div(
              class = "overview-grid",
              tags$div(
                class = "overview-panel overview-panel-steps",
                tags$div(
                  class = "overview-panel-header",
                  tags$h2(class = "overview-panel-title", "Get Started"),
                  tags$p(class = "overview-panel-subtitle", "Follow these steps to analyze your accelerometer data")
                ),
                tags$div(
                  class = "overview-step-list",
                  tags$div(
                    class = "overview-step overview-step--no-icon",
                    tags$div(class = "overview-step-number", "1"),
                    tags$div(
                      class = "overview-step-body",
                      tags$h3(class = "overview-step-title", "Import Data"),
                      tags$p(class = "overview-step-description",
                             "Load AGD files from ActiLife. Batch import supported for multiple participants.")
                    ),
                    tags$div(
                      class = "overview-step-meta",
                      actionButton(
                        ns("go_upload"),
                        "Get Started",
                        class = "btn-workflow btn-workflow-primary",
                        icon = icon("arrow-right")
                      )
                    )
                  ),
                  tags$div(
                    class = "overview-step overview-step--no-icon",
                    tags$div(class = "overview-step-number", "2"),
                    tags$div(
                      class = "overview-step-body",
                      tags$h3(class = "overview-step-title", "Validate Wear Time"),
                      tags$p(class = "overview-step-description",
                             "Identify valid recording periods using Choi, Troiano, or CANHR algorithms.")
                    ),
                    tags$div(
                      class = "overview-step-meta",
                      tags$div(class = "overview-step-hint",
                        "After importing data"
                      )
                    )
                  ),
                  tags$div(
                    class = "overview-step overview-step--no-icon",
                    tags$div(class = "overview-step-number", "3"),
                    tags$div(
                      class = "overview-step-body",
                      tags$h3(class = "overview-step-title", "Analyze & Export"),
                      tags$p(class = "overview-step-description",
                             "Run activity, sleep, and circadian analyses. Export publication-ready results.")
                    ),
                    tags$div(
                      class = "overview-step-meta",
                      tags$div(class = "overview-step-hint",
                        "After validation"
                      )
                    )
                  )
                )
              ),
              tags$div(
                class = "overview-panel overview-panel-details",
                tags$div(
                  class = "overview-panel-header",
                  tags$h2(class = "overview-panel-title", "Supported Formats & Methods"),
                  tags$p(class = "overview-panel-subtitle", "Evidence-based algorithms for accelerometer research")
                ),
                tags$div(
                  class = "overview-detail-block",
                  tags$h4(class = "overview-detail-title", icon("file-alt"), "File Format"),
                  tags$ul(class = "overview-detail-list",
                    tags$li(tags$strong(".agd"), " - ActiGraph database files (from ActiLife)")
                  ),
                  tags$p(class = "overview-detail-note",
                         "Contains epoch-level activity counts, steps, and inclinometer data")
                ),
                tags$div(
                  class = "overview-detail-block",
                  tags$h4(class = "overview-detail-title", icon("cogs"), "Analysis Methods"),
                  tags$ul(class = "overview-detail-list",
                    tags$li("Wear time: Choi, Troiano, CANHR2025"),
                    tags$li("Activity: Freedson, Evenson, Puyau cutpoints"),
                    tags$li("Sleep: Cole-Kripke, Sadeh, Tudor-Locke"),
                    tags$li("Circadian: L5/M10, IS/IV, RA, cosinor")
                  )
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
          fluidRow(
            # Wear Time
            column(3,
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
              )
            ),
            # Activity
            column(3,
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
              )
            ),
            # Sleep
            column(3,
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
              )
            ),
            # Circadian
            column(3,
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
              )
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
