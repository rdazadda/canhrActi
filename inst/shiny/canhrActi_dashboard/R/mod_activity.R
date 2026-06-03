# Module: Physical Activity Analysis
# Redesigned with chart-first layout, compact metrics, and collapsible controls

mod_activity_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Page Header
    page_header(
      icon_name = "running",
      title = "Physical Activity Analysis",
      subtitle = "Activity intensity & MVPA",
      status_output_id = ns("wear_time_status")
    ),

    # Compact Metrics Strip
    div(class = "metrics-strip metrics-strip--transparent",
      # File count badge
      div(class = "file-info-badge metrics-strip-fixed",
        textOutput(ns("files_count"), inline = TRUE), " files"
      ),

      # Sedentary metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_sedentary"), inline = TRUE)),
        div(class = "metric-label", "Sedentary")
      ),

      # Light metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_light"), inline = TRUE)),
        div(class = "metric-label", "Light")
      ),

      # MVPA metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_mvpa"), inline = TRUE)),
        div(class = "metric-label", "MVPA/day")
      ),

      # Steps metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_steps"), inline = TRUE)),
        div(class = "metric-label", "Steps/day")
      ),

      # Quick actions
      div(class = "cluster cluster--gap-2 ml-auto metrics-strip-fixed",
        actionButton(ns("run_btn"), span(icon("play"), "Run Analysis"),
                     class = "btn-primary"),
        actionButton(ns("clear_results"), span(icon("redo"), "Reset"),
                     class = "btn-default")
      )
    ),

    # Main Content: Two-column layout
    fluidRow(
      # Left: Controls (narrow)
      column(width = 3,
        # Essential Controls (always visible)
        div(class = "controls-panel",
          div(class = "controls-header",
            div(class = "controls-header-title",
              icon("sliders-h"), "Analysis Settings"
            )
          ),
          div(class = "mt-3",
            # Cut-points selector (most important)
            selectInput(ns("cut_points"), "Cut-Points Algorithm:",
              choices = c(
                "Adult: Freedson (1998)" = "freedson",
                "Adult: Troiano NHANES (2008)" = "troiano",
                "Adult: Matthews (2005)" = "matthews",
                "Children: Evenson (2008)" = "evenson",
                "Older: Copeland (2009)" = "copeland_older",
                "VM3: Sasaki (2011)" = "sasaki_vm3",
                "VM3: Freedson (2011)" = "freedson_vm3"
              ),
              selected = "freedson",
              width = "100%"
            ),

            # Data type toggle
            radioButtons(ns("data_type"), "Data Type:",
              choices = c("Axis 1" = "axis1", "Vector Magnitude" = "vm"),
              selected = "axis1", inline = TRUE
            ),

            # Wear time filter
            checkboxInput(ns("exclude_nonwear"),
              span(icon("check-circle"), " Apply Wear Time Filter"),
              value = TRUE
            ),
            tags$small(class = "control-hint text-muted mb-3",
              "Requires wear time analysis first"
            )
          )
        ),

        # Advanced Options (collapsible)
        div(class = "controls-panel",
          tags$div(
            `data-toggle` = "collapse",
            `data-target` = paste0("#", ns("advanced_options")),
            class = "controls-header controls-header--clickable",
            div(class = "controls-header-title",
              icon("cog"), "Advanced Options"
            ),
            div(class = "controls-toggle",
              icon("chevron-down"), "expand"
            )
          ),
          div(id = ns("advanced_options"), class = "collapse",
            div(class = "controls-body",
              # METs Algorithm
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("fire"), "METs Calculation"
                ),
                checkboxInput(ns("use_mets"), "Enable METs", value = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("use_mets")),
                  selectInput(ns("mets_algo"), NULL,
                    choices = c(
                      "Freedson VM3 (Sasaki 2011)" = "freedson.vm3",
                      "Freedson Adult (1998)" = "freedson.adult",
                      "Crouter 2-Regression (2010)" = "crouter"
                    ),
                    selected = "freedson.vm3"
                  )
                )
              ),

              # Energy Expenditure
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("bolt"), "Energy Expenditure"
                ),
                checkboxInput(ns("use_ee"), "Enable EE", value = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("use_ee")),
                  selectInput(ns("ee_algo"), NULL,
                    choices = c(
                      "Freedson Combination (1998)" = "freedson.combination",
                      "Freedson (1998)" = "freedson",
                      "Williams Work-Energy (1998)" = "williams"
                    ),
                    selected = "freedson.combination"
                  )
                )
              ),

              # MVPA Bouts
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("stopwatch"), "MVPA Bouts"
                ),
                checkboxInput(ns("use_bouts"), "Detect Bouts", value = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("use_bouts")),
                  # Row 1: Minimum
                  tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 8px 0;",
                    tags$span(style = "width: 70px; text-align: right; color: #666;", "Minimum:"),
                    tags$input(type = "number", id = ns("bout_min"), value = "10",
                               min = "1", max = "60", step = "1",
                               style = "width: 60px !important; height: 28px; padding: 4px 6px; text-align: center; border: 1px solid #ccc; border-radius: 4px;"),
                    tags$span(style = "color: #666; font-size: 13px;", "minutes")
                  ),
                  # Row 2: Rule
                  tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 8px 0;",
                    tags$span(style = "width: 70px; text-align: right; color: #666;", "Rule:"),
                    tags$select(id = ns("bout_rule"),
                                style = "width: 60px !important; height: 28px; padding: 2px 6px; border: 1px solid #ccc; border-radius: 4px;",
                      tags$option(value = "80pct", selected = "selected", "80%"),
                      tags$option(value = "consecutive", "Strict")
                    )
                  )
                )
              ),

              # Sedentary Analysis
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("couch"), "Sedentary Analysis"
                ),

                # Length section
                tags$div(style = "font-weight: 600; font-size: 13px; margin: 5px 0 8px 0;", "Length"),
                # Row 1: Minimum
                tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 6px 0;",
                  tags$span(style = "width: 70px; text-align: right; color: #666;", "Minimum:"),
                  tags$input(type = "number", id = ns("sed_min_length"), value = "10",
                             min = "1", max = "60", step = "1",
                             style = "width: 60px !important; height: 28px; padding: 4px 6px; text-align: center; border: 1px solid #ccc; border-radius: 4px;"),
                  tags$span(style = "color: #666; font-size: 13px;", "minutes")
                ),
                # Row 2: Drop Time
                tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 6px 0;",
                  tags$span(style = "width: 70px; text-align: right; color: #666;", "Drop Time:"),
                  tags$input(type = "number", id = ns("sed_drop_time"), value = "2",
                             min = "0", max = "10", step = "1",
                             style = "width: 60px !important; height: 28px; padding: 4px 6px; text-align: center; border: 1px solid #ccc; border-radius: 4px;"),
                  tags$span(style = "color: #666; font-size: 13px;", "minutes")
                ),

                # Count Levels section
                tags$div(style = "font-weight: 600; font-size: 13px; margin: 12px 0 8px 0;", "Count Levels"),
                # Row 3: Maximum
                tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 6px 0;",
                  tags$span(style = "width: 70px; text-align: right; color: #666;", "Maximum:"),
                  tags$input(type = "number", id = ns("sed_threshold"), value = "200",
                             min = "50", max = "500", step = "25",
                             style = "width: 60px !important; height: 28px; padding: 4px 6px; text-align: center; border: 1px solid #ccc; border-radius: 4px;"),
                  tags$span(style = "color: #666; font-size: 13px;", "per minute")
                ),

                # Checkboxes
                tags$div(style = "margin-top: 12px;",
                  checkboxInput(ns("sed_use_vm"), "Use Vector Magnitude (if available)", value = TRUE),
                  checkboxInput(ns("sed_ignore_first"), "Ignore First Sedentary Break of Each Day", value = FALSE)
                )
              ),

              # Age-based auto-select
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("magic"), "Auto-Select"
                ),
                checkboxInput(ns("auto_cutpoints"), "Age-based cut-points", value = FALSE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("auto_cutpoints")),
                  numericInput(ns("participant_age"), "Age (years):",
                    value = 35, min = 1, max = 120, step = 1)
                )
              )
            )
          )
        ),

        # Export Panel
        div(class = "controls-panel",
          div(class = "controls-header-title mb-3",
            icon("download"), "Export Data"
          ),
          div(class = "export-row",
            downloadButton(ns("export_summary"), span(icon("file-csv"), " Summary"),
              class = "btn-primary"),
            downloadButton(ns("export_daily"), span(icon("calendar"), " Daily"),
              class = "btn-info")
          ),
          div(class = "export-row mt-2",
            downloadButton(ns("export_hourly"), span(icon("clock"), " Hourly"),
              class = "btn-default"),
            downloadButton(ns("export_sedentary"), span(icon("couch"), " Sedentary"),
              class = "btn-warning")
          )
        )
      ),

      # Right: Charts and Results (wide)
      column(width = 9,
        # HERO CHART: Activity Intensity
        div(class = "hero-chart-container",
          div(class = "hero-chart-header",
            div(class = "hero-chart-title",
              icon("chart-area"), "Activity Intensity Distribution"
            ),
            div(class = "hero-chart-controls",
              uiOutput(ns("chart_status_badge")),
              selectInput(ns("selected_participant"), NULL,
                choices = c("All Participants" = "all"),
                width = "180px")
            )
          ),
          conditionalPanel(
            condition = "output.has_activity_results == false",
            ns = ns,
            chart_empty_state(
              title = "No Activity Data",
              message = "Click 'Run Analysis' to classify activity intensity levels",
              show_icon = FALSE
            )
          ),
          conditionalPanel(
            condition = "output.has_activity_results == true",
            ns = ns,
            plotOutput(ns("intensity_plot"), height = "420px")
          )
        ),

        # Tabbed Results Section
        div(class = "hero-chart-container results-tabs",
          tabsetPanel(
            id = ns("results_tabs"),
            type = "tabs",

            # Hourly Pattern Tab
            tabPanel(
              title = "Hourly Pattern",
              value = "hourly",
              div(class = "pt-4",
                conditionalPanel(
                  condition = "output.has_activity_results == false",
                  ns = ns,
                  chart_empty_state(
                    title = "Hourly Pattern",
                    message = "Run Analysis to see hourly activity patterns",
                    show_icon = FALSE,
                    extra_class = "chart-empty-state--spacious"
                  )
                ),
                conditionalPanel(
                  condition = "output.has_activity_results == true",
                  ns = ns,
                  plotOutput(ns("hourly_plot"), height = "300px")
                )
              )
            ),

            # Daily Summary Tab
            tabPanel(
              title = "Daily Summary",
              value = "daily",
              div(class = "pt-4",
                DT::dataTableOutput(ns("daily_table"))
              )
            ),

            # Detailed Results Tab
            tabPanel(
              title = "Summary Table",
              value = "summary",
              div(class = "pt-4",
                DT::dataTableOutput(ns("summary_table"))
              )
            ),

            # Files Tab
            tabPanel(
              title = "Files",
              value = "files",
              div(class = "pt-4",
                DT::dataTableOutput(ns("files_table"))
              )
            ),

            # VM Heatmap Tab
            tabPanel(
              title = "VM Heatmap",
              value = "vm_heatmap",
              div(class = "pt-4",
                conditionalPanel(
                  condition = "output.has_activity_results == false",
                  ns = ns,
                  chart_empty_state(
                    title = "Vector Magnitude Heatmap",
                    message = "Run Analysis to see activity heatmap",
                    show_icon = FALSE
                  )
                ),
                conditionalPanel(
                  condition = "output.has_activity_results == true",
                  ns = ns,
                  plotOutput(ns("vm_heatmap_plot"), height = "400px")
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_activity_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Update participant selector when results change
    observe({
      res <- results()
      if (length(res) > 0) {
        choices <- c("All Participants" = "all")
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
          choices = c("All Participants" = "all"),
          selected = "all")
      }
    })

    # Output for conditional panel
    output$has_activity_results <- reactive({
      length(results()) > 0
    })
    outputOptions(output, "has_activity_results", suspendWhenHidden = FALSE)

    # Files count
    output$files_count <- renderText({
      as.character(shared$file_count)
    })

    # Compact metrics (reactive to participant selection) - use daily data for consistency
    output$metric_sedentary <- renderText({
      res <- results()
      sel <- input$selected_participant
      if (is.null(res) || length(res) == 0 || is.null(sel)) return("--")
      if (sel == "all") {
        # Simple average of each participant's average (not weighted by days)
        participant_avgs <- c()
        for (r in res) {
          if (!is.null(r$daily) && "sedentary_hrs" %in% names(r$daily)) {
            participant_avgs <- c(participant_avgs, mean(r$daily$sedentary_hrs, na.rm = TRUE))
          }
        }
        if (length(participant_avgs) == 0) return("--")
        avg <- mean(participant_avgs, na.rm = TRUE)
        if (is.na(avg)) return("--")
        paste0(round(avg, 1), "h")
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        if (!is.null(r$daily) && "sedentary_hrs" %in% names(r$daily)) {
          avg <- mean(r$daily$sedentary_hrs, na.rm = TRUE)
          if (is.na(avg)) return("--")
          paste0(round(avg, 1), "h")
        } else {
          "--"
        }
      } else {
        "--"
      }
    })

    output$metric_light <- renderText({
      res <- results()
      sel <- input$selected_participant
      if (is.null(res) || length(res) == 0 || is.null(sel)) return("--")
      if (sel == "all") {
        # Simple average of each participant's average (not weighted by days)
        participant_avgs <- c()
        for (r in res) {
          if (!is.null(r$daily) && "light_hrs" %in% names(r$daily)) {
            participant_avgs <- c(participant_avgs, mean(r$daily$light_hrs, na.rm = TRUE))
          }
        }
        if (length(participant_avgs) == 0) return("--")
        avg <- mean(participant_avgs, na.rm = TRUE)
        if (is.na(avg)) return("--")
        paste0(round(avg, 1), "h")
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        if (!is.null(r$daily) && "light_hrs" %in% names(r$daily)) {
          avg <- mean(r$daily$light_hrs, na.rm = TRUE)
          if (is.na(avg)) return("--")
          paste0(round(avg, 1), "h")
        } else {
          "--"
        }
      } else {
        "--"
      }
    })

    output$metric_mvpa <- renderText({
      res <- results()
      sel <- input$selected_participant
      if (is.null(res) || length(res) == 0 || is.null(sel)) return("--")
      if (sel == "all") {
        # Simple average of each participant's average (not weighted by days)
        participant_avgs <- c()
        for (r in res) {
          if (!is.null(r$daily)) {
            mod_hrs <- if ("moderate_hrs" %in% names(r$daily)) r$daily$moderate_hrs else 0
            vig_hrs <- if ("vigorous_hrs" %in% names(r$daily)) r$daily$vigorous_hrs else 0
            vvig_hrs <- if ("very_vigorous_hrs" %in% names(r$daily)) r$daily$very_vigorous_hrs else 0
            mvpa_min <- (mod_hrs + vig_hrs + vvig_hrs) * 60
            participant_avgs <- c(participant_avgs, mean(mvpa_min, na.rm = TRUE))
          }
        }
        if (length(participant_avgs) == 0) return("--")
        avg <- mean(participant_avgs, na.rm = TRUE)
        if (is.na(avg)) return("--")
        paste0(round(avg), "m")
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        if (!is.null(r$daily)) {
          mod_hrs <- if ("moderate_hrs" %in% names(r$daily)) r$daily$moderate_hrs else 0
          vig_hrs <- if ("vigorous_hrs" %in% names(r$daily)) r$daily$vigorous_hrs else 0
          vvig_hrs <- if ("very_vigorous_hrs" %in% names(r$daily)) r$daily$very_vigorous_hrs else 0
          mvpa_min <- (mod_hrs + vig_hrs + vvig_hrs) * 60
          avg <- mean(mvpa_min, na.rm = TRUE)
          if (is.na(avg)) return("--")
          paste0(round(avg), "m")
        } else {
          "--"
        }
      } else {
        "--"
      }
    })

    output$metric_steps <- renderText({
      res <- results()
      sel <- input$selected_participant
      if (is.null(res) || length(res) == 0 || is.null(sel)) return("--")
      if (sel == "all") {
        step_values <- sapply(names(shared$files), function(fid) {
          f <- shared$files[[fid]]
          if ("steps" %in% names(f$data)) {
            total_steps <- sum(f$data$steps, na.rm = TRUE)
            n_days <- if ("timestamp" %in% names(f$data)) length(unique(as.Date(f$data$timestamp))) else 1
            return(total_steps / n_days)
          }
          return(NA)
        })
        avg <- mean(step_values, na.rm = TRUE)
        if (is.na(avg)) return("--")
        formatC(round(avg), format = "d", big.mark = ",")
      } else if (sel %in% names(res)) {
        fid <- sel
        f <- shared$files[[fid]]
        if (!is.null(f) && "steps" %in% names(f$data)) {
          total_steps <- sum(f$data$steps, na.rm = TRUE)
          n_days <- if ("timestamp" %in% names(f$data)) length(unique(as.Date(f$data$timestamp))) else 1
          avg <- total_steps / n_days
          formatC(round(avg), format = "d", big.mark = ",")
        } else {
          "--"
        }
      } else {
        "--"
      }
    })

    # Wear time status indicator
    output$wear_time_status <- renderUI({
      wt_available <- length(shared$results$wear_time) > 0
      if (wt_available) {
        status_badge("Wear Time Ready", "success")
      } else {
        status_badge("No Wear Time", "caution")
      }
    })

    # Chart status badge
    output$chart_status_badge <- renderUI({
      res <- results()
      if (length(res) > 0) {
        algo <- res[[1]]$parameters$cut_points %||% "freedson"
        tags$span(class = "status-indicator status-info",
          icon("check"), paste("Algorithm:", algo)
        )
      } else {
        NULL
      }
    })

    # Files table (compact)
    output$files_table <- DT::renderDataTable({
      if (shared$file_count == 0) {
        return(DT::datatable(
          data.frame(Message = "No files loaded. Go to Data Upload tab."),
          rownames = FALSE, options = list(dom = 't')
        ))
      }

      res <- results()
      wt_res <- shared$results$wear_time

      df <- data.frame(
        Subject = sapply(shared$files, function(f) f$subject_info$id %||% "N/A"),
        Serial = sapply(shared$files, function(f) f$device_info$serial_number %||% "N/A"),
        Validated = sapply(names(shared$files), function(fid) {
          if (fid %in% names(wt_res)) "Yes" else "No"
        }),
        Status = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) "Scored" else "Pending"
        }),
        Sedentary = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$sedentary_min / 60, 1), "h") else "-"
        }),
        Light = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$light_min / 60, 1), "h") else "-"
        }),
        MVPA = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$mvpa_min), "m") else "-"
        }),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        selection = "multiple",
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE
      ) %>%
        DT::formatStyle("Status",
          color = DT::styleEqual(c("Pending", "Scored"), c("#f59e0b", "#10b981")),
          fontWeight = "bold")
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
          selected_algo <- if (input$auto_cutpoints) {
            age <- input$participant_age %||% f$subject_info$age %||% 35
            if (age < 5) "pate_preschool"
            else if (age < 18) "evenson"
            else if (age >= 65) "copeland_older"
            else "freedson"
          } else {
            input$cut_points
          }

          # Apply mask to get valid data
          valid_data <- activity_data[analysis_mask]

          # Apply cut-points using unified function
          intensity <- tryCatch({
            canhrActi::apply_cutpoints(
              data = valid_data,
              algorithm = selected_algo,
              epoch_seconds = epoch_length,
              age = input$participant_age %||% f$subject_info$age
            )
          }, error = function(e) {
            showNotification(paste0("Could not score activity for ", f$name, " - check data format"), type = "error")
            return(NULL)
          })

          algo_used <- selected_algo

          if (is.null(intensity)) next

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
              showNotification(paste("MVPA bout detection failed for", f$name, ":", e$message), type = "warning", duration = 5)
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
              showNotification(paste("Sedentary fragmentation failed for", f$name, ":", e$message), type = "warning", duration = 5)
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
              showNotification(paste("METs calculation failed for", f$name, ":", e$message), type = "warning", duration = 5)
              NULL
            })

            if (!is.null(mets)) {
              mets_valid <- mets[analysis_mask]
              avg_mets <- mean(mets_valid, na.rm = TRUE)
            }
          }

          # Energy expenditure
          total_ee <- NA
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
              showNotification(paste("Energy expenditure calculation failed for", f$name, ":", e$message), type = "warning", duration = 5)
              NULL
            })
            if (!is.null(ee)) total_ee <- ee$total_kcal
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
              auto_cutpoints = input$auto_cutpoints,
              participant_age = input$participant_age,
              mets_algo = input$mets_algo,
              ee_algo = input$ee_algo,
              exclude_nonwear = input$exclude_nonwear
            )
          )
        }

        gc(verbose = FALSE)
      })

      results(all_results)
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

      showNotification(paste(length(all_results), "files scored"), type = "message")
    })

    # Clear results
    observeEvent(input$clear_results, {
      results(list())
      shared$results$activity <- NULL
      shared$results$sedentary_bouts <- NULL
      showNotification("Activity results cleared.", type = "message")
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run Analysis to see results"), rownames = FALSE))
      }

      df <- data.frame(
        Subject = sapply(res, function(r) r$subject_id),
        Days = sapply(res, function(r) r$n_days),
        `Sedentary (h)` = sapply(res, function(r) round(r$sedentary_min / 60, 1)),
        `Light (h)` = sapply(res, function(r) round(r$light_min / 60, 1)),
        `Moderate (min)` = sapply(res, function(r) round(r$moderate_min)),
        `Vigorous (min)` = sapply(res, function(r) round(r$vigorous_min)),
        `MVPA (min)` = sapply(res, function(r) round(r$mvpa_min)),
        `Avg METs` = sapply(res, function(r) if (!is.na(r$avg_mets)) round(r$avg_mets, 2) else NA),
        `MVPA Bouts` = sapply(res, function(r) r$n_bouts),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = 'tip'), rownames = FALSE)
    })

    # Daily table
    output$daily_table <- DT::renderDataTable({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run Analysis to see results"), rownames = FALSE))
      }

      all_daily <- list()
      for (r in res) {
        if (!is.null(r$daily) && nrow(r$daily) > 0) {
          d <- r$daily
          d$Subject <- r$subject_id
          all_daily[[length(all_daily) + 1]] <- d
        }
      }

      if (length(all_daily) == 0) {
        return(DT::datatable(data.frame(Message = "No daily data available"), rownames = FALSE))
      }

      df <- do.call(rbind, all_daily)
      df$date <- as.character(df$date)
      df$weekday <- weekdays(as.Date(df$date))

      DT::datatable(
        df[, c("Subject", "date", "weekday", "analyzed_hours", "sedentary", "light", "moderate", "vigorous")],
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE,
        colnames = c("Subject", "Date", "Day", "Hours", "Sedentary", "Light", "Moderate", "Vigorous")
      )
    })

    # HERO CHART: Intensity plot (larger, more prominent)
    output$intensity_plot <- renderPlot({
      res <- results()
      sel <- input$selected_participant

      # User-friendly empty state messaging
      validate(
        need(length(res) > 0,
             "No data yet")
      )

      # Filter results based on selection
      if (!is.null(sel) && sel != "all" && sel %in% names(res)) {
        res <- list(res[[sel]])
      }

      # Use daily data (source of truth) - sum hours across all days
      total_sedentary <- 0
      total_light <- 0
      total_moderate <- 0
      total_vigorous <- 0
      total_very_vigorous <- 0
      total_days <- 0

      for (r in res) {
        daily <- r$daily
        if (!is.null(daily) && nrow(daily) > 0) {
          total_days <- total_days + nrow(daily)
          total_sedentary <- total_sedentary + sum(daily$sedentary_hrs, na.rm = TRUE)
          total_light <- total_light + sum(daily$light_hrs, na.rm = TRUE)
          total_moderate <- total_moderate + sum(daily$moderate_hrs, na.rm = TRUE)
          total_vigorous <- total_vigorous + sum(daily$vigorous_hrs, na.rm = TRUE)
          total_very_vigorous <- total_very_vigorous + sum(daily$very_vigorous_hrs, na.rm = TRUE)
        }
      }

      validate(
        need(total_days > 0,
             "No valid data found")
      )

      total_hours <- total_sedentary + total_light + total_moderate + total_vigorous + total_very_vigorous

      df <- data.frame(
        intensity = factor(c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
          levels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")),
        hours = c(total_sedentary, total_light, total_moderate, total_vigorous, total_very_vigorous),
        pct = c(total_sedentary, total_light, total_moderate, total_vigorous, total_very_vigorous) / total_hours * 100
      )

      colors <- c("Sedentary" = "#94a3b8", "Light" = "#3b82f6", "Moderate" = "#f59e0b",
                  "Vigorous" = "#f97316", "Very Vigorous" = "#ef4444")

      ggplot2::ggplot(df, ggplot2::aes(x = intensity, y = hours, fill = intensity)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1fh\n(%.1f%%)", hours, pct)),
          vjust = -0.3, size = 4, fontface = "bold", color = "#1e293b") +
        ggplot2::scale_fill_manual(values = colors, guide = "none") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(x = NULL, y = "Total Hours") +
        canhrActi::theme_canhrActi() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 14, face = "bold", color = "#334155"),
          axis.text.y = ggplot2::element_text(size = 13, color = "#64748b"),
          axis.title.y = ggplot2::element_text(size = 14, color = "#64748b", margin = ggplot2::margin(r = 10))
        )
    })

    # Hourly pattern plot
    output$hourly_plot <- renderPlot({
      res <- results()
      sel <- input$selected_participant

      # User-friendly empty state messaging
      validate(
        need(length(res) > 0,
             "No hourly data")
      )

      # Filter results based on selection
      if (!is.null(sel) && sel != "all" && sel %in% names(res)) {
        res <- list(res[[sel]])
      }

      all_hourly <- data.frame()
      for (r in res) {
        if (!is.null(r$hourly)) {
          h <- r$hourly
          h$subject <- r$subject_id
          all_hourly <- rbind(all_hourly, h)
        }
      }

      validate(
        need(nrow(all_hourly) > 0,
             "Could not compute hourly patterns")
      )

      avg_hourly <- aggregate(counts ~ hour, all_hourly, mean, na.rm = TRUE)

      ggplot2::ggplot(avg_hourly, ggplot2::aes(x = hour, y = counts)) +
        ggplot2::geom_area(fill = "#3b82f6", alpha = 0.2) +
        ggplot2::geom_line(color = "#3b82f6", linewidth = 1.5) +
        ggplot2::geom_point(color = "#3b82f6", size = 2) +
        ggplot2::scale_x_continuous(breaks = seq(0, 23, 2),
          labels = sprintf("%02d:00", seq(0, 23, 2))) +
        ggplot2::labs(x = "Hour of Day", y = "Mean Activity Counts") +
        canhrActi::theme_canhrActi() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = 13, color = "#64748b"),
          axis.title = ggplot2::element_text(size = 14, color = "#64748b")
        )
    })

    # Export handlers
    output$export_summary <- downloadHandler(
      filename = function() {
        paste0("canhrActi_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
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
          weight <- f$subject_info$weight_lbs %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% ""
          epoch_sec <- f$epoch_length

          # Get algorithm
          algo <- r$parameters$cut_points %||% "freedson"

          # Get wear time mask AND daily validation
          wear_result <- shared$results$wear_time[[r$file_id]]
          wear_mask <- if (!is.null(wear_result) && !is.null(wear_result$wear)) {
            wear_result$wear
          } else {
            rep(TRUE, nrow(data))  # Default: assume all worn if no wear time analysis
          }

          # Get daily validation info and apply DAY-LEVEL validation
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

          # Get intensity and apply wear time mask
          intensity <- r$intensity_valid
          # Ensure non-wear epochs are NA (may already be, but double-check)
          if (length(intensity) == length(wear_mask)) {
            intensity[!wear_mask] <- NA
          }

          n_epochs <- length(intensity)
          # Count WEAR TIME epochs for percentage calculations
          n_wear_epochs <- sum(!is.na(intensity))

          # Axis data
          axis1 <- data$axis1
          axis2 <- if ("axis2" %in% names(data)) data$axis2 else rep(0, nrow(data))
          axis3 <- if ("axis3" %in% names(data)) data$axis3 else rep(0, nrow(data))
          steps <- if ("steps" %in% names(data)) data$steps else rep(0, nrow(data))
          lux <- if ("lux" %in% names(data)) data$lux else rep(0, nrow(data))
          vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

          # Calendar days and hours (only count hours WITH wear time)
          n_days <- r$n_days
          n_hours <- if ("timestamp" %in% names(data) && sum(wear_mask) > 0) {
            length(unique(paste(as.Date(data$timestamp[wear_mask]), format(data$timestamp[wear_mask], "%H"))))
          } else if (n_wear_epochs > 0) {
            n_wear_epochs * epoch_sec / 3600
          } else 0

          # Intensity counts (NA values excluded automatically with na.rm = TRUE)
          sedentary <- sum(intensity == "sedentary", na.rm = TRUE)
          light <- sum(intensity == "light", na.rm = TRUE)
          moderate <- sum(intensity == "moderate", na.rm = TRUE)
          vigorous <- sum(intensity == "vigorous", na.rm = TRUE)
          very_vigorous <- sum(intensity == "very_vigorous", na.rm = TRUE)
          total_mvpa <- moderate + vigorous + very_vigorous

          # Percentages based on WEAR TIME epochs only
          # If no wear time, all percentages are 0
          pct_sed <- if (n_wear_epochs > 0) 100 * sedentary / n_wear_epochs else 0
          pct_light <- if (n_wear_epochs > 0) 100 * light / n_wear_epochs else 0
          pct_mod <- if (n_wear_epochs > 0) 100 * moderate / n_wear_epochs else 0
          pct_vig <- if (n_wear_epochs > 0) 100 * vigorous / n_wear_epochs else 0
          pct_vvig <- if (n_wear_epochs > 0) 100 * very_vigorous / n_wear_epochs else 0
          pct_mvpa <- if (n_wear_epochs > 0) 100 * total_mvpa / n_wear_epochs else 0

          # Average MVPA per day
          avg_mvpa_per_day <- if (n_days > 0) total_mvpa / n_days else 0

          # Energy calculations
          kcals <- 0
          mets_avg <- 1
          if (!is.null(r$mets) && length(r$mets) > 0) {
            mets_avg <- mean(r$mets, na.rm = TRUE)
            weight_kg <- weight * 0.453592
            time_hours <- n_epochs * epoch_sec / 3600
            kcals <- mets_avg * weight_kg * time_hours
          } else if (!is.na(r$total_ee)) {
            kcals <- r$total_ee
          }
          avg_kcals_per_day <- if (n_days > 0) kcals / n_days else 0
          avg_kcals_per_hour <- if (n_hours > 0) kcals / n_hours else 0

          # MVPA Bout detection and statistics
          is_mvpa <- intensity %in% c("moderate", "vigorous", "very_vigorous")
          mvpa_bouts <- rle(is_mvpa)
          bout_starts <- cumsum(c(1, head(mvpa_bouts$lengths, -1)))
          bout_ends <- cumsum(mvpa_bouts$lengths)

          bout_info <- data.frame(
            start = bout_starts[mvpa_bouts$values],
            end = bout_ends[mvpa_bouts$values],
            length = mvpa_bouts$lengths[mvpa_bouts$values]
          )
          bout_min_epochs <- as.numeric(input$bout_min %||% 10) * (60 / epoch_sec)
          bout_info <- bout_info[bout_info$length >= bout_min_epochs, ]

          n_mvpa_bouts <- nrow(bout_info)
          total_mvpa_bout_time <- if (n_mvpa_bouts > 0) sum(bout_info$length) else 0
          avg_mvpa_bout_time <- if (n_mvpa_bouts > 0) mean(bout_info$length) else 0
          max_mvpa_bout_time <- if (n_mvpa_bouts > 0) max(bout_info$length) else 0
          min_mvpa_bout_time <- if (n_mvpa_bouts > 0) min(bout_info$length) else 0

          # Total counts in MVPA bouts
          total_mvpa_bout_counts <- 0
          if (n_mvpa_bouts > 0) {
            for (b in seq_len(nrow(bout_info))) {
              total_mvpa_bout_counts <- total_mvpa_bout_counts + sum(axis1[bout_info$start[b]:bout_info$end[b]], na.rm = TRUE)
            }
          }

          # Sedentary bout detection and statistics
          # Handle NA values from non-wear epochs
          is_sed <- !is.na(intensity) & intensity == "sedentary"
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

          n_sed_bouts <- nrow(sed_bout_info)
          total_sed_bout_time <- if (n_sed_bouts > 0) sum(sed_bout_info$length) else 0
          avg_sed_bout_length <- if (n_sed_bouts > 0) mean(sed_bout_info$length) else 0
          max_sed_bout_length <- if (n_sed_bouts > 0) max(sed_bout_info$length) else 0
          min_sed_bout_length <- if (n_sed_bouts > 0) min(sed_bout_info$length) else 0
          daily_avg_sed_bouts <- if (n_days > 0) n_sed_bouts / n_days else 0

          # Sedentary break detection and statistics
          # Only count breaks during valid wear time
          is_break <- !is.na(intensity) & intensity != "sedentary"
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

          n_breaks <- nrow(break_bout_info)
          total_break_time <- if (n_breaks > 0) sum(break_bout_info$length) else 0
          avg_break_length <- if (n_breaks > 0) mean(break_bout_info$length) else 0
          max_break_length <- if (n_breaks > 0) max(break_bout_info$length) else 0
          min_break_length <- if (n_breaks > 0) min(break_bout_info$length) else 0
          daily_avg_breaks <- if (n_days > 0) n_breaks / n_days else 0

          # Only use WEAR TIME epochs for count statistics
          if (n_wear_epochs > 0) {
            # Filter data by wear mask
            w_axis1 <- axis1[wear_mask]
            w_axis2 <- axis2[wear_mask]
            w_axis3 <- axis3[wear_mask]
            w_steps <- steps[wear_mask]
            w_lux <- lux[wear_mask]
            w_vm <- vm[wear_mask]

            # Axis statistics - wear time only
            axis1_counts <- sum(w_axis1, na.rm = TRUE)
            axis2_counts <- sum(w_axis2, na.rm = TRUE)
            axis3_counts <- sum(w_axis3, na.rm = TRUE)

            axis1_avg <- mean(w_axis1, na.rm = TRUE)
            axis2_avg <- mean(w_axis2, na.rm = TRUE)
            axis3_avg <- mean(w_axis3, na.rm = TRUE)

            axis1_max <- max(w_axis1, na.rm = TRUE)
            axis2_max <- max(w_axis2, na.rm = TRUE)
            axis3_max <- max(w_axis3, na.rm = TRUE)

            axis1_cpm <- axis1_avg * (60 / epoch_sec)
            axis2_cpm <- axis2_avg * (60 / epoch_sec)
            axis3_cpm <- axis3_avg * (60 / epoch_sec)

            # Vector magnitude statistics - wear time only
            vm_counts <- sum(w_vm, na.rm = TRUE)
            vm_avg <- mean(w_vm, na.rm = TRUE)
            vm_max <- max(w_vm, na.rm = TRUE)
            vm_cpm <- vm_avg * (60 / epoch_sec)

            # Steps statistics - wear time only
            steps_counts <- sum(w_steps, na.rm = TRUE)
            steps_avg <- mean(w_steps, na.rm = TRUE)
            steps_max <- max(w_steps, na.rm = TRUE)
            steps_per_min <- steps_avg * (60 / epoch_sec)

            # Lux statistics - wear time only
            lux_avg <- mean(w_lux, na.rm = TRUE)
            lux_max <- max(w_lux, na.rm = TRUE)

            # Time in minutes - wear time only
            time_min <- n_wear_epochs * epoch_sec / 60
          } else {
            # No wear time - all metrics are 0
            axis1_counts <- axis2_counts <- axis3_counts <- 0
            axis1_avg <- axis2_avg <- axis3_avg <- 0
            axis1_max <- axis2_max <- axis3_max <- 0
            axis1_cpm <- axis2_cpm <- axis3_cpm <- 0
            vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
            steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
            lux_avg <- lux_max <- 0
            time_min <- 0
          }

          row_data <- data.frame(
            Subject = r$subject_id,
            Filename = r$name,
            Epoch = epoch_sec,
            `Weight (lbs)` = weight,
            Age = age,
            Gender = gender,
            kcals = round(kcals, 3),
            `Average kcals per day` = round(avg_kcals_per_day, 3),
            `Average kcals per hour` = round(avg_kcals_per_hour, 3),
            METs = round(mets_avg, 3),
            # MVPA Bout statistics
            `MVPA Bouts` = n_mvpa_bouts,
            `Total Time in MVPA Bouts` = total_mvpa_bout_time,
            `Avg Time per MVPA Bout` = round(avg_mvpa_bout_time, 1),
            `Max Time per MVPA Bout` = max_mvpa_bout_time,
            `Min Time per MVPA Bout` = min_mvpa_bout_time,
            `Total Counts in MVPA Bouts` = total_mvpa_bout_counts,
            # Sedentary Bout statistics
            `Total Sedentary Bouts` = n_sed_bouts,
            `Total Time in Sedentary Bouts` = total_sed_bout_time,
            `Average Length of Sedentary Bouts` = round(avg_sed_bout_length, 1),
            `Maximum Length of Sedentary Bouts` = max_sed_bout_length,
            `Minimum Length of Sedentary Bouts` = min_sed_bout_length,
            `Daily Average of Sedentary Bouts` = round(daily_avg_sed_bouts, 1),
            # Sedentary Break statistics
            `Total Sedentary Breaks` = n_breaks,
            `Total Time in Sedentary Breaks` = total_break_time,
            `Average length of Sedentary Breaks` = round(avg_break_length, 1),
            `Max Length of Sedentary Breaks` = max_break_length,
            `Minimum Length of Sedentary Breaks` = min_break_length,
            `Daily Average of Sedentary Breaks` = round(daily_avg_breaks, 1),
            # Intensity counts
            Sedentary = sedentary,
            Light = light,
            Moderate = moderate,
            Vigorous = vigorous,
            `Very Vigorous` = very_vigorous,
            # Percentages
            `% in Sedentary` = sprintf("%.2f%%", pct_sed),
            `% in Light` = sprintf("%.2f%%", pct_light),
            `% in Moderate` = sprintf("%.2f%%", pct_mod),
            `% in Vigorous` = sprintf("%.2f%%", pct_vig),
            `% in Very Vigorous` = sprintf("%.2f%%", pct_vvig),
            `Total MVPA` = total_mvpa,
            `% in MVPA` = sprintf("%.2f%%", pct_mvpa),
            `Average MVPA Per day` = round(avg_mvpa_per_day, 1),
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
            # Metadata (using wear time epochs)
            `Number of Epochs` = n_wear_epochs,
            Time = round(time_min),
            `Calendar Days` = n_days,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          all_rows[[length(all_rows) + 1]] <- row_data
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    output$export_daily <- downloadHandler(
      filename = function() {
        paste0("canhrActi_Daily_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
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

          # Get algorithm name
          algo <- r$parameters$cut_points %||% "freedson"

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

            # Apply day-level validation - exclude invalid days entirely
            if (!is.null(daily_valid)) {
              for (d in seq_len(nrow(daily_valid))) {
                if (!daily_valid$valid[d]) {
                  # This day is INVALID - exclude all epochs
                  day_date <- as.Date(daily_valid$date[d])
                  wear_mask[data$date == day_date] <- FALSE
                }
              }
            }

            # Pre-calculate for all data
            axis1 <- data$axis1
            axis2 <- if ("axis2" %in% names(data)) data$axis2 else rep(0, nrow(data))
            axis3 <- if ("axis3" %in% names(data)) data$axis3 else rep(0, nrow(data))
            steps <- if ("steps" %in% names(data)) data$steps else rep(0, nrow(data))
            lux <- if ("lux" %in% names(data)) data$lux else rep(0, nrow(data))

            vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

            # Apply wear time mask BEFORE calculating intensity
            # Non-wear epochs should be NA, not classified as sedentary
            axis1_wear <- axis1
            axis1_wear[!wear_mask] <- NA

            all_cpm <- canhrActi::to_cpm(axis1_wear, epoch_sec)
            all_intensity <- tryCatch({
              canhrActi::apply_cutpoints(all_cpm, algo, epoch_sec)
            }, error = function(e) rep(NA_character_, nrow(data)))
            # Explicitly set non-wear to NA
            all_intensity[!wear_mask] <- NA

            # Detect MVPA bouts for the full dataset
            is_mvpa <- all_intensity %in% c("moderate", "vigorous", "very_vigorous")
            mvpa_bouts <- rle(is_mvpa)
            bout_starts <- cumsum(c(1, head(mvpa_bouts$lengths, -1)))
            bout_ends <- cumsum(mvpa_bouts$lengths)

            bout_info <- data.frame(
              start = bout_starts[mvpa_bouts$values],
              end = bout_ends[mvpa_bouts$values],
              length = mvpa_bouts$lengths[mvpa_bouts$values]
            )
            bout_min_epochs <- as.numeric(input$bout_min %||% 10) * (60 / epoch_sec)
            bout_info <- bout_info[bout_info$length >= bout_min_epochs, ]

            # Detect sedentary bouts
            # Handle NA values from non-wear epochs
            # NA == "sedentary" returns NA (not FALSE), which breaks rle()
            is_sed <- !is.na(all_intensity) & all_intensity == "sedentary"
            sed_bouts_rle <- rle(is_sed)
            sed_bout_starts <- cumsum(c(1, head(sed_bouts_rle$lengths, -1)))
            sed_bout_ends <- cumsum(sed_bouts_rle$lengths)
            # Only include TRUE values (actual sedentary bouts)
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
            # Only count breaks during valid wear time
            is_break <- !is.na(all_intensity) & all_intensity != "sedentary"
            break_bouts_rle <- rle(is_break)
            break_bout_starts <- cumsum(c(1, head(break_bouts_rle$lengths, -1)))
            break_bout_ends <- cumsum(break_bouts_rle$lengths)
            # Only include TRUE values (actual sedentary breaks)
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
              day_indices <- which(data$date == date_i)
              day_data <- data[day_indices, ]
              n_epochs <- nrow(day_data)
              if (n_epochs == 0) next

              # Get wear time for this specific day
              day_wear <- wear_mask[day_indices]
              n_wear_epochs <- sum(day_wear, na.rm = TRUE)

              # Get day data
              d_axis1 <- day_data$axis1
              d_axis2 <- if ("axis2" %in% names(day_data)) day_data$axis2 else rep(0, n_epochs)
              d_axis3 <- if ("axis3" %in% names(day_data)) day_data$axis3 else rep(0, n_epochs)
              d_steps <- if ("steps" %in% names(day_data)) day_data$steps else rep(0, n_epochs)
              d_lux <- if ("lux" %in% names(day_data)) day_data$lux else rep(0, n_epochs)
              d_vm <- sqrt(d_axis1^2 + d_axis2^2 + d_axis3^2)

              # Get intensity for this day (NA for non-wear epochs)
              day_intensity <- all_intensity[day_indices]

              # Intensity counts - only count wear time epochs (NA excluded automatically)
              sedentary <- sum(day_intensity == "sedentary", na.rm = TRUE)
              light <- sum(day_intensity == "light", na.rm = TRUE)
              moderate <- sum(day_intensity == "moderate", na.rm = TRUE)
              vigorous <- sum(day_intensity == "vigorous", na.rm = TRUE)
              very_vigorous <- sum(day_intensity == "very_vigorous", na.rm = TRUE)
              total_mvpa <- moderate + vigorous + very_vigorous

              # Percentages based on WEAR TIME epochs only
              # Days with 0 wear time get 0% for all categories
              pct_sed <- if (n_wear_epochs > 0) 100 * sedentary / n_wear_epochs else 0
              pct_light <- if (n_wear_epochs > 0) 100 * light / n_wear_epochs else 0
              pct_mod <- if (n_wear_epochs > 0) 100 * moderate / n_wear_epochs else 0
              pct_vig <- if (n_wear_epochs > 0) 100 * vigorous / n_wear_epochs else 0
              pct_vvig <- if (n_wear_epochs > 0) 100 * very_vigorous / n_wear_epochs else 0
              pct_mvpa <- if (n_wear_epochs > 0) 100 * total_mvpa / n_wear_epochs else 0

              # Hours in day (only hours with wear time)
              wear_hours <- if (n_wear_epochs > 0) {
                unique(as.numeric(format(day_data$timestamp[day_wear], "%H")))
              } else numeric(0)
              n_hours <- length(wear_hours)
              avg_mvpa_per_hour <- if (n_hours > 0) total_mvpa / n_hours else 0

              # MVPA Bout metrics for this day
              day_start <- min(day_indices)
              day_end <- max(day_indices)

              bouts_occurring <- if (nrow(bout_info) > 0) {
                bout_info[bout_info$start <= day_end & bout_info$end >= day_start, ]
              } else data.frame()
              n_bouts_occurring <- nrow(bouts_occurring)

              bouts_starting <- if (nrow(bout_info) > 0) {
                bout_info[bout_info$start >= day_start & bout_info$start <= day_end, ]
              } else data.frame()
              n_bouts_starting <- nrow(bouts_starting)

              bouts_ending <- if (nrow(bout_info) > 0) {
                bout_info[bout_info$end >= day_start & bout_info$end <= day_end, ]
              } else data.frame()
              n_bouts_ending <- nrow(bouts_ending)

              total_bout_time <- 0
              total_bout_counts <- 0
              if (nrow(bouts_occurring) > 0) {
                for (b in seq_len(nrow(bouts_occurring))) {
                  b_start <- max(bouts_occurring$start[b], day_start)
                  b_end <- min(bouts_occurring$end[b], day_end)
                  total_bout_time <- total_bout_time + (b_end - b_start + 1)
                  total_bout_counts <- total_bout_counts + sum(axis1[b_start:b_end], na.rm = TRUE)
                }
              }

              # Sedentary bout metrics
              sed_bouts_occurring <- if (nrow(sed_bout_info) > 0) {
                sed_bout_info[sed_bout_info$start <= day_end & sed_bout_info$end >= day_start, ]
              } else data.frame()
              n_sed_bouts_occurring <- nrow(sed_bouts_occurring)

              sed_bouts_starting <- if (nrow(sed_bout_info) > 0) {
                sed_bout_info[sed_bout_info$start >= day_start & sed_bout_info$start <= day_end, ]
              } else data.frame()
              n_sed_bouts_starting <- nrow(sed_bouts_starting)

              sed_bouts_ending <- if (nrow(sed_bout_info) > 0) {
                sed_bout_info[sed_bout_info$end >= day_start & sed_bout_info$end <= day_end, ]
              } else data.frame()
              n_sed_bouts_ending <- nrow(sed_bouts_ending)

              total_sed_bout_time <- 0
              if (nrow(sed_bouts_occurring) > 0) {
                for (b in seq_len(nrow(sed_bouts_occurring))) {
                  b_start <- max(sed_bouts_occurring$start[b], day_start)
                  b_end <- min(sed_bouts_occurring$end[b], day_end)
                  total_sed_bout_time <- total_sed_bout_time + (b_end - b_start + 1)
                }
              }

              # Sedentary break metrics
              break_bouts_occurring <- if (nrow(break_bout_info) > 0) {
                break_bout_info[break_bout_info$start <= day_end & break_bout_info$end >= day_start, ]
              } else data.frame()
              n_break_bouts_occurring <- nrow(break_bouts_occurring)

              break_bouts_starting <- if (nrow(break_bout_info) > 0) {
                break_bout_info[break_bout_info$start >= day_start & break_bout_info$start <= day_end, ]
              } else data.frame()
              n_break_bouts_starting <- nrow(break_bouts_starting)

              break_bouts_ending <- if (nrow(break_bout_info) > 0) {
                break_bout_info[break_bout_info$end >= day_start & break_bout_info$end <= day_end, ]
              } else data.frame()
              n_break_bouts_ending <- nrow(break_bouts_ending)

              total_break_time <- 0
              if (nrow(break_bouts_occurring) > 0) {
                for (b in seq_len(nrow(break_bouts_occurring))) {
                  b_start <- max(break_bouts_occurring$start[b], day_start)
                  b_end <- min(break_bouts_occurring$end[b], day_end)
                  total_break_time <- total_break_time + (b_end - b_start + 1)
                }
              }

              # Only use WEAR TIME epochs for count metrics              # If no wear time, all metrics are 0
              if (n_wear_epochs > 0) {
                # Filter data by wear mask
                w_axis1 <- d_axis1[day_wear]
                w_axis2 <- d_axis2[day_wear]
                w_axis3 <- d_axis3[day_wear]
                w_steps <- d_steps[day_wear]
                w_lux <- d_lux[day_wear]
                w_vm <- d_vm[day_wear]

                # Axis counts
                axis1_counts <- sum(w_axis1, na.rm = TRUE)
                axis2_counts <- sum(w_axis2, na.rm = TRUE)
                axis3_counts <- sum(w_axis3, na.rm = TRUE)

                axis1_avg <- mean(w_axis1, na.rm = TRUE)
                axis2_avg <- mean(w_axis2, na.rm = TRUE)
                axis3_avg <- mean(w_axis3, na.rm = TRUE)

                axis1_max <- max(w_axis1, na.rm = TRUE)
                axis2_max <- max(w_axis2, na.rm = TRUE)
                axis3_max <- max(w_axis3, na.rm = TRUE)

                axis1_cpm <- axis1_avg * (60 / epoch_sec)
                axis2_cpm <- axis2_avg * (60 / epoch_sec)
                axis3_cpm <- axis3_avg * (60 / epoch_sec)

                # Vector magnitude
                vm_counts <- sum(w_vm, na.rm = TRUE)
                vm_avg <- mean(w_vm, na.rm = TRUE)
                vm_max <- max(w_vm, na.rm = TRUE)
                vm_cpm <- vm_avg * (60 / epoch_sec)

                # Steps
                steps_counts <- sum(w_steps, na.rm = TRUE)
                steps_avg <- mean(w_steps, na.rm = TRUE)
                steps_max <- max(w_steps, na.rm = TRUE)
                steps_per_min <- steps_avg * (60 / epoch_sec)

                # Lux
                lux_avg <- mean(w_lux, na.rm = TRUE)
                lux_max <- max(w_lux, na.rm = TRUE)
              } else {
                # No wear time - all metrics are 0
                axis1_counts <- axis2_counts <- axis3_counts <- 0
                axis1_avg <- axis2_avg <- axis3_avg <- 0
                axis1_max <- axis2_max <- axis3_max <- 0
                axis1_cpm <- axis2_cpm <- axis3_cpm <- 0
                vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
                steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
                lux_avg <- lux_max <- 0
              }

              # Energy expenditure for this day
              kcals <- 0
              mets_avg <- 1
              if (!is.null(r$mets) && length(r$mets) >= max(day_indices)) {
                day_mets <- r$mets[day_indices]
                mets_avg <- mean(day_mets, na.rm = TRUE)
                weight_kg <- weight * 0.453592
                time_hours <- n_epochs * epoch_sec / 3600
                kcals <- mets_avg * weight_kg * time_hours
              }
              avg_hourly_kcals <- if (n_hours > 0) kcals / n_hours else 0

              # Day of week
              dow <- weekdays(as.Date(date_i))
              dow_num <- as.numeric(format(as.Date(date_i), "%u"))

              # Time in minutes (wear time only)
              time_min <- n_wear_epochs * epoch_sec / 60

              row_data <- data.frame(
                Subject = r$subject_id,
                Filename = r$name,
                Epoch = epoch_sec,
                `Weight (lbs)` = weight,
                Age = age,
                Gender = gender,
                Date = format(as.Date(date_i), "%m/%d/%Y"),
                `Day of Week` = dow,
                `Day of Week Num` = dow_num,
                kcals = round(kcals, 3),
                `Average Hourly kcals` = round(avg_hourly_kcals, 3),
                METs = round(mets_avg, 3),
                # MVPA Bout columns
                `Number of MVPA Bouts occurring in this day` = n_bouts_occurring,
                `Number of MVPA Bouts starting in this day` = n_bouts_starting,
                `Number of MVPA Bouts ending in this day` = n_bouts_ending,
                `Total time of MVPA Bouts occurring in this day` = total_bout_time,
                `Total activity counts of MVPA Bouts occurring in this day` = total_bout_counts,
                # Sedentary Bout columns
                `Number of Sedentary Bouts occurring in this day` = n_sed_bouts_occurring,
                `Number of Sedentary Bouts starting in this day` = n_sed_bouts_starting,
                `Number of Sedentary Bouts ending in this day` = n_sed_bouts_ending,
                `Total time of Sedentary Bouts occurring in this day` = total_sed_bout_time,
                # Sedentary Break columns
                `Number of Sedentary Breaks occurring in this day` = n_break_bouts_occurring,
                `Number of Sedentary Breaks starting in this day` = n_break_bouts_starting,
                `Number of Sedentary Breaks ending in this day` = n_break_bouts_ending,
                `Total time of Sedentary Breaks occurring in this day` = total_break_time,
                # Intensity counts
                Sedentary = sedentary,
                Light = light,
                Moderate = moderate,
                Vigorous = vigorous,
                `Very Vigorous` = very_vigorous,
                # Percentages
                `% in Sedentary` = sprintf("%.2f%%", pct_sed),
                `% in Light` = sprintf("%.2f%%", pct_light),
                `% in Moderate` = sprintf("%.2f%%", pct_mod),
                `% in Vigorous` = sprintf("%.2f%%", pct_vig),
                `% in Very Vigorous` = sprintf("%.2f%%", pct_vvig),
                `Total MVPA` = total_mvpa,
                `% in MVPA` = sprintf("%.2f%%", pct_mvpa),
                `Average MVPA Per Hour` = round(avg_mvpa_per_hour, 1),
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
                # Metadata (using wear time epochs)
                `Number of Epochs` = n_wear_epochs,
                Time = round(time_min),
                `Calendar Days` = if (n_wear_epochs > 0) 1 else 0,
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
              all_rows[[length(all_rows) + 1]] <- row_data
            }
          }
        }

        if (length(all_rows) == 0) {
          write.csv(data.frame(Message = "No daily data to export"), file, row.names = FALSE)
          return()
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    output$export_hourly <- downloadHandler(
      filename = function() {
        paste0("canhrActi_Hourly_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
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
            "sasaki_vm3" = "Sasaki VM3 (2011)",
            "freedson_vm3" = "Freedson VM3 (2011)",
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

            # Apply wear time mask BEFORE calculating intensity
            axis1_wear <- axis1
            axis1_wear[!wear_mask] <- NA

            all_cpm <- canhrActi::to_cpm(axis1_wear, epoch_sec)
            all_intensity <- tryCatch({
              canhrActi::apply_cutpoints(all_cpm, algo, epoch_sec)
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
                dow <- weekdays(as.Date(date_i))
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
                  `Total time of MVPA Bouts occurring in this hour` = total_bout_time,
                  `Total activity counts of MVPA Bouts occurring in this hour` = total_bout_counts,
                  # Sedentary Bout columns
                  `Number of Sedentary Bouts occurring in this hour` = n_sed_bouts_occurring,
                  `Number of Sedentary Bouts starting in this hour` = n_sed_bouts_starting,
                  `Number of Sedentary Bouts ending in this hour` = n_sed_bouts_ending,
                  `Total time of Sedentary Bouts occurring in this hour` = total_sed_bout_time,
                  # Sedentary Break columns
                  `Number of Sedentary Breaks occurring in this hour` = n_break_bouts_occurring,
                  `Number of Sedentary Breaks starting in this hour` = n_break_bouts_starting,
                  `Number of Sedentary Breaks ending in this hour` = n_break_bouts_ending,
                  `Total time of Sedentary Breaks occurring in this hour` = total_break_time,
                  # Intensity counts
                  Sedentary = sedentary,
                  Light = light,
                  Moderate = moderate,
                  Vigorous = vigorous,
                  `Very Vigorous` = very_vigorous,
                  # Percentages
                  `% in Sedentary` = sprintf("%.2f%%", pct_sed),
                  `% in Light` = sprintf("%.2f%%", pct_light),
                  `% in Moderate` = sprintf("%.2f%%", pct_mod),
                  `% in Vigorous` = sprintf("%.2f%%", pct_vig),
                  `% in Very Vigorous` = sprintf("%.2f%%", pct_vvig),
                  `Total MVPA` = total_mvpa,
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
                  Time = round(time_min),
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
        paste0("canhrActi_SedentaryAnalysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
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
              prev_end_time <- data$timestamp[bout_ends[i - 1]]
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
              `Sedentary Bout Start` = format(bout_start_time, "%m/%d/%Y %I:%M:%S %p"),
              `Sedentary Bout End` = format(bout_end_time, "%m/%d/%Y %I:%M:%S %p"),
              `Time in Sedentary Bout` = round(duration_min[i], 0),
              `Time since last Sedentary Bout` = round(time_since_last, 0),
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
              Time = round(duration_min[i], 0),
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
      res <- results()
      sel <- input$selected_participant

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see VM heatmap",
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
  })
}
