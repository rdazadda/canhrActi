# Module: Wear Time Validation
# Consistent with Activity/Sleep tabs: chart-first layout, compact metrics strip

mod_wear_time_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Page Header
    page_header(
      icon_name = "clock",
      title = "Wear Time Validation",
      subtitle = "Detect device wear and non-wear periods",
      status_output_id = ns("validation_status_badge")
    ),

    # Compact Metrics Strip (matching Activity/Sleep tab style)
    div(class = "metrics-strip metrics-strip--transparent",
      # File count badge
      div(class = "file-info-badge metrics-strip-fixed",
        textOutput(ns("sum_files"), inline = TRUE), "/",
        textOutput(ns("total_files"), inline = TRUE), " validated"
      ),

      # Valid Days metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("sum_valid_days"), inline = TRUE)),
        div(class = "metric-label", "Valid Days")
      ),

      # Avg Wear Time metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("sum_avg_wear"), inline = TRUE)),
        div(class = "metric-label", "Avg Wear Time")
      ),

      # Wear % metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("sum_wear_pct"), inline = TRUE)),
        div(class = "metric-label", "Wear %")
      ),

      # Algorithm selector (inline)
      div(class = "metric-card metric-card--inline",
        selectInput(ns("algorithm"), NULL,
                    choices = c("Choi (2011)" = "choi",
                                "Troiano (2008)" = "troiano",
                                "CANHR (2025)" = "canhr"),
                    selected = "choi", width = "145px")
      ),

      # Quick actions (matching Activity/Sleep tab)
      div(class = "cluster cluster--gap-2 ml-auto metrics-strip-fixed",
        actionButton(ns("run_btn"), span(icon("play"), "Run Validation"),
                     class = "btn-primary"),
        actionButton(ns("validate_selected"), span(icon("check"), "Validate Selected"),
                     class = "btn-default"),
        actionButton(ns("toggle_advanced"), span(icon("sliders-h"), "Advanced"),
                     class = "btn-default"),
        actionButton(ns("clear_results"), span(icon("redo"), "Reset"),
                     class = "btn-default")
      )
    ),

    # Advanced Settings Panel (collapsible)
    shinyjs::hidden(
      div(id = ns("advanced_panel"),
        div(class = "advanced-settings-box",
          # CSS Grid layout - 3 columns
          div(class = "advanced-settings-grid",

            # Column 1: Non-Wear Definition
            div(class = "settings-column",
              div(class = "settings-column-header",
                icon("sliders-h"), "Non-Wear Definition"
              ),
              div(class = "form-input-row",
                tags$label("Minimum Length", class = "form-input-label"),
                div(class = "form-input-wrapper",
                  numericInput(ns("min_length"), NULL, value = 90, min = 30, max = 180, width = "100%"),
                  span(class = "form-input-unit", "min")
                )
              ),
              div(class = "form-input-row",
                tags$label("Small Window", class = "form-input-label"),
                div(class = "form-input-wrapper",
                  numericInput(ns("small_window"), NULL, value = 30, min = 10, max = 60, width = "100%"),
                  span(class = "form-input-unit", "min")
                )
              ),
              div(class = "form-input-row",
                tags$label("Spike Tolerance", class = "form-input-label"),
                div(class = "form-input-wrapper",
                  numericInput(ns("spike_tolerance"), NULL, value = 2, min = 0, max = 10, width = "100%"),
                  span(class = "form-input-unit", "min")
                )
              ),
              div(class = "form-input-row",
                tags$label("Spike Stop Level", class = "form-input-label"),
                div(class = "form-input-wrapper",
                  numericInput(ns("spike_stoplevel"), NULL, value = 100, min = 0, max = 500, width = "100%"),
                  span(class = "form-input-unit", "CPM")
                )
              ),
              div(class = "form-checkbox-row",
                checkboxInput(ns("use_vm"), "Use Vector Magnitude", value = FALSE)
              )
            ),

            # Column 2: Validity Criteria
            div(class = "settings-column",
              div(class = "settings-column-header",
                icon("check-circle"), "Validity Criteria"
              ),
              div(class = "form-input-row",
                tags$label("Min Wear per Day", class = "form-input-label"),
                div(class = "form-input-wrapper",
                  numericInput(ns("min_wear_day"), NULL, value = 600, min = 0, max = 1440, width = "100%"),
                  span(class = "form-input-unit", "min")
                )
              ),
              div(class = "form-input-row",
                tags$label("Min Valid Days", class = "form-input-label"),
                numericInput(ns("min_valid_days"), NULL, value = 3, min = 0, max = 14, width = "100%")
              ),
              div(class = "form-input-row",
                tags$label("Min Weekdays", class = "form-input-label"),
                numericInput(ns("min_weekdays"), NULL, value = 0, min = 0, max = 5, width = "100%")
              ),
              div(class = "form-input-row",
                tags$label("Min Weekend Days", class = "form-input-label"),
                numericInput(ns("min_weekend"), NULL, value = 0, min = 0, max = 2, width = "100%")
              ),
              div(class = "form-help-text",
                icon("info-circle"),
                span("A valid day has >= 10 hours of wear time by default")
              )
            ),

            # Column 3: Additional Options
            div(class = "settings-column",
              div(class = "settings-column-header",
                icon("cog"), "Additional Options"
              ),
              div(class = "form-input-row",
                tags$label("Sleep Period Handling", class = "form-input-label"),
                selectInput(ns("sleep_option"), NULL,
                            choices = c("Ignore" = "ignore",
                                        "Mark As Wear" = "wear",
                                        "Mark As Non-Wear" = "nonwear"),
                            selected = "nonwear", width = "100%")
              ),
              div(class = "form-checkbox-row",
                checkboxInput(ns("use_ignore_short"), "Ignore short wear periods", value = FALSE)
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("use_ignore_short")),
                div(class = "form-input-row",
                  tags$label("Ignore periods shorter than", class = "form-input-label"),
                  div(class = "form-input-wrapper",
                    numericInput(ns("ignore_short_min"), NULL, value = 30, min = 0, max = 60, width = "100%"),
                    span(class = "form-input-unit", "min")
                  )
                )
              ),
              div(class = "form-divider",
                actionButton(ns("load_defaults"), span(icon("undo"), " Reset to Defaults"),
                             class = "btn-default btn-full")
              )
            )
          )
        )
      )
    ),

    # Processing Indicator
    shinyjs::hidden(
      div(id = ns("processing_indicator"), class = "processing-indicator",
        icon("spinner", class = "fa-spin fa-2x"),
        p(class = "processing-text", "Processing files..."),
        p(id = ns("processing_status"), class = "processing-detail", "Please wait")
      )
    ),

    # Hero Chart - Daily Wear Time (Full Width)
    fluidRow(
      column(12,
        div(class = "hero-chart-container",
          div(class = "hero-chart-header",
            div(class = "hero-chart-title",
              icon("chart-bar"), "Daily Wear Time Summary"
            ),
            div(style = "min-width: 180px;",
              selectInput(ns("chart_file"), NULL,
                          choices = c("All Files" = "all"), width = "100%")
            )
          ),
          div(class = "chart-summary-bar",
            uiOutput(ns("chart_summary"))
          ),
          conditionalPanel(
            condition = "output.has_wear_results == false", ns = ns,
            chart_empty_state(
              title = "No Wear Time Data",
              message = "Select an algorithm and click 'Run Validation' to analyze wear patterns",
              show_icon = FALSE
            )
          ),
          conditionalPanel(
            condition = "output.has_wear_results == true", ns = ns,
            plotOutput(ns("daily_chart"), height = "380px")
          )
        )
      )
    ),

    # File Status Table and Hourly Pattern
    fluidRow(
      # Files Table (Left)
      column(7,
        box(
          title = span(icon("file-alt"), "File Validation Status"),
          status = "primary", solidHeader = TRUE, width = NULL,
          collapsible = TRUE,
          div(class = "table-info",
            span(class = "status-text", textOutput(ns("files_loaded_text"), inline = TRUE)),
            span(class = "table-tip", icon("info-circle"), " Click row to view details")
          ),
          DT::dataTableOutput(ns("files_table"))
        )
      ),

      # Hourly Pattern (Right)
      column(5,
        box(
          title = span(icon("clock"), "Hourly Wear Pattern"),
          status = "info", solidHeader = TRUE, width = NULL,
          collapsible = TRUE,
          div(class = "chart-container",
            conditionalPanel(
              condition = "output.has_wear_results == false", ns = ns,
              chart_empty_state(
                title = "Hourly Wear Pattern",
                message = "Run validation to see hourly patterns",
                show_icon = FALSE
              )
            ),
            conditionalPanel(
              condition = "output.has_wear_results == true", ns = ns,
              plotOutput(ns("hourly_pattern"), height = "280px")
            )
          )
        )
      )
    ),

    # Detailed Results Tabs
    fluidRow(
      column(12,
        div(class = "hero-chart-container results-tabs",
          tabsetPanel(
            id = ns("detail_tabs"),
            type = "tabs",
            tabPanel(
              title = "Daily Summary",
              value = "daily_tab",
              DT::dataTableOutput(ns("daily_summary_table"))
            ),
            tabPanel(
              title = "Wear Periods",
              value = "periods_tab",
              fluidRow(
                column(8,
                  DT::dataTableOutput(ns("wear_periods_table"))
                ),
                column(4,
                  div(class = "periods-summary",
                    h5("Wear Period Statistics"),
                    uiOutput(ns("periods_stats"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_wear_time_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Output for conditional panel
    output$has_wear_results <- reactive({
      length(results()) > 0
    })
    outputOptions(output, "has_wear_results", suspendWhenHidden = FALSE)

    # Toggle advanced settings panel
    observeEvent(input$toggle_advanced, {
      shinyjs::toggle("advanced_panel")
    })

    # Load default parameters based on algorithm
    observeEvent(input$algorithm, {
      load_algorithm_defaults()
    })

    observeEvent(input$load_defaults, {
      load_algorithm_defaults()
    })

    load_algorithm_defaults <- function() {
      alg <- input$algorithm
      if (alg == "troiano") {
        updateNumericInput(session, "min_length", value = 60)
        updateNumericInput(session, "small_window", value = 30)
        updateNumericInput(session, "spike_tolerance", value = 2)
        updateNumericInput(session, "spike_stoplevel", value = 100)
      } else if (alg == "choi") {
        updateNumericInput(session, "min_length", value = 90)
        updateNumericInput(session, "small_window", value = 30)
        updateNumericInput(session, "spike_tolerance", value = 2)
        updateNumericInput(session, "spike_stoplevel", value = 100)
      } else if (alg == "canhr") {
        updateNumericInput(session, "min_length", value = 120)
        updateNumericInput(session, "small_window", value = 45)
        updateNumericInput(session, "spike_tolerance", value = 3)
        updateNumericInput(session, "spike_stoplevel", value = 150)
      }
    }

    # Update chart file selector
    observe({
      if (shared$file_count == 0) {
        updateSelectInput(session, "chart_file", choices = c("No files" = "none"))
      } else {
        choices <- c("All Files (Combined)" = "all")
        for (fid in names(shared$files)) {
          f <- shared$files[[fid]]
          choices <- c(choices, setNames(fid, f$subject_info$id))
        }
        updateSelectInput(session, "chart_file", choices = choices)
      }
    })

    # Files loaded text
    output$files_loaded_text <- renderText({
      paste(shared$file_count, "file(s) loaded")
    })

    # Summary strip outputs (matching Activity/Sleep tab pattern)
    output$sum_files <- renderText({
      res <- results()
      as.character(length(res))
    })

    output$total_files <- renderText({
      as.character(shared$file_count)
    })

    # Clear results handler
    observeEvent(input$clear_results, {
      results(list())
      showNotification("Wear time results cleared", type = "message", duration = 2)
    })

    output$sum_valid_days <- renderText({
      res <- results()
      sel <- input$chart_file
      if (length(res) == 0 || is.null(sel) || sel == "none") return("--")
      if (sel == "all") {
        total <- sum(sapply(res, function(r) r$valid_days))
        paste0(total)
      } else if (sel %in% names(res)) {
        paste0(res[[sel]]$valid_days)
      } else {
        "--"
      }
    })

    output$sum_avg_wear <- renderText({
      res <- results()
      sel <- input$chart_file
      if (length(res) == 0 || is.null(sel) || sel == "none") return("--")
      if (sel == "all") {
        avg <- mean(sapply(res, function(r) r$avg_wear), na.rm = TRUE)
        if (is.na(avg)) return("--")
        paste0(round(avg, 1), "h")
      } else if (sel %in% names(res)) {
        avg_val <- res[[sel]]$avg_wear
        if (is.na(avg_val)) return("--")
        paste0(round(avg_val, 1), "h")
      } else {
        "--"
      }
    })

    output$sum_wear_pct <- renderText({
      res <- results()
      sel <- input$chart_file
      if (length(res) == 0 || is.null(sel) || sel == "none") return("--")
      if (sel == "all") {
        avg <- mean(sapply(res, function(r) r$wear_pct), na.rm = TRUE)
        paste0(round(avg, 1), "%")
      } else if (sel %in% names(res)) {
        paste0(round(res[[sel]]$wear_pct, 1), "%")
      } else {
        "--"
      }
    })

    # Validation status badge for header
    output$validation_status_badge <- renderUI({
      res <- results()
      n <- length(res)
      if (n > 0) {
        valid_total <- sum(sapply(res, function(r) r$valid_days))
        status_badge(paste(n, "files |", valid_total, "valid days"), "success")
      } else {
        status_badge("Not validated", "pending")
      }
    })

    # Chart summary
    output$chart_summary <- renderUI({
      res <- results()
      sel <- input$chart_file
      if (length(res) == 0 || sel == "none") {
        return(tags$span(class = "chart-summary-text",
                         icon("info-circle"), " Select algorithm and click 'Run Validation' to begin"))
      }

      min_wear_hours <- input$min_wear_day / 60

      if (sel == "all") {
        total_valid <- sum(sapply(res, function(r) r$valid_days))
        total_days <- sum(sapply(res, function(r) r$total_days))
        meets_count <- sum(sapply(res, function(r) r$meets_criteria))

        tagList(
          tags$span(class = "chart-summary-stat",
            tags$strong(total_valid), " / ", total_days, " valid days"
          ),
          tags$span(class = "chart-summary-divider", "|"),
          tags$span(class = "chart-summary-stat",
            icon("check-circle"), meets_count, " subjects meet criteria"
          ),
          tags$span(class = "chart-summary-divider", "|"),
          tags$span(class = "chart-summary-stat",
            "Min: ", min_wear_hours, "h/day"
          )
        )
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        criteria_icon <- if (r$meets_criteria) icon("check-circle", class = "text-success")
                         else icon("times-circle", class = "text-warning")
        tagList(
          tags$span(class = "chart-summary-stat",
            tags$strong(r$valid_days), " / ", r$total_days, " valid days"
          ),
          tags$span(class = "chart-summary-divider", "|"),
          tags$span(class = "chart-summary-stat",
            r$valid_weekdays, " weekdays | ", r$valid_weekend, " weekend"
          ),
          tags$span(class = "chart-summary-divider", "|"),
          tags$span(class = "chart-summary-stat",
            criteria_icon, if (r$meets_criteria) " Meets criteria" else " Does not meet criteria"
          )
        )
      }
    })

    # Files table - Clean, essential columns with badges
    output$files_table <- DT::renderDataTable({
      if (shared$file_count == 0) {
        return(DT::datatable(
          data.frame(Message = "No files loaded. Go to Data Upload tab."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      res <- results()

      df <- data.frame(
        file_id = names(shared$files),
        Subject = sapply(shared$files, function(f) f$subject_info$id %||% "N/A"),
        Status = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            if (res[[fid]]$meets_criteria) "Valid" else "Incomplete"
          } else "Pending"
        }),
        Valid_Days = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            paste0(res[[fid]]$valid_days, "/", res[[fid]]$total_days)
          } else "--"
        }),
        Wear_Pct = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$wear_pct, 1), "%") else "--"
        }),
        Avg_Wear = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && !is.na(res[[fid]]$avg_wear)) {
            paste0(round(res[[fid]]$avg_wear, 1), "h")
          } else "--"
        }),
        Algorithm = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) toupper(res[[fid]]$algorithm) else "--"
        }),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df[, -1],  # Remove file_id from display
        selection = "multiple",
        options = list(
          pageLength = 8,
          dom = 'tip',
          scrollX = FALSE,
          columnDefs = list(
            list(width = '120px', targets = 0),
            list(width = '80px', targets = 1),
            list(width = '90px', targets = 2),
            list(width = '80px', targets = 3),
            list(width = '80px', targets = 4),
            list(width = '80px', targets = 5)
          )
        ),
        rownames = FALSE,
        colnames = c("Subject", "Status", "Valid Days", "Wear %", "Avg Wear", "Algorithm")
      ) %>%
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c("Valid", "Incomplete", "Pending"),
            c("#d4edda", "#fff3cd", "#f8f9fa")
          ),
          color = DT::styleEqual(
            c("Valid", "Incomplete", "Pending"),
            c("#155724", "#856404", "#6c757d")
          ),
          fontWeight = "bold"
        )
    })

    # Run validation on all files
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      shinyjs::disable("run_btn")
      shinyjs::disable("validate_selected")
      shinyjs::show("processing_indicator")

      shinyjs::delay(100, {
        tryCatch({
          run_validation(names(shared$files))
        }, finally = {
          shinyjs::hide("processing_indicator")
          shinyjs::enable("run_btn")
          shinyjs::enable("validate_selected")
        })
      })
    })

    # Validate selected files only
    observeEvent(input$validate_selected, {
      selected <- input$files_table_rows_selected
      req(selected)

      shinyjs::disable("run_btn")
      shinyjs::disable("validate_selected")
      shinyjs::show("processing_indicator")

      file_ids <- names(shared$files)[selected]

      shinyjs::delay(100, {
        tryCatch({
          run_validation(file_ids)
        }, finally = {
          shinyjs::hide("processing_indicator")
          shinyjs::enable("run_btn")
          shinyjs::enable("validate_selected")
        })
      })
    })

    # Helper: Format ETA
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    run_validation <- function(file_ids) {
      all_results <- results()
      n_files <- length(file_ids)

      count_col <- if (input$use_vm) "vector_magnitude" else "axis1"
      min_wear_hours <- input$min_wear_day / 60
      start_time <- Sys.time()
      progress_interval <- max(1, min(5, ceiling(n_files / 10)))
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
          if (!is.null(prefix) && nzchar(prefix)) {
            msg <- paste(prefix, msg)
          }
          notify_warning(msg)
          invokeRestart("muffleWarning")
        })
      }

      withProgress(message = "Validating wear time...", value = 0, {
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

          counts_raw <- if (count_col %in% names(data)) data[[count_col]] else data$axis1
          counts <- counts_raw
          if (!is.na(f$epoch_length) && f$epoch_length > 0 && f$epoch_length != 60) {
            counts <- counts_raw * (60 / f$epoch_length)
          }
          epoch_minutes <- f$epoch_length / 60
          non_wear_epochs <- as.integer(input$min_length / epoch_minutes)
          spike_tol_epochs <- as.integer(input$spike_tolerance / epoch_minutes)
          if (spike_tol_epochs < 1) spike_tol_epochs <- 1
          small_window_epochs <- as.integer(input$small_window / epoch_minutes)

          wear <- tryCatch({
            capture_warnings({
              if (input$algorithm == "troiano") {
                canhrActi::wear.troiano(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = input$spike_stoplevel
                )
              } else if (input$algorithm == "choi") {
                canhrActi::wear.choi(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = input$spike_stoplevel,
                  min_window_len = small_window_epochs
                )
              } else if (input$algorithm == "canhr") {
                canhrActi::wear.CANHR2025(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = input$spike_stoplevel,
                  min_window_len = small_window_epochs
                )
              } else {
                canhrActi::wear.CANHR2025(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = input$spike_stoplevel,
                  min_window_len = small_window_epochs
                )
              }
            }, prefix = paste0(f$name, ":"))
          }, error = function(e) {
            showNotification(paste0("Wear validation skipped for ", f$name), type = "error")
            return(NULL)
          })

          if (is.null(wear)) next

          daily <- NULL
          hourly <- NULL
          wear_periods <- NULL
          nonwear_periods <- NULL
          valid_weekdays <- 0
          valid_weekend <- 0

          if ("timestamp" %in% names(data)) {
            temp <- data
            temp$wear <- wear
            temp$date <- as.Date(temp$timestamp)
            temp$hour <- as.numeric(format(temp$timestamp, "%H"))
            temp$weekday <- weekdays(temp$timestamp)
            temp$is_weekend <- temp$weekday %in% c("Saturday", "Sunday")

            daily <- aggregate(wear ~ date, temp, sum)
            daily$wear_hours <- daily$wear * f$epoch_length / 3600
            daily$wear_min <- daily$wear_hours * 60
            daily$valid <- daily$wear_min >= input$min_wear_day
            daily$weekday <- weekdays(as.Date(daily$date))
            daily$is_weekend <- daily$weekday %in% c("Saturday", "Sunday")

            valid_weekdays <- sum(daily$valid & !daily$is_weekend)
            valid_weekend <- sum(daily$valid & daily$is_weekend)

            hourly <- aggregate(wear ~ hour, temp, mean)
            hourly$wear_pct <- hourly$wear * 100

            wear_periods <- canhrActi::get.wear.periods(wear, temp$timestamp, epoch_length = f$epoch_length)

            if (input$use_ignore_short && input$ignore_short_min > 0) {
              wear_periods <- wear_periods[wear_periods$duration_minutes >= input$ignore_short_min, ]
              if (nrow(wear_periods) > 0) {
                wear_periods$period <- seq_len(nrow(wear_periods))
              }
            }

            nonwear_periods <- detect_wear_periods(temp$timestamp, !wear, f$epoch_length, input$min_length)
          }

          valid_days <- if (!is.null(daily)) sum(daily$valid) else 0
          total_days <- if (!is.null(daily)) nrow(daily) else 0

          min_wear_for_day <- input$min_wear_day
          days_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day) else 0
          weekdays_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day & !daily$is_weekend) else 0
          weekend_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day & daily$is_weekend) else 0

          n_wear_periods <- if (!is.null(wear_periods) && nrow(wear_periods) > 0) nrow(wear_periods) else 0
          n_nonwear_periods <- if (!is.null(nonwear_periods) && nrow(nonwear_periods) > 0) nrow(nonwear_periods) else 0

          avg_wear_period_sec <- if (n_wear_periods > 0) {
            mean(wear_periods$duration_minutes, na.rm = TRUE) * 60
          } else 0

          avg_nonwear_period_sec <- if (n_nonwear_periods > 0) {
            mean(nonwear_periods$duration_min, na.rm = TRUE) * 60
          } else 0

          meets_criteria <- TRUE
          if (valid_days < input$min_valid_days) meets_criteria <- FALSE
          if (valid_weekdays < input$min_weekdays) meets_criteria <- FALSE
          if (valid_weekend < input$min_weekend) meets_criteria <- FALSE

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            algorithm = input$algorithm,
            parameters = list(
              min_length = input$min_length,
              small_window = input$small_window,
              spike_tolerance = input$spike_tolerance,
              spike_stoplevel = input$spike_stoplevel,
              use_vm = input$use_vm,
              min_wear_day = input$min_wear_day
            ),
            wear = wear,
            daily = daily,
            hourly = hourly,
            wear_periods = wear_periods,
            nonwear_periods = nonwear_periods,
            n_nonwear_periods = n_nonwear_periods,
            total_epochs = length(wear),
            wear_epochs = sum(wear),
            nonwear_epochs = sum(!wear),
            total_days = total_days,
            valid_days = valid_days,
            valid_weekdays = valid_weekdays,
            valid_weekend = valid_weekend,
            days_with_wear = days_with_wear,
            weekdays_with_wear = weekdays_with_wear,
            weekend_with_wear = weekend_with_wear,
            avg_wear_period_sec = avg_wear_period_sec,
            avg_nonwear_period_sec = avg_nonwear_period_sec,
            avg_wear = if (!is.null(daily) && any(daily$valid)) mean(daily$wear_hours[daily$valid], na.rm = TRUE) else NA,
            total_wear = if (!is.null(daily)) sum(daily$wear_hours) else NA,
            total_nonwear = if (!is.null(daily)) sum(24 - daily$wear_hours) else NA,
            wear_pct = round(sum(wear) / length(wear) * 100, 1),
            meets_criteria = meets_criteria,
            validated_at = Sys.time()
          )
        }

        gc(verbose = FALSE)
      })

      results(all_results)
      shared$results$wear_time <- all_results

      showNotification(paste("Wear time validation complete for", length(file_ids), "files"), type = "message")
    }

    # Helper function to detect continuous wear periods
    detect_wear_periods <- function(timestamps, wear, epoch_length, min_duration_min = 0) {
      if (length(wear) == 0) return(data.frame())

      periods <- data.frame()
      in_wear <- FALSE
      start_idx <- 1

      for (i in seq_along(wear)) {
        if (wear[i] && !in_wear) {
          in_wear <- TRUE
          start_idx <- i
        } else if (!wear[i] && in_wear) {
          in_wear <- FALSE
          duration <- (i - start_idx) * epoch_length / 60
          if (duration >= min_duration_min) {
            periods <- rbind(periods, data.frame(
              start = timestamps[start_idx],
              end = timestamps[i - 1],
              duration_min = duration
            ))
          }
        }
      }

      if (in_wear) {
        duration <- (length(timestamps) - start_idx + 1) * epoch_length / 60
        if (duration >= min_duration_min) {
          periods <- rbind(periods, data.frame(
            start = timestamps[start_idx],
            end = timestamps[length(timestamps)],
            duration_min = duration
          ))
        }
      }

      return(periods)
    }

    # Daily chart - Hero visualization
    output$daily_chart <- renderPlot({
      res <- results()
      req(length(res) > 0)

      sel <- input$chart_file
      min_wear_hours <- input$min_wear_day / 60

      if (sel == "all" || sel == "none") {
        all_daily <- data.frame()
        for (r in res) {
          if (!is.null(r$daily)) {
            d <- r$daily
            d$subject <- r$subject_id
            all_daily <- rbind(all_daily, d)
          }
        }
        req(nrow(all_daily) > 0)

        all_daily$day_label <- format(all_daily$date, "%a\n%m/%d")
        all_daily$day_type <- ifelse(all_daily$is_weekend, "Weekend", "Weekday")
        all_daily$status <- ifelse(all_daily$valid, "Valid", "Invalid")

        ggplot(all_daily, aes(x = date, y = wear_hours, fill = interaction(status, day_type))) +
          geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
          geom_hline(yintercept = min_wear_hours, linetype = "dashed", color = "#236192", linewidth = 1) +
          annotate("text", x = min(all_daily$date), y = min_wear_hours + 0.5,
                   label = paste0(min_wear_hours, "h threshold"), hjust = 0, size = 3.5, color = "#236192", fontface = "bold") +
          scale_fill_manual(
            values = c("Valid.Weekday" = "#17a589", "Invalid.Weekday" = "#e2e8f0",
                       "Valid.Weekend" = "#236192", "Invalid.Weekend" = "#f1f5f9"),
            labels = c("Valid.Weekday" = "Valid Weekday", "Invalid.Weekday" = "Invalid Weekday",
                       "Valid.Weekend" = "Valid Weekend", "Invalid.Weekend" = "Invalid Weekend"),
            name = "") +
          scale_y_continuous(limits = c(0, 26), breaks = seq(0, 24, 4),
                             sec.axis = sec_axis(~./24*100, name = "% of Day", breaks = seq(0, 100, 25))) +
          facet_wrap(~subject, scales = "free_x", ncol = min(length(unique(all_daily$subject)), 3)) +
          labs(title = NULL, x = "", y = "Wear Time (hours)") +
          canhrActi::theme_canhrActi() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                legend.position = "top",
                legend.box = "horizontal",
                legend.text = element_text(size = 10),
                strip.background = element_rect(fill = "#236192", color = NA),
                strip.text = element_text(color = "white", face = "bold", size = 11),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank())

      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        daily <- r$daily
        req(daily)

        daily$day_label <- paste0(format(daily$date, "%a"), "\n", format(daily$date, "%m/%d"))
        daily$day_type <- ifelse(daily$is_weekend, "Weekend", "Weekday")
        daily$status <- ifelse(daily$valid, "Valid", "Invalid")

        ggplot(daily, aes(x = factor(day_label, levels = day_label), y = wear_hours,
                          fill = interaction(status, day_type))) +
          geom_bar(stat = "identity", width = 0.75, color = "white", linewidth = 0.5) +
          geom_hline(yintercept = min_wear_hours, linetype = "dashed", color = "#236192", linewidth = 1.2) +
          geom_text(aes(label = sprintf("%.1fh", wear_hours)), vjust = -0.3, size = 4, fontface = "bold") +
          scale_fill_manual(
            values = c("Valid.Weekday" = "#17a589", "Invalid.Weekday" = "#e2e8f0",
                       "Valid.Weekend" = "#236192", "Invalid.Weekend" = "#f1f5f9"),
            labels = c("Valid.Weekday" = "Valid Weekday", "Invalid.Weekday" = "Invalid Weekday",
                       "Valid.Weekend" = "Valid Weekend", "Invalid.Weekend" = "Invalid Weekend"),
            name = "") +
          scale_y_continuous(limits = c(0, 28), breaks = seq(0, 24, 4),
                             sec.axis = sec_axis(~./24*100, name = "% of Day", breaks = seq(0, 100, 25))) +
          labs(title = paste("Subject:", r$subject_id),
               subtitle = paste("Algorithm:", toupper(r$algorithm), " | ",
                                sum(daily$valid), "of", nrow(daily), "days valid"),
               x = "", y = "Wear Time (hours)") +
          canhrActi::theme_canhrActi() +
          theme(axis.text.x = element_text(size = 13),
                legend.position = "top",
                legend.text = element_text(size = 10),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                plot.title = element_text(face = "bold", size = 15),
                plot.subtitle = element_text(color = "#64748b", size = 11))
      }
    })

    # Hourly pattern plot
    output$hourly_pattern <- renderPlot({
      res <- results()
      sel <- input$chart_file
      req(length(res) > 0)

      if (sel == "all" || sel == "none") {
        all_hourly <- data.frame()
        for (r in res) {
          if (!is.null(r$hourly)) {
            h <- r$hourly
            h$subject <- r$subject_id
            all_hourly <- rbind(all_hourly, h)
          }
        }
        req(nrow(all_hourly) > 0)

        avg_hourly <- aggregate(wear_pct ~ hour, all_hourly, mean, na.rm = TRUE)

        ggplot(avg_hourly, aes(x = hour, y = wear_pct)) +
          geom_area(fill = "#236192", alpha = 0.2) +
          geom_line(color = "#236192", linewidth = 1.5) +
          geom_point(color = "#FFCD00", size = 2.5) +
          scale_x_continuous(breaks = seq(0, 23, 3), labels = sprintf("%02d", seq(0, 23, 3))) +
          scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
          labs(title = "Average Hourly Wear Pattern",
               subtitle = paste(length(res), "files"),
               x = "Hour of Day", y = "Wear (%)") +
          canhrActi::theme_canhrActi() +
          theme(plot.title = element_text(face = "bold", size = 16),
                plot.subtitle = element_text(color = "#64748b"),
                panel.grid.minor = element_blank())

      } else if (sel %in% names(res)) {
        hourly <- res[[sel]]$hourly
        req(hourly)

        ggplot(hourly, aes(x = hour, y = wear_pct)) +
          geom_area(fill = "#236192", alpha = 0.2) +
          geom_line(color = "#236192", linewidth = 1.5) +
          geom_point(color = "#FFCD00", size = 2.5) +
          scale_x_continuous(breaks = seq(0, 23, 3), labels = sprintf("%02d", seq(0, 23, 3))) +
          scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
          labs(title = paste("Hourly Pattern -", res[[sel]]$subject_id),
               x = "Hour of Day", y = "Wear (%)") +
          canhrActi::theme_canhrActi() +
          theme(plot.title = element_text(face = "bold", size = 16),
                panel.grid.minor = element_blank())
      }
    })

    # Wear periods table
    output$wear_periods_table <- DT::renderDataTable({
      res <- results()
      sel <- input$chart_file

      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run validation first"), rownames = FALSE, options = list(dom = 't')))
      }

      if (sel == "all" || sel == "none") {
        all_periods <- data.frame()
        for (r in res) {
          if (!is.null(r$wear_periods) && nrow(r$wear_periods) > 0) {
            p <- r$wear_periods
            p$subject <- r$subject_id
            all_periods <- rbind(all_periods, p)
          }
        }
        if (nrow(all_periods) == 0) {
          return(DT::datatable(data.frame(Message = "No wear periods detected"), rownames = FALSE, options = list(dom = 't')))
        }
        display_df <- data.frame(
          Subject = all_periods$subject,
          Date = format(all_periods$start_time, "%Y-%m-%d"),
          Start = format(all_periods$start_time, "%H:%M"),
          End = format(all_periods$end_time, "%H:%M"),
          Duration = paste0(round(all_periods$duration_minutes / 60, 1), "h"),
          stringsAsFactors = FALSE
        )

        DT::datatable(display_df,
                      options = list(pageLength = 10, dom = 'tip', scrollX = FALSE),
                      rownames = FALSE)
      } else if (sel %in% names(res)) {
        periods <- res[[sel]]$wear_periods
        if (is.null(periods) || nrow(periods) == 0) {
          return(DT::datatable(data.frame(Message = "No wear periods detected"), rownames = FALSE, options = list(dom = 't')))
        }
        display_df <- data.frame(
          Period = periods$period,
          Date = format(periods$start_time, "%Y-%m-%d"),
          Start = format(periods$start_time, "%H:%M"),
          End = format(periods$end_time, "%H:%M"),
          Duration = paste0(round(periods$duration_minutes / 60, 1), "h"),
          stringsAsFactors = FALSE
        )

        DT::datatable(display_df,
                      options = list(pageLength = 10, dom = 'tip', scrollX = FALSE),
                      rownames = FALSE)
      }
    })

    # Periods statistics summary
    output$periods_stats <- renderUI({
      res <- results()
      sel <- input$chart_file

      if (length(res) == 0) {
        return(empty_state(
          title = NULL,
          message = "Run validation first",
          small = TRUE,
          show_icon = FALSE,
          include_base = FALSE
        ))
      }

      if (sel == "all" || sel == "none") {
        total_wear_periods <- sum(sapply(res, function(r) {
          if (!is.null(r$wear_periods)) nrow(r$wear_periods) else 0
        }))
        total_nonwear_periods <- sum(sapply(res, function(r) r$n_nonwear_periods))
        avg_wear_dur <- mean(sapply(res, function(r) r$avg_wear_period_sec / 60), na.rm = TRUE)
        avg_nonwear_dur <- mean(sapply(res, function(r) r$avg_nonwear_period_sec / 60), na.rm = TRUE)
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        total_wear_periods <- if (!is.null(r$wear_periods)) nrow(r$wear_periods) else 0
        total_nonwear_periods <- r$n_nonwear_periods
        avg_wear_dur <- r$avg_wear_period_sec / 60
        avg_nonwear_dur <- r$avg_nonwear_period_sec / 60
      } else {
        return(NULL)
      }

      tagList(
        div(class = "stat-row",
          span(class = "stat-label", "Wear Periods:"),
          span(class = "stat-value", total_wear_periods)
        ),
        div(class = "stat-row",
          span(class = "stat-label", "Non-Wear Periods:"),
          span(class = "stat-value", total_nonwear_periods)
        ),
        div(class = "stat-row",
          span(class = "stat-label", "Avg Wear Duration:"),
          span(class = "stat-value", paste0(round(avg_wear_dur, 0), " min"))
        ),
        div(class = "stat-row",
          span(class = "stat-label", "Avg Non-Wear Duration:"),
          span(class = "stat-value", paste0(round(avg_nonwear_dur, 0), " min"))
        )
      )
    })

    # Daily summary table
    output$daily_summary_table <- DT::renderDataTable({
      res <- results()
      sel <- input$chart_file

      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run validation first"), rownames = FALSE, options = list(dom = 't')))
      }

      if (sel == "all" || sel == "none") {
        all_daily <- data.frame()
        for (r in res) {
          if (!is.null(r$daily)) {
            d <- r$daily
            d$subject <- r$subject_id
            all_daily <- rbind(all_daily, d)
          }
        }
        if (nrow(all_daily) == 0) {
          return(DT::datatable(data.frame(Message = "No daily data"), rownames = FALSE, options = list(dom = 't')))
        }
        display_df <- data.frame(
          Subject = all_daily$subject,
          Date = format(all_daily$date, "%Y-%m-%d"),
          Day = all_daily$weekday,
          Wear_Hours = round(all_daily$wear_hours, 1),
          Status = ifelse(all_daily$valid, "Valid", "Invalid"),
          stringsAsFactors = FALSE
        )

        DT::datatable(display_df,
                      options = list(pageLength = 10, dom = 'tip', scrollX = FALSE),
                      rownames = FALSE,
                      colnames = c("Subject", "Date", "Day", "Wear (h)", "Status")) %>%
          DT::formatStyle(
            "Status",
            backgroundColor = DT::styleEqual(c("Valid", "Invalid"), c("#d4edda", "#f8f9fa")),
            color = DT::styleEqual(c("Valid", "Invalid"), c("#155724", "#6c757d"))
          )
      } else if (sel %in% names(res)) {
        daily <- res[[sel]]$daily
        if (is.null(daily)) {
          return(DT::datatable(data.frame(Message = "No daily data"), rownames = FALSE, options = list(dom = 't')))
        }
        display_df <- data.frame(
          Date = format(daily$date, "%Y-%m-%d"),
          Day = daily$weekday,
          Wear_Hours = round(daily$wear_hours, 1),
          Wear_Min = round(daily$wear_min, 0),
          Status = ifelse(daily$valid, "Valid", "Invalid"),
          stringsAsFactors = FALSE
        )

        DT::datatable(display_df,
                      options = list(pageLength = 10, dom = 'tip', scrollX = FALSE),
                      rownames = FALSE,
                      colnames = c("Date", "Day", "Wear (h)", "Wear (min)", "Status")) %>%
          DT::formatStyle(
            "Status",
            backgroundColor = DT::styleEqual(c("Valid", "Invalid"), c("#d4edda", "#f8f9fa")),
            color = DT::styleEqual(c("Valid", "Invalid"), c("#155724", "#6c757d"))
          )
      }
    })

  })
}
