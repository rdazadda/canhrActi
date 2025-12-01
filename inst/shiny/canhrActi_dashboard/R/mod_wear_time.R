# Module

mod_wear_time_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Page Header
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: linear-gradient(135deg, #236192 0%, #1a4a6f 100%); color: white; padding: 20px 25px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 4px 15px rgba(35, 97, 146, 0.3);",
          fluidRow(
            column(
              width = 8,
              h3(style = "margin: 0 0 8px 0; font-weight: 600;",
                 icon("clock", style = "margin-right: 10px;"),
                 "Wear Time Validation"),
              p(style = "margin: 0; opacity: 0.9; font-size: 13px;",
                "Detect periods when the device was worn vs. non-wear. Essential for accurate activity analysis.")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right;",
                tags$span(
                  style = "display: inline-block; background: rgba(255,205,0,0.2); padding: 8px 15px; border-radius: 20px; font-size: 11px; border: 1px solid rgba(255,205,0,0.4);",
                  icon("lightbulb", style = "color: #FFCD00; margin-right: 6px;"),
                  "Run this before Physical Activity analysis"
                )
              )
            )
          )
        )
      )
    ),

    # Summary Statistics at top
    fluidRow(
      valueBoxOutput(ns("vb_files_validated"), width = 3),
      valueBoxOutput(ns("vb_total_valid_days"), width = 3),
      valueBoxOutput(ns("vb_avg_wear_time"), width = 3),
      valueBoxOutput(ns("vb_avg_wear_pct"), width = 3)
    ),

    fluidRow(
      # Left column - Parameters
      column(
        width = 4,

        # Algorithm Selection
        box(
          title = span(icon("cogs", style = "margin-right: 8px;"), "Algorithm Selection"),
          status = "primary", solidHeader = TRUE, width = NULL,
          div(
            style = "background: rgba(35,97,146,0.05); border-radius: 8px; padding: 12px; margin-bottom: 10px;",
            selectInput(ns("algorithm"), "Detection Algorithm:",
                        choices = c("Choi (2011)" = "choi",
                                    "Troiano (2008)" = "troiano",
                                    "CANHR (2025)" = "canhr"),
                        selected = "choi")
          ),
          radioButtons(ns("param_mode"), "Parameters:",
                       choices = c("Default", "Custom"),
                       selected = "Default", inline = TRUE)
        ),

        # Define Non-Wear Period
        box(
          title = span(icon("ban", style = "margin-right: 8px;"), "Non-Wear Definition"),
          status = "info", width = NULL,

          div(
            style = "font-size: 12px;",
            fluidRow(
              column(6, tags$label("Minimum Length:", style = "line-height: 34px;")),
              column(4, numericInput(ns("min_length"), NULL, value = 90, min = 30, max = 180, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            fluidRow(
              column(6, tags$label("Small Window:", style = "line-height: 34px;")),
              column(4, numericInput(ns("small_window"), NULL, value = 30, min = 10, max = 60, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            fluidRow(
              column(6, tags$label("Spike Tolerance:", style = "line-height: 34px;")),
              column(4, numericInput(ns("spike_tolerance"), NULL, value = 2, min = 0, max = 10, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            fluidRow(
              column(6, tags$label("Spike Stop Level:", style = "line-height: 34px;")),
              column(4, numericInput(ns("spike_stoplevel"), NULL, value = 100, min = 0, max = 500, width = "100%")),
              column(2, tags$span("CPM", style = "line-height: 34px; color: #666;"))
            ),

            hr(style = "margin: 10px 0; border-color: #e8f4fc;"),
            checkboxInput(ns("use_vm"), span(icon("cube"), " Use Vector Magnitude"), value = FALSE)
          )
        ),

        # Optional Screen Parameters
        box(
          title = span(icon("filter", style = "margin-right: 8px;"), "Validity Criteria"),
          status = "warning", width = NULL, collapsible = TRUE,

          div(
            style = "font-size: 11px;",
            fluidRow(
              column(1, checkboxInput(ns("use_ignore_short"), NULL, value = FALSE)),
              column(5, tags$label("Ignore wear < :", style = "line-height: 34px;")),
              column(4, numericInput(ns("ignore_short_min"), NULL, value = 0, min = 0, max = 60, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            fluidRow(
              column(1, checkboxInput(ns("use_min_wear"), NULL, value = TRUE)),
              column(5, tags$label("Min wear/day:", style = "line-height: 34px;")),
              column(4, numericInput(ns("min_wear_day"), NULL, value = 600, min = 0, max = 1440, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_min_wear")),
              div(
                style = "background: rgba(255,205,0,0.1); border-radius: 6px; padding: 10px; margin-top: 10px;",
                fluidRow(
                  column(6, tags$label("Min valid days:", style = "padding-left: 10px; line-height: 34px;")),
                  column(6, numericInput(ns("min_valid_days"), NULL, value = 3, min = 0, max = 14, width = "100%"))
                ),
                fluidRow(
                  column(6, tags$label("Min weekdays:", style = "padding-left: 10px; line-height: 34px;")),
                  column(6, numericInput(ns("min_weekdays"), NULL, value = 0, min = 0, max = 5, width = "100%"))
                ),
                fluidRow(
                  column(6, tags$label("Min weekend:", style = "padding-left: 10px; line-height: 34px;")),
                  column(6, numericInput(ns("min_weekend"), NULL, value = 0, min = 0, max = 2, width = "100%"))
                )
              )
            ),

            hr(style = "margin: 10px 0; border-color: #e8f4fc;"),

            fluidRow(
              column(5, tags$label("Sleep Period Options", style = "line-height: 34px;")),
              column(7, selectInput(ns("sleep_option"), NULL,
                                    choices = c("Ignore" = "ignore",
                                                "Mark As Wear Time" = "wear",
                                                "Mark As Non Wear Time" = "nonwear"),
                                    selected = "nonwear", width = "100%"))
            )
          )
        ),

        # Run Analysis button
        box(
          width = NULL,
          style = "background: linear-gradient(135deg, #f8fafc 0%, #e8f4fc 100%);",
          actionButton(ns("run_btn"), span(icon("check-circle"), " Validate Wear Time"),
                       class = "btn-success btn-block btn-lg",
                       style = "font-size: 16px; padding: 15px; background: linear-gradient(135deg, #236192 0%, #1a4a6f 100%); border: none;"),
          hr(style = "border-color: #b8d4e8; margin: 12px 0;"),
          actionButton(ns("load_defaults"), span(icon("undo"), " Reset Defaults"),
                       class = "btn-default btn-block", style = "font-size: 12px;")
        )
      ),

      # Right column - Results
      column(
        width = 8,

        # Files table with validation status
        box(
          title = span(icon("list-alt", style = "margin-right: 8px;"), "Files & Validation Status"),
          status = "primary", solidHeader = TRUE, width = NULL,
          fluidRow(
            column(9, tags$span(textOutput(ns("files_loaded_text")),
                               style = "font-weight: 500; padding-top: 5px; color: #236192;")),
            column(3, actionButton(ns("validate_selected"), span(icon("check"), " Validate Selected"),
                                  class = "btn-info btn-sm pull-right",
                                  style = "background: linear-gradient(135deg, #FFCD00 0%, #e6b800 100%); border: none; color: #0d2137;"))
          ),
          hr(style = "margin: 10px 0; border-color: #e8f4fc;"),
          DT::dataTableOutput(ns("files_table"))
        ),

        # File selector for all charts
        box(
          title = NULL, width = 12,
          style = "background: linear-gradient(90deg, rgba(35,97,146,0.05) 0%, transparent 100%);",
          fluidRow(
            column(4, selectInput(ns("chart_file"), span(icon("eye"), " View File:"),
                                 choices = c("All Files (Combined)" = "all"))),
            column(8, tags$div(style = "padding-top: 25px;", uiOutput(ns("chart_summary"))))
          )
        ),

        # Main visualization tabs
        tabBox(
          title = NULL, width = 12, id = ns("viz_tabs"),

          # Tab 1: Daily Summary
          tabPanel(
            title = tagList(icon("chart-bar"), " Daily Summary"),
            value = "daily",
            div(style = "background: white; border-radius: 8px; padding: 10px;",
              plotOutput(ns("daily_chart"), height = "350px")
            )
          ),

          # Tab 2: Hourly Pattern
          tabPanel(
            title = tagList(icon("chart-line"), " Hourly Pattern"),
            value = "hourly",
            div(style = "background: white; border-radius: 8px; padding: 10px;",
              plotOutput(ns("hourly_pattern"), height = "300px")
            )
          )
        ),

        # Detailed Results Tables
        tabBox(
          title = NULL, width = 12,
          tabPanel(tagList(icon("clock"), " Wear Periods"),
                   DT::dataTableOutput(ns("wear_periods_table"))),
          tabPanel(tagList(icon("calendar"), " Daily Summary"),
                   DT::dataTableOutput(ns("daily_summary_table")))
        )
      )
    )
  )
}

mod_wear_time_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Load default parameters based on algorithm
    observeEvent(input$algorithm, {
      if (input$param_mode == "Default") {
        load_algorithm_defaults()
      }
    })

    observeEvent(input$load_defaults, {
      load_algorithm_defaults()
      updateRadioButtons(session, "param_mode", selected = "Default")
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
      paste("Files loaded:", shared$file_count)
    })

    # Helper function to format duration as "D H M S" like ActiLife
    format_duration <- function(total_seconds) {
      if (is.na(total_seconds) || total_seconds == 0) return("0 0 0 0")
      days <- floor(total_seconds / 86400)
      hours <- floor((total_seconds %% 86400) / 3600)
      mins <- floor((total_seconds %% 3600) / 60)
      secs <- floor(total_seconds %% 60)
      paste(days, hours, mins, secs)
    }

    # Files table - ActiLife style with all columns
    output$files_table <- DT::renderDataTable({
      if (shared$file_count == 0) {
        return(DT::datatable(data.frame(Message = "No files loaded. Go to Data Upload tab."), rownames = FALSE))
      }

      res <- results()

      df <- data.frame(
        file_id = names(shared$files),
        subject_name = sapply(shared$files, function(f) f$subject_info$id %||% "N/A"),
        serial_number = sapply(shared$files, function(f) f$device_info$serial_number %||% "N/A"),
        validated = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            paste0("Yes (", format(res[[fid]]$validated_at, "%m/%d/%Y"), ")")
          } else "No"
        }),
        has_wear_sensor = sapply(shared$files, function(f) {
          if (!is.null(f$device_info$has_wear_sensor) && f$device_info$has_wear_sensor) "Yes" else "No"
        }),
        wear_periods = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && !is.null(res[[fid]]$wear_periods)) nrow(res[[fid]]$wear_periods) else NA
        }),
        nonwear_periods = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) res[[fid]]$n_nonwear_periods else NA
        }),
        total_length = sapply(names(shared$files), function(fid) {
          f <- shared$files[[fid]]
          total_sec <- f$n_epochs * f$epoch_length
          format_duration(total_sec)
        }),
        wear_length = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            f <- shared$files[[fid]]
            wear_sec <- res[[fid]]$wear_epochs * f$epoch_length
            format_duration(wear_sec)
          } else "N/A"
        }),
        nonwear_length = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            f <- shared$files[[fid]]
            nonwear_sec <- res[[fid]]$nonwear_epochs * f$epoch_length
            format_duration(nonwear_sec)
          } else "N/A"
        }),
        avg_wear_period = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            format_duration(res[[fid]]$avg_wear_period_sec)
          } else "N/A"
        }),
        avg_nonwear_period = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            format_duration(res[[fid]]$avg_nonwear_period_sec)
          } else "N/A"
        }),
        wear_pct = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) round(res[[fid]]$wear_pct, 1) else NA
        }),
        nonwear_pct = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) round(100 - res[[fid]]$wear_pct, 1) else NA
        }),
        avg_wear_per_total = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && res[[fid]]$total_days > 0) {
            avg_sec <- (res[[fid]]$total_wear * 3600) / res[[fid]]$total_days
            format_duration(avg_sec)
          } else "N/A"
        }),
        avg_wear_per_wear_days = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && res[[fid]]$days_with_wear > 0) {
            avg_sec <- (res[[fid]]$total_wear * 3600) / res[[fid]]$days_with_wear
            format_duration(avg_sec)
          } else "N/A"
        }),
        avg_nonwear_per_total = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && res[[fid]]$total_days > 0) {
            avg_sec <- (res[[fid]]$total_nonwear * 3600) / res[[fid]]$total_days
            format_duration(avg_sec)
          } else "N/A"
        }),
        avg_nonwear_per_wear_days = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && res[[fid]]$days_with_wear > 0) {
            avg_sec <- (res[[fid]]$total_nonwear * 3600) / res[[fid]]$days_with_wear
            format_duration(avg_sec)
          } else "N/A"
        }),
        calendar_days_wear = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) res[[fid]]$days_with_wear else NA
        }),
        calendar_weekdays_wear = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) res[[fid]]$weekdays_with_wear else NA
        }),
        calendar_weekend_wear = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) res[[fid]]$weekend_with_wear else NA
        }),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df[, -1],  # Hide file_id column
        selection = "multiple",
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = FALSE,
                       columnDefs = list(list(width = '80px', targets = "_all"))),
        rownames = FALSE,
        colnames = c("Subject Name", "Serial Number", "Validated?", "Has Wear Sensor?",
                     "Wear Periods", "Non-Wear Periods", "Total Length", "Wear Length",
                     "Non-Wear Length", "Avg Wear Period", "Avg Non-Wear Period",
                     "Wear %", "Non-Wear %", "Avg Wear/Total Days", "Avg Wear/Wear Days",
                     "Avg Non-Wear/Total Days", "Avg Non-Wear/Wear Days",
                     "Days With Wear", "Weekdays With Wear", "Weekend Days With Wear")
      ) %>%
        DT::formatStyle("validated",
                        color = DT::styleEqual("No", "#c0392b"),
                        fontWeight = DT::styleEqual("No", "bold"))
    })

    # Run validation on all files
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)
      run_validation(names(shared$files))
    })

    # Validate selected files only
    observeEvent(input$validate_selected, {
      selected <- input$files_table_rows_selected
      req(selected)
      file_ids <- names(shared$files)[selected]
      run_validation(file_ids)
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

      # Get counts column to use
      count_col <- if (input$use_vm) "vector_magnitude" else "axis1"

      # Convert min wear per day from minutes to hours
      min_wear_hours <- input$min_wear_day / 60

      start_time <- Sys.time()

      withProgress(message = "Validating wear time...", value = 0, {
        for (i in seq_along(file_ids)) {
          fid <- file_ids[i]
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

          # Get counts data
          counts <- if (count_col %in% names(data)) data[[count_col]] else data$axis1

          # Convert parameters from minutes to epochs based on epoch length
          # The wear algorithms assume 1-minute epochs, so we need to convert
          epoch_minutes <- f$epoch_length / 60  # Convert epoch length to minutes
          non_wear_epochs <- as.integer(input$min_length / epoch_minutes)
          spike_tol_epochs <- as.integer(input$spike_tolerance / epoch_minutes)
          if (spike_tol_epochs < 1) spike_tol_epochs <- 1
          small_window_epochs <- as.integer(input$small_window / epoch_minutes)

          wear <- tryCatch({
            if (input$algorithm == "troiano") {
              wear.troiano(
                counts_per_minute = counts,
                non_wear_window = non_wear_epochs,
                spike_tolerance = spike_tol_epochs,
                spike_stoplevel = input$spike_stoplevel
              )
            } else if (input$algorithm == "choi") {
              wear.choi(
                counts_per_minute = counts,
                non_wear_window = non_wear_epochs,
                spike_tolerance = spike_tol_epochs,
                spike_stoplevel = input$spike_stoplevel,
                min_window_len = small_window_epochs
              )
            } else {
              wear.CANHR2025(
                counts_per_minute = counts,
                non_wear_window = non_wear_epochs,
                spike_tolerance = spike_tol_epochs,
                spike_stoplevel = input$spike_stoplevel,
                min_window_len = small_window_epochs
              )
            }
          }, error = function(e) {
            showNotification(paste("Error in", f$name, ":", e$message), type = "error")
            return(NULL)
          })

          if (is.null(wear)) next

          # Daily summary
          daily <- NULL
          hourly <- NULL
          wear_periods <- NULL
          nonwear_periods <- NULL

          if ("timestamp" %in% names(data)) {
            temp <- data
            temp$wear <- wear
            temp$date <- as.Date(temp$timestamp)
            temp$hour <- as.numeric(format(temp$timestamp, "%H"))
            temp$weekday <- weekdays(temp$timestamp)
            temp$is_weekend <- temp$weekday %in% c("Saturday", "Sunday")

            # Daily aggregation
            daily <- aggregate(wear ~ date, temp, sum)
            daily$wear_hours <- daily$wear * f$epoch_length / 3600
            daily$wear_min <- daily$wear_hours * 60
            daily$valid <- daily$wear_min >= input$min_wear_day
            daily$weekday <- weekdays(as.Date(daily$date))
            daily$is_weekend <- daily$weekday %in% c("Saturday", "Sunday")

            # Count valid weekdays and weekend days
            valid_weekdays <- sum(daily$valid & !daily$is_weekend)
            valid_weekend <- sum(daily$valid & daily$is_weekend)

            # Hourly wear pattern
            hourly <- aggregate(wear ~ hour, temp, mean)
            hourly$wear_pct <- hourly$wear * 100

            # Detect wear periods using the package function for consistency
            # Use get.wear.periods() which matches command-line results
            wear_periods <- get.wear.periods(wear, temp$timestamp, epoch_length = f$epoch_length)

            # Apply optional filtering if user enabled "ignore short periods"
            if (input$use_ignore_short && input$ignore_short_min > 0) {
              wear_periods <- wear_periods[wear_periods$duration_minutes >= input$ignore_short_min, ]
              if (nrow(wear_periods) > 0) {
                wear_periods$period <- seq_len(nrow(wear_periods))
              }
            }

            # Detect non-wear periods (use the non-wear window as minimum duration)
            # ActiLife counts non-wear periods that meet the algorithm's non-wear window criteria
            nonwear_periods <- detect_wear_periods(temp$timestamp, !wear, f$epoch_length, input$min_length)
          }

          # Check minimum requirements
          valid_days <- if (!is.null(daily)) sum(daily$valid) else 0
          total_days <- if (!is.null(daily)) nrow(daily) else 0

          # Calculate days with meaningful wear time
          # Use the user's "Minimum wear time per day" setting to match ActiLife behavior
          # ActiLife's "Calendar Days With Wear Time" counts days meeting the minimum threshold
          min_wear_for_day <- input$min_wear_day  # Use user's configured minimum
          days_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day) else 0
          weekdays_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day & !daily$is_weekend) else 0
          weekend_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day & daily$is_weekend) else 0

          # Calculate average wear/non-wear period durations
          n_wear_periods <- if (!is.null(wear_periods) && nrow(wear_periods) > 0) nrow(wear_periods) else 0
          n_nonwear_periods <- if (!is.null(nonwear_periods) && nrow(nonwear_periods) > 0) nrow(nonwear_periods) else 0

          avg_wear_period_sec <- if (n_wear_periods > 0) {
            mean(wear_periods$duration_minutes, na.rm = TRUE) * 60
          } else 0

          avg_nonwear_period_sec <- if (n_nonwear_periods > 0) {
            mean(nonwear_periods$duration_min, na.rm = TRUE) * 60
          } else 0

          meets_criteria <- TRUE
          if (input$use_min_wear) {
            if (valid_days < input$min_valid_days) meets_criteria <- FALSE
            if (valid_weekdays < input$min_weekdays) meets_criteria <- FALSE
            if (valid_weekend < input$min_weekend) meets_criteria <- FALSE
          }

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
            valid_weekdays = if (exists("valid_weekdays")) valid_weekdays else 0,
            valid_weekend = if (exists("valid_weekend")) valid_weekend else 0,
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

        # Memory cleanup
        gc(verbose = FALSE)
      })

      results(all_results)
      shared$results$wear_time <- all_results

      showNotification(paste("Wear time validation complete for", length(file_ids), "files!"), type = "message")
    }

    # Helper function to detect continuous wear periods
    # min_duration_min: minimum duration in minutes for a period to be counted
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
          # Only add periods that meet minimum duration
          if (duration >= min_duration_min) {
            periods <- rbind(periods, data.frame(
              start = timestamps[start_idx],
              end = timestamps[i - 1],
              duration_min = duration
            ))
          }
        }
      }

      # Handle case where wear continues to end
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

    # Value boxes
    output$vb_files_validated <- renderValueBox({
      res <- results()
      n <- length(res)
      valueBox(n, "Files Validated", icon = icon("check-circle"), color = if (n > 0) "green" else "red")
    })

    output$vb_total_valid_days <- renderValueBox({
      res <- results()
      if (length(res) == 0) {
        valueBox("--", "Total Valid Days", icon = icon("calendar-check"), color = "blue")
      } else {
        total <- sum(sapply(res, function(r) r$valid_days))
        valueBox(total, "Total Valid Days", icon = icon("calendar-check"), color = "blue")
      }
    })

    output$vb_avg_wear_time <- renderValueBox({
      res <- results()
      if (length(res) == 0) {
        valueBox("--", "Avg Wear Time", icon = icon("clock"), color = "yellow")
      } else {
        avg <- mean(sapply(res, function(r) r$avg_wear), na.rm = TRUE)
        valueBox(paste0(round(avg, 1), "h"), "Avg Wear Time", icon = icon("clock"), color = "yellow")
      }
    })

    output$vb_avg_wear_pct <- renderValueBox({
      res <- results()
      if (length(res) == 0) {
        valueBox("--", "Avg Wear %", icon = icon("percent"), color = "purple")
      } else {
        avg <- mean(sapply(res, function(r) r$wear_pct), na.rm = TRUE)
        valueBox(paste0(round(avg, 1), "%"), "Avg Wear %", icon = icon("percent"), color = "purple")
      }
    })

    # Chart summary
    output$chart_summary <- renderUI({
      res <- results()
      sel <- input$chart_file
      if (length(res) == 0 || sel == "none") return(NULL)

      if (sel == "all") {
        total_valid <- sum(sapply(res, function(r) r$valid_days))
        total_days <- sum(sapply(res, function(r) r$total_days))
        tags$span(paste0(total_valid, " / ", total_days, " valid days across all files"))
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        tags$span(paste0(r$valid_days, " / ", r$total_days, " valid days | ",
                        r$valid_weekdays, " weekdays | ", r$valid_weekend, " weekend days"))
      }
    })

    # Daily chart (Enhanced with weekday/weekend distinction)
    output$daily_chart <- renderPlot({
      res <- results()
      req(length(res) > 0)

      sel <- input$chart_file
      min_wear_hours <- input$min_wear_day / 60

      if (sel == "all" || sel == "none") {
        # Combine all daily data
        all_daily <- data.frame()
        for (r in res) {
          if (!is.null(r$daily)) {
            d <- r$daily
            d$subject <- r$subject_id
            all_daily <- rbind(all_daily, d)
          }
        }
        req(nrow(all_daily) > 0)

        # Add day labels
        all_daily$day_label <- format(all_daily$date, "%a\n%m/%d")
        all_daily$day_type <- ifelse(all_daily$is_weekend, "Weekend", "Weekday")

        ggplot(all_daily, aes(x = date, y = wear_hours, fill = interaction(valid, day_type))) +
          geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
          geom_hline(yintercept = min_wear_hours, linetype = "dashed", color = "#c0392b", linewidth = 1.2) +
          annotate("text", x = min(all_daily$date), y = min_wear_hours + 0.5,
                   label = paste0(min_wear_hours, "h minimum"), hjust = 0, size = 3, color = "#c0392b") +
          scale_fill_manual(
            values = c("TRUE.Weekday" = "#27ae60", "FALSE.Weekday" = "#95a5a6",
                       "TRUE.Weekend" = "#2980b9", "FALSE.Weekend" = "#bdc3c7"),
            labels = c("TRUE.Weekday" = "Valid Weekday", "FALSE.Weekday" = "Invalid Weekday",
                       "TRUE.Weekend" = "Valid Weekend", "FALSE.Weekend" = "Invalid Weekend"),
            name = "") +
          scale_y_continuous(limits = c(0, 26), breaks = seq(0, 24, 4),
                             sec.axis = sec_axis(~./24*100, name = "% of Day", breaks = seq(0, 100, 25))) +
          facet_wrap(~subject, scales = "free_x", ncol = 2) +
          labs(title = "Daily Wear Time Summary",
               subtitle = paste("Minimum valid day:", min_wear_hours, "hours"),
               x = "", y = "Wear Time (hours)") +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
                legend.position = "top",
                legend.box = "horizontal",
                strip.background = element_rect(fill = "#34495e", color = NA),
                strip.text = element_text(color = "white", face = "bold", size = 11),
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(color = "#7f8c8d"))

      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        daily <- r$daily
        req(daily)

        # Add day labels
        daily$day_label <- paste0(format(daily$date, "%a"), "\n", format(daily$date, "%m/%d"))
        daily$day_type <- ifelse(daily$is_weekend, "Weekend", "Weekday")

        ggplot(daily, aes(x = factor(day_label, levels = day_label), y = wear_hours,
                          fill = interaction(valid, day_type))) +
          geom_bar(stat = "identity", width = 0.75, color = "white", linewidth = 0.5) +
          geom_hline(yintercept = min_wear_hours, linetype = "dashed", color = "#c0392b", linewidth = 1.2) +
          geom_text(aes(label = sprintf("%.1fh", wear_hours)), vjust = -0.3, size = 3.5, fontface = "bold") +
          scale_fill_manual(
            values = c("TRUE.Weekday" = "#27ae60", "FALSE.Weekday" = "#95a5a6",
                       "TRUE.Weekend" = "#2980b9", "FALSE.Weekend" = "#bdc3c7"),
            labels = c("TRUE.Weekday" = "Valid Weekday", "FALSE.Weekday" = "Invalid Weekday",
                       "TRUE.Weekend" = "Valid Weekend", "FALSE.Weekend" = "Invalid Weekend"),
            name = "") +
          scale_y_continuous(limits = c(0, 28), breaks = seq(0, 24, 4),
                             sec.axis = sec_axis(~./24*100, name = "% of Day", breaks = seq(0, 100, 25))) +
          labs(title = paste("Daily Wear Time -", r$subject_id),
               subtitle = paste("Algorithm:", toupper(r$algorithm), "| Minimum:", min_wear_hours, "hours |",
                               sum(daily$valid), "of", nrow(daily), "days valid"),
               x = "", y = "Wear Time (hours)") +
          theme_minimal(base_size = 13) +
          theme(axis.text.x = element_text(size = 10),
                legend.position = "top",
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                plot.title = element_text(face = "bold", size = 15),
                plot.subtitle = element_text(color = "#7f8c8d"))
      }
    })

    # Wear periods table
    output$wear_periods_table <- DT::renderDataTable({
      res <- results()
      sel <- input$chart_file

      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run validation first"), rownames = FALSE))
      }

      if (sel == "all" || sel == "none") {
        # Combine all wear periods
        all_periods <- data.frame()
        for (r in res) {
          if (!is.null(r$wear_periods) && nrow(r$wear_periods) > 0) {
            p <- r$wear_periods
            p$subject <- r$subject_id
            all_periods <- rbind(all_periods, p)
          }
        }
        if (nrow(all_periods) == 0) {
          return(DT::datatable(data.frame(Message = "No wear periods detected"), rownames = FALSE))
        }
        # Format for display (get.wear.periods returns start_time, end_time, duration_minutes)
        display_df <- data.frame(
          subject = all_periods$subject,
          start = format(all_periods$start_time, "%Y-%m-%d %H:%M"),
          end = format(all_periods$end_time, "%Y-%m-%d %H:%M"),
          duration_min = round(all_periods$duration_minutes, 1),
          stringsAsFactors = FALSE
        )

        DT::datatable(display_df,
                      options = list(pageLength = 15, scrollX = TRUE),
                      rownames = FALSE,
                      colnames = c("Subject", "Start", "End", "Duration (min)"))
      } else if (sel %in% names(res)) {
        periods <- res[[sel]]$wear_periods
        if (is.null(periods) || nrow(periods) == 0) {
          return(DT::datatable(data.frame(Message = "No wear periods detected"), rownames = FALSE))
        }
        # Format for display (get.wear.periods returns start_time, end_time, duration_minutes)
        display_df <- data.frame(
          period = periods$period,
          start = format(periods$start_time, "%Y-%m-%d %H:%M"),
          end = format(periods$end_time, "%Y-%m-%d %H:%M"),
          duration_min = round(periods$duration_minutes, 1),
          stringsAsFactors = FALSE
        )

        DT::datatable(display_df,
                      options = list(pageLength = 15, scrollX = TRUE),
                      rownames = FALSE,
                      colnames = c("Period", "Start", "End", "Duration (min)"))
      }
    })

    # Daily summary table
    output$daily_summary_table <- DT::renderDataTable({
      res <- results()
      sel <- input$chart_file

      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run validation first"), rownames = FALSE))
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
          return(DT::datatable(data.frame(Message = "No daily data"), rownames = FALSE))
        }
        all_daily$date <- format(all_daily$date, "%Y-%m-%d")
        all_daily$wear_hours <- round(all_daily$wear_hours, 2)
        all_daily$wear_min <- round(all_daily$wear_min, 0)
        all_daily$valid <- ifelse(all_daily$valid, "Yes", "No")

        DT::datatable(all_daily[, c("subject", "date", "weekday", "wear_hours", "wear_min", "valid")],
                      options = list(pageLength = 15, scrollX = TRUE),
                      rownames = FALSE,
                      colnames = c("Subject", "Date", "Day", "Wear (h)", "Wear (min)", "Valid"))
      } else if (sel %in% names(res)) {
        daily <- res[[sel]]$daily
        if (is.null(daily)) {
          return(DT::datatable(data.frame(Message = "No daily data"), rownames = FALSE))
        }
        daily$date <- format(daily$date, "%Y-%m-%d")
        daily$wear_hours <- round(daily$wear_hours, 2)
        daily$wear_min <- round(daily$wear_min, 0)
        daily$valid <- ifelse(daily$valid, "Yes", "No")

        DT::datatable(daily[, c("date", "weekday", "wear_hours", "wear_min", "valid")],
                      options = list(pageLength = 15, scrollX = TRUE),
                      rownames = FALSE,
                      colnames = c("Date", "Day", "Wear (h)", "Wear (min)", "Valid"))
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
          geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
          geom_line(aes(group = 1), color = "#2c3e50", linewidth = 1) +
          scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d", seq(0, 23, 2))) +
          scale_y_continuous(limits = c(0, 100)) +
          labs(title = "Average Hourly Wear Pattern (All Files)",
               x = "Hour of Day", y = "Wear (%)") +
          theme_minimal(base_size = 12)

      } else if (sel %in% names(res)) {
        hourly <- res[[sel]]$hourly
        req(hourly)

        ggplot(hourly, aes(x = hour, y = wear_pct)) +
          geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
          geom_line(aes(group = 1), color = "#2c3e50", linewidth = 1) +
          scale_x_continuous(breaks = seq(0, 23, 2), labels = sprintf("%02d", seq(0, 23, 2))) +
          scale_y_continuous(limits = c(0, 100)) +
          labs(title = paste("Hourly Wear Pattern -", res[[sel]]$subject_id),
               x = "Hour of Day", y = "Wear (%)") +
          theme_minimal(base_size = 12)
      }
    })

  })
}

# Null coalesce
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
