# Module

mod_sleep_ui <- function(id) {

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
                 icon("moon", style = "margin-right: 10px;"),
                 "Sleep Analysis"),
              p(style = "margin: 0; opacity: 0.9; font-size: 13px;",
                "Detect and score sleep periods using validated algorithms. Analyze sleep efficiency, duration, and timing.")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right;",
                tags$span(
                  style = "display: inline-block; background: rgba(255,205,0,0.2); padding: 8px 15px; border-radius: 20px; font-size: 11px; border: 1px solid rgba(255,205,0,0.4);",
                  icon("info-circle", style = "color: #FFCD00; margin-right: 6px;"),
                  "Requires 60-second epoch data"
                )
              )
            )
          )
        )
      )
    ),

    # Summary Statistics at top
    fluidRow(
      valueBoxOutput(ns("vb_files_scored"), width = 3),
      valueBoxOutput(ns("vb_total_periods"), width = 3),
      valueBoxOutput(ns("vb_avg_efficiency"), width = 3),
      valueBoxOutput(ns("vb_avg_duration"), width = 3)
    ),

    fluidRow(
      # Left column - Options
      column(
        width = 4,

        # Sleep Period Scoring Options
        box(
          title = span(icon("bed", style = "margin-right: 8px;"), "Sleep Scoring Algorithm"),
          status = "primary", solidHeader = TRUE, width = NULL,

          div(
            style = "background: rgba(35,97,146,0.05); border-radius: 8px; padding: 12px;",
            selectInput(ns("algorithm"), "Select Algorithm:",
                        choices = c("Cole-Kripke (1992)" = "cole.kripke",
                                    "Sadeh (1994)" = "sadeh"),
                        selected = "cole.kripke")
          ),

          div(
            style = "background: linear-gradient(135deg, #e8f4fc 0%, #f8fafc 100%); border-radius: 8px; padding: 10px; margin-top: 10px; border-left: 3px solid #236192; font-size: 11px; color: #1a4a6f;",
            icon("info-circle", style = "color: #236192;"),
            " These algorithms require 60-second epochs."
          )
        ),

        # Sleep Period Detection Options
        box(
          title = span(icon("search", style = "margin-right: 8px;"), "Sleep Period Detection"),
          status = "info", width = NULL,

          div(
            style = "background: rgba(255,205,0,0.1); border-radius: 8px; padding: 12px; margin-bottom: 10px;",
            fluidRow(
              column(6, selectInput(ns("detection_method"), "Method:",
                                    choices = c("Tudor-Locke" = "tudor.locke"),
                                    selected = "tudor.locke")),
              column(6, radioButtons(ns("param_mode"), "Parameters:",
                                     choices = c("Default", "Custom"),
                                     selected = "Default", inline = TRUE))
            )
          ),

          div(
            style = "font-size: 12px;",
            h5(style = "color: #236192; font-weight: 600; margin-bottom: 12px;",
               icon("sliders-h"), " Sleep Period Definition"),

            fluidRow(
              column(6, tags$label("Min sleep period:", style = "line-height: 34px;")),
              column(4, numericInput(ns("min_sleep_period"), NULL, value = 160, min = 30, max = 480, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            fluidRow(
              column(6, tags$label("Bedtime definition:", style = "line-height: 34px;")),
              column(4, numericInput(ns("bedtime_start"), NULL, value = 5, min = 1, max = 30, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            ),

            fluidRow(
              column(6, tags$label("Wake time definition:", style = "line-height: 34px;")),
              column(4, numericInput(ns("wake_time_end"), NULL, value = 10, min = 1, max = 60, width = "100%")),
              column(2, tags$span("min", style = "line-height: 34px; color: #666;"))
            )
          )
        ),

        # Hidden inputs
        tags$div(style = "display: none;",
          numericInput(ns("max_sleep_period"), NULL, value = 1440),
          numericInput(ns("min_nonzero_epochs"), NULL, value = 0),
          checkboxInput(ns("use_min_nonzero"), NULL, value = FALSE)
        ),

        # Run Analysis Button
        box(
          width = NULL,
          style = "background: linear-gradient(135deg, #f8fafc 0%, #e8f4fc 100%);",
          actionButton(ns("run_btn"), span(icon("moon"), " Score Sleep Periods"),
                       class = "btn-success btn-block btn-lg",
                       style = "font-size: 16px; padding: 15px; background: linear-gradient(135deg, #236192 0%, #1a4a6f 100%); border: none;"),
          hr(style = "border-color: #b8d4e8; margin: 12px 0;"),
          div(
            style = "display: flex; flex-direction: column; gap: 8px;",
            downloadButton(ns("export_details"), span(icon("file-csv"), " Details"),
                          class = "btn-primary btn-block", style = "font-size: 12px;"),
            downloadButton(ns("export_summary"), span(icon("table"), " Summary"),
                          class = "btn-info btn-block", style = "font-size: 12px;")
          )
        )
      ),

      # Right column - Files and Results
      column(
        width = 8,

        # Files Table Header
        box(
          title = NULL, width = NULL,
          style = "background: linear-gradient(90deg, rgba(35,97,146,0.05) 0%, transparent 100%);",
          fluidRow(
            column(6,
              div(
                style = "display: inline-block; background: #236192; color: white; padding: 8px 15px; border-radius: 20px; font-weight: 500;",
                icon("file"), " ", textOutput(ns("files_loaded_text"), inline = TRUE)
              )
            ),
            column(6, align = "right",
              actionButton(ns("clear_results"), span(icon("trash"), " Clear Results"),
                          class = "btn-default btn-sm", style = "border: 1px solid #dc3545; color: #dc3545;")
            )
          )
        ),

        # Files Table
        box(
          title = span(icon("list-alt", style = "margin-right: 8px;"), "Files & Results"),
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
          DT::dataTableOutput(ns("files_table"))
        ),

        # Results tabs
        tabBox(
          title = NULL, width = 12, id = ns("results_tabs"),

          # Tab 1: Summary Table
          tabPanel(
            title = tagList(icon("table"), " Summary"),
            value = "summary",
            DT::dataTableOutput(ns("summary_table"))
          ),

          # Tab 2: Details Table
          tabPanel(
            title = tagList(icon("list"), " Details"),
            value = "details",
            DT::dataTableOutput(ns("details_table"))
          )
        )
      )
    )
  )
}

mod_sleep_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Load default parameters based on mode
    observeEvent(input$param_mode, {
      if (input$param_mode == "Default") {
        updateNumericInput(session, "min_sleep_period", value = 160)
        updateNumericInput(session, "bedtime_start", value = 5)
        updateNumericInput(session, "wake_time_end", value = 10)
        updateNumericInput(session, "max_sleep_period", value = 1440)
        updateNumericInput(session, "min_nonzero_epochs", value = 15)
      }
    })

    # Files loaded text
    output$files_loaded_text <- renderText({
      paste("Files loaded:", shared$file_count)
    })

    # Files table (ActiLife-style columns)
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
        `Subject Name` = sapply(shared$files, function(f) f$subject_info$id %||% "N/A"),
        `Serial Number` = sapply(shared$files, function(f) f$device_info$serial_number %||% "N/A"),
        Status = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) "Scored" else "Not Scored"
        }),
        `Sleep/Wake Algorithm` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) {
            alg <- res[[fid]]$algorithm
            if (alg == "cole.kripke") "Cole-Kripke" else if (alg == "sadeh") "Sadeh" else alg
          } else "-"
        }),
        `Sleep Period Detection` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) "Tudor-Locke" else "-"
        }),
        `Number of Sleep Periods` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) res[[fid]]$n_periods else NA
        }),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        selection = "multiple",
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE
      ) %>%
        DT::formatStyle("Status",
                        color = DT::styleEqual(c("Not Scored", "Scored"), c("#c0392b", "#27ae60")),
                        fontWeight = "bold")
    })

    # Helper: Format ETA
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    # Run sleep analysis
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      all_results <- list()
      n_files <- shared$file_count
      start_time <- Sys.time()

      withProgress(message = "Scoring sleep periods...", value = 0, {
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

          counts <- if ("axis1" %in% names(data)) data$axis1 else data[, 1]

          # Sleep state classification
          sleep_state <- tryCatch({
            if (input$algorithm == "cole.kripke") {
              canhrActi::sleep.cole.kripke(counts, apply_rescoring = TRUE)
            } else {
              canhrActi::sleep.sadeh(counts)
            }
          }, error = function(e) {
            showNotification(paste("Error in", f$name, ":", e$message), type = "error")
            return(NULL)
          })

          if (is.null(sleep_state)) next

          # Sleep periods detection using Tudor-Locke
          periods <- NULL
          if ("timestamp" %in% names(data)) {
            # Convert epoch-based parameters to actual values
            bedtime_epochs <- input$bedtime_start
            wake_epochs <- input$wake_time_end

            periods <- tryCatch({
              canhrActi::sleep.tudor.locke(
                sleep.state = sleep_state,
                timestamps = data$timestamp,
                counts = counts,
                bedtime_start = bedtime_epochs,
                wake_time_end = wake_epochs,
                min_sleep_period = input$min_sleep_period,
                max_sleep_period = input$max_sleep_period,
                min_nonzero_epochs = if (input$use_min_nonzero) input$min_nonzero_epochs else 0
              )
            }, error = function(e) {
              showNotification(paste("Period detection error in", f$name, ":", e$message), type = "warning")
              return(NULL)
            })
          }

          # Calculate summary stats
          n_periods <- if (!is.null(periods) && nrow(periods) > 0) nrow(periods) else 0
          avg_duration <- if (n_periods > 0) mean(periods$sleep_time, na.rm = TRUE) else NA
          avg_efficiency <- if (n_periods > 0) mean(periods$sleep_efficiency, na.rm = TRUE) else NA
          avg_awakenings <- if (n_periods > 0) mean(periods$number_of_awakenings, na.rm = TRUE) else NA
          avg_waso <- if (n_periods > 0) mean(periods$wake_time, na.rm = TRUE) else NA
          avg_latency <- NA
          if (n_periods > 0 && "onset" %in% names(periods) && "in_bed_time" %in% names(periods)) {
            latencies <- as.numeric(difftime(periods$onset, periods$in_bed_time, units = "mins"))
            avg_latency <- mean(latencies, na.rm = TRUE)
          }

          all_results[[fid]] <- list(
            file_id = fid,
            file_path = f$path,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            epoch_length = f$epoch_length,
            algorithm = input$algorithm,
            periods = periods,
            n_periods = n_periods,
            avg_duration = avg_duration,
            avg_efficiency = avg_efficiency,
            avg_awakenings = avg_awakenings,
            avg_waso = avg_waso,
            avg_latency = avg_latency,
            parameters = list(
              sleep_algorithm = input$algorithm,
              min_sleep_period = input$min_sleep_period,
              bedtime_start = input$bedtime_start,
              wake_time_end = input$wake_time_end,
              max_sleep_period = input$max_sleep_period,
              min_nonzero_epochs = input$min_nonzero_epochs
            )
          )
        }

        # Memory cleanup
        gc(verbose = FALSE)
      })

      results(all_results)
      shared$results$sleep <- all_results

      n_scored <- sum(sapply(all_results, function(r) r$n_periods > 0))
      showNotification(paste("Sleep scoring complete!", n_scored, "of", length(all_results), "files have sleep periods."), type = "message")
    })

    # Clear results
    observeEvent(input$clear_results, {
      results(list())
      shared$results$sleep <- NULL
      showNotification("Sleep results cleared.", type = "message")
    })

    # Value boxes
    output$vb_files_scored <- renderValueBox({
      res <- results()
      n <- length(res)
      valueBox(n, "Files Scored", icon = icon("file"), color = if (n > 0) "green" else "red")
    })

    output$vb_total_periods <- renderValueBox({
      res <- results()
      if (length(res) == 0) {
        valueBox(0, "Total Sleep Periods", icon = icon("moon"), color = "blue")
      } else {
        total <- sum(sapply(res, function(r) r$n_periods), na.rm = TRUE)
        valueBox(total, "Total Sleep Periods", icon = icon("moon"), color = "blue")
      }
    })

    output$vb_avg_efficiency <- renderValueBox({
      res <- results()
      if (length(res) == 0) {
        valueBox("--", "Avg Efficiency", icon = icon("percent"), color = "yellow")
      } else {
        avg <- mean(sapply(res, function(r) r$avg_efficiency), na.rm = TRUE)
        valueBox(paste0(round(avg, 1), "%"), "Avg Efficiency", icon = icon("percent"), color = "yellow")
      }
    })

    output$vb_avg_duration <- renderValueBox({
      res <- results()
      if (length(res) == 0) {
        valueBox("--", "Avg Duration", icon = icon("clock"), color = "purple")
      } else {
        avg <- mean(sapply(res, function(r) r$avg_duration), na.rm = TRUE)
        valueBox(paste0(round(avg / 60, 1), "h"), "Avg Duration", icon = icon("clock"), color = "purple")
      }
    })

    # Summary table (one row per file - matches BatchSleepExportSummary.csv)
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run 'Score Sleep Periods' to see results"), rownames = FALSE))
      }

      # Build summary rows matching package format
      rows <- lapply(res, function(r) {
        if (r$n_periods == 0 || is.null(r$periods)) {
          return(data.frame(
            `Subject Name` = r$subject_id,
            `Serial Number` = r$serial_number %||% "",
            `Sleep/Wake Algorithm` = if (r$algorithm == "cole.kripke") "Cole-Kripke" else "Sadeh",
            `Number of Sleep Periods` = 0,
            `Average Efficiency` = NA,
            `Average Total Sleep Time` = NA,
            `Average WASO` = NA,
            `Average Number of Awakenings` = NA,
            `Average Latency` = NA,
            check.names = FALSE,
            stringsAsFactors = FALSE
          ))
        }

        periods <- r$periods
        latencies <- as.numeric(difftime(periods$onset, periods$in_bed_time, units = "mins"))

        data.frame(
          `Subject Name` = r$subject_id,
          `Serial Number` = r$serial_number %||% "",
          `Sleep/Wake Algorithm` = if (r$algorithm == "cole.kripke") "Cole-Kripke" else "Sadeh",
          `Number of Sleep Periods` = r$n_periods,
          `Average Efficiency` = round(mean(periods$sleep_efficiency, na.rm = TRUE), 1),
          `Average Total Sleep Time` = round(mean(periods$sleep_time, na.rm = TRUE), 0),
          `Average WASO` = round(mean(periods$wake_time, na.rm = TRUE), 0),
          `Average Number of Awakenings` = round(mean(periods$number_of_awakenings, na.rm = TRUE), 1),
          `Average Latency` = round(mean(latencies, na.rm = TRUE), 0),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })

      df <- do.call(rbind, rows)

      DT::datatable(
        df,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Details table (one row per sleep period - matches BatchSleepExportDetails.csv)
    output$details_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run 'Score Sleep Periods' to see results"), rownames = FALSE))
      }

      # Build detail rows matching package format
      all_rows <- list()
      for (r in res) {
        if (is.null(r$periods) || nrow(r$periods) == 0) next

        algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke" else "Sadeh"

        for (i in 1:nrow(r$periods)) {
          period <- r$periods[i, ]
          latency <- as.numeric(difftime(period$onset, period$in_bed_time, units = "mins"))
          sleep_frag <- period$movement_index + period$fragmentation_index

          row_data <- data.frame(
            `Subject Name` = r$subject_id,
            `Sleep/Wake Algorithm` = algorithm_display,
            `In Bed Time` = format(period$in_bed_time, "%m/%d/%Y %I:%M %p"),
            `Out Bed Time` = format(period$out_bed_time, "%m/%d/%Y %I:%M %p"),
            Efficiency = round(period$sleep_efficiency, 1),
            Onset = format(period$onset, "%m/%d/%Y %I:%M %p"),
            Latency = round(latency, 0),
            `Total Sleep Time` = round(period$sleep_time, 0),
            WASO = round(period$wake_time, 0),
            `Number of Awakenings` = period$number_of_awakenings,
            `Movement Index` = round(period$movement_index, 3),
            `Fragmentation Index` = round(period$fragmentation_index, 3),
            `Sleep Fragmentation Index` = round(sleep_frag, 3),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          all_rows[[length(all_rows) + 1]] <- row_data
        }
      }

      if (length(all_rows) == 0) {
        return(DT::datatable(data.frame(Message = "No sleep periods detected"), rownames = FALSE))
      }

      df <- do.call(rbind, all_rows)

      DT::datatable(
        df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Helper function to format average time (circular mean for times around midnight)
    format_average_time <- function(times) {
      if (length(times) == 0) return("")
      if (!inherits(times, "POSIXt")) times <- as.POSIXct(times)

      hours <- as.numeric(format(times, "%H"))
      minutes <- as.numeric(format(times, "%M"))
      minutes_since_midnight <- hours * 60 + minutes

      # Circular mean
      angles_rad <- (minutes_since_midnight / 1440) * 2 * pi
      sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
      cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
      mean_angle <- atan2(sin_mean, cos_mean)
      if (mean_angle < 0) mean_angle <- mean_angle + 2 * pi

      avg_minutes <- (mean_angle / (2 * pi)) * 1440
      avg_hour <- floor(avg_minutes / 60) %% 24
      avg_min <- round(avg_minutes %% 60)

      # Format as H:MM AM/PM
      if (avg_hour == 0) {
        sprintf("12:%02d AM", avg_min)
      } else if (avg_hour < 12) {
        sprintf("%d:%02d AM", avg_hour, avg_min)
      } else if (avg_hour == 12) {
        sprintf("12:%02d PM", avg_min)
      } else {
        sprintf("%d:%02d PM", avg_hour - 12, avg_min)
      }
    }

    # Helper to format datetime for ActiLife
    format_actilife_datetime <- function(dt) {
      if (is.null(dt) || length(dt) == 0 || is.na(dt)) return("")
      formatted <- format(as.POSIXct(dt), format = "%m/%d/%Y %I:%M:%S %p")
      formatted <- gsub("^0", "", formatted)
      formatted <- gsub("/0", "/", formatted)
      formatted
    }

    # Export Details CSV (BatchSleepExportDetails.csv format - EXACT match)
    output$export_details <- downloadHandler(
      filename = function() {
        paste0("BatchSleepExportDetails_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- results()
        if (length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        all_rows <- list()

        for (r in res) {
          if (is.null(r$periods) || nrow(r$periods) == 0) next

          # Get metadata from shared files
          f <- shared$files[[r$file_id]]
          weight <- f$subject_info$weight %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% "Undefined"
          if (gender == "M") gender <- "Male"
          else if (gender == "F") gender <- "Female"
          else if (gender == "") gender <- "Undefined"

          algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke" else "Sadeh"

          for (i in 1:nrow(r$periods)) {
            period <- r$periods[i, ]
            latency <- as.numeric(difftime(period$onset, period$in_bed_time, units = "mins"))
            sleep_frag_index <- period$movement_index + period$fragmentation_index

            row_data <- data.frame(
              `Subject Name` = r$subject_id,
              `File Name` = r$name,
              `Serial Number` = r$serial_number %||% "",
              `Epoch Length` = r$epoch_length,
              Weight = weight,
              Age = age,
              Gender = gender,
              `Sleep/Wake Algorithm` = algorithm_display,
              `Sleep Period Detection Algorithm` = "Tudor-Locke Default",
              `In Bed Time` = format_actilife_datetime(period$in_bed_time),
              `Out Bed Time` = format_actilife_datetime(period$out_bed_time),
              Efficiency = round(period$sleep_efficiency, 3),
              Onset = format_actilife_datetime(period$onset),
              Latency = round(latency, 0),
              `Total Sleep Time` = round(period$sleep_time, 0),
              WASO = round(period$wake_time, 0),
              `Number of Awakenings` = period$number_of_awakenings,
              `Length of Awakenings in Minutes` = round(period$average_awakening, 2),
              `Activity Counts` = round(period$total_counts, 0),
              `Movement Index` = round(period$movement_index, 3),
              `Fragmentation Index` = round(period$fragmentation_index, 3),
              `Sleep Fragmentation Index` = round(sleep_frag_index, 3),
              check.names = FALSE,
              stringsAsFactors = FALSE
            )
            all_rows[[length(all_rows) + 1]] <- row_data
          }
        }

        if (length(all_rows) == 0) {
          write.csv(data.frame(Message = "No sleep periods to export"), file, row.names = FALSE)
          return()
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE)
      }
    )

    # Export Summary CSV (BatchSleepExportSummary.csv format - EXACT match)
    output$export_summary <- downloadHandler(
      filename = function() {
        paste0("BatchSleepExportSummary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- results()
        if (length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        all_rows <- list()

        for (r in res) {
          if (is.null(r$periods) || nrow(r$periods) == 0) next

          # Get metadata from shared files
          f <- shared$files[[r$file_id]]
          weight <- f$subject_info$weight %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% "Undefined"
          if (gender == "M") gender <- "Male"
          else if (gender == "F") gender <- "Female"
          else if (gender == "") gender <- "Undefined"

          algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke" else "Sadeh"
          periods <- r$periods

          # Calculate averages
          latencies <- as.numeric(difftime(periods$onset, periods$in_bed_time, units = "mins"))

          row_data <- data.frame(
            `Subject Name` = r$subject_id,
            `File Name` = r$name,
            `Serial Number` = r$serial_number %||% "",
            `Epoch Length` = r$epoch_length,
            Weight = weight,
            Age = age,
            Gender = gender,
            `Sleep/Wake Algorithm` = algorithm_display,
            `Sleep Period Detection Algorithm` = "Tudor-Locke Default",
            `Number of Sleep Periods` = r$n_periods,
            `Average In Bed Time` = format_average_time(periods$in_bed_time),
            `Average Out Bed Time` = format_average_time(periods$out_bed_time),
            `Average Efficiency` = round(mean(periods$sleep_efficiency, na.rm = TRUE), 3),
            `Average Onset` = format_average_time(periods$onset),
            `Average Latency` = round(mean(latencies, na.rm = TRUE), 0),
            `Average Total Sleep Time` = round(mean(periods$sleep_time, na.rm = TRUE), 0),
            `Average WASO` = round(mean(periods$wake_time, na.rm = TRUE), 2),
            `Average Number of Awakenings` = round(mean(periods$number_of_awakenings, na.rm = TRUE), 2),
            `Average Length of Awakenings in Minutes` = round(mean(periods$average_awakening, na.rm = TRUE), 2),
            `Average Activity Counts` = round(mean(periods$total_counts, na.rm = TRUE), 2),
            `Average Movement Index` = round(mean(periods$movement_index, na.rm = TRUE), 3),
            `Average Fragmentation Index` = round(mean(periods$fragmentation_index, na.rm = TRUE), 3),
            `Average Sleep Fragmentation Index` = round(mean(periods$movement_index + periods$fragmentation_index, na.rm = TRUE), 3),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          all_rows[[length(all_rows) + 1]] <- row_data
        }

        if (length(all_rows) == 0) {
          write.csv(data.frame(Message = "No sleep periods to export"), file, row.names = FALSE)
          return()
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

# Null coalesce
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
