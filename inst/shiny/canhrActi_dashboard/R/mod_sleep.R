# Module: Sleep Analysis - Redesigned UI/UX
# Consistent with Activity tab: chart-first layout, compact metrics, controls-panel structure

mod_sleep_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Page Header
    page_header(
      icon_name = "moon",
      title = "Sleep Analysis",
      subtitle = "Sleep detection & quality",
      status_output_id = ns("sleep_status_badge")
    ),

    # Compact Metrics Strip (matching Activity tab style)
    div(class = "metrics-strip metrics-strip--transparent",
      # File count badge
      div(class = "file-info-badge metrics-strip-fixed",
        textOutput(ns("metric_files_scored"), inline = TRUE), " files"
      ),

      # Sleep Periods metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_total_nights"), inline = TRUE)),
        div(class = "metric-label", "Sleep Periods")
      ),

      # Avg Duration metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_avg_duration"), inline = TRUE)),
        div(class = "metric-label", "Avg Duration")
      ),

      # Avg Efficiency metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_avg_efficiency"), inline = TRUE)),
        div(class = "metric-label", "Avg Efficiency")
      ),

      # Avg WASO metric
      div(class = "metric-card metric-card--inline",
        div(class = "metric-value", textOutput(ns("metric_avg_waso"), inline = TRUE)),
        div(class = "metric-label", "Avg WASO")
      ),

      # Quick actions (matching Activity tab)
      div(class = "cluster cluster--gap-2 ml-auto metrics-strip-fixed",
        actionButton(ns("run_btn"), span(icon("play"), "Run Analysis"),
                     class = "btn-primary"),
        actionButton(ns("clear_results"), span(icon("redo"), "Reset"),
                     class = "btn-default")
      )
    ),

    # Main Content: Two-column layout (matching Activity tab 3/9 split)
    fluidRow(
      # Left: Controls (narrow - width 3)
      column(width = 3,

        # Essential Controls (always visible)
        div(class = "controls-panel",
          div(class = "controls-header",
            div(class = "controls-header-title",
              icon("flask"), "Sleep Scoring"
            )
          ),
          div(class = "mt-3",
            # Algorithm selector
            radioButtons(
              ns("algorithm"),
              "Algorithm:",
              choices = c(
                "Cole-Kripke (Adults)" = "cole.kripke",
                "Sadeh (Youth)" = "sadeh"
              ),
              selected = "cole.kripke"
            ),

            # Algorithm info
            uiOutput(ns("algorithm_info")),

            # Wear time filter note
            tags$small(class = "control-hint text-muted mb-3",
              icon("info-circle"), " Requires wear time analysis for accuracy"
            )
          )
        ),

        # Advanced Options (collapsible - matching Activity tab)
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
              # Sleep Period Detection section
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("bed"), "Period Detection"
                ),

                # Row 1: Minimum sleep period
                tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 8px 0;",
                  tags$span(style = "width: 80px; text-align: right; color: #666;", "Min Period:"),
                  div(style = "width: 70px;",
                    numericInput(ns("min_sleep_period"), label = NULL, value = 160,
                                 min = 30, max = 480, step = 10, width = "100%")
                  ),
                  tags$span(style = "color: #666; font-size: 13px;", "min")
                ),

                # Row 2: Bedtime definition
                tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 8px 0;",
                  tags$span(style = "width: 80px; text-align: right; color: #666;", "Bedtime:"),
                  div(style = "width: 70px;",
                    numericInput(ns("bedtime_start"), label = NULL, value = 5,
                                 min = 1, max = 30, step = 1, width = "100%")
                  ),
                  tags$span(style = "color: #666; font-size: 13px;", "epochs")
                ),

                # Row 3: Wake time definition
                tags$div(style = "display: flex; align-items: center; gap: 8px; margin: 8px 0;",
                  tags$span(style = "width: 80px; text-align: right; color: #666;", "Wake:"),
                  div(style = "width: 70px;",
                    numericInput(ns("wake_time_end"), label = NULL, value = 10,
                                 min = 1, max = 60, step = 1, width = "100%")
                  ),
                  tags$span(style = "color: #666; font-size: 13px;", "epochs")
                )
              ),

              # Sleep Diary Integration (collapsible sub-section)
              div(class = "algo-group",
                div(class = "algo-group-header",
                  icon("book-open"), "Sleep Diary"
                ),
                checkboxInput(ns("use_diary"), "Enable Diary Integration", value = FALSE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("use_diary")),
                  fileInput(
                    ns("diary_file"),
                    NULL,
                    accept = c(".csv", ".xlsx"),
                    buttonLabel = icon("upload"),
                    placeholder = "Upload diary"
                  ),
                  selectInput(ns("diary_method"), "Method:",
                    choices = c(
                      "Validation Only" = "validation_only",
                      "Hybrid" = "hybrid",
                      "Diary Guided" = "diary_guided"
                    ),
                    selected = "validation_only"
                  ),
                  tags$small(class = "text-muted",
                    icon("info-circle"), " Requires: date, bedtime, waketime columns"
                  )
                )
              )
            )
          )
        ),

        # Hidden inputs for additional parameters
        tags$div(class = "hidden",
          numericInput(ns("max_sleep_period"), NULL, value = 1440),
          numericInput(ns("min_nonzero_epochs"), NULL, value = 0),
          checkboxInput(ns("use_min_nonzero"), NULL, value = FALSE),
          selectInput(ns("detection_method"), NULL, choices = c("Tudor-Locke" = "tudor.locke"), selected = "tudor.locke")
        ),

        # Export Panel (matching Activity tab)
        div(class = "controls-panel",
          div(class = "controls-header-title mb-3",
            icon("download"), "Export Data"
          ),
          div(class = "export-row",
            downloadButton(ns("export_summary"), span(icon("file-csv"), " Summary"),
              class = "btn-primary"),
            downloadButton(ns("export_details"), span(icon("table"), " Details"),
              class = "btn-info")
          )
        )
      ),

      # Right: Charts and Results (wide - width 9)
      column(width = 9,

        # HERO CHART: Sleep Visualization
        div(class = "hero-chart-container",
          div(class = "hero-chart-header",
            div(class = "hero-chart-title",
              icon("wave-square"), "Sleep Hypnogram"
            ),
            uiOutput(ns("file_selector_ui"))
          ),
          conditionalPanel(
            condition = "output.has_sleep_results == false",
            ns = ns,
            chart_empty_state(
              title = "No Sleep Data",
              message = "Click 'Run Analysis' to detect sleep periods",
              show_icon = FALSE
            )
          ),
          conditionalPanel(
            condition = "output.has_sleep_results == true",
            ns = ns,
            uiOutput(ns("hypnogram_chart_ui"))
          )
        ),

        # Tabbed Results Section (matching Activity tab)
        div(class = "hero-chart-container results-tabs",
          tabsetPanel(
            id = ns("results_tabs"),
            type = "tabs",

            # Night Summary Tab
            tabPanel(
              title = "Night Summary",
              value = "nights",
              div(class = "pt-4",
                conditionalPanel(
                  condition = "output.has_sleep_results == false",
                  ns = ns,
                  chart_empty_state(
                    title = "Night Summary",
                    message = "Run Analysis to see night-by-night metrics",
                    show_icon = FALSE,
                    extra_class = "chart-empty-state--spacious"
                  )
                ),
                conditionalPanel(
                  condition = "output.has_sleep_results == true",
                  ns = ns,
                  uiOutput(ns("night_cards"))
                )
              )
            ),

            # Sleep Metrics Tab
            tabPanel(
              title = "Sleep Metrics",
              value = "summary",
              div(class = "pt-4",
                DT::dataTableOutput(ns("summary_table"))
              )
            ),

            # Detailed Data Tab
            tabPanel(
              title = "Detailed Data",
              value = "details",
              div(class = "pt-4",
                DT::dataTableOutput(ns("details_table"))
              )
            ),

            # Diary Validation Tab
            tabPanel(
              title = "Diary Validation",
              value = "diary",
              div(class = "pt-4",
                DT::dataTableOutput(ns("diary_table"))
              )
            )
          )
        )
      )
    )
  )
}

mod_sleep_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Module constants
    SLEEP_PLOT_CONSTANTS <- list(
      NIGHT_OFFSET_HOURS = 6,      # Hours to offset for night detection
      HEIGHT_PER_DAY = 80,         # Pixels per day in hypnogram
      MIN_PLOT_HEIGHT = 320,       # Minimum plot height
      MAX_PLOT_HEIGHT = 800        # Maximum plot height
    )
    # Status badge for page header
    output$sleep_status_badge <- renderUI({
      res <- results()
      if (length(res) == 0) {
        status_badge("Not analyzed", "pending")
      } else {
        status_badge(paste(length(res), "files analyzed"), "success")
      }
    })

    results <- reactiveVal(list())
    diary_data <- reactiveVal(NULL)
    selected_file <- reactiveVal(NULL)

    # Algorithm info based on selection
    output$algorithm_info <- renderUI({
      req(input$algorithm)
      info <- if (input$algorithm == "cole.kripke") {
        list(
          text = "Recommended for adults (35-65 years). Uses activity counts from surrounding epochs.",
          color = "#236192"
        )
      } else {
        list(
          text = "Better for children and adolescents (10-25 years). More sensitive to activity.",
          color = "#6366f1"
        )
      }

      div(
        class = "sleep-info-alert",
        style = sprintf("background: %s10; border-left-color: %s;", info$color, info$color),
        icon("info-circle", style = sprintf("color: %s;", info$color)),
        info$text
      )
    })

    # File selector UI for hypnogram
    output$file_selector_ui <- renderUI({
      res <- results()
      if (length(res) == 0) return(NULL)

      file_choices <- sapply(res, function(r) r$subject_id %||% r$name)
      names(file_choices) <- file_choices

      selectInput(
        ns("selected_file"),
        NULL,
        choices = file_choices,
        selected = file_choices[1],
        width = "200px"
      )
    })

    # Track selected file
    observeEvent(input$selected_file, {
      selected_file(input$selected_file)
    })

    # Calculate number of unique days for dynamic height
    n_days_reactive <- reactive({
      res <- results()
      sel <- selected_file()

      if (length(res) == 0) return(4)  # default

      # Handle empty or NULL selection
      if (is.null(sel) || length(sel) == 0 || sel == "") {
        r <- res[[1]]
      } else {
        # Find selected result
        r <- NULL
        for (result in res) {
          result_id <- result$subject_id %||% result$name
          if (!is.null(result_id) && length(result_id) > 0 && result_id == sel) {
            r <- result
            break
          }
        }
        if (is.null(r)) r <- res[[1]]
      }

      if (is.null(r$timestamps)) return(4)

      # Count unique nights (defined as 6 PM to 12 PM next day)
      ts <- tryCatch(as.POSIXct(r$timestamps), error = function(e) NULL)
      if (is.null(ts)) return(4)

      night_dates <- as.Date(ts - SLEEP_PLOT_CONSTANTS$NIGHT_OFFSET_HOURS * 3600)
      n_days <- length(unique(night_dates))

      max(n_days, 1)
    })

    # Dynamic UI for hypnogram with calculated height
    output$hypnogram_chart_ui <- renderUI({
      n_days <- n_days_reactive()

      # Calculate height: minimum 80px per day, minimum total 320px, maximum 800px
      height_per_day <- SLEEP_PLOT_CONSTANTS$HEIGHT_PER_DAY
      total_height <- max(SLEEP_PLOT_CONSTANTS$MIN_PLOT_HEIGHT, min(SLEEP_PLOT_CONSTANTS$MAX_PLOT_HEIGHT, n_days * height_per_day))

      plotOutput(ns("hypnogram_chart"), height = paste0(total_height, "px"))
    })

    # Compact Metric Outputs
    output$metric_files_scored <- renderText({
      res <- results()
      as.character(length(res))
    })

    output$metric_total_nights <- renderText({
      res <- results()
      if (length(res) == 0) return("--")
      total <- sum(sapply(res, function(r) {
        np <- r$n_periods
        if (is.null(np) || length(np) == 0) return(0)
        as.numeric(np[1])
      }), na.rm = TRUE)
      as.character(total)
    })

    output$metric_avg_duration <- renderText({
      res <- results()
      if (length(res) == 0) return("--")
      durs <- sapply(res, function(r) {
        val <- r$avg_duration
        if (is.null(val) || length(val) == 0 || !is.numeric(val)) return(NA)
        as.numeric(val[1])
      })
      avg <- mean(durs, na.rm = TRUE)
      if (is.na(avg) || !is.finite(avg)) return("--")
      sprintf("%.1fh", avg / 60)
    })

    output$metric_avg_efficiency <- renderText({
      res <- results()
      if (length(res) == 0) return("--")
      effs <- sapply(res, function(r) {
        val <- r$avg_efficiency
        if (is.null(val) || length(val) == 0 || !is.numeric(val)) return(NA)
        as.numeric(val[1])
      })
      avg <- mean(effs, na.rm = TRUE)
      if (is.na(avg) || !is.finite(avg)) return("--")
      sprintf("%.0f%%", avg)
    })

    # Average WASO metric
    output$metric_avg_waso <- renderText({
      res <- results()
      if (length(res) == 0) return("--")
      wasos <- sapply(res, function(r) {
        val <- r$avg_waso
        if (is.null(val) || length(val) == 0 || !is.numeric(val)) return(NA)
        as.numeric(val[1])
      })
      avg <- mean(wasos, na.rm = TRUE)
      if (is.na(avg) || !is.finite(avg)) return("--")
      sprintf("%.0fm", avg)
    })

    # Output for conditional panels (matching Activity tab pattern)
    output$has_sleep_results <- reactive({
      length(results()) > 0
    })
    outputOptions(output, "has_sleep_results", suspendWhenHidden = FALSE)

    # Clear results handler
    observeEvent(input$clear_results, {
      results(list())
      selected_file(NULL)
      showNotification("Sleep results cleared", type = "message", duration = 2)
    })

    output$metric_avg_onset <- renderText({
      res <- results()
      if (length(res) == 0) return("--")

      # Collect all onset times
      all_onsets <- list()
      for (r in res) {
        if (!is.null(r$periods) && nrow(r$periods) > 0) {
          onset_times <- tryCatch(as.POSIXct(r$periods$onset), error = function(e) NULL)
          if (!is.null(onset_times)) {
            all_onsets <- c(all_onsets, list(onset_times))
          }
        }
      }

      if (length(all_onsets) == 0) return("--")

      all_times <- do.call(c, all_onsets)
      format_average_time(all_times)
    })

    # Hypnogram/Hypnodensity Chart
    output$hypnogram_chart <- renderPlot({
      res <- results()
      sel <- selected_file()

      if (length(res) == 0) {
        # Empty state
        plot(1, type = "n", xlim = c(0, 24), ylim = c(0, 1),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        text(12, 0.5, "Run Sleep Analysis to view sleep patterns",
             col = "#94a3b8", cex = 1.5, font = 2)
        return()
      }

      # Find selected result
      r <- NULL
      # Guard against NULL or empty sel
      if (!is.null(sel) && length(sel) > 0 && nchar(sel) > 0) {
        for (result in res) {
          result_id <- result$subject_id %||% result$name %||% ""
          if (length(result_id) > 0 && result_id == sel) {
            r <- result
            break
          }
        }
      }

      if (is.null(r)) r <- res[[1]]

      # Check if we have sleep state data
      if (is.null(r$sleep_state) || is.null(r$timestamps)) {
        plot(1, type = "n", xlim = c(0, 24), ylim = c(0, 1),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        text(12, 0.5, "No sleep data available for visualization",
             col = "#94a3b8", cex = 1.2)
        return()
      }

      # Get file data for counts
      f <- shared$files[[r$file_id]]
      counts_col <- if (!is.null(f) && "axis1" %in% names(f$data)) "axis1" else NULL
      data_for_plot <- f$data

      if (is.null(data_for_plot)) {
        create_simple_hypnogram(r)
        return()
      }

      data_for_plot$sleep_state <- r$sleep_state
      data_for_plot$timestamp <- r$timestamps

      # Render hypnogram
      tryCatch({
        canhrActi::plot_hypnogram(
          data = data_for_plot,
          timestamp_col = "timestamp",
          sleep_col = "sleep_state",
          counts_col = counts_col,
          sleep_periods = r$periods,
          show_metrics = TRUE,
          show_activity = TRUE,
          show_awakenings = TRUE,
          title = paste("Sleep Hypnogram -", r$subject_id %||% r$name)
        )
      }, error = function(e) {
        create_simple_hypnogram(r)
      })
    }, bg = "transparent")

    # Simple hypnogram fallback function
    create_simple_hypnogram <- function(r) {
      if (is.null(r$sleep_state) || is.null(r$timestamps)) {
        plot(1, type = "n", xlim = c(0, 24), ylim = c(0, 1),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
        text(12, 0.5, "No sleep data available", col = "#94a3b8", cex = 1.2)
        return()
      }

      ts <- as.POSIXct(r$timestamps)
      sleep <- r$sleep_state

      # Convert to numeric (0 = sleep, 1 = wake)
      if (is.character(sleep)) {
        sleep_num <- ifelse(toupper(sleep) == "W", 1, 0)
      } else {
        sleep_num <- as.numeric(sleep != 0)
      }

      # Time of day
      hours <- as.numeric(format(ts, "%H")) + as.numeric(format(ts, "%M")) / 60

      par(mar = c(4, 4, 2, 2), bg = "transparent")
      plot(hours, sleep_num, type = "n",
           xlim = c(0, 24), ylim = c(-0.1, 1.3),
           xlab = "Time of Day", ylab = "",
           xaxt = "n", yaxt = "n",
           main = paste("Sleep Hypnogram -", r$subject_id %||% r$name))

      # X-axis
      axis(1, at = seq(0, 24, by = 4),
           labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", "8 PM", "12 AM"))

      # Y-axis
      axis(2, at = c(0, 1), labels = c("Sleep", "Wake"), las = 1)

      # Fill rectangles for sleep/wake states
      for (i in 1:(length(hours) - 1)) {
        if (is.na(sleep_num[i])) next
        col <- if (sleep_num[i] == 0) "#1a365d" else "#f56565"
        rect(hours[i], -0.05, hours[i + 1], sleep_num[i] + 0.05, col = col, border = NA)
      }

      # Add grid
      abline(v = seq(0, 24, by = 4), col = "#e2e8f0", lty = 2)
      abline(h = c(0, 1), col = "#e2e8f0", lty = 2)
    }

    # Night Cards UI
    output$night_cards <- renderUI({
      res <- results()
      if (length(res) == 0) {
        return(empty_state(
          title = "No Sleep Analysis",
          message = "Run Analysis to view night-by-night summaries",
          show_icon = FALSE
        ))
      }

      # Collect all periods with file info
      all_cards <- list()
      for (r in res) {
        if (is.null(r$periods) || nrow(r$periods) == 0) next

        for (i in 1:nrow(r$periods)) {
          period <- r$periods[i, ]

          # Parse times
          in_bed <- tryCatch(as.POSIXct(period$in_bed_time), error = function(e) NA)
          out_bed <- tryCatch(as.POSIXct(period$out_bed_time), error = function(e) NA)

          # Determine date
          date_str <- if (!is.na(in_bed)) {
            format(in_bed, "%a, %b %d")
          } else {
            paste("Night", i)
          }

          # Onset/offset times
          onset_time <- tryCatch(format(as.POSIXct(period$onset), "%I:%M %p"), error = function(e) "--")
          offset_time <- tryCatch({
            # Calculate offset from onset + sleep_time + wake_time
            format(out_bed, "%I:%M %p")
          }, error = function(e) "--")

          # Duration
          duration_hrs <- round(period$sleep_time / 60, 1)

          # Efficiency
          efficiency <- round(period$sleep_efficiency, 0)

          # Health indicator based on efficiency
          health_class <- if (efficiency >= 85) "good" else if (efficiency >= 70) "moderate" else "poor"

          card_html <- div(
            class = "night-card",
            div(class = "night-date",
              div(class = "text-sm", date_str),
              div(class = "text-xs text-muted", r$subject_id %||% "")
            ),
            div(class = "night-metrics",
              div(class = "night-metric",
                div(class = "night-metric-value", sprintf("%.1fh", duration_hrs)),
                div(class = "night-metric-label", "Duration")
              ),
              div(class = "night-metric",
                div(class = "night-metric-value", sprintf("%d%%", efficiency)),
                div(class = "night-metric-label", "Efficiency")
              ),
              div(class = "night-metric",
                div(class = "night-metric-value", onset_time),
                div(class = "night-metric-label", "Onset")
              ),
              div(class = "night-metric",
                div(class = "night-metric-value", offset_time),
                div(class = "night-metric-label", "Wake")
              ),
              div(class = "night-metric",
                div(class = "night-metric-value", period$number_of_awakenings %||% "--"),
                div(class = "night-metric-label", "Awakenings")
              )
            ),
            div(class = paste("health-indicator", health_class))
          )

          all_cards[[length(all_cards) + 1]] <- card_html
        }
      }

      if (length(all_cards) == 0) {
        return(empty_state(
          title = "No Sleep Periods",
          message = "No sleep periods detected",
          show_icon = FALSE,
          small = TRUE,
          extra_class = "empty-state--compact"
        ))
      }

      do.call(tagList, all_cards)
    })

    # Handle diary file upload
    observeEvent(input$diary_file, {
      req(input$diary_file)
      tryCatch({
        file_ext <- tolower(tools::file_ext(input$diary_file$name))

        if (file_ext == "xlsx") {
          if (!requireNamespace("openxlsx", quietly = TRUE)) {
            showNotification("Install 'openxlsx' package to read Excel files", type = "error")
            return()
          }
          diary <- openxlsx::read.xlsx(input$diary_file$datapath)
        } else {
          diary <- read.csv(input$diary_file$datapath, stringsAsFactors = FALSE)
        }

        required_cols <- c("date", "bedtime", "waketime")
        if (!all(required_cols %in% tolower(names(diary)))) {
          showNotification("Diary must have columns: date, bedtime, waketime", type = "error")
          return()
        }
        names(diary) <- tolower(names(diary))
        # Parse dates with error handling for various formats
        diary$date <- tryCatch({
          as.Date(diary$date)
        }, error = function(e) {
          tryCatch(as.Date(diary$date, format = "%m/%d/%Y"), error = function(e2) {
            tryCatch(as.Date(diary$date, format = "%d/%m/%Y"), error = function(e3) NULL)
          })
        })
        if (is.null(diary$date) || all(is.na(diary$date))) {
          showNotification("Invalid date format in diary. Use YYYY-MM-DD or MM/DD/YYYY.", type = "error")
          return()
        }
        diary_data(diary)
        showNotification(paste("Loaded diary with", nrow(diary), "entries"), type = "message")
      }, error = function(e) {
        showNotification(paste("Error reading diary:", e$message), type = "error")
      })
    })

    # Helper: Format ETA
    # Using format_eta from shared_components.R instead
    # if (is.na(seconds) || seconds < 0) return("calculating...")
    # if (seconds < 60) return(paste0(round(seconds), "s"))
    # if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
    # return(paste0(round(seconds / 3600, 1), "h"))
    # }

    # Helper: Format average time (circular mean)
    format_average_time <- function(times) {
      if (length(times) == 0) return("--")
      if (!inherits(times, "POSIXt")) times <- as.POSIXct(times)

      hours <- as.numeric(format(times, "%H"))
      minutes <- as.numeric(format(times, "%M"))
      minutes_since_midnight <- hours * 60 + minutes

      angles_rad <- (minutes_since_midnight / 1440) * 2 * pi
      sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
      cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
      mean_angle <- atan2(sin_mean, cos_mean)
      if (mean_angle < 0) mean_angle <- mean_angle + 2 * pi

      avg_minutes <- (mean_angle / (2 * pi)) * 1440
      avg_hour <- floor(avg_minutes / 60) %% 24
      avg_min <- round(avg_minutes %% 60)

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

    # Run sleep analysis
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      #  Check if wear time has been analyzed
      wt_results <- shared$results$wear_time
      use_wear_time <- !is.null(wt_results) && length(wt_results) > 0

      # Warn if wear time not analyzed - this is IMPORTANT for sleep
      if (!use_wear_time) {
        showNotification(
          HTML("<strong>Important:</strong> Run Wear Time Analysis first for accurate sleep detection.<br>
                Non-wear periods (0 counts) may be incorrectly classified as sleep."),
          type = "warning",
          duration = 10
        )
      }

      file_ids <- names(shared$files)
      n_files <- length(file_ids)
      all_results <- vector("list", n_files)
      names(all_results) <- file_ids
      start_time <- Sys.time()

      progress_interval <- max(1, min(5, ceiling(n_files / 10)))

      withProgress(message = "Scoring sleep periods...", value = 0, {
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

          counts <- if ("axis1" %in% names(data)) data$axis1 else data[, 1]

          timestamps <- if ("timestamp" %in% names(data)) {
            data$timestamp
          } else if ("dataTimestamp" %in% names(data)) {
            as.POSIXct((data$dataTimestamp / 10000000 - 62135596800), origin = '1970-01-01', tz = 'UTC')
          } else {
            seq(from = Sys.time() - nrow(data) * 60, by = 60, length.out = nrow(data))
          }

          # Get wear time mask for this file
          wear_mask <- NULL
          if (use_wear_time && fid %in% names(wt_results)) {
            wear_mask <- wt_results[[fid]]$wear
          }

          sleep_state <- NULL
          periods <- NULL
          actual_algorithm <- input$algorithm
          detection_method_used <- "Tudor-Locke"

          if (actual_algorithm %in% c("cole.kripke", "sadeh")) {
            sleep_state <- tryCatch({
              if (actual_algorithm == "cole.kripke") {
                canhrActi::sleep.cole.kripke(counts, apply_rescoring = TRUE)
              } else {
                canhrActi::sleep.sadeh(counts)
              }
            }, error = function(e) {
              showNotification(paste0("Sleep scoring failed for ", f$name, " - may need more data"), type = "error")
              return(NULL)
            })

            if (!is.null(sleep_state) && "timestamp" %in% names(data)) {
              bedtime_epochs <- input$bedtime_start
              wake_epochs <- input$wake_time_end

              # Run Tudor-Locke period detection on FULL sleep_state (no NA masking yet)
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

            # AFTER period detection: Mark non-wear epochs as NA in sleep state
            # This is for display/export purposes - doesn't affect period detection
            if (!is.null(sleep_state) && !is.null(wear_mask) && length(wear_mask) == length(sleep_state)) {
              sleep_state[!wear_mask] <- NA
            }
          }

          if (is.null(sleep_state) && is.null(periods)) next

          n_periods <- if (!is.null(periods) && nrow(periods) > 0) nrow(periods) else 0
          avg_duration <- if (n_periods > 0) mean(periods$sleep_time, na.rm = TRUE) else NA
          avg_efficiency <- if (n_periods > 0) mean(periods$sleep_efficiency, na.rm = TRUE) else NA
          avg_awakenings <- if (n_periods > 0) mean(periods$number_of_awakenings, na.rm = TRUE) else NA
          avg_waso <- if (n_periods > 0) mean(periods$wake_time, na.rm = TRUE) else NA
          avg_latency <- NA
          if (n_periods > 0 && "onset" %in% names(periods) && "in_bed_time" %in% names(periods)) {
            onset_times <- as.POSIXct(periods$onset)
            in_bed_times <- as.POSIXct(periods$in_bed_time)
            latencies <- as.numeric(difftime(onset_times, in_bed_times, units = "mins"))
            avg_latency <- mean(latencies, na.rm = TRUE)
          }

          diary_result <- NULL
          if (isTRUE(input$use_diary) && !is.null(diary_data())) {
            diary_result <- tryCatch({
              canhrActi::integrate.sleep.diary(
                accel_sleep = sleep_state,
                diary = diary_data(),
                timestamps = timestamps,
                method = input$diary_method %||% "validation_only"
              )
            }, error = function(e) {
              NULL
            })
          }

          enhanced_frag <- NULL
          if ("timestamp" %in% names(data)) {
            enhanced_frag <- tryCatch({
              canhrActi::sleep.fragmentation.enhanced(
                sleep_state = sleep_state,
                timestamps = timestamps
              )
            }, error = function(e) {
              NULL
            })
          }

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            epoch_length = f$epoch_length,
            algorithm = actual_algorithm,
            detection_method = detection_method_used,
            periods = periods,
            sleep_state = sleep_state,
            timestamps = timestamps,
            wear_mask = wear_mask,  # Store wear mask for exports
            wear_time_applied = !is.null(wear_mask),  # Track if wear time was applied
            n_periods = n_periods,
            avg_duration = avg_duration,
            avg_efficiency = avg_efficiency,
            avg_awakenings = avg_awakenings,
            avg_waso = avg_waso,
            avg_latency = avg_latency,
            diary_result = diary_result,
            enhanced_fragmentation = enhanced_frag,
            parameters = list(
              sleep_algorithm = actual_algorithm,
              detection_method = detection_method_used,
              min_sleep_period = input$min_sleep_period,
              bedtime_start = input$bedtime_start,
              wake_time_end = input$wake_time_end,
              max_sleep_period = input$max_sleep_period,
              min_nonzero_epochs = input$min_nonzero_epochs,
              use_diary = input$use_diary,
              enhanced_fragmentation = TRUE,
              wear_time_filtered = !is.null(wear_mask)
            )
          )
        }

        gc(verbose = FALSE)
      })

      all_results <- Filter(Negate(is.null), all_results)
      results(all_results)
      shared$results$sleep <- all_results

      # Initialize selected_file to the first result for immediate hypnogram display
      if (length(all_results) > 0) {
        first_id <- all_results[[1]]$subject_id %||% all_results[[1]]$name
        selected_file(first_id)
      }

      n_scored <- sum(sapply(all_results, function(r) {
        np <- r$n_periods
        if (is.null(np) || length(np) == 0) return(FALSE)
        as.numeric(np[1]) > 0
      }))
      showNotification(paste("Sleep scoring complete!", n_scored, "of", length(all_results), "files have sleep periods."), type = "message")
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(
          data.frame(Message = "Run 'Sleep Analysis' to see results"),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      format_algorithm <- function(alg) {
        if (is.null(alg) || length(alg) == 0) return("-")
        alg <- as.character(alg[1])
        if (alg == "cole.kripke") "Cole-Kripke"
        else if (alg == "sadeh") "Sadeh"
        else alg
      }

      rows <- lapply(res, function(r) {
        if (is.null(r)) return(NULL)
        n_periods <- r$n_periods
        if (is.null(n_periods) || length(n_periods) == 0) n_periods <- 0
        n_periods <- as.numeric(n_periods[1])

        if (n_periods == 0 || is.null(r$periods) || (is.data.frame(r$periods) && nrow(r$periods) == 0)) {
          return(data.frame(
            Subject = r$subject_id %||% "Unknown",
            Algorithm = format_algorithm(r$algorithm),
            `Sleep Periods` = 0,
            `Avg Efficiency (%)` = NA,
            `Avg Duration (min)` = NA,
            `Avg WASO (min)` = NA,
            `Avg Awakenings` = NA,
            check.names = FALSE,
            stringsAsFactors = FALSE
          ))
        }

        periods <- r$periods

        data.frame(
          Subject = r$subject_id %||% "Unknown",
          Algorithm = format_algorithm(r$algorithm),
          `Sleep Periods` = n_periods,
          `Avg Efficiency (%)` = round(mean(periods$sleep_efficiency, na.rm = TRUE), 1),
          `Avg Duration (min)` = round(mean(periods$sleep_time, na.rm = TRUE), 0),
          `Avg WASO (min)` = round(mean(periods$wake_time, na.rm = TRUE), 0),
          `Avg Awakenings` = round(mean(periods$number_of_awakenings, na.rm = TRUE), 1),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })

      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        return(DT::datatable(data.frame(Message = "No sleep periods detected"), rownames = FALSE))
      }
      df <- do.call(rbind, rows)

      DT::datatable(
        df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = 'display compact stripe'
      ) %>%
        DT::formatStyle(
          'Avg Efficiency (%)',
          backgroundColor = DT::styleInterval(
            c(70, 85),
            c('#fed7d7', '#fefce8', '#c6f6d5')
          )
        )
    })

    # Details table
    output$details_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(
          data.frame(Message = "Run 'Sleep Analysis' to see results"),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      all_rows <- list()
      for (r in res) {
        if (is.null(r)) next
        if (is.null(r$periods)) next
        if (!is.data.frame(r$periods) || nrow(r$periods) == 0) next

        alg <- r$algorithm
        if (is.null(alg) || length(alg) == 0) alg <- "unknown"
        algorithm_display <- if (alg == "cole.kripke") "Cole-Kripke"
                             else if (alg == "sadeh") "Sadeh"
                             else as.character(alg)

        for (i in 1:nrow(r$periods)) {
          period <- r$periods[i, ]

          in_bed_posix <- as.POSIXct(period$in_bed_time)
          out_bed_posix <- as.POSIXct(period$out_bed_time)
          onset_posix <- as.POSIXct(period$onset)

          latency <- as.numeric(difftime(onset_posix, in_bed_posix, units = "mins"))
          sleep_frag <- period$movement_index + period$fragmentation_index

          row_data <- data.frame(
            Subject = r$subject_id,
            Date = format(in_bed_posix, "%m/%d/%Y"),
            `In Bed` = format(in_bed_posix, "%I:%M %p"),
            `Out Bed` = format(out_bed_posix, "%I:%M %p"),
            `Efficiency (%)` = round(period$sleep_efficiency, 1),
            `TST (min)` = round(period$sleep_time, 0),
            `WASO (min)` = round(period$wake_time, 0),
            Awakenings = period$number_of_awakenings,
            `Latency (min)` = round(latency, 0),
            `Frag Index` = round(sleep_frag, 3),
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
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = 'display compact stripe'
      )
    })

    # Diary concordance table
    output$diary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(
          data.frame(Message = "Run 'Sleep Analysis' with diary integration enabled"),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      all_rows <- list()
      for (r in res) {
        if (is.null(r$diary_result)) next

        concordance <- r$diary_result$concordance
        for (i in seq_len(nrow(concordance))) {
          row <- concordance[i, ]
          row_data <- data.frame(
            Subject = r$subject_id,
            Date = as.character(row$date),
            `Diary Bedtime` = row$diary_bedtime %||% "",
            `Diary Waketime` = row$diary_waketime %||% "",
            `Accel Onset` = if (!is.null(row$accel_onset)) format(row$accel_onset, "%I:%M %p") else "",
            `Accel Wake` = if (!is.null(row$accel_wake)) format(row$accel_wake, "%I:%M %p") else "",
            `Agreement (%)` = round(row$agreement_pct %||% NA, 1),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          all_rows[[length(all_rows) + 1]] <- row_data
        }
      }

      if (length(all_rows) == 0) {
        return(DT::datatable(
          data.frame(Message = "No diary data available. Enable diary integration and upload a CSV file."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      df <- do.call(rbind, all_rows)
      DT::datatable(
        df,
        options = list(pageLength = 15, scrollX = TRUE, dom = 'frtip'),
        rownames = FALSE,
        class = 'display compact stripe'
      )
    })


    # Helper to format datetime for export
    format_actilife_datetime <- function(dt) {
      if (is.null(dt) || length(dt) == 0 || is.na(dt)) return("")
      formatted <- format(as.POSIXct(dt), format = "%m/%d/%Y %I:%M:%S %p")
      formatted <- gsub("^0", "", formatted)
      formatted <- gsub("/0", "/", formatted)
      formatted
    }

    # Export Details CSV
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

          f <- shared$files[[r$file_id]]
          weight <- f$subject_info$weight_lbs %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% "Undefined"
          if (gender == "M") gender <- "Male"
          else if (gender == "F") gender <- "Female"
          else if (gender == "") gender <- "Undefined"

          algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke"
                               else if (r$algorithm == "sadeh") "Sadeh"
                               else r$algorithm

          for (i in 1:nrow(r$periods)) {
            period <- r$periods[i, ]
            in_bed_posix <- as.POSIXct(period$in_bed_time)
            onset_posix <- as.POSIXct(period$onset)
            latency <- as.numeric(difftime(onset_posix, in_bed_posix, units = "mins"))
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
              `Sleep Period Detection Algorithm` = r$detection_method %||% "Tudor-Locke",
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

    # Export Summary CSV
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

          f <- shared$files[[r$file_id]]
          weight <- f$subject_info$weight_lbs %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% "Undefined"
          if (gender == "M") gender <- "Male"
          else if (gender == "F") gender <- "Female"
          else if (gender == "") gender <- "Undefined"

          algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke"
                               else if (r$algorithm == "sadeh") "Sadeh"
                               else r$algorithm
          periods <- r$periods

          onset_times <- as.POSIXct(periods$onset)
          in_bed_times <- as.POSIXct(periods$in_bed_time)
          out_bed_times <- as.POSIXct(periods$out_bed_time)
          latencies <- as.numeric(difftime(onset_times, in_bed_times, units = "mins"))

          row_data <- data.frame(
            `Subject Name` = r$subject_id,
            `File Name` = r$name,
            `Serial Number` = r$serial_number %||% "",
            `Epoch Length` = r$epoch_length,
            Weight = weight,
            Age = age,
            Gender = gender,
            `Sleep/Wake Algorithm` = algorithm_display,
            `Sleep Period Detection Algorithm` = r$detection_method %||% "Tudor-Locke",
            `Number of Sleep Periods` = r$n_periods,
            `Average In Bed Time` = format_average_time(in_bed_times),
            `Average Out Bed Time` = format_average_time(out_bed_times),
            `Average Efficiency` = round(mean(periods$sleep_efficiency, na.rm = TRUE), 3),
            `Average Onset` = format_average_time(onset_times),
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
