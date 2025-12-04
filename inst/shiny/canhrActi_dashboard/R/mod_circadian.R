# Module

mod_circadian_ui <- function(id) {
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
                 icon("sun", style = "margin-right: 10px;"),
                 "Circadian Rhythm Analysis"),
              p(style = "margin: 0; opacity: 0.9; font-size: 13px;",
                "Analyze 24-hour activity patterns and circadian rhythm metrics: L5, M10, relative amplitude, and stability.")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right;",
                textOutput(ns("analysis_status"))
              )
            )
          )
        )
      )
    ),

    # Key metrics at top - Non-parametric metrics
    fluidRow(
      valueBoxOutput(ns("vb_l5"), width = 2),
      valueBoxOutput(ns("vb_m10"), width = 3),
      valueBoxOutput(ns("vb_ra"), width = 3),
      valueBoxOutput(ns("vb_is"), width = 2),
      valueBoxOutput(ns("vb_iv"), width = 2)
    ),

    # Cosinor metrics row
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: linear-gradient(135deg, #6f42c1 0%, #4a2c7a 100%); color: white; padding: 10px 15px; border-radius: 8px; margin-bottom: 15px;",
          span(icon("wave-square"), " ", strong("Cosinor Analysis"), " - Parametric rhythm fitting")
        )
      )
    ),
    fluidRow(
      valueBoxOutput(ns("vb_mesor"), width = 4),
      valueBoxOutput(ns("vb_amplitude"), width = 4),
      valueBoxOutput(ns("vb_acrophase"), width = 4)
    ),

    fluidRow(
      # Left column - Settings
      column(
        width = 3,
        box(
          title = span(icon("cogs", style = "margin-right: 8px;"), "Settings"),
          status = "primary", solidHeader = TRUE, width = NULL,

          div(
            style = "background: rgba(35,97,146,0.05); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
            selectInput(ns("metric"), span(icon("chart-line"), " Activity Metric"),
                        choices = c("Axis 1 (Vertical)" = "axis1",
                                    "Vector Magnitude" = "vm"),
                        selected = "axis1")
          ),

          div(
            style = "background: rgba(255,205,0,0.1); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
            checkboxInput(ns("use_wear_time"), span(icon("check-circle"), " Apply Wear Time Filter"), value = TRUE)
          ),

          hr(style = "border-color: #e8f4fc; margin: 15px 0;"),

          actionButton(ns("run_btn"), span(icon("play"), " Run Analysis"),
                      class = "btn-primary btn-block",
                      style = "background: linear-gradient(135deg, #236192 0%, #1a4a6f 100%); border: none; padding: 12px;"),

          hr(style = "border-color: #e8f4fc; margin: 15px 0;"),

          downloadButton(ns("dl_csv"), span(icon("download"), " Export CSV"),
                        class = "btn-success btn-block", style = "font-size: 12px;")
        ),

        # Metric definitions
        box(
          title = span(icon("info-circle", style = "margin-right: 8px;"), "Metric Definitions"),
          status = "info", width = NULL, collapsible = TRUE, collapsed = TRUE,
          div(
            style = "font-size: 11px; line-height: 1.8;",
            tags$p(tags$strong("Non-Parametric Metrics:")),
            tags$p(tags$span(style = "color: #236192; font-weight: 600;", "L5:"),
                  " Avg activity during least active 5 hours"),
            tags$p(tags$span(style = "color: #236192; font-weight: 600;", "M10:"),
                  " Avg activity during most active 10 hours"),
            tags$p(tags$span(style = "color: #FFCD00; font-weight: 600;", "RA:"),
                  " Relative amplitude = (M10-L5)/(M10+L5)"),
            tags$p(tags$span(style = "color: #28a745; font-weight: 600;", "IS:"),
                  " Interdaily stability (0-1, higher = consistent)"),
            tags$p(tags$span(style = "color: #dc3545; font-weight: 600;", "IV:"),
                  " Intradaily variability (lower = smoother)"),
            tags$hr(style = "margin: 8px 0;"),
            tags$p(tags$strong("Cosinor Parameters:")),
            tags$p(tags$span(style = "color: #6f42c1; font-weight: 600;", "MESOR:"),
                  " Midline estimating statistic of rhythm (mean level)"),
            tags$p(tags$span(style = "color: #6f42c1; font-weight: 600;", "Amplitude:"),
                  " Half peak-to-trough difference (rhythm strength)"),
            tags$p(tags$span(style = "color: #6f42c1; font-weight: 600;", "Acrophase:"),
                  " Time of peak activity (hours from midnight)")
          )
        )
      ),

      # Right column - Results
      column(
        width = 9,

        # File selector row
        box(
          title = NULL, width = 12,
          style = "background: linear-gradient(90deg, rgba(35,97,146,0.05) 0%, transparent 100%);",
          selectInput(ns("file_select"), span(icon("eye"), " View Results For:"),
                     choices = c("All Files (Average)" = "all"), width = "100%")
        ),

        # Main visualization - 24-Hour Activity Profile
        box(
          title = span(icon("chart-area", style = "margin-right: 8px;"), "24-Hour Activity Profile"),
          status = "primary", solidHeader = TRUE, width = 12,
          div(style = "background: white; border-radius: 8px; padding: 10px;",
            plotOutput(ns("profile_plot"), height = "320px")
          )
        ),

        # Summary table
        box(
          title = span(icon("table", style = "margin-right: 8px;"), "Circadian Summary"),
          status = "success", solidHeader = FALSE, width = 12,
          DT::dataTableOutput(ns("summary_table"))
        )
      )
    )
  )
}

mod_circadian_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Update file selector
    observe({
      if (shared$file_count == 0) {
        updateSelectInput(session, "file_select", choices = c("No files loaded" = "none"))
      } else {
        choices <- c("All Files (Average)" = "all")
        for (fid in names(shared$files)) {
          f <- shared$files[[fid]]
          choices <- c(choices, setNames(fid, paste0(f$subject_info$id, " - ", f$name)))
        }
        updateSelectInput(session, "file_select", choices = choices)
      }
    })

    # Run analysis
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      all_results <- list()
      n_files <- shared$file_count

      withProgress(message = "Analyzing circadian rhythm...", value = 0, {
        for (i in seq_along(names(shared$files))) {
          fid <- names(shared$files)[i]
          f <- shared$files[[fid]]
          data <- f$data

          setProgress(value = i / n_files, detail = paste("File:", f$name))

          # Activity metric
          activity <- if (input$metric == "vm" && all(c("axis1", "axis2", "axis3") %in% names(data))) {
            sqrt(data$axis1^2 + data$axis2^2 + data$axis3^2)
          } else {
            data$axis1
          }

          # Get wear time if using filter
          wear_time <- NULL
          if (input$use_wear_time && !is.null(shared$results$wear_time[[fid]])) {
            wear_time <- shared$results$wear_time[[fid]]$wear
          }

          res <- tryCatch({
            canhrActi::circadian.rhythm(
              counts = activity,
              timestamps = data$timestamp,
              wear_time = wear_time,
              epoch_length = f$epoch_length
            )
          }, error = function(e) {
            showNotification(paste("Error in", f$name, ":", e$message), type = "error")
            return(NULL)
          })

          if (is.null(res)) next

          # Extract cosinor results
          cosinor <- res$cosinor
          mesor <- if (!is.null(cosinor)) cosinor$mesor else NA
          amplitude <- if (!is.null(cosinor)) cosinor$amplitude else NA
          acrophase <- if (!is.null(cosinor)) cosinor$acrophase else NA

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            L5 = res$L5,
            L5_start = res$L5_start,
            M10 = res$M10,
            M10_start = res$M10_start,
            RA = res$RA,
            IS = res$IS,
            IV = res$IV,
            mesor = mesor,
            amplitude = amplitude,
            acrophase = acrophase,
            hourly_profile = res$hourly_profile
          )
        }
      })

      results(all_results)
      shared$results$circadian <- all_results

      showNotification(paste("Circadian analysis complete for", length(all_results), "files"), type = "message")
    })

    output$analysis_status <- renderText({
      n <- length(results())
      if (n == 0) "Not analyzed" else paste(n, "files analyzed")
    })

    # Current view data
    current_data <- reactive({
      res <- results()
      req(length(res) > 0)

      sel <- input$file_select

      if (sel == "all" || sel == "none") {
        list(
          mode = "all",
          results = res,
          L5 = mean(sapply(res, function(r) r$L5), na.rm = TRUE),
          M10 = mean(sapply(res, function(r) r$M10), na.rm = TRUE),
          RA = mean(sapply(res, function(r) r$RA), na.rm = TRUE),
          IS = mean(sapply(res, function(r) r$IS), na.rm = TRUE),
          IV = mean(sapply(res, function(r) r$IV), na.rm = TRUE),
          mesor = mean(sapply(res, function(r) r$mesor), na.rm = TRUE),
          amplitude = mean(sapply(res, function(r) r$amplitude), na.rm = TRUE),
          acrophase = mean(sapply(res, function(r) r$acrophase), na.rm = TRUE)
        )
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        list(
          mode = "single",
          result = r,
          L5 = r$L5,
          L5_start = r$L5_start,
          M10 = r$M10,
          M10_start = r$M10_start,
          RA = r$RA,
          IS = r$IS,
          IV = r$IV,
          mesor = r$mesor,
          amplitude = r$amplitude,
          acrophase = r$acrophase
        )
      } else {
        NULL
      }
    })

    # Value boxes - 5 key metrics
    output$vb_l5 <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$L5)) "--" else round(cd$L5)
      valueBox(val, "L5", icon = icon("moon"), color = "blue")
    })

    output$vb_m10 <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$M10)) "--" else round(cd$M10)
      valueBox(val, "M10", icon = icon("sun"), color = "yellow")
    })

    output$vb_ra <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$RA)) "--" else sprintf("%.3f", cd$RA)
      valueBox(val, "RA", icon = icon("arrows-alt-h"), color = "green")
    })

    output$vb_is <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$IS)) "--" else sprintf("%.2f", cd$IS)
      valueBox(val, "IS", icon = icon("calendar-check"), color = "aqua")
    })

    output$vb_iv <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$IV)) "--" else sprintf("%.2f", cd$IV)
      valueBox(val, "IV", icon = icon("chart-line"), color = "purple")
    })

    # Cosinor value boxes
    output$vb_mesor <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$mesor)) "--" else round(cd$mesor)
      valueBox(val, "MESOR (Mean Level)", icon = icon("minus"), color = "purple")
    })

    output$vb_amplitude <- renderValueBox({
      cd <- current_data()
      val <- if (is.null(cd) || is.na(cd$amplitude)) "--" else round(cd$amplitude)
      valueBox(val, "Amplitude (Rhythm Strength)", icon = icon("arrows-alt-v"), color = "purple")
    })

    output$vb_acrophase <- renderValueBox({
      cd <- current_data()
      if (is.null(cd) || is.na(cd$acrophase)) {
        val <- "--"
        subtitle <- "Acrophase (Peak Time)"
      } else {
        # Convert decimal hours to HH:MM format
        hours <- floor(cd$acrophase)
        minutes <- round((cd$acrophase - hours) * 60)
        val <- sprintf("%02d:%02d", hours, minutes)
        subtitle <- "Acrophase (Peak Time)"
      }
      valueBox(val, subtitle, icon = icon("clock"), color = "purple")
    })

    # Profile plot
    output$profile_plot <- renderPlot({
      cd <- current_data()
      req(cd)

      if (cd$mode == "single") {
        hourly <- cd$result$hourly_profile
        req(hourly)

        ggplot(hourly, aes(x = hour, y = mean_counts)) +
          geom_ribbon(aes(ymin = pmax(0, mean_counts - sd_counts),
                          ymax = mean_counts + sd_counts),
                      fill = "#3c8dbc", alpha = 0.2) +
          geom_line(color = "#3c8dbc", linewidth = 1.2) +
          geom_point(color = "#3c8dbc", size = 2.5) +
          scale_x_continuous(breaks = seq(0, 23, 3), labels = sprintf("%02d:00", seq(0, 23, 3))) +
          labs(title = paste("Activity Profile -", cd$result$subject_id),
               subtitle = sprintf("L5=%d (start %s)  |  M10=%d (start %s)  |  RA=%.3f",
                                  round(cd$L5), cd$L5_start, round(cd$M10), cd$M10_start, cd$RA),
               x = "Hour of Day", y = "Mean Activity (counts/min)") +
          theme_minimal(base_size = 13) +
          theme(plot.title = element_text(face = "bold"),
                panel.grid.minor = element_blank())
      } else {
        # Combine hourly profiles from all files
        all_hourly <- data.frame()
        for (r in cd$results) {
          if (!is.null(r$hourly_profile)) {
            h <- r$hourly_profile
            h$subject <- r$subject_id
            all_hourly <- rbind(all_hourly, h)
          }
        }

        req(nrow(all_hourly) > 0)

        avg_hourly <- aggregate(mean_counts ~ hour, all_hourly, mean, na.rm = TRUE)

        ggplot() +
          geom_line(data = all_hourly, aes(x = hour, y = mean_counts, group = subject),
                    color = "gray70", alpha = 0.5, linewidth = 0.5) +
          geom_line(data = avg_hourly, aes(x = hour, y = mean_counts),
                    color = "#3c8dbc", linewidth = 1.5) +
          geom_point(data = avg_hourly, aes(x = hour, y = mean_counts),
                     color = "#3c8dbc", size = 2.5) +
          scale_x_continuous(breaks = seq(0, 23, 3), labels = sprintf("%02d:00", seq(0, 23, 3))) +
          labs(title = paste("Average Activity Profile -", length(cd$results), "Files"),
               subtitle = sprintf("Avg: L5=%d  |  M10=%d  |  RA=%.3f  |  IS=%.2f  |  IV=%.2f",
                                  round(cd$L5), round(cd$M10), cd$RA, cd$IS, cd$IV),
               x = "Hour of Day", y = "Mean Activity (counts/min)") +
          theme_minimal(base_size = 13) +
          theme(plot.title = element_text(face = "bold"),
                panel.grid.minor = element_blank())
      }
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run analysis to see results"), rownames = FALSE))
      }

      # Format acrophase as HH:MM
      format_acrophase <- function(acro) {
        if (is.na(acro)) return("--")
        hours <- floor(acro)
        minutes <- round((acro - hours) * 60)
        sprintf("%02d:%02d", hours, minutes)
      }

      df <- data.frame(
        Subject = sapply(res, function(r) r$subject_id),
        File = sapply(res, function(r) r$name),
        L5 = sapply(res, function(r) round(r$L5)),
        L5_Start = sapply(res, function(r) r$L5_start),
        M10 = sapply(res, function(r) round(r$M10)),
        M10_Start = sapply(res, function(r) r$M10_start),
        RA = sapply(res, function(r) round(r$RA, 3)),
        IS = sapply(res, function(r) round(r$IS, 3)),
        IV = sapply(res, function(r) round(r$IV, 3)),
        MESOR = sapply(res, function(r) if (is.na(r$mesor)) NA else round(r$mesor)),
        Amplitude = sapply(res, function(r) if (is.na(r$amplitude)) NA else round(r$amplitude)),
        Acrophase = sapply(res, function(r) format_acrophase(r$acrophase)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE
      ) |> DT::formatStyle(
        columns = c("MESOR", "Amplitude", "Acrophase"),
        backgroundColor = "#f3e8ff"
      )
    })

    # Export CSV
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("circadian_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- results()
        req(length(res) > 0)

        df <- data.frame(
          subject_id = sapply(res, function(r) r$subject_id),
          file_name = sapply(res, function(r) r$name),
          L5 = sapply(res, function(r) r$L5),
          L5_start = sapply(res, function(r) r$L5_start),
          M10 = sapply(res, function(r) r$M10),
          M10_start = sapply(res, function(r) r$M10_start),
          RA = sapply(res, function(r) r$RA),
          IS = sapply(res, function(r) r$IS),
          IV = sapply(res, function(r) r$IV),
          mesor = sapply(res, function(r) r$mesor),
          amplitude = sapply(res, function(r) r$amplitude),
          acrophase_hours = sapply(res, function(r) r$acrophase),
          stringsAsFactors = FALSE
        )

        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
