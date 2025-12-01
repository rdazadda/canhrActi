# Module

mod_overview_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    # File selector at top
    column(
      width = 12,
      box(
        title = "File Selection", status = "info", solidHeader = TRUE,
        width = NULL, collapsible = TRUE,
        fluidRow(
          column(6, selectInput(ns("file_select"), "View Results For:", choices = c("All Files (Summary)" = "all"))),
          column(6, tags$div(style = "padding-top: 25px;", textOutput(ns("file_count_text"))))
        )
      )
    ),

    # Value boxes
    valueBoxOutput(ns("vb_valid_days"), width = 3),
    valueBoxOutput(ns("vb_wear_time"), width = 3),
    valueBoxOutput(ns("vb_mvpa"), width = 3),
    valueBoxOutput(ns("vb_steps"), width = 3),

    # Activity profile
    box(
      title = "Daily Activity Profile", status = "primary", solidHeader = TRUE,
      width = 8,
      plotOutput(ns("activity_plot"), height = "350px")
    ),

    # Data quality
    box(
      title = "Data Quality", status = "info", solidHeader = TRUE,
      width = 4,
      uiOutput(ns("quality_info"))
    ),

    # Intensity distribution
    box(
      title = "Time in Intensity Levels", status = "primary", solidHeader = TRUE,
      width = 6,
      plotOutput(ns("intensity_plot"), height = "300px")
    ),

    # Summary table for batch
    box(
      title = "Batch Summary", status = "success", solidHeader = TRUE,
      width = 6,
      DT::dataTableOutput(ns("batch_summary_table"))
    )
  )
}

mod_overview_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update file selector
    observe({
      if (shared$file_count == 0) {
        updateSelectInput(session, "file_select", choices = c("No files loaded" = "none"))
      } else {
        choices <- c("All Files (Summary)" = "all")
        file_choices <- sapply(shared$files, function(f) f$name)
        names(file_choices) <- sapply(shared$files, function(f) paste0(f$subject_info$id, " - ", f$name))
        choices <- c(choices, setNames(names(shared$files), names(file_choices)))
        updateSelectInput(session, "file_select", choices = choices)
      }
    })

    output$file_count_text <- renderText({
      paste(shared$file_count, "file(s) loaded")
    })

    # Compute analysis for all files
    batch_analysis <- reactive({
      req(shared$data_loaded, shared$file_count > 0)

      results <- list()

      for (fid in names(shared$files)) {
        f <- shared$files[[fid]]
        data <- f$data

        if (!"axis1" %in% names(data)) next

        # Wear time
        wear <- tryCatch({
          canhrActi::wear.choi(data$axis1, epoch_length = f$epoch_length)
        }, error = function(e) rep(TRUE, nrow(data)))

        # Intensity
        intensity <- tryCatch({
          canhrActi::freedson(data$axis1)
        }, error = function(e) NULL)

        # Daily summary
        daily <- NULL
        if ("timestamp" %in% names(data)) {
          temp_data <- data
          temp_data$date <- as.Date(temp_data$timestamp)
          temp_data$wear <- wear
          daily <- aggregate(wear ~ date, temp_data, sum)
          daily$wear_hours <- daily$wear * f$epoch_length / 3600
          daily$valid <- daily$wear_hours >= 10
        }

        # MVPA calculation
        mvpa_min <- 0
        if (!is.null(intensity)) {
          mvpa_epochs <- sum(intensity %in% c("moderate", "vigorous", "very_vigorous"))
          mvpa_min <- mvpa_epochs * f$epoch_length / 60
        }

        # Steps
        steps_total <- if ("steps" %in% names(data)) sum(data$steps, na.rm = TRUE) else NA

        results[[fid]] <- list(
          file_id = fid,
          name = f$name,
          subject_id = f$subject_info$id,
          wear = wear,
          intensity = intensity,
          daily = daily,
          valid_days = if (!is.null(daily)) sum(daily$valid) else 0,
          total_days = if (!is.null(daily)) nrow(daily) else 0,
          avg_wear_hours = if (!is.null(daily) && any(daily$valid)) mean(daily$wear_hours[daily$valid], na.rm = TRUE) else NA,
          mvpa_min = mvpa_min,
          steps_total = steps_total
        )
      }

      results
    })

    # Get current view data (selected file or all)
    current_data <- reactive({
      results <- batch_analysis()
      req(results)

      sel <- input$file_select

      if (sel == "all" || sel == "none") {
        # Aggregate all results
        list(
          mode = "all",
          results = results,
          valid_days = sum(sapply(results, function(r) r$valid_days)),
          total_days = sum(sapply(results, function(r) r$total_days)),
          avg_wear_hours = mean(sapply(results, function(r) r$avg_wear_hours), na.rm = TRUE),
          mvpa_min = sum(sapply(results, function(r) r$mvpa_min), na.rm = TRUE),
          steps_total = sum(sapply(results, function(r) r$steps_total), na.rm = TRUE)
        )
      } else if (sel %in% names(results)) {
        # Single file
        r <- results[[sel]]
        list(
          mode = "single",
          file_id = sel,
          result = r,
          valid_days = r$valid_days,
          total_days = r$total_days,
          avg_wear_hours = r$avg_wear_hours,
          mvpa_min = r$mvpa_min,
          steps_total = r$steps_total
        )
      } else {
        NULL
      }
    })

    # Value boxes
    output$vb_valid_days <- renderValueBox({
      cd <- current_data()
      if (is.null(cd)) {
        valueBox("--", "Valid Days", icon = icon("calendar-check"), color = "blue")
      } else {
        valueBox(cd$valid_days, "Valid Days", icon = icon("calendar-check"), color = "blue")
      }
    })

    output$vb_wear_time <- renderValueBox({
      cd <- current_data()
      if (is.null(cd) || is.na(cd$avg_wear_hours)) {
        valueBox("--", "Avg Wear Time", icon = icon("clock"), color = "green")
      } else {
        valueBox(paste0(round(cd$avg_wear_hours, 1), "h"), "Avg Wear Time", icon = icon("clock"), color = "green")
      }
    })

    output$vb_mvpa <- renderValueBox({
      cd <- current_data()
      if (is.null(cd)) {
        valueBox("--", "Total MVPA", icon = icon("running"), color = "yellow")
      } else {
        n_days <- max(cd$total_days, 1)
        daily_mvpa <- round(cd$mvpa_min / n_days)
        valueBox(paste0(daily_mvpa, " min/day"), "Avg MVPA", icon = icon("running"), color = "yellow")
      }
    })

    output$vb_steps <- renderValueBox({
      cd <- current_data()
      if (is.null(cd) || is.na(cd$steps_total)) {
        valueBox("N/A", "Daily Steps", icon = icon("shoe-prints"), color = "purple")
      } else {
        n_days <- max(cd$total_days, 1)
        avg_steps <- round(cd$steps_total / n_days)
        valueBox(format(avg_steps, big.mark = ","), "Daily Steps", icon = icon("shoe-prints"), color = "purple")
      }
    })

    # Activity plot
    output$activity_plot <- renderPlot({
      cd <- current_data()
      req(cd)

      if (cd$mode == "single") {
        # Single file plot
        f <- shared$files[[cd$file_id]]
        data <- f$data
        req("timestamp" %in% names(data), "axis1" %in% names(data))

        data$hour <- as.numeric(format(data$timestamp, "%H"))
        hourly <- aggregate(axis1 ~ hour, data, mean, na.rm = TRUE)

        ggplot(hourly, aes(x = hour, y = axis1)) +
          geom_area(fill = "#3c8dbc", alpha = 0.3) +
          geom_line(color = "#3c8dbc", linewidth = 1.5) +
          geom_point(color = "#f39c12", size = 3) +
          scale_x_continuous(breaks = seq(0, 23, 3), labels = sprintf("%02d:00", seq(0, 23, 3))) +
          labs(title = paste("Activity Profile -", f$subject_info$id), x = "Hour", y = "Mean Counts") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold", color = "#3c8dbc"))
      } else {
        # Aggregate plot across all files
        all_hourly <- data.frame()

        for (fid in names(shared$files)) {
          f <- shared$files[[fid]]
          data <- f$data
          if (!"timestamp" %in% names(data) || !"axis1" %in% names(data)) next

          data$hour <- as.numeric(format(data$timestamp, "%H"))
          hourly <- aggregate(axis1 ~ hour, data, mean, na.rm = TRUE)
          hourly$file <- fid
          hourly$subject <- f$subject_info$id
          all_hourly <- rbind(all_hourly, hourly)
        }

        if (nrow(all_hourly) == 0) return(NULL)

        # Average across files
        avg_hourly <- aggregate(axis1 ~ hour, all_hourly, mean, na.rm = TRUE)

        ggplot() +
          geom_line(data = all_hourly, aes(x = hour, y = axis1, group = file), color = "gray70", alpha = 0.5) +
          geom_area(data = avg_hourly, aes(x = hour, y = axis1), fill = "#3c8dbc", alpha = 0.3) +
          geom_line(data = avg_hourly, aes(x = hour, y = axis1), color = "#3c8dbc", linewidth = 1.5) +
          geom_point(data = avg_hourly, aes(x = hour, y = axis1), color = "#f39c12", size = 3) +
          scale_x_continuous(breaks = seq(0, 23, 3), labels = sprintf("%02d:00", seq(0, 23, 3))) +
          labs(title = paste("Average Activity Profile -", shared$file_count, "Files"),
               subtitle = "Gray lines = individual files, Blue = average",
               x = "Hour", y = "Mean Counts") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold", color = "#3c8dbc"))
      }
    })

    # Quality info
    output$quality_info <- renderUI({
      cd <- current_data()
      if (is.null(cd)) {
        return(tags$div(class = "text-center text-muted", style = "padding: 40px;",
                        icon("chart-bar", "fa-3x"), tags$p("Load data to see quality info")))
      }

      total <- cd$total_days
      valid <- cd$valid_days
      pct <- if (total > 0) round(valid / total * 100) else 0

      quality_color <- if (pct >= 80) "green" else if (pct >= 50) "orange" else "red"
      quality_text <- if (pct >= 80) "Excellent" else if (pct >= 50) "Good" else "Poor"

      n_files <- if (cd$mode == "all") shared$file_count else 1

      tags$div(
        tags$div(style = paste0("text-align: center; padding: 20px; background: ", quality_color, "; color: white; border-radius: 8px; margin-bottom: 15px;"),
                 tags$h2(quality_text, style = "margin: 0;"),
                 tags$p(paste0(pct, "% valid days"), style = "margin: 5px 0 0 0;")),
        tags$dl(class = "dl-horizontal",
                tags$dt("Files:"), tags$dd(n_files),
                tags$dt("Total Days:"), tags$dd(total),
                tags$dt("Valid Days (≥10h):"), tags$dd(valid),
                tags$dt("Epoch Length:"), tags$dd(paste(shared$epoch_length, "sec")))
      )
    })

    # Intensity plot
    output$intensity_plot <- renderPlot({
      cd <- current_data()
      req(cd)

      if (cd$mode == "single") {
        intensity <- cd$result$intensity
        req(intensity)
      } else {
        # Combine intensity from all files
        all_intensity <- c()
        for (r in cd$results) {
          if (!is.null(r$intensity)) {
            all_intensity <- c(all_intensity, r$intensity)
          }
        }
        intensity <- all_intensity
        req(length(intensity) > 0)
      }

      counts <- table(intensity)
      df <- data.frame(intensity = names(counts), epochs = as.numeric(counts))
      df$hours <- df$epochs * shared$epoch_length / 3600
      df$intensity <- factor(df$intensity, levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
      df <- df[!is.na(df$intensity), ]

      colors <- c("sedentary" = "#95a5a6", "light" = "#3498db", "moderate" = "#f1c40f",
                  "vigorous" = "#e67e22", "very_vigorous" = "#e74c3c")

      ggplot(df, aes(x = intensity, y = hours, fill = intensity)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = colors, guide = "none") +
        labs(title = "Time in Each Intensity Level", x = "", y = "Hours") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", color = "#3c8dbc"))
    })

    # Batch summary table
    output$batch_summary_table <- DT::renderDataTable({
      results <- batch_analysis()
      if (is.null(results) || length(results) == 0) {
        return(DT::datatable(data.frame(Message = "Load files to see summary"), rownames = FALSE))
      }

      df <- data.frame(
        Subject = sapply(results, function(r) r$subject_id),
        File = sapply(results, function(r) r$name),
        Valid_Days = sapply(results, function(r) r$valid_days),
        Total_Days = sapply(results, function(r) r$total_days),
        Avg_Wear_h = sapply(results, function(r) if (is.na(r$avg_wear_hours)) NA else round(r$avg_wear_hours, 1)),
        MVPA_min = sapply(results, function(r) round(r$mvpa_min)),
        Steps = sapply(results, function(r) if (is.na(r$steps_total)) NA else format(r$steps_total, big.mark = ",")),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subject", "File", "Valid Days", "Total Days", "Avg Wear (h)", "MVPA (min)", "Steps")
      )
    })
  })
}
