#' Sedentary Fragmentation Module
#'
#' Analyzes patterns of sedentary behavior accumulation

mod_sedentary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: linear-gradient(135deg, #6c757d 0%, #495057 100%); color: white; padding: 15px 20px; border-radius: 10px; margin-bottom: 20px;",
          h3(icon("couch"), " Sedentary Fragmentation Analysis", style = "margin: 0;"),
          p("Analyze patterns of how sedentary time is accumulated", style = "margin: 5px 0 0 0; opacity: 0.9;")
        )
      )
    ),

    # Analysis button
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
          fluidRow(
            column(4,
              actionButton(ns("analyze"), "Analyze Sedentary Patterns",
                          icon = icon("play"), class = "btn-primary btn-lg",
                          style = "width: 100%;")
            ),
            column(4,
              selectInput(ns("cut_points"), "Cut Points:",
                         choices = c("Freedson (1998)" = "freedson",
                                    "CANHR (2025)" = "canhr"),
                         selected = "freedson", width = "100%")
            ),
            column(4,
              div(style = "padding-top: 25px;",
                textOutput(ns("analysis_status"))
              )
            )
          )
        )
      )
    ),

    # Key metrics value boxes
    fluidRow(
      valueBoxOutput(ns("vb_total_sed"), width = 3),
      valueBoxOutput(ns("vb_breaks_per_hour"), width = 3),
      valueBoxOutput(ns("vb_mean_bout"), width = 3),
      valueBoxOutput(ns("vb_alpha"), width = 3)
    ),

    fluidRow(
      valueBoxOutput(ns("vb_total_bouts"), width = 3),
      valueBoxOutput(ns("vb_median_bout"), width = 3),
      valueBoxOutput(ns("vb_max_bout"), width = 3),
      valueBoxOutput(ns("vb_gini"), width = 3)
    ),

    # Metric definitions
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: #e9ecef; padding: 12px 15px; border-radius: 8px; margin-bottom: 20px;",
          strong("Metric Definitions: "),
          span("Breaks/Sed Hour = interruptions per hour of sitting | "),
          span("Mean Bout = average sitting spell length | "),
          span("Alpha = fragmentation index (higher = more fragmented) | "),
          span("Gini = inequality in bout lengths (0 = equal, 1 = dominated by long bouts)")
        )
      )
    ),

    # Plots and tables
    fluidRow(
      column(6,
        div(style = "background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          h4("Bout Duration Distribution"),
          plotOutput(ns("bout_dist_plot"), height = "320px")
        )
      ),
      column(6,
        div(style = "background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          h4("Sedentary Accumulation Curve"),
          plotOutput(ns("accumulation_plot"), height = "320px")
        )
      )
    ),

    fluidRow(
      column(12,
        div(style = "background: white; border-radius: 8px; padding: 15px;",
          h4("Detailed Results"),
          fluidRow(
            column(6,
              selectInput(ns("file_select"), "Select File:",
                         choices = c("All Files (Average)" = "all"),
                         width = "100%")
            ),
            column(6,
              div(style = "padding-top: 25px;",
                downloadButton(ns("dl_csv"), "Export CSV", class = "btn-success")
              )
            )
          ),
          DT::dataTableOutput(ns("summary_table"))
        )
      )
    )
  )
}

mod_sedentary_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Update file selector when files change
    observe({
      files <- shared$files
      if (length(files) > 0) {
        choices <- c("All Files (Average)" = "all")
        for (fid in names(files)) {
          f <- files[[fid]]
          choices[f$subject_info$id] <- fid
        }
        updateSelectInput(session, "file_select", choices = choices)
      }
    })

    # Run analysis
    observeEvent(input$analyze, {
      req(length(shared$files) > 0)

      # Check if wear time has been analyzed
      wt_results <- shared$results$wear_time
      use_wear_time <- !is.null(wt_results) && length(wt_results) > 0

      all_results <- list()

      withProgress(message = "Analyzing sedentary patterns...", value = 0, {
        n_files <- length(shared$files)

        for (i in seq_along(names(shared$files))) {
          fid <- names(shared$files)[i]
          f <- shared$files[[fid]]
          data <- f$data

          setProgress(value = i / n_files, detail = f$subject_info$id)

          # Need timestamps for fragmentation analysis
          if (!"timestamp" %in% names(data)) {
            showNotification(paste(f$name, ": No timestamps available"), type = "warning")
            next
          }

          counts <- data$axis1
          epoch_length <- f$epoch_length

          # Convert to CPM
          cpm <- canhrActi::to_cpm(counts, epoch_length)

          # Get wear time mask
          wear_mask <- NULL
          if (use_wear_time && fid %in% names(wt_results)) {
            wear_mask <- wt_results[[fid]]$wear
          }

          # Calculate intensity
          intensity <- tryCatch({
            if (input$cut_points == "freedson") {
              canhrActi::freedson(cpm)
            } else {
              canhrActi::CANHR.Cutpoints(cpm)
            }
          }, error = function(e) NULL)

          if (is.null(intensity)) next

          # Run fragmentation analysis
          fragmentation <- tryCatch({
            canhrActi::sedentary.fragmentation(
              intensity = intensity,
              timestamps = data$timestamp,
              wear_time = wear_mask,
              epoch_length = epoch_length
            )
          }, error = function(e) {
            showNotification(paste(f$name, ":", e$message), type = "error")
            NULL
          })

          if (is.null(fragmentation)) next

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            fragmentation = fragmentation
          )
        }
      })

      results(all_results)
      shared$results$sedentary <- all_results

      showNotification(
        paste("Sedentary analysis complete for", length(all_results), "files"),
        type = "message"
      )
    })

    output$analysis_status <- renderText({
      n <- length(results())
      if (n == 0) "Not analyzed" else paste(n, "files analyzed")
    })

    # Helper to get current fragmentation data
    current_frag <- reactive({
      res <- results()
      req(length(res) > 0)

      sel <- input$file_select

      if (sel == "all") {
        # Average across all files
        list(
          mode = "all",
          total_sedentary_min = mean(sapply(res, function(r) r$fragmentation$total_sedentary_min), na.rm = TRUE),
          total_bouts = sum(sapply(res, function(r) r$fragmentation$total_bouts), na.rm = TRUE),
          mean_bout_duration = mean(sapply(res, function(r) r$fragmentation$mean_bout_duration), na.rm = TRUE),
          median_bout_duration = mean(sapply(res, function(r) r$fragmentation$median_bout_duration), na.rm = TRUE),
          max_bout_duration = max(sapply(res, function(r) r$fragmentation$max_bout_duration), na.rm = TRUE),
          breaks_per_sed_hour = mean(sapply(res, function(r) r$fragmentation$breaks_per_sed_hour), na.rm = TRUE),
          alpha = mean(sapply(res, function(r) r$fragmentation$alpha), na.rm = TRUE),
          gini = mean(sapply(res, function(r) r$fragmentation$gini), na.rm = TRUE)
        )
      } else if (sel %in% names(res)) {
        r <- res[[sel]]
        list(
          mode = "single",
          total_sedentary_min = r$fragmentation$total_sedentary_min,
          total_bouts = r$fragmentation$total_bouts,
          mean_bout_duration = r$fragmentation$mean_bout_duration,
          median_bout_duration = r$fragmentation$median_bout_duration,
          max_bout_duration = r$fragmentation$max_bout_duration,
          breaks_per_sed_hour = r$fragmentation$breaks_per_sed_hour,
          alpha = r$fragmentation$alpha,
          gini = r$fragmentation$gini
        )
      } else {
        NULL
      }
    })

    # Value boxes
    output$vb_total_sed <- renderValueBox({
      cf <- current_frag()
      if (is.null(cf)) {
        valueBox("--", "Total Sedentary", icon = icon("couch"), color = "olive")
      } else {
        hours <- round(cf$total_sedentary_min / 60, 1)
        valueBox(paste0(hours, "h"), "Total Sedentary", icon = icon("couch"), color = "olive")
      }
    })

    output$vb_breaks_per_hour <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$breaks_per_sed_hour)) "--" else round(cf$breaks_per_sed_hour, 1)
      valueBox(val, "Breaks/Sed Hour", icon = icon("pause-circle"), color = "maroon")
    })

    output$vb_mean_bout <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$mean_bout_duration)) "--" else paste0(round(cf$mean_bout_duration, 1), "m")
      valueBox(val, "Mean Bout", icon = icon("clock"), color = "maroon")
    })

    output$vb_alpha <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$alpha)) "--" else round(cf$alpha, 2)
      valueBox(val, "Alpha", icon = icon("chart-line"), color = "maroon")
    })

    output$vb_total_bouts <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$total_bouts)) "--" else cf$total_bouts
      valueBox(val, "Total Bouts", icon = icon("list"), color = "navy")
    })

    output$vb_median_bout <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$median_bout_duration)) "--" else paste0(round(cf$median_bout_duration, 1), "m")
      valueBox(val, "Median Bout", icon = icon("clock"), color = "navy")
    })

    output$vb_max_bout <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$max_bout_duration)) "--" else paste0(round(cf$max_bout_duration), "m")
      valueBox(val, "Max Bout", icon = icon("arrow-up"), color = "navy")
    })

    output$vb_gini <- renderValueBox({
      cf <- current_frag()
      val <- if (is.null(cf) || is.na(cf$gini)) "--" else round(cf$gini, 3)
      valueBox(val, "Gini Index", icon = icon("balance-scale"), color = "navy")
    })

    # Bout distribution plot
    output$bout_dist_plot <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                           label = "Click 'Analyze Sedentary Patterns' to see results",
                           size = 5, hjust = 0.5) +
          ggplot2::theme_void()
      } else {
        # Combine bout distributions
        all_dist <- data.frame()
        for (r in res) {
          if (!is.null(r$fragmentation$bout_distribution)) {
            d <- r$fragmentation$bout_distribution
            d$subject <- r$subject_id
            all_dist <- rbind(all_dist, d)
          }
        }

        if (nrow(all_dist) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                             label = "No bout data available", size = 5, hjust = 0.5) +
            ggplot2::theme_void()
        } else {
          agg_dist <- aggregate(count ~ category, all_dist, sum)
          agg_dist$category <- factor(agg_dist$category,
                                       levels = c("1-5 min", "5-10 min", "10-20 min",
                                                 "20-30 min", "30-60 min", ">60 min"))

          ggplot2::ggplot(agg_dist, ggplot2::aes(x = category, y = count)) +
            ggplot2::geom_col(fill = "#6c757d", alpha = 0.8) +
            ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 4) +
            ggplot2::labs(
              title = "Sedentary Bout Duration Distribution",
              subtitle = paste("Total:", sum(agg_dist$count), "bouts"),
              x = "Bout Duration", y = "Number of Bouts"
            ) +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold"),
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
        }
      }
    })

    # Accumulation curve
    output$accumulation_plot <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                           label = "Click 'Analyze Sedentary Patterns' to see results",
                           size = 5, hjust = 0.5) +
          ggplot2::theme_void()
      } else {
        # Combine all bouts
        all_bouts <- data.frame()
        for (r in res) {
          if (!is.null(r$fragmentation$bouts) && nrow(r$fragmentation$bouts) > 0) {
            b <- r$fragmentation$bouts
            b$subject <- r$subject_id
            all_bouts <- rbind(all_bouts, b)
          }
        }

        if (nrow(all_bouts) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                             label = "No bout data available", size = 5, hjust = 0.5) +
            ggplot2::theme_void()
        } else {
          all_bouts <- all_bouts[order(all_bouts$duration_min, decreasing = TRUE), ]
          total_sed <- sum(all_bouts$duration_min)
          all_bouts$cum_time <- cumsum(all_bouts$duration_min)
          all_bouts$cum_pct <- all_bouts$cum_time / total_sed * 100
          all_bouts$bout_rank <- seq_len(nrow(all_bouts))
          all_bouts$bout_pct <- all_bouts$bout_rank / nrow(all_bouts) * 100

          # Calculate Gini
          n <- nrow(all_bouts)
          x <- sort(all_bouts$duration_min)
          gini <- (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))

          ggplot2::ggplot(all_bouts, ggplot2::aes(x = bout_pct, y = cum_pct)) +
            ggplot2::geom_line(color = "#6c757d", linewidth = 1.5) +
            ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
            ggplot2::labs(
              title = "Sedentary Time Accumulation",
              subtitle = sprintf("Gini = %.3f", gini),
              x = "% of Bouts (longest to shortest)",
              y = "% of Total Sedentary Time"
            ) +
            ggplot2::scale_x_continuous(limits = c(0, 100)) +
            ggplot2::scale_y_continuous(limits = c(0, 100)) +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
        }
      }
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(
          data.frame(Message = "Click 'Analyze Sedentary Patterns' to see results"),
          rownames = FALSE
        ))
      }

      df <- data.frame(
        Subject = sapply(res, function(r) r$subject_id),
        File = sapply(res, function(r) r$name),
        `Sedentary (min)` = sapply(res, function(r) round(r$fragmentation$total_sedentary_min)),
        `Total Bouts` = sapply(res, function(r) r$fragmentation$total_bouts),
        `Mean Bout (min)` = sapply(res, function(r) r$fragmentation$mean_bout_duration),
        `Median Bout (min)` = sapply(res, function(r) r$fragmentation$median_bout_duration),
        `Max Bout (min)` = sapply(res, function(r) r$fragmentation$max_bout_duration),
        `Breaks/Sed Hour` = sapply(res, function(r) r$fragmentation$breaks_per_sed_hour),
        Alpha = sapply(res, function(r) r$fragmentation$alpha),
        Gini = sapply(res, function(r) r$fragmentation$gini),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # CSV export
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("sedentary_fragmentation_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- results()
        req(length(res) > 0)

        df <- data.frame(
          subject_id = sapply(res, function(r) r$subject_id),
          file_name = sapply(res, function(r) r$name),
          total_sedentary_min = sapply(res, function(r) r$fragmentation$total_sedentary_min),
          total_bouts = sapply(res, function(r) r$fragmentation$total_bouts),
          mean_bout_min = sapply(res, function(r) r$fragmentation$mean_bout_duration),
          median_bout_min = sapply(res, function(r) r$fragmentation$median_bout_duration),
          max_bout_min = sapply(res, function(r) r$fragmentation$max_bout_duration),
          breaks_total = sapply(res, function(r) r$fragmentation$breaks_total),
          breaks_per_sed_hour = sapply(res, function(r) r$fragmentation$breaks_per_sed_hour),
          alpha = sapply(res, function(r) r$fragmentation$alpha),
          gini = sapply(res, function(r) r$fragmentation$gini),
          stringsAsFactors = FALSE
        )

        write.csv(df, file, row.names = FALSE)
      }
    )

  })
}
