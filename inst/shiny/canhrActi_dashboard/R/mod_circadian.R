# Circadian Rhythm Analysis Module
# Clean, professional design with hero visualization and organized metrics

mod_circadian_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Page Header
    page_header(
      icon_name = "sun",
      title = "Circadian Rhythm Analysis",
      subtitle = "24-hour activity patterns and rhythm metrics",
      status_output_id = ns("analysis_status_badge")
    ),

    # Control Bar - File selector and Run button
    div(class = "circadian-control-bar",
      div(class = "circadian-controls-left",
        div(class = "circadian-file-select",
          tags$label("Subject", class = "circadian-label"),
          selectInput(ns("file_select"), NULL,
                      choices = c("All Files (Average)" = "all"), width = "220px")
        )
      ),
      div(class = "circadian-controls-right",
        actionButton(ns("run_btn"), "Run Analysis",
                    class = "btn-primary circadian-run-btn"),
        # Settings toggle
        actionButton(ns("toggle_settings"), NULL,
                    class = "btn-default circadian-settings-toggle",
                    icon = icon("sliders-h"))
      )
    ),

    # Collapsible Settings Panel
    shinyjs::hidden(
      div(id = ns("settings_panel"), class = "circadian-settings-panel",
        div(class = "circadian-settings-grid",
          div(class = "circadian-setting-item",
            tags$label("Activity Metric", class = "circadian-label"),
            selectInput(ns("metric"), NULL,
                        choices = c("Axis 1 (Vertical)" = "axis1",
                                    "Vector Magnitude" = "vm"),
                        selected = "axis1", width = "100%")
          ),
          div(class = "circadian-setting-item",
            tags$label("Options", class = "circadian-label"),
            checkboxInput(ns("use_wear_time"), "Apply Wear Time Filter", value = TRUE)
          )
        )
      )
    ),

    # Core Metrics Strip - key metrics only
    uiOutput(ns("core_metrics_panel")),

    # Main Content Area
    fluidRow(
      # Left: Hero Chart
      column(8,
        # Tabbed Chart Panel
        div(class = "circadian-chart-panel",
          div(class = "circadian-chart-tabs",
            tags$button(id = ns("tab_profile"), class = "circadian-tab active",
                       onclick = paste0("Shiny.setInputValue('", ns("active_tab"), "', 'profile', {priority: 'event'})"),
                       "24-Hour Profile"),
            tags$button(id = ns("tab_actogram"), class = "circadian-tab",
                       onclick = paste0("Shiny.setInputValue('", ns("active_tab"), "', 'actogram', {priority: 'event'})"),
                       "Actogram"),
            tags$button(id = ns("tab_cosinor"), class = "circadian-tab",
                       onclick = paste0("Shiny.setInputValue('", ns("active_tab"), "', 'cosinor', {priority: 'event'})"),
                       "Cosinor Fit")
          ),
          div(class = "circadian-chart-content",
            conditionalPanel(
              condition = "output.has_circadian_results == false",
              ns = ns,
              chart_empty_state(
                title = "No Data",
                message = "Click 'Run Analysis' to generate circadian visualizations",
                show_icon = FALSE
              )
            ),
            conditionalPanel(
              condition = "output.has_circadian_results == true",
              ns = ns,
              plotOutput(ns("main_chart"), height = "420px")
            )
          )
        )
      ),

      # Right: Cosinor Results Card
      column(4,
        # Pattern Classification Card
        div(class = "circadian-pattern-card",
          uiOutput(ns("pattern_card"))
        ),

        # Cosinor Parameters Card
        div(class = "circadian-cosinor-card",
          div(class = "circadian-card-header", "Cosinor Analysis"),
          uiOutput(ns("cosinor_panel"))
        ),

        # Export
        div(class = "circadian-export-section",
          downloadButton(ns("dl_csv"), "Export Results (CSV)",
                        class = "btn-default btn-block circadian-export-btn")
        )
      )
    ),

    # Summary Table (collapsed by default)
    div(class = "circadian-table-section",
      tags$details(class = "circadian-details",
        tags$summary("View Full Data Table"),
        div(class = "circadian-table-wrapper",
          DT::dataTableOutput(ns("summary_table"))
        )
      )
    ),

    # Reference Panel (collapsed)
    div(class = "circadian-reference-section",
      tags$details(class = "circadian-details",
        tags$summary("Metric Definitions"),
        div(class = "circadian-reference-content",
          div(class = "circadian-ref-grid",
            div(class = "circadian-ref-group",
              h4("Non-Parametric Metrics"),
              tags$dl(
                tags$dt("L5"), tags$dd("Average activity during least active 5-hour window"),
                tags$dt("M10"), tags$dd("Average activity during most active 10-hour window"),
                tags$dt("RA"), tags$dd("Relative Amplitude: (M10-L5)/(M10+L5). Range 0-1, higher = stronger rhythm"),
                tags$dt("IS"), tags$dd("Interdaily Stability: consistency of rhythm across days (0-1)"),
                tags$dt("IV"), tags$dd("Intradaily Variability: fragmentation within days (lower = smoother)")
              )
            ),
            div(class = "circadian-ref-group",
              h4("Cosinor Parameters"),
              tags$dl(
                tags$dt("MESOR"), tags$dd("Midline Estimating Statistic of Rhythm (24h mean)"),
                tags$dt("Amplitude"), tags$dd("Half the peak-to-trough difference"),
                tags$dt("Acrophase"), tags$dd("Time of peak activity"),
                tags$dt("R-squared"), tags$dd("Proportion of variance explained by the model")
              )
            ),
            div(class = "circadian-ref-group",
              h4("Pattern Classification"),
              tags$dl(
                tags$dt("Unimodal"), tags$dd("Single daily peak, typical healthy pattern"),
                tags$dt("Bimodal"), tags$dd("Two distinct peaks (morning/evening)"),
                tags$dt("Mixed"), tags$dd("Complex pattern with multiple components")
              )
            )
          )
        )
      )
    )
  )
}

mod_circadian_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())
    active_tab <- reactiveVal("profile")

    # Output for conditional panel
    output$has_circadian_results <- reactive({
      length(results()) > 0
    })
    outputOptions(output, "has_circadian_results", suspendWhenHidden = FALSE)

    # Toggle settings panel
    observeEvent(input$toggle_settings, {
      shinyjs::toggle("settings_panel")
    })

    # Handle tab clicks
    observeEvent(input$active_tab, {
      active_tab(input$active_tab)
      # Update tab styling via JS
      shinyjs::runjs(sprintf("
        document.querySelectorAll('.circadian-tab').forEach(function(t) {
          t.classList.remove('active');
        });
        document.getElementById('%s').classList.add('active');
      ", ns(paste0("tab_", input$active_tab))))
    })

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

    # Run Analysis
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
      req(input$metric)
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
              epoch_length = f$epoch_length,
              use_cpp = TRUE
            )
          }, error = function(e) {
            showNotification(paste0("Circadian analysis incomplete for ", f$name), type = "error")
            return(NULL)
          })

          if (is.null(res)) next

          # Run multi-component cosinor analysis (24h + 12h harmonics)
          cosinor_ext <- tryCatch({
            canhrActi::cosinor.extended(
              counts = activity,
              timestamps = data$timestamp,
              harmonics = c(24, 12),
              wear_time = wear_time
            )
          }, error = function(e) {
            showNotification(
              paste0("Cosinor analysis incomplete for ", f$name, ": ", e$message),
              type = "warning",
              duration = 5
            )
            NULL
          })

          # Extract 12h component data
          h12_amplitude <- NA_real_
          h12_power <- NA_real_
          if (!is.null(cosinor_ext) && !is.null(cosinor_ext$components)) {
            h12_row <- cosinor_ext$components[cosinor_ext$components$period == 12, ]
            if (nrow(h12_row) > 0) {
              h12_amplitude <- h12_row$amplitude[1]
              h12_power <- h12_row$relative_power[1]
            }
          }

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
            phi = res$phi,
            coverage_percent = res$coverage_percent,
            mesor = if (!is.null(cosinor_ext)) cosinor_ext$mesor else NA_real_,
            amplitude = if (!is.null(cosinor_ext)) cosinor_ext$amplitude else NA_real_,
            acrophase = if (!is.null(cosinor_ext)) cosinor_ext$acrophase else NA_real_,
            acrophase_time = if (!is.null(cosinor_ext)) cosinor_ext$acrophase_time else NA_character_,
            r_squared = if (!is.null(cosinor_ext)) cosinor_ext$r_squared else NA_real_,
            pattern_type = if (!is.null(cosinor_ext)) cosinor_ext$pattern_type else NA_character_,
            is_bimodal = if (!is.null(cosinor_ext)) cosinor_ext$is_bimodal else NA,
            h12_amplitude = h12_amplitude,
            h12_power = h12_power,
            r_squared_improvement = if (!is.null(cosinor_ext)) cosinor_ext$r_squared_improvement else NA_real_,
            r_squared_single = if (!is.null(cosinor_ext)) cosinor_ext$r_squared_single else NA_real_,
            hourly_profile = res$hourly_profile
          )
        }
      })

      results(all_results)
      shared$results$circadian <- all_results

      showNotification(paste("Rhythm patterns analyzed for", length(all_results), "files"), type = "message")
    })

    output$analysis_status_badge <- renderUI({
      n <- length(results())
      if (n > 0) {
        status_badge(paste(n, "analyzed"), "success")
      } else {
        status_badge("Not analyzed", "pending")
      }
    })

    # Current view data
    current_data <- reactive({
      res <- results()
      req(length(res) > 0)

      sel <- input$file_select

      if (sel == "all" || sel == "none") {
        pattern_counts <- table(sapply(res, function(r) r$pattern_type))
        dominant_pattern <- if (length(pattern_counts) > 0) names(which.max(pattern_counts)) else NA_character_

        list(
          mode = "all",
          results = res,
          L5 = mean(sapply(res, function(r) r$L5), na.rm = TRUE),
          M10 = mean(sapply(res, function(r) r$M10), na.rm = TRUE),
          RA = mean(sapply(res, function(r) r$RA), na.rm = TRUE),
          IS = mean(sapply(res, function(r) r$IS), na.rm = TRUE),
          IV = mean(sapply(res, function(r) r$IV), na.rm = TRUE),
          phi = mean(sapply(res, function(r) r$phi), na.rm = TRUE),
          coverage_percent = mean(sapply(res, function(r) r$coverage_percent), na.rm = TRUE),
          mesor = mean(sapply(res, function(r) r$mesor), na.rm = TRUE),
          amplitude = mean(sapply(res, function(r) r$amplitude), na.rm = TRUE),
          acrophase = {
            # Use circular mean for acrophase (time is circular 0-24h)
            acro_vals <- sapply(res, function(r) r$acrophase)
            acro_vals <- acro_vals[!is.na(acro_vals)]
            if (length(acro_vals) == 0) NA_real_ else {
              radians <- acro_vals * 2 * pi / 24
              mean_sin <- mean(sin(radians))
              mean_cos <- mean(cos(radians))
              ((atan2(mean_sin, mean_cos) * 24 / (2 * pi)) + 24) %% 24
            }
          },
          r_squared = mean(sapply(res, function(r) r$r_squared), na.rm = TRUE),
          pattern_type = dominant_pattern,
          h12_amplitude = mean(sapply(res, function(r) r$h12_amplitude), na.rm = TRUE),
          h12_power = mean(sapply(res, function(r) r$h12_power), na.rm = TRUE),
          r_squared_improvement = mean(sapply(res, function(r) r$r_squared_improvement), na.rm = TRUE),
          is_bimodal = any(sapply(res, function(r) isTRUE(r$is_bimodal)))
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
          phi = r$phi,
          coverage_percent = r$coverage_percent,
          mesor = r$mesor,
          amplitude = r$amplitude,
          acrophase = r$acrophase,
          acrophase_time = r$acrophase_time,
          r_squared = r$r_squared,
          pattern_type = r$pattern_type,
          h12_amplitude = r$h12_amplitude,
          h12_power = r$h12_power,
          r_squared_improvement = r$r_squared_improvement,
          is_bimodal = r$is_bimodal
        )
      } else {
        NULL
      }
    })

    # Core metrics strip
    output$core_metrics_panel <- renderUI({
      cd <- current_data()

      # Helper to create metric card
      metric_item <- function(value, name, detail = NULL) {
        div(class = "metric-card metric-card--inline",
          div(class = "metric-value", value),
          div(class = "metric-label", name),
          if (!is.null(detail)) div(class = "metric-sublabel", detail)
        )
      }

      # Format values
      l5_val <- if (is.null(cd) || is.na(cd$L5)) "--" else format(round(cd$L5), big.mark = ",")
      m10_val <- if (is.null(cd) || is.na(cd$M10)) "--" else format(round(cd$M10), big.mark = ",")
      ra_val <- if (is.null(cd) || is.na(cd$RA)) "--" else sprintf("%.3f", cd$RA)
      is_val <- if (is.null(cd) || is.na(cd$IS)) "--" else sprintf("%.2f", cd$IS)

      # Sublabels
      l5_detail <- if (!is.null(cd) && !is.null(cd$L5_start) && !is.na(cd$L5_start)) paste("Start:", cd$L5_start) else NULL
      m10_detail <- if (!is.null(cd) && !is.null(cd$M10_start) && !is.na(cd$M10_start)) paste("Start:", cd$M10_start) else NULL

      div(class = "metrics-strip metrics-strip--transparent",
        metric_item(l5_val, "L5", l5_detail),
        metric_item(m10_val, "M10", m10_detail),
        metric_item(ra_val, "RA", "Relative Amplitude"),
        metric_item(is_val, "IS", "Stability")
      )
    })

    # Pattern classification card
    output$pattern_card <- renderUI({
      cd <- current_data()

      if (is.null(cd) || is.null(cd$pattern_type) || is.na(cd$pattern_type)) {
        return(div(class = "circadian-pattern-empty",
          "Run Analysis to see pattern classification"
        ))
      }

      # Get interpretation based on pattern
      interpretation <- switch(cd$pattern_type,
        "Strong 24h" = "Excellent circadian rhythm with dominant 24-hour cycle",
        "Moderate 24h" = "Good circadian rhythm with clear 24-hour pattern",
        "Bimodal" = "Two distinct activity peaks, often morning and evening",
        "Mixed" = "Complex activity pattern with multiple components",
        "Complex" = "Multi-component rhythm with 8-hour ultradian pattern",
        "Irregular" = "Fragmented or weak circadian rhythm",
        "Normal rhythm pattern"  # Default fallback
      )

      div(class = "circadian-pattern-display",
        div(class = "circadian-pattern-label", "Rhythm Pattern"),
        div(class = "circadian-pattern-value", cd$pattern_type),
        div(class = "circadian-pattern-interpretation", interpretation)
      )
    })

    # Cosinor parameters card
    output$cosinor_panel <- renderUI({
      cd <- current_data()

      if (is.null(cd) || is.na(cd$mesor)) {
        return(div(class = "circadian-cosinor-empty",
          "Cosinor analysis not available"
        ))
      }

      # Format values
      mesor_val <- format(round(cd$mesor), big.mark = ",")
      amp_val <- format(round(cd$amplitude), big.mark = ",")
      acro_val <- if (!is.null(cd$acrophase_time) && !is.na(cd$acrophase_time)) {
        cd$acrophase_time
      } else if (!is.na(cd$acrophase)) {
        sprintf("%.1fh", cd$acrophase)
      } else {
        "--"
      }
      r2_val <- if (!is.na(cd$r_squared)) sprintf("%.1f%%", cd$r_squared * 100) else "--"
      r2_pct <- if (!is.na(cd$r_squared)) cd$r_squared * 100 else 0

      tagList(
        div(class = "circadian-cosinor-grid",
          div(class = "circadian-cosinor-item",
            div(class = "circadian-cosinor-value", mesor_val),
            div(class = "circadian-cosinor-label", "MESOR")
          ),
          div(class = "circadian-cosinor-item",
            div(class = "circadian-cosinor-value", amp_val),
            div(class = "circadian-cosinor-label", "Amplitude")
          ),
          div(class = "circadian-cosinor-item",
            div(class = "circadian-cosinor-value", acro_val),
            div(class = "circadian-cosinor-label", "Acrophase")
          ),
          div(class = "circadian-cosinor-item",
            div(class = "circadian-cosinor-value", r2_val),
            div(class = "circadian-cosinor-label", "Model Fit")
          )
        ),
        div(class = "circadian-cosinor-fit",
          span(class = "circadian-fit-label", "R-squared"),
          div(class = "circadian-fit-bar",
            div(class = "circadian-fit-fill", style = sprintf("width: %s%%;", r2_pct))
          ),
          span(class = "circadian-fit-value", r2_val)
        )
      )
    })

    # Main chart
    output$main_chart <- renderPlot({
      cd <- current_data()

      # User-friendly empty state messaging
      validate(
        need(!is.null(cd),
             "\n\nNo Circadian Data\n\nRun Analysis to see patterns.\nSelect files and configure analysis parameters above."
        )
      )

      tab <- active_tab()

      if (tab == "profile") {
        # 24-hour activity profile
        if (cd$mode == "single") {
          hourly <- cd$result$hourly_profile
          req(hourly)

          ggplot(hourly, aes(x = hour, y = mean_counts)) +
            geom_ribbon(aes(ymin = pmax(0, mean_counts - sd_counts),
                            ymax = mean_counts + sd_counts),
                        fill = "#236192", alpha = 0.15) +
            geom_line(color = "#236192", linewidth = 1.2) +
            geom_point(color = "#236192", size = 2) +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0.02, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
            labs(x = NULL, y = "Activity (counts/min)") +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.title = element_text(color = "#1a202c")
            )
        } else {
          # Multi-file average
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
                      color = "#94a3b8", alpha = 0.4, linewidth = 0.4) +
            geom_line(data = avg_hourly, aes(x = hour, y = mean_counts),
                      color = "#236192", linewidth = 1.5) +
            geom_point(data = avg_hourly, aes(x = hour, y = mean_counts),
                       color = "#236192", size = 2.5) +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0.02, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
            labs(x = NULL, y = "Activity (counts/min)",
                 subtitle = sprintf("Average of %d subjects", length(cd$results))) +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.title = element_text(color = "#1a202c"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        }

      } else if (tab == "actogram") {
        # Actogram - double-plotted style
        if (cd$mode == "single") {
          hourly <- cd$result$hourly_profile
          req(hourly)

          # Create a simple double-plot actogram
          hourly_ext <- rbind(
            transform(hourly, hour = hour),
            transform(hourly, hour = hour + 24)
          )

          ggplot(hourly_ext, aes(x = hour, y = mean_counts)) +
            geom_area(fill = "#236192", alpha = 0.6) +
            geom_line(color = "#1a4a6f", linewidth = 0.8) +
            scale_x_continuous(breaks = seq(0, 48, 6),
                              labels = rep(sprintf("%02d:00", seq(0, 23, 6)), 2)[1:9],
                              expand = c(0, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
            labs(x = NULL, y = "Activity",
                 subtitle = "Double-plotted 48-hour view") +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major.x = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        } else {
          # Multi-file: show individual profiles stacked
          all_hourly <- data.frame()
          for (i in seq_along(cd$results)) {
            r <- cd$results[[i]]
            if (!is.null(r$hourly_profile)) {
              h <- r$hourly_profile
              h$subject <- r$subject_id
              h$row <- i
              all_hourly <- rbind(all_hourly, h)
            }
          }
          req(nrow(all_hourly) > 0)

          ggplot(all_hourly, aes(x = hour, y = row, fill = mean_counts)) +
            geom_tile() +
            scale_fill_gradient(low = "white", high = "#236192", name = "Activity") +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0, 0)) +
            scale_y_continuous(breaks = unique(all_hourly$row),
                              labels = unique(all_hourly$subject),
                              expand = c(0, 0)) +
            labs(x = NULL, y = NULL,
                 subtitle = "Subjects stacked vertically") +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.text.y = element_text(size = 10),
              legend.position = "right",
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        }

      } else if (tab == "cosinor") {
        # Cosinor fit visualization
        if (cd$mode == "single") {
          hourly <- cd$result$hourly_profile

          # User-friendly validation instead of silent req() failure
          validate(
            need(!is.null(hourly), "Hourly profile data not available"),
            need(!is.na(cd$mesor), "Cosinor analysis failed - MESOR could not be calculated.\nThis may indicate irregular or insufficient activity data."),
            need(!is.na(cd$amplitude), "Cosinor analysis failed - amplitude could not be calculated.\nCheck that the data has sufficient variability."),
            need(!is.na(cd$acrophase), "Cosinor analysis failed - acrophase could not be calculated.")
          )

          # Generate cosinor fit curve
          hours_fine <- seq(0, 24, by = 0.1)
          acro_rad <- (cd$acrophase / 24) * 2 * pi
          fitted <- cd$mesor + cd$amplitude * cos(2 * pi * hours_fine / 24 - acro_rad)
          fit_df <- data.frame(hour = hours_fine, fitted = fitted)

          ggplot() +
            geom_hline(yintercept = cd$mesor, linetype = "dashed", color = "#FFCD00", linewidth = 0.8) +
            geom_line(data = fit_df, aes(x = hour, y = fitted),
                     color = "#236192", linewidth = 1.5) +
            geom_point(data = hourly, aes(x = hour, y = mean_counts),
                      color = "#1a202c", fill = "#236192", shape = 21, size = 3, stroke = 0.8) +
            annotate("text", x = 23, y = cd$mesor, label = "MESOR",
                    hjust = 1, vjust = -0.5, color = "#FFCD00", fontface = "bold", size = 3.5) +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0.02, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.05, 0)) +
            labs(x = NULL, y = "Activity (counts/min)",
                 subtitle = sprintf("R-squared = %.3f | Acrophase = %s",
                                   cd$r_squared, cd$acrophase_time %||% sprintf("%.1fh", cd$acrophase))) +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.title = element_text(color = "#1a202c"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        } else {
          # Average cosinor with data points
          # User-friendly validation instead of silent req() failure
          validate(
            need(!is.na(cd$mesor), "Cosinor analysis failed - MESOR could not be calculated.\nInsufficient data across files for cosinor modeling."),
            need(!is.na(cd$amplitude), "Cosinor analysis failed - amplitude could not be calculated.\nData may lack sufficient circadian variability."),
            need(!is.na(cd$acrophase), "Cosinor analysis failed - acrophase could not be calculated.")
          )

          # Collect hourly profiles from all results
          all_hourly <- data.frame()
          for (r in cd$results) {
            if (!is.null(r$hourly_profile)) {
              h <- r$hourly_profile
              h$subject <- r$subject_id
              all_hourly <- rbind(all_hourly, h)
            }
          }

          # Calculate average hourly profile
          avg_hourly <- if (nrow(all_hourly) > 0) {
            aggregate(mean_counts ~ hour, all_hourly, mean, na.rm = TRUE)
          } else {
            NULL
          }

          hours_fine <- seq(0, 24, by = 0.1)
          acro_rad <- (cd$acrophase / 24) * 2 * pi
          fitted <- cd$mesor + cd$amplitude * cos(2 * pi * hours_fine / 24 - acro_rad)
          fit_df <- data.frame(hour = hours_fine, fitted = fitted)

          p <- ggplot() +
            geom_hline(yintercept = cd$mesor, linetype = "dashed", color = "#FFCD00", linewidth = 0.8) +
            geom_line(data = fit_df, aes(x = hour, y = fitted),
                     color = "#236192", linewidth = 1.5)

          # Add data points if available
          if (!is.null(avg_hourly) && nrow(avg_hourly) > 0) {
            p <- p + geom_point(data = avg_hourly, aes(x = hour, y = mean_counts),
                               color = "#1a202c", fill = "#236192", shape = 21, size = 3, stroke = 0.8)
          }

          p + annotate("text", x = 23, y = cd$mesor, label = "MESOR",
                    hjust = 1, vjust = -0.5, color = "#FFCD00", fontface = "bold", size = 3.5) +
            scale_x_continuous(breaks = seq(0, 23, 3),
                              labels = sprintf("%02d:00", seq(0, 23, 3)),
                              expand = c(0.02, 0)) +
            scale_y_continuous(labels = scales::comma, expand = c(0.05, 0)) +
            labs(x = NULL, y = "Activity (counts/min)",
                 subtitle = sprintf("Average cosinor fit (n=%d) | R-squared = %.3f",
                                   length(cd$results), cd$r_squared)) +
            canhrActi::theme_canhrActi() +
            theme(
              plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.4),
              panel.grid.minor = element_blank(),
              axis.text = element_text(color = "#64748b"),
              axis.title = element_text(color = "#1a202c"),
              plot.subtitle = element_text(color = "#64748b", size = 11)
            )
        }
      }
    })

    # Summary table
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run Analysis to see results"),
                            rownames = FALSE, options = list(dom = 't')))
      }

      safe_round <- function(x, digits = 0) {
        if (is.null(x) || length(x) == 0 || is.na(x)) NA_real_ else round(x, digits)
      }
      safe_str <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x)) NA_character_ else as.character(x)
      }

      df <- data.frame(
        Subject = sapply(res, function(r) safe_str(r$subject_id)),
        L5 = sapply(res, function(r) safe_round(r$L5)),
        M10 = sapply(res, function(r) safe_round(r$M10)),
        RA = sapply(res, function(r) safe_round(r$RA, 3)),
        IS = sapply(res, function(r) safe_round(r$IS, 3)),
        IV = sapply(res, function(r) safe_round(r$IV, 3)),
        MESOR = sapply(res, function(r) safe_round(r$mesor)),
        Amplitude = sapply(res, function(r) safe_round(r$amplitude)),
        Acrophase = sapply(res, function(r) safe_str(r$acrophase_time)),
        R2 = sapply(res, function(r) safe_round(r$r_squared, 3)),
        Pattern = sapply(res, function(r) safe_str(r$pattern_type)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE
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
          phi = sapply(res, function(r) r$phi),
          mesor = sapply(res, function(r) r$mesor),
          amplitude = sapply(res, function(r) r$amplitude),
          acrophase = sapply(res, function(r) r$acrophase),
          acrophase_time = sapply(res, function(r) r$acrophase_time),
          r_squared = sapply(res, function(r) r$r_squared),
          pattern_type = sapply(res, function(r) r$pattern_type),
          is_bimodal = sapply(res, function(r) r$is_bimodal),
          h12_amplitude = sapply(res, function(r) r$h12_amplitude),
          h12_power = sapply(res, function(r) r$h12_power),
          r_squared_single = sapply(res, function(r) r$r_squared_single),
          r_squared_improvement = sapply(res, function(r) r$r_squared_improvement),
          coverage_percent = sapply(res, function(r) r$coverage_percent),
          stringsAsFactors = FALSE
        )

        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
