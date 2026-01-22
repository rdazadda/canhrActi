# Advanced Graphing Module - Redesigned
# Gallery-style chart selection with full-screen focus
# Center for Alaska Native Health Research (CANHR)

mod_graphing_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Page Header
    page_header(
      icon_name = "chart-line",
      title = "Visualization Studio",
      subtitle = "Publication-ready charts and multi-panel comparisons",
      status_output_id = ns("data_status")
    ),

    # Quick Charts Bar with Generate button on right
    div(style = "display: flex; align-items: center; gap: 8px; padding: 12px 16px; background: #f8fafc; border-bottom: 1px solid #e2e8f0;",
      # Quick charts on left
      span(style = "font-weight: 500; color: #64748b; margin-right: 4px;", "Quick Charts:"),
      actionButton(ns("quick_timeline"), "Activity Timeline",
                   class = "quick-chart-btn"),
      actionButton(ns("quick_hypnogram"), "Sleep Hypnogram",
                   class = "quick-chart-btn"),
      actionButton(ns("quick_circadian"), "Circadian Profile",
                   class = "quick-chart-btn"),
      actionButton(ns("quick_heatmap"), "Activity Heatmap",
                   class = "quick-chart-btn"),
      actionButton(ns("quick_intensity"), "Intensity Breakdown",
                   class = "quick-chart-btn"),

      # Spacer to push Generate/Reset to right
      div(style = "flex: 1;"),

      # Generate and Reset on right
      actionButton(ns("generate_btn"), span(icon("play"), "Generate Chart"),
                   class = "btn-primary"),
      actionButton(ns("clear_chart"), span(icon("redo"), "Reset"),
                   class = "btn-default")
    ),

    fluidRow(
      # Main Chart Area (80%+)
      column(9, id = ns("main_col"),
        div(class = "chart-fullscreen-container",

          # Toolbar
          div(class = "chart-toolbar",
            div(class = "chart-title-area",
              h4(textOutput(ns("chart_title"), inline = TRUE)),
              uiOutput(ns("chart_type_badge"))
            ),
            div(class = "chart-actions",
              # Fullscreen button
              actionButton(ns("fullscreen_btn"), icon("expand"),
                           class = "btn btn-default btn-sm", title = "Toggle Fullscreen"),
              # Export Dropdown
              div(class = "export-dropdown-wrapper", style = "position: relative; display: inline-block;",
                actionButton(ns("export_btn"), span(icon("download"), "Export"),
                             class = "btn btn-primary btn-sm"),
                uiOutput(ns("export_dropdown_menu"))
              ),
              actionButton(ns("toggle_sidebar"), icon("sliders-h"),
                           class = "btn btn-default btn-sm", title = "Toggle Options")
            )
          ),

          # Chart Display Area
          div(class = "chart-display-area",
            uiOutput(ns("chart_output"))
          )
        )
      ),

      # Collapsible Sidebar (Controls)
      column(3, id = ns("sidebar_col"), class = "graphing-sidebar-col",
        div(class = "customize-sidebar",

          # File Selection Section
          div(class = "sidebar-section",
            div(class = "sidebar-section-header", id = ns("section_file_header"),
              span(icon("file"), " Data Source"),
              icon("chevron-down", class = "toggle-icon")
            ),
            div(class = "sidebar-section-content", id = ns("section_file_content"),
              selectInput(ns("selected_file"), NULL, choices = NULL, width = "100%"),
              uiOutput(ns("file_info_compact"))
            )
          ),

          # Chart Selection - Simple Dropdown
          div(class = "sidebar-section",
            div(class = "sidebar-section-header", id = ns("section_charts_header"),
              span(icon("chart-bar"), " Chart Type"),
              icon("chevron-down", class = "toggle-icon")
            ),
            div(class = "sidebar-section-content", id = ns("section_charts_content"),
              selectInput(ns("chart_select"), NULL,
                choices = list(
                  "Activity" = c(
                    "Daily Timeline" = "daily_timeline",
                    "Heatmap" = "heatmap",
                    "Intensity Pie" = "intensity_pie",
                    "Intensity Area" = "intensity_area",
                    "24h Clock" = "activity_clock"
                  ),
                  "Sleep" = c(
                    "Hypnogram" = "hypnogram",
                    "Sleep Quality" = "sleep_quality"
                  ),
                  "Circadian" = c(
                    "Polar Chart" = "polar",
                    "IS/IV Analysis" = "is_iv"
                  ),
                  "Summary" = c(
                    "Daily Bars" = "daily_bars",
                    "Weekend vs Weekday" = "weekend_weekday",
                    "Day Comparison" = "day_comparison"
                  )
                ),
                selected = "daily_timeline",
                width = "100%"
              )
            )
          ),

          # Chart Options Section
          div(class = "sidebar-section",
            div(class = "sidebar-section-header", id = ns("section_options_header"),
              span(icon("sliders-h"), " Chart Options"),
              icon("chevron-down", class = "toggle-icon")
            ),
            div(class = "sidebar-section-content", id = ns("section_options_content"),
              uiOutput(ns("chart_options"))
            )
          ),

          # Size & Export Section
          div(class = "sidebar-section",
            div(class = "sidebar-section-header", id = ns("section_size_header"),
              span(icon("expand-arrows-alt"), " Size"),
              icon("chevron-down", class = "toggle-icon")
            ),
            div(class = "sidebar-section-content", id = ns("section_size_content"),
              sliderInput(ns("plot_width"), "Width", value = 1000, min = 600, max = 2000, step = 50, post = "px"),
              sliderInput(ns("plot_height"), "Height", value = 800, min = 400, max = 2000, step = 50, post = "px")
            )
          ),

        )
      )
    ),

    # JavaScript for interactivity
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        // Sidebar section toggles
        $('.sidebar-section-header').click(function() {
          var content = $(this).next('.sidebar-section-content');
          $(this).toggleClass('collapsed');
          content.toggleClass('collapsed');
        });

        // Toggle sidebar visibility
        $('#%s').click(function() {
          var sidebar = $('#%s');
          var main = $('#%s');
          if (sidebar.is(':visible')) {
            sidebar.hide();
            main.removeClass('col-sm-9').addClass('col-sm-12');
          } else {
            sidebar.show();
            main.removeClass('col-sm-12').addClass('col-sm-9');
          }
        });

        // Fullscreen toggle
        $('#%s').click(function() {
          var container = $('.chart-fullscreen-container')[0];
          if (!document.fullscreenElement) {
            if (container.requestFullscreen) {
              container.requestFullscreen();
            } else if (container.webkitRequestFullscreen) {
              container.webkitRequestFullscreen();
            } else if (container.msRequestFullscreen) {
              container.msRequestFullscreen();
            }
            $(this).find('i').removeClass('fa-expand').addClass('fa-compress');
          } else {
            if (document.exitFullscreen) {
              document.exitFullscreen();
            } else if (document.webkitExitFullscreen) {
              document.webkitExitFullscreen();
            } else if (document.msExitFullscreen) {
              document.msExitFullscreen();
            }
            $(this).find('i').removeClass('fa-compress').addClass('fa-expand');
          }
        });

        // Update icon when exiting fullscreen via Escape key
        $(document).on('fullscreenchange webkitfullscreenchange', function() {
          if (!document.fullscreenElement && !document.webkitFullscreenElement) {
            $('#%s').find('i').removeClass('fa-compress').addClass('fa-expand');
          }
        });
      });
    ", ns("toggle_sidebar"), ns("sidebar_col"), ns("main_col"), ns("fullscreen_btn"), ns("fullscreen_btn"))))
  )
}

mod_graphing_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    current_plot <- reactiveVal(NULL)

    # Chart name lookup
    chart_names <- c(
      daily_timeline = "Daily Timeline", heatmap = "Heatmap",
      intensity_pie = "Intensity Pie", intensity_area = "Intensity Area",
      activity_clock = "24h Clock", hypnogram = "Hypnogram",
      sleep_quality = "Sleep Quality", polar = "Polar Chart",
      is_iv = "IS/IV Analysis", daily_bars = "Daily Bars",
      weekend_weekday = "Weekend vs Weekday", day_comparison = "Day Comparison"
    )

    # Chart category lookup
    chart_categories <- c(
      daily_timeline = "Activity", heatmap = "Activity",
      intensity_pie = "Activity", intensity_area = "Activity",
      activity_clock = "Activity", hypnogram = "Sleep",
      sleep_quality = "Sleep", polar = "Circadian",
      is_iv = "Circadian", daily_bars = "Summary",
      weekend_weekday = "Summary", day_comparison = "Summary"
    )

    # Data status indicator
    output$data_status <- renderUI({
      if (shared$file_count > 0) {
        status_badge(paste(shared$file_count, "file(s) loaded"), "success")
      } else {
        status_badge("No data loaded", "caution")
      }
    })

    # Files count for metrics strip
    output$files_count <- renderText({
      as.character(shared$file_count)
    })

    # Clear chart handler
    observeEvent(input$clear_chart, {
      current_plot(NULL)
      showNotification("Chart cleared", type = "message", duration = 2)
    })

    # Charts that support "All Participants" aggregation
    all_supported_charts <- c("intensity_pie", "polar", "daily_bars", "weekend_weekday")

    # Update file selection with "All Participants" option
    observe({
      req(shared$file_count > 0)
      file_choices <- setNames(names(shared$files),
                          sapply(shared$files, function(f) f$subject_info$id %||% f$name))
      # Add "All Participants" at the beginning
      choices <- c("All Participants" = "all", file_choices)
      updateSelectInput(session, "selected_file", choices = choices)
    })

    # File info compact display
    output$file_info_compact <- renderUI({
      req(input$selected_file)
      sel <- input$selected_file

      if (sel == "all") {
        # Show aggregate info for all participants
        total_days <- sum(sapply(shared$files, function(f) {
          if ("timestamp" %in% names(f$data)) {
            length(unique(as.Date(f$data$timestamp)))
          } else 0
        }))

        div(class = "file-info-box",
          tags$table(class = "info-table text-sm",
            tags$tr(tags$td("Participants"), tags$td(shared$file_count)),
            tags$tr(tags$td("Total Days"), tags$td(total_days)),
            tags$tr(tags$td("Note"), tags$td(tags$small("Aggregated view")))
          )
        )
      } else {
        req(shared$files[[sel]])
        f <- shared$files[[sel]]

        div(class = "file-info-box",
          tags$table(class = "info-table text-sm",
            tags$tr(tags$td("Subject"), tags$td(f$subject_info$id %||% "N/A")),
            tags$tr(tags$td("Epoch"), tags$td(paste(f$epoch_length, "sec"))),
            tags$tr(tags$td("Days"), tags$td({
              if ("timestamp" %in% names(f$data)) {
                length(unique(as.Date(f$data$timestamp)))
              } else "N/A"
            }))
          )
        )
      }
    })

    # Quick chart buttons
    observeEvent(input$quick_timeline, {
      updateSelectInput(session, "chart_select", selected = "daily_timeline")
      shinyjs::click(ns("generate_btn"))
    })

    observeEvent(input$quick_hypnogram, {
      updateSelectInput(session, "chart_select", selected = "hypnogram")
      shinyjs::click(ns("generate_btn"))
    })

    observeEvent(input$quick_circadian, {
      updateSelectInput(session, "chart_select", selected = "polar")
      shinyjs::click(ns("generate_btn"))
    })

    observeEvent(input$quick_heatmap, {
      updateSelectInput(session, "chart_select", selected = "heatmap")
      shinyjs::click(ns("generate_btn"))
    })

    observeEvent(input$quick_intensity, {
      updateSelectInput(session, "chart_select", selected = "intensity_pie")
      shinyjs::click(ns("generate_btn"))
    })

    # Chart title
    output$chart_title <- renderText({
      chart <- input$chart_select %||% "daily_timeline"
      chart_names[chart] %||% "Select a Chart"
    })

    # Chart type badge
    output$chart_type_badge <- renderUI({
      chart <- input$chart_select %||% "daily_timeline"
      category <- chart_categories[chart] %||% "Activity"
      span(class = "chart-type-badge", category)
    })

    # Chart-specific options (no title or color palette)
    output$chart_options <- renderUI({
      chart <- input$chart_select %||% "daily_timeline"

      switch(chart,
        "daily_timeline" = tagList(
          checkboxGroupInput(ns("show_axes"), "Show Axes",
                             choices = c("Axis 1" = "axis1", "Steps" = "steps", "VM" = "vm"),
                             selected = "axis1", inline = TRUE),
          checkboxInput(ns("show_cutpoints"), "Show Cut-Point Lines", value = TRUE)
        ),

        "heatmap" = tagList(
          selectInput(ns("heatmap_metric"), "Metric",
                      choices = c("Axis 1" = "axis1", "Steps" = "steps", "VM" = "vm")),
          checkboxInput(ns("heatmap_weekends"), "Highlight Weekends", value = TRUE),
          checkboxInput(ns("heatmap_normalize"), "Normalize by Day", value = FALSE)
        ),

        "intensity_pie" = tagList(
          selectInput(ns("pie_cutpoints"), "Cut-Points Algorithm",
                      choices = c("Freedson (1998)" = "freedson", "Evenson (2008)" = "evenson",
                                  "Troiano (2008)" = "troiano", "CANHR (2025)" = "canhr")),
          checkboxInput(ns("pie_donut"), "Donut Style", value = TRUE),
          checkboxInput(ns("pie_labels"), "Show Labels", value = TRUE)
        ),

        "hypnogram" = tagList(
          checkboxInput(ns("hyp_activity"), "Show Activity Overlay", value = TRUE),
          checkboxInput(ns("hyp_metrics"), "Show Sleep Metrics", value = TRUE),
          checkboxInput(ns("hyp_awakenings"), "Mark Awakenings", value = TRUE)
        ),

        "polar" = tagList(
          checkboxInput(ns("polar_ribbon"), "Show Confidence Ribbon", value = TRUE),
          checkboxInput(ns("polar_daytype"), "Separate Weekend/Weekday", value = FALSE),
          checkboxInput(ns("polar_L5M10"), "Show L5/M10 Arcs", value = TRUE)
        ),

        "day_comparison" = tagList(
          uiOutput(ns("day_selector"))
        ),

        NULL
      )
    })

    # Day selector for comparison chart
    output$day_selector <- renderUI({
      req(input$selected_file, shared$files[[input$selected_file]])
      f <- shared$files[[input$selected_file]]
      data <- f$data

      if ("timestamp" %in% names(data)) {
        dates <- unique(as.Date(data$timestamp))
        date_choices <- setNames(as.character(dates), format(dates, "%b %d (%a)"))
        checkboxGroupInput(ns("compare_days"), "Select Days",
                           choices = date_choices,
                           selected = date_choices[1:min(3, length(date_choices))])
      }
    })

    # Generate plot
    observeEvent(input$generate_btn, {
      req(input$selected_file)
      sel <- input$selected_file
      chart <- input$chart_select %||% "daily_timeline"

      # Handle "All Participants" selection
      if (sel == "all") {
        # Check if chart supports "All Participants"
        if (!(chart %in% all_supported_charts)) {
          showNotification(
            paste0("'", chart_names[chart], "' doesn't support All Participants view. Using first participant."),
            type = "warning", duration = 4
          )
          # Fall back to first participant
          sel <- names(shared$files)[1]
        }
      }

      # For individual participant or fallback
      if (sel != "all") {
        req(shared$files[[sel]])
        f <- shared$files[[sel]]
        data <- f$data
        epoch_len <- f$epoch_length
        subject_id <- f$subject_info$id %||% f$name
        chart_title <- paste(chart_names[chart] %||% "Chart", "-", subject_id)
      } else {
        # All Participants - aggregate title
        chart_title <- paste(chart_names[chart] %||% "Chart", "- All Participants")
      }

      p <- tryCatch({
        # Handle "All Participants" for supported charts
        if (sel == "all") {
          switch(chart,
            "intensity_pie" = {
              # Aggregate intensity data across all participants
              total_sedentary <- 0
              total_light <- 0
              total_moderate <- 0
              total_vigorous <- 0

              activity_results <- shared$results$activity
              if (!is.null(activity_results) && length(activity_results) > 0) {
                for (r in activity_results) {
                  if (!is.null(r$daily)) {
                    total_sedentary <- total_sedentary + sum(r$daily$sedentary_hrs * 60, na.rm = TRUE)
                    total_light <- total_light + sum(r$daily$light_hrs * 60, na.rm = TRUE)
                    total_moderate <- total_moderate + sum(r$daily$moderate_hrs * 60, na.rm = TRUE)
                    vig_hrs <- if ("vigorous_hrs" %in% names(r$daily)) r$daily$vigorous_hrs else 0
                    vvig_hrs <- if ("very_vigorous_hrs" %in% names(r$daily)) r$daily$very_vigorous_hrs else 0
                    total_vigorous <- total_vigorous + sum((vig_hrs + vvig_hrs) * 60, na.rm = TRUE)
                  }
                }
              }

              if (total_sedentary + total_light + total_moderate + total_vigorous == 0) {
                showNotification("No activity data. Run Activity Analysis first.", type = "warning")
                return(NULL)
              }

              intensity_minutes <- data.frame(
                intensity = factor(c("Sedentary", "Light", "Moderate", "Vigorous"),
                                   levels = c("Sedentary", "Light", "Moderate", "Vigorous")),
                minutes = c(total_sedentary, total_light, total_moderate, total_vigorous)
              )
              canhrActi::plot_intensity_pie_from_summary(
                intensity_summary = intensity_minutes,
                cutpoints = input$pie_cutpoints %||% "freedson",
                show_labels = input$pie_labels %||% TRUE,
                title = chart_title
              )
            },

            "polar" = {
              # Combine all participant data for circadian polar
              all_data <- do.call(rbind, lapply(shared$files, function(f) {
                d <- f$data
                if ("timestamp" %in% names(d) && "axis1" %in% names(d)) {
                  data.frame(timestamp = d$timestamp, axis1 = d$axis1)
                } else NULL
              }))

              if (is.null(all_data) || nrow(all_data) == 0) {
                showNotification("No valid data for polar chart.", type = "warning")
                return(NULL)
              }

              if (!inherits(all_data$timestamp, "POSIXct")) {
                all_data$timestamp <- as.POSIXct(all_data$timestamp)
              }

              canhrActi::plot_circadian_polar(
                data = all_data,
                show_ribbon = input$polar_ribbon %||% TRUE,
                by_day_type = input$polar_daytype %||% FALSE,
                show_L5M10 = FALSE,  # No L5/M10 for aggregated view
                title = chart_title
              )
            },

            "daily_bars" = {
              # Combine daily summaries across all participants
              all_daily <- do.call(rbind, lapply(names(shared$results$activity), function(fid) {
                r <- shared$results$activity[[fid]]
                if (!is.null(r$daily)) {
                  d <- r$daily
                  d$participant <- fid
                  d
                } else NULL
              }))

              if (is.null(all_daily) || nrow(all_daily) == 0) {
                showNotification("No activity data. Run Activity Analysis first.", type = "warning")
                return(NULL)
              }

              # Create aggregated daily bars using average per day across participants
              canhrActi::plot_daily_summary_bars(
                data = NULL,
                daily_summary = all_daily,
                title = chart_title
              )
            },

            "weekend_weekday" = {
              # Combine all data for weekend/weekday comparison
              all_data <- do.call(rbind, lapply(shared$files, function(f) {
                d <- f$data
                if ("timestamp" %in% names(d) && "axis1" %in% names(d)) {
                  data.frame(timestamp = d$timestamp, axis1 = d$axis1)
                } else NULL
              }))

              if (is.null(all_data) || nrow(all_data) == 0) {
                showNotification("No valid data for weekend/weekday comparison.", type = "warning")
                return(NULL)
              }

              canhrActi::plot_weekend_weekday(
                data = all_data,
                title = chart_title
              )
            },

            {
              showNotification("Chart not supported for All Participants.", type = "warning")
              NULL
            }
          )
        } else {
          # Individual participant charts
          switch(chart,
            "daily_timeline" = {
              canhrActi::plot_daily_timeline(
                data = data,
                show_axes = input$show_axes %||% "axis1",
                show_cutpoints = input$show_cutpoints %||% TRUE,
                epoch_length = epoch_len,
                title = chart_title
              )
            },

            "heatmap" = {
              canhrActi::plot_activity_heatmap(
                data = data,
                metric = input$heatmap_metric %||% "axis1",
                normalize = input$heatmap_normalize %||% FALSE,
                show_weekends = input$heatmap_weekends %||% TRUE,
                color_palette = "viridis",
                title = chart_title
              )
            },

            "intensity_pie" = {
              activity_data <- shared$results$activity[[sel]]
              if (!is.null(activity_data) && !is.null(activity_data$sedentary_min)) {
                intensity_minutes <- data.frame(
                  intensity = factor(c("Sedentary", "Light", "Moderate", "Vigorous"),
                                     levels = c("Sedentary", "Light", "Moderate", "Vigorous")),
                  minutes = c(
                    activity_data$sedentary_min %||% 0,
                    activity_data$light_min %||% 0,
                    activity_data$moderate_min %||% 0,
                    activity_data$vigorous_min %||% 0
                  )
                )
                canhrActi::plot_intensity_pie_from_summary(
                  intensity_summary = intensity_minutes,
                  cutpoints = activity_data$parameters$cut_points %||% "freedson",
                  show_labels = input$pie_labels %||% TRUE,
                  title = chart_title
                )
              } else {
                canhrActi::plot_intensity_pie(
                  data = data,
                  cutpoints = input$pie_cutpoints %||% "freedson",
                  epoch_length = epoch_len,
                  show_labels = input$pie_labels %||% TRUE,
                  donut_style = input$pie_donut %||% TRUE,
                  title = chart_title
                )
              }
            },

            "intensity_area" = {
              canhrActi::plot_intensity_area(
                data = data,
                cutpoints = input$pie_cutpoints %||% "freedson",
                epoch_length = epoch_len,
                title = chart_title
              )
            },

            "activity_clock" = {
              canhrActi::plot_activity_clock(
                data = data,
                title = chart_title
              )
            },

          "hypnogram" = {
            sleep_data <- shared$results$sleep[[sel]]
            sleep_state <- NULL

            if (!is.null(sleep_data)) {
              if (!is.null(sleep_data$sleep_state)) {
                sleep_state <- sleep_data$sleep_state
              } else if (!is.null(sleep_data$scoring) && !is.null(sleep_data$scoring$sleep_state)) {
                sleep_state <- sleep_data$scoring$sleep_state
              } else if ("sleep_state" %in% names(data)) {
                sleep_state <- data$sleep_state
              }
            }

            if (is.null(sleep_state)) {
              showNotification("No sleep scoring available. Run Sleep Analysis first.", type = "warning")
              return(NULL)
            }

            if (is.character(sleep_state)) {
              valid_vals <- all(sleep_state %in% c("S", "W", NA))
              if (!valid_vals) {
                sleep_state <- ifelse(toupper(substr(as.character(sleep_state), 1, 1)) == "W", "W", "S")
              }
            } else if (is.numeric(sleep_state)) {
              sleep_state <- ifelse(sleep_state == 1, "W", "S")
            }

            if (length(sleep_state) == nrow(data)) {
              data$sleep_state <- sleep_state
            } else {
              showNotification("Sleep state length mismatch.", type = "warning")
              return(NULL)
            }

            canhrActi::plot_hypnogram(
              data = data,
              sleep_col = "sleep_state",
              counts_col = if (input$hyp_activity %||% TRUE) "axis1" else NULL,
              show_metrics = input$hyp_metrics %||% TRUE,
              show_activity = input$hyp_activity %||% TRUE,
              show_awakenings = input$hyp_awakenings %||% TRUE,
              title = chart_title
            )
          },

          "sleep_quality" = {
            sleep_data <- shared$results$sleep[[sel]]
            if (is.null(sleep_data) || is.null(sleep_data$periods) || nrow(sleep_data$periods) == 0) {
              showNotification("No sleep data. Run Sleep Analysis first.", type = "warning")
              return(NULL)
            }
            canhrActi::plot_sleep_quality(
              sleep_data = sleep_data$periods,
              title = chart_title
            )
          },

          "polar" = {
            if (!"timestamp" %in% names(data)) {
              showNotification("Data missing 'timestamp' column.", type = "error")
              return(NULL)
            }
            if (!"axis1" %in% names(data)) {
              showNotification("Data missing 'axis1' column.", type = "error")
              return(NULL)
            }

            if (!inherits(data$timestamp, "POSIXct")) {
              data$timestamp <- as.POSIXct(data$timestamp)
            }

            circadian_data <- shared$results$circadian[[sel]]
            tryCatch({
              canhrActi::plot_circadian_polar(
                data = data,
                show_ribbon = input$polar_ribbon %||% TRUE,
                by_day_type = input$polar_daytype %||% FALSE,
                show_L5M10 = input$polar_L5M10 %||% TRUE,
                L5_onset = if (!is.null(circadian_data)) circadian_data$L5_start else NULL,
                M10_onset = if (!is.null(circadian_data)) circadian_data$M10_start else NULL,
                title = chart_title
              )
            }, error = function(e) {
              showNotification(paste("Polar chart error:", e$message), type = "error", duration = 15)
              NULL
            })
          },

          "is_iv" = {
            if (!"timestamp" %in% names(data)) {
              showNotification("Data missing 'timestamp' column.", type = "error")
              return(NULL)
            }
            if (!"axis1" %in% names(data)) {
              showNotification("Data missing 'axis1' column.", type = "error")
              return(NULL)
            }

            if (!inherits(data$timestamp, "POSIXct")) {
              data$timestamp <- as.POSIXct(data$timestamp)
            }

            circadian_data <- shared$results$circadian[[sel]]
            canhrActi::plot_is_iv(
              data = data,
              is_value = if (!is.null(circadian_data)) circadian_data$IS else NULL,
              iv_value = if (!is.null(circadian_data)) circadian_data$IV else NULL,
              title = chart_title
            )
          },

          "daily_bars" = {
            activity_data <- shared$results$activity[[sel]]
            if (!is.null(activity_data) && !is.null(activity_data$daily)) {
              canhrActi::plot_daily_summary_bars(
                data = data,
                daily_summary = activity_data$daily,
                title = chart_title
              )
            } else {
              canhrActi::plot_daily_summary_bars(
                data = data,
                epoch_length = epoch_len,
                title = chart_title
              )
            }
          },

          "weekend_weekday" = {
            canhrActi::plot_weekend_weekday(
              data = data,
              title = chart_title
            )
          },

          "day_comparison" = {
            if ("timestamp" %in% names(data)) {
              compare_dates <- if (!is.null(input$compare_days)) {
                as.Date(input$compare_days)
              } else {
                dates <- unique(as.Date(data$timestamp))
                dates[1:min(3, length(dates))]
              }
              canhrActi::plot_day_comparison(
                data = data,
                dates = compare_dates,
                title = chart_title
              )
            } else {
              showNotification("Data missing 'timestamp' column.", type = "error")
              NULL
            }
          },

          {
            showNotification("Chart type not yet implemented.", type = "warning")
            NULL
          }
        )
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        NULL
      })

      current_plot(p)
      if (!is.null(p)) {
        shared$visualization_complete <- TRUE
      }
    })

    # Chart output
    output$chart_output <- renderUI({
      p <- current_plot()
      width <- input$plot_width %||% 800
      height <- input$plot_height %||% 600

      if (is.null(p)) {
        div(class = "chart-placeholder",
          icon("chart-area"),
          h4("Select a Chart"),
          p("Choose a chart type from the gallery and click 'Generate Chart'")
        )
      } else {
        plotOutput(ns("main_plot"), width = paste0(width, "px"), height = paste0(height, "px"))
      }
    })

    # Render main plot
    output$main_plot <- renderPlot({
      p <- current_plot()
      if (is.null(p)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Click 'Generate Chart' to create visualization",
                            size = 5, color = "#64748b") +
          ggplot2::theme_void() +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#f5f7fa", color = NA))
      } else {
        p
      }
    }, bg = "white")

    # Helper function to create error placeholder plot
    create_error_plot <- function(message) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                          size = 6, color = "#dc3545", fontface = "bold") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))
    }

    # Helper function to safely export plot to file
    safe_export_plot <- function(file, plot_obj, device_func, width_in, height_in, dpi = NULL, format_name = "image") {
      p <- if (is.null(plot_obj)) {
        create_error_plot("No chart generated.\nPlease generate a chart first.")
      } else {
        plot_obj
      }

      result <- tryCatch({
        if (!is.null(dpi)) {
          device_func(file, width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
        } else {
          device_func(file, width = width_in, height = height_in, bg = "white")
        }
        print(p)
        grDevices::dev.off()
        TRUE
      }, error = function(e) {
        try(grDevices::dev.off(), silent = TRUE)
        FALSE
      })

      if (!result || !file.exists(file) || file.info(file)$size == 0) {
        tryCatch({
          if (!is.null(dpi)) {
            device_func(file, width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
          } else {
            device_func(file, width = width_in, height = height_in, bg = "white")
          }
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Export Error")
          text(1, 1, "Failed to generate chart.\nPlease try again.", cex = 1.2, col = "red")
          grDevices::dev.off()
        }, error = function(e2) {
          NULL
        })
      }
    }

    # Export dropdown toggle
    export_dropdown_visible <- reactiveVal(FALSE)

    observeEvent(input$export_btn, {
      export_dropdown_visible(!export_dropdown_visible())
    })

    # Hide dropdown when clicking elsewhere (via any download)
    observeEvent(input$download_png_300, { export_dropdown_visible(FALSE) }, ignoreInit = TRUE)
    observeEvent(input$download_png_150, { export_dropdown_visible(FALSE) }, ignoreInit = TRUE)
    observeEvent(input$download_pdf, { export_dropdown_visible(FALSE) }, ignoreInit = TRUE)
    observeEvent(input$download_svg, { export_dropdown_visible(FALSE) }, ignoreInit = TRUE)

    output$export_dropdown_menu <- renderUI({
      if (!export_dropdown_visible()) return(NULL)

      tagList(
        div(
          id = ns("export_menu_container"),
          style = "position: absolute; top: 100%; right: 0; z-index: 1000; background: white; border: 1px solid #ddd; border-radius: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.15); min-width: 150px; margin-top: 4px;",
          div(style = "display: flex; flex-direction: column;",
            downloadButton(ns("download_png_300"), "PNG (300 DPI)", style = "border: none; border-radius: 0; text-align: left; background: white;"),
            downloadButton(ns("download_png_150"), "PNG (150 DPI)", style = "border: none; border-radius: 0; text-align: left; background: white;"),
            downloadButton(ns("download_pdf"), "PDF", style = "border: none; border-radius: 0; text-align: left; background: white;"),
            downloadButton(ns("download_svg"), "SVG", style = "border: none; border-radius: 0; text-align: left; background: white;")
          )
        ),
        # Close dropdown when clicking download buttons
        tags$script(HTML(sprintf("
          $('#%s .btn').click(function() {
            setTimeout(function() {
              Shiny.setInputValue('%s', Math.random());
            }, 100);
          });
        ", ns("export_menu_container"), ns("close_export_dropdown"))))
      )
    })

    # Close dropdown when triggered by JS
    observeEvent(input$close_export_dropdown, {
      export_dropdown_visible(FALSE)
    }, ignoreInit = TRUE)

    output$download_png_300 <- downloadHandler(
      filename = function() {
        paste0("canhrActi_chart_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_300dpi.png")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::png, 12, 10, dpi = 300)
      },
      contentType = "image/png"
    )

    output$download_png_150 <- downloadHandler(
      filename = function() {
        paste0("canhrActi_chart_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_150dpi.png")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::png, 12, 10, dpi = 150)
      },
      contentType = "image/png"
    )

    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("canhrActi_chart_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::pdf, 12, 10)
      },
      contentType = "application/pdf"
    )

    output$download_svg <- downloadHandler(
      filename = function() {
        paste0("canhrActi_chart_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::svg, 12, 10)
      },
      contentType = "image/svg+xml"
    )

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("canhrActi_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        tryCatch({
          if (is.null(input$selected_file) || is.null(shared$files[[input$selected_file]])) {
            utils::write.csv(data.frame(Error = "No data available. Please select a file first."),
                             file, row.names = FALSE)
            showNotification("No data to export. Please select a file first.", type = "warning")
            return()
          }
          f <- shared$files[[input$selected_file]]
          utils::write.csv(f$data, file, row.names = FALSE)
        }, error = function(e) {
          utils::write.csv(data.frame(Error = paste("Export failed:", e$message)),
                           file, row.names = FALSE)
          showNotification(paste("Data export failed:", e$message), type = "error")
        })
      },
      contentType = "text/csv"
    )
  })
}
