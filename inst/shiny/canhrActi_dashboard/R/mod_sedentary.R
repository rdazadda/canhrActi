#' Sedentary Fragmentation Module - Redesigned
#'
#' Clean, insight-focused analysis of sedentary behavior patterns
#' Emphasizes actionable health insights over raw metrics

mod_sedentary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Page Header
    page_header(
      icon_name = "couch",
      title = "Sedentary Behavior Analysis",
      subtitle = "Sedentary behavior patterns",
      status_output_id = ns("analysis_status_badge")
    ),

    # CONTROLS BAR - Compact, clean
    fluidRow(
      column(12,
        div(
          class = "control-bar",
          fluidRow(
            column(3,
              actionButton(ns("analyze"), span(icon("play"), "Run Analysis"),
                          class = "btn-primary btn-block")
            ),
            column(3,
              selectInput(ns("file_select"), NULL,
                         choices = c("All Files (Average)" = "all"),
                         width = "100%")
            ),
            column(3,
              selectInput(ns("cut_points"), NULL,
                         choices = c("Freedson (1998)" = "freedson",
                                    "CANHR (2025)" = "canhr"),
                         selected = "freedson", width = "100%")
            ),
            column(3,
              tags$details(
                class = "sed-advanced-details",
                tags$summary(
                  class = "sed-advanced-summary",
                  icon("sliders-h"), " Advanced"
                ),
                div(
                  class = "sed-advanced-panel",
                  # Activity Tab Integration
                  div(
                    style = "margin-bottom: 12px; padding: 8px; background: #e8f4f8; border-radius: 4px; border-left: 3px solid #10b981;",
                    checkboxInput(ns("use_activity_bouts"), "Use Activity Tab Bouts", value = TRUE),
                    uiOutput(ns("activity_bouts_status")),
                    tags$small(
                      style = "color: #64748b; display: block; margin-top: -8px;",
                      "Uses same parameters as Activity tab (threshold, drop time, etc.)"
                    )
                  ),
                  sliderInput(ns("prolonged_threshold"), "Prolonged Bout Threshold",
                             min = 20, max = 60, value = 30, step = 5, post = " min"),
                  sliderInput(ns("min_bout_duration"), "Minimum Bout Duration",
                             min = 1, max = 10, value = 1, step = 1, post = " min"),
                  div(
                    style = "margin-top: 10px; padding: 8px; background: #f8fafc; border-radius: 4px; border-left: 3px solid #236192;",
                    checkboxInput(ns("include_sleep"), "Include Sleep in Sedentary (uncheck to exclude)", value = FALSE),
                    tags$small(
                      style = "color: #64748b; display: block; margin-top: -8px;",
                      "SBRN recommends: sedentary = waking behavior only"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),

    # KEY METRICS STRIP - Compact horizontal display
    fluidRow(
      column(12,
        div(
          class = "metrics-strip metrics-strip--transparent",
          uiOutput(ns("metric_sed_percent")),
          uiOutput(ns("metric_breaks_hr")),
          uiOutput(ns("metric_typical_bout")),
          uiOutput(ns("metric_alpha"))
        )
      )
    ),

    # HERO CHART - Daily Sedentary Pattern (Large, Primary Focus)
    fluidRow(
      column(12,
        div(
          class = "card",
          div(
            class = "card-header",
            div(class = "stack stack--gap-1",
              div(class = "card-title", "Daily Sedentary Pattern"),
              div(class = "card-subtitle", "When and how long you sit throughout the day")
            ),
            div(
              selectInput(ns("hero_chart_type"), NULL,
                         choices = c("Timeline View" = "timeline",
                                    "Hourly Heatmap" = "heatmap",
                                    "Bout Occurrence" = "occurrence"),
                         selected = "timeline", width = "180px")
            )
          ),
          div(class = "card-body",
            plotOutput(ns("hero_chart"), height = "320px")
          )
        )
      )
    ),

    # TWO-COLUMN LAYOUT: Fragmentation Insights + Prolonged Warnings
    fluidRow(
      # LEFT: Fragmentation Insights Card
      column(6,
        div(
          class = "card",
          div(class = "card-header",
            div(class = "card-title", "Fragmentation Pattern")
          ),
          div(class = "card-body",
            uiOutput(ns("fragmentation_insight_card"))
          )
        )
      ),
      # RIGHT: Prolonged Sedentary Warnings
      column(6,
        div(
          class = "card",
          div(class = "card-header",
            div(class = "card-title", "Prolonged Sitting Alert")
          ),
          div(class = "card-body",
            uiOutput(ns("prolonged_warning_card"))
          )
        )
      )
    ),

    # BOUT ANALYSIS - Tabbed Interface
    fluidRow(
      column(12,
        div(
          class = "card",
          div(class = "card-header",
            div(class = "card-title", "Detailed Bout Analysis")
          ),
          div(class = "card-body",
          tabsetPanel(
            id = ns("bout_tabs"),
            type = "pills",

            # Tab 1: Bout Distribution
            tabPanel(
              "Distribution",
              div(class = "py-4",
                fluidRow(
                  column(6, plotOutput(ns("bout_histogram"), height = "300px")),
                  column(6, plotOutput(ns("bout_categories"), height = "300px"))
                )
              )
            ),

            # Tab 2: Accumulation & Survival
            tabPanel(
              "Accumulation",
              div(class = "py-4",
                fluidRow(
                  column(6, plotOutput(ns("accumulation_curve"), height = "300px")),
                  column(6, plotOutput(ns("survival_curve"), height = "300px"))
                )
              )
            ),

            # Tab 3: Hourly Patterns
            tabPanel(
              "Hourly Breakdown",
              div(class = "py-4",
                fluidRow(
                  column(6, plotOutput(ns("hourly_bouts"), height = "300px")),
                  column(6, plotOutput(ns("hourly_duration"), height = "300px"))
                )
              )
            ),

            # Tab 4: Transitions
            tabPanel(
              "State Transitions",
              div(class = "py-4",
                fluidRow(
                  column(6, plotOutput(ns("transition_matrix"), height = "300px")),
                  column(6,
                    div(
                      class = "sed-help-panel",
                      h5(class = "sed-help-title", "Understanding Transitions"),
                      p(class = "sed-help-text",
                        tags$strong("SATP"), " (Sedentary to Active Transition Probability): ",
                        "Higher values indicate more frequent breaks from sitting. ",
                        "Values above 0.05 suggest good movement patterns."
                      ),
                      hr(class = "my-3"),
                      p(class = "sed-help-text",
                        tags$strong("ASTP"), " (Active to Sedentary Transition Probability): ",
                        "Lower values mean activity bouts are sustained longer. ",
                        "Balance between SATP and ASTP reflects overall movement quality."
                      )
                    )
                  )
                )
              )
            )
          )
          )
        )
      )
    ),

    # EXPERT METRICS - Collapsible Panel
    fluidRow(
      column(12,
        tags$details(
          class = "expert-panel",
          tags$summary(
            icon("microscope"), " Expert Metrics (SATP, ASTP, Alpha, Gini, W25/50/75/90)"
          ),
          div(
            class = "expert-content pt-4",
            fluidRow(
              # Transition Probabilities
              column(3,
                div(class = "expert-group",
                  h5("Transition Probabilities"),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_satp"), inline = TRUE)),
                    div(class = "adv-metric-label", "SATP"),
                    div(class = "adv-metric-desc", "Sed to Active")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_astp"), inline = TRUE)),
                    div(class = "adv-metric-label", "ASTP"),
                    div(class = "adv-metric-desc", "Active to Sed")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_abi"), inline = TRUE)),
                    div(class = "adv-metric-label", "ABI"),
                    div(class = "adv-metric-desc", "Balance Index")
                  )
                )
              ),
              # Distribution Shape
              column(3,
                div(class = "expert-group",
                  h5("Distribution Shape"),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_alpha"), inline = TRUE)),
                    div(class = "adv-metric-label", "Alpha"),
                    div(class = "adv-metric-desc", "Power-Law Exponent")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_gini"), inline = TRUE)),
                    div(class = "adv-metric-label", "Gini"),
                    div(class = "adv-metric-desc", "Inequality Index")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_dist_type"), inline = TRUE)),
                    div(class = "adv-metric-label", "Distribution"),
                    div(class = "adv-metric-desc", "Best Fit Model")
                  )
                )
              ),
              # Weighted Percentiles
              column(3,
                div(class = "expert-group",
                  h5("Weighted Bout Percentiles"),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_w25"), inline = TRUE)),
                    div(class = "adv-metric-label", "W25"),
                    div(class = "adv-metric-desc", "25th Percentile")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_w75"), inline = TRUE)),
                    div(class = "adv-metric-label", "W75"),
                    div(class = "adv-metric-desc", "75th Percentile")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_w90"), inline = TRUE)),
                    div(class = "adv-metric-label", "W90"),
                    div(class = "adv-metric-desc", "90th Percentile")
                  )
                )
              ),
              # Bout Statistics
              column(3,
                div(class = "expert-group",
                  h5("Bout Statistics"),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_total_bouts"), inline = TRUE)),
                    div(class = "adv-metric-label", "Total Bouts"),
                    div(class = "adv-metric-desc", "Count")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_mean_bout"), inline = TRUE)),
                    div(class = "adv-metric-label", "Mean Bout"),
                    div(class = "adv-metric-desc", "Average Duration")
                  ),
                  div(class = "adv-metric",
                    div(class = "adv-metric-value", textOutput(ns("exp_max_bout"), inline = TRUE)),
                    div(class = "adv-metric-label", "Max Bout"),
                    div(class = "adv-metric-desc", "Longest Session")
                  )
                )
              )
            )
          )
        )
      )
    ),

    # DATA TABLE & EXPORT
    fluidRow(
      column(12,
        div(
          class = "card",
          div(class = "card-header",
            div(class = "card-title", "All Subjects Summary"),
            div(
              class = "btn-group",
              downloadButton(ns("dl_csv"), span(icon("download"), "Summary CSV"),
                            class = "btn-success btn-sm"),
              downloadButton(ns("dl_bouts_csv"), span(icon("list"), "Bout-Level CSV"),
                            class = "btn-info btn-sm", style = "margin-left: 5px;"),
              tags$button(
                id = ns("show_ibi_analysis"),
                class = "btn btn-outline-secondary btn-sm action-button",
                style = "margin-left: 5px;",
                icon("chart-bar"), " Break Analysis"
              )
            )
          ),
          div(class = "card-body",
            DT::dataTableOutput(ns("summary_table")),
            # IBI Analysis panel (hidden by default)
            conditionalPanel(
              condition = sprintf("input['%s'] > 0", ns("show_ibi_analysis")),
              div(
                style = "margin-top: 20px; padding: 15px; background: #f8fafc; border-radius: 8px; border: 1px solid #e2e8f0;",
                h5(icon("chart-bar"), " Inter-Bout Interval Analysis (Break Patterns)", style = "color: #236192; margin-bottom: 15px;"),
                uiOutput(ns("ibi_analysis_output"))
              )
            )
          )
        )
      )
    )
  )
}

mod_sedentary_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Activity Tab Bouts Status Indicator
    output$activity_bouts_status <- renderUI({
      sed_bouts <- shared$results$sedentary_bouts
      if (!is.null(sed_bouts) && !is.null(sed_bouts$bouts) && length(sed_bouts$bouts) > 0) {
        params <- sed_bouts$parameters
        n_files <- length(sed_bouts$bouts)
        total_bouts <- sum(sapply(sed_bouts$bouts, nrow))
        tags$div(
          style = "font-size: 11px; color: #10b981; margin-top: 4px;",
          icon("check-circle"),
          sprintf(" %d bouts from %d files (threshold: %d CPM)", total_bouts, n_files, params$threshold)
        )
      } else {
        tags$div(
          style = "font-size: 11px; color: #f59e0b; margin-top: 4px;",
          icon("exclamation-triangle"),
          " Run Activity Analysis first to detect bouts"
        )
      }
    })

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

    # Run Analysis
    observeEvent(input$analyze, {
      req(length(shared$files) > 0)

      # Check if wear time has been analyzed
      wt_results <- shared$results$wear_time
      use_wear_time <- !is.null(wt_results) && length(wt_results) > 0

      # Check if sleep analysis has been run
      sleep_results <- shared$results$sleep
      use_sleep_exclusion <- !is.null(sleep_results) && length(sleep_results) > 0

      # Warn if wear time not analyzed
      if (!use_wear_time) {
        showNotification(
          HTML("<strong>Recommendation:</strong> Run Wear Time Analysis first for accurate results.<br>
                Currently, non-wear periods (0 counts) may be counted as sedentary."),
          type = "warning",
          duration = 8
        )
      }

      # Inform about sleep exclusion (per SBRN consensus: sedentary = waking behavior only)
      include_sleep <- input$include_sleep
      if (!include_sleep && use_sleep_exclusion) {
        showNotification(
          HTML("<strong>Sleep Exclusion Active:</strong> Sleep periods will be excluded from sedentary analysis.<br>
                Per SBRN consensus, sedentary behavior is defined as <em>waking</em> behavior only."),
          type = "message",
          duration = 6
        )
      } else if (!include_sleep && !use_sleep_exclusion) {
        showNotification(
          HTML("<strong>Note:</strong> Run Sleep Analysis first to exclude sleep periods from sedentary analysis.<br>
                Currently, sleep periods may be incorrectly counted as sedentary time."),
          type = "warning",
          duration = 8
        )
      }

      all_results <- list()

      # Check if Activity tab bouts are available and should be used
      use_activity_bouts <- input$use_activity_bouts %||% TRUE
      activity_bouts_available <- !is.null(shared$results$sedentary_bouts) &&
                                  !is.null(shared$results$sedentary_bouts$bouts) &&
                                  length(shared$results$sedentary_bouts$bouts) > 0

      if (use_activity_bouts && activity_bouts_available) {
        showNotification(
          HTML("<strong>Using Activity Tab Bouts:</strong> Fragmentation analysis uses the same bouts detected in Activity tab."),
          type = "message",
          duration = 5
        )
      }

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

          # Get sleep mask (to exclude sleep from sedentary analysis)
          # Per SBRN consensus: sedentary behavior is "waking behavior" only
          #
          #  Use DETECTED SLEEP PERIODS, not epoch-by-epoch classification!
          # The Sadeh/Cole-Kripke algorithms mark ANY low-activity epoch as "sleep",
          # which includes quiet waking sedentary (TV watching, reading, desk work).
          # Using raw sleep_state would incorrectly exclude these waking periods.
          #
          # Instead, we use the detected sleep period WINDOWS (in_bed_time to out_bed_time)
          # which represent actual sleep events, not just low activity.
          sleep_mask <- NULL
          if (!include_sleep && use_sleep_exclusion && fid %in% names(sleep_results)) {
            sleep_data <- sleep_results[[fid]]

            # PREFERRED: Use detected sleep periods (Tudor-Locke algorithm)
            if (!is.null(sleep_data$periods) && nrow(sleep_data$periods) > 0) {
              # Create mask based on sleep period windows
              sleep_mask <- rep(FALSE, nrow(data))
              n_periods <- nrow(sleep_data$periods)

              for (period_idx in seq_len(n_periods)) {
                # Convert character timestamps to POSIXct with explicit format
                # The sleep_analysis.R stores times as "%Y-%m-%d %H:%M:%S" strings
                period_start <- as.POSIXct(sleep_data$periods$in_bed_time[period_idx],
                                           format = "%Y-%m-%d %H:%M:%S",
                                           tz = attr(data$timestamp[1], "tzone") %||% "")
                period_end <- as.POSIXct(sleep_data$periods$out_bed_time[period_idx],
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = attr(data$timestamp[1], "tzone") %||% "")

                # Mark epochs within this sleep period
                in_period <- data$timestamp >= period_start & data$timestamp <= period_end
                sleep_mask[in_period] <- TRUE
              }

              # Count sleep epochs for notification
              n_sleep_epochs <- sum(sleep_mask)
              sleep_hours <- round(n_sleep_epochs * epoch_length / 3600, 1)

            } else if (!is.null(sleep_data$sleep_state)) {
              # FALLBACK: Use epoch classification only if no periods detected
              # This is less accurate but better than nothing
              sleep_mask <- sleep_data$sleep_state %in% "S"

              showNotification(
                HTML(paste0(
                  "<strong>Warning:</strong> No sleep periods detected for ", f$name, "<br>",
                  "Using epoch-by-epoch classification (less accurate).<br>",
                  "Quiet waking sedentary may be incorrectly excluded."
                )),
                type = "warning",
                duration = 8
              )
            }
          }

          # Calculate intensity
          intensity <- tryCatch({
            if (input$cut_points == "freedson") {
              canhrActi::freedson(cpm)
            } else {
              canhrActi::CANHR.Cutpoints(cpm)
            }
          }, error = function(e) {
            showNotification(paste("Intensity calculation failed for", f$name, ":", e$message), type = "warning", duration = 5)
            NULL
          })

          if (is.null(intensity)) next

          # DEBUG: Show analysis inputs
          n_total <- length(intensity)
          n_sedentary <- sum(intensity == "sedentary")
          n_wear <- if (!is.null(wear_mask)) sum(wear_mask) else n_total
          n_sleep <- if (!is.null(sleep_mask)) sum(sleep_mask) else 0

          # Apply filters manually to see counts
          is_sed <- intensity == "sedentary"
          if (!is.null(wear_mask)) is_sed <- is_sed & wear_mask
          if (!is.null(sleep_mask)) is_sed <- is_sed & !sleep_mask
          n_sed_final <- sum(is_sed)

          # Run fragmentation analysis
          fragmentation <- NULL
          activity_bouts_used <- FALSE

          # Option 1: Use Activity tab bouts if available and selected
          if (use_activity_bouts && activity_bouts_available && fid %in% names(shared$results$sedentary_bouts$bouts)) {
            activity_bouts <- shared$results$sedentary_bouts$bouts[[fid]]

            if (!is.null(activity_bouts) && nrow(activity_bouts) > 0) {
              # Calculate fragmentation metrics from Activity tab bouts
              bout_durations <- activity_bouts$duration_min
              total_bouts <- nrow(activity_bouts)

              # Total sedentary time from bouts
              total_sed_time <- sum(bout_durations)

              # Breaks per sedentary hour (bouts per hour of sedentary time)
              breaks_per_sed_hour <- if (total_sed_time > 0) total_bouts / (total_sed_time / 60) else 0

              # Bout duration statistics
              mean_bout <- mean(bout_durations)
              median_bout <- median(bout_durations)
              max_bout <- max(bout_durations)
              min_bout <- min(bout_durations)

              # Wx percentiles (duration below which X% of sedentary time accumulated)
              sorted_durations <- sort(bout_durations, decreasing = TRUE)
              cumsum_durations <- cumsum(sorted_durations)
              W25_idx <- which(cumsum_durations >= total_sed_time * 0.25)[1]
              W50_idx <- which(cumsum_durations >= total_sed_time * 0.50)[1]
              W75_idx <- which(cumsum_durations >= total_sed_time * 0.75)[1]
              W90_idx <- which(cumsum_durations >= total_sed_time * 0.90)[1]
              W25 <- if (!is.na(W25_idx)) sorted_durations[W25_idx] else max_bout
              W50 <- if (!is.na(W50_idx)) sorted_durations[W50_idx] else median_bout
              W75 <- if (!is.na(W75_idx)) sorted_durations[W75_idx] else median_bout
              W90 <- if (!is.na(W90_idx)) sorted_durations[W90_idx] else min_bout

              # Alpha (power law exponent) - using MLE for bout durations
              xmin <- min(bout_durations)
              if (xmin > 0 && total_bouts > 1) {
                alpha <- 1 + total_bouts / sum(log(bout_durations / xmin))
              } else {
                alpha <- NA
              }

              # Gini coefficient for bout duration inequality
              n <- length(bout_durations)
              if (n > 1) {
                sorted_d <- sort(bout_durations)
                gini <- (2 * sum(seq_along(sorted_d) * sorted_d) - (n + 1) * sum(sorted_d)) / (n * sum(sorted_d))
              } else {
                gini <- 0
              }

              # Transition probabilities (ASTP and SATP)
              # Calculate from the intensity vector if available
              is_sed_vec <- intensity == "sedentary"
              if (!is.null(wear_mask)) is_sed_vec <- is_sed_vec & wear_mask
              if (!is.null(sleep_mask)) is_sed_vec <- is_sed_vec & !sleep_mask

              n_sed <- sum(is_sed_vec, na.rm = TRUE)
              n_active <- sum(!is_sed_vec, na.rm = TRUE)
              transitions <- diff(as.numeric(is_sed_vec))
              n_sed_to_active <- sum(transitions == -1, na.rm = TRUE)
              n_active_to_sed <- sum(transitions == 1, na.rm = TRUE)

              SATP <- if (n_sed > 0) n_sed_to_active / n_sed else 0
              ASTP <- if (n_active > 0) n_active_to_sed / n_active else 0

              # Prolonged bouts (>30 min - standard threshold)
              n_30min_bouts <- sum(bout_durations >= 30)
              time_30min_bouts <- sum(bout_durations[bout_durations >= 30])
              pct_time_30min_bouts <- if (total_sed_time > 0) 100 * time_30min_bouts / total_sed_time else 0

              # Create bouts data frame for compatibility
              bouts_df <- data.frame(
                start_time = activity_bouts$start_time,
                end_time = activity_bouts$end_time,
                duration_min = bout_durations,
                stringsAsFactors = FALSE
              )

              # Generate survival curve for the bouts
              survival_data <- canhrActi::bout.survival.analysis(bout_durations)

              # Generate bout distribution by duration categories
              bout_categories <- cut(bout_durations,
                breaks = c(0, 5, 10, 20, 30, 60, Inf),
                labels = c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "30-60 min", ">60 min"),
                right = TRUE, include.lowest = TRUE
              )
              bout_table <- table(bout_categories)
              bout_distribution <- data.frame(
                category = names(bout_table),
                count = as.integer(bout_table),
                percent = round(as.numeric(bout_table) / length(bout_durations) * 100, 1),
                stringsAsFactors = FALSE
              )

              fragmentation <- list(
                total_bouts = total_bouts,
                total_sedentary_min = total_sed_time,
                breaks_per_sed_hour = breaks_per_sed_hour,
                mean_bout_duration = mean_bout,
                median_bout_duration = median_bout,
                max_bout_duration = max_bout,
                min_bout_duration = min_bout,
                W25 = W25,
                W50 = W50,
                W75 = W75,
                W90 = W90,
                alpha = alpha,
                gini = gini,
                ASTP = ASTP,
                SATP = SATP,
                n_30min_bouts = n_30min_bouts,
                pct_time_30min_bouts = pct_time_30min_bouts,
                bout_durations = bout_durations,
                bouts = bouts_df,
                bout_distribution = bout_distribution,
                survival_curve = survival_data$survival_curve,
                median_bout_survival = survival_data$median_survival,
                hazard_rate = survival_data$hazard_rate,
                source = "activity_tab"
              )
              activity_bouts_used <- TRUE
            }
          }

          # Option 2: Fall back to independent fragmentation analysis
          if (is.null(fragmentation)) {
            fragmentation <- tryCatch({
              canhrActi::sedentary.fragmentation(
                intensity = intensity,
                timestamps = data$timestamp,
                wear_time = wear_mask,
                sleep_mask = sleep_mask,
                epoch_length = epoch_length
              )
            }, error = function(e) {
              showNotification(paste(f$name, ":", e$message), type = "error")
              NULL
            })
            if (!is.null(fragmentation)) {
              fragmentation$source <- "independent"
            }
          }

          if (is.null(fragmentation)) next

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            fragmentation = fragmentation,
            intensity = intensity,
            timestamps = data$timestamp,
            wear_mask = wear_mask,
            sleep_excluded = !is.null(sleep_mask),
            sleep_mask = sleep_mask,
            activity_bouts_used = activity_bouts_used
          )
        }
      })

      results(all_results)
      shared$results$sedentary <- all_results

      # Count integration stats
      n_sleep_excluded <- sum(sapply(all_results, function(r) isTRUE(r$sleep_excluded)))
      n_activity_bouts <- sum(sapply(all_results, function(r) isTRUE(r$activity_bouts_used)))

      # Build notification message
      msg_parts <- c(paste0("<strong>Sedentary analysis complete</strong> for ", length(all_results), " files"))
      if (n_activity_bouts > 0) {
        msg_parts <- c(msg_parts, paste0("<span style='color: #10b981;'>✓ Using Activity tab bouts for ", n_activity_bouts, " file(s)</span>"))
      }
      if (n_sleep_excluded > 0) {
        msg_parts <- c(msg_parts, paste0("<span style='color: #17a589;'>✓ Sleep periods excluded from ", n_sleep_excluded, " file(s)</span>"))
      }

      showNotification(
        HTML(paste(msg_parts, collapse = "<br>")),
        type = "message",
        duration = 6
      )
    })

    output$analysis_status_badge <- renderUI({
      res <- results()
      n <- length(res)
      if (n > 0) {
        n_activity_bouts <- sum(sapply(res, function(r) isTRUE(r$activity_bouts_used)))
        n_sleep_excluded <- sum(sapply(res, function(r) isTRUE(r$sleep_excluded)))
        badge_text <- paste(n, "file(s) analyzed")
        extras <- c()
        if (n_activity_bouts > 0) extras <- c(extras, "integrated")
        if (n_sleep_excluded > 0) extras <- c(extras, "sleep excl.")
        if (length(extras) > 0) badge_text <- paste0(badge_text, " (", paste(extras, collapse = ", "), ")")
        status_badge(badge_text, "success")
      } else {
        status_badge("Ready to analyze", "pending")
      }
    })

    # Helper: Safely extract numeric value (handles NULL)
    safe_extract <- function(res_list, field) {
      vapply(res_list, function(r) {
        val <- r$fragmentation[[field]]
        if (is.null(val) || length(val) == 0) NA_real_ else as.numeric(val[1])
      }, FUN.VALUE = numeric(1))
    }

    # Helper: Get current fragmentation data
    current_frag <- reactive({
      res <- results()
      req(length(res) > 0)

      sel <- input$file_select

      if (sel == "all") {
        # Average across all files
        all_durations <- unlist(lapply(res, function(r) {
          if (!is.null(r$fragmentation$bouts)) r$fragmentation$bouts$duration_min else NULL
        }))
        abi_result <- if (length(all_durations) > 0) {
          canhrActi::activity.balance.index(all_durations)
        } else {
          list(ABI = NA_real_)
        }

        dist_type <- NA_character_
        for (r in res) {
          if (!is.null(r$fragmentation$distribution_fit)) {
            dist_type <- r$fragmentation$distribution_fit$best_model
            break
          }
        }

        list(
          mode = "all",
          total_sedentary_min = mean(safe_extract(res, "total_sedentary_min"), na.rm = TRUE),
          total_bouts = sum(safe_extract(res, "total_bouts"), na.rm = TRUE),
          mean_bout_duration = mean(safe_extract(res, "mean_bout_duration"), na.rm = TRUE),
          median_bout_duration = mean(safe_extract(res, "median_bout_duration"), na.rm = TRUE),
          max_bout_duration = max(safe_extract(res, "max_bout_duration"), na.rm = TRUE),
          breaks_per_sed_hour = mean(safe_extract(res, "breaks_per_sed_hour"), na.rm = TRUE),
          alpha = mean(safe_extract(res, "alpha"), na.rm = TRUE),
          gini = mean(safe_extract(res, "gini"), na.rm = TRUE),
          ASTP = mean(safe_extract(res, "ASTP"), na.rm = TRUE),
          SATP = mean(safe_extract(res, "SATP"), na.rm = TRUE),
          W50 = mean(safe_extract(res, "W50"), na.rm = TRUE),
          W25 = mean(safe_extract(res, "W25"), na.rm = TRUE),
          W75 = mean(safe_extract(res, "W75"), na.rm = TRUE),
          W90 = mean(safe_extract(res, "W90"), na.rm = TRUE),
          prolonged_percent = mean(safe_extract(res, "pct_time_30min_bouts"), na.rm = TRUE),
          prolonged_count = sum(safe_extract(res, "n_30min_bouts"), na.rm = TRUE),
          ABI = abi_result$ABI,
          dist_type = dist_type
        )
      } else if (sel %in% names(res)) {
        r <- res[[sel]]

        abi_result <- if (!is.null(r$fragmentation$bouts)) {
          canhrActi::activity.balance.index(r$fragmentation$bouts$duration_min)
        } else {
          list(ABI = NA_real_)
        }

        dist_type <- if (!is.null(r$fragmentation$distribution_fit)) {
          r$fragmentation$distribution_fit$best_model
        } else {
          NA_character_
        }

        list(
          mode = "single",
          total_sedentary_min = r$fragmentation$total_sedentary_min,
          total_bouts = r$fragmentation$total_bouts,
          mean_bout_duration = r$fragmentation$mean_bout_duration,
          median_bout_duration = r$fragmentation$median_bout_duration,
          max_bout_duration = r$fragmentation$max_bout_duration,
          breaks_per_sed_hour = r$fragmentation$breaks_per_sed_hour,
          alpha = r$fragmentation$alpha,
          gini = r$fragmentation$gini,
          ASTP = r$fragmentation$ASTP,
          SATP = r$fragmentation$SATP,
          W50 = r$fragmentation$W50,
          W25 = r$fragmentation$W25,
          W75 = r$fragmentation$W75,
          W90 = r$fragmentation$W90,
          prolonged_percent = r$fragmentation$pct_time_30min_bouts,
          prolonged_count = r$fragmentation$n_30min_bouts,
          ABI = abi_result$ABI,
          dist_type = dist_type
        )
      } else {
        NULL
      }
    })

    # KEY METRICS STRIP
    output$metric_sed_percent <- renderUI({
      cf <- current_frag()
      if (is.null(cf)) {
        metric_card("--", "Sedentary Time")
      } else {
        hours <- round(cf$total_sedentary_min / 60, 1)
        metric_card(paste0(hours, "h"), "Sedentary Time")
      }
    })

    output$metric_breaks_hr <- renderUI({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$breaks_per_sed_hour)) {
        metric_card("--/hr", "Breaks")
      } else {
        brk <- round(cf$breaks_per_sed_hour, 1)
        interp <- if (brk >= 3) "Frequent" else if (brk >= 1.5) "Moderate" else "Infrequent"
        metric_card(paste0(brk, "/hr"), "Breaks", interp)
      }
    })

    output$metric_typical_bout <- renderUI({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$W50)) {
        metric_card("--", "Typical Bout (W50)")
      } else {
        w50 <- round(cf$W50, 1)
        interp <- if (w50 < 15) "Short" else if (w50 < 30) "Moderate" else "Long"
        metric_card(paste0(w50, " min"), "Typical Bout (W50)", interp)
      }
    })

    output$metric_alpha <- renderUI({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$alpha)) {
        metric_card("--", "Alpha Index")
      } else {
        alpha <- round(cf$alpha, 2)
        interp <- if (alpha >= 2.0) "Fragmented" else if (alpha >= 1.5) "Mixed" else "Prolonged"
        metric_card(alpha, "Alpha Index", interp)
      }
    })

    # HERO CHART - Daily Pattern
    output$hero_chart <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Click 'Run Analysis' to visualize your sedentary behavior",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
      req(input$hero_chart_type)
        chart_type <- input$hero_chart_type

        # Combine all bouts
        all_bouts <- data.frame()
        for (r in res) {
          if (!is.null(r$fragmentation$bouts) && nrow(r$fragmentation$bouts) > 0) {
            b <- r$fragmentation$bouts
            b$hour <- as.integer(format(b$start_time, "%H"))
            b$date <- as.Date(b$start_time)
            b$subject <- r$subject_id
            all_bouts <- rbind(all_bouts, b)
          }
        }

        if (nrow(all_bouts) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                             label = "No sedentary bout data available", size = 5, hjust = 0.5) +
            ggplot2::theme_void()
        } else if (chart_type == "timeline") {
          # Timeline view: hourly sedentary minutes
          hourly_data <- aggregate(duration_min ~ hour, all_bouts, sum)
          hourly_counts <- table(all_bouts$hour)
          hourly_data$n_bouts <- as.numeric(hourly_counts[as.character(hourly_data$hour)])
          hourly_data$avg_duration <- hourly_data$duration_min / hourly_data$n_bouts

          # Add missing hours
          all_hours <- data.frame(hour = 0:23)
          hourly_data <- merge(all_hours, hourly_data, by = "hour", all.x = TRUE)
          hourly_data$duration_min[is.na(hourly_data$duration_min)] <- 0
          hourly_data$n_bouts[is.na(hourly_data$n_bouts)] <- 0
          hourly_data$avg_duration[is.na(hourly_data$avg_duration)] <- 0

          # Calculate color based on average bout duration
          hourly_data$bout_category <- cut(hourly_data$avg_duration,
                                           breaks = c(-Inf, 10, 20, 30, Inf),
                                           labels = c("Short (<10)", "Moderate (10-20)", "Long (20-30)", "Prolonged (>30)"))

          ggplot2::ggplot(hourly_data, ggplot2::aes(x = hour, y = duration_min)) +
            ggplot2::geom_area(fill = "#236192", alpha = 0.3) +
            ggplot2::geom_line(color = "#236192", linewidth = 1.2) +
            ggplot2::geom_point(ggplot2::aes(size = n_bouts, color = avg_duration), alpha = 0.8) +
            ggplot2::scale_color_gradient2(low = "#17a589", mid = "#FFCD00", high = "#236192",
                                          midpoint = 20, name = "Avg Bout\n(min)") +
            ggplot2::scale_size_continuous(name = "Bouts", range = c(2, 8)) +
            ggplot2::scale_x_continuous(breaks = seq(0, 23, 2),
                                       labels = paste0(seq(0, 23, 2), ":00")) +
            ggplot2::annotate("rect", xmin = 6, xmax = 9, ymin = -Inf, ymax = Inf,
                             fill = "#FFCD00", alpha = 0.08) +
            ggplot2::annotate("rect", xmin = 17, xmax = 21, ymin = -Inf, ymax = Inf,
                             fill = "#3a7ab0", alpha = 0.08) +
            ggplot2::labs(
              title = NULL,
              x = "Hour of Day",
              y = "Total Sedentary Minutes"
            ) +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              panel.grid.minor = ggplot2::element_blank(),
              panel.grid.major.x = ggplot2::element_blank(),
              legend.position = "right",
              axis.title = ggplot2::element_text(color = "#64748b"),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )

        } else if (chart_type == "heatmap") {
          # Heatmap view
          heatmap_data <- aggregate(duration_min ~ hour + date, all_bouts, sum)

          # Convert date to factor for proper discrete y-axis handling
          heatmap_data$date_label <- format(heatmap_data$date, "%b %d")
          heatmap_data$date_label <- factor(heatmap_data$date_label,
                                            levels = unique(heatmap_data$date_label[order(heatmap_data$date)]))

          ggplot2::ggplot(heatmap_data, ggplot2::aes(x = hour, y = date_label, fill = duration_min)) +
            ggplot2::geom_tile(color = "white", linewidth = 0.5, width = 1, height = 1) +
            ggplot2::scale_fill_gradient2(low = "#f8fafc", mid = "#3a7ab0", high = "#0f2d42",
                                         midpoint = median(heatmap_data$duration_min, na.rm = TRUE),
                                         name = "Minutes") +
            ggplot2::scale_x_continuous(breaks = seq(0, 23, 3), expand = c(0, 0)) +
            ggplot2::labs(title = NULL, x = "Hour of Day", y = "Date") +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              panel.grid = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_text(size = 10),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )

        } else {
          # Bout occurrence scatter
          all_bouts$time_of_day <- as.numeric(format(all_bouts$start_time, "%H")) +
                                   as.numeric(format(all_bouts$start_time, "%M")) / 60

          # Convert date to factor for proper discrete y-axis
          all_bouts$date_label <- format(all_bouts$date, "%b %d")
          all_bouts$date_label <- factor(all_bouts$date_label,
                                         levels = unique(all_bouts$date_label[order(all_bouts$date)]))

          ggplot2::ggplot(all_bouts, ggplot2::aes(x = time_of_day, y = date_label, size = duration_min, color = duration_min)) +
            ggplot2::geom_point(alpha = 0.6) +
            ggplot2::scale_color_gradient2(low = "#17a589", mid = "#FFCD00", high = "#236192",
                                          midpoint = 30, name = "Duration\n(min)") +
            ggplot2::scale_size_continuous(range = c(1, 8), guide = "none") +
            ggplot2::scale_x_continuous(breaks = seq(0, 24, 4),
                                       labels = paste0(seq(0, 24, 4), ":00"),
                                       limits = c(0, 24)) +
            ggplot2::labs(title = NULL, x = "Time of Day", y = "Date") +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              panel.grid.minor = ggplot2::element_blank(),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
        }
      }
    })

    # FRAGMENTATION INSIGHT CARD
    output$fragmentation_insight_card <- renderUI({
      cf <- current_frag()

      if (is.null(cf)) {
        return(empty_state(
          title = "No Fragmentation Data",
          message = "Run Analysis to see fragmentation insights",
          show_icon = FALSE,
          small = TRUE,
          extra_class = "empty-state--compact"
        ))
      } else {
        # Calculate fragmentation score
        score <- 50
        if (!is.na(cf$SATP) && !is.na(cf$W50) && !is.na(cf$prolonged_percent)) {
          satp_pts <- min(35, cf$SATP * 350)
          w50_pts <- max(0, 35 - (cf$W50 / 60) * 35)
          prolonged_pts <- max(0, 30 - cf$prolonged_percent * 0.5)
          score <- round(satp_pts + w50_pts + prolonged_pts)
        }

        # Determine pattern type
        if (score >= 70) {
          pattern <- "Well-Fragmented"
          pattern_color <- "var(--canhr-success)"
          pattern_bg <- "rgba(23, 165, 137, 0.1)"
          pattern_icon <- "check-circle"
          advice <- "Your sedentary time is broken up with frequent movement. Keep up the good habits!"
        } else if (score >= 45) {
          pattern <- "Moderately Fragmented"
          pattern_color <- "var(--canhr-blue)"
          pattern_bg <- "rgba(35, 97, 146, 0.1)"
          pattern_icon <- "info-circle"
          advice <- "You have some prolonged sitting periods. Consider setting reminders to move every 30 minutes."
        } else {
          pattern <- "Prolonged Pattern"
          pattern_color <- "var(--canhr-caution)"
          pattern_bg <- "rgba(244, 185, 66, 0.15)"
          pattern_icon <- "exclamation-circle"
          advice <- "Extended sedentary periods detected. Breaking up sitting improves metabolic health."
        }

        tagList(
          # Score display
          div(
            class = "sed-score",
            div(
              class = "sed-score-badge",
              style = paste0("background: ", pattern_bg, ";"),
              div(score, class = "sed-score-value", style = paste0("color: ", pattern_color, ";")),
              div("/100", class = "sed-score-unit")
            ),
            div(class = "sed-score-body",
              div(
                class = "cluster cluster--gap-2 mb-2",
                icon(pattern_icon, style = paste0("color: ", pattern_color, ";")),
                span(class = "sed-score-status", style = paste0("color: ", pattern_color, ";"), pattern)
              ),
              p(advice, class = "sed-score-advice")
            )
          ),

          # Key metrics
          div(
            class = "sed-metric-grid",
            div(
              class = "sed-metric-cell",
              div("Alpha", class = "sed-metric-label"),
              div(
                if (is.na(cf$alpha)) "--" else sprintf("%.2f", cf$alpha),
                class = "sed-metric-value"
              ),
              div(
                if (is.na(cf$alpha)) "N/A"
                else if (cf$alpha >= 2.0) "Many short bouts"
                else if (cf$alpha >= 1.5) "Mixed pattern"
                else "Few long bouts",
                class = "sed-metric-subtext"
              )
            ),
            div(
              class = "sed-metric-cell",
              div("Gini Index", class = "sed-metric-label"),
              div(
                if (is.na(cf$gini)) "--" else sprintf("%.3f", cf$gini),
                class = "sed-metric-value"
              ),
              div(
                if (is.na(cf$gini)) "N/A"
                else if (cf$gini < 0.4) "Even distribution"
                else if (cf$gini < 0.6) "Moderate inequality"
                else "High inequality",
                class = "sed-metric-subtext"
              )
            )
          )
        )
      }
    })

    # PROLONGED SEDENTARY WARNING CARD
    output$prolonged_warning_card <- renderUI({
      cf <- current_frag()

      if (is.null(cf)) {
        return(empty_state(
          title = "No Prolonged Sitting Data",
          message = "Run Analysis to check for prolonged sitting",
          show_icon = FALSE,
          small = TRUE,
          extra_class = "empty-state--compact"
        ))
      } else {
        prolonged_pct <- cf$prolonged_percent
        prolonged_count <- cf$prolonged_count %||% NA
        max_bout <- cf$max_bout_duration

        # Determine severity
        if (is.na(prolonged_pct) || prolonged_pct < 20) {
          severity <- "low"
          severity_color <- "var(--canhr-success)"
          severity_bg <- "rgba(23, 165, 137, 0.1)"
          severity_icon <- "check-circle"
          message <- "Minimal prolonged sitting detected. You're doing well at breaking up sedentary time."
        } else if (prolonged_pct < 40) {
          severity <- "moderate"
          severity_color <- "var(--canhr-blue)"
          severity_bg <- "rgba(35, 97, 146, 0.1)"
          severity_icon <- "info-circle"
          message <- "Some prolonged bouts found. Consider standing or walking every 30 minutes."
        } else {
          severity <- "high"
          severity_color <- "var(--canhr-caution)"
          severity_bg <- "rgba(244, 185, 66, 0.15)"
          severity_icon <- "exclamation-triangle"
          message <- "Significant prolonged sitting detected. This is associated with increased health risks."
        }

        tagList(
          # Alert banner
          div(
            class = "sed-alert",
            style = paste0("background: ", severity_bg, ";"),
            icon(severity_icon, class = "sed-alert-icon", style = paste0("color: ", severity_color, ";")),
            div(
              class = "sed-alert-body",
              div(
                if (is.na(prolonged_pct)) "--%" else paste0(round(prolonged_pct, 1), "%"),
                class = "sed-alert-value",
                style = paste0("color: ", severity_color, ";")
              ),
              div("of sedentary time in bouts >30 min", class = "sed-alert-label")
            )
          ),

          p(message, class = "sed-alert-message"),

          # Stats
          div(
            class = "sed-metric-grid",
            div(
              class = "sed-metric-cell sed-metric-cell--center",
              div(
                if (is.na(prolonged_count)) "--" else prolonged_count,
                class = "sed-metric-value"
              ),
              div("Prolonged Bouts", class = "sed-metric-label")
            ),
            div(
              class = "sed-metric-cell sed-metric-cell--center",
              div(
                if (is.na(max_bout)) "--" else paste0(round(max_bout), " min"),
                class = "sed-metric-value"
              ),
              div("Longest Bout", class = "sed-metric-label")
            )
          ),

          # Health tip
          if (prolonged_pct >= 20 && !is.na(prolonged_pct)) {
            div(
              class = "tip-box",
              icon("lightbulb"),
              tags$strong("Health Tip: "),
              "Try the 20-8-2 rule: 20 min sitting, 8 min standing, 2 min moving each half hour."
            )
          }
        )
      }
    })

    # BOUT ANALYSIS PLOTS

    # Bout histogram
    output$bout_histogram <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        all_bouts <- do.call(rbind, lapply(res, function(r) {
          if (!is.null(r$fragmentation$bouts)) r$fragmentation$bouts else NULL
        }))

        if (is.null(all_bouts) || nrow(all_bouts) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No bout data", size = 5) +
            ggplot2::theme_void()
        } else {
          avg_alpha <- mean(sapply(res, function(r) r$fragmentation$alpha), na.rm = TRUE)

          ggplot2::ggplot(all_bouts, ggplot2::aes(x = duration_min)) +
            ggplot2::geom_histogram(binwidth = 5, fill = "#236192", alpha = 0.8, color = "white") +
            ggplot2::geom_vline(xintercept = median(all_bouts$duration_min), linetype = "dashed",
                               color = "#FFCD00", linewidth = 1) +
            ggplot2::geom_vline(xintercept = 30, linetype = "dotted",
                               color = "#f4b942", linewidth = 1) +
            ggplot2::annotate("label", x = 30, y = Inf, label = "30 min threshold",
                             vjust = 1.5, size = 3, fill = "#fff8e1") +
            ggplot2::labs(
              title = "Bout Duration Distribution",
              subtitle = sprintf("Alpha = %.2f | Median = %.1f min | N = %d bouts",
                                avg_alpha, median(all_bouts$duration_min), nrow(all_bouts)),
              x = "Duration (minutes)", y = "Count"
            ) +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
              plot.subtitle = ggplot2::element_text(color = "#64748b"),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
        }
      }
    })

    # Bout categories
    output$bout_categories <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        all_dist <- do.call(rbind, lapply(res, function(r) {
          if (!is.null(r$fragmentation$bout_distribution)) r$fragmentation$bout_distribution else NULL
        }))

        if (is.null(all_dist) || nrow(all_dist) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No distribution data", size = 5) +
            ggplot2::theme_void()
        } else {
          agg_dist <- aggregate(count ~ category, all_dist, sum)
          agg_dist$category <- factor(agg_dist$category,
                                       levels = c("1-5 min", "5-10 min", "10-20 min",
                                                 "20-30 min", "30-60 min", ">60 min"))
          total <- sum(agg_dist$count)
          agg_dist$pct <- round(agg_dist$count / total * 100, 1)

          # Color based on duration category
          colors <- c("1-5 min" = "#17a589", "5-10 min" = "#3a7ab0",
                     "10-20 min" = "#236192", "20-30 min" = "#FFCD00",
                     "30-60 min" = "#f4b942", ">60 min" = "#e6a000")

          ggplot2::ggplot(agg_dist, ggplot2::aes(x = category, y = count, fill = category)) +
            ggplot2::geom_col(alpha = 0.9, show.legend = FALSE) +
            ggplot2::geom_text(ggplot2::aes(label = paste0(count, "\n(", pct, "%)")),
                              vjust = -0.3, size = 3.5) +
            ggplot2::scale_fill_manual(values = colors) +
            ggplot2::labs(
              title = "Bouts by Duration Category",
              subtitle = paste("Total:", total, "bouts"),
              x = NULL, y = "Number of Bouts"
            ) +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
              plot.subtitle = ggplot2::element_text(color = "#64748b"),
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
        }
      }
    })

    # Accumulation curve
    output$accumulation_curve <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        all_bouts <- do.call(rbind, lapply(res, function(r) {
          if (!is.null(r$fragmentation$bouts)) r$fragmentation$bouts else NULL
        }))

        if (is.null(all_bouts) || nrow(all_bouts) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No bout data", size = 5) +
            ggplot2::theme_void()
        } else {
          all_bouts <- all_bouts[order(all_bouts$duration_min, decreasing = TRUE), ]
          total_sed <- sum(all_bouts$duration_min)
          all_bouts$cum_time <- cumsum(all_bouts$duration_min)
          all_bouts$cum_pct <- all_bouts$cum_time / total_sed * 100
          all_bouts$bout_pct <- seq_len(nrow(all_bouts)) / nrow(all_bouts) * 100

          # Calculate Gini with bias correction (matches package implementation)
          n <- nrow(all_bouts)
          x <- sort(all_bouts$duration_min)
          gini <- (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))
          if (n > 1) gini <- gini * n / (n - 1)  # Finite-sample bias correction

          ggplot2::ggplot(all_bouts, ggplot2::aes(x = bout_pct, y = cum_pct)) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = bout_pct, ymax = cum_pct),
                                fill = "#236192", alpha = 0.2) +
            ggplot2::geom_line(color = "#236192", linewidth = 1.5) +
            ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#94a3b8") +
            ggplot2::annotate("text", x = 70, y = 30, label = paste("Gini =", round(gini, 3)),
                             size = 4, fontface = "bold", color = "#236192") +
            ggplot2::labs(
              title = "Sedentary Time Accumulation (Lorenz Curve)",
              subtitle = "Shaded area represents inequality in bout durations",
              x = "% of Bouts (longest first)", y = "% of Total Sedentary Time"
            ) +
            ggplot2::scale_x_continuous(limits = c(0, 100)) +
            ggplot2::scale_y_continuous(limits = c(0, 100)) +
            ggplot2::coord_fixed() +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
              plot.subtitle = ggplot2::element_text(color = "#64748b"),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
        }
      }
    })

    # Survival curve (Kaplan-Meier style)
    output$survival_curve <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        # Collect bout durations from all subjects
        all_durations <- list()
        groups <- c()
        for (r in res) {
          if (!is.null(r$fragmentation$bout_durations) && length(r$fragmentation$bout_durations) > 0) {
            all_durations[[r$subject_id]] <- r$fragmentation$bout_durations
            groups <- c(groups, r$subject_id)
          }
        }

        if (length(all_durations) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No bout data", size = 5) +
            ggplot2::theme_void()
        } else {
          # Use the new enhanced Kaplan-Meier survival visualization
          tryCatch({
            # If single subject, pass durations directly; if multiple, pass as list
            if (length(all_durations) == 1) {
              canhrActi::plot_survival_curves(
                bout_durations = all_durations[[1]],
                groups = NULL,
                show_ci = TRUE,
                show_median = TRUE,
                max_time = NULL,
                title = "Sedentary Bout Survival Analysis"
              )
            } else {
              # Multiple subjects - combine into one vector with group labels
              combined_durations <- unlist(all_durations)
              combined_groups <- rep(names(all_durations), sapply(all_durations, length))

              canhrActi::plot_survival_curves(
                bout_durations = combined_durations,
                groups = combined_groups,
                show_ci = TRUE,
                show_median = TRUE,
                max_time = NULL,
                title = "Sedentary Bout Survival Analysis"
              )
            }
          }, error = function(e) {
            # Fallback to original simple survival curve
            all_curves <- do.call(rbind, lapply(res, function(r) {
              if (!is.null(r$fragmentation$survival_curve)) {
                sc <- r$fragmentation$survival_curve
                sc$subject <- r$subject_id
                sc
              } else NULL
            }))

            if (is.null(all_curves) || nrow(all_curves) == 0) {
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No survival data", size = 5) +
                ggplot2::theme_void()
            } else {
              avg_w50 <- mean(sapply(res, function(r) r$fragmentation$W50), na.rm = TRUE)
              ggplot2::ggplot(all_curves, ggplot2::aes(x = time, y = survival_prob,
                                                       group = subject, color = subject)) +
                ggplot2::geom_step(alpha = 0.8, linewidth = 1) +
                ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "#94a3b8") +
                ggplot2::scale_y_continuous(labels = scales::percent_format()) +
                ggplot2::scale_color_brewer(palette = "Set2") +
                ggplot2::labs(title = "Bout Survival Curve", x = "Time (minutes)", y = "Survival Probability") +
                canhrActi::theme_canhrActi()
            }
          })
        }
      }
    })

    # Hourly bouts
    output$hourly_bouts <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        all_bouts <- do.call(rbind, lapply(res, function(r) {
          if (!is.null(r$fragmentation$bouts) && nrow(r$fragmentation$bouts) > 0) {
            b <- r$fragmentation$bouts
            b$hour <- as.integer(format(b$start_time, "%H"))
            b
          } else NULL
        }))

        if (is.null(all_bouts) || nrow(all_bouts) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No bout data", size = 5) +
            ggplot2::theme_void()
        } else {
          hourly_counts <- as.data.frame(table(all_bouts$hour))
          names(hourly_counts) <- c("hour", "count")
          hourly_counts$hour <- as.integer(as.character(hourly_counts$hour))

          # Fill missing hours
          all_hours <- data.frame(hour = 0:23)
          hourly_counts <- merge(all_hours, hourly_counts, by = "hour", all.x = TRUE)
          hourly_counts$count[is.na(hourly_counts$count)] <- 0

          ggplot2::ggplot(hourly_counts, ggplot2::aes(x = hour, y = count)) +
            ggplot2::geom_col(fill = "#3a7ab0", alpha = 0.8) +
            ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#FFCD00", linewidth = 1.5, span = 0.4) +
            ggplot2::scale_x_continuous(breaks = seq(0, 23, 3)) +
            ggplot2::labs(
              title = "Hourly Bout Frequency",
              subtitle = "Number of sedentary bouts starting each hour",
              x = "Hour of Day", y = "Number of Bouts"
            ) +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
              plot.subtitle = ggplot2::element_text(color = "#64748b"),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
        }
      }
    })

    # Hourly duration
    output$hourly_duration <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        all_bouts <- do.call(rbind, lapply(res, function(r) {
          if (!is.null(r$fragmentation$bouts) && nrow(r$fragmentation$bouts) > 0) {
            b <- r$fragmentation$bouts
            b$hour <- as.integer(format(b$start_time, "%H"))
            b
          } else NULL
        }))

        if (is.null(all_bouts) || nrow(all_bouts) == 0) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No bout data", size = 5) +
            ggplot2::theme_void()
        } else {
          ggplot2::ggplot(all_bouts, ggplot2::aes(x = factor(hour), y = duration_min)) +
            ggplot2::geom_boxplot(fill = "#236192", alpha = 0.6, outlier.alpha = 0.3) +
            ggplot2::geom_hline(yintercept = 30, linetype = "dashed", color = "#f4b942") +
            ggplot2::scale_x_discrete(breaks = as.character(seq(0, 23, 3))) +
            ggplot2::labs(
              title = "Hourly Bout Duration Distribution",
              subtitle = "Boxplot of bout durations by hour (dashed = 30 min threshold)",
              x = "Hour of Day", y = "Duration (minutes)"
            ) +
            canhrActi::theme_canhrActi() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", color = "#236192"),
              plot.subtitle = ggplot2::element_text(color = "#64748b"),
              plot.background = ggplot2::element_rect(fill = "white", color = NA)
            )
        }
      }
    })

    # Transition matrix
    output$transition_matrix <- renderPlot({
      res <- results()

      if (length(res) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Run Analysis to see results",
                           size = 5, hjust = 0.5, color = "#64748b") +
          ggplot2::theme_void()
      } else {
        avg_astp <- mean(sapply(res, function(r) r$fragmentation$ASTP), na.rm = TRUE)
        avg_satp <- mean(sapply(res, function(r) r$fragmentation$SATP), na.rm = TRUE)

        trans_data <- data.frame(
          from = c("Active", "Active", "Sedentary", "Sedentary"),
          to = c("Stay Active", "Go Sedentary", "Break (Get Up)", "Stay Sedentary"),
          prob = c(1 - avg_astp, avg_astp, avg_satp, 1 - avg_satp),
          type = c("stay", "change", "change", "stay")
        )
        trans_data$label <- sprintf("%.1f%%", trans_data$prob * 100)
        trans_data$from <- factor(trans_data$from, levels = c("Active", "Sedentary"))
        trans_data$to <- factor(trans_data$to, levels = c("Stay Active", "Go Sedentary", "Break (Get Up)", "Stay Sedentary"))

        ggplot2::ggplot(trans_data, ggplot2::aes(x = to, y = from, fill = prob)) +
          ggplot2::geom_tile(color = "white", linewidth = 2) +
          ggplot2::geom_text(ggplot2::aes(label = label), size = 6, fontface = "bold",
                            color = ifelse(trans_data$prob > 0.5, "white", "#1a202c")) +
          ggplot2::scale_fill_gradient2(low = "#e8f5e9", mid = "#42a5f5", high = "#0d47a1",
                                       midpoint = 0.5, limits = c(0, 1),
                                       name = "Probability") +
          ggplot2::labs(
            title = "State Transition Probabilities",
            subtitle = sprintf("SATP = %.3f (breaks) | ASTP = %.3f (sitting down)", avg_satp, avg_astp),
            x = "Transition To", y = "Current State"
          ) +
          canhrActi::theme_canhrActi() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", color = "#236192", hjust = 0.5),
            plot.subtitle = ggplot2::element_text(color = "#64748b", hjust = 0.5),
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
            plot.background = ggplot2::element_rect(fill = "white", color = NA)
          )
      }
    })

    # EXPERT METRICS OUTPUTS
    output$exp_satp <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$SATP)) "--" else sprintf("%.4f", cf$SATP)
    })

    output$exp_astp <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$ASTP)) "--" else sprintf("%.4f", cf$ASTP)
    })

    output$exp_abi <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$ABI)) "--" else sprintf("%.2f", cf$ABI)
    })

    output$exp_alpha <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$alpha)) "--" else sprintf("%.2f", cf$alpha)
    })

    output$exp_gini <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$gini)) "--" else sprintf("%.3f", cf$gini)
    })

    output$exp_dist_type <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$dist_type)) "--"
      else if (cf$dist_type == "power_law") "Power-Law"
      else if (cf$dist_type == "exponential") "Exponential"
      else cf$dist_type
    })

    output$exp_w25 <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$W25)) "--" else paste0(round(cf$W25, 1), " min")
    })

    output$exp_w75 <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$W75)) "--" else paste0(round(cf$W75, 1), " min")
    })

    output$exp_w90 <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$W90)) "--" else paste0(round(cf$W90, 1), " min")
    })

    output$exp_total_bouts <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$total_bouts)) "--" else format(cf$total_bouts, big.mark = ",")
    })

    output$exp_mean_bout <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$mean_bout_duration)) "--"
      else paste0(round(cf$mean_bout_duration, 1), " min")
    })

    output$exp_max_bout <- renderText({
      cf <- current_frag()
      if (is.null(cf) || is.na(cf$max_bout_duration)) "--"
      else paste0(round(cf$max_bout_duration), " min")
    })

    # SUMMARY TABLE
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (length(res) == 0) {
        return(DT::datatable(
          data.frame(Message = "Click 'Run Analysis' to see results"),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      df <- data.frame(
        Subject = sapply(res, function(r) r$subject_id),
        `Sed Hours` = sapply(res, function(r) {
          val <- r$fragmentation$total_sedentary_min
          if (is.null(val) || is.na(val)) NA_real_ else round(val / 60, 1)
        }),
        Bouts = sapply(res, function(r) {
          val <- r$fragmentation$total_bouts
          if (is.null(val)) NA_integer_ else val
        }),
        `Breaks/Hr` = sapply(res, function(r) {
          val <- r$fragmentation$breaks_per_sed_hour
          if (is.null(val) || is.na(val)) NA_real_ else round(val, 2)
        }),
        `W50 (min)` = sapply(res, function(r) {
          val <- r$fragmentation$W50
          if (is.null(val) || is.na(val)) NA_real_ else round(val, 1)
        }),
        Alpha = sapply(res, function(r) {
          val <- r$fragmentation$alpha
          if (is.null(val) || is.na(val)) NA_real_ else round(val, 2)
        }),
        Gini = sapply(res, function(r) {
          val <- r$fragmentation$gini
          if (is.null(val) || is.na(val)) NA_real_ else round(val, 3)
        }),
        SATP = sapply(res, function(r) {
          val <- r$fragmentation$SATP
          if (is.null(val) || is.na(val)) NA_real_ else round(val, 4)
        }),
        `Prolonged %` = sapply(res, function(r) {
          val <- r$fragmentation$pct_time_30min_bouts
          if (is.null(val) || is.na(val)) NA_real_ else round(val, 1)
        }),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "frtip",
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "Prolonged %",
          backgroundColor = DT::styleInterval(c(20, 40), c("#e8f5e9", "#fff8e1", "#ffebee"))
        ) |>
        DT::formatStyle(
          "Breaks/Hr",
          backgroundColor = DT::styleInterval(c(1.5, 3), c("#ffebee", "#fff8e1", "#e8f5e9"))
        )
    })

    # CSV EXPORT
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("sedentary_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- results()
        req(length(res) > 0)

        df <- data.frame(
          subject_id = sapply(res, function(r) r$subject_id),
          file_name = sapply(res, function(r) r$name),
          sleep_excluded = sapply(res, function(r) isTRUE(r$sleep_excluded)),
          total_sedentary_hours = sapply(res, function(r) round(r$fragmentation$total_sedentary_min / 60, 2)),
          total_bouts = sapply(res, function(r) r$fragmentation$total_bouts),
          mean_bout_min = sapply(res, function(r) round(r$fragmentation$mean_bout_duration, 2)),
          median_bout_min = sapply(res, function(r) round(r$fragmentation$median_bout_duration, 2)),
          max_bout_min = sapply(res, function(r) round(r$fragmentation$max_bout_duration, 2)),
          breaks_per_sed_hour = sapply(res, function(r) round(r$fragmentation$breaks_per_sed_hour, 3)),
          ASTP = sapply(res, function(r) round(r$fragmentation$ASTP, 4)),
          SATP = sapply(res, function(r) round(r$fragmentation$SATP, 4)),
          W25 = sapply(res, function(r) round(r$fragmentation$W25, 2)),
          W50 = sapply(res, function(r) round(r$fragmentation$W50, 2)),
          W75 = sapply(res, function(r) round(r$fragmentation$W75, 2)),
          W90 = sapply(res, function(r) round(r$fragmentation$W90, 2)),
          alpha = sapply(res, function(r) round(r$fragmentation$alpha, 3)),
          gini = sapply(res, function(r) round(r$fragmentation$gini, 4)),
          prolonged_bouts_count = sapply(res, function(r) r$fragmentation$n_30min_bouts),
          prolonged_percent = sapply(res, function(r) round(r$fragmentation$pct_time_30min_bouts, 2)),
          stringsAsFactors = FALSE
        )

        write.csv(df, file, row.names = FALSE)
      }
    )

    # BOUT-LEVEL CSV EXPORT (format)
    output$dl_bouts_csv <- downloadHandler(
      filename = function() {
        paste0("sedentary_bouts_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- results()
        req(length(res) > 0)

        all_bouts <- list()
        min_bout <- input$min_bout_duration

        for (fid in names(res)) {
          r <- res[[fid]]
          f <- shared$files[[fid]]
          data <- f$data
          epoch_length <- f$epoch_length

          # Get subject info
          subj <- f$subject_info
          subject_id <- subj$id
          weight_lbs <- if (!is.null(subj$weight_lbs)) subj$weight_lbs else 0
          age <- if (!is.null(subj$age)) subj$age else 0
          gender <- if (!is.null(subj$sex)) subj$sex else ""

          # Calculate CPM and identify sedentary epochs
          cpm <- data$axis1 * (60 / epoch_length)
          sedentary_threshold <- if (input$cut_points == "freedson") 100 else 150
          is_sedentary <- cpm < sedentary_threshold

          # Apply wear time if available
          if (fid %in% names(shared$results$wear_time)) {
            wear_mask <- shared$results$wear_time[[fid]]$wear
            is_sedentary <- is_sedentary & wear_mask
          }

          # Detect bouts using RLE
          rle_sed <- rle(is_sedentary)
          end_indices <- cumsum(rle_sed$lengths)
          start_indices <- c(1, end_indices[-length(end_indices)] + 1)

          sed_mask <- rle_sed$values
          bout_starts <- start_indices[sed_mask]
          bout_ends <- end_indices[sed_mask]

          if (length(bout_starts) == 0) next

          # Calculate durations and filter
          bout_lengths <- bout_ends - bout_starts + 1
          duration_min <- bout_lengths * (epoch_length / 60)
          valid_bouts <- duration_min >= min_bout

          bout_starts <- bout_starts[valid_bouts]
          bout_ends <- bout_ends[valid_bouts]
          duration_min <- duration_min[valid_bouts]

          if (length(bout_starts) == 0) next

          # Build bout-level data
          for (i in seq_along(bout_starts)) {
            start_idx <- bout_starts[i]
            end_idx <- bout_ends[i]

            bout_data <- data[start_idx:end_idx, ]
            bout_start_time <- data$timestamp[start_idx]
            bout_end_time <- data$timestamp[end_idx]

            # Inter-bout interval
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

            axis1_cpm <- axis1_avg * (60 / epoch_length)
            axis2_cpm <- axis2_avg * (60 / epoch_length)
            axis3_cpm <- axis3_avg * (60 / epoch_length)

            # Vector magnitude - full metrics
            if (all(c("axis1", "axis2", "axis3") %in% names(bout_data))) {
              vm <- sqrt(bout_data$axis1^2 + bout_data$axis2^2 + bout_data$axis3^2)
              vm_counts <- sum(vm, na.rm = TRUE)
              vm_avg <- mean(vm, na.rm = TRUE)
              vm_max <- max(vm, na.rm = TRUE)
              vm_cpm <- vm_avg * (60 / epoch_length)
            } else {
              vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
            }

            # Steps - full metrics
            if ("steps" %in% names(bout_data)) {
              steps_counts <- sum(bout_data$steps, na.rm = TRUE)
              steps_avg <- mean(bout_data$steps, na.rm = TRUE)
              steps_max <- max(bout_data$steps, na.rm = TRUE)
              steps_per_min <- steps_avg * (60 / epoch_length)
            } else {
              steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
            }

            # Lux (light)
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

            # column names (matching exact format)
            all_bouts[[length(all_bouts) + 1]] <- data.frame(
              Subject = subject_id,
              Filename = r$name,
              Epoch = epoch_length,
              `Weight (lbs)` = weight_lbs,
              Age = age,
              Gender = gender,
              `Sedentary Bout Start` = format(bout_start_time, "%m/%d/%Y %I:%M:%S %p"),
              `Sedentary Bout End` = format(bout_end_time, "%m/%d/%Y %I:%M:%S %p"),
              `Time in Sedentary Bout` = round(duration_min[i], 1),
              `Time since last Sedentary Bout` = round(time_since_last, 1),
              `Axis 1 Counts` = round(axis1_counts, 1),
              `Axis 2 Counts` = round(axis2_counts, 1),
              `Axis 3 Counts` = round(axis3_counts, 1),
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
              Time = round(duration_min[i], 1),
              `Calendar Days` = calendar_days,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }

        if (length(all_bouts) > 0) {
          bout_df <- do.call(rbind, all_bouts)
          write.csv(bout_df, file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "No sedentary bouts detected"), file, row.names = FALSE)
        }
      }
    )

    # INTER-BOUT INTERVAL ANALYSIS OUTPUT
    output$ibi_analysis_output <- renderUI({
      res <- results()
      req(length(res) > 0)

      # Collect all IBIs across files
      all_ibis <- c()
      min_bout <- input$min_bout_duration

      for (fid in names(res)) {
        r <- res[[fid]]
        f <- shared$files[[fid]]
        data <- f$data
        epoch_length <- f$epoch_length

        # Calculate CPM and identify sedentary epochs
        cpm <- data$axis1 * (60 / epoch_length)
        sedentary_threshold <- if (input$cut_points == "freedson") 100 else 150
        is_sedentary <- cpm < sedentary_threshold

        # Apply wear time
        if (fid %in% names(shared$results$wear_time)) {
          is_sedentary <- is_sedentary & shared$results$wear_time[[fid]]$wear
        }

        # Detect bouts
        rle_sed <- rle(is_sedentary)
        end_indices <- cumsum(rle_sed$lengths)
        start_indices <- c(1, end_indices[-length(end_indices)] + 1)

        sed_mask <- rle_sed$values
        bout_starts <- start_indices[sed_mask]
        bout_ends <- end_indices[sed_mask]

        if (length(bout_starts) < 2) next

        # Filter by min bout duration
        bout_lengths <- bout_ends - bout_starts + 1
        duration_min <- bout_lengths * (epoch_length / 60)
        valid_idx <- which(duration_min >= min_bout)

        if (length(valid_idx) < 2) next

        bout_starts <- bout_starts[valid_idx]
        bout_ends <- bout_ends[valid_idx]

        # Calculate IBIs
        for (i in 2:length(bout_starts)) {
          prev_end_time <- data$timestamp[bout_ends[i - 1]]
          curr_start_time <- data$timestamp[bout_starts[i]]
          ibi <- as.numeric(difftime(curr_start_time, prev_end_time, units = "mins"))
          if (ibi > 0) all_ibis <- c(all_ibis, ibi)
        }
      }

      if (length(all_ibis) == 0) {
        return(div(class = "alert alert-warning", "Not enough data for inter-bout interval analysis"))
      }

      # Calculate statistics
      n_breaks <- length(all_ibis)
      mean_ibi <- round(mean(all_ibis), 1)
      median_ibi <- round(median(all_ibis), 1)
      sd_ibi <- round(sd(all_ibis), 1)

      # Break classifications
      micro <- sum(all_ibis < 2)
      short <- sum(all_ibis >= 2 & all_ibis < 5)
      medium <- sum(all_ibis >= 5 & all_ibis < 15)
      long <- sum(all_ibis >= 15 & all_ibis < 30)
      extended <- sum(all_ibis >= 30)

      pct_short <- round(100 * (micro + short) / n_breaks, 1)
      pct_substantial <- round(100 * (long + extended) / n_breaks, 1)

      # Create UI
      tagList(
        fluidRow(
          column(4,
            div(class = "stat-box", style = "background: #e3f2fd; padding: 12px; border-radius: 6px; text-align: center;",
              div(style = "font-size: 24px; font-weight: bold; color: #1565c0;", n_breaks),
              div(style = "font-size: 12px; color: #64748b;", "Total Breaks")
            )
          ),
          column(4,
            div(class = "stat-box", style = "background: #e8f5e9; padding: 12px; border-radius: 6px; text-align: center;",
              div(style = "font-size: 24px; font-weight: bold; color: #2e7d32;", paste0(median_ibi, " min")),
              div(style = "font-size: 12px; color: #64748b;", "Median Break Duration")
            )
          ),
          column(4,
            div(class = "stat-box", style = "background: #fff3e0; padding: 12px; border-radius: 6px; text-align: center;",
              div(style = "font-size: 24px; font-weight: bold; color: #e65100;", paste0(pct_substantial, "%")),
              div(style = "font-size: 12px; color: #64748b;", "Substantial Breaks (>15 min)")
            )
          )
        ),
        div(style = "margin-top: 15px;",
          h6("Break Classifications:", style = "margin-bottom: 10px; color: #475569;"),
          tags$table(
            class = "table table-sm table-bordered",
            style = "font-size: 13px;",
            tags$thead(
              tags$tr(
                tags$th("Category"), tags$th("Count"), tags$th("Percent")
              )
            ),
            tags$tbody(
              tags$tr(tags$td("Micro-break (<2 min)"), tags$td(micro), tags$td(paste0(round(100*micro/n_breaks, 1), "%"))),
              tags$tr(tags$td("Short break (2-5 min)"), tags$td(short), tags$td(paste0(round(100*short/n_breaks, 1), "%"))),
              tags$tr(tags$td("Medium break (5-15 min)"), tags$td(medium), tags$td(paste0(round(100*medium/n_breaks, 1), "%"))),
              tags$tr(tags$td("Long break (15-30 min)"), tags$td(long), tags$td(paste0(round(100*long/n_breaks, 1), "%"))),
              tags$tr(tags$td("Extended break (>30 min)"), tags$td(extended), tags$td(paste0(round(100*extended/n_breaks, 1), "%")))
            )
          )
        ),
        div(style = "margin-top: 10px; padding: 10px; background: #f1f5f9; border-radius: 6px; font-size: 12px;",
          tags$strong("Interpretation: "),
          sprintf("%.1f%% of breaks are brief (<5 min), indicating frequent postural shifts. ", pct_short),
          sprintf("%.1f%% are substantial (>15 min), representing meaningful activity breaks.", pct_substantial),
          if (pct_substantial < 10) " Consider encouraging longer, more active breaks." else ""
        )
      )
    })

  })
}
