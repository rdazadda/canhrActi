# Module

mod_activity_ui <- function(id) {
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
                 icon("running", style = "margin-right: 10px;"),
                 "Physical Activity Analysis"),
              p(style = "margin: 0; opacity: 0.9; font-size: 13px;",
                "Score activity intensity levels, calculate MVPA, METs, and energy expenditure using validated algorithms.")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right;",
                uiOutput(ns("wear_time_status"))
              )
            )
          )
        )
      )
    ),

    # Summary Statistics at top
    fluidRow(
      valueBoxOutput(ns("vb_files_scored"), width = 3),
      valueBoxOutput(ns("vb_avg_mvpa"), width = 3),
      valueBoxOutput(ns("vb_avg_sedentary"), width = 3),
      valueBoxOutput(ns("vb_avg_mets"), width = 3)
    ),

    # Sedentary Fragmentation Header
    fluidRow(
      column(
        width = 12,
        div(
          style = "background: linear-gradient(135deg, #6c757d 0%, #495057 100%); color: white; padding: 10px 15px; border-radius: 8px; margin-bottom: 15px;",
          span(icon("couch"), " ", strong("Sedentary Fragmentation"), " - How sitting time is accumulated")
        )
      )
    ),

    # Sedentary Pattern Value Boxes
    fluidRow(
      valueBoxOutput(ns("vb_breaks_per_hour"), width = 3),
      valueBoxOutput(ns("vb_mean_bout"), width = 3),
      valueBoxOutput(ns("vb_total_bouts"), width = 3),
      valueBoxOutput(ns("vb_alpha"), width = 3)
    ),

    fluidRow(
      # Left column - Algorithms & Filters (like ActiLife)
      column(
        width = 4,

        # Algorithms Section
        box(
          title = span(icon("cogs", style = "margin-right: 8px;"), "Analysis Algorithms"),
          status = "primary", solidHeader = TRUE, width = NULL,

          # Cut Points and MVPA (main algorithm)
          div(
            style = "background: rgba(35,97,146,0.05); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
            checkboxInput(ns("use_cut_points"), span(icon("chart-bar"), " Cut Points & MVPA"), value = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_cut_points")),
              div(style = "padding-left: 20px;",
                selectInput(ns("cut_points"), NULL,
                            choices = c("Freedson Adult (1998)" = "freedson",
                                        "CANHR (2025)" = "CANHR"),
                            selected = "freedson"),
                tags$small(class = "text-muted", icon("info-circle"), " Uses Axis 1 counts")
              )
            )
          ),

          # METs
          div(
            style = "background: rgba(255,205,0,0.1); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
            checkboxInput(ns("use_mets"), span(icon("fire"), " Metabolic Equivalents (METs)"), value = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_mets")),
              div(style = "padding-left: 20px;",
                selectInput(ns("mets_algo"), NULL,
                            choices = c("Freedson VM3 (Sasaki 2011)" = "freedson.vm3",
                                        "Freedson Adult (1998)" = "freedson.adult",
                                        "Crouter 2-Regression (2010)" = "crouter",
                                        "Swartz Adult (2000)" = "swartz",
                                        "Hendelman Adult Overground (2000)" = "hendelman.adult",
                                        "Hendelman Adult Lifestyle (2000)" = "hendelman.lifestyle",
                                        "Leenders Adult Treadmill (2003)" = "leenders",
                                        "Yngve Adult Treadmill (2003)" = "yngve.treadmill",
                                        "Yngve Adult Overground (2003)" = "yngve.overground",
                                        "Brooks Adult Overground (2005)" = "brooks.overground",
                                        "Brooks Adult Body Mass (2005)" = "brooks.bm",
                                        "Freedson Children (2005)" = "freedson.children"),
                            selected = "freedson.vm3")
              )
            )
          ),

          # Energy Expenditure
          div(
            style = "background: rgba(40,167,69,0.1); border-radius: 8px; padding: 12px; margin-bottom: 12px;",
            checkboxInput(ns("use_ee"), span(icon("bolt"), " Energy Expenditure (kcal)"), value = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_ee")),
              div(style = "padding-left: 20px;",
                selectInput(ns("ee_algo"), NULL,
                            choices = c("Freedson Combination (1998)" = "freedson.combination",
                                        "Freedson (1998)" = "freedson",
                                        "Freedson VM3 (2011)" = "freedson.vm3",
                                        "Freedson VM3 Combination (2011)" = "freedson.vm3.combination",
                                        "Williams Work-Energy (1998)" = "williams"),
                            selected = "freedson.combination")
              )
            )
          ),

          # Bouts
          div(
            style = "background: rgba(23,162,184,0.1); border-radius: 8px; padding: 12px;",
            checkboxInput(ns("use_bouts"), span(icon("stopwatch"), " MVPA Bout Detection"), value = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_bouts")),
              div(style = "padding-left: 20px;",
                fluidRow(
                  column(6, numericInput(ns("bout_min"), "Min (min)", value = 10, min = 1, max = 60)),
                  column(6, selectInput(ns("bout_rule"), "Rule",
                                        choices = c("80%" = "80pct", "Consecutive" = "consecutive"),
                                        selected = "80pct"))
                )
              )
            )
          )
        ),

        # Filters Section
        box(
          title = span(icon("filter", style = "margin-right: 8px;"), "Filters"),
          status = "info", width = NULL,

          checkboxInput(ns("exclude_nonwear"), span(icon("check-circle"), " Exclude Non-Wear Times"), value = TRUE),

          div(
            style = "background: linear-gradient(135deg, #e8f4fc 0%, #f8fafc 100%); border-radius: 8px; padding: 10px; margin-top: 10px; border-left: 3px solid #236192; font-size: 11px; color: #1a4a6f;",
            icon("info-circle", style = "color: #236192;"),
            " Run Wear Time Validation first to enable this filter."
          )
        ),

        # Run Button
        box(
          width = NULL,
          style = "background: linear-gradient(135deg, #f8fafc 0%, #e8f4fc 100%);",
          actionButton(ns("run_btn"), span(icon("play"), " Score Physical Activity"),
                       class = "btn-success btn-block btn-lg",
                       style = "font-size: 16px; padding: 15px; background: linear-gradient(135deg, #236192 0%, #1a4a6f 100%); border: none;"),
          hr(style = "border-color: #b8d4e8; margin: 15px 0;"),
          div(
            style = "display: flex; flex-direction: column; gap: 8px;",
            downloadButton(ns("export_summary"), span(icon("file-csv"), " Summary"),
                          class = "btn-primary btn-block", style = "font-size: 12px;"),
            downloadButton(ns("export_daily"), span(icon("calendar-alt"), " Daily"),
                          class = "btn-info btn-block", style = "font-size: 12px;"),
            downloadButton(ns("export_hourly"), span(icon("clock"), " Hourly"),
                          class = "btn-default btn-block", style = "font-size: 12px;")
          )
        )
      ),

      # Right column - Data & Results
      column(
        width = 8,

        # Files Header
        box(
          title = NULL, width = NULL,
          style = "background: linear-gradient(90deg, rgba(35,97,146,0.05) 0%, transparent 100%);",
          fluidRow(
            column(6,
              div(style = "display: flex; align-items: center; gap: 15px;",
                div(
                  style = "background: #236192; color: white; padding: 8px 15px; border-radius: 20px; font-weight: 500;",
                  icon("file"), " ", textOutput(ns("files_count"), inline = TRUE), " files loaded"
                )
              )
            ),
            column(6, align = "right",
              actionButton(ns("clear_results"), span(icon("trash"), " Clear Results"),
                          class = "btn-default btn-sm", style = "border: 1px solid #dc3545; color: #dc3545;")
            )
          )
        ),

        # Data Table (ActiLife-style with activity columns)
        box(
          title = span(icon("list-alt", style = "margin-right: 8px;"), "Files & Results"),
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
          DT::dataTableOutput(ns("files_table"))
        ),

        # Results tabs
        tabBox(
          title = NULL, width = 12, id = ns("results_tabs"),

          # Summary Tab
          tabPanel(
            title = tagList(icon("table"), " Summary"),
            value = "summary",
            DT::dataTableOutput(ns("summary_table"))
          ),

          # Daily Tab
          tabPanel(
            title = tagList(icon("calendar"), " Daily"),
            value = "daily",
            DT::dataTableOutput(ns("daily_table"))
          ),

          # Charts Tab
          tabPanel(
            title = tagList(icon("chart-bar"), " Charts"),
            value = "charts",
            fluidRow(
              column(6,
                div(style = "background: white; border-radius: 8px; padding: 10px;",
                  plotOutput(ns("intensity_plot"), height = "280px")
                )
              ),
              column(6,
                div(style = "background: white; border-radius: 8px; padding: 10px;",
                  plotOutput(ns("hourly_plot"), height = "280px")
                )
              )
            )
          ),

          # Sedentary Patterns Tab
          tabPanel(
            title = tagList(icon("couch"), " Sedentary"),
            value = "sedentary",
            fluidRow(
              column(6,
                div(style = "background: white; border-radius: 8px; padding: 10px;",
                  plotOutput(ns("bout_dist_plot"), height = "280px")
                )
              ),
              column(6,
                div(style = "background: white; border-radius: 8px; padding: 10px;",
                  plotOutput(ns("accumulation_plot"), height = "280px")
                )
              )
            ),
            fluidRow(
              column(12,
                div(style = "margin-top: 15px;",
                  DT::dataTableOutput(ns("fragmentation_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_activity_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    results <- reactiveVal(list())

    # Files count
    output$files_count <- renderText({
      as.character(shared$file_count)
    })

    # Wear time status indicator (styled for header)
    output$wear_time_status <- renderUI({
      wt_available <- length(shared$results$wear_time) > 0
      if (wt_available) {
        div(
          style = "display: inline-block; background: rgba(40,167,69,0.3); padding: 8px 15px; border-radius: 20px; border: 1px solid rgba(40,167,69,0.5);",
          icon("check-circle", style = "color: #7fff7f;"), " Wear Time Available"
        )
      } else {
        div(
          style = "display: inline-block; background: rgba(255,205,0,0.2); padding: 8px 15px; border-radius: 20px; border: 1px solid rgba(255,205,0,0.4);",
          icon("exclamation-triangle", style = "color: #FFCD00;"), " No Wear Time"
        )
      }
    })

    # Files table (ActiLife-style with activity columns)
    output$files_table <- DT::renderDataTable({
      if (shared$file_count == 0) {
        return(DT::datatable(
          data.frame(Message = "No files loaded. Go to Data Upload tab."),
          rownames = FALSE, options = list(dom = 't')
        ))
      }

      res <- results()
      wt_res <- shared$results$wear_time

      df <- data.frame(
        `Subject Name` = sapply(shared$files, function(f) f$subject_info$id %||% "N/A"),
        `Serial Number` = sapply(shared$files, function(f) f$device_info$serial_number %||% "N/A"),
        `Validated?` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(wt_res)) "Yes" else "No"
        }),
        Status = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) "Scored" else "Not Scored"
        }),
        `Time in Sedentary` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$sedentary_min / 60, 1), "h") else "-"
        }),
        `Time in Light` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$light_min / 60, 1), "h") else "-"
        }),
        `Time in Moderate` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$moderate_min), "m") else "-"
        }),
        `Time in Vigorous` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$vigorous_min), "m") else "-"
        }),
        `MVPA` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res)) paste0(round(res[[fid]]$mvpa_min), "m") else "-"
        }),
        `Avg METs` = sapply(names(shared$files), function(fid) {
          if (fid %in% names(res) && !is.na(res[[fid]]$avg_mets)) round(res[[fid]]$avg_mets, 2) else "-"
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
                        fontWeight = "bold") %>%
        DT::formatStyle("Validated?",
                        color = DT::styleEqual(c("No", "Yes"), c("#95a5a6", "#27ae60")))
    })

    # Helper: Format ETA
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    # Run analysis
    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      all_results <- list()
      n_files <- shared$file_count
      wt_results <- shared$results$wear_time
      use_wear_time <- input$exclude_nonwear && length(wt_results) > 0
      start_time <- Sys.time()

      withProgress(message = "Scoring physical activity...", value = 0, {
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

          counts <- data$axis1
          epoch_length <- f$epoch_length

          # Convert counts to CPM (counts per minute) for cutpoint analysis
          # Freedson and other cutpoints are calibrated for 60-second epochs
          cpm <- canhrActi::to_cpm(counts, epoch_length)

          # Start with wear time mask if available and enabled
          analysis_mask <- rep(TRUE, length(counts))
          if (use_wear_time && fid %in% names(wt_results)) {
            analysis_mask <- wt_results[[fid]]$wear
          }

          # Apply mask to get valid CPM values
          valid_cpm <- cpm[analysis_mask]
          intensity <- tryCatch({
            if (input$cut_points == "freedson") {
              canhrActi::freedson(valid_cpm)
            } else {
              canhrActi::CANHR.Cutpoints(valid_cpm)
            }
          }, error = function(e) {
            showNotification(paste("Error in", f$name, ":", e$message), type = "error")
            return(NULL)
          })

          if (is.null(intensity)) next

          # MVPA bouts
          bouts <- NULL
          if (input$use_bouts) {
            bouts <- tryCatch({
              canhrActi::detect.mvpa.bouts(
                intensity = intensity,
                min_bout_length = input$bout_min,
                use_80_percent_rule = (input$bout_rule == "80pct")
              )
            }, error = function(e) NULL)
          }

          # Sedentary fragmentation analysis
          fragmentation <- NULL
          if ("timestamp" %in% names(data)) {
            # Build full intensity vector (with NA for non-wear)
            full_intensity <- rep(NA_character_, length(counts))
            full_intensity[analysis_mask] <- as.character(intensity)
            full_intensity <- factor(full_intensity,
                                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))

            fragmentation <- tryCatch({
              canhrActi::sedentary.fragmentation(
                intensity = full_intensity,
                timestamps = data$timestamp,
                wear_time = if (use_wear_time && fid %in% names(wt_results)) wt_results[[fid]]$wear else NULL,
                epoch_length = epoch_length
              )
            }, error = function(e) NULL)
          }

          # METs calculation
          mets <- NULL
          avg_mets <- NA
          if (input$use_mets) {
            mets <- tryCatch({
              subj_info <- list(
                mass = if (!is.null(f$subject_info$body_mass) && !is.na(f$subject_info$body_mass)) f$subject_info$body_mass else 70,
                age = if (!is.null(f$subject_info$age) && !is.na(f$subject_info$age)) f$subject_info$age else 35
              )
              canhrActi::calculate.mets(
                counts_data = data,
                algorithm = input$mets_algo,
                subject_info = subj_info,
                verbose = FALSE
              )
            }, error = function(e) NULL)

            if (!is.null(mets)) {
              mets_valid <- mets[analysis_mask]
              avg_mets <- mean(mets_valid, na.rm = TRUE)
            }
          }

          # Energy expenditure
          total_ee <- NA
          if (input$use_ee) {
            ee <- tryCatch({
              mass <- if (!is.null(f$subject_info$body_mass) && !is.na(f$subject_info$body_mass)) {
                f$subject_info$body_mass
              } else 70
              canhrActi::calculate.energy.expenditure.direct(
                counts_data = data, body_mass = mass,
                algorithm = input$ee_algo, epoch_length = f$epoch_length
              )
            }, error = function(e) NULL)
            if (!is.null(ee)) total_ee <- ee$total_kcal
          }

          # Calculate summary stats
          int_table <- table(intensity)
          n_valid_epochs <- sum(analysis_mask)
          n_days <- if ("timestamp" %in% names(data)) length(unique(as.Date(data$timestamp[analysis_mask]))) else 1
          epoch_min <- f$epoch_length / 60

          sedentary_min <- if ("sedentary" %in% names(int_table)) int_table["sedentary"] * epoch_min / n_days else 0
          light_min <- if ("light" %in% names(int_table)) int_table["light"] * epoch_min / n_days else 0
          moderate_min <- if ("moderate" %in% names(int_table)) int_table["moderate"] * epoch_min / n_days else 0
          vigorous_min <- sum(int_table[names(int_table) %in% c("vigorous", "very_vigorous")]) * epoch_min / n_days
          mvpa_min <- moderate_min + vigorous_min

          # Hourly pattern
          hourly <- NULL
          if ("timestamp" %in% names(data)) {
            temp <- data.frame(hour = as.numeric(format(data$timestamp, "%H")), counts = counts, mask = analysis_mask)
            temp <- temp[temp$mask, ]
            if (nrow(temp) > 0) hourly <- aggregate(counts ~ hour, temp, mean, na.rm = TRUE)
          }

          # Daily summary
          daily <- NULL
          if ("timestamp" %in% names(data)) {
            temp <- data
            temp$analyzed <- analysis_mask
            temp$date <- as.Date(temp$timestamp)
            temp$intensity <- NA
            temp$intensity[analysis_mask] <- as.character(intensity)

            daily <- aggregate(analyzed ~ date, temp, sum)
            daily$analyzed_hours <- daily$analyzed * f$epoch_length / 3600

            # Get intensity counts per day
            for (d in unique(temp$date)) {
              day_data <- temp[temp$date == d & temp$analyzed, ]
              if (nrow(day_data) > 0) {
                day_int <- table(day_data$intensity)
                daily[daily$date == d, "sedentary"] <- if ("sedentary" %in% names(day_int)) day_int["sedentary"] else 0
                daily[daily$date == d, "light"] <- if ("light" %in% names(day_int)) day_int["light"] else 0
                daily[daily$date == d, "moderate"] <- if ("moderate" %in% names(day_int)) day_int["moderate"] else 0
                daily[daily$date == d, "vigorous"] <- if ("vigorous" %in% names(day_int)) day_int["vigorous"] + if ("very_vigorous" %in% names(day_int)) day_int["very_vigorous"] else 0 else 0
              }
            }
          }

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            epoch_length = f$epoch_length,
            wear_time_applied = use_wear_time && fid %in% names(wt_results),
            intensity_valid = intensity,
            bouts = bouts,
            mets = mets,
            avg_mets = avg_mets,
            total_ee = total_ee,
            hourly = hourly,
            daily = daily,
            fragmentation = fragmentation,
            n_valid_epochs = n_valid_epochs,
            n_days = n_days,
            sedentary_min = as.numeric(sedentary_min),
            light_min = as.numeric(light_min),
            moderate_min = as.numeric(moderate_min),
            vigorous_min = as.numeric(vigorous_min),
            mvpa_min = as.numeric(mvpa_min),
            n_bouts = if (!is.null(bouts)) nrow(bouts) else 0,
            parameters = list(
              cut_points = input$cut_points,
              mets_algo = input$mets_algo,
              ee_algo = input$ee_algo,
              exclude_nonwear = input$exclude_nonwear
            )
          )
        }

        # Memory cleanup
        gc(verbose = FALSE)
      })

      results(all_results)
      shared$results$activity <- all_results

      showNotification(paste("Scoring complete for", length(all_results), "files!"), type = "message")
    })

    # Clear results
    observeEvent(input$clear_results, {
      results(list())
      shared$results$activity <- NULL
      showNotification("Activity results cleared.", type = "message")
    })

    # Value boxes
    output$vb_files_scored <- renderValueBox({
      res <- results()
      n <- if (is.null(res)) 0 else length(res)
      valueBox(n, "Files Scored", icon = icon("file"), color = if (n > 0) "green" else "red")
    })

    output$vb_avg_mvpa <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Avg MVPA/day", icon = icon("running"), color = "blue")
      } else {
        mvpa_values <- sapply(res, function(r) if (is.numeric(r$mvpa_min)) r$mvpa_min else NA)
        avg <- mean(mvpa_values, na.rm = TRUE)
        valueBox(paste0(round(avg), "m"), "Avg MVPA/day", icon = icon("running"), color = "blue")
      }
    })

    output$vb_avg_sedentary <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Avg Sedentary/day", icon = icon("couch"), color = "yellow")
      } else {
        sed_values <- sapply(res, function(r) if (is.numeric(r$sedentary_min)) r$sedentary_min else NA)
        avg <- mean(sed_values, na.rm = TRUE)
        valueBox(paste0(round(avg / 60, 1), "h"), "Avg Sedentary/day", icon = icon("couch"), color = "yellow")
      }
    })

    output$vb_avg_mets <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Avg METs", icon = icon("fire"), color = "orange")
      } else {
        mets_values <- sapply(res, function(r) if (is.numeric(r$avg_mets)) r$avg_mets else NA)
        avg <- mean(mets_values, na.rm = TRUE)
        if (is.na(avg)) {
          valueBox("--", "Avg METs", icon = icon("fire"), color = "orange")
        } else {
          valueBox(round(avg, 2), "Avg METs", icon = icon("fire"), color = "orange")
        }
      }
    })

    # Sedentary fragmentation value boxes
    output$vb_breaks_per_hour <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Breaks/Sed Hour", icon = icon("pause-circle"), color = "maroon")
      } else {
        breaks_values <- sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$breaks_per_sed_hour else NA
        })
        avg <- mean(breaks_values, na.rm = TRUE)
        if (is.na(avg)) {
          valueBox("--", "Breaks/Sed Hour", icon = icon("pause-circle"), color = "maroon")
        } else {
          valueBox(round(avg, 1), "Breaks/Sed Hour", icon = icon("pause-circle"), color = "maroon")
        }
      }
    })

    output$vb_mean_bout <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Mean Bout (min)", icon = icon("clock"), color = "maroon")
      } else {
        bout_values <- sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$mean_bout_duration else NA
        })
        avg <- mean(bout_values, na.rm = TRUE)
        if (is.na(avg)) {
          valueBox("--", "Mean Bout (min)", icon = icon("clock"), color = "maroon")
        } else {
          valueBox(paste0(round(avg, 1), "m"), "Mean Bout (min)", icon = icon("clock"), color = "maroon")
        }
      }
    })

    output$vb_total_bouts <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Total Sed Bouts", icon = icon("list"), color = "maroon")
      } else {
        bout_counts <- sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$total_bouts else NA
        })
        total <- sum(bout_counts, na.rm = TRUE)
        valueBox(total, "Total Sed Bouts", icon = icon("list"), color = "maroon")
      }
    })

    output$vb_alpha <- renderValueBox({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        valueBox("--", "Alpha (Fragmentation)", icon = icon("chart-line"), color = "maroon")
      } else {
        alpha_values <- sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$alpha else NA
        })
        avg <- mean(alpha_values, na.rm = TRUE)
        if (is.na(avg)) {
          valueBox("--", "Alpha (Fragmentation)", icon = icon("chart-line"), color = "maroon")
        } else {
          valueBox(round(avg, 2), "Alpha (Fragmentation)", icon = icon("chart-line"), color = "maroon")
        }
      }
    })

    # Summary table (matches export_summary format)
    output$summary_table <- DT::renderDataTable({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run 'Score Physical Activity' to see results"), rownames = FALSE))
      }

      df <- data.frame(
        Subject = sapply(res, function(r) r$subject_id),
        `Serial Number` = sapply(res, function(r) r$serial_number %||% ""),
        `Wear Filter` = sapply(res, function(r) if (r$wear_time_applied) "Yes" else "No"),
        `Days` = sapply(res, function(r) r$n_days),
        `Sedentary (h)` = sapply(res, function(r) round(r$sedentary_min / 60, 1)),
        `Light (h)` = sapply(res, function(r) round(r$light_min / 60, 1)),
        `Moderate (min)` = sapply(res, function(r) round(r$moderate_min)),
        `Vigorous (min)` = sapply(res, function(r) round(r$vigorous_min)),
        `MVPA (min)` = sapply(res, function(r) round(r$mvpa_min)),
        `Avg METs` = sapply(res, function(r) if (!is.na(r$avg_mets)) round(r$avg_mets, 2) else NA),
        `MVPA Bouts` = sapply(res, function(r) r$n_bouts),
        `EE (kcal)` = sapply(res, function(r) if (!is.na(r$total_ee)) round(r$total_ee) else NA),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    # Daily table
    output$daily_table <- DT::renderDataTable({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run 'Score Physical Activity' to see results"), rownames = FALSE))
      }

      all_daily <- list()
      for (r in res) {
        if (!is.null(r$daily) && nrow(r$daily) > 0) {
          d <- r$daily
          d$Subject <- r$subject_id
          all_daily[[length(all_daily) + 1]] <- d
        }
      }

      if (length(all_daily) == 0) {
        return(DT::datatable(data.frame(Message = "No daily data available"), rownames = FALSE))
      }

      df <- do.call(rbind, all_daily)
      df$date <- as.character(df$date)
      df$weekday <- weekdays(as.Date(df$date))

      DT::datatable(
        df[, c("Subject", "date", "weekday", "analyzed_hours", "sedentary", "light", "moderate", "vigorous")],
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Subject", "Date", "Day", "Analyzed (h)", "Sedentary", "Light", "Moderate", "Vigorous")
      )
    })

    # Intensity plot
    output$intensity_plot <- renderPlot({
      res <- results()
      req(length(res) > 0)

      all_intensity <- c()
      for (r in res) {
        if (!is.null(r$intensity_valid)) {
          all_intensity <- c(all_intensity, as.character(r$intensity_valid))
        }
      }
      req(length(all_intensity) > 0)

      int_cols <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")
      counts <- table(factor(all_intensity, levels = int_cols))
      df <- data.frame(
        intensity = factor(names(counts), levels = int_cols),
        hours = as.numeric(counts) * shared$epoch_length / 3600
      )

      colors <- c("sedentary" = "#95a5a6", "light" = "#3498db", "moderate" = "#f1c40f",
                  "vigorous" = "#e67e22", "very_vigorous" = "#e74c3c")

      ggplot2::ggplot(df, ggplot2::aes(x = intensity, y = hours, fill = intensity)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1fh", hours)), vjust = -0.5, size = 3) +
        ggplot2::scale_fill_manual(values = colors, guide = "none") +
        ggplot2::labs(title = "Time in Intensity Levels", x = "", y = "Hours") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9))
    })

    # Hourly plot
    output$hourly_plot <- renderPlot({
      res <- results()
      req(length(res) > 0)

      all_hourly <- data.frame()
      for (r in res) {
        if (!is.null(r$hourly)) {
          h <- r$hourly
          h$subject <- r$subject_id
          all_hourly <- rbind(all_hourly, h)
        }
      }
      req(nrow(all_hourly) > 0)

      avg_hourly <- aggregate(counts ~ hour, all_hourly, mean, na.rm = TRUE)

      ggplot2::ggplot(avg_hourly, ggplot2::aes(x = hour, y = counts)) +
        ggplot2::geom_area(fill = "#3c8dbc", alpha = 0.3) +
        ggplot2::geom_line(color = "#3c8dbc", linewidth = 1) +
        ggplot2::scale_x_continuous(breaks = seq(0, 23, 3)) +
        ggplot2::labs(title = "Average Hourly Pattern", x = "Hour", y = "Mean Counts") +
        ggplot2::theme_minimal(base_size = 11)
    })

    # Sedentary bout distribution plot
    output$bout_dist_plot <- renderPlot({
      res <- results()
      req(length(res) > 0)

      # Combine bout distributions from all files
      all_dist <- data.frame()
      for (r in res) {
        if (!is.null(r$fragmentation) && !is.null(r$fragmentation$bout_distribution)) {
          d <- r$fragmentation$bout_distribution
          d$subject <- r$subject_id
          all_dist <- rbind(all_dist, d)
        }
      }

      if (nrow(all_dist) == 0) {
        return(NULL)
      }

      # Aggregate across subjects
      agg_dist <- aggregate(count ~ category, all_dist, sum)
      agg_dist$category <- factor(agg_dist$category,
                                   levels = c("1-5 min", "5-10 min", "10-20 min",
                                             "20-30 min", "30-60 min", ">60 min"))

      ggplot2::ggplot(agg_dist, ggplot2::aes(x = category, y = count)) +
        ggplot2::geom_col(fill = "#6c757d", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 3.5) +
        ggplot2::labs(
          title = "Sedentary Bout Duration Distribution",
          subtitle = paste("Total:", sum(agg_dist$count), "bouts across all files"),
          x = "Bout Duration",
          y = "Number of Bouts"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    })

    # Sedentary accumulation curve
    output$accumulation_plot <- renderPlot({
      res <- results()
      req(length(res) > 0)

      # Combine all bouts from all files
      all_bouts <- data.frame()
      for (r in res) {
        if (!is.null(r$fragmentation) && !is.null(r$fragmentation$bouts) && nrow(r$fragmentation$bouts) > 0) {
          b <- r$fragmentation$bouts
          b$subject <- r$subject_id
          all_bouts <- rbind(all_bouts, b)
        }
      }

      if (nrow(all_bouts) == 0) {
        return(NULL)
      }

      # Sort by duration descending
      all_bouts <- all_bouts[order(all_bouts$duration_min, decreasing = TRUE), ]
      total_sed_time <- sum(all_bouts$duration_min)
      all_bouts$cumulative_time <- cumsum(all_bouts$duration_min)
      all_bouts$cumulative_pct <- all_bouts$cumulative_time / total_sed_time * 100
      all_bouts$bout_rank <- seq_len(nrow(all_bouts))
      all_bouts$bout_pct <- all_bouts$bout_rank / nrow(all_bouts) * 100

      # Calculate Gini for display
      n <- nrow(all_bouts)
      x <- sort(all_bouts$duration_min)
      gini <- (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))

      ggplot2::ggplot(all_bouts, ggplot2::aes(x = bout_pct, y = cumulative_pct)) +
        ggplot2::geom_line(color = "#6c757d", linewidth = 1.2) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
        ggplot2::labs(
          title = "Sedentary Time Accumulation",
          subtitle = sprintf("Gini = %.3f (0 = equal bouts, 1 = dominated by long bouts)", gini),
          x = "% of Bouts (longest to shortest)",
          y = "% of Total Sedentary Time"
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 100)) +
        ggplot2::scale_y_continuous(limits = c(0, 100)) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    })

    # Fragmentation summary table
    output$fragmentation_table <- DT::renderDataTable({
      res <- results()
      if (is.null(res) || length(res) == 0) {
        return(DT::datatable(data.frame(Message = "Run 'Score Physical Activity' to see results"), rownames = FALSE))
      }

      df <- data.frame(
        Subject = sapply(res, function(r) r$subject_id),
        `Sedentary (min)` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) round(r$fragmentation$total_sedentary_min) else NA
        }),
        `Total Bouts` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$total_bouts else NA
        }),
        `Mean Bout (min)` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$mean_bout_duration else NA
        }),
        `Median Bout (min)` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$median_bout_duration else NA
        }),
        `Max Bout (min)` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$max_bout_duration else NA
        }),
        `Total Breaks` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$breaks_total else NA
        }),
        `Breaks/Sed Hour` = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$breaks_per_sed_hour else NA
        }),
        Alpha = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$alpha else NA
        }),
        Gini = sapply(res, function(r) {
          if (!is.null(r$fragmentation)) r$fragmentation$gini else NA
        }),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        options = list(pageLength = 15, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE
      ) |> DT::formatStyle(
        columns = names(df),
        backgroundColor = "#f8f9fa"
      )
    })

    output$export_summary <- downloadHandler(
      filename = function() {
        paste0("canhrActi_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        all_rows <- list()
        for (r in res) {
          f <- shared$files[[r$file_id]]
          data <- f$data
          weight <- f$subject_info$weight %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% ""
          epoch_sec <- f$epoch_length

          # Get intensity vector
          intensity <- r$intensity_valid
          n_epochs <- length(intensity)

          # Count intensities
          sedentary <- sum(intensity == "sedentary", na.rm = TRUE)
          light <- sum(intensity == "light", na.rm = TRUE)
          moderate <- sum(intensity == "moderate", na.rm = TRUE)
          vigorous <- sum(intensity == "vigorous", na.rm = TRUE)
          very_vigorous <- sum(intensity == "very_vigorous", na.rm = TRUE)
          total_mvpa <- moderate + vigorous + very_vigorous

          # Percentages
          pct_sed <- if (n_epochs > 0) sprintf("%.2f%%", 100 * sedentary / n_epochs) else "0.00%"
          pct_light <- if (n_epochs > 0) sprintf("%.2f%%", 100 * light / n_epochs) else "0.00%"
          pct_mod <- if (n_epochs > 0) sprintf("%.2f%%", 100 * moderate / n_epochs) else "0.00%"
          pct_vig <- if (n_epochs > 0) sprintf("%.2f%%", 100 * vigorous / n_epochs) else "0.00%"
          pct_vvig <- if (n_epochs > 0) sprintf("%.2f%%", 100 * very_vigorous / n_epochs) else "0.00%"
          pct_mvpa <- if (n_epochs > 0) sprintf("%.2f%%", 100 * total_mvpa / n_epochs) else "0.00%"

          # Axis counts (from original data)
          axis1 <- data$axis1
          axis2 <- if ("axis2" %in% names(data)) data$axis2 else rep(0, nrow(data))
          axis3 <- if ("axis3" %in% names(data)) data$axis3 else rep(0, nrow(data))
          steps <- if ("steps" %in% names(data)) data$steps else rep(0, nrow(data))

          # Vector magnitude
          vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

          row_data <- data.frame(
            Subject = r$subject_id,
            Filename = r$name,
            Epoch = epoch_sec,
            `Weight (lbs)` = weight,
            Age = age,
            Gender = gender,
            Sedentary = sedentary,
            Light = light,
            Moderate = moderate,
            Vigorous = vigorous,
            `Very Vigorous` = very_vigorous,
            `% in Sedentary` = pct_sed,
            `% in Light` = pct_light,
            `% in Moderate` = pct_mod,
            `% in Vigorous` = pct_vig,
            `% in Very Vigorous` = pct_vvig,
            `Total MVPA` = total_mvpa,
            `% in MVPA` = pct_mvpa,
            `Average MVPA Per day` = round(r$mvpa_min, 1),
            `Axis 1 Counts` = sum(axis1, na.rm = TRUE),
            `Axis 2 Counts` = sum(axis2, na.rm = TRUE),
            `Axis 3 Counts` = sum(axis3, na.rm = TRUE),
            `Axis 1 Average Counts` = round(mean(axis1, na.rm = TRUE), 1),
            `Axis 2 Average Counts` = round(mean(axis2, na.rm = TRUE), 1),
            `Axis 3 Average Counts` = round(mean(axis3, na.rm = TRUE), 1),
            `Axis 1 Max Counts` = max(axis1, na.rm = TRUE),
            `Axis 2 Max Counts` = max(axis2, na.rm = TRUE),
            `Axis 3 Max Counts` = max(axis3, na.rm = TRUE),
            `Axis 1 CPM` = round(mean(axis1, na.rm = TRUE) * (60 / epoch_sec), 1),
            `Axis 2 CPM` = round(mean(axis2, na.rm = TRUE) * (60 / epoch_sec), 1),
            `Axis 3 CPM` = round(mean(axis3, na.rm = TRUE) * (60 / epoch_sec), 1),
            `Vector Magnitude Counts` = round(sum(vm, na.rm = TRUE), 1),
            `Vector Magnitude Average Counts` = round(mean(vm, na.rm = TRUE), 1),
            `Vector Magnitude Max Counts` = round(max(vm, na.rm = TRUE), 1),
            `Vector Magnitude CPM` = round(mean(vm, na.rm = TRUE) * (60 / epoch_sec), 1),
            `Steps Counts` = sum(steps, na.rm = TRUE),
            `Steps Average Counts` = round(mean(steps, na.rm = TRUE), 1),
            `Steps Max Counts` = max(steps, na.rm = TRUE),
            `Steps Per Minute` = round(mean(steps, na.rm = TRUE), 1),
            `Lux Average Counts` = 0,
            `Lux Max Counts` = 0,
            `Number of Epochs` = n_epochs,
            Time = n_epochs,
            `Calendar Days` = r$n_days,
            `Average METs` = if (!is.na(r$avg_mets)) round(r$avg_mets, 2) else NA,
            `Total EE (kcal)` = if (!is.na(r$total_ee)) round(r$total_ee) else NA,
            `MVPA Bouts` = r$n_bouts,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          all_rows[[length(all_rows) + 1]] <- row_data
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    output$export_daily <- downloadHandler(
      filename = function() {
        paste0("canhrActi_DailyDetailed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        all_rows <- list()
        for (r in res) {
          f <- shared$files[[r$file_id]]
          data <- f$data
          weight <- f$subject_info$weight %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% ""
          epoch_sec <- f$epoch_length

          # Add date column to data if not present
          if ("timestamp" %in% names(data)) {
            data$date <- as.Date(data$timestamp)
            dates <- unique(data$date)

            for (date_i in dates) {
              day_data <- data[data$date == date_i, ]
              n_epochs <- nrow(day_data)
              if (n_epochs == 0) next

              axis1 <- day_data$axis1
              axis2 <- if ("axis2" %in% names(day_data)) day_data$axis2 else rep(0, n_epochs)
              axis3 <- if ("axis3" %in% names(day_data)) day_data$axis3 else rep(0, n_epochs)
              steps <- if ("steps" %in% names(day_data)) day_data$steps else rep(0, n_epochs)
              vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

              # Convert to CPM and get intensity for this day
              day_cpm <- canhrActi::to_cpm(axis1, epoch_sec)
              day_intensity <- tryCatch({
                if (input$cut_points == "freedson") {
                  canhrActi::freedson(day_cpm)
                } else {
                  canhrActi::CANHR.Cutpoints(day_cpm)
                }
              }, error = function(e) rep("sedentary", n_epochs))

              sedentary <- sum(day_intensity == "sedentary", na.rm = TRUE)
              light <- sum(day_intensity == "light", na.rm = TRUE)
              moderate <- sum(day_intensity == "moderate", na.rm = TRUE)
              vigorous <- sum(day_intensity == "vigorous", na.rm = TRUE)
              very_vigorous <- sum(day_intensity == "very_vigorous", na.rm = TRUE)
              total_mvpa <- moderate + vigorous + very_vigorous

              pct_sed <- sprintf("%.2f%%", 100 * sedentary / n_epochs)
              pct_light <- sprintf("%.2f%%", 100 * light / n_epochs)
              pct_mod <- sprintf("%.2f%%", 100 * moderate / n_epochs)
              pct_vig <- sprintf("%.2f%%", 100 * vigorous / n_epochs)
              pct_vvig <- sprintf("%.2f%%", 100 * very_vigorous / n_epochs)
              pct_mvpa <- sprintf("%.2f%%", 100 * total_mvpa / n_epochs)

              row_data <- data.frame(
                Subject = r$subject_id,
                Filename = r$name,
                Epoch = epoch_sec,
                `Weight (lbs)` = weight,
                Age = age,
                Gender = gender,
                Date = format(as.Date(date_i), "%m/%d/%Y"),
                `Day of Week` = weekdays(as.Date(date_i)),
                `Day of Week Num` = as.numeric(format(as.Date(date_i), "%u")),
                Sedentary = sedentary,
                Light = light,
                Moderate = moderate,
                Vigorous = vigorous,
                `Very Vigorous` = very_vigorous,
                `% in Sedentary` = pct_sed,
                `% in Light` = pct_light,
                `% in Moderate` = pct_mod,
                `% in Vigorous` = pct_vig,
                `% in Very Vigorous` = pct_vvig,
                `Total MVPA` = total_mvpa,
                `% in MVPA` = pct_mvpa,
                `Average MVPA Per Hour` = round(total_mvpa / (n_epochs / 60), 1),
                `Axis 1 Counts` = sum(axis1, na.rm = TRUE),
                `Axis 2 Counts` = sum(axis2, na.rm = TRUE),
                `Axis 3 Counts` = sum(axis3, na.rm = TRUE),
                `Axis 1 Average Counts` = round(mean(axis1, na.rm = TRUE), 1),
                `Axis 2 Average Counts` = round(mean(axis2, na.rm = TRUE), 1),
                `Axis 3 Average Counts` = round(mean(axis3, na.rm = TRUE), 1),
                `Axis 1 Max Counts` = max(axis1, na.rm = TRUE),
                `Axis 2 Max Counts` = max(axis2, na.rm = TRUE),
                `Axis 3 Max Counts` = max(axis3, na.rm = TRUE),
                `Axis 1 CPM` = round(mean(axis1, na.rm = TRUE) * (60 / epoch_sec), 1),
                `Axis 2 CPM` = round(mean(axis2, na.rm = TRUE) * (60 / epoch_sec), 1),
                `Axis 3 CPM` = round(mean(axis3, na.rm = TRUE) * (60 / epoch_sec), 1),
                `Vector Magnitude Counts` = round(sum(vm, na.rm = TRUE), 1),
                `Vector Magnitude Average Counts` = round(mean(vm, na.rm = TRUE), 1),
                `Vector Magnitude Max Counts` = round(max(vm, na.rm = TRUE), 1),
                `Vector Magnitude CPM` = round(mean(vm, na.rm = TRUE) * (60 / epoch_sec), 1),
                `Steps Counts` = sum(steps, na.rm = TRUE),
                `Steps Average Counts` = round(mean(steps, na.rm = TRUE), 1),
                `Steps Max Counts` = max(steps, na.rm = TRUE),
                `Steps Per Minute` = round(mean(steps, na.rm = TRUE), 1),
                `Lux Average Counts` = 0,
                `Lux Max Counts` = 0,
                `Number of Epochs` = n_epochs,
                Time = n_epochs,
                `Calendar Days` = 1,
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
              all_rows[[length(all_rows) + 1]] <- row_data
            }
          }
        }

        if (length(all_rows) == 0) {
          write.csv(data.frame(Message = "No daily data to export"), file, row.names = FALSE)
          return()
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )

    output$export_hourly <- downloadHandler(
      filename = function() {
        paste0("canhrActi_HourlyDetailed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        res <- results()
        if (is.null(res) || length(res) == 0) {
          write.csv(data.frame(Message = "No results to export"), file, row.names = FALSE)
          return()
        }

        all_rows <- list()
        for (r in res) {
          f <- shared$files[[r$file_id]]
          data <- f$data
          weight <- f$subject_info$weight %||% 0
          age <- f$subject_info$age %||% 0
          gender <- f$subject_info$sex %||% ""
          epoch_sec <- f$epoch_length

          if ("timestamp" %in% names(data)) {
            data$date <- as.Date(data$timestamp)
            data$hour_24 <- as.numeric(format(data$timestamp, "%H"))
            data$hour_label <- format(data$timestamp, "%I:00 %p")

            dates <- unique(data$date)

            for (date_i in dates) {
              day_data <- data[data$date == date_i, ]
              hours_present <- unique(day_data$hour_24)

              for (hour_i in hours_present) {
                hour_data <- day_data[day_data$hour_24 == hour_i, ]
                n_epochs <- nrow(hour_data)
                if (n_epochs == 0) next

                hour_label <- hour_data$hour_label[1]

                axis1 <- hour_data$axis1
                axis2 <- if ("axis2" %in% names(hour_data)) hour_data$axis2 else rep(0, n_epochs)
                axis3 <- if ("axis3" %in% names(hour_data)) hour_data$axis3 else rep(0, n_epochs)
                steps <- if ("steps" %in% names(hour_data)) hour_data$steps else rep(0, n_epochs)
                vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

                # Convert to CPM for intensity classification
                hour_cpm <- canhrActi::to_cpm(axis1, epoch_sec)
                hour_intensity <- tryCatch({
                  if (input$cut_points == "freedson") {
                    canhrActi::freedson(hour_cpm)
                  } else {
                    canhrActi::CANHR.Cutpoints(hour_cpm)
                  }
                }, error = function(e) rep("sedentary", n_epochs))

                sedentary <- sum(hour_intensity == "sedentary", na.rm = TRUE)
                light <- sum(hour_intensity == "light", na.rm = TRUE)
                moderate <- sum(hour_intensity == "moderate", na.rm = TRUE)
                vigorous <- sum(hour_intensity == "vigorous", na.rm = TRUE)
                very_vigorous <- sum(hour_intensity == "very_vigorous", na.rm = TRUE)
                total_mvpa <- moderate + vigorous + very_vigorous

                pct_sed <- sprintf("%.2f%%", 100 * sedentary / n_epochs)
                pct_light <- sprintf("%.2f%%", 100 * light / n_epochs)
                pct_mod <- sprintf("%.2f%%", 100 * moderate / n_epochs)
                pct_vig <- sprintf("%.2f%%", 100 * vigorous / n_epochs)
                pct_vvig <- sprintf("%.2f%%", 100 * very_vigorous / n_epochs)
                pct_mvpa <- sprintf("%.2f%%", 100 * total_mvpa / n_epochs)

                row_data <- data.frame(
                  Subject = r$subject_id,
                  Filename = r$name,
                  Epoch = epoch_sec,
                  `Weight (lbs)` = weight,
                  Age = age,
                  Gender = gender,
                  Date = format(as.Date(date_i), "%m/%d/%Y"),
                  Hour = hour_label,
                  `Day of Week` = weekdays(as.Date(date_i)),
                  `Day of Week Num` = as.numeric(format(as.Date(date_i), "%u")),
                  Sedentary = sedentary,
                  Light = light,
                  Moderate = moderate,
                  Vigorous = vigorous,
                  `Very Vigorous` = very_vigorous,
                  `% in Sedentary` = pct_sed,
                  `% in Light` = pct_light,
                  `% in Moderate` = pct_mod,
                  `% in Vigorous` = pct_vig,
                  `% in Very Vigorous` = pct_vvig,
                  `Total MVPA` = total_mvpa,
                  `% in MVPA` = pct_mvpa,
                  `Axis 1 Counts` = sum(axis1, na.rm = TRUE),
                  `Axis 2 Counts` = sum(axis2, na.rm = TRUE),
                  `Axis 3 Counts` = sum(axis3, na.rm = TRUE),
                  `Axis 1 Average Counts` = round(mean(axis1, na.rm = TRUE), 1),
                  `Axis 2 Average Counts` = round(mean(axis2, na.rm = TRUE), 1),
                  `Axis 3 Average Counts` = round(mean(axis3, na.rm = TRUE), 1),
                  `Axis 1 Max Counts` = max(axis1, na.rm = TRUE),
                  `Axis 2 Max Counts` = max(axis2, na.rm = TRUE),
                  `Axis 3 Max Counts` = max(axis3, na.rm = TRUE),
                  `Axis 1 CPM` = round(mean(axis1, na.rm = TRUE) * (60 / epoch_sec), 1),
                  `Axis 2 CPM` = round(mean(axis2, na.rm = TRUE) * (60 / epoch_sec), 1),
                  `Axis 3 CPM` = round(mean(axis3, na.rm = TRUE) * (60 / epoch_sec), 1),
                  `Vector Magnitude Counts` = round(sum(vm, na.rm = TRUE), 1),
                  `Vector Magnitude Average Counts` = round(mean(vm, na.rm = TRUE), 1),
                  `Vector Magnitude Max Counts` = round(max(vm, na.rm = TRUE), 1),
                  `Vector Magnitude CPM` = round(mean(vm, na.rm = TRUE) * (60 / epoch_sec), 1),
                  `Steps Counts` = sum(steps, na.rm = TRUE),
                  `Steps Average Counts` = round(mean(steps, na.rm = TRUE), 1),
                  `Steps Max Counts` = max(steps, na.rm = TRUE),
                  `Steps Per Minute` = round(mean(steps, na.rm = TRUE), 1),
                  `Lux Average Counts` = 0,
                  `Lux Max Counts` = 0,
                  `Number of Epochs` = n_epochs,
                  Time = n_epochs,
                  `Calendar Days` = 1,
                  check.names = FALSE,
                  stringsAsFactors = FALSE
                )
                all_rows[[length(all_rows) + 1]] <- row_data
              }
            }
          }
        }

        if (length(all_rows) == 0) {
          write.csv(data.frame(Message = "No hourly data to export"), file, row.names = FALSE)
          return()
        }

        df <- do.call(rbind, all_rows)
        write.csv(df, file, row.names = FALSE, na = "", quote = TRUE)
      }
    )
  })
}

# Null coalesce
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
