# Module: Visualization
#
# A figure workbench: a toolbar of popovers (Chart, From, Options, Size,
# Interactive, Export) over an artboard that shows the figure at the size the
# fields name, scaled to fit. Helpers live in R/graphing_loaded.R.

mod_graphing_ui <- function(id) {
  ns <- NS(id)

  chart_choices <- lapply(gr_families(), function(f) as.list(f[[2]]))
  names(chart_choices) <- vapply(gr_families(), function(f) f[[1]], character(1))

  tagList(
    tags$div(
      class = "gr-page",

      # The toolbar
      tags$div(class = "gr-bar-out",
        tags$div(class = "gr-panel gr-tbar",
          gr_tb(ns, "charts", "Chart", "tb_chart", 248, uiOutput(ns("pop_charts"))),
          gr_tb(ns, "who", "From", "tb_who", 240, uiOutput(ns("pop_who"))),
          gr_tb(ns, "options", "Options", "tb_options", 248,
                tagList(uiOutput(ns("chart_options")), uiOutput(ns("pop_options_note")))),
          # Presets, then two fields for a size not on the list
          gr_tb(ns, "size", "Size", "tb_size", 300,
                tagList(
                  uiOutput(ns("pop_size")),
                  tags$div(class = "gr-custom",
                    numericInput(ns("plot_width"), "Width", value = 1000,
                                 min = 400, max = 4000, step = 50),
                    numericInput(ns("plot_height"), "Height", value = GR_DEFAULT_H,
                                 min = 400, max = 4000, step = 50)))),

          tags$span(class = "gr-tbar-right",
            tags$span(class = "gr-toggle",
                      checkboxInput(ns("interactive"), "Interactive", value = FALSE)),
            tags$span(class = "gr-tbsep"),
            gr_tb(ns, "export", NULL, "tb_export", 304, uiOutput(ns("pop_export")), right = TRUE),
            actionButton(ns("generate_btn"), "Draw chart", class = run_button_class("gr", FALSE))
          )
        )
      ),

      # The artboard
      tags$div(class = "gr-canvas-out",
        tags$div(class = "gr-canvas",
          uiOutput(ns("stale"), class = "gr-stale-slot"),
          uiOutput(ns("canvas_body"))
        ),
        # The readout under it is written by the page script, which knows how
        # big the canvas ended up
        tags$div(class = "gr-foot",
          tags$span(class = "gr-ro", "—"),
          tags$span(class = "gr-fs", title = "Fullscreen",
                    HTML(paste0('<svg viewBox="0 0 24 24" width="13" height="13" fill="none" ',
                                'stroke="currentColor" stroke-width="2" aria-hidden="true">',
                                '<path d="M3 9V3h6M21 9V3h-6M3 15v6h6M21 15v6h-6"/></svg>')),
                    tags$span("Fullscreen"))
        )
      ),

      # Hidden inputs the popovers drive; everything downstream reads these
      tags$div(class = "gr-hidden",
        selectInput(ns("chart_select"), NULL, choices = chart_choices,
                    selected = "daily_timeline", selectize = FALSE),
        selectInput(ns("selected_file"), NULL,
                    choices = c("All recordings" = "all"), selectize = FALSE)
      )
    ),
    tags$script(HTML(gr_page_script(ns(""))))
  )
}

mod_graphing_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    current_plot <- reactiveVal(NULL)
    current_chart_type <- reactiveVal(NULL)
    # What the figure on screen was drawn from, compared against the live
    # settings for the stale marker
    drawn_sig <- reactiveVal(NULL)
    drawn_who <- reactiveVal(NULL)
    drawn_at <- reactiveVal(NULL)

    # Chart names, from the one table in graphing_loaded.R
    chart_names <- gr_chart_names()

    # Charts that support "All participants" aggregation; the list is in graphing_loaded.R
    all_supported_charts <- gr_all_charts()

    # Update file selection with "All participants" option. Raw recordings go
    # in the same list; updateSelectInput ignores a value that is not among
    # the choices, so a recording absent here can never be picked.
    observe({
      raws <- shared$raw %||% list()
      req(shared$file_count > 0 || length(raws) > 0)
      # With only raw recordings loaded there are no count files, and setNames on NULL errors
      file_choices <- if (length(shared$files) == 0) character(0) else setNames(names(shared$files),
                          sapply(shared$files, function(f) f$subject_info$id %||% f$name))
      raw_choices <- if (length(raws) == 0) character(0) else setNames(
        names(raws), vapply(seq_along(raws), function(i) ovr_name(raws[[i]], names(raws)[i]),
                            character(1)))
      # Add "All participants" at the beginning
      choices <- c("All recordings" = "all", file_choices, raw_choices)
      updateSelectInput(session, "selected_file", choices = choices)
    })

    # Chart-specific options (no title or color palette)
    output$chart_options <- renderUI({
      chart <- input$chart_select %||% "daily_timeline"

      switch(chart,
        "daily_timeline" = tagList(
          checkboxGroupInput(ns("show_axes"), "Show axes",
                             choices = c("Axis 1" = "axis1", "Steps" = "steps", "VM" = "vm"),
                             selected = "axis1", inline = TRUE),
          checkboxInput(ns("show_cutpoints"), "Show cut point lines", value = TRUE)
        ),

        "heatmap" = tagList(
          selectInput(ns("heatmap_metric"), "Metric",
                      choices = c("Axis 1" = "axis1", "Steps" = "steps", "VM" = "vm")),
          checkboxInput(ns("heatmap_weekends"), "Highlight weekends", value = TRUE),
          checkboxInput(ns("heatmap_normalize"), "Normalize by day", value = FALSE)
        ),

        "intensity_pie" = tagList(
          selectInput(ns("pie_cutpoints"), "Cut points", choices = gr_cut_choices_for(shared, input$selected_file)),
          checkboxInput(ns("pie_donut"), "Donut style", value = TRUE),
          checkboxInput(ns("pie_labels"), "Show labels", value = TRUE)
        ),

        # these five read input$pie_cutpoints too
        "intensity_area" = ,
        "bout_histogram" = ,
        "bout_lorenz" = ,
        "bout_survival" = ,
        "sed_timeline" = ,
        "sed_heatmap" = ,
        "sed_occurrence" = ,
        "bout_categories" = ,
        "hourly_bouts" = ,
        "hourly_duration" = ,
        "transition_matrix" = tagList(
          selectInput(ns("pie_cutpoints"), "Cut points", choices = gr_cut_choices_for(shared, input$selected_file))
        ),

        "hypnogram" = tagList(
          checkboxInput(ns("hyp_activity"), "Show activity overlay", value = TRUE),
          checkboxInput(ns("hyp_metrics"), "Show sleep metrics", value = TRUE),
          checkboxInput(ns("hyp_awakenings"), "Mark awakenings", value = TRUE)
        ),

        "polar" = tagList(
          checkboxInput(ns("polar_ribbon"), "Show confidence ribbon", value = TRUE),
          checkboxInput(ns("polar_daytype"), "Separate Weekend/Weekday", value = FALSE),
          checkboxInput(ns("polar_L5M10"), "Show L5/M10 arcs", value = TRUE)
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
        date_choices <- setNames(as.character(dates), fmt_date(dates, "%b %d (%a)"))
        checkboxGroupInput(ns("compare_days"), "Select days",
                           choices = date_choices,
                           selected = date_choices[1:min(3, length(date_choices))])
      }
    })

    # Generate plot
    observeEvent(input$generate_btn, {
      # Read before the guard: what counts as nothing loaded depends on the chart's store
      chart <- input$chart_select %||% "daily_timeline"

      # Blocked only when nothing this chart can be drawn from is loaded: a raw
      # figure needs shared$raw, a Circadian or Sedentary chart takes either
      have_counts <- shared$file_count > 0 && length(shared$files) > 0
      have_raw <- length(shared$raw %||% list()) > 0
      can_draw <- if (gr_is_raw(chart)) have_raw
                  else if (gr_is_dual(chart)) have_counts || have_raw
                  else have_counts
      if (!can_draw) {
        showNotification("Load recordings on the Overview page first.", type = "warning", duration = 4)
        return(invisible(NULL))
      }
      req(input$selected_file)
      sel <- input$selected_file

      # The raw arm: a raw chart is drawn from shared$raw and returns here,
      # since everything below reads shared$files[[sel]], f$data and
      # f$epoch_length, which a raw recording does not have
      if (gr_is_raw(chart)) {
        r <- shared$raw[[sel]]
        if (is.null(r)) {
          showNotification("Pick a raw recording for this chart.", type = "warning", duration = 4)
          return(invisible(NULL))
        }
        p <- withProgress(message = paste("Drawing", chart_names[chart] %||% "chart"),
                          value = 0.4, tryCatch({
          switch(chart,
            "raw_quality"            = canhrActi::plot_raw_quality(r),
            "raw_calibration"        = canhrActi::plot_raw_calibration(r),
            "raw_gaps"               = canhrActi::plot_raw_gaps(r),
            "raw_chunks"             = canhrActi::plot_raw_chunks(r),
            "raw_days"               = canhrActi::plot_raw_days(r),
            "raw_nights"             = canhrActi::plot_raw_nights(r),
            "raw_wear_days"          = canhrActi::plot_raw_wear_days(r),
            "raw_sleep"              = canhrActi::plot_raw_sleep(r),
            "raw_sleep_nights"       = canhrActi::plot_raw_sleep_nights(r),
            "raw_sleep_regularity"   = canhrActi::plot_raw_sleep_regularity(r),
            # Base graphics, so a closure the export calls inside the device;
            # part 4 runs now and is captured by it
            "raw_ggir_sleep"         = {
              nights <- canhrActi::raw.sleep.nights(r)
              if (!is.data.frame(nights) || nrow(nights) == 0) {
                stop("GGIR part 4 found no nights in this recording.", call. = FALSE)
              }
              # nights kept by GGIR's cleaning code, as the Sleep page counts them
              nk <- tryCatch({
                a <- attr(nights, "canhrActi")
                sum(suppressWarnings(as.numeric(a$guiders$cleaningcode)) < 2, na.rm = TRUE)
              }, error = function(e) nrow(nights))
              nk <- max(1L, as.integer(nk))
              # GGIR's height: 96 px of margin, 40 px a night, 46 px of legend
              structure(function() canhrActi::raw.ggir.sleepplot(nights),
                        gr_height = 96 + 40 * nk + 46)
            },
            # GGIR's day panel, drawn from the milestone tree the Activity
            # page's report built
            "raw_ggir_panel"         = {
              rp <- (shared$raw_report %||% list())[[sel]]
              if (is.null(rp)) {
                stop("Run the raw analysis on the Activity page first: this figure ",
                     "is drawn from the report it builds.", call. = FALSE)
              }
              if (!identical(rp$state, "ok") || is.na(rp$dir) || !dir.exists(rp$dir)) {
                stop(switch(rp$state %||% "error",
                            no_ggir = "Install the GGIR package to draw this figure.",
                            paste0("GGIR's report is not available for this recording (",
                                   rp$state %||% "missing", ").")), call. = FALSE)
              }
              d <- rp$dir
              n <- canhrActi::raw.ggir.panelplot(d, count_only = TRUE)
              if (!identical(n$state, "ok") || n$rows == 0) {
                stop(switch(n$state,
                            no_data.table = "Install the data.table package to draw this figure.",
                            ok = "No window in this recording is long enough to draw.",
                            paste("The day panel could not be drawn:", n$state)), call. = FALSE)
              }
              structure(
                function() canhrActi::raw.ggir.panelplot(d),
                gr_aspect = (GGIR_PANEL_ROW_IN * n$height_rows) / GGIR_PANEL_W_IN,
                gr_png_writer = function(file, width_px)
                  canhrActi::raw.ggir.panel.png(d, file, width_px = width_px))
            },
            # Part 5 is scored on the Activity page, not here
            "raw_timeuse_bouts" = ,
            "raw_timeuse_gradient" = {
              tu <- (shared$raw_timeuse %||% list())[[sel]]
              if (is.null(tu)) {
                stop("Run the raw analysis on the Activity page first: these ",
                     "four figures are drawn from its GGIR part 5 result.",
                     call. = FALSE)
              }
              switch(chart,
                "raw_timeuse_bouts"    = canhrActi::plot_raw_timeuse_bouts(tu),
                "raw_timeuse_gradient" = canhrActi::plot_raw_timeuse_gradient(tu))
            },
            stop("Unknown raw chart: ", chart, call. = FALSE))
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 10)
          NULL
        }))

        # A figure that is a list of rows sets its own height from the width;
        # the field is updated so the toolbar still states the export size
        if (!is.null(p)) {
          h <- gr_height_for(p, plot_w())
          if (is.null(h)) set_height(GR_DEFAULT_H) else set_height(h, force = TRUE)
        }

        current_plot(p)
        current_chart_type(chart)
        drawn_sig(live_sig())
        drawn_who(sel)
        drawn_at(Sys.time())
        return(invisible(NULL))
      }

      # Handle "All participants" selection
      if (sel == "all") {
        # Check if chart supports "All participants"
        if (!(chart %in% all_supported_charts)) {
          showNotification(
            paste0("'", chart_names[chart], "' doesn't support All Participants view. Using first participant."),
            type = "warning", duration = 4
          )
          # Fall back to first participant
          sel <- names(shared$files)[1]
        }
      }

      # A Circadian or Sedentary chart drawn from a .gt3x: circ_series_for()
      # builds the frame the handler below reads. bundle stays NULL for
      # counts, which is what the Sedentary arm tests.
      bundle <- NULL
      if (sel != "all" && sel %in% names(shared$raw %||% list())) {
        if (!gr_is_dual(chart)) {
          showNotification(
            paste0("'", chart_names[chart] %||% chart,
                   "' is drawn from a counts recording. Pick one, or choose a Raw recording chart."),
            type = "warning", duration = 5)
          return(invisible(NULL))
        }
        # A chart that applies a cut point takes its metric from the cut
        # point, which names the metric it was validated on
        metric <- if (gr_uses_cuts(chart)) {
          sed_metric_for(shared, sed_resolve_cut(shared, input$pie_cutpoints, "raw"), "raw")
        } else {
          av <- circ_metric_choices(shared, "raw")
          if (length(av) > 0) unname(av[1]) else "raw:ENMO"
        }
        bundle <- circ_series_for(shared, sel,
                                  list(metric = metric,
                                       epoch_length = circ_target_epoch(shared, "raw")))
        if (is.null(bundle)) {
          showNotification("That raw recording has no usable series for this chart.",
                           type = "warning", duration = 5)
          return(invisible(NULL))
        }
        data <- data.frame(timestamp = bundle$timestamps, axis1 = bundle$activity)
        epoch_len <- bundle$epoch_length
        # without the extension; the same stem the export filename uses
        subject_id <- tools::file_path_sans_ext(ovr_name(shared$raw[[sel]], sel))
        chart_title <- paste(chart_names[chart] %||% "Chart", "-", subject_id)

      } else if (sel != "all") {
        # For individual participant or fallback
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

      # The axis label: counts for a counts recording, the bundle's metric for raw
      value_lab <- circ_axis_label(if (!is.null(bundle)) bundle$metric else "axis1")

      # The threshold set for a raw recording, in mg. get_cutpoint_thresholds()
      # falls through to Freedson for a key it does not know, so the numbers,
      # the unit and the label are passed explicitly.
      raw_cuts <- function() {
        if (is.null(bundle)) return(NULL)
        ck <- sed_resolve_cut(shared, input$pie_cutpoints, "raw")
        spec <- sed_cut_spec(shared, ck)
        if (is.null(spec) || !is.finite(spec$light)) return(NULL)
        fin <- function(v) if (is.finite(v)) as.numeric(v) else Inf
        list(thresholds = c(sedentary = fin(spec$light),
                            light     = fin(spec$moderate),
                            moderate  = fin(spec$vigorous),
                            vigorous  = Inf),
             unit = "mg",
             label = sed_cut_label(shared, ck, "raw"),
             intensity = sed_intensity_raw(bundle$activity, spec))
      }
      run_value_lab <- function() {
        m <- tryCatch(shared$results$circadian[[sel]]$parameters$metric, error = function(e) NULL)
        circ_axis_label(m %||% (if (!is.null(bundle)) bundle$metric else "axis1"))
      }

      p <- withProgress(message = paste("Drawing", chart_names[chart] %||% "chart"),
                        value = 0.4, tryCatch({
        # Handle "All participants" for supported charts
        if (sel == "all") {
          switch(chart,
            "intensity_pie" = {
              # Aggregate intensity data across all participants
              total_sedentary <- 0
              total_light <- 0
              total_moderate <- 0
              total_vigorous <- 0
              total_very_vigorous <- 0

              activity_results <- shared$results$activity
              if (!is.null(activity_results) && length(activity_results) > 0) {
                for (r in activity_results) {
                  if (!is.null(r$daily)) {
                    total_sedentary <- total_sedentary + sum(r$daily$sedentary_hrs * 60, na.rm = TRUE)
                    total_light <- total_light + sum(r$daily$light_hrs * 60, na.rm = TRUE)
                    total_moderate <- total_moderate + sum(r$daily$moderate_hrs * 60, na.rm = TRUE)
                    vig_hrs <- if ("vigorous_hrs" %in% names(r$daily)) r$daily$vigorous_hrs else 0
                    total_vigorous <- total_vigorous + sum(vig_hrs * 60, na.rm = TRUE)
                    vvig_hrs <- if ("very_vigorous_hrs" %in% names(r$daily)) r$daily$very_vigorous_hrs else 0
                    total_very_vigorous <- total_very_vigorous + sum(vvig_hrs * 60, na.rm = TRUE)
                  }
                }
              }

              if (total_sedentary + total_light + total_moderate + total_vigorous + total_very_vigorous == 0) {
                showNotification("No activity data. Run Activity Analysis first.", type = "warning")
                return(NULL)
              }

              intensity_minutes <- data.frame(
                intensity = factor(c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
                                   levels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")),
                minutes = c(total_sedentary, total_light, total_moderate, total_vigorous, total_very_vigorous)
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

            # The Activity tab's two cohort charts take a list of per-recording frames
            "intensity_hours" = {
              ar <- shared$results$activity
              if (length(ar) == 0) {
                showNotification("Run the Activity analysis first.", type = "warning")
                return(NULL)
              }
              canhrActi::plot_intensity_hours(lapply(ar, function(x) x$daily),
                                              title = chart_title)
            },
            "hourly_pattern" = {
              ar <- shared$results$activity
              if (length(ar) == 0) {
                showNotification("Run the Activity analysis first.", type = "warning")
                return(NULL)
              }
              canhrActi::plot_hourly_pattern(lapply(ar, function(x) x$hourly),
                                             title = chart_title)
            },

            # Bout charts pooled as the Sedentary tab pools them: the
            # fragmentation object is rebuilt per recording, then the bouts stacked
            "bout_histogram" = ,
            "bout_lorenz" = ,
            "bout_survival" = ,
            "transition_matrix" = ,
            "sed_timeline" = ,
            "sed_heatmap" = ,
            "sed_occurrence" = ,
            "bout_categories" = ,
            "hourly_bouts" = ,
            "hourly_duration" = {
              cp <- input$pie_cutpoints %||% "freedson"
              frs <- lapply(names(shared$files), function(fid) {
                f <- shared$files[[fid]]
                d <- f$data
                if (is.null(d) || !"timestamp" %in% names(d)) return(NULL)
                if (!inherits(d$timestamp, "POSIXct")) d$timestamp <- as.POSIXct(d$timestamp)
                el <- f$epoch_length %||% 60
                cpmv <- canhrActi::to_cpm(d$axis1, el)
                # the set the Options button names; apply_cutpoints() handles all four
                inten <- canhrActi::apply_cutpoints(cpmv, cp)
                wt <- tryCatch(shared$results$wear_time[[fid]]$wear, error = function(e) NULL)
                tryCatch(canhrActi::sedentary.fragmentation(
                  inten, d$timestamp,
                  wear_time = if (!is.null(wt) && length(wt) == nrow(d)) wt else NULL,
                  epoch_length = el), error = function(e) NULL)
              })
              frs <- Filter(Negate(is.null), frs)
              if (length(frs) == 0) {
                showNotification("No recording could be scored for bouts.", type = "warning")
                return(NULL)
              }
              pooled <- do.call(rbind, lapply(frs, function(x) x$bouts))

              # As the Sedentary tab defines them: the distribution shape is
              # re-estimated on the pooled bouts, ASTP is averaged
              pooled_stats <- tryCatch(
                canhrActi::bout.distribution.metrics(pooled$duration_min),
                error = function(e) list(alpha = NA_real_, SATP = NA_real_))
              mean_of <- function(nm) {
                v <- vapply(frs, function(x) {
                  y <- suppressWarnings(as.numeric(x[[nm]])[1])
                  if (length(y) == 1) y else NA_real_
                }, numeric(1))
                mean(v, na.rm = TRUE)
              }

              switch(chart,
                "bout_survival" = canhrActi::plot_survival_curves(pooled$duration_min,
                                                                  title = chart_title),
                "bout_histogram" = canhrActi::plot_bout_histogram_pooled(
                  pooled, alpha = pooled_stats$alpha, title = chart_title),
                "bout_lorenz" = canhrActi::plot_bout_accumulation(pooled, title = chart_title),
                # reads ASTP and SATP, not bouts, so it gets the cohort pair
                "transition_matrix" = canhrActi::plot_transition_matrix(
                  list(ASTP = mean_of("ASTP"), SATP = pooled_stats$SATP),
                  title = chart_title),
                "bout_categories" = canhrActi::plot_bout_categories(frs, title = chart_title),
                "sed_timeline" = canhrActi::plot_sedentary_timeline(pooled, title = chart_title),
                "sed_heatmap" = canhrActi::plot_sedentary_heatmap(pooled, title = chart_title),
                "sed_occurrence" = canhrActi::plot_sedentary_occurrence(pooled, title = chart_title),
                "hourly_bouts" = canhrActi::plot_hourly_bout_frequency(pooled, title = chart_title),
                "hourly_duration" = canhrActi::plot_hourly_bout_duration(pooled, title = chart_title)
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
                show_axes = if (length(input$show_axes)) input$show_axes else "axis1",
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
                  intensity = factor(c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous"),
                                     levels = c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")),
                  minutes = c(
                    activity_data$sedentary_min %||% 0,
                    activity_data$light_min %||% 0,
                    activity_data$moderate_min %||% 0,
                    activity_data$vigorous_min %||% 0,
                    activity_data$very_vigorous_min %||% 0
                  )
                )
                canhrActi::plot_intensity_pie_from_summary(
                  intensity_summary = intensity_minutes,
                  cutpoints = activity_data$parameters$cut_points %||% "freedson",
                  show_labels = input$pie_labels %||% TRUE,
                  title = chart_title
                )
              } else {
                rc <- raw_cuts()
                if (!is.null(rc)) {
                  canhrActi::plot_intensity_pie(
                    data = data, intensity = rc$intensity,
                    thresholds = rc$thresholds, unit = rc$unit,
                    cutpoint_label = rc$label,
                    epoch_length = epoch_len,
                    show_labels = input$pie_labels %||% TRUE,
                    donut_style = input$pie_donut %||% TRUE,
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
              }
            },

            "intensity_area" = {
              rc <- raw_cuts()
              if (!is.null(rc)) {
                canhrActi::plot_intensity_area(
                  data = data, intensity = rc$intensity,
                  thresholds = rc$thresholds, unit = rc$unit,
                  cutpoint_label = rc$label,
                  epoch_length = epoch_len,
                  title = chart_title
                )
              } else {
                canhrActi::plot_intensity_area(
                  data = data,
                  cutpoints = input$pie_cutpoints %||% "freedson",
                  epoch_length = epoch_len,
                  title = chart_title
                )
              }
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
                value_label = value_lab,
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
              value_label = value_lab,
              title = chart_title
            )
          },

          "actogram" = {
            if (!inherits(data$timestamp, "POSIXct")) data$timestamp <- as.POSIXct(data$timestamp)
            wt <- tryCatch(shared$results$wear_time[[sel]]$wear, error = function(e) NULL)
            cd <- shared$results$circadian[[sel]]
            ss <- tryCatch(shared$results$sleep[[sel]]$sleep_state, error = function(e) NULL)
            canhrActi::plot_actogram(
              data$axis1, data$timestamp, epoch_length = epoch_len,
              wear_time = if (!is.null(wt) && length(wt) == nrow(data)) wt else NULL,
              L5_onset = if (!is.null(cd)) cd$L5_start else NULL,
              M10_onset = if (!is.null(cd)) cd$M10_start else NULL,
              sleep_mask = if (!is.null(ss) && length(ss) == nrow(data)) ss else NULL,
              scale = "sqrt"
            )
          },

          "periodogram" = {
            if (!inherits(data$timestamp, "POSIXct")) data$timestamp <- as.POSIXct(data$timestamp)
            canhrActi::plot_periodogram(data$axis1, data$timestamp)
          },

          "chisq" = {
            if (!inherits(data$timestamp, "POSIXct")) data$timestamp <- as.POSIXct(data$timestamp)
            canhrActi::plot_chisq(data$axis1, data$timestamp, epoch_length = epoch_len)
          },

          "extcosinor" = {
            if (!inherits(data$timestamp, "POSIXct")) data$timestamp <- as.POSIXct(data$timestamp)
            canhrActi::plot_extended_cosinor(data$axis1, data$timestamp)
          },

          # The joint amplitude-acrophase confidence region behind cosinor_rhythm_detected
          "cosinor_ellipse" = {
            if (!inherits(data$timestamp, "POSIXct")) data$timestamp <- as.POSIXct(data$timestamp)
            canhrActi::plot_cosinor_ellipse(data$axis1, data$timestamp)
          },

          "dfa" = {
            canhrActi::plot_dfa(data$axis1)
          },

          # The Circadian tab's own two, read from that tab's stored result
          "circ_profile" = {
            cd <- shared$results$circadian[[sel]]
            if (is.null(cd) || is.null(cd$hourly_profile)) {
              showNotification("Run the Circadian analysis first.", type = "warning")
              return(NULL)
            }
            canhrActi::plot_circadian_profile(
              cd$hourly_profile,
              L5_start = cd$L5_start_hour %||% cd$L5_start,
              M10_start = cd$M10_start_hour %||% cd$M10_start,
              title = chart_title,
              value_label = run_value_lab())
          },

          "cosinor_fit" = {
            cd <- shared$results$circadian[[sel]]
            if (is.null(cd) || is.null(cd$hourly_profile)) {
              showNotification("Run the Circadian analysis first.", type = "warning")
              return(NULL)
            }
            canhrActi::plot_cosinor_fit(
              hourly_profile = cd$hourly_profile,
              mesor = cd$mesor, amplitude = cd$amplitude,
              acrophase = cd$acrophase, r_squared = cd$r_squared,
              acrophase_time = cd$acrophase_time,
              title = chart_title,
              value_label = run_value_lab())
          },

          # The Activity tab's own two, from that tab's per-day and per-hour frames
          "intensity_hours" = {
            ar <- shared$results$activity
            if (length(ar) == 0) {
              showNotification("Run the Activity analysis first.", type = "warning")
              return(NULL)
            }
            if (!identical(sel, "all")) ar <- ar[intersect(sel, names(ar))]
            canhrActi::plot_intensity_hours(
              lapply(ar, function(x) x$daily), title = chart_title)
          },

          "hourly_pattern" = {
            ar <- shared$results$activity
            if (length(ar) == 0) {
              showNotification("Run the Activity analysis first.", type = "warning")
              return(NULL)
            }
            if (!identical(sel, "all")) ar <- ar[intersect(sel, names(ar))]
            canhrActi::plot_hourly_pattern(
              lapply(ar, function(x) x$hourly), title = chart_title)
          },

          "bout_histogram" = ,
          "bout_lorenz" = ,
          "bout_survival" = ,
          "sed_timeline" = ,
          "sed_heatmap" = ,
          "sed_occurrence" = ,
          "bout_categories" = ,
          "hourly_bouts" = ,
          "hourly_duration" = ,
          "transition_matrix" = {
            if (!inherits(data$timestamp, "POSIXct")) data$timestamp <- as.POSIXct(data$timestamp)
            # A raw recording is classified by the published raw cut point
            # through the Sedentary tab's helper, with the wear mask from the bundle
            if (!is.null(bundle)) {
              cut_key <- sed_resolve_cut(shared, input$pie_cutpoints, "raw")
              b <- sed_prepare(shared, bundle, cut_key, "raw")
              if (!isTRUE(b$ok) || is.null(b$intensity)) {
                stop(b$note %||% "No published cut point fits this recording's metrics.",
                     call. = FALSE)
              }
              intensity <- b$intensity
              wt <- b$wear_time
            } else {
              cp <- input$pie_cutpoints %||% "freedson"
              cpm <- canhrActi::to_cpm(data$axis1, if (is.null(epoch_len)) 60 else epoch_len)
              intensity <- canhrActi::apply_cutpoints(cpm, cp)
              wt <- tryCatch(shared$results$wear_time[[sel]]$wear, error = function(e) NULL)
            }
            fr <- canhrActi::sedentary.fragmentation(
              intensity, data$timestamp,
              wear_time = if (!is.null(wt) && length(wt) == nrow(data)) wt else NULL,
              epoch_length = epoch_len
            )
            p_sed <- switch(chart,
              "bout_survival" = canhrActi::plot_survival_curves(fr$bouts$duration_min,
                                                                title = chart_title),
              "bout_histogram" = canhrActi::plot_bout_histogram(fr, title = chart_title),
              "bout_lorenz" = canhrActi::plot_bout_lorenz(fr, title = chart_title),
              "transition_matrix" = canhrActi::plot_transition_matrix(fr, title = chart_title),
              # The Sedentary tab's six, from the same fragmentation object
              "sed_timeline" = canhrActi::plot_sedentary_timeline(fr$bouts, title = chart_title),
              "sed_heatmap" = canhrActi::plot_sedentary_heatmap(fr$bouts, title = chart_title),
              "sed_occurrence" = canhrActi::plot_sedentary_occurrence(fr$bouts, title = chart_title),
              "bout_categories" = canhrActi::plot_bout_categories(fr, title = chart_title),
              "hourly_bouts" = canhrActi::plot_hourly_bout_frequency(fr$bouts, title = chart_title),
              "hourly_duration" = canhrActi::plot_hourly_bout_duration(fr$bouts, title = chart_title)
            )
            # The cut point goes in the caption, so an exported figure records
            # what produced it
            sed_cap <- if (!is.null(bundle)) {
              sed_cut_label(shared, sed_resolve_cut(shared, input$pie_cutpoints, "raw"), "raw")
            } else {
              sed_cut_label(shared, input$pie_cutpoints, "counts")
            }
            if (inherits(p_sed, "ggplot") || inherits(p_sed, "gg")) {
              p_sed <- p_sed + ggplot2::labs(caption = paste("Sedentary:", sed_cap))
            }
            p_sed
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
      }))

      # A chart drawn one row per day starts tall enough for its days
      if (!is.null(p)) {
        dh <- if (identical(sel, "all")) NULL else gr_day_height(chart, data$timestamp)
        set_height(dh %||% GR_DEFAULT_H)
      }

      current_plot(p)
      current_chart_type(chart)
      drawn_sig(live_sig())
      drawn_who(sel)
      drawn_at(Sys.time())
    })

    # The toolbar; each button shows its own value when closed
    cur_chart <- reactive(input$chart_select %||% "daily_timeline")
    cur_file <- reactive(input$selected_file %||% "all")
    # A typed field can be empty or nonsense mid-edit
    px <- function(v, fallback) {
      v <- suppressWarnings(as.integer(v))
      if (length(v) != 1L || is.na(v)) return(fallback)
      max(400L, min(4000L, v))
    }
    plot_w <- reactive(px(input$plot_width, 1000L))
    plot_h <- reactive(px(input$plot_height, GR_DEFAULT_H))

    # The height the page last set by itself. A figure that sizes itself
    # always takes its own; otherwise the page's height follows the chart
    # until the user picks one.
    auto_h <- reactiveVal(GR_DEFAULT_H)
    set_height <- function(h, force = FALSE) {
      h <- as.integer(h)
      if (!force && !identical(plot_h(), auto_h())) return(invisible())
      if (!identical(h, plot_h())) updateNumericInput(session, "plot_height", value = h)
      auto_h(h)
    }

    # Every PNG is 300 dpi, which is what a journal asks for
    PNG_DPI <- 300L

    who_label <- reactive({
      sel <- cur_file()
      if (identical(sel, "all")) {
        paste("All", pluralize(shared$file_count, "recording"))
      } else if (!is.null(shared$files[[sel]])) {
        shared$files[[sel]]$subject_info$id %||% shared$files[[sel]]$name
      } else if (!is.null(shared$raw[[sel]])) {
        ovr_name(shared$raw[[sel]], sel)
      } else {
        paste("All", pluralize(shared$file_count, "recording"))
      }
    })

    output$tb_chart <- renderUI({
      lab <- unname(chart_names[cur_chart()])
      tags$span(class = "gr-tb-v", if (is.na(lab)) "Chart" else lab)
    })
    output$tb_who <- renderUI(tags$span(class = "gr-tb-v", who_label()))
    output$tb_size <- renderUI(tags$span(class = "gr-tb-v", sprintf("%d × %d", plot_w(), plot_h())))
    output$tb_export <- renderUI(tags$span(class = "gr-tb-v", "Export"))
    output$tb_options <- renderUI({
      chart <- cur_chart()
      if (!gr_has_options(chart)) return(tags$span(class = "gr-tb-v is-muted", "None"))
      n <- gr_option_count(chart, input)
      # When the cut point set is the only option, name it rather than count it
      if (gr_uses_cuts(chart) && n == 0) {
        return(tags$span(class = "gr-tb-v",
                         gr_cut_label(input$pie_cutpoints, shared, cur_file())))
      }
      tags$span(class = "gr-pill", n)
    })

    output$pop_charts <- renderUI(gr_chart_menu(cur_chart()))
    output$pop_who <- renderUI({
      raws <- shared$raw %||% list()
      if (shared$file_count == 0 && length(raws) == 0) {
        return(tags$div(class = "gr-pm-note", "No recordings loaded."))
      }
      gr_who_menu(shared$files, raws, cur_file(), cur_chart() %in% gr_all_charts())
    })
    output$pop_options_note <- renderUI({
      if (gr_has_options(cur_chart())) return(NULL)
      tags$div(class = "gr-pm-note", "This chart has no options.")
    })
    output$pop_size <- renderUI({
      # the percentage is filled in by the page script
      tags$div(gr_size_extras(plot_w(), plot_h(), PNG_DPI))
    })
    output$pop_export <- renderUI(gr_export_menu(ns, fig_stem()))

    # The popovers report chart and recording as events
    observeEvent(input$chart_pick, {
      updateSelectInput(session, "chart_select", selected = input$chart_pick)
    })

    # The recordings a chart can be drawn from
    draw_ids <- function(chart) {
      if (gr_is_raw(chart)) names(shared$raw %||% list())
      else if (gr_is_dual(chart)) c(names(shared$files), names(shared$raw %||% list()))
      else names(shared$files)
    }

    # Move the recording chooser when the chart cannot draw the current
    # choice, so the From button is right before the draw
    observe({
      chart <- cur_chart()
      sel <- cur_file()
      ids <- draw_ids(chart)
      ok <- if (identical(sel, "all")) {
        # "all" pools shared$files, which no raw chart can use.
        !gr_is_raw(chart) && chart %in% gr_all_charts()
      } else {
        sel %in% ids
      }
      if (ok) return()
      if (length(ids)) updateSelectInput(session, "selected_file", selected = ids[[1]])
    })

    observeEvent(input$who_pick, {
      updateSelectInput(session, "selected_file", selected = input$who_pick)
      # One recording follows the user to the other tabs; All does not
      if (input$who_pick %in% draw_ids(cur_chart())) focus_set(shared, input$who_pick)
    })

    # The recording picked on another tab, when this chart can draw it
    on_tab_shown(shared, "graphing", function() {
      id <- focus_get(shared, among = draw_ids(cur_chart()))
      if (!is.null(id) && !identical(id, cur_file())) {
        updateSelectInput(session, "selected_file", selected = id)
      }
    })

    observeEvent(input$size_pick, {
      wh <- strsplit(input$size_pick, "x", fixed = TRUE)[[1]]
      updateNumericInput(session, "plot_width", value = as.integer(wh[1]))
      updateNumericInput(session, "plot_height", value = as.integer(wh[2]))
    })

    # Draw state. Size and Interactive re-render the figure as they change,
    # so they are left out of the signature.
    live_sig <- reactive({
      chart <- cur_chart()
      spec <- gr_option_inputs()[[chart]]
      opts <- if (is.null(spec)) NULL else {
        c(lapply(spec$groups, function(g) input[[g]]),
          lapply(spec$checks, function(k) input[[k]]),
          lapply(c("heatmap_metric", "pie_cutpoints"), function(s) input[[s]]))
      }
      # A chart drawn from the Activity page's part 5 goes stale when that
      # store is rebuilt, so its settings are part of the signature. Keyed on
      # the thresholds the results carry, not on the count or the ids, which
      # a re-analysis at another cut point leaves unchanged.
      ext <- if (gr_is_raw(chart)) {
        tu <- shared$raw_timeuse %||% list()
        paste(c(length(tu), names(tu),
                vapply(tu, function(x) {
                  s <- x$settings
                  paste(c(s$acc.metric, s$cutpoint, s$threshold.lig,
                          s$threshold.mod, s$threshold.vig,
                          x$status$state), collapse = ",")
                }, character(1))), collapse = "|")
      } else NULL
      paste(c(chart, cur_file(), unlist(lapply(opts, function(v) paste(v, collapse = ","))), ext),
            collapse = "|")
    })
    is_stale <- reactive(!is.null(drawn_sig()) && !identical(drawn_sig(), live_sig()))

    observe({
      drawn <- !is.null(current_plot())
      label <- if (!drawn) "Draw chart" else "Redraw"
      updateActionButton(session, "generate_btn", label = label)
      cls <- strsplit(run_button_class("gr", drawn, is_stale()), " ", fixed = TRUE)[[1]]
      for (k in c("gr-btn--go", "gr-btn--quiet", "is-stale")) {
        shinyjs::toggleClass(id = "generate_btn", class = k, condition = k %in% cls)
      }
    })

    output$stale <- renderUI({
      if (!is_stale()) return(NULL)
      tags$span(class = "gr-stale", tags$i(), "Options changed since this was drawn")
    })

    # The artboard. The figure is rendered at the size the fields name and
    # the page script scales it to the canvas; fit_scale is that ratio,
    # debounced so a window drag does not queue a render per frame.
    fit_scale <- reactive({
      v <- suppressWarnings(as.numeric(input$fit_scale))
      if (length(v) != 1 || is.na(v) || v <= 0) 1 else min(1, v)
    })
    fit_scale_d <- debounce(fit_scale, 300)

    live_chart <- reactive({
      ch <- current_chart_type()
      static_only <- c("polar", "activity_clock", "intensity_pie", "hypnogram", "actogram")
      # Every raw figure is static: ggplotly mangles the dense faceted panels
      # and cannot take base graphics at all
      isTRUE(input$interactive) && requireNamespace("plotly", quietly = TRUE) &&
        !is.null(ch) && !(ch %in% static_only) && !gr_is_raw(ch)
    })

    output$canvas_body <- renderUI({
      if (shared$file_count == 0 && length(shared$raw %||% list()) == 0) {
        return(gr_empty("No recordings loaded",
                        "Add files on the Overview page, then come back here."))
      }
      if (is.null(current_plot())) {
        return(gr_empty("No chart yet",
                        "Pick a chart and a recording, then press Draw chart."))
      }
      gr_sheet(ns, plot_w(), plot_h(), live_chart())
    })


    # Interactive (plotly) version, only wired when plotly is available.
    if (requireNamespace("plotly", quietly = TRUE)) {
      output$main_plotly <- plotly::renderPlotly({
        p <- current_plot()
        req(inherits(p, "ggplot"))
        tryCatch(suppressWarnings(plotly::ggplotly(p)),
                 error = function(e) plotly::ggplotly(ggplot2::ggplot()))
      })
    }

    # The preview is drawn at exp_w() x exp_h() inches at 96 * scale dpi, the
    # export call with a smaller dpi, rather than shrunk with CSS, which
    # resamples one-pixel gridlines away.
    output$main_plot <- renderImage({
      p <- current_plot()
      req(!is.null(p))
      s <- fit_scale_d()
      f <- tempfile(fileext = ".png")
      safe_export_plot(f, p, grDevices::png, exp_w(), exp_h(), dpi = 96 * s)
      list(src = f, contentType = "image/png", alt = "",
           width = round(plot_w() * s), height = round(plot_h() * s))
    }, deleteFile = TRUE)

    # Helper function to create error placeholder plot
    create_error_plot <- function(message) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                          size = 6, color = "#dc3545", fontface = "bold") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))
    }

    # Helper function to safely export plot to file
    #
    # One function serves the artboard and all three downloads. A ggplot is
    # printed into the device. Base graphics draws immediately, so it arrives
    # as a zero-argument closure and is called inside the device; printing it
    # would leave a blank png that looks like a successful export.
    safe_export_plot <- function(file, plot_obj, device_func, width_in, height_in, dpi = NULL, format_name = "image") {
      p <- if (is.null(plot_obj)) {
        create_error_plot("No chart generated.\nPlease generate a chart first.")
      } else {
        plot_obj
      }

      # A drawing may write its own png. GGIR's day panel does, through a
      # pdf, since a raster device drops a bout narrower than a pixel; the
      # vector formats take the branch below.
      pw <- gr_png_writer_of(p)
      if (!is.null(pw) && identical(device_func, grDevices::png)) {
        ok <- tryCatch({
          out <- pw(file, round(width_in * (dpi %||% 96)))
          identical(out$state, "ok") && file.exists(file) && file.info(file)$size > 0
        }, error = function(e) FALSE)
        if (ok) return(invisible(TRUE))
        # fall through to the placeholder below rather than leaving a stub
      }

      result <- if (!is.null(pw) && identical(device_func, grDevices::png)) FALSE else tryCatch({
        if (!is.null(dpi)) {
          device_func(file, width = width_in, height = height_in, units = "in", res = dpi, bg = "white")
        } else {
          device_func(file, width = width_in, height = height_in, bg = "white")
        }
        if (is.function(p)) p() else print(p)
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


    # Export size in inches, at the browser's 96 px to the inch
    exp_w <- reactive(plot_w() / 96)
    exp_h <- reactive(plot_h() / 96)

    # The figure's file name without an extension; the menu shows it and
    # every download builds on it
    fig_stem <- reactive({
      if (is.null(current_plot())) return(NULL)
      key <- current_chart_type() %||% "chart"
      lab <- unname(chart_names[key])
      chart <- gsub("[^A-Za-z0-9]+", "", if (is.na(lab)) key else lab)
      who <- drawn_who() %||% "all"
      who <- if (identical(who, "all")) {
        "AllRecordings"
      } else if (!is.null(shared$raw[[who]])) {
        # the recording's name, not its internal id
        gsub("[^A-Za-z0-9]+", "",
             tools::file_path_sans_ext(ovr_name(shared$raw[[who]], who)))
      } else {
        gsub("[^A-Za-z0-9]+", "", shared$files[[who]]$subject_info$id %||% who)
      }
      paste0("canhrActi_", chart, "_", who, "_",
             format(drawn_at() %||% Sys.time(), "%Y-%m-%d_%H%M%S"))
    })
    fig_name <- function(ext) paste0(fig_stem() %||% "canhrActi_chart", ".", ext)

    output$download_png <- downloadHandler(
      filename = function() {
        fig_name("png")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::png, exp_w(), exp_h(), dpi = PNG_DPI)
      },
      contentType = "image/png"
    )
    output$download_pdf <- downloadHandler(
      filename = function() {
        fig_name("pdf")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::pdf, exp_w(), exp_h())
      },
      contentType = "application/pdf"
    )

    output$download_svg <- downloadHandler(
      filename = function() {
        fig_name("svg")
      },
      content = function(file) {
        safe_export_plot(file, current_plot(), grDevices::svg, exp_w(), exp_h())
      },
      contentType = "image/svg+xml"
    )


    # The download links sit in a closed popover, and a suspended download
    # handler never receives its href. Last, because outputOptions only knows
    # outputs already defined.
    outputOptions(output, "main_plot", suspendWhenHidden = FALSE)

    for (dl in c("download_png", "download_pdf", "download_svg",
                 "pop_charts", "pop_who", "pop_options_note", "pop_size",
                 "pop_export", "chart_options")) {
      outputOptions(output, dl, suspendWhenHidden = FALSE)
    }
  })
}
