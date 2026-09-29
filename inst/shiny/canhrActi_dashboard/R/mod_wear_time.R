# Module: Wear time
# Rule bar, figures, a table of recordings with every day of each in its Days
# column, and the selected recording docked underneath, hour by hour.

mod_wear_time_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "wt-page",
      uiOutput(ns("rule"), class = "wt-out"),
      tags$div(id = ns("settings_panel"), class = "wt-settings", style = "display: none;",
               wt_settings_panel(ns)),
      uiOutput(ns("figures"), class = "wt-out"),
      uiOutput(ns("list_panel"), class = "wt-out wt-list-out"),
      uiOutput(ns("detail_panel"), class = "wt-out wt-detail-out"),

      # Shown while a run is going
      tags$div(id = ns("processing_indicator"), class = "wt-busy", style = "display: none;",
               tags$span(class = "wt-spinner", `aria-hidden` = "true"),
               tags$span(id = ns("processing_status"), "Validating"))
    ),
    tags$script(HTML(wt_page_script(ns(""))))
  )
}

# Settings panel: every parameter the run reads, with its unit and a note.
# A field that does not apply to the chosen algorithm is disabled.
wt_settings_panel <- function(ns) {
  field <- function(label, control, note, id = NULL) {
    tags$div(class = "wt-field",
      tags$div(class = "wt-field-k", label),
      control,
      tags$div(class = "wt-field-n", id = id, note))
  }

  # A number and its unit share one box
  num <- function(id, value, unit, ..., placeholder = NULL) {
    box <- numericInput(ns(id), NULL, value = value, width = "100%", ...)
    if (!is.null(placeholder)) box <- tagAppendAttributes(box, placeholder = placeholder, .cssSelector = "input")
    tags$div(class = "wt-num", box,
      tags$span(class = "wt-unit", `aria-hidden` = "true", unit))
  }

  check <- function(id, label, value = FALSE) {
    tags$div(class = "wt-check", checkboxInput(ns(id), label, value = value))
  }

  tags$div(
    class = "wt-panel wt-settings-grid",
    tags$div(
      class = "wt-fields",

      # selectize = FALSE: a native select matches the boxes beside it
      field("Algorithm",
            tags$div(class = "wt-select",
              selectInput(ns("algorithm"), NULL, width = "100%", selected = "choi", selectize = FALSE,
                          choices = c("Choi (2011)" = "choi", "Troiano (2008)" = "troiano",
                                      "CANHR (2025)" = "canhr"))),
            "how zeros become non-wear"),
      field("Non-wear window", num("min_length", 90, "min", min = 30, max = 180),
            "zeros that count as off"),
      field("Spike tolerance", num("spike_tolerance", 2, "min", min = 0, max = 10),
            "movement allowed inside a gap"),
      # Choi (2011) and ActiLife have no stop level, so Choi starts blank
      field("Spike stop level", num("spike_stoplevel", NULL, "counts", min = 0, max = 500, placeholder = "none"),
            "above this a spike ends the gap"),

      field("Small window", num("small_window", 30, "min", min = 10, max = 60),
            "Choi and CANHR", id = ns("note_small")),
      field("Counts", check("use_vm", "Vector magnitude"), "otherwise axis 1"),
      field("A day counts at", num("min_wear_day", 600, "min", min = 0, max = 1440),
            "10 h worn", id = ns("note_day")),
      field("A subject counts at", num("min_valid_days", 3, "days", min = 0, max = 14),
            "valid days needed"),

      field("Weekdays required", num("min_weekdays", 0, "days", min = 0, max = 5),
            "0 for no weekday rule"),
      field("Weekend days required", num("min_weekend", 0, "days", min = 0, max = 2),
            "0 for no weekend rule"),
      field("Ignore short wear", check("use_ignore_short", "Treat as non-wear"),
            "a knock inside a long gap is not wear"),
      field("Shorter than", num("ignore_short_min", 30, "min", min = 0, max = 60),
            "while that box is ticked", id = ns("note_short"))
    ),
    tags$div(
      class = "wt-settings-foot",
      actionButton(ns("load_defaults"), "Reset to the algorithm's defaults", class = "wt-btn wt-btn--text"),
      tags$span(class = "wt-spacer"),
      actionButton(ns("clear_results"), "Clear results", class = "wt-btn wt-btn--text is-destructive")
    )
  )
}

# Page script: row selection, sorting and the filter chips
wt_page_script <- function(ns_prefix) {
  js <- "
(function () {
  var NS = '__NS__';
  function setVal(name, value) { Shiny.setInputValue(NS + name, value, { priority: 'event' }); }

  document.addEventListener('click', function (e) {
    if (!e.target.closest || !e.target.closest('.wt-page')) return;

    // the raw interface's own controls: its table tabs, chart chips and rows.
    // Checked first because they carry their own data attributes and would
    // otherwise fall through to the counts handlers below.
    var wtab = e.target.closest('.wt-page [data-weartab]');
    if (wtab) { e.preventDefault(); setVal('weartab_set', wtab.dataset.weartab); return; }
    var wchart = e.target.closest('.wt-page [data-wearchart]');
    if (wchart) { e.preventDefault(); setVal('wearchart_set', wchart.dataset.wearchart); return; }
    var rfile = e.target.closest('.wt-page [data-rawfile]');
    if (rfile) { e.preventDefault(); setVal('rawfile_set', rfile.dataset.rawfile); return; }
    var rsort = e.target.closest('.wt-page [data-rawsort]');
    if (rsort) { e.preventDefault(); setVal('rawsort_set', rsort.dataset.rawsort); return; }

    var row = e.target.closest('.wt-row[data-fid]');
    if (row) {
      document.querySelectorAll('.wt-page .wt-row.is-selected').forEach(function (r) { r.classList.remove('is-selected'); });
      row.classList.add('is-selected');
      setVal('pick', row.dataset.fid);
      return;
    }
    var head = e.target.closest('.wt-page [data-sort]');
    if (head) { setVal('sort', head.dataset.sort); return; }
    var chip = e.target.closest('.wt-page [data-chip]');
    if (chip) { setVal('chip', chip.dataset.chip); return; }
  });

  document.addEventListener('keydown', function (e) {
    if (!e.target.closest || !e.target.closest('.wt-page')) return;
    var row = e.target.closest('.wt-row[data-fid]');
    if (!row) return;
    var rows = Array.prototype.slice.call(document.querySelectorAll('.wt-page .wt-row[data-fid]'));
    var i = rows.indexOf(row);
    var next = null;
    if (e.key === 'ArrowDown') next = rows[Math.min(i + 1, rows.length - 1)];
    else if (e.key === 'ArrowUp') next = rows[Math.max(i - 1, 0)];
    else if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); row.click(); return; }
    if (next) { e.preventDefault(); next.focus(); next.click(); }
  });
})();
"
  sub("__NS__", ns_prefix, js, fixed = TRUE)
}

mod_wear_time_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Raw interface. GGIR decided wear while reading, so there is no algorithm
    # to re-run. Drawing is in wear_raw.R; this is the state and the routing.
    raw_ids <- reactive(names(shared$raw %||% list()))
    raw_list <- reactive(shared$raw %||% list())
    has_raw <- reactive(length(raw_ids()) > 0)
    has_counts <- reactive(length(shared$files) > 0)
    view <- reactive({
      if (!has_raw()) return("counts")
      if (!has_counts()) return("raw")
      if (identical(local_raw$view, "raw")) "raw" else "counts"
    })
    local_raw <- reactiveValues(view = "raw", tab = "summary", sel = NULL, chart = "days", sort = NULL, dir = 1)

    observeEvent(input$weartab_set, {
      if (input$weartab_set %in% c("summary", "day")) local_raw$tab <- input$weartab_set
      # The two tabs do not share a column set, so a sort does not carry over.
      local_raw$sort <- NULL; local_raw$dir <- 1
    })
    observeEvent(input$wearchart_set, {
      if (input$wearchart_set %in% names(WTR_CHARTS)) local_raw$chart <- input$wearchart_set
    })
    # Click a header to sort, click it again to reverse
    observeEvent(input$rawsort_set, {
      i <- suppressWarnings(as.integer(input$rawsort_set))
      if (length(i) != 1 || is.na(i)) return()
      if (identical(local_raw$sort, i)) local_raw$dir <- -local_raw$dir
      else { local_raw$sort <- i; local_raw$dir <- 1 }
    }, ignoreInit = TRUE)

    # The select box and a row click both land here
    observeEvent(input$rawfile_set, {
      v <- input$rawfile_set
      local_raw$sel <- if (identical(v, "all") || !v %in% raw_ids()) NULL else v
      focus_set(shared, local_raw$sel)
    })
    observeEvent(input$rawwho_set, {
      v <- input$rawwho_set
      local_raw$sel <- if (identical(v, "all") || !v %in% raw_ids()) NULL else v
      focus_set(shared, local_raw$sel)
    }, ignoreInit = TRUE)
    # Drop a recording removed on the Overview
    observeEvent(raw_ids(), {
      if (!is.null(local_raw$sel) && !local_raw$sel %in% raw_ids()) local_raw$sel <- NULL
    })

    # Drawn only when both kinds are loaded
    raw_switch <- function() {
      if (!(has_counts() && has_raw())) return(NULL)
      v <- view()
      seg <- function(key, label, n) {
        # A hand-built button with only .action-button, so Shiny binds it but
        # the global a:not(.btn) and .content-wrapper .btn-default !important
        # rules do not reach it
        tags$button(id = ns(paste0("wear_view_", key)), type = "button",
                    class = paste("action-button ovr-seg-b",
                                  if (identical(v, key)) "is-on" else ""),
                    label, tags$b(fmt_int(n)))
      }
      tags$div(class = "ovr-seg", role = "tablist",
               seg("counts", "Counts", length(shared$files)),
               seg("raw", "Raw", length(raw_ids())))
    }
    observeEvent(input$wear_view_counts, local_raw$view <- "counts")
    observeEvent(input$wear_view_raw, local_raw$view <- "raw")

    results <- reactiveVal(list())

    # Drop the results of recordings removed on Overview
    observeEvent(names(shared$files), {
      res <- results()
      if (!length(res)) return()
      keep <- intersect(names(res), names(shared$files))
      if (length(keep) == length(res)) return()
      results(res[keep])
      shared$results$wear_time <- res[keep]
    }, ignoreNULL = FALSE)

    # Toggle advanced settings panel
    observeEvent(input$toggle_advanced, {
      shinyjs::toggle("settings_panel")
    })

    # Load default parameters based on algorithm
    observeEvent(input$algorithm, {
      load_algorithm_defaults()
    })

    observeEvent(input$load_defaults, {
      load_algorithm_defaults()
    })

    # Troiano has no small window, so the field is disabled
    observeEvent(input$algorithm, {
      choi_like <- !identical(input$algorithm, "troiano")
      shinyjs::toggleState("small_window", condition = choi_like)
      shinyjs::html("note_small", if (choi_like) "Choi and CANHR" else "not used by Troiano")
    }, ignoreInit = FALSE)

    # Minutes typed, hours shown beside the box
    observeEvent(input$min_wear_day, {
      m <- suppressWarnings(as.numeric(input$min_wear_day))
      h <- if (length(m) == 1 && !is.na(m)) m / 60 else NA_real_
      shinyjs::html("note_day", if (is.na(h)) "minutes worn"
                    else paste0(if (abs(h - round(h)) < 0.01) fmt_int(h) else fmt_dec(h, 1), " h worn"))
    }, ignoreInit = FALSE)


    observeEvent(input$use_ignore_short, {
      shinyjs::toggleState("ignore_short_min", condition = isTRUE(input$use_ignore_short))
    }, ignoreInit = FALSE)

    load_algorithm_defaults <- function() {
      alg <- input$algorithm
      if (alg == "troiano") {
        updateNumericInput(session, "min_length", value = 60)
        updateNumericInput(session, "small_window", value = 30)
        updateNumericInput(session, "spike_tolerance", value = 2)
        updateNumericInput(session, "spike_stoplevel", value = 100)
      } else if (alg == "choi") {
        updateNumericInput(session, "min_length", value = 90)
        updateNumericInput(session, "small_window", value = 30)
        updateNumericInput(session, "spike_tolerance", value = 2)
        updateNumericInput(session, "spike_stoplevel", value = "")
      } else if (alg == "canhr") {
        updateNumericInput(session, "min_length", value = 120)
        updateNumericInput(session, "small_window", value = 45)
        updateNumericInput(session, "spike_tolerance", value = 3)
        updateNumericInput(session, "spike_stoplevel", value = 150)
      }
    }

    # Clear results handler
    observeEvent(input$clear_results, {
      results(list())
    })

    # The empty-state button runs the same code as Run in the rule bar
    observeEvent(input$run_empty, {
      req(shared$data_loaded, shared$file_count > 0)
      shinyjs::show("processing_indicator")
      shinyjs::delay(100, {
        tryCatch(run_validation(names(shared$files)), finally = shinyjs::hide("processing_indicator"))
      })
    })

    observeEvent(input$run_btn, {
      req(shared$data_loaded, shared$file_count > 0)

      shinyjs::disable("run_btn")
      shinyjs::show("processing_indicator")

      shinyjs::delay(100, {
        tryCatch({
          run_validation(names(shared$files))
        }, finally = {
          shinyjs::hide("processing_indicator")
          shinyjs::enable("run_btn")
        })
      })
    })

    # Helper: Format ETA
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    run_validation <- function(file_ids) {
      all_results <- results()
      n_files <- length(file_ids)

      count_col <- if (input$use_vm) "vector_magnitude" else "axis1"
      start_time <- Sys.time()
      progress_interval <- max(1, min(5, ceiling(n_files / 10)))
      warned_msgs <- character()

      # Warnings raised inside the run are muffled and not shown on screen; a
      # recording that fails is reported separately
      notify_warning <- function(msg) {
        if (is.null(msg) || !nzchar(msg)) return()
        if (!msg %in% warned_msgs) warned_msgs <<- c(warned_msgs, msg)
      }

      capture_warnings <- function(expr, prefix = NULL) {
        withCallingHandlers(expr, warning = function(w) {
          msg <- conditionMessage(w)
          if (!is.null(prefix) && nzchar(prefix)) {
            msg <- paste(prefix, msg)
          }
          notify_warning(msg)
          invokeRestart("muffleWarning")
        })
      }

      withProgress(message = "Validating wear time...", value = 0, {
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

          counts_raw <- if (count_col %in% names(data)) data[[count_col]] else data$axis1
          counts <- counts_raw
          if (!is.na(f$epoch_length) && f$epoch_length > 0 && f$epoch_length != 60) {
            counts <- counts_raw * (60 / f$epoch_length)
          }
          epoch_minutes <- f$epoch_length / 60
          non_wear_epochs <- as.integer(input$min_length / epoch_minutes)
          spike_tol_epochs <- as.integer(input$spike_tolerance / epoch_minutes)
          if (spike_tol_epochs < 1) spike_tol_epochs <- 1
          small_window_epochs <- as.integer(input$small_window / epoch_minutes)
          # a blank stop level is none
          stop_level <- input$spike_stoplevel
          if (is.null(stop_level) || is.na(stop_level)) stop_level <- Inf

          wear <- tryCatch({
            capture_warnings({
              if (input$algorithm == "troiano") {
                canhrActi::wear.troiano(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = stop_level
                )
              } else if (input$algorithm == "choi") {
                canhrActi::wear.choi(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = stop_level,
                  min_window_len = small_window_epochs
                )
              } else if (input$algorithm == "canhr") {
                canhrActi::wear.CANHR2025(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = stop_level,
                  min_window_len = small_window_epochs
                )
              } else {
                canhrActi::wear.CANHR2025(
                  counts_per_minute = counts,
                  non_wear_window = non_wear_epochs,
                  spike_tolerance = spike_tol_epochs,
                  spike_stoplevel = stop_level,
                  min_window_len = small_window_epochs
                )
              }
            }, prefix = paste0(f$name, ":"))
          }, error = function(e) {
            showNotification(paste0("Wear validation skipped for ", f$name), type = "error")
            return(NULL)
          })

          if (is.null(wear)) next

          # Fold a short wear run between two non-wear spells into the gap
          if (isTRUE(input$use_ignore_short) && (input$ignore_short_min %||% 0) > 0) {
            runs <- rle(as.logical(wear))
            mins <- runs$lengths * f$epoch_length / 60
            short <- !is.na(runs$values) & runs$values & mins < input$ignore_short_min
            if (any(short)) {
              runs$values[short] <- FALSE
              wear <- inverse.rle(runs)
            }
          }

          daily <- NULL
          day_segs <- NULL
          hourly <- NULL
          wear_periods <- NULL
          nonwear_periods <- NULL
          valid_weekdays <- 0
          valid_weekend <- 0

          if ("timestamp" %in% names(data)) {
            temp <- data
            temp$wear <- wear
            temp$date <- as.Date(temp$timestamp)
            temp$hour <- as.numeric(format(temp$timestamp, "%H"))
            # Weekend by day number (0 Sunday, 6 Saturday); the day names follow the locale
            temp$is_weekend <- as.POSIXlt(temp$timestamp)$wday %in% c(0L, 6L)

            daily <- aggregate(wear ~ date, temp, sum)
            daily$wear_hours <- daily$wear * f$epoch_length / 3600
            daily$wear_min <- daily$wear_hours * 60
            daily$valid <- daily$wear_min >= input$min_wear_day
            daily$weekday <- fmt_date(as.Date(daily$date), "%A")
            daily$is_weekend <- as.POSIXlt(as.Date(daily$date))$wday %in% c(0L, 6L)

            # Recorded minutes per day and the wear runs inside them, which
            # separate a short recording day from a refused one
            temp$mins <- as.numeric(format(temp$timestamp, "%H")) * 60 +
              as.numeric(format(temp$timestamp, "%M"))
            by_date <- split(seq_len(nrow(temp)), as.character(temp$date))
            keys <- as.character(daily$date)
            daily$recorded_h <- vapply(keys, function(k) length(by_date[[k]]) * f$epoch_length / 3600, numeric(1))
            daily$first_min <- vapply(keys, function(k) min(temp$mins[by_date[[k]]]), numeric(1))
            daily$last_min <- vapply(keys, function(k) max(temp$mins[by_date[[k]]]) + f$epoch_length / 60, numeric(1))

            day_segs <- do.call(rbind, lapply(keys, function(k) {
              idx <- by_date[[k]]
              r <- rle(as.logical(temp$wear[idx]))
              stops <- cumsum(r$lengths)
              starts <- stops - r$lengths + 1
              data.frame(date = k, from = temp$mins[idx][starts],
                         to = temp$mins[idx][stops] + f$epoch_length / 60,
                         worn = !is.na(r$values) & r$values, stringsAsFactors = FALSE)
            }))

            valid_weekdays <- sum(daily$valid & !daily$is_weekend)
            valid_weekend <- sum(daily$valid & daily$is_weekend)

            hourly <- aggregate(wear ~ hour, temp, mean)
            hourly$wear_pct <- hourly$wear * 100

            wear_periods <- canhrActi::get.wear.periods(wear, temp$timestamp, epoch_length = f$epoch_length)


            nonwear_periods <- detect_wear_periods(temp$timestamp, !wear, f$epoch_length, input$min_length)
          }

          valid_days <- if (!is.null(daily)) sum(daily$valid) else 0
          total_days <- if (!is.null(daily)) nrow(daily) else 0

          min_wear_for_day <- input$min_wear_day
          days_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day) else 0
          weekdays_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day & !daily$is_weekend) else 0
          weekend_with_wear <- if (!is.null(daily)) sum(daily$wear_min >= min_wear_for_day & daily$is_weekend) else 0

          n_wear_periods <- if (!is.null(wear_periods) && nrow(wear_periods) > 0) nrow(wear_periods) else 0
          n_nonwear_periods <- if (!is.null(nonwear_periods) && nrow(nonwear_periods) > 0) nrow(nonwear_periods) else 0

          avg_wear_period_sec <- if (n_wear_periods > 0) {
            mean(wear_periods$duration_minutes, na.rm = TRUE) * 60
          } else 0

          avg_nonwear_period_sec <- if (n_nonwear_periods > 0) {
            mean(nonwear_periods$duration_min, na.rm = TRUE) * 60
          } else 0

          meets_criteria <- TRUE
          if (valid_days < input$min_valid_days) meets_criteria <- FALSE
          if (valid_weekdays < input$min_weekdays) meets_criteria <- FALSE
          if (valid_weekend < input$min_weekend) meets_criteria <- FALSE

          all_results[[fid]] <- list(
            file_id = fid,
            name = f$name,
            subject_id = f$subject_info$id,
            serial_number = f$device_info$serial_number,
            algorithm = input$algorithm,
            parameters = list(
              min_length = input$min_length,
              small_window = input$small_window,
              spike_tolerance = input$spike_tolerance,
              spike_stoplevel = input$spike_stoplevel,
              use_vm = input$use_vm,
              min_wear_day = input$min_wear_day,
              min_valid_days = input$min_valid_days,
              use_ignore_short = isTRUE(input$use_ignore_short),
              ignore_short_min = input$ignore_short_min
            ),
            wear = wear,
            daily = daily,
            day_segs = day_segs,
            hourly = hourly,
            wear_periods = wear_periods,
            nonwear_periods = nonwear_periods,
            n_nonwear_periods = n_nonwear_periods,
            total_epochs = length(wear),
            wear_epochs = sum(wear),
            nonwear_epochs = sum(!wear),
            total_days = total_days,
            valid_days = valid_days,
            valid_weekdays = valid_weekdays,
            valid_weekend = valid_weekend,
            days_with_wear = days_with_wear,
            weekdays_with_wear = weekdays_with_wear,
            weekend_with_wear = weekend_with_wear,
            avg_wear_period_sec = avg_wear_period_sec,
            avg_nonwear_period_sec = avg_nonwear_period_sec,
            avg_wear = if (!is.null(daily) && any(daily$valid)) mean(daily$wear_hours[daily$valid], na.rm = TRUE) else NA,
            total_wear = if (!is.null(daily)) sum(daily$wear_hours) else NA,
            total_nonwear = if (!is.null(daily)) sum(24 - daily$wear_hours) else NA,
            wear_pct = round(sum(wear) / length(wear) * 100, 1),
            meets_criteria = meets_criteria,
            validated_at = Sys.time()
          )
        }

        gc(verbose = FALSE)
      })

      results(all_results)
      shared$results$wear_time <- all_results

    }

    # Helper function to detect continuous wear periods
    detect_wear_periods <- function(timestamps, wear, epoch_length, min_duration_min = 0) {
      if (length(wear) == 0) return(data.frame())

      periods <- data.frame()
      in_wear <- FALSE
      start_idx <- 1

      for (i in seq_along(wear)) {
        if (wear[i] && !in_wear) {
          in_wear <- TRUE
          start_idx <- i
        } else if (!wear[i] && in_wear) {
          in_wear <- FALSE
          duration <- (i - start_idx) * epoch_length / 60
          if (duration >= min_duration_min) {
            periods <- rbind(periods, data.frame(
              start = timestamps[start_idx],
              end = timestamps[i - 1],
              duration_min = duration
            ))
          }
        }
      }

      if (in_wear) {
        duration <- (length(timestamps) - start_idx + 1) * epoch_length / 60
        if (duration >= min_duration_min) {
          periods <- rbind(periods, data.frame(
            start = timestamps[start_idx],
            end = timestamps[length(timestamps)],
            duration_min = duration
          ))
        }
      }

      return(periods)
    }

    picked <- reactiveVal(NULL)       # the recording open in the panel below
    chip <- reactiveVal("all")        # which filter chip is on
    sort_key <- reactiveVal("valid")  # valid | name | worn | bouts
    sort_dir <- reactiveVal(1)

    observeEvent(input$pick, {
      picked(input$pick)
      focus_set(shared, input$pick)
    }, ignoreInit = TRUE)
    # Open on the recording last picked on any page, on either side
    on_tab_shown(shared, "wear_time", function() {
      id <- focus_get(shared, among = names(shared$files))
      if (!is.null(id)) picked(id)
      rid <- focus_get(shared, among = raw_ids())
      if (!is.null(rid)) local_raw$sel <- rid
    })
    observeEvent(input$chip, chip(input$chip), ignoreInit = TRUE)
    observeEvent(input$sort, {
      key <- input$sort
      if (!key %in% c("name", "valid", "worn", "bouts")) return()
      if (identical(sort_key(), key)) sort_dir(-sort_dir()) else { sort_key(key); sort_dir(1) }
    }, ignoreInit = TRUE)

    min_hours <- reactive({
      m <- suppressWarnings(as.numeric(input$min_wear_day))
      if (length(m) != 1 || is.na(m)) 10 else m / 60
    })

    # The threshold the results were produced with, not the one in the boxes
    run_hours <- reactive({
      res <- results()
      if (length(res) == 0) return(min_hours())
      m <- suppressWarnings(as.numeric(res[[1]]$parameters$min_wear_day))
      if (length(m) != 1 || is.na(m)) min_hours() else m / 60
    })

    hours_label <- function(h) if (abs(h - round(h)) < 0.01) fmt_int(h) else fmt_dec(h, 1)
    run_hours_label <- reactive(hours_label(run_hours()))

    run_days <- reactive({
      res <- results()
      n <- if (length(res) == 0) input$min_valid_days else res[[1]]$parameters$min_valid_days
      n <- suppressWarnings(as.numeric(n))
      if (length(n) != 1 || is.na(n)) 3 else n
    })

    run_window <- reactive({
      res <- results()
      w <- if (length(res) == 0) input$min_length else res[[1]]$parameters$min_length
      w <- suppressWarnings(as.numeric(w))
      if (length(w) != 1 || is.na(w)) 90 else w
    })

    # True once the boxes no longer describe the run on screen.
    settings_moved <- reactive({
      res <- results()
      if (length(res) == 0) return(FALSE)
      p <- res[[1]]$parameters
      same <- function(a, b) {
        a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
        (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && abs(a - b) < 1e-9)
      }
      !identical(res[[1]]$algorithm, input$algorithm) ||
        !same(p$min_length, input$min_length) || !same(p$small_window, input$small_window) ||
        !same(p$spike_tolerance, input$spike_tolerance) || !same(p$spike_stoplevel, input$spike_stoplevel) ||
        !identical(isTRUE(p$use_vm), isTRUE(input$use_vm)) || !same(p$min_wear_day, input$min_wear_day)
    })
    min_hours_label <- reactive({
      h <- min_hours()
      if (abs(h - round(h)) < 0.01) fmt_int(h) else fmt_dec(h, 1)
    })

    # Worn and recorded totals from the daily rows, so the figures agree with the table
    file_totals <- function(res) {
      d <- res$daily
      if (is.null(d) || nrow(d) == 0) return(list(worn = NA_real_, recorded = NA_real_, share = NA_real_))
      worn <- sum(d$wear_hours, na.rm = TRUE)
      recorded <- sum(d$recorded_h, na.rm = TRUE)
      list(worn = worn, recorded = recorded,
           share = if (recorded > 0) worn / recorded else NA_real_)
    }

    dead_days <- function(res) {
      d <- res$daily
      if (is.null(d) || nrow(d) == 0) return(0L)
      sum(d$wear_hours < 0.5 & d$recorded_h > 23.9, na.rm = TRUE)
    }

    near_miss <- function(res) {
      d <- res$daily
      if (is.null(d) || nrow(d) == 0) return(0L)
      sum(!d$valid & d$wear_hours >= run_hours() - 1 & d$wear_hours < run_hours(), na.rm = TRUE)
    }

    # The list in table order: fewest valid days first by default, then least worn
    ordered <- reactive({
      res <- results()
      ids <- names(res)
      if (length(ids) < 2) return(ids)
      key <- sort_key()
      value <- switch(key,
        name  = vapply(res, function(r) tolower(present(r$subject_id) %||% r$name), character(1)),
        worn  = vapply(res, function(r) file_totals(r)$share %||% NA_real_, numeric(1)),
        bouts = vapply(res, function(r) as.numeric(r$n_nonwear_periods %||% 0), numeric(1)),
        vapply(res, function(r) as.numeric(r$valid_days), numeric(1)))
      tie <- vapply(res, function(r) file_totals(r)$share %||% NA_real_, numeric(1))
      ids[order(value, tie, decreasing = sort_dir() < 0, na.last = TRUE)]
    })

    shown <- reactive({
      res <- results()
      ids <- ordered()
      switch(chip(),
        excluded = ids[!vapply(res[ids], function(r) isTRUE(r$meets_criteria), logical(1))],
        dead = ids[vapply(res[ids], function(r) dead_days(r) > 0, logical(1))],
        near = ids[vapply(res[ids], function(r) near_miss(r) > 0, logical(1))],
        ids)
    })

    # The detail panel follows the list: the first shown row unless one is picked
    selected_id <- reactive({
      ids <- shown()
      if (length(ids) == 0) return(NULL)
      p <- picked()
      if (!is.null(p) && p %in% ids) p else ids[1]
    })

    output$raw_plot <- renderPlot({
      rs <- raw_list(); ids <- raw_ids()
      req(length(rs) > 0)
      i <- if (is.null(local_raw$sel)) 1L else match(local_raw$sel, ids)
      if (is.na(i)) i <- 1L
      r <- rs[[i]]
      if (identical(local_raw$chart, "quality")) canhrActi::plot_raw_quality(r)
      else canhrActi::plot_raw_wear_days(r)
    }, res = 96, height = function() {
      wtr_chart_height(session$clientData[[paste0("output_", ns("raw_plot"), "_width")]])
    })

    output$rule <- renderUI({
      if (identical(view(), "raw")) {
        rs <- raw_list()
        return(tagList(raw_switch(), wtr_rule(rs, ns)))
      }
      alg <- switch(input$algorithm %||% "choi",
                    troiano = "Troiano (2008)", canhr = "CANHR (2025)", "Choi (2011)")
      counts <- if (isTRUE(input$use_vm)) "vector magnitude" else "axis 1"
      # Both branches draw the switch; without it here Counts has no way back
      tagList(raw_switch(), tags$div(
        class = "wt-panel wt-rule",
        tags$span(class = "wt-rule-k", "Non-wear"),
        tags$span(class = "wt-rule-v",
          tags$b(alg), " · ", fmt_int(input$min_length %||% 90), " min window · ",
          fmt_int(input$spike_tolerance %||% 2), " min spike tolerance · ",
          if (is.null(input$spike_stoplevel) || is.na(input$spike_stoplevel)) "no stop level"
          else paste("stop", fmt_int(input$spike_stoplevel)), " · ", counts),
        tags$span(class = "wt-rule-sep", `aria-hidden` = "true"),
        tags$span(class = "wt-rule-k", "Valid"),
        tags$span(class = "wt-rule-v",
          "day at ", tags$b(paste0(min_hours_label(), " h")), " worn · subject at ",
          tags$b(fmt_int(input$min_valid_days %||% 3)), " days"),
        if (settings_moved()) tags$span(class = "wt-stale", "changed since the run below") else NULL,
        tags$span(class = "wt-rule-actions",
          actionButton(ns("toggle_advanced"), "Change", class = "wt-btn wt-btn--secondary"),
          actionButton(ns("run_btn"), if (length(results()) == 0) "Run validation" else "Re-run",
                       class = paste(run_button_class("wt", length(results()) > 0, settings_moved()),
                                     "wt-run")))
      ))
    })

    output$figures <- renderUI({
      if (identical(view(), "raw")) return(wtr_figures(raw_list()))
      res <- results()
      if (length(res) == 0) return(NULL)
      totals <- lapply(res, file_totals)
      worn <- sum(vapply(totals, function(t) t$worn %||% NA_real_, numeric(1)), na.rm = TRUE)
      recorded <- sum(vapply(totals, function(t) t$recorded %||% NA_real_, numeric(1)), na.rm = TRUE)
      days <- sum(vapply(res, function(r) as.numeric(r$total_days), numeric(1)))
      valid <- sum(vapply(res, function(r) as.numeric(r$valid_days), numeric(1)))
      pass <- sum(vapply(res, function(r) isTRUE(r$meets_criteria), logical(1)))

      # Mean over days, not of per-file means
      valid_hours <- unlist(lapply(res, function(r) {
        d <- r$daily
        if (is.null(d) || nrow(d) == 0) return(numeric(0))
        d$wear_hours[d$valid]
      }), use.names = FALSE)

      tags$div(
        class = "wt-panel wt-figs",
        wt_fig(fmt_int(length(res)), NULL, if (length(res) == 1) "Recording" else "Recordings"),
        wt_rule_div(),
        wt_fig(paste(fmt_int(pass), "of", fmt_int(length(res))), NULL, "Meet the criteria"),
        wt_rule_div(),
        wt_fig(paste(fmt_int(valid), "of", fmt_int(days)), NULL, "Valid days"),
        wt_rule_div(),
        wt_fig(if (length(valid_hours)) fmt_dec(mean(valid_hours), 1) else "–", "h", "Mean wear, valid days"),
        wt_rule_div(),
        wt_fig(fmt_int(round(worn)),
               if (recorded > 0) HTML(paste0("h &middot; ", round(worn / recorded * 100), "%")) else "h",
               "Worn of recorded")
      )
    })

    output$list_panel <- renderUI({
      if (identical(view(), "raw")) {
        rs <- raw_list(); ids <- raw_ids()
        return(tagList(
          wtr_chart_panel(rs, ids, local_raw$sel, local_raw$chart, ns),
          wtr_table_panel(rs, ids, local_raw$sel, local_raw$tab, ns,
                          local_raw$sort, local_raw$dir)))
      }
      res <- results()
      if (length(res) == 0) {
        return(tags$div(class = "wt-panel wt-empty",
          if (shared$file_count == 0) {
            tagList(tags$div(class = "wt-empty-t", "No recordings are loaded"),
                    tags$div(class = "wt-empty-s", "Add files on the Overview, then run validation here."))
          } else {
            tagList(tags$div(class = "wt-empty-t", "Nothing has been validated yet"),
                    tags$div(class = "wt-empty-s",
                             paste(plural(shared$file_count, "recording"),
                                   "loaded. The rule above is what will be applied to all of them.")),
                    actionButton(ns("run_empty"), "Run validation", class = "wt-btn wt-btn--secondary wt-btn--lg"))
          }))
      }

      ids <- shown()
      longest <- max(vapply(res, function(r) as.numeric(r$total_days %||% 0), numeric(1)), na.rm = TRUE)
      longest <- max(1, longest)
      sel <- selected_id()

      sort_th <- function(key, label, width = NULL, right = FALSE, title = NULL) {
        on <- identical(sort_key(), key)
        tags$th(
          class = paste("wt-sortable", if (on) "is-sorted" else ""),
          `data-sort` = key, role = "button", tabindex = "0", title = title,
          style = paste0(if (!is.null(width)) paste0("width: ", width, "px;"), if (right) " text-align: right;"),
          `aria-sort` = if (on) (if (sort_dir() > 0) "ascending" else "descending") else "none",
          label, if (on) wt_caret(sort_dir() > 0))
      }

      ticks <- lapply(seq(1, longest, by = 3), function(d)
        tags$span(class = "wt-tk", style = paste0("left: ", (d - 1) * 20, "px;"), d))
      if (longest %% 3 != 1) ticks <- c(ticks, list(
        tags$span(class = "wt-tk", style = paste0("left: ", (longest - 1) * 20, "px;"), longest)))

      row <- function(fid) {
        r <- res[[fid]]
        t <- file_totals(r)
        d <- r$daily
        cells <- if (is.null(d) || nrow(d) == 0) NULL else lapply(seq_len(nrow(d)), function(i) {
          wt_cell(d$wear_hours[i], d$recorded_h[i], d$valid[i],
                  paste0(fmt_date(d$date[i], "%a %e %b"), " · ", fmt_dec(d$wear_hours[i], 1),
                         " h worn of ", fmt_dec(d$recorded_h[i], 1), " h recorded"))
        })
        tags$tr(
          class = paste("wt-row", if (identical(fid, sel)) "is-selected" else ""),
          `data-fid` = fid, tabindex = if (identical(fid, sel)) "0" else "-1",
          role = "option", `aria-selected` = tolower(as.character(identical(fid, sel))),
          tags$td(class = "wt-sub", title = if (!is.null(present(r$subject_id))) cell_title(r$name),
                  present(r$subject_id) %||% r$name),
          tags$td(class = "wt-dim", if (!is.null(d) && nrow(d) > 0) fmt_date(d$date[1], "%e %b %Y") else "–"),
          tags$td(tags$span(class = "wt-cellrow", cells)),
          tags$td(class = "r", paste(fmt_int(r$valid_days), "of", fmt_int(r$total_days))),
          tags$td(class = "r", if (is.na(t$worn)) "–" else paste0(fmt_dec(t$worn, 1), " h")),
          tags$td(class = "r", if (is.na(t$share)) "–" else paste0(round(t$share * 100), "%")),
          tags$td(class = "r", fmt_int(r$n_nonwear_periods %||% 0)),
          tags$td(wt_verdict_cell(wt_recording_verdict(r, run_days(), run_hours_label())))
        )
      }

      n_excluded <- sum(!vapply(res, function(r) isTRUE(r$meets_criteria), logical(1)))
      n_dead <- sum(vapply(res, function(r) dead_days(r) > 0, logical(1)))
      n_near <- sum(vapply(res, function(r) near_miss(r) > 0, logical(1)))

      tags$div(
        class = "wt-panel wt-list",
        tags$div(
          class = "wt-list-head",
          tags$span(class = "wt-chips",
            wt_chip("All ", length(res), chip() == "all", "all"),
            wt_chip("Excluded ", n_excluded, chip() == "excluded", "excluded"),
            wt_chip("A day never worn ", n_dead, chip() == "dead", "dead"),
            wt_chip("A day missed by under 1 h ", n_near, chip() == "near", "near")),
          tags$span(class = "wt-key",
            tags$span(class = "wt-key-t", "Each day"),
            tags$span(class = "wt-key-i",
              tags$span(class = "wt-kn", "0 h"),
              tags$span(class = "wt-ramp",
                tags$b(class = "wt-s0"), tags$b(class = "wt-s1"), tags$b(class = "wt-s2"),
                tags$b(class = "wt-s3"), tags$b(class = "wt-s4")),
              tags$span(class = "wt-kn", "24 h worn")),
            tags$span(class = "wt-key-i",
              tags$span(class = "wt-cell wt-s1 is-bad"), paste0("under ", run_hours_label(), " h worn")),
            tags$span(class = "wt-key-i",
              tags$span(class = "wt-cell wt-s1 is-part"), "part day"),
            tags$span(class = "wt-key-sep", `aria-hidden` = "true"),
            downloadButton(ns("export_days"), "Export days", class = "wt-btn wt-btn--secondary", icon = NULL))
        ),
        tags$div(
          class = "wt-scroll",
          tags$table(
            class = "wt-table",
            tags$colgroup(
              tags$col(style = "width: 96px;"), tags$col(style = "width: 96px;"),
              tags$col(style = paste0("width: ", longest * 20 + 16, "px;")),
              tags$col(style = "width: 92px;"), tags$col(style = "width: 78px;"),
              tags$col(style = "width: 66px;"), tags$col(style = "width: 96px;"), tags$col()),
            tags$thead(tags$tr(
              sort_th("name", "Recording", 96),
              tags$th("Started"),
              tags$th(tags$span(class = "wt-dayhead",
                                tags$span(class = "wt-dayhead-l", "Days of the recording"), ticks)),
              sort_th("valid", "Valid days", 92, right = TRUE),
              tags$th(style = "text-align: right;", "Worn"),
              sort_th("worn", "Of rec.", 66, right = TRUE, title = "Worn of recorded"),
              sort_th("bouts", paste0("Off ≥ ", fmt_int(run_window()), " min"), 96, right = TRUE),
              tags$th("Verdict"))),
            tags$tbody(role = "listbox", `aria-label` = "Validated recordings", lapply(ids, row))
          )
        )
      )
    })

    output$detail_panel <- renderUI({
      # Nothing docks on the raw side; its tabbed panel is above
      if (identical(view(), "raw")) return(NULL)
      res <- results()
      fid <- selected_id()
      if (length(res) == 0 || is.null(fid)) return(NULL)
      r <- res[[fid]]
      days <- wt_days(r)
      t <- file_totals(r)
      d <- r$daily

      bouts <- r$nonwear_periods
      has_bouts <- !is.null(bouts) && is.data.frame(bouts) && nrow(bouts) > 0
      longest_bout <- if (has_bouts) max(bouts$duration_min, na.rm = TRUE) else 0

      # Same grid as the list above
      day_row <- function(day) {
        v <- wt_day_verdict(day, run_hours(), run_hours_label())
        gaps <- if (is.null(day$segs)) NULL else day$segs[!day$segs$worn, , drop = FALSE]
        n_gaps <- if (is.null(gaps)) 0L else nrow(gaps)
        off <- max(0, (day$recorded_h %||% 0) - (day$wear_hours %||% 0))
        tags$tr(
          class = if (isTRUE(day$valid)) NULL else "is-out",
          tags$td(class = "wt-dday", fmt_date(day$date, "%a %e %b")),
          tags$td(class = "wt-cstrip", wt_strip(day$segs)),
          tags$td(class = "r", paste0(fmt_dec(day$wear_hours, 1), " h")),
          tags$td(class = "r wt-dim", if (off < 0.05) "–" else paste0(fmt_dec(off, 1), " h")),
          tags$td(class = "r wt-dim", if (n_gaps == 0) "–" else fmt_int(n_gaps)),
          tags$td(wt_verdict_cell(v)))
      }

      tags$div(
        class = "wt-panel wt-detail",
        tags$div(class = "wt-detail-head",
          tags$span(class = "wt-who", present(r$subject_id) %||% r$name),
          tags$span(class = "wt-what", title = r$name, r$name),
          tags$span(class = "wt-what wt-right",
            if (!is.null(d) && nrow(d) > 0)
              paste0(fmt_date(d$date[1], "%e %b"), " to ", fmt_date(d$date[nrow(d)], "%e %b %Y"), " · ") else NULL,
            paste0(fmt_int(r$valid_days), " of ", fmt_int(r$total_days), " days valid · ",
                   fmt_dec(t$worn, 1), " h of ", fmt_dec(t$recorded, 1), " h worn",
                   if (!is.na(t$share)) paste0(" (", round(t$share * 100), "%)") else ""))),

        tags$div(
          class = "wt-scroll",
          tags$table(
            class = "wt-table wt-days",
            tags$colgroup(
              tags$col(style = "width: 112px;"), tags$col(),
              tags$col(style = "width: 76px;"), tags$col(style = "width: 66px;"),
              tags$col(style = "width: 56px;"), tags$col(style = "width: 340px;")),
            tags$thead(tags$tr(
              tags$th("Day"),
              tags$th(tags$span(class = "wt-axis",
                tags$span(class = "wt-tick is-first", style = "left: 0;", "00:00"),
                tags$span(class = "wt-tick", style = "left: 25%;", "06:00"),
                tags$span(class = "wt-tick", style = "left: 50%;", "12:00"),
                tags$span(class = "wt-tick", style = "left: 75%;", "18:00"),
                tags$span(class = "wt-tick is-last", style = "left: 100%;", "24:00"))),
              tags$th(style = "text-align: right;", "Worn"),
              tags$th(style = "text-align: right;", "Off"),
              tags$th(style = "text-align: right;", "Gaps"),
              tags$th("Verdict"))),
            tags$tbody(lapply(days, day_row)))
        ),

        tags$details(
          class = "wt-disc", `data-persist` = "wear.bouts", open = NA,
          tags$summary(
            tags$span(class = "wt-chev", `aria-hidden` = "true", wt_chevron()),
            tags$span(class = "wt-disc-t", paste0("Non-wear bouts of ", fmt_int(run_window()), " min or more")),
            tags$span(class = "wt-disc-s",
              tags$span(tags$b(fmt_int(if (has_bouts) nrow(bouts) else 0)), " in ", present(r$subject_id) %||% r$name),
              if (has_bouts) tags$span("longest ", tags$b(paste0(fmt_dec(longest_bout / 60, 1), " h"))) else NULL)),
          if (has_bouts) tags$div(
            class = "wt-bouts",
            tags$table(
              class = "wt-table wt-bouts-table",
              tags$colgroup(tags$col(style = "width: 190px;"), tags$col(style = "width: 190px;"),
                            tags$col(style = "width: 76px;"), tags$col()),
              tags$thead(tags$tr(
                tags$th("From"), tags$th("To"),
                tags$th(style = "text-align: right;", "Length"),
                tags$th("Against the longest"))),
              # Bar length relative to the longest bout
              tags$tbody(lapply(seq_len(nrow(bouts)), function(i) {
                mins <- bouts$duration_min[i]
                tags$tr(
                  tags$td(fmt_date(bouts$start[i], "%a %e %b %H:%M")),
                  tags$td(fmt_date(bouts$end[i], "%a %e %b %H:%M")),
                  tags$td(class = "r", if (mins >= 60) paste0(fmt_dec(mins / 60, 1), " h") else paste0(round(mins), " min")),
                  tags$td(tags$span(class = "wt-gauge",
                    tags$span(class = "wt-bar",
                      tags$i(style = sprintf("width: %.1f%%;", if (longest_bout > 0) mins / longest_bout * 100 else 0))))))
              })))
          ) else tags$div(class = "wt-bouts wt-none",
                          "None: the device was on the wrist for every epoch of this recording.")
        )
      )
    })

    # Whichever of the two exports is on screen, narrowed to the chosen recording
    output$raw_export <- downloadHandler(
      filename = function() {
        paste0(if (identical(local_raw$tab, "day")) "part2_daysummary" else "part2_summary",
               "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        rs <- raw_list(); ids <- raw_ids()
        df <- if (identical(local_raw$tab, "day")) wtr_day_df(rs, ids, local_raw$sel)
              else wtr_summary_df(rs, ids, local_raw$sel)
        # fwrite quotes only what needs it and writes NA as an empty cell,
        # GGIR's own csv style
        data.table::fwrite(df, file, row.names = FALSE, na = "")
      }
    )

    output$export_days <- downloadHandler(
      filename = function() paste0("wear_time_days_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        res <- results()
        rows <- do.call(rbind, lapply(names(res), function(fid) {
          r <- res[[fid]]
          d <- r$daily
          if (is.null(d) || nrow(d) == 0) return(NULL)
          data.frame(
            subject = present(r$subject_id) %||% r$name,
            file = r$name,
            date = format(d$date, "%Y-%m-%d"),
            weekday = d$weekday,
            recorded_hours = round(d$recorded_h, 3),
            worn_hours = round(d$wear_hours, 3),
            valid_day = d$valid,
            algorithm = r$algorithm,
            min_wear_hours = round(min_hours(), 2),
            stringsAsFactors = FALSE)
        }))
        if (is.null(rows)) rows <- data.frame()
        utils::write.csv(rows, file, row.names = FALSE)
      }
    )

  })
}
