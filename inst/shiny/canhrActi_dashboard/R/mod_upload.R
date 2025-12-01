# Module

mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Welcome Banner
    fluidRow(
      column(
        width = 12,
        div(
          class = "welcome-banner",
          style = "background: linear-gradient(135deg, #236192 0%, #1a4a6f 100%); color: white; padding: 25px 30px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 4px 15px rgba(35, 97, 146, 0.3);",
          fluidRow(
            column(
              width = 8,
              h3(style = "margin: 0 0 10px 0; font-weight: 600;",
                 icon("chart-line", style = "margin-right: 10px;"),
                 "Welcome to canhrActi"),
              p(style = "margin: 0; opacity: 0.9; font-size: 14px; line-height: 1.6;",
                "Accelerometer data analysis platform for physical activity research. ",
                "Upload ActiGraph files (.agd, .gt3x, .csv) to analyze wear time, ",
                "activity patterns, sleep, and circadian rhythms.")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right; padding-top: 5px;",
                tags$span(
                  style = "display: inline-block; background: rgba(255,205,0,0.2); padding: 8px 15px; border-radius: 20px; font-size: 12px; border: 1px solid rgba(255,205,0,0.4);",
                  icon("lightbulb", style = "color: #FFCD00; margin-right: 8px;"),
                  "Tip: Upload multiple files for batch analysis"
                )
              )
            )
          )
        )
      )
    ),

    # Summary boxes at top
    fluidRow(
      valueBoxOutput(ns("vb_file_count"), width = 3),
      valueBoxOutput(ns("vb_total_duration"), width = 3),
      valueBoxOutput(ns("vb_total_epochs"), width = 3),
      valueBoxOutput(ns("vb_avg_epoch_len"), width = 3)
    ),

    fluidRow(
      # Left column - Upload controls
      column(
        width = 4,

        # File upload box
        box(
          title = span(icon("upload", style = "margin-right: 8px;"), "Add Files"),
          status = "primary", solidHeader = TRUE,
          width = NULL,

          div(
            style = "background: linear-gradient(135deg, #e8f4fc 0%, #f8fafc 100%); border-radius: 8px; padding: 15px; margin-bottom: 15px; border: 2px dashed #b8d4e8;",
            div(style = "text-align: center; margin-bottom: 10px;",
                icon("cloud-upload-alt", style = "font-size: 32px; color: #236192; opacity: 0.7;")),
            fileInput(
              ns("files"),
              NULL,
              multiple = TRUE,
              accept = c(".agd", ".gt3x", ".csv"),
              buttonLabel = span(icon("folder-open"), " Select Files"),
              placeholder = "AGD, GT3X, or CSV files"
            )
          ),

          # Add directory - native folder picker using webkitdirectory
          tags$div(
            style = "margin-bottom: 15px;",
            tags$label("Or select entire folder:", class = "control-label", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
            tags$div(
              class = "input-group",
              tags$label(
                class = "btn btn-info btn-block",
                style = "margin-bottom: 0; background: linear-gradient(135deg, #FFCD00 0%, #e6b800 100%); border: none; color: #0d2137; font-weight: 500;",
                icon("folder-open"), " Browse Folder...",
                tags$input(
                  type = "file",
                  id = ns("dir_files"),
                  webkitdirectory = TRUE,
                  multiple = TRUE,
                  style = "display: none;",
                  accept = ".agd,.gt3x,.csv"
                )
              )
            )
          ),

          hr(style = "border-color: #e8f4fc; margin: 20px 0;"),

          div(
            style = "display: flex; gap: 10px;",
            div(style = "flex: 1;",
                actionButton(ns("demo_btn"), span(icon("database"), " Demo"),
                            class = "btn-default btn-block",
                            style = "border: 2px solid #FFCD00; color: #0d2137;")),
            div(style = "flex: 1;",
                actionButton(ns("clear_all_btn"), span(icon("trash"), " Clear"),
                            class = "btn-default btn-block",
                            style = "border: 2px solid #dc3545; color: #dc3545;"))
          )
        ),

        # Supported formats info
        box(
          title = span(icon("file-alt", style = "margin-right: 8px;"), "Supported Formats"),
          status = "info", width = NULL, collapsible = TRUE, collapsed = TRUE,
          div(
            style = "font-size: 12px;",
            tags$table(
              style = "width: 100%;",
              tags$tr(
                tags$td(style = "padding: 5px;", tags$span(class = "label label-primary", ".agd")),
                tags$td(style = "padding: 5px; color: #666;", "ActiGraph database files (epoch or raw)")
              ),
              tags$tr(
                tags$td(style = "padding: 5px;", tags$span(class = "label label-success", ".gt3x")),
                tags$td(style = "padding: 5px; color: #666;", "ActiGraph GT3X+ raw files")
              ),
              tags$tr(
                tags$td(style = "padding: 5px;", tags$span(class = "label label-warning", ".csv")),
                tags$td(style = "padding: 5px; color: #666;", "ActiLife exported CSV files")
              )
            ),
            hr(style = "margin: 10px 0;"),
            div(style = "color: #236192;",
                icon("info-circle"), " Raw files are automatically converted to activity counts")
          )
        ),

        # Device Information box (populated from file metadata)
        box(
          title = span(icon("microchip", style = "margin-right: 8px;"), "Device Information"),
          status = "info", solidHeader = FALSE,
          width = NULL,
          uiOutput(ns("device_info_display"))
        ),

        # Subject Biometric Information box (populated from file metadata)
        box(
          title = span(icon("user", style = "margin-right: 8px;"), "Subject Information"),
          status = "success", solidHeader = FALSE,
          width = NULL,
          uiOutput(ns("subject_info_display"))
        )
      ),

      # Right column - File list and info
      column(
        width = 8,

        # File list table
        box(
          title = span(icon("list-alt", style = "margin-right: 8px;"), "Loaded Files"),
          status = "primary", solidHeader = TRUE,
          width = NULL,
          DT::dataTableOutput(ns("file_table")),
          br(),
          fluidRow(
            column(6, actionButton(ns("remove_selected_btn"), span(icon("minus-circle"), " Remove Selected"),
                                   class = "btn-default", style = "border: 2px solid #dc3545; color: #dc3545;")),
            column(6, actionButton(ns("view_selected_btn"), span(icon("eye"), " View Selected"),
                                   class = "btn-primary"))
          )
        ),

        # Data preview table
        box(
          title = span(icon("table", style = "margin-right: 8px;"), "Data Preview"),
          status = "info", solidHeader = FALSE,
          width = NULL, collapsible = TRUE,
          DT::dataTableOutput(ns("preview_table"))
        )
      )
    )
  )
}

mod_upload_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state
    local <- reactiveValues(
      next_id = 1,
      selected_rows = NULL
    )

    # Helper: Get setting value from settings data frame
    get_setting <- function(settings, name) {
      if (is.null(settings) || !is.data.frame(settings)) return(NA)
      value <- settings$settingValue[tolower(settings$settingName) == tolower(name)]
      if (length(value) == 0) return(NA)
      value <- as.character(value)
      if (value == "" || value == "0") return(NA)
      return(value)
    }

    # Helper: Format height from cm to ft/in
    format_height <- function(height_cm) {
      if (is.na(height_cm) || height_cm == 0) return("N/A")
      height_cm <- as.numeric(height_cm)
      total_inches <- height_cm / 2.54
      feet <- floor(total_inches / 12)
      inches <- round(total_inches %% 12)
      paste0(feet, "ft ", inches, "in")
    }

    # Helper: Format weight from kg to lbs
    format_weight <- function(mass_kg) {
      if (is.na(mass_kg) || mass_kg == 0) return("N/A")
      mass_kg <- as.numeric(mass_kg)
      lbs <- round(mass_kg * 2.20462)
      paste0(lbs, "lbs")
    }

    # Helper: Format sex
    format_sex <- function(sex) {
      if (is.na(sex) || sex == "" || sex == "0") return("N/A")
      sex_lower <- tolower(as.character(sex))
      if (sex_lower %in% c("male", "m", "1")) return("Male")
      if (sex_lower %in% c("female", "f", "2")) return("Female")
      return(sex)
    }

    # Helper: Format timestamp from AGD format
    format_agd_timestamp <- function(ts_value) {
      if (is.na(ts_value)) return("N/A")
      ts_numeric <- as.numeric(ts_value)
      if (is.na(ts_numeric)) return("N/A")
      ts <- as.POSIXct((ts_numeric / 10000000 - 62135596800), origin = '1970-01-01', tz = 'UTC')
      format(ts, "%m/%d/%Y %I:%M %p")
    }

    # Helper: Load a single file
    load_single_file <- function(file_path, file_name) {
      ext <- tolower(tools::file_ext(file_name))

      result <- tryCatch({
        if (ext == "agd") {
          canhrActi::read.agd(file_path)
        } else if (ext == "csv") {
          canhrActi::load.actigraph.csv(file_path)
        } else if (ext == "gt3x") {
          canhrActi::read.gt3x.file(file_path)
        } else {
          stop("Unsupported file type: ", ext)
        }
      }, error = function(e) {
        return(list(error = e$message))
      })

      if (!is.null(result$error)) {
        return(list(success = FALSE, error = result$error))
      }

      # Extract raw data and settings
      if (is.list(result) && "data" %in% names(result)) {
        raw_data <- result$data
        settings <- result$settings
      } else {
        raw_data <- result
        settings <- NULL
      }

      # Convert AGD data to standard format with timestamps
      if ("dataTimestamp" %in% names(raw_data)) {
        # Extract axis values
        a1 <- if ("axis1" %in% names(raw_data)) raw_data$axis1 else NA
        a2 <- if ("axis2" %in% names(raw_data)) raw_data$axis2 else NA
        a3 <- if ("axis3" %in% names(raw_data)) raw_data$axis3 else NA

        # Calculate Vector Magnitude: sqrt(axis1^2 + axis2^2 + axis3^2)
        vm <- if (!all(is.na(a1)) && !all(is.na(a2)) && !all(is.na(a3))) {
          round(sqrt(a1^2 + a2^2 + a3^2), 1)
        } else {
          NA
        }

        data <- data.frame(
          timestamp = as.POSIXct((raw_data$dataTimestamp / 10000000 - 62135596800),
                                  origin = '1970-01-01', tz = 'UTC'),
          axis1 = a1,
          axis2 = a2,
          axis3 = a3,
          vector_magnitude = vm,
          steps = if ("steps" %in% names(raw_data)) raw_data$steps else NA,
          lux = if ("lux" %in% names(raw_data)) raw_data$lux else NA,
          inclineOff = if ("inclineOff" %in% names(raw_data)) raw_data$inclineOff else NA,
          inclineStanding = if ("inclineStanding" %in% names(raw_data)) raw_data$inclineStanding else NA,
          inclineSitting = if ("inclineSitting" %in% names(raw_data)) raw_data$inclineSitting else NA,
          inclineLying = if ("inclineLying" %in% names(raw_data)) raw_data$inclineLying else NA,
          stringsAsFactors = FALSE
        )
        # Remove columns that are all NA
        data <- data[, colSums(!is.na(data)) > 0]
      } else if ("timestamp" %in% names(raw_data)) {
        # Add vector magnitude if axis columns exist
        if (all(c("axis1", "axis2", "axis3") %in% names(raw_data))) {
          raw_data$vector_magnitude <- round(sqrt(raw_data$axis1^2 + raw_data$axis2^2 + raw_data$axis3^2), 1)
        }
        data <- raw_data
      } else {
        data <- raw_data
      }

      # Detect epoch length from settings or data
      epoch_len <- as.numeric(get_setting(settings, "epochlength"))
      if (is.na(epoch_len) && "timestamp" %in% names(data) && nrow(data) > 1) {
        diff_secs <- as.numeric(difftime(data$timestamp[2], data$timestamp[1], units = "secs"))
        epoch_len <- round(diff_secs)
      }
      if (is.na(epoch_len)) epoch_len <- 60

      # Calculate duration
      duration_hrs <- NA
      if ("timestamp" %in% names(data) && nrow(data) > 0) {
        duration_hrs <- as.numeric(difftime(max(data$timestamp), min(data$timestamp), units = "hours"))
      }

      # Extract comprehensive device info from settings
      device_info <- list(
        device_type = get_setting(settings, "devicetype"),
        serial_number = get_setting(settings, "deviceserial"),
        firmware = get_setting(settings, "firmwareversion"),
        battery = get_setting(settings, "batteryvoltage"),
        filter = get_setting(settings, "filter"),
        software = get_setting(settings, "softwarename"),
        software_version = get_setting(settings, "softwareversion"),
        epoch_length = epoch_len,
        start_datetime = get_setting(settings, "startdatetime"),
        stop_datetime = get_setting(settings, "stopdatetime"),
        download_datetime = get_setting(settings, "downloaddatetime"),
        sample_rate = get_setting(settings, "samplerate"),
        acceleration_scale = get_setting(settings, "accelerationscale"),
        acceleration_min = get_setting(settings, "accelerationmin"),
        acceleration_max = get_setting(settings, "accelerationmax"),
        modes = get_setting(settings, "modesstring")
      )

      # Extract comprehensive subject info from settings
      subject_info <- list(
        id = get_setting(settings, "subjectname"),
        sex = get_setting(settings, "sex"),
        age = get_setting(settings, "age"),
        date_of_birth = get_setting(settings, "dateofbirth"),
        height = get_setting(settings, "height"),
        mass = get_setting(settings, "mass"),
        limb = get_setting(settings, "limb"),
        side = get_setting(settings, "side"),
        dominance = get_setting(settings, "dominance"),
        race = get_setting(settings, "race")
      )

      # Fallback ID from file name if not in metadata
      if (is.na(subject_info$id) || subject_info$id == "") {
        subject_info$id <- tools::file_path_sans_ext(file_name)
      }

      return(list(
        success = TRUE,
        data = data,
        settings = settings,
        device_info = device_info,
        subject_info = subject_info,
        epoch_length = epoch_len,
        duration_hrs = duration_hrs,
        n_epochs = nrow(data)
      ))
    }

    # Helper: Format ETA
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    # Add files when selected
    observeEvent(input$files, {
      req(input$files)

      n_files <- nrow(input$files)
      start_time <- Sys.time()

      withProgress(message = "Loading files...", value = 0, {
        for (i in seq_len(n_files)) {
          file_info <- input$files[i, ]

          # Calculate ETA
          if (i > 1) {
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg_time <- elapsed / (i - 1)
            eta <- format_eta(avg_time * (n_files - i + 1))
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, " | ETA: ", eta, ")")
          } else {
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, ")")
          }
          setProgress(value = i / n_files, detail = detail_msg)

          result <- load_single_file(file_info$datapath, file_info$name)

          if (result$success) {
            file_id <- paste0("file_", local$next_id)

            shared$files[[file_id]] <- list(
              id = file_id,
              name = file_info$name,
              original_path = file_info$datapath,
              data = result$data,
              settings = result$settings,
              device_info = result$device_info,
              subject_info = result$subject_info,
              epoch_length = result$epoch_length,
              duration_hrs = result$duration_hrs,
              n_epochs = result$n_epochs
            )

            local$next_id <- local$next_id + 1
            shared$file_count <- length(shared$files)
            shared$data_loaded <- TRUE

            # Auto-select first file
            if (is.null(shared$selected_file)) {
              shared$selected_file <- file_id
            }
          } else {
            showNotification(paste("Error loading", file_info$name, ":", result$error), type = "error", duration = 5)
          }
        }
      })

      showNotification(paste(sum(sapply(input$files$name, function(x) TRUE)), "file(s) processed"), type = "message")
    })

    # Handle directory selection (files from folder via webkitdirectory)
    observeEvent(input$dir_files, {
      req(input$dir_files)

      # Filter to only supported file types
      all_files <- input$dir_files
      supported_ext <- c("agd", "gt3x", "csv")
      valid_idx <- which(tolower(tools::file_ext(all_files$name)) %in% supported_ext)

      if (length(valid_idx) == 0) {
        showNotification("No supported files (.agd, .gt3x, .csv) found in selected folder", type = "warning")
        return()
      }

      files_to_load <- all_files[valid_idx, ]
      n_files <- nrow(files_to_load)
      start_time <- Sys.time()

      withProgress(message = "Loading files from directory...", value = 0, {
        loaded <- 0
        for (i in seq_len(n_files)) {
          file_info <- files_to_load[i, ]

          # Calculate ETA
          if (i > 1) {
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg_time <- elapsed / (i - 1)
            eta <- format_eta(avg_time * (n_files - i + 1))
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, " | ETA: ", eta, ")")
          } else {
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, ")")
          }
          setProgress(value = i / n_files, detail = detail_msg)

          result <- load_single_file(file_info$datapath, file_info$name)

          if (result$success) {
            file_id <- paste0("file_", local$next_id)

            shared$files[[file_id]] <- list(
              id = file_id,
              name = file_info$name,
              original_path = file_info$datapath,
              data = result$data,
              settings = result$settings,
              device_info = result$device_info,
              subject_info = result$subject_info,
              epoch_length = result$epoch_length,
              duration_hrs = result$duration_hrs,
              n_epochs = result$n_epochs
            )

            local$next_id <- local$next_id + 1
            loaded <- loaded + 1

            if (is.null(shared$selected_file)) {
              shared$selected_file <- file_id
            }
          }
        }
        shared$file_count <- length(shared$files)
        shared$data_loaded <- length(shared$files) > 0

        # Memory cleanup
        gc(verbose = FALSE)
      })

      showNotification(paste(loaded, "of", n_files, "files loaded from directory"), type = "message")
    })

    # Demo files
    observeEvent(input$demo_btn, {
      withProgress(message = "Generating demo data...", value = 0, {
        for (i in 1:3) {
          setProgress(value = i / 3, detail = paste("Demo subject", i))

          n <- 4320 + sample(-500:500, 1)
          start_date <- as.POSIXct("2024-01-01 00:00:00") + (i - 1) * 86400 * 3
          ts <- seq(from = start_date, by = 60, length.out = n)
          hour <- as.numeric(format(ts, "%H"))

          # Vary activity patterns
          base_activity <- 400 + i * 50
          base <- ifelse(hour >= 7 & hour <= 22, base_activity, 30 + i * 10)

          a1 <- pmax(0, round(base + rnorm(n, 0, 200)))
          a2 <- pmax(0, round(base * 0.8 + rnorm(n, 0, 150)))
          a3 <- pmax(0, round(base * 0.6 + rnorm(n, 0, 100)))

          demo_data <- data.frame(
            timestamp = ts,
            axis1 = a1,
            axis2 = a2,
            axis3 = a3,
            vector_magnitude = round(sqrt(a1^2 + a2^2 + a3^2), 1),
            steps = rpois(n, ifelse(hour >= 7 & hour <= 22, 5 + i, 0.5))
          )

          file_id <- paste0("file_", local$next_id)

          shared$files[[file_id]] <- list(
            id = file_id,
            name = paste0("demo_subject_", i, ".agd"),
            original_path = NA,
            data = demo_data,
            settings = NULL,
            device_info = list(
              device_type = "Demo Device",
              serial_number = paste0("DEMO", sprintf("%05d", i)),
              firmware = "1.0.0",
              battery = "4.2V",
              filter = "Normal",
              software = "canhrActi Demo",
              software_version = as.character(packageVersion("canhrActi")),
              epoch_length = 60,
              start_datetime = NA,
              stop_datetime = NA,
              download_datetime = NA,
              sample_rate = "30",
              acceleration_scale = NA,
              acceleration_min = NA,
              acceleration_max = NA,
              modes = "Axis1, Axis2, Axis3, Steps"
            ),
            subject_info = list(
              id = paste0("DEMO_", sprintf("%03d", i)),
              sex = if (i %% 2 == 1) "Male" else "Female",
              age = 25 + i * 5,
              date_of_birth = NA,
              height = 165 + i * 5,
              mass = 65 + i * 5,
              limb = "Wrist",
              side = "Right",
              dominance = "Dominant",
              race = NA
            ),
            epoch_length = 60,
            duration_hrs = n / 60,
            n_epochs = n
          )

          local$next_id <- local$next_id + 1

          if (is.null(shared$selected_file)) {
            shared$selected_file <- file_id
          }
        }

        shared$file_count <- length(shared$files)
        shared$data_loaded <- TRUE
      })

      showNotification("3 demo files added!", type = "message")
    })

    # Clear all files
    observeEvent(input$clear_all_btn, {
      shared$files <- list()
      shared$file_count <- 0
      shared$selected_file <- NULL
      shared$data_loaded <- FALSE
      shared$results <- list(wear_time = list(), sleep = list(), activity = list(), circadian = list(), energy = list())
      local$next_id <- 1
      showNotification("All files cleared", type = "message")
    })

    # Remove selected files
    observeEvent(input$remove_selected_btn, {
      selected <- input$file_table_rows_selected
      req(selected)

      file_ids <- names(shared$files)
      ids_to_remove <- file_ids[selected]

      for (fid in ids_to_remove) {
        shared$files[[fid]] <- NULL
        # Clean up results
        shared$results$wear_time[[fid]] <- NULL
        shared$results$sleep[[fid]] <- NULL
        shared$results$activity[[fid]] <- NULL
        shared$results$circadian[[fid]] <- NULL
        shared$results$energy[[fid]] <- NULL
      }

      shared$file_count <- length(shared$files)
      shared$data_loaded <- length(shared$files) > 0

      if (shared$selected_file %in% ids_to_remove) {
        shared$selected_file <- if (length(shared$files) > 0) names(shared$files)[1] else NULL
      }

      showNotification(paste(length(ids_to_remove), "file(s) removed"), type = "message")
    })

    # View selected file
    observeEvent(input$view_selected_btn, {
      selected <- input$file_table_rows_selected
      req(selected, length(selected) == 1)

      file_ids <- names(shared$files)
      shared$selected_file <- file_ids[selected]
    })

    # Also select on row click
    observeEvent(input$file_table_rows_selected, {
      selected <- input$file_table_rows_selected
      if (length(selected) == 1) {
        file_ids <- names(shared$files)
        shared$selected_file <- file_ids[selected]
      }
    })

    # Value boxes
    output$vb_file_count <- renderValueBox({
      valueBox(shared$file_count, "Files Loaded", icon = icon("file"), color = "blue")
    })

    output$vb_total_duration <- renderValueBox({
      if (shared$file_count == 0) {
        valueBox("--", "Total Duration", icon = icon("clock"), color = "green")
      } else {
        total_hrs <- sum(sapply(shared$files, function(f) f$duration_hrs), na.rm = TRUE)
        valueBox(paste0(round(total_hrs, 1), "h"), "Total Duration", icon = icon("clock"), color = "green")
      }
    })

    output$vb_total_epochs <- renderValueBox({
      if (shared$file_count == 0) {
        valueBox("--", "Total Epochs", icon = icon("list"), color = "yellow")
      } else {
        total <- sum(sapply(shared$files, function(f) f$n_epochs), na.rm = TRUE)
        valueBox(format(total, big.mark = ","), "Total Epochs", icon = icon("list"), color = "yellow")
      }
    })

    output$vb_avg_epoch_len <- renderValueBox({
      if (shared$file_count == 0) {
        valueBox("--", "Epoch Length", icon = icon("stopwatch"), color = "purple")
      } else {
        epochs <- sapply(shared$files, function(f) f$epoch_length)
        avg_epoch <- round(mean(epochs, na.rm = TRUE))
        shared$epoch_length <- avg_epoch
        valueBox(paste0(avg_epoch, " sec"), "Epoch Length", icon = icon("stopwatch"), color = "purple")
      }
    })

    # File list table
    output$file_table <- DT::renderDataTable({
      if (shared$file_count == 0) {
        return(DT::datatable(data.frame(Message = "No files loaded. Add files or load demo data."), rownames = FALSE))
      }

      df <- data.frame(
        File = sapply(shared$files, function(f) f$name),
        Subject = sapply(shared$files, function(f) f$subject_info$id),
        Duration = sapply(shared$files, function(f) paste0(round(f$duration_hrs, 1), "h")),
        Epochs = sapply(shared$files, function(f) format(f$n_epochs, big.mark = ",")),
        Epoch_Len = sapply(shared$files, function(f) paste0(f$epoch_length, "s")),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        df,
        selection = "multiple",
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("File Name", "Subject ID", "Duration", "Epochs", "Epoch")
      )
    })

    # Device Information Display (like ActiLife)
    output$device_info_display <- renderUI({
      if (is.null(shared$selected_file) || is.null(shared$files[[shared$selected_file]])) {
        return(tags$div(class = "text-center text-muted", style = "padding: 15px;",
                        tags$p("Select a file to view device information")))
      }

      f <- shared$files[[shared$selected_file]]
      dev <- f$device_info

      # Format first and last epoch times
      first_epoch <- if (!is.na(dev$start_datetime)) format_agd_timestamp(dev$start_datetime) else "N/A"
      last_epoch <- if (!is.na(dev$stop_datetime)) format_agd_timestamp(dev$stop_datetime) else "N/A"

      # If timestamps not in settings, get from data
      if (first_epoch == "N/A" && "timestamp" %in% names(f$data) && nrow(f$data) > 0) {
        first_epoch <- format(min(f$data$timestamp), "%m/%d/%Y %I:%M %p")
        last_epoch <- format(max(f$data$timestamp), "%m/%d/%Y %I:%M %p")
      }

      # Battery formatting
      battery_str <- if (!is.na(dev$battery)) paste0(dev$battery, "V") else "N/A"
      if (!is.na(dev$battery) && is.numeric(as.numeric(dev$battery))) {
        battery_str <- sprintf("%.2fV", as.numeric(dev$battery))
      }

      tags$div(
        style = "font-size: 12px; overflow-x: auto;",
        tags$table(class = "table table-condensed table-bordered",
                   style = "margin-bottom: 0; table-layout: fixed; width: 100%;",
          tags$colgroup(
            tags$col(style = "width: 25%;"),
            tags$col(style = "width: 25%;"),
            tags$col(style = "width: 25%;"),
            tags$col(style = "width: 25%;")
          ),
          tags$tbody(
            tags$tr(tags$td(tags$strong("Device Type:")),
                    tags$td(style = "word-wrap: break-word;", dev$device_type %||% "N/A"),
                    tags$td(tags$strong("Epoch Length:")),
                    tags$td(style = "word-wrap: break-word;", paste0(dev$epoch_length, " sec"))),
            tags$tr(tags$td(tags$strong("Serial Number:")),
                    tags$td(style = "word-wrap: break-word;", dev$serial_number %||% "N/A"),
                    tags$td(tags$strong("First Epoch:")),
                    tags$td(style = "word-wrap: break-word;", first_epoch)),
            tags$tr(tags$td(tags$strong("Epoch Count:")),
                    tags$td(style = "word-wrap: break-word;", format(f$n_epochs, big.mark = ",")),
                    tags$td(tags$strong("Last Epoch:")),
                    tags$td(style = "word-wrap: break-word;", last_epoch)),
            tags$tr(tags$td(tags$strong("Firmware:")),
                    tags$td(style = "word-wrap: break-word;", dev$firmware %||% "N/A"),
                    tags$td(tags$strong("Sample Rate:")),
                    tags$td(style = "word-wrap: break-word;", if (!is.na(dev$sample_rate)) paste0(dev$sample_rate, " Hz") else "N/A")),
            tags$tr(tags$td(tags$strong("Battery:")),
                    tags$td(style = "word-wrap: break-word;", battery_str),
                    tags$td(tags$strong("Filter:")),
                    tags$td(style = "word-wrap: break-word;", dev$filter %||% "Normal")),
            tags$tr(tags$td(tags$strong("Software:")),
                    tags$td(style = "word-wrap: break-word;", paste0(dev$software %||% "", " ", dev$software_version %||% "")),
                    tags$td(tags$strong("Modes:")),
                    tags$td(style = "word-wrap: break-word;", dev$modes %||% "N/A"))
          )
        )
      )
    })

    # Subject Biometric Information Display (like ActiLife)
    output$subject_info_display <- renderUI({
      if (is.null(shared$selected_file) || is.null(shared$files[[shared$selected_file]])) {
        return(tags$div(class = "text-center text-muted", style = "padding: 15px;",
                        tags$p("Select a file to view subject information")))
      }

      f <- shared$files[[shared$selected_file]]
      subj <- f$subject_info

      # Format values
      sex_formatted <- format_sex(subj$sex)
      height_formatted <- format_height(subj$height)
      weight_formatted <- format_weight(subj$mass)
      age_str <- if (!is.na(subj$age) && subj$age != "0") subj$age else "N/A"
      dob_str <- if (!is.na(subj$date_of_birth)) format_agd_timestamp(subj$date_of_birth) else "N/A"
      limb_str <- if (!is.na(subj$limb) && subj$limb != "") subj$limb else "N/A"
      side_str <- if (!is.na(subj$side) && subj$side != "") subj$side else "N/A"
      dominance_str <- if (!is.na(subj$dominance) && subj$dominance != "") subj$dominance else "N/A"
      race_str <- if (!is.na(subj$race) && subj$race != "") subj$race else "N/A"

      tags$div(
        style = "font-size: 12px; overflow-x: auto;",
        tags$table(class = "table table-condensed table-bordered",
                   style = "margin-bottom: 0; table-layout: fixed; width: 100%;",
          tags$colgroup(
            tags$col(style = "width: 25%;"),
            tags$col(style = "width: 25%;"),
            tags$col(style = "width: 25%;"),
            tags$col(style = "width: 25%;")
          ),
          tags$tbody(
            tags$tr(tags$td(tags$strong("Subject Name:")),
                    tags$td(colspan = "3", style = "word-wrap: break-word;", subj$id %||% "N/A")),
            tags$tr(tags$td(tags$strong("Gender:")),
                    tags$td(style = "word-wrap: break-word;", sex_formatted),
                    tags$td(tags$strong("Date of Birth:")),
                    tags$td(style = "word-wrap: break-word;", dob_str)),
            tags$tr(tags$td(tags$strong("Height:")),
                    tags$td(style = "word-wrap: break-word;", height_formatted),
                    tags$td(tags$strong("Age:")),
                    tags$td(style = "word-wrap: break-word;", age_str)),
            tags$tr(tags$td(tags$strong("Weight:")),
                    tags$td(style = "word-wrap: break-word;", weight_formatted),
                    tags$td(tags$strong("Race:")),
                    tags$td(style = "word-wrap: break-word;", race_str)),
            tags$tr(tags$td(tags$strong("Limb:")),
                    tags$td(style = "word-wrap: break-word;", limb_str),
                    tags$td(tags$strong("Side:")),
                    tags$td(style = "word-wrap: break-word;", side_str)),
            tags$tr(tags$td(tags$strong("Dominance:")),
                    tags$td(colspan = "3", style = "word-wrap: break-word;", dominance_str))
          )
        )
      )
    })

    # Data preview table
    output$preview_table <- DT::renderDataTable({
      req(shared$selected_file, shared$files[[shared$selected_file]])

      data <- shared$files[[shared$selected_file]]$data
      display <- head(data, 500)

      if ("timestamp" %in% names(display)) {
        display$timestamp <- format(display$timestamp, "%Y-%m-%d %H:%M:%S")
      }

      DT::datatable(display, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}

# Null coalesce operator
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
