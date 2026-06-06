# 
# Upload Module - Redesigned UI/UX
# 
# Design Philosophy:
#   1. Progressive Disclosure - Start simple, reveal complexity as needed
#   2. Hero Upload Area - Make the primary action obvious and inviting
#   3. Visual Hierarchy - Upload > File List > Details
#   4. Clean, Card-Based Design - Modern, scannable interface
#   5. Minimal Metrics - Only show what matters
# 

mod_upload_ui <- function(id) {

  ns <- NS(id)

  tagList(
    div(
      class = "upload-shell",
      # SECTION 1: HERO UPLOAD ZONE
      # Design Decision: Large, centered upload area is the focal point.
      # Progressive disclosure: This is all users see initially.
      div(
        class = "upload-hero",
        id = ns("upload_zone"),
        # Accessibility: describe the upload zone
        role = "region",
        `aria-label` = "File upload area",
        # Clicking anywhere in the hero opens the file picker. The "Choose
        # Files" label is still the primary keyboard-accessible affordance;
        # this just makes the visible cursor: pointer feedback honest.
        onclick = paste0("document.getElementById('", ns("files"), "').click()"),
        div(class = "upload-hero-icon", icon("cloud-upload-alt"), `aria-hidden` = "true"),
        div(class = "upload-hero-title", id = ns("upload_title"), "Import Accelerometer Data"),
        div(class = "upload-hero-subtitle",
            "Drag and drop data files here, or use the options below"),

        # Hidden file input that covers the entire hero zone
        fileInput(
          ns("files"),
          NULL,
          multiple = TRUE,
          accept = c(".agd", ".gt3x"),
          buttonLabel = "",
          placeholder = ""
        )
      ),

      # SECTION 2: ACTION BUTTONS
      # Design Decision: Secondary actions below hero zone.
      # Demo button prominent for first-time users.
      div(
        class = "upload-actions",

        # Browse files button (individual selection)
        tags$label(
          class = "btn btn-upload-action btn-browse",
          `for` = ns("files"),
          icon("file-upload"), "Choose Files"
        ),

        # Folder selection button
        tags$label(
          class = "btn btn-upload-action btn-folder",
          icon("folder-open"), "Choose Folder",
          tags$input(
            type = "file",
            id = ns("dir_files"),
              # Note: webkitdirectory only works in Chrome/Edge browsers
            webkitdirectory = TRUE,
            multiple = TRUE,
            class = "hidden",
            accept = ".agd,.gt3x"
          )
        ),

        # Example data button
        actionButton(
          ns("demo_btn"),
          span(icon("database"), "Try Sample Files"),
          class = "btn-upload-action btn-demo"
        ),

        # Convert a raw .gt3x to a small .agd
        actionButton(
          ns("convert_btn"),
          span(icon("right-left"), "Convert .gt3x → .agd"),
          class = "btn-upload-action btn-convert"
        )
      ),

      div(
        class = "upload-helper",
        icon("info-circle"),
        "Supports .agd and raw .gt3x files. Raw .gt3x files are converted to counts on import."
      )
    ),

    # SECTION 3: METRICS STRIP (Conditional - only shown when files loaded)
    # Design Decision: Minimal, horizontal strip with only essential metrics.
    # Replaces the 4 bulky value boxes with a clean, compact design.
    uiOutput(ns("metrics_strip")),

    # SECTION 4: MAIN CONTENT AREA (Two columns when files loaded)
    # Design Decision: File list on left, details on right.
    # Uses progressive disclosure - only shown when files exist.
    uiOutput(ns("main_content"))
  )
}

mod_upload_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # LOCAL STATE
    local <- reactiveValues(
      next_id = 1,
      active_tab = "device"  # Track which details tab is active
    )

    # HELPER FUNCTIONS

    # Get setting value from settings data frame
    get_setting <- function(settings, name) {
      if (is.null(settings) || !is.data.frame(settings)) return(NA)
      value <- settings$settingValue[tolower(settings$settingName) == tolower(name)]
      if (length(value) == 0) return(NA)
      value <- as.character(value)
      if (value == "" || value == "0") return(NA)
      return(value)
    }

    # Format height from cm to ft/in
    format_height <- function(height_cm) {
      if (is.null(height_cm) || length(height_cm) == 0) return("N/A")
      height_cm <- suppressWarnings(as.numeric(height_cm))
      if (is.na(height_cm) || height_cm <= 0) return("N/A")
      total_inches <- height_cm / 2.54
      feet <- floor(total_inches / 12)
      inches <- round(total_inches %% 12)
      paste0(feet, "ft ", inches, "in")
    }

    # Format weight from kg to lbs
    format_weight <- function(mass_kg) {
      if (is.null(mass_kg) || length(mass_kg) == 0) return("N/A")
      mass_kg <- suppressWarnings(as.numeric(mass_kg))
      if (is.na(mass_kg) || mass_kg <= 0) return("N/A")
      lbs <- round(mass_kg * 2.20462)
      paste0(lbs, " lbs")
    }

    # Format sex
    format_sex <- function(sex) {
      if (is.null(sex) || length(sex) == 0) return("N/A")
      if (is.na(sex) || sex == "" || sex == "0") return("N/A")
      sex_lower <- tolower(as.character(sex))
      if (sex_lower %in% c("male", "m", "1")) return("Male")
      if (sex_lower %in% c("female", "f", "2")) return("Female")
      return(sex)
    }

    # Format timestamp from AGD format
    format_agd_timestamp <- function(ts_value) {
      if (is.na(ts_value)) return("N/A")
      ts_numeric <- as.numeric(ts_value)
      if (is.na(ts_numeric)) return("N/A")
      ts <- as.POSIXct((ts_numeric / 10000000 - 62135596800), origin = '1970-01-01', tz = 'UTC')
      format(ts, "%m/%d/%Y %I:%M %p")
    }

    # Format ETA for progress
    format_eta <- function(seconds) {
      if (is.na(seconds) || seconds < 0) return("calculating...")
      if (seconds < 60) return(paste0(round(seconds), "s"))
      if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
      return(paste0(round(seconds / 3600, 1), "h"))
    }

    # Load a single file
    # Memory management: Large files (>100MB) will show a warning as they may
    # cause slow UI responsiveness. For async processing support, install the
    # future and promises packages and enable: future::plan(future::multisession)
    load_single_file <- function(file_path, file_name) {
      ext <- tolower(tools::file_ext(file_name))

      # Check file size for memory management
      file_size_mb <- file.info(file_path)$size / (1024 * 1024)
      if (!is.na(file_size_mb) && file_size_mb > 100) {
        warning("Large file detected (", round(file_size_mb, 1), " MB): ",
                file_name, ". Processing may take longer and use significant memory.")
      }

      result <- tryCatch({
        if (ext == "agd") {
          canhrActi::read.agd(file_path)
        } else {
          # .gt3x is converted to .agd upstream (convert_and_load); only .agd reaches here.
          stop("Unsupported file type: ", ext, ". Only .agd files are supported here.")
        }
      }, error = function(e) {
        return(list(error = e$message))
      })

      if (!is.null(result$error)) {
        return(list(success = FALSE, error = result$error))
      }

      # Extract raw data and settings
      sleep_data <- NULL
      awakenings_data <- NULL
      wear_time_data <- NULL
      capsense_data <- NULL

      if (is.list(result) && "data" %in% names(result)) {
        raw_data <- result$data
        settings <- result$settings
        if (!is.null(result$sleep)) sleep_data <- result$sleep
        if (!is.null(result$awakenings)) awakenings_data <- result$awakenings
        if (!is.null(result$wear_time)) wear_time_data <- result$wear_time
        if (!is.null(result$capsense)) capsense_data <- result$capsense
      } else {
        raw_data <- result
        settings <- NULL
      }

      # Convert data to standard format with timestamps
      if ("dataTimestamp" %in% names(raw_data)) {
        a1 <- if ("axis1" %in% names(raw_data)) raw_data$axis1 else NA
        a2 <- if ("axis2" %in% names(raw_data)) raw_data$axis2 else NA
        a3 <- if ("axis3" %in% names(raw_data)) raw_data$axis3 else NA

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
        data <- data[, colSums(!is.na(data)) > 0]

      } else if ("timestamp" %in% names(raw_data)) {
        if (all(c("axis1", "axis2", "axis3") %in% names(raw_data)) &&
            !"vector_magnitude" %in% names(raw_data)) {
          raw_data$vector_magnitude <- round(sqrt(raw_data$axis1^2 + raw_data$axis2^2 + raw_data$axis3^2), 1)
        }
        data <- raw_data
      } else {
        data <- raw_data
      }

      # Detect epoch length
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

      # Extract device info
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

      # Extract subject info
      mass_val <- get_setting(settings, "mass")
      # Convert mass from kg to lbs for exports
      mass_kg <- suppressWarnings(as.numeric(mass_val))
      weight_lbs_val <- if (!is.na(mass_kg) && mass_kg > 0) round(mass_kg * 2.20462) else 0

      subject_info <- list(
        id = get_setting(settings, "subjectname"),
        sex = get_setting(settings, "sex"),
        age = get_setting(settings, "age"),
        date_of_birth = get_setting(settings, "dateofbirth"),
        height = get_setting(settings, "height"),
        mass = mass_val,
        weight_lbs = weight_lbs_val,  # Added for exports
        limb = get_setting(settings, "limb"),
        side = get_setting(settings, "side"),
        dominance = get_setting(settings, "dominance"),
        race = get_setting(settings, "race")
      )

      # Fallback ID from file name
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
        n_epochs = nrow(data),
        actilife_sleep = sleep_data,
        actilife_awakenings = awakenings_data,
        actilife_wear_time = wear_time_data,
        capsense = capsense_data
      ))
    }

    # FILE UPLOAD HANDLERS

    # Raw .gt3x conversion: content-hash cache + background (ExtendedTask).
    gt3x_cache_dir <- file.path(tempdir(), "canhrActi_gt3x_cache")
    dir.create(gt3x_cache_dir, showWarnings = FALSE, recursive = TRUE)

    add_file_record <- function(result, display_name, original_path) {
      if (!isTRUE(result$success)) {
        showNotification(paste0("Could not load ", display_name, ": ",
                                result$error %||% "unknown error"), type = "error", duration = 8)
        return(invisible())
      }
      file_id <- paste0("file_", local$next_id)
      shared$files[[file_id]] <- list(
        id = file_id, name = display_name, original_path = original_path,
        data = result$data, settings = result$settings,
        device_info = result$device_info, subject_info = result$subject_info,
        epoch_length = result$epoch_length, duration_hrs = result$duration_hrs,
        n_epochs = result$n_epochs, actilife_sleep = result$actilife_sleep,
        actilife_awakenings = result$actilife_awakenings,
        actilife_wear_time = result$actilife_wear_time, capsense = result$capsense
      )
      local$next_id <- local$next_id + 1
      shared$file_count <- length(shared$files)
      shared$data_loaded <- TRUE
      if (is.null(shared$selected_file)) shared$selected_file <- file_id
    }

    # Bounded conversion pool: small files convert in parallel, large files
    # (>50MB) run solo, so peak RAM stays safe (each conversion holds the full
    # raw signal, ~3GB at 30Hz and far more at higher rates).
    POOL_N   <- 2
    LARGE_MB <- 50
    conv_queue <- reactiveVal(list())
    pool <- lapply(seq_len(POOL_N), function(i) {
      ExtendedTask$new(function(datapath, out_agd, epoch, lfe, libpaths, name, orig) {
        promises::catch(
          promises::future_promise({
            .libPaths(libpaths)
            canhrActi::gt3x.to.agd(datapath, agd_path = out_agd, epoch = epoch, lfe = lfe)
            list(ok = TRUE, agd = out_agd, name = name, orig = orig)
          }, seed = TRUE),
          function(e) list(ok = FALSE, name = name, error = conditionMessage(e))
        )
      })
    })
    pool_large <- reactiveVal(rep(FALSE, POOL_N))   # slots currently running a large file

    pump_pool <- function() isolate({
      repeat {
        q <- conv_queue()
        if (length(q) == 0) break
        busy <- vapply(pool, function(t) t$status() == "running", logical(1))
        if (any(busy & pool_large())) break          # a large file is running -> wait
        job  <- q[[1]]
        if (job$large && any(busy)) break             # large waits for a fully idle pool
        free <- which(!busy)[1]
        if (is.na(free)) break
        conv_queue(q[-1])
        pl <- pool_large(); pl[free] <- job$large; pool_large(pl)
        pool[[free]]$invoke(job$datapath, job$cached, job$epoch, job$lfe, .libPaths(), job$name, job$datapath)
        if (job$large) break                          # large runs solo
      }
    })

    lapply(seq_len(POOL_N), function(i) {
      observeEvent(pool[[i]]$status(), {
        if (pool[[i]]$status() != "success") return()
        r  <- pool[[i]]$result()
        isolate({ pl <- pool_large(); pl[i] <- FALSE; pool_large(pl) })
        if (isTRUE(r$ok)) {
          add_file_record(load_single_file(r$agd, "converted.agd"), r$name, r$orig)
          showNotification(paste0("Converted ", r$name), type = "message", duration = 3)
        } else {
          showNotification(paste0("Conversion failed for ", r$name, ": ", r$error),
                           type = "error", duration = 8)
        }
        pump_pool()
      })
    })

    hash_memo <- new.env(parent = emptyenv())   # content hash per upload (files don't change in-session)
    cache_path <- function(datapath, epoch, lfe) {
      h <- hash_memo[[datapath]]
      if (is.null(h)) {
        h <- digest::digest(file = datapath, algo = "xxhash64")
        hash_memo[[datapath]] <- h
      }
      file.path(gt3x_cache_dir, paste0(h, "_e", epoch, "_", if (lfe) "l" else "n", ".agd"))
    }

    # Cached .agd path for a .gt3x, converting it once if not already cached.
    ensure_agd <- function(datapath, epoch, lfe) {
      cached <- cache_path(datapath, epoch, lfe)
      if (!file.exists(cached)) {
        canhrActi::gt3x.to.agd(datapath, agd_path = cached, epoch = epoch, lfe = lfe)
      }
      cached
    }

    # .agd output name for a .gt3x file name.
    agd_name <- function(nm, ep) sub("\\.gt3x$", sprintf("_%ssec.agd", ep), nm, ignore.case = TRUE)

    # Convert a .gt3x and load it into the dashboard (cache hit loads instantly).
    convert_and_load <- function(datapath, name, epoch = 60, lfe = FALSE, notify = TRUE) {
      cached <- cache_path(datapath, epoch, lfe)
      if (file.exists(cached)) {
        add_file_record(load_single_file(cached, "converted.agd"), name, datapath)
        return(invisible())
      }
      large <- isTRUE((file.info(datapath)$size / 1048576) > LARGE_MB)
      isolate({ q <- conv_queue(); q[[length(q) + 1]] <- list(
        datapath = datapath, cached = cached, name = name, epoch = epoch, lfe = lfe, large = large)
        conv_queue(q) })
      if (notify) showNotification(paste0("Converting ", name, " in the background…"),
                                   type = "message", duration = 4)
      pump_pool()
    }

    # Route an uploaded file: .agd loads now; .gt3x auto-converts (Normal, 60s) and loads.
    process_uploaded_file <- function(datapath, name) {
      if (tolower(tools::file_ext(name)) == "gt3x") {
        convert_and_load(datapath, name, epoch = 60, lfe = FALSE)
      } else {
        add_file_record(load_single_file(datapath, name), name, datapath)
      }
    }

    # Handle file upload
    observeEvent(input$files, {
      req(input$files)

      n_files <- nrow(input$files)
      start_time <- Sys.time()

      withProgress(message = "Loading files...", value = 0, {
        for (i in seq_len(n_files)) {
          file_info <- input$files[i, ]

          if (i > 1) {
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg_time <- elapsed / (i - 1)
            eta <- format_eta(avg_time * (n_files - i + 1))
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, " | ETA: ", eta, ")")
          } else {
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, ")")
          }
          setProgress(value = i / n_files, detail = detail_msg)

          process_uploaded_file(file_info$datapath, file_info$name)
        }
      })

      showNotification(paste(n_files, "file(s) processed"), type = "message")
    })

    # Handle directory selection
    observeEvent(input$dir_files, {
      req(input$dir_files)

      all_files <- input$dir_files
      supported_ext <- c("agd", "gt3x")
      valid_idx <- which(tolower(tools::file_ext(all_files$name)) %in% supported_ext)

      if (length(valid_idx) == 0) {
        showNotification("No .agd or .gt3x files found in selected folder", type = "warning")
        return()
      }

      files_to_load <- all_files[valid_idx, ]
      n_files <- nrow(files_to_load)
      start_time <- Sys.time()

      withProgress(message = "Loading files from directory...", value = 0, {
        loaded <- 0
        for (i in seq_len(n_files)) {
          file_info <- files_to_load[i, ]

          if (i > 1) {
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            avg_time <- elapsed / (i - 1)
            eta <- format_eta(avg_time * (n_files - i + 1))
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, " | ETA: ", eta, ")")
          } else {
            detail_msg <- paste0(file_info$name, " (", i, "/", n_files, ")")
          }
          setProgress(value = i / n_files, detail = detail_msg)

          process_uploaded_file(file_info$datapath, file_info$name)
          loaded <- loaded + 1
        }
        shared$file_count <- length(shared$files)
        shared$data_loaded <- length(shared$files) > 0
        # gc() removed - R handles this automatically
      })

      showNotification(paste(loaded, "of", n_files, "files loaded from directory"), type = "message")
    })

    # Load example AGD files
    observeEvent(input$demo_btn, {
      withProgress(message = "Loading example data...", value = 0, {
        data_dir <- "data"
        example_files <- list.files(data_dir, pattern = "\\.agd$", full.names = FALSE)
        n_files <- length(example_files)
        loaded <- 0

        for (i in seq_along(example_files)) {
          setProgress(value = i / n_files, detail = example_files[i])

          filepath <- file.path(data_dir, example_files[i])
          result <- load_single_file(filepath, example_files[i])

          if (!result$success) {
            showNotification(paste("Error:", example_files[i]), type = "error")
            next
          }

          file_id <- paste0("file_", local$next_id)

          shared$files[[file_id]] <- list(
            id = file_id,
            name = result$name %||% example_files[i],
            original_path = filepath,
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

        shared$file_count <- length(shared$files)
        shared$data_loaded <- TRUE
      })

      showNotification(paste(loaded, "example files loaded!"), type = "message")
    })

    # Convert a raw .gt3x: load it into the dashboard, and/or download the .agd.
    observeEvent(input$convert_btn, {
      showModal(modalDialog(
        title = NULL, footer = NULL, size = "m", easyClose = TRUE,
        div(class = "convert-modal",
          div(class = "convert-modal-header",
            div(class = "convert-modal-icon", icon("right-left")),
            div(
              h4(class = "convert-modal-title", "Convert Raw .gt3x"),
              p(class = "convert-modal-sub", "Compute activity counts from a raw accelerometer file")
            )
          ),
          div(class = "convert-field",
            tags$label(class = "convert-label", "Raw file(s)"),
            fileInput(ns("convert_gt3x"), NULL, accept = ".gt3x", multiple = TRUE, width = "100%"),
            tags$label(class = "btn btn-convert-folder",
              icon("folder-open"), span(" Choose a folder of .gt3x instead"),
              tags$input(type = "file", id = ns("convert_dir"), webkitdirectory = TRUE,
                         multiple = TRUE, class = "hidden", accept = ".gt3x")
            )
          ),
          div(class = "convert-options",
            div(class = "convert-field",
              tags$label(class = "convert-label", "Epoch length"),
              selectInput(ns("convert_epoch"), NULL, width = "100%", selected = 60,
                          choices = c("5 sec" = 5, "10 sec" = 10, "15 sec" = 15,
                                      "30 sec" = 30, "60 sec" = 60))
            ),
            div(class = "convert-field",
              tags$label(class = "convert-label", "Filter"),
              selectInput(ns("convert_filter"), NULL, width = "100%", selected = "normal",
                          choices = c("Normal" = "normal", "Low-frequency extension" = "lfe"))
            )
          ),
          div(class = "convert-note",
            icon("circle-info"),
            span(tags$b("Low-frequency extension"),
                 " captures slow, gentle movement (for older or clinical groups); keep ",
                 tags$b("Normal"), " for standard analysis. Large files take a while to convert.")
          ),
          div(class = "convert-modal-footer",
            tags$button(type = "button", class = "btn btn-convert-cancel",
                        `data-dismiss` = "modal", "Cancel"),
            downloadButton(ns("download_agd"), "Download", class = "btn-convert-secondary"),
            actionButton(ns("convert_load"), span(icon("chart-line"), " Convert & Analyze"),
                         class = "btn-convert-primary")
          )
        )
      ))
    })

    # The selected .gt3x files (from the file picker and/or the folder picker).
    selected_gt3x <- reactive({
      combined <- rbind(input$convert_gt3x, input$convert_dir)
      if (is.null(combined) || nrow(combined) == 0) return(NULL)
      combined[tolower(tools::file_ext(combined$name)) == "gt3x", , drop = FALSE]
    })

    # Convert every selected file and load each into the dashboard for analysis.
    observeEvent(input$convert_load, {
      sel <- selected_gt3x()
      if (is.null(sel) || nrow(sel) == 0) {
        showNotification("Choose a .gt3x file or a folder first.", type = "warning")
        return()
      }
      ep <- as.numeric(input$convert_epoch %||% 60); lf <- isTRUE(input$convert_filter == "lfe")
      for (i in seq_len(nrow(sel))) {
        convert_and_load(sel$datapath[i], sel$name[i], epoch = ep, lfe = lf, notify = FALSE)
      }
      removeModal()
      showNotification(sprintf("Converting %d file(s) in the background…", nrow(sel)),
                       type = "message", duration = 4)
    })

    output$download_agd <- downloadHandler(
      filename = function() {
        sel <- selected_gt3x(); ep <- input$convert_epoch %||% "60"
        if (is.null(sel) || nrow(sel) <= 1) {
          agd_name(if (!is.null(sel) && nrow(sel) >= 1) sel$name[1] else "converted.gt3x", ep)
        } else {
          sprintf("converted_%ssec_agd.zip", ep)
        }
      },
      content = function(file) {
        sel <- selected_gt3x()
        if (is.null(sel) || nrow(sel) == 0) stop("Choose a .gt3x file or folder first.")
        ep <- as.numeric(input$convert_epoch %||% 60); lf <- isTRUE(input$convert_filter == "lfe")
        agds <- character(nrow(sel)); outs <- character(nrow(sel))
        withProgress(message = "Converting to .agd…", value = 0, {
          for (i in seq_len(nrow(sel))) {
            setProgress(value = i / nrow(sel), detail = sel$name[i])
            agds[i] <- ensure_agd(sel$datapath[i], ep, lf)
            outs[i] <- agd_name(sel$name[i], ep)
          }
        })
        if (length(agds) == 1) {
          file.copy(agds[1], file, overwrite = TRUE)
        } else {
          stage <- tempfile("agd_zip_"); dir.create(stage)
          for (j in seq_along(agds)) file.copy(agds[j], file.path(stage, outs[j]), overwrite = TRUE)
          zip::zipr(zipfile = file, root = stage, files = outs)
        }
      }
    )

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

    # FILE CARD CLICK HANDLERS
    # These are generated dynamically for each file card

    # Track observed files to prevent memory leak from duplicate observers
    observed_files <- reactiveVal(character(0))

    #  Clean up observed_files when files are removed
    # This prevents memory accumulation when files are removed and re-added
    observe({
      current_files <- names(shared$files)
      already_observed <- observed_files()
      # Remove files from tracking that no longer exist
      still_valid <- intersect(already_observed, current_files)
      if (length(still_valid) < length(already_observed)) {
        observed_files(still_valid)
      }
    })

    # Create observers only for NEW files
    observe({
      file_ids <- names(shared$files)
      already_observed <- observed_files()
      new_files <- setdiff(file_ids, already_observed)

      if (length(new_files) > 0) {
        lapply(new_files, function(fid) {
          #  Added once = TRUE to select observer to prevent duplicates
          observeEvent(input[[paste0("select_", fid)]], {
            shared$selected_file <- fid
          }, ignoreInit = TRUE, once = FALSE)  # once = FALSE is OK here since we track observed_files

          observeEvent(input[[paste0("remove_", fid)]], {
            shared$files[[fid]] <- NULL
            shared$results$wear_time[[fid]] <- NULL
            shared$results$sleep[[fid]] <- NULL
            shared$results$activity[[fid]] <- NULL
            shared$results$circadian[[fid]] <- NULL
            shared$results$energy[[fid]] <- NULL

            shared$file_count <- length(shared$files)
            shared$data_loaded <- length(shared$files) > 0

            if (identical(shared$selected_file, fid)) {
              shared$selected_file <- if (length(shared$files) > 0) names(shared$files)[1] else NULL
            }

            # Remove from observed_files so it can be re-added
            observed_files(setdiff(observed_files(), fid))

            showNotification("File removed", type = "message")
          }, ignoreInit = TRUE, once = TRUE)
        })
        observed_files(union(already_observed, new_files))
      }
    })

    # Tab switching observers
    # Consolidated tab observers
    observeEvent(input$tab_device, { local$active_tab <- "device" })
    observeEvent(input$tab_subject, { local$active_tab <- "subject" })
    observeEvent(input$tab_preview, { local$active_tab <- "preview" })
    #     observeEvent(input$tab_subject, { local$active_tab <- "subject" }) # Consolidated above
    #     observeEvent(input$tab_preview, { local$active_tab <- "preview" }) # Consolidated above

    # RENDER: METRICS STRIP
    # Design Decision: Only shown when files are loaded. Compact horizontal strip.
    output$metrics_strip <- renderUI({
      req(shared$file_count > 0)

      total_hrs <- sum(sapply(shared$files, function(f) f$duration_hrs), na.rm = TRUE)
      total_epochs <- sum(sapply(shared$files, function(f) {
        n <- f$n_epochs
        if (is.null(n) || !is.numeric(n)) return(0)
        as.numeric(n[1])
      }), na.rm = TRUE)

      div(
        class = "metrics-strip",

        # Files count
        div(
          class = "metric-item",
          div(class = "metric-icon files", icon("file-alt")),
          div(
            div(class = "metric-value", shared$file_count),
            div(class = "metric-label", if (shared$file_count == 1) "File" else "Files")
          )
        ),

        # Total duration
        div(
          class = "metric-item",
          div(class = "metric-icon duration", icon("clock")),
          div(
            div(class = "metric-value", paste0(round(total_hrs, 1), "h")),
            div(class = "metric-label", "Recording Time")
          )
        ),

        # Total epochs
        div(
          class = "metric-item",
          div(class = "metric-icon epochs", icon("table")),
          div(
            div(class = "metric-value", format(total_epochs, big.mark = ",")),
            div(class = "metric-label", "Data Points")
          )
        )
      )
    })

    # RENDER: MAIN CONTENT (File List + Details Panel)
    # Design Decision: Two-column layout only when files exist.
    # Progressive disclosure - hidden when no files.
    output$main_content <- renderUI({
      req(shared$file_count > 0)

      fluidRow(
        # Left column: File List
        column(
          width = 5,
          div(
            class = "file-list-container",

            # Header with actions
            div(
              class = "file-list-header",
              h4(class = "file-list-title", icon("list"), "Imported Files"),
              div(
                class = "file-list-actions",
                actionButton(
                  ns("clear_all_btn"),
                  span(icon("trash-alt"), "Remove All"),
                  class = "btn btn-outline-danger btn-sm"
                )
              )
            ),

            # File cards
            div(
              class = "file-list-scroll",
              lapply(names(shared$files), function(fid) {
                f <- shared$files[[fid]]
                is_selected <- identical(shared$selected_file, fid)

                div(
                  class = paste("file-card", if (is_selected) "selected" else ""),
                  # Accessibility: keyboard navigation and ARIA attributes
                  tabindex = "0",
                  role = "button",
                  `aria-pressed` = tolower(as.character(is_selected)),
                  `aria-label` = paste("Select file", f$name),
                  onclick = paste0("Shiny.setInputValue('", ns(paste0("select_", fid)), "', Math.random())"),
                  # Keyboard support: Enter or Space to select
                  onkeydown = paste0("if(event.key==='Enter'||event.key===' '){event.preventDefault();Shiny.setInputValue('", ns(paste0("select_", fid)), "', Math.random())}"),

                  # File icon
                  div(class = "file-card-icon", icon("file-alt")),

                  # File info
                  div(
                    class = "file-card-info",
                    div(class = "file-card-name", f$name),
                    div(
                      class = "file-card-meta",
                      span(icon("user", class = "fa-sm"), " ", f$subject_info$id),
                      span(icon("clock", class = "fa-sm"), " ", round(f$duration_hrs, 1), "h"),
                      span(icon("layer-group", class = "fa-sm"), " ", f$epoch_length, "s epochs")
                    )
                  ),

                  # Quick actions
                  div(
                    class = "file-card-actions",
                    tags$button(
                      class = "btn btn-outline-danger btn-xs",
                      `aria-label` = paste("Remove file", f$name),
                      title = "Remove file",
                      onclick = paste0("event.stopPropagation(); Shiny.setInputValue('", ns(paste0("remove_", fid)), "', Math.random())"),
                      icon("times"),
                      span(class = "sr-only", "Remove")
                    )
                  )
                )
              })
            )
          )
        ),

        # Right column: Details Panel
        column(
          width = 7,
          uiOutput(ns("details_panel"))
        )
      )
    })

    # RENDER: DETAILS PANEL
    # Design Decision: Tabbed interface for Device/Subject/Preview info.
    # Only shown when a file is selected.
    output$details_panel <- renderUI({
      if (is.null(shared$selected_file) || is.null(shared$files[[shared$selected_file]])) {
        return(
          div(
            class = "details-panel",
            empty_state(
              title = NULL,
              message = "Select a file to view device and subject information",
              show_icon = FALSE
            )
          )
        )
      }

      f <- shared$files[[shared$selected_file]]

      div(
        class = "details-panel",

        # Header
        div(
          class = "details-header",
          h4(class = "details-title", f$name)
        ),

        # Tabs
        div(
          class = "details-tabs",
          tags$button(
            class = paste("details-tab", if (local$active_tab == "device") "active" else ""),
            onclick = paste0("Shiny.setInputValue('", ns("tab_device"), "', Math.random())"),
            icon("microchip"), " Device"
          ),
          tags$button(
            class = paste("details-tab", if (local$active_tab == "subject") "active" else ""),
            onclick = paste0("Shiny.setInputValue('", ns("tab_subject"), "', Math.random())"),
            icon("user"), " Subject"
          ),
          tags$button(
            class = paste("details-tab", if (local$active_tab == "preview") "active" else ""),
            onclick = paste0("Shiny.setInputValue('", ns("tab_preview"), "', Math.random())"),
            icon("table"), " Data Preview"
          )
        ),

        # Tab content
        div(
          class = "details-content",
          uiOutput(ns("tab_content"))
        )
      )
    })

    # RENDER: TAB CONTENT
    # Switches between Device, Subject, and Preview tabs
    output$tab_content <- renderUI({
      req(shared$selected_file, shared$files[[shared$selected_file]])

      f <- shared$files[[shared$selected_file]]

      if (local$active_tab == "device") {
        # Device Info Tab
        dev <- f$device_info

        first_epoch <- if (!is.na(dev$start_datetime)) format_agd_timestamp(dev$start_datetime) else "N/A"
        last_epoch <- if (!is.na(dev$stop_datetime)) format_agd_timestamp(dev$stop_datetime) else "N/A"

        if (first_epoch == "N/A" && "timestamp" %in% names(f$data) && nrow(f$data) > 0) {
          first_epoch <- format(min(f$data$timestamp), "%m/%d/%Y %I:%M %p")
          last_epoch <- format(max(f$data$timestamp), "%m/%d/%Y %I:%M %p")
        }

        div(
          class = "info-grid",
          div(class = "info-item",
              span(class = "info-label", "Device Type"),
              span(class = "info-value", dev$device_type %||% "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Serial Number"),
              span(class = "info-value", dev$serial_number %||% "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Epoch Length"),
              span(class = "info-value", paste0(dev$epoch_length, " sec"))),
          div(class = "info-item",
              span(class = "info-label", "Sample Rate"),
              span(class = "info-value", if (!is.na(dev$sample_rate)) paste0(dev$sample_rate, " Hz") else "N/A")),
          div(class = "info-item",
              span(class = "info-label", "First Epoch"),
              span(class = "info-value", first_epoch)),
          div(class = "info-item",
              span(class = "info-label", "Last Epoch"),
              span(class = "info-value", last_epoch)),
          div(class = "info-item",
              span(class = "info-label", "Firmware"),
              span(class = "info-value", dev$firmware %||% "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Filter"),
              span(class = "info-value", dev$filter %||% "Normal")),
          div(class = "info-item",
              span(class = "info-label", "Software"),
              span(class = "info-value", paste0(dev$software %||% "", " ", dev$software_version %||% ""))),
          div(class = "info-item",
              span(class = "info-label", "Total Epochs"),
              span(class = "info-value", format(f$n_epochs, big.mark = ",")))
        )

      } else if (local$active_tab == "subject") {
        # Subject Info Tab
        subj <- f$subject_info

        div(
          class = "info-grid",
          div(class = "info-item",
              span(class = "info-label", "Subject ID"),
              span(class = "info-value", subj$id %||% "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Gender"),
              span(class = "info-value", format_sex(subj$sex))),
          div(class = "info-item",
              span(class = "info-label", "Age"),
              span(class = "info-value", if (!is.na(subj$age) && subj$age != "0") subj$age else "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Date of Birth"),
              span(class = "info-value", if (!is.na(subj$date_of_birth)) format_agd_timestamp(subj$date_of_birth) else "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Height"),
              span(class = "info-value", format_height(subj$height))),
          div(class = "info-item",
              span(class = "info-label", "Weight"),
              span(class = "info-value", format_weight(subj$mass))),
          div(class = "info-item",
              span(class = "info-label", "Limb"),
              span(class = "info-value", if (!is.na(subj$limb) && subj$limb != "") subj$limb else "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Side"),
              span(class = "info-value", if (!is.na(subj$side) && subj$side != "") subj$side else "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Dominance"),
              span(class = "info-value", if (!is.na(subj$dominance) && subj$dominance != "") subj$dominance else "N/A")),
          div(class = "info-item",
              span(class = "info-label", "Race"),
              span(class = "info-value", if (!is.na(subj$race) && subj$race != "") subj$race else "N/A"))
        )

      } else {
        # Data Preview Tab
        DT::dataTableOutput(ns("preview_table"))
      }
    })

    # RENDER: PREVIEW TABLE
    output$preview_table <- DT::renderDataTable({
      req(shared$selected_file, shared$files[[shared$selected_file]])

      data <- shared$files[[shared$selected_file]]$data

      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No data available for preview"),
          rownames = FALSE
        ))
      }

      display <- head(data, 100)

      if ("timestamp" %in% names(display)) {
        display$timestamp <- format(display$timestamp, "%Y-%m-%d %H:%M:%S")
      }

      DT::datatable(
        display,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip',
          language = list(
            info = "Showing _START_ to _END_ of _TOTAL_ epochs (first 100 shown)"
          )
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
  })
}
