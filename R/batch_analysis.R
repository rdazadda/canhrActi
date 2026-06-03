#' Batch Process Multiple AGD Files
#'
#' Process multiple participants with optional parallel processing.
#'
#' @param files Character vector of AGD file paths OR a folder path
#' @param config Optional config list from batch.config(). If NULL, uses defaults.
#' @param wear_time_algorithm Wear time algorithm: "choi", "troiano", "CANHR2025"
#' @param intensity_algorithm Intensity algorithm: "freedson1998", "CANHR"
#' @param min_wear_hours Minimum hours for valid day (default: 10)
#' @param axis_to_analyze Axis: "axis1" or "vector_magnitude"
#' @param calculate_mets Calculate METs? (default: TRUE)
#' @param mets_algorithm METs algorithm (default: "freedson.vm3")
#' @param sleep_algorithm Sleep algorithm: "cole_kripke", "sadeh", or NULL
#' @param participant_age Age for age-specific algorithms
#' @param export Export CSV? (default: TRUE)
#' @param output_folder Output folder (default: ".")
#' @param calculate_fragmentation Calculate fragmentation? (default: TRUE)
#' @param calculate_circadian Calculate circadian? (default: TRUE)
#' @param exclude_sleep Exclude sleep from activity? (default: TRUE)
#' @param parallel Use parallel processing? (default: auto)
#' @param n_cores CPU cores (default: auto-detect)
#' @param verbose Show progress? (default: TRUE)
#' @return List with results and summary table
#'
#' @examples
#' \dontrun{
#' # Simple usage
#' results <- canhrActi.batch("C:/Data")
#'
#' # With config object
#' cfg <- batch.config(wear = "CANHR2025", sleep = "cole_kripke")
#' results <- canhrActi.batch("C:/Data", config = cfg)
#' }
#'
#' @export
canhrActi.batch <- function(files,
                         config = NULL,
                         wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                         intensity_algorithm = c("freedson1998", "CANHR"),
                         min_wear_hours = 10,
                         axis_to_analyze = c("axis1", "vector_magnitude"),
                         calculate_mets = TRUE,
                         mets_algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                            "hendelman.adult", "hendelman.lifestyle", "swartz",
                                            "leenders", "yngve.treadmill", "yngve.overground",
                                            "brooks.overground", "brooks.bm", "freedson.children"),
                         sleep_algorithm = NULL,
                         participant_age = NULL,
                         export = TRUE,
                         output_folder = ".",
                         calculate_fragmentation = TRUE,
                         calculate_circadian = TRUE,
                         exclude_sleep = TRUE,
                         parallel = NULL,
                         n_cores = NULL,
                         verbose = TRUE) {

  # Apply config if provided
  if (!is.null(config)) {
    if (!is.null(config$wear)) wear_time_algorithm <- config$wear
    if (!is.null(config$intensity)) intensity_algorithm <- config$intensity
    if (!is.null(config$min_wear)) min_wear_hours <- config$min_wear
    if (!is.null(config$axis)) axis_to_analyze <- config$axis
    if (!is.null(config$mets)) calculate_mets <- config$mets
    if (!is.null(config$mets_algo)) mets_algorithm <- config$mets_algo
    if (!is.null(config$sleep)) sleep_algorithm <- config$sleep
    if (!is.null(config$age)) participant_age <- config$age
    if (!is.null(config$fragmentation)) calculate_fragmentation <- config$fragmentation
    if (!is.null(config$circadian)) calculate_circadian <- config$circadian
    if (!is.null(config$exclude_sleep)) exclude_sleep <- config$exclude_sleep
    if (!is.null(config$parallel)) parallel <- config$parallel
    if (!is.null(config$cores)) n_cores <- config$cores
  }

  wear_time_algorithm <- match.arg(wear_time_algorithm)
  intensity_algorithm <- match.arg(intensity_algorithm)
  axis_to_analyze <- match.arg(axis_to_analyze)
  mets_algorithm <- match.arg(mets_algorithm)

  # Find files if folder provided
  if (length(files) == 1 && dir.exists(files)) {
    folder <- files
    files <- list.files(folder, pattern = "\\.agd$", full.names = TRUE, ignore.case = TRUE)

    if (length(files) == 0) {
      stop("No AGD files found in: ", folder, "\n",
           "Only ActiGraph .agd files are supported.")
    }

    if (verbose) {
      cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
      cat("canhrActi Batch Processing\n")
      cat(paste(rep("=", 60), collapse = ""), "\n\n", sep = "")
      cat("Found ", length(files), " ActiGraph .agd files\n", sep = "")
    }
  }

  n_files <- length(files)
  if (n_files == 0) stop("No files provided")

  # Auto-detect parallel settings
  if (is.null(parallel)) {
    parallel <- n_files > 4
  }

  if (is.null(n_cores)) {
    available_cores <- parallel::detectCores(logical = FALSE)
    n_cores <- min(available_cores - 1, 8, n_files)
    n_cores <- max(n_cores, 1)
  }

  if (verbose) {
    cat("\nProcessing Configuration:\n")
    cat("  Total files: ", n_files, "\n", sep = "")
    cat("  Parallel: ", if (parallel && n_files > 1) paste0("Yes (", n_cores, " cores)") else "No (sequential)", "\n", sep = "")
    cat("  Wear time: ", wear_time_algorithm, "\n", sep = "")
    cat("  Intensity: ", intensity_algorithm, "\n", sep = "")
    cat("\n")
  }

  start_time <- Sys.time()

  # Process function for single file
  process_single_file <- function(file_path, file_index, total_files) {
    result <- list(
      success = FALSE,
      file = basename(file_path),
      file_path = file_path,
      subject_id = NULL,
      analysis = NULL,
      summary_row = NULL,
      error = NULL
    )

    tryCatch({
      # Extract subject ID
      subject_id <- .extract_subject_id(file_path, file_index)
      result$subject_id <- subject_id

      # Run analysis
      analysis <- .canhrActi.single.internal(
        file_path, wear_time_algorithm, intensity_algorithm,
        min_wear_hours, axis_to_analyze,
        output_summary = FALSE,
        calculate_mets = calculate_mets, mets_algorithm = mets_algorithm,
        sleep_algorithm = sleep_algorithm,
        participant_age = participant_age,
        calculate_fragmentation = calculate_fragmentation,
        calculate_circadian = calculate_circadian,
        exclude_sleep = exclude_sleep
      )

      # Build summary row
      summary_row <- .build_summary_row(file_path, subject_id, analysis,
                                        intensity_algorithm, axis_to_analyze)

      result$analysis <- analysis
      result$summary_row <- summary_row
      result$success <- TRUE

    }, error = function(e) {
      result$error <- conditionMessage(e)
    })

    return(result)
  }

  # Process files
  if (parallel && n_files > 1 && n_cores > 1) {
    # Parallel processing
    if (verbose) cat("Processing files in parallel...\n\n")

    # Create cluster
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Export required functions and packages to cluster
    parallel::clusterEvalQ(cl, {
      library(canhrActi)
      library(DBI)
      library(RSQLite)
    })

    # Export parameters and data
    #  Also export 'files', 'n_files', and 'process_single_file' to cluster
    # These are needed inside the parLapply function
    parallel::clusterExport(cl, c(
      "wear_time_algorithm", "intensity_algorithm", "min_wear_hours",
      "axis_to_analyze", "calculate_mets", "mets_algorithm",
      "calculate_fragmentation", "calculate_circadian",
      "files", "n_files", "process_single_file"
    ), envir = environment())

    # Process with progress
    results_list <- parallel::parLapply(cl, seq_along(files), function(i) {
      process_single_file(files[i], i, n_files)
    })

    if (verbose) cat("\nParallel processing complete.\n")

  } else {
    # Sequential processing with progress
    results_list <- vector("list", n_files)

    for (i in seq_along(files)) {
      if (verbose) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (i > 1) {
          avg_time <- elapsed / (i - 1)
          remaining <- avg_time * (n_files - i + 1)
          eta <- .format_time(remaining)
        } else {
          eta <- "calculating..."
        }

        pct <- round((i - 1) / n_files * 100)
        bar_width <- 30
        filled <- round(bar_width * (i - 1) / n_files)
        bar <- paste0("[", paste(rep("=", filled), collapse = ""),
                      paste(rep(" ", bar_width - filled), collapse = ""), "]")

        cat("\r", bar, " ", pct, "% | File ", i, "/", n_files,
            " | ETA: ", eta, "          ", sep = "")
        flush.console()
      }

      results_list[[i]] <- process_single_file(files[i], i, n_files)

      # Periodic garbage collection
      if (i %% 5 == 0) {
        gc(verbose = FALSE, full = TRUE)
      }
    }

    if (verbose) {
      cat("\r[", paste(rep("=", 30), collapse = ""), "] 100% | Complete!",
          paste(rep(" ", 30), collapse = ""), "\n\n", sep = "")
    }
  }

  # Compile results
  all_results <- list()
  summary_rows <- list()
  failed_files <- character(0)
  success_count <- 0

  for (res in results_list) {
    if (res$success) {
      all_results[[res$subject_id]] <- res$analysis
      summary_rows[[length(summary_rows) + 1]] <- res$summary_row
      success_count <- success_count + 1
    } else {
      failed_files <- c(failed_files, res$file)
    }
  }

  # Build summary table
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
  } else {
    summary_table <- data.frame()
  }

  # Calculate processing stats
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (verbose) {
    cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
    cat("Processing Complete\n")
    cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
    cat("  Successful: ", success_count, "/", n_files, " files\n", sep = "")
    cat("  Failed: ", length(failed_files), " files\n", sep = "")
    cat("  Total time: ", .format_time(total_time), "\n", sep = "")
    cat("  Avg per file: ", round(total_time / n_files, 1), " seconds\n", sep = "")

    if (length(failed_files) > 0) {
      cat("\nFailed files:\n")
      for (f in failed_files[1:min(5, length(failed_files))]) {
        cat("  - ", f, "\n", sep = "")
      }
      if (length(failed_files) > 5) {
        cat("  ... and ", length(failed_files) - 5, " more\n", sep = "")
      }
    }

    if (nrow(summary_table) > 0) {
      cat("\nGroup Statistics:\n")
      cat("  Mean MVPA: ", round(mean(summary_table$`Total MVPA`, na.rm = TRUE), 1), " min/day\n", sep = "")
      cat("  Mean Sedentary: ", round(mean(summary_table$Sedentary, na.rm = TRUE) / 60, 1), " hrs/day\n", sep = "")
      cat("  Mean Valid Days: ", round(mean(summary_table$`Calendar Days`, na.rm = TRUE), 1), "\n", sep = "")
    }
    cat("\n")
  }

  # Export results
  if (export && nrow(summary_table) > 0) {
    if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

    summary_file <- file.path(output_folder, paste0("canhrActi_batch_summary_",
                                                     format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    utils::write.csv(summary_table, summary_file, row.names = FALSE)

    if (verbose) cat("Exported: ", summary_file, "\n\n", sep = "")
  }

  # Build result object
  result <- list(
    summary = summary_table,
    participants = all_results,
    n_participants = length(all_results),
    n_failed = length(failed_files),
    failed_files = failed_files,
    processing_time = total_time,
    group_stats = list(
      mean_valid_days = if (nrow(summary_table) > 0) mean(summary_table$`Calendar Days`, na.rm = TRUE) else NA,
      mean_wear_hours = if (nrow(summary_table) > 0) mean(summary_table$Time, na.rm = TRUE) else NA,
      mean_mvpa_minutes = if (nrow(summary_table) > 0) mean(summary_table$`Total MVPA`, na.rm = TRUE) else NA,
      mean_sedentary_hours = if (nrow(summary_table) > 0) mean(summary_table$Sedentary, na.rm = TRUE) / 60 else NA
    ),
    settings = list(
      wear_time_algorithm = wear_time_algorithm,
      intensity_algorithm = intensity_algorithm,
      parallel = parallel,
      n_cores = n_cores
    )
  )

  class(result) <- c("canhrActi_batch", "list")
  return(result)
}


# Helper: Extract subject ID from file
.extract_subject_id <- function(file_path, fallback_index) {
  tryCatch({
    ext <- tolower(tools::file_ext(file_path))

    if (ext == "agd") {
      con <- DBI::dbConnect(RSQLite::SQLite(), file_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      tables <- DBI::dbListTables(con)
      if ("settings" %in% tables) {
        settings <- DBI::dbReadTable(con, "settings")
        subj <- settings$settingValue[settings$settingName == "subjectname"]
        if (length(subj) > 0 && !is.na(subj) && subj != "" && subj != "0") {
          return(subj)
        }
      }
    }

    # Fallback: use filename
    id <- basename(file_path)
    id <- sub("\\.[^.]*$", "", id)  # Remove extension
    id <- sub("\\s*\\(.*\\)\\s*", "", id)  # Remove dates in parentheses
    id <- trimws(id)
    if (id == "" || is.na(id)) id <- paste0("subject_", fallback_index)
    return(id)

  }, error = function(e) {
    paste0("subject_", fallback_index)
  })
}


# Helper: Build summary row for a single file
.build_summary_row <- function(file_path, subject_id, analysis, intensity_algorithm, axis_to_analyze) {
  tryCatch({
    # Use data from analysis object instead of re-reading file (performance optimization)
    # The analysis object already contains all needed data from canhrActi()

    # Get metadata from analysis object
    subj_info <- analysis$subject_info
    age_val <- if (!is.null(subj_info$age) && !is.na(subj_info$age)) subj_info$age else 0
    gender_val <- if (!is.null(subj_info$sex) && !is.na(subj_info$sex)) {
      sex <- subj_info$sex
      ifelse(substr(sex, 1, 1) == "F", "F", ifelse(substr(sex, 1, 1) == "M", "M", ""))
    } else ""
    weight_lbs <- if (!is.null(subj_info$weight_lbs) && !is.na(subj_info$weight_lbs)) {
      subj_info$weight_lbs
    } else 0
    epoch_len <- if (!is.null(analysis$parameters$epoch_length)) {
      analysis$parameters$epoch_length
    } else 60

    # Safe max function that returns 0 instead of -Inf for empty/all-NA vectors
    safe_max <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(0)
      max(x)
    }

    # Compute the canonical ActiLife Summary via the SHARED helper so this batch
    # summary row reports IDENTICAL intensity numbers to export_summary and
    # export_summary_internal. The helper:
    #   - uses the already-classified epoch_data$intensity (NOT a 3rd re-classification)
    #   - restricts to VALID-DAY & WEAR epochs (excludes non-wear / invalid days)
    #   - uses ONE percentage denominator = wear-epoch count
    s <- .build_actilife_summary(analysis)

    if (is.null(s)) {
      # No valid days/epochs: emit zeroed intensity numbers but still report file
      epoch_sec <- epoch_len
      n_wear_epochs <- 0
      wear_data <- analysis$epoch_data[0, , drop = FALSE]
      sedentary_min <- light_min <- moderate_min <- vigorous_min <- 0
      very_vigorous_min <- mvpa_min <- 0
      sed_pct_str <- light_pct_str <- mod_pct_str <- "0.00%"
      vig_pct_str <- vvig_pct_str <- mvpa_pct_str <- "0.00%"
      calendar_days <- 1
    } else {
      epoch_sec <- s$epoch_sec
      n_wear_epochs <- s$n_wear_epochs
      wear_data <- s$wear_valid_epochs
      # Epoch counts -> minutes (consistent with the helper's epoch length)
      sedentary_min     <- s$sedentary     * (epoch_sec / 60)
      light_min         <- s$light         * (epoch_sec / 60)
      moderate_min      <- s$moderate      * (epoch_sec / 60)
      vigorous_min      <- s$vigorous      * (epoch_sec / 60)
      very_vigorous_min <- s$very_vigorous * (epoch_sec / 60)
      mvpa_min          <- s$total_mvpa    * (epoch_sec / 60)
      sed_pct_str   <- s$sed_pct_str
      light_pct_str <- s$light_pct_str
      mod_pct_str   <- s$mod_pct_str
      vig_pct_str   <- s$vig_pct_str
      vvig_pct_str  <- s$vvig_pct_str
      mvpa_pct_str  <- s$mvpa_pct_str
      calendar_days <- s$n_valid_days
    }

    # Axis/VM/steps statistics computed over the SAME valid-day wear epochs the
    # ActiLife Summary export uses, so all reported columns share one epoch universe.
    if (is.null(wear_data) || nrow(wear_data) == 0) {
      wear_data <- data.frame(axis1 = numeric(0), axis2 = numeric(0),
                              axis3 = numeric(0))
    }
    has_rows <- nrow(wear_data) > 0

    axis1_total <- if (has_rows) sum(wear_data$axis1, na.rm = TRUE) else 0
    axis2_total <- if (has_rows && "axis2" %in% names(wear_data)) sum(wear_data$axis2, na.rm = TRUE) else 0
    axis3_total <- if (has_rows && "axis3" %in% names(wear_data)) sum(wear_data$axis3, na.rm = TRUE) else 0
    axis1_avg <- if (has_rows) mean(wear_data$axis1, na.rm = TRUE) else 0
    axis2_avg <- if (has_rows && "axis2" %in% names(wear_data)) mean(wear_data$axis2, na.rm = TRUE) else 0
    axis3_avg <- if (has_rows && "axis3" %in% names(wear_data)) mean(wear_data$axis3, na.rm = TRUE) else 0
    axis1_max <- if (has_rows) safe_max(wear_data$axis1) else 0
    axis2_max <- if (has_rows && "axis2" %in% names(wear_data)) safe_max(wear_data$axis2) else 0
    axis3_max <- if (has_rows && "axis3" %in% names(wear_data)) safe_max(wear_data$axis3) else 0

    # Vector magnitude
    if (has_rows && all(c("axis1", "axis2", "axis3") %in% names(wear_data))) {
      vm <- sqrt(wear_data$axis1^2 + wear_data$axis2^2 + wear_data$axis3^2)
    } else if (has_rows) {
      vm <- wear_data$axis1
    } else {
      vm <- numeric(0)
    }
    vm_total <- if (length(vm) > 0) sum(vm, na.rm = TRUE) else 0
    vm_avg <- if (length(vm) > 0) mean(vm, na.rm = TRUE) else 0
    vm_max <- safe_max(vm)

    # Steps
    steps_total <- if (has_rows && "steps" %in% names(wear_data)) sum(wear_data$steps, na.rm = TRUE) else 0
    steps_avg <- if (has_rows && "steps" %in% names(wear_data)) mean(wear_data$steps, na.rm = TRUE) else 0
    steps_max <- if (has_rows && "steps" %in% names(wear_data)) safe_max(wear_data$steps) else 0

    # Lux
    lux_avg <- if (has_rows && "lux" %in% names(wear_data)) mean(wear_data$lux, na.rm = TRUE) else NA
    lux_max <- if (has_rows && "lux" %in% names(wear_data)) safe_max(wear_data$lux) else NA

    total_hours <- (n_wear_epochs * (epoch_sec / 60)) / 60
    avg_mvpa_per_day <- mvpa_min / max(calendar_days, 1)

    # Build row
    data.frame(
      Subject = subject_id,
      Filename = basename(file_path),
      Epoch = epoch_len,
      "Weight (lbs)" = weight_lbs,
      Age = age_val,
      Gender = gender_val,
      Sedentary = round(sedentary_min),
      Light = round(light_min),
      Moderate = round(moderate_min),
      Vigorous = round(vigorous_min),
      "Very Vigorous" = round(very_vigorous_min),
      "% in Sedentary" = sed_pct_str,
      "% in Light" = light_pct_str,
      "% in Moderate" = mod_pct_str,
      "% in Vigorous" = vig_pct_str,
      "% in Very Vigorous" = vvig_pct_str,
      "Total MVPA" = round(mvpa_min),
      "% in MVPA" = mvpa_pct_str,
      "Average MVPA Per day" = round(avg_mvpa_per_day, 1),
      "Axis 1 Counts" = axis1_total,
      "Axis 2 Counts" = axis2_total,
      "Axis 3 Counts" = axis3_total,
      "Axis 1 Average Counts" = round(axis1_avg, 1),
      "Axis 2 Average Counts" = round(axis2_avg, 1),
      "Axis 3 Average Counts" = round(axis3_avg, 1),
      "Axis 1 Max Counts" = axis1_max,
      "Axis 2 Max Counts" = axis2_max,
      "Axis 3 Max Counts" = axis3_max,
      "Axis 1 CPM" = round(axis1_avg * (60 / epoch_sec), 1),
      "Axis 2 CPM" = round(axis2_avg * (60 / epoch_sec), 1),
      "Axis 3 CPM" = round(axis3_avg * (60 / epoch_sec), 1),
      "Vector Magnitude Counts" = round(vm_total, 1),
      "Vector Magnitude Average Counts" = round(vm_avg, 1),
      "Vector Magnitude Max Counts" = round(vm_max, 1),
      "Vector Magnitude CPM" = round(vm_avg * (60 / epoch_sec), 1),
      "Steps Counts" = steps_total,
      "Steps Average Counts" = round(steps_avg, 1),
      "Steps Max Counts" = steps_max,
      "Steps Per Minute" = round(steps_avg, 1),
      "Lux Average Counts" = if (is.na(lux_avg)) NA else round(lux_avg, 1),
      "Lux Max Counts" = if (is.na(lux_max)) NA else round(lux_max, 1),
      "Number of Epochs" = n_wear_epochs,
      Time = round(total_hours, 1),
      "Calendar Days" = calendar_days,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    # Return minimal row on error
    data.frame(
      Subject = subject_id,
      Filename = basename(file_path),
      Epoch = NA, "Weight (lbs)" = NA, Age = NA, Gender = NA,
      Sedentary = NA, Light = NA, Moderate = NA, Vigorous = NA, "Very Vigorous" = NA,
      "% in Sedentary" = NA, "% in Light" = NA, "% in Moderate" = NA,
      "% in Vigorous" = NA, "% in Very Vigorous" = NA, "Total MVPA" = NA,
      "% in MVPA" = NA, "Average MVPA Per day" = NA,
      "Axis 1 Counts" = NA, "Axis 2 Counts" = NA, "Axis 3 Counts" = NA,
      "Axis 1 Average Counts" = NA, "Axis 2 Average Counts" = NA, "Axis 3 Average Counts" = NA,
      "Axis 1 Max Counts" = NA, "Axis 2 Max Counts" = NA, "Axis 3 Max Counts" = NA,
      "Axis 1 CPM" = NA, "Axis 2 CPM" = NA, "Axis 3 CPM" = NA,
      "Vector Magnitude Counts" = NA, "Vector Magnitude Average Counts" = NA,
      "Vector Magnitude Max Counts" = NA, "Vector Magnitude CPM" = NA,
      "Steps Counts" = NA, "Steps Average Counts" = NA, "Steps Max Counts" = NA,
      "Steps Per Minute" = NA, "Lux Average Counts" = NA, "Lux Max Counts" = NA,
      "Number of Epochs" = NA, Time = NA, "Calendar Days" = NA,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
}


# Helper: Format time duration
.format_time <- function(seconds) {
  if (is.na(seconds) || seconds < 0) return("--")
  if (seconds < 60) return(paste0(round(seconds), "s"))
  if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
  return(paste0(round(seconds / 3600, 1), "h"))
}


#' Print Method for Batch Results
#' @param x An object of class canhrActi_batch
#' @param ... Additional arguments
#' @export
print.canhrActi_batch <- function(x, ...) {
  cat("\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("canhrActi Batch Analysis Results\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Participants: ", x$n_participants, "\n", sep = "")
  cat("Failed: ", x$n_failed, "\n", sep = "")
  cat("Processing time: ", .format_time(x$processing_time), "\n\n", sep = "")

  cat("Group Statistics:\n")
  cat("  Mean Valid Days: ", round(x$group_stats$mean_valid_days, 1), "\n", sep = "")
  cat("  Mean Wear Time: ", round(x$group_stats$mean_wear_hours, 1), " hours\n", sep = "")
  cat("  Mean MVPA: ", round(x$group_stats$mean_mvpa_minutes, 1), " min/day\n", sep = "")
  cat("  Mean Sedentary: ", round(x$group_stats$mean_sedentary_hours, 1), " hours/day\n\n", sep = "")

  cat("Settings:\n")
  cat("  Wear time algorithm: ", x$settings$wear_time_algorithm, "\n", sep = "")
  cat("  Intensity algorithm: ", x$settings$intensity_algorithm, "\n", sep = "")
  cat("  Parallel processing: ", if (x$settings$parallel) paste0("Yes (", x$settings$n_cores, " cores)") else "No", "\n", sep = "")

  cat("\n")
  if (nrow(x$summary) > 0) {
    cat("Summary table preview (first 5 rows):\n")
    print(head(x$summary[, c("Subject", "Calendar Days", "Total MVPA", "Sedentary")], 5))
  }

  invisible(x)
}


#' Create Batch Processing Configuration
#'
#' Helper to create a config object for canhrActi.batch().
#' Only specify parameters you want to change from defaults.
#'
#' @param wear Wear time algorithm: "choi", "troiano", "CANHR2025"
#' @param intensity Intensity algorithm: "freedson1998", "CANHR"
#' @param min_wear Minimum wear hours (default: 10)
#' @param axis Axis: "axis1", "vector_magnitude"
#' @param mets Calculate METs? (default: TRUE)
#' @param mets_algo METs algorithm
#' @param sleep Sleep algorithm: "cole_kripke", "sadeh", NULL
#' @param age Participant age
#' @param fragmentation Calculate fragmentation? (default: TRUE)
#' @param circadian Calculate circadian? (default: TRUE)
#' @param exclude_sleep Exclude sleep from activity? (default: TRUE)
#' @param parallel Use parallel? (default: auto)
#' @param cores Number of CPU cores
#'
#' @return Config list for canhrActi.batch()
#'
#' @examples
#' \dontrun{
#' # Create config for CANHR analysis
#' cfg <- batch.config(wear = "CANHR2025", intensity = "CANHR")
#' results <- canhrActi.batch("C:/Data", config = cfg)
#'
#' # Config for sleep analysis
#' cfg <- batch.config(sleep = "cole_kripke", circadian = TRUE)
#' }
#'
#' @export
batch.config <- function(wear = NULL,
                         intensity = NULL,
                         min_wear = NULL,
                         axis = NULL,
                         mets = NULL,
                         mets_algo = NULL,
                         sleep = NULL,
                         age = NULL,
                         fragmentation = NULL,
                         circadian = NULL,
                         exclude_sleep = NULL,
                         parallel = NULL,
                         cores = NULL) {

  config <- list()
  if (!is.null(wear)) config$wear <- wear
  if (!is.null(intensity)) config$intensity <- intensity
  if (!is.null(min_wear)) config$min_wear <- min_wear
  if (!is.null(axis)) config$axis <- axis
  if (!is.null(mets)) config$mets <- mets
  if (!is.null(mets_algo)) config$mets_algo <- mets_algo
  if (!is.null(sleep)) config$sleep <- sleep
  if (!is.null(age)) config$age <- age
  if (!is.null(fragmentation)) config$fragmentation <- fragmentation
  if (!is.null(circadian)) config$circadian <- circadian
  if (!is.null(exclude_sleep)) config$exclude_sleep <- exclude_sleep
  if (!is.null(parallel)) config$parallel <- parallel
  if (!is.null(cores)) config$cores <- cores

  class(config) <- c("canhrActi_batch_config", "list")
  config
}
