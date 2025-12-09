#' Analyze Multiple AGD Files at Once (Batch Processing)
#'
#' Process multiple participants with parallel processing support.
#' Includes memory optimization for large datasets and detailed progress tracking.
#'
#' @param files Character vector of file paths OR a folder path
#' @param wear_time_algorithm Wear time detection algorithm (default: "choi")
#' @param intensity_algorithm Intensity classification algorithm (default: "freedson1998")
#' @param min_wear_hours Minimum hours for valid day (default: 10)
#' @param axis_to_analyze Which axis to analyze (default: "axis1")
#' @param calculate_mets Calculate METs and energy expenditure (default: TRUE)
#' @param mets_algorithm METs prediction algorithm (default: "freedson.vm3")
#' @param export Logical. Export results to CSV? (default: TRUE)
#' @param output_folder Where to save CSV files (default: current directory)
#' @param lfe_mode Logical. Low frequency extension mode (default: FALSE)
#' @param calculate_fragmentation Logical. Calculate sedentary fragmentation metrics? (default: TRUE)
#' @param calculate_circadian Logical. Calculate circadian rhythm metrics? (default: TRUE)
#' @param parallel Logical. Use parallel processing? (default: TRUE for >4 files)
#' @param n_cores Number of CPU cores to use (default: auto-detect, max 8)
#' @param verbose Logical. Show detailed progress? (default: TRUE)
#' @param memory_efficient Logical. Optimize memory for large files? (default: TRUE)
#' @return List with all results plus a combined summary table
#'
#' @examples
#' \dontrun{
#' # Basic usage - folder
#' results <- canhrActi.batch("C:/My Data Folder")
#'
#' # Parallel processing with 4 cores
#' results <- canhrActi.batch("C:/My Data", parallel = TRUE, n_cores = 4)
#'
#' # Memory-efficient mode for large datasets
#' results <- canhrActi.batch("C:/Large Study", memory_efficient = TRUE)
#'
#' # View combined summary
#' print(results$summary)
#' }
#'
#' @export
canhrActi.batch <- function(files,
                         wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                         intensity_algorithm = c("freedson1998", "CANHR"),
                         min_wear_hours = 10,
                         axis_to_analyze = c("axis1", "vector_magnitude"),
                         calculate_mets = TRUE,
                         mets_algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                            "hendelman.adult", "hendelman.lifestyle", "swartz",
                                            "leenders", "yngve.treadmill", "yngve.overground",
                                            "brooks.overground", "brooks.bm", "freedson.children"),
                         export = TRUE,
                         output_folder = ".",
                         lfe_mode = FALSE,
                         calculate_fragmentation = TRUE,
                         calculate_circadian = TRUE,
                         parallel = NULL,
                         n_cores = NULL,
                         verbose = TRUE,
                         memory_efficient = TRUE) {

  wear_time_algorithm <- match.arg(wear_time_algorithm)
  intensity_algorithm <- match.arg(intensity_algorithm)
  axis_to_analyze <- match.arg(axis_to_analyze)
  mets_algorithm <- match.arg(mets_algorithm)

  # Find files if folder provided
  if (length(files) == 1 && dir.exists(files)) {
    folder <- files
    agd_files <- list.files(folder, pattern = "\\.agd$", full.names = TRUE, ignore.case = TRUE)
    gt3x_files <- list.files(folder, pattern = "\\.gt3x$", full.names = TRUE, ignore.case = TRUE)
    bin_files <- list.files(folder, pattern = "\\.bin$", full.names = TRUE, ignore.case = TRUE)
    cwa_files <- list.files(folder, pattern = "\\.cwa$", full.names = TRUE, ignore.case = TRUE)
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

    files <- c(agd_files, gt3x_files, bin_files, cwa_files, csv_files)

    if (length(files) == 0) {
      stop("No supported accelerometer files found in: ", folder, "\n",
           "Supported formats: .agd, .gt3x, .bin, .cwa, .csv")
    }

    if (verbose) {
      cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
      cat("canhrActi Batch Processing\n")
      cat(paste(rep("=", 60), collapse = ""), "\n\n", sep = "")
      cat("Found accelerometer files:\n")
      if (length(agd_files) > 0) cat("  ", length(agd_files), " ActiGraph .agd files\n", sep = "")
      if (length(gt3x_files) > 0) cat("  ", length(gt3x_files), " ActiGraph .gt3x files\n", sep = "")
      if (length(bin_files) > 0) cat("  ", length(bin_files), " GENEActiv .bin files\n", sep = "")
      if (length(cwa_files) > 0) cat("  ", length(cwa_files), " Axivity .cwa files\n", sep = "")
      if (length(csv_files) > 0) cat("  ", length(csv_files), " CSV files\n", sep = "")
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
    cat("  Parallel: ", if (parallel && n_files > 1) paste0("Yes (", n_cores, " cores)") else "No", "\n", sep = "")
    cat("  Memory efficient: ", if (memory_efficient) "Yes" else "No", "\n", sep = "")
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
        output_summary = FALSE, lfe_mode = lfe_mode,
        calculate_mets = calculate_mets, mets_algorithm = mets_algorithm,
        calculate_fragmentation = calculate_fragmentation,
        calculate_circadian = calculate_circadian
      )

      # Build summary row (memory efficient - only keep what we need)
      summary_row <- .build_summary_row(file_path, subject_id, analysis,
                                        intensity_algorithm, axis_to_analyze)

      if (memory_efficient) {
        # Only keep essential data, discard raw data
        analysis$raw_data <- NULL
        analysis$epoch_data <- NULL
      }

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

    # Export parameters
    parallel::clusterExport(cl, c(
      "wear_time_algorithm", "intensity_algorithm", "min_wear_hours",
      "axis_to_analyze", "calculate_mets", "mets_algorithm", "lfe_mode",
      "calculate_circadian", "memory_efficient"
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
    ext <- tolower(tools::file_ext(file_path))

    # Default values
    age_val <- 0
    gender_val <- ""
    weight_lbs <- 0
    epoch_len <- 60

    # Try to get metadata
    if (ext == "agd") {
      con <- DBI::dbConnect(RSQLite::SQLite(), file_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      tables <- DBI::dbListTables(con)
      raw_data <- DBI::dbReadTable(con, "data")
      settings <- DBI::dbReadTable(con, "settings")

      # Apply filters if present
      if ("filters" %in% tables) {
        filters <- DBI::dbReadTable(con, "filters")
        if (nrow(filters) > 0) {
          filter_cond <- rep(FALSE, nrow(raw_data))
          for (f in 1:nrow(filters)) {
            filter_cond <- filter_cond |
              (raw_data$dataTimestamp >= filters$filterStartTimestamp[f] &
               raw_data$dataTimestamp <= filters$filterStopTimestamp[f])
          }
          raw_data <- raw_data[filter_cond, ]
        }
      }

      # Get settings
      age <- settings$settingValue[settings$settingName == "age"]
      sex <- settings$settingValue[settings$settingName == "sex"]
      mass_kg <- settings$settingValue[settings$settingName == "mass"]
      epoch_len <- as.numeric(settings$settingValue[settings$settingName == "epochlength"])

      age_val <- if (length(age) > 0 && !is.na(age) && age != "") as.numeric(age) else 0
      gender_val <- if (length(sex) > 0 && !is.na(sex) && sex != "") {
        ifelse(substr(sex, 1, 1) == "F", "F", ifelse(substr(sex, 1, 1) == "M", "M", ""))
      } else ""
      weight_lbs <- if (length(mass_kg) > 0 && !is.na(mass_kg) && mass_kg != "") {
        round(as.numeric(mass_kg) * 2.20462)
      } else 0

    } else {
      # For other file types, use analysis data
      raw_data <- if (!is.null(analysis$data)) analysis$data else data.frame(axis1 = 0)
      epoch_len <- if (!is.null(analysis$epoch_length)) analysis$epoch_length else 60
    }

    # Calculate axis statistics
    axis1_total <- sum(raw_data$axis1, na.rm = TRUE)
    axis2_total <- if ("axis2" %in% names(raw_data)) sum(raw_data$axis2, na.rm = TRUE) else 0
    axis3_total <- if ("axis3" %in% names(raw_data)) sum(raw_data$axis3, na.rm = TRUE) else 0
    axis1_avg <- mean(raw_data$axis1, na.rm = TRUE)
    axis2_avg <- if ("axis2" %in% names(raw_data)) mean(raw_data$axis2, na.rm = TRUE) else 0
    axis3_avg <- if ("axis3" %in% names(raw_data)) mean(raw_data$axis3, na.rm = TRUE) else 0
    axis1_max <- max(raw_data$axis1, na.rm = TRUE)
    axis2_max <- if ("axis2" %in% names(raw_data)) max(raw_data$axis2, na.rm = TRUE) else 0
    axis3_max <- if ("axis3" %in% names(raw_data)) max(raw_data$axis3, na.rm = TRUE) else 0

    # Vector magnitude
    if (all(c("axis1", "axis2", "axis3") %in% names(raw_data))) {
      vm <- sqrt(raw_data$axis1^2 + raw_data$axis2^2 + raw_data$axis3^2)
    } else {
      vm <- raw_data$axis1
    }
    vm_total <- sum(vm, na.rm = TRUE)
    vm_avg <- mean(vm, na.rm = TRUE)
    vm_max <- max(vm, na.rm = TRUE)

    # Steps
    steps_total <- if ("steps" %in% names(raw_data)) sum(raw_data$steps, na.rm = TRUE) else 0
    steps_avg <- if ("steps" %in% names(raw_data)) mean(raw_data$steps, na.rm = TRUE) else 0
    steps_max <- if ("steps" %in% names(raw_data)) max(raw_data$steps, na.rm = TRUE) else 0

    # Lux
    lux_avg <- if ("lux" %in% names(raw_data)) mean(raw_data$lux, na.rm = TRUE) else NA
    lux_max <- if ("lux" %in% names(raw_data)) max(raw_data$lux, na.rm = TRUE) else NA

    # Intensity classification - convert counts to CPM first
    counts <- if (axis_to_analyze == "axis1") raw_data$axis1 else vm
    cpm <- to_cpm(counts, epoch_len)
    intensity <- if (intensity_algorithm == "freedson1998") {
      freedson(cpm)
    } else {
      CANHR.Cutpoints(cpm)
    }

    # Calculate time in each intensity
    sedentary_min <- sum(intensity == "sedentary") * (epoch_len / 60)
    light_min <- sum(intensity == "light") * (epoch_len / 60)
    moderate_min <- sum(intensity == "moderate") * (epoch_len / 60)
    vigorous_min <- sum(intensity == "vigorous") * (epoch_len / 60)
    very_vigorous_min <- sum(intensity == "very_vigorous") * (epoch_len / 60)
    mvpa_min <- moderate_min + vigorous_min + very_vigorous_min

    total_min <- nrow(raw_data) * (epoch_len / 60)
    total_hours <- total_min / 60

    # Percentages
    sed_pct <- if (total_min > 0) (sedentary_min / total_min) * 100 else 0
    light_pct <- if (total_min > 0) (light_min / total_min) * 100 else 0
    mod_pct <- if (total_min > 0) (moderate_min / total_min) * 100 else 0
    vig_pct <- if (total_min > 0) (vigorous_min / total_min) * 100 else 0
    vvig_pct <- if (total_min > 0) (very_vigorous_min / total_min) * 100 else 0
    mvpa_pct <- if (total_min > 0) (mvpa_min / total_min) * 100 else 0

    calendar_days <- if (!is.null(analysis$overall_summary$valid_days)) {
      analysis$overall_summary$valid_days
    } else 1
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
      "% in Sedentary" = paste0(round(sed_pct, 2), "%"),
      "% in Light" = paste0(round(light_pct, 2), "%"),
      "% in Moderate" = paste0(round(mod_pct, 2), "%"),
      "% in Vigorous" = paste0(round(vig_pct, 2), "%"),
      "% in Very Vigorous" = paste0(round(vvig_pct, 2), "%"),
      "Total MVPA" = round(mvpa_min),
      "% in MVPA" = paste0(round(mvpa_pct, 2), "%"),
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
      "Axis 1 CPM" = round(axis1_avg * (60 / epoch_len), 1),
      "Axis 2 CPM" = round(axis2_avg * (60 / epoch_len), 1),
      "Axis 3 CPM" = round(axis3_avg * (60 / epoch_len), 1),
      "Vector Magnitude Counts" = round(vm_total, 1),
      "Vector Magnitude Average Counts" = round(vm_avg, 1),
      "Vector Magnitude Max Counts" = round(vm_max, 1),
      "Vector Magnitude CPM" = round(vm_avg * (60 / epoch_len), 1),
      "Steps Counts" = steps_total,
      "Steps Average Counts" = round(steps_avg, 1),
      "Steps Max Counts" = steps_max,
      "Steps Per Minute" = round(steps_avg, 1),
      "Lux Average Counts" = if (is.na(lux_avg)) NA else round(lux_avg, 1),
      "Lux Max Counts" = if (is.na(lux_max)) NA else round(lux_max, 1),
      "Number of Epochs" = nrow(raw_data),
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
