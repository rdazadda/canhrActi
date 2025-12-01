#' Batch Sleep Analysis for ActiGraph Data
#'
#' Performs sleep analysis on ActiGraph files with parallel processing support.
#' Includes memory optimization and detailed progress tracking.
#'
#' @param agd_file_path Single file, vector of files, or folder path
#' @param sleep_algorithm "cole.kripke" (adults) or "sadeh" (children/adolescents)
#' @param apply_rescoring Apply Webster's rescoring rules? (default: TRUE)
#' @param detect_sleep_period Detect sleep periods? (default: TRUE)
#' @param bedtime_start Consecutive sleep epochs for bedtime (default: 5)
#' @param wake_time_end Consecutive wake epochs for wake time (default: 10)
#' @param min_sleep_period Minimum sleep period in minutes (default: 160)
#' @param max_sleep_period Maximum sleep period in minutes (default: 1440)
#' @param min_nonzero_epochs Minimum non-zero epochs (default: 15)
#' @param export Export results to CSV? (default: TRUE)
#' @param output_dir Output directory (default: "sleep_analysis_output")
#' @param parallel Use parallel processing? (default: TRUE for >4 files)
#' @param n_cores Number of CPU cores (default: auto-detect, max 8)
#' @param verbose Show detailed progress? (default: TRUE)
#' @param memory_efficient Optimize memory for large files? (default: TRUE)
#'
#' @return List with results, summary, and parameters
#'
#' @examples
#' \dontrun{
#' # Single file
#' results <- canhrActi.sleep("participant.agd")
#'
#' # Parallel batch processing
#' results <- canhrActi.sleep("C:/Sleep Data", parallel = TRUE, n_cores = 4)
#'
#' # View summary
#' print(results$summary)
#' }
#'
#' @export
canhrActi.sleep <- function(agd_file_path,
                            sleep_algorithm = c("cole.kripke", "sadeh"),
                            apply_rescoring = TRUE,
                            detect_sleep_period = TRUE,
                            bedtime_start = 5,
                            wake_time_end = 10,
                            min_sleep_period = 160,
                            max_sleep_period = 1440,
                            min_nonzero_epochs = 15,
                            export = TRUE,
                            output_dir = "sleep_analysis_output",
                            parallel = NULL,
                            n_cores = NULL,
                            verbose = TRUE,
                            memory_efficient = TRUE) {

  sleep_algorithm <- match.arg(sleep_algorithm)

  # Handle folder or multiple files
  if (length(agd_file_path) == 1 && dir.exists(agd_file_path)) {
    agd_files <- list.files(agd_file_path, pattern = "\\.agd$", full.names = TRUE, ignore.case = TRUE)
    gt3x_files <- list.files(agd_file_path, pattern = "\\.gt3x$", full.names = TRUE, ignore.case = TRUE)
    files <- c(agd_files, gt3x_files)

    if (length(files) == 0) {
      stop("No .agd or .gt3x files found in: ", agd_file_path)
    }

    if (verbose) {
      cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
      cat("canhrActi Sleep Batch Processing\n")
      cat(paste(rep("=", 60), collapse = ""), "\n\n", sep = "")
      cat("Found files:\n")
      if (length(agd_files) > 0) cat("  ", length(agd_files), " .agd files\n", sep = "")
      if (length(gt3x_files) > 0) cat("  ", length(gt3x_files), " .gt3x files\n", sep = "")
    }
  } else if (length(agd_file_path) > 1) {
    files <- agd_file_path
  } else {
    files <- agd_file_path
  }

  n_files <- length(files)

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
    cat("  Algorithm: ", sleep_algorithm, "\n", sep = "")
    cat("  Rescoring: ", if (apply_rescoring) "Yes" else "No", "\n", sep = "")
    cat("\n")
  }

  start_time <- Sys.time()

  # Process function for single file
  process_single_sleep <- function(file_path, file_index) {
    result <- list(
      success = FALSE,
      file = basename(file_path),
      analysis = NULL,
      summary_row = NULL,
      error = NULL
    )

    tryCatch({
      analysis <- .canhrActi.sleep.single(
        agd_file_path = file_path,
        sleep_algorithm = sleep_algorithm,
        apply_rescoring = apply_rescoring,
        detect_sleep_period = detect_sleep_period,
        bedtime_start = bedtime_start,
        wake_time_end = wake_time_end,
        min_sleep_period = min_sleep_period,
        max_sleep_period = max_sleep_period,
        min_nonzero_epochs = min_nonzero_epochs
      )

      # Build summary row
      summary_row <- data.frame(
        file_name = basename(file_path),
        algorithm = sleep_algorithm,
        total_epochs = nrow(analysis$epoch_data),
        sleep_epochs = sum(analysis$epoch_data$sleep_wake == "S"),
        wake_epochs = sum(analysis$epoch_data$sleep_wake == "W"),
        sleep_periods_detected = nrow(analysis$sleep_periods),
        total_sleep_time_min = if (nrow(analysis$sleep_periods) > 0) sum(analysis$sleep_periods$sleep_time) else 0,
        avg_sleep_efficiency = if (nrow(analysis$sleep_periods) > 0) round(mean(analysis$sleep_periods$sleep_efficiency), 2) else 0,
        avg_awakenings = if (nrow(analysis$sleep_periods) > 0) round(mean(analysis$sleep_periods$number_of_awakenings), 2) else 0,
        stringsAsFactors = FALSE
      )

      if (memory_efficient) {
        # Keep only sleep periods, discard epoch data
        analysis$epoch_data <- NULL
      }

      result$analysis <- analysis
      result$summary_row <- summary_row
      result$success <- TRUE

    }, error = function(e) {
      result$error <- conditionMessage(e)
      result$summary_row <- data.frame(
        file_name = basename(file_path),
        algorithm = sleep_algorithm,
        total_epochs = NA, sleep_epochs = NA, wake_epochs = NA,
        sleep_periods_detected = 0, total_sleep_time_min = 0,
        avg_sleep_efficiency = 0, avg_awakenings = 0,
        stringsAsFactors = FALSE
      )
    })

    return(result)
  }

  # Process files
  if (parallel && n_files > 1 && n_cores > 1) {
    if (verbose) cat("Processing files in parallel...\n\n")

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      library(canhrActi)
      library(DBI)
      library(RSQLite)
    })

    parallel::clusterExport(cl, c(
      "sleep_algorithm", "apply_rescoring", "detect_sleep_period",
      "bedtime_start", "wake_time_end", "min_sleep_period",
      "max_sleep_period", "min_nonzero_epochs", "memory_efficient"
    ), envir = environment())

    results_list <- parallel::parLapply(cl, seq_along(files), function(i) {
      process_single_sleep(files[i], i)
    })

    if (verbose) cat("Parallel processing complete.\n\n")

  } else {
    # Sequential with progress
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

      results_list[[i]] <- process_single_sleep(files[i], i)
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
      all_results[[res$file]] <- res$analysis
      success_count <- success_count + 1
    } else {
      failed_files <- c(failed_files, res$file)
    }
    summary_rows[[length(summary_rows) + 1]] <- res$summary_row
  }

  summary_df <- do.call(rbind, summary_rows)
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (verbose) {
    cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
    cat("Processing Complete\n")
    cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
    cat("  Successful: ", success_count, "/", n_files, " files\n", sep = "")
    cat("  Failed: ", length(failed_files), " files\n", sep = "")
    cat("  Total time: ", .format_time(total_time), "\n", sep = "")
    cat("  Avg per file: ", round(total_time / n_files, 1), " seconds\n", sep = "")

    if (nrow(summary_df) > 0) {
      valid_rows <- summary_df[!is.na(summary_df$total_sleep_time_min), ]
      if (nrow(valid_rows) > 0) {
        cat("\nGroup Statistics:\n")
        cat("  Mean sleep periods: ", round(mean(valid_rows$sleep_periods_detected, na.rm = TRUE), 1), "\n", sep = "")
        cat("  Mean total sleep: ", round(mean(valid_rows$total_sleep_time_min, na.rm = TRUE), 1), " min\n", sep = "")
        cat("  Mean efficiency: ", round(mean(valid_rows$avg_sleep_efficiency, na.rm = TRUE), 1), "%\n", sep = "")
      }
    }
    cat("\n")
  }

  # Export
  if (export && length(all_results) > 0) {
    if (verbose) cat("Exporting results to: ", output_dir, "\n", sep = "")
    .export.actilife.sleep(all_results, output_dir)
  }

  # Build result
  batch_results <- list(
    results = all_results,
    summary = summary_df,
    n_files = n_files,
    n_success = success_count,
    n_failed = length(failed_files),
    failed_files = failed_files,
    processing_time = total_time,
    parameters = list(
      sleep_algorithm = sleep_algorithm,
      apply_rescoring = apply_rescoring,
      detect_sleep_period = detect_sleep_period,
      bedtime_start = bedtime_start,
      wake_time_end = wake_time_end,
      min_sleep_period = min_sleep_period,
      max_sleep_period = max_sleep_period,
      min_nonzero_epochs = min_nonzero_epochs,
      parallel = parallel,
      n_cores = n_cores
    )
  )

  class(batch_results) <- c("canhrActi_sleep_batch", "list")

  if (verbose) cat("Batch sleep analysis complete!\n\n")

  return(batch_results)
}


#' Single File Sleep Analysis (Internal)
#' @keywords internal
.canhrActi.sleep.single <- function(agd_file_path,
                                    sleep_algorithm,
                                    apply_rescoring,
                                    detect_sleep_period,
                                    bedtime_start,
                                    wake_time_end,
                                    min_sleep_period,
                                    max_sleep_period,
                                    min_nonzero_epochs) {

  is_gt3x <- grepl("\\.gt3x$", agd_file_path, ignore.case = TRUE)

  if (is_gt3x) {
    agd_data <- read.gt3x.file(agd_file_path, epoch_length = 60, lfe_mode = FALSE, verbose = FALSE)
  } else {
    agd_data <- read.agd(agd_file_path)
  }

  counts_data <- agd.counts(agd_data)
  subject_info <- extract.subject.info(agd_data)

  if (sleep_algorithm == "cole.kripke") {
    sleep_wake <- sleep.cole.kripke(counts_data$axis1, apply_rescoring = apply_rescoring)
  } else if (sleep_algorithm == "sadeh") {
    sleep_wake <- sleep.sadeh(counts_data$axis1)
  } else {
    stop("Unknown sleep algorithm: ", sleep_algorithm)
  }

  epoch_data <- data.frame(
    epoch = 1:nrow(counts_data),
    timestamp = counts_data$timestamp,
    date = as.Date(counts_data$timestamp),
    axis1 = counts_data$axis1,
    sleep_wake = sleep_wake,
    stringsAsFactors = FALSE
  )

  if (detect_sleep_period) {
    sleep_periods <- sleep.tudor.locke(
      sleep.state = sleep_wake,
      timestamps = counts_data$timestamp,
      counts = counts_data$axis1,
      bedtime_start = bedtime_start,
      wake_time_end = wake_time_end,
      min_sleep_period = min_sleep_period,
      max_sleep_period = max_sleep_period,
      min_nonzero_epochs = min_nonzero_epochs
    )
  } else {
    sleep_periods <- data.frame()
  }

  result <- list(
    subject_info = subject_info,
    epoch_data = epoch_data,
    sleep_periods = sleep_periods,
    parameters = list(
      file_path = agd_file_path,
      sleep_algorithm = sleep_algorithm,
      apply_rescoring = apply_rescoring,
      detect_sleep_period = detect_sleep_period
    )
  )

  class(result) <- c("canhrActi_sleep", "list")
  return(result)
}


#' Print Method for Sleep Batch Results
#' @param x An object of class canhrActi_sleep_batch
#' @param ... Additional arguments
#' @export
print.canhrActi_sleep_batch <- function(x, ...) {
  cat("\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("canhrActi Sleep Batch Analysis Results\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Files processed: ", x$n_success, "/", x$n_files, "\n", sep = "")
  cat("Processing time: ", .format_time(x$processing_time), "\n", sep = "")
  cat("Algorithm: ", x$parameters$sleep_algorithm, "\n\n", sep = "")

  if (nrow(x$summary) > 0) {
    valid <- x$summary[!is.na(x$summary$total_sleep_time_min), ]
    if (nrow(valid) > 0) {
      cat("Group Statistics:\n")
      cat("  Mean sleep periods: ", round(mean(valid$sleep_periods_detected, na.rm = TRUE), 1), "\n", sep = "")
      cat("  Mean total sleep: ", round(mean(valid$total_sleep_time_min, na.rm = TRUE), 1), " min\n", sep = "")
      cat("  Mean efficiency: ", round(mean(valid$avg_sleep_efficiency, na.rm = TRUE), 1), "%\n\n", sep = "")
    }
    cat("Summary (first 5 rows):\n")
    print(head(x$summary[, c("file_name", "sleep_periods_detected", "total_sleep_time_min", "avg_sleep_efficiency")], 5))
  }

  invisible(x)
}


#' Print Method for Single Sleep Analysis
#' @param x An object of class canhrActi_sleep
#' @param ... Additional arguments
#' @export
print.canhrActi_sleep <- function(x, ...) {
  cat("\ncanhrActi Sleep Analysis:", basename(x$parameters$file_path), "\n")
  cat("Algorithm:", x$parameters$sleep_algorithm, "\n")
  cat("Total epochs:", nrow(x$epoch_data), "\n")
  cat("Sleep epochs:", sum(x$epoch_data$sleep_wake == "S"),
      "(", round(100 * sum(x$epoch_data$sleep_wake == "S") / nrow(x$epoch_data), 1), "%)\n")
  cat("Sleep periods detected:", nrow(x$sleep_periods), "\n\n")
  if (nrow(x$sleep_periods) > 0) {
    cat("Sleep Period Summary:\n")
    print(x$sleep_periods)
  }
  invisible(x)
}
