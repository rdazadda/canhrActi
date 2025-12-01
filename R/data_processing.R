#' Process Multi-axis Accelerometer Data
#' @param accel_data Data frame with columns: timestamp, x, y, z
#' @param sampling_freq Numeric. Sampling frequency in Hz
#' @param epoch_length Numeric. Epoch length in seconds (default: 60)
#' @param wear_algorithm Character. "troiano", "choi", or "CANHR2025" (default: "troiano")
#' @param calculate_steps Logical. Whether to calculate step counts (default: TRUE)
#' @param step_method Character. "peak" or "autocorr" (default: "peak")
#' @param cutpoint_algorithm Function to apply cut points (default: freedson)
#' @param lfe_mode Logical. Low Frequency Extension mode for counts (default: FALSE)
#' @return List with epoch_data, intensity_summary, totals, MVPA, steps, params
#' @export
process <- function(accel_data,
                                       sampling_freq,
                                       epoch_length = 60,
                                       wear_algorithm = "troiano",
                                       calculate_steps = TRUE,
                                       step_method = "peak",
                                       cutpoint_algorithm = freedson,
                                       lfe_mode = FALSE) {

  required.cols <- c("timestamp", "x", "y", "z")
  missing.cols <- setdiff(required.cols, names(accel_data))
  if (length(missing.cols) > 0) {
    stop("Missing required columns: ", paste(missing.cols, collapse = ", "))
  }

  n.samples <- nrow(accel_data)
  duration.hours <- n.samples / sampling_freq / 3600

  counts.x <- counts(accel_data$x, sampling_freq, epoch_length, lfe_mode)
  counts.y <- counts(accel_data$y, sampling_freq, epoch_length, lfe_mode)
  counts.z <- counts(accel_data$z, sampling_freq, epoch_length, lfe_mode)

  vm.counts <- vm(counts.x, counts.y, counts.z)

  if (wear_algorithm == "troiano") {
    wear.time <- wear.troiano(counts.y)
  } else if (wear_algorithm == "choi") {
    wear.time <- wear.choi(counts.y)
  } else if (wear_algorithm == "CANHR2025") {
    wear.time <- wear.CANHR2025(counts.y)
  } else {
    stop("wear_algorithm must be 'troiano', 'choi', or 'CANHR2025'")
  }

  wear.minutes <- sum(wear.time)
  wear.hours <- wear.minutes / 60
  wear.percent <- 100 * wear.minutes / length(counts.y)

  intensity <- cutpoint_algorithm(counts.y)
  intensity.summary <- intensity(intensity, wear.time)

  steps <- NULL
  if (calculate_steps) {
    if (step_method == "autocorr") {
      steps <- steps.auto(accel_data$y, sampling_freq)
    } else {
      steps <- steps(accel_data$y, sampling_freq)
    }
  }

  mvpa.minutes <- mvpa(intensity, wear.time)

  epoch.data <- data.frame(
    epoch = 1:length(counts.y),
    counts_x = counts.x,
    counts_y = counts.y,
    counts_z = counts.z,
    counts_vm = vm.counts,
    wear_time = wear.time,
    intensity = intensity,
    stringsAsFactors = FALSE
  )

  results <- list(
    epoch_data = epoch.data,
    intensity_summary = intensity.summary,
    total_counts_y = sum(counts.y[wear.time]),
    total_counts_vm = sum(vm.counts[wear.time]),
    total_wear_minutes = wear.minutes,
    mvpa_minutes = mvpa.minutes,
    total_steps = steps,
    processing_params = list(
      sampling_freq = sampling_freq,
      epoch_length = epoch_length,
      wear_algorithm = wear_algorithm,
      step_method = if (is.null(steps)) NA else step_method,
      cutpoint_function = deparse(substitute(cutpoint_algorithm)),
      lfe_mode = lfe_mode
    )
  )
  class(results) <- c("canhrActi_results", "list")
  return(results)
}

#' Print Method for canhrActi Results
#'
#' @param x Object of class 'canhrActi_results'
#' @param ... Additional arguments (unused)
#'
#' @export
print.canhrActi_results <- function(x, ...) {
  cat("\ncanhrActi Processing Results\n")
  cat("Wear time:", x$total_wear_minutes, "min\n")
  cat("MVPA:", x$mvpa_minutes, "min\n")
  if (!is.null(x$total_steps)) cat("Steps:", x$total_steps, "\n")
  cat("\n")
  print(x$intensity_summary)
  invisible(x)
}
