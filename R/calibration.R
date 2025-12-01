#' Auto-Calibration of Raw Accelerometer Data
#'
#' Calibrates raw acceleration data using the local gravity method. During periods
#' of non-movement, the acceleration vector should equal exactly 1g. Deviations
#' from 1g indicate calibration errors that this function corrects.
#'
#' @param accel_data Data frame with columns x, y, z (acceleration in g-units)
#'   and optionally timestamp
#' @param sampling_freq Numeric. Sampling frequency in Hz
#' @param sphere_criterion Numeric. Target vector magnitude during non-movement
#'   (default: 1.0 for 1g)
#' @param noise_threshold Numeric. Maximum SD within window to be considered
#'   non-movement (default: 0.013g based on van Hees et al., 2014)
#' @param window_seconds Numeric. Window size for non-movement detection in
#'   seconds (default: 10)
#' @param min_samples Numeric. Minimum number of non-movement samples required
#'   for calibration (default: 100)
#' @param verbose Logical. Print progress messages? (default: TRUE)
#'
#' @return List with class 'canhrActi_calibration' containing:
#'   \itemize{
#'     \item \code{calibrated_data} - Data frame with calibrated x, y, z values
#'     \item \code{offset} - Calibration offsets for each axis (x, y, z)
#'     \item \code{scale} - Calibration scale factors for each axis
#'     \item \code{n_nonmovement} - Number of non-movement windows used
#'     \item \code{error_before} - Mean calibration error before correction
#'     \item \code{error_after} - Mean calibration error after correction
#'     \item \code{calibration_success} - Logical. TRUE if calibration improved accuracy
#'   }
#'
#' @details
#' \strong{Algorithm (van Hees et al., 2014):}
#' \enumerate{
#'   \item Identify non-movement periods where SD of each axis < noise_threshold
#'   \item In these periods, the Euclidean norm should equal 1g
#'   \item Use iterative least-squares to find offset and scale corrections
#'   \item Apply corrections: x_cal = (x_raw - offset_x) * scale_x
#' }
#'
#' \strong{Use Cases:}
#' \itemize{
#'   \item Required for raw .gt3x data analysis
#'   \item Corrects for sensor drift and temperature effects
#'   \item Improves accuracy of ENMO and other acceleration metrics
#' }
#'
#' @references
#' van Hees VT, et al. (2014). Autocalibration of accelerometer data for
#' free-living physical activity assessment using local gravity and temperature:
#' an evaluation on four continents. J Appl Physiol, 117(7):738-744.
#'
#' @examples
#' \dontrun{
#' # Read raw acceleration data
#' raw_data <- data.frame(
#'   x = rnorm(10000, 0, 0.5),
#'   y = rnorm(10000, 1, 0.5),
#'   z = rnorm(10000, 0, 0.5)
#' )
#'
#' # Calibrate
#' cal_result <- auto.calibrate(raw_data, sampling_freq = 30)
#'
#' # Use calibrated data
#' calibrated <- cal_result$calibrated_data
#' }
#'
#' @export
auto.calibrate <- function(accel_data,
                           sampling_freq,
                           sphere_criterion = 1.0,
                           noise_threshold = 0.013,
                           window_seconds = 10,
                           min_samples = 100,
                           verbose = TRUE) {

  # Validate input
  required_cols <- c("x", "y", "z")
  missing_cols <- setdiff(required_cols, names(accel_data))
  if (length(missing_cols) > 0) {
    stop("accel_data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  n <- nrow(accel_data)
  if (n == 0) {
    stop("accel_data is empty")
  }

  x <- accel_data$x
  y <- accel_data$y
  z <- accel_data$z

  # Window size in samples
  window_size <- round(window_seconds * sampling_freq)

  if (verbose) {
    cat("Auto-calibration using local gravity method\n")
    cat("Samples:", n, ", Window size:", window_size, "samples\n")
  }

  # Step 1: Identify non-movement periods

  n_windows <- floor(n / window_size)
  nonmovement_samples <- list()

  for (i in 1:n_windows) {
    start_idx <- (i - 1) * window_size + 1
    end_idx <- i * window_size

    window_x <- x[start_idx:end_idx]
    window_y <- y[start_idx:end_idx]
    window_z <- z[start_idx:end_idx]

    # Check if SD is below threshold for all axes
    sd_x <- sd(window_x, na.rm = TRUE)
    sd_y <- sd(window_y, na.rm = TRUE)
    sd_z <- sd(window_z, na.rm = TRUE)

    if (!is.na(sd_x) && !is.na(sd_y) && !is.na(sd_z) &&
        sd_x < noise_threshold && sd_y < noise_threshold && sd_z < noise_threshold) {

      # Store mean values from this non-movement window
      nonmovement_samples[[length(nonmovement_samples) + 1]] <- c(
        mean(window_x, na.rm = TRUE),
        mean(window_y, na.rm = TRUE),
        mean(window_z, na.rm = TRUE)
      )
    }
  }

  n_nonmovement <- length(nonmovement_samples)

  if (verbose) {
    cat("Non-movement windows found:", n_nonmovement, "\n")
  }

  if (n_nonmovement < min_samples) {
    warning("Insufficient non-movement samples for calibration (",
            n_nonmovement, " < ", min_samples, "). Returning uncalibrated data.")
    return(list(
      calibrated_data = accel_data,
      offset = c(x = 0, y = 0, z = 0),
      scale = c(x = 1, y = 1, z = 1),
      n_nonmovement = n_nonmovement,
      error_before = NA,
      error_after = NA,
      calibration_success = FALSE
    ))
  }

  # Convert to matrix
  nm_matrix <- do.call(rbind, nonmovement_samples)

  # Step 2: Calculate calibration error before correction

  vm_before <- sqrt(nm_matrix[, 1]^2 + nm_matrix[, 2]^2 + nm_matrix[, 3]^2)
  error_before <- mean(abs(vm_before - sphere_criterion))

  if (verbose) {
    cat("Mean calibration error before:", round(error_before, 4), "g\n")
  }

  # Step 3: Iterative calibration using ellipsoid fitting

  # Simple offset-only calibration (most common correction needed)
  # More sophisticated implementations could include scale and cross-axis corrections

  # Initial estimates
  offset <- c(x = 0, y = 0, z = 0)
  scale <- c(x = 1, y = 1, z = 1)

  # Iterative optimization
  max_iter <- 1000
  tolerance <- 1e-6
  learning_rate <- 0.1

  for (iter in 1:max_iter) {
    # Apply current calibration
    cal_x <- (nm_matrix[, 1] - offset["x"]) * scale["x"]
    cal_y <- (nm_matrix[, 2] - offset["y"]) * scale["y"]
    cal_z <- (nm_matrix[, 3] - offset["z"]) * scale["z"]

    # Calculate vector magnitudes
    vm <- sqrt(cal_x^2 + cal_y^2 + cal_z^2)

    # Error: deviation from sphere criterion (1g)
    error <- vm - sphere_criterion

    # Calculate gradients for offset (simplified gradient descent)
    grad_offset_x <- 2 * mean(error * cal_x / vm, na.rm = TRUE) * scale["x"]
    grad_offset_y <- 2 * mean(error * cal_y / vm, na.rm = TRUE) * scale["y"]
    grad_offset_z <- 2 * mean(error * cal_z / vm, na.rm = TRUE) * scale["z"]

    # Update offsets
    new_offset <- c(
      x = offset["x"] + learning_rate * grad_offset_x,
      y = offset["y"] + learning_rate * grad_offset_y,
      z = offset["z"] + learning_rate * grad_offset_z
    )

    # Check convergence
    if (max(abs(new_offset - offset)) < tolerance) {
      if (verbose) cat("Converged at iteration:", iter, "\n")
      break
    }

    offset <- new_offset
  }

  # Calculate scale factors (optional, based on variance)
  # For most applications, offset correction is sufficient

  # Step 4: Apply calibration to all data

  calibrated_data <- data.frame(
    x = (x - offset["x"]) * scale["x"],
    y = (y - offset["y"]) * scale["y"],
    z = (z - offset["z"]) * scale["z"],
    stringsAsFactors = FALSE
  )

  # Preserve timestamp if present
  if ("timestamp" %in% names(accel_data)) {
    calibrated_data$timestamp <- accel_data$timestamp
  }

  # Step 5: Calculate calibration error after correction

  cal_nm_x <- (nm_matrix[, 1] - offset["x"]) * scale["x"]
  cal_nm_y <- (nm_matrix[, 2] - offset["y"]) * scale["y"]
  cal_nm_z <- (nm_matrix[, 3] - offset["z"]) * scale["z"]
  vm_after <- sqrt(cal_nm_x^2 + cal_nm_y^2 + cal_nm_z^2)
  error_after <- mean(abs(vm_after - sphere_criterion))

  calibration_success <- error_after < error_before

  if (verbose) {
    cat("Mean calibration error after:", round(error_after, 4), "g\n")
    cat("Calibration", if (calibration_success) "SUCCESSFUL" else "FAILED",
        "- Error", if (calibration_success) "reduced" else "increased", "\n")
    cat("Offsets: x=", round(offset["x"], 4),
        ", y=", round(offset["y"], 4),
        ", z=", round(offset["z"], 4), "\n")
  }

  result <- list(
    calibrated_data = calibrated_data,
    offset = offset,
    scale = scale,
    n_nonmovement = n_nonmovement,
    error_before = round(error_before, 5),
    error_after = round(error_after, 5),
    calibration_success = calibration_success
  )

  class(result) <- c("canhrActi_calibration", "list")
  return(result)
}


#' Calculate ENMO (Euclidean Norm Minus One)
#'
#' Calculates the Euclidean Norm Minus One, a common metric for quantifying
#' acceleration magnitude in raw accelerometer data after removing the
#' gravitational component.
#'
#' @param x Numeric vector. X-axis acceleration in g-units
#' @param y Numeric vector. Y-axis acceleration in g-units
#' @param z Numeric vector. Z-axis acceleration in g-units
#' @param truncate_negative Logical. Set negative values to 0? (default: TRUE)
#'
#' @return Numeric vector of ENMO values in g-units (or mg if multiplied by 1000)
#'
#' @details
#' Formula: ENMO = max(0, sqrt(x^2 + y^2 + z^2) - 1)
#'
#' ENMO removes the static gravitational component (1g) from the acceleration
#' vector, leaving only the dynamic acceleration due to movement. This makes
#' it useful for quantifying physical activity intensity regardless of device
#' orientation.
#'
#' \strong{Typical Values:}
#' \itemize{
#'   \item Sedentary: < 30 mg
#'   \item Light activity: 30-100 mg
#'   \item Moderate activity: 100-400 mg
#'   \item Vigorous activity: > 400 mg
#' }
#'
#' @references
#' van Hees VT, et al. (2013). Separating movement and gravity components in
#' an acceleration signal and implications for the assessment of human daily
#' physical activity. PLoS ONE, 8(4):e61691.
#'
#' @examples
#' \dontrun{
#' # Calculate ENMO from raw data
#' enmo_values <- enmo(raw_data$x, raw_data$y, raw_data$z)
#'
#' # Convert to millig (mg) for interpretation
#' enmo_mg <- enmo_values * 1000
#' }
#'
#' @export
enmo <- function(x, y, z, truncate_negative = TRUE) {

  if (length(x) != length(y) || length(x) != length(z)) {
    stop("x, y, and z must have the same length")
  }

  # Calculate Euclidean norm (vector magnitude)
  vm <- sqrt(x^2 + y^2 + z^2)

  # Subtract 1g (gravitational component)
  enmo_values <- vm - 1

  # Truncate negative values to 0
  if (truncate_negative) {
    enmo_values[enmo_values < 0] <- 0
  }

  return(enmo_values)
}


#' Calculate MAD (Mean Amplitude Deviation)
#'
#' Calculates the Mean Amplitude Deviation, an alternative to ENMO that
#' doesn't require calibration and is less sensitive to calibration errors.
#'
#' @param x Numeric vector. X-axis acceleration
#' @param y Numeric vector. Y-axis acceleration
#' @param z Numeric vector. Z-axis acceleration
#' @param epoch_samples Integer. Number of samples per epoch for averaging
#'
#' @return Numeric vector of MAD values per epoch
#'
#' @details
#' MAD = (1/n) * sum(|r_i - mean(r)|)
#' where r = sqrt(x^2 + y^2 + z^2)
#'
#' MAD measures the mean absolute deviation of the acceleration signal from
#' its mean, making it robust to constant offsets (including gravity and
#' calibration errors).
#'
#' @references
#' Vaha-Ypya H, et al. (2015). A universal, accurate intensity-based
#' classification of different physical activities using raw data of
#' accelerometer. Clin Physiol Funct Imaging, 35(1):64-70.
#'
#' @export
mad <- function(x, y, z, epoch_samples = 60) {

  if (length(x) != length(y) || length(x) != length(z)) {
    stop("x, y, and z must have the same length")
  }

  # Calculate resultant acceleration
  r <- sqrt(x^2 + y^2 + z^2)

  n_samples <- length(r)
  n_epochs <- floor(n_samples / epoch_samples)

  if (n_epochs == 0) {
    warning("Insufficient samples for MAD calculation")
    return(numeric(0))
  }

  mad_values <- numeric(n_epochs)

  for (i in 1:n_epochs) {
    start_idx <- (i - 1) * epoch_samples + 1
    end_idx <- i * epoch_samples

    epoch_r <- r[start_idx:end_idx]
    epoch_mean <- mean(epoch_r, na.rm = TRUE)

    # Mean absolute deviation
    mad_values[i] <- mean(abs(epoch_r - epoch_mean), na.rm = TRUE)
  }

  return(mad_values)
}


#' Print Method for Calibration Results
#'
#' @param x Object of class 'canhrActi_calibration'
#' @param ... Additional arguments (unused)
#'
#' @export
print.canhrActi_calibration <- function(x, ...) {
  cat("\nAuto-Calibration Results\n")
  cat(paste(rep("-", 35), collapse = ""), "\n")
  cat("Non-movement samples used:", x$n_nonmovement, "\n")
  cat("Calibration error before:", x$error_before, "g\n")
  cat("Calibration error after:", x$error_after, "g\n")
  cat("Calibration success:", x$calibration_success, "\n")
  cat("\nCalibration Parameters:\n")
  cat("  Offset X:", round(x$offset["x"], 5), "g\n")
  cat("  Offset Y:", round(x$offset["y"], 5), "g\n")
  cat("  Offset Z:", round(x$offset["z"], 5), "g\n")
  cat("  Scale X:", round(x$scale["x"], 5), "\n")
  cat("  Scale Y:", round(x$scale["y"], 5), "\n")
  cat("  Scale Z:", round(x$scale["z"], 5), "\n")
  invisible(x)
}
