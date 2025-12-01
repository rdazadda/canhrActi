#' Read Multi-Brand Accelerometer Data
#'
#' Unified function to read accelerometer data from multiple device brands.
#' Automatically detects file format and dispatches to appropriate reader.
#'
#' @param filepath Path to accelerometer data file
#' @param epoch_length Epoch length in seconds for count calculation (default: 60)
#' @param verbose Logical. Print progress messages? (default: TRUE)
#' @param ... Additional arguments passed to device-specific readers
#'
#' @return List with standardized structure:
#'   \itemize{
#'     \item \code{data} - Data frame with timestamp, axis1, axis2, axis3, steps
#'     \item \code{settings} - Device and subject metadata
#'     \item \code{device_type} - Character. Device brand detected
#'     \item \code{file_type} - Character. File format
#'   }
#'
#' @details
#' \strong{Supported Formats:}
#' \itemize{
#'   \item \strong{ActiGraph}: .agd (processed), .gt3x (raw)
#'   \item \strong{GENEActiv}: .bin (raw)
#'   \item \strong{Axivity}: .cwa (raw)
#'   \item \strong{Generic CSV}: .csv (with x, y, z columns)
#' }
#'
#' All raw formats are converted to activity counts using the ActiGraph
#' algorithm (Neishabouri et al., 2022) for consistent analysis.
#'
#' @examples
#' \dontrun{
#' # Read any supported file
#' data <- read.accelerometer("participant.agd")
#' data <- read.accelerometer("participant.gt3x")
#' data <- read.accelerometer("participant.bin")  # GENEActiv
#' data <- read.accelerometer("participant.cwa")  # Axivity
#'
#' # Access standardized data
#' counts <- data$data
#' print(data$device_type)
#' }
#'
#' @export
read.accelerometer <- function(filepath,
                               epoch_length = 60,
                               verbose = TRUE,
                               ...) {

  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  # Detect file type from extension
  ext <- tolower(tools::file_ext(filepath))

  result <- switch(ext,
    "agd" = .read.agd.unified(filepath, verbose),
    "gt3x" = .read.gt3x.unified(filepath, epoch_length, verbose, ...),
    "bin" = .read.geneactiv.unified(filepath, epoch_length, verbose, ...),
    "cwa" = .read.axivity.unified(filepath, epoch_length, verbose, ...),
    "csv" = .read.csv.unified(filepath, epoch_length, verbose, ...),
    stop("Unsupported file format: .", ext,
         "\nSupported formats: .agd, .gt3x, .bin, .cwa, .csv")
  )

  return(result)
}


#' Read ActiGraph AGD File (Unified)
#'
#' @param filepath Path to .agd file
#' @param verbose Print messages?
#' @return Standardized data list
#' @keywords internal
.read.agd.unified <- function(filepath, verbose = TRUE) {

  if (verbose) cat("Reading ActiGraph AGD file\n")

  # Use existing read.agd function
  agd_data <- read.agd(filepath)
  counts_data <- agd.counts(agd_data)

  list(
    data = counts_data,
    settings = agd_data$settings,
    device_type = "ActiGraph",
    file_type = "agd"
  )
}


#' Read ActiGraph GT3X File (Unified)
#'
#' @param filepath Path to .gt3x file
#' @param epoch_length Epoch length in seconds
#' @param verbose Print messages?
#' @param ... Additional arguments
#' @return Standardized data list
#' @keywords internal
.read.gt3x.unified <- function(filepath, epoch_length = 60, verbose = TRUE, ...) {

  if (verbose) cat("Reading ActiGraph GT3X file\n")

  # Use existing read.gt3x.file function
  gt3x_data <- read.gt3x.file(filepath, epoch_length = epoch_length,
                               verbose = verbose, ...)

  counts_data <- data.frame(
    timestamp = as.POSIXct((gt3x_data$data$dataTimestamp / 10000000 - 62135596800),
                           origin = '1970-01-01', tz = 'UTC'),
    axis1 = gt3x_data$data$axis1,
    axis2 = gt3x_data$data$axis2,
    axis3 = gt3x_data$data$axis3,
    steps = gt3x_data$data$steps,
    stringsAsFactors = FALSE
  )

  list(
    data = counts_data,
    settings = gt3x_data$settings,
    device_type = "ActiGraph",
    file_type = "gt3x"
  )
}


#' Read GENEActiv BIN File
#'
#' Reads raw acceleration data from GENEActiv .bin files and converts to
#' activity counts using the ActiGraph algorithm.
#'
#' @param filepath Path to .bin file
#' @param epoch_length Epoch length in seconds (default: 60)
#' @param verbose Print progress messages? (default: TRUE)
#' @param calibrate Logical. Apply auto-calibration? (default: TRUE)
#'
#' @return Standardized data list
#'
#' @details
#' Requires the GENEAread package to be installed.
#' Raw acceleration is converted to ActiGraph-compatible counts for
#' consistent analysis across device brands.
#'
#' @keywords internal
.read.geneactiv.unified <- function(filepath, epoch_length = 60,
                                     verbose = TRUE, calibrate = TRUE, ...) {

  if (!requireNamespace("GENEAread", quietly = TRUE)) {
    stop("Package 'GENEAread' is required for GENEActiv .bin file support.\n",
         "Install it with: install.packages('GENEAread')")
  }

  if (verbose) cat("Reading GENEActiv BIN file\n")

  # Read raw data
  raw_data <- tryCatch({
    GENEAread::read.bin(filepath, downsample = 1, calibrate = calibrate)
  }, error = function(e) {
    stop("Failed to read GENEActiv file: ", e$message)
  })

  # Extract acceleration data
  accel_data <- raw_data$data.out

  if (verbose) {
    cat("Samples:", nrow(accel_data), "\n")
    cat("Sampling frequency:", raw_data$freq, "Hz\n")
  }

  # Extract timestamps and acceleration
  timestamps <- as.POSIXct(accel_data[, 1], origin = "1970-01-01", tz = "UTC")
  x <- accel_data[, 2]
  y <- accel_data[, 3]
  z <- accel_data[, 4]

  sampling_freq <- raw_data$freq

  # Convert to activity counts using ActiGraph algorithm
  if (verbose) cat("Converting to activity counts\n")

  if (requireNamespace("agcounts", quietly = TRUE)) {
    # Use official agcounts package if available
    raw_df <- data.frame(time = timestamps, X = x, Y = y, Z = z)
    counts_result <- agcounts::calculate_counts(raw_df, epoch = epoch_length)

    counts_x <- counts_result$Axis2
    counts_y <- counts_result$Axis1
    counts_z <- counts_result$Axis3
  } else {
    # Fall back to internal counts function
    counts_x <- counts(x, sampling_freq, epoch_length)
    counts_y <- counts(y, sampling_freq, epoch_length)
    counts_z <- counts(z, sampling_freq, epoch_length)
  }

  # Create epoch timestamps
  n_epochs <- length(counts_y)
  start_time <- timestamps[1]
  epoch_timestamps <- seq(start_time, by = epoch_length, length.out = n_epochs)

  counts_data <- data.frame(
    timestamp = epoch_timestamps,
    axis1 = counts_y,
    axis2 = counts_x,
    axis3 = counts_z,
    steps = NA_integer_,
    stringsAsFactors = FALSE
  )

  # Extract settings
  settings <- data.frame(
    settingName = c("subjectname", "deviceserial", "samplerate", "epochlength"),
    settingValue = c(
      if (!is.null(raw_data$subject.info$Code)) raw_data$subject.info$Code else "Unknown",
      if (!is.null(raw_data$header$Serial_Number)) raw_data$header$Serial_Number else "Unknown",
      as.character(sampling_freq),
      as.character(epoch_length)
    ),
    stringsAsFactors = FALSE
  )

  if (verbose) cat("GENEActiv file loaded successfully\n")

  list(
    data = counts_data,
    settings = settings,
    device_type = "GENEActiv",
    file_type = "bin"
  )
}


#' Read Axivity CWA File
#'
#' Reads raw acceleration data from Axivity .cwa files and converts to
#' activity counts using the ActiGraph algorithm.
#'
#' @param filepath Path to .cwa file
#' @param epoch_length Epoch length in seconds (default: 60)
#' @param verbose Print progress messages? (default: TRUE)
#'
#' @return Standardized data list
#'
#' @details
#' Requires the GGIRread or read.cwa package to be installed.
#' Raw acceleration is converted to ActiGraph-compatible counts for
#' consistent analysis across device brands.
#'
#' @keywords internal
.read.axivity.unified <- function(filepath, epoch_length = 60,
                                   verbose = TRUE, ...) {

  # Try GGIRread first, then read.cwa
  if (requireNamespace("GGIRread", quietly = TRUE)) {
    return(.read.axivity.ggirread(filepath, epoch_length, verbose))
  } else if (requireNamespace("read.cwa", quietly = TRUE)) {
    return(.read.axivity.readcwa(filepath, epoch_length, verbose))
  } else {
    stop("Package 'GGIRread' or 'read.cwa' is required for Axivity .cwa file support.\n",
         "Install with: install.packages('GGIRread') or install.packages('read.cwa')")
  }
}


#' Read Axivity using GGIRread
#' @keywords internal
.read.axivity.ggirread <- function(filepath, epoch_length, verbose) {

  if (verbose) cat("Reading Axivity CWA file using GGIRread\n")

  # Read raw data
  raw_data <- tryCatch({
    GGIRread::readAxivity(filepath, start = 0, end = 0)
  }, error = function(e) {
    stop("Failed to read Axivity file: ", e$message)
  })

  # Extract acceleration and timestamps
  timestamps <- as.POSIXct(raw_data$data$time, origin = "1970-01-01", tz = "UTC")
  x <- raw_data$data$x
  y <- raw_data$data$y
  z <- raw_data$data$z

  # Detect sampling frequency
  if (length(timestamps) > 1) {
    sampling_freq <- round(1 / as.numeric(median(diff(timestamps))))
  } else {
    sampling_freq <- 100  # Default for Axivity
  }

  if (verbose) {
    cat("Samples:", length(x), "\n")
    cat("Sampling frequency:", sampling_freq, "Hz\n")
  }

  # Convert to counts
  if (verbose) cat("Converting to activity counts\n")

  if (requireNamespace("agcounts", quietly = TRUE)) {
    raw_df <- data.frame(time = timestamps, X = x, Y = y, Z = z)
    counts_result <- agcounts::calculate_counts(raw_df, epoch = epoch_length)
    counts_x <- counts_result$Axis2
    counts_y <- counts_result$Axis1
    counts_z <- counts_result$Axis3
  } else {
    counts_x <- counts(x, sampling_freq, epoch_length)
    counts_y <- counts(y, sampling_freq, epoch_length)
    counts_z <- counts(z, sampling_freq, epoch_length)
  }

  n_epochs <- length(counts_y)
  start_time <- timestamps[1]
  epoch_timestamps <- seq(start_time, by = epoch_length, length.out = n_epochs)

  counts_data <- data.frame(
    timestamp = epoch_timestamps,
    axis1 = counts_y,
    axis2 = counts_x,
    axis3 = counts_z,
    steps = NA_integer_,
    stringsAsFactors = FALSE
  )

  settings <- data.frame(
    settingName = c("samplerate", "epochlength", "devicetype"),
    settingValue = c(as.character(sampling_freq), as.character(epoch_length), "Axivity"),
    stringsAsFactors = FALSE
  )

  if (verbose) cat("Axivity file loaded successfully\n")

  list(
    data = counts_data,
    settings = settings,
    device_type = "Axivity",
    file_type = "cwa"
  )
}


#' Read Axivity using read.cwa
#' @keywords internal
.read.axivity.readcwa <- function(filepath, epoch_length, verbose) {

  if (verbose) cat("Reading Axivity CWA file using read.cwa\n")

  raw_data <- tryCatch({
    read.cwa::read_cwa(filepath)
  }, error = function(e) {
    stop("Failed to read Axivity file: ", e$message)
  })

  data <- raw_data$data

  timestamps <- data$time
  x <- data$X
  y <- data$Y
  z <- data$Z

  # Detect sampling frequency from header or data
  sampling_freq <- if (!is.null(raw_data$header$frequency)) {
    raw_data$header$frequency
  } else {
    round(1 / as.numeric(median(diff(timestamps))))
  }

  if (verbose) {
    cat("Samples:", length(x), "\n")
    cat("Sampling frequency:", sampling_freq, "Hz\n")
    cat("Converting to activity counts\n")
  }

  # Convert to counts
  if (requireNamespace("agcounts", quietly = TRUE)) {
    raw_df <- data.frame(time = timestamps, X = x, Y = y, Z = z)
    counts_result <- agcounts::calculate_counts(raw_df, epoch = epoch_length)
    counts_x <- counts_result$Axis2
    counts_y <- counts_result$Axis1
    counts_z <- counts_result$Axis3
  } else {
    counts_x <- counts(x, sampling_freq, epoch_length)
    counts_y <- counts(y, sampling_freq, epoch_length)
    counts_z <- counts(z, sampling_freq, epoch_length)
  }

  n_epochs <- length(counts_y)
  start_time <- timestamps[1]
  epoch_timestamps <- seq(start_time, by = epoch_length, length.out = n_epochs)

  counts_data <- data.frame(
    timestamp = epoch_timestamps,
    axis1 = counts_y,
    axis2 = counts_x,
    axis3 = counts_z,
    steps = NA_integer_,
    stringsAsFactors = FALSE
  )

  settings <- data.frame(
    settingName = c("samplerate", "epochlength", "devicetype"),
    settingValue = c(as.character(sampling_freq), as.character(epoch_length), "Axivity"),
    stringsAsFactors = FALSE
  )

  if (verbose) cat("Axivity file loaded successfully\n")

  list(
    data = counts_data,
    settings = settings,
    device_type = "Axivity",
    file_type = "cwa"
  )
}


#' Read Generic CSV File
#'
#' Reads acceleration data from generic CSV files with x, y, z columns.
#'
#' @param filepath Path to .csv file
#' @param epoch_length Epoch length in seconds
#' @param verbose Print messages?
#' @param timestamp_col Column name for timestamps (default: auto-detect)
#' @param x_col Column name for X axis (default: auto-detect)
#' @param y_col Column name for Y axis (default: auto-detect)
#' @param z_col Column name for Z axis (default: auto-detect)
#' @param sampling_freq Sampling frequency in Hz (default: auto-detect)
#'
#' @return Standardized data list
#' @keywords internal
.read.csv.unified <- function(filepath, epoch_length = 60, verbose = TRUE,
                               timestamp_col = NULL, x_col = NULL, y_col = NULL,
                               z_col = NULL, sampling_freq = NULL, ...) {

  if (verbose) cat("Reading CSV accelerometer file\n")

  # Read CSV
  data <- read.csv(filepath, stringsAsFactors = FALSE)

  if (verbose) cat("Rows:", nrow(data), ", Columns:", ncol(data), "\n")

  # Auto-detect columns
  col_names <- tolower(names(data))

  # Timestamp column
  if (is.null(timestamp_col)) {
    ts_patterns <- c("timestamp", "time", "datetime", "date")
    for (pattern in ts_patterns) {
      match_idx <- grep(pattern, col_names)[1]
      if (!is.na(match_idx)) {
        timestamp_col <- names(data)[match_idx]
        break
      }
    }
  }

  # Axis columns
  if (is.null(x_col)) {
    x_patterns <- c("^x$", "accelerometer.x", "accel.x", "acc.x", "axis.?x")
    for (pattern in x_patterns) {
      match_idx <- grep(pattern, col_names, ignore.case = TRUE)[1]
      if (!is.na(match_idx)) {
        x_col <- names(data)[match_idx]
        break
      }
    }
  }

  if (is.null(y_col)) {
    y_patterns <- c("^y$", "accelerometer.y", "accel.y", "acc.y", "axis.?y")
    for (pattern in y_patterns) {
      match_idx <- grep(pattern, col_names, ignore.case = TRUE)[1]
      if (!is.na(match_idx)) {
        y_col <- names(data)[match_idx]
        break
      }
    }
  }

  if (is.null(z_col)) {
    z_patterns <- c("^z$", "accelerometer.z", "accel.z", "acc.z", "axis.?z")
    for (pattern in z_patterns) {
      match_idx <- grep(pattern, col_names, ignore.case = TRUE)[1]
      if (!is.na(match_idx)) {
        z_col <- names(data)[match_idx]
        break
      }
    }
  }

  # Validate required columns
  if (is.null(x_col) || is.null(y_col) || is.null(z_col)) {
    stop("Could not find x, y, z columns. Available columns: ",
         paste(names(data), collapse = ", "))
  }

  x <- data[[x_col]]
  y <- data[[y_col]]
  z <- data[[z_col]]

  # Get or detect timestamps
  if (!is.null(timestamp_col) && timestamp_col %in% names(data)) {
    timestamps <- as.POSIXct(data[[timestamp_col]])
  } else {
    warning("No timestamp column found. Creating sequential timestamps.")
    if (is.null(sampling_freq)) sampling_freq <- 30
    start_time <- as.POSIXct("2024-01-01 00:00:00")
    timestamps <- seq(start_time, by = 1/sampling_freq, length.out = nrow(data))
  }

  # Detect sampling frequency
  if (is.null(sampling_freq) && length(timestamps) > 1) {
    sampling_freq <- round(1 / as.numeric(median(diff(timestamps))))
  }
  if (is.null(sampling_freq)) sampling_freq <- 30

  if (verbose) cat("Sampling frequency:", sampling_freq, "Hz\n")

  # Check if data is raw or already counts
  is_raw <- max(abs(c(x, y, z)), na.rm = TRUE) < 20  # Raw data typically < 10g

  if (is_raw) {
    if (verbose) cat("Detected raw acceleration data. Converting to counts.\n")

    if (requireNamespace("agcounts", quietly = TRUE)) {
      raw_df <- data.frame(time = timestamps, X = x, Y = y, Z = z)
      counts_result <- agcounts::calculate_counts(raw_df, epoch = epoch_length)
      counts_x <- counts_result$Axis2
      counts_y <- counts_result$Axis1
      counts_z <- counts_result$Axis3
    } else {
      counts_x <- counts(x, sampling_freq, epoch_length)
      counts_y <- counts(y, sampling_freq, epoch_length)
      counts_z <- counts(z, sampling_freq, epoch_length)
    }

    n_epochs <- length(counts_y)
    start_time <- timestamps[1]
    epoch_timestamps <- seq(start_time, by = epoch_length, length.out = n_epochs)
  } else {
    if (verbose) cat("Detected epoch-level count data.\n")
    counts_x <- x
    counts_y <- y
    counts_z <- z
    epoch_timestamps <- timestamps
  }

  counts_data <- data.frame(
    timestamp = epoch_timestamps,
    axis1 = counts_y,
    axis2 = counts_x,
    axis3 = counts_z,
    steps = NA_integer_,
    stringsAsFactors = FALSE
  )

  settings <- data.frame(
    settingName = c("samplerate", "epochlength"),
    settingValue = c(as.character(sampling_freq), as.character(epoch_length)),
    stringsAsFactors = FALSE
  )

  if (verbose) cat("CSV file loaded successfully\n")

  list(
    data = counts_data,
    settings = settings,
    device_type = "Generic CSV",
    file_type = "csv"
  )
}


#' Check Available Device Support
#'
#' Lists which accelerometer device formats are supported based on
#' installed packages.
#'
#' @return Data frame with device types and availability
#'
#' @export
check.device.support <- function() {

  devices <- data.frame(
    device = c("ActiGraph AGD", "ActiGraph GT3X", "GENEActiv BIN",
               "Axivity CWA", "Generic CSV"),
    required_package = c("RSQLite", "read.gt3x + agcounts", "GENEAread",
                         "GGIRread or read.cwa", "base R"),
    installed = c(
      requireNamespace("RSQLite", quietly = TRUE),
      requireNamespace("read.gt3x", quietly = TRUE) &&
        requireNamespace("agcounts", quietly = TRUE),
      requireNamespace("GENEAread", quietly = TRUE),
      requireNamespace("GGIRread", quietly = TRUE) ||
        requireNamespace("read.cwa", quietly = TRUE),
      TRUE
    ),
    stringsAsFactors = FALSE
  )

  devices$status <- ifelse(devices$installed, "Available", "Not Available")

  cat("\ncanhrActi Device Support\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  for (i in 1:nrow(devices)) {
    status_symbol <- if (devices$installed[i]) "[OK]" else "[--]"
    cat(sprintf("%s %-20s %s\n", status_symbol, devices$device[i],
                if (!devices$installed[i])
                  paste("(install:", devices$required_package[i], ")") else ""))
  }

  cat("\n")
  invisible(devices)
}
