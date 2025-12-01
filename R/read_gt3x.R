#' Read ActiGraph .gt3x File (Raw Acceleration Data)
#'
#' Reads raw acceleration data from .gt3x files and converts to activity counts
#' using the Neishabouri et al. (2022) algorithm. This function allows direct
#' analysis of raw accelerometer data without requiring ActiLife software.
#'
#' @param filepath Path to .gt3x file
#' @param epoch_length Epoch length in seconds (default: 60)
#' @param lfe_mode Logical. Low Frequency Extension mode (default: FALSE)
#' @param verbose Logical. Print progress messages? (default: TRUE)
#'
#' @return List with same structure as read.agd():
#'   \itemize{
#'     \item \code{data} - Data frame with axis1, axis2, axis3, steps, dataTimestamp
#'     \item \code{settings} - List with subject information and device settings
#'   }
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Reads raw X, Y, Z acceleration data from .gt3x file using read.gt3x package
#'   \item Applies the ActiGraph counts algorithm (Neishabouri et al., 2022) to each axis
#'   \item Returns data in same format as .agd files for seamless integration
#' }
#'
#' \strong{Note:} .gt3x files contain raw acceleration data sampled at 30-100 Hz,
#' which is converted to activity counts per epoch. This is more computationally
#' intensive than reading pre-processed .agd files but provides access to the
#' original unfiltered data.
#'
#' \strong{Step Counting:} Currently, step counts are not calculated from raw .gt3x
#' data. This feature may be added in future versions. The steps column will contain NA values.
#'
#' @references
#' Neishabouri A, et al. (2022). Quantification of acceleration as activity
#' counts in ActiGraph wearable. Scientific Reports, 12(1), 11958.
#'
#' @examples
#' \dontrun{
#' # Read .gt3x file
#' gt3x_data <- read.gt3x.file("participant001.gt3x")
#'
#' # Extract counts (same format as .agd)
#' counts_data <- agd.counts(gt3x_data)
#'
#' # Or use directly with canhrActi()
#' results <- canhrActi("participant001.gt3x")
#' }
#'
#' @export
read.gt3x.file <- function(filepath, epoch_length = 60, lfe_mode = FALSE, verbose = TRUE) {

  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  if (!requireNamespace("read.gt3x", quietly = TRUE)) {
    stop("Package 'read.gt3x' is required for .gt3x file support.\n",
         "Install it with: install.packages('read.gt3x')")
  }

  if (verbose) {
    cat("Reading raw acceleration data from .gt3x file\n")
  }

  # Read raw acceleration data
  # read.gt3x returns: timestamp, X, Y, Z (in g units)
  # IMPORTANT: imputeZeroes = TRUE fills gaps from idle sleep mode with zeros
  #            This matches ActiLife behavior and ensures epoch counts match .agd files
  raw_data <- tryCatch({
    read.gt3x::read.gt3x(filepath, asDataFrame = TRUE, imputeZeroes = TRUE)
  }, error = function(e) {
    stop("Failed to read .gt3x file: ", e$message, "\n",
         "Please ensure the file is a valid ActiGraph .gt3x file.")
  })

  # Parse metadata from .gt3x file
  info <- tryCatch({
    read.gt3x::parse_gt3x_info(filepath)
  }, error = function(e) {
    warning("Could not parse .gt3x metadata. Using default values.")
    list()
  })

  # Extract sampling frequency
  sampling_freq <- if (!is.null(info$Sample_Rate)) {
    as.numeric(info$Sample_Rate)
  } else {
    warning("Sampling frequency not found in .gt3x file. Assuming 30 Hz.")
    30
  }

  if (verbose) {
    cat("Sampling frequency:", sampling_freq, "Hz\n")
    cat("Converting raw acceleration to activity counts\n")
  }

  # Calculate activity counts using the official agcounts package
  # This ensures 100% compatibility with ActiGraph's algorithm
  if (!requireNamespace("agcounts", quietly = TRUE)) {
    stop("Package 'agcounts' is required for .gt3x support.\n",
         "Install it with: install.packages('agcounts')")
  }

  # Calculate counts using official algorithm
  counts_result <- agcounts::calculate_counts(raw_data, epoch = epoch_length)

  # Extract counts for each axis
  # Note: ActiGraph axis convention:
  #   X = right-left (horizontal, perpendicular to body)
  #   Y = forward-backward (vertical when worn at hip)
  #   Z = up-down (anterior-posterior when worn at hip)
  #
  # In ActiLife/AGD files:
  #   axis1 = Y (vertical axis) - most commonly used
  #   axis2 = X (horizontal)
  #   axis3 = Z (anterior-posterior)

  counts_x <- counts_result$Axis2  # X-axis
  counts_y <- counts_result$Axis1  # Y-axis
  counts_z <- counts_result$Axis3  # Z-axis

  # Create epoch timestamps
  # Each epoch represents epoch_length seconds of data
  start_time <- if (!is.null(info$Start_Date)) {
    as.POSIXct(info$Start_Date, format = "%H:%M:%S %d/%m/%Y", tz = "UTC")
  } else {
    as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  }

  n_epochs <- length(counts_y)
  epoch_timestamps <- seq(start_time,
                          by = epoch_length,
                          length.out = n_epochs)

  # Convert POSIXct to ActiGraph timestamp format (100-nanosecond intervals since 0001-01-01)
  # This matches the dataTimestamp format in .agd files
  actigraph_timestamps <- as.numeric(epoch_timestamps) + 62135596800
  actigraph_timestamps <- actigraph_timestamps * 10000000

  if (verbose) {
    cat("Epochs created:", n_epochs, "\n")
    cat("Epoch length:", epoch_length, "seconds\n")
  }

  # Create data frame in same format as .agd files
  data <- data.frame(
    dataTimestamp = actigraph_timestamps,
    axis1 = counts_y,  # Y axis (vertical) -> axis1
    axis2 = counts_x,  # X axis (horizontal) -> axis2
    axis3 = counts_z,  # Z axis (anterior-posterior) -> axis3
    steps = NA_integer_,  # Step counting from raw data not yet implemented
    lux = NA_real_,       # Light data not available in basic .gt3x files
    inclineOff = NA_integer_,
    inclineStanding = NA_integer_,
    inclineSitting = NA_integer_,
    inclineLying = NA_integer_,
    stringsAsFactors = FALSE
  )

  # Extract settings from .gt3x metadata
  settings <- .extract.gt3x.settings(info)

  if (verbose) {
    cat(".gt3x file loaded and converted to counts\n")
  }

  return(list(
    data = data,
    settings = settings
  ))
}


#' Extract Settings from GT3X Metadata
#'
#' Internal function to parse .gt3x file metadata and convert to settings format
#' compatible with .agd files.
#'
#' @param info List returned from read.gt3x::parse_gt3x_info()
#' @return Data frame with settings in .agd format
#' @keywords internal
.extract.gt3x.settings <- function(info) {

  # Create settings data frame in same format as .agd files
  settings <- data.frame(
    settingName = character(),
    settingValue = character(),
    stringsAsFactors = FALSE
  )

  # Helper function to add setting
  add_setting <- function(name, value) {
    if (!is.null(value) && !is.na(value) && value != "") {
      settings <<- rbind(settings, data.frame(
        settingName = name,
        settingValue = as.character(value),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Extract common settings from .gt3x metadata
  add_setting("subjectname", info$Subject_Name)
  add_setting("sex", info$Sex)
  add_setting("age", info$Age)
  add_setting("height", info$Height)
  add_setting("mass", info$Mass)
  add_setting("race", info$Race)
  add_setting("startdatetime", info$Start_Date)
  add_setting("stopdatetime", info$Stop_Date)
  add_setting("epochlength", info$Epoch_Length)
  add_setting("deviceserial", info$Serial_Number)
  add_setting("deviceversion", info$Device_Type)
  add_setting("filter", info$Filter)
  add_setting("actualstartdate", info$Start_Date)

  # Add sampling rate
  if (!is.null(info$Sample_Rate)) {
    add_setting("samplerate", info$Sample_Rate)
  }

  # If settings is empty, add minimal default settings
  if (nrow(settings) == 0) {
    settings <- data.frame(
      settingName = c("subjectname", "epochlength", "samplerate"),
      settingValue = c("Unknown", "60", "30"),
      stringsAsFactors = FALSE
    )
  }

  return(settings)
}


#' Check if File is GT3X Format
#'
#' Quick check to determine if a file is in .gt3x format based on extension.
#'
#' @param filepath Character. Path to file
#' @return Logical. TRUE if file has .gt3x extension
#' @keywords internal
.is.gt3x.file <- function(filepath) {
  grepl("\\.gt3x$", filepath, ignore.case = TRUE)
}


#' Check if File is AGD Format
#'
#' Quick check to determine if a file is in .agd format based on extension.
#'
#' @param filepath Character. Path to file
#' @return Logical. TRUE if file has .agd extension
#' @keywords internal
.is.agd.file <- function(filepath) {
  grepl("\\.agd$", filepath, ignore.case = TRUE)
}
