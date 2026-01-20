#' Read ActiGraph Accelerometer Data
#'
#' Reads accelerometer data from ActiGraph AGD files (pre-processed epoch data).
#'
#' @param filepath Path to ActiGraph AGD file
#' @param verbose Logical. Print progress messages? (default: TRUE)
#' @param ... Additional arguments (unused, for compatibility)
#'
#' @return List with standardized structure:
#'   \itemize{
#'     \item \code{data} - Data frame with timestamp, axis1, axis2, axis3, steps
#'     \item \code{settings} - Device and subject metadata
#'     \item \code{device_type} - Character. Device brand ("ActiGraph")
#'     \item \code{file_type} - Character. File format ("agd")
#'   }
#'
#' @details
#' This function reads ActiGraph AGD files which contain pre-processed activity
#' counts from ActiLife software.
#'
#' @examples
#' \dontrun{
#' # Read AGD file
#' data <- read.accelerometer("participant.agd")
#'
#' # Access standardized data
#' counts <- data$data
#' print(data$device_type)
#' }
#'
#' @export
read.accelerometer <- function(filepath,
                               verbose = TRUE,
                               ...) {

  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  # Detect file type from extension
  ext <- tolower(tools::file_ext(filepath))

  if (ext != "agd") {
    stop("Unsupported file format: .", ext, "\n",
         "Only ActiGraph AGD files are supported.")
  }

  result <- .read.agd.unified(filepath, verbose)
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


#' Check Available Device Support
#'
#' Lists supported accelerometer device formats.
#'
#' @return Data frame with device types and availability
#'
#' @export
check.device.support <- function() {

  devices <- data.frame(
    device = c("ActiGraph AGD"),
    required_package = c("RSQLite"),
    installed = c(requireNamespace("RSQLite", quietly = TRUE)),
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
