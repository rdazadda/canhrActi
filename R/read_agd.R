#' Read ActiGraph .agd File
#'
#' Reads epoch-level AGD files (pre-processed in ActiLife). AGD files contain
#' activity counts that have already been computed from raw acceleration data.
#'
#' @param filepath Path to .agd file
#' @param include_sleep Logical. Include sleep analysis data if available? (default: TRUE)
#' @param include_wear_time Logical. Include wear time validation data if available? (default: TRUE)
#' @param verbose Logical. Print progress messages? (default: TRUE)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{data} - Data frame with epoch-level activity counts (axis1, axis2, axis3, steps, etc.)
#'     \item \code{settings} - Data frame with device and subject settings
#'     \item \code{sleep} - Sleep periods detected by ActiLife (if available and requested)
#'     \item \code{awakenings} - Awakening events during sleep (if available and requested)
#'     \item \code{wear_time} - Wear time validation bouts (if available and requested)
#'     \item \code{capsense} - Capacitive sensor data for on-body detection (if available)
#'     \item \code{tables} - Character vector of all tables in the AGD file
#'   }
#'
#' @details
#' AGD files are SQLite databases created by ActiLife software. They contain
#' pre-processed activity counts at user-specified epoch lengths (e.g., 60 seconds).
#'
#' @export
read.agd <- function(filepath, include_sleep = TRUE, include_wear_time = TRUE, verbose = TRUE) {

  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required. Install it with: install.packages('RSQLite')")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), filepath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)

  if (verbose) {
    cat("AGD file tables:", paste(tables, collapse = ", "), "\n")
  }

  # Read settings
  settings <- NULL
  if ("settings" %in% tables) {
    settings <- DBI::dbReadTable(con, "settings")
  }

  # Read epoch data (activity counts)
  data <- NULL
  if ("data" %in% tables) {
    data <- DBI::dbReadTable(con, "data")
  } else if ("epochs" %in% tables) {
    data <- DBI::dbReadTable(con, "epochs")
  } else {
    stop(sprintf("Could not find 'data' or 'epochs' table in .agd file.\nAvailable tables: %s",
                 paste(tables, collapse = ", ")))
  }

  if (verbose) {
    cat("Epochs loaded:", nrow(data), "\n")
  }

  # Build result list

  result <- list(
    data = data,
    settings = settings,
    tables = tables
  )

  # Read sleep data if requested and available
  if (include_sleep) {
    if ("sleep" %in% tables) {
      result$sleep <- DBI::dbReadTable(con, "sleep")
      if (verbose && nrow(result$sleep) > 0) {
        cat("Sleep periods found:", nrow(result$sleep), "\n")
      }
    }
    if ("awakenings" %in% tables) {
      result$awakenings <- DBI::dbReadTable(con, "awakenings")
      if (verbose && nrow(result$awakenings) > 0) {
        cat("Awakenings found:", nrow(result$awakenings), "\n")
      }
    }
  }

  # Read wear time validation data if requested and available
  if (include_wear_time) {
    if ("wtvBouts" %in% tables) {
      result$wear_time <- DBI::dbReadTable(con, "wtvBouts")
      if (verbose && nrow(result$wear_time) > 0) {
        cat("Wear time bouts:", nrow(result$wear_time), "\n")
      }
    }
    if ("filters" %in% tables) {
      result$filters <- DBI::dbReadTable(con, "filters")
    }
  }

  # Read capsense data if available (on-body detection)
  if ("capsense" %in% tables) {
    result$capsense <- DBI::dbReadTable(con, "capsense")
    if (verbose && nrow(result$capsense) > 0) {
      cat("Capsense samples:", nrow(result$capsense), "\n")
    }
  }

  return(result)
}


#' Extract Counts from .agd Data
#'
#' Extracts activity counts, timestamps, steps, and inclinometer data (if available)
#' from AGD file data. Inclinometer data can be used to enhance sleep detection
#' for AGD files that lack raw acceleration.
#'
#' @param agd_data List returned from read.agd()
#' @param convert.timestamps Logical. Convert ActiGraph timestamps to POSIXct (default: TRUE)
#' @param include_inclinometer Logical. Include inclinometer data if available? (default: TRUE)
#'
#' @return Data frame with counts per minute, timestamps, and optional inclinometer data:
#'   \itemize{
#'     \item \code{timestamp} - POSIXct timestamp (if convert.timestamps = TRUE)
#'     \item \code{axis1} - Vertical axis counts (Y-axis)
#'     \item \code{axis2} - Horizontal axis counts (X-axis)
#'     \item \code{axis3} - Anterior-posterior axis counts (Z-axis)
#'     \item \code{steps} - Step counts
#'     \item \code{lux} - Light intensity (if available)
#'     \item \code{inclineOff} - Off-body indicator (if available)
#'     \item \code{inclineStanding} - Standing indicator (if available)
#'     \item \code{inclineSitting} - Sitting indicator (if available)
#'     \item \code{inclineLying} - Lying indicator (if available)
#'   }
#'
#' @details
#' Inclinometer data is particularly useful for sleep detection in AGD files:
#' \itemize{
#'   \item \code{inclineLying = 1} indicates the device detected lying posture
#'   \item Combined with low activity (Cole-Kripke sleep), this provides better specificity
#'   \item Not all AGD files contain inclinometer data (depends on ActiLife processing settings)
#' }
#'
#' @export
agd.counts <- function(agd_data, convert.timestamps = TRUE, include_inclinometer = TRUE) {
  # Input validation
  if (is.null(agd_data) || !is.list(agd_data)) {
    stop("agd_data must be a list (output from read.agd)")
  }
  if (is.null(agd_data$data) || !is.data.frame(agd_data$data)) {
    stop("agd_data$data must be a data.frame")
  }

  data <- agd_data$data

  if (nrow(data) == 0) {
    warning("AGD data is empty (0 rows)")
    return(data.frame(timestamp = character(0), axis1 = integer(0),
                      axis2 = integer(0), axis3 = integer(0),
                      steps = integer(0), stringsAsFactors = FALSE))
  }

  timestamps <- NULL

  if ("dataTimestamp" %in% names(data)) {
    if (convert.timestamps) {
      #  Add validation for dataTimestamp column
      if (!is.numeric(data$dataTimestamp)) {
        warning("dataTimestamp column is not numeric, attempting conversion")
        data$dataTimestamp <- as.numeric(data$dataTimestamp)
      }

      # Check for NA values
      na_count <- sum(is.na(data$dataTimestamp))
      if (na_count > 0) {
        warning("Found ", na_count, " NA values in dataTimestamp column")
      }

      timestamps <- as.POSIXct((data$dataTimestamp / 10000000 - 62135596800),
                               origin = '1970-01-01', tz = 'UTC')

      #  Validate timestamps are in reasonable range (1990-2050)
      min_valid_date <- as.POSIXct("1990-01-01", tz = "UTC")
      max_valid_date <- as.POSIXct("2050-01-01", tz = "UTC")
      valid_timestamps <- !is.na(timestamps) & timestamps >= min_valid_date & timestamps <= max_valid_date

      if (any(!valid_timestamps)) {
        n_invalid <- sum(!valid_timestamps)
        warning("Found ", n_invalid, " timestamps outside valid range (1990-2050). ",
                "This may indicate data corruption or incorrect timestamp format.")
      }
    } else {
      timestamps <- data$dataTimestamp
    }
  } else if ("timestamp" %in% names(data)) {
    timestamps <- data$timestamp

    #  Validate existing timestamps if POSIXct
    if (inherits(timestamps, "POSIXct")) {
      min_valid_date <- as.POSIXct("1990-01-01", tz = "UTC")
      max_valid_date <- as.POSIXct("2050-01-01", tz = "UTC")
      valid_timestamps <- !is.na(timestamps) & timestamps >= min_valid_date & timestamps <= max_valid_date

      if (any(!valid_timestamps)) {
        n_invalid <- sum(!valid_timestamps)
        warning("Found ", n_invalid, " timestamps outside valid range (1990-2050).")
      }
    }
  }

  #  Hard stop when no real timestamp column is present. A sequential
  #  fallback produces non-POSIXct integer "timestamps" that break all
  #  downstream day-level and circadian analyses, so fail fast at read time.
  if (is.null(timestamps)) {
    stop("No valid timestamp column found (dataTimestamp or timestamp). ",
         "Time-based, day-level, and circadian analyses require real timestamps.")
  }

  # Build base data frame with counts
  result <- data.frame(
    timestamp = timestamps,
    axis1 = if ("axis1" %in% names(data)) data$axis1 else NA_integer_,
    axis2 = if ("axis2" %in% names(data)) data$axis2 else NA_integer_,
    axis3 = if ("axis3" %in% names(data)) data$axis3 else NA_integer_,
    steps = if ("steps" %in% names(data)) data$steps else NA_integer_,
    stringsAsFactors = FALSE
  )

  # Add light data if available
  if ("lux" %in% names(data)) {
    result$lux <- data$lux
  }

  # Add inclinometer data if requested and available
  # Inclinometer data is valuable for sleep detection in AGD files
  if (include_inclinometer) {
    if ("inclineOff" %in% names(data)) {
      result$inclineOff <- data$inclineOff
    }
    if ("inclineStanding" %in% names(data)) {
      result$inclineStanding <- data$inclineStanding
    }
    if ("inclineSitting" %in% names(data)) {
      result$inclineSitting <- data$inclineSitting
    }
    if ("inclineLying" %in% names(data)) {
      result$inclineLying <- data$inclineLying
    }
  }

  return(result)
}


#' Check if AGD Data Has Inclinometer Information
#'
#' Checks whether an AGD file contains inclinometer data, which can be used
#' to enhance sleep detection algorithms.
#'
#' @param agd_data List returned from read.agd()
#' @return Logical. TRUE if inclinometer data is available
#' @export
has.inclinometer <- function(agd_data) {
  data <- agd_data$data
  inclinometer_cols <- c("inclineOff", "inclineStanding", "inclineSitting", "inclineLying")
  any(inclinometer_cols %in% names(data))
}

#' Extract Subject Information from AGD Settings
#'
#' @param agd_data List returned from read.agd()
#' @return List with subject information
#' @keywords internal
extract.subject.info <- function(agd_data) {
  if (is.null(agd_data$settings)) {
    return(list(
      subject_id = NA,
      sex = NA,
      age = NA,
      height = NA,
      mass = NA,
      weight_lbs = NA
    ))
  }

  settings <- agd_data$settings

  get_setting <- function(name) {
    value <- settings$settingValue[tolower(settings$settingName) == tolower(name)]
    if (length(value) == 0) return(NA)
    value <- as.character(value)
    if (value == "" || value == "0") return(NA)
    return(value)
  }

  subject_id <- get_setting("subjectname")
  sex <- get_setting("sex")
  age <- get_setting("age")
  height <- get_setting("height")
  mass <- get_setting("mass")

  weight_lbs <- NA
  if (!is.na(mass) && mass != "0") {
    weight_lbs <- round(as.numeric(mass) * 2.20462, 1)
  }

  if (!is.na(age) && age != "0") {
    age <- as.numeric(age)
  } else {
    age <- NA
  }

  if (!is.na(sex) && sex != "") {
    sex_lower <- tolower(as.character(sex))
    if (sex_lower %in% c("male", "m", "1", "true")) {
      sex <- "M"
    } else if (sex_lower %in% c("female", "f", "2", "false")) {
      sex <- "F"
    } else if (sex_lower %in% c("undefined", "0", "")) {
      sex <- ""
    } else {
      sex <- toupper(sex)
    }
  } else {
    sex <- ""
  }

  return(list(
    subject_id = if (!is.na(subject_id)) subject_id else NA,
    sex = sex,
    age = if (!is.na(age)) age else 0,
    height = if (!is.na(height)) as.numeric(height) else NA,
    mass = if (!is.na(mass)) as.numeric(mass) else NA,
    weight_lbs = if (!is.na(weight_lbs)) weight_lbs else 0
  ))
}

#' Get Path to Example AGD Files
#'
#' @param file Character. Name of the example file or "list" to see available files.
#' @return Character. Full path to the example AGD file.
#' @export
#' @examples
#' example_agd()
#' example_agd("list")
#' agd_path <- example_agd(1)
example_agd <- function(file = 1) {
  extdata_dir <- system.file("extdata", package = "canhrActi")
  if (extdata_dir == "") {
    stop("Example data not found. Package may not be installed correctly.")
  }
  agd_files <- list.files(extdata_dir, pattern = "\\.agd$", full.names = TRUE)
  if (length(agd_files) == 0) {
    stop("No AGD files found in extdata directory.")
  }
  if (is.character(file) && file == "list") {
    return(basename(agd_files))
  }
  if (is.numeric(file)) {
    if (file < 1 || file > length(agd_files)) {
      stop("File index out of range. Use example_agd('list') to see available files.")
    }
    return(agd_files[file])
  }
  matches <- grep(file, agd_files, value = TRUE)
  if (length(matches) == 0) {
    stop("No matching file found. Use example_agd('list') to see available files.")
  }
  return(matches[1])
}
