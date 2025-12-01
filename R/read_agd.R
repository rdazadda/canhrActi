#' Read ActiGraph .agd File
#'
#' Reads both epoch-level AGD files (pre-processed in ActiLife) and raw AGD files
#' (high-frequency acceleration data). Raw AGD files are automatically detected
#' and converted to activity counts.
#'
#' @param filepath Path to .agd file
#' @param epoch_length Epoch length in seconds for raw data conversion (default: 60)
#' @param verbose Logical. Print progress messages? (default: TRUE)
#' @return List with data and settings
#' @export
read.agd <- function(filepath, epoch_length = 60, verbose = TRUE) {

  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required. Install it with: install.packages('RSQLite')")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), filepath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)

  # Read settings first (needed for sampling frequency)
  settings <- NULL
  if ("settings" %in% tables) {
    settings <- DBI::dbReadTable(con, "settings")
  }

  # Check if this is a raw AGD file (has accelerometer table)
  is_raw <- "accelerometer" %in% tables

  if (is_raw) {
    # Raw AGD file - contains high-frequency acceleration data
    if (verbose) cat("Detected raw AGD file. Converting to activity counts...\n")

    result <- .read.agd.raw(con, settings, epoch_length, verbose)

  } else {
    # Epoch AGD file - already has counts
    if ("data" %in% tables) {
      data <- DBI::dbReadTable(con, "data")
    } else if ("epochs" %in% tables) {
      data <- DBI::dbReadTable(con, "epochs")
    } else {
      stop(sprintf("Could not find 'data', 'epochs', or 'accelerometer' table in .agd file.\nAvailable tables: %s",
                   paste(tables, collapse = ", ")))
    }

    result <- list(
      data = data,
      settings = settings,
      is_raw = FALSE
    )
  }

  return(result)
}


#' Read Raw AGD File (Internal)
#'
#' Reads raw acceleration data from AGD file and converts to activity counts.
#'
#' @param con Database connection
#' @param settings Settings data frame
#' @param epoch_length Epoch length in seconds
#' @param verbose Print progress messages?
#' @return List with data and settings
#' @keywords internal
.read.agd.raw <- function(con, settings, epoch_length = 60, verbose = TRUE) {

  # Read raw accelerometer data
  raw_data <- DBI::dbReadTable(con, "accelerometer")

  if (verbose) cat("Raw samples:", nrow(raw_data), "\n")

  # Get sampling frequency from settings
  sampling_freq <- 30  # Default
  if (!is.null(settings)) {
    freq_row <- settings$settingValue[settings$settingName == "samplerate"]
    if (length(freq_row) > 0 && !is.na(freq_row[1])) {
      sampling_freq <- as.numeric(freq_row[1])
    }
  }

  if (verbose) cat("Sampling frequency:", sampling_freq, "Hz\n")

  # Extract acceleration columns
  # Raw AGD can have different column names: x/y/z, axis1/axis2/axis3, accelerometerX/Y/Z
  if ("x" %in% names(raw_data)) {
    x <- raw_data$x
    y <- raw_data$y
    z <- raw_data$z
  } else if ("axis1" %in% names(raw_data)) {
    x <- raw_data$axis1
    y <- raw_data$axis2
    z <- raw_data$axis3
  } else if ("accelerometerX" %in% names(raw_data)) {
    x <- raw_data$accelerometerX
    y <- raw_data$accelerometerY
    z <- raw_data$accelerometerZ
  } else {
    # Try to find numeric columns
    num_cols <- sapply(raw_data, is.numeric)
    if (sum(num_cols) >= 3) {
      cols <- names(raw_data)[num_cols][1:3]
      x <- raw_data[[cols[1]]]
      y <- raw_data[[cols[2]]]
      z <- raw_data[[cols[3]]]
      if (verbose) cat("Using columns:", paste(cols, collapse = ", "), "\n")
    } else {
      stop("Could not find acceleration columns in raw AGD file.\nAvailable columns: ",
           paste(names(raw_data), collapse = ", "))
    }
  }

  # Get timestamps
  if ("dataTimestamp" %in% names(raw_data)) {
    timestamps <- as.POSIXct((raw_data$dataTimestamp / 10000000 - 62135596800),
                             origin = '1970-01-01', tz = 'UTC')
  } else if ("timestamp" %in% names(raw_data)) {
    timestamps <- as.POSIXct(raw_data$timestamp)
  } else {
    # Create timestamps from start time in settings
    start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
    if (!is.null(settings)) {
      start_row <- settings$settingValue[settings$settingName == "startdatetime"]
      if (length(start_row) > 0 && !is.na(start_row[1])) {
        start_ticks <- as.numeric(start_row[1])
        start_time <- as.POSIXct((start_ticks / 10000000 - 62135596800),
                                 origin = '1970-01-01', tz = 'UTC')
      }
    }
    timestamps <- seq(start_time, by = 1/sampling_freq, length.out = nrow(raw_data))
  }

  if (verbose) cat("Converting raw acceleration to activity counts...\n")

  # Convert to activity counts
  # Try agcounts package first, fall back to internal function
  if (requireNamespace("agcounts", quietly = TRUE)) {
    if (verbose) cat("Using agcounts package for count conversion\n")

    raw_df <- data.frame(
      time = timestamps,
      X = x,
      Y = y,
      Z = z
    )

    counts_result <- tryCatch({
      agcounts::calculate_counts(raw_df, epoch = epoch_length, sample_rate = sampling_freq)
    }, error = function(e) {
      if (verbose) cat("agcounts failed:", e$message, "\nFalling back to internal conversion\n")
      NULL
    })

    if (!is.null(counts_result)) {
      # agcounts returns Axis1, Axis2, Axis3
      counts_x <- counts_result$Axis1
      counts_y <- counts_result$Axis2
      counts_z <- counts_result$Axis3

      n_epochs <- nrow(counts_result)
      start_time <- timestamps[1]
      epoch_timestamps <- seq(start_time, by = epoch_length, length.out = n_epochs)

      # Convert ActiGraph timestamp format
      epoch_ticks <- as.numeric(epoch_timestamps + 62135596800) * 10000000

      data <- data.frame(
        dataTimestamp = epoch_ticks,
        axis1 = counts_x,
        axis2 = counts_y,
        axis3 = counts_z,
        steps = 0,
        stringsAsFactors = FALSE
      )

      if (verbose) cat("Conversion complete:", n_epochs, "epochs\n")

      return(list(
        data = data,
        settings = settings,
        is_raw = TRUE,
        raw_samples = nrow(raw_data),
        sampling_freq = sampling_freq
      ))
    }
  }

  # Fall back to internal counts function
  if (verbose) cat("Using internal count conversion\n")

  counts_x <- counts(x, sampling_freq, epoch_length)
  counts_y <- counts(y, sampling_freq, epoch_length)
  counts_z <- counts(z, sampling_freq, epoch_length)

  n_epochs <- length(counts_x)
  start_time <- timestamps[1]
  epoch_timestamps <- seq(start_time, by = epoch_length, length.out = n_epochs)

  # Convert to ActiGraph timestamp format
  epoch_ticks <- as.numeric(epoch_timestamps + 62135596800) * 10000000

  data <- data.frame(
    dataTimestamp = epoch_ticks,
    axis1 = counts_x,
    axis2 = counts_y,
    axis3 = counts_z,
    steps = 0,
    stringsAsFactors = FALSE
  )

  if (verbose) cat("Conversion complete:", n_epochs, "epochs\n")

  return(list(
    data = data,
    settings = settings,
    is_raw = TRUE,
    raw_samples = nrow(raw_data),
    sampling_freq = sampling_freq
  ))
}


#' Check if AGD File is Raw
#'
#' @param filepath Path to .agd file
#' @return Logical. TRUE if raw AGD, FALSE if epoch AGD
#' @keywords internal
.is.agd.file <- function(filepath) {
  tolower(tools::file_ext(filepath)) == "agd"
}


#' Read AGD Metadata Only
#'
#' Quickly reads just the metadata/settings from an AGD file without loading data.
#'
#' @param filepath Path to .agd file
#' @return List with file info
#' @keywords internal
.read.agd.metadata <- function(filepath) {
  con <- DBI::dbConnect(RSQLite::SQLite(), filepath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)

  is_raw <- "accelerometer" %in% tables
  has_epochs <- "data" %in% tables || "epochs" %in% tables

  settings <- NULL
  if ("settings" %in% tables) {
    settings <- DBI::dbReadTable(con, "settings")
  }

  # Get row counts
  n_rows <- 0
  if (is_raw) {
    n_rows <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM accelerometer")[[1]]
  } else if (has_epochs) {
    table_name <- if ("data" %in% tables) "data" else "epochs"
    n_rows <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", table_name))[[1]]
  }

  list(
    is_raw = is_raw,
    has_epochs = has_epochs,
    n_rows = n_rows,
    tables = tables,
    settings = settings
  )
}


#' Extract Counts from .agd Data
#'
#' @param agd_data List returned from read.agd()
#' @param convert.timestamps Logical. Convert ActiGraph timestamps to POSIXct (default: TRUE)
#' @return Data frame with counts per minute and timestamps
#' @export
agd.counts <- function(agd_data, convert.timestamps = TRUE) {
  data <- agd_data$data

  if ("dataTimestamp" %in% names(data)) {
    if (convert.timestamps) {
      timestamps <- as.POSIXct((data$dataTimestamp / 10000000 - 62135596800),
                               origin = '1970-01-01', tz = 'UTC')
    } else {
      timestamps <- data$dataTimestamp
    }
  } else {
    timestamps <- seq_len(nrow(data))
  }

  data.frame(
    timestamp = timestamps,
    axis1 = if ("axis1" %in% names(data)) data$axis1 else NA,
    axis2 = if ("axis2" %in% names(data)) data$axis2 else NA,
    axis3 = if ("axis3" %in% names(data)) data$axis3 else NA,
    steps = if ("steps" %in% names(data)) data$steps else NA,
    stringsAsFactors = FALSE
  )
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
    value <- settings$settingValue[settings$settingName == name]
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
