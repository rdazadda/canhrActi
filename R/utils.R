# Input Validation Helpers

#' Validate and Clean Activity Counts
#' @param counts Numeric vector
#' @param name Name for error messages (default: "counts")
#' @param replace_na Replace NA with 0? (default: TRUE)
#' @return Cleaned numeric vector
#' @keywords internal
validate_counts <- function(counts, name = "counts", replace_na = TRUE) {
  if (length(counts) == 0) stop(name, " vector is empty")
  if (replace_na && any(is.na(counts))) {
    n_na <- sum(is.na(counts))
    warning(n_na, " NA values in ", name, ". Replacing with 0.")
    counts[is.na(counts)] <- 0
  }
  counts
}

#' Validate Vector Lengths Match
#' @param ... Named vectors to check
#' @keywords internal
validate_lengths <- function(...) {
  args <- list(...)
  lens <- sapply(args, length)
  if (length(unique(lens)) > 1) {
    msg <- paste(names(args), "=", lens, collapse = ", ")
    stop("Length mismatch: ", msg)
  }
  invisible(TRUE)
}

#' Validate Timestamps
#' @param ts Timestamps to validate
#' @keywords internal
validate_timestamps <- function(ts) {
  if (!inherits(ts, "POSIXct") && !inherits(ts, "POSIXt")) {
    stop("timestamps must be POSIXct class")
  }
  invisible(TRUE)
}

#' Load ActiGraph CSV Data
#'
#' @param filepath Character. Path to the CSV file
#' @param skip.lines Numeric. Number of header lines to skip (default: 10)
#'
#' @return Data frame with timestamp, x, y, z columns
#'
#' @export
load.actigraph.csv <- function(filepath, skip.lines = 10) {
  if (!file.exists(filepath)) stop(sprintf("File not found: %s", filepath))
  data <- read.csv(filepath, skip = skip.lines, header = TRUE, stringsAsFactors = FALSE)
  col.mapping <- c(
    "Accelerometer.X" = "x",
    "Accelerometer.Y" = "y",
    "Accelerometer.Z" = "z",
    "Accelerometer X" = "x",
    "Accelerometer Y" = "y",
    "Accelerometer Z" = "z",
    "X" = "x",
    "Y" = "y",
    "Z" = "z"
  )
  for (old.name in names(col.mapping)) {
    if (old.name %in% names(data)) names(data)[names(data) == old.name] <- col.mapping[old.name]
  }
  required <- c("x","y","z")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  if (!"timestamp" %in% names(data)) {
    warning("No timestamp column found. Creating sequential timestamps (assume 60 Hz).")
    start.time <- as.POSIXct("2024-01-01 00:00:00")
    data$timestamp <- seq(from = start.time, by = 1/60, length.out = nrow(data))
  }
  data[, c("timestamp","x","y","z")]
}

#' Detect Sampling Frequency from timestamps
#'
#' @param timestamps POSIXct vector of timestamps
#'
#' @return Numeric. Detected sampling frequency in Hz
#'
#' @export
sample.rate <- function(timestamps) {
  if (length(timestamps) < 2) stop("Need at least 2 timestamps")
  intervals <- as.numeric(diff(timestamps))
  round(1 / median(intervals))
}

#' Filter Accelerometer Data by Time Range
#'
#' @param accel.data Data frame with timestamp column
#' @param start.time POSIXct or character. Start time for filter
#' @param end.time POSIXct or character. End time for filter
#'
#' @return Data frame filtered to the specified time range
#'
#' @export
filter.time.range <- function(accel.data, start.time, end.time) {
  if (!"timestamp" %in% names(accel.data)) stop("Data must have a 'timestamp' column")
  if (is.character(start.time)) start.time <- as.POSIXct(start.time)
  if (is.character(end.time)) end.time <- as.POSIXct(end.time)
  accel.data[accel.data$timestamp >= start.time & accel.data$timestamp <= end.time, ]
}

#' Calculate Data Quality Metrics
#'
#' @param accel.data Data frame with x, y, z columns
#'
#' @return List with quality metrics (n.samples, missing counts, outliers)
#'
#' @export
quality <- function(accel.data) {
  required <- c("x","y","z")
  if (!all(required %in% names(accel.data))) stop("Data must have x, y, z columns")
  list(
    n.samples = nrow(accel.data),
    missing.x = sum(is.na(accel.data$x)),
    missing.y = sum(is.na(accel.data$y)),
    missing.z = sum(is.na(accel.data$z)),
    percent.missing = 100 * sum(is.na(accel.data[, required])) / (nrow(accel.data) * 3),
    mean.magnitude = mean(sqrt(accel.data$x^2 + accel.data$y^2 + accel.data$z^2), na.rm = TRUE),
    outliers.x = sum(abs(accel.data$x) > 10, na.rm = TRUE),
    outliers.y = sum(abs(accel.data$y) > 10, na.rm = TRUE),
    outliers.z = sum(abs(accel.data$z) > 10, na.rm = TRUE)
  )
}

#' Identify Valid Wear Days
#'
#' @param timestamps Vector of POSIXct timestamps
#' @param wear_time Logical vector indicating wear time
#' @param min.wear.hours Minimum hours of wear time for a valid day (default: 10)
#' @param epoch_length Numeric. Epoch length in seconds (default: 60)
#' @return List with valid.days, daily.summary, n.valid.days, and valid.day.index
#' @export
valid.days <- function(timestamps, wear_time, min.wear.hours = 10, epoch_length = 60) {

  if (length(timestamps) != length(wear_time)) {
    stop("timestamps and wear_time must have the same length")
  }

  if (!inherits(timestamps, "POSIXct") && !inherits(timestamps, "POSIXt")) {
    stop("timestamps must be POSIXct or POSIXt class")
  }

  # Calculate minutes per epoch based on epoch_length
 minutes_per_epoch <- epoch_length / 60

  dates <- as.Date(timestamps)
  unique.dates <- unique(dates)

  daily.stats <- data.frame(
    date = character(),
    total.epochs = integer(),
    wear.epochs = integer(),
    wear.minutes = numeric(),
    wear.hours = numeric(),
    is.valid = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(unique.dates)) {
    date <- unique.dates[i]
    day_idx <- dates == date

    total.epochs <- sum(day_idx)
    wear.epochs <- sum(wear_time[day_idx])
    wear.minutes <- wear.epochs * minutes_per_epoch
    wear.hours <- wear.minutes / 60
    is.valid <- wear.hours >= min.wear.hours

    daily.stats <- rbind(daily.stats, data.frame(
      date = as.character(date),
      total.epochs = total.epochs,
      wear.epochs = wear.epochs,
      wear.minutes = wear.minutes,
      wear.hours = round(wear.hours, 2),
      is.valid = is.valid,
      stringsAsFactors = FALSE
    ))
  }

  valid.dates <- daily.stats$date[daily.stats$is.valid]
  n.valid.days <- length(valid.dates)
  valid.day.index <- as.character(dates) %in% valid.dates

  result <- list(
    valid_days = valid.dates,
    daily_summary = daily.stats,
    n_valid_days = n.valid.days,
    valid_day_index = valid.day.index,
    min_wear_hours = min.wear.hours
  )

  class(result) <- c("canhrActi_valid_days", "list")
  return(result)
}

#' Print Method for Valid Days Results
#'
#' @param x Object of class 'canhrActi_valid_days'
#' @param ... Additional arguments (unused)
#'
#' @export
print.canhrActi_valid_days <- function(x, ...) {
  cat("\nValid Day Detection\n")
  cat("Criterion:", x$min_wear_hours, "hours\n")
  cat("Valid days:", x$n_valid_days, "/", nrow(x$daily_summary), "\n")
  if (x$n_valid_days > 0) {
    cat("Dates:", paste(x$valid_days, collapse = ", "), "\n")
  }
  invisible(x)
}
