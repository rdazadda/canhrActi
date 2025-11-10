#' Generate Synthetic Accelerometer Data
#' @export
synthetic <- function(duration.hours = 24,
                                    sampling.freq = 60,
                                    add.activity = TRUE,
                                    n.bouts = 10,
                                    step.freq = 2) {
  n.samples <- duration.hours * 3600 * sampling.freq
  timestamps <- seq(from = as.POSIXct("2024-01-01 00:00:00"), by = 1/sampling.freq, length.out = n.samples)
  x <- rnorm(n.samples, mean = 0, sd = 0.02)
  y <- rnorm(n.samples, mean = 1, sd = 0.02)
  z <- rnorm(n.samples, mean = 0, sd = 0.02)
  if (add.activity) {
    for (i in 1:n.bouts) {
      bout.start <- sample(1:(n.samples - 600 * sampling.freq), 1)
      bout.length <- sample(300:900, 1) * sampling.freq
      bout.end <- min(bout.start + bout.length, n.samples)
      t <- seq(0, (bout.end - bout.start) / sampling.freq, length.out = bout.end - bout.start + 1)
      y[bout.start:bout.end] <- y[bout.start:bout.end] + 0.3 * sin(2 * pi * step.freq * t)
      x[bout.start:bout.end] <- x[bout.start:bout.end] + 0.1 * sin(2 * pi * step.freq * t + pi/4)
    }
  }
  data.frame(timestamp = timestamps, x = x, y = y, z = z, stringsAsFactors = FALSE)
}

#' Load ActiGraph CSV Data
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
#' @export
sample.rate <- function(timestamps) {
  if (length(timestamps) < 2) stop("Need at least 2 timestamps")
  intervals <- as.numeric(diff(timestamps))
  round(1 / median(intervals))
}

#' Subset Accelerometer Data by Time
#' @export
subset.time <- function(accel.data, start.time, end.time) {
  if (!"timestamp" %in% names(accel.data)) stop("Data must have a 'timestamp' column")
  if (is.character(start.time)) start.time <- as.POSIXct(start.time)
  if (is.character(end.time)) end.time <- as.POSIXct(end.time)
  accel.data[accel.data$timestamp >= start.time & accel.data$timestamp <= end.time, ]
}

#' Calculate Data Quality Metrics
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
#' @return List with valid.days, daily.summary, n.valid.days, and valid.day.index
#' @export
valid.days <- function(timestamps, wear_time, min.wear.hours = 10) {

  if (length(timestamps) != length(wear_time)) {
    stop("timestamps and wear_time must have the same length")
  }

  if (!inherits(timestamps, "POSIXct") && !inherits(timestamps, "POSIXt")) {
    stop("timestamps must be POSIXct or POSIXt class")
  }

  dates <- as.Date(timestamps)
  unique.dates <- unique(dates)

  daily.stats <- data.frame(
    date = character(),
    total.epochs = integer(),
    wear.epochs = integer(),
    wear.minutes = integer(),
    wear.hours = numeric(),
    is.valid = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(unique.dates)) {
    date <- unique.dates[i]
    day_idx <- dates == date

    total.epochs <- sum(day_idx)
    wear.epochs <- sum(wear_time[day_idx])
    wear.minutes <- wear.epochs
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
