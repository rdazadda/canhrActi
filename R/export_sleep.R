#' Export Sleep Data to ActiLife Format (Internal)
#'
#' Internal function called by batch_sleep.R to export ActiLife-compatible files.
#'
#' @param results.list List of individual sleep results
#' @param output_dir Output directory
#' @keywords internal
.export.actilife.sleep <- function(results.list, output_dir) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Export 1: BatchSleepExportDetails.csv (all sleep periods)
  details.file <- file.path(output_dir, "BatchSleepExportDetails.csv")
  details.data <- .create.actilife.batch.details(results.list)
  write.csv(details.data, details.file, row.names = FALSE)
  cat("Exported:", details.file, "\n")

  # Export 2: BatchSleepExportSummary.csv (one row per participant)
  summary.file <- file.path(output_dir, "BatchSleepExportSummary.csv")
  summary.data <- .create.actilife.batch.summary(results.list)
  write.csv(summary.data, summary.file, row.names = FALSE)
  cat("Exported:", summary.file, "\n")
}


#' Read AGD Metadata (Internal Helper)
#'
#' Extracts participant metadata from AGD database file.
#'
#' @param file_path Path to AGD file
#' @return List with metadata fields
#' @keywords internal
.read.agd.metadata <- function(file_path) {
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), file_path)
    settings <- DBI::dbReadTable(con, "settings")
    DBI::dbDisconnect(con)

    # Helper to safely get settings
    get_setting <- function(name, default = "") {
      val <- settings$settingValue[settings$settingName == name]
      if (length(val) == 0 || is.na(val)) return(default)
      return(val)
    }

    # Extract subject name
    subject_name <- get_setting("subjectname")
    if (subject_name == "" || subject_name == "0") {
      subject_name <- basename(file_path)
      subject_name <- sub("\\.[aA][gG][dD]$", "", subject_name)
    }

    # Extract and format gender
    sex <- get_setting("sex")
    gender <- .format.gender(sex)

    # Extract numeric values
    weight_kg <- as.numeric(get_setting("mass", "0"))
    weight_lbs <- if (!is.na(weight_kg) && weight_kg > 0) {
      round(weight_kg * 2.20462, 1)
    } else {
      0
    }

    list(
      subject_name = subject_name,
      serial_number = get_setting("deviceserial"),
      epoch_length = as.numeric(get_setting("epochlength", "60")),
      weight_lbs = weight_lbs,
      age = as.numeric(get_setting("age", "0")),
      gender = gender
    )
  }, error = function(e) {
    # Return defaults on error
    list(
      subject_name = sub("\\.[aA][gG][dD]$", "", basename(file_path)),
      serial_number = "",
      epoch_length = 60,
      weight_lbs = 0,
      age = 0,
      gender = "Undefined"
    )
  })
}


#' Format Gender for ActiLife (Internal Helper)
#'
#' Converts sex code to ActiLife format.
#'
#' @param sex Sex character code
#' @return Formatted gender string
#' @keywords internal
.format.gender <- function(sex) {
  if (length(sex) == 0 || is.na(sex) || nchar(sex) == 0) {
    return("Undefined")
  }
  first_char <- substr(sex, 1, 1)
  switch(first_char,
         "F" = "Female",
         "M" = "Male",
         "Undefined")
}


#' Create ActiLife Batch Details (Internal)
#'
#' Creates the BatchSleepExportDetails.csv file with one row per sleep period.
#'
#' @param results.list List of sleep results
#' @return Data frame in ActiLife format
#' @keywords internal
.create.actilife.batch.details <- function(results.list) {

  all.rows <- list()

  for (file.name in names(results.list)) {
    result <- results.list[[file.name]]

    if (is.null(result) || nrow(result$sleep_periods) == 0) {
      next
    }

    # Get participant info from parameters
    params <- result$parameters
    file.path <- params$file_path

    # Extract info from AGD file
    metadata <- .read.agd.metadata(file.path)
    subject.name <- metadata$subject_name
    serial.number <- metadata$serial_number
    epoch.length <- metadata$epoch_length
    weight.lbs <- metadata$weight_lbs
    age <- metadata$age
    gender <- metadata$gender

    # Get algorithm names
    sleep.algorithm <- params$sleep_algorithm
    algorithm.display <- if (sleep.algorithm == "cole.kripke") {
      "Cole-Kripke"
    } else if (sleep.algorithm == "sadeh") {
      "Sadeh"
    } else {
      sleep.algorithm
    }

    # Process each sleep period
    periods <- result$sleep_periods

    for (i in 1:nrow(periods)) {
      period <- periods[i, ]

      # Format dates to M/D/YYYY H:MM:SS AM/PM
      in.bed <- .format.actilife.datetime(period$in_bed_time)
      out.bed <- .format.actilife.datetime(period$out_bed_time)
      onset <- .format.actilife.datetime(period$onset)

      # Calculate latency (minutes from in bed to onset)
      latency <- as.numeric(difftime(period$onset, period$in_bed_time, units = "mins"))

      # Calculate Sleep Fragmentation Index
      sleep.frag.index <- period$movement_index + period$fragmentation_index

      # Create row
      row.data <- data.frame(
        `Subject Name` = subject.name,
        `File Name` = basename(file.name),
        `Serial Number` = serial.number,
        `Epoch Length` = epoch.length,
        Weight = weight.lbs,
        Age = age,
        Gender = gender,
        `Sleep/Wake Algorithm` = algorithm.display,
        `Sleep Period Detection Algorithm` = "Tudor-Locke Default",
        `In Bed Time` = in.bed,
        `Out Bed Time` = out.bed,
        Efficiency = round(period$sleep_efficiency, 3),
        Onset = onset,
        Latency = round(latency, 0),
        `Total Sleep Time` = round(period$sleep_time, 0),
        WASO = round(period$wake_time, 0),
        `Number of Awakenings` = period$number_of_awakenings,
        `Length of Awakenings in Minutes` = round(period$average_awakening, 2),
        `Activity Counts` = round(period$total_counts, 0),
        `Movement Index` = round(period$movement_index, 3),
        `Fragmentation Index` = round(period$fragmentation_index, 3),
        `Sleep Fragmentation Index` = round(sleep.frag.index, 3),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      all.rows[[length(all.rows) + 1]] <- row.data
    }
  }

  if (length(all.rows) == 0) {
    return(data.frame())
  }

  # Combine all rows
  combined <- do.call(rbind, all.rows)
  return(combined)
}


#' Create ActiLife Batch Summary (Internal)
#'
#' Creates the BatchSleepExportSummary.csv file with one row per participant.
#'
#' @param results.list List of sleep results
#' @return Data frame in ActiLife format
#' @keywords internal
.create.actilife.batch.summary <- function(results.list) {

  all.rows <- list()

  for (file.name in names(results.list)) {
    result <- results.list[[file.name]]

    if (is.null(result) || nrow(result$sleep_periods) == 0) {
      next
    }

    # Get participant info from parameters
    params <- result$parameters
    file.path <- params$file_path

    # Extract info from AGD file
    metadata <- .read.agd.metadata(file.path)
    subject.name <- metadata$subject_name
    serial.number <- metadata$serial_number
    epoch.length <- metadata$epoch_length
    weight.lbs <- metadata$weight_lbs
    age <- metadata$age
    gender <- metadata$gender

    # Get algorithm names
    sleep.algorithm <- params$sleep_algorithm
    algorithm.display <- if (sleep.algorithm == "cole.kripke") {
      "Cole-Kripke"
    } else if (sleep.algorithm == "sadeh") {
      "Sadeh"
    } else {
      sleep.algorithm
    }

    # Calculate averages across all sleep periods for this participant
    periods <- result$sleep_periods
    n.periods <- nrow(periods)

    # Average times (format as H:MM AM/PM)
    avg.in.bed <- .format.average.time(periods$in_bed_time)
    avg.out.bed <- .format.average.time(periods$out_bed_time)
    avg.onset <- .format.average.time(periods$onset)

    # Average latency
    latencies <- sapply(1:nrow(periods), function(i) {
      as.numeric(difftime(periods$onset[i], periods$in_bed_time[i], units = "mins"))
    })
    avg.latency <- round(mean(latencies), 0)

    # Other averages
    avg.efficiency <- round(mean(periods$sleep_efficiency), 3)
    avg.sleep.time <- round(mean(periods$sleep_time), 0)
    avg.waso <- round(mean(periods$wake_time), 2)
    avg.awakenings <- round(mean(periods$number_of_awakenings), 2)
    avg.awakening.length <- round(mean(periods$average_awakening), 2)
    avg.counts <- round(mean(periods$total_counts), 2)
    avg.movement <- round(mean(periods$movement_index), 3)
    avg.fragmentation <- round(mean(periods$fragmentation_index), 3)
    avg.sleep.frag <- round(mean(periods$movement_index + periods$fragmentation_index), 3)

    # Create row
    row.data <- data.frame(
      `Subject Name` = subject.name,
      `File Name` = basename(file.name),
      `Serial Number` = serial.number,
      `Epoch Length` = epoch.length,
      Weight = weight.lbs,
      Age = age,
      Gender = gender,
      `Sleep/Wake Algorithm` = algorithm.display,
      `Sleep Period Detection Algorithm` = "Tudor-Locke Default",
      `Number of Sleep Periods` = n.periods,
      `Average In Bed Time` = avg.in.bed,
      `Average Out Bed Time` = avg.out.bed,
      `Average Efficiency` = avg.efficiency,
      `Average Onset` = avg.onset,
      `Average Latency` = avg.latency,
      `Average Total Sleep Time` = avg.sleep.time,
      `Average WASO` = avg.waso,
      `Average Number of Awakenings` = avg.awakenings,
      `Average Length of Awakenings in Minutes` = avg.awakening.length,
      `Average Activity Counts` = avg.counts,
      `Average Movement Index` = avg.movement,
      `Average Fragmentation Index` = avg.fragmentation,
      `Average Sleep Fragmentation Index` = avg.sleep.frag,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    all.rows[[length(all.rows) + 1]] <- row.data
  }

  if (length(all.rows) == 0) {
    return(data.frame())
  }

  # Combine all rows
  combined <- do.call(rbind, all.rows)
  return(combined)
}


#' Format DateTime for ActiLife (Internal)
#'
#' Converts R datetime to ActiLife format: M/D/YYYY H:MM:SS AM/PM
#'
#' @param dt POSIXct datetime
#' @return Character string in ActiLife format
#' @keywords internal
.format.actilife.datetime <- function(dt) {
  if (is.null(dt) || length(dt) == 0) return("")
  if (is.na(dt)) return("")

  # Ensure it's a POSIXct object
  if (!inherits(dt, "POSIXt")) {
    dt <- as.POSIXct(dt)
  }

  # Format as M/D/YYYY H:MM:SS AM/PM
  formatted <- format.POSIXct(dt, format = "%m/%d/%Y %I:%M:%S %p")

  # Remove leading zeros from month and day
  formatted <- gsub("^0", "", formatted)  # Month
  formatted <- gsub("/0", "/", formatted)  # Day

  return(formatted)
}


#' Format Average Time for ActiLife (Internal)
#'
#' Converts average of POSIXct times to ActiLife format: H:MM AM/PM
#' Uses circular mean to handle times around midnight properly
#'
#' @param times Vector of POSIXct datetimes
#' @return Character string in ActiLife time format
#' @keywords internal
.format.average.time <- function(times) {
  if (length(times) == 0) return("")

  # Ensure times are POSIXct
  if (!inherits(times, "POSIXt")) {
    times <- as.POSIXct(times)
  }

  # Extract hour and minute components
  hours <- as.numeric(format(times, "%H"))
  minutes <- as.numeric(format(times, "%M"))

  # Convert to minutes since midnight
  minutes.since.midnight <- hours * 60 + minutes

  # Use circular mean for times around midnight
  # Convert to angles (0-360 degrees, 0 = midnight)
  angles.rad <- (minutes.since.midnight / 1440) * 2 * pi

  # Calculate circular mean
  sin.mean <- mean(sin(angles.rad), na.rm = TRUE)
  cos.mean <- mean(cos(angles.rad), na.rm = TRUE)

  # Convert back to angle
  mean.angle <- atan2(sin.mean, cos.mean)
  if (mean.angle < 0) mean.angle <- mean.angle + 2 * pi

  # Convert to minutes since midnight
  avg.minutes <- (mean.angle / (2 * pi)) * 1440

  # Convert back to hours and minutes
  avg.hour <- floor(avg.minutes / 60) %% 24
  avg.min <- round(avg.minutes %% 60)

  # Format as H:MM AM/PM
  if (avg.hour == 0) {
    time.str <- sprintf("12:%02d AM", avg.min)
  } else if (avg.hour < 12) {
    time.str <- sprintf("%d:%02d AM", avg.hour, avg.min)
  } else if (avg.hour == 12) {
    time.str <- sprintf("12:%02d PM", avg.min)
  } else {
    time.str <- sprintf("%d:%02d PM", avg.hour - 12, avg.min)
  }

  return(time.str)
}
