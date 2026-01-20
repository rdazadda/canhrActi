#' Export ActiLife-Compatible Summary Report
#'
#' Exports a summary CSV file matching ActiLife's Desktop_Summary.csv format.
#' One row per participant per valid day.
#'
#' @param analysis_results An canhrActi_analysis object from canhrActi() function
#' @param output_path Character. Full path for output CSV file
#' @param subject_id Character. Subject identifier (default: extracts from filename)
#' @param weight_lbs Numeric. Weight in pounds (optional)
#' @param age Numeric. Age in years (optional)
#' @param gender Character. Gender ("M", "F", or "") (optional)
#'
#' @return Invisibly returns the data frame that was exported
#' @export
export_summary <- function(analysis_results,
                           output_path,
                           subject_id = NULL,
                           weight_lbs = NULL,
                           age = NULL,
                           gender = NULL) {

  if (!inherits(analysis_results, "canhrActi_analysis")) {
    stop("analysis_results must be an canhrActi_analysis object from canhrActi()")
  }

  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    if (is.null(subject_id)) subject_id <- subj$subject_id
    if (is.null(weight_lbs)) weight_lbs <- subj$weight_lbs
    if (is.null(age)) age <- subj$age
    if (is.null(gender)) gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  daily <- analysis_results$daily_summary

  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60  # default
  }

  filename <- basename(analysis_results$parameters$file_path)

  if ("is_valid_day" %in% names(daily)) {
    valid_daily <- daily[daily$is_valid_day == TRUE, ]
  } else if ("is.valid" %in% names(daily)) {
    valid_daily <- daily[daily$is.valid == TRUE, ]
  } else {
    stop("Could not find validity column (is_valid_day or is.valid) in daily_summary")
  }

  if (nrow(valid_daily) == 0) {
    warning("No valid days to export")
    return(invisible(NULL))
  }

  valid_dates <- as.Date(valid_daily$date)
  all_valid_epochs <- epoch_data[epoch_data$date %in% valid_dates, ]

  # Only count wear time epochs for intensity calculations (non-wear excluded)
  sedentary <- sum(all_valid_epochs$intensity == "sedentary" & all_valid_epochs$wear_time)
  light <- sum(all_valid_epochs$intensity == "light" & all_valid_epochs$wear_time)
  moderate <- sum(all_valid_epochs$intensity == "moderate" & all_valid_epochs$wear_time)
  vigorous <- sum(all_valid_epochs$intensity == "vigorous" & all_valid_epochs$wear_time)
  very_vigorous <- sum(all_valid_epochs$intensity == "very_vigorous" & all_valid_epochs$wear_time)
  total_mvpa <- moderate + vigorous + very_vigorous
  total_epochs <- nrow(all_valid_epochs)

  # Guard against division by zero
  if (total_epochs == 0) {
    warning("No valid epochs to export")
    return(invisible(NULL))
  }

  wear_valid_epochs <- all_valid_epochs[all_valid_epochs$wear_time, ]

  if (nrow(wear_valid_epochs) > 0) {
    vm_counts <- sqrt(wear_valid_epochs$axis1^2 + wear_valid_epochs$axis2^2 + wear_valid_epochs$axis3^2)
  } else {
    vm_counts <- numeric(0)
  }

  avg_mvpa_per_day <- if (nrow(valid_daily) > 0) total_mvpa / nrow(valid_daily) else 0

  # Use wear epochs for percentage calculations (not total epochs)
  n_wear_epochs <- nrow(wear_valid_epochs)

  summary_data <- data.frame(
    "Subject" = subject_id,
    "Filename" = filename,
    "Epoch" = epoch_sec,
    "Weight (lbs)" = weight_lbs,
    "Age" = age,
    "Gender" = gender,
    "Sedentary" = sedentary,
    "Light" = light,
    "Moderate" = moderate,
    "Vigorous" = vigorous,
    "Very Vigorous" = very_vigorous,
    "% in Sedentary" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * sedentary / n_wear_epochs) else "0.00%",
    "% in Light" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * light / n_wear_epochs) else "0.00%",
    "% in Moderate" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * moderate / n_wear_epochs) else "0.00%",
    "% in Vigorous" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * vigorous / n_wear_epochs) else "0.00%",
    "% in Very Vigorous" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * very_vigorous / n_wear_epochs) else "0.00%",
    "Total MVPA" = total_mvpa,
    "% in MVPA" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * total_mvpa / n_wear_epochs) else "0.00%",
    "Average MVPA Per day" = round(avg_mvpa_per_day, 1),
    "Axis 1 Counts" = if(n_wear_epochs > 0) sum(wear_valid_epochs$axis1) else 0,
    "Axis 2 Counts" = if(n_wear_epochs > 0) sum(wear_valid_epochs$axis2) else 0,
    "Axis 3 Counts" = if(n_wear_epochs > 0) sum(wear_valid_epochs$axis3) else 0,
    "Axis 1 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$axis1), 1) else 0,
    "Axis 2 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$axis2), 1) else 0,
    "Axis 3 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$axis3), 1) else 0,
    "Axis 1 Max Counts" = if(n_wear_epochs > 0) max(wear_valid_epochs$axis1) else 0,
    "Axis 2 Max Counts" = if(n_wear_epochs > 0) max(wear_valid_epochs$axis2) else 0,
    "Axis 3 Max Counts" = if(n_wear_epochs > 0) max(wear_valid_epochs$axis3) else 0,
    "Axis 1 CPM" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$axis1) * (60 / epoch_sec), 1) else 0,
    "Axis 2 CPM" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$axis2) * (60 / epoch_sec), 1) else 0,
    "Axis 3 CPM" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$axis3) * (60 / epoch_sec), 1) else 0,
    "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
    "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
    "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
    "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts) * (60 / epoch_sec), 1) else 0,
    "Steps Counts" = if(n_wear_epochs > 0) sum(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Average Counts" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Steps Max Counts" = if(n_wear_epochs > 0 && any(!is.na(wear_valid_epochs$steps))) max(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Per Minute" = if(n_wear_epochs > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE) * (60 / epoch_sec), 1) else 0,
    "Lux Average Counts" = 0,
    "Lux Max Counts" = 0,
    "Number of Epochs" = n_wear_epochs,
    "Time" = round(n_wear_epochs * (epoch_sec / 60), 1),
    "Calendar Days" = nrow(valid_daily),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  write.csv(summary_data, output_path, row.names = FALSE, na = "", quote = TRUE)

  invisible(summary_data)
}


#' Export ActiLife-Compatible Daily Detailed Report
#'
#' Exports a daily summary CSV file matching ActiLife's Desktop_DailyDetailed.csv format.
#' One row per participant per day (includes non-valid days with zeros).
#'
#' @param analysis_results An canhrActi_analysis object from canhrActi() function
#' @param output_path Character. Full path for output CSV file
#' @param subject_id Character. Subject identifier (default: extracts from filename)
#' @param weight_lbs Numeric. Weight in pounds (optional)
#' @param age Numeric. Age in years (optional)
#' @param gender Character. Gender ("M", "F", or "") (optional)
#'
#' @return Invisibly returns the data frame that was exported
#' @export
export_daily_detailed <- function(analysis_results,
                                   output_path,
                                   subject_id = NULL,
                                   weight_lbs = NULL,
                                   age = NULL,
                                   gender = NULL) {

  if (!inherits(analysis_results, "canhrActi_analysis")) {
    stop("analysis_results must be an canhrActi_analysis object from canhrActi()")
  }

  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    if (is.null(subject_id)) subject_id <- subj$subject_id
    if (is.null(weight_lbs)) weight_lbs <- subj$weight_lbs
    if (is.null(age)) age <- subj$age
    if (is.null(gender)) gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  daily <- analysis_results$daily_summary

  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60
  }

  filename <- basename(analysis_results$parameters$file_path)

  rows_list <- list()

  for (i in 1:nrow(daily)) {
    date_i <- as.Date(daily$date[i])
    day_epochs_all <- epoch_data[epoch_data$date == date_i, ]

    if (nrow(day_epochs_all) == 0) {
      rows_list[[i]] <- list(
        Subject = subject_id,
        Filename = filename,
        Epoch = epoch_sec,
        "Weight (lbs)" = weight_lbs,
        Age = age,
        Gender = gender,
        Date = format(date_i, "%m/%d/%Y"),
        "Day of Week" = weekdays(date_i),
        "Day of Week Num" = as.numeric(format(date_i, "%u")),
        Sedentary = 0,
        Light = 0,
        Moderate = 0,
        Vigorous = 0,
        "Very Vigorous" = 0,
        "% in Sedentary" = "0.00%",
        "% in Light" = "0.00%",
        "% in Moderate" = "0.00%",
        "% in Vigorous" = "0.00%",
        "% in Very Vigorous" = "0.00%",
        "Total MVPA" = 0,
        "% in MVPA" = "0.00%",
        "Average MVPA Per Hour" = 0,
        "Axis 1 Counts" = 0,
        "Axis 2 Counts" = 0,
        "Axis 3 Counts" = 0,
        "Axis 1 Average Counts" = 0,
        "Axis 2 Average Counts" = 0,
        "Axis 3 Average Counts" = 0,
        "Axis 1 Max Counts" = 0,
        "Axis 2 Max Counts" = 0,
        "Axis 3 Max Counts" = 0,
        "Axis 1 CPM" = 0,
        "Axis 2 CPM" = 0,
        "Axis 3 CPM" = 0,
        "Vector Magnitude Counts" = 0,
        "Vector Magnitude Average Counts" = 0,
        "Vector Magnitude Max Counts" = 0,
        "Vector Magnitude CPM" = 0,
        "Steps Counts" = 0,
        "Steps Average Counts" = 0,
        "Steps Max Counts" = 0,
        "Steps Per Minute" = 0,
        "Lux Average Counts" = 0,
        "Lux Max Counts" = 0,
        "Number of Epochs" = 0,
        Time = 0,
        "Calendar Days" = 0
      )
      next
    }

    # Only count wear time epochs for intensity calculations (non-wear excluded)
    sedentary <- sum(day_epochs_all$intensity == "sedentary" & day_epochs_all$wear_time)
    light <- sum(day_epochs_all$intensity == "light" & day_epochs_all$wear_time)
    moderate <- sum(day_epochs_all$intensity == "moderate" & day_epochs_all$wear_time)
    vigorous <- sum(day_epochs_all$intensity == "vigorous" & day_epochs_all$wear_time)
    very_vigorous <- sum(day_epochs_all$intensity == "very_vigorous" & day_epochs_all$wear_time)
    total_mvpa <- moderate + vigorous + very_vigorous

    wear_epochs <- day_epochs_all[day_epochs_all$wear_time, ]
    n_wear_epochs <- nrow(wear_epochs)

    if (nrow(wear_epochs) > 0) {
      vm_counts <- sqrt(wear_epochs$axis1^2 + wear_epochs$axis2^2 + wear_epochs$axis3^2)
    } else {
      vm_counts <- numeric(0)
    }

    rows_list[[i]] <- list(
      Subject = subject_id,
      Filename = filename,
      Epoch = epoch_sec,
      "Weight (lbs)" = weight_lbs,
      Age = age,
      Gender = gender,
      Date = format(as.Date(date_i), "%m/%d/%Y"),
      "Day of Week" = weekdays(as.Date(date_i)),
      "Day of Week Num" = as.numeric(format(as.Date(date_i), "%u")),
      Sedentary = sedentary,
      Light = light,
      Moderate = moderate,
      Vigorous = vigorous,
      "Very Vigorous" = very_vigorous,
      "% in Sedentary" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * sedentary / n_wear_epochs) else "0.00%",
      "% in Light" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * light / n_wear_epochs) else "0.00%",
      "% in Moderate" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * moderate / n_wear_epochs) else "0.00%",
      "% in Vigorous" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * vigorous / n_wear_epochs) else "0.00%",
      "% in Very Vigorous" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * very_vigorous / n_wear_epochs) else "0.00%",
      "Total MVPA" = total_mvpa,
      "% in MVPA" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * total_mvpa / n_wear_epochs) else "0.00%",
      "Average MVPA Per Hour" = if(n_wear_epochs > 0) round(total_mvpa / (n_wear_epochs / 60), 1) else 0,
      "Axis 1 Counts" = if(n_wear_epochs > 0) sum(wear_epochs$axis1) else 0,
      "Axis 2 Counts" = if(n_wear_epochs > 0) sum(wear_epochs$axis2) else 0,
      "Axis 3 Counts" = if(n_wear_epochs > 0) sum(wear_epochs$axis3) else 0,
      "Axis 1 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_epochs$axis1), 1) else 0,
      "Axis 2 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_epochs$axis2), 1) else 0,
      "Axis 3 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_epochs$axis3), 1) else 0,
      "Axis 1 Max Counts" = if(n_wear_epochs > 0) max(wear_epochs$axis1) else 0,
      "Axis 2 Max Counts" = if(n_wear_epochs > 0) max(wear_epochs$axis2) else 0,
      "Axis 3 Max Counts" = if(n_wear_epochs > 0) max(wear_epochs$axis3) else 0,
      "Axis 1 CPM" = if(n_wear_epochs > 0) round(mean(wear_epochs$axis1) * (60 / epoch_sec), 1) else 0,
      "Axis 2 CPM" = if(n_wear_epochs > 0) round(mean(wear_epochs$axis2) * (60 / epoch_sec), 1) else 0,
      "Axis 3 CPM" = if(n_wear_epochs > 0) round(mean(wear_epochs$axis3) * (60 / epoch_sec), 1) else 0,
      "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
      "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
      "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
      "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts) * (60 / epoch_sec), 1) else 0,
      "Steps Counts" = if(n_wear_epochs > 0) sum(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Average Counts" = if(n_wear_epochs > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Steps Max Counts" = if(n_wear_epochs > 0) max(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Per Minute" = if(n_wear_epochs > 0) round(mean(wear_epochs$steps, na.rm = TRUE) * (60 / epoch_sec), 1) else 0,
      "Lux Average Counts" = 0,
      "Lux Max Counts" = 0,
      "Number of Epochs" = n_wear_epochs,
      Time = round(n_wear_epochs * (epoch_sec / 60), 1),
      "Calendar Days" = 1
    )
  }

  daily_data <- do.call(rbind.data.frame, c(rows_list, stringsAsFactors = FALSE, make.row.names = FALSE))

  write.csv(daily_data, output_path, row.names = FALSE, na = "", quote = TRUE)

  invisible(daily_data)
}


#' Export ActiLife-Compatible Hourly Detailed Report
#'
#' Exports an hourly summary CSV file matching ActiLife's Desktop_HourlyDetailed.csv format.
#' One row per hour of the day.
#'
#' @param analysis_results An canhrActi_analysis object from canhrActi() function
#' @param output_path Character. Full path for output CSV file
#' @param subject_id Character. Subject identifier (default: extracts from filename)
#' @param weight_lbs Numeric. Weight in pounds (optional)
#' @param age Numeric. Age in years (optional)
#' @param gender Character. Gender ("M", "F", or "") (optional)
#'
#' @return Invisibly returns the data frame that was exported
#' @export
export_hourly_detailed <- function(analysis_results,
                                    output_path,
                                    subject_id = NULL,
                                    weight_lbs = NULL,
                                    age = NULL,
                                    gender = NULL) {

  if (!inherits(analysis_results, "canhrActi_analysis")) {
    stop("analysis_results must be an canhrActi_analysis object from canhrActi()")
  }

  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    if (is.null(subject_id)) subject_id <- subj$subject_id
    if (is.null(weight_lbs)) weight_lbs <- subj$weight_lbs
    if (is.null(age)) age <- subj$age
    if (is.null(gender)) gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  daily <- analysis_results$daily_summary

  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60
  }

  filename <- basename(analysis_results$parameters$file_path)

  if ("is_valid_day" %in% names(daily)) {
    valid_dates <- as.Date(daily$date[daily$is_valid_day == TRUE])
  } else if ("is.valid" %in% names(daily)) {
    valid_dates <- as.Date(daily$date[daily$is.valid == TRUE])
  } else {
    valid_dates <- as.Date(daily$date)
  }

  epoch_data$hour <- format(epoch_data$timestamp, "%I:00 %p")
  epoch_data$hour_24 <- as.numeric(format(epoch_data$timestamp, "%H"))

  dates <- unique(epoch_data$date)
  hourly_rows <- list()

  for (date_i in dates) {
    day_data <- epoch_data[epoch_data$date == date_i, ]
    hours_present <- unique(day_data$hour_24)
    is_valid_date <- as.Date(date_i) %in% valid_dates

    for (hour_i in hours_present) {
      all_hour_data <- day_data[day_data$hour_24 == hour_i, ]

      hour_label <- as.character(all_hour_data$hour[1])

      if (!is_valid_date) {
        hourly_rows[[length(hourly_rows) + 1]] <- list(
          Subject = subject_id,
          Filename = filename,
          Epoch = epoch_sec,
          "Weight (lbs)" = weight_lbs,
          Age = age,
          Gender = gender,
          Date = format(as.Date(date_i), "%m/%d/%Y"),
          Hour = hour_label,
          "Day of Week" = weekdays(as.Date(date_i)),
          "Day of Week Num" = as.numeric(format(as.Date(date_i), "%u")),
          Sedentary = 0,
          Light = 0,
          Moderate = 0,
          Vigorous = 0,
          "Very Vigorous" = 0,
          "% in Sedentary" = "0.00%",
          "% in Light" = "0.00%",
          "% in Moderate" = "0.00%",
          "% in Vigorous" = "0.00%",
          "% in Very Vigorous" = "0.00%",
          "Total MVPA" = 0,
          "% in MVPA" = "0.00%",
          "Axis 1 Counts" = 0,
          "Axis 2 Counts" = 0,
          "Axis 3 Counts" = 0,
          "Axis 1 Average Counts" = 0,
          "Axis 2 Average Counts" = 0,
          "Axis 3 Average Counts" = 0,
          "Axis 1 Max Counts" = 0,
          "Axis 2 Max Counts" = 0,
          "Axis 3 Max Counts" = 0,
          "Axis 1 CPM" = 0,
          "Axis 2 CPM" = 0,
          "Axis 3 CPM" = 0,
          "Vector Magnitude Counts" = 0,
          "Vector Magnitude Average Counts" = 0,
          "Vector Magnitude Max Counts" = 0,
          "Vector Magnitude CPM" = 0,
          "Steps Counts" = 0,
          "Steps Average Counts" = 0,
          "Steps Max Counts" = 0,
          "Steps Per Minute" = 0,
          "Lux Average Counts" = 0,
          "Lux Max Counts" = 0,
          "Number of Epochs" = 0,
          Time = 0,
          "Calendar Days" = 1
        )
        next
      }

      # Only count wear time epochs for intensity calculations (non-wear excluded)
      sedentary <- sum(all_hour_data$intensity == "sedentary" & all_hour_data$wear_time)
      light <- sum(all_hour_data$intensity == "light" & all_hour_data$wear_time)
      moderate <- sum(all_hour_data$intensity == "moderate" & all_hour_data$wear_time)
      vigorous <- sum(all_hour_data$intensity == "vigorous" & all_hour_data$wear_time)
      very_vigorous <- sum(all_hour_data$intensity == "very_vigorous" & all_hour_data$wear_time)
      total_mvpa <- moderate + vigorous + very_vigorous

      wear_hour_data <- all_hour_data[all_hour_data$wear_time, ]
      n_wear_epochs <- nrow(wear_hour_data)

      if (nrow(wear_hour_data) > 0) {
        vm_counts <- sqrt(wear_hour_data$axis1^2 + wear_hour_data$axis2^2 + wear_hour_data$axis3^2)
      } else {
        vm_counts <- numeric(0)
      }

      hourly_rows[[length(hourly_rows) + 1]] <- list(
        Subject = subject_id,
        Filename = filename,
        Epoch = epoch_sec,
        "Weight (lbs)" = weight_lbs,
        Age = age,
        Gender = gender,
        Date = format(as.Date(date_i), "%m/%d/%Y"),
        Hour = hour_label,
        "Day of Week" = weekdays(as.Date(date_i)),
        "Day of Week Num" = as.numeric(format(as.Date(date_i), "%u")),
        Sedentary = sedentary,
        Light = light,
        Moderate = moderate,
        Vigorous = vigorous,
        "Very Vigorous" = very_vigorous,
        "% in Sedentary" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * sedentary / n_wear_epochs) else "0.00%",
        "% in Light" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * light / n_wear_epochs) else "0.00%",
        "% in Moderate" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * moderate / n_wear_epochs) else "0.00%",
        "% in Vigorous" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * vigorous / n_wear_epochs) else "0.00%",
        "% in Very Vigorous" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * very_vigorous / n_wear_epochs) else "0.00%",
        "Total MVPA" = total_mvpa,
        "% in MVPA" = if(n_wear_epochs > 0) sprintf("%.2f%%", 100 * total_mvpa / n_wear_epochs) else "0.00%",
        "Axis 1 Counts" = if(n_wear_epochs > 0) sum(wear_hour_data$axis1) else 0,
        "Axis 2 Counts" = if(n_wear_epochs > 0) sum(wear_hour_data$axis2) else 0,
        "Axis 3 Counts" = if(n_wear_epochs > 0) sum(wear_hour_data$axis3) else 0,
        "Axis 1 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_hour_data$axis1), 1) else 0,
        "Axis 2 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_hour_data$axis2), 1) else 0,
        "Axis 3 Average Counts" = if(n_wear_epochs > 0) round(mean(wear_hour_data$axis3), 1) else 0,
        "Axis 1 Max Counts" = if(n_wear_epochs > 0) max(wear_hour_data$axis1) else 0,
        "Axis 2 Max Counts" = if(n_wear_epochs > 0) max(wear_hour_data$axis2) else 0,
        "Axis 3 Max Counts" = if(n_wear_epochs > 0) max(wear_hour_data$axis3) else 0,
        "Axis 1 CPM" = if(n_wear_epochs > 0) round(mean(wear_hour_data$axis1) * (60 / epoch_sec), 1) else 0,
        "Axis 2 CPM" = if(n_wear_epochs > 0) round(mean(wear_hour_data$axis2) * (60 / epoch_sec), 1) else 0,
        "Axis 3 CPM" = if(n_wear_epochs > 0) round(mean(wear_hour_data$axis3) * (60 / epoch_sec), 1) else 0,
        "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
        "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
        "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
        "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts) * (60 / epoch_sec), 1) else 0,
        "Steps Counts" = if(n_wear_epochs > 0) sum(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Average Counts" = if(n_wear_epochs > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Steps Max Counts" = if(n_wear_epochs > 0) max(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Per Minute" = if(n_wear_epochs > 0) round(mean(wear_hour_data$steps, na.rm = TRUE) * (60 / epoch_sec), 1) else 0,
        "Lux Average Counts" = 0,
        "Lux Max Counts" = 0,
        "Number of Epochs" = n_wear_epochs,
        Time = round(n_wear_epochs * (epoch_sec / 60), 1),
        "Calendar Days" = 1
      )
    }
  }

  hourly_data <- do.call(rbind.data.frame, c(hourly_rows, stringsAsFactors = FALSE, make.row.names = FALSE))

  write.csv(hourly_data, output_path, row.names = FALSE, na = "", quote = TRUE)

  invisible(hourly_data)
}


#' Export All ActiLife-Compatible Reports
#'
#' Convenience function to export all three ActiLife report formats at once.
#' Works with both single file analysis results (canhrActi_analysis) and batch
#' analysis results (canhrActi_batch).
#'
#' @param analysis_results An canhrActi_analysis object from canhrActi() function OR
#'   an canhrActi_batch object from batch analysis
#' @param output_dir Character. Directory where output files will be saved
#' @param prefix Character. Prefix for output filenames (default: "canhrActi")
#' @param subject_id Character. Subject identifier (default: extracts from filename)
#' @param weight_lbs Numeric. Weight in pounds (optional)
#' @param age Numeric. Age in years (optional)
#' @param gender Character. Gender ("M", "F", or "") (optional)
#'
#' @return List with paths to the three exported files
#' @export
#'
#' @examples
#' \dontrun{
#' # Single file analysis
#' results <- canhrActi("myfile.agd")
#' export_canhrActi(results, output_dir = "C:/MyReports")
#'
#' # Batch analysis - exports all participants
#' results <- canhrActi("C:/Data Folder")
#' export_canhrActi(results, output_dir = "C:/MyReports")
#'
#' # Single file with custom metadata
#' results <- canhrActi("myfile.agd")
#' export_canhrActi(results,
#'                  output_dir = "C:/MyReports",
#'                  subject_id = "PARTICIPANT001",
#'                  weight_lbs = 150,
#'                  age = 35,
#'                  gender = "F")
#' }
export_canhrActi <- function(analysis_results,
                             output_dir,
                             prefix = "canhrActi",
                             subject_id = NULL,
                             weight_lbs = NULL,
                             age = NULL,
                             gender = NULL) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (inherits(analysis_results, "canhrActi_batch")) {
    export_actilife_batch(analysis_results, output_dir, prefix)
    return(invisible(list(
      summary = file.path(output_dir, paste0(prefix, "_Summary.csv")),
      daily_detailed = file.path(output_dir, paste0(prefix, "_DailyDetailed.csv")),
      hourly_detailed = file.path(output_dir, paste0(prefix, "_HourlyDetailed.csv"))
    )))
  }

  summary_path <- file.path(output_dir, paste0(prefix, "_Summary.csv"))
  daily_path <- file.path(output_dir, paste0(prefix, "_DailyDetailed.csv"))
  hourly_path <- file.path(output_dir, paste0(prefix, "_HourlyDetailed.csv"))

  export_summary(analysis_results, summary_path, subject_id, weight_lbs, age, gender)
  export_daily_detailed(analysis_results, daily_path, subject_id, weight_lbs, age, gender)
  export_hourly_detailed(analysis_results, hourly_path, subject_id, weight_lbs, age, gender)

  message("Exported ActiLife reports to: ", output_dir)

  invisible(list(
    summary = summary_path,
    daily_detailed = daily_path,
    hourly_detailed = hourly_path
  ))
}


#' Export ActiLife Reports for Batch Analysis
#'
#' Internal function to export ActiLife-compatible reports for batch analysis results.
#' Combines all participants into single Summary, DailyDetailed, and HourlyDetailed files.
#'
#' @param batch_results An canhrActi_batch object
#' @param output_dir Character. Output directory path
#' @param prefix Character. Prefix for output filenames
#'
#' @return Invisibly returns paths to exported files
#' @keywords internal
export_actilife_batch <- function(batch_results, output_dir, prefix = "canhrActi") {

  all_summary <- list()
  all_daily <- list()
  all_hourly <- list()

  for (participant_name in names(batch_results$participants)) {
    participant_result <- batch_results$participants[[participant_name]]

    summary_data <- tryCatch({
      temp_summary <- export_summary_internal(participant_result)
      temp_summary
    }, error = function(e) {
      warning("Failed to export summary for ", participant_name, ": ", e$message)
      NULL
    })

    daily_data <- tryCatch({
      export_daily_detailed_internal(participant_result)
    }, error = function(e) {
      warning("Failed to export daily detailed for ", participant_name, ": ", e$message)
      NULL
    })

    hourly_data <- tryCatch({
      export_hourly_detailed_internal(participant_result)
    }, error = function(e) {
      warning("Failed to export hourly detailed for ", participant_name, ": ", e$message)
      NULL
    })

    if (!is.null(summary_data)) all_summary[[participant_name]] <- summary_data
    if (!is.null(daily_data)) all_daily[[participant_name]] <- daily_data
    if (!is.null(hourly_data)) all_hourly[[participant_name]] <- hourly_data
  }

  combined_summary <- do.call(rbind, all_summary)
  combined_daily <- do.call(rbind, all_daily)
  combined_hourly <- do.call(rbind, all_hourly)

  summary_path <- file.path(output_dir, paste0(prefix, "_Summary.csv"))
  daily_path <- file.path(output_dir, paste0(prefix, "_DailyDetailed.csv"))
  hourly_path <- file.path(output_dir, paste0(prefix, "_HourlyDetailed.csv"))

  write.csv(combined_summary, summary_path, row.names = FALSE, na = "", quote = TRUE)
  write.csv(combined_daily, daily_path, row.names = FALSE, na = "", quote = TRUE)
  write.csv(combined_hourly, hourly_path, row.names = FALSE, na = "", quote = TRUE)

  message("Exported ActiLife batch reports (", nrow(combined_summary), " participants)")

  invisible(list(
    summary = summary_path,
    daily_detailed = daily_path,
    hourly_detailed = hourly_path
  ))
}


#' Internal Summary Export (Returns Data Frame)
#' @keywords internal
export_summary_internal <- function(analysis_results) {
  subject_id <- NULL
  weight_lbs <- NULL
  age <- NULL
  gender <- NULL

  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    subject_id <- subj$subject_id
    weight_lbs <- subj$weight_lbs
    age <- subj$age
    gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  daily <- analysis_results$daily_summary

  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60  # default
  }

  filename <- basename(analysis_results$parameters$file_path)

  if ("is_valid_day" %in% names(daily)) {
    valid_daily <- daily[daily$is_valid_day == TRUE, ]
  } else if ("is.valid" %in% names(daily)) {
    valid_daily <- daily[daily$is.valid == TRUE, ]
  } else {
    stop("Could not find validity column (is_valid_day or is.valid) in daily_summary")
  }

  if (nrow(valid_daily) == 0) {
    warning("No valid days to export")
    return(NULL)
  }

  valid_dates <- as.Date(valid_daily$date)
  all_valid_epochs <- epoch_data[epoch_data$date %in% valid_dates, ]

  # Only count wear time epochs for intensity calculations (non-wear excluded)
  sedentary <- sum(all_valid_epochs$intensity == "sedentary" & all_valid_epochs$wear_time)
  light <- sum(all_valid_epochs$intensity == "light" & all_valid_epochs$wear_time)
  moderate <- sum(all_valid_epochs$intensity == "moderate" & all_valid_epochs$wear_time)
  vigorous <- sum(all_valid_epochs$intensity == "vigorous" & all_valid_epochs$wear_time)
  very_vigorous <- sum(all_valid_epochs$intensity == "very_vigorous" & all_valid_epochs$wear_time)
  total_mvpa <- moderate + vigorous + very_vigorous
  total_epochs <- nrow(all_valid_epochs)

  # Guard against division by zero
  if (total_epochs == 0) {
    warning("No valid epochs to export")
    return(invisible(NULL))
  }

  wear_valid_epochs <- all_valid_epochs[all_valid_epochs$wear_time, ]

  if (nrow(wear_valid_epochs) > 0) {
    vm_counts <- sqrt(wear_valid_epochs$axis1^2 + wear_valid_epochs$axis2^2 + wear_valid_epochs$axis3^2)
  } else {
    vm_counts <- numeric(0)
  }

  avg_mvpa_per_day <- if (nrow(valid_daily) > 0) total_mvpa / nrow(valid_daily) else 0

  summary_data <- data.frame(
    "Subject" = subject_id,
    "Filename" = filename,
    "Epoch" = epoch_sec,
    "Weight (lbs)" = weight_lbs,
    "Age" = age,
    "Gender" = gender,
    "Sedentary" = sedentary,
    "Light" = light,
    "Moderate" = moderate,
    "Vigorous" = vigorous,
    "Very Vigorous" = very_vigorous,
    "% in Sedentary" = sprintf("%.2f%%", 100 * sedentary / total_epochs),
    "% in Light" = sprintf("%.2f%%", 100 * light / total_epochs),
    "% in Moderate" = sprintf("%.2f%%", 100 * moderate / total_epochs),
    "% in Vigorous" = sprintf("%.2f%%", 100 * vigorous / total_epochs),
    "% in Very Vigorous" = sprintf("%.2f%%", 100 * very_vigorous / total_epochs),
    "Total MVPA" = total_mvpa,
    "% in MVPA" = sprintf("%.2f%%", 100 * total_mvpa / total_epochs),
    "Average MVPA Per day" = round(avg_mvpa_per_day, 1),
    "Axis 1 Counts" = if(nrow(wear_valid_epochs) > 0) sum(wear_valid_epochs$axis1) else 0,
    "Axis 2 Counts" = if(nrow(wear_valid_epochs) > 0) sum(wear_valid_epochs$axis2) else 0,
    "Axis 3 Counts" = if(nrow(wear_valid_epochs) > 0) sum(wear_valid_epochs$axis3) else 0,
    "Axis 1 Average Counts" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis1), 1) else 0,
    "Axis 2 Average Counts" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis2), 1) else 0,
    "Axis 3 Average Counts" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis3), 1) else 0,
    "Axis 1 Max Counts" = if(nrow(wear_valid_epochs) > 0) max(wear_valid_epochs$axis1) else 0,
    "Axis 2 Max Counts" = if(nrow(wear_valid_epochs) > 0) max(wear_valid_epochs$axis2) else 0,
    "Axis 3 Max Counts" = if(nrow(wear_valid_epochs) > 0) max(wear_valid_epochs$axis3) else 0,
    "Axis 1 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis1) * (60 / epoch_sec), 1) else 0,
    "Axis 2 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis2) * (60 / epoch_sec), 1) else 0,
    "Axis 3 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis3) * (60 / epoch_sec), 1) else 0,
    "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
    "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
    "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
    "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts) * (60 / epoch_sec), 1) else 0,
    "Steps Counts" = if(nrow(wear_valid_epochs) > 0) sum(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Average Counts" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Steps Max Counts" = if(nrow(wear_valid_epochs) > 0 && any(!is.na(wear_valid_epochs$steps))) max(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Per Minute" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Lux Average Counts" = 0,
    "Lux Max Counts" = 0,
    "Number of Epochs" = total_epochs,
    "Time" = round(total_epochs * (epoch_sec / 60), 1),
    "Calendar Days" = nrow(valid_daily),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  return(summary_data)
}


#' Internal Daily Detailed Export (Returns Data Frame)
#' @keywords internal
export_daily_detailed_internal <- function(analysis_results) {
  subject_id <- NULL
  weight_lbs <- NULL
  age <- NULL
  gender <- NULL

  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    subject_id <- subj$subject_id
    weight_lbs <- subj$weight_lbs
    age <- subj$age
    gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  daily <- analysis_results$daily_summary

  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60
  }

  filename <- basename(analysis_results$parameters$file_path)

  rows_list <- list()

  for (i in 1:nrow(daily)) {
    date_i <- as.Date(daily$date[i])
    day_epochs_all <- epoch_data[epoch_data$date == date_i, ]

    if (nrow(day_epochs_all) == 0) {
      rows_list[[i]] <- list(
        Subject = subject_id,
        Filename = filename,
        Epoch = epoch_sec,
        "Weight (lbs)" = weight_lbs,
        Age = age,
        Gender = gender,
        Date = format(date_i, "%m/%d/%Y"),
        "Day of Week" = weekdays(date_i),
        "Day of Week Num" = as.numeric(format(date_i, "%u")),
        Sedentary = 0,
        Light = 0,
        Moderate = 0,
        Vigorous = 0,
        "Very Vigorous" = 0,
        "% in Sedentary" = "0.00%",
        "% in Light" = "0.00%",
        "% in Moderate" = "0.00%",
        "% in Vigorous" = "0.00%",
        "% in Very Vigorous" = "0.00%",
        "Total MVPA" = 0,
        "% in MVPA" = "0.00%",
        "Average MVPA Per Hour" = 0,
        "Axis 1 Counts" = 0,
        "Axis 2 Counts" = 0,
        "Axis 3 Counts" = 0,
        "Axis 1 Average Counts" = 0,
        "Axis 2 Average Counts" = 0,
        "Axis 3 Average Counts" = 0,
        "Axis 1 Max Counts" = 0,
        "Axis 2 Max Counts" = 0,
        "Axis 3 Max Counts" = 0,
        "Axis 1 CPM" = 0,
        "Axis 2 CPM" = 0,
        "Axis 3 CPM" = 0,
        "Vector Magnitude Counts" = 0,
        "Vector Magnitude Average Counts" = 0,
        "Vector Magnitude Max Counts" = 0,
        "Vector Magnitude CPM" = 0,
        "Steps Counts" = 0,
        "Steps Average Counts" = 0,
        "Steps Max Counts" = 0,
        "Steps Per Minute" = 0,
        "Lux Average Counts" = 0,
        "Lux Max Counts" = 0,
        "Number of Epochs" = 0,
        Time = 0,
        "Calendar Days" = 0
      )
      next
    }

    #  Changed | (OR) to & (AND) - non-wear should NOT be counted as sedentary
    # Only count sedentary epochs that are ALSO wear time (matches summary export logic)
    sedentary <- sum(day_epochs_all$intensity == "sedentary" & day_epochs_all$wear_time)
    light <- sum(day_epochs_all$intensity == "light" & day_epochs_all$wear_time)
    moderate <- sum(day_epochs_all$intensity == "moderate" & day_epochs_all$wear_time)
    vigorous <- sum(day_epochs_all$intensity == "vigorous" & day_epochs_all$wear_time)
    very_vigorous <- sum(day_epochs_all$intensity == "very_vigorous" & day_epochs_all$wear_time)
    total_mvpa <- moderate + vigorous + very_vigorous
    total_epochs <- nrow(day_epochs_all)

    wear_epochs <- day_epochs_all[day_epochs_all$wear_time, ]

    if (nrow(wear_epochs) > 0) {
      vm_counts <- sqrt(wear_epochs$axis1^2 + wear_epochs$axis2^2 + wear_epochs$axis3^2)
    } else {
      vm_counts <- numeric(0)
    }

    rows_list[[i]] <- list(
      Subject = subject_id,
      Filename = filename,
      Epoch = epoch_sec,
      "Weight (lbs)" = weight_lbs,
      Age = age,
      Gender = gender,
      Date = format(as.Date(date_i), "%m/%d/%Y"),
      "Day of Week" = weekdays(as.Date(date_i)),
      "Day of Week Num" = as.numeric(format(as.Date(date_i), "%u")),
      Sedentary = sedentary,
      Light = light,
      Moderate = moderate,
      Vigorous = vigorous,
      "Very Vigorous" = very_vigorous,
      "% in Sedentary" = sprintf("%.2f%%", 100 * sedentary / total_epochs),
      "% in Light" = sprintf("%.2f%%", 100 * light / total_epochs),
      "% in Moderate" = sprintf("%.2f%%", 100 * moderate / total_epochs),
      "% in Vigorous" = sprintf("%.2f%%", 100 * vigorous / total_epochs),
      "% in Very Vigorous" = sprintf("%.2f%%", 100 * very_vigorous / total_epochs),
      "Total MVPA" = total_mvpa,
      "% in MVPA" = sprintf("%.2f%%", 100 * total_mvpa / total_epochs),
      "Average MVPA Per Hour" = round(total_mvpa / (total_epochs / 60), 1),
      "Axis 1 Counts" = if(nrow(wear_epochs) > 0) sum(wear_epochs$axis1) else 0,
      "Axis 2 Counts" = if(nrow(wear_epochs) > 0) sum(wear_epochs$axis2) else 0,
      "Axis 3 Counts" = if(nrow(wear_epochs) > 0) sum(wear_epochs$axis3) else 0,
      "Axis 1 Average Counts" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis1), 1) else 0,
      "Axis 2 Average Counts" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis2), 1) else 0,
      "Axis 3 Average Counts" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis3), 1) else 0,
      "Axis 1 Max Counts" = if(nrow(wear_epochs) > 0) max(wear_epochs$axis1) else 0,
      "Axis 2 Max Counts" = if(nrow(wear_epochs) > 0) max(wear_epochs$axis2) else 0,
      "Axis 3 Max Counts" = if(nrow(wear_epochs) > 0) max(wear_epochs$axis3) else 0,
      "Axis 1 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis1) * (60 / epoch_sec), 1) else 0,
      "Axis 2 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis2) * (60 / epoch_sec), 1) else 0,
      "Axis 3 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis3) * (60 / epoch_sec), 1) else 0,
      "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
      "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
      "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
      "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts) * (60 / epoch_sec), 1) else 0,
      "Steps Counts" = if(nrow(wear_epochs) > 0) sum(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Average Counts" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Steps Max Counts" = if(nrow(wear_epochs) > 0) max(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Per Minute" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Lux Average Counts" = 0,
      "Lux Max Counts" = 0,
      "Number of Epochs" = total_epochs,
      Time = round(total_epochs * (epoch_sec / 60), 1),
      "Calendar Days" = 1
    )
  }

  daily_data <- do.call(rbind.data.frame, c(rows_list, stringsAsFactors = FALSE, make.row.names = FALSE))
  return(daily_data)
}


#' Internal Hourly Detailed Export (Returns Data Frame)
#' @keywords internal
export_hourly_detailed_internal <- function(analysis_results) {
  subject_id <- NULL
  weight_lbs <- NULL
  age <- NULL
  gender <- NULL

  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    subject_id <- subj$subject_id
    weight_lbs <- subj$weight_lbs
    age <- subj$age
    gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  daily <- analysis_results$daily_summary

  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60
  }

  filename <- basename(analysis_results$parameters$file_path)

  if ("is_valid_day" %in% names(daily)) {
    valid_dates <- as.Date(daily$date[daily$is_valid_day == TRUE])
  } else if ("is.valid" %in% names(daily)) {
    valid_dates <- as.Date(daily$date[daily$is.valid == TRUE])
  } else {
    valid_dates <- as.Date(daily$date)
  }

  epoch_data$hour <- format(epoch_data$timestamp, "%I:00 %p")
  epoch_data$hour_24 <- as.numeric(format(epoch_data$timestamp, "%H"))

  dates <- unique(epoch_data$date)
  hourly_rows <- list()

  for (date_i in dates) {
    day_data <- epoch_data[epoch_data$date == date_i, ]
    hours_present <- unique(day_data$hour_24)
    is_valid_date <- as.Date(date_i) %in% valid_dates

    for (hour_i in hours_present) {
      all_hour_data <- day_data[day_data$hour_24 == hour_i, ]

      hour_label <- as.character(all_hour_data$hour[1])

      if (!is_valid_date) {
        hourly_rows[[length(hourly_rows) + 1]] <- list(
          Subject = subject_id,
          Filename = filename,
          Epoch = epoch_sec,
          "Weight (lbs)" = weight_lbs,
          Age = age,
          Gender = gender,
          Date = format(as.Date(date_i), "%m/%d/%Y"),
          Hour = hour_label,
          "Day of Week" = weekdays(as.Date(date_i)),
          "Day of Week Num" = as.numeric(format(as.Date(date_i), "%u")),
          Sedentary = 0,
          Light = 0,
          Moderate = 0,
          Vigorous = 0,
          "Very Vigorous" = 0,
          "% in Sedentary" = "0.00%",
          "% in Light" = "0.00%",
          "% in Moderate" = "0.00%",
          "% in Vigorous" = "0.00%",
          "% in Very Vigorous" = "0.00%",
          "Total MVPA" = 0,
          "% in MVPA" = "0.00%",
          "Axis 1 Counts" = 0,
          "Axis 2 Counts" = 0,
          "Axis 3 Counts" = 0,
          "Axis 1 Average Counts" = 0,
          "Axis 2 Average Counts" = 0,
          "Axis 3 Average Counts" = 0,
          "Axis 1 Max Counts" = 0,
          "Axis 2 Max Counts" = 0,
          "Axis 3 Max Counts" = 0,
          "Axis 1 CPM" = 0,
          "Axis 2 CPM" = 0,
          "Axis 3 CPM" = 0,
          "Vector Magnitude Counts" = 0,
          "Vector Magnitude Average Counts" = 0,
          "Vector Magnitude Max Counts" = 0,
          "Vector Magnitude CPM" = 0,
          "Steps Counts" = 0,
          "Steps Average Counts" = 0,
          "Steps Max Counts" = 0,
          "Steps Per Minute" = 0,
          "Lux Average Counts" = 0,
          "Lux Max Counts" = 0,
          "Number of Epochs" = 0,
          Time = 0,
          "Calendar Days" = 1
        )
        next
      }

      #  Changed | (OR) to & (AND) - non-wear should NOT be counted as sedentary
      # Only count sedentary epochs that are ALSO wear time (matches summary export logic)
      sedentary <- sum(all_hour_data$intensity == "sedentary" & all_hour_data$wear_time)
      light <- sum(all_hour_data$intensity == "light" & all_hour_data$wear_time)
      moderate <- sum(all_hour_data$intensity == "moderate" & all_hour_data$wear_time)
      vigorous <- sum(all_hour_data$intensity == "vigorous" & all_hour_data$wear_time)
      very_vigorous <- sum(all_hour_data$intensity == "very_vigorous" & all_hour_data$wear_time)
      total_mvpa <- moderate + vigorous + very_vigorous
      total_epochs <- nrow(all_hour_data)

      wear_hour_data <- all_hour_data[all_hour_data$wear_time, ]

      if (nrow(wear_hour_data) > 0) {
        vm_counts <- sqrt(wear_hour_data$axis1^2 + wear_hour_data$axis2^2 + wear_hour_data$axis3^2)
      } else {
        vm_counts <- numeric(0)
      }

      hourly_rows[[length(hourly_rows) + 1]] <- list(
        Subject = subject_id,
        Filename = filename,
        Epoch = epoch_sec,
        "Weight (lbs)" = weight_lbs,
        Age = age,
        Gender = gender,
        Date = format(as.Date(date_i), "%m/%d/%Y"),
        Hour = hour_label,
        "Day of Week" = weekdays(as.Date(date_i)),
        "Day of Week Num" = as.numeric(format(as.Date(date_i), "%u")),
        Sedentary = sedentary,
        Light = light,
        Moderate = moderate,
        Vigorous = vigorous,
        "Very Vigorous" = very_vigorous,
        "% in Sedentary" = sprintf("%.2f%%", 100 * sedentary / total_epochs),
        "% in Light" = sprintf("%.2f%%", 100 * light / total_epochs),
        "% in Moderate" = sprintf("%.2f%%", 100 * moderate / total_epochs),
        "% in Vigorous" = sprintf("%.2f%%", 100 * vigorous / total_epochs),
        "% in Very Vigorous" = sprintf("%.2f%%", 100 * very_vigorous / total_epochs),
        "Total MVPA" = total_mvpa,
        "% in MVPA" = sprintf("%.2f%%", 100 * total_mvpa / total_epochs),
        "Axis 1 Counts" = if(nrow(wear_hour_data) > 0) sum(wear_hour_data$axis1) else 0,
        "Axis 2 Counts" = if(nrow(wear_hour_data) > 0) sum(wear_hour_data$axis2) else 0,
        "Axis 3 Counts" = if(nrow(wear_hour_data) > 0) sum(wear_hour_data$axis3) else 0,
        "Axis 1 Average Counts" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis1), 1) else 0,
        "Axis 2 Average Counts" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis2), 1) else 0,
        "Axis 3 Average Counts" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis3), 1) else 0,
        "Axis 1 Max Counts" = if(nrow(wear_hour_data) > 0) max(wear_hour_data$axis1) else 0,
        "Axis 2 Max Counts" = if(nrow(wear_hour_data) > 0) max(wear_hour_data$axis2) else 0,
        "Axis 3 Max Counts" = if(nrow(wear_hour_data) > 0) max(wear_hour_data$axis3) else 0,
        "Axis 1 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis1) * (60 / epoch_sec), 1) else 0,
        "Axis 2 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis2) * (60 / epoch_sec), 1) else 0,
        "Axis 3 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis3) * (60 / epoch_sec), 1) else 0,
        "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
        "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
        "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
        "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts) * (60 / epoch_sec), 1) else 0,
        "Steps Counts" = if(nrow(wear_hour_data) > 0) sum(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Average Counts" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Steps Max Counts" = if(nrow(wear_hour_data) > 0) max(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Per Minute" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Lux Average Counts" = 0,
        "Lux Max Counts" = 0,
        "Number of Epochs" = total_epochs,
        Time = round(total_epochs * (epoch_sec / 60), 1),
        "Calendar Days" = 1
      )
    }
  }

  hourly_data <- do.call(rbind.data.frame, c(hourly_rows, stringsAsFactors = FALSE, make.row.names = FALSE))
  return(hourly_data)
}


#' Export Sedentary Bout Analysis (ActiLife-Compatible)
#'
#' Exports bout-level sedentary analysis matching ActiLife's Sedentary Analysis
#' CSV format. One row per sedentary bout with comprehensive metrics including
#' inter-bout intervals (time since last bout), activity counts, steps, and more.
#'
#' @param analysis_results A canhrActi_analysis object from canhrActi()
#' @param output_path Character. Full path for output CSV file (optional, returns data if NULL)
#' @param sedentary_threshold Numeric. CPM threshold for sedentary (default: 100)
#' @param min_bout_length Numeric. Minimum bout duration in minutes (default: 1)
#' @param subject_id Character. Subject identifier (default: extracts from filename)
#' @param weight_lbs Numeric. Weight in pounds (optional)
#' @param age Numeric. Age in years (optional)
#' @param gender Character. Gender ("M", "F", or "") (optional)
#'
#' @return Data frame with bout-level metrics (also written to CSV if output_path provided)
#'
#' @details
#' This function produces output matching ActiLife's Sedentary Analysis export,
#' enabling direct comparison and compatibility with existing workflows.
#'
#' \strong{Key Metrics:}
#' \itemize{
#'   \item \strong{Time in Sedentary Bout}: Duration of each bout in minutes
#'   \item \strong{Time since last Sedentary Bout}: Inter-bout interval (IBI) - critical
#'     for understanding break patterns. Higher variability in IBI indicates more
#'     fragmented sedentary patterns.
#'   \item \strong{Activity counts}: Total, average, max, and CPM for each axis
#'   \item \strong{Vector magnitude}: Combined 3-axis activity metrics
#'   \item \strong{Steps}: Step counts during each bout
#' }
#'
#' \strong{Research Applications:}
#' \itemize{
#'   \item Analyze bout duration distributions (W50, alpha, Gini)
#'   \item Study inter-bout intervals (break patterns)
#'   \item Examine time-of-day effects on sedentary accumulation
#'   \item Compare weekday vs weekend patterns
#' }
#'
#' @references
#' Chastin SF, Granat MH (2010). Methods for objective measure, quantification
#' and analysis of sedentary behaviour and inactivity. Gait Posture, 31(1):82-86.
#'
#' @examples
#' \dontrun{
#' result <- canhrActi("participant.agd")
#' bouts <- export_sedentary_bouts(result, "sedentary_bouts.csv")
#'
#' # Analyze inter-bout intervals
#' mean(bouts$`Time since last Sedentary Bout`, na.rm = TRUE)
#'
#' # W50 calculation directly from bout data
#' sorted <- sort(bouts$`Time in Sedentary Bout`, decreasing = TRUE)
#' cumulative <- cumsum(sorted)
#' w50 <- sorted[which(cumulative >= sum(sorted) / 2)[1]]
#' }
#'
#' @export
export_sedentary_bouts <- function(analysis_results,
                                   output_path = NULL,
                                   sedentary_threshold = 100,
                                   min_bout_length = 1,
                                   subject_id = NULL,
                                   weight_lbs = NULL,
                                   age = NULL,
                                   gender = NULL) {

  if (!inherits(analysis_results, "canhrActi_analysis")) {
    stop("analysis_results must be a canhrActi_analysis object from canhrActi()")
  }

  # Extract subject info
  if (!is.null(analysis_results$subject_info)) {
    subj <- analysis_results$subject_info
    if (is.null(subject_id)) subject_id <- subj$subject_id
    if (is.null(weight_lbs)) weight_lbs <- subj$weight_lbs
    if (is.null(age)) age <- subj$age
    if (is.null(gender)) gender <- subj$sex
  }

  if (is.null(subject_id) || is.na(subject_id)) {
    subject_id <- tools::file_path_sans_ext(basename(analysis_results$parameters$file_path))
  }

  if (is.null(weight_lbs) || is.na(weight_lbs)) weight_lbs <- 0
  if (is.null(age) || is.na(age)) age <- 0
  if (is.null(gender) || is.na(gender)) gender <- ""

  epoch_data <- analysis_results$epoch_data
  filename <- basename(analysis_results$parameters$file_path)

  # Get epoch length
  if (nrow(epoch_data) >= 2) {
    epoch_sec <- as.numeric(difftime(epoch_data$timestamp[2],
                                     epoch_data$timestamp[1],
                                     units = "secs"))
  } else {
    epoch_sec <- 60
  }

  # Identify sedentary epochs (using wear time if available)
  cpm <- epoch_data$axis1 * (60 / epoch_sec)
  is_sedentary <- cpm < sedentary_threshold

  # Apply wear time mask if available
  if ("wear_time" %in% names(epoch_data)) {
    is_sedentary <- is_sedentary & epoch_data$wear_time
  }

  # Detect sedentary bouts using run-length encoding
  rle_sed <- rle(is_sedentary)
  end_indices <- cumsum(rle_sed$lengths)
  start_indices <- c(1, end_indices[-length(end_indices)] + 1)

  # Extract sedentary runs
  sed_mask <- rle_sed$values
  bout_starts <- start_indices[sed_mask]
  bout_ends <- end_indices[sed_mask]

  if (length(bout_starts) == 0) {
    message("No sedentary bouts detected")
    return(data.frame())
  }

  # Calculate bout durations
  bout_lengths <- bout_ends - bout_starts + 1
  duration_min <- bout_lengths * (epoch_sec / 60)

  # Filter by minimum bout length
  valid_bouts <- duration_min >= min_bout_length
  bout_starts <- bout_starts[valid_bouts]
  bout_ends <- bout_ends[valid_bouts]
  duration_min <- duration_min[valid_bouts]

  if (length(bout_starts) == 0) {
    message("No sedentary bouts >= ", min_bout_length, " minutes detected")
    return(data.frame())
  }

  n_bouts <- length(bout_starts)
  rows_list <- vector("list", n_bouts)

  for (i in seq_len(n_bouts)) {
    start_idx <- bout_starts[i]
    end_idx <- bout_ends[i]

    bout_data <- epoch_data[start_idx:end_idx, ]
    bout_start_time <- epoch_data$timestamp[start_idx]
    bout_end_time <- epoch_data$timestamp[end_idx]

    # Calculate inter-bout interval (time since last sedentary bout ended)
    if (i == 1) {
      time_since_last <- 0
    } else {
      prev_end_time <- epoch_data$timestamp[bout_ends[i - 1]]
      time_since_last <- as.numeric(difftime(bout_start_time, prev_end_time, units = "mins"))
    }

    # Calculate number of epochs
    n_epochs <- nrow(bout_data)

    # Activity counts
    axis1_counts <- sum(bout_data$axis1, na.rm = TRUE)
    axis2_counts <- if ("axis2" %in% names(bout_data)) sum(bout_data$axis2, na.rm = TRUE) else 0
    axis3_counts <- if ("axis3" %in% names(bout_data)) sum(bout_data$axis3, na.rm = TRUE) else 0

    axis1_avg <- mean(bout_data$axis1, na.rm = TRUE)
    axis2_avg <- if ("axis2" %in% names(bout_data)) mean(bout_data$axis2, na.rm = TRUE) else 0
    axis3_avg <- if ("axis3" %in% names(bout_data)) mean(bout_data$axis3, na.rm = TRUE) else 0

    axis1_max <- max(bout_data$axis1, na.rm = TRUE)
    axis2_max <- if ("axis2" %in% names(bout_data)) max(bout_data$axis2, na.rm = TRUE) else 0
    axis3_max <- if ("axis3" %in% names(bout_data)) max(bout_data$axis3, na.rm = TRUE) else 0

    axis1_cpm <- axis1_avg * (60 / epoch_sec)
    axis2_cpm <- axis2_avg * (60 / epoch_sec)
    axis3_cpm <- axis3_avg * (60 / epoch_sec)

    # Vector magnitude
    if (all(c("axis1", "axis2", "axis3") %in% names(bout_data))) {
      vm <- sqrt(bout_data$axis1^2 + bout_data$axis2^2 + bout_data$axis3^2)
      vm_counts <- sum(vm, na.rm = TRUE)
      vm_avg <- mean(vm, na.rm = TRUE)
      vm_max <- max(vm, na.rm = TRUE)
      vm_cpm <- vm_avg * (60 / epoch_sec)
    } else {
      vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
    }

    # Steps
    if ("steps" %in% names(bout_data)) {
      steps_counts <- sum(bout_data$steps, na.rm = TRUE)
      steps_avg <- mean(bout_data$steps, na.rm = TRUE)
      steps_max <- max(bout_data$steps, na.rm = TRUE)
      steps_per_min <- steps_avg * (60 / epoch_sec)
    } else {
      steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
    }

    # Lux (light)
    if ("lux" %in% names(bout_data)) {
      lux_avg <- mean(bout_data$lux, na.rm = TRUE)
      lux_max <- max(bout_data$lux, na.rm = TRUE)
    } else {
      lux_avg <- lux_max <- 0
    }

    # Calendar days spanned
    start_date <- as.Date(bout_start_time)
    end_date <- as.Date(bout_end_time)
    calendar_days <- as.numeric(end_date - start_date) + 1

    rows_list[[i]] <- list(
      Subject = subject_id,
      Filename = filename,
      Epoch = epoch_sec,
      `Weight (lbs)` = weight_lbs,
      Age = age,
      Gender = gender,
      `Sedentary Bout Start` = format(bout_start_time, "%m/%d/%Y %I:%M:%S %p"),
      `Sedentary Bout End` = format(bout_end_time, "%m/%d/%Y %I:%M:%S %p"),
      `Time in Sedentary Bout` = round(duration_min[i], 1),
      `Time since last Sedentary Bout` = round(time_since_last, 1),
      `Axis 1 Counts` = round(axis1_counts, 1),
      `Axis 2 Counts` = round(axis2_counts, 1),
      `Axis 3 Counts` = round(axis3_counts, 1),
      `Axis 1 Average Counts` = round(axis1_avg, 1),
      `Axis 2 Average Counts` = round(axis2_avg, 1),
      `Axis 3 Average Counts` = round(axis3_avg, 1),
      `Axis 1 Max Counts` = axis1_max,
      `Axis 2 Max Counts` = axis2_max,
      `Axis 3 Max Counts` = axis3_max,
      `Axis 1 CPM` = round(axis1_cpm, 1),
      `Axis 2 CPM` = round(axis2_cpm, 1),
      `Axis 3 CPM` = round(axis3_cpm, 1),
      `Vector Magnitude Counts` = round(vm_counts, 1),
      `Vector Magnitude Average Counts` = round(vm_avg, 1),
      `Vector Magnitude Max Counts` = round(vm_max, 1),
      `Vector Magnitude CPM` = round(vm_cpm, 1),
      `Steps Counts` = steps_counts,
      `Steps Average Counts` = round(steps_avg, 1),
      `Steps Max Counts` = steps_max,
      `Steps Per Minute` = round(steps_per_min, 1),
      `Lux Average Counts` = round(lux_avg, 1),
      `Lux Max Counts` = lux_max,
      `Number of Epochs` = n_epochs,
      Time = round(duration_min[i], 1),
      `Calendar Days` = calendar_days
    )
  }

  bout_data <- do.call(rbind.data.frame, c(rows_list, stringsAsFactors = FALSE, make.row.names = FALSE))

  # Write to file if output_path provided
  if (!is.null(output_path)) {
    utils::write.csv(bout_data, output_path, row.names = FALSE)
    message("Exported ", n_bouts, " sedentary bouts to: ", output_path)
  }

  invisible(bout_data)
}


#' Analyze Inter-Bout Intervals (Breaks from Sedentary)
#'
#' Detailed analysis of inter-bout intervals (IBIs) from sedentary bout data.
#' IBIs represent breaks from sedentary behavior and are critical for understanding
#' how sedentary time is accumulated.
#'
#' @param bout_data Data frame from export_sedentary_bouts() or with column
#'   'Time since last Sedentary Bout'
#'
#' @return List with IBI statistics and classifications
#'
#' @details
#' Inter-bout intervals (IBIs) represent the duration of active/standing time

#' between consecutive sedentary bouts. Analysis of IBIs provides insight into:
#'
#' \itemize{
#'   \item Break frequency and duration patterns
#'   \item Whether breaks are short (toilet, snack) or long (exercise, commute)
#'   \item Time-of-day variations in break patterns
#' }
#'
#' \strong{Break Classifications:}
#' \itemize{
#'   \item \strong{Micro-break}: < 2 minutes (postural shift, brief stand)
#'   \item \strong{Short break}: 2-5 minutes (bathroom, brief task)
#'   \item \strong{Medium break}: 5-15 minutes (walk, snack preparation)
#'   \item \strong{Long break}: 15-30 minutes (meal, short walk)
#'   \item \strong{Extended break}: > 30 minutes (exercise, commute, outdoor activity)
#' }
#'
#' @references
#' Healy GN, et al. (2008). Breaks in sedentary time: beneficial associations
#' with metabolic risk. Diabetes Care, 31(4):661-666.
#'
#' @export
analyze_inter_bout_intervals <- function(bout_data) {

  # Handle both original column names and R's syntactically valid names
  col_name <- NULL
  if ("Time since last Sedentary Bout" %in% names(bout_data)) {
    col_name <- "Time since last Sedentary Bout"
  } else if ("Time.since.last.Sedentary.Bout" %in% names(bout_data)) {
    col_name <- "Time.since.last.Sedentary.Bout"
  } else {
    stop("bout_data must contain 'Time since last Sedentary Bout' column")
  }

  ibi <- bout_data[[col_name]]

  # Exclude first bout (IBI = 0 or NA)
  ibi_valid <- ibi[ibi > 0]

  if (length(ibi_valid) == 0) {
    return(list(
      n_breaks = 0,
      mean_ibi = NA,
      median_ibi = NA,
      sd_ibi = NA,
      min_ibi = NA,
      max_ibi = NA,
      break_classifications = NULL,
      message = "No inter-bout intervals found"
    ))
  }

  # Basic statistics
  stats <- list(
    n_breaks = length(ibi_valid),
    mean_ibi = round(mean(ibi_valid), 2),
    median_ibi = round(median(ibi_valid), 2),
    sd_ibi = round(sd(ibi_valid), 2),
    min_ibi = round(min(ibi_valid), 2),
    max_ibi = round(max(ibi_valid), 2),
    cv_ibi = round(sd(ibi_valid) / mean(ibi_valid), 3),
    iqr_ibi = round(IQR(ibi_valid), 2)
  )

  # Percentiles
  stats$percentiles <- round(quantile(ibi_valid, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95)), 2)

  # Break classifications
  classifications <- data.frame(
    category = c("Micro-break (<2 min)", "Short break (2-5 min)",
                 "Medium break (5-15 min)", "Long break (15-30 min)",
                 "Extended break (>30 min)"),
    count = c(
      sum(ibi_valid < 2),
      sum(ibi_valid >= 2 & ibi_valid < 5),
      sum(ibi_valid >= 5 & ibi_valid < 15),
      sum(ibi_valid >= 15 & ibi_valid < 30),
      sum(ibi_valid >= 30)
    ),
    stringsAsFactors = FALSE
  )
  classifications$percent <- round(100 * classifications$count / length(ibi_valid), 1)

  stats$break_classifications <- classifications

  # Health-related summary
  # Research shows longer breaks are more beneficial for metabolic health
  stats$pct_breaks_under_5min <- round(100 * sum(ibi_valid < 5) / length(ibi_valid), 1)
  stats$pct_breaks_over_15min <- round(100 * sum(ibi_valid >= 15) / length(ibi_valid), 1)

  class(stats) <- c("canhrActi_ibi_analysis", "list")
  return(stats)
}


#' Print Inter-Bout Interval Analysis
#'
#' @param x Object from analyze_inter_bout_intervals()
#' @param ... Additional arguments (ignored)
#'
#' @export
print.canhrActi_ibi_analysis <- function(x, ...) {
  cat("\n=== Inter-Bout Interval Analysis ===\n\n")
  cat("Number of breaks:", x$n_breaks, "\n\n")

  cat("Break Duration Statistics (minutes):\n")
  cat("  Mean:   ", x$mean_ibi, "\n")
  cat("  Median: ", x$median_ibi, "\n")
  cat("  SD:     ", x$sd_ibi, "\n")
  cat("  CV:     ", x$cv_ibi, "\n")
  cat("  Range:  ", x$min_ibi, "-", x$max_ibi, "\n")
  cat("  IQR:    ", x$iqr_ibi, "\n\n")

  cat("Break Classifications:\n")
  for (i in seq_len(nrow(x$break_classifications))) {
    cat(sprintf("  %-25s %4d (%5.1f%%)\n",
                x$break_classifications$category[i],
                x$break_classifications$count[i],
                x$break_classifications$percent[i]))
  }

  cat("\nHealth Indicators:\n")
  cat("  Short breaks (<5 min):    ", x$pct_breaks_under_5min, "%\n")
  cat("  Substantial breaks (>15 min):", x$pct_breaks_over_15min, "%\n")

  invisible(x)
}
