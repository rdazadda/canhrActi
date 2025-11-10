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

  sedentary <- sum(all_valid_epochs$intensity == "sedentary" | !all_valid_epochs$wear_time)
  light <- sum(all_valid_epochs$intensity == "light" & all_valid_epochs$wear_time)
  moderate <- sum(all_valid_epochs$intensity == "moderate" & all_valid_epochs$wear_time)
  vigorous <- sum(all_valid_epochs$intensity == "vigorous" & all_valid_epochs$wear_time)
  very_vigorous <- sum(all_valid_epochs$intensity == "very_vigorous" & all_valid_epochs$wear_time)
  total_mvpa <- moderate + vigorous + very_vigorous
  total_epochs <- nrow(all_valid_epochs)

  wear_valid_epochs <- all_valid_epochs[all_valid_epochs$wear_time, ]

  if (nrow(wear_valid_epochs) > 0) {
    vm_counts <- sqrt(wear_valid_epochs$axis1^2 + wear_valid_epochs$axis2^2 + wear_valid_epochs$axis3^2)
  } else {
    vm_counts <- numeric(0)
  }

  avg_mvpa_per_day <- total_mvpa / nrow(valid_daily)

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
    "Axis 1 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis1), 1) else 0,
    "Axis 2 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis2), 1) else 0,
    "Axis 3 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis3), 1) else 0,
    "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
    "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
    "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
    "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
    "Steps Counts" = if(nrow(wear_valid_epochs) > 0) sum(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Average Counts" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Steps Max Counts" = if(nrow(wear_valid_epochs) > 0) max(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Per Minute" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Lux Average Counts" = 0,
    "Lux Max Counts" = 0,
    "Number of Epochs" = total_epochs,
    "Time" = total_epochs,
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

    sedentary <- sum(day_epochs_all$intensity == "sedentary" | !day_epochs_all$wear_time)
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
      "Axis 1 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis1), 1) else 0,
      "Axis 2 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis2), 1) else 0,
      "Axis 3 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis3), 1) else 0,
      "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
      "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
      "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
      "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
      "Steps Counts" = if(nrow(wear_epochs) > 0) sum(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Average Counts" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Steps Max Counts" = if(nrow(wear_epochs) > 0) max(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Per Minute" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Lux Average Counts" = 0,
      "Lux Max Counts" = 0,
      "Number of Epochs" = total_epochs,
      Time = total_epochs,
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

      sedentary <- sum(all_hour_data$intensity == "sedentary" | !all_hour_data$wear_time)
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
        "Axis 1 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis1), 1) else 0,
        "Axis 2 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis2), 1) else 0,
        "Axis 3 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis3), 1) else 0,
        "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
        "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
        "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
        "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
        "Steps Counts" = if(nrow(wear_hour_data) > 0) sum(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Average Counts" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Steps Max Counts" = if(nrow(wear_hour_data) > 0) max(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Per Minute" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Lux Average Counts" = 0,
        "Lux Max Counts" = 0,
        "Number of Epochs" = total_epochs,
        Time = total_epochs,
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

  sedentary <- sum(all_valid_epochs$intensity == "sedentary" | !all_valid_epochs$wear_time)
  light <- sum(all_valid_epochs$intensity == "light" & all_valid_epochs$wear_time)
  moderate <- sum(all_valid_epochs$intensity == "moderate" & all_valid_epochs$wear_time)
  vigorous <- sum(all_valid_epochs$intensity == "vigorous" & all_valid_epochs$wear_time)
  very_vigorous <- sum(all_valid_epochs$intensity == "very_vigorous" & all_valid_epochs$wear_time)
  total_mvpa <- moderate + vigorous + very_vigorous
  total_epochs <- nrow(all_valid_epochs)

  wear_valid_epochs <- all_valid_epochs[all_valid_epochs$wear_time, ]

  if (nrow(wear_valid_epochs) > 0) {
    vm_counts <- sqrt(wear_valid_epochs$axis1^2 + wear_valid_epochs$axis2^2 + wear_valid_epochs$axis3^2)
  } else {
    vm_counts <- numeric(0)
  }

  avg_mvpa_per_day <- total_mvpa / nrow(valid_daily)

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
    "Axis 1 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis1), 1) else 0,
    "Axis 2 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis2), 1) else 0,
    "Axis 3 CPM" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$axis3), 1) else 0,
    "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
    "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
    "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
    "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
    "Steps Counts" = if(nrow(wear_valid_epochs) > 0) sum(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Average Counts" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Steps Max Counts" = if(nrow(wear_valid_epochs) > 0) max(wear_valid_epochs$steps, na.rm = TRUE) else 0,
    "Steps Per Minute" = if(nrow(wear_valid_epochs) > 0) round(mean(wear_valid_epochs$steps, na.rm = TRUE), 1) else 0,
    "Lux Average Counts" = 0,
    "Lux Max Counts" = 0,
    "Number of Epochs" = total_epochs,
    "Time" = total_epochs,
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

    sedentary <- sum(day_epochs_all$intensity == "sedentary" | !day_epochs_all$wear_time)
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
      "Axis 1 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis1), 1) else 0,
      "Axis 2 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis2), 1) else 0,
      "Axis 3 CPM" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$axis3), 1) else 0,
      "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
      "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
      "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
      "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
      "Steps Counts" = if(nrow(wear_epochs) > 0) sum(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Average Counts" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Steps Max Counts" = if(nrow(wear_epochs) > 0) max(wear_epochs$steps, na.rm = TRUE) else 0,
      "Steps Per Minute" = if(nrow(wear_epochs) > 0) round(mean(wear_epochs$steps, na.rm = TRUE), 1) else 0,
      "Lux Average Counts" = 0,
      "Lux Max Counts" = 0,
      "Number of Epochs" = total_epochs,
      Time = total_epochs,
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

      sedentary <- sum(all_hour_data$intensity == "sedentary" | !all_hour_data$wear_time)
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
        "Axis 1 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis1), 1) else 0,
        "Axis 2 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis2), 1) else 0,
        "Axis 3 CPM" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$axis3), 1) else 0,
        "Vector Magnitude Counts" = if(length(vm_counts) > 0) round(sum(vm_counts), 1) else 0,
        "Vector Magnitude Average Counts" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
        "Vector Magnitude Max Counts" = if(length(vm_counts) > 0) round(max(vm_counts), 1) else 0,
        "Vector Magnitude CPM" = if(length(vm_counts) > 0) round(mean(vm_counts), 1) else 0,
        "Steps Counts" = if(nrow(wear_hour_data) > 0) sum(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Average Counts" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Steps Max Counts" = if(nrow(wear_hour_data) > 0) max(wear_hour_data$steps, na.rm = TRUE) else 0,
        "Steps Per Minute" = if(nrow(wear_hour_data) > 0) round(mean(wear_hour_data$steps, na.rm = TRUE), 1) else 0,
        "Lux Average Counts" = 0,
        "Lux Max Counts" = 0,
        "Number of Epochs" = total_epochs,
        Time = total_epochs,
        "Calendar Days" = 1
      )
    }
  }

  hourly_data <- do.call(rbind.data.frame, c(hourly_rows, stringsAsFactors = FALSE, make.row.names = FALSE))
  return(hourly_data)
}
