#' canhrActi: Analyze ActiGraph AGD Files
#'
#' Main function for analyzing ActiGraph .agd files. Automatically handles
#' single files, multiple files, or entire folders.
#'
#' @param agd_file_path Character. Can be:
#'   \itemize{
#'     \item Single file path (e.g., "path/to/file.agd")
#'     \item Vector of file paths (e.g., c("file1.agd", "file2.agd"))
#'     \item Folder path (will analyze all .agd files in folder)
#'   }
#' @param wear_time_algorithm Character. Which wear time detection algorithm to use:
#'   \itemize{
#'     \item \code{"choi"} - Choi et al. (2011) algorithm (recommended, default)
#'     \item \code{"troiano"} - Troiano et al. (2007) algorithm
#'     \item \code{"CANHR2025"} - CANHR 2025 custom algorithm
#'   }
#' @param intensity_algorithm Character. Which intensity classification to use:
#'   \itemize{
#'     \item \code{"freedson1998"} - Standard Freedson Adult cut points (default)
#'     \item \code{"CANHR"} - CANHR custom cut points
#'   }
#' @param min_wear_hours Numeric. Minimum hours of wear time for a valid day (default: 10)
#' @param axis_to_analyze Character. Which axis to use for analysis:
#'   \itemize{
#'     \item \code{"axis1"} - Vertical axis (Y-axis, default, matches most research)
#'     \item \code{"vector_magnitude"} - 3-axis vector magnitude
#'   }
#'   \strong{WARNING:} Freedson (1998) and most published cut points were validated
#'   using vertical axis (axis1) only. Using vector magnitude with axis1-validated
#'   cut points may produce results that differ from published literature. Use axis1
#'   (default) for standard research comparability.
#' @param output_summary Logical. Print detailed summary to console? (default: TRUE)
#'
#' @return A list of class \code{"canhrActi_analysis"} containing:
#'   \itemize{
#'     \item \code{participant_info} - Data frame with participant details
#'     \item \code{epoch_data} - Data frame with per-epoch counts and classifications
#'     \item \code{daily_summary} - Data frame with per-day statistics
#'     \item \code{overall_summary} - Data frame with overall statistics
#'     \item \code{intensity_summary} - Data frame with intensity distribution
#'     \item \code{valid_days} - Character vector of valid dates (YYYY-MM-DD)
#'     \item \code{wear_time_periods} - Data frame with continuous wear periods
#'     \item \code{parameters} - List of analysis parameters used
#'   }
#'
#' @details
#' \strong{Algorithms Implemented:}
#'
#' \strong{Activity Counts:}
#' Uses ActiGraph's official algorithm (Neishabouri et al., 2022) with exact
#' IIR filter coefficients to produce identical counts to ActiLife software.
#'
#' \strong{Wear Time Detection:}
#' \itemize{
#'   \item \strong{Choi (2011):} 90-minute window with 2-minute spike tolerance
#'     and 30-minute upstream/downstream zero-count validation. More sophisticated
#'     and recommended for modern research.
#'   \item \strong{Troiano (2007):} 60-minute window with 2-minute spike tolerance.
#'     Widely used in NHANES and epidemiological studies.
#'   \item \strong{CANHR 2025:} 120-minute window with 3-minute spike tolerance
#'     and 45-minute upstream/downstream validation. Custom algorithm.
#' }
#'
#' \strong{Intensity Classification:}
#' \itemize{
#'   \item \strong{Freedson (1998):} Validated cut points for adults worn at hip
#'     \itemize{
#'       \item Sedentary: 0-99 CPM
#'       \item Light: 100-1951 CPM
#'       \item Moderate: 1952-5724 CPM
#'       \item Vigorous: 5725-9498 CPM
#'       \item Very Vigorous: >=9499 CPM
#'     }
#'   \item \strong{CANHR:} Custom cut points for specific populations
#'     \itemize{
#'       \item Sedentary: 0-150 CPM
#'       \item Light: 151-2200 CPM
#'       \item Moderate: 2201-6000 CPM
#'       \item Vigorous: 6001-10000 CPM
#'       \item Very Vigorous: >=10001 CPM
#'     }
#' }
#'
#' \strong{Valid Day Criteria:}
#' Days with >=10 hours (default) of wear time are considered valid for analysis.
#' This threshold can be adjusted via the \code{min_wear_hours} parameter.
#'
#' @references
#' \itemize{
#'   \item Neishabouri A, et al. (2022). Quantification of acceleration as activity
#'     counts in ActiGraph wearable. Scientific Reports, 12(1), 11958.
#'   \item Choi L, et al. (2011). Validation of accelerometer wear and nonwear time
#'     classification algorithm. Medicine & Science in Sports & Exercise, 43(2), 357-364.
#'   \item Troiano RP, et al. (2007). Physical activity in the United States measured
#'     by accelerometer. Medicine & Science in Sports & Exercise, 40(1), 181-188.
#'   \item Freedson PS, et al. (1998). Calibration of the Computer Science and
#'     Applications, Inc. accelerometer. Medicine & Science in Sports & Exercise,
#'     30(5), 777-781.
#' }
#'
#' @examples
#' \dontrun{
#' # Single file
#' results <- canhrActi("path/to/file.agd")
#'
#' # Multiple files (batch)
#' results <- canhrActi(c("file1.agd", "file2.agd"))
#'
#' # Entire folder
#' results <- canhrActi("C:/My Data Folder")
#'
#' # View overall summary
#' print(results)
#'
#' # Access daily summary
#' print(results$daily_summary)
#'
#' # Access epoch-by-epoch data
#' head(results$epoch_data)
#'
#' # Use Troiano algorithm instead
#' results <- canhrActi("file.agd",
#'                      wear_time_algorithm = "troiano")
#'
#' # Use vector magnitude instead of vertical axis
#' results <- canhrActi("file.agd",
#'                      axis_to_analyze = "vector_magnitude")
#'
#' # Use CANHR cut points
#' results <- canhrActi("file.agd",
#'                      intensity_algorithm = "CANHR")
#'
#' # Require 8 hours for valid day (instead of 10)
#' results <- canhrActi("file.agd", min_wear_hours = 8)
#' }
#'
#' @export
canhrActi <- function(agd_file_path,
                              wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                              intensity_algorithm = c("freedson1998", "CANHR"),
                              min_wear_hours = 10,
                              axis_to_analyze = c("axis1", "vector_magnitude"),
                              output_summary = TRUE,
                              lfe_mode = FALSE) {

  if ((length(agd_file_path) == 1 && dir.exists(agd_file_path)) || length(agd_file_path) > 1) {
    return(canhrActi.batch(agd_file_path, wear_time_algorithm, intensity_algorithm,
                           min_wear_hours, axis_to_analyze,
                           export = !output_summary, lfe_mode = lfe_mode))
  }

  return(.canhrActi.single.internal(agd_file_path, wear_time_algorithm, intensity_algorithm,
                                    min_wear_hours, axis_to_analyze,
                                    output_summary, lfe_mode))
}


.canhrActi.single.internal <- function(agd_file_path,
                                   wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                                   intensity_algorithm = c("freedson1998", "CANHR"),
                                   min_wear_hours = 10,
                                   axis_to_analyze = c("axis1", "vector_magnitude"),
                                   output_summary = TRUE,
                                   lfe_mode = FALSE) {

  wear_time_algorithm <- match.arg(wear_time_algorithm)
  intensity_algorithm <- match.arg(intensity_algorithm)
  axis_to_analyze <- match.arg(axis_to_analyze)

  # Validate parameter combinations
  if (axis_to_analyze == "vector_magnitude" && intensity_algorithm == "freedson1998") {
    warning("Freedson (1998) cutpoints were validated using axis1 only.\n",
            "Using vector_magnitude may produce results that differ from published literature.\n",
            "Consider using axis_to_analyze = 'axis1' for standard research comparability.")
  }

  if (!file.exists(agd_file_path)) {
    stop("File not found: ", agd_file_path, "\n",
         "Please check that:\n",
         "  1. The file path is correct\n",
         "  2. The file exists in the specified location\n",
         "  3. You have read permissions for this file")
  }

  if (!grepl("\\.agd$", agd_file_path, ignore.case = TRUE)) {
    warning("File does not have .agd extension: ", basename(agd_file_path), "\n",
            "Expected .agd file format. Attempting to read anyway...")
  }

  if (min_wear_hours < 0 || min_wear_hours > 24) {
    stop("min_wear_hours must be between 0 and 24. You provided: ", min_wear_hours, "\n",
         "Common values: 10 hours (default), 8 hours (lenient), 12 hours (strict)")
  }

  if (output_summary) {
    cat("\nAnalyzing:", basename(agd_file_path), "\n")
  }

  agd.data <- read.agd(agd_file_path)
  counts.data <- agd.counts(agd.data)
  subject_info <- extract.subject.info(agd.data)

  if (axis_to_analyze == "axis1") {
    counts.for.analysis <- counts.data$axis1
  } else {
    counts.for.analysis <- vm(counts.data$axis1, counts.data$axis2, counts.data$axis3)
  }

  if (wear_time_algorithm == "choi") {
    wear.time <- wear.choi(counts.for.analysis)
  } else if (wear_time_algorithm == "troiano") {
    wear.time <- wear.troiano(counts.for.analysis)
  } else if (wear_time_algorithm == "CANHR2025") {
    wear.time <- wear.CANHR2025(counts.for.analysis)
  }

  wear.minutes <- sum(wear.time)
  wear.hours <- wear.minutes / 60
  wear.percent <- 100 * wear.minutes / length(wear.time)

  if (intensity_algorithm == "freedson1998") {
    intensity <- freedson(counts.for.analysis)
  } else if (intensity_algorithm == "CANHR") {
    intensity <- CANHR.Cutpoints(counts.for.analysis)
  } else {
    intensity <- CANHR.Cutpoints(counts.for.analysis)
  }

  valid.days.results <- valid.days(counts.data$timestamp, wear.time, min.wear.hours = min_wear_hours)

  intensity.summary <- intensity(intensity, wear.time)
  mvpa.minutes <- mvpa(intensity, wear.time)

  epoch.data <- data.frame(
    epoch = 1:nrow(counts.data),
    timestamp = counts.data$timestamp,
    date = as.Date(counts.data$timestamp),
    axis1 = counts.data$axis1,
    axis2 = counts.data$axis2,
    axis3 = counts.data$axis3,
    steps = counts.data$steps,
    counts_used = counts.for.analysis,
    wear_time = wear.time,
    intensity = as.character(intensity),
    is_valid_day = valid.days.results$valid_day_index,
    stringsAsFactors = FALSE
  )

  daily.stats <- valid.days.results$daily_summary

  # Vectorized calculation of daily intensity minutes
  wear.epochs <- epoch.data[epoch.data$wear_time, ]

  if (nrow(wear.epochs) > 0) {
    daily.intensity <- aggregate(
      cbind(sedentary = intensity == "sedentary",
            light = intensity == "light",
            moderate = intensity == "moderate",
            vigorous = intensity == "vigorous",
            very_vigorous = intensity == "very_vigorous",
            mvpa = intensity %in% c("moderate", "vigorous", "very_vigorous"),
            counts_used = counts_used) ~ date,
      data = wear.epochs,
      FUN = function(x) if (is.logical(x)) sum(x) else mean(x)
    )

    # Merge with daily.stats
    daily.stats <- merge(daily.stats, daily.intensity, by = "date", all.x = TRUE, sort = FALSE)
    names(daily.stats)[names(daily.stats) == "sedentary"] <- "sedentary_min"
    names(daily.stats)[names(daily.stats) == "light"] <- "light_min"
    names(daily.stats)[names(daily.stats) == "moderate"] <- "moderate_min"
    names(daily.stats)[names(daily.stats) == "vigorous"] <- "vigorous_min"
    names(daily.stats)[names(daily.stats) == "very_vigorous"] <- "very_vigorous_min"
    names(daily.stats)[names(daily.stats) == "mvpa"] <- "mvpa_min"
    names(daily.stats)[names(daily.stats) == "counts_used"] <- "average_cpm"
  } else {
    daily.stats$sedentary_min <- NA
    daily.stats$light_min <- NA
    daily.stats$moderate_min <- NA
    daily.stats$vigorous_min <- NA
    daily.stats$very_vigorous_min <- NA
    daily.stats$mvpa_min <- NA
    daily.stats$average_cpm <- NA
  }

  overall.summary <- data.frame(
    total_days = nrow(daily.stats),
    valid_days = valid.days.results$n_valid_days,
    total_wear_minutes = wear.minutes,
    total_wear_hours = round(wear.hours, 2),
    average_wear_per_day = round(wear.hours / nrow(daily.stats), 2),
    sedentary_minutes = intensity.summary$minutes[intensity.summary$intensity == "sedentary"],
    light_minutes = intensity.summary$minutes[intensity.summary$intensity == "light"],
    moderate_minutes = intensity.summary$minutes[intensity.summary$intensity == "moderate"],
    vigorous_minutes = intensity.summary$minutes[intensity.summary$intensity == "vigorous"],
    very_vigorous_minutes = intensity.summary$minutes[intensity.summary$intensity == "very_vigorous"],
    mvpa_minutes = mvpa.minutes,
    sedentary_percent = round(intensity.summary$percentage[intensity.summary$intensity == "sedentary"], 2),
    light_percent = round(intensity.summary$percentage[intensity.summary$intensity == "light"], 2),
    moderate_percent = round(intensity.summary$percentage[intensity.summary$intensity == "moderate"], 2),
    vigorous_percent = round(intensity.summary$percentage[intensity.summary$intensity == "vigorous"], 2),
    very_vigorous_percent = round(intensity.summary$percentage[intensity.summary$intensity == "very_vigorous"], 2),
    mvpa_percent = round(100 * mvpa.minutes / wear.minutes, 2),
    stringsAsFactors = FALSE
  )

  results <- list(
    epoch_data = epoch.data,
    daily_summary = daily.stats,
    overall_summary = overall.summary,
    intensity_summary = intensity.summary,
    valid_days = valid.days.results$valid_days,
    subject_info = subject_info,
    parameters = list(
      file_path = agd_file_path,
      wear_time_algorithm = wear_time_algorithm,
      intensity_algorithm = intensity_algorithm,
      axis_analyzed = axis_to_analyze,
      min_wear_hours = min_wear_hours
    )
  )

  class(results) <- c("canhrActi_analysis", "list")

  if (output_summary) {
    cat("\nAnalysis Complete\n")
    cat("Valid days:", overall.summary$valid_days, "/", overall.summary$total_days, "\n")
    cat("Total wear time:", overall.summary$total_wear_hours, "hours\n")
    cat("MVPA:", overall.summary$mvpa_minutes, "minutes\n\n")
  }

  return(results)
}


#' Print Method for canhrActi Analysis Results
#' @param x An object of class \code{canhrActi_analysis}
#' @param ... Additional arguments (not used)
#' @export
print.canhrActi_analysis <- function(x, ...) {
  s <- x$overall_summary
  cat("\ncanhrActi Analysis:", basename(x$parameters$file_path), "\n")
  cat("Valid days:", s$valid_days, "/", s$total_days, "\n")
  cat("Wear time:", s$total_wear_hours, "hours\n")
  cat("MVPA:", s$mvpa_minutes, "min (", s$mvpa_percent, "%)\n\n")
  invisible(x)
}


#' Summary Method for canhrActi Analysis Results
#' @param object An object of class \code{canhrActi_analysis}
#' @param ... Additional arguments (not used)
#' @export
summary.canhrActi_analysis <- function(object, ...) {
  print(object$daily_summary)
  invisible(object)
}

`%||%` <- function(a, b) if (is.null(a)) b else a


#' Analyze AGD File
#'
#' Alias for \code{\link{canhrActi}}.
#'
#' @param ... Arguments passed to \code{canhrActi()}
#' @export
analyze.agd.file <- function(...) {
  canhrActi(...)
}
