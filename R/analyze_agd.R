#' canhrActi: Analyze ActiGraph AGD Files
#'
#' Main function for analyzing ActiGraph accelerometer files. Supports
#' pre-processed .agd files. Automatically handles single files, multiple
#' files, or entire folders.
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
#'     \item \code{"evenson"} - Evenson children cut points (5-8 years)
#'     \item \code{"puyau"} - Puyau children cut points
#'     \item \code{"mattocks"} - Mattocks children cut points (12 years)
#'     \item \code{"pate_preschool"} - Pate preschool cut points (3-5 years)
#'     \item \code{"troiano"} - Troiano adult cut points
#'     \item \code{"sasaki_vm3"} - Sasaki triaxial cut points (requires vector_magnitude)
#'     \item \code{"copeland_older"} - Copeland older adult cut points (65+)
#'     \item \code{"auto"} - Auto-select based on participant age (if available)
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
#' @param calculate_mets Logical. Calculate METs and energy expenditure? (default: TRUE)
#' @param mets_algorithm Character. METs prediction algorithm:
#'   \itemize{
#'     \item \code{"freedson.vm3"} - Freedson VM3 (Sasaki 2011, default)
#'     \item \code{"freedson.adult"} - Freedson Adult (1998)
#'     \item \code{"crouter"} - Crouter 2-regression (2010)
#'     \item \code{"hendelman.adult"} - Hendelman Adult Overground (2000)
#'     \item \code{"hendelman.lifestyle"} - Hendelman Adult Lifestyle (2000)
#'     \item \code{"swartz"} - Swartz Adult (2000)
#'     \item \code{"leenders"} - Leenders Adult Treadmill (2003)
#'     \item \code{"yngve.treadmill"} - Yngve Adult Treadmill (2003)
#'     \item \code{"yngve.overground"} - Yngve Adult Overground (2003)
#'     \item \code{"brooks.overground"} - Brooks Adult Overground (2005)
#'     \item \code{"brooks.bm"} - Brooks Adult with Body Mass (2005)
#'     \item \code{"freedson.children"} - Freedson Children (2005)
#'   }
#' @param sleep_algorithm Character. Sleep detection algorithm to use (default: NULL for none):
#'   \itemize{
#'     \item \code{NULL} - No sleep analysis (default)
#'     \item \code{"cole_kripke"} - Cole-Kripke (1992) algorithm
#'     \item \code{"sadeh"} - Sadeh (1994) algorithm
#'     \item \code{"tudor_locke"} - Tudor-Locke algorithm
#'     \item \code{"auto"} - Auto-select Cole-Kripke algorithm
#'   }
#' @param participant_age Numeric. Participant age in years for auto-selecting
#'   age-appropriate cut-points (optional). If NULL, uses adult cut-points.
#' @param output_summary Logical. Print detailed summary to console? (default: TRUE)
#' @param calculate_fragmentation Logical. Calculate sedentary fragmentation metrics? (default: TRUE)
#' @param calculate_circadian Logical. Calculate circadian rhythm metrics (L5, M10, IS, IV)? (default: TRUE)
#' @param exclude_sleep Logical. Exclude detected sleep periods from sedentary fragmentation
#'   analysis? (default: TRUE). Per SBRN consensus, sedentary behavior is defined as
#'   \strong{waking} behavior only. When TRUE, uses Cole-Kripke + Tudor-Locke algorithms
#'   to detect consolidated sleep PERIODS (not epoch-by-epoch classification) and excludes
#'   them from fragmentation metrics. This avoids the common pitfall where sleep detection
#'   algorithms misclassify daytime sedentary behavior as sleep.
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
#'     \item \code{mets_summary} - Data frame with METs statistics (if calculate_mets = TRUE)
#'     \item \code{energy_expenditure_summary} - Data frame with kcal statistics (if calculate_mets = TRUE)
#'     \item \code{fragmentation} - Sedentary fragmentation metrics (if calculate_fragmentation = TRUE)
#'     \item \code{circadian} - Circadian rhythm metrics L5, M10, IS, IV, RA, phi (if calculate_circadian = TRUE)
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
#'       \item Sedentary: 0-100 CPM
#'       \item Light: 101-1951 CPM
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
#' # Single .agd file
#' results <- canhrActi("path/to/file.agd")
#'
#' # Multiple files (batch)
#' results <- canhrActi(c("file1.agd", "file2.agd"))
#'
#' # Entire folder (analyzes all .agd files)
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
#'
#' }
#'
#' @export
canhrActi <- function(agd_file_path,
                              wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                              intensity_algorithm = c("freedson1998", "CANHR", "evenson", "puyau",
                                                       "mattocks", "pate_preschool", "troiano",
                                                       "sasaki_vm3", "copeland_older", "auto"),
                              min_wear_hours = 10,
                              axis_to_analyze = c("axis1", "vector_magnitude"),
                              calculate_mets = TRUE,
                              mets_algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                                 "hendelman.adult", "hendelman.lifestyle", "swartz",
                                                 "leenders", "yngve.treadmill", "yngve.overground",
                                                 "brooks.overground", "brooks.bm", "freedson.children"),
                              sleep_algorithm = NULL,
                              participant_age = NULL,
                              output_summary = TRUE,
                              calculate_fragmentation = TRUE,
                              calculate_circadian = TRUE,
                              exclude_sleep = TRUE) {

  if ((length(agd_file_path) == 1 && dir.exists(agd_file_path)) || length(agd_file_path) > 1) {
    return(canhrActi.batch(agd_file_path, wear_time_algorithm, intensity_algorithm,
                           min_wear_hours, axis_to_analyze,
                           export = !output_summary,
                           calculate_mets = calculate_mets, mets_algorithm = mets_algorithm,
                           sleep_algorithm = sleep_algorithm,
                           participant_age = participant_age,
                           calculate_fragmentation = calculate_fragmentation,
                           calculate_circadian = calculate_circadian,
                           exclude_sleep = exclude_sleep))
  }

  return(.canhrActi.single.internal(agd_file_path, wear_time_algorithm, intensity_algorithm,
                                    min_wear_hours, axis_to_analyze,
                                    output_summary,
                                    calculate_mets, mets_algorithm,
                                    sleep_algorithm = sleep_algorithm,
                                    participant_age = participant_age,
                                    calculate_fragmentation, calculate_circadian,
                                    exclude_sleep = exclude_sleep))
}


.canhrActi.single.internal <- function(agd_file_path,
                                   wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                                   intensity_algorithm = c("freedson1998", "CANHR", "evenson", "puyau",
                                                            "mattocks", "pate_preschool", "troiano",
                                                            "sasaki_vm3", "copeland_older", "auto"),
                                   min_wear_hours = 10,
                                   axis_to_analyze = c("axis1", "vector_magnitude"),
                                   output_summary = TRUE,
                                   calculate_mets = TRUE,
                                   mets_algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                                      "hendelman.adult", "hendelman.lifestyle", "swartz",
                                                      "leenders", "yngve.treadmill", "yngve.overground",
                                                      "brooks.overground", "brooks.bm", "freedson.children"),
                                   sleep_algorithm = NULL,
                                   participant_age = NULL,
                                   calculate_fragmentation = TRUE,
                                   calculate_circadian = TRUE,
                                   exclude_sleep = TRUE) {

  wear_time_algorithm <- match.arg(wear_time_algorithm)
  intensity_algorithm <- match.arg(intensity_algorithm)
  axis_to_analyze <- match.arg(axis_to_analyze)
  mets_algorithm <- match.arg(mets_algorithm)

  # Auto-select intensity algorithm based on age
  if (intensity_algorithm == "auto" && !is.null(participant_age)) {
    if (participant_age < 5) {
      intensity_algorithm <- "pate_preschool"
      if (output_summary) message("Age < 5: Using Pate preschool cut-points")
    } else if (participant_age < 18) {
      intensity_algorithm <- "evenson"
      if (output_summary) message("Age 5-17: Using Evenson children cut-points")
    } else if (participant_age >= 65) {
      intensity_algorithm <- "copeland_older"
      if (output_summary) message("Age >= 65: Using Copeland older adult cut-points")
    } else {
      intensity_algorithm <- "freedson1998"
      if (output_summary) message("Age 18-64: Using Freedson adult cut-points")
    }
  } else if (intensity_algorithm == "auto") {
    intensity_algorithm <- "freedson1998"
    if (output_summary) message("No age provided; defaulting to Freedson adult cut-points")
  }

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

  # Check file type - only AGD files supported
  file_ext <- tolower(tools::file_ext(agd_file_path))
  if (file_ext != "agd") {
    stop("Unsupported file format: ", basename(agd_file_path), "\n",
         "Only ActiGraph .agd files (pre-processed from ActiLife) are supported.")
  }

  if (min_wear_hours < 0 || min_wear_hours > 24) {
    stop("min_wear_hours must be between 0 and 24. You provided: ", min_wear_hours, "\n",
         "Common values: 10 hours (default), 8 hours (lenient), 12 hours (strict)")
  }

  # Device type description
  device_description <- "ActiGraph .agd (pre-processed counts)"

  if (output_summary) {
    cat("\nAnalyzing:", basename(agd_file_path), "\n")
    cat("File type:", device_description, "\n")
  }

  # Read AGD file
  agd.data <- read.agd(agd_file_path)

  counts.data <- agd.counts(agd.data)
  subject_info <- extract.subject.info(agd.data)

  # Determine epoch length from settings or data
  epoch_length <- 60  # default
  if (!is.null(agd.data$settings) && is.data.frame(agd.data$settings)) {
    epoch_val <- agd.data$settings$settingValue[tolower(agd.data$settings$settingName) == "epochlength"]
    if (length(epoch_val) > 0 && !is.na(epoch_val[1])) {
      epoch_length <- as.numeric(epoch_val[1])
    }
  }
  # Fallback: calculate from timestamps if we have at least 2 rows
  if (epoch_length <= 0 && nrow(counts.data) > 1) {
    time_diff <- as.numeric(difftime(counts.data$timestamp[2], counts.data$timestamp[1], units = "secs"))
    if (!is.na(time_diff) && time_diff > 0) {
      epoch_length <- round(time_diff)
    }
  }
  if (is.na(epoch_length) || epoch_length <= 0) epoch_length <- 60

  if (axis_to_analyze == "axis1") {
    counts.for.analysis <- counts.data$axis1
  } else {
    counts.for.analysis <- vm(counts.data$axis1, counts.data$axis2, counts.data$axis3)
  }

  counts_for_wear <- counts.for.analysis
  if (!is.na(epoch_length) && epoch_length > 0 && epoch_length != 60) {
    counts_for_wear <- counts.for.analysis * (60 / epoch_length)
  }

  if (wear_time_algorithm == "choi") {
    wear.time <- wear.choi(counts_for_wear, epoch_length = epoch_length)
  } else if (wear_time_algorithm == "troiano") {
    wear.time <- wear.troiano(counts_for_wear, epoch_length = epoch_length)
  } else if (wear_time_algorithm == "CANHR2025") {
    wear.time <- wear.CANHR2025(counts_for_wear, epoch_length = epoch_length)
  }

  # Calculate wear time accounting for epoch length
  minutes_per_epoch <- epoch_length / 60
  wear.minutes <- sum(wear.time) * minutes_per_epoch
  wear.hours <- wear.minutes / 60
  wear.percent <- 100 * sum(wear.time) / length(wear.time)

  # Extract wear time periods (start/end timestamps for continuous wear)
  wear.time.periods <- get.wear.periods(wear.time, counts.data$timestamp, epoch_length = epoch_length)

  # Apply intensity classification based on algorithm
  # Convert counts to CPM for cutpoint analysis
  cpm.for.analysis <- to_cpm(counts.for.analysis, epoch_length)
  data.for.analysis <- cpm.for.analysis
  analysis_metric <- "CPM"

  # Apply selected cut-point algorithm
  intensity <- switch(intensity_algorithm,
      "freedson1998" = freedson(cpm.for.analysis),
      "CANHR" = CANHR.Cutpoints(cpm.for.analysis),
      "evenson" = evenson(cpm.for.analysis),
      "puyau" = puyau(cpm.for.analysis),
      "mattocks" = mattocks(cpm.for.analysis),
      "pate_preschool" = pate_preschool(cpm.for.analysis),
      "troiano" = troiano(cpm.for.analysis),
      "sasaki_vm3" = sasaki_vm3(cpm.for.analysis),
      "copeland_older" = copeland_older(cpm.for.analysis),
      # Default
      freedson(cpm.for.analysis)
    )

  valid.days.results <- valid.days(counts.data$timestamp, wear.time, min.wear.hours = min_wear_hours, epoch_length = epoch_length)

  intensity.summary <- intensity(intensity, wear.time, epoch_seconds = epoch_length)
  mvpa.minutes <- mvpa(intensity, wear.time, epoch_seconds = epoch_length)

  mets <- NULL
  mets_summary <- NULL
  ee_summary <- NULL
  kcal_per_epoch <- NULL

  if (calculate_mets) {
    mets <- calculate.mets(counts.data, algorithm = mets_algorithm,
                          subject_info = subject_info, epoch_length = epoch_length,
                          verbose = FALSE)

    body_mass <- extract.body.mass(subject_info)
    ee <- calculate.energy.expenditure(mets, body_mass, epoch_length = epoch_length)
    kcal_per_epoch <- ee$kcal_per_epoch

    mets_avg <- calculate.average.mets(mets, wear.time, counts.data$timestamp)
    ee_summary <- summarize.energy.expenditure(kcal_per_epoch, intensity, wear.time)

    mets_summary <- data.frame(
      average_mets = mets_avg$average_mets,
      total_kcal = ee$total_kcal,
      stringsAsFactors = FALSE
    )
  }

  # Create epoch data
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

  if (calculate_mets) {
    epoch.data$mets <- mets
    epoch.data$kcal <- kcal_per_epoch
  }

  # Sleep analysis
  sleep_results <- NULL
  if (!is.null(sleep_algorithm)) {
    if (output_summary) cat("Performing sleep analysis...\n")

    # Auto-select sleep algorithm
    if (sleep_algorithm == "auto") {
      sleep_algorithm <- "cole_kripke"
      if (output_summary) message("Using Cole-Kripke algorithm")
    }

    tryCatch({
      if (sleep_algorithm == "tudor_locke") {
        sleep_scores <- sleep.cole.kripke(counts.for.analysis, apply_rescoring = TRUE, epoch_length = epoch_length)
        sleep_periods <- sleep.tudor.locke(
          sleep.state = sleep_scores,
          timestamps = counts.data$timestamp,
          counts = counts.for.analysis,
          epoch_length = epoch_length
        )
        epoch.data$sleep <- sleep_scores

        sleep_results <- list(
          sleep = sleep_scores,
          periods = sleep_periods,
          algorithm = "Tudor-Locke",
          base_algorithm = "Cole-Kripke (1992)"
        )
      } else {
        sleep_scores <- switch(sleep_algorithm,
          "cole_kripke" = sleep.cole.kripke(counts.for.analysis, apply_rescoring = TRUE, epoch_length = epoch_length),
          "sadeh" = sleep.sadeh(counts.for.analysis, epoch_length = epoch_length),
          sleep.cole.kripke(counts.for.analysis, apply_rescoring = TRUE, epoch_length = epoch_length)
        )
        epoch.data$sleep <- sleep_scores

        sleep_results <- list(
          sleep = sleep_scores,
          algorithm = switch(sleep_algorithm,
            "cole_kripke" = "Cole-Kripke (1992)",
            "sadeh" = "Sadeh (1994)",
            "Unknown"
          )
        )
      }
    }, error = function(e) {
      if (output_summary) warning("Sleep analysis failed: ", e$message)
      sleep_results <<- NULL
    })
  }

  daily.stats <- valid.days.results$daily_summary

  # Vectorized calculation of daily intensity minutes
  wear.epochs <- epoch.data[epoch.data$wear_time, ]

  if (nrow(wear.epochs) > 0) {
    if (calculate_mets) {
      daily.intensity <- aggregate(
        cbind(sedentary = intensity == "sedentary",
              light = intensity == "light",
              moderate = intensity == "moderate",
              vigorous = intensity == "vigorous",
              very_vigorous = intensity == "very_vigorous",
              mvpa = intensity %in% c("moderate", "vigorous", "very_vigorous"),
              counts_used = counts_used,
              mets = mets,
              kcal = kcal) ~ date,
        data = wear.epochs,
        FUN = function(x) if (is.logical(x)) sum(x) else mean(x)
      )
    } else {
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
    }

    daily.stats <- merge(daily.stats, daily.intensity, by = "date", all.x = TRUE, sort = FALSE)

    # Convert epoch counts to minutes using epoch_length
    daily.stats$sedentary_min <- daily.stats$sedentary * minutes_per_epoch
    daily.stats$light_min <- daily.stats$light * minutes_per_epoch
    daily.stats$moderate_min <- daily.stats$moderate * minutes_per_epoch
    daily.stats$vigorous_min <- daily.stats$vigorous * minutes_per_epoch
    daily.stats$very_vigorous_min <- daily.stats$very_vigorous * minutes_per_epoch
    daily.stats$mvpa_min <- daily.stats$mvpa * minutes_per_epoch
    daily.stats$average_cpm <- daily.stats$counts_used

    # Remove intermediate columns
    daily.stats$sedentary <- NULL
    daily.stats$light <- NULL
    daily.stats$moderate <- NULL
    daily.stats$vigorous <- NULL
    daily.stats$very_vigorous <- NULL
    daily.stats$mvpa <- NULL
    daily.stats$counts_used <- NULL

    if (calculate_mets) {
      daily.stats$average_mets <- daily.stats$mets
      daily.stats$total_kcal <- daily.stats$kcal
      daily.stats$mets <- NULL
      daily.stats$kcal <- NULL
    }
  } else {
    daily.stats$sedentary_min <- NA
    daily.stats$light_min <- NA
    daily.stats$moderate_min <- NA
    daily.stats$vigorous_min <- NA
    daily.stats$very_vigorous_min <- NA
    daily.stats$mvpa_min <- NA
    daily.stats$average_cpm <- NA
    if (calculate_mets) {
      daily.stats$average_mets <- NA
      daily.stats$total_kcal <- NA
    }
  }

  overall.summary <- data.frame(
    total_days = nrow(daily.stats),
    valid_days = valid.days.results$n_valid_days,
    total_wear_minutes = wear.minutes,
    total_wear_hours = round(wear.hours, 2),
    average_wear_per_day = if (nrow(daily.stats) > 0) round(wear.hours / nrow(daily.stats), 2) else NA_real_,
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
    mvpa_percent = if (wear.minutes > 0) round(100 * mvpa.minutes / wear.minutes, 2) else 0,
    stringsAsFactors = FALSE
  )

  # Filter to valid days only for fragmentation and circadian analyses

  # Research-based best practices (Migueles et al., 2017; van Someren et al., 1999):
  # - Fragmentation metrics require valid days (>=10h wear) for reliable estimates
 # - Circadian metrics (L5, M10, IS, IV) need multiple complete days
  # - Minimum 3 valid days recommended; 4+ preferred for reliability

  valid_day_data <- epoch.data[epoch.data$is_valid_day, ]
  n_valid_days <- valid.days.results$n_valid_days

  fragmentation_results <- NULL
  sleep_exclusion_info <- NULL
  if (calculate_fragmentation) {
    if (output_summary) cat("Calculating sedentary fragmentation...\n")

    # Warn if insufficient valid days for reliable fragmentation metrics
    if (n_valid_days < 3) {
      if (output_summary) {
        warning("Only ", n_valid_days, " valid day(s) available. ",
                "Minimum 3 valid days recommended for reliable fragmentation metrics. ",
                "Results should be interpreted with caution.")
      }
    }

    if (n_valid_days > 0) {
      # Create sleep mask if sleep exclusion is requested
      # Per SBRN consensus: sedentary behavior = WAKING behavior only
      sleep_mask <- NULL

      if (exclude_sleep) {
        if (output_summary) cat("  Detecting sleep periods for exclusion (SBRN: sedentary = waking only)...\n")

        sleep_mask <- tryCatch({
          sleep_state <- sleep.cole.kripke(valid_day_data$axis1, apply_rescoring = TRUE, epoch_length = epoch_length)

          sleep_periods <- sleep.tudor.locke(
            sleep.state = sleep_state,
            timestamps = valid_day_data$timestamp,
            counts = valid_day_data$axis1,
            epoch_length = epoch_length
          )

          # Create mask from detected sleep PERIOD WINDOWS
          # Only exclude epochs within actual detected sleep periods, not epoch-by-epoch classification
          mask <- rep(FALSE, nrow(valid_day_data))

          if (!is.null(sleep_periods) && nrow(sleep_periods) > 0) {
            tz <- attr(valid_day_data$timestamp[1], "tzone")
            if (is.null(tz) || tz == "") tz <- "UTC"

            for (i in seq_len(nrow(sleep_periods))) {
              period_start <- as.POSIXct(sleep_periods$in_bed_time[i],
                                         format = "%Y-%m-%d %H:%M:%S", tz = tz)
              period_end <- as.POSIXct(sleep_periods$out_bed_time[i],
                                       format = "%Y-%m-%d %H:%M:%S", tz = tz)

              in_period <- valid_day_data$timestamp >= period_start &
                          valid_day_data$timestamp <= period_end
              mask[in_period] <- TRUE
            }

            n_periods <- nrow(sleep_periods)
            n_excluded <- sum(mask)
            hours_excluded <- round(n_excluded * epoch_length / 3600, 1)

            if (output_summary) {
              cat(sprintf("  Sleep exclusion: %d period(s), %d epochs (%.1f hours) excluded\n",
                          n_periods, n_excluded, hours_excluded))
            }

            # Store info for results
            sleep_exclusion_info <- list(
              periods_detected = n_periods,
              epochs_excluded = n_excluded,
              hours_excluded = hours_excluded,
              sleep_periods = sleep_periods
            )
          } else {
            if (output_summary) cat("  No sleep periods detected - analyzing all waking wear time\n")
          }

          mask
        }, error = function(e) {
          if (output_summary) warning("Sleep detection failed: ", e$message, ". Proceeding without sleep exclusion.")
          NULL
        })
      }

      fragmentation_results <- tryCatch({
        sedentary.fragmentation(
          intensity = valid_day_data$intensity,
          timestamps = valid_day_data$timestamp,
          wear_time = valid_day_data$wear_time,
          sleep_mask = sleep_mask,  # Exclude detected sleep periods
          epoch_length = epoch_length
        )
      }, error = function(e) {
        if (output_summary) warning("Fragmentation calculation failed: ", e$message)
        NULL
      })

      # Add sleep exclusion info to fragmentation results
      if (!is.null(fragmentation_results) && !is.null(sleep_exclusion_info)) {
        fragmentation_results$sleep_excluded <- TRUE
        fragmentation_results$sleep_exclusion_info <- sleep_exclusion_info
      } else if (!is.null(fragmentation_results)) {
        fragmentation_results$sleep_excluded <- FALSE
      }
    } else {
      if (output_summary) warning("No valid days available for fragmentation analysis")
    }
  }

  circadian_results <- NULL
  if (calculate_circadian) {
    if (output_summary) cat("Calculating circadian rhythm metrics...\n")

    # Warn if insufficient valid days for reliable circadian metrics
    # IS (interdaily stability) specifically requires multiple days
    if (n_valid_days < 3) {
      if (output_summary) {
        warning("Only ", n_valid_days, " valid day(s) available. ",
                "Minimum 3 valid days recommended for reliable circadian rhythm metrics. ",
                "IS (interdaily stability) requires multiple days to compute.")
      }
    }

    if (n_valid_days > 0) {
      tryCatch({
        circadian_results <- circadian.rhythm(
          counts = valid_day_data$axis1,
          timestamps = valid_day_data$timestamp,
          wear_time = valid_day_data$wear_time,
          epoch_length = epoch_length
        )
      }, error = function(e) {
        if (output_summary) warning("Circadian calculation failed: ", e$message)
        circadian_results <<- NULL
      })
    } else {
      if (output_summary) warning("No valid days available for circadian analysis")
    }
  }

  results <- list(
    epoch_data = epoch.data,
    daily_summary = daily.stats,
    overall_summary = overall.summary,
    intensity_summary = intensity.summary,
    valid_days = valid.days.results$valid_days,
    wear_time_periods = wear.time.periods,
    subject_info = subject_info,
    mets_summary = mets_summary,
    energy_expenditure_summary = ee_summary,
    fragmentation = fragmentation_results,
    circadian = circadian_results,
    sleep = sleep_results,
    parameters = list(
      file_path = agd_file_path,
      epoch_length = epoch_length,
      wear_time_algorithm = wear_time_algorithm,
      intensity_algorithm = intensity_algorithm,
      axis_analyzed = axis_to_analyze,
      analysis_metric = "CPM",
      min_wear_hours = min_wear_hours,
      calculate_mets = calculate_mets,
      mets_algorithm = if (calculate_mets) mets_algorithm else NA,
      sleep_algorithm = sleep_algorithm,
      participant_age = participant_age,
      calculate_fragmentation = calculate_fragmentation,
      calculate_circadian = calculate_circadian
    )
  )

  class(results) <- c("canhrActi_analysis", "list")

  if (output_summary) {
    cat("\nAnalysis Complete\n")
    cat("Valid days:", overall.summary$valid_days, "/", overall.summary$total_days, "\n")
    cat("Total wear time:", overall.summary$total_wear_hours, "hours\n")
    cat("Intensity algorithm:", intensity_algorithm, "\n")
    cat("MVPA:", overall.summary$mvpa_minutes, "minutes\n")
    if (calculate_mets && !is.null(mets_summary)) {
      cat("Average METs:", mets_summary$average_mets, "\n")
      cat("Total Energy Expenditure:", round(mets_summary$total_kcal, 1), "kcal\n")
    }
    if (calculate_fragmentation && !is.null(fragmentation_results)) {
      cat("Sedentary bouts:", fragmentation_results$total_bouts, "\n")
      cat("Breaks per sed hour:", fragmentation_results$breaks_per_sed_hour, "\n")
      cat("Alpha (power-law):", fragmentation_results$alpha, "\n")
      cat("Gini coefficient:", fragmentation_results$gini, "\n")
    }
    if (calculate_circadian && !is.null(circadian_results)) {
      cat("L5:", circadian_results$L5, "cpm at", circadian_results$L5_start, "\n")
      cat("M10:", circadian_results$M10, "cpm at", circadian_results$M10_start, "\n")
      cat("Relative Amplitude:", circadian_results$RA, "\n")
    }
    if (!is.null(sleep_results)) {
      cat("\nSleep Analysis (", sleep_results$algorithm, "):\n", sep = "")
      if (!is.null(sleep_results$metrics)) {
        cat("  Total Sleep Time:", round(sleep_results$metrics$total_sleep_time_min, 1), "min\n")
        cat("  Sleep Efficiency:", round(sleep_results$metrics$sleep_efficiency, 1), "%\n")
        cat("  WASO:", round(sleep_results$metrics$waso_min, 1), "min\n")
        cat("  Awakenings:", sleep_results$metrics$n_awakenings, "\n")
      }
    }
    cat("\n")
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
  cat("MVPA:", s$mvpa_minutes, "min (", s$mvpa_percent, "%)\n")
  cat("\n")
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

#' Analyze AGD File
#'
#' Alias for \code{\link{canhrActi}}.
#'
#' @param ... Arguments passed to \code{canhrActi()}
#' @export
analyze.agd.file <- function(...) {
  canhrActi(...)
}


extract.body.mass <- function(subject_info) {
  mass <- subject_info$mass

  if (is.null(mass) || is.na(mass) || mass <= 0) {
    return(70)
  }

  return(as.numeric(mass))
}
