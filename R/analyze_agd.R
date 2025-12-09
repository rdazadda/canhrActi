#' canhrActi: Analyze ActiGraph Files (AGD or GT3X)
#'
#' Main function for analyzing ActiGraph accelerometer files. Supports both
#' pre-processed .agd files and raw .gt3x files. Automatically handles
#' single files, multiple files, or entire folders.
#'
#' @param agd_file_path Character. Can be:
#'   \itemize{
#'     \item Single file path (e.g., "path/to/file.agd" or "path/to/file.gt3x")
#'     \item Vector of file paths (e.g., c("file1.agd", "file2.gt3x"))
#'     \item Folder path (will analyze all .agd and .gt3x files in folder)
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
#' @param output_summary Logical. Print detailed summary to console? (default: TRUE)
#' @param lfe_mode Logical. Enable Low Frequency Extension mode for GT3X files? (default: FALSE)
#' @param calculate_fragmentation Logical. Calculate sedentary fragmentation metrics? (default: TRUE)
#' @param calculate_circadian Logical. Calculate circadian rhythm metrics (L5, M10, IS, IV)? (default: TRUE)
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
#'     \item \code{circadian} - Circadian rhythm metrics L5, M10, IS, IV, RA, cosinor (if calculate_circadian = TRUE)
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
#' # Single .gt3x file (raw data)
#' results <- canhrActi("path/to/file.gt3x")
#'
#' # Multiple files (batch) - can mix .agd and .gt3x
#' results <- canhrActi(c("file1.agd", "file2.gt3x"))
#'
#' # Entire folder (analyzes all .agd and .gt3x files)
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
                              intensity_algorithm = c("freedson1998", "CANHR"),
                              min_wear_hours = 10,
                              axis_to_analyze = c("axis1", "vector_magnitude"),
                              calculate_mets = TRUE,
                              mets_algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                                 "hendelman.adult", "hendelman.lifestyle", "swartz",
                                                 "leenders", "yngve.treadmill", "yngve.overground",
                                                 "brooks.overground", "brooks.bm", "freedson.children"),
                              output_summary = TRUE,
                              lfe_mode = FALSE,
                              calculate_fragmentation = TRUE,
                              calculate_circadian = TRUE) {

  if ((length(agd_file_path) == 1 && dir.exists(agd_file_path)) || length(agd_file_path) > 1) {
    return(canhrActi.batch(agd_file_path, wear_time_algorithm, intensity_algorithm,
                           min_wear_hours, axis_to_analyze,
                           export = !output_summary, lfe_mode = lfe_mode,
                           calculate_mets = calculate_mets, mets_algorithm = mets_algorithm,
                           calculate_fragmentation = calculate_fragmentation,
                           calculate_circadian = calculate_circadian))
  }

  return(.canhrActi.single.internal(agd_file_path, wear_time_algorithm, intensity_algorithm,
                                    min_wear_hours, axis_to_analyze,
                                    output_summary, lfe_mode,
                                    calculate_mets, mets_algorithm,
                                    calculate_fragmentation, calculate_circadian))
}


.canhrActi.single.internal <- function(agd_file_path,
                                   wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                                   intensity_algorithm = c("freedson1998", "CANHR"),
                                   min_wear_hours = 10,
                                   axis_to_analyze = c("axis1", "vector_magnitude"),
                                   output_summary = TRUE,
                                   lfe_mode = FALSE,
                                   calculate_mets = TRUE,
                                   mets_algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                                      "hendelman.adult", "hendelman.lifestyle", "swartz",
                                                      "leenders", "yngve.treadmill", "yngve.overground",
                                                      "brooks.overground", "brooks.bm", "freedson.children"),
                                   calculate_fragmentation = TRUE,
                                   calculate_circadian = TRUE) {

  wear_time_algorithm <- match.arg(wear_time_algorithm)
  intensity_algorithm <- match.arg(intensity_algorithm)
  axis_to_analyze <- match.arg(axis_to_analyze)
  mets_algorithm <- match.arg(mets_algorithm)

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

  # Check file type - support multiple device formats
  file_ext <- tolower(tools::file_ext(agd_file_path))
  is_agd <- file_ext == "agd"
  is_gt3x <- file_ext == "gt3x"
  is_bin <- file_ext == "bin"      # GENEActiv
  is_cwa <- file_ext == "cwa"      # Axivity
  is_csv <- file_ext == "csv"      # Generic CSV

  supported_formats <- c("agd", "gt3x", "bin", "cwa", "csv")
  if (!file_ext %in% supported_formats) {
    stop("Unsupported file format: ", basename(agd_file_path), "\n",
         "Supported formats:\n",
         "  - .agd: ActiGraph pre-processed data (from ActiLife)\n",
         "  - .gt3x: ActiGraph raw acceleration data\n",
         "  - .bin: GENEActiv raw data (requires GENEAread package)\n",
         "  - .cwa: Axivity raw data (requires GGIRread package)\n",
         "  - .csv: Generic CSV with x, y, z columns")
  }

  if (min_wear_hours < 0 || min_wear_hours > 24) {
    stop("min_wear_hours must be between 0 and 24. You provided: ", min_wear_hours, "\n",
         "Common values: 10 hours (default), 8 hours (lenient), 12 hours (strict)")
  }

  # Device type descriptions
  device_descriptions <- c(
    agd = "ActiGraph .agd (pre-processed counts)",
    gt3x = "ActiGraph .gt3x (raw acceleration)",
    bin = "GENEActiv .bin (raw acceleration)",
    cwa = "Axivity .cwa (raw acceleration)",
    csv = "Generic CSV (raw acceleration)"
  )

  if (output_summary) {
    cat("\nAnalyzing:", basename(agd_file_path), "\n")
    cat("File type:", device_descriptions[file_ext], "\n")
  }

  # Read file based on type - unified multi-device support
  if (is_agd) {
    # ActiGraph AGD - pre-processed counts
    agd.data <- read.agd(agd_file_path)
  } else if (is_gt3x) {
    # ActiGraph GT3X - raw data with built-in counts conversion
    agd.data <- read.gt3x.file(agd_file_path, epoch_length = 60, lfe_mode = lfe_mode, verbose = output_summary)
  } else {
    # Other devices (GENEActiv, Axivity, CSV) - use unified reader
    if (output_summary) cat("Using unified accelerometer reader...\n")
    accel_data <- read.accelerometer(agd_file_path, epoch_length = 60, verbose = output_summary)

    # Convert to canhrActi standard format
    agd.data <- list(
      data = data.frame(
        timestamp = accel_data$data$timestamp,
        axis1 = accel_data$data$axis1,
        axis2 = accel_data$data$axis2,
        axis3 = accel_data$data$axis3,
        steps = if ("steps" %in% names(accel_data$data)) accel_data$data$steps else 0,
        stringsAsFactors = FALSE
      ),
      settings = accel_data$settings
    )
  }

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

  # Extract wear time periods (start/end timestamps for continuous wear)
  wear.time.periods <- get.wear.periods(wear.time, counts.data$timestamp, epoch_length = epoch_length)

  # Convert counts to CPM for cutpoint analysis
  # Freedson and other cutpoints are calibrated for 60-second epochs
  cpm.for.analysis <- to_cpm(counts.for.analysis, epoch_length)

  if (intensity_algorithm == "freedson1998") {
    intensity <- freedson(cpm.for.analysis)
  } else if (intensity_algorithm == "CANHR") {
    intensity <- CANHR.Cutpoints(cpm.for.analysis)
  } else {
    intensity <- CANHR.Cutpoints(cpm.for.analysis)
  }

  valid.days.results <- valid.days(counts.data$timestamp, wear.time, min.wear.hours = min_wear_hours)

  intensity.summary <- intensity(intensity, wear.time)
  mvpa.minutes <- mvpa(intensity, wear.time)

  mets <- NULL
  mets_summary <- NULL
  ee_summary <- NULL
  kcal_per_epoch <- NULL

  if (calculate_mets) {
    mets <- calculate.mets(counts.data, algorithm = mets_algorithm,
                          subject_info = subject_info, verbose = FALSE)

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
    names(daily.stats)[names(daily.stats) == "sedentary"] <- "sedentary_min"
    names(daily.stats)[names(daily.stats) == "light"] <- "light_min"
    names(daily.stats)[names(daily.stats) == "moderate"] <- "moderate_min"
    names(daily.stats)[names(daily.stats) == "vigorous"] <- "vigorous_min"
    names(daily.stats)[names(daily.stats) == "very_vigorous"] <- "very_vigorous_min"
    names(daily.stats)[names(daily.stats) == "mvpa"] <- "mvpa_min"
    names(daily.stats)[names(daily.stats) == "counts_used"] <- "average_cpm"
    if (calculate_mets) {
      names(daily.stats)[names(daily.stats) == "mets"] <- "average_mets"
      names(daily.stats)[names(daily.stats) == "kcal"] <- "total_kcal"
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
      tryCatch({
        fragmentation_results <- sedentary.fragmentation(
          intensity = valid_day_data$intensity,
          timestamps = valid_day_data$timestamp,
          wear_time = valid_day_data$wear_time,
          epoch_length = epoch_length
        )
      }, error = function(e) {
        if (output_summary) warning("Fragmentation calculation failed: ", e$message)
        fragmentation_results <<- NULL
      })
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
    parameters = list(
      file_path = agd_file_path,
      epoch_length = epoch_length,
      wear_time_algorithm = wear_time_algorithm,
      intensity_algorithm = intensity_algorithm,
      axis_analyzed = axis_to_analyze,
      min_wear_hours = min_wear_hours,
      calculate_mets = calculate_mets,
      mets_algorithm = if (calculate_mets) mets_algorithm else NA,
      calculate_fragmentation = calculate_fragmentation,
      calculate_circadian = calculate_circadian
    )
  )

  class(results) <- c("canhrActi_analysis", "list")

  if (output_summary) {
    cat("\nAnalysis Complete\n")
    cat("Valid days:", overall.summary$valid_days, "/", overall.summary$total_days, "\n")
    cat("Total wear time:", overall.summary$total_wear_hours, "hours\n")
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


extract.body.mass <- function(subject_info) {
  mass <- subject_info$mass

  if (is.null(mass) || is.na(mass) || mass <= 0) {
    return(70)
  }

  return(as.numeric(mass))
}
