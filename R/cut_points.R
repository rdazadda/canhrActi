#' @title Activity Intensity Cut-Points Library
#'
#' @description
#' Validated cut-points for activity intensity classification.
#' Includes algorithms for adults, children, and older adults from ActiGraph
#' and triaxial/vector magnitude approaches.
#'
#' @name cut-points
NULL


# UTILITY FUNCTIONS

#' Convert Counts Per Epoch to Counts Per Minute (CPM)
#'
#' Converts activity counts from any epoch length to standardized counts per minute.
#' This is essential for applying cutpoints which are defined for 60-second epochs.
#'
#' @param counts Numeric vector of counts per epoch
#' @param epoch_length Epoch length in seconds (e.g., 30, 60, 10)
#'
#' @return Numeric vector of counts per minute (CPM)
#'
#' @details
#' The Freedson and other cutpoint algorithms are calibrated for 60-second epochs.
#' When using data collected at different epoch lengths, counts must be converted
#' to CPM before applying cutpoints:
#' \itemize{
#'   \item 60-second epochs: CPM = counts (no conversion needed)
#'   \item 30-second epochs: CPM = counts * 2
#'   \item 15-second epochs: CPM = counts * 4
#'   \item 10-second epochs: CPM = counts * 6
#' }
#'
#' @examples
#' # 30-second epoch data
#' counts_30sec <- c(500, 1000, 2500)
#' cpm <- to_cpm(counts_30sec, epoch_length = 30)
#' # Returns: 1000, 2000, 5000
#'
#' @export
to_cpm <- function(counts, epoch_length = 60) {
  if (!is.numeric(counts)) stop("counts must be numeric")
  if (!is.numeric(epoch_length) || epoch_length <= 0) {
    stop("epoch_length must be a positive number")
  }
  counts * (60 / epoch_length)
}


#' Warn About Negative Activity Counts (internal helper)
#'
#' Centralizes the negative-count contract shared by all cut-point algorithms so
#' that every function warns consistently. Valid accelerometer data never has
#' negative counts; when present they are treated as sedentary (count-threshold
#' algorithms) via the lowest comparison, so this helper only surfaces a warning.
#'
#' @param counts Numeric vector of counts (CPM or VM CPM)
#' @param algorithm Character name of the calling algorithm (for the message)
#'
#' @return Invisibly returns \code{counts} unchanged.
#'
#' @keywords internal
#' @noRd
.warn_negative_counts <- function(counts, algorithm = "cut-point") {
  neg_counts <- sum(counts < 0, na.rm = TRUE)
  if (neg_counts > 0) {
    warning("Found ", neg_counts, " negative count values in ", algorithm, ". ",
            "Negative counts are invalid and will be classified as sedentary.")
  }
  invisible(counts)
}


# ADULT CUT-POINTS (COUNT-BASED)

#' Apply Freedson Adult (1998) Cut Points
#'
#' Classifies activity counts into intensity categories using the Freedson Adult
#' (1998) cutpoints. This is the most widely used adult cut-point set.
#'
#' @param counts_per_minute Numeric vector of counts per minute (CPM).
#'   If your data is not in 60-second epochs, use \code{\link{to_cpm}} to convert first.
#'
#' @return Ordered factor with levels: sedentary, light, moderate, vigorous, very_vigorous
#'
#' @details
#' The Freedson Adult (1998) cutpoints are based on treadmill validation studies:
#' \itemize{
#'   \item Sedentary: 0-99 CPM (< 1.5 METs)
#'   \item Light: 100-1951 CPM (1.5-2.99 METs)
#'   \item Moderate: 1952-5724 CPM (3.0-5.99 METs)
#'   \item Vigorous: 5725-9498 CPM (6.0-8.99 METs)
#'   \item Very Vigorous: >= 9499 CPM (>= 9.0 METs)
#' }
#'
#' @references
#' Freedson PS, Melanson E, Sirard J. Calibration of the Computer Science and
#' Applications, Inc. accelerometer. Med Sci Sports Exerc. 1998;30(5):777-781.
#'
#' @examples
#' # 60-second epoch data (counts = CPM)
#' counts <- c(50, 500, 2000, 6000, 10000)
#' intensity <- freedson(counts)
#'
#' @seealso \code{\link{to_cpm}} for epoch conversion
#' @family adult cut-points
#'
#' @export
freedson <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  # Warn about negative counts (should not occur in valid accelerometer data)
  .warn_negative_counts(counts_per_minute, "freedson")

  # Freedson (1998) cut-points: 0-99 sedentary, 100-1951 light, 1952-5724 moderate,
  # 5725-9498 vigorous, 9499+ very vigorous
  # Reference: Freedson PS et al. (1998). Med Sci Sports Exerc. 30(5):777-781
  intensity[counts_per_minute < 100]   <- "sedentary"
  intensity[counts_per_minute >= 100 & counts_per_minute <= 1951] <- "light"
  intensity[counts_per_minute >= 1952 & counts_per_minute <= 5724] <- "moderate"
  intensity[counts_per_minute >= 5725 & counts_per_minute <= 9498] <- "vigorous"
  intensity[counts_per_minute >= 9499] <- "very_vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
         ordered = TRUE)
}


#' Apply Troiano (2008) Cut Points
#'
#' Adult cut-points derived from NHANES accelerometer data analysis.
#' Slightly lower moderate threshold than Freedson, making it more sensitive.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Troiano (2008) cutpoints:
#' \itemize{
#'   \item Sedentary: 0-99 CPM
#'   \item Light: 100-2019 CPM
#'   \item Moderate: 2020-5998 CPM
#'   \item Vigorous: >= 5999 CPM
#' }
#'
#' @references
#' Troiano RP, et al. (2008). Physical activity in the United States measured
#' by accelerometer. Med Sci Sports Exerc, 40(1), 181-188.
#'
#' @family adult cut-points
#' @export
troiano <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "troiano")

  intensity[counts_per_minute < 100]   <- "sedentary"
  intensity[counts_per_minute >= 100 & counts_per_minute < 2020] <- "light"
  intensity[counts_per_minute >= 2020 & counts_per_minute < 5999] <- "moderate"
  intensity[counts_per_minute >= 5999] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Matthews (2005) Cut Points
#'
#' Adult cut-points from NHANES III data, validated against doubly labeled water.
#' Commonly used in epidemiological studies.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Matthews (2005) cutpoints:
#' \itemize{
#'   \item Sedentary: 0-99 CPM (< 1.5 METs)
#'   \item Light: 100-759 CPM (1.5-2.9 METs)
#'   \item Lifestyle: 760-1951 CPM (3.0-3.9 METs)
#'   \item Moderate: 1952-5724 CPM (4.0-5.9 METs)
#'   \item Vigorous: >= 5725 CPM (>= 6.0 METs)
#' }
#'
#' @references
#' Matthews CE, et al. (2005). Sources of variance in daily physical activity
#' levels as measured by an accelerometer. Med Sci Sports Exerc, 37(2), 290-298.
#'
#' @family adult cut-points
#' @export
matthews <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "matthews")

  intensity[counts_per_minute < 100]   <- "sedentary"
  intensity[counts_per_minute >= 100 & counts_per_minute < 760] <- "light"
  intensity[counts_per_minute >= 760 & counts_per_minute < 1952] <- "lifestyle"
  intensity[counts_per_minute >= 1952 & counts_per_minute < 5725] <- "moderate"
  intensity[counts_per_minute >= 5725] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "lifestyle", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Santos-Lozano Adult (2013) Cut Points
#'
#' Adult cut-points validated against indirect calorimetry in a Spanish population.
#' Provides separate thresholds for younger and older adults.
#'
#' @param counts_per_minute Numeric vector of CPM
#' @param age_group Character: "younger" (18-64) or "older" (65+)
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Santos-Lozano (2013) cutpoints for younger adults (18-64):
#' \itemize{
#'   \item Sedentary: 0-99 CPM
#'   \item Light: 100-3207 CPM
#'   \item Moderate: 3208-8564 CPM
#'   \item Vigorous: >= 8565 CPM
#' }
#'
#' For older adults (65+):
#' \itemize{
#'   \item Sedentary: 0-99 CPM
#'   \item Light: 100-2750 CPM
#'   \item Moderate: 2751-9358 CPM
#'   \item Vigorous: >= 9359 CPM
#' }
#'
#' @references
#' Santos-Lozano A, et al. (2013). Validation and determination
#' of physical activity intensity cut points. Int J Sports Med, 34(11), 975-982.
#'
#' @family adult cut-points
#' @export
santos_lozano <- function(counts_per_minute, age_group = c("younger", "older")) {
  age_group <- match.arg(age_group)
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "santos_lozano")

  if (age_group == "younger") {
    intensity[counts_per_minute < 100]   <- "sedentary"
    intensity[counts_per_minute >= 100 & counts_per_minute < 3208] <- "light"
    intensity[counts_per_minute >= 3208 & counts_per_minute < 8565] <- "moderate"
    intensity[counts_per_minute >= 8565] <- "vigorous"
  } else {
    intensity[counts_per_minute < 100]   <- "sedentary"
    intensity[counts_per_minute >= 100 & counts_per_minute < 2751] <- "light"
    intensity[counts_per_minute >= 2751 & counts_per_minute < 9359] <- "moderate"
    intensity[counts_per_minute >= 9359] <- "vigorous"
  }
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Crouter 2-Regression (2006) Cut Points
#'
#' Adult cut-points using a refined two-regression model that improves accuracy
#' during intermittent activity by using coefficient of variation.
#'
#' @param counts_per_minute Numeric vector of CPM
#' @param cv Numeric vector of coefficient of variation for each epoch (optional)
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' When CV is provided, the Crouter 2-regression model uses:
#' \itemize{
#'   \item CV < 10: METs = 2.379833 * exp(0.00013529 * CPM)
#'   \item CV >= 10: METs = 2.330519 + (0.001646 * CPM) - (1.2017e-7 * CPM^2)
#' }
#'
#' Without CV (the default in this package's pipeline), a NON-VALIDATED count
#' threshold approximation is used instead of the published 2-regression model:
#' \itemize{
#'   \item Sedentary: 0-50 CPM
#'   \item Light: 51-1040 CPM (walking threshold)
#'   \item Moderate: 1041-5724 CPM
#'   \item Vigorous: >= 5725 CPM
#' }
#' \strong{Warning:} These single-cut count thresholds are ad-hoc and are NOT part
#' of the Crouter, Clowers & Bassett (2006) two-regression method, which is a
#' CV-driven MET-prediction model and does not define single count cut-points.
#' The 1040 value is the Crouter walk/run separation count and 5724/5725 are
#' borrowed from Freedson (1998). To run the published 2-regression model you
#' must supply the per-epoch \code{cv} argument.
#'
#' @references
#' Crouter SE, Clowers KG, Bassett DR Jr. (2006). A novel method for using
#' accelerometer data to predict energy expenditure. J Appl Physiol,
#' 100(4), 1324-1331.
#'
#' @family adult cut-points
#' @export
crouter <- function(counts_per_minute, cv = NULL) {
  n <- length(counts_per_minute)

  .warn_negative_counts(counts_per_minute, "crouter")

  if (!is.null(cv)) {
    # Use 2-regression model.
    # Clamp negative CPM to 0 so invalid inputs cannot be pushed into a
    # non-sedentary class by the exp()/polynomial MET equations below.
    cpm_clamped <- pmax(counts_per_minute, 0)
    mets <- numeric(n)

    # Low CV (continuous walking)
    low_cv <- !is.na(cv) & cv < 10
    mets[low_cv] <- 2.379833 * exp(0.00013529 * cpm_clamped[low_cv])

    # High CV (intermittent activity / lifestyle)
    # Reference: Crouter SE, Clowers KG, Bassett DR Jr. (2006). J Appl Physiol, 100(4):1324-1331
    # Lifestyle equation (Crouter 2006, full 4-term cubic):
    #   METs = 2.330519 + 0.001646*CPM - 1.2017e-7*CPM^2 + 3.3779e-12*CPM^3
    high_cv <- !is.na(cv) & cv >= 10
    cpm_high <- cpm_clamped[high_cv]
    mets[high_cv] <- 2.330519 + (0.001646 * cpm_high) -
      (1.2017e-7 * cpm_high^2) + (3.3779e-12 * cpm_high^3)

    # Convert METs to intensity
    intensity <- character(n)
    intensity[mets < 1.5] <- "sedentary"
    intensity[mets >= 1.5 & mets < 3] <- "light"
    intensity[mets >= 3 & mets < 6] <- "moderate"
    intensity[mets >= 6] <- "vigorous"

  } else {
    # Standard thresholds
    intensity <- character(n)
    intensity[counts_per_minute <= 50] <- "sedentary"
    intensity[counts_per_minute > 50 & counts_per_minute <= 1040] <- "light"
    intensity[counts_per_minute > 1040 & counts_per_minute < 5725] <- "moderate"
    intensity[counts_per_minute >= 5725] <- "vigorous"
  }

  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


# TRIAXIAL / VECTOR MAGNITUDE CUT-POINTS

#' Apply Sasaki VM3 (2011) Cut Points
#'
#' Triaxial cut-points using Vector Magnitude (VM) counts from all three axes.
#' More accurate than uniaxial for capturing multi-planar movement.
#'
#' @param vm_counts_per_minute Numeric vector of vector magnitude CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Vector Magnitude is calculated as: VM = sqrt(x^2 + y^2 + z^2)
#'
#' Sasaki (2011) VM cutpoints:
#' \itemize{
#'   \item Sedentary: 0-199 VM CPM
#'   \item Light: 200-2689 VM CPM
#'   \item Moderate: 2690-6166 VM CPM
#'   \item Vigorous: 6167-9642 VM CPM
#'   \item Very Vigorous: >= 9643 VM CPM
#' }
#'
#' @references
#' Sasaki JE, et al. (2011). Validation and comparison of ActiGraph activity
#' monitors. J Sci Med Sport, 14(5), 411-416.
#'
#' @family triaxial cut-points
#' @export
sasaki_vm3 <- function(vm_counts_per_minute) {
  n <- length(vm_counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(vm_counts_per_minute, "sasaki_vm3")

  # Sasaki et al. (2011) VM cut-points:
  # Sedentary: 0-199, Light: 200-2689, Moderate: 2690-6166, Vigorous: 6167-9642, Very Vigorous: >=9643
  # FIXED: Changed 2691 to 2690 to match published thresholds exactly
  intensity[vm_counts_per_minute < 200]   <- "sedentary"
  intensity[vm_counts_per_minute >= 200 & vm_counts_per_minute < 2690] <- "light"
  intensity[vm_counts_per_minute >= 2690 & vm_counts_per_minute < 6167] <- "moderate"
  intensity[vm_counts_per_minute >= 6167 & vm_counts_per_minute < 9643] <- "vigorous"
  intensity[vm_counts_per_minute >= 9643] <- "very_vigorous"
  intensity[is.na(vm_counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
         ordered = TRUE)
}


#' Apply Freedson VM3 (2011) Cut Points
#'
#' Triaxial cut-points from Freedson's Vector Magnitude validation study.
#' Alternative to Sasaki VM3 with slightly different thresholds.
#'
#' @param vm_counts_per_minute Numeric vector of vector magnitude CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Freedson VM3 (2011) cutpoints:
#' \itemize{
#'   \item Sedentary: 0-99 VM CPM
#'   \item Light: 100-2452 VM CPM
#'   \item Moderate: 2453-6891 VM CPM
#'   \item Vigorous: >= 6892 VM CPM
#' }
#'
#' @references
#' Freedson PS, et al. (2011). Evaluation of artificial neural network algorithms
#' for predicting METs and activity type from accelerometer data. J Sci Med Sport.
#'
#' @family triaxial cut-points
#' @export
freedson_vm3 <- function(vm_counts_per_minute) {
  n <- length(vm_counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(vm_counts_per_minute, "freedson_vm3")

  intensity[vm_counts_per_minute < 100]   <- "sedentary"
  intensity[vm_counts_per_minute >= 100 & vm_counts_per_minute < 2453] <- "light"
  intensity[vm_counts_per_minute >= 2453 & vm_counts_per_minute < 6892] <- "moderate"
  intensity[vm_counts_per_minute >= 6892] <- "vigorous"
  intensity[is.na(vm_counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


# CHILDREN CUT-POINTS

#' Apply Evenson (2008) Children Cut Points
#'
#' Cut-points for children (5-8 years) validated against indirect calorimetry.
#' Recommended by systematic reviews as most accurate for children.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Evenson (2008) cutpoints for children:
#' \itemize{
#'   \item Sedentary: 0-100 CPM (< 1.5 METs)
#'   \item Light: 101-2295 CPM (1.5-3.99 METs)
#'   \item Moderate: 2296-4011 CPM (4.0-6.99 METs)
#'   \item Vigorous: >= 4012 CPM (>= 7.0 METs)
#' }
#'
#' @references
#' Evenson KR, et al. (2008). Calibration of two objective measures of physical
#' activity for children. J Sports Sci, 26(14), 1557-1565.
#'
#' Trost SG, et al. (2011). Comparison of accelerometer cut points for
#' predicting activity intensity in youth. Med Sci Sports Exerc, 43(7), 1360-1368.
#'
#' @family children cut-points
#' @export
evenson <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "evenson")

  intensity[counts_per_minute <= 100] <- "sedentary"
  intensity[counts_per_minute > 100 & counts_per_minute <= 2295] <- "light"
  intensity[counts_per_minute > 2295 & counts_per_minute <= 4011] <- "moderate"
  intensity[counts_per_minute > 4011] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Puyau (2002) Children Cut Points
#'
#' Cut-points for children validated using room calorimetry.
#' Lower thresholds than Evenson, may overestimate MVPA.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Puyau (2002) cutpoints:
#' \itemize{
#'   \item Sedentary: 0-799 CPM
#'   \item Light: 800-3199 CPM
#'   \item Moderate: 3200-8199 CPM
#'   \item Vigorous: >= 8200 CPM
#' }
#'
#' @references
#' Puyau MR, et al. (2002). Validation and calibration of physical activity
#' monitors in children. Obes Res, 10(3), 150-157.
#'
#' @family children cut-points
#' @export
puyau <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "puyau")

  intensity[counts_per_minute < 800] <- "sedentary"
  intensity[counts_per_minute >= 800 & counts_per_minute < 3200] <- "light"
  intensity[counts_per_minute >= 3200 & counts_per_minute < 8200] <- "moderate"
  intensity[counts_per_minute >= 8200] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Mattocks (2007) Children Cut Points
#'
#' Cut-points for children (12 years) validated in the ALSPAC cohort study.
#' Higher moderate threshold than Evenson.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Mattocks (2007) cutpoints:
#' \itemize{
#'   \item Sedentary: 0-100 CPM
#'   \item Light: 101-3580 CPM
#'   \item Moderate: 3581-6129 CPM
#'   \item Vigorous: >= 6130 CPM
#' }
#'
#' @references
#' Mattocks C, et al. (2007). Calibration of an accelerometer during free-living
#' activities in children. Int J Pediatr Obes, 2(4), 218-226.
#'
#' @family children cut-points
#' @export
mattocks <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "mattocks")

  intensity[counts_per_minute <= 100] <- "sedentary"
  intensity[counts_per_minute > 100 & counts_per_minute <= 3580] <- "light"
  intensity[counts_per_minute > 3580 & counts_per_minute <= 6129] <- "moderate"
  intensity[counts_per_minute > 6129] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Pate (2006) Preschooler Cut Points
#'
#' Cut-points specifically validated for preschool-aged children (3-5 years).
#' Uses lower thresholds appropriate for young children.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Pate (2006) cutpoints for preschoolers:
#' \itemize{
#'   \item Sedentary: 0-799 CPM
#'   \item Light: 800-1679 CPM
#'   \item Moderate: 1680-3367 CPM
#'   \item Vigorous: >= 3368 CPM
#' }
#'
#' @references
#' Pate RR, et al. (2006). Validation and calibration of an accelerometer in
#' preschool children. Obesity, 14(11), 2000-2006.
#'
#' @family children cut-points
#' @export
pate_preschool <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "pate_preschool")

  intensity[counts_per_minute < 800] <- "sedentary"
  intensity[counts_per_minute >= 800 & counts_per_minute < 1680] <- "light"
  intensity[counts_per_minute >= 1680 & counts_per_minute < 3368] <- "moderate"
  intensity[counts_per_minute >= 3368] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Butte (2014) Preschooler Cut Points
#'
#' Cut-points for preschoolers validated against room calorimetry in the
#' VIVA project. Provides age-specific thresholds.
#'
#' @param counts_per_minute Numeric vector of CPM
#' @param age Age in years (3-6)
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Butte (2014) provides age-specific regression equations. This implementation
#' is a COUNT-THRESHOLD APPROXIMATION of those equations: it varies only the
#' single MVPA cut by age and applies fixed sedentary/light cuts for all ages.
#' Approximate cutpoints:
#' Age 3-4:
#' \itemize{
#'   \item Sedentary: 0-239 CPM
#'   \item Light: 240-2119 CPM
#'   \item Moderate-Vigorous: >= 2120 CPM
#' }
#' Age 5-6:
#' \itemize{
#'   \item Sedentary: 0-239 CPM
#'   \item Light: 240-2295 CPM
#'   \item Moderate-Vigorous: >= 2296 CPM
#' }
#'
#' @references
#' Butte NF, Wong WW, Lee JS, Adolph AL, Puyau MR, Zakeri IF. (2014). Prediction
#' of energy expenditure and physical activity in preschoolers. Med Sci Sports
#' Exerc, 46(7), 1216-1226.
#'
#' @family children cut-points
#' @export
butte_preschool <- function(counts_per_minute, age = 4) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "butte_preschool")

  # Age-dependent moderate threshold
  mvpa_threshold <- if (age < 5) 2120 else 2296

  intensity[counts_per_minute < 240] <- "sedentary"
  intensity[counts_per_minute >= 240 & counts_per_minute < mvpa_threshold] <- "light"
  intensity[counts_per_minute >= mvpa_threshold] <- "mvpa"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "mvpa"),
         ordered = TRUE)
}


#' Apply Chandler (2016) Children Cut Points
#'
#' Cut-points for children validated with indirect calorimetry using a
#' wrist-worn accelerometer. Specifically developed for 8-12 year olds.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Chandler (2016) cutpoints for 8-12 year-old children, wrist-worn (15-sec epochs):
#' \itemize{
#'   \item Sedentary: 0-5 counts/15s (0-20 CPM)
#'   \item Light: 6-404 counts/15s (21-1616 CPM)
#'   \item Moderate: 405-810 counts/15s (1617-3240 CPM)
#'   \item Vigorous: >= 811 counts/15s (>= 3241 CPM)
#' }
#'
#' Note: Values shown are converted to CPM from original 15-second epoch values.
#'
#' @references
#' Chandler JL, et al. (2016). Classification of physical activity intensities
#' using a wrist-worn accelerometer in 8-12-year-old children.
#' Pediatr Obes, 11(2), 120-127.
#'
#' @family children cut-points
#' @export
chandler <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "chandler")

  intensity[counts_per_minute <= 20] <- "sedentary"
  intensity[counts_per_minute > 20 & counts_per_minute <= 1616] <- "light"
  intensity[counts_per_minute > 1616 & counts_per_minute <= 3240] <- "moderate"
  intensity[counts_per_minute > 3240] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Copeland (2009) Children Cut Points
#'
#' Cut-points for children (13-14 years) validated in free-living conditions.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Copeland (2009) children cutpoints:
#' \itemize{
#'   \item Sedentary: 0-100 CPM
#'   \item Light: 101-2220 CPM
#'   \item Moderate: 2221-6130 CPM
#'   \item Vigorous: >= 6131 CPM
#' }
#'
#' @references
#' Source citation requires confirmation. The 0/100/2220/6130 CPM thresholds
#' implemented here are child-magnitude cut-points and do NOT correspond to the
#' older-adult paper (Copeland JL, Esliger DW. (2009). Accelerometer assessment
#' of physical activity in active, healthy older adults. J Aging Phys Act,
#' 17(1), 17-30) that was previously (and incorrectly) cited here; that paper is
#' the source for \code{\link{copeland_older}}, not this children function. The
#' validation study for these specific child thresholds has not been confirmed.
#'
#' @family children cut-points
#' @export
copeland <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "copeland")

  intensity[counts_per_minute <= 100] <- "sedentary"
  intensity[counts_per_minute > 100 & counts_per_minute <= 2220] <- "light"
  intensity[counts_per_minute > 2220 & counts_per_minute <= 6130] <- "moderate"
  intensity[counts_per_minute > 6130] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


#' Apply Romanzini (2014) Youth Cut Points
#'
#' Cut-points for youth validated in Brazilian adolescents against indirect
#' calorimetry during various activities.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Romanzini (2014) cutpoints:
#' \itemize{
#'   \item Sedentary: 0-180 CPM
#'   \item Light: 181-756 CPM
#'   \item Moderate: 757-1111 CPM
#'   \item Vigorous: >= 1112 CPM
#' }
#'
#' Note: These thresholds are notably lower than other cut-points.
#'
#' @references
#' Romanzini M, et al. (2014). Calibration of accelerometers
#' in adolescents. Eur J Sport Sci, 14(1), 91-99.
#'
#' @family children cut-points
#' @export
romanzini <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "romanzini")

  intensity[counts_per_minute <= 180] <- "sedentary"
  intensity[counts_per_minute > 180 & counts_per_minute <= 756] <- "light"
  intensity[counts_per_minute > 756 & counts_per_minute <= 1111] <- "moderate"
  intensity[counts_per_minute > 1111] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


# OLDER ADULTS CUT-POINTS

#' Apply Copeland (2009) Older Adult Cut Points
#'
#' Cut-points specifically validated for healthy older adults (65+).
#' Lower thresholds account for reduced movement efficiency with age.
#'
#' @param counts_per_minute Numeric vector of CPM
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Copeland (2009) cutpoints for older adults:
#' \itemize{
#'   \item Sedentary: 0-99 CPM
#'   \item Light: 100-1040 CPM
#'   \item Moderate: 1041-1800 CPM (lower than adults)
#'   \item Vigorous: >= 1801 CPM
#' }
#'
#' Note: The moderate threshold is substantially lower than adult cut-points
#' to account for age-related changes in movement patterns.
#'
#' @references
#' Copeland JL, Esliger DW. (2009). Accelerometer assessment of physical
#' activity in active, healthy older adults. J Aging Phys Act, 17(1), 17-30.
#'
#' @family older adult cut-points
#' @export
copeland_older <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "copeland_older")

  intensity[counts_per_minute < 100] <- "sedentary"
  intensity[counts_per_minute >= 100 & counts_per_minute <= 1040] <- "light"
  intensity[counts_per_minute > 1040 & counts_per_minute <= 1800] <- "moderate"
  intensity[counts_per_minute > 1800] <- "vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous"),
         ordered = TRUE)
}


# CUSTOM CUT-POINTS

#' Apply CANHR Custom Cut Points
#'
#' Custom cut-points developed by the Center for Alaska Native Health Research.
#'
#' @param counts_per_minute Numeric vector of counts per minute
#'
#' @return Ordered factor with intensity levels
#'
#' @export
CANHR.Cutpoints <- function(counts_per_minute) {
  n <- length(counts_per_minute)
  intensity <- character(n)

  .warn_negative_counts(counts_per_minute, "CANHR")

  intensity[counts_per_minute <= 150] <- "sedentary"
  intensity[counts_per_minute > 150 & counts_per_minute <= 2200] <- "light"
  intensity[counts_per_minute > 2200 & counts_per_minute <= 6000] <- "moderate"
  intensity[counts_per_minute > 6000 & counts_per_minute <= 10000] <- "vigorous"
  intensity[counts_per_minute > 10000] <- "very_vigorous"
  intensity[is.na(counts_per_minute)] <- NA_character_

  factor(intensity,
         levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
         ordered = TRUE)
}


#' Apply Custom Cut Points
#'
#' Apply user-defined cut-point thresholds for intensity classification.
#'
#' @param data Numeric vector of activity metric (CPM, VM CPM, etc.)
#' @param thresholds Named numeric vector of thresholds, e.g.,
#'   c(light = 100, moderate = 2020, vigorous = 5999)
#' @param labels Character vector of intensity labels (default: derived from thresholds)
#'
#' @return Ordered factor with intensity levels
#'
#' @examples
#' \dontrun{
#' # Custom thresholds
#' my_thresholds <- c(light = 150, moderate = 2500, vigorous = 6000)
#' intensity <- custom_cutpoints(cpm, thresholds = my_thresholds)
#' }
#'
#' @export
custom_cutpoints <- function(data,
                              thresholds,
                              labels = NULL) {

  if (is.null(labels)) {
    if (is.null(names(thresholds)) || any(names(thresholds) == "")) {
      stop("When 'labels' is NULL, 'thresholds' must be a fully named numeric ",
           "vector (e.g., c(light = 100, moderate = 2020, vigorous = 5999)).")
    }
    labels <- c("sedentary", names(thresholds))
  }

  # Each threshold opens a new category above the base level, so the number of
  # labels must be exactly one more than the number of thresholds; otherwise the
  # classification loop would index out of range and produce NA factor levels.
  stopifnot(length(labels) == length(thresholds) + 1)

  .warn_negative_counts(data, "custom_cutpoints")

  thresholds <- sort(thresholds)
  n <- length(data)
  intensity <- character(n)

  # First level (below lowest threshold)
  intensity[data < thresholds[1]] <- labels[1]

  # Middle levels
  for (i in seq_along(thresholds)) {
    if (i < length(thresholds)) {
      intensity[data >= thresholds[i] & data < thresholds[i + 1]] <- labels[i + 1]
    } else {
      intensity[data >= thresholds[i]] <- labels[i + 1]
    }
  }

  intensity[is.na(data)] <- NA_character_

  factor(intensity, levels = labels, ordered = TRUE)
}


# UNIFIED CUT-POINT FUNCTION

#' Apply Cut Points with Algorithm Selection
#'
#' Unified function to apply any supported cut-point algorithm.
#'
#' @param data Numeric vector of activity metric (CPM or VM CPM)
#' @param algorithm Character string specifying algorithm. See Details.
#' @param epoch_seconds Current epoch length (for CPM conversion)
#' @param age Numeric age in years (for age-specific algorithms)
#' @param ... Additional arguments passed to specific algorithms
#'
#' @return Ordered factor with intensity levels
#'
#' @details
#' Supported algorithms:
#'
#' \strong{Adult count-based:}
#' "freedson", "troiano", "matthews", "santos_lozano_younger",
#' "santos_lozano_older", "crouter"
#'
#' \strong{Triaxial/VM:}
#' "sasaki_vm3", "freedson_vm3"
#'
#' \strong{Children count-based:}
#' "evenson", "puyau", "mattocks", "pate_preschool",
#' "butte_preschool", "chandler", "copeland", "romanzini"
#'
#' \strong{Older adults:}
#' "copeland_older"
#'
#' \strong{Custom:}
#' "canhr"
#'
#' @examples
#' \dontrun{
#' # Apply Freedson adult cutpoints
#' intensity <- apply_cutpoints(cpm, "freedson")
#'
#' # Apply Evenson children cutpoints
#' intensity <- apply_cutpoints(cpm, "evenson")
#'
#' # Auto-select based on age
#' intensity <- apply_cutpoints(cpm, "auto", age = 8)  # Uses Evenson
#' intensity <- apply_cutpoints(cpm, "auto", age = 35) # Uses Freedson
#' }
#'
#' @export
apply_cutpoints <- function(data,
                             algorithm = "freedson",
                             epoch_seconds = 60,
                             age = NULL,
                             ...) {

  # Convert to CPM if needed (for count-based algorithms)
  needs_cpm <- algorithm %in% c(
    "freedson", "troiano", "matthews", "santos_lozano_younger",
    "santos_lozano_older", "crouter", "evenson", "puyau", "mattocks",
    "pate_preschool", "butte_preschool", "chandler", "copeland",
    "romanzini", "copeland_older", "canhr"
  )

  if (needs_cpm && epoch_seconds != 60) {
    data <- to_cpm(data, epoch_seconds)
  }

  # Auto-select algorithm based on age
  if (algorithm == "auto") {
    if (is.null(age)) {
      algorithm <- "freedson"  # Default to adult
      message("No age provided; defaulting to Freedson adult cut-points")
    } else if (age < 5) {
      algorithm <- "pate_preschool"
      message("Age < 5; using Pate preschool cut-points")
    } else if (age < 18) {
      algorithm <- "evenson"
      message("Age 5-17; using Evenson children cut-points")
    } else if (age >= 65) {
      algorithm <- "copeland_older"
      message("Age >= 65; using Copeland older adult cut-points")
    } else {
      algorithm <- "freedson"
      message("Age 18-64; using Freedson adult cut-points")
    }
  }

  # Apply selected algorithm
  result <- switch(algorithm,
    # Adult count-based
    "freedson" = freedson(data),
    "troiano" = troiano(data),
    "matthews" = matthews(data),
    "santos_lozano_younger" = santos_lozano(data, "younger"),
    "santos_lozano_older" = santos_lozano(data, "older"),
    "crouter" = crouter(data, ...),

    # Triaxial/VM
    "sasaki_vm3" = sasaki_vm3(data),
    "freedson_vm3" = freedson_vm3(data),

    # Children count-based
    "evenson" = evenson(data),
    "puyau" = puyau(data),
    "mattocks" = mattocks(data),
    "pate_preschool" = pate_preschool(data),
    "butte_preschool" = butte_preschool(data, ...),
    "chandler" = chandler(data),
    "copeland" = copeland(data),
    "romanzini" = romanzini(data),

    # Older adults
    "copeland_older" = copeland_older(data),

    # Custom
    "canhr" = CANHR.Cutpoints(data),

    # Default
    stop("Unknown algorithm: ", algorithm,
         "\nSee ?apply_cutpoints for supported algorithms")
  )

  return(result)
}


#' Get Cutpoint Thresholds
#'
#' Returns the numeric CPM thresholds for a given cutpoint algorithm.
#' This is useful for visualizations and custom analyses.
#'
#' @param algorithm Character string specifying the cutpoint algorithm.
#'   Options: "freedson", "troiano", "matthews", "evenson", "puyau",
#'   "sasaki_vm3", "freedson_vm3", "canhr", etc.
#'
#' @return Named list with threshold values representing the START of each
#'   intensity level (i.e., the minimum CPM for that category):
#'   \itemize{
#'     \item sedentary: Start of light activity (values below this are sedentary)
#'     \item light: Start of moderate activity
#'     \item moderate: Start of vigorous activity
#'     \item vigorous: Start of very vigorous (or Inf if not defined)
#'   }
#'
#' @details
#' Thresholds are aligned with the actual algorithm implementations in this
#' package. For algorithms using inclusive thresholds (e.g., <= 100), the
#' returned value is the first CPM that would NOT be in that category.
#'
#' @examples
#' # Get Freedson thresholds
#' thresholds <- get_cutpoint_thresholds("freedson")
#' thresholds$moderate  # 5725 (moderate starts at 5725 CPM)
#'
#' @export
get_cutpoint_thresholds <- function(algorithm = "freedson") {
  algorithm <- tolower(algorithm)

  # Thresholds represent the START of each intensity level
  # Values are aligned with actual algorithm implementations
  thresholds <- switch(algorithm,
    # Adult cut-points (count-based)
    # Freedson (1998): sed < 100, light 100-1951, mod 1952-5724, vig 5725-9498, vvig >= 9499
    "freedson" = list(sedentary = 100, light = 1952, moderate = 5725, vigorous = 9499),

    # Troiano (2008): sed < 100, light 100-2019, mod 2020-5998, vig >= 5999
    "troiano" = list(sedentary = 100, light = 2020, moderate = 5999, vigorous = Inf),

    # Matthews (2005): sed < 100, light 100-759, lifestyle 760-1951, mod 1952-5724, vig >= 5725
    "matthews" = list(sedentary = 100, light = 760, lifestyle = 1952, moderate = 5725, vigorous = Inf),

    # Santos-Lozano younger (2013): sed < 100, light 100-3207, mod 3208-8564, vig >= 8565
    "santos_lozano" = ,
    "santos_lozano_younger" = list(sedentary = 100, light = 3208, moderate = 8565, vigorous = Inf),

    # Santos-Lozano older (2013): sed < 100, light 100-2750, mod 2751-9358, vig >= 9359
    "santos_lozano_older" = list(sedentary = 100, light = 2751, moderate = 9359, vigorous = Inf),

    # Crouter (2006) without CV: sed <= 50, light 51-1040, mod 1041-5724, vig >= 5725
    "crouter" = list(sedentary = 51, light = 1041, moderate = 5725, vigorous = Inf),

    # Triaxial (VM-based)
    # Sasaki VM3 (2011): sed < 200, light 200-2689, mod 2690-6166, vig 6167-9642, vvig >= 9643
    "sasaki_vm3" = list(sedentary = 200, light = 2690, moderate = 6167, vigorous = 9643),

    # Freedson VM3 (2011): sed < 100, light 100-2452, mod 2453-6891, vig >= 6892
    "freedson_vm3" = list(sedentary = 100, light = 2453, moderate = 6892, vigorous = Inf),

    # Children cut-points
    # Evenson (2008): sed <= 100, light 101-2295, mod 2296-4011, vig > 4011
    "evenson" = list(sedentary = 101, light = 2296, moderate = 4012, vigorous = Inf),

    # Puyau (2002): sed < 800, light 800-3199, mod 3200-8199, vig >= 8200
    "puyau" = list(sedentary = 800, light = 3200, moderate = 8200, vigorous = Inf),

    # Mattocks (2007): sed <= 100, light 101-3580, mod 3581-6129, vig > 6129
    "mattocks" = list(sedentary = 101, light = 3581, moderate = 6130, vigorous = Inf),

    # Pate preschool (2006): sed < 800, light 800-1679, mod 1680-3367, vig >= 3368
    "pate_preschool" = list(sedentary = 800, light = 1680, moderate = 3368, vigorous = Inf),

    # Butte preschool (2014) age 5+: sed < 240, light 240-2295, mvpa >= 2296
    "butte_preschool" = list(sedentary = 240, light = 2296, moderate = Inf, vigorous = Inf),

    # Chandler (2016): sed <= 20, light 21-1616, mod 1617-3240, vig > 3240
    "chandler" = list(sedentary = 21, light = 1617, moderate = 3241, vigorous = Inf),

    # Copeland children (2009): sed <= 100, light 101-2220, mod 2221-6130, vig > 6130
    "copeland" = list(sedentary = 101, light = 2221, moderate = 6131, vigorous = Inf),

    # Romanzini (2014): sed <= 180, light 181-756, mod 757-1111, vig > 1111
    "romanzini" = list(sedentary = 181, light = 757, moderate = 1112, vigorous = Inf),

    # Older adults
    # Copeland older (2009): sed < 100, light 100-1040, mod 1041-1800, vig > 1800
    "copeland_older" = list(sedentary = 100, light = 1041, moderate = 1801, vigorous = Inf),

    # CANHR custom: sed <= 150, light 151-2200, mod 2201-6000, vig 6001-10000, vvig > 10000
    "canhr" = list(sedentary = 151, light = 2201, moderate = 6001, vigorous = 10001),

    # Default: Freedson
    list(sedentary = 100, light = 1952, moderate = 5725, vigorous = 9499)
  )

  return(thresholds)
}


#' List Available Cut-Point Algorithms
#'
#' Returns information about all available cut-point algorithms
#' including their target population and reference.
#'
#' @param category Filter by category: "all", "adult", "children",
#'   "older_adult", "triaxial"
#'
#' @return Data frame with algorithm information
#'
#' @examples
#' # List all algorithms
#' list_cutpoints()
#'
#' # List only children algorithms
#' list_cutpoints("children")
#'
#' @export
list_cutpoints <- function(category = "all") {

  cutpoints_info <- data.frame(
    algorithm = c(
      # Adult count-based
      "freedson", "troiano", "matthews", "santos_lozano_younger",
      "santos_lozano_older", "crouter",
      # Triaxial
      "sasaki_vm3", "freedson_vm3",
      # Children
      "evenson", "puyau", "mattocks", "pate_preschool",
      "butte_preschool", "chandler", "copeland", "romanzini",
      # Older adults
      "copeland_older",
      # Custom
      "canhr"
    ),
    category = c(
      rep("adult", 6),
      rep("triaxial", 2),
      rep("children", 8),
      "older_adult",
      "custom"
    ),
    input = c(
      rep("CPM", 6),
      rep("VM CPM", 2),
      rep("CPM", 8),
      "CPM",
      "CPM"
    ),
    reference = c(
      "Freedson 1998", "Troiano 2008", "Matthews 2005",
      "Santos-Lozano 2013", "Santos-Lozano 2013", "Crouter 2006",
      "Sasaki 2011", "Freedson 2011",
      "Evenson 2008", "Puyau 2002", "Mattocks 2007", "Pate 2006",
      "Butte 2014", "Chandler 2016", "Copeland 2009", "Romanzini 2014",
      "Copeland 2009",
      "CANHR Custom"
    ),
    stringsAsFactors = FALSE
  )

  if (category != "all") {
    cutpoints_info <- cutpoints_info[cutpoints_info$category == category, ]
  }

  return(cutpoints_info)
}


# SUMMARY FUNCTIONS

#' Summarize Activity Intensity
#'
#' Calculate time spent in each intensity category.
#'
#' @param intensity_levels Factor vector from any cut-point function
#' @param wear_time Logical vector indicating wear time (optional)
#' @param epoch_seconds Epoch length in seconds (default: 60)
#'
#' @return Data frame with minutes and percentage in each category
#'
#' @export
intensity <- function(intensity_levels, wear_time = NULL, epoch_seconds = 60) {
  if (is.null(wear_time)) wear_time <- rep(TRUE, length(intensity_levels))
  if (length(intensity_levels) != length(wear_time)) {
    stop("intensity_levels and wear_time must have the same length")
  }

  intensity_wear <- intensity_levels[wear_time]
  summary_table <- table(intensity_wear)
  total_wear_epochs <- sum(wear_time)
  minutes_per_epoch <- epoch_seconds / 60

  # Guard against division by zero
  pct_values <- if (total_wear_epochs > 0) {
    as.numeric(summary_table) / total_wear_epochs * 100
  } else {
    rep(NA_real_, length(summary_table))
  }

  data.frame(
    intensity = names(summary_table),
    epochs = as.numeric(summary_table),
    minutes = as.numeric(summary_table) * minutes_per_epoch,
    percentage = pct_values,
    stringsAsFactors = FALSE
  )
}


#' Calculate MVPA Minutes
#'
#' Calculate total minutes of Moderate-to-Vigorous Physical Activity.
#'
#' @param intensity_levels Factor or character vector of intensity levels
#' @param wear_time Logical vector indicating valid wear time (default: all TRUE)
#' @param epoch_seconds Epoch length in seconds (default: 60)
#' @param include_vigorous Include vigorous activity? (default: TRUE)
#'
#' @return Numeric. Total MVPA minutes during wear time
#'
#' @export
mvpa <- function(intensity_levels,
                  wear_time = NULL,
                  epoch_seconds = 60,
                  include_vigorous = TRUE) {

  if (is.null(wear_time)) wear_time <- rep(TRUE, length(intensity_levels))
  intensity_wear <- intensity_levels[wear_time]
  minutes_per_epoch <- epoch_seconds / 60

  mvpa_levels <- if (include_vigorous) {
    c("moderate", "vigorous", "very_vigorous")
  } else {
    "moderate"
  }

  sum(intensity_wear %in% mvpa_levels, na.rm = TRUE) * minutes_per_epoch
}


#' Calculate Sedentary Time
#'
#' Calculate total minutes of sedentary behavior.
#'
#' @param intensity_levels Factor or character vector of intensity levels
#' @param wear_time Logical vector indicating valid wear time
#' @param epoch_seconds Epoch length in seconds (default: 60)
#'
#' @return Numeric. Total sedentary minutes during wear time
#'
#' @export
sedentary_time <- function(intensity_levels,
                            wear_time = NULL,
                            epoch_seconds = 60) {

  if (is.null(wear_time)) wear_time <- rep(TRUE, length(intensity_levels))
  intensity_wear <- intensity_levels[wear_time]
  minutes_per_epoch <- epoch_seconds / 60

  sum(intensity_wear %in% c("sedentary", "inactivity"), na.rm = TRUE) * minutes_per_epoch
}


#' Calculate Light Activity Time
#'
#' Calculate total minutes of light physical activity.
#'
#' @param intensity_levels Factor or character vector of intensity levels
#' @param wear_time Logical vector indicating valid wear time
#' @param epoch_seconds Epoch length in seconds (default: 60)
#' @param include_lifestyle Include "lifestyle" intensity if present (default: TRUE)
#'
#' @return Numeric. Total light activity minutes during wear time
#'
#' @export
light_activity <- function(intensity_levels,
                            wear_time = NULL,
                            epoch_seconds = 60,
                            include_lifestyle = TRUE) {

  if (is.null(wear_time)) wear_time <- rep(TRUE, length(intensity_levels))
  intensity_wear <- intensity_levels[wear_time]
  minutes_per_epoch <- epoch_seconds / 60

  light_levels <- if (include_lifestyle) c("light", "lifestyle") else "light"

  sum(intensity_wear %in% light_levels, na.rm = TRUE) * minutes_per_epoch
}


#' Compare Cut-Point Algorithms
#'
#' Apply multiple cut-point algorithms and compare results.
#'
#' @param data Numeric vector of activity data
#' @param algorithms Character vector of algorithm names to compare
#' @param epoch_seconds Epoch length in seconds
#' @param wear_time Logical vector indicating valid wear time
#'
#' @return Data frame comparing MVPA minutes across algorithms
#'
#' @examples
#' \dontrun{
#' # Compare adult algorithms
#' compare_cutpoints(cpm, c("freedson", "troiano", "matthews"))
#'
#' # Compare children algorithms
#' compare_cutpoints(cpm, c("evenson", "puyau", "mattocks"))
#' }
#'
#' @export
compare_cutpoints <- function(data,
                               algorithms = c("freedson", "troiano", "evenson"),
                               epoch_seconds = 60,
                               wear_time = NULL) {

  if (is.null(wear_time)) wear_time <- rep(TRUE, length(data))

  results <- data.frame(
    algorithm = character(0),
    sedentary_min = numeric(0),
    light_min = numeric(0),
    mvpa_min = numeric(0),
    stringsAsFactors = FALSE
  )

  for (algo in algorithms) {
    tryCatch({
      intensity <- apply_cutpoints(data, algo, epoch_seconds)

      results <- rbind(results, data.frame(
        algorithm = algo,
        sedentary_min = sedentary_time(intensity, wear_time, epoch_seconds),
        light_min = light_activity(intensity, wear_time, epoch_seconds),
        mvpa_min = mvpa(intensity, wear_time, epoch_seconds),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      warning(paste("Failed to apply", algo, ":", e$message))
    })
  }

  return(results)
}
