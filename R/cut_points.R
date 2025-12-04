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
  if (!is.numeric(epoch_length) || epoch_length <= 0) stop("epoch_length must be a positive number")
  counts * (60 / epoch_length)
}

#' Apply Freedson Adult (1998) Cut Points
#'
#' Classifies activity counts into intensity categories using the Freedson Adult (1998) cutpoints.
#'
#' @param counts_per_minute Numeric vector of counts per minute (CPM).
#'   If your data is not in 60-second epochs, use \code{\link{to_cpm}} to convert first.
#'
#' @return Ordered factor with levels: sedentary, light, moderate, vigorous, very_vigorous
#'
#' @details
#' The Freedson Adult (1998) cutpoints are based on treadmill validation studies:
#' \itemize{
#'   \item Sedentary: 0-100 CPM (< 1.5 METs)
#'   \item Light: 101-1951 CPM (1.5-2.99 METs)
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
#' # 30-second epoch data (must convert first)
#' counts_30sec <- c(25, 250, 1000, 3000, 5000)
#' cpm <- to_cpm(counts_30sec, epoch_length = 30)
#' intensity <- freedson(cpm)
#'
#' @seealso \code{\link{to_cpm}} for epoch conversion
#'
#' @export
freedson <- function(counts_per_minute) {
  intensity <- character(length(counts_per_minute))
  intensity[counts_per_minute >= 0   & counts_per_minute <= 100]  <- "sedentary"
  intensity[counts_per_minute >= 101 & counts_per_minute <= 1951] <- "light"
  intensity[counts_per_minute >= 1952 & counts_per_minute <= 5724] <- "moderate"
  intensity[counts_per_minute >= 5725 & counts_per_minute <= 9498] <- "vigorous"
  intensity[counts_per_minute >= 9499] <- "very_vigorous"
  factor(intensity, levels = c("sedentary","light","moderate","vigorous","very_vigorous"), ordered = TRUE)
}

#' Apply CANHR Cut Points
#' @param counts_per_minute Numeric vector of counts per minute
#' @return Ordered factor with levels: sedentary, light, moderate, vigorous, very_vigorous
#' @export
CANHR.Cutpoints <- function(counts_per_minute) {
  intensity <- character(length(counts_per_minute))
  intensity[counts_per_minute >= 0   & counts_per_minute <= 150]  <- "sedentary"
  intensity[counts_per_minute >= 151 & counts_per_minute <= 2200] <- "light"
  intensity[counts_per_minute >= 2201 & counts_per_minute <= 6000] <- "moderate"
  intensity[counts_per_minute >= 6001 & counts_per_minute <= 10000] <- "vigorous"
  intensity[counts_per_minute >= 10001] <- "very_vigorous"
  factor(intensity, levels = c("sedentary","light","moderate","vigorous","very_vigorous"), ordered = TRUE)
}

#' Summarize Activity Intensity
#' @param intensity_levels Factor vector from freedson
#' @param wear_time Logical vector indicating wear time (optional)
#' @return Data frame with minutes and percentage in each category
#' @export
intensity <- function(intensity_levels, wear_time = NULL) {
  if (is.null(wear_time)) wear_time <- rep(TRUE, length(intensity_levels))
  if (length(intensity_levels) != length(wear_time)) stop("intensity_levels and wear_time must have the same length")
  intensity_wear <- intensity_levels[wear_time]
  summary_table <- table(intensity_wear)
  total_wear_minutes <- sum(wear_time)
  data.frame(
    intensity = names(summary_table),
    minutes = as.numeric(summary_table),
    percentage = as.numeric(summary_table) / total_wear_minutes * 100,
    stringsAsFactors = FALSE
  )
}

#' Calculate MVPA Minutes
#'
#' @param intensity_levels Factor or character vector of intensity levels
#' @param wear_time Logical vector indicating valid wear time (default: all TRUE)
#' @param include_vigorous Logical. Include vigorous activity in MVPA? (default: TRUE)
#'
#' @return Numeric. Total MVPA minutes during wear time
#'
#' @export
mvpa <- function(intensity_levels, wear_time = NULL, include_vigorous = TRUE) {
  if (is.null(wear_time)) wear_time <- rep(TRUE, length(intensity_levels))
  intensity_wear <- intensity_levels[wear_time]
  if (include_vigorous) sum(intensity_wear %in% c("moderate","vigorous","very_vigorous")) else sum(intensity_wear == "moderate")
}
