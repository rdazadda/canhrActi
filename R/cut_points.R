#' Apply Freedson Adult (1998) Cut Points
#' @param counts_per_minute Numeric vector of counts per minute
#' @return Ordered factor with levels: sedentary, light, moderate, vigorous, very_vigorous
#' @export
freedson <- function(counts_per_minute) {
  intensity <- character(length(counts_per_minute))
  intensity[counts_per_minute >= 0   & counts_per_minute <= 99]   <- "sedentary"
  intensity[counts_per_minute >= 100 & counts_per_minute <= 1951] <- "light"
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
