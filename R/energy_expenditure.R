#' Calculate Energy Expenditure
#'
#' Converts METs to kilocalories (kcal) using body mass and time.
#'
#' @param mets Numeric vector of METs values
#' @param body_mass Numeric. Body mass in kilograms
#' @param epoch_length Numeric. Epoch length in seconds (default: 60)
#'
#' @return List with kcal_per_epoch, kcal_per_min, total_kcal
#'
#' @details
#' Formula: kcal/min = METs × 3.5 × body_mass (kg) / 200
#'
#' @references
#' Byrne NM, et al. (2005). J Appl Physiol, 99(3):1112-9.
#'
#' @export
calculate.energy.expenditure <- function(mets, body_mass, epoch_length = 60) {

  if (missing(body_mass) || is.null(body_mass) || is.na(body_mass)) {
    warning("Body mass not provided. Using default 70 kg.\n",
            "For accurate energy expenditure, provide subject body mass.")
    body_mass <- 70
  }

  if (body_mass <= 0 || body_mass > 300) {
    stop("Invalid body mass: ", body_mass, " kg\n",
         "Body mass must be between 0 and 300 kg")
  }

  kcal_per_min <- mets * 3.5 * body_mass / 200

  minutes_per_epoch <- epoch_length / 60
  kcal_per_epoch <- kcal_per_min * minutes_per_epoch

  total_kcal <- sum(kcal_per_epoch, na.rm = TRUE)

  return(list(
    kcal_per_epoch = kcal_per_epoch,
    kcal_per_min = kcal_per_min,
    total_kcal = total_kcal
  ))
}


#' Calculate Energy Expenditure Directly from Counts
#'
#' Direct count-to-kcal prediction equations (without METs intermediate step).
#'
#' @param counts_data Data frame from agd.counts()
#' @param body_mass Numeric. Body mass in kilograms
#' @param algorithm Character. Direct EE prediction algorithm:
#'   \itemize{
#'     \item \code{"williams"} - Williams Work-Energy (1998)
#'     \item \code{"freedson"} - Freedson (1998)
#'     \item \code{"freedson.combination"} - Freedson Combination (1998, default)
#'     \item \code{"freedson.vm3"} - Freedson VM3 (2011)
#'     \item \code{"freedson.vm3.combination"} - Freedson VM3 Combination (2011)
#'   }
#' @param epoch_length Numeric. Epoch length in seconds (default: 60)
#'
#' @return List with kcal_per_epoch, kcal_per_min, total_kcal
#'
#' @references
#' ActiGraph support documentation
#'
#' @export
calculate.energy.expenditure.direct <- function(counts_data,
                                                body_mass,
                                                algorithm = c("freedson.combination", "williams",
                                                            "freedson", "freedson.vm3",
                                                            "freedson.vm3.combination"),
                                                epoch_length = 60) {

  algorithm <- match.arg(algorithm)

  if (missing(body_mass) || is.null(body_mass) || is.na(body_mass)) {
    warning("Body mass not provided. Using default 70 kg.")
    body_mass <- 70
  }

  # Dispatch to appropriate algorithm function
  kcal_per_min <- switch(algorithm,
    "williams" = ee.williams(counts_data, body_mass, epoch_length),
    "freedson" = ee.freedson(counts_data, body_mass, epoch_length),
    "freedson.combination" = ee.freedson.combination(counts_data, body_mass, epoch_length),
    "freedson.vm3" = ee.freedson.vm3(counts_data, body_mass, epoch_length),
    "freedson.vm3.combination" = ee.freedson.vm3.combination(counts_data, body_mass, epoch_length),
    stop("Unknown algorithm: ", algorithm)
  )

  minutes_per_epoch <- epoch_length / 60
  kcal_per_epoch <- kcal_per_min * minutes_per_epoch

  total_kcal <- sum(kcal_per_epoch, na.rm = TRUE)

  return(list(
    kcal_per_epoch = kcal_per_epoch,
    kcal_per_min = kcal_per_min,
    total_kcal = total_kcal
  ))
}


ee.williams <- function(counts_data, body_mass, epoch_length) {
  cpm <- counts_data$axis1

  if (epoch_length != 60) {
    scale <- 60 / epoch_length
    cpm <- cpm * scale
  }

  kcal_per_min <- cpm * 0.0000191 * body_mass

  return(kcal_per_min)
}


ee.freedson <- function(counts_data, body_mass, epoch_length) {
  # Freedson (1998) energy expenditure equation
  # Original equation only valid for CPM > 1951 (MVPA range)
  #
  # For CPM > 1951: kcal/min = 0.00094*CPM + 0.1346*mass - 7.37418
  # For CPM <= 1951: Uses resting metabolic rate estimate (1 MET baseline)
  #
  # Note: Use ee.freedson.combination() for full-range EE estimation
  # that applies Williams equation for light activity.
  #
  # Reference: Freedson PS et al. (1998). Med Sci Sports Exerc. 30(5):777-781

  cpm <- counts_data$axis1
  scale <- 1

  if (epoch_length != 60) {
    scale <- 60 / epoch_length
    cpm <- cpm * scale
  }

  # For CPM > 1951, use Freedson equation
  # For CPM <= 1951, use resting EE (1 MET = 3.5 * body_mass / 200 kcal/min)
  resting_kcal_per_min <- 3.5 * body_mass / 200

  kcal_per_min <- ifelse(cpm > 1951,
                          (0.00094 * cpm + (0.1346 * body_mass - 7.37418)),
                          resting_kcal_per_min)

  # Ensure no negative values and minimum of resting EE
  kcal_per_min[kcal_per_min < resting_kcal_per_min] <- resting_kcal_per_min

  return(kcal_per_min)
}


ee.freedson.combination <- function(counts_data, body_mass, epoch_length) {
  cpm <- counts_data$axis1
  scale <- 1  # default

  if (epoch_length != 60) {
    scale <- 60 / epoch_length
    cpm <- cpm * scale
  }

  # Vectorized calculation
  kcal_per_min <- ifelse(cpm > 1951,
                          scale * (0.00094 * cpm + (0.1346 * body_mass - 7.37418)),
                          cpm * 0.0000191 * body_mass)

  kcal_per_min[kcal_per_min < 0] <- 0

  return(kcal_per_min)
}


ee.freedson.vm3 <- function(counts_data, body_mass, epoch_length) {
  vm <- vm(counts_data$axis1, counts_data$axis2, counts_data$axis3)
  vmcpm <- vm

  if (epoch_length != 60) {
    scale <- 60 / epoch_length
    vmcpm <- vm * scale
  }

  kcal_per_min <- ifelse(vmcpm > 2453,
                          0.001064 * vm + 0.087512 * body_mass - 5.500229,
                          0)

  kcal_per_min[kcal_per_min < 0] <- 0

  return(kcal_per_min)
}


ee.freedson.vm3.combination <- function(counts_data, body_mass, epoch_length) {
  vm <- vm(counts_data$axis1, counts_data$axis2, counts_data$axis3)
  cpm <- counts_data$axis1
  vmcpm <- vm

  if (epoch_length != 60) {
    scale <- 60 / epoch_length
    vmcpm <- vm * scale
    cpm <- cpm * scale
  }

  # Vectorized calculation
  kcal_per_min <- ifelse(vmcpm > 2453,
                          0.001064 * vm + 0.087512 * body_mass - 5.500229,
                          cpm * 0.0000191 * body_mass)

  kcal_per_min[kcal_per_min < 0] <- 0

  return(kcal_per_min)
}


#' Summarize Energy Expenditure by Intensity
#'
#' @param kcal_per_epoch Numeric vector of kcal per epoch
#' @param intensity Factor vector of intensity classifications
#' @param wear_time Logical vector indicating wear time
#'
#' @return Data frame with intensity, total_kcal, percent_kcal
#'
#' @export
summarize.energy.expenditure <- function(kcal_per_epoch, intensity, wear_time) {

  wear_epochs <- wear_time == TRUE
  kcal_wear <- kcal_per_epoch[wear_epochs]
  intensity_wear <- intensity[wear_epochs]

  total_kcal <- sum(kcal_wear, na.rm = TRUE)

  ee_summary <- aggregate(kcal_wear ~ intensity_wear, FUN = sum, na.rm = TRUE)
  names(ee_summary) <- c("intensity", "total_kcal")

  ee_summary$percent_kcal <- round(100 * ee_summary$total_kcal / total_kcal, 2)

  intensity_order <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")
  ee_summary$intensity <- factor(ee_summary$intensity, levels = intensity_order)
  ee_summary <- ee_summary[order(ee_summary$intensity), ]
  rownames(ee_summary) <- NULL

  return(ee_summary)
}


#' Calculate Average METs
#'
#' @param mets Numeric vector of METs values
#' @param wear_time Logical vector indicating wear time
#' @param timestamp POSIXct vector of timestamps
#'
#' @return List with average_mets, daily_average_mets
#'
#' @export
calculate.average.mets <- function(mets, wear_time, timestamp) {

  wear_epochs <- wear_time == TRUE
  mets_wear <- mets[wear_epochs]

  average_mets <- mean(mets_wear, na.rm = TRUE)

  dates <- as.Date(timestamp)
  daily_mets <- data.frame(
    date = dates[wear_epochs],
    mets = mets_wear
  )

  daily_average <- aggregate(mets ~ date, data = daily_mets, FUN = mean, na.rm = TRUE)
  names(daily_average) <- c("date", "average_mets")
  daily_average$average_mets <- round(daily_average$average_mets, 2)

  return(list(
    average_mets = round(average_mets, 2),
    daily_average_mets = daily_average
  ))
}
