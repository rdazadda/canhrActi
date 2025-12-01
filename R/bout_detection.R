#' Detect MVPA Bouts
#'
#' Identifies sustained periods of moderate-to-vigorous physical activity (MVPA)
#' meeting specified bout criteria. Used to assess adherence to physical activity
#' guidelines which recommend MVPA in bouts of at least 10 minutes.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param min_bout_length Integer. Minimum bout duration in minutes (default: 10)
#' @param drop_time_allowance Integer. Maximum minutes below MVPA threshold allowed
#'   within a bout (default: 2). This allows for brief interruptions such as
#'   waiting at traffic lights during jogging.
#' @param use_80_percent_rule Logical. If TRUE, uses 80% rule instead of drop time
#'   (default: FALSE). With 80% rule, at least 80% of minutes in a bout must be MVPA.
#' @param mvpa_levels Character vector. Intensity levels considered MVPA
#'   (default: c("moderate", "vigorous", "very_vigorous"))
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{bout_number} - Sequential bout identifier
#'     \item \code{start_index} - Starting epoch index
#'     \item \code{end_index} - Ending epoch index
#'     \item \code{bout_length} - Duration in minutes
#'     \item \code{mvpa_minutes} - Minutes at MVPA within bout
#'     \item \code{mvpa_percent} - Percentage of bout at MVPA
#'   }
#'
#' @details
#' \strong{Standard Method (Drop Time Allowance):}
#'
#' Identifies bouts of at least \code{min_bout_length} consecutive minutes where
#' activity is at MVPA level, allowing up to \code{drop_time_allowance} minutes
#' to drop below MVPA without breaking the bout. This method follows NHANES and
#' CDC standards.
#'
#' Example: With 10-minute minimum and 2-minute allowance, a valid bout could be:
#' M-M-M-L-M-M-M-M-M-M (8 MVPA + 2 light = valid 10-minute bout)
#'
#' \strong{80% Rule Method:}
#'
#' Requires at least 80% of minutes within a bout to be at MVPA level. For a
#' 10-minute bout, this means at least 8 minutes must be MVPA. This is equivalent
#' to the standard method for 10-minute bouts but differs for longer bouts.
#'
#' @references
#' Troiano RP, et al. (2008). Physical activity in the United States measured by
#' accelerometer. Medicine & Science in Sports & Exercise, 40(1):181-188.
#'
#' CDC/NHANES Physical Activity Monitor procedures:
#' https://www.cdc.gov/pcd/issues/2012/11_0332.htm
#'
#' Matthews CE, et al. (2014). Interruption in physical activity bout analysis.
#' BMC Research Notes, 7:284.
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' bouts <- detect.mvpa.bouts(results$epoch_data$intensity)
#'
#' # Summary
#' cat("Total MVPA bouts:", nrow(bouts), "\n")
#' cat("Total bouted MVPA:", sum(bouts$mvpa_minutes), "minutes\n")
#'
#' # Conservative approach (1 minute allowance)
#' bouts_strict <- detect.mvpa.bouts(results$epoch_data$intensity,
#'                                   drop_time_allowance = 1)
#'
#' # 80% rule approach
#' bouts_80 <- detect.mvpa.bouts(results$epoch_data$intensity,
#'                               use_80_percent_rule = TRUE)
#' }
#'
#' @export
detect.mvpa.bouts <- function(intensity,
                              min_bout_length = 10,
                              drop_time_allowance = 2,
                              use_80_percent_rule = FALSE,
                              mvpa_levels = c("moderate", "vigorous", "very_vigorous")) {

  if (length(intensity) == 0) {
    return(data.frame(
      bout_number = integer(0),
      start_index = integer(0),
      end_index = integer(0),
      bout_length = integer(0),
      mvpa_minutes = integer(0),
      mvpa_percent = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  if (min_bout_length < 1) {
    stop("min_bout_length must be at least 1 minute")
  }

  if (drop_time_allowance < 0) {
    stop("drop_time_allowance must be non-negative")
  }

  if (drop_time_allowance >= min_bout_length) {
    warning("drop_time_allowance (", drop_time_allowance,
            ") is >= min_bout_length (", min_bout_length,
            "). This may produce unexpected results.")
  }

  intensity <- as.character(intensity)
  is.mvpa <- intensity %in% mvpa_levels

  if (use_80_percent_rule) {
    bouts <- .detect.bouts.80.percent(is.mvpa, min_bout_length)
  } else {
    bouts <- .detect.bouts.drop.time(is.mvpa, min_bout_length, drop_time_allowance)
  }

  if (length(bouts) == 0) {
    return(data.frame(
      bout_number = integer(0),
      start_index = integer(0),
      end_index = integer(0),
      bout_length = integer(0),
      mvpa_minutes = integer(0),
      mvpa_percent = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, lapply(1:length(bouts), function(i) {
    bout <- bouts[[i]]
    mvpa.count <- sum(is.mvpa[bout$start:bout$end])
    bout.len <- bout$end - bout$start + 1

    data.frame(
      bout_number = i,
      start_index = bout$start,
      end_index = bout$end,
      bout_length = bout.len,
      mvpa_minutes = mvpa.count,
      mvpa_percent = round(100 * mvpa.count / bout.len, 1),
      stringsAsFactors = FALSE
    )
  }))

  return(result)
}


#' Detect Bouts Using Drop Time Allowance Method
#'
#' Internal function implementing NHANES/CDC standard bout detection.
#'
#' @param is_mvpa Logical vector indicating MVPA epochs
#' @param min_bout_length Minimum bout length in minutes
#' @param drop_time_allowance Maximum consecutive minutes below MVPA allowed
#'
#' @return List of bouts, each with start and end indices
#' @keywords internal
.detect.bouts.drop.time <- function(is_mvpa, min_bout_length, drop_time_allowance) {
  n <- length(is_mvpa)
  bouts <- list()
  i <- 1

  while (i <= n) {
    if (!is_mvpa[i]) {
      i <- i + 1
      next
    }

    bout.start <- i
    consecutive.drops <- 0
    j <- i

    while (j <= n) {
      if (is_mvpa[j]) {
        consecutive.drops <- 0
        j <- j + 1
      } else {
        consecutive.drops <- consecutive.drops + 1

        if (consecutive.drops > drop_time_allowance) {
          bout.end <- j - consecutive.drops
          bout.length <- bout.end - bout.start + 1

          if (bout.length >= min_bout_length) {
            bouts[[length(bouts) + 1]] <- list(start = bout.start, end = bout.end)
          }

          i <- j
          break
        }
        j <- j + 1
      }

      if (j > n) {
        bout.end <- j - 1 - max(0, consecutive.drops - drop_time_allowance)
        bout.length <- bout.end - bout.start + 1

        if (bout.length >= min_bout_length) {
          bouts[[length(bouts) + 1]] <- list(start = bout.start, end = bout.end)
        }

        i <- j
        break
      }
    }
  }

  return(bouts)
}


#' Detect Bouts Using 80 Percent Rule
#'
#' Internal function implementing 80% rule for bout detection.
#'
#' @param is_mvpa Logical vector indicating MVPA epochs
#' @param min_bout_length Minimum bout length in minutes
#'
#' @return List of bouts, each with start and end indices
#' @keywords internal
.detect.bouts.80.percent <- function(is_mvpa, min_bout_length) {
  n <- length(is_mvpa)
  bouts <- list()

  if (n < min_bout_length) {
    return(bouts)
  }

  i <- 1
  while (i <= n - min_bout_length + 1) {
    window.start <- i
    window.end <- i + min_bout_length - 1

    mvpa.count <- sum(is_mvpa[window.start:window.end])
    required.mvpa <- ceiling(0.8 * min_bout_length)

    if (mvpa.count >= required.mvpa) {
      bout.start <- window.start
      bout.end <- window.end

      j <- window.end + 1
      while (j <= n) {
        extended.mvpa <- sum(is_mvpa[bout.start:j])
        extended.length <- j - bout.start + 1
        required.extended <- ceiling(0.8 * extended.length)

        if (extended.mvpa >= required.extended) {
          bout.end <- j
          j <- j + 1
        } else {
          break
        }
      }

      bouts[[length(bouts) + 1]] <- list(start = bout.start, end = bout.end)
      i <- bout.end + 1
    } else {
      i <- i + 1
    }
  }

  return(bouts)
}


#' Summarize MVPA Bouts
#'
#' Calculates summary statistics for detected MVPA bouts.
#'
#' @param bouts Data frame returned from \code{detect.mvpa.bouts}
#'
#' @return List with summary statistics:
#'   \itemize{
#'     \item \code{total_bouts} - Number of bouts detected
#'     \item \code{total_bouted_mvpa} - Total MVPA minutes in bouts
#'     \item \code{mean_bout_length} - Average bout duration
#'     \item \code{median_bout_length} - Median bout duration
#'     \item \code{min_bout_length} - Shortest bout
#'     \item \code{max_bout_length} - Longest bout
#'   }
#'
#' @export
summarize.mvpa.bouts <- function(bouts) {
  if (nrow(bouts) == 0) {
    return(list(
      total_bouts = 0,
      total_bouted_mvpa = 0,
      mean_bout_length = 0,
      median_bout_length = 0,
      min_bout_length = NA,
      max_bout_length = NA
    ))
  }

  list(
    total_bouts = nrow(bouts),
    total_bouted_mvpa = sum(bouts$mvpa_minutes),
    mean_bout_length = round(mean(bouts$bout_length), 1),
    median_bout_length = median(bouts$bout_length),
    min_bout_length = min(bouts$bout_length),
    max_bout_length = max(bouts$bout_length)
  )
}
