# Ported from GGIR 3.3-9 R/CalcSleepRegularityIndex.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Two changes: the time parser is this
# package's .raw.iso8601.to.posix, and LC_TIME is forced to "C" for the duration of the
# call so the weekday column is deterministic.

#' Sleep Regularity Index per Day Pair (GGIR CalcSleepRegularityIndex)
#'
#' Phillips et al. (2017) Sleep Regularity Index as GGIR computes it in part 3, from the
#' sustained inactivity bout column of the part-3 epoch series. For each pair of consecutive
#' calendar dates the index is the probability that the two days hold the same sleep or wake
#' state at the same time of day, rescaled to -100 to 100. One row per day pair, the shape
#' GGIR's part 4 joins against for \code{SleepRegularityIndex1} and \code{SriFractionValid}.
#'
#' @details
#' Columns matching \code{"temperature|selfreported|angle|invalid_|guider|ACC|step_count"}
#' are dropped and the first remaining column other than \code{time}, \code{invalid} and
#' \code{night} is the sleep column. Invalid epochs become NA. Epochs shorter than 30 s are
#' aggregated to 30 s bins on absolute Unix time by mean and base R \code{round}, so
#' \code{M} is 2880; otherwise \code{M} is \code{86400/epochsize}. Days run calendar
#' midnight to midnight. A day pair with no valid slot keeps the value 0, not NA, so read
#' \code{frac_valid} to tell it from a genuine 0. Three distinct calendar dates are needed;
#' with fewer the return value is the scalar \code{NA}. The \code{date} column is
#' \code{"%d/%m/%Y"} with zero padding, unlike \code{calendar_date} in nightsummary.
#'
#' @section Comparison with canhrActi's own Sleep Regularity Index:
#' \code{\link{sri.matrix}} and \code{\link{sleep.regularity.index}} implement the same
#' Phillips estimator and agree numerically once the series is aggregated the GGIR way, but
#' differ in granularity (per day pair here, one pooled scalar there), in what a fully
#' invalid pair returns (0 here, \code{NA} there), in the minimum number of dates (three
#' here, two there) and in rounding. Use this function for the number GGIR would report or
#' to feed \code{\link{raw.sleep.nights}}; use \code{\link{sri.matrix}} for the count-based
#' pipeline.
#'
#' @param data Data frame of the part-3 epoch series: a \code{time} column (POSIXct, or ISO 8601
#'   character or factor, which is coerced), an \code{invalid} column of 0/1, optionally a
#'   \code{night} column, and one column of 0/1 sleep state per sustained inactivity bout
#'   definition.
#' @param epochsize Short epoch length in seconds.
#' @param desiredtz Olson timezone name used to rebuild the timestamps. The empty string
#'   (system timezone) is passed through unchanged.
#' @param SRI1_smoothing_wsize_hrs Smoothing window in hours, or NULL for no smoothing. Both
#'   smoothing arguments must be non-NULL for smoothing to run.
#' @param SRI1_smoothing_frac Fraction of the smoothing window that has to be sleep for the
#'   smoothed epoch to be sleep, or NULL.
#' @return A data frame with \code{Ndays - 1} rows and the columns \code{day} (integer),
#'   \code{SleepRegularityIndex} (numeric, 3 decimals), \code{weekday} (character, full English
#'   name of the first day of the pair), \code{frac_valid} (numeric, 4 decimals) and \code{date}
#'   (character, \code{"%d/%m/%Y"}, the first day of the pair). With fewer than three distinct
#'   calendar dates the return value is the scalar \code{NA}.
#' @references
#' Phillips AJK, Clerx WM, O'Brien CS, Sano A, Barger LK, Picard RW, Lockley SW, Klerman EB,
#' Czeisler CA (2017). Irregular sleep/wake patterns are associated with poorer academic
#' performance and delayed circadian and sleep/wake timing. \emph{Scientific Reports}, 7(1):3216.
#' \doi{10.1038/s41598-017-03171-4}
#' @seealso \code{\link{sri.matrix}} and \code{\link{sleep.regularity.index}} for canhrActi's own
#'   estimator over count data.
#' @examples
#' # Three calendar days of a perfectly regular sleeper at 60 s epochs
#' tt <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 1440 * 3)
#' d <- data.frame(time = tt,
#'                 invalid = 0,
#'                 night = rep(1:3, each = 1440),
#'                 T5A5 = as.numeric(as.POSIXlt(tt)$hour < 8))
#' raw.sleep.regularity(d, epochsize = 60, desiredtz = "UTC")
#' @export
raw.sleep.regularity <- function(data, epochsize, desiredtz = "",
                                 SRI1_smoothing_wsize_hrs = NULL,
                                 SRI1_smoothing_frac = NULL) {
  # C time locale for the call, so weekday is the full English name
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  if (inherits(data$time[1], "character") || inherits(data$time[1], "factor")) {
    data$time <- .raw.iso8601.to.posix(data$time, tz = desiredtz)
  }
  data <- data[, grep("temperature|selfreported|angle|invalid_|guider|ACC|step_count", colnames(data), invert = TRUE)]
  sleepcol <- grep("sleepnap", colnames(data))
  if (length(sleepcol) != 1) {
    # Part-3 path: first remaining column is the sustained inactivity bout series
    sleepcol <- which(colnames(data) %in% c("time", "invalid", "night") == FALSE)[1]
    if (!is.null(SRI1_smoothing_wsize_hrs) && !is.null(SRI1_smoothing_frac)) {
      data[, sleepcol] <- zoo::rollmean(x = data[, sleepcol], k = (SRI1_smoothing_wsize_hrs * 3600) / epochsize, fill = 0)
      data[, sleepcol] <- ifelse(data[, sleepcol] >= SRI1_smoothing_frac, yes = 1, no = 0)
    }
    invalid_epochs <- which(data$invalid == 1)
  } else {
    # Part-6 path (sleepnap column); not reached from this package
    invalid_epochs <- which(data$invalidepoch == 1)
  }
  if (length(invalid_epochs) > 0) {
    is.na(data[invalid_epochs, sleepcol]) <- TRUE
  }
  # Aggregate to 30 s bins on absolute Unix time, as in Phillips et al. (2017);
  # mean then base R round, which is half to even
  if (epochsize < 30 & (30 / epochsize) == round(30 / epochsize)) {
    data$time_num <- floor(as.numeric(data$time) / 30) * 30
    data <- aggregate(data[, sleepcol],
                      by = list(data$time_num), FUN = mean)
    colnames(data) <- c("time_num", "sleepstate")
    data$sleepstate <- round(data$sleepstate)
    M <- 24 * 60 * 2
  } else {
    data$time_num <- as.numeric(data$time)
    colnames(data)[sleepcol] <- "sleepstate"
    M <- (24 * 3600) / epochsize
  }
  epochsize <- max(c(epochsize, 30))
  data$time <- as.POSIXlt(data$time_num, tz = desiredtz, origin = "1970-01-01")
  data$date <- as.Date(data$time)
  convert2SecInDay <- function(x) {
    tmp1 <- format(x, format = "%H %M %S")
    tmp2 <- unlist(strsplit(tmp1, " "))
    SecInDay <- sum(as.numeric(tmp2) * c(3600, 60, 1))
  }
  data$SecInDay <- sapply(data$time, FUN = convert2SecInDay)
  # Pad every date to the full SecInDay grid; absent calendar days are not inserted
  uniqueDates <- unique(data$date)
  Ndays <- length(uniqueDates)
  MaxSecInDay <- max(data$SecInDay)
  SecSequence <- seq(0, MaxSecInDay, by = epochsize)
  SecInDayTmp <- rep(SecSequence, times = Ndays)
  DatesTmp <- rep(uniqueDates, each = length(SecSequence))
  temp_df <- data.frame(SecInDay = SecInDayTmp, date = DatesTmp, stringsAsFactors = FALSE)
  data <- merge(x = data, y = temp_df, by.all = c("SecInDay", "date"), all = TRUE)

  # Index per pair of consecutive dates; needs three distinct dates
  NR <- Ndays - 1
  if (NR > 1) {
    SleepRegularityIndex <- data.frame(day = 1:NR, SleepRegularityIndex = numeric(NR),
                                       weekday = character(NR), frac_valid = numeric(NR),
                                       date = character(NR), stringsAsFactors = FALSE)
    for (i in 1:NR) {
      thisday <- which(data$date == uniqueDates[i] & data$SecInDay < (24 * 3600))
      nextday <- which(data$date == uniqueDates[i + 1] & data$SecInDay < (24 * 3600))
      # 23 h and 25 h day-length guards, only reachable across a daylight saving change
      ne25 <- ((3600 * 25) / epochsize)
      ne24 <- ((3600 * 24) / epochsize)
      ne23 <- ((3600 * 23) / epochsize)
      if (length(thisday) == ne25) {
        thisday <- thisday[1:ne24]
      }
      if (length(nextday) == ne25) {
        nextday <- nextday[1:ne24]
      }
      if (length(thisday) == ne23) {
        nextday <- nextday[1:ne23]
      }
      if (length(nextday) == ne23) {
        thisday <- thisday[1:ne23]
      }
      EqualState <- c(data$sleepstate[thisday] == data$sleepstate[nextday])
      testNA <- c(is.na(data$sleepstate[thisday]) | is.na(data$sleepstate[nextday]))
      SummedValue <- sum(ifelse(test = EqualState[which(testNA == FALSE)] == TRUE, yes = 1, no = 0))
      NValuesSkipped <- length(which(testNA == TRUE))
      SleepRegularityIndex$frac_valid[i] <- round((M - NValuesSkipped) / M, digits = 4)
      if (M != NValuesSkipped) {
        SleepRegularityIndex$SleepRegularityIndex[i] <- round(-100 + (200 / (M - NValuesSkipped)) * SummedValue, digits = 3)
      }
      SleepRegularityIndex$weekday[i] <- .in_c_time(weekdays(abbreviate = FALSE, x = uniqueDates[i]))
      SleepRegularityIndex$date[i] <- format(as.Date(uniqueDates[i],
                                                     origin = "1970-01-01"), format = "%d/%m/%Y")
    }
  } else {
    SleepRegularityIndex <- NA
  }
  return(SleepRegularityIndex)
}
