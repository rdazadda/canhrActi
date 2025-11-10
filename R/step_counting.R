#' Count Steps from Raw Acceleration Using Peak Detection
#'
#' Implements step detection based on published research using bandpass filtering
#' and zero-crossing detection. Uses wrist-worn threshold (0.0359g) from
#' transparent step detection methods.
#'
#' @param acceleration Numeric vector of vertical axis acceleration (Y-axis) in g-units
#' @param sampling_freq Numeric. Sampling frequency in Hz (default: 60)
#' @param peak_threshold Numeric. Minimum peak height in g (default: 0.0359 for wrist)
#' @param min_step_interval Minimum time between steps in seconds (default: 0.3)
#' @return Integer. Total number of steps detected
#'
#' @details
#' The algorithm follows published ActiGraph step detection methodology:
#' \itemize{
#'   \item Bandpass filter: 0.25-2.5 Hz (4th order Butterworth, zero-lag)
#'   \item Threshold: 0.0359g (validated for wrist-worn devices)
#'   \item Detection: Zero-crossing with threshold on filtered acceleration
#' }
#'
#' @references
#' Bandpass filter and threshold from ActiGraph research literature.
#' See: PMC9586317 for transparent step detection methods.
#'
#' @export
steps <- function(acceleration,
                  sampling_freq = 60,
                  peak_threshold = 0.0359,
                  min_step_interval = 0.3) {
  if (length(acceleration) == 0) return(0L)

  downsample.factor <- sampling_freq / 30
  if (downsample.factor > 1) {
    signal.30hz <- gsignal::decimate(acceleration, q = round(downsample.factor), ftype = "iir")
  } else {
    signal.30hz <- acceleration
  }

  signal.30hz <- signal.30hz - mean(signal.30hz, na.rm = TRUE)

  nyquist <- 30 / 2
  low.freq <- 0.25 / nyquist
  high.freq <- 2.5 / nyquist
  low.freq <- max(low.freq, 0.001)
  high.freq <- min(high.freq, 0.999)
  butter.filt <- gsignal::butter(n = 4, w = c(low.freq, high.freq), type = "pass")
  filtered.signal <- gsignal::filtfilt(butter.filt, signal.30hz)

  steps <- 0L
  last.step.idx <- -Inf
  min.samples.between.steps <- min_step_interval * 30

  for (i in 2:(length(filtered.signal) - 1)) {
    if (filtered.signal[i - 1] < -peak_threshold && filtered.signal[i] > peak_threshold) {
      if ((i - last.step.idx) >= min.samples.between.steps) {
        steps <- steps + 1L
        last.step.idx <- i
      }
    }
  }

  return(steps)
}

#' Count Steps Using Autocorrelation Method
#' @param acceleration Numeric vector of vertical axis acceleration
#' @param sampling_freq Numeric. Sampling frequency in Hz
#' @param min_cadence,max_cadence Expected cadence bounds (spm)
#' @return Integer. Estimated total number of steps
#' @export
steps.auto <- function(acceleration,
                                 sampling_freq = 60,
                                 min_cadence = 60,
                                 max_cadence = 200) {
  if (length(acceleration) < sampling_freq * 2) {
    warning("Insufficient data for autocorrelation method")
    return(0L)
  }

  if (sampling_freq > 30) {
    signal <- gsignal::decimate(acceleration, q = round(sampling_freq / 30))
    fs <- 30
  } else {
    signal <- acceleration
    fs <- sampling_freq
  }

  max.lag <- round(fs * 60 / min_cadence)
  acf.result <- stats::acf(signal, lag.max = max.lag, plot = FALSE)
  acf.vals <- acf.result$acf[-1]
  peaks <- which(diff(sign(diff(acf.vals))) == -2) + 1
  if (length(peaks) == 0) return(0L)

  peak.idx <- peaks[which.max(acf.vals[peaks])]
  period.samples <- peak.idx

  step.freq.hz <- fs / period.samples
  step.freq.per.min <- step.freq.hz * 60
  if (step.freq.per.min < min_cadence || step.freq.per.min > max_cadence) return(0L)

  duration.minutes <- length(acceleration) / sampling_freq / 60
  as.integer(round(step.freq.per.min * duration.minutes))
}
