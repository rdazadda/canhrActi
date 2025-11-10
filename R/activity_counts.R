# Constants for ActiGraph algorithm (Neishabouri et al., 2022)
# IIR filter coefficients from Table 1: Exact coefficients for ActiGraph activity counts algorithm
.actigraph_iir_coef_a <- c(1, -3.63, 5.04, -3.10, 0.506, 0.324, -0.157, 0.0195)
.actigraph_iir_coef_b <- c(-0.00934, -0.0255, -0.00424, 0.0442, 0.0365, -0.0119, -0.0229, -0.00679)
.actigraph_rescale_factor <- 17.127404
.actigraph_downsample_target_hz <- 30
.actigraph_final_sample_hz <- 10
.actigraph_threshold_max <- 128
.actigraph_threshold_min <- 4
.actigraph_lfe_min_threshold <- 1

#' Activity Counts
#'
#' Calculate activity counts from raw acceleration using ActiGraph's official algorithm.
#'
#' @param acceleration Numeric vector of raw acceleration (in g)
#' @param sampling_freq Sampling frequency in Hz (default: 60)
#' @param epoch_length Epoch length in seconds (default: 60)
#' @param lfe_mode Low Frequency Extension mode (default: FALSE)
#' @return Numeric vector of counts per epoch
#' @export
counts <- function(acceleration,
                   sampling_freq = 60,
                   epoch_length = 60,
                   lfe_mode = FALSE) {

  if (length(acceleration) == 0) {
    return(numeric(0))
  }

  if (sampling_freq <= 0 || epoch_length <= 0) {
    stop("sampling_freq and epoch_length must be positive")
  }

  if (any(is.na(acceleration))) {
    warning("NA values detected in acceleration data (", sum(is.na(acceleration)),
            " values). Replacing with 0.")
    acceleration[is.na(acceleration)] <- 0
  }

  downsample.factor.1 <- sampling_freq / .actigraph_downsample_target_hz
  if (downsample.factor.1 > 1) {
    signal.30hz <- gsignal::decimate(acceleration,
                                     q = round(downsample.factor.1),
                                     ftype = "iir")
  } else {
    signal.30hz <- acceleration
  }

  zi <- gsignal::filter_zi(.actigraph_iir_coef_b, .actigraph_iir_coef_a)
  filtered.result <- gsignal::filter(.actigraph_iir_coef_b, .actigraph_iir_coef_a,
                                     signal.30hz, zi * signal.30hz[1])
  filtered.signal <- filtered.result$y

  rescaled.signal <- filtered.signal * .actigraph_rescale_factor

  abs.signal <- abs(rescaled.signal)

  if (lfe_mode) {
    thresholded.signal <- abs.signal
    thresholded.signal[thresholded.signal > .actigraph_threshold_max] <- .actigraph_threshold_max
    below.min <- thresholded.signal < .actigraph_threshold_min
    thresholded.signal[below.min] <- floor(thresholded.signal[below.min]) - 1
    thresholded.signal[thresholded.signal < .actigraph_lfe_min_threshold] <- 0
  } else {
    thresholded.signal <- abs.signal
    thresholded.signal[thresholded.signal > .actigraph_threshold_max] <- .actigraph_threshold_max
    thresholded.signal[thresholded.signal < .actigraph_threshold_min] <- 0
  }

  n.samples <- length(thresholded.signal)
  if (n.samples < 3) {
    warning("Insufficient samples (", n.samples, ") to compute activity counts. Need at least 3 samples.")
    return(numeric(0))
  }

  cumsum.signal <- cumsum(thresholded.signal)
  cumsum.shifted <- c(0, 0, 0, cumsum.signal[1:(n.samples - 3)])
  rolling.sum <- cumsum.signal - cumsum.shifted
  signal.10hz <- floor(rolling.sum[seq(3, n.samples, by = 3)] / 3)

  samples.per.epoch <- .actigraph_final_sample_hz * epoch_length
  n.epochs <- floor(length(signal.10hz) / samples.per.epoch)

  if (n.epochs == 0) {
    warning("Insufficient data to create even one epoch")
    return(numeric(0))
  }

  counts <- numeric(n.epochs)
  for (i in 1:n.epochs) {
    start.idx <- (i - 1) * samples.per.epoch + 1
    end.idx <- i * samples.per.epoch
    counts[i] <- sum(signal.10hz[start.idx:end.idx])
  }

  return(counts)
}

#' Vector Magnitude
#'
#' @param counts_x X-axis counts
#' @param counts_y Y-axis counts
#' @param counts_z Z-axis counts
#' @return Vector magnitude counts
#' @export
vm <- function(counts_x, counts_y, counts_z) {
  if (length(counts_x) != length(counts_y) || length(counts_x) != length(counts_z)) {
    stop("All count vectors must have the same length")
  }
  sqrt(counts_x^2 + counts_y^2 + counts_z^2)
}
