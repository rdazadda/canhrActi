#' Vector Magnitude
#'
#' Calculate vector magnitude from triaxial activity counts.
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
