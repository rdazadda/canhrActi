#' Calculate Validation Metrics
#' @param reference Numeric vector of reference/gold standard values
#' @param test Numeric vector of test/new method values
#' @return List of bias, MAE, MAPE, RMSE, r, ICC, LoA, n
#' @export
validation.metrics <- function(reference, test) {
  if (length(reference) != length(test)) stop("reference and test must have same length")
  valid.idx <- !is.na(reference) & !is.na(test)
  reference <- reference[valid.idx]
  test <- test[valid.idx]
  if (length(reference) == 0) stop("No valid paired observations after removing NAs")

  error <- test - reference
  absolute.error <- abs(error)
  percent.error <- (error / reference) * 100

  mean.diff <- mean(error)
  sd.diff <- sd(error)
  loa.lower <- mean.diff - 1.96 * sd.diff
  loa.upper <- mean.diff + 1.96 * sd.diff

  metrics <- list(
    mean_error = mean(error),
    mean_absolute_error = mean(absolute.error),
    mean_percent_error = mean(percent.error, na.rm = TRUE),
    rmse = sqrt(mean(error^2)),
    correlation = cor(reference, test, use = "complete.obs"),
    icc = icc(reference, test),
    loa_lower = loa.lower,
    loa_upper = loa.upper,
    n = length(reference)
  )
  class(metrics) <- c("canhrActi_validation", "list")
  return(metrics)
}

#' Calculate Intraclass Correlation Coefficient (ICC)
#' @noRd
icc <- function(x, y) {
  if (length(x) != length(y)) stop("x and y must have same length")
  data <- data.frame(value = c(x, y), method = rep(c("x", "y"), each = length(x)), subject = rep(1:length(x), 2))
  grand.mean <- mean(data$value)
  subject.means <- tapply(data$value, data$subject, mean)
  bs.var <- var(subject.means)
  subject.vars <- tapply(data$value, data$subject, var)
  ws.var <- mean(subject.vars, na.rm = TRUE)
  if (is.na(ws.var) || ws.var == 0) ws.var <- 1e-10
  icc <- bs.var / (bs.var + ws.var)
  return(icc)
}

#' Print Method for Validation Metrics
#' @export
print.canhrActi_validation <- function(x, ...) {
  cat("\nValidation Metrics (n =", x$n, ")\n")
  cat("Bias:", round(x$mean_error, 2), "\n")
  cat("MAE:", round(x$mean_absolute_error, 2), "\n")
  cat("RMSE:", round(x$rmse, 2), "\n")
  cat("r:", round(x$correlation, 3), "\n")
  cat("ICC:", round(x$icc, 3), "\n")
  cat("LoA:", round(x$loa_lower, 2), "to", round(x$loa_upper, 2), "\n")
  invisible(x)
}

#' Confusion Matrix Metrics (binary)
#' @export
confusion.metrics <- function(reference, predicted) {
  if (length(reference) != length(predicted)) {
    stop("reference and predicted must have same length")
  }
  reference <- as.logical(reference)
  predicted <- as.logical(predicted)
  tp <- sum(predicted & reference)
  tn <- sum(!predicted & !reference)
  fp <- sum(predicted & !reference)
  fn <- sum(!predicted & reference)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- tp / (tp + fp)
  f1.score <- 2 * (precision * sensitivity) / (precision + sensitivity)

  list(
    true_positive = tp,
    true_negative = tn,
    false_positive = fp,
    false_negative = fn,
    sensitivity = sensitivity,
    specificity = specificity,
    accuracy = accuracy,
    precision = precision,
    f1_score = f1.score
  )
}
