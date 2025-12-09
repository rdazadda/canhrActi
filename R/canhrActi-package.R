#' canhrActi: CANHR ActiGraph Physical Activity Analysis
#'
#' @importFrom utils flush.console head
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check notes for ggplot2 aes variables
utils::globalVariables(c("category", "count", "bout_percent",
                         "cumulative_percent", "duration_min"))
