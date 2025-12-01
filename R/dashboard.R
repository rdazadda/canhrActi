#' Launch canhrActi Dashboard
#'
#' Launches the interactive Shiny dashboard for accelerometer data analysis.
#' The dashboard provides a user-friendly interface for all canhrActi analysis
#' functions including wear time detection, sleep analysis, physical activity
#' classification, circadian rhythm analysis, and batch processing.
#'
#' @param launch.browser Logical. Whether to launch the app in the default web browser.
#'   Default is TRUE. Set to FALSE to run in RStudio viewer.
#' @param port Integer. The TCP port for the Shiny app. Default is NULL (random port).
#' @param host Character. The hostname. Default is "127.0.0.1" (localhost).
#'
#' @return Invisibly returns the Shiny app object.
#'
#' @details
#' The dashboard includes the following modules:
#' \itemize{
#'   \item \strong{Data Upload}: Load AGD, GT3X, or CSV files, configure subject info
#'   \item \strong{Overview}: Quick summary of data quality and key metrics
#'   \item \strong{Wear Time}: Detect wear/non-wear time using Choi, Troiano, or CANHR algorithms
#'   \item \strong{Sleep Analysis}: Cole-Kripke, Sadeh, and Tudor-Locke sleep detection
#'   \item \strong{Physical Activity}: Intensity classification and MVPA bout detection
#'   \item \strong{Circadian Rhythm}: L5, M10, IS, IV, and cosinor analysis
#'   \item \strong{Batch Processing}: Process multiple files at once
#'   \item \strong{Export}: Generate CSV exports and HTML reports
#' }
#'
#' @section Required Packages:
#' The dashboard requires additional packages that are suggested dependencies:
#' \itemize{
#'   \item shiny
#'   \item bslib
#'   \item bsicons
#'   \item DT
#'   \item shinyjs
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the dashboard in default browser
#' run_dashboard()
#'
#' # Launch in RStudio viewer
#' run_dashboard(launch.browser = FALSE)
#'
#' # Launch on specific port
#' run_dashboard(port = 3838)
#' }
#'
#' @seealso
#' \code{\link{canhrActi}} for command-line analysis,
#' \code{\link{canhrActi.batch}} for batch processing
#'
#' @export
run_dashboard <- function(launch.browser = TRUE, port = NULL, host = "127.0.0.1") {

  # Check for required packages
  required_packages <- c("shiny", "bslib", "bsicons", "DT", "shinyjs")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]


  if (length(missing_packages) > 0) {
    stop(
      "The following packages are required for the dashboard but not installed:\n",
      paste("  -", missing_packages, collapse = "\n"),
      "\n\nInstall them with:\n",
      "  install.packages(c('", paste(missing_packages, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # Find app directory

app_dir <- system.file("shiny", "canhrActi_dashboard", package = "canhrActi")

  if (app_dir == "") {
    stop(
      "Could not find the dashboard app directory. ",
      "This may indicate a problem with the package installation.",
      call. = FALSE
    )
  }

  # Launch the app
  message("Starting canhrActi Dashboard...")

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}


#' @rdname run_dashboard
#' @export
canhrActi.dashboard <- run_dashboard
