port <- as.integer(Sys.getenv("CANHR_SHINY_PORT", "3838"))
host <- Sys.getenv("CANHR_SHINY_HOST", "127.0.0.1")

options(
  shiny.port = port,
  shiny.host = host,
  shiny.launch.browser = FALSE,
  shiny.autoreload = FALSE,
  shiny.maxRequestSize = 500 * 1024^2,
  browser = "false"
)

bundled_lib <- file.path(R.home(), "library")
if (dir.exists(bundled_lib)) {
  .libPaths(bundled_lib)
}

suppressPackageStartupMessages({
  library(canhrActi)
  library(shiny)
})

if (!("plot_periodogram" %in% getNamespaceExports("canhrActi"))) {
  stop("Bundled canhrActi is out of date. Rebuild with `npm run setup:packages`.", call. = FALSE)
}

cat("__CANHRACTI_READY__\n", file = stdout())
flush(stdout())

shiny::runApp(
  appDir = system.file("shiny", "canhrActi_dashboard", package = "canhrActi"),
  port = port,
  host = host,
  launch.browser = FALSE,
  quiet = TRUE
)
