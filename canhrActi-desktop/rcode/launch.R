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

suppressPackageStartupMessages({
  library(canhrActi)
  library(shiny)
})

cat("__CANHRACTI_READY__\n", file = stdout())
flush(stdout())

shiny::runApp(
  appDir = system.file("shiny", "canhrActi_dashboard", package = "canhrActi"),
  port = port,
  host = host,
  launch.browser = FALSE,
  quiet = TRUE
)
