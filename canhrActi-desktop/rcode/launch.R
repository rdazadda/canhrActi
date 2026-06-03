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

# Pin the library path to the bundled R library so a stale per-user
# canhrActi (e.g. an old copy in the default win-library) cannot shadow the
# bundled one and load a namespace without the current exports.
bundled_lib <- file.path(R.home(), "library")
if (dir.exists(bundled_lib)) {
  .libPaths(bundled_lib)
}

suppressPackageStartupMessages({
  library(canhrActi)
  library(shiny)
})

# Fail fast with a clear message if the bundled canhrActi is stale/incomplete,
# rather than launching a dashboard whose plots die inside tryCatch.
if (!("plot_periodogram" %in% getNamespaceExports("canhrActi"))) {
  stop("Bundled canhrActi is out of date (missing plot_* exports). ",
       "Rebuild the app library with `npm run setup:packages`.", call. = FALSE)
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
