# The dashboard launchers start with only the packages they use

test_that("run_dashboard checks the packages app.R attaches and launches the app folder", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinydashboard")
  skip_if_not_installed("shinyjs")

  # The package vector the function checks before it starts
  checked <- NULL
  for (e in as.list(body(run_dashboard))) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("required_packages"))) {
      checked <- eval(e[[3]], baseenv())
    }
  }
  expect_setequal(checked, c("shiny", "shinydashboard", "shinyjs"))

  launched <- NULL
  local_mocked_bindings(runApp = function(...) {
    launched <<- list(...)
    invisible(NULL)
  }, .package = "shiny")
  expect_message(run_dashboard(launch.browser = FALSE, port = 3838L, host = "127.0.0.1"),
                 "Starting canhrActi Dashboard")
  app_dir <- system.file("shiny", "canhrActi_dashboard", package = "canhrActi")
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_identical(launched, list(appDir = app_dir, launch.browser = FALSE, port = 3838L, host = "127.0.0.1"))
  expect_identical(canhrActi.dashboard, run_dashboard)
})

test_that("launch_visualization_dashboard runs its server without shiny attached", {
  skip_if_not_installed("shiny")
  # testServer() attaches shiny, so the server is run after shiny is detached again;
  # the search path is put back as it was at the end
  attached <- "package:shiny" %in% search()
  pos <- match("package:shiny", search())
  withr::defer({
    if (attached && !"package:shiny" %in% search()) {
      suppressPackageStartupMessages(library("shiny", pos = pos, character.only = TRUE))
    }
    if (!attached && "package:shiny" %in% search()) detach("package:shiny", character.only = TRUE)
  })

  # Two days in 15-minute epochs
  ts <- seq(as.POSIXct("2024-03-01 00:00:00", tz = "UTC"), by = 900, length.out = 2 * 96)
  app <- launch_visualization_dashboard(data.frame(timestamp = ts, axis1 = 100), launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
  upload <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(timestamp = ts[1:96], axis1 = 5), upload)

  suppressPackageStartupMessages(shiny::testServer(app, {
    detach("package:shiny", character.only = TRUE)
    session$setInputs(date_range = as.Date(c("2024-03-01", "2024-03-02")), metrics = "axis1",
                      show_cutpoints = TRUE, cutpoint_set = "freedson_adult",
                      show_inclinometer = FALSE, equal_scales = TRUE, y_max = 5000)
    expect_equal(nrow(filtered_data()), 2 * 96)
    expect_type(output$timeline_plot, "list")
    # A false condition in req() stops the output silently
    expect_error(output$incl_pie, class = "shiny.silent.error")
    session$setInputs(data_file = data.frame(name = "upload.rds", datapath = upload))
    expect_equal(nrow(app_data()), 96)
  }))
})
