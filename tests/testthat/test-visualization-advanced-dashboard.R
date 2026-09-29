# launch_visualization_dashboard(): every output and control of the page is served

# Ids of the outputs and controls the UI declares, and the ones the server uses
.dashboard_ids <- function() {
  ids <- list(ui_out = character(), ui_in = character(), used_out = character(), used_in = character())
  add <- function(kind, id) ids[[kind]] <<- union(ids[[kind]], id)
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    name <- if (is.name(fn)) as.character(fn) else if (is.call(fn) && identical(fn[[1]], as.name("::"))) as.character(fn[[3]]) else ""
    first <- if (length(e) > 1) e[[2]] else NULL
    if (is.character(first) && grepl("Output$", name)) add("ui_out", first)
    if (is.character(first) && name == "downloadButton") add("ui_out", first)
    if (is.character(first) && grepl("Input$", name)) add("ui_in", first)
    if (name == "tabsetPanel" && is.character(e$id)) add("ui_in", e$id)
    if (name == "$" && is.name(first) && is.name(e[[3]])) {
      if (identical(first, as.name("output"))) add("used_out", as.character(e[[3]]))
      if (identical(first, as.name("input"))) add("used_in", as.character(e[[3]]))
    }
    args <- as.list(e)[-1]
    for (i in seq_along(args)) {
      if (!identical(args[[i]], quote(expr = ))) walk(args[[i]])
    }
  }
  walk(body(launch_visualization_dashboard))
  ids
}

# Friday and Saturday in Alaska in 15 min epochs: 08:00 to 12:00 at 1980 counts per minute
# (moderate for Freedson, light for the custom set) with 100 steps an epoch, 20 lux at night
.dash_data <- function() {
  ts <- seq(as.POSIXct("2024-03-01 00:00:00", tz = "America/Anchorage"), by = 900, length.out = 2 * 96)
  h <- as.POSIXlt(ts)$hour
  active <- h >= 8 & h < 12
  data.frame(timestamp = ts, axis1 = ifelse(active, 1980 * 15, 50), steps = ifelse(active, 100, 0),
             inclinometer = ifelse(h >= 8 & h < 20, "standing", "lying"),
             lux = ifelse(h >= 8 & h < 20, 500, 20))
}

# testServer() attaches shiny; put the search path back as it was
.local_shiny_search <- function(env = parent.frame()) {
  attached <- "package:shiny" %in% search()
  withr::defer({
    if (!attached && "package:shiny" %in% search()) detach("package:shiny", character.only = TRUE)
  }, envir = env)
}

test_that("every output the dashboard shows has server code and every control is read", {
  ids <- .dashboard_ids()
  expect_true(all(c("timeline_plot", "summary_bars", "summary_table", "download_plot", "download_all") %in% ids$ui_out))
  expect_identical(setdiff(ids$ui_out, ids$used_out), character(0))
  expect_identical(setdiff(ids$used_out, ids$ui_out), character(0))
  expect_identical(setdiff(ids$ui_in, ids$used_in), character(0))
})

test_that("the daily summary tab gives each local day's steps and minutes on the chosen cut points", {
  skip_if_not_installed("shiny")
  .local_shiny_search()
  app <- launch_visualization_dashboard(.dash_data(), launch = FALSE)

  suppressPackageStartupMessages(shiny::testServer(app, {
    session$setInputs(date_range = as.Date(c("2024-03-01", "2024-03-02")), metrics = "axis1",
                      show_cutpoints = TRUE, cutpoint_set = "freedson_adult", show_inclinometer = FALSE,
                      equal_scales = TRUE, y_max = 5000, main_tabs = "Daily Summary")
    # the date range is read on the recording's clock, so no epoch is lost
    expect_identical(nrow(filtered_data()), 192L)

    s <- summary_plot()$data
    expect_identical(sort(unique(s$date)), as.Date(c("2024-03-01", "2024-03-02")))
    expect_equal(s$value[s$metric == "steps"], c(1600, 1600))
    expect_equal(s$value[s$metric == "mvpa_min"], c(240, 240))
    expect_equal(s$value[s$metric == "sedentary_min"], c(1200, 1200))
    expect_type(output$summary_bars, "list")
    table_html <- output$summary_table
    for (cell in c("Date", "Steps", "MVPA (min)", "Sedentary (min)", "Fri 03/01/2024", "Sat 03/02/2024",
                   "1,600", "240", "1,200")) {
      expect_match(table_html, cell, fixed = TRUE)
    }

    # the custom set starts moderate at 2000 counts per minute
    session$setInputs(cutpoint_set = "custom")
    s <- summary_plot()$data
    expect_equal(s$value[s$metric == "mvpa_min"], c(0, 0))

    # the posture plots come back bare for a single plot
    expect_type(output$incl_pie, "list")
    expect_type(output$incl_hourly, "list")
  }))
})

test_that("Download All Plots zips every plot the columns allow", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("zip")
  .local_shiny_search()
  # small figures keep the test quick
  real_export <- export_all_plots
  local_mocked_bindings(export_all_plots = function(data, output_dir, ...) {
    real_export(data, output_dir = output_dir, width = 3, height = 2, dpi = 40)
  })
  app <- launch_visualization_dashboard(.dash_data(), launch = FALSE)

  suppressMessages(shiny::testServer(app, {
    session$setInputs(date_range = as.Date(c("2024-03-01", "2024-03-02")), main_tabs = "Activity Timeline")
    zipped <- output$download_all
    expect_identical(tools::file_ext(zipped), "zip")
    # the download sits in a folder of its own in tempdir(), removed after reading
    folder <- dirname(zipped)
    files <- tryCatch(zip::zip_list(zipped)$filename, finally = {
      if (normalizePath(dirname(folder)) == normalizePath(tempdir())) unlink(folder, recursive = TRUE)
    })
    expect_setequal(files, paste0(c("01_daily_timeline", "02_activity_heatmap", "03_inclinometer_pie",
                                    "04_light_exposure", "05_light_summary", "06_steps_daily", "07_steps_cumulative",
                                    "10_day_comparison_overlay", "11_day_comparison_facet", "12_weekend_weekday"),
                                  ".png"))
  }))
})

test_that("Download Current Plot saves what the open tab shows, and the sleep tab draws a sleep column", {
  skip_if_not_installed("shiny")
  .local_shiny_search()
  saved <- NULL
  local_mocked_bindings(ggsave = function(filename, plot, ...) {
    saved <<- plot
    file.create(filename)
  }, .package = "ggplot2")
  d <- .dash_data()
  # asleep from midnight to six
  d$sleep <- ifelse(as.POSIXlt(d$timestamp)$hour < 6, "S", "W")
  app <- launch_visualization_dashboard(d, launch = FALSE)

  suppressMessages(shiny::testServer(app, {
    session$setInputs(date_range = as.Date(c("2024-03-01", "2024-03-02")), metrics = "axis1",
                      show_cutpoints = TRUE, cutpoint_set = "freedson_adult", show_inclinometer = FALSE,
                      equal_scales = TRUE, y_max = 5000, heatmap_metric = "steps", step_goal = 10000)
    expect_s3_class(sleep_p(), "ggplot")
    shown <- list("Activity Timeline" = timeline_p(), "Activity Heatmap" = heatmap_p(),
                  "Sleep Analysis" = sleep_p(), "Daily Summary" = summary_plot())
    for (tab in names(shown)) {
      session$setInputs(main_tabs = tab)
      output$download_plot
      expect_identical(saved, shown[[tab]])
    }
    if (requireNamespace("patchwork", quietly = TRUE)) {
      for (tab in c("Inclinometer/Posture", "Light Exposure", "Steps")) {
        session$setInputs(main_tabs = tab)
        output$download_plot
        expect_s3_class(saved, "patchwork")
      }
    }
  }))

  app <- launch_visualization_dashboard(.dash_data(), launch = FALSE)
  suppressMessages(shiny::testServer(app, {
    session$setInputs(date_range = as.Date(c("2024-03-01", "2024-03-02")))
    expect_error(sleep_p(), "sleep_state, sleep_wake or sleep column")
  }))
})
