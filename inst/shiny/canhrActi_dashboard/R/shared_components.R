# 
# CANHR ACTIGRAPH DASHBOARD - COMPONENT LIBRARY
# University of Alaska Fairbanks
#
# This file provides a consistent, reusable component API to replace
# shinydashboard defaults with cleaner, more modern components.
#
# Components:
#   1. metric_display() - Replaces valueBox() with cleaner metrics
#   2. data_card() - Replaces box() with modern card design
#   3. chart_panel() - Specialized for charts with controls
#   4. control_group() - Groups related form controls
#   5. page_layout() - Consistent page structure
#   6. empty_state() - When no data/results available
#   7. status_pill() - Small status indicators
#   8. workflow_header() - Progress through workflow steps
#
# Usage: Source this file BEFORE other modules in app.R
# 

# Null coalesce operator (defined once, used everywhere)
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# 
# 1. METRIC DISPLAY
# 
#' Metric Display Component
#'
#' A clean, modern metric display that replaces shinydashboard::valueBox().
#' Provides better visual hierarchy and optional trend indicators.
#'
#' @param value The main metric value (string or number)
#' @param label Primary label below the value
#' @param subtitle Optional secondary label/description
#' @param trend Optional trend indicator: list(direction = "up"|"down"|"flat", value = "5%")
#' @param size Size variant: "sm", "md", "lg" (default: "md")
#' @param color_accent Optional accent color: "primary", "success", "warning", "info" (default: "primary")
#' @param icon_name Optional FontAwesome icon name (without "fa-" prefix)
#'
#' @return A Shiny tag object
#'
#' @examples
#' metric_display("14.2h", "Avg Wear Time", "Per valid day", size = "lg")
#' metric_display("85%", "Data Quality", trend = list(direction = "up", value = "+3%"))
#' metric_display(72, "MVPA Minutes", icon_name = "running")
#'
metric_display <- function(value,
                           label,
                           subtitle = NULL,
                           trend = NULL,
                           size = c("md", "sm", "lg"),
                           color_accent = c("primary", "success", "warning", "info"),
                           icon_name = NULL) {
  size <- match.arg(size)
  color_accent <- match.arg(color_accent)

  # Size-based CSS classes
  size_class <- paste0("canhr-metric-", size)

  # Build trend indicator if provided
  trend_html <- NULL
  if (!is.null(trend) && is.list(trend)) {
    trend_icon <- switch(trend$direction %||% "flat",
      "up" = "arrow-up",
      "down" = "arrow-down",
      "caret-right"
    )
    trend_class <- switch(trend$direction %||% "flat",
      "up" = "canhr-trend-up",
      "down" = "canhr-trend-down",
      "canhr-trend-flat"
    )
    trend_html <- tags$span(
      class = paste("canhr-trend", trend_class),
      icon(trend_icon),
      trend$value %||% ""
    )
  }

  # Build icon if provided
  icon_html <- NULL
  if (!is.null(icon_name)) {
    icon_html <- tags$div(
      class = "canhr-metric-icon",
      icon(icon_name)
    )
  }

  # Assemble the component
  tags$div(
    class = paste("canhr-metric", size_class, paste0("canhr-accent-", color_accent)),
    if (!is.null(icon_html)) icon_html,
    tags$div(
      class = "canhr-metric-content",
      tags$div(class = "canhr-metric-value", value),
      tags$div(class = "canhr-metric-label", label),
      if (!is.null(subtitle)) tags$div(class = "canhr-metric-subtitle", subtitle),
      if (!is.null(trend_html)) trend_html
    )
  )
}

#' Metric Display Output (Server-side rendering)
#'
#' Use this to render metric_display() from server side
#'
#' @param outputId Output ID for the metric
#' @param ... Additional arguments passed to uiOutput
#'
metric_display_output <- function(outputId, ...) {
  tags$div(
    class = "canhr-metric-placeholder",
    uiOutput(outputId, ...)
  )
}

# 
# 2. DATA CARD
# 
#' Data Card Component
#'
#' A cleaner replacement for shinydashboard::box() with modern styling,
#' optional status indicators, and better header/footer handling.
#'
#' @param title Card title (can include icons via tagList)
#' @param ... Card body content
#' @param status Optional status: "primary", "success", "warning", "info" (adds colored top border)
#' @param collapsible Logical, whether card can be collapsed (default: FALSE)
#' @param collapsed Logical, whether card starts collapsed (default: FALSE)
#' @param footer Optional footer content (for action buttons, etc.)
#' @param width Bootstrap column width (1-12, or NULL for full width)
#' @param header_extra Extra content in header (right side)
#' @param id Optional ID for the card element
#' @param fill Logical, whether to fill available height (default: FALSE)
#'
#' @return A Shiny tag object
#'
#' @examples
#' data_card("Settings", status = "primary",
#'           selectInput("algo", "Algorithm", choices = c("A", "B")),
#'           footer = actionButton("run", "Run"))
#'
data_card <- function(title = NULL,
                      ...,
                      status = NULL,
                      collapsible = FALSE,
                      collapsed = FALSE,
                      footer = NULL,
                      width = NULL,
                      header_extra = NULL,
                      id = NULL,
                      fill = FALSE) {

  # Status class
  status_class <- if (!is.null(status)) paste0("canhr-card-", status) else ""

  # Collapsible handling
  collapse_class <- if (collapsible && collapsed) "canhr-card-collapsed" else ""

  # Build header if title provided
  header_html <- NULL
  if (!is.null(title)) {
    header_content <- tags$div(
      class = "canhr-card-title",
      if (is.character(title)) title else title
    )

    collapse_btn <- NULL
    if (collapsible) {
      collapse_btn <- tags$button(
        class = "canhr-card-collapse-btn",
        type = "button",
        onclick = "$(this).closest('.canhr-card').toggleClass('canhr-card-collapsed');",
        icon(if (collapsed) "chevron-down" else "chevron-up")
      )
    }

    header_html <- tags$div(
      class = "canhr-card-header",
      header_content,
      if (!is.null(header_extra)) tags$div(class = "canhr-card-header-extra", header_extra),
      collapse_btn
    )
  }

  # Build footer if provided
  footer_html <- NULL
  if (!is.null(footer)) {
    footer_html <- tags$div(class = "canhr-card-footer", footer)
  }

  # Assemble card
  card <- tags$div(
    id = id,
    class = paste("canhr-card", status_class, collapse_class,
                  if (fill) "canhr-card-fill" else ""),
    header_html,
    tags$div(class = "canhr-card-body", ...),
    footer_html
  )

  # Wrap in column if width specified
  if (!is.null(width)) {
    column(width = width, card)
  } else {
    card
  }
}

# 
# 3. CHART PANEL
# 
#' Chart Panel Component
#'
#' A specialized panel for displaying charts with optional controls.
#' Handles proper aspect ratios and provides a clean container for plots.
#'
#' @param title Panel title
#' @param chart_output The chart output (e.g., plotOutput(ns("plot")))
#' @param controls Optional controls (displayed in header or collapsible sidebar)
#' @param controls_position Where to show controls: "header", "sidebar", "none" (default: "none")
#' @param height Chart height in pixels or CSS units (default: "400px")
#' @param status Optional status: "primary", "success", "warning", "info"
#' @param subtitle Optional subtitle below chart
#' @param download_btn Optional download button ID (will render downloadButton)
#'
#' @return A Shiny tag object
#'
#' @examples
#' chart_panel("Activity Timeline",
#'             plotOutput(ns("timeline_plot")),
#'             height = "350px",
#'             controls = selectInput(ns("metric"), "Metric", c("VM", "Axis1")),
#'             controls_position = "header")
#'
chart_panel <- function(title,
                        chart_output,
                        controls = NULL,
                        controls_position = c("none", "header", "sidebar"),
                        height = "400px",
                        status = NULL,
                        subtitle = NULL,
                        download_btn = NULL) {

  controls_position <- match.arg(controls_position)
  status_class <- if (!is.null(status)) paste0("canhr-chart-", status) else ""

  # Build controls section based on position
  controls_html <- NULL
  if (!is.null(controls) && controls_position != "none") {
    if (controls_position == "header") {
      controls_html <- tags$div(class = "canhr-chart-controls-header", controls)
    } else if (controls_position == "sidebar") {
      controls_html <- tags$div(
        class = "canhr-chart-controls-sidebar",
        tags$div(class = "canhr-chart-controls-inner", controls)
      )
    }
  }

  # Download button
  download_html <- NULL
  if (!is.null(download_btn)) {
    download_html <- tags$div(
      class = "canhr-chart-download",
      downloadButton(download_btn, label = "", class = "btn-sm canhr-btn-icon")
    )
  }

  # Build header
  header_html <- tags$div(
    class = "canhr-chart-header",
    tags$div(
      class = "canhr-chart-title-area",
      tags$h4(class = "canhr-chart-title", title),
      if (!is.null(subtitle)) tags$p(class = "canhr-chart-subtitle", subtitle)
    ),
    if (controls_position == "header" && !is.null(controls_html)) controls_html,
    download_html
  )

  # Chart wrapper with proper sizing
  chart_wrapper <- tags$div(
    class = "canhr-chart-wrapper",
    style = paste0("height: ", height, ";"),
    chart_output
  )

  # Assemble the panel
  tags$div(
    class = paste("canhr-chart-panel", status_class),
    header_html,
    tags$div(
      class = "canhr-chart-content",
      if (controls_position == "sidebar" && !is.null(controls_html)) controls_html,
      chart_wrapper
    )
  )
}

# 
# 4. CONTROL GROUP
# 
#' Control Group Component
#'
#' Groups related form controls with consistent label styling and optional help text.
#' Use this to organize settings and parameters.
#'
#' @param label Group label (displayed above controls)
#' @param ... Form controls (inputs, selects, etc.)
#' @param help_text Optional help text displayed below controls
#' @param inline Logical, arrange controls horizontally (default: FALSE)
#' @param background Add subtle background (default: FALSE)
#' @param icon_name Optional icon for the label
#'
#' @return A Shiny tag object
#'
#' @examples
#' control_group("Algorithm Settings",
#'               selectInput("algo", "Algorithm", c("A", "B")),
#'               numericInput("threshold", "Threshold", 100),
#'               help_text = "Select the scoring algorithm for activity classification")
#'
control_group <- function(label,
                          ...,
                          help_text = NULL,
                          inline = FALSE,
                          background = FALSE,
                          icon_name = NULL) {

  # Build label with optional icon
  label_html <- if (!is.null(icon_name)) {
    tags$label(class = "canhr-control-group-label", icon(icon_name), " ", label)
  } else {
    tags$label(class = "canhr-control-group-label", label)
  }

  # Controls container
  controls_class <- if (inline) "canhr-controls-inline" else "canhr-controls-stacked"

  # Help text
  help_html <- if (!is.null(help_text)) {
    tags$p(class = "canhr-control-help", icon("info-circle"), help_text)
  }

  # Container class
  container_class <- paste("canhr-control-group",
                           if (background) "canhr-control-group-bg" else "")

  tags$div(
    class = container_class,
    label_html,
    tags$div(class = controls_class, ...),
    help_html
  )
}

# 
# 5. PAGE LAYOUT
# 
#' Page Layout Component
#'
#' Provides consistent page structure with title, subtitle, optional metrics strip,
#' and main content area. Use this as the top-level wrapper for each tab/page.
#'
#' @param title Page title
#' @param subtitle Page subtitle/description
#' @param icon_name Icon for the page header
#' @param metrics_area Optional row of metric displays at the top
#' @param main_content The main page content
#' @param status_output_id Optional output ID for a status badge in the header
#' @param header_actions Optional action buttons in header (right side)
#'
#' @return A Shiny tag object
#'
#' @examples
#' page_layout(
#'   title = "Physical Activity",
#'   subtitle = "Analyze activity intensity and MVPA",
#'   icon_name = "running",
#'   metrics_area = fluidRow(
#'     metric_display("72m", "MVPA", size = "lg"),
#'     metric_display("8.5h", "Sedentary", size = "lg")
#'   ),
#'   main_content = tagList(
#'     data_card("Settings", ...)
#'   )
#' )
#'
page_layout <- function(title,
                        subtitle = NULL,
                        icon_name = NULL,
                        metrics_area = NULL,
                        main_content,
                        status_output_id = NULL,
                        header_actions = NULL) {

  # Page header
  header_html <- tags$div(
    class = "canhr-page-header",
    tags$div(
      class = "canhr-page-header-content",
      if (!is.null(icon_name)) tags$div(class = "canhr-page-header-icon", icon(icon_name)),
      tags$div(
        class = "canhr-page-header-text",
        tags$h2(class = "canhr-page-title", title),
        if (!is.null(subtitle)) tags$p(class = "canhr-page-subtitle", subtitle)
      )
    ),
    tags$div(
      class = "canhr-page-header-actions",
      if (!is.null(status_output_id)) tags$div(class = "canhr-page-status", uiOutput(status_output_id)),
      if (!is.null(header_actions)) header_actions
    )
  )

  # Metrics strip if provided
  metrics_html <- NULL
  if (!is.null(metrics_area)) {
    metrics_html <- tags$div(
      class = "canhr-metrics-strip",
      metrics_area
    )
  }

  # Assemble the page
  tagList(
    header_html,
    metrics_html,
    tags$div(class = "canhr-page-content", main_content)
  )
}

#' Page Header (Standalone)
#'
#' A simpler standalone header for pages that don't need full page_layout
#'
#' @param icon_name FontAwesome icon name
#' @param title Page title
#' @param subtitle Page subtitle
#' @param status_output_id Optional output ID for status badge
#'
page_header <- function(icon_name, title, subtitle, status_output_id = NULL) {
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "page-header",
          fluidRow(
            column(
              width = if (!is.null(status_output_id)) 8 else 12,
              h3(icon(icon_name), title),
              p(subtitle)
            ),
            if (!is.null(status_output_id)) {
              column(
                width = 4,
                div(class = "header-status", uiOutput(status_output_id))
              )
            }
          )
        )
      )
    )
  )
}

# 
# 6. EMPTY STATE
# 
#' Empty State Component
#'
#' Display a friendly message when no data or results are available.
#' Includes optional icon, title, message, and call-to-action button.
#'
#' @param icon_name Optional FontAwesome icon name (use NULL to omit)
#' @param title Optional main message (e.g., "No Data Available")
#' @param message Optional secondary message with more detail
#' @param action_button Optional action button (use actionButton())
#' @param small Use smaller variant (default: FALSE)
#' @param show_icon Whether to render the icon (default: TRUE when icon_name is set)
#' @param include_base Whether to include the base empty-state container styles
#' @param extra_class Additional class names to apply to the container
#'
#' @return A Shiny tag object
#'
#' @examples
#' empty_state("chart-bar", "No Activity Data",
#'             "Run 'Score Physical Activity' to see charts",
#'             action_button = actionButton("run", "Run Analysis", class = "btn-primary"))
#'
empty_state <- function(icon_name = NULL,
                        title = NULL,
                        message = NULL,
                        action_button = NULL,
                        small = FALSE,
                        show_icon = !is.null(icon_name),
                        include_base = TRUE,
                        extra_class = NULL) {
  classes <- c(
    if (include_base) "empty-state",
    if (small) "empty-state-sm",
    extra_class
  )
  classes <- paste(classes[!is.null(classes) & nzchar(classes)], collapse = " ")

  tags$div(
    class = classes,
    if (show_icon && !is.null(icon_name)) {
      tags$div(class = "empty-state-icon", icon(icon_name))
    },
    if (!is.null(title)) tags$div(class = "empty-state-title", title),
    if (!is.null(message)) tags$div(class = "empty-state-description", message),
    if (!is.null(action_button)) tags$div(class = "empty-state-action", action_button)
  )
}

# 
# 7. STATUS PILL
# 
#' Status Pill Component
#'
#' Small, colored status indicators for inline display.
#'
#' @param label Status text
#' @param status Status type: "pending", "processing", "success", "warning", "error"
#' @param icon_name Optional icon (default based on status)
#'
#' @return A Shiny tag object
#'
#' @examples
#' status_pill("Complete", "success")
#' status_pill("Processing...", "processing")
#' status_pill("Needs Review", "warning")
#'
status_pill <- function(label,
                        status = c("pending", "processing", "success", "warning", "error")) {
  status <- match.arg(status)

  # Default icons per status
  icon_map <- list(
    pending = "clock",
    processing = "spinner",
    success = "check-circle",
    warning = "exclamation-triangle",
    error = "times-circle"
  )

  # Icon with animation for processing
  icon_class <- if (status == "processing") "fa-spin" else ""

  tags$span(
    class = paste("canhr-status-pill", paste0("canhr-status-", status)),
    icon(icon_map[[status]], class = icon_class),
    label
  )
}

#' Status Badge (Legacy compatibility)
#'
#' Compatible with existing status_badge usage
#'
#' @param text Badge text
#' @param status Status type: "success", "pending", "caution"
#'
status_badge <- function(text, status = "pending") {
  valid_statuses <- c("success", "pending", "caution")
  status <- match.arg(status, valid_statuses)

  icon_name <- switch(status,
    "success" = "check-circle",
    "pending" = "info-circle",
    "caution" = "exclamation-triangle"
  )

  span(
    class = paste("status-badge", paste0("status-", status)),
    icon(icon_name), text
  )
}

# 
# 8. WORKFLOW HEADER
# 
#' Workflow Header Component
#'
#' Shows progress through a multi-step workflow (e.g., Upload -> Analyze -> Export).
#' Highlights the current step and shows completion status.
#'
#' @param current_step Index of current step (1-based)
#' @param steps Character vector of step names
#' @param completed_steps Integer vector of completed step indices
#'
#' @return A Shiny tag object
#'
#' @examples
#' workflow_header(
#'   current_step = 2,
#'   steps = c("Upload", "Configure", "Analyze", "Export"),
#'   completed_steps = c(1)
#' )
#'
workflow_header <- function(current_step,
                            steps,
                            completed_steps = integer(0)) {

  step_items <- lapply(seq_along(steps), function(i) {
    is_current <- i == current_step
    is_completed <- i %in% completed_steps
    is_future <- i > current_step && !is_completed

    step_class <- paste(
      "canhr-workflow-step",
      if (is_current) "canhr-workflow-current" else "",
      if (is_completed) "canhr-workflow-completed" else "",
      if (is_future) "canhr-workflow-future" else ""
    )

    # Step indicator (number or check)
    indicator <- if (is_completed) {
      tags$span(class = "canhr-workflow-indicator", icon("check"))
    } else {
      tags$span(class = "canhr-workflow-indicator", i)
    }

    tags$div(
      class = step_class,
      indicator,
      tags$span(class = "canhr-workflow-label", steps[i]),
      if (i < length(steps)) tags$span(class = "canhr-workflow-connector")
    )
  })

  tags$div(
    class = "canhr-workflow-header",
    tags$div(class = "canhr-workflow-steps", step_items)
  )
}

#' Workflow Header Output (Server-side rendering)
#'
#' Use this for dynamically updating workflow state
#'
#' @param outputId Output ID for the workflow header
#'
workflow_header_output <- function(outputId) {
  uiOutput(outputId, class = "canhr-workflow-container")
}

# 
# ADDITIONAL HELPER COMPONENTS
# 

#' Info Note
#'
#' Small informational text with icon
#'
#' @param text The info text
#'
info_note <- function(text) {
  p(class = "info-note", icon("info-circle"), text)
}

#' Tip Box
#'
#' Highlighted tip/hint box
#'
#' @param text Tip text
#'
tip_box <- function(text) {
  div(class = "tip-box", icon("lightbulb"), text)
}

#' Box with Icon (Legacy compatibility)
#'
#' Creates a box with icon in the title
#'
#' @param icon_name FontAwesome icon name
#' @param title Box title
#' @param ... Content and additional arguments passed to box()
#' @param status Box status
#' @param solidHeader Whether to use solid header
#'
box_with_icon <- function(icon_name, title, ..., status = NULL, solidHeader = FALSE) {
  box_title <- span(icon(icon_name), title)
  shinydashboard::box(
    title = box_title,
    status = status,
    solidHeader = solidHeader,
    ...
  )
}

#' Metric Card (Legacy compatibility)
#'
#' Simple metric card for quick displays
#'
#' @param value Metric value
#' @param label Metric label
#' @param sublabel Optional sublabel
#'
metric_card <- function(value, label, sublabel = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-value", value),
    div(class = "metric-label", label),
    if (!is.null(sublabel)) div(class = "metric-sublabel", sublabel)
  )
}

#' Score Card
#'
#' Colored score display card
#'
#' @param value Score value
#' @param label Score label
#' @param status Status: "good", "moderate", "caution"
#'
score_card <- function(value, label, status = "moderate") {
  valid_statuses <- c("good", "moderate", "caution")
  status <- match.arg(status, valid_statuses)

  div(
    class = paste("score-card", paste0("score-", status)),
    div(class = "score-value", value),
    div(class = "score-label", label)
  )
}

#' Quality Badge
#'
#' Badge showing data quality level
#'
#' @param percent Quality percentage
#'
quality_badge <- function(percent) {
  status <- if (percent >= 80) "good"
            else if (percent >= 50) "moderate"
            else "needs-attention"

  label <- if (percent >= 80) "Good"
           else if (percent >= 50) "Moderate"
           else "Needs Attention"

  span(
    class = paste("quality-badge", paste0("quality-", status)),
    paste0(round(percent), "% - ", label)
  )
}

#' Action Button with Icon
#'
#' Styled action button with icon
#'
#' @param id Button input ID
#' @param label Button label
#' @param icon_name FontAwesome icon name
#' @param class CSS classes (default: "btn-primary btn-block btn-lg")
#'
action_btn <- function(id, label, icon_name, class = "btn-primary btn-block btn-lg") {
  actionButton(id, span(icon(icon_name), label), class = class)
}

#' Download Button with Icon
#'
#' Styled download button with icon
#'
#' @param id Button output ID
#' @param label Button label
#' @param icon_name FontAwesome icon name
#' @param class CSS classes
#'
download_btn <- function(id, label, icon_name, class = "btn-default btn-block btn-sm") {
  downloadButton(id, span(icon(icon_name), label), class = class)
}

#' File Info Table
#'
#' Displays file information in a table format
#'
#' @param info_list Named list of info key-value pairs
#'
file_info_table <- function(info_list) {
  div(
    class = "file-info-box",
    tags$table(
      class = "info-table",
      lapply(names(info_list), function(key) {
        tags$tr(
          tags$td(key),
          tags$td(info_list[[key]])
        )
      })
    )
  )
}

#' Chart Container
#'
#' Container for plot outputs
#'
#' @param content Plot output or other content
#' @param large Use large variant (default: FALSE)
#'
chart_container <- function(content, large = FALSE) {
  div(
    class = if (large) "chart-container-lg" else "chart-container",
    content
  )
}

#' Processing Indicator
#'
#' Shows a spinner during processing
#'
#' @param id Element ID for showing/hiding
#' @param message Processing message
#'
processing_indicator <- function(id, message = "Processing...") {
  shinyjs::hidden(
    div(
      id = id,
      class = "processing-indicator",
      icon("spinner", class = "fa-spin fa-2x"),
      p(class = "processing-text", message),
      p(class = "processing-detail", "Please wait")
    )
  )
}

# 
# FORMATTING UTILITIES
# 

#' Format Duration
#'
#' Format seconds into human-readable duration (D H M format)
#'
#' @param total_seconds Duration in seconds
#'
format_duration <- function(total_seconds) {
  if (is.na(total_seconds) || total_seconds == 0) return("0d 0h 0m")
  days <- floor(total_seconds / 86400)
  hours <- floor((total_seconds %% 86400) / 3600)
  mins <- floor((total_seconds %% 3600) / 60)

  if (days > 0) {
    paste0(days, "d ", hours, "h ", mins, "m")
  } else if (hours > 0) {
    paste0(hours, "h ", mins, "m")
  } else {
    paste0(mins, "m")
  }
}

#' Format ETA
#'
#' Format ETA for progress messages
#'
#' @param seconds Estimated seconds remaining
#'
format_eta <- function(seconds) {
  if (is.na(seconds) || seconds < 0) return("calculating...")
  if (seconds < 60) return(paste0(round(seconds), "s"))
  if (seconds < 3600) return(paste0(round(seconds / 60, 1), "m"))
  return(paste0(round(seconds / 3600, 1), "h"))
}

# 
# CONSTANTS
# 

#' Dashboard Constants
#'
#' Commonly used constants for the dashboard
#'
DASHBOARD_CONSTANTS <- list(
  # Default values
  DEFAULT_EPOCH = 60,
  DEFAULT_AGE = 35,
  DEFAULT_BODY_MASS = 70,
  DEFAULT_MIN_WEAR_HOURS = 10,
  DEFAULT_MIN_VALID_DAYS = 3,
  DEFAULT_MVPA_GOAL = 30,

  # Timestamp conversion constants
  TICKS_PER_SECOND = 10000000,
  EPOCH_DIFF = 62135596800,

  # Messages
  MSG_NO_FILES = "No files loaded. Go to Upload tab.",
  MSG_RUN_ANALYSIS = "Run Analysis to see results.",
  MSG_NO_DATA = "No data available."
)

#' Plot Color Palette
#'
#' Consistent colors for plots throughout the dashboard
#'
PLOT_COLORS <- list(
  # Intensity colors
  sedentary = "#94a3b8",
  light = "#3a7ab0",
  moderate = "#FFCD00",
  vigorous = "#17a589",
  very_vigorous = "#236192",

  # Status colors
  valid = "#17a589",
  invalid = "#94a3b8",

  # Brand colors
  primary = "#236192",
  primary_dark = "#1a4a6f",
  accent = "#FFCD00",
  success = "#17a589",
  caution = "#f4b942",

  # Chart colors
  line = "#236192",
  fill = "#3a7ab0",
  grid = "#e2e8f0"
)

# 
# ACCESSIBILITY & UI UTILITIES
# 

#' Chart Empty State
#'
#' A specialized empty state component for chart panels.
#' Displays when no data is available to visualize.
#'
#' @param icon_name FontAwesome icon name (default: "chart-bar")
#' @param title Title text
#' @param message Description text
#' @param action_label Optional action button label
#' @param action_id Optional action button ID (for Shiny observer)
#' @param ns Namespace function for Shiny modules
#' @param show_icon Whether to render the icon (default: TRUE when icon_name is set)
#' @param extra_class Additional class names to apply to the container
#'
#' @return A Shiny tag object
#'
chart_empty_state <- function(icon_name = "chart-bar",
                              title = "No Data Available",
                              message = "Run Analysis to see visualizations",
                              action_label = NULL,
                              action_id = NULL,
                              ns = identity,
                              show_icon = !is.null(icon_name),
                              extra_class = NULL) {
  classes <- c("chart-empty-state", extra_class)
  classes <- paste(classes[!is.null(classes) & nzchar(classes)], collapse = " ")

  tags$div(
    class = classes,
    role = "status",
    `aria-live` = "polite",
    if (show_icon && !is.null(icon_name)) {
      tags$div(class = "empty-state-icon", icon(icon_name), `aria-hidden` = "true")
    },
    if (!is.null(title)) tags$div(class = "empty-state-title", title),
    if (!is.null(message)) tags$div(class = "empty-state-description", message),
    if (!is.null(action_label) && !is.null(action_id)) {
      tags$div(
        class = "empty-state-action",
        actionButton(
          ns(action_id),
          action_label,
          class = "btn btn-primary"
        )
      )
    }
  )
}

#' Loading Overlay
#'
#' A full-panel loading overlay for use during async operations.
#' Can be shown/hidden with shinyjs::show()/hide().
#'
#' @param id Element ID for show/hide control
#' @param message Loading message to display
#'
#' @return A Shiny tag object
#'
loading_overlay <- function(id, message = "Loading...") {
  shinyjs::hidden(
    tags$div(
      id = id,
      class = "loading-overlay",
      role = "status",
      `aria-live` = "polite",
      tags$div(class = "loading-spinner", `aria-hidden` = "true"),
      tags$div(class = "loading-text", message),
      tags$span(class = "sr-only", message)
    )
  )
}

#' Metric with Tooltip
#'
#' A metric display with an info tooltip for explanation.
#'
#' @param value The metric value
#' @param label The metric label
#' @param tooltip Tooltip text explaining the metric
#' @param icon_name Optional icon name
#'
#' @return A Shiny tag object
#'
metric_with_tooltip <- function(value, label, tooltip, icon_name = NULL) {
  tags$div(
    class = "metric",
    if (!is.null(icon_name)) {
      tags$div(class = "metric-icon", icon(icon_name), `aria-hidden` = "true")
    },
    tags$div(
      class = "metric-content",
      tags$div(
        class = "metric-value-row",
        tags$span(class = "metric-value", value),
        tags$span(
          class = "metric-info-icon tooltip-trigger",
          `data-tooltip` = tooltip,
          tabindex = "0",
          role = "button",
          `aria-label` = paste("Info about", label),
          icon("info-circle")
        )
      ),
      tags$div(class = "metric-label", label)
    )
  )
}

#' Accessible Button
#'
#' An action button with proper ARIA attributes.
#'
#' @param inputId Button input ID
#' @param label Button label
#' @param icon Icon to display (optional)
#' @param class CSS class(es)
#' @param disabled Whether button is disabled
#' @param aria_label Accessible label (defaults to label text)
#' @param ... Additional attributes
#'
#' @return A Shiny tag object
#'
accessible_button <- function(inputId,
                               label,
                               icon = NULL,
                               class = "btn-primary",
                               disabled = FALSE,
                               aria_label = NULL,
                               ...) {
  btn <- actionButton(
    inputId = inputId,
    label = label,
    icon = icon,
    class = class,
    ...
  )

 # Add accessibility attributes
  btn$attribs$`aria-label` <- aria_label %||% as.character(label)
  if (disabled) {
    btn$attribs$disabled <- "disabled"
    btn$attribs$`aria-disabled` <- "true"
  }

  btn
}

#' Skeleton Loader
#'
#' Creates placeholder content while data is loading.
#'
#' @param type Type of skeleton: "text", "chart", "card", "metric"
#' @param lines Number of text lines (for type="text")
#'
#' @return A Shiny tag object
#'
skeleton_loader <- function(type = c("text", "chart", "card", "metric"),
                             lines = 3) {
  type <- match.arg(type)

  switch(type,
    "text" = tags$div(
      class = "skeleton-container",
      `aria-hidden` = "true",
      lapply(seq_len(lines), function(i) {
        width <- if (i == lines) "short" else if (i %% 2 == 0) "medium" else ""
        tags$div(class = paste("skeleton skeleton-text", width))
      })
    ),
    "chart" = tags$div(
      class = "skeleton skeleton-chart",
      `aria-hidden` = "true"
    ),
    "card" = tags$div(
      class = "skeleton-container",
      `aria-hidden` = "true",
      tags$div(class = "skeleton skeleton-text short"),
      tags$div(class = "skeleton skeleton-text"),
      tags$div(class = "skeleton skeleton-text medium")
    ),
    "metric" = tags$div(
      class = "skeleton-container",
      `aria-hidden` = "true",
      tags$div(class = "skeleton skeleton-text short", style = "height: 2em; margin-bottom: 0.5em;"),
      tags$div(class = "skeleton skeleton-text medium", style = "height: 1em;")
    )
  )
}

#' Screen Reader Only Text
#'
#' Creates text that is only visible to screen readers.
#'
#' @param text The text content
#'
#' @return A Shiny tag object
#'
sr_only <- function(text) {
  tags$span(class = "sr-only", text)
}

#' Validation Message
#'
#' Displays a validation message below form fields.
#'
#' @param id Element ID
#' @param message The validation message
#' @param type Message type: "error", "warning", "success"
#'
#' @return A Shiny tag object
#'
validation_message <- function(id, message = "", type = c("error", "warning", "success")) {
  type <- match.arg(type)

  icon_name <- switch(type,
    error = "times-circle",
    warning = "exclamation-triangle",
    success = "check-circle"
  )

  shinyjs::hidden(
    tags$div(
      id = id,
      class = paste("validation-message", type),
      role = "alert",
      icon(icon_name),
      tags$span(message)
    )
  )
}
