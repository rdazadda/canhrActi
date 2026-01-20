# 
# CANHR CUSTOM COMPONENT LIBRARY
# University of Alaska Fairbanks - Center for Alaska Native Health Research
#
# Custom R/Shiny components to replace shinydashboard defaults with modern,
# branded components using CSS Grid, Flexbox, and CSS Variables.
#
# Components:
#   1. canhr_card() - Custom card with hero/info modes, collapsible
#   2. canhr_panel() - Collapsible advanced options panel
#   3. canhr_workflow_step() - Workflow indicator with connected visual flow
#   4. canhr_header() - Custom branded header
#
# Usage: Source this file in app.R after shared_components.R
# 

# 
# CSS INJECTION - Injects component styles once per session
# 

#' Get Component Stylesheet
#'
#' Returns the CSS for all custom components. This is injected once per session.
#'
#' @return HTML style tag with component CSS
#'
canhr_component_styles <- function() {
  tags$style(HTML('
    /* CANHR COMPONENT LIBRARY - CSS */

    /* CSS VARIABLES */
    :root {
      /* Primary brand palette */
      --canhr-navy: #0d2137;
      --canhr-navy-light: #1a3a5c;
      --canhr-teal: #17a589;
      --canhr-teal-light: #1abc9c;
      --canhr-teal-muted: rgba(23, 165, 137, 0.12);
      --canhr-gold: #FFCD00;
      --canhr-gold-dark: #b8860b;
      --canhr-gold-hover: #e6b800;
      --canhr-gold-muted: rgba(255, 205, 0, 0.15);

      /* Neutral gray palette */
      --canhr-gray-50: #f8fafc;
      --canhr-gray-100: #f1f5f9;
      --canhr-gray-200: #e2e8f0;
      --canhr-gray-300: #cbd5e1;
      --canhr-gray-400: #94a3b8;
      --canhr-gray-500: #64748b;
      --canhr-gray-600: #475569;
      --canhr-gray-700: #334155;
      --canhr-gray-800: #1e293b;
      --canhr-gray-900: #0f172a;

      /* Gradient backgrounds */
      --canhr-header-gradient: linear-gradient(180deg, #f8fafc 0%, #f1f5f9 100%);
      --canhr-disabled-gradient: linear-gradient(180deg, #f1f5f9 0%, #e2e8f0 100%);
      --canhr-focus-gradient: linear-gradient(180deg, #e2e8f0 0%, #cbd5e1 100%);
      --canhr-hero-gradient: linear-gradient(135deg, #f8fafc 0%, #ffffff 100%);

      /* Card backgrounds */
      --card-bg: #ffffff;
      --card-bg-hero: var(--canhr-hero-gradient);
      --card-border: var(--canhr-gray-200);
      --card-shadow: 0 1px 3px rgba(0,0,0,0.06), 0 1px 2px rgba(0,0,0,0.04);
      --card-shadow-hover: 0 4px 12px rgba(0,0,0,0.08), 0 2px 4px rgba(0,0,0,0.04);

      /* Animation */
      --transition-smooth: 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      --transition-bounce: 0.4s cubic-bezier(0.68, -0.55, 0.265, 1.55);
    }

    /* 1. CANHR_CARD */

    /* Base card structure */
    .canhr-custom-card {
      background: var(--card-bg);
      border: 1px solid var(--card-border);
      border-radius: 12px;
      box-shadow: var(--card-shadow);
      overflow: hidden;
      transition: box-shadow var(--transition-smooth), transform var(--transition-smooth);
      margin-bottom: 20px;
    }

    .canhr-custom-card:hover {
      box-shadow: var(--card-shadow-hover);
    }

    /* --- HERO MODE --- */
    /* Minimal chrome, chart-focused layout */
    .canhr-custom-card.canhr-card-hero {
      background: var(--card-bg-hero);
      border: none;
      box-shadow: var(--card-shadow);
    }

    .canhr-custom-card.canhr-card-hero .canhr-card-header {
      background: transparent;
      border-bottom: none;
      padding: 16px 20px 8px;
    }

    .canhr-custom-card.canhr-card-hero .canhr-card-title {
      font-size: 13px;
      font-weight: 600;
      color: var(--canhr-text-muted, #64748b);
      text-transform: uppercase;
      letter-spacing: 0.8px;
    }

    .canhr-custom-card.canhr-card-hero .canhr-card-body {
      padding: 8px 20px 20px;
    }

    .canhr-custom-card.canhr-card-hero .canhr-card-body .shiny-plot-output,
    .canhr-custom-card.canhr-card-hero .canhr-card-body img {
      border-radius: 8px;
      overflow: hidden;
    }

    /* --- INFO MODE --- */
    /* Icon, value, label layout for metrics */
    .canhr-custom-card.canhr-card-info {
      display: grid;
      grid-template-columns: auto 1fr;
      gap: 16px;
      padding: 20px 24px;
      align-items: center;
      border-left: 4px solid var(--canhr-teal);
    }

    .canhr-custom-card.canhr-card-info .canhr-card-icon {
      width: 56px;
      height: 56px;
      display: flex;
      align-items: center;
      justify-content: center;
      background: var(--canhr-teal-muted);
      border-radius: 12px;
      color: var(--canhr-teal);
      font-size: 24px;
      transition: transform var(--transition-bounce);
    }

    .canhr-custom-card.canhr-card-info:hover .canhr-card-icon {
      transform: scale(1.08);
    }

    .canhr-custom-card.canhr-card-info .canhr-card-content {
      display: flex;
      flex-direction: column;
    }

    .canhr-custom-card.canhr-card-info .canhr-card-value {
      font-size: 28px;
      font-weight: 700;
      color: var(--canhr-navy);
      line-height: 1.1;
      letter-spacing: -0.5px;
    }

    .canhr-custom-card.canhr-card-info .canhr-card-label {
      font-size: 12px;
      font-weight: 600;
      color: var(--canhr-text-muted, #64748b);
      text-transform: uppercase;
      letter-spacing: 0.5px;
      margin-top: 4px;
    }

    .canhr-custom-card.canhr-card-info .canhr-card-sublabel {
      font-size: 11px;
      color: var(--canhr-text-light, #94a3b8);
      margin-top: 2px;
    }

    /* Info card color variants */
    .canhr-custom-card.canhr-card-info.canhr-color-teal {
      border-left-color: var(--canhr-teal);
    }
    .canhr-custom-card.canhr-card-info.canhr-color-teal .canhr-card-icon {
      background: var(--canhr-teal-muted);
      color: var(--canhr-teal);
    }

    .canhr-custom-card.canhr-card-info.canhr-color-gold {
      border-left-color: var(--canhr-gold);
    }
    .canhr-custom-card.canhr-card-info.canhr-color-gold .canhr-card-icon {
      background: var(--canhr-gold-muted);
      color: var(--canhr-gold-dark);
    }

    .canhr-custom-card.canhr-card-info.canhr-color-navy {
      border-left-color: var(--canhr-navy);
    }
    .canhr-custom-card.canhr-card-info.canhr-color-navy .canhr-card-icon {
      background: rgba(13, 33, 55, 0.08);
      color: var(--canhr-navy);
    }

    /* --- Standard card header/body --- */
    .canhr-custom-card .canhr-card-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 14px 20px;
      background: var(--canhr-header-gradient);
      border-bottom: 1px solid var(--card-border);
    }

    .canhr-custom-card .canhr-card-title-group {
      display: flex;
      align-items: center;
      gap: 10px;
    }

    .canhr-custom-card .canhr-card-title-icon {
      color: var(--canhr-blue, #236192);
      font-size: 16px;
    }

    .canhr-custom-card .canhr-card-title {
      font-size: 14px;
      font-weight: 600;
      color: var(--canhr-text, #1a202c);
      margin: 0;
    }

    .canhr-custom-card .canhr-card-actions {
      display: flex;
      align-items: center;
      gap: 8px;
    }

    .canhr-custom-card .canhr-card-body {
      padding: 20px;
    }

    /* --- Collapsible functionality --- */
    .canhr-custom-card .canhr-collapse-btn {
      width: 28px;
      height: 28px;
      display: flex;
      align-items: center;
      justify-content: center;
      background: transparent;
      border: none;
      border-radius: 6px;
      color: var(--canhr-text-muted, #64748b);
      cursor: pointer;
      transition: all var(--transition-smooth);
    }

    .canhr-custom-card .canhr-collapse-btn:hover {
      background: rgba(35, 97, 146, 0.08);
      color: var(--canhr-blue, #236192);
    }

    .canhr-custom-card .canhr-collapse-btn .fa,
    .canhr-custom-card .canhr-collapse-btn .fas {
      transition: transform var(--transition-smooth);
    }

    .canhr-custom-card.canhr-card-collapsed .canhr-card-body {
      display: none;
    }

    .canhr-custom-card.canhr-card-collapsed .canhr-collapse-btn .fa,
    .canhr-custom-card.canhr-card-collapsed .canhr-collapse-btn .fas {
      transform: rotate(-180deg);
    }

    /* 2. CANHR_PANEL */
    .canhr-adv-panel {
      background: var(--card-bg);
      border: 1px solid var(--card-border);
      border-radius: 10px;
      margin-bottom: 20px;
      overflow: hidden;
    }

    .canhr-adv-panel-header {
      display: flex;
      align-items: center;
      gap: 12px;
      padding: 14px 20px;
      background: var(--canhr-disabled-gradient);
      cursor: pointer;
      user-select: none;
      transition: background var(--transition-smooth);
    }

    .canhr-adv-panel-header:hover {
      background: var(--canhr-focus-gradient);
    }

    .canhr-adv-panel-icon {
      width: 32px;
      height: 32px;
      display: flex;
      align-items: center;
      justify-content: center;
      background: rgba(35, 97, 146, 0.1);
      border-radius: 8px;
      color: var(--canhr-blue, #236192);
      font-size: 14px;
    }

    .canhr-adv-panel-title {
      flex: 1;
      font-size: 13px;
      font-weight: 600;
      color: var(--canhr-text, #1a202c);
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }

    .canhr-adv-panel-chevron {
      color: var(--canhr-text-muted, #64748b);
      font-size: 12px;
      transition: transform var(--transition-smooth);
    }

    .canhr-adv-panel.canhr-panel-collapsed .canhr-adv-panel-chevron {
      transform: rotate(-90deg);
    }

    /* Panel body with smooth animation */
    .canhr-adv-panel-body {
      max-height: 2000px;
      padding: 20px;
      border-top: 1px solid var(--card-border);
      background: var(--canhr-gray-50);
      overflow: hidden;
      transition: max-height 0.4s ease-in-out, padding 0.3s ease-in-out, opacity 0.3s ease;
      opacity: 1;
    }

    .canhr-adv-panel.canhr-panel-collapsed .canhr-adv-panel-body {
      max-height: 0;
      padding-top: 0;
      padding-bottom: 0;
      opacity: 0;
      border-top: none;
    }

    /* Options grid inside panel */
    .canhr-adv-panel-body .canhr-options-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 16px;
    }

    .canhr-adv-panel-body .canhr-option-group {
      padding: 16px;
      background: white;
      border-radius: 8px;
      border: 1px solid var(--card-border);
    }

    .canhr-adv-panel-body .canhr-option-group-title {
      font-size: 11px;
      font-weight: 700;
      color: var(--canhr-blue, #236192);
      text-transform: uppercase;
      letter-spacing: 0.8px;
      margin-bottom: 12px;
      padding-bottom: 8px;
      border-bottom: 1px solid #e2e8f0;
    }

    /* 3. CANHR_WORKFLOW_STEP */
    .canhr-workflow-container {
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 24px 20px;
      background: var(--card-bg);
      border: 1px solid var(--card-border);
      border-radius: 12px;
      margin-bottom: 20px;
      box-shadow: var(--card-shadow);
    }

    .canhr-workflow-steps-row {
      display: flex;
      align-items: center;
      gap: 0;
    }

    /* Individual step */
    .canhr-workflow-step-item {
      display: flex;
      align-items: center;
      position: relative;
    }

    .canhr-workflow-step-content {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 8px;
      z-index: 1;
    }

    /* Step number circle */
    .canhr-workflow-number {
      width: 40px;
      height: 40px;
      display: flex;
      align-items: center;
      justify-content: center;
      border-radius: 50%;
      font-size: 16px;
      font-weight: 700;
      background: var(--canhr-gray-200);
      color: var(--canhr-gray-400);
      border: 3px solid var(--canhr-gray-200);
      transition: all var(--transition-smooth);
      position: relative;
    }

    .canhr-workflow-step-item.canhr-step-pending .canhr-workflow-number {
      background: var(--canhr-gray-100);
      border-color: var(--canhr-gray-300);
      color: var(--canhr-gray-400);
    }

    .canhr-workflow-step-item.canhr-step-active .canhr-workflow-number {
      background: var(--canhr-blue, #236192);
      border-color: var(--canhr-blue, #236192);
      color: white;
      box-shadow: 0 0 0 4px rgba(35, 97, 146, 0.2);
      animation: canhr-pulse 2s infinite;
    }

    .canhr-workflow-step-item.canhr-step-complete .canhr-workflow-number {
      background: var(--canhr-teal);
      border-color: var(--canhr-teal);
      color: white;
    }

    @keyframes canhr-pulse {
      0%, 100% { box-shadow: 0 0 0 4px rgba(35, 97, 146, 0.2); }
      50% { box-shadow: 0 0 0 8px rgba(35, 97, 146, 0.1); }
    }

    /* Step labels */
    .canhr-workflow-title {
      font-size: 12px;
      font-weight: 600;
      color: #64748b;
      text-align: center;
      max-width: 100px;
      transition: color var(--transition-smooth);
    }

    .canhr-workflow-step-item.canhr-step-active .canhr-workflow-title {
      color: var(--canhr-blue, #236192);
    }

    .canhr-workflow-step-item.canhr-step-complete .canhr-workflow-title {
      color: var(--canhr-teal);
    }

    /* Connecting line */
    .canhr-workflow-connector-line {
      width: 60px;
      height: 3px;
      background: var(--canhr-gray-200);
      margin: 0 8px;
      margin-top: -20px;
      border-radius: 2px;
      position: relative;
      overflow: hidden;
    }

    .canhr-workflow-connector-line::after {
      content: "";
      position: absolute;
      left: 0;
      top: 0;
      height: 100%;
      width: 0%;
      background: var(--canhr-teal);
      border-radius: 2px;
      transition: width 0.5s ease-in-out;
    }

    .canhr-workflow-step-item.canhr-step-complete + .canhr-workflow-step-item .canhr-workflow-connector-line::after,
    .canhr-workflow-connector-line.canhr-connector-complete::after {
      width: 100%;
    }

    /* 4. CANHR_HEADER */
    .canhr-app-header {
      display: grid;
      grid-template-columns: auto 1fr auto;
      align-items: center;
      gap: 24px;
      padding: 12px 24px;
      background: linear-gradient(135deg, var(--canhr-navy) 0%, var(--canhr-navy-light) 100%);
      border-bottom: 3px solid var(--canhr-gold);
      min-height: 64px;
    }

    /* Logo area */
    .canhr-header-logo {
      display: flex;
      align-items: center;
      gap: 12px;
    }

    .canhr-header-logo img {
      height: 40px;
      width: auto;
    }

    .canhr-header-logo-text {
      display: flex;
      flex-direction: column;
    }

    .canhr-header-brand {
      font-size: 20px;
      font-weight: 700;
      color: white;
      letter-spacing: 0.5px;
    }

    .canhr-header-brand .canhr-brand-accent {
      color: var(--canhr-gold);
    }

    .canhr-header-tagline {
      font-size: 10px;
      color: rgba(255, 255, 255, 0.7);
      text-transform: uppercase;
      letter-spacing: 1px;
    }

    /* App title area */
    .canhr-header-title {
      text-align: center;
    }

    .canhr-header-app-name {
      font-size: 18px;
      font-weight: 600;
      color: white;
      margin: 0;
    }

    .canhr-header-app-subtitle {
      font-size: 11px;
      color: rgba(255, 255, 255, 0.6);
      margin: 2px 0 0 0;
    }

    /* User actions area */
    .canhr-header-actions {
      display: flex;
      align-items: center;
      gap: 12px;
    }

    .canhr-header-action-btn {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 8px 16px;
      background: rgba(255, 255, 255, 0.1);
      border: 1px solid rgba(255, 255, 255, 0.2);
      border-radius: 8px;
      color: white;
      font-size: 13px;
      font-weight: 500;
      text-decoration: none;
      transition: all var(--transition-smooth);
      cursor: pointer;
    }

    .canhr-header-action-btn:hover {
      background: rgba(255, 255, 255, 0.2);
      border-color: rgba(255, 255, 255, 0.3);
      color: white;
      text-decoration: none;
    }

    .canhr-header-action-btn.canhr-btn-primary {
      background: var(--canhr-gold);
      border-color: var(--canhr-gold);
      color: var(--canhr-navy);
    }

    .canhr-header-action-btn.canhr-btn-primary:hover {
      background: var(--canhr-gold-hover);
      border-color: var(--canhr-gold-hover);
    }

    /* User avatar/info */
    .canhr-header-user {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 6px 12px;
      background: rgba(255, 255, 255, 0.08);
      border-radius: 8px;
    }

    .canhr-header-user-avatar {
      width: 32px;
      height: 32px;
      border-radius: 50%;
      background: var(--canhr-teal);
      display: flex;
      align-items: center;
      justify-content: center;
      color: white;
      font-size: 14px;
      font-weight: 600;
    }

    .canhr-header-user-name {
      font-size: 13px;
      color: white;
      font-weight: 500;
    }

    /* RESPONSIVE ADJUSTMENTS */
    @media (max-width: 992px) {
      .canhr-app-header {
        grid-template-columns: auto 1fr;
        gap: 16px;
      }

      .canhr-header-title {
        text-align: left;
      }

      .canhr-header-actions {
        display: none;
      }

      .canhr-workflow-connector-line {
        width: 40px;
      }
    }

    @media (max-width: 768px) {
      .canhr-custom-card.canhr-card-info {
        grid-template-columns: 1fr;
        text-align: center;
      }

      .canhr-custom-card.canhr-card-info .canhr-card-icon {
        margin: 0 auto;
      }

      .canhr-workflow-steps-row {
        flex-wrap: wrap;
        gap: 16px;
        justify-content: center;
      }

      .canhr-workflow-connector-line {
        display: none;
      }

      .canhr-adv-panel-body .canhr-options-grid {
        grid-template-columns: 1fr;
      }
    }
  '))
}

# 
# 1. CANHR_CARD - Custom card component
# 

#' Custom Card Component
#'
#' A flexible card component with two modes:
#' - Hero mode: Minimal chrome, chart-focused (for visualizations)
#' - Info mode: Icon + value + label layout (for metrics/KPIs)
#'
#' @param mode Card mode: "hero" (charts) or "info" (metrics) or "standard"
#' @param title Card title (used in hero and standard modes)
#' @param icon_name FontAwesome icon name (used in info mode)
#' @param value Metric value (used in info mode)
#' @param label Metric label (used in info mode)
#' @param sublabel Optional sublabel (used in info mode)
#' @param ... Card body content (used in hero and standard modes)
#' @param color Custom color theme: "teal", "gold", "navy" (info mode only)
#' @param collapsible Whether card can be collapsed (default: FALSE)
#' @param collapsed Whether card starts collapsed (default: FALSE)
#' @param id Optional HTML id attribute
#' @param width Bootstrap column width (NULL for no wrapper)
#' @param header_extra Extra content for the header (actions, badges, etc.)
#'
#' @return Shiny tag object
#'
#' @examples
#' # Hero mode for charts
#' canhr_card(mode = "hero", title = "Activity Timeline",
#'            plotOutput("my_plot"))
#'
#' # Info mode for metrics
#' canhr_card(mode = "info", icon_name = "clock", value = "14.2h",
#'            label = "Avg Wear Time", sublabel = "Per valid day",
#'            color = "teal")
#'
#' # Standard collapsible card
#' canhr_card(mode = "standard", title = "Settings", collapsible = TRUE,
#'            selectInput("algo", "Algorithm", c("A", "B")))
#'
canhr_card <- function(mode = c("standard", "hero", "info"),
                       title = NULL,
                       icon_name = NULL,
                       value = NULL,
                       label = NULL,
                       sublabel = NULL,
                       ...,
                       color = c("teal", "gold", "navy"),
                       collapsible = FALSE,
                       collapsed = FALSE,
                       id = NULL,
                       width = NULL,
                       header_extra = NULL) {

  mode <- match.arg(mode)
  color <- match.arg(color)

  # Build card based on mode
  card <- switch(mode,

    # HERO MODE - Minimal chrome, chart-focused
    "hero" = {
      header_html <- NULL
      if (!is.null(title)) {
        header_html <- tags$div(
          class = "canhr-card-header",
          tags$h4(class = "canhr-card-title", title),
          if (!is.null(header_extra)) tags$div(class = "canhr-card-actions", header_extra)
        )
      }

      tags$div(
        id = id,
        class = "canhr-custom-card canhr-card-hero",
        header_html,
        tags$div(class = "canhr-card-body", ...)
      )
    },

    # INFO MODE - Icon, value, label layout
    "info" = {
      tags$div(
        id = id,
        class = paste("canhr-custom-card canhr-card-info", paste0("canhr-color-", color)),

        # Icon
        if (!is.null(icon_name)) {
          tags$div(class = "canhr-card-icon", icon(icon_name))
        },

        # Content
        tags$div(
          class = "canhr-card-content",
          if (!is.null(value)) tags$div(class = "canhr-card-value", value),
          if (!is.null(label)) tags$div(class = "canhr-card-label", label),
          if (!is.null(sublabel)) tags$div(class = "canhr-card-sublabel", sublabel)
        )
      )
    },

    # STANDARD MODE - Full-featured card
    "standard" = {
      collapse_class <- if (collapsible && collapsed) "canhr-card-collapsed" else ""

      # Header with optional collapse button
      header_html <- NULL
      if (!is.null(title) || collapsible) {
        collapse_btn <- NULL
        if (collapsible) {
          collapse_btn <- tags$button(
            type = "button",
            class = "canhr-collapse-btn",
            onclick = "$(this).closest('.canhr-custom-card').toggleClass('canhr-card-collapsed');",
            icon(if (collapsed) "chevron-down" else "chevron-up")
          )
        }

        header_html <- tags$div(
          class = "canhr-card-header",
          tags$div(
            class = "canhr-card-title-group",
            if (!is.null(icon_name)) tags$span(class = "canhr-card-title-icon", icon(icon_name)),
            if (!is.null(title)) tags$h4(class = "canhr-card-title", title)
          ),
          tags$div(
            class = "canhr-card-actions",
            if (!is.null(header_extra)) header_extra,
            collapse_btn
          )
        )
      }

      tags$div(
        id = id,
        class = paste("canhr-custom-card", collapse_class),
        header_html,
        tags$div(class = "canhr-card-body", ...)
      )
    }
  )

  # Wrap in column if width specified
  if (!is.null(width)) {
    column(width = width, card)
  } else {
    card
  }
}

# 
# 2. CANHR_PANEL - Collapsible advanced options panel
# 

#' Collapsible Advanced Options Panel
#'
#' A collapsible panel for advanced options. Starts collapsed by default
#' with smooth animation on expand. Styled with "Advanced Options" appearance.
#'
#' @param title Panel title (default: "Advanced Options")
#' @param ... Panel content (typically form controls)
#' @param collapsed Whether panel starts collapsed (default: TRUE)
#' @param icon_name Icon for the panel header (default: "sliders-h")
#' @param id Optional HTML id attribute
#'
#' @return Shiny tag object
#'
#' @examples
#' canhr_panel(
#'   title = "Advanced Options",
#'   numericInput("threshold", "Threshold", 100),
#'   selectInput("method", "Method", c("A", "B", "C"))
#' )
#'
canhr_panel <- function(title = "Advanced Options",
                        ...,
                        collapsed = TRUE,
                        icon_name = "sliders-h",
                        id = NULL) {

  panel_id <- id %||% paste0("canhr_panel_", sample(1e6, 1))
  collapse_class <- if (collapsed) "canhr-panel-collapsed" else ""

  tags$div(
    id = panel_id,
    class = paste("canhr-adv-panel", collapse_class),

    # Header (clickable to toggle)
    tags$div(
      class = "canhr-adv-panel-header",
      onclick = sprintf("$('#%s').toggleClass('canhr-panel-collapsed');", panel_id),

      tags$div(class = "canhr-adv-panel-icon", icon(icon_name)),
      tags$span(class = "canhr-adv-panel-title", title),
      tags$span(class = "canhr-adv-panel-chevron", icon("chevron-down"))
    ),

    # Body (animated collapse/expand)
    tags$div(
      class = "canhr-adv-panel-body",
      ...
    )
  )
}

#' Options Grid for Advanced Panel
#'
#' Creates a responsive grid layout for options inside canhr_panel()
#'
#' @param ... Option groups or individual controls
#'
#' @return Shiny tag object
#'
canhr_options_grid <- function(...) {
  tags$div(class = "canhr-options-grid", ...)
}

#' Option Group for Advanced Panel
#'
#' Groups related options with a title
#'
#' @param title Group title
#' @param ... Controls within the group
#'
#' @return Shiny tag object
#'
canhr_option_group <- function(title, ...) {
  tags$div(
    class = "canhr-option-group",
    tags$div(class = "canhr-option-group-title", title),
    ...
  )
}

# 
# 3. CANHR_WORKFLOW_STEP - Workflow indicator component
# 

#' Workflow Step Indicator
#'
#' Creates a visual workflow indicator showing multiple steps with their
#' status (pending, active, complete) and connected visual flow.
#'
#' @param steps List of step definitions, each with: number, title, status
#'              status can be: "pending", "active", "complete"
#' @param id Optional HTML id attribute
#'
#' @return Shiny tag object
#'
#' @examples
#' canhr_workflow_step(steps = list(
#'   list(number = 1, title = "Upload Data", status = "complete"),
#'   list(number = 2, title = "Configure", status = "active"),
#'   list(number = 3, title = "Analyze", status = "pending"),
#'   list(number = 4, title = "Export", status = "pending")
#' ))
#'
canhr_workflow_step <- function(steps, id = NULL) {

  n_steps <- length(steps)

  step_elements <- lapply(seq_along(steps), function(i) {
    step <- steps[[i]]
    status_class <- paste0("canhr-step-", step$status)

    # Check if previous step is complete (for connector styling)
    prev_complete <- i > 1 && steps[[i-1]]$status == "complete"

    # Step content
    step_content <- tags$div(
      class = paste("canhr-workflow-step-item", status_class),

      # Connector line (before all except first)
      if (i > 1) {
        tags$div(
          class = paste("canhr-workflow-connector-line",
                       if (prev_complete) "canhr-connector-complete" else "")
        )
      },

      # Step circle and label
      tags$div(
        class = "canhr-workflow-step-content",
        tags$div(
          class = "canhr-workflow-number",
          if (step$status == "complete") icon("check") else step$number
        ),
        tags$div(class = "canhr-workflow-title", step$title)
      )
    )

    step_content
  })

  tags$div(
    id = id,
    class = "canhr-workflow-container",
    tags$div(
      class = "canhr-workflow-steps-row",
      step_elements
    )
  )
}

#' Render Workflow Step (Server-side)
#'
#' Helper to create workflow step output for dynamic updates
#'
#' @param outputId The output slot id
#'
#' @return Shiny UI output element
#'
canhr_workflow_output <- function(outputId) {
  uiOutput(outputId, class = "canhr-workflow-output-wrapper")
}

# 
# 4. CANHR_HEADER - Custom branded header
# 

#' Custom Branded Header
#'
#' A custom branded header component with logo placement, app title,
#' and user actions area. Replaces the default shinydashboard header.
#'
#' @param logo_src Path to logo image (relative to www/ folder)
#' @param brand_text Brand text (can include HTML with spans for accent)
#' @param tagline Optional tagline below brand text
#' @param app_title Main application title
#' @param app_subtitle Optional app subtitle
#' @param actions List of action buttons/links for the right side
#' @param user_name Optional user name to display
#' @param user_initial Optional user initial for avatar (default: first letter of name)
#'
#' @return Shiny tag object
#'
#' @examples
#' canhr_header(
#'   logo_src = "uaf_logo.png",
#'   brand_text = HTML("<span class='canhr-brand-accent'>CANHR</span>Acti"),
#'   tagline = "Center for Alaska Native Health Research",
#'   app_title = "Accelerometer Analysis",
#'   actions = list(
#'     list(label = "Help", icon = "question-circle", href = "https://help.example.com"),
#'     list(label = "Export", icon = "download", onclick = "Shiny.setInputValue('export', true)")
#'   ),
#'   user_name = "Dr. Smith"
#' )
#'
canhr_header <- function(logo_src = NULL,
                         brand_text = "CANHRActi",
                         tagline = NULL,
                         app_title = NULL,
                         app_subtitle = NULL,
                         actions = NULL,
                         user_name = NULL,
                         user_initial = NULL) {

  # Logo area
  logo_html <- tags$div(
    class = "canhr-header-logo",
    if (!is.null(logo_src)) tags$img(src = logo_src, alt = "Logo"),
    tags$div(
      class = "canhr-header-logo-text",
      tags$span(class = "canhr-header-brand", brand_text),
      if (!is.null(tagline)) tags$span(class = "canhr-header-tagline", tagline)
    )
  )

  # Title area (center)
  title_html <- NULL
  if (!is.null(app_title)) {
    title_html <- tags$div(
      class = "canhr-header-title",
      tags$h1(class = "canhr-header-app-name", app_title),
      if (!is.null(app_subtitle)) tags$p(class = "canhr-header-app-subtitle", app_subtitle)
    )
  }

  # Actions area (right)
  actions_html <- NULL
  if (!is.null(actions) || !is.null(user_name)) {
    action_buttons <- lapply(actions, function(action) {
      if (!is.null(action$href)) {
        tags$a(
          href = action$href,
          target = "_blank",
          class = paste("canhr-header-action-btn", action$class %||% ""),
          icon(action$icon),
          action$label
        )
      } else {
        tags$button(
          type = "button",
          class = paste("canhr-header-action-btn", action$class %||% ""),
          onclick = action$onclick %||% "",
          icon(action$icon),
          action$label
        )
      }
    })

    user_html <- NULL
    if (!is.null(user_name)) {
      initial <- user_initial %||% substr(user_name, 1, 1)
      user_html <- tags$div(
        class = "canhr-header-user",
        tags$div(class = "canhr-header-user-avatar", initial),
        tags$span(class = "canhr-header-user-name", user_name)
      )
    }

    actions_html <- tags$div(
      class = "canhr-header-actions",
      action_buttons,
      user_html
    )
  }

  tags$header(
    class = "canhr-app-header",
    logo_html,
    title_html,
    actions_html
  )
}

# 
# INITIALIZATION HELPER
# 

#' Initialize CANHR Components
#'
#' Call this function in your UI to inject the component styles.
#' Should be called once, typically in the head section.
#'
#' @return Shiny tagList with style injection
#'
#' @examples
#' ui <- fluidPage(
#'   canhr_components_init(),
#'   # ... rest of UI
#' )
#'
canhr_components_init <- function() {
  tagList(
    canhr_component_styles()
  )
}
