#' canhrActi ggplot2 Theme
#'
#' A clean, professional theme for canhrActi visualizations.
#' Publication-ready with excellent readability and consistent with
#' the dashboard's 6-size typography system.
#'
#' Typography sizes (matching CSS):
#' - Caption/footnotes: 11px (0.79x base)
#' - Labels/metadata: 12px (0.86x base)
#' - Body text: 14px (base_size)
#' - Emphasis/titles: 16px (1.14x base)
#' - Section headings: 20px (1.43x base)
#' - Page titles: 24px (1.71x base)
#'
#' @param base_size Numeric. Base font size in points (default: 14)
#' @param base_family Character. Base font family (default: "")
#' @param grid Logical. Show major grid lines? (default: TRUE)
#' @param dark Logical. Use dark mode? (default: FALSE)
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_canhrActi()
#' }
#'
#' @export
theme_canhrActi <- function(base_size = 14, base_family = "", grid = TRUE, dark = FALSE) {

  # Color palette matching CSS design system
  colors <- if (dark) {
    list(
      text_primary = "#F8FAFC",    # --color-gray-50
      text_secondary = "#CBD5E1",  # --color-gray-300
      text_muted = "#94A3B8",      # --color-gray-400
      background = "#1E293B",      # --color-gray-800
      panel_bg = "#0F172A",        # --color-gray-900
      grid = "#334155",            # --color-gray-700
      border = "#475569",          # --color-gray-600
      accent = "#5C9ACC"           # --color-primary-lighter
    )
  } else {
    list(
      text_primary = "#111111",    # Near black - high readability
      text_secondary = "#1F2937",  # Dark gray
      text_muted = "#374151",      # Medium-dark for secondary text
      background = "#FFFFFF",      # White
      panel_bg = "#FFFFFF",        # White
      grid = "#E2E8F0",            # --border-default
      border = "#CBD5E1",          # --color-gray-300
      accent = "#236192"           # --color-primary
    )
  }

  type_scale <- list(
    caption = 0.79,
    label = 0.86,
    body = 1.0,
    emphasis = 1.14,
    heading = 1.43,
    title = 1.71
  )

  # Start with theme_minimal as base

  theme <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)

  theme <- theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size * type_scale$heading,
        face = "bold",
        color = colors$text_primary,
        hjust = 0,
        margin = ggplot2::margin(b = 8),
        lineheight = 1.2
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * type_scale$body,
        color = colors$text_secondary,
        hjust = 0,
        margin = ggplot2::margin(b = 12),
        lineheight = 1.4
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * type_scale$caption,
        color = colors$text_muted,
        hjust = 1,
        margin = ggplot2::margin(t = 8),
        lineheight = 1.3
      ),

      axis.title = ggplot2::element_text(
        size = base_size * type_scale$body,
        face = "bold",
        color = colors$text_primary,
        margin = ggplot2::margin(t = 8, r = 8)
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 10)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10),
        angle = 90
      ),
      axis.text = ggplot2::element_text(
        size = base_size * type_scale$label,
        color = colors$text_secondary
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 4)
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 4)
      ),
      axis.ticks = ggplot2::element_line(
        color = colors$border,
        linewidth = 0.5
      ),
      axis.ticks.length = ggplot2::unit(4, "pt"),

      legend.title = ggplot2::element_text(
        size = base_size * type_scale$body,
        face = "bold",
        color = colors$text_primary
      ),
      legend.text = ggplot2::element_text(
        size = base_size * type_scale$label,
        color = colors$text_secondary
      ),
      legend.position = "right",
      legend.justification = "top",
      legend.background = ggplot2::element_rect(
        fill = colors$background,
        color = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = colors$background,
        color = NA
      ),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 8),

      panel.background = ggplot2::element_rect(
        fill = colors$panel_bg,
        color = NA
      ),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1.5, "lines"),

      plot.background = ggplot2::element_rect(
        fill = colors$background,
        color = NA
      ),

      plot.margin = ggplot2::margin(12, 12, 12, 12),

      strip.text = ggplot2::element_text(
        size = base_size * type_scale$body,
        face = "bold",
        color = colors$text_primary,
        margin = ggplot2::margin(b = 8, t = 8)
      ),
      strip.background = ggplot2::element_rect(
        fill = colors$grid,
        color = NA
      )
    )

  # Grid lines
  if (grid) {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          color = colors$grid,
          linewidth = 0.4
        ),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  return(theme)
}


#' Set canhrActi Theme as Default
#'
#' Sets theme_canhrActi() as the default ggplot2 theme for the session.
#'
#' @param ... Arguments passed to theme_canhrActi()
#'
#' @export
set_canhrActi_theme <- function(...) {
  ggplot2::theme_set(theme_canhrActi(...))
  invisible(NULL)
}


#' canhrActi Color Generator
#'
#' Generates colors from canhrActi palettes for visualizations.
#' These colors match the CSS design system.
#' For named palettes (intensity, status, sleep), use \code{\link{canhrActi_palette}}.
#'
#' @param n Integer. Number of colors to return. If NULL, returns all.
#' @param type Character. One of "categorical", "sequential", "diverging", or "intensity"
#'
#' @return A character vector of hex color codes
#'
#' @examples
#' \dontrun{
#' colors <- canhrActi_colors(4)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_manual(values = canhrActi_colors(3))
#' }
#'
#' @export
canhrActi_colors <- function(n = NULL, type = "categorical") {
  # Colorblind-safe palettes based on Okabe-Ito and Wong recommendations
  palettes <- list(
    # For categories (up to 8 distinct colors) - Okabe-Ito palette
    categorical = c(
      "#0072B2",  # Blue (primary)
      "#E69F00",  # Orange
      "#009E73",  # Bluish green
      "#CC79A7",  # Reddish purple
      "#56B4E9",  # Sky blue
      "#D55E00",  # Vermillion
      "#F0E442",
      "#000000"   # Black
    ),

    # For sequential data (light to dark) - Viridis-inspired blue
    sequential = c(
      "#F7FBFF",
      "#DEEBF7",
      "#C6DBEF",
      "#9ECAE1",
      "#6BAED6",
      "#4292C6",
      "#2171B5",
      "#084594"
    ),

    # For diverging data (negative to positive) - Purple-Orange colorblind-safe
    diverging = c(
      "#B35806",  # Strong negative (orange)
      "#E08214",  # Moderate negative
      "#FDB863",  # Weak negative
      "#F7F7F7",  # Neutral
      "#B2ABD2",  # Weak positive
      "#8073AC",  # Moderate positive
      "#542788"   # Strong positive (purple)
    ),

    # For activity intensity levels - COLORBLIND-SAFE
    # Designed for clear distinction in all forms of color blindness
    intensity = c(
      "#64748B",  # Sedentary (neutral gray) - works for all
      "#56B4E9",  # Light (sky blue) - Okabe-Ito
      "#009E73",  # Moderate (bluish green) - Okabe-Ito
      "#E69F00",  # Vigorous (orange) - Okabe-Ito
      "#D55E00"   # Very vigorous (vermillion) - Okabe-Ito
    ),

    # Named intensity palette for explicit use
    intensity_named = c(
      sedentary = "#64748B",
      light = "#56B4E9",
      moderate = "#009E73",
      vigorous = "#E69F00",
      very_vigorous = "#D55E00"
    ),

    # Sleep states - high contrast for sleep/wake distinction
    sleep = c(
      wake = "#56B4E9",       # Sky blue
      sleep = "#0072B2",      # Dark blue
      rem = "#CC79A7",        # Reddish purple
      nrem = "#009E73"        # Bluish green
    ),

    # Wear status - clearly distinguishable
    wear = c(
      wear = "#009E73",       # Bluish green (valid)
      nonwear = "#E69F00"     # Orange (attention)
    ),

    # Posture/inclinometer - distinct from intensity
    posture = c(
      standing = "#0072B2",   # Blue
      sitting = "#E69F00",    # Orange
      lying = "#CC79A7",      # Reddish purple
      off = "#64748B"         # Gray
    )
  )

  colors <- palettes[[type]]

  if (is.null(n)) {
    return(colors)
  }

  if (n <= length(colors)) {
    return(colors[1:n])
  } else {
    # Interpolate if more colors needed
    return(grDevices::colorRampPalette(colors)(n))
  }
}


#' ggplot2 Scale for canhrActi Colors
#'
#' Discrete color scale using the canhrActi palette.
#'
#' @param type Character. Palette type (see canhrActi_colors)
#' @param ... Additional arguments passed to ggplot2::scale_color_manual
#'
#' @return A ggplot2 scale object
#'
#' @export
scale_color_canhrActi <- function(type = "categorical", ...) {
  ggplot2::scale_color_manual(values = canhrActi_colors(type = type), ...)
}


#' ggplot2 Fill Scale for canhrActi Colors
#'
#' Discrete fill scale using the canhrActi palette.
#'
#' @param type Character. Palette type (see canhrActi_colors)
#' @param ... Additional arguments passed to ggplot2::scale_fill_manual
#'
#' @return A ggplot2 scale object
#'
#' @export
scale_fill_canhrActi <- function(type = "categorical", ...) {
  ggplot2::scale_fill_manual(values = canhrActi_colors(type = type), ...)
}
