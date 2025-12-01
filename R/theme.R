#' canhrActi ggplot2 Theme
#'
#' A clean, professional theme for canhrActi visualizations.
#' Publication-ready with excellent readability.
#'
#' @param base_size Numeric. Base font size in points (default: 12)
#' @param base_family Character. Base font family (default: "")
#' @param grid Logical. Show major grid lines? (default: TRUE)
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
theme_canhrActi <- function(base_size = 12, base_family = "", grid = TRUE) {

  # Start with theme_minimal as base
  theme <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)

  # Customize
  theme <- theme +
    ggplot2::theme(
      # Text elements
      plot.title = ggplot2::element_text(
        size = base_size * 1.3,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 1.0,
        color = "#666666",
        hjust = 0,
        margin = ggplot2::margin(b = 15)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.8,
        color = "#999999",
        hjust = 1,
        margin = ggplot2::margin(t = 10)
      ),

      # Axis text
      axis.title = ggplot2::element_text(
        size = base_size * 1.0,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.9,
        color = "#333333"
      ),

      # Legend
      legend.title = ggplot2::element_text(
        size = base_size * 1.0,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        size = base_size * 0.9
      ),
      legend.position = "right",
      legend.justification = "top",

      # Panel
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_blank(),

      # Plot background
      plot.background = ggplot2::element_rect(fill = "white", color = NA),

      # Margins
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  # Grid lines
  if (grid) {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "#E0E0E0", linewidth = 0.3),
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
