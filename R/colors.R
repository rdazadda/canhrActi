#' canhrActi Color Palettes
#'
#' Defines the official canhrActi color palette for consistent visualizations.
#'
#' @name canhrActi_colors
#' @keywords internal
NULL

#' Get canhrActi Color Palette
#'
#' @param palette Character. Which palette to retrieve:
#'   \itemize{
#'     \item \code{"intensity"} - 5 colors for activity intensity levels
#'     \item \code{"status"} - Colors for valid/invalid, wear/nonwear
#'     \item \code{"sleep"} - Colors for sleep states
#'     \item \code{"primary"} - All 6 primary colors
#'   }
#' @param reverse Logical. Reverse the color order? (default: FALSE)
#'
#' @return Named character vector of hex colors
#'
#' @examples
#' \dontrun{
#' # Get intensity colors
#' cols <- canhrActi_palette("intensity")
#' barplot(1:5, col = cols)
#' }
#'
#' @export
canhrActi_palette <- function(palette = c("intensity", "status", "sleep", "primary"),
                              reverse = FALSE) {
  palette <- match.arg(palette)

  palettes <- list(
    # Activity intensity levels (sedentary -> very vigorous)
    intensity = c(
      sedentary      = "#1A4D7A",  # Darker deep blue - rest/base
      light          = "#4DB8E8",  # Richer sky blue - gentle activity
      moderate       = "#FFB800",  # Richer golden yellow - active
      vigorous       = "#D95520",  # Darker burnt orange - high energy
      very_vigorous  = "#E6358B"   # Darker hot pink - peak intensity
    ),

    # Status and quality indicators
    status = c(
      valid          = "#71984A",  # Olive green - good/valid
      invalid        = "#E8E8E8",  # Light gray - missing/invalid
      wear           = "#71984A",  # Green - device worn
      nonwear        = "#E8E8E8",  # Gray - device not worn
      warning        = "#DF6A2E",  # Orange - attention needed
      highlight      = "#FFCD00"   # Yellow - emphasis
    ),

    # Sleep states
    sleep = c(
      wake           = "#FFCD00",  # Yellow - awake/activity
      sleep          = "#236192",  # Blue - sleep
      bedtime        = "#71984A",  # Green - sleep onset
      waketime       = "#DF6A2E",  # Orange - wake time
      period         = "#87D1E6"   # Light blue - sleep period background
    ),

    # Full primary palette
    primary = c(
      blue           = "#236192",
      yellow         = "#FFCD00",
      orange         = "#DF6A2E",
      cyan           = "#87D1E6",
      green          = "#71984A",
      pink           = "#F45197"
    )
  )

  colors <- palettes[[palette]]

  if (reverse) {
    colors <- rev(colors)
  }

  return(colors)
}


#' Get Single canhrActi Color
#'
#' Retrieve a specific color from the canhrActi palette.
#'
#' @param name Character. Color name (e.g., "blue", "yellow", "sedentary", "valid")
#'
#' @return Character hex color code
#'
#' @examples
#' \dontrun{
#' canhrActi_color("blue")
#' canhrActi_color("moderate")
#' }
#'
#' @export
canhrActi_color <- function(name) {
  all_colors <- c(
    # Primary colors
    blue           = "#236192",
    yellow         = "#FFCD00",
    orange         = "#DF6A2E",
    cyan           = "#87D1E6",
    green          = "#71984A",
    pink           = "#F45197",

    # Semantic colors
    sedentary      = "#1A4D7A",
    light          = "#4DB8E8",
    moderate       = "#FFB800",
    vigorous       = "#D95520",
    very_vigorous  = "#E6358B",
    valid          = "#71984A",
    invalid        = "#E8E8E8",
    wear           = "#71984A",
    nonwear        = "#E8E8E8",
    warning        = "#DF6A2E",
    highlight      = "#FFCD00",
    wake           = "#FFCD00",
    sleep          = "#236192",
    bedtime        = "#71984A",
    waketime       = "#DF6A2E",
    period         = "#87D1E6"
  )

  if (!name %in% names(all_colors)) {
    stop("Color '", name, "' not found in canhrActi palette.\n",
         "Available colors: ", paste(names(all_colors), collapse = ", "))
  }

  return(all_colors[[name]])
}


#' Show canhrActi Color Palettes
#'
#' Display all available color palettes with a visual preview.
#'
#' @export
show_canhrActi_colors <- function() {
  palettes <- c("intensity", "status", "sleep", "primary")

  for (pal in palettes) {
    colors <- canhrActi_palette(pal)
    cat("\n", toupper(pal), "Palette:\n", sep = "")
    for (i in seq_along(colors)) {
      cat(sprintf("  %-15s %s\n", names(colors)[i], colors[i]))
    }
  }

  invisible(NULL)
}
