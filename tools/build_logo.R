# Build the canhrActi hex logo from tools/logo-mark.png, and every copy of it:
# the README hex, the dashboard logo and the desktop app icon.
# Run from the package root:  Rscript tools/build_logo.R
# Then rebuild the desktop .ico, .icns and installer pictures from the new icon.

library(ggplot2)
library(showtext)
library(sysfonts)

font_add_google("Manrope", "manrope")
font_add_google("Montserrat", "montserrat")
showtext_auto()
showtext_opts(dpi = 600)

navy <- "#1E3A5F"
deep <- "#14263F"
mid  <- "#2A4F7E"
gold <- "#FFCD00"

logo <- png::readPNG("tools/logo-mark.png")

# clear the faint specks in the transparent background
solid <- logo[, , 4] > 0.5
near  <- solid
for (dx in -3:3) for (dy in -3:3) {
  r <- pmin(pmax(seq_len(nrow(solid)) + dy, 1), nrow(solid))
  k <- pmin(pmax(seq_len(ncol(solid)) + dx, 1), ncol(solid))
  near <- near | solid[r, k]
}
logo[, , 4][!near] <- 0

hexagon <- function(scale = 1) {
  data.frame(
    x = scale * c(0, -sqrt(3) / 2, -sqrt(3) / 2, 0, sqrt(3) / 2, sqrt(3) / 2, 0),
    y = scale * c(1, 0.5, -0.5, -1, -0.5, 0.5, 1)
  )
}

circle <- function(r, cx = 0, cy = 0) {
  t <- seq(0, 2 * pi, length.out = 240)
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}

clamp <- function(x) pmin(pmax(x, 0), 1)

# pixel grid over the plot area, for the gradient and the shadow
n <- 700
X <- matrix(seq(-1, 1, length.out = n), n, n, byrow = TRUE)
Y <- matrix(seq(1, -1, length.out = n), n, n)

# radial gradient from mid to deep, clipped to the hex
radial <- function(s = 0.935, cy = 0.1) {
  d  <- clamp(sqrt(X^2 + (Y - cy)^2) / 1.05)^1.4
  c1 <- grDevices::col2rgb(mid) / 255
  c2 <- grDevices::col2rgb(deep) / 255
  a  <- array(0, c(n, n, 4))
  for (k in 1:3) a[, , k] <- c1[k] + (c2[k] - c1[k]) * d
  a[, , 4] <- abs(X) <= s * sqrt(3) / 2 & abs(Y) <= s - abs(X) / sqrt(3)
  a
}

# soft shadow under the disc
shadow <- function(r, cy) {
  d <- sqrt(X^2 + (Y - cy + 0.025)^2)
  a <- array(0, c(n, n, 4))
  a[, , 4] <- 0.22 * clamp((r + 0.07 - d) / 0.14)^2
  a
}

cy   <- 0.17    # centre of the disc
half <- 0.545   # half the width of the logo image
disc <- 0.60    # radius of the white disc

# the side dots are cut at the edge of logo-mark.png, so they are drawn whole here
to_xy <- function(col, row) {
  c(-half + (col - 0.5) / 1024 * 2 * half, cy + half - (row - 0.5) / 1024 * 2 * half)
}
dots <- do.call(rbind, lapply(c(16.5, 1008.5), function(col) {
  p <- to_xy(col, 510.5)
  cbind(circle(25.5 / 1024 * 2 * half, p[1], p[2]), id = col)
}))

name <- sprintf("<span style='color:#FFFFFF'>canhr</span><span style='color:%s'>Acti</span>", gold)

logo_hex <- ggplot() +
  geom_polygon(data = hexagon(1), aes(x, y), fill = navy) +
  annotation_raster(radial(), -1, 1, -1, 1, interpolate = TRUE) +
  geom_path(data = hexagon(0.9), aes(x, y), colour = gold, linewidth = 0.35, linejoin = "mitre") +
  annotation_raster(shadow(disc, cy), -1, 1, -1, 1, interpolate = TRUE) +
  geom_polygon(data = circle(disc, 0, cy), aes(x, y), fill = "#FFFFFF") +
  geom_path(data = circle(disc - 0.004, 0, cy), aes(x, y), colour = gold, linewidth = 0.3) +
  annotation_raster(logo, -half, half, cy - half, cy + half, interpolate = TRUE) +
  geom_polygon(data = dots, aes(x, y, group = id), fill = "#FDD118") +
  ggtext::geom_richtext(
    aes(x = 0, y = -0.535, label = name), family = "manrope", fontface = "bold", size = 3.9,
    fill = NA, label.colour = NA, label.padding = grid::unit(0, "pt")
  ) +
  annotate(
    "text", x = 0, y = -0.675, label = "CANHR · UAF",
    colour = "#A9BCD6", family = "montserrat", fontface = "bold", size = 1.6
  ) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))

# README hex, at the usual sticker size
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(
  "man/figures/logo.png",
  logo_hex + coord_fixed(xlim = c(-1, 1), ylim = c(-1.05, 1.05), expand = FALSE, clip = "off"),
  width = 5.08, height = 5.86, units = "cm", dpi = 600, bg = "transparent"
)

# square copies, for places that show the logo in a square box; text and
# lines are sized in cm, so the square keeps the sticker's scale
square <- logo_hex + coord_fixed(xlim = c(-1.02, 1.02), ylim = c(-1.02, 1.02), expand = FALSE, clip = "off")
side <- 5.08 * 1.02
save_square <- function(file, px) {
  dpi <- px / (side / 2.54)
  showtext_opts(dpi = dpi)
  ggsave(file, square, width = side, height = side, units = "cm", dpi = dpi, bg = "transparent")
}
save_square("inst/shiny/canhrActi_dashboard/www/logo.png", 512)
save_square("canhrActi-desktop/build/icon.png", 1024)
save_square("canhrActi-desktop/src/renderer/icon.png", 1024)
