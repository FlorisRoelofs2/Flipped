# global.R

library(shiny)
library(later)
library(bslib)
library(fontawesome)

# Thema instellingen
app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#007bff",
  base_font = font_google("Roboto"),
  heading_font = font_google("Bangers")
)

# Helper functies (optioneel)
format_points <- function(points) {
  paste0(points, " pts")
}

# Streak multiplier only starts having effect after 1st flip