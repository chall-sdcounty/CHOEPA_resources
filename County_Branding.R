##########################################################
# Set Defaults for County Brand (color and plot template)
##########################################################

# Install if needed:
# install.packages(c("tidyverse", "showtext", "ggtext"))

library(tidyverse)
library(showtext)
library(ggtext)  # optional: rich text titles, superscripts, spans

# -----------------------------------------
# Define brand palette and font
# -----------------------------------------
brand_colors <- c(
  "DarkBlue"   = "#003087",
  "LightBlue"  = "#00B5E2",
  "DarkGreen"  = "#67823A",
  "LightGreen" = "#A9C23F",
  "Orange"     = "#FF9E18",
  "Peach"      = "#F3CFB3",
  "DarkGray"   = "#545859"
)

# add Times New Roman font
font_add(family = "Times New Roman",
         regular = "C:/Windows/Fonts/times.ttf",
         bold    = "C:/Windows/Fonts/timesbd.ttf",
         italic  = "C:/Windows/Fonts/timesi.ttf",
         bolditalic = "C:/Windows/Fonts/timesbi.ttf")

# -----------------------------------------
# function to use brand colors
# -----------------------------------------
use_county_brand_colors <- function(palette = brand_colors) {
  scale_colour_discrete <<- function(...) ggplot2::scale_color_manual(values = unname(palette), ...)
  scale_color_discrete  <<- function(...) ggplot2::scale_color_manual(values = unname(palette), ...)
  scale_fill_discrete   <<- function(...) ggplot2::scale_fill_manual(values = unname(palette), ...)
  message("Global discrete scales now use brand_colors.")
}

# function to reset to original colors
reset_county_brand_colors <- function() {
  rm(list = c("scale_colour_discrete","scale_color_discrete","scale_fill_discrete"), envir = .GlobalEnv)
  message("Discrete scales reset to ggplot2 defaults (restart R if needed).")
}

# -----------------------------------------
# Function to Define County theme (fonts, typography etc.)
# -----------------------------------------
theme_county <- function(
    base_size   = 12,
    base_family = "Times New Roman",
    grid_color  = "#E6E6E6",
    legend_pos  = "top"
) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(family = base_family),
      plot.title.position = "plot",
      plot.title   = ggplot2::element_text(face = "bold", size = base_size * 1.4, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(size = base_size * 1.1, margin = ggplot2::margin(b = 10)),
      plot.caption  = ggplot2::element_text(color = "gray40", size = base_size * 0.9, margin = ggplot2::margin(t = 8)),
      axis.title.x  = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y  = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.text     = ggplot2::element_text(color = "gray20"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = grid_color, linewidth = 0.5),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.spacing    = grid::unit(1, "lines"),
      legend.position  = legend_pos,
      legend.title     = ggplot2::element_text(face = "bold"),
      legend.key.width = grid::unit(14, "pt"),
      legend.text      = ggplot2::element_text(size = base_size * 0.95),
      strip.background = ggplot2::element_rect(fill = "#F7F7F7", color = NA),
      strip.text       = ggplot2::element_text(face = "bold")
    )
}

# -----------------------------------------
# Apply brand colors
# -----------------------------------------
use_county_brand_colors()

