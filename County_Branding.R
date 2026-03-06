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
  "DarkBlue"    = "#003087",
  "LightBlue"   = "#00B5E2",
  "DarkGreen"   = "#67823A",
  "LightGreen"  = "#A9C23F",
  "Orange"      = "#FF9E18",
  "Peach"       = "#F3CFB3",
  "DarkGray"    = "#545859", 
  "BlueGrad1"   = "#B6C7E4", 
  "BlueGrad2"   = "#6E91C7",
  "BlueGrad3"   = "#2B58A8",
  "BlueGrad4"   = "#003087",
  "BlueGrad5"   = "#001E5A",
  "GreenGrad1"  = "#D8EAB4", 
  "GreenGrad2"  = "#B4C98A",
  "GreenGrad3"  = "#8FA861",
  "GreenGrad4"  = "#67823A",
  "GreenGrad5"  = "#405226"
  
)

# add Times New Roman font

font_add(
  family     = "Times New Roman",
  regular    = "C:/Windows/Fonts/times.ttf",
  bold       = "C:/Windows/Fonts/timesbd.ttf",
  italic     = "C:/Windows/Fonts/timesi.ttf",
  bolditalic = "C:/Windows/Fonts/timesbi.ttf"
)

# 2) Activate showtext (must be ON before plotting)
showtext_auto(enable = TRUE)
# Optional: better text quality for saved images
showtext_opts(dpi = 300)


# -----------------------------------------
# function to use brand colors
# -----------------------------------------

use_county_brand_colors <- function(palette = brand_colors) {
  stopifnot(is.character(palette), length(palette) >= 1)
  
  # Use unname() to avoid "No shared levels found..." warnings
  vals <- unname(palette)
  
  assign("scale_colour_discrete",
         function(...) scale_color_manual(values = vals, ...),
         envir = .GlobalEnv)
  
  assign("scale_color_discrete",
         function(...) scale_color_manual(values = vals, ...),
         envir = .GlobalEnv)
  
  assign("scale_fill_discrete",
         function(...) scale_fill_manual(values = vals, ...),
         envir = .GlobalEnv)
  
  message("✅ Global discrete scales set: color/fill will use your brand_colors by default in this session.")
}

reset_county_brand_colors <- function() {
  # Remove global overrides to reveal ggplot2’s originals again
  for (nm in c("scale_colour_discrete", "scale_color_discrete", "scale_fill_discrete")) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = nm, envir = .GlobalEnv)
    }
  }
  message("↩️ Discrete scales reset to ggplot2 defaults. (You may need to re-run plots.)")
}


# -----------------------------------------
# Function to Define County theme (fonts, typography etc.)
# -----------------------------------------
theme_county <- function(
    base_size   = 12,
    base_family = "Times New Roman",
    grid_color  = "#E6E6E6",
    legend_pos  = "none"
) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family),
      plot.margin = margin(t = 8, r = 15, b = 8, l = 5),
      plot.title.position = "plot",
      plot.title   = element_text(face = "bold", hjust = 0.5, size = base_size * 1.1, margin = margin(b = 2)),
      plot.subtitle = element_text(face = "italic", size = base_size * 0.9,  hjust = 0.5, margin = margin(b = 0)),
      plot.caption  = element_text(size = base_size * 0.9, margin = margin(t = 8)),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(color = "black"),
      axis.text.y   = element_text(color = "black", hjust = 1, margin = margin(r = -5, l = 0)),  # right-justify, zero gap
      axis.title.x  = element_text(margin = margin(t = 6)),
      axis.title.y  = element_text(margin = margin(r = 6)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.spacing    = grid::unit(1, "lines"),
      legend.position  = legend_pos,
      legend.title = element_blank(),
      legend.key.width = grid::unit(14, "pt"),
      legend.key.spacing.x = unit(14, "pt"),
      legend.text      = element_text(size = base_size * 0.95),
      strip.background = element_rect(fill = "#F7F7F7", color = NA),
      strip.text       = element_text(face = "bold")
    )
}

# -----------------------------------------
# Apply brand colors
# -----------------------------------------
use_county_brand_colors()

