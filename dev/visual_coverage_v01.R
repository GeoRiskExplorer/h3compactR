# =============================================================================
# h3compactR
# Visual QA — polygon coverage rules
#
# Compare the four polygon-to-H3 coverage definitions on one canvas.
#
# Development / visual QA script only.
# Not part of the package API.
# =============================================================================


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

library(h3compactR)
library(sf)
library(ggplot2)
library(patchwork)

sf::sf_use_s2(TRUE)


# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

geometry_id <- "complex"
resolution <- 8L
min_overlap <- 0.50


# -----------------------------------------------------------------------------
# Source polygon
# -----------------------------------------------------------------------------

data(
  "toy_polygons",
  package = "h3compactR"
)

source_polygon <- toy_polygons[
  toy_polygons$geometry_id == geometry_id,
]

stopifnot(
  nrow(source_polygon) == 1L
)


# -----------------------------------------------------------------------------
# Generate the four coverage representations
# -----------------------------------------------------------------------------

coverage_within <- h3_cover_polygon(
  source_polygon,
  resolution = resolution,
  boundary = "within"
)

coverage_center <- h3_cover_polygon(
  source_polygon,
  resolution = resolution,
  boundary = "center"
)

coverage_intersects <- h3_cover_polygon(
  source_polygon,
  resolution = resolution,
  boundary = "intersects"
)

coverage_overlap <- h3_cover_polygon(
  source_polygon,
  resolution = resolution,
  boundary = "overlap",
  min_overlap = min_overlap
)


# -----------------------------------------------------------------------------
# QA
# -----------------------------------------------------------------------------

stopifnot(
  all(coverage_within %in% coverage_intersects),
  all(coverage_center %in% coverage_intersects),
  all(coverage_overlap %in% coverage_intersects)
)


coverage_summary <- data.frame(
  method = c(
    "Within",
    "Center",
    "Intersects",
    paste0("Overlap >= ", min_overlap)
  ),
  cells = c(
    length(coverage_within),
    length(coverage_center),
    length(coverage_intersects),
    length(coverage_overlap)
  )
)

print(coverage_summary)


# -----------------------------------------------------------------------------
# Convert H3 indexes to sf geometry
# -----------------------------------------------------------------------------

to_h3_sf <- function(cells) {

  if (length(cells) == 0L) {
    return(
      sf::st_sf(
        h3 = character(),
        geometry = sf::st_sfc(crs = 4326)
      )
    )
  }

  geometry <- h3jsr::cell_to_polygon(
    cells,
    simple = FALSE
  )

  geometry <- sf::st_transform(
    geometry,
    sf::st_crs(source_polygon)
  )

  sf::st_sf(
    h3 = cells,
    geometry = sf::st_geometry(geometry)
  )
}


sf_within <- to_h3_sf(
  coverage_within
)

sf_center <- to_h3_sf(
  coverage_center
)

sf_intersects <- to_h3_sf(
  coverage_intersects
)

sf_overlap <- to_h3_sf(
  coverage_overlap
)


# -----------------------------------------------------------------------------
# Common plot function
# -----------------------------------------------------------------------------

coverage_plot <- function(
  cells_sf,
  source,
  title,
  subtitle
) {

  ggplot() +

    geom_sf(
      data = cells_sf,
      fill = NA,
      linewidth = 0.25
    ) +

    geom_sf(
      data = source,
      fill = NA,
      linewidth = 0.8
    ) +

    coord_sf(
      datum = NA
    ) +

    labs(
      title = title,
      subtitle = subtitle
    ) +

    theme_void() +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 12,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 9,
        hjust = 0.5
      ),
      plot.margin = margin(
        8,
        8,
        8,
        8
      )
    )
}


# -----------------------------------------------------------------------------
# Individual panels
# -----------------------------------------------------------------------------

p_within <- coverage_plot(
  sf_within,
  source_polygon,
  title = "Within",
  subtitle = paste(
    length(coverage_within),
    "cells"
  )
)

p_center <- coverage_plot(
  sf_center,
  source_polygon,
  title = "Center",
  subtitle = paste(
    length(coverage_center),
    "cells"
  )
)

p_intersects <- coverage_plot(
  sf_intersects,
  source_polygon,
  title = "Intersects",
  subtitle = paste(
    length(coverage_intersects),
    "cells"
  )
)

p_overlap <- coverage_plot(
  sf_overlap,
  source_polygon,
  title = paste0(
    "Overlap >= ",
    min_overlap
  ),
  subtitle = paste(
    length(coverage_overlap),
    "cells"
  )
)


# -----------------------------------------------------------------------------
# Assemble one comparison canvas
# -----------------------------------------------------------------------------

coverage_canvas <- (
  p_within |
    p_center
) /
  (
    p_intersects |
      p_overlap
  ) +
  plot_annotation(
    title = "Polygon-to-H3 coverage methods",
    subtitle = paste0(
      "Geometry: ",
      geometry_id,
      "   |   H3 resolution: ",
      resolution
    ),
    caption = paste0(
      "Overlap threshold = ",
      min_overlap,
      " of H3 cell area"
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 16,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 10,
        hjust = 0.5
      ),
      plot.caption = element_text(
        size = 8,
        hjust = 0.5
      )
    )
  )


# -----------------------------------------------------------------------------
# Display
# -----------------------------------------------------------------------------

print(
  coverage_canvas
)


# -----------------------------------------------------------------------------
# Optional high-resolution QA export
# -----------------------------------------------------------------------------

output_dir <- file.path(
  "dev",
  "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  paste0(
    "coverage_comparison_",
    geometry_id,
    "_res",
    resolution,
    ".png"
  )
)

ggsave(
  filename = output_file,
  plot = coverage_canvas,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300
)

cat(
  "\nCoverage visual QA saved to:\n",
  normalizePath(
    output_file,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)