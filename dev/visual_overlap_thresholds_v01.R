# =============================================================================
# h3compactR
# Visual QA — overlap coverage thresholds
#
# Purpose:
#   Compare polygon-to-H3 overlap coverage across a range of minimum overlap
#   thresholds using the same polygon and H3 resolution.
#
# Definition:
#
#   overlap =
#     area(H3 cell intersect polygon) /
#     area(H3 cell)
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

thresholds <- c(
  0.10,
  0.25,
  0.50,
  0.75,
  0.90,
  1.00
)


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
# Generate overlap coverage
# -----------------------------------------------------------------------------

coverage <- lapply(
  thresholds,
  function(threshold) {

    h3_cover_polygon(
      source_polygon,
      resolution = resolution,
      boundary = "overlap",
      min_overlap = threshold
    )
  }
)

names(coverage) <- sprintf(
  "%.2f",
  thresholds
)


# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

coverage_summary <- data.frame(
  threshold = thresholds,
  cells = vapply(
    coverage,
    length,
    integer(1)
  )
)

cat(
  "\n",
  paste(rep("=", 72), collapse = ""),
  "\nH3COMPACTR — OVERLAP THRESHOLD VISUAL QA\n",
  paste(rep("=", 72), collapse = ""),
  "\n\n",
  sep = ""
)

print(
  coverage_summary,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# Monotonic QA
#
# Increasing the minimum overlap threshold must never introduce a new cell.
# -----------------------------------------------------------------------------

for (i in 2:length(coverage)) {

  lower <- coverage[[i - 1L]]
  higher <- coverage[[i]]

  stopifnot(
    all(
      higher %in% lower
    )
  )

  stopifnot(
    length(higher) <= length(lower)
  )
}

cat(
  "\nMonotonic threshold QA: PASS\n"
)


# -----------------------------------------------------------------------------
# Convert H3 indexes to sf
# -----------------------------------------------------------------------------

to_h3_sf <- function(cells) {

  if (length(cells) == 0L) {

    return(
      sf::st_sf(
        h3 = character(),
        geometry = sf::st_sfc(
          crs = sf::st_crs(source_polygon)
        )
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


coverage_sf <- lapply(
  coverage,
  to_h3_sf
)


# -----------------------------------------------------------------------------
# Common plotting function
# -----------------------------------------------------------------------------

overlap_plot <- function(
  cells_sf,
  source,
  threshold,
  cell_count
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
      title = paste0(
        "Overlap >= ",
        sprintf(
          "%.2f",
          threshold
        )
      ),
      subtitle = paste(
        cell_count,
        "cells"
      )
    ) +

    theme_void() +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 9,
        hjust = 0.5
      ),
      plot.margin = margin(
        6,
        6,
        6,
        6
      )
    )
}


# -----------------------------------------------------------------------------
# Build plots
# -----------------------------------------------------------------------------

plots <- Map(
  function(
    cells_sf,
    threshold,
    cell_count
  ) {

    overlap_plot(
      cells_sf = cells_sf,
      source = source_polygon,
      threshold = threshold,
      cell_count = cell_count
    )
  },
  coverage_sf,
  thresholds,
  coverage_summary$cells
)


# -----------------------------------------------------------------------------
# Assemble one 3 x 2 comparison canvas
#
# Reading order:
#
#   0.10     0.25
#   0.50     0.75
#   0.90     1.00
# -----------------------------------------------------------------------------

overlap_canvas <- wrap_plots(
  plots,
  ncol = 2
) +
  plot_annotation(
    title = "Polygon-to-H3 overlap thresholds",
    subtitle = paste0(
      "Geometry: ",
      geometry_id,
      "   |   H3 resolution: ",
      resolution
    ),
    caption = paste0(
      "Threshold = minimum proportion of each H3 cell area ",
      "covered by the source polygon"
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
  overlap_canvas
)


# -----------------------------------------------------------------------------
# Export
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
    "overlap_thresholds_",
    geometry_id,
    "_res",
    resolution,
    ".png"
  )
)

ggsave(
  filename = output_file,
  plot = overlap_canvas,
  width = 10,
  height = 13,
  units = "in",
  dpi = 300
)


# -----------------------------------------------------------------------------
# Complete
# -----------------------------------------------------------------------------

cat(
  "\n",
  paste(rep("=", 72), collapse = ""),
  "\nOVERLAP THRESHOLD VISUAL QA COMPLETE\n",
  paste(rep("=", 72), collapse = ""),
  "\n",
  sep = ""
)

cat(
  "\nOutput:\n",
  normalizePath(
    output_file,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)