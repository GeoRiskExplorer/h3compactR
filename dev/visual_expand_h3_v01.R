# =============================================================================
# h3compactR
# Visual QA — H3 neighbourhood expansion
#
# Purpose:
#   Visually confirm that expand_h3() produces coherent H3 neighbourhood
#   expansion while preserving the original cell set and H3 resolution.
#
# Development script only — not part of the public package API.
# =============================================================================

library(h3compactR)
library(sf)
library(ggplot2)
library(patchwork)


# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

geometry_id <- "complex"
resolution <- 8L


# -----------------------------------------------------------------------------
# 2. Select source polygon
# -----------------------------------------------------------------------------

source_polygon <- toy_polygons[
  toy_polygons$geometry_id == geometry_id,
]


# -----------------------------------------------------------------------------
# 3. Generate source H3 coverage
# -----------------------------------------------------------------------------

source_cells <- h3_cover_polygon(
  source_polygon,
  resolution = resolution,
  boundary = "center"
)


# -----------------------------------------------------------------------------
# 4. Expand H3 coverage
# -----------------------------------------------------------------------------

ring_1 <- expand_h3(
  source_cells,
  rings = 1L
)

ring_2 <- expand_h3(
  source_cells,
  rings = 2L
)


# -----------------------------------------------------------------------------
# 5. Numerical QA
# -----------------------------------------------------------------------------

stopifnot(
  all(source_cells %in% ring_1),
  all(ring_1 %in% ring_2),
  all(h3jsr::get_res(source_cells) == resolution),
  all(h3jsr::get_res(ring_1) == resolution),
  all(h3jsr::get_res(ring_2) == resolution)
)

cat(
  "\n",
  "======================================================================\n",
  "EXPAND_H3 VISUAL QA\n",
  "======================================================================\n",
  "Geometry:       ", geometry_id, "\n",
  "Resolution:     ", resolution, "\n",
  "Source cells:   ", length(source_cells), "\n",
  "Ring 1 cells:   ", length(ring_1), "\n",
  "Ring 2 cells:   ", length(ring_2), "\n",
  "Ring 1 added:   ", length(setdiff(ring_1, source_cells)), "\n",
  "Ring 2 added:   ", length(setdiff(ring_2, source_cells)), "\n",
  "======================================================================\n",
  sep = ""
)


# -----------------------------------------------------------------------------
# 6. Convert H3 indexes to geometry
# -----------------------------------------------------------------------------

source_sf <- h3jsr::cell_to_polygon(
  source_cells,
  simple = FALSE
)

ring_1_sf <- h3jsr::cell_to_polygon(
  ring_1,
  simple = FALSE
)

ring_2_sf <- h3jsr::cell_to_polygon(
  ring_2,
  simple = FALSE
)


# -----------------------------------------------------------------------------
# 7. Plot helper
# -----------------------------------------------------------------------------

make_plot <- function(
  h3_geometry,
  title,
  subtitle
) {

  ggplot() +
    geom_sf(
      data = h3_geometry,
      fill = NA,
      linewidth = 0.30
    ) +
    geom_sf(
      data = source_polygon,
      fill = NA,
      linewidth = 0.65
    ) +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      plot.subtitle = element_text(
        size = 9
      )
    )
}


# -----------------------------------------------------------------------------
# 8. Build comparison plots
# -----------------------------------------------------------------------------

p_source <- make_plot(
  source_sf,
  "Original coverage",
  paste0(
    length(source_cells),
    " cells"
  )
)

p_ring_1 <- make_plot(
  ring_1_sf,
  "Expansion: 1 ring",
  paste0(
    length(ring_1),
    " cells | +",
    length(setdiff(ring_1, source_cells)),
    " cells"
  )
)

p_ring_2 <- make_plot(
  ring_2_sf,
  "Expansion: 2 rings",
  paste0(
    length(ring_2),
    " cells | +",
    length(setdiff(ring_2, source_cells)),
    " cells"
  )
)


# -----------------------------------------------------------------------------
# 9. Assemble visual QA figure
# -----------------------------------------------------------------------------

figure <- (
  p_source |
    p_ring_1 |
    p_ring_2
) +
  plot_annotation(
    title = "H3 neighbourhood expansion",
    subtitle = paste0(
      "Geometry: ",
      geometry_id,
      " | H3 resolution: ",
      resolution
    ),
    caption = paste0(
      "Expansion is an H3 neighbourhood operation. ",
      "New cells are not constrained to the source polygon."
    )
  )


# -----------------------------------------------------------------------------
# 10. Display figure
# -----------------------------------------------------------------------------

print(
  figure
)


# -----------------------------------------------------------------------------
# 11. Export figure
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
    "expand_h3_",
    geometry_id,
    "_res",
    resolution,
    ".png"
  )
)

ggsave(
  filename = output_file,
  plot = figure,
  width = 12,
  height = 5,
  units = "in",
  dpi = 200
)

cat(
  "\nFigure written to:\n",
  output_file,
  "\n",
  sep = ""
)
