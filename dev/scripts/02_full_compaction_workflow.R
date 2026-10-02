# =============================================================================
# h3compactR full compaction workflow
# =============================================================================
#
# Purpose:
# Demonstrate a complete toy workflow using:
#   1. source H3 generation;
#   2. native H3 compaction;
#   3. boundary refinement;
#   4. synthetic point assignment;
#   5. count aggregation;
#   6. structural QA; and
#   7. cartographic QA.
#
# This is a development workflow only. It should remain independent of any
# PLM-specific production data or analytical calculations.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load package and dependencies
# -----------------------------------------------------------------------------

devtools::load_all()

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(h3jsr)
})


# -----------------------------------------------------------------------------
# 2. Development parameters
# -----------------------------------------------------------------------------

base_resolution <- 7L
boundary_resolution <- 8L

point_count <- 500L
random_seed <- 20260711L


# -----------------------------------------------------------------------------
# 3. Create toy spatial container
# -----------------------------------------------------------------------------
#
# A simple Melbourne-area bounding polygon is used here as the analysis
# container. Later tests should use more irregular and discontinuous geometry.

container <- st_as_sfc(
  st_bbox(
    c(
      xmin = 144.80,
      ymin = -37.95,
      xmax = 145.15,
      ymax = -37.70
    ),
    crs = st_crs(4326)
  )
)

container_sf <- st_sf(
  container_id = "toy_container",
  geometry = container
)


# -----------------------------------------------------------------------------
# 4. Build source H3 support
# -----------------------------------------------------------------------------
#
# Generate the complete source grid at the base resolution.
# These cells represent the authoritative spatial support for this toy example.

source_h3 <- polygon_to_cells(
  container,
  res = base_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()


# -----------------------------------------------------------------------------
# 5. Native H3 compaction
# -----------------------------------------------------------------------------
#
# Compact the source H3 set using the H3 hierarchy.
# Complete groups of child cells may be represented by their parent cells.

compacted_h3 <- compact_h3(source_h3)


# -----------------------------------------------------------------------------
# 6. Boundary refinement
# -----------------------------------------------------------------------------
#
# Refine the compact representation around the container boundary.
# The goal is to retain cartographic efficiency while improving spatial fit
# at edges where coarse compact cells may extend beyond the target container.

refined <- refine_h3_boundary(
  source_h3 = source_h3,
  compacted_h3 = compacted_h3,
  container = container,
  boundary_resolution = boundary_resolution
)


# -----------------------------------------------------------------------------
# 7. Generate synthetic point events
# -----------------------------------------------------------------------------
#
# Points provide a simple test of mixed-resolution assignment and generic
# count preservation.

set.seed(random_seed)

points <- st_sample(
  container_sf,
  size = point_count,
  exact = TRUE
) |>
  st_sf() |>
  mutate(
    point_id = row_number(),
    event_count = 1L
  )


# -----------------------------------------------------------------------------
# 8. Assign source H3 IDs to points
# -----------------------------------------------------------------------------
#
# Assign each point to the finer boundary resolution.
# This gives us a controlled source H3 reference for testing aggregation.

points$source_h3 <- point_to_cell(
  points,
  res = boundary_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE)


# -----------------------------------------------------------------------------
# 9. Aggregate point counts to refined compact cells
# -----------------------------------------------------------------------------
#
# Count aggregation is used here only as a generic functional test.
# Project-specific analytical aggregation should remain outside h3compactR.

counts <- aggregate_h3_counts(
  points,
  source_h3 = source_h3,
  compacted_h3 = refined$h3,
  count = event_count
)


# -----------------------------------------------------------------------------
# 10. Build mapped compact geometry
# -----------------------------------------------------------------------------
#
# Join aggregated counts to the refined display geometry.

map_sf <- refined$display_geometry |>
  left_join(
    counts,
    by = c(
      "h3" = "matched_h3",
      "resolution" = "matched_resolution"
    )
  ) |>
  mutate(
    count = coalesce(.data$count, 0)
  )


# -----------------------------------------------------------------------------
# 11. Structural QA
# -----------------------------------------------------------------------------

hierarchy_overlaps <- nrow(
  check_h3_hierarchy(refined$h3)
)

input_event_total <- sum(points$event_count)

output_event_total <- sum(counts$count)

stopifnot(
  hierarchy_overlaps == 0L,
  input_event_total == output_event_total
)


# -----------------------------------------------------------------------------
# 12. Console QA summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FULL COMPACTION WORKFLOW QA\n")
cat("============================================================\n\n")

cat("Source H3 cells:       ", length(source_h3), "\n")
cat("Native compact cells:  ", length(compacted_h3), "\n")
cat("Refined compact cells: ", length(refined$h3), "\n")
cat("Hierarchy overlaps:    ", hierarchy_overlaps, "\n")
cat("Input event total:     ", input_event_total, "\n")
cat("Output event total:    ", output_event_total, "\n")
cat("\n")

print(refined$qa)


# -----------------------------------------------------------------------------
# 13. Cartographic QA plot
# -----------------------------------------------------------------------------
#
# Plot:
#   - toy container boundary;
#   - refined mixed-resolution compact cells;
#   - synthetic point events.
#
# Explicit print() is required so the plot displays when this file is executed
# with source().

comparison_plot <- ggplot() +
  geom_sf(
    data = container_sf,
    fill = "grey98",
    colour = "black",
    linewidth = 0.7
  ) +
  geom_sf(
    data = map_sf,
    aes(fill = factor(.data$resolution)),
    colour = "grey30",
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_sf(
    data = points,
    size = 0.3,
    alpha = 0.4
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +
  labs(
    title = "Boundary-refined H3 compaction",
    subtitle = paste0(
      length(source_h3),
      " source cells → ",
      length(compacted_h3),
      " native compact cells → ",
      length(refined$h3),
      " refined cells"
    ),
    caption = paste0(
      "Boundary gap reduction: ",
      round(refined$qa$gap_reduction_pct, 1),
      "%"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  ) +
  coord_sf()

print(comparison_plot)


# -----------------------------------------------------------------------------
# 14. Final workflow status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FULL COMPACTION WORKFLOW PASSED\n")
cat("============================================================\n")