# =============================================================================
# h3compactR
# Boundary refinement experiment
#
# File:
#   dev/playpen/compaction/02_boundary_refinement.R
#
# Purpose:
#   Test whether finer H3 cells can supplement gaps around a container boundary
#   while retaining the compacted interior grid.
#
# Development status:
#   Experimental. Do not move this logic into R/ until the behaviour,
#   parameters and failure cases are settled.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load package and dependencies
# -----------------------------------------------------------------------------

devtools::load_all()

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(h3jsr)
})


# -----------------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------------

base_resolution <- 7L
boundary_resolution <- 8L

point_count <- 500L
random_seed <- 20260711L

# Buffer used only to generate enough candidate boundary cells.
boundary_search_buffer_m <- 3000

# Ignore microscopic intersections caused by floating-point geometry.
minimum_gap_overlap_m2 <- 1

container_bbox <- c(
  xmin = 144.80,
  ymin = -37.95,
  xmax = 145.15,
  ymax = -37.70
)


# -----------------------------------------------------------------------------
# 3. Create container
# -----------------------------------------------------------------------------

container_sf <- st_as_sfc(
  st_bbox(
    container_bbox,
    crs = st_crs(4326)
  )
) |>
  st_sf(
    container_id = "toy_container",
    geometry = _
  )

stopifnot(
  st_crs(container_sf)$epsg == 4326,
  all(st_is_valid(container_sf))
)


# -----------------------------------------------------------------------------
# 4. Create and compact the base H3 grid
# -----------------------------------------------------------------------------

source_h3 <- h3jsr::polygon_to_cells(
  container_sf,
  res = base_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique() |>
  validate_h3()

compacted_h3 <- compact_h3(source_h3)

source_sf_full <- h3jsr::cell_to_polygon(
  source_h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  mutate(
    resolution = h3jsr::get_res(.data$h3),
    grid_role = "source"
  )

compacted_sf_full <- h3jsr::cell_to_polygon(
  compacted_h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  mutate(
    resolution = h3jsr::get_res(.data$h3),
    grid_role = "compacted_interior"
  )


# -----------------------------------------------------------------------------
# 5. Clip the compacted grid to the container
# -----------------------------------------------------------------------------

compacted_sf_display <- suppressWarnings(
  st_intersection(
    compacted_sf_full,
    select(container_sf, container_id)
  )
) |>
  filter(!st_is_empty(.data$geometry)) |>
  select(
    h3,
    resolution,
    grid_role,
    geometry
  )


# -----------------------------------------------------------------------------
# 6. Identify container gaps
#
# Extension candidate:
#   identify_h3_container_gaps()
#
# Why useful:
#   Separates canonical H3 coverage from the actual polygon container and
#   provides a measurable basis for deciding whether refinement is required.
# -----------------------------------------------------------------------------

container_7899 <- st_transform(container_sf, 7899)

compacted_union_7899 <- compacted_sf_display |>
  st_transform(7899) |>
  st_union()

gap_sf <- suppressWarnings(
  st_difference(
    container_7899,
    compacted_union_7899
  )
) |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  st_transform(4326)

gap_sf$gap_id <- seq_len(nrow(gap_sf))

initial_gap_area_m2 <- gap_sf |>
  st_transform(7899) |>
  st_area() |>
  sum() |>
  as.numeric()

cat("\n============================================================\n")
cat("INITIAL CONTAINER GAP QA\n")
cat("============================================================\n\n")

cat("Base source cells:", length(source_h3), "\n")
cat("Compacted interior cells:", length(compacted_h3), "\n")
cat("Initial uncovered container area m2:", round(initial_gap_area_m2, 2), "\n")


# -----------------------------------------------------------------------------
# 7. Generate candidate finer cells around the boundary
#
# We buffer only to ensure polygon_to_cells() creates candidate cells on both
# sides of the container edge. The buffer is not part of the final output.
# -----------------------------------------------------------------------------


source_r8 <- h3jsr::get_children(
  h3_address = source_h3,
  res = boundary_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

candidate_h3 <- h3jsr::get_disk(
  h3_address = source_r8,
  ring_size = 1,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique() |>
  validate_h3()

candidate_parent_h3 <- h3jsr::get_parent(
  h3_address = candidate_h3,
  res = base_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE)

candidate_table <- tibble(
  h3 = candidate_h3,
  parent_h3 = candidate_parent_h3
) |>
  filter(!.data$parent_h3 %in% source_h3)

candidate_sf <- h3jsr::cell_to_polygon(
  candidate_table$h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  left_join(candidate_table, by = "h3") |>
  mutate(
    resolution = boundary_resolution,
    grid_role = "boundary_candidate"
  )


# -----------------------------------------------------------------------------
# 8. Retain finer cells intersecting uncovered container areas
# -----------------------------------------------------------------------------

candidate_sf_7899 <- candidate_sf |>
  st_transform(7899) |>
  st_make_valid()

gap_sf_7899 <- gap_sf |>
  st_transform(7899) |>
  st_make_valid()

intersects_gap <- lengths(
  st_intersects(
    candidate_sf_7899,
    gap_sf_7899
  )
) > 0L

boundary_sf_full_7899 <- candidate_sf_7899 |>
  filter(intersects_gap) |>
  mutate(grid_role = "boundary_refinement")

boundary_h3 <- unique(boundary_sf_full_7899$h3)

boundary_sf_display <- suppressWarnings(
  st_intersection(
    boundary_sf_full_7899,
    gap_sf_7899
  )
) |>
  filter(!st_is_empty(geometry)) |>
  select(
    h3,
    resolution,
    grid_role,
    geometry
  ) |>
  st_transform(4326)

boundary_sf_full <- boundary_sf_full_7899 |>
  st_transform(4326)

cat("\n============================================================\n")
cat("BOUNDARY REFINEMENT\n")
cat("============================================================\n\n")

cat("Boundary candidate cells:", nrow(candidate_table), "\n")
cat("Boundary cells retained:", length(boundary_h3), "\n")
cat("Boundary resolution:", boundary_resolution, "\n")


# -----------------------------------------------------------------------------
# 9. Build the combined mixed-resolution H3 set
# -----------------------------------------------------------------------------

refined_h3 <- unique(c(
  compacted_h3,
  boundary_h3
))

refined_h3 <- validate_h3(refined_h3)

refined_hierarchy_qa <- check_h3_hierarchy(refined_h3)

cat("\n============================================================\n")
cat("REFINED HIERARCHY QA\n")
cat("============================================================\n\n")

cat("Total refined H3 cells:", length(refined_h3), "\n")
cat("Hierarchy overlaps:", nrow(refined_hierarchy_qa), "\n")

if (nrow(refined_hierarchy_qa) > 0L) {
  print(refined_hierarchy_qa)
}

stopifnot(
  nrow(refined_hierarchy_qa) == 0L
)


# -----------------------------------------------------------------------------
# 10. Create final clipped display geometry and calculate remaining gaps
# -----------------------------------------------------------------------------

compacted_display_7899 <- compacted_sf_display |>
  st_transform(7899) |>
  st_make_valid()

boundary_display_7899 <- boundary_sf_display |>
  st_transform(7899) |>
  st_make_valid()

refined_display_7899 <- bind_rows(
  compacted_display_7899,
  boundary_display_7899
)

refined_display_union_7899 <- refined_display_7899 |>
  st_geometry() |>
  st_union()

remaining_gap_7899 <- suppressWarnings(
  st_difference(
    st_geometry(container_7899),
    refined_display_union_7899
  )
) |>
  st_make_valid()

remaining_gap_area_m2 <- remaining_gap_7899 |>
  st_area() |>
  sum() |>
  as.numeric()

gap_reduction_pct <- if (initial_gap_area_m2 > 0) {
  100 * (
    initial_gap_area_m2 - remaining_gap_area_m2
  ) / initial_gap_area_m2
} else {
  100
}

# Convert only after all geometry calculations are complete.
refined_display_sf <- refined_display_7899 |>
  st_transform(4326)

cat("\n============================================================\n")
cat("REFINED CONTAINER COVERAGE QA\n")
cat("============================================================\n\n")

cat("Initial gap area m2:", round(initial_gap_area_m2, 2), "\n")
cat("Remaining gap area m2:", round(remaining_gap_area_m2, 2), "\n")
cat("Gap reduction %:", round(gap_reduction_pct, 2), "\n")


# -----------------------------------------------------------------------------
# 11. Validate refined H3 coverage at the finest resolution
#
# Extension candidate:
#   qa_h3_coverage()
#
# Why useful:
#   Confirms that adding boundary cells does not lose or duplicate logical H3
#   coverage when the mixed-resolution set is expanded to one resolution.
# -----------------------------------------------------------------------------

source_at_boundary_res <- h3jsr::uncompact(
  source_h3,
  res = boundary_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

expected_refined_at_boundary_res <- unique(c(
  source_at_boundary_res,
  boundary_h3
))

actual_refined_at_boundary_res <- h3jsr::uncompact(
  refined_h3,
  res = boundary_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

coverage_missing <- setdiff(
  expected_refined_at_boundary_res,
  actual_refined_at_boundary_res
)

coverage_additional <- setdiff(
  actual_refined_at_boundary_res,
  expected_refined_at_boundary_res
)

cat("\n============================================================\n")
cat("REFINED ROUND-TRIP QA\n")
cat("============================================================\n\n")

cat("Missing fine cells:", length(coverage_missing), "\n")
cat("Additional fine cells:", length(coverage_additional), "\n")
cat(
  "Exact refined set equality:",
  setequal(
    expected_refined_at_boundary_res,
    actual_refined_at_boundary_res
  ),
  "\n"
)

stopifnot(
  length(coverage_missing) == 0L,
  length(coverage_additional) == 0L
)


# -----------------------------------------------------------------------------
# 12. Test points sampled from the actual container
#
# Unlike the first workflow, points are now sampled from the container rather
# than the original centre-selected H3 coverage. This directly tests whether
# boundary refinement resolves previously unmatched edge points.
#
# Extension candidate:
#   assign_points_to_compacted_h3()
#
# Why useful:
#   Would combine point_to_cell() and match_h3_to_compacted() into one safe,
#   compaction-specific point assignment workflow.
# -----------------------------------------------------------------------------

set.seed(random_seed)

toy_points <- st_sample(
  container_sf,
  size = point_count,
  type = "random",
  exact = TRUE
) |>
  st_sf() |>
  mutate(
    point_id = row_number(),
    event_count = 1L
  )

toy_points$source_h3 <- h3jsr::point_to_cell(
  toy_points,
  res = boundary_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE)

point_lookup <- match_h3_to_compacted(
  source_h3 = toy_points$source_h3,
  compacted_h3 = refined_h3
)

toy_points <- toy_points |>
  mutate(
    matched_h3 = point_lookup$matched_h3,
    matched_resolution = point_lookup$matched_resolution,
    match_type = point_lookup$match_type
  )

assignment_summary <- toy_points |>
  st_drop_geometry() |>
  count(
    .data$matched_resolution,
    .data$match_type,
    name = "point_count"
  ) |>
  arrange(.data$matched_resolution, .data$match_type)

unmatched_points <- sum(is.na(toy_points$matched_h3))

cat("\n============================================================\n")
cat("REFINED POINT ASSIGNMENT QA\n")
cat("============================================================\n\n")

print(assignment_summary)

cat("Input points:", nrow(toy_points), "\n")
cat("Unmatched points:", unmatched_points, "\n")


# -----------------------------------------------------------------------------
# 13. Aggregate counts and validate totals
# -----------------------------------------------------------------------------

refined_counts <- toy_points |>
  st_drop_geometry() |>
  filter(!is.na(.data$matched_h3)) |>
  group_by(
    .data$matched_h3,
    .data$matched_resolution
  ) |>
  summarise(
    event_count = sum(.data$event_count),
    .groups = "drop"
  )

input_event_total <- sum(toy_points$event_count)
matched_event_total <- sum(refined_counts$event_count)

cat("\n============================================================\n")
cat("REFINED COUNT QA\n")
cat("============================================================\n\n")

cat("Input event total:", input_event_total, "\n")
cat("Matched event total:", matched_event_total, "\n")
cat("Difference:", matched_event_total - input_event_total, "\n")

stopifnot(
  unmatched_points == 0L,
  input_event_total == matched_event_total
)


# -----------------------------------------------------------------------------
# 14. Final QA summary
# -----------------------------------------------------------------------------

refinement_qa <- tibble(
  base_source_cells = length(source_h3),
  compacted_interior_cells = length(compacted_h3),
  boundary_cells = length(boundary_h3),
  refined_total_cells = length(refined_h3),
  base_resolution = base_resolution,
  boundary_resolution = boundary_resolution,
  initial_gap_area_m2 = round(initial_gap_area_m2, 2),
  remaining_gap_area_m2 = round(remaining_gap_area_m2, 2),
  gap_reduction_pct = round(gap_reduction_pct, 2),
  hierarchy_overlaps = nrow(refined_hierarchy_qa),
  roundtrip_missing = length(coverage_missing),
  roundtrip_additional = length(coverage_additional),
  unmatched_points = unmatched_points,
  input_event_total = input_event_total,
  matched_event_total = matched_event_total
)

cat("\n============================================================\n")
cat("FINAL BOUNDARY REFINEMENT QA\n")
cat("============================================================\n\n")

print(refinement_qa)


# -----------------------------------------------------------------------------
# 15. Plot
# -----------------------------------------------------------------------------

refinement_plot <- ggplot() +
  geom_sf(
    data = container_sf,
    fill = "grey98",
    colour = "black",
    linewidth = 0.9
  ) +
  geom_sf(
    data = refined_display_sf,
    aes(fill = factor(.data$resolution)),
    colour = "grey30",
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_sf(
    data = toy_points,
    size = 0.35,
    alpha = 0.45
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +
  labs(
    title = "Boundary-refined mixed-resolution H3 grid",
    subtitle = paste0(
      length(compacted_h3),
      " compacted interior cells + ",
      length(boundary_h3),
      " finer boundary cells"
    ),
    caption = paste0(
      "Container gap reduced by ",
      round(gap_reduction_pct, 2),
      "% | Unmatched points: ",
      unmatched_points
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  ) +
  coord_sf(
    xlim = container_bbox[c("xmin", "xmax")],
    ylim = container_bbox[c("ymin", "ymax")],
    expand = FALSE
  )

print(refinement_plot)


# -----------------------------------------------------------------------------
# 16. Completion
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("BOUNDARY REFINEMENT EXPERIMENT PASSED\n")
cat("============================================================\n\n")