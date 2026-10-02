# =============================================================================
# h3compactR
# Core compaction workflow
#
# File:
#   dev/scripts/01_core_compaction_workflow.R
#
# Purpose:
#   Demonstrate the current package workflow using a toy polygon, structural
#   H3 compaction, round-trip QA, mixed-resolution assignment and plotting.
#
# This script is intended to become the basis of the getting-started vignette.
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
point_count <- 500L
random_seed <- 20260711L

container_bbox <- c(
  xmin = 144.80,
  ymin = -37.95,
  xmax = 145.15,
  ymax = -37.70
)


# -----------------------------------------------------------------------------
# 3. Create container polygon
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
  inherits(container_sf, "sf"),
  st_crs(container_sf)$epsg == 4326,
  all(st_is_valid(container_sf))
)


# -----------------------------------------------------------------------------
# 4. Create source H3 grid
# -----------------------------------------------------------------------------

source_h3 <- h3jsr::polygon_to_cells(
  container_sf,
  res = base_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

source_h3 <- validate_h3(source_h3)

source_resolution_summary <- summarise_h3_resolution(source_h3)

source_sf_full <- h3jsr::cell_to_polygon(
  source_h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  mutate(
    resolution = h3jsr::get_res(.data$h3),
    cell_status = "source"
  )

cat("\n============================================================\n")
cat("SOURCE GRID\n")
cat("============================================================\n\n")

cat("Source cells:", length(source_h3), "\n")
print(source_resolution_summary)


# -----------------------------------------------------------------------------
# 5. Compact source H3 grid
# -----------------------------------------------------------------------------

compaction_result <- compact_h3(
  source_h3,
  simple = FALSE
)

compacted_h3 <- compaction_result$compacted_h3

compacted_sf_full <- h3jsr::cell_to_polygon(
  compacted_h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  mutate(
    resolution = h3jsr::get_res(.data$h3),
    cell_status = "compacted"
  )

cat("\n============================================================\n")
cat("COMPACTED GRID\n")
cat("============================================================\n\n")

cat("Compacted cells:", length(compacted_h3), "\n")
print(compaction_result$resolution_summary)


# -----------------------------------------------------------------------------
# 6. Structural compaction QA
# -----------------------------------------------------------------------------

compaction_qa <- qa_h3_compaction(
  source_h3 = source_h3,
  compacted_h3 = compacted_h3,
  target_resolution = base_resolution
)

hierarchy_qa <- check_h3_hierarchy(compacted_h3)

cat("\n============================================================\n")
cat("STRUCTURAL COMPACTION QA\n")
cat("============================================================\n\n")

print(compaction_qa)

cat(
  "Parent-descendant overlaps:",
  nrow(hierarchy_qa),
  "\n"
)

stopifnot(
  compaction_qa$compacted_cells < compaction_qa$source_cells,
  compaction_qa$roundtrip_equal,
  compaction_qa$roundtrip_missing == 0L,
  compaction_qa$roundtrip_additional == 0L,
  nrow(hierarchy_qa) == 0L
)


# -----------------------------------------------------------------------------
# 7. Create clipped display geometry
# -----------------------------------------------------------------------------

source_sf_display <- suppressWarnings(
  st_intersection(
    source_sf_full,
    select(container_sf, container_id)
  )
) |>
  filter(!st_is_empty(.data$geometry))

compacted_sf_display <- suppressWarnings(
  st_intersection(
    compacted_sf_full,
    select(container_sf, container_id)
  )
) |>
  filter(!st_is_empty(.data$geometry))

display_outside_geometry <- suppressWarnings(
  st_difference(
    st_union(compacted_sf_display),
    st_union(container_sf)
  )
)

display_outside_area_m2 <- display_outside_geometry |>
  st_transform(7899) |>
  st_area() |>
  sum() |>
  as.numeric()

cat("\n============================================================\n")
cat("DISPLAY GEOMETRY QA\n")
cat("============================================================\n\n")

cat(
  "Clipped geometry outside container m2:",
  round(display_outside_area_m2, 8),
  "\n"
)

stopifnot(
  display_outside_area_m2 < 0.01
)


# -----------------------------------------------------------------------------
# 8. Create points inside source H3 coverage
# -----------------------------------------------------------------------------

source_coverage_sf <- source_sf_full |>
  summarise()

set.seed(random_seed)

toy_points <- st_sample(
  source_coverage_sf,
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
  res = base_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE)

cat("\n============================================================\n")
cat("POINT SOURCE QA\n")
cat("============================================================\n\n")

cat("Points:", nrow(toy_points), "\n")
cat(
  "Points outside source H3 set:",
  sum(!toy_points$source_h3 %in% source_h3),
  "\n"
)

stopifnot(
  nrow(toy_points) == point_count,
  all(toy_points$source_h3 %in% source_h3)
)


# -----------------------------------------------------------------------------
# 9. Match points to the mixed-resolution grid
# -----------------------------------------------------------------------------

point_lookup <- match_h3_to_compacted(
  source_h3 = toy_points$source_h3,
  compacted_h3 = compacted_h3
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
    matched_resolution,
    match_type,
    name = "point_count"
  ) |>
  arrange(matched_resolution, match_type)

cat("\n============================================================\n")
cat("POINT ASSIGNMENT QA\n")
cat("============================================================\n\n")

print(assignment_summary)

cat(
  "Unmatched points:",
  sum(is.na(toy_points$matched_h3)),
  "\n"
)

stopifnot(
  !anyNA(toy_points$matched_h3),
  all(toy_points$matched_h3 %in% compacted_h3)
)


# -----------------------------------------------------------------------------
# 10. Aggregate point counts to retained H3 cells
# -----------------------------------------------------------------------------

compacted_counts <- toy_points |>
  st_drop_geometry() |>
  group_by(
    matched_h3,
    matched_resolution
  ) |>
  summarise(
    event_count = sum(.data$event_count),
    .groups = "drop"
  )

input_event_total <- sum(toy_points$event_count)
output_event_total <- sum(compacted_counts$event_count)

cat("\n============================================================\n")
cat("COUNT AGGREGATION QA\n")
cat("============================================================\n\n")

cat("Input event total:", input_event_total, "\n")
cat("Output event total:", output_event_total, "\n")
cat("Difference:", output_event_total - input_event_total, "\n")

stopifnot(
  input_event_total == output_event_total
)


# -----------------------------------------------------------------------------
# 11. Join aggregated counts to compacted geometry
# -----------------------------------------------------------------------------

compacted_map_sf <- compacted_sf_display |>
  left_join(
    compacted_counts,
    by = c(
      "h3" = "matched_h3",
      "resolution" = "matched_resolution"
    )
  ) |>
  mutate(
    event_count = coalesce(.data$event_count, 0L)
  )

stopifnot(
  sum(compacted_map_sf$event_count) == input_event_total
)


# -----------------------------------------------------------------------------
# 12. Final workflow QA
# -----------------------------------------------------------------------------

workflow_qa <- tibble(
  source_cells = length(source_h3),
  compacted_cells = length(compacted_h3),
  reduction_n = source_cells - compacted_cells,
  reduction_pct = round(
    100 * reduction_n / source_cells,
    2
  ),
  source_resolution = base_resolution,
  compact_min_resolution = min(h3jsr::get_res(compacted_h3)),
  compact_max_resolution = max(h3jsr::get_res(compacted_h3)),
  hierarchy_overlaps = nrow(hierarchy_qa),
  roundtrip_equal = compaction_qa$roundtrip_equal,
  unmatched_points = sum(is.na(toy_points$matched_h3)),
  input_event_total = input_event_total,
  output_event_total = output_event_total
)

cat("\n============================================================\n")
cat("FINAL WORKFLOW QA\n")
cat("============================================================\n\n")

print(workflow_qa)


# -----------------------------------------------------------------------------
# 13. Plot mixed-resolution result
# -----------------------------------------------------------------------------

workflow_plot <- ggplot() +
  geom_sf(
    data = container_sf,
    fill = "grey98",
    colour = "black",
    linewidth = 0.9
  ) +
  geom_sf(
    data = source_sf_display,
    fill = NA,
    colour = "grey80",
    linewidth = 0.15
  ) +
  geom_sf(
    data = compacted_map_sf,
    aes(fill = factor(.data$resolution)),
    colour = "grey30",
    linewidth = 0.3,
    alpha = 0.6
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
    title = "Core mixed-resolution H3 workflow",
    subtitle = paste0(
      workflow_qa$source_cells,
      " source cells reduced to ",
      workflow_qa$compacted_cells,
      " retained cells"
    ),
    caption = paste0(
      "Reduction: ",
      workflow_qa$reduction_pct,
      "% | ",
      workflow_qa$input_event_total,
      " points preserved"
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

print(workflow_plot)


# -----------------------------------------------------------------------------
# 14. Completion
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("CORE COMPACTION WORKFLOW PASSED\n")
cat("============================================================\n\n")