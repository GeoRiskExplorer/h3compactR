# =============================================================================
# h3compactR
# Compare native and boundary-refined compaction strategies
# =============================================================================

devtools::load_all()

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(h3jsr)
})

base_resolution <- 7L
boundary_resolution <- 8L
analysis_crs <- 7899

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

source_h3 <- polygon_to_cells(
  container_sf,
  res = base_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique() |>
  validate_h3()

native_h3 <- compact_h3(source_h3)

refined_result <- refine_h3_boundary(
  source_h3 = source_h3,
  compacted_h3 = native_h3,
  container = container_sf,
  boundary_resolution = boundary_resolution,
  analysis_crs = analysis_crs
)

native_score <- score_h3_compaction(
  source_h3 = source_h3,
  compacted_h3 = native_h3,
  container = container_sf,
  analysis_crs = analysis_crs
) |>
  mutate(strategy = "native")

refined_score <- score_h3_compaction(
  source_h3 = source_h3,
  compacted_h3 = refined_result$h3,
  container = container_sf,
  analysis_crs = analysis_crs
) |>
  mutate(strategy = "boundary_refined")

strategy_comparison <- bind_rows(
  native_score,
  refined_score
) |>
  select(
    strategy,
    source_cells,
    compacted_cells,
    reduction_n,
    reduction_pct,
    resolution_count,
    compact_min_resolution,
    compact_max_resolution,
    hierarchy_overlaps,
    coverage_preserved,
    coverage_extended,
    roundtrip_equal,
    coverage_pct,
    gap_pct,
    gap_area_m2
  )

cat("\n============================================================\n")
cat("COMPACTION STRATEGY COMPARISON\n")
cat("============================================================\n\n")

print(strategy_comparison)

stopifnot(
  all(strategy_comparison$hierarchy_overlaps == 0L),
  native_score$roundtrip_equal,
  native_score$coverage_preserved,
  !native_score$coverage_extended,
  refined_score$coverage_preserved,
  refined_score$coverage_extended,
  refined_score$coverage_pct >= native_score$coverage_pct,
  refined_score$gap_area_m2 <= native_score$gap_area_m2
)

native_sf <- h3_compaction_geometry(
  h3 = native_h3,
  container = container_sf,
  clip = TRUE,
  output_crs = 4326,
  analysis_crs = analysis_crs
) |>
  mutate(strategy = "Native compaction")

refined_sf <- refined_result$display_geometry |>
  mutate(strategy = "Boundary refined")

comparison_sf <- bind_rows(
  native_sf,
  refined_sf
)

comparison_plot <- ggplot() +
  geom_sf(
    data = container_sf,
    fill = "grey98",
    colour = "black",
    linewidth = 0.7
  ) +
  geom_sf(
    data = comparison_sf,
    aes(fill = factor(.data$resolution)),
    colour = "grey30",
    linewidth = 0.22,
    alpha = 0.7
  ) +
  facet_wrap(~strategy) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "H3 compaction strategy comparison",
    subtitle = paste0(
      "Native reduction: ",
      native_score$reduction_pct,
      "% | Refined coverage: ",
      refined_score$coverage_pct,
      "%"
    )
  )

print(comparison_plot)

cat("\n============================================================\n")
cat("STRATEGY COMPARISON PASSED\n")
cat("============================================================\n\n")
