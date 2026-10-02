# =============================================================================
# h3compactR
# 04_irregular_polygon_compaction.R
# =============================================================================
#
# Purpose:
# Validate H3 compaction across multiple irregular and discontinuous polygons.
#
# This development workflow tests:
#   1. irregular polygon source support;
#   2. discontinuous polygon handling;
#   3. native H3 compaction;
#   4. hierarchy validity;
#   5. exact round-trip equality;
#   6. spatial coverage preservation;
#   7. geometry generation; and
#   8. cartographic behaviour across multiple polygon shapes.
#
# This script uses synthetic geometry only.
#
# It is intended to support later applied validation against irregular PLM
# geometry, while keeping h3compactR independent of PLM-specific data,
# analytical measures and project paths.
#
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
# 2. Development parameters
# -----------------------------------------------------------------------------

source_resolution <- 8L
analysis_crs <- 7899


# -----------------------------------------------------------------------------
# 3. Create synthetic irregular polygons
# -----------------------------------------------------------------------------
#
# Three separate polygons are deliberately used:
#
#   A. large_irregular
#      Larger, asymmetric polygon with multiple angled edges.
#
#   B. medium_narrow
#      Narrower polygon intended to test elongated geometry.
#
#   C. small_compact
#      Smaller discontinuous polygon spatially separated from the others.
#
# Together these provide a basic discontinuous multi-polygon test case.

poly_1 <- st_polygon(
  list(
    matrix(
      c(
        144.80, -37.92,
        144.90, -37.96,
        145.00, -37.91,
        145.04, -37.82,
        144.97, -37.76,
        144.86, -37.78,
        144.80, -37.92
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
)

poly_2 <- st_polygon(
  list(
    matrix(
      c(
        145.18, -37.90,
        145.31, -37.88,
        145.34, -37.83,
        145.24, -37.80,
        145.17, -37.84,
        145.18, -37.90
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
)

poly_3 <- st_polygon(
  list(
    matrix(
      c(
        144.96, -37.66,
        145.03, -37.68,
        145.06, -37.62,
        145.01, -37.58,
        144.95, -37.61,
        144.96, -37.66
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
)

polygons <- st_sf(
  polygon_id = c(
    "large_irregular",
    "medium_narrow",
    "small_compact"
  ),
  geometry = st_sfc(
    poly_1,
    poly_2,
    poly_3,
    crs = 4326
  )
)


# -----------------------------------------------------------------------------
# 4. Validate input polygons
# -----------------------------------------------------------------------------

stopifnot(
  nrow(polygons) == 3L,
  all(st_is_valid(polygons)),
  !any(st_is_empty(polygons)),
  st_crs(polygons)$epsg == 4326
)

cat("\n")
cat("============================================================\n")
cat("INPUT POLYGON QA\n")
cat("============================================================\n\n")

cat("Polygon count: ", nrow(polygons), "\n")
cat("All valid:     ", all(st_is_valid(polygons)), "\n")
cat("Any empty:     ", any(st_is_empty(polygons)), "\n")
cat("CRS EPSG:      ", st_crs(polygons)$epsg, "\n")


# -----------------------------------------------------------------------------
# 5. Compact each polygon independently
# -----------------------------------------------------------------------------
#
# Each polygon is processed independently.
#
# This is deliberate:
# discontinuous polygons must not be accidentally dissolved or treated as one
# continuous source area during compaction.

results <- lapply(
  seq_len(nrow(polygons)),
  function(i) {

    # -------------------------------------------------------------------------
    # 5a. Build source H3 support
    # -------------------------------------------------------------------------

    source_h3 <- polygon_to_cells(
      polygons[i, ],
      res = source_resolution,
      simple = TRUE
    ) |>
      unlist(use.names = FALSE) |>
      unique() |>
      validate_h3()


    # -------------------------------------------------------------------------
    # 5b. Validate source H3 support
    # -------------------------------------------------------------------------

    source_res <- get_res(source_h3)

    stopifnot(
      length(source_h3) > 0L,
      length(source_h3) == length(unique(source_h3)),
      all(source_res == source_resolution)
    )


    # -------------------------------------------------------------------------
    # 5c. Native H3 compaction
    # -------------------------------------------------------------------------

    compacted_h3 <- compact_h3(source_h3)


    # -------------------------------------------------------------------------
    # 5d. Score compact representation
    # -------------------------------------------------------------------------

    score <- score_h3_compaction(
      source_h3 = source_h3,
      compacted_h3 = compacted_h3,
      container = polygons[i, ],
      analysis_crs = analysis_crs
    )


    # -------------------------------------------------------------------------
    # 5e. Build display geometry
    # -------------------------------------------------------------------------

    geometry <- h3_compaction_geometry(
      h3 = compacted_h3,
      container = polygons[i, ],
      clip = TRUE,
      output_crs = 4326,
      analysis_crs = analysis_crs
    ) |>
      mutate(
        polygon_id = polygons$polygon_id[[i]]
      )


    # -------------------------------------------------------------------------
    # 5f. Return polygon result
    # -------------------------------------------------------------------------

    list(
      polygon_id = polygons$polygon_id[[i]],
      source_h3 = source_h3,
      compacted_h3 = compacted_h3,
      score = score,
      geometry = geometry
    )
  }
)


# -----------------------------------------------------------------------------
# 6. Build combined QA table
# -----------------------------------------------------------------------------

qa <- bind_rows(
  lapply(
    results,
    function(x) {
      x$score |>
        mutate(
          polygon_id = x$polygon_id
        )
    }
  )
) |>
  select(
    polygon_id,
    source_cells,
    compacted_cells,
    reduction_n,
    reduction_pct,
    resolution_count,
    hierarchy_overlaps,
    roundtrip_equal,
    coverage_preserved,
    coverage_pct,
    gap_pct
  )


# -----------------------------------------------------------------------------
# 7. Print compaction QA
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("IRREGULAR POLYGON COMPACTION QA\n")
cat("============================================================\n\n")

print(qa)


# -----------------------------------------------------------------------------
# 8. Hard structural acceptance checks
# -----------------------------------------------------------------------------
#
# These are non-negotiable structural checks.
#
# Every polygon must:
#   - produce a compact representation no larger than the source;
#   - contain no hierarchy overlaps;
#   - reproduce the source H3 support exactly on round trip; and
#   - preserve spatial coverage.

stopifnot(
  nrow(qa) == 3L,
  all(qa$source_cells > 0L),
  all(qa$compacted_cells > 0L),
  all(qa$compacted_cells <= qa$source_cells),
  all(qa$hierarchy_overlaps == 0L),
  all(qa$roundtrip_equal),
  all(qa$coverage_preserved)
)


# -----------------------------------------------------------------------------
# 9. Explicit round-trip equality QA
# -----------------------------------------------------------------------------
#
# score_h3_compaction() already reports round-trip equality.
#
# This section performs an additional direct set comparison so the development
# workflow independently confirms that uncompaction reproduces every source H3
# cell and introduces no additional source-resolution cells.

roundtrip_qa <- bind_rows(
  lapply(
    results,
    function(x) {

      roundtrip_h3 <- uncompact_h3(
        x$compacted_h3,
        resolution = source_resolution
      )

      tibble(
        polygon_id = x$polygon_id,
        source_cells = length(x$source_h3),
        roundtrip_cells = length(roundtrip_h3),
        missing_cells = length(
          setdiff(
            x$source_h3,
            roundtrip_h3
          )
        ),
        additional_cells = length(
          setdiff(
            roundtrip_h3,
            x$source_h3
          )
        ),
        exact_set_equal = setequal(
          x$source_h3,
          roundtrip_h3
        )
      )
    }
  )
)

cat("\n")
cat("============================================================\n")
cat("ROUND-TRIP QA\n")
cat("============================================================\n\n")

print(roundtrip_qa)

stopifnot(
  all(roundtrip_qa$missing_cells == 0L),
  all(roundtrip_qa$additional_cells == 0L),
  all(roundtrip_qa$exact_set_equal)
)


# -----------------------------------------------------------------------------
# 10. Build combined display geometry
# -----------------------------------------------------------------------------

map_sf <- bind_rows(
  lapply(
    results,
    `[[`,
    "geometry"
  )
)


# -----------------------------------------------------------------------------
# 11. Geometry QA
# -----------------------------------------------------------------------------

geometry_qa <- tibble(
  feature_count = nrow(map_sf),
  invalid_geometry = sum(!st_is_valid(map_sf)),
  empty_geometry = sum(st_is_empty(map_sf)),
  crs_epsg = st_crs(map_sf)$epsg
)

cat("\n")
cat("============================================================\n")
cat("DISPLAY GEOMETRY QA\n")
cat("============================================================\n\n")

print(geometry_qa)

stopifnot(
  geometry_qa$feature_count > 0L,
  geometry_qa$invalid_geometry == 0L,
  geometry_qa$empty_geometry == 0L,
  geometry_qa$crs_epsg == 4326
)


# -----------------------------------------------------------------------------
# 12. Resolution summary
# -----------------------------------------------------------------------------

resolution_qa <- map_sf |>
  st_drop_geometry() |>
  count(
    polygon_id,
    resolution,
    name = "cell_count"
  ) |>
  group_by(polygon_id) |>
  mutate(
    cell_pct = round(
      100 * cell_count / sum(cell_count),
      2
    )
  ) |>
  ungroup()

cat("\n")
cat("============================================================\n")
cat("COMPACT RESOLUTION SUMMARY\n")
cat("============================================================\n\n")

print(resolution_qa)


# -----------------------------------------------------------------------------
# 13. Cartographic QA plot
# -----------------------------------------------------------------------------
#
# Important:
#
# coord_sf() cannot be used with free facet scales.
#
# All three panels therefore use a common spatial scale. This is intentional:
# it preserves relative geographic size and helps identify whether the
# compaction behaves differently across large, narrow and isolated polygons.
#
# The plot assesses:
#   - mixed-resolution structure;
#   - boundary behaviour;
#   - obvious gaps;
#   - obvious overlaps;
#   - behaviour across discontinuous source geometry.

plot_irregular <- ggplot() +
  geom_sf(
    data = polygons,
    fill = "grey98",
    colour = "black",
    linewidth = 0.8
  ) +
  geom_sf(
    data = map_sf,
    aes(
      fill = factor(.data$resolution)
    ),
    colour = "grey30",
    linewidth = 0.22,
    alpha = 0.7
  ) +
  facet_wrap(
    ~ polygon_id,
    ncol = 3
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +
  labs(
    title = "Compaction across irregular and discontinuous polygons",
    subtitle = paste0(
      "Source resolution ",
      source_resolution,
      " | exact round trip for all polygons | ",
      "hierarchy overlaps = 0"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  ) +
  coord_sf()

print(plot_irregular)


# -----------------------------------------------------------------------------
# 14. Final workflow summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FINAL IRREGULAR POLYGON QA\n")
cat("============================================================\n\n")

cat(
  "Polygons tested:             ",
  nrow(qa),
  "\n"
)

cat(
  "Total source cells:          ",
  sum(qa$source_cells),
  "\n"
)

cat(
  "Total compacted cells:       ",
  sum(qa$compacted_cells),
  "\n"
)

cat(
  "Total hierarchy overlaps:    ",
  sum(qa$hierarchy_overlaps),
  "\n"
)

cat(
  "Total round-trip missing:    ",
  sum(roundtrip_qa$missing_cells),
  "\n"
)

cat(
  "Total round-trip additional: ",
  sum(roundtrip_qa$additional_cells),
  "\n"
)

cat(
  "All round trips exact:       ",
  all(roundtrip_qa$exact_set_equal),
  "\n"
)

cat(
  "All coverage preserved:      ",
  all(qa$coverage_preserved),
  "\n"
)


# -----------------------------------------------------------------------------
# 15. Final workflow status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("IRREGULAR POLYGON WORKFLOW PASSED\n")
cat("============================================================\n\n")