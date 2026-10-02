# =============================================================================
# h3compactR
# 07_relaxed_compaction_irregular.R
# =============================================================================
#
# Purpose:
# Stress-test relaxed H3 compaction across irregular and discontinuous source
# regions using synthetic geometry.
#
# This workflow compares:
#
#   - exact compaction;
#   - relaxed compaction at several coverage thresholds.
#
# It validates:
#
#   - retained cell counts;
#   - resolution composition;
#   - authoritative source membership;
#   - geometric spillover;
#   - hierarchy validity;
#   - deterministic source-to-compact mapping;
#   - disconnected/irregular support behaviour; and
#   - cartographic consequences.
#
# This script uses synthetic data only and remains independent of PLM.
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
# 2. Parameters
# -----------------------------------------------------------------------------

source_resolution <- 8L

thresholds <- c(
  1.00,
  6 / 7,
  5 / 7,
  4 / 7
)


# -----------------------------------------------------------------------------
# 3. Create irregular and discontinuous polygons
# -----------------------------------------------------------------------------

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

stopifnot(
  nrow(polygons) == 3L,
  all(st_is_valid(polygons)),
  !any(st_is_empty(polygons))
)


# -----------------------------------------------------------------------------
# 4. Build authoritative Res 8 source support
# -----------------------------------------------------------------------------
#
# Build each polygon independently, then combine.
#
# This preserves discontinuity in the source-support test.

source_parts <- lapply(
  seq_len(nrow(polygons)),
  function(i) {

    h3 <- polygon_to_cells(
      polygons[i, ],
      res = source_resolution,
      simple = TRUE
    ) |>
      unlist(use.names = FALSE) |>
      unique()

    tibble(
      polygon_id = polygons$polygon_id[[i]],
      source_h3 = h3
    )
  }
)

source_tbl <- bind_rows(
  source_parts
) |>
  distinct(
    source_h3,
    .keep_all = TRUE
  )

source_h3 <- source_tbl$source_h3 |>
  validate_h3()

stopifnot(
  length(source_h3) ==
    n_distinct(source_h3),

  all(
    get_res(source_h3) ==
      source_resolution
  )
)


# -----------------------------------------------------------------------------
# 5. Exact compaction baseline
# -----------------------------------------------------------------------------

exact_h3 <- compact_h3(
  source_h3
)

exact_lookup <- build_h3_aggregation_lookup(
  source_h3 = source_h3,
  compacted_h3 = exact_h3
)


# -----------------------------------------------------------------------------
# 6. Run relaxed threshold sweep
# -----------------------------------------------------------------------------

results <- lapply(
  thresholds,
  function(threshold) {

    result <- compact_h3_relaxed(
      h3 = source_h3,
      min_coverage = threshold,
      min_resolution = 6L,
      simple = FALSE
    )

    list(
      threshold = threshold,
      result = result
    )
  }
)


# -----------------------------------------------------------------------------
# 7. Build threshold summary
# -----------------------------------------------------------------------------

summary_tbl <- bind_rows(
  lapply(
    results,
    function(x) {

      qa <- x$result$qa

      tibble(
        min_coverage = x$threshold,
        source_cells = qa$source_cells,
        compact_cells = qa$compact_cells,
        reduction_n = qa$reduction_n,
        reduction_pct = qa$reduction_pct,
        min_resolution = qa$min_resolution,
        max_resolution = qa$max_resolution,
        exact_cells = qa$exact_cells,
        relaxed_cells = qa$relaxed_cells,
        self_cells = qa$self_cells,
        spillover_cells = qa$spillover_cells,
        spillover_pct = qa$spillover_pct,
        hierarchy_overlaps = qa$hierarchy_overlaps,
        missing_source_memberships =
          qa$missing_source_memberships,
        duplicate_source_memberships =
          qa$duplicate_source_memberships,
        roundtrip_missing =
          qa$roundtrip_missing,
        roundtrip_additional =
          qa$roundtrip_additional,
        source_coverage_preserved =
          qa$source_coverage_preserved
      )
    }
  )
)

cat("\n")
cat("============================================================\n")
cat("RELAXED COMPACTION — IRREGULAR THRESHOLD SUMMARY\n")
cat("============================================================\n\n")

print(
  summary_tbl,
  n = Inf
)


# -----------------------------------------------------------------------------
# 8. Hard membership and hierarchy QA
# -----------------------------------------------------------------------------

stopifnot(
  all(
    summary_tbl$hierarchy_overlaps == 0L
  ),

  all(
    summary_tbl$missing_source_memberships == 0L
  ),

  all(
    summary_tbl$duplicate_source_memberships == 0L
  ),

  all(
    summary_tbl$roundtrip_missing == 0L
  ),

  all(
    summary_tbl$source_coverage_preserved
  )
)


# -----------------------------------------------------------------------------
# 9. Verify exact threshold reproduces exact compaction
# -----------------------------------------------------------------------------

exact_relaxed <- results[
  which.min(
    abs(
      thresholds - 1
    )
  )
][[1]]$result

stopifnot(
  setequal(
    exact_relaxed$h3,
    exact_h3
  ),

  setequal(
    exact_relaxed$lookup$source_h3,
    exact_lookup$source_h3
  )
)


# -----------------------------------------------------------------------------
# 10. Build resolution-composition table
# -----------------------------------------------------------------------------

resolution_summary <- bind_rows(
  lapply(
    results,
    function(x) {

      tibble(
        compact_h3 =
          x$result$h3,

        resolution =
          get_res(
            x$result$h3
          ),

        min_coverage =
          x$threshold
      )
    }
  )
) |>
  count(
    min_coverage,
    resolution,
    name = "cell_count"
  ) |>
  group_by(
    min_coverage
  ) |>
  mutate(
    cell_pct =
      round(
        100 *
          cell_count /
          sum(cell_count),
        2
      )
  ) |>
  ungroup()

cat("\n")
cat("============================================================\n")
cat("RELAXED COMPACTION — RESOLUTION COMPOSITION\n")
cat("============================================================\n\n")

print(
  resolution_summary,
  n = Inf
)


# -----------------------------------------------------------------------------
# 11. Build geometry for visual comparison
# -----------------------------------------------------------------------------

plot_geometry <- bind_rows(
  lapply(
    results,
    function(x) {

      h3_compaction_geometry(
        h3 = x$result$h3,
        output_crs = 4326
      ) |>
        mutate(
          threshold = paste0(
            "coverage ≥ ",
            round(
              x$threshold,
              3
            )
          )
        )
    }
  )
)


# -----------------------------------------------------------------------------
# 12. Build source support geometry
# -----------------------------------------------------------------------------

source_geometry <- h3jsr::cell_to_polygon(
  source_h3,
  simple = FALSE
) |>
  st_as_sf()


# -----------------------------------------------------------------------------
# 13. Cartographic QA plot
# -----------------------------------------------------------------------------
#
# The source Res 8 support is drawn faintly underneath the retained compact
# geometry.
#
# This allows direct inspection of:
#
#   - larger promoted parents;
#   - where spillover expands beyond authoritative source support;
#   - where fragmented Res 8 support remains;
#   - whether narrow/disconnected shapes are being over-generalised.

plot_relaxed_irregular <- ggplot() +

  geom_sf(
    data = source_geometry,
    fill = NA,
    colour = "grey80",
    linewidth = 0.08
  ) +

  geom_sf(
    data = plot_geometry,
    aes(
      fill = factor(
        .data$resolution
      )
    ),
    colour = "grey30",
    linewidth = 0.12,
    alpha = 0.65
  ) +

  facet_wrap(
    ~ threshold,
    ncol = 2
  ) +

  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +

  labs(
    title =
      "Relaxed H3 compaction across irregular and discontinuous support",

    subtitle =
      "Grey outlines = authoritative Res 8 support | coloured cells = retained compact geometry"
  ) +

  theme_minimal() +

  theme(
    panel.grid.major =
      element_blank(),

    legend.position =
      "right"
  ) +

  coord_sf()

print(
  plot_relaxed_irregular
)


# -----------------------------------------------------------------------------
# 14. Spillover-specific QA
# -----------------------------------------------------------------------------
#
# Confirm spillover increases monotonically or remains equal as the threshold
# is relaxed.
#
# A lower threshold must not somehow reduce geometric expansion relative to a
# stricter threshold.

ordered_summary <- summary_tbl |>
  arrange(
    desc(
      min_coverage
    )
  )

stopifnot(
  all(
    diff(
      ordered_summary$spillover_cells
    ) >= 0
  )
)


# -----------------------------------------------------------------------------
# 15. Determinism spot check
# -----------------------------------------------------------------------------

test_threshold <- 5 / 7

forward <- compact_h3_relaxed(
  h3 = source_h3,
  min_coverage = test_threshold,
  min_resolution = 6L,
  simple = FALSE
)

reverse <- compact_h3_relaxed(
  h3 = rev(source_h3),
  min_coverage = test_threshold,
  min_resolution = 6L,
  simple = FALSE
)

stopifnot(
  setequal(
    forward$h3,
    reverse$h3
  ),

  identical(
    forward$lookup |>
      arrange(
        source_h3
      ),

    reverse$lookup |>
      arrange(
        source_h3
      )
  )
)


# -----------------------------------------------------------------------------
# 16. Final console summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("RELAXED IRREGULAR COMPACTION QA\n")
cat("============================================================\n\n")

cat(
  "Authoritative source cells: ",
  length(source_h3),
  "\n"
)

cat(
  "Exact compact cells:        ",
  length(exact_h3),
  "\n"
)

cat(
  "Thresholds tested:          ",
  paste(
    round(
      thresholds,
      3
    ),
    collapse = ", "
  ),
  "\n"
)

cat(
  "All source memberships preserved: ",
  all(
    summary_tbl$missing_source_memberships == 0L
  ),
  "\n"
)

cat(
  "All hierarchy overlap checks passed: ",
  all(
    summary_tbl$hierarchy_overlaps == 0L
  ),
  "\n"
)

cat(
  "All source coverage preserved: ",
  all(
    summary_tbl$source_coverage_preserved
  ),
  "\n"
)


# -----------------------------------------------------------------------------
# 17. Final status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("RELAXED IRREGULAR COMPACTION TEST PASSED\n")
cat("============================================================\n\n")