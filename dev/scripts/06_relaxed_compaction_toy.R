# =============================================================================
# h3compactR
# 06_relaxed_compaction_toy.R
# =============================================================================
#
# Purpose:
# Prove the core relaxed-compaction behaviour using a known 6-of-7 H3 example.
#
# Expected:
#
# Exact compaction:
#   6 Res 8 cells remain.
#
# Relaxed compaction at 6/7:
#   1 Res 7 parent is retained.
#
# Authoritative membership:
#   remains exactly 6 cells.
#
# Geometric round trip:
#   contains 7 Res 8 cells.
#
# Spillover:
#   exactly 1 Res 8 cell.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load package
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
# 2. Build known parent and children
# -----------------------------------------------------------------------------

parent_h3 <- "87be63563ffffff"

children_h3 <- h3jsr::get_children(
  parent_h3,
  res = 8,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  sort()

stopifnot(
  length(children_h3) == 7L
)


# -----------------------------------------------------------------------------
# 3. Create 6-of-7 authoritative source support
# -----------------------------------------------------------------------------

source_h3 <- children_h3[
  1:6
]

missing_child <- children_h3[
  7
]


# -----------------------------------------------------------------------------
# 4. Exact compaction
# -----------------------------------------------------------------------------

exact_h3 <- compact_h3(
  source_h3
)

stopifnot(
  length(exact_h3) == 6L,
  !parent_h3 %in% exact_h3
)


# -----------------------------------------------------------------------------
# 5. Relaxed compaction below 6/7 threshold
# -----------------------------------------------------------------------------

relaxed <- compact_h3_relaxed(
  h3 = source_h3,
  min_coverage = 6 / 7,
  simple = FALSE
)


# -----------------------------------------------------------------------------
# 6. Core promotion QA
# -----------------------------------------------------------------------------

stopifnot(
  length(relaxed$h3) == 1L,
  identical(relaxed$h3, parent_h3)
)


# -----------------------------------------------------------------------------
# 7. Authoritative membership QA
# -----------------------------------------------------------------------------

lookup <- relaxed$lookup

stopifnot(
  nrow(lookup) == 6L,
  dplyr::n_distinct(lookup$source_h3) == 6L,
  setequal(
    lookup$source_h3,
    source_h3
  ),
  all(
    lookup$compact_h3 ==
      parent_h3
  ),
  !missing_child %in%
    lookup$source_h3
)


# -----------------------------------------------------------------------------
# 8. Spillover QA
# -----------------------------------------------------------------------------

metadata <- relaxed$metadata

stopifnot(
  nrow(metadata) == 1L,
  metadata$source_cell_count == 6L,
  metadata$possible_source_cell_count == 7L,
  abs(
    metadata$coverage_ratio -
      (6 / 7)
  ) < 1e-12,
  metadata$spillover_cell_count == 1L,
  metadata$compaction_mode == "relaxed"
)


# -----------------------------------------------------------------------------
# 9. Geometric round-trip QA
# -----------------------------------------------------------------------------

roundtrip_h3 <- uncompact_h3(
  relaxed$h3,
  resolution = 8L
)

stopifnot(
  length(roundtrip_h3) == 7L,
  length(
    setdiff(
      source_h3,
      roundtrip_h3
    )
  ) == 0L,
  setequal(
    setdiff(
      roundtrip_h3,
      source_h3
    ),
    missing_child
  )
)


# -----------------------------------------------------------------------------
# 10. Aggregation integrity QA
# -----------------------------------------------------------------------------
#
# Give each authoritative source cell a known value.
# The missing spillover child receives NOTHING.

source_data <- tibble(
  source_h3 = source_h3,
  count = seq_along(source_h3)
)

aggregated <- source_data |>
  left_join(
    lookup,
    by = "source_h3"
  ) |>
  group_by(
    compact_h3,
    compact_resolution
  ) |>
  summarise(
    count = sum(count),
    .groups = "drop"
  )

stopifnot(
  sum(source_data$count) ==
    sum(aggregated$count)
)

# -----------------------------------------------------------------------------
# 11. Build visual QA geometry
# -----------------------------------------------------------------------------
#
# The plot deliberately distinguishes:
#
#   - the six authoritative Res 8 source cells;
#   - the one missing Res 8 child represented as geometric spillover;
#   - the retained relaxed Res 7 parent.
#
# This makes the analytical/geometric distinction visible.

source_geometry <- h3jsr::cell_to_polygon(
  source_h3,
  simple = FALSE
) |>
  st_as_sf() |>
  mutate(
    support_type = "Authoritative Res 8"
  )

spillover_geometry <- h3jsr::cell_to_polygon(
  missing_child,
  simple = FALSE
) |>
  st_as_sf() |>
  mutate(
    support_type = "Spillover Res 8"
  )

parent_geometry <- h3jsr::cell_to_polygon(
  parent_h3,
  simple = FALSE
) |>
  st_as_sf()


# -----------------------------------------------------------------------------
# 12. Plot relaxed compaction behaviour
# -----------------------------------------------------------------------------

plot_relaxed <- ggplot() +

  # Full retained Res 7 parent
  geom_sf(
    data = parent_geometry,
    fill = NA,
    colour = "black",
    linewidth = 1.2
  ) +

  # Authoritative Res 8 membership
  geom_sf(
    data = source_geometry,
    aes(fill = .data$support_type),
    colour = "grey30",
    linewidth = 0.5,
    alpha = 0.75
  ) +

  # Geometric spillover
  geom_sf(
    data = spillover_geometry,
    aes(fill = .data$support_type),
    colour = "grey30",
    linewidth = 0.7,
    alpha = 0.75
  ) +

  scale_fill_brewer(
    palette = "Set2",
    name = NULL
  ) +

  theme_minimal() +

  theme(
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  ) +

  labs(
    title = "Relaxed H3 compaction — 6 of 7 source cells",
    subtitle = paste0(
      "Six authoritative Res 8 cells represented by one Res 7 parent | ",
      "spillover = 1 Res 8 cell"
    ),
    caption = paste0(
      "Black outline = retained Res 7 parent. ",
      "Spillover affects geometry only; it is excluded from aggregation membership."
    )
  )

print(plot_relaxed)


# -----------------------------------------------------------------------------
# 11. Below-threshold QA
# -----------------------------------------------------------------------------

below_threshold <- compact_h3_relaxed(
  h3 = source_h3,
  min_coverage = 0.90,
  simple = FALSE
)

stopifnot(
  length(below_threshold$h3) == 6L,
  !parent_h3 %in%
    below_threshold$h3,
  below_threshold$qa$spillover_cells == 0L
)


# -----------------------------------------------------------------------------
# 12. Console summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("RELAXED COMPACTION — 6 OF 7 TOY QA\n")
cat("============================================================\n\n")

cat("Authoritative source cells: ", length(source_h3), "\n")
cat("Exact compact cells:        ", length(exact_h3), "\n")
cat("Relaxed compact cells:      ", length(relaxed$h3), "\n")
cat("Authoritative memberships:  ", nrow(lookup), "\n")
cat("Geometric round-trip cells: ", length(roundtrip_h3), "\n")
cat("Spillover cells:            ", relaxed$qa$spillover_cells, "\n")
cat("Missing source cells:       ", relaxed$qa$roundtrip_missing, "\n")
cat(
  "Aggregation difference:    ",
  sum(aggregated$count) -
    sum(source_data$count),
  "\n"
)

cat("\n")
print(relaxed$metadata)

cat("\n")
print(relaxed$qa)


# -----------------------------------------------------------------------------
# 13. Final status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("RELAXED COMPACTION TOY TEST PASSED\n")
cat("============================================================\n\n")