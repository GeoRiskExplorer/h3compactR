# =============================================================================
# h3compactR
# 05_lineage_aggregation_integrity.R
# =============================================================================
#
# Purpose:
# Validate the public aggregation lookup API and prove that authoritative
# source H3 attributes can be aggregated to compact cells without:
#
#   - missing source cells;
#   - duplicate source assignments;
#   - invalid parent/self relationships;
#   - resolution mismatches;
#   - round-trip loss;
#   - aggregation leakage; or
#   - non-deterministic assignment.
#
# This workflow uses synthetic data only.
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
  library(h3jsr)
})


# -----------------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------------

source_resolution <- 8L


# -----------------------------------------------------------------------------
# 3. Create toy source polygon
# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------
# 4. Build source H3 support
# -----------------------------------------------------------------------------
#
# This synthetic Res 8 support acts as the authoritative source support for
# this development test.

source_h3 <- polygon_to_cells(
  container,
  res = source_resolution,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique() |>
  validate_h3()

stopifnot(
  length(source_h3) > 0L,
  length(source_h3) == length(unique(source_h3)),
  all(get_res(source_h3) == source_resolution)
)


# -----------------------------------------------------------------------------
# 5. Build compact H3 representation
# -----------------------------------------------------------------------------

compacted_h3 <- compact_h3(
  source_h3
)

stopifnot(
  length(compacted_h3) <= length(source_h3),
  nrow(check_h3_hierarchy(compacted_h3)) == 0L
)


# -----------------------------------------------------------------------------
# 6. Build aggregation lookup using public package API
# -----------------------------------------------------------------------------

lookup <- build_h3_aggregation_lookup(
  source_h3 = source_h3,
  compacted_h3 = compacted_h3
)


# -----------------------------------------------------------------------------
# 7. Aggregation lookup structural QA
# -----------------------------------------------------------------------------

lookup_qa <- tibble(
  source_cells = length(source_h3),
  compact_cells = length(compacted_h3),
  lookup_rows = nrow(lookup),

  distinct_source_h3 = n_distinct(
    lookup$source_h3
  ),

  missing_source_h3 = length(
    setdiff(
      source_h3,
      lookup$source_h3
    )
  ),

  unexpected_source_h3 = length(
    setdiff(
      lookup$source_h3,
      source_h3
    )
  ),

  duplicate_source_assignments =
    nrow(lookup) -
    n_distinct(lookup$source_h3),

  unexpected_compact_h3 = length(
    setdiff(
      unique(lookup$compact_h3),
      compacted_h3
    )
  ),

  resolution_mismatch = sum(
    get_res(lookup$compact_h3) !=
      lookup$compact_resolution
  )
)

cat("\n")
cat("============================================================\n")
cat("AGGREGATION LOOKUP STRUCTURAL QA\n")
cat("============================================================\n\n")

print(lookup_qa)

stopifnot(
  lookup_qa$lookup_rows ==
    lookup_qa$source_cells,

  lookup_qa$distinct_source_h3 ==
    lookup_qa$source_cells,

  lookup_qa$missing_source_h3 == 0L,
  lookup_qa$unexpected_source_h3 == 0L,
  lookup_qa$duplicate_source_assignments == 0L,
  lookup_qa$unexpected_compact_h3 == 0L,
  lookup_qa$resolution_mismatch == 0L
)


# -----------------------------------------------------------------------------
# 8. Explicit ancestry QA
# -----------------------------------------------------------------------------
#
# Independently confirm that every lookup row represents either:
#
#   - the source cell itself; or
#   - the correct H3 ancestor at the recorded compact resolution.

ancestry_qa <- lookup |>
  rowwise() |>
  mutate(
    expected_compact_h3 = if (
      source_h3 == compact_h3
    ) {
      source_h3
    } else {
      get_parent(
        source_h3,
        res = compact_resolution,
        simple = TRUE
      ) |>
        unlist(use.names = FALSE)
    },

    ancestry_valid =
      expected_compact_h3 == compact_h3
  ) |>
  ungroup()

invalid_ancestry <- sum(
  !ancestry_qa$ancestry_valid
)

cat("\n")
cat("Invalid ancestry assignments: ",
    invalid_ancestry,
    "\n")

stopifnot(
  invalid_ancestry == 0L
)


# -----------------------------------------------------------------------------
# 9. Create deterministic source attributes
# -----------------------------------------------------------------------------
#
# These values deliberately use simple deterministic sequences so any
# misassignment can be traced and reproduced.
#
# All fields below are additive and therefore suitable for conservation QA.

source_data <- tibble(
  source_h3 = sort(source_h3)
) |>
  mutate(
    source_sequence = row_number(),

    event_count =
      source_sequence %% 7L,

    exposure_count =
      source_sequence * 10L,

    expected_count =
      source_sequence / 100
  )


# -----------------------------------------------------------------------------
# 10. Attach aggregation lookup
# -----------------------------------------------------------------------------

aggregation_data <- source_data |>
  left_join(
    lookup,
    by = "source_h3"
  )

stopifnot(
  nrow(aggregation_data) ==
    nrow(source_data),

  !anyNA(
    aggregation_data$compact_h3
  ),

  !anyNA(
    aggregation_data$compact_resolution
  )
)


# -----------------------------------------------------------------------------
# 11. Aggregate source attributes to compact cells
# -----------------------------------------------------------------------------

compact_data <- aggregation_data |>
  group_by(
    compact_h3,
    compact_resolution
  ) |>
  summarise(
    source_cell_count = n(),

    source_sequence_sum =
      sum(source_sequence),

    event_count =
      sum(event_count),

    exposure_count =
      sum(exposure_count),

    expected_count =
      sum(expected_count),

    .groups = "drop"
  )


# -----------------------------------------------------------------------------
# 12. Global aggregation conservation QA
# -----------------------------------------------------------------------------
#
# Every additive source total must equal the corresponding compact total.

aggregation_qa <- tibble(
  metric = c(
    "source_sequence",
    "event_count",
    "exposure_count",
    "expected_count"
  ),

  source_total = c(
    sum(source_data$source_sequence),
    sum(source_data$event_count),
    sum(source_data$exposure_count),
    sum(source_data$expected_count)
  ),

  compact_total = c(
    sum(compact_data$source_sequence_sum),
    sum(compact_data$event_count),
    sum(compact_data$exposure_count),
    sum(compact_data$expected_count)
  )
) |>
  mutate(
    difference =
      compact_total -
      source_total
  )

cat("\n")
cat("============================================================\n")
cat("AGGREGATION CONSERVATION QA\n")
cat("============================================================\n\n")

print(aggregation_qa)

stopifnot(
  all(
    abs(
      aggregation_qa$difference
    ) < 1e-10
  )
)


# -----------------------------------------------------------------------------
# 13. Per-compact-cell aggregation QA
# -----------------------------------------------------------------------------
#
# Recalculate compact-cell values independently from lookup membership.
#
# This catches incorrect assignment that could theoretically be hidden if only
# global totals were checked.

compact_recheck <- aggregation_data |>
  group_by(
    compact_h3,
    compact_resolution
  ) |>
  summarise(
    check_source_cell_count = n(),

    check_sequence_sum =
      sum(source_sequence),

    check_event_count =
      sum(event_count),

    check_exposure_count =
      sum(exposure_count),

    check_expected_count =
      sum(expected_count),

    .groups = "drop"
  )

cell_qa <- compact_data |>
  left_join(
    compact_recheck,
    by = c(
      "compact_h3",
      "compact_resolution"
    )
  ) |>
  mutate(
    source_count_equal =
      source_cell_count ==
      check_source_cell_count,

    sequence_equal =
      source_sequence_sum ==
      check_sequence_sum,

    event_equal =
      event_count ==
      check_event_count,

    exposure_equal =
      exposure_count ==
      check_exposure_count,

    expected_equal =
      abs(
        expected_count -
          check_expected_count
      ) < 1e-10
  )

stopifnot(
  all(cell_qa$source_count_equal),
  all(cell_qa$sequence_equal),
  all(cell_qa$event_equal),
  all(cell_qa$exposure_equal),
  all(cell_qa$expected_equal)
)


# -----------------------------------------------------------------------------
# 14. Round-trip support QA
# -----------------------------------------------------------------------------

roundtrip_h3 <- uncompact_h3(
  compacted_h3,
  resolution = source_resolution
)

roundtrip_qa <- tibble(
  source_cells =
    length(source_h3),

  roundtrip_cells =
    length(roundtrip_h3),

  missing_cells = length(
    setdiff(
      source_h3,
      roundtrip_h3
    )
  ),

  additional_cells = length(
    setdiff(
      roundtrip_h3,
      source_h3
    )
  ),

  exact_set_equal = setequal(
    source_h3,
    roundtrip_h3
  )
)

cat("\n")
cat("============================================================\n")
cat("ROUND-TRIP SUPPORT QA\n")
cat("============================================================\n\n")

print(roundtrip_qa)

stopifnot(
  roundtrip_qa$missing_cells == 0L,
  roundtrip_qa$additional_cells == 0L,
  roundtrip_qa$exact_set_equal
)


# -----------------------------------------------------------------------------
# 15. Determinism QA
# -----------------------------------------------------------------------------
#
# Rebuild the lookup using reversed source order.
#
# After sorting, both lookup tables must be identical.

lookup_reverse <- build_h3_aggregation_lookup(
  source_h3 = rev(source_h3),
  compacted_h3 = compacted_h3
)

deterministic <- identical(
  lookup |>
    arrange(source_h3),
  lookup_reverse |>
    arrange(source_h3)
)

cat("\n")
cat("Deterministic aggregation lookup: ",
    deterministic,
    "\n")

stopifnot(
  deterministic
)


# -----------------------------------------------------------------------------
# 16. Lookup composition summary
# -----------------------------------------------------------------------------

lookup_summary <- lookup |>
  count(
    compact_resolution,
    match_type,
    name = "source_cell_count"
  ) |>
  arrange(
    compact_resolution,
    match_type
  )

cat("\n")
cat("============================================================\n")
cat("AGGREGATION LOOKUP COMPOSITION\n")
cat("============================================================\n\n")

print(lookup_summary)


# -----------------------------------------------------------------------------
# 17. Final QA summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("FINAL AGGREGATION MAPPING + INTEGRITY QA\n")
cat("============================================================\n\n")

cat(
  "Source H3 cells:               ",
  length(source_h3),
  "\n"
)

cat(
  "Compact H3 cells:              ",
  length(compacted_h3),
  "\n"
)

cat(
  "Aggregation lookup rows:       ",
  nrow(lookup),
  "\n"
)

cat(
  "Missing source assignments:    ",
  lookup_qa$missing_source_h3,
  "\n"
)

cat(
  "Duplicate source assignments:  ",
  lookup_qa$duplicate_source_assignments,
  "\n"
)

cat(
  "Invalid ancestry assignments:  ",
  invalid_ancestry,
  "\n"
)

cat(
  "Resolution mismatches:         ",
  lookup_qa$resolution_mismatch,
  "\n"
)

cat(
  "Round-trip missing:            ",
  roundtrip_qa$missing_cells,
  "\n"
)

cat(
  "Round-trip additional:         ",
  roundtrip_qa$additional_cells,
  "\n"
)

cat(
  "Exact round trip:              ",
  roundtrip_qa$exact_set_equal,
  "\n"
)

cat(
  "Deterministic lookup:          ",
  deterministic,
  "\n"
)

cat(
  "Aggregation differences:       ",
  paste(
    aggregation_qa$difference,
    collapse = ", "
  ),
  "\n"
)


# -----------------------------------------------------------------------------
# 18. Final workflow status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("AGGREGATION MAPPING + INTEGRITY PASSED\n")
cat("============================================================\n\n")