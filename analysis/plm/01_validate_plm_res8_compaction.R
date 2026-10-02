# =============================================================================
# h3compactR
# 01_validate_plm_res8_compaction.R
# =============================================================================
#
# Purpose:
# Apply h3compactR to the authoritative PLM Stage 2 H3 Resolution 8 baseline.
#
# This applied validation proves that:
#
#   1. the authoritative baseline contains the expected 147,894 Res 8 cells;
#   2. all source H3 indexes are valid and unique;
#   3. native H3 compaction preserves the complete source support;
#   4. every source Res 8 cell maps to exactly one compact parent/self cell;
#   5. aggregation lookup assignments contain no gaps, duplicates or
#      resolution mismatches;
#   6. the compact representation round-trips exactly to Res 8;
#   7. authoritative additive values aggregate without loss or leakage;
#   8. the resulting compact geometry is valid; and
#   9. the mapping is suitable for downstream PLM analytical aggregation.
#
# IMPORTANT:
#
# This script belongs to the applied development/validation layer only.
#
# PLM-specific paths, table names and analytical fields must NOT be embedded
# inside h3compactR package functions.
#
# h3compactR provides:
#   - H3 compaction;
#   - aggregation lookup;
#   - hierarchy/round-trip QA;
#   - geometry.
#
# PLM_Analytics remains authoritative for analytical calculations and
# recalculation of derived measures such as SMR.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load package and applied-validation dependencies
# -----------------------------------------------------------------------------

devtools::load_all()

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(sf)
  library(ggplot2)
  library(h3jsr)
})


# -----------------------------------------------------------------------------
# 2. PLM applied-validation parameters
# -----------------------------------------------------------------------------

plm_root <- "F:/PLM_Prod/PLM_Analytics"

duckdb_path <- file.path(
  plm_root,
  "duckdb",
  "plm_prod.duckdb"
)

baseline_schema <- "analysis"
baseline_table <- "h3_res8_baseline"

h3_field <- "hex_id"

expected_source_cells <- 147894L
expected_source_resolution <- 8L


# -----------------------------------------------------------------------------
# 3. Authoritative additive fields
# -----------------------------------------------------------------------------
#
# These values can legitimately be summed through the aggregation lookup.
#
# Derived measures such as SMR, confidence limits, classifications and
# seasonal SMRs are deliberately excluded.

additive_fields <- c(
  "event_count",
  "exposure_annual_raw",
  "exposure_annual_model",
  "expected",
  "events_summer",
  "events_autumn",
  "events_winter",
  "events_spring"
)


# -----------------------------------------------------------------------------
# 4. Validate PLM project and DuckDB
# -----------------------------------------------------------------------------

if (!dir.exists(plm_root)) {
  stop(
    "PLM Analytics root does not exist: ",
    plm_root
  )
}

if (!file.exists(duckdb_path)) {
  stop(
    "PLM DuckDB does not exist: ",
    duckdb_path
  )
}

cat("\n")
cat("============================================================\n")
cat("PLM APPLIED VALIDATION — PROJECT\n")
cat("============================================================\n\n")

cat("PLM root:          ", plm_root, "\n")
cat("Authoritative DB:  ", duckdb_path, "\n")
cat("Baseline table:    ", baseline_schema, ".", baseline_table, "\n", sep = "")
cat("Expected Res 8:    ", format(expected_source_cells, big.mark = ","), "\n")


# -----------------------------------------------------------------------------
# 5. Open authoritative DuckDB read-only
# -----------------------------------------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = duckdb_path,
  read_only = TRUE
)

if (!DBI::dbIsValid(con)) {
  stop(
    "Could not establish a valid read-only DuckDB connection."
  )
}

cat("\nDuckDB connection: VALID\n")


# -----------------------------------------------------------------------------
# 6. Inspect authoritative baseline schema
# -----------------------------------------------------------------------------

baseline_fields <- DBI::dbGetQuery(
  con,
  "
  SELECT
    column_name,
    data_type
  FROM information_schema.columns
  WHERE table_schema = 'analysis'
    AND table_name = 'h3_res8_baseline'
  ORDER BY ordinal_position
  "
)

required_fields <- c(
  h3_field,
  additive_fields
)

missing_required_fields <- setdiff(
  required_fields,
  baseline_fields$column_name
)

if (length(missing_required_fields) > 0L) {
  stop(
    "Required baseline field(s) missing: ",
    paste(missing_required_fields, collapse = ", ")
  )
}

cat("\n")
cat("============================================================\n")
cat("BASELINE INPUT CONTRACT\n")
cat("============================================================\n\n")

cat("H3 field: ", h3_field, "\n")
cat(
  "Additive fields: ",
  paste(additive_fields, collapse = ", "),
  "\n"
)


# -----------------------------------------------------------------------------
# 7. Read required authoritative baseline fields
# -----------------------------------------------------------------------------
#
# Only the H3 identifier and selected additive components are read.
#
# No derived PLM analytical measures are recalculated here.

baseline_ref <- paste0(
  DBI::dbQuoteIdentifier(con, baseline_schema),
  ".",
  DBI::dbQuoteIdentifier(con, baseline_table)
)

field_sql <- paste(
  DBI::dbQuoteIdentifier(
    con,
    c(h3_field, additive_fields)
  ),
  collapse = ", "
)

baseline <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT ",
    field_sql,
    " FROM ",
    baseline_ref
  )
) |>
  tibble::as_tibble()

names(baseline)[
  names(baseline) == h3_field
] <- "source_h3"

baseline <- baseline |>
  mutate(
    source_h3 = as.character(.data$source_h3)
  )


# -----------------------------------------------------------------------------
# 8. Authoritative Res 8 source-support QA
# -----------------------------------------------------------------------------

source_qa <- tibble(
  baseline_rows = nrow(baseline),

  distinct_source_h3 =
    n_distinct(baseline$source_h3),

  missing_source_h3 =
    sum(is.na(baseline$source_h3)),

  duplicate_source_h3 =
    nrow(baseline) -
    n_distinct(baseline$source_h3),

  invalid_source_h3 =
    sum(
      !h3jsr::is_valid(
        baseline$source_h3
      )
    ),

  resolution_mismatch =
    sum(
      h3jsr::get_res(
        baseline$source_h3
      ) != expected_source_resolution
    )
)

cat("\n")
cat("============================================================\n")
cat("AUTHORITATIVE RES 8 SUPPORT QA\n")
cat("============================================================\n\n")

print(source_qa)

stopifnot(
  source_qa$baseline_rows ==
    expected_source_cells,

  source_qa$distinct_source_h3 ==
    expected_source_cells,

  source_qa$missing_source_h3 == 0L,

  source_qa$duplicate_source_h3 == 0L,

  source_qa$invalid_source_h3 == 0L,

  source_qa$resolution_mismatch == 0L
)

source_h3 <- validate_h3(
  baseline$source_h3
)


# -----------------------------------------------------------------------------
# 9. Additive-field completeness QA
# -----------------------------------------------------------------------------
#
# Missing values are not silently converted to zero.
#
# If an authoritative additive field contains missing values, aggregation
# behaviour must be explicitly reviewed before proceeding.

additive_missing_qa <- tibble(
  field = additive_fields,
  missing_n = vapply(
    baseline[additive_fields],
    function(x) sum(is.na(x)),
    integer(1)
  )
)

cat("\n")
cat("============================================================\n")
cat("ADDITIVE FIELD COMPLETENESS QA\n")
cat("============================================================\n\n")

print(additive_missing_qa)

if (any(additive_missing_qa$missing_n > 0L)) {
  stop(
    "One or more additive fields contain missing values. ",
    "Review before aggregation."
  )
}


# -----------------------------------------------------------------------------
# 10. Build native compact H3 representation
# -----------------------------------------------------------------------------

cat("\nBuilding native compact representation...\n")

compacted_h3 <- compact_h3(
  source_h3
)

compact_resolution_summary <- summarise_h3_resolution(
  compacted_h3
)

cat("\n")
cat("============================================================\n")
cat("COMPACT H3 SUMMARY\n")
cat("============================================================\n\n")

cat(
  "Source cells:  ",
  format(length(source_h3), big.mark = ","),
  "\n"
)

cat(
  "Compact cells: ",
  format(length(compacted_h3), big.mark = ","),
  "\n"
)

cat(
  "Reduction:     ",
  round(
    100 *
      (length(source_h3) - length(compacted_h3)) /
      length(source_h3),
    2
  ),
  "%\n\n"
)

print(compact_resolution_summary)


# -----------------------------------------------------------------------------
# 11. Structural compaction QA
# -----------------------------------------------------------------------------

compaction_qa <- qa_h3_compaction(
  source_h3 = source_h3,
  compacted_h3 = compacted_h3,
  target_resolution = expected_source_resolution
)

cat("\n")
cat("============================================================\n")
cat("STRUCTURAL COMPACTION QA\n")
cat("============================================================\n\n")

print(compaction_qa)

stopifnot(
  compaction_qa$hierarchy_overlaps == 0L,
  compaction_qa$roundtrip_missing == 0L,
  compaction_qa$roundtrip_additional == 0L,
  compaction_qa$roundtrip_equal
)


# -----------------------------------------------------------------------------
# 12. Build authoritative Res 8 → compact aggregation lookup
# -----------------------------------------------------------------------------
#
# This may be the slowest section with the current internal matching
# implementation because every source cell is validated against the compact
# hierarchy.
#
# The applied run therefore also provides a useful scalability test.

cat("\nBuilding Res 8 → compact aggregation lookup...\n")

lookup_start <- Sys.time()

lookup <- build_h3_aggregation_lookup(
  source_h3 = source_h3,
  compacted_h3 = compacted_h3
)

lookup_elapsed <- difftime(
  Sys.time(),
  lookup_start,
  units = "secs"
)

cat(
  "Aggregation lookup runtime: ",
  round(as.numeric(lookup_elapsed), 1),
  " seconds\n"
)


# -----------------------------------------------------------------------------
# 13. Aggregation lookup contract QA
# -----------------------------------------------------------------------------

lookup_qa <- tibble(
  source_cells =
    length(source_h3),

  lookup_rows =
    nrow(lookup),

  distinct_source_h3 =
    n_distinct(lookup$source_h3),

  missing_source_h3 =
    length(
      setdiff(
        source_h3,
        lookup$source_h3
      )
    ),

  unexpected_source_h3 =
    length(
      setdiff(
        lookup$source_h3,
        source_h3
      )
    ),

  duplicate_source_assignments =
    nrow(lookup) -
    n_distinct(lookup$source_h3),

  unexpected_compact_h3 =
    length(
      setdiff(
        unique(lookup$compact_h3),
        compacted_h3
      )
    ),

  resolution_mismatch =
    sum(
      h3jsr::get_res(
        lookup$compact_h3
      ) !=
        lookup$compact_resolution
    )
)

cat("\n")
cat("============================================================\n")
cat("RES 8 → COMPACT LOOKUP QA\n")
cat("============================================================\n\n")

print(lookup_qa)

stopifnot(
  lookup_qa$source_cells ==
    expected_source_cells,

  lookup_qa$lookup_rows ==
    expected_source_cells,

  lookup_qa$distinct_source_h3 ==
    expected_source_cells,

  lookup_qa$missing_source_h3 == 0L,

  lookup_qa$unexpected_source_h3 == 0L,

  lookup_qa$duplicate_source_assignments == 0L,

  lookup_qa$unexpected_compact_h3 == 0L,

  lookup_qa$resolution_mismatch == 0L
)


# -----------------------------------------------------------------------------
# 14. Explicit H3 ancestry QA
# -----------------------------------------------------------------------------

cat("\nChecking compact parent/self ancestry...\n")

ancestry_valid <- mapply(
  FUN = function(
      source_cell,
      compact_cell,
      compact_resolution
  ) {

    if (source_cell == compact_cell) {
      return(TRUE)
    }

    expected_parent <- h3jsr::get_parent(
      source_cell,
      res = compact_resolution,
      simple = TRUE
    ) |>
      unlist(use.names = FALSE)

    expected_parent == compact_cell
  },

  source_cell =
    lookup$source_h3,

  compact_cell =
    lookup$compact_h3,

  compact_resolution =
    lookup$compact_resolution,

  USE.NAMES = FALSE
)

invalid_ancestry <- sum(
  !ancestry_valid
)

cat(
  "Invalid ancestry assignments: ",
  invalid_ancestry,
  "\n"
)

stopifnot(
  invalid_ancestry == 0L
)


# -----------------------------------------------------------------------------
# 15. Exact round-trip support QA
# -----------------------------------------------------------------------------

roundtrip_h3 <- uncompact_h3(
  compacted_h3,
  resolution = expected_source_resolution
)

roundtrip_qa <- tibble(
  source_cells =
    length(source_h3),

  roundtrip_cells =
    length(roundtrip_h3),

  missing_cells =
    length(
      setdiff(
        source_h3,
        roundtrip_h3
      )
    ),

  additional_cells =
    length(
      setdiff(
        roundtrip_h3,
        source_h3
      )
    ),

  exact_set_equal =
    setequal(
      source_h3,
      roundtrip_h3
    )
)

cat("\n")
cat("============================================================\n")
cat("EXACT ROUND-TRIP SUPPORT QA\n")
cat("============================================================\n\n")

print(roundtrip_qa)

stopifnot(
  roundtrip_qa$source_cells ==
    expected_source_cells,

  roundtrip_qa$roundtrip_cells ==
    expected_source_cells,

  roundtrip_qa$missing_cells == 0L,

  roundtrip_qa$additional_cells == 0L,

  roundtrip_qa$exact_set_equal
)


# -----------------------------------------------------------------------------
# 16. Join authoritative baseline to aggregation lookup
# -----------------------------------------------------------------------------

baseline_mapped <- baseline |>
  left_join(
    lookup,
    by = "source_h3"
  )

mapping_join_qa <- tibble(
  source_rows =
    nrow(baseline),

  mapped_rows =
    nrow(baseline_mapped),

  missing_compact_assignment =
    sum(
      is.na(
        baseline_mapped$compact_h3
      )
    )
)

cat("\n")
cat("============================================================\n")
cat("BASELINE → LOOKUP JOIN QA\n")
cat("============================================================\n\n")

print(mapping_join_qa)

stopifnot(
  mapping_join_qa$source_rows ==
    expected_source_cells,

  mapping_join_qa$mapped_rows ==
    expected_source_cells,

  mapping_join_qa$missing_compact_assignment == 0L
)


# -----------------------------------------------------------------------------
# 17. Aggregate authoritative additive fields
# -----------------------------------------------------------------------------
#
# This aggregation is performed here only to validate conservation.
#
# h3compactR itself does not know these PLM field names or analytical rules.

compact_aggregated <- baseline_mapped |>
  group_by(
    .data$compact_h3,
    .data$compact_resolution
  ) |>
  summarise(
    source_cell_count = n(),

    across(
      all_of(additive_fields),
      ~ sum(.x),
      .names = "{.col}"
    ),

    .groups = "drop"
  )


# -----------------------------------------------------------------------------
# 18. Authoritative aggregation conservation QA
# -----------------------------------------------------------------------------

source_totals <- vapply(
  baseline[additive_fields],
  sum,
  numeric(1)
)

compact_totals <- vapply(
  compact_aggregated[additive_fields],
  sum,
  numeric(1)
)

aggregation_qa <- tibble(
  field = additive_fields,
  source_total = as.numeric(source_totals),
  compact_total = as.numeric(compact_totals)
) |>
  mutate(
    difference =
      .data$compact_total -
      .data$source_total,

    tolerance =
      pmax(
        1e-10,
        abs(.data$source_total) * 1e-10
      ),

    preserved =
      abs(.data$difference) <=
      .data$tolerance
  )

cat("\n")
cat("============================================================\n")
cat("AUTHORITATIVE AGGREGATION CONSERVATION QA\n")
cat("============================================================\n\n")

print(
  aggregation_qa,
  n = Inf
)

stopifnot(
  all(aggregation_qa$preserved)
)


# -----------------------------------------------------------------------------
# 19. Aggregation membership QA
# -----------------------------------------------------------------------------

membership_qa <- tibble(
  compact_cells =
    nrow(compact_aggregated),

  expected_compact_cells =
    length(compacted_h3),

  source_memberships =
    sum(
      compact_aggregated$source_cell_count
    ),

  expected_source_memberships =
    expected_source_cells,

  membership_difference =
    source_memberships -
    expected_source_memberships
)

cat("\n")
cat("============================================================\n")
cat("AGGREGATION MEMBERSHIP QA\n")
cat("============================================================\n\n")

print(membership_qa)

stopifnot(
  membership_qa$compact_cells ==
    membership_qa$expected_compact_cells,

  membership_qa$source_memberships ==
    expected_source_cells,

  membership_qa$membership_difference == 0L
)


# -----------------------------------------------------------------------------
# 20. Determinism QA
# -----------------------------------------------------------------------------
#
# Rebuild the lookup in reverse source order and verify that the sorted
# source-to-compact relationship is unchanged.

cat("\nChecking deterministic aggregation mapping...\n")

lookup_reverse <- build_h3_aggregation_lookup(
  source_h3 = rev(source_h3),
  compacted_h3 = compacted_h3
)

deterministic <- identical(
  lookup |>
    arrange(.data$source_h3),

  lookup_reverse |>
    arrange(.data$source_h3)
)

cat(
  "Deterministic aggregation lookup: ",
  deterministic,
  "\n"
)

stopifnot(
  deterministic
)


# -----------------------------------------------------------------------------
# 21. Aggregation lookup composition
# -----------------------------------------------------------------------------

lookup_composition <- lookup |>
  count(
    .data$compact_resolution,
    .data$match_type,
    name = "source_cell_count"
  ) |>
  arrange(
    .data$compact_resolution,
    .data$match_type
  )

cat("\n")
cat("============================================================\n")
cat("AGGREGATION LOOKUP COMPOSITION\n")
cat("============================================================\n\n")

print(lookup_composition)


# -----------------------------------------------------------------------------
# 22. Build compact geometry
# -----------------------------------------------------------------------------

cat("\nBuilding compact geometry...\n")

compact_sf <- h3_compaction_geometry(
  h3 = compacted_h3,
  output_crs = 4326
)


# -----------------------------------------------------------------------------
# 23. Compact geometry QA
# -----------------------------------------------------------------------------

geometry_qa <- tibble(
  compact_h3_cells =
    length(compacted_h3),

  geometry_rows =
    nrow(compact_sf),

  invalid_geometry =
    sum(
      !st_is_valid(
        compact_sf
      )
    ),

  empty_geometry =
    sum(
      st_is_empty(
        compact_sf
      )
    ),

  crs_epsg =
    st_crs(
      compact_sf
    )$epsg
)

cat("\n")
cat("============================================================\n")
cat("COMPACT GEOMETRY QA\n")
cat("============================================================\n\n")

print(geometry_qa)

stopifnot(
  geometry_qa$geometry_rows ==
    geometry_qa$compact_h3_cells,

  geometry_qa$invalid_geometry == 0L,

  geometry_qa$empty_geometry == 0L,

  geometry_qa$crs_epsg == 4326
)


# -----------------------------------------------------------------------------
# 24. Cartographic QA plot
# -----------------------------------------------------------------------------
#
# Resolution only.
#
# No PLM analytical values are symbolised.

compact_plot <- ggplot(
  compact_sf
) +
  geom_sf(
    aes(
      fill = factor(
        .data$resolution
      )
    ),
    colour = "grey30",
    linewidth = 0.05
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +
  labs(
    title = "PLM Stage 2 — native compact H3 support",
    subtitle = paste0(
      format(
        length(source_h3),
        big.mark = ","
      ),
      " authoritative Res 8 cells → ",
      format(
        length(compacted_h3),
        big.mark = ","
      ),
      " compact cells"
    )
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
  compact_plot
)


# -----------------------------------------------------------------------------
# 25. Final applied validation summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PLM STAGE 2 — H3COMPACTR APPLIED VALIDATION\n")
cat("============================================================\n\n")

cat(
  "Authoritative source cells:       ",
  format(
    length(source_h3),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Compact cells:                    ",
  format(
    length(compacted_h3),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Aggregation lookup rows:          ",
  format(
    nrow(lookup),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Missing source assignments:       ",
  lookup_qa$missing_source_h3,
  "\n"
)

cat(
  "Duplicate source assignments:     ",
  lookup_qa$duplicate_source_assignments,
  "\n"
)

cat(
  "Invalid ancestry assignments:     ",
  invalid_ancestry,
  "\n"
)

cat(
  "Resolution mismatches:            ",
  lookup_qa$resolution_mismatch,
  "\n"
)

cat(
  "Round-trip missing cells:         ",
  roundtrip_qa$missing_cells,
  "\n"
)

cat(
  "Round-trip additional cells:      ",
  roundtrip_qa$additional_cells,
  "\n"
)

cat(
  "Exact round-trip equality:        ",
  roundtrip_qa$exact_set_equal,
  "\n"
)

cat(
  "Deterministic aggregation map:    ",
  deterministic,
  "\n"
)

cat(
  "Aggregation membership difference:",
  membership_qa$membership_difference,
  "\n"
)

cat(
  "Additive fields preserved:        ",
  all(
    aggregation_qa$preserved
  ),
  "\n"
)

cat(
  "Invalid compact geometries:       ",
  geometry_qa$invalid_geometry,
  "\n"
)


# -----------------------------------------------------------------------------
# 26. Close authoritative read-only connection
# -----------------------------------------------------------------------------

if (DBI::dbIsValid(con)) {

  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
}

cat("\nDuckDB connection closed.\n")


# -----------------------------------------------------------------------------
# 27. Final applied-validation status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PLM RES 8 → COMPACT APPLIED VALIDATION PASSED\n")
cat("============================================================\n\n")