# =============================================================================
# h3compactR
# END-TO-END QA — POLYGON ATTRIBUTES -> H3 -> COMPACTION -> ATTRIBUTES -> MAP
#
# Purpose:
#   Demonstrate and validate the complete V1 analytical workflow:
#
#     polygon features
#         -> H3 coverage
#         -> polygon assignment + retained attributes
#         -> additive source measures
#         -> H3 compaction
#         -> source-to-compact lookup
#         -> additive aggregation + categorical provenance collapse
#         -> compact H3 geometry for mapping
#
# Notes:
#   - Source H3 cells remain the authoritative analytical support.
#   - Polygon geometry is used for assignment, not carried through the tables.
#   - Compaction is global and may therefore produce compact cells whose source
#     descendants belong to more than one polygon feature.
#   - Such categorical provenance is represented by sorted unique values such
#     as "100; 200".
#   - H3 geometry is constructed only when mapping is required.
# =============================================================================

library(h3compactR)
library(sf)


# =============================================================================
# 1. Prepare generic polygon attributes
# =============================================================================

polygons <- toy_membership_polygons

polygons$feature_name <- paste0(
  "Feature ",
  polygons$feature_id
)

polygons$group_name <- c(
  "Group A",
  "Group A",
  "Group B",
  "Group B",
  "Group C"
)

polygons$class_name <- c(
  "Class 1",
  "Class 1",
  "Class 2",
  "Class 2",
  "Class 3"
)


# =============================================================================
# 2. Build authoritative source H3 support
# =============================================================================

polygon_union <- sf::st_union(
  sf::st_geometry(polygons)
)

polygon_union <- sf::st_sf(
  geometry = polygon_union
)

coverage_time <- system.time({

  source_h3 <- h3_cover_polygon(
    x = polygon_union,
    resolution = 9L,
    boundary = "intersects"
  )

})


# =============================================================================
# 3. Assign source H3 cells to polygon features
# =============================================================================

assignment_time <- system.time({

  assigned <- h3_assign_polygon(
    x = source_h3,
    polygons = polygons,
    id = "feature_id",
    keep = c(
      "feature_name",
      "group_name",
      "class_name"
    ),
    outside = "unassigned"
  )

})

resolved <- !assigned$assignment_method %in% c(
  "ambiguous",
  "unassigned"
)

if (!all(resolved)) {
  stop(
    "The source coverage contains unresolved polygon assignments."
  )
}


# =============================================================================
# 4. Add deterministic synthetic additive measures
# =============================================================================
#
# These fields exist only to test additive aggregation through the complete
# workflow. They are deliberately simple and reproducible.
# =============================================================================

source_data <- assigned

source_data$events <- rep(
  c(0, 1, 0, 2, 1),
  length.out = nrow(source_data)
)

source_data$measure_a <- seq_len(
  nrow(source_data)
)

source_data$measure_b <- seq_len(
  nrow(source_data)
) * 10


# =============================================================================
# 5. Compact the authoritative H3 support
# =============================================================================

compaction_time <- system.time({

  compacted_h3 <- compact_h3(
    x = source_data$h3,
    min_resolution = 7L
  )

})


# =============================================================================
# 6. Build source-to-compact ownership lookup
# =============================================================================

lookup_time <- system.time({

  lookup <- h3_compaction_lookup(
    source = source_data$h3,
    compacted = compacted_h3
  )

})


# =============================================================================
# 7. Aggregate additive values and categorical provenance
# =============================================================================

aggregation_time <- system.time({

  compact_data <- aggregate_h3(
    data = source_data,
    lookup = lookup,
    sum = c(
      "events",
      "measure_a",
      "measure_b"
    ),
    collapse = c(
      "polygon_id",
      "feature_name",
      "group_name",
      "class_name"
    )
  )

})


# =============================================================================
# 8. Derive QA display field from native provenance counts
# =============================================================================
#
# aggregate_h3() returns `<field>_n` for every collapsed provenance field.
# polygon_id_n is therefore the authoritative count of unique source polygons
# represented by each compact H3 owner.
# =============================================================================

compact_data$feature_membership <- ifelse(
  compact_data$polygon_id_n == 1L,
  "Single feature",
  "Multiple features"
)


# =============================================================================
# 9. Console summary
# =============================================================================

cat(
  "\n",
  paste(rep("=", 76), collapse = ""),
  "\nH3COMPACTR — END-TO-END WORKFLOW QA\n",
  paste(rep("=", 76), collapse = ""),
  "\n\n",
  sep = ""
)

cat(
  "Source H3 cells:          ",
  format(length(source_h3), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Compacted H3 cells:       ",
  format(length(compacted_h3), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Cell reduction:           ",
  format(
    length(source_h3) - length(compacted_h3),
    big.mark = ","
  ),
  " (",
  round(
    100 * (
      1 -
        length(compacted_h3) /
          length(source_h3)
    ),
    2
  ),
  "%)\n",
  sep = ""
)

cat(
  "Single-feature compact:   ",
  format(
    sum(compact_data$polygon_id_n == 1L),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Multi-feature compact:    ",
  format(
    sum(compact_data$polygon_id_n > 1L),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

cat(
  "Coverage time:            ",
  round(coverage_time[["elapsed"]], 3),
  " s\n",
  sep = ""
)

cat(
  "Assignment time:          ",
  round(assignment_time[["elapsed"]], 3),
  " s\n",
  sep = ""
)

cat(
  "Compaction time:          ",
  round(compaction_time[["elapsed"]], 3),
  " s\n",
  sep = ""
)

cat(
  "Lookup time:              ",
  round(lookup_time[["elapsed"]], 3),
  " s\n",
  sep = ""
)

cat(
  "Aggregation time:         ",
  round(aggregation_time[["elapsed"]], 3),
  " s\n",
  sep = ""
)

cat(
  "Total measured workflow:  ",
  round(
    coverage_time[["elapsed"]] +
      assignment_time[["elapsed"]] +
      compaction_time[["elapsed"]] +
      lookup_time[["elapsed"]] +
      aggregation_time[["elapsed"]],
    3
  ),
  " s\n\n",
  sep = ""
)


# =============================================================================
# 10. Inspect source assignment table
# =============================================================================

cat(
  paste(rep("-", 76), collapse = ""),
  "\nSOURCE H3 ATTRIBUTE TABLE\n",
  paste(rep("-", 76), collapse = ""),
  "\n",
  sep = ""
)

print(
  head(
    source_data[
      ,
      c(
        "h3",
        "polygon_id",
        "feature_name",
        "group_name",
        "class_name",
        "assignment_method",
        "events",
        "measure_a",
        "measure_b"
      )
    ],
    12
  )
)


# =============================================================================
# 11. Inspect compact attribute table
# =============================================================================

cat(
  "\n",
  paste(rep("-", 76), collapse = ""),
  "\nCOMPACT H3 ATTRIBUTE TABLE\n",
  paste(rep("-", 76), collapse = ""),
  "\n",
  sep = ""
)

print(
  head(
    compact_data[
      ,
      c(
        "compact_h3",
        "compact_resolution",
        "source_cell_n",
        "events",
        "polygon_id",
        "feature_name",
        "group_name",
        "class_name",
        "polygon_id_n"
      )
    ],
    15
  )
)


# =============================================================================
# 12. Inspect genuinely multi-feature compact cells
# =============================================================================

multi_feature <- compact_data[
  compact_data$polygon_id_n > 1L,
  ,
  drop = FALSE
]

cat(
  "\n",
  paste(rep("-", 76), collapse = ""),
  "\nMULTI-FEATURE COMPACT CELLS\n",
  paste(rep("-", 76), collapse = ""),
  "\n",
  sep = ""
)

if (nrow(multi_feature) == 0L) {

  cat(
    "No multi-feature compact cells were produced by this fixture.\n"
  )

} else {

  print(
    multi_feature[
      ,
      c(
        "compact_h3",
        "compact_resolution",
        "source_cell_n",
        "polygon_id",
        "polygon_id_n",
        "feature_name",
        "feature_name_n",
        "group_name",
        "group_name_n",
        "class_name",
        "class_name_n"
      )
    ]
  )
}


# =============================================================================
# 13. Core analytical QA
# =============================================================================

qa <- data.frame(
  check = c(
    "source_assignment_one_row_per_h3",
    "source_assignment_non_spatial",
    "source_assignment_complete",
    "lookup_one_row_per_source_h3",
    "lookup_source_support_exact",
    "compact_row_count_exact",
    "source_cell_count_conserved",
    "events_conserved",
    "measure_a_conserved",
    "measure_b_conserved",
    "compact_h3_support_exact",
    "collapsed_feature_values_present"
  ),
  pass = c(
    nrow(source_data) == length(unique(source_h3)),
    !inherits(source_data, "sf"),
    all(resolved),
    nrow(lookup) == length(source_h3),
    setequal(lookup$source_h3, source_h3),
    nrow(compact_data) == length(compacted_h3),
    sum(compact_data$source_cell_n) == nrow(source_data),
    sum(compact_data$events) == sum(source_data$events),
    sum(compact_data$measure_a) == sum(source_data$measure_a),
    sum(compact_data$measure_b) == sum(source_data$measure_b),
    setequal(compact_data$compact_h3, compacted_h3),
    all(!is.na(compact_data$polygon_id))
  )
)

cat(
  "\n",
  paste(rep("-", 76), collapse = ""),
  "\nCORE QA\n",
  paste(rep("-", 76), collapse = ""),
  "\n",
  sep = ""
)

print(qa)

if (!all(qa$pass)) {
  stop("End-to-end analytical QA failed.")
}


# =============================================================================
# 14. Validate collapsed provenance against source descendants
# =============================================================================
#
# Reconstruct the expected polygon IDs for every compact owner directly from
# the authoritative source rows and the ownership lookup.
# =============================================================================

source_lookup_position <- match(
  source_data$h3,
  lookup$source_h3
)

source_owner <- lookup$compact_h3[
  source_lookup_position
]

expected_polygon_id <- vapply(
  compact_data$compact_h3,
  function(owner) {

    values <- source_data$polygon_id[
      source_owner == owner
    ]

    paste(
      sort(
        unique(
          values
        )
      ),
      collapse = "; "
    )
  },
  character(1)
)

if (!identical(
  compact_data$polygon_id,
  unname(expected_polygon_id)
)) {
  stop(
    "Collapsed polygon provenance does not match authoritative source descendants."
  )
}

cat(
  "\nPASS — collapsed polygon provenance matches source descendants.\n"
)


# =============================================================================
# 15. Explicit global and polygon-level value reconciliation
# =============================================================================
#
# IMPORTANT:
#   Polygon-level reconciliation is performed from authoritative source rows
#   carried through the source-to-compact lookup. Compact categorical strings
#   such as "500; 600" are provenance only and are NEVER exploded to allocate
#   compact additive totals back to multiple polygons.
#
#   Every authoritative source H3 must:
#     1. occur once in source_data,
#     2. occur once in lookup,
#     3. map to exactly one compact owner,
#     4. contribute every additive value exactly once.
# =============================================================================

reconciled_source <- source_data

reconciled_position <- match(
  reconciled_source$h3,
  lookup$source_h3
)

reconciled_source$compact_h3 <- lookup$compact_h3[
  reconciled_position
]

if (anyNA(reconciled_source$compact_h3)) {
  stop("Polygon reconciliation failed: one or more source H3 cells have no compact owner.")
}

source_polygon_accounting <- aggregate(
  cbind(
    source_cells = rep(1L, nrow(source_data)),
    events = source_data$events,
    measure_a = source_data$measure_a,
    measure_b = source_data$measure_b
  ),
  by = list(
    polygon_id = source_data$polygon_id
  ),
  FUN = sum
)

reconciled_polygon_accounting <- aggregate(
  cbind(
    reconciled_cells = rep(1L, nrow(reconciled_source)),
    reconciled_events = reconciled_source$events,
    reconciled_measure_a = reconciled_source$measure_a,
    reconciled_measure_b = reconciled_source$measure_b
  ),
  by = list(
    polygon_id = reconciled_source$polygon_id
  ),
  FUN = sum
)

polygon_reconciliation <- merge(
  source_polygon_accounting,
  reconciled_polygon_accounting,
  by = "polygon_id",
  all = TRUE,
  sort = TRUE
)

polygon_reconciliation$cell_diff <- polygon_reconciliation$reconciled_cells - polygon_reconciliation$source_cells
polygon_reconciliation$event_diff <- polygon_reconciliation$reconciled_events - polygon_reconciliation$events
polygon_reconciliation$measure_a_diff <- polygon_reconciliation$reconciled_measure_a - polygon_reconciliation$measure_a
polygon_reconciliation$measure_b_diff <- polygon_reconciliation$reconciled_measure_b - polygon_reconciliation$measure_b

polygon_reconciliation$pass <- with(
  polygon_reconciliation,
  cell_diff == 0 &
    event_diff == 0 &
    measure_a_diff == 0 &
    measure_b_diff == 0
)

polygon_reconciliation <- polygon_reconciliation[
  ,
  c(
    "polygon_id",
    "source_cells",
    "reconciled_cells",
    "cell_diff",
    "events",
    "reconciled_events",
    "event_diff",
    "measure_a",
    "reconciled_measure_a",
    "measure_a_diff",
    "measure_b",
    "reconciled_measure_b",
    "measure_b_diff",
    "pass"
  )
]

polygon_total <- data.frame(
  polygon_id = "TOTAL",
  source_cells = sum(polygon_reconciliation$source_cells),
  reconciled_cells = sum(polygon_reconciliation$reconciled_cells),
  cell_diff = sum(polygon_reconciliation$cell_diff),
  events = sum(polygon_reconciliation$events),
  reconciled_events = sum(polygon_reconciliation$reconciled_events),
  event_diff = sum(polygon_reconciliation$event_diff),
  measure_a = sum(polygon_reconciliation$measure_a),
  reconciled_measure_a = sum(polygon_reconciliation$reconciled_measure_a),
  measure_a_diff = sum(polygon_reconciliation$measure_a_diff),
  measure_b = sum(polygon_reconciliation$measure_b),
  reconciled_measure_b = sum(polygon_reconciliation$reconciled_measure_b),
  measure_b_diff = sum(polygon_reconciliation$measure_b_diff),
  pass = all(polygon_reconciliation$pass)
)

polygon_reconciliation_display <- rbind(
  polygon_reconciliation,
  polygon_total
)

cat(
  "\n",
  paste(rep("-", 120), collapse = ""),
  "\nPOLYGON-LEVEL VALUE RECONCILIATION\n",
  paste(rep("-", 120), collapse = ""),
  "\n",
  sep = ""
)

print(
  polygon_reconciliation_display,
  row.names = FALSE
)

global_reconciliation <- data.frame(
  measure = c(
    "source_cells",
    "events",
    "measure_a",
    "measure_b"
  ),
  source_total = c(
    nrow(source_data),
    sum(source_data$events),
    sum(source_data$measure_a),
    sum(source_data$measure_b)
  ),
  compact_total = c(
    sum(compact_data$source_cell_n),
    sum(compact_data$events),
    sum(compact_data$measure_a),
    sum(compact_data$measure_b)
  )
)

global_reconciliation$difference <- global_reconciliation$compact_total - global_reconciliation$source_total
global_reconciliation$pass <- global_reconciliation$difference == 0

cat(
  "\n",
  paste(rep("-", 76), collapse = ""),
  "\nGLOBAL ADDITIVE RECONCILIATION\n",
  paste(rep("-", 76), collapse = ""),
  "\n",
  sep = ""
)

print(
  global_reconciliation,
  row.names = FALSE
)

if (!all(polygon_reconciliation$pass)) {
  stop("Polygon-level value reconciliation failed.")
}

if (!all(global_reconciliation$pass)) {
  stop("Global additive reconciliation failed.")
}


# =============================================================================
# 16. Explicit compact provenance reconciliation
# =============================================================================

expected_polygon_n <- vapply(
  compact_data$compact_h3,
  function(owner) {
    length(
      unique(
        source_data$polygon_id[
          source_owner == owner
        ]
      )
    )
  },
  integer(1)
)

compact_provenance_reconciliation <- data.frame(
  compact_h3 = compact_data$compact_h3,
  source_cell_n = compact_data$source_cell_n,
  polygon_id = compact_data$polygon_id,
  polygon_id_n = compact_data$polygon_id_n,
  expected_polygon_id = unname(expected_polygon_id),
  expected_polygon_id_n = unname(expected_polygon_n),
  stringsAsFactors = FALSE
)

compact_provenance_reconciliation$pass <- with(
  compact_provenance_reconciliation,
  polygon_id == expected_polygon_id &
    polygon_id_n == expected_polygon_id_n
)

cat(
  "\n",
  paste(rep("-", 110), collapse = ""),
  "\nCOMPACT PROVENANCE RECONCILIATION — MULTI-POLYGON CELLS\n",
  paste(rep("-", 110), collapse = ""),
  "\n",
  sep = ""
)

compact_provenance_multi <- compact_provenance_reconciliation[
  compact_provenance_reconciliation$polygon_id_n > 1L,
  ,
  drop = FALSE
]

if (nrow(compact_provenance_multi) == 0L) {
  cat("No multi-polygon compact cells were produced by this fixture.\n")
} else {
  print(
    compact_provenance_multi,
    row.names = FALSE
  )
}

if (!all(compact_provenance_reconciliation$pass)) {
  stop("Compact polygon provenance reconciliation failed.")
}

cat(
  "\nPASS — global totals, polygon-level values, and compact provenance reconcile exactly.\n"
)


# =============================================================================
# 17. Convert compact H3 to geometry only for mapping
# =============================================================================

geometry_time <- system.time({

  compact_map <- h3jsr::cell_to_polygon(
    compact_data$compact_h3,
    simple = FALSE
  )

})

map_attributes <- setdiff(
  names(compact_data),
  "compact_h3"
)

for (attribute in map_attributes) {
  compact_map[[attribute]] <- compact_data[[attribute]]
}

cat(
  "Compact geometry conversion: ",
  round(geometry_time[["elapsed"]], 3),
  " s\n",
  sep = ""
)


# =============================================================================
# 18. Build source geometry for comparison
# =============================================================================

source_map <- h3jsr::cell_to_polygon(
  source_data$h3,
  simple = FALSE
)

source_map$polygon_id <- source_data$polygon_id
source_map$feature_name <- source_data$feature_name


# =============================================================================
# 19. Visual QA
# =============================================================================

old_par <- par(
  mfrow = c(2, 3),
  mar = c(1, 1, 3, 1)
)

plot(
  sf::st_geometry(polygons),
  main = "1. Source polygon features",
  border = "black",
  lwd = 2
)

plot(
  source_map["polygon_id"],
  main = "2. Source H3 assignment",
  key.pos = NULL,
  reset = FALSE
)

plot(
  compact_map["compact_resolution"],
  main = "3. Compact H3 resolution",
  key.pos = NULL,
  reset = FALSE
)

plot(
  compact_map["polygon_id"],
  main = "4. Collapsed feature IDs",
  key.pos = NULL,
  reset = FALSE
)

plot(
  compact_map["feature_membership"],
  main = "5. Single vs multiple features",
  key.pos = NULL,
  reset = FALSE
)

plot(
  compact_map["events"],
  main = "6. Aggregated additive values",
  key.pos = NULL,
  reset = FALSE
)

par(old_par)


# =============================================================================
# 20. Final acceptance
# =============================================================================

final_acceptance <- data.frame(
  check = c(
    "analytical_qa",
    "global_additive_reconciliation",
    "polygon_value_reconciliation",
    "compact_provenance_reconciliation",
    "provenance_qa",
    "compact_geometry_created",
    "compact_geometry_row_count",
    "compact_attributes_accessible",
    "additive_values_mappable"
  ),
  pass = c(
    all(qa$pass),
    all(global_reconciliation$pass),
    all(polygon_reconciliation$pass),
    all(compact_provenance_reconciliation$pass),
    identical(
      compact_data$polygon_id,
      unname(expected_polygon_id)
    ),
    inherits(compact_map, "sf"),
    nrow(compact_map) == nrow(compact_data),
    all(
      c(
        "polygon_id",
        "feature_name",
        "group_name",
        "class_name"
      ) %in% names(compact_map)
    ),
    "events" %in% names(compact_map)
  )
)

cat(
  "\n",
  paste(rep("=", 76), collapse = ""),
  "\nFINAL ACCEPTANCE\n",
  paste(rep("=", 76), collapse = ""),
  "\n",
  sep = ""
)

print(final_acceptance)

if (!all(final_acceptance$pass)) {
  stop("End-to-end workflow acceptance failed.")
}

cat(
  "\nPASS — complete h3compactR V1 analytical workflow validated.\n"
)
