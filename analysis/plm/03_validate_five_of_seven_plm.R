# =============================================================================
# h3compactR
# 03_validate_five_of_seven_plm.R
# =============================================================================
#
# Purpose:
# Final applied validation of the selected PLM 5/7 relaxed-compaction
# candidate.
#
# IMPORTANT:
#   - reuse the cached compact H3 support;
#   - do NOT recompute relaxed compaction;
#   - build the authoritative lookup once;
#   - validate analytical conservation;
#   - generate final compact geometry;
#   - visually inspect compact support against authoritative Res 8 support.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load package and dependencies
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
# 2. Paths and parameters
# -----------------------------------------------------------------------------

plm_root <- "F:/PLM_Prod/PLM_Analytics"

duckdb_path <- file.path(
  plm_root,
  "duckdb",
  "plm_prod.duckdb"
)

cache_file <- file.path(
  "analysis",
  "plm",
  "cache",
  "plm_relaxed_five_of_seven.rds"
)

expected_source_cells <- 147894L
source_resolution <- 8L

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
# 3. Load frozen 5/7 candidate
# -----------------------------------------------------------------------------

if (!file.exists(cache_file)) {
  stop(
    "Cached 5/7 candidate not found: ",
    cache_file
  )
}

candidate <- readRDS(
  cache_file
)

compact_h3 <- candidate$compact_h3

stopifnot(
  length(compact_h3) == 29642L,
  nrow(check_h3_hierarchy(compact_h3)) == 0L
)

cat("\n")
cat("============================================================\n")
cat("PLM 5/7 CANDIDATE — CACHED SUPPORT\n")
cat("============================================================\n\n")

cat("Compact cells:    ", format(length(compact_h3), big.mark = ","), "\n")
cat("Coverage threshold: 5/7\n")
cat("Cached spillover: ", format(length(candidate$spillover_h3), big.mark = ","), "\n")


# -----------------------------------------------------------------------------
# 4. Read authoritative Res 8 baseline
# -----------------------------------------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = duckdb_path,
  read_only = TRUE
)

baseline <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT hex_id, ",
    paste(additive_fields, collapse = ", "),
    " FROM analysis.h3_res8_baseline"
  )
) |>
  tibble::as_tibble() |>
  rename(
    source_h3 = .data$hex_id
  ) |>
  mutate(
    source_h3 = as.character(.data$source_h3)
  )

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

source_h3 <- validate_h3(
  baseline$source_h3
)

stopifnot(
  length(source_h3) == expected_source_cells,
  n_distinct(source_h3) == expected_source_cells,
  all(h3jsr::get_res(source_h3) == source_resolution)
)


# -----------------------------------------------------------------------------
# 5. Build authoritative aggregation lookup
# -----------------------------------------------------------------------------
#
# This is expected to be slow with the current matcher.
# It is performed ONCE for the selected production candidate.

cat("\nBuilding authoritative 147,894-row aggregation lookup...\n")

lookup_start <- Sys.time()

lookup <- build_h3_aggregation_lookup(
  source_h3 = source_h3,
  compacted_h3 = compact_h3
)

lookup_seconds <- as.numeric(
  difftime(
    Sys.time(),
    lookup_start,
    units = "secs"
  )
)

cat(
  "Lookup runtime: ",
  round(lookup_seconds, 1),
  " seconds\n"
)


# -----------------------------------------------------------------------------
# 6. Lookup QA
# -----------------------------------------------------------------------------

lookup_qa <- tibble(
  source_cells = length(source_h3),
  lookup_rows = nrow(lookup),
  distinct_source_h3 = n_distinct(lookup$source_h3),

  missing_source_h3 =
    length(setdiff(source_h3, lookup$source_h3)),

  unexpected_source_h3 =
    length(setdiff(lookup$source_h3, source_h3)),

  duplicate_source_assignments =
    nrow(lookup) -
    n_distinct(lookup$source_h3),

  unexpected_compact_h3 =
    length(
      setdiff(
        unique(lookup$compact_h3),
        compact_h3
      )
    ),

  resolution_mismatch =
    sum(
      h3jsr::get_res(lookup$compact_h3) !=
        lookup$compact_resolution
    )
)

cat("\n")
cat("============================================================\n")
cat("5/7 AGGREGATION LOOKUP QA\n")
cat("============================================================\n\n")

print(lookup_qa)

stopifnot(
  lookup_qa$lookup_rows == expected_source_cells,
  lookup_qa$distinct_source_h3 == expected_source_cells,
  lookup_qa$missing_source_h3 == 0L,
  lookup_qa$unexpected_source_h3 == 0L,
  lookup_qa$duplicate_source_assignments == 0L,
  lookup_qa$unexpected_compact_h3 == 0L,
  lookup_qa$resolution_mismatch == 0L
)


# -----------------------------------------------------------------------------
# 7. Geometric support QA
# -----------------------------------------------------------------------------

roundtrip_h3 <- uncompact_h3(
  compact_h3,
  resolution = source_resolution
)

roundtrip_qa <- tibble(
  source_cells = length(source_h3),
  reconstructed_cells = length(roundtrip_h3),

  missing_source_cells =
    length(
      setdiff(
        source_h3,
        roundtrip_h3
      )
    ),

  spillover_cells =
    length(
      setdiff(
        roundtrip_h3,
        source_h3
      )
    ),

  spillover_pct =
    round(
      100 *
        length(
          setdiff(
            roundtrip_h3,
            source_h3
          )
        ) /
        length(source_h3),
      2
    )
)

cat("\n")
cat("============================================================\n")
cat("5/7 GEOMETRIC SUPPORT QA\n")
cat("============================================================\n\n")

print(roundtrip_qa)

stopifnot(
  roundtrip_qa$missing_source_cells == 0L,
  roundtrip_qa$spillover_cells == 15020L
)


# -----------------------------------------------------------------------------
# 8. Aggregate authoritative additive attributes
# -----------------------------------------------------------------------------

mapped <- baseline |>
  left_join(
    lookup,
    by = "source_h3"
  )

stopifnot(
  nrow(mapped) == expected_source_cells,
  !anyNA(mapped$compact_h3)
)

compact_attributes <- mapped |>
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
# 9. Aggregation conservation QA
# -----------------------------------------------------------------------------

source_totals <- vapply(
  baseline[additive_fields],
  sum,
  numeric(1)
)

compact_totals <- vapply(
  compact_attributes[additive_fields],
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

    preserved =
      abs(.data$difference) <=
      pmax(
        1e-10,
        abs(.data$source_total) * 1e-10
      )
  )

cat("\n")
cat("============================================================\n")
cat("5/7 ANALYTICAL CONSERVATION QA\n")
cat("============================================================\n\n")

print(
  aggregation_qa,
  n = Inf
)

stopifnot(
  all(aggregation_qa$preserved)
)


# -----------------------------------------------------------------------------
# 10. Build final compact geometry
# -----------------------------------------------------------------------------

cat("\nBuilding final 5/7 compact geometry...\n")

compact_sf <- h3_compaction_geometry(
  h3 = compact_h3,
  output_crs = 4326
) |>
  rename(
    compact_hex_id = .data$h3,
    compact_resolution = .data$resolution
  )


# -----------------------------------------------------------------------------
# 11. Build authoritative Res 8 QA geometry
# -----------------------------------------------------------------------------
#
# Used for visual comparison only.

source_sf <- h3jsr::cell_to_polygon(
  source_h3,
  simple = FALSE
)

names(source_sf)[
  names(source_sf) == "h3_address"
] <- "hex_id"


# -----------------------------------------------------------------------------
# 12. Final geometry QA
# -----------------------------------------------------------------------------

geometry_qa <- tibble(
  compact_cells = length(compact_h3),
  geometry_rows = nrow(compact_sf),
  invalid_geometry = sum(!st_is_valid(compact_sf)),
  empty_geometry = sum(st_is_empty(compact_sf)),
  crs_epsg = st_crs(compact_sf)$epsg
)

cat("\n")
cat("============================================================\n")
cat("5/7 FINAL GEOMETRY QA\n")
cat("============================================================\n\n")

print(geometry_qa)

stopifnot(
  geometry_qa$geometry_rows == 29642L,
  geometry_qa$invalid_geometry == 0L,
  geometry_qa$empty_geometry == 0L,
  geometry_qa$crs_epsg == 4326
)


# -----------------------------------------------------------------------------
# 13. Resolution composition
# -----------------------------------------------------------------------------

resolution_summary <- compact_sf |>
  st_drop_geometry() |>
  count(
    .data$compact_resolution,
    name = "cell_count"
  ) |>
  mutate(
    cell_pct =
      round(
        100 *
          .data$cell_count /
          sum(.data$cell_count),
        2
      )
  )

cat("\n")
cat("============================================================\n")
cat("5/7 RESOLUTION COMPOSITION\n")
cat("============================================================\n\n")

print(resolution_summary)


# -----------------------------------------------------------------------------
# 14. Cartographic QA plot
# -----------------------------------------------------------------------------
#
# Authoritative Res 8 support is drawn as faint grey outlines.
# Selected 5/7 compact support is drawn over it and coloured by resolution.
#
# Any geometric expansion is therefore visible while the authoritative
# analytical membership remains controlled by the lookup.

compact_plot <- ggplot() +

  geom_sf(
    data = source_sf,
    fill = NA,
    colour = "grey80",
    linewidth = 0.015
  ) +

  geom_sf(
    data = compact_sf,
    aes(
      fill = factor(
        .data$compact_resolution
      )
    ),
    colour = "grey25",
    linewidth = 0.025,
    alpha = 0.70
  ) +

  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +

  labs(
    title = "PLM Stage 2 — relaxed 5/7 compact support",

    subtitle = paste0(
      "147,894 authoritative Res 8 cells → ",
      format(
        nrow(compact_sf),
        big.mark = ","
      ),
      " compact cells | ",
      format(
        roundtrip_qa$spillover_cells,
        big.mark = ","
      ),
      " spillover descendants (",
      roundtrip_qa$spillover_pct,
      "%)"
    ),

    caption =
      "Grey outlines show authoritative Res 8 support; coloured polygons show retained compact H3 geometry."
  ) +

  theme_minimal() +

  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  ) +

  coord_sf()

print(compact_plot)


# -----------------------------------------------------------------------------
# 15. Final acceptance summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PLM 5/7 FINAL ACCEPTANCE SUMMARY\n")
cat("============================================================\n\n")

cat("Authoritative source cells: 147,894\n")
cat(
  "Compact cells:              ",
  format(length(compact_h3), big.mark = ","),
  "\n"
)

cat(
  "Lookup rows:                ",
  format(nrow(lookup), big.mark = ","),
  "\n"
)

cat(
  "Spillover cells:            ",
  format(roundtrip_qa$spillover_cells, big.mark = ","),
  "\n"
)

cat(
  "Spillover percentage:       ",
  roundtrip_qa$spillover_pct,
  "%\n"
)

cat(
  "Missing source support:     ",
  roundtrip_qa$missing_source_cells,
  "\n"
)

cat(
  "Duplicate assignments:      ",
  lookup_qa$duplicate_source_assignments,
  "\n"
)

cat(
  "All additive totals kept:   ",
  all(aggregation_qa$preserved),
  "\n"
)

cat(
  "Invalid geometry:           ",
  geometry_qa$invalid_geometry,
  "\n"
)


# -----------------------------------------------------------------------------
# 16. Final status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PLM 5/7 CANDIDATE VALIDATION PASSED\n")
cat("============================================================\n\n")