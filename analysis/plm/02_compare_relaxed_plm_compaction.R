# =============================================================================
# h3compactR
# 02_compare_relaxed_plm_compaction.R
# =============================================================================
#
# Purpose:
# Fast structural comparison of exact and relaxed H3 compaction against the
# authoritative PLM Stage 2 Resolution 8 support.
#
# This script deliberately avoids building the full 147,894-row aggregation
# lookup for every threshold.
#
# It compares compact support only:
#   - compact cell count;
#   - resolution composition;
#   - geometric spillover;
#   - missing source support;
#   - hierarchy overlap;
#   - runtime.
#
# Each candidate is checkpointed locally as an RDS file so interrupted runs
# do not need to be recomputed.
#
# Full aggregation lookup and analytical conservation QA should be run only
# for the selected candidate.
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
  library(h3jsr)
})


# -----------------------------------------------------------------------------
# 2. PLM parameters
# -----------------------------------------------------------------------------

plm_root <- "F:/PLM_Prod/PLM_Analytics"

duckdb_path <- file.path(
  plm_root,
  "duckdb",
  "plm_prod.duckdb"
)

baseline_table <- "analysis.h3_res8_baseline"

expected_source_cells <- 147894L
source_resolution <- 8L

candidate_min_resolution <- 5L

thresholds <- c(
  exact = 1,
  six_of_seven = 6 / 7,
  five_of_seven = 5 / 7
)


# -----------------------------------------------------------------------------
# 3. Local development cache
# -----------------------------------------------------------------------------
#
# These files are development checkpoints only.
# They are NOT authoritative PLM outputs.

cache_dir <- file.path(
  "analysis",
  "plm",
  "cache"
)

dir.create(
  cache_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -----------------------------------------------------------------------------
# 4. Validate authoritative source
# -----------------------------------------------------------------------------

if (!file.exists(duckdb_path)) {
  stop(
    "PLM DuckDB does not exist: ",
    duckdb_path
  )
}

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = duckdb_path,
  read_only = TRUE
)

if (!DBI::dbIsValid(con)) {
  stop(
    "Could not establish valid PLM DuckDB connection."
  )
}

source_tbl <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT hex_id ",
    "FROM ",
    baseline_table
  )
) |>
  tibble::as_tibble() |>
  transmute(
    source_h3 = as.character(.data$hex_id)
  )

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

source_h3 <- source_tbl$source_h3 |>
  validate_h3()

stopifnot(
  length(source_h3) ==
    expected_source_cells,

  n_distinct(source_h3) ==
    expected_source_cells,

  all(
    h3jsr::get_res(source_h3) ==
      source_resolution
  )
)


# -----------------------------------------------------------------------------
# 5. Source QA
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PLM FAST RELAXED COMPACTION SWEEP\n")
cat("============================================================\n\n")

cat(
  "Authoritative Res 8 cells: ",
  format(
    length(source_h3),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Minimum candidate resolution: ",
  candidate_min_resolution,
  "\n\n"
)


# -----------------------------------------------------------------------------
# 6. Candidate runner
# -----------------------------------------------------------------------------

run_candidate <- function(
    candidate_name,
    threshold
) {

  cache_file <- file.path(
    cache_dir,
    paste0(
      "plm_relaxed_",
      candidate_name,
      ".rds"
    )
  )


  # ---------------------------------------------------------------------------
  # 6a. Reuse checkpoint if available
  # ---------------------------------------------------------------------------

  if (file.exists(cache_file)) {

    cat(
      "Loading cached candidate: ",
      candidate_name,
      "\n"
    )

    return(
      readRDS(
        cache_file
      )
    )
  }


# ---------------------------------------------------------------------------
# 6b. Run structural compaction only
# ---------------------------------------------------------------------------

cat(
  "Running candidate: ",
  candidate_name,
  " | threshold = ",
  round(threshold, 6),
  "\n",
  sep = ""
)

start_time <- Sys.time()

if (identical(candidate_name, "exact")) {

  # Native H3 compaction is the authoritative exact baseline.
  compact_h3 <- h3compactR::compact_h3(
    source_h3
  )

} else {

  # Relaxed candidates use the coverage-constrained algorithm.
  compact_h3 <- h3compactR::compact_h3_relaxed(
    h3 = source_h3,
    min_coverage = threshold,
    min_resolution = candidate_min_resolution,
    simple = TRUE
  )
}

elapsed_seconds <- as.numeric(
  difftime(
    Sys.time(),
    start_time,
    units = "secs"
  )
)


  # ---------------------------------------------------------------------------
  # 6c. Hierarchy QA
  # ---------------------------------------------------------------------------

  hierarchy_overlaps <- nrow(
    check_h3_hierarchy(
      compact_h3
    )
  )


  # ---------------------------------------------------------------------------
  # 6d. Geometric round-trip QA
  # ---------------------------------------------------------------------------
  #
  # Relaxed output may expand beyond authoritative source support.
  #
  # Missing source cells are prohibited.
  # Additional cells are measured spillover.

  roundtrip_h3 <- uncompact_h3(
    compact_h3,
    resolution = source_resolution
  )

  missing_h3 <- setdiff(
    source_h3,
    roundtrip_h3
  )

  additional_h3 <- setdiff(
    roundtrip_h3,
    source_h3
  )


  # ---------------------------------------------------------------------------
  # 6e. Resolution composition
  # ---------------------------------------------------------------------------

  resolution_table <- tibble(
    resolution =
      h3jsr::get_res(
        compact_h3
      )
  ) |>
    count(
      resolution,
      name = "cell_count"
    ) |>
    mutate(
      cell_pct =
        round(
          100 *
            .data$cell_count /
            sum(
              .data$cell_count
            ),
          2
        )
    )


  # ---------------------------------------------------------------------------
  # 6f. Summary
  # ---------------------------------------------------------------------------

  summary <- tibble(
    candidate =
      candidate_name,

    min_coverage =
      threshold,

    source_cells =
      length(source_h3),

    compact_cells =
      length(compact_h3),

    reduction_n =
      length(source_h3) -
      length(compact_h3),

    reduction_pct =
      round(
        100 *
          (
            length(source_h3) -
              length(compact_h3)
          ) /
          length(source_h3),
        2
      ),

    compact_min_resolution =
      min(
        h3jsr::get_res(
          compact_h3
        )
      ),

    compact_max_resolution =
      max(
        h3jsr::get_res(
          compact_h3
        )
      ),

    hierarchy_overlaps =
      hierarchy_overlaps,

    missing_source_cells =
      length(
        missing_h3
      ),

    spillover_cells =
      length(
        additional_h3
      ),

    spillover_pct =
      round(
        100 *
          length(
            additional_h3
          ) /
          length(source_h3),
        2
      ),

    source_coverage_preserved =
      length(
        missing_h3
      ) == 0L,

    runtime_seconds =
      elapsed_seconds
  )


  # ---------------------------------------------------------------------------
  # 6g. Hard QA
  # ---------------------------------------------------------------------------

  stopifnot(
    hierarchy_overlaps == 0L,
    length(missing_h3) == 0L
  )


  # ---------------------------------------------------------------------------
  # 6h. Checkpoint result
  # ---------------------------------------------------------------------------

  result <- list(
    candidate =
      candidate_name,

    threshold =
      threshold,

    compact_h3 =
      compact_h3,

    summary =
      summary,

    resolution_table =
      resolution_table,

    spillover_h3 =
      additional_h3,

    runtime_seconds =
      elapsed_seconds
  )

  saveRDS(
    result,
    cache_file
  )

  cat(
    "Saved: ",
    cache_file,
    "\n"
  )

  cat(
    "Compact cells: ",
    format(
      length(compact_h3),
      big.mark = ","
    ),
    " | spillover: ",
    format(
      length(additional_h3),
      big.mark = ","
    ),
    " | runtime: ",
    round(
      elapsed_seconds,
      1
    ),
    " sec\n\n",
    sep = ""
  )

  result
}


# -----------------------------------------------------------------------------
# 7. Run candidate sweep
# -----------------------------------------------------------------------------

results <- Map(
  f = run_candidate,
  candidate_name = names(thresholds),
  threshold = as.numeric(thresholds)
)

# -----------------------------------------------------------------------------
# 8. Build candidate comparison
# -----------------------------------------------------------------------------

candidate_summary <- bind_rows(
  lapply(
    results,
    `[[`,
    "summary"
  )
)

cat("\n")
cat("============================================================\n")
cat("PLM CANDIDATE COMPARISON\n")
cat("============================================================\n\n")

print(
  candidate_summary,
  n = Inf
)


# -----------------------------------------------------------------------------
# 9. Resolution composition
# -----------------------------------------------------------------------------

resolution_summary <- bind_rows(
  lapply(
    results,
    function(x) {

      x$resolution_table |>
        mutate(
          candidate =
            x$candidate,

          min_coverage =
            x$threshold
        )
    }
  )
) |>
  select(
    candidate,
    min_coverage,
    resolution,
    cell_count,
    cell_pct
  )

cat("\n")
cat("============================================================\n")
cat("PLM RESOLUTION COMPOSITION\n")
cat("============================================================\n\n")

print(
  resolution_summary,
  n = Inf
)


# -----------------------------------------------------------------------------
# 10. Compare against exact candidate
# -----------------------------------------------------------------------------

exact_cells <- candidate_summary |>
  filter(
    .data$candidate ==
      "exact"
  ) |>
  pull(
    .data$compact_cells
  )

comparison <- candidate_summary |>
  mutate(
    additional_reduction_vs_exact =
      exact_cells -
      .data$compact_cells,

    pct_fewer_cells_vs_exact =
      round(
        100 *
          (
            exact_cells -
              .data$compact_cells
          ) /
          exact_cells,
        2
      )
  )

cat("\n")
cat("============================================================\n")
cat("RELAXED GAIN VS EXACT\n")
cat("============================================================\n\n")

print(
  comparison |>
    select(
      candidate,
      min_coverage,
      compact_cells,
      reduction_pct,
      additional_reduction_vs_exact,
      pct_fewer_cells_vs_exact,
      spillover_cells,
      spillover_pct,
      runtime_seconds
    ),
  n = Inf
)


# -----------------------------------------------------------------------------
# 11. Hard sweep QA
# -----------------------------------------------------------------------------

stopifnot(
  all(
    candidate_summary$source_cells ==
      expected_source_cells
  ),

  all(
    candidate_summary$hierarchy_overlaps ==
      0L
  ),

  all(
    candidate_summary$missing_source_cells ==
      0L
  ),

  all(
    candidate_summary$source_coverage_preserved
  )
)


# -----------------------------------------------------------------------------
# 12. Final status
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PLM FAST RELAXED COMPACTION SWEEP PASSED\n")
cat("============================================================\n\n")

cat(
  "Candidate checkpoints:\n"
)

print(
  list.files(
    cache_dir,
    pattern = "^plm_relaxed_.*\\.rds$",
    full.names = TRUE
  )
)