# =============================================================================
# h3compactR
# Relaxed H3 compaction
# =============================================================================


#' Compact H3 cells using a minimum coverage threshold
#'
#' Performs controlled hierarchical H3 compaction where a parent cell may be
#' retained when a specified proportion of its source-resolution descendants
#' are present.
#'
#' Unlike [h3compactR::compact_h3()], relaxed compaction may introduce
#' geometric spillover. Missing descendants are represented by the retained
#' parent geometry but are not added to authoritative source membership.
#'
#' Source-to-compact membership is preserved through
#' [h3compactR::build_h3_aggregation_lookup()].
#'
#' @param h3 Character vector of unique H3 indexes from one source resolution.
#' @param min_coverage Minimum proportion of source-resolution descendants that
#'   must be present for a parent to be retained. Must be greater than 0 and
#'   less than or equal to 1.
#' @param min_resolution Coarsest H3 resolution allowed. When `NULL`, only one
#'   resolution of relaxed promotion is attempted.
#' @param simple When `TRUE`, return only retained compact H3 indexes. When
#'   `FALSE`, return compact indexes, aggregation lookup, compact-cell metadata,
#'   and QA.
#'
#' @return A character vector when `simple = TRUE`, otherwise a list.
#'
#' @export
compact_h3_relaxed <- function(
    h3,
    min_coverage = 0.80,
    min_resolution = NULL,
    simple = TRUE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate inputs
  # ---------------------------------------------------------------------------

  source_h3 <- validate_h3(h3)

  if (length(source_h3) == 0L) {
    cli::cli_abort(
      "`h3` must contain at least one H3 index."
    )
  }

  source_h3 <- sort(
    unique(source_h3)
  )

  source_resolutions <- unique(
    h3jsr::get_res(source_h3)
  )

  if (length(source_resolutions) != 1L) {
    cli::cli_abort(
      "`h3` must contain exactly one source resolution."
    )
  }

  source_resolution <- source_resolutions[[1]]

  if (
    length(min_coverage) != 1L ||
    is.na(min_coverage) ||
    !is.numeric(min_coverage) ||
    min_coverage <= 0 ||
    min_coverage > 1
  ) {
    cli::cli_abort(
      "`min_coverage` must be one numeric value greater than 0 and <= 1."
    )
  }

  if (source_resolution == 0L) {
    cli::cli_abort(
      "Resolution 0 H3 cells cannot be promoted to a coarser resolution."
    )
  }

  if (is.null(min_resolution)) {

    min_resolution <- source_resolution - 1L

  } else {

    if (
      length(min_resolution) != 1L ||
      is.na(min_resolution) ||
      min_resolution < 0L ||
      min_resolution >= source_resolution
    ) {
      cli::cli_abort(
        paste0(
          "`min_resolution` must be one integer from 0 to ",
          source_resolution - 1L,
          "."
        )
      )
    }

    min_resolution <- as.integer(
      min_resolution
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Start with authoritative source cells
  # ---------------------------------------------------------------------------

  retained_h3 <- source_h3


  # ---------------------------------------------------------------------------
  # 3. Test candidate parents from fine to coarse
  # ---------------------------------------------------------------------------
  #
  # Coverage is always calculated against the ORIGINAL source-resolution
  # support.
  #
  # This avoids compounding relaxed coverage when recursively promoting through
  # several H3 resolutions.

  promotion_resolutions <- seq.int(
    from = source_resolution - 1L,
    to = min_resolution,
    by = -1L
  )

  for (target_resolution in promotion_resolutions) {

    candidate_parents <- h3jsr::get_parent(
      source_h3,
      res = target_resolution,
      simple = TRUE
    ) |>
      unlist(use.names = FALSE) |>
      unique() |>
      sort()

    qualifying_parents <- vapply(
      candidate_parents,
      FUN = function(parent_h3) {

        possible_descendants <- h3jsr::get_children(
          parent_h3,
          res = source_resolution,
          simple = TRUE
        ) |>
          unlist(use.names = FALSE)

        source_descendant_count <- sum(
          possible_descendants %in% source_h3
        )

        coverage_ratio <-
          source_descendant_count /
          length(possible_descendants)

        coverage_ratio >= min_coverage
      },
      FUN.VALUE = logical(1)
    )

    promoted_parents <- candidate_parents[
      qualifying_parents
    ]

    if (length(promoted_parents) == 0L) {
      next
    }


    # -------------------------------------------------------------------------
    # 3a. Remove retained descendants represented by promoted parents
    # -------------------------------------------------------------------------

    retained_resolution <- h3jsr::get_res(
      retained_h3
    )

    retained_parent_at_target <- vapply(
      seq_along(retained_h3),
      FUN = function(i) {

        if (retained_resolution[[i]] <= target_resolution) {
          return(retained_h3[[i]])
        }

        h3jsr::get_parent(
          retained_h3[[i]],
          res = target_resolution,
          simple = TRUE
        ) |>
          unlist(use.names = FALSE)
      },
      FUN.VALUE = character(1)
    )

    remove_descendant <- retained_parent_at_target %in%
      promoted_parents

    retained_h3 <- c(
      retained_h3[!remove_descendant],
      promoted_parents
    ) |>
      unique() |>
      sort()
  }


  # ---------------------------------------------------------------------------
  # 4. Final hierarchy QA
  # ---------------------------------------------------------------------------

  retained_h3 <- validate_h3(
    retained_h3
  )

  hierarchy_qa <- check_h3_hierarchy(
    retained_h3
  )

  if (nrow(hierarchy_qa) > 0L) {
    cli::cli_abort(
      "Relaxed compaction produced parent-descendant hierarchy overlap."
    )
  }

  if (simple) {
    return(retained_h3)
  }


  # ---------------------------------------------------------------------------
  # 5. Build authoritative source → compact lookup
  # ---------------------------------------------------------------------------
  #
  # Only actual input source cells enter this lookup.
  #
  # Geometric spillover descendants are NEVER added to authoritative
  # membership.

  lookup <- build_h3_aggregation_lookup(
    source_h3 = source_h3,
    compacted_h3 = retained_h3
  )


  # ---------------------------------------------------------------------------
  # 6. Build compact-cell coverage metadata
  # ---------------------------------------------------------------------------

  compact_metadata <- lapply(
    retained_h3,
    FUN = function(compact_cell) {

      compact_resolution <- h3jsr::get_res(
        compact_cell
      )

      possible_descendants <- if (
        compact_resolution == source_resolution
      ) {

        compact_cell

      } else {

        h3jsr::get_children(
          compact_cell,
          res = source_resolution,
          simple = TRUE
        ) |>
          unlist(use.names = FALSE)
      }

      source_cell_count <- sum(
        possible_descendants %in% source_h3
      )

      possible_source_cell_count <- length(
        possible_descendants
      )

      spillover_cell_count <-
        possible_source_cell_count -
        source_cell_count

      coverage_ratio <-
        source_cell_count /
        possible_source_cell_count

      compaction_mode <- if (
        compact_resolution == source_resolution
      ) {
        "self"
      } else if (
        spillover_cell_count == 0L
      ) {
        "exact"
      } else {
        "relaxed"
      }

      tibble::tibble(
        compact_h3 = compact_cell,
        compact_resolution = compact_resolution,
        source_cell_count = source_cell_count,
        possible_source_cell_count = possible_source_cell_count,
        coverage_ratio = coverage_ratio,
        spillover_cell_count = spillover_cell_count,
        spillover_ratio =
          spillover_cell_count /
          possible_source_cell_count,
        compaction_mode = compaction_mode
      )
    }
  ) |>
    dplyr::bind_rows()


  # ---------------------------------------------------------------------------
  # 7. Geometric round-trip QA
  # ---------------------------------------------------------------------------
  #
  # Relaxed compaction is expected to reconstruct a SUPERSET of the source.
  #
  # Missing source cells are not permitted.
  # Additional cells are quantified as spillover.

  reconstructed_h3 <- uncompact_h3(
    retained_h3,
    resolution = source_resolution
  )

  missing_source_h3 <- setdiff(
    source_h3,
    reconstructed_h3
  )

  additional_h3 <- setdiff(
    reconstructed_h3,
    source_h3
  )


  # ---------------------------------------------------------------------------
  # 8. Membership QA
  # ---------------------------------------------------------------------------

  membership_missing <- length(
    setdiff(
      source_h3,
      lookup$source_h3
    )
  )

  membership_duplicates <-
    nrow(lookup) -
    dplyr::n_distinct(
      lookup$source_h3
    )


  # ---------------------------------------------------------------------------
  # 9. QA summary
  # ---------------------------------------------------------------------------

  qa <- tibble::tibble(
    source_cells = length(source_h3),
    compact_cells = length(retained_h3),

    reduction_n =
      length(source_h3) -
      length(retained_h3),

    reduction_pct =
      round(
        100 *
          (
            length(source_h3) -
              length(retained_h3)
          ) /
          length(source_h3),
        2
      ),

    min_coverage = min_coverage,

    min_resolution =
      min(
        h3jsr::get_res(
          retained_h3
        )
      ),

    max_resolution =
      max(
        h3jsr::get_res(
          retained_h3
        )
      ),

    exact_cells =
      sum(
        compact_metadata$compaction_mode ==
          "exact"
      ),

    relaxed_cells =
      sum(
        compact_metadata$compaction_mode ==
          "relaxed"
      ),

    self_cells =
      sum(
        compact_metadata$compaction_mode ==
          "self"
      ),

    spillover_cells =
      length(additional_h3),

    spillover_pct =
      round(
        100 *
          length(additional_h3) /
          length(source_h3),
        2
      ),

    hierarchy_overlaps =
      nrow(hierarchy_qa),

    missing_source_memberships =
      membership_missing,

    duplicate_source_memberships =
      membership_duplicates,

    roundtrip_missing =
      length(missing_source_h3),

    roundtrip_additional =
      length(additional_h3),

    source_coverage_preserved =
      length(missing_source_h3) == 0L
  )


  # ---------------------------------------------------------------------------
  # 10. Hard authoritative-membership contract
  # ---------------------------------------------------------------------------

  if (membership_missing > 0L) {
    cli::cli_abort(
      "Relaxed compaction lost authoritative source membership."
    )
  }

  if (membership_duplicates > 0L) {
    cli::cli_abort(
      "Relaxed compaction produced duplicated source membership."
    )
  }

  if (length(missing_source_h3) > 0L) {
    cli::cli_abort(
      "Relaxed compact support does not cover all source H3 cells."
    )
  }


  # ---------------------------------------------------------------------------
  # 11. Return structured result
  # ---------------------------------------------------------------------------

  list(
    h3 = retained_h3,
    lookup = lookup,
    metadata = compact_metadata,
    qa = qa
  )
}