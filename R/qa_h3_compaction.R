#' Audit an H3 compaction
#'
#' Performs analytical quality-assurance checks on the relationship between an
#' authoritative single-resolution H3 source grid and its compacted
#' mixed-resolution representation.
#'
#' @param source A character vector of authoritative source H3 cell indexes.
#'   Source cells must all have the same H3 resolution.
#' @param compacted A character vector of compacted H3 cell indexes.
#' @param lookup Optional source-to-compacted ownership lookup produced by
#'   [h3_compaction_lookup()]. If `NULL`, the lookup is generated internally.
#'
#' @details
#' The source H3 grid is treated as the authoritative analytical support.
#' Quality assurance therefore evaluates whether the compacted representation
#' reconstructs that support exactly and whether every source cell has one
#' valid compact owner.
#'
#' The audit is based on H3 hierarchy rather than displayed polygon geometry.
#' Apparent geometric extension of a mixed-resolution H3 polygon beyond a study
#' boundary does not imply additional analytical membership.
#'
#' The audit reports:
#'
#' * source and compacted cell counts;
#' * reduction in cell count;
#' * source and compacted resolutions;
#' * exact and ancestor ownership counts;
#' * round-trip missing and additional source cells;
#' * exact reconstruction status;
#' * hierarchy integrity; and
#' * ownership integrity.
#'
#' @return A list of class `"h3_compaction_qa"` containing:
#'
#' \describe{
#'   \item{summary}{One-row tibble containing the main QA metrics.}
#'   \item{resolution}{Tibble describing the compacted resolution mix.}
#'   \item{ownership}{Tibble describing source-cell ownership by compact
#'   resolution and relationship.}
#' }
#'
#' @references
#' H3 hierarchy, compaction and uncompaction:
#' <https://h3geo.org/docs/highlights/indexing/>.
#'
#' @export
qa_h3_compaction <- function(
  source,
  compacted,
  lookup = NULL
) {

  # ---------------------------------------------------------------------------
  # 1. Validate source H3 input
  # ---------------------------------------------------------------------------

  if (!is.character(source)) {
    cli::cli_abort(
      "`source` must be a character vector of H3 cell indexes."
    )
  }

  if (length(source) == 0L) {
    cli::cli_abort(
      "`source` must contain at least one H3 cell."
    )
  }

  if (anyNA(source) || any(!nzchar(source))) {
    cli::cli_abort(
      "`source` must not contain missing or empty H3 indexes."
    )
  }

  source_valid <- h3jsr::is_valid(source)

  if (anyNA(source_valid) || !all(source_valid)) {
    cli::cli_abort(
      "`source` contains invalid H3 indexes."
    )
  }

  source <- sort(unique(source))


  # ---------------------------------------------------------------------------
  # 2. Establish authoritative source resolution
  # ---------------------------------------------------------------------------

  source_resolutions <- h3jsr::get_res(source)
  unique_source_resolutions <- unique(source_resolutions)

  if (length(unique_source_resolutions) != 1L) {
    cli::cli_abort(
      "`source` must contain H3 cells at a single resolution."
    )
  }

  source_resolution <- as.integer(
    unique_source_resolutions[[1L]]
  )


  # ---------------------------------------------------------------------------
  # 3. Validate compacted H3 input
  # ---------------------------------------------------------------------------

  if (!is.character(compacted)) {
    cli::cli_abort(
      "`compacted` must be a character vector of H3 cell indexes."
    )
  }

  if (length(compacted) == 0L) {
    cli::cli_abort(
      "`compacted` must contain at least one H3 cell."
    )
  }

  if (anyNA(compacted) || any(!nzchar(compacted))) {
    cli::cli_abort(
      "`compacted` must not contain missing or empty H3 indexes."
    )
  }

  compact_valid <- h3jsr::is_valid(compacted)

  if (anyNA(compact_valid) || !all(compact_valid)) {
    cli::cli_abort(
      "`compacted` contains invalid H3 indexes."
    )
  }

  compacted <- sort(unique(compacted))

  compact_resolutions <- as.integer(
    h3jsr::get_res(compacted)
  )

  if (any(compact_resolutions > source_resolution)) {
    cli::cli_abort(
      paste0(
        "`compacted` contains H3 cells finer than the authoritative ",
        "source resolution."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Reconstruct source support from compacted H3 cells
  # ---------------------------------------------------------------------------

  reconstructed <- h3jsr::uncompact(
    compacted,
    res = source_resolution,
    simple = TRUE
  )

  reconstructed <- sort(
    unique(
      as.character(reconstructed)
    )
  )

  reconstructed_valid <- h3jsr::is_valid(
    reconstructed
  )

  if (
    anyNA(reconstructed_valid) ||
      !all(reconstructed_valid)
  ) {
    cli::cli_abort(
      "Round-trip reconstruction produced invalid H3 indexes."
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Compare reconstructed and authoritative source support
  # ---------------------------------------------------------------------------

  missing_source <- setdiff(
    source,
    reconstructed
  )

  additional_source <- setdiff(
    reconstructed,
    source
  )

  exact_reconstruction <-
    length(missing_source) == 0L &&
    length(additional_source) == 0L


  # ---------------------------------------------------------------------------
  # 6. Validate compact hierarchy integrity
  # ---------------------------------------------------------------------------

  hierarchy_overlap_n <- 0L

  compact_resolution_values <- sort(
    unique(compact_resolutions)
  )

  if (length(compact_resolution_values) > 1L) {

    for (fine_resolution in compact_resolution_values) {

      fine_cells <- compacted[
        compact_resolutions == fine_resolution
      ]

      coarser_resolutions <- compact_resolution_values[
        compact_resolution_values < fine_resolution
      ]

      if (
        length(fine_cells) == 0L ||
          length(coarser_resolutions) == 0L
      ) {
        next
      }

      for (coarse_resolution in coarser_resolutions) {

        coarse_cells <- compacted[
          compact_resolutions == coarse_resolution
        ]

        fine_parents <- h3jsr::get_parent(
          fine_cells,
          res = coarse_resolution
        )

        hierarchy_overlap_n <-
          hierarchy_overlap_n +
          sum(
            as.character(fine_parents) %in%
              coarse_cells
          )
      }
    }
  }

  hierarchy_integrity <-
    hierarchy_overlap_n == 0L


  # ---------------------------------------------------------------------------
  # 7. Establish or validate source ownership lookup
  # ---------------------------------------------------------------------------

  if (is.null(lookup)) {
    lookup <- h3_compaction_lookup(
      source = source,
      compacted = compacted
    )
  } else {

    required_lookup_fields <- c(
      "source_h3",
      "source_resolution",
      "compact_h3",
      "compact_resolution",
      "relationship"
    )

    if (!is.data.frame(lookup)) {
      cli::cli_abort(
        "`lookup` must be a data frame produced by `h3_compaction_lookup()`."
      )
    }

    missing_lookup_fields <- setdiff(
      required_lookup_fields,
      names(lookup)
    )

    if (length(missing_lookup_fields) > 0L) {
      cli::cli_abort(
        paste0(
          "`lookup` is missing required field",
          if (length(missing_lookup_fields) == 1L) "" else "s",
          ": ",
          paste(missing_lookup_fields, collapse = ", "),
          "."
        )
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 8. Validate ownership integrity
  # ---------------------------------------------------------------------------

  lookup_source_unique <-
    !anyDuplicated(lookup$source_h3)

  lookup_source_complete <-
    setequal(
      lookup$source_h3,
      source
    )

  lookup_owner_retained <-
    all(
      lookup$compact_h3 %in%
        compacted
    )

  lookup_relationship_valid <-
    all(
      lookup$relationship %in%
        c("exact", "ancestor")
    )

  ownership_integrity <-
    lookup_source_unique &&
    lookup_source_complete &&
    lookup_owner_retained &&
    lookup_relationship_valid

  unmatched_source_n <- if (
    lookup_source_complete
  ) {
    0L
  } else {
    length(
      setdiff(
        source,
        lookup$source_h3
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 9. Summarise source ownership
  # ---------------------------------------------------------------------------

  exact_source_n <- sum(
    lookup$relationship == "exact"
  )

  ancestor_source_n <- sum(
    lookup$relationship == "ancestor"
  )

  ownership_groups <- interaction(
    lookup$relationship,
    lookup$compact_resolution,
    drop = TRUE,
    lex.order = TRUE
  )

  ownership_split <- split(
    seq_len(nrow(lookup)),
    ownership_groups
  )

  ownership <- lapply(
    ownership_split,
    function(rows) {
      tibble::tibble(
        relationship = lookup$relationship[rows[1L]],
        compact_resolution = as.integer(
          lookup$compact_resolution[rows[1L]]
        ),
        source_cell_n = length(rows)
      )
    }
  )

  ownership <- do.call(
    rbind,
    ownership
  )

  ownership <- tibble::as_tibble(
    ownership
  )

  ownership <- ownership[
    order(
      ownership$compact_resolution,
      ownership$relationship
    ),
  ]


  # ---------------------------------------------------------------------------
  # 10. Summarise compacted resolution mix
  # ---------------------------------------------------------------------------

  resolution_values <- sort(
    unique(compact_resolutions)
  )

  resolution <- tibble::tibble(
    compact_resolution = resolution_values,
    compact_cell_n = vapply(
      resolution_values,
      function(resolution_value) {
        sum(
          compact_resolutions ==
            resolution_value
        )
      },
      integer(1)
    )
  )


  # ---------------------------------------------------------------------------
  # 11. Calculate compaction metrics
  # ---------------------------------------------------------------------------

  source_cell_n <- length(source)
  compact_cell_n <- length(compacted)

  reduction_n <-
    source_cell_n -
    compact_cell_n

  reduction_pct <-
    100 * reduction_n /
    source_cell_n


  # ---------------------------------------------------------------------------
  # 12. Build QA summary
  # ---------------------------------------------------------------------------

  summary <- tibble::tibble(
    source_cell_n = source_cell_n,
    compact_cell_n = compact_cell_n,
    reduction_n = reduction_n,
    reduction_pct = reduction_pct,
    source_resolution = source_resolution,
    compact_resolution_min = min(compact_resolutions),
    compact_resolution_max = max(compact_resolutions),
    exact_source_n = exact_source_n,
    ancestor_source_n = ancestor_source_n,
    unmatched_source_n = unmatched_source_n,
    roundtrip_missing_n = length(missing_source),
    roundtrip_additional_n = length(additional_source),
    hierarchy_overlap_n = hierarchy_overlap_n,
    exact_reconstruction = exact_reconstruction,
    hierarchy_integrity = hierarchy_integrity,
    ownership_integrity = ownership_integrity
  )


  # ---------------------------------------------------------------------------
  # 13. Return structured QA result
  # ---------------------------------------------------------------------------

  result <- list(
    summary = summary,
    resolution = resolution,
    ownership = ownership
  )

  class(result) <- c(
    "h3_compaction_qa",
    "list"
  )

  result
}