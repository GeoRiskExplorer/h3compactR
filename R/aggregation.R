# =============================================================================
# h3compactR
# Aggregation functions
# =============================================================================


#' Build an H3 aggregation lookup
#'
#' Creates a deterministic one-row-per-source-cell lookup between a
#' single-resolution source H3 support and a retained mixed-resolution
#' compact H3 representation.
#'
#' Each source H3 cell must resolve to exactly one retained compact cell,
#' either to itself or to a valid H3 ancestor.
#'
#' The lookup contains no analytical values. It provides the spatial
#' relationship required by downstream workflows to aggregate attributes from
#' the source H3 support to the compact representation.
#'
#' @param source_h3 Character vector of unique source H3 indexes. All source
#'   cells must have the same H3 resolution.
#' @param compacted_h3 Character vector of retained mixed-resolution compact
#'   H3 indexes.
#'
#' @return A tibble with one row per source H3 cell containing:
#'   `source_h3`, `compact_h3`, `compact_resolution`, and `match_type`.
#'
#' @export
build_h3_aggregation_lookup <- function(
    source_h3,
    compacted_h3
) {

  # ---------------------------------------------------------------------------
  # Validate inputs
  # ---------------------------------------------------------------------------

  source_h3 <- validate_h3(source_h3)

  compacted_h3 <- validate_h3(compacted_h3)

  if (length(source_h3) == 0L) {
    cli::cli_abort(
      "`source_h3` must contain at least one H3 index."
    )
  }

  if (length(compacted_h3) == 0L) {
    cli::cli_abort(
      "`compacted_h3` must contain at least one H3 index."
    )
  }


  # ---------------------------------------------------------------------------
  # Validate source resolution
  # ---------------------------------------------------------------------------

  source_resolution <- unique(
    h3jsr::get_res(source_h3)
  )

  if (length(source_resolution) != 1L) {
    cli::cli_abort(
      "`source_h3` must contain exactly one source resolution."
    )
  }


  # ---------------------------------------------------------------------------
  # Validate compact hierarchy
  # ---------------------------------------------------------------------------

  hierarchy_qa <- check_h3_hierarchy(
    compacted_h3
  )

  if (nrow(hierarchy_qa) > 0L) {
    cli::cli_abort(
      "`compacted_h3` contains parent-descendant hierarchy overlap."
    )
  }


  # ---------------------------------------------------------------------------
  # Match source H3 cells to retained compact cells
  # ---------------------------------------------------------------------------
  #
  # All H3 parent/self matching is delegated to the existing package matching
  # engine. This avoids maintaining multiple ancestry algorithms.

  lookup <- match_h3_to_compacted(
    source_h3 = source_h3,
    compacted_h3 = compacted_h3
  )


  # ---------------------------------------------------------------------------
  # Validate complete one-to-one source assignment
  # ---------------------------------------------------------------------------

  if (nrow(lookup) != length(source_h3)) {
    cli::cli_abort(
      "Aggregation lookup row count does not equal the source H3 count."
    )
  }

  unmatched_n <- sum(
    is.na(lookup$matched_h3)
  )

  if (unmatched_n > 0L) {
    cli::cli_abort(
      "{unmatched_n} source H3 cell(s) could not be matched."
    )
  }

  if (anyDuplicated(lookup$source_h3)) {
    cli::cli_abort(
      "Aggregation lookup contains duplicated source H3 assignments."
    )
  }

  missing_source <- setdiff(
    source_h3,
    lookup$source_h3
  )

  if (length(missing_source) > 0L) {
    cli::cli_abort(
      "{length(missing_source)} source H3 cell(s) are missing from the lookup."
    )
  }


  # ---------------------------------------------------------------------------
  # Validate compact assignments
  # ---------------------------------------------------------------------------

  unexpected_compact <- setdiff(
    unique(lookup$matched_h3),
    compacted_h3
  )

  if (length(unexpected_compact) > 0L) {
    cli::cli_abort(
      "Aggregation lookup contains unexpected compact H3 indexes."
    )
  }

  recorded_resolution <- h3jsr::get_res(
    lookup$matched_h3
  )

  if (any(
    recorded_resolution != lookup$matched_resolution
  )) {
    cli::cli_abort(
      "Recorded compact resolution does not agree with the H3 index."
    )
  }


  # ---------------------------------------------------------------------------
  # Return clean aggregation lookup
  # ---------------------------------------------------------------------------

  tibble::tibble(
    source_h3 = lookup$source_h3,
    compact_h3 = lookup$matched_h3,
    compact_resolution = lookup$matched_resolution,
    match_type = ifelse(
      lookup$source_h3 == lookup$matched_h3,
      "self",
      "ancestor"
    )
  )
}


#' Aggregate counts to compact H3 cells
#'
#' Aggregates a numeric count field from source H3 records to a retained
#' mixed-resolution compact H3 representation.
#'
#' Source-to-compact assignment is performed using
#' [h3compactR::build_h3_aggregation_lookup()].
#'
#' This is a generic convenience function for additive counts. Derived
#' statistics such as rates, ratios, expected values, or relative risks should
#' be recalculated by the downstream analytical workflow rather than summed or
#' averaged by this function.
#'
#' @param data A data frame containing source H3 indexes and a numeric count.
#' @param source_h3 Source H3 column in `data`.
#' @param compacted_h3 Character vector of retained mixed-resolution compact
#'   H3 indexes.
#' @param count Numeric count column in `data`.
#'
#' @return A tibble containing one row per retained compact H3 cell represented
#'   in the input data, with `compact_h3`, `compact_resolution`, and `count`.
#'
#' @export
aggregate_h3_counts <- function(
    data,
    source_h3,
    compacted_h3,
    count
) {

  # ---------------------------------------------------------------------------
  # Capture input columns
  # ---------------------------------------------------------------------------

  source_h3 <- rlang::ensym(
    source_h3
  )

  count <- rlang::ensym(
    count
  )

  source_values <- dplyr::pull(
    data,
    !!source_h3
  )

  count_values <- dplyr::pull(
    data,
    !!count
  )


  # ---------------------------------------------------------------------------
  # Validate count values
  # ---------------------------------------------------------------------------

  if (!is.numeric(count_values)) {
    cli::cli_abort(
      "`count` must be numeric."
    )
  }

  if (length(source_values) != length(count_values)) {
    cli::cli_abort(
      "Source H3 and count columns must have the same number of values."
    )
  }


  # ---------------------------------------------------------------------------
  # Build aggregation lookup
  # ---------------------------------------------------------------------------
  #
  # Multiple data rows may occupy the same source H3 cell. Build the lookup
  # once for the distinct source H3 support, then join it back to the records.

  unique_source_h3 <- unique(
    source_values
  )

  lookup <- build_h3_aggregation_lookup(
    source_h3 = unique_source_h3,
    compacted_h3 = compacted_h3
  )


  # ---------------------------------------------------------------------------
  # Attach compact assignment to source records
  # ---------------------------------------------------------------------------

  aggregation_data <- tibble::tibble(
    source_h3 = source_values,
    count_value = count_values
  ) |>
    dplyr::left_join(
      lookup,
      by = "source_h3"
    )

  unmatched_n <- sum(
    is.na(aggregation_data$compact_h3)
  )

  if (unmatched_n > 0L) {
    cli::cli_abort(
      "{unmatched_n} input row(s) could not be assigned to a compact H3 cell."
    )
  }


  # ---------------------------------------------------------------------------
  # Aggregate additive counts
  # ---------------------------------------------------------------------------

  aggregation_data |>
    dplyr::group_by(
      compact_h3,
      compact_resolution
    ) |>
    dplyr::summarise(
      count = sum(
        count_value,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}