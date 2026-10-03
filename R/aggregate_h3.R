#' Aggregate attributes to compacted H3 cells
#'
#' Aggregates explicitly selected additive attributes and collapses explicitly
#' selected categorical attributes from authoritative source H3 cells to their
#' compacted H3 owners using an H3 compaction lookup.
#'
#' @param data A data frame containing one row per authoritative source H3 cell.
#' @param lookup A lookup produced by [h3_compaction_lookup()].
#' @param sum Optional character vector naming additive numeric fields to
#'   aggregate by summation.
#' @param collapse Optional character vector naming source attributes whose
#'   unique non-missing values should be retained for each compacted H3 owner.
#'   Multiple unique values are returned as a deterministic semicolon-separated
#'   character string. For each collapsed field, a companion `<field>_n` column
#'   reports the number of unique non-missing source values represented by each
#'   compacted H3 owner.
#' @param h3_col Name of the H3 index column in `data`. Defaults to `"h3"`.
#'
#' @details
#' `aggregate_h3()` treats the source H3 grid as the authoritative analytical
#' support. Values are transferred to compacted H3 cells through the explicit
#' source-to-owner relationship in `lookup`; compacted polygon geometry is not
#' used for attribute assignment.
#'
#' Fields supplied to `sum` and `collapse` are handled differently and must be
#' selected explicitly. The function does not infer aggregation rules from
#' field names or data types.
#'
#' `sum` is intended for additive numeric quantities. Missing additive values
#' are rejected and additive totals are checked for conservation.
#'
#' `collapse` is intended for categorical identifiers or descriptive source
#' attributes. Duplicate values within a compact owner are removed before
#' values are collapsed. Missing values are ignored when non-missing values are
#' present. If all source values for a compact owner are missing, the compact
#' value is `NA_character_`.
#'
#' Collapsed values are a compact representation of source-cell provenance.
#' They do not create a new analytical category and do not replace the
#' authoritative source-to-compact relationship in `lookup`.
#'
#' Derived measures such as rates, proportions, ratios, relative risks, or
#' averages should generally be recalculated from their aggregated components
#' after aggregation rather than aggregated directly.
#'
#' @return A tibble containing one row per compacted H3 owner, with
#'   `compact_h3`, `compact_resolution`, `source_cell_n`, requested additive
#'   fields, requested collapsed attributes, and a `<field>_n` provenance count
#'   for every collapsed attribute.
#'
#' @export
aggregate_h3 <- function(
  data,
  lookup,
  sum = NULL,
  collapse = NULL,
  h3_col = "h3"
) {

  # ---------------------------------------------------------------------------
  # 1. Validate source data
  # ---------------------------------------------------------------------------

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  if (nrow(data) == 0L) {
    cli::cli_abort("`data` must contain at least one source H3 row.")
  }

  if (
    !is.character(h3_col) ||
      length(h3_col) != 1L ||
      is.na(h3_col) ||
      !nzchar(h3_col)
  ) {
    cli::cli_abort(
      "`h3_col` must be the name of one H3 column in `data`."
    )
  }

  if (!h3_col %in% names(data)) {
    cli::cli_abort(
      paste0(
        "H3 column `",
        h3_col,
        "` was not found in `data`."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Validate requested fields
  # ---------------------------------------------------------------------------

  validate_field_names <- function(fields, argument) {

    if (is.null(fields)) {
      return(character())
    }

    if (
      !is.character(fields) ||
        anyNA(fields) ||
        any(!nzchar(fields))
    ) {
      cli::cli_abort(
        paste0(
          "`",
          argument,
          "` must be NULL or a character vector of field names."
        )
      )
    }

    unique(fields)
  }

  sum_fields <- validate_field_names(sum, "sum")
  collapse_fields <- validate_field_names(collapse, "collapse")

  if (
    length(sum_fields) == 0L &&
      length(collapse_fields) == 0L
  ) {
    cli::cli_abort(
      "At least one field must be supplied to `sum` or `collapse`."
    )
  }

  overlap_fields <- intersect(
    sum_fields,
    collapse_fields
  )

  if (length(overlap_fields) > 0L) {
    cli::cli_abort(
      paste0(
        "Fields cannot be requested in both `sum` and `collapse`: ",
        paste(overlap_fields, collapse = ", "),
        "."
      )
    )
  }

  requested_fields <- c(
    sum_fields,
    collapse_fields
  )

  missing_fields <- setdiff(
    requested_fields,
    names(data)
  )

  if (length(missing_fields) > 0L) {
    cli::cli_abort(
      paste0(
        "Requested aggregation field",
        if (length(missing_fields) == 1L) "" else "s",
        " not found in `data`: ",
        paste(missing_fields, collapse = ", "),
        "."
      )
    )
  }

  if (h3_col %in% requested_fields) {
    cli::cli_abort(
      "`h3_col` cannot also be requested in `sum` or `collapse`."
    )
  }

  reserved_output_fields <- c(
    "compact_h3",
    "compact_resolution",
    "source_cell_n"
  )

  reserved_requested <- intersect(
    requested_fields,
    reserved_output_fields
  )

  if (length(reserved_requested) > 0L) {
    cli::cli_abort(
      paste0(
        "Requested fields conflict with reserved output field",
        if (length(reserved_requested) == 1L) "" else "s",
        ": ",
        paste(reserved_requested, collapse = ", "),
        "."
      )
    )
  }

  collapse_count_fields <- paste0(
    collapse_fields,
    "_n"
  )

  count_name_conflicts <- intersect(
    collapse_count_fields,
    names(data)
  )

  if (length(count_name_conflicts) > 0L) {
    cli::cli_abort(
      paste0(
        "Collapsed provenance count field",
        if (length(count_name_conflicts) == 1L) "" else "s",
        " would overwrite existing field",
        if (length(count_name_conflicts) == 1L) "" else "s",
        ": ",
        paste(count_name_conflicts, collapse = ", "),
        ". Rename the existing field before aggregation."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Validate additive fields
  # ---------------------------------------------------------------------------

  if (length(sum_fields) > 0L) {

    non_numeric <- sum_fields[
      !vapply(
        data[sum_fields],
        is.numeric,
        logical(1)
      )
    ]

    if (length(non_numeric) > 0L) {
      cli::cli_abort(
        paste0(
          "Additive aggregation requires numeric fields. Non-numeric field",
          if (length(non_numeric) == 1L) "" else "s",
          ": ",
          paste(non_numeric, collapse = ", "),
          "."
        )
      )
    }

    fields_with_na <- sum_fields[
      vapply(
        data[sum_fields],
        anyNA,
        logical(1)
      )
    ]

    if (length(fields_with_na) > 0L) {
      cli::cli_abort(
        paste0(
          "Missing values were found in additive field",
          if (length(fields_with_na) == 1L) "" else "s",
          ": ",
          paste(fields_with_na, collapse = ", "),
          ". Missing values must be resolved explicitly before aggregation."
        )
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 4. Validate source H3 identifiers
  # ---------------------------------------------------------------------------

  source_h3 <- data[[h3_col]]

  if (!is.character(source_h3)) {
    cli::cli_abort(
      paste0(
        "`",
        h3_col,
        "` must be a character vector of H3 indexes."
      )
    )
  }

  if (anyNA(source_h3) || any(!nzchar(source_h3))) {
    cli::cli_abort(
      paste0(
        "`",
        h3_col,
        "` must not contain missing or empty H3 indexes."
      )
    )
  }

  source_valid <- h3jsr::is_valid(source_h3)

  if (anyNA(source_valid) || !all(source_valid)) {
    cli::cli_abort(
      paste0(
        "`",
        h3_col,
        "` contains invalid H3 indexes."
      )
    )
  }

  if (anyDuplicated(source_h3)) {
    cli::cli_abort(
      paste0(
        "`data` must contain exactly one row per authoritative source H3 cell. ",
        "Duplicate values were found in `",
        h3_col,
        "`."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Validate lookup structure
  # ---------------------------------------------------------------------------

  if (!is.data.frame(lookup)) {
    cli::cli_abort(
      "`lookup` must be a data frame produced by `h3_compaction_lookup()`."
    )
  }

  required_lookup_fields <- c(
    "source_h3",
    "source_resolution",
    "compact_h3",
    "compact_resolution",
    "relationship"
  )

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

  if (anyDuplicated(lookup$source_h3)) {
    cli::cli_abort(
      "`lookup` must contain exactly one ownership row per source H3 cell."
    )
  }

  if (
    anyNA(lookup$source_h3) ||
      anyNA(lookup$compact_h3) ||
      anyNA(lookup$compact_resolution)
  ) {
    cli::cli_abort(
      "`lookup` contains missing H3 ownership information."
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Require exact agreement between data and lookup source support
  # ---------------------------------------------------------------------------

  data_not_lookup <- setdiff(
    source_h3,
    lookup$source_h3
  )

  lookup_not_data <- setdiff(
    lookup$source_h3,
    source_h3
  )

  if (
    length(data_not_lookup) > 0L ||
      length(lookup_not_data) > 0L
  ) {
    cli::cli_abort(
      paste0(
        "`data` and `lookup` do not describe the same authoritative source ",
        "H3 support. Data-only cells: ",
        length(data_not_lookup),
        "; lookup-only cells: ",
        length(lookup_not_data),
        "."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Match source rows to compact ownership
  # ---------------------------------------------------------------------------

  lookup_position <- match(
    source_h3,
    lookup$source_h3
  )

  compact_owner <- lookup$compact_h3[lookup_position]
  compact_resolution <- lookup$compact_resolution[lookup_position]


  # ---------------------------------------------------------------------------
  # 8. Establish compact groups
  # ---------------------------------------------------------------------------

  compact_cells <- sort(
    unique(compact_owner)
  )

  compact_position <- match(
    compact_cells,
    compact_owner
  )

  compact_resolution_out <- compact_resolution[compact_position]

  source_cell_n <- as.integer(
    table(
      factor(
        compact_owner,
        levels = compact_cells
      )
    )
  )


  # ---------------------------------------------------------------------------
  # 9. Aggregate additive fields
  # ---------------------------------------------------------------------------

  aggregated_values <- vector(
    "list",
    length(sum_fields)
  )

  names(aggregated_values) <- sum_fields

  for (field in sum_fields) {

    group_sum <- rowsum(
      data[[field]],
      group = compact_owner,
      reorder = FALSE
    )

    group_h3 <- rownames(group_sum)

    output_position <- match(
      compact_cells,
      group_h3
    )

    if (anyNA(output_position)) {
      cli::cli_abort(
        paste0(
          "Internal aggregation error while aligning compact H3 owners for field `",
          field,
          "`."
        )
      )
    }

    aggregated_values[[field]] <- as.numeric(
      group_sum[output_position, 1L]
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Collapse unique categorical attributes
  # ---------------------------------------------------------------------------

  collapsed_values <- vector(
    "list",
    length(collapse_fields)
  )

  collapsed_counts <- vector(
    "list",
    length(collapse_fields)
  )

  names(collapsed_values) <- collapse_fields
  names(collapsed_counts) <- collapse_fields

  for (field in collapse_fields) {

    values <- data[[field]]

    collapsed_field <- vapply(
      compact_cells,
      function(owner) {

        owner_values <- values[compact_owner == owner]
        owner_values <- owner_values[!is.na(owner_values)]

        if (length(owner_values) == 0L) {
          return(NA_character_)
        }

        owner_values <- sort(
          unique(
            as.character(owner_values)
          )
        )

        paste(
          owner_values,
          collapse = "; "
        )
      },
      character(1)
    )

    collapsed_count <- vapply(
      compact_cells,
      function(owner) {

        owner_values <- values[compact_owner == owner]
        owner_values <- owner_values[!is.na(owner_values)]

        length(
          unique(
            as.character(owner_values)
          )
        )
      },
      integer(1)
    )

    collapsed_values[[field]] <- unname(collapsed_field)
    collapsed_counts[[field]] <- unname(collapsed_count)
  }


  # ---------------------------------------------------------------------------
  # 11. Build compact attribute table
  # ---------------------------------------------------------------------------

  result <- tibble::tibble(
    compact_h3 = compact_cells,
    compact_resolution = compact_resolution_out,
    source_cell_n = source_cell_n
  )

  for (field in sum_fields) {
    result[[field]] <- aggregated_values[[field]]
  }

  for (field in collapse_fields) {
    result[[field]] <- collapsed_values[[field]]
    result[[paste0(field, "_n")]] <- collapsed_counts[[field]]
  }


  # ---------------------------------------------------------------------------
  # 12. Validate source-cell conservation
  # ---------------------------------------------------------------------------

  if (base::sum(result$source_cell_n) != nrow(data)) {
    cli::cli_abort(
      "Source-cell conservation failed during H3 aggregation."
    )
  }


  # ---------------------------------------------------------------------------
  # 13. Validate additive-field conservation
  # ---------------------------------------------------------------------------

  for (field in sum_fields) {

    source_total <- base::sum(data[[field]])
    compact_total <- base::sum(result[[field]])

    conserved <- isTRUE(
      all.equal(
        source_total,
        compact_total,
        tolerance = sqrt(.Machine$double.eps),
        check.attributes = FALSE
      )
    )

    if (!conserved) {
      cli::cli_abort(
        paste0(
          "Additive-field conservation failed for `",
          field,
          "`. Source total = ",
          format(source_total, digits = 15),
          "; compact total = ",
          format(compact_total, digits = 15),
          "."
        )
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 14. Validate collapsed attribute representation
  # ---------------------------------------------------------------------------

  for (field in collapse_fields) {

    for (i in seq_along(compact_cells)) {

      owner <- compact_cells[i]
      source_values <- data[[field]][compact_owner == owner]
      source_values <- source_values[!is.na(source_values)]

      expected <- if (length(source_values) == 0L) {
        NA_character_
      } else {
        paste(
          sort(
            unique(
              as.character(source_values)
            )
          ),
          collapse = "; "
        )
      }

      expected_n <- length(
        unique(
          as.character(source_values)
        )
      )

      actual <- result[[field]][i]
      actual_n <- result[[paste0(field, "_n")]][i]

      if (!identical(actual, expected)) {
        cli::cli_abort(
          paste0(
            "Collapsed attribute validation failed for field `",
            field,
            "` and compact H3 `",
            owner,
            "`."
          )
        )
      }

      if (!identical(actual_n, expected_n)) {
        cli::cli_abort(
          paste0(
            "Collapsed provenance count validation failed for field `",
            field,
            "` and compact H3 `",
            owner,
            "`."
          )
        )
      }
    }
  }


  # ---------------------------------------------------------------------------
  # 15. Return compact attributes
  # ---------------------------------------------------------------------------

  result
}
