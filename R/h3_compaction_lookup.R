#' Build a source-to-compacted H3 lookup
#'
#' Creates a one-to-one lookup between authoritative source H3 cells and the
#' compacted H3 cells that represent them.
#'
#' Each source cell is assigned to exactly one compacted cell: either the same
#' H3 cell (`"exact"`) or a retained H3 ancestor (`"ancestor"`).
#'
#' @param source A character vector of authoritative source H3 cell indexes.
#'   Source cells must all have the same H3 resolution.
#' @param compacted A character vector of H3 cell indexes representing an exact
#'   hierarchical compaction of `source`. The compacted vector may contain
#'   multiple H3 resolutions.
#'
#' @details
#' The source H3 grid remains the authoritative analytical support. This
#' function records how those source cells are represented by a mixed-resolution
#' compacted H3 grid.
#'
#' Assignment is based entirely on the H3 hierarchy. No polygon intersection,
#' spatial overlay, centroid matching, or other geometric operation is used.
#'
#' A compacted cell may geometrically extend beyond the original study
#' footprint when displayed as a polygon. Such geometric area does not create
#' additional source membership. Only cells supplied in `source` receive lookup
#' rows.
#'
#' The function requires complete one-to-one source ownership. It errors if a
#' source cell cannot be assigned to a compacted cell, if more than one
#' compacted cell could own the same source cell, or if the compacted
#' representation contains cells finer than the source resolution.
#'
#' @return A tibble with one row per source H3 cell and five columns:
#'
#' \describe{
#'   \item{source_h3}{Authoritative source H3 cell index.}
#'   \item{source_resolution}{H3 resolution of the source cell.}
#'   \item{compact_h3}{Compacted H3 cell that owns the source cell.}
#'   \item{compact_resolution}{H3 resolution of the compacted owner.}
#'   \item{relationship}{Either `"exact"` or `"ancestor"`.}
#' }
#'
#' @examples
#' source <- h3_cover_polygon(toy_polygons[3, ], resolution = 8)
#' compacted <- compact_h3(source, min_resolution = 7)
#' lookup <- h3_compaction_lookup(source, compacted)
#' head(lookup)
#'
#' @references
#' H3 hierarchy and logical containment: <https://h3geo.org/docs/highlights/indexing/>.
#' R access is provided through O'Brien's `h3jsr` package
#' (\doi{10.32614/CRAN.package.h3jsr}).
#'
#' @export
h3_compaction_lookup <- function(
  source,
  compacted
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

  compact_resolutions <- h3jsr::get_res(compacted)

  if (any(compact_resolutions > source_resolution)) {
    cli::cli_abort(
      paste0(
        "`compacted` contains H3 cells finer than the authoritative ",
        "source resolution."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Prepare ownership vectors
  # ---------------------------------------------------------------------------

  n_source <- length(source)

  compact_owner <- rep(
    NA_character_,
    n_source
  )

  compact_owner_resolution <- rep(
    NA_integer_,
    n_source
  )

  relationship <- rep(
    NA_character_,
    n_source
  )


  # ---------------------------------------------------------------------------
  # 5. Match exact retained source cells
  # ---------------------------------------------------------------------------

  compact_at_source_resolution <- compacted[
    compact_resolutions == source_resolution
  ]

  if (length(compact_at_source_resolution) > 0L) {

    exact_match <- match(
      source,
      compact_at_source_resolution
    )

    exact_rows <- !is.na(exact_match)

    compact_owner[exact_rows] <- source[exact_rows]

    compact_owner_resolution[exact_rows] <-
      source_resolution

    relationship[exact_rows] <- "exact"
  }


  # ---------------------------------------------------------------------------
  # 6. Match unresolved cells to retained ancestors
  # ---------------------------------------------------------------------------

  ancestor_resolutions <- sort(
    unique(
      compact_resolutions[
        compact_resolutions < source_resolution
      ]
    ),
    decreasing = TRUE
  )

  for (target_resolution in ancestor_resolutions) {

    unresolved_rows <- which(
      is.na(compact_owner)
    )

    if (length(unresolved_rows) == 0L) {
      break
    }

    source_unresolved <- source[
      unresolved_rows
    ]

    candidate_parents <- h3jsr::get_parent(
      source_unresolved,
      res = target_resolution
    )

    candidate_parents <- as.character(
      candidate_parents
    )

    compact_at_target <- compacted[
      compact_resolutions == target_resolution
    ]

    matched <- candidate_parents %in%
      compact_at_target

    if (any(matched)) {

      matched_rows <- unresolved_rows[
        matched
      ]

      compact_owner[matched_rows] <-
        candidate_parents[matched]

      compact_owner_resolution[matched_rows] <-
        target_resolution

      relationship[matched_rows] <- "ancestor"
    }
  }


  # ---------------------------------------------------------------------------
  # 7. Require complete source ownership
  # ---------------------------------------------------------------------------

  unmatched <- is.na(compact_owner)

  if (any(unmatched)) {

    cli::cli_abort(
      paste0(
        "Compacted H3 ownership is incomplete: ",
        sum(unmatched),
        " source cell",
        if (sum(unmatched) == 1L) "" else "s",
        " could not be assigned to a retained compacted cell."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Validate assigned hierarchy relationships
  # ---------------------------------------------------------------------------

  ancestor_rows <- relationship == "ancestor"

  if (any(ancestor_rows)) {

    assigned_resolutions <- sort(
      unique(
        compact_owner_resolution[
          ancestor_rows
        ]
      ),
      decreasing = TRUE
    )

    for (target_resolution in assigned_resolutions) {

      rows <- which(
        ancestor_rows &
          compact_owner_resolution == target_resolution
      )

      expected_parent <- h3jsr::get_parent(
        source[rows],
        res = target_resolution
      )

      expected_parent <- as.character(
        expected_parent
      )

      if (!all(expected_parent == compact_owner[rows])) {
        cli::cli_abort(
          "Invalid source-to-compacted H3 hierarchy relationship detected."
        )
      }
    }
  }


  # ---------------------------------------------------------------------------
  # 9. Validate exact relationships
  # ---------------------------------------------------------------------------

  exact_rows <- relationship == "exact"

  if (
    any(exact_rows) &&
      !all(source[exact_rows] == compact_owner[exact_rows])
  ) {
    cli::cli_abort(
      "Invalid exact source-to-compacted H3 relationship detected."
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return one-row-per-source lookup
  # ---------------------------------------------------------------------------

  tibble::tibble(
    source_h3 = source,
    source_resolution = rep(
      source_resolution,
      n_source
    ),
    compact_h3 = compact_owner,
    compact_resolution = compact_owner_resolution,
    relationship = relationship
  )
}