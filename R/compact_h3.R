#' Compact H3 cells with an optional resolution floor
#'
#' Compacts a single-resolution H3 cell set using the established H3
#' hierarchical compaction operation.
#'
#' An optional minimum resolution can be supplied to prevent the compacted
#' representation from containing cells coarser than a specified H3
#' resolution.
#'
#' @param x A character vector of H3 cell indexes. All input cells must be at
#'   the same H3 resolution.
#' @param min_resolution Optional integer defining the coarsest H3 resolution
#'   permitted in the compacted result. For example, with source cells at
#'   resolution 10 and `min_resolution = 8`, the output may contain resolution
#'   8, 9, and 10 cells, but never resolution 7 or coarser. When `NULL`, native
#'   H3 compaction is returned without a package-imposed resolution floor.
#'
#' @details
#' `compact_h3()` performs exact hierarchical H3 compaction. It does not use
#' partial child coverage, polygon intersection, geometric clipping, or relaxed
#' compaction rules.
#'
#' The underlying compaction operation is provided by [h3jsr::compact()].
#'
#' When `min_resolution` is supplied, native H3 compaction is performed first.
#' Any resulting cells coarser than the requested minimum resolution are then
#' expanded back to `min_resolution` using the established H3 hierarchy.
#'
#' Consequently, the resolution constraint does not introduce additional H3
#' membership. Uncompacting the result to the original source resolution should
#' reconstruct the original H3 cell set exactly.
#'
#' Input is restricted to a single source resolution so that the authoritative
#' analytical support is explicit. Mixed-resolution H3 is an output of
#' compaction rather than an accepted source representation.
#'
#' @return A character vector containing unique compacted H3 cell indexes.
#'
#' @references
#' H3 hierarchical indexing and compaction: <https://h3geo.org/docs/highlights/indexing/>.
#' R access is provided through O'Brien's `h3jsr` package
#' (\doi{10.32614/CRAN.package.h3jsr}).
#'
#' @export
compact_h3 <- function(
  x,
  min_resolution = NULL
) {

  # ---------------------------------------------------------------------------
  # 1. Validate H3 input
  # ---------------------------------------------------------------------------

  if (!is.character(x)) {
    cli::cli_abort(
      "`x` must be a character vector of H3 cell indexes."
    )
  }

  if (length(x) == 0L) {
    return(character())
  }

  if (anyNA(x) || any(!nzchar(x))) {
    cli::cli_abort(
      "`x` must not contain missing or empty H3 indexes."
    )
  }

  valid <- h3jsr::is_valid(x)

  if (
    anyNA(valid) ||
      !all(valid)
  ) {
    cli::cli_abort(
      "`x` contains invalid H3 indexes."
    )
  }

  x <- sort(
    unique(x)
  )

  # ---------------------------------------------------------------------------
  # 2. Establish authoritative source resolution
  # ---------------------------------------------------------------------------

  source_resolutions <- h3jsr::get_res(
    x
  )

  unique_source_resolutions <- unique(
    source_resolutions
  )

  if (length(unique_source_resolutions) != 1L) {
    cli::cli_abort(
      "`x` must contain H3 cells at a single source resolution."
    )
  }

  source_resolution <- as.integer(
    unique_source_resolutions[[1L]]
  )

  # ---------------------------------------------------------------------------
  # 3. Validate minimum permitted resolution
  # ---------------------------------------------------------------------------

  if (!is.null(min_resolution)) {

    if (
      length(min_resolution) != 1L ||
        !is.numeric(min_resolution) ||
        is.na(min_resolution) ||
        !is.finite(min_resolution) ||
        min_resolution != as.integer(min_resolution) ||
        min_resolution < 0L ||
        min_resolution > source_resolution
    ) {
      cli::cli_abort(
        paste0(
          "`min_resolution` must be a single integer between 0 and ",
          source_resolution,
          " for source H3 cells at resolution ",
          source_resolution,
          "."
        )
      )
    }

    min_resolution <- as.integer(
      min_resolution
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Perform native exact H3 compaction
  # ---------------------------------------------------------------------------

  compacted <- h3jsr::compact(
    x,
    simple = TRUE
  )

  compacted <- as.character(
    unlist(
      compacted,
      use.names = FALSE
    )
  )

  compacted <- compacted[
    !is.na(compacted) &
      nzchar(compacted)
  ]

  compacted <- sort(
    unique(compacted)
  )

  if (length(compacted) == 0L) {
    return(character())
  }

  # ---------------------------------------------------------------------------
  # 5. Apply optional resolution floor
  # ---------------------------------------------------------------------------

  if (!is.null(min_resolution)) {

    compacted_resolution <- h3jsr::get_res(
      compacted
    )

    too_coarse <- compacted_resolution < min_resolution

    if (any(too_coarse)) {

      coarse_cells <- compacted[
        too_coarse
      ]

      retained_cells <- compacted[
        !too_coarse
      ]

      floor_cells <- h3jsr::uncompact(
        coarse_cells,
        res = min_resolution,
        simple = TRUE
      )

      floor_cells <- as.character(
        unlist(
          floor_cells,
          use.names = FALSE
        )
      )

      floor_cells <- floor_cells[
        !is.na(floor_cells) &
          nzchar(floor_cells)
      ]

      compacted <- sort(
        unique(
          c(
            retained_cells,
            floor_cells
          )
        )
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Validate compacted H3 output
  # ---------------------------------------------------------------------------

  compacted_valid <- h3jsr::is_valid(
    compacted
  )

  if (
    anyNA(compacted_valid) ||
      !all(compacted_valid)
  ) {
    cli::cli_abort(
      "H3 compaction returned an invalid H3 index."
    )
  }

  compacted_resolution <- h3jsr::get_res(
    compacted
  )

  if (
    any(compacted_resolution > source_resolution)
  ) {
    cli::cli_abort(
      "H3 compaction produced cells finer than the source resolution."
    )
  }

  if (
    !is.null(min_resolution) &&
      any(compacted_resolution < min_resolution)
  ) {
    cli::cli_abort(
      "H3 compaction produced cells coarser than `min_resolution`."
    )
  }

  # ---------------------------------------------------------------------------
  # 7. Validate exact source reconstruction
  # ---------------------------------------------------------------------------

  reconstructed <- h3jsr::uncompact(
    compacted,
    res = source_resolution,
    simple = TRUE
  )

  reconstructed <- as.character(
    unlist(
      reconstructed,
      use.names = FALSE
    )
  )

  reconstructed <- sort(
    unique(
      reconstructed[
        !is.na(reconstructed) &
          nzchar(reconstructed)
      ]
    )
  )

  if (!setequal(x, reconstructed)) {
    cli::cli_abort(
      paste0(
        "Exact H3 compaction failed source reconstruction. ",
        "The compacted result does not reproduce the original source H3 set."
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 8. Return exact mixed-resolution H3 representation
  # ---------------------------------------------------------------------------

  compacted
}