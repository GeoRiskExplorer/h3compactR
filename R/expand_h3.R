#' Expand an H3 cell set by neighbouring cells
#'
#' Expands an existing set of H3 cells by including neighbouring cells within
#' a specified H3 grid distance.
#'
#' This operation is independent of polygon coverage. It can be applied to any
#' valid H3 cell set and does not alter the resolution of the supplied cells.
#'
#' @param x A character vector of H3 cell indexes.
#' @param rings A single non-negative integer defining the H3 grid distance
#'   used for expansion. `rings = 0` returns the original cell set,
#'   `rings = 1` includes immediately neighbouring cells, and larger values
#'   progressively expand the cell set.
#'
#' @details
#' Expansion uses the H3 grid neighbourhood around each supplied cell. Results
#' from all input cells are combined and duplicate H3 indexes are removed.
#'
#' The function expects all supplied H3 cells to have the same resolution.
#' Mixed-resolution input is rejected because grid-distance neighbourhoods are
#' defined relative to cells at a common H3 resolution.
#'
#' Expansion does not test whether newly included cells intersect an original
#' polygon or other source geometry. It is a purely H3-based neighbourhood
#' operation.
#'
#' @return A character vector containing unique H3 cell indexes.
#'
#' @examples
#' cells <- h3_cover_polygon(toy_polygons[1, ], resolution = 7)
#' expanded <- expand_h3(cells, rings = 1)
#' c(source = length(cells), expanded = length(expanded))
#'
#' @references
#' H3 grid hierarchy and neighbourhood operations: <https://h3geo.org/>.
#' R access is provided through O'Brien's `h3jsr` package
#' (\doi{10.32614/CRAN.package.h3jsr}).
#'
#' @export
expand_h3 <- function(
  x,
  rings = 1L
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

  x <- unique(x)

  # ---------------------------------------------------------------------------
  # 2. Validate ring distance
  # ---------------------------------------------------------------------------

  if (
    length(rings) != 1L ||
      !is.numeric(rings) ||
      is.na(rings) ||
      !is.finite(rings) ||
      rings != as.integer(rings) ||
      rings < 0
  ) {
    cli::cli_abort(
      "`rings` must be a single non-negative integer."
    )
  }

  rings <- as.integer(rings)

  # ---------------------------------------------------------------------------
  # 3. Validate common H3 resolution
  # ---------------------------------------------------------------------------

  resolutions <- h3jsr::get_res(x)

  if (length(unique(resolutions)) != 1L) {
    cli::cli_abort(
      "`x` must contain H3 cells at a single resolution."
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Return unchanged cell set for zero expansion
  # ---------------------------------------------------------------------------

  if (rings == 0L) {
    return(x)
  }

  # ---------------------------------------------------------------------------
  # 5. Expand H3 neighbourhood
  # ---------------------------------------------------------------------------

  expanded <- lapply(
    x,
    function(cell) {
      h3jsr::get_disk(
        cell,
        ring_size = rings,
        simple = TRUE
      )
    }
  )

  expanded <- as.character(
    unlist(
      expanded,
      use.names = FALSE
    )
  )

  expanded <- expanded[
    !is.na(expanded) &
      nzchar(expanded)
  ]

  expanded <- unique(
    expanded
  )

  # ---------------------------------------------------------------------------
  # 6. Validate result
  # ---------------------------------------------------------------------------

  if (length(expanded) > 0L) {

    valid_expanded <- h3jsr::is_valid(
      expanded
    )

    if (
      anyNA(valid_expanded) ||
        !all(valid_expanded)
    ) {
      cli::cli_abort(
        "H3 neighbourhood expansion returned an invalid H3 index."
      )
    }

    expanded_resolution <- h3jsr::get_res(
      expanded
    )

    if (
      any(
        expanded_resolution != unique(resolutions)
      )
    ) {
      cli::cli_abort(
        "H3 neighbourhood expansion changed the H3 resolution."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Return expanded H3 cell set
  # ---------------------------------------------------------------------------

  expanded
}