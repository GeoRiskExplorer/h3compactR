#' Match source H3 cells to a compacted H3 grid
#'
#' Matches each source cell to an exact retained cell where available,
#' otherwise to the nearest retained ancestor.
#'
#' @param source_h3 Source H3 indexes.
#' @param compacted_h3 Valid mixed-resolution compacted H3 indexes.
#'
#' @return A tibble containing source and matched H3 indexes.
#' @export
match_h3_to_compacted <- function(
    source_h3,
    compacted_h3
) {
  source_h3 <- validate_h3(
    source_h3,
    allow_duplicates = TRUE
  )

  compacted_h3 <- validate_h3(compacted_h3)

  hierarchy_qa <- check_h3_hierarchy(compacted_h3)

  if (nrow(hierarchy_qa) > 0L) {
    cli::cli_abort(
      "The compacted grid contains parent-descendant overlap."
    )
  }

  compact_resolutions <- h3jsr::get_res(compacted_h3)

  matched_h3 <- rep(NA_character_, length(source_h3))
  matched_resolution <- rep(NA_integer_, length(source_h3))
  match_type <- rep(NA_character_, length(source_h3))

  for (i in seq_along(source_h3)) {
    source_cell <- source_h3[[i]]
    source_resolution <- h3jsr::get_res(source_cell)

    candidate_resolutions <- sort(
      unique(c(
        source_resolution,
        compact_resolutions[
          compact_resolutions <= source_resolution
        ]
      )),
      decreasing = TRUE
    )

    for (candidate_resolution in candidate_resolutions) {
      candidate_h3 <- if (
        candidate_resolution == source_resolution
      ) {
        source_cell
      } else {
        h3jsr::get_parent(
          h3_address = source_cell,
          res = candidate_resolution,
          simple = TRUE
        ) |>
          unlist(use.names = FALSE)
      }

      if (candidate_h3 %in% compacted_h3) {
        matched_h3[[i]] <- candidate_h3
        matched_resolution[[i]] <- candidate_resolution
        match_type[[i]] <- if (
          candidate_resolution == source_resolution
        ) {
          "exact"
        } else {
          "ancestor"
        }

        break
      }
    }
  }

  tibble::tibble(
    source_h3 = source_h3,
    source_resolution = h3jsr::get_res(source_h3),
    matched_h3 = matched_h3,
    matched_resolution = matched_resolution,
    match_type = match_type
  )
}