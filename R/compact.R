#' Compact H3 cells
#'
#' Compacts complete H3 sibling sets into their parent cells.
#'
#' The underlying H3 compaction operation is provided by
#' [h3jsr::compact()]. Additional validation, deterministic ordering and
#' hierarchy QA are provided by `h3compactR`.
#'
#' @param h3 Character vector of H3 indexes.
#' @param simple Return only the compacted indexes when `TRUE`.
#'
#' @return A character vector or structured compaction result.
#'
#' @references
#' O'Brien, L. h3jsr: Access Uber's H3 Library.
#'
#' @export
compact_h3 <- function(h3, simple = TRUE) {
  input_h3 <- validate_h3(h3)

  compacted_h3 <- h3jsr::compact(
    input_h3,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()

  hierarchy_qa <- check_h3_hierarchy(compacted_h3)

  if (nrow(hierarchy_qa) > 0L) {
    cli::cli_abort(
      "Compaction produced parent-descendant hierarchy overlap."
    )
  }

  if (simple) {
    return(compacted_h3)
  }

  list(
    input_h3 = input_h3,
    compacted_h3 = compacted_h3,
    hierarchy_qa = hierarchy_qa,
    resolution_summary = summarise_h3_resolution(compacted_h3)
  )
}