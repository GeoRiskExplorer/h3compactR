#' Check for parent-descendant overlap
#'
#' Detects cases where a parent H3 cell and one of its descendants are both
#' present in the same H3 set.
#'
#' @param h3 Character vector of H3 indexes.
#'
#' @return A tibble describing hierarchy overlaps.
#' @export
check_h3_hierarchy <- function(h3) {
  h3 <- validate_h3(h3)
  resolutions <- h3jsr::get_res(h3)

  results <- vector("list", length(h3))
  result_n <- 0L

  for (i in seq_along(h3)) {
    child_h3 <- h3[[i]]
    child_res <- resolutions[[i]]

    parent_resolutions <- sort(
      unique(resolutions[resolutions < child_res])
    )

    for (parent_res in parent_resolutions) {
      parent_h3 <- h3jsr::get_parent(
        h3_address = child_h3,
        res = parent_res,
        simple = TRUE
      ) |>
        unlist(use.names = FALSE)

      if (parent_h3 %in% h3) {
        result_n <- result_n + 1L

        results[[result_n]] <- tibble::tibble(
          parent_h3 = parent_h3,
          parent_resolution = parent_res,
          child_h3 = child_h3,
          child_resolution = child_res
        )
      }
    }
  }

  if (result_n == 0L) {
    return(
      tibble::tibble(
        parent_h3 = character(),
        parent_resolution = integer(),
        child_h3 = character(),
        child_resolution = integer()
      )
    )
  }

  dplyr::bind_rows(results[seq_len(result_n)])
}