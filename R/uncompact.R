#' Uncompact H3 cells
#'
#' @param h3 H3 indexes.
#' @param resolution Target resolution.
#' @return A character vector.
#' @export
uncompact_h3 <- function(h3, resolution) {
  h3 <- validate_h3(h3)

  if (length(resolution) != 1L || is.na(resolution)) {
    cli::cli_abort("`resolution` must be one non-missing value.")
  }

  h3jsr::uncompact(h3, res = resolution, simple = TRUE) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()
}
