#' Validate H3 indexes
#'
#' @param h3 Character vector of H3 indexes.
#' @param allow_na Whether missing indexes are allowed.
#' @param allow_duplicates Whether duplicated indexes are allowed.
#'
#' @return The validated H3 indexes as a character vector.
#' @export
validate_h3 <- function(
    h3,
    allow_na = FALSE,
    allow_duplicates = FALSE
) {
  h3 <- as.character(h3)

  if (!allow_na && anyNA(h3)) {
    cli::cli_abort("Missing H3 indexes detected.")
  }

  check_h3 <- h3[!is.na(h3)]

  if (length(check_h3) > 0L && !all(h3jsr::is_valid(check_h3))) {
    cli::cli_abort("Invalid H3 indexes detected.")
  }

  if (!allow_duplicates && anyDuplicated(check_h3)) {
    cli::cli_abort("Duplicated H3 indexes detected.")
  }

  h3
}


#' Summarise H3 resolutions
#'
#' @param h3 Character vector of H3 indexes.
#'
#' @return A tibble containing resolution counts and percentages.
#' @export
summarise_h3_resolution <- function(h3) {
  h3 <- validate_h3(h3)

  tibble::tibble(
    h3 = h3,
    resolution = h3jsr::get_res(h3)
  ) |>
    dplyr::count(.data$resolution, name = "cell_count") |>
    dplyr::mutate(
      cell_pct = round(
        100 * .data$cell_count / sum(.data$cell_count),
        2
      )
    )
}