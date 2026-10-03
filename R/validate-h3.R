# Internal H3 validation helpers
#
# These helpers provide consistent validation for core package functions.
# They are intentionally not exported.


# -----------------------------------------------------------------------------
# 1. Validate an H3 character vector
# -----------------------------------------------------------------------------

.validate_h3_vector <- function(
  x,
  arg = "x",
  allow_empty = FALSE
) {

  if (!is.character(x)) {
    cli::cli_abort(
      paste0(
        "`",
        arg,
        "` must be a character vector of H3 cell indexes."
      )
    )
  }

  if (!allow_empty && length(x) == 0L) {
    cli::cli_abort(
      paste0(
        "`",
        arg,
        "` must contain at least one H3 cell."
      )
    )
  }

  if (length(x) == 0L) {
    return(character())
  }

  if (anyNA(x) || any(!nzchar(x))) {
    cli::cli_abort(
      paste0(
        "`",
        arg,
        "` must not contain missing or empty H3 indexes."
      )
    )
  }

  valid <- h3jsr::is_valid(x)

  if (anyNA(valid) || !all(valid)) {
    cli::cli_abort(
      paste0(
        "`",
        arg,
        "` contains invalid H3 indexes."
      )
    )
  }

  sort(unique(x))
}


# -----------------------------------------------------------------------------
# 2. Require a single H3 resolution
# -----------------------------------------------------------------------------

.single_h3_resolution <- function(
  x,
  arg = "x"
) {

  resolutions <- as.integer(
    h3jsr::get_res(x)
  )

  unique_resolutions <- unique(
    resolutions
  )

  if (length(unique_resolutions) != 1L) {
    cli::cli_abort(
      paste0(
        "`",
        arg,
        "` must contain H3 cells at a single resolution."
      )
    )
  }

  as.integer(
    unique_resolutions[[1L]]
  )
}