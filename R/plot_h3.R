#' Plot H3 cells
#'
#' Provides a simple cartographic representation of H3 cells, with optional
#' source polygon context.
#'
#' @param x A character vector of valid H3 cell indexes.
#' @param context Optional `sf` or `sfc` polygon geometry to display as
#'   geographic context.
#' @param ... Additional graphical parameters passed to the H3 cell plot.
#'
#' @return Invisibly returns the H3 cell geometry used for plotting.
#'
#' @details
#' H3 indexes are converted to polygon geometry for display. The H3 indexes
#' remain the analytical identifiers; the generated geometry is a
#' cartographic representation.
#'
#' This function is intended as the common plotting interface for
#' `h3compactR`. Additional package objects and visual QA representations may
#' be supported through this interface in future versions.
#'
#' @examples
#' cells <- h3_cover_polygon(
#'   toy_polygons[toy_polygons$geometry_id == "regular", ],
#'   resolution = 8
#' )
#'
#' plot_h3(
#'   cells,
#'   context = toy_polygons[
#'     toy_polygons$geometry_id == "regular",
#'   ]
#' )
#'
#' @references
#' H3 cell geometry access is provided through O'Brien's `h3jsr` package
#' (\doi{10.32614/CRAN.package.h3jsr}); spatial geometry uses `sf`.
#'
#' @export
plot_h3 <- function(x, context = NULL, ...) {

  # ---------------------------------------------------------------------------
  # Validate H3 indexes
  # ---------------------------------------------------------------------------

  if (!is.character(x)) {
    cli::cli_abort(
      "{.arg x} must be a character vector of H3 cell indexes."
    )
  }

  if (length(x) == 0L) {
    cli::cli_abort(
      "{.arg x} must contain at least one H3 cell index."
    )
  }

  if (anyNA(x)) {
    cli::cli_abort(
      "{.arg x} must not contain missing H3 cell indexes."
    )
  }

  if (!all(h3jsr::is_valid(x))) {
    cli::cli_abort(
      "{.arg x} contains invalid H3 cell indexes."
    )
  }

  # ---------------------------------------------------------------------------
  # Convert H3 indexes to geometry
  # ---------------------------------------------------------------------------

  h3_geometry <- h3jsr::cell_to_polygon(
    x,
    simple = FALSE
  )

  # ---------------------------------------------------------------------------
  # Validate optional context
  # ---------------------------------------------------------------------------

  if (!is.null(context)) {

    if (!inherits(context, c("sf", "sfc"))) {
      cli::cli_abort(
        "{.arg context} must be an {.cls sf} or {.cls sfc} object."
      )
    }

    if (is.na(sf::st_crs(context))) {
      cli::cli_abort(
        "{.arg context} must have a defined coordinate reference system."
      )
    }

    context <- sf::st_transform(
      context,
      sf::st_crs(h3_geometry)
    )
  }

  # ---------------------------------------------------------------------------
  # Plot
  # ---------------------------------------------------------------------------

  graphics::plot(
    sf::st_geometry(h3_geometry),
    ...
  )

  if (!is.null(context)) {
    graphics::plot(
      sf::st_geometry(context),
      add = TRUE,
      border = "black",
      lwd = 2
    )
  }

  invisible(h3_geometry)
}