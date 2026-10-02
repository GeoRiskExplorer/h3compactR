#' Create H3 compaction geometry
#'
#' @param h3 H3 indexes.
#' @param container Optional sf or sfc polygon.
#' @param clip Clip geometry to the container.
#' @param output_crs Output CRS.
#' @param analysis_crs Projected CRS used for clipping.
#' @return An sf object.
#' @export
h3_compaction_geometry <- function(
    h3,
    container = NULL,
    clip = FALSE,
    output_crs = 4326,
    analysis_crs = 7899
) {
  h3 <- validate_h3(h3)

  out <- h3jsr::cell_to_polygon(h3, simple = FALSE)
  names(out)[names(out) == "h3_address"] <- "h3"
  out$resolution <- h3jsr::get_res(out$h3)

  if (!clip) {
    return(sf::st_transform(out, output_crs))
  }

  if (is.null(container)) {
    cli::cli_abort("`container` is required when `clip = TRUE`.")
  }

  container_sf <- if (inherits(container, "sfc")) {
    sf::st_sf(container_id = 1L, geometry = container)
  } else if (inherits(container, "sf")) {
    container
  } else {
    cli::cli_abort("`container` must be an sf or sfc object.")
  }

  clipped <- suppressWarnings(
    sf::st_intersection(
      sf::st_transform(out, analysis_crs),
      sf::st_transform(sf::st_make_valid(container_sf), analysis_crs)
    )
  )

  clipped <- clipped[!sf::st_is_empty(clipped), ]
  sf::st_transform(clipped, output_crs)
}
