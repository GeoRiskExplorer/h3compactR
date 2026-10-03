reference_intersects <- function(
  x,
  resolution,
  buffer_m = 50000
) {

  # ---------------------------------------------------------------------------
  # Construct a deliberately broad search region
  # ---------------------------------------------------------------------------

  x_projected <- sf::st_transform(
    x,
    3857
  )

  search_area <- sf::st_buffer(
    sf::st_union(x_projected),
    dist = buffer_m
  )

  search_area <- sf::st_transform(
    search_area,
    4326
  )

  # ---------------------------------------------------------------------------
  # Generate H3 cells covering the broad search area
  # ---------------------------------------------------------------------------

  search_cells <- h3jsr::polygon_to_cells(
    search_area,
    res = resolution,
    simple = TRUE
  )

  search_cells <- unique(
    as.character(
      unlist(
        search_cells,
        use.names = FALSE
      )
    )
  )

  if (length(search_cells) == 0L) {
    return(character())
  }

  # ---------------------------------------------------------------------------
  # Convert broad candidate universe to polygons
  # ---------------------------------------------------------------------------

  search_geometry <- h3jsr::cell_to_polygon(
    search_cells,
    simple = FALSE
  )

  search_geometry <- sf::st_transform(
    search_geometry,
    4326
  )

  domain <- sf::st_union(
    sf::st_transform(
      sf::st_geometry(x),
      4326
    )
  )

  # ---------------------------------------------------------------------------
  # Exact intersection predicate
  # ---------------------------------------------------------------------------

  keep <- lengths(
    sf::st_intersects(
      sf::st_geometry(search_geometry),
      domain
    )
  ) > 0L

  search_cells[keep]
}