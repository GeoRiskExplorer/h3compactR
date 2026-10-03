# ---------------------------------------------------------------------------
# Internal helper: complete H3 cell containment
# ---------------------------------------------------------------------------

.cover_within <- function(
  cells,
  geometry_4326
) {

  cells <- unique(
    as.character(cells)
  )

  cells <- cells[
    !is.na(cells) &
      nzchar(cells)
  ]

  if (length(cells) == 0L) {
    return(character())
  }

  cell_geometry <- h3jsr::cell_to_polygon(
    cells,
    simple = FALSE
  )

  cell_geometry <- sf::st_transform(
    cell_geometry,
    4326
  )

  keep <- lengths(
    sf::st_within(
      sf::st_geometry(cell_geometry),
      geometry_4326
    )
  ) > 0L

  unique(
    as.character(
      cells[keep]
    )
  )
}