# Internal H3 candidate generation
#
# Builds an H3 candidate set for geometric boundary predicates.
#
# Standard centre-based polygon coverage is used where available. Additional
# seed cells are derived from points guaranteed to lie on the source polygon,
# allowing polygons smaller than an H3 cell to generate candidates.
#
# Point-on-surface operations are performed in a projected CRS because planar
# geometric operations should not be performed directly on longitude/latitude
# coordinates.
#
# Candidate cells are expanded by one H3 grid disk before exact geometric
# predicates are applied.
#
# This helper is intentionally not exported.
#
# @keywords internal
.cover_candidates <- function(
  center_cells,
  geometry_4326,
  resolution
) {

  # ---------------------------------------------------------------------------
  # Normalise centre cells
  # ---------------------------------------------------------------------------

  center_cells <- unique(
    as.character(center_cells)
  )

  center_cells <- center_cells[
    !is.na(center_cells)
  ]

  # ---------------------------------------------------------------------------
  # Determine a local projected CRS for point-on-surface operations
  #
  # A local azimuthal equidistant projection is centred on the source
  # geometry. This avoids hard-coding a regional projected CRS and allows
  # the helper to operate on polygons from different parts of the world.
  # ---------------------------------------------------------------------------

  bbox <- sf::st_bbox(
    geometry_4326
  )

  lon_0 <- mean(
    c(
      bbox[["xmin"]],
      bbox[["xmax"]]
    )
  )

  lat_0 <- mean(
    c(
      bbox[["ymin"]],
      bbox[["ymax"]]
    )
  )

  local_crs <- paste(
    "+proj=aeqd",
    paste0("+lat_0=", lat_0),
    paste0("+lon_0=", lon_0),
    "+datum=WGS84",
    "+units=m",
    "+no_defs"
  )

  geometry_projected <- sf::st_transform(
    geometry_4326,
    local_crs
  )

  # ---------------------------------------------------------------------------
  # Create polygon-derived seed points
  # ---------------------------------------------------------------------------

  seed_points <- sf::st_point_on_surface(
    geometry_projected
  )

  seed_points <- sf::st_transform(
    seed_points,
    4326
  )

  # ---------------------------------------------------------------------------
  # Convert seed points to H3 cells
  # ---------------------------------------------------------------------------

  seed_cells <- h3jsr::point_to_cell(
    seed_points,
    res = resolution
  )

  seed_cells <- unique(
    as.character(
      unlist(
        seed_cells,
        use.names = FALSE
      )
    )
  )

  seed_cells <- seed_cells[
    !is.na(seed_cells)
  ]

  # ---------------------------------------------------------------------------
  # Combine centre and polygon-derived seeds
  # ---------------------------------------------------------------------------

  seeds <- unique(
    c(
      center_cells,
      seed_cells
    )
  )

  if (length(seeds) == 0L) {
    return(character())
  }

  valid <- h3jsr::is_valid(
    seeds
  )

  if (
    anyNA(valid) ||
      !all(valid)
  ) {
    cli::cli_abort(
      "Internal H3 candidate generation received an invalid H3 index."
    )
  }

  # ---------------------------------------------------------------------------
  # Expand each seed by one H3 grid disk
  # ---------------------------------------------------------------------------

  neighbours <- lapply(
    seeds,
    function(cell) {

      h3jsr::get_disk(
        cell,
        ring_size = 1,
        simple = TRUE
      )
    }
  )

  candidates <- unique(
    as.character(
      unlist(
        neighbours,
        use.names = FALSE
      )
    )
  )

  candidates <- candidates[
    !is.na(candidates)
  ]

  unique(candidates)
}