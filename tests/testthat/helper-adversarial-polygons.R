# Adversarial polygon fixtures for H3 coverage testing.
#
# These geometries deliberately exercise boundary cases that are not
# represented by the normal package toy data.

make_adversarial_polygons <- function() {

  # ---------------------------------------------------------------------------
  # Thin horizontal sliver
  # ---------------------------------------------------------------------------

  thin_sliver <- sf::st_polygon(
    list(
      matrix(
        c(
          144.80, -37.800,
          145.20, -37.800,
          145.20, -37.795,
          144.80, -37.795,
          144.80, -37.800
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Very small polygon
  #
  # Deliberately small enough that centre-based coverage may contain few or
  # no cells at the test resolution while intersection coverage can still
  # legitimately contain cells.
  # ---------------------------------------------------------------------------

  tiny <- sf::st_polygon(
    list(
      matrix(
        c(
          144.9990, -37.8010,
          145.0010, -37.8010,
          145.0010, -37.7990,
          144.9990, -37.7990,
          144.9990, -37.8010
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Deep concavity
  # ---------------------------------------------------------------------------

  deep_concavity <- sf::st_polygon(
    list(
      matrix(
        c(
          144.80, -37.90,
          145.20, -37.90,
          145.20, -37.70,
          145.08, -37.70,
          145.08, -37.84,
          144.92, -37.84,
          144.92, -37.70,
          144.80, -37.70,
          144.80, -37.90
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Narrow corridor / dumbbell
  # ---------------------------------------------------------------------------

  narrow_corridor <- sf::st_polygon(
    list(
      matrix(
        c(
          144.80, -37.88,
          144.92, -37.88,
          144.92, -37.805,
          145.08, -37.805,
          145.08, -37.88,
          145.20, -37.88,
          145.20, -37.72,
          145.08, -37.72,
          145.08, -37.795,
          144.92, -37.795,
          144.92, -37.72,
          144.80, -37.72,
          144.80, -37.88
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Polygon with narrow internal hole
  # ---------------------------------------------------------------------------

  outer <- matrix(
    c(
      144.80, -37.90,
      145.20, -37.90,
      145.20, -37.70,
      144.80, -37.70,
      144.80, -37.90
    ),
    ncol = 2,
    byrow = TRUE
  )

  inner <- matrix(
    c(
      144.985, -37.88,
      145.015, -37.88,
      145.015, -37.72,
      144.985, -37.72,
      144.985, -37.88
    ),
    ncol = 2,
    byrow = TRUE
  )

  narrow_hole <- sf::st_polygon(
    list(
      outer,
      inner
    )
  )

  # ---------------------------------------------------------------------------
  # Multipart / discontinuous polygon
  # ---------------------------------------------------------------------------

  part_a <- list(
    matrix(
      c(
        144.80, -37.86,
        144.90, -37.86,
        144.90, -37.76,
        144.80, -37.76,
        144.80, -37.86
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  part_b <- list(
    matrix(
      c(
        145.10, -37.86,
        145.20, -37.86,
        145.20, -37.76,
        145.10, -37.76,
        145.10, -37.86
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  multipart <- sf::st_multipolygon(
    list(
      part_a,
      part_b
    )
  )

  # ---------------------------------------------------------------------------
  # Assemble
  # ---------------------------------------------------------------------------

  sf::st_sf(
    geometry_id = c(
      "thin_sliver",
      "tiny",
      "deep_concavity",
      "narrow_corridor",
      "narrow_hole",
      "multipart"
    ),
    geometry = sf::st_sfc(
      thin_sliver,
      tiny,
      deep_concavity,
      narrow_corridor,
      narrow_hole,
      multipart,
      crs = 4326
    )
  )
}