# =============================================================================
# H3COMPACTR — TOPOLOGICALLY SOUND MEMBERSHIP FIXTURE
# =============================================================================
#
# Purpose:
#   Deterministic polygon geography for testing H3-to-polygon membership.
#
# Includes:
#   - adjacent polygons sharing a boundary
#   - a polygon containing a genuine hole
#   - a separate polygon occupying that hole
#   - a multipart polygon
#
# The valid fixture contains no positive-area overlap between features.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Helpers
# -----------------------------------------------------------------------------

rectangle_ring <- function(xmin, ymin, xmax, ymax) {

  matrix(
    c(
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin
    ),
    ncol = 2,
    byrow = TRUE
  )
}


rectangle_polygon <- function(xmin, ymin, xmax, ymax) {

  sf::st_polygon(
    list(
      rectangle_ring(
        xmin,
        ymin,
        xmax,
        ymax
      )
    )
  )
}


# -----------------------------------------------------------------------------
# 2. Adjacent polygons
# -----------------------------------------------------------------------------

feature_100 <- rectangle_polygon(
  144.90, -37.90,
  145.00, -37.80
)

feature_200 <- rectangle_polygon(
  145.00, -37.90,
  145.10, -37.80
)


# -----------------------------------------------------------------------------
# 3. Donut polygon and enclave
# -----------------------------------------------------------------------------

feature_500 <- sf::st_polygon(
  list(
    rectangle_ring(
      144.90, -37.78,
      145.10, -37.66
    ),
    rectangle_ring(
      144.97, -37.74,
      145.03, -37.70
    )
  )
)

feature_600 <- rectangle_polygon(
  144.97, -37.74,
  145.03, -37.70
)


# -----------------------------------------------------------------------------
# 4. Multipart polygon
# -----------------------------------------------------------------------------

part_a <- rectangle_ring(
  144.91, -37.64,
  144.95, -37.60
)

part_b <- rectangle_ring(
  145.05, -37.64,
  145.09, -37.60
)

feature_400 <- sf::st_multipolygon(
  list(
    list(part_a),
    list(part_b)
  )
)


# -----------------------------------------------------------------------------
# 5. Valid membership geography
# -----------------------------------------------------------------------------

toy_membership_polygons <- sf::st_sf(
  feature_id = c(
    "100",
    "200",
    "500",
    "600",
    "400"
  ),
  geometry = sf::st_sfc(
    feature_100,
    feature_200,
    feature_500,
    feature_600,
    feature_400,
    crs = 4326
  )
)


# -----------------------------------------------------------------------------
# 6. Intentionally invalid overlapping geography
# -----------------------------------------------------------------------------

overlap_a <- rectangle_polygon(
  144.90, -37.90,
  145.02, -37.80
)

overlap_b <- rectangle_polygon(
  144.98, -37.90,
  145.10, -37.80
)

toy_membership_overlap <- sf::st_sf(
  feature_id = c(
    "A",
    "B"
  ),
  geometry = sf::st_sfc(
    overlap_a,
    overlap_b,
    crs = 4326
  )
)


# -----------------------------------------------------------------------------
# 7. Basic fixture validation
# -----------------------------------------------------------------------------

stopifnot(
  nrow(toy_membership_polygons) == 5L,
  all(sf::st_is_valid(toy_membership_polygons)),
  !anyDuplicated(toy_membership_polygons$feature_id),
  all(sf::st_is_valid(toy_membership_overlap))
)


# -----------------------------------------------------------------------------
# 8. Save package data
# -----------------------------------------------------------------------------

usethis::use_data(
  toy_membership_polygons,
  toy_membership_overlap,
  overwrite = TRUE
)