# =============================================================================
# H3COMPACTR — TOPOLOGICALLY SOUND MEMBERSHIP FIXTURE
# =============================================================================
#
# Valid analytical polygon geography for testing H3 membership.
#
# Includes:
#   - adjacent polygons with a shared boundary
#   - a true donut polygon
#   - a separate polygon occupying the donut hole
#   - a multipart polygon
#
# No feature interiors overlap.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Geometry helpers
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
# 2. Adjacent polygons — valid shared boundary
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
# 3. Donut polygon with genuine interior hole
# -----------------------------------------------------------------------------

outer_500 <- rectangle_ring(
  144.90, -37.78,
  145.10, -37.66
)

hole_500 <- rectangle_ring(
  144.97, -37.74,
  145.03, -37.70
)

# Reverse the hole ring so its orientation is opposite the exterior ring.
hole_500 <- hole_500[nrow(hole_500):1, ]

feature_500 <- sf::st_polygon(
  list(
    outer_500,
    hole_500
  )
)


# -----------------------------------------------------------------------------
# 4. Enclave occupying the donut hole
# -----------------------------------------------------------------------------

feature_600 <- rectangle_polygon(
  144.97, -37.74,
  145.03, -37.70
)


# -----------------------------------------------------------------------------
# 5. Multipart polygon
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
# 6. Build valid sf geography
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
# 7. Validate fixture
# -----------------------------------------------------------------------------

stopifnot(
  nrow(toy_membership_polygons) == 5L,
  identical(
    toy_membership_polygons$feature_id,
    c("100", "200", "500", "600", "400")
  ),
  all(sf::st_is_valid(toy_membership_polygons)),
  !anyDuplicated(toy_membership_polygons$feature_id)
)


# -----------------------------------------------------------------------------
# 8. Save package data
# -----------------------------------------------------------------------------

usethis::use_data(
  toy_membership_polygons,
  overwrite = TRUE
)