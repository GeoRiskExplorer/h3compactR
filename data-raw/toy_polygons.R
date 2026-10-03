# Build deterministic toy polygons used by h3compactR examples and tests.
#
# These geometries are intentionally synthetic. They provide reproducible
# polygon shapes for testing H3 coverage behaviour without depending on
# external spatial datasets.

library(sf)

make_polygon <- function(coords) {
  st_polygon(list(as.matrix(coords)))
}

regular <- make_polygon(
  data.frame(
    x = c(144.90, 145.00, 145.00, 144.90, 144.90),
    y = c(-37.85, -37.85, -37.75, -37.75, -37.85)
  )
)

irregular <- make_polygon(
  data.frame(
    x = c(
      145.05, 145.14, 145.17, 145.13,
      145.16, 145.08, 145.03, 145.05
    ),
    y = c(
      -37.86, -37.84, -37.79, -37.77,
      -37.72, -37.73, -37.80, -37.86
    )
  )
)

complex <- make_polygon(
  data.frame(
    x = c(
      144.72, 144.78, 144.81, 144.86,
      144.84, 144.91, 144.88, 144.82,
      144.79, 144.74, 144.70, 144.72
    ),
    y = c(
      -37.75, -37.77, -37.74, -37.76,
      -37.71, -37.68, -37.64, -37.67,
      -37.63, -37.66, -37.70, -37.75
    )
  )
)

outer <- rbind(
  c(145.20, -37.85),
  c(145.32, -37.85),
  c(145.32, -37.73),
  c(145.20, -37.73),
  c(145.20, -37.85)
)

inner <- rbind(
  c(145.24, -37.81),
  c(145.24, -37.77),
  c(145.28, -37.77),
  c(145.28, -37.81),
  c(145.24, -37.81)
)

hole <- st_polygon(
  list(
    outer,
    inner
  )
)

toy_polygons <- st_sf(
  geometry_id = c(
    "regular",
    "irregular",
    "complex",
    "hole"
  ),
  geometry = st_sfc(
    regular,
    irregular,
    complex,
    hole,
    crs = 4326
  )
)

stopifnot(
  nrow(toy_polygons) == 4L,
  all(st_is_valid(toy_polygons)),
  !any(st_is_empty(toy_polygons)),
  identical(
    toy_polygons$geometry_id,
    c("regular", "irregular", "complex", "hole")
  )
)

usethis::use_data(
  toy_polygons,
  overwrite = TRUE
)