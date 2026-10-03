test_that("adversarial polygon fixtures are valid", {

  x <- make_adversarial_polygons()

  expect_s3_class(
    x,
    "sf"
  )

  expect_equal(
    nrow(x),
    6L
  )

  expect_true(
    all(sf::st_is_valid(x))
  )

  expect_false(
    any(sf::st_is_empty(x))
  )

  expect_equal(
    sf::st_crs(x)$epsg,
    4326
  )
})


test_that("within coverage remains geometrically contained", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    cells <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "within"
    )

    if (length(cells) == 0L) {
      next
    }

    cells_sf <- h3jsr::cell_to_polygon(
      cells,
      simple = FALSE
    )

    domain <- sf::st_union(
      sf::st_geometry(polygon)
    )

    contained <- lengths(
      sf::st_within(
        sf::st_geometry(cells_sf),
        domain
      )
    ) > 0L

    expect_true(
      all(contained)
    )
  }
})


test_that("within coverage is a subset of center coverage", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    within <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "within"
    )

    center <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "center"
    )

    expect_true(
      all(within %in% center)
    )
  }
})


test_that("intersects coverage is complete against broad reference search", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    observed <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "intersects"
    )

    reference <- reference_intersects(
      polygon,
      resolution = 8
    )

    missing <- setdiff(
      reference,
      observed
    )

    extra <- setdiff(
      observed,
      reference
    )

    expect_length(
      missing,
      0L
    )

    expect_length(
      extra,
      0L
    )
  }
})


test_that("intersects coverage contains center coverage", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    center <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "center"
    )

    intersects <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "intersects"
    )

    expect_true(
      all(center %in% intersects)
    )
  }
})


test_that("adversarial coverage outputs contain valid H3 indexes", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    for (
      boundary in
        c("center", "within", "intersects")
    ) {

      cells <- h3_cover_polygon(
        polygon,
        resolution = 8,
        boundary = boundary
      )

      if (length(cells) == 0L) {
        next
      }

      expect_type(
        cells,
        "character"
      )

      expect_false(
        anyNA(cells)
      )

      expect_equal(
        length(cells),
        length(unique(cells))
      )

      expect_true(
        all(h3jsr::is_valid(cells))
      )

      expect_true(
        all(h3jsr::get_res(cells) == 8L)
      )
    }
  }
})


test_that("empty centre coverage is normalised to character(0)", {

  x <- make_adversarial_polygons()

  tiny <- x[
    x$geometry_id == "tiny",
  ]

  center <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "center"
  )

  within <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "within"
  )

  expect_type(
    center,
    "character"
  )

  expect_length(
    center,
    0L
  )

  expect_type(
    within,
    "character"
  )

  expect_length(
    within,
    0L
  )
})

test_that("intersects works when center coverage is empty", {

  x <- make_adversarial_polygons()

  tiny <- x[
    x$geometry_id == "tiny",
  ]

  center <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "center"
  )

  within <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "within"
  )

  intersects <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "intersects"
  )

  reference <- reference_intersects(
    tiny,
    resolution = 8
  )

  expect_length(
    center,
    0L
  )

  expect_length(
    within,
    0L
  )

  expect_gt(
    length(intersects),
    0L
  )

  expect_setequal(
    intersects,
    reference
  )
})