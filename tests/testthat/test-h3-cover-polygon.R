test_that("h3_cover_polygon returns H3 cells", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "regular",
  ]

  cells <- h3_cover_polygon(
    x,
    resolution = 8
  )

  expect_type(cells, "character")
  expect_gt(length(cells), 0L)
  expect_false(anyNA(cells))
  expect_equal(length(cells), length(unique(cells)))

  expect_true(
    all(h3jsr::is_valid(cells))
  )
})


test_that("h3_cover_polygon returns requested resolution", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "regular",
  ]

  cells <- h3_cover_polygon(
    x,
    resolution = 8
  )

  expect_true(
    all(h3jsr::get_res(cells) == 8L)
  )
})


test_that("h3_cover_polygon is deterministic", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "irregular",
  ]

  a <- h3_cover_polygon(x, 8)
  b <- h3_cover_polygon(x, 8)

  expect_setequal(a, b)
})


test_that("h3_cover_polygon handles all toy polygon forms", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    cells <- h3_cover_polygon(
      x,
      resolution = 8
    )

    expect_gt(length(cells), 0L)

    expect_true(
      all(h3jsr::is_valid(cells))
    )
  }
})


test_that("h3_cover_polygon accepts projected input", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "regular",
  ]

  x_projected <- sf::st_transform(
    x,
    3857
  )

  geographic <- h3_cover_polygon(x, 8)
  projected <- h3_cover_polygon(x_projected, 8)

  expect_setequal(
    geographic,
    projected
  )
})


test_that("h3_cover_polygon rejects missing CRS", {

  x <- sf::st_geometry(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ]
  )

  sf::st_crs(x) <- NA

  expect_error(
    h3_cover_polygon(x, 8),
    "defined coordinate reference system"
  )
})


test_that("h3_cover_polygon rejects non-polygon geometry", {

  x <- sf::st_sfc(
    sf::st_point(c(145, -37.8)),
    crs = 4326
  )

  expect_error(
    h3_cover_polygon(x, 8),
    "POLYGON or MULTIPOLYGON"
  )
})


test_that("h3_cover_polygon validates resolution", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "regular",
  ]

  expect_error(
    h3_cover_polygon(x, -1),
    "between 0 and 15"
  )

  expect_error(
    h3_cover_polygon(x, 16),
    "between 0 and 15"
  )

  expect_error(
    h3_cover_polygon(x, 8.5),
    "between 0 and 15"
  )

  expect_error(
    h3_cover_polygon(x, c(7, 8)),
    "between 0 and 15"
  )
})

test_that("center remains the default boundary rule", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "irregular",
  ]

  default <- h3_cover_polygon(
    x,
    resolution = 8
  )

  explicit <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "center"
  )

  expect_setequal(default, explicit)
})


test_that("within coverage returns valid cells", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    cells <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "within"
    )

    expect_type(cells, "character")
    expect_false(anyNA(cells))
    expect_equal(
      length(cells),
      length(unique(cells))
    )

    if (length(cells) > 0L) {
      expect_true(
        all(h3jsr::is_valid(cells))
      )

      expect_true(
        all(h3jsr::get_res(cells) == 8L)
      )
    }
  }
})


test_that("within cells are geometrically contained", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    cells <- h3_cover_polygon(
      x,
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

    x_4326 <- sf::st_transform(
      sf::st_geometry(x),
      4326
    )

    domain <- sf::st_union(x_4326)

    contained <- lengths(
      sf::st_within(
        sf::st_geometry(cells_sf),
        domain
      )
    ) > 0L

    expect_true(all(contained))
  }
})


test_that("within coverage is no larger than candidate centre coverage", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    center <- h3_cover_polygon(
      x,
      8,
      boundary = "center"
    )

    within <- h3_cover_polygon(
      x,
      8,
      boundary = "within"
    )

    expect_lte(
      length(within),
      length(
        unique(
          unlist(
            h3jsr::get_disk(
              center,
              ring_size = 1,
              simple = TRUE
            ),
            use.names = FALSE
          )
        )
      )
    )
  }
})


test_that("within respects polygon holes", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "hole",
  ]

  cells <- h3_cover_polygon(
    x,
    8,
    boundary = "within"
  )

  cells_sf <- h3jsr::cell_to_polygon(
    cells,
    simple = FALSE
  )

  domain <- sf::st_union(
    sf::st_geometry(x)
  )

  expect_true(
    all(
      lengths(
        sf::st_within(
          sf::st_geometry(cells_sf),
          domain
        )
      ) > 0L
    )
  )
})


test_that("h3_cover_polygon validates boundary", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "regular",
  ]

  expect_error(
    h3_cover_polygon(
      x,
      8,
      boundary = "banana"
    ),
    "boundary"
  )
})

test_that("intersects coverage returns valid H3 cells", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    cells <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "intersects"
    )

    expect_type(cells, "character")
    expect_gt(length(cells), 0L)
    expect_false(anyNA(cells))

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
})


test_that("intersects cells geometrically intersect source polygon", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    cells <- h3_cover_polygon(
      x,
      8,
      boundary = "intersects"
    )

    cells_sf <- h3jsr::cell_to_polygon(
      cells,
      simple = FALSE
    )

    domain <- sf::st_union(
      sf::st_transform(
        sf::st_geometry(x),
        4326
      )
    )

    intersects <- lengths(
      sf::st_intersects(
        sf::st_geometry(cells_sf),
        domain
      )
    ) > 0L

    expect_true(all(intersects))
  }
})


test_that("within cells are a subset of intersects cells", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    within <- h3_cover_polygon(
      x,
      8,
      boundary = "within"
    )

    intersects <- h3_cover_polygon(
      x,
      8,
      boundary = "intersects"
    )

    expect_true(
      all(within %in% intersects)
    )

    expect_lte(
      length(within),
      length(intersects)
    )
  }
})


test_that("center cells are a subset of intersects cells", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    center <- h3_cover_polygon(
      x,
      8,
      boundary = "center"
    )

    intersects <- h3_cover_polygon(
      x,
      8,
      boundary = "intersects"
    )

    expect_true(
      all(center %in% intersects)
    )
  }
})

test_that("coverage rules show expected inclusion ordering", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    within <- h3_cover_polygon(
      x, 8,
      boundary = "within"
    )

    center <- h3_cover_polygon(
      x, 8,
      boundary = "center"
    )

    intersects <- h3_cover_polygon(
      x, 8,
      boundary = "intersects"
    )

    expect_lt(
      length(within),
      length(center)
    )

    expect_lt(
      length(center),
      length(intersects)
    )

    expect_true(
      all(within %in% intersects)
    )

    expect_true(
      all(center %in% intersects)
    )
  }
})