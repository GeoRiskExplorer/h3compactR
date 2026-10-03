test_that("overlap coverage returns valid H3 cells", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "complex",
  ]

  cells <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.5
  )

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
    all(
      h3jsr::is_valid(cells)
    )
  )

  expect_true(
    all(
      h3jsr::get_res(cells) == 8
    )
  )
})


test_that("higher overlap thresholds cannot add cells", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "complex",
  ]

  overlap_25 <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.25
  )

  overlap_50 <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.50
  )

  overlap_75 <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.75
  )

  expect_true(
    all(
      overlap_75 %in% overlap_50
    )
  )

  expect_true(
    all(
      overlap_50 %in% overlap_25
    )
  )

  expect_lte(
    length(overlap_75),
    length(overlap_50)
  )

  expect_lte(
    length(overlap_50),
    length(overlap_25)
  )
})

test_that("overlap coverage is monotonic across high thresholds", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    overlap_90 <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0.90
    )

    overlap_99 <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0.99
    )

    overlap_100 <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 1
    )

    expect_true(
      all(overlap_100 %in% overlap_99)
    )

    expect_true(
      all(overlap_99 %in% overlap_90)
    )

    expect_lte(
      length(overlap_100),
      length(overlap_99)
    )

    expect_lte(
      length(overlap_99),
      length(overlap_90)
    )
  }
})

test_that("overlap coverage is a subset of intersects coverage", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    overlap <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0.5
    )

    intersects <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "intersects"
    )

    expect_true(
      all(
        overlap %in% intersects
      )
    )
  }
})


test_that("overlap thresholds behave consistently on adversarial polygons", {

  x <- make_adversarial_polygons()

  for (id in x$geometry_id) {

    polygon <- x[
      x$geometry_id == id,
    ]

    overlap_10 <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0.10
    )

    overlap_50 <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0.50
    )

    overlap_90 <- h3_cover_polygon(
      polygon,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0.90
    )

    expect_true(
      all(
        overlap_90 %in% overlap_50
      )
    )

    expect_true(
      all(
        overlap_50 %in% overlap_10
      )
    )
  }
})


test_that("tiny polygons are handled by overlap coverage", {

  x <- make_adversarial_polygons()

  tiny <- x[
    x$geometry_id == "tiny",
  ]

  center <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "center"
  )

  intersects <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "intersects"
  )

  overlap <- h3_cover_polygon(
    tiny,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.001
  )

  expect_length(
    center,
    0L
  )

  expect_gt(
    length(intersects),
    0L
  )

  expect_true(
    all(
      overlap %in% intersects
    )
  )

  expect_type(
    overlap,
    "character"
  )

  expect_false(
    anyNA(overlap)
  )
})


test_that("overlap output is deterministic", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "irregular",
  ]

  first <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.5
  )

  second <- h3_cover_polygon(
    x,
    resolution = 8,
    boundary = "overlap",
    min_overlap = 0.5
  )

  expect_setequal(
    first,
    second
  )
})


test_that("min_overlap is required for overlap coverage", {

  x <- toy_polygons[
    1,
  ]

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap"
    ),
    "min_overlap"
  )
})


test_that("min_overlap must be greater than zero", {

  x <- toy_polygons[
    1,
  ]

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 0
    ),
    "min_overlap"
  )

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = -0.5
    ),
    "min_overlap"
  )
})


test_that("min_overlap cannot exceed one", {

  x <- toy_polygons[
    1,
  ]

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 1.01
    ),
    "min_overlap"
  )
})


test_that("min_overlap must be a single finite numeric value", {

  x <- toy_polygons[
    1,
  ]

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = NA_real_
    ),
    "min_overlap"
  )

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = Inf
    ),
    "min_overlap"
  )

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = c(0.25, 0.5)
    ),
    "min_overlap"
  )

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = "0.5"
    ),
    "min_overlap"
  )
})


test_that("min_overlap is rejected for non-overlap boundary rules", {

  x <- toy_polygons[
    1,
  ]

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "center",
      min_overlap = 0.5
    ),
    "only used"
  )

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "within",
      min_overlap = 0.5
    ),
    "only used"
  )

  expect_error(
    h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "intersects",
      min_overlap = 0.5
    ),
    "only used"
  )
})


test_that("all four boundary rules are accepted", {

  x <- toy_polygons[
    toy_polygons$geometry_id == "regular",
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

  intersects <- h3_cover_polygon(
    x,
    8,
    boundary = "intersects"
  )

  overlap <- h3_cover_polygon(
    x,
    8,
    boundary = "overlap",
    min_overlap = 0.5
  )

  expect_type(center, "character")
  expect_type(within, "character")
  expect_type(intersects, "character")
  expect_type(overlap, "character")
})

test_that("100 percent overlap is consistent with within coverage", {

  for (id in toy_polygons$geometry_id) {

    x <- toy_polygons[
      toy_polygons$geometry_id == id,
    ]

    overlap_100 <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "overlap",
      min_overlap = 1
    )

    within <- h3_cover_polygon(
      x,
      resolution = 8,
      boundary = "within"
    )

    expect_setequal(
      overlap_100,
      within
    )
  }
})