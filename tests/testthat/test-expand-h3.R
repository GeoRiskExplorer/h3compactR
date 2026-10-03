test_that("expand_h3 expands a valid H3 cell set", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  expanded <- expand_h3(
    x,
    rings = 1
  )

  expect_type(expanded, "character")
  expect_true(all(h3jsr::is_valid(expanded)))
  expect_gt(length(expanded), length(x))
  expect_true(all(x %in% expanded))
})


test_that("zero rings returns the original unique H3 cell set", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  result <- expand_h3(
    x,
    rings = 0
  )

  expect_setequal(result, x)
})


test_that("larger ring distances cannot remove cells", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  ring_1 <- expand_h3(
    x,
    rings = 1
  )

  ring_2 <- expand_h3(
    x,
    rings = 2
  )

  expect_true(all(x %in% ring_1))
  expect_true(all(ring_1 %in% ring_2))
  expect_gte(length(ring_2), length(ring_1))
})


test_that("expand_h3 preserves H3 resolution", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 8
  )

  expanded <- expand_h3(
    x,
    rings = 2
  )

  expect_true(
    all(
      h3jsr::get_res(expanded) == 8L
    )
  )
})


test_that("duplicate input cells do not duplicate output cells", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  duplicated_x <- c(
    x,
    x[1:5]
  )

  expanded <- expand_h3(
    duplicated_x,
    rings = 1
  )

  expect_false(
    anyDuplicated(expanded) > 0L
  )
})


test_that("empty H3 input returns an empty character vector", {

  result <- expand_h3(
    character(),
    rings = 1
  )

  expect_identical(
    result,
    character()
  )
})


test_that("expand_h3 rejects invalid H3 indexes", {

  expect_error(
    expand_h3(
      "not_an_h3_cell",
      rings = 1
    ),
    "invalid H3 indexes"
  )
})


test_that("expand_h3 rejects missing H3 indexes", {

  expect_error(
    expand_h3(
      c(NA_character_),
      rings = 1
    ),
    "missing or empty"
  )
})


test_that("expand_h3 rejects invalid ring distances", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  expect_error(
    expand_h3(x, rings = -1),
    "non-negative integer"
  )

  expect_error(
    expand_h3(x, rings = 1.5),
    "non-negative integer"
  )

  expect_error(
    expand_h3(x, rings = NA_real_),
    "non-negative integer"
  )

  expect_error(
    expand_h3(x, rings = c(1, 2)),
    "non-negative integer"
  )
})


test_that("expand_h3 rejects mixed-resolution input", {

  x8 <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  x9 <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 9
  )

  mixed <- c(
    x8[1],
    x9[1]
  )

  expect_error(
    expand_h3(
      mixed,
      rings = 1
    ),
    "single resolution"
  )
})


test_that("expand_h3 is deterministic", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "irregular",
    ],
    resolution = 8
  )

  a <- expand_h3(
    x,
    rings = 2
  )

  b <- expand_h3(
    x,
    rings = 2
  )

  expect_identical(a, b)
})