test_that("compact_h3 returns valid H3 indexes", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  result <- compact_h3(x)

  expect_type(result, "character")
  expect_true(all(h3jsr::is_valid(result)))
  expect_false(anyDuplicated(result) > 0L)
})


test_that("native exact compaction reduces a compactable H3 grid", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 9
  )

  result <- compact_h3(x)

  expect_lte(
    length(result),
    length(x)
  )
})


test_that("compact_h3 produces mixed resolutions where hierarchy permits", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  result <- compact_h3(
    x,
    min_resolution = 7
  )

  resolutions <- unique(
    h3jsr::get_res(result)
  )

  expect_true(
    all(resolutions >= 7L)
  )

  expect_true(
    all(resolutions <= 9L)
  )
})


test_that("minimum resolution prevents coarser output", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  result <- compact_h3(
    x,
    min_resolution = 8
  )

  resolutions <- h3jsr::get_res(
    result
  )

  expect_true(
    all(resolutions >= 8L)
  )

  expect_true(
    all(resolutions <= 9L)
  )
})


test_that("minimum resolution equal to source preserves source support", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 8
  )

  result <- compact_h3(
    x,
    min_resolution = 8
  )

  expect_setequal(
    result,
    x
  )
})


test_that("exact compaction reconstructs the source H3 set", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(
    x,
    min_resolution = 7
  )

  reconstructed <- h3jsr::uncompact(
    compacted,
    res = 9,
    simple = TRUE
  )

  reconstructed <- as.character(
    unlist(
      reconstructed,
      use.names = FALSE
    )
  )

  expect_setequal(
    reconstructed,
    x
  )
})


test_that("resolution-constrained compaction remains exact", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "irregular",
    ],
    resolution = 9
  )

  for (floor in c(7L, 8L, 9L)) {

    compacted <- compact_h3(
      x,
      min_resolution = floor
    )

    reconstructed <- h3jsr::uncompact(
      compacted,
      res = 9,
      simple = TRUE
    )

    reconstructed <- as.character(
      unlist(
        reconstructed,
        use.names = FALSE
      )
    )

    expect_setequal(
      reconstructed,
      x
    )

    expect_true(
      all(
        h3jsr::get_res(compacted) >= floor
      )
    )
  }
})


test_that("coarser permitted floors cannot require more compact cells", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  floor_9 <- compact_h3(
    x,
    min_resolution = 9
  )

  floor_8 <- compact_h3(
    x,
    min_resolution = 8
  )

  floor_7 <- compact_h3(
    x,
    min_resolution = 7
  )

  expect_lte(
    length(floor_8),
    length(floor_9)
  )

  expect_lte(
    length(floor_7),
    length(floor_8)
  )
})


test_that("compact_h3 is deterministic", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  a <- compact_h3(
    x,
    min_resolution = 7
  )

  b <- compact_h3(
    x,
    min_resolution = 7
  )

  expect_identical(
    a,
    b
  )
})


test_that("duplicate input H3 indexes are normalised", {

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

  result <- compact_h3(
    duplicated_x,
    min_resolution = 7
  )

  expect_false(
    anyDuplicated(result) > 0L
  )
})


test_that("empty H3 input returns an empty character vector", {

  expect_identical(
    compact_h3(character()),
    character()
  )
})


test_that("compact_h3 rejects invalid H3 indexes", {

  expect_error(
    compact_h3(
      "not_an_h3_cell"
    ),
    "invalid H3 indexes"
  )
})


test_that("compact_h3 rejects missing H3 indexes", {

  expect_error(
    compact_h3(
      NA_character_
    ),
    "missing or empty"
  )
})


test_that("compact_h3 rejects mixed-resolution source input", {

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
    compact_h3(mixed),
    "single source resolution"
  )
})


test_that("compact_h3 validates minimum resolution", {

  x <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  expect_error(
    compact_h3(
      x,
      min_resolution = -1
    ),
    "between 0 and 8"
  )

  expect_error(
    compact_h3(
      x,
      min_resolution = 9
    ),
    "between 0 and 8"
  )

  expect_error(
    compact_h3(
      x,
      min_resolution = 7.5
    ),
    "between 0 and 8"
  )

  expect_error(
    compact_h3(
      x,
      min_resolution = NA_real_
    ),
    "between 0 and 8"
  )

  expect_error(
    compact_h3(
      x,
      min_resolution = c(7, 8)
    ),
    "between 0 and 8"
  )
})