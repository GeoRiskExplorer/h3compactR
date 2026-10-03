test_that("toy_polygons has expected structure", {

  expect_s3_class(toy_polygons, "sf")
  expect_equal(nrow(toy_polygons), 4L)

  expect_identical(
    toy_polygons$geometry_id,
    c("regular", "irregular", "complex", "hole")
  )

  expect_true(all(sf::st_is_valid(toy_polygons)))
  expect_false(any(sf::st_is_empty(toy_polygons)))

  expect_equal(
    sf::st_crs(toy_polygons)$epsg,
    4326
  )
})


test_that("toy_polygons contains an interior hole", {

  hole_geometry <- sf::st_geometry(
    toy_polygons[toy_polygons$geometry_id == "hole", ]
  )[[1]]

  expect_equal(
    length(hole_geometry),
    2L
  )
})