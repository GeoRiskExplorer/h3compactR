test_that("compaction handles three irregular discontinuous polygons", {
  poly_1 <- sf::st_polygon(list(matrix(
    c(
      144.80, -37.92,
      144.90, -37.96,
      145.00, -37.91,
      145.04, -37.82,
      144.97, -37.76,
      144.86, -37.78,
      144.80, -37.92
    ),
    ncol = 2,
    byrow = TRUE
  )))

  poly_2 <- sf::st_polygon(list(matrix(
    c(
      145.18, -37.90,
      145.31, -37.88,
      145.34, -37.83,
      145.24, -37.80,
      145.17, -37.84,
      145.18, -37.90
    ),
    ncol = 2,
    byrow = TRUE
  )))

  poly_3 <- sf::st_polygon(list(matrix(
    c(
      144.96, -37.66,
      145.03, -37.68,
      145.06, -37.62,
      145.01, -37.58,
      144.95, -37.61,
      144.96, -37.66
    ),
    ncol = 2,
    byrow = TRUE
  )))

  polygons <- sf::st_sf(
    polygon_id = c("large_irregular", "medium_narrow", "small_compact"),
    geometry = sf::st_sfc(poly_1, poly_2, poly_3, crs = 4326)
  )

  expect_true(all(sf::st_is_valid(polygons)))
  expect_equal(nrow(polygons), 3L)

  source_h3 <- h3jsr::polygon_to_cells(
    polygons,
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  source_h3 <- validate_h3(source_h3)
  compacted_h3 <- compact_h3(source_h3)

  score <- score_h3_compaction(
    source_h3 = source_h3,
    compacted_h3 = compacted_h3,
    container = polygons,
    analysis_crs = 7899
  )

  expect_gt(length(source_h3), 0L)
  expect_lt(length(compacted_h3), length(source_h3))
  expect_equal(nrow(check_h3_hierarchy(compacted_h3)), 0L)
  expect_true(score$roundtrip_equal)
  expect_true(score$coverage_preserved)
  expect_false(score$coverage_extended)
  expect_equal(score$roundtrip_missing, 0L)
  expect_equal(score$roundtrip_additional, 0L)

  compacted_sf <- h3_compaction_geometry(
    h3 = compacted_h3,
    container = polygons,
    clip = TRUE,
    output_crs = 4326,
    analysis_crs = 7899
  )

  expect_s3_class(compacted_sf, "sf")
  expect_true(all(sf::st_is_valid(compacted_sf)))
  expect_false(any(sf::st_is_empty(compacted_sf)))
})


test_that("each irregular polygon compacts independently", {
  poly_1 <- sf::st_polygon(list(matrix(
    c(
      144.80, -37.92,
      144.90, -37.96,
      145.00, -37.91,
      145.04, -37.82,
      144.97, -37.76,
      144.86, -37.78,
      144.80, -37.92
    ),
    ncol = 2,
    byrow = TRUE
  )))

  poly_2 <- sf::st_polygon(list(matrix(
    c(
      145.18, -37.90,
      145.31, -37.88,
      145.34, -37.83,
      145.24, -37.80,
      145.17, -37.84,
      145.18, -37.90
    ),
    ncol = 2,
    byrow = TRUE
  )))

  poly_3 <- sf::st_polygon(list(matrix(
    c(
      144.96, -37.66,
      145.03, -37.68,
      145.06, -37.62,
      145.01, -37.58,
      144.95, -37.61,
      144.96, -37.66
    ),
    ncol = 2,
    byrow = TRUE
  )))

  polygons <- sf::st_sf(
    polygon_id = c("large_irregular", "medium_narrow", "small_compact"),
    geometry = sf::st_sfc(poly_1, poly_2, poly_3, crs = 4326)
  )

  per_polygon <- lapply(seq_len(nrow(polygons)), function(i) {
    source_h3 <- h3jsr::polygon_to_cells(
      polygons[i, ],
      res = 8,
      simple = TRUE
    ) |>
      unlist(use.names = FALSE) |>
      unique()

    compacted_h3 <- compact_h3(source_h3)

    tibble::tibble(
      polygon_id = polygons$polygon_id[[i]],
      source_cells = length(source_h3),
      compacted_cells = length(compacted_h3),
      hierarchy_overlaps = nrow(check_h3_hierarchy(compacted_h3)),
      roundtrip_equal = qa_h3_compaction(
        source_h3 = source_h3,
        compacted_h3 = compacted_h3,
        target_resolution = 8
      )$roundtrip_equal
    )
  }) |>
    dplyr::bind_rows()

  expect_equal(nrow(per_polygon), 3L)
  expect_true(all(per_polygon$source_cells > 0L))
  expect_true(all(per_polygon$compacted_cells > 0L))
  expect_true(all(per_polygon$compacted_cells <= per_polygon$source_cells))
  expect_true(all(per_polygon$hierarchy_overlaps == 0L))
  expect_true(all(per_polygon$roundtrip_equal))
})
