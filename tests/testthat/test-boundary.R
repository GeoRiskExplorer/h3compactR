test_that("boundary refinement preserves hierarchy", {
  container <- sf::st_as_sfc(
    sf::st_bbox(
      c(
        xmin = 144.80,
        ymin = -37.95,
        xmax = 145.15,
        ymax = -37.70
      ),
      crs = sf::st_crs(4326)
    )
  )

  source_h3 <- h3jsr::polygon_to_cells(
    container,
    res = 7,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  compacted_h3 <- compact_h3(source_h3)

  result <- refine_h3_boundary(
    source_h3,
    compacted_h3,
    container,
    boundary_resolution = 8
  )

  expect_equal(nrow(check_h3_hierarchy(result$h3)), 0L)
  expect_lte(
    result$qa$remaining_gap_area_m2,
    result$qa$initial_gap_area_m2
  )
})
