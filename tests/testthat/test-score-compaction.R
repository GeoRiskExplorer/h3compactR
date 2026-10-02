test_that("score_h3_compaction reports exact round trip", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  score <- score_h3_compaction(
    source_h3 = children_h3,
    compacted_h3 = parent_h3
  )

  expect_true(score$roundtrip_equal)
  expect_equal(score$roundtrip_missing, 0L)
  expect_equal(score$roundtrip_additional, 0L)
  expect_equal(score$hierarchy_overlaps, 0L)
  expect_lt(score$compacted_cells, score$source_cells)
})


test_that("score_h3_compaction validates count preservation", {
  parent_h3 <- "8828308281fffff"

  children_h3 <- h3jsr::get_children(
    h3_address = parent_h3,
    res = 9,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  score <- score_h3_compaction(
    source_h3 = children_h3,
    compacted_h3 = parent_h3,
    source_count = 100,
    compacted_count = 100
  )

  expect_true(score$count_preserved)
  expect_equal(score$count_difference, 0)
})


test_that("score_h3_compaction requires both count totals", {
  expect_error(
    score_h3_compaction(
      source_h3 = "8828308281fffff",
      compacted_h3 = "8828308281fffff",
      source_count = 10
    ),
    "must be supplied together"
  )
})


test_that("score_h3_compaction calculates container coverage", {

  parent_h3 <- "8828308281fffff"

  geometry <- h3_compaction_geometry(parent_h3)

  score <- score_h3_compaction(
    source_h3 = parent_h3,
    compacted_h3 = parent_h3,
    container = geometry,
    analysis_crs = 3857
  )

  expect_true(score$coverage_preserved)
  expect_false(score$coverage_extended)
  expect_equal(score$coverage_pct, 100)

  expect_lt(
    score$gap_area_m2 / score$container_area_m2,
    1e-5
  )
})