test_that("qa_h3_compaction reports exact compaction correctly", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  qa <- qa_h3_compaction(
    source,
    compacted
  )

  expect_s3_class(
    qa,
    "h3_compaction_qa"
  )

  expect_true(
    qa$summary$exact_reconstruction
  )

  expect_true(
    qa$summary$hierarchy_integrity
  )

  expect_true(
    qa$summary$ownership_integrity
  )

  expect_equal(
    qa$summary$roundtrip_missing_n,
    0L
  )

  expect_equal(
    qa$summary$roundtrip_additional_n,
    0L
  )

  expect_equal(
    qa$summary$unmatched_source_n,
    0L
  )
})


test_that("QA reports expected complex example metrics", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  qa <- qa_h3_compaction(
    source,
    compacted
  )

  expect_equal(
    qa$summary$source_cell_n,
    1285L
  )

  expect_equal(
    qa$summary$compact_cell_n,
    235L
  )

  expect_equal(
    qa$summary$exact_source_n,
    165L
  )

  expect_equal(
    qa$summary$ancestor_source_n,
    1120L
  )

  expect_equal(
    qa$summary$source_resolution,
    9L
  )

  expect_equal(
    qa$summary$compact_resolution_min,
    7L
  )

  expect_equal(
    qa$summary$compact_resolution_max,
    9L
  )
})


test_that("ownership totals equal authoritative source cells", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  qa <- qa_h3_compaction(
    source,
    compacted
  )

  expect_equal(
    base::sum(qa$ownership$source_cell_n),
    length(source)
  )

  expect_equal(
    qa$summary$exact_source_n +
      qa$summary$ancestor_source_n,
    length(source)
  )
})


test_that("resolution table describes all compacted cells", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  qa <- qa_h3_compaction(
    source,
    compacted
  )

  expect_equal(
    base::sum(qa$resolution$compact_cell_n),
    length(compacted)
  )

  expect_equal(
    qa$resolution$compact_resolution,
    c(7L, 8L, 9L)
  )

  expect_equal(
    qa$resolution$compact_cell_n,
    c(15L, 55L, 165L)
  )
})


test_that("ownership table reproduces hierarchy membership", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  qa <- qa_h3_compaction(
    source,
    compacted
  )

  ancestor_r7 <- qa$ownership[
    qa$ownership$relationship == "ancestor" &
      qa$ownership$compact_resolution == 7L,
  ]

  ancestor_r8 <- qa$ownership[
    qa$ownership$relationship == "ancestor" &
      qa$ownership$compact_resolution == 8L,
  ]

  exact_r9 <- qa$ownership[
    qa$ownership$relationship == "exact" &
      qa$ownership$compact_resolution == 9L,
  ]

  expect_equal(
    ancestor_r7$source_cell_n,
    735L
  )

  expect_equal(
    ancestor_r8$source_cell_n,
    385L
  )

  expect_equal(
    exact_r9$source_cell_n,
    165L
  )
})


test_that("QA accepts a supplied valid lookup", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  qa <- qa_h3_compaction(
    source,
    compacted,
    lookup = lookup
  )

  expect_true(
    qa$summary$ownership_integrity
  )
})


test_that("QA identifies a non-exact compact representation", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 8
  )

  compacted <- compact_h3(
    source,
    min_resolution = 7
  )

  altered <- compacted[-1]

  expect_error(
    qa_h3_compaction(
      source,
      altered
    ),
    "ownership is incomplete"
  )
})


test_that("QA rejects mixed-resolution source input", {

  source_r8 <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  source_r9 <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 9
  )

  source_mixed <- c(
    source_r8[1],
    source_r9[1]
  )

  expect_error(
    qa_h3_compaction(
      source_mixed,
      source_r8
    ),
    "single resolution"
  )
})