test_that("lookup contains exactly one row per source H3 cell", {

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

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  expect_s3_class(
    lookup,
    "tbl_df"
  )

  expect_equal(
    nrow(lookup),
    length(unique(source))
  )

  expect_equal(
    length(unique(lookup$source_h3)),
    nrow(lookup)
  )
})


test_that("every lookup owner is a retained compacted H3 cell", {

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

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  expect_true(
    all(lookup$compact_h3 %in% compacted)
  )
})


test_that("lookup records exact and ancestor relationships", {

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

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  expect_true(
    all(
      lookup$relationship %in%
        c("exact", "ancestor")
    )
  )

  expect_true(
    any(lookup$relationship == "exact")
  )

  expect_true(
    any(lookup$relationship == "ancestor")
  )
})


test_that("exact relationships retain the source H3 index", {

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

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  exact <- lookup[
    lookup$relationship == "exact",
  ]

  expect_true(
    all(
      exact$source_h3 ==
        exact$compact_h3
    )
  )

  expect_true(
    all(
      exact$source_resolution ==
        exact$compact_resolution
    )
  )
})


test_that("ancestor relationships use genuine H3 parents", {

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

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  ancestor <- lookup[
    lookup$relationship == "ancestor",
  ]

  for (target_resolution in unique(
    ancestor$compact_resolution
  )) {

    rows <- ancestor$compact_resolution ==
      target_resolution

    expected <- h3jsr::get_parent(
      ancestor$source_h3[rows],
      res = target_resolution
    )

    expect_equal(
      as.character(expected),
      ancestor$compact_h3[rows]
    )
  }
})


test_that("lookup preserves source resolution", {

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

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  expect_true(
    all(
      lookup$source_resolution == 9L
    )
  )

  expect_true(
    all(
      lookup$compact_resolution <=
        lookup$source_resolution
    )
  )
})


test_that("no-compaction lookup is entirely exact", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 8
  )

  compacted <- compact_h3(
    source,
    min_resolution = 8
  )

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  expect_true(
    all(
      lookup$relationship == "exact"
    )
  )

  expect_setequal(
    lookup$compact_h3,
    source
  )
})


test_that("lookup works with unrestricted exact compaction", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "complex",
    ],
    resolution = 9
  )

  compacted <- compact_h3(source)

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  expect_equal(
    nrow(lookup),
    length(source)
  )

  expect_false(
    anyNA(lookup$compact_h3)
  )

  expect_true(
    all(
      lookup$compact_h3 %in% compacted
    )
  )
})


test_that("duplicate source indexes are normalised", {

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

  source_duplicates <- c(
    source,
    source[1:5]
  )

  lookup <- h3_compaction_lookup(
    source_duplicates,
    compacted
  )

  expect_equal(
    nrow(lookup),
    length(unique(source))
  )
})


test_that("lookup rejects mixed-resolution source H3", {

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

  mixed_source <- c(
    source_r8[1],
    source_r9[1]
  )

  expect_error(
    h3_compaction_lookup(
      mixed_source,
      source_r8
    ),
    "single resolution"
  )
})


test_that("lookup rejects invalid source H3", {

  expect_error(
    h3_compaction_lookup(
      "not_an_h3_cell",
      "not_an_h3_cell"
    ),
    "invalid H3 indexes"
  )
})


test_that("lookup rejects missing source H3", {

  expect_error(
    h3_compaction_lookup(
      NA_character_,
      "not_used"
    ),
    "missing or empty"
  )
})


test_that("lookup rejects empty source input", {

  expect_error(
    h3_compaction_lookup(
      character(),
      character()
    ),
    "at least one"
  )
})


test_that("lookup rejects incomplete compact ownership", {

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

  incomplete <- compacted[-1]

  expect_error(
    h3_compaction_lookup(
      source,
      incomplete
    ),
    "ownership is incomplete"
  )
})


test_that("lookup rejects compacted cells finer than source", {

  source <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 8
  )

  finer <- h3_cover_polygon(
    toy_polygons[
      toy_polygons$geometry_id == "regular",
    ],
    resolution = 9
  )

  expect_error(
    h3_compaction_lookup(
      source,
      finer
    ),
    "finer than"
  )
})