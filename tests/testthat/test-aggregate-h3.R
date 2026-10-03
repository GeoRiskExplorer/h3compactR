# =============================================================================
# h3compactR
# Tests — aggregate_h3()
#
# Tests additive aggregation, categorical provenance collapse, combined
# aggregation, deterministic output, and source-support safeguards.
# =============================================================================


# =============================================================================
# 1. Shared test fixture
# =============================================================================

make_aggregate_fixture <- function(
  geometry_id = "complex",
  resolution = 9L,
  min_resolution = 7L
) {

  source <- h3_cover_polygon(
    toy_polygons[toy_polygons$geometry_id == geometry_id, ],
    resolution = resolution
  )

  compacted <- compact_h3(
    source,
    min_resolution = min_resolution
  )

  lookup <- h3_compaction_lookup(
    source,
    compacted
  )

  list(
    source = source,
    compacted = compacted,
    lookup = lookup
  )
}


# =============================================================================
# 2. Additive aggregation
# =============================================================================

test_that("aggregate_h3 conserves additive attributes", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source)),
    exposure = seq_along(fixture$source),
    water_area_m2 = seq_along(fixture$source) * 10
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = c(
      "events",
      "exposure",
      "water_area_m2"
    )
  )

  expect_equal(
    nrow(result),
    length(fixture$compacted)
  )

  expect_equal(
    base::sum(result$source_cell_n),
    length(fixture$source)
  )

  expect_equal(
    base::sum(result$events),
    base::sum(source_data$events)
  )

  expect_equal(
    base::sum(result$exposure),
    base::sum(source_data$exposure)
  )

  expect_equal(
    base::sum(result$water_area_m2),
    base::sum(source_data$water_area_m2)
  )
})


test_that("source ownership counts align with compact-cell values", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    value = rep(1, length(fixture$source))
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = "value"
  )

  # Regression test: grand-total conservation alone is insufficient.
  # Every compact owner must receive the values from its own descendants.
  expect_equal(
    result$value,
    result$source_cell_n
  )

  expect_equal(
    base::sum(result$source_cell_n),
    length(fixture$source)
  )
})


# =============================================================================
# 3. Categorical collapse
# =============================================================================

test_that("aggregate_h3 supports collapse-only aggregation", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    feature_id = rep(
      c("100", "200"),
      length.out = length(fixture$source)
    )
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    collapse = "feature_id"
  )

  expect_equal(
    nrow(result),
    length(fixture$compacted)
  )

  expect_true(
    "feature_id" %in% names(result)
  )

  expect_type(
    result$feature_id,
    "character"
  )

  expect_type(
    result$feature_id_n,
    "integer"
  )

  expect_true(
    all(result$feature_id_n >= 1L)
  )

  expect_equal(
    base::sum(result$source_cell_n),
    length(fixture$source)
  )
})


test_that("collapsed attributes contain unique values only", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    feature_id = rep(
      c("200", "100", "100", "200"),
      length.out = length(fixture$source)
    )
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    collapse = "feature_id"
  )

  expect_true(
    all(
      result$feature_id %in% c(
        "100",
        "200",
        "100; 200"
      )
    )
  )

  expect_false(
    any(
      grepl(
        "100; 100|200; 200",
        result$feature_id
      )
    )
  )
})


test_that("collapsed values use deterministic sorted ordering", {

  fixture <- make_aggregate_fixture()

  source_data_a <- data.frame(
    h3 = fixture$source,
    category = rep(
      c("Zulu", "Alpha", "Beta"),
      length.out = length(fixture$source)
    )
  )

  source_data_b <- source_data_a[
    rev(seq_len(nrow(source_data_a))),
    ,
    drop = FALSE
  ]

  result_a <- aggregate_h3(
    data = source_data_a,
    lookup = fixture$lookup,
    collapse = "category"
  )

  result_b <- aggregate_h3(
    data = source_data_b,
    lookup = fixture$lookup,
    collapse = "category"
  )

  expect_identical(
    result_a,
    result_b
  )

  multi_value <- result_a$category[
    grepl(
      ";",
      result_a$category,
      fixed = TRUE
    )
  ]

  if (length(multi_value) > 0L) {
    expect_true(
      all(
        !grepl(
          "Zulu; Alpha|Beta; Alpha|Zulu; Beta",
          multi_value
        )
      )
    )
  }
})


test_that("multiple categorical attributes can be collapsed together", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    feature_id = rep(
      c("100", "200"),
      length.out = length(fixture$source)
    ),
    feature_name = rep(
      c("Alpha", "Beta"),
      length.out = length(fixture$source)
    ),
    group_name = rep(
      c("Group 1", "Group 1", "Group 2"),
      length.out = length(fixture$source)
    )
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    collapse = c(
      "feature_id",
      "feature_name",
      "group_name"
    )
  )

  expect_true(
    all(
      c(
        "feature_id",
        "feature_name",
        "group_name"
      ) %in% names(result)
    )
  )

  expect_equal(
    nrow(result),
    length(fixture$compacted)
  )
})


# =============================================================================
# 4. Missing categorical values
# =============================================================================

test_that("collapse ignores missing values when non-missing values exist", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    category = rep("Alpha", length(fixture$source))
  )

  source_data$category[seq(1L, nrow(source_data), by = 2L)] <- NA_character_

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    collapse = "category"
  )

  expect_true(
    all(
      is.na(result$category) |
        result$category == "Alpha"
    )
  )
})


test_that("collapse returns NA when all source values for an owner are missing", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  owner <- fixture$lookup$compact_h3[1]

  owner_source <- fixture$lookup$source_h3[
    fixture$lookup$compact_h3 == owner
  ]

  source_data <- data.frame(
    h3 = fixture$source,
    category = rep("Alpha", length(fixture$source))
  )

  source_data$category[
    source_data$h3 %in% owner_source
  ] <- NA_character_

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    collapse = "category"
  )

  expect_true(
    is.na(
      result$category[
        result$compact_h3 == owner
      ]
    )
  )
})


# =============================================================================
# 5. Combined additive and categorical aggregation
# =============================================================================

test_that("sum and collapse work together", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source)),
    area_m2 = seq_along(fixture$source) * 10,
    feature_id = rep(
      c("100", "200"),
      length.out = length(fixture$source)
    ),
    feature_name = rep(
      c("Alpha", "Beta"),
      length.out = length(fixture$source)
    )
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = c(
      "events",
      "area_m2"
    ),
    collapse = c(
      "feature_id",
      "feature_name"
    )
  )

  expect_equal(
    result$events,
    result$source_cell_n
  )

  expect_equal(
    base::sum(result$area_m2),
    base::sum(source_data$area_m2)
  )

  expect_true(
    all(
      c(
        "feature_id",
        "feature_name"
      ) %in% names(result)
    )
  )
})


test_that("a compact owner can preserve multiple source feature identities", {

  fixture <- make_aggregate_fixture()

  multi_owner <- fixture$lookup$compact_h3[
    duplicated(fixture$lookup$compact_h3)
  ][1]

  expect_false(
    is.na(multi_owner)
  )

  owner_source <- fixture$lookup$source_h3[
    fixture$lookup$compact_h3 == multi_owner
  ]

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source)),
    feature_id = rep("100", length(fixture$source)),
    feature_name = rep("Alpha", length(fixture$source))
  )

  split_point <- max(
    1L,
    floor(length(owner_source) / 2L)
  )

  second_group <- owner_source[
    seq.int(
      split_point + 1L,
      length(owner_source)
    )
  ]

  if (length(second_group) == 0L) {
    second_group <- owner_source[length(owner_source)]
  }

  source_data$feature_id[
    source_data$h3 %in% second_group
  ] <- "200"

  source_data$feature_name[
    source_data$h3 %in% second_group
  ] <- "Beta"

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = "events",
    collapse = c(
      "feature_id",
      "feature_name"
    )
  )

  owner_result <- result[
    result$compact_h3 == multi_owner,
    ,
    drop = FALSE
  ]

  expect_equal(
    owner_result$feature_id,
    "100; 200"
  )

  expect_equal(
    owner_result$feature_name,
    "Alpha; Beta"
  )

  expect_equal(
    owner_result$feature_id_n,
    2L
  )

  expect_equal(
    owner_result$feature_name_n,
    2L
  )

  expect_equal(
    owner_result$events,
    owner_result$source_cell_n
  )

  expect_equal(
    base::sum(result$events),
    base::sum(source_data$events)
  )
})


test_that("cross-polygon compact owners conserve additive values exactly once", {

  fixture <- make_aggregate_fixture()

  multi_owner <- fixture$lookup$compact_h3[
    duplicated(fixture$lookup$compact_h3)
  ][1]

  owner_source <- fixture$lookup$source_h3[
    fixture$lookup$compact_h3 == multi_owner
  ]

  source_data <- data.frame(
    h3 = fixture$source,
    population = rep(100, length(fixture$source)),
    polygon_id = rep("A", length(fixture$source))
  )

  split_point <- max(
    1L,
    floor(length(owner_source) / 2L)
  )

  second_group <- owner_source[
    seq.int(
      split_point + 1L,
      length(owner_source)
    )
  ]

  if (length(second_group) == 0L) {
    second_group <- owner_source[length(owner_source)]
  }

  source_data$polygon_id[
    source_data$h3 %in% second_group
  ] <- "B"

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = "population",
    collapse = "polygon_id"
  )

  owner_result <- result[
    result$compact_h3 == multi_owner,
    ,
    drop = FALSE
  ]

  expect_equal(
    owner_result$polygon_id,
    "A; B"
  )

  expect_equal(
    owner_result$polygon_id_n,
    2L
  )

  expect_equal(
    owner_result$population,
    owner_result$source_cell_n * 100
  )

  expect_equal(
    base::sum(result$population),
    base::sum(source_data$population)
  )

  expect_equal(
    base::sum(result$source_cell_n),
    nrow(source_data)
  )
})


# =============================================================================
# 6. Custom H3 column
# =============================================================================

test_that("aggregate_h3 works with a custom H3 column name", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    source_cell = fixture$source,
    events = rep(1, length(fixture$source)),
    category = rep("Alpha", length(fixture$source))
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = "events",
    collapse = "category",
    h3_col = "source_cell"
  )

  expect_equal(
    base::sum(result$events),
    length(fixture$source)
  )

  expect_true(
    all(result$category == "Alpha")
  )
})


# =============================================================================
# 7. Deterministic compact output
# =============================================================================

test_that("aggregate_h3 output is deterministic under source row order", {

  fixture <- make_aggregate_fixture()

  source_data <- data.frame(
    h3 = fixture$source,
    events = seq_along(fixture$source),
    category = rep(
      c("B", "A", "C"),
      length.out = length(fixture$source)
    )
  )

  reversed_data <- source_data[
    rev(seq_len(nrow(source_data))),
    ,
    drop = FALSE
  ]

  result_a <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = "events",
    collapse = "category"
  )

  result_b <- aggregate_h3(
    data = reversed_data,
    lookup = fixture$lookup,
    sum = "events",
    collapse = "category"
  )

  expect_identical(
    result_a,
    result_b
  )
})


# =============================================================================
# 8. Invalid additive input
# =============================================================================

test_that("aggregate_h3 rejects missing additive values", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source))
  )

  source_data$events[1] <- NA_real_

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      sum = "events"
    ),
    "Missing values"
  )
})


test_that("aggregate_h3 rejects non-numeric additive fields", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    category = rep("A", length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      sum = "category"
    ),
    "numeric"
  )
})


# =============================================================================
# 9. Invalid field specifications
# =============================================================================

test_that("aggregate_h3 requires sum or collapse", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup
    ),
    "At least one field"
  )
})


test_that("aggregate_h3 rejects fields requested in both sum and collapse", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      sum = "events",
      collapse = "events"
    ),
    "both"
  )
})


test_that("aggregate_h3 rejects unknown sum fields", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      sum = "does_not_exist"
    ),
    "not found"
  )
})


test_that("aggregate_h3 rejects unknown collapse fields", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    category = rep("A", length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      collapse = "does_not_exist"
    ),
    "not found"
  )
})


test_that("aggregate_h3 rejects the H3 identifier as an aggregation field", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      collapse = "h3"
    ),
    "h3_col"
  )
})


test_that("aggregate_h3 rejects provenance count name collisions", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    category = rep("A", length(fixture$source)),
    category_n = rep(99L, length(fixture$source))
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      collapse = "category"
    ),
    "overwrite existing"
  )
})


# =============================================================================
# 10. Source-support safeguards
# =============================================================================

test_that("aggregate_h3 rejects duplicate source rows", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = c(
      fixture$source,
      fixture$source[1]
    ),
    events = 1
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      sum = "events"
    ),
    "Duplicate"
  )
})


test_that("aggregate_h3 requires complete agreement with lookup support", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source[-1],
    events = rep(
      1,
      length(fixture$source) - 1L
    )
  )

  expect_error(
    aggregate_h3(
      data = source_data,
      lookup = fixture$lookup,
      sum = "events"
    ),
    "same authoritative source"
  )
})


# =============================================================================
# 11. Duplicate requested fields
# =============================================================================

test_that("duplicate requested fields are reduced to one output column", {

  fixture <- make_aggregate_fixture(
    geometry_id = "regular",
    resolution = 8L,
    min_resolution = 7L
  )

  source_data <- data.frame(
    h3 = fixture$source,
    events = rep(1, length(fixture$source)),
    category = rep("Alpha", length(fixture$source))
  )

  result <- aggregate_h3(
    data = source_data,
    lookup = fixture$lookup,
    sum = c(
      "events",
      "events"
    ),
    collapse = c(
      "category",
      "category"
    )
  )

  expect_equal(
    sum(names(result) == "events"),
    1L
  )

  expect_equal(
    sum(names(result) == "category"),
    1L
  )
})
