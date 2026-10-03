# =============================================================================
# H3 POLYGON ASSIGNMENT
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Shared fixtures
# -----------------------------------------------------------------------------

assignment_extent <- sf::st_union(
  toy_membership_polygons
)

assignment_h3 <- h3_cover_polygon(
  assignment_extent,
  resolution = 9,
  boundary = "intersects"
)

assignment_result <- h3_assign_polygon(
  assignment_h3,
  toy_membership_polygons,
  id = "feature_id",
  outside = "unassigned"
)

expanded_h3 <- expand_h3(
  assignment_h3,
  rings = 3L
)

expanded_unassigned <- h3_assign_polygon(
  expanded_h3,
  toy_membership_polygons,
  id = "feature_id",
  outside = "unassigned"
)

expanded_nearest <- h3_assign_polygon(
  expanded_h3,
  toy_membership_polygons,
  id = "feature_id",
  outside = "nearest"
)


# -----------------------------------------------------------------------------
# 2. Output structure
# -----------------------------------------------------------------------------

test_that("assignment returns one row per unique H3 cell", {

  expect_equal(
    nrow(assignment_result),
    length(unique(assignment_h3))
  )

  expect_false(
    anyDuplicated(assignment_result$h3) > 0L
  )
})


test_that("assignment preserves complete H3 support", {

  expect_setequal(
    assignment_result$h3,
    assignment_h3
  )
})


test_that("assignment returns documented QA fields", {

  expect_named(
    assignment_result,
    c(
      "h3",
      "polygon_id",
      "assignment_method",
      "assignment_distance",
      "candidate_n"
    )
  )
})


test_that("H3 output is deterministic and sorted", {

  expect_identical(
    assignment_result$h3,
    sort(unique(assignment_h3))
  )
})


# -----------------------------------------------------------------------------
# 3. Centre assignment
# -----------------------------------------------------------------------------

test_that("interior cells are assigned by centre", {

  rows <- assignment_result$assignment_method == "centre"

  expect_true(any(rows))

  expect_true(
    all(assignment_result$assignment_distance[rows] == 0)
  )

  expect_true(
    all(assignment_result$candidate_n[rows] == 1L)
  )

  expect_false(
    anyNA(assignment_result$polygon_id[rows])
  )
})


# -----------------------------------------------------------------------------
# 4. Boundary intersection assignment
# -----------------------------------------------------------------------------

test_that("boundary cells can be assigned by geometric intersection", {

  rows <- assignment_result$assignment_method == "intersects"

  expect_true(any(rows))

  expect_true(
    all(assignment_result$assignment_distance[rows] == 0)
  )

  expect_true(
    all(assignment_result$candidate_n[rows] == 1L)
  )

  expect_false(
    anyNA(assignment_result$polygon_id[rows])
  )
})


test_that("intersects coverage fixture is completely assigned", {

  expect_false(
    any(assignment_result$assignment_method == "unassigned")
  )

  expect_false(
    anyNA(assignment_result$polygon_id)
  )
})


test_that("all fixture polygon identifiers receive cells", {

  expect_setequal(
    unique(assignment_result$polygon_id),
    toy_membership_polygons$feature_id
  )
})


# -----------------------------------------------------------------------------
# 5. Footprint mode
# -----------------------------------------------------------------------------

test_that("outside unassigned mode preserves external cells without ownership", {

  rows <- expanded_unassigned$assignment_method == "unassigned"

  expect_true(any(rows))

  expect_true(
    all(is.na(expanded_unassigned$polygon_id[rows]))
  )

  expect_true(
    all(is.na(expanded_unassigned$assignment_distance[rows]))
  )

  expect_true(
    all(expanded_unassigned$candidate_n[rows] == 0L)
  )
})


test_that("footprint ownership is unchanged by adding outside cells", {

  original_rows <- match(
    assignment_result$h3,
    expanded_unassigned$h3
  )

  expect_identical(
    expanded_unassigned$polygon_id[original_rows],
    assignment_result$polygon_id
  )

  expect_identical(
    expanded_unassigned$assignment_method[original_rows],
    assignment_result$assignment_method
  )
})


# -----------------------------------------------------------------------------
# 6. Complete nearest assignment
# -----------------------------------------------------------------------------

test_that("nearest mode leaves no cells unassigned", {

  expect_false(
    any(expanded_nearest$assignment_method == "unassigned")
  )
})


test_that("nearest mode assigns the external fixture cells", {

  rows <- expanded_nearest$assignment_method == "nearest"

  expect_true(any(rows))

  expect_false(
    anyNA(expanded_nearest$polygon_id[rows])
  )

  expect_true(
    all(expanded_nearest$assignment_distance[rows] >= 0)
  )

  expect_true(
    all(is.finite(expanded_nearest$assignment_distance[rows]))
  )

  expect_true(
    all(expanded_nearest$candidate_n[rows] == 1L)
  )
})


test_that("nearest mode preserves geometrically established ownership", {

  footprint_owned <- expanded_unassigned$assignment_method != "unassigned"

  expect_identical(
    expanded_nearest$polygon_id[footprint_owned],
    expanded_unassigned$polygon_id[footprint_owned]
  )

  expect_identical(
    expanded_nearest$assignment_method[footprint_owned],
    expanded_unassigned$assignment_method[footprint_owned]
  )
})


test_that("nearest mode changes only previously external cells", {

  changed <- expanded_nearest$polygon_id !=
    expanded_unassigned$polygon_id

  changed[is.na(changed)] <- TRUE

  expect_true(
    all(
      expanded_unassigned$assignment_method[changed] == "unassigned"
    )
  )
})


test_that("nearest assignment produces complete ownership unless ambiguous", {

  unresolved <- expanded_nearest$assignment_method == "ambiguous"

  expect_true(
    all(
      !is.na(expanded_nearest$polygon_id) |
        unresolved
    )
  )
})


# -----------------------------------------------------------------------------
# 7. Allowed assignment methods
# -----------------------------------------------------------------------------

test_that("assignment methods are restricted to documented values", {

  allowed <- c(
    "centre",
    "intersects",
    "largest_overlap",
    "nearest",
    "ambiguous",
    "unassigned"
  )

  expect_true(
    all(assignment_result$assignment_method %in% allowed)
  )

  expect_true(
    all(expanded_unassigned$assignment_method %in% allowed)
  )

  expect_true(
    all(expanded_nearest$assignment_method %in% allowed)
  )
})


test_that("unresolved cells never receive polygon ownership", {

  results <- list(
    assignment_result,
    expanded_unassigned,
    expanded_nearest
  )

  for (result in results) {

    rows <- result$assignment_method %in%
      c(
        "ambiguous",
        "unassigned"
      )

    expect_true(
      all(is.na(result$polygon_id[rows]))
    )
  }
})


test_that("resolved cells always receive polygon ownership", {

  results <- list(
    assignment_result,
    expanded_unassigned,
    expanded_nearest
  )

  for (result in results) {

    rows <- !result$assignment_method %in%
      c(
        "ambiguous",
        "unassigned"
      )

    expect_false(
      anyNA(result$polygon_id[rows])
    )
  }
})


# -----------------------------------------------------------------------------
# 8. Input-order determinism
# -----------------------------------------------------------------------------

test_that("H3 input order does not affect footprint assignment", {

  reversed_result <- h3_assign_polygon(
    rev(assignment_h3),
    toy_membership_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expect_identical(
    assignment_result,
    reversed_result
  )
})


test_that("H3 input order does not affect nearest assignment", {

  reversed_result <- h3_assign_polygon(
    rev(expanded_h3),
    toy_membership_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  expect_identical(
    expanded_nearest,
    reversed_result
  )
})


test_that("duplicate H3 input does not duplicate output", {

  duplicated_result <- h3_assign_polygon(
    c(
      assignment_h3,
      assignment_h3[1:10]
    ),
    toy_membership_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expect_identical(
    duplicated_result,
    assignment_result
  )
})


# -----------------------------------------------------------------------------
# 9. Polygon-order invariance
# -----------------------------------------------------------------------------

test_that("polygon order does not affect footprint ownership", {

  reversed_polygons <- toy_membership_polygons[
    rev(seq_len(nrow(toy_membership_polygons))),
  ]

  reversed_result <- h3_assign_polygon(
    assignment_h3,
    reversed_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expect_identical(
    assignment_result,
    reversed_result
  )
})


test_that("polygon order does not affect nearest ownership", {

  reversed_polygons <- toy_membership_polygons[
    rev(seq_len(nrow(toy_membership_polygons))),
  ]

  reversed_result <- h3_assign_polygon(
    expanded_h3,
    reversed_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  expect_identical(
    expanded_nearest,
    reversed_result
  )
})


# -----------------------------------------------------------------------------
# 10. CRS invariance
# -----------------------------------------------------------------------------

test_that("projected polygon CRS preserves footprint ownership", {

  projected_polygons <- sf::st_transform(
    toy_membership_polygons,
    7899
  )

  projected_result <- h3_assign_polygon(
    assignment_h3,
    projected_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expect_identical(
    projected_result$h3,
    assignment_result$h3
  )

  expect_identical(
    projected_result$polygon_id,
    assignment_result$polygon_id
  )

  expect_identical(
    projected_result$assignment_method,
    assignment_result$assignment_method
  )
})


test_that("projected polygon CRS preserves nearest ownership", {

  projected_polygons <- sf::st_transform(
    toy_membership_polygons,
    7899
  )

  projected_result <- h3_assign_polygon(
    expanded_h3,
    projected_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  expect_identical(
    projected_result$h3,
    expanded_nearest$h3
  )

  expect_identical(
    projected_result$polygon_id,
    expanded_nearest$polygon_id
  )

  expect_identical(
    projected_result$assignment_method,
    expanded_nearest$assignment_method
  )
})


# -----------------------------------------------------------------------------
# 11. Polygon topology
# -----------------------------------------------------------------------------

test_that("positive-area polygon overlap is rejected", {

  bad_polygons <- sf::st_sf(
    feature_id = c(
      "A",
      "B"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(
              144.90, -37.80,
              145.00, -37.80,
              145.00, -37.70,
              144.90, -37.70,
              144.90, -37.80
            ),
            ncol = 2,
            byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(
              144.95, -37.75,
              145.05, -37.75,
              145.05, -37.65,
              144.95, -37.65,
              144.95, -37.75
            ),
            ncol = 2,
            byrow = TRUE
          )
        )
      ),
      crs = 4326
    )
  )

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      bad_polygons,
      id = "feature_id"
    ),
    "overlap",
    ignore.case = TRUE
  )
})


# -----------------------------------------------------------------------------
# 12. Input validation
# -----------------------------------------------------------------------------

test_that("non-character H3 input is rejected", {

  expect_error(
    h3_assign_polygon(
      123,
      toy_membership_polygons,
      id = "feature_id"
    )
  )
})


test_that("empty H3 input is rejected", {

  expect_error(
    h3_assign_polygon(
      character(),
      toy_membership_polygons,
      id = "feature_id"
    )
  )
})


test_that("missing H3 input is rejected", {

  expect_error(
    h3_assign_polygon(
      c(
        assignment_h3[1],
        NA_character_
      ),
      toy_membership_polygons,
      id = "feature_id"
    )
  )
})


test_that("invalid H3 input is rejected", {

  expect_error(
    h3_assign_polygon(
      "not-an-h3-cell",
      toy_membership_polygons,
      id = "feature_id"
    )
  )
})


test_that("non-sf polygon input is rejected", {

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      data.frame(feature_id = "A"),
      id = "feature_id"
    )
  )
})


test_that("missing polygon identifier column is rejected", {

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      toy_membership_polygons,
      id = "missing_id"
    )
  )
})


test_that("duplicate polygon identifiers are rejected", {

  bad_polygons <- toy_membership_polygons

  bad_polygons$feature_id[2] <- bad_polygons$feature_id[1]

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      bad_polygons,
      id = "feature_id"
    )
  )
})


test_that("missing polygon identifiers are rejected", {

  bad_polygons <- toy_membership_polygons

  bad_polygons$feature_id[1] <- NA_character_

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      bad_polygons,
      id = "feature_id"
    )
  )
})


test_that("invalid outside policy is rejected", {

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      toy_membership_polygons,
      id = "feature_id",
      outside = "whatever"
    )
  )
})

# =============================================================================
# RETAINED POLYGON ATTRIBUTES
# =============================================================================

test_that("selected polygon attributes are retained", {

  polygons <- toy_membership_polygons

  polygons$feature_name <- paste0(
    "Feature ",
    polygons$feature_id
  )

  polygons$group_name <- c(
    "A",
    "A",
    "B",
    "B",
    "C"
  )

  result <- h3_assign_polygon(
    assignment_h3,
    polygons,
    id = "feature_id",
    keep = c(
      "feature_name",
      "group_name"
    ),
    outside = "unassigned"
  )

  expect_true(
    all(
      c(
        "feature_name",
        "group_name"
      ) %in% names(result)
    )
  )

  polygon_attributes <- sf::st_drop_geometry(
    polygons
  )

  expected_row <- match(
    result$polygon_id,
    polygon_attributes$feature_id
  )

  expect_identical(
    result$feature_name,
    polygon_attributes$feature_name[
      expected_row
    ]
  )

  expect_identical(
    result$group_name,
    polygon_attributes$group_name[
      expected_row
    ]
  )
})


test_that("multiple retained attributes preserve polygon values", {

  polygons <- toy_membership_polygons

  polygons$feature_name <- paste0(
    "Feature ",
    polygons$feature_id
  )

  polygons$class_name <- c(
    "Alpha",
    "Alpha",
    "Beta",
    "Beta",
    "Gamma"
  )

  polygons$numeric_code <- seq_len(
    nrow(polygons)
  )

  result <- h3_assign_polygon(
    assignment_h3,
    polygons,
    id = "feature_id",
    keep = c(
      "feature_name",
      "class_name",
      "numeric_code"
    )
  )

  expect_equal(
    nrow(result),
    length(unique(assignment_h3))
  )

  expect_false(
    anyNA(result$feature_name)
  )

  expect_false(
    anyNA(result$class_name)
  )

  expect_false(
    anyNA(result$numeric_code)
  )
})


test_that("unassigned H3 receive missing retained attributes", {

  polygons <- toy_membership_polygons

  polygons$feature_name <- paste0(
    "Feature ",
    polygons$feature_id
  )

  result <- h3_assign_polygon(
    expanded_h3,
    polygons,
    id = "feature_id",
    keep = "feature_name",
    outside = "unassigned"
  )

  outside_rows <- result$assignment_method == "unassigned"

  expect_true(
    any(outside_rows)
  )

  expect_true(
    all(
      is.na(
        result$feature_name[
          outside_rows
        ]
      )
    )
  )
})


test_that("nearest H3 receive attributes of assigned polygon", {

  polygons <- toy_membership_polygons

  polygons$feature_name <- paste0(
    "Feature ",
    polygons$feature_id
  )

  result <- h3_assign_polygon(
    expanded_h3,
    polygons,
    id = "feature_id",
    keep = "feature_name",
    outside = "nearest"
  )

  nearest_rows <- result$assignment_method == "nearest"

  expect_true(
    any(nearest_rows)
  )

  expect_false(
    anyNA(
      result$feature_name[
        nearest_rows
      ]
    )
  )

  polygon_attributes <- sf::st_drop_geometry(
    polygons
  )

  expected_row <- match(
    result$polygon_id[
      nearest_rows
    ],
    polygon_attributes$feature_id
  )

  expect_identical(
    result$feature_name[
      nearest_rows
    ],
    polygon_attributes$feature_name[
      expected_row
    ]
  )
})


test_that("keep NULL preserves the standard output", {

  result <- h3_assign_polygon(
    assignment_h3,
    toy_membership_polygons,
    id = "feature_id"
  )

  expect_identical(
    names(result),
    c(
      "h3",
      "polygon_id",
      "assignment_method",
      "assignment_distance",
      "candidate_n"
    )
  )
})


test_that("duplicate keep attributes are reduced to one column", {

  polygons <- toy_membership_polygons

  polygons$feature_name <- paste0(
    "Feature ",
    polygons$feature_id
  )

  result <- h3_assign_polygon(
    assignment_h3,
    polygons,
    id = "feature_id",
    keep = c(
      "feature_name",
      "feature_name"
    )
  )

  expect_equal(
    sum(names(result) == "feature_name"),
    1L
  )
})


test_that("id in keep is not duplicated", {

  result <- h3_assign_polygon(
    assignment_h3,
    toy_membership_polygons,
    id = "feature_id",
    keep = "feature_id"
  )

  expect_equal(
    sum(names(result) == "polygon_id"),
    1L
  )

  expect_false(
    "feature_id" %in% names(result)
  )
})


test_that("unknown keep attributes are rejected", {

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      toy_membership_polygons,
      id = "feature_id",
      keep = "does_not_exist"
    ),
    "Unknown polygon attributes"
  )
})


test_that("keep must be character", {

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      toy_membership_polygons,
      id = "feature_id",
      keep = 123
    ),
    "character vector"
  )
})


test_that("geometry cannot be retained through keep", {

  geometry_column <- attr(
    toy_membership_polygons,
    "sf_column"
  )

  expect_error(
    h3_assign_polygon(
      assignment_h3,
      toy_membership_polygons,
      id = "feature_id",
      keep = geometry_column
    ),
    "geometry"
  )
})