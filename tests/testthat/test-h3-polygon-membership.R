# =============================================================================
# H3 POLYGON MEMBERSHIP
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Build authoritative H3 support
# -----------------------------------------------------------------------------

membership_extent <- sf::st_union(
  toy_membership_polygons
)

membership_h3 <- h3_cover_polygon(
  membership_extent,
  resolution = 9,
  boundary = "intersects"
)


# -----------------------------------------------------------------------------
# 2. Valid topology
# -----------------------------------------------------------------------------

test_that("valid membership geography is accepted", {

  result <- h3_polygon_membership(
    membership_h3,
    toy_membership_polygons,
    id = "feature_id"
  )

  expect_s3_class(result, "tbl_df")

  expect_named(
    result,
    c(
      "h3",
      "polygon_id"
    )
  )

  expect_gt(
    nrow(result),
    0L
  )
})


test_that("all polygon identifiers receive membership", {

  result <- h3_polygon_membership(
    membership_h3,
    toy_membership_polygons,
    id = "feature_id"
  )

  expect_setequal(
    unique(result$polygon_id),
    toy_membership_polygons$feature_id
  )
})


test_that("multipart polygon retains one feature identifier", {

  result <- h3_polygon_membership(
    membership_h3,
    toy_membership_polygons,
    id = "feature_id"
  )

  feature_400 <- result[
    result$polygon_id == "400",
  ]

  expect_gt(
    nrow(feature_400),
    0L
  )

  expect_true(
    all(feature_400$polygon_id == "400")
  )
})


test_that("membership relationships are unique", {

  result <- h3_polygon_membership(
    membership_h3,
    toy_membership_polygons,
    id = "feature_id"
  )

  expect_false(
    anyDuplicated(result) > 0L
  )
})


# -----------------------------------------------------------------------------
# 3. Topology validation
# -----------------------------------------------------------------------------

test_that("adjacent polygons sharing boundaries are accepted", {

  adjacent <- toy_membership_polygons[
    toy_membership_polygons$feature_id %in% c(
      "100",
      "200"
    ),
  ]

  adjacent_h3 <- h3_cover_polygon(
    sf::st_union(adjacent),
    resolution = 9,
    boundary = "intersects"
  )

  expect_no_error(
    h3_polygon_membership(
      adjacent_h3,
      adjacent,
      id = "feature_id"
    )
  )
})


test_that("donut and enclave polygons are accepted", {

  enclave <- toy_membership_polygons[
    toy_membership_polygons$feature_id %in% c(
      "500",
      "600"
    ),
  ]

  enclave_h3 <- h3_cover_polygon(
    sf::st_union(enclave),
    resolution = 9,
    boundary = "intersects"
  )

  expect_no_error(
    h3_polygon_membership(
      enclave_h3,
      enclave,
      id = "feature_id"
    )
  )
})


test_that("positive-area polygon overlap is rejected", {

  overlap_a <- sf::st_polygon(
    list(
      matrix(
        c(
          144.90, -37.90,
          145.02, -37.90,
          145.02, -37.80,
          144.90, -37.80,
          144.90, -37.90
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  overlap_b <- sf::st_polygon(
    list(
      matrix(
        c(
          144.98, -37.90,
          145.10, -37.90,
          145.10, -37.80,
          144.98, -37.80,
          144.98, -37.90
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  overlapping_polygons <- sf::st_sf(
    feature_id = c("A", "B"),
    geometry = sf::st_sfc(
      overlap_a,
      overlap_b,
      crs = 4326
    )
  )

  overlap_h3 <- h3_cover_polygon(
    sf::st_union(overlapping_polygons),
    resolution = 9,
    boundary = "intersects"
  )

  expect_error(
    h3_polygon_membership(
      overlap_h3,
      overlapping_polygons,
      id = "feature_id"
    ),
    "overlapping interiors"
  )
})


# -----------------------------------------------------------------------------
# 4. Input validation
# -----------------------------------------------------------------------------

test_that("invalid H3 indexes are rejected", {

  expect_error(
    h3_polygon_membership(
      "not-an-h3-index",
      toy_membership_polygons,
      id = "feature_id"
    ),
    "invalid H3"
  )
})


test_that("missing identifier column is rejected", {

  expect_error(
    h3_polygon_membership(
      membership_h3,
      toy_membership_polygons,
      id = "missing_id"
    ),
    "not found"
  )
})


test_that("duplicate polygon identifiers are rejected", {

  polygons <- toy_membership_polygons

  polygons$feature_id[2] <- polygons$feature_id[1]

  expect_error(
    h3_polygon_membership(
      membership_h3,
      polygons,
      id = "feature_id"
    ),
    "uniquely identify"
  )
})


test_that("membership result is deterministic", {

  result_1 <- h3_polygon_membership(
    membership_h3,
    toy_membership_polygons,
    id = "feature_id"
  )

  result_2 <- h3_polygon_membership(
    rev(membership_h3),
    toy_membership_polygons,
    id = "feature_id"
  )

  expect_identical(
    result_1,
    result_2
  )
})