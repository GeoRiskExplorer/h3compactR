# =============================================================================
# H3 POLYGON ASSIGNMENT — ADVERSARIAL TESTS
# =============================================================================
#
# Purpose:
# Deliberately exercise difficult spatial assignment cases that should not be
# expected to occur naturally in the general package fixture.
#
# Production code should not be changed merely to satisfy these fixtures.
# Each constructed fixture first verifies its own spatial preconditions.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Helper — H3 polygon
# -----------------------------------------------------------------------------

.make_h3_polygon <- function(h3, crs = 4326) {

  geometry <- h3jsr::cell_to_polygon(
    h3,
    simple = FALSE
  )

  sf::st_transform(
    geometry,
    crs
  )
}


# -----------------------------------------------------------------------------
# 2. Helper — local projected CRS
# -----------------------------------------------------------------------------

.make_local_crs <- function(x) {

  x_4326 <- sf::st_transform(
    x,
    4326
  )

  bbox <- sf::st_bbox(
    x_4326
  )

  lon_0 <- mean(
    c(
      bbox[["xmin"]],
      bbox[["xmax"]]
    )
  )

  lat_0 <- mean(
    c(
      bbox[["ymin"]],
      bbox[["ymax"]]
    )
  )

  paste0(
    "+proj=aeqd +lat_0=",
    lat_0,
    " +lon_0=",
    lon_0,
    " +datum=WGS84 +units=m +no_defs"
  )
}


# -----------------------------------------------------------------------------
# 3. Helper — centre-gap fixture
# -----------------------------------------------------------------------------
#
# Creates two topologically valid polygons separated by a vertical corridor.
#
# The H3 centre lies inside the corridor and therefore belongs to neither
# polygon. The H3 cell itself intersects both polygons.
#
# `left_fraction` and `right_fraction` control how much of the H3 bounding
# width is available to the two candidate polygons.
#
# This deliberately forces assignment beyond the centre stage.
# -----------------------------------------------------------------------------

.make_gap_fixture <- function(
  h3,
  gap_fraction = 0.08,
  left_fraction = 1,
  right_fraction = 1
) {

  cell <- .make_h3_polygon(
    h3
  )

  local_crs <- .make_local_crs(
    cell
  )

  cell_projected <- sf::st_transform(
    cell,
    local_crs
  )

  centre <- h3jsr::cell_to_point(
    h3,
    simple = FALSE
  )

  centre_projected <- sf::st_transform(
    centre,
    local_crs
  )

  centre_xy <- sf::st_coordinates(
    centre_projected
  )[1, ]

  bbox <- sf::st_bbox(
    cell_projected
  )

  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]

  gap_half <- width * gap_fraction / 2

  left_edge <- centre_xy[["X"]] - gap_half
  right_edge <- centre_xy[["X"]] + gap_half

  left_available <- left_edge - bbox[["xmin"]]
  right_available <- bbox[["xmax"]] - right_edge

  left_min <- left_edge -
    left_available * left_fraction

  right_max <- right_edge +
    right_available * right_fraction

  pad <- max(
    width,
    height
  )

  left_box <- sf::st_polygon(
    list(
      matrix(
        c(
          left_min,
          bbox[["ymin"]] - pad,

          left_edge,
          bbox[["ymin"]] - pad,

          left_edge,
          bbox[["ymax"]] + pad,

          left_min,
          bbox[["ymax"]] + pad,

          left_min,
          bbox[["ymin"]] - pad
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  right_box <- sf::st_polygon(
    list(
      matrix(
        c(
          right_edge,
          bbox[["ymin"]] - pad,

          right_max,
          bbox[["ymin"]] - pad,

          right_max,
          bbox[["ymax"]] + pad,

          right_edge,
          bbox[["ymax"]] + pad,

          right_edge,
          bbox[["ymin"]] - pad
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  boxes <- sf::st_sfc(
    left_box,
    right_box,
    crs = local_crs
  )

  cell_geometry <- sf::st_geometry(
    cell_projected
  )

  left_geometry <- suppressWarnings(
    sf::st_intersection(
      cell_geometry,
      boxes[1]
    )
  )

  right_geometry <- suppressWarnings(
    sf::st_intersection(
      cell_geometry,
      boxes[2]
    )
  )

  fixture <- sf::st_sf(
    polygon_id = c(
      "LEFT",
      "RIGHT"
    ),
    geometry = sf::st_sfc(
      left_geometry[[1]],
      right_geometry[[1]],
      crs = local_crs
    )
  )

  sf::st_transform(
    fixture,
    4326
  )
}


# -----------------------------------------------------------------------------
# 4. Helper — measure H3 overlap with candidate polygons
# -----------------------------------------------------------------------------

.measure_overlap <- function(
  h3,
  polygons
) {

  cell <- .make_h3_polygon(
    h3
  )

  local_crs <- .make_local_crs(
    cell
  )

  cell_projected <- sf::st_transform(
    cell,
    local_crs
  )

  polygons_projected <- sf::st_transform(
    polygons,
    local_crs
  )

  vapply(
    seq_len(nrow(polygons_projected)),
    function(i) {

      intersection <- suppressWarnings(
        sf::st_intersection(
          sf::st_geometry(cell_projected),
          sf::st_geometry(polygons_projected)[i]
        )
      )

      if (length(intersection) == 0L) {
        return(0)
      }

      sum(
        as.numeric(
          sf::st_area(
            intersection
          )
        )
      )
    },
    numeric(1)
  )
}


# -----------------------------------------------------------------------------
# 5. Helper — verify centre-gap fixture
# -----------------------------------------------------------------------------

.verify_gap_fixture <- function(
  h3,
  polygons
) {

  centre <- h3jsr::cell_to_point(
    h3,
    simple = FALSE
  )

  centre <- sf::st_transform(
    centre,
    sf::st_crs(polygons)
  )

  centre_relation <- sf::st_within(
    centre,
    polygons,
    sparse = TRUE
  )

  cell <- .make_h3_polygon(
    h3,
    crs = sf::st_crs(polygons)
  )

  intersection_relation <- sf::st_intersects(
    cell,
    polygons,
    sparse = TRUE
  )

  list(
    centre_candidate_n = length(centre_relation[[1]]),
    intersection_candidate_n = length(intersection_relation[[1]]),
    overlap_area = .measure_overlap(
      h3,
      polygons
    )
  )
}


# -----------------------------------------------------------------------------
# 6. Select a stable H3 cell
# -----------------------------------------------------------------------------

adversarial_h3 <- h3_cover_polygon(
  x = toy_membership_polygons,
  resolution = 9L,
  boundary = "center"
)

expect_gt(
  length(adversarial_h3),
  10L
)

test_h3 <- adversarial_h3[
  floor(length(adversarial_h3) / 2)
]


# -----------------------------------------------------------------------------
# 7. Shared boundaries remain valid topology
# -----------------------------------------------------------------------------

test_that("adjacent polygons sharing a boundary are accepted", {

  cell <- .make_h3_polygon(
    test_h3
  )

  local_crs <- .make_local_crs(
    cell
  )

  cell_projected <- sf::st_transform(
    cell,
    local_crs
  )

  bbox <- sf::st_bbox(
    cell_projected
  )

  split_x <- mean(
    c(
      bbox[["xmin"]],
      bbox[["xmax"]]
    )
  )

  pad <- max(
    bbox[["xmax"]] - bbox[["xmin"]],
    bbox[["ymax"]] - bbox[["ymin"]]
  )

  left_polygon <- sf::st_polygon(
    list(
      matrix(
        c(
          bbox[["xmin"]] - pad,
          bbox[["ymin"]] - pad,

          split_x,
          bbox[["ymin"]] - pad,

          split_x,
          bbox[["ymax"]] + pad,

          bbox[["xmin"]] - pad,
          bbox[["ymax"]] + pad,

          bbox[["xmin"]] - pad,
          bbox[["ymin"]] - pad
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  right_polygon <- sf::st_polygon(
    list(
      matrix(
        c(
          split_x,
          bbox[["ymin"]] - pad,

          bbox[["xmax"]] + pad,
          bbox[["ymin"]] - pad,

          bbox[["xmax"]] + pad,
          bbox[["ymax"]] + pad,

          split_x,
          bbox[["ymax"]] + pad,

          split_x,
          bbox[["ymin"]] - pad
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  fixture <- sf::st_sf(
    polygon_id = c(
      "LEFT",
      "RIGHT"
    ),
    geometry = sf::st_sfc(
      left_polygon,
      right_polygon,
      crs = local_crs
    )
  )

  expect_true(
    all(sf::st_is_valid(fixture))
  )

  expect_silent(
    h3_assign_polygon(
      test_h3,
      fixture,
      id = "polygon_id",
      outside = "unassigned"
    )
  )
})


# -----------------------------------------------------------------------------
# 8. Largest-overlap fixture preconditions
# -----------------------------------------------------------------------------

largest_overlap_fixture <- .make_gap_fixture(
  test_h3,
  gap_fraction = 0.08,
  left_fraction = 1,
  right_fraction = 0.55
)

largest_overlap_qa <- .verify_gap_fixture(
  test_h3,
  largest_overlap_fixture
)


test_that("largest-overlap fixture bypasses centre assignment", {

  expect_equal(
    largest_overlap_qa$centre_candidate_n,
    0L
  )
})


test_that("largest-overlap fixture intersects two polygons", {

  expect_equal(
    largest_overlap_qa$intersection_candidate_n,
    2L
  )
})


test_that("largest-overlap fixture has unequal candidate areas", {

  expect_equal(
    length(largest_overlap_qa$overlap_area),
    2L
  )

  expect_true(
    all(largest_overlap_qa$overlap_area > 0)
  )

  expect_gt(
    abs(
      largest_overlap_qa$overlap_area[1] -
        largest_overlap_qa$overlap_area[2]
    ),
    sqrt(.Machine$double.eps)
  )
})


# -----------------------------------------------------------------------------
# 9. Largest-overlap assignment
# -----------------------------------------------------------------------------

test_that("multiple intersections use unique greatest overlap", {

  result <- h3_assign_polygon(
    test_h3,
    largest_overlap_fixture,
    id = "polygon_id",
    outside = "unassigned"
  )

  expect_equal(
    result$assignment_method,
    "largest_overlap"
  )

  expect_equal(
    result$candidate_n,
    2L
  )

  expect_false(
    is.na(result$polygon_id)
  )

  expect_equal(
    result$assignment_distance,
    0
  )

  expected_id <- largest_overlap_fixture$polygon_id[
    which.max(
      largest_overlap_qa$overlap_area
    )
  ]

  expect_equal(
    result$polygon_id,
    expected_id
  )
})


test_that("largest-overlap assignment is polygon-order invariant", {

  forward <- h3_assign_polygon(
    test_h3,
    largest_overlap_fixture,
    id = "polygon_id",
    outside = "unassigned"
  )

  reverse <- h3_assign_polygon(
    test_h3,
    largest_overlap_fixture[2:1, ],
    id = "polygon_id",
    outside = "unassigned"
  )

  expect_identical(
    forward,
    reverse
  )
})


# -----------------------------------------------------------------------------
# 10. Search for a controlled equal-overlap fixture
# -----------------------------------------------------------------------------
#
# H3 cells are not axis-aligned regular shapes in projected coordinates.
# Rather than assuming a nominal 50:50 bounding-box split is equal area, search
# a small range of right-side fractions and retain the closest measurable pair.
#
# This is fixture construction, not production assignment logic.
# -----------------------------------------------------------------------------

equal_candidates <- seq(
  0.80,
  1.20,
  length.out = 81L
)

equal_search <- lapply(
  equal_candidates,
  function(right_fraction) {

    fixture <- .make_gap_fixture(
      test_h3,
      gap_fraction = 0.08,
      left_fraction = 1,
      right_fraction = right_fraction
    )

    qa <- .verify_gap_fixture(
      test_h3,
      fixture
    )

    relative_difference <- abs(
      qa$overlap_area[1] -
        qa$overlap_area[2]
    ) /
      max(
        qa$overlap_area
      )

    list(
      fixture = fixture,
      qa = qa,
      relative_difference = relative_difference
    )
  }
)

equal_difference <- vapply(
  equal_search,
  function(x) {
    x$relative_difference
  },
  numeric(1)
)

equal_best <- equal_search[[which.min(equal_difference)]]

equal_overlap_fixture <- equal_best$fixture
equal_overlap_qa <- equal_best$qa


# -----------------------------------------------------------------------------
# 11. Equal-overlap fixture preconditions
# -----------------------------------------------------------------------------

test_that("equal-overlap fixture bypasses centre assignment", {

  expect_equal(
    equal_overlap_qa$centre_candidate_n,
    0L
  )
})


test_that("equal-overlap fixture intersects two polygons", {

  expect_equal(
    equal_overlap_qa$intersection_candidate_n,
    2L
  )
})


test_that("equal-overlap fixture is approximately balanced", {

  expect_lt(
    equal_best$relative_difference,
    0.01
  )
})


# -----------------------------------------------------------------------------
# 12. Equal-overlap assignment never depends on polygon order
# -----------------------------------------------------------------------------
#
# The current production tolerance is intentionally much tighter than the
# fixture-search tolerance above. Therefore this test does not falsely demand
# "ambiguous" unless the measured areas satisfy the production tolerance.
#
# Regardless of whether the fixture resolves to largest_overlap or ambiguous,
# polygon record order must never change the analytical result.
# -----------------------------------------------------------------------------

test_that("near-equal overlap assignment is polygon-order invariant", {

  forward <- h3_assign_polygon(
    test_h3,
    equal_overlap_fixture,
    id = "polygon_id",
    outside = "unassigned"
  )

  reverse <- h3_assign_polygon(
    test_h3,
    equal_overlap_fixture[2:1, ],
    id = "polygon_id",
    outside = "unassigned"
  )

  expect_identical(
    forward,
    reverse
  )

  expect_true(
    forward$assignment_method %in%
      c(
        "largest_overlap",
        "ambiguous"
      )
  )

  expect_equal(
    forward$candidate_n,
    2L
  )

  if (forward$assignment_method == "ambiguous") {

    expect_true(
      is.na(forward$polygon_id)
    )
  } else {

    expect_false(
      is.na(forward$polygon_id)
    )
  }
})


# -----------------------------------------------------------------------------
# 13. Build genuinely external H3 cells for nearest-assignment tests
# -----------------------------------------------------------------------------
#
# Construct this fixture explicitly rather than relying on state created by
# another test file. Expand the authoritative polygon coverage by several H3
# rings, then retain cells that remain unassigned under normal polygon
# ownership. These are the cells used to test `outside = "nearest"`.
# -----------------------------------------------------------------------------

assignment_h3 <- h3_cover_polygon(
  x = toy_membership_polygons,
  resolution = 9L,
  boundary = "center"
)

expanded_h3 <- expand_h3(
  assignment_h3,
  rings = 3L
)

expanded_unassigned <- h3_assign_polygon(
  x = expanded_h3,
  polygons = toy_membership_polygons,
  id = "feature_id",
  outside = "unassigned"
)

# -----------------------------------------------------------------------------
# 14. Nearest assignment — genuinely external cells
# -----------------------------------------------------------------------------

test_that("genuinely external cells receive nearest ownership", {

  outside_h3 <- expanded_unassigned$h3[
    expanded_unassigned$assignment_method == "unassigned"
  ]

  expect_gt(
    length(outside_h3),
    0L
  )

  nearest <- h3_assign_polygon(
    outside_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  expect_false(
    any(nearest$assignment_method == "unassigned")
  )

  expect_false(
    anyNA(nearest$polygon_id)
  )

  expect_true(
    all(nearest$assignment_method == "nearest")
  )

  expect_true(
    all(is.finite(nearest$assignment_distance))
  )

  expect_true(
    all(nearest$assignment_distance >= 0)
  )

  expect_true(
    all(nearest$candidate_n == 1L)
  )
})


# -----------------------------------------------------------------------------
# 15. Nearest assignment — polygon-order invariance
# -----------------------------------------------------------------------------

test_that("nearest ownership is polygon-order invariant", {

  outside_h3 <- expanded_unassigned$h3[
    expanded_unassigned$assignment_method == "unassigned"
  ]

  sample_n <- min(
    100L,
    length(outside_h3)
  )

  sample_h3 <- outside_h3[
    seq_len(sample_n)
  ]

  forward <- h3_assign_polygon(
    sample_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  reversed_polygons <- toy_membership_polygons[
    rev(seq_len(nrow(toy_membership_polygons))),
  ]

  reverse <- h3_assign_polygon(
    sample_h3,
    reversed_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  expect_identical(
    forward,
    reverse
  )
})


# -----------------------------------------------------------------------------
# 16. Donut and enclave
# -----------------------------------------------------------------------------

test_that("donut and enclave remain independent polygon ownership", {

  donut <- toy_membership_polygons[
    toy_membership_polygons$feature_id == "500",
  ]

  enclave <- toy_membership_polygons[
    toy_membership_polygons$feature_id == "600",
  ]

  expect_equal(
    nrow(donut),
    1L
  )

  expect_equal(
    nrow(enclave),
    1L
  )

  local_crs <- .make_local_crs(
    enclave
  )

  donut_projected <- sf::st_transform(
    donut,
    local_crs
  )

  enclave_projected <- sf::st_transform(
    enclave,
    local_crs
  )

  enclave_point <- sf::st_point_on_surface(
    sf::st_geometry(
      enclave_projected
    )
  )

  expect_false(
    as.logical(
      sf::st_within(
        enclave_point,
        donut_projected,
        sparse = FALSE
      )[1, 1]
    )
  )

  enclave_h3 <- h3_cover_polygon(
    enclave,
    resolution = 9,
    boundary = "center"
  )

  expect_gt(
    length(enclave_h3),
    0L
  )

  result <- h3_assign_polygon(
    enclave_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expect_true(
    all(result$polygon_id == "600")
  )
})


# -----------------------------------------------------------------------------
# 17. Multipart polygon
# -----------------------------------------------------------------------------

test_that("disconnected multipart geometry retains one polygon identifier", {

  multipart <- toy_membership_polygons[
    toy_membership_polygons$feature_id == "400",
  ]

  expect_equal(
    nrow(multipart),
    1L
  )

  expect_equal(
    as.character(
      sf::st_geometry_type(
        multipart
      )
    ),
    "MULTIPOLYGON"
  )

  multipart_h3 <- h3_cover_polygon(
    multipart,
    resolution = 9,
    boundary = "center"
  )

  expect_gt(
    length(multipart_h3),
    0L
  )

  result <- h3_assign_polygon(
    multipart_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expect_true(
    all(result$polygon_id == "400")
  )
})


# -----------------------------------------------------------------------------
# 18. Positive-area polygon overlap is rejected
# -----------------------------------------------------------------------------

test_that("positive-area overlap is rejected even when features are valid", {

  cell <- .make_h3_polygon(
    test_h3
  )

  local_crs <- .make_local_crs(
    cell
  )

  cell_projected <- sf::st_transform(
    cell,
    local_crs
  )

  bbox <- sf::st_bbox(
    cell_projected
  )

  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]

  polygon_a <- sf::st_polygon(
    list(
      matrix(
        c(
          bbox[["xmin"]] - width,
          bbox[["ymin"]] - height,

          bbox[["xmin"]] + 0.65 * width,
          bbox[["ymin"]] - height,

          bbox[["xmin"]] + 0.65 * width,
          bbox[["ymax"]] + height,

          bbox[["xmin"]] - width,
          bbox[["ymax"]] + height,

          bbox[["xmin"]] - width,
          bbox[["ymin"]] - height
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  polygon_b <- sf::st_polygon(
    list(
      matrix(
        c(
          bbox[["xmin"]] + 0.35 * width,
          bbox[["ymin"]] - height,

          bbox[["xmax"]] + width,
          bbox[["ymin"]] - height,

          bbox[["xmax"]] + width,
          bbox[["ymax"]] + height,

          bbox[["xmin"]] + 0.35 * width,
          bbox[["ymax"]] + height,

          bbox[["xmin"]] + 0.35 * width,
          bbox[["ymin"]] - height
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  bad_polygons <- sf::st_sf(
    polygon_id = c(
      "A",
      "B"
    ),
    geometry = sf::st_sfc(
      polygon_a,
      polygon_b,
      crs = local_crs
    )
  )

  expect_true(
    all(sf::st_is_valid(bad_polygons))
  )

  expect_error(
    h3_assign_polygon(
      test_h3,
      bad_polygons,
      id = "polygon_id",
      outside = "unassigned"
    ),
    "overlap",
    ignore.case = TRUE
  )
})