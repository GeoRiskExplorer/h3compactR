test_that("aggregation lookup is complete and unique", {

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
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  compacted_h3 <- compact_h3(source_h3)

  lookup <- build_h3_aggregation_lookup(
    source_h3 = source_h3,
    compacted_h3 = compacted_h3
  )

  expect_equal(
    nrow(lookup),
    length(source_h3)
  )

  expect_equal(
    dplyr::n_distinct(lookup$source_h3),
    length(source_h3)
  )

  expect_setequal(
    lookup$source_h3,
    source_h3
  )

  expect_setequal(
    unique(lookup$compact_h3),
    compacted_h3
  )

  expect_equal(
    anyDuplicated(lookup$source_h3),
    0L
  )

  expect_false(
    anyNA(lookup$compact_h3)
  )

  expect_true(
    all(
      h3jsr::get_res(lookup$compact_h3) ==
        lookup$compact_resolution
    )
  )
})


test_that("aggregation lookup records valid H3 ancestry", {

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
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  compacted_h3 <- compact_h3(source_h3)

  lookup <- build_h3_aggregation_lookup(
    source_h3 = source_h3,
    compacted_h3 = compacted_h3
  )

  expected_compact_h3 <- mapply(
    FUN = function(source_h3, compact_h3, compact_resolution) {

      if (source_h3 == compact_h3) {
        return(source_h3)
      }

      h3jsr::get_parent(
        source_h3,
        res = compact_resolution,
        simple = TRUE
      ) |>
        unlist(use.names = FALSE)
    },
    source_h3 = lookup$source_h3,
    compact_h3 = lookup$compact_h3,
    compact_resolution = lookup$compact_resolution,
    USE.NAMES = FALSE
  )

  expect_equal(
    expected_compact_h3,
    lookup$compact_h3
  )
})


test_that("aggregation lookup is deterministic", {

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
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  compacted_h3 <- compact_h3(source_h3)

  lookup_a <- build_h3_aggregation_lookup(
    source_h3 = source_h3,
    compacted_h3 = compacted_h3
  ) |>
    dplyr::arrange(.data$source_h3)

  lookup_b <- build_h3_aggregation_lookup(
    source_h3 = rev(source_h3),
    compacted_h3 = compacted_h3
  ) |>
    dplyr::arrange(.data$source_h3)

  expect_identical(
    lookup_a,
    lookup_b
  )
})


test_that("aggregation through lookup preserves additive values", {

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
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  compacted_h3 <- compact_h3(source_h3)

  lookup <- build_h3_aggregation_lookup(
    source_h3 = source_h3,
    compacted_h3 = compacted_h3
  )

  source_data <- tibble::tibble(
    source_h3 = sort(source_h3)
  ) |>
    dplyr::mutate(
      sequence_value = dplyr::row_number(),
      event_count = .data$sequence_value %% 7L,
      exposure_count = .data$sequence_value * 10L,
      expected_count = .data$sequence_value / 100
    )

  compact_data <- source_data |>
    dplyr::left_join(
      lookup,
      by = "source_h3"
    ) |>
    dplyr::group_by(
      .data$compact_h3,
      .data$compact_resolution
    ) |>
    dplyr::summarise(
      sequence_value = sum(.data$sequence_value),
      event_count = sum(.data$event_count),
      exposure_count = sum(.data$exposure_count),
      expected_count = sum(.data$expected_count),
      .groups = "drop"
    )

  expect_equal(
    sum(source_data$sequence_value),
    sum(compact_data$sequence_value)
  )

  expect_equal(
    sum(source_data$event_count),
    sum(compact_data$event_count)
  )

  expect_equal(
    sum(source_data$exposure_count),
    sum(compact_data$exposure_count)
  )

  expect_equal(
    sum(source_data$expected_count),
    sum(compact_data$expected_count),
    tolerance = 1e-10
  )
})


test_that("aggregation lookup rejects duplicate source support", {

  parent <- "87be63563ffffff"

  children <- h3jsr::get_children(
    parent,
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  source_h3 <- c(
    children,
    children[[1]]
  )

  expect_error(
    build_h3_aggregation_lookup(
      source_h3 = source_h3,
      compacted_h3 = parent
    ),
    "Duplicated H3 indexes"
  )
})


test_that("aggregation lookup rejects incomplete compact support", {

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
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  compacted_h3 <- compact_h3(source_h3)

  incomplete_compact <- compacted_h3[-1]

  expect_error(
    build_h3_aggregation_lookup(
      source_h3 = source_h3,
      compacted_h3 = incomplete_compact
    ),
    "could not be matched"
  )
})