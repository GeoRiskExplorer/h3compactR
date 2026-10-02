# =============================================================================
# h3compactR
# Relaxed compaction regression test — irregular/discontinuous support
# =============================================================================


test_that(
  "relaxed compaction improves reduction while preserving source membership",
  {

    # -------------------------------------------------------------------------
    # 1. Build three irregular/discontinuous polygons
    # -------------------------------------------------------------------------

    poly_1 <- sf::st_polygon(
      list(
        matrix(
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
        )
      )
    )

    poly_2 <- sf::st_polygon(
      list(
        matrix(
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
        )
      )
    )

    poly_3 <- sf::st_polygon(
      list(
        matrix(
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
        )
      )
    )

    polygons <- sf::st_sf(
      polygon_id = c(
        "large_irregular",
        "medium_narrow",
        "small_compact"
      ),
      geometry = sf::st_sfc(
        poly_1,
        poly_2,
        poly_3,
        crs = 4326
      )
    )


    # -------------------------------------------------------------------------
    # 2. Build authoritative Res 8 support
    # -------------------------------------------------------------------------

    source_h3 <- h3jsr::polygon_to_cells(
      polygons,
      res = 8,
      simple = TRUE
    ) |>
      unlist(use.names = FALSE) |>
      unique() |>
      sort()


    # -------------------------------------------------------------------------
    # 3. Exact and relaxed compaction
    # -------------------------------------------------------------------------

    exact_h3 <- compact_h3(
      source_h3
    )

    relaxed <- compact_h3_relaxed(
      h3 = source_h3,
      min_coverage = 5 / 7,
      min_resolution = 6L,
      simple = FALSE
    )


    # -------------------------------------------------------------------------
    # 4. Core regression expectations
    # -------------------------------------------------------------------------

    expect_lt(
      length(relaxed$h3),
      length(exact_h3)
    )

    expect_gt(
      relaxed$qa$spillover_cells,
      0L
    )

    expect_equal(
      relaxed$qa$missing_source_memberships,
      0L
    )

    expect_equal(
      relaxed$qa$duplicate_source_memberships,
      0L
    )

    expect_equal(
      relaxed$qa$hierarchy_overlaps,
      0L
    )

    expect_equal(
      relaxed$qa$roundtrip_missing,
      0L
    )

    expect_true(
      relaxed$qa$source_coverage_preserved
    )

    expect_equal(
      nrow(relaxed$lookup),
      length(source_h3)
    )

    expect_setequal(
      relaxed$lookup$source_h3,
      source_h3
    )
  }
)