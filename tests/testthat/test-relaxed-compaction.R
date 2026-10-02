# =============================================================================
# h3compactR
# Tests for relaxed H3 compaction
# =============================================================================


# -----------------------------------------------------------------------------
# Test fixture
# -----------------------------------------------------------------------------

make_child_fixture <- function(n_children = 6L) {

  parent_h3 <- "87be63563ffffff"

  children_h3 <- h3jsr::get_children(
    parent_h3,
    res = 8,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    sort()

  list(
    parent_h3 = parent_h3,
    children_h3 = children_h3,
    source_h3 = children_h3[
      seq_len(n_children)
    ]
  )
}


# -----------------------------------------------------------------------------
# 1. Exact 7-of-7 behaviour
# -----------------------------------------------------------------------------

test_that(
  "min_coverage 1 reproduces exact promotion for a complete child set",
  {

    fixture <- make_child_fixture(7L)

    exact <- compact_h3(
      fixture$source_h3
    )

    relaxed <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 1,
      simple = TRUE
    )

    expect_setequal(
      relaxed,
      exact
    )

    expect_identical(
      length(relaxed),
      1L
    )

    expect_true(
      fixture$parent_h3 %in% relaxed
    )
  }
)


# -----------------------------------------------------------------------------
# 2. Six-of-seven relaxed promotion
# -----------------------------------------------------------------------------

test_that(
  "six of seven children promote at the six-sevenths threshold",
  {

    fixture <- make_child_fixture(6L)

    result <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 6 / 7,
      simple = FALSE
    )

    expect_identical(
      length(result$h3),
      1L
    )

    expect_identical(
      result$h3,
      fixture$parent_h3
    )

    expect_equal(
      result$metadata$coverage_ratio,
      6 / 7,
      tolerance = 1e-12
    )

    expect_identical(
      result$metadata$source_cell_count,
      6L
    )

    expect_identical(
      result$metadata$possible_source_cell_count,
      7L
    )

    expect_identical(
      result$metadata$spillover_cell_count,
      1L
    )

    expect_identical(
      result$metadata$compaction_mode,
      "relaxed"
    )
  }
)


# -----------------------------------------------------------------------------
# 3. Six-of-seven does not promote above threshold
# -----------------------------------------------------------------------------

test_that(
  "six of seven children remain fine resolution above threshold",
  {

    fixture <- make_child_fixture(6L)

    result <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 0.90,
      simple = FALSE
    )

    expect_identical(
      length(result$h3),
      6L
    )

    expect_false(
      fixture$parent_h3 %in%
        result$h3
    )

    expect_true(
      all(
        h3jsr::get_res(result$h3) == 8L
      )
    )

    expect_identical(
      result$qa$spillover_cells,
      0L
    )
  }
)


# -----------------------------------------------------------------------------
# 4. Five-of-seven threshold behaviour
# -----------------------------------------------------------------------------

test_that(
  "five of seven children promote only at an eligible threshold",
  {

    fixture <- make_child_fixture(5L)

    promoted <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 5 / 7,
      simple = FALSE
    )

    retained <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 0.72,
      simple = FALSE
    )

    expect_identical(
      promoted$h3,
      fixture$parent_h3
    )

    expect_identical(
      promoted$metadata$source_cell_count,
      5L
    )

    expect_identical(
      promoted$metadata$spillover_cell_count,
      2L
    )

    expect_identical(
      length(retained$h3),
      5L
    )

    expect_false(
      fixture$parent_h3 %in%
        retained$h3
    )
  }
)


# -----------------------------------------------------------------------------
# 5. Authoritative lookup contains source cells only
# -----------------------------------------------------------------------------

test_that(
  "relaxed lookup preserves authoritative membership without spillover",
  {

    fixture <- make_child_fixture(6L)

    missing_child <- setdiff(
      fixture$children_h3,
      fixture$source_h3
    )

    result <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 6 / 7,
      simple = FALSE
    )

    expect_identical(
      nrow(result$lookup),
      6L
    )

    expect_identical(
      dplyr::n_distinct(
        result$lookup$source_h3
      ),
      6L
    )

    expect_setequal(
      result$lookup$source_h3,
      fixture$source_h3
    )

    expect_true(
      all(
        result$lookup$compact_h3 ==
          fixture$parent_h3
      )
    )

    expect_false(
      missing_child %in%
        result$lookup$source_h3
    )
  }
)


# -----------------------------------------------------------------------------
# 6. Geometric round trip is a measured superset
# -----------------------------------------------------------------------------

test_that(
  "relaxed round trip preserves source support and quantifies spillover",
  {

    fixture <- make_child_fixture(6L)

    result <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 6 / 7,
      simple = FALSE
    )

    roundtrip <- uncompact_h3(
      result$h3,
      resolution = 8L
    )

    expect_identical(
      length(
        setdiff(
          fixture$source_h3,
          roundtrip
        )
      ),
      0L
    )

    expect_identical(
      length(
        setdiff(
          roundtrip,
          fixture$source_h3
        )
      ),
      1L
    )

    expect_identical(
      result$qa$roundtrip_missing,
      0L
    )

    expect_identical(
      result$qa$roundtrip_additional,
      1L
    )

    expect_true(
      result$qa$source_coverage_preserved
    )
  }
)


# -----------------------------------------------------------------------------
# 7. Aggregation is conserved
# -----------------------------------------------------------------------------

test_that(
  "authoritative values aggregate without leakage into spillover",
  {

    fixture <- make_child_fixture(6L)

    result <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 6 / 7,
      simple = FALSE
    )

    source_data <- tibble::tibble(
      source_h3 = fixture$source_h3,
      event_count = seq_len(6L),
      exposure = seq_len(6L) * 100
    )

    mapped <- source_data |>
      dplyr::left_join(
        result$lookup,
        by = "source_h3"
      )

    aggregated <- mapped |>
      dplyr::group_by(
        .data$compact_h3
      ) |>
      dplyr::summarise(
        event_count =
          sum(.data$event_count),
        exposure =
          sum(.data$exposure),
        .groups = "drop"
      )

    expect_equal(
      sum(aggregated$event_count),
      sum(source_data$event_count)
    )

    expect_equal(
      sum(aggregated$exposure),
      sum(source_data$exposure)
    )

    expect_false(
      anyNA(mapped$compact_h3)
    )
  }
)


# -----------------------------------------------------------------------------
# 8. Determinism
# -----------------------------------------------------------------------------

test_that(
  "relaxed compaction is independent of input order",
  {

    fixture <- make_child_fixture(6L)

    forward <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 6 / 7,
      simple = FALSE
    )

    reverse <- compact_h3_relaxed(
      rev(fixture$source_h3),
      min_coverage = 6 / 7,
      simple = FALSE
    )

    expect_setequal(
      forward$h3,
      reverse$h3
    )

    expect_equal(
      forward$lookup |>
        dplyr::arrange(.data$source_h3),
      reverse$lookup |>
        dplyr::arrange(.data$source_h3)
    )
  }
)


# -----------------------------------------------------------------------------
# 9. No hierarchy overlap
# -----------------------------------------------------------------------------

test_that(
  "relaxed output contains no parent descendant overlap",
  {

    fixture <- make_child_fixture(6L)

    result <- compact_h3_relaxed(
      fixture$source_h3,
      min_coverage = 6 / 7,
      simple = FALSE
    )

    hierarchy <- check_h3_hierarchy(
      result$h3
    )

    expect_identical(
      nrow(hierarchy),
      0L
    )
  }
)


# -----------------------------------------------------------------------------
# 10. Invalid coverage thresholds are rejected
# -----------------------------------------------------------------------------

test_that(
  "invalid relaxed coverage thresholds are rejected",
  {

    fixture <- make_child_fixture(6L)

    expect_error(
      compact_h3_relaxed(
        fixture$source_h3,
        min_coverage = 0
      ),
      "min_coverage"
    )

    expect_error(
      compact_h3_relaxed(
        fixture$source_h3,
        min_coverage = 1.01
      ),
      "min_coverage"
    )

    expect_error(
      compact_h3_relaxed(
        fixture$source_h3,
        min_coverage = NA_real_
      ),
      "min_coverage"
    )
  }
)