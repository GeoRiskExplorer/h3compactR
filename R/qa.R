#' Validate an H3 compaction round trip
#'
#' @param source_h3 Original source H3 cells.
#' @param compacted_h3 Compacted H3 cells.
#' @param target_resolution Resolution used to reconstruct the source grid.
#'
#' @return A one-row QA tibble.
#' @export
qa_h3_compaction <- function(
    source_h3,
    compacted_h3,
    target_resolution = max(h3jsr::get_res(source_h3))
) {
  source_h3 <- validate_h3(source_h3)
  compacted_h3 <- validate_h3(compacted_h3)

  reconstructed_h3 <- h3jsr::uncompact(
    compacted_h3,
    res = target_resolution,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  hierarchy_qa <- check_h3_hierarchy(compacted_h3)

  tibble::tibble(
    source_cells = length(source_h3),
    compacted_cells = length(compacted_h3),
    reduction_n = length(source_h3) - length(compacted_h3),
    reduction_pct = round(
      100 * (
        length(source_h3) - length(compacted_h3)
      ) / length(source_h3),
      2
    ),
    compact_min_resolution = min(h3jsr::get_res(compacted_h3)),
    compact_max_resolution = max(h3jsr::get_res(compacted_h3)),
    hierarchy_overlaps = nrow(hierarchy_qa),
    roundtrip_missing = length(
      setdiff(source_h3, reconstructed_h3)
    ),
    roundtrip_additional = length(
      setdiff(reconstructed_h3, source_h3)
    ),
    roundtrip_equal = setequal(
      source_h3,
      reconstructed_h3
    )
  )
}

#' Score an H3 compaction result
#'
#' Calculates structural, coverage and cartographic diagnostics for an H3
#' compaction result. The function does not modify the H3 grid.
#'
#' Exact round-trip equality is assessed at the finest resolution present
#' across the source and compacted H3 sets. A refined result may preserve all
#' source coverage while also extending coverage beyond the original source
#' set.
#'
#' @param source_h3 Original source H3 indexes.
#' @param compacted_h3 Compacted or refined H3 indexes.
#' @param container Optional `sf` or `sfc` polygon used to calculate container
#'   coverage and gap area.
#' @param source_count Optional total count before aggregation.
#' @param compacted_count Optional total count after aggregation.
#' @param analysis_crs Projected CRS used for area calculations.
#'
#' @return A one-row tibble containing compaction diagnostics.
#' @export
score_h3_compaction <- function(
    source_h3,
    compacted_h3,
    container = NULL,
    source_count = NULL,
    compacted_count = NULL,
    analysis_crs = 7899
) {
  source_h3 <- validate_h3(source_h3)
  compacted_h3 <- validate_h3(compacted_h3)

  if (length(source_h3) == 0L) {
    cli::cli_abort("`source_h3` must contain at least one H3 index.")
  }

  if (length(compacted_h3) == 0L) {
    cli::cli_abort("`compacted_h3` must contain at least one H3 index.")
  }

  source_resolutions <- h3jsr::get_res(source_h3)
  compacted_resolutions <- h3jsr::get_res(compacted_h3)

  source_resolution <- max(source_resolutions)

  comparison_resolution <- max(
    source_resolutions,
    compacted_resolutions
  )

  source_comparison_h3 <- uncompact_h3(
    source_h3,
    resolution = comparison_resolution
  )

  reconstructed_h3 <- uncompact_h3(
    compacted_h3,
    resolution = comparison_resolution
  )

  hierarchy_qa <- check_h3_hierarchy(compacted_h3)

  source_n <- length(source_h3)
  compacted_n <- length(compacted_h3)

  reduction_n <- source_n - compacted_n
  reduction_pct <- 100 * reduction_n / source_n

  missing_h3 <- length(
    setdiff(
      source_comparison_h3,
      reconstructed_h3
    )
  )

  additional_h3 <- length(
    setdiff(
      reconstructed_h3,
      source_comparison_h3
    )
  )

  roundtrip_equal <-
    missing_h3 == 0L &&
    additional_h3 == 0L

  coverage_preserved <- missing_h3 == 0L
  coverage_extended <- additional_h3 > 0L

  count_difference <- NA_real_
  count_preserved <- NA

  if (!is.null(source_count) || !is.null(compacted_count)) {
    if (is.null(source_count) || is.null(compacted_count)) {
      cli::cli_abort(
        "`source_count` and `compacted_count` must be supplied together."
      )
    }

    if (
      length(source_count) != 1L ||
      length(compacted_count) != 1L ||
      !is.numeric(source_count) ||
      !is.numeric(compacted_count) ||
      is.na(source_count) ||
      is.na(compacted_count)
    ) {
      cli::cli_abort(
        paste0(
          "`source_count` and `compacted_count` must each be ",
          "one non-missing numeric value."
        )
      )
    }

    count_difference <- compacted_count - source_count

    count_preserved <- isTRUE(
      all.equal(
        source_count,
        compacted_count,
        tolerance = 1e-10
      )
    )
  }

  container_area_m2 <- NA_real_
  covered_area_m2 <- NA_real_
  gap_area_m2 <- NA_real_
  gap_pct <- NA_real_
  coverage_pct <- NA_real_

  if (!is.null(container)) {
    if (!inherits(container, c("sf", "sfc"))) {
      cli::cli_abort(
        "`container` must be an sf or sfc object."
      )
    }

    container_sf <- if (inherits(container, "sfc")) {
      sf::st_sf(
        container_id = seq_along(container),
        geometry = container
      )
    } else {
      container
    }

    if (is.na(sf::st_crs(container_sf))) {
      cli::cli_abort(
        "`container` must have a defined CRS."
      )
    }

    if (any(sf::st_is_empty(container_sf))) {
      cli::cli_abort(
        "`container` contains empty geometry."
      )
    }

    container_proj <- container_sf |>
      sf::st_make_valid() |>
      sf::st_transform(analysis_crs)

    container_union <- sf::st_union(
      sf::st_geometry(container_proj)
    )

    compacted_geometry <- h3_compaction_geometry(
      h3 = compacted_h3,
      output_crs = analysis_crs
    ) |>
      sf::st_make_valid()

    covered_geometry <- suppressWarnings(
      sf::st_intersection(
        sf::st_geometry(compacted_geometry),
        container_union
      )
    )

    covered_geometry <- covered_geometry[
      !sf::st_is_empty(covered_geometry)
    ]

    container_area_m2 <- as.numeric(
      sum(sf::st_area(container_union))
    )

    covered_area_m2 <- if (length(covered_geometry) == 0L) {
      0
    } else {
      as.numeric(
        sum(
          sf::st_area(
            sf::st_union(covered_geometry)
          )
        )
      )
    }

    # Small positive differences can occur through projection and overlay
    # precision, so the reported gap is constrained to zero or greater.
    gap_area_m2 <- max(
      container_area_m2 - covered_area_m2,
      0
    )

    if (container_area_m2 > 0) {
      coverage_pct <- min(
        100 * covered_area_m2 / container_area_m2,
        100
      )

      gap_pct <- 100 * gap_area_m2 / container_area_m2
    }
  }

  resolution_summary <- summarise_h3_resolution(
    compacted_h3
  )

  tibble::tibble(
    source_cells = source_n,
    compacted_cells = compacted_n,
    reduction_n = reduction_n,
    reduction_pct = round(reduction_pct, 2),
    source_resolution = source_resolution,
    comparison_resolution = comparison_resolution,
    compact_min_resolution = min(compacted_resolutions),
    compact_max_resolution = max(compacted_resolutions),
    resolution_count = nrow(resolution_summary),
    hierarchy_overlaps = nrow(hierarchy_qa),
    roundtrip_missing = missing_h3,
    roundtrip_additional = additional_h3,
    roundtrip_equal = roundtrip_equal,
    coverage_preserved = coverage_preserved,
    coverage_extended = coverage_extended,
    source_count = if (is.null(source_count)) {
      NA_real_
    } else {
      source_count
    },
    compacted_count = if (is.null(compacted_count)) {
      NA_real_
    } else {
      compacted_count
    },
    count_difference = count_difference,
    count_preserved = count_preserved,
    container_area_m2 = container_area_m2,
    covered_area_m2 = covered_area_m2,
    gap_area_m2 = gap_area_m2,
    gap_pct = round(gap_pct, 4),
    coverage_pct = round(coverage_pct, 2)
  )
}