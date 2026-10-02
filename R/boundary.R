#' Refine H3 coverage along a container boundary
#'
#' Adds finer cells around uncovered container edges while preserving the
#' compacted interior hierarchy.
#'
#' @param source_h3 Original source H3 cells.
#' @param compacted_h3 Compacted mixed-resolution H3 cells.
#' @param container An sf or sfc polygon.
#' @param boundary_resolution Finer boundary resolution.
#' @param analysis_crs Projected CRS for geometry operations.
#' @return A list with refined H3 indexes, display geometry and QA.
#' @export
refine_h3_boundary <- function(
    source_h3,
    compacted_h3,
    container,
    boundary_resolution,
    analysis_crs = 7899
) {
  source_h3 <- validate_h3(source_h3)
  compacted_h3 <- validate_h3(compacted_h3)

  if (!inherits(container, c("sf", "sfc"))) {
    cli::cli_abort("`container` must be an sf or sfc polygon.")
  }

  container_sf <- if (inherits(container, "sfc")) {
    sf::st_sf(container_id = 1L, geometry = container)
  } else {
    container
  }

  container_sf <- sf::st_make_valid(container_sf)

  source_res <- unique(h3jsr::get_res(source_h3))

  if (length(source_res) != 1L) {
    cli::cli_abort("`source_h3` must contain one source resolution.")
  }

  if (boundary_resolution <= source_res) {
    cli::cli_abort("`boundary_resolution` must be finer than source resolution.")
  }

  if (nrow(check_h3_hierarchy(compacted_h3)) > 0L) {
    cli::cli_abort("`compacted_h3` contains hierarchy overlap.")
  }

  compacted_sf <- h3jsr::cell_to_polygon(compacted_h3, simple = FALSE)
  names(compacted_sf)[names(compacted_sf) == "h3_address"] <- "h3"
  compacted_sf$resolution <- h3jsr::get_res(compacted_sf$h3)
  compacted_sf$grid_role <- "compacted_interior"

  container_proj <- sf::st_transform(container_sf, analysis_crs)

  compacted_display <- suppressWarnings(
    sf::st_intersection(
      sf::st_transform(compacted_sf, analysis_crs),
      container_proj
    )
  )
  compacted_display <- compacted_display[!sf::st_is_empty(compacted_display), ]

  gap <- suppressWarnings(
    sf::st_difference(
      sf::st_geometry(container_proj),
      sf::st_union(sf::st_geometry(compacted_display))
    )
  ) |>
    sf::st_make_valid()

  initial_gap_area_m2 <- as.numeric(sum(sf::st_area(gap)))

  source_children <- h3jsr::get_children(
    h3_address = source_h3,
    res = boundary_resolution,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique()

  candidate_h3 <- h3jsr::get_disk(
    h3_address = source_children,
    ring_size = 1,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE) |>
    unique() |>
    validate_h3()

  candidate_parent <- h3jsr::get_parent(
    h3_address = candidate_h3,
    res = source_res,
    simple = TRUE
  ) |>
    unlist(use.names = FALSE)

  candidate_tbl <- tibble::tibble(
    h3 = candidate_h3,
    parent_h3 = candidate_parent
  ) |>
    dplyr::filter(!.data$parent_h3 %in% source_h3)

  candidate_sf <- h3jsr::cell_to_polygon(
    candidate_tbl$h3,
    simple = FALSE
  )
  names(candidate_sf)[names(candidate_sf) == "h3_address"] <- "h3"

  candidate_sf <- dplyr::left_join(candidate_sf, candidate_tbl, by = "h3")
  candidate_sf$resolution <- boundary_resolution
  candidate_sf$grid_role <- "boundary_refinement"

  candidate_proj <- sf::st_make_valid(
    sf::st_transform(candidate_sf, analysis_crs)
  )

  keep <- lengths(sf::st_intersects(candidate_proj, gap)) > 0L
  boundary_proj <- candidate_proj[keep, ]
  boundary_h3 <- unique(boundary_proj$h3)

  refined_h3 <- validate_h3(unique(c(compacted_h3, boundary_h3)))

  if (nrow(check_h3_hierarchy(refined_h3)) > 0L) {
    cli::cli_abort("Boundary refinement produced hierarchy overlap.")
  }

  boundary_display <- suppressWarnings(
    sf::st_intersection(boundary_proj, gap)
  )
  boundary_display <- boundary_display[!sf::st_is_empty(boundary_display), ]

  refined_display <- dplyr::bind_rows(
    compacted_display,
    boundary_display
  )

  remaining_gap <- suppressWarnings(
    sf::st_difference(
      sf::st_geometry(container_proj),
      sf::st_union(sf::st_geometry(refined_display))
    )
  ) |>
    sf::st_make_valid()

  remaining_gap_area_m2 <- as.numeric(sum(sf::st_area(remaining_gap)))

  qa <- tibble::tibble(
    source_cells = length(source_h3),
    compacted_cells = length(compacted_h3),
    boundary_cells = length(boundary_h3),
    refined_cells = length(refined_h3),
    initial_gap_area_m2 = initial_gap_area_m2,
    remaining_gap_area_m2 = remaining_gap_area_m2,
    gap_reduction_pct = if (initial_gap_area_m2 > 0) {
      100 * (initial_gap_area_m2 - remaining_gap_area_m2) /
        initial_gap_area_m2
    } else {
      100
    },
    hierarchy_overlaps = 0L
  )

  list(
    h3 = refined_h3,
    boundary_h3 = boundary_h3,
    display_geometry = sf::st_transform(refined_display, 4326),
    remaining_gap = sf::st_transform(
      sf::st_sf(geometry = remaining_gap),
      4326
    ),
    qa = qa
  )
}
