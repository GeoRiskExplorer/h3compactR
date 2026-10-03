#' Cover polygons with H3 cells
#'
#' Creates an H3 representation of polygon geometry using one of four
#' explicitly defined polygon coverage rules.
#'
#' H3 indexing is performed in longitude/latitude coordinates using
#' EPSG:4326. Input geometry may use another coordinate reference system and
#' is transformed internally.
#'
#' @param x An `sf` or `sfc` object containing `POLYGON` or `MULTIPOLYGON`
#'   geometry.
#' @param resolution A single integer H3 resolution from 0 to 15.
#' @param boundary Character string defining the polygon-to-H3 coverage rule.
#'   One of `"center"`, `"within"`, `"intersects"`, or `"overlap"`.
#' @param min_overlap Minimum proportion of an H3 cell that must be covered by
#'   the source polygon when `boundary = "overlap"`. Must be greater than 0
#'   and less than or equal to 1. Must be `NULL` for all other boundary rules.
#'
#' @details
#' The available coverage rules answer different spatial questions:
#'
#' * `"center"` uses standard H3 centre-based polygon coverage. A cell is
#'   included when its H3 cell centre lies within the source polygon.
#' * `"within"` retains only H3 cells completely contained by the source
#'   polygon.
#' * `"intersects"` retains H3 cells that have any geometric intersection
#'   with the source polygon.
#' * `"overlap"` retains H3 cells where at least `min_overlap` of the H3
#'   cell area is covered by the source polygon.
#'
#' For overlap coverage, the proportion is defined as
#'
#' \deqn{
#'   area(H3 cell intersect polygon) / area(H3 cell)
#' }
#'
#' When `min_overlap = 1`, complete geometric containment is evaluated using
#' the same topological containment rule as `boundary = "within"`. This avoids
#' numerical ambiguity from comparing projected area ratios to exactly one.
#'
#' Area calculations for proportional overlap thresholds below one are
#' performed in a local projected coordinate reference system rather than
#' directly in longitude/latitude coordinates.
#'
#' The function returns H3 indexes as character values. It does not return
#' polygon geometry and does not perform H3 compaction.
#'
#' Geometric coverage rules such as `"within"`, `"intersects"`, and
#' `"overlap"` require H3 polygon conversion and spatial predicates and are
#' therefore expected to be slower and more memory intensive than standard
#' centre-based coverage.
#'
#' Large polygons at fine H3 resolutions can generate millions of cells and
#' may require substantial memory. The function does not impose an arbitrary
#' maximum polygon size or H3 resolution.
#'
#' @return A character vector containing unique H3 cell indexes. If no cells
#'   satisfy the requested coverage rule, `character(0)` is returned.
#'
#' @references
#' H3 indexing: <https://h3geo.org/>. R access is provided through O'Brien's
#' `h3jsr` package (\doi{10.32614/CRAN.package.h3jsr}). Spatial geometry
#' operations use `sf`; see Pebesma (2018), \doi{10.32614/RJ-2018-009}.
#'
#' @export
h3_cover_polygon <- function(
  x,
  resolution,
  boundary = "center",
  min_overlap = NULL
) {

  # ---------------------------------------------------------------------------
  # 1. Validate input object
  # ---------------------------------------------------------------------------

  if (
    !inherits(x, "sf") &&
      !inherits(x, "sfc")
  ) {
    cli::cli_abort(
      "`x` must be an {.cls sf} or {.cls sfc} object."
    )
  }

  geometry <- if (inherits(x, "sf")) {
    sf::st_geometry(x)
  } else {
    x
  }

  if (length(geometry) == 0L) {
    cli::cli_abort(
      "`x` must contain at least one geometry."
    )
  }

  if (is.na(sf::st_crs(geometry))) {
    cli::cli_abort(
      "`x` must have a defined coordinate reference system."
    )
  }

  if (any(sf::st_is_empty(geometry))) {
    cli::cli_abort(
      "`x` must not contain empty geometry."
    )
  }

  geometry_type <- as.character(
    sf::st_geometry_type(
      geometry,
      by_geometry = TRUE
    )
  )

  if (
    any(
      !geometry_type %in% c(
        "POLYGON",
        "MULTIPOLYGON"
      )
    )
  ) {
    cli::cli_abort(
      "`x` must contain only POLYGON or MULTIPOLYGON geometry."
    )
  }

  geometry_valid <- sf::st_is_valid(
    geometry
  )

  if (
    anyNA(geometry_valid) ||
      !all(geometry_valid)
  ) {
    cli::cli_abort(
      "`x` contains invalid polygon geometry."
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Validate H3 resolution
  # ---------------------------------------------------------------------------

  if (
    length(resolution) != 1L ||
      !is.numeric(resolution) ||
      is.na(resolution) ||
      !is.finite(resolution) ||
      resolution != as.integer(resolution) ||
      resolution < 0 ||
      resolution > 15
  ) {
    cli::cli_abort(
      "`resolution` must be a single integer between 0 and 15."
    )
  }

  resolution <- as.integer(
    resolution
  )

  # ---------------------------------------------------------------------------
  # 3. Validate boundary rule
  # ---------------------------------------------------------------------------

  valid_boundaries <- c(
    "center",
    "within",
    "intersects",
    "overlap"
  )

  if (
    length(boundary) != 1L ||
      !is.character(boundary) ||
      is.na(boundary) ||
      !boundary %in% valid_boundaries
  ) {
    cli::cli_abort(
      "`boundary` must be one of: {.val center}, {.val within}, {.val intersects}, or {.val overlap}."
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Validate overlap parameter
  # ---------------------------------------------------------------------------

  if (boundary == "overlap") {

    if (
      is.null(min_overlap) ||
        length(min_overlap) != 1L ||
        !is.numeric(min_overlap) ||
        is.na(min_overlap) ||
        !is.finite(min_overlap) ||
        min_overlap <= 0 ||
        min_overlap > 1
    ) {
      cli::cli_abort(
        "`min_overlap` must be a single numeric value greater than 0 and less than or equal to 1 when `boundary = \"overlap\"`."
      )
    }

  } else if (!is.null(min_overlap)) {

    cli::cli_abort(
      "`min_overlap` is only used when `boundary = \"overlap\"`."
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Transform source geometry to H3 coordinate system
  # ---------------------------------------------------------------------------

  geometry_4326 <- sf::st_transform(
    geometry,
    4326
  )

  # Treat all supplied polygon geometry as one coverage domain.
  coverage_geometry <- sf::st_union(
    geometry_4326
  )

  # ---------------------------------------------------------------------------
  # 6. Generate standard H3 centre coverage
  # ---------------------------------------------------------------------------

  center_cells <- h3jsr::polygon_to_cells(
    geometry_4326,
    res = resolution,
    simple = TRUE
  )

  center_cells <- as.character(
    unlist(
      center_cells,
      use.names = FALSE
    )
  )

  # h3jsr can represent an empty polygon coverage result as NA.
  # Package output normalises this to character(0).
  center_cells <- center_cells[
    !is.na(center_cells) &
      nzchar(center_cells)
  ]

  center_cells <- unique(
    center_cells
  )

  if (length(center_cells) > 0L) {

    center_valid <- h3jsr::is_valid(
      center_cells
    )

    if (
      anyNA(center_valid) ||
        !all(center_valid)
    ) {
      cli::cli_abort(
        "H3 centre coverage returned an invalid H3 index."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Centre coverage
  # ---------------------------------------------------------------------------

  if (boundary == "center") {
    return(center_cells)
  }

  # ---------------------------------------------------------------------------
  # 8. Within coverage
  #
  # A cell completely contained by the source polygon must also have its
  # centre inside the polygon. Standard centre coverage therefore provides
  # the complete candidate set for this rule.
  # ---------------------------------------------------------------------------

  if (boundary == "within") {
    return(
      .cover_within(
        cells = center_cells,
        geometry_4326 = coverage_geometry
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 9. Intersects coverage
  #
  # Candidate generation includes standard centre coverage and polygon-derived
  # seed cells. This allows polygons smaller than an H3 cell to generate
  # intersecting candidates even when centre coverage is empty.
  # ---------------------------------------------------------------------------

  if (boundary == "intersects") {

    candidate_cells <- .cover_candidates(
      center_cells = center_cells,
      geometry_4326 = geometry_4326,
      resolution = resolution
    )

    if (length(candidate_cells) == 0L) {
      return(character())
    }

    candidate_geometry <- h3jsr::cell_to_polygon(
      candidate_cells,
      simple = FALSE
    )

    candidate_geometry <- sf::st_transform(
      candidate_geometry,
      4326
    )

    keep <- lengths(
      sf::st_intersects(
        sf::st_geometry(candidate_geometry),
        coverage_geometry
      )
    ) > 0L

    result <- candidate_cells[
      keep
    ]

    return(
      unique(
        as.character(result)
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 10. Proportional overlap coverage
  #
  # overlap =
  #   area(H3 cell intersect polygon) /
  #   area(H3 cell)
  #
  # The intersecting candidate universe is reused here. Area calculations are
  # performed in a local Azimuthal Equidistant projection centred on the
  # source coverage geometry.
  # ---------------------------------------------------------------------------

  if (boundary == "overlap") {

    candidate_cells <- .cover_candidates(
      center_cells = center_cells,
      geometry_4326 = geometry_4326,
      resolution = resolution
    )

    if (length(candidate_cells) == 0L) {
      return(character())
    }

    # -------------------------------------------------------------------------
    # 10.1 Exact complete containment
    #
    # A threshold of 1 means complete geometric containment. Use the same
    # topological operation as `boundary = "within"` rather than relying on
    # floating-point equality of projected area ratios.
    # -------------------------------------------------------------------------

    if (identical(min_overlap, 1)) {
      return(
        .cover_within(
          cells = candidate_cells,
          geometry_4326 = coverage_geometry
        )
      )
    }

    candidate_geometry <- h3jsr::cell_to_polygon(
      candidate_cells,
      simple = FALSE
    )

    candidate_geometry <- sf::st_transform(
      candidate_geometry,
      4326
    )

    # -------------------------------------------------------------------------
    # 10.2 Create local projected CRS
    # -------------------------------------------------------------------------

    bbox <- sf::st_bbox(
      coverage_geometry
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

    local_crs <- paste(
      "+proj=aeqd",
      paste0("+lat_0=", lat_0),
      paste0("+lon_0=", lon_0),
      "+datum=WGS84",
      "+units=m",
      "+no_defs"
    )

    cells_projected <- sf::st_transform(
      candidate_geometry,
      local_crs
    )

    polygon_projected <- sf::st_transform(
      coverage_geometry,
      local_crs
    )

    # -------------------------------------------------------------------------
    # 10.3 Calculate complete H3 cell areas
    # -------------------------------------------------------------------------

    cell_area <- as.numeric(
      sf::st_area(
        cells_projected
      )
    )

    if (
      anyNA(cell_area) ||
        any(!is.finite(cell_area)) ||
        any(cell_area <= 0)
    ) {
      cli::cli_abort(
        "Unable to calculate valid H3 cell areas for overlap coverage."
      )
    }

    # -------------------------------------------------------------------------
    # 10.4 Calculate intersection areas
    # -------------------------------------------------------------------------

    intersection <- suppressWarnings(
      sf::st_intersection(
        sf::st_geometry(cells_projected),
        polygon_projected
      )
    )

    intersection_area <- numeric(
      length(candidate_cells)
    )

    if (length(intersection) > 0L) {

      hit_index <- attr(
        intersection,
        "idx"
      )

      if (is.null(hit_index)) {
        cli::cli_abort(
          "Unable to recover H3 cell relationships from overlap geometry."
        )
      }

      areas <- as.numeric(
        sf::st_area(
          intersection
        )
      )

      if (
        anyNA(areas) ||
          any(!is.finite(areas))
      ) {
        cli::cli_abort(
          "Unable to calculate valid intersection areas for overlap coverage."
        )
      }

      area_by_cell <- rowsum(
        areas,
        group = hit_index[, 1L],
        reorder = FALSE
      )

      cell_index <- as.integer(
        rownames(area_by_cell)
      )

      intersection_area[
        cell_index
      ] <- area_by_cell[, 1L]
    }

    # -------------------------------------------------------------------------
    # 10.5 Calculate proportional overlap
    # -------------------------------------------------------------------------

    overlap_fraction <- intersection_area / cell_area

    keep <- overlap_fraction >= min_overlap

    result <- candidate_cells[
      keep
    ]

    return(
      unique(
        as.character(result)
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 11. Defensive fallback
  # ---------------------------------------------------------------------------

  character()
}