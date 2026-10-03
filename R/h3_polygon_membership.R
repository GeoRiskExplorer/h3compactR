#' Relate H3 cells to polygon features
#'
#' Creates an explicit spatial relationship between H3 cells and a
#' topologically sound polygon geography.
#'
#' @param x A character vector of valid H3 cell indexes.
#' @param polygons An `sf` object containing polygon or multipolygon features.
#' @param id Name of the polygon identifier column.
#'
#' @return A tibble containing `h3` and `polygon_id`. A H3 cell may appear
#'   more than once where it intersects more than one valid polygon feature.
#'
#' @details
#' Polygon features may share boundaries but must not have positive-area
#' overlap. Overlapping polygon interiors are rejected because they do not
#' provide a suitable basis for unambiguous polygon membership.
#'
#' This function records spatial relationships only. It does not assign
#' exclusive ownership, allocate polygon values, aggregate attributes, or
#' alter H3 geometry.
#'
#' @references
#' Spatial vector and geometric operations use `sf`; see Pebesma E (2018),
#' \doi{10.32614/RJ-2018-009}. H3 geometry access is provided through `h3jsr`.
#'
#' @export
h3_polygon_membership <- function(x, polygons, id) {

  # ---------------------------------------------------------------------------
  # 1. Validate H3 input
  # ---------------------------------------------------------------------------

  if (!is.character(x)) {
    cli::cli_abort(
      "{.arg x} must be a character vector of H3 cell indexes."
    )
  }

  if (length(x) == 0L) {
    cli::cli_abort(
      "{.arg x} must contain at least one H3 cell index."
    )
  }

  if (anyNA(x) || any(x == "")) {
    cli::cli_abort(
      "{.arg x} must not contain missing or empty H3 cell indexes."
    )
  }

  if (!all(h3jsr::is_valid(x))) {
    cli::cli_abort(
      "{.arg x} contains invalid H3 cell indexes."
    )
  }

  x <- sort(unique(x))


  # ---------------------------------------------------------------------------
  # 2. Validate polygon object
  # ---------------------------------------------------------------------------

  if (!inherits(polygons, "sf")) {
    cli::cli_abort(
      "{.arg polygons} must be an {.cls sf} object."
    )
  }

  if (nrow(polygons) == 0L) {
    cli::cli_abort(
      "{.arg polygons} must contain at least one polygon feature."
    )
  }

  if (is.na(sf::st_crs(polygons))) {
    cli::cli_abort(
      "{.arg polygons} must have a defined coordinate reference system."
    )
  }

  if (!all(sf::st_is_valid(polygons))) {
    cli::cli_abort(
      paste0(
        "{.arg polygons} contains invalid geometry. ",
        "Repair the polygon geometry before assigning H3 membership."
      )
    )
  }

  geometry_type <- as.character(
    sf::st_geometry_type(polygons)
  )

  if (!all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    cli::cli_abort(
      "{.arg polygons} must contain only polygon or multipolygon geometry."
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Validate polygon identifier
  # ---------------------------------------------------------------------------

  if (!is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      id == "") {

    cli::cli_abort(
      "{.arg id} must be the name of one polygon identifier column."
    )
  }

  if (!id %in% names(polygons)) {
    cli::cli_abort(
      "Column {.field {id}} was not found in {.arg polygons}."
    )
  }

  polygon_id <- polygons[[id]]

  if (anyNA(polygon_id)) {
    cli::cli_abort(
      "Polygon identifier column {.field {id}} must not contain missing values."
    )
  }

  if (anyDuplicated(polygon_id)) {
    cli::cli_abort(
      paste0(
        "Polygon identifier column {.field {id}} must uniquely identify ",
        "polygon features."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Validate polygon topology
  # ---------------------------------------------------------------------------
  #
  # Adjacent polygons may share boundaries. What is prohibited here is
  # positive-area overlap between different polygon features.
  #
  # Perform the area test in a local projected CRS rather than longitude /
  # latitude coordinates.
  # ---------------------------------------------------------------------------

  polygons_4326 <- sf::st_transform(
    polygons,
    4326
  )

  bbox <- sf::st_bbox(polygons_4326)

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

  local_crs <- paste0(
    "+proj=aeqd +lat_0=",
    lat_0,
    " +lon_0=",
    lon_0,
    " +datum=WGS84 +units=m +no_defs"
  )

  polygons_projected <- sf::st_transform(
    polygons,
    local_crs
  )

  candidate_pairs <- sf::st_intersects(
    sf::st_geometry(polygons_projected)
  )

  overlap_pairs <- list()

  for (i in seq_len(nrow(polygons_projected))) {

    candidates <- candidate_pairs[[i]]

    candidates <- candidates[
      candidates > i
    ]

    if (length(candidates) == 0L) {
      next
    }

    for (j in candidates) {

      overlap_geometry <- suppressWarnings(
        sf::st_intersection(
          sf::st_geometry(polygons_projected)[i],
          sf::st_geometry(polygons_projected)[j]
        )
      )

      if (length(overlap_geometry) == 0L) {
        next
      }

      overlap_area <- sum(
        as.numeric(
          sf::st_area(overlap_geometry)
        )
      )

      if (is.finite(overlap_area) && overlap_area > 0) {

        overlap_pairs[[length(overlap_pairs) + 1L]] <- c(
          as.character(polygon_id[i]),
          as.character(polygon_id[j])
        )
      }
    }
  }

  if (length(overlap_pairs) > 0L) {

    pair_text <- vapply(
      overlap_pairs,
      function(z) {
        paste0(
          "'",
          z[1],
          "' and '",
          z[2],
          "'"
        )
      },
      character(1)
    )

    cli::cli_abort(
      c(
        "Polygon topology is not suitable for H3 membership.",
        "x" = paste0(
          "Features with overlapping interiors: ",
          paste(pair_text, collapse = "; "),
          "."
        ),
        "i" = paste0(
          "Polygon features may share boundaries but must not have ",
          "positive-area overlap."
        ),
        "i" = "Resolve polygon overlaps before continuing."
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Convert H3 indexes to geometry
  # ---------------------------------------------------------------------------

  h3_geometry <- h3jsr::cell_to_polygon(
    x,
    simple = FALSE
  )

  h3_geometry <- sf::st_transform(
    h3_geometry,
    sf::st_crs(polygons)
  )


  # ---------------------------------------------------------------------------
  # 6. Calculate H3-to-polygon relationships
  # ---------------------------------------------------------------------------

  intersections <- sf::st_intersects(
    sf::st_geometry(h3_geometry),
    sf::st_geometry(polygons)
  )

  relationship_n <- lengths(intersections)


  # ---------------------------------------------------------------------------
  # 7. Return empty relationship where appropriate
  # ---------------------------------------------------------------------------

  if (!any(relationship_n > 0L)) {

    return(
      tibble::tibble(
        h3 = character(),
        polygon_id = polygon_id[integer()]
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Build explicit relationship table
  # ---------------------------------------------------------------------------

  result <- tibble::tibble(
    h3 = rep(
      x,
      relationship_n
    ),
    polygon_id = polygon_id[
      unlist(
        intersections,
        use.names = FALSE
      )
    ]
  )


  # ---------------------------------------------------------------------------
  # 9. Validate relationship result
  # ---------------------------------------------------------------------------

  if (anyDuplicated(result)) {
    cli::cli_abort(
      "Internal membership validation failed: duplicate relationships."
    )
  }

  if (!all(result$h3 %in% x)) {
    cli::cli_abort(
      "Internal membership validation failed: unexpected H3 indexes."
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return deterministic result
  # ---------------------------------------------------------------------------

  result[
    order(
      result$h3,
      match(result$polygon_id, polygon_id)
    ),
  ]
}