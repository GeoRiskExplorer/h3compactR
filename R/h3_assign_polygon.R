#' Assign H3 cells to polygon features
#'
#' Assigns H3 cells to polygon features using polygon membership and,
#' optionally, nearest-polygon assignment for cells outside the polygon
#' geography.
#'
#' @param x A character vector of valid H3 cell indexes.
#' @param polygons An `sf` object containing polygon or multipolygon features.
#' @param id Name of the polygon identifier column.
#' @param keep Optional character vector naming additional polygon attributes
#'   to retain in the returned assignment table. Polygon geometry is not
#'   retained. Values are transferred after polygon assignment is resolved.
#' @param outside How cells lying entirely outside the polygon geography should
#'   be handled. `"unassigned"` retains them without polygon ownership.
#'   `"nearest"` assigns them to the nearest polygon where ownership can be
#'   determined uniquely.
#'
#' @return A tibble containing one row per unique H3 cell with `h3`,
#'   `polygon_id`, `assignment_method`, `assignment_distance`, and
#'   `candidate_n`. Attributes requested through `keep` are appended as
#'   ordinary columns.
#'
#' @details
#' Assignment methods are:
#'
#' * `"centre"`: the H3 centre lies within one polygon.
#' * `"intersects"`: the centre lies outside, but the H3 cell intersects
#'   exactly one polygon.
#' * `"largest_overlap"`: the H3 cell intersects multiple polygons and one
#'   polygon has a unique greatest overlap.
#' * `"nearest"`: the H3 cell lies outside all polygons and is assigned to
#'   the nearest polygon.
#' * `"ambiguous"`: polygon ownership cannot be determined uniquely.
#' * `"unassigned"`: the cell lies outside the polygon geography and
#'   `outside = "unassigned"`.
#'
#' Polygon features may share boundaries but must not have positive-area
#' interior overlap.
#'
#' `outside = "nearest"` produces a complete assignment surface except where
#' nearest ownership is genuinely ambiguous. It does not impose an arbitrary
#' maximum search distance.
#'
#' @examples
#' cells <- h3_cover_polygon(toy_membership_polygons, resolution = 7)
#' assignment <- h3_assign_polygon(
#'   cells,
#'   toy_membership_polygons,
#'   id = "feature_id"
#' )
#' head(assignment)
#'
#' @references
#' Spatial vector and geometric operations use `sf`; see Pebesma E (2018),
#' \doi{10.32614/RJ-2018-009}. H3 geometry access is provided through `h3jsr`.
#'
#' @export
h3_assign_polygon <- function(
  x,
  polygons,
  id,
  keep = NULL,
  outside = c("unassigned", "nearest")
) {

  # ---------------------------------------------------------------------------
  # 1. Validate H3 input
  # ---------------------------------------------------------------------------

  if (!is.character(x)) {
    cli::cli_abort("`x` must be a character vector of H3 indexes.")
  }

  if (length(x) == 0L) {
    cli::cli_abort("`x` must contain at least one H3 index.")
  }

  if (anyNA(x)) {
    cli::cli_abort("`x` must not contain missing H3 indexes.")
  }

  if (!all(h3jsr::is_valid(x))) {
    cli::cli_abort("`x` contains invalid H3 indexes.")
  }

  x <- sort(unique(x))


  # ---------------------------------------------------------------------------
  # 2. Validate polygon input
  # ---------------------------------------------------------------------------

  if (!inherits(polygons, "sf")) {
    cli::cli_abort("`polygons` must be an `sf` object.")
  }

  if (nrow(polygons) == 0L) {
    cli::cli_abort("`polygons` must contain at least one feature.")
  }

  if (is.na(sf::st_crs(polygons))) {
    cli::cli_abort("`polygons` must have a defined CRS.")
  }

  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    cli::cli_abort("`id` must name one polygon identifier column.")
  }

  if (!id %in% names(polygons)) {
    cli::cli_abort(
      paste0(
        "`",
        id,
        "` was not found in `polygons`."
      )
    )
  }

  polygon_id <- polygons[[id]]

  if (anyNA(polygon_id)) {
    cli::cli_abort(
      "Polygon identifiers must not contain missing values."
    )
  }

  if (anyDuplicated(polygon_id)) {
    cli::cli_abort(
      "Polygon identifiers must be unique for H3 assignment."
    )
  }

  geometry_type <- unique(
    as.character(
      sf::st_geometry_type(
        polygons,
        by_geometry = TRUE
      )
    )
  )

  if (!all(geometry_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    cli::cli_abort(
      "`polygons` must contain only POLYGON or MULTIPOLYGON geometry."
    )
  }

  if (!all(sf::st_is_valid(polygons))) {
    cli::cli_abort(
      "`polygons` contains invalid geometry."
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Validate retained polygon attributes
  # ---------------------------------------------------------------------------

  if (!is.null(keep)) {

    if (!is.character(keep)) {
      cli::cli_abort(
        "`keep` must be a character vector of polygon attribute names."
      )
    }

    if (anyNA(keep)) {
      cli::cli_abort(
        "`keep` must not contain missing values."
      )
    }

    if (any(!nzchar(keep))) {
      cli::cli_abort(
        "`keep` must not contain empty attribute names."
      )
    }

    keep <- unique(keep)
    missing_keep <- setdiff(keep, names(polygons))

    if (length(missing_keep) > 0L) {
      cli::cli_abort(
        paste0(
          "Unknown polygon attributes requested in `keep`: ",
          paste(missing_keep, collapse = ", "),
          "."
        )
      )
    }

    geometry_column <- attr(polygons, "sf_column")

    if (geometry_column %in% keep) {
      cli::cli_abort(
        "`keep` must contain polygon attributes, not the geometry column."
      )
    }

    keep <- setdiff(keep, id)
  }


  # ---------------------------------------------------------------------------
  # 4. Validate assignment policy
  # ---------------------------------------------------------------------------

  outside <- match.arg(outside)


  # ---------------------------------------------------------------------------
  # 5. Validate polygon topology
  # ---------------------------------------------------------------------------
  #
  # Reuse the established polygon-topology validation for now. This will be
  # extracted to a shared internal helper once assignment behaviour is frozen.
  # ---------------------------------------------------------------------------

  invisible(
    h3_polygon_membership(
      x = x[1],
      polygons = polygons,
      id = id
    )
  )


  # ---------------------------------------------------------------------------
  # 6. Initialise result
  # ---------------------------------------------------------------------------

  result_polygon_id <- polygon_id[
    rep(
      NA_integer_,
      length(x)
    )
  ]

  result_method <- rep(
    "unassigned",
    length(x)
  )

  result_distance <- rep(
    NA_real_,
    length(x)
  )

  result_candidate_n <- integer(
    length(x)
  )


  # ---------------------------------------------------------------------------
  # 7. Fast centre assignment
  # ---------------------------------------------------------------------------

  centres <- h3jsr::cell_to_point(
    x,
    simple = FALSE
  )

  centres <- sf::st_transform(
    centres,
    sf::st_crs(polygons)
  )

  centre_relation <- sf::st_within(
    centres,
    polygons,
    sparse = TRUE
  )

  centre_n <- lengths(
    centre_relation
  )

  if (any(centre_n > 1L)) {
    cli::cli_abort(
      "Polygon topology produced multiple assignments for an H3 centre."
    )
  }

  centre_assigned <- which(
    centre_n == 1L
  )

  if (length(centre_assigned) > 0L) {

    centre_polygon_row <- vapply(
      centre_relation[centre_assigned],
      `[`,
      integer(1),
      1L
    )

    result_polygon_id[centre_assigned] <- polygon_id[
      centre_polygon_row
    ]

    result_method[centre_assigned] <- "centre"
    result_distance[centre_assigned] <- 0
    result_candidate_n[centre_assigned] <- 1L
  }


  # ---------------------------------------------------------------------------
  # 8. Polygonise only cells unresolved by centre
  # ---------------------------------------------------------------------------

  unresolved_index <- which(
    result_method == "unassigned"
  )

  outside_index <- integer()

  if (length(unresolved_index) > 0L) {

    unresolved_h3 <- x[
      unresolved_index
    ]

    unresolved_geometry <- h3jsr::cell_to_polygon(
      unresolved_h3,
      simple = FALSE
    )

    unresolved_geometry <- sf::st_transform(
      unresolved_geometry,
      sf::st_crs(polygons)
    )


    # -------------------------------------------------------------------------
    # 8.1 Determine geometric polygon membership
    # -------------------------------------------------------------------------

    intersection_relation <- sf::st_intersects(
      unresolved_geometry,
      polygons,
      sparse = TRUE
    )

    intersection_n <- lengths(
      intersection_relation
    )


    # -------------------------------------------------------------------------
    # 8.2 Single polygon intersection
    # -------------------------------------------------------------------------

    single_local <- which(
      intersection_n == 1L
    )

    if (length(single_local) > 0L) {

      single_result_index <- unresolved_index[
        single_local
      ]

      single_polygon_row <- vapply(
        intersection_relation[single_local],
        `[`,
        integer(1),
        1L
      )

      result_polygon_id[single_result_index] <- polygon_id[
        single_polygon_row
      ]

      result_method[single_result_index] <- "intersects"
      result_distance[single_result_index] <- 0
      result_candidate_n[single_result_index] <- 1L
    }


    # -------------------------------------------------------------------------
    # 8.3 Multiple polygon intersections
    # -------------------------------------------------------------------------

    multiple_local <- which(
      intersection_n > 1L
    )

    if (length(multiple_local) > 0L) {

      multiple_result_index <- unresolved_index[
        multiple_local
      ]

      multiple_geometry <- unresolved_geometry[
        multiple_local,
      ]


      # -----------------------------------------------------------------------
      # 9. Construct local projected CRS for overlap measurement
      # -----------------------------------------------------------------------

      polygons_4326 <- sf::st_transform(
        polygons,
        4326
      )

      bbox <- sf::st_bbox(
        polygons_4326
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

      local_crs <- paste0(
        "+proj=aeqd +lat_0=",
        lat_0,
        " +lon_0=",
        lon_0,
        " +datum=WGS84 +units=m +no_defs"
      )

      multiple_projected <- sf::st_transform(
        multiple_geometry,
        local_crs
      )

      polygons_projected <- sf::st_transform(
        polygons,
        local_crs
      )


      # -----------------------------------------------------------------------
      # 10. Resolve multiple intersections
      # -----------------------------------------------------------------------

      for (j in seq_along(multiple_local)) {

        result_i <- multiple_result_index[j]

        candidate_rows <- intersection_relation[[multiple_local[j]]]

        candidate_ids <- polygon_id[
          candidate_rows
        ]

        cell_geometry <- sf::st_geometry(
          multiple_projected
        )[j]

        intersection_area <- numeric(
          length(candidate_rows)
        )

        for (k in seq_along(candidate_rows)) {

          candidate_geometry <- sf::st_geometry(
            polygons_projected
          )[candidate_rows[k]]

          intersection_geometry <- suppressWarnings(
            sf::st_intersection(
              cell_geometry,
              candidate_geometry
            )
          )

          if (length(intersection_geometry) > 0L) {

            intersection_area[k] <- sum(
              as.numeric(
                sf::st_area(
                  intersection_geometry
                )
              )
            )
          }
        }

        maximum_area <- max(
          intersection_area
        )

        tolerance <- max(
          sqrt(.Machine$double.eps) * maximum_area,
          sqrt(.Machine$double.eps)
        )

        winner <- which(
          abs(
            intersection_area -
              maximum_area
          ) <= tolerance
        )

        result_candidate_n[result_i] <- length(
          candidate_ids
        )

        if (length(winner) != 1L) {

          result_polygon_id[result_i] <- NA
          result_method[result_i] <- "ambiguous"
          result_distance[result_i] <- 0

          next
        }

        result_polygon_id[result_i] <- candidate_ids[
          winner
        ]

        result_method[result_i] <- "largest_overlap"
        result_distance[result_i] <- 0
      }
    }


    # -------------------------------------------------------------------------
    # 11. Identify cells completely outside polygon geography
    # -------------------------------------------------------------------------

    outside_local <- which(
      intersection_n == 0L
    )

    if (length(outside_local) > 0L) {

      outside_index <- unresolved_index[
        outside_local
      ]
    }
  }


  # ---------------------------------------------------------------------------
  # 12. Assign genuinely outside cells to nearest polygon
  # ---------------------------------------------------------------------------
  #
  # Nearest assignment is geometric rather than an arbitrary H3-ring search.
  #
  # The query is run against both polygon orders. If polygon ownership changes
  # solely because feature order changes, the result is treated as ambiguous
  # rather than accepting record order as a spatial decision rule.
  # ---------------------------------------------------------------------------

  if (
    outside == "nearest" &&
      length(outside_index) > 0L
  ) {

    outside_centres <- centres[
      outside_index,
    ]

    nearest_forward <- sf::st_nearest_feature(
      outside_centres,
      polygons
    )

    reverse_order <- rev(
      seq_len(
        nrow(polygons)
      )
    )

    polygons_reverse <- polygons[
      reverse_order,
    ]

    nearest_reverse_local <- sf::st_nearest_feature(
      outside_centres,
      polygons_reverse
    )

    nearest_reverse <- reverse_order[
      nearest_reverse_local
    ]

    same_nearest <- nearest_forward == nearest_reverse

    unique_nearest_index <- outside_index[
      same_nearest
    ]

    if (length(unique_nearest_index) > 0L) {

      unique_polygon_row <- nearest_forward[
        same_nearest
      ]

      result_polygon_id[
        unique_nearest_index
      ] <- polygon_id[
        unique_polygon_row
      ]

      result_method[
        unique_nearest_index
      ] <- "nearest"

      result_candidate_n[
        unique_nearest_index
      ] <- 1L
    }

    ambiguous_nearest_index <- outside_index[
      !same_nearest
    ]

    if (length(ambiguous_nearest_index) > 0L) {

      result_polygon_id[
        ambiguous_nearest_index
      ] <- NA

      result_method[
        ambiguous_nearest_index
      ] <- "ambiguous"

      result_candidate_n[
        ambiguous_nearest_index
      ] <- 2L
    }


    # -------------------------------------------------------------------------
    # 12.1 Calculate nearest distance for QA
    # -------------------------------------------------------------------------

    if (length(unique_nearest_index) > 0L) {

      nearest_points <- centres[
        unique_nearest_index,
      ]

      nearest_polygons <- polygons[
        nearest_forward[
          same_nearest
        ],
      ]

      nearest_lines <- sf::st_nearest_points(
        nearest_points,
        nearest_polygons,
        pairwise = TRUE
      )

      distance_crs <- sf::st_crs(
        polygons
      )

      if (sf::st_is_longlat(distance_crs)) {

        polygons_4326 <- sf::st_transform(
          polygons,
          4326
        )

        bbox <- sf::st_bbox(
          polygons_4326
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

        distance_crs <- paste0(
          "+proj=aeqd +lat_0=",
          lat_0,
          " +lon_0=",
          lon_0,
          " +datum=WGS84 +units=m +no_defs"
        )

        nearest_lines <- sf::st_transform(
          nearest_lines,
          distance_crs
        )
      }

      result_distance[
        unique_nearest_index
      ] <- as.numeric(
        sf::st_length(
          nearest_lines
        )
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 13. Build result
  # ---------------------------------------------------------------------------

  result <- tibble::tibble(
    h3 = x,
    polygon_id = result_polygon_id,
    assignment_method = result_method,
    assignment_distance = result_distance,
    candidate_n = result_candidate_n
  )


  # ---------------------------------------------------------------------------
  # 14. Validate result
  # ---------------------------------------------------------------------------

  if (nrow(result) != length(x)) {
    cli::cli_abort(
      "Internal assignment validation failed: H3 row count changed."
    )
  }

  if (anyDuplicated(result$h3)) {
    cli::cli_abort(
      "Internal assignment validation failed: duplicate H3 rows."
    )
  }

  if (!setequal(result$h3, x)) {
    cli::cli_abort(
      "Internal assignment validation failed: H3 support changed."
    )
  }

  unresolved <- result$assignment_method %in%
    c(
      "ambiguous",
      "unassigned"
    )

  if (
    any(
      !is.na(
        result$polygon_id[
          unresolved
        ]
      )
    )
  ) {
    cli::cli_abort(
      "Internal assignment validation failed: unresolved cells received ownership."
    )
  }

  resolved <- !unresolved

  if (
    any(
      is.na(
        result$polygon_id[
          resolved
        ]
      )
    )
  ) {
    cli::cli_abort(
      "Internal assignment validation failed: resolved cells lack ownership."
    )
  }

  if (
    outside == "nearest" &&
      any(
        result$assignment_method == "unassigned"
      )
  ) {
    cli::cli_abort(
      "Internal assignment validation failed: nearest assignment left cells unassigned."
    )
  }


  # ---------------------------------------------------------------------------
  # 15. Attach selected polygon attributes
  # ---------------------------------------------------------------------------
  #
  # Polygon ownership has already been resolved spatially. Additional
  # attributes are transferred with an ordinary table lookup. Polygon geometry
  # is deliberately not carried into the result.
  # ---------------------------------------------------------------------------

  if (!is.null(keep) && length(keep) > 0L) {

    polygon_attributes <- sf::st_drop_geometry(polygons)

    polygon_row <- match(
      result$polygon_id,
      polygon_attributes[[id]]
    )

    for (attribute in keep) {
      result[[attribute]] <- polygon_attributes[[attribute]][polygon_row]
    }
  }


  # ---------------------------------------------------------------------------
  # 16. Return deterministic result
  # ---------------------------------------------------------------------------

  result <- result[
    order(result$h3),
  ]

  rownames(result) <- NULL

  result
}