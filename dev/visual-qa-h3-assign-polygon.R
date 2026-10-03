# =============================================================================
# h3compactR
# VISUAL QA — H3 POLYGON ASSIGNMENT
# =============================================================================
#
# Development-only visual QA for h3_assign_polygon().
#
# Runs the same analytical scenarios across multiple H3 resolutions so that
# assignment behaviour can be inspected independently of any single resolution.
#
# IMPORTANT
# ---------
# Increasing H3 resolution can increase cell counts very rapidly. Higher
# resolution therefore increases memory use, geometry construction costs and
# spatial-operation runtime.
#
# This script deliberately reports cell counts and elapsed assignment time.
#
# No universal "safe" H3 resolution or cell-count threshold is assumed here.
# Practical limits depend on:
#   - polygon extent and complexity
#   - H3 resolution
#   - number of polygon features
#   - proportion of boundary / unresolved cells
#   - whether nearest outside assignment is requested
#   - available memory and computing environment
#
# Package documentation and vignettes should encourage users to inspect
# expected cell counts before unnecessarily increasing H3 resolution.
#
# Output:
#   One graphics device containing one row per H3 resolution and eight
#   analytical QA scenarios per row.
#
# No files are written.
# =============================================================================


devtools::load_all()


# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

qa_resolutions <- c(
  8L,
  9L,
  10L
)

qa_expand_rings <- 3L


# -----------------------------------------------------------------------------
# 1. Helpers
# -----------------------------------------------------------------------------

.qa_h3_sf <- function(result) {

  geometry <- h3jsr::cell_to_polygon(
    result$h3,
    simple = FALSE
  )

  geometry$polygon_id <- result$polygon_id
  geometry$assignment_method <- result$assignment_method
  geometry$assignment_distance <- result$assignment_distance
  geometry$candidate_n <- result$candidate_n

  geometry
}


.qa_local_crs <- function(x) {

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


.qa_build_overlap_fixture <- function(h3) {

  cell <- h3jsr::cell_to_polygon(
    h3,
    simple = FALSE
  )

  local_crs <- .qa_local_crs(
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

  centre <- sf::st_transform(
    centre,
    local_crs
  )

  centre_xy <- sf::st_coordinates(
    centre
  )[1, ]

  bbox <- sf::st_bbox(
    cell_projected
  )

  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]

  gap_half <- width * 0.04
  left_edge <- centre_xy[["X"]] - gap_half
  right_edge <- centre_xy[["X"]] + gap_half

  pad <- max(
    width,
    height
  )

  left_box <- sf::st_polygon(
    list(
      matrix(
        c(
          bbox[["xmin"]] - pad,
          bbox[["ymin"]] - pad,

          left_edge,
          bbox[["ymin"]] - pad,

          left_edge,
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

  right_max <- right_edge +
    0.55 *
    (
      bbox[["xmax"]] -
        right_edge
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

  candidate_boxes <- sf::st_sfc(
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
      candidate_boxes[1]
    )
  )

  right_geometry <- suppressWarnings(
    sf::st_intersection(
      cell_geometry,
      candidate_boxes[2]
    )
  )

  polygons <- sf::st_sf(
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

  list(
    h3 = h3,
    cell = cell_projected,
    centre = centre,
    polygons = polygons
  )
}


.qa_timed_assignment <- function(
  x,
  polygons,
  id,
  outside
) {

  timing <- system.time(
    result <- h3_assign_polygon(
      x,
      polygons,
      id = id,
      outside = outside
    )
  )

  list(
    result = result,
    elapsed = unname(timing[["elapsed"]])
  )
}


.qa_method_counts <- function(result) {

  methods <- c(
    "centre",
    "intersects",
    "largest_overlap",
    "nearest",
    "ambiguous",
    "unassigned"
  )

  counts <- table(
    factor(
      result$assignment_method,
      levels = methods
    )
  )

  as.integer(
    counts
  )
}


# -----------------------------------------------------------------------------
# 2. Run one resolution
# -----------------------------------------------------------------------------

.qa_run_resolution <- function(resolution) {

  cat(
    "\n",
    "======================================================================\n",
    "H3 ASSIGNMENT VISUAL QA — RESOLUTION ",
    resolution,
    "\n",
    "======================================================================\n",
    sep = ""
  )


  # ---------------------------------------------------------------------------
  # 2.1 Authoritative polygon footprint
  # ---------------------------------------------------------------------------

  assignment_extent <- sf::st_union(
    toy_membership_polygons
  )

  coverage_time <- system.time(
    assignment_h3 <- h3_cover_polygon(
      assignment_extent,
      resolution = resolution,
      boundary = "intersects"
    )
  )

  footprint_run <- .qa_timed_assignment(
    assignment_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  footprint_result <- footprint_run$result

  footprint_sf <- .qa_h3_sf(
    footprint_result
  )


  # ---------------------------------------------------------------------------
  # 2.2 Expanded support
  # ---------------------------------------------------------------------------

  expanded_h3 <- expand_h3(
    assignment_h3,
    rings = qa_expand_rings
  )

  expanded_unassigned_run <- .qa_timed_assignment(
    expanded_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "unassigned"
  )

  expanded_nearest_run <- .qa_timed_assignment(
    expanded_h3,
    toy_membership_polygons,
    id = "feature_id",
    outside = "nearest"
  )

  expanded_unassigned <- expanded_unassigned_run$result
  expanded_nearest <- expanded_nearest_run$result

  expanded_unassigned_sf <- .qa_h3_sf(
    expanded_unassigned
  )

  expanded_nearest_sf <- .qa_h3_sf(
    expanded_nearest
  )


  # ---------------------------------------------------------------------------
  # 2.3 Donut + enclave
  # ---------------------------------------------------------------------------

  donut_enclave <- toy_membership_polygons[
    toy_membership_polygons$feature_id %in%
      c(
        "500",
        "600"
      ),
  ]

  donut_extent <- sf::st_union(
    donut_enclave
  )

  donut_h3 <- h3_cover_polygon(
    donut_extent,
    resolution = resolution,
    boundary = "intersects"
  )

  donut_result <- h3_assign_polygon(
    donut_h3,
    donut_enclave,
    id = "feature_id",
    outside = "unassigned"
  )

  donut_sf <- .qa_h3_sf(
    donut_result
  )


  # ---------------------------------------------------------------------------
  # 2.4 Multipart
  # ---------------------------------------------------------------------------

  multipart <- toy_membership_polygons[
    toy_membership_polygons$feature_id == "400",
  ]

  multipart_h3 <- h3_cover_polygon(
    multipart,
    resolution = resolution,
    boundary = "intersects"
  )

  multipart_result <- h3_assign_polygon(
    multipart_h3,
    multipart,
    id = "feature_id",
    outside = "unassigned"
  )

  multipart_sf <- .qa_h3_sf(
    multipart_result
  )


  # ---------------------------------------------------------------------------
  # 2.5 Largest-overlap scenario
  # ---------------------------------------------------------------------------

  centre_cells <- footprint_result$h3[
    footprint_result$assignment_method == "centre"
  ]

  if (length(centre_cells) == 0L) {
    stop(
      "No centre-assigned H3 cells available for overlap QA."
    )
  }

  overlap_h3 <- centre_cells[
    floor(length(centre_cells) / 2)
  ]

  overlap_fixture <- .qa_build_overlap_fixture(
    overlap_h3
  )

  overlap_result <- h3_assign_polygon(
    overlap_h3,
    overlap_fixture$polygons,
    id = "polygon_id",
    outside = "unassigned"
  )


  # ---------------------------------------------------------------------------
  # 2.6 Summary
  # ---------------------------------------------------------------------------

  footprint_counts <- .qa_method_counts(
    footprint_result
  )

  expanded_unassigned_counts <- .qa_method_counts(
    expanded_unassigned
  )

  expanded_nearest_counts <- .qa_method_counts(
    expanded_nearest
  )

  summary <- data.frame(
    resolution = resolution,
    footprint_cells = length(assignment_h3),
    expanded_cells = length(expanded_h3),
    coverage_elapsed = unname(coverage_time[["elapsed"]]),
    footprint_assignment_elapsed = footprint_run$elapsed,
    expanded_unassigned_elapsed = expanded_unassigned_run$elapsed,
    expanded_nearest_elapsed = expanded_nearest_run$elapsed,
    centre_n = footprint_counts[1],
    intersects_n = footprint_counts[2],
    largest_overlap_n = footprint_counts[3],
    footprint_unassigned_n = footprint_counts[6],
    expanded_nearest_n = expanded_nearest_counts[4],
    expanded_nearest_unassigned_n = expanded_nearest_counts[6],
    expanded_nearest_missing_id_n = sum(
      is.na(
        expanded_nearest$polygon_id
      )
    ),
    stringsAsFactors = FALSE
  )

  print(
    summary,
    row.names = FALSE
  )

  cat(
    "\nFootprint methods:\n"
  )

  print(
    table(
      footprint_result$assignment_method,
      useNA = "ifany"
    )
  )

  cat(
    "\nExpanded / unassigned methods:\n"
  )

  print(
    table(
      expanded_unassigned$assignment_method,
      useNA = "ifany"
    )
  )

  cat(
    "\nExpanded / nearest methods:\n"
  )

  print(
    table(
      expanded_nearest$assignment_method,
      useNA = "ifany"
    )
  )

  cat(
    "\nLargest-overlap result:\n"
  )

  print(
    overlap_result
  )


  # ---------------------------------------------------------------------------
  # 2.7 Return all QA objects
  # ---------------------------------------------------------------------------

  list(
    resolution = resolution,
    summary = summary,
    assignment_h3 = assignment_h3,
    footprint_result = footprint_result,
    footprint_sf = footprint_sf,
    expanded_h3 = expanded_h3,
    expanded_unassigned = expanded_unassigned,
    expanded_unassigned_sf = expanded_unassigned_sf,
    expanded_nearest = expanded_nearest,
    expanded_nearest_sf = expanded_nearest_sf,
    donut_enclave = donut_enclave,
    donut_result = donut_result,
    donut_sf = donut_sf,
    multipart = multipart,
    multipart_result = multipart_result,
    multipart_sf = multipart_sf,
    overlap_fixture = overlap_fixture,
    overlap_result = overlap_result
  )
}


# -----------------------------------------------------------------------------
# 3. Run all resolutions
# -----------------------------------------------------------------------------

qa_results <- lapply(
  qa_resolutions,
  .qa_run_resolution
)

names(qa_results) <- paste0(
  "r",
  qa_resolutions
)


# -----------------------------------------------------------------------------
# 4. Combined numerical summary
# -----------------------------------------------------------------------------

qa_summary <- do.call(
  rbind,
  lapply(
    qa_results,
    function(x) {
      x$summary
    }
  )
)

rownames(qa_summary) <- NULL

cat(
  "\n",
  "======================================================================\n",
  "MULTI-RESOLUTION QA SUMMARY\n",
  "======================================================================\n",
  sep = ""
)

print(
  qa_summary,
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 5. Plot configuration
# -----------------------------------------------------------------------------

method_levels <- c(
  "centre",
  "intersects",
  "largest_overlap",
  "nearest",
  "ambiguous",
  "unassigned"
)

method_fill <- seq_along(
  method_levels
)

polygon_ids <- sort(
  unique(
    toy_membership_polygons$feature_id
  )
)

polygon_fill <- seq_along(
  polygon_ids
)


# -----------------------------------------------------------------------------
# 6. Combined visual QA
# -----------------------------------------------------------------------------
#
# Rows:
#   H3 resolution 8
#   H3 resolution 9
#   H3 resolution 10
#
# Columns:
#   1. Polygon footprint
#   2. Assignment method
#   3. Outside unassigned
#   4. Outside nearest
#   5. Complete ownership
#   6. Donut + enclave
#   7. Multipart
#   8. Largest overlap
# -----------------------------------------------------------------------------

old_par <- par(
  mfrow = c(
    length(qa_resolutions),
    8
  ),
  mar = c(
    0.5,
    0.5,
    2.2,
    0.25
  ),
  oma = c(
    0,
    0,
    3,
    0
  )
)


for (i in seq_along(qa_results)) {

  qa <- qa_results[[i]]

  resolution <- qa$resolution


  # ---------------------------------------------------------------------------
  # 6.1 Polygon footprint
  # ---------------------------------------------------------------------------

  cell_fill <- polygon_fill[
    match(
      qa$footprint_result$polygon_id,
      polygon_ids
    )
  ]

  plot(
    sf::st_geometry(qa$footprint_sf),
    col = cell_fill,
    border = "white",
    lwd = 0.1,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 1. Footprint"
    )
  )

  plot(
    sf::st_geometry(toy_membership_polygons),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.2 Assignment method
  # ---------------------------------------------------------------------------

  cell_method_fill <- method_fill[
    match(
      qa$footprint_result$assignment_method,
      method_levels
    )
  ]

  plot(
    sf::st_geometry(qa$footprint_sf),
    col = cell_method_fill,
    border = "grey60",
    lwd = 0.1,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 2. Method"
    )
  )

  plot(
    sf::st_geometry(toy_membership_polygons),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.3 Outside unassigned
  # ---------------------------------------------------------------------------

  expanded_unassigned_fill <- method_fill[
    match(
      qa$expanded_unassigned$assignment_method,
      method_levels
    )
  ]

  plot(
    sf::st_geometry(qa$expanded_unassigned_sf),
    col = expanded_unassigned_fill,
    border = "grey70",
    lwd = 0.08,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 3. Unassigned"
    )
  )

  plot(
    sf::st_geometry(toy_membership_polygons),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.4 Outside nearest
  # ---------------------------------------------------------------------------

  expanded_nearest_method_fill <- method_fill[
    match(
      qa$expanded_nearest$assignment_method,
      method_levels
    )
  ]

  plot(
    sf::st_geometry(qa$expanded_nearest_sf),
    col = expanded_nearest_method_fill,
    border = "grey70",
    lwd = 0.08,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 4. Nearest"
    )
  )

  plot(
    sf::st_geometry(toy_membership_polygons),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.5 Complete ownership
  # ---------------------------------------------------------------------------

  nearest_polygon_fill <- polygon_fill[
    match(
      qa$expanded_nearest$polygon_id,
      polygon_ids
    )
  ]

  plot(
    sf::st_geometry(qa$expanded_nearest_sf),
    col = nearest_polygon_fill,
    border = "white",
    lwd = 0.08,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 5. Ownership"
    )
  )

  plot(
    sf::st_geometry(toy_membership_polygons),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.6 Donut + enclave
  # ---------------------------------------------------------------------------

  donut_ids <- sort(
    unique(
      qa$donut_enclave$feature_id
    )
  )

  donut_fill <- seq_along(
    donut_ids
  )

  donut_cell_fill <- donut_fill[
    match(
      qa$donut_result$polygon_id,
      donut_ids
    )
  ]

  plot(
    sf::st_geometry(qa$donut_sf),
    col = donut_cell_fill,
    border = "white",
    lwd = 0.1,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 6. Donut"
    )
  )

  plot(
    sf::st_geometry(qa$donut_enclave),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.7 Multipart
  # ---------------------------------------------------------------------------

  plot(
    sf::st_geometry(qa$multipart_sf),
    col = 1,
    border = "white",
    lwd = 0.1,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 7. Multipart"
    )
  )

  plot(
    sf::st_geometry(qa$multipart),
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 1
  )


  # ---------------------------------------------------------------------------
  # 6.8 Largest overlap
  # ---------------------------------------------------------------------------

  plot(
    sf::st_geometry(qa$overlap_fixture$cell),
    col = "grey90",
    border = "black",
    lwd = 1.5,
    axes = FALSE,
    main = paste0(
      "R",
      resolution,
      " | 8. Overlap"
    )
  )

  plot(
    sf::st_geometry(qa$overlap_fixture$polygons),
    add = TRUE,
    col = c(
      2,
      3
    ),
    border = "black",
    lwd = 1
  )

  plot(
    sf::st_geometry(qa$overlap_fixture$centre),
    add = TRUE,
    pch = 16,
    cex = 0.7
  )
}


mtext(
  "h3compactR — h3_assign_polygon() multi-resolution visual QA",
  outer = TRUE,
  side = 3,
  line = 1,
  font = 2,
  cex = 1.2
)

par(
  old_par
)


# -----------------------------------------------------------------------------
# 7. Acceptance checks
# -----------------------------------------------------------------------------

qa_acceptance <- data.frame(
  resolution = qa_resolutions,
  footprint_complete = vapply(
    qa_results,
    function(x) {
      !anyNA(x$footprint_result$polygon_id)
    },
    logical(1)
  ),
  nearest_complete = vapply(
    qa_results,
    function(x) {
      !anyNA(x$expanded_nearest$polygon_id) &&
        !any(
          x$expanded_nearest$assignment_method == "unassigned"
        )
    },
    logical(1)
  ),
  overlap_resolved = vapply(
    qa_results,
    function(x) {
      identical(
        x$overlap_result$assignment_method,
        "largest_overlap"
      ) &&
        identical(
          x$overlap_result$candidate_n,
          2L
        )
    },
    logical(1)
  ),
  stringsAsFactors = FALSE
)

cat(
  "\n",
  "======================================================================\n",
  "MULTI-RESOLUTION ACCEPTANCE\n",
  "======================================================================\n",
  sep = ""
)

print(
  qa_acceptance,
  row.names = FALSE
)