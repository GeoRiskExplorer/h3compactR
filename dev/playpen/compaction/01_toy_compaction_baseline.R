# =============================================================================
# h3compactR
# Toy H3 compaction baseline
#
# File:
#   dev/playpen/compaction/01_toy_compaction_baseline.R
#
# Purpose:
#   Test structural H3 compaction, container handling, hierarchy integrity,
#   round-trip coverage and point assignment to a mixed-resolution grid.
#
# Important:
#   This is experimental development code, not package API code.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(h3jsr)
})


# -----------------------------------------------------------------------------
# 2. Parameters
# -----------------------------------------------------------------------------

base_res <- 7L
point_n <- 500L
random_seed <- 20260711L

# Container polygon.
container_sfc <- st_as_sfc(
  st_bbox(
    c(
      xmin = 144.80,
      ymin = -37.95,
      xmax = 145.15,
      ymax = -37.70
    ),
    crs = st_crs(4326)
  )
)

container_sf <- st_sf(
  container_id = "toy_container",
  geometry = container_sfc
)

stopifnot(
  inherits(container_sf, "sf"),
  st_crs(container_sf)$epsg == 4326,
  all(st_is_valid(container_sf))
)


# -----------------------------------------------------------------------------
# 3. Convert the container to source H3 cells
# -----------------------------------------------------------------------------

source_h3 <- polygon_to_cells(
  container_sf,
  res = base_res,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

source_res <- get_res(source_h3)

source_sf_full <- cell_to_polygon(
  source_h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  mutate(
    resolution = get_res(h3),
    geometry_type = "canonical_h3"
  )

cat("\n============================================================\n")
cat("SOURCE GRID\n")
cat("============================================================\n\n")

cat("Source H3 cells:", length(source_h3), "\n")
cat("Source resolution:", paste(unique(source_res), collapse = ", "), "\n")
cat("Duplicate indexes:", anyDuplicated(source_h3), "\n")
cat("Invalid indexes:", sum(!is_valid(source_h3)), "\n")

stopifnot(
  length(source_h3) > 0L,
  !anyNA(source_h3),
  !anyDuplicated(source_h3),
  all(is_valid(source_h3)),
  all(source_res == base_res)
)


# -----------------------------------------------------------------------------
# 4. Compact source cells
# -----------------------------------------------------------------------------

compact_h3 <- compact(
  source_h3,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

compact_res <- get_res(compact_h3)

compact_sf_full <- cell_to_polygon(
  compact_h3,
  simple = FALSE
) |>
  rename(h3 = h3_address) |>
  mutate(
    resolution = get_res(h3),
    geometry_type = "canonical_h3"
  )

cat("\n============================================================\n")
cat("COMPACTED GRID\n")
cat("============================================================\n\n")

cat("Compacted H3 cells:", length(compact_h3), "\n")
cat(
  "Compacted resolutions:",
  paste(sort(unique(compact_res)), collapse = ", "),
  "\n"
)

resolution_summary <- compact_sf_full |>
  st_drop_geometry() |>
  count(resolution, name = "cell_count") |>
  arrange(resolution) |>
  mutate(
    cell_pct = round(100 * cell_count / sum(cell_count), 2)
  )

print(resolution_summary)

stopifnot(
  length(compact_h3) < length(source_h3),
  !anyNA(compact_h3),
  !anyDuplicated(compact_h3),
  all(is_valid(compact_h3)),
  min(compact_res) <= base_res,
  max(compact_res) <= base_res
)


# -----------------------------------------------------------------------------
# 5. Container geometry handling
# -----------------------------------------------------------------------------

# Canonical H3 polygons can extend beyond the polygon used to select them.
# Retain canonical geometry for H3 analysis, but create a clipped geometry
# specifically for display.

compact_sf_display <- suppressWarnings(
  st_intersection(
    compact_sf_full,
    select(container_sf, container_id)
  )
) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    geometry_type = "container_clipped"
  )

source_sf_display <- suppressWarnings(
  st_intersection(
    source_sf_full,
    select(container_sf, container_id)
  )
) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    geometry_type = "container_clipped"
  )

outside_container <- suppressWarnings(
  st_difference(
    st_union(compact_sf_full),
    st_union(container_sf)
  )
)

outside_area_m2 <- outside_container |>
  st_transform(7899) |>
  st_area() |>
  sum() |>
  as.numeric()

clipped_outside_check <- suppressWarnings(
  st_difference(
    st_union(compact_sf_display),
    st_union(container_sf)
  )
)

clipped_outside_area_m2 <- clipped_outside_check |>
  st_transform(7899) |>
  st_area() |>
  sum() |>
  as.numeric()

cat("\n============================================================\n")
cat("CONTAINER QA\n")
cat("============================================================\n\n")

cat(
  "Canonical H3 area outside container m2:",
  round(outside_area_m2, 2),
  "\n"
)

cat(
  "Clipped display area outside container m2:",
  round(clipped_outside_area_m2, 8),
  "\n"
)

stopifnot(
  clipped_outside_area_m2 < 0.01
)


# -----------------------------------------------------------------------------
# 6. Parent-descendant overlap QA
# -----------------------------------------------------------------------------

find_hierarchy_overlaps <- function(h3) {

  h3 <- unique(as.character(h3))
  resolutions <- get_res(h3)

  output <- vector("list", length(h3))
  output_i <- 0L

  for (i in seq_along(h3)) {

    child <- h3[[i]]
    child_res <- resolutions[[i]]

    possible_parent_res <- resolutions[
      resolutions < child_res
    ] |>
      unique() |>
      sort()

    if (length(possible_parent_res) == 0L) {
      next
    }

    for (parent_res in possible_parent_res) {

      parent <- get_parent(
        h3_address = child,
        res = parent_res,
        simple = TRUE
      ) |>
        unlist(use.names = FALSE)

      if (parent %in% h3) {

        output_i <- output_i + 1L

        output[[output_i]] <- tibble(
          parent_h3 = parent,
          parent_resolution = parent_res,
          child_h3 = child,
          child_resolution = child_res
        )
      }
    }
  }

  if (output_i == 0L) {
    return(
      tibble(
        parent_h3 = character(),
        parent_resolution = integer(),
        child_h3 = character(),
        child_resolution = integer()
      )
    )
  }

  bind_rows(output[seq_len(output_i)])
}

hierarchy_overlaps <- find_hierarchy_overlaps(compact_h3)

cat("\n============================================================\n")
cat("HIERARCHY OVERLAP QA\n")
cat("============================================================\n\n")

cat(
  "Parent-descendant overlaps:",
  nrow(hierarchy_overlaps),
  "\n"
)

if (nrow(hierarchy_overlaps) > 0L) {
  print(hierarchy_overlaps)
}

stopifnot(
  nrow(hierarchy_overlaps) == 0L
)


# -----------------------------------------------------------------------------
# 7. Compaction round-trip QA
# -----------------------------------------------------------------------------

roundtrip_h3 <- uncompact(
  compact_h3,
  res = base_res,
  simple = TRUE
) |>
  unlist(use.names = FALSE) |>
  unique()

missing_after_roundtrip <- setdiff(
  source_h3,
  roundtrip_h3
)

additional_after_roundtrip <- setdiff(
  roundtrip_h3,
  source_h3
)

cat("\n============================================================\n")
cat("ROUND-TRIP QA\n")
cat("============================================================\n\n")

cat("Original source cells:", length(source_h3), "\n")
cat("Round-trip cells:", length(roundtrip_h3), "\n")
cat("Missing after round trip:", length(missing_after_roundtrip), "\n")
cat("Additional after round trip:", length(additional_after_roundtrip), "\n")
cat("Exact set equality:", setequal(source_h3, roundtrip_h3), "\n")

stopifnot(
  length(missing_after_roundtrip) == 0L,
  length(additional_after_roundtrip) == 0L,
  setequal(source_h3, roundtrip_h3)
)


# -----------------------------------------------------------------------------
# 8. Create toy points inside the actual source H3 coverage
# -----------------------------------------------------------------------------

source_coverage_sf <- source_sf_full |>
  summarise()

set.seed(random_seed)

toy_points <- st_sample(
  source_coverage_sf,
  size = point_n,
  type = "random",
  exact = TRUE
) |>
  st_sf() |>
  mutate(
    point_id = row_number(),
    event_count = 1L
  )

toy_points$source_h3 <- point_to_cell(
  toy_points,
  res = base_res,
  simple = TRUE
) |>
  unlist(use.names = FALSE)

stopifnot(
  nrow(toy_points) == point_n,
  all(toy_points$source_h3 %in% source_h3)
)

# -----------------------------------------------------------------------------
# 8A. Confirm every sampled point belongs to the source H3 coverage
# -----------------------------------------------------------------------------

cat("\n--- Point-to-source coverage QA ---\n")

cat(
  "Unique point cells absent from source grid:",
  length(setdiff(unique(toy_points$source_h3), source_h3)),
  "\n"
)

cat(
  "Points whose H3 cell is absent from source grid:",
  sum(!toy_points$source_h3 %in% source_h3),
  "\n"
)

# -----------------------------------------------------------------------------
# 9. Prototype: assign fine H3 cells to finest retained compacted cell
# -----------------------------------------------------------------------------

match_to_compacted_h3 <- function(
    source_h3,
    compacted_h3
) {

  source_h3 <- as.character(source_h3)
  compacted_h3 <- unique(as.character(compacted_h3))

  if (anyNA(source_h3)) {
    stop("Missing source H3 indexes detected.", call. = FALSE)
  }

  if (!all(is_valid(source_h3))) {
    stop("Invalid source H3 indexes detected.", call. = FALSE)
  }

  if (!all(is_valid(compacted_h3))) {
    stop("Invalid compacted H3 indexes detected.", call. = FALSE)
  }

  compact_resolutions <- get_res(compacted_h3)
  minimum_compact_resolution <- min(compact_resolutions)

  matched_h3 <- rep(NA_character_, length(source_h3))
  matched_resolution <- rep(NA_integer_, length(source_h3))
  match_type <- rep(NA_character_, length(source_h3))

  for (i in seq_along(source_h3)) {

    source_cell <- source_h3[[i]]
    source_resolution <- get_res(source_cell)

    candidate_resolutions <- seq(
      from = source_resolution,
      to = minimum_compact_resolution,
      by = -1L
    )

    for (candidate_resolution in candidate_resolutions) {

      candidate_h3 <- if (candidate_resolution == source_resolution) {

        source_cell

      } else {

        get_parent(
          h3_address = source_cell,
          res = candidate_resolution,
          simple = TRUE
        ) |>
          unlist(use.names = FALSE)
      }

      if (candidate_h3 %in% compacted_h3) {

        matched_h3[[i]] <- candidate_h3
        matched_resolution[[i]] <- candidate_resolution

        match_type[[i]] <- if (
          candidate_resolution == source_resolution
        ) {
          "exact"
        } else {
          "ancestor"
        }

        break
      }
    }
  }

  tibble(
    source_h3 = source_h3,
    matched_h3 = matched_h3,
    source_resolution = get_res(source_h3),
    matched_resolution = matched_resolution,
    match_type = match_type
  )
}

# Remove assignment fields when rerunning this section
toy_points <- toy_points |>
  select(
    -any_of(c(
      "matched_h3",
      "matched_resolution",
      "match_type"
    ))
  )

point_lookup <- match_to_compacted_h3(
  source_h3 = toy_points$source_h3,
  compacted_h3 = compact_h3
)

toy_points <- toy_points |>
  mutate(
    matched_h3 = point_lookup$matched_h3,
    matched_resolution = point_lookup$matched_resolution,
    match_type = point_lookup$match_type
  )

assignment_summary <- toy_points |>
  st_drop_geometry() |>
  count(
    matched_resolution,
    match_type,
    name = "point_count"
  ) |>
  arrange(matched_resolution, match_type)

cat("\n============================================================\n")
cat("POINT ASSIGNMENT QA\n")
cat("============================================================\n\n")

print(assignment_summary)

cat(
  "Unmatched points:",
  sum(is.na(toy_points$matched_h3)),
  "\n"
)

cat(
  "Input event total:",
  sum(toy_points$event_count),
  "\n"
)

point_counts_compacted <- toy_points |>
  st_drop_geometry() |>
  group_by(matched_h3, matched_resolution) |>
  summarise(
    event_count = sum(event_count),
    .groups = "drop"
  )

cat(
  "Compacted event total:",
  sum(point_counts_compacted$event_count),
  "\n"
)

stopifnot(
  !anyNA(toy_points$matched_h3),
  all(toy_points$matched_h3 %in% compact_h3),
  sum(point_counts_compacted$event_count) ==
    sum(toy_points$event_count)
)


# -----------------------------------------------------------------------------
# 10. Overall QA summary
# -----------------------------------------------------------------------------

qa <- tibble(
  source_cells = length(source_h3),
  compact_cells = length(compact_h3),
  reduction_n = source_cells - compact_cells,
  reduction_pct = round(
    100 * reduction_n / source_cells,
    2
  ),
  source_min_res = min(source_res),
  source_max_res = max(source_res),
  compact_min_res = min(compact_res),
  compact_max_res = max(compact_res),
  compact_resolution_count = n_distinct(compact_res),
  hierarchy_overlaps = nrow(hierarchy_overlaps),
  roundtrip_missing = length(missing_after_roundtrip),
  roundtrip_additional = length(additional_after_roundtrip),
  unmatched_points = sum(is.na(toy_points$matched_h3)),
  source_event_total = sum(toy_points$event_count),
  compact_event_total = sum(point_counts_compacted$event_count)
)

cat("\n============================================================\n")
cat("FINAL QA SUMMARY\n")
cat("============================================================\n\n")

print(qa)

stopifnot(
  qa$compact_cells < qa$source_cells,
  qa$compact_min_res <= qa$source_min_res,
  qa$hierarchy_overlaps == 0L,
  qa$roundtrip_missing == 0L,
  qa$roundtrip_additional == 0L,
  qa$unmatched_points == 0L,
  qa$source_event_total == qa$compact_event_total
)


# -----------------------------------------------------------------------------
# 11. Plot
# -----------------------------------------------------------------------------

compaction_plot <- ggplot() +
  geom_sf(
    data = container_sf,
    fill = "grey98",
    colour = "black",
    linewidth = 0.9
  ) +
  geom_sf(
    data = source_sf_display,
    fill = NA,
    colour = "grey75",
    linewidth = 0.15
  ) +
  geom_sf(
    data = compact_sf_display,
    aes(fill = factor(resolution)),
    colour = "grey25",
    linewidth = 0.3,
    alpha = 0.55
  ) +
  geom_sf(
    data = toy_points,
    size = 0.4,
    alpha = 0.5
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    name = "H3 resolution"
  ) +
  labs(
    title = "Mixed-resolution H3 compaction",
    subtitle = paste0(
      "Container-clipped display geometry | ",
      qa$source_cells,
      " source cells to ",
      qa$compact_cells,
      " compacted cells"
    ),
    caption = paste0(
      "Reduction: ",
      qa$reduction_pct,
      "% | Points assigned to finest retained H3 cell"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "right"
  ) +
  coord_sf(
    xlim = c(144.80, 145.15),
    ylim = c(-37.95, -37.70),
    expand = FALSE
  )

print(compaction_plot)