# =============================================================================
# h3compactR
# QA — H3 polygon assignment attributes
#
# Purpose:
#   1. Confirm multiple polygon attributes survive assignment.
#   2. Confirm the result remains a lightweight non-spatial table.
#   3. Reconstruct H3 geometry only when plotting is required.
#   4. Confirm retained attributes remain directly usable for mapping.
# =============================================================================

library(h3compactR)
library(sf)


# =============================================================================
# 1. Prepare generic polygon attributes
# =============================================================================

polygons <- toy_membership_polygons

polygons$feature_name <- paste0(
  "Feature ",
  polygons$feature_id
)

polygons$group_name <- c(
  "Group A",
  "Group A",
  "Group B",
  "Group B",
  "Group C"
)

polygons$class_name <- c(
  "Class 1",
  "Class 1",
  "Class 2",
  "Class 2",
  "Class 3"
)


# =============================================================================
# 2. Generate H3 support
# =============================================================================

polygon_union <- sf::st_union(
  sf::st_geometry(polygons)
)

polygon_union <- sf::st_sf(
  geometry = polygon_union
)

source_h3 <- h3_cover_polygon(
  polygon_union,
  resolution = 9,
  boundary = "intersects"
)


# =============================================================================
# 3. Assign polygons and retain attributes
# =============================================================================

assignment_time <- system.time({

  assigned <- h3_assign_polygon(
    x = source_h3,
    polygons = polygons,
    id = "feature_id",
    keep = c(
      "feature_name",
      "group_name",
      "class_name"
    ),
    outside = "unassigned"
  )

})


# =============================================================================
# 4. Inspect lightweight joined table
# =============================================================================

cat(
  "\n",
  paste(rep("=", 72), collapse = ""),
  "\nH3 ASSIGNMENT ATTRIBUTE QA\n",
  paste(rep("=", 72), collapse = ""),
  "\n\n",
  sep = ""
)

cat(
  "H3 cells:          ",
  format(nrow(assigned), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Assignment time:   ",
  round(assignment_time[["elapsed"]], 3),
  " seconds\n",
  sep = ""
)

cat(
  "Result inherits sf: ",
  inherits(assigned, "sf"),
  "\n\n",
  sep = ""
)

print(
  head(
    assigned,
    12
  )
)

cat("\nColumns:\n")

print(
  names(assigned)
)


# =============================================================================
# 5. Attribute QA
# =============================================================================

required_columns <- c(
  "h3",
  "polygon_id",
  "assignment_method",
  "assignment_distance",
  "candidate_n",
  "feature_name",
  "group_name",
  "class_name"
)

missing_columns <- setdiff(
  required_columns,
  names(assigned)
)

if (length(missing_columns) > 0L) {
  stop(
    paste0(
      "Missing expected columns: ",
      paste(missing_columns, collapse = ", ")
    )
  )
}

resolved <- !assigned$assignment_method %in% c(
  "ambiguous",
  "unassigned"
)

if (anyNA(assigned$feature_name[resolved])) {
  stop("Resolved H3 cells contain missing feature_name values.")
}

if (anyNA(assigned$group_name[resolved])) {
  stop("Resolved H3 cells contain missing group_name values.")
}

if (anyNA(assigned$class_name[resolved])) {
  stop("Resolved H3 cells contain missing class_name values.")
}


# =============================================================================
# 6. Convert to H3 geometry only for plotting
# =============================================================================

geometry_time <- system.time({

  assigned_geometry <- h3jsr::cell_to_polygon(
    assigned$h3,
    simple = FALSE
  )

})

assigned_map <- assigned_geometry

attribute_columns <- setdiff(
  names(assigned),
  "h3"
)

for (attribute in attribute_columns) {
  assigned_map[[attribute]] <- assigned[[attribute]]
}

cat(
  "\nGeometry conversion: ",
  round(geometry_time[["elapsed"]], 3),
  " seconds\n",
  sep = ""
)

cat(
  "Mapped rows:         ",
  format(nrow(assigned_map), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Mapped object is sf: ",
  inherits(assigned_map, "sf"),
  "\n",
  sep = ""
)


# =============================================================================
# 7. Plot retained attributes
# =============================================================================

old_par <- par(
  mfrow = c(2, 2),
  mar = c(1, 1, 3, 1)
)

plot(
  assigned_map["polygon_id"],
  main = "Polygon identifier",
  key.pos = NULL,
  reset = FALSE
)

plot(
  assigned_map["feature_name"],
  main = "Feature name",
  key.pos = NULL,
  reset = FALSE
)

plot(
  assigned_map["group_name"],
  main = "Group",
  key.pos = NULL,
  reset = FALSE
)

plot(
  assigned_map["class_name"],
  main = "Class",
  key.pos = NULL,
  reset = FALSE
)

par(old_par)


# =============================================================================
# 8. Final QA
# =============================================================================

qa <- data.frame(
  check = c(
    "one_row_per_h3",
    "lightweight_table",
    "feature_name_complete",
    "group_name_complete",
    "class_name_complete",
    "geometry_reconstructed",
    "geometry_row_count"
  ),
  pass = c(
    nrow(assigned) == length(unique(source_h3)),
    !inherits(assigned, "sf"),
    !anyNA(assigned$feature_name[resolved]),
    !anyNA(assigned$group_name[resolved]),
    !anyNA(assigned$class_name[resolved]),
    inherits(assigned_map, "sf"),
    nrow(assigned_map) == nrow(assigned)
  )
)

cat("\nQA:\n")
print(qa)

if (!all(qa$pass)) {
  stop("Assignment attribute QA failed.")
}

cat(
  "\nPASS — retained attributes remain directly accessible and plottable.\n"
)