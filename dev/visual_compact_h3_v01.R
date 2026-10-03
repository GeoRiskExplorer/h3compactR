# =============================================================================
# h3compactR
# Visual QA — exact H3 compaction
# =============================================================================
#
# Purpose:
#   Compare exact H3 compaction across different minimum permitted resolutions.
#
#   The authoritative source support is generated at H3 resolution 9.
#   Compaction is then compared using:
#
#     - minimum resolution 9
#     - minimum resolution 8
#     - minimum resolution 7
#     - unrestricted native H3 compaction
#
#   Cells are coloured by their actual retained H3 resolution.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load development package
# -----------------------------------------------------------------------------

devtools::load_all()


# -----------------------------------------------------------------------------
# 2. Check visual QA dependencies
# -----------------------------------------------------------------------------

required_packages <- c(
  "ggplot2",
  "patchwork"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing visual QA package(s): ",
    paste(missing_packages, collapse = ", ")
  )
}


# -----------------------------------------------------------------------------
# 3. Visual QA configuration
# -----------------------------------------------------------------------------

geometry_id <- "complex"
source_resolution <- 9L

figure_dir <- file.path(
  "dev",
  "figures"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_file <- file.path(
  figure_dir,
  "compact_h3_complex_res9.png"
)


# -----------------------------------------------------------------------------
# 4. Select source polygon
# -----------------------------------------------------------------------------

source_polygon <- toy_polygons[
  toy_polygons$geometry_id == geometry_id,
]

if (nrow(source_polygon) != 1L) {
  stop(
    "Expected exactly one toy polygon for geometry_id = ",
    geometry_id
  )
}


# -----------------------------------------------------------------------------
# 5. Generate authoritative source H3 support
# -----------------------------------------------------------------------------

source_h3 <- h3_cover_polygon(
  source_polygon,
  resolution = source_resolution,
  boundary = "center"
)

if (length(source_h3) == 0L) {
  stop(
    "Source H3 coverage is empty."
  )
}


# -----------------------------------------------------------------------------
# 6. Generate exact compaction scenarios
# -----------------------------------------------------------------------------

compact_r9 <- compact_h3(
  source_h3,
  min_resolution = 9L
)

compact_r8 <- compact_h3(
  source_h3,
  min_resolution = 8L
)

compact_r7 <- compact_h3(
  source_h3,
  min_resolution = 7L
)

compact_native <- compact_h3(
  source_h3
)


# -----------------------------------------------------------------------------
# 7. Numerical QA
# -----------------------------------------------------------------------------

scenario_list <- list(
  "Minimum R9" = compact_r9,
  "Minimum R8" = compact_r8,
  "Minimum R7" = compact_r7,
  "Unrestricted" = compact_native
)

scenario_floor <- c(
  "Minimum R9" = 9L,
  "Minimum R8" = 8L,
  "Minimum R7" = 7L,
  "Unrestricted" = NA_integer_
)

for (scenario_name in names(scenario_list)) {

  compacted <- scenario_list[[scenario_name]]

  compact_resolutions <- h3jsr::get_res(
    compacted
  )

  # ---------------------------------------------------------------------------
  # 7.1 All output must remain valid H3
  # ---------------------------------------------------------------------------

  stopifnot(
    all(
      h3jsr::is_valid(compacted)
    )
  )


  # ---------------------------------------------------------------------------
  # 7.2 No output may be finer than source support
  # ---------------------------------------------------------------------------

  stopifnot(
    all(
      compact_resolutions <= source_resolution
    )
  )


  # ---------------------------------------------------------------------------
  # 7.3 Resolution floor must be respected
  # ---------------------------------------------------------------------------

  floor_value <- scenario_floor[
    scenario_name
  ]

  if (!is.na(floor_value)) {

    stopifnot(
      all(
        compact_resolutions >= floor_value
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 7.4 Exact round-trip reconstruction
  # ---------------------------------------------------------------------------

  reconstructed <- h3jsr::uncompact(
    compacted,
    res = source_resolution,
    simple = TRUE
  ) |>
    unlist(
      use.names = FALSE
    ) |>
    as.character() |>
    unique()

  stopifnot(
    setequal(
      source_h3,
      reconstructed
    )
  )
}


# -----------------------------------------------------------------------------
# 8. Build plotting geometry
# -----------------------------------------------------------------------------

make_plot_data <- function(
  h3,
  scenario
) {

  geometry <- h3jsr::cell_to_polygon(
    h3,
    simple = FALSE
  )

  names(geometry)[
    names(geometry) == "h3_address"
  ] <- "h3"

  geometry$resolution <- factor(
    h3jsr::get_res(
      geometry$h3
    ),
    levels = 0:15
  )

  geometry$scenario <- scenario

  geometry
}


plot_r9 <- make_plot_data(
  compact_r9,
  "Minimum R9"
)

plot_r8 <- make_plot_data(
  compact_r8,
  "Minimum R8"
)

plot_r7 <- make_plot_data(
  compact_r7,
  "Minimum R7"
)

plot_native <- make_plot_data(
  compact_native,
  "Unrestricted"
)


# -----------------------------------------------------------------------------
# 9. Scenario statistics
# -----------------------------------------------------------------------------

scenario_stats <- data.frame(
  scenario = names(scenario_list),
  source_cells = length(source_h3),
  compact_cells = vapply(
    scenario_list,
    length,
    integer(1)
  ),
  stringsAsFactors = FALSE
)

scenario_stats$reduction_n <-
  scenario_stats$source_cells -
  scenario_stats$compact_cells

scenario_stats$reduction_pct <-
  100 *
  scenario_stats$reduction_n /
  scenario_stats$source_cells


# -----------------------------------------------------------------------------
# 10. Plot helper
# -----------------------------------------------------------------------------

make_panel <- function(
  geometry,
  scenario_name
) {

  stat <- scenario_stats[
    scenario_stats$scenario == scenario_name,
  ]

  subtitle <- paste0(
    "Source: ",
    format(
      stat$source_cells,
      big.mark = ","
    ),
    "  |  Compact: ",
    format(
      stat$compact_cells,
      big.mark = ","
    ),
    "  |  Reduction: ",
    sprintf(
      "%.1f%%",
      stat$reduction_pct
    )
  )

  ggplot2::ggplot() +

    ggplot2::geom_sf(
      data = source_polygon,
      fill = NA,
      linewidth = 0.8
    ) +

    ggplot2::geom_sf(
      data = geometry,
      ggplot2::aes(
        fill = resolution
      ),
      linewidth = 0.15
    ) +

    ggplot2::geom_sf(
      data = source_polygon,
      fill = NA,
      linewidth = 0.8
    ) +

    ggplot2::labs(
      title = scenario_name,
      subtitle = subtitle,
      fill = "H3 resolution"
    ) +

    ggplot2::coord_sf(
      datum = NA
    ) +

    ggplot2::theme_void() +

    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9
      ),
      legend.position = "bottom"
    )
}


# -----------------------------------------------------------------------------
# 11. Create comparison figure
# -----------------------------------------------------------------------------

panel_r9 <- make_panel(
  plot_r9,
  "Minimum R9"
)

panel_r8 <- make_panel(
  plot_r8,
  "Minimum R8"
)

panel_r7 <- make_panel(
  plot_r7,
  "Minimum R7"
)

panel_native <- make_panel(
  plot_native,
  "Unrestricted"
)

comparison_plot <-
  (
    panel_r9 +
      panel_r8
  ) /
  (
    panel_r7 +
      panel_native
  ) +
  patchwork::plot_annotation(
    title = "Exact H3 compaction",
    subtitle = paste0(
      "Geometry: ",
      geometry_id,
      " | Authoritative source resolution: ",
      source_resolution
    )
  )


# -----------------------------------------------------------------------------
# 12. Display visual QA
# -----------------------------------------------------------------------------

print(comparison_plot)


# -----------------------------------------------------------------------------
# 13. Console QA summary
# -----------------------------------------------------------------------------

cat(
  "\n",
  paste0(rep("=", 70), collapse = ""),
  "\n",
  "COMPACT_H3 VISUAL QA\n",
  paste0(rep("=", 70), collapse = ""),
  "\n",
  sep = ""
)

cat(
  "Geometry:           ", geometry_id,
  "\n",
  "Source resolution:  ", source_resolution,
  "\n",
  "Source cells:       ", length(source_h3),
  "\n\n",
  sep = ""
)

for (scenario_name in names(scenario_list)) {

  compacted <- scenario_list[[scenario_name]]
  resolutions <- h3jsr::get_res(compacted)

  stat <- scenario_stats[
    scenario_stats$scenario == scenario_name,
  ]

  cat(
    scenario_name,
    "\n",
    "  Compact cells:    ", stat$compact_cells,
    "\n",
    "  Reduction:        ", sprintf("%.2f%%", stat$reduction_pct),
    "\n",
    "  Resolutions:      ",
    paste(sort(unique(resolutions)), collapse = ", "),
    "\n",
    sep = ""
  )

  resolution_table <- table(resolutions)

  cat(
    "  Resolution mix:   ",
    paste(
      paste0(
        "R",
        names(resolution_table),
        "=",
        as.integer(resolution_table)
      ),
      collapse = " | "
    ),
    "\n\n",
    sep = ""
  )
}

cat(
  paste0(rep("=", 70), collapse = ""),
  "\n",
  sep = ""
)