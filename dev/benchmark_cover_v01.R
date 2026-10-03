library(h3compactR)
library(sf)
library(h3jsr)

data("toy_polygons", package = "h3compactR")

cat(
  "\n",
  paste(rep("=", 72), collapse = ""),
  "\nH3COMPACTR — COVERAGE BENCHMARK V01\n",
  paste(rep("=", 72), collapse = ""),
  "\n\n",
  sep = ""
)

# ---------------------------------------------------------------------------
# Test geometries
# ---------------------------------------------------------------------------

sizes <- c(
  small = 1,
  medium = 5,
  large = 20
)

base <- toy_polygons[
  toy_polygons$geometry_id == "complex",
]

make_scaled_polygon <- function(x, factor) {

  geom <- st_geometry(x)

  centroid <- st_centroid(st_union(geom))

  scaled <- (geom - centroid) * factor + centroid

  st_sf(
    geometry = scaled,
    crs = st_crs(x)
  )
}

geometries <- lapply(
  sizes,
  \(factor) make_scaled_polygon(base, factor)
)

# ---------------------------------------------------------------------------
# Benchmark grid
# ---------------------------------------------------------------------------

resolutions <- 6:11

results <- list()
i <- 1L

for (size_name in names(geometries)) {

  x <- geometries[[size_name]]

  for (res in resolutions) {

    gc()

    timing <- system.time({
      cells <- h3_cover_polygon(
        x,
        resolution = res
      )
    })

    results[[i]] <- data.frame(
      size = size_name,
      resolution = res,
      cells = length(cells),
      elapsed_sec = unname(timing["elapsed"]),
      user_sec = unname(timing["user.self"]),
      system_sec = unname(timing["sys.self"]),
      stringsAsFactors = FALSE
    )

    cat(
      sprintf(
        "%-8s | res %2d | %10s cells | %8.3f sec\n",
        size_name,
        res,
        format(length(cells), big.mark = ","),
        unname(timing["elapsed"])
      )
    )

    i <- i + 1L
  }
}

benchmark_results <- do.call(
  rbind,
  results
)

cat("\n")
print(
  benchmark_results,
  row.names = FALSE
)

cat("\nSession:\n")
print(sessionInfo())