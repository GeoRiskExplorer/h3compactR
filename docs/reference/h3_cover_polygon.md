# Cover polygons with H3 cells

Creates an H3 representation of polygon geometry using one of four
explicitly defined polygon coverage rules.

## Usage

``` r
h3_cover_polygon(x, resolution, boundary = "center", min_overlap = NULL)
```

## Arguments

- x:

  An `sf` or `sfc` object containing `POLYGON` or `MULTIPOLYGON`
  geometry.

- resolution:

  A single integer H3 resolution from 0 to 15.

- boundary:

  Character string defining the polygon-to-H3 coverage rule. One of
  `"center"`, `"within"`, `"intersects"`, or `"overlap"`.

- min_overlap:

  Minimum proportion of an H3 cell that must be covered by the source
  polygon when `boundary = "overlap"`. Must be greater than 0 and less
  than or equal to 1. Must be `NULL` for all other boundary rules.

## Value

A character vector containing unique H3 cell indexes. If no cells
satisfy the requested coverage rule, `character(0)` is returned.

## Details

H3 indexing is performed in longitude/latitude coordinates using
EPSG:4326. Input geometry may use another coordinate reference system
and is transformed internally.

The available coverage rules answer different spatial questions:

- `"center"` uses standard H3 centre-based polygon coverage. A cell is
  included when its H3 cell centre lies within the source polygon.

- `"within"` retains only H3 cells completely contained by the source
  polygon.

- `"intersects"` retains H3 cells that have any geometric intersection
  with the source polygon.

- `"overlap"` retains H3 cells where at least `min_overlap` of the H3
  cell area is covered by the source polygon.

For overlap coverage, the proportion is defined as

\$\$ area(H3 cell intersect polygon) / area(H3 cell) \$\$

When `min_overlap = 1`, complete geometric containment is evaluated
using the same topological containment rule as `boundary = "within"`.
This avoids numerical ambiguity from comparing projected area ratios to
exactly one.

Area calculations for proportional overlap thresholds below one are
performed in a local projected coordinate reference system rather than
directly in longitude/latitude coordinates.

The function returns H3 indexes as character values. It does not return
polygon geometry and does not perform H3 compaction.

Geometric coverage rules such as `"within"`, `"intersects"`, and
`"overlap"` require H3 polygon conversion and spatial predicates and are
therefore expected to be slower and more memory intensive than standard
centre-based coverage.

Large polygons at fine H3 resolutions can generate millions of cells and
may require substantial memory. The function does not impose an
arbitrary maximum polygon size or H3 resolution.

## References

H3 indexing: <https://h3geo.org/>. R access is provided through
O'Brien's `h3jsr` package
([doi:10.32614/CRAN.package.h3jsr](https://doi.org/10.32614/CRAN.package.h3jsr)
). Spatial geometry operations use `sf`; see Pebesma (2018),
[doi:10.32614/RJ-2018-009](https://doi.org/10.32614/RJ-2018-009) .

## Examples

``` r
cells <- h3_cover_polygon(toy_polygons[1, ], resolution = 7)
length(cells)
#> [1] 17

intersecting <- h3_cover_polygon(
  toy_polygons[1, ],
  resolution = 7,
  boundary = "intersects"
)
length(intersecting)
#> [1] 28
```
