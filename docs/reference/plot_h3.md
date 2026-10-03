# Plot H3 cells

Provides a simple cartographic representation of H3 cells, with optional
source polygon context.

## Usage

``` r
plot_h3(x, context = NULL, ...)
```

## Arguments

- x:

  A character vector of valid H3 cell indexes.

- context:

  Optional `sf` or `sfc` polygon geometry to display as geographic
  context.

- ...:

  Additional graphical parameters passed to the H3 cell plot.

## Value

Invisibly returns the H3 cell geometry used for plotting.

## Details

H3 indexes are converted to polygon geometry for display. The H3 indexes
remain the analytical identifiers; the generated geometry is a
cartographic representation.

This function is intended as the common plotting interface for
`h3compactR`. Additional package objects and visual QA representations
may be supported through this interface in future versions.

## References

H3 cell geometry access is provided through O'Brien's `h3jsr` package
([doi:10.32614/CRAN.package.h3jsr](https://doi.org/10.32614/CRAN.package.h3jsr)
); spatial geometry uses `sf`.

## Examples

``` r
cells <- h3_cover_polygon(
  toy_polygons[toy_polygons$geometry_id == "regular", ],
  resolution = 8
)

plot_h3(
  cells,
  context = toy_polygons[
    toy_polygons$geometry_id == "regular",
  ]
)

```
