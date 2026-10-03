# Expand an H3 cell set by neighbouring cells

Expands an existing set of H3 cells by including neighbouring cells
within a specified H3 grid distance.

## Usage

``` r
expand_h3(x, rings = 1L)
```

## Arguments

- x:

  A character vector of H3 cell indexes.

- rings:

  A single non-negative integer defining the H3 grid distance used for
  expansion. `rings = 0` returns the original cell set, `rings = 1`
  includes immediately neighbouring cells, and larger values
  progressively expand the cell set.

## Value

A character vector containing unique H3 cell indexes.

## Details

This operation is independent of polygon coverage. It can be applied to
any valid H3 cell set and does not alter the resolution of the supplied
cells.

Expansion uses the H3 grid neighbourhood around each supplied cell.
Results from all input cells are combined and duplicate H3 indexes are
removed.

The function expects all supplied H3 cells to have the same resolution.
Mixed-resolution input is rejected because grid-distance neighbourhoods
are defined relative to cells at a common H3 resolution.

Expansion does not test whether newly included cells intersect an
original polygon or other source geometry. It is a purely H3-based
neighbourhood operation.

## References

H3 grid hierarchy and neighbourhood operations: <https://h3geo.org/>. R
access is provided through O'Brien's `h3jsr` package
([doi:10.32614/CRAN.package.h3jsr](https://doi.org/10.32614/CRAN.package.h3jsr)
).

## Examples

``` r
cells <- h3_cover_polygon(toy_polygons[1, ], resolution = 7)
expanded <- expand_h3(cells, rings = 1)
c(source = length(cells), expanded = length(expanded))
#>   source expanded 
#>       17       36 
```
