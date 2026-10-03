# Compact H3 cells with an optional resolution floor

Compacts a single-resolution H3 cell set using the established H3
hierarchical compaction operation.

## Usage

``` r
compact_h3(x, min_resolution = NULL)
```

## Arguments

- x:

  A character vector of H3 cell indexes. All input cells must be at the
  same H3 resolution.

- min_resolution:

  Optional integer defining the coarsest H3 resolution permitted in the
  compacted result. For example, with source cells at resolution 10 and
  `min_resolution = 8`, the output may contain resolution 8, 9, and 10
  cells, but never resolution 7 or coarser. When `NULL`, native H3
  compaction is returned without a package-imposed resolution floor.

## Value

A character vector containing unique compacted H3 cell indexes.

## Details

An optional minimum resolution can be supplied to prevent the compacted
representation from containing cells coarser than a specified H3
resolution.

`compact_h3()` performs exact hierarchical H3 compaction. It does not
use partial child coverage, polygon intersection, geometric clipping, or
relaxed compaction rules.

The underlying compaction operation is provided by
[`h3jsr::compact()`](https://obrl-soil.github.io/h3jsr/reference/compact.html).

When `min_resolution` is supplied, native H3 compaction is performed
first. Any resulting cells coarser than the requested minimum resolution
are then expanded back to `min_resolution` using the established H3
hierarchy.

Consequently, the resolution constraint does not introduce additional H3
membership. Uncompacting the result to the original source resolution
should reconstruct the original H3 cell set exactly.

Input is restricted to a single source resolution so that the
authoritative analytical support is explicit. Mixed-resolution H3 is an
output of compaction rather than an accepted source representation.

## References

H3 hierarchical indexing and compaction:
<https://h3geo.org/docs/highlights/indexing/>. R access is provided
through O'Brien's `h3jsr` package
([doi:10.32614/CRAN.package.h3jsr](https://doi.org/10.32614/CRAN.package.h3jsr)
).

## Examples

``` r
source <- h3_cover_polygon(toy_polygons[3, ], resolution = 8)
compacted <- compact_h3(source, min_resolution = 7)
c(source = length(source), compacted = length(compacted))
#>    source compacted 
#>       183        81 
```
