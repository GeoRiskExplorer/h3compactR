# Build a source-to-compacted H3 lookup

Creates a one-to-one lookup between authoritative source H3 cells and
the compacted H3 cells that represent them.

## Usage

``` r
h3_compaction_lookup(source, compacted)
```

## Arguments

- source:

  A character vector of authoritative source H3 cell indexes. Source
  cells must all have the same H3 resolution.

- compacted:

  A character vector of H3 cell indexes representing an exact
  hierarchical compaction of `source`. The compacted vector may contain
  multiple H3 resolutions.

## Value

A tibble with one row per source H3 cell and five columns:

- source_h3:

  Authoritative source H3 cell index.

- source_resolution:

  H3 resolution of the source cell.

- compact_h3:

  Compacted H3 cell that owns the source cell.

- compact_resolution:

  H3 resolution of the compacted owner.

- relationship:

  Either `"exact"` or `"ancestor"`.

## Details

Each source cell is assigned to exactly one compacted cell: either the
same H3 cell (`"exact"`) or a retained H3 ancestor (`"ancestor"`).

The source H3 grid remains the authoritative analytical support. This
function records how those source cells are represented by a
mixed-resolution compacted H3 grid.

Assignment is based entirely on the H3 hierarchy. No polygon
intersection, spatial overlay, centroid matching, or other geometric
operation is used.

A compacted cell may geometrically extend beyond the original study
footprint when displayed as a polygon. Such geometric area does not
create additional source membership. Only cells supplied in `source`
receive lookup rows.

The function requires complete one-to-one source ownership. It errors if
a source cell cannot be assigned to a compacted cell, if more than one
compacted cell could own the same source cell, or if the compacted
representation contains cells finer than the source resolution.

## References

H3 hierarchy and logical containment:
<https://h3geo.org/docs/highlights/indexing/>. R access is provided
through O'Brien's `h3jsr` package
([doi:10.32614/CRAN.package.h3jsr](https://doi.org/10.32614/CRAN.package.h3jsr)
).

## Examples

``` r
source <- h3_cover_polygon(toy_polygons[3, ], resolution = 8)
compacted <- compact_h3(source, min_resolution = 7)
lookup <- h3_compaction_lookup(source, compacted)
head(lookup)
#> # A tibble: 6 × 5
#>   source_h3       source_resolution compact_h3   compact_resolution relationship
#>   <chr>                       <int> <chr>                     <int> <chr>       
#> 1 88be630903fffff                 8 88be630903f…                  8 exact       
#> 2 88be630907fffff                 8 88be630907f…                  8 exact       
#> 3 88be630911fffff                 8 87be63091ff…                  7 ancestor    
#> 4 88be630913fffff                 8 87be63091ff…                  7 ancestor    
#> 5 88be630915fffff                 8 87be63091ff…                  7 ancestor    
#> 6 88be630917fffff                 8 87be63091ff…                  7 ancestor    
```
