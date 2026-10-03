# Aggregate attributes to compacted H3 cells

Aggregates explicitly selected additive attributes and collapses
explicitly selected categorical attributes from authoritative source H3
cells to their compacted H3 owners using an H3 compaction lookup.

## Usage

``` r
aggregate_h3(data, lookup, sum = NULL, collapse = NULL, h3_col = "h3")
```

## Arguments

- data:

  A data frame containing one row per authoritative source H3 cell.

- lookup:

  A lookup produced by
  [`h3_compaction_lookup()`](https://georiskexplorer.github.io/h3compactR/reference/h3_compaction_lookup.md).

- sum:

  Optional character vector naming additive numeric fields to aggregate
  by summation.

- collapse:

  Optional character vector naming source attributes whose unique
  non-missing values should be retained for each compacted H3 owner.
  Multiple unique values are returned as a deterministic
  semicolon-separated character string. For each collapsed field, a
  companion `<field>_n` column reports the number of unique non-missing
  source values represented by each compacted H3 owner.

- h3_col:

  Name of the H3 index column in `data`. Defaults to `"h3"`.

## Value

A tibble containing one row per compacted H3 owner, with `compact_h3`,
`compact_resolution`, `source_cell_n`, requested additive fields,
requested collapsed attributes, and a `<field>_n` provenance count for
every collapsed attribute.

## Details

`aggregate_h3()` treats the source H3 grid as the authoritative
analytical support. Values are transferred to compacted H3 cells through
the explicit source-to-owner relationship in `lookup`; compacted polygon
geometry is not used for attribute assignment.

Fields supplied to `sum` and `collapse` are handled differently and must
be selected explicitly. The function does not infer aggregation rules
from field names or data types.

`sum` is intended for additive numeric quantities. Missing additive
values are rejected and additive totals are checked for conservation.

`collapse` is intended for categorical identifiers or descriptive source
attributes. Duplicate values within a compact owner are removed before
values are collapsed. Missing values are ignored when non-missing values
are present. If all source values for a compact owner are missing, the
compact value is `NA_character_`.

Collapsed values are a compact representation of source-cell provenance.
They do not create a new analytical category and do not replace the
authoritative source-to-compact relationship in `lookup`.

Derived measures such as rates, proportions, ratios, relative risks, or
averages should generally be recalculated from their aggregated
components after aggregation rather than aggregated directly.

## References

The source-to-owner hierarchy follows H3 logical containment; see
<https://h3geo.org/docs/highlights/indexing/>. H3 operations are
accessed through O'Brien's `h3jsr` package
([doi:10.32614/CRAN.package.h3jsr](https://doi.org/10.32614/CRAN.package.h3jsr)
).

## Examples

``` r
source <- h3_cover_polygon(toy_polygons[3, ], resolution = 8)
compacted <- compact_h3(source, min_resolution = 7)
lookup <- h3_compaction_lookup(source, compacted)

source_data <- data.frame(
  h3 = source,
  count = rep(1, length(source)),
  source_id = rep("example", length(source))
)

aggregated <- aggregate_h3(
  source_data,
  lookup,
  sum = "count",
  collapse = "source_id"
)
head(aggregated)
#> # A tibble: 6 × 6
#>   compact_h3      compact_resolution source_cell_n count source_id source_id_n
#>   <chr>                        <int>         <int> <dbl> <chr>           <int>
#> 1 87be63091ffffff                  7             7     7 example             1
#> 2 87be63092ffffff                  7             7     7 example             1
#> 3 87be63093ffffff                  7             7     7 example             1
#> 4 87be6309affffff                  7             7     7 example             1
#> 5 87be6309effffff                  7             7     7 example             1
#> 6 87be63461ffffff                  7             7     7 example             1
```
