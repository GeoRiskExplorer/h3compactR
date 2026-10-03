# Audit an H3 compaction

Performs analytical quality-assurance checks on the relationship between
an authoritative single-resolution H3 source grid and its compacted
mixed-resolution representation.

## Usage

``` r
qa_h3_compaction(source, compacted, lookup = NULL)
```

## Arguments

- source:

  A character vector of authoritative source H3 cell indexes. Source
  cells must all have the same H3 resolution.

- compacted:

  A character vector of compacted H3 cell indexes.

- lookup:

  Optional source-to-compacted ownership lookup produced by
  [`h3_compaction_lookup()`](https://georiskexplorer.github.io/h3compactR/reference/h3_compaction_lookup.md).
  If `NULL`, the lookup is generated internally.

## Value

A list of class `"h3_compaction_qa"` containing:

- summary:

  One-row tibble containing the main QA metrics.

- resolution:

  Tibble describing the compacted resolution mix.

- ownership:

  Tibble describing source-cell ownership by compact resolution and
  relationship.

## Details

The source H3 grid is treated as the authoritative analytical support.
Quality assurance therefore evaluates whether the compacted
representation reconstructs that support exactly and whether every
source cell has one valid compact owner.

The audit is based on H3 hierarchy rather than displayed polygon
geometry. Apparent geometric extension of a mixed-resolution H3 polygon
beyond a study boundary does not imply additional analytical membership.

The audit reports:

- source and compacted cell counts;

- reduction in cell count;

- source and compacted resolutions;

- exact and ancestor ownership counts;

- round-trip missing and additional source cells;

- exact reconstruction status;

- hierarchy integrity; and

- ownership integrity.

## References

H3 hierarchy, compaction and uncompaction:
<https://h3geo.org/docs/highlights/indexing/>.

## Examples

``` r
source <- h3_cover_polygon(toy_polygons[3, ], resolution = 8)
compacted <- compact_h3(source, min_resolution = 7)
qa <- qa_h3_compaction(source, compacted)
qa$summary
#> # A tibble: 1 × 16
#>   source_cell_n compact_cell_n reduction_n reduction_pct source_resolution
#>           <int>          <int>       <int>         <dbl>             <int>
#> 1           183             81         102          55.7                 8
#> # ℹ 11 more variables: compact_resolution_min <int>,
#> #   compact_resolution_max <int>, exact_source_n <int>,
#> #   ancestor_source_n <int>, unmatched_source_n <int>,
#> #   roundtrip_missing_n <int>, roundtrip_additional_n <int>,
#> #   hierarchy_overlap_n <int>, exact_reconstruction <lgl>,
#> #   hierarchy_integrity <lgl>, ownership_integrity <lgl>
```
