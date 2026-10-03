# Polygon-to-H3 compaction workflow

## Overview

`h3compactR` provides a small workflow for representing polygon
geography with H3 cells, compacting those cells using established H3
hierarchy operations, retaining the relationship between source and
compacted cells, aggregating attributes explicitly, and validating the
result.

The central analytical rule is:

> The source H3 grid remains the authoritative analytical support.
> Compaction changes its representation; it does not change source-cell
> membership or invent analytical values.

This vignette uses one authoritative source grid throughout the complete
workflow.

``` r

library(h3compactR)
#> Loading required package: sf
#> Linking to GEOS 3.14.1, GDAL 3.12.1, PROJ 9.7.1; sf_use_s2() is TRUE
library(sf)

data(toy_membership_polygons)
```

## 1. Prepare example polygons

The package example geography contains several polygon forms useful for
demonstrating assignment and compaction. For this vignette we create
generic feature identifiers and attributes directly from the example
rows.

``` r

polygons <- toy_membership_polygons

polygons$feature_id <- sprintf(
  "feature_%02d",
  seq_len(nrow(polygons))
)

polygons$feature_name <- paste(
  "Feature",
  seq_len(nrow(polygons))
)

polygons$group_name <- rep(
  c("Group A", "Group B"),
  length.out = nrow(polygons)
)

polygons
#> Simple feature collection with 5 features and 3 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 144.9 ymin: -37.9 xmax: 145.1 ymax: -37.6
#> Geodetic CRS:  WGS 84
#>   feature_id                       geometry feature_name group_name
#> 1 feature_01 POLYGON ((144.9 -37.9, 145 ...    Feature 1    Group A
#> 2 feature_02 POLYGON ((145 -37.9, 145.1 ...    Feature 2    Group B
#> 3 feature_03 POLYGON ((144.9 -37.78, 145...    Feature 3    Group A
#> 4 feature_04 POLYGON ((144.97 -37.74, 14...    Feature 4    Group B
#> 5 feature_05 MULTIPOLYGON (((144.91 -37....    Feature 5    Group A
```

The identifier used for assignment must uniquely identify polygon
features. The package validates polygon topology before performing
polygon relationship or assignment operations.

## 2. Create the authoritative source H3 grid

First create a single polygon footprint from the example features. The
H3 coverage generated from this footprint is the authoritative source
support used for assignment, compaction, lookup, aggregation, and QA
below.

``` r

polygon_union <- sf::st_union(
  sf::st_geometry(polygons)
)

polygon_union <- sf::st_sf(
  geometry = polygon_union
)

source_h3 <- h3_cover_polygon(
  x = polygon_union,
  resolution = 8L,
  boundary = "intersects"
)

length(source_h3)
#> [1] 635
```

### Coverage rules

[`h3_cover_polygon()`](https://georiskexplorer.github.io/h3compactR/reference/h3_cover_polygon.md)
supports four explicit boundary rules:

- `"center"` retains cells whose H3 centre is covered by the polygon;
- `"within"` retains cells fully contained by the polygon;
- `"intersects"` retains cells having any geometric intersection with
  the polygon; and
- `"overlap"` retains cells meeting a specified proportional overlap
  threshold.

For overlap coverage, the retained proportion is

``` math
\frac{\operatorname{area}(\mathrm{H3\ cell}\cap\mathrm{polygon})}
     {\operatorname{area}(\mathrm{H3\ cell})}.
```

For example:

``` r

overlap_h3 <- h3_cover_polygon(
  x = polygon_union,
  resolution = 8L,
  boundary = "overlap",
  min_overlap = 0.5
)
```

The choice of boundary rule is an analytical decision. Geometry-based
rules such as `within`, `intersects`, and `overlap` require more spatial
processing than standard centre-based coverage.

## 3. Optional neighbourhood expansion

[`expand_h3()`](https://georiskexplorer.github.io/h3compactR/reference/expand_h3.md)
adds neighbouring cells at the same H3 resolution. Expansion is separate
from polygon coverage so that the two operations remain explicit.

``` r

expanded_h3 <- expand_h3(
  source_h3,
  rings = 1L
)

c(
  source = length(source_h3),
  expanded = length(expanded_h3)
)
#>   source expanded 
#>      635      801
```

The expanded grid is not used in the remaining workflow. The original
`source_h3` object remains authoritative.

## 4. Assign source cells to polygon features

Assignment is performed on the same source H3 cells created above.

``` r

source_assignment <- h3_assign_polygon(
  x = source_h3,
  polygons = polygons,
  id = "feature_id",
  keep = c("feature_name", "group_name"),
  outside = "unassigned"
)

head(source_assignment)
#> # A tibble: 6 × 7
#>   h3              polygon_id assignment_method assignment_distance candidate_n
#>   <chr>           <chr>      <chr>                           <dbl>       <int>
#> 1 88be631825fffff feature_01 intersects                          0           1
#> 2 88be631901fffff feature_01 centre                              0           1
#> 3 88be631903fffff feature_01 centre                              0           1
#> 4 88be631905fffff feature_01 centre                              0           1
#> 5 88be631907fffff feature_01 centre                              0           1
#> 6 88be631909fffff feature_01 centre                              0           1
#> # ℹ 2 more variables: feature_name <chr>, group_name <chr>

table(
  source_assignment$assignment_method,
  useNA = "ifany"
)
#> 
#>          centre      intersects largest_overlap 
#>             537              96               2
```

Assignment methods describe how ownership was established. Depending on
the geometry, these may include centre-based assignment, a unique
intersection, largest overlap, nearest assignment when requested,
ambiguity, or an unassigned result.

For this example the authoritative footprint should resolve to one
polygon owner per source H3 cell.

``` r

stopifnot(
  nrow(source_assignment) == length(source_h3),
  identical(sort(source_assignment$h3), sort(source_h3)),
  !any(source_assignment$assignment_method %in% c("ambiguous", "unassigned")),
  !anyNA(source_assignment$polygon_id)
)
```

## 5. Compact the source H3 representation

Compaction operates on the authoritative source indexes.
`min_resolution` defines the coarsest H3 resolution permitted in the
compact representation.

``` r

compacted_h3 <- compact_h3(
  x = source_h3,
  min_resolution = 7L
)

c(
  source = length(source_h3),
  compacted = length(compacted_h3)
)
#>    source compacted 
#>       635       239
```

The result may contain more than one H3 resolution. This is expected:
compaction replaces complete groups of child cells with their H3 parent
where the hierarchy permits it.

Compaction should therefore be understood as a hierarchical
representation of the source grid rather than a new analytical support.

## 6. Build the source-to-compact lookup

[`h3_compaction_lookup()`](https://georiskexplorer.github.io/h3compactR/reference/h3_compaction_lookup.md)
records exactly which compact H3 cell owns each source H3 cell.

``` r

lookup <- h3_compaction_lookup(
  source = source_h3,
  compacted = compacted_h3
)

head(lookup)
#> # A tibble: 6 × 5
#>   source_h3       source_resolution compact_h3   compact_resolution relationship
#>   <chr>                       <int> <chr>                     <int> <chr>       
#> 1 88be631825fffff                 8 88be631825f…                  8 exact       
#> 2 88be631901fffff                 8 87be63190ff…                  7 ancestor    
#> 3 88be631903fffff                 8 87be63190ff…                  7 ancestor    
#> 4 88be631905fffff                 8 87be63190ff…                  7 ancestor    
#> 5 88be631907fffff                 8 87be63190ff…                  7 ancestor    
#> 6 88be631909fffff                 8 87be63190ff…                  7 ancestor

table(lookup$relationship)
#> 
#> ancestor    exact 
#>      462      173
```

Each authoritative source cell must occur exactly once in this
relationship.

``` r

stopifnot(
  nrow(lookup) == length(source_h3),
  identical(sort(lookup$source_h3), sort(source_h3))
)
```

This lookup is the analytical bridge between the source grid and the
compacted representation.

## 7. Aggregate attributes safely

Additive measures can be summed through the lookup. Categorical
attributes can be collapsed to retain provenance.

Here we create a small deterministic additive measure on the source
cells.

``` r

source_data <- source_assignment

source_data$observations <- rep(
  c(0, 1, 2, 1),
  length.out = nrow(source_data)
)

source_data$source_weight <- seq_len(
  nrow(source_data)
)

head(source_data)
#> # A tibble: 6 × 9
#>   h3              polygon_id assignment_method assignment_distance candidate_n
#>   <chr>           <chr>      <chr>                           <dbl>       <int>
#> 1 88be631825fffff feature_01 intersects                          0           1
#> 2 88be631901fffff feature_01 centre                              0           1
#> 3 88be631903fffff feature_01 centre                              0           1
#> 4 88be631905fffff feature_01 centre                              0           1
#> 5 88be631907fffff feature_01 centre                              0           1
#> 6 88be631909fffff feature_01 centre                              0           1
#> # ℹ 4 more variables: feature_name <chr>, group_name <chr>, observations <dbl>,
#> #   source_weight <int>
```

Now aggregate using the lookup created from exactly the same
authoritative H3 support.

``` r

compact_data <- aggregate_h3(
  data = source_data,
  lookup = lookup,
  sum = c("observations", "source_weight"),
  collapse = c(
    "polygon_id",
    "feature_name",
    "group_name"
  ),
  h3_col = "h3"
)

head(compact_data)
#> # A tibble: 6 × 11
#>   compact_h3      compact_resolution source_cell_n observations source_weight
#>   <chr>                        <int>         <int>        <dbl>         <dbl>
#> 1 87be63190ffffff                  7             7            8            35
#> 2 87be63192ffffff                  7             7            7           126
#> 3 87be63193ffffff                  7             7            8           175
#> 4 87be63196ffffff                  7             7            7           280
#> 5 87be63406ffffff                  7             7            7           378
#> 6 87be63409ffffff                  7             7            8           455
#> # ℹ 6 more variables: polygon_id <chr>, polygon_id_n <int>, feature_name <chr>,
#> #   feature_name_n <int>, group_name <chr>, group_name_n <int>
```

[`aggregate_h3()`](https://georiskexplorer.github.io/h3compactR/reference/aggregate_h3.md)
does not infer aggregation semantics. Additive and categorical fields
are declared explicitly.

Collapsed categorical values record source provenance. They are not new
analytical categories. Where more than one unique source value
contributes to a compact owner, the values are retained in deterministic
collapsed form and the corresponding `*_n` field records the number of
unique non-missing source values represented.

### Check additive conservation

``` r

stopifnot(
  sum(source_data$observations) ==
    sum(compact_data$observations),
  sum(source_data$source_weight) ==
    sum(compact_data$source_weight)
)

c(
  source_observations = sum(source_data$observations),
  compact_observations = sum(compact_data$observations),
  source_weight = sum(source_data$source_weight),
  compact_weight = sum(compact_data$source_weight)
)
#>  source_observations compact_observations        source_weight 
#>                  635                  635               201930 
#>       compact_weight 
#>               201930
```

This conservation is fundamental: every authoritative source H3
contributes its additive value exactly once to exactly one compact
owner.

Derived quantities such as rates, proportions, and percentages should
not normally be averaged during compaction. Their appropriate numerator
and denominator components should be aggregated and the derived quantity
recalculated afterwards.

## 8. Validate the compaction

[`qa_h3_compaction()`](https://georiskexplorer.github.io/h3compactR/reference/qa_h3_compaction.md)
checks the structural relationship between the source and compacted H3
representations.

``` r

qa <- qa_h3_compaction(
  source = source_h3,
  compacted = compacted_h3
)

qa
#> $summary
#> # A tibble: 1 × 16
#>   source_cell_n compact_cell_n reduction_n reduction_pct source_resolution
#>           <int>          <int>       <int>         <dbl>             <int>
#> 1           635            239         396          62.4                 8
#> # ℹ 11 more variables: compact_resolution_min <int>,
#> #   compact_resolution_max <int>, exact_source_n <int>,
#> #   ancestor_source_n <int>, unmatched_source_n <int>,
#> #   roundtrip_missing_n <int>, roundtrip_additional_n <int>,
#> #   hierarchy_overlap_n <int>, exact_reconstruction <lgl>,
#> #   hierarchy_integrity <lgl>, ownership_integrity <lgl>
#> 
#> $resolution
#> # A tibble: 2 × 2
#>   compact_resolution compact_cell_n
#>                <int>          <int>
#> 1                  7             66
#> 2                  8            173
#> 
#> $ownership
#> # A tibble: 2 × 3
#>   relationship compact_resolution source_cell_n
#>   <chr>                     <int>         <int>
#> 1 ancestor                      7           462
#> 2 exact                         8           173
#> 
#> attr(,"class")
#> [1] "h3_compaction_qa" "list"
```

The QA process checks round-trip reconstruction, hierarchy consistency,
source ownership, resolution composition, and the reduction achieved by
compaction.

## 9. Cartographic inspection

[`plot_h3()`](https://georiskexplorer.github.io/h3compactR/reference/plot_h3.md)
provides a lightweight way to inspect H3 geometry against polygon
context.

``` r

plot_h3(
  source_h3,
  context = polygons,
  main = "Source H3 representation"
)
```

![](h3compactR-workflow_files/figure-html/plot-source-1.png)

``` r

plot_h3(
  compacted_h3,
  context = polygons,
  main = "Compacted H3 representation"
)
```

![](h3compactR-workflow_files/figure-html/plot-compact-1.png)

Visual inspection is useful because a compact H3 parent may extend
geographically beyond the exact union of its represented source cells.
H3 parent-child hierarchy is logically exact, but polygon geometry
across resolutions should not be interpreted as exact geographic
containment.

For that reason, source membership and analytical values remain attached
to the authoritative source support and are transferred through the
explicit lookup rather than inferred from the visible footprint of a
compact parent.

## 10. Polygon membership as a separate relationship

[`h3_polygon_membership()`](https://georiskexplorer.github.io/h3compactR/reference/h3_polygon_membership.md)
can be used when the required result is the geometric relationship
between H3 cells and polygon features rather than a single assigned
owner.

``` r

membership <- h3_polygon_membership(
  x = source_h3,
  polygons = polygons,
  id = "feature_id"
)

head(membership)
#> # A tibble: 6 × 2
#>   h3              polygon_id
#>   <chr>           <chr>     
#> 1 88be631825fffff feature_01
#> 2 88be631901fffff feature_01
#> 3 88be631903fffff feature_01
#> 4 88be631905fffff feature_01
#> 5 88be631907fffff feature_01
#> 6 88be631909fffff feature_01
```

Membership and assignment answer different questions:

- membership records H3-to-polygon relationships;
- assignment resolves one polygon owner for each H3 cell.

Keeping these operations separate avoids silently converting a geometric
relationship into an ownership rule.

## 11. Computational scale

Standard centre-based H3 coverage is substantially less computationally
demanding than geometry-based coverage rules. `within`, `intersects`,
and `overlap` require additional H3 geometry construction and spatial
operations and can become computationally expensive for very large areas
at fine H3 resolutions.

There is no universal maximum cell count that is appropriate for every
workflow. Resolution and boundary rule should be selected according to
the analytical purpose, and large workflows should be tested on
representative subsets before scaling.

Performance optimisation for very large geometry-heavy workflows is an
area for future package development.

## 12. Analytical principles

The workflow demonstrated here follows several deliberate principles:

1.  **The source H3 grid is authoritative.** Compaction changes
    representation, not source support.
2.  **Compaction follows the H3 hierarchy.** It does not create a new H3
    compaction algorithm.
3.  **Source-to-compact relationships are explicit.** Analytical values
    are not inferred from compact polygon appearance.
4.  **Aggregation follows variable meaning.** Additive measures are
    summed; categorical fields may retain provenance; derived measures
    should be recalculated from appropriate components.
5.  **QA is part of the workflow.** Round-trip reconstruction,
    ownership, hierarchy, conservation, and cartographic inspection
    provide complementary checks.
6.  **Spatial relationships and ownership are separate concepts.**
    Membership does not silently imply assignment.

## Attribution

`h3compactR` builds on the H3 hierarchical geospatial indexing system
and the H3 operations exposed to R through `h3jsr`. Spatial geometry
operations use the `sf` ecosystem.

The package does not introduce a new H3 compaction algorithm. Its
contribution is the reproducible analytical workflow around polygon
coverage choices, hierarchical compaction, source-to-compact
relationships, explicit attribute aggregation, provenance, quality
assurance, and cartographic representation.

The package’s emphasis on making the choice and interpretation of
geographic units explicit is also informed by the broader cartographic
literature on geographic units and choropleth mapping, including Boscoe
and Pickle (2003).

See the package references for full attribution and source details.
