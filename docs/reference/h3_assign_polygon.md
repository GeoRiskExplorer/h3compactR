# Assign H3 cells to polygon features

Assigns H3 cells to polygon features using polygon membership and,
optionally, nearest-polygon assignment for cells outside the polygon
geography.

## Usage

``` r
h3_assign_polygon(
  x,
  polygons,
  id,
  keep = NULL,
  outside = c("unassigned", "nearest")
)
```

## Arguments

- x:

  A character vector of valid H3 cell indexes.

- polygons:

  An `sf` object containing polygon or multipolygon features.

- id:

  Name of the polygon identifier column.

- keep:

  Optional character vector naming additional polygon attributes to
  retain in the returned assignment table. Polygon geometry is not
  retained. Values are transferred after polygon assignment is resolved.

- outside:

  How cells lying entirely outside the polygon geography should be
  handled. `"unassigned"` retains them without polygon ownership.
  `"nearest"` assigns them to the nearest polygon where ownership can be
  determined uniquely.

## Value

A tibble containing one row per unique H3 cell with `h3`, `polygon_id`,
`assignment_method`, `assignment_distance`, and `candidate_n`.
Attributes requested through `keep` are appended as ordinary columns.

## Details

Assignment methods are:

- `"centre"`: the H3 centre lies within one polygon.

- `"intersects"`: the centre lies outside, but the H3 cell intersects
  exactly one polygon.

- `"largest_overlap"`: the H3 cell intersects multiple polygons and one
  polygon has a unique greatest overlap.

- `"nearest"`: the H3 cell lies outside all polygons and is assigned to
  the nearest polygon.

- `"ambiguous"`: polygon ownership cannot be determined uniquely.

- `"unassigned"`: the cell lies outside the polygon geography and
  `outside = "unassigned"`.

Polygon features may share boundaries but must not have positive-area
interior overlap.

`outside = "nearest"` produces a complete assignment surface except
where nearest ownership is genuinely ambiguous. It does not impose an
arbitrary maximum search distance.

## References

Spatial vector and geometric operations use `sf`; see Pebesma E (2018),
[doi:10.32614/RJ-2018-009](https://doi.org/10.32614/RJ-2018-009) . H3
geometry access is provided through `h3jsr`.

## Examples

``` r
cells <- h3_cover_polygon(toy_membership_polygons, resolution = 7)
assignment <- h3_assign_polygon(
  cells,
  toy_membership_polygons,
  id = "feature_id"
)
head(assignment)
#> # A tibble: 6 × 5
#>   h3              polygon_id assignment_method assignment_distance candidate_n
#>   <chr>           <chr>      <chr>                           <dbl>       <int>
#> 1 87be63190ffffff 100        centre                              0           1
#> 2 87be63191ffffff 100        centre                              0           1
#> 3 87be63192ffffff 100        centre                              0           1
#> 4 87be63193ffffff 100        centre                              0           1
#> 5 87be63196ffffff 100        centre                              0           1
#> 6 87be63404ffffff 400        centre                              0           1
```
