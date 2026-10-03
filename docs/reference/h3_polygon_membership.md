# Relate H3 cells to polygon features

Creates an explicit spatial relationship between H3 cells and a
topologically sound polygon geography.

## Usage

``` r
h3_polygon_membership(x, polygons, id)
```

## Arguments

- x:

  A character vector of valid H3 cell indexes.

- polygons:

  An `sf` object containing polygon or multipolygon features.

- id:

  Name of the polygon identifier column.

## Value

A tibble containing `h3` and `polygon_id`. A H3 cell may appear more
than once where it intersects more than one valid polygon feature.

## Details

Polygon features may share boundaries but must not have positive-area
overlap. Overlapping polygon interiors are rejected because they do not
provide a suitable basis for unambiguous polygon membership.

This function records spatial relationships only. It does not assign
exclusive ownership, allocate polygon values, aggregate attributes, or
alter H3 geometry.

## References

Spatial vector and geometric operations use `sf`; see Pebesma E (2018),
[doi:10.32614/RJ-2018-009](https://doi.org/10.32614/RJ-2018-009) . H3
geometry access is provided through `h3jsr`.

## Examples

``` r
cells <- h3_cover_polygon(toy_membership_polygons, resolution = 7)
membership <- h3_polygon_membership(
  cells,
  toy_membership_polygons,
  id = "feature_id"
)
head(membership)
#> # A tibble: 6 × 2
#>   h3              polygon_id
#>   <chr>           <chr>     
#> 1 87be63190ffffff 100       
#> 2 87be63191ffffff 100       
#> 3 87be63192ffffff 100       
#> 4 87be63193ffffff 100       
#> 5 87be63196ffffff 100       
#> 6 87be63404ffffff 400       
```
