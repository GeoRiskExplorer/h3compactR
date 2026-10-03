# Package index

## Polygon coverage

Create and expand authoritative single-resolution H3 representations.

- [`h3_cover_polygon()`](https://georiskexplorer.github.io/h3compactR/reference/h3_cover_polygon.md)
  : Cover polygons with H3 cells
- [`expand_h3()`](https://georiskexplorer.github.io/h3compactR/reference/expand_h3.md)
  : Expand an H3 cell set by neighbouring cells

## Compaction and relationships

Compact H3 representations and retain explicit relationships with their
source cells.

- [`compact_h3()`](https://georiskexplorer.github.io/h3compactR/reference/compact_h3.md)
  : Compact H3 cells with an optional resolution floor
- [`h3_compaction_lookup()`](https://georiskexplorer.github.io/h3compactR/reference/h3_compaction_lookup.md)
  : Build a source-to-compacted H3 lookup

## Attribute aggregation

Transfer additive measures and categorical provenance from authoritative
source cells to compact owners.

- [`aggregate_h3()`](https://georiskexplorer.github.io/h3compactR/reference/aggregate_h3.md)
  : Aggregate attributes to compacted H3 cells

## Polygon relationships

Relate H3 cells to polygon features and establish exclusive polygon
ownership where required.

- [`h3_polygon_membership()`](https://georiskexplorer.github.io/h3compactR/reference/h3_polygon_membership.md)
  : Relate H3 cells to polygon features
- [`h3_assign_polygon()`](https://georiskexplorer.github.io/h3compactR/reference/h3_assign_polygon.md)
  : Assign H3 cells to polygon features

## Quality assurance and cartography

Validate compact representations and visually inspect H3 geography.

- [`qa_h3_compaction()`](https://georiskexplorer.github.io/h3compactR/reference/qa_h3_compaction.md)
  : Audit an H3 compaction
- [`plot_h3()`](https://georiskexplorer.github.io/h3compactR/reference/plot_h3.md)
  : Plot H3 cells

## Example data

- [`toy_polygons`](https://georiskexplorer.github.io/h3compactR/reference/toy_polygons.md)
  : Synthetic polygons for H3 workflow testing
- [`toy_membership_polygons`](https://georiskexplorer.github.io/h3compactR/reference/toy_membership_polygons.md)
  : Synthetic polygon geography for membership and assignment examples
