# h3compactR

`h3compactR` provides a small, explicit workflow for polygon coverage, H3 compaction, source-to-compacted relationships, attribute aggregation, quality assurance and cartographic representation.

The package **does not implement or claim a new H3 compaction algorithm**. It builds an application and QA layer around established H3 operations exposed to R through `h3jsr`.

## Core workflow

```text
POLYGON
  |
  +-- h3_cover_polygon() ----> single-resolution source H3 grid
                                |
                                +-- expand_h3() [optional]
                                |
                                +-- compact_h3()
                                      |
                                      +-- h3_compaction_lookup()
                                      |      |
                                      |      +-- aggregate_h3()
                                      |
                                      +-- qa_h3_compaction()
                                      |
                                      +-- plot_h3()
```

`h3_polygon_membership()` and `h3_assign_polygon()` provide companion tools when H3 cells need to retain a documented relationship with source polygon features.

## Analytical position

The source, single-resolution H3 grid remains the **authoritative analytical support**. Compaction changes how that support is represented hierarchically; it does not create new observations or redistribute source values. Each source H3 cell should contribute its additive values exactly once to its compact owner. Derived rates and proportions should be recalculated from their component measures rather than averaged by default.

This distinction matters because H3 provides exact **logical** hierarchy while geographic containment between resolutions is approximate. Compact parent geometry should therefore not be interpreted as an exact polygonal union of descendant cells.

## Attribution

`h3compactR` depends on and is informed by substantial prior work:

- **H3** provides the hierarchical geospatial indexing system and the underlying hierarchy, neighbourhood, compaction and uncompaction concepts used by this package. H3 is developed by Uber Technologies and released under the Apache 2.0 license.
- **h3jsr**, developed by Lauren O'Brien, provides R access to H3 through `h3-js` and V8. `h3compactR` uses `h3jsr` rather than reimplementing H3.
- **sf**, led by Edzer Pebesma and contributors, provides the simple-features representation and spatial geometry operations used throughout the package.
- **Boscoe & Pickle (2003)** informs the broader cartographic framing that geographic-unit choice involves trade-offs among resolution, stability, area, familiarity, data availability and functional relevance. It is background to the package's cartographic thinking, not the source of H3 compaction or an algorithm implemented here.

See the package-level help (`?h3compactR`) for formal references.

## Scope and limitations

- H3 indexing is based on geographic coordinates; polygon inputs are transformed as required for H3 operations.
- Geometry- and area-based operations may require an appropriate projected CRS internally.
- Mixed-resolution compact representations are hierarchical representations, not replacements for the authoritative source support.
- Polygon membership and assignment require valid, non-overlapping polygon interiors. The package does not silently repair, snap, dissolve or otherwise alter invalid source geography.
- Aggregation rules are explicit. Additive measures may be summed; categorical/provenance fields may be collapsed. The package does not guess how arbitrary attributes should be aggregated.
- Computational cost depends strongly on H3 resolution, polygon complexity and operation. No universal safe maximum cell count is claimed.

## Development status

The package is currently under development. Its analytical core is covered by an automated test suite, but public API and documentation should be treated as pre-release until a stable version is tagged.

## References

Boscoe, F. P., & Pickle, L. W. (2003). Choosing geographic units for choropleth rate maps, with an emphasis on public health applications. *Cartography and Geographic Information Science*, 30(3), 237–248. https://doi.org/10.1559/152304003100011171

O'Brien, L. *h3jsr: Access Uber's H3 Library*. R package. https://doi.org/10.32614/CRAN.package.h3jsr

Pebesma, E. (2018). Simple Features for R: Standardized Support for Spatial Vector Data. *The R Journal*, 10(1), 439–446. https://doi.org/10.32614/RJ-2018-009

Uber Technologies, Inc. *H3: Hexagonal Hierarchical Geospatial Indexing System*. https://h3geo.org/
