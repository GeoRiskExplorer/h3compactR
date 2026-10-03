# Changelog

## h3compactR 1.0.0

### Initial release

First public release of `h3compactR`.

#### Core workflow

- Represent polygon geography using explicit H3 coverage rules.
- Expand single-resolution H3 grids using neighbourhood rings.
- Compact H3 grids while optionally controlling the minimum retained
  resolution.
- Maintain explicit source-to-compact H3 relationships.
- Aggregate additive attributes and retain categorical provenance.
- Validate compaction, reconstruction, hierarchy and ownership
  integrity.
- Support polygon membership and exclusive polygon assignment.
- Provide lightweight cartographic QA for single- and mixed-resolution
  H3 representations.

#### Analytical principles

- The single-resolution source H3 grid remains the authoritative
  analytical support.
- Compaction changes hierarchical representation rather than creating
  new observations.
- Attribute aggregation is explicit rather than inferred.
- Additive measures are conserved through source-to-compact
  relationships.
- Derived measures should be recalculated from appropriate aggregated
  components.
- H3 hierarchy is distinguished from exact polygonal geographic
  containment.

#### Quality assurance

Version 1.0.0 includes automated tests covering coverage, expansion,
compaction, lookup construction, aggregation, polygon relationships,
assignment, plotting, adversarial geometry and package behaviour.

The complete analytical workflow has also been validated through
end-to-end source-cell, attribute and provenance reconciliation.

#### Documentation

- Complete function reference documentation and runnable examples.
- End-to-end workflow vignette.
- Package-level analytical guidance.
- Explicit attribution and references for H3, `h3jsr`, `sf`, and
  relevant cartographic literature.

#### Known limitations

Geometry-intensive coverage methods such as `within`, `intersects`, and
`overlap` can become computationally expensive for very large geographic
areas at fine H3 resolutions. Performance optimisation for large
geometry-heavy workflows is planned for future development.
