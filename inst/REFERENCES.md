# References and attribution

This file records external projects and literature directly relevant to `h3compactR`. Inclusion does not imply that those sources endorse this package.

## Underlying H3 system

Uber Technologies, Inc. **H3: Hexagonal Hierarchical Geospatial Indexing System.** https://h3geo.org/

`h3compactR` relies on H3 concepts and operations including hierarchical indexing, parent/child relationships, grid neighbourhoods, compaction and uncompaction. The package does not claim authorship of those algorithms.

## R interface to H3

O'Brien, Lauren. **h3jsr: Access Uber's H3 Library.** R package. DOI: 10.32614/CRAN.package.h3jsr. https://obrl-soil.github.io/h3jsr/

`h3compactR` uses `h3jsr` for H3 operations rather than reimplementing the H3 library.

## Spatial vector infrastructure

Pebesma, Edzer (2018). **Simple Features for R: Standardized Support for Spatial Vector Data.** *The R Journal*, 10(1), 439–446. DOI: 10.32614/RJ-2018-009.

The `sf` package provides spatial vector representation and geometric operations used by `h3compactR`.

## Cartographic background

Boscoe, Francis P., and Linda W. Pickle (2003). **Choosing Geographic Units for Choropleth Rate Maps, with an Emphasis on Public Health Applications.** *Cartography and Geographic Information Science*, 30(3), 237–248. DOI: 10.1559/152304003100011171.

This paper is cited for its discussion of trade-offs in selecting geographic units for mapping. It is not cited as the source of H3 compaction or of an `h3compactR` algorithm.
