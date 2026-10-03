#' h3compactR: Polygon Coverage, Compaction and Aggregation for H3 Spatial Data
#'
#' `h3compactR` provides a small workflow for representing polygons with H3
#' cells, compacting those cells using established H3 methods, maintaining
#' relationships between source and compacted cells, aggregating attributes,
#' and validating the resulting spatial representation.
#'
#' The package builds on the H3 ecosystem and the `h3jsr` R package.
#' `h3compactR` does not implement or claim a new H3 compaction algorithm.
#' Instead, it provides an application layer around polygon coverage,
#' hierarchical relationships, attribute aggregation, cartographic
#' representation, and quality assurance.
#'
#' @section Package scope:
#' The core workflow is:
#'
#' 1. represent polygons using a single-resolution H3 grid;
#' 2. compact that grid using established H3 compaction;
#' 3. relate source cells to retained mixed-resolution cells;
#' 4. aggregate attributes using explicit aggregation rules; and
#' 5. validate coverage, hierarchy and aggregation integrity.
#'
#' @section H3 implementation:
#' H3 indexing and hierarchical operations are provided through `h3jsr`.
#' Users requiring direct access to the broader H3 API should use `h3jsr`
#' directly.
#'
#' @section Analytical principles:
#' The source, single-resolution H3 grid is treated as the authoritative
#' analytical support. Compaction changes the hierarchical representation of
#' that support; it does not create new source observations or imply that a
#' coarser H3 cell is an exact geometric union of its descendants. Additive
#' attributes should therefore be aggregated through an explicit source-to-
#' compacted lookup, while derived measures should be recalculated from their
#' underlying components rather than averaged by default.
#'
#' H3 documents its hierarchy as exact in the index while geographic
#' containment between resolutions is approximate. Applications requiring
#' exact polygon boundaries should therefore retain the distinction between
#' H3 hierarchy and polygon geometry.
#'
#' @section Attribution and background:
#' H3 indexing, hierarchy, neighbourhood and compaction operations used by
#' this package originate in the H3 project. R access to H3 is provided by
#' Lauren O'Brien's `h3jsr` package. Spatial vector representation and geometric
#' operations are provided by `sf` and its underlying spatial libraries.
#'
#' Boscoe and Pickle (2003) is included as cartographic background for the
#' broader principle that selection of mapping units involves trade-offs among
#' resolution, stability, area, familiarity, data availability and functional
#' relevance. It is not presented as the source of H3 compaction or of the
#' algorithms implemented by `h3compactR`.
#'
#' @references
#' Uber Technologies, Inc. H3: Hexagonal hierarchical geospatial indexing
#' system. <https://h3geo.org/>
#'
#' O'Brien L. `h3jsr`: Access Uber's H3 Library. R package.
#' \doi{10.32614/CRAN.package.h3jsr}
#'
#' Pebesma E (2018). Simple Features for R: Standardized Support for Spatial
#' Vector Data. *The R Journal*, 10(1), 439-446.
#' \doi{10.32614/RJ-2018-009}
#'
#' Boscoe FP, Pickle LW (2003). Choosing Geographic Units for Choropleth Rate
#' Maps, with an Emphasis on Public Health Applications. *Cartography and
#' Geographic Information Science*, 30(3), 237-248.
#' \doi{10.1559/152304003100011171}
#'
#' @import sf
#' @keywords internal
"_PACKAGE"