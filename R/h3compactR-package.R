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
#' @import sf
#' @keywords internal
"_PACKAGE"