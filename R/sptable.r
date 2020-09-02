#' Convert from various forms to a table.
#'
#' Decompose a \code{\link[sp]{Spatial}} or sf object to a single table structured as a row for every coordinate in all the sub-geometries, including duplicated coordinates that close polygonal rings, close lines and shared vertices between objects.
#'
#' Input can be a of type sf or  \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}}, \code{\link[sp]{SpatialMultiPointsDataFrame}} or a \code{\link[sp]{SpatialPointsDataFrame}}.
#' For simplicity \code{sptable} and its inverses \code{sp} and \code{sf} assume that all geometry can be encoded with object, branch, island, order, x and y.
#' and that the type of topology is identified by which of these are present.
#'
#' For simple features objects with mixed types of topology the result is consistent, but probably not useful.
#' Columns that aren't present in one type will be present, padded with NA. (This is work in progress).
#'
#'
#' @param x \code{\link[sp]{Spatial}} object
#' @param ... ignored
#' @return \code{\link[tibble]{tibble}}  with columns
#' \itemize{
#'  \item SpatialPolygonsDataFrame "object_"   "branch_"   "island_"   "order_" "x"    "y_"
#'  \item SpatialLinesDataFrame "object_"   "branch_" "order_"  "x_"      "y_"
#'  \item SpatialPointsDataFrame  "object_" x_"      "y_"
#'  \item SpatialMultiPointsDataFrame "object_" "branch_" "x_" "y_"
#'  \item sf some combination of the above
#' }
#' @export
#' @importFrom dplyr mutate
#' @examples
#' ## holey is a decomposed SpatialPolygonsDataFrame
#' spdata <- sp(holey)
#' library(sp)
#' plot(spdata, col = rainbow(nrow(spdata), alpha = 0.4))
#' points(holey$x_, holey$y_, cex = 4)
#' holes <- subset(holey, !island_)
#' ## add the points that only belong to holes
#' points(holes$x_, holes$y_, pch = "+", cex = 2)
#'
#' ## manipulate based on topology
#' ## convert to not-holes
#' notahole <- holes
#' notahole$island_ <- TRUE
#' #also convert to singular objects - note that this now means we have an overlapping pair of polys
#' #because the door had a hole filled by another object
#' notahole$object_ <- notahole$branch_
#' plot(sp(notahole), add = TRUE, col = "red")
#'
sptable <- function(x, ...) {
  UseMethod("sptable")
}


