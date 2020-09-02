#' Convert between different types of spatial objects.
#'
#' @description Facilities for converting between different types of spatial objects, including an in-place method to modify the underlying geometry of 'Spatial' classes using data frame idioms.The spbabel package provides functions to round-trip a Spatial object to a single table and back.
#'
#' @details
#'
#'  \tabular{ll}{
#'  \code{\link{sptable<-}} \tab modify a Spatial object in-place  \cr
#'  \code{\link{sptable}} \tab create a \code{\link[tibble]{tibble}} from Spatial DataFrame object  \cr
#'  \code{\link{sp}} \tab create Spatial DataFrame object from table \cr
#'  }
#' @name spbabel-package
#' @docType package
NULL

#' MultiPointsDataFrame data set
#'
#' @name mpoint1
#' @docType data
#' @rdname mpoint1
NULL

#' "South-east" map data.
#'
#' Created in /data-raw/ \code{semap} is the \code{sptable} version of some of maptools 'wrld_simpl' and \code{seatt} is the matching attribute data, linked by 'object_'.
#' @name semap
#' @docType data
#' @examples
#' # recreate as sp object
#' mp <- sp(semap, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")
#'
NULL


#' "South-east" map attribute data.
#'
#' Created in /data-raw/.
#' @name seatt
#' @docType data
#' @rdname semap
NULL

#' Multi-part, multi-holed, neighbouring, not completely topological polygons.
#'
#' Created in /data-raw/ from a manual drawing built in Manifold GIS.
#' @name holey
#' @docType data
#' @rdname holey
NULL

#' Multi-object track with x, y, z, and time.
#'
#' Created in /data-raw/track.r
#' @name track
#' @docType data
#' @rdname track
NULL
