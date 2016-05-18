#' A basis for converting between different types of spatial objects.
#'
#'# @importFrom utils 
#' @aliases NULL
#' @details The spbabel package provides functions to round-trip a Spatial object to a single table and back.
#' 
#
#' @section I. sptable: an round-trip extension to fortify for Spatial 
#' 
#' @section II. Conversion:
#'  \tabular{ll}{
#'  \code{\link{sptable}} \tab create a \code{\link[dplyr]{tbl_df}} from Spatial*DataFrame  \cr
#'  \code{\link{sp}} \tab create Spatial object from table \cr
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
#' Created in /data-raw/ \code{semap} is the setable() version of some of maptools wrld_simpl, and \code{seatt} is the matching attribute data, linked by `object`. 
#' @name semap
#' @docType data
#' @examples 
#' # recreate as sp object
#' mp <- sp(semap, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")
#' 
NULL


#' "South-east" map attribute data. 
#' 
#' Created in /data-raw/
#' @name seatt
#' @docType data
#' @rdname semap
NULL

