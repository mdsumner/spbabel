#' Convert from sp package 'Spatial' classes to a table.
#'
#' Decompose a \code{\link[sp]{Spatial}} object to a single table structured as a row for every coordinate in all the sub-geometries, including duplicated coordinates that close polygonal rings, close lines and shared vertices between objects. 
#' 
#' Input can be a \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}}, \code{\link[sp]{SpatialMultiPointsDataFrame}} or a \code{\link[sp]{SpatialPointsDataFrame}}.
#' For simplicity \code{sptable} and its inverse \code{sp} assume that all geometry can be encoded with object, branch, island, order, x and y. 
#' and that the type of topology is identified by which of these are present. 
#' 
#' This is analogous to the following but in spbabel provides a consistent way to round-trip back to Spatial classes and other forms. 
#' \itemize{
#'  \item \code{\link[broom]{sp_tidiers}} (replacement of 'ggplot2::fortify').
#'  \item \code{\link[raster]{geom}}
#'  \item \code{\link[sp]{SpatialPolygonsDataFrame-class}} with its 'as(as(x, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")' workflow. 
#' }
#' 
#' @param x \code{\link[sp]{Spatial}} object
#' @param ... ignored
#' @return \code{\link[tibble]{tibble}}  with columns
#' \itemize{
#'  \item SpatialPolygonsDataFrame "object_"   "branch_"   "island_"   "order_" "x"    "y_"
#'  \item SpatialLinesDataFrame "object_"   "branch_" "order_"  "x_"      "y_"
#'  \item SpatialPointsDataFrame  "object_" x_"      "y_"
#'  \item SpatialMultiPointsDataFrame "object_" "branch_" "x_" "y_"
#' }
#' @export
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
#' ## example using in-place modification with sptable<-
#' library(maptools)
#' data(wrld_simpl)
#' spdata2 <- spdata
#' library(dplyr)
#' ## modify the geometry on this object without separating the vertices from the objects
#' sptable(spdata2) <- sptable(spdata2) %>% mutate(x_ = x_ + 10, y_ = y_ + 5)
sptable <- function(x, ...) {
  UseMethod("sptable")
}

#' @export
#' @rdname sptable
sptable.SpatialPolygonsDataFrame <- function(x, ...) {
  .gobbleGeom(x, ...)
}

#' @export
#' @rdname sptable
sptable.SpatialLinesDataFrame <- function(x, ...) {
  mat2d_f(.gobbleGeom(x, ...))
}

#' @export
#' @rdname sptable
#' @importFrom dplyr bind_cols
sptable.SpatialPointsDataFrame <- function(x, ...) {
  #df <- mat2d_f(.pointsGeom(x, ...))
  df <- .pointsGeom(x, ...)
  df$object_ <- as.integer(df$object_) ## not needed once .pointsGeom uses tbl_df
  df
}

#' @export
#' @rdname sptable
#' @importFrom dplyr bind_cols
sptable.SpatialMultiPointsDataFrame <- function(x, ...) {
   #df <- mat2d_f(.pointsGeom(x))
  df <- .pointsGeom(x, ...)
   df$object_ <- as.integer(df$object_) 
   df$branch_ <- as.integer(df$branch_) 
   df
}
## TODO multipoints
#' @importFrom tibble as_tibble
mat2d_f <- function(x) {
  as_tibble(as.data.frame((x)))
}


#' @rdname sptable
#' @param object Spatial object
#' @param value modified sptable version of object
#'
#' @return Spatial object
#' @export
"sptable<-" <-
  function(object, value) {
    # joiner <- idmaker(20)
    #    datadata <- as.data.frame(object) 
    #    datadata[[joiner]] <- seq(nrow(datadata))
    #    value[[joiner]] <- as.integer(factor(value[["object_"]]))
    #    datadata <- inner_join(datadata, value[, joiner])
    if (nrow(object)  == length(unique(value$object_))) {
      ## assume ok
    } else {
      warning("dropping attribute data since object number and metadata rows not the same")
      return(sp(value,  crs = proj4string(object)))
      }
    sp(value,  as.data.frame(object), proj4string(object))

  }





.coordsIJ <- function(x, i, j, type) {
  switch(type, 
         line = x@lines[[i]]@Lines[[j]]@coords, 
         poly =  x@polygons[[i]]@Polygons[[j]]@coords)
}

.nsubobs <- function(x, i, type) {
  length(
    switch(type, 
         line = x@lines[[i]]@Lines, 
         poly = x@polygons[[i]]@Polygons)
)
}

.island <- function(x, i, j, type) {
  switch(type, 
         line = NULL, 
         ## negate here since it will be NULL outside for lines
         poly = !x@polygons[[i]]@Polygons[[j]]@hole
  )
}
## adapted from raster package R/geom.R
## generalized on Polygon and Line
#' @importFrom sp geometry
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
.gobbleGeom <-   function(x,  ...) {
  gx <- geometry(x)
  typ <- switch(class(gx), 
                SpatialPolygons = "poly", 
                SpatialLines = "line")
  nobs <- length(geometry(x))
  objlist <- vector("list", nobs)
  cnt <- 0L
  for (i in seq(nobs)) {
      nsubobs <- .nsubobs(x, i, typ) 
      ps <- lapply(1:nsubobs,
                   function(j) {
                     coords <- .coordsIJ(x, i, j, typ)
                     nr <- nrow(coords)
                     lst <- list(
                                 branch_ = rep(j + cnt, nr), 
                                 island_ = rep(.island(x, i, j, typ), nr), 
                                 order_ = seq(nr),
                                 x_ = coords[,1], 
                                 y_ = coords[,2])
                     lst[!unlist(lapply(lst, is.null))]
                   }
      )
      psd <- bind_rows(ps)
      objlist[[i]] <- bind_cols(tibble(object_ = rep(i, nrow(psd))), psd)
      cnt <- cnt + nsubobs
    }
  obs <- bind_rows(objlist) ## do.call(bind_rows, objlist)
  
  rownames(obs) <- NULL

  attr(obs, "crs") <- proj4string(x)
  return( obs )
}




.pointsGeom <-  function(x, ...) {
  ## this will have to become a tbl
  xy <- as_tibble(as.data.frame(coordinates(x)))
  cnames <- c('object_', 'x_', 'y_')
  ##xy <- cbind(1:nrow(xy), xy)
  if (is.list(x@coords)) {
    br <- rep(seq_along(x@coords), unlist(lapply(x@coords, nrow)))
    cnames <- c('branch_', 'object_', 'x_', 'y_')
    xy <- bind_cols(tibble(br), tibble(br), xy)
  } else {
    br <- seq(nrow(xy))
    xy <- bind_cols(tibble(br), xy)
  }
  
  colnames(xy) <- cnames
  return(xy)
}
