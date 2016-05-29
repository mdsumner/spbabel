
setOldClass( c("tbl_df", "tbl", "data.frame" ) )
setOldClass( c("grouped_df", "tbl_df", "tbl", "data.frame" ) )



#' Convert from dplyr tbl form to Spatial*DataFrame.
#'
#' @param x data_frame as created by \code{\link{sptable}}
#' @param crs projection, defaults to \code{NA_character_}
#' @param attr_tab remaining data from the attributes
#' @param ... unused
#' @return Spatial*
#' @export
#' @importFrom dplyr %>% distinct_ as_data_frame
#' @importFrom sp coordinates CRS SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines SpatialLinesDataFrame Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @examples 
#' semap1 <- semap  %>% dplyr::filter(y_ > -89.9999)
#' sp_obj <- sp(semap1, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")
#' ## look, seamless Antarctica!
#' ## library(rgdal); plot(spTransform(sp_obj, "+proj=laea +lat_0=-70"))
sp <- function(x, ...) {
 UseMethod("sp")
}

#' @rdname sp
#' @export
sp.data.frame <- function(x, attr_tab = NULL, crs, ...) {
  spFromTable(x, attr_tab = attr_tab, crs = crs, ...)
}



## spFromTable must deal with the following cases (so we don't need class-methods)
## fortified table (done)
## sp_df type, easy just grab the "Spatial_" column
## sportify type, unnest once
## nsp_df type, unnest twice (need name of column)
## db_df type, inner_join Object, Branch, Vertex and re-nest to sportify

#' @importFrom dplyr left_join
spFromTable <- function(x, attr_tab =  NULL, crs, ..., topol_ = NULL) {
  if (missing(crs)) crs <- attr(x, "crs")
  if (is.null(crs)) crs <- NA_character_
  ## raster::geom form
  if (is.null(topol_)) target <- detectSpClass(x)
  dat <- x %>% distinct_("object_")
  toremove <- setdiff(geomnames()[[target]], "object_")
  if (length(toremove) < (length(names(x)) -1)) {
    dat1 <- x
    for (i in seq_along(toremove)) dat1[[toremove[i]]] <- NULL
    dat <- dplyr::left_join(dat, dat1)
  }
   n_object <- length(unique(x$object_))
   n_attribute <- nrow(attr_tab)
   if (is.null(n_attribute)) n_attribute <- n_object
  if (!(n_attribute == n_object)) stop("number of rows in attr must match distinct object in x") 
  if (!is.null(attr_tab)) dat <- bind_cols(dat, attr_tab)
  dat <- as.data.frame(dat)
  gom <- switch(target,
                SpatialPolygonsDataFrame = reverse_geomPoly(x, dat, crs),
                SpatialLinesDataFrame = reverse_geomLine(x, dat, crs),
                SpatialPointsDataFrame = reverse_geomPoint(x, dat, crs)
  )
  gom
}



## convert this to sptable type (Spatial_, Object)
geomnames <- function() {
  list(SpatialPolygonsDataFrame = c("object_",  "branch_", "island_", "order_", "x_", "y_"),
       SpatialLinesDataFrame = c("object_",  "branch_", "order_", "x_", "y_"),
       SpatialPointsDataFrame = c("object_", "x_", "y_"), 
       ## strictly why not allow ordering on Multipoints, but why use sp for them anyway . . .
       SpatialMultPointsDataFrame = c("branch_", "object_", "x_", "y_"))
}


reverse_geomPoly <- function(x, d, proj) {
  objects <- split(x, x$object_)
  ## remove those columns used by reconstruction?
  d$branch_ <- d$object_ <- d$island_ <- d$order_ <- d$x_ <- d$y_ <- NULL
  if (ncol(d) < 1L) d$id_ <- seq(nrow(d))  ## we might end up with no attributes
  ## match.ID should be replaced by method to carry the original rownames somehow
  SpatialPolygonsDataFrame(SpatialPolygons(lapply(objects, loopBranchPoly), proj4string = CRS(proj)), d, match.ID = FALSE)
}
loopBranchPoly <- function(a) Polygons(lapply(split(a, a$branch_), function(b) Polygon(as.matrix(b[, c("x_", "y_")]), hole = !b$island_[1L] == 1)), as.character(a$object_[1L]))


reverse_geomLine <- function(x, d, proj) {
  objects <- split(x, x$object_)
  d$branch_ <- d$object_ <- d$order_ <- d$x_ <- d$y_ <- NULL
  if (ncol(d) < 1L) d$id_ <- seq(nrow(d))  ## we might end up with no attributes
  SpatialLinesDataFrame(SpatialLines(lapply(objects, loopBranchLine), proj4string = CRS(proj)), d)
}
loopBranchLine<- function(a) Lines(lapply(split(a, a$branch_), function(b) Polygon(as.matrix(b[, c("x_", "y_")]))), as.character(a$object_[1L]))

reverse_geomPoint <- function(a, d, proj) {
  # stop("not implemented")
  ## the decomposition is not yet applied for Multipoints . . .
  ## if (length(unique(a$object)) > 1) warning("no support for Multipoints yet")
  SpatialPointsDataFrame(SpatialPoints(as.matrix(a[, c("x_", "y_")])), d, proj4string = CRS(proj))
}
#' @importFrom sp SpatialMultiPointsDataFrame SpatialMultiPoints
reverse_geomMultPoint <- function(a, d, proj) {

  SpatialMultiPointsDataFrame(SpatialMultiPoints(lapply(split(a[, c("x_", "y_")], a$branch_), as.matrix)), d, proj4string = CRS(proj))
}
detectSpClass <- function(x) {
  #if ("topol_" %in% names(x)) return(topol2sp(x$topol_))
  gn <- geomnames()
  for (i in seq_along(gn)) {
    if (all(gn[[i]] %in% names(x))) return(names(gn)[i])
  }
  
  #if (all(gn$SpatialPolygonsDataFrame %in% names(x))) return("SpatialPolygonsDataFrame")
  #if (all(gn$SpatialLinesDataFrame %in% names(x))) return("SpatialLinesDataFrame")
  #if (all(gn$SpatialPointsDataFrame %in% names(x))) return("SpatialPointsDataFrame")
  #if (all(gn$SpatialMultiPointsDataFrame %in% names(x))) return("SpatialMultiPointsDataFrame")
  cat("cannot find matching topology type from these columns")
  print(x)
  stop('cannot create Spatial* object from this input')
  
}