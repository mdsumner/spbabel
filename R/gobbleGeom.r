# .coordsIJ <- function(x, i, j, type) {
#   switch(type, 
#          line = x@lines[[i]]@Lines[[j]]@coords, 
#          poly =  x@polygons[[i]]@Polygons[[j]]@coords)
# }
# 
# .nsubobs <- function(x, i, type) {
#   length(
#     switch(type, 
#            line = x@lines[[i]]@Lines, 
#            poly = x@polygons[[i]]@Polygons)
#   )
# }
# 
# .island <- function(x, i, j, type) {
#   switch(type, 
#          line = NULL, 
#          ## negate here since it will be NULL outside for lines
#          poly = !x@polygons[[i]]@Polygons[[j]]@hole
#   )
# }
# ## adapted from raster package R/geom.R
# ## generalized on Polygon and Line
# #' @importFrom sp geometry
# #' @importFrom dplyr bind_rows
# #' @importFrom tibble tibble
# .gobbleGeom <-   function(x,  ...) {
#   gx <- geometry(x)
#   typ <- switch(class(gx), 
#                 SpatialPolygons = "poly", 
#                 SpatialLines = "line")
#   nobs <- length(geometry(x))
#   objlist <- vector("list", nobs)
#   cnt <- 0L
#   for (i in seq(nobs)) {
#     nsubobs <- .nsubobs(x, i, typ) 
#     ps <- lapply(1:nsubobs,
#                  function(j) {
#                    coords <- .coordsIJ(x, i, j, typ)
#                    nr <- nrow(coords)
#                    lst <- list(
#                      branch_ = rep(j + cnt, nr), 
#                      island_ = rep(.island(x, i, j, typ), nr), 
#                      order_ = seq(nr),
#                      x_ = coords[,1], 
#                      y_ = coords[,2])
#                    lst[!unlist(lapply(lst, is.null))]
#                  }
#     )
#     psd <- bind_rows(ps)
#     objlist[[i]] <- bind_cols(tibble(object_ = rep(i, nrow(psd))), psd)
#     cnt <- cnt + nsubobs
#   }
#   obs <- bind_rows(objlist) ## do.call(bind_rows, objlist)
#   
#   rownames(obs) <- NULL
#   
#   attr(obs, "crs") <- proj4string(x)
#   return( obs )
# }
# 
