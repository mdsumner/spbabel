# #' Convert Spatial*DataFrame objects to gris
# #'
# #' All availables Spatial*DataFrame types are supported, and are interpreted via the Branch model. 
# #' The gris function can ingest \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}}, \code{\link[sp]{SpatialPointsDataFrame}}, and \code{\link[sp]{SpatialMultiPointsDataFrame}} objects. 
# #' 
# #' See  \code{vignette("branch-vs-primitives")}
# #' @param x Spatial* object
# #' @param ... not used
# #' @aliases gris.Spatial
# #' @return gris
# #' @export
# gris <- function(x, ...) {
#   UseMethod("gris")
# }
# 
# #' @export
# #' @rdname gris
# gris.Spatial <- function(x, ...) {
#   ## one method for all sp (need to dummify if doesn't have a dataframe)
#   x <- mtable(x, ...)
#   class(x) <- c("gris", "list")
#   x
# }





#' A decomposition of 'vector' map data structures to tables. 
#' 
#' Creates a set of related tables to store the appropriate
#' entities in spatial map data. 
#'
#' The basic entities behind spatial data, and hence the "map tables" are: 
#' \describe{
#'  \item{vertices}{the positions in geometric space, e.g. x, y, z, time, long, lat, salinity etc.}
#'  \item{branches}{a single connected chain of vertices, or "parts"}
#'  \item{objects}{a collection of branches aligned to a row of metadata}
#'  }
#'  
#'  This is the basic "topology" of traditional GIS vector data, for points, 
#'  lines, polygons and their multi-counterparts. By default map_tables will 
#'  produce these tables and also de-duplicated the input vertices, adding a 
#'  fourth table to link vertices to branches. 
#'  
#'  Other topology types such as triangle or quad meshes can extend this 
#'  four-entity model, or exist without the branches at all. See "mesh_table" ??
#'  
#'  
#' @param x object to tidy
#' @param ... arguments passed to methods
#'
#' @return list of tibbles
#' @export
#' @examples
#' data(holey)
#' spholey <- sp(holey)
#' map_table(spholey)
map_table <- function(x, ...) {
  UseMethod("map_table")
}



#' @export
#' @importFrom tibble as_tibble
map_table.Spatial <- function(x, ...) {
  
  # fugn incongruous points as ever
  as.data.frame.SpatialMultiPointsDataFrame <- function(x, ...) {
    x@data
  }
  ## I will regret these internal functions . . .
  ## needs a proper fix
  as.data.frame.SpatialPointsDataFrame <- function(x, ...) {
    x@data
  }
  
  
  tabmap <- sptable(x)
  ## why did this ever work?
#  tabdat <- tibble::as_tibble(x)
  tabdat <- tibble::as_tibble(as.data.frame(x)) 
  ## remove this if sptable is updated
  tabdat$object_ <- id_n(nrow(tabdat))
  tabmap$object_ <- tabdat$object_[tabmap$object_]
  if (class(x) == "SpatialPointsDataFrame") {
    tabmap$branch_ <- id_n(nrow(tabmap))
  } else {
  tabmap$branch_ <- id_n(length(unique(tabmap$branch_)))[factor(tabmap$branch_)]
  }
  out <- map_table_From2(tabdat, tabmap)

  # no class or methods in spbabel for map_table()
  #class(out) <- c("map_table", "list")
  out
}


#' Convert two linked tables to four. 
#' 
#' sptable to gris
#'
#' @param dat1 object "meta"data
#' @param map1 geometry data in sptable form
#'
#' @importFrom dplyr %>% bind_rows distinct_ mutate select select_
#' @importFrom tibble tibble
#' @importFrom utils head
#' @noRd
map_table_From2 <- function(dat1, map1) {
  ## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
  v_atts <- c("x_", "y_")
  o_atts <- setdiff(names(map1), v_atts)
  b_atts <- setdiff(o_atts, c("order_", "vertex_"))
  bxv_atts <- c(setdiff(names(map1), c("object_", "island_", v_atts)), "vertex_")
 
 ver_ <- as.integer(factor(do.call(paste, select_(map1, .dots = v_atts))))
  map1[["vertex_"]] <- id_n(length(unique(ver_)))[ver_]
   b <- distinct_(map1, .dots = b_atts) 
  ## four tables (dat1, map2, map4, map5)

    bXv <- dplyr::select_(map1, .dots = bxv_atts)
    #print(head(map1))
    v <- map1[!duplicated(map1$vertex_), c(v_atts, "vertex_")]
    #  v <- map1 %>% distinct_(.dots = c(v_atts, "vertex_"))
  class(dat1) <- c("object_table", class(dat1))
  class(b) <- c("branch_table", class(b))
  class(bXv) <- c("branch_link_vertex_table", class(bXv))
  class(v) <- c("vertex_table", class(v))
  res <- list(o = dat1, b = b,  bXv = bXv, v = v)
  res
}

#map_table.RasterLayer <- function(x, ...) {
  ## v is the corners
  ## b is the quad index
  ## o is the pixel discrete values
#}
