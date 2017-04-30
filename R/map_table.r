
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
#'  These are currently classed as object_table, branch_table, branch_link_vertex_table, and vertex_table. But there are no methods. 
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



#' Convert two linked tables to four. 
#' 
#' sptable to gris
#'
#' @param dat1 object "meta"data
#' @param map1 geometry data in sptable form
#' @param v_atts the vertex attributes to de-duplicate by (x_, y_ by default)
#' @importFrom dplyr %>% distinct_  select_
#' @importFrom tibble tibble
#' @importFrom utils head
#' @noRd
map_table_From2 <- function(dat1, map1,   v_atts = c("x_", "y_")) {
  ## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
  
  o_atts <- setdiff(names(map1), v_atts)
  b_atts <- setdiff(o_atts, c("order_", "vertex_"))
  bxv_atts <- c(setdiff(names(map1), c("object_", "island_", v_atts)), "vertex_")
  pasty <- dplyr::select_(map1, .dots = v_atts)
  
  ver_ <- as.integer(factor(do.call(paste, pasty)))
  map1[["vertex_"]] <- id_n(length(unique(ver_)))[ver_]
  b <- dplyr::distinct_(map1, .dots = b_atts) 
  ## four tables (dat1, map2, map4, map5)
  
  bXv <- map1[, intersect(names(map1), c("branch_", "vertex_", "order_"))]
  #print(head(map1))
  v <- map1[!duplicated(map1$vertex_), c(v_atts, "vertex_")]
  class(dat1) <- c("object_table", class(dat1))
  class(b) <- c("branch_table", class(b))
  class(bXv) <- c("branch_link_vertex_table", class(bXv))
  class(v) <- c("vertex_table", class(v))
  res <- structure(list(o = dat1, b = b,  bXv = bXv, v = v), class = c("map_table", "list"))
  res
}

#map_table.RasterLayer <- function(x, ...) {
## v is the corners
## b is the quad index
## o is the pixel discrete values
#}
