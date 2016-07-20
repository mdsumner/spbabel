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


#' Cascading subset on object for mtable. 
#'
#' @param x 
#' @param ... 
#'
#' @noRd
semi_cascade <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
  first <- dplyr::filter(x[[tables[1]]], ...)
  x[[1]] <- last <- first 
  tables <- tables[-1]
  for (itab in tables) {
    x[[itab]] <- last <- dplyr::semi_join(x[[itab]], last)
  }
  x
}

inner_cascade <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
  first <- dplyr::filter(x[[tables[1]]], ...)
  #x[[1]] <- last <- first 
  tables <- tables[-1]
  for (itab in tables) {
    first <-  dplyr::inner_join(x[[itab]], first)
  }
  first
}


#' Multiple tidy tables
#' 
#' Creates a set of tidy tables from input objects. 
#'
#' 
#' @param x object to tidy
#' @param ... arguments passed to methods
#'
#' @return list of tibbles
#' @noRd
#' @examples
#' data(holey)
#' spholey <- sp(holey)
#' mtable(spholey)
mtable <- function(x, ...) {
  UseMethod("mtable")
}

#' @noRd
#' @importFrom tibble as_tibble
mtable.Spatial <- function(x, ...) {
  tabmap <- spbabel::sptable(x)
  tabdat <- tibble::as_tibble(x)
  
  ## remove this if sptable is updated
  tabdat$object_ <- id_n(nrow(tabdat))
  tabmap$object_ <- tabdat$object_[tabmap$object_]
  
  tabmap$branch_ <- id_n(length(unique(tabmap$branch_)))[factor(tabmap$branch_)]
 
  mtableFrom2(tabdat, tabmap)
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
#' @noRd
mtableFrom2 <- function(dat1, map1) {
  ## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
  v_atts <- c("x_", "y_")
  o_atts <- setdiff(names(map1), v_atts)
  b_atts <- setdiff(o_atts, c("order_", "vertex_"))
  bxv_atts <- c(setdiff(names(map1), c("object_", "island_", v_atts)), "vertex_")
 
  ## classify unique vertices by unique index
  ## could tidy this up some more . . .
  map1 <- map1 %>%
    mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts))))) %>% 
    mutate(vertex_ = spbabel:::id_n(length(unique(vertex_)))[vertex_])
  
  branchV_to_segmentV <- function(x) {
    head(matrix(x, ncol = 2, nrow = length(x) + 1L), -1L)
  }
  
  #map1$vertex_ <- id_nrow(nrow(map1))[map1$vertex_]
  ## branches, owner object and island status
  b <- map1 %>% distinct_(.dots = b_atts) 
  ## four tables (dat1, map2, map4, map5)
  bXv <- map1 %>% dplyr::select_(.dots = bxv_atts)
  v <- map1 %>% distinct_(.dots = c(v_atts, "vertex_"))
  res <- list(o = dat1, b = b,  bXv = bXv, v = v)
  res
}


mtableFrom2_old <- function(dat1, map1) {
  ## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
  v_atts <- c("x_", "y_")
  o_atts <- setdiff(names(map1), v_atts)
  b_atts <- setdiff(o_atts, c("order_", "vertex_"))
  bxv_atts <- c(setdiff(names(map1), c("object_", "island_", v_atts)), "vertex_")
  ## classify unique vertices by unique index
  ## could tidy this up some more . . .
  map1 <- map1 %>%
    mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts))))) %>% 
    mutate(vertex_ = id_n(length(unique(vertex_)))[vertex_])
  
  #map1$vertex_ <- id_nrow(nrow(map1))[map1$vertex_]
  ## branches, owner object and island status
  b <- map1 %>% distinct_(.dots = b_atts) 
  ## four tables (dat1, map2, map4, map5)
  bXv <- map1 %>% dplyr::select_(.dots = bxv_atts)
  v <- map1 %>% distinct_(.dots = c(v_atts, "vertex_"))
  res <- list(o = dat1, b = b,  bXv = bXv, v = v)
  res
}
