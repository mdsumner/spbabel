line_mesh_map_table1 <- function(tabs) {
  tabs$v$countingIndex <- seq(nrow(tabs$v))
  nonuq <- dplyr::inner_join(tabs$bXv, tabs$v, "vertex_")
  
  ps <- RTriangle::pslg(P = as.matrix(tabs$v[, c("x_", "y_")]),
                        S = do.call(rbind, lapply(split(nonuq, nonuq$branch_),
                                                  function(x) path2seg(x$countingIndex))))
  
  tabs$v <- tibble::tibble(x_ = ps$P[,1], y_ = ps$P[,2], vertex_ = spbabel:::id_n(nrow(ps$P)))
  tabs$b <- tabs$bXv <- NULL
  tabs$l <- tibble::tibble(segment_ = spbabel:::id_n(nrow(ps$S)), object_ = tabs$o$object_[1])
  tabs$lXv <- tibble::tibble(segment_ = rep(tabs$l$segment_, each = 2), 
                             vertex_ = tabs$v$vertex_[as.vector(t(ps$S))])
  
  tabs
}


#' @importFrom utils head
path2seg <- function(x) {
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, byrow = FALSE)), -2L)
}


#' Spatial primitives
#' 
#' Spatial lines and spatial polygons are composed of 1D primitives (line segments)
#' Spatial points and spatial multipoints are composed of 0D primtives (coordinates)
#' Spatial 2D primitives can be generated from 1D primitives either 
#' - directly (not implemented)
#' - by triangulation (using ear-clipping or constrained Delaunay)
#' and from 0D primitives by
#' - triangulation
#' @param x Spatial object
#' @export
#' @param ... other args
primitives <- function(x, ...) {
  UseMethod("primitives")
}

#' @rdname primitives
#' @export
primitives.Spatial <- function(x, ...) {
  pr4 <- proj4string(x)

  ## function to always return DataFrame
  #x <- alwaysDataFrame(x)
  
  tabs <- spbabel::map_table(x)
  ll <- vector("list", nrow(tabs$o))
  for (i_obj in seq(nrow(tabs$o))) {
    tabs_i <- tabs; tabs_i$o <- tabs_i$o[i_obj, ]
    tabs_i <- semi_cascade(tabs_i)
    tt_i <- line_mesh_map_table1(tabs_i)
    # plot.trimesh(tt_i)
    # scan("", 1L)
    # rgl::rgl.clear()
    ll[[i_obj]] <- tt_i
  }
  
  outlist <- vector("list", length(ll[[1]]))
  nms <- names(ll[[1]])
  names(outlist) <- nms
  for (i in seq_along(outlist)) {
    outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
  }
  
  ## renormalize the vertices
  allverts <- dplyr::inner_join(outlist$lXv, outlist$v, "vertex_")
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, sep = "_")))
  allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  outlist$lXv <- allverts[, c("segment_", "vertex_")]
  outlist$v <- dplyr::distinct_(allverts, "x_", "y_", "vertex_")
  ## finally add longitude and latitude
  outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_")
  class(outlist) <- "linemesh"
  outlist
}



semi_join_be_quiet_if_there_is_only_1 <- function(x, y, by = NULL, copy = FALSE, ...) {
  comm <- intersect(names(x), names(y))
  if (length(comm) == 1L) {
    by <- comm
  }
  semi_join(x, y, by = by, copy = copy, ...)
}


#' Cascading subset on object for \code{map_table}. 
#'
#' @param x 
#' @param ... 
#' @importFrom dplyr semi_join
#' @noRd
semi_cascade <- function(x, ..., tables = c("o", "b", "bXv", "v")) {
  first <- dplyr::filter(x[[tables[1]]], ...)
  x[[1]] <- last <- first 
  tables <- tables[-1]
  for (itab in tables) {
    x[[itab]] <- last <- semi_join_be_quiet_if_there_is_only_1(x[[itab]], last)
  }
  x
}