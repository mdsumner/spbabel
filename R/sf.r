
#' @export
#' @importFrom tibble as_tibble
map_table.sf <- function(x, ...) {
  
  tabmap <- sptable(x)
  ## why did this ever work?
  #  tabdat <- tibble::as_tibble(x)
  tabdat <- tibble::as_tibble(x) 
  ## remove this if sptable is updated
  tabdat$object_ <- id_n(nrow(tabdat))
  tabmap$object_ <- tabdat$object_[tabmap$object_]
  out <- map_table_From2(tabdat, tabmap)
  # no class or methods in spbabel for map_table()
  #class(out) <- c("map_table", "list")
  out
}




as.list.sfc <- function(x) {
  rapply(rapply(unclass(x), matrix2list, how = "replace"), unclass, how = "list")
}

polygonData.sf <- function(x) {
  matrix2list <- function(x) {
    split(x, rep(seq(ncol(x)), each = nrow(x)))
  }
  structure(rapply(rapply(unclass(x), matrix2list, how = "replace"), unclass, how = "list"), 
            bbox = matrix(attr(nc[[attr(nc, "sf_column")]], "bbox"), ncol = 2))
}
#' @export
#' @importFrom sf st_geometry
#' @importFrom tibble as_tibble
sptable.sf <- function(x) {
  g <- sf::st_geometry(x)
  
  gtab <- do.call(rbind,lapply(seq_along(g), 
         function(object_)  do.call(rbind, lapply(seq_along(g[[object_]]), 
                                    function(branch_) do.call(rbind, lapply(seq_along(g[[object_]][[branch_]]), 
                                                                               function(sub_index) cbind(g[[object_]][[branch_]][[sub_index]], object_, branch_, sub_index)))))))
  
  

  pmap_df <- function(x) {
    x <- as_tibble(x); 
    names(x)[1:2] <- c("x_", "y_")
    x[["branch_"]] <- cumsum(c(0, diff(x[["branch_"]]))) + x[["branch_"]] + x[["object_"]]
    x <- x %>% mutate(island_ = sub_index == 1, sub_index = NULL)
  }
  pmap_df(gtab)
}

#' @importFrom sf st_crs
rangl.sf <- function (x, max_area = NULL, ...) 
{
  pr4 <- sf::st_crs(x)$proj4string
 
  tabs <- spbabel::map_table(x)
  ll <- vector("list", nrow(tabs$o))
  for (i_obj in seq(nrow(tabs$o))) {
    tabs_i <- tabs
    tabs_i$o <- tabs_i$o[i_obj, ]
    tabs_i <- rangl:::semi_cascade(tabs_i)
    tt_i <- rangl:::tri_mesh_map_table1(tabs_i, max_area = max_area)
    ll[[i_obj]] <- tt_i
  }
  outlist <- vector("list", length(ll[[1]]))
  nms <- names(ll[[1]])
  names(outlist) <- nms
  for (i in seq_along(outlist)) {
    outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
  }
  allverts <- dplyr::inner_join(outlist$tXv, outlist$v, "vertex_")
  allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, 
                                            sep = "_")))
  allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
  outlist$tXv <- allverts[, c("triangle_", "vertex_")]
  outlist$v <- dplyr::distinct_(allverts, "vertex_", .keep_all = TRUE)[, 
                                                                       c("x_", "y_", "vertex_")]
  outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", 
                                 ctime = format(Sys.time(), tz = "UTC"))
  class(outlist) <- "trimesh"
  outlist
}