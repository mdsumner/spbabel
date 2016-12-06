
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
#
# as.list.sfc <- function(x) {
#   rapply(rapply(unclass(x), matrix2list, how = "replace"), unclass, how = "list")
# }
# 
# polygonData.sf <- function(x) {
#   matrix2list <- function(x) {
#     split(x, rep(seq(ncol(x)), each = nrow(x)))
#   }
#   structure(rapply(rapply(unclass(x), matrix2list, how = "replace"), unclass, how = "list"), 
#             bbox = matrix(attr(nc[[attr(nc, "sf_column")]], "bbox"), ncol = 2))
# }
# 
# #' @importFrom sf st_crs
# rangl.sf <- function (x, max_area = NULL, ...) 
# {
#   pr4 <- sf::st_crs(x)$proj4string
#   
#   tabs <- spbabel::map_table(x)
#   ll <- vector("list", nrow(tabs$o))
#   for (i_obj in seq(nrow(tabs$o))) {
#     tabs_i <- tabs
#     tabs_i$o <- tabs_i$o[i_obj, ]
#     tabs_i <- rangl:::semi_cascade(tabs_i)
#     tt_i <- rangl:::tri_mesh_map_table1(tabs_i, max_area = max_area)
#     ll[[i_obj]] <- tt_i
#   }
#   outlist <- vector("list", length(ll[[1]]))
#   nms <- names(ll[[1]])
#   names(outlist) <- nms
#   for (i in seq_along(outlist)) {
#     outlist[[i]] <- dplyr::bind_rows(lapply(ll, "[[", nms[i]))
#   }
#   allverts <- dplyr::inner_join(outlist$tXv, outlist$v, "vertex_")
#   allverts$uvert <- as.integer(factor(paste(allverts$x_, allverts$y_, 
#                                             sep = "_")))
#   allverts$vertex_ <- spbabel:::id_n(length(unique(allverts$uvert)))[allverts$uvert]
#   outlist$tXv <- allverts[, c("triangle_", "vertex_")]
#   outlist$v <- dplyr::distinct_(allverts, "vertex_", .keep_all = TRUE)[, 
#                                                                        c("x_", "y_", "vertex_")]
#   outlist$meta <- tibble::tibble(proj = pr4, x = "x_", y = "y_", 
#                                  ctime = format(Sys.time(), tz = "UTC"))
#   class(outlist) <- "trimesh"
#   outlist
# }


#' Normal form for sf
#' 
#' @param x sf object
#' @param ... ignored
#'
#' @export
feature_table <- function(x, ...) {
  UseMethod("feature_table")
}

#' @export
feature_table.default <- function(x, ...) {
  tibble::as_tibble(as_matrix(x))
}

#' @export
feature_table.GEOMETRYCOLLECTION <- function(x, ...) lapply(x, feature_table)


as_matrix <- function(x, ...) UseMethod("as_matrix")
matrixOrVector <- 
function(x, gclass) {
  colnms <- unlist(strsplit(gclass, ""))
  ##print(colnms)
  x <- unclass(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = length(x))
  structure(as.matrix(x), dimnames = list(NULL, colnms))
}


as_matrix.GEOMETRYCOLLECTION <- function(x, ...) lapply(x, function(a) as_matrix(a))

#as_matrix.sfc_GEOMETRYCOLLECTION <- function(x, ...) as_matrix(x[[1]])

as_matrix.MULTIPOLYGON <-  
  function(x, ...) {
    do.call(rbind, lapply(seq_along(x),  function(polygon_) do.call(rbind, lapply(seq_along(x[[polygon_]]), 
                          function(island_) cbind(matrixOrVector(x[[polygon_]][[island_]], class(x)[1L]),  polygon_, island_)))))
    
}

as_matrix.POLYGON <-  
  function(x, ...) {
    do.call(rbind, lapply(seq_along(x), 
                          function(island_) cbind(matrixOrVector(x[[island_]], class(x)[1L]),  island_)))
  }

as_matrix.MULTILINESTRING <- 
  function(x, ...) {
    do.call(rbind, lapply(seq_along(x), 
                          function(branch_) cbind(matrixOrVector(x[[branch_]], class(x)[1L]),  branch_)))
  }

as_matrix.POINT <- as_matrix.MULTIPOINT  <- as_matrix.LINESTRING <- function(x, ...) {
  matrixOrVector(x, class(x)[1L])
}



# #' @importFrom sf st_geometry
# #' @importFrom tibble as_tibble
# sptable.sf <- function(x) {
#   g <- sf::st_geometry(x)
#   
#   ## multipolygon
#   gtab <- do.call(rbind,lapply(seq_along(g), 
#                                function(object_)  do.call(rbind, lapply(seq_along(g[[object_]]), 
#                                                                         function(branch_) do.call(rbind, lapply(seq_along(g[[object_]][[branch_]]), 
#                                                                                                                 function(sub_index) cbind(g[[object_]][[branch_]][[sub_index]], object_, branch_, sub_index)))))))
#   # matrixOrVector <- function(x) {
#   #   if (is.null(dim(x))) x <- matrix(x, ncol = length(x))
#   #   x
#   # }
#   # ## polygon, multilinestring, linestring, 
#   # gtab <- do.call(rbind,lapply(seq_along(g), 
#   #                              function(object_)  do.call(rbind, lapply(seq_along(g[[object_]]), 
#   #                                                                       function(branch_) cbind(g[[object_]][[branch_]], object_, branch_)))))
#   # ## multipoint, point
#   # gtab <- do.call(rbind,lapply(seq_along(g), 
#   #                              function(branch_)  cbind(matrixOrVector(g[[branch_]]), branch_)))
#   # 
#   # 
#   # 
#   pmap_df <- function(x) {
#     x <- as_tibble(x); 
#     names(x)[1:2] <- c("x_", "y_")
#     x[["branch_"]] <- cumsum(c(0, diff(x[["branch_"]]))) + x[["branch_"]] + x[["object_"]]
#     x <- x %>% mutate(island_ = sub_index == 1, sub_index = NULL)
#     x
#   }
#   pmap_df(gtab)
# }
