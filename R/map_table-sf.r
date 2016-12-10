#' @export
#' @importFrom tibble as_tibble
map_table.sf <- function(x, ...) {
  tabmap <- sptable(x)
  ## why did this ever work?
  #  tabdat <- tibble::as_tibble(x)
  tabdat <- as_tibble(drop_sf_geometry(x) )
  ## remove this if sptable is updated
  tabdat$object_ <- id_n(nrow(tabdat))
  tabmap$object_ <- tabdat$object_[factor(tabmap$object_)]
  out <- map_table_From2(tabdat, tabmap)
  # no class or methods in spbabel for map_table()
  class(out) <- c("map_table", "list")
  out
}
drop_sf_geometry <- function(x) {
  x[, -match(attr(x, "sf_column"), names(x))]
}

matrix2list <- function(x) {
  if (!is.null(dim(x))) {
    x <- split(x, rep(seq(ncol(x)), each = nrow(x)))
  } else {
    x <- as.list(x)
  }
  x
}
# polygonData.sf <- function(x) {
# 
#   structure(rapply(rapply(unclass(x), matrix2list, how = "replace"), unclass, how = "list"),
#             bbox = matrix(attr(x[[attr(x, "sf_column")]], "bbox"), ncol = 2))
# }
#' @export
#' @importFrom sf st_geometry
#' @importFrom tibble as_tibble
sptable.sf <- function(x, ...) {
  g <- sf::st_geometry(x)

 # gtab <- do.call(rbind,lapply(seq_along(g),
  #                             function(object_)  do.call(rbind, lapply(seq_along(g[[object_]]),
  #                                                                      function(branch_) do.call(rbind, lapply(seq_along(g[[object_]][[branch_]]),
  #                                                                                                              function(sub_index) cbind(g[[object_]][[branch_]][[sub_index]], object_, branch_, sub_index)))))))
 gtab <- bind_rows(feature_table.sfc(g), .id = "object_")
 if (length(unique(gtab[["type"]])) > 1) warning("geometry has more than one topological type")

 sf_to_grisnames <- function(gnames) {
     gnames <- gsub("^X$", "x_", gnames)
     gnames <- gsub("^Y$", "y_", gnames)
     gnames <- gsub("^Z$", "z_", gnames)
     gnames <- gsub("^M$", "m_", gnames)
     gnames <- gsub("^type$", "type_", gnames)
     gnames
 }
 names(gtab) <- sf_to_grisnames(names(gtab))
 gtab$order_ <-  seq(nrow(gtab))
 gtab$type_ <- NULL
 if (!is.null(gtab[["parent_"]])) {
   gtab$island_ <- gtab$parent_ == 1
   gtab$parent_ <- NULL
 }
 #x <- pmap_df(gtab)
 class(gtab) <- c("coordinate_table", class(gtab))
 gtab
}


# pmap_df <- function(x) {
#   x <- as_tibble(x);
#   names(x)[1:2] <- c("x_", "y_")
#   x[["branch_"]] <- cumsum(c(0, diff(x[["branch_"]]))) + x[["branch_"]] + x[["object_"]]
#   x$island_ <- x$sub_index == 1;
#   x$sub_index <- NULL
#   #x <- x %>% mutate(island_ = sub_index == 1, sub_index = NULL)
# 
#   x
# 
# }
# 
# 
# 
# this causes problems clashing with sf
#as.list.sfc <- function(x) {
#  rapply(rapply(unclass(x), matrix2list, how = "replace"), unclass, how = "list")
#}

## we always need to watch for points as vector rather than matrix
# matrixOrVector0 <- function(x) {
#   ##print(colnms)
#   x <- unclass(x)
#   if (is.null(dim(x))) x <- matrix(x, ncol = length(x))
#   x
# }