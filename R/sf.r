#' @export
#' @importFrom tibble as_tibble
map_table.sf <- function(x, ...) {
  tabmap <- sptable(x)
  tabdat <- tibble::as_tibble(drop_sf_geometry(x) )
  ## remove this if sptable is updated
  tabdat$object_ <- id_n(nrow(tabdat))
  tabmap$object_ <- tabdat$object_[factor(tabmap$object_)]
  out <- map_table_From2(tabdat, tabmap, ...)
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

#' @export
#importFrom sf st_geometry
#' @importFrom tibble as_tibble
sptable.sf <- function(x, ...) {
  ## g <- sf::st_geometry(x)
  g <- x[[attr(x, "sf_column")]]
  
  ftl <- feature_table.sfc(g)
  
  
 if ("island_" %in% names(ftl[[1]])) {
   ftl <- lapply(ftl, function(x) {
     x[["branch_"]] <- paste(x[["branch_"]], x[["island_"]])
     x[["island_"]] <- as.integer(factor(x[["branch_"]])) < 2
     
     x
   })
 }
  gtab <- dplyr::bind_rows(ftl, .id = "object_")
  
  if ("branch_" %in% names(ftl[[1]])) {
    gtab[["branch_"]] <- as.integer(factor(gtab[["branch_"]]))
  }
  
  gtab[["object_"]] <- as.integer(factor(gtab[["object_"]]))
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
  #if (!is.null(gtab[["parent_"]])) {
  #  gtab$island_ <- gtab$parent_ == 1
  #  gtab$parent_ <- NULL
  #}
  class(gtab) <- c("coordinate_table", class(gtab))
  gtab
}


sf_types <- function() {
  sf_topologies  <-
    c("GEOMETRY",
      "POINT",
      "LINESTRING",
      "POLYGON",
      "MULTIPOINT",
      "MULTILINESTRING",
      "MULTIPOLYGON",
      "GEOMETRYCOLLECTION",
      "CIRCULARSTRING",
      "COMPOUNDCURVE",
      "CURVEPOLYGON",
      "MULTICURVE",
      "MULTISURFACE",
      "CURVE",
      "SURFACE",
      "POLYHEDRALSURFACE",
      "TIN",
      "TRIANGLE")
  factor(sf_topologies, sf_topologies)
}

sp_sf_types <- function() {
  c(SpatialPoints = "POINT", SpatialMultiPoints = "MULTIPOINT",
    SpatialLines = "MULTILINESTRING", SpatialPolygons = "MULTIPOLYGON",
    SpatialCollection = "GEOMETRYCOLLECTION")
}

sf_type <- function(sp_type) {
  #factor(sp_sf_types()[sp_type], levels(sf_types()))
  sp_sf_types()[sp_type]
}






#' Normal form for sf
#'
#' A `feature_table` is a normal form for simple features, where all branches are
#' recorded in one table with attributes object_, branch_, type_, parent_. All instances of parent_
#' are NA except for the holes in multipolygon.
#'
#' There is wasted information stored this way, but that's because this is intended as a lowest common denominator format.
#'
#'
#' There are three tables, objects (the feature attributes and ID), branches (the parts),
#' coordinates (the X, Y, Z, M values).
#'
#' @param x sf object
#' @param ... ignored
#'
#' @export
feature_table <- function(x, ...) {
  UseMethod("feature_table")
}


#' @export
#' @importFrom dplyr mutate
feature_table.default <- function(x, ...) {
  x <- dplyr::mutate(as_tibble(x), type = rev(class(x))[2L])
  nms <- intersect(c("branch_"), names(x))
  for (i in seq_along(nms)) x[[nms[i]]] <- id_n(length(unique(x[[nms[i]]])))[x[[nms[i]]]]
  x
}


#' @export
feature_table.GEOMETRYCOLLECTION <- function(x, ...) dplyr::bind_rows(lapply(x, feature_table))
#' @export
## this unname is only because there are NA names, which kill object_
feature_table.sfc <- function(x, ...) {
  out <- unname(lapply(x, feature_table))
 out 
}



#' Individual geometries as tibbles. 
#'
#' @param x sf geometry of type sfg
#'
#' @return tibble
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' 
#' tibble::as_tibble(sf::st_point(c(1, 1, 1)))
as_tibble.sfg <- function(x) {
     x <- as_tibble(as_matrix(x))
     ## convert non-coordinates to integer (remove this when someone cracks the limit)
     nms <- setdiff(names(x), c("X", "Y", "Z", "M"))
     for (i in seq_along(nms)) x[[nms[i]]] <- as.integer(x[[nms[i]]])
     x
}
as_matrix <- function(x, ...) UseMethod("as_matrix")
matrixOrVector <-
  function(x, gclass) {
    colnms <- unlist(strsplit(gclass, ""))
    ##print(colnms)
    #x <- unclass(x)
    if (is.null(dim(x))) x <- matrix(x, ncol = length(x))
    structure(as.matrix(x), dimnames = list(NULL, colnms))
  }


as_matrix.GEOMETRYCOLLECTION <- function(x, ...) lapply(x, function(a) as_matrix(a))

#as_matrix.sfc_GEOMETRYCOLLECTION <- function(x, ...) as_matrix(x[[1]])

as_matrix.MULTIPOLYGON <-
  function(x, ...) {
    do.call(rbind, lapply(seq_along(x),  function(island_) do.call(rbind, lapply(seq_along(x[[island_]]),
                                                                                 function(branch_) cbind(matrixOrVector(x[[island_]][[branch_]], class(x)[1L]),  island_, branch_)))))
    
  }

as_matrix.POLYGON <-
  function(x, ...) {
    do.call(rbind, lapply(seq_along(x),
                          function(branch_) cbind(matrixOrVector(x[[branch_]], class(x)[1L]),  branch_)))
  }

as_matrix.MULTILINESTRING <-
  function(x, ...) {
    do.call(rbind, lapply(seq_along(x),
                          function(branch_) cbind(matrixOrVector(x[[branch_]], class(x)[1L]),  branch_)))
  }


as_matrix.MULTIPOINT <- function(x, ...) {
  x <- matrixOrVector(x, class(x)[1L])
  cbind(x, branch_ = seq_len(nrow(x)))
}
as_matrix.POINT <- as_matrix.LINESTRING <- function(x, ...) {
  matrixOrVector(x, class(x)[1L])
}


