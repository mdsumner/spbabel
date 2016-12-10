# #' @export
# feature_table_dedupe.Spatial <- function(x, ...) {
#   mt <- map_table(x)
#   object <- mt$o
#   ## avoid Spatial hole-parents for now, maybe forever
#   branch <- mt$b %>% mutate(type = sf_type(class(geometry(x))), parent = NA_character_)
#   #object_, branch_, type_, parent_
#   vertex <- mt$v
#   structure(list(object = object, branch = branch, vertex = vertex), class = "feature_table")
# }

## tibble methods:


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
  factor(sp_sf_types()[sp_type], levels(sf_types()))
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
feature_table.default <- function(x, ...) {
  mutate(tibble::as_tibble(as_matrix(x)), type = class(x)[2L])
}
#' @export
feature_table.MULTIPOLYGON <- function(x, ...) {
   ## pretty sure I will get rid of this parent_ stuff, it needs to be done differently
    x <- mutate(tibble::as_tibble(as_matrix(x)), type = class(x)[2L])
  x[["branch_"]] <-  id_n(length(unique(x[["parent_"]])))[x[["parent_"]]]
 x
}

#' @export
feature_table.GEOMETRYCOLLECTION <- function(x, ...) bind_rows(lapply(x, feature_table))
#' @export
## this unname is only because there are NA names, which kill object_
feature_table.sfc <- function(x, ...) unname(lapply(x, feature_table))

# @importFrom sf st_geometry
# feature_table.sf <- function(x, ...) {
#   idx <- match(attr(x, "sf_column"), names(x))
#   ## watch out for indexing out the geometry column
#   ## because drop = TRUE in old data frames
#   object <- tibble::as_tibble(x)[, -idx]
#   geometry_tables <- lapply(sf::st_geometry(x), feature_table)
#   list(object = object, geometry = geometry_tables)
# }

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
    do.call(rbind, lapply(seq_along(x),  function(parent_) do.call(rbind, lapply(seq_along(x[[parent_]]),
                                                                                  function(part_) cbind(matrixOrVector(x[[parent_]][[part_]], class(x)[1L]),  parent_)))))

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


