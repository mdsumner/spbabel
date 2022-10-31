
#' @export
#' @importFrom stats setNames
#' @rdname sptable
sptable.SpatialPolygons <- function(x, ...) {
  x <- setNames(as_tibble(rasterpoly(x)),
                c("object_",  "part_", "branch_", "island_", "x_", "y_"))
  x[["part_"]] <- NULL
  x[["order_"]] <- unlist(lapply(split(x[["branch_"]], x[["branch_"]]), seq_along), use.names = FALSE)
  x[["island_"]] <- x[["island_"]] == 0
  x[["object_"]] <- as.integer(x[["object_"]])
  x[["branch_"]] <- as.integer(x[["branch_"]])
  x[["order_"]] <- as.integer(x[["order_"]])
  x[, c("object_", "branch_", "island_", "order_", "x_", "y_")]

}

#' @export
#' @rdname sptable
#' @importFrom stats setNames
sptable.SpatialLines <- function(x, ...) {

  x <- setNames(as_tibble(rasterline(x)),
                c("object_",  "part_", "branch_", "x_", "y_"))
  x[["order_"]] <- unlist(lapply(split(x[["branch_"]], x[["branch_"]]), seq_along), use.names = FALSE)
  x[["part_"]] <- NULL
  x[["object_"]] <- as.integer(x[["object_"]])
  x[["branch_"]] <- as.integer(x[["branch_"]])
  x[["order_"]] <- as.integer(x[["order_"]])
  x[, c("object_", "branch_", "order_", "x_", "y_")]

}

#' @export
#' @rdname sptable
#' @importFrom dplyr bind_cols
sptable.SpatialPointsDataFrame <- function(x, ...) {
  df <- .pointsGeom(x, ...)
  df$object_ <- as.integer(df$object_) ## not needed once .pointsGeom uses tbl_df
  df
}

#' @export
#' @rdname sptable
sptable.SpatialMultiPointsDataFrame <- function(x, ...) {
  df <- .pointsGeom(x, ...)
  df$object_ <- as.integer(df$object_)
  df$branch_ <- as.integer(df$branch_)
  df
}
# ## TODO multipoints
# #' @importFrom tibble as_tibble
# mat2d_f <- function(x) {
#   as_tibble(as.data.frame((x)))
# }


#' @rdname sptable
#' @param object Spatial object
#' @param value modified sptable version of object
#'
#' @return Spatial object
#' @export
"sptable<-" <-
  function(object, value) {
    # joiner <- idmaker(20)
    #    datadata <- as.data.frame(object)
    #    datadata[[joiner]] <- seq(nrow(datadata))
    #    value[[joiner]] <- as.integer(factor(value[["object_"]]))
    #    datadata <- inner_join(datadata, value[, joiner])
    if (nrow(object)  == length(unique(value$object_))) {
      ## assume ok
    } else {
      warning("dropping attribute data since object number and metadata rows not the same")
      return(sp(value,  crs = proj4string(object)))
    }
    sp(value,  as.data.frame(object), proj4string(object))

  }
#' @importFrom dplyr bind_cols
.pointsGeom <-  function(x, ...) {
  ## this will have to become a tbl
  xy <- as_tibble(as.data.frame(coordinates(x)))
  cnames <- c('object_', 'x_', 'y_')
  ##xy <- cbind(1:nrow(xy), xy)
  if (is.list(x@coords)) {
    br <- rep(seq_along(x@coords), unlist(lapply(x@coords, nrow), use.names = FALSE))
    cnames <- c('branch_', 'object_', 'x_', 'y_')
    xy <- dplyr::bind_cols(tibble(br), tibble(br), xy)
  } else {
    br <- seq(nrow(xy))
    xy <- dplyr::bind_cols(tibble(br), xy)
  }

  colnames(xy) <- cnames
  return(xy)
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
  if (inherits(x, "SpatialPointsDataFrame")) {
    tabmap$branch_ <- id_n(nrow(tabmap))
  } else {
    tabmap$branch_ <- id_n(length(unique(tabmap$branch_)))[factor(tabmap$branch_)]
  }

  out <- map_table_From2(tabdat, tabmap)

  # no class or methods in spbabel for map_table()
  #class(out) <- c("map_table", "list")
  out
}

## rasterpoly because adapted from raster::geom
rasterpoly <- function (x, sepNA = FALSE, ...)
{
  nobs <- length(x@polygons)
  objlist <- list()
  cnt <- 0

  for (i in 1:nobs) {
    nsubobs <- length(x@polygons[[i]]@Polygons)
    ps <- lapply(1:nsubobs, function(j) cbind(j,
                                              j + cnt, x@polygons[[i]]@Polygons[[j]]@hole,
                                              x@polygons[[i]]@Polygons[[j]]@coords))
    objlist[[i]] <- cbind(i, do.call(rbind, ps))
    cnt <- cnt + nsubobs
  }
  #}
  obs <- do.call(rbind, objlist)
  colnames(obs) <- c("object", "part", "cump", "hole",
                     "x", "y")
  rownames(obs) <- NULL
  #if (sepNA) {
  #  obs[is.na(obs[, 2]), ] <- NA
  #}
  obs
}
## rasterline because adapted from raster::geom
rasterline <-  function (x, sepNA = FALSE, ...)  {
  nobs <- length(x@lines)
  objlist <- list()
  cnt <- 0
  for (i in 1:nobs) {
    nsubobj <- length(x@lines[[i]]@Lines)
    ps <- lapply(1:nsubobj, function(j) cbind(j,
                                              j + cnt, x@lines[[i]]@Lines[[j]]@coords))
    objlist[[i]] <- cbind(i, do.call(rbind, ps))
    cnt <- cnt + nsubobj
  }
  #}
  obs <- do.call(rbind, objlist)
  colnames(obs) <- c("object", "part", "cump", "x", "y")
  rownames(obs) <- NULL
  obs
}
