has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}


has_data <- function(x) .hasSlot(x, "data")

#' @importFrom methods .hasSlot
#' @importFrom sp bbox proj4string 
#' @importFrom tibble as_tibble 
.print_Spatial <- 
  function (x, ...) 
  {
    cat("class       :", class(x), "\n")
    isRaster <- hasData <- FALSE
    nc <- 0
    if (has_data(x)) {
      hasData <- TRUE
      nc <- ncol(x@data)
    }
    ln <- 1
    
    ln <- length(x)
    cat("features    :", ln, "\n")
    
    e <- bbox(x)
    if (ln > 0) {
      cat("extent      : ", e[1, 1], ", ", e[1, 2], ", ", e[2, 
                                                            1], ", ", e[2, 2], "  (xmin, xmax, ymin, ymax)\n", 
          sep = "")
    }
    cat("coord. ref. :", proj4string(x), "\n")
    if (hasData) {
      x <- x@data
      maxnl <- 15
      if (!isRaster) {
        cat("variables   : ", nc, "\n", sep = "")
      }
      if (!inherits(x, "tbl_df")) x <- tibble::as_tibble(x)
      print(x)
    }
  }

#' Sp methods
#' @param object Spatial object
#' @param x Spatial object
#' @param ... ignored
#' @title sp methods
#' @rdname sp-methods
#' @export
#' @importFrom methods show 
setMethod("show", "SpatialPolygonsDataFrame", 
          function(object) {
            .print_Spatial(object)
          }
          
)

#' @rdname sp-methods
#' @export
setMethod("show", "SpatialLinesDataFrame", 
          function(object) {
            .print_Spatial(object)
          }
)

#' @rdname sp-methods
#' @export
setMethod("show", "SpatialPointsDataFrame", 
          function(object) {
            .print_Spatial(object)
          }
)

#' @rdname sp-methods
#' @export
setMethod ('print' , 'Spatial', 
           function(x, ...) {
             .print_Spatial(x)
           }
)

