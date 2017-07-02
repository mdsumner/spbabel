#' Get the xy coordinates. 
#' 
#' A generic version of sp's 'coordinates' to get a centroid.
#' 
#' The default method handles any sp or sf object
#' @importFrom dplyr select
#' @name xy_
#' @export
xy_ <- function(x) UseMethod("xy_")
#' @name xy_
#' @export
xy_.default <- function(x) {
  dplyr::select(spbabel::sptable(x) %>% 
                  group_by(object_) %>% 
                  ## this is where ketchup should be used
                  ## to handle different kinds of centroids
                  summarize(x_ = mean(x_), y_ = mean(y_)), x_, y_)
}
#' @name xy_
#' @export
xy_.BasicRaster <- function(x) {
  stats::setNames(tibble::as_tibble(sp::coordinates(x)), c("x_", "y_"))
}
