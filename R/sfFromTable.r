setOldClass( c("coordinate_table", "tbl_df", "tbl", "data.frame" ) )

## TBD

setOldClass( c("tbl_df", "tbl", "data.frame" ) )
setOldClass( c("grouped_df", "tbl_df", "tbl", "data.frame" ) )



#' TBD Convert from dplyr tbl form to simple features.
#' 
#' Not yet implemented. 
#' @param x tibble as created by \code{\link{sptable}}
#' @param crs projection, defaults to \code{NA_character_}
#' @param attr_tab remaining data from the attributes
#' @param ... unused
#' @return sf
#' 
sf <- function(x, ...) {
  #stop("not yet implemented")
  UseMethod("sf")
}

#' @rdname sf
sf.data.frame <- function(x, attr_tab = NULL, crs, ...) {
  sfFromTable(x, attr_tab = attr_tab, crs = crs, ...)
}

sfFromTable <- function(x, attr_tab =  NULL, crs, ..., topol_ = NULL) {
  sf::st_as_sf(sp(x, attr_tab = attr_tab, crs = crs, topol_ = topol_))
}