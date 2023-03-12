#' @export
#' @rdname sptable
#' @importFrom dplyr bind_cols
map_table.trip <- function(x, ...) {
  
  if (!requireNamespace("trip", quietly = TRUE)) {
    stop("trip package is required but not available\n try 'install.packages(\"trip\")'")
  }
  mpts <- as_trip_MultiPoints(x)
  tabs <- map_table(mpts)
  ## now put the stuff back on
  tabs$bXv <- dplyr::bind_cols(tabs$bXv, x@data)
  tabs
}

as_trip_MultiPoints <- function(x, ...) {
  tor <- x@TOR.columns
  SpatialMultiPointsDataFrame(
    ## sheesh
    lapply(split(as.data.frame(coordinates(x)), x[[tor[2L]]]), as.matrix), 
    data.frame(object_ = seq_len(length(unique(x[[tor[2L]]]))), stringsAsFactors = FALSE), 
    proj4string = CRS(proj4string(x)), match.ID = FALSE)
}

