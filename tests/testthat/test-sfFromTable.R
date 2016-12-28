context("sfFromTable")

library(sf)
#mix <- st_sfc(st_point(1:2), st_linestring(matrix(1:4,2)))



ncpoly <- readRDS(system.file("extdata", "testdata", "nc.rds", package = "spbabel"))

## soon to be in sf
Paste1 <- function(lst) do.call(c, lapply(lst, unclass))
Paste0 <- function(lst) lapply(lst, unclass)
st_cast.MULTIPOLYGON <- function(x, to) {
  switch(to, 
         MULTIPOLYGON = x, 
         MULTILINESTRING = st_multilinestring(     unlist(Paste0(x), recursive = FALSE, use.names = FALSE)), 
         MULTIPOINT = st_multipoint(do.call(rbind, Tail1(unlist(Paste0(x), recursive = FALSE, use.names = FALSE)))), 
         ## loss, drop to first part
         POLYGON = {warning("polygon from first part only"); st_polygon(x[[1L]])}, 
         LINESTRING = {warning("line from first ring only"); st_linestring(x[[1L]][[1L]])}, 
         ## loss, drop to first coordinate of first ring of first part
         POINT = {warning("point from first coordinate only"); st_point(x[[1L]][[1L]][1L, , drop = TRUE])}
  )
}
ncline <- ncpoly
st_geometry <- st_sfc(lapply(st_geometry(ncline), function(x) st_cast.MULTIPOLYGON(x, "MULTILINESTRING")))
line <- st_as_sf(st_as_sf(as(as(ncpoly, "Spatial"), "SpatialLinesDataFrame")))
#library(sp)
#ncpoint <- st_as_sf(as(as(ncline, "Spatial"), "SpatialMultiPointsDataFrame"))

test_that("re building sf works", {
  expect_equal(2 * 2, 4)
})



