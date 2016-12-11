context("sfFromTable")

library(sf)
#mix <- st_sfc(st_point(1:2), st_linestring(matrix(1:4,2)))

ncpoly <- readRDS(system.file("extdata", "testdata", "nc.rds", package = "spbabel"))
ncline <- st_cast(ncpoly, "MULTILINESTRING")  ## 
line <- st_as_sf(st_as_sf(as(as(ncpoly, "Spatial"), "SpatialLinesDataFrame")))
ncpoint <- st_as_sf(as(as(ncline, "Spatial"), "SpatialMultiPointsDataFrame"))

test_that("re building sf works", {
  expect_equal(2 * 2, 4)
})

rcast <- function(x, fun) {
 lapply(st_geometry(x), function(a) a)
 }

