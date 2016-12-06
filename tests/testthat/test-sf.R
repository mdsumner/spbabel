
context("sf")

ncpoly <- readRDS(system.file("extdata", "testdata", "nc.rds", package = "spbabel"))
ncline <- st_as_sf(st_as_sf(as(as(ncpoly, "Spatial"), "SpatialLinesDataFrame")))
ncpoint <- st_as_sf(as(as(ncline, "Spatial"), "SpatialMultiPointsDataFrame"))
sptable(ncpoly)
sptable(ncline)

test_that("sf conversion works", {
 expect_that(map_table(nc), is_a("list"))  
 expect_that(map_table(st_as_sf(as(as(nc, "Spatial"), "SpatialLinesDataFrame"))), is_a("list"))
 expect_that(map_table(st_as_sf(as(as(as(nc, "Spatial"), "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame"))), is_a("list"))
})
