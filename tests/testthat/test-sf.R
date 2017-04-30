library(testthat)
context("sf")
library(sf)
mix <- st_sfc(st_point(1:2), st_linestring(matrix(1:4,2)))

ncpoly <- readRDS(system.file("extdata", "testdata", "nc.rds", package = "spbabel"))
ncline <- st_as_sf(st_as_sf(as(as(ncpoly, "Spatial"), "SpatialLinesDataFrame")))
ncpoint <- st_as_sf(as(as(ncline, "Spatial"), "SpatialMultiPointsDataFrame"))
#sptable(ncpoly)
#sptable(ncline)

nc <- ncpoly
test_that("sf conversion works", {
  expect_that(map_table(nc), is_a("list"))

  ## sptable now supported
  expect_silent(map_table(st_as_sf(as(as(nc, "Spatial"), "SpatialLinesDataFrame"))))
  expect_silent(map_table(st_as_sf(as(as(as(nc, "Spatial"), "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame"))))
})


sfzoo <- function() {

  x <- st_point(c(1,2))

  p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
  mp <- st_multipoint(p)
  s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  ls <- st_linestring(s1)
  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
  s3 <- rbind(c(0,4.4), c(0.6,5))
  mls <- st_multilinestring(list(s1,s2,s3))
  p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
  p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
  pol <-st_polygon(list(p1,p2))
  p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
  p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
  p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
  mpol <- st_multipolygon(list(list(p1,p2), list(p3,p4), list(p5)))
  #gc <- st_geometrycollection(list(mp, mpol, ls))

  list(point = x, multipoint = mp, linestring = ls, multilinestring = mls, polygon = pol,
       multipolygon = mpol)
}
sfgeomc <- function() {
  gc <- st_geometrycollection(sfzoo())
}

example(st_read)
Zoo <- do.call(st_sfc, sfzoo())
GC <- sfgeomc()

zoodoo <- data.frame(x = seq_along(Zoo)); zoodoo[["geometry"]]  <- Zoo; zoodoo <- st_as_sf(zoodoo)
gcdoo <- data.frame(x = 1L); gcdoo[["geometry"]] <- st_sfc(GC); gcdoo <- st_as_sf(gcdoo)

library(dplyr)
test_that("correct interpretations", {
  expect_that(distinct(sptable(nc[57, ])), is_a("data.frame"))
  expect_that(nrow(distinct(sptable(nc[57, ]), island_)), equals(2L))
})


test_that("individual topology types work as feature tables", {
  feature_table(zoodoo[1, ]$geometry)
  feature_table(zoodoo[2, ]$geometry)
  feature_table(zoodoo[3, ]$geometry)
  feature_table(zoodoo[4, ]$geometry)
  feature_table(zoodoo[5, ]$geometry)
  feature_table(zoodoo[6, ]$geometry)
})
test_that("sptable can decompose the basic types", {

  sptable(zoodoo[1, ])
  sptable(zoodoo[2, ])
  sptable(zoodoo[3, ])
  sptable(zoodoo[4, ])
  sptable(zoodoo[5, ])
  sptable(zoodoo[6, ])
  expect_warning(sptable(zoodoo), "geometry has more than one topological type")
})

gz <- rbind(zoodoo, gcdoo)

sf_g_apply <- function(x, fun) {
  lapply(sf::st_geometry(x), fun)
}
topology_types <- c("point", "multipoint", "linestring", "multilinestring", "polygon",
                    "multipolygon")
test_that("feature_table methods work", {
  expect_that(sf_g_apply(zoodoo, spbabel:::feature_table), is_a("list")) %>%
    expect_named(topology_types)
  expect_that(sf_g_apply(gcdoo, spbabel:::feature_table), is_a("list"))
  expect_that(feature_table(gcdoo$geometry), is_a("list"))
  expect_that(feature_table(zoodoo$geometry), is_a("list"))
})


test_that('internal functions run', {
  spbabel:::sp_sf_types()
  spbabel:::sf_types()
  expect_that(unname(spbabel:::sf_type("SpatialPolygons")), equals("MULTIPOLYGON"))
})

# sfh <- st_as_sf(sp(holey))
# a <- do.call(rbind, lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"), function(x) st_cast(sfh, x)))
# b <- st_as_sf(tibble(geometry = st_sfc(st_geometrycollection(lapply(st_geometry(a), identity)))))