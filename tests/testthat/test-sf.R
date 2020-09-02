library(testthat)
testthat::skip_if_not(requireNamespace("sf"))

context("sf")
library(sf)
mix <- st_sfc(st_point(1:2), st_linestring(matrix(1:4,2)))

ncpoly <- readRDS(system.file("extdata", "testdata", "nc.rds", package = "spbabel"))
ncline <- st_as_sf(as(as(ncpoly, "Spatial"), "SpatialLinesDataFrame"))
ncpoint <- st_as_sf(as(as(ncline, "Spatial"), "SpatialMultiPointsDataFrame"))
#sptable(ncpoly)
#sptable(ncline)

test_that("columns are correct", {
  sptable(ncpoint[50:52, ]) %>% expect_named(c("object_", "x_", "y_", "branch_"))
})
test_that("round trip sf to sp works", {
  sptable(ncpoly[10:12, ]) %>% sp() %>% expect_s4_class("SpatialPolygonsDataFrame")
  sptable(st_cast(ncpoly[10:12, ], "POLYGON", warn = FALSE)) %>% sp() %>% expect_s4_class("SpatialPolygonsDataFrame")
  as_matrix(ncpoly$geometry[[1]]) %>% tibble::as_tibble() %>% expect_named(c("X", "Y", "island_", "branch_"))
  as_matrix(st_cast(ncpoly$geometry[[1]], "POLYGON", warn = FALSE)) %>% tibble::as_tibble() %>% expect_named(c("X", "Y", "island_", "branch_"))

  suppressWarnings(as_matrix(st_cast(ncpoly$geometry[[4]], "LINESTRING", warn = FALSE))) %>% tibble::as_tibble() %>% expect_named(c("X", "Y"))
  as_matrix(st_cast(ncpoly$geometry[[4]], "MULTILINESTRING", warn = FALSE)) %>% tibble::as_tibble() %>% expect_named(c("X", "Y", "branch_"))


  suppressWarnings(as_matrix(st_cast(ncpoly$geometry[[4]], "POINT", warn = FALSE))) %>% tibble::as_tibble() %>% expect_named(c("X", "Y"))
  as_matrix(st_cast(ncpoly$geometry[[4]], "MULTIPOINT", warn = FALSE)) %>% tibble::as_tibble() %>% expect_named(c("X", "Y", "branch_"))

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

nc <- read_sf(system.file("shape/nc.shp", package="sf"))
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
  lapply(st_geometry(x), fun)
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

test_that("recomposition works", {
  sptable(nc[, 1:4]) %>% sp()
  expect_error(sptable(nc[, 1:4]) %>% sf(), "available")
})

test_that('internal functions run', {
  spbabel:::sp_sf_types()
  spbabel:::sf_types()
  expect_that(unname(spbabel:::sf_type("SpatialPolygons")), equals("MULTIPOLYGON"))
})


## multipoint
mp <- st_sf(a = 1:2, geometry = st_sfc(st_multipoint(cbind(0, 1:2)), st_multipoint(cbind(0, 1:4))))
test_that("multipoints recreated", {
  tab <- as.data.frame(mp)
  if (attr(mp, "sf_column") %in% names(mp)) tab[[attr(mp, "sf_column")]] <- NULL
  map <- spbabel::sptable(mp)
# crs <- attr(tab[[attr(mp, "sf_column")]], "crs")$proj4string
  spbabel::sp(map, tab) %>% expect_s4_class("SpatialMultiPointsDataFrame")


})

# sfh <- st_as_sf(sp(holey))
# a <- do.call(rbind, lapply(c("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT", "POLYGON", "LINESTRING", "POINT"), function(x) st_cast(sfh, x)))
# b <- st_as_sf(tibble(geometry = st_sfc(st_geometrycollection(lapply(st_geometry(a), identity)))))


test_that("raw geometry works", {
          sptable(st_geometry(mp)) %>%
    expect_named(c("object_", "x_", "y_", "branch_"))
          expect_warning(ew <- sptable(st_geometry(mix)) , "more than one")
          ew %>% expect_named(c("object_", "x_", "y_"))
}
          )
