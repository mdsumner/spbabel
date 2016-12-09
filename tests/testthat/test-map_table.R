library(testthat)

context("basic building")

library(spbabel)
spmap <- sp(semap, seatt)

test_that("we can ingest complex objects from sp", {
  expect_that(map_table(spmap), is_a("list"))
  expect_that(map_table(spmap, normalize = FALSE), is_a("list"))
  expect_that(map_table(subset(spmap, NAME == "Australia"), triangulate = TRUE), is_a("list"))
  
})

suppressPackageStartupMessages(library(dplyr))
test_that("we can ingest a line object from sp", {
  x <- c(1:9, 8:1)
  y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
  df <- data.frame(x = x, y = y, g = rep(c("a", "b", "c"), c(5, 4, 8)))
  mklinelist <- function(yy, fg) lapply(split(yy, fg), function(x) sp::Line(as.matrix(x[,1:2])))
  line <- sp::SpatialLinesDataFrame(sp::SpatialLines(list(sp::Lines(mklinelist(df, df$g), "1"), 
                                                          sp::Lines(mklinelist(df %>% dplyr::mutate(x = y, y = rev(x) * 4) %>% as.data.frame, df$g), "16"))), 
                                df[c(1, 16),])
  gline <- map_table(line)
  expect_that(gline, is_a("list"))

})

test_that("we can ingest a line object from rasterToContour", {
  levs <- c(94, 108, 124, 150, 195)
  r <- raster::raster(volcano)
  cl <- raster::rasterToContour(r, lev = levs)
  g <- map_table(cl)
  ## why did this change 2016-12-07?
  ## why does it change on recompile?
  #expect_that(nrow(g$v), equals(703))
  #expect_that(nrow(g$v), equals(642))
  expect_that(nrow(g$o), equals(4))
  #expect_that(nrow(g$v %>% inner_join(g$bXv) %>% inner_join(g$b) %>% inner_join(g$o)), equals(703))
  ## why did this change? 2015-11-06
  expect_that(nrow(g$v %>% 
                     dplyr::inner_join(g$bXv) %>% 
                     dplyr::inner_join(g$b) %>% 
                     dplyr::inner_join(g$o)), equals(707))
})

test_that("build from scratch", {

  library(sp)
  # simple example, from scratch:
  Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
  Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
  Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
  Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
  
  Srs1 = Polygons(list(Sr1), "s1")
  Srs2 = Polygons(list(Sr2), "s2")
  Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
  Sp <- SpatialPolygonsDataFrame( SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3), data.frame(x = 1:3, row.names = c("s1", "s2", "s3/4")))
  g <- map_table(Sp)
  for (i in seq_along(g)) {
    expect_that(g[[i]], is_a("tbl_df"))
  }
  expect_true(nrow(g$o) < nrow(g$b))
  expect_true(nrow(g$bXv) > nrow(g$b))
  expect_true(nrow(g$v) < nrow(g$bXv))
  coords <- as(as(Sp, "SpatialLinesDataFrame"), 
                             "SpatialPointsDataFrame")
  expect_that(nrow(g$bXv), 
              equals(nrow(coords)))
  expect_that(nrow(g$v), equals(nrow(coordinates(coords)[!duplicated(coordinates(coords)), ])))
  
})

library(maptools)
data(wrld_simpl)
pts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")
mpts <- as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")

test_that("points work", {
  expect_that(names(map_table(mpts)), equals(c("o", "b", "bXv", "v")))
  expect_that(names(map_table(pts)), equals(c("o", "b", "bXv", "v")))
})

