library(testthat)
context("replacement")

testthat::skip_if_not(requireNamespace("maptools"))
library(maptools)
library(dplyr)
data(wrld_simpl)
poly1 <- wrld_simpl[50:80, ]

sptable(poly1) <-  dplyr::mutate(sptable(poly1), x_ = x_ - 5)
test_that("replacement sptable works", {
  expect_that(poly1, is_a("SpatialPolygonsDataFrame"))
})

poly2 <- wrld_simpl[1:10, ]
test_that("replacement works even when modifying the number of objects. vs. branches", {
  expect_warning( sptable(poly2) <- sptable(poly2) %>% mutate(object_ = branch_), "dropping")
})



