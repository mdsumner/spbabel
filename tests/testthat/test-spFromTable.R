library(testthat)
context("sp From Table")
library(maptools)
library(dplyr)
library(spbabel)

data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame")

sptab <- sptable(poly1)
sp0 <- sp(sptab, attr_tab = as.data.frame(poly1))

sptabmod <- sptab %>% mutate(object_ = branch_)
sp(sptabmod)
test_that("we can round-trip sensibly", {
  expect_true(all(names(poly1) == names(sp0)))
  expect_that(nrow(poly1), equals(nrow(sp0)))
  
  # gah these are such a pain
  #expect_true(proj4string(sp) == gsub("^ ", "", proj4string(poly1)))
})

test_that("can rebuild without attributes", {
  expect_that(sp(sptab), is_a(class(poly1)))
})

sptab$new <- runif(nrow(sptab))
sp1 <- sp(sptab, attr_tab = as.data.frame(sptab))
test_that("attributes are preserved, and adding a new one does only that", {
  expect_true(all(names(poly1) == names(sp1)))
  expect_that(setdiff( names(sp1), names(poly1)), equals("new"))
  
})

test_that("mismatched attributes and object number is an error", {
  expect_error(sp(sptab, attr_tab = data.frame(x = sample(1:20, 1))), "number of rows in attr must match distinct object in x")
})

sptable(poly1) <- sptable(poly1) %>% mutate(x_ = x_ - 5)
test_that("replacement sptable works", {
  expect_that(poly1, is_a("SpatialPolygonsDataFrame"))
})
