library(testthat)
context("sp From Table")
library(maptools)

library(spbabel)
data(mpoint1)
## generate all topology types
data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame") 

## create sptable versions
poly1tab <- sptable(poly1)
line1tab <- sptable(line1)
point1tab <- sptable(point1)

## reconstructed versions
poly0 <- sp(poly1tab, attr_tab = as.data.frame(poly1))
line0 <- sp(line1tab, attr_tab = as.data.frame(line1))
## cannot round-trip with as.data.frame because lines to points adds
## the line metadata (i.e. a partial fortify)
point0 <- sp(point1tab, attr_tab = point1@data)


test_that("we can round-trip sensibly", {
  expect_true(all(names(poly1) == names(poly0)))
  expect_that(nrow(poly1), equals(nrow(poly0)))
  
  expect_true(all(names(line1) == names(line0)))
  expect_that(nrow(line1), equals(nrow(line0)))
  
  ## sp() needs to no replicate names
  expect_true(all(names(point1) == names(point0)))
  expect_that(nrow(point1), equals(nrow(point0)))
  
  # gah these are such a pain
  #expect_true(proj4string(sp) == gsub("^ ", "", proj4string(poly1)))
})

sptabmod <-  poly1tab
sptabmod$object_ <- sptabmod$branch_
spmod <- sp(sptabmod)
test_that("if objects = branchs then fewer rows", {
  expect_that(nrow(spmod), equals(nrow(distinct_(poly1tab, "branch_"))))
})


test_that("can rebuild without attributes", {
  expect_that(sp(poly1tab), is_a(class(poly1)))
})

poly1tab$new <- runif(nrow(poly1tab))
sp1 <- sp(poly1tab, attr_tab = poly1@data)
test_that("attributes are preserved, and adding a new one does only that", {
  skip_on_cran()
  expect_true(all(names(poly1) %in% names(sp1)))
  expect_that(base::setdiff( names(sp1), names(poly1)), equals("new"))
  
})

test_that("mismatched attributes and object number is an error", {
  expect_error(sp(poly1tab, attr_tab = data.frame(x = sample(1:20, 1))), "number of rows in attr must match distinct object in x")
})

sptable(poly1) <- sptable(poly1) %>% mutate(x_ = x_ - 5)
test_that("replacement sptable works", {
  expect_that(poly1, is_a("SpatialPolygonsDataFrame"))
})
