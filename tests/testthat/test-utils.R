context("utils")

## TODO: Rename context
## TODO: Add more tests
library(spbabel)
data(holey)
sph <- sp(holey)
x <- c(1L)
r <- raster::setValues(raster::raster(sph), 1)
test_that("utils tests work", {
  expect_true(spbabel:::has_data(sph))
  expect_true(spbabel:::has_names(sph))
  expect_false(spbabel:::has_names(x))
  expect_output(show(sph))
  expect_output(spbabel::print(sph))
  sph1 <- sph
  sph1@data <- as.data.frame(sph@data)
  expect_output(spbabel::print(sph1))

  expect_output(show(as(sph, "SpatialLinesDataFrame")))
  expect_output(show(as(as(sph, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")))
  expect_output(show(as(as(sph, "SpatialLinesDataFrame"), "SpatialMultiPointsDataFrame")))

})
