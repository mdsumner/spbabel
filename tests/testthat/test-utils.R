context("utils")

## TODO: Rename context
## TODO: Add more tests
library(spbabel)
data(holey)
sph <- sp(holey)
x <- c(1L)
test_that("utils tests work", {
  expect_that(spbabel:::has_data(sph), is_true())
  expect_that(spbabel:::has_names(sph), is_true())
  expect_that(spbabel:::has_names(x), is_false())
 # expect_that(spbabel:::.detectSpatial(class(sp::geometry(sph))), is_a("function"))
  print(sph)

})
