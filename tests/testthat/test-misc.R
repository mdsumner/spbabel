context("misc")

## TODO: Rename context
## TODO: Add more tests

test_that("misc", {

  expect_that(spbabel:::detectSpClass(data.frame(x = 1)), throws_error("cannot create Spatial"))
})
