context("maps")

## TODO: Rename context
## TODO: Add more tests
library(maps)
m <- map(plot = FALSE, fill = TRUE)

test_that("convert maps works", {
  expect_that(sp(m), is_a("SpatialPolygonsDataFrame"))
})
