context("column-types")

data(holey)
base::suppressPackageStartupMessages(library(dplyr))
data(holey)
sph <- sp(holey)  ## ok

library(dplyr)
h1 <- holey %>% mutate(object_ = as.character(object_))
h2 <- holey %>% mutate(branch_ = as.character(branch_ ))
h3 <- holey %>% mutate(branch_ = factor(branch_ ))
h4 <- holey %>% mutate(object_= factor(object_ ))

test_that("branch and object can be character", {
  expect_that(sp(h1), is_a(class(sph)))
  expect_that(sp(h2), is_a(class(sph)))
})

test_that("branch and object can be factor", {
  expect_that(sp(h3), is_a(class(sph)))
  expect_that(sp(h4), is_a(class(sph)))
})

