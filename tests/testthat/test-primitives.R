context("primitives")

## TODO: Rename context
## TODO: Add more tests
compact_indexes <- function(x) {
  v <- x$v 
  lXv <- x$lXv
  l <- x$l
  o <- x$o
  
  lXv$vertex_ <- as.integer(factor(lXv$vertex_, levels = v$vertex_))
  v$vertex_ <- seq_len(nrow(v))
  
  lXv$segment_ <- as.integer(factor(lXv$segment_, levels = l$segment_))
  l$segment_ <- seq_len(nrow(l)) ## won't be integer if long
  l$object_ <- as.integer(factor(l$object_), levels = x$o$object_)
  o$object_ <- seq_len(nrow(o))
  x$o <- o
  x$l <- l
  x$lXv <- lXv
  x$v <- v
  x
}


library(maptools)
data(wrld_simpl)
test_that("primitives works", {
  lin1 <- compact_indexes(primitives(wrld_simpl))
  lin2 <- compact_indexes(primitives(as(wrld_simpl, "SpatialLinesDataFrame")))
  ## these can't be identical unless we use joins to maintain the restructure  

})
