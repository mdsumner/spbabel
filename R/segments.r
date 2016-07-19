## convert long to wide

segment_lw <- function(x) {
  bind_rows(lapply(split(x, x$segment_), function(xa) tibble(segment_ = xa$segment_[1L], s1 = xa$vertex_[1L], s2 = xa$vertex_[2L])))
}

#' @noRd
#' @examples 
#' tb <- tibble(segment_ = 1:4, s1 = 1:4, s2 = 2:5)
#' sg <- segment_wl(tb)
#' all(segment_lw(sg) == tb)
segment_wl <- function(x) {
  tibble(vertex_ = as.vector(t(as.matrix(x[, c("s1", "s2")]))), 
         segment_ = rep(x$segment_, each = 2))
}


branchV_to_segmentV <- function(x) {
  suppressWarnings(
    x <- head(matrix(x, ncol = 2, nrow = length(x) + 1L), -1L)
  )
  colnames(x) <- c("s1", "s2")
  as_tibble(x)
}

segmentV_to_vertexV <- function(x) {
  tibble(vertex_ = as.vector(t(as.matrix(x[, c("s1", "s2")]))), 
         segment_ = rep(x$segment_, each = 2))
}
make_segment_table <- function(x) {
  ubranch <- unique(x$branch_)
  splmap <- split(x, x$branch_)
  bind_rows(lapply(seq_along(ubranch), 
                   function(b_id) mutate(branchV_to_segmentV(splmap[[b_id]]$vertex_), 
                                         branch_ = ubranch[b_id]))) %>% mutate(segment_ = row_number())
}

# v_atts <- c("x_", "y_", "vertex_")
# o <- sph@data %>% mutate(object_ = row_number())
# map1 <- sptable(sph) %>% 
#   mutate(vertex_ = make_unq(map1[, setdiff(v_atts, "vertex_")]))
# b <- map1 %>% distinct(branch_, object_)
# segment <- make_segment_table(map1[, c("branch_", "vertex_")])
# bXs <- segment[, c("segment_", "branch_")]
# s <- segment[, c("segment_", "s1", "s2")]
# sXv <- segment_wl(s)
# v <- select_(map1, .dots = v_atts) %>% distinct(vertex_, .keep_all = TRUE)
# 
# x <- list(o = o, b = b, bXs = bXs, sXv = sXv, v = v)

plotx <- function(x) {
  y <- spbabel:::inner_cascade(x, tables = names(x))
  plot(y[, c("x_", "y_")])
  lapply(split(y, y$segment_), function(x) segments(x$x_[1], x$y_[1], 
                                                   x$x_[2], x$y_[2]))
  invisible(NULL)
}

# object <- dat1
# bXs <- distinct(segment, segment_, branch_)
# 
# 
# 
# ## all coords
# allcoords <- sptable(x)
# ## classify verts to unique
# allcoords$vertex_ <- make_unq(allcoords[, v_atts])
# allcoords$branch_ <- make_unq(allcoords[, "branch_"])
# segment <- make_segment_table(allcoords[, c("branch_", "vertex_")])
# library(tibble)
# library(tidyr)
# library(purrr)
# segments <- map1 %>% 
#   split(.$branch_) %>% 
#   map_df(branchV_to_segmentV)
# 
# mtcars %>%
#   split(.$cyl) %>%
#   map(~ lm(mpg ~ wt, data = .)) %>%
#   map(summary) %>%
#   map_dbl("r.squared")