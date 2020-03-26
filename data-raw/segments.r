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


plotx <- function(x) {
  y <- inner_cascade(x, tables = names(x))
  plot(y[, c("x_", "y_")])
  lapply(split(y, y$segment_), function(x) segments(x$x_[1], x$y_[1],
                                                   x$x_[2], x$y_[2]))
  invisible(NULL)
}
