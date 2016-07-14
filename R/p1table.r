recompose <- function(x, rcnames = c("v", "bXv", "b"), clnames = NULL) {
  y <- x[[rcnames[1L]]]
  if (length(rcnames) > 1) {
    for (i in seq_along(rcnames)[-1L]) {
      if (is.null(clnames)) { 
        y <- inner_join(y, x[[rcnames[[i]]]])
      } else {
        y <- inner_join(y, x[[rcnames[i]]], clnames[i-1])
      }
    }
  }
  y
}

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
sp_p1 <- function(x, ...) {
  ## re-compose to just two tables
  tab <- recompose(x, c("v", "bXv", "b"))
  sp(tab, attr_tab = x$o)
}

#' Multiple tidy tables
#' 
#' Creates a set of tidy tables from input objects. 
#'
#' 
#' @param x object to tidy
#' @param ... arguments passed to methods
#'
#' @return list of tibbles
#' @export
#'
#' @examples
#' data(holey)
#' spholey <- sp(holey)
#' mtable(spholey)
p1table <- function(x, ...) {
  UseMethod("p1table")
}

#' @export
p1table.default <- function(x, ...) {
  x$s <- p1tableFromM(x)
  x
}

#' @importFrom dplyr bind_cols bind_rows select slice inner_join tibble
p1tableFromM <- function(x) {
  br <- vector("list", nrow(x$o))
  mtab <- spbabel:::recompose(x, clnames = c("vertex_", "branch_"))
  for (i in seq(nrow(x$o))) {
    obj <- x$o %>% dplyr::select(object_) %>% 
      dplyr::slice(i)  %>% 
      dplyr::inner_join(mtab)
    br[[i]] <- dplyr::bind_rows(lapply(split(obj, obj$branch_), 
                                       function(a) dplyr::bind_cols(pairs0(a$vertex_), tibble(branch_ = head(a$branch_, -1)))))
  }
  bind_rows(br)
} 

#' @importFrom tibble tibble
pairs0 <- function (x) {
  tibble(s0 = head(x, -1), s1 = tail(x, -1))
}
#' @importFrom dplyr inner_join transmute 
seg2struct <- function(x) {
  v <- x$v
  v$i <- seq(nrow(v))
  s <- x$s
  s %>% inner_join(v, c("s0" = "vertex_")) %>% 
    transmute(s1, i0 = i) %>% 
    inner_join(v, c("s1" = "vertex_")) %>% 
    transmute(i0, i1 = i) 
}