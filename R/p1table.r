recompose <- function(x, rcnames = c("v", "bXv", "b")) {
  y <- x[[rcnames[1L]]]
  if (length(rcnames) > 1) {
    for (i in seq_along(rcnames)[-1L]) {
      y <- inner_join(y, x[[rcnames[i]]])
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
  for (i in seq(nrow(x$o))) {
    obj <- x$o %>% select(object_) %>% slice(i)  %>% inner_join(x$b, "object_") %>% inner_join(x$bXv, "branch_") %>% inner_join(x$v, "vertex_")
    br[[i]] <- bind_rows(lapply(split(obj, obj$branch_), function(a) bind_cols(pairs0(a$vertex_), tibble(branch_ = head(a$branch_, -1)))))
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