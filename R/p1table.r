
# internal method that produces sptable() analog from mtable
cascade_inner <- function(x, rcnames = c("v", "bXv", "b")) {
  y <- x[[rcnames[1L]]]
  if (length(rcnames) > 1) {
    for (i in seq_along(rcnames)[-1L]) {
        y <- inner_join(y, x[[rcnames[[i]]]])
    }
  }
  y
}

# propagate a subset from objects down
cascade_semi <- function(x, y, rcnames = c("b", "bXv", "v")) {
  for (i in seq_along(rcnames)) {
    name <- rcnames[[i]]
    x[[name]] <- semi_join(x[[name]])
  }
  y
}

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
#' @noRd
#'
#' @examples
#' data(holey)
#' spholey <- sp(holey)
#' mtable(spholey)
p1table <- function(x, ...) {
  UseMethod("p1table")
}

#' @noRd
p1table.Spatial <- function(x, ...) {
  mlist <- mtable(x)
  br <- vector("list", nrow(tabdat))
  for (i in seq(nrow(tabdat))) {
    mobj <- cascade_semi(mlist, object_ == i) ## dplyr::inner_join(tabdat[i, "object_"], tabmap, "object_")
    br[[i]] <- dplyr::bind_rows(lapply(split(objmap, objmap$branch_), 
                                       function(a) dplyr::bind_cols(pairs0(a$vertex_), tibble(branch_ = head(a$branch_, -1)))))
  }
  bind_rows(br)
}

#' @noRd
p1table.default <- function(x, ...) {
  x$s <- p1tableFromM(x)
  x
}

#' @noRd
#' @importFrom dplyr bind_cols bind_rows select slice inner_join tibble
p1tableFromM <- function(x) {
  br <- vector("list", nrow(x$o))
  mtab <- cascade_inner(x)
  for (i in seq(nrow(x$o))) {
    obj <- x$o %>% dplyr::select(object_) %>% 
      dplyr::slice(i)  %>% 
      dplyr::inner_join(mtab)
    br[[i]] <- dplyr::bind_rows(lapply(split(obj, obj$branch_), 
                                       function(a) dplyr::bind_cols(pairs0(a$vertex_), tibble(branch_ = head(a$branch_, -1)))))
  }
  bind_rows(br)
} 

#' @noRd
#' @importFrom tibble tibble
pairs0 <- function (x) {
  tibble(s0 = head(x, -1), s1 = tail(x, -1))
}
#' @noRd
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