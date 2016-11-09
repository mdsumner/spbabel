
#' @export
#' @importFrom tibble as_tibble
map_table.sf <- function(x, ...) {
  
  tabmap <- sptable(x)
  ## why did this ever work?
  #  tabdat <- tibble::as_tibble(x)
  tabdat <- tibble::as_tibble(x) 
  ## remove this if sptable is updated
  tabdat$object_ <- id_n(nrow(tabdat))
  tabmap$object_ <- tabdat$object_[tabmap$object_]
  out <- map_table_From2(tabdat, tabmap)
  # no class or methods in spbabel for map_table()
  #class(out) <- c("map_table", "list")
  out
}


pmap_df <- function(x) {
  x <- as_tibble(x); 
  names(x)[1:2] <- c("x_", "y_")
  x[["branch_"]] <- cumsum(c(0, diff(x[["branch_"]]))) + x[["branch_"]] + x[["object_"]]
  x$island_ <- x$sub_index == 1; 
  x$sub_index <- NULL
  #x <- x %>% mutate(island_ = sub_index == 1, sub_index = NULL)
  x
  
}

#' @importFrom sf st_geometry
#' @importFrom tibble as_tibble
sptable.sf <- function(x) {
  g <- sf::st_geometry(x)
  
  gtab <- do.call(rbind,lapply(seq_along(g), 
         function(object_)  do.call(rbind, lapply(seq_along(g[[object_]]), 
                                    function(branch_) do.call(rbind, lapply(seq_along(g[[object_]][[branch_]]), 
                                                                               function(sub_index) cbind(g[[object_]][[branch_]][[sub_index]], object_, branch_, sub_index)))))))
  
  


  pmap_df(gtab)
}

