b_undle <- function(p2, cls) {
  bind_rows(lapply(p2, function(p3) as_tibble(setNames(as.data.frame(p3), cls))), .id = "island_")
}
#xx <- bind_rows(lapply(sf::st_geometry(x), function(p1) bind_rows(lapply(p1, b_undle, cls = .xyzmNames(class(p1)[1])))), .id = "object_") %>% 
#  mutate(branch_ = paste(object_, island_, sep = "_"), island_ = island_ == "1", order_ = row_number())

#ss <- spbabel::sp(xx)
#plot(ss, col = sample(viridis::viridis(nrow(ss))))


## matrix (or vector as matrix)
.mat <- function(x) {
  if (is.null(nrow(x))) {
    x <- matrix(x, nrow = 1L)
  }
  x
}

.xyzmNames <- function(x) {
  names <- c(X = "x_", Y = "y_", Z = "z_", M = "m_")
  names[unlist(strsplit(x, ""))]
}
## matrix to tibble
.mat2df <- function(x) {
  
  #cnames <- class(x)[1L]
  
  #  setNames(tibble::as_tibble(.mat(unclass(x))), .xyzmNames(cnames))
  setNames(tibble::as_tibble(.mat(unclass(x))), c("x", "y", "z", "m")[seq(ncol(x))])
  #tibble::as_tibble(.mat(unclass(x)))
}

mt_sf <- function(x, ...) {
  dat <- x
  dat[[attr(x, "sf_column")]] <- NULL
  dat <- tibble::as_tibble(dat)
  dat$object_ <- as.character(nrow(dat))
  xx <- bind_rows(lapply(sf::st_geometry(x), function(p1) bind_rows(lapply(p1, b_undle, cls = .xyzmNames(class(p1)[1])))), .id = "object_") %>% 
    mutate(branch_ = paste(object_, island_, sep = "_"), island_ = island_ == "1", order_ = row_number())
  
  spbabel:::map_table_From2(dat, xx)
}


mt <- mt_sf(x)

