#devtools::install_github("edzer/sfr")

library(sf)
## example data
cl1 = cbind(rnorm(3, 10), rnorm(3, 10))
cl2 = cbind(rnorm(5, 10), rnorm(5,  0))
cl3 = cbind(rnorm(7,  0), rnorm(7, 10))
mp = sp::SpatialMultiPoints(list(cl1, cl2, cl3))
mpdf = sp::SpatialMultiPointsDataFrame(list(cl1, cl2, cl3), data.frame(a = 1:3))

library(mapview)
lin <- st_as_sf(atlStorms2005)
pol <- st_as_sf(gadmCHE)
pt <- st_as_sf(breweries91)
mp <- st_as_sf(mpdf)

## matrix (or vector as matrix)
.mat <- function(x) {
  if (is.null(nrow(x))) {
    x <- matrix(x, nrow = 1L)
  }
  x
}
## matrix to tibble
.mat2df <- function(x) {
  tibble::as_tibble(.mat(unclass(x)))
}
## list to df
## marker denotes the first as the island

.Plist2df <- function(x) {
  dplyr::bind_rows(lapply(x, .mat2df), .id = "marker") %>% mutate(island_ = marker == "1", order_ = row_number)
}

.Llist2df <- function(x) {
  dplyr::bind_rows(lapply(x, .mat2df), .id = "marker") %>% mutate(order_ = row_number())
}
## tests

#dplyr::bind_rows(lapply(sf::st_geometry(pt), .mat2df), .id = "object_")
#dplyr::bind_rows(lapply(sf::st_geometry(mp), .mat2df), .id = "object_") %>% mutate(branch_ = row_number())
#dplyr::bind_rows(lapply(sf::st_geometry(pol), function(p) dplyr::bind_rows(lapply(p, .list2df), .id = "branch")), .id = "object") 
#dplyr::bind_rows(lapply(sf::st_geometry(lin), function(p) dplyr::bind_rows(lapply(p, .list2df), .id = "branch")), .id = "object") 

  ## the holes fail in ggplot2
  ##%>% ggplot() ) + aes(x = V1, y = V2, group = branch, fill = object) + spbabel:::geom_polygon()
sptable.sf <- function(x, ...) {
  if (inherits(st_geometry(x), "sfc_POINT")) {
    dplyr::bind_rows(lapply(sf::st_geometry(x), .mat2df), .id = "object_")
  }
  if (inherits(st_geometry(x), "sfc_MULTIPOINT")) {
    dplyr::bind_rows(lapply(sf::st_geometry(x), .mat2df), .id = "object_") %>% mutate(branch_ = row_number())
  }
  if (inherits(st_geometry(x), "sfc_LINESTRING")) {
    dplyr::bind_rows(lapply(sf::st_geometry(x), .mat2df), .id = "object_") %>% mutate(branch_ = row_number())
  }
  
  if (inherits(st_geometry(x), "sfc_MULTIPOLYGON")) {
    dplyr::bind_rows(lapply(sf::st_geometry(x), function(p) dplyr::bind_rows(lapply(p, .list2df), .id = "branch")), .id = "object") 
    
  }
  
}
map_table.sf <- function(x, ...) {
  dat <- x
  dat$geometry <- NULL
  map_table_From2(dat, sptable(x))
}


  