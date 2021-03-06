sfzoo <- function() {
  
  x <- st_point(c(1,2))
  
  p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
  mp <- st_multipoint(p)
  s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  ls <- st_linestring(s1)
  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
  s3 <- rbind(c(0,4.4), c(0.6,5))
  mls <- st_multilinestring(list(s1,s2,s3))
  p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
  p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
  pol <-st_polygon(list(p1,p2))
  p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
  p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
  p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
  mpol <- st_multipolygon(list(list(p1,p2), list(p3,p4), list(p5)))
  #gc <- st_geometrycollection(list(mp, mpol, ls))
  
  list(point = x, multipoint = mp, linestring = ls, multilinestring = mls, polygon = pol, 
       multipolygon = mpol)
}
sfgeomc <- function() {
  gc <- st_geometrycollection(setNames(sfzoo(), "geometrycollection"))
}


library(sf)
Zoo <- do.call(st_sfc, sfzoo())
GC <- st_sfc(sfgeomc())

zoodoo <- data.frame(x = seq_along(Zoo)); zoodoo[["geometry"]]  <- Zoo; zoodoo <- st_as_sf(zoodoo)
gcdoo <- data.frame(x = 1L); gcdoo[["geometry"]] <- GC; gcdoo <- st_as_sf(gcdoo)


gz <- rbind(zoodoo, gcdoo)

spbabel:::sf_g_apply(zoodoo, spbabel:::feature_table)
spbabel:::sf_g_apply(gcdoo, spbabel:::feature_table)
