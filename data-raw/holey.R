p1 <- cbind(x = c(0, 0, 0.75, 1,   0.5, 0.8, 0.69, 0), 
            y = c(0, 1, 1,    0.8, 0.7, 0.6, 0,    0))
p2 <- cbind(x = c(0.2, 0.2, 0.3, 0.5, 0.5, 0.2), 
            y = c(0.2, 0.4, 0.6, 0.4, 0.2, 0.2))
p3 <- cbind(x = c(0.2, 0.4, 0.4, 0.2), 
            y = c(0.8, 0.9, 0.8, 0.8))
library(sp)
holey <- SpatialPolygonsDataFrame(SpatialPolygons(list(
  Polygons(list(Polygon(p1 * 1.2 + 1)), "1"), 
  Polygons(list(Polygon(p1, hole  =FALSE), 
     Polygon(p2, hole = TRUE), 
     Polygon(p3, hole = TRUE)), "2"))), data.frame(x = 1:2))

devtools::use_data(holey)


