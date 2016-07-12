# #devtools::install_github("mdsumner/manifoldr")
#
# library(manifoldr)
# drawing <- DrawingA("data-raw/branched.map", "Drawing")
# library(spbabel)
# holy_poly <- sptable(drawing)
# devtools::use_data(holy_poly, compress = "bzip2")


library(rgdal)
drawing <- readOGR("data-raw", "mybranch")
library(spbabel)
holey <- sptable(drawing)
devtools::use_data(holey, compress = "bzip2")
