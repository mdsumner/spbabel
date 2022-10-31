library(maptools)
data(wrld_simpl)
.wrld_simpl <- wrld_simpl
usethis::use_data(.wrld_simpl, internal = TRUE, compress = "xz")
