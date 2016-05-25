library(maptools)
data(wrld_simpl)
library(spbabel)
library(dplyr)
xtab <- sptable(wrld_simpl)

seindex <- dplyr::filter(xtab, x_ > 100 & y_ < 0) %>% distinct(object_)
seatt <- seindex %>% inner_join(as.data.frame(wrld_simpl) %>% mutate(object_ = row_number()))
semap <- xtab %>% filter(object_ %in% seatt$object_)

devtools::use_data(semap, overwrite = TRUE)
devtools::use_data(seatt, overwrite = TRUE)
