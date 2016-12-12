## ----eval=FALSE----------------------------------------------------------
#  library(dplyr)
#  db <- src_sqlite("data-raw/maps_county.sqlite3", create = FALSE)
#  db

## ----eval=FALSE----------------------------------------------------------
#  tbl(db, "geom")
#  
#  tbl(db, "metadata")

## ----eval=FALSE----------------------------------------------------------
#  meta <- tbl(db, "metadata")
#  
#  meta %>% filter(state == "wyoming", county %in% c("big horn", "johnson")) %>%
#    select(county, group) %>% inner_join(tbl(db, "geom"))

## ----eval=FALSE----------------------------------------------------------
#  ggplot(
#    meta %>% filter(state == "wyoming", county %in% c("big horn", "johnson")) %>%
#    select(county, group) %>% inner_join(tbl(db, "geom")) %>%
#      collect()
#  
#  ) + aes(x = long, y = lat, group = subregion, fill = subregion) + geom_polygon()

## ----eval=FALSE----------------------------------------------------------
#  ggplot(
#    meta %>% filter(state == "wyoming") %>%
#    select(county, group) %>% inner_join(tbl(db, "geom")) %>%
#      collect() %>% group_by(subregion) %>% mutate(labx = mean(long), laby = mean(lat))
#  
#  ) + aes(x = long, y = lat, group = subregion, fill = subregion, label = subregion) + geom_polygon() + geom_path() + geom_text(aes(x = labx, y = laby)) +
#    scale_fill_grey(start = 0.3, guide = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  library(spbabel)
#    tbl(db, "metadata") %>% filter(state == "wyoming") %>%
#    select(county, group) %>% inner_join(tbl(db, "geom")) %>%
#      collect() %>% transmute(x_ = long, y_ = lat, branch_ = group, object_ = group, order_ = row_number(), island_ = TRUE, county = subregion, state = region) %>% sp(crs = "+proj=longlat +ellps=WGS84")
#  

