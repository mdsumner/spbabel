## ----eval=TRUE-----------------------------------------------------------
library(maps)
## there's a long side-story here, that we ignore for now
amap <- map("world", plot = FALSE, resolution = 0, fill = TRUE)
map(amap, fill = TRUE)
str(amap)

## ------------------------------------------------------------------------
sum(is.na(amap$x))

## ------------------------------------------------------------------------
fortify.map <- function(x) {
  bad <- is.na(x$x)
  seq_ids <- cumsum(bad) + 1L
  data.frame(x = x$x, y = x$y, group = seq_ids, id = seq_ids)[!bad, ]
}
library(ggplot2)
amaptab <- fortify(amap) 

ggplot(amaptab) + aes(x = x, y = y, group = group, fill = group) + geom_polygon()

## ------------------------------------------------------------------------
## for now, it's easier to subset by re-reading
ant <- map("world", regions = "Antarctica", plot = FALSE, fill = TRUE)
ggplot(fortify(ant)) + aes(x = x, y = y, group = group, fill = group) + geom_polygon() + geom_path() 

## ------------------------------------------------------------------------
library(ggalt)
prj <- "+proj=laea +lat_0=-90 +ellps=WGS84 +lon_0=140"
ggplot(fortify(ant)) + aes(x = x, y = y, group = group, fill = group) + geom_polygon() + geom_path() + coord_proj(prj)

## ------------------------------------------------------------------------
library(maptools)
data(wrld_simpl)
ant2 <- subset(wrld_simpl, NAME == "Antarctica")
rownames(ant2) <- NULL
## note it's now called "long"/"lat" not "x"/"y"
ggplot(fortify(ant2)) + aes(x = long, y = lat, group = group, fill = group) + geom_polygon() + geom_path() + coord_proj(prj) + guides(fill = FALSE)

## ------------------------------------------------------------------------
## for now, it's easier to subset by re-reading
afr <- map("world", regions = c("South Africa", "Lesotho"), plot = FALSE, fill = TRUE)
ggplot(fortify(afr)) + aes(x = x, y = y, group = group, fill = group) + geom_polygon() + geom_path()

## ------------------------------------------------------------------------
## for now, it's easier to subset by re-reading
library(ggpolypath)
safr <- map("world", regions = "South Africa", plot = FALSE, fill = TRUE)
ggplot(fortify(safr)) + aes(x = x, y = y, group =group, col = factor(group)) + geom_polypath() + geom_path() 

## ------------------------------------------------------------------------
library(dplyr)
sp_afr <- fortify(afr) %>% 
  transmute(x_ = x, y_ = y, object_ = unlist(lapply(strsplit(afr$names[id], ":"), "[", 1L)), branch_ = id, 
         island_ = !grepl("hole", afr$names[id]), order_ = row_number()) %>% 
  spbabel::sp()

plot(sp_afr, col = rainbow(nrow(sp_afr)))
plot(sp_afr[2, ], col = "grey")

## ------------------------------------------------------------------------
library(spbabel)
wrld_geom <- sptable(wrld_simpl)
wrld_data <- as.data.frame(wrld_simpl) ## make clear that we need two tables

(sp_rtrip <- sp(wrld_geom, attr_tab = wrld_data))


## ------------------------------------------------------------------------
wrld_geom <- sptable(wrld_simpl)
wrld_data <- as.data.frame(wrld_simpl) ## make clear that we need two tables
wrld_prj <- proj4string(wrld_simpl)
(sp_rtrip <- sp(wrld_geom, attr_tab = wrld_data, crs = wrld_prj))


## ------------------------------------------------------------------------
wrld_geom <- sptable(wrld_simpl[9, ])
wrld_prj <- proj4string(wrld_simpl)
sp(wrld_geom, crs = wrld_prj)
wrld_geom$object_ <- wrld_geom$branch_
sp(wrld_geom, crs = wrld_prj)


## ----eval=FALSE----------------------------------------------------------
#  wrld <- wrld_simpl[9:10, ] ## pick two objects this time
#  sptable(wrld) <- sptable(wrld) %>% mutate(object_ = branch_)
#  wrld
#  

## ----eval=FALSE----------------------------------------------------------
#  library(dplyr)
#  library(tibble)
#  library(maps)
#  library(ggplot2)
#  db <- src_sqlite("data-raw/maps_county.sqlite3", create = TRUE)
#  mp <- map("county", plot = FALSE, fill = TRUE)
#  ## todo make sure the object ids are right
#  ## and that these are sp()-able - add interpreters for the fortify form (?)
#  geom <- copy_to(db, fortify(mp), name = "geom", temporary = FALSE)
#  ## I can be a bit old-fashioned
#  statename <- substr(mp$name, 1, unlist(gregexpr(",", mp$name)) - 1L)
#  countyname <- substr(mp$name, unlist(gregexpr(",", mp$name)) + 1, nchar(mp$name))
#  dat <- copy_to(db, tibble(county = countyname,
#                            state = statename, group = seq_along(statename)), name = "metadata", temporary = FALSE)
#  geom
#  dat
#  rm(geom, dat, db)

## ---- fig.width = 9, fig.height = 9--------------------------------------
allnumbers <- function(x) {x[] <- lapply(x, function(a) if(is.character(a)) as.integer(factor(a)) else as.numeric(a)); x}

oz <- subset(wrld_simpl, NAME == "Australia")
plot(allnumbers(fortify(oz)), pch = 19, cex = 0.2)

## ----eval=FALSE----------------------------------------------------------
#  library(dplyr)
#  library(tibble)
#  library(maps)
#  library(ggplot2)
#  db <- src_sqlite("data-raw/maps_county_nf.sqlite3", create = TRUE)
#  mp <- map("county", plot = FALSE, fill = TRUE)
#  

