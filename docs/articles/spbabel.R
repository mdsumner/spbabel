## ---- echo = FALSE,message=FALSE-----------------------------------------
library(dplyr)
library(maptools)
library(ggplot2)

## ------------------------------------------------------------------------
library(maptools)
data(wrld_simpl)
library(spbabel)
(oz <- subset(wrld_simpl, NAME == "Australia"))

## long-form encoding of objects
oztab <- sptable(oz)

##  make a copy to modify
woz <- oz
library(dplyr)
## modify the geometry on this object without separating the vertices from the objects
sptable(woz) <- sptable(woz) %>% mutate(x_ = x_ - 5)

# plot to compare 
plot(oz, col = "grey")
plot(woz, add = TRUE)

## ------------------------------------------------------------------------

pp <- sptable(wrld_simpl %>% subset(NAME == "Japan" | grepl("Korea", NAME)))
## explode (or "disaggregate"") objects to individual polygons
## here each branch becomes an object, and each object only has one branch
## (ignoring hole-vs-island status for now)
wone <- sp(pp %>% mutate(object_ = branch_), crs = attr(pp, "crs"))
op <- par(mfrow = c(2, 1), mar = rep(0, 4))
plot(sp(pp), col = grey(seq(0.3, 0.7, length = length(unique(pp$object)))))
plot(wone, col = sample(rainbow(nrow(wone), alpha = 0.6)), border = NA)
par(op)

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(pp) + aes(x = x_, y = y_, fill = factor(object_), group = branch_) + geom_polygon()
## resample the branch_ IDs to mix up the colours so that 
## object disaggregation is more apparent
set.seed(10)
ggplot(pp) + aes(x = x_, y = y_, fill = factor(sample(branch_)), group = branch_, col = object_) + geom_polygon()

## ------------------------------------------------------------------------
codes <- c("AUS",  "SLB",  "FJI", "FSM", "KIR", 
"NCL", "NFK", "CCK", "CXR", "VUT", "NRU",  "PNG", 
"SGP", "TUV", "IDN", "TLS", "PLW", "MHL", "NZL")

mm <- map_table(wrld_simpl %>% subset(ISO3 %in% codes))

## cross the dateline properly
mm$v <- mm$v %>% mutate(x_ = ifelse(x_ < 0, x_ + 360, x_))
plot(mm$v[, c("x_", "y_")])

## get an sp object
## cascaded inner join and arrange within branch
ctable <- with(mm, 
       inner_join(v, bXv) %>%  inner_join(b) %>% 
         arrange(branch_, order_) %>% 
         dplyr::select(-vertex_))

mm_sp <- sp(ctable, 
            attr_tab = mm$o)

mm_sp
plot(mm_sp, col = grey(seq(0, 1, length = nrow(mm_sp))))

## ---- fig.height=9, fig.width=9------------------------------------------
## get a gg plot

ggplot(ctable %>% inner_join(dplyr::select(mm$o, NAME, object_))) + 
  aes(x = x_, y = y_, group = branch_, fill = NAME) + 
  geom_polygon() + ggplot2::coord_equal()


