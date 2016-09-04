# The maps fill=FALSE output is the arcs (ala TopoJSON) between node vertices. 
# 
# should be able to find cycles by linking seqs and vertices, to reconstruct polys: 
#   
#   Verify with analogous plots in maps, sp, base, and ggplot2- with object comparisons, and maybe digests of the coords ??
# 



## logic to make this work is
## check length of seqs vs. length of names
## if length(unique(names)) is not equal to length(seqs) 
##    return LINES, with no attributes
## otherwise
##    returns SPoly with names as attribute on objects
## ignore part to object union, as that should happen on the arcs
#' @export
#' @rdname sp
#' @importFrom utils tail
sp.map <- function(x) {
  bad <- is.na(x$x)
  ## TODO check badx and bady are the same
  partsID <- cumsum(bad) + 1L
  
  ## bad drops all the polygon list bounds
  coords <- tibble::as_tibble(list(x_ = x$x, y_ = x$y, branch_ = partsID))[!bad, ]
  coords$object_ <- coords$branch_ 
  coords$island_ <- TRUE  ## hmm needs work
  coords$order_ <- seq(nrow(coords))
  
  length(unique(coords$branch_))
  partname <- x$names
  partname[duplicated(partname)] <- paste0(partname[duplicated(partname)], ":1")
  
  obj <- unlist(lapply(strsplit(partname, ":"), "[", 1L))
  partiden <- unlist(lapply(strsplit(x$names, ":"), tail, 1L))
  
  coords$object_ <- obj[coords$branch_]
  coords$branch_ <- partiden[coords$branch_]
  
  spbabel::sp(coords)         
}

## Possibly, can resurrect polygons from the arcs, but there's still no attributes
# library(maps)
# library(tibble)
# 
# 
# fcounty <- map("county", fill = TRUE, plot = FALSE)
# lcounty <- map("county", fill = FALSE, plot = FALSE)
# 
# str(fcounty)
# str(lcounty)
# 
# miss <- is.na(lcounty$x)
# seqs <- cumsum(miss)
# v <- tibble(x = lcounty$x, y = lcounty$y, group = seqs)[!miss, ]
# 
# ## non-unique instances of vertices
# ## with ide of original arc sequence
# vi <- v %>% mutate(.vx0 = as.integer(factor(paste(x, y, sep = "_")))) 
# 
# ## unique vertices
# vu <- vi %>% dplyr::select(x, y, .vx0) %>% distinct()
# vi <- vi %>% dplyr::select(.vx0, group) 
# 
# ## trace each group
# topbot <- vi %>% group_by(group) %>% slice(c(1, n()))
# 
# topbot %>% filter(.vx0 == 29897)
# non_uniques <- vi[duplicated(vi$.vx0), ]
# polys <- list()
# for (jj in unique(non_uniques$.vx0)) {
#   groups <- subset(non_uniques, .vx0 == jj)$group
#   xx <- vi %>% dplyr::filter(group %in% groups) %>% inner_join(vu) 
#   plot(xx$x, xx$y)
#   lapply(split(xx, xx$group), function(x) lines(x$x, x$y, col = "red"))
#   scan("", 1)
# }
# 
# 
# library(ggplot2)
# ggplot(v) + aes(x = x, y = y, group = group, col = group ) + geom_line() + 
#   scale_color_continuous(guide = FALSE)
# 
# distinct(v)


