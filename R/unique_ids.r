## internal


# letters and numbers
letsnumbers <- function() {
  c(letters, LETTERS, 0:9)
}


## sample and paste letters and numbers together
idmaker <- function(n = 10L) {
  paste(sample(letsnumbers(), n, replace = TRUE)  , collapse = "")
}


## create n unique ids
id_n <- function(x = 1, n = 5L) {
  replicate(x, idmaker(n))
}


## reclassify input ids
# id_rec <- function(x) {
#   x <- factor(x)
#   levels(x) <- id_n(nlevels(x))
#   levels(x)[x]
# }
# 
# 
# make_unq <- function(x) {
#   id_rec(do.call(paste, as.list(x)))
# }