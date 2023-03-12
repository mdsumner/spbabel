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
id_n <- function(x = 1, n = 10L) {
  replicate(x, idmaker(n))
}

