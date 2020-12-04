find_prod <- function(input, n_combs) {
  combs  <- combn(input, n_combs)
  sum_ix <- which(colSums(combs) == 2020)
  prod(combs[, sum_ix])
}

x <- as.integer(readLines("input.txt", warn = FALSE))

# challenge 1
find_prod(x, 2)

# challenge 2
find_prod(x, 3)