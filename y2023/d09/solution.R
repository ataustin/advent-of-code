input <- regmatches(readLines("input.txt"), gregexpr("-?\\d+", readLines("input.txt")))
input <- lapply(input, as.numeric)

get_prediction <- function(history) {
  new <- history[length(history)]
  while(!all(history == 0)) {
    history <- diff(history)
    new     <- c(new, history[length(history)])
  }
  sum(new)
}

# part 1
sum(sapply(input, get_prediction))

# part 2
sum(sapply(input, function(x) get_prediction(rev(x))))
