input <- regmatches(readLines("input.txt"), gregexpr("-?\\d+", readLines("input.txt")))
input <- lapply(input, as.numeric)

get_prediction <- function(history, back = FALSE) {
  new <- history[if(back) 1 else length(history)]
  while(!all(history == 0)) {
    history <- diff(history)
    new     <- c(new, history[if(back) 1 else length(history)])
  }
  if(back) look_back(rev(new)) else sum(new)
}

look_back <- function(sequence) {
  result <- 0
  for(i in 2:length(sequence)) {
    result <- sequence[i] - result
  }
  result
}

# part 1
sum(sapply(input, get_prediction))

# part 2
sum(sapply(input, get_prediction, back = TRUE))
