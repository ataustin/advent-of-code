x <- 1 * (do.call(rbind, strsplit(readLines("input.txt"), "")) == "@")

count <- function(input) {
  mat  <- cbind(0, rbind(0, input, 0), 0)
  sums <- matrix(0, nrow(input), ncol(input))
  for(i in (-1):1) {
    for(j in (-1):1) {
      if(i == 0 && j == 0) next()
      rows <- (2 + i):(nrow(mat) + i - 1)
      cols <- (2 + j):(ncol(mat) + j - 1)
      sums <- sums + mat[rows, cols]
    }
  }
  sums[input == 0] <- Inf
  sums
}

# part 1
remove_ct <- sum(count(x) < 4)
remove_ct

# part 2
remove <- remove_ct
while(remove_ct > 0) {
  x[count(x) < 4] <- 0
  remove_ct       <- sum(count(x) < 4)
  remove          <- remove + remove_ct
}

remove
