options(scipen = 999)
x <- readLines("input.txt")

# helpers
extract <- function(string, pattern) {
  unlist(regmatches(string, gregexpr(pattern, string)))
}

# parse input
lines   <- lapply(x, extract, "[a-z]{3}")
devices <- sapply(lines, `[`, 1)
outputs <- setNames(sapply(lines, tail, -1), devices)

# generate connectivity matrix, rows = start node, cols = end node
elements <- c("you",
              setdiff(sort(unique(unlist(lines))), c("you", "out")),
              "out")
mat      <- matrix(0, nrow = length(elements), ncol = length(elements),
                   dimnames = list(elements, elements))
ix       <- cbind(gsub("[0-9]", "", names(unlist(outputs))),
                  unlist(outputs))
mat[ix]  <- 1

# use matrix powers to count all paths from one node to another
result <- mat
starts <- c("you", "svr", "fft", "dac")
ends   <- c("fft", "dac", "out")
counts <- matrix(0, nrow = length(starts), ncol = length(ends),
                 dimnames = list(starts, ends))
while(any(result > 0)) {
  result   <- result %*% mat
  counts[starts, ends] <- counts[starts, ends] + result[starts, ends]
}

# part 1
counts["you", "out"]

# part 2
prod(counts[cbind(tail(starts, -1), ends)])
