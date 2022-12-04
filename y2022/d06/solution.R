input <- strsplit(readLines("input.txt"), "")[[1]]

find_marker <- function(input, chars) {
  marker <- FALSE
  i <- 0
  while(!marker) {
    i <- i + 1
    marker <- !anyDuplicated(input[i:(i + chars - 1)])
  }
  return(i + chars - 1)
}

# part 1
find_marker(input, 4)

# part 2
find_marker(input, 14)
