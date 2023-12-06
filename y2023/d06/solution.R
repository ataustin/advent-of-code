input <- readLines("input.txt")
dat   <- as.data.frame(regmatches(input, gregexpr("\\d+", input)),
                       col.names = c("time", "distance"))
dat[] <- lapply(dat, as.integer)

count_wins <- function(row) {
  st    <- sqrt(row[1]^2 - 4*row[2])
  
  # want the number of integers *between* the two roots of the quadratic
  range <- c(floor((row[1] - st)/2),
             ceiling((row[1] + st)/2))
  diff(range) - 1
}

# part 1
prod(apply(dat, 1, count_wins))

# part 2
row <- as.numeric(gsub("[^0-9]", "", input))
count_wins(row)
