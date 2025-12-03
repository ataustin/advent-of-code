x     <- readLines("input.txt")
moves <- as.integer(gsub("L", "-", gsub("R", "", x)))
stops <- cumsum(c(50, moves))   # representation of where dial stops

# Part 1
sum(stops %% 100 == 0)

# Part 2
zeros <- 0
for(i in 1:(length(stops)-1)) {
  move_past <- (stops[i]:stops[i + 1])[-1]  # don't double-count
  zeros     <- zeros + sum(move_past %% 100 == 0)
}
zeros
