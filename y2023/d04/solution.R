input <- strsplit(gsub("^Card.*:\\s+", "", readLines("input.txt")), "\\s+")

divide_ix  <- which(input[[1]] == "|")
win_num_ix <- 1:(divide_ix - 1)
card_ix    <- (divide_ix + 1):length(input[[1]])

wins   <- sapply(input, function(x) length(intersect(x[win_num_ix], x[card_ix])))
points <- 2 ^ (wins - 1) * (wins > 0)

# part 1
sum(points)

# part 2
total <- rep(1, length(wins))

for(i in seq_along(wins)) {
  this_win <- wins[i]
  if(this_win > 0) {
    next_wins_ix <- (i + 1):(i + this_win)
    total[next_wins_ix] <- total[next_wins_ix] + total[i]
  }
}

sum(total)
