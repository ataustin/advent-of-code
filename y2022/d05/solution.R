N <- 8
crates <- read.fwf("input.txt", widths = rep(4, 9), n = N)
crates <- lapply(crates, function(col) gsub("[^A-Z]", "", col))
crates <- lapply(crates, function(vec) rev(vec[vec != ""]))

moves <- read.table(text = gsub("move|from|to", ",", readLines("input.txt")),
                  sep = ",", strip.white = TRUE, skip = N + 2,
                  col.names = c("", "count", "from", "to"))

move_crates <- function(crates, move, mover_fun) {
  from_stack <- crates[[move$from]]
  crates[[move$from]] <- head(from_stack, length(from_stack) - move$count)
  crates[[move$to]]   <- c(crates[[move$to]], mover_fun(tail(from_stack, move$count)))
  crates
}

solve <- function(crates, moves, mover_fun) {
  for(i in 1:nrow(moves)) crates <- move_crates(crates, moves[i, ], mover_fun)
  paste(sapply(crates, tail, 1), collapse = "")
}

# part 1
p1_crates <- crates
solve(p1_crates, moves, rev)

# part 2
p2_crates <- crates
solve(p2_crates, moves, identity)
