x       <- do.call(rbind, strsplit(readLines("input.txt"), ""))
beam_ix <- which(x[1, ] == "S")
split   <- rbind((x[apply(x, 1, \(y) !all(y == ".")), ] == "^")[2:(nrow(x)/2), ],
                 FALSE)

# Setup
# initialize a matrix to indicate how many beams have passed a given point
ct_mat <- matrix(0, nrow(split), ncol(split))
ct_mat[1, beam_ix] <- 1

# at each step ("turn") track where the beams are
parent <- vector(mode = "list", length = nrow(split))
parent[[1]] <- data.frame(turn = 1,
                          beam = beam_ix)

for(turn in 1:(nrow(split) - 1)) {
  # splits and non-splits result in different behavior; separate them
  to_split  <- parent[[turn]]$beam %in% which(split[turn, ])
  par_split <- split(parent[[turn]], to_split)

  for(l in unique(to_split)) {
    # for each behavior, identify the new beams and their parents
    s <- get(as.character(l), par_split)
    new_beam <- if(l) c(s$beam - 1, s$beam + 1) else s$beam
    parent[[turn + 1]] <-
      rbind(parent[[turn + 1]],
            data.frame(turn     = turn + 1,
                       beam     = new_beam,
                       par_row  = turn,
                       par_col  = s$beam  # use recycling for l TRUE
                       ))
  }

  # a beam's counts are retrieved by looking up parent counts in the matrix
  turn_dat <- parent[[turn + 1]]
  turn_dat$par_sum <- ct_mat[as.matrix(turn_dat[c("par_row", "par_col")])]

  # when beams combine, we add their counts and update the matrix
  node_dat <- aggregate(par_sum ~ turn + beam, data = turn_dat, sum)
  ct_mat[as.matrix(node_dat[c("turn", "beam")])] <- node_dat$par_sum

  # this becomes the input for the next step
  parent[[turn + 1]] <- node_dat
}


# part 1: not all splits are used; find splits that have beams pass through
sum(split & ct_mat > 0)

# part 2: we've already found all the counts for all beams; just add them
sum(parent[[length(parent)]]$par_sum)
