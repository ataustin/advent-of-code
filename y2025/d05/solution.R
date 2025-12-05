options(scipen = 999)
x         <- readLines("input.txt")
ranges    <- grep("[0-9]-", x, value = TRUE)
ids       <- as.numeric(setdiff(x, c(ranges, "")))
range_dat <- matrix(as.numeric(unlist(strsplit(ranges, "-"))), ncol = 2, byrow = TRUE)

btw <- function(v, range) {
  v >= range[1] && v <= range[2]
}

# part 1
fresh <- apply(range_dat, 1, function(r) sapply(ids, btw, range = r))
sum(rowSums(fresh) > 0)

# part 2
range_dat <- range_dat[order(range_dat[, 1]), ]  # order by range start
i <- 1

while(i < nrow(range_dat)) {
  # we'll inspect these ranges in this iteration
  new_ranges <- range_dat[i:nrow(range_dat), , drop = FALSE]

  # begin with known smallest start. find other starts inside that range
  min_in_range <- sapply(new_ranges[, 1], btw, range_dat[i, ])

  # replace the current end value with the largest end value of all ranges
  range_dat[i, 2] <- max(new_ranges[min_in_range, 2])

  # we have a widened range.  we can eliminate redundant ranges
  range_dat <- rbind(range_dat[1:i, ], new_ranges[!min_in_range, ])

  # new widened range may now have new overlaps; account for this
  i <- if(btw(range_dat[i + 1, 1], range_dat[i, ])) i else i + 1
}

sum(range_dat[, 2] - range_dat[, 1]) + nrow(range_dat)
