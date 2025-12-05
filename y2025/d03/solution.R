options(scipen = 999)
x <- do.call(rbind, strsplit(readLines("input.txt"), ""))

mj <- function(bank, bats = 12) {
  joltage <- character(bats)
  max_ix  <- 0
  for(i in 1:bats) {
    choose_from <- 1:(length(bank) - (bats - i))
    max_ix     <- which.max(bank[choose_from])
    # can put early stopping here if(max_ix == max(choose_from))
    joltage[i] <- bank[choose_from][max_ix]
    bank[1:max_ix] <- "0"
  }

  as.numeric(paste(joltage, collapse = ""))
}

# part 1
sum(apply(x, 1, mj, bats = 2))

# part 2
sum(apply(x, 1, mj))
