input <- read.fwf("input.txt", widths = rep(1, 12))

gamma      <- function(input) colMeans(input) >= 0.5
epsilon    <- function(input) !gamma(input)
bin_to_dec <- function(bin) sum(2^((length(bin):1) - 1) * bin)


# challenge 1
bin_to_dec(gamma(input)) * bin_to_dec(epsilon(input))


# challenge 2
get_rating <- function(input, PATTERN_FUN) {
  remaining <- input
  
  while(nrow(remaining) > 1) {  # stop when you've found the row
    pattern    <- as.integer(PATTERN_FUN(remaining))
    row_select <- remaining[, 1] == pattern[1]
    col_select <- if(length(pattern) == 1) 1 else -1
    remaining  <- remaining[row_select, col_select, drop = FALSE]
  }
  
  bin_to_dec(input[rownames(remaining), ])  # convert that row
}

# use gamma to get oxygen rating, epsilon to get CO2 rating
get_rating(input, gamma) * get_rating(input, epsilon)
