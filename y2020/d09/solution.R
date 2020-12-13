input <- as.numeric(readLines("input.txt"))

# challenge 1
preamble_length <- 25
previous_ix <- 1:preamble_length
sums <- outer(input, input, `+`)
diag(sums) <- 0

input_valid <- logical(length(input) - preamble_length)

for(i in (preamble_length + 1):length(input)) {
  input_valid[i-preamble_length] <- any(sums[previous_ix, previous_ix] == input[i])
  previous_ix <- previous_ix + 1
}

first_invalid_ix  <- which(!input_valid)[1]
first_invalid_nbr <- input[-(1:preamble_length)][first_invalid_ix]
first_invalid_nbr


# challenge 2
found_sum <- FALSE
start <- 0

while(!found_sum) {
  start <- start + 1
  contiguous_sums  <- cumsum(input[start:(first_invalid_ix - 1)])
  has_matching_sum <- contiguous_sums == first_invalid_nbr

  if(any(has_matching_sum)) {
    found_sum <- TRUE
    end <- which(has_matching_sum)[1] - 1
  }
}

range <- input[start:(start + end)]
max(range) + min(range)