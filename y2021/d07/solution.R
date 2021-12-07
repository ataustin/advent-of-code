input <- scan("input.txt", sep = ",")

# helper functions
cost1 <- function(start, end) sum(abs(start - end))
cost2 <- function(start, end) sum((abs(start-end)+1) * abs(start-end)/2)

# we will compare run times of brute-force and optimization approaches
brute_force <- function(input, cost_fn) {
  cost <- sum(input^2)   # guaranteed to be too large
  for(pos in min(input):max(input)) {
    new_cost <- cost_fn(input, pos)
    if(new_cost < cost) cost <- new_cost
  }
  cost
}

optimization <- function(input, cost_fn) {
  opt <- optimize(cost_fn, range(input), start = input)
  min(cost_fn(input, floor(opt$minimum)),  # check integer solutions only
      cost_fn(input, ceiling(opt$minimum)))
}

# challenge solutions
optimization(input, cost1)
optimization(input, cost2)

# benchmarking -- note esp. difference in memory allocation!
bench::mark(brute_force(input, cost2),
            optimization(input, cost2))
