# challenge 1
input <- as.integer(readLines("input.txt", warn = FALSE))
sum(diff(input) > 0)


# challenge 2
sum(diff(input, lag = 3) > 0)
