input <- scan("input.txt", sep = ",")

# helper functions
cost1 <- function(start, end) sum(abs(start - end))
cost2 <- function(start, end) sum((abs(start-end)+1) * abs(start-end)/2)

# we will compare run times of brute-force and optimization approaches
brute_force <- function(input, cost_fn) {
  cost <- Inf
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

# benchmarking
bench::mark(brute_force(input, cost2),
            optimization(input, cost2))

# plotting the cost
plot_data <- function(input, cost_fn, model) {
  positions <- min(input):max(input)
  costs     <- numeric(length(positions))
  for(i in seq_along(positions)) {
    costs[i] <- cost_fn(input, positions[i])
  }
  out <- data.frame(pos  = positions,
                    cost = costs)
  out$cost_norm <- 100 * out$cost / max(out$cost)
  out$model <- model
  out
}

pl1 <- plot_data(input, cost1, "Constant")
pl2 <- plot_data(input, cost2, "Additive")
pl  <- rbind(pl1, pl2)
pl$model <- factor(pl$model,
                   levels = c("Constant", "Additive"),
                   ordered = TRUE)

library(ggplot2)
ggplot(pl, aes(pos, cost_norm, color = model)) +
  geom_line(size = 1.3) +
  labs(x = "Final Position",
       y = "Relative Fuel Cost",
       title = "Fuel cost as a % of max of each solution",
       color = "Fuel Burn") +
  theme_minimal() +
  scale_color_manual(values = c("dodgerblue", "red"))
