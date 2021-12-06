input <- factor(scan("input.txt", sep = ","), levels = 0:8)

# helper function
count_fish <- function(input, days) {
  f <- as.numeric(table(input)) # frequency vector for each day
  for(i in seq(days)) {
    new_fish <- f[1]      # day 0 fish spawn and reset
    f[1:8]   <- f[2:9]    # other fish move closer to spawn
    f[7]     <- f[7] + new_fish  # add resets to day 6
    f[9]     <- new_fish  # spawns start on day 8
  }
  sum(f)
}

# challenge 1
count_fish(input, 80)

# challenge 2
options(scipen = 999) # to print large number
count_fish(input, 256)
