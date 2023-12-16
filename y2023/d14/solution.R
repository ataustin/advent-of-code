input <- as.matrix(read.fwf("input.txt",
                            widths = rep(1, nchar(readLines("input.txt", n = 1))),
                            comment.char = ""))

btw <- function(x, range) x > range[1] & x < range[2]

move_column <- function(col) {
  col   <- c("#", col, "#")
  round <- which(col == "O")
  cube  <- which(col == "#")
  dot   <- which(col == ".")
  for(i in seq_along(cube)) {
    cube_range <- c(cube[i], cube[i + 1])
    o_count <- sum(btw(round, cube_range))
    ._count <- sum(btw(dot, cube_range))
    if(!o_count) next()
    new_o_start <- cube[i] + 1
    new_o_range <- new_o_start:(new_o_start + o_count - 1)
    new_._range <- (max(new_o_range) + 1):(cube_range[2] - 1)
    col[new_o_range] <- "O"
    if(._count) col[new_._range] <- "."
  }
  col[2:(length(col) - 1)]
}

calculate_load <- function(mat) {
  sum(rowSums(mat == "O") * (nrow(mat):1))
}

tilt_up <- function(mat) apply(mat, 2, move_column)

# part 1
calculate_load(tilt_up(input))


# part 2
rotate <- function(mat) t(mat)[, ncol(mat):1]

platform <- input
cycles   <- 500
burn_in  <- 150

# arrangements are cyclical so we do a small number to find the pattern
for(i in 1:cycles) {
  for(direction in 1:4) {
    platform <- tilt_up(platform)
    platform <- rotate(platform)
  }
  if(i == burn_in) {
    # after enough cycles (a "burn-in" period) we capture a snapshot
    reference <- platform
  }
  if(i > burn_in) {
    # and when we're back to the same arrangement, we've identified the pattern
    if(all(platform == reference)) break()
  }
}

# math inspired by @riinu@mastodon.scot
period <- i - burn_in
more_cycles <- burn_in + period + ((1000000000 - burn_in) %% period) - i

for(i in 1:more_cycles) {
  for(direction in 1:4) {
    platform <- tilt_up(platform)
    platform <- rotate(platform)
  }
}

calculate_load(platform)
