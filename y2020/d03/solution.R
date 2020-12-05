compute_trees <- function(slope, input) {
  vertical_ix   <- seq(1, length(input), by = slope["down"])
  horizontal_ix <- c(1, 1 + (1:(length(vertical_ix) - 1)) * slope["over"])

  # grids repeat, so put the index in terms of a single grid
  grid_width   <- nchar(input[[1]])
  extra_grid   <- floor((horizontal_ix - 1) / grid_width)
  horizon_wrap <- horizontal_ix - (grid_width * extra_grid)
  positions    <- cbind(vertical_ix, horizon_wrap)

  grid_split  <- strsplit(input, split = "")
  grid_matrix <- do.call(rbind, grid_split)

  path <- grid_matrix[positions]
  sum(path == "#")
}


# challenge 1
input <- readLines("input.txt", warn = FALSE)
slope <- c(down = 1, over = 3)
compute_trees(slope, input)


# challenge 2
slopes <- data.frame(down = c(1, 1, 1, 1, 2),
                     over = c(1, 3, 5, 7, 1))

trees <- apply(slopes, 1, compute_trees, input = input)
prod(trees)

