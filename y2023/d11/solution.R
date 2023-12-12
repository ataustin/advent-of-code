input <- as.matrix(read.fwf("input.txt", comment.char = "",
                            widths = rep(1, nchar(readLines("input.txt", n = 1)))))


get_total_dist <- function(observations, expansion = 1) {
  is_galaxy  <- observations == "#"
  expansion_row <- (rowSums(is_galaxy) == 0) * expansion
  expansion_col <- (colSums(is_galaxy) == 0) * expansion
  
  row_offset <- setNames(cumsum(expansion_row), 1:length(expansion_row))
  col_offset <- setNames(cumsum(expansion_col), 1:length(expansion_col))
  
  galaxy_coord <- which(is_galaxy, arr.ind = TRUE)
  expanded_row <- galaxy_coord[, 1] + row_offset[galaxy_coord[, 1]]
  expanded_col <- galaxy_coord[, 2] + col_offset[galaxy_coord[, 2]]
  
  expanded_coord <- cbind(expanded_row, expanded_col)
  sum(dist(expanded_coord, "manhattan"))
}

# part 1
get_total_dist(input)

# part 2
options(scipen = 999)
get_total_dist(input, 1000000 - 1)
