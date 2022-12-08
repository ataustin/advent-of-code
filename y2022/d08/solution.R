input <- as.matrix(read.fwf("input.txt",
                            widths = rep(1, nchar(readLines("input.txt", n = 1)))))

clockwise <- function(x) t(apply(x, 2, rev))
counterclockwise <- function(x) apply(t(x), 2, rev)

# part 1
visible_from_left <- function(mat) {
  t(apply(mat, 1, function(x) x == cummax(x) & !duplicated(x)))
}

visibility_matrix <-
  visible_from_left(input) |
  clockwise(visible_from_left(counterclockwise(input))) |
  clockwise(clockwise(visible_from_left(clockwise(clockwise(input))))) |
  counterclockwise(visible_from_left(clockwise(input)))

sum(visibility_matrix)

# part 2
count_vis <- function(x) min(sum(cumsum(x[1] <= x) == 1), length(x) - 1)

tree_score_from_left <- function(mat) {
  count_mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  
  for(col in 1:(ncol(count_mat) - 1)) {
    count_mat[, col] <- apply(mat[, col:ncol(mat)], 1, count_vis)
  }
  
  count_mat
}

scenic_scores <-
  tree_score_from_left(input) *
  clockwise(tree_score_from_left(counterclockwise(input))) *
  clockwise(clockwise(tree_score_from_left(clockwise(clockwise(input))))) *
  counterclockwise(tree_score_from_left(clockwise(input)))

max(scenic_scores)
