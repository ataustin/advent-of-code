input <- as.matrix(read.fwf("input.txt", widths = rep(1, nchar(readLines("input.txt", n = 1)))))

# challenge 1
get_rowwise_min_mat <- function(m) {
  mat_sd  <- apply(m, 1, function(row) sign(diff(row)))
  mat_dsd <- t(apply(mat_sd, 2, diff))
  min_mat <- cbind(mat_sd[1, ] == 1,             # TRUE if local min at left edge
                   mat_dsd == 2,                 # TRUE at interior local min
                   mat_sd[nrow(mat_sd), ] == -1  # TRUE if local min at right edge
  )
  min_mat
}

get_min_coords <- function(m) {
  min_rows   <- get_rowwise_min_mat(m)
  min_cols   <- t(get_rowwise_min_mat(t(m)))
  min_coords <- min_rows & min_cols
  min_coords
}

min_coords <- get_min_coords(input)
sum(input[min_coords] + 1)


# challenge 2
coord_steps <- function(coord) {
  row_coords <- (coord[1] - 1):(coord[1] + 1)
  col_coords <- (coord[2] - 1):(coord[2] + 1)
  coord_vec  <- c(row_coords, rep(coord[1], 3), rep(coord[2], 3), col_coords)
  coords     <- matrix(coord_vec, ncol = 2)
  coords
}

purge_coords <- function(coords, purge) {
  all_coords <- rbind(purge, coords, purge)  # purge twice to ensure purge is purged!
  all_coords[!duplicated(all_coords) & !duplicated(all_coords, fromLast = TRUE), ]
}

expand_boundary <- function(boundary, visited) {
  if(!nrow(boundary)) return(boundary)
  expand_list   <- lapply(1:nrow(boundary),
                          function(i) purge_coords(coord_steps(boundary[i, ]), visited))
  expand_coords <- do.call(rbind, expand_list)
  purge_coords(expand_coords[!duplicated(expand_coords), ], boundary)
}

# challenge 2 -- core function
basin_size <- function(seed, mat) {
  basin <- visited <- seed
  boundary <- purge_coords(coord_steps(seed), basin)
  while(length(boundary)) {
    in_basin <- mat[boundary] != 9
    basin    <- rbind(basin, boundary[in_basin, ])
    visited  <- rbind(visited, boundary)
    boundary <- expand_boundary(boundary[in_basin, , drop = FALSE], visited)
  }
  nrow(basin)
}


bordered_input <- cbind(9, rbind(9, input, 9), 9)  # so we never go out of bounds
seeds <- which(min_coords, arr.ind = TRUE) + 1     # because we padded the borders
sizes <- sapply(1:nrow(seeds), function(i) basin_size(seeds[i, ], bordered_input))
prod(sort(sizes, decreasing = TRUE)[1:3])
