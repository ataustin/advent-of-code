x <- as.matrix(read.fwf("input.txt", rep(1, 10)))

adjacent <- function(coord) {
  rows <- max(coord[1]-1, 1):min(coord[1]+1, 10)
  cols <- max(coord[2]-1, 1):min(coord[2]+1, 10)
  grid <- expand.grid(rows, cols)
  box  <- as.matrix(grid[!(grid$Var1 == coord[1] &
                           grid$Var2 == coord[2]), ])
  box
}

round <- 0
accumulator <- 0
sync <- FALSE

while(!sync) {
  round <- round + 1
  x     <- x + 1
  flash <- which(x > 9, arr.ind = TRUE)
  
  while(length(flash)) {
    x[flash] <- -Inf
    for(i in 1:nrow(flash)) {
      adjacent_coords    <- adjacent(flash[i, ])
      x[adjacent_coords] <- x[adjacent_coords] + 1
    }
    flash <- which(x > 9, arr.ind = TRUE)
  }
  
  total_flashes <- sum(is.infinite(x))
  if(round <= 100) accumulator <- accumulator + sum(total_flashes)
  x[is.infinite(x)] <- 0
  
  if(total_flashes == 100) sync <- TRUE
}

accumulator  # challenge 1
round        # challenge 2
