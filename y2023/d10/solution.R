# this works but is not well generalized for other inputs.
# it's super gnarly and needs extreme cleanup

map <- as.matrix(read.fwf("input.txt", widths = rep(1, nchar(readLines("input.txt", n = 1)))))

connectors <- list(
  up    = c("|", "F", "7"),
  down  = c("|", "J", "L"),
  left  = c("-", "L", "F"),
  right = c("-", "J", "7")
)


look <- list(
  down  = rbind(cbind(".", map, "."), ".", "."),
  up    = rbind(".", ".", cbind(".", map, ".")),
  right = cbind(rbind(".", map, "."), ".", "."),
  left  = cbind(".", ".", rbind(".", map, ".")),
  here  = cbind(".", rbind(".", map, "."), ".")
)


opposite <- function(dir) {
  if(dir == "up") "down" else if(dir == "down") "up" else if(dir == "left") "right" else "left"
}

can_connect <- function(dir, connectors, look, one_way = FALSE) {
  here <- opposite(dir)
  valid_connect <- (look[[dir]] %in% connectors[[dir]])
  if(!one_way) valid_connect <- valid_connect & (look$here %in% connectors[[here]])
  matrix(valid_connect, nrow = nrow(look[[dir]]))
}


has_connection <- setNames(lapply(names(connectors), can_connect, connectors, look),
                           names(connectors))

move_coord <- function(coord, dir) {
  coord + c((dir == "down") - (dir == "up"), -(dir == "left") + (dir == "right"))
}

start_pos   <- which(look$here == "S", arr.ind = TRUE)
coord_mat <- coord <- matrix(start_pos + c(0, 1), nrow = 1)
steps <- 1
came_from <- "left"
valid_moves <- TRUE

while(any(valid_moves)) {
  valid_moves <- sapply(has_connection, function(x) x[coord])
  valid_moves <- valid_moves[setdiff(names(valid_moves), came_from)]
  if(any(valid_moves)) {
    this_dir    <- names(valid_moves)[valid_moves]
    came_from   <- opposite(this_dir)
    coord       <- move_coord(coord, this_dir)
    coord_mat  <- rbind(coord_mat, coord)
  }
  steps       <- steps + 1
}


# part 1
steps/2

# part 2
# borrowing ideas from day 18
shoelace <- function(x, y) {
  sum(x * c(y[2:length(y)], y[1]))
}

get_interior <- function(x, y) {
  0.5 * abs(shoelace(x, y) - shoelace(y, x))
}

coord_mat <- rbind(start_pos, coord_mat, start_pos)
get_interior(coord_mat[, 1], coord_mat[, 2]) - steps/2 + 1

