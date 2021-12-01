input <- readLines("input.txt", warn = FALSE)

# txt <- "L.LL.LL.LL
# LLLLLLL.LL
# L.L.L..L..
# LLLL.LL.LL
# L.LL.LL.LL
# L.LLLLL.LL
# ..L.L.....
# LLLLLLLLLL
# L.LLLLLL.L
# L.LLLLL.LL"

# x <- unlist(strsplit(txt, "\n"))
input <- do.call(rbind, strsplit(input, ""))


get_new_state <- function(occupation, seating, row, col, threshold) {
  this_seat  <- seating[row, col]
  if(this_seat == ".") return(".")

  seat_value <- compute_seat_value(occupation, row, col)

  if(this_seat == "L" && seat_value == 0) return("#")
  if(seat_value >= threshold) return("L")

  this_seat
}


# challenge 1
get_local_submatrix <- function(mat, row, col) {
  rows <- (row - 1):(row + 1)
  cols <- (col - 1):(col + 1)
  
  rows <- rows[rows > 0 & rows <= nrow(mat)]
  cols <- cols[cols > 0 & cols <= ncol(mat)]
  
  mat[rows, cols]
}


compute_seat_value <- function(mat, row, col) {
  sum(get_local_submatrix(mat, row, col)) - mat[row, col]
}

new <- input
current <- matrix("", nrow(new), ncol(new))

while(!all(current == new)) {
  current <- new
  is_occupied <- current == "#"

  for(i in 1:nrow(current)) {
    for(j in 1:ncol(current)) {
      new[i, j] <- get_new_state(is_occupied, current, i, j, 4)
    }
  }
}

sum(new == "#")


# challenge 2
get_local_submatrix(mat, row, col) {
  
}