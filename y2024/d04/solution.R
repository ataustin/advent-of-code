width <- nchar(readLines("input.txt", n = 1))
input <- as.matrix(read.fwf("input.txt", rep(1, width)))
page  <- cbind(".", rbind(".", input, "."), ".")

# part 1
# treat each row, column, and diagonal as a vector
# and find XMAS forward and backward
count_string <- function(row) {
  char <- paste(row, collapse = "")
  sum(gregexec("XMAS", char)[[1]] > 0) +
    sum(gregexec("SAMX", char)[[1]] > 0)
}

get_diags <- function(mat) {
  c(split(mat, row(mat) - col(mat)),
    split(mat, row(mat) + col(mat)))
}

sum(apply(page, 1, count_string)) +
  sum(apply(page, 2, count_string)) +
  sum(sapply(get_diags(page), count_string))


# part 2
# identify the A's and look diagonally for MAS
is_xmas <- function(a_coord, mat) {
  d1      <- rbind(a_coord - 1, a_coord, a_coord + 1)
  letters <- c(paste(mat[d1], collapse = ""),
               paste(mat[cbind(d1[, 1], rev(d1[, 2]))], collapse = ""))
  all(grepl("MAS|SAM", letters))
}

a_coords <- which(page == "A", arr.ind = TRUE)
sum(apply(a_coords, 1, is_xmas, page))