x <- setNames(read.csv("input.txt", header = FALSE)[2:1],
              c("row", "col"))

## part 1
a <- outer(x[, 1], x[, 1], \(x, y) abs(x - y) + 1)
b <- outer(x[, 2], x[, 2], \(x, y) abs(x - y) + 1)
max(a*b)

## part 2
x$dr <- c(diff(x$row), x$row[1] - x$row[nrow(x)])  # change in row number
x$dc <- c(diff(x$col), x$col[1] - x$col[nrow(x)])  # change in col number

# get all integer coordinates
coord_list <- lapply(1:nrow(x), \(i)
      cbind(x$row[i]:(x$row[i] + x$dr[i]), x$col[i]:(x$col[i] + x$dc[i]))
)

coords <- setNames(as.data.frame(do.call(rbind, coord_list)),
                   c("row", "col"))[2:1]
reds   <- coords[duplicated(coords), ]

# visual inspection reveals the way to approach the problem
plot(coords, type = "l")
points(reds, pch = 20, col = rainbow(nrow(reds)))

# find candidate red tiles, accounting for the use of diff()
extreme_ix <- which(x$dc %in% range(x$dc))
candidates <- x[c(extreme_ix[1] + 1, extreme_ix[2]), c("col", "row")]

# find candidate areas -- based on plot, there can be only two
get_area <- function(candidate, row_dir, coords, reds) {
  upper_bound <- coords[coords$col == candidate$col &
                          row_dir(coords$row, candidate$row), ]
  l_bounds <- reds[reds$row <= upper_bound$row &
                     reds$col < (upper_bound$col / 10), ]

  row  <- max(l_bounds$row)
  col <- min(l_bounds$col[l_bounds$row == row])

  prod(abs(candidate - c(col, row)) + 1)
}

max(get_area(candidates[1, ], `>`, coords, reds),
    get_area(candidates[2, ], `<`, coords, reds))
