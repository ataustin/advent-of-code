input <- as.matrix(read.fwf("input.txt",
                            widths = rep(1, nchar(readLines("input.txt", n = 1)))))
start_coord <- which(input == "S", arr.ind = TRUE)
end_coord   <- which(input == "E", arr.ind = TRUE)

input[rbind(start_coord, end_coord)] <- c("a", "z")

heights <- t(apply(input, 1, match, letters))

make_shift <- function(mat) {
  frame <- matrix(TRUE, nrow = nrow(mat), ncol = ncol(mat))
  frame_shift <- list(right = cbind(FALSE, FALSE, rbind(FALSE, frame, FALSE)),
                      down  = rbind(FALSE, FALSE, cbind(FALSE, frame, FALSE)),
                      left  = cbind(rbind(FALSE, frame, FALSE), FALSE, FALSE),
                      up    = rbind(cbind(FALSE, frame, FALSE), FALSE, FALSE),
                      mid = cbind(FALSE, rbind(FALSE, frame, FALSE), FALSE)
  )
  
  bound_mat  <- cbind(Inf, rbind(Inf, mat, Inf), Inf)
  directions <- c("right", "down", "left", "up")
  shift_list <- setNames(vector(mode = "list", length = length(directions)),
                         directions)

  for(dir in directions) {
    shift_list[[dir]] <- c(bound_mat[frame_shift[[dir]]])
  }

  coords <- data.frame(row    = rep(1:nrow(heights), ncol(heights)),
                       col    = rep(1:ncol(heights), each = nrow(heights)),
                       height = c(mat))  # recycled
  
  all_shifts <- cbind(coords, stack(shift_list))

  all_shifts$change <- all_shifts$values - all_shifts$height
  allowed    <- all_shifts[all_shifts$change < 2, ]
  
  row_shifts <- ifelse(allowed$ind == "up", -1,
                       ifelse(allowed$ind == "down", 1, 0))
  col_shifts <- ifelse(allowed$ind == "left", -1,
                       ifelse(allowed$ind == "right", 1, 0))
  allowed$row_target <- allowed$row + row_shifts
  allowed$col_target <- allowed$col + col_shifts
  allowed
}


coords <- data.frame(row = rep(1:nrow(heights), ncol(heights)),
                     col = rep(1:ncol(heights), each = nrow(heights)))
x <- head(as.data.frame(make_shift(heights)))
shifts <- make_shift(heights)










# helper functions
smallest_risk <- function(map) {
  risk <- matrix(Inf, nrow = nrow(map), ncol = ncol(map))
  risk[1, 1] <- 0
  frame <- matrix(TRUE, nrow = nrow(map), ncol = ncol(map))
  frame_shift <- list(right = cbind(FALSE, FALSE, rbind(FALSE, frame, FALSE)),
                      down  = rbind(FALSE, FALSE, cbind(FALSE, frame, FALSE)),
                      left  = cbind(rbind(FALSE, frame, FALSE), FALSE, FALSE),
                      up    = rbind(cbind(FALSE, frame, FALSE), FALSE, FALSE),
                      mid = cbind(FALSE, rbind(FALSE, frame, FALSE), FALSE)
  )
  
  risk <- cbind(Inf, rbind(Inf, risk, Inf), Inf)
  map  <- cbind(Inf, rbind(Inf, map, Inf), Inf)
  
  continue <- TRUE
  while(continue) {
    before <- risk
    for(shift in frame_shift[c("right", "down", "left", "up")]) {
      after <- risk
      #after[shift] <- risk[frame_shift$mid] + map[shift]
      after[shift] <- risk[frame_shift$mid] + map[shift]
      risk <- ifelse(risk > after, after, risk)  # element-wise minimum
    }
    continue <- any(risk != before)
  }
  
  risk[nrow(risk) - 1, ncol(risk) - 1]  # the ending cell accounting for border
}

# part 1
smallest_risk(map)

# part 2
map_row <- cbind(map, map + 1, map + 2, map + 3, map + 4)
map5    <- rbind(map_row, map_row + 1, map_row + 2, map_row + 3, map_row + 4)
map5[map5 > 9] <- map5[map5 > 9] - 9

smallest_risk(map5)