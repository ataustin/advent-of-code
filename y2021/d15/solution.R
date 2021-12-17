map <- as.matrix(read.fwf("input.txt", widths = rep(1, nchar(readLines("input.txt", n = 1)))))

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
    for(frame in frame_shift[c("right", "down", "left", "up")]) {
      after <- risk
      after[frame] <- risk[frame_shift$mid] + map[frame]
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
 