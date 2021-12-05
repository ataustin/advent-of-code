# read data
draws <- as.integer(read.table("input.txt", nrows = 1, sep = ","))

boards <- read.table("input.txt", skip = 2, blank.lines.skip = TRUE)
boards <- split(boards, rep(1:(nrow(boards)/5), each = 5))

# helper functions
mark <- function(board, draw) {
  board[which(board == draw, arr.ind = TRUE)] <- (-1)
  board
}

win <- function(board) {
  any(colSums(board) == -5) || any(rowSums(board) == -5)
}

score <- function(board, draw) {
  (sum(board) + sum(board < 0)) * draw
}

# find winners in order; store their order and score
win_order <- win_score <- integer(length(boards))

for(draw in draws) {
  for(i in seq_along(boards)) {
    if(win_order[i] != 0) next()  # if this board has won, skip
    boards[[i]] <- mark(boards[[i]], draw)
    if(win(boards[[i]])) { # if this board wins, record order and score
      win_order[i] <- max(win_order) + 1
      win_score[i] <- score(boards[[i]], draw)
    }
  }
}

# challenge 1
win_score[win_order == 1]

# challenge 2
win_score[win_order == length(boards)] 
