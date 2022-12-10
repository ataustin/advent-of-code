input <- read.table("input.txt", header = FALSE, col.names = c("dir", "ct"))

head_move     <- input
head_move$ct  <- ifelse(input$dir %in% c("R", "U"), input$ct, -input$ct)
head_move$dir <- ifelse(input$dir %in% c("R", "L"), "x", "y")

start_offset <- 1
head_pos <- data.frame(x = rep(0, nrow(input) + start_offset),
                       y = rep(0, nrow(input) + start_offset))

# make x,y movement instructions for each input line
for(i in 1:nrow(head_move)) {
  this_move <- head_move[i, ]
  head_pos[i + start_offset, this_move$dir] <- this_move$ct
}

# compute x,y coordinates after each input line
head_pos[] <- lapply(head_pos, cumsum)

# enumerate all single-step moves
expand_pos <- function(head_first, head_last) {
  head_x <- (head_first$x):(head_last$x)
  head_y <- (head_first$y):(head_last$y)
  tail(data.frame(x = head_x,
                  y = head_y),
       -1)
}

all_head_list  <- lapply(2:nrow(head_pos),
                         function(i) expand_pos(head_pos[i - 1, ], head_pos[i, ]))
all_head_moves <- do.call(rbind, all_head_list)

# tail positions placeholder
tail_pos <- data.frame(x = rep(0, nrow(all_head_moves)),
                       y = rep(0, nrow(all_head_moves)))

# helper: distance of tail from head
get_tail_dist <- function(head_pos, tail_pos) {
  x_dist <- head_pos$x - tail_pos$x
  y_dist <- head_pos$y - tail_pos$y
  c(x_dist, y_dist)
}

# helper: convert a tail distance to a tail movement instruction
reduce_value <- function(dist) {
  large_dist <- which(abs(dist) > 1)
  dist[large_dist] <- sign(dist[large_dist])
  dist
}

# helper: update tail coords based on head position
move_tail <- function(head_pos, tail_pos) {
  tail_dist <- get_tail_dist(head_pos, tail_pos)
  if(all(abs(tail_dist) <= 1L)) return(tail_pos)
  tail_pos + reduce_value(tail_dist)
}


# part 1: for each head movement, figure out where tail belongs
for(i in 2:nrow(all_head_moves)) {
  tail_pos[i, ] <- move_tail(all_head_moves[i, ], tail_pos[i - 1, ])
}

nrow(unique(tail_pos))


# part 2: previous iteration's tail data becomes next interation's head data
system.time(
for(rope in 2:9) {
  all_head_moves <- tail_pos
  for(i in 2:nrow(all_head_moves)) {
    tail_pos[i, ] <- move_tail(all_head_moves[i, ], tail_pos[i - 1, ])
  }
}
)

nrow(unique(tail_pos))
