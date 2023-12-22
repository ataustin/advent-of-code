input <- setNames(read.table("input.txt", comment.char = ""),
                  c("dir", "ct", "color"))

moves <- data.frame(dir      = c("R", "L", "U", "D"),
                    dx       = c(1, -1, 0, 0),
                    dy       = c(0, 0, 1, -1),
                    dir_code = c(0, 2, 3, 1))

input$step <- 1:nrow(input)

build_border <- function(plan, moves) {
  border <- merge(plan, moves)
  border <- border[order(border$step), ]
  
  border$x <- cumsum(border$dx * border$ct)
  border$y <- cumsum(border$dy * border$ct)
  
  data.frame(x = c(0, border$x),
             y = c(0, border$y))
}

shoelace <- function(x, y) {
  sum(x * c(y[2:length(y)], y[1]))
}

get_interior <- function(x, y) {
  0.5 * abs(shoelace(x, y) - shoelace(y, x))
}

# part 1 -- account for width of border and the loop closing piece
border <- build_border(input, moves)
get_interior(border$x, border$y) + sum(input$ct) / 2 + 1

# part 2
input$dir_code <- substr(input$color, 8, 8)
input$hex_code <- substr(input$color, 3, 7)
input$ct       <- as.integer(as.hexmode(input$hex_code))
input$dir      <- NULL

border <- build_border(input, moves)

options(scipen = 999)
get_interior(border$x, border$y) + sum(input$ct) / 2 + 1