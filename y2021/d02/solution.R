input <- read.delim("input.txt", sep = " ", header = FALSE,
                    col.names = c("direction", "amt"),
                    colClasses = c("character", "integer"))

calc_position <- function(dat, use_aim = FALSE) {
  # d_ for delta
  dat$d_horiz <- ifelse(dat$direction == "forward",
                        dat$amt, 0)
  dat$d_depth <- ifelse(dat$direction != "forward",
                        ifelse(dat$direction == "down",
                               dat$amt, -dat$amt),
                        0)
  
  if(use_aim) {
    dat$aim     <- cumsum(dat$d_depth)
    dat$d_depth <- dat$aim * dat$d_horiz
  }
  
  pos <- sum(dat$d_horiz) * sum(dat$d_depth)
  pos
}

# challenge 1
calc_position(input)

# challenge 2
calc_position(input, use_aim = TRUE)

