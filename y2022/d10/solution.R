input <- read.delim(text = readLines("input.txt"), sep = " ",
                    col.names = c("do", "val"), header = FALSE)

expand_cycles <- function(instruction) {
  if(instruction[1] == "noop") return(data.frame(do = "noop", val = 0))
  rbind(data.frame(do = "addx", val = 0), instruction)
}

instr <- do.call(rbind, apply(input, 1, expand_cycles))
instr$current_val <- head(c(1, cumsum(instr$val) + 1), -1)

# part 1
target_cycles <- seq(from = 20, to = 220, by = 40)
sum(target_cycles * instr$current_val[target_cycles])

# part 2
instr$crt_pos    <- rep(0:39, times = 6)
instr$sprite_min <- instr$current_val - 1
instr$sprite_max <- instr$current_val + 1

instr$lit <- 
  with(instr, ifelse(crt_pos <= sprite_max & crt_pos >= sprite_min, 1, 0))

image(matrix(instr$lit, nrow = 40, ncol = 6)[, 6:1])
