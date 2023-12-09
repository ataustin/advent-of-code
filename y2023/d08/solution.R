input   <- readLines("input.txt")
instr   <- strsplit(input[1], "")[[1]]
map_dat <- read.table(text = gsub("[^A-Z0-9]", " ", input[3:length(input)]))

node   <- map_dat[[1]]
l_dest <- match(map_dat[[2]], node)
r_dest <- match(map_dat[[3]], node)

# this links a starting node to the node index of the L or R move
dest   <- matrix(c(l_dest, r_dest), ncol = 2, dimnames = list(node, c("L", "R")))

get_steps <- function(start, target, instr, dest) {
  i    <- 0  # which instruction
  j    <- 0  # number of times through a whole instruction set
  while(!grepl(target, start)) {
    i <- i + 1
    if(i > length(instr)) {i <- 1; j <- j + 1}
    start <- rownames(dest)[dest[start, instr[i]]]
  }
  j * length(instr) + i
}

# part 1
get_steps("AAA", "ZZZ", instr, dest)

# part 2
options(scipen = 999)
starts <- grep("A$", node, value = TRUE)
steps  <- sapply(starts, get_steps, "Z$", instr, dest)

gcd_euclid <- function(x, y) {
  a <- max(x, y)
  b <- min(x, y)
  if(b == 0) return(a)
  gcd_euclid(b, a %% b)
}

lcm <- function(x, y) {
  x * y / gcd_euclid(x, y)
}

Reduce(lcm, steps)
