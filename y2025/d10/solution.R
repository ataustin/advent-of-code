library(lpSolve)
x <- readLines("input.txt")


# helpers
extract <- function(string, pattern) {
  # base version of str_extract
  unlist(regmatches(string, gregexpr(pattern, string)))
}

make_buttons <- function(schematics, ncol) {
  # turns schematics into matrix of 0/1, one row per button
  spl <- strsplit(schematics, ",")
  int <- lapply(spl, \(s) as.integer(s) + 1)
  mat <- matrix(0, nrow = length(schematics), ncol = ncol)
  crd <- cbind(rep(1:length(int), times = lengths(int)), unlist(int))

  mat[crd] <- mat[crd] + 1
  mat
}

bin_to_int <- function(vec, base = 2) {
  # turns a binary vector into an integer
  strtoi(paste(vec, collapse = ""), base)
}

flip <- function(current, button_ints) {
  # flips bits of current integer, using all buttons
  unique(outer(current, button_ints, bitwXor))
}


# computations
flip_cts <- jolt_cts <- numeric(length(x))

for(i in seq_along(x)) {
  # parse input
  machine    <- x[i]
  lights     <- ifelse(extract(machine, "[\\.#]") == "#", 1, 0)
  schematics <- gsub("\\(|\\)", "", extract(machine, "\\([0-9,]*\\)"))
  buttons    <- make_buttons(schematics, length(lights))

  # data for part 1
  button_int <- apply(buttons, 1, bin_to_int)
  light_int  <- bin_to_int(lights)

  flip_ct <- 0
  results <- bin_to_int(rep(0L, ncol(buttons)))  # initial value
  while(!any(results == light_int)) {
    results <- flip(results, button_int)
    flip_ct <- flip_ct + 1
  }
  flip_cts[i] <- flip_ct

  # data for part 2
  jolts      <- as.integer(extract(gsub("^.*\\{", "", machine), "[0-9]{1,3}"))
  t_buttons  <- t(buttons)

  obj <- rep(1, ncol(t_buttons))
  dir <- rep("=", nrow(t_buttons))
  res <- lp("min", obj, t_buttons, dir, jolts, int.vec = 1:length(obj))
  jolt_cts[i] <- res$objval
}


# part 1
sum(flip_cts)

# part 2
sum(jolt_cts)
