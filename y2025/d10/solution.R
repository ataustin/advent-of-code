x <- readLines("input.txt")

# helpers
extract <- function(string, pattern) {
  unlist(regmatches(string, gregexpr(pattern, string)))
}

make_buttons <- function(schematics, ncol) {
  spl <- strsplit(schematics, ",")
  int <- lapply(spl, \(s) as.integer(s) + 1)
  mat <- matrix(0, nrow = length(schematics), ncol = ncol)
  crd <- cbind(rep(1:length(int), times = lengths(int)), unlist(int))

  mat[crd] <- mat[crd] + 1
  mat
}

vec_to_int <- function(vec, base = 2) {
  strtoi(paste(vec, collapse = ""), base)
}

press <- function(current, button_ints) {
  unique(outer(current, button_ints, bitwXor))
}


# part 1
press_vec <- numeric(length(x))

for(i in seq_along(x)) {
  machine    <- x[i]
  target     <- ifelse(extract(machine, "[\\.#]") == "#", 1, 0)
  schematics <- gsub("\\(|\\)", "", extract(machine, "\\([0-9,]*\\)"))

  buttons <- make_buttons(schematics, length(target))
  b_ints  <- apply(buttons, 1, vec_to_int)
  t_int   <- vec_to_int(target)

  presses <- 0
  results <- vec_to_int(rep(0L, ncol(buttons)))  # initial value
  while(!any(results == t_int)) {
    results <- press(results, b_ints)
    presses <- presses + 1
  }

  press_vec[i] <- presses
}

sum(press_vec)
