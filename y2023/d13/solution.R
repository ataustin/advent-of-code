input <- strsplit(readChar("input.txt", file.info("input.txt")$size), "\n\n")[[1]]
lines <- lapply(input, function(x) strsplit(x, "\n")[[1]])
notes <- lapply(lines, function(x) matrix(unlist(strsplit(x, "")),
                                          ncol = nchar(x[1]),
                                          byrow = TRUE) == "#")

reflects <- function(mat, i, smudge = FALSE) {
  left  <- mat[, 1:(i / 2)]
  right <- mat[, i:((i / 2) + 1)]
  tab   <- table(left + right)
  if(smudge) {
    all(c("0", "1", "2") %in% names(tab)) && tab["1"] == 1
  } else {
    all(names(tab) %in% c("0", "2"))
  }
}

get_score <- function(dir, start, index, cols) {
  (1 + (99 * (dir == "horizontal"))) *
    abs((index / 2) - ((start == "reverse") * cols))
}

summarize <- function(mat, smudge = FALSE) {
  for(dir in c("vertical", "horizontal")) {
    if(dir == "horizontal") mat <- t(mat)
    for(start in c("forward", "reverse")) {
      if(start == "reverse") mat <- mat[, ncol(mat):1]
      for(i in seq(2, ncol(mat), by = 2)) {
        has_reflection <- reflects(mat, i, smudge = smudge)
        if(has_reflection) return(get_score(dir, start, i, ncol(mat)))
      }
    }
  }
}

# part 1
sum(sapply(notes, summarize))

# part 2
sum(sapply(notes, summarize, smudge = TRUE))
