input <- read.table(text = gsub("\\|", "", readLines("input.txt")))

# helper functions
str_sort <- function(string) paste(sort(unlist(strsplit(string, ""))), collapse = "")

build_col <- function(string) {
  col  <- setNames(numeric(7), letters[1:7])
  ones <- unlist(strsplit(string, ""))
  col[ones] <- 1
  col
}

build_mat <- function(codes, colnames = NULL) {
  mat <- sapply(codes, build_col)
  colnames(mat) <- if(is.null(colnames)) sapply(codes, str_sort) else colnames
  mat
}

mat_to_vec <- function(mat) rowSums(t(mat) %*% mat)

build_lock <- function(codes) {
  mat  <- build_mat(codes)
  mat_to_vec(mat)
}

decode <- function(row, key) {
  lock <- build_lock(row[1:10])
  digits  <- sapply(row[11:14], str_sort)
  numbers <- names(key)[match(lock[digits], key)]
  as.integer(numbers)
}

# challenge 1
segments <- setNames(c("abcefg", "cf", "acdeg", "acdfg", "bcdf",
                       "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"),
                     0:9)
key <- mat_to_vec(build_mat(segments, colnames = 0:9))
decoded <- apply(input, 1, decode, key = key)  # matrix with decoded row in columns
sum(decoded %in% c(1, 4, 7, 8))

# challenge 2
ints <- apply(decoded, 2, paste, collapse = "") 
sum(as.integer(ints))
