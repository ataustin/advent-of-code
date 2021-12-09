input <- read.table(text = gsub("\\|", "", readLines("input.txt")))

# helpers
str_sort <- function(string) paste(sort(unlist(strsplit(string, ""))), collapse = "")

segments <- matrix(c(
  1, 1, 1, 0, 1, 1, 1,  # 0
  0, 0, 1, 0, 0, 1, 0,  # 1
  1, 0, 1, 1, 1, 0, 1,  # 2
  1, 0, 1, 1, 0, 1, 1,  # 3
  0, 1, 1, 1, 0, 1, 0,  # 4
  1, 1, 0, 1, 0, 1, 1,  # 5
  1, 1, 0, 1, 1, 1, 1,  # 6
  1, 0, 1, 0, 0, 1, 0,  # 7
  1, 1, 1, 1, 1, 1, 1,  # 8
  1, 1, 1, 1, 0, 1, 1), # 9
  nrow = 7,
  dimnames = list(letters[1:7], 0:9)
)

key <- rowSums(t(segments) %*% segments)

build_col <- function(string) {
  col  <- setNames(numeric(7), letters[1:7])
  ones <- unlist(strsplit(string, ""))
  col[ones] <- 1
  col
}

decode <- function(row, key) {
  codes <- row[1:10]
  mat   <- sapply(codes, build_col)
  colnames(mat) <- sapply(codes, str_sort)
  lock <- rowSums(t(mat) %*% mat)
  
  digits  <- sapply(row[11:14], str_sort)
  numbers <- names(key)[match(lock[digits], key)]
  as.integer(numbers)
}

# challenge 1
decoded <- apply(input, 1, decode, key = key)  # matrix with decoded row in columns
sum(decoded %in% c(1, 4, 7, 8))

# challenge 2
ints <- apply(decoded, 2, paste, collapse = "") 
sum(as.integer(ints))
