input <- read.table(text = gsub("\\|", "", readLines("input.txt")))

# helper functions
str_sort <- function(string) paste(sort(unlist(strsplit(string, ""))), collapse = "")

vgsub <- Vectorize(gsub, "pattern")

nchar_diff <- function(df, numeral, char_target) {
  target  <- df$code[is.element(df$numeral, numeral)]
  pattern <- paste0("[", df$code, "]")
  nchar(vgsub(pattern, "", target)) == char_target
}

numeral_select <- function(df, nchar, numeral, target) {
  df$nchar == nchar & nchar_diff(df, numeral, target)
}

decode <- function(row) {
  knowns <- data.frame(nchar = c(2, 4, 3, 7), numeral = c(1, 4, 7, 8))
  df <- data.frame(code = row[1:10])
  df$nchar <- nchar(df$code)
  df <- merge(df, knowns, by = "nchar", all.x = TRUE)
  
  df$numeral[numeral_select(df, 6, 1, 1)] <- 6
  df$numeral[numeral_select(df, 5, 6, 1)] <- 5
  df$numeral[numeral_select(df, 5, 1, 0)] <- 3
  df$numeral[df$nchar == 5 & is.na(df$numeral)] <- 2
  df$numeral[numeral_select(df, 6, 3, 0)] <- 9
  df$numeral[df$nchar == 6 & is.na(df$numeral)] <- 0
  
  df$code <- sapply(df$code, str_sort)
  display <- sapply(row[11:14], str_sort)

  df$numeral[match(display, df$code)]
}

# challenge 1
decoded <- apply(input, 1, decode)  # matrix with decoded row in columns
sum(decoded %in% c(1, 4, 7, 8))

# challenge 2
ints <- apply(decoded, 2, paste, collapse = "") 
sum(as.integer(ints))
