input <- paste(readLines("input.txt"), collapse = "")

extract <- function(string, pattern) {
  unlist(regmatches(string, gregexpr(pattern, string)))
}

calculate <- function(string) {
  muls <- extract(string, "mul\\([0-9]{1,3},[0-9]{1,3}\\)")
  nums <- lapply(muls, extract, "[0-9]{1,3}")
  sum(sapply(nums, function(n) prod(as.numeric(n))))
}

# part 1
calculate(input)

# part 2
calculate(gsub("don't\\(\\).*?do\\(\\)", "", input))
