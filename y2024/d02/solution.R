input <- lapply(strsplit(readLines("input.txt"), " "), as.numeric)

# helpers
between <- function(x, a, b) x >= a & x <= b

is_safe <- function(record, min = 1, max = 3) {
  dr <- diff(record)
  length(unique(sign(dr))) == 1 && between(max(abs(dr)), min, max)
}

dampen <- function(record) {
  any(sapply(seq_along(record), function(ix) is_safe(record[-ix])))
}

# part 1
p1_is_safe <- sapply(input, is_safe)
sum(p1_is_safe)

# part 2
p2_is_safe <- sapply(input[!p1_is_safe], dampen)
sum(p1_is_safe) + sum(p2_is_safe)
