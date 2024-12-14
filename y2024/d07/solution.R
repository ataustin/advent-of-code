options(scipen = 999)

input   <- readLines("input.txt")
splits  <- strsplit(input, ": | ")
results <- as.numeric(sapply(splits, head, 1))
values  <- lapply(splits, \(x) as.numeric(tail(x, -1)))

# helpers
make_ops <- function(values, ops) {
  expand.grid(rep(list(ops), length(values) - 1), stringsAsFactors = FALSE)
}

concat <- function(x) as.numeric(paste(x, collapse = ""))

compute <- function(o, v, r) {
  # loop over operation strings, find their functions and apply them to
  # subsequent pairs of elements in the values vector
  vv <- v[1]
  sapply(seq_along(o), \(i) {vv <<- get(o[i])(c(vv, v[i+1]))})
  vv
}

is_calibrated <- function(values, result, ops = c("prod", "sum")) {
  ops <- make_ops(values, ops)
  ans <- apply(ops, 1, compute, values, result)
  any(ans == result)
}


# part 1
p1_is_calibrated <- sapply(seq_along(results), \(i)
                           is_calibrated(values[[i]], results[i]))
p1 <- sum(results[p1_is_calibrated])
p1

# part 2
p2_results <- results[!p1_is_calibrated]
p2_values    <- values[!p1_is_calibrated]
p2_is_calibrated <- sapply(seq_along(p2_results), \(i)
                           is_calibrated(p2_values[[i]], p2_results[i],
                                         ops = c("prod", "sum", "concat")))
p2 <- sum(p2_results[p2_is_calibrated]) + p1
p2



# alternate compute function using recursion; slightly slower.
# this still needs some tweaking to solve part 2.
# I think the order in which values and operations are used is wrong.
compute <- function(ops, values, result) {
  if(length(ops) == 1) return(get(ops)(values))
  if(any(values >= result)) return(-Inf)
  
  get(ops[length(ops)])(
    c(values[length(values)],
      compute(head(ops, length(ops) - 1),
              head(values, length(values) - 1),
              result))
  )
}
