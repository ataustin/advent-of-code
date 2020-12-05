parse_record <- function(text) {
  policy_pwd    <- unlist(strsplit(text, ": "))
  number_letter <- unlist(strsplit(policy_pwd[1], " "))
  min_max       <- unlist(strsplit(number_letter[1], "-"))

  pwd_data <- data.frame(min = as.integer(min_max[1]),
                         max = as.integer(min_max[2]),
                         letter = number_letter[2],
                         password = policy_pwd[2],
                         appearances = sum(unlist(gregexpr(number_letter[2],
                                                           policy_pwd[2])) > 0))

  pwd_data$min_letter <- substr(pwd_data$password, pwd_data$min, pwd_data$min)
  pwd_data$max_letter <- substr(pwd_data$password, pwd_data$max, pwd_data$max)

  pwd_data
}

input <- readLines("input.txt")

parse_list <- lapply(input, parse_record)
parse_data <- do.call(rbind, parse_list)


# challenge 1
is_between <- function(x, min, max) {
  x >= min & x <= max
}

with(parse_data, sum(is_between(appearances, min, max)))


# challenge 2
is_singleton <- function(letter, first, second) {
  xor(letter == first, letter == second)
}

with(parse_data, sum(is_singleton(letter, min_letter, max_letter)))



