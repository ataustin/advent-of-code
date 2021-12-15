template <- readLines("input.txt", n = 1)

rules <- read.csv(text = gsub(" -> ", ",", readLines("input.txt")),
                  header = FALSE, skip = 2,
                  col.names = c("pattern", "insert"))

# helper functions
template_pairs <- function(template) {
  chars <- unlist(strsplit(template, split = ""))
  paste(chars[1:(nchar(template)-1)], chars[2:nchar(template)], sep = "")
}

run_update <- function(count, map) {
  update <- merge(count, map)
  count  <- with(update, aggregate(ct, list(becomes = becomes), sum, na.rm = TRUE))
  names(count) <- c("pair", "ct")
  count
}

letter_range_diff <- function(template, rules, rounds) {
  pairs <- setNames(as.data.frame(table(template_pairs(template))), c("pair", "ct"))
  count <- merge(data.frame(pair = rules$pattern), pairs, all.x = TRUE)
  map   <- data.frame(pair    = rep(rules$pattern, times = 2),
                      becomes = c(paste(substr(rules$pattern, 1, 1), rules$insert, sep = ""),
                                  paste(rules$insert, substr(rules$pattern, 2, 2), sep = "")))
  
  for(i in 1:rounds) count <- run_update(count, map)
  
  count$letter <- substr(count$pair, 2, 2)
  totals       <- aggregate(count$ct, list(letter = count$letter), sum)
  totals$x     <- ifelse(totals$letter == substr(template, 1, 1), totals$x + 1, totals$x)
  diff(range(totals$x))
}

# solutions
options(scipen = 999)
letter_range_diff(template, rules, 10) # part 1
letter_range_diff(template, rules, 40) # part 2
