input <- as.integer(readLines("input.txt", warn = FALSE))
input <- data.frame(calories = input,
                    elf      = cumsum(is.na(input)))

# part 1
total    <- aggregate(calories ~ elf, data = input, sum, na.rm = TRUE)
top_cals <- sort(total$calories, decreasing = TRUE)
top_cals[1]

# part 2
sum(top_cals[1:3])
