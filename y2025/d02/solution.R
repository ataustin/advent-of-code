input <- strsplit(t(read.csv("input.txt", header = FALSE)), "-")
ids   <- unlist(lapply(input, \(x) x[1]:x[2]))

# part 1
sum(ids[grepl("^(.+)\\1$", ids)])

# part 2
sum(ids[grepl("^(.+)\\1+$", ids)])
