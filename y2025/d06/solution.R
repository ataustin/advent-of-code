options(scipen = 999)

# part 1
x1   <- read.table("input.txt")
ops  <- as.character(x1[nrow(x1), ])
vals <- as.data.frame(lapply(x1[-nrow(x1), ], as.numeric))

sum(sapply(1:length(ops), function(i) Reduce(ops[i], vals[[i]])))

# part 2
x2   <- readLines("input.txt")[-nrow(x1)]
ints <- strsplit(format(x2, width = max(nchar(x2)), justify = "left"), "")
nums <- as.numeric(apply(t(do.call(rbind, ints)), 1, paste, collapse = ""))
vals <- unname(split(nums[!is.na(nums)], cumsum(is.na(nums))[!is.na(nums)]))

sum(sapply(1:length(ops), function(i) Reduce(ops[i], vals[[i]])))
