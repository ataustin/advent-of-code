x <- readLines("input.txt")

# region data
regions      <- grep("x", x, value = TRUE)
region_dims  <- lapply(strsplit(gsub(":.*", "", regions), "x"), as.numeric)
min_capacity <- sapply(region_dims, \(d) prod(floor(d / 3)))

# shape data
count_each   <- lapply(strsplit(gsub("^.*: ", "", regions), " "), as.numeric)
count_totals <- sapply(count_each, sum)

# solution
sum(count_totals <= min_capacity)
