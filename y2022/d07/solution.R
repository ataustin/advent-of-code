# we don't use "ls" or "dir" components to get directory structure
input <- subset(read.delim(text = gsub("^\\$ ", "", readLines("input.txt")),
                           sep = " ", header = FALSE),
                V1 != "ls" & V1 != "dir")

# use the input to determine file paths and file sizes; store a list for each path
this_path <- NULL
lst <- vector(mode = "list", length = nrow(input))

for(i in 1:nrow(input)) {
  row <- input[i, ]
  
  if(grepl("^[0-9]", row$V1)) {
    lst[[i]] <- list(rel  = row$V2,
                     abs  = c(this_path, row$V2),
                     size = as.integer(row$V1))
  }
  
  if(row$V2 == "..") {
    this_path <- head(this_path, -1)
  } else if(row$V1 == "cd") {
    this_path <- c(this_path, row$V2)
    lst[[i]] <- list(rel = row$V2,
                      abs = this_path,
                      size = 0)
  }
}

lst <- Filter(Negate(is.null), lst)

# structure this list as a data.frame including full file paths
files <- data.frame(path  = sapply(lst, function(x) paste(x$abs, collapse = "/")),
                    depth = sapply(lst, function(x) length(x$abs)),
                    size  = sapply(lst, function(x) x$size))
files$dir <- dirname(files$path)


sum_size <- 0  # for part 1
totals   <- data.frame(path = "/", depth = 0, size = 0, dir = "/")  # for part 2

# starting with the deepest directories, sum up file sizes. The sums become directory
# sizes so add them back to the data for the summation on the next deepest directories.
# Repeat until all directories are done.
while(nrow(files) > 1) {
  is_max_depth <- files$depth == max(files$depth)
  these_files  <- files[is_max_depth, ]
  these_sizes  <- aggregate(size ~ dir + depth, these_files, sum)
  sum_size     <- sum_size + sum(subset(these_sizes$size, these_sizes$size < 100000))
  
  these_sizes <- data.frame(path  = these_sizes$dir,
                            depth = these_sizes$depth - 1,
                            size  = these_sizes$size,
                            dir   = dirname(these_sizes$dir))
  
  files  <- rbind(files[!is_max_depth, ], these_sizes)
  totals <- rbind(totals, these_sizes)
}


# part 1
sum_size

# part 2
total    <- 70000000
required <- 30000000
used     <- files$size
needed   <- required - (total - used)

min(totals$size[totals$size > needed])
