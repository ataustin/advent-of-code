# data
input <- readLines("input.txt")

dots  <- read.csv(text = input, comment.char = "f", header = FALSE, col.names = c("col", "row"))
dots  <- dots + 1  # correct for 0-indexing

folds <- read.table(text = gsub("fold along ", "", input[grepl("^f", input)]), sep = "=",
                    col.names = c("dim", "which"))

paper <- matrix(FALSE, nrow = max(dots$row), ncol = max(dots$col))
paper[as.matrix(dots[, c(2, 1)])] <- TRUE

# helper function
fold <- function(paper, how) {
  if(how$dim == "y") return((paper + paper[nrow(paper):1, ])[1:(how$which), ] > 0)
  if(how$dim == "x") return((paper + paper[, ncol(paper):1])[, 1:(how$which)] > 0)
}

# part 1
sum(fold(paper, folds[1, ]))

# part 2
for(i in 1:nrow(folds)) paper <- fold(paper, folds[i, ])
image(t(paper)[, nrow(paper):1])
