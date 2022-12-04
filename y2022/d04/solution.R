input <- read.delim("input.txt", sep = ",", header = FALSE)

find_overlap <- function(row) {
  ranges   <- lapply(strsplit(row, "-"), as.integer)
  sections <- lapply(ranges, function(r) seq(r[[1]], r[[2]], by = 1L))
  overlap  <- Reduce(intersect, sections)
  data.frame(len1       = length(sections[[1]]),
             len2       = length(sections[[2]]),
             len_shared = length(overlap))
}

section_data <- do.call(rbind, apply(input, 1, find_overlap))

# part 1
with(section_data, sum(len1 == len_shared | len2 == len_shared))

# part 2
sum(section_data$len_shared != 0)
