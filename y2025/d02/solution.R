input <- strsplit(t(read.csv("input.txt", header = FALSE)), "-")

get_candidates <- function(range) {
  vec <- range[1]:range[2]
  vec <- vec[nchar(vec) %% 2 == 0]
  vec
}

ids <- unlist(lapply(input, get_candidates))
nch <- nchar(ids)
h1  <- substr(ids, start = 1, stop = nch/2)
h2  <- substr(ids, start = (nch/2) + 1, stop = nch)

sum(ids[h1 == h2])
