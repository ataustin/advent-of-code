binary_search <- function(characters, candidates) {
  if(length(candidates) == 1) return(candidates)

  midpoint  <- mean(candidates)
  min_index <- min(candidates)
  max_index <- max(candidates)
  
  if(characters[1] %in% c("F", "L")) {
    max_index <- floor(midpoint)
  } else {
    min_index <- ceiling(midpoint)
  }

  out <- binary_search(characters[-1], min_index:max_index)
  out
}


find_location <- function(input, start, stop, candidates) {
  moves <- sapply(input, substr, start = start, stop = stop)
  chars <- strsplit(moves, "")
  locs  <- sapply(chars, binary_search, candidates = candidates)

  locs
}


input <- readLines("input.txt", warn = FALSE)

# challenge 1
rows <- find_location(input, 1, 7, 0:127)
cols <- find_location(input, 8, 10, 0:7)
seat_ids <- (rows * 8) + cols
max(seat_ids)


# challenge 2
ids_sorted <- sort(seat_ids)
ids_sorted[diff(ids_sorted) == 2] + 1


