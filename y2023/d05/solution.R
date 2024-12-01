input <- strsplit(readChar("input.txt", 1e6), "\n\n")[[1]]

raw <- setNames(regmatches(input, gregexpr("\\d+", input)),
                regmatches(input, gregexpr("^[a-z\\-]*", input)))
raw[] <- lapply(raw, as.numeric)

seeds <- raw[[1]]
maps  <- raw[2:length(raw)]

make_map_data <- function(map) {
  dat <- setNames(as.data.frame(matrix(map, ncol = 3, byrow = TRUE)),
                  c("dest_start", "source_start", "length"))
  dat$dest_end   <- dat$dest_start + dat$length - 1
  dat$source_end <- dat$source_start + dat$length - 1
  dat
}

maps <- lapply(maps, make_map_data)

get_destination <- function(source_number, map) {
  in_range <- source_number <= map$source_end & source_number >= map$source_start
  if(!any(in_range)) return(source_number)
  map$dest_start[in_range] + source_number - map$source_start[in_range]
}


find_closest <- function(seeds, maps) {
  closest <- max(maps[[length(maps)]]$dest_end)
  
  for(s in seeds) {
    for(i in seq_along(maps)) {
      s <- get_destination(s, maps[[i]])
    }
    if(s < closest) closest <- s
  }
  
  closest
}


# part 1
find_closest(seeds, maps)


# part 2
make_map_data <- function(map) {
  dat <- setNames(as.data.frame(matrix(map, ncol = 3, byrow = TRUE)),
                  c("dest_low", "source_low", "length"))
  dat$dest_high   <- dat$dest_low + dat$length - 1
  dat$source_high <- dat$source_low + dat$length - 1
  dat[c("length", "source_low", "source_high", "dest_low", "dest_high")]
}

maps <- lapply(maps, make_map_data)


`%btw%` <- function(v, range) {v > range[1] & v < range[2]}

map_range <- function(range, map) {
  map$slow_gte_rlow   <- map$source_low >= range[1]
  map$slow_lte_rhigh  <- map$source_low <= range[2]
  map$shigh_gte_rlow  <- map$source_high >= range[1]
  map$shigh_lte_rhigh <- map$source_high <= range[2]
  map
}













