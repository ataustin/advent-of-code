input <- readLines("input.txt")

# functions
get_number_box <- function(number_start, length, ncol, nrow) {
  number_end   <- number_start + length - 1
  left_offset  <- number_start %% ncol != 1
  right_offset <- number_end %% ncol != 0
  
  this_line <- (number_start - left_offset):(number_end + right_offset)
  above     <- if(number_end - ncol > 0) this_line - ncol else NULL
  below     <- if(number_start + ncol < ncol * nrow) this_line + ncol else NULL
  
  c(above, this_line, below)
}

get_part_info <- function(start, length, ncol, nrow, grid) {
  box     <- get_number_box(start, length, ncol, nrow)
  values  <- grid[box]
  is_part <- any(grepl("[^0-9\\.]", values))
  star_ix <- if("*" %in% values) box[values == "*"] else NA
  data.frame(is_part = is_part,
             star_ix = star_ix)
}

# data prep
schematic  <- paste(input, collapse = "")
number_loc <- gregexpr("\\d+", schematic)

## data containing numbers, their start positions and their length
num_dat <- data.frame(number = as.integer(regmatches(schematic, number_loc)[[1]]),
                      start  = unlist(number_loc),
                      length = attr(number_loc[[1]], "match.length"))

## data containing whether a number is a part and the location of the * characters
schematic_split <- strsplit(schematic, "")[[1]]

part_dat <- do.call(rbind, lapply(1:nrow(num_dat), function(i)
                                  get_part_info(num_dat$start[i], num_dat$length[i],
                                                nchar(input[1]), length(input),
                                                schematic_split)))

all_dat <- data.frame(num_dat, part_dat)

# part 1
sum(all_dat$number[all_dat$is_part])

# part 2
all_dat <- all_dat[!is.na(all_dat$star_ix), ]
all_dat$is_gear <- duplicated(all_dat$star_ix) | duplicated(all_dat$star_ix, fromLast = TRUE)
sum(aggregate(number ~ star_ix, all_dat[all_dat$is_gear, ], prod)$number)
