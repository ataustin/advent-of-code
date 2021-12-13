map <- read.table("input.txt", sep = "-", col.names = c("here", "there"))
map <- rbind(map, setNames(map, rev(names(map))))

# helper functions
is_small     <- function(cave) grepl("[a-z]", cave)
is_end       <- function(paths) last_col(paths) == "end"
last_col     <- function(df) df[, ncol(df)]
rows_to_list <- function(df) split(df, seq(nrow(df)))

is_small_revisit <- function(row) {
  cave <- row[length(row)]
  is_small(cave) && (cave %in% as.character(row[1:(length(row)-1)]))
}

merge_available_paths <- function(paths, map) {
  incoming_cols <- names(paths)
  paths_col     <- incoming_cols[length(incoming_cols)]
  merged        <- merge(paths, map,
                         by.x = paths_col, by.y = "here",
                         suffixes = c("", ncol(paths)))
  merged[c(incoming_cols, setdiff(names(merged), incoming_cols))]
}

count_paths <- function(map, max_revisits) {
  paths         <- map[map$here == "start", ]
  paths$revisit <- 0
  paths         <- paths[(c("revisit", "here", "there"))]
  completed     <- vector(mode = "list")
  
  while(nrow(paths)) {
    paths     <- merge_available_paths(paths, map)
    ends      <- paths[is_end(paths), ]
    if(nrow(ends)) {
      completed <- append(completed, rows_to_list(ends))
      paths     <- paths[!is_end(paths), ]
    }
    
    paths          <- paths[!last_col(paths) == "start", ]
    small_revisits <- apply(paths, 1, is_small_revisit)
    if(any(small_revisits)) {
      paths$revisit[small_revisits] <- paths$revisit[small_revisits] + 1
      paths <- paths[paths$revisit <= max_revisits, ]
    }
  }
  
  length(completed)
}

# solutions
count_paths(map, max_revisits = 0)  # part 1
count_paths(map, max_revisits = 1)  # part 2
