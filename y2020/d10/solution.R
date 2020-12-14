input <- as.integer(readLines("input.txt", warn = FALSE))

# challenge 1
all_ratings <- c(0, input, max(input) + 3)
jolt_dist <- table(diff(sort(all_ratings)))
jolt_dist["1"] * jolt_dist["3"]


# challenge 2
return_runs <- function(adapters) {
  full_chain <- c(0, sort(adapters), max(adapters) + 3)
  jolt_diffs <- c(0, diff(full_chain))

  diff_rle  <- rle(jolt_diffs)
  run_end   <- cumsum(diff_rle$lengths)[diff_rle$values == 1]
  run_start <- run_end - diff_rle$lengths[diff_rle$values == 1]

  n_runs <- length(run_end)
  runs   <- lapply(1:n_runs, function(i) full_chain[c(run_start[i]:run_end[i])])
  runs
}


arrange_run <- function(run) {
  if(length(run) == 2) return(list(run))
  if(length(run) == 3) return(list(run, run[c(1, 3)]))
  eligibles <- run[2:(length(run) - 1)]
  combs <- lapply(0:length(eligibles),
                  function(i) combn(eligibles, i, simplify = FALSE))
  combs <- unlist(combs, recursive = FALSE)
  combs[] <- lapply(combs, function(x) c(run[1], x, run[length(run)]))
  combs
}

count_valid_run_arrangements <- function(arrangements) {
  diffs <- lapply(arrangements, diff)
  diff_max <- lapply(diffs, max)
  valid_ct <- sum(unlist(diff_max) < 4)
  valid_ct
}


all_runs <- return_runs(input)
arrangement_list <- lapply(all_runs, arrange_run)
valid_combinations <- sapply(arrangement_list, count_valid_run_arrangements)
prod(valid_combinations)
