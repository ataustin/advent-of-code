input <- readLines("input.txt")

# for each game, create a data frame structure
structure_game <- function(sets) {
  count <- regmatches(sets, gregexpr("\\d+", sets))
  color <- regmatches(sets, gregexpr("[a-z]+", sets))
  set   <- rep(1:length(sets), times = lengths(count))
  data.frame(set   = set,
             color = unlist(color),
             count = as.integer(unlist(count)))
}

# combine all games and find the max cubes of each color per game
sets    <- strsplit(gsub("Game \\d+: ", "", input), split = "; ")
games   <- setNames(lapply(sets, structure_game), 1:length(games))
game_df <- do.call(rbind, games)
game_df$game <- gsub("\\..*$", "", rownames(game_df))

max_cubes <- aggregate(count ~ color + game, y, max)

# part 1
limit <- data.frame(color = c("red", "green", "blue"),
                    max   = c(12, 13, 14))

merged <- merge(max_cubes, limit, by = "color")
merged$possible <- merged$count <= merged$max

outcome <- aggregate(possible ~ game, merged, all)
sum(as.integer(outcome$game[outcome$possible]))

# part 2
powers <- aggregate(count ~ game, max_cubes, prod)
sum(powers$count)
