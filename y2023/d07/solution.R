input <- read.table("input.txt", col.names = c("hand", "bid"))
hands <- strsplit(input$hand, "")

get_rank <- function(hands, strength_map, type_map, use_joker = FALSE) {
  tabs  <- if(use_joker) lapply(hands, function(x) if(all(x == "J")) 0 else table(x[x != "J"])) else lapply(hands, table)
  j_ct  <- if(use_joker) sapply(hands, function(h) sum(h == "J")) else rep(0, length(hands))
  u_ct  <- ifelse(j_ct == 5, 1, lengths(tabs))
  max_ct <- sapply(tabs, max) + j_ct
  type_code <- paste(u_ct, max_ct, sep = "")
  strength <- sapply(hands, function(h) paste(strength_map[h], collapse = ""))
  type_map <- c("15" = 7, "24" = 6, "23" = 5, "33" = 4,
                "32" = 3, "42" = 2, "51" = 1)
  hand_rank <- rank(paste0(type_map[type_code], strength))
  hand_rank 
}


# part 1
strength_map <- setNames(letters[13:1], c("A", "K", "Q", "J", "T", 9:2))
sum(get_rank(hands, strength_map) * input$bid)

# part 2
strength_map <- setNames(letters[13:1], c("A", "K", "Q", "T", 9:2, "J"))
sum(get_rank(hands, strength_map, use_joker = TRUE) * input$bid)
