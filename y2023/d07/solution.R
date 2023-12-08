input <- read.table("input.txt", col.names = c("hand", "bid"))

hands <- strsplit(input$hand, "")
tabs  <- lapply(hands, table)
input$u_ct   <- lengths(tabs)
input$max_ct <- sapply(tabs, max)
input$type_code <- paste(input$u_ct, input$max_ct, sep = "")

rank_map <- c("15" = 7, "24" = 6, "23" = 5, "33" = 4,
              "32" = 3, "42" = 2, "51" = 1)

input$type_rank <- rank_map[input$type_code]

strength_map <- data.frame(face     = c("A", "K", "Q", "J", "T", 9:2),
                           strength = letters[13:1])

get_strength <- function(hand, map) {
  paste(map$strength[match(hand, map$face)], collapse = "")
}

input$strength <- vapply(hands, get_strength, character(1), strength_map)
input$overall_rank <- rank(paste0(input$type_rank, input$strength))


# part 1
sum(input$overall_rank * input$bid)


# part 2
input$j_ct <- sapply(hands, function(x) sum(x == "J"))
input$u_ct <- ifelse(input$j_ct == 5, 1, input$u_ct - (input$j_ct > 0))
tabs  <- lapply(hands, function(x) if(all(x == "J")) 0 else table(x[x != "J"]))
input$max_ct <- sapply(tabs, max) + input$j_ct
# input$max_ct <- ifelse(input$j_ct == 5, 5, input$max_ct)
input$type_code <- paste(input$u_ct, input$max_ct, sep = "")


input$type_rank <- rank_map[input$type_code]

strength_map <- data.frame(face     = c("A", "K", "Q", "T", 9:2, "J"),
                           strength = letters[13:1])


input$strength <- vapply(hands, get_strength, character(1), strength_map)

input$overall_rank <- rank(paste0(input$type_rank, input$strength))

sum(input$overall_rank * input$bid)
