input <- read.delim("input.txt", header = FALSE, col.names = "raw")
input$monkey <- cumsum(grepl("^Monkey", input$raw)) - 1
notes <- split(input$raw, input$monkey)

parse_notes <- function(notes) {
  if(grepl("\\+", notes[3])) {
    fun <- `+`
  } else if(grepl("old$", notes[3])) {
    fun <- `^`
  } else {
    fun <- `*`
  }
  
  if(grepl("old$", notes[3])) {
    amt <- 2
  } else {
    amt <- as.integer(gsub("[^0-9]", "", notes[3]))
  }
  
  list(
    items  = as.integer(strsplit(gsub("[^0-9,]", "", notes[2]), ",")[[1]]),
    operation = list(
      fun = fun,
      amt = amt
    ),
    test = list(
      divisor = as.integer(gsub("[^0-9]", "", notes[4])),
      true  = gsub("[^0-9]", "", notes[5]),
      false = gsub("[^0-9]", "", notes[6])
    )
  )
}


monkey_business <- function(parsed, rounds, part = 1, common_denom) {
  counts <- rep(0, length(parsed))
  
  for(round in 1:rounds) {
    for(note in seq_along(parsed)) {
      this <- parsed[[note]]
      if(!length(this$items)) next()
      counts[note] <- counts[note] + length(this$items)
      
      for(item in seq_along(this$items)) {
        worry <- this$items[item]
        if(part == 1) {
          worry <- floor(this$operation$fun(worry, this$operation$amt) / 3)
        } else {
          worry <- this$operation$fun(worry, this$operation$amt) %% common_denom
        }
        
        next_monkey <- if(worry %% this$test$divisor == 0) this$test$true else this$test$false
        parsed[[next_monkey]]$items <- c(parsed[[next_monkey]]$items, worry)
      }
      parsed[[note]]$items <- integer(0)
    }
  }
  
  prod(sort(counts, decreasing = TRUE)[1:2])
}


# part 1
parsed <- lapply(notes, parse_notes)
monkey_business(parsed, 20)

# part 2
common_denom <- prod(sapply(parsed, function(x) x$test$divisor))
monkey_business(parsed, 10000, 2, common_denom)
