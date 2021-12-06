input <- read.csv(text = gsub(" -> ", ",", readLines("input.txt")),
                  header = FALSE,
                  col.names = c("x1", "y1", "x2", "y2"))

# helper functions
make_coords <- function(row) {
  paste(row["x1"]:row["x2"], row["y1"]:row["y2"])
}

overlap <- function(input) {
  coord_list  <- apply(input, 1, make_coords)
  coord_table <- table(unlist(coord_list))
  sum(coord_table > 1)
}

# challenge 1
overlap(input[input$y2 == input$y1 | input$x2 == input$x1, ])

# challenge 2
overlap(input)

