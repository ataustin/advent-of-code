input <- read.delim("input.txt", sep = " ",
                    header = FALSE, col.names = c("them", "me"))

# helpers
lookup <- function(play, mat) mat[play[[1]], play[[2]]]

# part 1
dimnames <- list(c("A", "B", "C"), c("X", "Y", "Z"))
score    <- matrix(c(1+3, 2+6, 3+0,
                     1+0, 2+3, 3+6,
                     1+6, 2+0, 3+3),
                   byrow = TRUE, nrow = 3, dimnames = dimnames)

input$score <- apply(input, 1, lookup, mat = score)
sum(input$score)

# part 2
shape <- matrix(c("Z", "X", "Y",
                  "X", "Y", "Z",
                  "Y", "Z", "X"),
                byrow = TRUE, nrow = 3, dimnames = dimnames)

input$me    <- apply(input, 1, lookup, mat = shape)
input$score <- apply(input, 1, lookup, mat = score)
sum(input$score)
