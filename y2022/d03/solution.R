input <- data.frame(contents = readLines("input.txt"))

input$size  <- nchar(input$contents) / 2
input$cpt1  <- substr(input$contents, 1, input$size)
input$cpt2  <- substr(input$contents, input$size + 1, input$size * 2)
input$group <- rep((1:(nrow(input)/3)), each = 3)

# helper
find_common <- function(strings) Reduce(intersect, strsplit(strings, ""))

# part 1
input$common <- apply(input[c("cpt1", "cpt2")], 1, find_common)
sum(match(input$common, c(letters, LETTERS)))

# part 2
badges <- aggregate(contents ~ group, input, find_common)
sum(match(badges$contents, c(letters, LETTERS)))
