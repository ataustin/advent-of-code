x <- read.table("input.txt", col.names = c("a", "b"))

# part 1
sum(abs(sort(x$a) - sort(x$b)))

# part 2
tab   <- with(x, table(b[b %in% a]))
score <- as.data.frame(as.numeric(names(tab)) * tab)
x     <- merge(x, score, by.x = "a", by.y = "Var1")
sum(x$Freq)
