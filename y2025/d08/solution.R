x <- read.csv("input.txt", header = FALSE)

d <- dist(x)
h <- hclust(d, method = "single")

# part 1
# h/t Jonathan Marshall @jmarshallnz.bsky.social for the h arg to simplify part 1
ct1 <- cutree(h, h = sort(d)[1000])  # cut so that 1000 pairs are connected
tb1 <- sort(table(ct1), decreasing = TRUE)  # groups boxes together and counts
prod(tb1[1:3])

# part 2
m <- as.matrix(d)
diag(m) <- Inf

ct2 <- cutree(h, k = 2)  # cut so that there are only 2 groups left
ix1 <- which(ct2 > 1)    # this is index of the last remaining join
ix2 <- which(m[ix1, ] == min(m[ix1, ]))  # and this is the one it's closest to
prod(x[c(ix1, ix2), 1])  # multiply the x values of the points at those indices


# my original answer for part 1 (for posterity)
# I couldn't figure out what h value to use for cutree in part 1 so I did this instead
n_closest <- if(nrow(x) < 1000) 10 else 1000  # accommodate test and real data

m <- as.matrix(d)
diag(m) <- Inf

j <- vector("list", n_closest)
for(i in seq_along(j)) {
  # sequentially identify the points closes to each other
  cl     <- which(m == min(m), arr.ind = TRUE)
  m[cl]  <- Inf
  j[[i]] <- sort(cl[1, ])
}

ord   <- do.call(rbind, j)   # index of boxes to join, in order down the rows
boxes <- data.frame(number = 1:nrow(x), circuit = 0)  # circuit label bucket

for(i in 1:n_closest) {
  # identify which circuit a pair of points belongs to
  these_boxes <- ord[i, ]
  in_circuit <- boxes$circuit[these_boxes] > 0
  if(all(!in_circuit)) {  # singletons; form their own circuit
    boxes$circuit[these_boxes] <-max(boxes$circuit) + 1
  } else if(all(in_circuit)) {  # two circuits joining, or closing a loop
    to_update <- boxes$circuit %in% boxes$circuit[these_boxes]
    boxes$circuit[to_update] <- min(boxes$circuit[to_update])
  } else if(any(in_circuit)) {  # adding singleton to circuit
    boxes$circuit[these_boxes[!in_circuit]] <- boxes$circuit[these_boxes[in_circuit]]
  }
}

tab <- table(boxes$circuit[boxes$circuit > 0])
prod(sort(tab, decreasing = TRUE)[1:3])
