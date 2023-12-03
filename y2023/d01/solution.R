input <- readLines("input.txt")

decode <- function(input) {
  nums <- gsub("[a-z]", "", input)
  digs <- paste0(substr(nums, 1, 1),
                 substr(nums, nchar(nums), nchar(nums)))
  sum(as.integer(digs))
}


## Part 1
decode(input)


## Part 2
digs <- c(twone = 21, eightwo = 82, oneight = 18,
          five = 5, nine = 9, eight = 8, two = 2, three = 3,
          one = 1, four = 4, six = 6, seven = 7)

for(num in names(digs)) {
  input <- gsub(num, digs[num], input)
}

decode(input)