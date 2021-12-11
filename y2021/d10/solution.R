input <- strsplit(readLines("input.txt"), "")

# lookup tables
closers   <- c(")", "]", "}", ">")
key_match <- setNames(c("(", "[", "{", "<"), nm = c(closers))
points1   <- setNames(c(3, 57, 1197, 25137), closers)
points2   <- setNames(1:4, closers)

# helper functions
find_score <- function(chars, key_match, points1, points2) {
  line <- chars[1]   # we grow and shrink this line according to next char
  for(char in chars[2:length(chars)]) {
    if(char %in% key_match) {                           # if an opener
      line <- c(line, char)                             # grow line with opener
    } else if(key_match[char] == line[length(line)]) {  # if closer and match opener
        line <- line[-length(line)]                     # remove opener
    } else {                                            # otherwise you're a corrupt character
        return(data.frame(corrupt = points1[char], incomplete = NA))
    }
  }
  # reaching this point means you've grown too many openers without closers (incomplete)
  closes <- setNames(names(key_match), key_match)[line]
  score  <- Reduce(function(x, y) (x*5) + points2[y], rev(closes), init = 0)
  return(data.frame(corrupt = NA, incomplete = score))
}

# challenge 1
points <- lapply(input, find_score, key_match, points1, points2)
points <- do.call(rbind, points)
sum(points$corrupt, na.rm = TRUE)

# challenge 2
median(points$incomplete, na.rm = TRUE)
