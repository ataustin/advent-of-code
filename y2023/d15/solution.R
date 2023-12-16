input <- strsplit(readLines("input.txt"), ",")[[1]]

hash <- function(string) {
  current <- 0
  for(code in utf8ToInt(string)) current <- ((current + code) * 17) %% 256
  current
}

# part 1
sum(sapply(input, hash))


# part 2
yank      <- function(char, pattern) unlist(regmatches(char, gregexpr(pattern, char)))
remove    <- function(vec, el) vec[setdiff(names(vec), el)]
get_focus <- function(box_number, contents) (box_number) * (1:length(contents)) * contents

steps <- data.frame(label = yank(input, "^[a-z]*"),
                    task  = yank(input, "=|-"),
                    focal = as.numeric(yank(input, "-$|[0-9]")), # will create NAs by design
                    box   = sapply(yank(input, "^[a-z]*"), hash) + 1)

boxes <- lapply(rep(0, 256), numeric)

for(i in 1:nrow(steps)) {
  row <- steps[i, ]
  if(row$task == "-") {
    boxes[[row$box]] <- remove(boxes[[row$box]], row$label)
  } else {
    boxes[[row$box]][row$label] <- row$focal
  }
}

sum(unlist(sapply(1:length(boxes), function(i) get_focus(i, boxes[[i]]))))
