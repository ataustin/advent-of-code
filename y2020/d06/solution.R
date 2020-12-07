input <- readChar("input.txt", nchar = file.info("input.txt")$size)
group_answers <- strsplit(unlist(strsplit(input, "\n\n")), "\n")

# challenge 1
all_yes <- lapply(group_answers, function(ans) unlist(strsplit(ans, "")))
sum(lengths(lapply(all_yes, unique)))

# challenge 2
each_yes <- lapply(group_answers, strsplit, split = "")
common_yes <- lapply(each_yes, function(ans) Reduce(intersect, ans))
sum(lengths(common_yes))