clean_rules <- function(input) {
  input <- gsub("bags", "bag", input)
  input <- gsub(" no ", " 0 ", input)
  input <- gsub("\\.", "", input)
  input
}


get_bag_lookup <- function(content_string) {
  contents <- unlist(strsplit(content_string, ", "))
  counts_color <- setNames(as.integer(substr(contents, 1, 1)),
                           substr(contents, 3, nchar(contents)))
  counts_color
}


parse_rules <- function(input) {
  content_list <- vector(mode = "list", length = length(input))
  type_contents <- strsplit(input, " contain ")
  names(content_list) <- vapply(type_contents, `[`, character(1), i = 1)
  content_list[] <- lapply(type_contents, `[`, i = 2)
  content_list[] <- lapply(content_list, get_bag_lookup)
  content_list[] <- lapply(content_list, as.list)

  content_list
}


input <- readLines("input.txt", warn = FALSE)
rules_text <- clean_rules(input)
rules <- parse_rules(rules_text)


# challenge 1
contains_target_bag <- function(this_bag, rules, target) {
  if(sum(unlist(this_bag)) > 0) {
    if(target %in% names(this_bag)) {
      return(TRUE)
    } else {
      out <- lapply(names(this_bag),
                    function(nm) contains_target_bag(rules[[nm]], rules, target))
    }
  } else {
    return(FALSE)
  }

  any(unlist(out))
}


has_shiny_gold <- sapply(rules, contains_target_bag,
                         rules = rules, target = "shiny gold bag")
sum(has_shiny_gold)




# challenge 2
compute_all_bags <- function(this_bag, rules) {
  bag_name <- names(this_bag)
  this_bag <- unlist(this_bag)
  downstream_bag_count <- integer(length(this_bag))
  
  for(i in seq_along(rules[[bag_name]])) {
    downstream_bag_count[i] <- compute_all_bags(rules[[bag_name]][i], rules)
  }

  return(this_bag + this_bag * sum(downstream_bag_count))
}


sgb <- rules[["shiny gold bag"]]

total <- 0
for(i in seq_along(sgb)) {
  total <- total + compute_all_bags(sgb[i], rules)
}
total