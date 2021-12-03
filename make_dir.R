make_dir <- function(day) {
  day_char <- formatC(day, width = 2, flag = "0")
  dirname  <- paste0("d", day_char)
  dir.create(dirname)
  
  file_names <- c("challenge.md", "input.txt", "solution.R")
  files <- file.path(dirname, file_names)
  file.create(files)
}