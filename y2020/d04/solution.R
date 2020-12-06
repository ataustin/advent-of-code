build_passport_data <- function(input) {
  passports <- unlist(strsplit(x, "\n\n"))
  fields    <- strsplit(passports, " |\n")
  kv_list   <- lapply(fields, strsplit, split = ":")  # most granular split
  kv_matrix <- lapply(kv_list, function(x) do.call(rbind, x)) # keys, values paired
  named_lists <- lapply(kv_matrix, function(x) setNames(as.list(x[, 2]), x[, 1]))
  df_list     <- lapply(named_lists, data.frame, stringsAsFactors = FALSE)
  
  # fill in missing data and bind all rows
  all_fields <- Reduce(unique, lapply(df_list, names))
  complete_df_list <- lapply(df_list, complete_data, all_fields = all_fields)
  passport_data <- do.call(rbind, complete_df_list)
  passport_data[] <- lapply(passport_data, type.convert, as.is = TRUE)

  passport_data
}

complete_data <- function(data, all_fields) {
  new_cols <- setdiff(all_fields, names(data))
  data[new_cols] <- NA
  data
}

input <- readChar("input.txt", nchars = file.info("input.txt")$size)
passport_data <- build_passport_data(input)


# challenge 1
no_cid <- passport_data[setdiff(names(passport_data), "cid")]
valid  <- no_cid[!apply(no_cid, 1, anyNA), ]
nrow(valid)


# challenge 2
between <- function(x, min, max) x >= min & x <= max

convert_height <- function(height) {
  has_units <- grepl("cm|in", height)
  units     <- ifelse(has_units, ifelse(grepl("cm", height), "cm", "in"), NA)
  amt       <- gsub("cm|in", "", height)
  data.frame(amt   = amt,
             units = units,
             max   = ifelse(units == "cm", 193, 76),
             min   = ifelse(units == "cm", 150, 59))
}

is_valid <- function(passport_data) {
  byr_valid <- between(passport_data$byr, 1920, 2002)
  iyr_valid <- between(passport_data$iyr, 2010, 2020)
  eyr_valid <- between(passport_data$eyr, 2020, 2030)
  hcl_valid <- grepl("^\\#[0-9a-f]{6}$", passport_data$hcl)
  ecl_valid <- passport_data$ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  pid_valid <- grepl("^[0-9]{9}$", passport_data$pid)

  hgt_data  <- convert_height(passport_data$hgt)
  hgt_valid <- !is.na(hgt_data$units) & between(hgt_data$amt, hgt_data$min, hgt_data$max)

  byr_valid & iyr_valid & eyr_valid & hcl_valid & ecl_valid & pid_valid & hgt_valid
}


sum(is_valid(valid))
