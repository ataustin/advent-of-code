move <- function(sign) {
  switch(sign, "+" = `+`, "-" = `-`)
}

execute <- function(step) {
  switch(step,
         "nop" = {i <<- i + 1},
         "jmp" = {i <<- move(direction[i])(i, amount[i])},
         "acc" = {accum <<- move(direction[i])(accum, amount[i]);
                  i <<- i + 1}
          )
}

input       <- readLines("input.txt", warn = FALSE)
instruction <- substr(input, 1, 3)
direction   <- substr(input, 5, 5)
amount      <- as.integer(substr(input, 6, nchar(input)))


# challenge 1
i <- 1
accum <- 0
visited <- rep(FALSE, length(input))

while(!visited[i]) {
  visited[i]  <- TRUE
  execute(instruction[i])
}

accum


# challenge 2
jmp_ix <- which(instruction == "jmp")
jmp_pointer <- 1
i <- 1

while(i <= length(input)) {
  i       <- 1
  accum   <- 0
  visited <- rep(FALSE, length(input))

  instruction[jmp_ix[jmp_pointer]] <- "nop"

  while(!is.na(visited[i]) && !visited[i]) {
    visited[i] <- TRUE
    execute(instruction[i])
  }

  instruction[jmp_ix[jmp_pointer]] <- "jmp"
  jmp_pointer <- jmp_pointer + 1
}

accum