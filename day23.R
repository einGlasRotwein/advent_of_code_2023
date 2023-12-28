
source("day23_functions.R")

# Example input, if needed
# day23 <- readLines("./puzzle_input/input_day23_exp.txt")

day23 <- readLines("./puzzle_input/input_day23.txt")

day23 <- do.call(rbind, strsplit(day23, ""))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

start_tile <- nrow(day23) * (which(day23[1, ] == ".") - 1) + 1

paths <- list(start_tile)
record_length <- 0

while (length(paths) != 0) {
  
  record_length <- max(sapply(paths, length))
  paths <- scenic_walk(paths, day23)
  paths
  
}

record_length - 1 # first tile does not count
# 2106

Sys.time() - start
