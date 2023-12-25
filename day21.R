
source("day21_functions.R")

# Example input, if needed
# day21 <- readLines("./puzzle_input/input_day21_exp.txt")

day21 <- readLines("./puzzle_input/input_day21.txt")

day21 <- do.call(rbind, strsplit(day21, ""))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

paths <- which(day21 == "S")
day21[day21 == "S"] <- "." # turn into garden tile

# n_steps <- 16 # for example
n_steps <- 64

pb <- txtProgressBar(min = 0, max = n_steps, initial = 0, style = 3) 

for (i in 1:n_steps) {
  
  paths <- walking_elf(paths, day21)
  setTxtProgressBar(pb, i)
  
}

close(pb)

length(paths)
# 3637

Sys.time() - start
