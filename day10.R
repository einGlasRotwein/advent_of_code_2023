
## INPUT PART 1 ----------------------------------------------------------------

# Example input, if needed
# day10 <- readLines("puzzle_input/input_day10_exp3.txt")

day10 <- readLines("puzzle_input/input_day10.txt")

day10 <- as.matrix(do.call(rbind, strsplit(day10, "")))

source("day10_functions.R")

## PART 1 ----------------------------------------------------------------------

start_node <- which(day10 == "S")

# Get all neighbours of S, the only position where the direction is not clear. 
pipe_paths <- list(
  start_node + 1, # down
  start_node - 1, # up
  start_node + nrow(day10), # right
  start_node - nrow(day10) # left
)

# Eliminate possible travel directions that are out of bounds
node_idx <- 
  c(
    start_node %% nrow(day10) != 0, # bottom row (can't go down)
    (start_node - 1) %% nrow(day10) != 0, # top row (can't go up)
    !(start_node - 1) %in% (length(day10) - nrow(day10) + 1):length(day10), # right col (can't go right)
    !start_node %in% 1:nrow(day10) # left col (can't go left)
  )

pipe_paths <- ifelse(node_idx, pipe_paths, NA)

# Eliminate paths: We can only travel to pipes where the opening is facing our 
# current location.

# down: |, L, J
# up: |, 7, F
# right: -, 7, J
# left: -, L, F

pipe_directions <- list(
  c("|", "L", "J"), # down
  c("|", "7", "F"), # up
  c("-", "7", "J"), # right
  c("-", "L", "F") # left
)

valid_pipes <- 
  mapply(
    function(x, y) ifelse(is.na(x), FALSE, day10[x] %in% y),
    pipe_paths, pipe_directions 
  )

pipe_paths <- pipe_paths[valid_pipes]

# We always have to store the previous node, to determine the direction in 
# which we have to travel the pipes.
pipe_paths <- lapply(pipe_paths, function(x) c(start_node, x))

initial_path_lengths <- rep(1, length(pipe_paths))

# Stop if at least two paths end in the same location. When two paths are 
# equally long and end in the same location, that means a loop has been found.
end_points <- sapply(pipe_paths, function(x) x[length(x)])

while (length(unique(end_points)) == length(end_points)) {
  travel_output <- pipe_travel(pipe_paths, day10, initial_path_lengths)
  pipe_paths <- travel_output[[1]]
  initial_path_lengths <- travel_output[[2]]
  end_points <- sapply(pipe_paths, function(x) x[length(x)])
}

unique(initial_path_lengths)
# 7063
