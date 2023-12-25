
## INPUT PART 1 ----------------------------------------------------------------

# Example input, if needed
# day10 <- readLines("puzzle_input/input_day10_exp6.txt")

day10 <- readLines("puzzle_input/input_day10.txt")

day10 <- as.matrix(do.call(rbind, strsplit(day10, "")))

source("day10_functions.R")

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

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

# Save the entire path (needed for part 2 later)
pipe_paths <- lapply(pipe_paths, function(x) c(start_node, x))

# Stop if at least two paths end in the same location. When two paths are 
# equally long and end in the same location, that means a loop has been found.
end_points <- sapply(pipe_paths, function(x) x[length(x)])

while (length(unique(end_points)) == length(end_points)) {
  pipe_paths <- pipe_travel(pipe_paths, day10)
  end_points <- sapply(pipe_paths, function(x) x[length(x)])
}

unique(sapply(pipe_paths, length)) - 1
# 7063

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

# Build paths from the two halves
loop <- c(pipe_paths[[1]], rev(pipe_paths[[2]])[-1])

# Convert to x/y coordinates
coordinates <- 
  data.frame(
    x = loop %% nrow(day10),
    y = ceiling(loop / nrow(day10))
  )

# Anti-clockwise for shoelace
coordinates <- coordinates[nrow(coordinates):1, ]

a <- 0

for (i in 1:(nrow(coordinates) - 1)) {
  a <- a + coordinates$x[i] * coordinates$y[i + 1] - 
    coordinates$y[i] * coordinates$x[i + 1] - 1
}

options(scipen = 999)

# I don't have a clue why it's + 1 :-D
# Also, we need to subtract the step size (always 1) for every coordinate - but 
# not for example 5 and 6. Not sure why.
a / 2 + 1 
# 589

Sys.time() - start
