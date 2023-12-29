
# Example input, if needed
# day24 <- readLines("./puzzle_input/input_day24_exp.txt")

day24 <- readLines("./puzzle_input/input_day24.txt")

day24 <- strsplit(day24, " @ ")
positions <- lapply(day24, function(x) as.numeric(unlist(strsplit(x[1], ", "))))
velocities <- lapply(day24, function(x) as.numeric(unlist(strsplit(x[2], ", "))))

collision <- function(posa, va, posb, vb) {
  
  m_a <- va[2] / va[1]
  m_b <- vb[2] / vb[1]
  
  b_a <- posa[2] - (m_a * posa[1])
  b_b <- posb[2] - (m_b * posb[1])
  
  x <- (b_b - b_a) / (m_a - m_b)
  y <- m_a * x + b_a
  
  return(c(x, y))
}

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# for example
# range_min <- 7
# range_max <- 27

range_min <- 200000000000000
range_max <- 400000000000000

# All pairs
pairs <- combn(length(positions), 2)

collision_counter <- 0

for (i in 1:ncol(pairs)) {
  
  collision_pos <- 
    collision(
      positions[[pairs[1, i]]], velocities[[pairs[1, i]]], 
      positions[[pairs[2, i]]], velocities[[pairs[2, i]]]
    )
  
  valid_collision <- 
    # in area of interest
    all(collision_pos >= range_min & collision_pos <= range_max) &
    
    # after start point of both hail stones
    all(
      ifelse(
        abs(c(velocities[[pairs[1, i]]][1:2], velocities[[pairs[2, i]]][1:2])) == 
          c(velocities[[pairs[1, i]]][1:2], velocities[[pairs[2, i]]][1:2]),
        collision_pos >= c(positions[[pairs[1, i]]][1:2], positions[[pairs[2, i]]][1:2]),
        collision_pos <= c(positions[[pairs[1, i]]][1:2], positions[[pairs[2, i]]][1:2])
      )
    )
  
  if (valid_collision) collision_counter <- collision_counter + 1
}

collision_counter
# 28174

Sys.time() - start
