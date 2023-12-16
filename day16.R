
# Example input, if needed
# day16 <- readLines("./puzzle_input/input_day16_exp.txt")

source("day16_functions.R")

day16 <- readLines("./puzzle_input/input_day16.txt")

day16 <- do.call(rbind, strsplit(day16, ""))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

beam_paths <- list(c(1, 1))

if (day16[rbind(beam_paths[[1]])] == "\\") {
  beam_directions <- c("D")
} else if (day16[rbind(beam_paths[[1]])] == "/") {
  beam_directions <- c("U")
} else {
  beam_directions <- c("R")
}

visited <- run_beam(beam_paths, beam_directions, day16)

sum(visited)
# 8021

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

rm(list = ls())

# Example input, if needed
# day16 <- readLines("./puzzle_input/input_day16_exp.txt")

source("day16_functions.R")

day16 <- readLines("./puzzle_input/input_day16.txt")

day16 <- do.call(rbind, strsplit(day16, ""))

start <- Sys.time()

start_x <- 
  c(
    rep(1, ncol(day16)), # top row
    1:nrow(day16), # right col
    rep(nrow(day16), ncol(day16)), # bottom row 
    1:nrow(day16) # left col
  )

start_y <- 
  c(
    1:ncol(day16), 
    rep(ncol(day16), nrow(day16)), 
    1:ncol(day16), 
    rep(1, nrow(day16))
  )

start_direction <- 
  c(
    rep("D", nrow(day16)),
    rep("L", ncol(day16)),
    rep("U", nrow(day16)),
    rep("R", ncol(day16))
  )

energized <- vector("numeric", length(start_x))

for (i_start_pos in seq_along(start_x)) {
  
  beam_paths <- list(c(start_x[i_start_pos], start_y[i_start_pos]))
  start_dir <- start_direction[i_start_pos]
  
  if (start_dir == "R") {
    
    if (day16[rbind(beam_paths[[1]])] == "\\") {
      beam_directions <- c("D")
    } else if (day16[rbind(beam_paths[[1]])] == "/") {
      beam_directions <- c("U")
    } else if (day16[rbind(beam_paths[[1]])] == "|") {
      beam_paths <- c(list(beam_paths[[1]]), list(beam_paths[[1]]))
      beam_directions <- c("U", "D")
    } else {
      beam_directions <- c("R")
    }
    
  } else if (start_dir == "L") {
    
    if (day16[rbind(beam_paths[[1]])] == "\\") {
      beam_directions <- c("U")
    } else if (day16[rbind(beam_paths[[1]])] == "/") {
      beam_directions <- c("D")
    } else if (day16[rbind(beam_paths[[1]])] == "|") {
      beam_paths <- c(list(beam_paths[[1]]), list(beam_paths[[1]]))
      beam_directions <- c("U", "D")
    } else {
      beam_directions <- c("L")
    }
    
  } else if (start_dir == "U") {
    
    if (day16[rbind(beam_paths[[1]])] == "\\") {
      beam_directions <- c("L")
    } else if (day16[rbind(beam_paths[[1]])] == "/") {
      beam_directions <- c("R")
    } else if (day16[rbind(beam_paths[[1]])] == "-") {
      beam_paths <- c(list(beam_paths[[1]]), list(beam_paths[[1]]))
      beam_directions <- c("R", "L")
    } else {
      beam_directions <- c("U")
    }
    
  } else if (start_dir == "D") {
    
    if (day16[rbind(beam_paths[[1]])] == "\\") {
      beam_directions <- c("R")
    } else if (day16[rbind(beam_paths[[1]])] == "/") {
      beam_directions <- c("L")
    } else if (day16[rbind(beam_paths[[1]])] == "-") {
      beam_paths <- c(list(beam_paths[[1]]), list(beam_paths[[1]]))
      beam_directions <- c("R", "L")
    } else {
      beam_directions <- c("D")
    }
    
  }
  
  visited <- run_beam(beam_paths, beam_directions, day16)
  
  energized[i_start_pos] <- sum(visited)
  
}

max(energized)
# 8216

Sys.time() - start
