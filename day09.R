
# Example input, if needed
# day09 <- readLines("puzzle_input/input_day09_exp.txt")

day09 <- readLines("puzzle_input/input_day09.txt")

day09 <- lapply(strsplit(day09, " "), as.numeric)

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

extrapolations <- vector("numeric", length(day09))

for (i_seq in seq_along(day09)) {
  temp_seq <- day09[[i_seq]]
  end_vals <- temp_seq[length(temp_seq)]
  
  while (!all(temp_seq == 0)) {
    temp_seq <- diff(temp_seq)
    end_vals <- c(end_vals, temp_seq[length(temp_seq)])
  }
  
  new_vals <- cumsum(end_vals)
  extrapolations[i_seq] <- new_vals[length(new_vals)] 
}

sum(extrapolations)
# 1930746032

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

extrapolations <- vector("numeric", length(day09))

for (i_seq in seq_along(day09)) {
  temp_seq <- day09[[i_seq]]
  start_vals <- temp_seq[1]
  
  while (!all(temp_seq == 0)) {
    temp_seq <- diff(temp_seq)
    start_vals <- c(start_vals, temp_seq[1])
  }
  
  new_vals <- 
    Reduce("-", start_vals, accumulate = TRUE, right = TRUE)
  extrapolations[i_seq] <- new_vals[1] 
}

sum(extrapolations)
# 1154

Sys.time() - start
