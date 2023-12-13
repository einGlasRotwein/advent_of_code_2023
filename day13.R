
# Example input, if needed
# day13 <- readLines("./puzzle_input/input_day13_exp.txt")

day13 <- readLines("./puzzle_input/input_day13.txt")

day13 <- tapply(day13, cumsum(day13 == ""), function(x) {list(x)})
day13 <- lapply(day13, function(x) strsplit(x[x != ""], ""))
day13 <- lapply(day13, function(x) do.call(rbind, x))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

horizontal <- rep(0, length(day13))
vertical <- rep(0, length(day13))

for (i_mat in seq_along(day13)) {
  
  # Check for mirrored rows
  for (j_row in 1:(nrow(day13[[i_mat]]) - 1)) {
    # How many rows in each direction we need.
    n_rows <- min(j_row, nrow(day13[[i_mat]]) - j_row)
    
    half1 <- day13[[i_mat]][(1 + j_row - n_rows):j_row, ]
    # Already flipped
    half2 <- day13[[i_mat]][(j_row + n_rows):(j_row + 1), ]
    
    if (identical(half1, half2)) {
      horizontal[i_mat] <- j_row
      break
    }
  }
  
  # Check for mirrored columns
  for (j_col in 1:(ncol(day13[[i_mat]]) - 1)) {
    # How many rows in each direction we need.
    n_cols <- min(j_col, ncol(day13[[i_mat]]) - j_col)
    
    half1 <- day13[[i_mat]][ , (1 + j_col - n_cols):j_col]
    # Already flipped
    half2 <- day13[[i_mat]][ , (j_col + n_cols):(j_col + 1)]
    
    if (identical(half1, half2)) {
      vertical[i_mat] <- j_col
      break
    }
  }
  
}

sum(horizontal * 100) + sum(vertical)
# 33122

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

# Instead of finding identical matches, we now try to find the two halfs that 
# are identical except for ONE location.

horizontal <- rep(0, length(day13))
vertical <- rep(0, length(day13))

for (i_mat in seq_along(day13)) {
  
  # Check for mirrored rows
  for (j_row in 1:(nrow(day13[[i_mat]]) - 1)) {
    # How many rows in each direction we need.
    n_rows <- min(j_row, nrow(day13[[i_mat]]) - j_row)
    
    half1 <- day13[[i_mat]][(1 + j_row - n_rows):j_row, ]
    # Already flipped
    half2 <- day13[[i_mat]][(j_row + n_rows):(j_row + 1), ]
    
    if (sum(half1 != half2) == 1) {
      horizontal[i_mat] <- j_row
      break
    }
  }
  
  # Check for mirrored columns
  for (j_col in 1:(ncol(day13[[i_mat]]) - 1)) {
    # How many rows in each direction we need.
    n_cols <- min(j_col, ncol(day13[[i_mat]]) - j_col)
    
    half1 <- day13[[i_mat]][ , (1 + j_col - n_cols):j_col]
    # Already flipped
    half2 <- day13[[i_mat]][ , (j_col + n_cols):(j_col + 1)]
    
    if (sum(half1 != half2) == 1) {
      vertical[i_mat] <- j_col
      break
    }
  }
  
}

sum(horizontal * 100) + sum(vertical)
# 32312

Sys.time() - start
