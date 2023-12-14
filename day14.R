
# Example input, if needed
# day14 <- readLines("./puzzle_input/input_day14_exp.txt")

day14 <- readLines("./puzzle_input/input_day14.txt")

day14 <- do.call(rbind, strsplit(day14, ""))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# Rock IDs: We want to start with the first rock in the north row, then the 
# second, then move one row south ...
round_rocks <- 
  data.frame(
    which(day14 == "O", arr.ind = TRUE)
  )

round_rocks <- 
  with(
    round_rocks, 
    round_rocks[order(row, col), ]
  )

for (i_rock in 1:nrow(round_rocks)) {
  temp_pos <- round_rocks[i_rock, ]
  
  roll_rock <- 
    ifelse(
      length(day14[temp_pos$row - 1, temp_pos$col] %in% c("O", "#")) == 0,
      FALSE,
      !day14[temp_pos$row - 1, temp_pos$col] %in% c("O", "#")
    )
  
  while(roll_rock) {
    temp_pos$row <- temp_pos$row - 1
    
    roll_rock <- 
      ifelse(
        length(day14[temp_pos$row - 1, temp_pos$col] %in% c("O", "#")) == 0,
        FALSE,
        !day14[temp_pos$row - 1, temp_pos$col] %in% c("O", "#")
      )
  }
  
  day14[round_rocks$row[i_rock], round_rocks$col[i_rock]] <- "."
  day14[temp_pos$row, temp_pos$col] <- "O"
}

# Number of round rocks in each row, multiplied with reverse row index
sum(apply(day14, 1, function(x) sum(x == "O")) * nrow(day14):1)
# 105249

Sys.time() - start
