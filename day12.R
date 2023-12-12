
# Example data, if needed
# day12 <- readLines("./puzzle_input/input_day12_exp.txt")

day12 <- readLines("./puzzle_input/input_day12.txt")
day12 <- strsplit(day12, " ")

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

springs <- lapply(day12, function(x) unlist(strsplit(x[1], "")))
logs <- lapply(day12, function(x) as.numeric(unlist(strsplit(x[2], ","))))
valid_arrangements <- vector("numeric", length(springs))

pb <- 
  txtProgressBar(
    min = 0, max = length(springs), initial = 0, style = 3
  ) 

# Get current broken spring counts
for (i_spring in seq_along(springs)) {
  
  n_broken <- sum(logs[[i_spring]])
  n_functional <- length(springs[[i_spring]]) - n_broken
  
  temp_seq <- vector("list", 1)
  
  for (j_pos in seq_along(springs[[i_spring]])) {
    # If position is not ?, append the current symbol to each current sequence
    if (springs[[i_spring]][j_pos] != "?") {
      temp_seq <- lapply(temp_seq, function(x) c(x, springs[[i_spring]][j_pos]))
    } else {
      # If position is ?, generate two new sequence for each existing sequence,
      # one appended with ., and one with #
      temp_seq <- 
        c(
          lapply(temp_seq, function(x) c(x, "#")),
          lapply(temp_seq, function(x) c(x, "."))
        )
    }
    
    # Remove all seqs that are already invalid, i.e. where
    # 1) The sum of # is > than the target sum of #
    temp_seq <- temp_seq[sapply(temp_seq, function(x) sum(x == "#")) <= n_broken]
    
    # 2) The sum of . is > than the target sum of .
    temp_seq <- temp_seq[sapply(temp_seq, function(x) sum(x == ".")) <= n_functional]
    
    # If counts already do not add up anymore
    counts_ok <- 
      sapply(temp_seq, function(x) {
        
        temp_rle <- rle(x)
        temp_broken_rle <- temp_rle$lengths[temp_rle$values == "#"]
        temp_broken_rle <- 
          ifelse(length(temp_broken_rle) == 0, 0, temp_broken_rle)
        temp_broken_logs <- 
          c(
            temp_broken_rle, 
            rep(TRUE, length(logs[[i_spring]]) - length(temp_broken_rle))
          )
        
        all(temp_broken_logs <= logs[[i_spring]])
      })
    
    temp_seq <- temp_seq[counts_ok]
    
  }
  
  # Remove all sequences where the counts don't add up (because the last count 
  # is too low).
  counts_ok <- 
    sapply(temp_seq, function(x) {
      
      temp_rle <- rle(x)
      temp_broken_rle <- temp_rle$lengths[temp_rle$values == "#"]
      identical(
        as.integer(temp_broken_rle),
        as.integer(logs[[i_spring]])
      )
      
    })
  
  temp_seq <- temp_seq[counts_ok]
  
  valid_arrangements[i_spring] <- length(temp_seq)
  
  setTxtProgressBar(pb, i_spring)
  
}

close(pb)

sum(valid_arrangements)
# 7025

Sys.time() - start
