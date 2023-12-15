
# Example input, if needed
# day15 <- readLines("./puzzle_input/input_day15_exp.txt")

day15 <- readLines("./puzzle_input/input_day15.txt")

day15 <- unlist(strsplit(day15, ","))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

steps <- 
  sapply(
    day15, function(x) {
      Reduce(function(y, z) ((y + z) * 17) %% 256, utf8ToInt(x), init = 0)
    },
    USE.NAMES = FALSE
  )

sum(steps)
# 511215 

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

boxes <- vector("list", 256)

for (i_step in seq_along(day15)) {
  temp_command <- day15[[i_step]]
  hash <- gsub("[^a-z]", "", temp_command)
  box_no <- 
    # add 1 because list index starts at 1, not 0
    Reduce(function(y, z) ((y + z) * 17) %% 256, utf8ToInt(hash), init = 0) + 1
  operation <- gsub("[^=-]", "", temp_command)
  lense_in_box <- any(names(boxes[[box_no]]) %in% hash)
  
  if (lense_in_box) {
    if (operation == "=") {
      value <- gsub("[^0-9]", "", temp_command)
      boxes[[box_no]][names(boxes[[box_no]]) == hash] <- value
    } else {
      boxes[[box_no]] <- boxes[[box_no]][-which(names(boxes[[box_no]]) == hash)]
    }
  } else {
    if (operation == "=") {
      value <- gsub("[^0-9]", "", temp_command)
      names(value) <- hash
      boxes[[box_no]] <- c(boxes[[box_no]], value)
    }
  }
  
}

box_values <- vector("numeric", length(boxes))

for (i_box in seq_along(boxes)) {
  box_values[i_box] <- 
    sum((1 + i_box - 1) * as.numeric(boxes[[i_box]]) * (1:length(boxes[[i_box]])))
}

sum(box_values)
# 236057

Sys.time() - start
