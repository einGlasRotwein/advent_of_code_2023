
# Example input, if needed
# day03 <- readLines("puzzle_input/input_day03_exp.txt")

day03 <- readLines("puzzle_input/input_day03.txt")
day03 <- strsplit(day03, "")
day03 <- t(matrix(unlist(day03), nrow = length(day03)))

## FUNCTIONS -------------------------------------------------------------------

# Get neighbours in a matrix, given a position. Uses the matrix only for 
# boundaries.
get_neighbours <- 
  function(x, y, mat) {
    x_cors <- rep(c(x - 1, x, x + 1), each = 3)
    y_cors <- rep(c(y - 1, y, y + 1), 3)
    
    cors <- cbind(x_cors, y_cors)
    
    # Exclude origin
    cors <- cors[!(cors[, 1] == x & cors[, 2] == y), ]
    
    # Exclude everything outside matrix dimensions
    cors <- cors[cors[ , 1] <= nrow(mat), ]
    cors <- cors[cors[ , 2] <= ncol(mat), ]
    
    return(cors)
  }

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# Turn all symbols into a single symbol for easier searching
part1 <- sub("[^0-9.]", "x", day03)

# Get number positions
number_pos <- arrayInd(which(part1 %in% 0:9), dim(part1))
number_pos <- data.frame(number_pos[order(number_pos[ , 1]), ])

# Get number digits
number_pos$digit <- part1[cbind(number_pos$X1, number_pos$X2)]

# Digits that are adjacent on the x-axis belong to the same number. Assign ID.
number_pos$dist <- c(1, diff(number_pos$X2))
number_pos$num_id <- cumsum(number_pos$dist != 1) + 1

# Get neighbours for every number and check whether one of them is a symbol (x)
number_pos$sym_neighbour <- NA

for (i_pos in 1:nrow(number_pos)) {
  temp_neighbours <- 
    part1[
      get_neighbours(
        number_pos$X1[i_pos], 
        number_pos$X2[i_pos], part1
      )
    ]
  
  number_pos$sym_neighbour[i_pos] <- any(temp_neighbours == "x")
}

# Identify all numbers that have at least one neighbouring symbol
have_neighbours <- 
  which(as.vector(by(number_pos$sym_neighbour, number_pos$num_id, sum)) > 0)

# Get numbers
have_neighbours <- number_pos[number_pos$num_id %in% have_neighbours, ]

target_nums <- 
  by(
    have_neighbours$digit, 
    have_neighbours$num_id, 
    function(x) as.numeric(paste0(x, collapse = ""))
  )

sum(target_nums)
# 550934

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

# Remove all symbols that are not *
part2 <- sub("[^0-9.*]", ".", day03)

# Get * positions and give them an ID
gear_pos <- data.frame(which(part2 == "*", arr.ind = TRUE))
gear_pos$gear_ratio <- NA

# This time, it's more efficient to work with the neighbours of the gears, not 
# the digits.
for (i_gear in 1:nrow(gear_pos)) {
  temp_neighbours <- 
    get_neighbours(
      gear_pos$row[i_gear], 
      gear_pos$col[i_gear], part2
    )
  
  # Any neighbours that are digits?
  if(any(grepl("[0-9]", part2[temp_neighbours]))) {
    # Check whether the gear is adjacent to exactly 2 numbers
    # Use number positions from previous step
    neighbour_nums <- 
      number_pos[
        paste0(number_pos$X1, "-", number_pos$X2) %in% 
          paste0(temp_neighbours[ , 1], "-", temp_neighbours[ , 2]),
      ]
    
    # If only two neighbouring numbers, identify and multiply them
    if(length(unique(neighbour_nums$num_id)) == 2) {
      neighbour_nums <- 
        number_pos[number_pos$num_id %in% neighbour_nums$num_id, ]
      
      neighbour_nums <-
        by(
          neighbour_nums$digit, 
          neighbour_nums$num_id, 
          function(x) as.numeric(paste0(x, collapse = ""))
        )
      
      gear_pos$gear_ratio[i_gear] <- prod(neighbour_nums)
    }
  }
}

sum(gear_pos$gear_ratio, na.rm = TRUE)
# 81997870

Sys.time() - start
