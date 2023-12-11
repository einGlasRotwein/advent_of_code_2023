
# Example input, if needed
# day11 <- readLines("puzzle_input/input_day11_exp.txt")

day11 <- readLines("puzzle_input/input_day11.txt")
day11 <- strsplit(day11, "")
day11 <- as.matrix(do.call(rbind, day11))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# If there's an empty row between the two galaxies, that needs to count n-times.
expansion <- 2

# Identify all rows and columns without galaxies.

empty_rows <- which(apply(day11, 1, function(x) all(x == ".")))
empty_cols <- which(apply(day11, 2, function(x) all(x == ".")))

# Find all galaxies
galaxies <- as.data.frame(which(day11 == "#", arr.ind = TRUE))
galaxies$id <- 1:nrow(galaxies)

# All pairs of galaxies
galaxy_pairs <- t(combn(galaxies$id, 2))
galaxy_distances <- vector("numeric", nrow(galaxy_pairs))

pb <- 
  txtProgressBar(
    min = 0, max = length(galaxy_distances), initial = 0, style = 3
  ) 

for (i in seq_along(galaxy_distances)) {
  galaxy1 <- galaxies[galaxy_pairs[i, 1], c(1, 2)]
  galaxy2 <- galaxies[galaxy_pairs[i, 2], c(1, 2)]
  
  dist <- sum(abs(galaxy1 - galaxy2))
  
  # Add expansion, i.e. number of empty rows + cols
  dist <- 
    dist + 
    sum(
      empty_rows > min(galaxy1$row, galaxy2$row) & 
        empty_rows < max(galaxy1$row, galaxy2$row)
    ) *
    (expansion - 1)
  
  dist <- 
    dist + 
    sum(
      empty_cols > min(galaxy1$col, galaxy2$col) & 
        empty_cols < max(galaxy1$col, galaxy2$col)
    ) *
    (expansion - 1)
  
  galaxy_distances[i] <- dist
  
  setTxtProgressBar(pb, i)
}

close(pb)

sum(galaxy_distances)
# 9693756

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

expansion <- 1000000

galaxy_distances <- vector("numeric", nrow(galaxy_pairs))

pb <- 
  txtProgressBar(
    min = 0, max = length(galaxy_distances), initial = 0, style = 3
  ) 

for (i in seq_along(galaxy_distances)) {
  galaxy1 <- galaxies[galaxy_pairs[i, 1], c(1, 2)]
  galaxy2 <- galaxies[galaxy_pairs[i, 2], c(1, 2)]
  
  dist <- sum(abs(galaxy1 - galaxy2))
  
  # Add expansion, i.e. number of empty rows + cols
  dist <- 
    dist + 
    sum(
      empty_rows > min(galaxy1$row, galaxy2$row) & 
        empty_rows < max(galaxy1$row, galaxy2$row)
    ) *
    (expansion - 1)
  
  dist <- 
    dist + 
    sum(
      empty_cols > min(galaxy1$col, galaxy2$col) & 
        empty_cols < max(galaxy1$col, galaxy2$col)
    ) *
    (expansion - 1)
  
  galaxy_distances[i] <- dist
  
  setTxtProgressBar(pb, i)
}

close(pb)

sum(galaxy_distances)
# 717878258016

Sys.time() - start
