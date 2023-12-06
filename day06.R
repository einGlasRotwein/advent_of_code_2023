
# Example input, if needed
# day06 <- read.table("puzzle_input/input_day06_exp.txt")

day06 <- read.table("puzzle_input/input_day06.txt")

day06 <- as.data.frame(t(day06))
names(day06) <- sub(":", "", day06[1, ])
day06 <- day06[-1, ]
rownames(day06) <- NULL
day06 <- data.frame(lapply(day06, as.numeric))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

day06$n_ways <- NA # number of ways to win

for (i_row in 1:nrow(day06)) {
  race_time <- day06$Time[i_row]
  button_pressed <- 1:race_time
  record <- day06$Distance[i_row]
  
  distances_possible <- (race_time - button_pressed) * button_pressed
  day06$n_ways[i_row] <- length(distances_possible[distances_possible > record]) 
}

prod(day06$n_ways)
# 588588

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

# Lol, I thought my solution for part 1 would be too "expensive" and I needed 
# something more efficient for part 2, but it turns out, it works just like 
# that.

start <- Sys.time()

race_time <- as.numeric(paste(day06$Time, collapse = ""))
button_pressed <- 1:race_time
record <- as.numeric(paste(day06$Distance, collapse = ""))

distances_possible <- (race_time - button_pressed) * button_pressed
length(distances_possible[distances_possible > record]) 
# 34655848

Sys.time() - start
