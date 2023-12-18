
# Example input, if needed
day18 <- readLines("./puzzle_input/input_day18_exp.txt")

# day18 <- readLines("./puzzle_input/input_day18.txt")

day18 <- as.data.frame(do.call(rbind, strsplit(day18, " ")))
names(day18) <- c("direction", "steps", "colour")
day18$steps <- as.numeric(day18$steps)
day18$colour <- gsub("[()]", "", day18$colour)

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# Get all node coordinates

day18$steps_converted <- 
  ifelse(day18$direction %in% c("L", "U"), -day18$steps, day18$steps)

day18$x <- 
  cumsum(ifelse(day18$direction %in% c("U", "D"), day18$steps_converted, 0)) + 1

day18$y <- 
  cumsum(ifelse(day18$direction %in% c("L", "R"), day18$steps_converted, 0)) + 1

day18 <- 
  rbind.data.frame(
    data.frame(
      direction = "",
      steps = 0,
      colour = "",
      steps_converted = 0,
      x = 1,
      y = 1
    ),
    day18
  )

day18 <- day18[nrow(day18):1, ]

# The honest part is where I googled and found the Shoelace algorithm, but then 
# I cheated a little bit because Reddit told me that I needed to take care of 
# the outline as well.

a <- 0

for (i in 1:(nrow(day18) - 1)) {
  a <- a + day18$x[i] * day18$y[i + 1] - day18$y[i] * day18$x[i + 1] + 
    day18$steps[i] # to include the outside border as well
}

# I don't have a clue why it's + 1 :-D
a / 2 + 1 
# 68115

Sys.time() - start

## -----------------------------------------------------------------------------

# day18 <- readLines("./puzzle_input/input_day18_exp.txt")

day18 <- readLines("./puzzle_input/input_day18.txt")

day18 <- as.data.frame(do.call(rbind, strsplit(day18, " ")))
names(day18) <- c("direction", "steps", "colour")
day18$steps <- as.numeric(day18$steps)
day18$colour <- gsub("[()]", "", day18$colour)

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

day18$steps <- strtoi(substr(day18$colour, 2, 6), base = 16)
day18$direction <- substr(day18$colour, 7, 7)
day18$direction <- 
  factor(day18$direction, levels = 0:3, labels = c("R", "D", "L", "U"))

# Get coordinates
day18$steps_converted <- 
  ifelse(day18$direction %in% c("L", "U"), -day18$steps, day18$steps)

day18$x <- 
  cumsum(ifelse(day18$direction %in% c("U", "D"), day18$steps_converted, 0)) + 1

day18$y <- 
  cumsum(ifelse(day18$direction %in% c("L", "R"), day18$steps_converted, 0)) + 1

day18 <- 
  rbind.data.frame(
    data.frame(
      direction = "",
      steps = 0,
      colour = "",
      steps_converted = 0,
      x = 1,
      y = 1
    ),
    day18
  )

day18 <- day18[nrow(day18):1, ]

a <- 0

for (i in 1:(nrow(day18) - 1)) {
  a <- a + day18$x[i] * day18$y[i + 1] - day18$y[i] * day18$x[i + 1] + 
    day18$steps[i] # to include the outside border as well
}

options(scipen = 999)

# I don't have a clue why it's + 1 :-D
a / 2 + 1 
# 71262565063800

Sys.time() - start
