
# Example input, if needed
# day04 <- readLines("puzzle_input/input_day04_exp.txt")

day04 <- readLines("puzzle_input/input_day04.txt")

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

day04 <- sub("Card.*[0-9]+: ", "", day04)

# Split into winning numbers and my numbers
day04 <- 
  lapply(
    day04, 
    function(x) lapply(
      strsplit(x, " | ", fixed = TRUE), 
      function(y) strsplit(y, " ")
    )
  )

# Check how many winning numbers we got per card
n_winners <- vector("numeric", length(day04))

for (i_card in seq_along(day04)) {
  winning_nums <- day04[[i_card]][[1]][[1]]
  drawn_nums <- day04[[i_card]][[1]][[2]]
  
  winning_nums <- winning_nums[winning_nums != ""]
  drawn_nums <- drawn_nums[drawn_nums != ""]
  
  n_winners[i_card] <- sum(drawn_nums %in% winning_nums)
}

# Each match is worth 1 point, then doubled for every further match.
# (Pretty sure there's a more elegant way if you're not too stupid for math :-D)
points_won <- 
  ifelse(
    n_winners == 0,
    0,
    2 ** ifelse(n_winners != 0, n_winners - 1, n_winners)
  )

sum(points_won)
# 23941

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

n_copies <- rep(1, length(points_won))

# How many copies of each card do I win?
for (i_card in seq_along(n_copies)) {
  if (n_winners[i_card] != 0) {
    copies <- i_card + 1:n_winners[i_card]
    copies <- copies[copies <= length(n_copies)]
    n_copies[copies] <- n_copies[copies] + n_copies[i_card] 
  }
}

sum(n_copies)
# 5571760

Sys.time() - start
