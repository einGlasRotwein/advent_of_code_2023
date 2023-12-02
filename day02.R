
library(tidyverse)

day02 <- readLines("puzzle_input/input_day02.txt")

process_colour <- function(game, colour_name) {
  matches <- regexec(paste0("[0-9]+ ", colour_name), game)
  colours <- substr(
    game, 
    sapply(matches, function(x) x), 
    sapply(matches, function(x) x) + 
      sapply(matches, function(x) attr(x, "match.length") - 1)
  )
  colours <- as.numeric(gsub("[^0-9]+", "", colours))
  colours <- ifelse(is.na(colours), 0, colours)
  
  return(colours)
}

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# Split into games and draws
games <- 
  lapply(
    strsplit(day02, ": "), function(x) {
      game_id <- as.numeric(gsub("[^0-9]", "", x[1]))
      games <- trimws(unlist(strsplit(x[2], ";")))
    }
  )

# Extract colour counts per game x draw
colour_counts <- vector("list", length(games))

for (i_game in seq_along(games)) {
  reds <- process_colour(games[[i_game]], "red")
  blues <- process_colour(games[[i_game]], "blue")
  greens <- process_colour(games[[i_game]], "green")
  
  colour_counts[[i_game]] <- list(red = reds, blue = blues, green = greens)
}

# Games possible with
# 12 red cubes, 13 green cubes, and 14 blue cubes
possible_games <- 
  which(
    sapply(colour_counts, function(x) all(x$red <= 12)) &
      sapply(colour_counts, function(x) all(x$green <= 13)) &
      sapply(colour_counts, function(x) all(x$blue <= 14))
  )

sum(possible_games)
# 2348

start - Sys.time()

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

# Minimum set of cubes: Product of the maximum number for each colour

set_powers <- 
  sapply(colour_counts, function(x) {
    prod(sapply(x, function(y) max(y)))
  })

sum(set_powers)
# 76008

start - Sys.time()
