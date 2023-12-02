
library(tidyverse)

day02 <- readLines("puzzle_input/input_day02.txt")

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
  red_matches <- regexec("[0-9]+ red", games[[i_game]])
  reds <- substr(
    games[[i_game]], 
    sapply(red_matches, function(x) x), 
    sapply(red_matches, function(x) x) + 
      sapply(red_matches, function(x) attr(x, "match.length") - 1)
  )
  reds <- as.numeric(gsub("[^0-9]+", "", reds))
  reds <- ifelse(is.na(reds), 0, reds)
  
  blue_matches <- regexec("[0-9]+ blue", games[[i_game]])
  blues <- substr(
    games[[i_game]], 
    sapply(blue_matches, function(x) x), 
    sapply(blue_matches, function(x) x) + 
      sapply(blue_matches, function(x) attr(x, "match.length") - 1)
  )
  blues <- as.numeric(gsub("[^0-9]+", "", blues))
  blues <- ifelse(is.na(blues), 0, blues)
  
  green_matches <- regexec("[0-9]+ green", games[[i_game]])
  greens <- substr(
    games[[i_game]], 
    sapply(green_matches, function(x) x), 
    sapply(green_matches, function(x) x) + 
      sapply(green_matches, function(x) attr(x, "match.length") - 1)
  )
  greens <- as.numeric(gsub("[^0-9]+", "", greens))
  greens <- ifelse(is.na(greens), 0, greens)
  
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
