
# Example input, if needed
# day07 <- read.table("puzzle_input/input_day07_exp.txt")

day07 <- read.table("puzzle_input/input_day07.txt")

names(day07) <- c("hand", "bid")

hands_split <- as.data.frame(do.call(rbind, strsplit(day07$hand, "")))
names(hands_split) <- paste0("hand", 1:5)

hands_split$bid <- day07$bid

# Function to identify the hand type. Hand is a vector of (five) cards.

# Five of a kind = 7
# Four of a kind = 6
# Full house = 5
# Three of a kind = 4
# Two pair = 3
# One pair = 2
# High card = 1

identify_hand <- function(hand) {
  
  hand_table <- table(hand)
  
  if (any(hand_table == 5)) { # Five of a kind
    return(7)
  } else if (any(hand_table == 4)) { # Four of a kind
    return(6)
  } else if (any(hand_table == 3) & any(hand_table == 2)) { # full house
    return(5)
  } else if (any(hand_table == 3)) { # three of a kind
    return(4)
  } else if (sum(hand_table == 2) == 2) { # two pair
    return(3)
  } else if (any(hand_table == 2)) { # one pair
    return(2)
  } else { # high card
    return(1)
  }
}

# Identify the hand, but if there's a joker (or more), use it to make the hand 
# stronger
wildcard_hand <- function(hand) {
  
  hand_table <- table(hand)
  
  if (any(hand == "J")) {
    # How many jokers?
    n_jokers <- hand_table[names(hand_table) == "J"]
    remaining_cards <- hand_table[names(hand_table) != "J"]
    
    if (any(remaining_cards + n_jokers == 5) | n_jokers == 5) { # Five of a kind
      return(7)
    } else if (any(remaining_cards + n_jokers == 4)) { # Four of a kind
      return(6)
    } else if (n_jokers == 1 & sum(remaining_cards == 2) == 2) { # full house
      # A full house is only an option when there's only one joker. When 
      # there's two jokers (or more) and a full house can be achieved, five or 
      # four of a kind can also be achieved, which are stronger.
      return(5)
    } else if (any(remaining_cards + n_jokers == 3)) { # three of a kind
      return(4)
    } else if (n_jokers == 1 & any(remaining_cards == 2)) { # two pair
      # Two pairs can only be achieved when there's only one joker. When 
      # there's two jokers (or more), three of a kind (or a higher hand) can be 
      # achieved, which are stronger.
      return(3)
    } else { # one pair
      # When there's a joker, at least one pair is possible
      return(2)
    }
    
  } else {
    return(identify_hand(hand))
  }
  
}

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

card_hierarchy <- c("A", "K", "Q", "J", "T", 9:2)

part1 <- hands_split

# First step: Identify type.
part1$hand_type <- 
  apply(part1[paste0("hand", 1:5)], 1, identify_hand)

# Establish card hierarchy
part1[paste0("hand", 1:5)] <- 
  as.data.frame(
    lapply(part1[paste0("hand", 1:5)], function(x) {
      as.numeric(factor(x, levels = card_hierarchy, ordered = TRUE))
    })
  )

# Now sort by hand type (descending), card 1, 2, 3 ...
part1_ordered <- 
  with(
    part1, 
    part1[order(-hand_type, hand1, hand2, hand3, hand4, hand5), ]
  )

# multiply bid with rank
sum(part1_ordered$bid * nrow(part1_ordered):1)
# 248453531

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

card_hierarchy <- c("A", "K", "Q", "T", 9:2, "J")

part2 <- hands_split

# Identify type.
part2$hand_type <- 
  apply(part2[paste0("hand", 1:5)], 1, wildcard_hand)

# Establish card hierarchy
part2[paste0("hand", 1:5)] <- 
  as.data.frame(
    lapply(part2[paste0("hand", 1:5)], function(x) {
      as.numeric(factor(x, levels = card_hierarchy, ordered = TRUE))
    })
  )

# sort
part2_ordered <- 
  with(
    part2, 
    part2[order(-hand_type, hand1, hand2, hand3, hand4, hand5), ]
  )

# multiply bid with rank
sum(part2_ordered$bid * nrow(part2_ordered):1)
# 248781813

Sys.time() - start
