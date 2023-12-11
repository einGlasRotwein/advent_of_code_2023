
pipe_travel <- function(paths, nodes, initial_path_lengths) {
  
  for (i in seq_along(paths)) {
    temp_node <- paths[[i]][length(paths[[i]])]
    temp_previous_node <- paths[[i]][1]
    
    # Determine what kind of pipe we have
    temp_pipe <- day10[temp_node]
    
    # Go up or down, depending on where we came from
    if (temp_pipe == "|") {
      
      temp_neighbours <- c(
        temp_node + 1, # down
        temp_node - 1 # up
      )
      
      # Check whether we can go down/up (e.g., can't go 
      # down if temp_node is already at the bottom)
      node_idx <- 
        c(
          temp_node %% nrow(nodes) != 0, 
          (temp_node - 1) %% nrow(nodes) != 0
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "L") {
      
      temp_neighbours <- c(
        temp_node - 1, # up
        temp_node + nrow(nodes) # right
      )
      
      node_idx <- 
        c(
          (temp_node - 1) %% nrow(nodes) != 0,
          !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes)
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "-") {
      
      temp_neighbours <- c(
        temp_node - nrow(nodes), # left
        temp_node + nrow(nodes) # right
      )
      
      node_idx <- 
        c(
          !temp_node %in% 1:nrow(nodes),
          !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes)
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "7") {
      
      temp_neighbours <- c(
        temp_node - nrow(nodes), # left
        temp_node + 1 # down
      )
      
      node_idx <- 
        c(
          !temp_node %in% 1:nrow(nodes),
          temp_node %% nrow(nodes) != 0
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "J") {
      
      temp_neighbours <- c(
        temp_node - nrow(nodes), # left
        temp_node - 1 # up
      )
      
      node_idx <- 
        c(
          !temp_node %in% 1:nrow(nodes),
          (temp_node - 1) %% nrow(nodes) != 0
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "F") {
      
      temp_neighbours <- c(
        temp_node + 1, # down
        temp_node + nrow(nodes) # right
      )
      
      node_idx <- 
        c(
          temp_node %% nrow(nodes) != 0, 
          !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes)
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    }
    
    # Don't go to the previous location
    # Remove previous previous position to manage memory
    paths[[i]] <- 
      c(
        paths[[i]][-1], 
        temp_neighbours[temp_neighbours != temp_previous_node]
      )
    
    initial_path_lengths[i] <- initial_path_lengths[i] + 1
    
  }
  
  return(list(paths, initial_path_lengths))
  
}

# Stores the entire path
map_pipe_travel <- function(paths, nodes) {
  
  for (i in seq_along(paths)) {
    temp_node <- paths[[i]][length(paths[[i]])]
    temp_previous_node <- paths[[i]][length(paths[[i]]) - 1]
    
    # Determine what kind of pipe we have
    temp_pipe <- day10[temp_node]
    
    # Go up or down, depending on where we came from
    if (temp_pipe == "|") {
      
      temp_neighbours <- c(
        temp_node + 1, # down
        temp_node - 1 # up
      )
      
      # Check whether we can go down/up (e.g., can't go 
      # down if temp_node is already at the bottom)
      node_idx <- 
        c(
          temp_node %% nrow(nodes) != 0, 
          (temp_node - 1) %% nrow(nodes) != 0
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "L") {
      
      temp_neighbours <- c(
        temp_node - 1, # up
        temp_node + nrow(nodes) # right
      )
      
      node_idx <- 
        c(
          (temp_node - 1) %% nrow(nodes) != 0,
          !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes)
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "-") {
      
      temp_neighbours <- c(
        temp_node - nrow(nodes), # left
        temp_node + nrow(nodes) # right
      )
      
      node_idx <- 
        c(
          !temp_node %in% 1:nrow(nodes),
          !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes)
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "7") {
      
      temp_neighbours <- c(
        temp_node - nrow(nodes), # left
        temp_node + 1 # down
      )
      
      node_idx <- 
        c(
          !temp_node %in% 1:nrow(nodes),
          temp_node %% nrow(nodes) != 0
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "J") {
      
      temp_neighbours <- c(
        temp_node - nrow(nodes), # left
        temp_node - 1 # up
      )
      
      node_idx <- 
        c(
          !temp_node %in% 1:nrow(nodes),
          (temp_node - 1) %% nrow(nodes) != 0
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    } else if (temp_pipe == "F") {
      
      temp_neighbours <- c(
        temp_node + 1, # down
        temp_node + nrow(nodes) # right
      )
      
      node_idx <- 
        c(
          temp_node %% nrow(nodes) != 0, 
          !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes)
        )
      
      temp_neighbours <- temp_neighbours[node_idx]
      
    }
    
    paths[[i]] <- 
      c(
        paths[[i]], 
        # Don't go to the previous location
        temp_neighbours[temp_neighbours != temp_previous_node]
      )
    
  }
  
  return(paths)
  
}

# Determine which tile the start pipe needs to be
missing_pipe <- function(start_idx, idx1, idx2) {
  diffs <- c(start_idx - idx1, start_idx - idx2)
  
  if (all(diffs < -0)) { # bottom + right
    return("F")
  } else if (all(diffs > 0)) { # top + left
    return("J")
  } else if (all(abs(diffs) == 1)) { # up + down
    return("|")
  } else if (all(abs(diffs) > 1)) { # left + right
    return("-")
  } else if (any(diffs > 1)) { # bottom + left
    return("7")
  } else if (any(diffs < -1)) { # top + right
    return("L")
  }
}

# Check whether a tile hits the border of the pipe map (i.e., a tile named 
# "b"). Takes into account whether we can "squeeze" through pipes to get the 
# next "actual" neighbour.
hits_border <- function(tile_idx, pipe_map) {
  
  # Get all neighbours of the current location.
  temp_neighbours <- c(
    tile_idx + 1, # down
    tile_idx - 1, # up
    tile_idx + nrow(pipe_map), # right
    tile_idx - nrow(pipe_map) # left
  )
  
  # If no border is hit, determine whether any neighbouring tile is part of a 
  # "gap" we could "squeeze through"
  if (any(pipe_map[temp_neighbours] == "b")) {
    
    return(TRUE)
    
  } else if (all(pipe_map[temp_neighbours] %in% c(".", "-", "|"))) {
    # If a straight part of the loop is hit, we can't squeeze through from this 
    # position.
    return(FALSE)
    
  } else {
    
    # "Squeeze through" until another border is found or squeezing through is 
    # not possible anymore.
    
    # DOWN LEFT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[1]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos - nrow(temp_map)]
      right <- temp_map[temp_pos]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos + 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # DOWN RIGHT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[1]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos]
      right <- temp_map[temp_pos + nrow(temp_map)]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos + 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # UP LEFT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[2]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos - nrow(temp_map)]
      right <- temp_map[temp_pos]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos - 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # UP RIGHT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[2]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos]
      right <- temp_map[temp_pos+ nrow(temp_map)]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos - 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # RIGHT UP
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[3]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos - 1]
      down <- temp_map[temp_pos]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos + nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # RIGHT DOWN
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[3]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos]
      down <- temp_map[temp_pos + 1]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos + nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # LEFT UP
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[4]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos - 1]
      down <- temp_map[temp_pos]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos - nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # LEFT DOWN
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[4]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos]
      down <- temp_map[temp_pos + 1]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos - nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    return(FALSE)
    
  }
}

get_all_neighbours <- function(tile_idx, pipe_map) {
  all_neighbours <- 
    list(
      tile_idx + 1 + nrow(pipe_map),
      tile_idx + 1,
      tile_idx + 1 - nrow(pipe_map),
      tile_idx - nrow(pipe_map),
      tile_idx - nrow(pipe_map) - 1,
      tile_idx - 1,
      tile_idx - 1 + nrow(pipe_map),
      tile_idx + nrow(pipe_map)
    )
  
  return(all_neighbours)
}

hits_ground <- function(tile_idx, pipe_map) {
  
  # Get all neighbours of the current location.
  temp_neighbours <- c(
    tile_idx + 1, # down
    tile_idx - 1, # up
    tile_idx + nrow(pipe_map), # right
    tile_idx - nrow(pipe_map) # left
  )
  
  if (all(pipe_map[temp_neighbours] %in% c("x", "y"))) return(FALSE)
  
  # If no ground tile is hit, determine whether any neighbouring tile is part of a 
  # "gap" we could "squeeze through"
  if (any(pipe_map[temp_neighbours] == ".")) {
    
    return(TRUE)
    
    # If no ground tile is hit, determine whether any neighbouring tile is part 
    # of a "gap" we could "squeeze through"
  } else {
    
    # "Squeeze through" until another ground tile is found or squeezing through 
    # in any direction is not possible anymore.
    
    # We can squeeze through in 8 possible directions:
    # up left, up right, right up, right down, down right, down left, 
    # left down, left up
    
    all_neighbours <- get_all_neighbours(tile_idx, pipe_map)
    
    # At any point, the direction may change.
    
    # We can e.g. squeeze up left when the top left position is %in%
    # c("|", "J", "7") AND the top position is %in% c("|", "F", "L"). 
    # Map these rules:
    
    # For down left/right and up left/right
    # Left in:
    c("|", "J", "7")
    # Right in:
    c("|", "F", "L")
    
    # For right up/down and left right/down
    # Up in:
    c("-", "J", "L")
    # Down in:
    c("-", "7", "F")
    
    # Determine whether we can squeeze through at any point at this location. 
    # If so, continue the respective path, check the neighbours again ...
    
    # TO DO FROM HERE
    
    
    
    # DOWN LEFT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[1]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos - nrow(temp_map)]
      right <- temp_map[temp_pos]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos + 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # DOWN RIGHT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[1]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos]
      right <- temp_map[temp_pos + nrow(temp_map)]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos + 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # UP LEFT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[2]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos - nrow(temp_map)]
      right <- temp_map[temp_pos]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos - 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # UP RIGHT
    # Squeeze through while there's a gap between left and right
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[2]
    
    while (squeeze_through) {
      left <- temp_map[temp_pos]
      right <- temp_map[temp_pos+ nrow(temp_map)]
      
      if (any(c(left, right) == "b")) return(TRUE)
      
      if (left %in% c("|", "J", "7") & right %in% c("|", "F", "L")) {
        temp_pos <- temp_pos - 1
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # RIGHT UP
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[3]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos - 1]
      down <- temp_map[temp_pos]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos + nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # RIGHT DOWN
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[3]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos]
      down <- temp_map[temp_pos + 1]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos + nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # LEFT UP
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[4]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos - 1]
      down <- temp_map[temp_pos]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos - nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    # LEFT DOWN
    # Squeeze through while there's a gap between up and down
    squeeze_through <- TRUE
    temp_pos <- temp_neighbours[4]
    
    while (squeeze_through) {
      up <- temp_map[temp_pos]
      down <- temp_map[temp_pos + 1]
      
      if (any(c(up, down) == "b")) return(TRUE)
      
      if (up %in% c("-", "J", "L") & down %in% c("-", "7", "F")) {
        temp_pos <- temp_pos - nrow(pipe_map)
      } else {
        squeeze_through <- FALSE
      }
    }
    
    return(FALSE)
    
  }
}
