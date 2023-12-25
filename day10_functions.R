
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
