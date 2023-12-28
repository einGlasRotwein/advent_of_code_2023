
scenic_walk <- function(old_paths, nodes) {
  
  new_paths <- list()
  
  for (i in seq_along(old_paths)) {
    temp_node <- old_paths[[i]][[length(old_paths[[i]])]]
    
    if (nodes[temp_node] == "v") {
      
      temp_neighbours <- temp_node + 1
      
    } else if (nodes[temp_node] == "^") {
      
      temp_neighbours <- temp_node - 1
      
    } else if (nodes[temp_node] == ">") {
      
      temp_neighbours <- temp_node + nrow(nodes)
      
    } else if (nodes[temp_node] == "<") {
      
      temp_neighbours <- temp_node - nrow(nodes)
      
    } else if (nodes[temp_node] == ".") {
      
      # Get all neighbours of the current location.
      temp_neighbours <- c(
        temp_node + 1, # down
        temp_node - 1, # up
        temp_node + nrow(nodes), # right
        temp_node - nrow(nodes) # left
      )
      
    }
    
    # If a node is at the top, it e.g. can't go up.
    # If a node is at the bottom (position == nrow), it can't go to the top
    # (position == nrow + 1)
    node_idx <- 
      c(
        temp_node %% nrow(nodes) != 0, # bottom row (can't go down)
        (temp_node - 1) %% nrow(nodes) != 0, # top row (can't go up)
        !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes), # right col (can't go right)
        !temp_node %in% 1:nrow(nodes) # left col (can't go left)
      )
    
    temp_neighbours <- temp_neighbours[node_idx]
    
    # Remove all nodes that are out of bound
    temp_neighbours <- temp_neighbours[temp_neighbours %in% 1:length(nodes)]
    
    # Discard all neighbours that are forest
    temp_neighbours <- temp_neighbours[nodes[temp_neighbours] != "#"]
    
    # Don't go to any previous location
    temp_neighbours <- temp_neighbours[!temp_neighbours %in% old_paths[[i]]]
    
    # If neighbours are empty (i.e. there are no more valid moves from 
    # this position), don't add to path.
    if (length(temp_neighbours) != 0) {
      
      temp_continue <- 
        expand.grid(
          temp_node,
          temp_neighbours,
          stringsAsFactors = FALSE
        )
      
      temp_continue <- split(temp_continue, seq(nrow(temp_continue)))
      temp_continue <- lapply(temp_continue, function(x) unname(unlist(x)))
      
      temp_add_paths <-
        lapply(temp_continue, function(x){
          c(old_paths[[i]], x[-1])
        })
      
      new_paths <- c(new_paths, temp_add_paths)
      
    }
  }
  
  # Reduce: Keep only 1 path for each end node (because we don't care how we 
  # got there - they all have the same length)
  end_nodes <- sapply(new_paths, function(x) x[length(x)])
  
  new_paths <- new_paths[match(unique(end_nodes), end_nodes)]
  
  return(new_paths)
}
