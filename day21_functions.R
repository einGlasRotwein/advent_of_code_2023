
# Don't save entire path to save memory

walking_elf <- function(old_paths, nodes) {
  
  new_paths <- vector("numeric", 0)
  
  for (i in seq_along(old_paths)) {
    
    temp_node <- old_paths[i]
    
    # Get all neighbours of the current location.
    temp_neighbours <- c(
      temp_node + 1, # down
      temp_node - 1, # up
      temp_node + nrow(nodes), # right
      temp_node - nrow(nodes) # left
    )
    
    # Remove nodes out of bound
    node_idx <- 
      c(
        temp_node %% nrow(nodes) != 0, # bottom row (can't go down)
        (temp_node - 1) %% nrow(nodes) != 0, # top row (can't go up)
        !(temp_node - 1) %in% (length(nodes) - nrow(nodes) + 1):length(nodes), # right col (can't go right)
        !temp_node %in% 1:nrow(nodes) # left col (can't go left)
      )
    
    temp_neighbours <- temp_neighbours[node_idx]
    
    temp_neighbours <- temp_neighbours[temp_neighbours %in% 1:length(nodes)]
    
    # Don't walk on rocks
    temp_neighbours <- temp_neighbours[!temp_neighbours %in% which(nodes == "#")]
    
    # If neighbours are empty (i.e. there are no more valid moves from 
    # this position), don't add to path.
    if (length(temp_neighbours) != 0) {
      
      new_paths <- unique(c(new_paths, temp_neighbours))
    }
    
  }
  
  return(new_paths)
}
