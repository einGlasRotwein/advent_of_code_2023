
# NOTE: THIS CODE TAKES 10 HOURS TO RUN!

library(igraph)

# Example input, if needed
# day23 <- readLines("./puzzle_input/input_day23_exp.txt")

day23 <- readLines("./puzzle_input/input_day23.txt")

day23 <- do.call(rbind, strsplit(day23, ""))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

start_tile <- nrow(day23) * (which(day23[1, ] == ".") - 1) + 1
end_tile <- nrow(day23) * (which(day23[nrow(day23), ] == ".") - 1) + nrow(day23)

# Turn all slopes into ground tiles
day23[day23 %in% c("v", "^", "<", ">")] <- "."

# Find all vertices, i.e. all . tiles where more than two corner tiles are .
ground <- which(day23 == ".")
is_node <- rep(FALSE, length(ground))

for (i in seq_along(ground)) {
  
  # If up AND down or left AND right are #, it's not a node
  not_a_node <- 
    (day23[ground[i] + 1] == "#" & day23[ground[i] - 1] == "#") |
    (day23[ground[i] + nrow(day23)] == "#" & day23[ground[i] - nrow(day23)] == "#")
  
  is_node[i] <- !not_a_node
  
}

ground <- ground[is_node]

# Junctions are critical nodes
is_critical <- rep(FALSE, length(ground))

for (i in seq_along(ground)) {
  
  is_critical[i] <- 
    sum(
      day23[
        c(
          ground[i] + 1, 
          ground[i] - 1, 
          ground[i] + nrow(day23), 
          ground[i] - nrow(day23)
        )
      ] == "#"
    ) < 2
  
}

# VERY cumbersome way to create a network from the matrix, lol

vertices <- c(start_tile, ground, end_tile)

vertices_coordinates <- 
  data.frame(
    x = ifelse(vertices %% nrow(day23) == 0, nrow(day23), vertices %% nrow(day23)),
    y = ceiling(vertices / nrow(day23)),
    is_critical = c(TRUE, is_critical, TRUE)
  )

vertices_coordinates <- 
  vertices_coordinates[order(vertices_coordinates$x, vertices_coordinates$y), ]

rownames(vertices_coordinates) <- NULL

nw_info <- data.frame(from = c(), to = c(), weight = c())

for (i in 1:nrow(vertices_coordinates)) {
  
  # Get adjacent node to the right or down (left and up should be redundant),
  # i.e. the nearest nodes right/down. Determine whether there's a connection 
  # and how long the edge is.
  temp_tile <- day23[vertices_coordinates$x[i], vertices_coordinates$y[i]]
  
  right <- 
    vertices_coordinates[
      vertices_coordinates$x == vertices_coordinates$x[i] &
        vertices_coordinates$y > vertices_coordinates$y[i],
    ]
  
  down <- 
    vertices_coordinates[
      vertices_coordinates$y == vertices_coordinates$y[i] &
        vertices_coordinates$x > vertices_coordinates$x[i], 
    ]
  
  if (nrow(right) != 0) {
    
    clear_path <- 
      all(
        day23[vertices_coordinates$x[i], vertices_coordinates$y[i]:right$y[1]] != "#"
      )
    
    if (clear_path) {
      
      nw_info <- 
        rbind.data.frame(
          nw_info,
          data.frame(
            from = paste(vertices_coordinates[i, 1:2], collapse = "-"), 
            to = paste(right[1, 1:2], collapse = "-"), 
            weight = right$y[1] - vertices_coordinates$y[i]
          )
        )
      
    }
    
  }
  
  if (nrow(down) != 0) {
    
    clear_path <- 
      all(
        day23[vertices_coordinates$x[i]:down$x[1], vertices_coordinates$y[i]] != "#"
      )
    
    if (clear_path) {
      
      nw_info <- 
        rbind.data.frame(
          nw_info,
          data.frame(
            from = paste(vertices_coordinates[i, 1:2], collapse = "-"), 
            to = paste(down[1, 1:2], collapse = "-"), 
            weight = down$x[1] - vertices_coordinates$x[i]
          )
        ) 
      
    }
    
  }
  
}

# Repeat all nodes on other direction.
nw_info <- 
  rbind.data.frame(
    nw_info,
    data.frame(
      from = nw_info$to,
      to = nw_info$from,
      weight = nw_info$weight
    )
  )

# Delete non-critical nodes
vertices_coordinates$id <- 
  apply(
    vertices_coordinates[, 1:2], 
    1, 
    function(x) paste(x, collapse = "-")
  )

critical_vertices <- 
  nw_info[
    nw_info$from %in% vertices_coordinates$id[vertices_coordinates$is_critical], 
  ]

rownames(critical_vertices) <- NULL

new_vertices <- data.frame(from = c(), to = c(), weight = c())

for (i in 1:nrow(critical_vertices)) {
  
  temp_from <- critical_vertices$from[i]
  temp_to <- critical_vertices$to[i]
  weight_counter <- critical_vertices$weight[i]
  
  # Find next critical node and delete all in between
  while (!temp_to %in% critical_vertices$from) {
    
    previous <- temp_to
    
    temp_weight <- 
      nw_info$weight[
        nw_info$from == temp_to & nw_info$to != temp_from
      ]
    
    temp_to <- 
      nw_info$to[
        nw_info$from == temp_to & nw_info$to != temp_from
      ]
    
    if (length(temp_to) == 0) break
    
    temp_from <- previous
    
    weight_counter <- weight_counter + temp_weight
    
  }
  
  if (length(temp_to) == 0) next
  
  new_vertices <- 
    rbind.data.frame(
      new_vertices,
      data.frame(
        from = critical_vertices$from[i],
        to = temp_to,
        weight = weight_counter
      )
    )
  
}

nw <- graph_from_edgelist(as.matrix(new_vertices[ , 1:2]), directed = TRUE)

# E(nw)$weight <- new_vertices$weight

nw <- as.undirected(nw, "collapse")

all_paths <- 
  all_simple_paths(
    nw,
    paste0(vertices_coordinates[1, 1:2], collapse = "-"), 
    paste0(vertices_coordinates[nrow(vertices_coordinates), 1:2], collapse = "-")
  )

# It seems like I can't simply get the weight of a path?!

path_weights <- vector("numeric", length(all_paths))

for (i in seq_along(all_paths)) {
  
  temp_weight <- 0
  
  for (j in 1:(length(all_paths[[i]]) - 1)) {
    
    x <- names(all_paths[[i]][j])
    y <- names(all_paths[[i]][j + 1])
    temp_weight <-  temp_weight +
      new_vertices$weight[new_vertices$from == x & new_vertices$to == y]
    
  }
  
  path_weights[i] <- temp_weight
  
}

max(path_weights)
# 6350

Sys.time() - start
