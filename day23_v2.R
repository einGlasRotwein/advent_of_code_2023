
library(igraph)

# Example input, if needed
# day23 <- readLines("./puzzle_input/input_day23_exp.txt")

day23 <- readLines("./puzzle_input/input_day23.txt")

day23 <- do.call(rbind, strsplit(day23, ""))

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

start_tile <- nrow(day23) * (which(day23[1, ] == ".") - 1) + 1
end_tile <- nrow(day23) * (which(day23[nrow(day23), ] == ".") - 1) + nrow(day23)

# Find all vertices, i.e. all ^, v, > and < tiles, and all . tiles where two 
# corner tiles (or more) are .
slopes <- which(day23 %in% c("v", "^", "<", ">"))
ground <- which(day23 == ".")
is_node <- rep(FALSE, length(ground))

for (i in seq_along(ground)) {
  
  # If up AND down or left AND right are #, it's not a node
  not_a_node <- 
    (day23[ground[i] + 1] == "#" & day23[ground[i] - 1] == "#") |
    (day23[ground[i] + nrow(day23)] == "#" & day23[ground[i] - nrow(day23)] == "#")
  
  is_node[i] <- !not_a_node
  
}

# VERY cumbersome way to create a network from the matrix, lol

vertices <- c(start_tile, slopes, ground[is_node], end_tile)

vertices_coordinates <- 
  data.frame(
    x = ifelse(vertices %% nrow(day23) == 0, nrow(day23), vertices %% nrow(day23)),
    y = ceiling(vertices / nrow(day23))
  )

vertices_coordinates <- 
  vertices_coordinates[order(vertices_coordinates$x, vertices_coordinates$y), ]

rownames(vertices_coordinates) <- NULL

nw_info <- data.frame(from = c(), to = c(), weight = c())

for (i in 1:nrow(vertices_coordinates)) {
  
  # Get adjacent node to the right or down (left and up should be redundant),
  # i.e. the nearest nodes right/down. Determine whether there's a connection 
  # and how long the edge is.
  # If the edge is directed, only add to -> from - if it's not directed, add 
  # to -> from and from -> to
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
  
  if (temp_tile %in% c(".", ">")) {
    
    if (nrow(right) != 0) {
      
      clear_path <- 
        all(
          day23[vertices_coordinates$x[i], vertices_coordinates$y[i]:right$y[1]] != "#"
        )
      
      if (clear_path) {
        
        if (day23[right$x[1], right$y[1]] == "<") {
          
          nw_info <- 
            rbind.data.frame(
              nw_info,
              data.frame(
                from = paste(right[1, ], collapse = "-"), 
                to = paste(vertices_coordinates[i, ], collapse = "-"), 
                weight = right$y[1] - vertices_coordinates$y[i]
              )
            )
          
        } else if (temp_tile == ">") {
          
          nw_info <- 
            rbind.data.frame(
              nw_info,
              data.frame(
                from = paste(vertices_coordinates[i, ], collapse = "-"), 
                to = paste(right[1, ], collapse = "-"), 
                weight = right$y[1] - vertices_coordinates$y[i]
              )
            ) 
          
        } else {
          
          nw_info <- 
            rbind.data.frame(
              nw_info,
              data.frame(
                from = c(
                  paste(vertices_coordinates[i, ], collapse = "-"),
                  paste(right[1, ], collapse = "-")
                ), 
                to = c(
                  paste(right[1, ], collapse = "-"),
                  paste(vertices_coordinates[i, ], collapse = "-")
                ), 
                weight = right$y[1] - vertices_coordinates$y[i]
              )
            ) 
          
        }
        
      }
      
    }
    
  }
  
  if (temp_tile %in% c(".", "v")) {
    
    if (nrow(down) != 0) {
      
      clear_path <- 
        all(
          day23[vertices_coordinates$x[i]:down$x[1], vertices_coordinates$y[i]] != "#"
        )
      
      if (clear_path) {
        
        if (day23[down$x[1], down$y[1]] == "^") {
          
          nw_info <- 
            rbind.data.frame(
              nw_info,
              data.frame(
                from = paste(down[1, ], collapse = "-"), 
                to = paste(vertices_coordinates[i, ], collapse = "-"), 
                weight = down$x[1] - vertices_coordinates$x[i]
              )
            )
          
        } else if (temp_tile == "v") {
          
          nw_info <- 
            rbind.data.frame(
              nw_info,
              data.frame(
                from = paste(vertices_coordinates[i, ], collapse = "-"), 
                to = paste(down[1, ], collapse = "-"), 
                weight = down$x[1] - vertices_coordinates$x[i]
              )
            ) 
          
        } else {
          
          nw_info <- 
            rbind.data.frame(
              nw_info,
              data.frame(
                from = c(
                  paste(vertices_coordinates[i, ], collapse = "-"),
                  paste(down[1, ], collapse = "-")
                ), 
                to = c(
                  paste(down[1, ], collapse = "-"),
                  paste(vertices_coordinates[i, ], collapse = "-")
                ), 
                weight = down$x[1] - vertices_coordinates$x[i]
              )
            ) 
          
        }
        
      }
      
    }
    
  }
  
}

nw <- graph_from_edgelist(as.matrix(nw_info[ , 1:2]), directed = TRUE)

E(nw)$weight <- nw_info$weight

all_paths <- 
  all_simple_paths(
    nw,
    paste0(vertices_coordinates[1, ], collapse = "-"), 
    paste0(vertices_coordinates[nrow(vertices_coordinates), ], collapse = "-")
  )

# It seems like I can't simply get the weight of a path?!

path_weights <- vector("numeric", length(all_paths))

for (i in seq_along(all_paths)) {
  
  temp_weight <- 0
  
  for (j in 1:(length(all_paths[[i]]) - 1)) {
    
    x <- names(all_paths[[i]][j])
    y <- names(all_paths[[i]][j + 1])
    temp_weight <-  temp_weight +
      nw_info$weight[nw_info$from == x & nw_info$to == y]
    
  }
  
  path_weights[i] <- temp_weight
  
}

max(path_weights)
# 2106

Sys.time() - start
