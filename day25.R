
library(igraph)

# Example input, if needed
# day25 <- readLines("./puzzle_input/input_day25_exp.txt")

day25 <- readLines("./puzzle_input/input_day25.txt")

day25 <- strsplit(day25, ": ")
from <- sapply(day25, function(x) x[1])
to <- lapply(day25, function(x) unlist(strsplit(x[2], " ")))

vertices <- data.frame(from = c(), to = c())

for (i in seq_along(from)) {
  
  for (j in seq_along(to[[i]])) {
    
    vertices <- 
      rbind.data.frame(
        vertices,
        data.frame(
          from = from[i],
          to = to[[i]][j]
        )
      )
    
  }
  
}

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

nw <- graph_from_edgelist(as.matrix(vertices), directed = FALSE)

# Delete the three edges with the highest betweenness
e_btw <- edge_betweenness(nw)

cut_nw <- delete.edges(nw, E(nw)[order(e_btw, decreasing = TRUE)][1:3])

prod(components(cut_nw)$csize)
# 507626

Sys.time() - start
