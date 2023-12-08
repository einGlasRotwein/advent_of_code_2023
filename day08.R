
# Requires the library "numbers"

day08 <- readLines("puzzle_input/input_day08.txt")
directions <- unlist(strsplit(day08[1], ""))
day08 <- day08[-(1:2)]

day08 <- as.data.frame(do.call(rbind, strsplit(day08, " = ")))
names(day08) <- c("node", "options")
day08$options <- gsub("[()]", "", day08$options)

options <- as.data.frame(do.call(rbind, strsplit(day08$options, ", ")))
names(options) <- c("L", "R")

day08 <- cbind.data.frame(day08, options)
day08$options <- NULL

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

temp_node <- "AAA"
n_steps <- 0

while (temp_node != "ZZZ") {
  temp_direction <- 
    directions[
      ifelse(
        (n_steps + 1) %% length(directions) == 0, 
        length(directions), 
        (n_steps + 1) %% length(directions)
      )
    ]
  temp_node <- day08[day08$node == temp_node, temp_direction]
  n_steps <- n_steps + 1
}

n_steps
# 18673

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

# As expected, brute-forcing this takes too long. Instead, figure out at which 
# intervals each start node hits a node ending with Z (and when the pattern 
# repeats for each node), then figure out at which point all nodes converge at 
# a node ending with Z.

start <- Sys.time()

temp_nodes <- day08$node[grepl("..A", day08$node)]
first_z <- vector("character", length(temp_nodes))
z_visits <- vector("list", length(temp_nodes))
z_visits <- lapply(z_visits, function(x) vector("numeric", 0))

for (i_node in seq_along(temp_nodes)) {
  n_steps <- 0
  temp_node <- temp_nodes[i_node]
  go_flag <- TRUE
  
  while (go_flag) {
    temp_direction <- 
      directions[
        ifelse(
          (n_steps + 1) %% length(directions) == 0, 
          length(directions), 
          (n_steps + 1) %% length(directions)
        )
      ]
    
    temp_node <- day08[day08$node == temp_node, temp_direction]
    n_steps <- n_steps + 1
    
    if (grepl("..Z", temp_node)) {
      
      if (first_z[i_node] == "") {
        
        first_z[i_node] <- temp_node
        z_visits[[i_node]] <- c(z_visits[[i_node]], n_steps)
        
      } else {
        
        if (first_z[i_node] != temp_node) {
          z_visits[[i_node]] <- c(z_visits[[i_node]], n_steps)
        } else {
          go_flag <- FALSE
        }
        
      }
      
    } 
  }
}

# Luckily, each node only visits a single Z location repeatedly. We only need 
# to figure out when they are all in that location at the same time.
z_visits <- unlist(z_visits)

# ... which can be determined using the least common multiple
options(scipen = 999)
numbers::mLCM(z_visits)

Sys.time() - start

