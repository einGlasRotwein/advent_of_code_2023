
run_beam <- function(beam_paths, beam_directions, beam_field) {
  
  past_commands <- c()
  visited <- beam_field == "x"
  
  while (length(beam_paths) != 0) {
    
    new_paths <- list()
    new_beam_directions <- vector("character", 0)
    
    for (i in seq_along(beam_paths)) {
      
      # Search for next mirror (or end of matrix)
      ## RIGHT -----------------------------------------------------------------
      if (beam_directions[i] == "R") {
        
        not_empty <- which(!beam_field[beam_paths[[i]][1], ] %in% c(".", "-"))
        next_pos <- not_empty[not_empty > beam_paths[[i]][2]][1]
        
        if (is.na(next_pos)) {
          # Visit until end of matrix
          visited[
            matrix(
              c(
                rep(beam_paths[[i]][1], length(beam_paths[[i]][2]:ncol(visited))),
                beam_paths[[i]][2]:ncol(visited)
              ),
              ncol = 2
            )
          ] <- TRUE
          next
        }
        
        next_mirror <- beam_field[beam_paths[[i]][1], next_pos]
        
        visited[
          matrix(
            c(
              rep(beam_paths[[i]][1], length(beam_paths[[i]][2]:next_pos)),
              beam_paths[[i]][2]:next_pos
            ),
            ncol = 2
          )
        ] <- TRUE
        
        if (next_mirror == "|") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(beam_paths[[i]][1], next_pos), c(beam_paths[[i]][1], next_pos))
            )
          
          new_beam_directions <- c(new_beam_directions, c("U", "D"))
          
        } else if (next_mirror == "/") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(beam_paths[[i]][1], next_pos))
            )
          
          new_beam_directions <- c(new_beam_directions, "U")
          
        } else if (next_mirror == "\\") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(beam_paths[[i]][1], next_pos))
            )
          
          new_beam_directions <- c(new_beam_directions, "D")
          
        }
        
        ## LEFT ----------------------------------------------------------------
      } else if (beam_directions[i] == "L") {
        
        not_empty <- which(!beam_field[beam_paths[[i]][1], ] %in% c(".", "-"))
        next_pos <- 
          not_empty[not_empty < beam_paths[[i]][2]][
            sum(not_empty < beam_paths[[i]][2])
          ]
        
        if (length(next_pos) == 0) {
          
          visited[
            matrix(
              c(
                rep(beam_paths[[i]][1], length(1:beam_paths[[i]][2])),
                1:beam_paths[[i]][2]
              ),
              ncol = 2
            )
          ] <- TRUE
          next
          
        }
        
        next_mirror <- beam_field[beam_paths[[i]][1], next_pos]
        
        visited[
          matrix(
            c(
              rep(beam_paths[[i]][1], length(beam_paths[[i]][2]:next_pos)),
              beam_paths[[i]][2]:next_pos
            ),
            ncol = 2
          )
        ] <- TRUE
        
        if (next_mirror == "|") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(beam_paths[[i]][1], next_pos), c(beam_paths[[i]][1], next_pos))
            )
          
          new_beam_directions <- c(new_beam_directions, c("U", "D"))
          
        } else if (next_mirror == "/") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(beam_paths[[i]][1], next_pos))
            )
          
          new_beam_directions <- c(new_beam_directions, "D")
          
        } else if (next_mirror == "\\") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(beam_paths[[i]][1], next_pos))
            )
          
          new_beam_directions <- c(new_beam_directions, "U")
          
        }
        
        ## UP ------------------------------------------------------------------
      } else if (beam_directions[i] == "U") {
        
        not_empty <- which(!beam_field[ , beam_paths[[i]][2]] %in% c(".", "|"))
        next_pos <- 
          not_empty[not_empty < beam_paths[[i]][1]][
            sum(not_empty < beam_paths[[i]][1])
          ]
        
        if (length(next_pos) == 0) {
          
          visited[
            matrix(
              c(
                1:beam_paths[[i]][1],
                rep(beam_paths[[i]][2], length(1:beam_paths[[i]][1]))
              ),
              ncol = 2
            )
          ] <- TRUE
          next
          
        }
        
        next_mirror <- beam_field[next_pos, beam_paths[[i]][2]]
        
        visited[
          matrix(
            c(
              beam_paths[[i]][1]:next_pos,
              rep(beam_paths[[i]][2], length(beam_paths[[i]][1]:next_pos))
            ),
            ncol = 2
          )
        ] <- TRUE
        
        if (next_mirror == "-") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(next_pos, beam_paths[[i]][2]), c(next_pos, beam_paths[[i]][2]))
            )
          
          new_beam_directions <- c(new_beam_directions, c("R", "L"))
          
        } else if (next_mirror == "/") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(next_pos, beam_paths[[i]][2]))
            )
          
          new_beam_directions <- c(new_beam_directions, "R")
          
        } else if (next_mirror == "\\") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(next_pos, beam_paths[[i]][2]))
            )
          
          new_beam_directions <- c(new_beam_directions, "L")
          
        }
        
        ## DOWN ----------------------------------------------------------------
      } else if (beam_directions[i] == "D") {
        
        not_empty <- which(!beam_field[ , beam_paths[[i]][2]] %in% c(".", "|"))
        next_pos <- not_empty[not_empty > beam_paths[[i]][1]][1]
        
        if (is.na(next_pos)) {
          
          visited[
            matrix(
              c(
                beam_paths[[i]][1]:nrow(visited),
                rep(beam_paths[[i]][2], length(beam_paths[[i]][1]:nrow(visited)))
              ),
              ncol = 2
            )
          ] <- TRUE
          next
          
        }
        
        next_mirror <- beam_field[next_pos, beam_paths[[i]][2]]
        
        visited[
          matrix(
            c(
              beam_paths[[i]][1]:next_pos,
              rep(beam_paths[[i]][2], length(beam_paths[[i]][1]:next_pos))
            ),
            ncol = 2
          )
        ] <- TRUE
        
        if (next_mirror == "-") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(next_pos, beam_paths[[i]][2]), c(next_pos, beam_paths[[i]][2]))
            )
          
          new_beam_directions <- c(new_beam_directions, c("R", "L"))
          
        } else if (next_mirror == "/") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(next_pos, beam_paths[[i]][2]))
            )
          
          new_beam_directions <- c(new_beam_directions, "L")
          
        } else if (next_mirror == "\\") {
          
          new_paths <- 
            c(
              new_paths, 
              list(c(next_pos, beam_paths[[i]][2]))
            )
          
          new_beam_directions <- c(new_beam_directions, "R")
          
        }
        
      }
      
    }
    
    # keep unique paths (throw out loops)
    path_ids <- 
      paste0(
        sapply(new_paths, function(x) paste0(x, collapse = "")), new_beam_directions
      )
    
    # Throw out loops
    beam_paths <- new_paths[!path_ids %in% past_commands]
    beam_directions <- new_beam_directions[!path_ids %in% past_commands]
    path_ids <- path_ids[!path_ids %in% past_commands]
    
    past_commands <- c(past_commands, path_ids)
    
  }
  
  return(visited)
  
}
