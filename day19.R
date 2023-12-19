
# Example input, if needed
# day19 <- readLines("./puzzle_input/input_day19_exp.txt")

day19 <- readLines("./puzzle_input/input_day19.txt")

blank <- which(day19 == "")

workflows_initial <- day19[1:(blank - 1)]
workflows <- strsplit(gsub(".*\\{(.+)\\}.*", "\\1", workflows_initial), ",")
names(workflows) <- gsub("(.+?)(\\{.*)", "\\1", workflows_initial)

# The parts all have the same structure (x, m, a, s), so we're lazy and 
# hardcode that bit.
parts <- day19[(blank + 1):length(day19)]
parts <- gsub("[{}]", "", parts)
parts <- strsplit(parts, ",")
parts <- 
  lapply(parts, function(x) {
    x <- as.numeric(gsub("[^0-9]", "", x))
    names(x) <- c("x", "m", "a", "s")
    x
  })

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

part_status <- vector("character", length(parts))

for (i_part in seq_along(parts)) {
  
  temp_part <- parts[[i_part]]
  workflow_name <- "in"
  status <- ""
  
  while (!status %in% c("A", "R")) {
    
    temp_flow <- workflows[[workflow_name]]
    
    for (j_flow in seq_along(temp_flow)) {
      
      # If there is one, apply rule. Otherwise, move part.
      if (grepl(":", temp_flow[j_flow])) {
        
        rule <- gsub("(.+?)(\\:.*)", "\\1", temp_flow[j_flow])
        letter <- substr(rule, 1, 1)
        rule <- paste0("temp_part['", letter, "']", substr(rule, 2, nchar(rule)))
        
        if (eval(parse(text = rule))) {
          
          workflow_name <- sub(".*\\:", "", temp_flow[j_flow])
          status <- ifelse(workflow_name %in% c("A", "R"), workflow_name, "")
          break
          
        }
        
      } else {
        
        workflow_name <- temp_flow[j_flow]
        status <- ifelse(workflow_name %in% c("A", "R"), workflow_name, "")
        break
        
      }
      
    }
    
  }
  
  part_status[i_part] <- status
  
}

sum(sapply(parts[part_status == "A"], sum))
# 287054

Sys.time() - start
