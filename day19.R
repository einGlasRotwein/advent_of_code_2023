
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

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

# Create all paths through workflows and determine which ranges of xmas values 
# are allowed.

ranges <- 
  list(
    list(
      min = c(x = 1, m = 1, a = 1, s = 1),
      max = c(x = 4000, m = 4000, a = 4000, s = 4000)
    )
  )

# Track all flows in case there's a loop ... (we shouldn't be allowed to go 
# back)
paths <- list("in")

while (!all(sapply(paths, function(x) x[length(x)]) %in% c("A", "R"))) {
  
  new_ranges <- list()
  new_paths <- list()
  
  for (i in seq_along(ranges)) {
    
    temp_flow_name <- paths[[i]][length(paths[[i]])]
    template <- ranges[[i]]
    
    if (temp_flow_name %in% c("A", "R")) {
      
      to_add <- template
      new_paths <- c(new_paths, list(paths[[i]]))
      new_ranges <- c(new_ranges, list(to_add))
      next
      
    }
    
    temp_flow <- workflows[[temp_flow_name]]
    
    for (j_flow in seq_along(temp_flow)) {
      
      to_add <- template
      
      # If there is one, apply rule. Otherwise, move part.
      if (grepl(":", temp_flow[j_flow])) {
        
        rule <- gsub("(.+?)(\\:.*)", "\\1", temp_flow[j_flow])
        letter <- substr(rule, 1, 1)
        sign <- substr(rule, 2, 2)
        num <- as.numeric(substr(rule, 3, nchar(rule)))
        
        if (sign == ">") {
          to_add$min[letter] <- num + 1
        } else if (sign == "<") {
          to_add$max[letter] <- num - 1
        }
        
        # Is there a loop?
        if (any(sub(".*\\:", "", temp_flow[j_flow]) == paths[[i]])) {
          stop("Loop detected!")
        }
        
        new_paths <- 
          c(
            new_paths, 
            list(c(paths[[i]], sub(".*\\:", "", temp_flow[j_flow])))
          )
        
        new_ranges <- c(new_ranges, list(to_add))
        
        # Modify template - if a part made it to the second rule, it did not 
        # pass the first.
        if (sign == ">") {
          template$max[letter] <- num
        } else if (sign == "<") {
          template$min[letter] <- num
        }
        
      } else {
        
        # Is there a loop?
        if (any(sub(".*\\:", "", temp_flow[j_flow]) == paths[[i]])) {
          stop("Loop detected!")
        }
        
        new_paths <- 
          c(
            new_paths, 
            list(c(paths[[i]], sub(".*\\:", "", temp_flow[j_flow])))
          )
        
        new_ranges <- c(new_ranges, list(template))
        
      }
      
    }
    
  }
  
  ranges <- new_ranges
  paths <- new_paths
  
}

accepted_ranges <- ranges[sapply(paths, function(x) x[length(x)]) == "A"]

options(scipen = 999)

sum(sapply(accepted_ranges, function(x) prod(x$max - x$min + 1)))
# 131619440296497

Sys.time() - start
