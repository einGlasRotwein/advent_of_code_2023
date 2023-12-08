
# Example input, if needed
# day05 <- readLines("puzzle_input/input_day05_exp.txt")

day05 <- readLines("puzzle_input/input_day05.txt")

options(scipen = 999)

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

seeds <- as.numeric(unlist(strsplit(sub("seeds: ", "", day05[1]), " ")))
day05 <- day05[-1]

day05 <- day05[day05 != ""]

day05 <- data.frame(info = day05)

day05$map <- ifelse(!grepl("[0-9]", day05$info), sub(" map:", "", day05$info), NA)
day05$group <- cumsum(!is.na(day05$map))

day05$map <- 
  rep(day05$map[!is.na(day05$map)], times = rle(day05$group)$lengths)

day05 <- day05[grepl("[0-9]", day05$info), ]

seed_ends <- vector("numeric", length(seeds))

for (i_seed in seq_along(seeds)) {
  temp_source <- seeds[i_seed]
  
  for (j_group in unique(day05$group)) {
    
    for (k_row in 1:sum(day05$group == j_group)) {
      temp_info <- day05$info[day05$group == j_group][k_row]
      temp_info <- as.numeric(unlist(strsplit(temp_info, " ")))
      source_in_row <- 
        temp_source >= temp_info[2] & temp_source <= temp_info[2] + (temp_info[3] - 1)
      
      # If source is part of the row, find the corresponding destination, then 
      # break
      if (source_in_row) {
        temp_diff <- temp_source - temp_info[2]
        temp_source <- temp_info[1] + temp_diff
        break
      }
    }
    
  }
  
  seed_ends[i_seed] <- temp_source
}

min(seed_ends)
# 26273516

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

start <- Sys.time()

# Carry seeds through the levels of the map, converting them to new ranges
infos <- lapply(strsplit(day05$info, " "), as.numeric)

day05$destination_min <- sapply(infos, `[`, 1)
day05$destination_max <- sapply(infos, `[`, 1) + (sapply(infos, `[`, 3) - 1)
day05$source_min <- sapply(infos, `[`, 2)
day05$source_max <- sapply(infos, `[`, 2) + (sapply(infos, `[`, 3) - 1)

# Use seed ranges (start = r1, stop = r2) instead of a single seed
seed_r1 <- seeds[c(TRUE, FALSE)]
seed_r2 <- seed_r1 + (seeds[c(FALSE, TRUE)] - 1)

day05$info <- NULL

seed_locations <- vector("list", length = length(seed_r1))

for (i_seed in 1:length(seed_r1)) {
  
  temp_range <-
    data.frame(
      source_min = seed_r1[i_seed],
      source_max = seed_r2[i_seed]
    )
  
  for (j_group in unique(day05$group)) {
    
    row_counter <- 1
    
    while (row_counter <= nrow(temp_range)) {
      
      range_min <- temp_range$source_min[row_counter]
      range_max <- temp_range$source_max[row_counter]
      
      for (k_row in 1:sum(day05$group == j_group)) {
        
        source_min <- day05$source_min[day05$group == j_group][k_row]
        source_max <- day05$source_max[day05$group == j_group][k_row]
        
        min_in_range <- range_min >= source_min & range_min <= source_max
        max_in_range <- range_max >= source_min & range_max <= source_max
        
        new_dest_min <- day05$destination_min[day05$group == j_group][k_row]
        new_dest_max <- day05$destination_max[day05$group == j_group][k_row]
        
        if (min_in_range & max_in_range) {
          
          diff <- source_min - range_min
          temp_range$source_min[row_counter] <- new_dest_min - diff
          
          diff <- source_max - range_max
          temp_range$source_max[row_counter] <- new_dest_max - diff
          
          row_counter <- row_counter + 1
          
          break
          
        } else if (min_in_range) {
          
          new_range <- 
            data.frame(
              source_min = source_max + 1,
              source_max = range_max
            )
          
          diff <- source_min - range_min
          temp_range$source_min[row_counter] <- new_dest_min - diff
          
          temp_range$source_max[row_counter] <- new_dest_max
          
          temp_range <- 
            rbind.data.frame(
              temp_range,
              new_range
            )
          
          row_counter <- row_counter + 1
          
          break
          
        } else if (max_in_range) {
          
          new_range <- 
            data.frame(
              source_min = range_min,
              source_max = source_min - 1
            )
          
          diff <- source_max - range_max
          temp_range$source_max[row_counter] <- new_dest_max - diff
          
          # new rang min
          temp_range$source_min[row_counter] <- new_dest_min
          
          temp_range <- 
            rbind.data.frame(
              temp_range,
              new_range
            )
          
          row_counter <- row_counter + 1
          
          break
          
        }
        
        if (k_row == sum(day05$group == j_group)) row_counter <- row_counter + 1
        
      }
      
    }
    
  }
  
  seed_locations[[i_seed]] <- temp_range
  
}

min(sapply(seed_locations, function(x) min(x$source_min)))

Sys.time() - start
