
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
