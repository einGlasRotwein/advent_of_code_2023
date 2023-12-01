
day01 <- readLines("./puzzle_input/input_day01.txt")

## PART 1 ----------------------------------------------------------------------

start <- Sys.time()

# Get rid of anything that's not a digit
nums <- gsub("[^0-9]", "", day01)

first <- substr(nums, 1, 1) 
last <- substr(nums, nchar(nums), nchar(nums))
calib_num <- as.numeric(paste0(first, last))

sum(calib_num)

day01 <- readLines("./puzzle_input/input_day01.txt")
# 53386

Sys.time() - start

## PART 2 ----------------------------------------------------------------------

# As always, the AoC punishes lazy solutions in part 2 ...

start <- Sys.time()

num_lookup <- 1:9
names(num_lookup) <- 
  c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

num_matches_first <- 
  regexec(paste0("[0-9]|", paste0(names(num_lookup), collapse = "|")), day01)

first <- 
  substr(
    day01,
    sapply(num_matches_first, function(x) x),
    sapply(num_matches_first, function(x) x + attr(x, "match.length") - 1)
  )

# It ain't ugly regex if it works ...
num_matches_last <- 
  gregexec(
    paste0("^.*([0-9]|", paste0(names(num_lookup), collapse = "|"), ").*$"),
    day01
  )

last <- 
  substr(
    day01,
    sapply(num_matches_last, function(x) x[nrow(x), 1]),
    sapply(
      num_matches_last, 
      function(x) x[nrow(x), 1] + attr(x, "match.length")[nrow(x), 1] - 1
    )
  )

# Replace words with numbers
for (i_num in seq_along(num_lookup)) {
  first[first == names(num_lookup)[i_num]] <- num_lookup[i_num]
  last[last == names(num_lookup)[i_num]] <- num_lookup[i_num]
}

calib_num <- as.numeric(paste0(first, last))

sum(calib_num)
# 53312

Sys.time() - start
