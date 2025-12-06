solve_part1 <- function(input) {
  input <- paste(input, collapse = "\n")
  
  pattern <- "mul\\((\\d+),(\\d+)\\)"
  matches <- gregexpr(pattern, input, perl = TRUE)
  str_matches <- regmatches(input, matches)[[1]]
  
  numbers <- gsub("mul\\((\\d+),(\\d+)\\)", "\\1 \\2", str_matches)
  sum <- 0
  for (pair in numbers) {
    nums <- as.numeric(strsplit(pair, " ")[[1]])
    sum <- sum + (nums[1] * nums[2])
  }
  return(sum)
}

solve_part2 <- function(input) {
  input <- paste(input, collapse = "\n")
  
  # Extract all commands
  pattern <- "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)"
  matches <- gregexpr(pattern, input, perl = TRUE)
  commands <- regmatches(input, matches)[[1]]
  
  enabled <- TRUE
  sum <- 0
  
  for (cmd in commands) {
    if (cmd == "do()") {
      enabled <- TRUE
    } else if (cmd == "don't()") {
      enabled <- FALSE
    } else {
      nums <- as.numeric(strsplit(gsub("mul\\((\\d+),(\\d+)\\)", "\\1 \\2", cmd), " ")[[1]])
      if (enabled) {
        sum <- sum + (nums[1] * nums[2])
      }
    }
  }
  return(sum)
}
