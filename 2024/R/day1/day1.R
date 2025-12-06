# In Clojure, we would use (ns) for namespacing and (:require) for imports
# R doesn't have direct equivalents, but we can load necessary packages if needed
# The original Clojure code uses clojure.string for string operations and clojure.java.io for file operations

# parse_input: Equivalent to Clojure's parse-input function
# In Clojure, we used ->> threading macro for sequential transformations
# Here we'll achieve similar functionality with R's pipe-friendly approach
parse_input <- function(input) {
  # str/split-lines in Clojure splits on newlines
  # strsplit in R splits on pattern, unlist flattens the result
  lines <- unlist(strsplit(input, "\n"))
  
  # In Clojure: (map #(mapv parse-long (str/split % #"\s+")))
  # We're splitting each line on whitespace and converting to numbers
  # mapply is R's equivalent to Clojure's map function here
  result <- lapply(lines, function(line) {
    as.numeric(unlist(strsplit(trimws(line), "\\s+")))
  })
  
  return(result)
}

# solve_part1: Equivalent to Clojure's solve-part1 function
# In Clojure, we used let for local bindings and threading macros for transformation
solve_part1 <- function(input) {
  data <- parse_input(input)
  
  # Extract first and second elements
  # In Clojure: (map first data) and (map second data)
  firsts <- sapply(data, function(x) x[1])
  seconds <- sapply(data, function(x) x[2])
  
  # Sort both sequences as in the original Clojure code
  firsts_sorted <- sort(firsts)
  seconds_sorted <- sort(seconds)
  
  # Calculate absolute differences between sorted pairs
  # In Clojure: (map #(Math/abs (apply - %)) pairs)
  differences <- abs(firsts_sorted - seconds_sorted)
  
  # Sum all differences (reduce + in Clojure)
  return(sum(differences))
}

# solve_part2: Equivalent to Clojure's solve-part2 function
solve_part2 <- function(input) {
  data <- parse_input(input)
  
  # Extract first elements
  firsts <- sapply(data, function(x) x[1])
  seconds <- sapply(data, function(x) x[2])
  
  # For each first element, multiply by count of matching second elements
  # In Clojure: (map #(* % (count (filter #{%} (map second data)))))
  result <- sapply(firsts, function(x) {
    x * sum(seconds == x)
  })
  
  # Sum all results
  return(sum(result))
}

# Main solver functions
# In Clojure, we used io/resource to get file content
# Here we'll use R's readLines
solve <- function() {
  input <- paste(readLines("day1.txt"), collapse="\n")
  solve_part1(input)
}

solve2 <- function() {
  input <- paste(readLines("day1.txt"), collapse="\n")
  solve_part2(input)
}
