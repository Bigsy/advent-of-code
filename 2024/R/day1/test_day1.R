# Load the testthat package for testing
library(testthat)

# Source our implementation
source("day1.R")

# Read the actual input file
input <- paste(readLines("day1.txt"), collapse="\n")

# Test case 1: Real input for part 1
test_that("solve_part1 works with real input", {
  result <- solve_part1(input)
  # Replace EXPECTED_VALUE with the known correct answer for part 1
  expect_equal(result, 1603498)
})

# Test case 2: Real input for part 2
test_that("solve_part2 works with real input", {
  result <- solve_part2(input)
  # Replace EXPECTED_VALUE with the known correct answer for part 2
  expect_equal(result, 25574739)
})
