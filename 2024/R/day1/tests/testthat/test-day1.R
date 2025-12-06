# Load implementation
source("../../day1.R")

test_that("solve_part1 works with real input", {
  input <- paste(readLines("../../day1.txt"), collapse="\n")
  result <- solve_part1(input)
  expect_equal(result, 1603498)
})

test_that("solve_part2 works with real input", {
  input <- paste(readLines("../../day1.txt"), collapse="\n")
  result <- solve_part2(input)
  expect_equal(result, 25574739)
})
