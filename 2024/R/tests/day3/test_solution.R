library(testthat)
source("../../src/day3/solution.R")

# Read input file
input <- readLines("day3.txt", warn=FALSE)
input <- paste(input, collapse="\n")

test_that("Day 3 Part 1 returns correct result", {
  result <- solve_part1(input)
  expect_equal(result, 167650499)
})

test_that("Day 3 Part 2 returns correct result", {
  result <- solve_part2(input)
  expect_equal(result, 95846796)
})
