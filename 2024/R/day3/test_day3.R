library(testthat)
source("day3.R")

test_that("Part 1 solution is correct", {
  input <- readLines("day3.txt", warn = FALSE)
  result <- solve_part1(input)
  expect_equal(result, 167650499)
})

test_that("Part 2 solution is correct", {
  input <- readLines("day3.txt", warn = FALSE)
  result <- solve_part2(input)
  expect_equal(result, 95846796)
})
