defmodule Day2 do
  @moduledoc """
  Day 2 solution converted from Clojure
  Original Clojure solution used string parsing and sequence operations
  to process numbers and check their differences
  """

  @doc """
  Parses input string into a list of lists of integers
  Equivalent to Clojure's parse-input function
  """
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.trim()
      |> String.split(~r/\s+/)
      |> Enum.map(&String.to_integer/1)
    end)
  end

  @doc """
  Checks if the differences between consecutive numbers are valid
  Equivalent to Clojure's valid-differences? function
  """
  def valid_differences?(nums) do
    diffs = Enum.zip(nums, tl(nums)) |> Enum.map(fn {a, b} -> b - a end)
    
    all_valid_range? = Enum.all?(diffs, fn diff -> abs(diff) >= 1 and abs(diff) <= 3 end)
    all_same_sign? = Enum.all?(diffs, &(&1 > 0)) or Enum.all?(diffs, &(&1 < 0))
    
    all_valid_range? and all_same_sign?
  end

  @doc """
  Solves part 1 - counts sequences with valid differences
  Equivalent to Clojure's solve-part1 function
  """
  def solve_part1(input) do
    input
    |> parse_input()
    |> Enum.count(&valid_differences?/1)
  end

  @doc """
  Solves part 2 - counts sequences that are valid or can become valid by removing one number
  Equivalent to Clojure's solve-part2 function
  """
  def solve_part2(input) do
    parsed = parse_input(input)
    
    Enum.count(parsed, fn nums ->
      valid_differences?(nums) or
        Enum.with_index(nums)
        |> Enum.any?(fn {_, i} ->
          nums_without_i = List.delete_at(nums, i)
          valid_differences?(nums_without_i)
        end)
    end)
  end
end
