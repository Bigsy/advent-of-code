defmodule Day3Test do
  use ExUnit.Case
  doctest Day3

  test "part1" do
    input = File.read!("priv/day3.txt")
    assert Day3.solve_part1(input) == 167650499
  end

  test "part2" do
    input = File.read!("priv/day3.txt")
    assert Day3.solve_part2(input) == 95846796
  end
end
