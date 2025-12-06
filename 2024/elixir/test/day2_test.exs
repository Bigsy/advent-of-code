defmodule Day2Test do
  use ExUnit.Case
  doctest Day2

  test "part1 returns correct result" do
    input = File.read!("priv/day2.txt")
    assert Day2.solve_part1(input) == 411
  end

  test "part2 returns correct result" do
    input = File.read!("priv/day2.txt")
    assert Day2.solve_part2(input) == 465
  end
end
