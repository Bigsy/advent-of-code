defmodule Day5Test do
  use ExUnit.Case
  doctest Day5

  setup do
    input = File.read!("priv/day5.txt")
    {:ok, input: input}
  end

  test "part 1 solution", %{input: input} do
    result = Day5.solve_part1(input)
    assert result == 5639
  end

  test "part 2 solution", %{input: input} do
    result = Day5.solve_part2(input)
    assert result == 5273
  end
end
