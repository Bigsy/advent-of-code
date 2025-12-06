defmodule Day8Test do
  use ExUnit.Case

  test "solve part 1" do
    assert Day8.solve_part1(File.read!("priv/day8.txt")) == 308
  end

  test "solve part 2" do
    assert Day8.solve_part2(File.read!("priv/day8.txt")) == 1147
  end
end
