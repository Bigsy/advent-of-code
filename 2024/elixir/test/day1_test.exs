defmodule Day1Test do
  use ExUnit.Case
  doctest Day1

  test "solve/0 returns correct result for part 1" do
    assert Day1.solve() == 1603498
  end

  test "solve2/0 returns correct result for part 2" do
    assert Day1.solve2() == 25574739
  end
end
