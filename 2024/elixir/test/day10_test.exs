defmodule Day10Test do
  use ExUnit.Case
  doctest Day10

  test "part1 matches Clojure solution" do
    input = File.read!("priv/day10.txt")
    assert Day10.part1(input) == 682
  end

  test "part2 matches Clojure solution" do
    input = File.read!("priv/day10.txt")
    assert Day10.part2(input) == 1511
  end

  test "part1 example from puzzle" do
    input = """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """
    assert Day10.part1(input) == 36
  end

  test "part2 example from puzzle" do
    input = """
    012345
    123456
    234567
    345678
    4.6789
    56789.
    """
    assert Day10.part2(input) == 227
  end
end
