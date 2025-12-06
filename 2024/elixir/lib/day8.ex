defmodule Day8 do
  # Similar to Clojure's (defn parse-grid [input] ...)
  # The main differences:
  # - String.split instead of clojure.string/split-lines
  # - Enum.flat_map instead of for comprehension
  # - Returns a tuple {grid, width, height} instead of a map
  def parse_grid(input) do
    lines = String.split(input, "\n", trim: true)
    height = length(lines)
    width = String.length(List.first(lines))

    grid =
      lines
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, y} ->
        line
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.filter(fn {char, _} -> char != "." end)
        |> Enum.map(fn {char, x} -> {{x, y}, char} end)
      end)
      |> Map.new()

    {grid, width, height}
  end

  # Similar to Clojure's (group-by) but using Enum.group_by
  # In Clojure this would be:
  # (reduce-kv (fn [acc pos freq] (update acc freq (fnil conj []) pos)) {} grid)
  def get_antennas_by_freq(grid) do
    grid
    |> Enum.group_by(fn {_pos, freq} -> freq end, fn {pos, _freq} -> pos end)
  end

  # Direct port of Clojure's aligned? function
  # Main difference is pattern matching in function head instead of destructuring
  def aligned?({x1, y1}, {x2, y2}, {x3, y3}) do
    dx1 = x2 - x1
    dy1 = y2 - y1
    dx2 = x3 - x2
    dy2 = y3 - y2
    dx1 * dy2 == dy1 * dx2
  end

  # Similar to Clojure's distance-squared function
  # Uses pattern matching instead of destructuring
  def distance_squared({x1, y1}, {x2, y2}) do
    dx = x2 - x1
    dy = y2 - y1
    dx * dx + dy * dy
  end

  # In Clojure this would use a for comprehension
  # Elixir's for is similar but with different syntax:
  # - <- is like :let in Clojure
  # - Multiple generators stack like in Clojure
  # - Returns list instead of lazy seq
  def find_antinodes({_grid, width, height}, p1, p2) do
    d12 = distance_squared(p1, p2)

    for x <- 0..(width - 1),
        y <- 0..(height - 1),
        p3 = {x, y},
        p3 != p1 and p3 != p2,
        aligned?(p1, p2, p3) do
      d23 = distance_squared(p2, p3)
      d13 = distance_squared(p1, p3)

      if (d23 == 4 * d12 and d13 == d12) or
           (d13 == 4 * d12 and d23 == d12) do
        p3
      end
    end
    |> Enum.filter(& &1)  # Similar to (filter identity) in Clojure
  end

  # Part 2 version removes distance checks
  # Similar structure to Clojure's for comprehension
  def find_antinodes_part2({_grid, width, height}, p1, p2) do
    for x <- 0..(width - 1),
        y <- 0..(height - 1),
        p3 = {x, y},
        aligned?(p1, p2, p3),
        do: p3
  end

  # Main solving function
  # Key differences from Clojure:
  # - Uses |> operator instead of -> threading macro
  # - MapSet instead of #{} set literal
  # - elem(parsed, 0) instead of (:grid parsed)
  def solve_part1(input) do
    parsed = parse_grid(input)
    antennas = get_antennas_by_freq(elem(parsed, 0))

    antennas
    |> Enum.flat_map(fn {_freq, positions} ->
      if length(positions) < 2 do
        []
      else
        for p1 <- positions,
            p2 <- positions,
            p1 != p2,
            antinode <- find_antinodes(parsed, p1, p2),
            do: antinode
      end
    end)
    |> MapSet.new()  # Like (into #{} ...) in Clojure
    |> MapSet.size() # Like (count set) in Clojure
  end

  # Part 2 is similar but uses find_antinodes_part2
  def solve_part2(input) do
    parsed = parse_grid(input)
    antennas = get_antennas_by_freq(elem(parsed, 0))

    antennas
    |> Enum.flat_map(fn {_freq, positions} ->
      if length(positions) < 2 do
        []
      else
        for p1 <- positions,
            p2 <- positions,
            p1 != p2,
            antinode <- find_antinodes_part2(parsed, p1, p2),
            do: antinode
      end
    end)
    |> MapSet.new()
    |> MapSet.size()
  end
end
