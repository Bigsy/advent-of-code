defmodule Day1 do
  # This function is equivalent to the Clojure:
  # (defn parse-input [input]
  #   (->> (str/split-lines input)
  #        (map #(mapv parse-long (str/split % #"\s+")))))
  def parse_input(input) do
    input
    # String.split("\n") is like str/split-lines
    |> String.split("\n", trim: true)
    # Enum.map is like Clojure's map
    # The fn -> end block is like Clojure's #(...)
    |> Enum.map(fn line ->
      line
      # ~r/\s+/ is a regex like #"\s+" in Clojure
      |> String.split(~r/\s+/)
      # &String.to_integer/1 is like parse-long
      # The & syntax is like Clojure's #()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  # Equivalent to Clojure:
  # (defn solve-part1 [input]
  #   (let [data (parse-input input)
  #         pairs (map vector 
  #                   (sort (map first data))
  #                   (sort (map second data)))]
  #     (reduce + (map #(Math/abs (apply - %)) pairs))))
  def solve_part1(input) do
    data = parse_input(input)
    # List.first/1 is like Clojure's first
    # Enum.sort is like Clojure's sort
    firsts = data |> Enum.map(&List.first/1) |> Enum.sort()
    # List.last/1 is like Clojure's second (in this case, since we know there are 2 elements)
    seconds = data |> Enum.map(&List.last/1) |> Enum.sort()
    
    # Enum.zip is like Clojure's (map vector xs ys)
    Enum.zip(firsts, seconds)
    # The {a, b} pattern match is like Clojure's destructuring
    # abs(a - b) is like (Math/abs (apply - [a b]))
    |> Enum.map(fn {a, b} -> abs(a - b) end)
    # Enum.sum is like (reduce +)
    |> Enum.sum()
  end

  # Equivalent to Clojure:
  # (defn solve-part2 [input]
  #   (let [data (parse-input input)]
  #     (->> (map first data)
  #          (map #(* % (count (filter #{%} (map second data)))))
  #          (reduce +))))
  def solve_part2(input) do
    data = parse_input(input)
    firsts = Enum.map(data, &List.first/1)
    seconds = Enum.map(data, &List.last/1)
    
    firsts
    |> Enum.map(fn x -> 
      # Enum.count with a predicate is like (count (filter pred coll))
      # &(&1 == x) is like #(= % x) in Clojure
      # The whole expression is like (* x (count (filter #{x} seconds)))
      x * Enum.count(seconds, &(&1 == x))
    end)
    |> Enum.sum()
  end

  # Equivalent to Clojure:
  # (defn solve [] (solve-part1 (slurp (io/resource "day1.txt"))))
  def solve do
    # File.read! is like slurp
    # We use priv/ directory instead of resources in Elixir
    File.read!("priv/day1.txt")
    |> solve_part1()
  end

  # Equivalent to:
  # (defn solve2 [] (solve-part2 (slurp (io/resource "day1.txt"))))
  def solve2 do
    File.read!("priv/day1.txt")
    |> solve_part2()
  end
end
