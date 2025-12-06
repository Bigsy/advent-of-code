# Elixir modules are like Clojure namespaces (ns)
# This is equivalent to (ns Day3) in Clojure
defmodule Day3 do
  # Function definition similar to (defn solve-part1 [input] ...) in Clojure
  def solve_part1(input) do
    # Regex in Elixir uses ~r/.../ syntax instead of Clojure's #"..."
    # This regex looks for pattern mul(123,456) and captures the numbers
    ~r/mul\((\d{1,3}),(\d{1,3})\)/
    # |> is the pipe operator, similar to Clojure's -> threading macro
    # Regex.scan is like (re-seq pattern input) in Clojure
    |> Regex.scan(input)
    # Enum.map is like (map ...) in Clojure
    # [_, x, y] is destructuring like [[_ x y]] in Clojure
    |> Enum.map(fn [_, x, y] ->
      # String.to_integer is like Clojure's (Integer/parseInt)
      String.to_integer(x) * String.to_integer(y)
    end)
    # Enum.sum is like (reduce + 0 coll) in Clojure
    |> Enum.sum()
  end

  def solve_part2(input) do
    # Multiple regex patterns with | (or), matching either mul(x,y) or do()/don't()
    ~r/mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)/
    |> Regex.scan(input)
    # Pattern matching in function, similar to core.match in Clojure
    # Keywords in Elixir use : prefix just like Clojure
    |> Enum.map(fn
      ["do()"] -> :enable      # Like :enable in Clojure
      ["don't()"] -> :disable  # Like :disable in Clojure
      # Tuple {:mul, x, y} is like a vector [:mul x y] in Clojure
      [_, x, y] -> {:mul, String.to_integer(x), String.to_integer(y)}
    end)
    # %{} is a map literal like {} in Clojure
    # Enum.reduce is like (reduce ...) in Clojure
    |> Enum.reduce(%{enabled?: true, sum: 0}, fn
      # Pattern matching in function arguments, like destructuring in Clojure
      # When enabled, multiply numbers and add to sum
      {:mul, x, y}, %{enabled?: true, sum: sum} ->
        %{enabled?: true, sum: sum + x * y}
      # When disabled, ignore multiplication (pass through accumulator)
      {:mul, _, _}, acc ->
        acc
      # Enable future operations (like (assoc acc :enabled? true))
      :enable, acc ->
        %{acc | enabled?: true}
      # Disable future operations
      :disable, acc ->
        %{acc | enabled?: false}
    end)
    # Map.get is like (get map :key) or (:key map) in Clojure
    |> Map.get(:sum)
  end
end
