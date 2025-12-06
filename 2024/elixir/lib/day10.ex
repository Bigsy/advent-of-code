defmodule Day10 do
  @moduledoc """
  Solution for Day 10 of Advent of Code 2024
  Translated from Clojure to Elixir with explanatory comments
  """

  @doc """
  # Clojure to Elixir Translation Notes:
  
  ## Data Structure Changes:
  - Clojure's persistent queue -> Elixir's list (used as queue)
  - Clojure's vectors -> Elixir's lists
  - Clojure's sets -> Elixir's MapSet
  
  ## Function Translation:
  - Clojure's (let) -> Multiple function heads or pattern matching
  - Clojure's (for) comprehension -> Elixir's for comprehension
  - Clojure's (filter) -> Elixir's Enum.filter
  - Clojure's (reduce) -> Elixir's Enum.reduce
  """

  @neighbors [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]

  def part1(input) do
    # Parse input into grid (similar to Clojure's split-lines and grid creation)
    lines = String.split(input, "\n", trim: true)
    height = length(lines)
    width = String.length(List.first(lines))
    
    # Create grid with height and coordinates (like Clojure's for comprehension)
    grid = for y <- 0..(height-1),
              x <- 0..(width-1),
              char = String.at(Enum.at(lines, y), x),
              char != ".",
              do: {String.to_integer(char), {x, y}}
    
    # Find trailheads (height 0 positions)
    trailheads = Enum.filter(grid, fn {height, _pos} -> height == 0 end)
    
    # Sum up scores for each trailhead
    trailheads
    |> Enum.map(&find_reachable_nines(&1, lines, width, height))
    |> Enum.sum()
  end

  def part2(input) do
    # Similar setup to part1
    lines = String.split(input, "\n", trim: true)
    height = length(lines)
    width = String.length(List.first(lines))
    
    grid = for y <- 0..(height-1),
              x <- 0..(width-1),
              char = String.at(Enum.at(lines, y), x),
              char != ".",
              do: {String.to_integer(char), {x, y}}
    
    trailheads = Enum.filter(grid, fn {height, _pos} -> height == 0 end)
    
    # Sum up distinct paths for each trailhead
    trailheads
    |> Enum.map(&find_distinct_paths(&1, lines, width, height))
    |> Enum.sum()
  end

  # Helper functions that mirror Clojure's local functions
  
  defp find_reachable_nines({_height, start_pos}, lines, width, height) do
    # Initialize queue with starting position and height 0
    initial = {0, start_pos}
    initial_seen = MapSet.new([start_pos])
    
    do_bfs([initial], initial_seen, MapSet.new(), lines, width, height)
  end
  
  defp do_bfs([], _seen, nines, _lines, _width, _height) do
    # Base case: queue empty, return count of found nines
    MapSet.size(nines)
  end
  
  defp do_bfs([{curr_height, pos} | rest], seen, nines, lines, width, height) do
    {x, y} = pos
    next_height = curr_height + 1
    
    # Find valid next positions (like Clojure's for comprehension with :when)
    next_positions = for {dx, dy} <- @neighbors,
                        nx = x + dx,
                        ny = y + dy,
                        next_pos = {nx, ny},
                        valid_pos?(next_pos, width, height),
                        not MapSet.member?(seen, next_pos),
                        char = String.at(Enum.at(lines, ny), nx),
                        char != ".",
                        String.to_integer(char) == next_height,
                        do: {next_height, next_pos}
    
    new_seen = Enum.reduce(next_positions, seen, fn {_, pos}, acc -> MapSet.put(acc, pos) end)
    new_nines = if curr_height == 9, do: MapSet.put(nines, pos), else: nines
    
    do_bfs(rest ++ next_positions, new_seen, new_nines, lines, width, height)
  end
  
  defp find_distinct_paths({_height, start_pos}, lines, width, height) do
    # Initialize stack with starting state
    initial = {[{0, start_pos}], MapSet.new([start_pos])}
    do_dfs([initial], MapSet.new(), lines, width, height)
  end
  
  defp do_dfs([], paths, _lines, _width, _height) do
    # Base case: stack empty, return count of unique paths
    MapSet.size(paths)
  end
  
  defp do_dfs([{path, seen} | rest], paths, lines, width, height) do
    {curr_height, pos} = List.first(path)
    {x, y} = pos
    next_height = curr_height + 1
    
    # Find next valid positions for the path
    next_positions = for {dx, dy} <- @neighbors,
                        nx = x + dx,
                        ny = y + dy,
                        next_pos = {nx, ny},
                        valid_pos?(next_pos, width, height),
                        not MapSet.member?(seen, next_pos),
                        char = String.at(Enum.at(lines, ny), nx),
                        char != ".",
                        String.to_integer(char) == next_height,
                        do: {next_height, next_pos}
    
    # Create new states for each next position
    new_states = Enum.map(next_positions, fn next ->
      {[next | path], MapSet.put(seen, elem(next, 1))}
    end)
    
    new_paths = if curr_height == 9, do: MapSet.put(paths, seen), else: paths
    
    do_dfs(new_states ++ rest, new_paths, lines, width, height)
  end
  
  defp valid_pos?({x, y}, width, height) do
    x >= 0 and x < width and y >= 0 and y < height
  end
  
  defp get_height({x, y}, lines) do
    char = String.at(Enum.at(lines, y), x)
    if char == ".", do: nil, else: String.to_integer(char)
  end
end
