"""
This module solves Day 1 of Advent of Code 2024.
Translated from Clojure to Python with explanatory comments for Clojure developers.
"""

from pathlib import Path
from typing import List, Tuple

def parse_input(input_str: str) -> List[List[int]]:
    """
    Parses the input string into a list of number pairs.
    
    In Clojure this was:
    (->> (str/split-lines input)
         (map #(mapv parse-long (str/split % #"\s+"))))
    
    The ->> is a thread-last macro that pipes the result of each expression
    as the last argument to the next expression. In Python we write this
    more imperatively.
    """
    # Split lines is like Clojure's str/split-lines
    lines = input_str.split('\n')
    
    # For each line, split on whitespace and convert to integers
    # This replaces Clojure's (map #(mapv parse-long (str/split % #"\s+")))
    # mapv in Clojure returns a vector instead of a lazy sequence
    return [list(map(int, line.split())) for line in lines]

def solve_part1(input_str: str) -> int:
    """
    Solves part 1 of the puzzle.
    
    In Clojure this was:
    (let [data (parse-input input)
          pairs (map vector 
                    (sort (map first data))
                    (sort (map second data)))]
      (reduce + (map #(Math/abs (apply - %)) pairs)))
    
    The let binding in Clojure creates local bindings. In Python we just
    use regular variable assignment.
    """
    data = parse_input(input_str)
    
    # Extract and sort first and second elements separately
    # In Clojure (map first data) gets first elements, (map second data) gets second elements
    firsts = sorted(row[0] for row in data)
    seconds = sorted(row[1] for row in data)
    
    # Create pairs using zip (similar to Clojure's map vector)
    pairs = list(zip(firsts, seconds))
    
    # Calculate absolute differences and sum them
    # Replaces (reduce + (map #(Math/abs (apply - %)) pairs))
    return sum(abs(a - b) for a, b in pairs)

def solve_part2(input_str: str) -> int:
    """
    Solves part 2 of the puzzle.
    
    In Clojure this was:
    (let [data (parse-input input)]
      (->> (map first data)
           (map #(* % (count (filter #{%} (map second data)))))
           (reduce +)))
    
    The ->> macro in Clojure threads the result through the expressions.
    In Python we write this more directly.
    """
    data = parse_input(input_str)
    
    # Get all first elements
    firsts = [row[0] for row in data]
    # Get all second elements
    seconds = [row[1] for row in data]
    
    # For each first number, multiply it by how many times it appears in seconds
    # In Clojure, #{%} creates a set with one element for filtering
    result = sum(x * seconds.count(x) for x in firsts)
    return result

def solve() -> int:
    """
    Solves part 1 using input from day1.txt.
    
    In Clojure this used:
    (slurp (io/resource "day1.txt"))
    
    Python doesn't have a direct equivalent to Clojure's io/resource,
    so we use pathlib to handle paths relative to this file.
    """
    input_path = Path(__file__).parent / "day1.txt"
    with open(input_path) as f:
        return solve_part1(f.read())

def solve2() -> int:
    """
    Solves part 2 using input from day1.txt.
    Similar to solve() but uses solve_part2.
    """
    input_path = Path(__file__).parent / "day1.txt"
    with open(input_path) as f:
        return solve_part2(f.read())

if __name__ == "__main__":
    print(f"Part 1: {solve()}")
    print(f"Part 2: {solve2()}")
