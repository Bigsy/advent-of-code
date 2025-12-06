"""
Day 1: Solution for Advent of Code 2024
"""

from pathlib import Path
from typing import List, Tuple

class Day01:
    def __init__(self, input_file: Path = None):
        """Initialize with optional input file path."""
        self.input_file = input_file or Path(__file__).parents[2] / "input" / "day01.txt"
    
    @staticmethod
    def parse_input(input_str: str) -> List[List[int]]:
        """Parse the input string into a list of number pairs."""
        lines = input_str.split('\n')
        return [list(map(int, line.split())) for line in lines if line.strip()]
    
    def solve_part1(self, input_str: str = None) -> int:
        """
        Solve part 1 of the puzzle.
        
        Args:
            input_str: Optional input string. If not provided, reads from input file.
        """
        if input_str is None:
            input_str = self.input_file.read_text()
        
        data = self.parse_input(input_str)
        firsts = sorted(row[0] for row in data)
        seconds = sorted(row[1] for row in data)
        pairs = list(zip(firsts, seconds))
        return sum(abs(a - b) for a, b in pairs)

    def solve_part2(self, input_str: str = None) -> int:
        """
        Solve part 2 of the puzzle.
        
        Args:
            input_str: Optional input string. If not provided, reads from input file.
        """
        if input_str is None:
            input_str = self.input_file.read_text()
        
        data = self.parse_input(input_str)
        firsts = [row[0] for row in data]
        seconds = [row[1] for row in data]
        return sum(x * seconds.count(x) for x in firsts)

def main():
    """Main entry point."""
    solver = Day01()
    print(f"Part 1: {solver.solve_part1()}")
    print(f"Part 2: {solver.solve_part2()}")

if __name__ == "__main__":
    main()
