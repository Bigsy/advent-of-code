"""
Tests for Day 1 solutions.
"""
import unittest
from pathlib import Path
from aoc2024.day01.solution import Day01

class TestDay01(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures."""
        self.solver = Day01()
        
    def test_parse_input(self):
        """Test input parsing."""
        sample_input = "1 2\n3 4"
        expected = [[1, 2], [3, 4]]
        self.assertEqual(self.solver.parse_input(sample_input), expected)
    
    def test_solve_part1(self):
        """Test part 1 solution."""
        result = self.solver.solve_part1()
        self.assertEqual(result, 1603498)
    
    def test_solve_part2(self):
        """Test part 2 solution."""
        result = self.solver.solve_part2()
        self.assertEqual(result, 25574739)
    
    def test_sample_input(self):
        """Test with a small sample input."""
        sample_input = "1 2\n3 4\n5 6"
        self.assertEqual(self.solver.solve_part1(sample_input), 3)  # |1-2| + |3-4| + |5-6| = 1 + 1 + 1 = 3
        # Add more sample test cases as needed

if __name__ == '__main__':
    unittest.main()
