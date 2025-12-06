"""
Test cases for Day 1 solutions.
"""
import unittest
from day1 import solve, solve2

class TestDay1(unittest.TestCase):
    def test_solve_part1(self):
        """Test that solve() returns the expected result."""
        result = solve()
        self.assertEqual(result, 1603498, "Part 1 solution should be 1603498")
    
    def test_solve_part2(self):
        """Test that solve2() returns the expected result."""
        result = solve2()
        self.assertEqual(result, 25574739, "Part 2 solution should be 25574739")

if __name__ == '__main__':
    unittest.main()
