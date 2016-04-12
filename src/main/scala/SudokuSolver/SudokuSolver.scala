trait SudokuSolver{
  /**
   * Solve a given puzzle from a given filename
   */
  def solve(puzzle: Puzzle): Vector[Vector[Option[Int]]]
}