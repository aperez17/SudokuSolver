trait SudokuSolver{
  /**
   * Solve a given puzzle from a given filename
   */
  def solve(puzzle: Puzzle): Seq[Seq[Option[Int]]]
}