case class sudukoEngine(
    algorithm: String,
    implementation: sudukoSolver){
  
  /**
   * Solve all the puzzles and output the result to true if all
   * puzzles were successfully passed
   * @param puzzles: filenames for the puzzles to solve
   */
  def solve(puzzles: Set[Puzzle]): Boolean = {
    val results = puzzles.map(implementation.solve(_))
    !results.contains(false)
  }
}