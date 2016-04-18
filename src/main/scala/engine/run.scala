object Run {
  def main(args: Array[String]) = {
    println("Run SudokuEngine on all solvers")
    val BackTrackingEngine = SudokuEngine("BackTracking", BackTracker)
    val puzzles = SudokuPuzzles.testCases
    val parsedPuzzles = for{
        puzzleLocation <- puzzles.keys
      } yield {
        SudokuParser.puzzleFromFile(puzzles(puzzleLocation))
      }
    BackTrackingEngine.solve(parsedPuzzles.toSet)
    println("DONE")
  }
}
