object Run {
  def main(args: Array[String]) = {
    println("Run SudokuEngine on all solvers")
    val BackTrackingEngine = SudokuEngine("BackTracking", BackTracker)
    val DancingLinksEngine = SudokuEngine("DancingLinks", DancingLinks)
    val puzzles = SudokuPuzzles.testCases
    val parsedPuzzles = for{
        puzzleLocation <- puzzles.keys
      } yield {
        SudokuParser.puzzleFromFile(puzzles(puzzleLocation))
      }
    BackTrackingEngine.solve(parsedPuzzles.toSet)
    DancingLinksEngine.solve(parsedPuzzles.toSet)
    println("DONE")
  }
}
